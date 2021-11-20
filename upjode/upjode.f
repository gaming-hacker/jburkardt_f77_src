C  UPJODE.FOR  Version 1.12  09 October 1990
C
      PROGRAM UPJODE

c*********************************************************************72
c
cc UPJODE is an interactive ODE solver.
c
C  Version 1.12
C
C  Changed to flow field arrows of uniform size.
C
C  At the expense of a "patch", the flow field command no longer kills
C  the initial condition data.
C
C  The program was failing to accept the input "T, Y1" for the graph
C  command.  CHRREA was stripping the "," as a terminator for T, but then
C  accepting the blank as the terminator for the next (null) input!
C  This has been patched, not fixed.  CHRREA should be rewritten!
C
C  Version 1.11
C
C  Major rewrites to plotting utilities.  Trying to get clipping working.
C
C  Graph of more than one quantity did not work, fixed now.
C
C  Display X and Y axis if in the plot.
C
C  Graphics routines were restarting and shutting down graphics each time.
C    This causes some graphics files to lose all but the latest image.
C    Now UPJODE starts and shuts down graphics once.
C
C  Questions:
C
C  Can you have Y'=SIN(T)/T graph the flow field properly at T=0?  No.
C
C  Could you redesign to allow one to overlay F and G output?
C  Can autonomous equations use F command for vectors dy2/dt / dy1/dt?
C  I can't add that in time for this release!
C
C  Version 1.10
C
C  05 September 1990, Flow fields were not properly scaled.
C  Test case: y1'=3*t*t, plot from 0 < t < 4, 0 < y < 1, slope
C  at right hand points should be almost vertical.  Fixed now.
C
C  04 September 1990, Serious problems in SETUP were corrected.
C
C  28 August 1990, SAMPLE now prints out a message after setting up
C  the sample problem.  Also, if FLOW is called, it now sets NTAB
C  to 0, and TYPE checks this, and no longer prints out information
C  that should have been discarded.
C
C  27 August 1990, changed SUBROUTINE STEP so that, if exact solution
C  is known, it is properly evaluated at initial T.
C
      PARAMETER (MAXMET=10)
      PARAMETER (MAXEQN=4)
      PARAMETER (MAXRHS=2*MAXEQN)
      PARAMETER (MAXVAR=3*MAXEQN+1)
      PARAMETER (MAXFIX=80)
      PARAMETER (MAXPLT=10)
      PARAMETER (MAXRPN=MAXVAR*MAXFIX)
      PARAMETER (MAXTAB=201)
C
      CHARACTER CARRAY*80
      CHARACTER COM*1
      CHARACTER FILHLP*30
      CHARACTER FILNAM*30
      DIMENSION IHAVE(MAXVAR)
      CHARACTER INFIX*(MAXFIX)
      DIMENSION IOUNIT(4)
      DIMENSION IPLOT(2,MAXPLT)
      CHARACTER IRHS(MAXRHS)*(MAXFIX)
      DIMENSION IRPN(MAXRPN)
      DIMENSION ITEMP(MAXVAR)
      CHARACTER LINE*80
      CHARACTER LINE1*80
      CHARACTER METNAM(MAXMET)*60
      CHARACTER NAMES(MAXVAR)*3
      CHARACTER NAMVAR*10
      CHARACTER OUTPUT*255
      CHARACTER PROMPT*80
      DIMENSION TABLE(MAXTAB,MAXVAR)
      DIMENSION TABLE2(MAXVAR)
C
      COMMON /ANYCOM/ IPLT1,IPLT2,IXPLT1,IXPLT2,IYPLT1,
     *                IYPLT2,MARRAY,XPLT1,XPLT2,YPLT1,YPLT2
      COMMON /ANYCHR/ CARRAY
      COMMON /CHRCOM/ NPAGE
C
C  Initialization
C
      CALL INIT(FILHLP,IERROR,IOUNIT,MAXTAB,MAXVAR,METNAM,NAMES,
     *NPAGE,ROVER,TABLE)
C
C  Initialize compiler
C
      CALL COMRPN('I',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,VALUE,IOUNIT)
      CALL COMRPN('A',IERROR,IFRM,INFIX,IRPN,MAXRPN,'T',VALUE,IOUNIT)
      DO I = 2, 8, 2
        CALL COMRPN('A',IERROR,IFRM,INFIX,IRPN,
     *  MAXRPN,NAMES(I),VALUE,IOUNIT)
      end do
C
C  Set initial right hand side and solution
C
      CALL SAMPLE(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *MAXEQN,MAXRPN,MAXTAB,MAXVAR,METHOD,NEQN,
     *NSTEPS,NTAB,OUTPUT,TABLE,TSTOP)
C
      OUTPUT=' '
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='UPJODE - Version 1.12  09 October 1990'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT=' '
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='Demonstrate the numerical solution of'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='first order ordinary differential equations.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
C
      ICOM=0
      XPLT1=0.0
      XPLT2=1.0
      YPLT1=0.0
      YPLT2=1.0
      CALL ANYPLT(ICOM)
C
C  Next command?
C
30    CONTINUE
      IERROR=0
      NLINE=0
      PROMPT='command (H for help)'
      CALL CHRREA(LINE1,LINE,NLINE,PROMPT,IOUNIT,IERROR,0)
      IF(IERROR.NE.0)GO TO 30
      CALL CAPCHR(LINE1)
C
C  Check first for assignment statements.
C
      IF(INDEX(LINE1,'=').GT.1)THEN
        II=INDEX(LINE1,'=')
        NAMVAR=LINE1(1:II-1)
        CALL CHRCHP(LINE1,1,II)
        NLINE=0
        LINE=LINE1
        CALL CHRLEN(LINE,NLINE)
        CALL SETVAL(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *  LINE,MAXEQN,MAXMET,MAXRPN,MAXTAB,MAXVAR,METHOD,METNAM,
     *  NAMVAR,NEQN,NLINE,NSTEPS,NTAB,OUTPUT,PROMPT,ROVER,TABLE,TSTOP)
        GO TO 30
      ELSE
        LINE=LINE1
        CALL CHRLEN(LINE,NLINE)
        CALL CHRREA(COM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0)
      end if
C
C  B=Set up new problem
C
      IF(COM.EQ.'B')THEN
        CALL SETUP(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *  ISET,LINE,MAXEQN,MAXMET,MAXRPN,MAXTAB,MAXVAR,METHOD,METNAM,
     *  NAMVAR,NEQN,NLINE,NSTEPS,NTAB,OUTPUT,PROMPT,ROVER,TABLE,TSTOP)
C
C  C=Continue integration
C
      ELSEIF(COM.EQ.'C')THEN
        CALL STEP(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRPN,
     *  MAXEQN,MAXRPN,MAXTAB,MAXVAR,METHOD,NAMES,NEQN,
     *  NSTEPS,NTAB,OUTPUT,ROVER,TABLE,TSTOP)
C
C  D=Disk file
C
      ELSEIF(COM.EQ.'D')THEN
        CALL DISK(FILNAM,IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
C
C  F=Flow field
C
      ELSEIF(COM.EQ.'F')THEN
        DO I=1,MAXVAR
          TABLE2(I)=TABLE(1,I)
        end do
        CALL FLOW(ICOMP,IERROR,INFIX,IOUNIT,IRPN,LINE,MAXRPN,
     *  MAXTAB,MAXVAR,NAMES,NEQN,NLINE,NTAB,OUTPUT,PROMPT,TABLE)
        DO I=1,MAXVAR
          TABLE(1,I)=TABLE2(I)
        end do
C
C  G=Graphics
C
      ELSEIF(COM.EQ.'G')THEN
        CALL GRAFF(IERROR,IHAVE,IOUNIT,IPLOT,LINE,MAXPLT,MAXTAB,
     *  MAXVAR,NAMES,NLINE,NTAB,OUTPUT,PROMPT,TABLE)
C
C  H=Help
C
      ELSEIF(COM.EQ.'H')THEN
        CALL HELPER(IOUNIT,OUTPUT)
C
C  N=Note
C
      ELSEIF(COM.EQ.'N')THEN
        CALL NOTE(IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
C
C  O=input plot file
C
      ELSEIF(COM.EQ.'O')THEN
        CALL INDUMP(FILNAM,IERROR,IHAVE,IOUNIT,ITEMP,LINE,MAXTAB,
     *  MAXVAR,NLINE,NTAB,PROMPT,TABLE)
C
C  P=Dump plot file
C
      ELSEIF(COM.EQ.'P')THEN
        CALL DUMP(FILNAM,IERROR,IHAVE,IOUNIT,ITEMP,LINE,MAXTAB,
     *  MAXVAR,NAMES,NLINE,NTAB,OUTPUT,PROMPT,TABLE)
C
C  Q=Quit
C
      ELSEIF(COM.EQ.'Q')THEN
        CALL QUIT(FILNAM,IERROR,IOUNIT,NLINE,LINE,OUTPUT,PROMPT)
C
C  R=Read input from file
C
      ELSEIF(COM.EQ.'R')THEN
        CALL READER(FILNAM,IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
C
C  S=Sample problem
C
      ELSEIF(COM.EQ.'S')THEN
        CALL SAMPLE(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *  MAXEQN,MAXRPN,MAXTAB,MAXVAR,METHOD,NEQN,
     *  NSTEPS,NTAB,OUTPUT,TABLE,TSTOP)
C
C  T=Type parameters
C
      ELSEIF(COM.EQ.'T')THEN
        CALL TYPE(IOUNIT,IHAVE,IPRINT,IRHS,MAXEQN,
     *  MAXTAB,MAXVAR,METHOD,METNAM,NEQN,NSTEPS,NTAB,OUTPUT,
     *  ROVER,TABLE,TSTOP)
C
C  V=What variables can I set?
C
      ELSEIF(COM.EQ.'V')THEN
        CALL SHOSET(IOUNIT,OUTPUT)
C
C  W=Write environment and problem to file
C
      ELSEIF(COM.EQ.'W')THEN
        CALL WRITER(FILNAM,IERROR,IOUNIT,IPRINT,IRHS,LINE,
     *  MAXEQN,MAXTAB,MAXVAR,METHOD,NEQN,NLINE,
     *  NSTEPS,OUTPUT,PROMPT,TABLE,TSTOP)
C
C  ? - Extensive help from UPJODE.HLP
C
      ELSEIF(COM.EQ.'?')THEN
        CALL GETHLP(FILHLP,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
C
C  Unrecognized command
C
      ELSE
        IERROR=1
        OUTPUT='Unknown command ignored.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
      end if
C
      IF(IERROR.NE.0.AND.IOUNIT(1).NE.0)THEN
        OUTPUT='An error has occurred in UPJODE while using input'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='from a file.  That file will be closed, and input'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='will only be accepted from the user!'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        CLOSE(UNIT=IOUNIT(1))
        IOUNIT(1)=0
        NPAGE=0
      end if
      GO TO 30
      END
      SUBROUTINE CAPCHR(STRING)

c*********************************************************************72
c
cc CAPCHR capitalizes a string.
c
      CHARACTER CHAR*1
      LOGICAL   LGE
      LOGICAL   LLE
      CHARACTER STRING*(*)

      NCOPY=LEN(STRING)

      DO I = 1, NCOPY
        IF(LGE(STRING(I:I),'a').AND.LLE(STRING(I:I),'z'))THEN
          ITEMP=ICHAR(STRING(I:I))+ICHAR('A')-ICHAR('a')
          STRING(I:I)=CHAR(ITEMP)
      end if
      end do

      RETURN
      END
      SUBROUTINE CHRCHP(STRING,ILO,IHI)

c*********************************************************************72
C
C  CHRCHP accepts a STRING of characters and removes positions
C  ILO through IHI, pushes the end of STRING down and pads with blanks.  
C
C  Using quotes to denote the beginning and end of the string,
C  then calling CHRCHP with STRING='Fred is not a jerk!'
C  and ILO=9 and IHI=12 will result in the output
C  STRING='Fred is a jerk!    '
C
      CHARACTER CHRTMP*1
      CHARACTER STRING*(*)
C
      NCOPY=LEN(STRING)
C
      IF(ILO.GT.IHI)RETURN
C
      DO I=ILO,NCOPY
        INEW=I-ILO+IHI+1
        IF(INEW.LE.NCOPY)THEN
          CHRTMP=STRING(INEW:INEW)
          STRING(I:I)=CHRTMP
        ELSE
          STRING(I:I)=' '
        end if
      end do

      RETURN
      END
      SUBROUTINE CHRCTI(STRING,NCHAR,INTVAL,IERROR,LCHAR)

c*********************************************************************72
C
C  CHRCTI accepts a STRING of NCHAR characters and reads an integer
C  from STRING into INTVAL.  The STRING must begin with an integer
C  but that may be followed by other information.
C  CHRCTI will read as many characters as possible until it reaches
C  the end of the STRING, or encounters a character which cannot be
C  part of the number.
C
C  Legal input is 
C  blanks // initial + or - sign // integer part // blanks,// final comma
C  with most quantities optional.  
C
      CHARACTER CHRTMP*1
      LOGICAL   LGE
      LOGICAL   LLE
      CHARACTER STRING*(*)
C
      NCOPY=NCHAR
      IF(NCOPY.LE.0)NCOPY=LEN(STRING)
C
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
        ELSEIF(IHAVE.EQ.3)THEN
          IHAVE=11
        end if
      ELSEIF(CHRTMP.EQ.',')THEN
        IF(IHAVE.NE.1)THEN
          ITERM=1
          IHAVE=12
          LCHAR=LCHAR+1
        end if
      ELSEIF(CHRTMP.EQ.'-')THEN
        IF(IHAVE.EQ.1)THEN
          IHAVE=2
          ISGN=-1
        ELSE
          ITERM=1
        end if
      ELSEIF(CHRTMP.EQ.'+')THEN
        IF(IHAVE.EQ.1)THEN
          IHAVE=2
        ELSE
          ITERM=1
        end if
      ELSEIF(LGE(CHRTMP,'0').AND.LLE(CHRTMP,'9').AND.IHAVE.LT.11)THEN
        IHAVE=3
        READ(CHRTMP,'(I1)')NDIG
        ITOP=10*ITOP+NDIG
      ELSE
        ITERM=1
      end if
      IF(ITERM.NE.1.AND.LCHAR+1.LT.NCOPY)GO TO 10
      IF(ITERM.NE.1.AND.LCHAR+1.EQ.NCOPY)LCHAR=NCOPY
C
C  Number seems to have terminated.  Have we got a legal number?
C
      IF(IHAVE.EQ.1.OR.IHAVE.EQ.2)THEN
        IERROR=IHAVE
        WRITE(*,*)'CHRCTI - IERROR=',IERROR
        WRITE(*,*)'CHRCTI - Illegal or nonnumeric input:'
        WRITE(*,'(1X,A)')STRING
        RETURN
      end if
C
C  Number seems OK.  Form it.
C
      INTVAL=ISGN*ITOP
      RETURN
      END
      SUBROUTINE CHRCTR(STRING,NCHAR,RVAL,IERROR,LCHAR)

c*********************************************************************72
C
C  CHRCTR accepts a STRING of NCHAR characters and reads a real
C  number from STRING into RVAL.  The STRING must begin with a real
C  number, but that may be followed by other information.
C  CHRCTR will read as many characters as possible until it reaches
C  the end of the STRING, or encounters a character which cannot be
C  part of the real number.
C
C  Legal input is 
C  blanks, initial sign, integer part, decimal, fraction part, 
C  E, exponent sign, exponent integer part, exponent decimal, exponent
C  fraction part, blanks, final comma, with most quantities optional.  
C
C  Examples: 15, 15.0, -14E-7, E2, -12.73E-98.23, etc.
C
      CHARACTER CHRTMP*1
      LOGICAL   LGE
      LOGICAL   LLE
      CHARACTER STRING*(*)
C
      NCOPY=NCHAR
      IF(NCOPY.LE.0)NCOPY=LEN(STRING)
C
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
        ELSEIF(IHAVE.GT.1)THEN
          IHAVE=11
        end if
      ELSEIF(CHRTMP.EQ.',')THEN
        IF(IHAVE.NE.1)THEN
          ITERM=1
          IHAVE=12
          LCHAR=LCHAR+1
        end if
      ELSEIF(CHRTMP.EQ.'-')THEN
        IF(IHAVE.EQ.1)THEN
          IHAVE=2
          ISGN=-1
        ELSEIF(IHAVE.EQ.6)THEN
          IHAVE=7
          JSGN=-1
        ELSE
          ITERM=1
        end if
      ELSEIF(CHRTMP.EQ.'+')THEN
        IF(IHAVE.EQ.1)THEN
          IHAVE=2
        ELSEIF(IHAVE.EQ.6)THEN
          IHAVE=7
        ELSE
          ITERM=1
        end if
      ELSEIF(CHRTMP.EQ.'.')THEN
        IF(IHAVE.LT.4)THEN
          IHAVE=4
        ELSEIF(IHAVE.GE.6.AND.IHAVE.LE.8)THEN
          IHAVE=9
        ELSE
          ITERM=1
        end if
      ELSEIF(CHRTMP.EQ.'E'.OR.CHRTMP.EQ.'e')THEN
        IF(IHAVE.LT.6)THEN
          IHAVE=6
        ELSE
          ITERM=1
        end if
      ELSEIF(LGE(CHRTMP,'0').AND.LLE(CHRTMP,'9').AND.IHAVE.LT.11)THEN
        IF(IHAVE.LE.2)THEN
          IHAVE=3
        ELSEIF(IHAVE.EQ.4)THEN
          IHAVE=5
        ELSEIF(IHAVE.EQ.6.OR.IHAVE.EQ.7)THEN
          IHAVE=8
        ELSEIF(IHAVE.EQ.9)THEN
          IHAVE=10
        end if
        READ(CHRTMP,'(I1)')NDIG
        IF(IHAVE.EQ.3)THEN
          ITOP=10*ITOP+NDIG
        ELSEIF(IHAVE.EQ.5)THEN
          ITOP=10*ITOP+NDIG
          IBOT=10*IBOT
        ELSEIF(IHAVE.EQ.8)THEN
          JTOP=10*JTOP+NDIG
        ELSEIF(IHAVE.EQ.10)THEN
          JTOP=10*JTOP+NDIG
          JBOT=10*JBOT
        end if
      ELSE
        ITERM=1
      end if
      IF(ITERM.NE.1.AND.LCHAR+1.LT.NCOPY)GO TO 10
      IF(ITERM.NE.1.AND.LCHAR+1.EQ.NCOPY)LCHAR=NCOPY
C
C  Number seems to have terminated.  Have we got a legal number?
C
      IF(IHAVE.EQ.1.OR.IHAVE.EQ.2.OR.IHAVE.EQ.6.OR.IHAVE.EQ.7)THEN
        IERROR=IHAVE
        WRITE(*,*)'CHRCTR - Illegal or nonnumeric input:'
        CALL CHRLEN(STRING,LLEN)
        WRITE(*,'(1X,A)')STRING(1:LLEN)
        RETURN
      end if
C
C  Number seems OK.  Form it.
C
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
C
C  CHRDB1 accepts a STRING of characters and removes all blanks
C  and nulls, left justifying the remainder and padding with blanks.
C
      CHARACTER CHAR*1
      CHARACTER CHRTMP*1
      CHARACTER STRING*(*)
C
      NCOPY=LEN(STRING)
C
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
C
C  CHRDB2 accepts a STRING of characters.  It replaces all nulls
C  by blanks.  It replaces all strings of consecutive blanks by a single 
C  blank, left justifying the remainder and padding with blanks.
C
      CHARACTER CHAR*1
      CHARACTER STRING*(*)
      CHARACTER NEWCHR*1
      CHARACTER OLDCHR*1
C
      NCOPY=LEN(STRING)
C
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
C
C  CHRINP is used by CHRREA, INTREA and RELREA
C  to print the PROMPT and read the LINE if necessary.
C
C  IERROR - If IERROR is nonzero on input, CHRINP halts the program.
C           On output, IERROR=0 if no errors were detected,
C           IERROR=1 if there was an error in the read,
C           IERROR=2 if there was an end-of-file in the read.
C
      CHARACTER CHRINT*6
      DIMENSION IOUNIT(*)
      CHARACTER LINE*80
      CHARACTER OUTPUT*90
      CHARACTER PROMPT*80
      COMMON /CHRCOM/ NPAGE
C
      IF(IERROR.NE.0)THEN
        OUTPUT='CHRINP - Fatal nonzero value of IERROR='//CHRINT(IERROR)
        CALL CHRWRT(OUTPUT,1,IOUNIT)
        STOP
      end if
10    CONTINUE
C
C  If necessary, print prompt before reading input.
C  Turn off echo on units between 30 and 39.
C
      IF(NLINE.EQ.0)THEN
        LINE=' '
      ELSE
        CALL CHRLEN(LINE,NLINE)
      end if
      IF(NLINE.LE.0)THEN
        DO I=2,4
          IF(IOUNIT(I).GE.30.AND.IOUNIT(I).LE.39)IOUNIT(I)=-IOUNIT(I)
        end do
        CALL CHRLEN(PROMPT,LCHAR)
        IF(LCHAR.GT.0)THEN
          WRITE(OUTPUT,'(''Enter '',80A1)')(PROMPT(I:I),I=1,LCHAR)
          CALL CHRWRT(OUTPUT,0,IOUNIT)
        end if
        DO I=2,4
          IF(IOUNIT(I).LE.-30.AND.IOUNIT(I).GE.-39)IOUNIT(I)=-IOUNIT(I)
        end do
        IF(IOUNIT(1).LE.0)THEN
          READ(*,'(A80)',END=70,ERR=60)LINE
        ELSE
          READ(IOUNIT(1),'(A80)',END=70,ERR=60)LINE
        end if
        CALL CHRDB2(LINE)
        CALL CHRLEN(LINE,NLINE)
C
C  Don't echo input to IOUNIT(2)
C
        IF(IOUNIT(1).LT.40.OR.IOUNIT(1).GT.49)THEN
          IOSAVE=IOUNIT(2)
          IF(IOUNIT(1).LE.0)IOUNIT(2)=-1
          CALL CHRWRT(LINE,0,IOUNIT)
          IOUNIT(2)=IOSAVE
        end if
      end if
      IF(IOUNIT(1).EQ.0)NPAGE=0
C
C  If item was read, remove item from PROMPT list
C
      IF(NLINE.GT.0)THEN
        ICOMMA=INDEX(PROMPT,',')
        IF(ICOMMA.GT.0.AND.ICOMMA.LT.80.AND.
     *  PROMPT(ICOMMA+1:ICOMMA+1).EQ.' ')ICOMMA=ICOMMA+1
        CALL CHRCHP(PROMPT,1,ICOMMA)
      end if
      RETURN
70    CONTINUE
      IERROR=2
      IF(IOUNIT(1).GT.0)THEN
        CLOSE(UNIT=IOUNIT(1))
        IOUNIT(1)=0
        NPAGE=0
        OUTPUT='CHRINP - End of input from file.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='         Now seeking input from user.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        IERROR=0
        GO TO 10
      end if
      OUTPUT='CHRINP - End of user input.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='       - Fatal error.  Stopping!'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      STOP
60    CONTINUE
      IERROR=1
      OUTPUT='CHRINP - Error in input format.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='CHRINP - Input line follows:'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      CALL CHRWRT(LINE,0,IOUNIT)
      IF(IOUNIT(1).LE.0)THEN
        NLINE=0
        GO TO 10
      end if
      RETURN
      END
      FUNCTION CHRINT(INTVAL)

c*********************************************************************72
C
C  CHRINT accepts an integer and returns in CHRINT the 6-character
C  representation of the integer, right justified, or '******' if the 
C  integer is too large or negative to fit in six positions.
C
      CHARACTER CHRINT*6
      CHARACTER CHRTMP*6

      IF(INTVAL.GT.999999)THEN
        CHRTMP='******'
      ELSEIF(INTVAL.LT.-99999)THEN
        CHRTMP='-*****'
      ELSE
        WRITE(CHRTMP,'(I6)')INTVAL
      end if
      CHRINT=CHRTMP
      RETURN
      END
      SUBROUTINE CHRITC(STRING,NCHAR,INTVAL)

c*********************************************************************72
C
C  CHRITC accepts an integer in INTVAL and stores it in a STRING of
C  NCHAR characters.  The last digit of the integer is stored in the
C  last character of the STRING.  Any left-over entries in STRING
C  are filled with blanks.  If the integer is too large to be
C  written into STRING, STRING is filled with '*' characters.
C
      CHARACTER CHAR*1
      CHARACTER STRING*(*)
C
      NCOPY=NCHAR
      IF(NCOPY.LE.0)NCOPY=LEN(STRING)
C
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
C
C  CHRLEN accepts a STRING of characters and returns LCHAR,
C  the length of the string up to the last nonblank, nonnull.
C
      CHARACTER CHAR*1
      CHARACTER STRING*(*)
C
      NCOPY=LEN(STRING)
C
      DO I=1,NCOPY
        LCHAR=NCOPY+1-I
        IF(STRING(LCHAR:LCHAR).NE.' '.AND.
     *     STRING(LCHAR:LCHAR).NE.CHAR(0))RETURN
      end do
      LCHAR=0
      RETURN
      END
      SUBROUTINE CHRREA(STRING,LINE,NLINE,PROMPT,IOUNIT,IERROR,ITERM)

c*********************************************************************72
C
C  CHRREA accepts LINE, which is assumed to contain NLINE user input
C  characters, where NLINE may be less than 1, and a PROMPT line.
C  If NLINE is less than 1, the PROMPT is printed and user input read
C  from IOUNIT(1) into LINE, and NLINE updated.
C  In either case, up to NCHAR characters are read from LINE into
C  the character STRING and the positions read are removed,
C  and NLINE updated.
C
C  PROMPT is also updated.  On satisfactory input of STRING, everything
C  in PROMPT up to and including the first comma is removed.
C
C  IOUNIT(1) represents the input unit.  0 is taken to be the user
C  and we READ(*,format) the input.
C  IOUNIT(2) is taken to be a standard output unit.  Input is never
C  echoed to IOUNIT(2), but may be to other units.
C  Later units:  If their values is between 30 and 39, user input is
C  copied to them, but no output.
C  If between 40 and 49, output is copied to them, but no input.
C  If the unit number is negative, no input is read, nor output written.
C
C  IERROR - 0, No error occurred.
C           1, Format error during read.
C           2, End of file during read.
C
C  ITERM  - 0, No check for terminators.
C           1, Blank, slash, comma, and equal terminate input.
C
      CHARACTER CHRTMP
      DIMENSION IOUNIT(*)
      CHARACTER LINE*80
      CHARACTER OUTPUT*90
      CHARACTER PROMPT*80
      CHARACTER STRING*(*)
C
      NCHAR=LEN(STRING)
      STRING=' '
      CALL CHRINP(IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
      IF(IERROR.NE.0)RETURN
C
C  Remove double blanks, and capitalize LINE
C
C     IF(ITERM.EQ.2.OR.ITERM.EQ.3)CALL CHRDB2(LINE)
      CALL CAPCHR(LINE)
C
C  Null input acceptable for character input only
C
      IF(NLINE.LE.0)RETURN
C
C  Take one character at a time from LINE and put into STRING.
C
      LCHAR=0
      DO 50 I=1,NCHAR
        IF(LCHAR.NE.0)GO TO 50
        CHRTMP=LINE(I:I)
        IF(ITERM.EQ.1)THEN
          IF(CHRTMP.EQ.' '.OR.CHRTMP.EQ.'/'.OR.
     *       CHRTMP.EQ.','.OR.CHRTMP.EQ.'=')LCHAR=I
      end if
        IF(LCHAR.EQ.0)STRING(I:I)=CHRTMP
50      CONTINUE
      IF(LCHAR.EQ.0)LCHAR=NCHAR
      CALL CHRCHP(LINE,1,LCHAR)
      CALL CHRLEN(LINE,NLINE)
      RETURN
      END
      CHARACTER*14 FUNCTION CHRREL(RVAL)

c*********************************************************************72
C
C  CHRREL accepts a real number in RVAL and returns in CHRREL a 
C  14-character representation, right justified, of that number.
C
      CHARACTER CHRTMP*14
C
C  We can't seem to write directly into CHRREL because of compiler
C  quibbles.
C
      WRITE(CHRTMP,'(G14.6)')RVAL
      CHRREL=CHRTMP
      RETURN
      END
      SUBROUTINE CHRWRT(STRING,ICOMP,IOUNIT)

c*********************************************************************72
C
C  CHRWRT writes a character STRING of at most NCHAR characters to one
C  or more output units.
C
C  ICOMP  - Input flag. 
C           0, no action.
C           1, then double blanks are removed from the STRING before 
C              printing.  
C           2, paging is turned off.
C           3, initialization, with number of lines per page set to 22
C
C  IOUNIT - vector of length 4, containing in positions 2 
C           through 4 either a negative value, or 0 for the user output 
C           unit *, or the value of a Fortran logical output unit.
C
      CHARACTER ISAY*1
      DIMENSION IOUNIT(*)
      CHARACTER STRING*(*)
      SAVE IPAGE, ICALL
      COMMON /CHRCOM/ NPAGE
      DATA ICALL /0/
C
      IF(ICOMP.EQ.2)THEN
        IPAGE=-1
        ICALL=1
        RETURN
      ELSEIF(ICOMP.EQ.3)THEN
        ICALL=1
        NPAGE=0
        IPAGE=22
        RETURN
      end if
C
      IF(ICALL.EQ.0)THEN
        ICALL=1
        NPAGE=0
        IPAGE=22
      end if
C
C  If first call, with I/O attached to user, see if paging desired
C
      IF(IPAGE.EQ.0.AND.IOUNIT(1).EQ.0.AND.IOUNIT(2).EQ.0)THEN
        WRITE(*,*)'Enter lines per page, 0 for no paging.'
        READ(*,*,END=20,ERR=20)IPAGE
        IF(IPAGE.EQ.0)IPAGE=-1
20      CONTINUE
      end if    
C
      IF(IPAGE.GT.0.AND.
     *   NPAGE.GE.IPAGE.AND.
     *   IOUNIT(2).EQ.0.AND.
     *   IOUNIT(1).LE.0)THEN      
        NPAGE=0
        WRITE(*,*)'Press RETURN for more'
        READ(*,'(A1)',END=25,ERR=25)ISAY
        IF(ISAY.EQ.'Q'.OR.ISAY.EQ.'q')IPAGE=0
25      CONTINUE
      end if
      IF(ICOMP.EQ.1)CALL CHRDB2(STRING)
      CALL CHRLEN(STRING,LCHAR)
      IF(LCHAR.LE.0)LCHAR=1
      DO I=2,4
        IF(IOUNIT(I).EQ.0)THEN
          WRITE(*,'(1X,A)')STRING(1:LCHAR)
        ELSEIF(IOUNIT(I).GT.0)THEN
          WRITE(IOUNIT(I),'(1X,A)')STRING(1:LCHAR)
        end if
      end do
      NPAGE=NPAGE+1
      RETURN
      END
      SUBROUTINE COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,
     * Y,YP,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)

c*********************************************************************72
C
cc COMPUT ...
c
      CHARACTER INFIX*(*)
      DIMENSION IOUNIT(*)
      DIMENSION IRPN(MAXRPN)
      CHARACTER NAMES(MAXVAR)*3
      CHARACTER NAMVAR*10
      CHARACTER OUTPUT*(*)
      DIMENSION Y(NEQN)
      DIMENSION YP(NEQN)
C
      IERROR=0
      CALL COMRPN('V',IERROR,IFRM,INFIX,IRPN,MAXRPN,'T',TVAL,IOUNIT)
      DO 22 I=1,NEQN
        NAMVAR=NAMES(2*I)
        CALL COMRPN('V',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,Y(I),
     *  IOUNIT)
22      CONTINUE
C
C  Evaluate formulas
C
      DO I=1,NEQN
        IFRM=I
        CALL COMRPN('E',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,YP(I),
     *  IOUNIT)
        IF(ABS(YP(I)).GT.ROVER)THEN
          IERROR=101
          OUTPUT='Overflow in value of derivative!'
          CALL CHRWRT(OUTPUT,0,IOUNIT)
          RETURN
        end if
      end do
      RETURN
      END
      SUBROUTINE COMRPN(COM,IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVR,VALUE,
     *IOUNIT)

c*********************************************************************72
c
cc COMRPN
c
      PARAMETER (MAXSYM=75)
      PARAMETER (MAXVAL=MAXSYM)
C
      CHARACTER CHRINT*6
      CHARACTER COM*1
      CHARACTER INFIX*(*)
      DIMENSION INTSYM(MAXSYM)
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IPVAL(MAXSYM)
      DIMENSION IRPN(MAXRPN)
      DIMENSION ISTACK(MAXSYM)
      DIMENSION LENSYM(MAXSYM)
      CHARACTER NAMVR*(*)
      CHARACTER NAMVAR*10
      CHARACTER OUTPUT*100
      CHARACTER SYMBOL(MAXSYM)*10
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
C
      NAMVAR=NAMVR
      IMPLIC=0
      MAXFIX=LEN(INFIX)
      MAXFRM=(MAXRPN/MAXFIX)
      CALL CAPCHR(NAMVAR)
      CALL CAPCHR(INFIX)
      CALL CHRDB1(NAMVAR)
      IERROR=0
      IF(COM.EQ.'E'.OR.COM.EQ.'F'.OR.COM.EQ.'G')THEN
        IF(IFRM.LE.0.OR.IFRM.GT.MAXFRM)THEN
          OUTPUT='COMRPN - Illegal formula index='//CHRINT(IFRM)
          CALL CHRWRT(OUTPUT,1,IOUNIT)
          IERROR=1
          RETURN
        end if
      end if
C
C  COM=I initialize
C  Note that the assignment of NSYMP below must be updated
C  if new symbols are added.
C
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
        DO 30 I=1,NSYMP
          CALL CHRLEN(SYMBOL(I),LCHAR)
          LENSYM(I)=LCHAR
          IPVAL(I)=I
          VALSYM(I)=0.0
          IF(SYMBOL(I).EQ.'NEG')INEG=I
          IF(SYMBOL(I).EQ.'$')IFINIS=I
          IF(SYMBOL(I).EQ.'INDEX1')INDX1=I
          IF(SYMBOL(I).EQ.'PI')VALSYM(I)=4.0*ATAN2(1.0,1.0)
          IF(SYMBOL(I).EQ.'EPS')VALSYM(I)=EPS
30        CONTINUE
        DO I=1,MAXRPN
          IRPN(I)=0
        end do
        INFIX=' '
C
C  COM=A, Add variable
C
      ELSEIF(COM.EQ.'A')THEN
        CALL SYMADD(IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     1  MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,
     *  VALSYM,IOUNIT,OUTPUT,IFREE)
C
C  COM=V, Set variable value
C  COM=R, Get variable value
C
      ELSEIF(COM.EQ.'R'.OR.COM.EQ.'V')THEN
        CALL SYMVAL(COM,IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     1  MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,VALSYM,VALUE,
     *  IOUNIT,OUTPUT,IFREE)
C
C  COM=F or G, compile formula.
C
      ELSEIF(COM.EQ.'F'.OR.COM.EQ.'G')THEN
        NUVAR=1
        IF(COM.EQ.'G')NUVAR=0
C
C  Compress and measure formula
C
        CALL CHRDB1(INFIX)
        CALL CHRLEN(INFIX,LENFIX)
        IF(LENFIX.LE.0)THEN
          IERROR=1
          OUTPUT='COMRPN - Formula has zero length.'
          CALL CHRWRT(OUTPUT,1,IOUNIT)
          RETURN
        end if
C
C  Parenthesis check
C
        ISUM=0
        DO 90 I=1,LENFIX
          IF(INFIX(I:I).EQ.'(')ISUM=ISUM+1
          IF(INFIX(I:I).EQ.')')ISUM=ISUM-1
          IF(ISUM.LT.0)IERROR=1
90        CONTINUE
        IF(ISUM.NE.0)IERROR=1
        DO 100 I=2,LENFIX
          IF(INFIX(I-1:I-1).EQ.')'.AND.INFIX(I:I).EQ.'(')IERROR=1
          IF(INFIX(I-1:I-1).EQ.'('.AND.INFIX(I:I).EQ.')')IERROR=1
100       CONTINUE
        IF(IERROR.NE.0)THEN
          OUTPUT='COMRPN - Unbalanced or illegal parentheses.'
          CALL CHRWRT(OUTPUT,0,IOUNIT)
          RETURN
        end if
C
C  Convert to string of integers
C
        CALL SYMCHK(IERROR,IFINIS,IFREE,IMPLIC,INDX1,INEG,INFIX,
     *  INTSYM,IOPSYM,IOUNIT,IPRSYM,IPVAL,LENFIX,LENSYM,
     *  MAXSYM,MAXVAL,NINT,NSYM,NSYMP,NUVAR,OUTPUT,SYMBOL,
     *  VALSYM)
        IF(IERROR.NE.0)THEN
          OUTPUT='COMRPN - Could not compile formula.'
          CALL CHRWRT(OUTPUT,0,IOUNIT)
          IF(IMPLIC.NE.0)GO TO 160
          RETURN
        end if
C
C  Convert to RPN
C
        IMIN=(IFRM-1)*80+1
        CALL RPNSET(IERROR,INTSYM,IOPSYM,IPRSYM,IRPN(IMIN),ISTACK,
     1  MAXFIX,MAXSYM,NINT,NRPN,SYMBOL,IOUNIT,OUTPUT,IFINIS)
        IF(IERROR.NE.0)THEN
          OUTPUT='COMRPN - Could not compile formula.'
          CALL CHRWRT(OUTPUT,0,IOUNIT)
          IF(IMPLIC.NE.0)GO TO 160
          RETURN
        end if
        INFIX=' '
C
C  Check that operators and arguments are balanced
C
        IHI=IMIN+NRPN-1
        CALL RPNCHK(IERROR,IHI,ILO,IOPSYM,IRPN,MAXRPN,MAXSYM)
        IF(IERROR.NE.0.OR.ILO.NE.IMIN)THEN
          OUTPUT='COMRPN - Illegal formula'
          CALL CHRWRT(OUTPUT,1,IOUNIT)
          IERROR=1
          IF(IMPLIC.NE.0)GO TO 160
          RETURN
        end if
C
C  For implicit definition via equality, evaluate formula to
C  get dimensions
C
        IF(IMPLIC.NE.0)GO TO 150
      ELSEIF(COM.EQ.'E')THEN
        GO TO 150
      ELSE
        OUTPUT='COMRPN - Unknown command = '//COM
        CALL CHRWRT(OUTPUT,1,IOUNIT)
        IERROR=1
      end if
      RETURN
C
C  COM=E, Evaluate formula
C
150   CONTINUE
      IMIN=(IFRM-1)*80+1
      NRPN=80
      IFREESV=IFREE
      CALL RPNVAL(IERROR,IOPSYM,IPRSYM,IPVAL,IRPN(IMIN),ISTACK,
     *LENSYM,MAXRPN,MAXSYM,MAXVAL,IFREE,NRPN,NSYM,NSYMS,SYMBOL,VALSYM,
     *VALUE,IOUNIT,OUTPUT)
      IFREE=IFREESV
      IF(IERROR.NE.0)THEN
        VALUE=0.0
        OUTPUT='COMRPN - Evaluation error.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        IF(IMPLIC.NE.0)GO TO 160
      end if
      RETURN
C
C  If problems, cancel implicit declaration
C
160   CONTINUE
      OUTPUT='COMRPN - Cancelling implicit variable '
      CALL CHRWRT(OUTPUT,1,IOUNIT)
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
      CHARACTER FILNAM*(*)
      DIMENSION IOUNIT(*)
      CHARACTER OUTPUT*(*)
C
      OPEN(UNIT=18,FILE=FILNAM,STATUS='OLD',ERR=10)
      OUTPUT='UPJODE is deleting the old copy of '//FILNAM
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      CLOSE(UNIT=18,STATUS='DELETE')
10    CONTINUE
      RETURN
      END
      SUBROUTINE DISK(FILNAM,IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)

c*********************************************************************72
C
CC DISK opens a new disk file.
c
C  If disk file is opened, close it.  
C  If disk file is not open, delete any old copy, and open a new one.
C
      CHARACTER FILNAM*(*)
      DIMENSION IOUNIT(*)
      CHARACTER LINE*(*)
      CHARACTER OUTPUT*(*)
      CHARACTER PROMPT*(*)
C
      IF(IOUNIT(3).GT.0)THEN
        OUTPUT='UPJODE is closing the disk transcript file.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        CLOSE(UNIT=IOUNIT(3))
        IOUNIT(3)=-1
      ELSE
        PROMPT='transcript filename'
        CALL CHRREA(FILNAM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0)
        IF(IERROR.NE.0)RETURN
        IOUNIT(3)=21
        CALL DELFIL(FILNAM,IOUNIT,OUTPUT)
        OPEN(UNIT=IOUNIT(3),FILE=FILNAM,STATUS='NEW',ERR=10)
        OUTPUT='UPJODE is opening the file '//FILNAM
        CALL CHRWRT(OUTPUT,0,IOUNIT)
      end if
      RETURN
C
C  File not found
C
10    CONTINUE
      OUTPUT='File could not be opened.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
      END
      SUBROUTINE DUMP(FILNAM,IERROR,IHAVE,IOUNIT,ITEMP,LINE,MAXTAB,
     *MAXVAR,NAMES,NLINE,NTAB,OUTPUT,PROMPT,TABLE)

c*********************************************************************72
c
cc DUMP saves data to a file.
c
      CHARACTER FILNAM*(*)
      DIMENSION IHAVE(MAXVAR)
      DIMENSION IOUNIT(*)
      DIMENSION ITEMP(MAXVAR)
      CHARACTER LINE*(*)
      CHARACTER NAMES(MAXVAR)*3
      CHARACTER OUTPUT*(*)
      CHARACTER PROMPT*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)
C
      IF(NTAB.LE.1)THEN
        IERROR=1
        OUTPUT='There is no data to write to a file!'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if
C
      PROMPT='filename to store saved data in.'
      CALL CHRREA(FILNAM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0)
      IF(IERROR.NE.0)RETURN
      CALL DELFIL(FILNAM,IOUNIT,OUTPUT)
C
      NDO=0
      DO I=1,MAXVAR
        IF(IHAVE(I).NE.0)THEN
          NDO=NDO+1
          ITEMP(NDO)=I
        end if
      end do
C
      OPEN(UNIT=17,FILE=FILNAM,STATUS='NEW',ERR=30)
      WRITE(17,*)'UPJODE created this file.'
      WRITE(17,'('' Variables:'',13(1X,A3))')
     *(NAMES(ITEMP(I)),I=1,NDO)
      WRITE(17,'(1X,15I4)')NTAB,(IHAVE(I),I=1,MAXVAR)
      WRITE(17,*)'Table:'
      DO 20 I=1,NTAB
        WRITE(17,'(1X,6G12.4)')(TABLE(I,ITEMP(J)),J=1,NDO)
20      CONTINUE
      WRITE(17,*)'End.'
      CLOSE(UNIT=17)
C
      OUTPUT='Data stored in file.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
C
30    CONTINUE
      IERROR=2
      RETURN
      END
      SUBROUTINE FLOW(ICOMP,IERROR,INFIX,IOUNIT,IRPN,LINE,MAXRPN,
     *MAXTAB,MAXVAR,NAMES,NEQN,NLINE,NTAB,OUTPUT,PROMPT,TABLE)

c*********************************************************************72
C
cc FLOW ???
c
C  TABLE  Output, REAL TABLE(NY,NX), contains the direction field values.
C         TABLE(I,J) contains the scaled value of DYDT at Y(I), X(J).
C         Here Y(I)=((NY-I)*YMIN + (I-1)*YMAX) / REAL (NY-1)
C         and  X(J)=((NX-J)*XMIN + (J-1)*XMAX) / REAL (NX-1)
C         The scaling done divides each entry of TABLE by DY/(DX*DMAX)
C         where DY = (YMAX-YMIN)/REAL(NY-1), DX=(XMAX-XMIN)/REAL(NX-1)
C         and DMAX is the maximum absolute value of the entries in DIR.
C         This guarantees that the flow field vector (DX, TABLE(I,J)*DX)
C         will fit reasonably well inside a box of width DT, height DY,
C         centered at Y(I), X(J).
C
C  ICOMP  Input, INTEGER ICOMP, the component of the solution whose
C         flow field is to be mapped.
C
C  NX,
C  NY     Input, INTEGER NX, NY, the number of points in the X and Y directions.
C
C  XMAX,
C  XMIN,
C  YMAX,
C  YMIN   Input, REAL XMAX, XMIN, YMAX, YMIN, the maximum X, minimum X,
C         maximum Y and minimum Y.
C
      CHARACTER CARRAY*80
      CHARACTER CHRINT*6
      CHARACTER INFIX*(*)
      DIMENSION IOUNIT(*)
      DIMENSION IRPN(MAXRPN)
      CHARACTER LINE*(*)
      CHARACTER NAMES(MAXVAR)*3
      CHARACTER NAMVAR*10
      CHARACTER OUTPUT*(*)
      CHARACTER PROMPT*(*)
      DIMENSION TABLE(*)
C
      COMMON /ANYCOM/ IPLT1,IPLT2,IXPLT1,IXPLT2,IYPLT1,
     *                IYPLT2,MARRAY,XPLT1,XPLT2,YPLT1,YPLT2
      COMMON /ANYCHR/ CARRAY
C
      IERROR=0
      PROMPT='number of T points, minimum T, maximum T.'
      CALL INTREA(NX,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      IF(NX.LT.1)THEN
        IERROR=1
        OUTPUT='Number of T points must be at least 1!'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if
      CALL RELREA(XMIN,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      CALL RELREA(XMAX,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      IF(IERROR.NE.0)RETURN
      TEMP1=MIN(XMIN,XMAX)
      TEMP2=MAX(XMIN,XMAX)
      XMIN=TEMP1
      XMAX=TEMP2
      IF(XMIN.EQ.XMAX)THEN
        XMIN=XMIN-0.5
        XMAX=XMAX+0.5
      end if
C
      PROMPT='number of Y points, minimum Y, maximum Y.'
      CALL INTREA(NY,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      IF(NY.LT.1)THEN
        IERROR=1
        OUTPUT='Number of Y points must be at least 1!'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if
      CALL RELREA(YMIN,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      CALL RELREA(YMAX,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      IF(IERROR.NE.0)RETURN
      TEMP1=MIN(YMIN,YMAX)
      TEMP2=MAX(YMIN,YMAX)
      YMIN=TEMP1
      YMAX=TEMP2
      IF(YMIN.EQ.YMAX)THEN
        YMIN=YMIN-0.5
        YMAX=YMAX+0.5
      end if
C
      IF(NX*NY.GT.MAXVAR*MAXTAB)THEN
        OUTPUT='Sorry, you have asked for too many points.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='The product NX*NY must be less than '//
     *  CHRINT(MAXVAR*MAXTAB)
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='but your value is '//CHRINT(NX*NY)
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        IERROR=1
        RETURN
      end if
C
      IF(NEQN.GT.1)THEN
        PROMPT='Y component number.'
        CALL INTREA(ICOMP,LINE,NLINE,PROMPT,IOUNIT,IERROR)
        IF(IERROR.NE.0)RETURN
      ELSE
        ICOMP=1
      end if
C
C  Note that all solution data beyond the initial condition is about to 
C  be overwritten:
C
      IF(NTAB.GT.0)NTAB=1
C
      DMAX=1.0
      IF(NX.GT.1)DX=(XMAX-XMIN)/REAL(NX-1)
      IF(NY.GT.1)DY=(YMAX-YMIN)/REAL(NY-1)
C
C  Set a value of X, and tell COMRPN what that value is.
C
      DO 20 J=1,NX
        IF(NX.GT.1)THEN
          X=((NX-J)*XMIN + (J-1)*XMAX) / REAL (NX-1)
        ELSE
          X=0.5*(XMIN+XMAX)
        end if
        CALL COMRPN('V',IERROR,IFRM,INFIX,IRPN,MAXRPN,'T',X,IOUNIT)
C
C  Set a value of Y, and tell COMRPN what that value is.
C  Then ask COMRPN to evaluate DYDT(T,Y).
C
        DO 10 I=1,NY
          IF(NY.GT.1)THEN
            Y=((NY-I)*YMIN + (I-1)*YMAX) / REAL (NY-1)
          ELSE
            Y=0.5*(YMIN+YMAX)
          end if
          NAMVAR=NAMES(2*ICOMP)
          CALL COMRPN('V',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,Y,IOUNIT)
          CALL COMRPN('E',IERROR,ICOMP,INFIX,IRPN,MAXRPN,NAMVAR,VALUE,
     *    IOUNIT)
          TABLE((J-1)*NY+I)=VALUE
C         DMAX=MAX(DMAX,ABS(VALUE))
10        CONTINUE
20      CONTINUE
C
C  Scale direction vector so that if you multiply it by DX
C  it's guaranteed to be no greater than DY.
C
C     SCALE=0.5*DY/DMAX
C     IF(SCALE.GT.0.5*DX)SCALE=0.5*DX
C     DELT=SCALE
C     DO J=1,NX*NY
C       TABLE(J)=SCALE*TABLE(J)
C     end do
C
C  Do the drawing.
C
      XPLT1=0.00
      XPLT2=1.00
      YPLT1=0.00
      YPLT2=1.00
      IPLT1=1
      ICOM=2
      CALL ANYPLT(ICOM)
C
      XPLT1=XMIN-DX
      XPLT2=XMAX-XMIN+2.0*DX
      YPLT1=YMIN-DY
      YPLT2=YMAX-YMIN+2.0*DY
      ICOM=3
      CALL ANYPLT(ICOM)
C
C  Draw X and Y axis if they appear
C
      IF(XMIN.LE.0.0.AND.0.0.LE.XMAX)THEN
        XPLT1=0.0
        YPLT1=YMIN
        ICOM=4
        CALL ANYPLT(ICOM)
        XPLT1=0.0
        YPLT1=YMAX
        ICOM=5
        CALL ANYPLT(ICOM)
      end if
      IF(YMIN.LE.0.0.AND.0.0.LE.YMAX)THEN
        XPLT1=XMIN
        YPLT1=0.0
        ICOM=4
        CALL ANYPLT(ICOM)
        XPLT1=XMAX
        YPLT1=0.0
        ICOM=5
        CALL ANYPLT(ICOM)
      end if
C
      CARRAY='.'
C     VSCALE=0.75*MIN((XMAX-XMIN)/REAL(NX-1), (YMAX-YMIN)/REAL(NY-1))
      ALEN=0.9*MIN(1.0/REAL(NX),1.0/REAL(NY))
      DO 60 I=1,NY
        DO 50 J=1,NX
          X1=XMIN+(J-1)*(XMAX-XMIN)/REAL(NX-1)
          Y1=YMIN+(I-1)*(YMAX-YMIN)/REAL(NY-1)
          XPLT1=X1
          YPLT1=Y1
          ICOM=11
          CALL ANYPLT(ICOM)
          ICOM=4
          CALL ANYPLT(ICOM)
C
          TEMPX=1.0
          TEMPY=TABLE((J-1)*NY+I)
          ANGLE=ATAN(TEMPY)
C         TEMPD=SQRT(TEMPY**2+TEMPX**2)
C         TEMPX=VSCALE*TEMPX/TEMPD
C         TEMPY=VSCALE*TEMPY/TEMPD
C
C         X2=X1+TEMPX
C         Y2=Y1+TEMPY
          X2=X1+(XMAX-XMIN)*ALEN*COS(ANGLE)
          Y2=Y1+(YMAX-YMIN)*ALEN*SIN(ANGLE)
          XPLT1=X2
          YPLT1=Y2
          ICOM=5
          CALL ANYPLT(ICOM)
C
C         UU=X2-X1
C         VV=Y2-Y1
C         TNORM=SQRT((UU)**2+(VV)**2)
C         IF(TNORM.GT.0.0)THEN
C           THET=1.57-ATAN(2.0)
C           THET=0.5*THET
C           ALPH=ATAN2(VV,UU)
C           DEL=SQRT(5.0)*TNORM/3.0
C           U1=X1+DEL*COS(ALPH-THET)
C           V1=Y1+DEL*SIN(ALPH-THET)
C           U2=X1+DEL*COS(ALPH+THET)
C           V2=Y1+DEL*SIN(ALPH+THET)
C           XPLT1=U1
C           YPLT1=V1
C           ICOM=4
C           CALL ANYPLT(ICOM)
C           XPLT1=X2
C           YPLT1=Y2
C           ICOM=5
C           CALL ANYPLT(ICOM)
C           XPLT1=U2
C           YPLT1=V2
C           ICOM=5
C           CALL ANYPLT(ICOM)
C         end if
50        CONTINUE
60      CONTINUE
      ICOM=9
      CALL ANYPLT(ICOM)
      RETURN
      END
      SUBROUTINE FUNVAL(IARG1,IARG2,IERROR,IOPSYM,IPRSYM,IPVAL,
     *ITEMP,LENSYM,MAXSYM,MAXVAL,NSYMS,SYM,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IPSET,IFREE)

c*********************************************************************72
c
cc FUNVAL
c
      CHARACTER CTEMP*3
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IPVAL(MAXSYM)
      DIMENSION LENSYM(MAXSYM)
      CHARACTER OUTPUT*100
      CHARACTER SYM*10
      CHARACTER SYMBOL(MAXSYM)*10
      DIMENSION VALSYM(MAXVAL)
C
      IPSET=0
      IERROR=0
      INDEX1=IPVAL(IARG1)
      INDEX2=0
      IF(IARG2.NE.0)INDEX2=IPVAL(IARG2)
C
      IF(NSYMS.GE.MAXSYM)THEN
        IERROR=1
        OUTPUT='FUNVAL - Not enough storage.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if
C
      NSYMS=NSYMS+1
      IOPSYM(NSYMS)=0
      IPRSYM(NSYMS)=10
      LENSYM(NSYMS)=6
      ITEMP=ITEMP+1
      CALL CHRITC(CTEMP,3,ITEMP)
      IF(ITEMP.LE.99)CTEMP(1:1)='0'
      IF(ITEMP.LE.9)CTEMP(2:2)='0'
      SYMBOL(NSYMS)='STK000'
      SYMBOL(NSYMS)(4:6)=CTEMP
      IPVAL(NSYMS)=IFREE
      INDEX4=IPVAL(NSYMS)
C
        ARG1=VALSYM(IPVAL(IARG1))
        ARG2=0.0
        IF(IARG2.NE.0)ARG2=VALSYM(IPVAL(IARG2))
        IF(SYM.EQ.'+')THEN
          VALSYM(INDEX4)=ARG1+ARG2
          RETURN
        ELSEIF(SYM.EQ.'-')THEN
          VALSYM(INDEX4)=ARG1-ARG2
          RETURN
        ELSEIF(SYM.EQ.'/')THEN
          IF(ARG2.EQ.0.0)THEN
            IERROR=1
            OUTPUT='FUNVAL - Attempt to divide by 0.'
            CALL CHRWRT(OUTPUT,0,IOUNIT)
          ELSE
            VALSYM(INDEX4)=ARG1/ARG2
          end if
          RETURN
        ELSEIF(SYM.EQ.'*')THEN
          VALSYM(INDEX4)=ARG1*ARG2
          RETURN
        ELSEIF(SYM.EQ.'**'.OR.SYM.EQ.'^')THEN
          IF(ARG1.EQ.0.0.AND.ARG2.EQ.0.0)THEN
            OUTPUT='FUNVAL - Attempt to compute 0**0'
            CALL CHRWRT(OUTPUT,0,IOUNIT)
            IERROR=1
            RETURN
          end if
          IF(ARG1.LT.0.0.AND.REAL(INT(ARG2)).NE.ARG2)THEN
            WRITE(OUTPUT,1040)ARG1,ARG2
            CALL CHRWRT(OUTPUT,1,IOUNIT)
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
        ELSEIF(SYM.EQ.'=')THEN
          ARG1=ARG2
          VALSYM(INDEX1)=ARG2
          VALSYM(INDEX4)=ARG1
          RETURN
        ELSEIF(SYM.EQ.'ABS')THEN
          VALSYM(INDEX4)=ABS(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'ACOS')THEN
          IF(ARG1.LT.-1.0.OR.ARG1.GT.1.0)THEN
            WRITE(OUTPUT,1000)SYM,ARG1
            CALL CHRWRT(OUTPUT,1,IOUNIT)
            IERROR=1
            RETURN
          end if
          VALSYM(INDEX4)=ACOS(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'ALOG'.OR.SYM.EQ.'LN'.OR.SYM.EQ.'LOG')THEN
          IF(ARG1.LE.0.0)THEN
            IERROR=1
            WRITE(OUTPUT,1000)SYM,ARG1
            CALL CHRWRT(OUTPUT,1,IOUNIT)
          ELSE
            VALSYM(INDEX4)=ALOG(ARG1)
          end if
          RETURN
        ELSEIF(SYM.EQ.'ALOG10'.OR.SYM.EQ.'LOG10')THEN
          IF(ARG1.LE.0.0)THEN
            WRITE(OUTPUT,1000)SYM,ARG1
            CALL CHRWRT(OUTPUT,1,IOUNIT)
            IERROR=1
            RETURN
          end if
          VALSYM(INDEX4)=ALOG10(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'ASIN')THEN
          IF(ARG1.LT.-1.0.OR.ARG1.GT.1.0)THEN
            WRITE(OUTPUT,1000)SYM,ARG1
            CALL CHRWRT(OUTPUT,1,IOUNIT)
            IERROR=1
            RETURN
          end if
          VALSYM(INDEX4)=ASIN(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'ATAN')THEN
          VALSYM(INDEX4)=ATAN(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'ATAN2')THEN
          IF(ARG1.EQ.0.0.AND.ARG2.EQ.0.0)ARG2=1.0
          VALSYM(INDEX4)=ATAN2(ARG1,ARG2)
          RETURN
        ELSEIF(SYM.EQ.'COS')THEN
          VALSYM(INDEX4)=COS(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'COSH')THEN
          VALSYM(INDEX4)=COSH(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'EXP')THEN
          VALSYM(INDEX4)=EXP(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'INT')THEN
          VALSYM(INDEX4)=ANINT(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'MAX')THEN
          VALSYM(INDEX4)=ARG1
          IF(VALSYM(INDEX4).LT.ARG2)VALSYM(INDEX4)=ARG2
          RETURN
        ELSEIF(SYM.EQ.'MIN')THEN
          VALSYM(INDEX4)=ARG1
          IF(VALSYM(INDEX4).GT.ARG2)VALSYM(INDEX4)=ARG2
          RETURN
        ELSEIF(SYM.EQ.'NEG')THEN
          VALSYM(INDEX4)=-VALSYM(INDEX1)
          RETURN
        ELSEIF(SYM.EQ.'SIN'.OR.SYM.EQ.'SINE')THEN
          VALSYM(INDEX4)=SIN(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'SINH')THEN
          VALSYM(INDEX4)=SINH(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'SQRT')THEN
          IF(ARG1.LT.0.0)THEN
            IERROR=1
            WRITE(OUTPUT,1000)SYM,ARG1
            CALL CHRWRT(OUTPUT,1,IOUNIT)
          ELSE
            VALSYM(INDEX4)=SQRT(ARG1)
          end if
          RETURN
        ELSEIF(SYM.EQ.'STEP')THEN
          IF(ARG1.LT.0.0)VALSYM(INDEX4)=0.0
          IF(ARG1.GE.0.0)VALSYM(INDEX4)=1.0
          RETURN
        ELSEIF(SYM.EQ.'TAN')THEN
          VALSYM(INDEX4)=TAN(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'TANH')THEN
          VALSYM(INDEX4)=TANH(ARG1)
          RETURN
        ELSEIF(SYM.EQ.'!')THEN
          IF(ARG1.LT.0.0.OR.ARG1.GT.25.0)THEN
            OUTPUT='FUNVAL - Argument out of range for factorial.'
            CALL CHRWRT(OUTPUT,0,IOUNIT)
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
C
      IERROR=1
      OUTPUT='FUNVAL - Unknown operation: '//SYM
      CALL CHRWRT(OUTPUT,1,IOUNIT)
      RETURN
1000  FORMAT('FUNVAL - Illegal ',A10,' of ',G14.6)
1040  FORMAT('FUNVAL - Illegal exponentiation ',G14.6,' **',G14.6)
      END
      SUBROUTINE GETHLP(FILHLP,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)

c*********************************************************************72
c
cc GETHLP
c
      PARAMETER (MAXTOP=40)
      CHARACTER CHOICE*75
      CHARACTER CTEMP*75
      CHARACTER CTEMP2*75
      CHARACTER FILHLP*30
      CHARACTER INLINE*75
      DIMENSION IOUNIT(*)
      CHARACTER LAB*1
      CHARACTER LEVELC(MAXTOP)*75
      DIMENSION LEVELM(10)
      DIMENSION LEVELT(MAXTOP)
      CHARACTER LINE*80
      CHARACTER OUTPUT*(*)
      CHARACTER PROMPT*80
      COMMON /CHRCOM/ NPAGE
      IERROR=0
      LHUNIT=55
C
C  Open help file
C
      OPEN(UNIT=LHUNIT,FILE=FILHLP,STATUS='OLD',ERR=100)
C    *SHARED,READONLY)
      LEVELO=0
      LEVEL=1
      ILINE=1
C
C  Move to beginning of current topic by reading MOVE lines from the
C  top of the file.  Record this position, corresponding to the current
C  LEVEL, in LEVELM, in case we later want to back up.  Print out the
C  heading line of this topic.
C
10    CONTINUE
      JERROR=0
      MOVE=ILINE
      LEVELM(LEVEL)=ILINE
      DO I=1,MOVE-1
        READ(LHUNIT,'(1X)',END=110,ERR=110)
      end do
      OUTPUT=' '
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      READ(LHUNIT,'(2A)',END=110,ERR=110)LAB,INLINE
      CALL CHRWRT(INLINE,0,IOUNIT)
C
C  If 'going down' or redisplaying, (as opposed to backing up), display the
C  information available under the current topic.  We stop printing
C  when we hit a numeric label.  If this label is less than or equal to the
C  current level, there are no subtopics.  Otherwise, we now move ahead
C  to print out the list of subtopics available for this topic.
C
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
        CALL CHRWRT(INLINE,0,IOUNIT)
        GO TO 30
      ELSE
        NTOP=0
        INLINE=' '
        LAB=' '
      end if
C
C  Locate each subtopic by examining column 1, searching for integer labels.
C
C  Assuming we are at level LEVEL, we are searching for labels equal
C  to LEVEL+1.  As we encounter each such label, we want to store the
C  rest of the line as a subtopic.  We ignore labels greater than LEVEL+1
C  because these are sub-subtopics, and we cease our search when we
C  reach a label less than or equal to LEVEL.
C
40    CONTINUE
      IF(LGE(LAB,'0').AND.LLE(LAB,'9'))THEN
        READ(LAB,'(I1)')NUM
        IF(NUM.LE.LEVEL)GO TO 50
        IF(NUM.EQ.LEVEL+1)THEN
          NTOP=NTOP+1
          IF(NTOP.EQ.1)THEN
            OUTPUT=' '
            CALL CHRWRT(OUTPUT,0,IOUNIT)
            OUTPUT='Help is available on:'
            CALL CHRWRT(OUTPUT,0,IOUNIT)
            OUTPUT=' '
            CALL CHRWRT(OUTPUT,0,IOUNIT)
          end if
          WRITE(OUTPUT,'(1X,A75)')INLINE
          CALL CHRWRT(OUTPUT,0,IOUNIT)
          LEVELT(NTOP)=MOVE
          LEVELC(NTOP)=INLINE
        end if
      end if
      READ(LHUNIT,'(2A)',END=50,ERR=50)LAB,INLINE
      MOVE=MOVE+1
      GO TO 40
50    CONTINUE
C
C  Display subtopics
C
      OUTPUT=' '
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='RETURN to back up, ? to redisplay, CTRL-Z to quit.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
C
C  Prompt for user choice of new topic, exit, or back up
C
60    CONTINUE
      IERROR=0
      NLINE=0
      IF(NTOP.GT.0)THEN
        PROMPT='topic you want help on, or RETURN or ? or CTRL-Z.'
      ELSE
        PROMPT='RETURN or ? or CTRL-Z.'
      end if
      CALL CHRREA(CHOICE,LINE,NLINE,PROMPT,IOUNIT,IERROR,0)
      IF(IERROR.NE.0)THEN
        IERROR=0
        CLOSE(UNIT=LHUNIT)
        RETURN
      end if
      NPAGE=0
      CALL CHRDB2(CHOICE)
      CALL CHRLEN(CHOICE,LENC)
      IF(LENC.LE.0)CHOICE='!'
      CTEMP=CHOICE
      CALL CAPCHR(CTEMP)
C
C  Two errors in a row, OK, but three suggests that something is wrong.
C
      IF(IERROR.NE.0)THEN
        JERROR=JERROR+1
        IF(JERROR.LE.4)GO TO 60
        OUTPUT='Too many input errors in a row!'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
      end if
C
C  Consider ending this help session
C
      IF((CTEMP.EQ.'!'.AND.LEVEL.EQ.1).OR.
     *   JERROR.GT.4)THEN
        CLOSE(UNIT=LHUNIT)
        RETURN
      end if
C
C  User wants to back up to a supertopic.  We must rewind.
C
      REWIND LHUNIT
      LEVELO=LEVEL
      IF(CTEMP.EQ.'!')THEN
        LEVEL=LEVEL-1
        ILINE=LEVELM(LEVEL)
C
C  Redisplay current topic
C
      ELSEIF(CTEMP.EQ.'?')THEN
        GO TO 10
C
C  User wants to go down to a subtopic.
C
      ELSE
        DO 80 I=1,NTOP
          CTEMP2=LEVELC(I)
          CALL CHRDB2(CTEMP2)
          CALL CAPCHR(CTEMP2)
          ITOP=I
          DO J=1,LENC
            IF(CTEMP(J:J).NE.CTEMP2(J:J))GO TO 80
          end do
          GO TO 90
80        CONTINUE
        OUTPUT='Sorry, no help available on ' // CHOICE
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        JERROR=JERROR+1
        GO TO 60
90      CONTINUE
        LEVEL=LEVEL+1
        ILINE=LEVELT(ITOP)
      end if
      GO TO 10
C
C  Error opening help file
C
100   CONTINUE
      IERROR=1
      OUTPUT='Could not open the help file '//FILHLP
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
C
C  Error reading help file
C
110   CONTINUE
      IERROR=1
      OUTPUT='Unexpected end or error while reading '//FILHLP
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      CLOSE(UNIT=LHUNIT)
      RETURN
      END
      SUBROUTINE GRAFF(IERROR,IHAVE,IOUNIT,IPLOT,LINE,MAXPLT,MAXTAB,
     *MAXVAR,NAMES,NLINE,NTAB,OUTPUT,PROMPT,TABLE)

c*********************************************************************72
c
cc GRAFF
c
      CHARACTER CHRREL*14
      CHARACTER IGRAFX*3
      CHARACTER IGRAFY*3
      DIMENSION IHAVE(*)
      DIMENSION IOUNIT(*)
      DIMENSION IPLOT(2,MAXPLT)
      CHARACTER ISAY*1
      CHARACTER LINE*(*)
      CHARACTER NAMES(MAXVAR)*3
      CHARACTER OUTPUT*(*)
      CHARACTER PROMPT*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)
C
      COMMON /ANYCOM/ IPLT1,IPLT2,IXPLT1,IXPLT2,IYPLT1,
     *                IYPLT2,MARRAY,XPLT1,XPLT2,YPLT1,YPLT2
C
      IF(NTAB.LE.1)THEN
        OUTPUT='No data to plot!'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if
C
      NPLOT=0
10    CONTINUE
      NLINE=0
      PROMPT='horizontal variable (or RETURN), vertical variable'
      CALL CHRREA(IGRAFX,LINE,NLINE,PROMPT,IOUNIT,IERROR,1)
      IF(IERROR.NE.0)RETURN
      CALL CHRLEN(IGRAFX,LCHAR)
      IF(LCHAR.LE.0)GO TO 30
      CALL CAPCHR(IGRAFX)
      CALL CHRDB1(LINE)
      CALL CHRREA(IGRAFY,LINE,NLINE,PROMPT,IOUNIT,IERROR,1)
      IF(IERROR.NE.0)RETURN
      CALL CAPCHR(IGRAFY)
C
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
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        GO TO 10
      ELSEIF(IND2.EQ.0)THEN
        OUTPUT='I do not recognize the variable '//IGRAFY
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        GO TO 10
      end if
      IF(IGRAFX.EQ.IGRAFY)THEN
        OUTPUT='You cannot graph '//IGRAFX//' against itself!'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
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
      DO 50 I=1,NPLOT
        IND1=IPLOT(1,I)
        IND2=IPLOT(2,I)
        DO 40 J=1,NTAB
          XMAX=MAX(XMAX,TABLE(J,IND1))
          XMIN=MIN(XMIN,TABLE(J,IND1))
          YMAX=MAX(YMAX,TABLE(J,IND2))
          YMIN=MIN(YMIN,TABLE(J,IND2))
40        CONTINUE
50      CONTINUE
      PROMPT='Y to alter range of plot'
      CALL CHRREA(ISAY,LINE,NLINE,PROMPT,IOUNIT,IERROR,0)
      IF(ISAY.EQ.'Y')THEN
        OUTPUT='Horizontal range is '//CHRREL(XMIN)//' to'
     *  //CHRREL(XMAX)
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        PROMPT='new minimum, new maximum'
        CALL RELREA(XMIN,LINE,NLINE,PROMPT,IOUNIT,IERROR)
        CALL RELREA(XMAX,LINE,NLINE,PROMPT,IOUNIT,IERROR)
        OUTPUT='Vertical range is '//CHRREL(YMIN)//' to'
     *  //CHRREL(YMAX)
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        PROMPT='new minimum, new maximum'
        CALL RELREA(YMIN,LINE,NLINE,PROMPT,IOUNIT,IERROR)
        CALL RELREA(YMAX,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      end if
C
      XDEL=XMAX-XMIN
      XMIN=XMIN-0.05*XDEL
      XMAX=XMAX+0.05*XDEL
      YDEL=YMAX-YMIN
      YMIN=YMIN-0.05*YDEL
      YMAX=YMAX+0.05*YDEL
C
      XPLT1=0.00
      XPLT2=1.00
      YPLT1=0.00
      YPLT2=1.00
      IPLT1=1
      ICOM=2
      CALL ANYPLT(ICOM)
C
      XPLT1=XMIN
      XPLT2=XMAX-XMIN
      YPLT1=YMIN
      YPLT2=YMAX-YMIN
      ICOM=3
      CALL ANYPLT(ICOM)
C
C  Plot X and Y axis if they appear on the graph.
C
      IF(XMIN.LE.0.0.AND.0.0.LE.XMAX)THEN
        XPLT1=0.0
        YPLT1=YMIN
        ICOM=4
        CALL ANYPLT(ICOM)
        XPLT1=0.0
        YPLT1=YMAX
        ICOM=5
        CALL ANYPLT(ICOM)
      end if
C
      IF(YMIN.LE.0.0.AND.0.0.LE.YMAX)THEN
        XPLT1=XMIN
        YPLT1=0.0
        ICOM=4
        CALL ANYPLT(ICOM)
        XPLT1=XMAX
        YPLT1=0.0
        ICOM=5
        CALL ANYPLT(ICOM)
      end if
C
      DO 70 I=1,NPLOT
        ICOM=4
        IND1=IPLOT(1,I)
        IND2=IPLOT(2,I)
        DO 60 J=1,NTAB
          XPLT1=TABLE(J,IND1)
          YPLT1=TABLE(J,IND2)
          CALL ANYPLT(ICOM)
          ICOM=5
60        CONTINUE
70      CONTINUE
C
      ICOM=9
      CALL ANYPLT(ICOM)
      RETURN
      END
      SUBROUTINE HELPER(IOUNIT,OUTPUT)

c*********************************************************************72
c
cc HELPER prints out a list of available commands.
c
      DIMENSION IOUNIT(*)
      CHARACTER OUTPUT*(*)

      OUTPUT='B - set up new problem'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='C - integrate current problem '
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='D - Open/close transcript file.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='F - Graph the flow field T versus Y''.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='G - Graph solution components.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='H - Help (print this list).'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='O - Read solution data from file.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='P - Write solution data to file.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='Q - Quit.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='R - Read commands from file'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='S - Set up sample problem'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='T - Type out parameters'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='V - What parameters can I set?'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='W - Write problem data to file.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='? - Display extensive help'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='variable = value  Assign value to variable.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)

      RETURN
      END
      SUBROUTINE INDUMP(FILNAM,IERROR,IHAVE,IOUNIT,ITEMP,LINE,MAXTAB,
     *MAXVAR,NLINE,NTAB,PROMPT,TABLE)

c*********************************************************************72
c
cc INDUMP
c
      CHARACTER FILNAM*(*)
      DIMENSION IHAVE(MAXVAR)
      DIMENSION IOUNIT(*)
      DIMENSION ITEMP(MAXVAR)
      CHARACTER LINE*(*)
      CHARACTER PROMPT*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)
C
      PROMPT='filename to read saved data from.'
      CALL CHRREA(FILNAM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0)
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
C
30    CONTINUE
      WRITE(*,*)'INDUMP - Could not open dump file!'
      RETURN
C
40    CONTINUE
      WRITE(*,*)'INDUMP - Information was missing from the file!'
      CLOSE(UNIT=17)
      RETURN
      END
      SUBROUTINE INIT(FILHLP,IERROR,IOUNIT,MAXTAB,MAXVAR,METNAM,NAMES,
     *NPAGE,ROVER,TABLE)

c*********************************************************************72
c
cc INIT
c
      CHARACTER FILHLP*(*)
      DIMENSION IOUNIT(*)
      CHARACTER METNAM(*)*(*)
      CHARACTER NAMES(*)*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)
C
      FILHLP='upjode.hlp'
      IERROR=0
      IOUNIT(1)=0
      IOUNIT(2)=0
      IOUNIT(3)=-1
      IOUNIT(4)=-1
      METNAM(1)='Euler method'
      METNAM(2)='Runge-Kutta method of order 2'
      METNAM(3)='Runge Kutta method of order 4'
      METNAM(4)='Midpoint method'
      METNAM(5)='Runge-Kutta-Fehlberg method of order 5'
      METNAM(6)='Adams-Bashforth method of order 2'
      METNAM(7)='Adams-Bashforth method of order 4'
      METNAM(8)='Adams-Bashforth-Moulton Predictor/Corrector order 2'
      METNAM(9)='Adams-Bashforth-Moulton Predictor/Corrector order 4'
      METNAM(10)='Milne-Simpson Predictor/Corrector of order 3'
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
      RETURN
      END
      SUBROUTINE INTREA(INTVAL,LINE,NLINE,PROMPT,IOUNIT,IERROR)

c*********************************************************************72
c
cc INTREA
c
      DIMENSION IOUNIT(*)
      CHARACTER LINE*80
      CHARACTER OUTPUT*90
      CHARACTER PROMPT*80
      INTVAL=0
10    CONTINUE
      CALL CHRINP(IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
      IF(IERROR.NE.0)RETURN
      IF(NLINE.LE.0)GO TO 10
      CALL CHRCTI(LINE,NLINE,INTVAL,IERROR,LCHAR)
      CALL CHRCHP(LINE,1,LCHAR)
      CALL CHRLEN(LINE,NLINE)
      RETURN
      END
      SUBROUTINE NOTE(IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)

c*********************************************************************72
c
cc NOTE
c
      DIMENSION IOUNIT(*)
      CHARACTER LINE*(*)
      CHARACTER OUTPUT*(*)
      CHARACTER PROMPT*(*)
C
      OUTPUT='**************************************************'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      PROMPT='comments, followed by period in column 1.'
10    CONTINUE
      NLINE=0
      CALL CHRREA(OUTPUT,LINE,NLINE,PROMPT,IOUNIT,IERROR,0)
      PROMPT=' '
      IF(IERROR.NE.0)RETURN
      IF(OUTPUT(1:1).NE.'.')GO TO 10
      OUTPUT='**************************************************'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
      END
      SUBROUTINE QUIT(FILNAM,IERROR,IOUNIT,NLINE,LINE,OUTPUT,
     *PROMPT)

c*********************************************************************72
c
cc QUIT
c
      CHARACTER COM*1
      CHARACTER FILNAM*(*)
      DIMENSION IOUNIT(*)
      CHARACTER LINE*(*)
      CHARACTER OUTPUT*(*)
      CHARACTER PROMPT*(*)
C
      NLINE=0
      PROMPT='Y to confirm you want to stop'
      CALL CHRREA(COM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0)
      IF(IERROR.NE.0)RETURN
      CALL CAPCHR(COM)
      IF(COM.NE.'Y')RETURN
C
C  Shut down
C
      OUTPUT='UPJODE is stopping now.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      IF(IOUNIT(3).NE.(-1))
     *CALL DISK(FILNAM,IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
      ICOM=1
      CALL ANYPLT(ICOM)
      STOP
      END
      SUBROUTINE READER(FILNAM,IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)

c*********************************************************************72
c
cc READER
c
      CHARACTER FILNAM*(*)
      INTEGER   IERROR
      INTEGER   IOUNIT(*)
      CHARACTER LINE*(*)
      INTEGER   NLINE
      CHARACTER OUTPUT*(*)
      CHARACTER PROMPT*(*)
C
      PROMPT='input filename.'
      CALL CHRREA(FILNAM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0)
      IF(IERROR.NE.0)RETURN
      IOUNIT(1)=31
      OPEN(UNIT=IOUNIT(1),FILE=FILNAM,STATUS='OLD',ERR=10)
      OUTPUT='Input will come from '//FILNAM
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
C
C  File not found
C
10    CONTINUE
      OUTPUT='File could not be opened.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
      END
      SUBROUTINE RELREA(RVAL,LINE,NLINE,PROMPT,IOUNIT,IERROR)

c*********************************************************************72
c
cc RELREA reads a real number from the input string.
c
      DIMENSION IOUNIT(*)
      CHARACTER LINE*80
      CHARACTER OUTPUT*90
      CHARACTER PROMPT*80
      RVAL=0.0
10    CONTINUE
      CALL CHRINP(IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
      IF(IERROR.NE.0)RETURN
      IF(NLINE.LE.0)GO TO 10
      CALL CHRCTR(LINE,NLINE,TOP,IERROR,LCHAR)
      IF(LINE(LCHAR+1:LCHAR+1).NE.'/')THEN
        RVAL=TOP
      ELSE
        LCHAR=LCHAR+1
        CALL CHRCHP(LINE,1,LCHAR)
        CALL CHRCTR(LINE,NLINE,BOT,IERROR,LCHAR)
        IF(BOT.EQ.0.0)BOT=1.0
        RVAL=TOP/BOT
      end if
      CALL CHRCHP(LINE,1,LCHAR)
      CALL CHRLEN(LINE,NLINE)
      RETURN
      END
      SUBROUTINE RPNCHK(IERROR,IHI,ILO,IOPSYM,IRPN,MAXRPN,MAXSYM)

c*********************************************************************72
C
cc RPNCHK checks an RPN string.
c
C  Starting at the location IHI, find the position ILO such that
C  IRPN(ILO)...IRPN(IHI) represents a single argument, that is,
C  a complete RPN expression
C
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
cc RPNSET
c
      DIMENSION INTSYM(MAXSYM)
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IRPN(MAXFIX)
      DIMENSION ISTACK(MAXSYM)
      CHARACTER OUTPUT*100
      CHARACTER SYM*10
      CHARACTER SYM2*10
      CHARACTER SYMBOL(MAXSYM)*10
      NRPN=0
      IERROR=0
      DO I=1,MAXFIX
        IRPN(I)=0
      end do
C
C  Null formula
C
      IF(NINT.LE.0)THEN
        OUTPUT='RPNSET - Null formula?'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
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
        CALL CHRWRT(OUTPUT,0,IOUNIT)
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
C
C  Variable or constant goes immediately into IRPN
C
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
C
C  Done, pop stack
C
60    CONTINUE
      IF(ISTAK.LE.0)THEN
        NRPN=NRPN+1
        IRPN(NRPN)=IFINIS
        IF(IREAD.LT.NINT)THEN
          OUTPUT='RPNSET - Some of formula left over'
          CALL CHRWRT(OUTPUT,0,IOUNIT)
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
cc RPNVAL
c
      CHARACTER CTEMP*3
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IPVAL(MAXSYM)
      DIMENSION IRPN(MAXRPN)
      DIMENSION ISTACK(MAXSYM)
      DIMENSION LENSYM(MAXSYM)
      CHARACTER OUTPUT*100
      CHARACTER SYM*10
      CHARACTER SYMBOL(MAXSYM)*10
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
        CALL CHRWRT(OUTPUT,0,IOUNIT)
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
C
C  Constants and variables go into stack
C
      IF(IOPSYM(ISYM).EQ.0)THEN
        IF(ISYM.LE.NSYM)THEN
          ISTAK=ISTAK+1
          ISTACK(ISTAK)=ISYM
          ISYMO=ISYM
        ELSE
          IF(NSYMS.GE.MAXSYM)THEN
            OUTPUT='RPNVAL - Not enough storage.'
            CALL CHRWRT(OUTPUT,0,IOUNIT)
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
          CALL CHRITC(CTEMP,3,ITEMP)
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
C
C  Pull off arguments
C
      IARG1=ISTACK(ISTAK)
      IARG2=0
      IF(IOPSYM(ISYM).EQ.2)THEN
        IARG2=ISTACK(ISTAK)
        ISTAK=ISTAK-1
        IARG1=ISTACK(ISTAK)
      end if
      SYM=SYMBOL(ISYM)
      CALL FUNVAL(IARG1,IARG2,IERROR,IOPSYM,IPRSYM,IPVAL,
     *ITEMP,LENSYM,MAXSYM,MAXVAL,NSYMS,SYM,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IPSET,IFREE)
      ISYMO=NSYMS
      IF(IERROR.NE.0)THEN
        OUTPUT='RPNVAL - Evaluation abandoned.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
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
      SUBROUTINE SAMPLE(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *MAXEQN,MAXRPN,MAXTAB,MAXVAR,METHOD,NEQN,
     *NSTEPS,NTAB,OUTPUT,TABLE,TSTOP)

c*********************************************************************72
c
cc SAMPLE sets up a sample problem.
c
      DIMENSION IHAVE(MAXVAR)
      CHARACTER INFIX*(*)
      integer iounit(*)
      CHARACTER IRHS(*)*(*)
      DIMENSION IRPN(MAXRPN)
      CHARACTER NAMVAR*10
      DIMENSION TABLE(MAXTAB,MAXVAR)
      CHARACTER OUTPUT*(*)
C
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
      CALL COMRPN('G',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,VALUE,IOUNIT)
      IF(IERROR.NE.0)RETURN
      IHAVE(1+2*MAXEQN+1)=1
      INFIX='SIN(T)'
      IRHS(MAXEQN+1)=INFIX
      IFRM=MAXEQN+1
      CALL COMRPN('G',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,VALUE,IOUNIT)
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
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
      END
      SUBROUTINE SETVAL(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *LINE,MAXEQN,MAXMET,MAXRPN,MAXTAB,MAXVAR,METHOD,METNAM,
     *NAMVAR,NEQN,NLINE,NSTEPS,NTAB,OUTPUT,PROMPT,ROVER,TABLE,TSTOP)

c*********************************************************************72
C
cc SETVAL assigns a value to a symbol.
c
      CHARACTER CHRINT*6 
      DIMENSION IHAVE(*)
      CHARACTER INFIX*(*)
      DIMENSION IOUNIT(*)
      CHARACTER IRHS(*)*(*)
      DIMENSION IRPN(*)
      CHARACTER LINE*(*)
      LOGICAL   LGE
      LOGICAL   LLT
      CHARACTER METNAM(MAXMET)*60
      CHARACTER NAMVAR*(*)
      CHARACTER OUTPUT*(*)
      CHARACTER PROMPT*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)
C
201   CONTINUE
C
C  Read value
C
      PROMPT=NAMVAR
      IF(NAMVAR.EQ.'X1'.OR.NAMVAR.EQ.'Y1'''.OR.
     *   NAMVAR.EQ.'X2'.OR.NAMVAR.EQ.'Y2'''.OR.
     *   NAMVAR.EQ.'X3'.OR.NAMVAR.EQ.'Y3'''.OR.
     *   NAMVAR.EQ.'X4'.OR.NAMVAR.EQ.'Y4''')THEN
        CALL CHRREA(INFIX,LINE,NLINE,PROMPT,IOUNIT,IERROR,0)
      ELSEIF(LLT(NAMVAR,'I').OR.LGE(NAMVAR,'O'))THEN
        CALL RELREA(TEMP,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      ELSE
        CALL INTREA(ITEMP,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      end if
      IF(IERROR.NE.0)RETURN
C
C  Assign value to proper variable
C
      IF(NAMVAR.EQ.'IPRINT')THEN
        IPRINT=ITEMP
      ELSEIF(NAMVAR.EQ.'METHOD')THEN
        IF(ITEMP.GE.1.AND.ITEMP.LE.MAXMET)THEN
          METHOD=ITEMP
          OUTPUT='Method='//METNAM(ITEMP)
          CALL CHRWRT(OUTPUT,0,IOUNIT)
        ELSE
          OUTPUT='The following methods are available'
          CALL CHRWRT(OUTPUT,0,IOUNIT)
          DO 205 I=1,MAXMET
            OUTPUT=CHRINT(I)//'  '//METNAM(I)
            CALL CHRWRT(OUTPUT,0,IOUNIT)
205         CONTINUE
          GO TO 201
        end if
      ELSEIF(NAMVAR.EQ.'NEQN')THEN
        IF(ITEMP.GE.1.AND.ITEMP.LE.MAXEQN)THEN
          NEQN=ITEMP
        ELSE
          OUTPUT='NEQN must be between 1 and '//CHRINT(MAXEQN)
          CALL CHRDB2(OUTPUT)
          CALL CHRWRT(OUTPUT,0,IOUNIT)
        end if
      ELSEIF(NAMVAR.EQ.'NSTEPS')THEN
        NSTEPS=ITEMP
      ELSEIF(NAMVAR.EQ.'ROVER')THEN
        ROVER=TEMP
      ELSEIF(NAMVAR.EQ.'T')THEN
        NTAB=1
        TABLE(1,1)=TEMP
        IHAVE(1)=1
      ELSEIF(NAMVAR.EQ.'TINIT')THEN
        NTAB=1
        TABLE(1,1)=TEMP
        IHAVE(1)=1
      ELSEIF(NAMVAR.EQ.'TSTOP')THEN
        TSTOP=TEMP
      ELSEIF(NAMVAR.EQ.'Y1')THEN
        NTAB=1
        TABLE(1,2)=TEMP
        IHAVE(2)=1
      ELSEIF(NAMVAR.EQ.'Y2')THEN
        NTAB=1
        TABLE(1,4)=TEMP
        IHAVE(4)=1
      ELSEIF(NAMVAR.EQ.'Y3')THEN
        NTAB=1
        TABLE(1,6)=TEMP
        IHAVE(6)=1
      ELSEIF(NAMVAR.EQ.'Y4')THEN
        NTAB=1
        TABLE(1,8)=TEMP
        IHAVE(8)=1
      ELSEIF(NAMVAR.EQ.'Y1'''.OR.NAMVAR.EQ.'X1'.OR.
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
          CALL COMRPN('G',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,VALUE,
     *    IOUNIT)
        end if
        IF(IERROR.NE.0)THEN
          OUTPUT='Formula was not acceptable.'
          CALL CHRWRT(OUTPUT,0,IOUNIT)
          ITAG=0
        ELSEIF(IRHS(IFRM).EQ.' ')THEN
          OUTPUT='This variable will now be undefined.'
          CALL CHRWRT(OUTPUT,0,IOUNIT)
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
        OUTPUT='UPJODE did not recognize the variable you were'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='trying to set.  Use the "V" command for a list.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
      end if
      RETURN
      END
      SUBROUTINE SETUP(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *ISET,LINE,MAXEQN,MAXMET,MAXRPN,MAXTAB,MAXVAR,METHOD,METNAM,
     *NAMVAR,NEQN,NLINE,NSTEPS,NTAB,OUTPUT,PROMPT,ROVER,TABLE,TSTOP)

c*********************************************************************72
c
cc SETUP sets up a problem.
c
      DIMENSION IHAVE(*)
      CHARACTER INFIX*(*)
      DIMENSION IOUNIT(*)
      CHARACTER IRHS(*)*(*)
      DIMENSION IRPN(*)
      CHARACTER LINE*(*)
      CHARACTER METNAM(*)*(*)
      CHARACTER NAMVAR*(*)
      CHARACTER OUTPUT*(*)
      CHARACTER PROMPT*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)
C
      ISET=0
10    CONTINUE
C
      IF(IERROR.NE.0)THEN
        OUTPUT='Error occurred while setting up problem.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if
C
      ISET=ISET+1
      IF(ISET.EQ.1)THEN
        OUTPUT='NEQN is the number of equations, between 1 and 4.'
        NAMVAR='NEQN'
      ELSEIF(ISET.EQ.2)THEN
        OUTPUT='TINIT is the initial value of the independent variable.'
        NAMVAR='TINIT'
      ELSEIF(ISET.EQ.3)THEN
        OUTPUT='Y1 is the initial value of the first ODE component.'
        NAMVAR='Y1'
        IF(NEQN.EQ.1)ISET=6
      ELSEIF(ISET.EQ.4)THEN
        OUTPUT='The initial value of Y2 is needed.'
        NAMVAR='Y2'
        IF(NEQN.EQ.2)ISET=6
      ELSEIF(ISET.EQ.5)THEN
        OUTPUT='The initial value of Y3 is needed.'
        NAMVAR='Y3'
        IF(NEQN.EQ.3)ISET=6
      ELSEIF(ISET.EQ.6)THEN
        OUTPUT='The initial value of Y4 is needed.'
        NAMVAR='Y4'
      ELSEIF(ISET.EQ.7)THEN
        OUTPUT='We need to enter the right hand sides of the form'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='Y1''=F(T,Y1) (or perhaps a more complicated form).'
        NAMVAR='Y1'''
      ELSEIF(ISET.EQ.8)THEN
        IF(NEQN.LE.1)GO TO 10
        OUTPUT='We need the formula for Y2'''
        NAMVAR='Y2'''  
      ELSEIF(ISET.EQ.9)THEN
        IF(NEQN.LE.2)GO TO 10
        OUTPUT='We need the formula for Y3'''
        NAMVAR='Y3'''
      ELSEIF(ISET.EQ.10)THEN
        IF(NEQN.LE.3)GO TO 10
        OUTPUT='We need the formula for Y4'''
        NAMVAR='Y4'''
      ELSEIF(ISET.EQ.11)THEN
        OUTPUT='If a formula for Y1 is known, enter it now, or a blank.'
        NAMVAR='X1'
      ELSEIF(ISET.EQ.12)THEN
        IF(NEQN.LE.1)GO TO 10
        OUTPUT='We can accept the formula for Y2 now, or a blank.'
        NAMVAR='X2'  
      ELSEIF(ISET.EQ.13)THEN
        IF(NEQN.LE.2)GO TO 10
        OUTPUT='We can accept the formula for Y3 now, or a blank.'
        NAMVAR='X3'
      ELSEIF(ISET.EQ.14)THEN
        IF(NEQN.LE.3)GO TO 10
        OUTPUT='We can accept the formula for Y4 now, or a blank.'
        NAMVAR='X4'
      ELSEIF(ISET.EQ.15)THEN
        OUTPUT='The number of the ODE method is needed. (1-10)'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='0=Help, 1=Euler, 2=Mid, 3=RK2, 4=RK4, 5=RK5,'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='6=AB2, 7=AB4, 8=ABM2, 9=ABM4, 10=MS4'
        NAMVAR='METHOD'
      ELSEIF(ISET.EQ.16)THEN
        OUTPUT='Please specify the number of steps to take (1-200)'
        NAMVAR='NSTEPS'
      ELSEIF(ISET.EQ.17)THEN
        OUTPUT='Please specify the value of T at which to stop.'
        NAMVAR='TSTOP'
      ELSE
        OUTPUT='Problem has been set up'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='Use the C command to integrate.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      CALL CHRLEN(LINE,NLINE)
      CALL SETVAL(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *LINE,MAXEQN,MAXMET,MAXRPN,MAXTAB,MAXVAR,METHOD,METNAM,
     *NAMVAR,NEQN,NLINE,NSTEPS,NTAB,OUTPUT,PROMPT,ROVER,TABLE,TSTOP)
      GO TO 10
      END
      SUBROUTINE SHOSET(IOUNIT,OUTPUT)

c*********************************************************************72
C
cc SHOSET shows settings.
c
      DIMENSION IOUNIT(*)
      CHARACTER OUTPUT*(*)
C
      OUTPUT='Here are the parameters you can set via commands like'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='  "IPRINT=5" or TINIT=4.5 or "Y1''=SIN(T)"'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT=' '
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='IPRINT - printout frequency'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='METHOD - integration method'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='NEQN   - number of equations'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='NSTEPS - number of steps to take'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='ROVER  - overflow threshhold'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='TINIT  - initial time'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='TSTOP  - stopping time'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='Y1     - initial condition (also Y2, Y3, or Y4)'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='Y1''    - formula for derivative of Y1.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='X1     - exact formula for Y1, if known.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT=' '
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
      END
      SUBROUTINE STEP(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRPN,
     *MAXEQN,MAXRPN,MAXTAB,MAXVAR,METHOD,NAMES,NEQN,
     *NSTEPS,NTAB,OUTPUT,ROVER,TABLE,TSTOP)

c*********************************************************************72
c
cc STEP
c
      PARAMETER (MXEQN=4)
      DIMENSION IHAVE(MAXVAR)
      CHARACTER INFIX*(*)
      DIMENSION IOUNIT(*)
      DIMENSION IRPN(MAXRPN)
      CHARACTER NAMES(MAXVAR)*3
      CHARACTER NAMVAR*10
      CHARACTER OUTPUT*120
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
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if

      IF(ABS(TABLE(1,1)-TSTOP).LT.0.00001*ABS(TSTOP))THEN
        IERROR=1
        OUTPUT='Please set TSTOP so we go somewhere!'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if

      TVAL=TABLE(1,1)
      DO I=1,NEQN
        YTEMP(I)=TABLE(1,2*I)
      end do
      CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *YPTEMP,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
      IF(IERROR.NE.0)RETURN
      DO I=1,NEQN
        TABLE(1,2*I+1)=YPTEMP(I)
      end do

      CALL COMRPN('V',IERROR,IFRM,INFIX,IRPN,MAXRPN,'T',TVAL,IOUNIT)
      IF(IERROR.NE.0)RETURN
      DO I=1,NEQN
        IVAR=1+2*MAXEQN+I
        IF(IHAVE(IVAR).NE.1)GO TO 141
        IFRM=MAXEQN+I
        CALL COMRPN('E',IERROR,IFRM,INFIX,IRPN,
     *  MAXRPN,NAMVAR,VALUE,IOUNIT)
        IF(IERROR.NE.0)RETURN
        TABLE(1,IVAR)=VALUE
141     CONTINUE
      end do

      OUTPUT=' '
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      IF(NEQN.EQ.1.AND.IHAVE(1+2*MAXEQN+1).EQ.1)THEN
        OUTPUT='Step      T              Y1             X1'
      ELSEIF(NEQN.EQ.1)THEN
        OUTPUT='Step      T              Y1             Y1'''
      ELSE
        WRITE(OUTPUT,1270)(' ',J,J=1,NEQN)
      end if
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT=' '
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      INDHI=2*NEQN
      ITEMP=NTAB-1
      IF(NEQN.EQ.1.AND.IHAVE(1+2*MAXEQN+1).EQ.1)THEN
        WRITE(OUTPUT,1280)ITEMP,TABLE(1,1),TABLE(1,2),
     *  TABLE(1,1+2*MAXEQN+1)
      ELSEIF(NEQN.EQ.1)THEN
        WRITE(OUTPUT,1280)ITEMP,(TABLE(1,I),I=1,3)
      ELSE
        WRITE(OUTPUT,1280)ITEMP,TABLE(1,1),(TABLE(1,IND),IND=2,INDHI,2)
      end if
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      TBEGIN=TABLE(1,1)
      DELT=(TSTOP-TBEGIN)/FLOAT(NSTEPS)
      DO 560 ISTEP=1,NSTEPS
C
C  Update time = position where solution is sought
C
        TOLD=TABLE(NTAB,1)
        TIME=((NSTEPS-ISTEP)*TBEGIN+ISTEP*TSTOP)/REAL(NSTEPS)
        TABLE(NTAB+1,1)=TIME
        GO TO (180,230,260,200,320,400,420,440,470,500)METHOD
C
C  METHOD=1, Euler method
C
  180   CONTINUE

        DO I=1,NEQN
          TEMP1=TABLE(NTAB,2*I+1)
          TABLE(NTAB+1,2*I)=TABLE(NTAB,2*I)+DELT*TEMP1
        end do
        GO TO 530
C
C  METHOD=2, Runge Kutta method order 2
C
230     CONTINUE
        TVAL=TIME
        DO I=1,NEQN
          YTEMP(I)=TABLE(NTAB,2*I)+DELT*TABLE(NTAB,2*I+1)
        end do
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YPTEMP,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        DO I=1,NEQN
          TEMP1=0.5*(TABLE(NTAB,2*I+1)+YPTEMP(I))
          TABLE(NTAB+1,2*I)=TABLE(NTAB,2*I)+DELT*TEMP1
        end do
        GO TO 530
C
C  METHOD=3, Runge-Kutta of order 4
C
260     CONTINUE
        DO I=1,NEQN
          YK1(I)=TABLE(NTAB,2*I+1)
        end do
        TVAL=TOLD+0.50*DELT
        DO I=1,NEQN
          YTEMP(I)=TABLE(NTAB,2*I)+0.5*DELT*YK1(I)
        end do
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YK2,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        DO I=1,NEQN
          YTEMP(I)=TABLE(NTAB,2*I)+0.5*DELT*YK2(I)
        end do
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YK3,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        TVAL=TOLD+DELT
        DO I=1,NEQN
          YTEMP(I)=TABLE(NTAB,2*I)+DELT*YK3(I)
        end do
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YK4,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        DO I=1,NEQN
          TEMP=(YK1(I)+2.0*YK2(I)+2.0*YK3(I)+YK4(I))/6.0
          TABLE(NTAB+1,2*I)=TABLE(NTAB,2*I)+DELT*TEMP
        end do
        GO TO 530
C
C  METHOD=4, Modified Euler method
C
  200   CONTINUE
        DO 210 I=1,NEQN
          YTEMP(I)=TABLE(NTAB,2*I)+0.5*DELT*TABLE(NTAB,2*I+1)
  210     CONTINUE
        TVAL=TOLD+0.5*DELT
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YPTEMP,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        DO I=1,NEQN
          TABLE(NTAB+1,2*I)=TABLE(NTAB,2*I)+DELT*YPTEMP(I)
        end do
        GO TO 530
C
C  METHOD=5, Runge-Kutta-Fehlberg method of orders 5 and 6
C
320     CONTINUE
        TVAL=TOLD
        DO I=1,NEQN
          YK1(I)=TABLE(NTAB,2*I+1)
        end do
        TVAL=TOLD+0.25*DELT
        DO 340 I=1,NEQN
          YTEMP(I)=TABLE(NTAB,2*I)+0.25*DELT*YK1(I)
  340     CONTINUE
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YK2,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        TVAL=TOLD+3.0*DELT/8.0
        DO 350 I=1,NEQN
          YTEMP(I)=TABLE(NTAB,2*I)+DELT*(3.0*YK1(I)+9.0*YK2(I))/32.0
  350     CONTINUE
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YK3,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        TVAL=TOLD+12.0*DELT/13.0
        DO 360 I=1,NEQN
          TEMP1=1932.0*YK1(I)
          TEMP2=-7200.0*YK2(I)
          TEMP3=7296.0*YK3(I)
          YTEMP(I)=TABLE(NTAB,2*I)+DELT*(TEMP1+TEMP2+TEMP3)/2197.0
  360     CONTINUE
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YK4,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        TVAL=TIME
        DO 370 I=1,NEQN
          TEMP1=439.0*YK1(I)/216.0
          TEMP2=-8.0*YK2(I)
          TEMP3=3680.0*YK3(I)/513.0
          TEMP4=-845.0*YK4(I)/4104.0
          YTEMP(I)=TABLE(NTAB,2*I)+DELT*(TEMP1+TEMP2+TEMP3+TEMP4)
  370     CONTINUE
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YK5,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        TVAL=TOLD+0.5*DELT
        DO I=1,NEQN
          TEMP1=-8.0*YK1(I)/27.0
          TEMP2=2.0*YK2(I)
          TEMP3=-3544.0*YK3(I)/2565.0
          TEMP4=1859.0*YK4(I)/4104.0
          TEMP5=-11.0*YK5(I)/40.0
          YTEMP(I)=TABLE(NTAB,2*I)+DELT*(TEMP1+TEMP2+TEMP3+TEMP4+TEMP5)
        end do
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YK6,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        DO 390 I=1,NEQN
          TEMP1=25.0*YK1(I)/216.0
          TEMP2=1408.0*YK3(I)/2565.0
          TEMP3=2197.0*YK4(I)/4104.0
          TEMP4=-YK5(I)/5.0
          TEMP=TEMP1+TEMP2+TEMP3+TEMP4
          TABLE(NTAB+1,2*I)=TABLE(NTAB,2*I)+DELT*TEMP
  390     CONTINUE
        GO TO 530
C
C  METHOD=6, Adams-Bashforth method of order 2
C
400     CONTINUE
        IF(NTAB.LT.2) GO TO 230
        DO 410 I=1,NEQN
          TEMP=3.0*TABLE(NTAB,2*I+1)-TABLE(NTAB-1,2*I+1)
          TABLE(NTAB+1,2*I)=TABLE(NTAB,2*I)+0.5*DELT*TEMP
  410     CONTINUE
        GO TO 530
C
C  METHOD=7, Adams-Bashforth method of order 4
C
420     CONTINUE
        IF(NTAB.LT.4) GO TO 260
        DO 430 I=1,NEQN
          TEMP1=55.0*TABLE(NTAB,2*I+1)/24.0
          TEMP2=-59.0*TABLE(NTAB-1,2*I+1)/24.0
          TEMP3=37.0*TABLE(NTAB-2,2*I+1)/24.0
          TEMP4=-9.0*TABLE(NTAB-3,2*I+1)/24.0
          TEMP=TEMP1+TEMP2+TEMP3+TEMP4
          TABLE(NTAB+1,2*I)=TABLE(NTAB,2*I)+DELT*TEMP
  430     CONTINUE
        GO TO 530
C
C  METHOD=8, Adams-Bashforth predictor of order 2,
C            Adams-Moulton corrector of order 2
C
440     CONTINUE
        IF(NTAB.LT.2)GO TO 230
        DO 450 I=1,NEQN
          TEMP=3.0*TABLE(NTAB,2*I+1)-TABLE(NTAB-1,2*I+1)
          YTEMP(I)=TABLE(NTAB,2*I)+0.5*DELT*TEMP
  450     CONTINUE
        TVAL=TIME
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YPTEMP,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        DO 460 I=1,NEQN
          TEMP=YPTEMP(I)+TABLE(NTAB,2*I+1)
          TABLE(NTAB+1,2*I)=TABLE(NTAB,2*I)+0.5*DELT*TEMP
  460     CONTINUE
        GO TO 530
C
C  METHOD=9, Adams-Bashforth predictor of order 4
C            Adams-Moulton corrector of order 4
C
470     CONTINUE
        IF(NTAB.LT.4) GO TO 260
        DO 480 I=1,NEQN
          TEMP1=55.0*TABLE(NTAB,2*I+1)/24.0
          TEMP2=-59.0*TABLE(NTAB-1,2*I+1)/24.0
          TEMP3=37.0*TABLE(NTAB-2,2*I+1)/24.0
          TEMP4=-9.0*TABLE(NTAB-3,2*I+1)/24.0
          YTEMP(I)=TABLE(NTAB,2*I)+DELT*(TEMP1+TEMP2+TEMP3+TEMP4)
  480     CONTINUE
        TVAL=TIME
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YPTEMP,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        DO I=1,NEQN
          TEMP1=9.0*YPTEMP(I)/24.0
          TEMP2=19.0*TABLE(NTAB,2*I+1)/24.0
          TEMP3=-5.0*TABLE(NTAB-1,2*I+1)/24.0
          TEMP4=TABLE(NTAB-2,2*I+1)/24.0
          TEMP=TEMP1+TEMP2+TEMP3+TEMP4
          TABLE(NTAB+1,2*I)=TABLE(NTAB,2*I)+DELT*TEMP
        end do
        GO TO 530
C
C  METHOD=10, Milne predictor of order 3
C             Simpson corrector of order 3
C
500     CONTINUE
        IF(NTAB.LT.4) GO TO 260
        DO 510 I=1,NEQN
          TEMP1=8.0*TABLE(NTAB,2*I+1)/3.0
          TEMP2=-4.0*TABLE(NTAB-1,2*I+1)/3.0
          TEMP3=8.0*TABLE(NTAB-2,2*I+1)/3.0
          YTEMP(I)=TABLE(NTAB-3,2*I)+DELT*(TEMP1+TEMP2+TEMP3)
  510     CONTINUE
        TVAL=TIME
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *  YPTEMP,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        DO 520 I=1,NEQN
          TEMP1=YPTEMP(I)/3.0
          TEMP2=4.0*TABLE(NTAB,2*I+1)/3.0
          TEMP3=TABLE(NTAB-1,2*I+1)/3.0
          TABLE(NTAB+1,2*I)=TABLE(NTAB-1,2*I)+DELT*(TEMP1+TEMP2+TEMP3)
  520     CONTINUE
        GO TO 530
C
C  All join here
C
530     CONTINUE
        DO 540 I=1,NEQN
          YTEMP(I)=TABLE(NTAB+1,2*I)
          IF(ABS(YTEMP(I)).GT.ROVER)THEN
            IERROR=100
            OUTPUT='Overflow in value of solution!'
            CALL CHRWRT(OUTPUT,0,IOUNIT)
            RETURN
          end if
  540     CONTINUE
        CALL COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TIME,YTEMP,
     *  YPTEMP,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        DO 550 I=1,NEQN
          TABLE(NTAB+1,1+2*I)=YPTEMP(I)
  550     CONTINUE
C
C  Evaluate exact formulas
C
        VALUE=TABLE(NTAB+1,1)
        CALL COMRPN('V',IERROR,IFRM,INFIX,IRPN,
     *  MAXRPN,'T',VALUE,IOUNIT)
        IF(IERROR.NE.0)RETURN
        DO 551 I=1,NEQN
          IVAR=1+2*MAXEQN+I
          IF(IHAVE(IVAR).NE.1)GO TO 551
          IFRM=MAXEQN+I
          CALL COMRPN('E',IERROR,IFRM,INFIX,IRPN,
     *    MAXRPN,NAMVAR,VALUE,IOUNIT)
          IF(IERROR.NE.0)RETURN
          TABLE(NTAB+1,IVAR)=VALUE
551       CONTINUE
        NTAB=NTAB+1
        IF(IPRINT.GT.0)THEN
          IF(MOD(ISTEP,IPRINT).EQ.0)THEN
            INDHI=2*NEQN
            ITEMP=NTAB-1
            IF(NEQN.EQ.1.AND.IHAVE(1+2*MAXEQN+1).EQ.1)THEN
              WRITE(OUTPUT,1280)ITEMP,TABLE(NTAB,1),TABLE(NTAB,2),
     *        TABLE(NTAB,1+2*MAXEQN+1)
            ELSEIF(NEQN.EQ.1)THEN
              WRITE(OUTPUT,1280)ITEMP,(TABLE(NTAB,IND),IND=1,3)
            ELSE
              WRITE(OUTPUT,1280)
     1        ITEMP,TABLE(NTAB,1),(TABLE(NTAB,IND),IND=2,INDHI,2)
            end if
            CALL CHRWRT(OUTPUT,0,IOUNIT)
          end if
        end if
  560   CONTINUE
      RETURN
 1270 FORMAT('Step      T ',12X,8(A1,'Y',I1,12X))
 1280 FORMAT(I4,9G16.8)
      END
      SUBROUTINE SYMADD(IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     *MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IFREE)

c*********************************************************************72
C
cc SYMADD adds a symbol to the list.
c
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IPVAL(MAXSYM)
      DIMENSION LENSYM(MAXSYM)
      CHARACTER NAMVAR*10
      CHARACTER OUTPUT*100
      CHARACTER SYMBOL(MAXSYM)*10
      DIMENSION VALSYM(MAXVAL)
C
      IERROR=0
      CALL CHRLEN(NAMVAR,LENNAM)
      IF(LENNAM.LE.0)THEN
        OUTPUT='SYMADD - Null name?'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        IERROR=1
        RETURN
      end if
C
C  Check for name already in use
C
      DO I=1,NSYMP
        IF(NAMVAR.EQ.SYMBOL(I))THEN
          OUTPUT='SYMADD - Name '//NAMVAR//' reserved.'
          CALL CHRWRT(OUTPUT,1,IOUNIT)
          IERROR=1
          RETURN
        end if
      end do
C
C  Is there room for another symbol?
C
      IF(NSYM.GE.MAXSYM)THEN
        OUTPUT='SYMADD - No room to add '//NAMVAR
        CALL CHRWRT(OUTPUT,1,IOUNIT)
        IERROR=1
        RETURN
      end if
      IF(IFREE.GT.MAXVAL)THEN
        IERROR=1
        OUTPUT='SYMADD - No room to add '//NAMVAR
        CALL CHRWRT(OUTPUT,1,IOUNIT)
        RETURN
      end if
C
C  Insert symbol.
C
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
C
cc SYMCHK converts a string to a list of symbols.
c
C  Input is INFIX.  Output is INTSYM, containing integers standing for
C  the recognized variable names, or constants used in the formula.
C
      CHARACTER INFIX*(*)
      DIMENSION INTSYM(MAXSYM)
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IPVAL(MAXSYM)
      DIMENSION LENSYM(MAXSYM)
      CHARACTER NAMVAR*10
      LOGICAL   NOTLET
      LOGICAL   NOTNUM
      CHARACTER OUTPUT*100
      CHARACTER SYM*1
      CHARACTER SYM1*10
      CHARACTER SYM2*10
      CHARACTER SYM3*10
      CHARACTER SYMBOL(MAXSYM)*10
      DIMENSION VALSYM(MAXVAL)
      IMPLIC=0
      IERROR=0
      NINT=0
      MATCH=0
      IPOS=1
C
C  Consider possibility of implicit declaration of new variable
C  via assignment statement.
C
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
      CALL SYMADD(IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     *MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IFREE)
      NINT=NINT+1
      INTSYM(NINT)=NSYM
      MATCH=NSYM
      IMPLIC=NSYM
      OUTPUT='SYMCHK - Implicit definition of '//NAMVAR
      CALL CHRWRT(OUTPUT,1,IOUNIT)
C
C  Process next chunk of formula
C
30    CONTINUE
      IF(IPOS.GT.LENFIX)THEN
        NINT=NINT+1
        INTSYM(NINT)=IFINIS
        RETURN
      end if
      MATCHO=MATCH
      MATCH=0
      LENMAT=0
      DO 50 I=1,NSYM
        LENS=LENSYM(I)
        IF(LENS.LE.LENMAT)GO TO 50
        IF((IPOS-1+LENS).GT.LENFIX)GO TO 50
        DO J=1,LENS
          IF(INFIX(IPOS-1+J:IPOS-1+J).NE.SYMBOL(I)(J:J))GO TO 50
        end do
        LENMAT=LENS
        MATCH=I
50      CONTINUE
      IF(MATCH.EQ.0)GO TO 110
C
C  Watch out for new, implicit symbol whose initial part matches old symbol
C
      IF(IOPSYM(MATCH).NE.0)GO TO 70
      IF(IPOS+LENMAT-1.GE.LENFIX)GO TO 70
      INEXT=IPOS+LENMAT
      DO 60 I=43,52
        IF(INFIX(INEXT:INEXT).EQ.SYMBOL(I))GO TO 70
60      CONTINUE
      GO TO 140
C
C  We match an old symbol
C
70    CONTINUE
      SYM1=SYMBOL(MATCH)
      SYM2=' '
      IF(MATCHO.NE.0)SYM2=SYMBOL(MATCHO)
C
C  Check for unary minus or plus
C
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
C
C  Check for matrix/vector index or two place operator
C
      IF(SYM1.EQ.')')THEN
        ICOM=0
        ILPR=0
        IRPR=1
        DO 90 IBACK=1,NINT
          I=NINT+1-IBACK
          IF(INTSYM(I).LE.0)GO TO 90
          SYM3=SYMBOL(INTSYM(I))
          IF(SYM3.EQ.','.AND.IRPR-ILPR.EQ.1)THEN
            IF(ICOM.NE.0)THEN
              IERROR=1
              OUTPUT='SYMCHK - Too many commas.'
              CALL CHRWRT(OUTPUT,0,IOUNIT)
              RETURN
            end if
            ICOM=I
          ELSEIF(SYM3.EQ.'(')THEN
            ILPR=ILPR+1
          ELSEIF(SYM3.EQ.')')THEN
            IRPR=IRPR+1
          end if
          IF(ILPR.EQ.IRPR)THEN
            IF(I.LE.1)GO TO 100
            IF(INTSYM(I-1).LE.0)GO TO 100
            SYM3=SYMBOL(INTSYM(I-1))
            IF(IOPSYM(INTSYM(I-1)).NE.0.AND.SYM3.NE.'MAX'
     *      .AND.SYM3.NE.'MIN'.AND.SYM3.NE.'ATAN2'
     *      .AND.SYM3.NE.'POLY')GO TO 100
C
C  Copy ) into INTSYM
C
            NINT=NINT+1
            INTSYM(NINT)=MATCH
C
C  ) should be followed by INDX1 if vector/matrix reference
C
            IF(ICOM.EQ.0)THEN
              MATCH=INDX1
            ELSE
C
C  Insert )( to divide arguments
C
              INTSYM(ICOM)=MATCH
              DO 80 JBACK=1,NINT-ICOM
                J=NINT+1-JBACK
                INTSYM(J+1)=INTSYM(J)
80              CONTINUE
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
      end if
100   CONTINUE
      LENS=LENSYM(MATCH)
      NINT=NINT+1
      INTSYM(NINT)=MATCH
      IPOS=IPOS+LENS
      GO TO 30
C
C  Check for constant
C
110   CONTINUE
      NWORDS=LENFIX+1-IPOS
      CALL CHRCTR(INFIX(IPOS:),NWORDS,RVAL,IERROR,LCHAR)
      IF(LCHAR.LE.0)GO TO 140
      IF(IERROR.NE.0)THEN
        OUTPUT='SYMCHK - Illegal real number.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
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
      ELSEIF(0.0001.LE.ABS(RVAL).AND.ABS(RVAL).LE.1)THEN
        WRITE(NAMVAR,1050)RVAL
      ELSE
        WRITE(NAMVAR,1060)RVAL
      end if
      CALL CHRDB1(NAMVAR)
      CALL SYMADD(IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     *MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IFREE)
      IF(IERROR.NE.0)RETURN
      IPOINT=IPVAL(NSYM)
      VALSYM(IPOINT)=RVAL
      MATCH=NSYM
130   CONTINUE
C
C  Advance LCHAR characters, except that CHRCTR will read in a
C  comma as the end of the number, and we have to back up in
C  such a case.
C
      IF(INFIX(IPOS+LCHAR-1:IPOS+LCHAR-1).NE.',')THEN
        IPOS=IPOS+LCHAR
      ELSE
        IPOS=IPOS+LCHAR-1
      end if
      NINT=NINT+1
      INTSYM(NINT)=MATCH
      GO TO 30
C
C  Consider implicit variable declaration on right hand side.
C
140   CONTINUE
      IF(NUVAR.EQ.0)THEN
        IERROR=1
        OUTPUT='SYMCHK - Undeclared variable.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
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
      CALL CHRLEN(NAMVAR,LCHAR)
      IF(LCHAR.LE.0)GO TO 170
      IPOS=IPOS+LCHAR
      CALL SYMADD(IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     *MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IFREE)
      NINT=NINT+1
      INTSYM(NINT)=NSYM
      MATCH=NSYM
      OUTPUT='SYMCHK - Undeclared variable '//NAMVAR//' assumed scalar.'
      CALL CHRWRT(OUTPUT,1,IOUNIT)
      GO TO 30
170   CONTINUE
      IERROR=1
      WRITE(OUTPUT,1010)IPOS
      CALL CHRWRT(OUTPUT,1,IOUNIT)
      WRITE(OUTPUT,'(1X,A)')INFIX(1:LENFIX)
      CALL CHRWRT(OUTPUT,0,IOUNIT)
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
cc SYMVAL
c
      CHARACTER COM*1
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IPVAL(MAXSYM)
      DIMENSION LENSYM(MAXSYM)
      CHARACTER NAMVAR*10
      CHARACTER OUTPUT*100
      CHARACTER SYMBOL(MAXSYM)*10
      DIMENSION VALSYM(MAXVAL)
C
      IERROR=0
      CALL CHRLEN(NAMVAR,LENNAM)
      IF(LENNAM.LE.0)THEN
        IERROR=1
        OUTPUT='SYMVAL - Cannot understand variable '//NAMVAR
        CALL CHRWRT(OUTPUT,1,IOUNIT)
        RETURN
      end if
C
C  Search for match
C
      DO I=1,NSYM
        MATCH=I
        IF(NAMVAR.EQ.SYMBOL(I))GO TO 20
      end do
      IF(COM.EQ.'R')THEN
        OUTPUT='SYMVAL - Could not find '//NAMVAR
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        IF(COM.EQ.'R')IERROR=1
      end if
C
C  Add name to list
C
      CALL SYMADD(IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     *MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IFREE)
      IF(IERROR.NE.0)RETURN
      MATCH=NSYM
      GO TO 30
20    CONTINUE
      IF(IOPSYM(MATCH).NE.0)IERROR=1
      IF(IERROR.NE.0)THEN
        OUTPUT='SYMVAL - Variable may not be altered.'
        CALL CHRWRT(OUTPUT,1,IOUNIT)
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
     *ROVER,TABLE,TSTOP)

c*********************************************************************72
C
cc TYPE prints out the current data.
c
      CHARACTER CHRINT*6
      CHARACTER CHRREL*14
      DIMENSION IHAVE(MAXVAR)
      DIMENSION IOUNIT(*)
      CHARACTER IRHS(*)*(*)
      CHARACTER METNAM(*)*60
      CHARACTER OUTPUT*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)
C
      IF(NEQN.EQ.1)THEN
        OUTPUT='There is one equation:'
      ELSEIF(NEQN.GT.1)THEN
        OUTPUT='There are NEQN='//CHRINT(NEQN)//' equations:'
      end if
      CALL CHRWRT(OUTPUT,1,IOUNIT)
      DO I=1,NEQN
        WRITE(OUTPUT,1090)I,IRHS(I)
        CALL CHRWRT(OUTPUT,1,IOUNIT)
      end do
      DO 220 I=1,NEQN
        IVAR=1+2*MAXEQN+I
        IF(IHAVE(IVAR).NE.0)THEN
          WRITE(OUTPUT,1091)I,IRHS(I+MAXEQN)
          CALL CHRWRT(OUTPUT,1,IOUNIT)
        end if
220     CONTINUE
      IF(IHAVE(1).NE.0)THEN
        OUTPUT=' '
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='Initial condition defined at TINIT='//CHRREL(TABLE(1,1))
        CALL CHRWRT(OUTPUT,1,IOUNIT)
        DO I=1,NEQN
          WRITE(OUTPUT,1000)I,TABLE(1,2*I)
          CALL CHRWRT(OUTPUT,1,IOUNIT)
        end do
      ELSE
        OUTPUT='Initial conditions have not been specified.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
      end if
      OUTPUT=' '
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='The integration method is METHOD='//CHRINT(METHOD)
      CALL CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT=METNAM(METHOD)
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='taking NSTEPS='//CHRINT(NSTEPS)//' steps'
      CALL CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT='increasing T to TSTOP='//CHRREL(TSTOP)
      CALL CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT=' '
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      IF(IHAVE(1).NE.0.AND.NTAB.GT.1)THEN
        OUTPUT='Solution has been computed from'
        CALL CHRWRT(OUTPUT,1,IOUNIT)
        OUTPUT='T='//CHRREL(TABLE(1,1))//' to '//CHRREL(TABLE(NTAB,1))
        CALL CHRWRT(OUTPUT,1,IOUNIT)
      ELSE
        OUTPUT='Solution has not been computed.'
        CALL CHRWRT(OUTPUT,0,IOUNIT)
      end if
      OUTPUT=' '
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='Writing output every IPRINT='//CHRINT(IPRINT)//' steps.'
      CALL CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT='There are '//CHRINT(NTAB)//' plot points saved.'
      CALL CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT='At most '//CHRINT(MAXTAB)//' plot points are allowed.'
      CALL CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT='Overflow threshhold is ROVER='//CHRREL(ROVER)
      CALL CHRWRT(OUTPUT,1,IOUNIT)
      RETURN
1000  FORMAT('Initial Y',I1,'=',G14.6)
1090  FORMAT('Y',I1,'''=',A80)
1091  FORMAT('Exact value X',I1,'=',A80)
      END
      SUBROUTINE WRITER(FILNAM,IERROR,IOUNIT,IPRINT,IRHS,LINE,
     *MAXEQN,MAXTAB,MAXVAR,METHOD,NEQN,NLINE,
     *NSTEPS,OUTPUT,PROMPT,TABLE,TSTOP)

c*********************************************************************72
C
cc WRITER writes a file that defines an ODE.
c
      CHARACTER CHRINT*6
      CHARACTER CHRREL*14
      CHARACTER FILNAM*(*)
      DIMENSION IOUNIT(*)
      CHARACTER IRHS(*)*(*)
      CHARACTER LINE*80
      CHARACTER OUTPUT*120
      CHARACTER PROMPT*80
      DIMENSION TABLE(MAXTAB,MAXVAR)
C
      PROMPT='output filename.'
      CALL CHRREA(FILNAM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0)
      IOSAVE=IOUNIT(2)
      IOUNIT(2)=41
      OPEN(UNIT=41,FILE=FILNAM,STATUS='NEW',ERR=20)
      OUTPUT='NOTE'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='This file was created by UPJODE, and defines an ODE.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='.'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='NEQN='//CHRINT(NEQN)
      CALL CHRDB1(OUTPUT)
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='TINIT='//CHRREL(TABLE(1,1))
      CALL CHRDB1(OUTPUT)
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      DO I=1,NEQN
        OUTPUT='Y'//CHRINT(I)//'='//CHRREL(TABLE(1,2*I))
        CALL CHRDB1(OUTPUT)
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='Y1''='//IRHS(I)
        CALL CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='X1='//IRHS(MAXEQN+I)
        CALL CHRWRT(OUTPUT,0,IOUNIT)
      end do
      OUTPUT='METHOD='//CHRINT(METHOD)
      CALL CHRDB2(OUTPUT)
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='IPRINT='//CHRINT(IPRINT)
      CALL CHRDB1(OUTPUT)
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='NSTEPS='//CHRINT(NSTEPS)
      CALL CHRDB1(OUTPUT)
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='TSTOP='//CHRREL(TSTOP)
      CALL CHRDB1(OUTPUT)
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      CLOSE(UNIT=41)
      IOUNIT(2)=IOSAVE
      OUTPUT='UPJODE has written the file '//FILNAM
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      RETURN

20    CONTINUE
      IERROR=1
      OUTPUT='WRITER - Error trying to open file!'
      CALL CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
      END

