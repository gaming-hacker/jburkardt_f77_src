C  LISTER.F  12 August 1992
C
      program main

C*********************************************************************72
C
cc MAIN is the main program for LISTER.
c
C  Discussion:
C
C    One goal eventually:  produce a SORTED list of entry points.
C
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
c  Local Parameters:
C
C    INIT   is a flag which tells NEXLIN that we haven't read any lines
C    yet at all.
C  
C    NBLANK is the number of blank lines found.
C
C    NCOMMON is the number of common block statements.
C    (How about the number of common blocks?)
C
C    NDATA is the number of DATA statements.
C
C    NENTRY is the number of entry points found.
C
C    NINCLUDE is the number of INCLUDE statements.
C
C    NLONG  is the number of lines longer than 72 characters found.
C
C    NMAIN  is the number of "main" programs (or floating code fragments!)
C
      implicit  none

      INTEGER MAXMOD
      PARAMETER (MAXMOD=200)

      CHARACTER CHRTMP*66
      CHARACTER CTEMP3*3
      CHARACTER DLINE*1614
      integer   i
      integer   ieof
      INTEGER   INDEXI
      CHARACTER INFILE*40
      integer   init
      CHARACTER INPUT*80
      integer   iounit
      character isay*1
      integer   len2
      integer   lenc
      integer   lenl
      LOGICAL   LEQI
      CHARACTER LINE(19)*80
      INTEGER   MODBEG(MAXMOD)
      INTEGER   MODEND(MAXMOD)
      CHARACTER MODNAM(MAXMOD)*80
      integer   modopen
      CHARACTER MODTYP(MAXMOD)*80
      integer   nbegin
      integer   nblank
      integer   ncomment
      integer   ncommon
      INTEGER   NDATA
      integer   nentry
      integer   ninclude
      integer   nline
      integer   nlong
      integer   nmain
      integer   nmodactive
      integer   nmoddone
      LOGICAL   NUMBERS
      integer   numlin
      CHARACTER OUTFILE*40
      integer   s_len_trim
      CHARACTER TYPE*20

      NUMBERS=.FALSE.

      call timestamp ( )
      WRITE(*,*)' '
      WRITE(*,*)'LISTER'
      WRITE(*,*)'Version 1.06'
      WRITE(*,*)'12 August 1992'
      WRITE(*,*)'LISTER is a FORTRAN file analysis program.'
      WRITE(*,*)' '
C
C   Get name of input source code file.
C
909   CONTINUE
      WRITE(*,*)'Enter input file name:'
      READ(*,'(A)')INFILE
      OPEN(UNIT=11,FILE=INFILE,STATUS='OLD',ERR=99)
C
908   CONTINUE
      WRITE(*,*)'Enter output filename or * for screen:'
      READ(*,'(A)')OUTFILE

      IF(OUTFILE.NE.'*')THEN
        IOUNIT=8
        OPEN(UNIT=IOUNIT,FILE=OUTFILE,STATUS='NEW',ERR=98)
      ELSE
        IOUNIT=6
      end if

      WRITE(*,*)' '
      WRITE(*,*)'Do you want line numbers printed out?'
      READ(*,'(A)')ISAY
      IF(LEQI(ISAY,'Y'))NUMBERS=.TRUE.
C
C  Initialize the counters
C
      CALL INITAL(INIT,NBEGIN,NBLANK,NCOMMENT,NCOMMON,NDATA,
     &  NENTRY,NINCLUDE,NLINE,NLONG,NMAIN,NMODDONE,NMODACTIVE)
C
C  Get next line (or continuation lines)
C
10    CONTINUE

      IF(IEOF.EQ.1)GO TO 20

      CALL NEXLIN(IEOF,INIT,INPUT,LINE,NBLANK,NLINE,NLONG,NUMLIN)

      IF(LINE(1).EQ.' ')GO TO 10
C
C  Comment lines may be immediately skipped.
C
      IF(LEQI(LINE(1)(1:1),'C').OR.
     *   LEQI(LINE(1)(1:1),'D').OR.
     *   LEQI(LINE(1)(1:1),'*'))THEN
        NCOMMENT=NCOMMENT+1
        GO TO 10
      END IF
C
C  Create a single "super" line out of the line and all its continuations.
C  From here on we will only work with DLINE.
C
      DLINE=' '
      LENL=0
      DO I=1,NUMLIN
        CHRTMP=LINE(I)(7:72)
        CALL s_blank_delete ( CHRTMP )
        LEN2 = s_len_trim ( CHRTMP )
        DLINE(LENL+1:LENL+LEN2)=CHRTMP
        LENL=LENL+LEN2
      end do
C
C  Remove the '*8' from 'REAL*8 FUNCTION' and so on.
C  Also, determine the TYPE of the function.
C
      CALL NOSTAR(DLINE,LENL,TYPE)
C
C  If the line is 'END', then we are exiting a module.  Record
C  the location of the last line.
C
      IF(LEQI(DLINE,'END'))THEN
        DO I=1,MODOPEN
          MODEND(I)=NLINE
        end do
        NBEGIN=NLINE+1
        DO I=1,MODOPEN
          LENC = s_len_trim ( MODTYP(I) )
          WRITE(IOUNIT,'(1X,A)')MODTYP(I)(1:LENC)
          LENC= s_len_trim ( MODNAM(I) )
          WRITE(IOUNIT,'(1X,A)')MODNAM(I)(1:LENC)
          IF(NUMBERS)WRITE(IOUNIT,'(1X,2I12)')MODBEG(I),MODEND(I)
        end do
        MODOPEN=0
        GO TO 10
      end if
C
C  In the case of ENTRY's, do not assign preceding comments to them
C
      IF(MODOPEN.GT.0.AND.DLINE(1:5).EQ.'ENTRY')THEN
        NENTRY=NENTRY+1
        MODOPEN=MODOPEN+1
        MODBEG(MODOPEN)=NLINE
        MODTYP(MODOPEN)='ENTRY'
        MODNAM(MODOPEN)=DLINE(6:85)
        GO TO 10
      end if
C
C  Catch COMMON blocks here.
C
      IF(LEQI(DLINE(1:6),'COMMON'))THEN
        NCOMMON=NCOMMON+1
        GO TO 10
      end if
C
C  Catch DATA statements here.
C
      IF(LEQI(DLINE(1:4),'DATA'))THEN
        NDATA=NDATA+1
        GO TO 10
      end if
C
C  Count INCLUDE statements here.
C
      IF(LEQI(DLINE(1:7),'INCLUDE'))THEN
        NINCLUDE=NINCLUDE+1
        GO TO 10
      end if

      IF(MODOPEN.NE.0)GO TO 10
C
C  We are expecting a new module at this point
C
      IF(LEQI(DLINE(1:7),'PROGRAM'))THEN
        NMAIN=NMAIN+1
        NENTRY=NENTRY+1
        MODOPEN=1
        MODBEG(MODOPEN)=NBEGIN
        MODTYP(MODOPEN)=DLINE(1:7)
        MODNAM(MODOPEN)=DLINE(8:87)
        GO TO 10
      end if

      IF(LEQI(DLINE(1:9),'BLOCKDATA'))THEN
        NENTRY=NENTRY+1
        MODOPEN=1
        MODBEG(MODOPEN)=NBEGIN
        MODTYP(MODOPEN)='BLOCK DATA'
        MODNAM(MODOPEN)=DLINE(10:89)
        GO TO 10
      end if

      IF(LEQI(DLINE(1:10),'SUBROUTINE'))THEN
        NENTRY=NENTRY+1
        MODOPEN=1
        MODBEG(MODOPEN)=NBEGIN
        MODTYP(MODOPEN)='SUBROUTINE'
        MODNAM(MODOPEN)=DLINE(11:90)
        GO TO 10
      end if

      IF(DLINE(1:8).EQ.'FUNCTION'.OR.
     *   DLINE(1:21).EQ.'DOUBLECOMPLEXFUNCTION'.OR.
     *   DLINE(1:23).EQ.'DOUBLEPRECISIONFUNCTION'.OR.
     *   DLINE(1:15).EQ.'INTEGERFUNCTION'.OR.
     *   DLINE(1:15).EQ.'LOGICALFUNCTION'.OR.
     *   DLINE(1:12).EQ.'BYTEFUNCTION'.OR.
     *   DLINE(1:17).EQ.'CHARACTERFUNCTION'.OR.
     *   DLINE(1:12).EQ.'REALFUNCTION'.OR.
     *   DLINE(1:15).EQ.'COMPLEXFUNCTION')THEN
        NENTRY=NENTRY+1
        MODOPEN=1
        MODBEG(MODOPEN)=NBEGIN
        MODTYP(MODOPEN)='FUNCTION'
        MODNAM(MODOPEN)=DLINE(1:80)
        GO TO 10
      end if

      IF(INDEXI(DLINE,'FUNCTION').NE.0)THEN
        WRITE(*,*)'FUNCTION statement of unexpected type.'
        NENTRY=NENTRY+1
        MODOPEN=1
        MODBEG(MODOPEN)=NBEGIN
        MODTYP(MODOPEN)='FUNCTION'
        MODNAM(MODOPEN)=DLINE(1:80)
        GO TO 10
      end if
C
C  Unnamed MAIN program?
C
      NENTRY=NENTRY+1
      NMAIN=NMAIN+1
      MODOPEN=1
      MODBEG(MODOPEN)=NBEGIN
      MODTYP(MODOPEN)='Unnamed Main'
      CALL CHRITC0(CTEMP3,NMAIN)
      MODNAM(MODOPEN)='MAIN'//CTEMP3
      GO TO 10

20    CONTINUE

      CALL SUMMARY(INFILE,NBLANK,NCOMMENT,NCOMMON,NDATA,NENTRY,
     &  NINCLUDE,NLINE,NLONG,NMAIN,OUTFILE)

      IF(OUTFILE.NE.'*')CLOSE(UNIT=8)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LISTER'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      STOP
C
C  Error opening input file
C
99    CONTINUE
      WRITE(*,*)'LISTER could not open the file ',INFILE
      WRITE(*,*)'Could you have typed the name incorrectly?'
      WRITE(*,*)' '
      GO TO 909
C 
C  Error opening output file
C
98    CONTINUE
      WRITE(*,*)'LISTER could not open the file ',OUTFILE
      WRITE(*,*)'Could the name be illegal, or already in use?'
      WRITE(*,*)' '
      GO TO 908
      END
      SUBROUTINE CAPCHR(STRING)

C*********************************************************************72
C
cc CAPCHR capitalizes a string.
c
c  Discussion:
c
C    CAPCHR accepts a STRING of NCHAR characters and replaces any lowercase 
C    letters by uppercase ones.  Compare CHRCAP, which requires the string 
C    length, and CHRLOW which lowercases a string.
C
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
C    Input/output, CHARACTER*(*) STRING, the string of characters
C    to be transformed.
C
      implicit none

      INTEGER   I
      INTEGER   ITEMP
      INTEGER   NCHAR
      CHARACTER STRING*(*)

      NCHAR=LEN(STRING)

      DO I=1,NCHAR
        IF(LGE(STRING(I:I),'a').AND.LLE(STRING(I:I),'z'))THEN
          ITEMP=ICHAR(STRING(I:I))+ICHAR('A')-ICHAR('a')
          STRING(I:I)=CHAR(ITEMP)
        end if
      end do

      RETURN
      END
      SUBROUTINE CHRITC0(STRING,INTVAL)

C*********************************************************************72
C
cc CHRITC0 converts an integer to a string padded with zeros.
c
c  Discussion:
c
C    CHRITC0 accepts an integer in INTVAL and stores it in a STRING of
C    The last digit of the integer is stored in the last character of the 
C    STRING.  Any left-over entries in STRING are filled with zeroes.  If the 
C    integer is too large to be written into STRING, STRING is filled with 
C    '*' characters.
C
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
      implicit none

      CHARACTER CHAR*1
      integer i
      integer idig
      integer intval
      integer ioff
      integer ival
      integer j
      integer ncopy
      CHARACTER STRING*(*)

      NCOPY=LEN(STRING)

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
        STRING(J:J)='0'
      end do

      RETURN
      END
      SUBROUTINE CHRUP2(STRING,STRNG2,STRNG3)

C*********************************************************************72
C
cc CHRUP2 copies a string up to the first occurrence of a substring.
C
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
      implicit none

      integer i
      integer len1
      integer len2
      integer len3
      CHARACTER STRING*(*)
      CHARACTER STRNG2*(*)
      CHARACTER STRNG3*(*)

      LEN1=LEN(STRING)
      LEN2=LEN(STRNG2)
      LEN3=LEN(STRNG3)

      STRNG2=' '
      I=0
10    CONTINUE
      I=I+1
      IF(I.GT.LEN1)RETURN
      IF(I.GT.LEN2)RETURN
      IF(I+LEN3-1.LE.LEN1)THEN
        IF(STRING(I:I+LEN3-1).EQ.STRNG3)RETURN
      end if
      STRNG2(I:I)=STRING(I:I)
      GO TO 10
      END
      FUNCTION INDEXI(STRING,SUB)

C*********************************************************************72
C
cc INDEXI is a case-insensitive search for a substring.
c
c  Discussion:
c
C    INDEXI is a case-insensitive INDEX function.  It returns the location in
C    STRING at which the substring SUB is first found, or 0 if the substring does
C    not occur at all.
C
C    INDEXI is also trailing blank insensitive.  This is very important for those
C    cases where you have stored information in larger variables.  If STRING is of
C    length 80, and SUB is of length 80, then if STRING='FRED' and SUB='RED', a
C    match would not be reported by the standard FORTRAN INDEX, because it treats
C    both variables as being 80 characters long!  INDEXI assumes that trailing
C    blanks represent garbage!
C
C    Because of the suppression of trailing blanks, INDEXI cannot be used to
C    find, say, the first occurrence of the string 'A '.  However, INDEXI
C    treats as a special case the occurrence where STRING or SUB is entirely
C    blank.  Thus you can use INDEXI to search for occurrences of double or
C    triple blanks in a string, for example, although INDEX itself would be
C    just as suitable for that problem.
C
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
C    Input, CHARACTER*(*) STRING, the string to be searched.
C
C    Input, CHARACTER*(*) SUB, the substring to search for.
C
C    Output, INTEGER INDEXI.  0 if SUB does not occur in STRING.  Otherwise
C    STRING(INDEXI:INDEXI+LENS-1) = SUB, where LENS is the length of SUB,
C    and is the first place this happens.  However, note that INDEXI
C    ignores case, unlike the standard FORTRAN INDEX function.
C
      implicit none

      INTEGER   I
      INTEGER   INDEXI
      LOGICAL   LEQI
      INTEGER   LLEN1
      INTEGER   LLEN2
      integer   s_len_trim
      CHARACTER STRING*(*)
      CHARACTER SUB*(*)

      INDEXI=0

      LLEN1 = s_len_trim ( STRING )
      LLEN2 = s_len_trim ( SUB )
C
C  In case STRING or SUB is blanks, use LEN
C
      IF(LLEN1.EQ.0)LLEN1=LEN(STRING)
      IF(LLEN2.EQ.0)LLEN2=LEN(SUB)

      IF(LLEN2.GT.LLEN1) then
        RETURN
      end if

      DO I=1,LLEN1+1-LLEN2
        IF(LEQI(STRING(I:I+LLEN2-1),SUB))THEN
          INDEXI=I
          RETURN
        end if
      end do

      RETURN
      END
      SUBROUTINE INITAL(INIT,NBEGIN,NBLANK,NCOMMENT,NCOMMON,NDATA,
     &  NENTRY,NINCLUDE,NLINE,NLONG,NMAIN,NMODDONE,NMODACTIVE)

C*********************************************************************72
C
Cc INIT sets initial values for data.
C
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
      integer init
      integer nbegin
      integer nblank
      integer ncommon
      integer ndata
      integer nentry
      integer ninclude
      integer nline
      integer nlong
      integer nmain
      integer nmodone
      integer nmodactive

      init=1
      nbegin=1
      nblank=0
      ncomment=0
      ncommon=0
      ndata=0
      nentry=0
      ninclude=0
      nline=0
      nlong=0
      nmain=0
      nmoddone=0
      nmodactive=0

      return
      end
      logical function ldigit(string)

C*********************************************************************72
C
Cc LDIGIT is TRUE if STRING is made up of digits and blanks only.
C
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
      implicit none

      CHARACTER CHRTMP*1
      integer   i
      integer   lenc
      LOGICAL   LGT
      LOGICAL   LLT
      CHARACTER STRING*(*)

      LENC=LEN(STRING)

      LDIGIT=.FALSE.
      DO I=1,LENC
        CHRTMP=STRING(I:I)
        IF(CHRTMP.NE.' ')THEN
          IF(LLT(CHRTMP,'0').OR.LGT(CHRTMP,'9'))RETURN
        end if
      end do

      LDIGIT=.TRUE.

      RETURN
      END
      FUNCTION LEQI(STRNG1,STRNG2)

C*********************************************************************72
C
cc LEQI is a case-insensitive comparison on strings for equality.
c
c  Discussion:
c
C    There is no FORTRAN LEQ function, but if there were, it would be case
C    sensitive.  LEQI is a case insensitive comparison of two strings for 
C    equality.  Thus, LEQI('Anjana','ANJANA') is .TRUE.
C
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
C    Input, CHARACTER*(*) STRNG1, STRNG2, the strings to compare.
C
C    Output, LOGICAL LEQI, the result of the comparison.
C
      implicit  none

      INTEGER   I
      INTEGER   LEN1
      INTEGER   LEN2
      INTEGER   LENC
      LOGICAL   LEQI
      CHARACTER S1*1
      CHARACTER S2*1
      CHARACTER STRNG1*(*)
      CHARACTER STRNG2*(*)

      LEN1=LEN(STRNG1)
      LEN2=LEN(STRNG2)
      LENC=MIN(LEN1,LEN2)

      LEQI=.FALSE.

      DO I=1,LENC
        S1=STRNG1(I:I)
        S2=STRNG2(I:I)
        CALL CAPCHR(S1)
        CALL CAPCHR(S2)
        IF(S1.NE.S2)RETURN
      end do
 
      IF(LEN1.GT.LENC.AND.STRNG1(LENC+1:LEN1).NE.' ')RETURN
      IF(LEN2.GT.LENC.AND.STRNG2(LENC+1:LEN2).NE.' ')RETURN
      LEQI=.TRUE.

      RETURN
      END
      SUBROUTINE NEXLIN(IEOF,INIT,INPUT,LINE,NBLANK,NLINE,NLONG,
     *NUMLIN)

C*********************************************************************72
C
cc NEXLIN reads a logical FORTRAN line.
c
c  Discussion:
c
C    Eventually, this routine will read up to 19 lines at a time,
C    (if continuations are involved).
C
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
      implicit none

      integer   ieof
      integer   init
      CHARACTER INPUT*80
      integer   lenl
      LOGICAL   LEQI
      CHARACTER LINE(19)*80
      integer   nblank
      integer   nline
      integer   nlong
      integer   numlin
      integer   s_len_trim

      IEOF=0
      NUMLIN=0

      IF(INIT.EQ.1)THEN
        READ(11,'(A)',END=30)INPUT
        CALL CAPCHR(INPUT)
        INIT=0
      end if
C
C  Store old line, INPUT, as first line of new FORTRAN command.
C
10    CONTINUE
      NLINE=NLINE+1
      NUMLIN=NUMLIN+1
      LINE(NUMLIN)=INPUT
      READ(11,'(A)',END=20)INPUT
      CALL CAPCHR(INPUT)
C
C  Set LENL to the length of the line.
C  Noncomment lines longer than 72 should be noted.
C
      LENL=s_len_trim ( INPUT )

      IF(LENL.LE.0)THEN
        NBLANK=NBLANK+1
      ELSEIF(LENL.GT.72)THEN
        IF(NLONG.EQ.0)THEN
          WRITE(*,*)'Source contains lines longer than 72 characters!'
          WRITE(*,*)'First one at line ',NLINE
        end if
        NLONG=NLONG+1
      end if
C
C  If this is not a continuation line, we're done.
C
      IF(LEQI(INPUT(1:1),'C'))RETURN
      IF(LEQI(INPUT(1:1),'D'))RETURN
      IF(INPUT(1:1).EQ.'*')RETURN
      IF(INPUT(6:6).EQ.' ')RETURN

      IF(NUMLIN.GE.19)THEN
        WRITE(*,*)'Too many continuation lines!'
        RETURN
        end if
      GO TO 10

20    CONTINUE
      INPUT=' '
      IEOF=1
      CLOSE(UNIT=11)
      RETURN

30    CONTINUE
      LINE(1)=' '
      IEOF=1
      WRITE(*,*)'LISTER could not open your file!'

      STOP
      END
      SUBROUTINE NOSTAR(STRING,LENS,TYPE)

C*********************************************************************72
C
cc NOSTAR handles the occurrence of a complicated datatype involving a star.
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
      implicit none

      INTEGER   INDEXI
      integer   istar
      LOGICAL   LDIGIT
      integer   lens
      LOGICAL   LEQI
      CHARACTER STRING*(*)
      CHARACTER TYPE*20

      TYPE=' '
      IF ( INDEXI(STRING,'FUNCTION') .EQ. 0 ) then
        RETURN
      end if

      ISTAR=0

      IF(LEQI(STRING(1:10),'CHARACTER*'))THEN
        ISTAR=10
      ELSEIF(LEQI(STRING(1:8),'COMPLEX*'))THEN
        ISTAR=8
      ELSEIF(LEQI(STRING(1:14),'DOUBLECOMPLEX*'))THEN
        ISTAR=14
      ELSEIF(LEQI(STRING(1:16),'DOUBLEPRECISION*'))THEN
        ISTAR=16
      ELSEIF(LEQI(STRING(1:8),'INTEGER*'))THEN
        ISTAR=8
      ELSEIF(LEQI(STRING(1:8),'LOGICAL*'))THEN
        ISTAR=8
      ELSEIF(LEQI(STRING(1:5),'REAL*'))THEN
        ISTAR=5
      ELSEIF(LEQI(STRING(1:5),'BYTE*'))THEN
        ISTAR=5
      end if

      IF(ISTAR.EQ.0)RETURN
      CALL CHRUP2(STRING,TYPE,'*')

10    CONTINUE

      TYPE(ISTAR:ISTAR)=STRING(ISTAR:ISTAR)
      STRING(ISTAR:ISTAR)=' '
      ISTAR=ISTAR+1
      IF(LDIGIT(STRING(ISTAR:ISTAR)))GO TO 10

      CALL s_blank_delete ( STRING )

      RETURN
      END
      subroutine s_blank_delete ( s )

c*********************************************************************72
c
cc S_BLANK_DELETE removes blanks from a string, left justifying the remainder.
c
c  Discussion:
c
c    All TAB characters are also removed.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character*(*) S, the string to be transformed.
c
      implicit none

      character ch
      integer get
      integer put
      character*(*) s
      integer s_len_trim
      integer s_length
      character tab

      tab = char ( 9 )

      put = 0
      s_length = s_len_trim ( s )

      do get = 1, s_length

        ch = s(get:get)

        if ( ch .ne. ' ' .and. ch .ne. tab ) then
          put = put + 1
          s(put:put) = ch
        end if

      end do

      s(put+1:s_length) = ' '

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
      SUBROUTINE SUMMARY(INFILE,NBLANK,NCOMMENT,NCOMMON,NDATA,
     &  NENTRY,NINCLUDE,NLINE,NLONG,NMAIN,OUTFILE)
C
C*********************************************************************72
C
cc SUMMARY prints a summary of the program data.
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
      implicit none

      CHARACTER INFILE*(*)
      integer   itemp
      integer   nblank
      integer   ncomment
      integer   ncommon
      INTEGER   NDATA
      integer   nentry
      integer   ninclude
      integer   nline
      integer   nlong
      integer   nmain
      CHARACTER OUTFILE*(*)
      integer   s_len_trim

      ITEMP = s_len_trim ( INFILE )
      WRITE(*,*) ' '
      WRITE(*,*) ' '
      WRITE(*,'(A)') INFILE(1:ITEMP)
      WRITE(*,*) ' '
      WRITE(*,*) NLINE,' lines.'
      WRITE(*,*) NMAIN,' main programs or code fragments.'
      WRITE(*,*) NENTRY,' entry points.'
      WRITE(*,*) NBLANK,' blank lines.'
      WRITE(*,*) NLONG,' lines longer than 72 columns.'
      WRITE(*,*) NCOMMENT,' comment lines.'
      WRITE(*,*) NCOMMON,' COMMON statements.'
      WRITE(*,*) NDATA,' DATA statements.'
      WRITE(*,*) NINCLUDE,' INCLUDE statements.'

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
