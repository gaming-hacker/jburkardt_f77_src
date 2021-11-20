C  ANYMAC.FOR   Version 1.05  14 September 1990  
C  ANYPLT/Macintosh interface
C
C  ANYPLT is a subroutine which provides a simple, standard interface
C  between FORTRAN programs and various output devices.  To run a
C  program which calls ANYPLT on a different machine, the program
C  is not modified in any way, but a different version of the ANYPLT
C  program is provided.  Currently, the following versions are available:
C
C  ANYATT - AT&T PC6300 graphics (640 by 400).  Requires ATTPLT.ASM.
C  ANYBUG - Simple debugging output to a file.
C  ANYCAL - CALCOMP file output.  Available on many mainframes.
C  ANYIBM - IBM PC hi resolution (640 by 200).  Requires IBMPLT.ASM.
C  ANYMAC - Macintosh graphics.  (512 by 342) Requires auxilliary TOOLBX.SUB.
C  ANYNCR - NCAR graphics package.  
C  ANYNUL - Does nothing.
C  ANYP10 - PLOT10 interactive graphics. (1024 by 768)
C  ANYTTY - Simple 'typewriter' graphics (80 by 24)
C
C  The personal computer routines generally require an auxilliary
C  set of routines written in assembly language.
C
C  ANYMAC was written by John Burkardt
C
C  The symbolic output of characters, numbers and other printable 
C  characters was made possible by adaptation of a routine written
C  by Bill Furey of the University of Pittsburgh, Department of
C  Crystallography.
C
      SUBROUTINE ANYPLT(ICOM)
C
C***********************************************************************
C
C  Macintosh graphics.
C
C  When linking, the compiled file 'toolbx.sub' must also be included.
C
      INTEGER*4 LINETO,MOVETO
      PARAMETER (LINETO=Z'89109000')
      PARAMETER (MOVETO=Z'89309000')
      SAVE IFONT
      SAVE IPOINT
      SAVE IXMN,IXMX,IYMN,IYMX
      SAVE IXMIN,IXMAX,IYMIN,IYMAX
      SAVE ROTATE
      SAVE XMIN,XMAX,YMIN,YMAX
      SAVE XSMIN,XSMAX,YSMIN,YSMAX
      SAVE XSMN,XSMX,YSMN,YSMX
      SAVE XSTART,YSTART
C
      CHARACTER CARRAY*80
      CHARACTER CONG*1
      INTEGER*4 GRAFPTR
      CHARACTER ISAY*1
      INTEGER   IFONT(1617)
      INTEGER   IPOINT(95)
      INTEGER*4 IX
      INTEGER*4 IY
      LOGICAL   ROTATE
      COMMON /ANYCOM/ IPLT1,IPLT2,IXPLT1,IXPLT2,IYPLT1,
     1                IYPLT2,MARRAY,XPLT1,XPLT2,YPLT1,YPLT2
      COMMON /ANYCHR/ CARRAY
C
C  Pointer array into IFONT
C
      DATA (IPOINT(I),I=1,95) /
     $   1,   3,  26,  45,  66, 102, 130, 156, 166, 186, 206, 222, 233,
     $ 249, 255, 267, 273, 293, 306, 328, 353, 363, 383, 411, 423, 457,
     $ 483, 506, 533, 541, 552, 560, 587, 625, 638, 665, 683, 699, 714,
     $ 727, 754, 770, 786, 805, 818, 826, 838, 848, 868, 884, 909, 930,
     $ 956, 967, 981, 989,1001,1012,1025,1035,1045,1051,1061,1069,1075,
     $1081,1108,1131,1149,1172,1194,1214,1243,1260,1284,1307,1323,1336,
     $1364,1381,1401,1424,1447,1464,1486,1499,1516,1524,1536,1547,1560,
     $1570,1586,1592,1608/
C
C  IFONT contains the strokes defining the various symbols.
C
      DATA (IFONT(I),I=   1, 396)/
     $ 1, 0, 2,10,11, 9,22,10,23,11,22,10,11, 0, 9, 7, 9, 9,11, 9,11, 7,
     $ 9, 7, 0, 2, 8,17, 7,23, 9,23, 8,17, 0,14,17,13,23,15,23,14,17, 0,
     $ 4, 9,23, 7, 7, 0,13,23,11, 7, 0, 5,17,15,17, 0, 5,13,15,13, 0, 3,
     $15,19,13,21, 9,21, 7,19, 7,17, 9,15,13,15,15,13,15,11,13, 9, 9, 9,
     $ 7,11, 0, 9,23, 9, 7, 0,13,23,13, 7, 0, 3, 5,23, 9,23, 9,19, 5,19,
     $ 5,23, 0,17,23, 5, 7, 0,13, 7,13,11,17,11,17, 7,13, 7, 0, 1,17, 7,
     $ 7,17, 7,19, 9,21,13,21,15,19,15,17, 5,13, 5,11, 9, 7,13, 7,17,15,
     $ 0, 1,10,17, 9,23,11,23,10,17, 0, 1,12,23,11,21,10,19, 9,17, 9,15,
     $ 9,13,10,11,11, 9,12, 7, 0, 1,12,23,13,21,14,19,15,17,15,15,15,13,
     $14,11,13, 9,12, 7, 0, 3, 7,15,15,15, 0,13,19, 9,11, 0, 9,19,13,11,
     $ 0, 2, 7,15,15,15, 0,11,19,11,11, 0, 1,11, 7, 9, 7, 9, 9,11, 9,11,
     $ 7,11, 6,10, 4, 0, 1, 7,15,15,15, 0, 1, 9, 7, 9, 9,11, 9,11, 7, 9,
     $ 7, 0, 1,15,23, 7, 7, 0, 1, 9,23,13,23,15,19,15,11,13, 7, 9, 7, 7,
     $11, 7,19, 9,23, 0, 2, 7,21, 9,23, 9, 7, 0, 7, 7,11, 7, 0, 1, 5,21,
     $ 9,23,15,23,17,21,17,19,15,17, 7,13, 5,10, 5, 7,17, 7, 0, 2, 5,23,
     $17,23,15,17,13,15, 9,15, 0,13,15,17,13,17,10,14, 7, 8, 7, 5,10, 0,
     $ 1,13, 7,13,23, 5,13,17,13, 0, 1,17,23, 5,23, 5,17,13,17,17,15,17,
     $11,13, 7, 9, 7, 5,11, 0, 1,17,19,13,23, 9,23, 5,19, 5,13, 9,15,13/
      DATA (IFONT(I),I= 397, 792)/
     $15,17,13,17,11,13, 7, 9, 7, 5,11, 5,13, 0, 1, 5,19, 5,23,17,23,11,
     $15,11, 7, 0, 1, 8,15, 6,17, 6,21, 8,23,14,23,16,21,16,17,14,15, 8,
     $15, 5,13, 5, 9, 8, 7,14, 7,17, 9,17,13,14,15, 0, 1,17,17,15,15, 7,
     $15, 5,17, 5,21, 7,23,15,23,17,21,17,11,15, 7, 7, 7, 5,11, 0, 2, 9,
     $13, 9,15,11,15,11,13, 9,13, 0, 9, 7, 9, 9,11, 9,11, 7, 9, 7, 0, 2,
     $ 9,13, 9,15,11,15,11,13, 9,13, 0,11, 7, 9, 7, 9, 9,11, 9,11, 7,11,
     $ 6,10, 4, 0, 1,17,21, 5,15,17, 9, 0, 2, 7,15,15,15, 0, 7, 9,15, 9,
     $ 0, 1, 5,21,17,15, 5, 9, 0, 2, 7,21, 9,23,13,23,15,21,15,19,11,15,
     $11,11, 0,10, 7,10, 9,12, 9,12, 7,10, 7, 0, 1,13, 7, 9, 7, 5,11, 5,
     $19, 9,23,13,23,17,19,17,11,15, 9,13,11,12,10,10,10, 9,11, 9,15,10,
     $16,12,16,13,15,13,11, 0, 2, 5, 7,11,23,17, 7, 0, 8,15,14,15, 0, 2,
     $ 5, 7, 5,23,15,23,17,21,17,17,15,15, 5,15, 0,15,15,17,13,17, 9,15,
     $ 7, 5, 7, 0, 1,17,19,13,23, 9,23, 5,19, 5,11, 9, 7,13, 7,17,11, 0,
     $ 1, 5, 7, 5,23,13,23,17,19,17,11,13, 7, 5, 7, 0, 2,17,23, 5,23, 5,
     $ 7,17, 7, 0, 5,15,12,15, 0, 2, 5, 7, 5,23,17,23, 0, 5,15,12,15, 0,
     $ 2,17,19,13,23, 9,23, 5,19, 5,11, 9, 7,13, 7,17,11,17,15,13,15, 0,
     $17,11,17, 7, 0, 3, 5, 7, 5,23, 0, 5,15,17,15, 0,17,23,17, 7, 0, 3,
     $ 9,23,13,23, 0,11,23,11, 7, 0, 9, 7,13, 7, 0, 2,15,23,15,11,12, 7/
      DATA (IFONT(I),I= 793,1188)/
     $ 8, 7, 5,11, 5,13, 0,13,23,17,23, 0, 2, 5, 7, 5,23, 0,17,23, 5,15,
     $17, 7, 0, 1, 5,23, 5, 7,17, 7, 0, 1, 5, 7, 5,23,11,11,17,23,17, 7,
     $ 0, 1, 5, 7, 5,23,17, 7,17,23, 0, 1,17,19,13,23, 9,23, 5,19, 5,11,
     $ 9, 7,13, 7,17,11,17,19, 0, 1, 5, 7, 5,23,13,23,17,21,17,17,13,15,
     $ 5,15, 0, 2,17,19,13,23, 9,23, 5,19, 5,11, 9, 7,13, 7,17,11,17,19,
     $ 0,13,11,17, 7, 0, 2, 5, 7, 5,23,13,23,17,21,17,17,13,15, 5,15, 0,
     $13,15,17, 7, 0, 1,17,19,13,23, 9,23, 5,20, 5,18, 9,15,13,15,17,12,
     $17,10,13, 7, 9, 7, 5,10, 0, 2, 5,23,17,23, 0,11,23,11, 7, 0, 1, 5,
     $23, 5,10, 8, 7,14, 7,17,10,17,23, 0, 1, 5,23,11, 7,17,23, 0, 1, 5,
     $23, 8, 7,11,17,14, 7,17,23, 0, 2, 5,23,17, 7, 0,17,23, 5, 7, 0, 2,
     $ 5,23,11,13,17,23, 0,11,13,11, 7, 0, 1, 5,23,17,23, 5, 7,17, 7, 0,
     $ 1,11,23, 7,23, 7, 7,11, 7, 0, 1, 7,23,15, 7, 0, 1, 7,23,11,23,11,
     $ 7, 7, 7, 0, 1, 7,21,11,23,15,21, 0, 1, 5, 3,17, 3, 0, 1, 9,23,13,
     $19, 0, 2, 7,14, 9,15,13,15,15,14,15, 7, 0,15,12, 9,12, 7,11, 7, 8,
     $ 9, 7,13, 7,15, 8, 0, 2, 7,23, 7, 7, 0, 7,13, 9,15,13,15,15,13,15,
     $ 9,13, 7, 9, 7, 7, 9, 0, 1,15,13,13,15, 9,15, 7,13, 7, 9, 9, 7,13,
     $ 7,15, 9, 0, 2,15,13,13,15, 9,15, 7,13, 7, 9, 9, 7,13, 7,15, 9, 0,
     $15,23,15, 7, 0, 1, 7,11,15,11,15,13,13,15, 9,15, 7,13, 7, 9, 9, 7/
      DATA (IFONT(I),I=1189,1584)/
     $13, 7,15, 9, 0, 3, 9, 7, 9,23,13,23,13,22, 0, 8,15,12,15, 0, 8, 7,
     $11, 7, 0, 2,15,13,13,15, 9,15, 7,13, 7, 9, 9, 7,13, 7,15, 9, 0,15,
     $13,15, 3,13, 1, 9, 1, 7, 3, 0, 2, 7, 7, 7,23, 0, 7,14, 9,15,13,15,
     $15,14,15, 7, 0, 3, 9,15,11,15,11, 7, 0, 9, 7,13, 7, 0, 9,17, 9,19,
     $11,19,11,17, 9,17, 0, 2, 9,15,11,15,11, 1, 7, 1, 7, 3, 0, 9,17,11,
     $17,11,19, 9,19, 9,17, 0, 3, 7, 7, 7,23, 0,15,15, 7,10, 0, 9,11,15,
     $ 7, 0, 2, 9,23,11,23,11, 7, 0, 9, 7,13, 7, 0, 3, 7,15, 7, 7, 0, 7,
     $14, 8,15,10,15,11,14,11, 7, 0,11,14,12,15,14,15,15,14,15, 7, 0, 2,
     $ 7, 7, 7,15, 0, 7,14, 9,15,13,15,15,14,15, 7, 0, 1, 7,13, 9,15,13,
     $15,15,13,15, 9,13, 7, 9, 7, 7, 9, 7,13, 0, 2, 7,13, 9,15,13,15,15,
     $13,15, 9,13, 7, 9, 7, 7, 9, 0, 7,14, 7, 1, 0, 2,15,13,13,15, 9,15,
     $ 7,13, 7, 9, 9, 7,13, 7,15, 9, 0,15,14,15, 1, 0, 2, 7,15, 9,15, 9,
     $ 7, 0, 9,13,11,15,13,15,15,13, 0, 1,15,13,13,15, 9,15, 7,13, 9,11,
     $13,11,15, 9,13, 7, 9, 7, 7, 9, 0, 2, 9,23, 9, 7,11, 7, 0, 7,17,11,
     $17, 0, 2, 7,15, 7, 9, 9, 7,13, 7,15, 9, 0,15,15,15, 7, 0, 1, 7,15,
     $11, 7,15,15, 0, 1, 7,15, 9, 7,11,11,13, 7,15,15, 0, 2, 7,15,15, 7,
     $ 0, 7, 7,15,15, 0, 2, 7,15,11, 7, 0,15,15,10, 5, 7, 1, 0, 1, 7,15,
     $15,15, 7, 7,15, 7, 0, 1,11,23, 7,23, 9,17, 7,15, 9,13, 7, 7,11, 7/
      DATA (IFONT(I),I=1585,1617)/
     $ 0, 1, 9,23, 9, 7, 0, 1, 7,23,11,23, 9,17,11,15, 9,13,11, 7, 7, 7,
     $ 0, 1, 5,21, 7,23,15,21,17,23, 0/
C
C  Note that the whole Macintosh screen is NOT available to FORTRAN.
C
C     XSMN=18
C     XSMX=494
      XSMN=116
      XSMX=396
      YSMN=311
      YSMX=31
      IXMN=XSMN
      IXMX=XSMX
      IYMN=YSMN
      IYMX=YSMX
C
C  ICOM=0  Enable graphics.  For interactive graphics, clear the screen.
C  For graphics which create a file, open the file.
C  Also, user will input the portion of the (0,1) by (0,1) output
C  screen that is to be used.  At this point, the effective screen
C  (IXMIN,IXMAX) by (IYMIN,IYMAX) must be computed.
C
      IF(ICOM.EQ.0)THEN
        XSMIN=XSMN+XPLT1*(XSMX-XSMN)
        XSMAX=XSMN+XPLT2*(XSMX-XSMN)
        YSMIN=YSMN+YPLT1*(YSMX-YSMN)
        YSMAX=YSMN+YPLT2*(YSMX-YSMN)
        IXMIN=XSMIN
        IXMAX=XSMAX
        IYMIN=YSMIN
        IYMAX=YSMAX
        XMIN=0.0
        XMAX=1.0
        YMIN=0.0
        YMAX=1.0
        ENDIF
C
C  ICOM=1  Disable graphics.  For interactive graphics, clear the screen.
C  For graphics which create a file, close the file.
C  (Doesn't really apply for Macintosh)
C
C  ICOM=2  Begin plot
C
      IF(ICOM.EQ.2)THEN
        XMIN=0.0
        XMAX=1.0
        YMIN=0.0
        YMAX=1.0
        DO 33 I=1,48
          WRITE(*,'(1X)')
33        CONTINUE
        ENDIF
C
C  ICOM=3  Define user coordinate system (XMIN,XMAX), (YMIN,YMAX).
C
      IF(ICOM.EQ.3)THEN
        XMIN=XPLT1
        XMAX=XMIN+XPLT2
        YMIN=YPLT1
        YMAX=YMIN+YPLT2
        ENDIF
C
C  ICOM=4  Move to point.  Only purpose is to begin a line.
C  Input is X, Y in user coordinates.
C
      IF(ICOM.EQ.4)THEN
        XOLD=XPLT1
        YOLD=YPLT1
        ENDIF
C
C  ICOM=5  Draw to point.  Only called after a previous move or draw.
C  Input is X, Y in user coordinates.
C
      IF(ICOM.EQ.5)THEN
C
        CALL CLIP(XOLD,YOLD,XPLT1,YPLT1,XC,YC,XD,YD,IDRAW,
     *  XMIN,XMAX,YMIN,YMAX)
C
        IF(IDRAW.EQ.1)THEN
          CALL XTOIX(IX1,IXMAX,IXMIN,XC,XMAX,XMIN)
          CALL XTOIX(IY1,IYMAX,IYMIN,YC,YMAX,YMIN)
          CALL XTOIX(IX2,IXMAX,IXMIN,XD,XMAX,XMIN)
          CALL XTOIX(IY2,IYMAX,IYMIN,YD,YMAX,YMIN)
C
          IX=IX1
          IY=IY1
          CALL TOOLBX(MOVETO,IX,IY)
          IX=IX2
          IY=IY2
          CALL TOOLBX(LINETO,IX,IY)
          ENDIF
C
        XOLD=XPLT1
        YOLD=YPLT1
        ENDIF
C
C  ICOM=6  Clear screen.  Must be a better way on the Macintosh, but I
C  don't know it yet.
C
      IF(ICOM.EQ.6)THEN
        DO 330 I=1,48
          WRITE(*,'(1X)')
330       CONTINUE
          ENDIF
C
C  ICOM=7,  Write string at position.  
C  Variable height and angle should be supported.  
C  Note that for this command, screen coordinates are used.
C  Thus a width of 0.1 is intended to mean 1/10 of screen size.
C
      IF(ICOM.EQ.7)THEN
C 
C  Set scale factor for character height
C
        CSIZE=XPLT2
        ANGLE=YPLT2
        SCL2=CSIZE*.0625
C
C  Set starting point for line of text (lower left corner of first
C  character) and origin about which rotation is performed.
C
        XB=XPLT1
        XROT=XPLT1
        YB=YPLT1
        YROT=YPLT1
        ROTATE=.FALSE.
C
C  Get trig functions if rotation required, converting from 
C  degrees to radians.
C
        IF(ANGLE.NE.0.0)THEN
          CA=COS(ANGLE*.017453)
          SA=SIN(ANGLE*.017453)
          ROTATE=.TRUE.
          ENDIF
C
C  Loop over all characters in string
C
        DO 30 ICR=1,MARRAY
C
C  Get ASCII code for character and shift by 31 so first printable
C  character becomes code 1
C
          IASCII=ICHAR(CARRAY(ICR:ICR))-31
C
C  Replace any non-printable characters with blanks
C
          IF((IASCII.LT.1).OR.(IASCII.GT.95))IASCII=1
C
C  Get pointer to this character in font table
C
          IP=IPOINT(IASCII)
C
C  Get number of "vectors" required to draw character.
C  Here "vectors" means number of times pen is lowered, not the
C  number of pen strokes. (=1 for blanks, due to way algorithm is coded)
C
          NVEC=IFONT(IP)
C
C  Loop over all required pen movements
C
          DO 20 IV=1,NVEC
            IPEN=3
            IP=IP+1
15          CONTINUE
            IF(IFONT(IP).EQ.0)GO TO 20
            X=XB+(IFONT(IP)-1)*SCL2
            Y=YB+(IFONT(IP+1)-7)*SCL2
C
C  Apply rotation if necessary
C
            IF(ROTATE)THEN
              XT=X-XROT
              YT=Y-YROT
              X=CA*XT-SA*YT+XROT
              Y=SA*XT+CA*YT+YROT
              ENDIF
C
C  Plot the pen stroke
C
           IF(IPEN.EQ.3)THEN
              CALL XTOIX(IX2,IXMAX,IXMIN,X,1.0,0.0)
              CALL XTOIX(IY2,IYMAX,IYMIN,Y,1.0,0.0)
              IX=IX2
              IY=IY2
              CALL TOOLBX(MOVETO,IX,IY)
            ELSE
              IX1=IX2
              IY1=IY2
              CALL XTOIX(IX2,IXMAX,IXMIN,X,1.0,0.0)
              CALL XTOIX(IY2,IYMAX,IYMIN,Y,1.0,0.0)
              IX=IX2
              IY=IY2
              CALL TOOLBX(LINETO,IX,IY)
              ENDIF
            IPEN=2
            IP=IP+2
            GO TO 15
20          CONTINUE
C
C  Advance base to compensate for character just drawn
C
          XB=XB+CSIZE
30        CONTINUE
        ENDIF
C
C  ICOM=8  Use virtual cursor.  Not implemented.
C
C  ICOM=9  End plot
C
      IF(ICOM.EQ.9)THEN
        WRITE(*,'(1X)')
        READ(*,'(1X)')
        DO 70 I=1,48
          WRITE(*,'(1X)')
70        CONTINUE
        ENDIF
C
C  ICOM=10  Ring bell
C
      IF(ICOM.EQ.10)THEN
        CONG=CHAR(7)
        WRITE(*,'(1X,A1)')CONG
        ENDIF
C
C  ICOM=11  Mark data with a set of strokes that are like
C  a *, a +, or an X.  If a '.' is requested, actually try to
C  draw a point (pixel) if possible.
C
      IF(ICOM.EQ.11)THEN
        IF(CARRAY(1:1).EQ.' ')THEN
          RETURN
        ELSEIF(CARRAY(1:1).EQ.'.')THEN
          CALL XTOIX(IX1,IXMAX,IXMIN,XPLT1,XMAX,XMIN)
          CALL XTOIX(IY1,IYMAX,IYMIN,YPLT1,YMAX,YMIN)
          IX=IX1
          IY=IY1
          CALL TOOLBX(MOVETO,IX,IY)
          CALL TOOLBX(LINETO,IX,IY)
        ELSE
          DELT=0.5*XLEN*14.0/REAL(IXLEN)
          X1=XPLT1+DELT
          X2=XPLT1-DELT
          Y1=YPLT1+DELT
          Y2=YPLT1-DELT
          CALL XTOIX(IX1,IXMAX,IXMIN,X1,XMAX,XMIN)
          CALL XTOIX(IX2,IXMAX,IXMIN,X2,XMAX,XMIN)
          CALL XTOIX(IX3,IXMAX,IXMIN,X3,XMAX,XMIN)
          CALL XTOIX(IY1,IYMAX,IYMIN,Y1,YMAX,YMIN)
          CALL XTOIX(IY2,IYMAX,IYMIN,Y2,YMAX,YMIN)
          CALL XTOIX(IY3,IYMAX,IYMIN,Y3,YMAX,YMIN)
          IF(CARRAY(1:1).NE.'+')THEN
            IX=IX1
            IY=IY2
            CALL TOOLBX(MOVETO,IX,IY)
            IX=IX2
            IY=IY1
            CALL TOOLBX(LINETO,IX,IY)
            IX=IX1
            IY=IY1
            CALL TOOLBX(MOVETO,IX,IY)
            IX=IX2
            IY=IY2
            CALL TOOLBX(LINETO,IX,IY)
            ENDIF
          IF(CARRAY(1:1).NE.'X'.AND.CARRAY(1:1).NE.'x')THEN
            IX=IX1
            IY=IY3
            CALL TOOLBX(MOVETO,IX,IY)
            IX=IX2
            IY=IY3
            CALL TOOLBX(LINETO,IX,IY)
            IX=IX3
            IY=IY1
            CALL TOOLBX(MOVETO,IX,IY)
            IX=IX3
            IY=IY2
            CALL TOOLBX(LINETO,IX,IY)
            IX2=IX3
            ENDIF
          ENDIF
        ENDIF
C
C  ICOM=12, Return screen X, Y maximum and minimum in pixels
C  or other 'natural' coordinates.
C
      IF(ICOM.EQ.12)THEN
        XPLT1=XSMN
        XPLT2=XSMX
        YPLT1=YSMN
        YPLT2=YSMX
        ENDIF
C
C  ICOM=13  Return version number and date of code.
C
      IF(ICOM.EQ.13)THEN
        CARRAY='ANYPLT - Version 1.05  14 September 1990  Macintosh'
        ENDIF
      RETURN
      END
      SUBROUTINE XTOIX(IX,IXMAX,IXMIN,X,XMAX,XMIN)
C
C***********************************************************************
C
      IX=INT(REAL(IXMAX-IXMIN)*(X-XMIN)/(XMAX-XMIN))+IXMIN
      IF(IX.LT.IXMIN.AND.IX.LT.IXMAX)IX=MIN(IXMIN,IXMAX)
      IF(IX.GT.IXMIN.AND.IX.GT.IXMAX)IX=MAX(IXMIN,IXMAX)
      RETURN
      END
      SUBROUTINE CLIP(XA,YA,XB,YB,XC,YC,XD,YD,IDRAW,X0,X1,Y0,Y1)
C
      DIMENSION XVAL(2),YVAL(2)
C
      CALL CLIP1(XA,YA,XB,YB,XVAL,YVAL,NEXT,X0,X1,Y0,Y1)
      IF(NEXT.LT.2)THEN
        IDRAW=0
      ELSE
        IDRAW=1
        ENDIF
      XC=XVAL(1)
      YC=YVAL(1)
      XD=XVAL(2)
      YD=YVAL(2)
      RETURN
      END
      SUBROUTINE CLIP1(XA,YA,XB,YB,XVAL,YVAL,NEXT,X0,X1,Y0,Y1)
C
      DIMENSION XVAL(2),YVAL(2)
C
C  Check to see if both points are interior to the box, and hence
C  nothing need be done.
C
      NEXT=0
      IF(
     *  (X0.LE.XA.AND.XA.LE.X1).AND.
     *  (Y0.LE.YA.AND.YA.LE.Y1))THEN
        NEXT=NEXT+1
        XVAL(NEXT)=XA
        YVAL(NEXT)=YA
        ENDIF
      IF(
     *  (X0.LE.XB.AND.XB.LE.X1).AND.
     *  (Y0.LE.YB.AND.YB.LE.Y1))THEN
        NEXT=NEXT+1
        XVAL(NEXT)=XB
        YVAL(NEXT)=YB
        ENDIF
      IF(NEXT.EQ.2)RETURN
C
      CALL CLIP2(XA,YA,XB,YB,X0,Y0,Y1,Y,XVAL,YVAL,NEXT)
      IF(NEXT.EQ.2)RETURN
      CALL CLIP2(XA,YA,XB,YB,X1,Y0,Y1,Y,XVAL,YVAL,NEXT)
      IF(NEXT.EQ.2)RETURN
      CALL CLIP2(YA,XA,YB,XB,Y0,X0,X1,X,YVAL,XVAL,NEXT)
      IF(NEXT.EQ.2)RETURN
      CALL CLIP2(YA,XA,YB,XB,Y1,X0,X1,X,YVAL,XVAL,NEXT)
      RETURN
      END
      SUBROUTINE CLIP2(XA,YA,XB,YB,X0,Y0,Y1,Y,XVAL,YVAL,NEXT)
C
      DIMENSION XVAL(*),YVAL(*)
C
      IF(XB.EQ.XA)RETURN
C
      T=(X0-XA)/(XB-XA)
      IF(T.GE.0.0.AND.T.LE.1.0)THEN
        Y=(1.0-T)*YA+T*YB
        IF(Y0.LE.Y.AND.Y.LE.Y1)THEN
          NEXT=NEXT+1
          XVAL(NEXT)=X0
          YVAL(NEXT)=Y
          ENDIF
        ENDIF
      RETURN
      END
