C  ANYNUL.FOR  08 September 1990  Version 1.01
C  ANYPLT dummy graphics interface
C
C  ANYPLT is a subroutine which provides a simple, standard interface
C  between FORTRAN programs and various output devices.  To run a
C  program which calls ANYPLT on a different machine, the program
C  is not modified in any way, but a different version of the ANYPLT
C  program is provided.  Currently, the following versions are available:
C
C  ANYATT - AT&T PC6300 super hi resolution graphics (640 by 400).
C           Requires auxilliary assembly language file ATTPLT.ASM.
C  ANYBUG - Simple debugging output to a file.
C  ANYCAL - CALCOMP file output.  Available on many mainframes.
C  ANYIBM - IBM PC hi resolution (640 by 200).  Requires auxilliary
C           assembly language file IBMPLT.ASM.
C  ANYMAC - Macintosh graphics.  Requires auxilliary routine TOOLBX.SUB.
C  ANYNCR - NCAR graphics package.
C  ANYNUL - Dummy, does nothing.
C  ANYP10 - PLOT10 interactive graphics. (1024 by 768)
C  ANYTTY - Simple 'typewriter' graphics (80 by 24)
C
C  The personal computer routines generally require an auxilliary
C  set of routines written in assembly language.
C
C  ANYPLT was written by
C
C  John Burkardt
C  Staff Programmer
C  Mathematics Department
C  University of Pittsburgh
C  Pittsburgh, PA
C
C  The symbolic output of characters, numbers and other printable 
C  characters was made possible by adaptation of a routine written
C  by Bill Furey of the University of Pittsburgh, Department of
C  Crystallography.
C
      SUBROUTINE ANYPLT(ICOM)

C*********************************************************************72
C
      CHARACTER CARRAY*80
C
C     COMMON /ANYCOM/ IPLT1,IPLT2,IXPLT1,IXPLT2,IYPLT1,
C    1                IYPLT2,MARRAY,XPLT1,XPLT2,YPLT1,YPLT2
      COMMON /ANYCHR/ CARRAY
C
C  ICOM=0  Enable graphics
C
C     IF(ICOM.EQ.0)THEN
C       ENDIF
C
C  ICOM=1  Disable graphics
C
C     IF(ICOM.EQ.1)THEN
C       ENDIF
C
C  ICOM=2  Begin new plot
C
C     IF(ICOM.EQ.2)THEN
C       ENDIF
C
C  ICOM=3  Define plot size
C
C     IF(ICOM.EQ.3)THEN
C       ENDIF
C
C  ICOM=4  Move to point
C
C     IF(ICOM.EQ.4)THEN
C       ENDIF
C
C  ICOM=5  Draw to point
C
C     IF(ICOM.EQ.5)THEN
C       ENDIF
C
C  ICOM=6  Clear screen
C
C     IF(ICOM.EQ.6)THEN
C       ENDIF
C
C  ICOM=7,  Write string at position.  
C  Variable height and angle should be supported.  
C  Note that for this command, screen coordinates are used.
C  Thus a width of 0.1 is intended to mean 1/10 of screen size.
C
C     IF(ICOM.EQ.7)THEN
C       ENDIF
C
C  ICOM=8  Use virtual cursor
C
C     IF(ICOM.EQ.8)THEN
C       ENDIF
C
C  ICOM=9  End plot
C
C     IF(ICOM.EQ.9)THEN
C       ENDIF
C
C  ICOM=10  Ring bell
C
C     IF(ICOM.EQ.10)THEN
C       ENDIF
C
C  ICOM=11  Mark data
C
C     IF(ICOM.EQ.11)THEN
C       ENDIF
C
C  ICOM=12  Screen data
C
C     IF(ICOM.EQ.12)THEN
C       ENDIF
C
C  ICOM=13  Return version
C
      IF(ICOM.EQ.13)THEN
        CARRAY='ANYNUL - Version 1.01  08 September 1990  NULL'
        ENDIF
      RETURN
      END
