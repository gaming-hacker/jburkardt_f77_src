C  LISPRB.F  12 August 1992
C
      PROGRAM FRED
C
C  A sample FORTRAN file for analysis by LISTER.
C
C  This is the main program.
C
      X=1.0
      STOP
      END
      subroutine barbara
C
C  Let's insert a line that's longer than 72 characters here...
C
      X=X+MATRIX(I,J)*VECTOR(J)+INTEG(I)*CONSTRNT+ALPHA(I,J)*BETA(J)*GAMMA(1)
      RETURN
      END
      BLOCK DATA CARL
      DATA CHARLIE /0.6/
      END
C
C  These comment statements are not part of the body of any routine.
C  LISTER will assignm them to the next routine, DAVID.
C
      FUNCTION DAVID
C
      COMMON /FRED/ X,Y,Z
C
      DAVID=6.0
      RETURN
      END
      REAL
     *FUNCTION EARL
C
C  LISTER ought to recognize this!
C
      T=5.0
      ENTRY FRITZ
      EARL=17.0
      RETURN
      END
      INTEGER FUNCTION GARVEY
      GARVEY=1
      RETURN
      END
      LOGICAL FUNCTION HARVEY
      INCLUDE 'FRED.DAT'
      HARVEY=.TRUE.
      RETURN
      END
      CHARACTER FUNCTION INEZ
      INCLUDE 'FRED.DAT'
      INEZ='*'
      RETURN
      END
      COMPLEX FUNCTION JERRY
      JERRY=CMPLX(1.0,2.0)
      RETURN
      END

      REAL*8 FUNCTION KARL
C
C  LISTER will have problems with this.
C
      KARL=98.0
      RETURN
      END
      INTEGER*8
     *FUNCTION LARRY
      LARRY=3
      RETURN
      END
      CHARACTER*16 FUNCTION MOE
      MOE='Soitenly!'
      RETURN
      END
      LOGICAL*1 FUNCTION NINBAD
      NINBAD=.TRUE.
      RETURN
      END
      BLOCKDATA OLIVER
C
C  Here, we spelled "BLOCK DATA" as one word.
C
      DATA CHARLIE /0.6/
      END
