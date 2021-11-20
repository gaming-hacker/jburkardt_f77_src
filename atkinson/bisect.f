      SUBROUTINE BISECT ( F, A, B, EPS, ROOT, IER)
C
C     THE PROGRAM USES THE BISECTION METHOD TO SOLVE
C     THE EQUATION
C                   F(X) = 0.
C     THE SOLUTION IS TO BE IN [A,B] AND IT IS ASSUMED
C     THAT
C                F(A)*F(B) .LE. 0.
C     THE SOLUTION IS RETURNED IN ROOT, AND IT IS TO
C     BE IN ERROR BY AT MOST EPS.
C
C     IER IS AN ERROR INDICATOR.
C     IF IER=0 ON COMPLETION OF THE ROUTINE, THEN THE
C        SOLUTION HAS BEEN COMPUTED SATISFACTORILY.
C     IF IER=1, THEN F(A)*F(B) WAS GREATER THAN 0,
C        CONTRARY TO ASSUMPTION.
c
c  Reference:
c
c    Kendall Atkinson, Weimin Han,
c    Elementary Numerical Analysis,
c    Wiley, 2004,
c    ISBN: 0471433373,
c    LC: QA297.A83.2004.
c
      PARAMETER(ZERO=0.0, ONE=1.0, TWO=2.0)
C
C     INITIALIZE.
      FA = F(A)
      FB = F(B)
      SFA = SIGN(ONE, FA)
      SFB = SIGN(ONE, FB)
      IF(SFA*SFB .GT. ZERO) THEN
C         THE CHOICE OF A AND B IS IN ERROR.
          IER = 1
          RETURN
      END IF
C
C     CREATE A NEW VALUE OF C, THE MIDPOINT OF [A,B].
10    C = (A + B)/TWO
      IF(ABS(B-C) .LE. EPS) THEN
C         C IS AN ACCEPTABLE SOLUTION OF F(X)=0.
          ROOT = C
          IER = 0
          RETURN
      END IF
C
C     THE VALUE OF C WAS NOT SUFFICIENTLY ACCURATE.
C     BEGIN A NEW ITERATION.
      FC = F(C)
      IF(FC .EQ. ZERO) then
         ROOT = C
          IER = 0
          RETURN
      END IF
      SFC = SIGN(ONE, FC)
      IF(SFB*SFC .GT. ZERO) THEN
C         THE SOLUTION IS IN [A,C].
          B = C
          SFB = SFC
      ELSE
C         THE SOLUTION IS IN [C,B].
          A = C
          SFA = SFC
      END IF
      GO TO 10
      END
