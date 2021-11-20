      SUBROUTINE RIBESL(X, ALPHA, NB, IZE, B, NCALC)

c*********************************************************************72
c
cc RIBESL computes Bessel I functions with real (non integer) order.
c
C  THIS ROUTINE CALCULATES BESSEL FUNCTIONS I SUB(N+ALPHA) (X)
C  FOR NON-NEGATIVE ARGUMENT X, AND NON-NEGATIVE ORDER N+ALPHA,
C  WITH OR WITHOUT EXPONENTIAL SCALING.
C
C
C EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE
C
C X     - WORKING PRECISION NON-NEGATIVE REAL ARGUMENT FOR WHICH
C         I'S OR EXPONENTIALLY SCALED I'S (I*EXP(-X))
C         ARE TO BE CALCULATED.  IF I'S ARE TO BE CALCULATED,
C         X MUST BE LESS THAN EXPARG (SEE BELOW).
C ALPHA - WORKING PRECISION FRACTIONAL PART OF ORDER FOR WHICH
C         I'S OR EXPONENTIALLY SCALED I'S (I*EXP(-X)) ARE
C         TO BE CALCULATED.  0 .LE. ALPHA .LT. 1.0.
C NB    - INTEGER NUMBER OF FUNCTIONS TO BE CALCULATED, NB .GT. 0.
C         THE FIRST FUNCTION CALCULATED IS OF ORDER ALPHA, AND THE
C         LAST IS OF ORDER (NB - 1 + ALPHA).
C IZE   - INTEGER TYPE.  IZE = 1 IF UNSCALED I'S ARE TO CALCULATED,
C         AND 2 IF EXPONENTIALLY SCALED I'S ARE TO BE CALCULATED.
C B     - WORKING PRECISION OUTPUT VECTOR OF LENGTH NB.  IF THE ROUTINE
C         TERMINATES NORMALLY (NCALC=NB), THE VECTOR B CONTAINS THE
C         FUNCTIONS I/ALPHA/(X) THROUGH I/NB-1+ALPHA/(X), OR THE
C         CORRESPONDING EXPONENTIALLY SCALED FUNCTIONS.
C NCALC - INTEGER OUTPUT VARIABLE INDICATING POSSIBLE ERRORS.
C         BEFORE USING THE VECTOR B, THE USER SHOULD CHECK THAT
C         NCALC=NB, I.E., ALL ORDERS HAVE BEEN CALCULATED TO
C         THE DESIRED ACCURACY.  SEE ERROR RETURNS BELOW.
C
C
C*******************************************************************
C*******************************************************************
C
C EXPLANATION OF MACHINE-DEPENDENT CONSTANTS
C
C NSIG   - DECIMAL SIGNIFICANCE DESIRED.  SHOULD BE SET TO
C          IFIX(ALOG10(2)*NBIT+1), WHERE NBIT IS THE NUMBER OF
C          BITS IN THE MANTISSA OF A WORKING PRECISION VARIABLE.
C          SETTING NSIG LOWER WILL RESULT IN DECREASED ACCURACY
C          WHILE SETTING NSIG HIGHER WILL INCREASE CPU TIME
C          WITHOUT INCREASING ACCURACY.  THE TRUNCATION ERROR
C          IS LIMITED TO A RELATIVE ERROR OF T=.5*10**(-NSIG).
C ENTEN  - 10.0 ** K, WHERE K IS THE LARGEST INTEGER SUCH THAT
C          ENTEN IS MACHINE-REPRESENTABLE IN WORKING PRECISION.
C ENSIG  - 10.0 ** NSIG.
C RTNSIG - 10.0 ** (-K) FOR THE SMALLEST INTEGER K SUCH THAT
C          K .GE. NSIG/4.
C ENMTEN - THE SMALLEST ABS(X) SUCH THAT X/4 DOES NOT UNDERFLOW.
C XLARGE - UPPER LIMIT ON THE MAGNITUDE OF X WHEN IZE=2.  BEAR
C          IN MIND THAT IF ABS(X)=N, THEN AT LEAST N ITERATIONS
C          OF THE BACKWARD RECURSION WILL BE EXECUTED.
C EXPARG - LARGEST WORKING PRECISION ARGUMENT THAT THE LIBRARY
C          EXP ROUTINE CAN HANDLE AND UPPER LIMIT ON THE
C          MAGNITUDE OF X WHEN IZE=1.
C
C     APPROXIMATE VALUES FOR SOME IMPORTANT MACHINES ARE:
C
C         IBM/195    CDC/7600  UNIVAC/1108   VAX 11/780 (UNIX)
C          (D.P.)  (S.P.,RNDG)    (D.P.)     (S.P.)     (D.P.)
C
C NSIG      16         14          18           8        17
C ENTEN   1.0D75     1.0E322     1.0D307     1.0E38    1.0D38
C ENSIG   1.0D16     1.0E14      1.0D18      1.0E8     1.0D17
C RTNSIG  1.0D-4     1.0E-4      1.0D-5      1.0E-2    1.0D-4
C ENMTEN  2.2D-78    1.0E-290    1.2D-308    1.2E-37   1.2D-37
C XLARGE  1.0D4      1.0E4       1.0D4       1.0E4     1.0D4
C EXPARG  174.0D0    740.0E0     709.0D0     88.0E0    88.0D0
C
C*******************************************************************
C*******************************************************************
C
C
C ERROR RETURNS
C
C  IN CASE OF AN ERROR,  NCALC .NE. NB,  AND NOT ALL I'S ARE
C  CALCULATED TO THE DESIRED ACCURACY.
C
C  NCALC .LT. 0:  AN ARGUMENT IS OUT OF RANGE. FOR EXAMPLE,
C     NB .LE. 0, IZE IS NOT 1 OR 2, OR IZE=1 AND ABS(X) .GE. EXPARG.
C     IN THIS CASE, THE B-VECTOR IS NOT CALCULATED, AND NCALC IS
C     SET TO  MIN0(NB,0)-1  SO THAT NCALC .NE. NB.
C
C  NB .GT. NCALC .GT. 0: NOT ALL REQUESTED FUNCTION VALUES COULD
C     BE CALCULATED ACCURATELY.  THIS USUALLY OCCURS BECAUSE NB IS
C     MUCH LARGER THAN ABS(X).  IN THIS CASE, B(N) IS CALCULATED
C     TO THE DESIRED ACCURACY FOR  N .LE. NCALC,  BUT PRECISION
C     IS LOST FOR NCALC .LT. N .LE. NB.  IF B(N) DOES NOT VANISH
C     FOR  N .GT. NCALC  (BECAUSE IT IS TOO SMALL TO BE REPRESENTED),
C     AND  B(N)/B(NCALC) = 10**(-K), THEN ONLY THE FIRST NSIG-K
C     SIGNIFICANT FIGURES OF B(N) CAN BE TRUSTED.
C
C
C OTHER SUBPROGRAMS REQUIRED (SINGLE PRECISION VERSION)
C
C     EXP,GAMMA,AMAX1,SQRT,FLOAT,IFIX,MIN0
C
C OTHER SUBPROGRAMS REQUIRED (DOUBLE PRECISION VERSION)
C
C     DBLE,DEXP,DGAMMA,DMAX1,DSQRT,FLOAT,IFIX,MIN0,SNGL
C
C
C ACKNOWLEDGEMENT
C
C  THIS PROGRAM IS BASED ON A PROGRAM WRITTEN BY DAVID J.
C  SOOKNE (2) THAT COMPUTES VALUES OF THE BESSEL FUNCTIONS J OR
C  I OF REAL ARGUMENT AND INTEGER ORDER.  MODIFICATIONS INCLUDE
C  THE RESTRICTION OF THE COMPUTATION TO THE I BESSEL FUNCTION
C  OF NON-NEGATIVE REAL ARGUMENT, THE EXTENSION OF THE COMPUTATION
C  TO ARBITRARY POSITIVE ORDER, THE INCLUSION OF OPTIONAL
C  EXPONENTIAL SCALING, AND THE ELIMINATION OF MOST UNDERFLOW.
C
C REFERENCES
C
C  (1) OLVER, F. W. J., AND SOOKNE, D. J., "A NOTE ON BACKWARD
C        RECURRENCE ALGORITHMS," MATH. COMP. 26, 1972, PP 941-947.
C
C  (2) SOOKNE, D. J., "BESSEL FUNCTIONS OF REAL ARGUMENT AND
C        INTEGER ORDER," NBS JOUR. OF RES. B. 77B, 1973, PP 125-132.
C
C
C      MODIFIED BY: W. J. CODY
C                   APPLIED MATHEMATICS DIVISION
C                   ARGONNE NATIONAL LABORATORY
C                   ARGONNE, IL  60439
C
C      LATEST MODIFICATION: MAY 18, 1982
C
C-------------------------------------------------------------------
      INTEGER IZE, L, MAGX, N, NB, NBMX, NCALC, NEND, NSIG, NSTART
CS    REAL              ALPHA,B,EM,EMPAL,EMP2AL,EN,ENMTEN,ENSIG,
CS   2 ENTEN,EXPARG,GAMMA,HALF,HALFX,ONE,P,PLAST,POLD,PSAVE,PSAVEL,
CS   3 RTNSIG,SUM,TEMPA,TEMPB,TEMPC,TEST,TOVER,TWO,X,XLARGE,ZERO
      DOUBLE PRECISION ALPHA, B, EM, EMPAL, EMP2AL, EN, ENMTEN, ENSIG,
     * ENTEN, EXPARG, DGAMMA, HALF, HALFX, ONE, P, PLAST, POLD, PSAVE,
     * PSAVEL, RTNSIG, SUM, TEMPA, TEMPB, TEMPC, TEST, TOVER, TWO, X,
     * XLARGE, ZERO
      DIMENSION B(NB)
C-------------------------------------------------------------------
C  MATHEMATICAL CONSTANTS
C-------------------------------------------------------------------
CS    DATA ONE,TWO,ZERO,HALF/1.0E0,2.0E0,0.0E0,0.5E0/
      DATA ONE, TWO, ZERO, HALF /1.0D0,2.0D0,0.0D0,0.5D0/
C-------------------------------------------------------------------
C  MACHINE DEPENDENT PARAMETERS
C-------------------------------------------------------------------
CS    DATA NSIG,XLARGE,EXPARG / 7,1.0E4,88.0E0/
CS    DATA ENTEN,ENSIG,RTNSIG/1.0E38,1.0E7,1.0E-2/
CS    DATA ENMTEN/1.2E-37/
      DATA NSIG, XLARGE, EXPARG /17,1.0D4,88.0D0/
      DATA ENTEN, ENSIG, RTNSIG /1.0D38,1.0D17,1.0D-4/
      DATA ENMTEN /1.2D-37/
C-------------------------------------------------------------------
CS    MAGX = IFIX(X)
      MAGX = IFIX(SNGL(X))
      IF ((NB.GT.0) .AND. (X.GE.ZERO) .AND. (ALPHA.GE.ZERO) .AND.
     * (ALPHA.LT.ONE) .AND. (((IZE.EQ.1) .AND. (X.LE.EXPARG)) .OR.
     * ((IZE.EQ.2) .AND. (X.LE.XLARGE)))) GO TO 10
C-------------------------------------------------------------------
C ERROR RETURN -- X,NB,OR IZE IS OUT OF RANGE
C-------------------------------------------------------------------
      NCALC = MIN0(NB,0) - 1
      RETURN
C-------------------------------------------------------------------
C USE 2-TERM ASCENDING SERIES FOR SMALL X
C-------------------------------------------------------------------
   10 NCALC = NB
      IF (X.LT.RTNSIG) GO TO 210
C-------------------------------------------------------------------
C INITIALIZE THE FORWARD SWEEP, THE P-SEQUENCE OF OLVER
C-------------------------------------------------------------------
      NBMX = NB - MAGX
      N = MAGX + 1
CS    EN = FLOAT(N+N) + (ALPHA+ALPHA)
      EN = DBLE(FLOAT(N+N)) + (ALPHA+ALPHA)
      PLAST = ONE
      P = EN/X
C-------------------------------------------------------------------
C CALCULATE GENERAL SIGNIFICANCE TEST
C-------------------------------------------------------------------
      TEST = ENSIG + ENSIG
CS    IF (2*MAGX .GT. 5*NSIG) TEST = SQRT(TEST*P)
      IF (2*MAGX.GT.5*NSIG) TEST = DSQRT(TEST*P)
CS    IF (2*MAGX .LE. 5*NSIG) TEST = TEST / 1.585E0**MAGX
      IF (2*MAGX.LE.5*NSIG) TEST = TEST/1.585D0**MAGX
      IF (NBMX.LT.3) GO TO 30
C-------------------------------------------------------------------
C CALCULATE P-SEQUENCE UNTIL N = NB-1.  CHECK FOR POSSIBLE OVERFLOW.
C-------------------------------------------------------------------
      TOVER = ENTEN/ENSIG
      NSTART = MAGX + 2
      NEND = NB - 1
      DO 20 N=NSTART,NEND
        EN = EN + TWO
        POLD = PLAST
        PLAST = P
        P = EN*PLAST/X + POLD
        IF (P.GT.TOVER) GO TO 40
   20 CONTINUE
      N = NEND
CS    EN = FLOAT(N+N) + (ALPHA+ALPHA)
      EN = DBLE(FLOAT(N+N)) + (ALPHA+ALPHA)
C-------------------------------------------------------------------
C CALCULATE SPECIAL SIGNIFICANCE TEST FOR NBMX.GT.2.
C-------------------------------------------------------------------
CS    TEST = AMAX1(TEST,SQRT(PLAST*ENSIG)*SQRT(P+P))
      TEST = DMAX1(TEST,DSQRT(PLAST*ENSIG)*DSQRT(P+P))
C-------------------------------------------------------------------
C CALCULATE P-SEQUENCE UNTIL SIGNIFICANCE TEST PASSES
C-------------------------------------------------------------------
   30 N = N + 1
      EN = EN + TWO
      POLD = PLAST
      PLAST = P
      P = EN*PLAST/X + POLD
      IF (P.LT.TEST) GO TO 30
      GO TO 80
C-------------------------------------------------------------------
C TO AVOID OVERFLOW, DIVIDE P-SEQUENCE BY TOVER.  CALCULATE
C P-SEQUENCE UNTIL ABS(P).GT.1.
C-------------------------------------------------------------------
   40 TOVER = ENTEN
      P = P/TOVER
      PLAST = PLAST/TOVER
      PSAVE = P
      PSAVEL = PLAST
      NSTART = N + 1
   50 N = N + 1
      EN = EN + TWO
      POLD = PLAST
      PLAST = P
      P = EN*PLAST/X + POLD
      IF (P.LE.ONE) GO TO 50
      TEMPB = EN/X
C-------------------------------------------------------------------
C CALCULATE BACKWARD TEST, AND FIND NCALC, THE HIGHEST N
C SUCH THAT THE TEST IS PASSED.
C-------------------------------------------------------------------
      TEST = POLD*PLAST*(HALF-HALF/(TEMPB*TEMPB))/ENSIG
      P = PLAST*TOVER
      N = N - 1
      EN = EN - TWO
      NEND = MIN0(NB,N)
      DO 60 L=NSTART,NEND
        NCALC = L
        POLD = PSAVEL
        PSAVEL = PSAVE
        PSAVE = EN*PSAVEL/X + POLD
        IF (PSAVE*PSAVEL.GT.TEST) GO TO 70
   60 CONTINUE
      NCALC = NEND + 1
   70 NCALC = NCALC - 1
C-------------------------------------------------------------------
C INITIALIZE THE BACKWARD RECURSION AND THE NORMALIZATION SUM
C-------------------------------------------------------------------
   80 N = N + 1
      EN = EN + TWO
      TEMPB = ZERO
      TEMPA = ONE/P
CS    EM = FLOAT(N) - ONE
      EM = DBLE(FLOAT(N)) - ONE
      EMPAL = EM + ALPHA
      EMP2AL = (EM-ONE) + (ALPHA+ALPHA)
      SUM = TEMPA*EMPAL*EMP2AL/EM
      NEND = N - NB
      IF (NEND) 130, 110, 90
C-------------------------------------------------------------------
C RECUR BACKWARD VIA DIFFERENCE EQUATION, CALCULATING (BUT
C NOT STORING) B(N), UNTIL N = NB.
C-------------------------------------------------------------------
   90 DO 100 L=1,NEND
        N = N - 1
        EN = EN - TWO
        TEMPC = TEMPB
        TEMPB = TEMPA
        TEMPA = (EN*TEMPB)/X + TEMPC
        EM = EM - ONE
        EMP2AL = EMP2AL - ONE
        IF (N.EQ.1) GO TO 110
        IF (N.EQ.2) EMP2AL = ONE
        EMPAL = EMPAL - ONE
        SUM = (SUM+TEMPA*EMPAL)*EMP2AL/EM
  100 CONTINUE
C-------------------------------------------------------------------
C STORE B(NB)
C-------------------------------------------------------------------
  110 B(N) = TEMPA
      IF (NB.GT.1) GO TO 120
      SUM = (SUM+SUM) + TEMPA
      GO TO 190
C-------------------------------------------------------------------
C CALCULATE AND STORE B(NB-1)
C-------------------------------------------------------------------
  120 N = N - 1
      EN = EN - TWO
      B(N) = (EN*TEMPA)/X + TEMPB
      IF (N.EQ.1) GO TO 180
      EM = EM - ONE
      EMP2AL = EMP2AL - ONE
      IF (N.EQ.2) EMP2AL = ONE
      EMPAL = EMPAL - ONE
      SUM = (SUM+B(N)*EMPAL)*EMP2AL/EM
      GO TO 150
C-------------------------------------------------------------------
C N.LT.NB, SO STORE B(N) AND SET HIGHER ORDERS TO ZERO
C-------------------------------------------------------------------
  130 B(N) = TEMPA
      NEND = -NEND
      DO 140 L=1,NEND
        B(N+L) = ZERO
  140 CONTINUE
  150 NEND = N - 2
      IF (NEND.EQ.0) GO TO 170
C-------------------------------------------------------------------
C CALCULATE VIA DIFFERENCE EQUATION AND STORE B(N), UNTIL N = 2
C-------------------------------------------------------------------
      DO 160 L=1,NEND
        N = N - 1
        EN = EN - TWO
        B(N) = (EN*B(N+1))/X + B(N+2)
        EM = EM - ONE
        EMP2AL = EMP2AL - ONE
        IF (N.EQ.2) EMP2AL = ONE
        EMPAL = EMPAL - ONE
        SUM = (SUM+B(N)*EMPAL)*EMP2AL/EM
  160 CONTINUE
C-------------------------------------------------------------------
C CALCULATE B(1)
C-------------------------------------------------------------------
  170 B(1) = TWO*EMPAL*B(2)/X + B(3)
  180 SUM = (SUM+SUM) + B(1)
C-------------------------------------------------------------------
C NORMALIZE.  DIVIDE ALL B(N) BY SUM.
C-------------------------------------------------------------------
  190 CONTINUE
CS    IF (ALPHA.NE.ZERO)SUM=SUM*GAMMA(ONE+ALPHA)*(X*HALF)**(-ALPHA)
      IF (ALPHA.NE.ZERO) SUM = SUM*DGAMMA(ONE+ALPHA)*(X*HALF)**(-ALPHA)
CS    IF (IZE .EQ. 1) SUM = SUM * EXP(-X)
      IF (IZE.EQ.1) SUM = SUM*DEXP(-X)
      TEMPA = ENMTEN
      IF (SUM.GT.ONE) TEMPA = TEMPA*SUM
      DO 200 N=1,NB
        IF (B(N).LT.TEMPA) B(N) = ZERO
        B(N) = B(N)/SUM
  200 CONTINUE
      RETURN
C-------------------------------------------------------------------
C TWO-TERM ASCENDING SERIES FOR SMALL X
C-------------------------------------------------------------------
  210 TEMPA = ONE
      EMPAL = ONE + ALPHA
      HALFX = ZERO
      IF (X.GT.ENMTEN) HALFX = HALF*X
CS    IF (ALPHA .NE. ZERO) TEMPA = HALFX ** ALPHA / GAMMA(EMPAL)
      IF (ALPHA.NE.ZERO) TEMPA = HALFX**ALPHA/DGAMMA(EMPAL)
CS    IF (IZE .EQ. 2) TEMPA = TEMPA * EXP(-X)
      IF (IZE.EQ.2) TEMPA = TEMPA*DEXP(-X)
      TEMPB = ZERO
      IF ((X+ONE).GT.ONE) TEMPB = HALFX*HALFX
      B(1) = TEMPA + TEMPA*TEMPB/EMPAL
      IF ((X.NE.ZERO) .AND. (B(1).EQ.ZERO)) NCALC = 0
      IF (NB.EQ.1) GO TO 250
      IF (X.GT.ZERO) GO TO 230
      DO 220 N=2,NB
        B(N) = ZERO
  220 CONTINUE
      GO TO 250
C-------------------------------------------------------------------
C CALCULATE HIGHER ORDER FUNCTIONS
C-------------------------------------------------------------------
  230 TEMPC = HALFX
      TOVER = (ENMTEN+ENMTEN)/X
      IF (TEMPB.NE.ZERO) TOVER = ENMTEN/TEMPB
      DO 240 N=2,NB
        TEMPA = TEMPA/EMPAL
        EMPAL = EMPAL + ONE
        TEMPA = TEMPA*TEMPC
        IF (TEMPA.LE.TOVER*EMPAL) TEMPA = ZERO
        B(N) = TEMPA + TEMPA*TEMPB/EMPAL
        IF ((B(N).EQ.ZERO) .AND. (NCALC.GT.N)) NCALC = N - 1
  240 CONTINUE
  250 RETURN
C ---------- LAST CARD OF RIBESL ----------
      END

