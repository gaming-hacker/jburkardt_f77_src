      FUNCTION BVALU (T, A, N, K, IDERIV, X, INBV, WORK)

c*********************************************************************72
c
cc BVALU evaluates a B-spline or its derivatives.
c
C***BEGIN PROLOGUE  BVALU
C***PURPOSE  Evaluate the B-representation of a B-spline at X for the
C            function value or any of its derivatives.
C***LIBRARY   SLATEC
C***CATEGORY  E3, K6
C***TYPE      SINGLE PRECISION (BVALU-S, DBVALU-D)
C***KEYWORDS  DIFFERENTIATION OF B-SPLINE, EVALUATION OF B-SPLINE
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C     Written by Carl de Boor and modified by D. E. Amos
C
C     Abstract
C         BVALU is the BVALUE function of the reference.
C
C         BVALU evaluates the B-representation (T,A,N,K) of a B-spline
C         at X for the function value on IDERIV = 0 or any of its
C         derivatives on IDERIV = 1,2,...,K-1.  Right limiting values
C         (right derivatives) are returned except at the right end
C         point X=T(N+1) where left limiting values are computed.  The
C         spline is defined on T(K) .LE. X .LE. T(N+1).  BVALU returns
C         a fatal error message when X is outside of this interval.
C
C         To compute left derivatives or left limiting values at a
C         knot T(I), replace N by I-1 and set X=T(I), I=K+1,N+1.
C
C         BVALU calls INTRV
C
C     Description of Arguments
C         Input
C          T       - knot vector of length N+K
C          A       - B-spline coefficient vector of length N
C          N       - number of B-spline coefficients
C                    N = sum of knot multiplicities-K
C          K       - order of the B-spline, K .GE. 1
C          IDERIV  - order of the derivative, 0 .LE. IDERIV .LE. K-1
C                    IDERIV=0 returns the B-spline value
C          X       - argument, T(K) .LE. X .LE. T(N+1)
C          INBV    - an initialization parameter which must be set
C                    to 1 the first time BVALU is called.
C
C         Output
C          INBV    - INBV contains information for efficient process-
C                    ing after the initial call and INBV must not
C                    be changed by the user.  Distinct splines require
C                    distinct INBV parameters.
C          WORK    - work vector of length 3*K.
C          BVALU   - value of the IDERIV-th derivative at X
C
C     Error Conditions
C         An improper input is a fatal error
C
C***REFERENCES  Carl de Boor, Package for calculating with B-splines,
C                 SIAM Journal on Numerical Analysis 14, 3 (June 1977),
C                 pp. 441-472.
C***ROUTINES CALLED  INTRV, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   800901  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  BVALU
C
      real bvalu
      INTEGER I,IDERIV,IDERP1,IHI,IHMKMJ,ILO,IMK,IMKPJ, INBV, IPJ,
     1 IP1, IP1MJ, J, JJ, J1, J2, K, KMIDER, KMJ, KM1, KPK, MFLAG, N
      REAL A, FKMJ, T, WORK, X
C     DIMENSION T(N+K), WORK(3*K)
      DIMENSION T(*), A(*), WORK(*)
C***FIRST EXECUTABLE STATEMENT  BVALU
      BVALU = 0.0E0
      IF(K.LT.1) GO TO 102
      IF(N.LT.K) GO TO 101
      IF(IDERIV.LT.0 .OR. IDERIV.GE.K) GO TO 110
      KMIDER = K - IDERIV
C
C *** FIND *I* IN (K,N) SUCH THAT T(I) .LE. X .LT. T(I+1)
C     (OR, .LE. T(I+1) IF T(I) .LT. T(I+1) = T(N+1)).
      KM1 = K - 1
      CALL INTRV(T, N+1, X, INBV, I, MFLAG)
      IF (X.LT.T(K)) GO TO 120
      IF (MFLAG.EQ.0) GO TO 20
      IF (X.GT.T(I)) GO TO 130
   10 IF (I.EQ.K) GO TO 140
      I = I - 1
      IF (X.EQ.T(I)) GO TO 10
C
C *** DIFFERENCE THE COEFFICIENTS *IDERIV* TIMES
C     WORK(I) = AJ(I), WORK(K+I) = DP(I), WORK(K+K+I) = DM(I), I=1.K
C
   20 IMK = I - K
      DO 30 J=1,K
        IMKPJ = IMK + J
        WORK(J) = A(IMKPJ)
   30 CONTINUE
      IF (IDERIV.EQ.0) GO TO 60
      DO 50 J=1,IDERIV
        KMJ = K - J
        FKMJ = KMJ
        DO 40 JJ=1,KMJ
          IHI = I + JJ
          IHMKMJ = IHI - KMJ
          WORK(JJ) = (WORK(JJ+1)-WORK(JJ))/(T(IHI)-T(IHMKMJ))*FKMJ
   40   CONTINUE
   50 CONTINUE
C
C *** COMPUTE VALUE AT *X* IN (T(I),(T(I+1)) OF IDERIV-TH DERIVATIVE,
C     GIVEN ITS RELEVANT B-SPLINE COEFF. IN AJ(1),...,AJ(K-IDERIV).
   60 IF (IDERIV.EQ.KM1) GO TO 100
      IP1 = I + 1
      KPK = K + K
      J1 = K + 1
      J2 = KPK + 1
      DO 70 J=1,KMIDER
        IPJ = I + J
        WORK(J1) = T(IPJ) - X
        IP1MJ = IP1 - J
        WORK(J2) = X - T(IP1MJ)
        J1 = J1 + 1
        J2 = J2 + 1
   70 CONTINUE
      IDERP1 = IDERIV + 1
      DO 90 J=IDERP1,KM1
        KMJ = K - J
        ILO = KMJ
        DO 80 JJ=1,KMJ
          WORK(JJ) = (WORK(JJ+1)*WORK(KPK+ILO)+WORK(JJ)
     1              *WORK(K+JJ))/(WORK(KPK+ILO)+WORK(K+JJ))
          ILO = ILO - 1
   80   CONTINUE
   90 CONTINUE
  100 BVALU = WORK(1)
      RETURN
C
C
  101 CONTINUE
      CALL XERMSG ('SLATEC', 'BVALU', 'N DOES NOT SATISFY N.GE.K', 2,
     +   1)
      RETURN
  102 CONTINUE
      CALL XERMSG ('SLATEC', 'BVALU', 'K DOES NOT SATISFY K.GE.1', 2,
     +   1)
      RETURN
  110 CONTINUE
      CALL XERMSG ('SLATEC', 'BVALU',
     +   'IDERIV DOES NOT SATISFY 0.LE.IDERIV.LT.K', 2, 1)
      RETURN
  120 CONTINUE
      CALL XERMSG ('SLATEC', 'BVALU',
     +   'X IS N0T GREATER THAN OR EQUAL TO T(K)', 2, 1)
      RETURN
  130 CONTINUE
      CALL XERMSG ('SLATEC', 'BVALU',
     +   'X IS NOT LESS THAN OR EQUAL TO T(N+1)', 2, 1)
      RETURN
  140 CONTINUE
      CALL XERMSG ('SLATEC', 'BVALU',
     +   'A LEFT LIMITING VALUE CANNOT BE OBTAINED AT T(K)', 2, 1)
      RETURN
      END
      INTEGER FUNCTION CHFCM (D1, D2, DELTA)

c*********************************************************************72
c
cc CHFCM checks a single cubic for monotonicity.
c
C***BEGIN PROLOGUE  CHFCM
C***SUBSIDIARY
C***PURPOSE  Check a single cubic for monotonicity.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (CHFCM-S, DCHFCM-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C *Usage:
C
C        REAL  D1, D2, DELTA
C        INTEGER  ISMON, CHFCM
C
C        ISMON = CHFCM (D1, D2, DELTA)
C
C *Arguments:
C
C     D1,D2:IN  are the derivative values at the ends of an interval.
C
C     DELTA:IN  is the data slope over that interval.
C
C *Function Return Values:
C     ISMON : indicates the monotonicity of the cubic segment:
C             ISMON = -3  if function is probably decreasing;
C             ISMON = -1  if function is strictly decreasing;
C             ISMON =  0  if function is constant;
C             ISMON =  1  if function is strictly increasing;
C             ISMON =  2  if function is non-monotonic;
C             ISMON =  3  if function is probably increasing.
C           If ABS(ISMON)=3, the derivative values are too close to the
C           boundary of the monotonicity region to declare monotonicity
C           in the presence of roundoff error.
C
C *Description:
C
C          CHFCM:  Cubic Hermite Function -- Check Monotonicity.
C
C    Called by  PCHCM  to determine the monotonicity properties of the
C    cubic with boundary derivative values D1,D2 and chord slope DELTA.
C
C *Cautions:
C     This is essentially the same as old CHFMC, except that a
C     new output value, -3, was added February 1989.  (Formerly, -3
C     and +3 were lumped together in the single value 3.)  Codes that
C     flag nonmonotonicity by "IF (ISMON.EQ.2)" need not be changed.
C     Codes that check via "IF (ISMON.GE.3)" should change the test to
C     "IF (IABS(ISMON).GE.3)".  Codes that declare monotonicity via
C     "IF (ISMON.LE.1)" should change to "IF (IABS(ISMON).LE.1)".
C
C   REFER TO  PCHCM
C
C***ROUTINES CALLED  R1MACH
C***REVISION HISTORY  (YYMMDD)
C   820518  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   831201  Changed from  ISIGN  to SIGN  to correct bug that
C           produced wrong sign when -1 .LT. DELTA .LT. 0 .
C   890206  Added SAVE statements.
C   890207  Added sign to returned value ISMON=3 and corrected
C           argument description accordingly.
C   890306  Added caution about changed output.
C   890407  Changed name from CHFMC to CHFCM, as requested at the
C           March 1989 SLATEC CML meeting, and made a few other
C           minor modifications necessitated by this change.
C   890407  Converted to new SLATEC format.
C   890407  Modified DESCRIPTION to LDOC format.
C   891214  Moved SAVE statements.  (WRB)
C***END PROLOGUE  CHFCM
C
C  Fortran intrinsics used:  SIGN.
C  Other routines used:  R1MACH.
C
C ----------------------------------------------------------------------
C
C  Programming notes:
C
C     TEN is actually a tuning parameter, which determines the width of
C     the fuzz around the elliptical boundary.
C
C     To produce a double precision version, simply:
C        a. Change CHFCM to DCHFCM wherever it occurs,
C        b. Change the real declarations to double precision, and
C        c. Change the constants ZERO, ONE, ... to double precision.
C
C  DECLARE ARGUMENTS.
C
      REAL  D1, D2, DELTA
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  ISMON, ITRUE
      REAL  A, B, EPS, FOUR, ONE, PHI, TEN, THREE, TWO, ZERO
      SAVE ZERO, ONE, TWO, THREE, FOUR
      SAVE TEN
C
C  INITIALIZE.
C
      DATA  ZERO /0./,  ONE /1.0/,  TWO /2./,  THREE /3./,  FOUR /4./,
     1      TEN /10./
C
C        MACHINE-DEPENDENT PARAMETER -- SHOULD BE ABOUT 10*UROUND.
C***FIRST EXECUTABLE STATEMENT  CHFCM
      EPS = TEN*R1MACH(4)
C
C  MAKE THE CHECK.
C
      IF (DELTA .EQ. ZERO)  THEN
C        CASE OF CONSTANT DATA.
         IF ((D1.EQ.ZERO) .AND. (D2.EQ.ZERO))  THEN
            ISMON = 0
         ELSE
            ISMON = 2
         ENDIF
      ELSE
C        DATA IS NOT CONSTANT -- PICK UP SIGN.
         ITRUE = SIGN (ONE, DELTA)
         A = D1/DELTA
         B = D2/DELTA
         IF ((A.LT.ZERO) .OR. (B.LT.ZERO))  THEN
            ISMON = 2
         ELSE IF ((A.LE.THREE-EPS) .AND. (B.LE.THREE-EPS))  THEN
C           INSIDE SQUARE (0,3)X(0,3)  IMPLIES   OK.
            ISMON = ITRUE
         ELSE IF ((A.GT.FOUR+EPS) .AND. (B.GT.FOUR+EPS))  THEN
C           OUTSIDE SQUARE (0,4)X(0,4)  IMPLIES   NONMONOTONIC.
            ISMON = 2
         ELSE
C           MUST CHECK AGAINST BOUNDARY OF ELLIPSE.
            A = A - TWO
            B = B - TWO
            PHI = ((A*A + B*B) + A*B) - THREE
            IF (PHI .LT. -EPS)  THEN
               ISMON = ITRUE
            ELSE IF (PHI .GT. EPS)  THEN
               ISMON = 2
            ELSE
C              TO CLOSE TO BOUNDARY TO TELL,
C                  IN THE PRESENCE OF ROUND-OFF ERRORS.
               ISMON = 3*ITRUE
            ENDIF
         ENDIF
      ENDIF
C
C  RETURN VALUE.
C
      CHFCM = ISMON
      RETURN
      END
      SUBROUTINE CHFDV (X1, X2, F1, F2, D1, D2, NE, XE, FE, DE, NEXT,
     +   IERR)

c*********************************************************************72
c
cc CHFDV evaluates a cubic polynomial and derivative in Hermite form.
c
C***BEGIN PROLOGUE  CHFDV
C***PURPOSE  Evaluate a cubic polynomial given in Hermite form and its
C            first derivative at an array of points.  While designed for
C            use by PCHFD, it may be useful directly as an evaluator
C            for a piecewise cubic Hermite function in applications,
C            such as graphing, where the interval is known in advance.
C            If only function values are required, use CHFEV instead.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3, H1
C***TYPE      SINGLE PRECISION (CHFDV-S, DCHFDV-D)
C***KEYWORDS  CUBIC HERMITE DIFFERENTIATION, CUBIC HERMITE EVALUATION,
C             CUBIC POLYNOMIAL EVALUATION, PCHIP
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C        CHFDV:  Cubic Hermite Function and Derivative Evaluator
C
C     Evaluates the cubic polynomial determined by function values
C     F1,F2 and derivatives D1,D2 on interval (X1,X2), together with
C     its first derivative, at the points  XE(J), J=1(1)NE.
C
C     If only function values are required, use CHFEV, instead.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        INTEGER  NE, NEXT(2), IERR
C        REAL  X1, X2, F1, F2, D1, D2, XE(NE), FE(NE), DE(NE)
C
C        CALL  CHFDV (X1,X2, F1,F2, D1,D2, NE, XE, FE, DE, NEXT, IERR)
C
C   Parameters:
C
C     X1,X2 -- (input) endpoints of interval of definition of cubic.
C           (Error return if  X1.EQ.X2 .)
C
C     F1,F2 -- (input) values of function at X1 and X2, respectively.
C
C     D1,D2 -- (input) values of derivative at X1 and X2, respectively.
C
C     NE -- (input) number of evaluation points.  (Error return if
C           NE.LT.1 .)
C
C     XE -- (input) real array of points at which the functions are to
C           be evaluated.  If any of the XE are outside the interval
C           [X1,X2], a warning error is returned in NEXT.
C
C     FE -- (output) real array of values of the cubic function defined
C           by  X1,X2, F1,F2, D1,D2  at the points  XE.
C
C     DE -- (output) real array of values of the first derivative of
C           the same function at the points  XE.
C
C     NEXT -- (output) integer array indicating number of extrapolation
C           points:
C            NEXT(1) = number of evaluation points to left of interval.
C            NEXT(2) = number of evaluation points to right of interval.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           "Recoverable" errors:
C              IERR = -1  if NE.LT.1 .
C              IERR = -2  if X1.EQ.X2 .
C                (Output arrays have not been changed in either case.)
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811019  DATE WRITTEN
C   820803  Minor cosmetic changes for release 1.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  CHFDV
C  Programming notes:
C
C     To produce a double precision version, simply:
C        a. Change CHFDV to DCHFDV wherever it occurs,
C        b. Change the real declaration to double precision, and
C        c. Change the constant ZERO to double precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  NE, NEXT(2), IERR
      REAL  X1, X2, F1, F2, D1, D2, XE(*), FE(*), DE(*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I
      REAL  C2, C2T2, C3, C3T3, DEL1, DEL2, DELTA, H, X, XMI, XMA, ZERO
      SAVE ZERO
      DATA  ZERO /0./
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  CHFDV
      IF (NE .LT. 1)  GO TO 5001
      H = X2 - X1
      IF (H .EQ. ZERO)  GO TO 5002
C
C  INITIALIZE.
C
      IERR = 0
      NEXT(1) = 0
      NEXT(2) = 0
      XMI = MIN(ZERO, H)
      XMA = MAX(ZERO, H)
C
C  COMPUTE CUBIC COEFFICIENTS (EXPANDED ABOUT X1).
C
      DELTA = (F2 - F1)/H
      DEL1 = (D1 - DELTA)/H
      DEL2 = (D2 - DELTA)/H
C                                           (DELTA IS NO LONGER NEEDED.)
      C2 = -(DEL1+DEL1 + DEL2)
      C2T2 = C2 + C2
      C3 = (DEL1 + DEL2)/H
C                               (H, DEL1 AND DEL2 ARE NO LONGER NEEDED.)
      C3T3 = C3+C3+C3
C
C  EVALUATION LOOP.
C
      DO 500  I = 1, NE
         X = XE(I) - X1
         FE(I) = F1 + X*(D1 + X*(C2 + X*C3))
         DE(I) = D1 + X*(C2T2 + X*C3T3)
C          COUNT EXTRAPOLATION POINTS.
         IF ( X.LT.XMI )  NEXT(1) = NEXT(1) + 1
         IF ( X.GT.XMA )  NEXT(2) = NEXT(2) + 1
C        (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.)
  500 CONTINUE
C
C  NORMAL RETURN.
C
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     NE.LT.1 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'CHFDV',
     +   'NUMBER OF EVALUATION POINTS LESS THAN ONE', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     X1.EQ.X2 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'CHFDV', 'INTERVAL ENDPOINTS EQUAL', IERR,
     +   1)
      RETURN
      END
      SUBROUTINE CHFEV (X1, X2, F1, F2, D1, D2, NE, XE, FE, NEXT, IERR)

c*********************************************************************72
c
cc CHFEV evaluates a cubic polynomial in Hermite form.
c
C***BEGIN PROLOGUE  CHFEV
C***PURPOSE  Evaluate a cubic polynomial given in Hermite form at an
C            array of points.  While designed for use by PCHFE, it may
C            be useful directly as an evaluator for a piecewise cubic
C            Hermite function in applications, such as graphing, where
C            the interval is known in advance.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3
C***TYPE      SINGLE PRECISION (CHFEV-S, DCHFEV-D)
C***KEYWORDS  CUBIC HERMITE EVALUATION, CUBIC POLYNOMIAL EVALUATION,
C             PCHIP
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          CHFEV:  Cubic Hermite Function EValuator
C
C     Evaluates the cubic polynomial determined by function values
C     F1,F2 and derivatives D1,D2 on interval (X1,X2) at the points
C     XE(J), J=1(1)NE.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        INTEGER  NE, NEXT(2), IERR
C        REAL  X1, X2, F1, F2, D1, D2, XE(NE), FE(NE)
C
C        CALL  CHFEV (X1,X2, F1,F2, D1,D2, NE, XE, FE, NEXT, IERR)
C
C   Parameters:
C
C     X1,X2 -- (input) endpoints of interval of definition of cubic.
C           (Error return if  X1.EQ.X2 .)
C
C     F1,F2 -- (input) values of function at X1 and X2, respectively.
C
C     D1,D2 -- (input) values of derivative at X1 and X2, respectively.
C
C     NE -- (input) number of evaluation points.  (Error return if
C           NE.LT.1 .)
C
C     XE -- (input) real array of points at which the function is to be
C           evaluated.  If any of the XE are outside the interval
C           [X1,X2], a warning error is returned in NEXT.
C
C     FE -- (output) real array of values of the cubic function defined
C           by  X1,X2, F1,F2, D1,D2  at the points  XE.
C
C     NEXT -- (output) integer array indicating number of extrapolation
C           points:
C            NEXT(1) = number of evaluation points to left of interval.
C            NEXT(2) = number of evaluation points to right of interval.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           "Recoverable" errors:
C              IERR = -1  if NE.LT.1 .
C              IERR = -2  if X1.EQ.X2 .
C                (The FE-array has not been changed in either case.)
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811019  DATE WRITTEN
C   820803  Minor cosmetic changes for release 1.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890703  Corrected category record.  (WRB)
C   890703  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  CHFEV
C  Programming notes:
C
C     To produce a double precision version, simply:
C        a. Change CHFEV to DCHFEV wherever it occurs,
C        b. Change the real declaration to double precision, and
C        c. Change the constant ZERO to double precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  NE, NEXT(2), IERR
      REAL  X1, X2, F1, F2, D1, D2, XE(*), FE(*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I
      REAL  C2, C3, DEL1, DEL2, DELTA, H, X, XMI, XMA, ZERO
      SAVE ZERO
      DATA  ZERO /0./
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  CHFEV
      IF (NE .LT. 1)  GO TO 5001
      H = X2 - X1
      IF (H .EQ. ZERO)  GO TO 5002
C
C  INITIALIZE.
C
      IERR = 0
      NEXT(1) = 0
      NEXT(2) = 0
      XMI = MIN(ZERO, H)
      XMA = MAX(ZERO, H)
C
C  COMPUTE CUBIC COEFFICIENTS (EXPANDED ABOUT X1).
C
      DELTA = (F2 - F1)/H
      DEL1 = (D1 - DELTA)/H
      DEL2 = (D2 - DELTA)/H
C                                           (DELTA IS NO LONGER NEEDED.)
      C2 = -(DEL1+DEL1 + DEL2)
      C3 = (DEL1 + DEL2)/H
C                               (H, DEL1 AND DEL2 ARE NO LONGER NEEDED.)
C
C  EVALUATION LOOP.
C
      DO 500  I = 1, NE
         X = XE(I) - X1
         FE(I) = F1 + X*(D1 + X*(C2 + X*C3))
C          COUNT EXTRAPOLATION POINTS.
         IF ( X.LT.XMI )  NEXT(1) = NEXT(1) + 1
         IF ( X.GT.XMA )  NEXT(2) = NEXT(2) + 1
C        (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.)
  500 CONTINUE
C
C  NORMAL RETURN.
C
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     NE.LT.1 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'CHFEV',
     +   'NUMBER OF EVALUATION POINTS LESS THAN ONE', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     X1.EQ.X2 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'CHFEV', 'INTERVAL ENDPOINTS EQUAL', IERR,
     +   1)
      RETURN
      END
      REAL FUNCTION CHFIE (X1, X2, F1, F2, D1, D2, A, B)

c*********************************************************************72
c
cc CHFIE evaluates the integral of a single cubic for PCHIA.
c
C***BEGIN PROLOGUE  CHFIE
C***SUBSIDIARY
C***PURPOSE  Evaluates integral of a single cubic for PCHIA
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (CHFIE-S, DCHFIE-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C          CHFIE:  Cubic Hermite Function Integral Evaluator.
C
C     Called by  PCHIA  to evaluate the integral of a single cubic (in
C     Hermite form) over an arbitrary interval (A,B).
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        REAL  X1, X2, F1, F2, D1, D2, A, B
C        REAL  VALUE, CHFIE
C
C        VALUE = CHFIE (X1, X2, F1, F2, D1, D2, A, B)
C
C   Parameters:
C
C     VALUE -- (output) value of the requested integral.
C
C     X1,X2 -- (input) endpoints if interval of definition of cubic.
C
C     F1,F2 -- (input) function values at the ends of the interval.
C
C     D1,D2 -- (input) derivative values at the ends of the interval.
C
C     A,B -- (input) endpoints of interval of integration.
C
C***SEE ALSO  PCHIA
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   820730  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   870813  Minor cosmetic changes.
C   890411  1. Added SAVE statements (Vers. 3.2).
C           2. Added SIX to REAL declaration.
C   890411  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR section in prologue.  (WRB)
C   930503  Corrected to set VALUE=0 when IERR.ne.0.  (FNF)
C   930504  Eliminated IERR and changed name from CHFIV to CHFIE.  (FNF)
C***END PROLOGUE  CHFIE
C
C  Programming notes:
C  1. There is no error return from this routine because zero is
C     indeed the mathematically correct answer when X1.EQ.X2 .
C**End
C
C  DECLARE ARGUMENTS.
C
      REAL  X1, X2, F1, F2, D1, D2, A, B
C
C  DECLARE LOCAL VARIABLES.
C
      REAL  DTERM, FOUR, FTERM, H, HALF, PHIA1, PHIA2, PHIB1, PHIB2,
     *      PSIA1, PSIA2, PSIB1, PSIB2, SIX, TA1, TA2, TB1, TB2, THREE,
     *      TWO, UA1, UA2, UB1, UB2
      SAVE HALF, TWO, THREE, FOUR, SIX
C
C  INITIALIZE.
C
      DATA  HALF /0.5/,  TWO /2./,  THREE /3./,  FOUR /4./,  SIX /6./
C
C  VALIDITY CHECK INPUT.
C
C***FIRST EXECUTABLE STATEMENT  CHFIE
      IF (X1 .EQ. X2)  THEN
         CHFIE = 0
      ELSE
         H = X2 - X1
         TA1 = (A - X1) / H
         TA2 = (X2 - A) / H
         TB1 = (B - X1) / H
         TB2 = (X2 - B) / H
C
         UA1 = TA1**3
         PHIA1 = UA1 * (TWO - TA1)
         PSIA1 = UA1 * (THREE*TA1 - FOUR)
         UA2 = TA2**3
         PHIA2 =  UA2 * (TWO - TA2)
         PSIA2 = -UA2 * (THREE*TA2 - FOUR)
C
         UB1 = TB1**3
         PHIB1 = UB1 * (TWO - TB1)
         PSIB1 = UB1 * (THREE*TB1 - FOUR)
         UB2 = TB2**3
         PHIB2 =  UB2 * (TWO - TB2)
         PSIB2 = -UB2 * (THREE*TB2 - FOUR)
C
         FTERM =   F1*(PHIA2 - PHIB2) + F2*(PHIB1 - PHIA1)
         DTERM = ( D1*(PSIA2 - PSIB2) + D2*(PSIB1 - PSIA1) )*(H/SIX)
C
         CHFIE = (HALF*H) * (FTERM + DTERM)
      ENDIF
C
      RETURN
      END
      LOGICAL FUNCTION COMP (IERACT, IEREXP, LOUT, KPRINT)

c*********************************************************************72
c
cc COMP compares actual and expected error flag values.
c
C***BEGIN PROLOGUE  COMP
C***SUBSIDIARY
C***PURPOSE  Compare actual and expected values of error flag.
C***LIBRARY   SLATEC
C***KEYWORDS  QUICK CHECK SERVICE ROUTINE
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C     COMPARE ACTUAL VALUE OF IERR WITH EXPECTED VALUE.
C        PRINT ERROR MESSAGE IF THEY DON'T AGREE.
C
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   890618  REVISION DATE from Version 3.2
C   890706  Cosmetic changes to prologue.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  Revised prologue.  (FNF)
C   900316  Minor modification to format 5010.  (FNF)
C   910708  Minor modifications in use of KPRINT.  (WRB)
C***END PROLOGUE  COMP
      INTEGER  IERACT, IEREXP, LOUT, KPRINT
C***FIRST EXECUTABLE STATEMENT  COMP
      IF (IERACT .EQ. IEREXP)  THEN
         COMP = .TRUE.
         IF (KPRINT .GE. 3)  WRITE (LOUT, 5010)
 5010    FORMAT ('     OK.')
      ELSE
         COMP = .FALSE.
         IF (KPRINT .GE. 3)  WRITE (LOUT, 5020)  IERACT
 5020    FORMAT (' *** COMPARE FAILED -- IERR =',I5)
      ENDIF
C
      RETURN
C------------- LAST LINE OF COMP FOLLOWS -----------------------------
      END
      function d1mach ( i )

c*********************************************************************72
c
cc D1MACH returns double precision real machine-dependent constants.
c
c  Discussion:
c
c    D1MACH can be used to obtain machine-dependent parameters
c    for the local machine environment.  It is a function
c    with one input argument, and can be called as follows:
c
c      D = D1MACH ( I )
c
c    where I=1,...,5.  The output value of D above is
c    determined by the input value of I:.
c
c    D1MACH ( 1) = B^(EMIN-1), the smallest positive magnitude.
c    D1MACH ( 2) = B^EMAX*(1 - B^(-T)), the largest magnitude.
c    D1MACH ( 3) = B^(-T), the smallest relative spacing.
c    D1MACH ( 4) = B^(1-T), the largest relative spacing.
c    D1MACH ( 5) = LOG10(B)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    25 April 2007
c
c  Author:
c
c    Original FORTRAN77 version by Phyllis Fox, Andrew Hall, Norman Schryer.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Phyllis Fox, Andrew Hall, Norman Schryer,
c    Algorithm 528:
c    Framework for a Portable Library,
c    ACM Transactions on Mathematical Software,
c    Volume 4, Number 2, June 1978, page 176-188.
c
c  Parameters:
c
c    Input, integer I, the index of the desired constant.
c
c    Output, double precision D1MACH, the value of the constant.
c
      implicit none

      double precision d1mach
      integer i

      if ( i < 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'D1MACH - Fatal error!'
        write ( *, '(a)' ) '  The input argument I is out of bounds.'
        write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 5.'
        write ( *, '(a,i12)' ) '  I = ', i
        d1mach = 0.0D+00
        stop
      else if ( i == 1 ) then
        d1mach = 4.450147717014403D-308
      else if ( i == 2 ) then
        d1mach = 8.988465674311579D+307
      else if ( i == 3 ) then
        d1mach = 1.110223024625157D-016
      else if ( i == 4 ) then
        d1mach = 2.220446049250313D-016
      else if ( i == 5 ) then
        d1mach = 0.301029995663981D+000
      else if ( 5 < i ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'D1MACH - Fatal error!'
        write ( *, '(a)' ) '  The input argument I is out of bounds.'
        write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 5.'
        write ( *, '(a,i12)' ) '  I = ', i
        d1mach = 0.0D+00
        stop
      end if

      return
      end
      DOUBLE PRECISION FUNCTION DBVALU (T, A, N, K, IDERIV, X, INBV,
     +   WORK)

c*********************************************************************72
c
cc DBVALU evaluates a B-spline or its derivatives.
c
C***BEGIN PROLOGUE  DBVALU
C***PURPOSE  Evaluate the B-representation of a B-spline at X for the
C            function value or any of its derivatives.
C***LIBRARY   SLATEC
C***CATEGORY  E3, K6
C***TYPE      DOUBLE PRECISION (BVALU-S, DBVALU-D)
C***KEYWORDS  DIFFERENTIATION OF B-SPLINE, EVALUATION OF B-SPLINE
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C     Written by Carl de Boor and modified by D. E. Amos
C
C     Abstract   **** a double precision routine ****
C         DBVALU is the BVALUE function of the reference.
C
C         DBVALU evaluates the B-representation (T,A,N,K) of a B-spline
C         at X for the function value on IDERIV=0 or any of its
C         derivatives on IDERIV=1,2,...,K-1.  Right limiting values
C         (right derivatives) are returned except at the right end
C         point X=T(N+1) where left limiting values are computed.  The
C         spline is defined on T(K) .LE. X .LE. T(N+1).  DBVALU returns
C         a fatal error message when X is outside of this interval.
C
C         To compute left derivatives or left limiting values at a
C         knot T(I), replace N by I-1 and set X=T(I), I=K+1,N+1.
C
C         DBVALU calls DINTRV
C
C     Description of Arguments
C
C         Input      T,A,X are double precision
C          T       - knot vector of length N+K
C          A       - B-spline coefficient vector of length N
C          N       - number of B-spline coefficients
C                    N = sum of knot multiplicities-K
C          K       - order of the B-spline, K .GE. 1
C          IDERIV  - order of the derivative, 0 .LE. IDERIV .LE. K-1
C                    IDERIV = 0 returns the B-spline value
C          X       - argument, T(K) .LE. X .LE. T(N+1)
C          INBV    - an initialization parameter which must be set
C                    to 1 the first time DBVALU is called.
C
C         Output     WORK,DBVALU are double precision
C          INBV    - INBV contains information for efficient process-
C                    ing after the initial call and INBV must not
C                    be changed by the user.  Distinct splines require
C                    distinct INBV parameters.
C          WORK    - work vector of length 3*K.
C          DBVALU  - value of the IDERIV-th derivative at X
C
C     Error Conditions
C         An improper input is a fatal error
C
C***REFERENCES  Carl de Boor, Package for calculating with B-splines,
C                 SIAM Journal on Numerical Analysis 14, 3 (June 1977),
C                 pp. 441-472.
C***ROUTINES CALLED  DINTRV, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   800901  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DBVALU
C
      INTEGER I,IDERIV,IDERP1,IHI,IHMKMJ,ILO,IMK,IMKPJ, INBV, IPJ,
     1 IP1, IP1MJ, J, JJ, J1, J2, K, KMIDER, KMJ, KM1, KPK, MFLAG, N
      DOUBLE PRECISION A, FKMJ, T, WORK, X
      DIMENSION T(*), A(*), WORK(*)
C***FIRST EXECUTABLE STATEMENT  DBVALU
      DBVALU = 0.0D0
      IF(K.LT.1) GO TO 102
      IF(N.LT.K) GO TO 101
      IF(IDERIV.LT.0 .OR. IDERIV.GE.K) GO TO 110
      KMIDER = K - IDERIV
C
C *** FIND *I* IN (K,N) SUCH THAT T(I) .LE. X .LT. T(I+1)
C     (OR, .LE. T(I+1) IF T(I) .LT. T(I+1) = T(N+1)).
      KM1 = K - 1
      CALL DINTRV(T, N+1, X, INBV, I, MFLAG)
      IF (X.LT.T(K)) GO TO 120
      IF (MFLAG.EQ.0) GO TO 20
      IF (X.GT.T(I)) GO TO 130
   10 IF (I.EQ.K) GO TO 140
      I = I - 1
      IF (X.EQ.T(I)) GO TO 10
C
C *** DIFFERENCE THE COEFFICIENTS *IDERIV* TIMES
C     WORK(I) = AJ(I), WORK(K+I) = DP(I), WORK(K+K+I) = DM(I), I=1.K
C
   20 IMK = I - K
      DO 30 J=1,K
        IMKPJ = IMK + J
        WORK(J) = A(IMKPJ)
   30 CONTINUE
      IF (IDERIV.EQ.0) GO TO 60
      DO 50 J=1,IDERIV
        KMJ = K - J
        FKMJ = KMJ
        DO 40 JJ=1,KMJ
          IHI = I + JJ
          IHMKMJ = IHI - KMJ
          WORK(JJ) = (WORK(JJ+1)-WORK(JJ))/(T(IHI)-T(IHMKMJ))*FKMJ
   40   CONTINUE
   50 CONTINUE
C
C *** COMPUTE VALUE AT *X* IN (T(I),(T(I+1)) OF IDERIV-TH DERIVATIVE,
C     GIVEN ITS RELEVANT B-SPLINE COEFF. IN AJ(1),...,AJ(K-IDERIV).
   60 IF (IDERIV.EQ.KM1) GO TO 100
      IP1 = I + 1
      KPK = K + K
      J1 = K + 1
      J2 = KPK + 1
      DO 70 J=1,KMIDER
        IPJ = I + J
        WORK(J1) = T(IPJ) - X
        IP1MJ = IP1 - J
        WORK(J2) = X - T(IP1MJ)
        J1 = J1 + 1
        J2 = J2 + 1
   70 CONTINUE
      IDERP1 = IDERIV + 1
      DO 90 J=IDERP1,KM1
        KMJ = K - J
        ILO = KMJ
        DO 80 JJ=1,KMJ
          WORK(JJ) = (WORK(JJ+1)*WORK(KPK+ILO)+WORK(JJ)
     1              *WORK(K+JJ))/(WORK(KPK+ILO)+WORK(K+JJ))
          ILO = ILO - 1
   80   CONTINUE
   90 CONTINUE
  100 DBVALU = WORK(1)
      RETURN
C
C
  101 CONTINUE
      CALL XERMSG ('SLATEC', 'DBVALU', 'N DOES NOT SATISFY N.GE.K', 2,
     +   1)
      RETURN
  102 CONTINUE
      CALL XERMSG ('SLATEC', 'DBVALU', 'K DOES NOT SATISFY K.GE.1', 2,
     +   1)
      RETURN
  110 CONTINUE
      CALL XERMSG ('SLATEC', 'DBVALU',
     +   'IDERIV DOES NOT SATISFY 0.LE.IDERIV.LT.K', 2, 1)
      RETURN
  120 CONTINUE
      CALL XERMSG ('SLATEC', 'DBVALU',
     +   'X IS N0T GREATER THAN OR EQUAL TO T(K)', 2, 1)
      RETURN
  130 CONTINUE
      CALL XERMSG ('SLATEC', 'DBVALU',
     +   'X IS NOT LESS THAN OR EQUAL TO T(N+1)', 2, 1)
      RETURN
  140 CONTINUE
      CALL XERMSG ('SLATEC', 'DBVALU',
     +   'A LEFT LIMITING VALUE CANNOT BE OBTAINED AT T(K)', 2, 1)
      RETURN
      END
      INTEGER FUNCTION DCHFCM (D1, D2, DELTA)

c*********************************************************************72
c
cc DCHFCM checks a single cubic for monotonicity.
c
C***BEGIN PROLOGUE  DCHFCM
C***SUBSIDIARY
C***PURPOSE  Check a single cubic for monotonicity.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (CHFCM-S, DCHFCM-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C *Usage:
C
C        DOUBLE PRECISION  D1, D2, DELTA
C        INTEGER  ISMON, DCHFCM
C
C        ISMON = DCHFCM (D1, D2, DELTA)
C
C *Arguments:
C
C     D1,D2:IN  are the derivative values at the ends of an interval.
C
C     DELTA:IN  is the data slope over that interval.
C
C *Function Return Values:
C     ISMON : indicates the monotonicity of the cubic segment:
C             ISMON = -3  if function is probably decreasing;
C             ISMON = -1  if function is strictly decreasing;
C             ISMON =  0  if function is constant;
C             ISMON =  1  if function is strictly increasing;
C             ISMON =  2  if function is non-monotonic;
C             ISMON =  3  if function is probably increasing.
C           If ABS(ISMON)=3, the derivative values are too close to the
C           boundary of the monotonicity region to declare monotonicity
C           in the presence of roundoff error.
C
C *Description:
C
C          DCHFCM:  Cubic Hermite Function -- Check Monotonicity.
C
C    Called by  DPCHCM  to determine the monotonicity properties of the
C    cubic with boundary derivative values D1,D2 and chord slope DELTA.
C
C *Cautions:
C     This is essentially the same as old DCHFMC, except that a
C     new output value, -3, was added February 1989.  (Formerly, -3
C     and +3 were lumped together in the single value 3.)  Codes that
C     flag nonmonotonicity by "IF (ISMON.EQ.2)" need not be changed.
C     Codes that check via "IF (ISMON.GE.3)" should change the test to
C     "IF (IABS(ISMON).GE.3)".  Codes that declare monotonicity via
C     "IF (ISMON.LE.1)" should change to "IF (IABS(ISMON).LE.1)".
C
C   REFER TO  DPCHCM
C
C***ROUTINES CALLED  D1MACH
C***REVISION HISTORY  (YYMMDD)
C   820518  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   831201  Changed from  ISIGN  to SIGN  to correct bug that
C           produced wrong sign when -1 .LT. DELTA .LT. 0 .
C   890206  Added SAVE statements.
C   890209  Added sign to returned value ISMON=3 and corrected
C           argument description accordingly.
C   890306  Added caution about changed output.
C   890407  Changed name from DCHFMC to DCHFCM, as requested at the
C           March 1989 SLATEC CML meeting, and made a few other
C           minor modifications necessitated by this change.
C   890407  Converted to new SLATEC format.
C   890407  Modified DESCRIPTION to LDOC format.
C   891214  Moved SAVE statements.  (WRB)
C***END PROLOGUE  DCHFCM
C
C  Fortran intrinsics used:  DSIGN.
C  Other routines used:  D1MACH.
C
C ----------------------------------------------------------------------
C
C  Programming notes:
C
C     TEN is actually a tuning parameter, which determines the width of
C     the fuzz around the elliptical boundary.
C
C     To produce a single precision version, simply:
C        a. Change DCHFCM to CHFCM wherever it occurs,
C        b. Change the double precision declarations to real, and
C        c. Change the constants ZERO, ONE, ... to single precision.
C
C  DECLARE ARGUMENTS.
C
      DOUBLE PRECISION  D1, D2, DELTA, D1MACH
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER ISMON, ITRUE
      DOUBLE PRECISION  A, B, EPS, FOUR, ONE, PHI, TEN, THREE, TWO,
     * ZERO
      SAVE ZERO, ONE, TWO, THREE, FOUR
      SAVE TEN
C
C  INITIALIZE.
C
      DATA ZERO /0.D0/, ONE/1.D0/, TWO/2.D0/, THREE/3.D0/, FOUR/4.D0/,
     1      TEN /10.D0/
C
C        MACHINE-DEPENDENT PARAMETER -- SHOULD BE ABOUT 10*UROUND.
C***FIRST EXECUTABLE STATEMENT  DCHFCM
      EPS = TEN*D1MACH(4)
C
C  MAKE THE CHECK.
C
      IF (DELTA .EQ. ZERO)  THEN
C        CASE OF CONSTANT DATA.
         IF ((D1.EQ.ZERO) .AND. (D2.EQ.ZERO))  THEN
            ISMON = 0
         ELSE
            ISMON = 2
         ENDIF
      ELSE
C        DATA IS NOT CONSTANT -- PICK UP SIGN.
         ITRUE = DSIGN (ONE, DELTA)
         A = D1/DELTA
         B = D2/DELTA
         IF ((A.LT.ZERO) .OR. (B.LT.ZERO))  THEN
            ISMON = 2
         ELSE IF ((A.LE.THREE-EPS) .AND. (B.LE.THREE-EPS))  THEN
C           INSIDE SQUARE (0,3)X(0,3)  IMPLIES   OK.
            ISMON = ITRUE
         ELSE IF ((A.GT.FOUR+EPS) .AND. (B.GT.FOUR+EPS))  THEN
C           OUTSIDE SQUARE (0,4)X(0,4)  IMPLIES   NONMONOTONIC.
            ISMON = 2
         ELSE
C           MUST CHECK AGAINST BOUNDARY OF ELLIPSE.
            A = A - TWO
            B = B - TWO
            PHI = ((A*A + B*B) + A*B) - THREE
            IF (PHI .LT. -EPS)  THEN
               ISMON = ITRUE
            ELSE IF (PHI .GT. EPS)  THEN
               ISMON = 2
            ELSE
C              TO CLOSE TO BOUNDARY TO TELL,
C                  IN THE PRESENCE OF ROUND-OFF ERRORS.
               ISMON = 3*ITRUE
            ENDIF
         ENDIF
      ENDIF
C
C  RETURN VALUE.
C
      DCHFCM = ISMON
      RETURN
      END
      SUBROUTINE DCHFDV (X1, X2, F1, F2, D1, D2, NE, XE, FE, DE, NEXT,
     +   IERR)
c
c*********************************************************************72
c
cc DCHFDV evaluates a cubic Hermite polynomial and derivative at many points.
c
C***BEGIN PROLOGUE  DCHFDV
C***PURPOSE  Evaluate a cubic polynomial given in Hermite form and its
C            first derivative at an array of points.  While designed for
C            use by DPCHFD, it may be useful directly as an evaluator
C            for a piecewise cubic Hermite function in applications,
C            such as graphing, where the interval is known in advance.
C            If only function values are required, use DCHFEV instead.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3, H1
C***TYPE      DOUBLE PRECISION (CHFDV-S, DCHFDV-D)
C***KEYWORDS  CUBIC HERMITE DIFFERENTIATION, CUBIC HERMITE EVALUATION,
C             CUBIC POLYNOMIAL EVALUATION, PCHIP
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C        DCHFDV:  Cubic Hermite Function and Derivative Evaluator
C
C     Evaluates the cubic polynomial determined by function values
C     F1,F2 and derivatives D1,D2 on interval (X1,X2), together with
C     its first derivative, at the points  XE(J), J=1(1)NE.
C
C     If only function values are required, use DCHFEV, instead.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        INTEGER  NE, NEXT(2), IERR
C        DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, XE(NE), FE(NE),
C                          DE(NE)
C
C        CALL  DCHFDV (X1,X2, F1,F2, D1,D2, NE, XE, FE, DE, NEXT, IERR)
C
C   Parameters:
C
C     X1,X2 -- (input) endpoints of interval of definition of cubic.
C           (Error return if  X1.EQ.X2 .)
C
C     F1,F2 -- (input) values of function at X1 and X2, respectively.
C
C     D1,D2 -- (input) values of derivative at X1 and X2, respectively.
C
C     NE -- (input) number of evaluation points.  (Error return if
C           NE.LT.1 .)
C
C     XE -- (input) real*8 array of points at which the functions are to
C           be evaluated.  If any of the XE are outside the interval
C           [X1,X2], a warning error is returned in NEXT.
C
C     FE -- (output) real*8 array of values of the cubic function
C           defined by  X1,X2, F1,F2, D1,D2  at the points  XE.
C
C     DE -- (output) real*8 array of values of the first derivative of
C           the same function at the points  XE.
C
C     NEXT -- (output) integer array indicating number of extrapolation
C           points:
C            NEXT(1) = number of evaluation points to left of interval.
C            NEXT(2) = number of evaluation points to right of interval.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           "Recoverable" errors:
C              IERR = -1  if NE.LT.1 .
C              IERR = -2  if X1.EQ.X2 .
C                (Output arrays have not been changed in either case.)
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811019  DATE WRITTEN
C   820803  Minor cosmetic changes for release 1.
C   870707  Corrected XERROR calls for d.p. names(s).
C   870813  Minor cosmetic changes.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DCHFDV
C  Programming notes:
C
C     To produce a single precision version, simply:
C        a. Change DCHFDV to CHFDV wherever it occurs,
C        b. Change the double precision declaration to real, and
C        c. Change the constant ZERO to single precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  NE, NEXT(2), IERR
      DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, XE(*), FE(*), DE(*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I
      DOUBLE PRECISION  C2, C2T2, C3, C3T3, DEL1, DEL2, DELTA, H, X,
     *  XMI, XMA, ZERO
      SAVE ZERO
      DATA  ZERO /0.D0/
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  DCHFDV
      IF (NE .LT. 1)  GO TO 5001
      H = X2 - X1
      IF (H .EQ. ZERO)  GO TO 5002
C
C  INITIALIZE.
C
      IERR = 0
      NEXT(1) = 0
      NEXT(2) = 0
      XMI = MIN(ZERO, H)
      XMA = MAX(ZERO, H)
C
C  COMPUTE CUBIC COEFFICIENTS (EXPANDED ABOUT X1).
C
      DELTA = (F2 - F1)/H
      DEL1 = (D1 - DELTA)/H
      DEL2 = (D2 - DELTA)/H
C                                           (DELTA IS NO LONGER NEEDED.)
      C2 = -(DEL1+DEL1 + DEL2)
      C2T2 = C2 + C2
      C3 = (DEL1 + DEL2)/H
C                               (H, DEL1 AND DEL2 ARE NO LONGER NEEDED.)
      C3T3 = C3+C3+C3
C
C  EVALUATION LOOP.
C
      DO 500  I = 1, NE
         X = XE(I) - X1
         FE(I) = F1 + X*(D1 + X*(C2 + X*C3))
         DE(I) = D1 + X*(C2T2 + X*C3T3)
C          COUNT EXTRAPOLATION POINTS.
         IF ( X.LT.XMI )  NEXT(1) = NEXT(1) + 1
         IF ( X.GT.XMA )  NEXT(2) = NEXT(2) + 1
C        (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.)
  500 CONTINUE
C
C  NORMAL RETURN.
C
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     NE.LT.1 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DCHFDV',
     +   'NUMBER OF EVALUATION POINTS LESS THAN ONE', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     X1.EQ.X2 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DCHFDV', 'INTERVAL ENDPOINTS EQUAL',
     +   IERR, 1)
      RETURN
      END
      SUBROUTINE DCHFEV (X1, X2, F1, F2, D1, D2, NE, XE, FE, NEXT, IERR)

c*********************************************************************72
c
cc DCHFEV evaluates a cubic Hermite polynomial at many points.
c
C***BEGIN PROLOGUE  DCHFEV
C***PURPOSE  Evaluate a cubic polynomial given in Hermite form at an
C            array of points.  While designed for use by DPCHFE, it may
C            be useful directly as an evaluator for a piecewise cubic
C            Hermite function in applications, such as graphing, where
C            the interval is known in advance.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3
C***TYPE      DOUBLE PRECISION (CHFEV-S, DCHFEV-D)
C***KEYWORDS  CUBIC HERMITE EVALUATION, CUBIC POLYNOMIAL EVALUATION,
C             PCHIP
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          DCHFEV:  Cubic Hermite Function EValuator
C
C     Evaluates the cubic polynomial determined by function values
C     F1,F2 and derivatives D1,D2 on interval (X1,X2) at the points
C     XE(J), J=1(1)NE.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        INTEGER  NE, NEXT(2), IERR
C        DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, XE(NE), FE(NE)
C
C        CALL  DCHFEV (X1,X2, F1,F2, D1,D2, NE, XE, FE, NEXT, IERR)
C
C   Parameters:
C
C     X1,X2 -- (input) endpoints of interval of definition of cubic.
C           (Error return if  X1.EQ.X2 .)
C
C     F1,F2 -- (input) values of function at X1 and X2, respectively.
C
C     D1,D2 -- (input) values of derivative at X1 and X2, respectively.
C
C     NE -- (input) number of evaluation points.  (Error return if
C           NE.LT.1 .)
C
C     XE -- (input) real*8 array of points at which the function is to
C           be evaluated.  If any of the XE are outside the interval
C           [X1,X2], a warning error is returned in NEXT.
C
C     FE -- (output) real*8 array of values of the cubic function
C           defined by  X1,X2, F1,F2, D1,D2  at the points  XE.
C
C     NEXT -- (output) integer array indicating number of extrapolation
C           points:
C            NEXT(1) = number of evaluation points to left of interval.
C            NEXT(2) = number of evaluation points to right of interval.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           "Recoverable" errors:
C              IERR = -1  if NE.LT.1 .
C              IERR = -2  if X1.EQ.X2 .
C                (The FE-array has not been changed in either case.)
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811019  DATE WRITTEN
C   820803  Minor cosmetic changes for release 1.
C   870813  Corrected XERROR calls for d.p. names(s).
C   890206  Corrected XERROR calls.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DCHFEV
C  Programming notes:
C
C     To produce a single precision version, simply:
C        a. Change DCHFEV to CHFEV wherever it occurs,
C        b. Change the double precision declaration to real, and
C        c. Change the constant ZERO to single precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  NE, NEXT(2), IERR
      DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, XE(*), FE(*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I
      DOUBLE PRECISION  C2, C3, DEL1, DEL2, DELTA, H, X, XMI, XMA,
     * ZERO
      SAVE ZERO
      DATA  ZERO /0.D0/
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  DCHFEV
      IF (NE .LT. 1)  GO TO 5001
      H = X2 - X1
      IF (H .EQ. ZERO)  GO TO 5002
C
C  INITIALIZE.
C
      IERR = 0
      NEXT(1) = 0
      NEXT(2) = 0
      XMI = MIN(ZERO, H)
      XMA = MAX(ZERO, H)
C
C  COMPUTE CUBIC COEFFICIENTS (EXPANDED ABOUT X1).
C
      DELTA = (F2 - F1)/H
      DEL1 = (D1 - DELTA)/H
      DEL2 = (D2 - DELTA)/H
C                                           (DELTA IS NO LONGER NEEDED.)
      C2 = -(DEL1+DEL1 + DEL2)
      C3 = (DEL1 + DEL2)/H
C                               (H, DEL1 AND DEL2 ARE NO LONGER NEEDED.)
C
C  EVALUATION LOOP.
C
      DO 500  I = 1, NE
         X = XE(I) - X1
         FE(I) = F1 + X*(D1 + X*(C2 + X*C3))
C          COUNT EXTRAPOLATION POINTS.
         IF ( X.LT.XMI )  NEXT(1) = NEXT(1) + 1
         IF ( X.GT.XMA )  NEXT(2) = NEXT(2) + 1
C        (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.)
  500 CONTINUE
C
C  NORMAL RETURN.
C
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     NE.LT.1 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DCHFEV',
     +   'NUMBER OF EVALUATION POINTS LESS THAN ONE', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     X1.EQ.X2 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DCHFEV', 'INTERVAL ENDPOINTS EQUAL',
     +   IERR, 1)
      RETURN
      END
      DOUBLE PRECISION FUNCTION DCHFIE (X1, X2, F1, F2, D1, D2, A, B)

c*********************************************************************72
c
cc DCHFIE evaluates the integral of a single cubic for DPCHIA.
c
C***BEGIN PROLOGUE  DCHFIE
C***SUBSIDIARY
C***PURPOSE  Evaluates integral of a single cubic for DPCHIA
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (CHFIE-S, DCHFIE-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C          DCHFIE:  Cubic Hermite Function Integral Evaluator.
C
C     Called by  DPCHIA  to evaluate the integral of a single cubic (in
C     Hermite form) over an arbitrary interval (A,B).
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, A, B
C        DOUBLE PRECISION  VALUE, DCHFIE
C
C        VALUE = DCHFIE (X1, X2, F1, F2, D1, D2, A, B)
C
C   Parameters:
C
C     VALUE -- (output) value of the requested integral.
C
C     X1,X2 -- (input) endpoints if interval of definition of cubic.
C
C     F1,F2 -- (input) function values at the ends of the interval.
C
C     D1,D2 -- (input) derivative values at the ends of the interval.
C
C     A,B -- (input) endpoints of interval of integration.
C
C***SEE ALSO  DPCHIA
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   820730  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   870707  Corrected subroutine name from DCHIV to DCHFIV.
C   870813  Minor cosmetic changes.
C   890411  1. Added SAVE statements (Vers. 3.2).
C           2. Added SIX to DOUBLE PRECISION declaration.
C   890411  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR section in prologue.  (WRB)
C   930503  Corrected to set VALUE=0 when IERR.ne.0.  (FNF)
C   930504  Eliminated IERR and changed name DCHFIV to DCHFIE.  (FNF)
C***END PROLOGUE  DCHFIE
C
C  Programming notes:
C  1. There is no error return from this routine because zero is
C     indeed the mathematically correct answer when X1.EQ.X2 .
C**End
C
C  DECLARE ARGUMENTS.
C
      DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, A, B
C
C  DECLARE LOCAL VARIABLES.
C
      DOUBLE PRECISION  DTERM, FOUR, FTERM, H, HALF, PHIA1, PHIA2,
     *      PHIB1, PHIB2, PSIA1, PSIA2, PSIB1, PSIB2, SIX, TA1, TA2,
     *      TB1, TB2, THREE, TWO, UA1, UA2, UB1, UB2
      SAVE HALF, TWO, THREE, FOUR, SIX
C
C  INITIALIZE.
C
      DATA  HALF/.5D0/, TWO/2.D0/, THREE/3.D0/, FOUR/4.D0/, SIX/6.D0/
C
C  VALIDITY CHECK INPUT.
C
C***FIRST EXECUTABLE STATEMENT  DCHFIE
      IF (X1 .EQ. X2)  THEN
         DCHFIE = 0
      ELSE
         H = X2 - X1
         TA1 = (A - X1) / H
         TA2 = (X2 - A) / H
         TB1 = (B - X1) / H
         TB2 = (X2 - B) / H
C
         UA1 = TA1**3
         PHIA1 = UA1 * (TWO - TA1)
         PSIA1 = UA1 * (THREE*TA1 - FOUR)
         UA2 = TA2**3
         PHIA2 =  UA2 * (TWO - TA2)
         PSIA2 = -UA2 * (THREE*TA2 - FOUR)
C
         UB1 = TB1**3
         PHIB1 = UB1 * (TWO - TB1)
         PSIB1 = UB1 * (THREE*TB1 - FOUR)
         UB2 = TB2**3
         PHIB2 =  UB2 * (TWO - TB2)
         PSIB2 = -UB2 * (THREE*TB2 - FOUR)
C
         FTERM =   F1*(PHIA2 - PHIB2) + F2*(PHIB1 - PHIA1)
         DTERM = ( D1*(PSIA2 - PSIB2) + D2*(PSIB1 - PSIA1) )*(H/SIX)
C
         DCHFIE = (HALF*H) * (FTERM + DTERM)
      ENDIF
C
      RETURN
      END
      SUBROUTINE DEVCHK (LOUT, KPRINT, NPTS, XEV, FEV, DEV, FEV2, FAIL)

c*********************************************************************72
c
cc DEVCHK tests the evaluation accuracy of DCHFDV and DCHFEV for DPCHQ1.
c
C***BEGIN PROLOGUE  DEVCHK
C***SUBSIDIARY
C***PURPOSE  Test evaluation accuracy of DCHFDV and DCHFEV for DPCHQ1.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (EVCHCK-S, DEVCHK-D)
C***KEYWORDS  PCHIP EVALUATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C -------- CODE TO TEST EVALUATION ACCURACY OF DCHFDV AND DCHFEV -------
C
C     USING FUNCTION AND DERIVATIVE VALUES FROM A CUBIC (COMPUTED IN
C     DOUBLE PRECISION) AT NINT DIFFERENT (X1,X2) PAIRS:
C     1. CHECKS THAT DCHFDV AND DCHFEV BOTH REPRODUCE ENDPOINT VALUES.
C     2. EVALUATES AT NPTS POINTS, 10 OF WHICH ARE OUTSIDE THE INTERVAL
C        AND:
C        A. CHECKS ACCURACY OF DCHFDV FUNCTION AND DERIVATIVE VALUES
C           AGAINST EXACT VALUES.
C        B. CHECKS THAT RETURNED VALUES OF NEXT SUM TO 10.
C        C. CHECKS THAT FUNCTION VALUES FROM DCHFEV AGREE WITH THOSE
C           FROM DCHFDV.
C
C
C     FORTRAN INTRINSICS USED:  ABS, MAX, MIN.
C     FORTRAN LIBRARY ROUTINES USED:  SQRT, (READ), (WRITE).
C     SLATEC LIBRARY ROUTINES USED:  DCHFDV, DCHFEV, D1MACH, RAND.
C     OTHER ROUTINES USED:  DFDTRU.
C
C***ROUTINES CALLED  D1MACH, DCHFDV, DCHFEV, DFDTRU, RAND
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   820624  CONVERTED TO QUICK CHECK FOR SLATEC LIBRARY.
C   820630  1. MODIFIED DEFINITIONS OF RELATIVE ERROR AND TEST
C             TOLERANCES.
C           2. VARIOUS IMPROVEMENTS TO OUTPUT FORMATS.
C   820716  1. SET MACHEP VIA A CALL TO D1MACH.
C           2. CHANGED FROM FORTLIB'S RANF TO SLATEC'S RAND.
C   890628  1. Removed unnecessary IMPLICIT declaration.
C           2. Removed unnecessary variable NEV.
C           3. Other changes to reduce S.P./D.P. differences.
C   890629  Added RERR to DOUBLE PRECISION declaration.
C   890706  Cosmetic changes to prologue.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  Revised prologue and improved some output formats.  (FNF)
C           Also moved formats to end to be consistent with other PCHIP
C           quick checks.
C   900316  Additional minor cosmetic changes.  (FNF)
C   900321  Changed name of DFTRUE to DFDTRU and made additional minor
C           cosmetic changes.  (FNF)
C   901130  Added 1P's to formats and revised some to reduce maximum
C           line length.  (FNF)
C   910708  Minor modifications in use of KPRINT.  (WRB)
C   910801  Added EXTERNAL statement for RAND due to problem on IBM
C           RS 6000.  (WRB)
C   910819  Changed argument to RAND function from a D.P. zero to a
C           S.P. zero.  (WRB)
C***END PROLOGUE  DEVCHK
C
C  Declare arguments.
C
      INTEGER  LOUT, KPRINT, NPTS
      DOUBLE PRECISION XEV(*), FEV(*), DEV(*), FEV2(*)
      LOGICAL  FAIL
C
C  DECLARATIONS.
C
      INTEGER  I, IERR, IINT, NEXT(2), NEXT2(2), NINT
      DOUBLE PRECISION
     *      AED, AED2, AEDMAX, AEDMIN, AEF, AEF2, AEFMAX, AEFMIN,
     *      CHECK(2), CHECKF(2), CHECKD(2), D1, D2, DERMAX, DTRUE, DX,
     *      EPS1, EPS2, F1, F2, FACT, FERMAX, FLOORD, FLOORF, FOUR,
     *      FTRUE, LEFT(3), MACHEP,
     *      ONE, RED, RED2, REDMAX, REDMIN, REF, REF2, REFMAX,
     *      REFMIN, RIGHT(3), SMALL, TEN, TOL1, TOL2,
     *      X1, X2, XADMAX, XADMIN, XAFMAX, XAFMIN, XRDMAX,
     *      XRDMIN, XRFMAX, XRFMIN, ZERO
      LOGICAL  FAILOC, FAILNX
C
      DOUBLE PRECISION  D1MACH
C       The following should stay REAL (no D.P. equivalent).
      REAL  RAND
      EXTERNAL  RAND
C
C  DEFINE RELATIVE ERROR WITH FLOOR.
C
      DOUBLE PRECISION  RERR, ERR, VALUE, FLOOR
      RERR(ERR,VALUE,FLOOR) = ERR / MAX(ABS(VALUE), FLOOR)
C
C  INITIALIZE.
C
      DATA  ZERO /0.D0/, ONE /1.D0/, FOUR /4.D0/, TEN /10.D0/
      DATA  SMALL  /1.0D-10/
      DATA  NINT /3/
      DATA   LEFT /-1.5D0, 2.0D-10, 1.0D0 /
      DATA  RIGHT / 2.5D0, 3.0D-10, 1.0D+8/
C
C***FIRST EXECUTABLE STATEMENT  DEVCHK
      MACHEP = D1MACH(4)
      EPS1 = FOUR*MACHEP
      EPS2 = TEN*MACHEP
C
      FAIL = .FALSE.
C
      IF (KPRINT .GE. 2)  WRITE (LOUT, 3000)
C
C  CYCLE OVER INTERVALS.
C
      DO 90  IINT = 1, NINT
      X1 =  LEFT(IINT)
      X2 = RIGHT(IINT)
C
      FACT = MAX(SQRT(X2-X1), ONE)
      TOL1 = EPS1 * FACT
      TOL2 = EPS2 * FACT
C
C  COMPUTE AND PRINT ENDPOINT VALUES.
C
      CALL DFDTRU (X1, F1, D1)
      CALL DFDTRU (X2, F2, D2)
C
      IF (KPRINT .GE. 3)  THEN
         IF (IINT .EQ. 1)  WRITE (LOUT, 2000)
         WRITE (LOUT, '(/)')
         WRITE (LOUT, 2001)  'X1', X1, 'X2', X2
         WRITE (LOUT, 2001)  'F1', F1, 'F2', F2
         WRITE (LOUT, 2001)  'D1', D1, 'D2', D2
      ENDIF
C
      IF (KPRINT .GE. 2)  WRITE (LOUT, 3001)  X1, X2
C
C  COMPUTE FLOORS FOR RELATIVE ERRORS.
C
      FLOORF = MAX( MIN(ABS(F1),ABS(F2)), SMALL)
      FLOORD = MAX( MIN(ABS(D1),ABS(D2)), SMALL)
C
C  CHECK REPRODUCTION OF ENDPOINT VALUES.
C
      XEV(1) = X1
      XEV(2) = X2
C     -----------------------------------------------------------
      CALL DCHFDV (X1, X2, F1, F2, D1, D2, 2, XEV, CHECKF, CHECKD,
     *            NEXT, IERR)
C     -----------------------------------------------------------
      AEF  = CHECKF(1)-F1
      REF  = RERR(AEF , F1, FLOORF)
      AEF2 = CHECKF(2)-F2
      REF2 = RERR(AEF2, F2, FLOORF)
      AED  = CHECKD(1)-D1
      RED  = RERR(AED , D1, FLOORD)
      AED2 = CHECKD(2)-D2
      RED2 = RERR(AED2, D2, FLOORD)
C
      FAILOC = MAX(ABS(REF),ABS(REF2),ABS(RED),ABS(RED2)) .GT. TOL1
      FAIL = FAIL .OR. FAILOC
C
      IF (KPRINT .GE. 3)  THEN
         WRITE (LOUT, 2002)  NEXT, AEF, AEF2, AED, AED2
         WRITE (LOUT, 2003)  REF, REF2, RED, RED2
      ENDIF
C
      IF (FAILOC .AND. (KPRINT.GE.2))  WRITE (LOUT, 3002)
C
C  DCHFEV SHOULD AGREE EXACTLY WITH DCHFDV.
C                     -------
C     --------------------------------------------------------------
      CALL DCHFEV (X1, X2, F1, F2, D1, D2, 2, XEV, CHECK, NEXT, IERR)
C     --------------------------------------------------------------
      FAILOC = (CHECK(1).NE.CHECKF(1)) .OR. (CHECK(2).NE.CHECKF(2))
      FAIL = FAIL .OR. FAILOC
C
      IF (FAILOC .AND. (KPRINT.GE.2))  WRITE (LOUT, 3003)
C
C  EVALUATE AT NPTS 'UNIFORMLY RANDOM' POINTS IN (X1,X2).
C     THIS VERSION EXTENDS EVALUATION DOMAIN BY ADDING 4 SUBINTERVALS
C     TO LEFT AND 6 TO RIGHT OF [X1,X2].
C
      DX = (X2-X1)/(NPTS-10)
      DO 20  I = 1, NPTS
         XEV(I) = (X1 + (I-5)*DX) + DX*RAND(0.0E0)
   20 CONTINUE
C     --------------------------------------------------------
      CALL DCHFDV (X1, X2, F1, F2, D1, D2, NPTS, XEV, FEV, DEV,
     *            NEXT, IERR)
C     --------------------------------------------------------
      IF (IERR .NE. 0)  THEN
         FAILOC = .TRUE.
         IF (KPRINT .GE. 2)  WRITE (LOUT, 4003)  IERR
      ELSE
C
C     CUMULATE LARGEST AND SMALLEST ERRORS FOR SUMMARY.
C
      DO 30  I = 1, NPTS
         CALL DFDTRU (XEV(I), FTRUE, DTRUE)
         AEF = FEV(I) - FTRUE
         REF = RERR(AEF, FTRUE, FLOORF)
         AED = DEV(I) - DTRUE
         RED = RERR(AED, DTRUE, FLOORD)
C
         IF (I .EQ. 1)  THEN
C            INITIALIZE.
            AEFMIN = AEF
            AEFMAX = AEF
            AEDMIN = AED
            AEDMAX = AED
            REFMIN = REF
            REFMAX = REF
            REDMIN = RED
            REDMAX = RED
            XAFMIN = XEV(1)
            XAFMAX = XEV(1)
            XADMIN = XEV(1)
            XADMAX = XEV(1)
            XRFMIN = XEV(1)
            XRFMAX = XEV(1)
            XRDMIN = XEV(1)
            XRDMAX = XEV(1)
         ELSE
C            SELECT.
            IF (AEF .LT. AEFMIN)  THEN
               AEFMIN = AEF
               XAFMIN = XEV(I)
            ELSE IF (AEF .GT. AEFMAX)  THEN
               AEFMAX = AEF
               XAFMAX = XEV(I)
            ENDIF
            IF (AED .LT. AEDMIN)  THEN
               AEDMIN = AED
               XADMIN = XEV(I)
            ELSE IF (AED .GT. AEDMAX)  THEN
               AEDMAX = AED
               XADMAX = XEV(I)
            ENDIF
            IF (REF .LT. REFMIN)  THEN
               REFMIN = REF
               XRFMIN = XEV(I)
            ELSE IF (REF .GT. REFMAX)  THEN
               REFMAX = REF
               XRFMAX = XEV(I)
            ENDIF
            IF (RED .LT. REDMIN)  THEN
               REDMIN = RED
               XRDMIN = XEV(I)
            ELSE IF (RED .GT. REDMAX)  THEN
               REDMAX = RED
               XRDMAX = XEV(I)
            ENDIF
         ENDIF
   30    CONTINUE
C
         FERMAX = MAX (ABS(REFMAX), ABS(REFMIN))
         DERMAX = MAX (ABS(REDMAX), ABS(REDMIN))
C
         FAILNX = (NEXT(1) + NEXT(2)) .NE. 10
         FAILOC = FAILNX .OR. (MAX(FERMAX, DERMAX) .GT. TOL2)
      ENDIF
      FAIL = FAIL .OR. FAILOC
C
C  PRINT SUMMARY.
C
      IF (KPRINT .GE. 3)  THEN
         WRITE (LOUT, 2004)  NPTS-10, NEXT
C
         WRITE (LOUT, 2005)  'MIN', AEFMIN, REFMIN, AEDMIN, REDMIN
         WRITE (LOUT, 2006) XAFMIN, XRFMIN, XADMIN, XRDMIN
         WRITE (LOUT, 2005)  'MAX', AEFMAX, REFMAX, AEDMAX, REDMAX
         WRITE (LOUT, 2006) XAFMAX, XRFMAX, XADMAX, XRDMAX
      ENDIF
C
      IF (KPRINT .GE. 2)  THEN
         IF (FAILOC) THEN
            IF (FERMAX .GT. TOL2)  WRITE (LOUT, 3006) 'F', FERMAX, TOL2
            IF (DERMAX .GT. TOL2)  WRITE (LOUT, 3006) 'D', DERMAX, TOL2
            IF (FAILNX)  WRITE (LOUT, 4006)  NEXT
         ELSE
            WRITE (LOUT, 5006)
         ENDIF
      ENDIF
C
C  CHECK THAT DCHFEV AGREES WITH DCHFDV.
C
C     -----------------------------------------------------------------
      CALL DCHFEV (X1, X2, F1, F2, D1, D2, NPTS, XEV, FEV2, NEXT2, IERR)
C     -----------------------------------------------------------------
      IF (IERR .NE. 0)  THEN
         FAILOC = .TRUE.
         IF (KPRINT .GE. 2)  WRITE (LOUT, 3007)  IERR
      ELSE
         AEFMAX = ABS(FEV2(1) - FEV(1))
         XAFMAX = XEV(1)
         DO 40  I = 2, NPTS
            AEF = ABS(FEV2(I) - FEV(I))
            IF (AEF .GT. AEFMAX)  THEN
               AEFMAX = AEF
               XAFMAX = XEV(I)
            ENDIF
   40    CONTINUE
         FAILNX = (NEXT2(1).NE.NEXT(1)) .OR. (NEXT2(2).NE.NEXT(2))
         FAILOC = FAILNX .OR. (AEFMAX.NE.ZERO)
         IF (KPRINT .GE. 2)  THEN
            IF (FAILOC)  THEN
               WRITE (LOUT, 3008)
               IF (AEFMAX.NE.ZERO)  WRITE (LOUT, 3009)  AEFMAX, XAFMAX
               IF (FAILNX)  WRITE (LOUT, 4009)  NEXT2, NEXT
            ELSE
               WRITE (LOUT, 5009)
            ENDIF
         ENDIF
      ENDIF
C
      FAIL = FAIL .OR. FAILOC
C
C  GO BACK FOR ANOTHER INTERVAL.
C
   90 CONTINUE
C
      RETURN
C
C  FORMATS.
C
 2000 FORMAT (/10X,'DCHFDV ACCURACY TEST')
 2001 FORMAT (10X,A2,' =',1P,D18.10,5X,A2,' =',D18.10)
 2002 FORMAT (/' ERRORS AT ENDPOINTS:',40X,'(NEXT =',2I3,')'
     *        // 1P,4X,'F1:',D13.5,4X,'F2:',D13.5,
     *              4X,'D1:',D13.5,4X,'D2:',D13.5)
 2003 FORMAT (1P,4(7X,D13.5))
 2004 FORMAT (/' ERRORS AT ',I5,' INTERIOR POINTS + 10 OUTSIDE:',
     *                15X,'(NEXT =',2I3,')'
     *        //30X,'FUNCTION',17X,'DERIVATIVE'
     *         /15X,2(11X,'ABS',9X,'REL') )
 2005 FORMAT (/5X,A3,'IMUM ERROR:  ',1P,2D12.4,2X,2D12.4)
 2006 FORMAT ( 5X,'LOCATED AT X =  ',1P,2D12.4,2X,2D12.4)
 3000 FORMAT (//10X,'DEVCHK RESULTS'/10X,'--------------')
 3001 FORMAT (/10X,'INTERVAL = (',1P,D12.5,',',D12.5,' ):' )
 3002 FORMAT (/' ***** DCHFDV FAILED TO REPRODUCE ENDPOINT VALUES.')
 3003 FORMAT (/' ***** DCHFEV DOES NOT AGREE WITH DCHFDV AT ENDPOINTS.')
 3006 FORMAT (/' ***** MAXIMUM RELATIVE ERROR IN ',A1,' =',1P,D12.5,','
     *        /        17X,'EXCEEDS TOLERANCE =',D12.5)
 3007 FORMAT (/' ***** ERROR ***** DCHFEV RETURNED IERR =',I5)
 3008 FORMAT (/' ***** DCHFEV DID NOT AGREE WITH DCHFDV:')
 3009 FORMAT (7X,'MAXIMUM DIFFERENCE ',1P,D12.5,
     *                '; OCCURRED AT X =',D12.5)
 4003 FORMAT (/' ***** ERROR ***** DCHFDV RETURNED IERR =',I5)
 4006 FORMAT (/' ***** REPORTED NEXT =',2I5,'   RATHER THAN    4    6')
 4009 FORMAT (7X,'REPORTED NEXT =',2I3,'   RATHER THAN ',2I3)
 5006 FORMAT (/' DCHFDV RESULTS OK.')
 5009 FORMAT (/' DCHFEV AGREES WITH DCHFDV.')
C------------- LAST LINE OF DEVCHK FOLLOWS -----------------------------
      END
      SUBROUTINE DEVERK (LOUT, KPRINT, FAIL)

c*********************************************************************72
c
cc DEVERK tests error returns from DPCHIP evaluators for DPCHQ1.
c
C***BEGIN PROLOGUE  DEVERK
C***SUBSIDIARY
C***PURPOSE  Test error returns from DPCHIP evaluators for DPCHQ1.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (EVERCK-S, DEVERK-D)
C***KEYWORDS  PCHIP EVALUATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C --------- CODE TO TEST ERROR RETURNS FROM DPCHIP EVALUATORS. ---------
C
C
C     FORTRAN LIBRARY ROUTINES USED:  (WRITE).
C     SLATEC LIBRARY ROUTINES USED:  DCHFDV, DCHFEV, DPCHFD, DPCHFE,
C                                    XERDMP, XGETF, XSETF.
C     OTHER ROUTINES USED:  COMP.
C
C***ROUTINES CALLED  COMP, DCHFDV, DCHFEV, DPCHFD, DPCHFE, XERDMP,
C                    XGETF, XSETF
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   820715  CONVERTED TO QUICK CHECK FOR SLATEC LIBRARY.
C   890207  ADDED CALLS TO ERROR HANDLER.
C   890316  Added call to XERDMP if KPRINT.GT.2 (FNF).
C   890706  Cosmetic changes to prologue.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891009  Removed unreferenced statement label.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900309  Added COMP to list of routines called.  (FNF)
C   900315  Revised prologue and improved some output formats.  (FNF)
C   900316  Deleted INCFD tests because some compilers object to them,
C           and made additional minor cosmetic changes.  (FNF)
C   900322  Made miscellaneous cosmetic changes.  (FNF)
C   910708  Minor modifications in use of KPRINT.  (WRB)
C   930504  Removed parens from constants in WRITE statements.  (FNF)
C***END PROLOGUE  DEVERK
C
C  Declare arguments.
C
      INTEGER  LOUT, KPRINT
      LOGICAL  FAIL
C
C  DECLARATIONS.
C
      INTEGER  I, IERR, KONTRL, N, NERR, NEXT(2)
      DOUBLE PRECISION D(10), DUM, F(10), TEMP, X(10)
      LOGICAL  COMP, SKIP
C
C  INITIALIZE.
C
      PARAMETER (N = 10)
C***FIRST EXECUTABLE STATEMENT  DEVERK
      NERR = 0
C
      CALL XGETF (KONTRL)
      IF (KPRINT .LE. 2) THEN
         CALL XSETF (0)
      ELSE
         CALL XSETF (1)
      ENDIF
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 2000)
      IF (KPRINT .GE. 2)  WRITE (LOUT, 5000)
C
C  FIRST, TEST DCHFEV AND DCHFDV.
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -1
      CALL DCHFEV (0.D0, 1.D0, 3.D0, 7.D0, 3.D0, 6.D0, 0, DUM, DUM,
     * NEXT, IERR)
      IF (.NOT. COMP (IERR, -1, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -2
      CALL DCHFEV (1.D0, 1.D0, 3.D0, 7.D0, 3.D0, 6.D0, 1, DUM, DUM,
     * NEXT, IERR)
      IF (.NOT. COMP (IERR, -2, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -1
      CALL DCHFDV (0.D0, 1.D0, 3.D0, 7.D0, 3.D0, 6.D0, 0, DUM, DUM,
     * DUM, NEXT, IERR)
      IF (.NOT. COMP (IERR, -1, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -2
      CALL DCHFDV (1.D0, 1.D0, 3.D0, 7.D0, 3.D0, 6.D0, 1, DUM, DUM,
     * DUM, NEXT, IERR)
      IF (.NOT. COMP (IERR, -2, LOUT, KPRINT) )  NERR = NERR + 1
C
C  SET UP PCH DEFINITION.
C
      DO 10  I = 1, N
         X(I) = I
         F(I) = I + 2
         D(I) = 1.D0
   10 CONTINUE
C
C  SWAP POINTS 4 AND 7, SO X-ARRAY IS OUT OF ORDER.
C
      TEMP = X(4)
      X(4) = X(7)
      X(7) = TEMP
C
C  NOW, TEST DPCHFE AND DPCHFD.
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -1
      SKIP = .FALSE.
      CALL DPCHFE (1, X, F, D, 1, SKIP, 0, DUM, DUM, IERR)
      IF (.NOT. COMP (IERR, -1, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -3
      SKIP = .FALSE.
      CALL DPCHFE (N, X, F, D, 1, SKIP, 0, DUM, DUM, IERR)
      IF (.NOT. COMP (IERR, -3, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -4
      SKIP = .TRUE.
      CALL DPCHFE (N, X, F, D, 1, SKIP, 0, DUM, DUM, IERR)
      IF (.NOT. COMP (IERR, -4, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -1
      SKIP = .FALSE.
      CALL DPCHFD (1, X, F, D, 1, SKIP, 0, DUM, DUM, DUM, IERR)
      IF (.NOT. COMP (IERR, -1, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -3
      SKIP = .FALSE.
      CALL DPCHFD (N, X, F, D, 1, SKIP, 0, DUM, DUM, DUM, IERR)
      IF (.NOT. COMP (IERR, -3, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -4
      SKIP = .TRUE.
      CALL DPCHFD (N, X, F, D, 1, SKIP, 0, DUM, DUM, DUM, IERR)
      IF (.NOT. COMP (IERR, -4, LOUT, KPRINT) )  NERR = NERR + 1
C
C  SUMMARIZE RESULTS.
C
      IF (KPRINT .GT. 2)  CALL XERDMP
      IF (NERR .EQ. 0)  THEN
         FAIL = .FALSE.
         IF (KPRINT .GE. 2)  WRITE (LOUT, 5002)
      ELSE
         FAIL = .TRUE.
         IF (KPRINT .GE. 2)  WRITE (LOUT, 5003)  NERR
      ENDIF
C
C  TERMINATE.
C
      CALL XSETF (KONTRL)
      RETURN
C
C  FORMATS.
C
 2000 FORMAT ('1'//10X,'TEST ERROR RETURNS')
 5000 FORMAT (//10X,'DEVERK RESULTS'/10X,'--------------')
 5001 FORMAT (/' THIS CALL SHOULD RETURN IERR =',I3)
 5002 FORMAT (/' ALL ERROR RETURNS OK.')
 5003 FORMAT (//' ***** TROUBLE IN DEVERK *****'
     *        //5X,I5,' TESTS FAILED TO GIVE EXPECTED RESULTS.')
C------------- LAST LINE OF DEVERK FOLLOWS -----------------------------
      END
      SUBROUTINE DEVPCK (LOUT, KPRINT, X, Y, F, FX, FY, XE, YE, FE, DE,
     +   FE2, FAIL)

c*********************************************************************72
c
cc DEVPCK tests usage of increment argument in DPCHFD and DPCHFE for DPCHQ1.
c
C***BEGIN PROLOGUE  DEVPCK
C***SUBSIDIARY
C***PURPOSE  Test usage of increment argument in DPCHFD and DPCHFE for
C            DPCHQ1.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (EVPCCK-S, DEVPCK-D)
C***KEYWORDS  PCHIP EVALUATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C ---- CODE TO TEST USAGE OF INCREMENT ARGUMENT IN DPCHFD AND DPCHFE ---
C
C     EVALUATES A BICUBIC FUNCTION AND ITS FIRST PARTIAL DERIVATIVES
C     ON A 4X6 MESH CONTAINED IN A 10X10 ARRAY.
C
C     INTERPOLATION OF THESE DATA ALONG MESH LINES IN EITHER DIMENSION
C     SHOULD AGREE WITH CORRECT FUNCTION WITHIN ROUNDOFF ERROR.
C
C     ARRAYS ARE ARGUMENTS ONLY TO ALLOW SHARING STORAGE WITH OTHER
C     TEST ROUTINES.
C
C     NOTE:  RUN WITH KPRINT=4 FOR FULL GORY DETAILS (10 PAGES WORTH).
C
C
C     FORTRAN INTRINSICS USED:  ABS.
C     FORTRAN LIBRARY ROUTINES USED:  (WRITE).
C     SLATEC LIBRARY ROUTINES USED:  DPCHFD, DPCHFE, D1MACH.
C
C***ROUTINES CALLED  D1MACH, DPCHFD, DPCHFE
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   820714  CONVERTED TO QUICK CHECK FOR SLATEC LIBRARY.
C   820715  1. CORRECTED SOME FORMATS.
C           2. ADDED CALL TO D1MACH TO SET MACHEP.
C   890406  1. Modified to make sure final elements of X and XE
C             agree, to avoid possible failure due to roundoff
C             error.
C           2. Added printout of TOL in case of failure.
C           3. Removed unnecessary IMPLICIT declaration.
C           4. Corrected a few S.P. constants to D.P.
C           5. Minor cosmetic changes.
C   890706  Cosmetic changes to prologue.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891004  Cosmetic changes to prologue.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  Revised prologue and improved some output formats.  (FNF)
C   900316  Additional minor cosmetic changes.  (FNF)
C   900321  Made miscellaneous cosmetic changes.  (FNF)
C   901130  Made many changes to output:  (FNF)
C           1. Reduced amount of output for KPRINT=3.  (Now need to
C              use KPRINT=4 for full output.)
C           2. Added 1P's to formats and revised some to reduce maximum
C              line length.
C   910708  Minor modifications in use of KPRINT.  (WRB)
C   930317  Improved output formats.  (FNF)
C***END PROLOGUE  DEVPCK
C
C  Declare arguments.
C
      INTEGER  LOUT, KPRINT
      LOGICAL  FAIL
      DOUBLE PRECISION
     *      X(10), Y(10), F(10,10), FX(10,10), FY(10,10),
     *      XE(51), YE(51), FE(51), DE(51), FE2(51)
C
C  DECLARATIONS.
C
      INTEGER  I, IER2, IERR, INC, J, K, NE, NERR, NMAX, NX, NY
      LOGICAL  FAILD, FAILE, FAILOC, SKIP
      DOUBLE PRECISION
     *      DERMAX, DERR, DTRUE, DX, FDIFF, FDIFMX, FERMAX, FERR,
     *      FTRUE, MACHEP, TOL, PDERMX, PDIFMX, PFERMX, ZERO
      DOUBLE PRECISION  D1MACH
C
C  DEFINE TEST FUNCTION AND DERIVATIVES.
C
      DOUBLE PRECISION  AX, AY, FCN, DFDX, DFDY
      FCN (AX,AY) =  AX*(AY*AY)*(AX*AX + 1.D0)
      DFDX(AX,AY) = (AY*AY)*(3.D0*AX*AX + 1.D0)
      DFDY(AX,AY) =   2.D0*AX*AY*(AX*AX + 1.D0)
C
      DATA  NMAX /10/,  NX /4/,  NY /6/
      DATA  NE /51/
      DATA  ZERO /0.D0/
C
C  INITIALIZE.
C
C***FIRST EXECUTABLE STATEMENT  DEVPCK
      MACHEP = D1MACH(4)
C       Following tolerance is looser than S.P. version to avoid
C       spurious failures on some systems.
      TOL = 25.D0*MACHEP
C
      FAIL = .FALSE.
C
C  SET UP 4-BY-6 MESH IN A 10-BY-10 ARRAY:
C     X =  0.25(0.25)1.   ;
C     Y = -0.75(0.5 )1.75 .
C
      DO 1  I = 1, NX-1
         X(I) = 0.25D0*I
    1 CONTINUE
      X(NX) = 1.D0
      DO 5  J = 1, NY
         Y(J) = 0.5D0*J - 1.25D0
         DO 4  I = 1, NX
             F(I,J) = FCN (X(I), Y(J))
            FX(I,J) = DFDX(X(I), Y(J))
            FY(I,J) = DFDY(X(I), Y(J))
    4    CONTINUE
    5 CONTINUE
C
C  SET UP EVALUATION POINTS:
C     XE =  0.(0.02)1. ;
C     YE = -2.(0.08)2. .
C
      DX = 1.D0/(NE-1)
      DO 8  K = 1, NE-1
         XE(K) = DX*(K-1)
         YE(K) = 4.D0*XE(K) - 2.D0
    8 CONTINUE
      XE(NE) = 1.D0
      YE(NE) = 2.D0
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 1000)
      IF (KPRINT .GE. 2)  WRITE (LOUT, 1001)
C
C  EVALUATE ON HORIZONTAL MESH LINES (Y FIXED, X RUNNING) ..............
C
      NERR = 0
      INC = 1
      SKIP = .FALSE.
      DO 20  J = 1, NY
C        --------------------------------------------------------------
         CALL DPCHFD (NX, X, F(1,J), FX(1,J), INC, SKIP, NE, XE, FE, DE,
     *               IERR)
C        --------------------------------------------------------------
         IF (KPRINT .GE. 3)
     *       WRITE (LOUT, 2000)  INC, 'J', J, 'Y', Y(J), IERR
         IF (IERR .LT. 0)  GO TO 15
         IF (KPRINT .GT. 3)  WRITE (LOUT, 2001)  'X'
C
C        DPCHFE SHOULD AGREE EXACTLY WITH DPCHFD.
C
C        -----------------------------------------------------------
         CALL DPCHFE (NX, X, F(1,J), FX(1,J), INC, SKIP, NE, XE, FE2,
     *               IER2)
C        -----------------------------------------------------------
C
         DO 10  K = 1, NE
            FTRUE =  FCN(XE(K), Y(J))
            FERR = FE(K) - FTRUE
            DTRUE = DFDX(XE(K), Y(J))
            DERR = DE(K) - DTRUE
            IF (KPRINT .GT. 3)
     *         WRITE (LOUT, 2002)  XE(K), FTRUE, FE(K), FERR,
     *                                    DTRUE, DE(K), DERR
            IF (K .EQ. 1)  THEN
C              INITIALIZE.
               FERMAX = ABS(FERR)
               PFERMX = XE(1)
               DERMAX = ABS(DERR)
               PDERMX = XE(1)
               FDIFMX = ABS(FE2(1) - FE(1))
               PDIFMX = XE(1)
            ELSE
C              SELECT.
               FERR = ABS(FERR)
               IF (FERR .GT. FERMAX)  THEN
                  FERMAX = FERR
                  PFERMX = XE(K)
               ENDIF
               DERR = ABS(DERR)
               IF (DERR .GT. DERMAX)  THEN
                  DERMAX = DERR
                  PDERMX = XE(K)
               ENDIF
               FDIFF = ABS(FE2(K) - FE(K))
               IF (FDIFF .GT. FDIFMX)  THEN
                  FDIFMX = FDIFF
                  PDIFMX = XE(K)
               ENDIF
            ENDIF
   10    CONTINUE
C
         FAILD = (FERMAX.GT.TOL) .OR. (DERMAX.GT.TOL)
         FAILE = FDIFMX .NE. ZERO
         FAILOC = FAILD .OR. FAILE .OR. (IERR.NE.13) .OR. (IER2.NE.IERR)
C
         IF (FAILOC .AND. (KPRINT.GE.2))
     *      WRITE (LOUT, 2003)  'J', J, 'Y', Y(J)
C
         IF ((KPRINT.GE.3) .OR. (FAILD.AND.(KPRINT.EQ.2)) )
     *      WRITE (LOUT, 2004)  FERMAX, PFERMX, DERMAX, PDERMX
         IF (FAILD .AND. (KPRINT.GE.2))  WRITE (LOUT, 2014)  TOL
C
         IF ((KPRINT.GE.3) .OR. (FAILE.AND.(KPRINT.EQ.2)) )
     *      WRITE (LOUT, 2005)  FDIFMX, PDIFMX
C
         IF ((IERR.NE.13) .AND. (KPRINT.GE.2))
     *      WRITE (LOUT, 2006)  'D', IERR, 13
C
         IF ((IER2.NE.IERR) .AND. (KPRINT.GE.2))
     *      WRITE (LOUT, 2006)  'E', IER2, IERR
         GO TO 19
C
   15    CONTINUE
         FAILOC = .TRUE.
         IF (KPRINT .GE. 2)  WRITE (LOUT, 3000) IERR
C
   19    CONTINUE
         IF (FAILOC)  NERR = NERR + 1
         FAIL = FAIL .OR. FAILOC
   20 CONTINUE
C
      IF (KPRINT .GE. 2)  THEN
         IF (NERR .GT. 0)  THEN
            WRITE (LOUT, 3001)  NERR, 'J'
         ELSE
            WRITE (LOUT, 4000)  'J'
         ENDIF
      ENDIF
C
C  EVALUATE ON VERTICAL MESH LINES (X FIXED, Y RUNNING) ................
C
      NERR = 0
      INC = NMAX
      SKIP = .FALSE.
      DO 40  I = 1, NX
C        --------------------------------------------------------------
         CALL DPCHFD (NY, Y, F(I,1), FY(I,1), INC, SKIP, NE, YE, FE, DE,
     *               IERR)
C        --------------------------------------------------------------
         IF (KPRINT .GE. 3)
     *       WRITE (LOUT, 2000)  INC, 'I', I, 'X', X(I), IERR
         IF (IERR .LT. 0)  GO TO 35
         IF (KPRINT .GT. 3)  WRITE (LOUT, 2001)  'Y'
C
C        DPCHFE SHOULD AGREE EXACTLY WITH DPCHFD.
C
C        -----------------------------------------------------------
         CALL DPCHFE (NY, Y, F(I,1), FY(I,1), INC, SKIP, NE, YE, FE2,
     *               IER2)
C        -----------------------------------------------------------
C
         DO 30  K = 1, NE
            FTRUE =  FCN(X(I), YE(K))
            FERR = FE(K) - FTRUE
            DTRUE = DFDY(X(I), YE(K))
            DERR = DE(K) - DTRUE
            IF (KPRINT .GT. 3)
     *         WRITE (LOUT, 2002)  YE(K), FTRUE, FE(K), FERR,
     *                                    DTRUE, DE(K), DERR
            IF (K .EQ. 1)  THEN
C              INITIALIZE.
               FERMAX = ABS(FERR)
               PFERMX = YE(1)
               DERMAX = ABS(DERR)
               PDERMX = YE(1)
               FDIFMX = ABS(FE2(1) - FE(1))
               PDIFMX = YE(1)
            ELSE
C              SELECT.
               FERR = ABS(FERR)
               IF (FERR .GT. FERMAX)  THEN
                  FERMAX = FERR
                  PFERMX = YE(K)
               ENDIF
               DERR = ABS(DERR)
               IF (DERR .GT. DERMAX)  THEN
                  DERMAX = DERR
                  PDERMX = YE(K)
               ENDIF
               FDIFF = ABS(FE2(K) - FE(K))
               IF (FDIFF .GT. FDIFMX)  THEN
                  FDIFMX = FDIFF
                  PDIFMX = YE(K)
               ENDIF
            ENDIF
   30    CONTINUE
C
         FAILD = (FERMAX.GT.TOL) .OR. (DERMAX.GT.TOL)
         FAILE = FDIFMX .NE. ZERO
         FAILOC = FAILD .OR. FAILE .OR. (IERR.NE.20) .OR. (IER2.NE.IERR)
C
         IF (FAILOC .AND. (KPRINT.GE.2))
     *      WRITE (LOUT, 2003)  'I', I, 'X', X(I)
C
         IF ((KPRINT.GE.3) .OR. (FAILD.AND.(KPRINT.EQ.2)) )
     *      WRITE (LOUT, 2004)  FERMAX, PFERMX, DERMAX, PDERMX
         IF (FAILD .AND. (KPRINT.GE.2))  WRITE (LOUT, 2014)  TOL
C
         IF ((KPRINT.GE.3) .OR. (FAILE.AND.(KPRINT.EQ.2)) )
     *      WRITE (LOUT, 2005)  FDIFMX, PDIFMX
C
         IF ((IERR.NE.20) .AND. (KPRINT.GE.2))
     *      WRITE (LOUT, 2006)  'D', IERR, 20
C
         IF ((IER2.NE.IERR) .AND. (KPRINT.GE.2))
     *      WRITE (LOUT, 2006)  'E', IER2, IERR
         GO TO 39
C
   35    CONTINUE
         FAILOC = .TRUE.
         IF (KPRINT .GE. 2)  WRITE (LOUT, 3000) IERR
C
   39    CONTINUE
         IF (FAILOC)  NERR = NERR + 1
         FAIL = FAIL .OR. FAILOC
   40 CONTINUE
C
      IF (KPRINT .GE. 2)  THEN
         IF (NERR .GT. 0)  THEN
            WRITE (LOUT, 3001)  NERR, 'I'
         ELSE
            WRITE (LOUT, 4000)  'I'
         ENDIF
      ENDIF
C
C  TERMINATE.
C
      RETURN
C
C  FORMATS.
C
 1000 FORMAT ('1'//10X,'TEST DPCHFE AND DPCHFD')
 1001 FORMAT (//10X,'DEVPCK RESULTS'/10X,'--------------')
 2000 FORMAT (//20X,'DPCHFD INCREMENT TEST -- INCFD = ',I2
     *        /15X,'ON ',A1,'-LINE ',I2,',  ',A1,' =',F8.4,
     *           '  --  IERR =',I3)
 2001 FORMAT ( /3X,A1,'E',10X,'F',8X,'FE',9X,'DIFF',
     *                    13X,'D',8X,'DE',9X,'DIFF')
 2002 FORMAT (F7.2,2(2X,2F10.5,1P,D15.5,0P))
 2003 FORMAT (/' ***** DPCHFD AND/OR DPCHFE FAILED ON ',A1,'-LINE ',I1,
     *                             ',  ',A1,' =',F8.4)
 2004 FORMAT (/19X,'  MAXIMUM ERROR IN FUNCTION =',1P,
     *                                   1P,D13.5,0P,' (AT',F6.2,'),'
     *        /33X,    'IN DERIVATIVE =',1P,D13.5,0P,' (AT',F6.2,').' )
 2005 FORMAT ( '  MAXIMUM DIFFERENCE BETWEEN DPCHFE AND DPCHFD =',
     *                                   1P,D13.5,0P,' (AT',F6.2,').' )
 2006 FORMAT (/'  DPCHF',A1,' RETURNED IERR = ',I2,' INSTEAD OF ',I2)
 2014 FORMAT ('  *** BOTH SHOULD BE .LE. TOL =',1P,D12.5,' ***')
 3000 FORMAT (//' ***** ERROR ***** DPCHFD RETURNED IERR =',I5//)
 3001 FORMAT (//' ***** ERROR ***** DPCHFD AND/OR DPCHFE FAILED ON',I2,
     *                                1X, A1,'-LINES.'//)
 4000 FORMAT (/' DPCHFD AND DPCHFE OK ON ',A1,'-LINES.')
C------------- LAST LINE OF DEVPCK FOLLOWS -----------------------------
      END
      SUBROUTINE DFDTRU (X, F, D)

c*********************************************************************72
c
cc DFDTRU computes exact function values for DEVCHK.
c
C***BEGIN PROLOGUE  DFDTRU
C***SUBSIDIARY
C***PURPOSE  Compute exact function values for DEVCHK.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (FDTRUE-S, DFDTRU-D)
C***KEYWORDS  PCHIP EVALUATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C        COMPUTE EXACT FUNCTION VALUES IN DOUBLE PRECISION.
C
C                   F(X) = X*(X+1)*(X-2)
C
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   890618  REVISION DATE from Version 3.2
C   890706  Cosmetic changes to prologue.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  Revised prologue.  (FNF)
C   900316  Deleted variables ONE and TWO.  (FNF)
C   900321  Changed name of d.p. version from DFTRUE to DFDTRU.
C***END PROLOGUE  DFDTRU
      DOUBLE PRECISION  X, F, D
      DOUBLE PRECISION  FACT1, FACT2, XX
C
C***FIRST EXECUTABLE STATEMENT  DFDTRU
      XX = X
      FACT1 = XX + 1
      FACT2 = XX - 2
      F = XX * FACT1 * FACT2
      D = FACT1*FACT2 + XX*(FACT1 + FACT2)
C
      RETURN
C------------- LAST LINE OF DFDTRU FOLLOWS -----------------------------
      END
      SUBROUTINE DINTRV (XT, LXT, X, ILO, ILEFT, MFLAG)

c*********************************************************************72
c
cc DINTRV determines the interval containing, or nearest, a point.
c
C***BEGIN PROLOGUE  DINTRV
C***PURPOSE  Compute the largest integer ILEFT in 1 .LE. ILEFT .LE. LXT
C            such that XT(ILEFT) .LE. X where XT(*) is a subdivision of
C            the X interval.
C***LIBRARY   SLATEC
C***CATEGORY  E3, K6
C***TYPE      DOUBLE PRECISION (INTRV-S, DINTRV-D)
C***KEYWORDS  B-SPLINE, DATA FITTING, INTERPOLATION, SPLINES
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C     Written by Carl de Boor and modified by D. E. Amos
C
C     Abstract    **** a double precision routine ****
C         DINTRV is the INTERV routine of the reference.
C
C         DINTRV computes the largest integer ILEFT in 1 .LE. ILEFT .LE.
C         LXT such that XT(ILEFT) .LE. X where XT(*) is a subdivision of
C         the X interval.  Precisely,
C
C                      X .LT. XT(1)                1         -1
C         if  XT(I) .LE. X .LT. XT(I+1)  then  ILEFT=I  , MFLAG=0
C           XT(LXT) .LE. X                         LXT        1,
C
C         That is, when multiplicities are present in the break point
C         to the left of X, the largest index is taken for ILEFT.
C
C     Description of Arguments
C
C         Input      XT,X are double precision
C          XT      - XT is a knot or break point vector of length LXT
C          LXT     - length of the XT vector
C          X       - argument
C          ILO     - an initialization parameter which must be set
C                    to 1 the first time the spline array XT is
C                    processed by DINTRV.
C
C         Output
C          ILO     - ILO contains information for efficient process-
C                    ing after the initial call and ILO must not be
C                    changed by the user.  Distinct splines require
C                    distinct ILO parameters.
C          ILEFT   - largest integer satisfying XT(ILEFT) .LE. X
C          MFLAG   - signals when X lies out of bounds
C
C     Error Conditions
C         None
C
C***REFERENCES  Carl de Boor, Package for calculating with B-splines,
C                 SIAM Journal on Numerical Analysis 14, 3 (June 1977),
C                 pp. 441-472.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   800901  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DINTRV
C
      INTEGER IHI, ILEFT, ILO, ISTEP, LXT, MFLAG, MIDDLE
      DOUBLE PRECISION X, XT
      DIMENSION XT(*)
C***FIRST EXECUTABLE STATEMENT  DINTRV
      IHI = ILO + 1
      IF (IHI.LT.LXT) GO TO 10
      IF (X.GE.XT(LXT)) GO TO 110
      IF (LXT.LE.1) GO TO 90
      ILO = LXT - 1
      IHI = LXT
C
   10 IF (X.GE.XT(IHI)) GO TO 40
      IF (X.GE.XT(ILO)) GO TO 100
C
C *** NOW X .LT. XT(IHI) . FIND LOWER BOUND
      ISTEP = 1
   20 IHI = ILO
      ILO = IHI - ISTEP
      IF (ILO.LE.1) GO TO 30
      IF (X.GE.XT(ILO)) GO TO 70
      ISTEP = ISTEP*2
      GO TO 20
   30 ILO = 1
      IF (X.LT.XT(1)) GO TO 90
      GO TO 70
C *** NOW X .GE. XT(ILO) . FIND UPPER BOUND
   40 ISTEP = 1
   50 ILO = IHI
      IHI = ILO + ISTEP
      IF (IHI.GE.LXT) GO TO 60
      IF (X.LT.XT(IHI)) GO TO 70
      ISTEP = ISTEP*2
      GO TO 50
   60 IF (X.GE.XT(LXT)) GO TO 110
      IHI = LXT
C
C *** NOW XT(ILO) .LE. X .LT. XT(IHI) . NARROW THE INTERVAL
   70 MIDDLE = (ILO+IHI)/2
      IF (MIDDLE.EQ.ILO) GO TO 100
C     NOTE. IT IS ASSUMED THAT MIDDLE = ILO IN CASE IHI = ILO+1
      IF (X.LT.XT(MIDDLE)) GO TO 80
      ILO = MIDDLE
      GO TO 70
   80 IHI = MIDDLE
      GO TO 70
C *** SET OUTPUT AND RETURN
   90 MFLAG = -1
      ILEFT = 1
      RETURN
  100 MFLAG = 0
      ILEFT = ILO
      RETURN
  110 MFLAG = 1
      ILEFT = LXT
      RETURN
      END
      SUBROUTINE DPCHBS (N, X, F, D, INCFD, KNOTYP, NKNOTS, T, BCOEF,
     +   NDIM, KORD, IERR)

c*********************************************************************72
c
cc DPCHBS: Piecewise cubic Hermite to B-Spline converter.
c
C***BEGIN PROLOGUE  DPCHBS
C***PURPOSE  Piecewise Cubic Hermite to B-Spline converter.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3
C***TYPE      DOUBLE PRECISION (PCHBS-S, DPCHBS-D)
C***KEYWORDS  B-SPLINES, CONVERSION, CUBIC HERMITE INTERPOLATION,
C             PIECEWISE CUBIC INTERPOLATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Computing and Mathematics Research Division
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C *Usage:
C
C        INTEGER  N, INCFD, KNOTYP, NKNOTS, NDIM, KORD, IERR
C        PARAMETER  (INCFD = ...)
C        DOUBLE PRECISION  X(nmax), F(INCFD,nmax), D(INCFD,nmax),
C       *      T(2*nmax+4), BCOEF(2*nmax)
C
C        CALL DPCHBS (N, X, F, D, INCFD, KNOTYP, NKNOTS, T, BCOEF,
C       *             NDIM, KORD, IERR)
C
C *Arguments:
C
C     N:IN  is the number of data points, N.ge.2 .  (not checked)
C
C     X:IN  is the real array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.   (not checked)
C           nmax, the dimension of X, must be .ge.N.
C
C     F:IN  is the real array of dependent variable values.
C           F(1+(I-1)*INCFD) is the value corresponding to X(I).
C           nmax, the second dimension of F, must be .ge.N.
C
C     D:IN  is the real array of derivative values at the data points.
C           D(1+(I-1)*INCFD) is the value corresponding to X(I).
C           nmax, the second dimension of D, must be .ge.N.
C
C     INCFD:IN  is the increment between successive values in F and D.
C           This argument is provided primarily for 2-D applications.
C           It may have the value 1 for one-dimensional applications,
C           in which case F and D may be singly-subscripted arrays.
C
C     KNOTYP:IN  is a flag to control the knot sequence.
C           The knot sequence T is normally computed from X by putting
C           a double knot at each X and setting the end knot pairs
C           according to the value of KNOTYP:
C              KNOTYP = 0:  Quadruple knots at X(1) and X(N).  (default)
C              KNOTYP = 1:  Replicate lengths of extreme subintervals:
C                           T( 1 ) = T( 2 ) = X(1) - (X(2)-X(1))  ;
C                           T(M+4) = T(M+3) = X(N) + (X(N)-X(N-1)).
C              KNOTYP = 2:  Periodic placement of boundary knots:
C                           T( 1 ) = T( 2 ) = X(1) - (X(N)-X(N-1));
C                           T(M+4) = T(M+3) = X(N) + (X(2)-X(1))  .
C              Here M=NDIM=2*N.
C           If the input value of KNOTYP is negative, however, it is
C           assumed that NKNOTS and T were set in a previous call.
C           This option is provided for improved efficiency when used
C           in a parametric setting.
C
C     NKNOTS:INOUT  is the number of knots.
C           If KNOTYP.GE.0, then NKNOTS will be set to NDIM+4.
C           If KNOTYP.LT.0, then NKNOTS is an input variable, and an
C              error return will be taken if it is not equal to NDIM+4.
C
C     T:INOUT  is the array of 2*N+4 knots for the B-representation.
C           If KNOTYP.GE.0, T will be returned by DPCHBS with the
C              interior double knots equal to the X-values and the
C              boundary knots set as indicated above.
C           If KNOTYP.LT.0, it is assumed that T was set by a
C              previous call to DPCHBS.  (This routine does **not**
C              verify that T forms a legitimate knot sequence.)
C
C     BCOEF:OUT  is the array of 2*N B-spline coefficients.
C
C     NDIM:OUT  is the dimension of the B-spline space.  (Set to 2*N.)
C
C     KORD:OUT  is the order of the B-spline.  (Set to 4.)
C
C     IERR:OUT  is an error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           "Recoverable" errors:
C              IERR = -4  if KNOTYP.GT.2 .
C              IERR = -5  if KNOTYP.LT.0 and NKNOTS.NE.(2*N+4).
C
C *Description:
C     DPCHBS computes the B-spline representation of the PCH function
C     determined by N,X,F,D.  To be compatible with the rest of PCHIP,
C     DPCHBS includes INCFD, the increment between successive values of
C     the F- and D-arrays.
C
C     The output is the B-representation for the function:  NKNOTS, T,
C     BCOEF, NDIM, KORD.
C
C *Caution:
C     Since it is assumed that the input PCH function has been
C     computed by one of the other routines in the package PCHIP,
C     input arguments N, X, INCFD are **not** checked for validity.
C
C *Restrictions/assumptions:
C     1. N.GE.2 .  (not checked)
C     2. X(i).LT.X(i+1), i=1,...,N .  (not checked)
C     3. INCFD.GT.0 .  (not checked)
C     4. KNOTYP.LE.2 .  (error return if not)
C    *5. NKNOTS = NDIM+4 = 2*N+4 .  (error return if not)
C    *6. T(2*k+1) = T(2*k) = X(k), k=1,...,N .  (not checked)
C
C       * Indicates this applies only if KNOTYP.LT.0 .
C
C *Portability:
C     Argument INCFD is used only to cause the compiler to generate
C     efficient code for the subscript expressions (1+(I-1)*INCFD) .
C     The normal usage, in which DPCHBS is called with one-dimensional
C     arrays F and D, is probably non-Fortran 77, in the strict sense,
C     but it works on all systems on which DPCHBS has been tested.
C
C *See Also:
C     PCHIC, PCHIM, or PCHSP can be used to determine an interpolating
C        PCH function from a set of data.
C     The B-spline routine DBVALU can be used to evaluate the
C        B-representation that is output by DPCHBS.
C        (See BSPDOC for more information.)
C
C***REFERENCES  F. N. Fritsch, "Representations for parametric cubic
C                 splines," Computer Aided Geometric Design 6 (1989),
C                 pp.79-82.
C***ROUTINES CALLED  DPCHKT, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   870701  DATE WRITTEN
C   900405  Converted Fortran to upper case.
C   900405  Removed requirement that X be dimensioned N+1.
C   900406  Modified to make PCHKT a subsidiary routine to simplify
C           usage.  In the process, added argument INCFD to be com-
C           patible with the rest of PCHIP.
C   900410  Converted prologue to SLATEC 4.0 format.
C   900410  Added calls to XERMSG and changed constant 3. to 3 to
C           reduce single/double differences.
C   900411  Added reference.
C   900430  Produced double precision version.
C   900501  Corrected declarations.
C   930317  Minor cosmetic changes.  (FNF)
C   930514  Corrected problems with dimensioning of arguments and
C           clarified DESCRIPTION.  (FNF)
C   930604  Removed  NKNOTS from DPCHKT call list.  (FNF)
C***END PROLOGUE  DPCHBS
C
C*Internal Notes:
C
C**End
C
C  Declare arguments.
C
      INTEGER  N, INCFD, KNOTYP, NKNOTS, NDIM, KORD, IERR
      DOUBLE PRECISION  X(*), F(INCFD,*), D(INCFD,*), T(*), BCOEF(*)
C
C  Declare local variables.
C
      INTEGER  K, KK
      DOUBLE PRECISION  DOV3, HNEW, HOLD
      CHARACTER*8  LIBNAM, SUBNAM
C***FIRST EXECUTABLE STATEMENT  DPCHBS
C
C  Initialize.
C
      NDIM = 2*N
      KORD = 4
      IERR = 0
      LIBNAM = 'SLATEC'
      SUBNAM = 'DPCHBS'
C
C  Check argument validity.  Set up knot sequence if OK.
C
      IF ( KNOTYP.GT.2 )  THEN
         IERR = -1
         CALL XERMSG (LIBNAM, SUBNAM, 'KNOTYP GREATER THAN 2', IERR, 1)
         RETURN
      ENDIF
      IF ( KNOTYP.LT.0 )  THEN
         IF ( NKNOTS.NE.NDIM+4 )  THEN
            IERR = -2
            CALL XERMSG (LIBNAM, SUBNAM,
     *                    'KNOTYP.LT.0 AND NKNOTS.NE.(2*N+4)', IERR, 1)
            RETURN
         ENDIF
      ELSE
C          Set up knot sequence.
         NKNOTS = NDIM + 4
         CALL DPCHKT (N, X, KNOTYP, T)
      ENDIF
C
C  Compute B-spline coefficients.
C
      HNEW = T(3) - T(1)
      DO 40  K = 1, N
         KK = 2*K
         HOLD = HNEW
C          The following requires mixed mode arithmetic.
         DOV3 = D(1,K)/3
         BCOEF(KK-1) = F(1,K) - HOLD*DOV3
C          The following assumes T(2*K+1) = X(K).
         HNEW = T(KK+3) - T(KK+1)
         BCOEF(KK) = F(1,K) + HNEW*DOV3
   40 CONTINUE
C
C  Terminate.
C
      RETURN
      END
      SUBROUTINE DPCHCE (IC, VC, N, X, H, SLOPE, D, INCFD, IERR)

c*********************************************************************72
c
cc DPCHCE sets boundary conditions for DPCHIC.
c
C***BEGIN PROLOGUE  DPCHCE
C***SUBSIDIARY
C***PURPOSE  Set boundary conditions for DPCHIC
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (PCHCE-S, DPCHCE-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C          DPCHCE:  DPCHIC End Derivative Setter.
C
C    Called by DPCHIC to set end derivatives as requested by the user.
C    It must be called after interior derivative values have been set.
C                      -----
C
C    To facilitate two-dimensional applications, includes an increment
C    between successive values of the D-array.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  IC(2), N, IERR
C        DOUBLE PRECISION  VC(2), X(N), H(N), SLOPE(N), D(INCFD,N)
C
C        CALL  DPCHCE (IC, VC, N, X, H, SLOPE, D, INCFD, IERR)
C
C   Parameters:
C
C     IC -- (input) integer array of length 2 specifying desired
C           boundary conditions:
C           IC(1) = IBEG, desired condition at beginning of data.
C           IC(2) = IEND, desired condition at end of data.
C           ( see prologue to DPCHIC for details. )
C
C     VC -- (input) real*8 array of length 2 specifying desired boundary
C           values.  VC(1) need be set only if IC(1) = 2 or 3 .
C                    VC(2) need be set only if IC(2) = 2 or 3 .
C
C     N -- (input) number of data points.  (assumes N.GE.2)
C
C     X -- (input) real*8 array of independent variable values.  (the
C           elements of X are assumed to be strictly increasing.)
C
C     H -- (input) real*8 array of interval lengths.
C     SLOPE -- (input) real*8 array of data slopes.
C           If the data are (X(I),Y(I)), I=1(1)N, then these inputs are:
C                  H(I) =  X(I+1)-X(I),
C              SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1.
C
C     D -- (input) real*8 array of derivative values at the data points.
C           The value corresponding to X(I) must be stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C          (output) the value of D at X(1) and/or X(N) is changed, if
C           necessary, to produce the requested boundary conditions.
C           no other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in D.
C           This argument is provided primarily for 2-D applications.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning errors:
C              IERR = 1  if IBEG.LT.0 and D(1) had to be adjusted for
C                        monotonicity.
C              IERR = 2  if IEND.LT.0 and D(1+(N-1)*INCFD) had to be
C                        adjusted for monotonicity.
C              IERR = 3  if both of the above are true.
C
C    -------
C    WARNING:  This routine does no validity-checking of arguments.
C    -------
C
C  Fortran intrinsics used:  ABS.
C
C***SEE ALSO  DPCHIC
C***ROUTINES CALLED  DPCHDF, DPCHST, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820218  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   870707  Corrected XERROR calls for d.p. name(s).
C   890206  Corrected XERROR calls.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR section in prologue.  (WRB)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  DPCHCE
C
C  Programming notes:
C     1. The function DPCHST(ARG1,ARG2)  is assumed to return zero if
C        either argument is zero, +1 if they are of the same sign, and
C        -1 if they are of opposite sign.
C     2. One could reduce the number of arguments and amount of local
C        storage, at the expense of reduced code clarity, by passing in
C        the array WK (rather than splitting it into H and SLOPE) and
C        increasing its length enough to incorporate STEMP and XTEMP.
C     3. The two monotonicity checks only use the sufficient conditions.
C        Thus, it is possible (but unlikely) for a boundary condition to
C        be changed, even though the original interpolant was monotonic.
C        (At least the result is a continuous function of the data.)
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  IC(2), N, INCFD, IERR
      DOUBLE PRECISION  VC(2), X(*), H(*), SLOPE(*), D(INCFD,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  IBEG, IEND, IERF, INDEX, J, K
      DOUBLE PRECISION  HALF, STEMP(3), THREE, TWO, XTEMP(4), ZERO
      SAVE ZERO, HALF, TWO, THREE
      DOUBLE PRECISION  DPCHDF, DPCHST
C
C  INITIALIZE.
C
      DATA  ZERO /0.D0/,  HALF/.5D0/,  TWO/2.D0/, THREE/3.D0/
C
C***FIRST EXECUTABLE STATEMENT  DPCHCE
      IBEG = IC(1)
      IEND = IC(2)
      IERR = 0
C
C  SET TO DEFAULT BOUNDARY CONDITIONS IF N IS TOO SMALL.
C
      IF ( ABS(IBEG).GT.N )  IBEG = 0
      IF ( ABS(IEND).GT.N )  IEND = 0
C
C  TREAT BEGINNING BOUNDARY CONDITION.
C
      IF (IBEG .EQ. 0)  GO TO 2000
      K = ABS(IBEG)
      IF (K .EQ. 1)  THEN
C        BOUNDARY VALUE PROVIDED.
         D(1,1) = VC(1)
      ELSE IF (K .EQ. 2)  THEN
C        BOUNDARY SECOND DERIVATIVE PROVIDED.
         D(1,1) = HALF*( (THREE*SLOPE(1) - D(1,2)) - HALF*VC(1)*H(1) )
      ELSE IF (K .LT. 5)  THEN
C        USE K-POINT DERIVATIVE FORMULA.
C        PICK UP FIRST K POINTS, IN REVERSE ORDER.
         DO 10  J = 1, K
            INDEX = K-J+1
C           INDEX RUNS FROM K DOWN TO 1.
            XTEMP(J) = X(INDEX)
            IF (J .LT. K)  STEMP(J) = SLOPE(INDEX-1)
   10    CONTINUE
C                 -----------------------------
         D(1,1) = DPCHDF (K, XTEMP, STEMP, IERF)
C                 -----------------------------
         IF (IERF .NE. 0)  GO TO 5001
      ELSE
C        USE 'NOT A KNOT' CONDITION.
         D(1,1) = ( THREE*(H(1)*SLOPE(2) + H(2)*SLOPE(1))
     *             - TWO*(H(1)+H(2))*D(1,2) - H(1)*D(1,3) ) / H(2)
      ENDIF
C
      IF (IBEG .GT. 0)  GO TO 2000
C
C  CHECK D(1,1) FOR COMPATIBILITY WITH MONOTONICITY.
C
      IF (SLOPE(1) .EQ. ZERO)  THEN
         IF (D(1,1) .NE. ZERO)  THEN
            D(1,1) = ZERO
            IERR = IERR + 1
         ENDIF
      ELSE IF ( DPCHST(D(1,1),SLOPE(1)) .LT. ZERO)  THEN
         D(1,1) = ZERO
         IERR = IERR + 1
      ELSE IF ( ABS(D(1,1)) .GT. THREE*ABS(SLOPE(1)) )  THEN
         D(1,1) = THREE*SLOPE(1)
         IERR = IERR + 1
      ENDIF
C
C  TREAT END BOUNDARY CONDITION.
C
 2000 CONTINUE
      IF (IEND .EQ. 0)  GO TO 5000
      K = ABS(IEND)
      IF (K .EQ. 1)  THEN
C        BOUNDARY VALUE PROVIDED.
         D(1,N) = VC(2)
      ELSE IF (K .EQ. 2)  THEN
C        BOUNDARY SECOND DERIVATIVE PROVIDED.
         D(1,N) = HALF*( (THREE*SLOPE(N-1) - D(1,N-1)) +
     *                                           HALF*VC(2)*H(N-1) )
      ELSE IF (K .LT. 5)  THEN
C        USE K-POINT DERIVATIVE FORMULA.
C        PICK UP LAST K POINTS.
         DO 2010  J = 1, K
            INDEX = N-K+J
C           INDEX RUNS FROM N+1-K UP TO N.
            XTEMP(J) = X(INDEX)
            IF (J .LT. K)  STEMP(J) = SLOPE(INDEX)
 2010    CONTINUE
C                 -----------------------------
         D(1,N) = DPCHDF (K, XTEMP, STEMP, IERF)
C                 -----------------------------
         IF (IERF .NE. 0)  GO TO 5001
      ELSE
C        USE 'NOT A KNOT' CONDITION.
         D(1,N) = ( THREE*(H(N-1)*SLOPE(N-2) + H(N-2)*SLOPE(N-1))
     *             - TWO*(H(N-1)+H(N-2))*D(1,N-1) - H(N-1)*D(1,N-2) )
     *                                                         / H(N-2)
      ENDIF
C
      IF (IEND .GT. 0)  GO TO 5000
C
C  CHECK D(1,N) FOR COMPATIBILITY WITH MONOTONICITY.
C
      IF (SLOPE(N-1) .EQ. ZERO)  THEN
         IF (D(1,N) .NE. ZERO)  THEN
            D(1,N) = ZERO
            IERR = IERR + 2
         ENDIF
      ELSE IF ( DPCHST(D(1,N),SLOPE(N-1)) .LT. ZERO)  THEN
         D(1,N) = ZERO
         IERR = IERR + 2
      ELSE IF ( ABS(D(1,N)) .GT. THREE*ABS(SLOPE(N-1)) )  THEN
         D(1,N) = THREE*SLOPE(N-1)
         IERR = IERR + 2
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURN.
C
 5001 CONTINUE
C     ERROR RETURN FROM DPCHDF.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHCE', 'ERROR RETURN FROM DPCHDF',
     +   IERR, 1)
      RETURN
      END
      SUBROUTINE DPCHCI (N, H, SLOPE, D, INCFD)

c*********************************************************************72
c
cc DPCHCI sets interior derivatives for DPCHIC.
c
C***BEGIN PROLOGUE  DPCHCI
C***SUBSIDIARY
C***PURPOSE  Set interior derivatives for DPCHIC
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (PCHCI-S, DPCHCI-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C          DPCHCI:  DPCHIC Initial Derivative Setter.
C
C    Called by DPCHIC to set derivatives needed to determine a monotone
C    piecewise cubic Hermite interpolant to the data.
C
C    Default boundary conditions are provided which are compatible
C    with monotonicity.  If the data are only piecewise monotonic, the
C    interpolant will have an extremum at each point where monotonicity
C    switches direction.
C
C    To facilitate two-dimensional applications, includes an increment
C    between successive values of the D-array.
C
C    The resulting piecewise cubic Hermite function should be identical
C    (within roundoff error) to that produced by DPCHIM.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N
C        DOUBLE PRECISION  H(N), SLOPE(N), D(INCFD,N)
C
C        CALL  DPCHCI (N, H, SLOPE, D, INCFD)
C
C   Parameters:
C
C     N -- (input) number of data points.
C           If N=2, simply does linear interpolation.
C
C     H -- (input) real*8 array of interval lengths.
C     SLOPE -- (input) real*8 array of data slopes.
C           If the data are (X(I),Y(I)), I=1(1)N, then these inputs are:
C                  H(I) =  X(I+1)-X(I),
C              SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1.
C
C     D -- (output) real*8 array of derivative values at data points.
C           If the data are monotonic, these values will determine a
C           a monotone cubic Hermite function.
C           The value corresponding to X(I) is stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C           No other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in D.
C           This argument is provided primarily for 2-D applications.
C
C    -------
C    WARNING:  This routine does no validity-checking of arguments.
C    -------
C
C  Fortran intrinsics used:  ABS, MAX, MIN.
C
C***SEE ALSO  DPCHIC
C***ROUTINES CALLED  DPCHST
C***REVISION HISTORY  (YYMMDD)
C   820218  DATE WRITTEN
C   820601  Modified end conditions to be continuous functions of
C           data when monotonicity switches in next interval.
C   820602  1. Modified formulas so end conditions are less prone
C             to over/underflow problems.
C           2. Minor modification to HSUM calculation.
C   820805  Converted to SLATEC library version.
C   870813  Minor cosmetic changes.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR section in prologue.  (WRB)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  DPCHCI
C
C  Programming notes:
C     1. The function  DPCHST(ARG1,ARG2)  is assumed to return zero if
C        either argument is zero, +1 if they are of the same sign, and
C        -1 if they are of opposite sign.
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD
      DOUBLE PRECISION  H(*), SLOPE(*), D(INCFD,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, NLESS1
      DOUBLE PRECISION  DEL1, DEL2, DMAX, DMIN, DRAT1, DRAT2, HSUM,
     *      HSUMT3, THREE, W1, W2, ZERO
      SAVE ZERO, THREE
      DOUBLE PRECISION  DPCHST
C
C  INITIALIZE.
C
      DATA  ZERO /0.D0/, THREE/3.D0/
C***FIRST EXECUTABLE STATEMENT  DPCHCI
      NLESS1 = N - 1
      DEL1 = SLOPE(1)
C
C  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION.
C
      IF (NLESS1 .GT. 1)  GO TO 10
      D(1,1) = DEL1
      D(1,N) = DEL1
      GO TO 5000
C
C  NORMAL CASE  (N .GE. 3).
C
   10 CONTINUE
      DEL2 = SLOPE(2)
C
C  SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE
C     SHAPE-PRESERVING.
C
      HSUM = H(1) + H(2)
      W1 = (H(1) + HSUM)/HSUM
      W2 = -H(1)/HSUM
      D(1,1) = W1*DEL1 + W2*DEL2
      IF ( DPCHST(D(1,1),DEL1) .LE. ZERO)  THEN
         D(1,1) = ZERO
      ELSE IF ( DPCHST(DEL1,DEL2) .LT. ZERO)  THEN
C        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES.
         DMAX = THREE*DEL1
         IF (ABS(D(1,1)) .GT. ABS(DMAX))  D(1,1) = DMAX
      ENDIF
C
C  LOOP THROUGH INTERIOR POINTS.
C
      DO 50  I = 2, NLESS1
         IF (I .EQ. 2)  GO TO 40
C
         HSUM = H(I-1) + H(I)
         DEL1 = DEL2
         DEL2 = SLOPE(I)
   40    CONTINUE
C
C        SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC.
C
         D(1,I) = ZERO
         IF ( DPCHST(DEL1,DEL2) .LE. ZERO)  GO TO 50
C
C        USE BRODLIE MODIFICATION OF BUTLAND FORMULA.
C
         HSUMT3 = HSUM+HSUM+HSUM
         W1 = (HSUM + H(I-1))/HSUMT3
         W2 = (HSUM + H(I)  )/HSUMT3
         DMAX = MAX( ABS(DEL1), ABS(DEL2) )
         DMIN = MIN( ABS(DEL1), ABS(DEL2) )
         DRAT1 = DEL1/DMAX
         DRAT2 = DEL2/DMAX
         D(1,I) = DMIN/(W1*DRAT1 + W2*DRAT2)
C
   50 CONTINUE
C
C  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE
C     SHAPE-PRESERVING.
C
      W1 = -H(N-1)/HSUM
      W2 = (H(N-1) + HSUM)/HSUM
      D(1,N) = W1*DEL1 + W2*DEL2
      IF ( DPCHST(D(1,N),DEL2) .LE. ZERO)  THEN
         D(1,N) = ZERO
      ELSE IF ( DPCHST(DEL1,DEL2) .LT. ZERO)  THEN
C        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES.
         DMAX = THREE*DEL2
         IF (ABS(D(1,N)) .GT. ABS(DMAX))  D(1,N) = DMAX
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
      END
      SUBROUTINE DPCHCM (N, X, F, D, INCFD, SKIP, ISMON, IERR)

c*********************************************************************72
c
cc DPCHCM checks a cubic Hermite function for monotonicity.
c
C***BEGIN PROLOGUE  DPCHCM
C***PURPOSE  Check a cubic Hermite function for monotonicity.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3
C***TYPE      DOUBLE PRECISION (PCHCM-S, DPCHCM-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION,
C             PCHIP, PIECEWISE CUBIC INTERPOLATION, UTILITY ROUTINE
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Computing & Mathematics Research Division
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C *Usage:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, ISMON(N), IERR
C        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N)
C        LOGICAL  SKIP
C
C        CALL  DPCHCM (N, X, F, D, INCFD, SKIP, ISMON, IERR)
C
C *Arguments:
C
C     N:IN  is the number of data points.  (Error return if N.LT.2 .)
C
C     X:IN  is a real*8 array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F:IN  is a real*8 array of function values.  F(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     D:IN  is a real*8 array of derivative values.  D(1+(I-1)*INCFD) is
C           is the value corresponding to X(I).
C
C     INCFD:IN  is the increment between successive values in F and D.
C           (Error return if  INCFD.LT.1 .)
C
C     SKIP:INOUT  is a logical variable which should be set to
C           .TRUE. if the user wishes to skip checks for validity of
C           preceding parameters, or to .FALSE. otherwise.
C           This will save time in case these checks have already
C           been performed.
C           SKIP will be set to .TRUE. on normal return.
C
C     ISMON:OUT  is an integer array indicating on which intervals the
C           PCH function defined by  N, X, F, D  is monotonic.
C           For data interval [X(I),X(I+1)],
C             ISMON(I) = -3  if function is probably decreasing;
C             ISMON(I) = -1  if function is strictly decreasing;
C             ISMON(I) =  0  if function is constant;
C             ISMON(I) =  1  if function is strictly increasing;
C             ISMON(I) =  2  if function is non-monotonic;
C             ISMON(I) =  3  if function is probably increasing.
C                If ABS(ISMON)=3, this means that the D-values are near
C                the boundary of the monotonicity region.  A small
C                increase produces non-monotonicity; decrease, strict
C                monotonicity.
C           The above applies to I=1(1)N-1.  ISMON(N) indicates whether
C              the entire function is monotonic on [X(1),X(N)].
C
C     IERR:OUT  is an error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C          (The ISMON-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C *Description:
C
C          DPCHCM:  Piecewise Cubic Hermite -- Check Monotonicity.
C
C     Checks the piecewise cubic Hermite function defined by  N,X,F,D
C     for monotonicity.
C
C     To provide compatibility with DPCHIM and DPCHIC, includes an
C     increment between successive values of the F- and D-arrays.
C
C *Cautions:
C     This provides the same capability as old DPCHMC, except that a
C     new output value, -3, was added February 1989.  (Formerly, -3
C     and +3 were lumped together in the single value 3.)  Codes that
C     flag nonmonotonicity by "IF (ISMON.EQ.2)" need not be changed.
C     Codes that check via "IF (ISMON.GE.3)" should change the test to
C     "IF (IABS(ISMON).GE.3)".  Codes that declare monotonicity via
C     "IF (ISMON.LE.1)" should change to "IF (IABS(ISMON).LE.1)".
C
C***REFERENCES  F. N. Fritsch and R. E. Carlson, Monotone piecewise
C                 cubic interpolation, SIAM Journal on Numerical Ana-
C                 lysis 17, 2 (April 1980), pp. 238-246.
C***ROUTINES CALLED  DCHFCM, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820518  DATE WRITTEN
C   820804  Converted to SLATEC library version.
C   831201  Reversed order of subscripts of F and D, so that the
C           routine will work properly when INCFD.GT.1 .  (Bug!)
C   870707  Corrected XERROR calls for d.p. name(s).
C   890206  Corrected XERROR calls.
C   890209  Added possible ISMON value of -3 and modified code so
C           that 1,3,-1 produces ISMON(N)=2, rather than 3.
C   890306  Added caution about changed output.
C   890407  Changed name from DPCHMC to DPCHCM, as requested at the
C           March 1989 SLATEC CML meeting, and made a few other
C           minor modifications necessitated by this change.
C   890407  Converted to new SLATEC format.
C   890407  Modified DESCRIPTION to LDOC format.
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920429  Revised format and order of references.  (WRB,FNF)
C***END PROLOGUE  DPCHCM
C
C  Fortran intrinsics used:  ISIGN.
C  Other routines used:  CHFCM, XERMSG.
C
C ----------------------------------------------------------------------
C
C  Programming notes:
C
C     An alternate organization would have separate loops for computing
C     ISMON(i), i=1,...,NSEG, and for the computation of ISMON(N).  The
C     first loop can be readily parallelized, since the NSEG calls to
C     CHFCM are independent.  The second loop can be cut short if
C     ISMON(N) is ever equal to 2, for it cannot be changed further.
C
C     To produce a single precision version, simply:
C        a. Change DPCHCM to PCHCM wherever it occurs,
C        b. Change DCHFCM to CHFCM wherever it occurs, and
C        c. Change the double precision declarations to real.
C
C  DECLARE ARGUMENTS.
C
      INTEGER N, INCFD, ISMON(N), IERR
      DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N)
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER I, NSEG
      DOUBLE PRECISION  DELTA
      INTEGER DCHFCM
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  DPCHCM
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
      SKIP = .TRUE.
C
C  FUNCTION DEFINITION IS OK -- GO ON.
C
    5 CONTINUE
      NSEG = N - 1
      DO 90  I = 1, NSEG
         DELTA = (F(1,I+1)-F(1,I))/(X(I+1)-X(I))
C                   -------------------------------
         ISMON(I) = DCHFCM (D(1,I), D(1,I+1), DELTA)
C                   -------------------------------
         IF (I .EQ. 1)  THEN
            ISMON(N) = ISMON(1)
         ELSE
C           Need to figure out cumulative monotonicity from following
C           "multiplication table":
C
C                    +        I S M O N (I)
C                     +  -3  -1   0   1   3   2
C                      +------------------------+
C               I   -3 I -3  -3  -3   2   2   2 I
C               S   -1 I -3  -1  -1   2   2   2 I
C               M    0 I -3  -1   0   1   3   2 I
C               O    1 I  2   2   1   1   3   2 I
C               N    3 I  2   2   3   3   3   2 I
C              (N)   2 I  2   2   2   2   2   2 I
C                      +------------------------+
C           Note that the 2 row and column are out of order so as not
C           to obscure the symmetry in the rest of the table.
C
C           No change needed if equal or constant on this interval or
C           already declared nonmonotonic.
            IF ( (ISMON(I).NE.ISMON(N)) .AND. (ISMON(I).NE.0)
     .                                  .AND. (ISMON(N).NE.2) )  THEN
               IF ( (ISMON(I).EQ.2) .OR. (ISMON(N).EQ.0) )  THEN
                  ISMON(N) =  ISMON(I)
               ELSE IF (ISMON(I)*ISMON(N) .LT. 0)  THEN
C                 This interval has opposite sense from curve so far.
                  ISMON(N) = 2
               ELSE
C                 At this point, both are nonzero with same sign, and
C                 we have already eliminated case both +-1.
                  ISMON(N) = ISIGN (3, ISMON(N))
               ENDIF
            ENDIF
         ENDIF
   90 CONTINUE
C
C  NORMAL RETURN.
C
      IERR = 0
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHCM',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHCM', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHCM',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      RETURN
      END
      SUBROUTINE DPCHCS (SWITCH, N, H, SLOPE, D, INCFD, IERR)

c*********************************************************************72
c
cc DPCHCS adjusts derivative values for DPCHIC.
c
C***BEGIN PROLOGUE  DPCHCS
C***SUBSIDIARY
C***PURPOSE  Adjusts derivative values for DPCHIC
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (PCHCS-S, DPCHCS-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C         DPCHCS:  DPCHIC Monotonicity Switch Derivative Setter.
C
C     Called by  DPCHIC  to adjust the values of D in the vicinity of a
C     switch in direction of monotonicity, to produce a more "visually
C     pleasing" curve than that given by  DPCHIM .
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, IERR
C        DOUBLE PRECISION  SWITCH, H(N), SLOPE(N), D(INCFD,N)
C
C        CALL  DPCHCS (SWITCH, N, H, SLOPE, D, INCFD, IERR)
C
C   Parameters:
C
C     SWITCH -- (input) indicates the amount of control desired over
C           local excursions from data.
C
C     N -- (input) number of data points.  (assumes N.GT.2 .)
C
C     H -- (input) real*8 array of interval lengths.
C     SLOPE -- (input) real*8 array of data slopes.
C           If the data are (X(I),Y(I)), I=1(1)N, then these inputs are:
C                  H(I) =  X(I+1)-X(I),
C              SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1.
C
C     D -- (input) real*8 array of derivative values at the data points,
C           as determined by DPCHCI.
C          (output) derivatives in the vicinity of switches in direction
C           of monotonicity may be adjusted to produce a more "visually
C           pleasing" curve.
C           The value corresponding to X(I) is stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C           No other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in D.
C           This argument is provided primarily for 2-D applications.
C
C     IERR -- (output) error flag.  should be zero.
C           If negative, trouble in DPCHSW.  (should never happen.)
C
C    -------
C    WARNING:  This routine does no validity-checking of arguments.
C    -------
C
C  Fortran intrinsics used:  ABS, MAX, MIN.
C
C***SEE ALSO  DPCHIC
C***ROUTINES CALLED  DPCHST, DPCHSW
C***REVISION HISTORY  (YYMMDD)
C   820218  DATE WRITTEN
C   820617  Redesigned to (1) fix  problem with lack of continuity
C           approaching a flat-topped peak (2) be cleaner and
C           easier to verify.
C           Eliminated subroutines PCHSA and PCHSX in the process.
C   820622  1. Limited fact to not exceed one, so computed D is a
C             convex combination of DPCHCI value and DPCHSD value.
C           2. Changed fudge from 1 to 4 (based on experiments).
C   820623  Moved PCHSD to an inline function (eliminating MSWTYP).
C   820805  Converted to SLATEC library version.
C   870707  Corrected conversion to double precision.
C   870813  Minor cosmetic changes.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891006  Modified spacing in computation of DFLOC.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR section in prologue.  (WRB)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  DPCHCS
C
C  Programming notes:
C     1. The function  DPCHST(ARG1,ARG2)  is assumed to return zero if
C        either argument is zero, +1 if they are of the same sign, and
C        -1 if they are of opposite sign.
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, IERR
      DOUBLE PRECISION  SWITCH, H(*), SLOPE(*), D(INCFD,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, INDX, K, NLESS1
      DOUBLE PRECISION  DEL(3), DEXT, DFLOC, DFMX, FACT, FUDGE, ONE,
     *      SLMAX, WTAVE(2), ZERO
      SAVE ZERO, ONE, FUDGE
      DOUBLE PRECISION  DPCHST
C
C  DEFINE INLINE FUNCTION FOR WEIGHTED AVERAGE OF SLOPES.
C
      DOUBLE PRECISION  DPCHSD, S1, S2, H1, H2
      DPCHSD(S1,S2,H1,H2) = (H2/(H1+H2))*S1 + (H1/(H1+H2))*S2
C
C  INITIALIZE.
C
      DATA  ZERO /0.D0/,  ONE/1.D0/
      DATA  FUDGE /4.D0/
C***FIRST EXECUTABLE STATEMENT  DPCHCS
      IERR = 0
      NLESS1 = N - 1
C
C  LOOP OVER SEGMENTS.
C
      DO 900  I = 2, NLESS1
         IF ( DPCHST(SLOPE(I-1),SLOPE(I)) )  100, 300, 900
C             --------------------------
C
  100    CONTINUE
C
C....... SLOPE SWITCHES MONOTONICITY AT I-TH POINT .....................
C
C           DO NOT CHANGE D IF 'UP-DOWN-UP'.
            IF (I .GT. 2)  THEN
               IF ( DPCHST(SLOPE(I-2),SLOPE(I)) .GT. ZERO)  GO TO 900
C                   --------------------------
            ENDIF
            IF (I .LT. NLESS1)  THEN
               IF ( DPCHST(SLOPE(I+1),SLOPE(I-1)) .GT. ZERO)  GO TO 900
C                   ----------------------------
            ENDIF
C
C   ....... COMPUTE PROVISIONAL VALUE FOR D(1,I).
C
            DEXT = DPCHSD (SLOPE(I-1), SLOPE(I), H(I-1), H(I))
C
C   ....... DETERMINE WHICH INTERVAL CONTAINS THE EXTREMUM.
C
            IF ( DPCHST(DEXT, SLOPE(I-1)) )  200, 900, 250
C                -----------------------
C
  200       CONTINUE
C              DEXT AND SLOPE(I-1) HAVE OPPOSITE SIGNS --
C                        EXTREMUM IS IN (X(I-1),X(I)).
               K = I-1
C              SET UP TO COMPUTE NEW VALUES FOR D(1,I-1) AND D(1,I).
               WTAVE(2) = DEXT
               IF (K .GT. 1)
     *            WTAVE(1) = DPCHSD (SLOPE(K-1), SLOPE(K), H(K-1), H(K))
               GO TO 400
C
  250       CONTINUE
C              DEXT AND SLOPE(I) HAVE OPPOSITE SIGNS --
C                        EXTREMUM IS IN (X(I),X(I+1)).
               K = I
C              SET UP TO COMPUTE NEW VALUES FOR D(1,I) AND D(1,I+1).
               WTAVE(1) = DEXT
               IF (K .LT. NLESS1)
     *            WTAVE(2) = DPCHSD (SLOPE(K), SLOPE(K+1), H(K), H(K+1))
               GO TO 400
C
  300    CONTINUE
C
C....... AT LEAST ONE OF SLOPE(I-1) AND SLOPE(I) IS ZERO --
C                     CHECK FOR FLAT-TOPPED PEAK .......................
C
            IF (I .EQ. NLESS1)  GO TO 900
            IF ( DPCHST(SLOPE(I-1), SLOPE(I+1)) .GE. ZERO)  GO TO 900
C                -----------------------------
C
C           WE HAVE FLAT-TOPPED PEAK ON (X(I),X(I+1)).
            K = I
C           SET UP TO COMPUTE NEW VALUES FOR D(1,I) AND D(1,I+1).
            WTAVE(1) = DPCHSD (SLOPE(K-1), SLOPE(K), H(K-1), H(K))
            WTAVE(2) = DPCHSD (SLOPE(K), SLOPE(K+1), H(K), H(K+1))
C
  400    CONTINUE
C
C....... AT THIS POINT WE HAVE DETERMINED THAT THERE WILL BE AN EXTREMUM
C        ON (X(K),X(K+1)), WHERE K=I OR I-1, AND HAVE SET ARRAY WTAVE--
C           WTAVE(1) IS A WEIGHTED AVERAGE OF SLOPE(K-1) AND SLOPE(K),
C                    IF K.GT.1
C           WTAVE(2) IS A WEIGHTED AVERAGE OF SLOPE(K) AND SLOPE(K+1),
C                    IF K.LT.N-1
C
         SLMAX = ABS(SLOPE(K))
         IF (K .GT. 1)    SLMAX = MAX( SLMAX, ABS(SLOPE(K-1)) )
         IF (K.LT.NLESS1) SLMAX = MAX( SLMAX, ABS(SLOPE(K+1)) )
C
         IF (K .GT. 1)  DEL(1) = SLOPE(K-1) / SLMAX
         DEL(2) = SLOPE(K) / SLMAX
         IF (K.LT.NLESS1)  DEL(3) = SLOPE(K+1) / SLMAX
C
         IF ((K.GT.1) .AND. (K.LT.NLESS1))  THEN
C           NORMAL CASE -- EXTREMUM IS NOT IN A BOUNDARY INTERVAL.
            FACT = FUDGE* ABS(DEL(3)*(DEL(1)-DEL(2))*(WTAVE(2)/SLMAX))
            D(1,K) = D(1,K) + MIN(FACT,ONE)*(WTAVE(1) - D(1,K))
            FACT = FUDGE* ABS(DEL(1)*(DEL(3)-DEL(2))*(WTAVE(1)/SLMAX))
            D(1,K+1) = D(1,K+1) + MIN(FACT,ONE)*(WTAVE(2) - D(1,K+1))
         ELSE
C           SPECIAL CASE K=1 (WHICH CAN OCCUR ONLY IF I=2) OR
C                        K=NLESS1 (WHICH CAN OCCUR ONLY IF I=NLESS1).
            FACT = FUDGE* ABS(DEL(2))
            D(1,I) = MIN(FACT,ONE) * WTAVE(I-K+1)
C              NOTE THAT I-K+1 = 1 IF K=I  (=NLESS1),
C                        I-K+1 = 2 IF K=I-1(=1).
         ENDIF
C
C
C....... ADJUST IF NECESSARY TO LIMIT EXCURSIONS FROM DATA.
C
         IF (SWITCH .LE. ZERO)  GO TO 900
C
         DFLOC = H(K)*ABS(SLOPE(K))
         IF (K .GT. 1)    DFLOC = MAX( DFLOC, H(K-1)*ABS(SLOPE(K-1)) )
         IF (K.LT.NLESS1) DFLOC = MAX( DFLOC, H(K+1)*ABS(SLOPE(K+1)) )
         DFMX = SWITCH*DFLOC
         INDX = I-K+1
C        INDX = 1 IF K=I, 2 IF K=I-1.
C        ---------------------------------------------------------------
         CALL DPCHSW(DFMX, INDX, D(1,K), D(1,K+1), H(K), SLOPE(K), IERR)
C        ---------------------------------------------------------------
         IF (IERR .NE. 0)  RETURN
C
C....... END OF SEGMENT LOOP.
C
  900 CONTINUE
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DPCHDF (K, X, S, IERR)

c*********************************************************************72
c
cc DPCHDF computes divided differences for DPCHCE and DPCHSP.
c
C***BEGIN PROLOGUE  DPCHDF
C***SUBSIDIARY
C***PURPOSE  Computes divided differences for DPCHCE and DPCHSP
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (PCHDF-S, DPCHDF-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C          DPCHDF:   DPCHIP Finite Difference Formula
C
C     Uses a divided difference formulation to compute a K-point approx-
C     imation to the derivative at X(K) based on the data in X and S.
C
C     Called by  DPCHCE  and  DPCHSP  to compute 3- and 4-point boundary
C     derivative approximations.
C
C ----------------------------------------------------------------------
C
C     On input:
C        K      is the order of the desired derivative approximation.
C               K must be at least 3 (error return if not).
C        X      contains the K values of the independent variable.
C               X need not be ordered, but the values **MUST** be
C               distinct.  (Not checked here.)
C        S      contains the associated slope values:
C                  S(I) = (F(I+1)-F(I))/(X(I+1)-X(I)), I=1(1)K-1.
C               (Note that S need only be of length K-1.)
C
C     On return:
C        S      will be destroyed.
C        IERR   will be set to -1 if K.LT.2 .
C        DPCHDF  will be set to the desired derivative approximation if
C               IERR=0 or to zero if IERR=-1.
C
C ----------------------------------------------------------------------
C
C***SEE ALSO  DPCHCE, DPCHSP
C***REFERENCES  Carl de Boor, A Practical Guide to Splines, Springer-
C                 Verlag, New York, 1978, pp. 10-16.
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820503  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   870707  Corrected XERROR calls for d.p. name(s).
C   870813  Minor cosmetic changes.
C   890206  Corrected XERROR calls.
C   890411  Added SAVE statements (Vers. 3.2).
C   890411  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB)
C   920429  Revised format and order of references.  (WRB,FNF)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  DPCHDF
C
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  K, IERR
      DOUBLE PRECISION  X(K), S(K)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, J
      DOUBLE PRECISION  VALUE, ZERO
      SAVE ZERO
      DATA  ZERO /0.D0/
C
C  CHECK FOR LEGAL VALUE OF K.
C
C***FIRST EXECUTABLE STATEMENT  DPCHDF
      IF (K .LT. 3)  GO TO 5001
C
C  COMPUTE COEFFICIENTS OF INTERPOLATING POLYNOMIAL.
C
      DO 10  J = 2, K-1
         DO 9  I = 1, K-J
            S(I) = (S(I+1)-S(I))/(X(I+J)-X(I))
    9    CONTINUE
   10 CONTINUE
C
C  EVALUATE DERIVATIVE AT X(K).
C
      VALUE = S(1)
      DO 20  I = 2, K-1
         VALUE = S(I) + VALUE*(X(K)-X(I))
   20 CONTINUE
C
C  NORMAL RETURN.
C
      IERR = 0
      DPCHDF = VALUE
      RETURN
C
C  ERROR RETURN.
C
 5001 CONTINUE
C     K.LT.3 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHDF', 'K LESS THAN THREE', IERR, 1)
      DPCHDF = ZERO
      RETURN
      END
      SUBROUTINE DPCHEV(N,X,F,D,NVAL,XVAL,FVAL,DVAL,IERR)

c*********************************************************************72
c
cc DPCHEV: value and derivative of spline or cubic Hermite at many points.
c
C***BEGIN PROLOGUE  DPCHEV
C***DATE WRITTEN   870828   (YYMMDD)
C***REVISION DATE  870828   (YYMMDD)
C***CATEGORY NO.  E3,H1
C***KEYWORDS  CUBIC HERMITE OR SPLINE DIFFERENTIATION,CUBIC HERMITE
C             EVALUATION,EASY TO USE SPLINE OR CUBIC HERMITE EVALUATOR
C***AUTHOR  KAHANER, D.K., (NBS)
C             SCIENTIFIC COMPUTING DIVISION
C             NATIONAL BUREAU OF STANDARDS
C             ROOM A161, TECHNOLOGY BUILDING
C             GAITHERSBURG, MARYLAND 20899
C             (301) 975-3808
C***PURPOSE  Evaluates the function and first derivative of a piecewise
C            cubic Hermite or spline function at an array of points
C            XVAL.  It is easy to use.
C***DESCRIPTION
C
C          DPCHEV:  Piecewise Cubic Hermite or Spline Derivative
C                   Evaluator. Easy to Use.
C
C     From the book "Numerical Methods and Software"
C          by  D. Kahaner, C. Moler, S. Nash
C                 Prentice Hall 1988
C
C     Evaluates the function and first derivative of the cubic Hermite
C     or spline function defined by  N, X, F, D, at the array of points
C     XVAL.
C
C
C     This is an easy to use driver for the routines by F.N. Fritsch
C     described in reference (2) below. Those also have other 
C     capabilities.
C
C ----------------------------------------------------------------------
C
C  Calling sequence: CALL  DPCHEV (N, X, F, D, NVAL, XVAL, FVAL, DVAL, IERR)
C
C     INTEGER  N, NVAL, IERR
C     DOUBLE PRECISION  X(N), F(N), D(N), XVAL(NVAL), FVAL(NVAL), DVAL(NVAL)
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) double precision array of independent variable 
C           values.  The elements of X must be strictly increasing:
C             X(I-1) .LT. X(I),  I = 2(1)N. (Error return if not.)
C
C     F -- (input) double precision array of function values.  F(I) is
C           the value corresponding to X(I).
C
C     D -- (input) double precision array of derivative values.  
C          D(I) is the value corresponding to X(I).
C
C  NVAL -- (input) number of points at which the functions are to be
C           evaluated. ( Error return if NVAL.LT.1 )
C
C  XVAL -- (input) double precision array of points at which the 
C          functions are to be evaluated.
C
C          NOTES:
C           1. The evaluation will be most efficient if the elements
C              of XVAL are increasing relative to X;
C              that is,   XVAL(J) .GE. X(I)
C              implies    XVAL(K) .GE. X(I),  all K.GE.J .
C           2. If any of the XVAL are outside the interval [X(1),X(N)],
C              values are extrapolated from the nearest extreme cubic,
C              and a warning error is returned.
C
C  FVAL -- (output) double precision array of values of the cubic 
C          Hermite function defined by  N, X, F, D  at the points  XVAL.
C
C  DVAL -- (output) double precision array of values of the 
C          first derivative of the same function at the points  XVAL.
C
C  IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  means that extrapolation was performed at
C                 IERR points.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if NVAL.LT.1 .
C           (Output arrays have not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C              IERR = -5  if an error has occurred in the lower-level
C                         routine DCHFDV.  NB: this should never happen.
C                         Notify the author **IMMEDIATELY** if it does.
C
C ----------------------------------------------------------------------
C***REFERENCES  1. F.N.FRITSCH AND R.E.CARLSON, 'MONOTONE PIECEWISE
C                 CUBIC INTERPOLATION,' SIAM J.NUMER.ANAL. 17, 2 (APRIL
C                 1980), 238-246.
C               2. F.N.FRITSCH, 'PIECEWISE CUBIC HERMITE INTERPOLATION
C                 PACKAGE, FINAL SPECIFICATIONS', LAWRENCE LIVERMORE
C                 NATIONAL LABORATORY, COMPUTER DOCUMENTATION UCID-30194
C                 AUGUST 1982.
C***ROUTINES CALLED  DPCHFD
C***END PROLOGUE  DPCHEV
      INTEGER  N, NVAL, IERR
      DOUBLE PRECISION  X(N), F(N), D(N), XVAL(NVAL), FVAL(NVAL), 
     *DVAL(NVAL)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER INCFD
      LOGICAL SKIP
      DATA SKIP /.TRUE./
      DATA INCFD /1/
C
C
C***FIRST EXECUTABLE STATEMENT  DPCHEV
C
      CALL DPCHFD(N,X,F,D,INCFD,SKIP,NVAL,XVAL,FVAL,DVAL,IERR)
C
C
 5000 CONTINUE
      RETURN
      END
      SUBROUTINE DPCHEZ ( N, X, F, D, SPLINE, WK, LWK, IERR )

c*********************************************************************72
c
cc DPCHEZ sets up a spline or cubic Hermite interpolant.
c
C***BEGIN PROLOGUE  DPCHEZ
C***DATE WRITTEN   870821   (YYMMDD)
C***REVISION DATE  870908   (YYMMDD)
C***CATEGORY NO.  E1B
C***KEYWORDS  CUBIC HERMITE MONOTONE INTERPOLATION, SPLINE
C             INTERPOLATION, EASY TO USE PIECEWISE CUBIC INTERPOLATION
C***AUTHOR  KAHANER, D.K., (NBS)
C             SCIENTIFIC COMPUTING DIVISION
C             NATIONAL BUREAU OF STANDARDS
C             GAITHERSBURG, MARYLAND 20899
C             (301) 975-3808
C***PURPOSE  Easy to use spline or cubic Hermite interpolation.
C***DESCRIPTION
C
C          DPCHEZ:  Piecewise Cubic Interpolation, Easy to Use.
C
C     From the book "Numerical Methods and Software"
C          by  D. Kahaner, C. Moler, S. Nash
C               Prentice Hall 1988
C
C     Sets derivatives for spline (two continuous derivatives) or
C     Hermite cubic (one continuous derivative) interpolation.
C     Spline interpolation is smoother, but may not "look" right if the
C     data contains both "steep" and "flat" sections.  Hermite cubics
C     can produce a "visually pleasing" and monotone interpolant to
C     monotone data. This is an easy to use driver for the routines
C     by F. N. Fritsch in reference (4) below. Various boundary
C     conditions are set to default values by DPCHEZ. Many other choices
C     are available in the subroutines PCHIC, DPCHIM and DPCHSP.
C
C     Use PCHEV to evaluate the resulting function and its derivative.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:   CALL  DPCHEZ (N, X, F, D, SPLINE, WK, LWK, IERR)
C
C     INTEGER  N, IERR,  LWK
C     DOUBLE PRECISION  X(N), F(N), D(N), WK(*)
C     LOGICAL SPLINE
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C           If N=2, simply does linear interpolation.
C
C     X -- (input) double precision array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) double precision array of dependent variable values to be inter-
C           polated.  F(I) is value corresponding to X(I).
C
C     D -- (output) double precision array of derivative values at the data points.
C
C     SPLINE -- (input) logical variable to specify if the interpolant
C           is to be a spline with two continuous derivaties
C           (set SPLINE=.TRUE.) or a Hermite cubic interpolant with one
C           continuous derivative (set SPLINE=.FALSE.).
C        Note: If SPLINE=.TRUE. the interpolating spline satisfies the
C           default "not-a-knot" boundary condition, with a continuous
C           third derivative at X(2) and X(N-1). See reference (3).
C              If SPLINE=.FALSE. the interpolating Hermite cubic will be
C           monotone if the input data is monotone. Boundary conditions
C           computed from the derivative of a local quadratic unless thi
C           alters monotonicity.
C
C     WK -- (scratch) double precision work array, which must be declared by the cal
C           program to be at least 2*N if SPLINE is .TRUE. and not used
C           otherwise.
C
C     LWK -- (input) length of work array WK. (Error return if
C           LWK.LT.2*N and SPLINE is .TRUE., not checked otherwise.)
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  (can only occur when SPLINE=.FALSE.) means tha
C                 IERR switches in the direction of monotonicity were de
C                 When SPLINE=.FALSE.,  DPCHEZ guarantees that if the inp
C                 data is monotone, the interpolant will be too. This wa
C                 is to alert you to the fact that the input data was no
C                 monotone.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -7  if LWK is less than 2*N and SPLINE is .TRUE.
C             (The D-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C ----------------------------------------------------------------------
C***REFERENCES  1. F.N.FRITSCH AND R.E.CARLSON, 'MONOTONE PIECEWISE
C                 CUBIC INTERPOLATION,' SIAM J.NUMER.ANAL. 17, 2 (APRIL
C                 1980), 238-246.
C               2. F.N.FRITSCH AND J.BUTLAND, 'A METHOD FOR CONSTRUCTING
C                 LOCAL MONOTONE PIECEWISE CUBIC INTERPOLANTS,' LLNL
C                 PREPRINT UCRL-87559 (APRIL 1982).
C               3. CARL DE BOOR, A PRACTICAL GUIDE TO SPLINES, SPRINGER-
C                 VERLAG (NEW YORK, 1978).  (ESP. CHAPTER IV, PP.49-62.)
C               4. F.N.FRITSCH, 'PIECEWISE CUBIC HERMITE INTERPOLATION
C                 PACKAGE, FINAL SPECIFICATIONS', LAWRENCE LIVERMORE
C                 NATIONAL LABORATORY, COMPUTER DOCUMENTATION UCID-30194
C                 AUGUST 1982.
C***ROUTINES CALLED  DPCHIM,DPCHSP
C***END PROLOGUE  DPCHEZ
      INTEGER  N, LWK, IERR
      DOUBLE PRECISION  X(N), F(N), D(N), WK(LWK)
      LOGICAL SPLINE
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER IC(2), INCFD
      DOUBLE PRECISION  VC(2)
      DATA IC(1) /0/
      DATA IC(2) /0/
      DATA INCFD /1/
C
C
C***FIRST EXECUTABLE STATEMENT  DPCHEZ
C
      IF ( SPLINE ) THEN
        CALL  DPCHSP ( IC, VC, N, X, F, D, INCFD, WK, LWK, IERR )
      ELSE
        CALL  DPCHIM ( N, X, F, D, INCFD, IERR )
      ENDIF
C
C  ERROR CONDITIONS ALREADY CHECKED IN DPCHSP OR DPCHIM

      RETURN
      END
      SUBROUTINE DPCHFD (N, X, F, D, INCFD, SKIP, NE, XE, FE, DE, IERR)

c*********************************************************************72
c
cc DPCHFD evaluates a piecewise cubic Hermite and derivative at many points.
c
C***BEGIN PROLOGUE  DPCHFD
C***PURPOSE  Evaluate a piecewise cubic Hermite function and its first
C            derivative at an array of points.  May be used by itself
C            for Hermite interpolation, or as an evaluator for DPCHIM
C            or DPCHIC. If only function values are required, use
C            DPCHFE instead.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3, H1
C***TYPE      DOUBLE PRECISION (PCHFD-S, DPCHFD-D)
C***KEYWORDS  CUBIC HERMITE DIFFERENTIATION, CUBIC HERMITE EVALUATION,
C             HERMITE INTERPOLATION, PCHIP, PIECEWISE CUBIC EVALUATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          DPCHFD:  Piecewise Cubic Hermite Function and Derivative
C                  evaluator
C
C     Evaluates the cubic Hermite function defined by  N, X, F, D,  to-
C     gether with its first derivative, at the points  XE(J), J=1(1)NE.
C
C     If only function values are required, use DPCHFE, instead.
C
C     To provide compatibility with DPCHIM and DPCHIC, includes an
C     increment between successive values of the F- and D-arrays.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, NE, IERR
C        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N), XE(NE), FE(NE),
C                          DE(NE)
C        LOGICAL  SKIP
C
C        CALL  DPCHFD (N, X, F, D, INCFD, SKIP, NE, XE, FE, DE, IERR)
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real*8 array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real*8 array of function values.  F(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     D -- (input) real*8 array of derivative values.  D(1+(I-1)*INCFD)
C           is the value corresponding to X(I).
C
C     INCFD -- (input) increment between successive values in F and D.
C           (Error return if  INCFD.LT.1 .)
C
C     SKIP -- (input/output) logical variable which should be set to
C           .TRUE. if the user wishes to skip checks for validity of
C           preceding parameters, or to .FALSE. otherwise.
C           This will save time in case these checks have already
C           been performed (say, in DPCHIM or DPCHIC).
C           SKIP will be set to .TRUE. on normal return.
C
C     NE -- (input) number of evaluation points.  (Error return if
C           NE.LT.1 .)
C
C     XE -- (input) real*8 array of points at which the functions are to
C           be evaluated.
C
C
C          NOTES:
C           1. The evaluation will be most efficient if the elements
C              of XE are increasing relative to X;
C              that is,   XE(J) .GE. X(I)
C              implies    XE(K) .GE. X(I),  all K.GE.J .
C           2. If any of the XE are outside the interval [X(1),X(N)],
C              values are extrapolated from the nearest extreme cubic,
C              and a warning error is returned.
C
C     FE -- (output) real*8 array of values of the cubic Hermite
C           function defined by  N, X, F, D  at the points  XE.
C
C     DE -- (output) real*8 array of values of the first derivative of
C           the same function at the points  XE.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  means that extrapolation was performed at
C                 IERR points.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if NE.LT.1 .
C           (Output arrays have not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C              IERR = -5  if an error has occurred in the lower-level
C                         routine DCHFDV.  NB: this should never happen.
C                         Notify the author **IMMEDIATELY** if it does.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DCHFDV, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811020  DATE WRITTEN
C   820803  Minor cosmetic changes for release 1.
C   870707  Corrected XERROR calls for d.p. name(s).
C   890206  Corrected XERROR calls.
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DPCHFD
C  Programming notes:
C
C     1. To produce a single precision version, simply:
C        a. Change DPCHFD to PCHFD, and DCHFDV to CHFDV, wherever they
C           occur,
C        b. Change the double precision declaration to real,
C
C     2. Most of the coding between the call to DCHFDV and the end of
C        the IR-loop could be eliminated if it were permissible to
C        assume that XE is ordered relative to X.
C
C     3. DCHFDV does not assume that X1 is less than X2.  thus, it would
C        be possible to write a version of DPCHFD that assumes a strict-
C        ly decreasing X-array by simply running the IR-loop backwards
C        (and reversing the order of appropriate tests).
C
C     4. The present code has a minor bug, which I have decided is not
C        worth the effort that would be required to fix it.
C        If XE contains points in [X(N-1),X(N)], followed by points .LT.
C        X(N-1), followed by points .GT.X(N), the extrapolation points
C        will be counted (at least) twice in the total returned in IERR.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, NE, IERR
      DOUBLE PRECISION  X(*), F(INCFD,*), D(INCFD,*), XE(*), FE(*),
     * DE(*)
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IERC, IR, J, JFIRST, NEXT(2), NJ
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  DPCHFD
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
    5 CONTINUE
      IF ( NE.LT.1 )  GO TO 5004
      IERR = 0
      SKIP = .TRUE.
C
C  LOOP OVER INTERVALS.        (   INTERVAL INDEX IS  IL = IR-1  . )
C                              ( INTERVAL IS X(IL).LE.X.LT.X(IR) . )
      JFIRST = 1
      IR = 2
   10 CONTINUE
C
C     SKIP OUT OF LOOP IF HAVE PROCESSED ALL EVALUATION POINTS.
C
         IF (JFIRST .GT. NE)  GO TO 5000
C
C     LOCATE ALL POINTS IN INTERVAL.
C
         DO 20  J = JFIRST, NE
            IF (XE(J) .GE. X(IR))  GO TO 30
   20    CONTINUE
         J = NE + 1
         GO TO 40
C
C     HAVE LOCATED FIRST POINT BEYOND INTERVAL.
C
   30    CONTINUE
         IF (IR .EQ. N)  J = NE + 1
C
   40    CONTINUE
         NJ = J - JFIRST
C
C     SKIP EVALUATION IF NO POINTS IN INTERVAL.
C
         IF (NJ .EQ. 0)  GO TO 50
C
C     EVALUATE CUBIC AT XE(I),  I = JFIRST (1) J-1 .
C
C       ----------------------------------------------------------------
        CALL DCHFDV (X(IR-1),X(IR), F(1,IR-1),F(1,IR), D(1,IR-1),D(1,IR)
     *              ,NJ, XE(JFIRST), FE(JFIRST), DE(JFIRST), NEXT, IERC)
C       ----------------------------------------------------------------
         IF (IERC .LT. 0)  GO TO 5005
C
         IF (NEXT(2) .EQ. 0)  GO TO 42
C        IF (NEXT(2) .GT. 0)  THEN
C           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(2) TO THE
C           RIGHT OF X(IR).
C
            IF (IR .LT. N)  GO TO 41
C           IF (IR .EQ. N)  THEN
C              THESE ARE ACTUALLY EXTRAPOLATION POINTS.
               IERR = IERR + NEXT(2)
               GO TO 42
   41       CONTINUE
C           ELSE
C              WE SHOULD NEVER HAVE GOTTEN HERE.
               GO TO 5005
C           ENDIF
C        ENDIF
   42    CONTINUE
C
         IF (NEXT(1) .EQ. 0)  GO TO 49
C        IF (NEXT(1) .GT. 0)  THEN
C           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(1) TO THE
C           LEFT OF X(IR-1).
C
            IF (IR .GT. 2)  GO TO 43
C           IF (IR .EQ. 2)  THEN
C              THESE ARE ACTUALLY EXTRAPOLATION POINTS.
               IERR = IERR + NEXT(1)
               GO TO 49
   43       CONTINUE
C           ELSE
C              XE IS NOT ORDERED RELATIVE TO X, SO MUST ADJUST
C              EVALUATION INTERVAL.
C
C              FIRST, LOCATE FIRST POINT TO LEFT OF X(IR-1).
               DO 44  I = JFIRST, J-1
                  IF (XE(I) .LT. X(IR-1))  GO TO 45
   44          CONTINUE
C              NOTE-- CANNOT DROP THROUGH HERE UNLESS THERE IS AN ERROR
C                     IN DCHFDV.
               GO TO 5005
C
   45          CONTINUE
C              RESET J.  (THIS WILL BE THE NEW JFIRST.)
               J = I
C
C              NOW FIND OUT HOW FAR TO BACK UP IN THE X-ARRAY.
               DO 46  I = 1, IR-1
                  IF (XE(J) .LT. X(I)) GO TO 47
   46          CONTINUE
C              NB-- CAN NEVER DROP THROUGH HERE, SINCE XE(J).LT.X(IR-1).
C
   47          CONTINUE
C              AT THIS POINT, EITHER  XE(J) .LT. X(1)
C                 OR      X(I-1) .LE. XE(J) .LT. X(I) .
C              RESET IR, RECOGNIZING THAT IT WILL BE INCREMENTED BEFORE
C              CYCLING.
               IR = MAX(1, I-1)
C           ENDIF
C        ENDIF
   49    CONTINUE
C
         JFIRST = J
C
C     END OF IR-LOOP.
C
   50 CONTINUE
      IR = IR + 1
      IF (IR .LE. N)  GO TO 10
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHFD',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHFD', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHFD',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      RETURN
C
 5004 CONTINUE
C     NE.LT.1 RETURN.
      IERR = -4
      CALL XERMSG ('SLATEC', 'DPCHFD',
     +   'NUMBER OF EVALUATION POINTS LESS THAN ONE', IERR, 1)
      RETURN
C
 5005 CONTINUE
C     ERROR RETURN FROM DCHFDV.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -5
      CALL XERMSG ('SLATEC', 'DPCHFD',
     +   'ERROR RETURN FROM DCHFDV -- FATAL', IERR, 2)
      RETURN
      END
      SUBROUTINE DPCHFE (N, X, F, D, INCFD, SKIP, NE, XE, FE, IERR)

c*********************************************************************72
c
cc DPCHFE evaluates a piecewise cubic Hermite function at many points.
c
C***BEGIN PROLOGUE  DPCHFE
C***PURPOSE  Evaluate a piecewise cubic Hermite function at an array of
C            points.  May be used by itself for Hermite interpolation,
C            or as an evaluator for DPCHIM or DPCHIC.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3
C***TYPE      DOUBLE PRECISION (PCHFE-S, DPCHFE-D)
C***KEYWORDS  CUBIC HERMITE EVALUATION, HERMITE INTERPOLATION, PCHIP,
C             PIECEWISE CUBIC EVALUATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          DPCHFE:  Piecewise Cubic Hermite Function Evaluator
C
C     Evaluates the cubic Hermite function defined by  N, X, F, D  at
C     the points  XE(J), J=1(1)NE.
C
C     To provide compatibility with DPCHIM and DPCHIC, includes an
C     increment between successive values of the F- and D-arrays.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, NE, IERR
C        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N), XE(NE), FE(NE)
C        LOGICAL  SKIP
C
C        CALL  DPCHFE (N, X, F, D, INCFD, SKIP, NE, XE, FE, IERR)
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real*8 array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real*8 array of function values.  F(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     D -- (input) real*8 array of derivative values.  D(1+(I-1)*INCFD)
C           is the value corresponding to X(I).
C
C     INCFD -- (input) increment between successive values in F and D.
C           (Error return if  INCFD.LT.1 .)
C
C     SKIP -- (input/output) logical variable which should be set to
C           .TRUE. if the user wishes to skip checks for validity of
C           preceding parameters, or to .FALSE. otherwise.
C           This will save time in case these checks have already
C           been performed (say, in DPCHIM or DPCHIC).
C           SKIP will be set to .TRUE. on normal return.
C
C     NE -- (input) number of evaluation points.  (Error return if
C           NE.LT.1 .)
C
C     XE -- (input) real*8 array of points at which the function is to
C           be evaluated.
C
C          NOTES:
C           1. The evaluation will be most efficient if the elements
C              of XE are increasing relative to X;
C              that is,   XE(J) .GE. X(I)
C              implies    XE(K) .GE. X(I),  all K.GE.J .
C           2. If any of the XE are outside the interval [X(1),X(N)],
C              values are extrapolated from the nearest extreme cubic,
C              and a warning error is returned.
C
C     FE -- (output) real*8 array of values of the cubic Hermite
C           function defined by  N, X, F, D  at the points  XE.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  means that extrapolation was performed at
C                 IERR points.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if NE.LT.1 .
C             (The FE-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DCHFEV, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811020  DATE WRITTEN
C   820803  Minor cosmetic changes for release 1.
C   870707  Corrected XERROR calls for d.p. name(s).
C   890206  Corrected XERROR calls.
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DPCHFE
C  Programming notes:
C
C     1. To produce a single precision version, simply:
C        a. Change DPCHFE to PCHFE, and DCHFEV to CHFEV, wherever they
C           occur,
C        b. Change the double precision declaration to real,
C
C     2. Most of the coding between the call to DCHFEV and the end of
C        the IR-loop could be eliminated if it were permissible to
C        assume that XE is ordered relative to X.
C
C     3. DCHFEV does not assume that X1 is less than X2.  thus, it would
C        be possible to write a version of DPCHFE that assumes a
C        decreasing X-array by simply running the IR-loop backwards
C        (and reversing the order of appropriate tests).
C
C     4. The present code has a minor bug, which I have decided is not
C        worth the effort that would be required to fix it.
C        If XE contains points in [X(N-1),X(N)], followed by points .LT.
C        X(N-1), followed by points .GT.X(N), the extrapolation points
C        will be counted (at least) twice in the total returned in IERR.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, NE, IERR
      DOUBLE PRECISION  X(*), F(INCFD,*), D(INCFD,*), XE(*), FE(*)
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IERC, IR, J, JFIRST, NEXT(2), NJ
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  DPCHFE
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
    5 CONTINUE
      IF ( NE.LT.1 )  GO TO 5004
      IERR = 0
      SKIP = .TRUE.
C
C  LOOP OVER INTERVALS.        (   INTERVAL INDEX IS  IL = IR-1  . )
C                              ( INTERVAL IS X(IL).LE.X.LT.X(IR) . )
      JFIRST = 1
      IR = 2
   10 CONTINUE
C
C     SKIP OUT OF LOOP IF HAVE PROCESSED ALL EVALUATION POINTS.
C
         IF (JFIRST .GT. NE)  GO TO 5000
C
C     LOCATE ALL POINTS IN INTERVAL.
C
         DO 20  J = JFIRST, NE
            IF (XE(J) .GE. X(IR))  GO TO 30
   20    CONTINUE
         J = NE + 1
         GO TO 40
C
C     HAVE LOCATED FIRST POINT BEYOND INTERVAL.
C
   30    CONTINUE
         IF (IR .EQ. N)  J = NE + 1
C
   40    CONTINUE
         NJ = J - JFIRST
C
C     SKIP EVALUATION IF NO POINTS IN INTERVAL.
C
         IF (NJ .EQ. 0)  GO TO 50
C
C     EVALUATE CUBIC AT XE(I),  I = JFIRST (1) J-1 .
C
C       ----------------------------------------------------------------
        CALL DCHFEV (X(IR-1),X(IR), F(1,IR-1),F(1,IR), D(1,IR-1),D(1,IR)
     *              ,NJ, XE(JFIRST), FE(JFIRST), NEXT, IERC)
C       ----------------------------------------------------------------
         IF (IERC .LT. 0)  GO TO 5005
C
         IF (NEXT(2) .EQ. 0)  GO TO 42
C        IF (NEXT(2) .GT. 0)  THEN
C           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(2) TO THE
C           RIGHT OF X(IR).
C
            IF (IR .LT. N)  GO TO 41
C           IF (IR .EQ. N)  THEN
C              THESE ARE ACTUALLY EXTRAPOLATION POINTS.
               IERR = IERR + NEXT(2)
               GO TO 42
   41       CONTINUE
C           ELSE
C              WE SHOULD NEVER HAVE GOTTEN HERE.
               GO TO 5005
C           ENDIF
C        ENDIF
   42    CONTINUE
C
         IF (NEXT(1) .EQ. 0)  GO TO 49
C        IF (NEXT(1) .GT. 0)  THEN
C           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(1) TO THE
C           LEFT OF X(IR-1).
C
            IF (IR .GT. 2)  GO TO 43
C           IF (IR .EQ. 2)  THEN
C              THESE ARE ACTUALLY EXTRAPOLATION POINTS.
               IERR = IERR + NEXT(1)
               GO TO 49
   43       CONTINUE
C           ELSE
C              XE IS NOT ORDERED RELATIVE TO X, SO MUST ADJUST
C              EVALUATION INTERVAL.
C
C              FIRST, LOCATE FIRST POINT TO LEFT OF X(IR-1).
               DO 44  I = JFIRST, J-1
                  IF (XE(I) .LT. X(IR-1))  GO TO 45
   44          CONTINUE
C              NOTE-- CANNOT DROP THROUGH HERE UNLESS THERE IS AN ERROR
C                     IN DCHFEV.
               GO TO 5005
C
   45          CONTINUE
C              RESET J.  (THIS WILL BE THE NEW JFIRST.)
               J = I
C
C              NOW FIND OUT HOW FAR TO BACK UP IN THE X-ARRAY.
               DO 46  I = 1, IR-1
                  IF (XE(J) .LT. X(I)) GO TO 47
   46          CONTINUE
C              NB-- CAN NEVER DROP THROUGH HERE, SINCE XE(J).LT.X(IR-1).
C
   47          CONTINUE
C              AT THIS POINT, EITHER  XE(J) .LT. X(1)
C                 OR      X(I-1) .LE. XE(J) .LT. X(I) .
C              RESET IR, RECOGNIZING THAT IT WILL BE INCREMENTED BEFORE
C              CYCLING.
               IR = MAX(1, I-1)
C           ENDIF
C        ENDIF
   49    CONTINUE
C
         JFIRST = J
C
C     END OF IR-LOOP.
C
   50 CONTINUE
      IR = IR + 1
      IF (IR .LE. N)  GO TO 10
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHFE',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHFE', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHFE',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      RETURN
C
 5004 CONTINUE
C     NE.LT.1 RETURN.
      IERR = -4
      CALL XERMSG ('SLATEC', 'DPCHFE',
     +   'NUMBER OF EVALUATION POINTS LESS THAN ONE', IERR, 1)
      RETURN
C
 5005 CONTINUE
C     ERROR RETURN FROM DCHFEV.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -5
      CALL XERMSG ('SLATEC', 'DPCHFE',
     +   'ERROR RETURN FROM DCHFEV -- FATAL', IERR, 2)
      RETURN
      END
      DOUBLE PRECISION FUNCTION DPCHIA (N, X, F, D, INCFD, SKIP, A, B,
     +   IERR)

c*********************************************************************72
c
cc DPCHIA evaluates the definite integral of a piecewise cubic Hermite function.
c
C***BEGIN PROLOGUE  DPCHIA
C***PURPOSE  Evaluate the definite integral of a piecewise cubic
C            Hermite function over an arbitrary interval.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3, H2A1B2
C***TYPE      DOUBLE PRECISION (PCHIA-S, DPCHIA-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, NUMERICAL INTEGRATION, PCHIP,
C             QUADRATURE
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          DPCHIA:  Piecewise Cubic Hermite Integrator, Arbitrary limits
C
C     Evaluates the definite integral of the cubic Hermite function
C     defined by  N, X, F, D  over the interval [A, B].
C
C     To provide compatibility with DPCHIM and DPCHIC, includes an
C     increment between successive values of the F- and D-arrays.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, IERR
C        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N), A, B
C        DOUBLE PRECISION  VALUE, DPCHIA
C        LOGICAL  SKIP
C
C        VALUE = DPCHIA (N, X, F, D, INCFD, SKIP, A, B, IERR)
C
C   Parameters:
C
C     VALUE -- (output) value of the requested integral.
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real*8 array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real*8 array of function values.  F(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     D -- (input) real*8 array of derivative values.  D(1+(I-1)*INCFD)
C           is the value corresponding to X(I).
C
C     INCFD -- (input) increment between successive values in F and D.
C           (Error return if  INCFD.LT.1 .)
C
C     SKIP -- (input/output) logical variable which should be set to
C           .TRUE. if the user wishes to skip checks for validity of
C           preceding parameters, or to .FALSE. otherwise.
C           This will save time in case these checks have already
C           been performed (say, in DPCHIM or DPCHIC).
C           SKIP will be set to .TRUE. on return with IERR.GE.0 .
C
C     A,B -- (input) the limits of integration.
C           NOTE:  There is no requirement that [A,B] be contained in
C                  [X(1),X(N)].  However, the resulting integral value
C                  will be highly suspect, if not.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning errors:
C              IERR = 1  if  A  is outside the interval [X(1),X(N)].
C              IERR = 2  if  B  is outside the interval [X(1),X(N)].
C              IERR = 3  if both of the above are true.  (Note that this
C                        means that either [A,B] contains data interval
C                        or the intervals do not intersect at all.)
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C                (VALUE will be zero in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C              IERR = -4  in case of an error return from DPCHID (which
C                         should never occur).
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DCHFIE, DPCHID, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820730  DATE WRITTEN
C   820804  Converted to SLATEC library version.
C   870707  Corrected XERROR calls for d.p. name(s).
C   870707  Corrected conversion to double precision.
C   870813  Minor cosmetic changes.
C   890206  Corrected XERROR calls.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   930503  Corrected to set VALUE=0 when IERR.lt.0.  (FNF)
C   930504  Changed DCHFIV to DCHFIE.  (FNF)
C***END PROLOGUE  DPCHIA
C
C  Programming notes:
C  1. The error flag from DPCHID is tested, because a logic flaw
C     could conceivably result in IERD=-4, which should be reported.
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, IERR
      DOUBLE PRECISION  X(*), F(INCFD,*), D(INCFD,*), A, B
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IA, IB, IERD, IL, IR
      DOUBLE PRECISION  VALUE, XA, XB, ZERO
      SAVE ZERO
      DOUBLE PRECISION  DCHFIE, DPCHID
C
C  INITIALIZE.
C
      DATA  ZERO /0.D0/
C***FIRST EXECUTABLE STATEMENT  DPCHIA
      VALUE = ZERO
C
C  VALIDITY-CHECK ARGUMENTS.
C
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
    5 CONTINUE
      SKIP = .TRUE.
      IERR = 0
      IF ( (A.LT.X(1)) .OR. (A.GT.X(N)) )  IERR = IERR + 1
      IF ( (B.LT.X(1)) .OR. (B.GT.X(N)) )  IERR = IERR + 2
C
C  COMPUTE INTEGRAL VALUE.
C
      IF (A .NE. B)  THEN
         XA = MIN (A, B)
         XB = MAX (A, B)
         IF (XB .LE. X(2))  THEN
C           INTERVAL IS TO LEFT OF X(2), SO USE FIRST CUBIC.
C                   ---------------------------------------
            VALUE = DCHFIE (X(1),X(2), F(1,1),F(1,2),
     +                                 D(1,1),D(1,2), A, B)
C                   ---------------------------------------
         ELSE IF (XA .GE. X(N-1))  THEN
C           INTERVAL IS TO RIGHT OF X(N-1), SO USE LAST CUBIC.
C                   ------------------------------------------
            VALUE = DCHFIE(X(N-1),X(N), F(1,N-1),F(1,N),
     +                                  D(1,N-1),D(1,N), A, B)
C                   ------------------------------------------
         ELSE
C           'NORMAL' CASE -- XA.LT.XB, XA.LT.X(N-1), XB.GT.X(2).
C      ......LOCATE IA AND IB SUCH THAT
C               X(IA-1).LT.XA.LE.X(IA).LE.X(IB).LE.XB.LE.X(IB+1)
            IA = 1
            DO 10  I = 1, N-1
               IF (XA .GT. X(I))  IA = I + 1
   10       CONTINUE
C             IA = 1 IMPLIES XA.LT.X(1) .  OTHERWISE,
C             IA IS LARGEST INDEX SUCH THAT X(IA-1).LT.XA,.
C
            IB = N
            DO 20  I = N, IA, -1
               IF (XB .LT. X(I))  IB = I - 1
   20       CONTINUE
C             IB = N IMPLIES XB.GT.X(N) .  OTHERWISE,
C             IB IS SMALLEST INDEX SUCH THAT XB.LT.X(IB+1) .
C
C     ......COMPUTE THE INTEGRAL.
            IF (IB .LT. IA)  THEN
C              THIS MEANS IB = IA-1 AND
C                 (A,B) IS A SUBSET OF (X(IB),X(IA)).
C                      -------------------------------------------
               VALUE = DCHFIE (X(IB),X(IA), F(1,IB),F(1,IA),
     +                                      D(1,IB),D(1,IA), A, B)
C                      -------------------------------------------
            ELSE
C
C              FIRST COMPUTE INTEGRAL OVER (X(IA),X(IB)).
C                (Case (IB .EQ. IA) is taken care of by initialization
C                 of VALUE to ZERO.)
               IF (IB .GT. IA)  THEN
C                         ---------------------------------------------
                  VALUE = DPCHID (N, X, F, D, INCFD, SKIP, IA, IB, IERD)
C                         ---------------------------------------------
                  IF (IERD .LT. 0)  GO TO 5004
               ENDIF
C
C              THEN ADD ON INTEGRAL OVER (XA,X(IA)).
               IF (XA .LT. X(IA))  THEN
                  IL = MAX(1, IA-1)
                  IR = IL + 1
C                                 -------------------------------------
                  VALUE = VALUE + DCHFIE (X(IL),X(IR), F(1,IL),F(1,IR),
     +                                      D(1,IL),D(1,IR), XA, X(IA))
C                                 -------------------------------------
               ENDIF
C
C              THEN ADD ON INTEGRAL OVER (X(IB),XB).
               IF (XB .GT. X(IB))  THEN
                  IR = MIN (IB+1, N)
                  IL = IR - 1
C                                 -------------------------------------
                  VALUE = VALUE + DCHFIE (X(IL),X(IR), F(1,IL),F(1,IR),
     +                                      D(1,IL),D(1,IR), X(IB), XB)
C                                 -------------------------------------
               ENDIF
C
C              FINALLY, ADJUST SIGN IF NECESSARY.
               IF (A .GT. B)  VALUE = -VALUE
            ENDIF
         ENDIF
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      DPCHIA = VALUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHIA',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      GO TO 5000
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHIA', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      GO TO 5000
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHIA',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      GO TO 5000
C
 5004 CONTINUE
C     TROUBLE IN DPCHID.  (SHOULD NEVER OCCUR.)
      IERR = -4
      CALL XERMSG ('SLATEC', 'DPCHIA', 'TROUBLE IN DPCHID', IERR, 1)
      GO TO 5000
      END
      SUBROUTINE DPCHIC (IC, VC, SWITCH, N, X, F, D, INCFD, WK, NWK,
     +   IERR)

c*********************************************************************72
c
cc DPCHIC sets derivatives for a monotone piecewise cubic Hermite interpolant.
c
C***BEGIN PROLOGUE  DPCHIC
C***PURPOSE  Set derivatives needed to determine a piecewise monotone
C            piecewise cubic Hermite interpolant to given data.
C            User control is available over boundary conditions and/or
C            treatment of points where monotonicity switches direction.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E1A
C***TYPE      DOUBLE PRECISION (PCHIC-S, DPCHIC-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION,
C             PCHIP, PIECEWISE CUBIC INTERPOLATION,
C             SHAPE-PRESERVING INTERPOLATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C         DPCHIC:  Piecewise Cubic Hermite Interpolation Coefficients.
C
C     Sets derivatives needed to determine a piecewise monotone piece-
C     wise cubic interpolant to the data given in X and F satisfying the
C     boundary conditions specified by IC and VC.
C
C     The treatment of points where monotonicity switches direction is
C     controlled by argument SWITCH.
C
C     To facilitate two-dimensional applications, includes an increment
C     between successive values of the F- and D-arrays.
C
C     The resulting piecewise cubic Hermite function may be evaluated
C     by DPCHFE or DPCHFD.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  IC(2), N, NWK, IERR
C        DOUBLE PRECISION  VC(2), SWITCH, X(N), F(INCFD,N), D(INCFD,N),
C                          WK(NWK)
C
C        CALL DPCHIC (IC, VC, SWITCH, N, X, F, D, INCFD, WK, NWK, IERR)
C
C   Parameters:
C
C     IC -- (input) integer array of length 2 specifying desired
C           boundary conditions:
C           IC(1) = IBEG, desired condition at beginning of data.
C           IC(2) = IEND, desired condition at end of data.
C
C           IBEG = 0  for the default boundary condition (the same as
C                     used by DPCHIM).
C           If IBEG.NE.0, then its sign indicates whether the boundary
C                     derivative is to be adjusted, if necessary, to be
C                     compatible with monotonicity:
C              IBEG.GT.0  if no adjustment is to be performed.
C              IBEG.LT.0  if the derivative is to be adjusted for
C                     monotonicity.
C
C           Allowable values for the magnitude of IBEG are:
C           IBEG = 1  if first derivative at X(1) is given in VC(1).
C           IBEG = 2  if second derivative at X(1) is given in VC(1).
C           IBEG = 3  to use the 3-point difference formula for D(1).
C                     (Reverts to the default b.c. if N.LT.3 .)
C           IBEG = 4  to use the 4-point difference formula for D(1).
C                     (Reverts to the default b.c. if N.LT.4 .)
C           IBEG = 5  to set D(1) so that the second derivative is con-
C              tinuous at X(2). (Reverts to the default b.c. if N.LT.4.)
C              This option is somewhat analogous to the "not a knot"
C              boundary condition provided by DPCHSP.
C
C          NOTES (IBEG):
C           1. An error return is taken if ABS(IBEG).GT.5 .
C           2. Only in case  IBEG.LE.0  is it guaranteed that the
C              interpolant will be monotonic in the first interval.
C              If the returned value of D(1) lies between zero and
C              3*SLOPE(1), the interpolant will be monotonic.  This
C              is **NOT** checked if IBEG.GT.0 .
C           3. If IBEG.LT.0 and D(1) had to be changed to achieve mono-
C              tonicity, a warning error is returned.
C
C           IEND may take on the same values as IBEG, but applied to
C           derivative at X(N).  In case IEND = 1 or 2, the value is
C           given in VC(2).
C
C          NOTES (IEND):
C           1. An error return is taken if ABS(IEND).GT.5 .
C           2. Only in case  IEND.LE.0  is it guaranteed that the
C              interpolant will be monotonic in the last interval.
C              If the returned value of D(1+(N-1)*INCFD) lies between
C              zero and 3*SLOPE(N-1), the interpolant will be monotonic.
C              This is **NOT** checked if IEND.GT.0 .
C           3. If IEND.LT.0 and D(1+(N-1)*INCFD) had to be changed to
C              achieve monotonicity, a warning error is returned.
C
C     VC -- (input) real*8 array of length 2 specifying desired boundary
C           values, as indicated above.
C           VC(1) need be set only if IC(1) = 1 or 2 .
C           VC(2) need be set only if IC(2) = 1 or 2 .
C
C     SWITCH -- (input) indicates desired treatment of points where
C           direction of monotonicity switches:
C           Set SWITCH to zero if interpolant is required to be mono-
C           tonic in each interval, regardless of monotonicity of data.
C             NOTES:
C              1. This will cause D to be set to zero at all switch
C                 points, thus forcing extrema there.
C              2. The result of using this option with the default boun-
C                 dary conditions will be identical to using DPCHIM, but
C                 will generally cost more compute time.
C                 This option is provided only to facilitate comparison
C                 of different switch and/or boundary conditions.
C           Set SWITCH nonzero to use a formula based on the 3-point
C              difference formula in the vicinity of switch points.
C           If SWITCH is positive, the interpolant on each interval
C              containing an extremum is controlled to not deviate from
C              the data by more than SWITCH*DFLOC, where DFLOC is the
C              maximum of the change of F on this interval and its two
C              immediate neighbors.
C           If SWITCH is negative, no such control is to be imposed.
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real*8 array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real*8 array of dependent variable values to be
C           interpolated.  F(1+(I-1)*INCFD) is value corresponding to
C           X(I).
C
C     D -- (output) real*8 array of derivative values at the data
C           points.  These values will determine a monotone cubic
C           Hermite function on each subinterval on which the data
C           are monotonic, except possibly adjacent to switches in
C           monotonicity. The value corresponding to X(I) is stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C           No other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in F and D.
C           This argument is provided primarily for 2-D applications.
C           (Error return if  INCFD.LT.1 .)
C
C     WK -- (scratch) real*8 array of working storage.  The user may
C           wish to know that the returned values are:
C              WK(I)     = H(I)     = X(I+1) - X(I) ;
C              WK(N-1+I) = SLOPE(I) = (F(1,I+1) - F(1,I)) / H(I)
C           for  I = 1(1)N-1.
C
C     NWK -- (input) length of work array.
C           (Error return if  NWK.LT.2*(N-1) .)
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning errors:
C              IERR = 1  if IBEG.LT.0 and D(1) had to be adjusted for
C                        monotonicity.
C              IERR = 2  if IEND.LT.0 and D(1+(N-1)*INCFD) had to be
C                        adjusted for monotonicity.
C              IERR = 3  if both of the above are true.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if ABS(IBEG).GT.5 .
C              IERR = -5  if ABS(IEND).GT.5 .
C              IERR = -6  if both of the above are true.
C              IERR = -7  if NWK.LT.2*(N-1) .
C             (The D-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  1. F. N. Fritsch, Piecewise Cubic Hermite Interpolation
C                 Package, Report UCRL-87285, Lawrence Livermore Natio-
C                 nal Laboratory, July 1982.  [Poster presented at the
C                 SIAM 30th Anniversary Meeting, 19-23 July 1982.]
C               2. F. N. Fritsch and J. Butland, A method for construc-
C                 ting local monotone piecewise cubic interpolants, SIAM
C                 Journal on Scientific and Statistical Computing 5, 2
C                 (June 1984), pp. 300-304.
C               3. F. N. Fritsch and R. E. Carlson, Monotone piecewise
C                 cubic interpolation, SIAM Journal on Numerical Ana-
C                 lysis 17, 2 (April 1980), pp. 238-246.
C***ROUTINES CALLED  DPCHCE, DPCHCI, DPCHCS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820218  DATE WRITTEN
C   820804  Converted to SLATEC library version.
C   870707  Corrected XERROR calls for d.p. name(s).
C   870813  Updated Reference 2.
C   890206  Corrected XERROR calls.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920429  Revised format and order of references.  (WRB,FNF)
C***END PROLOGUE  DPCHIC
C  Programming notes:
C
C     To produce a single precision version, simply:
C        a. Change DPCHIC to PCHIC wherever it occurs,
C        b. Change DPCHCE to PCHCE wherever it occurs,
C        c. Change DPCHCI to PCHCI wherever it occurs,
C        d. Change DPCHCS to PCHCS wherever it occurs,
C        e. Change the double precision declarations to real, and
C        f. Change the constant  ZERO  to single precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  IC(2), N, INCFD, NWK, IERR
      DOUBLE PRECISION  VC(2), SWITCH, X(*), F(INCFD,*), D(INCFD,*),
     * WK(NWK)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IBEG, IEND, NLESS1
      DOUBLE PRECISION  ZERO
      SAVE ZERO
      DATA  ZERO /0.D0/
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  DPCHIC
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
      IBEG = IC(1)
      IEND = IC(2)
      IERR = 0
      IF (ABS(IBEG) .GT. 5)  IERR = IERR - 1
      IF (ABS(IEND) .GT. 5)  IERR = IERR - 2
      IF (IERR .LT. 0)  GO TO 5004
C
C  FUNCTION DEFINITION IS OK -- GO ON.
C
      NLESS1 = N - 1
      IF ( NWK .LT. 2*NLESS1 )  GO TO 5007
C
C  SET UP H AND SLOPE ARRAYS.
C
      DO 20  I = 1, NLESS1
         WK(I) = X(I+1) - X(I)
         WK(NLESS1+I) = (F(1,I+1) - F(1,I)) / WK(I)
   20 CONTINUE
C
C  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION.
C
      IF (NLESS1 .GT. 1)  GO TO 1000
      D(1,1) = WK(2)
      D(1,N) = WK(2)
      GO TO 3000
C
C  NORMAL CASE  (N .GE. 3) .
C
 1000 CONTINUE
C
C  SET INTERIOR DERIVATIVES AND DEFAULT END CONDITIONS.
C
C     --------------------------------------
      CALL DPCHCI (N, WK(1), WK(N), D, INCFD)
C     --------------------------------------
C
C  SET DERIVATIVES AT POINTS WHERE MONOTONICITY SWITCHES DIRECTION.
C
      IF (SWITCH .EQ. ZERO)  GO TO 3000
C     ----------------------------------------------------
      CALL DPCHCS (SWITCH, N, WK(1), WK(N), D, INCFD, IERR)
C     ----------------------------------------------------
      IF (IERR .NE. 0)  GO TO 5008
C
C  SET END CONDITIONS.
C
 3000 CONTINUE
      IF ( (IBEG.EQ.0) .AND. (IEND.EQ.0) )  GO TO 5000
C     -------------------------------------------------------
      CALL DPCHCE (IC, VC, N, X, WK(1), WK(N), D, INCFD, IERR)
C     -------------------------------------------------------
      IF (IERR .LT. 0)  GO TO 5009
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHIC',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHIC', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHIC',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      RETURN
C
 5004 CONTINUE
C     IC OUT OF RANGE RETURN.
      IERR = IERR - 3
      CALL XERMSG ('SLATEC', 'DPCHIC', 'IC OUT OF RANGE', IERR, 1)
      RETURN
C
 5007 CONTINUE
C     NWK .LT. 2*(N-1)  RETURN.
      IERR = -7
      CALL XERMSG ('SLATEC', 'DPCHIC', 'WORK ARRAY TOO SMALL', IERR, 1)
      RETURN
C
 5008 CONTINUE
C     ERROR RETURN FROM DPCHCS.
      IERR = -8
      CALL XERMSG ('SLATEC', 'DPCHIC', 'ERROR RETURN FROM DPCHCS',
     +   IERR, 1)
      RETURN
C
 5009 CONTINUE
C     ERROR RETURN FROM DPCHCE.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -9
      CALL XERMSG ('SLATEC', 'DPCHIC', 'ERROR RETURN FROM DPCHCE',
     +   IERR, 1)
      RETURN
      END
      DOUBLE PRECISION FUNCTION DPCHID (N, X, F, D, INCFD, SKIP, IA, IB,
     +   IERR)

c*********************************************************************72
c
cc DPCHID: integral of a piecewise cubic Hermite function between data points.
c
C***BEGIN PROLOGUE  DPCHID
C***PURPOSE  Evaluate the definite integral of a piecewise cubic
C            Hermite function over an interval whose endpoints are data
C            points.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3, H2A1B2
C***TYPE      DOUBLE PRECISION (PCHID-S, DPCHID-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, NUMERICAL INTEGRATION, PCHIP,
C             QUADRATURE
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          DPCHID:  Piecewise Cubic Hermite Integrator, Data limits
C
C     Evaluates the definite integral of the cubic Hermite function
C     defined by  N, X, F, D  over the interval [X(IA), X(IB)].
C
C     To provide compatibility with DPCHIM and DPCHIC, includes an
C     increment between successive values of the F- and D-arrays.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, IA, IB, IERR
C        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N)
C        LOGICAL  SKIP
C
C        VALUE = DPCHID (N, X, F, D, INCFD, SKIP, IA, IB, IERR)
C
C   Parameters:
C
C     VALUE -- (output) value of the requested integral.
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real*8 array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real*8 array of function values.  F(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     D -- (input) real*8 array of derivative values.  D(1+(I-1)*INCFD)
C           is the value corresponding to X(I).
C
C     INCFD -- (input) increment between successive values in F and D.
C           (Error return if  INCFD.LT.1 .)
C
C     SKIP -- (input/output) logical variable which should be set to
C           .TRUE. if the user wishes to skip checks for validity of
C           preceding parameters, or to .FALSE. otherwise.
C           This will save time in case these checks have already
C           been performed (say, in DPCHIM or DPCHIC).
C           SKIP will be set to .TRUE. on return with IERR = 0 or -4.
C
C     IA,IB -- (input) indices in X-array for the limits of integration.
C           both must be in the range [1,N].  (Error return if not.)
C           No restrictions on their relative values.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if IA or IB is out of range.
C                (VALUE will be zero in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820723  DATE WRITTEN
C   820804  Converted to SLATEC library version.
C   870707  Corrected XERROR calls for d.p. name(s).
C   870813  Minor cosmetic changes.
C   890206  Corrected XERROR calls.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   930504  Corrected to set VALUE=0 when IERR.ne.0.  (FNF)
C***END PROLOGUE  DPCHID
C
C  Programming notes:
C  1. This routine uses a special formula that is valid only for
C     integrals whose limits coincide with data values.  This is
C     mathematically equivalent to, but much more efficient than,
C     calls to DCHFIE.
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, IA, IB, IERR
      DOUBLE PRECISION  X(*), F(INCFD,*), D(INCFD,*)
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IUP, LOW
      DOUBLE PRECISION  H, HALF, SIX, SUM, VALUE, ZERO
      SAVE ZERO, HALF, SIX
C
C  INITIALIZE.
C
      DATA  ZERO /0.D0/,  HALF/.5D0/, SIX/6.D0/
C***FIRST EXECUTABLE STATEMENT  DPCHID
      VALUE = ZERO
C
C  VALIDITY-CHECK ARGUMENTS.
C
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
    5 CONTINUE
      SKIP = .TRUE.
      IF ((IA.LT.1) .OR. (IA.GT.N))  GO TO 5004
      IF ((IB.LT.1) .OR. (IB.GT.N))  GO TO 5004
      IERR = 0
C
C  COMPUTE INTEGRAL VALUE.
C
      IF (IA .NE. IB)  THEN
         LOW = MIN(IA, IB)
         IUP = MAX(IA, IB) - 1
         SUM = ZERO
         DO 10  I = LOW, IUP
            H = X(I+1) - X(I)
            SUM = SUM + H*( (F(1,I) + F(1,I+1)) +
     *                      (D(1,I) - D(1,I+1))*(H/SIX) )
   10    CONTINUE
         VALUE = HALF * SUM
         IF (IA .GT. IB)  VALUE = -VALUE
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      DPCHID = VALUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHID',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      GO TO 5000
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHID', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      GO TO 5000
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHID',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      GO TO 5000
C
 5004 CONTINUE
C     IA OR IB OUT OF RANGE RETURN.
      IERR = -4
      CALL XERMSG ('SLATEC', 'DPCHID', 'IA OR IB OUT OF RANGE', IERR,
     +   1)
      GO TO 5000
      END
      SUBROUTINE DPCHIM (N, X, F, D, INCFD, IERR)

c*********************************************************************72
c
cc DPCHIM sets derivatives for a monotone piecewise cubic Hermite interpolant.
c
C***BEGIN PROLOGUE  DPCHIM
C***PURPOSE  Set derivatives needed to determine a monotone piecewise
C            cubic Hermite interpolant to given data.  Boundary values
C            are provided which are compatible with monotonicity.  The
C            interpolant will have an extremum at each point where mono-
C            tonicity switches direction.  (See DPCHIC if user control
C            is desired over boundary or switch conditions.)
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E1A
C***TYPE      DOUBLE PRECISION (PCHIM-S, DPCHIM-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION,
C             PCHIP, PIECEWISE CUBIC INTERPOLATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          DPCHIM:  Piecewise Cubic Hermite Interpolation to
C                  Monotone data.
C
C     Sets derivatives needed to determine a monotone piecewise cubic
C     Hermite interpolant to the data given in X and F.
C
C     Default boundary conditions are provided which are compatible
C     with monotonicity.  (See DPCHIC if user control of boundary con-
C     ditions is desired.)
C
C     If the data are only piecewise monotonic, the interpolant will
C     have an extremum at each point where monotonicity switches direc-
C     tion.  (See DPCHIC if user control is desired in such cases.)
C
C     To facilitate two-dimensional applications, includes an increment
C     between successive values of the F- and D-arrays.
C
C     The resulting piecewise cubic Hermite function may be evaluated
C     by DPCHFE or DPCHFD.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, IERR
C        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N)
C
C        CALL  DPCHIM (N, X, F, D, INCFD, IERR)
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C           If N=2, simply does linear interpolation.
C
C     X -- (input) real*8 array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real*8 array of dependent variable values to be
C           interpolated.  F(1+(I-1)*INCFD) is value corresponding to
C           X(I).  DPCHIM is designed for monotonic data, but it will
C           work for any F-array.  It will force extrema at points where
C           monotonicity switches direction.  If some other treatment of
C           switch points is desired, DPCHIC should be used instead.
C                                     -----
C     D -- (output) real*8 array of derivative values at the data
C           points.  If the data are monotonic, these values will
C           determine a monotone cubic Hermite function.
C           The value corresponding to X(I) is stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C           No other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in F and D.
C           This argument is provided primarily for 2-D applications.
C           (Error return if  INCFD.LT.1 .)
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  means that IERR switches in the direction
C                 of monotonicity were detected.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C             (The D-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  1. F. N. Fritsch and J. Butland, A method for construc-
C                 ting local monotone piecewise cubic interpolants, SIAM
C                 Journal on Scientific and Statistical Computing 5, 2
C                 (June 1984), pp. 300-304.
C               2. F. N. Fritsch and R. E. Carlson, Monotone piecewise
C                 cubic interpolation, SIAM Journal on Numerical Ana-
C                 lysis 17, 2 (April 1980), pp. 238-246.
C***ROUTINES CALLED  DPCHST, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811103  DATE WRITTEN
C   820201  1. Introduced  DPCHST  to reduce possible over/under-
C             flow problems.
C           2. Rearranged derivative formula for same reason.
C   820602  1. Modified end conditions to be continuous functions
C             of data when monotonicity switches in next interval.
C           2. Modified formulas so end conditions are less prone
C             of over/underflow problems.
C   820803  Minor cosmetic changes for release 1.
C   870707  Corrected XERROR calls for d.p. name(s).
C   870813  Updated Reference 1.
C   890206  Corrected XERROR calls.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920429  Revised format and order of references.  (WRB,FNF)
C***END PROLOGUE  DPCHIM
C  Programming notes:
C
C     1. The function  DPCHST(ARG1,ARG2)  is assumed to return zero if
C        either argument is zero, +1 if they are of the same sign, and
C        -1 if they are of opposite sign.
C     2. To produce a single precision version, simply:
C        a. Change DPCHIM to PCHIM wherever it occurs,
C        b. Change DPCHST to PCHST wherever it occurs,
C        c. Change all references to the Fortran intrinsics to their
C           single precision equivalents,
C        d. Change the double precision declarations to real, and
C        e. Change the constants ZERO and THREE to single precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, IERR
      DOUBLE PRECISION  X(*), F(INCFD,*), D(INCFD,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, NLESS1
      DOUBLE PRECISION  DEL1, DEL2, DMAX, DMIN, DRAT1, DRAT2, DSAVE,
     *      H1, H2, HSUM, HSUMT3, THREE, W1, W2, ZERO
      SAVE ZERO, THREE
      DOUBLE PRECISION  DPCHST
      DATA  ZERO /0.D0/, THREE/3.D0/
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  DPCHIM
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
      IERR = 0
      NLESS1 = N - 1
      H1 = X(2) - X(1)
      DEL1 = (F(1,2) - F(1,1))/H1
      DSAVE = DEL1
C
C  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION.
C
      IF (NLESS1 .GT. 1)  GO TO 10
      D(1,1) = DEL1
      D(1,N) = DEL1
      GO TO 5000
C
C  NORMAL CASE  (N .GE. 3).
C
   10 CONTINUE
      H2 = X(3) - X(2)
      DEL2 = (F(1,3) - F(1,2))/H2
C
C  SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE
C     SHAPE-PRESERVING.
C
      HSUM = H1 + H2
      W1 = (H1 + HSUM)/HSUM
      W2 = -H1/HSUM
      D(1,1) = W1*DEL1 + W2*DEL2
      IF ( DPCHST(D(1,1),DEL1) .LE. ZERO)  THEN
         D(1,1) = ZERO
      ELSE IF ( DPCHST(DEL1,DEL2) .LT. ZERO)  THEN
C        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES.
         DMAX = THREE*DEL1
         IF (ABS(D(1,1)) .GT. ABS(DMAX))  D(1,1) = DMAX
      ENDIF
C
C  LOOP THROUGH INTERIOR POINTS.
C
      DO 50  I = 2, NLESS1
         IF (I .EQ. 2)  GO TO 40
C
         H1 = H2
         H2 = X(I+1) - X(I)
         HSUM = H1 + H2
         DEL1 = DEL2
         DEL2 = (F(1,I+1) - F(1,I))/H2
   40    CONTINUE
C
C        SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC.
C
         D(1,I) = ZERO
         IF ( DPCHST(DEL1,DEL2) )  42, 41, 45
C
C        COUNT NUMBER OF CHANGES IN DIRECTION OF MONOTONICITY.
C
   41    CONTINUE
         IF (DEL2 .EQ. ZERO)  GO TO 50
         IF ( DPCHST(DSAVE,DEL2) .LT. ZERO)  IERR = IERR + 1
         DSAVE = DEL2
         GO TO 50
C
   42    CONTINUE
         IERR = IERR + 1
         DSAVE = DEL2
         GO TO 50
C
C        USE BRODLIE MODIFICATION OF BUTLAND FORMULA.
C
   45    CONTINUE
         HSUMT3 = HSUM+HSUM+HSUM
         W1 = (HSUM + H1)/HSUMT3
         W2 = (HSUM + H2)/HSUMT3
         DMAX = MAX( ABS(DEL1), ABS(DEL2) )
         DMIN = MIN( ABS(DEL1), ABS(DEL2) )
         DRAT1 = DEL1/DMAX
         DRAT2 = DEL2/DMAX
         D(1,I) = DMIN/(W1*DRAT1 + W2*DRAT2)
C
   50 CONTINUE
C
C  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE
C     SHAPE-PRESERVING.
C
      W1 = -H2/HSUM
      W2 = (H2 + HSUM)/HSUM
      D(1,N) = W1*DEL1 + W2*DEL2
      IF ( DPCHST(D(1,N),DEL2) .LE. ZERO)  THEN
         D(1,N) = ZERO
      ELSE IF ( DPCHST(DEL1,DEL2) .LT. ZERO)  THEN
C        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES.
         DMAX = THREE*DEL2
         IF (ABS(D(1,N)) .GT. ABS(DMAX))  D(1,N) = DMAX
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHIM',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHIM', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHIM',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      RETURN
      END
      SUBROUTINE DPCHKT (N, X, KNOTYP, T)

c*********************************************************************72
c
cc DPCHKT computes the B-spline knot sequence for DPCHBS.
c
C***BEGIN PROLOGUE  DPCHKT
C***SUBSIDIARY
C***PURPOSE  Compute B-spline knot sequence for DPCHBS.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3
C***TYPE      DOUBLE PRECISION (PCHKT-S, DPCHKT-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C     Set a knot sequence for the B-spline representation of a PCH
C     function with breakpoints X.  All knots will be at least double.
C     Endknots are set as:
C        (1) quadruple knots at endpoints if KNOTYP=0;
C        (2) extrapolate the length of end interval if KNOTYP=1;
C        (3) periodic if KNOTYP=2.
C
C  Input arguments:  N, X, KNOTYP.
C  Output arguments:  T.
C
C  Restrictions/assumptions:
C     1. N.GE.2 .  (not checked)
C     2. X(i).LT.X(i+1), i=1,...,N .  (not checked)
C     3. 0.LE.KNOTYP.LE.2 .  (Acts like KNOTYP=0 for any other value.)
C
C***SEE ALSO  DPCHBS
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   870701  DATE WRITTEN
C   900405  Converted Fortran to upper case.
C   900410  Converted prologue to SLATEC 4.0 format.
C   900410  Minor cosmetic changes.
C   900430  Produced double precision version.
C   930514  Changed NKNOTS from an output to an input variable.  (FNF)
C   930604  Removed unused variable NKNOTS from argument list.  (FNF)
C***END PROLOGUE  DPCHKT
C
C*Internal Notes:
C
C  Since this is subsidiary to DPCHBS, which validates its input before
C  calling, it is unnecessary for such validation to be done here.
C
C**End
C
C  Declare arguments.
C
      INTEGER  N, KNOTYP
      DOUBLE PRECISION  X(*), T(*)
C
C  Declare local variables.
C
      INTEGER  J, K, NDIM
      DOUBLE PRECISION  HBEG, HEND
C***FIRST EXECUTABLE STATEMENT  DPCHKT
C
C  Initialize.
C
      NDIM = 2*N
C
C  Set interior knots.
C
      J = 1
      DO 20  K = 1, N
         J = J + 2
         T(J) = X(K)
         T(J+1) = T(J)
   20 CONTINUE
C     Assertion:  At this point T(3),...,T(NDIM+2) have been set and
C                 J=NDIM+1.
C
C  Set end knots according to KNOTYP.
C
      HBEG = X(2) - X(1)
      HEND = X(N) - X(N-1)
      IF (KNOTYP.EQ.1 )  THEN
C          Extrapolate.
         T(2) = X(1) - HBEG
         T(NDIM+3) = X(N) + HEND
      ELSE IF ( KNOTYP.EQ.2 )  THEN
C          Periodic.
         T(2) = X(1) - HEND
         T(NDIM+3) = X(N) + HBEG
      ELSE
C          Quadruple end knots.
         T(2) = X(1)
         T(NDIM+3) = X(N)
      ENDIF
      T(1) = T(2)
      T(NDIM+4) = T(NDIM+3)
C
C  Terminate.
C
      RETURN
      END
      SUBROUTINE DPCHQ1 (LUN, KPRINT, IPASS)

c*********************************************************************72
c
cc DPCHQ1: Test the PCHIP evaluators DCHFDV, DCHFEV, DPCHFD, DPCHFE.
c
C***BEGIN PROLOGUE  DPCHQ1
C***PURPOSE  Test the PCHIP evaluators DCHFDV, DCHFEV, DPCHFD, DPCHFE.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (PCHQK1-S, DPCHQ1-D)
C***KEYWORDS  PCHIP EVALUATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C             DPCHIP QUICK CHECK NUMBER 1
C
C     TESTS THE EVALUATORS:  DCHFDV, DCHFEV, DPCHFD, DPCHFE.
C *Usage:
C
C        INTEGER  LUN, KPRINT, IPASS
C
C        CALL DPCHQ1 (LUN, KPRINT, IPASS)
C
C *Arguments:
C
C     LUN   :IN  is the unit number to which output is to be written.
C
C     KPRINT:IN  controls the amount of output, as specified in the
C                SLATEC Guidelines.
C
C     IPASS:OUT  will contain a pass/fail flag.  IPASS=1 is good.
C                IPASS=0 indicates one or more tests failed.
C
C *Description:
C
C   This routine carries out three tests of the PCH evaluators:
C     DEVCHK tests the single-cubic evaluators.
C     DEVPCK tests the full PCH evaluators.
C     DEVERK exercises the error returns in all evaluators.
C
C***ROUTINES CALLED  DEVCHK, DEVERK, DEVPCK
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   890306  Changed IPASS to the more accurate name IFAIL.  (FNF)
C   890307  Removed conditional on call to DEVERK.
C   890706  Cosmetic changes to prologue.  (WRB)
C   891004  Correction in prologue.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900309  Added DEVERK to list of routines called.  (FNF)
C   900314  Improved some output formats.
C   900315  Revised prologue and improved some output formats.  (FNF)
C   900316  Additional minor cosmetic changes.  (FNF)
C   900321  Removed IFAIL from call sequence for SLATEC standards and
C           made miscellaneous cosmetic changes.  (FNF)
C   930317  Improved output formats.  (FNF)
C***END PROLOGUE  DPCHQ1
C
C  Declare arguments.
C
      INTEGER  LUN, KPRINT, IPASS
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I1, I2, I3, I4, I5, I6, I7, I8, I9, IFAIL, NPTS
      DOUBLE PRECISION  WORK (4000)
      LOGICAL  FAIL
C
C***FIRST EXECUTABLE STATEMENT  DPCHQ1
      IF (KPRINT .GE. 2)  WRITE (LUN, 1000) KPRINT
C
C  TEST DCHFDV AND DCHFEV.
C
      IFAIL = 0
      NPTS = 1000
      I1 = 1  + NPTS
      I2 = I1 + NPTS
      I3 = I2 + NPTS
      CALL DEVCHK (LUN, KPRINT, NPTS, WORK(1), WORK(I1), WORK(I2),
     *                                          WORK(I3), FAIL)
      IF (FAIL)  IFAIL = IFAIL + 1
C
C  TEST DPCHFD AND DPCHFE.
C
      I1 = 1  +  10
      I2 = I1 +  10
      I3 = I2 + 100
      I4 = I3 + 100
      I5 = I4 + 100
      I6 = I5 +  51
      I7 = I6 +  51
      I8 = I7 +  51
      I9 = I8 +  51
      CALL DEVPCK (LUN, KPRINT, WORK(1), WORK(I1), WORK(I2), WORK(I3),
     *             WORK(I4), WORK(I5), WORK(I6), WORK(I7), WORK(I8),
     *             WORK(I9), FAIL)
      IF (FAIL)  IFAIL = IFAIL + 2
C
C  TEST ERROR RETURNS.
C
      CALL DEVERK (LUN, KPRINT, FAIL)
      IF (FAIL)  IFAIL = IFAIL + 4
C
C  PRINT SUMMARY AND TERMINATE.
C     At this point, IFAIL has the following value:
C        IFAIL = 0  IF ALL TESTS PASSED.
C        IFAIL BETWEEN 1 AND 7 IS THE SUM OF:
C           IFAIL=1  IF SINGLE CUBIC  TEST FAILED. (SEE DEVCHK OUTPUT.)
C           IFAIL=2  IF DPCHFD/DPCHFE TEST FAILED. (SEE DEVPCK OUTPUT.)
C           IFAIL=4  IF ERROR RETURN  TEST FAILED. (SEE DEVERK OUTPUT.)
C
      IF ((KPRINT.GE.2).AND.(IFAIL.NE.0))  WRITE (LUN, 3001)  IFAIL
C
      IF (IFAIL.EQ.0)  THEN
         IPASS = 1
         IF (KPRINT.GE.2) WRITE(LUN,99998)
      ELSE
         IPASS = 0
         IF (KPRINT.GE.1) WRITE(LUN,99999)
      ENDIF
C
      RETURN
C
C  FORMATS.
C
 1000 FORMAT ('1'/' ------------ DPCHIP QUICK CHECK OUTPUT',
     *        ' ------------' //20X,'( KPRINT =',I2,' )')
 3001 FORMAT (/' *** TROUBLE ***',I5,' EVALUATION TESTS FAILED.')
99998 FORMAT (/' ------------ DPCHIP PASSED  ALL EVALUATION TESTS',
     *        ' ------------')
99999 FORMAT (/' ************ DPCHIP FAILED SOME EVALUATION TESTS',
     *        ' ************')
C------------- LAST LINE OF DPCHQ1 FOLLOWS -----------------------------
      END
      SUBROUTINE DPCHQ2 (LUN, KPRINT, IPASS)

c*********************************************************************72
c
cc DPCHQ2 tests the PCHIP integrators DPCHIA and DPCHID.
c
C***BEGIN PROLOGUE  DPCHQ2
C***PURPOSE  Test the PCHIP integrators DPCHIA and DPCHID.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (PCHQK2-S, DPCHQ2-D)
C***KEYWORDS  PCHIP INTEGRATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C             DPCHIP QUICK CHECK NUMBER 2
C
C     TESTS THE INTEGRATORS:  DPCHIA, DPCHID.
C *Usage:
C
C        INTEGER  LUN, KPRINT, IPASS
C
C        CALL DPCHQ2 (LUN, KPRINT, IPASS)
C
C *Arguments:
C
C     LUN   :IN  is the unit number to which output is to be written.
C
C     KPRINT:IN  controls the amount of output, as specified in the
C                SLATEC Guidelines.
C
C     IPASS:OUT  will contain a pass/fail flag.  IPASS=1 is good.
C                IPASS=0 indicates one or more tests failed.
C
C *Description:
C
C   This routine constructs data from a cubic, integrates it with DPCHIA
C   and compares the results with the correct answer.
C   Since DPCHIA calls DPCHID, this tests both integrators.
C
C***ROUTINES CALLED  D1MACH, DPCHIA
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   890306  Changed IPASS to the more accurate name IFAIL.  (FNF)
C   890316  1. Removed IMPLICIT statement.                  (FNF)
C           2. Eliminated unnecessary variable N1.          (FNF)
C           3. Miscellaneous cosmetic changes.              (FNF)
C   891004  Cosmetic changes to prologue.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900314  Improved some output formats.  (FNF)
C   900315  Revised prologue and improved some output formats.  (FNF)
C   900316  Additional minor cosmetic changes.  (FNF)
C   900321  Removed IFAIL from call sequence for SLATEC standards and
C           made miscellaneous cosmetic changes.  (FNF)
C   900323  Corrected list of routines called.  (FNF)
C   901130  Added 1P's to formats; changed to allow KPRINT.gt.3.  (FNF)
C   910708  Minor modifications in use of KPRINT.  (WRB)
C   930317  Improved output formats.  (FNF)
C***END PROLOGUE  DPCHQ2
C
C  Declare arguments.
C
      INTEGER  LUN, KPRINT, IPASS
C
C  DECLARE VARIABLES.
C
      INTEGER  I, IEREXP(17), IERR, IFAIL, N, NPAIRS
      DOUBLE PRECISION
     *      A(17), B(17), CALC, D(7), ERRMAX, ERROR, F(7), MACHEP,
     *      ONE, THREE, THRQTR, TOL, TRUE, TWO, X(7)
      LOGICAL  FAIL, SKIP
C
C  DECLARE EXTERNALS.
C
      DOUBLE PRECISION  DPCHIA, D1MACH
C
C  DEFINE TEST FUNCTIONS.
C
      DOUBLE PRECISION AX, FCN, DERIV, ANTDER
      FCN(AX) = THREE*AX*AX*(AX-TWO)
      DERIV(AX) = THREE*AX*(TWO*(AX-TWO) + AX)
      ANTDER(AX) = AX**3 * (THRQTR*AX - TWO)
C
C  INITIALIZE.
C
      DATA  THRQTR /0.75D0/,  ONE /1.D0/,  TWO /2.D0/,  THREE /3.D0/
      DATA  N /7/
      DATA  X /-4.D0, -2.D0, -0.9D0, 0.D0, 0.9D0, 2.D0, 4.D0/
      DATA  NPAIRS /17/
      DATA  A /-3.0D0, 3.0D0,-0.5D0,-0.5D0,-0.5D0,-4.0D0,-4.0D0, 3.0D0,
     *  -5.0D0,-5.0D0,-6.0D0, 6.0D0,-1.5D0,-1.5D0,-3.0D0, 3.0D0, 0.5D0/
      DATA  B / 3.0D0,-3.0D0, 1.0D0, 2.0D0, 5.0D0,-0.5D0, 4.0D0, 5.0D0,
     *  -3.0D0, 5.0D0,-5.0D0, 5.0D0,-0.5D0,-1.0D0,-2.5D0, 3.5D0, 0.5D0/
      DATA  IEREXP /0,0,0,0,2,0,0,2,1,3,3,3,0,0,0,0,0/
C
C  SET PASS/FAIL TOLERANCE.
C
C***FIRST EXECUTABLE STATEMENT  DPCHQ2
      MACHEP = D1MACH(4)
      TOL = 100.D0*MACHEP
C
C  SET UP PCH FUNCTION DEFINITION.
C
      DO 10  I = 1, N
         F(I) =   FCN(X(I))
         D(I) = DERIV(X(I))
   10 CONTINUE
C
      IF (KPRINT .GE. 3)  WRITE (LUN, 1000)
      IF (KPRINT .GE. 2)  WRITE (LUN, 1001)
      IF (KPRINT .GE. 3)  WRITE (LUN, 1002)  (X(I), F(I), D(I), I=1,N)
C
C  LOOP OVER (A,B)-PAIRS.
C
      IF (KPRINT .GE. 3)  WRITE (LUN, 2000)
C
      IFAIL = 0
C
      SKIP = .FALSE.
      DO 20  I = 1, NPAIRS
C               ---------------------------------------------
         CALC = DPCHIA (N, X, F, D, 1, SKIP, A(I), B(I), IERR)
C               ---------------------------------------------
         IF (IERR .GE. 0)  THEN
            FAIL = IERR .NE. IEREXP(I)
            TRUE = ANTDER(B(I)) - ANTDER(A(I))
            ERROR = CALC - TRUE
            IF (KPRINT .GE. 3)  THEN
               IF (FAIL)  THEN
                 WRITE (LUN, 2001) A(I), B(I), IERR, TRUE, CALC, ERROR,
     *                                          IEREXP(I)
               ELSE
                 WRITE (LUN, 2002) A(I), B(I), IERR, TRUE, CALC, ERROR
               ENDIF
            ENDIF
C
            ERROR = ABS(ERROR) / MAX(ONE, ABS(TRUE))
            IF (FAIL .OR. (ERROR.GT.TOL))  IFAIL = IFAIL + 1
            IF (I .EQ. 1)  THEN
               ERRMAX = ERROR
            ELSE
               ERRMAX = MAX(ERRMAX, ERROR)
            ENDIF
         ELSE
            IF (KPRINT .GE. 3)  WRITE (LUN, 2002)  A(I), B(I), IERR
            IFAIL = IFAIL + 1
         ENDIF
   20 CONTINUE
C
C  PRINT SUMMARY.
C
      IF (KPRINT .GE. 2)  THEN
         WRITE (LUN, 2003)  ERRMAX, TOL
         IF (IFAIL .NE. 0)  WRITE (LUN, 3001)  IFAIL
      ENDIF
C
C  TERMINATE.
C
      IF (IFAIL.EQ.0)  THEN
         IPASS = 1
         IF (KPRINT.GE.2) WRITE(LUN,99998)
      ELSE
         IPASS = 0
         IF (KPRINT.GE.1) WRITE(LUN,99999)
      ENDIF
C
      RETURN
C
C  FORMATS.
C
 1000 FORMAT ('1'//10X,'TEST DPCHIP INTEGRATORS')
 1001 FORMAT (//10X,'DPCHQ2 RESULTS'/10X,'--------------')
 1002 FORMAT (// 5X,'DATA:' //11X,'X',9X,'F',9X,'D' /(5X,3F10.3) )
 2000 FORMAT (// 5X,'TEST RESULTS:'
     *        //'    A     B    ERR     TRUE',16X,'CALC',15X,'ERROR')
 2001 FORMAT (2F6.1,I5,1P,2D20.10,D15.5,'  (',I1,') *****' )
 2002 FORMAT (2F6.1,I5,1P,2D20.10,D15.5)
 2003 FORMAT (/'  MAXIMUM RELATIVE ERROR IS:',1P,D15.5,
     *                       ',   TOLERANCE:',1P,D15.5)
 3001 FORMAT (/' *** TROUBLE ***',I5,' INTEGRATION TESTS FAILED.')
99998 FORMAT (/' ------------ DPCHIP PASSED  ALL INTEGRATION TESTS',
     *        ' ------------')
99999 FORMAT (/' ************ DPCHIP FAILED SOME INTEGRATION TESTS',
     *        ' ************')
C------------- LAST LINE OF DPCHQ2 FOLLOWS -----------------------------
      END
      SUBROUTINE DPCHQ3 (LUN, KPRINT, IPASS)

c*********************************************************************72
c
cc DPCHQ3 tests the PCHIP interpolators DPCHIC, DPCHIM, DPCHSP.
c
C***BEGIN PROLOGUE  DPCHQ3
C***PURPOSE  Test the PCHIP interpolators DPCHIC, DPCHIM, DPCHSP.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (PCHQK3-S, DPCHQ3-D)
C***KEYWORDS  PCHIP INTERPOLATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C             DPCHIP QUICK CHECK NUMBER 3
C
C     TESTS THE INTERPOLATORS:  DPCHIC, DPCHIM, DPCHSP.
C *Usage:
C
C        INTEGER  LUN, KPRINT, IPASS
C
C        CALL DPCHQ3 (LUN, KPRINT, IPASS)
C
C *Arguments:
C
C     LUN   :IN  is the unit number to which output is to be written.
C
C     KPRINT:IN  controls the amount of output, as specified in the
C                SLATEC Guidelines.
C
C     IPASS:OUT  will contain a pass/fail flag.  IPASS=1 is good.
C                IPASS=0 indicates one or more tests failed.
C
C *Description:
C
C   This routine interpolates a constructed data set with all three
C   DPCHIP interpolators and compares the results with those obtained
C   on a Cray X/MP.  Two different values of the DPCHIC parameter SWITCH
C   are used.
C
C *Remarks:
C     1. The Cray results are given only to nine significant figures,
C        so don't expect them to match to more.
C     2. The results will depend to some extent on the accuracy of
C        the EXP function.
C
C***ROUTINES CALLED  COMP, D1MACH, DPCHIC, DPCHIM, DPCHSP
C***REVISION HISTORY  (YYMMDD)
C   900309  DATE WRITTEN
C   900314  Converted to a subroutine and added a SLATEC 4.0 prologue.
C   900315  Revised prologue and improved some output formats.  (FNF)
C   900316  Made TOLD machine-dependent and added extra output when
C           KPRINT=3.  (FNF)
C   900320  Added E0's to DATA statement for X to reduce single/double
C           differences, and other minor cosmetic changes.
C   900320  Converted to double precision.
C   900321  Removed IFAIL from call sequence for SLATEC standards and
C           made miscellaneous cosmetic changes.  (FNF)
C   900322  Minor changes to reduce single/double differences.  (FNF)
C   900530  Tolerance (TOLD) and argument to DPCHIC changed.  (WRB)
C   900802  Modified TOLD formula and constants in DPCHIC calls to
C           correct DPCHQ3 failures.  (FNF)
C   901130  Several significant changes:  (FNF)
C           1. Changed comparison between DPCHIM and DPCHIC to only
C              require agreement to machine precision.
C           2. Revised to print more output when KPRINT=3.
C           3. Added 1P's to formats.
C   910708  Minor modifications in use of KPRINT.  (WRB)
C   930317  Improved output formats.  (FNF)
C***END PROLOGUE  DPCHQ3
C
C*Internal Notes:
C
C     TOLD is used to compare with stored Cray results.  Its value
C          should be consistent with significance of stored values.
C     TOLZ is used for cases in which exact equality is expected.
C     TOL  is used for cases in which agreement to machine precision
C          is expected.
C**End
C
C  Declare arguments.
C
      INTEGER  LUN, KPRINT, IPASS
      LOGICAL  COMP
      DOUBLE PRECISION  D1MACH
C
C  Declare variables.
C
      INTEGER  I, IC(2), IERR, IFAIL, N, NBAD, NBADZ, NWK
      PARAMETER  (N = 9,  NWK = 2*N)
      DOUBLE PRECISION  D(N), DC(N), DC5, DC6, DM(N), DS(N), ERR, F(N),
     *                MONE, TOL, TOLD, TOLZ, VC(2), X(N), WK(NWK), ZERO
      PARAMETER  (ZERO = 0.0D0,  MONE = -1.0D0)
      CHARACTER*6  RESULT
C
C  Initialize.
C
C       Data.
      DATA  IC /0, 0/
      DATA  X /-2.2D0,-1.2D0,-1.0D0,-0.5D0,-0.01D0, 0.5D0, 1.0D0,
     *          2.0D0, 2.2D0/
C
C       Results generated on Cray X/MP (9 sign. figs.)
      DATA  DM / 0.            , 3.80027352D-01, 7.17253009D-01,
     *           5.82014161D-01, 0.            ,-5.68208031D-01,
     *          -5.13501618D-01,-7.77910977D-02,-2.45611117D-03/
      DATA  DC5,DC6 / 1.76950158D-02,-5.69579814D-01/
      DATA  DS /-5.16830792D-02, 5.71455855D-01, 7.40530225D-01,
     *           7.63864934D-01, 1.92614386D-02,-7.65324380D-01,
     *          -7.28209035D-01,-7.98445427D-02,-2.85983446D-02/
C
C***FIRST EXECUTABLE STATEMENT  DPCHQ3
      IFAIL = 0
C
C        Set tolerances.
      TOL  = 10*D1MACH(4)
      TOLD = MAX( 1.0D-7, 10*TOL )
      TOLZ = ZERO
C
      IF (KPRINT .GE. 3)  WRITE (LUN, 1000)
      IF (KPRINT .GE. 2)  WRITE (LUN, 1001)
C
C  Set up data.
C
      DO 10  I = 1, N
         F(I) = EXP(-X(I)**2)
   10 CONTINUE
C
      IF (KPRINT .GE. 3)  THEN
         WRITE (LUN, 1002)
         DO 12  I = 1, 4
            WRITE (LUN, 1010)  X(I), F(I), DM(I), DS(I)
   12    CONTINUE
            WRITE (LUN, 1011)  X(5), F(5), DM(5), DC5, DS(5)
            WRITE (LUN, 1011)  X(6), F(6), DM(6), DC6, DS(6)
         DO 15  I = 7, N
            WRITE (LUN, 1010)  X(I), F(I), DM(I), DS(I)
   15    CONTINUE
      ENDIF
C
C  Test DPCHIM.
C
      IF (KPRINT.GE.3)  WRITE (LUN, 2000) 'IM'
C     --------------------------------
      CALL DPCHIM (N, X, F, D, 1, IERR)
C     --------------------------------
C        Expect IERR=1 (one monotonicity switch).
      IF ( KPRINT.GE.3 )  WRITE (LUN, 2001) 1
      IF ( .NOT.COMP (IERR, 1, LUN, KPRINT) )  THEN
         IFAIL = IFAIL + 1
      ELSE
         IF ( KPRINT.GE.3 )  WRITE (LUN, 2002)
         NBAD = 0
         NBADZ = 0
         DO 20  I = 1, N
            RESULT = '  OK'
C             D-values should agree with stored values.
C               (Zero values should agree exactly.)
            IF ( DM(I).EQ.ZERO )  THEN
               ERR = ABS( D(I) )
               IF ( ERR.GT.TOLZ )  THEN
                  NBADZ = NBADZ + 1
                  RESULT = '**BADZ'
               ENDIF
            ELSE
               ERR = ABS( (D(I)-DM(I))/DM(I) )
               IF ( ERR.GT.TOLD )  THEN
                  NBAD = NBAD + 1
                  RESULT = '**BAD'
               ENDIF
            ENDIF
            IF (KPRINT.GE.3)
     *         WRITE (LUN, 2003)  I, X(I), D(I), ERR, RESULT
   20    CONTINUE
         IF ( (NBADZ.NE.0).OR.(NBAD.NE.0) )  THEN
            IFAIL = IFAIL + 1
            IF ((NBADZ.NE.0).AND.(KPRINT.GE.2))
     *         WRITE (LUN, 2004)  NBAD
            IF ((NBAD.NE.0).AND.(KPRINT.GE.2))
     *         WRITE (LUN, 2005)  NBAD, 'IM', TOLD
         ELSE
            IF (KPRINT.GE.2)  WRITE (LUN, 2006)  'IM'
         ENDIF
      ENDIF
C
C  Test DPCHIC -- options set to reproduce DPCHIM.
C
      IF (KPRINT.GE.3)  WRITE (LUN, 2000) 'IC'
C     --------------------------------------------------------
      CALL DPCHIC (IC, VC, ZERO, N, X, F, DC, 1, WK, NWK, IERR)
C     --------------------------------------------------------
C        Expect IERR=0 .
      IF ( KPRINT.GE.3 )  WRITE (LUN, 2001) 0
      IF ( .NOT.COMP (IERR, 0, LUN, KPRINT) )  THEN
         IFAIL = IFAIL + 1
      ELSE
         IF ( KPRINT.GE.3 )  WRITE (LUN, 2002)
         NBAD = 0
         DO 30  I = 1, N
            RESULT = '  OK'
C           D-values should agree exactly with those computed by DPCHIM.
C            (To be generous, will only test to machine precision.)
            ERR = ABS( D(I)-DC(I) )
            IF ( ERR.GT.TOL )  THEN
               NBAD = NBAD + 1
               RESULT = '**BAD'
            ENDIF
            IF (KPRINT.GE.3)
     *         WRITE (LUN, 2003)  I, X(I), DC(I), ERR, RESULT
   30    CONTINUE
         IF ( NBAD.NE.0 )  THEN
            IFAIL = IFAIL + 1
            IF (KPRINT.GE.2)  WRITE (LUN, 2005)  NBAD, 'IC', TOL
         ELSE
            IF (KPRINT.GE.2)  WRITE (LUN, 2006)  'IC'
         ENDIF
      ENDIF
C
C  Test DPCHIC -- default nonzero switch derivatives.
C
      IF (KPRINT.GE.3)  WRITE (LUN, 2000) 'IC'
C     -------------------------------------------------------
      CALL DPCHIC (IC, VC, MONE, N, X, F, D, 1, WK, NWK, IERR)
C     -------------------------------------------------------
C        Expect IERR=0 .
      IF ( KPRINT.GE.3 )  WRITE (LUN, 2001) 0
      IF ( .NOT.COMP (IERR, 0, LUN, KPRINT) )  THEN
         IFAIL = IFAIL + 1
      ELSE
         IF ( KPRINT.GE.3 )  WRITE (LUN, 2002)
         NBAD = 0
         NBADZ = 0
         DO 40  I = 1, N
            RESULT = '  OK'
C            D-values should agree exactly with those computed in
C            previous call, except at points 5 and 6.
            IF ( (I.LT.5).OR.(I.GT.6) )  THEN
               ERR = ABS( D(I)-DC(I) )
               IF ( ERR.GT.TOLZ )  THEN
                  NBADZ = NBADZ + 1
                  RESULT = '**BADA'
               ENDIF
            ELSE
               IF ( I.EQ.5 )  THEN
                  ERR = ABS( (D(I)-DC5)/DC5 )
               ELSE
                  ERR = ABS( (D(I)-DC6)/DC6 )
               ENDIF
               IF ( ERR.GT.TOLD )  THEN
                  NBAD = NBAD + 1
                  RESULT = '**BAD'
               ENDIF
            ENDIF
            IF (KPRINT.GE.3)
     *         WRITE (LUN, 2003)  I, X(I), D(I), ERR, RESULT
   40    CONTINUE
         IF ( (NBADZ.NE.0).OR.(NBAD.NE.0) )  THEN
            IFAIL = IFAIL + 1
            IF ((NBADZ.NE.0).AND.(KPRINT.GE.2))
     *         WRITE (LUN, 2007)  NBAD
            IF ((NBAD.NE.0).AND.(KPRINT.GE.2))
     *         WRITE (LUN, 2005)  NBAD, 'IC', TOLD
         ELSE
            IF (KPRINT.GE.2)  WRITE (LUN, 2006)  'IC'
         ENDIF
      ENDIF
C
C  Test DPCHSP.
C
      IF (KPRINT.GE.3)  WRITE (LUN, 2000) 'SP'
C     -------------------------------------------------
      CALL DPCHSP (IC, VC, N, X, F, D, 1, WK, NWK, IERR)
C     -------------------------------------------------
C        Expect IERR=0 .
      IF ( KPRINT.GE.3 )  WRITE (LUN, 2001) 0
      IF ( .NOT.COMP (IERR, 0, LUN, KPRINT) )  THEN
         IFAIL = IFAIL + 1
      ELSE
         IF ( KPRINT.GE.3 )  WRITE (LUN, 2002)
         NBAD = 0
         DO 50  I = 1, N
            RESULT = '  OK'
C             D-values should agree with stored values.
            ERR = ABS( (D(I)-DS(I))/DS(I) )
            IF ( ERR.GT.TOLD )  THEN
               NBAD = NBAD + 1
               RESULT = '**BAD'
            ENDIF
            IF (KPRINT.GE.3)
     *         WRITE (LUN, 2003)  I, X(I), D(I), ERR, RESULT
   50    CONTINUE
         IF ( NBAD.NE.0 )  THEN
            IFAIL = IFAIL + 1
            IF (KPRINT.GE.2)  WRITE (LUN, 2005)  NBAD, 'SP', TOLD
         ELSE
            IF (KPRINT.GE.2)  WRITE (LUN, 2006)  'SP'
         ENDIF
      ENDIF
C
C  PRINT SUMMARY AND TERMINATE.
C
      IF ((KPRINT.GE.2).AND.(IFAIL.NE.0))  WRITE (LUN, 3001)  IFAIL
C
      IF (IFAIL.EQ.0)  THEN
         IPASS = 1
         IF (KPRINT.GE.2) WRITE(LUN,99998)
      ELSE
         IPASS = 0
         IF (KPRINT.GE.1) WRITE(LUN,99999)
      ENDIF
C
      RETURN
C
C  FORMATS.
C
 1000 FORMAT ('1'//10X,'TEST DPCHIP INTERPOLATORS')
 1001 FORMAT (//10X,'DPCHQ3 RESULTS'/10X,'--------------')
 1002 FORMAT (// 5X,'DATA:'
     *         /39X,'---------- EXPECTED D-VALUES ----------'
     *         /12X,'X',9X,'F',18X,'DM',13X,'DC',13X,'DS')
 1010 FORMAT (5X,F10.2,1P,D15.5,4X,D15.5,15X,D15.5)
 1011 FORMAT (5X,F10.2,1P,D15.5,4X,3D15.5)
 2000 FORMAT (/5X,'DPCH',A2,' TEST:')
 2001 FORMAT (15X,'EXPECT  IERR =',I5)
 2002 FORMAT (/9X,'I',7X,'X',9X,'D',13X,'ERR')
 2003 FORMAT (5X,I5,F10.2,1P,2D15.5,2X,A)
 2004 FORMAT (/'    **',I5,' DPCHIM RESULTS FAILED TO BE EXACTLY ZERO.')
 2005 FORMAT (/'    **',I5,' DPCH',A2,' RESULTS FAILED TOLERANCE TEST.',
     *                     '  TOL =',1P,D10.3)
 2006 FORMAT (/5X,'  ALL DPCH',A2,' RESULTS OK.')
 2007 FORMAT (/'    **',I5,' DPCHIC RESULTS FAILED TO AGREE WITH',
     *                      ' PREVIOUS CALL.')
 3001 FORMAT (/' *** TROUBLE ***',I5,' INTERPOLATION TESTS FAILED.')
99998 FORMAT (/' ------------ DPCHIP PASSED  ALL INTERPOLATION TESTS',
     *        ' ------------')
99999 FORMAT (/' ************ DPCHIP FAILED SOME INTERPOLATION TESTS',
     *        ' ************')
C------------- LAST LINE OF DPCHQ3 FOLLOWS -----------------------------
      END
      SUBROUTINE DPCHQ4 (LUN, KPRINT, IPASS)

c*********************************************************************72
c
cc DPCHQ4 tests the PCHIP monotonicity checker DPCHCM.
c
C***BEGIN PROLOGUE  DPCHQ4
C***PURPOSE  Test the PCHIP monotonicity checker DPCHCM.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (PCHQK4-S, DPCHQ4-D)
C***KEYWORDS  PCHIP MONOTONICITY CHECKER QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C             DPCHIP QUICK CHECK NUMBER 4
C
C     TESTS THE MONOTONICITY CHECKER:  DPCHCM.
C *Usage:
C
C        INTEGER  LUN, KPRINT, IPASS
C
C        CALL DPCHQ4 (LUN, KPRINT, IPASS)
C
C *Arguments:
C
C     LUN   :IN  is the unit number to which output is to be written.
C
C     KPRINT:IN  controls the amount of output, as specified in the
C                SLATEC Guidelines.
C
C     IPASS:OUT  will contain a pass/fail flag.  IPASS=1 is good.
C                IPASS=0 indicates one or more tests failed.
C
C *Description:
C
C   This routine tests a constructed data set with three different
C   INCFD settings and compares with the expected results.  It then
C   runs a special test to check for bug in overall monotonicity found
C   in DPCHMC.  Finally, it reverses the data and repeats all tests.
C
C***ROUTINES CALLED  DPCHCM
C***REVISION HISTORY  (YYMMDD)
C   890208  DATE WRITTEN
C   890306  Changed LOUT to LUN and added it to call list.  (FNF)
C   890316  Removed DATA statements to suit new quick check standards.
C   890410  Changed PCHMC to PCHCM.
C   890410  Added a SLATEC 4.0 format prologue.
C   900314  Changed name from PCHQK3 to PCHQK4 and improved some output
C           formats.
C   900315  Revised prologue and improved some output formats.  (FNF)
C   900320  Converted to double precision.
C   900321  Removed IFAIL from call sequence for SLATEC standards and
C           made miscellaneous cosmetic changes.  (FNF)
C   900322  Added declarations so all variables are declared.  (FNF)
C   910708  Minor modifications in use of KPRINT.  (WRB)
C   930317  Improved output formats.  (FNF)
C***END PROLOGUE  DPCHQ4
C
C*Internal Notes:
C
C     Data set-up is done via assignment statements to avoid modifying
C     DATA-loaded arrays, as required by the 1989 SLATEC Guidelines.
C     Run with KPRINT=3 to display the data.
C**End
C
C  Declare arguments.
C
      INTEGER  LUN, KPRINT, IPASS
C
C  DECLARE VARIABLES.
C
      INTEGER  MAXN, MAXN2, MAXN3, NB
      PARAMETER  (MAXN = 16,  MAXN2 = 8,  MAXN3 = 6,  NB = 7)
      INTEGER  I, IERR, IFAIL, INCFD, ISMEX1(MAXN), ISMEX2(MAXN2),
     *         ISMEX3(MAXN3), ISMEXB(NB), ISMON(MAXN), K, N, NS(3)
      DOUBLE PRECISION  D(MAXN), DB(NB), F(MAXN), FB(NB), X(MAXN)
      LOGICAL  SKIP
C
C  DEFINE EXPECTED RESULTS.
C
      DATA  ISMEX1 / 1, 1,-1, 1, 1,-1, 1, 1,-1, 1, 1,-1, 1, 1,-1, 2/
      DATA  ISMEX2 / 1, 2, 2, 1, 2, 2, 1, 2/
      DATA  ISMEX3 / 1, 1, 1, 1, 1, 1/
      DATA  ISMEXB / 1, 3, 1, -1, -3, -1, 2/
C
C  DEFINE TEST DATA.
C
      DATA  NS /16, 8, 6/
C
C***FIRST EXECUTABLE STATEMENT  DPCHQ4
      IF (KPRINT .GE. 3)  WRITE (LUN, 1000)
      IF (KPRINT .GE. 2)  WRITE (LUN, 1001)
C
C       Define X, F, D.
      DO 1  I = 1, MAXN
         X(I) = I
         D(I) = 0.D0
    1 CONTINUE
      DO 2  I = 2, MAXN, 3
         D(I) = 2.D0
    2 CONTINUE
      DO 3  I = 1, 3
         F(I) = X(I)
         F(I+ 3) = F(I  ) + 1.D0
         F(I+ 6) = F(I+3) + 1.D0
         F(I+ 9) = F(I+6) + 1.D0
         F(I+12) = F(I+9) + 1.D0
    3 CONTINUE
      F(16) = 6.D0
C       Define FB, DB.
      FB(1) = 0.D0
      FB(2) = 2.D0
      FB(3) = 3.D0
      FB(4) = 5.D0
      DB(1) = 1.D0
      DB(2) = 3.D0
      DB(3) = 3.D0
      DB(4) = 0.D0
      DO 4  I = 1, 3
         FB(NB-I+1) =  FB(I)
         DB(NB-I+1) = -DB(I)
    4 CONTINUE
C
C  INITIALIZE.
C
      IFAIL = 0
C
      IF (KPRINT .GE. 3)  THEN
         WRITE (LUN, 1002)
         DO 10  I = 1, NB
            WRITE (LUN, 1010)  I, X(I), F(I), D(I), FB(I), DB(I)
   10    CONTINUE
         DO 20  I = NB+1, MAXN
            WRITE (LUN, 1010)  I, X(I), F(I), D(I)
   20    CONTINUE
      ENDIF
C
C  TRANSFER POINT FOR SECOND SET OF TESTS.
C
   25 CONTINUE
C
C  Loop over a series of values of INCFD.
C
      DO 30  INCFD = 1, 3
         N = NS(INCFD)
         SKIP = .FALSE.
C        -------------------------------------------------
         CALL DPCHCM (N, X, F, D, INCFD, SKIP, ISMON, IERR)
C        -------------------------------------------------
         IF (KPRINT.GE.3)
     *      WRITE (LUN, 2000)  INCFD, IERR, (ISMON(I), I=1,N)
         IF ( IERR.NE.0 )  THEN
            IFAIL = IFAIL + 1
            IF (KPRINT.GE.3)  WRITE (LUN,2001)
         ELSE
            DO 29  I = 1, N
               IF (INCFD.EQ.1)  THEN
                  IF ( ISMON(I).NE.ISMEX1(I) )  THEN
                     IFAIL = IFAIL + 1
                     IF (KPRINT.GE.3)
     *                  WRITE (LUN, 2002)  (ISMEX1(K),K=1,N)
                     GO TO 30
                  ENDIF
               ELSE IF (INCFD.EQ.2) THEN
                  IF ( ISMON(I).NE.ISMEX2(I) )  THEN
                     IFAIL = IFAIL + 1
                     IF (KPRINT.GE.3)
     *                  WRITE (LUN, 2002)  (ISMEX2(K),K=1,N)
                     GO TO 30
                  ENDIF
               ELSE
                  IF ( ISMON(I).NE.ISMEX3(I) )  THEN
                     IFAIL = IFAIL + 1
                     IF (KPRINT.GE.3)
     *                  WRITE (LUN, 2002)  (ISMEX3(K),K=1,N)
                     GO TO 30
                  ENDIF
               ENDIF
   29       CONTINUE
         ENDIF
   30 CONTINUE
C
C  Test for -1,3,1 bug.
C
      SKIP = .FALSE.
C     ------------------------------------------------
      CALL DPCHCM (NB, X, FB, DB, 1, SKIP, ISMON, IERR)
C     ------------------------------------------------
      IF (KPRINT.GE.3)
     *   WRITE (LUN, 2030)  IERR, (ISMON(I), I=1,NB)
      IF ( IERR.NE.0 )  THEN
         IFAIL = IFAIL + 1
         IF (KPRINT.GE.3)  WRITE (LUN,2001)
      ELSE
         DO 34  I = 1, NB
            IF ( ISMON(I).NE.ISMEXB(I) )  THEN
               IFAIL = IFAIL + 1
               IF (KPRINT.GE.3)
     *            WRITE (LUN, 2002)  (ISMEXB(K),K=1,NB)
               GO TO 35
            ENDIF
   34    CONTINUE
      ENDIF
   35 CONTINUE
C
      IF (F(1).LT.0.)  GO TO 90
C
C  Change sign and do again.
C
      IF (KPRINT.GE.3)  WRITE (LUN, 2050)
      DO 40  I = 1, MAXN
         F(I) = -F(I)
         D(I) = -D(I)
         IF ( ISMEX1(I).NE.2 )  ISMEX1(I) = -ISMEX1(I)
   40 CONTINUE
      DO 42  I = 1, MAXN2
         IF ( ISMEX2(I).NE.2 )  ISMEX2(I) = -ISMEX2(I)
   42 CONTINUE
      DO 43  I = 1, MAXN3
         IF ( ISMEX3(I).NE.2 )  ISMEX3(I) = -ISMEX3(I)
   43 CONTINUE
      DO 50  I = 1, NB
         FB(I) = -FB(I)
         DB(I) = -DB(I)
         IF ( ISMEXB(I).NE.2 )  ISMEXB(I) = -ISMEXB(I)
   50 CONTINUE
      GO TO 25
C
C  PRINT SUMMARY AND TERMINATE.
C
   90 CONTINUE
      IF ((KPRINT.GE.2).AND.(IFAIL.NE.0))  WRITE (LUN, 3001)  IFAIL
C
      IF (IFAIL.EQ.0)  THEN
         IPASS = 1
         IF (KPRINT.GE.2) WRITE(LUN,99998)
      ELSE
         IPASS = 0
         IF (KPRINT.GE.1) WRITE(LUN,99999)
      ENDIF
C
      RETURN
C
C  FORMATS.
C
 1000 FORMAT ('1'//10X,'TEST DPCHIP MONOTONICITY CHECKER')
 1001 FORMAT (//10X,'DPCHQ4 RESULTS'/10X,'--------------')
 1002 FORMAT (// 5X,'DATA:'
     *        // 9X,'I',4X,'X',5X,'F',5X,'D',5X,'FB',4X,'DB')
 1010 FORMAT (5X,I5,5F6.1)
 2000 FORMAT (/4X,'INCFD =',I2,':  IERR =',I3/15X,'ISMON =',16I3)
 2001 FORMAT (' *** Failed -- bad IERR value.')
 2002 FORMAT (' *** Failed -- expect:',16I3)
 2030 FORMAT (/4X,' Bug test:  IERR =',I3/15X,'ISMON =',7I3)
 2050 FORMAT (/4X,'Changing sign of data.....')
 3001 FORMAT (/' *** TROUBLE ***',I5,' MONOTONICITY TESTS FAILED.')
99998 FORMAT (/' ------------ DPCHIP PASSED  ALL MONOTONICITY TESTS',
     *        ' ------------')
99999 FORMAT (/' ************ DPCHIP FAILED SOME MONOTONICITY TESTS',
     *        ' ************')
C------------- LAST LINE OF DPCHQ4 FOLLOWS -----------------------------
      END
      SUBROUTINE DPCHQ5 (LUN, KPRINT, IPASS)

c*********************************************************************72
c
cc DPCHQ5 tests the PCH to B-spline conversion routine DPCHBS.
c
C***BEGIN PROLOGUE  DPCHQ5
C***PURPOSE  Test the PCH to B-spline conversion routine DPCHBS.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (PCHQK5-S, DPCHQ5-D)
C***KEYWORDS  PCHIP CONVERSION ROUTINE QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C             DPCHIP QUICK CHECK NUMBER 5
C
C     TESTS THE CONVERSION ROUTINE:  DPCHBS.
C *Usage:
C
C        INTEGER  LUN, KPRINT, IPASS
C
C        CALL DPCHQ5 (LUN, KPRINT, IPASS)
C
C *Arguments:
C
C     LUN   :IN  is the unit number to which output is to be written.
C
C     KPRINT:IN  controls the amount of output, as specified in the
C                SLATEC Guidelines.
C
C     IPASS:OUT  will contain a pass/fail flag.  IPASS=1 is good.
C                IPASS=0 indicates one or more tests failed.
C
C *Description:
C
C   This routine tests a constructed data set with four different
C   KNOTYP settings.  It computes the function and derivatives of the
C   resulting B-representation via DBVALU and compares with PCH data.
C
C *Caution:
C   This routine assumes DBVALU has already been successfully tested.
C
C***ROUTINES CALLED  DBVALU, DPCHBS, D1MACH
C***REVISION HISTORY  (YYMMDD)
C   900411  DATE WRITTEN
C   900412  Corrected minor errors in initial implementation.
C   900430  Produced double precision version.
C   900501  Corrected declarations.
C   930317  Improved output formats.  (FNF)
C***END PROLOGUE  DPCHQ5
C
C*Internal Notes:
C  TOL  is the tolerance to use for quantities that should only
C       theoretically be equal.
C  TOLZ is the tolerance to use for quantities that should be exactly
C       equal.
C
C**End
C
C  Declare arguments.
C
      INTEGER  LUN, KPRINT, IPASS
C
C  Declare externals.
C
      DOUBLE PRECISION  DBVALU, D1MACH
      EXTERNAL  DBVALU, DPCHBS, D1MACH
C
C  Declare variables.
C
      INTEGER  I, IERR, IFAIL, INBV, J, KNOTYP, K, N, NDIM, NKNOTS
      PARAMETER  (N = 9)
      DOUBLE PRECISION  BCOEF(2*N), D(N), DCALC, DERR, DERMAX, F(N),
     *      FCALC, FERR, FERMAX, T(2*N+4), TERR, TERMAX, TOL, TOLZ,
     *      TSAVE(2*N+4), WORK(16*N), X(N), ZERO
      PARAMETER  (ZERO = 0.0D0)
      LOGICAL  FAIL
C
C  Define relative error function.
C
      DOUBLE PRECISION  ANS, ERR, RELERR
      RELERR (ERR, ANS) = ABS(ERR) / MAX(1.0D-5,ABS(ANS))
C
C  Define test data.
C
      DATA  X /-2.2D0,   -1.2D0,   -1.0D0,   -0.5D0,   -0.01D0,
     *          0.5D0,    1.0D0,    2.0D0,    2.2D0/
      DATA  F / 0.0079D0, 0.2369D0, 0.3679D0, 0.7788D0, 0.9999D0,
     *          0.7788D0, 0.3679D0, 0.1083D0, 0.0079D0/
      DATA  D / 0.0000D0, 0.3800D0, 0.7173D0, 0.5820D0, 0.0177D0,
     *         -0.5696D0,-0.5135D0,-0.0778D0,-0.0025D0/
C
C  Initialize.
C
C***FIRST EXECUTABLE STATEMENT  DPCHQ5
      IFAIL = 0
      TOL = 100*D1MACH(4)
      TOLZ = ZERO
C
      IF (KPRINT.GE.3)  WRITE (LUN, 1000)
      IF (KPRINT.GE.2)  WRITE (LUN, 1001)
C
C  Loop over a series of values of KNOTYP.
C
      IF (KPRINT.GE.3)  WRITE (LUN, 1010)
      DO 300  KNOTYP = 2, -1, -1
C        ------------
         CALL DPCHBS (N, X, F, D, 1, KNOTYP, NKNOTS, T, BCOEF, NDIM, K,
     *               IERR)
C        ------------
         IF (KPRINT.GE.3)
     *       WRITE (LUN, 2000) KNOTYP, NKNOTS, NDIM, K, IERR
         IF ( IERR.NE.0 )  THEN
            IFAIL = IFAIL + 1
            IF (KPRINT.GE.3)  WRITE (LUN, 2001)
         ELSE
C             Compare evaluated results with inputs to DPCHBS.
            INBV = 1
            FERMAX = ZERO
            DERMAX = ZERO
            IF (KPRINT.GE.3)  THEN
               WRITE (LUN, 2002)
               WRITE (LUN, 2003)  T(1), T(2)
               J = 1
            ENDIF
            DO 100  I = 1, N
               FCALC = DBVALU (T, BCOEF, NDIM, K, 0, X(I), INBV, WORK)
               FERR = F(I) - FCALC
               FERMAX = MAX(FERMAX, RELERR(FERR,F(I)) )
               DCALC = DBVALU (T, BCOEF, NDIM, K, 1, X(I), INBV, WORK)
               DERR = D(I) - DCALC
               DERMAX = MAX(DERMAX, RELERR(DERR,D(I)) )
               IF (KPRINT.GE.3)  THEN
                  J = J + 2
                  WRITE (LUN, 2004)  X(I), T(J), T(J+1), F(I), FERR,
     *                                                   D(I), DERR
               ENDIF
  100       CONTINUE
            IF (KPRINT.GE.3)  THEN
               J = J + 2
               WRITE (LUN, 2003)  T(J), T(J+1)
            ENDIF
            FAIL = (FERMAX.GT.TOL).OR.(DERMAX.GT.TOL)
            IF (FAIL)  IFAIL = IFAIL + 1
            IF ((KPRINT.GE.3).OR.(KPRINT.GE.2).AND.FAIL)
     *         WRITE (LUN, 2005)  FERMAX, DERMAX, TOL
         ENDIF
C
C          Special check for KNOTYP=-1.
         IF (KNOTYP.EQ.0)  THEN
C             Save knot vector for next test.
            DO 200  I = 1, NKNOTS
               TSAVE(I) = T(I)
  200       CONTINUE
         ELSE IF (KNOTYP.EQ.-1)  THEN
C             Check that knot vector is unchanged.
            TERMAX = ZERO
            DO 250  I = 1, NKNOTS
               TERR = ABS(T(I) - TSAVE(I))
               TERMAX = MAX(TERMAX, TERR)
  250       CONTINUE
            IF (TERMAX.GT.TOLZ)  THEN
               IFAIL = IFAIL + 1
               IF (KPRINT.GE.2)  WRITE (LUN, 2007)  TERMAX, TOLZ
            ENDIF
         ENDIF
  300 CONTINUE
C
C  PRINT SUMMARY AND TERMINATE.
C
      IF ((KPRINT.GE.2).AND.(IFAIL.NE.0))  WRITE (LUN, 3001)  IFAIL
C
      IF (IFAIL.EQ.0)  THEN
         IPASS = 1
         IF (KPRINT.GE.2) WRITE(LUN,99998)
      ELSE
         IPASS = 0
         IF (KPRINT.GE.1) WRITE(LUN,99999)
      ENDIF
C
      RETURN
C
C  FORMATS.
C
 1000 FORMAT ('1'//10X,'TEST PCH TO B-SPLINE CONVERTER')
 1001 FORMAT (//10X,'DPCHQ5 RESULTS'/10X,'--------------')
 1010 FORMAT (/4X,'(Results should be the same for all KNOTYP values.)')
 2000 FORMAT (/4X,'KNOTYP =',I2,':  NKNOTS =',I3,',  NDIM =',I3,
     *                        ',  K =',I2,',  IERR =',I3)
 2001 FORMAT (' *** Failed -- bad IERR value.')
 2002 FORMAT (/15X,'X',9X,'KNOTS',10X,'F',7X,'FERR',8X,'D',7X,'DERR')
 2003 FORMAT (18X,2F8.2)
 2004 FORMAT (10X,3F8.2,F10.4,1P,D10.2,0P,F10.4,1P,D10.2)
 2005 FORMAT (/5X,'Maximum relative errors:'
     *       /15X,'F-error =',1P,D13.5,5X,'D-error =',D13.5
     *        /5X,'Both should be less than  TOL =',D13.5)
 2007 FORMAT (/' *** T-ARRAY MAXIMUM CHANGE =',1P,D13.5,
     *           ';  SHOULD NOT EXCEED TOLZ =',D13.5)
 3001 FORMAT (/' *** TROUBLE ***',I5,' CONVERSION TESTS FAILED.')
99998 FORMAT (/' ------------ DPCHIP PASSED  ALL CONVERSION TESTS',
     *        ' ------------')
99999 FORMAT (/' ************ DPCHIP FAILED SOME CONVERSION TESTS',
     *        ' ************')
C------------- LAST LINE OF DPCHQ5 FOLLOWS -----------------------------
      END
      DOUBLE PRECISION FUNCTION DPCHQA(N,X,F,D,A,B,IERR)

c*********************************************************************72
c
cc DPCHQA: definite integral of spline or piecewise cubic Hermite interpolant.
c
C***BEGIN PROLOGUE  DPCHQA
C***DATE WRITTEN   870829   (YYMMDD)
C***REVISION DATE  870829   (YYMMDD)
C***CATEGORY NO.  E3,H2A2
C***KEYWORDS  EASY TO USE CUBIC HERMITE OR SPLINE INTEGRATION
C             NUMERICAL INTEGRATION, QUADRATURE
C***AUTHOR  KAHANER, D.K., (NBS)
C             SCIENTIFIC COMPUTING DIVISION
C             NATIONAL BUREAU OF STANDARDS
C             ROOM A161, TECHNOLOGY BUILDING
C             GAITHERSBURG, MARYLAND 20899
C             (301) 975-3808
C***PURPOSE  Evaluates the definite integral of a piecewise cubic Hermit
C            or spline function over an arbitrary interval, easy to use.
C***DESCRIPTION
C
C          DPCHQA:  Piecewise Cubic Hermite or Spline Integrator,
C                  Arbitrary limits, Easy to Use.
C
C          From the book "Numerical Methods and Software"
C                  by  D. Kahaner, C. Moler, S. Nash
C                          Prentice Hall 1988
C
C     Evaluates the definite integral of the cubic Hermite or spline
C     function defined by  N, X, F, D  over the interval [A, B].  This
C     is an easy to use driver for the routine DPCHIA by F.N. Fritsch
C     described in reference (2) below. That routine also has other
C     capabilities.
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C           VALUE = DPCHQA (N, X, F, D, A, B, IERR)
C
C     INTEGER  N, IERR
C     DOUBLE PRECISION  X(N), F(N), D(N), A, B
C
C   Parameters:
C
C     VALUE -- (output) VALUE of the requested integral.
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) double precision array of independent variable
C           values.  The elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) double precision array of function values.
C           F(I) is the value corresponding to X(I).
C
C     D -- (input) double precision array of derivative values.  D(I) is
C           the value corresponding to X(I).
C
C     A,B -- (input) the limits of integration.
C           NOTE:  There is no requirement that [A,B] be contained in
C                  [X(1),X(N)].  However, the resulting integral value
C                  will be highly suspect, if not.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning errors:
C              IERR = 1  if  A  is outside the interval [X(1),X(N)].
C              IERR = 2  if  B  is outside the interval [X(1),X(N)].
C              IERR = 3  if both of the above are true.  (Note that this
C                        means that either [A,B] contains data interval
C                        or the intervals do not intersect at all.)
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -3  if the X-array is not strictly increasing.
C                (Value has not been computed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  1. F.N.FRITSCH AND R.E.CARLSON, 'MONOTONE PIECEWISE
C                 CUBIC INTERPOLATION,' SIAM J.NUMER.ANAL. 17, 2 (APRIL
C                 1980), 238-246.
C               2. F.N.FRITSCH, 'PIECEWISE CUBIC HERMITE INTERPOLATION
C                 PACKAGE, FINAL SPECIFICATIONS', LAWRENCE LIVERMORE
C                 NATIONAL LABORATORY, COMPUTER DOCUMENTATION UCID-30194
C                 AUGUST 1982.
C***ROUTINES CALLED  DPCHIA
C***END PROLOGUE  DPCHQA
      INTEGER  N, IERR
      DOUBLE PRECISION  X(N), F(N), D(N), A, B
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  INCFD
      DOUBLE PRECISION  DPCHIA
      LOGICAL SKIP
C
C  INITIALIZE.
C
      DATA  INCFD /1/
      DATA  SKIP /.TRUE./
C
C
C***FIRST EXECUTABLE STATEMENT  DPCHQA

      DPCHQA  =  DPCHIA( N, X, F, D, INCFD, SKIP, A, B, IERR )
C
C ERROR MESSAGES ARE FROM LOWER LEVEL ROUTINES
      RETURN
      END
      SUBROUTINE DPCHSP (IC, VC, N, X, F, D, INCFD, WK, NWK, IERR)

c*********************************************************************72
c
cc DPCHSP: derivatives for Hermite representation of cubic spline interpolant.
c
C***BEGIN PROLOGUE  DPCHSP
C***PURPOSE  Set derivatives needed to determine the Hermite represen-
C            tation of the cubic spline interpolant to given data, with
C            specified boundary conditions.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E1A
C***TYPE      DOUBLE PRECISION (PCHSP-S, DPCHSP-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, PCHIP,
C             PIECEWISE CUBIC INTERPOLATION, SPLINE INTERPOLATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          DPCHSP:   Piecewise Cubic Hermite Spline
C
C     Computes the Hermite representation of the cubic spline inter-
C     polant to the data given in X and F satisfying the boundary
C     conditions specified by IC and VC.
C
C     To facilitate two-dimensional applications, includes an increment
C     between successive values of the F- and D-arrays.
C
C     The resulting piecewise cubic Hermite function may be evaluated
C     by DPCHFE or DPCHFD.
C
C     NOTE:  This is a modified version of C. de Boor's cubic spline
C            routine CUBSPL.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  IC(2), N, NWK, IERR
C        DOUBLE PRECISION  VC(2), X(N), F(INCFD,N), D(INCFD,N), WK(NWK)
C
C        CALL  DPCHSP (IC, VC, N, X, F, D, INCFD, WK, NWK, IERR)
C
C   Parameters:
C
C     IC -- (input) integer array of length 2 specifying desired
C           boundary conditions:
C           IC(1) = IBEG, desired condition at beginning of data.
C           IC(2) = IEND, desired condition at end of data.
C
C           IBEG = 0  to set D(1) so that the third derivative is con-
C              tinuous at X(2).  This is the "not a knot" condition
C              provided by de Boor's cubic spline routine CUBSPL.
C              < This is the default boundary condition. >
C           IBEG = 1  if first derivative at X(1) is given in VC(1).
C           IBEG = 2  if second derivative at X(1) is given in VC(1).
C           IBEG = 3  to use the 3-point difference formula for D(1).
C                     (Reverts to the default b.c. if N.LT.3 .)
C           IBEG = 4  to use the 4-point difference formula for D(1).
C                     (Reverts to the default b.c. if N.LT.4 .)
C          NOTES:
C           1. An error return is taken if IBEG is out of range.
C           2. For the "natural" boundary condition, use IBEG=2 and
C              VC(1)=0.
C
C           IEND may take on the same values as IBEG, but applied to
C           derivative at X(N).  In case IEND = 1 or 2, the value is
C           given in VC(2).
C
C          NOTES:
C           1. An error return is taken if IEND is out of range.
C           2. For the "natural" boundary condition, use IEND=2 and
C              VC(2)=0.
C
C     VC -- (input) real*8 array of length 2 specifying desired boundary
C           values, as indicated above.
C           VC(1) need be set only if IC(1) = 1 or 2 .
C           VC(2) need be set only if IC(2) = 1 or 2 .
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real*8 array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real*8 array of dependent variable values to be
C           interpolated.  F(1+(I-1)*INCFD) is value corresponding to
C           X(I).
C
C     D -- (output) real*8 array of derivative values at the data
C           points.  These values will determine the cubic spline
C           interpolant with the requested boundary conditions.
C           The value corresponding to X(I) is stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C           No other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in F and D.
C           This argument is provided primarily for 2-D applications.
C           (Error return if  INCFD.LT.1 .)
C
C     WK -- (scratch) real*8 array of working storage.
C
C     NWK -- (input) length of work array.
C           (Error return if NWK.LT.2*N .)
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if IBEG.LT.0 or IBEG.GT.4 .
C              IERR = -5  if IEND.LT.0 of IEND.GT.4 .
C              IERR = -6  if both of the above are true.
C              IERR = -7  if NWK is too small.
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C             (The D-array has not been changed in any of these cases.)
C              IERR = -8  in case of trouble solving the linear system
C                         for the interior derivative values.
C             (The D-array may have been changed in this case.)
C             (             Do **NOT** use it!                )
C
C***REFERENCES  Carl de Boor, A Practical Guide to Splines, Springer-
C                 Verlag, New York, 1978, pp. 53-59.
C***ROUTINES CALLED  DPCHDF, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820503  DATE WRITTEN
C   820804  Converted to SLATEC library version.
C   870707  Corrected XERROR calls for d.p. name(s).
C   890206  Corrected XERROR calls.
C   890411  Added SAVE statements (Vers. 3.2).
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920429  Revised format and order of references.  (WRB,FNF)
C***END PROLOGUE  DPCHSP
C  Programming notes:
C
C     To produce a single precision version, simply:
C        a. Change DPCHSP to PCHSP wherever it occurs,
C        b. Change the double precision declarations to real, and
C        c. Change the constants ZERO, HALF, ... to single precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  IC(2), N, INCFD, NWK, IERR
      DOUBLE PRECISION  VC(2), X(*), F(INCFD,*), D(INCFD,*), WK(2,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  IBEG, IEND, INDEX, J, NM1
      DOUBLE PRECISION  G, HALF, ONE, STEMP(3), THREE, TWO, XTEMP(4),
     *  ZERO
      SAVE ZERO, HALF, ONE, TWO, THREE
      DOUBLE PRECISION  DPCHDF
C
      DATA  ZERO /0.D0/, HALF/.5D0/, ONE/1.D0/, TWO/2.D0/, THREE/3.D0/
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  DPCHSP
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  J = 2, N
         IF ( X(J).LE.X(J-1) )  GO TO 5003
    1 CONTINUE
C
      IBEG = IC(1)
      IEND = IC(2)
      IERR = 0
      IF ( (IBEG.LT.0).OR.(IBEG.GT.4) )  IERR = IERR - 1
      IF ( (IEND.LT.0).OR.(IEND.GT.4) )  IERR = IERR - 2
      IF ( IERR.LT.0 )  GO TO 5004
C
C  FUNCTION DEFINITION IS OK -- GO ON.
C
      IF ( NWK .LT. 2*N )  GO TO 5007
C
C  COMPUTE FIRST DIFFERENCES OF X SEQUENCE AND STORE IN WK(1,.). ALSO,
C  COMPUTE FIRST DIVIDED DIFFERENCE OF DATA AND STORE IN WK(2,.).
      DO 5  J=2,N
         WK(1,J) = X(J) - X(J-1)
         WK(2,J) = (F(1,J) - F(1,J-1))/WK(1,J)
    5 CONTINUE
C
C  SET TO DEFAULT BOUNDARY CONDITIONS IF N IS TOO SMALL.
C
      IF ( IBEG.GT.N )  IBEG = 0
      IF ( IEND.GT.N )  IEND = 0
C
C  SET UP FOR BOUNDARY CONDITIONS.
C
      IF ( (IBEG.EQ.1).OR.(IBEG.EQ.2) )  THEN
         D(1,1) = VC(1)
      ELSE IF (IBEG .GT. 2)  THEN
C        PICK UP FIRST IBEG POINTS, IN REVERSE ORDER.
         DO 10  J = 1, IBEG
            INDEX = IBEG-J+1
C           INDEX RUNS FROM IBEG DOWN TO 1.
            XTEMP(J) = X(INDEX)
            IF (J .LT. IBEG)  STEMP(J) = WK(2,INDEX)
   10    CONTINUE
C                 --------------------------------
         D(1,1) = DPCHDF (IBEG, XTEMP, STEMP, IERR)
C                 --------------------------------
         IF (IERR .NE. 0)  GO TO 5009
         IBEG = 1
      ENDIF
C
      IF ( (IEND.EQ.1).OR.(IEND.EQ.2) )  THEN
         D(1,N) = VC(2)
      ELSE IF (IEND .GT. 2)  THEN
C        PICK UP LAST IEND POINTS.
         DO 15  J = 1, IEND
            INDEX = N-IEND+J
C           INDEX RUNS FROM N+1-IEND UP TO N.
            XTEMP(J) = X(INDEX)
            IF (J .LT. IEND)  STEMP(J) = WK(2,INDEX+1)
   15    CONTINUE
C                 --------------------------------
         D(1,N) = DPCHDF (IEND, XTEMP, STEMP, IERR)
C                 --------------------------------
         IF (IERR .NE. 0)  GO TO 5009
         IEND = 1
      ENDIF
C
C --------------------( BEGIN CODING FROM CUBSPL )--------------------
C
C  **** A TRIDIAGONAL LINEAR SYSTEM FOR THE UNKNOWN SLOPES S(J) OF
C  F  AT X(J), J=1,...,N, IS GENERATED AND THEN SOLVED BY GAUSS ELIM-
C  INATION, WITH S(J) ENDING UP IN D(1,J), ALL J.
C     WK(1,.) AND WK(2,.) ARE USED FOR TEMPORARY STORAGE.
C
C  CONSTRUCT FIRST EQUATION FROM FIRST BOUNDARY CONDITION, OF THE FORM
C             WK(2,1)*S(1) + WK(1,1)*S(2) = D(1,1)
C
      IF (IBEG .EQ. 0)  THEN
         IF (N .EQ. 2)  THEN
C           NO CONDITION AT LEFT END AND N = 2.
            WK(2,1) = ONE
            WK(1,1) = ONE
            D(1,1) = TWO*WK(2,2)
         ELSE
C           NOT-A-KNOT CONDITION AT LEFT END AND N .GT. 2.
            WK(2,1) = WK(1,3)
            WK(1,1) = WK(1,2) + WK(1,3)
            D(1,1) =((WK(1,2) + TWO*WK(1,1))*WK(2,2)*WK(1,3)
     *                        + WK(1,2)**2*WK(2,3)) / WK(1,1)
         ENDIF
      ELSE IF (IBEG .EQ. 1)  THEN
C        SLOPE PRESCRIBED AT LEFT END.
         WK(2,1) = ONE
         WK(1,1) = ZERO
      ELSE
C        SECOND DERIVATIVE PRESCRIBED AT LEFT END.
         WK(2,1) = TWO
         WK(1,1) = ONE
         D(1,1) = THREE*WK(2,2) - HALF*WK(1,2)*D(1,1)
      ENDIF
C
C  IF THERE ARE INTERIOR KNOTS, GENERATE THE CORRESPONDING EQUATIONS AND
C  CARRY OUT THE FORWARD PASS OF GAUSS ELIMINATION, AFTER WHICH THE J-TH
C  EQUATION READS    WK(2,J)*S(J) + WK(1,J)*S(J+1) = D(1,J).
C
      NM1 = N-1
      IF (NM1 .GT. 1)  THEN
         DO 20 J=2,NM1
            IF (WK(2,J-1) .EQ. ZERO)  GO TO 5008
            G = -WK(1,J+1)/WK(2,J-1)
            D(1,J) = G*D(1,J-1)
     *                  + THREE*(WK(1,J)*WK(2,J+1) + WK(1,J+1)*WK(2,J))
            WK(2,J) = G*WK(1,J-1) + TWO*(WK(1,J) + WK(1,J+1))
   20    CONTINUE
      ENDIF
C
C  CONSTRUCT LAST EQUATION FROM SECOND BOUNDARY CONDITION, OF THE FORM
C           (-G*WK(2,N-1))*S(N-1) + WK(2,N)*S(N) = D(1,N)
C
C     IF SLOPE IS PRESCRIBED AT RIGHT END, ONE CAN GO DIRECTLY TO BACK-
C     SUBSTITUTION, SINCE ARRAYS HAPPEN TO BE SET UP JUST RIGHT FOR IT
C     AT THIS POINT.
      IF (IEND .EQ. 1)  GO TO 30
C
      IF (IEND .EQ. 0)  THEN
         IF (N.EQ.2 .AND. IBEG.EQ.0)  THEN
C           NOT-A-KNOT AT RIGHT ENDPOINT AND AT LEFT ENDPOINT AND N = 2.
            D(1,2) = WK(2,2)
            GO TO 30
         ELSE IF ((N.EQ.2) .OR. (N.EQ.3 .AND. IBEG.EQ.0))  THEN
C           EITHER (N=3 AND NOT-A-KNOT ALSO AT LEFT) OR (N=2 AND *NOT*
C           NOT-A-KNOT AT LEFT END POINT).
            D(1,N) = TWO*WK(2,N)
            WK(2,N) = ONE
            IF (WK(2,N-1) .EQ. ZERO)  GO TO 5008
            G = -ONE/WK(2,N-1)
         ELSE
C           NOT-A-KNOT AND N .GE. 3, AND EITHER N.GT.3 OR  ALSO NOT-A-
C           KNOT AT LEFT END POINT.
            G = WK(1,N-1) + WK(1,N)
C           DO NOT NEED TO CHECK FOLLOWING DENOMINATORS (X-DIFFERENCES).
            D(1,N) = ((WK(1,N)+TWO*G)*WK(2,N)*WK(1,N-1)
     *                  + WK(1,N)**2*(F(1,N-1)-F(1,N-2))/WK(1,N-1))/G
            IF (WK(2,N-1) .EQ. ZERO)  GO TO 5008
            G = -G/WK(2,N-1)
            WK(2,N) = WK(1,N-1)
         ENDIF
      ELSE
C        SECOND DERIVATIVE PRESCRIBED AT RIGHT ENDPOINT.
         D(1,N) = THREE*WK(2,N) + HALF*WK(1,N)*D(1,N)
         WK(2,N) = TWO
         IF (WK(2,N-1) .EQ. ZERO)  GO TO 5008
         G = -ONE/WK(2,N-1)
      ENDIF
C
C  COMPLETE FORWARD PASS OF GAUSS ELIMINATION.
C
      WK(2,N) = G*WK(1,N-1) + WK(2,N)
      IF (WK(2,N) .EQ. ZERO)   GO TO 5008
      D(1,N) = (G*D(1,N-1) + D(1,N))/WK(2,N)
C
C  CARRY OUT BACK SUBSTITUTION
C
   30 CONTINUE
      DO 40 J=NM1,1,-1
         IF (WK(2,J) .EQ. ZERO)  GO TO 5008
         D(1,J) = (D(1,J) - WK(1,J)*D(1,J+1))/WK(2,J)
   40 CONTINUE
C --------------------(  END  CODING FROM CUBSPL )--------------------
C
C  NORMAL RETURN.
C
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHSP',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHSP', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHSP',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      RETURN
C
 5004 CONTINUE
C     IC OUT OF RANGE RETURN.
      IERR = IERR - 3
      CALL XERMSG ('SLATEC', 'DPCHSP', 'IC OUT OF RANGE', IERR, 1)
      RETURN
C
 5007 CONTINUE
C     NWK TOO SMALL RETURN.
      IERR = -7
      CALL XERMSG ('SLATEC', 'DPCHSP', 'WORK ARRAY TOO SMALL', IERR, 1)
      RETURN
C
 5008 CONTINUE
C     SINGULAR SYSTEM.
C   *** THEORETICALLY, THIS CAN ONLY OCCUR IF SUCCESSIVE X-VALUES   ***
C   *** ARE EQUAL, WHICH SHOULD ALREADY HAVE BEEN CAUGHT (IERR=-3). ***
      IERR = -8
      CALL XERMSG ('SLATEC', 'DPCHSP', 'SINGULAR LINEAR SYSTEM', IERR,
     +   1)
      RETURN
C
 5009 CONTINUE
C     ERROR RETURN FROM DPCHDF.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -9
      CALL XERMSG ('SLATEC', 'DPCHSP', 'ERROR RETURN FROM DPCHDF',
     +   IERR, 1)
      RETURN
      END
      DOUBLE PRECISION FUNCTION DPCHST (ARG1, ARG2)

c*********************************************************************72
c
cc DPCHST: carry out a sign test.
c
C***BEGIN PROLOGUE  DPCHST
C***SUBSIDIARY
C***PURPOSE  DPCHIP Sign-Testing Routine
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (PCHST-S, DPCHST-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C         DPCHST:  DPCHIP Sign-Testing Routine.
C
C
C     Returns:
C        -1. if ARG1 and ARG2 are of opposite sign.
C         0. if either argument is zero.
C        +1. if ARG1 and ARG2 are of the same sign.
C
C     The object is to do this without multiplying ARG1*ARG2, to avoid
C     possible over/underflow problems.
C
C  Fortran intrinsics used:  SIGN.
C
C***SEE ALSO  DPCHCE, DPCHCI, DPCHCS, DPCHIM
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   811103  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   870813  Minor cosmetic changes.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  DPCHST
C
C**End
C
C  DECLARE ARGUMENTS.
C
      DOUBLE PRECISION  ARG1, ARG2
C
C  DECLARE LOCAL VARIABLES.
C
      DOUBLE PRECISION  ONE, ZERO
      SAVE ZERO, ONE
      DATA  ZERO /0.D0/,  ONE/1.D0/
C
C  PERFORM THE TEST.
C
C***FIRST EXECUTABLE STATEMENT  DPCHST
      DPCHST = SIGN(ONE,ARG1) * SIGN(ONE,ARG2)
      IF ((ARG1.EQ.ZERO) .OR. (ARG2.EQ.ZERO))  DPCHST = ZERO
C
      RETURN
      END
      SUBROUTINE DPCHSW (DFMAX, IEXTRM, D1, D2, H, SLOPE, IERR)

c*********************************************************************72
c
cc DPCHSW limits excursion from data for DPCHCS.
c
C***BEGIN PROLOGUE  DPCHSW
C***SUBSIDIARY
C***PURPOSE  Limits excursion from data for DPCHCS
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      DOUBLE PRECISION (PCHSW-S, DPCHSW-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C         DPCHSW:  DPCHCS Switch Excursion Limiter.
C
C     Called by  DPCHCS  to adjust D1 and D2 if necessary to insure that
C     the extremum on this interval is not further than DFMAX from the
C     extreme data value.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        INTEGER  IEXTRM, IERR
C        DOUBLE PRECISION  DFMAX, D1, D2, H, SLOPE
C
C        CALL  DPCHSW (DFMAX, IEXTRM, D1, D2, H, SLOPE, IERR)
C
C   Parameters:
C
C     DFMAX -- (input) maximum allowed difference between F(IEXTRM) and
C           the cubic determined by derivative values D1,D2.  (assumes
C           DFMAX.GT.0.)
C
C     IEXTRM -- (input) index of the extreme data value.  (assumes
C           IEXTRM = 1 or 2 .  Any value .NE.1 is treated as 2.)
C
C     D1,D2 -- (input) derivative values at the ends of the interval.
C           (Assumes D1*D2 .LE. 0.)
C          (output) may be modified if necessary to meet the restriction
C           imposed by DFMAX.
C
C     H -- (input) interval length.  (Assumes  H.GT.0.)
C
C     SLOPE -- (input) data slope on the interval.
C
C     IERR -- (output) error flag.  should be zero.
C           If IERR=-1, assumption on D1 and D2 is not satisfied.
C           If IERR=-2, quadratic equation locating extremum has
C                       negative discriminant (should never occur).
C
C    -------
C    WARNING:  This routine does no validity-checking of arguments.
C    -------
C
C  Fortran intrinsics used:  ABS, SIGN, SQRT.
C
C***SEE ALSO  DPCHCS
C***ROUTINES CALLED  D1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820218  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   870707  Corrected XERROR calls for d.p. name(s).
C   870707  Replaced DATA statement for SMALL with a use of D1MACH.
C   870813  Minor cosmetic changes.
C   890206  Corrected XERROR calls.
C   890411  1. Added SAVE statements (Vers. 3.2).
C           2. Added DOUBLE PRECISION declaration for D1MACH.
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB)
C   920526  Eliminated possible divide by zero problem.  (FNF)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  DPCHSW
C
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  IEXTRM, IERR
      DOUBLE PRECISION  DFMAX, D1, D2, H, SLOPE
C
C  DECLARE LOCAL VARIABLES.
C
      DOUBLE PRECISION  CP, FACT, HPHI, LAMBDA, NU, ONE, PHI, RADCAL,
     *                  RHO, SIGMA, SMALL, THAT, THIRD, THREE, TWO, ZERO
      SAVE ZERO, ONE, TWO, THREE, FACT
      SAVE THIRD
      DOUBLE PRECISION  D1MACH
C
      DATA  ZERO /0.D0/,  ONE /1.D0/,  TWO /2.D0/, THREE /3.D0/,
     *      FACT /100.D0/
C        THIRD SHOULD BE SLIGHTLY LESS THAN 1/3.
      DATA  THIRD /0.33333D0/
C
C  NOTATION AND GENERAL REMARKS.
C
C     RHO IS THE RATIO OF THE DATA SLOPE TO THE DERIVATIVE BEING TESTED.
C     LAMBDA IS THE RATIO OF D2 TO D1.
C     THAT = T-HAT(RHO) IS THE NORMALIZED LOCATION OF THE EXTREMUM.
C     PHI IS THE NORMALIZED VALUE OF P(X)-F1 AT X = XHAT = X-HAT(RHO),
C           WHERE  THAT = (XHAT - X1)/H .
C        THAT IS, P(XHAT)-F1 = D*H*PHI,  WHERE D=D1 OR D2.
C     SIMILARLY,  P(XHAT)-F2 = D*H*(PHI-RHO) .
C
C      SMALL SHOULD BE A FEW ORDERS OF MAGNITUDE GREATER THAN MACHEPS.
C***FIRST EXECUTABLE STATEMENT  DPCHSW
      SMALL = FACT*D1MACH(4)
C
C  DO MAIN CALCULATION.
C
      IF (D1 .EQ. ZERO)  THEN
C
C        SPECIAL CASE -- D1.EQ.ZERO .
C
C          IF D2 IS ALSO ZERO, THIS ROUTINE SHOULD NOT HAVE BEEN CALLED.
         IF (D2 .EQ. ZERO)  GO TO 5001
C
         RHO = SLOPE/D2
C          EXTREMUM IS OUTSIDE INTERVAL WHEN RHO .GE. 1/3 .
         IF (RHO .GE. THIRD)  GO TO 5000
         THAT = (TWO*(THREE*RHO-ONE)) / (THREE*(TWO*RHO-ONE))
         PHI = THAT**2 * ((THREE*RHO-ONE)/THREE)
C
C          CONVERT TO DISTANCE FROM F2 IF IEXTRM.NE.1 .
         IF (IEXTRM .NE. 1)  PHI = PHI - RHO
C
C          TEST FOR EXCEEDING LIMIT, AND ADJUST ACCORDINGLY.
         HPHI = H * ABS(PHI)
         IF (HPHI*ABS(D2) .GT. DFMAX)  THEN
C           AT THIS POINT, HPHI.GT.0, SO DIVIDE IS OK.
            D2 = SIGN (DFMAX/HPHI, D2)
         ENDIF
      ELSE
C
         RHO = SLOPE/D1
         LAMBDA = -D2/D1
         IF (D2 .EQ. ZERO)  THEN
C
C           SPECIAL CASE -- D2.EQ.ZERO .
C
C             EXTREMUM IS OUTSIDE INTERVAL WHEN RHO .GE. 1/3 .
            IF (RHO .GE. THIRD)  GO TO 5000
            CP = TWO - THREE*RHO
            NU = ONE - TWO*RHO
            THAT = ONE / (THREE*NU)
         ELSE
            IF (LAMBDA .LE. ZERO)  GO TO 5001
C
C           NORMAL CASE -- D1 AND D2 BOTH NONZERO, OPPOSITE SIGNS.
C
            NU = ONE - LAMBDA - TWO*RHO
            SIGMA = ONE - RHO
            CP = NU + SIGMA
            IF (ABS(NU) .GT. SMALL)  THEN
               RADCAL = (NU - (TWO*RHO+ONE))*NU + SIGMA**2
               IF (RADCAL .LT. ZERO)  GO TO 5002
               THAT = (CP - SQRT(RADCAL)) / (THREE*NU)
            ELSE
               THAT = ONE/(TWO*SIGMA)
            ENDIF
         ENDIF
         PHI = THAT*((NU*THAT - CP)*THAT + ONE)
C
C          CONVERT TO DISTANCE FROM F2 IF IEXTRM.NE.1 .
         IF (IEXTRM .NE. 1)  PHI = PHI - RHO
C
C          TEST FOR EXCEEDING LIMIT, AND ADJUST ACCORDINGLY.
         HPHI = H * ABS(PHI)
         IF (HPHI*ABS(D1) .GT. DFMAX)  THEN
C           AT THIS POINT, HPHI.GT.0, SO DIVIDE IS OK.
            D1 = SIGN (DFMAX/HPHI, D1)
            D2 = -LAMBDA*D1
         ENDIF
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      IERR = 0
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     D1 AND D2 BOTH ZERO, OR BOTH NONZERO AND SAME SIGN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHSW', 'D1 AND/OR D2 INVALID', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     NEGATIVE VALUE OF RADICAL (SHOULD NEVER OCCUR).
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHSW', 'NEGATIVE RADICAL', IERR, 1)
      RETURN
      END
      SUBROUTINE EVCHCK (LOUT, KPRINT, NPTS, XEV, FEV, DEV, FEV2, FAIL)

c*********************************************************************72
c
cc EVCHK tests evaluation accuracy of CHFDV and DHFEV for PCHQK1.
c
C***BEGIN PROLOGUE  EVCHCK
C***SUBSIDIARY
C***PURPOSE  Test evaluation accuracy of CHFDV and CHFEV for PCHQK1.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (EVCHCK-S, DEVCHK-D)
C***KEYWORDS  PCHIP EVALUATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C -------- CODE TO TEST EVALUATION ACCURACY OF CHFDV AND CHFEV --------
C
C     USING FUNCTION AND DERIVATIVE VALUES FROM A CUBIC (COMPUTED IN
C     DOUBLE PRECISION) AT NINT DIFFERENT (X1,X2) PAIRS:
C     1. CHECKS THAT CHFDV AND CHFEV BOTH REPRODUCE ENDPOINT VALUES.
C     2. EVALUATES AT NPTS POINTS, 10 OF WHICH ARE OUTSIDE THE INTERVAL
C        AND:
C        A. CHECKS ACCURACY OF CHFDV FUNCTION AND DERIVATIVE VALUES
C           AGAINST EXACT VALUES.
C        B. CHECKS THAT RETURNED VALUES OF NEXT SUM TO 10.
C        C. CHECKS THAT FUNCTION VALUES FROM CHFEV AGREE WITH THOSE
C           FROM CHFDV.
C
C
C     FORTRAN INTRINSICS USED:  ABS, MAX, MIN.
C     FORTRAN LIBRARY ROUTINES USED:  SQRT, (READ), (WRITE).
C     SLATEC LIBRARY ROUTINES USED:  CHFDV, CHFEV, R1MACH, RAND.
C     OTHER ROUTINES USED:  FDTRUE.
C
C***ROUTINES CALLED  CHFDV, CHFEV, FDTRUE, R1MACH, RAND
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   820624  CONVERTED TO QUICK CHECK FOR SLATEC LIBRARY.
C   820630  1. MODIFIED DEFINITIONS OF RELATIVE ERROR AND TEST
C             TOLERANCES.
C           2. VARIOUS IMPROVEMENTS TO OUTPUT FORMATS.
C   820716  1. SET MACHEP VIA A CALL TO R1MACH.
C           2. CHANGED FROM FORTLIB'S RANF TO SLATEC'S RAND.
C   890629  1. Appended E0 to real constants to reduce S.P./D.P.
C             differences.
C           2. Other minor cosmetic changes.
C   890831  Modified array declarations.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891004  Cosmetic changes to prologue.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  Revised prologue and improved some output formats.  (FNF)
C           Also moved formats to end to be consistent with other PCHIP
C           quick checks.
C   900316  Additional minor cosmetic changes.  (FNF)
C   900321  Made miscellaneous cosmetic changes.  (FNF)
C   901130  Added 1P's to formats and revised some to reduce maximum
C           line length.  (FNF)
C   910708  Minor modifications in use of KPRINT.  (WRB)
C   910801  Added EXTERNAL statement for RAND due to problem on IBM
C           RS 6000.  (WRB)
C***END PROLOGUE  EVCHCK
C
C  Declare arguments.
C
      INTEGER  LOUT, KPRINT, NPTS
      REAL  XEV(*), FEV(*), DEV(*), FEV2(*)
      LOGICAL  FAIL
C
C  DECLARATIONS.
C
      INTEGER  I, IERR, IINT, NEXT(2), NEXT2(2), NINT
      REAL  AED, AED2, AEDMAX, AEDMIN, AEF, AEF2, AEFMAX, AEFMIN,
     *      CHECK(2), CHECKF(2), CHECKD(2), D1, D2, DERMAX, DTRUE, DX,
     *      EPS1, EPS2, F1, F2, FACT, FERMAX, FLOORD, FLOORF, FOUR,
     *      FTRUE, LEFT(3), MACHEP,
     *      ONE, RED, RED2, REDMAX, REDMIN, REF, REF2, REFMAX,
     *      REFMIN, RIGHT(3), SMALL, TEN, TOL1, TOL2,
     *      X1, X2, XADMAX, XADMIN, XAFMAX, XAFMIN, XRDMAX,
     *      XRDMIN, XRFMAX, XRFMIN, ZERO
      LOGICAL  FAILOC, FAILNX
C
      REAL  R1MACH
C       The following should stay REAL (no D.P. equivalent).
      REAL  RAND
      EXTERNAL  RAND
C
C  DEFINE RELATIVE ERROR WITH FLOOR.
C
      REAL  RERR, ERR, VALUE, FLOOR
      RERR(ERR,VALUE,FLOOR) = ERR / MAX(ABS(VALUE), FLOOR)
C
C  INITIALIZE.
C
      DATA  ZERO /0.E0/, ONE /1.E0/, FOUR /4.E0/, TEN /10.E0/
      DATA  SMALL  /1.0E-10/
      DATA  NINT /3/
      DATA   LEFT /-1.5E0, 2.0E-10, 1.0E0 /
      DATA  RIGHT / 2.5E0, 3.0E-10, 1.0E+8/
C
C***FIRST EXECUTABLE STATEMENT  EVCHCK
      MACHEP = R1MACH(4)
      EPS1 = FOUR*MACHEP
      EPS2 = TEN*MACHEP
C
      FAIL = .FALSE.
C
      IF (KPRINT .GE. 2)  WRITE (LOUT, 3000)
C
C  CYCLE OVER INTERVALS.
C
      DO 90  IINT = 1, NINT
      X1 =  LEFT(IINT)
      X2 = RIGHT(IINT)
C
      FACT = MAX(SQRT(X2-X1), ONE)
      TOL1 = EPS1 * FACT
      TOL2 = EPS2 * FACT
C
C  COMPUTE AND PRINT ENDPOINT VALUES.
C
      CALL FDTRUE (X1, F1, D1)
      CALL FDTRUE (X2, F2, D2)
C
      IF (KPRINT .GE. 3)  THEN
         IF (IINT .EQ. 1)  WRITE (LOUT, 2000)
         WRITE (LOUT, '(/)')
         WRITE (LOUT, 2001)  'X1', X1, 'X2', X2
         WRITE (LOUT, 2001)  'F1', F1, 'F2', F2
         WRITE (LOUT, 2001)  'D1', D1, 'D2', D2
      ENDIF
C
      IF (KPRINT .GE. 2)  WRITE (LOUT, 3001)  X1, X2
C
C  COMPUTE FLOORS FOR RELATIVE ERRORS.
C
      FLOORF = MAX( MIN(ABS(F1),ABS(F2)), SMALL)
      FLOORD = MAX( MIN(ABS(D1),ABS(D2)), SMALL)
C
C  CHECK REPRODUCTION OF ENDPOINT VALUES.
C
      XEV(1) = X1
      XEV(2) = X2
C     -----------------------------------------------------------
      CALL CHFDV (X1, X2, F1, F2, D1, D2, 2, XEV, CHECKF, CHECKD,
     *            NEXT, IERR)
C     -----------------------------------------------------------
      AEF  = CHECKF(1)-F1
      REF  = RERR(AEF , F1, FLOORF)
      AEF2 = CHECKF(2)-F2
      REF2 = RERR(AEF2, F2, FLOORF)
      AED  = CHECKD(1)-D1
      RED  = RERR(AED , D1, FLOORD)
      AED2 = CHECKD(2)-D2
      RED2 = RERR(AED2, D2, FLOORD)
C
      FAILOC = MAX(ABS(REF),ABS(REF2),ABS(RED),ABS(RED2)) .GT. TOL1
      FAIL = FAIL .OR. FAILOC
C
      IF (KPRINT .GE. 3)  THEN
         WRITE (LOUT, 2002)  NEXT, AEF, AEF2, AED, AED2
         WRITE (LOUT, 2003)  REF, REF2, RED, RED2
      ENDIF
C
      IF (FAILOC .AND. (KPRINT.GE.2))  WRITE (LOUT, 3002)
C
C  CHFEV SHOULD AGREE EXACTLY WITH CHFDV.
C                     -------
C     --------------------------------------------------------------
      CALL CHFEV (X1, X2, F1, F2, D1, D2, 2, XEV, CHECK, NEXT, IERR)
C     --------------------------------------------------------------
      FAILOC = (CHECK(1).NE.CHECKF(1)) .OR. (CHECK(2).NE.CHECKF(2))
      FAIL = FAIL .OR. FAILOC
C
      IF (FAILOC .AND. (KPRINT.GE.2))  WRITE (LOUT, 3003)
C
C  EVALUATE AT NPTS 'UNIFORMLY RANDOM' POINTS IN (X1,X2).
C     THIS VERSION EXTENDS EVALUATION DOMAIN BY ADDING 4 SUBINTERVALS
C     TO LEFT AND 6 TO RIGHT OF [X1,X2].
C
      DX = (X2-X1)/(NPTS-10)
      DO 20  I = 1, NPTS
         XEV(I) = (X1 + (I-5)*DX) + DX*RAND(ZERO)
   20 CONTINUE
C     --------------------------------------------------------
      CALL CHFDV (X1, X2, F1, F2, D1, D2, NPTS, XEV, FEV, DEV,
     *            NEXT, IERR)
C     --------------------------------------------------------
      IF (IERR .NE. 0)  THEN
         FAILOC = .TRUE.
         IF (KPRINT .GE. 2)  WRITE (LOUT, 4003)  IERR
      ELSE
C
C     CUMULATE LARGEST AND SMALLEST ERRORS FOR SUMMARY.
C
      DO 30  I = 1, NPTS
         CALL FDTRUE (XEV(I), FTRUE, DTRUE)
         AEF = FEV(I) - FTRUE
         REF = RERR(AEF, FTRUE, FLOORF)
         AED = DEV(I) - DTRUE
         RED = RERR(AED, DTRUE, FLOORD)
C
         IF (I .EQ. 1)  THEN
C            INITIALIZE.
            AEFMIN = AEF
            AEFMAX = AEF
            AEDMIN = AED
            AEDMAX = AED
            REFMIN = REF
            REFMAX = REF
            REDMIN = RED
            REDMAX = RED
            XAFMIN = XEV(1)
            XAFMAX = XEV(1)
            XADMIN = XEV(1)
            XADMAX = XEV(1)
            XRFMIN = XEV(1)
            XRFMAX = XEV(1)
            XRDMIN = XEV(1)
            XRDMAX = XEV(1)
         ELSE
C            SELECT.
            IF (AEF .LT. AEFMIN)  THEN
               AEFMIN = AEF
               XAFMIN = XEV(I)
            ELSE IF (AEF .GT. AEFMAX)  THEN
               AEFMAX = AEF
               XAFMAX = XEV(I)
            ENDIF
            IF (AED .LT. AEDMIN)  THEN
               AEDMIN = AED
               XADMIN = XEV(I)
            ELSE IF (AED .GT. AEDMAX)  THEN
               AEDMAX = AED
               XADMAX = XEV(I)
            ENDIF
            IF (REF .LT. REFMIN)  THEN
               REFMIN = REF
               XRFMIN = XEV(I)
            ELSE IF (REF .GT. REFMAX)  THEN
               REFMAX = REF
               XRFMAX = XEV(I)
            ENDIF
            IF (RED .LT. REDMIN)  THEN
               REDMIN = RED
               XRDMIN = XEV(I)
            ELSE IF (RED .GT. REDMAX)  THEN
               REDMAX = RED
               XRDMAX = XEV(I)
            ENDIF
         ENDIF
   30    CONTINUE
C
         FERMAX = MAX (ABS(REFMAX), ABS(REFMIN))
         DERMAX = MAX (ABS(REDMAX), ABS(REDMIN))
C
         FAILNX = (NEXT(1) + NEXT(2)) .NE. 10
         FAILOC = FAILNX .OR. (MAX(FERMAX, DERMAX) .GT. TOL2)
      ENDIF
      FAIL = FAIL .OR. FAILOC
C
C  PRINT SUMMARY.
C
      IF (KPRINT .GE. 3)  THEN
         WRITE (LOUT, 2004)  NPTS-10, NEXT
C
         WRITE (LOUT, 2005)  'MIN', AEFMIN, REFMIN, AEDMIN, REDMIN
         WRITE (LOUT, 2006) XAFMIN, XRFMIN, XADMIN, XRDMIN
         WRITE (LOUT, 2005)  'MAX', AEFMAX, REFMAX, AEDMAX, REDMAX
         WRITE (LOUT, 2006) XAFMAX, XRFMAX, XADMAX, XRDMAX
      ENDIF
C
      IF (KPRINT .GE. 2)  THEN
         IF (FAILOC) THEN
            IF (FERMAX .GT. TOL2)  WRITE (LOUT, 3006) 'F', FERMAX, TOL2
            IF (DERMAX .GT. TOL2)  WRITE (LOUT, 3006) 'D', DERMAX, TOL2
            IF (FAILNX)  WRITE (LOUT, 4006)  NEXT
         ELSE
            WRITE (LOUT, 5006)
         ENDIF
      ENDIF
C
C  CHECK THAT CHFEV AGREES WITH CHFDV.
C
C     -----------------------------------------------------------------
      CALL CHFEV (X1, X2, F1, F2, D1, D2, NPTS, XEV, FEV2, NEXT2, IERR)
C     -----------------------------------------------------------------
      IF (IERR .NE. 0)  THEN
         FAILOC = .TRUE.
         IF (KPRINT .GE. 2)  WRITE (LOUT, 3007)  IERR
      ELSE
         AEFMAX = ABS(FEV2(1) - FEV(1))
         XAFMAX = XEV(1)
         DO 40  I = 2, NPTS
            AEF = ABS(FEV2(I) - FEV(I))
            IF (AEF .GT. AEFMAX)  THEN
               AEFMAX = AEF
               XAFMAX = XEV(I)
            ENDIF
   40    CONTINUE
         FAILNX = (NEXT2(1).NE.NEXT(1)) .OR. (NEXT2(2).NE.NEXT(2))
         FAILOC = FAILNX .OR. (AEFMAX.NE.ZERO)
         IF (KPRINT .GE. 2)  THEN
            IF (FAILOC)  THEN
               WRITE (LOUT, 3008)
               IF (AEFMAX.NE.ZERO)  WRITE (LOUT, 3009)  AEFMAX, XAFMAX
               IF (FAILNX)  WRITE (LOUT, 4009)  NEXT2, NEXT
            ELSE
               WRITE (LOUT, 5009)
            ENDIF
         ENDIF
      ENDIF
C
      FAIL = FAIL .OR. FAILOC
C
C  GO BACK FOR ANOTHER INTERVAL.
C
   90 CONTINUE
C
      RETURN
C
C  FORMATS.
C
 2000 FORMAT (/10X,'CHFDV ACCURACY TEST')
 2001 FORMAT (10X,A2,' =',1P,E18.10,5X,A2,' =',E18.10)
 2002 FORMAT (/' ERRORS AT ENDPOINTS:',40X,'(NEXT =',2I3,')'
     *        // 1P,4X,'F1:',E13.5,4X,'F2:',E13.5,
     *              4X,'D1:',E13.5,4X,'D2:',E13.5)
 2003 FORMAT (1P,4(7X,E13.5))
 2004 FORMAT (/' ERRORS AT ',I5,' INTERIOR POINTS + 10 OUTSIDE:',
     *                15X,'(NEXT =',2I3,')'
     *        //30X,'FUNCTION',17X,'DERIVATIVE'
     *         /15X,2(11X,'ABS',9X,'REL') )
 2005 FORMAT (/5X,A3,'IMUM ERROR:  ',1P,2E12.4,2X,2E12.4)
 2006 FORMAT ( 5X,'LOCATED AT X =  ',1P,2E12.4,2X,2E12.4)
 3000 FORMAT (//10X,'EVCHCK RESULTS'/10X,'--------------')
 3001 FORMAT (/10X,'INTERVAL = (',1P,E12.5,',',E12.5,' ):' )
 3002 FORMAT (/' ***** CHFDV FAILED TO REPRODUCE ENDPOINT VALUES.')
 3003 FORMAT (/' ***** CHFEV DOES NOT AGREE WITH CHFDV AT ENDPOINTS.')
 3006 FORMAT (/' ***** MAXIMUM RELATIVE ERROR IN ',A1,' =',1P,E12.5,','
     *        /        17X,'EXCEEDS TOLERANCE =',E12.5)
 3007 FORMAT (/' ***** ERROR ***** CHFEV RETURNED IERR =',I5)
 3008 FORMAT (/' ***** CHFEV DID NOT AGREE WITH CHFDV:')
 3009 FORMAT (7X,'MAXIMUM DIFFERENCE ',1P,E12.5,
     *                '; OCCURRED AT X =',E12.5)
 4003 FORMAT (/' ***** ERROR ***** CHFDV RETURNED IERR =',I5)
 4006 FORMAT (/' ***** REPORTED NEXT =',2I5,'   RATHER THAN    4    6')
 4009 FORMAT (7X,'REPORTED NEXT =',2I3,'   RATHER THAN ',2I3)
 5006 FORMAT (/' CHFDV RESULTS OK.')
 5009 FORMAT (/' CHFEV AGREES WITH CHFDV.')
C------------- LAST LINE OF EVCHCK FOLLOWS -----------------------------
      END
      SUBROUTINE EVERCK (LOUT, KPRINT, FAIL)

c*********************************************************************72
c
cc EVERCK tests error returns from PCHIP evaluators for PCHQK1.
c
C***BEGIN PROLOGUE  EVERCK
C***SUBSIDIARY
C***PURPOSE  Test error returns from PCHIP evaluators for PCHQK1.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (EVERCK-S, DEVERK-D)
C***KEYWORDS  PCHIP EVALUATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C --------- CODE TO TEST ERROR RETURNS FROM PCHIP EVALUATORS. ---------
C
C
C     FORTRAN LIBRARY ROUTINES USED:  (WRITE).
C     SLATEC LIBRARY ROUTINES USED:  CHFDV, CHFEV, PCHFD, PCHFE,
C                                    XERDMP, XGETF, XSETF.
C     OTHER ROUTINES USED:  COMP.
C
C***ROUTINES CALLED  CHFDV, CHFEV, COMP, PCHFD, PCHFE, XERDMP, XGETF,
C                    XSETF
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   820715  CONVERTED TO QUICK CHECK FOR SLATEC LIBRARY.
C   890207  ADDED CALLS TO ERROR HANDLER.
C   890316  Added call to XERDMP if KPRINT.GT.2 (FNF).
C   890629  Appended E0 to real constants to reduce S.P./D.P.
C           differences.
C   890706  Cosmetic changes to prologue.  (WRB)
C   891009  Removed unreferenced statement label.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900309  Added COMP to list of routines called.  (FNF)
C   900315  Revised prologue and improved some output formats.  (FNF)
C   900316  Deleted INCFD tests because some compilers object to them,
C           and made additional minor cosmetic changes.  (FNF)
C   900322  Made miscellaneous cosmetic changes.  (FNF)
C   910708  Minor modifications in use of KPRINT.  (WRB)
C   930504  Removed parens from constants in WRITE statements.  (FNF)
C***END PROLOGUE  EVERCK
C
C  Declare arguments.
C
      INTEGER  LOUT, KPRINT
      LOGICAL  FAIL
C
C  DECLARATIONS.
C
      INTEGER  I, IERR, KONTRL, N, NERR, NEXT(2)
      REAL  D(10), DUM, F(10), TEMP, X(10)
      LOGICAL  COMP, SKIP
C
C  INITIALIZE.
C
      PARAMETER (N = 10)
C***FIRST EXECUTABLE STATEMENT  EVERCK
      NERR = 0
C
      CALL XGETF (KONTRL)
      IF (KPRINT .LE. 2) THEN
         CALL XSETF (0)
      ELSE
         CALL XSETF (1)
      ENDIF
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 2000)
      IF (KPRINT .GE. 2)  WRITE (LOUT, 5000)
C
C  FIRST, TEST CHFEV AND CHFDV.
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -1
      CALL CHFEV (0.E0, 1.E0, 3.E0, 7.E0, 3.E0, 6.E0, 0, DUM, DUM,
     * NEXT, IERR)
      IF (.NOT. COMP (IERR, -1, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -2
      CALL CHFEV (1.E0, 1.E0, 3.E0, 7.E0, 3.E0, 6.E0, 1, DUM, DUM,
     * NEXT, IERR)
      IF (.NOT. COMP (IERR, -2, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -1
      CALL CHFDV (0.E0, 1.E0, 3.E0, 7.E0, 3.E0, 6.E0, 0, DUM, DUM, DUM,
     * NEXT, IERR)
      IF (.NOT. COMP (IERR, -1, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -2
      CALL CHFDV (1.E0, 1.E0, 3.E0, 7.E0, 3.E0, 6.E0, 1, DUM, DUM, DUM,
     * NEXT, IERR)
      IF (.NOT. COMP (IERR, -2, LOUT, KPRINT) )  NERR = NERR + 1
C
C  SET UP PCH DEFINITION.
C
      DO 10  I = 1, N
         X(I) = I
         F(I) = I + 2
         D(I) = 1.E0
   10 CONTINUE
C
C  SWAP POINTS 4 AND 7, SO X-ARRAY IS OUT OF ORDER.
C
      TEMP = X(4)
      X(4) = X(7)
      X(7) = TEMP
C
C  NOW, TEST PCHFE AND PCHFD.
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -1
      SKIP = .FALSE.
      CALL PCHFE (1, X, F, D, 1, SKIP, 0, DUM, DUM, IERR)
      IF (.NOT. COMP (IERR, -1, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -3
      SKIP = .FALSE.
      CALL PCHFE (N, X, F, D, 1, SKIP, 0, DUM, DUM, IERR)
      IF (.NOT. COMP (IERR, -3, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -4
      SKIP = .TRUE.
      CALL PCHFE (N, X, F, D, 1, SKIP, 0, DUM, DUM, IERR)
      IF (.NOT. COMP (IERR, -4, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -1
      SKIP = .FALSE.
      CALL PCHFD (1, X, F, D, 1, SKIP, 0, DUM, DUM, DUM, IERR)
      IF (.NOT. COMP (IERR, -1, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -3
      SKIP = .FALSE.
      CALL PCHFD (N, X, F, D, 1, SKIP, 0, DUM, DUM, DUM, IERR)
      IF (.NOT. COMP (IERR, -3, LOUT, KPRINT) )  NERR = NERR + 1
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 5001)  -4
      SKIP = .TRUE.
      CALL PCHFD (N, X, F, D, 1, SKIP, 0, DUM, DUM, DUM, IERR)
      IF (.NOT. COMP (IERR, -4, LOUT, KPRINT) )  NERR = NERR + 1
C
C  SUMMARIZE RESULTS.
C
      IF (KPRINT .GT. 2)  CALL XERDMP
      IF (NERR .EQ. 0)  THEN
         FAIL = .FALSE.
         IF (KPRINT .GE. 2)  WRITE (LOUT, 5002)
      ELSE
         FAIL = .TRUE.
         IF (KPRINT .GE. 2)  WRITE (LOUT, 5003)  NERR
      ENDIF
C
C  TERMINATE.
C
      CALL XSETF (KONTRL)
      RETURN
C
C  FORMATS.
C
 2000 FORMAT ('1'//10X,'TEST ERROR RETURNS')
 5000 FORMAT (//10X,'EVERCK RESULTS'/10X,'--------------')
 5001 FORMAT (/' THIS CALL SHOULD RETURN IERR =',I3)
 5002 FORMAT (/' ALL ERROR RETURNS OK.')
 5003 FORMAT (//' ***** TROUBLE IN EVERCK *****'
     *        //5X,I5,' TESTS FAILED TO GIVE EXPECTED RESULTS.')
C------------- LAST LINE OF EVERCK FOLLOWS -----------------------------
      END
      SUBROUTINE EVPCCK (LOUT, KPRINT, X, Y, F, FX, FY, XE, YE, FE, DE,
     +   FE2, FAIL)

c*********************************************************************72
c
cc EVPCCK tests usage of increment argument in PCHFD and PCHFE for PCHQK1.
c
C***BEGIN PROLOGUE  EVPCCK
C***SUBSIDIARY
C***PURPOSE  Test usage of increment argument in PCHFD and PCHFE for
C            PCHQK1.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (EVPCCK-S, DEVPCK-D)
C***KEYWORDS  PCHIP EVALUATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C ---- CODE TO TEST USAGE OF INCREMENT ARGUMENT IN PCHFD AND PCHFE ----
C
C     EVALUATES A BICUBIC FUNCTION AND ITS FIRST PARTIAL DERIVATIVES
C     ON A 4X6 MESH CONTAINED IN A 10X10 ARRAY.
C
C     INTERPOLATION OF THESE DATA ALONG MESH LINES IN EITHER DIMENSION
C     SHOULD AGREE WITH CORRECT FUNCTION WITHIN ROUNDOFF ERROR.
C
C     ARRAYS ARE ARGUMENTS ONLY TO ALLOW SHARING STORAGE WITH OTHER
C     TEST ROUTINES.
C
C     NOTE:  RUN WITH KPRINT=4 FOR FULL GORY DETAILS (10 PAGES WORTH).
C
C
C     FORTRAN INTRINSICS USED:  ABS.
C     FORTRAN LIBRARY ROUTINES USED:  (WRITE).
C     SLATEC LIBRARY ROUTINES USED:  PCHFD, PCHFE, R1MACH.
C
C***ROUTINES CALLED  PCHFD, PCHFE, R1MACH
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   820714  CONVERTED TO QUICK CHECK FOR SLATEC LIBRARY.
C   820715  1. CORRECTED SOME FORMATS.
C           2. ADDED CALL TO R1MACH TO SET MACHEP.
C   890406  1. Modified to make sure final elements of X and XE
C             agree, to avoid possible failure due to roundoff
C             error.
C           2. Added printout of TOL in case of failure.
C           3. Minor cosmetic changes.
C   890407  Appended E0 to real constants to reduce S.P./D.P.
C           differences.
C   890706  Cosmetic changes to prologue.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891004  Cosmetic changes to prologue.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  Revised prologue and improved some output formats.  (FNF)
C   900316  Additional minor cosmetic changes.  (FNF)
C   900321  Made miscellaneous cosmetic changes.  (FNF)
C   901130  Made many changes to output:  (FNF)
C           1. Reduced amount of output for KPRINT=3.  (Now need to
C              use KPRINT=4 for full output.)
C           2. Added 1P's to formats and revised some to reduce maximum
C              line length.
C   910708  Minor modifications in use of KPRINT.  (WRB)
C   930317  Improved output formats.  (FNF)
C***END PROLOGUE  EVPCCK
C
C  Declare arguments.
C
      INTEGER  LOUT, KPRINT
      LOGICAL  FAIL
      REAL  X(10), Y(10), F(10,10), FX(10,10), FY(10,10),
     *      XE(51), YE(51), FE(51), DE(51), FE2(51)
C
C  DECLARATIONS.
C
      INTEGER  I, IER2, IERR, INC, J, K, NE, NERR, NMAX, NX, NY
      LOGICAL  FAILD, FAILE, FAILOC, SKIP
      REAL  DERMAX, DERR, DTRUE, DX, FDIFF, FDIFMX, FERMAX, FERR,
     *      FTRUE, MACHEP, TOL, PDERMX, PDIFMX, PFERMX, ZERO
      REAL  R1MACH
C
C  DEFINE TEST FUNCTION AND DERIVATIVES.
C
      REAL  AX, AY, FCN, DFDX, DFDY
      FCN(AX,AY)  =  AX*(AY*AY)*(AX*AX + 1.E0)
      DFDX(AX,AY) = (AY*AY)*(3.E0*AX*AX + 1.E0)
      DFDY(AX,AY) =   2.E0*AX*AY*(AX*AX + 1.E0)
C
      DATA  NMAX /10/,  NX /4/,  NY /6/
      DATA  NE /51/
      DATA  ZERO /0.E0/
C
C  INITIALIZE.
C
C***FIRST EXECUTABLE STATEMENT  EVPCCK
      MACHEP = R1MACH(4)
      TOL = 10.E0*MACHEP
C
      FAIL = .FALSE.
C
C  SET UP 4-BY-6 MESH IN A 10-BY-10 ARRAY:
C     X =  0.25(0.25)1.   ;
C     Y = -0.75(0.5 )1.75 .
C
      DO 1  I = 1, NX-1
         X(I) = 0.25E0*I
    1 CONTINUE
      X(NX) = 1.E0
      DO 5  J = 1, NY
         Y(J) = 0.5E0*J - 1.25E0
         DO 4  I = 1, NX
             F(I,J) = FCN (X(I), Y(J))
            FX(I,J) = DFDX(X(I), Y(J))
            FY(I,J) = DFDY(X(I), Y(J))
    4    CONTINUE
    5 CONTINUE
C
C  SET UP EVALUATION POINTS:
C     XE =  0.(0.02)1. ;
C     YE = -2.(0.08)2. .
C
      DX = 1.E0/(NE-1)
      DO 8  K = 1, NE-1
         XE(K) = DX*(K-1)
         YE(K) = 4.E0*XE(K) - 2.E0
    8 CONTINUE
      XE(NE) = 1.E0
      YE(NE) = 2.E0
C
      IF (KPRINT .GE. 3)  WRITE (LOUT, 1000)
      IF (KPRINT .GE. 2)  WRITE (LOUT, 1001)
C
C  EVALUATE ON HORIZONTAL MESH LINES (Y FIXED, X RUNNING) ..............
C
      NERR = 0
      INC = 1
      SKIP = .FALSE.
      DO 20  J = 1, NY
C        --------------------------------------------------------------
         CALL PCHFD (NX, X, F(1,J), FX(1,J), INC, SKIP, NE, XE, FE, DE,
     *               IERR)
C        --------------------------------------------------------------
         IF (KPRINT .GE. 3)
     *       WRITE (LOUT, 2000)  INC, 'J', J, 'Y', Y(J), IERR
         IF (IERR .LT. 0)  GO TO 15
         IF (KPRINT .GT. 3)  WRITE (LOUT, 2001)  'X'
C
C        PCHFE SHOULD AGREE EXACTLY WITH PCHFD.
C
C        -----------------------------------------------------------
         CALL PCHFE (NX, X, F(1,J), FX(1,J), INC, SKIP, NE, XE, FE2,
     *               IER2)
C        -----------------------------------------------------------
C
         DO 10  K = 1, NE
            FTRUE =  FCN(XE(K), Y(J))
            FERR = FE(K) - FTRUE
            DTRUE = DFDX(XE(K), Y(J))
            DERR = DE(K) - DTRUE
            IF (KPRINT .GT. 3)
     *         WRITE (LOUT, 2002)  XE(K), FTRUE, FE(K), FERR,
     *                                    DTRUE, DE(K), DERR
            IF (K .EQ. 1)  THEN
C              INITIALIZE.
               FERMAX = ABS(FERR)
               PFERMX = XE(1)
               DERMAX = ABS(DERR)
               PDERMX = XE(1)
               FDIFMX = ABS(FE2(1) - FE(1))
               PDIFMX = XE(1)
            ELSE
C              SELECT.
               FERR = ABS(FERR)
               IF (FERR .GT. FERMAX)  THEN
                  FERMAX = FERR
                  PFERMX = XE(K)
               ENDIF
               DERR = ABS(DERR)
               IF (DERR .GT. DERMAX)  THEN
                  DERMAX = DERR
                  PDERMX = XE(K)
               ENDIF
               FDIFF = ABS(FE2(K) - FE(K))
               IF (FDIFF .GT. FDIFMX)  THEN
                  FDIFMX = FDIFF
                  PDIFMX = XE(K)
               ENDIF
            ENDIF
   10    CONTINUE
C
         FAILD = (FERMAX.GT.TOL) .OR. (DERMAX.GT.TOL)
         FAILE = FDIFMX .NE. ZERO
         FAILOC = FAILD .OR. FAILE .OR. (IERR.NE.13) .OR. (IER2.NE.IERR)
C
         IF (FAILOC .AND. (KPRINT.GE.2))
     *      WRITE (LOUT, 2003)  'J', J, 'Y', Y(J)
C
         IF ((KPRINT.GE.3) .OR. (FAILD.AND.(KPRINT.EQ.2)) )
     *      WRITE (LOUT, 2004)  FERMAX, PFERMX, DERMAX, PDERMX
         IF (FAILD .AND. (KPRINT.GE.2))  WRITE (LOUT, 2014)  TOL
C
         IF ((KPRINT.GE.3) .OR. (FAILE.AND.(KPRINT.EQ.2)) )
     *      WRITE (LOUT, 2005)  FDIFMX, PDIFMX
C
         IF ((IERR.NE.13) .AND. (KPRINT.GE.2))
     *      WRITE (LOUT, 2006)  'D', IERR, 13
C
         IF ((IER2.NE.IERR) .AND. (KPRINT.GE.2))
     *      WRITE (LOUT, 2006)  'E', IER2, IERR
         GO TO 19
C
   15    CONTINUE
         FAILOC = .TRUE.
         IF (KPRINT .GE. 2)  WRITE (LOUT, 3000) IERR
C
   19    CONTINUE
         IF (FAILOC)  NERR = NERR + 1
         FAIL = FAIL .OR. FAILOC
   20 CONTINUE
C
      IF (KPRINT .GE. 2)  THEN
         IF (NERR .GT. 0)  THEN
            WRITE (LOUT, 3001)  NERR, 'J'
         ELSE
            WRITE (LOUT, 4000)  'J'
         ENDIF
      ENDIF
C
C  EVALUATE ON VERTICAL MESH LINES (X FIXED, Y RUNNING) ................
C
      NERR = 0
      INC = NMAX
      SKIP = .FALSE.
      DO 40  I = 1, NX
C        --------------------------------------------------------------
         CALL PCHFD (NY, Y, F(I,1), FY(I,1), INC, SKIP, NE, YE, FE, DE,
     *               IERR)
C        --------------------------------------------------------------
         IF (KPRINT .GE. 3)
     *       WRITE (LOUT, 2000)  INC, 'I', I, 'X', X(I), IERR
         IF (IERR .LT. 0)  GO TO 35
         IF (KPRINT .GT. 3)  WRITE (LOUT, 2001)  'Y'
C
C        PCHFE SHOULD AGREE EXACTLY WITH PCHFD.
C
C        -----------------------------------------------------------
         CALL PCHFE (NY, Y, F(I,1), FY(I,1), INC, SKIP, NE, YE, FE2,
     *               IER2)
C        -----------------------------------------------------------
C
         DO 30  K = 1, NE
            FTRUE =  FCN(X(I), YE(K))
            FERR = FE(K) - FTRUE
            DTRUE = DFDY(X(I), YE(K))
            DERR = DE(K) - DTRUE
            IF (KPRINT .GT. 3)
     *         WRITE (LOUT, 2002)  YE(K), FTRUE, FE(K), FERR,
     *                                    DTRUE, DE(K), DERR
            IF (K .EQ. 1)  THEN
C              INITIALIZE.
               FERMAX = ABS(FERR)
               PFERMX = YE(1)
               DERMAX = ABS(DERR)
               PDERMX = YE(1)
               FDIFMX = ABS(FE2(1) - FE(1))
               PDIFMX = YE(1)
            ELSE
C              SELECT.
               FERR = ABS(FERR)
               IF (FERR .GT. FERMAX)  THEN
                  FERMAX = FERR
                  PFERMX = YE(K)
               ENDIF
               DERR = ABS(DERR)
               IF (DERR .GT. DERMAX)  THEN
                  DERMAX = DERR
                  PDERMX = YE(K)
               ENDIF
               FDIFF = ABS(FE2(K) - FE(K))
               IF (FDIFF .GT. FDIFMX)  THEN
                  FDIFMX = FDIFF
                  PDIFMX = YE(K)
               ENDIF
            ENDIF
   30    CONTINUE
C
         FAILD = (FERMAX.GT.TOL) .OR. (DERMAX.GT.TOL)
         FAILE = FDIFMX .NE. ZERO
         FAILOC = FAILD .OR. FAILE .OR. (IERR.NE.20) .OR. (IER2.NE.IERR)
C
         IF (FAILOC .AND. (KPRINT.GE.2))
     *      WRITE (LOUT, 2003)  'I', I, 'X', X(I)
C
         IF ((KPRINT.GE.3) .OR. (FAILD.AND.(KPRINT.EQ.2)) )
     *      WRITE (LOUT, 2004)  FERMAX, PFERMX, DERMAX, PDERMX
         IF (FAILD .AND. (KPRINT.GE.2))  WRITE (LOUT, 2014)  TOL
C
         IF ((KPRINT.GE.3) .OR. (FAILE.AND.(KPRINT.EQ.2)) )
     *      WRITE (LOUT, 2005)  FDIFMX, PDIFMX
C
         IF ((IERR.NE.20) .AND. (KPRINT.GE.2))
     *      WRITE (LOUT, 2006)  'D', IERR, 20
C
         IF ((IER2.NE.IERR) .AND. (KPRINT.GE.2))
     *      WRITE (LOUT, 2006)  'E', IER2, IERR
         GO TO 39
C
   35    CONTINUE
         FAILOC = .TRUE.
         IF (KPRINT .GE. 2)  WRITE (LOUT, 3000) IERR
C
   39    CONTINUE
         IF (FAILOC)  NERR = NERR + 1
         FAIL = FAIL .OR. FAILOC
   40 CONTINUE
C
      IF (KPRINT .GE. 2)  THEN
         IF (NERR .GT. 0)  THEN
            WRITE (LOUT, 3001)  NERR, 'I'
         ELSE
            WRITE (LOUT, 4000)  'I'
         ENDIF
      ENDIF
C
C  TERMINATE.
C
      RETURN
C
C  FORMATS.
C
 1000 FORMAT ('1'//10X,'TEST PCHFE AND PCHFD')
 1001 FORMAT (//10X,'EVPCCK RESULTS'/10X,'--------------')
 2000 FORMAT (//20X,'PCHFD INCREMENT TEST -- INCFD = ',I2
     *        /15X,'ON ',A1,'-LINE ',I2,',  ',A1,' =',F8.4,
     *           '  --  IERR =',I3)
 2001 FORMAT ( /3X,A1,'E',10X,'F',8X,'FE',9X,'DIFF',
     *                    13X,'D',8X,'DE',9X,'DIFF')
 2002 FORMAT (F7.2,2(2X,2F10.5,1P,E15.5,0P))
 2003 FORMAT (/' ***** PCHFD AND/OR PCHFE FAILED ON ',A1,'-LINE ',I1,
     *                             ',  ',A1,' =',F8.4)
 2004 FORMAT (/17X,'  MAXIMUM ERROR IN FUNCTION =',1P,
     *                                   1P,E13.5,0P,' (AT',F6.2,'),'
     *        /31X,    'IN DERIVATIVE =',1P,E13.5,0P,' (AT',F6.2,').' )
 2005 FORMAT ( '  MAXIMUM DIFFERENCE BETWEEN PCHFE AND PCHFD =',
     *                                   1P,E13.5,0P,' (AT',F6.2,').' )
 2006 FORMAT (/'  PCHF',A1,' RETURNED IERR = ',I2,' INSTEAD OF ',I2)
 2014 FORMAT ('  *** BOTH SHOULD BE .LE. TOL =',1P,E12.5,' ***')
 3000 FORMAT (//' ***** ERROR ***** PCHFD RETURNED IERR =',I5//)
 3001 FORMAT (//' ***** ERROR ***** PCHFD AND/OR PCHFE FAILED ON',I2,
     *                                1X,A1,'-LINES.'//)
 4000 FORMAT (/' PCHFD AND PCHFE OK ON ',A1,'-LINES.')
C------------- LAST LINE OF EVPCCK FOLLOWS -----------------------------
      END
      SUBROUTINE FDTRUE (X, F, D)

c*********************************************************************72
c
cc FDTRUE computes exact function values for EVCHCK.
c
C***BEGIN PROLOGUE  FDTRUE
C***SUBSIDIARY
C***PURPOSE  Compute exact function values for EVCHCK.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (FDTRUE-S, DFDTRU-D)
C***KEYWORDS  PCHIP EVALUATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C        COMPUTE EXACT FUNCTION VALUES IN DOUBLE PRECISION.
C
C                   F(X) = X*(X+1)*(X-2)
C
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   890618  REVISION DATE from Version 3.2
C   890706  Cosmetic changes to prologue.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  Revised prologue.  (FNF)
C   900316  Deleted variables ONE and TWO.  (FNF)
C   900321  Changed name of d.p. version from DFTRUE to DFDTRU.
C***END PROLOGUE  FDTRUE
      REAL  X, F, D
      DOUBLE PRECISION  FACT1, FACT2, XX
C
C***FIRST EXECUTABLE STATEMENT  FDTRUE
      XX = X
      FACT1 = XX + 1
      FACT2 = XX - 2
      F = XX * FACT1 * FACT2
      D = FACT1*FACT2 + XX*(FACT1 + FACT2)
C
      RETURN
C------------- LAST LINE OF FDTRUE FOLLOWS -----------------------------
      END
      SUBROUTINE FDUMP ( )

c*********************************************************************72
c
cc FDUMP can be used to print a symbolic dump.
c
C***BEGIN PROLOGUE  FDUMP
C***PURPOSE  Symbolic dump (should be locally written).
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3
C***TYPE      ALL (FDUMP-A)
C***KEYWORDS  ERROR, XERMSG
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C        ***Note*** Machine Dependent Routine
C        FDUMP is intended to be replaced by a locally written
C        version which produces a symbolic dump.  Failing this,
C        it should be replaced by a version which prints the
C        subprogram nesting list.  Note that this dump must be
C        printed on each of up to five files, as indicated by the
C        XGETUA routine.  See XSETUA and XGETUA for details.
C
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C***END PROLOGUE  FDUMP
C***FIRST EXECUTABLE STATEMENT  FDUMP
      RETURN
      END
      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      function i1mach ( i )

c*********************************************************************72
c
cc I1MACH returns integer machine dependent constants.
c
c  Discussion:
c
c    Input/output unit numbers.
c
c      I1MACH(1) = the standard input unit.
c      I1MACH(2) = the standard output unit.
c      I1MACH(3) = the standard punch unit.
c      I1MACH(4) = the standard error message unit.
c
c    Words.
c
c      I1MACH(5) = the number of bits per integer storage unit.
c      I1MACH(6) = the number of characters per integer storage unit.
c
c    Integers.
c
c    Assume integers are represented in the S digit base A form:
c
c      Sign * (X(S-1)*A^(S-1) + ... + X(1)*A + X(0))
c
c    where 0 <= X(1:S-1) < A.
c
c      I1MACH(7) = A, the base.
c      I1MACH(8) = S, the number of base A digits.
c      I1MACH(9) = A^S-1, the largest integer.
c
c    Floating point numbers
c
c    Assume floating point numbers are represented in the T digit 
c    base B form:
c
c      Sign * (B^E) * ((X(1)/B) + ... + (X(T)/B^T) )
c
c    where 0 <= X(I) < B for I=1 to T, 0 < X(1) and EMIN <= E <= EMAX.
c
c      I1MACH(10) = B, the base.
c
c    Single precision
c
c      I1MACH(11) = T, the number of base B digits.
c      I1MACH(12) = EMIN, the smallest exponent E.
c      I1MACH(13) = EMAX, the largest exponent E.
c
c    Double precision
c
c      I1MACH(14) = T, the number of base B digits.
c      I1MACH(15) = EMIN, the smallest exponent E.
c      I1MACH(16) = EMAX, the largest exponent E.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    25 April 2007
c
c  Author:
c
c    Original FORTRAN77 version by Phyllis Fox, Andrew Hall, Norman Schryer.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Phyllis Fox, Andrew Hall, Norman Schryer,
c    Algorithm 528,
c    Framework for a Portable Library,
c    ACM Transactions on Mathematical Software,
c    Volume 4, Number 2, June 1978, page 176-188.
c
c  Parameters:
c
c    Input, integer I, chooses the parameter to be returned.
c    1 <= I <= 16.
c
c    Output, integer I1MACH, the value of the chosen parameter.
c
      implicit none

      integer i
      integer i1mach

      if ( i < 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I1MACH - Fatal error!'
        write ( *, '(a)' ) '  The input argument I is out of bounds.'
        write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 16.'
        write ( *, '(a,i12)' ) '  I = ', i
        i1mach = 0
        stop
      else if ( i == 1 ) then
        i1mach = 5
      else if ( i == 2 ) then
        i1mach = 6
      else if ( i == 3 ) then
        i1mach = 7
      else if ( i == 4 ) then
        i1mach = 6
      else if ( i == 5 ) then
        i1mach = 32
      else if ( i == 6 ) then
        i1mach = 4
      else if ( i == 7 ) then
        i1mach = 2
      else if ( i == 8 ) then
        i1mach = 31
      else if ( i == 9 ) then
        i1mach = 2147483647
      else if ( i == 10 ) then
        i1mach = 2
      else if ( i == 11 ) then
        i1mach = 24
      else if ( i == 12 ) then
        i1mach = -125
      else if ( i == 13 ) then
        i1mach = 128
      else if ( i == 14 ) then
        i1mach = 53
      else if ( i == 15 ) then
        i1mach = -1021
      else if ( i == 16 ) then
        i1mach = 1024
      else if ( 16 < i ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I1MACH - Fatal error!'
        write ( *, '(a)' ) '  The input argument I is out of bounds.'
        write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 16.'
        write ( *, '(a,i12)' ) '  I = ', i
        i1mach = 0
        stop
      end if

      return
      end
      SUBROUTINE INTRV (XT, LXT, X, ILO, ILEFT, MFLAG)

c*********************************************************************72
c
cc INTRV seeks the interval containing or nearest to a given point.
c
C***BEGIN PROLOGUE  INTRV
C***PURPOSE  Compute the largest integer ILEFT in 1 .LE. ILEFT .LE. LXT
C            such that XT(ILEFT) .LE. X where XT(*) is a subdivision
C            of the X interval.
C***LIBRARY   SLATEC
C***CATEGORY  E3, K6
C***TYPE      SINGLE PRECISION (INTRV-S, DINTRV-D)
C***KEYWORDS  B-SPLINE, DATA FITTING, INTERPOLATION, SPLINES
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C     Written by Carl de Boor and modified by D. E. Amos
C
C     Abstract
C         INTRV is the INTERV routine of the reference.
C
C         INTRV computes the largest integer ILEFT in 1 .LE. ILEFT .LE.
C         LXT such that XT(ILEFT) .LE. X where XT(*) is a subdivision of
C         the X interval.  Precisely,
C
C                      X .LT. XT(1)                1         -1
C         if  XT(I) .LE. X .LT. XT(I+1)  then  ILEFT=I  , MFLAG=0
C           XT(LXT) .LE. X                         LXT        1,
C
C         That is, when multiplicities are present in the break point
C         to the left of X, the largest index is taken for ILEFT.
C
C     Description of Arguments
C         Input
C          XT      - XT is a knot or break point vector of length LXT
C          LXT     - length of the XT vector
C          X       - argument
C          ILO     - an initialization parameter which must be set
C                    to 1 the first time the spline array XT is
C                    processed by INTRV.
C
C         Output
C          ILO     - ILO contains information for efficient process-
C                    ing after the initial call, and ILO must not be
C                    changed by the user.  Distinct splines require
C                    distinct ILO parameters.
C          ILEFT   - largest integer satisfying XT(ILEFT) .LE. X
C          MFLAG   - signals when X lies out of bounds
C
C     Error Conditions
C         None
C
C***REFERENCES  Carl de Boor, Package for calculating with B-splines,
C                 SIAM Journal on Numerical Analysis 14, 3 (June 1977),
C                 pp. 441-472.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   800901  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  INTRV
C
      INTEGER IHI, ILEFT, ILO, ISTEP, LXT, MFLAG, MIDDLE
      REAL X, XT
      DIMENSION XT(*)
C***FIRST EXECUTABLE STATEMENT  INTRV
      IHI = ILO + 1
      IF (IHI.LT.LXT) GO TO 10
      IF (X.GE.XT(LXT)) GO TO 110
      IF (LXT.LE.1) GO TO 90
      ILO = LXT - 1
      IHI = LXT
C
   10 IF (X.GE.XT(IHI)) GO TO 40
      IF (X.GE.XT(ILO)) GO TO 100
C
C *** NOW X .LT. XT(IHI) . FIND LOWER BOUND
      ISTEP = 1
   20 IHI = ILO
      ILO = IHI - ISTEP
      IF (ILO.LE.1) GO TO 30
      IF (X.GE.XT(ILO)) GO TO 70
      ISTEP = ISTEP*2
      GO TO 20
   30 ILO = 1
      IF (X.LT.XT(1)) GO TO 90
      GO TO 70
C *** NOW X .GE. XT(ILO) . FIND UPPER BOUND
   40 ISTEP = 1
   50 ILO = IHI
      IHI = ILO + ISTEP
      IF (IHI.GE.LXT) GO TO 60
      IF (X.LT.XT(IHI)) GO TO 70
      ISTEP = ISTEP*2
      GO TO 50
   60 IF (X.GE.XT(LXT)) GO TO 110
      IHI = LXT
C
C *** NOW XT(ILO) .LE. X .LT. XT(IHI) . NARROW THE INTERVAL
   70 MIDDLE = (ILO+IHI)/2
      IF (MIDDLE.EQ.ILO) GO TO 100
C     NOTE. IT IS ASSUMED THAT MIDDLE = ILO IN CASE IHI = ILO+1
      IF (X.LT.XT(MIDDLE)) GO TO 80
      ILO = MIDDLE
      GO TO 70
   80 IHI = MIDDLE
      GO TO 70
C *** SET OUTPUT AND RETURN
   90 MFLAG = -1
      ILEFT = 1
      RETURN
  100 MFLAG = 0
      ILEFT = ILO
      RETURN
  110 MFLAG = 1
      ILEFT = LXT
      RETURN
      END
      FUNCTION J4SAVE (IWHICH, IVALUE, ISET)

c*********************************************************************72
c
cc J4SAVE saves or recalls global variables needed by the error handler.
c
C***BEGIN PROLOGUE  J4SAVE
C***SUBSIDIARY
C***PURPOSE  Save or recall global variables needed by error
C            handling routines.
C***LIBRARY   SLATEC (XERROR)
C***TYPE      INTEGER (J4SAVE-I)
C***KEYWORDS  ERROR MESSAGES, ERROR NUMBER, RECALL, SAVE, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        J4SAVE saves and recalls several global variables needed
C        by the library error handling routines.
C
C     Description of Parameters
C      --Input--
C        IWHICH - Index of item desired.
C                = 1 Refers to current error number.
C                = 2 Refers to current error control flag.
C                = 3 Refers to current unit number to which error
C                    messages are to be sent.  (0 means use standard.)
C                = 4 Refers to the maximum number of times any
C                     message is to be printed (as set by XERMAX).
C                = 5 Refers to the total number of units to which
C                     each error message is to be written.
C                = 6 Refers to the 2nd unit for error messages
C                = 7 Refers to the 3rd unit for error messages
C                = 8 Refers to the 4th unit for error messages
C                = 9 Refers to the 5th unit for error messages
C        IVALUE - The value to be set for the IWHICH-th parameter,
C                 if ISET is .TRUE. .
C        ISET   - If ISET=.TRUE., the IWHICH-th parameter will BE
C                 given the value, IVALUE.  If ISET=.FALSE., the
C                 IWHICH-th parameter will be unchanged, and IVALUE
C                 is a dummy parameter.
C      --Output--
C        The (old) value of the IWHICH-th parameter will be returned
C        in the function value, J4SAVE.
C
C***SEE ALSO  XERMSG
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900205  Minor modifications to prologue.  (WRB)
C   900402  Added TYPE section.  (WRB)
C   910411  Added KEYWORDS section.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  J4SAVE
      LOGICAL ISET
      INTEGER IPARAM(9)
      SAVE IPARAM
      DATA IPARAM(1),IPARAM(2),IPARAM(3),IPARAM(4)/0,2,0,10/
      DATA IPARAM(5)/1/
      DATA IPARAM(6),IPARAM(7),IPARAM(8),IPARAM(9)/0,0,0,0/
C***FIRST EXECUTABLE STATEMENT  J4SAVE
      J4SAVE = IPARAM(IWHICH)
      IF (ISET) IPARAM(IWHICH) = IVALUE
      RETURN
      END
      SUBROUTINE PCHBS (N, X, F, D, INCFD, KNOTYP, NKNOTS, T, BCOEF,
     +   NDIM, KORD, IERR)

c*********************************************************************72
c
cc PCHBS: Piecewise cubic Hermite to B-Spline converter.
c
C***BEGIN PROLOGUE  PCHBS
C***PURPOSE  Piecewise Cubic Hermite to B-Spline converter.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3
C***TYPE      SINGLE PRECISION (PCHBS-S, DPCHBS-D)
C***KEYWORDS  B-SPLINES, CONVERSION, CUBIC HERMITE INTERPOLATION,
C             PIECEWISE CUBIC INTERPOLATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Computing and Mathematics Research Division
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C *Usage:
C
C        INTEGER  N, INCFD, KNOTYP, NKNOTS, NDIM, KORD, IERR
C        PARAMETER  (INCFD = ...)
C        REAL  X(nmax), F(INCFD,nmax), D(INCFD,nmax), T(2*nmax+4),
C       *      BCOEF(2*nmax)
C
C        CALL  PCHBS (N, X, F, D, INCFD, KNOTYP, NKNOTS, T, BCOEF,
C       *             NDIM, KORD, IERR)
C
C *Arguments:
C
C     N:IN  is the number of data points, N.ge.2 .  (not checked)
C
C     X:IN  is the real array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.   (not checked)
C           nmax, the dimension of X, must be .ge.N.
C
C     F:IN  is the real array of dependent variable values.
C           F(1+(I-1)*INCFD) is the value corresponding to X(I).
C           nmax, the second dimension of F, must be .ge.N.
C
C     D:IN  is the real array of derivative values at the data points.
C           D(1+(I-1)*INCFD) is the value corresponding to X(I).
C           nmax, the second dimension of D, must be .ge.N.
C
C     INCFD:IN  is the increment between successive values in F and D.
C           This argument is provided primarily for 2-D applications.
C           It may have the value 1 for one-dimensional applications,
C           in which case F and D may be singly-subscripted arrays.
C
C     KNOTYP:IN  is a flag to control the knot sequence.
C           The knot sequence T is normally computed from X by putting
C           a double knot at each X and setting the end knot pairs
C           according to the value of KNOTYP:
C              KNOTYP = 0:  Quadruple knots at X(1) and X(N).  (default)
C              KNOTYP = 1:  Replicate lengths of extreme subintervals:
C                           T( 1 ) = T( 2 ) = X(1) - (X(2)-X(1))  ;
C                           T(M+4) = T(M+3) = X(N) + (X(N)-X(N-1)).
C              KNOTYP = 2:  Periodic placement of boundary knots:
C                           T( 1 ) = T( 2 ) = X(1) - (X(N)-X(N-1));
C                           T(M+4) = T(M+3) = X(N) + (X(2)-X(1))  .
C              Here M=NDIM=2*N.
C           If the input value of KNOTYP is negative, however, it is
C           assumed that NKNOTS and T were set in a previous call.
C           This option is provided for improved efficiency when used
C           in a parametric setting.
C
C     NKNOTS:INOUT  is the number of knots.
C           If KNOTYP.GE.0, then NKNOTS will be set to NDIM+4.
C           If KNOTYP.LT.0, then NKNOTS is an input variable, and an
C              error return will be taken if it is not equal to NDIM+4.
C
C     T:INOUT  is the array of 2*N+4 knots for the B-representation.
C           If KNOTYP.GE.0, T will be returned by PCHBS with the
C              interior double knots equal to the X-values and the
C              boundary knots set as indicated above.
C           If KNOTYP.LT.0, it is assumed that T was set by a
C              previous call to PCHBS.  (This routine does **not**
C              verify that T forms a legitimate knot sequence.)
C
C     BCOEF:OUT  is the array of 2*N B-spline coefficients.
C
C     NDIM:OUT  is the dimension of the B-spline space.  (Set to 2*N.)
C
C     KORD:OUT  is the order of the B-spline.  (Set to 4.)
C
C     IERR:OUT  is an error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           "Recoverable" errors:
C              IERR = -4  if KNOTYP.GT.2 .
C              IERR = -5  if KNOTYP.LT.0 and NKNOTS.NE.(2*N+4).
C
C *Description:
C     PCHBS computes the B-spline representation of the PCH function
C     determined by N,X,F,D.  To be compatible with the rest of PCHIP,
C     PCHBS includes INCFD, the increment between successive values of
C     the F- and D-arrays.
C
C     The output is the B-representation for the function:  NKNOTS, T,
C     BCOEF, NDIM, KORD.
C
C *Caution:
C     Since it is assumed that the input PCH function has been
C     computed by one of the other routines in the package PCHIP,
C     input arguments N, X, INCFD are **not** checked for validity.
C
C *Restrictions/assumptions:
C     1. N.GE.2 .  (not checked)
C     2. X(i).LT.X(i+1), i=1,...,N .  (not checked)
C     3. INCFD.GT.0 .  (not checked)
C     4. KNOTYP.LE.2 .  (error return if not)
C    *5. NKNOTS = NDIM+4 = 2*N+4 .  (error return if not)
C    *6. T(2*k+1) = T(2*k) = X(k), k=1,...,N .  (not checked)
C
C       * Indicates this applies only if KNOTYP.LT.0 .
C
C *Portability:
C     Argument INCFD is used only to cause the compiler to generate
C     efficient code for the subscript expressions (1+(I-1)*INCFD) .
C     The normal usage, in which PCHBS is called with one-dimensional
C     arrays F and D, is probably non-Fortran 77, in the strict sense,
C     but it works on all systems on which PCHBS has been tested.
C
C *See Also:
C     PCHIC, PCHIM, or PCHSP can be used to determine an interpolating
C        PCH function from a set of data.
C     The B-spline routine BVALU can be used to evaluate the
C        B-representation that is output by PCHBS.
C        (See BSPDOC for more information.)
C
C***REFERENCES  F. N. Fritsch, "Representations for parametric cubic
C                 splines," Computer Aided Geometric Design 6 (1989),
C                 pp.79-82.
C***ROUTINES CALLED  PCHKT, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   870701  DATE WRITTEN
C   900405  Converted Fortran to upper case.
C   900405  Removed requirement that X be dimensioned N+1.
C   900406  Modified to make PCHKT a subsidiary routine to simplify
C           usage.  In the process, added argument INCFD to be com-
C           patible with the rest of PCHIP.
C   900410  Converted prologue to SLATEC 4.0 format.
C   900410  Added calls to XERMSG and changed constant 3. to 3 to
C           reduce single/double differences.
C   900411  Added reference.
C   900501  Corrected declarations.
C   930317  Minor cosmetic changes.  (FNF)
C   930514  Corrected problems with dimensioning of arguments and
C           clarified DESCRIPTION.  (FNF)
C   930604  Removed  NKNOTS from PCHKT call list.  (FNF)
C***END PROLOGUE  PCHBS
C
C*Internal Notes:
C
C**End
C
C  Declare arguments.
C
      INTEGER  N, INCFD, KNOTYP, NKNOTS, NDIM, KORD, IERR
      REAL  X(*), F(INCFD,*), D(INCFD,*), T(*), BCOEF(*)
C
C  Declare local variables.
C
      INTEGER  K, KK
      REAL  DOV3, HNEW, HOLD
      CHARACTER*8  LIBNAM, SUBNAM
C***FIRST EXECUTABLE STATEMENT  PCHBS
C
C  Initialize.
C
      NDIM = 2*N
      KORD = 4
      IERR = 0
      LIBNAM = 'SLATEC'
      SUBNAM = 'PCHBS'
C
C  Check argument validity.  Set up knot sequence if OK.
C
      IF ( KNOTYP.GT.2 )  THEN
         IERR = -1
         CALL XERMSG (LIBNAM, SUBNAM, 'KNOTYP GREATER THAN 2', IERR, 1)
         RETURN
      ENDIF
      IF ( KNOTYP.LT.0 )  THEN
         IF ( NKNOTS.NE.NDIM+4 )  THEN
            IERR = -2
            CALL XERMSG (LIBNAM, SUBNAM,
     *                    'KNOTYP.LT.0 AND NKNOTS.NE.(2*N+4)', IERR, 1)
            RETURN
         ENDIF
      ELSE
C          Set up knot sequence.
         NKNOTS = NDIM + 4
         CALL PCHKT (N, X, KNOTYP, T)
      ENDIF
C
C  Compute B-spline coefficients.
C
      HNEW = T(3) - T(1)
      DO 40  K = 1, N
         KK = 2*K
         HOLD = HNEW
C          The following requires mixed mode arithmetic.
         DOV3 = D(1,K)/3
         BCOEF(KK-1) = F(1,K) - HOLD*DOV3
C          The following assumes T(2*K+1) = X(K).
         HNEW = T(KK+3) - T(KK+1)
         BCOEF(KK) = F(1,K) + HNEW*DOV3
   40 CONTINUE
C
C  Terminate.
C
      RETURN
      END
      SUBROUTINE PCHCE (IC, VC, N, X, H, SLOPE, D, INCFD, IERR)

c*********************************************************************72
c
cc PCHCE sets boundary conditions for PCHIC.
c
C***BEGIN PROLOGUE  PCHCE
C***SUBSIDIARY
C***PURPOSE  Set boundary conditions for PCHIC
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (PCHCE-S, DPCHCE-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C          PCHCE:  PCHIC End Derivative Setter.
C
C    Called by PCHIC to set end derivatives as requested by the user.
C    It must be called after interior derivative values have been set.
C                      -----
C
C    To facilitate two-dimensional applications, includes an increment
C    between successive values of the D-array.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  IC(2), N, IERR
C        REAL  VC(2), X(N), H(N), SLOPE(N), D(INCFD,N)
C
C        CALL  PCHCE (IC, VC, N, X, H, SLOPE, D, INCFD, IERR)
C
C   Parameters:
C
C     IC -- (input) integer array of length 2 specifying desired
C           boundary conditions:
C           IC(1) = IBEG, desired condition at beginning of data.
C           IC(2) = IEND, desired condition at end of data.
C           ( see prologue to PCHIC for details. )
C
C     VC -- (input) real array of length 2 specifying desired boundary
C           values.  VC(1) need be set only if IC(1) = 2 or 3 .
C                    VC(2) need be set only if IC(2) = 2 or 3 .
C
C     N -- (input) number of data points.  (assumes N.GE.2)
C
C     X -- (input) real array of independent variable values.  (the
C           elements of X are assumed to be strictly increasing.)
C
C     H -- (input) real array of interval lengths.
C     SLOPE -- (input) real array of data slopes.
C           If the data are (X(I),Y(I)), I=1(1)N, then these inputs are:
C                  H(I) =  X(I+1)-X(I),
C              SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1.
C
C     D -- (input) real array of derivative values at the data points.
C           The value corresponding to X(I) must be stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C          (output) the value of D at X(1) and/or X(N) is changed, if
C           necessary, to produce the requested boundary conditions.
C           no other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in D.
C           This argument is provided primarily for 2-D applications.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning errors:
C              IERR = 1  if IBEG.LT.0 and D(1) had to be adjusted for
C                        monotonicity.
C              IERR = 2  if IEND.LT.0 and D(1+(N-1)*INCFD) had to be
C                        adjusted for monotonicity.
C              IERR = 3  if both of the above are true.
C
C    -------
C    WARNING:  This routine does no validity-checking of arguments.
C    -------
C
C  Fortran intrinsics used:  ABS.
C
C***SEE ALSO  PCHIC
C***ROUTINES CALLED  PCHDF, PCHST, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820218  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   870707  Minor corrections made to prologue..
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR section in prologue.  (WRB)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  PCHCE
C
C  Programming notes:
C     1. The function  PCHST(ARG1,ARG2)  is assumed to return zero if
C        either argument is zero, +1 if they are of the same sign, and
C        -1 if they are of opposite sign.
C     2. One could reduce the number of arguments and amount of local
C        storage, at the expense of reduced code clarity, by passing in
C        the array WK (rather than splitting it into H and SLOPE) and
C        increasing its length enough to incorporate STEMP and XTEMP.
C     3. The two monotonicity checks only use the sufficient conditions.
C        Thus, it is possible (but unlikely) for a boundary condition to
C        be changed, even though the original interpolant was monotonic.
C        (At least the result is a continuous function of the data.)
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  IC(2), N, INCFD, IERR
      REAL  VC(2), X(*), H(*), SLOPE(*), D(INCFD,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  IBEG, IEND, IERF, INDEX, J, K
      REAL  HALF, STEMP(3), THREE, TWO, XTEMP(4), ZERO
      SAVE ZERO, HALF, TWO, THREE
      REAL  PCHDF, PCHST
C
C  INITIALIZE.
C
      DATA  ZERO /0./,  HALF /0.5/,  TWO /2./,  THREE /3./
C
C***FIRST EXECUTABLE STATEMENT  PCHCE
      IBEG = IC(1)
      IEND = IC(2)
      IERR = 0
C
C  SET TO DEFAULT BOUNDARY CONDITIONS IF N IS TOO SMALL.
C
      IF ( ABS(IBEG).GT.N )  IBEG = 0
      IF ( ABS(IEND).GT.N )  IEND = 0
C
C  TREAT BEGINNING BOUNDARY CONDITION.
C
      IF (IBEG .EQ. 0)  GO TO 2000
      K = ABS(IBEG)
      IF (K .EQ. 1)  THEN
C        BOUNDARY VALUE PROVIDED.
         D(1,1) = VC(1)
      ELSE IF (K .EQ. 2)  THEN
C        BOUNDARY SECOND DERIVATIVE PROVIDED.
         D(1,1) = HALF*( (THREE*SLOPE(1) - D(1,2)) - HALF*VC(1)*H(1) )
      ELSE IF (K .LT. 5)  THEN
C        USE K-POINT DERIVATIVE FORMULA.
C        PICK UP FIRST K POINTS, IN REVERSE ORDER.
         DO 10  J = 1, K
            INDEX = K-J+1
C           INDEX RUNS FROM K DOWN TO 1.
            XTEMP(J) = X(INDEX)
            IF (J .LT. K)  STEMP(J) = SLOPE(INDEX-1)
   10    CONTINUE
C                 -----------------------------
         D(1,1) = PCHDF (K, XTEMP, STEMP, IERF)
C                 -----------------------------
         IF (IERF .NE. 0)  GO TO 5001
      ELSE
C        USE 'NOT A KNOT' CONDITION.
         D(1,1) = ( THREE*(H(1)*SLOPE(2) + H(2)*SLOPE(1))
     *             - TWO*(H(1)+H(2))*D(1,2) - H(1)*D(1,3) ) / H(2)
      ENDIF
C
      IF (IBEG .GT. 0)  GO TO 2000
C
C  CHECK D(1,1) FOR COMPATIBILITY WITH MONOTONICITY.
C
      IF (SLOPE(1) .EQ. ZERO)  THEN
         IF (D(1,1) .NE. ZERO)  THEN
            D(1,1) = ZERO
            IERR = IERR + 1
         ENDIF
      ELSE IF ( PCHST(D(1,1),SLOPE(1)) .LT. ZERO)  THEN
         D(1,1) = ZERO
         IERR = IERR + 1
      ELSE IF ( ABS(D(1,1)) .GT. THREE*ABS(SLOPE(1)) )  THEN
         D(1,1) = THREE*SLOPE(1)
         IERR = IERR + 1
      ENDIF
C
C  TREAT END BOUNDARY CONDITION.
C
 2000 CONTINUE
      IF (IEND .EQ. 0)  GO TO 5000
      K = ABS(IEND)
      IF (K .EQ. 1)  THEN
C        BOUNDARY VALUE PROVIDED.
         D(1,N) = VC(2)
      ELSE IF (K .EQ. 2)  THEN
C        BOUNDARY SECOND DERIVATIVE PROVIDED.
         D(1,N) = HALF*( (THREE*SLOPE(N-1) - D(1,N-1)) +
     *                                           HALF*VC(2)*H(N-1) )
      ELSE IF (K .LT. 5)  THEN
C        USE K-POINT DERIVATIVE FORMULA.
C        PICK UP LAST K POINTS.
         DO 2010  J = 1, K
            INDEX = N-K+J
C           INDEX RUNS FROM N+1-K UP TO N.
            XTEMP(J) = X(INDEX)
            IF (J .LT. K)  STEMP(J) = SLOPE(INDEX)
 2010    CONTINUE
C                 -----------------------------
         D(1,N) = PCHDF (K, XTEMP, STEMP, IERF)
C                 -----------------------------
         IF (IERF .NE. 0)  GO TO 5001
      ELSE
C        USE 'NOT A KNOT' CONDITION.
         D(1,N) = ( THREE*(H(N-1)*SLOPE(N-2) + H(N-2)*SLOPE(N-1))
     *             - TWO*(H(N-1)+H(N-2))*D(1,N-1) - H(N-1)*D(1,N-2) )
     *                                                         / H(N-2)
      ENDIF
C
      IF (IEND .GT. 0)  GO TO 5000
C
C  CHECK D(1,N) FOR COMPATIBILITY WITH MONOTONICITY.
C
      IF (SLOPE(N-1) .EQ. ZERO)  THEN
         IF (D(1,N) .NE. ZERO)  THEN
            D(1,N) = ZERO
            IERR = IERR + 2
         ENDIF
      ELSE IF ( PCHST(D(1,N),SLOPE(N-1)) .LT. ZERO)  THEN
         D(1,N) = ZERO
         IERR = IERR + 2
      ELSE IF ( ABS(D(1,N)) .GT. THREE*ABS(SLOPE(N-1)) )  THEN
         D(1,N) = THREE*SLOPE(N-1)
         IERR = IERR + 2
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURN.
C
 5001 CONTINUE
C     ERROR RETURN FROM PCHDF.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHCE', 'ERROR RETURN FROM PCHDF', IERR,
     +   1)
      RETURN
      END
      SUBROUTINE PCHCI (N, H, SLOPE, D, INCFD)

c*********************************************************************72
c
cc PCHCI sets interior derivatives for PCHIC.
c
C***BEGIN PROLOGUE  PCHCI
C***SUBSIDIARY
C***PURPOSE  Set interior derivatives for PCHIC
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (PCHCI-S, DPCHCI-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C          PCHCI:  PCHIC Initial Derivative Setter.
C
C    Called by PCHIC to set derivatives needed to determine a monotone
C    piecewise cubic Hermite interpolant to the data.
C
C    Default boundary conditions are provided which are compatible
C    with monotonicity.  If the data are only piecewise monotonic, the
C    interpolant will have an extremum at each point where monotonicity
C    switches direction.
C
C    To facilitate two-dimensional applications, includes an increment
C    between successive values of the D-array.
C
C    The resulting piecewise cubic Hermite function should be identical
C    (within roundoff error) to that produced by PCHIM.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N
C        REAL  H(N), SLOPE(N), D(INCFD,N)
C
C        CALL  PCHCI (N, H, SLOPE, D, INCFD)
C
C   Parameters:
C
C     N -- (input) number of data points.
C           If N=2, simply does linear interpolation.
C
C     H -- (input) real array of interval lengths.
C     SLOPE -- (input) real array of data slopes.
C           If the data are (X(I),Y(I)), I=1(1)N, then these inputs are:
C                  H(I) =  X(I+1)-X(I),
C              SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1.
C
C     D -- (output) real array of derivative values at the data points.
C           If the data are monotonic, these values will determine a
C           a monotone cubic Hermite function.
C           The value corresponding to X(I) is stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C           No other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in D.
C           This argument is provided primarily for 2-D applications.
C
C    -------
C    WARNING:  This routine does no validity-checking of arguments.
C    -------
C
C  Fortran intrinsics used:  ABS, MAX, MIN.
C
C***SEE ALSO  PCHIC
C***ROUTINES CALLED  PCHST
C***REVISION HISTORY  (YYMMDD)
C   820218  DATE WRITTEN
C   820601  Modified end conditions to be continuous functions of
C           data when monotonicity switches in next interval.
C   820602  1. Modified formulas so end conditions are less prone
C             to over/underflow problems.
C           2. Minor modification to HSUM calculation.
C   820805  Converted to SLATEC library version.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR section in prologue.  (WRB)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  PCHCI
C
C  Programming notes:
C     1. The function  PCHST(ARG1,ARG2)  is assumed to return zero if
C        either argument is zero, +1 if they are of the same sign, and
C        -1 if they are of opposite sign.
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD
      REAL  H(*), SLOPE(*), D(INCFD,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, NLESS1
      REAL  DEL1, DEL2, DMAX, DMIN, DRAT1, DRAT2, HSUM, HSUMT3, THREE,
     *      W1, W2, ZERO
      SAVE ZERO, THREE
      REAL  PCHST
C
C  INITIALIZE.
C
      DATA  ZERO /0./,  THREE /3./
C***FIRST EXECUTABLE STATEMENT  PCHCI
      NLESS1 = N - 1
      DEL1 = SLOPE(1)
C
C  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION.
C
      IF (NLESS1 .GT. 1)  GO TO 10
      D(1,1) = DEL1
      D(1,N) = DEL1
      GO TO 5000
C
C  NORMAL CASE  (N .GE. 3).
C
   10 CONTINUE
      DEL2 = SLOPE(2)
C
C  SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE
C     SHAPE-PRESERVING.
C
      HSUM = H(1) + H(2)
      W1 = (H(1) + HSUM)/HSUM
      W2 = -H(1)/HSUM
      D(1,1) = W1*DEL1 + W2*DEL2
      IF ( PCHST(D(1,1),DEL1) .LE. ZERO)  THEN
         D(1,1) = ZERO
      ELSE IF ( PCHST(DEL1,DEL2) .LT. ZERO)  THEN
C        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES.
         DMAX = THREE*DEL1
         IF (ABS(D(1,1)) .GT. ABS(DMAX))  D(1,1) = DMAX
      ENDIF
C
C  LOOP THROUGH INTERIOR POINTS.
C
      DO 50  I = 2, NLESS1
         IF (I .EQ. 2)  GO TO 40
C
         HSUM = H(I-1) + H(I)
         DEL1 = DEL2
         DEL2 = SLOPE(I)
   40    CONTINUE
C
C        SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC.
C
         D(1,I) = ZERO
         IF ( PCHST(DEL1,DEL2) .LE. ZERO)  GO TO 50
C
C        USE BRODLIE MODIFICATION OF BUTLAND FORMULA.
C
         HSUMT3 = HSUM+HSUM+HSUM
         W1 = (HSUM + H(I-1))/HSUMT3
         W2 = (HSUM + H(I)  )/HSUMT3
         DMAX = MAX( ABS(DEL1), ABS(DEL2) )
         DMIN = MIN( ABS(DEL1), ABS(DEL2) )
         DRAT1 = DEL1/DMAX
         DRAT2 = DEL2/DMAX
         D(1,I) = DMIN/(W1*DRAT1 + W2*DRAT2)
C
   50 CONTINUE
C
C  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE
C     SHAPE-PRESERVING.
C
      W1 = -H(N-1)/HSUM
      W2 = (H(N-1) + HSUM)/HSUM
      D(1,N) = W1*DEL1 + W2*DEL2
      IF ( PCHST(D(1,N),DEL2) .LE. ZERO)  THEN
         D(1,N) = ZERO
      ELSE IF ( PCHST(DEL1,DEL2) .LT. ZERO)  THEN
C        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES.
         DMAX = THREE*DEL2
         IF (ABS(D(1,N)) .GT. ABS(DMAX))  D(1,N) = DMAX
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
      END
      SUBROUTINE PCHCM (N, X, F, D, INCFD, SKIP, ISMON, IERR)

c*********************************************************************72
c
cc PCHCM checks a cubic Hermite function for monotonicity.
c
C***BEGIN PROLOGUE  PCHCM
C***PURPOSE  Check a cubic Hermite function for monotonicity.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3
C***TYPE      SINGLE PRECISION (PCHCM-S, DPCHCM-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION,
C             PCHIP, PIECEWISE CUBIC INTERPOLATION, UTILITY ROUTINE
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Computing & Mathematics Research Division
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C *Usage:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, ISMON(N), IERR
C        REAL  X(N), F(INCFD,N), D(INCFD,N)
C        LOGICAL  SKIP
C
C        CALL  PCHCM (N, X, F, D, INCFD, SKIP, ISMON, IERR)
C
C *Arguments:
C
C     N:IN  is the number of data points.  (Error return if N.LT.2 .)
C
C     X:IN  is a real array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F:IN  is a real array of function values.  F(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     D:IN  is a real array of derivative values.  D(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     INCFD:IN  is the increment between successive values in F and D.
C           (Error return if  INCFD.LT.1 .)
C
C     SKIP:INOUT  is a logical variable which should be set to
C           .TRUE. if the user wishes to skip checks for validity of
C           preceding parameters, or to .FALSE. otherwise.
C           This will save time in case these checks have already
C           been performed.
C           SKIP will be set to .TRUE. on normal return.
C
C     ISMON:OUT  is an integer array indicating on which intervals the
C           PCH function defined by  N, X, F, D  is monotonic.
C           For data interval [X(I),X(I+1)],
C             ISMON(I) = -3  if function is probably decreasing;
C             ISMON(I) = -1  if function is strictly decreasing;
C             ISMON(I) =  0  if function is constant;
C             ISMON(I) =  1  if function is strictly increasing;
C             ISMON(I) =  2  if function is non-monotonic;
C             ISMON(I) =  3  if function is probably increasing.
C                If ABS(ISMON)=3, this means that the D-values are near
C                the boundary of the monotonicity region.  A small
C                increase produces non-monotonicity; decrease, strict
C                monotonicity.
C           The above applies to I=1(1)N-1.  ISMON(N) indicates whether
C              the entire function is monotonic on [X(1),X(N)].
C
C     IERR:OUT  is an error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C          (The ISMON-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C *Description:
C
C          PCHCM:  Piecewise Cubic Hermite -- Check Monotonicity.
C
C     Checks the piecewise cubic Hermite function defined by  N,X,F,D
C     for monotonicity.
C
C     To provide compatibility with PCHIM and PCHIC, includes an
C     increment between successive values of the F- and D-arrays.
C
C *Cautions:
C     This provides the same capability as old PCHMC, except that a
C     new output value, -3, was added February 1989.  (Formerly, -3
C     and +3 were lumped together in the single value 3.)  Codes that
C     flag nonmonotonicity by "IF (ISMON.EQ.2)" need not be changed.
C     Codes that check via "IF (ISMON.GE.3)" should change the test to
C     "IF (IABS(ISMON).GE.3)".  Codes that declare monotonicity via
C     "IF (ISMON.LE.1)" should change to "IF (IABS(ISMON).LE.1)".
C
C***REFERENCES  F. N. Fritsch and R. E. Carlson, Monotone piecewise
C                 cubic interpolation, SIAM Journal on Numerical Ana-
C                 lysis 17, 2 (April 1980), pp. 238-246.
C***ROUTINES CALLED  CHFCM, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820518  DATE WRITTEN
C   820804  Converted to SLATEC library version.
C   831201  Reversed order of subscripts of F and D, so that the
C           routine will work properly when INCFD.GT.1 .  (Bug!!)
C   870707  Minor cosmetic changes to prologue.
C   890208  Added possible ISMON value of -3 and modified code so
C           that 1,3,-1 produces ISMON(N)=2, rather than 3.
C   890306  Added caution about changed output.
C   890407  Changed name from PCHMC to PCHCM, as requested at the
C           March 1989 SLATEC CML meeting, and made a few other
C           minor modifications necessitated by this change.
C   890407  Converted to new SLATEC format.
C   890407  Modified DESCRIPTION to LDOC format.
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920429  Revised format and order of references.  (WRB,FNF)
C***END PROLOGUE  PCHCM
C
C  Fortran intrinsics used:  ISIGN.
C  Other routines used:  CHFCM, XERMSG.
C
C ----------------------------------------------------------------------
C
C  Programming notes:
C
C     An alternate organization would have separate loops for computing
C     ISMON(i), i=1,...,NSEG, and for the computation of ISMON(N).  The
C     first loop can be readily parallelized, since the NSEG calls to
C     CHFCM are independent.  The second loop can be cut short if
C     ISMON(N) is ever equal to 2, for it cannot be changed further.
C
C     To produce a double precision version, simply:
C        a. Change PCHCM to DPCHCM wherever it occurs,
C        b. Change CHFCM to DCHFCM wherever it occurs, and
C        c. Change the real declarations to double precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, ISMON(N), IERR
      REAL  X(N), F(INCFD,N), D(INCFD,N)
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, NSEG
      REAL  DELTA
      INTEGER  CHFCM
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  PCHCM
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
      SKIP = .TRUE.
C
C  FUNCTION DEFINITION IS OK -- GO ON.
C
    5 CONTINUE
      NSEG = N - 1
      DO 90  I = 1, NSEG
         DELTA = (F(1,I+1)-F(1,I))/(X(I+1)-X(I))
C                   -------------------------------
         ISMON(I) = CHFCM (D(1,I), D(1,I+1), DELTA)
C                   -------------------------------
         IF (I .EQ. 1)  THEN
            ISMON(N) = ISMON(1)
         ELSE
C           Need to figure out cumulative monotonicity from following
C           "multiplication table":
C
C                    +        I S M O N (I)
C                     +  -3  -1   0   1   3   2
C                      +------------------------+
C               I   -3 I -3  -3  -3   2   2   2 I
C               S   -1 I -3  -1  -1   2   2   2 I
C               M    0 I -3  -1   0   1   3   2 I
C               O    1 I  2   2   1   1   3   2 I
C               N    3 I  2   2   3   3   3   2 I
C              (N)   2 I  2   2   2   2   2   2 I
C                      +------------------------+
C           Note that the 2 row and column are out of order so as not
C           to obscure the symmetry in the rest of the table.
C
C           No change needed if equal or constant on this interval or
C           already declared nonmonotonic.
            IF ( (ISMON(I).NE.ISMON(N)) .AND. (ISMON(I).NE.0)
     .                                  .AND. (ISMON(N).NE.2) )  THEN
               IF ( (ISMON(I).EQ.2) .OR. (ISMON(N).EQ.0) )  THEN
                  ISMON(N) =  ISMON(I)
               ELSE IF (ISMON(I)*ISMON(N) .LT. 0)  THEN
C                 This interval has opposite sense from curve so far.
                  ISMON(N) = 2
               ELSE
C                 At this point, both are nonzero with same sign, and
C                 we have already eliminated case both +-1.
                  ISMON(N) = ISIGN (3, ISMON(N))
               ENDIF
            ENDIF
         ENDIF
   90 CONTINUE
C
C  NORMAL RETURN.
C
      IERR = 0
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHCM',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'PCHCM', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'PCHCM', 'X-ARRAY NOT STRICTLY INCREASING'
     +   , IERR, 1)
      RETURN
      END
      SUBROUTINE PCHCS (SWITCH, N, H, SLOPE, D, INCFD, IERR)

c*********************************************************************72
c
cc PCHCS adjusts derivative values for PCHIC.
c
C***BEGIN PROLOGUE  PCHCS
C***SUBSIDIARY
C***PURPOSE  Adjusts derivative values for PCHIC
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (PCHCS-S, DPCHCS-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C         PCHCS:  PCHIC Monotonicity Switch Derivative Setter.
C
C     Called by  PCHIC  to adjust the values of D in the vicinity of a
C     switch in direction of monotonicity, to produce a more "visually
C     pleasing" curve than that given by  PCHIM .
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, IERR
C        REAL  SWITCH, H(N), SLOPE(N), D(INCFD,N)
C
C        CALL  PCHCS (SWITCH, N, H, SLOPE, D, INCFD, IERR)
C
C   Parameters:
C
C     SWITCH -- (input) indicates the amount of control desired over
C           local excursions from data.
C
C     N -- (input) number of data points.  (assumes N.GT.2 .)
C
C     H -- (input) real array of interval lengths.
C     SLOPE -- (input) real array of data slopes.
C           If the data are (X(I),Y(I)), I=1(1)N, then these inputs are:
C                  H(I) =  X(I+1)-X(I),
C              SLOPE(I) = (Y(I+1)-Y(I))/H(I),  I=1(1)N-1.
C
C     D -- (input) real array of derivative values at the data points,
C           as determined by PCHCI.
C          (output) derivatives in the vicinity of switches in direction
C           of monotonicity may be adjusted to produce a more "visually
C           pleasing" curve.
C           The value corresponding to X(I) is stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C           No other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in D.
C           This argument is provided primarily for 2-D applications.
C
C     IERR -- (output) error flag.  should be zero.
C           If negative, trouble in PCHSW.  (should never happen.)
C
C    -------
C    WARNING:  This routine does no validity-checking of arguments.
C    -------
C
C  Fortran intrinsics used:  ABS, MAX, MIN.
C
C***SEE ALSO  PCHIC
C***ROUTINES CALLED  PCHST, PCHSW
C***REVISION HISTORY  (YYMMDD)
C   820218  DATE WRITTEN
C   820617  Redesigned to (1) fix  problem with lack of continuity
C           approaching a flat-topped peak (2) be cleaner and
C           easier to verify.
C           Eliminated subroutines PCHSA and PCHSX in the process.
C   820622  1. Limited fact to not exceed one, so computed D is a
C             convex combination of PCHCI value and PCHSD value.
C           2. Changed fudge from 1 to 4 (based on experiments).
C   820623  Moved PCHSD to an inline function (eliminating MSWTYP).
C   820805  Converted to SLATEC library version.
C   870813  Minor cosmetic changes.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR section in prologue.  (WRB)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  PCHCS
C
C  Programming notes:
C     1. The function  PCHST(ARG1,ARG2)  is assumed to return zero if
C        either argument is zero, +1 if they are of the same sign, and
C        -1 if they are of opposite sign.
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, IERR
      REAL  SWITCH, H(*), SLOPE(*), D(INCFD,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, INDX, K, NLESS1
      REAL  DEL(3), DEXT, DFLOC, DFMX, FACT, FUDGE, ONE, SLMAX,
     *      WTAVE(2), ZERO
      SAVE ZERO, ONE, FUDGE
      REAL  PCHST
C
C  DEFINE INLINE FUNCTION FOR WEIGHTED AVERAGE OF SLOPES.
C
      REAL  PCHSD, S1, S2, H1, H2
      PCHSD(S1,S2,H1,H2) = (H2/(H1+H2))*S1 + (H1/(H1+H2))*S2
C
C  INITIALIZE.
C
      DATA  ZERO /0./,  ONE /1./
      DATA  FUDGE /4./
C***FIRST EXECUTABLE STATEMENT  PCHCS
      IERR = 0
      NLESS1 = N - 1
C
C  LOOP OVER SEGMENTS.
C
      DO 900  I = 2, NLESS1
         IF ( PCHST(SLOPE(I-1),SLOPE(I)) )  100, 300, 900
C             --------------------------
C
  100    CONTINUE
C
C....... SLOPE SWITCHES MONOTONICITY AT I-TH POINT .....................
C
C           DO NOT CHANGE D IF 'UP-DOWN-UP'.
            IF (I .GT. 2)  THEN
               IF ( PCHST(SLOPE(I-2),SLOPE(I)) .GT. ZERO)  GO TO 900
C                   --------------------------
            ENDIF
            IF (I .LT. NLESS1)  THEN
               IF ( PCHST(SLOPE(I+1),SLOPE(I-1)) .GT. ZERO)  GO TO 900
C                   ----------------------------
            ENDIF
C
C   ....... COMPUTE PROVISIONAL VALUE FOR D(1,I).
C
            DEXT = PCHSD (SLOPE(I-1), SLOPE(I), H(I-1), H(I))
C
C   ....... DETERMINE WHICH INTERVAL CONTAINS THE EXTREMUM.
C
            IF ( PCHST(DEXT, SLOPE(I-1)) )  200, 900, 250
C                -----------------------
C
  200       CONTINUE
C              DEXT AND SLOPE(I-1) HAVE OPPOSITE SIGNS --
C                        EXTREMUM IS IN (X(I-1),X(I)).
               K = I-1
C              SET UP TO COMPUTE NEW VALUES FOR D(1,I-1) AND D(1,I).
               WTAVE(2) = DEXT
               IF (K .GT. 1)
     *            WTAVE(1) = PCHSD (SLOPE(K-1), SLOPE(K), H(K-1), H(K))
               GO TO 400
C
  250       CONTINUE
C              DEXT AND SLOPE(I) HAVE OPPOSITE SIGNS --
C                        EXTREMUM IS IN (X(I),X(I+1)).
               K = I
C              SET UP TO COMPUTE NEW VALUES FOR D(1,I) AND D(1,I+1).
               WTAVE(1) = DEXT
               IF (K .LT. NLESS1)
     *            WTAVE(2) = PCHSD (SLOPE(K), SLOPE(K+1), H(K), H(K+1))
               GO TO 400
C
  300    CONTINUE
C
C....... AT LEAST ONE OF SLOPE(I-1) AND SLOPE(I) IS ZERO --
C                     CHECK FOR FLAT-TOPPED PEAK .......................
C
            IF (I .EQ. NLESS1)  GO TO 900
            IF ( PCHST(SLOPE(I-1), SLOPE(I+1)) .GE. ZERO)  GO TO 900
C                -----------------------------
C
C           WE HAVE FLAT-TOPPED PEAK ON (X(I),X(I+1)).
            K = I
C           SET UP TO COMPUTE NEW VALUES FOR D(1,I) AND D(1,I+1).
            WTAVE(1) = PCHSD (SLOPE(K-1), SLOPE(K), H(K-1), H(K))
            WTAVE(2) = PCHSD (SLOPE(K), SLOPE(K+1), H(K), H(K+1))
C
  400    CONTINUE
C
C....... AT THIS POINT WE HAVE DETERMINED THAT THERE WILL BE AN EXTREMUM
C        ON (X(K),X(K+1)), WHERE K=I OR I-1, AND HAVE SET ARRAY WTAVE--
C           WTAVE(1) IS A WEIGHTED AVERAGE OF SLOPE(K-1) AND SLOPE(K),
C                    IF K.GT.1
C           WTAVE(2) IS A WEIGHTED AVERAGE OF SLOPE(K) AND SLOPE(K+1),
C                    IF K.LT.N-1
C
         SLMAX = ABS(SLOPE(K))
         IF (K .GT. 1)    SLMAX = MAX( SLMAX, ABS(SLOPE(K-1)) )
         IF (K.LT.NLESS1) SLMAX = MAX( SLMAX, ABS(SLOPE(K+1)) )
C
         IF (K .GT. 1)  DEL(1) = SLOPE(K-1) / SLMAX
         DEL(2) = SLOPE(K) / SLMAX
         IF (K.LT.NLESS1)  DEL(3) = SLOPE(K+1) / SLMAX
C
         IF ((K.GT.1) .AND. (K.LT.NLESS1))  THEN
C           NORMAL CASE -- EXTREMUM IS NOT IN A BOUNDARY INTERVAL.
            FACT = FUDGE* ABS(DEL(3)*(DEL(1)-DEL(2))*(WTAVE(2)/SLMAX))
            D(1,K) = D(1,K) + MIN(FACT,ONE)*(WTAVE(1) - D(1,K))
            FACT = FUDGE* ABS(DEL(1)*(DEL(3)-DEL(2))*(WTAVE(1)/SLMAX))
            D(1,K+1) = D(1,K+1) + MIN(FACT,ONE)*(WTAVE(2) - D(1,K+1))
         ELSE
C           SPECIAL CASE K=1 (WHICH CAN OCCUR ONLY IF I=2) OR
C                        K=NLESS1 (WHICH CAN OCCUR ONLY IF I=NLESS1).
            FACT = FUDGE* ABS(DEL(2))
            D(1,I) = MIN(FACT,ONE) * WTAVE(I-K+1)
C              NOTE THAT I-K+1 = 1 IF K=I  (=NLESS1),
C                        I-K+1 = 2 IF K=I-1(=1).
         ENDIF
C
C
C....... ADJUST IF NECESSARY TO LIMIT EXCURSIONS FROM DATA.
C
         IF (SWITCH .LE. ZERO)  GO TO 900
C
         DFLOC = H(K)*ABS(SLOPE(K))
         IF (K .GT. 1)    DFLOC = MAX( DFLOC, H(K-1)*ABS(SLOPE(K-1)) )
         IF (K.LT.NLESS1) DFLOC = MAX( DFLOC, H(K+1)*ABS(SLOPE(K+1)) )
         DFMX = SWITCH*DFLOC
         INDX = I-K+1
C        INDX = 1 IF K=I, 2 IF K=I-1.
C        ---------------------------------------------------------------
         CALL PCHSW (DFMX, INDX, D(1,K), D(1,K+1), H(K), SLOPE(K), IERR)
C        ---------------------------------------------------------------
         IF (IERR .NE. 0)  RETURN
C
C....... END OF SEGMENT LOOP.
C
  900 CONTINUE
C
      RETURN
      END
      REAL FUNCTION PCHDF (K, X, S, IERR)

c*********************************************************************72
c
cc PCHDF computes divided differences for PCHCE and PCHSP.
c
C***BEGIN PROLOGUE  PCHDF
C***SUBSIDIARY
C***PURPOSE  Computes divided differences for PCHCE and PCHSP
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (PCHDF-S, DPCHDF-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C          PCHDF:   PCHIP Finite Difference Formula
C
C     Uses a divided difference formulation to compute a K-point approx-
C     imation to the derivative at X(K) based on the data in X and S.
C
C     Called by  PCHCE  and  PCHSP  to compute 3- and 4-point boundary
C     derivative approximations.
C
C ----------------------------------------------------------------------
C
C     On input:
C        K      is the order of the desired derivative approximation.
C               K must be at least 3 (error return if not).
C        X      contains the K values of the independent variable.
C               X need not be ordered, but the values **MUST** be
C               distinct.  (Not checked here.)
C        S      contains the associated slope values:
C                  S(I) = (F(I+1)-F(I))/(X(I+1)-X(I)), I=1(1)K-1.
C               (Note that S need only be of length K-1.)
C
C     On return:
C        S      will be destroyed.
C        IERR   will be set to -1 if K.LT.2 .
C        PCHDF  will be set to the desired derivative approximation if
C               IERR=0 or to zero if IERR=-1.
C
C ----------------------------------------------------------------------
C
C***SEE ALSO  PCHCE, PCHSP
C***REFERENCES  Carl de Boor, A Practical Guide to Splines, Springer-
C                 Verlag, New York, 1978, pp. 10-16.
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820503  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   870813  Minor cosmetic changes.
C   890411  Added SAVE statements (Vers. 3.2).
C   890411  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB)
C   920429  Revised format and order of references.  (WRB,FNF)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  PCHDF
C
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  K, IERR
      REAL  X(K), S(K)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, J
      REAL  VALUE, ZERO
      SAVE ZERO
      DATA  ZERO /0./
C
C  CHECK FOR LEGAL VALUE OF K.
C
C***FIRST EXECUTABLE STATEMENT  PCHDF
      IF (K .LT. 3)  GO TO 5001
C
C  COMPUTE COEFFICIENTS OF INTERPOLATING POLYNOMIAL.
C
      DO 10  J = 2, K-1
         DO 9  I = 1, K-J
            S(I) = (S(I+1)-S(I))/(X(I+J)-X(I))
    9    CONTINUE
   10 CONTINUE
C
C  EVALUATE DERIVATIVE AT X(K).
C
      VALUE = S(1)
      DO 20  I = 2, K-1
         VALUE = S(I) + VALUE*(X(K)-X(I))
   20 CONTINUE
C
C  NORMAL RETURN.
C
      IERR = 0
      PCHDF = VALUE
      RETURN
C
C  ERROR RETURN.
C
 5001 CONTINUE
C     K.LT.3 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHDF', 'K LESS THAN THREE', IERR, 1)
      PCHDF = ZERO
      RETURN
      END
      SUBROUTINE PCHEV(N,X,F,D,NVAL,XVAL,FVAL,DVAL,IERR)

c*********************************************************************72
c
cc PCHEV: value and derivative of spline or cubic Hermite at many points.
c
C***BEGIN PROLOGUE  PCHEV
C***DATE WRITTEN   870828   (YYMMDD)
C***REVISION DATE  870828   (YYMMDD)
C***CATEGORY NO.  E3,H1
C***KEYWORDS  CUBIC HERMITE OR SPLINE DIFFERENTIATION,CUBIC HERMITE
C             EVALUATION,EASY TO USE SPLINE OR CUBIC HERMITE EVALUATOR
C***AUTHOR  KAHANER, D.K., (NBS)
C             SCIENTIFIC COMPUTING DIVISION
C             NATIONAL BUREAU OF STANDARDS
C             ROOM A161, TECHNOLOGY BUILDING
C             GAITHERSBURG, MARYLAND 20899
C             (301) 975-3808
C***PURPOSE  Evaluates the function and first derivative of a piecewise
C            cubic Hermite or spline function at an array of points
C            XVAL.  It is easy to use.
C***DESCRIPTION
C
C          PCHEV:  Piecewise Cubic Hermite or Spline Derivative
C                   Evaluator. Easy to Use.
C
C     From the book "Numerical Methods and Software"
C          by  D. Kahaner, C. Moler, S. Nash
C                 Prentice Hall 1988
C
C     Evaluates the function and first derivative of the cubic Hermite
C     or spline function defined by  N, X, F, D, at the array of points
C     XVAL.
C
C
C     This is an easy to use driver for the routines by F.N. Fritsch
C     described in reference (2) below. Those also have other 
C     capabilities.
C
C ----------------------------------------------------------------------
C
C  Calling sequence: CALL PCHEV (N, X, F, D, NVAL, XVAL, FVAL, DVAL, IERR)
C
C     INTEGER  N, NVAL, IERR
C     real  X(N), F(N), D(N), XVAL(NVAL), FVAL(NVAL), DVAL(NVAL)
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real array of independent variable 
C           values.  The elements of X must be strictly increasing:
C             X(I-1) .LT. X(I),  I = 2(1)N. (Error return if not.)
C
C     F -- (input) real array of function values.  F(I) is
C           the value corresponding to X(I).
C
C     D -- (input) real array of derivative values.  
C          D(I) is the value corresponding to X(I).
C
C  NVAL -- (input) number of points at which the functions are to be
C           evaluated. ( Error return if NVAL.LT.1 )
C
C  XVAL -- (input) real array of points at which the 
C          functions are to be evaluated.
C
C          NOTES:
C           1. The evaluation will be most efficient if the elements
C              of XVAL are increasing relative to X;
C              that is,   XVAL(J) .GE. X(I)
C              implies    XVAL(K) .GE. X(I),  all K.GE.J .
C           2. If any of the XVAL are outside the interval [X(1),X(N)],
C              values are extrapolated from the nearest extreme cubic,
C              and a warning error is returned.
C
C  FVAL -- (output) real array of values of the cubic 
C          Hermite function defined by  N, X, F, D  at the points  XVAL.
C
C  DVAL -- (output) real array of values of the 
C          first derivative of the same function at the points  XVAL.
C
C  IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  means that extrapolation was performed at
C                 IERR points.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if NVAL.LT.1 .
C           (Output arrays have not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C              IERR = -5  if an error has occurred in the lower-level
C                         routine CHFDV.  NB: this should never happen.
C                         Notify the author **IMMEDIATELY** if it does.
C
C ----------------------------------------------------------------------
C***REFERENCES  1. F.N.FRITSCH AND R.E.CARLSON, 'MONOTONE PIECEWISE
C                 CUBIC INTERPOLATION,' SIAM J.NUMER.ANAL. 17, 2 (APRIL
C                 1980), 238-246.
C               2. F.N.FRITSCH, 'PIECEWISE CUBIC HERMITE INTERPOLATION
C                 PACKAGE, FINAL SPECIFICATIONS', LAWRENCE LIVERMORE
C                 NATIONAL LABORATORY, COMPUTER DOCUMENTATION UCID-30194
C                 AUGUST 1982.
C***ROUTINES CALLED  PCHFD
C***END PROLOGUE  PCHEV
      INTEGER  N, NVAL, IERR
      real  X(N), F(N), D(N), XVAL(NVAL), FVAL(NVAL), 
     *DVAL(NVAL)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER INCFD
      LOGICAL SKIP
      DATA SKIP /.TRUE./
      DATA INCFD /1/
C
C
C***FIRST EXECUTABLE STATEMENT PCHEV
C
      CALL PCHFD(N,X,F,D,INCFD,SKIP,NVAL,XVAL,FVAL,DVAL,IERR)
C
C
 5000 CONTINUE
      RETURN
      END
      SUBROUTINE PCHEZ ( N, X, F, D, SPLINE, WK, LWK, IERR )

c*********************************************************************72
c
cc PCHEZ sets up a spline or cubic Hermite interpolant.
c
C***BEGIN PROLOGUE  PCHEZ
C***DATE WRITTEN   870821   (YYMMDD)
C***REVISION DATE  870908   (YYMMDD)
C***CATEGORY NO.  E1B
C***KEYWORDS  CUBIC HERMITE MONOTONE INTERPOLATION, SPLINE
C             INTERPOLATION, EASY TO USE PIECEWISE CUBIC INTERPOLATION
C***AUTHOR  KAHANER, D.K., (NBS)
C             SCIENTIFIC COMPUTING DIVISION
C             NATIONAL BUREAU OF STANDARDS
C             GAITHERSBURG, MARYLAND 20899
C             (301) 975-3808
C***PURPOSE  Easy to use spline or cubic Hermite interpolation.
C***DESCRIPTION
C
C          PCHEZ:  Piecewise Cubic Interpolation, Easy to Use.
C
C     From the book "Numerical Methods and Software"
C          by  D. Kahaner, C. Moler, S. Nash
C               Prentice Hall 1988
C
C     Sets derivatives for spline (two continuous derivatives) or
C     Hermite cubic (one continuous derivative) interpolation.
C     Spline interpolation is smoother, but may not "look" right if the
C     data contains both "steep" and "flat" sections.  Hermite cubics
C     can produce a "visually pleasing" and monotone interpolant to
C     monotone data. This is an easy to use driver for the routines
C     by F. N. Fritsch in reference (4) below. Various boundary
C     conditions are set to default values by PCHEZ. Many other choices
C     are available in the subroutines PCHIC, PCHIM and PCHSP.
C
C     Use PCHEV to evaluate the resulting function and its derivative.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:   CALL  PCHEZ (N, X, F, D, SPLINE, WK, LWK, IERR)
C
C     INTEGER  N, IERR,  LWK
C     real  X(N), F(N), D(N), WK(*)
C     LOGICAL SPLINE
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C           If N=2, simply does linear interpolation.
C
C     X -- (input) real array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real array of dependent variable values to be inter-
C           polated.  F(I) is value corresponding to X(I).
C
C     D -- (output) real array of derivative values at the data points.
C
C     SPLINE -- (input) logical variable to specify if the interpolant
C           is to be a spline with two continuous derivaties
C           (set SPLINE=.TRUE.) or a Hermite cubic interpolant with one
C           continuous derivative (set SPLINE=.FALSE.).
C        Note: If SPLINE=.TRUE. the interpolating spline satisfies the
C           default "not-a-knot" boundary condition, with a continuous
C           third derivative at X(2) and X(N-1). See reference (3).
C              If SPLINE=.FALSE. the interpolating Hermite cubic will be
C           monotone if the input data is monotone. Boundary conditions
C           computed from the derivative of a local quadratic unless thi
C           alters monotonicity.
C
C     WK -- (scratch) real work array, which must be declared by the cal
C           program to be at least 2*N if SPLINE is .TRUE. and not used
C           otherwise.
C
C     LWK -- (input) length of work array WK. (Error return if
C           LWK.LT.2*N and SPLINE is .TRUE., not checked otherwise.)
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  (can only occur when SPLINE=.FALSE.) means tha
C                 IERR switches in the direction of monotonicity were de
C                 When SPLINE=.FALSE.,  PCHEZ guarantees that if the inp
C                 data is monotone, the interpolant will be too. This wa
C                 is to alert you to the fact that the input data was no
C                 monotone.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -7  if LWK is less than 2*N and SPLINE is .TRUE.
C             (The D-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C ----------------------------------------------------------------------
C***REFERENCES  1. F.N.FRITSCH AND R.E.CARLSON, 'MONOTONE PIECEWISE
C                 CUBIC INTERPOLATION,' SIAM J.NUMER.ANAL. 17, 2 (APRIL
C                 1980), 238-246.
C               2. F.N.FRITSCH AND J.BUTLAND, 'A METHOD FOR CONSTRUCTING
C                 LOCAL MONOTONE PIECEWISE CUBIC INTERPOLANTS,' LLNL
C                 PREPRINT UCRL-87559 (APRIL 1982).
C               3. CARL DE BOOR, A PRACTICAL GUIDE TO SPLINES, SPRINGER-
C                 VERLAG (NEW YORK, 1978).  (ESP. CHAPTER IV, PP.49-62.)
C               4. F.N.FRITSCH, 'PIECEWISE CUBIC HERMITE INTERPOLATION
C                 PACKAGE, FINAL SPECIFICATIONS', LAWRENCE LIVERMORE
C                 NATIONAL LABORATORY, COMPUTER DOCUMENTATION UCID-30194
C                 AUGUST 1982.
C***ROUTINES CALLED  PCHIM,PCHSP
C***END PROLOGUE  PCHEZ
      INTEGER  N, LWK, IERR
      real  X(N), F(N), D(N), WK(LWK)
      LOGICAL SPLINE
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER IC(2), INCFD
      real  VC(2)
      DATA IC(1) /0/
      DATA IC(2) /0/
      DATA INCFD /1/
C
C
C***FIRST EXECUTABLE STATEMENT  PCHEZ
C
      IF ( SPLINE ) THEN
        CALL  PCHSP ( IC, VC, N, X, F, D, INCFD, WK, LWK, IERR )
      ELSE
        CALL  PCHIM ( N, X, F, D, INCFD, IERR )
      ENDIF
C
C  ERROR CONDITIONS ALREADY CHECKED IN PCHSP OR PCHIM

      RETURN
      END
      SUBROUTINE PCHFD (N, X, F, D, INCFD, SKIP, NE, XE, FE, DE, IERR)

c*********************************************************************72
c
cc PCHFD evaluates a piecewise cubic Hermite and derivative at many points.
c
C***BEGIN PROLOGUE  PCHFD
C***PURPOSE  Evaluate a piecewise cubic Hermite function and its first
C            derivative at an array of points.  May be used by itself
C            for Hermite interpolation, or as an evaluator for PCHIM
C            or PCHIC.  If only function values are required, use
C            PCHFE instead.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3, H1
C***TYPE      SINGLE PRECISION (PCHFD-S, DPCHFD-D)
C***KEYWORDS  CUBIC HERMITE DIFFERENTIATION, CUBIC HERMITE EVALUATION,
C             HERMITE INTERPOLATION, PCHIP, PIECEWISE CUBIC EVALUATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          PCHFD:  Piecewise Cubic Hermite Function and Derivative
C                  evaluator
C
C     Evaluates the cubic Hermite function defined by  N, X, F, D,  to-
C     gether with its first derivative, at the points  XE(J), J=1(1)NE.
C
C     If only function values are required, use PCHFE, instead.
C
C     To provide compatibility with PCHIM and PCHIC, includes an
C     increment between successive values of the F- and D-arrays.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, NE, IERR
C        REAL  X(N), F(INCFD,N), D(INCFD,N), XE(NE), FE(NE), DE(NE)
C        LOGICAL  SKIP
C
C        CALL  PCHFD (N, X, F, D, INCFD, SKIP, NE, XE, FE, DE, IERR)
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real array of function values.  F(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     D -- (input) real array of derivative values.  D(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     INCFD -- (input) increment between successive values in F and D.
C           (Error return if  INCFD.LT.1 .)
C
C     SKIP -- (input/output) logical variable which should be set to
C           .TRUE. if the user wishes to skip checks for validity of
C           preceding parameters, or to .FALSE. otherwise.
C           This will save time in case these checks have already
C           been performed (say, in PCHIM or PCHIC).
C           SKIP will be set to .TRUE. on normal return.
C
C     NE -- (input) number of evaluation points.  (Error return if
C           NE.LT.1 .)
C
C     XE -- (input) real array of points at which the functions are to
C           be evaluated.
C
C
C          NOTES:
C           1. The evaluation will be most efficient if the elements
C              of XE are increasing relative to X;
C              that is,   XE(J) .GE. X(I)
C              implies    XE(K) .GE. X(I),  all K.GE.J .
C           2. If any of the XE are outside the interval [X(1),X(N)],
C              values are extrapolated from the nearest extreme cubic,
C              and a warning error is returned.
C
C     FE -- (output) real array of values of the cubic Hermite function
C           defined by  N, X, F, D  at the points  XE.
C
C     DE -- (output) real array of values of the first derivative of
C           the same function at the points  XE.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  means that extrapolation was performed at
C                 IERR points.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if NE.LT.1 .
C           (Output arrays have not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C              IERR = -5  if an error has occurred in the lower-level
C                         routine CHFDV.  NB: this should never happen.
C                         Notify the author **IMMEDIATELY** if it does.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CHFDV, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811020  DATE WRITTEN
C   820803  Minor cosmetic changes for release 1.
C   870707  Minor cosmetic changes to prologue.
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  PCHFD
C  Programming notes:
C
C     1. To produce a double precision version, simply:
C        a. Change PCHFD to DPCHFD, and CHFDV to DCHFDV, wherever they
C           occur,
C        b. Change the real declaration to double precision,
C
C     2. Most of the coding between the call to CHFDV and the end of
C        the IR-loop could be eliminated if it were permissible to
C        assume that XE is ordered relative to X.
C
C     3. CHFDV does not assume that X1 is less than X2.  thus, it would
C        be possible to write a version of PCHFD that assumes a strict-
C        ly decreasing X-array by simply running the IR-loop backwards
C        (and reversing the order of appropriate tests).
C
C     4. The present code has a minor bug, which I have decided is not
C        worth the effort that would be required to fix it.
C        If XE contains points in [X(N-1),X(N)], followed by points .LT.
C        X(N-1), followed by points .GT.X(N), the extrapolation points
C        will be counted (at least) twice in the total returned in IERR.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, NE, IERR
      REAL  X(*), F(INCFD,*), D(INCFD,*), XE(*), FE(*), DE(*)
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IERC, IR, J, JFIRST, NEXT(2), NJ
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  PCHFD
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
    5 CONTINUE
      IF ( NE.LT.1 )  GO TO 5004
      IERR = 0
      SKIP = .TRUE.
C
C  LOOP OVER INTERVALS.        (   INTERVAL INDEX IS  IL = IR-1  . )
C                              ( INTERVAL IS X(IL).LE.X.LT.X(IR) . )
      JFIRST = 1
      IR = 2
   10 CONTINUE
C
C     SKIP OUT OF LOOP IF HAVE PROCESSED ALL EVALUATION POINTS.
C
         IF (JFIRST .GT. NE)  GO TO 5000
C
C     LOCATE ALL POINTS IN INTERVAL.
C
         DO 20  J = JFIRST, NE
            IF (XE(J) .GE. X(IR))  GO TO 30
   20    CONTINUE
         J = NE + 1
         GO TO 40
C
C     HAVE LOCATED FIRST POINT BEYOND INTERVAL.
C
   30    CONTINUE
         IF (IR .EQ. N)  J = NE + 1
C
   40    CONTINUE
         NJ = J - JFIRST
C
C     SKIP EVALUATION IF NO POINTS IN INTERVAL.
C
         IF (NJ .EQ. 0)  GO TO 50
C
C     EVALUATE CUBIC AT XE(I),  I = JFIRST (1) J-1 .
C
C       ----------------------------------------------------------------
        CALL CHFDV (X(IR-1),X(IR), F(1,IR-1),F(1,IR), D(1,IR-1),D(1,IR),
     *              NJ, XE(JFIRST), FE(JFIRST), DE(JFIRST), NEXT, IERC)
C       ----------------------------------------------------------------
         IF (IERC .LT. 0)  GO TO 5005
C
         IF (NEXT(2) .EQ. 0)  GO TO 42
C        IF (NEXT(2) .GT. 0)  THEN
C           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(2) TO THE
C           RIGHT OF X(IR).
C
            IF (IR .LT. N)  GO TO 41
C           IF (IR .EQ. N)  THEN
C              THESE ARE ACTUALLY EXTRAPOLATION POINTS.
               IERR = IERR + NEXT(2)
               GO TO 42
   41       CONTINUE
C           ELSE
C              WE SHOULD NEVER HAVE GOTTEN HERE.
               GO TO 5005
C           ENDIF
C        ENDIF
   42    CONTINUE
C
         IF (NEXT(1) .EQ. 0)  GO TO 49
C        IF (NEXT(1) .GT. 0)  THEN
C           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(1) TO THE
C           LEFT OF X(IR-1).
C
            IF (IR .GT. 2)  GO TO 43
C           IF (IR .EQ. 2)  THEN
C              THESE ARE ACTUALLY EXTRAPOLATION POINTS.
               IERR = IERR + NEXT(1)
               GO TO 49
   43       CONTINUE
C           ELSE
C              XE IS NOT ORDERED RELATIVE TO X, SO MUST ADJUST
C              EVALUATION INTERVAL.
C
C              FIRST, LOCATE FIRST POINT TO LEFT OF X(IR-1).
               DO 44  I = JFIRST, J-1
                  IF (XE(I) .LT. X(IR-1))  GO TO 45
   44          CONTINUE
C              NOTE-- CANNOT DROP THROUGH HERE UNLESS THERE IS AN ERROR
C                     IN CHFDV.
               GO TO 5005
C
   45          CONTINUE
C              RESET J.  (THIS WILL BE THE NEW JFIRST.)
               J = I
C
C              NOW FIND OUT HOW FAR TO BACK UP IN THE X-ARRAY.
               DO 46  I = 1, IR-1
                  IF (XE(J) .LT. X(I)) GO TO 47
   46          CONTINUE
C              NB-- CAN NEVER DROP THROUGH HERE, SINCE XE(J).LT.X(IR-1).
C
   47          CONTINUE
C              AT THIS POINT, EITHER  XE(J) .LT. X(1)
C                 OR      X(I-1) .LE. XE(J) .LT. X(I) .
C              RESET IR, RECOGNIZING THAT IT WILL BE INCREMENTED BEFORE
C              CYCLING.
               IR = MAX(1, I-1)
C           ENDIF
C        ENDIF
   49    CONTINUE
C
         JFIRST = J
C
C     END OF IR-LOOP.
C
   50 CONTINUE
      IR = IR + 1
      IF (IR .LE. N)  GO TO 10
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHFD',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'PCHFD', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'PCHFD', 'X-ARRAY NOT STRICTLY INCREASING'
     +   , IERR, 1)
      RETURN
C
 5004 CONTINUE
C     NE.LT.1 RETURN.
      IERR = -4
      CALL XERMSG ('SLATEC', 'PCHFD',
     +   'NUMBER OF EVALUATION POINTS LESS THAN ONE', IERR, 1)
      RETURN
C
 5005 CONTINUE
C     ERROR RETURN FROM CHFDV.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -5
      CALL XERMSG ('SLATEC', 'PCHFD',
     +   'ERROR RETURN FROM CHFDV -- FATAL', IERR, 2)
      RETURN
      END
      SUBROUTINE PCHFE (N, X, F, D, INCFD, SKIP, NE, XE, FE, IERR)

c*********************************************************************72
c
cc PCHFE evaluates a piecewise cubic Hermite function at many points.
c
C***BEGIN PROLOGUE  PCHFE
C***PURPOSE  Evaluate a piecewise cubic Hermite function at an array of
C            points.  May be used by itself for Hermite interpolation,
C            or as an evaluator for PCHIM or PCHIC.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3
C***TYPE      SINGLE PRECISION (PCHFE-S, DPCHFE-D)
C***KEYWORDS  CUBIC HERMITE EVALUATION, HERMITE INTERPOLATION, PCHIP,
C             PIECEWISE CUBIC EVALUATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          PCHFE:  Piecewise Cubic Hermite Function Evaluator
C
C     Evaluates the cubic Hermite function defined by  N, X, F, D  at
C     the points  XE(J), J=1(1)NE.
C
C     To provide compatibility with PCHIM and PCHIC, includes an
C     increment between successive values of the F- and D-arrays.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, NE, IERR
C        REAL  X(N), F(INCFD,N), D(INCFD,N), XE(NE), FE(NE)
C        LOGICAL  SKIP
C
C        CALL  PCHFE (N, X, F, D, INCFD, SKIP, NE, XE, FE, IERR)
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real array of function values.  F(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     D -- (input) real array of derivative values.  D(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     INCFD -- (input) increment between successive values in F and D.
C           (Error return if  INCFD.LT.1 .)
C
C     SKIP -- (input/output) logical variable which should be set to
C           .TRUE. if the user wishes to skip checks for validity of
C           preceding parameters, or to .FALSE. otherwise.
C           This will save time in case these checks have already
C           been performed (say, in PCHIM or PCHIC).
C           SKIP will be set to .TRUE. on normal return.
C
C     NE -- (input) number of evaluation points.  (Error return if
C           NE.LT.1 .)
C
C     XE -- (input) real array of points at which the function is to be
C           evaluated.
C
C          NOTES:
C           1. The evaluation will be most efficient if the elements
C              of XE are increasing relative to X;
C              that is,   XE(J) .GE. X(I)
C              implies    XE(K) .GE. X(I),  all K.GE.J .
C           2. If any of the XE are outside the interval [X(1),X(N)],
C              values are extrapolated from the nearest extreme cubic,
C              and a warning error is returned.
C
C     FE -- (output) real array of values of the cubic Hermite function
C           defined by  N, X, F, D  at the points  XE.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  means that extrapolation was performed at
C                 IERR points.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if NE.LT.1 .
C             (The FE-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CHFEV, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811020  DATE WRITTEN
C   820803  Minor cosmetic changes for release 1.
C   870707  Minor cosmetic changes to prologue.
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  PCHFE
C  Programming notes:
C
C     1. To produce a double precision version, simply:
C        a. Change PCHFE to DPCHFE, and CHFEV to DCHFEV, wherever they
C           occur,
C        b. Change the real declaration to double precision,
C
C     2. Most of the coding between the call to CHFEV and the end of
C        the IR-loop could be eliminated if it were permissible to
C        assume that XE is ordered relative to X.
C
C     3. CHFEV does not assume that X1 is less than X2.  thus, it would
C        be possible to write a version of PCHFE that assumes a strict-
C        ly decreasing X-array by simply running the IR-loop backwards
C        (and reversing the order of appropriate tests).
C
C     4. The present code has a minor bug, which I have decided is not
C        worth the effort that would be required to fix it.
C        If XE contains points in [X(N-1),X(N)], followed by points .LT.
C        X(N-1), followed by points .GT.X(N), the extrapolation points
C        will be counted (at least) twice in the total returned in IERR.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, NE, IERR
      REAL  X(*), F(INCFD,*), D(INCFD,*), XE(*), FE(*)
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IERC, IR, J, JFIRST, NEXT(2), NJ
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  PCHFE
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
    5 CONTINUE
      IF ( NE.LT.1 )  GO TO 5004
      IERR = 0
      SKIP = .TRUE.
C
C  LOOP OVER INTERVALS.        (   INTERVAL INDEX IS  IL = IR-1  . )
C                              ( INTERVAL IS X(IL).LE.X.LT.X(IR) . )
      JFIRST = 1
      IR = 2
   10 CONTINUE
C
C     SKIP OUT OF LOOP IF HAVE PROCESSED ALL EVALUATION POINTS.
C
         IF (JFIRST .GT. NE)  GO TO 5000
C
C     LOCATE ALL POINTS IN INTERVAL.
C
         DO 20  J = JFIRST, NE
            IF (XE(J) .GE. X(IR))  GO TO 30
   20    CONTINUE
         J = NE + 1
         GO TO 40
C
C     HAVE LOCATED FIRST POINT BEYOND INTERVAL.
C
   30    CONTINUE
         IF (IR .EQ. N)  J = NE + 1
C
   40    CONTINUE
         NJ = J - JFIRST
C
C     SKIP EVALUATION IF NO POINTS IN INTERVAL.
C
         IF (NJ .EQ. 0)  GO TO 50
C
C     EVALUATE CUBIC AT XE(I),  I = JFIRST (1) J-1 .
C
C       ----------------------------------------------------------------
        CALL CHFEV (X(IR-1),X(IR), F(1,IR-1),F(1,IR), D(1,IR-1),D(1,IR),
     *              NJ, XE(JFIRST), FE(JFIRST), NEXT, IERC)
C       ----------------------------------------------------------------
         IF (IERC .LT. 0)  GO TO 5005
C
         IF (NEXT(2) .EQ. 0)  GO TO 42
C        IF (NEXT(2) .GT. 0)  THEN
C           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(2) TO THE
C           RIGHT OF X(IR).
C
            IF (IR .LT. N)  GO TO 41
C           IF (IR .EQ. N)  THEN
C              THESE ARE ACTUALLY EXTRAPOLATION POINTS.
               IERR = IERR + NEXT(2)
               GO TO 42
   41       CONTINUE
C           ELSE
C              WE SHOULD NEVER HAVE GOTTEN HERE.
               GO TO 5005
C           ENDIF
C        ENDIF
   42    CONTINUE
C
         IF (NEXT(1) .EQ. 0)  GO TO 49
C        IF (NEXT(1) .GT. 0)  THEN
C           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(1) TO THE
C           LEFT OF X(IR-1).
C
            IF (IR .GT. 2)  GO TO 43
C           IF (IR .EQ. 2)  THEN
C              THESE ARE ACTUALLY EXTRAPOLATION POINTS.
               IERR = IERR + NEXT(1)
               GO TO 49
   43       CONTINUE
C           ELSE
C              XE IS NOT ORDERED RELATIVE TO X, SO MUST ADJUST
C              EVALUATION INTERVAL.
C
C              FIRST, LOCATE FIRST POINT TO LEFT OF X(IR-1).
               DO 44  I = JFIRST, J-1
                  IF (XE(I) .LT. X(IR-1))  GO TO 45
   44          CONTINUE
C              NOTE-- CANNOT DROP THROUGH HERE UNLESS THERE IS AN ERROR
C                     IN CHFEV.
               GO TO 5005
C
   45          CONTINUE
C              RESET J.  (THIS WILL BE THE NEW JFIRST.)
               J = I
C
C              NOW FIND OUT HOW FAR TO BACK UP IN THE X-ARRAY.
               DO 46  I = 1, IR-1
                  IF (XE(J) .LT. X(I)) GO TO 47
   46          CONTINUE
C              NB-- CAN NEVER DROP THROUGH HERE, SINCE XE(J).LT.X(IR-1).
C
   47          CONTINUE
C              AT THIS POINT, EITHER  XE(J) .LT. X(1)
C                 OR      X(I-1) .LE. XE(J) .LT. X(I) .
C              RESET IR, RECOGNIZING THAT IT WILL BE INCREMENTED BEFORE
C              CYCLING.
               IR = MAX(1, I-1)
C           ENDIF
C        ENDIF
   49    CONTINUE
C
         JFIRST = J
C
C     END OF IR-LOOP.
C
   50 CONTINUE
      IR = IR + 1
      IF (IR .LE. N)  GO TO 10
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHFE',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'PCHFE', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'PCHFE', 'X-ARRAY NOT STRICTLY INCREASING'
     +   , IERR, 1)
      RETURN
C
 5004 CONTINUE
C     NE.LT.1 RETURN.
      IERR = -4
      CALL XERMSG ('SLATEC', 'PCHFE',
     +   'NUMBER OF EVALUATION POINTS LESS THAN ONE', IERR, 1)
      RETURN
C
 5005 CONTINUE
C     ERROR RETURN FROM CHFEV.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -5
      CALL XERMSG ('SLATEC', 'PCHFE',
     +   'ERROR RETURN FROM CHFEV -- FATAL', IERR, 2)
      RETURN
      END
      REAL FUNCTION PCHIA (N, X, F, D, INCFD, SKIP, A, B, IERR)

c*********************************************************************72
c
cc PCHIA evaluates the definite integral of a piecewise cubic Hermite function.
c
C***BEGIN PROLOGUE  PCHIA
C***PURPOSE  Evaluate the definite integral of a piecewise cubic
C            Hermite function over an arbitrary interval.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3, H2A1B2
C***TYPE      SINGLE PRECISION (PCHIA-S, DPCHIA-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, NUMERICAL INTEGRATION, PCHIP,
C             QUADRATURE
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          PCHIA:  Piecewise Cubic Hermite Integrator, Arbitrary limits
C
C     Evaluates the definite integral of the cubic Hermite function
C     defined by  N, X, F, D  over the interval [A, B].
C
C     To provide compatibility with PCHIM and PCHIC, includes an
C     increment between successive values of the F- and D-arrays.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, IERR
C        REAL  X(N), F(INCFD,N), D(INCFD,N), A, B
C        REAL  VALUE, PCHIA
C        LOGICAL  SKIP
C
C        VALUE = PCHIA (N, X, F, D, INCFD, SKIP, A, B, IERR)
C
C   Parameters:
C
C     VALUE -- (output) value of the requested integral.
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real array of function values.  F(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     D -- (input) real array of derivative values.  D(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     INCFD -- (input) increment between successive values in F and D.
C           (Error return if  INCFD.LT.1 .)
C
C     SKIP -- (input/output) logical variable which should be set to
C           .TRUE. if the user wishes to skip checks for validity of
C           preceding parameters, or to .FALSE. otherwise.
C           This will save time in case these checks have already
C           been performed (say, in PCHIM or PCHIC).
C           SKIP will be set to .TRUE. on return with IERR.GE.0 .
C
C     A,B -- (input) the limits of integration.
C           NOTE:  There is no requirement that [A,B] be contained in
C                  [X(1),X(N)].  However, the resulting integral value
C                  will be highly suspect, if not.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning errors:
C              IERR = 1  if  A  is outside the interval [X(1),X(N)].
C              IERR = 2  if  B  is outside the interval [X(1),X(N)].
C              IERR = 3  if both of the above are true.  (Note that this
C                        means that either [A,B] contains data interval
C                        or the intervals do not intersect at all.)
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C                (VALUE will be zero in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C              IERR = -4  in case of an error return from PCHID (which
C                         should never occur).
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CHFIE, PCHID, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820730  DATE WRITTEN
C   820804  Converted to SLATEC library version.
C   870707  Corrected double precision conversion instructions.
C   870813  Minor cosmetic changes.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   930503  Corrected to set VALUE=0 when IERR.lt.0.  (FNF)
C   930504  Changed CHFIV to CHFIE.  (FNF)
C***END PROLOGUE  PCHIA
C
C  Programming notes:
C  1. The error flag from PCHID is tested, because a logic flaw
C     could conceivably result in IERD=-4, which should be reported.
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, IERR
      REAL  X(*), F(INCFD,*), D(INCFD,*), A, B
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IA, IB, IERD, IL, IR
      REAL  VALUE, XA, XB, ZERO
      SAVE ZERO
      REAL  CHFIE, PCHID
C
C  INITIALIZE.
C
      DATA  ZERO /0./
C***FIRST EXECUTABLE STATEMENT  PCHIA
      VALUE = ZERO
C
C  VALIDITY-CHECK ARGUMENTS.
C
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
    5 CONTINUE
      SKIP = .TRUE.
      IERR = 0
      IF ( (A.LT.X(1)) .OR. (A.GT.X(N)) )  IERR = IERR + 1
      IF ( (B.LT.X(1)) .OR. (B.GT.X(N)) )  IERR = IERR + 2
C
C  COMPUTE INTEGRAL VALUE.
C
      IF (A .NE. B)  THEN
         XA = MIN (A, B)
         XB = MAX (A, B)
         IF (XB .LE. X(2))  THEN
C           INTERVAL IS TO LEFT OF X(2), SO USE FIRST CUBIC.
C                   --------------------------------------
            VALUE = CHFIE (X(1),X(2), F(1,1),F(1,2),
     +                                D(1,1),D(1,2), A, B)
C                   --------------------------------------
         ELSE IF (XA .GE. X(N-1))  THEN
C           INTERVAL IS TO RIGHT OF X(N-1), SO USE LAST CUBIC.
C                   -----------------------------------------
            VALUE = CHFIE(X(N-1),X(N), F(1,N-1),F(1,N),
     +                                 D(1,N-1),D(1,N), A, B)
C                   -----------------------------------------
         ELSE
C           'NORMAL' CASE -- XA.LT.XB, XA.LT.X(N-1), XB.GT.X(2).
C      ......LOCATE IA AND IB SUCH THAT
C               X(IA-1).LT.XA.LE.X(IA).LE.X(IB).LE.XB.LE.X(IB+1)
            IA = 1
            DO 10  I = 1, N-1
               IF (XA .GT. X(I))  IA = I + 1
   10       CONTINUE
C             IA = 1 IMPLIES XA.LT.X(1) .  OTHERWISE,
C             IA IS LARGEST INDEX SUCH THAT X(IA-1).LT.XA,.
C
            IB = N
            DO 20  I = N, IA, -1
               IF (XB .LT. X(I))  IB = I - 1
   20       CONTINUE
C             IB = N IMPLIES XB.GT.X(N) .  OTHERWISE,
C             IB IS SMALLEST INDEX SUCH THAT XB.LT.X(IB+1) .
C
C     ......COMPUTE THE INTEGRAL.
            IF (IB .LT. IA)  THEN
C              THIS MEANS IB = IA-1 AND
C                 (A,B) IS A SUBSET OF (X(IB),X(IA)).
C                      ------------------------------------------
               VALUE = CHFIE (X(IB),X(IA), F(1,IB),F(1,IA),
     +                                     D(1,IB),D(1,IA), A, B)
C                      ------------------------------------------
            ELSE
C
C              FIRST COMPUTE INTEGRAL OVER (X(IA),X(IB)).
C                (Case (IB .EQ. IA) is taken care of by initialization
C                 of VALUE to ZERO.)
               IF (IB .GT. IA)  THEN
C                         ---------------------------------------------
                  VALUE = PCHID (N, X, F, D, INCFD, SKIP, IA, IB, IERD)
C                         ---------------------------------------------
                  IF (IERD .LT. 0)  GO TO 5004
               ENDIF
C
C              THEN ADD ON INTEGRAL OVER (XA,X(IA)).
               IF (XA .LT. X(IA))  THEN
                  IL = MAX(1, IA-1)
                  IR = IL + 1
C                                 -------------------------------------
                  VALUE = VALUE + CHFIE (X(IL),X(IR), F(1,IL),F(1,IR),
     +                                      D(1,IL),D(1,IR), XA, X(IA))
C                                 -------------------------------------
               ENDIF
C
C              THEN ADD ON INTEGRAL OVER (X(IB),XB).
               IF (XB .GT. X(IB))  THEN
                  IR = MIN (IB+1, N)
                  IL = IR - 1
C                                 -------------------------------------
                  VALUE = VALUE + CHFIE (X(IL),X(IR), F(1,IL),F(1,IR),
     +                                      D(1,IL),D(1,IR), X(IB), XB)
C                                 -------------------------------------
               ENDIF
C
C              FINALLY, ADJUST SIGN IF NECESSARY.
               IF (A .GT. B)  VALUE = -VALUE
            ENDIF
         ENDIF
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      PCHIA = VALUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHIA',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      GO TO 5000
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'PCHIA', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      GO TO 5000
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'PCHIA',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      GO TO 5000
C
 5004 CONTINUE
C     TROUBLE IN PCHID.  (SHOULD NEVER OCCUR.)
      IERR = -4
      CALL XERMSG ('SLATEC', 'PCHIA', 'TROUBLE IN PCHID', IERR, 1)
      GO TO 5000
      END
      SUBROUTINE PCHIC (IC, VC, SWITCH, N, X, F, D, INCFD, WK, NWK,
     +   IERR)

c*********************************************************************72
c
cc PCHIC sets derivatives for a monotone piecewise cubic Hermite interpolant.
c
C***BEGIN PROLOGUE  PCHIC
C***PURPOSE  Set derivatives needed to determine a piecewise monotone
C            piecewise cubic Hermite interpolant to given data.
C            User control is available over boundary conditions and/or
C            treatment of points where monotonicity switches direction.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E1A
C***TYPE      SINGLE PRECISION (PCHIC-S, DPCHIC-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION,
C             PCHIP, PIECEWISE CUBIC INTERPOLATION,
C             SHAPE-PRESERVING INTERPOLATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C         PCHIC:  Piecewise Cubic Hermite Interpolation Coefficients.
C
C     Sets derivatives needed to determine a piecewise monotone piece-
C     wise cubic interpolant to the data given in X and F satisfying the
C     boundary conditions specified by IC and VC.
C
C     The treatment of points where monotonicity switches direction is
C     controlled by argument SWITCH.
C
C     To facilitate two-dimensional applications, includes an increment
C     between successive values of the F- and D-arrays.
C
C     The resulting piecewise cubic Hermite function may be evaluated
C     by PCHFE or PCHFD.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  IC(2), N, NWK, IERR
C        REAL  VC(2), SWITCH, X(N), F(INCFD,N), D(INCFD,N), WK(NWK)
C
C        CALL  PCHIC (IC, VC, SWITCH, N, X, F, D, INCFD, WK, NWK, IERR)
C
C   Parameters:
C
C     IC -- (input) integer array of length 2 specifying desired
C           boundary conditions:
C           IC(1) = IBEG, desired condition at beginning of data.
C           IC(2) = IEND, desired condition at end of data.
C
C           IBEG = 0  for the default boundary condition (the same as
C                     used by PCHIM).
C           If IBEG.NE.0, then its sign indicates whether the boundary
C                     derivative is to be adjusted, if necessary, to be
C                     compatible with monotonicity:
C              IBEG.GT.0  if no adjustment is to be performed.
C              IBEG.LT.0  if the derivative is to be adjusted for
C                     monotonicity.
C
C           Allowable values for the magnitude of IBEG are:
C           IBEG = 1  if first derivative at X(1) is given in VC(1).
C           IBEG = 2  if second derivative at X(1) is given in VC(1).
C           IBEG = 3  to use the 3-point difference formula for D(1).
C                     (Reverts to the default b.c. if N.LT.3 .)
C           IBEG = 4  to use the 4-point difference formula for D(1).
C                     (Reverts to the default b.c. if N.LT.4 .)
C           IBEG = 5  to set D(1) so that the second derivative is con-
C              tinuous at X(2). (Reverts to the default b.c. if N.LT.4.)
C              This option is somewhat analogous to the "not a knot"
C              boundary condition provided by PCHSP.
C
C          NOTES (IBEG):
C           1. An error return is taken if ABS(IBEG).GT.5 .
C           2. Only in case  IBEG.LE.0  is it guaranteed that the
C              interpolant will be monotonic in the first interval.
C              If the returned value of D(1) lies between zero and
C              3*SLOPE(1), the interpolant will be monotonic.  This
C              is **NOT** checked if IBEG.GT.0 .
C           3. If IBEG.LT.0 and D(1) had to be changed to achieve mono-
C              tonicity, a warning error is returned.
C
C           IEND may take on the same values as IBEG, but applied to
C           derivative at X(N).  In case IEND = 1 or 2, the value is
C           given in VC(2).
C
C          NOTES (IEND):
C           1. An error return is taken if ABS(IEND).GT.5 .
C           2. Only in case  IEND.LE.0  is it guaranteed that the
C              interpolant will be monotonic in the last interval.
C              If the returned value of D(1+(N-1)*INCFD) lies between
C              zero and 3*SLOPE(N-1), the interpolant will be monotonic.
C              This is **NOT** checked if IEND.GT.0 .
C           3. If IEND.LT.0 and D(1+(N-1)*INCFD) had to be changed to
C              achieve monotonicity, a warning error is returned.
C
C     VC -- (input) real array of length 2 specifying desired boundary
C           values, as indicated above.
C           VC(1) need be set only if IC(1) = 1 or 2 .
C           VC(2) need be set only if IC(2) = 1 or 2 .
C
C     SWITCH -- (input) indicates desired treatment of points where
C           direction of monotonicity switches:
C           Set SWITCH to zero if interpolant is required to be mono-
C           tonic in each interval, regardless of monotonicity of data.
C             NOTES:
C              1. This will cause D to be set to zero at all switch
C                 points, thus forcing extrema there.
C              2. The result of using this option with the default boun-
C                 dary conditions will be identical to using PCHIM, but
C                 will generally cost more compute time.
C                 This option is provided only to facilitate comparison
C                 of different switch and/or boundary conditions.
C           Set SWITCH nonzero to use a formula based on the 3-point
C              difference formula in the vicinity of switch points.
C           If SWITCH is positive, the interpolant on each interval
C              containing an extremum is controlled to not deviate from
C              the data by more than SWITCH*DFLOC, where DFLOC is the
C              maximum of the change of F on this interval and its two
C              immediate neighbors.
C           If SWITCH is negative, no such control is to be imposed.
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real array of dependent variable values to be inter-
C           polated.  F(1+(I-1)*INCFD) is value corresponding to X(I).
C
C     D -- (output) real array of derivative values at the data points.
C           These values will determine a monotone cubic Hermite func-
C           tion on each subinterval on which the data are monotonic,
C           except possibly adjacent to switches in monotonicity.
C           The value corresponding to X(I) is stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C           No other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in F and D.
C           This argument is provided primarily for 2-D applications.
C           (Error return if  INCFD.LT.1 .)
C
C     WK -- (scratch) real array of working storage.  The user may wish
C           to know that the returned values are:
C              WK(I)     = H(I)     = X(I+1) - X(I) ;
C              WK(N-1+I) = SLOPE(I) = (F(1,I+1) - F(1,I)) / H(I)
C           for  I = 1(1)N-1.
C
C     NWK -- (input) length of work array.
C           (Error return if  NWK.LT.2*(N-1) .)
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning errors:
C              IERR = 1  if IBEG.LT.0 and D(1) had to be adjusted for
C                        monotonicity.
C              IERR = 2  if IEND.LT.0 and D(1+(N-1)*INCFD) had to be
C                        adjusted for monotonicity.
C              IERR = 3  if both of the above are true.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if ABS(IBEG).GT.5 .
C              IERR = -5  if ABS(IEND).GT.5 .
C              IERR = -6  if both of the above are true.
C              IERR = -7  if NWK.LT.2*(N-1) .
C             (The D-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  1. F. N. Fritsch, Piecewise Cubic Hermite Interpolation
C                 Package, Report UCRL-87285, Lawrence Livermore Nation-
C                 al Laboratory, July 1982.  [Poster presented at the
C                 SIAM 30th Anniversary Meeting, 19-23 July 1982.]
C               2. F. N. Fritsch and J. Butland, A method for construc-
C                 ting local monotone piecewise cubic interpolants, SIAM
C                 Journal on Scientific and Statistical Computing 5, 2
C                 (June 1984), pp. 300-304.
C               3. F. N. Fritsch and R. E. Carlson, Monotone piecewise
C                 cubic interpolation, SIAM Journal on Numerical Ana-
C                 lysis 17, 2 (April 1980), pp. 238-246.
C***ROUTINES CALLED  PCHCE, PCHCI, PCHCS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820218  DATE WRITTEN
C   820804  Converted to SLATEC library version.
C   870813  Updated Reference 2.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920429  Revised format and order of references.  (WRB,FNF)
C***END PROLOGUE  PCHIC
C  Programming notes:
C
C     To produce a double precision version, simply:
C        a. Change PCHIC to DPCHIC wherever it occurs,
C        b. Change PCHCE to DPCHCE wherever it occurs,
C        c. Change PCHCI to DPCHCI wherever it occurs,
C        d. Change PCHCS to DPCHCS wherever it occurs,
C        e. Change the real declarations to double precision, and
C        f. Change the constant  ZERO  to double precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  IC(2), N, INCFD, NWK, IERR
      REAL  VC(2), SWITCH, X(*), F(INCFD,*), D(INCFD,*), WK(NWK)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IBEG, IEND, NLESS1
      REAL  ZERO
      SAVE ZERO
      DATA  ZERO /0./
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  PCHIC
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
      IBEG = IC(1)
      IEND = IC(2)
      IERR = 0
      IF (ABS(IBEG) .GT. 5)  IERR = IERR - 1
      IF (ABS(IEND) .GT. 5)  IERR = IERR - 2
      IF (IERR .LT. 0)  GO TO 5004
C
C  FUNCTION DEFINITION IS OK -- GO ON.
C
      NLESS1 = N - 1
      IF ( NWK .LT. 2*NLESS1 )  GO TO 5007
C
C  SET UP H AND SLOPE ARRAYS.
C
      DO 20  I = 1, NLESS1
         WK(I) = X(I+1) - X(I)
         WK(NLESS1+I) = (F(1,I+1) - F(1,I)) / WK(I)
   20 CONTINUE
C
C  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION.
C
      IF (NLESS1 .GT. 1)  GO TO 1000
      D(1,1) = WK(2)
      D(1,N) = WK(2)
      GO TO 3000
C
C  NORMAL CASE  (N .GE. 3) .
C
 1000 CONTINUE
C
C  SET INTERIOR DERIVATIVES AND DEFAULT END CONDITIONS.
C
C     --------------------------------------
      CALL PCHCI (N, WK(1), WK(N), D, INCFD)
C     --------------------------------------
C
C  SET DERIVATIVES AT POINTS WHERE MONOTONICITY SWITCHES DIRECTION.
C
      IF (SWITCH .EQ. ZERO)  GO TO 3000
C     ----------------------------------------------------
      CALL PCHCS (SWITCH, N, WK(1), WK(N), D, INCFD, IERR)
C     ----------------------------------------------------
      IF (IERR .NE. 0)  GO TO 5008
C
C  SET END CONDITIONS.
C
 3000 CONTINUE
      IF ( (IBEG.EQ.0) .AND. (IEND.EQ.0) )  GO TO 5000
C     -------------------------------------------------------
      CALL PCHCE (IC, VC, N, X, WK(1), WK(N), D, INCFD, IERR)
C     -------------------------------------------------------
      IF (IERR .LT. 0)  GO TO 5009
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHIC',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'PCHIC', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'PCHIC', 'X-ARRAY NOT STRICTLY INCREASING'
     +   , IERR, 1)
      RETURN
C
 5004 CONTINUE
C     IC OUT OF RANGE RETURN.
      IERR = IERR - 3
      CALL XERMSG ('SLATEC', 'PCHIC', 'IC OUT OF RANGE', IERR, 1)
      RETURN
C
 5007 CONTINUE
C     NWK .LT. 2*(N-1)  RETURN.
      IERR = -7
      CALL XERMSG ('SLATEC', 'PCHIC', 'WORK ARRAY TOO SMALL', IERR, 1)
      RETURN
C
 5008 CONTINUE
C     ERROR RETURN FROM PCHCS.
      IERR = -8
      CALL XERMSG ('SLATEC', 'PCHIC', 'ERROR RETURN FROM PCHCS', IERR,
     +   1)
      RETURN
C
 5009 CONTINUE
C     ERROR RETURN FROM PCHCE.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -9
      CALL XERMSG ('SLATEC', 'PCHIC', 'ERROR RETURN FROM PCHCE', IERR,
     +   1)
      RETURN
      END
      REAL FUNCTION PCHID (N, X, F, D, INCFD, SKIP, IA, IB, IERR)

c*********************************************************************72
c
cc PCHID: integral of a piecewise cubic Hermite function between data points.
c
C***BEGIN PROLOGUE  PCHID
C***PURPOSE  Evaluate the definite integral of a piecewise cubic
C            Hermite function over an interval whose endpoints are data
C            points.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3, H2A1B2
C***TYPE      SINGLE PRECISION (PCHID-S, DPCHID-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, NUMERICAL INTEGRATION, PCHIP,
C             QUADRATURE
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          PCHID:  Piecewise Cubic Hermite Integrator, Data limits
C
C     Evaluates the definite integral of the cubic Hermite function
C     defined by  N, X, F, D  over the interval [X(IA), X(IB)].
C
C     To provide compatibility with PCHIM and PCHIC, includes an
C     increment between successive values of the F- and D-arrays.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, IA, IB, IERR
C        REAL  X(N), F(INCFD,N), D(INCFD,N)
C        LOGICAL  SKIP
C
C        VALUE = PCHID (N, X, F, D, INCFD, SKIP, IA, IB, IERR)
C
C   Parameters:
C
C     VALUE -- (output) value of the requested integral.
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real array of function values.  F(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     D -- (input) real array of derivative values.  D(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     INCFD -- (input) increment between successive values in F and D.
C           (Error return if  INCFD.LT.1 .)
C
C     SKIP -- (input/output) logical variable which should be set to
C           .TRUE. if the user wishes to skip checks for validity of
C           preceding parameters, or to .FALSE. otherwise.
C           This will save time in case these checks have already
C           been performed (say, in PCHIM or PCHIC).
C           SKIP will be set to .TRUE. on return with IERR = 0 or -4.
C
C     IA,IB -- (input) indices in X-array for the limits of integration.
C           both must be in the range [1,N].  (Error return if not.)
C           No restrictions on their relative values.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if IA or IB is out of range.
C                (VALUE will be zero in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820723  DATE WRITTEN
C   820804  Converted to SLATEC library version.
C   870813  Minor cosmetic changes.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   930504  Corrected to set VALUE=0 when IERR.ne.0.  (FNF)
C***END PROLOGUE  PCHID
C
C  Programming notes:
C  1. This routine uses a special formula that is valid only for
C     integrals whose limits coincide with data values.  This is
C     mathematically equivalent to, but much more efficient than,
C     calls to CHFIE.
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, IA, IB, IERR
      REAL  X(*), F(INCFD,*), D(INCFD,*)
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IUP, LOW
      REAL  H, HALF, SIX, SUM, VALUE, ZERO
      SAVE ZERO, HALF, SIX
C
C  INITIALIZE.
C
      DATA  ZERO /0./,  HALF /0.5/,  SIX /6./
C***FIRST EXECUTABLE STATEMENT  PCHID
      VALUE = ZERO
C
C  VALIDITY-CHECK ARGUMENTS.
C
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
    5 CONTINUE
      SKIP = .TRUE.
      IF ((IA.LT.1) .OR. (IA.GT.N))  GO TO 5004
      IF ((IB.LT.1) .OR. (IB.GT.N))  GO TO 5004
      IERR = 0
C
C  COMPUTE INTEGRAL VALUE.
C
      IF (IA .NE. IB)  THEN
         LOW = MIN(IA, IB)
         IUP = MAX(IA, IB) - 1
         SUM = ZERO
         DO 10  I = LOW, IUP
            H = X(I+1) - X(I)
            SUM = SUM + H*( (F(1,I) + F(1,I+1)) +
     *                      (D(1,I) - D(1,I+1))*(H/SIX) )
   10    CONTINUE
         VALUE = HALF * SUM
         IF (IA .GT. IB)  VALUE = -VALUE
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      PCHID = VALUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHID',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      GO TO 5000
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'PCHID', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      GO TO 5000
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'PCHID',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      GO TO 5000
C
 5004 CONTINUE
C     IA OR IB OUT OF RANGE RETURN.
      IERR = -4
      CALL XERMSG ('SLATEC', 'PCHID', 'IA OR IB OUT OF RANGE', IERR, 1)
      GO TO 5000
      END
      SUBROUTINE PCHIM (N, X, F, D, INCFD, IERR)

c*********************************************************************72
c
cc PCHIM sets derivatives for a monotone piecewise cubic Hermite interpolant.
c
C***BEGIN PROLOGUE  PCHIM
C***PURPOSE  Set derivatives needed to determine a monotone piecewise
C            cubic Hermite interpolant to given data.  Boundary values
C            are provided which are compatible with monotonicity.  The
C            interpolant will have an extremum at each point where mono-
C            tonicity switches direction.  (See PCHIC if user control is
C            desired over boundary or switch conditions.)
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E1A
C***TYPE      SINGLE PRECISION (PCHIM-S, DPCHIM-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION,
C             PCHIP, PIECEWISE CUBIC INTERPOLATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          PCHIM:  Piecewise Cubic Hermite Interpolation to
C                  Monotone data.
C
C     Sets derivatives needed to determine a monotone piecewise cubic
C     Hermite interpolant to the data given in X and F.
C
C     Default boundary conditions are provided which are compatible
C     with monotonicity.  (See PCHIC if user control of boundary con-
C     ditions is desired.)
C
C     If the data are only piecewise monotonic, the interpolant will
C     have an extremum at each point where monotonicity switches direc-
C     tion.  (See PCHIC if user control is desired in such cases.)
C
C     To facilitate two-dimensional applications, includes an increment
C     between successive values of the F- and D-arrays.
C
C     The resulting piecewise cubic Hermite function may be evaluated
C     by PCHFE or PCHFD.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, IERR
C        REAL  X(N), F(INCFD,N), D(INCFD,N)
C
C        CALL  PCHIM (N, X, F, D, INCFD, IERR)
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C           If N=2, simply does linear interpolation.
C
C     X -- (input) real array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real array of dependent variable values to be inter-
C           polated.  F(1+(I-1)*INCFD) is value corresponding to X(I).
C           PCHIM is designed for monotonic data, but it will work for
C           any F-array.  It will force extrema at points where mono-
C           tonicity switches direction.  If some other treatment of
C           switch points is desired, PCHIC should be used instead.
C                                     -----
C     D -- (output) real array of derivative values at the data points.
C           If the data are monotonic, these values will determine a
C           a monotone cubic Hermite function.
C           The value corresponding to X(I) is stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C           No other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in F and D.
C           This argument is provided primarily for 2-D applications.
C           (Error return if  INCFD.LT.1 .)
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  means that IERR switches in the direction
C                 of monotonicity were detected.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C             (The D-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  1. F. N. Fritsch and J. Butland, A method for construc-
C                 ting local monotone piecewise cubic interpolants, SIAM
C                 Journal on Scientific and Statistical Computing 5, 2
C                 (June 1984), pp. 300-304.
C               2. F. N. Fritsch and R. E. Carlson, Monotone piecewise
C                 cubic interpolation, SIAM Journal on Numerical Ana-
C                 lysis 17, 2 (April 1980), pp. 238-246.
C***ROUTINES CALLED  PCHST, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811103  DATE WRITTEN
C   820201  1. Introduced  PCHST  to reduce possible over/under-
C             flow problems.
C           2. Rearranged derivative formula for same reason.
C   820602  1. Modified end conditions to be continuous functions
C             of data when monotonicity switches in next interval.
C           2. Modified formulas so end conditions are less prone
C             of over/underflow problems.
C   820803  Minor cosmetic changes for release 1.
C   870813  Updated Reference 1.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920429  Revised format and order of references.  (WRB,FNF)
C***END PROLOGUE  PCHIM
C  Programming notes:
C
C     1. The function  PCHST(ARG1,ARG2)  is assumed to return zero if
C        either argument is zero, +1 if they are of the same sign, and
C        -1 if they are of opposite sign.
C     2. To produce a double precision version, simply:
C        a. Change PCHIM to DPCHIM wherever it occurs,
C        b. Change PCHST to DPCHST wherever it occurs,
C        c. Change all references to the Fortran intrinsics to their
C           double precision equivalents,
C        d. Change the real declarations to double precision, and
C        e. Change the constants ZERO and THREE to double precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, IERR
      REAL  X(*), F(INCFD,*), D(INCFD,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, NLESS1
      REAL  DEL1, DEL2, DMAX, DMIN, DRAT1, DRAT2, DSAVE,
     *      H1, H2, HSUM, HSUMT3, THREE, W1, W2, ZERO
      SAVE ZERO, THREE
      REAL  PCHST
      DATA  ZERO /0./,  THREE /3./
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  PCHIM
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
      IERR = 0
      NLESS1 = N - 1
      H1 = X(2) - X(1)
      DEL1 = (F(1,2) - F(1,1))/H1
      DSAVE = DEL1
C
C  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION.
C
      IF (NLESS1 .GT. 1)  GO TO 10
      D(1,1) = DEL1
      D(1,N) = DEL1
      GO TO 5000
C
C  NORMAL CASE  (N .GE. 3).
C
   10 CONTINUE
      H2 = X(3) - X(2)
      DEL2 = (F(1,3) - F(1,2))/H2
C
C  SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE
C     SHAPE-PRESERVING.
C
      HSUM = H1 + H2
      W1 = (H1 + HSUM)/HSUM
      W2 = -H1/HSUM
      D(1,1) = W1*DEL1 + W2*DEL2
      IF ( PCHST(D(1,1),DEL1) .LE. ZERO)  THEN
         D(1,1) = ZERO
      ELSE IF ( PCHST(DEL1,DEL2) .LT. ZERO)  THEN
C        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES.
         DMAX = THREE*DEL1
         IF (ABS(D(1,1)) .GT. ABS(DMAX))  D(1,1) = DMAX
      ENDIF
C
C  LOOP THROUGH INTERIOR POINTS.
C
      DO 50  I = 2, NLESS1
         IF (I .EQ. 2)  GO TO 40
C
         H1 = H2
         H2 = X(I+1) - X(I)
         HSUM = H1 + H2
         DEL1 = DEL2
         DEL2 = (F(1,I+1) - F(1,I))/H2
   40    CONTINUE
C
C        SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC.
C
         D(1,I) = ZERO
         IF ( PCHST(DEL1,DEL2) )  42, 41, 45
C
C        COUNT NUMBER OF CHANGES IN DIRECTION OF MONOTONICITY.
C
   41    CONTINUE
         IF (DEL2 .EQ. ZERO)  GO TO 50
         IF ( PCHST(DSAVE,DEL2) .LT. ZERO)  IERR = IERR + 1
         DSAVE = DEL2
         GO TO 50
C
   42    CONTINUE
         IERR = IERR + 1
         DSAVE = DEL2
         GO TO 50
C
C        USE BRODLIE MODIFICATION OF BUTLAND FORMULA.
C
   45    CONTINUE
         HSUMT3 = HSUM+HSUM+HSUM
         W1 = (HSUM + H1)/HSUMT3
         W2 = (HSUM + H2)/HSUMT3
         DMAX = MAX( ABS(DEL1), ABS(DEL2) )
         DMIN = MIN( ABS(DEL1), ABS(DEL2) )
         DRAT1 = DEL1/DMAX
         DRAT2 = DEL2/DMAX
         D(1,I) = DMIN/(W1*DRAT1 + W2*DRAT2)
C
   50 CONTINUE
C
C  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE
C     SHAPE-PRESERVING.
C
      W1 = -H2/HSUM
      W2 = (H2 + HSUM)/HSUM
      D(1,N) = W1*DEL1 + W2*DEL2
      IF ( PCHST(D(1,N),DEL2) .LE. ZERO)  THEN
         D(1,N) = ZERO
      ELSE IF ( PCHST(DEL1,DEL2) .LT. ZERO)  THEN
C        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES.
         DMAX = THREE*DEL2
         IF (ABS(D(1,N)) .GT. ABS(DMAX))  D(1,N) = DMAX
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHIM',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'PCHIM', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'PCHIM', 'X-ARRAY NOT STRICTLY INCREASING'
     +   , IERR, 1)
      RETURN
      END
      SUBROUTINE PCHKT (N, X, KNOTYP, T)

c*********************************************************************72
c
cc PCHKT computes the B-spline knot sequence for PCHBS.
c
C***BEGIN PROLOGUE  PCHKT
C***SUBSIDIARY
C***PURPOSE  Compute B-spline knot sequence for PCHBS.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3
C***TYPE      SINGLE PRECISION (PCHKT-S, DPCHKT-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C     Set a knot sequence for the B-spline representation of a PCH
C     function with breakpoints X.  All knots will be at least double.
C     Endknots are set as:
C        (1) quadruple knots at endpoints if KNOTYP=0;
C        (2) extrapolate the length of end interval if KNOTYP=1;
C        (3) periodic if KNOTYP=2.
C
C  Input arguments:  N, X, KNOTYP.
C  Output arguments:  T.
C
C  Restrictions/assumptions:
C     1. N.GE.2 .  (not checked)
C     2. X(i).LT.X(i+1), i=1,...,N .  (not checked)
C     3. 0.LE.KNOTYP.LE.2 .  (Acts like KNOTYP=0 for any other value.)
C
C***SEE ALSO  PCHBS
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   870701  DATE WRITTEN
C   900405  Converted Fortran to upper case.
C   900410  Converted prologue to SLATEC 4.0 format.
C   900410  Minor cosmetic changes.
C   930514  Changed NKNOTS from an output to an input variable.  (FNF)
C   930604  Removed unused variable NKNOTS from argument list.  (FNF)
C***END PROLOGUE  PCHKT
C
C*Internal Notes:
C
C  Since this is subsidiary to PCHBS, which validates its input before
C  calling, it is unnecessary for such validation to be done here.
C
C**End
C
C  Declare arguments.
C
      INTEGER  N, KNOTYP
      REAL  X(*), T(*)
C
C  Declare local variables.
C
      INTEGER  J, K, NDIM
      REAL  HBEG, HEND
C***FIRST EXECUTABLE STATEMENT  PCHKT
C
C  Initialize.
C
      NDIM = 2*N
C
C  Set interior knots.
C
      J = 1
      DO 20  K = 1, N
         J = J + 2
         T(J) = X(K)
         T(J+1) = T(J)
   20 CONTINUE
C     Assertion:  At this point T(3),...,T(NDIM+2) have been set and
C                 J=NDIM+1.
C
C  Set end knots according to KNOTYP.
C
      HBEG = X(2) - X(1)
      HEND = X(N) - X(N-1)
      IF (KNOTYP.EQ.1 )  THEN
C          Extrapolate.
         T(2) = X(1) - HBEG
         T(NDIM+3) = X(N) + HEND
      ELSE IF ( KNOTYP.EQ.2 )  THEN
C          Periodic.
         T(2) = X(1) - HEND
         T(NDIM+3) = X(N) + HBEG
      ELSE
C          Quadruple end knots.
         T(2) = X(1)
         T(NDIM+3) = X(N)
      ENDIF
      T(1) = T(2)
      T(NDIM+4) = T(NDIM+3)
C
C  Terminate.
C
      RETURN
      END
      real FUNCTION PCHQA(N,X,F,D,A,B,IERR)

c*********************************************************************72
c
cc PCHQA: definite integral of spline or piecewise cubic Hermite interpolant.
c
C***BEGIN PROLOGUE  PCHQA
C***DATE WRITTEN   870829   (YYMMDD)
C***REVISION DATE  870829   (YYMMDD)
C***CATEGORY NO.  E3,H2A2
C***KEYWORDS  EASY TO USE CUBIC HERMITE OR SPLINE INTEGRATION
C             NUMERICAL INTEGRATION, QUADRATURE
C***AUTHOR  KAHANER, D.K., (NBS)
C             SCIENTIFIC COMPUTING DIVISION
C             NATIONAL BUREAU OF STANDARDS
C             ROOM A161, TECHNOLOGY BUILDING
C             GAITHERSBURG, MARYLAND 20899
C             (301) 975-3808
C***PURPOSE  Evaluates the definite integral of a piecewise cubic Hermit
C            or spline function over an arbitrary interval, easy to use.
C***DESCRIPTION
C
C          PCHQA:  Piecewise Cubic Hermite or Spline Integrator,
C                  Arbitrary limits, Easy to Use.
C
C          From the book "Numerical Methods and Software"
C                  by  D. Kahaner, C. Moler, S. Nash
C                          Prentice Hall 1988
C
C     Evaluates the definite integral of the cubic Hermite or spline
C     function defined by  N, X, F, D  over the interval [A, B].  This
C     is an easy to use driver for the routine PCHIA by F.N. Fritsch
C     described in reference (2) below. That routine also has other
C     capabilities.
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C           VALUE = PCHQA (N, X, F, D, A, B, IERR)
C
C     INTEGER  N, IERR
C     real  X(N), F(N), D(N), A, B
C
C   Parameters:
C
C     VALUE -- (output) VALUE of the requested integral.
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real array of independent variable
C           values.  The elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real array of function values.
C           F(I) is the value corresponding to X(I).
C
C     D -- (input) real array of derivative values.  D(I) is
C           the value corresponding to X(I).
C
C     A,B -- (input) the limits of integration.
C           NOTE:  There is no requirement that [A,B] be contained in
C                  [X(1),X(N)].  However, the resulting integral value
C                  will be highly suspect, if not.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning errors:
C              IERR = 1  if  A  is outside the interval [X(1),X(N)].
C              IERR = 2  if  B  is outside the interval [X(1),X(N)].
C              IERR = 3  if both of the above are true.  (Note that this
C                        means that either [A,B] contains data interval
C                        or the intervals do not intersect at all.)
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -3  if the X-array is not strictly increasing.
C                (Value has not been computed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  1. F.N.FRITSCH AND R.E.CARLSON, 'MONOTONE PIECEWISE
C                 CUBIC INTERPOLATION,' SIAM J.NUMER.ANAL. 17, 2 (APRIL
C                 1980), 238-246.
C               2. F.N.FRITSCH, 'PIECEWISE CUBIC HERMITE INTERPOLATION
C                 PACKAGE, FINAL SPECIFICATIONS', LAWRENCE LIVERMORE
C                 NATIONAL LABORATORY, COMPUTER DOCUMENTATION UCID-30194
C                 AUGUST 1982.
C***ROUTINES CALLED  PCHIA
C***END PROLOGUE  PCHQA
      INTEGER  N, IERR
      real  X(N), F(N), D(N), A, B
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  INCFD
      real  PCHIA
      LOGICAL SKIP
C
C  INITIALIZE.
C
      DATA  INCFD /1/
      DATA  SKIP /.TRUE./
C
C
C***FIRST EXECUTABLE STATEMENT  PCHQA

      PCHQA  =  PCHIA( N, X, F, D, INCFD, SKIP, A, B, IERR )
C
C ERROR MESSAGES ARE FROM LOWER LEVEL ROUTINES
      RETURN
      END
      SUBROUTINE PCHQK1 (LUN, KPRINT, IPASS)

c*********************************************************************72
c
cc PCHQK1 tests the PCHIP evaluators CHFDV, CHFEV, PCHFD and PCHFE.
c
C***BEGIN PROLOGUE  PCHQK1
C***PURPOSE  Test the PCHIP evaluators CHFDV, CHFEV, PCHFD and PCHFE.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (PCHQK1-S, DPCHQ1-D)
C***KEYWORDS  PCHIP EVALUATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C              PCHIP QUICK CHECK NUMBER 1
C
C     TESTS THE EVALUATORS:  CHFDV, CHFEV, PCHFD, PCHFE.
C *Usage:
C
C        INTEGER  LUN, KPRINT, IPASS
C
C        CALL PCHQK1 (LUN, KPRINT, IPASS)
C
C *Arguments:
C
C     LUN   :IN  is the unit number to which output is to be written.
C
C     KPRINT:IN  controls the amount of output, as specified in the
C                SLATEC Guidelines.
C
C     IPASS:OUT  will contain a pass/fail flag.  IPASS=1 is good.
C                IPASS=0 indicates one or more tests failed.
C
C *Description:
C
C   This routine carries out three tests of the PCH evaluators:
C     EVCHCK tests the single-cubic evaluators.
C     EVPCCK tests the full PCH evaluators.
C     EVERCK exercises the error returns in all evaluators.
C
C***ROUTINES CALLED  EVCHCK, EVERCK, EVPCCK
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   890306  Changed IPASS to the more accurate name IFAIL.  (FNF)
C   890618  REVISION DATE from Version 3.2
C   890706  Cosmetic changes to prologue.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900309  Added EVERCK to list of routines called.  (FNF)
C   900314  Improved some output formats.
C   900315  Revised prologue and improved some output formats.  (FNF)
C   900316  Additional minor cosmetic changes.  (FNF)
C   900321  Removed IFAIL from call sequence for SLATEC standards and
C           made miscellaneous cosmetic changes.  (FNF)
C   930317  Improved output formats.  (FNF)
C***END PROLOGUE  PCHQK1
C
C  Declare arguments.
C
      INTEGER  LUN, KPRINT, IPASS
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I1, I2, I3, I4, I5, I6, I7, I8, I9, IFAIL, NPTS
      REAL  WORK (4000)
      LOGICAL  FAIL
C
C***FIRST EXECUTABLE STATEMENT  PCHQK1
      IF (KPRINT .GE. 2)  WRITE (LUN, 1000) KPRINT
C
C  TEST CHFDV AND CHFEV.
C
      IFAIL = 0
      NPTS = 1000
      I1 = 1  + NPTS
      I2 = I1 + NPTS
      I3 = I2 + NPTS
      CALL EVCHCK (LUN, KPRINT, NPTS, WORK(1), WORK(I1), WORK(I2),
     *                                          WORK(I3), FAIL)
      IF (FAIL)  IFAIL = IFAIL + 1
C
C  TEST PCHFD AND PCHFE.
C
      I1 = 1  +  10
      I2 = I1 +  10
      I3 = I2 + 100
      I4 = I3 + 100
      I5 = I4 + 100
      I6 = I5 +  51
      I7 = I6 +  51
      I8 = I7 +  51
      I9 = I8 +  51
      CALL EVPCCK (LUN, KPRINT, WORK(1), WORK(I1), WORK(I2), WORK(I3),
     *             WORK(I4), WORK(I5), WORK(I6), WORK(I7), WORK(I8),
     *             WORK(I9), FAIL)
      IF (FAIL)  IFAIL = IFAIL + 2
C
C  TEST ERROR RETURNS.
C
      CALL EVERCK (LUN, KPRINT, FAIL)
      IF (FAIL)  IFAIL = IFAIL + 4
C
C  PRINT SUMMARY AND TERMINATE.
C     At this point, IFAIL has the following value:
C        IFAIL = 0  IF ALL TESTS PASSED.
C        IFAIL BETWEEN 1 AND 7 IS THE SUM OF:
C           IFAIL=1  IF SINGLE CUBIC TEST FAILED. (SEE EVCHCK OUTPUT.)
C           IFAIL=2  IF PCHFD/PCHFE  TEST FAILED. (SEE EVPCCK OUTPUT.)
C           IFAIL=4  IF ERROR RETURN TEST FAILED. (SEE EVERCK OUTPUT.)
C
      IF ((KPRINT.GE.2).AND.(IFAIL.NE.0))  WRITE (LUN, 3001)  IFAIL
C
      IF (IFAIL.EQ.0)  THEN
         IPASS = 1
         IF (KPRINT.GE.2) WRITE(LUN,99998)
      ELSE
         IPASS = 0
         IF (KPRINT.GE.1) WRITE(LUN,99999)
      ENDIF
C
      RETURN
C
C  FORMATS.
C
 1000 FORMAT ('1'/' ------------  PCHIP QUICK CHECK OUTPUT',
     *        ' ------------' //20X,'( KPRINT =',I2,' )')
 3001 FORMAT (/' *** TROUBLE ***',I5,' EVALUATION TESTS FAILED.')
99998 FORMAT (/' ------------  PCHIP PASSED  ALL EVALUATION TESTS',
     *        ' ------------')
99999 FORMAT (/' ************  PCHIP FAILED SOME EVALUATION TESTS',
     *        ' ************')
C------------- LAST LINE OF PCHQK1 FOLLOWS -----------------------------
      END
      SUBROUTINE PCHQK2 (LUN, KPRINT, IPASS)

c*********************************************************************72
c
cc PCHQK2 tests the PCHIP integrators PCHIA and PCHID.
c
C***BEGIN PROLOGUE  PCHQK2
C***PURPOSE  Test the PCHIP integrators PCHIA and PCHID.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (PCHQK2-S, DPCHQ2-D)
C***KEYWORDS  PCHIP INTEGRATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C              PCHIP QUICK CHECK NUMBER 2
C
C     TESTS THE INTEGRATORS:  PCHIA, PCHID.
C *Usage:
C
C        INTEGER  LUN, KPRINT, IPASS
C
C        CALL PCHQK2 (LUN, KPRINT, IPASS)
C
C *Arguments:
C
C     LUN   :IN  is the unit number to which output is to be written.
C
C     KPRINT:IN  controls the amount of output, as specified in the
C                SLATEC Guidelines.
C
C     IPASS:OUT  will contain a pass/fail flag.  IPASS=1 is good.
C                IPASS=0 indicates one or more tests failed.
C
C *Description:
C
C   This routine constructs data from a cubic, integrates it with PCHIA
C   and compares the results with the correct answer.
C   Since PCHIA calls PCHID, this tests both integrators.
C
C***ROUTINES CALLED  PCHIA, R1MACH
C***REVISION HISTORY  (YYMMDD)
C   820601  DATE WRITTEN
C   890306  Changed IPASS to the more accurate name IFAIL.  (FNF)
C   890316  Added declarations as in DPCHQ2.  (FNF)
C   890629  Appended E0 to real constants to reduce S.P./D.P.
C           differences.
C   890706  Cosmetic changes to prologue.  (WRB)
C   891004  Cosmetic changes to prologue.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900314  Improved some output formats.  (FNF)
C   900315  Revised prologue and improved some output formats.  (FNF)
C   900316  Additional minor cosmetic changes.  (FNF)
C   900321  Removed IFAIL from call sequence for SLATEC standards and
C           made miscellaneous cosmetic changes.  (FNF)
C   901130  Added 1P's to formats; changed to allow KPRINT.gt.3.  (FNF)
C   910708  Minor modifications in use of KPRINT.  (WRB)
C   930317  Improved output formats.  (FNF)
C***END PROLOGUE  PCHQK2
C
C  Declare arguments.
C
      INTEGER  LUN, KPRINT, IPASS
C
C  DECLARE VARIABLES.
C
      INTEGER  I, IEREXP(17), IERR, IFAIL, N, NPAIRS
      REAL  A(17), B(17), CALC, D(7), ERRMAX, ERROR, F(7), MACHEP,
     *      ONE, THREE, THRQTR, TOL, TRUE, TWO, X(7)
      LOGICAL  FAIL, SKIP
C
C  DECLARE EXTERNALS.
C
      REAL  PCHIA, R1MACH
C
C  DEFINE TEST FUNCTIONS.
C
      REAL  AX, FCN, DERIV, ANTDER
      FCN(AX) = THREE*AX*AX*(AX-TWO)
      DERIV(AX) = THREE*AX*(TWO*(AX-TWO) + AX)
      ANTDER(AX) = AX**3 * (THRQTR*AX - TWO)
C
C  INITIALIZE.
C
      DATA  THRQTR /0.75E0/,  ONE /1.E0/,  TWO /2.E0/,  THREE /3.E0/
      DATA  N /7/
      DATA  X /-4.E0, -2.E0, -0.9E0, 0.E0, 0.9E0, 2.E0, 4.E0/
      DATA  NPAIRS /17/
      DATA  A /-3.0E0, 3.0E0,-0.5E0,-0.5E0,-0.5E0,-4.0E0,-4.0E0, 3.0E0,
     *  -5.0E0,-5.0E0,-6.0E0, 6.0E0,-1.5E0,-1.5E0,-3.0E0, 3.0E0, 0.5E0/
      DATA  B / 3.0E0,-3.0E0, 1.0E0, 2.0E0, 5.0E0,-0.5E0, 4.0E0, 5.0E0,
     *  -3.0E0, 5.0E0,-5.0E0, 5.0E0,-0.5E0,-1.0E0,-2.5E0, 3.5E0, 0.5E0/
      DATA  IEREXP /0,0,0,0,2,0,0,2,1,3,3,3,0,0,0,0,0/
C
C  SET PASS/FAIL TOLERANCE.
C
C***FIRST EXECUTABLE STATEMENT  PCHQK2
      MACHEP = R1MACH(4)
      TOL = 100.E0*MACHEP
C
C  SET UP PCH FUNCTION DEFINITION.
C
      DO 10  I = 1, N
         F(I) =   FCN(X(I))
         D(I) = DERIV(X(I))
   10 CONTINUE
C
      IF (KPRINT .GE. 3)  WRITE (LUN, 1000)
      IF (KPRINT .GE. 2)  WRITE (LUN, 1001)
      IF (KPRINT .GE. 3)  WRITE (LUN, 1002)  (X(I), F(I), D(I), I=1,N)
C
C  LOOP OVER (A,B)-PAIRS.
C
      IF (KPRINT .GE. 3)  WRITE (LUN, 2000)
C
      IFAIL = 0
C
      SKIP = .FALSE.
      DO 20  I = 1, NPAIRS
C               ---------------------------------------------
         CALC = PCHIA (N, X, F, D, 1, SKIP, A(I), B(I), IERR)
C               ---------------------------------------------
         IF (IERR .GE. 0)  THEN
            FAIL = IERR .NE. IEREXP(I)
            TRUE = ANTDER(B(I)) - ANTDER(A(I))
            ERROR = CALC - TRUE
            IF (KPRINT .GE. 3)  THEN
               IF (FAIL)  THEN
                 WRITE (LUN, 2001) A(I), B(I), IERR, TRUE, CALC, ERROR,
     *                                          IEREXP(I)
               ELSE
                 WRITE (LUN, 2002) A(I), B(I), IERR, TRUE, CALC, ERROR
               ENDIF
            ENDIF
C
            ERROR = ABS(ERROR) / MAX(ONE, ABS(TRUE))
            IF (FAIL .OR. (ERROR.GT.TOL))  IFAIL = IFAIL + 1
            IF (I .EQ. 1)  THEN
               ERRMAX = ERROR
            ELSE
               ERRMAX = MAX(ERRMAX, ERROR)
            ENDIF
         ELSE
            IF (KPRINT .GE. 3)  WRITE (LUN, 2002)  A(I), B(I), IERR
            IFAIL = IFAIL + 1
         ENDIF
   20 CONTINUE
C
C  PRINT SUMMARY.
C
      IF (KPRINT .GE. 2)  THEN
         WRITE (LUN, 2003)  ERRMAX, TOL
         IF (IFAIL .NE. 0)  WRITE (LUN, 3001)  IFAIL
      ENDIF
C
C  TERMINATE.
C
      IF (IFAIL.EQ.0)  THEN
         IPASS = 1
         IF (KPRINT.GE.2) WRITE(LUN,99998)
      ELSE
         IPASS = 0
         IF (KPRINT.GE.1) WRITE(LUN,99999)
      ENDIF
C
      RETURN
C
C  FORMATS.
C
 1000 FORMAT ('1'//10X,'TEST PCHIP INTEGRATORS')
 1001 FORMAT (//10X,'PCHQK2 RESULTS'/10X,'--------------')
 1002 FORMAT (// 5X,'DATA:' //11X,'X',9X,'F',9X,'D' /(5X,3F10.3) )
 2000 FORMAT (// 5X,'TEST RESULTS:'
     *        //'    A     B    ERR     TRUE',16X,'CALC',15X,'ERROR')
 2001 FORMAT (2F6.1,I5,1P,2E20.10,E15.5,'  (',I1,') *****' )
 2002 FORMAT (2F6.1,I5,1P,2E20.10,E15.5)
 2003 FORMAT (/'  MAXIMUM RELATIVE ERROR IS:',1P,E15.5,
     *                       ',   TOLERANCE:',1P,E15.5)
 3001 FORMAT (/' *** TROUBLE ***',I5,' INTEGRATION TESTS FAILED.')
99998 FORMAT (/' ------------  PCHIP PASSED  ALL INTEGRATION TESTS',
     *        ' ------------')
99999 FORMAT (/' ************  PCHIP FAILED SOME INTEGRATION TESTS',
     *        ' ************')
C------------- LAST LINE OF PCHQK2 FOLLOWS -----------------------------
      END
      SUBROUTINE PCHQK3 (LUN, KPRINT, IPASS)

c*********************************************************************72
c
cc PCHQK3 tests the PCHIP interpolators PCHIC, PCHIM, PCHSP.
c
C***BEGIN PROLOGUE  PCHQK3
C***PURPOSE  Test the PCHIP interpolators PCHIC, PCHIM, PCHSP.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (PCHQK3-S, DPCHQ3-D)
C***KEYWORDS  PCHIP INTERPOLATOR QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C              PCHIP QUICK CHECK NUMBER 3
C
C     TESTS THE INTERPOLATORS:  PCHIC, PCHIM, PCHSP.
C *Usage:
C
C        INTEGER  LUN, KPRINT, IPASS
C
C        CALL PCHQK3 (LUN, KPRINT, IPASS)
C
C *Arguments:
C
C     LUN   :IN  is the unit number to which output is to be written.
C
C     KPRINT:IN  controls the amount of output, as specified in the
C                SLATEC Guidelines.
C
C     IPASS:OUT  will contain a pass/fail flag.  IPASS=1 is good.
C                IPASS=0 indicates one or more tests failed.
C
C *Description:
C
C   This routine interpolates a constructed data set with all three
C    PCHIP interpolators and compares the results with those obtained
C   on a Cray X/MP.  Two different values of the  PCHIC parameter SWITCH
C   are used.
C
C *Remarks:
C     1. The Cray results are given only to nine significant figures,
C        so don't expect them to match to more.
C     2. The results will depend to some extent on the accuracy of
C        the EXP function.
C
C***ROUTINES CALLED  COMP, PCHIC, PCHIM, PCHSP, R1MACH
C***REVISION HISTORY  (YYMMDD)
C   900309  DATE WRITTEN
C   900314  Converted to a subroutine and added a SLATEC 4.0 prologue.
C   900315  Revised prologue and improved some output formats.  (FNF)
C   900316  Made TOLD machine-dependent and added extra output when
C           KPRINT=3.  (FNF)
C   900320  Added E0's to DATA statement for X to reduce single/double
C           differences, and other minor cosmetic changes.
C   900321  Removed IFAIL from call sequence for SLATEC standards and
C           made miscellaneous cosmetic changes.  (FNF)
C   900322  Minor changes to reduce single/double differences.  (FNF)
C   900530  Tolerance (TOLD) changed.  (WRB)
C   900802  Modified TOLD formula and constants in PCHIC calls to be
C           compatible with DPCHQ3.  (FNF)
C   901130  Several significant changes:  (FNF)
C           1. Changed comparison between PCHIM and PCHIC to only
C              require agreement to machine precision.
C           2. Revised to print more output when KPRINT=3.
C           3. Added 1P's to formats.
C   910708  Minor modifications in use of KPRINT.  (WRB)
C   930317  Improved output formats.  (FNF)
C***END PROLOGUE  PCHQK3
C
C*Internal Notes:
C
C     TOLD is used to compare with stored Cray results.  Its value
C          should be consistent with significance of stored values.
C     TOLZ is used for cases in which exact equality is expected.
C     TOL  is used for cases in which agreement to machine precision
C          is expected.
C**End
C
C  Declare arguments.
C
      INTEGER  LUN, KPRINT, IPASS
      LOGICAL  COMP
      REAL  R1MACH
C
C  Declare variables.
C
      INTEGER  I, IC(2), IERR, IFAIL, N, NBAD, NBADZ, NWK
      PARAMETER  (N = 9,  NWK = 2*N)
      REAL  D(N), DC(N), DC5, DC6, DM(N), DS(N), ERR, F(N), MONE, TOL,
     *      TOLD, TOLZ, VC(2), X(N), WK(NWK), ZERO
      PARAMETER  (ZERO = 0.0E0,  MONE = -1.0E0)
      CHARACTER*6  RESULT
C
C  Initialize.
C
C       Data.
      DATA  IC /0, 0/
      DATA  X /-2.2E0,-1.2E0,-1.0E0,-0.5E0,-0.01E0, 0.5E0, 1.0E0,
     *          2.0E0, 2.2E0/
C
C       Results generated on Cray X/MP (9 sign. figs.)
      DATA  DM / 0.            , 3.80027352E-01, 7.17253009E-01,
     *           5.82014161E-01, 0.            ,-5.68208031E-01,
     *          -5.13501618E-01,-7.77910977E-02,-2.45611117E-03/
      DATA  DC5,DC6 / 1.76950158E-02,-5.69579814E-01/
      DATA  DS /-5.16830792E-02, 5.71455855E-01, 7.40530225E-01,
     *           7.63864934E-01, 1.92614386E-02,-7.65324380E-01,
     *          -7.28209035E-01,-7.98445427E-02,-2.85983446E-02/
C
C***FIRST EXECUTABLE STATEMENT  PCHQK3
      IFAIL = 0
C
C        Set tolerances.
      TOL  = 10*R1MACH(4)
      TOLD = MAX( 1.0E-7, 10*TOL )
      TOLZ = ZERO
C
      IF (KPRINT .GE. 3)  WRITE (LUN, 1000)
      IF (KPRINT .GE. 2)  WRITE (LUN, 1001)
C
C  Set up data.
C
      DO 10  I = 1, N
         F(I) = EXP(-X(I)**2)
   10 CONTINUE
C
      IF (KPRINT .GE. 3)  THEN
         WRITE (LUN, 1002)
         DO 12  I = 1, 4
            WRITE (LUN, 1010)  X(I), F(I), DM(I), DS(I)
   12    CONTINUE
            WRITE (LUN, 1011)  X(5), F(5), DM(5), DC5, DS(5)
            WRITE (LUN, 1011)  X(6), F(6), DM(6), DC6, DS(6)
         DO 15  I = 7, N
            WRITE (LUN, 1010)  X(I), F(I), DM(I), DS(I)
   15    CONTINUE
      ENDIF
C
C  Test PCHIM.
C
      IF (KPRINT.GE.3)  WRITE (LUN, 2000) 'IM'
C     --------------------------------
      CALL PCHIM (N, X, F, D, 1, IERR)
C     --------------------------------
C        Expect IERR=1 (one monotonicity switch).
      IF ( KPRINT.GE.3 )  WRITE (LUN, 2001) 1
      IF ( .NOT.COMP (IERR, 1, LUN, KPRINT) )  THEN
         IFAIL = IFAIL + 1
      ELSE
         IF ( KPRINT.GE.3 )  WRITE (LUN, 2002)
         NBAD = 0
         NBADZ = 0
         DO 20  I = 1, N
            RESULT = '  OK'
C             D-values should agree with stored values.
C               (Zero values should agree exactly.)
            IF ( DM(I).EQ.ZERO )  THEN
               ERR = ABS( D(I) )
               IF ( ERR.GT.TOLZ )  THEN
                  NBADZ = NBADZ + 1
                  RESULT = '**BADZ'
               ENDIF
            ELSE
               ERR = ABS( (D(I)-DM(I))/DM(I) )
               IF ( ERR.GT.TOLD )  THEN
                  NBAD = NBAD + 1
                  RESULT = '**BAD'
               ENDIF
            ENDIF
            IF (KPRINT.GE.3)
     *         WRITE (LUN, 2003)  I, X(I), D(I), ERR, RESULT
   20    CONTINUE
         IF ( (NBADZ.NE.0).OR.(NBAD.NE.0) )  THEN
            IFAIL = IFAIL + 1
            IF ((NBADZ.NE.0).AND.(KPRINT.GE.2))
     *         WRITE (LUN, 2004)  NBAD
            IF ((NBAD.NE.0).AND.(KPRINT.GE.2))
     *         WRITE (LUN, 2005)  NBAD, 'IM', TOLD
         ELSE
            IF (KPRINT.GE.2)  WRITE (LUN, 2006)  'IM'
         ENDIF
      ENDIF
C
C  Test PCHIC -- options set to reproduce PCHIM.
C
      IF (KPRINT.GE.3)  WRITE (LUN, 2000) 'IC'
C     --------------------------------------------------------
      CALL PCHIC (IC, VC, ZERO, N, X, F, DC, 1, WK, NWK, IERR)
C     --------------------------------------------------------
C        Expect IERR=0 .
      IF ( KPRINT.GE.3 )  WRITE (LUN, 2001) 0
      IF ( .NOT.COMP (IERR, 0, LUN, KPRINT) )  THEN
         IFAIL = IFAIL + 1
      ELSE
         IF ( KPRINT.GE.3 )  WRITE (LUN, 2002)
         NBAD = 0
         DO 30  I = 1, N
            RESULT = '  OK'
C           D-values should agree exactly with those computed by PCHIM.
C            (To be generous, will only test to machine precision.)
            ERR = ABS( D(I)-DC(I) )
            IF ( ERR.GT.TOL )  THEN
               NBAD = NBAD + 1
               RESULT = '**BAD'
            ENDIF
            IF (KPRINT.GE.3)
     *         WRITE (LUN, 2003)  I, X(I), DC(I), ERR, RESULT
   30    CONTINUE
         IF ( NBAD.NE.0 )  THEN
            IFAIL = IFAIL + 1
            IF (KPRINT.GE.2)  WRITE (LUN, 2005)  NBAD, 'IC', TOL
         ELSE
            IF (KPRINT.GE.2)  WRITE (LUN, 2006)  'IC'
         ENDIF
      ENDIF
C
C  Test PCHIC -- default nonzero switch derivatives.
C
      IF (KPRINT.GE.3)  WRITE (LUN, 2000) 'IC'
C     -------------------------------------------------------
      CALL PCHIC (IC, VC, MONE, N, X, F, D, 1, WK, NWK, IERR)
C     -------------------------------------------------------
C        Expect IERR=0 .
      IF ( KPRINT.GE.3 )  WRITE (LUN, 2001) 0
      IF ( .NOT.COMP (IERR, 0, LUN, KPRINT) )  THEN
         IFAIL = IFAIL + 1
      ELSE
         IF ( KPRINT.GE.3 )  WRITE (LUN, 2002)
         NBAD = 0
         NBADZ = 0
         DO 40  I = 1, N
            RESULT = '  OK'
C            D-values should agree exactly with those computed in
C            previous call, except at points 5 and 6.
            IF ( (I.LT.5).OR.(I.GT.6) )  THEN
               ERR = ABS( D(I)-DC(I) )
               IF ( ERR.GT.TOLZ )  THEN
                  NBADZ = NBADZ + 1
                  RESULT = '**BADA'
               ENDIF
            ELSE
               IF ( I.EQ.5 )  THEN
                  ERR = ABS( (D(I)-DC5)/DC5 )
               ELSE
                  ERR = ABS( (D(I)-DC6)/DC6 )
               ENDIF
               IF ( ERR.GT.TOLD )  THEN
                  NBAD = NBAD + 1
                  RESULT = '**BAD'
               ENDIF
            ENDIF
            IF (KPRINT.GE.3)
     *         WRITE (LUN, 2003)  I, X(I), D(I), ERR, RESULT
   40    CONTINUE
         IF ( (NBADZ.NE.0).OR.(NBAD.NE.0) )  THEN
            IFAIL = IFAIL + 1
            IF ((NBADZ.NE.0).AND.(KPRINT.GE.2))
     *         WRITE (LUN, 2007)  NBAD
            IF ((NBAD.NE.0).AND.(KPRINT.GE.2))
     *         WRITE (LUN, 2005)  NBAD, 'IC', TOLD
         ELSE
            IF (KPRINT.GE.2)  WRITE (LUN, 2006)  'IC'
         ENDIF
      ENDIF
C
C  Test PCHSP.
C
      IF (KPRINT.GE.3)  WRITE (LUN, 2000) 'SP'
C     -------------------------------------------------
      CALL PCHSP (IC, VC, N, X, F, D, 1, WK, NWK, IERR)
C     -------------------------------------------------
C        Expect IERR=0 .
      IF ( KPRINT.GE.3 )  WRITE (LUN, 2001) 0
      IF ( .NOT.COMP (IERR, 0, LUN, KPRINT) )  THEN
         IFAIL = IFAIL + 1
      ELSE
         IF ( KPRINT.GE.3 )  WRITE (LUN, 2002)
         NBAD = 0
         DO 50  I = 1, N
            RESULT = '  OK'
C             D-values should agree with stored values.
            ERR = ABS( (D(I)-DS(I))/DS(I) )
            IF ( ERR.GT.TOLD )  THEN
               NBAD = NBAD + 1
               RESULT = '**BAD'
            ENDIF
            IF (KPRINT.GE.3)
     *         WRITE (LUN, 2003)  I, X(I), D(I), ERR, RESULT
   50    CONTINUE
         IF ( NBAD.NE.0 )  THEN
            IFAIL = IFAIL + 1
            IF (KPRINT.GE.2)  WRITE (LUN, 2005)  NBAD, 'SP', TOLD
         ELSE
            IF (KPRINT.GE.2)  WRITE (LUN, 2006)  'SP'
         ENDIF
      ENDIF
C
C  PRINT SUMMARY AND TERMINATE.
C
      IF ((KPRINT.GE.2).AND.(IFAIL.NE.0))  WRITE (LUN, 3001)  IFAIL
C
      IF (IFAIL.EQ.0)  THEN
         IPASS = 1
         IF (KPRINT.GE.2) WRITE(LUN,99998)
      ELSE
         IPASS = 0
         IF (KPRINT.GE.1) WRITE(LUN,99999)
      ENDIF
C
      RETURN
C
C  FORMATS.
C
 1000 FORMAT ('1'//10X,'TEST PCHIP INTERPOLATORS')
 1001 FORMAT (//10X,'PCHQK3 RESULTS'/10X,'--------------')
 1002 FORMAT (// 5X,'DATA:'
     *         /39X,'---------- EXPECTED D-VALUES ----------'
     *         /12X,'X',9X,'F',18X,'DM',13X,'DC',13X,'DS')
 1010 FORMAT (5X,F10.2,1P,E15.5,4X,E15.5,15X,E15.5)
 1011 FORMAT (5X,F10.2,1P,E15.5,4X,3E15.5)
 2000 FORMAT (/5X,'PCH',A2,' TEST:')
 2001 FORMAT (15X,'EXPECT  IERR =',I5)
 2002 FORMAT (/9X,'I',7X,'X',9X,'D',13X,'ERR')
 2003 FORMAT (5X,I5,F10.2,1P,2E15.5,2X,A)
 2004 FORMAT (/'    **',I5,'  PCHIM RESULTS FAILED TO BE EXACTLY ZERO.')
 2005 FORMAT (/'    **',I5,'  PCH',A2,' RESULTS FAILED TOLERANCE TEST.',
     *                     '  TOL =',1P,E10.3)
 2006 FORMAT (/5X,'  ALL  PCH',A2,' RESULTS OK.')
 2007 FORMAT (/'    **',I5,'  PCHIC RESULTS FAILED TO AGREE WITH',
     *                      ' PREVIOUS CALL.')
 3001 FORMAT (/' *** TROUBLE ***',I5,' INTERPOLATION TESTS FAILED.')
99998 FORMAT (/' ------------  PCHIP PASSED  ALL INTERPOLATION TESTS',
     *        ' ------------')
99999 FORMAT (/' ************  PCHIP FAILED SOME INTERPOLATION TESTS',
     *        ' ************')
C------------- LAST LINE OF PCHQK3 FOLLOWS -----------------------------
      END
      SUBROUTINE PCHQK4 (LUN, KPRINT, IPASS)

c*********************************************************************72
c
cc PCHQK4 tests the PCHIP monotonicity checker PCHCM.
c
C***BEGIN PROLOGUE  PCHQK4
C***PURPOSE  Test the PCHIP monotonicity checker PCHCM.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (PCHQK4-S, DPCHQ4-D)
C***KEYWORDS  PCHIP MONOTONICITY CHECKER QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C              PCHIP QUICK CHECK NUMBER 4
C
C     TESTS THE MONOTONICITY CHECKER:  PCHCM.
C *Usage:
C
C        INTEGER  LUN, KPRINT, IPASS
C
C        CALL PCHQK4 (LUN, KPRINT, IPASS)
C
C *Arguments:
C
C     LUN   :IN  is the unit number to which output is to be written.
C
C     KPRINT:IN  controls the amount of output, as specified in the
C                SLATEC Guidelines.
C
C     IPASS:OUT  will contain a pass/fail flag.  IPASS=1 is good.
C                IPASS=0 indicates one or more tests failed.
C
C *Description:
C
C   This routine tests a constructed data set with three different
C   INCFD settings and compares with the expected results.  It then
C   runs a special test to check for bug in overall monotonicity found
C   in PCHMC.  Finally, it reverses the data and repeats all tests.
C
C***ROUTINES CALLED  PCHCM
C***REVISION HISTORY  (YYMMDD)
C   890208  DATE WRITTEN
C   890306  Changed LOUT to LUN and added it to call list.  (FNF)
C   890316  Removed DATA statements to suit new quick check standards.
C   890410  Changed PCHMC to PCHCM.
C   890410  Added a SLATEC 4.0 format prologue.
C   900314  Changed name from PCHQK3 to PCHQK4 and improved some output
C           formats.
C   900315  Revised prologue and improved some output formats.  (FNF)
C   900321  Removed IFAIL from call sequence for SLATEC standards and
C           made miscellaneous cosmetic changes.  (FNF)
C   900322  Added declarations so all variables are declared.  (FNF)
C   910708  Minor modifications in use of KPRINT.  (WRB)
C   930317  Improved output formats.  (FNF)
C***END PROLOGUE  PCHQK4
C
C*Internal Notes:
C
C     Data set-up is done via assignment statements to avoid modifying
C     DATA-loaded arrays, as required by the 1989 SLATEC Guidelines.
C     Run with KPRINT=3 to display the data.
C**End
C
C  Declare arguments.
C
      INTEGER  LUN, KPRINT, IPASS
C
C  DECLARE VARIABLES.
C
      INTEGER  MAXN, MAXN2, MAXN3, NB
      PARAMETER  (MAXN = 16,  MAXN2 = 8,  MAXN3 = 6,  NB = 7)
      INTEGER  I, IERR, IFAIL, INCFD, ISMEX1(MAXN), ISMEX2(MAXN2),
     *         ISMEX3(MAXN3), ISMEXB(NB), ISMON(MAXN), K, N, NS(3)
      REAL  D(MAXN), DB(NB), F(MAXN), FB(NB), X(MAXN)
      LOGICAL  SKIP
C
C  DEFINE EXPECTED RESULTS.
C
      DATA  ISMEX1 / 1, 1,-1, 1, 1,-1, 1, 1,-1, 1, 1,-1, 1, 1,-1, 2/
      DATA  ISMEX2 / 1, 2, 2, 1, 2, 2, 1, 2/
      DATA  ISMEX3 / 1, 1, 1, 1, 1, 1/
      DATA  ISMEXB / 1, 3, 1, -1, -3, -1, 2/
C
C  DEFINE TEST DATA.
C
      DATA  NS /16, 8, 6/
C
C***FIRST EXECUTABLE STATEMENT  PCHQK4
      IF (KPRINT .GE. 3)  WRITE (LUN, 1000)
      IF (KPRINT .GE. 2)  WRITE (LUN, 1001)
C
C       Define X, F, D.
      DO 1  I = 1, MAXN
         X(I) = I
         D(I) = 0.E0
    1 CONTINUE
      DO 2  I = 2, MAXN, 3
         D(I) = 2.E0
    2 CONTINUE
      DO 3  I = 1, 3
         F(I) = X(I)
         F(I+ 3) = F(I  ) + 1.E0
         F(I+ 6) = F(I+3) + 1.E0
         F(I+ 9) = F(I+6) + 1.E0
         F(I+12) = F(I+9) + 1.E0
    3 CONTINUE
      F(16) = 6.E0
C       Define FB, DB.
      FB(1) = 0.E0
      FB(2) = 2.E0
      FB(3) = 3.E0
      FB(4) = 5.E0
      DB(1) = 1.E0
      DB(2) = 3.E0
      DB(3) = 3.E0
      DB(4) = 0.E0
      DO 4  I = 1, 3
         FB(NB-I+1) =  FB(I)
         DB(NB-I+1) = -DB(I)
    4 CONTINUE
C
C  INITIALIZE.
C
      IFAIL = 0
C
      IF (KPRINT .GE. 3)  THEN
         WRITE (LUN, 1002)
         DO 10  I = 1, NB
            WRITE (LUN, 1010)  I, X(I), F(I), D(I), FB(I), DB(I)
   10    CONTINUE
         DO 20  I = NB+1, MAXN
            WRITE (LUN, 1010)  I, X(I), F(I), D(I)
   20    CONTINUE
      ENDIF
C
C  TRANSFER POINT FOR SECOND SET OF TESTS.
C
   25 CONTINUE
C
C  Loop over a series of values of INCFD.
C
      DO 30  INCFD = 1, 3
         N = NS(INCFD)
         SKIP = .FALSE.
C        -------------------------------------------------
         CALL PCHCM (N, X, F, D, INCFD, SKIP, ISMON, IERR)
C        -------------------------------------------------
         IF (KPRINT.GE.3)
     *      WRITE (LUN, 2000)  INCFD, IERR, (ISMON(I), I=1,N)
         IF ( IERR.NE.0 )  THEN
            IFAIL = IFAIL + 1
            IF (KPRINT.GE.3)  WRITE (LUN,2001)
         ELSE
            DO 29  I = 1, N
               IF (INCFD.EQ.1)  THEN
                  IF ( ISMON(I).NE.ISMEX1(I) )  THEN
                     IFAIL = IFAIL + 1
                     IF (KPRINT.GE.3)
     *                  WRITE (LUN, 2002)  (ISMEX1(K),K=1,N)
                     GO TO 30
                  ENDIF
               ELSE IF (INCFD.EQ.2) THEN
                  IF ( ISMON(I).NE.ISMEX2(I) )  THEN
                     IFAIL = IFAIL + 1
                     IF (KPRINT.GE.3)
     *                  WRITE (LUN, 2002)  (ISMEX2(K),K=1,N)
                     GO TO 30
                  ENDIF
               ELSE
                  IF ( ISMON(I).NE.ISMEX3(I) )  THEN
                     IFAIL = IFAIL + 1
                     IF (KPRINT.GE.3)
     *                  WRITE (LUN, 2002)  (ISMEX3(K),K=1,N)
                     GO TO 30
                  ENDIF
               ENDIF
   29       CONTINUE
         ENDIF
   30 CONTINUE
C
C  Test for -1,3,1 bug.
C
      SKIP = .FALSE.
C     ------------------------------------------------
      CALL PCHCM (NB, X, FB, DB, 1, SKIP, ISMON, IERR)
C     ------------------------------------------------
      IF (KPRINT.GE.3)
     *   WRITE (LUN, 2030)  IERR, (ISMON(I), I=1,NB)
      IF ( IERR.NE.0 )  THEN
         IFAIL = IFAIL + 1
         IF (KPRINT.GE.3)  WRITE (LUN,2001)
      ELSE
         DO 34  I = 1, NB
            IF ( ISMON(I).NE.ISMEXB(I) )  THEN
               IFAIL = IFAIL + 1
               IF (KPRINT.GE.3)
     *            WRITE (LUN, 2002)  (ISMEXB(K),K=1,NB)
               GO TO 35
            ENDIF
   34    CONTINUE
      ENDIF
   35 CONTINUE
C
      IF (F(1).LT.0.)  GO TO 90
C
C  Change sign and do again.
C
      IF (KPRINT.GE.3)  WRITE (LUN, 2050)
      DO 40  I = 1, MAXN
         F(I) = -F(I)
         D(I) = -D(I)
         IF ( ISMEX1(I).NE.2 )  ISMEX1(I) = -ISMEX1(I)
   40 CONTINUE
      DO 42  I = 1, MAXN2
         IF ( ISMEX2(I).NE.2 )  ISMEX2(I) = -ISMEX2(I)
   42 CONTINUE
      DO 43  I = 1, MAXN3
         IF ( ISMEX3(I).NE.2 )  ISMEX3(I) = -ISMEX3(I)
   43 CONTINUE
      DO 50  I = 1, NB
         FB(I) = -FB(I)
         DB(I) = -DB(I)
         IF ( ISMEXB(I).NE.2 )  ISMEXB(I) = -ISMEXB(I)
   50 CONTINUE
      GO TO 25
C
C  PRINT SUMMARY AND TERMINATE.
C
   90 CONTINUE
      IF ((KPRINT.GE.2).AND.(IFAIL.NE.0))  WRITE (LUN, 3001)  IFAIL
C
      IF (IFAIL.EQ.0)  THEN
         IPASS = 1
         IF (KPRINT.GE.2) WRITE(LUN,99998)
      ELSE
         IPASS = 0
         IF (KPRINT.GE.1) WRITE(LUN,99999)
      ENDIF
C
      RETURN
C
C  FORMATS.
C
 1000 FORMAT ('1'//10X,'TEST PCHIP MONOTONICITY CHECKER')
 1001 FORMAT (//10X,'PCHQK4 RESULTS'/10X,'--------------')
 1002 FORMAT (// 5X,'DATA:'
     *        // 9X,'I',4X,'X',5X,'F',5X,'D',5X,'FB',4X,'DB')
 1010 FORMAT (5X,I5,5F6.1)
 2000 FORMAT (/4X,'INCFD =',I2,':  IERR =',I3/15X,'ISMON =',16I3)
 2001 FORMAT (' *** Failed -- bad IERR value.')
 2002 FORMAT (' *** Failed -- expect:',16I3)
 2030 FORMAT (/4X,' Bug test:  IERR =',I3/15X,'ISMON =',7I3)
 2050 FORMAT (/4X,'Changing sign of data.....')
 3001 FORMAT (/' *** TROUBLE ***',I5,' MONOTONICITY TESTS FAILED.')
99998 FORMAT (/' ------------  PCHIP PASSED  ALL MONOTONICITY TESTS',
     *        ' ------------')
99999 FORMAT (/' ************  PCHIP FAILED SOME MONOTONICITY TESTS',
     *        ' ************')
C------------- LAST LINE OF PCHQK4 FOLLOWS -----------------------------
      END
      SUBROUTINE PCHQK5 (LUN, KPRINT, IPASS)

c*********************************************************************72
c
cc PCHQK5 tests the PCH to B-spline conversion routine PCHBS.
c
C***BEGIN PROLOGUE  PCHQK5
C***PURPOSE  Test the PCH to B-spline conversion routine PCHBS.
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (PCHQK5-S, DPCHQ5-D)
C***KEYWORDS  PCHIP CONVERSION ROUTINE QUICK CHECK
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C              PCHIP QUICK CHECK NUMBER 5
C
C     TESTS THE CONVERSION ROUTINE:  PCHBS.
C *Usage:
C
C        INTEGER  LUN, KPRINT, IPASS
C
C        CALL PCHQK5 (LUN, KPRINT, IPASS)
C
C *Arguments:
C
C     LUN   :IN  is the unit number to which output is to be written.
C
C     KPRINT:IN  controls the amount of output, as specified in the
C                SLATEC Guidelines.
C
C     IPASS:OUT  will contain a pass/fail flag.  IPASS=1 is good.
C                IPASS=0 indicates one or more tests failed.
C
C *Description:
C
C   This routine tests a constructed data set with four different
C   KNOTYP settings.  It computes the function and derivatives of the
C   resulting B-representation via BVALU and compares with PCH data.
C
C *Caution:
C   This routine assumes BVALU has already been successfully tested.
C
C***ROUTINES CALLED  BVALU, PCHBS, R1MACH
C***REVISION HISTORY  (YYMMDD)
C   900411  DATE WRITTEN
C   900412  Corrected minor errors in initial implementation.
C   900430  Corrected errors in prologue.
C   900501  Corrected declarations.
C   930317  Improved output formats.  (FNF)
C***END PROLOGUE  PCHQK5
C
C*Internal Notes:
C  TOL  is the tolerance to use for quantities that should only
C       theoretically be equal.
C  TOLZ is the tolerance to use for quantities that should be exactly
C       equal.
C
C**End
C
C  Declare arguments.
C
      INTEGER  LUN, KPRINT, IPASS
C
C  Declare externals.
C
      REAL  BVALU, R1MACH
      EXTERNAL  BVALU, PCHBS, R1MACH
C
C  Declare variables.
C
      INTEGER  I, IERR, IFAIL, INBV, J, KNOTYP, K, N, NDIM, NKNOTS
      PARAMETER  (N = 9)
      REAL  BCOEF(2*N), D(N), DCALC, DERR, DERMAX, F(N), FCALC, FERR,
     *      FERMAX, T(2*N+4), TERR, TERMAX, TOL, TOLZ, TSAVE(2*N+4),
     *      WORK(16*N), X(N), ZERO
      PARAMETER  (ZERO = 0.0E0)
      LOGICAL  FAIL
C
C  Define relative error function.
C
      REAL  ANS, ERR, RELERR
      RELERR (ERR, ANS) = ABS(ERR) / MAX(1.0E-5,ABS(ANS))
C
C  Define test data.
C
      DATA  X /-2.2E0,   -1.2E0,   -1.0E0,   -0.5E0,   -0.01E0,
     *          0.5E0,    1.0E0,    2.0E0,    2.2E0/
      DATA  F / 0.0079E0, 0.2369E0, 0.3679E0, 0.7788E0, 0.9999E0,
     *          0.7788E0, 0.3679E0, 0.1083E0, 0.0079E0/
      DATA  D / 0.0000E0, 0.3800E0, 0.7173E0, 0.5820E0, 0.0177E0,
     *         -0.5696E0,-0.5135E0,-0.0778E0,-0.0025E0/
C
C  Initialize.
C
C***FIRST EXECUTABLE STATEMENT  PCHQK5
      IFAIL = 0
      TOL = 100*R1MACH(4)
      TOLZ = ZERO
C
      IF (KPRINT.GE.3)  WRITE (LUN, 1000)
      IF (KPRINT.GE.2)  WRITE (LUN, 1001)
C
C  Loop over a series of values of KNOTYP.
C
      IF (KPRINT.GE.3)  WRITE (LUN, 1010)
      DO 300  KNOTYP = 2, -1, -1
C        ------------
         CALL PCHBS (N, X, F, D, 1, KNOTYP, NKNOTS, T, BCOEF, NDIM, K,
     *               IERR)
C        ------------
         IF (KPRINT.GE.3)
     *       WRITE (LUN, 2000) KNOTYP, NKNOTS, NDIM, K, IERR
         IF ( IERR.NE.0 )  THEN
            IFAIL = IFAIL + 1
            IF (KPRINT.GE.3)  WRITE (LUN, 2001)
         ELSE
C             Compare evaluated results with inputs to PCHBS.
            INBV = 1
            FERMAX = ZERO
            DERMAX = ZERO
            IF (KPRINT.GE.3)  THEN
               WRITE (LUN, 2002)
               WRITE (LUN, 2003)  T(1), T(2)
               J = 1
            ENDIF
            DO 100  I = 1, N
               FCALC = BVALU (T, BCOEF, NDIM, K, 0, X(I), INBV, WORK)
               FERR = F(I) - FCALC
               FERMAX = MAX(FERMAX, RELERR(FERR,F(I)) )
               DCALC = BVALU (T, BCOEF, NDIM, K, 1, X(I), INBV, WORK)
               DERR = D(I) - DCALC
               DERMAX = MAX(DERMAX, RELERR(DERR,D(I)) )
               IF (KPRINT.GE.3)  THEN
                  J = J + 2
                  WRITE (LUN, 2004)  X(I), T(J), T(J+1), F(I), FERR,
     *                                                   D(I), DERR
               ENDIF
  100       CONTINUE
            IF (KPRINT.GE.3)  THEN
               J = J + 2
               WRITE (LUN, 2003)  T(J), T(J+1)
            ENDIF
            FAIL = (FERMAX.GT.TOL).OR.(DERMAX.GT.TOL)
            IF (FAIL)  IFAIL = IFAIL + 1
            IF ((KPRINT.GE.3).OR.(KPRINT.GE.2).AND.FAIL)
     *         WRITE (LUN, 2005)  FERMAX, DERMAX, TOL
         ENDIF
C
C          Special check for KNOTYP=-1.
         IF (KNOTYP.EQ.0)  THEN
C             Save knot vector for next test.
            DO 200  I = 1, NKNOTS
               TSAVE(I) = T(I)
  200       CONTINUE
         ELSE IF (KNOTYP.EQ.-1)  THEN
C             Check that knot vector is unchanged.
            TERMAX = ZERO
            DO 250  I = 1, NKNOTS
               TERR = ABS(T(I) - TSAVE(I))
               TERMAX = MAX(TERMAX, TERR)
  250       CONTINUE
            IF (TERMAX.GT.TOLZ)  THEN
               IFAIL = IFAIL + 1
               IF (KPRINT.GE.2)  WRITE (LUN, 2007)  TERMAX, TOLZ
            ENDIF
         ENDIF
  300 CONTINUE
C
C  PRINT SUMMARY AND TERMINATE.
C
      IF ((KPRINT.GE.2).AND.(IFAIL.NE.0))  WRITE (LUN, 3001)  IFAIL
C
      IF (IFAIL.EQ.0)  THEN
         IPASS = 1
         IF (KPRINT.GE.2) WRITE(LUN,99998)
      ELSE
         IPASS = 0
         IF (KPRINT.GE.1) WRITE(LUN,99999)
      ENDIF
C
      RETURN
C
C  FORMATS.
C
 1000 FORMAT ('1'//10X,'TEST PCH TO B-SPLINE CONVERTER')
 1001 FORMAT (//10X,'PCHQK5 RESULTS'/10X,'--------------')
 1010 FORMAT (/4X,'(Results should be the same for all KNOTYP values.)')
 2000 FORMAT (/4X,'KNOTYP =',I2,':  NKNOTS =',I3,',  NDIM =',I3,
     *                        ',  K =',I2,',  IERR =',I3)
 2001 FORMAT (' *** Failed -- bad IERR value.')
 2002 FORMAT (/15X,'X',9X,'KNOTS',10X,'F',7X,'FERR',8X,'D',7X,'DERR')
 2003 FORMAT (18X,2F8.2)
 2004 FORMAT (10X,3F8.2,F10.4,1P,E10.2,0P,F10.4,1P,E10.2)
 2005 FORMAT (/5X,'Maximum relative errors:'
     *       /15X,'F-error =',1P,E13.5,5X,'D-error =',E13.5
     *        /5X,'Both should be less than  TOL =',E13.5)
 2007 FORMAT (/' *** T-ARRAY MAXIMUM CHANGE =',1P,E13.5,
     *           ';  SHOULD NOT EXCEED TOLZ =',E13.5)
 3001 FORMAT (/' *** TROUBLE ***',I5,' CONVERSION TESTS FAILED.')
99998 FORMAT (/' ------------  PCHIP PASSED  ALL CONVERSION TESTS',
     *        ' ------------')
99999 FORMAT (/' ************  PCHIP FAILED SOME CONVERSION TESTS',
     *        ' ************')
C------------- LAST LINE OF PCHQK5 FOLLOWS -----------------------------
      END
      SUBROUTINE PCHSP (IC, VC, N, X, F, D, INCFD, WK, NWK, IERR)

c*********************************************************************72
c
cc PCHSP: derivatives for Hermite representation of cubic spline interpolant.
c
C***BEGIN PROLOGUE  PCHSP
C***PURPOSE  Set derivatives needed to determine the Hermite represen-
C            tation of the cubic spline interpolant to given data, with
C            specified boundary conditions.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E1A
C***TYPE      SINGLE PRECISION (PCHSP-S, DPCHSP-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, PCHIP,
C             PIECEWISE CUBIC INTERPOLATION, SPLINE INTERPOLATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          PCHSP:   Piecewise Cubic Hermite Spline
C
C     Computes the Hermite representation of the cubic spline inter-
C     polant to the data given in X and F satisfying the boundary
C     conditions specified by IC and VC.
C
C     To facilitate two-dimensional applications, includes an increment
C     between successive values of the F- and D-arrays.
C
C     The resulting piecewise cubic Hermite function may be evaluated
C     by PCHFE or PCHFD.
C
C     NOTE:  This is a modified version of C. de Boor's cubic spline
C            routine CUBSPL.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  IC(2), N, NWK, IERR
C        REAL  VC(2), X(N), F(INCFD,N), D(INCFD,N), WK(NWK)
C
C        CALL  PCHSP (IC, VC, N, X, F, D, INCFD, WK, NWK, IERR)
C
C   Parameters:
C
C     IC -- (input) integer array of length 2 specifying desired
C           boundary conditions:
C           IC(1) = IBEG, desired condition at beginning of data.
C           IC(2) = IEND, desired condition at end of data.
C
C           IBEG = 0  to set D(1) so that the third derivative is con-
C              tinuous at X(2).  This is the "not a knot" condition
C              provided by de Boor's cubic spline routine CUBSPL.
C              < This is the default boundary condition. >
C           IBEG = 1  if first derivative at X(1) is given in VC(1).
C           IBEG = 2  if second derivative at X(1) is given in VC(1).
C           IBEG = 3  to use the 3-point difference formula for D(1).
C                     (Reverts to the default b.c. if N.LT.3 .)
C           IBEG = 4  to use the 4-point difference formula for D(1).
C                     (Reverts to the default b.c. if N.LT.4 .)
C          NOTES:
C           1. An error return is taken if IBEG is out of range.
C           2. For the "natural" boundary condition, use IBEG=2 and
C              VC(1)=0.
C
C           IEND may take on the same values as IBEG, but applied to
C           derivative at X(N).  In case IEND = 1 or 2, the value is
C           given in VC(2).
C
C          NOTES:
C           1. An error return is taken if IEND is out of range.
C           2. For the "natural" boundary condition, use IEND=2 and
C              VC(2)=0.
C
C     VC -- (input) real array of length 2 specifying desired boundary
C           values, as indicated above.
C           VC(1) need be set only if IC(1) = 1 or 2 .
C           VC(2) need be set only if IC(2) = 1 or 2 .
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real array of dependent variable values to be inter-
C           polated.  F(1+(I-1)*INCFD) is value corresponding to X(I).
C
C     D -- (output) real array of derivative values at the data points.
C           These values will determine the cubic spline interpolant
C           with the requested boundary conditions.
C           The value corresponding to X(I) is stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C           No other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in F and D.
C           This argument is provided primarily for 2-D applications.
C           (Error return if  INCFD.LT.1 .)
C
C     WK -- (scratch) real array of working storage.
C
C     NWK -- (input) length of work array.
C           (Error return if NWK.LT.2*N .)
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if IBEG.LT.0 or IBEG.GT.4 .
C              IERR = -5  if IEND.LT.0 of IEND.GT.4 .
C              IERR = -6  if both of the above are true.
C              IERR = -7  if NWK is too small.
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C             (The D-array has not been changed in any of these cases.)
C              IERR = -8  in case of trouble solving the linear system
C                         for the interior derivative values.
C             (The D-array may have been changed in this case.)
C             (             Do **NOT** use it!                )
C
C***REFERENCES  Carl de Boor, A Practical Guide to Splines, Springer-
C                 Verlag, New York, 1978, pp. 53-59.
C***ROUTINES CALLED  PCHDF, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820503  DATE WRITTEN
C   820804  Converted to SLATEC library version.
C   870707  Minor cosmetic changes to prologue.
C   890411  Added SAVE statements (Vers. 3.2).
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920429  Revised format and order of references.  (WRB,FNF)
C***END PROLOGUE  PCHSP
C  Programming notes:
C
C     To produce a double precision version, simply:
C        a. Change PCHSP to DPCHSP wherever it occurs,
C        b. Change the real declarations to double precision, and
C        c. Change the constants ZERO, HALF, ... to double precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  IC(2), N, INCFD, NWK, IERR
      REAL  VC(2), X(*), F(INCFD,*), D(INCFD,*), WK(2,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  IBEG, IEND, INDEX, J, NM1
      REAL  G, HALF, ONE, STEMP(3), THREE, TWO, XTEMP(4), ZERO
      SAVE ZERO, HALF, ONE, TWO, THREE
      REAL  PCHDF
C
      DATA  ZERO /0./,  HALF /0.5/,  ONE /1./,  TWO /2./,  THREE /3./
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  PCHSP
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  J = 2, N
         IF ( X(J).LE.X(J-1) )  GO TO 5003
    1 CONTINUE
C
      IBEG = IC(1)
      IEND = IC(2)
      IERR = 0
      IF ( (IBEG.LT.0).OR.(IBEG.GT.4) )  IERR = IERR - 1
      IF ( (IEND.LT.0).OR.(IEND.GT.4) )  IERR = IERR - 2
      IF ( IERR.LT.0 )  GO TO 5004
C
C  FUNCTION DEFINITION IS OK -- GO ON.
C
      IF ( NWK .LT. 2*N )  GO TO 5007
C
C  COMPUTE FIRST DIFFERENCES OF X SEQUENCE AND STORE IN WK(1,.). ALSO,
C  COMPUTE FIRST DIVIDED DIFFERENCE OF DATA AND STORE IN WK(2,.).
      DO 5  J=2,N
         WK(1,J) = X(J) - X(J-1)
         WK(2,J) = (F(1,J) - F(1,J-1))/WK(1,J)
    5 CONTINUE
C
C  SET TO DEFAULT BOUNDARY CONDITIONS IF N IS TOO SMALL.
C
      IF ( IBEG.GT.N )  IBEG = 0
      IF ( IEND.GT.N )  IEND = 0
C
C  SET UP FOR BOUNDARY CONDITIONS.
C
      IF ( (IBEG.EQ.1).OR.(IBEG.EQ.2) )  THEN
         D(1,1) = VC(1)
      ELSE IF (IBEG .GT. 2)  THEN
C        PICK UP FIRST IBEG POINTS, IN REVERSE ORDER.
         DO 10  J = 1, IBEG
            INDEX = IBEG-J+1
C           INDEX RUNS FROM IBEG DOWN TO 1.
            XTEMP(J) = X(INDEX)
            IF (J .LT. IBEG)  STEMP(J) = WK(2,INDEX)
   10    CONTINUE
C                 --------------------------------
         D(1,1) = PCHDF (IBEG, XTEMP, STEMP, IERR)
C                 --------------------------------
         IF (IERR .NE. 0)  GO TO 5009
         IBEG = 1
      ENDIF
C
      IF ( (IEND.EQ.1).OR.(IEND.EQ.2) )  THEN
         D(1,N) = VC(2)
      ELSE IF (IEND .GT. 2)  THEN
C        PICK UP LAST IEND POINTS.
         DO 15  J = 1, IEND
            INDEX = N-IEND+J
C           INDEX RUNS FROM N+1-IEND UP TO N.
            XTEMP(J) = X(INDEX)
            IF (J .LT. IEND)  STEMP(J) = WK(2,INDEX+1)
   15    CONTINUE
C                 --------------------------------
         D(1,N) = PCHDF (IEND, XTEMP, STEMP, IERR)
C                 --------------------------------
         IF (IERR .NE. 0)  GO TO 5009
         IEND = 1
      ENDIF
C
C --------------------( BEGIN CODING FROM CUBSPL )--------------------
C
C  **** A TRIDIAGONAL LINEAR SYSTEM FOR THE UNKNOWN SLOPES S(J) OF
C  F  AT X(J), J=1,...,N, IS GENERATED AND THEN SOLVED BY GAUSS ELIM-
C  INATION, WITH S(J) ENDING UP IN D(1,J), ALL J.
C     WK(1,.) AND WK(2,.) ARE USED FOR TEMPORARY STORAGE.
C
C  CONSTRUCT FIRST EQUATION FROM FIRST BOUNDARY CONDITION, OF THE FORM
C             WK(2,1)*S(1) + WK(1,1)*S(2) = D(1,1)
C
      IF (IBEG .EQ. 0)  THEN
         IF (N .EQ. 2)  THEN
C           NO CONDITION AT LEFT END AND N = 2.
            WK(2,1) = ONE
            WK(1,1) = ONE
            D(1,1) = TWO*WK(2,2)
         ELSE
C           NOT-A-KNOT CONDITION AT LEFT END AND N .GT. 2.
            WK(2,1) = WK(1,3)
            WK(1,1) = WK(1,2) + WK(1,3)
            D(1,1) =((WK(1,2) + TWO*WK(1,1))*WK(2,2)*WK(1,3)
     *                        + WK(1,2)**2*WK(2,3)) / WK(1,1)
         ENDIF
      ELSE IF (IBEG .EQ. 1)  THEN
C        SLOPE PRESCRIBED AT LEFT END.
         WK(2,1) = ONE
         WK(1,1) = ZERO
      ELSE
C        SECOND DERIVATIVE PRESCRIBED AT LEFT END.
         WK(2,1) = TWO
         WK(1,1) = ONE
         D(1,1) = THREE*WK(2,2) - HALF*WK(1,2)*D(1,1)
      ENDIF
C
C  IF THERE ARE INTERIOR KNOTS, GENERATE THE CORRESPONDING EQUATIONS AND
C  CARRY OUT THE FORWARD PASS OF GAUSS ELIMINATION, AFTER WHICH THE J-TH
C  EQUATION READS    WK(2,J)*S(J) + WK(1,J)*S(J+1) = D(1,J).
C
      NM1 = N-1
      IF (NM1 .GT. 1)  THEN
         DO 20 J=2,NM1
            IF (WK(2,J-1) .EQ. ZERO)  GO TO 5008
            G = -WK(1,J+1)/WK(2,J-1)
            D(1,J) = G*D(1,J-1)
     *                  + THREE*(WK(1,J)*WK(2,J+1) + WK(1,J+1)*WK(2,J))
            WK(2,J) = G*WK(1,J-1) + TWO*(WK(1,J) + WK(1,J+1))
   20    CONTINUE
      ENDIF
C
C  CONSTRUCT LAST EQUATION FROM SECOND BOUNDARY CONDITION, OF THE FORM
C           (-G*WK(2,N-1))*S(N-1) + WK(2,N)*S(N) = D(1,N)
C
C     IF SLOPE IS PRESCRIBED AT RIGHT END, ONE CAN GO DIRECTLY TO BACK-
C     SUBSTITUTION, SINCE ARRAYS HAPPEN TO BE SET UP JUST RIGHT FOR IT
C     AT THIS POINT.
      IF (IEND .EQ. 1)  GO TO 30
C
      IF (IEND .EQ. 0)  THEN
         IF (N.EQ.2 .AND. IBEG.EQ.0)  THEN
C           NOT-A-KNOT AT RIGHT ENDPOINT AND AT LEFT ENDPOINT AND N = 2.
            D(1,2) = WK(2,2)
            GO TO 30
         ELSE IF ((N.EQ.2) .OR. (N.EQ.3 .AND. IBEG.EQ.0))  THEN
C           EITHER (N=3 AND NOT-A-KNOT ALSO AT LEFT) OR (N=2 AND *NOT*
C           NOT-A-KNOT AT LEFT END POINT).
            D(1,N) = TWO*WK(2,N)
            WK(2,N) = ONE
            IF (WK(2,N-1) .EQ. ZERO)  GO TO 5008
            G = -ONE/WK(2,N-1)
         ELSE
C           NOT-A-KNOT AND N .GE. 3, AND EITHER N.GT.3 OR  ALSO NOT-A-
C           KNOT AT LEFT END POINT.
            G = WK(1,N-1) + WK(1,N)
C           DO NOT NEED TO CHECK FOLLOWING DENOMINATORS (X-DIFFERENCES).
            D(1,N) = ((WK(1,N)+TWO*G)*WK(2,N)*WK(1,N-1)
     *                  + WK(1,N)**2*(F(1,N-1)-F(1,N-2))/WK(1,N-1))/G
            IF (WK(2,N-1) .EQ. ZERO)  GO TO 5008
            G = -G/WK(2,N-1)
            WK(2,N) = WK(1,N-1)
         ENDIF
      ELSE
C        SECOND DERIVATIVE PRESCRIBED AT RIGHT ENDPOINT.
         D(1,N) = THREE*WK(2,N) + HALF*WK(1,N)*D(1,N)
         WK(2,N) = TWO
         IF (WK(2,N-1) .EQ. ZERO)  GO TO 5008
         G = -ONE/WK(2,N-1)
      ENDIF
C
C  COMPLETE FORWARD PASS OF GAUSS ELIMINATION.
C
      WK(2,N) = G*WK(1,N-1) + WK(2,N)
      IF (WK(2,N) .EQ. ZERO)   GO TO 5008
      D(1,N) = (G*D(1,N-1) + D(1,N))/WK(2,N)
C
C  CARRY OUT BACK SUBSTITUTION
C
   30 CONTINUE
      DO 40 J=NM1,1,-1
         IF (WK(2,J) .EQ. ZERO)  GO TO 5008
         D(1,J) = (D(1,J) - WK(1,J)*D(1,J+1))/WK(2,J)
   40 CONTINUE
C --------------------(  END  CODING FROM CUBSPL )--------------------
C
C  NORMAL RETURN.
C
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHSP',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'PCHSP', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'PCHSP', 'X-ARRAY NOT STRICTLY INCREASING'
     +   , IERR, 1)
      RETURN
C
 5004 CONTINUE
C     IC OUT OF RANGE RETURN.
      IERR = IERR - 3
      CALL XERMSG ('SLATEC', 'PCHSP', 'IC OUT OF RANGE', IERR, 1)
      RETURN
C
 5007 CONTINUE
C     NWK TOO SMALL RETURN.
      IERR = -7
      CALL XERMSG ('SLATEC', 'PCHSP', 'WORK ARRAY TOO SMALL', IERR, 1)
      RETURN
C
 5008 CONTINUE
C     SINGULAR SYSTEM.
C   *** THEORETICALLY, THIS CAN ONLY OCCUR IF SUCCESSIVE X-VALUES   ***
C   *** ARE EQUAL, WHICH SHOULD ALREADY HAVE BEEN CAUGHT (IERR=-3). ***
      IERR = -8
      CALL XERMSG ('SLATEC', 'PCHSP', 'SINGULAR LINEAR SYSTEM', IERR,
     +   1)
      RETURN
C
 5009 CONTINUE
C     ERROR RETURN FROM PCHDF.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -9
      CALL XERMSG ('SLATEC', 'PCHSP', 'ERROR RETURN FROM PCHDF', IERR,
     +   1)
      RETURN
      END
      REAL FUNCTION PCHST (ARG1, ARG2)

c*********************************************************************72
c
cc PCHST: carry out a sign test.
c
C***BEGIN PROLOGUE  PCHST
C***SUBSIDIARY
C***PURPOSE  PCHIP Sign-Testing Routine
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (PCHST-S, DPCHST-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C         PCHST:  PCHIP Sign-Testing Routine.
C
C     Returns:
C        -1. if ARG1 and ARG2 are of opposite sign.
C         0. if either argument is zero.
C        +1. if ARG1 and ARG2 are of the same sign.
C
C     The object is to do this without multiplying ARG1*ARG2, to avoid
C     possible over/underflow problems.
C
C  Fortran intrinsics used:  SIGN.
C
C***SEE ALSO  PCHCE, PCHCI, PCHCS, PCHIM
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   811103  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   870813  Minor cosmetic changes.
C   890411  Added SAVE statements (Vers. 3.2).
C   890411  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  PCHST
C
C**End
C
C  DECLARE ARGUMENTS.
C
      REAL  ARG1, ARG2
C
C  DECLARE LOCAL VARIABLES.
C
      REAL  ONE, ZERO
      SAVE ZERO, ONE
      DATA  ZERO /0./,  ONE /1./
C
C  PERFORM THE TEST.
C
C***FIRST EXECUTABLE STATEMENT  PCHST
      PCHST = SIGN(ONE,ARG1) * SIGN(ONE,ARG2)
      IF ((ARG1.EQ.ZERO) .OR. (ARG2.EQ.ZERO))  PCHST = ZERO
C
      RETURN
      END
      SUBROUTINE PCHSW (DFMAX, IEXTRM, D1, D2, H, SLOPE, IERR)

c*********************************************************************72
c
cc PCHSW limits excursion from data for PCHCS.
c
C***BEGIN PROLOGUE  PCHSW
C***SUBSIDIARY
C***PURPOSE  Limits excursion from data for PCHCS
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (PCHSW-S, DPCHSW-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C         PCHSW:  PCHCS Switch Excursion Limiter.
C
C     Called by  PCHCS  to adjust D1 and D2 if necessary to insure that
C     the extremum on this interval is not further than DFMAX from the
C     extreme data value.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        INTEGER  IEXTRM, IERR
C        REAL  DFMAX, D1, D2, H, SLOPE
C
C        CALL  PCHSW (DFMAX, IEXTRM, D1, D2, H, SLOPE, IERR)
C
C   Parameters:
C
C     DFMAX -- (input) maximum allowed difference between F(IEXTRM) and
C           the cubic determined by derivative values D1,D2.  (assumes
C           DFMAX.GT.0.)
C
C     IEXTRM -- (input) index of the extreme data value.  (assumes
C           IEXTRM = 1 or 2 .  Any value .NE.1 is treated as 2.)
C
C     D1,D2 -- (input) derivative values at the ends of the interval.
C           (Assumes D1*D2 .LE. 0.)
C          (output) may be modified if necessary to meet the restriction
C           imposed by DFMAX.
C
C     H -- (input) interval length.  (Assumes  H.GT.0.)
C
C     SLOPE -- (input) data slope on the interval.
C
C     IERR -- (output) error flag.  should be zero.
C           If IERR=-1, assumption on D1 and D2 is not satisfied.
C           If IERR=-2, quadratic equation locating extremum has
C                       negative discriminant (should never occur).
C
C    -------
C    WARNING:  This routine does no validity-checking of arguments.
C    -------
C
C  Fortran intrinsics used:  ABS, SIGN, SQRT.
C
C***SEE ALSO  PCHCS
C***ROUTINES CALLED  R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   820218  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   870707  Replaced DATA statement for SMALL with a use of R1MACH.
C   890411  1. Added SAVE statements (Vers. 3.2).
C           2. Added REAL R1MACH for consistency with D.P. version.
C   890411  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB)
C   920526  Eliminated possible divide by zero problem.  (FNF)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  PCHSW
C
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  IEXTRM, IERR
      REAL  DFMAX, D1, D2, H, SLOPE
C
C  DECLARE LOCAL VARIABLES.
C
      REAL  CP, FACT, HPHI, LAMBDA, NU, ONE, PHI, RADCAL, RHO, SIGMA,
     *      SMALL, THAT, THIRD, THREE, TWO, ZERO
      SAVE ZERO, ONE, TWO, THREE, FACT
      SAVE THIRD
      REAL R1MACH
C
      DATA  ZERO /0./,  ONE /1./,  TWO /2./,  THREE /3./, FACT /100./
C        THIRD SHOULD BE SLIGHTLY LESS THAN 1/3.
      DATA  THIRD /0.33333/
C
C  NOTATION AND GENERAL REMARKS.
C
C     RHO IS THE RATIO OF THE DATA SLOPE TO THE DERIVATIVE BEING TESTED.
C     LAMBDA IS THE RATIO OF D2 TO D1.
C     THAT = T-HAT(RHO) IS THE NORMALIZED LOCATION OF THE EXTREMUM.
C     PHI IS THE NORMALIZED VALUE OF P(X)-F1 AT X = XHAT = X-HAT(RHO),
C           WHERE  THAT = (XHAT - X1)/H .
C        THAT IS, P(XHAT)-F1 = D*H*PHI,  WHERE D=D1 OR D2.
C     SIMILARLY,  P(XHAT)-F2 = D*H*(PHI-RHO) .
C
C      SMALL SHOULD BE A FEW ORDERS OF MAGNITUDE GREATER THAN MACHEPS.
C***FIRST EXECUTABLE STATEMENT  PCHSW
      SMALL = FACT*R1MACH(4)
C
C  DO MAIN CALCULATION.
C
      IF (D1 .EQ. ZERO)  THEN
C
C        SPECIAL CASE -- D1.EQ.ZERO .
C
C          IF D2 IS ALSO ZERO, THIS ROUTINE SHOULD NOT HAVE BEEN CALLED.
         IF (D2 .EQ. ZERO)  GO TO 5001
C
         RHO = SLOPE/D2
C          EXTREMUM IS OUTSIDE INTERVAL WHEN RHO .GE. 1/3 .
         IF (RHO .GE. THIRD)  GO TO 5000
         THAT = (TWO*(THREE*RHO-ONE)) / (THREE*(TWO*RHO-ONE))
         PHI = THAT**2 * ((THREE*RHO-ONE)/THREE)
C
C          CONVERT TO DISTANCE FROM F2 IF IEXTRM.NE.1 .
         IF (IEXTRM .NE. 1)  PHI = PHI - RHO
C
C          TEST FOR EXCEEDING LIMIT, AND ADJUST ACCORDINGLY.
         HPHI = H * ABS(PHI)
         IF (HPHI*ABS(D2) .GT. DFMAX)  THEN
C           AT THIS POINT, HPHI.GT.0, SO DIVIDE IS OK.
            D2 = SIGN (DFMAX/HPHI, D2)
         ENDIF
      ELSE
C
         RHO = SLOPE/D1
         LAMBDA = -D2/D1
         IF (D2 .EQ. ZERO)  THEN
C
C           SPECIAL CASE -- D2.EQ.ZERO .
C
C             EXTREMUM IS OUTSIDE INTERVAL WHEN RHO .GE. 1/3 .
            IF (RHO .GE. THIRD)  GO TO 5000
            CP = TWO - THREE*RHO
            NU = ONE - TWO*RHO
            THAT = ONE / (THREE*NU)
         ELSE
            IF (LAMBDA .LE. ZERO)  GO TO 5001
C
C           NORMAL CASE -- D1 AND D2 BOTH NONZERO, OPPOSITE SIGNS.
C
            NU = ONE - LAMBDA - TWO*RHO
            SIGMA = ONE - RHO
            CP = NU + SIGMA
            IF (ABS(NU) .GT. SMALL)  THEN
               RADCAL = (NU - (TWO*RHO+ONE))*NU + SIGMA**2
               IF (RADCAL .LT. ZERO)  GO TO 5002
               THAT = (CP - SQRT(RADCAL)) / (THREE*NU)
            ELSE
               THAT = ONE/(TWO*SIGMA)
            ENDIF
         ENDIF
         PHI = THAT*((NU*THAT - CP)*THAT + ONE)
C
C          CONVERT TO DISTANCE FROM F2 IF IEXTRM.NE.1 .
         IF (IEXTRM .NE. 1)  PHI = PHI - RHO
C
C          TEST FOR EXCEEDING LIMIT, AND ADJUST ACCORDINGLY.
         HPHI = H * ABS(PHI)
         IF (HPHI*ABS(D1) .GT. DFMAX)  THEN
C           AT THIS POINT, HPHI.GT.0, SO DIVIDE IS OK.
            D1 = SIGN (DFMAX/HPHI, D1)
            D2 = -LAMBDA*D1
         ENDIF
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      IERR = 0
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     D1 AND D2 BOTH ZERO, OR BOTH NONZERO AND SAME SIGN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHSW', 'D1 AND/OR D2 INVALID', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     NEGATIVE VALUE OF RADICAL (SHOULD NEVER OCCUR).
      IERR = -2
      CALL XERMSG ('SLATEC', 'PCHSW', 'NEGATIVE RADICAL', IERR, 1)
      RETURN
      END
      function r1mach ( i )

c*********************************************************************72
c
cc R1MACH returns single precision real machine constants.
c
c  Discussion:
c
c    Assume that single precision real numbers are stored with a mantissa 
c    of T digits in base B, with an exponent whose value must lie 
c    between EMIN and EMAX.  Then for values of I between 1 and 5, 
c    R1MACH will return the following values:
c
c      R1MACH(1) = B^(EMIN-1), the smallest positive magnitude.
c      R1MACH(2) = B^EMAX*(1-B^(-T)), the largest magnitude.
c      R1MACH(3) = B^(-T), the smallest relative spacing.
c      R1MACH(4) = B^(1-T), the largest relative spacing.
c      R1MACH(5) = log10(B)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    25 April 2007
c
c  Author:
c
c    Original FORTRAN77 version by Phyllis Fox, Andrew Hall, Norman Schryer.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Phyllis Fox, Andrew Hall, Norman Schryer,
c    Algorithm 528,
c    Framework for a Portable Library,
c    ACM Transactions on Mathematical Software,
c    Volume 4, Number 2, June 1978, page 176-188.
c
c  Parameters:
c
c    Input, integer I, chooses the parameter to be returned.
c    1 <= I <= 5.
c
c    Output, real R1MACH, the value of the chosen parameter.
c
      implicit none

      integer i
      real r1mach

      if ( i < 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R1MACH - Fatal error!'
        write ( *, '(a)' ) '  The input argument I is out of bounds.'
        write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 5.'
        write ( *, '(a,i12)' ) '  I = ', i
        r1mach = 0.0E+00
        stop
      else if ( i == 1 ) then
        r1mach = 1.1754944E-38
      else if ( i == 2 ) then
        r1mach = 3.4028235E+38
      else if ( i == 3 ) then
        r1mach = 5.9604645E-08
      else if ( i == 4 ) then
        r1mach = 1.1920929E-07
      else if ( i == 5 ) then
        r1mach = 0.3010300E+00
      else if ( 5 < i ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R1MACH - Fatal error!'
        write ( *, '(a)' ) '  The input argument I is out of bounds.'
        write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 5.'
        write ( *, '(a,i12)' ) '  I = ', i
        r1mach = 0.0E+00
        stop
      end if

      return
      end
      FUNCTION RAND ( R )

c*********************************************************************72
c
cc RAND generates a uniformly distributed random number.
c
C***BEGIN PROLOGUE  RAND
C***PURPOSE  Generate a uniformly distributed random number.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  L6A21
C***TYPE      SINGLE PRECISION (RAND-S)
C***KEYWORDS  FNLIB, RANDOM NUMBER, SPECIAL FUNCTIONS, UNIFORM
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C      This pseudo-random number generator is portable among a wide
C variety of computers.  RAND(R) undoubtedly is not as good as many
C readily available installation dependent versions, and so this
C routine is not recommended for widespread usage.  Its redeeming
C feature is that the exact same random numbers (to within final round-
C off error) can be generated from machine to machine.  Thus, programs
C that make use of random numbers can be easily transported to and
C checked in a new environment.
C
C      The random numbers are generated by the linear congruential
C method described, e.g., by Knuth in Seminumerical Methods (p.9),
C Addison-Wesley, 1969.  Given the I-th number of a pseudo-random
C sequence, the I+1 -st number is generated from
C             X(I+1) = (A*X(I) + C) MOD M,
C where here M = 2**22 = 4194304, C = 1731 and several suitable values
C of the multiplier A are discussed below.  Both the multiplier A and
C random number X are represented in double precision as two 11-bit
C words.  The constants are chosen so that the period is the maximum
C possible, 4194304.
C
C      In order that the same numbers be generated from machine to
C machine, it is necessary that 23-bit integers be reducible modulo
C 2**11 exactly, that 23-bit integers be added exactly, and that 11-bit
C integers be multiplied exactly.  Furthermore, if the restart option
C is used (where R is between 0 and 1), then the product R*2**22 =
C R*4194304 must be correct to the nearest integer.
C
C      The first four random numbers should be .0004127026,
C .6750836372, .1614754200, and .9086198807.  The tenth random number
C is .5527787209, and the hundredth is .3600893021 .  The thousandth
C number should be .2176990509 .
C
C      In order to generate several effectively independent sequences
C with the same generator, it is necessary to know the random number
C for several widely spaced calls.  The I-th random number times 2**22,
C where I=K*P/8 and P is the period of the sequence (P = 2**22), is
C still of the form L*P/8.  In particular we find the I-th random
C number multiplied by 2**22 is given by
C I   =  0  1*P/8  2*P/8  3*P/8  4*P/8  5*P/8  6*P/8  7*P/8  8*P/8
C RAND=  0  5*P/8  2*P/8  7*P/8  4*P/8  1*P/8  6*P/8  3*P/8  0
C Thus the 4*P/8 = 2097152 random number is 2097152/2**22.
C
C      Several multipliers have been subjected to the spectral test
C (see Knuth, p. 82).  Four suitable multipliers roughly in order of
C goodness according to the spectral test are
C    3146757 = 1536*2048 + 1029 = 2**21 + 2**20 + 2**10 + 5
C    2098181 = 1024*2048 + 1029 = 2**21 + 2**10 + 5
C    3146245 = 1536*2048 +  517 = 2**21 + 2**20 + 2**9 + 5
C    2776669 = 1355*2048 + 1629 = 5**9 + 7**7 + 1
C
C      In the table below LOG10(NU(I)) gives roughly the number of
C random decimal digits in the random numbers considered I at a time.
C C is the primary measure of goodness.  In both cases bigger is better.
C
C                   LOG10 NU(I)              C(I)
C       A       I=2  I=3  I=4  I=5    I=2  I=3  I=4  I=5
C
C    3146757    3.3  2.0  1.6  1.3    3.1  1.3  4.6  2.6
C    2098181    3.3  2.0  1.6  1.2    3.2  1.3  4.6  1.7
C    3146245    3.3  2.2  1.5  1.1    3.2  4.2  1.1  0.4
C    2776669    3.3  2.1  1.6  1.3    2.5  2.0  1.9  2.6
C   Best
C    Possible   3.3  2.3  1.7  1.4    3.6  5.9  9.7  14.9
C
C             Input Argument --
C R      If R=0., the next random number of the sequence is generated.
C        If R .LT. 0., the last generated number will be returned for
C          possible use in a restart procedure.
C        If R .GT. 0., the sequence of random numbers will start with
C          the seed R mod 1.  This seed is also returned as the value of
C          RAND provided the arithmetic is done exactly.
C
C             Output Value --
C RAND   a pseudo-random number between 0. and 1.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C***END PROLOGUE  RAND
      SAVE IA1, IA0, IA1MA0, IC, IX1, IX0
      DATA IA1, IA0, IA1MA0 /1536, 1029, 507/
      DATA IC /1731/
      DATA IX1, IX0 /0, 0/
C***FIRST EXECUTABLE STATEMENT  RAND
      IF (R.LT.0.) GO TO 10
      IF (R.GT.0.) GO TO 20
C
C           A*X = 2**22*IA1*IX1 + 2**11*(IA1*IX1 + (IA1-IA0)*(IX0-IX1)
C                   + IA0*IX0) + IA0*IX0
C
      IY0 = IA0*IX0
      IY1 = IA1*IX1 + IA1MA0*(IX0-IX1) + IY0
      IY0 = IY0 + IC
      IX0 = MOD (IY0, 2048)
      IY1 = IY1 + (IY0-IX0)/2048
      IX1 = MOD (IY1, 2048)
C
 10   RAND = IX1*2048 + IX0
      RAND = RAND / 4194304.
      RETURN
C
 20   IX1 = MOD(R,1.)*4194304. + 0.5
      IX0 = MOD (IX1, 2048)
      IX1 = (IX1-IX0)/2048
      GO TO 10
C
      END
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 June 2014
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
     &  d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, 
     &  trim ( ampm )

      return
      end
      SUBROUTINE XERCNT (LIBRAR, SUBROU, MESSG, NERR, LEVEL, KONTRL)

c*********************************************************************72
c
cc XERCNT allows user control over handling of errors.
c
C***BEGIN PROLOGUE  XERCNT
C***SUBSIDIARY
C***PURPOSE  Allow user control over handling of errors.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XERCNT-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        Allows user control over handling of individual errors.
C        Just after each message is recorded, but before it is
C        processed any further (i.e., before it is printed or
C        a decision to abort is made), a call is made to XERCNT.
C        If the user has provided his own version of XERCNT, he
C        can then override the value of KONTROL used in processing
C        this message by redefining its value.
C        KONTRL may be set to any value from -2 to 2.
C        The meanings for KONTRL are the same as in XSETF, except
C        that the value of KONTRL changes only for this message.
C        If KONTRL is set to a value outside the range from -2 to 2,
C        it will be moved back into that range.
C
C     Description of Parameters
C
C      --Input--
C        LIBRAR - the library that the routine is in.
C        SUBROU - the subroutine that XERMSG is being called from
C        MESSG  - the first 20 characters of the error message.
C        NERR   - same as in the call to XERMSG.
C        LEVEL  - same as in the call to XERMSG.
C        KONTRL - the current value of the control flag as set
C                 by a call to XSETF.
C
C      --Output--
C        KONTRL - the new value of KONTRL.  If KONTRL is not
C                 defined, it will remain at its original value.
C                 This changed value of control affects only
C                 the current occurrence of the current message.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900206  Routine changed from user-callable to subsidiary.  (WRB)
C   900510  Changed calling sequence to include LIBRARY and SUBROUTINE
C           names, changed routine name from XERCTL to XERCNT.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERCNT
      CHARACTER*(*) LIBRAR, SUBROU, MESSG
C***FIRST EXECUTABLE STATEMENT  XERCNT
      RETURN
      END
      SUBROUTINE XERDMP ( )

c*********************************************************************72
c
cc XERDMP prints the error tables and clears them.
c
C***BEGIN PROLOGUE  XERDMP
C***PURPOSE  Print the error tables and then clear them.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XERDMP-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        XERDMP prints the error tables, then clears them.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  XERSVE
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900510  Changed call of XERSAV to XERSVE.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERDMP
C***FIRST EXECUTABLE STATEMENT  XERDMP
      CALL XERSVE (' ',' ',' ',0,0,0,KOUNT)
      RETURN
      END
      SUBROUTINE XERHLT (MESSG)

c*********************************************************************72
c
cc XERHLT halts program execution after a fatal error.
c
C***BEGIN PROLOGUE  XERHLT
C***SUBSIDIARY
C***PURPOSE  Abort program execution and print error message.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XERHLT-A)
C***KEYWORDS  ABORT PROGRAM EXECUTION, ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        ***Note*** machine dependent routine
C        XERHLT aborts the execution of the program.
C        The error message causing the abort is given in the calling
C        sequence, in case one needs it for printing on a dayfile,
C        for example.
C
C     Description of Parameters
C        MESSG is as in XERMSG.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900206  Routine changed from user-callable to subsidiary.  (WRB)
C   900510  Changed calling sequence to delete length of character
C           and changed routine name from XERABT to XERHLT.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERHLT
      CHARACTER*(*) MESSG
C***FIRST EXECUTABLE STATEMENT  XERHLT
      STOP
      END
      SUBROUTINE XERMSG (LIBRAR, SUBROU, MESSG, NERR, LEVEL)

c*********************************************************************72
c
cc XERMSG processes error messages.
c
C***BEGIN PROLOGUE  XERMSG
C***PURPOSE  Process error messages for SLATEC and other libraries.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XERMSG-A)
C***KEYWORDS  ERROR MESSAGE, XERROR
C***AUTHOR  Fong, Kirby, (NMFECC at LLNL)
C***DESCRIPTION
C
C   XERMSG processes a diagnostic message in a manner determined by the
C   value of LEVEL and the current value of the library error control
C   flag, KONTRL.  See subroutine XSETF for details.
C
C    LIBRAR   A character constant (or character variable) with the name
C             of the library.  This will be 'SLATEC' for the SLATEC
C             Common Math Library.  The error handling package is
C             general enough to be used by many libraries
C             simultaneously, so it is desirable for the routine that
C             detects and reports an error to identify the library name
C             as well as the routine name.
C
C    SUBROU   A character constant (or character variable) with the name
C             of the routine that detected the error.  Usually it is the
C             name of the routine that is calling XERMSG.  There are
C             some instances where a user callable library routine calls
C             lower level subsidiary routines where the error is
C             detected.  In such cases it may be more informative to
C             supply the name of the routine the user called rather than
C             the name of the subsidiary routine that detected the
C             error.
C
C    MESSG    A character constant (or character variable) with the text
C             of the error or warning message.  In the example below,
C             the message is a character constant that contains a
C             generic message.
C
C                   CALL XERMSG ('SLATEC', 'MMPY',
C                  *'THE ORDER OF THE MATRIX EXCEEDS THE ROW DIMENSION',
C                  *3, 1)
C
C             It is possible (and is sometimes desirable) to generate a
C             specific message--e.g., one that contains actual numeric
C             values.  Specific numeric values can be converted into
C             character strings using formatted WRITE statements into
C             character variables.  This is called standard Fortran
C             internal file I/O and is exemplified in the first three
C             lines of the following example.  You can also catenate
C             substrings of characters to construct the error message.
C             Here is an example showing the use of both writing to
C             an internal file and catenating character strings.
C
C                   CHARACTER*5 CHARN, CHARL
C                   WRITE (CHARN,10) N
C                   WRITE (CHARL,10) LDA
C                10 FORMAT(I5)
C                   CALL XERMSG ('SLATEC', 'MMPY', 'THE ORDER'//CHARN//
C                  *   ' OF THE MATRIX EXCEEDS ITS ROW DIMENSION OF'//
C                  *   CHARL, 3, 1)
C
C             There are two subtleties worth mentioning.  One is that
C             the // for character catenation is used to construct the
C             error message so that no single character constant is
C             continued to the next line.  This avoids confusion as to
C             whether there are trailing blanks at the end of the line.
C             The second is that by catenating the parts of the message
C             as an actual argument rather than encoding the entire
C             message into one large character variable, we avoid
C             having to know how long the message will be in order to
C             declare an adequate length for that large character
C             variable.  XERMSG calls XERPRN to print the message using
C             multiple lines if necessary.  If the message is very long,
C             XERPRN will break it into pieces of 72 characters (as
C             requested by XERMSG) for printing on multiple lines.
C             Also, XERMSG asks XERPRN to prefix each line with ' *  '
C             so that the total line length could be 76 characters.
C             Note also that XERPRN scans the error message backwards
C             to ignore trailing blanks.  Another feature is that
C             the substring '$$' is treated as a new line sentinel
C             by XERPRN.  If you want to construct a multiline
C             message without having to count out multiples of 72
C             characters, just use '$$' as a separator.  '$$'
C             obviously must occur within 72 characters of the
C             start of each line to have its intended effect since
C             XERPRN is asked to wrap around at 72 characters in
C             addition to looking for '$$'.
C
C    NERR     An integer value that is chosen by the library routine's
C             author.  It must be in the range -99 to 999 (three
C             printable digits).  Each distinct error should have its
C             own error number.  These error numbers should be described
C             in the machine readable documentation for the routine.
C             The error numbers need be unique only within each routine,
C             so it is reasonable for each routine to start enumerating
C             errors from 1 and proceeding to the next integer.
C
C    LEVEL    An integer value in the range 0 to 2 that indicates the
C             level (severity) of the error.  Their meanings are
C
C            -1  A warning message.  This is used if it is not clear
C                that there really is an error, but the user's attention
C                may be needed.  An attempt is made to only print this
C                message once.
C
C             0  A warning message.  This is used if it is not clear
C                that there really is an error, but the user's attention
C                may be needed.
C
C             1  A recoverable error.  This is used even if the error is
C                so serious that the routine cannot return any useful
C                answer.  If the user has told the error package to
C                return after recoverable errors, then XERMSG will
C                return to the Library routine which can then return to
C                the user's routine.  The user may also permit the error
C                package to terminate the program upon encountering a
C                recoverable error.
C
C             2  A fatal error.  XERMSG will not return to its caller
C                after it receives a fatal error.  This level should
C                hardly ever be used; it is much better to allow the
C                user a chance to recover.  An example of one of the few
C                cases in which it is permissible to declare a level 2
C                error is a reverse communication Library routine that
C                is likely to be called repeatedly until it integrates
C                across some interval.  If there is a serious error in
C                the input such that another step cannot be taken and
C                the Library routine is called again without the input
C                error having been corrected by the caller, the Library
C                routine will probably be called forever with improper
C                input.  In this case, it is reasonable to declare the
C                error to be fatal.
C
C    Each of the arguments to XERMSG is input; none will be modified by
C    XERMSG.  A routine may make multiple calls to XERMSG with warning
C    level messages; however, after a call to XERMSG with a recoverable
C    error, the routine should return to the user.  Do not try to call
C    XERMSG with a second recoverable error after the first recoverable
C    error because the error package saves the error number.  The user
C    can retrieve this error number by calling another entry point in
C    the error handling package and then clear the error number when
C    recovering from the error.  Calling XERMSG in succession causes the
C    old error number to be overwritten by the latest error number.
C    This is considered harmless for error numbers associated with
C    warning messages but must not be done for error numbers of serious
C    errors.  After a call to XERMSG with a recoverable error, the user
C    must be given a chance to call NUMXER or XERCLR to retrieve or
C    clear the error number.
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  FDUMP, J4SAVE, XERCNT, XERHLT, XERPRN, XERSVE
C***REVISION HISTORY  (YYMMDD)
C   880101  DATE WRITTEN
C   880621  REVISED AS DIRECTED AT SLATEC CML MEETING OF FEBRUARY 1988.
C           THERE ARE TWO BASIC CHANGES.
C           1.  A NEW ROUTINE, XERPRN, IS USED INSTEAD OF XERPRT TO
C               PRINT MESSAGES.  THIS ROUTINE WILL BREAK LONG MESSAGES
C               INTO PIECES FOR PRINTING ON MULTIPLE LINES.  '$$' IS
C               ACCEPTED AS A NEW LINE SENTINEL.  A PREFIX CAN BE
C               ADDED TO EACH LINE TO BE PRINTED.  XERMSG USES EITHER
C               ' ***' OR ' *  ' AND LONG MESSAGES ARE BROKEN EVERY
C               72 CHARACTERS (AT MOST) SO THAT THE MAXIMUM LINE
C               LENGTH OUTPUT CAN NOW BE AS GREAT AS 76.
C           2.  THE TEXT OF ALL MESSAGES IS NOW IN UPPER CASE SINCE THE
C               FORTRAN STANDARD DOCUMENT DOES NOT ADMIT THE EXISTENCE
C               OF LOWER CASE.
C   880708  REVISED AFTER THE SLATEC CML MEETING OF JUNE 29 AND 30.
C           THE PRINCIPAL CHANGES ARE
C           1.  CLARIFY COMMENTS IN THE PROLOGUES
C           2.  RENAME XRPRNT TO XERPRN
C           3.  REWORK HANDLING OF '$$' IN XERPRN TO HANDLE BLANK LINES
C               SIMILAR TO THE WAY FORMAT STATEMENTS HANDLE THE /
C               CHARACTER FOR NEW RECORDS.
C   890706  REVISED WITH THE HELP OF FRED FRITSCH AND REG CLEMENS TO
C           CLEAN UP THE CODING.
C   890721  REVISED TO USE NEW FEATURE IN XERPRN TO COUNT CHARACTERS IN
C           PREFIX.
C   891013  REVISED TO CORRECT COMMENTS.
C   891214  Prologue converted to Version 4.0 format.  (WRB)
C   900510  Changed test on NERR to be -9999999 < NERR < 99999999, but
C           NERR .ne. 0, and on LEVEL to be -2 < LEVEL < 3.  Added
C           LEVEL=-1 logic, changed calls to XERSAV to XERSVE, and
C           XERCTL to XERCNT.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERMSG
      CHARACTER*(*) LIBRAR, SUBROU, MESSG
      CHARACTER*8 XLIBR, XSUBR
      CHARACTER*72  TEMP
      CHARACTER*20  LFIRST
C***FIRST EXECUTABLE STATEMENT  XERMSG
      LKNTRL = J4SAVE (2, 0, .FALSE.)
      MAXMES = J4SAVE (4, 0, .FALSE.)
C
C       LKNTRL IS A LOCAL COPY OF THE CONTROL FLAG KONTRL.
C       MAXMES IS THE MAXIMUM NUMBER OF TIMES ANY PARTICULAR MESSAGE
C          SHOULD BE PRINTED.
C
C       WE PRINT A FATAL ERROR MESSAGE AND TERMINATE FOR AN ERROR IN
C          CALLING XERMSG.  THE ERROR NUMBER SHOULD BE POSITIVE,
C          AND THE LEVEL SHOULD BE BETWEEN 0 AND 2.
C
      IF (NERR.LT.-9999999 .OR. NERR.GT.99999999 .OR. NERR.EQ.0 .OR.
     *   LEVEL.LT.-1 .OR. LEVEL.GT.2) THEN
         CALL XERPRN (' ***', -1, 'FATAL ERROR IN...$$ ' //
     *      'XERMSG -- INVALID ERROR NUMBER OR LEVEL$$ '//
     *      'JOB ABORT DUE TO FATAL ERROR.', 72)
         CALL XERSVE (' ', ' ', ' ', 0, 0, 0, KDUMMY)
         CALL XERHLT (' ***XERMSG -- INVALID INPUT')
         RETURN
      ENDIF
C
C       RECORD THE MESSAGE.
C
      I = J4SAVE (1, NERR, .TRUE.)
      CALL XERSVE (LIBRAR, SUBROU, MESSG, 1, NERR, LEVEL, KOUNT)
C
C       HANDLE PRINT-ONCE WARNING MESSAGES.
C
      IF (LEVEL.EQ.-1 .AND. KOUNT.GT.1) RETURN
C
C       ALLOW TEMPORARY USER OVERRIDE OF THE CONTROL FLAG.
C
      XLIBR  = LIBRAR
      XSUBR  = SUBROU
      LFIRST = MESSG
      LERR   = NERR
      LLEVEL = LEVEL
      CALL XERCNT (XLIBR, XSUBR, LFIRST, LERR, LLEVEL, LKNTRL)
C
      LKNTRL = MAX(-2, MIN(2,LKNTRL))
      MKNTRL = ABS(LKNTRL)
C
C       SKIP PRINTING IF THE CONTROL FLAG VALUE AS RESET IN XERCNT IS
C       ZERO AND THE ERROR IS NOT FATAL.
C
      IF (LEVEL.LT.2 .AND. LKNTRL.EQ.0) GO TO 30
      IF (LEVEL.EQ.0 .AND. KOUNT.GT.MAXMES) GO TO 30
      IF (LEVEL.EQ.1 .AND. KOUNT.GT.MAXMES .AND. MKNTRL.EQ.1) GO TO 30
      IF (LEVEL.EQ.2 .AND. KOUNT.GT.MAX(1,MAXMES)) GO TO 30
C
C       ANNOUNCE THE NAMES OF THE LIBRARY AND SUBROUTINE BY BUILDING A
C       MESSAGE IN CHARACTER VARIABLE TEMP (NOT EXCEEDING 66 CHARACTERS)
C       AND SENDING IT OUT VIA XERPRN.  PRINT ONLY IF CONTROL FLAG
C       IS NOT ZERO.
C
      IF (LKNTRL .NE. 0) THEN
         TEMP(1:21) = 'MESSAGE FROM ROUTINE '
         I = MIN(LEN(SUBROU), 16)
         TEMP(22:21+I) = SUBROU(1:I)
         TEMP(22+I:33+I) = ' IN LIBRARY '
         LTEMP = 33 + I
         I = MIN(LEN(LIBRAR), 16)
         TEMP(LTEMP+1:LTEMP+I) = LIBRAR (1:I)
         TEMP(LTEMP+I+1:LTEMP+I+1) = '.'
         LTEMP = LTEMP + I + 1
         CALL XERPRN (' ***', -1, TEMP(1:LTEMP), 72)
      ENDIF
C
C       IF LKNTRL IS POSITIVE, PRINT AN INTRODUCTORY LINE BEFORE
C       PRINTING THE MESSAGE.  THE INTRODUCTORY LINE TELLS THE CHOICE
C       FROM EACH OF THE FOLLOWING THREE OPTIONS.
C       1.  LEVEL OF THE MESSAGE
C              'INFORMATIVE MESSAGE'
C              'POTENTIALLY RECOVERABLE ERROR'
C              'FATAL ERROR'
C       2.  WHETHER CONTROL FLAG WILL ALLOW PROGRAM TO CONTINUE
C              'PROG CONTINUES'
C              'PROG ABORTED'
C       3.  WHETHER OR NOT A TRACEBACK WAS REQUESTED.  (THE TRACEBACK
C           MAY NOT BE IMPLEMENTED AT SOME SITES, SO THIS ONLY TELLS
C           WHAT WAS REQUESTED, NOT WHAT WAS DELIVERED.)
C              'TRACEBACK REQUESTED'
C              'TRACEBACK NOT REQUESTED'
C       NOTICE THAT THE LINE INCLUDING FOUR PREFIX CHARACTERS WILL NOT
C       EXCEED 74 CHARACTERS.
C       WE SKIP THE NEXT BLOCK IF THE INTRODUCTORY LINE IS NOT NEEDED.
C
      IF (LKNTRL .GT. 0) THEN
C
C       THE FIRST PART OF THE MESSAGE TELLS ABOUT THE LEVEL.
C
         IF (LEVEL .LE. 0) THEN
            TEMP(1:20) = 'INFORMATIVE MESSAGE,'
            LTEMP = 20
         ELSEIF (LEVEL .EQ. 1) THEN
            TEMP(1:30) = 'POTENTIALLY RECOVERABLE ERROR,'
            LTEMP = 30
         ELSE
            TEMP(1:12) = 'FATAL ERROR,'
            LTEMP = 12
         ENDIF
C
C       THEN WHETHER THE PROGRAM WILL CONTINUE.
C
         IF ((MKNTRL.EQ.2 .AND. LEVEL.GE.1) .OR.
     *       (MKNTRL.EQ.1 .AND. LEVEL.EQ.2)) THEN
            TEMP(LTEMP+1:LTEMP+14) = ' PROG ABORTED,'
            LTEMP = LTEMP + 14
         ELSE
            TEMP(LTEMP+1:LTEMP+16) = ' PROG CONTINUES,'
            LTEMP = LTEMP + 16
         ENDIF
C
C       FINALLY TELL WHETHER THERE SHOULD BE A TRACEBACK.
C
         IF (LKNTRL .GT. 0) THEN
            TEMP(LTEMP+1:LTEMP+20) = ' TRACEBACK REQUESTED'
            LTEMP = LTEMP + 20
         ELSE
            TEMP(LTEMP+1:LTEMP+24) = ' TRACEBACK NOT REQUESTED'
            LTEMP = LTEMP + 24
         ENDIF
         CALL XERPRN (' ***', -1, TEMP(1:LTEMP), 72)
      ENDIF
C
C       NOW SEND OUT THE MESSAGE.
C
      CALL XERPRN (' *  ', -1, MESSG, 72)
C
C       IF LKNTRL IS POSITIVE, WRITE THE ERROR NUMBER AND REQUEST A
C          TRACEBACK.
C
      IF (LKNTRL .GT. 0) THEN
         WRITE (TEMP, '(''ERROR NUMBER = '', I8)') NERR
         DO 10 I=16,22
            IF (TEMP(I:I) .NE. ' ') GO TO 20
   10    CONTINUE
C
   20    CALL XERPRN (' *  ', -1, TEMP(1:15) // TEMP(I:23), 72)
         CALL FDUMP
      ENDIF
C
C       IF LKNTRL IS NOT ZERO, PRINT A BLANK LINE AND AN END OF MESSAGE.
C
      IF (LKNTRL .NE. 0) THEN
         CALL XERPRN (' *  ', -1, ' ', 72)
         CALL XERPRN (' ***', -1, 'END OF MESSAGE', 72)
         CALL XERPRN ('    ',  0, ' ', 72)
      ENDIF
C
C       IF THE ERROR IS NOT FATAL OR THE ERROR IS RECOVERABLE AND THE
C       CONTROL FLAG IS SET FOR RECOVERY, THEN RETURN.
C
   30 IF (LEVEL.LE.0 .OR. (LEVEL.EQ.1 .AND. MKNTRL.LE.1)) RETURN
C
C       THE PROGRAM WILL BE STOPPED DUE TO AN UNRECOVERED ERROR OR A
C       FATAL ERROR.  PRINT THE REASON FOR THE ABORT AND THE ERROR
C       SUMMARY IF THE CONTROL FLAG AND THE MAXIMUM ERROR COUNT PERMIT.
C
      IF (LKNTRL.GT.0 .AND. KOUNT.LT.MAX(1,MAXMES)) THEN
         IF (LEVEL .EQ. 1) THEN
            CALL XERPRN
     *         (' ***', -1, 'JOB ABORT DUE TO UNRECOVERED ERROR.', 72)
         ELSE
            CALL XERPRN(' ***', -1, 'JOB ABORT DUE TO FATAL ERROR.', 72)
         ENDIF
         CALL XERSVE (' ', ' ', ' ', -1, 0, 0, KDUMMY)
         CALL XERHLT (' ')
      ELSE
         CALL XERHLT (MESSG)
      ENDIF
      RETURN
      END
      SUBROUTINE XERPRN (PREFIX, NPREF, MESSG, NWRAP)

c*********************************************************************72
c
cc XERPRN prints error messages from XERMSG.
c
C***BEGIN PROLOGUE  XERPRN
C***SUBSIDIARY
C***PURPOSE  Print error messages processed by XERMSG.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XERPRN-A)
C***KEYWORDS  ERROR MESSAGES, PRINTING, XERROR
C***AUTHOR  Fong, Kirby, (NMFECC at LLNL)
C***DESCRIPTION
C
C This routine sends one or more lines to each of the (up to five)
C logical units to which error messages are to be sent.  This routine
C is called several times by XERMSG, sometimes with a single line to
C print and sometimes with a (potentially very long) message that may
C wrap around into multiple lines.
C
C PREFIX  Input argument of type CHARACTER.  This argument contains
C         characters to be put at the beginning of each line before
C         the body of the message.  No more than 16 characters of
C         PREFIX will be used.
C
C NPREF   Input argument of type INTEGER.  This argument is the number
C         of characters to use from PREFIX.  If it is negative, the
C         intrinsic function LEN is used to determine its length.  If
C         it is zero, PREFIX is not used.  If it exceeds 16 or if
C         LEN(PREFIX) exceeds 16, only the first 16 characters will be
C         used.  If NPREF is positive and the length of PREFIX is less
C         than NPREF, a copy of PREFIX extended with blanks to length
C         NPREF will be used.
C
C MESSG   Input argument of type CHARACTER.  This is the text of a
C         message to be printed.  If it is a long message, it will be
C         broken into pieces for printing on multiple lines.  Each line
C         will start with the appropriate prefix and be followed by a
C         piece of the message.  NWRAP is the number of characters per
C         piece; that is, after each NWRAP characters, we break and
C         start a new line.  In addition the characters '$$' embedded
C         in MESSG are a sentinel for a new line.  The counting of
C         characters up to NWRAP starts over for each new line.  The
C         value of NWRAP typically used by XERMSG is 72 since many
C         older error messages in the SLATEC Library are laid out to
C         rely on wrap-around every 72 characters.
C
C NWRAP   Input argument of type INTEGER.  This gives the maximum size
C         piece into which to break MESSG for printing on multiple
C         lines.  An embedded '$$' ends a line, and the count restarts
C         at the following character.  If a line break does not occur
C         on a blank (it would split a word) that word is moved to the
C         next line.  Values of NWRAP less than 16 will be treated as
C         16.  Values of NWRAP greater than 132 will be treated as 132.
C         The actual line length will be NPREF + NWRAP after NPREF has
C         been adjusted to fall between 0 and 16 and NWRAP has been
C         adjusted to fall between 16 and 132.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  I1MACH, XGETUA
C***REVISION HISTORY  (YYMMDD)
C   880621  DATE WRITTEN
C   880708  REVISED AFTER THE SLATEC CML SUBCOMMITTEE MEETING OF
C           JUNE 29 AND 30 TO CHANGE THE NAME TO XERPRN AND TO REWORK
C           THE HANDLING OF THE NEW LINE SENTINEL TO BEHAVE LIKE THE
C           SLASH CHARACTER IN FORMAT STATEMENTS.
C   890706  REVISED WITH THE HELP OF FRED FRITSCH AND REG CLEMENS TO
C           STREAMLINE THE CODING AND FIX A BUG THAT CAUSED EXTRA BLANK
C           LINES TO BE PRINTED.
C   890721  REVISED TO ADD A NEW FEATURE.  A NEGATIVE VALUE OF NPREF
C           CAUSES LEN(PREFIX) TO BE USED AS THE LENGTH.
C   891013  REVISED TO CORRECT ERROR IN CALCULATING PREFIX LENGTH.
C   891214  Prologue converted to Version 4.0 format.  (WRB)
C   900510  Added code to break messages between words.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERPRN
      CHARACTER*(*) PREFIX, MESSG
      INTEGER NPREF, NWRAP
      CHARACTER*148 CBUFF
      INTEGER IU(5), NUNIT
      CHARACTER*2 NEWLIN
      PARAMETER (NEWLIN = '$$')
C***FIRST EXECUTABLE STATEMENT  XERPRN
      CALL XGETUA(IU,NUNIT)
C
C       A ZERO VALUE FOR A LOGICAL UNIT NUMBER MEANS TO USE THE STANDARD
C       ERROR MESSAGE UNIT INSTEAD.  I1MACH(4) RETRIEVES THE STANDARD
C       ERROR MESSAGE UNIT.
C
      N = I1MACH(4)
      DO 10 I=1,NUNIT
         IF (IU(I) .EQ. 0) IU(I) = N
   10 CONTINUE
C
C       LPREF IS THE LENGTH OF THE PREFIX.  THE PREFIX IS PLACED AT THE
C       BEGINNING OF CBUFF, THE CHARACTER BUFFER, AND KEPT THERE DURING
C       THE REST OF THIS ROUTINE.
C
      IF ( NPREF .LT. 0 ) THEN
         LPREF = LEN(PREFIX)
      ELSE
         LPREF = NPREF
      ENDIF
      LPREF = MIN(16, LPREF)
      IF (LPREF .NE. 0) CBUFF(1:LPREF) = PREFIX
C
C       LWRAP IS THE MAXIMUM NUMBER OF CHARACTERS WE WANT TO TAKE AT ONE
C       TIME FROM MESSG TO PRINT ON ONE LINE.
C
      LWRAP = MAX(16, MIN(132, NWRAP))
C
C       SET LENMSG TO THE LENGTH OF MESSG, IGNORE ANY TRAILING BLANKS.
C
      LENMSG = LEN(MESSG)
      N = LENMSG
      DO 20 I=1,N
         IF (MESSG(LENMSG:LENMSG) .NE. ' ') GO TO 30
         LENMSG = LENMSG - 1
   20 CONTINUE
   30 CONTINUE
C
C       IF THE MESSAGE IS ALL BLANKS, THEN PRINT ONE BLANK LINE.
C
      IF (LENMSG .EQ. 0) THEN
         CBUFF(LPREF+1:LPREF+1) = ' '
         DO 40 I=1,NUNIT
            WRITE(IU(I), '(A)') CBUFF(1:LPREF+1)
   40    CONTINUE
         RETURN
      ENDIF
C
C       SET NEXTC TO THE POSITION IN MESSG WHERE THE NEXT SUBSTRING
C       STARTS.  FROM THIS POSITION WE SCAN FOR THE NEW LINE SENTINEL.
C       WHEN NEXTC EXCEEDS LENMSG, THERE IS NO MORE TO PRINT.
C       WE LOOP BACK TO LABEL 50 UNTIL ALL PIECES HAVE BEEN PRINTED.
C
C       WE LOOK FOR THE NEXT OCCURRENCE OF THE NEW LINE SENTINEL.  THE
C       INDEX INTRINSIC FUNCTION RETURNS ZERO IF THERE IS NO OCCURRENCE
C       OR IF THE LENGTH OF THE FIRST ARGUMENT IS LESS THAN THE LENGTH
C       OF THE SECOND ARGUMENT.
C
C       THERE ARE SEVERAL CASES WHICH SHOULD BE CHECKED FOR IN THE
C       FOLLOWING ORDER.  WE ARE ATTEMPTING TO SET LPIECE TO THE NUMBER
C       OF CHARACTERS THAT SHOULD BE TAKEN FROM MESSG STARTING AT
C       POSITION NEXTC.
C
C       LPIECE .EQ. 0   THE NEW LINE SENTINEL DOES NOT OCCUR IN THE
C                       REMAINDER OF THE CHARACTER STRING.  LPIECE
C                       SHOULD BE SET TO LWRAP OR LENMSG+1-NEXTC,
C                       WHICHEVER IS LESS.
C
C       LPIECE .EQ. 1   THE NEW LINE SENTINEL STARTS AT MESSG(NEXTC:
C                       NEXTC).  LPIECE IS EFFECTIVELY ZERO, AND WE
C                       PRINT NOTHING TO AVOID PRODUCING UNNECESSARY
C                       BLANK LINES.  THIS TAKES CARE OF THE SITUATION
C                       WHERE THE LIBRARY ROUTINE HAS A MESSAGE OF
C                       EXACTLY 72 CHARACTERS FOLLOWED BY A NEW LINE
C                       SENTINEL FOLLOWED BY MORE CHARACTERS.  NEXTC
C                       SHOULD BE INCREMENTED BY 2.
C
C       LPIECE .GT. LWRAP+1  REDUCE LPIECE TO LWRAP.
C
C       ELSE            THIS LAST CASE MEANS 2 .LE. LPIECE .LE. LWRAP+1
C                       RESET LPIECE = LPIECE-1.  NOTE THAT THIS
C                       PROPERLY HANDLES THE END CASE WHERE LPIECE .EQ.
C                       LWRAP+1.  THAT IS, THE SENTINEL FALLS EXACTLY
C                       AT THE END OF A LINE.
C
      NEXTC = 1
   50 LPIECE = INDEX(MESSG(NEXTC:LENMSG), NEWLIN)
      IF (LPIECE .EQ. 0) THEN
C
C       THERE WAS NO NEW LINE SENTINEL FOUND.
C
         IDELTA = 0
         LPIECE = MIN(LWRAP, LENMSG+1-NEXTC)
         IF (LPIECE .LT. LENMSG+1-NEXTC) THEN
            DO 52 I=LPIECE+1,2,-1
               IF (MESSG(NEXTC+I-1:NEXTC+I-1) .EQ. ' ') THEN
                  LPIECE = I-1
                  IDELTA = 1
                  GOTO 54
               ENDIF
   52       CONTINUE
         ENDIF
   54    CBUFF(LPREF+1:LPREF+LPIECE) = MESSG(NEXTC:NEXTC+LPIECE-1)
         NEXTC = NEXTC + LPIECE + IDELTA
      ELSEIF (LPIECE .EQ. 1) THEN
C
C       WE HAVE A NEW LINE SENTINEL AT MESSG(NEXTC:NEXTC+1).
C       DON'T PRINT A BLANK LINE.
C
         NEXTC = NEXTC + 2
         GO TO 50
      ELSEIF (LPIECE .GT. LWRAP+1) THEN
C
C       LPIECE SHOULD BE SET DOWN TO LWRAP.
C
         IDELTA = 0
         LPIECE = LWRAP
         DO 56 I=LPIECE+1,2,-1
            IF (MESSG(NEXTC+I-1:NEXTC+I-1) .EQ. ' ') THEN
               LPIECE = I-1
               IDELTA = 1
               GOTO 58
            ENDIF
   56    CONTINUE
   58    CBUFF(LPREF+1:LPREF+LPIECE) = MESSG(NEXTC:NEXTC+LPIECE-1)
         NEXTC = NEXTC + LPIECE + IDELTA
      ELSE
C
C       IF WE ARRIVE HERE, IT MEANS 2 .LE. LPIECE .LE. LWRAP+1.
C       WE SHOULD DECREMENT LPIECE BY ONE.
C
         LPIECE = LPIECE - 1
         CBUFF(LPREF+1:LPREF+LPIECE) = MESSG(NEXTC:NEXTC+LPIECE-1)
         NEXTC  = NEXTC + LPIECE + 2
      ENDIF
C
C       PRINT
C
      DO 60 I=1,NUNIT
         WRITE(IU(I), '(A)') CBUFF(1:LPREF+LPIECE)
   60 CONTINUE
C
      IF (NEXTC .LE. LENMSG) GO TO 50
      RETURN
      END
      SUBROUTINE XERSVE (LIBRAR, SUBROU, MESSG, KFLAG, NERR, LEVEL,
     +   ICOUNT)

c*********************************************************************72
c
cc XERSVE records that an error has occurred.
c
C***BEGIN PROLOGUE  XERSVE
C***SUBSIDIARY
C***PURPOSE  Record that an error has occurred.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3
C***TYPE      ALL (XERSVE-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C *Usage:
C
C        INTEGER  KFLAG, NERR, LEVEL, ICOUNT
C        CHARACTER * (len) LIBRAR, SUBROU, MESSG
C
C        CALL XERSVE (LIBRAR, SUBROU, MESSG, KFLAG, NERR, LEVEL, ICOUNT)
C
C *Arguments:
C
C        LIBRAR :IN    is the library that the message is from.
C        SUBROU :IN    is the subroutine that the message is from.
C        MESSG  :IN    is the message to be saved.
C        KFLAG  :IN    indicates the action to be performed.
C                      when KFLAG > 0, the message in MESSG is saved.
C                      when KFLAG=0 the tables will be dumped and
C                      cleared.
C                      when KFLAG < 0, the tables will be dumped and
C                      not cleared.
C        NERR   :IN    is the error number.
C        LEVEL  :IN    is the error severity.
C        ICOUNT :OUT   the number of times this message has been seen,
C                      or zero if the table has overflowed and does not
C                      contain this message specifically.  When KFLAG=0,
C                      ICOUNT will not be altered.
C
C *Description:
C
C   Record that this error occurred and possibly dump and clear the
C   tables.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  I1MACH, XGETUA
C***REVISION HISTORY  (YYMMDD)
C   800319  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900413  Routine modified to remove reference to KFLAG.  (WRB)
C   900510  Changed to add LIBRARY NAME and SUBROUTINE to calling
C           sequence, use IF-THEN-ELSE, make number of saved entries
C           easily changeable, changed routine name from XERSAV to
C           XERSVE.  (RWC)
C   910626  Added LIBTAB and SUBTAB to SAVE statement.  (BKS)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERSVE
      PARAMETER (LENTAB=10)
      INTEGER LUN(5)
      CHARACTER*(*) LIBRAR, SUBROU, MESSG
      CHARACTER*8  LIBTAB(LENTAB), SUBTAB(LENTAB), LIB, SUB
      CHARACTER*20 MESTAB(LENTAB), MES
      DIMENSION NERTAB(LENTAB), LEVTAB(LENTAB), KOUNT(LENTAB)
      SAVE LIBTAB, SUBTAB, MESTAB, NERTAB, LEVTAB, KOUNT, KOUNTX, NMSG
      DATA KOUNTX/0/, NMSG/0/
C***FIRST EXECUTABLE STATEMENT  XERSVE
C
      IF (KFLAG.LE.0) THEN
C
C        Dump the table.
C
         IF (NMSG.EQ.0) RETURN
C
C        Print to each unit.
C
         CALL XGETUA (LUN, NUNIT)
         DO 20 KUNIT = 1,NUNIT
            IUNIT = LUN(KUNIT)
            IF (IUNIT.EQ.0) IUNIT = I1MACH(4)
C
C           Print the table header.
C
            WRITE (IUNIT,9000)
C
C           Print body of table.
C
            DO 10 I = 1,NMSG
               WRITE (IUNIT,9010) LIBTAB(I), SUBTAB(I), MESTAB(I),
     *            NERTAB(I),LEVTAB(I),KOUNT(I)
   10       CONTINUE
C
C           Print number of other errors.
C
            IF (KOUNTX.NE.0) WRITE (IUNIT,9020) KOUNTX
            WRITE (IUNIT,9030)
   20    CONTINUE
C
C        Clear the error tables.
C
         IF (KFLAG.EQ.0) THEN
            NMSG = 0
            KOUNTX = 0
         ENDIF
      ELSE
C
C        PROCESS A MESSAGE...
C        SEARCH FOR THIS MESSG, OR ELSE AN EMPTY SLOT FOR THIS MESSG,
C        OR ELSE DETERMINE THAT THE ERROR TABLE IS FULL.
C
         LIB = LIBRAR
         SUB = SUBROU
         MES = MESSG
         DO 30 I = 1,NMSG
            IF (LIB.EQ.LIBTAB(I) .AND. SUB.EQ.SUBTAB(I) .AND.
     *         MES.EQ.MESTAB(I) .AND. NERR.EQ.NERTAB(I) .AND.
     *         LEVEL.EQ.LEVTAB(I)) THEN
                  KOUNT(I) = KOUNT(I) + 1
                  ICOUNT = KOUNT(I)
                  RETURN
            ENDIF
   30    CONTINUE
C
         IF (NMSG.LT.LENTAB) THEN
C
C           Empty slot found for new message.
C
            NMSG = NMSG + 1
            LIBTAB(I) = LIB
            SUBTAB(I) = SUB
            MESTAB(I) = MES
            NERTAB(I) = NERR
            LEVTAB(I) = LEVEL
            KOUNT (I) = 1
            ICOUNT    = 1
         ELSE
C
C           Table is full.
C
            KOUNTX = KOUNTX+1
            ICOUNT = 0
         ENDIF
      ENDIF
      RETURN
C
C     Formats.
C
 9000 FORMAT ('0          ERROR MESSAGE SUMMARY' /
     +   ' LIBRARY    SUBROUTINE MESSAGE START             NERR',
     +   '     LEVEL     COUNT')
 9010 FORMAT (1X,A,3X,A,3X,A,3I10)
 9020 FORMAT ('0OTHER ERRORS NOT INDIVIDUALLY TABULATED = ', I10)
 9030 FORMAT (1X)
      END
      SUBROUTINE XGETF (KONTRL)

c*********************************************************************72
c
cc XGETF returns the current value of the error control flag.
c
C***BEGIN PROLOGUE  XGETF
C***PURPOSE  Return the current value of the error control flag.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XGETF-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C   Abstract
C        XGETF returns the current value of the error control flag
C        in KONTRL.  See subroutine XSETF for flag value meanings.
C        (KONTRL is an output parameter only.)
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  J4SAVE
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XGETF
C***FIRST EXECUTABLE STATEMENT  XGETF
      KONTRL = J4SAVE(2,0,.FALSE.)
      RETURN
      END
      SUBROUTINE XGETUA (IUNITA, N)

c*********************************************************************72
c
cc XGETUA returns error unit numbers.
c
C***BEGIN PROLOGUE  XGETUA
C***PURPOSE  Return unit number(s) to which error messages are being
C            sent.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XGETUA-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        XGETUA may be called to determine the unit number or numbers
C        to which error messages are being sent.
C        These unit numbers may have been set by a call to XSETUN,
C        or a call to XSETUA, or may be a default value.
C
C     Description of Parameters
C      --Output--
C        IUNIT - an array of one to five unit numbers, depending
C                on the value of N.  A value of zero refers to the
C                default unit, as defined by the I1MACH machine
C                constant routine.  Only IUNIT(1),...,IUNIT(N) are
C                defined by XGETUA.  The values of IUNIT(N+1),...,
C                IUNIT(5) are not defined (for N .LT. 5) or altered
C                in any way by XGETUA.
C        N     - the number of units to which copies of the
C                error messages are being sent.  N will be in the
C                range from 1 to 5.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  J4SAVE
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XGETUA
      DIMENSION IUNITA(5)
C***FIRST EXECUTABLE STATEMENT  XGETUA
      N = J4SAVE(5,0,.FALSE.)
      DO 30 I=1,N
         INDEX = I+4
         IF (I.EQ.1) INDEX = 3
         IUNITA(I) = J4SAVE(INDEX,0,.FALSE.)
   30 CONTINUE
      RETURN
      END
      SUBROUTINE XSETF (KONTRL)

c*********************************************************************72
c
cc XSETF sets the error control flag.
c
C***BEGIN PROLOGUE  XSETF
C***PURPOSE  Set the error control flag.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3A
C***TYPE      ALL (XSETF-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        XSETF sets the error control flag value to KONTRL.
C        (KONTRL is an input parameter only.)
C        The following table shows how each message is treated,
C        depending on the values of KONTRL and LEVEL.  (See XERMSG
C        for description of LEVEL.)
C
C        If KONTRL is zero or negative, no information other than the
C        message itself (including numeric values, if any) will be
C        printed.  If KONTRL is positive, introductory messages,
C        trace-backs, etc., will be printed in addition to the message.
C
C              ABS(KONTRL)
C        LEVEL        0              1              2
C        value
C          2        fatal          fatal          fatal
C
C          1     not printed      printed         fatal
C
C          0     not printed      printed        printed
C
C         -1     not printed      printed        printed
C                                  only           only
C                                  once           once
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  J4SAVE, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900510  Change call to XERRWV to XERMSG.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XSETF
      CHARACTER *8 XERN1
C***FIRST EXECUTABLE STATEMENT  XSETF
      IF (ABS(KONTRL) .GT. 2) THEN
         WRITE (XERN1, '(I8)') KONTRL
         CALL XERMSG ('SLATEC', 'XSETF',
     *      'INVALID ARGUMENT = ' // XERN1, 1, 2)
         RETURN
      ENDIF
C
      JUNK = J4SAVE(2,KONTRL,.TRUE.)
      RETURN
      END
