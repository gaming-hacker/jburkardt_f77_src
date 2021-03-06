      SUBROUTINE CEXQAD(Z,N,KODE,TOL,CW,KERR)
C
C     CEXQAD COMPUTES EXPONENTIAL INTEGRALS E(N,Z) OF A COMPLEX ARGUMENT
C     Z BY QUADRATURE ON  (T**(N-1)*EXP(-T)/(Z+T))/(N-1)! FROM T=0 TO
C     T=INFINITY. KODE=1 RETURNS CW=E(N,Z) WHILE KODE=2 RETURNS
C     CW=E(N,Z)*CEXP(Z). TOL IS THE REQUESTED RELATIVE ERROR AND
C     KERR.NE.0 IS AN ERROR FLAG INDICATING A PREMATURE TRUNCATION OF
C     AN INTEGRAL IN CEXQAD. THE QUADRATURE IS DONE AS TWO REAL
C     INTEGRALS SINCE Z IS COMPLEX.
C
      COMMON/QAD/FNM,GLN,X,Y,IFLAG
      EXTERNAL FQCEX
      COMPLEX Z,CW
      FN=FLOAT(N)
      FNM=FN-1.0
      GM=1.0E0
      IF(N.EQ.1) GO TO 6
      DO 5 I=2,N
        GM=GM*FLOAT(I-1)
    5 CONTINUE
    6 CONTINUE
      GLN=ALOG(GM)
      KERR=0
      X=REAL(Z)
      Y=AIMAG(Z)
C-----------------------------------------------------------------------
C     REAL PART OF E(N,Z)
C-----------------------------------------------------------------------
      IFLAG=1
      S1=0.0E0
      B=0.0E0
      DO 10 I=1,100
        A=B
        B=B+5.0E0
        XTOL=TOL
        CALL GAUS8(FQCEX,A,B,XTOL,ANS,IERR)
        S1=S1+ANS
        IF(ABS(ANS).LT.ABS(S1)*TOL) GO TO 20
   10 CONTINUE
      KERR=1
   20 CONTINUE
C-----------------------------------------------------------------------
C     IMAGINARY PART OF E(N,Z)
C-----------------------------------------------------------------------
      IFLAG=2
      S2=0.0E0
      IF(Y.EQ.0.0E0) GO TO 42
      B=0.0E0
      DO 30 I=1,100
        A=B
        B=B+5.0E0
        XTOL=TOL
        CALL GAUS8(FQCEX,A,B,XTOL,ANS,IERR)
        S2=S2+ANS
        IF(ABS(ANS).LT.ABS(S2)*TOL) GO TO 40
   30 CONTINUE
      KERR=2
   40 CONTINUE
   42 CONTINUE
      CW=CMPLX(S1,-S2)
      IF(KODE.EQ.1) CW=CW*CEXP(-Z)
      RETURN
      END
      FUNCTION FQCEX(T)
      COMMON/QAD/FNM,GLN,X,Y,IFLAG
      A=-T+FNM*ALOG(T)-GLN
      A=EXP(A)
      IF(IFLAG.EQ.2) GO TO 10
      B=(X+T)/((X+T)**2+Y**2)
      GO TO 20
   10 CONTINUE
      B=Y/((X+T)**2+Y**2)
   20 CONTINUE
      FQCEX=A*B
      RETURN
      END
      SUBROUTINE GAUS8(FUN, A, B, ERR, ANS, IERR)
C
C     WRITTEN BY R.E. JONES
C
C     ABSTRACT
C        GAUS8 INTEGRATES REAL FUNCTIONS OF ONE VARIABLE OVER FINITE
C        INTERVALS USING AN ADAPTIVE 8-POINT LEGENDRE-GAUSS ALGORITHM.
C        GAUS8 IS INTENDED PRIMARILY FOR HIGH ACCURACY INTEGRATION
C        OR INTEGRATION OF SMOOTH FUNCTIONS.
C
C        GAUS8 CALLS I1MACH, R1MACH, XERROR
C
C     DESCRIPTION OF ARGUMENTS
C
C        INPUT--
C        FUN - NAME OF EXTERNAL FUNCTION TO BE INTEGRATED. THIS NAME
C              MUST BE IN AN EXTERNAL STATEMENT IN THE CALLING PROGRAM.
C              FUN MUST BE A FUNCTION OF ONE REAL ARGUMENT. THE VALUE
C              OF THE ARGUMENT TO FUN IS THE VARIABLE OF INTEGRATION
C              WHICH RANGES FROM A TO B.
C        A   - LOWER LIMIT OF INTEGRAL
C        B   - UPPER LIMIT OF INTEGRAL (MAY BE LESS THAN A)
C        ERR - IS A REQUESTED PSEUDORELATIVE ERROR TOLERANCE.  NORMALLY
C              PICK A VALUE OF ABS(ERR) SO THAT STOL.LT.ABS(ERR).LE.
C              1.0E-3 WHERE STOL IS THE SINGLE PRECISION UNIT ROUNDOFF
C              =R1MACH(4). ANS WILL NORMALLY HAVE NO MORE ERROR THAN
C              ASB(ERR) TIMES THE INTEGRAL OF THE ABSOLUTE VALUE OF
C              FUN(X). USUALLY, SMALLER VALUES FOR ERR YIELD MORE
C              ACCURACY AND REQUIRE MORE FUNCTION EVALUATIONS.
C
C              A NEGATIVE VALUE FOR ERR CAUSES AN ESTIMATE OF THE
C              ABSOLUTE ERROR IN ANS TO BE RETURNED IN ERR. NOTE THAT
C              ERR MUST BE A VARIABLE (NOT A CONSTANT) IN THIS CASE.
C              NOTE ALSO THAT THE USER MUST RESET THE VALUE OF ERR
C              BEFORE MAKING ANY MORE CALLS THAT USE THE VARIABLE ERR.
C
C        OUTPUT--
C        ERR - WILL BE AN ESTIMATE OF THE ABSOLUTE ERROR IN ANS IF THE
C              INPUT VALUE OF ERR WAS NEGATIVE.  (ERR IS UNCHANGED IF
C              THE INPUT VALUE OF ERR WAS NONNEGATIVE.)  THE ESTIMATED
C              ERROR IS SOLELY FOR INFORMATION TO THE USER AND SHOULD
C              NOT BE USED AS A CORRECTION TO THE COMPUTED INTEGRAL.
C        ANS - COMPUTED VALUE OF INTEGRAL
C        IERR- A STATUS CODE
C            --NORMAL CODES
C               1 ANS MOST LIKELY MEETS REQUESTED ERROR TOLERANCE,
C                 OR A=B.
C              -1 A AND B ARE TOO NEARLY EQUAL TO ALLOW NORMAL
C                 INTEGRATION.  ANS IS SET TO ZERO.
C            --ABNORMAL CODE
C               2 ANS PROBABLY DOES NOT MEET REQUESTED ERROR TOLERANCE.
C***END PROLOGUE
      EXTERNAL FUN
      INTEGER ICALL, IERR, K, KML, KMX, L, LMN, LMX, LR, MXL, NBITS,
     * NIB, NLMN, NLMX
      INTEGER I1MACH
      REAL A, AA, AE, ANIB, ANS, AREA, B, C, CE, EE, EF, EPS, ERR, EST,
     * GL, GLR, GR, HH, SQ2, TOL, VL, VR, W1, W2, W3, W4, X1, X2, X3,
     * X4, X, H
      REAL R1MACH, G8, FUN
      DIMENSION AA(30), HH(30), LR(30), VL(30), GR(30)
      DATA X1, X2, X3, X4/
     1     1.83434642495649805E-01,     5.25532409916328986E-01,
     2     7.96666477413626740E-01,     9.60289856497536232E-01/
      DATA W1, W2, W3, W4/
     1     3.62683783378361983E-01,     3.13706645877887287E-01,
     2     2.22381034453374471E-01,     1.01228536290376259E-01/
      DATA ICALL  /  0  /
      DATA SQ2/1.41421356E0/
      DATA NLMN/1/,KMX/5000/,KML/6/
      G8(X,H)=H*((W1*(FUN(X-X1*H) + FUN(X+X1*H))
     1           +W2*(FUN(X-X2*H) + FUN(X+X2*H)))
     2          +(W3*(FUN(X-X3*H) + FUN(X+X3*H))
     3           +W4*(FUN(X-X4*H) + FUN(X+X4*H))))
C
C     INITIALIZE
C
      IF (ICALL.NE.0) CALL XERROR('GAUS8- GAUS8 CALLED RECURSIVELY.  REC
     *URSIVE CALLS ARE ILLEGAL IN FORTRAN.', 73, 7, 2)
      ICALL = 1
      K = I1MACH(11)
      ANIB = R1MACH(5)*FLOAT(K)/0.30102000E0
      NBITS = INT(ANIB)
      NLMX = (NBITS*5)/8
      ANS = 0.0E0
      IERR = 1
      CE = 0.0E0
      IF (A.EQ.B) GO TO 140
      LMX = NLMX
      LMN = NLMN
      IF (B.EQ.0.0E0) GO TO 10
      IF (SIGN(1.0E0,B)*A.LE.0.0E0) GO TO 10
      C = ABS(1.0E0-A/B)
      IF (C.GT.0.1E0) GO TO 10
      IF (C.LE.0.0E0) GO TO 140
      ANIB = 0.5E0 - ALOG(C)/0.69314718E0
      NIB = INT(ANIB)
      LMX = MIN0(NLMX,NBITS-NIB-7)
      IF (LMX.LT.1) GO TO 130
      LMN = MIN0(LMN,LMX)
   10 TOL = AMAX1(ABS(ERR),2.0E0**(5-NBITS))/2.0E0
      IF (ERR.EQ.0.0E0) TOL = SQRT(R1MACH(4))
      EPS = TOL
      HH(1) = (B-A)/4.0E0
      AA(1) = A
      LR(1) = 1
      L = 1
      EST = G8(AA(L)+2.0E0*HH(L),2.0E0*HH(L))
      K = 8
      AREA = ABS(EST)
      EF = 0.5E0
      MXL = 0
C
C     COMPUTE REFINED ESTIMATES, ESTIMATE THE ERROR, ETC.
C
   20 GL = G8(AA(L)+HH(L),HH(L))
      GR(L) = G8(AA(L)+3.0E0*HH(L),HH(L))
      K = K + 16
      AREA = AREA + (ABS(GL)+ABS(GR(L))-ABS(EST))
C     IF (L.LT.LMN) GO TO 11
      GLR = GL + GR(L)
      EE = ABS(EST-GLR)*EF
      AE = AMAX1(EPS*AREA,TOL*ABS(GLR))
      IF (EE-AE) 40, 40, 50
   30 MXL = 1
   40 CE = CE + (EST-GLR)
      IF (LR(L)) 60, 60, 80
C
C     CONSIDER THE LEFT HALF OF THIS LEVEL
C
   50 IF (K.GT.KMX) LMX = KML
      IF (L.GE.LMX) GO TO 30
      L = L + 1
      EPS = EPS*0.5E0
      EF = EF/SQ2
      HH(L) = HH(L-1)*0.5E0
      LR(L) = -1
      AA(L) = AA(L-1)
      EST = GL
      GO TO 20
C
C     PROCEED TO RIGHT HALF AT THIS LEVEL
C
   60 VL(L) = GLR
   70 EST = GR(L-1)
      LR(L) = 1
      AA(L) = AA(L) + 4.0E0*HH(L)
      GO TO 20
C
C     RETURN ONE LEVEL
C
   80 VR = GLR
   90 IF (L.LE.1) GO TO 120
      L = L - 1
      EPS = EPS*2.0E0
      EF = EF*SQ2
      IF (LR(L)) 100, 100, 110
  100 VL(L) = VL(L+1) + VR
      GO TO 70
  110 VR = VL(L+1) + VR
      GO TO 90
C
C      EXIT
C
  120 ANS = VR
      IF ((MXL.EQ.0) .OR. (ABS(CE).LE.2.0E0*TOL*AREA)) GO TO 140
      IERR = 2
      CALL XERROR('GAUS8- ANS IS PROBABLY INSUFFICIENTLY ACCURATE.',
     * 47, 3, 1)
      GO TO 140
  130 IERR = -1
      CALL XERROR('GAUS8- THE FOLLOWING TEMPORARY DIAGNOSTIC WILL APPEAR
     * ONLY ONCE.  A AND B ARE TOO NEARLY EQUAL TO ALLOW NORMAL INTEGRAT
     *ION.  ANS IS SET TO ZERO, AND IERR=-1.', 157, 1, -1)
  140 ICALL = 0
      IF (ERR.LT.0.0E0) ERR = CE
      RETURN
      END
      SUBROUTINE CEXINT(Z, N, KODE, TOL, M, CY, IERR)
C***BEGIN PROLOGUE  CEXINT
C***DATE WRITTEN   870515   (YYMMDD)
C***REVISION DATE  870515   (YYMMDD)
C***CATEGORY NO.  B5E
C***KEYWORDS  EXPONENTIAL INTEGRALS, SINE INTEGRAL, COSINE INTEGRAL
C***AUTHOR  AMOS, DONALD E., SANDIA NATIONAL LABORATORIES
C***PURPOSE  TO COMPUTE EXPONENTIAL INTEGRALS OF A COMPLEX ARGUMENT
C***DESCRIPTION
C
C         ON KODE=1, CEXINT COMPUTES AN M MEMBER SEQUENCE OF COMPLEX
C         EXPONENTIAL INTEGRALS CY(J)=E(N+J-1,Z), J=1,...,M, FOR
C         POSITIVE ORDERS N,...,N+M-1 AND COMPLEX Z IN THE CUT PLANE
C         -PI.LT.ARG(Z).LE.PI (N=1 AND Z=CMPLX(0.0,0.0) CANNOT HOLD AT
C         THE SAME TIME). ON KODE=2, CEXINT COMPUTES SCALED FUNCTIONS
C
C                          CY(J)=E(N+J-1,Z)*CEXP(Z),      J=1,...,M,
C
C         WHICH REMOVES THE EXPONENTIAL BEHAVIOR IN BOTH THE LEFT AND
C         RIGHT HALF PLANES. DEFINITIONS AND NOTATION ARE FOUND IN THE
C         NBS HANDBOOK OF MATHEMATICAL FUNCTIONS (REF. 1).
C
C         INPUT
C           Z      - Z=CMPLX(X,Y), -PI.LT.ARG(Z).LE.PI
C           N      - INTEGER ORDER OF INITIAL E FUNCTION, N=1,2,...
C                    (N=1 AND Z=CMPLX(0.0,0.0) IS AN ERROR)
C           KODE   - A PARAMETER TO INDICATE THE SCALING OPTION
C                    KODE= 1  RETURNS
C                             CY(J)=E(N+J-1,Z),          J=1,...,M
C                        = 2  RETURNS
C                             CY(J)=E(N+J-1,Z)*CEXP(Z),  J=1,...,M
C           TOL    - PRECISION (ACCURACY) DESIRED FOR THE SEQUENCE,
C                    URND.LE.TOL.LE.1.0E-3, WHERE URND IS LIMITED BY
C                    URND=AMAX1(UNIT ROUNDOFF,1.0E-18) AND UNIT
C                    ROUNDOFF = R1MACH(4) IN TERMS OF MACHINE CONSTANT
C                    ROUTINE R1MACH
C           M      - NUMBER OF E FUNCTIONS IN THE SEQUENCE, M.GE.1
C
C         OUTPUT
C           CY     - A COMPLEX VECTOR WHOSE FIRST M COMPONENTS CONTAIN
C                    VALUES FOR THE SEQUENCE
C                    CY(J)=E(N+J-1,Z)  OR
C                    CY(J)=E(N+J-1,Z)*CEXP(Z), J=1,...,M
C                    DEPENDING ON KODE.
C           IERR   - ERROR FLAG
C                    IERR=0, NORMAL RETURN - COMPUTATION COMPLETED
C                    IERR=1, INPUT ERROR   - NO COMPUTATION
C                    IERR=2, UNDERFLOW     - FIRST M COMPONENTS OF CY
C                            SET TO ZERO, CY(J)=CMPLX(0.0,0.0), J=1,M,
C                            REAL(Z).GT.0.0 TOO LARGE ON KODE=1
C                    IERR=3, OVERFLOW      - NO COMPUTATION,
C                            REAL(Z).LT.0.0 TOO SMALL ON KODE=1
C                    IERR=4, CABS(Z) OR N+M-1 LARGE - COMPUTATION DONE
C                            BUT LOSSES OF SIGNIFICANCE BY ARGUMENT
C                            REDUCTION PRODUCE LESS THAN HALF OF MACHINE
C                            ACCURACY
C                    IERR=5, CABS(Z) OR N+M-1 TOO LARGE - NO COMPUTA-
C                            TION BECAUSE OF COMPLETE LOSSES OF SIGNIFI-
C                            CANCE BY ARGUMENT REDUCTION
C                    IERR=6, ERROR         - NO COMPUTATION,
C                            ALGORITHM TERMINATION CONDITION NOT MET.
C                            SEE LONG DESCRIPTION ABOUT PARAMETER ICMAX.
C                    IERR=7, ERROR         - NO COMPUTATION,
C                            DISCRIMINATION ERROR. THIS CONDITION SHOULD
C                            NEVER OCCUR.
C
C***LONG DESCRIPTION
C
C         CEXINT USES A COMBINATION OF POWER SERIES AND BACKWARD RECUR-
C         RENCE DESCRIBED IN REF. 2 FOR THE COMPLEX Z PLANE EXCEPT FOR
C         A STRIP 2*YB WIDE ABOUT THE NEGATIVE REAL AXIS, WHERE ANALYTIC
C         CONTINUATION IS CARRIED OUT BY LIMITED USE OF TAYLOR SERIES.
C         THE SWITCH FROM BACKWARD RECURRENCE TO TAYLOR SERIES IS
C         NECESSARY BECAUSE BACKWARD RECURRENCE IS SLOWLY CONVERGENT
C         NEAR THE NEGATIVE REAL AXIS. THE BOUNDARIES Y=-YB AND
C         Y=YB WERE DETERMINED SO THAT BACKWARD RECURRENCE WOULD CON-
C         VERGE EASILY WITH N AS LARGE AS 100 AND TOL AS SMALL AS
C         1.0E-18. SUBROUTINE CEXENZ DOES THE BACKWARD RECURRENCE AND
C         SUBROUTINE CACEXI DOES THE ANALYTIC CONTINUATION. TO START THE
C         CONTINUATION, CACEXI CALLS CEXENZ WITH ZB=CMPLX(X,YB).
C         IF CEXENZ RETURNS IERR=6, THEN YB IS INCREASED BY 0.5 UNTIL
C         CEXENZ RETURNS IERR=0 OR 10 TRIES, WHICHEVER COMES FIRST.
C         WHEN IERR=0, THEN THE ANALYTIC CONTINUATION PROCEEDS VERTI-
C         CALLY DOWN FROM ZB=CMPLX(X,YB) TO Z=CMPLX(X,Y), 0.LE.Y.LT.YB.
C         CONJUGATION IS USED FOR Y.LT.0. YB INCREASES AS TOL DECREASES
C         TO KEEP CONVERGENCE RATES UP AND RECURRENCE DOWN.
C
C         PARAMETER ICDIM=250 ALLOCATES STORAGE FOR THE COEFFICIENTS OF
C         THE BACKWARD RECURRENCE ALGORITHM. IF THE ALGORITHM TERMINA-
C         TION CONDITION IS NOT MET IN ICDIM STEPS, THEN RECURRENCE PRO-
C         CEEDS WITH NO ADDITIONAL STORAGE UNTIL THE TERMINATION CONDI-
C         TION IS MET OR THE LIMIT ICMAX=2000 IS EXCEEDED. THE PURPOSE
C         OF STORAGE IS TO MAKE THE ALGORITHM MORE EFFICIENT. THE TERMI-
C         NATION CONDITION IS MET IN LESS THAN 250 STEPS OVER MOST OF
C         THE COMPLEX PLANE EXCLUDING THE STRIP ABS(Y).LT.YB, X.LT.0.
C         EXCEPTIONS TO THIS RULE ARE GENERATED NEAR STRIP BOUNDARIES
C         WHEN N+M-1 AND CABS(Z) ARE LARGE AND NEARLY EQUAL. IN THESE
C         CASES, THE CONVERGENCE IS VERY SLOW AND ADDITIONAL RECURRENCE
C         (UP TO ICMAX) MUST BE USED. ON THE OTHERHAND, THESE REGIONS
C         OF SLOW CONVERGENCE ARE KEPT SMALL BY ADJUSTING YB AS A FUNC-
C         TION OF TOL. THESE REGIONS COULD BE ELIMINATED ENTIRELY BY
C         MAKING YB SUFFICIENTLY LARGE, BUT THE EXPENSE AND INSTABILITY
C         OF CONTINUATION BY TAYLOR SERIES NOT ONLY GOES UP, BUT THE
C         COMPUTATIONAL EXPENSE BECOMES EXCESSIVELY LARGE IN OTHER PARTS
C         OF THE LEFT HALF PLANE (Y.LT.YB) WHERE THE BACKWARD RECURRENCE
C         ALGORITHM WOULD CONVERGE RAPIDLY.
C
C         DERIVATIVES FOR SUCCESSIVE POWER SERIES ARE NOT COMPUTED BY
C         EVALUATING DERIVATIVES OF A PREVIOUS POWER SERIES. BECAUSE OF
C         THE RELATION
C
C           (1)           DE(N,Z)/DZ = - E(N-1,Z),
C
C         SUCCESSIVE DERIVATIVES AT Z ARE GIVEN BY LOWER ORDER FUNCTIONS
C         AND CAN BE COMPUTED IN A STABLE FASHION BY BACKWARD RECURRENCE
C         USING (2) PROVIDED THAT THE BEGINNING ORDER NUB IS SMALLER
C         THAN THE ARGUMENT. TO ACHIEVE THIS FOR ALL INTERMEDIATE VALUES
C         ZZ BETWEEN ZB AND Z, WE TAKE NUB=MINO(N+M-1,INT(CABS(Z)+0.5)).
C         TO START, E(NUB,ZB) IS EVALUATED BY THE BACKWARD RECURRENCE
C         ALGORITHM OF REF. 3. TO CONTINUE THE FUNCTION FROM ZB TO Z VIA
C         INTERMEDIATE VALUES ZZ, DERIVATIVES OF E(NUB,ZB) ARE COMPUTED
C         BY BACKWARD RECURRENCE ON (2). THIS ALLOWS A STEP (NO LARGER
C         THAN 0.5) TO ZZ FOR E(NUB,ZZ) USING THE TAYLOR SERIES ABOUT
C         ZB. NOW, WE APPLY (2) AGAIN STARTING AT E(NUB,ZZ) FOR THE DE-
C         RIVATIVES AT ZZ AND TAKE ANOTHER STEP, ETC. NOTICE THAT THE
C         STABILITY CONDITION FOR BACKWARD RECURRENCE, NUB.LE.CABS(Z)
C         .LE.CABS(ZZ).LE.CABS(ZB), IS SATISFIED FOR ALL INTERMEDIATE
C         VALUES ZZ. THE FINAL SEQUENCE FOR ORDERS N,...,N+M-1 IS GEN-
C         ERATED FROM (2) BY BACKWARD RECURRENCE, FORWARD RECURRENCE
C         OR BOTH ONCE E(NUB,Z) HAS BEEN COMPUTED.
C
C         RECURRENCE WITH THE RELATION
C
C            (2)     N*E(N+1,Z) + Z*E(N,Z) = CEXP(-Z)
C
C         IN A DIRECTION AWAY FROM THE INTEGER CLOSEST TO CABS(Z) IS
C         STABLE. FOR NEGATIVE ORDERS, THE RECURRENCE
C
C               E( 0,Z) = CEXP(-Z)/Z
C               E(-N,Z) = ( CEXP(-Z)+N*E(-N+1,Z) )/Z   ,N=1,2,...
C
C         IS NUMERICALLY STABLE FOR ALL Z.
C
C         THE (CAPITAL) SINE AND COSINE INTEGRALS CAN BE COMPUTED FROM
C
C                 SI(Z) =  (E(1,I*Z)-E(1,-I*Z))/(2*I) + PI/2
C                 CI(Z) = -(E(1,I*Z)+E(1,-I*Z))/2
C
C         IN -PI/2.LT.ARG(Z).LE.PI/2, (I**2=-1), WHILE THE PRINCIPAL
C         VALUED EXPONENTIAL INTEGRAL EI(X) CAN BE COMPUTED FROM
C
C             EI( X) = -(E(1,-X+I*0)+E(1,-X-I*0))/2 = -REAL(E(1,-X))
C             EI(-X) = -REAL(E(1,X))
C
C         FOR X.GT.0.0 TO AN ACCURACY TOL. IF Z = X.GT.0 THEN THE
C         REAL SINE AND COSINE INTEGRALS ARE GIVEN BY
C
C                 SI(X) = AIMAG(E(1,I*X)) + PI/2
C                 CI(X) = -REAL(E(1,I*X)) .
C
C         THE ANALYTIC CONTINUATION TO OTHER SHEETS CAN BE DONE BY
C         THE RELATIONS
C
C         E(N,Z*CEXP(2*PI*M*I)) = E(N,Z) - 2*PI*M*I*(-Z)**(N-1)/(N-1)!
C
C         WHERE M=+1 OR M=-1 AND I**2=-1.
C
C***REFERENCES  HANDBOOK OF MATHEMATICAL FUNCTIONS BY M. ABRAMOWITZ
C                 AND I. A. STEGUN, NBS AMS SERIES 55, U.S. DEPT. OF
C                 COMMERCE, 1955.
C
C               COMPUTATION OF EXPONENTIAL INTEGRALS OF COMPLEX ARGUMENT
C                 BY D. E. AMOS, ACM TRANS. MATH. SOFTWARE
C
C               COMPUTATION OF EXPONENTIAL INTEGRALS
C                 BY D. E. AMOS, ACM TRANS. MATH. SOFTWARE, VOL 6, NO. 3
C                 SEPTEMBER 1980, PP. 365-377; ALGORITHM 556, EXPONEN-
C                 TIAL INTEGRALS, PP. 420-428.
C
C               REMARK ON ALGORITHM 556
C                 BY D. E. AMOS, ACM TRANS. MATH. SOFTWARE, VOL 9, NO. 4
C                 DECEMBER 1983, P. 525.
C
C               UNIFORM ASYMPTOTIC EXPANSIONS FOR EXPONENTIAL INTE-
C                 GRALS E(N,X) AND BICKLEY FUNCTIONS KI(N,X) BY D. E.
C                 AMOS, ACM TRANS. MATH. SOFTWARE, VOL 9, NO. 4,
C                 DECEMBER. 1983, PP. 467-479; ALGORITHM 609, A PORTA-
C                 BLE FORTRAN SUBROUTINE FOR BICKLEY FUNCTIONS KI(N,X),
C                 PP. 480-493.
C
C***ROUTINES CALLED  CACEXI,CEXENZ,I1MACH,R1MACH
C***END PROLOGUE  CEXINT
C
C
      INTEGER I, ICDIM, IERR, K, KODE, K1, K2, M, N, I1MACH
      REAL AA, ALIM, AZ, BB, CA, D, ELIM, FN, RBRY, R1M5, TOL, URND, X,
     * Y, YB, HTOL, R1MACH
      COMPLEX Z, CY, CB
C-----------------------------------------------------------------------
C     DIMENSION CA(ICDIM),CB(ICDIM)
C-----------------------------------------------------------------------
      DIMENSION CY(M), CA(250), CB(250)
      DATA ICDIM /250/
C***FIRST EXECUTABLE STATEMENT  CEXINT
      IERR = 0
      X = REAL(Z)
      Y = AIMAG(Z)
      IF (X.EQ.0.0E0 .AND. Y.EQ.0.0E0 .AND. N.EQ.1) IERR = 1
      IF (N.LT.1) IERR = 1
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR = 1
      IF (M.LT.1) IERR = 1
      URND = AMAX1(R1MACH(4),1.0E-18)
      IF (TOL.LT.URND .OR. TOL.GT.1.0E-3) IERR = 1
      IF (IERR.NE.0) RETURN
      IF (X.EQ.0.0E0 .AND. Y.EQ.0.0E0 .AND. N.GT.1) GO TO 40
C-----------------------------------------------------------------------
C     SET PARAMETERS RELATED TO MACHINE CONSTANTS.
C     URND IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18.
C     ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT.
C     EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/URND    AND
C     EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*URND       ARE INTERVALS NEAR
C     UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC IS DONE.
C-----------------------------------------------------------------------
      K1 = I1MACH(12)
      K2 = I1MACH(13)
      R1M5 = R1MACH(5)
      K = MIN0(IABS(K1),IABS(K2))
      ELIM = 2.303E0*(FLOAT(K)*R1M5-3.0E0)
      K1 = I1MACH(11) - 1
      AA = R1M5*FLOAT(K1)
      AA = AA*2.303E0
      ALIM = ELIM + AMAX1(-AA,-41.45E0)
      RBRY = 2.0E0
      IF (URND.GT.1.0E-8) RBRY = 1.0E0
C-----------------------------------------------------------------------
C     TEST VARIABLES FOR RANGE. CABS(Z) CANNOT BE LARGER THAN THE ARG-
C     UMENT OF THE INT( ) FUNCTION.
C-----------------------------------------------------------------------
      AZ = CABS(Z)
      FN = FLOAT(N+M-1)
      AA = 0.5E0/URND
      BB = FLOAT(I1MACH(9))*0.5E0
      AA = AMIN1(AA,BB)
      IF (AZ.GT.AA) GO TO 60
      IF (FN.GT.AA) GO TO 60
      AA = SQRT(AA)
      IF (AZ.GT.AA) IERR = 4
      IF (FN.GT.AA) IERR = 4
      IF (X.LT.0.0E0) GO TO 10
C-----------------------------------------------------------------------
C     BACKWARD RECURRENCE FOR THE RIGHT HALF PLANE, X.GE.0.0E0
C-----------------------------------------------------------------------
      CALL CEXENZ(Z, N, KODE, M, CY, IERR, RBRY, TOL, ELIM, ALIM,
     * ICDIM, CA, CB)
      RETURN
   10 CONTINUE
      IF (AZ.GT.RBRY) GO TO 20
C-----------------------------------------------------------------------
C     POWER SERIES FOR CABS(Z).LE.RBRY AND X.LT.0.0E0
C-----------------------------------------------------------------------
      CALL CEXENZ(Z, N, KODE, M, CY, IERR, RBRY, TOL, ELIM, ALIM,
     * ICDIM, CA, CB)
      RETURN
   20 CONTINUE
      D = -0.4342945E0*ALOG(TOL)
      YB = 10.5E0 - 0.538460E0*(18.0E0-D)
      IF (ABS(Y).LT.YB) GO TO 30
C-----------------------------------------------------------------------
C     BACKWARD RECURRENCE EXTERIOR TO THE STRIP ABS(Y).LT.YB, X.LT.0.0
C-----------------------------------------------------------------------
      HTOL = 0.125E0*TOL
      CALL CEXENZ(Z, N, KODE, M, CY, IERR, RBRY, HTOL, ELIM, ALIM,
     * ICDIM, CA, CB)
      RETURN
   30 CONTINUE
C-----------------------------------------------------------------------
C     TAYLOR SERIES IN CACEXI FOR ANALYTIC CONTINUATION
C-----------------------------------------------------------------------
      CALL CACEXI(Z, N, KODE, M, CY, IERR, YB, RBRY, TOL, ELIM, ALIM,
     * ICDIM, CA, CB)
      RETURN
   40 CONTINUE
      DO 50 I=1,M
        CY(I) = CMPLX(1.0E0/FLOAT(N+I-2),0.0E0)
   50 CONTINUE
      RETURN
   60 CONTINUE
      IERR = 5
      RETURN
      END
      SUBROUTINE CEXENZ(Z, N, KODE, M, CY, IERR, RBRY, TOL, ELIM, ALIM,
     * ICDIM, CA, CB)
C***BEGIN PROLOGUE  CEXENZ
C***REFER TO  CEXINT
C
C      CEXENZ COMPUTES THE EXPONENTIAL INTEGRAL BY MEANS OF POWER SERIES
C      AND A BACKWARD RECURRENCE ALGORITHM FOR THE CONFLUENT HYPER-
C      GEOMETRIC REPRESENTATION.
C
C***ROUTINES CALLED PSIXN
C***END PROLOGUE  CEXENZ
C
      INTEGER I, IC, ICASE, ICDIM, ICMAX, ICT, IERR, IK, IND, IZ, JSET,
     * K, KFLAG, KN, KODE, KS, M, ML, MU, N, ND, NM
      REAL AA, AAM, AAMS, AEM, AH, AK, ALIM, AP1, AT, AZ, BK, BT, CA,
     * DK, ELIM, ERR, EULER, FC, FNM, RBRY, RTOLA, TOL, TOLA, X, XTOL,
     * Y, CK, PSIXN
      COMPLEX CP1, CP2, CPT, CAT, CBT, CY1, CY2, CY(M), CYY(2),
     * CNORM, CB, Z, CS, CAK, EMZ, CAA, TZ, FZ, CT, CZERO, CONE,
     * SCLE, RSCLE
      DIMENSION CA(ICDIM), CB(ICDIM)
      DATA EULER /-5.77215664901532861E-01/
      DATA CZERO, CONE  /(0.0E0,0.0E0), (1.0E0,0.0E0)/
      DATA ICMAX /2000/
      SCLE = CONE
      RSCLE = CONE
      X = REAL(Z)
      Y = AIMAG(Z)
      AZ = CABS(Z)
      IF (AZ.GT.RBRY) GO TO 80
C-----------------------------------------------------------------------
C     SERIES FOR E(N,Z) FOR CABS(Z).LE.RBRY
C-----------------------------------------------------------------------
      IZ = INT(AZ+0.5E0)
C-----------------------------------------------------------------------
C     ICASE=1 MEANS INTEGER CLOSEST TO CABS(Z) IS 2 AND N=1
C     ICASE=2 MEANS INTEGER CLOSEST TO CABS(Z) IS 0,1, OR 2 AND N.GE.2
C-----------------------------------------------------------------------
      ICASE = 2
      IF (IZ.GT.N) ICASE = 1
      NM = N - ICASE + 1
      ND = NM + 1
      IND = 3 - ICASE
      MU = M - IND
      ML = 1
      KS = ND
      FNM = FLOAT(NM)
      CS = CZERO
      XTOL = 0.3333E0*TOL
      AAM = 1.0E0
      IF (ND.EQ.1) GO TO 10
      AAM = 1.0E0/FNM
      CS = CMPLX(AAM,0.0E0)
   10 CONTINUE
      CAA = CONE
      AA = 1.0E0
      AK = 1.0E0
C-----------------------------------------------------------------------
C     LIMIT INDEX I TO IK TO PREVENT UNDERFLOW ON SMALL VALUES OF Z
C-----------------------------------------------------------------------
      IK = 35
      IF (AZ.LT.XTOL*AAM) IK = 1
      DO 50 I=1,IK
        AT=1.0E0/AK
        CAA = -CAA*Z*CMPLX(AT,0.0E0)
        AA=AA*AZ*AT
        IF (I.EQ.NM) GO TO 30
        CS = CS - CAA*CMPLX(1.0E0/(AK-FNM),0.0E0)
        GO TO 40
   30   CONTINUE
        CS = CS + CAA*(-CLOG(Z)+CMPLX(PSIXN(ND),0.0E0))
   40   CONTINUE
        IF (AA.LE.XTOL*CABS(CS)) GO TO 60
        AK = AK+1.0E0
   50 CONTINUE
   60 CONTINUE
      IF (ND.EQ.1) CS = CS + (-CLOG(Z)+CMPLX(EULER,0.0E0))
      IF (KODE.EQ.2) CS = CS*CEXP(Z)
      CY(1) = CS
      CT = CS
      EMZ = CONE
      IF (M.EQ.1) GO TO 70
      CY(IND) = CS
      AK = FLOAT(KS)
      IF (KODE.EQ.1) EMZ = CEXP(-Z)
      GO TO (300, 320), ICASE
   70 CONTINUE
      IF (ICASE.EQ.2) RETURN
      IF (KODE.EQ.1) EMZ = CEXP(-Z)
      CY(1) = (EMZ-CS)/Z
      RETURN
C-----------------------------------------------------------------------
C     BACKWARD RECURSIVE MILLER ALGORITHM FOR
C              E(N,Z)=EXP(-Z)*(Z**(N-1))*U(N,N,Z)
C     WITH RECURSION AWAY FROM N=INTEGER CLOSEST TO CABS(Z)
C     U(A,B,Z) IS THE SECOND CONFLUENT HYPERGEOMETRIC FUNCTION
C-----------------------------------------------------------------------
   80 CONTINUE
      EMZ = CONE
      IF (KODE.EQ.2) GO TO 140
C-----------------------------------------------------------------------
C     SCALE NEAR EXPONENT EXTREMES ON KODE=1
C-----------------------------------------------------------------------
      IF (X.LT.0.0E0) GO TO 90
      AT = X + FLOAT(N+M-1)
      CT = CMPLX(AT,Y)
      AA = CABS(CT)
      AT = X + ALOG(AA)
      IF (AT.GT.ELIM) GO TO 150
      KFLAG = 1
      GO TO 100
   90 CONTINUE
      AT = X
      IF (AT.LT.(-ELIM)) GO TO 340
      KFLAG = 2
  100 CONTINUE
      IF (ABS(AT).LT.ALIM) GO TO 130
      TOLA = EXP(ALIM-ELIM)
      RTOLA = 1.0E0/TOLA
      IF (KFLAG.EQ.2) GO TO 110
      SCLE = CMPLX(RTOLA,0.0E0)
      RSCLE = CMPLX(TOLA,0.0E0)
      GO TO 120
  110 CONTINUE
      SCLE = CMPLX(TOLA,0.0E0)
      RSCLE = CMPLX(RTOLA,0.0E0)
  120 CONTINUE
      EMZ = SCLE
  130 CONTINUE
      EMZ = EMZ*CEXP(-Z)
  140 CONTINUE
      IZ = INT(AZ+0.5E0)
      KN = N + M - 1
      IF (KN.LE.IZ) GO TO 170
      IF (N.LT.IZ .AND. IZ.LT.KN) GO TO 200
      IF (N.GE.IZ) GO TO 190
      IERR=7
      RETURN
  150 CONTINUE
      IERR = 2
      DO 160 I=1,M
        CY(I) = CZERO
  160 CONTINUE
      RETURN
  170 CONTINUE
      ICASE = 1
      KS = KN
      ML = M - 1
      MU = -1
      IND = M
      IF (KN.GT.1) GO TO 210
  180 CONTINUE
      KS = 2
      ICASE = 3
      GO TO 210
  190 CONTINUE
      ICASE = 2
      IND = 1
      KS = N
      MU = M - 1
      IF (N.GT.1) GO TO 210
      IF (KN.EQ.1) GO TO 180
      IZ = 2
  200 CONTINUE
      ICASE = 1
      KS = IZ
      ML = IZ - N
      IND = ML + 1
      MU = KN - IZ
  210 CONTINUE
      IK = KS/2
      AH = FLOAT(IK)
      JSET = 1 + KS - 2*IK
C-----------------------------------------------------------------------
C     START COMPUTATION FOR
C              CYY(1) = C*U( A , A ,Z)    JSET=1
C              CYY(1) = C*U(A+1,A+1,Z)    JSET=2
C     FOR AN EVEN INTEGER A.
C-----------------------------------------------------------------------
      IC = 0
      AA = AH + AH
      CAA = CMPLX(AA,0.0E0)
      AAM = AA - 1.0E0
      AAMS = AAM*AAM
      TZ = Z + Z
      FZ = TZ + TZ
      AK = AH
      XTOL = TOL
      CT = CMPLX(AAMS,0.0E0) + FZ*CMPLX(AH,0.0E0)
      CAK = Z + CAA
      AEM = (AK+1.0E0)/XTOL
      AEM = AEM/CABS(CAK)
      BK = AA
      CK = AH*AH
C-----------------------------------------------------------------------
C     FORWARD RECURSION FOR P(IC),P(IC+1) AND INDEX IC FOR BACKWARD
C     RECURSION
C-----------------------------------------------------------------------
      CP1 = CZERO
      CP2 = CONE
  220 CONTINUE
      IC = IC + 1
      IF (IC.GT.ICDIM) GO TO 230
      AK = AK + 1.0E0
      CK = CK + 1.0E0
      AT = BK/(BK+AK+CK)
      BK = BK + AK + AK
      CAT = CMPLX(AT,0.0E0)
      CA(IC) = AT
      BT = 1.0E0/(AK+1.0E0)
      CBT = (CMPLX(AK+AK,0.0E0)+Z)*CMPLX(BT)
      CB(IC) = CBT
      CPT = CP2
      CP2 = CBT*CP2 - CAT*CP1
      CP1 = CPT
      CT = CT + FZ
      AEM = AEM*AT
      BT = CABS(CT)
      ERR = AEM/SQRT(BT)
      AP1 = CABS(CP1)
      IF (ERR*(AK+1.0E0)/AP1.GT.AP1) GO TO 220
      GO TO 250
C-----------------------------------------------------------------------
C     CONTINUE FORWARD RECURRENCE UNINDEXED WHEN IC EXCEEDS ICDIM
C-----------------------------------------------------------------------
  230 CONTINUE
      IC = IC - 1
  240 CONTINUE
      IC = IC + 1
      IF (IC.GT.ICMAX) GO TO 350
      AK = AK + 1.0E0
      CK = CK + 1.0E0
      AT = BK/(BK+AK+CK)
      BK = BK + AK + AK
      CAT = CMPLX(AT,0.0E0)
      BT = 1.0E0/(AK+1.0E0)
      CBT = (CMPLX(AK+AK,0.0E0)+Z)*CMPLX(BT)
      CPT = CP2
      CP2 = CBT*CP2 - CAT*CP1
      CP1 = CPT
      CT = CT + FZ
      AEM = AEM*AT
      BT = CABS(CT)
      ERR = AEM/SQRT(BT)
      AP1 = CABS(CP1)
      IF (ERR*(AK+1.0E0)/AP1.GT.AP1) GO TO 240
  250 CONTINUE
      FC = FLOAT(IC)
      AT = ((FC+1.0E0)/(AK+1.0E0))*((AK+AH)/(AK+1.0E0))
      CAT = CMPLX(AT,0.0E0)*CSQRT(CT/(CT+FZ))
      CY2 = CAT*(CP1/CP2)
      CY1 = CONE
C-----------------------------------------------------------------------
C     BACKWARD RECURRENCE FOR
C             CY1=             C*U( A ,A,Z)
C             CY2= C*(A/(1+A/2))*U(A+1,A,Z)
C-----------------------------------------------------------------------
      IF (IC.LE.ICDIM) GO TO 270
C-----------------------------------------------------------------------
C     BACKWARD RECUR UNINDEXED WHEN IC EXCEEDS ICDIM
C-----------------------------------------------------------------------
      BT = AA + X
  260 CONTINUE
      AK = AH + FC
      BK = AK + 1.0E0
      CK = AAM + FC
      DK = BT + FC + FC
      AT = (AK/FC)*(BK/CK)
      CBT = CMPLX(DK/BK,Y/BK)
      CPT = CY1
      CY1 = (CBT*CY1-CY2)*CMPLX(AT,0.0E0)
      CY2 = CPT
      FC = FC - 1.0E0
      IC = IC - 1
      IF (IC.GT.ICDIM) GO TO 260
  270 CONTINUE
      ICT = IC
      DO 280 K=1,ICT
        AT = 1.0E0/CA(IC)
        CPT = CY1
        CY1 = (CB(IC)*CY1-CY2)*CMPLX(AT,0.0E0)
        CY2 = CPT
        IC = IC - 1
  280 CONTINUE
C-----------------------------------------------------------------------
C     THE CONTIGUOUS RELATION
C              Z*U(B,C+1,Z)=(C-B)*U(B,C,Z)+U(B-1,C,Z)
C     WITH  B=A+1 , C=A IS USED FOR
C             CYY(2) = C * U(A+1,A+1,Z)
C     Z IS INCORPORATED INTO THE NORMALIZING RELATION FOR CNORM.
C-----------------------------------------------------------------------
      CPT = CY2/CY1
      CNORM = CONE - CPT*CMPLX((AH+1.0E0)/AA,0.0E0)
      CYY(1) = CONE/(CNORM*CAA+Z)
      CYY(2) = CNORM*CYY(1)
      IF (ICASE.EQ.3) GO TO 290
      CT = EMZ*CYY(JSET)
      CY(IND) = CT*RSCLE
      CS = CT
      IF (M.EQ.1) RETURN
      AK = FLOAT(KS)
      GO TO (300, 320), ICASE
C-----------------------------------------------------------------------
C     RECURSION SECTION  N*E(N+1,Z) + Z*E(N,Z)=EMZ
C-----------------------------------------------------------------------
  290 CONTINUE
      CT = EMZ*(CONE-CYY(1))/Z
      CY(1) = CT*RSCLE
      RETURN
  300 CONTINUE
      CAA = CMPLX(AK,0.0E0)
      TZ = CONE/Z
      K = IND - 1
      DO 310 I=1,ML
        CAA = CAA - CONE
        CT = (EMZ-CAA*CT)*TZ
        CY(K) = CT*RSCLE
        K = K - 1
  310 CONTINUE
      IF (MU.LE.0) RETURN
      AK = FLOAT(KS)
  320 CONTINUE
      K = IND
      DO 330 I=1,MU
        CS = (EMZ-Z*CS)*CMPLX(1.0E0/AK,0.0E0)
        CY(K+1) = CS*RSCLE
        AK = AK + 1.0E0
        K = K + 1
  330 CONTINUE
      RETURN
  340 CONTINUE
      IERR = 3
      RETURN
  350 CONTINUE
      IERR = 6
      RETURN
      END
      SUBROUTINE CACEXI(Z, NU, KODE, N, Y, IERR, YB, RBRY, TOL, ELIM,
     * ALIM, ICDIM, CA, CB)
C***BEGIN PROLOGUE  CACEXI
C***REFER TO  CEXINT
C
C     CACEXI COMPUTES THE ANALYTIC CONTINUATION OF THE EXPONENTIAL
C     INTEGRAL FOR X.LT.0 AND -YB.LT.Y.LT.YB BY TAYLOR SERIES IN
C     INCREMENTS OF HALF A UNIT. THE CONTINUATION PROCEEDS
C     VERTICALLY DOWN FROM ZB=CMPLX(X,YB) TO Z=CMPLX(X,Y) FOR 0.0.LE.
C     Y.LT.YB. CONJUGATION IS USED FOR Y.LT.0.0E0.
C
C***ROUTINES CALLED  CEXENZ
C***END PROLOGUE  CACEXI
C
      INTEGER I, IAZ, ICDIM, IERR, IL, IS, IY, K, KB,  KL, KMAX,
     * KODE, KS, KYB, N, NB, NFLG, NU, NUB
      REAL ALIM, AZ, CA, DEL, ELIM, FK, RBRY, RTOLA, TOL, TOLA, YB,
     * YT, ZI, ZID, ZR, XTOL, RZI, ATRM, FJ, RW, RQ(64), ASUM, HTOL
      COMPLEX Y(N), YY(1), CONE, CEX, SUM, TRM, Z, ZZ, ZP(64), ZT, SCLE,
     * RSCLE, TRMS, CB, ZW, CEZT
      DIMENSION CA(ICDIM), CB(ICDIM)
      DATA CONE /(1.0E0,0.0E0)/
      SCLE = CONE
      RSCLE = CONE
      ZR = REAL(Z)
      ZI = AIMAG(Z)
      ZID = ZI
      KYB = 0
      IF (ZI.LT.0.0E0) ZID = -ZID
      AZ = CABS(Z)
      IAZ = INT(AZ+0.5E0)
C-----------------------------------------------------------------------
C     SET ORDER NUB=MIN0(N+M-1,INT(CABS(Z)+0.5)), GENERATE REMAINDER
C     OF THE SEQUENCE AFTER THE COMPUTATION OF E(NUB,Z)
C-----------------------------------------------------------------------
      IF (NU.GE.IAZ) GO TO 10
      IF (NU+N-1.LE.IAZ) GO TO 20
      NUB = IAZ
      NB = 0
      NFLG = 3
      KB = NUB - NU + 1
      KS = KB + 1
      KL = N
      IS = 1
      IL = KB - 1
      GO TO 40
   10 CONTINUE
      NUB = MAX0(IAZ,1)
      NB = NU - NUB
      NFLG = 1
      KB = 1
      KS = 2
      KL = N
      GO TO 40
   20 CONTINUE
      NUB = NU + N - 1
      NB = 0
      NFLG = 2
      IS = 2
      IL = N
      KB = N
      GO TO 40
C-----------------------------------------------------------------------
C     SET PARAMETERS FOR ANALYTIC CONTINUATION FROM Y=YB INTO THE REGION
C     0.LE.ZID.LT.YB.
C-----------------------------------------------------------------------
   30 CONTINUE
      YB = YB + 0.5E0
      KYB = KYB + 1
      IF (KYB.GT.10) RETURN
      IERR = 0
   40 CONTINUE
      DEL = YB - ZID
C-----------------------------------------------------------------------
C     MAKE DEL LARGE ENOUGH TO AVOID UNDERFLOW IN GENERATION OF POWERS
C-----------------------------------------------------------------------
      IF(ABS(DEL).GT.1.0E-4) GO TO 50
      YB = YB + 1.0E-4
      DEL = YB - ZID
   50 CONTINUE
      HTOL = 0.125E0*TOL
      ZZ = CMPLX(ZR,YB)
      CALL CEXENZ(ZZ,NUB,2,1,YY,IERR,RBRY,HTOL,ELIM,ALIM,ICDIM,CA,CB)
      IF(IERR.EQ.6) GO TO 30
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION VIA TAYLOR SERIES FOR ORDER NUB
C-----------------------------------------------------------------------
      IY=INT(DEL+DEL)
      YT=DEL/FLOAT(IY+1)
      SUM=YY(1)
      HTOL=0.25E0*TOL
      TRM=SUM
      CEZT=CMPLX(COS(YT),-SIN(YT))
      ZW=CONE
      ZP(1)=CONE
      FK=1.0E0
      FJ=FLOAT(NUB-1)
      ZT=CONE/ZZ
C-----------------------------------------------------------------------
C     TERMS OF THE SERIES TRM=E(NUB-K,ZZ)*(YT**K)/K!,  K=0,1,... ARE
C     GENERATED BY BACKWARD RECURRENCE. E IS SCALED BY CEXP(ZZ).
C-----------------------------------------------------------------------
      DO 60 K=2,64
        RW=YT/FK
        ZW=ZW*CMPLX(0.0E0,RW)
        RW = FJ*RW
        TRM=(ZW-CMPLX(0.0E0,RW)*TRM)*ZT
        SUM=SUM+TRM
        ZP(K)=ZW
        RQ(K)=RW
        ASUM=CABS(SUM)
        ATRM=CABS(TRM)
        IF(ATRM.LT.HTOL*ASUM) GO TO 70
        FK=FK+1.0E0
        FJ=FJ-1.0E0
   60 CONTINUE
      K=64
   70 CONTINUE
      KMAX=K
      SUM=SUM*CEZT
      IF(IY.EQ.0) GO TO 100
      DO 90 I=1,IY
        RZI=FLOAT(IY-I+1)*YT+ZID
        ZZ=CMPLX(ZR,RZI)
        ZT=CONE/ZZ
        TRM=SUM
        DO 80 K=2,KMAX
          TRM=(ZP(K)-CMPLX(0.0E0,RQ(K))*TRM)*ZT
          SUM=SUM+TRM
   80   CONTINUE
        ATRM=CABS(TRM)
        ASUM=CABS(SUM)
        XTOL=HTOL*ASUM
        IF(ATRM.LT.XTOL) GO TO 86
        IF(KMAX.GE.64) GO TO 86
        KMAX=KMAX+1
        DO 84 K=KMAX,64
          RW=YT/FK
          ZW=ZW*CMPLX(0.0E0,RW)
          RW=FJ*RW
          TRM=(ZW-CMPLX(0.0E0,RW)*TRM)*ZT
          SUM=SUM+TRM
          ZP(K)=ZW
          RQ(K)=RW
          ATRM=CABS(TRM)
          IF(ATRM.LT.XTOL) GO TO 85
          FK=FK+1.0E0
          FJ=FJ-1.0E0
   84   CONTINUE
        K=64
   85   CONTINUE
        KMAX=K
   86   CONTINUE
        SUM=SUM*CEZT
   90 CONTINUE
  100 CONTINUE
C-----------------------------------------------------------------------
C     FORM SEQUENCE UP OR DOWN FROM ORDER NUB
C-----------------------------------------------------------------------
      IF (ZI.LT.0.0E0) SUM = CONJG(SUM)
      CEX = CONE
C-----------------------------------------------------------------------
C     SCALE NEAR OVERFLOW LIMIT ON KODE=1
C-----------------------------------------------------------------------
      IF (KODE.EQ.2) GO TO 160
      IF (ABS(ZR).LT.ALIM) GO TO 150
      IF (ABS(ZR).GT.ELIM) GO TO 220
      TOLA = EXP(ALIM-ELIM)
      RTOLA = 1.0E0/TOLA
      SCLE = CMPLX(TOLA,0.0E0)
      RSCLE = CMPLX(RTOLA,0.0E0)
  150 CONTINUE
      CEX = SCLE*CEXP(-Z)
  160 CONTINUE
      TRM = SUM*CEX
      Y(KB) = TRM*RSCLE
      TRMS = TRM
      IF (N.EQ.1 .AND. NFLG.NE.1) RETURN
      IF (NFLG.EQ.2) GO TO 200
      FK = FLOAT(NUB)
      IF (NFLG.NE.1 .OR. NB.EQ.0) GO TO 180
      DO 170 K=1,NB
        TRM = (CEX-Z*TRM)*CMPLX(1.0E0/FK,0.0E0)
        FK = FK + 1.0E0
  170 CONTINUE
  180 CONTINUE
      Y(KB) = TRM*RSCLE
      TRMS = TRM
      IF (N.EQ.1) RETURN
      DO 190 K=KS,KL
        TRM = (CEX-Z*TRM)*CMPLX(1.0E0/FK,0.0E0)
        Y(K) = TRM*RSCLE
        FK = FK + 1.0E0
  190 CONTINUE
      IF (NFLG.EQ.1) RETURN
  200 CONTINUE
      K = KB - 1
      FK = FLOAT(NUB-1)
      ZT=CONE/Z
      DO 210 I=IS,IL
        TRMS = (CEX-CMPLX(FK,0.0E0)*TRMS)*ZT
        Y(K) = TRMS*RSCLE
        K = K - 1
        FK = FK - 1.0E0
  210 CONTINUE
      RETURN
  220 CONTINUE
      IERR = 3
      RETURN
      END
      FUNCTION PSIXN(N)
C***BEGIN PROLOGUE  PSIXN
C***REFER TO  EXINT,BSKIN
C
C     THIS SUBROUTINE RETURNS VALUES OF PSI(X)=DERIVATIVE OF LOG
C     GAMMA(X), X.GT.0.0 AT INTEGER ARGUMENTS. A TABLE LOOK-UP IS
C     PERFORMED FOR N.LE.100, AND THE ASYMPTOTIC EXPANSION IS
C     EVALUATED FOR N.GT.100.
C
C***ROUTINES CALLED  R1MACH
C***END PROLOGUE  PSIXN
C
      INTEGER N, K
      REAL AX, B, C, FN, RFN2, TRM, S, WDTOL
      REAL R1MACH, PSIXN
      DIMENSION B(6), C(100)
C-----------------------------------------------------------------------
C             PSIXN(N), N = 1,100
C-----------------------------------------------------------------------
      DATA C(1), C(2), C(3), C(4), C(5), C(6), C(7), C(8), C(9), C(10),
     1     C(11), C(12), C(13), C(14), C(15), C(16), C(17), C(18),
     2     C(19), C(20), C(21), C(22), C(23), C(24)/
     3    -5.77215664901532861E-01,     4.22784335098467139E-01,
     4     9.22784335098467139E-01,     1.25611766843180047E+00,
     5     1.50611766843180047E+00,     1.70611766843180047E+00,
     6     1.87278433509846714E+00,     2.01564147795561000E+00,
     7     2.14064147795561000E+00,     2.25175258906672111E+00,
     8     2.35175258906672111E+00,     2.44266167997581202E+00,
     9     2.52599501330914535E+00,     2.60291809023222227E+00,
     A     2.67434666166079370E+00,     2.74101332832746037E+00,
     B     2.80351332832746037E+00,     2.86233685773922507E+00,
     C     2.91789241329478063E+00,     2.97052399224214905E+00,
     D     3.02052399224214905E+00,     3.06814303986119667E+00,
     E     3.11359758531574212E+00,     3.15707584618530734E+00/
      DATA C(25), C(26), C(27), C(28), C(29), C(30), C(31), C(32),
     1     C(33), C(34), C(35), C(36), C(37), C(38), C(39), C(40),
     2     C(41), C(42), C(43), C(44), C(45), C(46), C(47), C(48)/
     3     3.19874251285197401E+00,     3.23874251285197401E+00,
     4     3.27720405131351247E+00,     3.31424108835054951E+00,
     5     3.34995537406483522E+00,     3.38443813268552488E+00,
     6     3.41777146601885821E+00,     3.45002953053498724E+00,
     7     3.48127953053498724E+00,     3.51158256083801755E+00,
     8     3.54099432554389990E+00,     3.56956575411532847E+00,
     9     3.59734353189310625E+00,     3.62437055892013327E+00,
     A     3.65068634839381748E+00,     3.67632737403484313E+00,
     B     3.70132737403484313E+00,     3.72571761793728215E+00,
     C     3.74952714174680596E+00,     3.77278295570029433E+00,
     D     3.79551022842756706E+00,     3.81773245064978928E+00,
     E     3.83947158108457189E+00,     3.86074817682925274E+00/
      DATA C(49), C(50), C(51), C(52), C(53), C(54), C(55), C(56),
     1     C(57), C(58), C(59), C(60), C(61), C(62), C(63), C(64),
     2     C(65), C(66), C(67), C(68), C(69), C(70), C(71), C(72)/
     3     3.88158151016258607E+00,     3.90198967342789220E+00,
     4     3.92198967342789220E+00,     3.94159751656514710E+00,
     5     3.96082828579591633E+00,     3.97969621032421822E+00,
     6     3.99821472884273674E+00,     4.01639654702455492E+00,
     7     4.03425368988169777E+00,     4.05179754953082058E+00,
     8     4.06903892884116541E+00,     4.08598808138353829E+00,
     9     4.10265474805020496E+00,     4.11904819067315578E+00,
     A     4.13517722293122029E+00,     4.15105023880423617E+00,
     B     4.16667523880423617E+00,     4.18205985418885155E+00,
     C     4.19721136934036670E+00,     4.21213674247469506E+00,
     D     4.22684262482763624E+00,     4.24133537845082464E+00,
     E     4.25562109273653893E+00,     4.26970559977879245E+00/
      DATA C(73), C(74), C(75), C(76), C(77), C(78), C(79), C(80),
     1     C(81), C(82), C(83), C(84), C(85), C(86), C(87), C(88),
     2     C(89), C(90), C(91), C(92), C(93), C(94), C(95), C(96)/
     3     4.28359448866768134E+00,     4.29729311880466764E+00,
     4     4.31080663231818115E+00,     4.32413996565151449E+00,
     5     4.33729786038835659E+00,     4.35028487337536958E+00,
     6     4.36310538619588240E+00,     4.37576361404398366E+00,
     7     4.38826361404398366E+00,     4.40060929305632934E+00,
     8     4.41280441500754886E+00,     4.42485260777863319E+00,
     9     4.43675736968339510E+00,     4.44852207556574804E+00,
     A     4.46014998254249223E+00,     4.47164423541605544E+00,
     B     4.48300787177969181E+00,     4.49424382683587158E+00,
     C     4.50535493794698269E+00,     4.51634394893599368E+00,
     D     4.52721351415338499E+00,     4.53796620232542800E+00,
     E     4.54860450019776842E+00,     4.55913081598724211E+00/
      DATA C(97), C(98), C(99), C(100)/
     1     4.56954748265390877E+00,     4.57985676100442424E+00,
     2     4.59006084263707730E+00,     4.60016185273808740E+00/
C-----------------------------------------------------------------------
C             COEFFICIENTS OF ASYMPTOTIC EXPANSION
C-----------------------------------------------------------------------
      DATA B(1), B(2), B(3), B(4), B(5), B(6)/
     1     8.33333333333333333E-02,    -8.33333333333333333E-03,
     2     3.96825396825396825E-03,    -4.16666666666666666E-03,
     3     7.57575757575757576E-03,    -2.10927960927960928E-02/
C
      IF (N.GT.100) GO TO 10
      PSIXN = C(N)
      RETURN
   10 CONTINUE
      WDTOL = AMAX1(R1MACH(4),1.0E-18)
      FN = FLOAT(N)
      AX = 1.0E0
      S = -0.5E0/FN
      IF (ABS(S).LE.WDTOL) GO TO 30
      RFN2 = 1.0E0/(FN*FN)
      DO 20 K=1,6
        AX = AX*RFN2
        TRM = -B(K)*AX
        IF (ABS(TRM).LT.WDTOL) GO TO 30
        S = S + TRM
   20 CONTINUE
   30 CONTINUE
      PSIXN = S + ALOG(FN)
      RETURN
      END
      PROGRAM ZQCCEX
C
C     BEFORE THIS PROGRAM CAN BE EXECUTED, YOUR MACHINE MUST BE DEFINED
C     IN SUBROUTINES I1MACH AND D1MACH.
C
C     ZQCCEX IS A QUICK CHECK PROGRAM TO COMPARE EXPONENTIAL INTEGRALS
C     E(N,Z) FROM SUBROUTINE ZEXINT,
C
C               CALL ZEXINT(ZR,ZI,N,KODE,TOL,M,CYR,CYI,IERR)
C
C     AGAINST EXPONENTIAL INTEGRALS FROM QUADRATURE SUBROUTINE ZEXQAD,
C
C               CALL ZEXQAD(ZR,ZI,N,KODE,TOL,CQR,CQI,KERR)
C
C     WHERE A COMPLEX NUMBER Z=(ZR,ZI) IS CARRIED AS A DOUBLE PRECISION
C     ORDERED PAIR, ZR,ZI.
C
C     Z VALUES ARE TAKEN FROM THE REGION -6.5.LE.X.LT.5.5,-6.LE.Y.LE.6.
C     ORDERS N RUN FROM 3 TO 11 AND THE NUMBER OF MEMBERS M IN THE
C     SEQUENCE E(N+K-1,Z), K=1,M RUNS FROM 1 TO 3. BOTH SCALING OPTIONS
C
C                CY(K) = E(N+K-1,Z)                K=1,M     KODE=1
C                        E(N+K-1,Z)*CEXP(Z)        K=1,M     KODE=2
C
C     ARE CHECKED AND THE REQUESTED ACCURACY TOL IS THE LARGER OF
C     UNIT ROUNDOFF AND 1.0D-9. RELATIVE ERRORS ERR1 AND ERR2 FOR THE
C     FIRST AND LAST MEMBERS OF THE SEQUENCE ARE COMPUTED AND COMPARED
C     AGAINST 100.0*TOL. IF A CHECK DOES NOT OCCUR, Z,ERR1,ERR2,N,M,KODE
C     AND ERROR FLAGS IERR FROM ZEXINT, KERR1 FROM ZEXQAD, AND KERR2
C     FROM ZEXQAD ARE PRINTED. VALUES CY(1),CQ1 AND CY(N+M-1),CQ2 WHICH
C     WERE COMPARED IN ERR1 AND ERR2 ARE PRINTED NEXT. KERR1.NE.0 OR
C     KERR2.NE.0 INDICATE A PREMATURE TRUNCATION OF THE INTEGRAL EVAL-
C     UATION IN ZEXQAD. THE SUFFIXES 1 AND 2 CORRESPOND TO EVALUATIONS
C     AT ORDERS N AND N+M-1 RESPECTIVELY.
C
C     CQCCEX CALLS ZEXINT,ZEXQAD AND LOWER LEVEL ROUTINES ZEXENZ,ZACEXI,
C     DPSIXN,ZEXQAD,DGAUS8,DFQCEX,ZMLT,ZDIV,ZSQRT,ZLOG,ZEXP,ZABS,I1MACH,
C     D1MACH,XERROR,FDUMP
C
C     THE CORRESPONDING SINGLE PRECISION COMPLEX VARIABLES IN CQCCEX
C     ARE:
C     COMPLEX Z,CY(10),CQ1,CQ2
      EXTERNAL ZABS
      DOUBLE PRECISION ZR,ZI,CYR(10),CYI(10),CQ1R,CQ1I,CQ2R,CQ2I,
     * TOL,XTOL,ERR1R,ERR1I,ERR2R,ERR2I,ERR1,ERR2,D1MACH,ZABS
      IPRNT = 0
      TOL=DMAX1(1.0D-9,D1MACH(4))
      XTOL=TOL*100.0D0
      PRINT 700
  700 FORMAT(' ZEXINT VS QUADRATURE FOR PARAMETERS:'/)
      PRINT 701
  701 FORMAT('  KODE   M    N       Y        -6.5.LE.X.LE.5.5')
      DO 10 KODE=1,2
        DO 20 M=1,3
          DO 30 N=3,11,2
            DO 40 IY=1,13,3
              ZI=-6.0D0+DBLE(FLOAT(IY-1))
              PRINT 702,KODE,M,N,ZI
  702         FORMAT(I5,I5,I5,E13.5)
              DO 50 IX=2,14,4
                ZR=-7.5D0+DBLE(FLOAT(IX-1))
                IF(ZI.EQ.0.0D0 .AND. ZR.LE.0.0D0) GO TO 50
                CALL ZEXINT(ZR,ZI,N,KODE,TOL,M,CYR,CYI,IERR)
                NB=N
                CALL ZEXQAD(ZR,ZI,NB,KODE,TOL,CQ1R,CQ1I,KERR1)
                ERR1R=CYR(1)-CQ1R
                ERR1I=CYI(1)-CQ1I
                ERR1=ZABS(ERR1R,ERR1I)/ZABS(CQ1R,CQ1I)
                NB=N+M-1
                CALL ZEXQAD(ZR,ZI,NB,KODE,TOL,CQ2R,CQ2I,KERR2)
                ERR2R=CYR(M)-CQ2R
                ERR2I=CYI(M)-CQ2I
                ERR2=ZABS(ERR2R,ERR2I)/ZABS(CQ2R,CQ2I)
                IF(ERR1.LT.XTOL.AND.ERR2.LT.XTOL) GO TO 45
                IF(IPRNT.EQ.1) GO TO 42
                IPRNT=1
                OPEN (7,FILE='ZEXDAT7',STATUS='UNKNOWN')
                WRITE(7,800)
  800           FORMAT(' ZR,ZI,ERR1,ERR2,N,M,KODE,IERR,KERR1,KERR2')
   42           CONTINUE
                WRITE(7,900) ZR,ZI,ERR1,ERR2,N,M,KODE,IERR,KERR1,KERR2
                WRITE(7,900) CYR(1),CYI(1),CQ1R,CQ1I
                WRITE(7,900) CYR(M),CYI(M),CQ2R,CQ2I
  900           FORMAT(4D11.3,6I5)
   45           CONTINUE
   50         CONTINUE
   40       CONTINUE
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
      IF(IPRNT.NE.0) GO TO 60
      PRINT 950
  950 FORMAT(/' QUICK CHECKS FOR ZEXINT ARE OK.')
      GO TO 70
   60 CONTINUE
      PRINT 951
  951 FORMAT(/' SEE DATA FILE ZEXDAT7 FOR ERRORS')
   70 CONTINUE
      END
      SUBROUTINE ZEXQAD(ZR,ZI,N,KODE,TOL,CWR,CWI,KERR)
C
C     ZEXQAD COMPUTES EXPONENTIAL INTEGRALS E(N,Z) OF A COMPLEX ARGUMENT
C     Z BY QUADRATURE ON  (T**(N-1)*EXP(-T)/(Z+T))/(N-1)! FROM T=0 TO
C     T=INFINITY. KODE=1 RETURNS CW=E(N,Z) WHILE KODE=2 RETURNS
C     CW=E(N,Z)*CEXP(Z). TOL IS THE REQUESTED RELATIVE ERROR AND
C     KERR.NE.0 IS AN ERROR FLAG INDICATING A PREMATURE TRUNCATION OF
C     AN INTEGRAL IN ZEXQAD. THE QUADRATURE IS DONE AS TWO REAL
C     INTEGRALS SINCE Z IS COMPLEX. Z IS CARRIED AS DOUBLE PRECISION
C     ORDERED PAIRS ZR,ZI AND RETURNS DOUBLE PRECISION PAIRS CWR,CWI.
C
      COMMON/QAD/FNM,GLN,X,Y,IFLAG
      EXTERNAL DFQCEX
C     THE CORRESPONDING SINGLE PRECISION COMPLEX VARIABLES IN CEXQAD
C     ARE:
C     COMPLEX Z,CW
      DOUBLE PRECISION ZR,ZI,CWR,CWI,TOL,XTOL,FN,FNM,GM,GLN,A,B,
     * ANS,S1,S2,X,Y,DFQCEX
      FN=DBLE(FLOAT(N))
      FNM=FN-1.0D0
      GM=1.0D0
      IF(N.EQ.1) GO TO 6
      DO 5 I=2,N
        GM=GM*DBLE(FLOAT(I-1))
    5 CONTINUE
    6 CONTINUE
      GLN=DLOG(GM)
      KERR=0
      X=ZR
      Y=ZI
C-----------------------------------------------------------------------
C     REAL PART OF E(N,Z), IFLAG = 1
C-----------------------------------------------------------------------
      IFLAG=1
      S1=0.0D0
      B=0.0D0
      DO 10 I=1,100
        A=B
        B=B+5.0D0
        XTOL=TOL
        CALL DGAUS8(DFQCEX,A,B,XTOL,ANS,IERR)
        S1=S1+ANS
        IF(DABS(ANS).LT.DABS(S1)*TOL) GO TO 20
   10 CONTINUE
      KERR=1
   20 CONTINUE
C-----------------------------------------------------------------------
C     IMAGINARY PART OF E(N,Z), IFLAG = 2
C-----------------------------------------------------------------------
      IFLAG=2
      S2=0.0D0
      IF(ZI.EQ.0.0D0) GO TO 42
      B=0.0D0
      DO 30 I=1,100
        A=B
        B=B+5.0D0
        XTOL=TOL
        CALL DGAUS8(DFQCEX,A,B,XTOL,ANS,IERR)
        S2=S2+ANS
        IF(DABS(ANS).LT.DABS(S2)*TOL) GO TO 40
   30 CONTINUE
      KERR=2
   40 CONTINUE
   42 CONTINUE
      CWR=S1
      CWI=-S2
      IF(KODE.EQ.2) GO TO 45
      CALL ZEXP(-ZR,-ZI,S1,S2)
      ANS=S1*CWR-S2*CWI
      CWI=S1*CWI+S2*CWR
      CWR=ANS
   45 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DFQCEX(T)
      COMMON/QAD/FNM,GLN,X,Y,IFLAG
      DOUBLE PRECISION A,B,FNM,GLN,X,Y,T
      A=-T+FNM*DLOG(T)-GLN
      A=DEXP(A)
      IF(IFLAG.EQ.2) GO TO 10
      B=(X+T)/((X+T)**2+Y**2)
      GO TO 20
   10 CONTINUE
      B=Y/((X+T)**2+Y**2)
   20 CONTINUE
      DFQCEX=A*B
      RETURN
      END
      SUBROUTINE DGAUS8(FUN, A, B, ERR, ANS, IERR)
C
C     WRITTEN BY R.E. JONES
C
C     ABSTRACT  *** A DOUBLE PRECISION ROUTINE ***
C        DGAUS8 INTEGRATES REAL FUNCTIONS OF ONE VARIABLE OVER FINITE
C        INTERVALS USING AN ADAPTIVE 8-POINT LEGENDRE-GAUSS ALGORITHM.
C        DGAUS8 IS INTENDED PRIMARILY FOR HIGH ACCURACY INTEGRATION
C        OR INTEGRATION OF SMOOTH FUNCTIONS.
C
C        THE MAXIMUM NUMBER OF SIGNIFICANT DIGITS OBTAINABLE IN ANS
C        IS THE SMALLER OF 18 AND THE NUMBER OF DIGITS CARRIED IN
C        DOUBLE PRECISION ARITHMETIC.
C
C        DGAUS8 CALLS I1MACH, D1MACH, XERROR
C
C     DESCRIPTION OF ARGUMENTS
C
C        INPUT--* FUN,A,B,ERR ARE DOUBLE PRECISION *
C        FUN - NAME OF EXTERNAL FUNCTION TO BE INTEGRATED. THIS NAME
C              MUST BE IN AN EXTERNAL STATEMENT IN THE CALLING PROGRAM.
C              FUN MUST BE A DOUBLE PRECISION FUNCTION OF ONE DOUBLE
C              PRECISION ARGUMENT. THE VALUE OF THE ARGUMENT TO FUN
C              IS THE VARIABLE OF INTEGRATION WHICH RANGES FROM A TO
C              B.
C        A   - LOWER LIMIT OF INTEGRAL
C        B   - UPPER LIMIT OF INTEGRAL (MAY BE LESS THAN A)
C        ERR - IS A REQUESTED PSEUDORELATIVE ERROR TOLERANCE.  NORMALLY
C              PICK A VALUE OF DABS(ERR) SO THAT DTOL.LT.DABS(ERR).LE.
C              1.0D-3 WHERE DTOL IS THE LARGER OF 1.0D-18 AND THE
C              DOUBLE PRECISION UNIT ROUNDOFF = D1MACH(4). ANS WILL
C              NORMALLY HAVE NO MORE ERROR THAN DABS(ERR) TIMES THE
C              INTEGRAL OF THE ABSOLUTE VALUE OF FUN(X). USUALLY,
C              SMALLER VALUES OF ERR YIELD MORE ACCURACY AND REQUIRE
C              MORE FUNCTION EVALUATIONS.
C
C              A NEGATIVE VALUE FOR ERR CAUSES AN ESTIMATE OF THE
C              ABSOLUTE ERROR IN ANS TO BE RETURNED IN ERR. NOTE THAT
C              ERR MUST BE A VARIABLE (NOT A CONSTANT) IN THIS CASE.
C              NOTE ALSO THAT THE USER MUST RESET THE VALUE OF ERR
C              BEFORE MAKING ANY MORE CALLS THAT USE THE VARIABLE ERR.
C
C        OUTPUT--* ERR,ANS ARE DOUBLE PRECISION *
C        ERR - WILL BE AN ESTIMATE OF THE ABSOLUTE ERROR IN ANS IF THE
C              INPUT VALUE OF ERR WAS NEGATIVE.  (ERR IS UNCHANGED IF
C              THE INPUT VALUE OF ERR WAS NONNEGATIVE.)  THE ESTIMATED
C              ERROR IS SOLELY FOR INFORMATION TO THE USER AND SHOULD
C              NOT BE USED AS A CORRECTION TO THE COMPUTED INTEGRAL.
C        ANS - COMPUTED VALUE OF INTEGRAL
C        IERR- A STATUS CODE
C            --NORMAL CODES
C               1 ANS MOST LIKELY MEETS REQUESTED ERROR TOLERANCE,
C                 OR A=B.
C              -1 A AND B ARE TOO NEARLY EQUAL TO ALLOW NORMAL
C                 INTEGRATION.  ANS IS SET TO ZERO.
C            --ABNORMAL CODE
C               2 ANS PROBABLY DOES NOT MEET REQUESTED ERROR TOLERANCE.
C***END PROLOGUE
      EXTERNAL FUN
      INTEGER ICALL, IERR, K, KML, KMX, L, LMN, LMX, LR, MXL, NBITS,
     * NIB, NLMN, NLMX
      INTEGER I1MACH
      DOUBLE PRECISION A,AA,AE,ANIB,ANS,AREA,B,C,CE,DTOL,EE,EF,
     * EPS, ERR, EST, GL, GLR, GR, HH, SQ2, TOL, VL, VR, W1, W2, W3,
     * W4, X1, X2, X3, X4, X, H
      DOUBLE PRECISION D1MACH, G8, FUN
      DIMENSION AA(30), HH(30), LR(30), VL(30), GR(30)
      DATA X1, X2, X3, X4/
     1     1.83434642495649805D-01,     5.25532409916328986D-01,
     2     7.96666477413626740D-01,     9.60289856497536232D-01/
      DATA W1, W2, W3, W4/
     1     3.62683783378361983D-01,     3.13706645877887287D-01,
     2     2.22381034453374471D-01,     1.01228536290376259D-01/
      DATA ICALL  /  0  /
      DATA SQ2/1.41421356D0/
      DATA NLMN/1/,KMX/5000/,KML/6/
      G8(X,H)=H*((W1*(FUN(X-X1*H) + FUN(X+X1*H))
     1           +W2*(FUN(X-X2*H) + FUN(X+X2*H)))
     2          +(W3*(FUN(X-X3*H) + FUN(X+X3*H))
     3           +W4*(FUN(X-X4*H) + FUN(X+X4*H))))
C
C     INITIALIZE
C
      IF (ICALL.NE.0) CALL XERROR('DGAUS8- DGAUS8 CALLED RECURSIVELY. RE
     *CURSIVE CALLS ARE ILLEGAL IN FORTRAN.',74, 7, 2)
      ICALL = 1
      DTOL=D1MACH(4)
      DTOL=DMAX1(DTOL,1.0D-18)
      IF(DABS(ERR).LT.DTOL) GO TO 150
      K = I1MACH(14)
      ANIB = D1MACH(5)*DBLE(FLOAT(K))/0.30102000D0
      NBITS = INT(SNGL(ANIB))
      NLMX = (NBITS*5)/8
      ANS = 0.0D0
      IERR = 1
      CE = 0.0D0
      IF (A.EQ.B) GO TO 140
      LMX = NLMX
      LMN = NLMN
      IF (B.EQ.0.0D0) GO TO 10
      IF (DSIGN(1.0D0,B)*A.LE.0.0D0) GO TO 10
      C = DABS(1.0D0-A/B)
      IF (C.GT.0.1D0) GO TO 10
      IF (C.LE.0.0D0) GO TO 140
      ANIB = 0.5D0 - DLOG(C)/0.69314718D0
      NIB = INT(SNGL(ANIB))
      LMX = MIN0(NLMX,NBITS-NIB-7)
      IF (LMX.LT.1) GO TO 130
      LMN = MIN0(LMN,LMX)
   10 TOL = DMAX1(DABS(ERR),2.0D0**(5-NBITS))/2.0D0
      IF (ERR.EQ.0.0D0) TOL = DSQRT(D1MACH(4))
      EPS = TOL
      HH(1) = (B-A)/4.0D0
      AA(1) = A
      LR(1) = 1
      L = 1
      EST = G8(AA(L)+2.0D0*HH(L),2.0D0*HH(L))
      K = 8
      AREA = DABS(EST)
      EF = 0.5D0
      MXL = 0
C
C     COMPUTE REFINED ESTIMATES, ESTIMATE THE ERROR, ETC.
C
   20 GL = G8(AA(L)+HH(L),HH(L))
      GR(L) = G8(AA(L)+3.0D0*HH(L),HH(L))
      K = K + 16
      AREA = AREA + (DABS(GL)+DABS(GR(L))-DABS(EST))
C     IF (L.LT.LMN) GO TO 11
      GLR = GL + GR(L)
      EE = DABS(EST-GLR)*EF
      AE = DMAX1(EPS*AREA,TOL*DABS(GLR))
      IF (EE-AE) 40, 40, 50
   30 MXL = 1
   40 CE = CE + (EST-GLR)
      IF (LR(L)) 60, 60, 80
C
C     CONSIDER THE LEFT HALF OF THIS LEVEL
C
   50 IF (K.GT.KMX) LMX = KML
      IF (L.GE.LMX) GO TO 30
      L = L + 1
      EPS = EPS*0.5D0
      EF = EF/SQ2
      HH(L) = HH(L-1)*0.5D0
      LR(L) = -1
      AA(L) = AA(L-1)
      EST = GL
      GO TO 20
C
C     PROCEED TO RIGHT HALF AT THIS LEVEL
C
   60 VL(L) = GLR
   70 EST = GR(L-1)
      LR(L) = 1
      AA(L) = AA(L) + 4.0D0*HH(L)
      GO TO 20
C
C     RETURN ONE LEVEL
C
   80 VR = GLR
   90 IF (L.LE.1) GO TO 120
      L = L - 1
      EPS = EPS*2.0D0
      EF = EF*SQ2
      IF (LR(L)) 100, 100, 110
  100 VL(L) = VL(L+1) + VR
      GO TO 70
  110 VR = VL(L+1) + VR
      GO TO 90
C
C      EXIT
C
  120 ANS = VR
      IF ((MXL.EQ.0) .OR. (DABS(CE).LE.2.0D0*TOL*AREA)) GO TO 140
      IERR = 2
      CALL XERROR('DGAUS8- ANS IS PROBABLY INSUFFICIENTLY ACCURATE.',
     * 48, 3, 1)
      GO TO 140
  130 IERR = -1
      CALL XERROR('DGAUS8- THE FOLLOWING TEMPORARY DIAGNOSTIC WILL APPEA
     *R ONLY ONCE.  A AND B ARE TOO NEARLY EQUAL TO ALLOW NORMAL INTEGRA
     *TION.  ANS IS SET TO ZERO, AND IERR=-1.', 158, 1, -1)
  140 ICALL = 0
      IF (ERR.LT.0.0D0) ERR = CE
      RETURN
  150 CONTINUE
      CALL XERROR('DGAUS8- ABS(ERR) IS TOO SMALL.', 30, 2, 1)
      RETURN
      END
      SUBROUTINE ZEXINT(ZR, ZI, N, KODE, TOL, M, CYR, CYI, IERR)
C***BEGIN PROLOGUE  ZEXINT
C***DATE WRITTEN   870515   (YYMMDD)
C***REVISION DATE  870515   (YYMMDD)
C***CATEGORY NO.  B5E
C***KEYWORDS  EXPONENTIAL INTEGRALS, SINE INTEGRAL, COSINE INTEGRAL
C***AUTHOR  AMOS, DONALD E., SANDIA NATIONAL LABORATORIES
C***PURPOSE  TO COMPUTE EXPONENTIAL INTEGRALS OF A COMPLEX ARGUMENT
C***DESCRIPTION
C
C                 **** A DOUBLE PRECISION ROUTINE ****
C
C         ON KODE=1, ZEXINT COMPUTES AN M MEMBER SEQUENCE OF COMPLEX
C         EXPONENTIAL INTEGRALS CY(J)=E(N+J-1,Z), J=1,...,M, FOR
C         POSITIVE ORDERS N,...,N+M-1 AND COMPLEX Z IN THE CUT PLANE
C         -PI.LT.ARG(Z).LE.PI (N=1 AND Z=CMPLX(0.0,0.0) CANNOT HOLD AT
C         THE SAME TIME). ON KODE=2, ZEXINT COMPUTES SCALED FUNCTIONS
C
C                          CY(J)=E(N+J-1,Z)*CEXP(Z),      J=1,...,M,
C
C         WHICH REMOVES THE EXPONENTIAL BEHAVIOR IN BOTH THE LEFT AND
C         RIGHT HALF PLANES. DEFINITIONS AND NOTATION ARE FOUND IN THE
C         NBS HANDBOOK OF MATHEMATICAL FUNCTIONS (REF. 1).
C
C         INPUT      ZR,ZI,TOL ARE DOUBLE PRECISION
C           ZR,ZI  - Z=CMPLX(ZR,ZI), -PI.LT.ARG(Z).LE.PI
C           N      - INTEGER ORDER OF INITIAL E FUNCTION, N=1,2,...
C                    (N=1 AND Z=CMPLX(0.0,0.0) IS AN ERROR)
C           KODE   - A PARAMETER TO INDICATE THE SCALING OPTION
C                    KODE= 1  RETURNS
C                             CY(J)=E(N+J-1,Z),          J=1,...,M
C                        = 2  RETURNS
C                             CY(J)=E(N+J-1,Z)*CEXP(Z),  J=1,...,M
C           TOL    - PRECISION (ACCURACY) DESIRED FOR THE SEQUENCE,
C                    URND.LE.TOL.LE.1.0D-3, WHERE URND IS LIMITED BY
C                    URND=DMAX1(UNIT ROUNDOFF,1.0D-18) AND UNIT
C                    ROUNDOFF = D1MACH(4) IN TERMS OF MACHINE CONSTANT
C                    ROUTINE D1MACH
C           M      - NUMBER OF E FUNCTIONS IN THE SEQUENCE, M.GE.1
C
C         OUTPUT     CYR,CYI ARE DOUBLE PRECISION
C           CYR,CYI- VECTORS WHOSE FIRST M COMPONENTS CONTAIN THE REAL
C                    AND IMAGINARY PARTS OF
C                    CY(J)=E(N+J-1,Z)  OR
C                    CY(J)=E(N+J-1,Z)*CEXP(Z), J=1,...,M
C                    DEPENDING ON KODE WHERE CY(J)=CMPLX(CYR(J),CYI(J)).
C           IERR   - ERROR FLAG
C                    IERR=0, NORMAL RETURN - COMPUTATION COMPLETED
C                    IERR=1, INPUT ERROR   - NO COMPUTATION
C                    IERR=2, UNDERFLOW     - FIRST M COMPONENTS OF CY
C                            SET TO ZERO, CY(J)=CMPLX(0.0,0.0), J=1,M,
C                            REAL(Z).GT.0.0 TOO LARGE ON KODE=1
C                    IERR=3, OVERFLOW      - NO COMPUTATION,
C                            REAL(Z).LT.0.0 TOO SMALL ON KODE=1
C                    IERR=4, CABS(Z) OR N+M-1 LARGE - COMPUTATION DONE
C                            BUT LOSSES OF SIGNIFICANCE BY ARGUMENT
C                            REDUCTION PRODUCE LESS THAN HALF OF MACHINE
C                            ACCURACY
C                    IERR=5, CABS(Z) OR N+M-1 TOO LARGE - NO COMPUTA-
C                            TION BECAUSE OF COMPLETE LOSSES OF SIGNIFI-
C                            CANCE BY ARGUMENT REDUCTION
C                    IERR=6, ERROR         - NO COMPUTATION,
C                            ALGORITHM TERMINATION CONDITION NOT MET.
C                            SEE LONG DESCRIPTION ABOUT PARAMETER ICMAX.
C                    IERR=7, ERROR         - NO COMPUTATION,
C                            DISCRIMINATION ERROR. THIS CONDITION SHOULD
C                            NEVER OCCUR.
C
C***LONG DESCRIPTION
C
C         ZEXINT USES A COMBINATION OF POWER SERIES AND BACKWARD RECUR-
C         RENCE DESCRIBED IN REF. 2 FOR THE COMPLEX Z PLANE EXCEPT FOR
C         A STRIP 2*YB WIDE ABOUT THE NEGATIVE REAL AXIS, WHERE ANALYTIC
C         CONTINUATION IS CARRIED OUT BY LIMITED USE OF TAYLOR SERIES.
C         THE SWITCH FROM BACKWARD RECURRENCE TO TAYLOR SERIES IS
C         NECESSARY BECAUSE BACKWARD RECURRENCE IS SLOWLY CONVERGENT
C         NEAR THE NEGATIVE REAL AXIS. THE BOUNDARIES Y=-YB AND
C         Y=YB WERE DETERMINED SO THAT BACKWARD RECURRENCE WOULD CON-
C         VERGE EASILY WITH N AS LARGE AS 100 AND TOL AS SMALL AS
C         1.0D-18. SUBROUTINE ZEXENZ DOES THE BACKWARD RECURRENCE AND
C         SUBROUTINE ZACEXI DOES THE ANALYTIC CONTINUATION. TO START THE
C         CONTINUATION, ZACEXI CALLS ZEXENZ WITH ZB=CMPLX(X,YB).
C         IF ZEXENZ RETURNS IERR=6, THEN YB IS INCREASED BY 0.5 UNTIL
C         ZEXENZ RETURNS IERR=0 OR 10 TRIES, WHICHEVER COMES FIRST.
C         WHEN IERR=0, THEN THE ANALYTIC CONTINUATION PROCEEDS VERTI-
C         CALLY DOWN FROM ZB=CMPLX(X,YB) TO Z=CMPLX(X,Y), 0.LE.Y.LT.YB.
C         CONJUGATION IS USED FOR Y.LT.0. YB INCREASES AS TOL DECREASES
C         TO KEEP CONVERGENCE RATES UP AND RECURRENCE DOWN.
C
C         PARAMETER ICDIM=250 ALLOCATES STORAGE FOR THE COEFFICIENTS OF
C         THE BACKWARD RECURRENCE ALGORITHM. IF THE ALGORITHM TERMINA-
C         TION CONDITION IS NOT MET IN ICDIM STEPS, THEN RECURRENCE PRO-
C         CEEDS WITH NO ADDITIONAL STORAGE UNTIL THE TERMINATION CONDI-
C         TION IS MET OR THE LIMIT ICMAX=2000 IS EXCEEDED. THE PURPOSE
C         OF STORAGE IS TO MAKE THE ALGORITHM MORE EFFICIENT. THE TERMI-
C         NATION CONDITION IS MET IN LESS THAN 250 STEPS OVER MOST OF
C         THE COMPLEX PLANE EXCLUDING THE STRIP DABS(Y).LT.YB, X.LT.0.
C         EXCEPTIONS TO THIS RULE ARE GENERATED NEAR STRIP BOUNDARIES
C         WHEN N+M-1 AND CABS(Z) ARE LARGE AND NEARLY EQUAL. IN THESE
C         CASES, THE CONVERGENCE IS VERY SLOW AND ADDITIONAL RECURRENCE
C         (UP TO ICMAX) MUST BE USED. ON THE OTHERHAND, THESE REGIONS
C         OF SLOW CONVERGENCE ARE KEPT SMALL BY ADJUSTING YB AS A FUNC-
C         TION OF TOL. THESE REGIONS COULD BE ELIMINATED ENTIRELY BY
C         MAKING YB SUFFICIENTLY LARGE, BUT THE EXPENSE AND INSTABILITY
C         OF CONTINUATION BY TAYLOR SERIES NOT ONLY GOES UP, BUT THE
C         COMPUTATIONAL EXPENSE BECOMES EXCESSIVELY LARGE IN OTHER PARTS
C         OF THE LEFT HALF PLANE (Y.LT.YB) WHERE THE BACKWARD RECURRENCE
C         ALGORITHM WOULD CONVERGE RAPIDLY.
C
C         DERIVATIVES FOR SUCCESSIVE POWER SERIES ARE NOT COMPUTED BY
C         EVALUATING DERIVATIVES OF A PREVIOUS POWER SERIES. BECAUSE OF
C         THE RELATION
C
C           (1)           DE(N,Z)/DZ = - E(N-1,Z),
C
C         SUCCESSIVE DERIVATIVES AT Z ARE GIVEN BY LOWER ORDER FUNCTIONS
C         AND CAN BE COMPUTED IN A STABLE FASHION BY BACKWARD RECURRENCE
C         USING (2) PROVIDED THAT THE BEGINNING ORDER NUB IS SMALLER
C         THAN THE ARGUMENT. TO ACHIEVE THIS FOR ALL INTERMEDIATE VALUES
C         ZZ BETWEEN ZB AND Z, WE TAKE NUB=MINO(N+M-1,INT(CABS(Z)+0.5)).
C         TO START, E(NUB,ZB) IS EVALUATED BY THE BACKWARD RECURRENCE
C         ALGORITHM OF REF. 3. TO CONTINUE THE FUNCTION FROM ZB TO Z VIA
C         INTERMEDIATE VALUES ZZ, DERIVATIVES OF E(NUB,ZB) ARE COMPUTED
C         BY BACKWARD RECURRENCE ON (2). THIS ALLOWS A STEP (NO LARGER
C         THAN 0.5) TO ZZ FOR E(NUB,ZZ) USING THE TAYLOR SERIES ABOUT
C         ZB. NOW, WE APPLY (2) AGAIN STARTING AT E(NUB,ZZ) FOR THE DE-
C         RIVATIVES AT ZZ AND TAKE ANOTHER STEP, ETC. NOTICE THAT THE
C         STABILITY CONDITION FOR BACKWARD RECURRENCE, NUB.LE.CABS(Z)
C         .LE.CABS(ZZ).LE.CABS(ZB), IS SATISFIED FOR ALL INTERMEDIATE
C         VALUES ZZ. THE FINAL SEQUENCE FOR ORDERS N,...,N+M-1 IS GEN-
C         ERATED FROM (2) BY BACKWARD RECURRENCE, FORWARD RECURRENCE
C         OR BOTH ONCE E(NUB,Z) HAS BEEN COMPUTED.
C
C         RECURRENCE WITH THE RELATION
C
C            (2)     N*E(N+1,Z) + Z*E(N,Z) = CEXP(-Z)
C
C         IN A DIRECTION AWAY FROM THE INTEGER CLOSEST TO CABS(Z) IS
C         STABLE. FOR NEGATIVE ORDERS, THE RECURRENCE
C
C               E( 0,Z) = CEXP(-Z)/Z
C               E(-N,Z) = ( CEXP(-Z)+N*E(-N+1,Z) )/Z   ,N=1,2,...
C
C         IS NUMERICALLY STABLE FOR ALL Z.
C
C         THE (CAPITAL) SINE AND COSINE INTEGRALS CAN BE COMPUTED FROM
C
C                 SI(Z) =  (E(1,I*Z)-E(1,-I*Z))/(2*I) + PI/2
C                 CI(Z) = -(E(1,I*Z)+E(1,-I*Z))/2
C
C         IN -PI/2.LT.ARG(Z).LE.PI/2, (I**2=-1), WHILE THE PRINCIPAL
C         VALUED EXPONENTIAL INTEGRAL EI(X) CAN BE COMPUTED FROM
C
C             EI( X) = -(E(1,-X+I*0)+E(1,-X-I*0))/2 = -REAL(E(1,-X))
C             EI(-X) = -REAL(E(1,X))
C
C         FOR X.GT.0.0 TO AN ACCURACY TOL. IF Z = X.GT.0 THEN THE
C         REAL SINE AND COSINE INTEGRALS ARE GIVEN BY
C
C                 SI(X) = AIMAG(E(1,I*X)) + PI/2
C                 CI(X) = -REAL(E(1,I*X)) .
C
C         THE ANALYTIC CONTINUATION TO OTHER SHEETS CAN BE DONE BY
C         THE RELATIONS
C
C         E(N,Z*CEXP(2*PI*M*I)) = E(N,Z) - 2*PI*M*I*(-Z)**(N-1)/(N-1)!
C
C         WHERE M=+1 OR M=-1 AND I**2=-1.
C
C***REFERENCES  HANDBOOK OF MATHEMATICAL FUNCTIONS BY M. ABRAMOWITZ
C                 AND I. A. STEGUN, NBS AMS SERIES 55, U.S. DEPT. OF
C                 COMMERCE, 1955.
C
C               COMPUTATION OF EXPONENTIAL INTEGRALS OF COMPLEX ARGUMENT
C                 BY D. E. AMOS, ACM TRANS. MATH. SOFTWARE
C
C               COMPUTATION OF EXPONENTIAL INTEGRALS
C                 BY D. E. AMOS, ACM TRANS. MATH. SOFTWARE, VOL 6, NO. 3
C                 SEPTEMBER 1980, PP. 365-377; ALGORITHM 556, EXPONEN-
C                 TIAL INTEGRALS, PP. 420-428.
C
C               REMARK ON ALGORITHM 556
C                 BY D. E. AMOS, ACM TRANS. MATH. SOFTWARE, VOL 9, NO. 4
C                 DECEMBER 1983, P. 525.
C
C               UNIFORM ASYMPTOTIC EXPANSIONS FOR EXPONENTIAL INTE-
C                 GRALS E(N,X) AND BICKLEY FUNCTIONS KI(N,X) BY D. E.
C                 AMOS, ACM TRANS. MATH. SOFTWARE, VOL 9, NO. 4,
C                 DECEMBER. 1983, PP. 467-479; ALGORITHM 609, A PORTA-
C                 BLE FORTRAN SUBROUTINE FOR BICKLEY FUNCTIONS KI(N,X),
C                 PP. 480-493.
C
C***ROUTINES CALLED  ZACEXI,ZEXENZ,I1MACH,D1MACH
C***END PROLOGUE  ZEXINT
C
C
      EXTERNAL ZABS
      INTEGER I, ICDIM, IERR, K, KODE, K1, K2, M, N, I1MACH
      DOUBLE PRECISION AA, ALIM, AZ, BB, CA, D, ELIM, FN, RBRY, R1M5,
     * TOL, URND, ZR, ZI, YB, HTOL, CYR, CYI, CBR, CBI, D1MACH, ZABS
C     THE CORRESPONDING SINGLE PRECISION VARIABLES IN CEXINT ARE:
C     COMPLEX Z, CY, CB
C-----------------------------------------------------------------------
C     DIMENSION CA(ICDIM),CB(ICDIM)
C-----------------------------------------------------------------------
      DIMENSION CYR(M), CYI(M), CA(250), CBR(250), CBI(250)
      DATA ICDIM /250/
C***FIRST EXECUTABLE STATEMENT  ZEXINT
      IERR = 0
      IF (ZR.EQ.0.0D0 .AND. ZI.EQ.0.0D0 .AND. N.EQ.1) IERR = 1
      IF (N.LT.1) IERR = 1
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR = 1
      IF (M.LT.1) IERR = 1
      URND = DMAX1(D1MACH(4),1.0D-18)
      IF (TOL.LT.URND .OR. TOL.GT.1.0D-3) IERR = 1
      IF (IERR.NE.0) RETURN
      IF (ZR.EQ.0.0D0 .AND. ZI.EQ.0.0D0 .AND. N.GT.1) GO TO 40
C-----------------------------------------------------------------------
C     SET PARAMETERS RELATED TO MACHINE CONSTANTS.
C     URND IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18.
C     ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT.
C     EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/URND    AND
C     EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*URND       ARE INTERVALS NEAR
C     UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC IS DONE.
C-----------------------------------------------------------------------
      K1 = I1MACH(15)
      K2 = I1MACH(16)
      R1M5 = D1MACH(5)
      K = MIN0(IABS(K1),IABS(K2))
      ELIM = 2.303D0*(FLOAT(K)*R1M5-3.0D0)
      K1 = I1MACH(14) - 1
      AA = R1M5*FLOAT(K1)
      AA = AA*2.303D0
      ALIM = ELIM + DMAX1(-AA,-41.45D0)
      RBRY = 2.0D0
      IF (URND.GT.1.0D-8) RBRY = 1.0D0
C-----------------------------------------------------------------------
C     TEST VARIABLES FOR RANGE. CABS(Z) CANNOT BE LARGER THAN THE ARG-
C     UMENT OF THE INT( ) FUNCTION.
C-----------------------------------------------------------------------
      AZ = ZABS(ZR,ZI)
      FN = DBLE(FLOAT(N+M-1))
      AA = 0.5D0/URND
      BB = FLOAT(I1MACH(9))*0.5D0
      AA = DMIN1(AA,BB)
      IF (AZ.GT.AA) GO TO 60
      IF (FN.GT.AA) GO TO 60
      AA = DSQRT(AA)
      IF (AZ.GT.AA) IERR = 4
      IF (FN.GT.AA) IERR = 4
      IF (ZR.LT.0.0D0) GO TO 10
C-----------------------------------------------------------------------
C     BACKWARD RECURRENCE FOR THE RIGHT HALF PLANE, ZR.GE.0.0D0
C-----------------------------------------------------------------------
      CALL ZEXENZ(ZR, ZI, N, KODE, M, CYR, CYI, IERR, RBRY, TOL, ELIM,
     * ALIM, ICDIM, CA, CBR, CBI)
      RETURN
   10 CONTINUE
      IF (AZ.GT.RBRY) GO TO 20
C-----------------------------------------------------------------------
C     POWER SERIES FOR CABS(Z).LE.RBRY AND ZR.LT.0.0D0
C-----------------------------------------------------------------------
      CALL ZEXENZ(ZR, ZI, N, KODE, M, CYR, CYI, IERR, RBRY, TOL, ELIM,
     * ALIM, ICDIM, CA, CBR, CBI)
      RETURN
   20 CONTINUE
      D = -0.4342945D0*DLOG(TOL)
      YB = 10.5D0 - 0.538460D0*(18.0D0-D)
      IF (DABS(ZI).LT.YB) GO TO 30
C-----------------------------------------------------------------------
C     BACKWARD RECURRENCE EXTERIOR TO THE STRIP DABS(ZI).LT.YB,ZR.LT.0.0
C-----------------------------------------------------------------------
      HTOL = 0.125D0*TOL
      CALL ZEXENZ(ZR, ZI, N, KODE, M, CYR, CYI, IERR, RBRY, HTOL, ELIM,
     * ALIM, ICDIM, CA, CBR, CBI)
      RETURN
   30 CONTINUE
C-----------------------------------------------------------------------
C     TAYLOR SERIES IN ZACEXI FOR ANALYTIC CONTINUATION
C-----------------------------------------------------------------------
      CALL ZACEXI(ZR, ZI, N, KODE, M, CYR, CYI, IERR, YB, RBRY, TOL,
     * ELIM, ALIM, ICDIM, CA, CBR, CBI)
      RETURN
   40 CONTINUE
      DO 50 I=1,M
        CYR(I) = 1.0D0/DBLE(FLOAT(N+I-2))
        CYI(I) = 0.0D0
   50 CONTINUE
      RETURN
   60 CONTINUE
      IERR = 5
      RETURN
      END
      SUBROUTINE ZEXENZ(ZR, ZI, N, KODE, M, CYR, CYI, IERR, RBRY, TOL,
     * ELIM, ALIM, ICDIM, CA, CBR, CBI)
C***BEGIN PROLOGUE  ZEXENZ
C***REFER TO  ZEXINT
C
C      ZEXENZ COMPUTES THE EXPONENTIAL INTEGRAL BY MEANS OF POWER SERIES
C      AND A BACKWARD RECURRENCE ALGORITHM FOR THE CONFLUENT HYPER-
C      GEOMETRIC REPRESENTATION.
C
C***ROUTINES CALLED DPSIXN
C***END PROLOGUE  ZEXENZ
C
      EXTERNAL ZABS
      INTEGER I, IC, ICASE, ICDIM, ICMAX, ICT, IERR, IK, IND, IZ, JSET,
     * K, KFLAG, KN, KODE, KS, M, ML, MU, N, ND, NM, KERR
      DOUBLE PRECISION AA, AAM, AAMS, AEM, AH, AK, ALIM, AP1, AT, AZ,
     * BK, BT, CA, DK, ELIM, ERR, EULER, FC, FNM, RBRY, RTOLA, TOL,
     * TOLA, XTOL, ZR, ZI, CK, DPSIXN, ZABS
      DOUBLE PRECISION CP1R, CP1I, CP2R, CP2I, CPTR, CPTI, CATR, CATI,
     * CBTR, CBTI, CY1R, CY1I, CY2R, CY2I, CYR(M), CYI(M), CYYR(2),
     * CYYI(2), CNORMR, CNORMI, CBR, CBI, CSR, CSI, CAKR, CAKI, EMZR,
     * EMZI, CAAR, CAAI, TZR, TZI, FZR, FZI, CTR, CTI, CZEROR, CZEROI,
     * CONER, CONEI, SCLER, SCLEI, RSCLER
C     THE CORRESPONDING SINGLE PRECISION COMPLEX VARIABLES IN CEXENZ ARE
C     COMPLEX CP1, CP2, CPT, CAT, CBT, CY1, CY2, CY(M), CYY(2),
C    * CNORM, CB, Z, CS, CAK, EMZ, CAA, TZ, FZ, CT, CZERO, CONE,
C    * SCLE, RSCLE
      DIMENSION CA(ICDIM), CBR(ICDIM), CBI(ICDIM)
      DATA EULER /-5.77215664901532861D-01/
      DATA CZEROR, CZEROI, CONER, CONEI  /0.0D0, 0.0D0, 1.0D0, 0.0D0/
      DATA ICMAX /2000/
      SCLER = CONER
      SCLEI = CONEI
      RSCLER = CONER
      AZ = ZABS(ZR,ZI)
      IF (AZ.GT.RBRY) GO TO 80
C-----------------------------------------------------------------------
C     SERIES FOR E(N,Z) FOR CABS(Z).LE.RBRY
C-----------------------------------------------------------------------
      IZ = INT(SNGL(AZ+0.5D0))
C-----------------------------------------------------------------------
C     ICASE=1 MEANS INTEGER CLOSEST TO CABS(Z) IS 2 AND N=1
C     ICASE=2 MEANS INTEGER CLOSEST TO CABS(Z) IS 0,1, OR 2 AND N.GE.2
C-----------------------------------------------------------------------
      ICASE = 2
      IF (IZ.GT.N) ICASE = 1
      NM = N - ICASE + 1
      ND = NM + 1
      IND = 3 - ICASE
      MU = M - IND
      ML = 1
      KS = ND
      FNM = DBLE(FLOAT(NM))
      CSR = CZEROR
      CSI = CZEROI
      XTOL = 0.3333D0*TOL
      AAM = 1.0D0
      IF (ND.EQ.1) GO TO 10
      AAM = 1.0D0/FNM
      CSR = AAM
      CSI = 0.0E0
   10 CONTINUE
      CAAR = CONER
      CAAI = CONEI
      AA = 1.0D0
      AK = 1.0D0
C-----------------------------------------------------------------------
C     LIMIT INDEX I TO IK TO PREVENT UNDERFLOW ON SMALL VALUES OF Z
C-----------------------------------------------------------------------
      IK = 35
      IF (AZ.LT.XTOL*AAM) IK = 1
      DO 50 I=1,IK
        AT=1.0D0/AK
        AP1 = -(CAAR*ZR-CAAI*ZI)*AT
        CAAI = -(CAAR*ZI+CAAI*ZR)*AT
        CAAR = AP1
        AA=AA*AZ*AT
        IF (I.EQ.NM) GO TO 30
        AT = 1.0D0/(AK-FNM)
        CSR = CSR - CAAR*AT
        CSI = CSI - CAAI*AT
        GO TO 40
   30   CONTINUE
        CALL ZLOG(ZR,ZI,CATR,CATI,KERR)
        CATR = -CATR + DPSIXN(ND)
        CATI = -CATI
        CSR = CSR + CAAR*CATR - CAAI*CATI
        CSI = CSI + CAAR*CATI + CAAI*CATR
   40   CONTINUE
        IF (AA.LE.XTOL*ZABS(CSR,CSI)) GO TO 60
        AK = AK+1.0D0
   50 CONTINUE
   60 CONTINUE
      IF (ND.NE.1) GO TO 65
      CALL ZLOG(ZR,ZI,CATR,CATI,KERR)
      CATR = -CATR + EULER
      CATI = -CATI
      CSR = CSR + CATR
      CSI = CSI + CATI
   65 CONTINUE
      IF (KODE.EQ.1) GO TO 66
      CALL ZEXP(ZR,ZI,CATR,CATI)
      AP1 = CSR*CATR-CSI*CATI
      CSI = CSR*CATI+CSI*CATR
      CSR = AP1
   66 CONTINUE
      CYR(1) = CSR
      CYI(1) = CSI
      CTR = CSR
      CTI = CSI
      EMZR = CONER
      EMZI = CONEI
      IF (M.EQ.1) GO TO 70
      CYR(IND) = CSR
      CYI(IND) = CSI
      AK = DBLE(FLOAT(KS))
      IF (KODE.EQ.2) GO TO 67
      CALL ZEXP(-ZR,-ZI,EMZR,EMZI)
   67 CONTINUE
      GO TO (300, 320), ICASE
   70 CONTINUE
      IF (ICASE.EQ.2) RETURN
      IF (KODE.EQ.2) GO TO 71
      CALL ZEXP(-ZR,-ZI,EMZR,EMZI)
   71 CONTINUE
      CATR = EMZR - CSR
      CATI = EMZI - CSI
      CALL ZDIV(CATR,CATI,ZR,ZI,CYR(1),CYI(1))
      RETURN
C-----------------------------------------------------------------------
C     BACKWARD RECURSIVE MILLER ALGORITHM FOR
C              E(N,Z)=EXP(-Z)*(Z**(N-1))*U(N,N,Z)
C     WITH RECURSION AWAY FROM N=INTEGER CLOSEST TO CABS(Z)
C     U(A,B,Z) IS THE SECOND CONFLUENT HYPERGEOMETRIC FUNCTION
C-----------------------------------------------------------------------
   80 CONTINUE
      EMZR = CONER
      EMZI = CONEI
      IF (KODE.EQ.2) GO TO 140
C-----------------------------------------------------------------------
C     SCALE NEAR EXPONENT EXTREMES ON KODE=1
C-----------------------------------------------------------------------
      IF (ZR.LT.0.0D0) GO TO 90
      AT = ZR + DBLE(FLOAT(N+M-1))
      CTR = AT
      CTI = ZI
      AA = ZABS(CTR,CTI)
      AT = ZR + DLOG(AA)
      IF (AT.GT.ELIM) GO TO 150
      KFLAG = 1
      GO TO 100
   90 CONTINUE
      AT = ZR
      IF (AT.LT.(-ELIM)) GO TO 340
      KFLAG = 2
  100 CONTINUE
      IF (DABS(AT).LT.ALIM) GO TO 130
      TOLA = DEXP(ALIM-ELIM)
      RTOLA = 1.0D0/TOLA
      IF (KFLAG.EQ.2) GO TO 110
      SCLER = RTOLA
      SCLEI = 0.0D0
      RSCLER = TOLA
      GO TO 120
  110 CONTINUE
      SCLER = TOLA
      SCLEI = 0.0D0
      RSCLER = RTOLA
  120 CONTINUE
      EMZR = SCLER
      EMZI = SCLEI
  130 CONTINUE
      CALL ZEXP(-ZR,-ZI,CATR,CATI)
      AP1 = EMZR*CATR-EMZI*CATI
      EMZI = EMZR*CATI+EMZI*CATR
      EMZR = AP1
  140 CONTINUE
      IZ = INT(SNGL(AZ+0.5D0))
      KN = N + M - 1
      IF (KN.LE.IZ) GO TO 170
      IF (N.LT.IZ .AND. IZ.LT.KN) GO TO 200
      IF (N.GE.IZ) GO TO 190
      IERR=7
      RETURN
  150 CONTINUE
      IERR = 2
      DO 160 I=1,M
        CYR(I) = CZEROR
        CYI(I) = CZEROI
  160 CONTINUE
      RETURN
  170 CONTINUE
      ICASE = 1
      KS = KN
      ML = M - 1
      MU = -1
      IND = M
      IF (KN.GT.1) GO TO 210
  180 CONTINUE
      KS = 2
      ICASE = 3
      GO TO 210
  190 CONTINUE
      ICASE = 2
      IND = 1
      KS = N
      MU = M - 1
      IF (N.GT.1) GO TO 210
      IF (KN.EQ.1) GO TO 180
      IZ = 2
  200 CONTINUE
      ICASE = 1
      KS = IZ
      ML = IZ - N
      IND = ML + 1
      MU = KN - IZ
  210 CONTINUE
      IK = KS/2
      AH = DBLE(FLOAT(IK))
      JSET = 1 + KS - 2*IK
C-----------------------------------------------------------------------
C     START COMPUTATION FOR
C              CYY(1) = C*U( A , A ,Z)    JSET=1
C              CYY(1) = C*U(A+1,A+1,Z)    JSET=2
C     FOR AN EVEN INTEGER A.
C-----------------------------------------------------------------------
      IC = 0
      AA = AH + AH
      CAAR = AA
      CAAI = 0.0D0
      AAM = AA - 1.0D0
      AAMS = AAM*AAM
      TZR = ZR + ZR
      TZI = ZI + ZI
      FZR = TZR + TZR
      FZI = TZI + TZI
      AK = AH
      XTOL = TOL
      CTR = AAMS + FZR*AH
      CTI = FZI*AH
      CAKR = ZR + CAAR
      CAKI = ZI + CAAI
      AEM = (AK+1.0D0)/XTOL
      AEM = AEM/ZABS(CAKR,CAKI)
      BK = AA
      CK = AH*AH
C-----------------------------------------------------------------------
C     FORWARD RECURSION FOR P(IC),P(IC+1) AND INDEX IC FOR BACKWARD
C     RECURSION
C-----------------------------------------------------------------------
      CP1R = CZEROR
      CP1I = CZEROI
      CP2R = CONER
      CP2I = CONEI
  220 CONTINUE
      IC = IC + 1
      IF (IC.GT.ICDIM) GO TO 230
      AK = AK + 1.0D0
      CK = CK + 1.0D0
      AT = BK/(BK+AK+CK)
      BK = BK + AK + AK
      CA(IC)=AT
      BT = 1.0D0/(AK+1.0D0)
      CBTR = (AK+AK+ZR)*BT
      CBTI = ZI*BT
      CBR(IC) = CBTR
      CBI(IC) = CBTI
      CPTR = CP2R
      CPTI = CP2I
      AP1 = CBTR*CP2R-CBTI*CP2I-CP1R*AT
      CP2I = CBTR*CP2I+CBTI*CP2R-CP1I*AT
      CP2R = AP1
      CP1R = CPTR
      CP1I = CPTI
      CTR = CTR + FZR
      CTI = CTI + FZI
      AEM = AEM*AT
      BT = ZABS(CTR,CTI)
      ERR = AEM/DSQRT(BT)
      AP1 = ZABS(CP1R,CP1I)
      IF (ERR*(AK+1.0D0)/AP1.GT.AP1) GO TO 220
      GO TO 250
C-----------------------------------------------------------------------
C     CONTINUE FORWARD RECURRENCE UNINDEXED WHEN IC EXCEEDS ICDIM
C-----------------------------------------------------------------------
  230 CONTINUE
      IC = IC - 1
  240 CONTINUE
      IC = IC + 1
      IF (IC.GT.ICMAX) GO TO 350
      AK = AK + 1.0D0
      CK = CK + 1.0D0
      AT = BK/(BK+AK+CK)
      BK = BK + AK + AK
      BT = 1.0D0/(AK+1.0D0)
      CBTR = (AK+AK+ZR)*BT
      CBTI = ZI*BT
      CPTR = CP2R
      CPTI = CP2I
      AP1 = CBTR*CP2R-CBTI*CP2I-CP1R*AT
      CP2I = CBTR*CP2I+CBTI*CP2R-CP1I*AT
      CP2R = AP1
      CP1R = CPTR
      CP1I = CPTI
      CTR = CTR + FZR
      CTI = CTI + FZI
      AEM = AEM*AT
      BT = ZABS(CTR,CTI)
      ERR = AEM/DSQRT(BT)
      AP1 = ZABS(CP1R,CP1I)
      IF (ERR*(AK+1.0D0)/AP1.GT.AP1) GO TO 240
  250 CONTINUE
      FC = DBLE(FLOAT(IC))
      AT = ((FC+1.0D0)/(AK+1.0D0))*((AK+AH)/(AK+1.0D0))
      CATR = CTR + FZR
      CATI = CTI + FZI
      CALL ZDIV(CTR,CTI,CATR,CATI,CATR,CATI)
      CALL ZSQRT(CATR,CATI,CATR,CATI)
      CATR = CATR*AT
      CATI = CATI*AT
      CALL ZDIV(CP1R,CP1I,CP2R,CP2I,CBTR,CBTI)
      CY2R = CATR*CBTR-CATI*CBTI
      CY2I = CATR*CBTI+CATI*CBTR
      CY1R = CONER
      CY1I = CONEI
C-----------------------------------------------------------------------
C     BACKWARD RECURRENCE FOR
C             CY1=             C*U( A ,A,Z)
C             CY2= C*(A/(1+A/2))*U(A+1,A,Z)
C-----------------------------------------------------------------------
      IF (IC.LE.ICDIM) GO TO 270
C-----------------------------------------------------------------------
C     BACKWARD RECUR UNINDEXED WHEN IC EXCEEDS ICDIM
C-----------------------------------------------------------------------
      BT = AA + ZR
  260 CONTINUE
      AK = AH + FC
      BK = AK + 1.0D0
      CK = AAM + FC
      DK = BT + FC + FC
      AT = (AK/FC)*(BK/CK)
      CBTR = DK/BK
      CBTI = ZI/BK
      CPTR = CY1R
      CPTI = CY1I
      AP1 = (CBTR*CY1R-CBTI*CY1I-CY2R)*AT
      CY1I = (CBTR*CY1I+CBTI*CY1R-CY2I)*AT
      CY1R = AP1
      CY2R = CPTR
      CY2I = CPTI
      FC = FC - 1.0D0
      IC = IC - 1
      IF (IC.GT.ICDIM) GO TO 260
  270 CONTINUE
      ICT = IC
      DO 280 K=1,ICT
        AT = 1.0D0/CA(IC)
        CPTR = CY1R
        CPTI = CY1I
        AP1 = (CBR(IC)*CY1R-CBI(IC)*CY1I-CY2R)*AT
        CY1I = (CBR(IC)*CY1I+CBI(IC)*CY1R-CY2I)*AT
        CY1R = AP1
        CY2R = CPTR
        CY2I = CPTI
        IC = IC - 1
  280 CONTINUE
C-----------------------------------------------------------------------
C     THE CONTIGUOUS RELATION
C              Z*U(B,C+1,Z)=(C-B)*U(B,C,Z)+U(B-1,C,Z)
C     WITH  B=A+1 , C=A IS USED FOR
C             CYY(2) = C * U(A+1,A+1,Z)
C     Z IS INCORPORATED INTO THE NORMALIZING RELATION FOR CNORM.
C-----------------------------------------------------------------------
      CALL ZDIV(CY2R,CY2I,CY1R,CY1I,CPTR,CPTI)
      BT = (AH+1.0D0)/AA
      CNORMR = CONER - CPTR*BT
      CNORMI = CONEI - CPTI*BT
      CATR = CNORMR*CAAR-CNORMI*CAAI+ZR
      CATI = CNORMR*CAAI+CNORMI*CAAR+ZI
      CALL ZDIV(CONER,CONEI,CATR,CATI,CYYR(1),CYYI(1))
      CYYR(2) = CNORMR*CYYR(1)-CNORMI*CYYI(1)
      CYYI(2) = CNORMR*CYYI(1)+CNORMI*CYYR(1)
      IF (ICASE.EQ.3) GO TO 290
      CTR = EMZR*CYYR(JSET)-EMZI*CYYI(JSET)
      CTI = EMZR*CYYI(JSET)+EMZI*CYYR(JSET)
      CYR(IND) = CTR*RSCLER
      CYI(IND) = CTI*RSCLER
      CSR = CTR
      CSI = CTI
      IF (M.EQ.1) RETURN
      AK = DBLE(FLOAT(KS))
      GO TO (300, 320), ICASE
C-----------------------------------------------------------------------
C     RECURSION SECTION  N*E(N+1,Z) + Z*E(N,Z)=EMZ
C-----------------------------------------------------------------------
  290 CONTINUE
      CATR = CONER - CYYR(1)
      CATI = CONEI - CYYI(1)
      CBTR = EMZR*CATR-EMZI*CATI
      CBTI = EMZR*CATI+EMZI*CATR
      CALL ZDIV(CBTR,CBTI,ZR,ZI,CTR,CTI)
      CYR(1) = CTR*RSCLER
      CYI(1) = CTI*RSCLER
      RETURN
  300 CONTINUE
      CAAR = AK
      CAAI = 0.0D0
      CALL ZDIV(CONER,CONEI,ZR,ZI,TZR,TZI)
      K = IND - 1
      DO 310 I=1,ML
        CAAR = CAAR - CONER
        CAAI = CAAI - CONEI
        CATR = EMZR-(CAAR*CTR-CAAI*CTI)
        CATI = EMZI-(CAAR*CTI+CAAI*CTR)
        CTR = CATR*TZR-CATI*TZI
        CTI = CATR*TZI+CATI*TZR
        CYR(K) = CTR*RSCLER
        CYI(K) = CTI*RSCLER
        K = K - 1
  310 CONTINUE
      IF (MU.LE.0) RETURN
      AK = DBLE(FLOAT(KS))
  320 CONTINUE
      K = IND
      DO 330 I=1,MU
        AT = 1.0D0/AK
        AP1 = (EMZR-(ZR*CSR-ZI*CSI))*AT
        CSI = (EMZI-(ZR*CSI+ZI*CSR))*AT
        CSR = AP1
        CYR(K+1) = CSR*RSCLER
        CYI(K+1) = CSI*RSCLER
        AK = AK + 1.0D0
        K = K + 1
  330 CONTINUE
      RETURN
  340 CONTINUE
      IERR = 3
      RETURN
  350 CONTINUE
      IERR = 6
      RETURN
      END
      SUBROUTINE ZACEXI(ZR, ZI, NU, KODE, N, YR, YI, IERR, YB, RBRY,
     * TOL, ELIM, ALIM, ICDIM, CA, CBR, CBI)
C***BEGIN PROLOGUE  ZACEXI
C***REFER TO  ZEXINT
C
C     ZACEXI COMPUTES THE ANALYTIC CONTINUATION OF THE EXPONENTIAL
C     INTEGRAL FOR X.LT.0 AND -YB.LT.Y.LT.YB BY TAYLOR SERIES IN
C     INCREMENTS OF HALF A UNIT. THE CONTINUATION PROCEEDS
C     VERTICALLY DOWN FROM ZB=CMPLX(X,YB) TO Z=CMPLX(X,Y) FOR 0.0.LE.
C     Y.LT.YB. CONJUGATION IS USED FOR Y.LT.0.0D0.
C
C***ROUTINES CALLED  ZEXENZ
C***END PROLOGUE  ZACEXI
C
      EXTERNAL ZABS
      INTEGER I, IAZ, ICDIM, IERR, IL, IS, IY, K, KB,  KL, KMAX,
     * KODE, KS, KYB, N, NB, NFLG, NU, NUB
      DOUBLE PRECISION ALIM, AZ, CA, DEL, ELIM, FK, RBRY, RTOLA, TOL,
     * TOLA, YB, YT, ZI, ZID, ZR, XTOL, RZI, ATRM, FJ, RW, RQ(64),
     * ASUM, HTOL, AT, ZABS
      DOUBLE PRECISION YR(N), YI(N), YYR(1), YYI(1), CONER, CONEI, CEXR,
     * CEXI, SUMR, SUMI, TRMR, TRMI, ZZR, ZZI, ZPR(64), ZPI(64),
     * ZTR, ZTI, SCLER, RSCLER, TRMSR, TRMSI, CBR, CBI,
     * ZWR, ZWI, CEZTR, CEZTI, CTR, CTI
C     THE CORRESPONDING SINGLE PRECISION COMPLEX VARIABLES IN CACEXI ARE
C     COMPLEX Y(N), YY(1), CONE, CEX, SUM, TRM, Z, ZZ, ZP(64), ZT, SCLE,
C    * RSCLE, TRMS, CB, ZW, CEZT
      DIMENSION CA(ICDIM), CBR(ICDIM), CBI(ICDIM)
      DATA CONER, CONEI/ 1.0D0, 0.0D0 /
      SCLER = CONER
      RSCLER = CONER
      ZID = ZI
      KYB = 0
      IF (ZI.LT.0.0D0) ZID = -ZID
      AZ = ZABS(ZR,ZI)
      IAZ = INT(SNGL(AZ+0.5D0))
C-----------------------------------------------------------------------
C     SET ORDER NUB=MIN0(N+M-1,INT(CABS(Z)+0.5)), GENERATE REMAINDER
C     OF THE SEQUENCE AFTER THE COMPUTATION OF E(NUB,Z)
C-----------------------------------------------------------------------
      IF (NU.GE.IAZ) GO TO 10
      IF (NU+N-1.LE.IAZ) GO TO 20
      NUB = IAZ
      NB = 0
      NFLG = 3
      KB = NUB - NU + 1
      KS = KB + 1
      KL = N
      IS = 1
      IL = KB - 1
      GO TO 40
   10 CONTINUE
      NUB = MAX0(IAZ,1)
      NB = NU - NUB
      NFLG = 1
      KB = 1
      KS = 2
      KL = N
      GO TO 40
   20 CONTINUE
      NUB = NU + N - 1
      NB = 0
      NFLG = 2
      IS = 2
      IL = N
      KB = N
      GO TO 40
C-----------------------------------------------------------------------
C     SET PARAMETERS FOR ANALYTIC CONTINUATION FROM Y=YB INTO THE REGION
C     0.LE.ZID.LT.YB.
C-----------------------------------------------------------------------
   30 CONTINUE
      YB = YB + 0.5D0
      KYB = KYB + 1
      IF (KYB.GT.10) RETURN
      IERR = 0
   40 CONTINUE
      DEL = YB - ZID
C-----------------------------------------------------------------------
C     MAKE DEL LARGE ENOUGH TO AVOID UNDERFLOW IN GENERATION OF POWERS
C-----------------------------------------------------------------------
      IF(DABS(DEL).GT.1.0D-4) GO TO 50
      YB = YB + 1.0D-4
      DEL = YB - ZID
   50 CONTINUE
      HTOL = 0.125D0*TOL
      ZZR = ZR
      ZZI = YB
      CALL ZEXENZ(ZZR,ZZI,NUB,2,1,YYR,YYI,IERR,RBRY,HTOL,ELIM,ALIM,
     * ICDIM,CA,CBR,CBI)
      IF(IERR.EQ.6) GO TO 30
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION VIA TAYLOR SERIES FOR ORDER NUB
C-----------------------------------------------------------------------
      IY=INT(SNGL(DEL+DEL))
      YT=DEL/DBLE(FLOAT(IY+1))
      SUMR=YYR(1)
      SUMI=YYI(1)
      HTOL=0.25D0*TOL
      TRMR=SUMR
      TRMI=SUMI
      CEZTR=DCOS(YT)
      CEZTI=-DSIN(YT)
      ZWR=CONER
      ZWI=CONEI
      ZPR(1)=CONER
      ZPI(1)=CONEI
      FK=1.0D0
      FJ=DBLE(FLOAT(NUB-1))
      CALL ZDIV(CONER,CONEI,ZZR,ZZI,ZTR,ZTI)
C-----------------------------------------------------------------------
C     TERMS OF THE SERIES TRM=E(NUB-K,ZZ)*(YT**K)/K!,  K=0,1,... ARE
C     GENERATED BY BACKWARD RECURRENCE. E IS SCALED BY CEXP(ZZ).
C-----------------------------------------------------------------------
      DO 60 K=2,64
        RW=YT/FK
        ATRM=-ZWI*RW
        ZWI=ZWR*RW
        ZWR=ATRM
        RW = FJ*RW
        CTR=ZWR+TRMI*RW
        CTI=ZWI-TRMR*RW
        TRMR=CTR*ZTR-CTI*ZTI
        TRMI=CTR*ZTI+CTI*ZTR
        SUMR=SUMR+TRMR
        SUMI=SUMI+TRMI
        ZPR(K)=ZWR
        ZPI(K)=ZWI
        RQ(K)=RW
        ASUM=ZABS(SUMR,SUMI)
        ATRM=ZABS(TRMR,TRMI)
        IF(ATRM.LT.HTOL*ASUM) GO TO 70
        FK=FK+1.0D0
        FJ=FJ-1.0D0
   60 CONTINUE
      K=64
   70 CONTINUE
      KMAX=K
      ATRM=SUMR*CEZTR-SUMI*CEZTI
      SUMI=SUMR*CEZTI+SUMI*CEZTR
      SUMR=ATRM
      IF(IY.EQ.0) GO TO 100
      DO 90 I=1,IY
        RZI=DBLE(FLOAT(IY-I+1))*YT+ZID
        ZZR=ZR
        ZZI=RZI
        CALL ZDIV(CONER,CONEI,ZZR,ZZI,ZTR,ZTI)
        TRMR=SUMR
        TRMI=SUMI
        DO 80 K=2,KMAX
          CTR=ZPR(K)+TRMI*RQ(K)
          CTI=ZPI(K)-TRMR*RQ(K)
          TRMR=CTR*ZTR-CTI*ZTI
          TRMI=CTR*ZTI+CTI*ZTR
          SUMR=SUMR+TRMR
          SUMI=SUMI+TRMI
   80   CONTINUE
        ATRM=ZABS(TRMR,TRMI)
        ASUM=ZABS(SUMR,SUMI)
        XTOL=HTOL*ASUM
        IF(ATRM.LT.XTOL) GO TO 86
        IF(KMAX.GE.64) GO TO 86
        KMAX=KMAX+1
        DO 84 K=KMAX,64
          RW=YT/FK
          ATRM=-ZWI*RW
          ZWI=ZWR*RW
          ZWR=ATRM
          RW=FJ*RW
          CTR=ZWR+TRMI*RW
          CTI=ZWI-TRMR*RW
          TRMR=CTR*ZTR-CTI*ZTI
          TRMI=CTR*ZTI+CTI*ZTR
          SUMR=SUMR+TRMR
          SUMI=SUMI+TRMI
          ZPR(K)=ZWR
          ZPI(K)=ZWI
          RQ(K)=RW
          ATRM=ZABS(TRMR,TRMI)
          IF(ATRM.LT.XTOL) GO TO 85
          FK=FK+1.0D0
          FJ=FJ-1.0D0
   84   CONTINUE
        K=64
   85   CONTINUE
        KMAX=K
   86   CONTINUE
        ATRM=SUMR*CEZTR-SUMI*CEZTI
        SUMI=SUMR*CEZTI+SUMI*CEZTR
        SUMR=ATRM
   90 CONTINUE
  100 CONTINUE
C-----------------------------------------------------------------------
C     FORM SEQUENCE UP OR DOWN FROM ORDER NUB
C-----------------------------------------------------------------------
      IF (ZI.LT.0.0D0) SUMI = -SUMI
      CEXR = CONER
      CEXI = CONEI
C-----------------------------------------------------------------------
C     SCALE NEAR OVERFLOW LIMIT ON KODE=1
C-----------------------------------------------------------------------
      IF (KODE.EQ.2) GO TO 160
      IF (DABS(ZR).LT.ALIM) GO TO 150
      IF (DABS(ZR).GT.ELIM) GO TO 220
      TOLA = DEXP(ALIM-ELIM)
      RTOLA = 1.0D0/TOLA
      SCLER=TOLA
      RSCLER=RTOLA
  150 CONTINUE
      CALL ZEXP(-ZR,-ZI,CTR,CTI)
      CEXR=SCLER*CTR
      CEXI=SCLER*CTI
  160 CONTINUE
      TRMR=SUMR*CEXR-SUMI*CEXI
      TRMI=SUMR*CEXI+SUMI*CEXR
      YR(KB)=TRMR*RSCLER
      YI(KB)=TRMI*RSCLER
      TRMSR=TRMR
      TRMSI=TRMI
      IF (N.EQ.1 .AND. NFLG.NE.1) RETURN
      IF (NFLG.EQ.2) GO TO 200
      FK = DBLE(FLOAT(NUB))
      IF (NFLG.NE.1 .OR. NB.EQ.0) GO TO 180
      DO 170 K=1,NB
        AT=1.0D0/FK
        ATRM=(CEXR-(ZR*TRMR-ZI*TRMI))*AT
        TRMI=(CEXI-(ZR*TRMI+ZI*TRMR))*AT
        TRMR=ATRM
        FK = FK + 1.0D0
  170 CONTINUE
  180 CONTINUE
      YR(KB)=TRMR*RSCLER
      YI(KB)=TRMI*RSCLER
      TRMSR=TRMR
      TRMSI=TRMI
      IF (N.EQ.1) RETURN
      DO 190 K=KS,KL
        AT=1.0D0/FK
        ATRM=(CEXR-(ZR*TRMR-ZI*TRMI))*AT
        TRMI=(CEXI-(ZR*TRMI+ZI*TRMR))*AT
        TRMR=ATRM
        YR(K)=TRMR*RSCLER
        YI(K)=TRMI*RSCLER
        FK = FK + 1.0D0
  190 CONTINUE
      IF (NFLG.EQ.1) RETURN
  200 CONTINUE
      K = KB - 1
      FK = DBLE(FLOAT(NUB-1))
      CALL ZDIV(CONER,CONEI,ZR,ZI,ZTR,ZTI)
      DO 210 I=IS,IL
        CTR=CEXR-FK*TRMSR
        CTI=CEXI-FK*TRMSI
        TRMSR=CTR*ZTR-CTI*ZTI
        TRMSI=CTR*ZTI+CTI*ZTR
        YR(K)=TRMSR*RSCLER
        YI(K)=TRMSI*RSCLER
        K = K - 1
        FK = FK - 1.0D0
  210 CONTINUE
      RETURN
  220 CONTINUE
      IERR = 3
      RETURN
      END
      DOUBLE PRECISION FUNCTION DPSIXN(N)
C***BEGIN PROLOGUE  DPSIXN
C***REFER TO  DEXINT,DBSKIN
C
C     THIS SUBROUTINE RETURNS VALUES OF PSI(X)=DERIVATIVE OF LOG
C     GAMMA(X), X.GT.0.0 AT INTEGER ARGUMENTS. A TABLE LOOK-UP IS
C     PERFORMED FOR N.LE.100, AND THE ASYMPTOTIC EXPANSION IS
C     EVALUATED FOR N.GT.100.
C
C***ROUTINES CALLED  D1MACH
C***END PROLOGUE  DPSIXN
C
      INTEGER N, K
      DOUBLE PRECISION AX, B, C, FN, RFN2, TRM, S, WDTOL
      DOUBLE PRECISION D1MACH
      DIMENSION B(6), C(100)
C
C             DPSIXN(N), N = 1,100
      DATA C(1), C(2), C(3), C(4), C(5), C(6), C(7), C(8), C(9), C(10),
     1     C(11), C(12), C(13), C(14), C(15), C(16), C(17), C(18),
     2     C(19), C(20), C(21), C(22), C(23), C(24)/
     3    -5.77215664901532861D-01,     4.22784335098467139D-01,
     4     9.22784335098467139D-01,     1.25611766843180047D+00,
     5     1.50611766843180047D+00,     1.70611766843180047D+00,
     6     1.87278433509846714D+00,     2.01564147795561000D+00,
     7     2.14064147795561000D+00,     2.25175258906672111D+00,
     8     2.35175258906672111D+00,     2.44266167997581202D+00,
     9     2.52599501330914535D+00,     2.60291809023222227D+00,
     A     2.67434666166079370D+00,     2.74101332832746037D+00,
     B     2.80351332832746037D+00,     2.86233685773922507D+00,
     C     2.91789241329478063D+00,     2.97052399224214905D+00,
     D     3.02052399224214905D+00,     3.06814303986119667D+00,
     E     3.11359758531574212D+00,     3.15707584618530734D+00/
      DATA C(25), C(26), C(27), C(28), C(29), C(30), C(31), C(32),
     1     C(33), C(34), C(35), C(36), C(37), C(38), C(39), C(40),
     2     C(41), C(42), C(43), C(44), C(45), C(46), C(47), C(48)/
     3     3.19874251285197401D+00,     3.23874251285197401D+00,
     4     3.27720405131351247D+00,     3.31424108835054951D+00,
     5     3.34995537406483522D+00,     3.38443813268552488D+00,
     6     3.41777146601885821D+00,     3.45002953053498724D+00,
     7     3.48127953053498724D+00,     3.51158256083801755D+00,
     8     3.54099432554389990D+00,     3.56956575411532847D+00,
     9     3.59734353189310625D+00,     3.62437055892013327D+00,
     A     3.65068634839381748D+00,     3.67632737403484313D+00,
     B     3.70132737403484313D+00,     3.72571761793728215D+00,
     C     3.74952714174680596D+00,     3.77278295570029433D+00,
     D     3.79551022842756706D+00,     3.81773245064978928D+00,
     E     3.83947158108457189D+00,     3.86074817682925274D+00/
      DATA C(49), C(50), C(51), C(52), C(53), C(54), C(55), C(56),
     1     C(57), C(58), C(59), C(60), C(61), C(62), C(63), C(64),
     2     C(65), C(66), C(67), C(68), C(69), C(70), C(71), C(72)/
     3     3.88158151016258607D+00,     3.90198967342789220D+00,
     4     3.92198967342789220D+00,     3.94159751656514710D+00,
     5     3.96082828579591633D+00,     3.97969621032421822D+00,
     6     3.99821472884273674D+00,     4.01639654702455492D+00,
     7     4.03425368988169777D+00,     4.05179754953082058D+00,
     8     4.06903892884116541D+00,     4.08598808138353829D+00,
     9     4.10265474805020496D+00,     4.11904819067315578D+00,
     A     4.13517722293122029D+00,     4.15105023880423617D+00,
     B     4.16667523880423617D+00,     4.18205985418885155D+00,
     C     4.19721136934036670D+00,     4.21213674247469506D+00,
     D     4.22684262482763624D+00,     4.24133537845082464D+00,
     E     4.25562109273653893D+00,     4.26970559977879245D+00/
      DATA C(73), C(74), C(75), C(76), C(77), C(78), C(79), C(80),
     1     C(81), C(82), C(83), C(84), C(85), C(86), C(87), C(88),
     2     C(89), C(90), C(91), C(92), C(93), C(94), C(95), C(96)/
     3     4.28359448866768134D+00,     4.29729311880466764D+00,
     4     4.31080663231818115D+00,     4.32413996565151449D+00,
     5     4.33729786038835659D+00,     4.35028487337536958D+00,
     6     4.36310538619588240D+00,     4.37576361404398366D+00,
     7     4.38826361404398366D+00,     4.40060929305632934D+00,
     8     4.41280441500754886D+00,     4.42485260777863319D+00,
     9     4.43675736968339510D+00,     4.44852207556574804D+00,
     A     4.46014998254249223D+00,     4.47164423541605544D+00,
     B     4.48300787177969181D+00,     4.49424382683587158D+00,
     C     4.50535493794698269D+00,     4.51634394893599368D+00,
     D     4.52721351415338499D+00,     4.53796620232542800D+00,
     E     4.54860450019776842D+00,     4.55913081598724211D+00/
      DATA C(97), C(98), C(99), C(100)/
     1     4.56954748265390877D+00,     4.57985676100442424D+00,
     2     4.59006084263707730D+00,     4.60016185273808740D+00/
C             COEFFICIENTS OF ASYMPTOTIC EXPANSION
      DATA B(1), B(2), B(3), B(4), B(5), B(6)/
     1     8.33333333333333333D-02,    -8.33333333333333333D-03,
     2     3.96825396825396825D-03,    -4.16666666666666666D-03,
     3     7.57575757575757576D-03,    -2.10927960927960928D-02/
C
      IF (N.GT.100) GO TO 10
      DPSIXN = C(N)
      RETURN
   10 CONTINUE
      WDTOL = DMAX1(D1MACH(4),1.0D-18)
      FN = DBLE(FLOAT(N))
      AX = 1.0D0
      S = -0.5D0/FN
      IF (DABS(S).LE.WDTOL) GO TO 30
      RFN2 = 1.0D0/(FN*FN)
      DO 20 K=1,6
        AX = AX*RFN2
        TRM = -B(K)*AX
        IF (DABS(TRM).LT.WDTOL) GO TO 30
        S = S + TRM
   20 CONTINUE
   30 CONTINUE
      DPSIXN = S + DLOG(FN)
      RETURN
      END
      SUBROUTINE ZMLT(AR, AI, BR, BI, CR, CI)
C***BEGIN PROLOGUE  ZMLT
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY
C
C     DOUBLE PRECISION COMPLEX MULTIPLY, C=A*B.
C
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  ZMLT
      DOUBLE PRECISION AR, AI, BR, BI, CR, CI, CA, CB
      CA = AR*BR - AI*BI
      CB = AR*BI + AI*BR
      CR = CA
      CI = CB
      RETURN
      END
      SUBROUTINE ZDIV(AR, AI, BR, BI, CR, CI)
C***BEGIN PROLOGUE  ZDIV
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY
C
C     DOUBLE PRECISION COMPLEX DIVIDE C=A/B.
C
C***ROUTINES CALLED  ZABS
C***END PROLOGUE  ZDIV
      EXTERNAL ZABS
      DOUBLE PRECISION AR, AI, BR, BI, CR, CI, BM, CA, CB, CC, CD
      DOUBLE PRECISION ZABS
      BM = 1.0D0/ZABS(BR,BI)
      CC = BR*BM
      CD = BI*BM
      CA = (AR*CC+AI*CD)*BM
      CB = (AI*CC-AR*CD)*BM
      CR = CA
      CI = CB
      RETURN
      END
      SUBROUTINE ZSQRT(AR, AI, BR, BI)
C***BEGIN PROLOGUE  ZSQRT
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY
C
C     DOUBLE PRECISION COMPLEX SQUARE ROOT, B=CSQRT(A)
C
C***ROUTINES CALLED  ZABS
C***END PROLOGUE  ZSQRT
      EXTERNAL ZABS
      DOUBLE PRECISION AR, AI, BR, BI, ZM, DTHETA, DPI, DRT
      DOUBLE PRECISION ZABS
      DATA DRT , DPI / 7.071067811865475244008443621D-1,
     1                 3.141592653589793238462643383D+0/
      ZM = ZABS(AR,AI)
      ZM = DSQRT(ZM)
      IF (AR.EQ.0.0D+0) GO TO 10
      IF (AI.EQ.0.0D+0) GO TO 20
      DTHETA = DATAN(AI/AR)
      IF (DTHETA.LE.0.0D+0) GO TO 40
      IF (AR.LT.0.0D+0) DTHETA = DTHETA - DPI
      GO TO 50
   10 IF (AI.GT.0.0D+0) GO TO 60
      IF (AI.LT.0.0D+0) GO TO 70
      BR = 0.0D+0
      BI = 0.0D+0
      RETURN
   20 IF (AR.GT.0.0D+0) GO TO 30
      BR = 0.0D+0
      BI = DSQRT(DABS(AR))
      RETURN
   30 BR = DSQRT(AR)
      BI = 0.0D+0
      RETURN
   40 IF (AR.LT.0.0D+0) DTHETA = DTHETA + DPI
   50 DTHETA = DTHETA*0.5D+0
      BR = ZM*DCOS(DTHETA)
      BI = ZM*DSIN(DTHETA)
      RETURN
   60 BR = ZM*DRT
      BI = ZM*DRT
      RETURN
   70 BR = ZM*DRT
      BI = -ZM*DRT
      RETURN
      END
      SUBROUTINE ZEXP(AR, AI, BR, BI)
C***BEGIN PROLOGUE  ZEXP
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY
C
C     DOUBLE PRECISION COMPLEX EXPONENTIAL FUNCTION B=EXP(A)
C
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  ZEXP
      DOUBLE PRECISION AR, AI, BR, BI, ZM, CA, CB
      ZM = DEXP(AR)
      CA = ZM*DCOS(AI)
      CB = ZM*DSIN(AI)
      BR = CA
      BI = CB
      RETURN
      END
      SUBROUTINE ZLOG(AR, AI, BR, BI, IERR)
C***BEGIN PROLOGUE  ZLOG
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY
C
C     DOUBLE PRECISION COMPLEX LOGARITHM B=CLOG(A)
C     IERR=0,NORMAL RETURN      IERR=1, Z=CMPLX(0.0,0.0)
C***ROUTINES CALLED  ZABS
C***END PROLOGUE  ZLOG
      EXTERNAL ZABS
      INTEGER IERR
      DOUBLE PRECISION AR, AI, BR, BI, ZM, DTHETA, DPI, DHPI
      DOUBLE PRECISION ZABS
      DATA DPI , DHPI  / 3.141592653589793238462643383D+0,
     1                   1.570796326794896619231321696D+0/
C
      IERR=0
      IF (AR.EQ.0.0D+0) GO TO 10
      IF (AI.EQ.0.0D+0) GO TO 20
      DTHETA = DATAN(AI/AR)
      IF (DTHETA.LE.0.0D+0) GO TO 40
      IF (AR.LT.0.0D+0) DTHETA = DTHETA - DPI
      GO TO 50
   10 IF (AI.EQ.0.0D+0) GO TO 60
      BI = DHPI
      BR = DLOG(DABS(AI))
      IF (AI.LT.0.0D+0) BI = -BI
      RETURN
   20 IF (AR.GT.0.0D+0) GO TO 30
      BR = DLOG(DABS(AR))
      BI = DPI
      RETURN
   30 BR = DLOG(AR)
      BI = 0.0D+0
      RETURN
   40 IF (AR.LT.0.0D+0) DTHETA = DTHETA + DPI
   50 ZM = ZABS(AR,AI)
      BR = DLOG(ZM)
      BI = DTHETA
      RETURN
   60 CONTINUE
      IERR=1
      RETURN
      END
      DOUBLE PRECISION FUNCTION ZABS(ZR, ZI)
C***BEGIN PROLOGUE  ZABS
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY
C
C     ZABS COMPUTES THE ABSOLUTE VALUE OR MAGNITUDE OF A DOUBLE
C     PRECISION COMPLEX VARIABLE CMPLX(ZR,ZI)
C
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  ZABS
      DOUBLE PRECISION ZR, ZI, U, V, Q, S
      U = DABS(ZR)
      V = DABS(ZI)
      S = U + V
C-----------------------------------------------------------------------
C     S*1.0D0 MAKES AN UNNORMALIZED UNDERFLOW ON CDC MACHINES INTO A
C     TRUE FLOATING ZERO
C-----------------------------------------------------------------------
      S = S*1.0D+0
      IF (S.EQ.0.0D+0) GO TO 20
      IF (U.GT.V) GO TO 10
      Q = U/V
      ZABS = V*DSQRT(1.D+0+Q*Q)
      RETURN
   10 Q = V/U
      ZABS = U*DSQRT(1.D+0+Q*Q)
      RETURN
   20 ZABS = 0.0D+0
      RETURN
      END
      INTEGER FUNCTION I1MACH(I)
C***BEGIN PROLOGUE  I1MACH
C***DATE WRITTEN   750101   (YYMMDD)
C***REVISION DATE  860501   (YYMMDD)
C***CATEGORY NO.  R1
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  FOX, P. A., (BELL LABS)
C           HALL, A. D., (BELL LABS)
C           SCHRYER, N. L., (BELL LABS)
C***PURPOSE  RETURN INTEGER MACHINE DEPENDENT CONSTANTS.
C***DESCRIPTION
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C   THESE MACHINE CONSTANT ROUTINES MUST BE ACTIVATED FOR
C   A PARTICULAR ENVIRONMENT.
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C     I1MACH CAN BE USED TO OBTAIN MACHINE-DEPENDENT PARAMETERS
C     FOR THE LOCAL MACHINE ENVIRONMENT.  IT IS A FUNCTION
C     SUBROUTINE WITH ONE (INPUT) ARGUMENT, AND CAN BE CALLED
C     AS FOLLOWS, FOR EXAMPLE
C
C          K = I1MACH(I)
C
C     WHERE I=1,...,16.  THE (OUTPUT) VALUE OF K ABOVE IS
C     DETERMINED BY THE (INPUT) VALUE OF I.  THE RESULTS FOR
C     VARIOUS VALUES OF I ARE DISCUSSED BELOW.
C
C  I/O UNIT NUMBERS.
C    I1MACH( 1) = THE STANDARD INPUT UNIT.
C    I1MACH( 2) = THE STANDARD OUTPUT UNIT.
C    I1MACH( 3) = THE STANDARD PUNCH UNIT.
C    I1MACH( 4) = THE STANDARD ERROR MESSAGE UNIT.
C
C  WORDS.
C    I1MACH( 5) = THE NUMBER OF BITS PER INTEGER STORAGE UNIT.
C    I1MACH( 6) = THE NUMBER OF CHARACTERS PER INTEGER STORAGE UNIT.
C
C  INTEGERS.
C    ASSUME INTEGERS ARE REPRESENTED IN THE S-DIGIT, BASE-A FORM
C
C               SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C
C               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,S-1.
C    I1MACH( 7) = A, THE BASE.
C    I1MACH( 8) = S, THE NUMBER OF BASE-A DIGITS.
C    I1MACH( 9) = A**S - 1, THE LARGEST MAGNITUDE.
C
C  FLOATING-POINT NUMBERS.
C    ASSUME FLOATING-POINT NUMBERS ARE REPRESENTED IN THE T-DIGIT,
C    BASE-B FORM
C               SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C               WHERE 0 .LE. X(I) .LT. B FOR I=1,...,T,
C               0 .LT. X(1), AND EMIN .LE. E .LE. EMAX.
C    I1MACH(10) = B, THE BASE.
C
C  SINGLE-PRECISION
C    I1MACH(11) = T, THE NUMBER OF BASE-B DIGITS.
C    I1MACH(12) = EMIN, THE SMALLEST EXPONENT E.
C    I1MACH(13) = EMAX, THE LARGEST EXPONENT E.
C
C  DOUBLE-PRECISION
C    I1MACH(14) = T, THE NUMBER OF BASE-B DIGITS.
C    I1MACH(15) = EMIN, THE SMALLEST EXPONENT E.
C    I1MACH(16) = EMAX, THE LARGEST EXPONENT E.
C
C  TO ALTER THIS FUNCTION FOR A PARTICULAR ENVIRONMENT,
C  THE DESIRED SET OF DATA STATEMENTS SHOULD BE ACTIVATED BY
C  REMOVING THE C FROM COLUMN 1.  ALSO, THE VALUES OF
C  I1MACH(1) - I1MACH(4) SHOULD BE CHECKED FOR CONSISTENCY
C  WITH THE LOCAL OPERATING SYSTEM.
C***REFERENCES  FOX P.A., HALL A.D., SCHRYER N.L.,*FRAMEWORK FOR A
C                 PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  I1MACH
C
      INTEGER IMACH(16),OUTPUT
      EQUIVALENCE (IMACH(4),OUTPUT)
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
C
C     DATA IMACH( 1) /    7 /
C     DATA IMACH( 2) /    2 /
C     DATA IMACH( 3) /    2 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   36 /
C     DATA IMACH( 6) /    4 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   33 /
C     DATA IMACH( 9) / Z1FFFFFFFF /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   24 /
C     DATA IMACH(12) / -256 /
C     DATA IMACH(13) /  255 /
C     DATA IMACH(14) /   60 /
C     DATA IMACH(15) / -256 /
C     DATA IMACH(16) /  255 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
C
C     DATA IMACH( 1) /   5 /
C     DATA IMACH( 2) /   6 /
C     DATA IMACH( 3) /   7 /
C     DATA IMACH( 4) /   6 /
C     DATA IMACH( 5) /  48 /
C     DATA IMACH( 6) /   6 /
C     DATA IMACH( 7) /   2 /
C     DATA IMACH( 8) /  39 /
C     DATA IMACH( 9) / O0007777777777777 /
C     DATA IMACH(10) /   8 /
C     DATA IMACH(11) /  13 /
C     DATA IMACH(12) / -50 /
C     DATA IMACH(13) /  76 /
C     DATA IMACH(14) /  26 /
C     DATA IMACH(15) / -50 /
C     DATA IMACH(16) /  76 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
C
C     DATA IMACH( 1) /      5 /
C     DATA IMACH( 2) /      6 /
C     DATA IMACH( 3) /      7 /
C     DATA IMACH( 4) /      6 /
C     DATA IMACH( 5) /     48 /
C     DATA IMACH( 6) /      6 /
C     DATA IMACH( 7) /      2 /
C     DATA IMACH( 8) /     39 /
C     DATA IMACH( 9) / O0007777777777777 /
C     DATA IMACH(10) /      8 /
C     DATA IMACH(11) /     13 /
C     DATA IMACH(12) /    -50 /
C     DATA IMACH(13) /     76 /
C     DATA IMACH(14) /     26 /
C     DATA IMACH(15) / -32754 /
C     DATA IMACH(16) /  32780 /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C     FOR FTN4
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    7 /
C     DATA IMACH( 4) / 6LOUTPUT /
C     DATA IMACH( 5) /   60 /
C     DATA IMACH( 6) /   10 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   48 /
C     DATA IMACH( 9) / 00007777777777777777B /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   47 /
C     DATA IMACH(12) / -929 /
C     DATA IMACH(13) / 1070 /
C     DATA IMACH(14) /   94 /
C     DATA IMACH(15) / -929 /
C     DATA IMACH(16) / 1069 /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C     FOR FTN5
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    7 /
C     DATA IMACH( 4) / L"OUTPUT" /
C     DATA IMACH( 5) /   60 /
C     DATA IMACH( 6) /   10 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   48 /
C     DATA IMACH( 9) / O"00007777777777777777" /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   47 /
C     DATA IMACH(12) / -929 /
C     DATA IMACH(13) / 1070 /
C     DATA IMACH(14) /   94 /
C     DATA IMACH(15) / -929 /
C     DATA IMACH(16) / 1069 /
C
C     MACHINE CONSTANTS FOR THE CRAY 1
C
C     DATA IMACH( 1) /   100 /
C     DATA IMACH( 2) /   101 /
C     DATA IMACH( 3) /   102 /
C     DATA IMACH( 4) /   101 /
C     DATA IMACH( 5) /    64 /
C     DATA IMACH( 6) /     8 /
C     DATA IMACH( 7) /     2 /
C     DATA IMACH( 8) /    63 /
C     DATA IMACH( 9) / 777777777777777777777B /
C     DATA IMACH(10) /     2 /
C     DATA IMACH(11) /    47 /
C     DATA IMACH(12) / -8189 /
C     DATA IMACH(13) /  8190 /
C     DATA IMACH(14) /    94 /
C     DATA IMACH(15) / -8099 /
C     DATA IMACH(16) /  8190 /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C
C     DATA IMACH( 1) /  11 /
C     DATA IMACH( 2) /  12 /
C     DATA IMACH( 3) /   8 /
C     DATA IMACH( 4) /  10 /
C     DATA IMACH( 5) /  16 /
C     DATA IMACH( 6) /   2 /
C     DATA IMACH( 7) /   2 /
C     DATA IMACH( 8) /  15 /
C     DATA IMACH( 9) / 32767 /
C     DATA IMACH(10) /  16 /
C     DATA IMACH(11) /   6 /
C     DATA IMACH(12) / -64 /
C     DATA IMACH(13) /  63 /
C     DATA IMACH(14) /  14 /
C     DATA IMACH(15) / -64 /
C     DATA IMACH(16) /  63 /
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    0 /
C     DATA IMACH( 4) /    6 /
C     DATA IMACH( 5) /   24 /
C     DATA IMACH( 6) /    3 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   23 /
C     DATA IMACH( 9) / 8388607 /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   23 /
C     DATA IMACH(12) / -127 /
C     DATA IMACH(13) /  127 /
C     DATA IMACH(14) /   38 /
C     DATA IMACH(15) / -127 /
C     DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /   43 /
C     DATA IMACH( 4) /    6 /
C     DATA IMACH( 5) /   36 /
C     DATA IMACH( 6) /    6 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   35 /
C     DATA IMACH( 9) / O377777777777 /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   27 /
C     DATA IMACH(12) / -127 /
C     DATA IMACH(13) /  127 /
C     DATA IMACH(14) /   63 /
C     DATA IMACH(15) / -127 /
C     DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     3 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    4 /
C     DATA IMACH( 4) /    1 /
C     DATA IMACH( 5) /   16 /
C     DATA IMACH( 6) /    2 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   15 /
C     DATA IMACH( 9) / 32767 /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   23 /
C     DATA IMACH(12) / -128 /
C     DATA IMACH(13) /  127 /
C     DATA IMACH(14) /   39 /
C     DATA IMACH(15) / -128 /
C     DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     4 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    4 /
C     DATA IMACH( 4) /    1 /
C     DATA IMACH( 5) /   16 /
C     DATA IMACH( 6) /    2 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   15 /
C     DATA IMACH( 9) / 32767 /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   23 /
C     DATA IMACH(12) / -128 /
C     DATA IMACH(13) /  127 /
C     DATA IMACH(14) /   55 /
C     DATA IMACH(15) / -128 /
C     DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR THE HP 9000
C
C     DATA IMACH( 1) /     5 /
C     DATA IMACH( 2) /     6 /
C     DATA IMACH( 3) /     6 /
C     DATA IMACH( 4) /     7 /
C     DATA IMACH( 5) /    32 /
C     DATA IMACH( 6) /     4 /
C     DATA IMACH( 7) /     2 /
C     DATA IMACH( 8) /    32 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /     2 /
C     DATA IMACH(11) /    24 /
C     DATA IMACH(12) /  -126 /
C     DATA IMACH(13) /   127 /
C     DATA IMACH(14) /    53 /
C     DATA IMACH(15) / -1015 /
C     DATA IMACH(16) /  1017 /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA IMACH( 1) /   5 /
C     DATA IMACH( 2) /   6 /
C     DATA IMACH( 3) /   7 /
C     DATA IMACH( 4) /   6 /
C     DATA IMACH( 5) /  32 /
C     DATA IMACH( 6) /   4 /
C     DATA IMACH( 7) /  16 /
C     DATA IMACH( 8) /  31 /
C     DATA IMACH( 9) / Z7FFFFFFF /
C     DATA IMACH(10) /  16 /
C     DATA IMACH(11) /   6 /
C     DATA IMACH(12) / -64 /
C     DATA IMACH(13) /  63 /
C     DATA IMACH(14) /  14 /
C     DATA IMACH(15) / -64 /
C     DATA IMACH(16) /  63 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    5 /
C     DATA IMACH( 4) /    6 /
C     DATA IMACH( 5) /   36 /
C     DATA IMACH( 6) /    5 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   35 /
C     DATA IMACH( 9) / "377777777777 /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   27 /
C     DATA IMACH(12) / -128 /
C     DATA IMACH(13) /  127 /
C     DATA IMACH(14) /   54 /
C     DATA IMACH(15) / -101 /
C     DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    5 /
C     DATA IMACH( 4) /    6 /
C     DATA IMACH( 5) /   36 /
C     DATA IMACH( 6) /    5 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   35 /
C     DATA IMACH( 9) / "377777777777 /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   27 /
C     DATA IMACH(12) / -128 /
C     DATA IMACH(13) /  127 /
C     DATA IMACH(14) /   62 /
C     DATA IMACH(15) / -128 /
C     DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGER ARITHMETIC.
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    5 /
C     DATA IMACH( 4) /    6 /
C     DATA IMACH( 5) /   32 /
C     DATA IMACH( 6) /    4 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   24 /
C     DATA IMACH(12) / -127 /
C     DATA IMACH(13) /  127 /
C     DATA IMACH(14) /   56 /
C     DATA IMACH(15) / -127 /
C     DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     16-BIT INTEGER ARITHMETIC.
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    5 /
C     DATA IMACH( 4) /    6 /
C     DATA IMACH( 5) /   16 /
C     DATA IMACH( 6) /    2 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   15 /
C     DATA IMACH( 9) / 32767 /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   24 /
C     DATA IMACH(12) / -127 /
C     DATA IMACH(13) /  127 /
C     DATA IMACH(14) /   56 /
C     DATA IMACH(15) / -127 /
C     DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES. FTN COMPILER
C
C     DATA IMACH( 1) /     5 /
C     DATA IMACH( 2) /     6 /
C     DATA IMACH( 3) /     1 /
C     DATA IMACH( 4) /     6 /
C     DATA IMACH( 5) /    36 /
C     DATA IMACH( 6) /     4 /
C     DATA IMACH( 7) /     2 /
C     DATA IMACH( 8) /    35 /
C     DATA IMACH( 9) / O377777777777 /
C     DATA IMACH(10) /     2 /
C     DATA IMACH(11) /    27 /
C     DATA IMACH(12) /  -128 /
C     DATA IMACH(13) /   127 /
C     DATA IMACH(14) /    60 /
C     DATA IMACH(15) / -1024 /
C     DATA IMACH(16) /  1023 /
C
C     MACHINE CONSTANTS FOR THE VAX 11/780
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    5 /
C     DATA IMACH( 4) /    6 /
C     DATA IMACH( 5) /   32 /
C     DATA IMACH( 6) /    4 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   24 /
C     DATA IMACH(12) / -127 /
C     DATA IMACH(13) /  127 /
C     DATA IMACH(14) /   56 /
C     DATA IMACH(15) / -127 /
C     DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR THE ELXSI 6400
C
C     DATA IMACH( 1) /     5 /
C     DATA IMACH( 2) /     6 /
C     DATA IMACH( 3) /     6 /
C     DATA IMACH( 4) /     6 /
C     DATA IMACH( 5) /    32 /
C     DATA IMACH( 6) /     4 /
C     DATA IMACH( 7) /     2 /
C     DATA IMACH( 8) /    32 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /     2 /
C     DATA IMACH(11) /    24 /
C     DATA IMACH(12) /  -126 /
C     DATA IMACH(13) /   127 /
C     DATA IMACH(14) /    53 /
C     DATA IMACH(15) / -1022 /
C     DATA IMACH(16) /  1023 /
C
C     MACHINE CONSTANTS FOR THE Z80 MICROPROCESSOR
C
C     DATA IMACH( 1) /    1 /
C     DATA IMACH( 2) /    1 /
C     DATA IMACH( 3) /    0 /
C     DATA IMACH( 4) /    1 /
C     DATA IMACH( 5) /   16 /
C     DATA IMACH( 6) /    2 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   15 /
C     DATA IMACH( 9) / 32767 /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   24 /
C     DATA IMACH(12) / -127 /
C     DATA IMACH(13) /  127 /
C     DATA IMACH(14) /   56 /
C     DATA IMACH(15) / -127 /
C     DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR THE IBM PC - MICROSOFT FORTRAN
C
C     DATA IMACH( 1) /     5 /
C     DATA IMACH( 2) /     6 /
C     DATA IMACH( 3) /     6 /
C     DATA IMACH( 4) /     0 /
C     DATA IMACH( 5) /    32 /
C     DATA IMACH( 6) /     4 /
C     DATA IMACH( 7) /     2 /
C     DATA IMACH( 8) /    31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /     2 /
C     DATA IMACH(11) /    24 /
C     DATA IMACH(12) /  -126 /
C     DATA IMACH(13) /   127 /
C     DATA IMACH(14) /    53 /
C     DATA IMACH(15) / -1022 /
C     DATA IMACH(16) /  1023 /
C
C     MACHINE CONSTANTS FOR THE IBM PC - PROFESSIONAL FORTRAN
C     AND LAHEY FORTRAN
C
C     DATA IMACH( 1) /     4 /
C     DATA IMACH( 2) /     7 /
C     DATA IMACH( 3) /     7 /
C     DATA IMACH( 4) /     0 /
C     DATA IMACH( 5) /    32 /
C     DATA IMACH( 6) /     4 /
C     DATA IMACH( 7) /     2 /
C     DATA IMACH( 8) /    31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /     2 /
C     DATA IMACH(11) /    24 /
C     DATA IMACH(12) /  -126 /
C     DATA IMACH(13) /   127 /
C     DATA IMACH(14) /    53 /
C     DATA IMACH(15) / -1022 /
C     DATA IMACH(16) /  1023 /
C
C***FIRST EXECUTABLE STATEMENT  I1MACH
      IF (I .LT. 1  .OR.  I .GT. 16) GO TO 10
C
      I1MACH=IMACH(I)
      RETURN
C
   10 CONTINUE
      WRITE(OUTPUT,9000)
9000  FORMAT('1ERROR    1 IN I1MACH - I OUT OF BOUNDS ')
C
C     CALL FDUMP
C
C
      STOP
      END
      DOUBLE PRECISION FUNCTION D1MACH(I)
C***BEGIN PROLOGUE  D1MACH
C***DATE WRITTEN   750101   (YYMMDD)
C***REVISION DATE  860501   (YYMMDD)
C***CATEGORY NO.  R1
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  FOX, P. A., (BELL LABS)
C           HALL, A. D., (BELL LABS)
C           SCHRYER, N. L., (BELL LABS)
C***PURPOSE  RETURN DOUBLE PRECISION MACHINE DEPENDENT CONSTANTS.
C***DESCRIPTION
C
C     D1MACH CAN BE USED TO OBTAIN MACHINE-DEPENDENT PARAMETERS
C     FOR THE LOCAL MACHINE ENVIRONMENT.  IT IS A FUNCTION
C     SUBPROGRAM WITH ONE (INPUT) ARGUMENT, AND CAN BE CALLED
C     AS FOLLOWS, FOR EXAMPLE
C
C          D = D1MACH(I)
C
C     WHERE I=1,...,5.  THE (OUTPUT) VALUE OF D ABOVE IS
C     DETERMINED BY THE (INPUT) VALUE OF I.  THE RESULTS FOR
C     VARIOUS VALUES OF I ARE DISCUSSED BELOW.
C
C  DOUBLE-PRECISION MACHINE CONSTANTS
C  D1MACH( 1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
C  D1MACH( 2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
C  D1MACH( 3) = B**(-T), THE SMALLEST RELATIVE SPACING.
C  D1MACH( 4) = B**(1-T), THE LARGEST RELATIVE SPACING.
C  D1MACH( 5) = LOG10(B)
C***REFERENCES  FOX P.A., HALL A.D., SCHRYER N.L.,*FRAMEWORK FOR A
C                 PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  D1MACH
C
      INTEGER SMALL(4)
      INTEGER LARGE(4)
      INTEGER RIGHT(4)
      INTEGER DIVER(4)
      INTEGER LOG10(4)
C
      DOUBLE PRECISION DMACH(5)
C
      EQUIVALENCE (DMACH(1),SMALL(1))
      EQUIVALENCE (DMACH(2),LARGE(1))
      EQUIVALENCE (DMACH(3),RIGHT(1))
      EQUIVALENCE (DMACH(4),DIVER(1))
      EQUIVALENCE (DMACH(5),LOG10(1))
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
C
C     DATA SMALL(1) / ZC00800000 /
C     DATA SMALL(2) / Z000000000 /
C
C     DATA LARGE(1) / ZDFFFFFFFF /
C     DATA LARGE(2) / ZFFFFFFFFF /
C
C     DATA RIGHT(1) / ZCC5800000 /
C     DATA RIGHT(2) / Z000000000 /
C
C     DATA DIVER(1) / ZCC6800000 /
C     DATA DIVER(2) / Z000000000 /
C
C     DATA LOG10(1) / ZD00E730E7 /
C     DATA LOG10(2) / ZC77800DC0 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
C
C     DATA SMALL(1) / O1771000000000000 /
C     DATA SMALL(2) / O0000000000000000 /
C
C     DATA LARGE(1) / O0777777777777777 /
C     DATA LARGE(2) / O0007777777777777 /
C
C     DATA RIGHT(1) / O1461000000000000 /
C     DATA RIGHT(2) / O0000000000000000 /
C
C     DATA DIVER(1) / O1451000000000000 /
C     DATA DIVER(2) / O0000000000000000 /
C
C     DATA LOG10(1) / O1157163034761674 /
C     DATA LOG10(2) / O0006677466732724 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
C
C     DATA SMALL(1) / O1771000000000000 /
C     DATA SMALL(2) / O7770000000000000 /
C
C     DATA LARGE(1) / O0777777777777777 /
C     DATA LARGE(2) / O7777777777777777 /
C
C     DATA RIGHT(1) / O1461000000000000 /
C     DATA RIGHT(2) / O0000000000000000 /
C
C     DATA DIVER(1) / O1451000000000000 /
C     DATA DIVER(2) / O0000000000000000 /
C
C     DATA LOG10(1) / O1157163034761674 /
C     DATA LOG10(2) / O0006677466732724 /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C     FOR FTN4
C
C     DATA SMALL(1) / 00564000000000000000B /
C     DATA SMALL(2) / 00000000000000000000B /
C
C     DATA LARGE(1) / 37757777777777777777B /
C     DATA LARGE(2) / 37157777777777777777B /
C
C     DATA RIGHT(1) / 15624000000000000000B /
C     DATA RIGHT(2) / 00000000000000000000B /
C
C     DATA DIVER(1) / 15634000000000000000B /
C     DATA DIVER(2) / 00000000000000000000B /
C
C     DATA LOG10(1) / 17164642023241175717B /
C     DATA LOG10(2) / 16367571421742254654B /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C     FOR FTN5
C
C     DATA SMALL(1) / O"00564000000000000000" /
C     DATA SMALL(2) / O"00000000000000000000" /
C
C     DATA LARGE(1) / O"37757777777777777777" /
C     DATA LARGE(2) / O"37157777777777777777" /
C
C     DATA RIGHT(1) / O"15624000000000000000" /
C     DATA RIGHT(2) / O"00000000000000000000" /
C
C     DATA DIVER(1) / O"15634000000000000000" /
C     DATA DIVER(2) / O"00000000000000000000" /
C
C     DATA LOG10(1) / O"17164642023241175717" /
C     DATA LOG10(2) / O"16367571421742254654" /
C
C     MACHINE CONSTANTS FOR THE CRAY 1
C
C     DATA SMALL(1) / 201354000000000000000B /
C     DATA SMALL(2) / 000000000000000000000B /
C
C     DATA LARGE(1) / 577767777777777777777B /
C     DATA LARGE(2) / 000007777777777777774B /
C
C     DATA RIGHT(1) / 376434000000000000000B /
C     DATA RIGHT(2) / 000000000000000000000B /
C
C     DATA DIVER(1) / 376444000000000000000B /
C     DATA DIVER(2) / 000000000000000000000B /
C
C     DATA LOG10(1) / 377774642023241175717B /
C     DATA LOG10(2) / 000007571421742254654B /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C
C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -
C     STATIC DMACH(5)
C
C     DATA SMALL/20K,3*0/,LARGE/77777K,3*177777K/
C     DATA RIGHT/31420K,3*0/,DIVER/32020K,3*0/
C     DATA LOG10/40423K,42023K,50237K,74776K/
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C
C     DATA SMALL(1), SMALL(2) / '20000000, '00000201 /
C     DATA LARGE(1), LARGE(2) / '37777777, '37777577 /
C     DATA RIGHT(1), RIGHT(2) / '20000000, '00000333 /
C     DATA DIVER(1), DIVER(2) / '20000000, '00000334 /
C     DATA LOG10(1), LOG10(2) / '23210115, '10237777 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.
C
C     DATA SMALL(1), SMALL(2) / O402400000000, O000000000000 /
C     DATA LARGE(1), LARGE(2) / O376777777777, O777777777777 /
C     DATA RIGHT(1), RIGHT(2) / O604400000000, O000000000000 /
C     DATA DIVER(1), DIVER(2) / O606400000000, O000000000000 /
C     DATA LOG10(1), LOG10(2) / O776464202324, O117571775714 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     THREE WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA SMALL(1), SMALL(2), SMALL(3) / 40000B,       0,       1 /
C     DATA LARGE(1), LARGE(2), LARGE(3) / 77777B, 177777B, 177776B /
C     DATA RIGHT(1), RIGHT(2), RIGHT(3) / 40000B,       0,    265B /
C     DATA DIVER(1), DIVER(2), DIVER(3) / 40000B,       0,    276B /
C     DATA LOG10(1), LOG10(2), LOG10(3) / 46420B,  46502B,  77777B /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     FOUR WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA SMALL(1), SMALL(2) /  40000B,       0 /
C     DATA SMALL(3), SMALL(4) /       0,       1 /
C     DATA LARGE(1), LARGE(2) /  77777B, 177777B /
C     DATA LARGE(3), LARGE(4) / 177777B, 177776B /
C     DATA RIGHT(1), RIGHT(2) /  40000B,       0 /
C     DATA RIGHT(3), RIGHT(4) /       0,    225B /
C     DATA DIVER(1), DIVER(2) /  40000B,       0 /
C     DATA DIVER(3), DIVER(4) /       0,    227B /
C     DATA LOG10(1), LOG10(2) /  46420B,  46502B /
C     DATA LOG10(3), LOG10(4) /  76747B, 176377B /
C
C     MACHINE CONSTANTS FOR THE HP 9000
C
C     D1MACH(1) = 2.8480954D-306
C     D1MACH(2) = 1.40444776D+306
C     D1MACH(3) = 2.22044605D-16
C     D1MACH(4) = 4.44089210D-16
C     D1MACH(5) = 3.01029996D-1
C
C     DATA SMALL(1), SMALL(2) / 00040000000B, 00000000000B /
C     DATA LARGE(1), LARGE(2) / 17737777777B, 37777777777B /
C     DATA RIGHT(1), RIGHT(2) / 07454000000B, 00000000000B /
C     DATA DIVER(1), DIVER(2) / 07460000000B, 00000000000B /
C     DATA LOG10(1), LOG10(2) / 07764642023B, 12047674777B /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA SMALL(1), SMALL(2) / Z00100000, Z00000000 /
C     DATA LARGE(1), LARGE(2) / Z7FFFFFFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z33100000, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z34100000, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z41134413, Z509F79FF /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
C
C     DATA SMALL(1), SMALL(2) / "033400000000, "000000000000 /
C     DATA LARGE(1), LARGE(2) / "377777777777, "344777777777 /
C     DATA RIGHT(1), RIGHT(2) / "113400000000, "000000000000 /
C     DATA DIVER(1), DIVER(2) / "114400000000, "000000000000 /
C     DATA LOG10(1), LOG10(2) / "177464202324, "144117571776 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
C
C     DATA SMALL(1), SMALL(2) / "000400000000, "000000000000 /
C     DATA LARGE(1), LARGE(2) / "377777777777, "377777777777 /
C     DATA RIGHT(1), RIGHT(2) / "103400000000, "000000000000 /
C     DATA DIVER(1), DIVER(2) / "104400000000, "000000000000 /
C     DATA LOG10(1), LOG10(2) / "177464202324, "476747767461 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1), SMALL(2) /    8388608,           0 /
C     DATA LARGE(1), LARGE(2) / 2147483647,          -1 /
C     DATA RIGHT(1), RIGHT(2) /  612368384,           0 /
C     DATA DIVER(1), DIVER(2) /  620756992,           0 /
C     DATA LOG10(1), LOG10(2) / 1067065498, -2063872008 /
C
C     DATA SMALL(1), SMALL(2) / O00040000000, O00000000000 /
C     DATA LARGE(1), LARGE(2) / O17777777777, O37777777777 /
C     DATA RIGHT(1), RIGHT(2) / O04440000000, O00000000000 /
C     DATA DIVER(1), DIVER(2) / O04500000000, O00000000000 /
C     DATA LOG10(1), LOG10(2) / O07746420232, O20476747770 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     16-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1), SMALL(2) /    128,      0 /
C     DATA SMALL(3), SMALL(4) /      0,      0 /
C
C     DATA LARGE(1), LARGE(2) /  32767,     -1 /
C     DATA LARGE(3), LARGE(4) /     -1,     -1 /
C
C     DATA RIGHT(1), RIGHT(2) /   9344,      0 /
C     DATA RIGHT(3), RIGHT(4) /      0,      0 /
C
C     DATA DIVER(1), DIVER(2) /   9472,      0 /
C     DATA DIVER(3), DIVER(4) /      0,      0 /
C
C     DATA LOG10(1), LOG10(2) /  16282,   8346 /
C     DATA LOG10(3), LOG10(4) / -31493, -12296 /
C
C     DATA SMALL(1), SMALL(2) / O000200, O000000 /
C     DATA SMALL(3), SMALL(4) / O000000, O000000 /
C
C     DATA LARGE(1), LARGE(2) / O077777, O177777 /
C     DATA LARGE(3), LARGE(4) / O177777, O177777 /
C
C     DATA RIGHT(1), RIGHT(2) / O022200, O000000 /
C     DATA RIGHT(3), RIGHT(4) / O000000, O000000 /
C
C     DATA DIVER(1), DIVER(2) / O022400, O000000 /
C     DATA DIVER(3), DIVER(4) / O000000, O000000 /
C
C     DATA LOG10(1), LOG10(2) / O037632, O020232 /
C     DATA LOG10(3), LOG10(4) / O102373, O147770 /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES. FTN COMPILER
C
C     DATA SMALL(1), SMALL(2) / O000040000000, O000000000000 /
C     DATA LARGE(1), LARGE(2) / O377777777777, O777777777777 /
C     DATA RIGHT(1), RIGHT(2) / O170540000000, O000000000000 /
C     DATA DIVER(1), DIVER(2) / O170640000000, O000000000000 /
C     DATA LOG10(1), LOG10(2) / O177746420232, O411757177572 /
C
C     MACHINE CONSTANTS FOR VAX 11/780
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C     ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSYEMS***
C     *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
C
C     DATA SMALL(1), SMALL(2) /        128,           0 /
C     DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C     DATA RIGHT(1), RIGHT(2) /       9344,           0 /
C     DATA DIVER(1), DIVER(2) /       9472,           0 /
C     DATA LOG10(1), LOG10(2) /  546979738,  -805796613 /
C
C     DATA SMALL(1), SMALL(2) / Z00000080, Z00000000 /
C     DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z00002480, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z00002500, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z209A3F9A, ZCFF884FB /
C
C     MACHINE CONSTANTS FOR VAX 11/780 (G-FLOATING)
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C     ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSYEMS***
C     *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
C
C     DATA SMALL(1), SMALL(2) /         16,           0 /
C     DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C     DATA RIGHT(1), RIGHT(2) /      15552,           0 /
C     DATA DIVER(1), DIVER(2) /      15568,           0 /
C     DATA LOG10(1), LOG10(2) /  1142112243, 2046775455 /
C
C     DATA SMALL(1), SMALL(2) / Z00000010, Z00000000 /
C     DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z00003CC0, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z00003CD0, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z44133FF3, Z79FF509F /
C
C     MACHINE CONSTANTS FOR THE ELXSI 6400
C     (ASSUMING REAL*8 IS THE DEFAULT DOUBLE PRECISION)
C
C     DATA SMALL(1), SMALL(2) / '00100000'X,'00000000'X /
C     DATA LARGE(1), LARGE(2) / '7FEFFFFF'X,'FFFFFFFF'X /
C     DATA RIGHT(1), RIGHT(2) / '3CB00000'X,'00000000'X /
C     DATA DIVER(1), DIVER(2) / '3CC00000'X,'00000000'X /
C     DATA LOG10(1), DIVER(2) / '3FD34413'X,'509F79FF'X /
C
C     MACHINE CONSTANTS FOR THE IBM PC - MICROSOFT FORTRAN
C
C     DATA SMALL(1), SMALL(2) / #00000000, #00100000 /
C     DATA LARGE(1), LARGE(2) / #FFFFFFFF, #7FEFFFFF /
C     DATA RIGHT(1), RIGHT(2) / #00000000, #3CA00000 /
C     DATA DIVER(1), DIVER(2) / #00000000, #3CB00000 /
C     DATA LOG10(1), LOG10(2) / #509F79FF, #3FD34413 /
C
C     MACHINE CONSTANTS FOR THE IBM PC - PROFESSIONAL FORTRAN
C     AND LAHEY FORTRAN
C
C     DATA SMALL(1), SMALL(2) / Z'00000000', Z'00100000' /
C     DATA LARGE(1), LARGE(2) / Z'FFFFFFFF', Z'7FEFFFFF' /
C     DATA RIGHT(1), RIGHT(2) / Z'00000000', Z'3CA00000' /
C     DATA DIVER(1), DIVER(2) / Z'00000000', Z'3CB00000' /
C     DATA LOG10(1), LOG10(2) / Z'509F79FF', Z'3FD34413' /
C
C***FIRST EXECUTABLE STATEMENT  D1MACH
      IF (I .LT. 1  .OR.  I .GT. 5)
     1   CALL XERROR( 'D1MACH -- I OUT OF BOUNDS',25,1,2)
C
      D1MACH = DMACH(I)
      RETURN
C
      END
      REAL FUNCTION R1MACH(I)
C***BEGIN PROLOGUE  R1MACH
C***DATE WRITTEN   790101   (YYMMDD)
C***REVISION DATE  860501   (YYMMDD)
C***CATEGORY NO.  R1
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  FOX, P. A., (BELL LABS)
C           HALL, A. D., (BELL LABS)
C           SCHRYER, N. L., (BELL LABS)
C***PURPOSE  RETURN SINGLE PRECISION MACHINE DEPENDENT CONSTANTS.
C***DESCRIPTION
C
C     R1MACH CAN BE USED TO OBTAIN MACHINE-DEPENDENT PARAMETERS
C     FOR THE LOCAL MACHINE ENVIRONMENT.  IT IS A FUNCTION
C     SUBROUTINE WITH ONE (INPUT) ARGUMENT, AND CAN BE CALLED
C     AS FOLLOWS, FOR EXAMPLE
C
C          A = R1MACH(I)
C
C     WHERE I=1,...,5.  THE (OUTPUT) VALUE OF A ABOVE IS
C     DETERMINED BY THE (INPUT) VALUE OF I.  THE RESULTS FOR
C     VARIOUS VALUES OF I ARE DISCUSSED BELOW.
C
C  SINGLE-PRECISION MACHINE CONSTANTS
C  R1MACH(1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
C  R1MACH(2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
C  R1MACH(3) = B**(-T), THE SMALLEST RELATIVE SPACING.
C  R1MACH(4) = B**(1-T), THE LARGEST RELATIVE SPACING.
C  R1MACH(5) = LOG10(B)
C***REFERENCES  FOX, P.A., HALL, A.D., SCHRYER, N.L, *FRAMEWORK FOR
C                 A PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHE-
C                 MATICAL SOFTWARE, VOL. 4, NO. 2, JUNE 1978,
C                 PP. 177-188.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  R1MACH
C
      INTEGER SMALL(2)
      INTEGER LARGE(2)
      INTEGER RIGHT(2)
      INTEGER DIVER(2)
      INTEGER LOG10(2)
C
      REAL RMACH(5)
C
      EQUIVALENCE (RMACH(1),SMALL(1))
      EQUIVALENCE (RMACH(2),LARGE(1))
      EQUIVALENCE (RMACH(3),RIGHT(1))
      EQUIVALENCE (RMACH(4),DIVER(1))
      EQUIVALENCE (RMACH(5),LOG10(1))
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
C
C     DATA RMACH(1) / Z400800000 /
C     DATA RMACH(2) / Z5FFFFFFFF /
C     DATA RMACH(3) / Z4E9800000 /
C     DATA RMACH(4) / Z4EA800000 /
C     DATA RMACH(5) / Z500E730E8 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700/6700/7700 SYSTEMS.
C
C     DATA RMACH(1) / O1771000000000000 /
C     DATA RMACH(2) / O0777777777777777 /
C     DATA RMACH(3) / O1311000000000000 /
C     DATA RMACH(4) / O1301000000000000 /
C     DATA RMACH(5) / O1157163034761675 /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C     FOR FTN4
C
C     DATA RMACH(1) / 00564000000000000000B /
C     DATA RMACH(2) / 37767777777777777776B /
C     DATA RMACH(3) / 16414000000000000000B /
C     DATA RMACH(4) / 16424000000000000000B /
C     DATA RMACH(5) / 17164642023241175720B /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C     FOR FTN5
C
C     DATA RMACH(1) / O"00564000000000000000" /
C     DATA RMACH(2) / O"37767777777777777776" /
C     DATA RMACH(3) / O"16414000000000000000" /
C     DATA RMACH(4) / O"16424000000000000000" /
C     DATA RMACH(5) / O"17164642023241175720" /
C
C     MACHINE CONSTANTS FOR THE CRAY 1
C
C     DATA RMACH(1) / 200034000000000000000B /
C     DATA RMACH(2) / 577767777777777777776B /
C     DATA RMACH(3) / 377224000000000000000B /
C     DATA RMACH(4) / 377234000000000000000B /
C     DATA RMACH(5) / 377774642023241175720B /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C
C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -
C     STATIC RMACH(5)
C
C     DATA SMALL/20K,0/,LARGE/77777K,177777K/
C     DATA RIGHT/35420K,0/,DIVER/36020K,0/
C     DATA LOG10/40423K,42023K/
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C
C     DATA SMALL(1), SMALL(2) / '20000000, '00000201 /
C     DATA LARGE(1), LARGE(2) / '37777777, '00000177 /
C     DATA RIGHT(1), RIGHT(2) / '20000000, '00000352 /
C     DATA DIVER(1), DIVER(2) / '20000000, '00000353 /
C     DATA LOG10(1), LOG10(2) / '23210115, '00000377 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.
C
C     DATA RMACH(1) / O402400000000 /
C     DATA RMACH(2) / O376777777777 /
C     DATA RMACH(3) / O714400000000 /
C     DATA RMACH(4) / O716400000000 /
C     DATA RMACH(5) / O776464202324 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     3 WORD DOUBLE PRECISION WITH FTN4
C
C     DATA SMALL(1), SMALL(2) / 40000B,       1 /
C     DATA LARGE(1), LARGE(2) / 77777B, 177776B /
C     DATA RIGHT(1), RIGHT(2) / 40000B,    325B /
C     DATA DIVER(1), DIVER(2) / 40000B,    327B /
C     DATA LOG10(1), LOG10(2) / 46420B,  46777B /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     4 WORD DOUBLE PRECISION WITH FTN4
C
C     DATA SMALL(1), SMALL(2) / 40000B,       1 /
C     DATA LARGE91), LARGE(2) / 77777B, 177776B /
C     DATA RIGHT(1), RIGHT(2) / 40000B,    325B /
C     DATA DIVER(1), DIVER(2) / 40000B,    327B /
C     DATA LOG10(1), LOG10(2) / 46420B,  46777B /
C
C     MACHINE CONSTANTS FOR THE HP 9000
C
C     R1MACH(1) = 1.17549435E-38
C     R1MACH(2) = 1.70141163E+38
C     R1MACH(3) = 5.960464478E-8
C     R1MACH(4) = 1.119209290E-7
C     R1MACH(5) = 3.01030010E-1
C
C     DATA SMALL(1) / 00040000000B /
C     DATA LARGE(1) / 17677777777B /
C     DATA RIGHT(1) / 06340000000B /
C     DATA DIVER(1) / 06400000000B /
C     DATA LOG10(1) / 07646420233B /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86  AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA RMACH(1) / Z00100000 /
C     DATA RMACH(2) / Z7FFFFFFF /
C     DATA RMACH(3) / Z3B100000 /
C     DATA RMACH(4) / Z3C100000 /
C     DATA RMACH(5) / Z41134413 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA OR KI PROCESSOR).
C
C     DATA RMACH(1) / "000400000000 /
C     DATA RMACH(2) / "377777777777 /
C     DATA RMACH(3) / "146400000000 /
C     DATA RMACH(4) / "147400000000 /
C     DATA RMACH(5) / "177464202324 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1) /    8388608 /
C     DATA LARGE(1) / 2147483647 /
C     DATA RIGHT(1) /  880803840 /
C     DATA DIVER(1) /  889192448 /
C     DATA LOG10(1) / 1067065499 /
C
C     DATA RMACH(1) / O00040000000 /
C     DATA RMACH(2) / O17777777777 /
C     DATA RMACH(3) / O06440000000 /
C     DATA RMACH(4) / O06500000000 /
C     DATA RMACH(5) / O07746420233 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     16-BIT INTEGERS  (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1), SMALL(2) /   128,     0 /
C     DATA LARGE(1), LARGE(2) / 32767,    -1 /
C     DATA RIGHT(1), RIGHT(2) / 13440,     0 /
C     DATA DIVER(1), DIVER(2) / 13568,     0 /
C     DATA LOG10(1), LOG10(2) / 16282,  8347 /
C
C     DATA SMALL(1), SMALL(2) / O000200, O000000 /
C     DATA LARGE(1), LARGE(2) / O077777, O177777 /
C     DATA RIGHT(1), RIGHT(2) / O032200, O000000 /
C     DATA DIVER(1), DIVER(2) / O032400, O000000 /
C     DATA LOG10(1), LOG10(2) / O037632, O020233 /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C
C     DATA RMACH(1) / O000400000000 /
C     DATA RMACH(2) / O377777777777 /
C     DATA RMACH(3) / O146400000000 /
C     DATA RMACH(4) / O147400000000 /
C     DATA RMACH(5) / O177464202324 /
C
C     MACHINE CONSTANTS FOR THE VAX 11/780
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C     ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSTEMS***
C     *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
C
C     DATA SMALL(1) /       128 /
C     DATA LARGE(1) /    -32769 /
C     DATA RIGHT(1) /     13440 /
C     DATA DIVER(1) /     13568 /
C     DATA LOG10(1) / 547045274 /
C
C     DATA SMALL(1) / Z00000080 /
C     DATA LARGE(1) / ZFFFF7FFF /
C     DATA RIGHT(1) / Z00003480 /
C     DATA DIVER(1) / Z00003500 /
C     DATA LOG10(1) / Z209B3F9A /
C
C     MACHINE CONSTANTS FOR THE ELXSI 6400
C     ASSUMING REAL*4 IS THE DEFAULT REAL
C
C     DATA SMALL(1) / '00800000'X /
C     DATA LARGE(1) / '7F7FFFFF'X /
C     DATA RIGHT(1) / '33800000'X /
C     DATA DIVER(1) / '34000000'X /
C     DATA LOG10(1) / '3E9A209B'X /
C
C     MACHINE CONSTANTS FOR THE Z80 MICROPROCESSOR
C
C     DATA SMALL(1), SMALL(2) /     0,    256 /
C     DATA LARGE(1), LARGE(2) /    -1,   -129 /
C     DATA RIGHT(1), RIGHT(2) /     0,  26880 /
C     DATA DIVER(1), DIVER(2) /     0,  27136 /
C     DATA LOG10(1), LOG10(2) /  8347,  32538 /
C
C     MACHINE CONSTANTS FOR THE IBM PC - MICROSOFT FORTRAN
C
C     DATA SMALL(1) / #00800000 /
C     DATA LARGE(1) / #7F7FFFFF /
C     DATA RIGHT(1) / #33800000 /
C     DATA DIVER(1) / #34000000 /
C     DATA LOG10(1) / #3E9A209A /
C
C     MACHINE CONSTANTS FOR THE IBM PC - PROFESSIONAL FORTRAN
C     AND LAHEY FORTRAN
C
C     DATA SMALL(1)/ Z'00800000' /
C     DATA LARGE(1)/ Z'7F7FFFFF' /
C     DATA RIGHT(1)/ Z'33800000' /
C     DATA DIVER(1)/ Z'34000000' /
C     DATA LOG10(1)/ Z'3E9A209A' /
C
C***FIRST EXECUTABLE STATEMENT  R1MACH
      IF (I .LT. 1  .OR.  I .GT. 5)
     1   CALL XERROR ( 'R1MACH -- I OUT OF BOUNDS',25,1,2)
C
      R1MACH = RMACH(I)
      RETURN
C
      END
      SUBROUTINE FDUMP
C***BEGIN PROLOGUE  FDUMP
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  850801   (YYMMDD)
C***CATEGORY NO.  R3
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  SYMBOLIC DUMP (SHOULD BE LOCALLY WRITTEN).
C***DESCRIPTION
C
C        ***NOTE*** MACHINE DEPENDENT ROUTINE
C        FDUMP IS INTENDED TO BE REPLACED BY A LOCALLY WRITTEN
C        VERSION WHICH PRODUCES A SYMBOLIC DUMP.  FAILING THIS,
C        IT SHOULD BE REPLACED BY A VERSION WHICH PRINTS THE
C        SUBPROGRAM NESTING LIST.  NOTE THAT THIS DUMP MUST BE
C        PRINTED ON EACH OF UP TO FIVE FILES, AS INDICATED BY THE
C        XGETUA ROUTINE.  SEE XSETUA AND XGETUA FOR DETAILS.
C
C     WRITTEN BY RON JONES, WITH SLATEC COMMON MATH LIBRARY SUBCOMMITTEE
C***REFERENCES  (NONE)
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  FDUMP
C***FIRST EXECUTABLE STATEMENT  FDUMP
      RETURN
      END
      SUBROUTINE XERROR(MESS,NMESS,NERR,LEVEL)
C***BEGIN PROLOGUE  XERROR
C***DATE WRITTEN   880401   (YYMMDD)
C***REVISION DATE  880401   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  AMOS, D. E., (SNLA)
C***PURPOSE  TO PRINT AN ERROR MESSAGE
C***DESCRIPTION
C
C     ABSTRACT
C        XERROR IS A DUMMY SLATEC LIBRARY ROUTINE TO PRINT AN ERROR
C        MESSAGE.  THE CALL LIST IS CONSISTENT WITH THE CORRESPONDING
C        SLATEC LIBRARY SUBROUTINE.
C
C     DESCRIPTION OF PARAMETERS
C      --INPUT--
C        MESSG - THE HOLLERITH MESSAGE TO BE PROCESSED.
C        NMESSG- THE ACTUAL NUMBER OF CHARACTERS IN MESSG.
C        NERR  - THE ERROR NUMBER ASSOCIATED WITH THIS MESSAGE.
C                (A DUMMY VARIABLE NOT USED, BUT NEEDED TO BE COMPATIBLE
C                 WITH THE CORRESPONDING SLATEC ROUTINE)
C        LEVEL - ERROR CATEGORY.
C                (A DUMMY VARIABLE NOT USED, BUT NEEDED TO BE COMPATIBLE
C                 WITH THE CORRESPONDING SLATEC ROUTINE)
C
C***REFERENCES  JONES R.E., KAHANER D.K., 'XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE', SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  NONE
C***END PROLOGUE  XERROR
      CHARACTER*(*) MESS
      INTEGER NMESS,NERR,LEVEL,NN,NR,K,I,KMIN,MIN
      IF (NMESS.LE.0) THEN
        PRINT *,' IN XERROR, NMESS IS OUT OF RANGE'
      ELSE
        NN=NMESS/70
        NR=NMESS-70*NN
        IF(NR.NE.0) NN=NN+1
        K=1
        PRINT 900
  900   FORMAT(/)
        DO 10 I=1,NN
          KMIN=MIN(K+69,NMESS)
          PRINT *, MESS(K:KMIN)
          K=K+70
   10   CONTINUE
        RETURN
      END IF
      END
      SUBROUTINE XERRWV(MESSG,NMESSG,NERR,LEVEL,NI,I1,I2,NR,R1,R2)
C***BEGIN PROLOGUE  XERRWV
C***DATE WRITTEN   880401   (YYMMDD)
C***REVISION DATE  880401   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  AMOS, D. E., (SNLA)
C***PURPOSE  PROCESS AN ERROR MESSAGE ALLOWING 2 INTEGER AND 2 REAL
C            VALUES TO BE INCLUDED IN THE MESSAGE.
C***DESCRIPTION
C
C     ABSTRACT
C        XERRWV IS A DUMMY SLATEC LIBRARY ROUTINE TO PRINT AN ERROR
C        MESSAGE AND PRINT UP TO 2 INTEGER VARIABLES AND 2 REAL
C        VARIABLES. THE CALL LIST IS CONSISTENT WITH THE CORRESPONDING
C        SLATEC LIBRARY SUBROUTINE.
C
C     DESCRIPTION OF PARAMETERS
C      --INPUT--
C        MESSG - THE HOLLERITH MESSAGE TO BE PROCESSED.
C        NMESSG- THE ACTUAL NUMBER OF CHARACTERS IN MESSG.
C        NERR  - THE ERROR NUMBER ASSOCIATED WITH THIS MESSAGE.
C                (A DUMMY VARIABLE NOT USED, BUT NEEDED TO BE COMPATIBLE
C                 WITH THE CORRESPONDING SLATEC ROUTINE)
C        LEVEL - ERROR CATEGORY.
C                (A DUMMY VARIABLE NOT USED, BUT NEEDED TO BE COMPATIBLE
C                 WITH THE CORRESPONDING SLATEC ROUTINE)
C        NI    - NUMBER OF INTEGER VALUES TO BE PRINTED. (0 TO 2)
C        I1    - FIRST INTEGER VALUE.
C        I2    - SECOND INTEGER VALUE.
C        NR    - NUMBER OF REAL VALUES TO BE PRINTED. (0 TO 2)
C        R1    - FIRST REAL VALUE.
C        R2    - SECOND REAL VALUE.
C
C***REFERENCES  JONES R.E., KAHANER D.K., 'XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE', SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  XERRWV
      CHARACTER*(*) MESSG
      INTEGER I1,I2,NI,NR,NMESSG,NERR,LEVEL
      REAL R1,R2
      CALL XERROR(MESSG,NMESSG,NERR,LEVEL)
      IF (NI.LT.0 .OR. NI.GT.2) THEN
        PRINT *,' IN XERRWV, NI IS OUT OF RANGE'
      ELSE
        IF (NI.NE.0) THEN
          IF (NI.EQ.1) THEN
            PRINT *,' I1 = ',I1
          ELSE
            PRINT *,' I1 , I2 =',I1,I2
          END IF
        END IF
      END IF
      IF (NR.LT.0 .OR. NI.GT.2) THEN
        PRINT *,' IN XERRWV, NR IS OUT OF RANGE'
      ELSE
        IF (NR.NE.0) THEN
          IF (NR.EQ.1) THEN
            PRINT *,' R1 =',R1
          ELSE
            PRINT *,' R1 , R2 =',R1,R2
          END IF
        END IF
      END IF
      RETURN
      END
