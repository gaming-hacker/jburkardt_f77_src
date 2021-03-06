
C Collected Algorithms from CACM
C Algorithm 449
C Solution of Linear Programming Problems in 0-1 Variables [H]
C Fiala, F., 1971
C ------------------------------------------------------------------
      SUBROUTINE MAXL01(MO,NO,NEST,M,N,AO,BO,A,B,B1,S1,C,X,S,SO,BC,T,
     +                  IND,INC,NESTEX,V,NOPT,OPTS,NI,NAT)
C THIS SUBROUTINE FINDS THE MAXIMUM AND ALL MAXIMIZING
C POINTS TO THE LINEAR OBJECTIVE FUNCTION (1) SUBJECT TO M-1
C LINEAR CONSRAINTS (2.I) WITH N(GREATER THAN 1) ZERO-ONE
C VARIABLES AND INTEGER COEFFICIENTS.
C THE MEANING OF THE INPUT PARAMETERS.
C MO, NO, NEST ARE THE ADJUSTABLE DIMENSIONS SPECIFYING THE
C UPPER BOUNDS FOR THE NUMBER OF ALL CONSTRAINTS, VARIABLES
C AND ALTERNATIVE OTPIMAL SOLUTIONS, RESPECTIVELY.
C M IS THE NUMBER OF CONSTRAINTS INCLUDING THE SUPPLEMENTARY
C ONE. N IS THE NUMBER OF THE VARIABLES. AO IS THE TWO-
C DIMENSIONAL ARRAY CONTAINING IN THE FIRST M ROWS AND N
C COLUMNS THE COEFFICIENTS OF ALL CONSTRAINTS. THE FIRST ROW
C CONTAINS THE COEFFICIENTS OF THE SUPPLEMENTARY CONSTRAINT.
C THE ONE-DIMENSIONAL ARRAY BO CONTAINS THE RIGHT-HAND SIDE
C TERMS OF THE CONSTRAINTS. BO(1) IS THE ABSOLUTE TERM OF
C THE OBJECTIVE FUNCTION. AO, BO REMAIN UNCHANGED DURING
C THE WHOLE PROCEDURE.
C THE MEANING OF THE AUXILIARY PARAMETERS.
C THE TWO-DIMENSIONAL ARRAY A OR THE ONE-DIMENSIONAL ARRAY B
C CONTAINS THE COEFFICIENTS OR THE RIGHT-HAND SIDE TERMS
C OF THE CURRENT SYSTEM OF CONSTRAINTS, RESPECTIVELY.
C VNEG IS THE SUM OF ALL NEGATIVE COEFFICIENTS IN THE
C OBJECTIVE FUNCTION MINUS 1.
C ITEST=1 OR 0 INDICATES IF THE WHOLE SYSTEM OF CONSTRAINTS
C IS REDUNDANT OR NOT, RESPECTIVELY. SIMILARLY, THE I-TH
C COMPONENT OF THE ONE-DIMENSIONAL ARRAY IND INDICATES
C WHETHER THE I-TH CONSTRAINT IS REDUNDANT OR NOT.
C THE ONE-DIMENSIONAL ARRAY X CONTAINS THE CURRENT PARTIAL
C SOLUTION. A FREE VARIABLE IS REPRESENTED BY A COMPONENT
C EQUAL TO 2.
C THE ONE-DIMENSIONAL ARRAY S OR BC OR T INDICATES THE ORDER
C AND MANNER IN WHICH THE FIXED VARIABLES WERE ASSIGNED
C THEIR VALUES OR THE BRANCHING POINTS OR THE BRANCHING
C POINTS IN WHICH THE ACCELERATIING TEST CAN BE APPLIED,
C RESPECTIVELY. NS IS THE NUMBER OF COMPONENTS IN S AND BC.
C THE ONE-DIMENSIONAL ARRAYS B1,S1,SO AND C HAVE AN
C AUXILIARY CHARACTER.
C THE MEANING OF THE OUTPUT PARAMETERS.
C INC=0 OR 1 MEANS THAT THE GIVEN PROBLEM IS CONSISTENT
C OR INCONSISTENT, RESPECTIVELY.
C NESTEX=0 OR 1 INDICATES THE ESTIMATED NUMBER OF
C FEASIBLE SOLUTIONS WAS NOT OR WAS EXCEEDED, RESPECTIVELY.
C V IS THE MAXIMAL VALUE OF THE OBJECTIVE FUNCTION.
C NOPT IS THE NUMBER OF ALL MAXIMIZING POINTS.
C THE TWO-DIMENSIONAL ARRAY OPTS CONTAINS IN THE FIRST NOPT
C ROWS ALL MAXIMIZING POINTS. A COMPONENT MAY BE EQUAL TO 2
C WHICH INDICATES THAT THE VALUE OF THE CORRESPONDING
C VARIABLE CAN BE ARBITRARY. NI OR NAT INDICATE THE NUMBER
C OF ITERATIONS OR THE NUMBER OF SUCCESSFUL APPLICATIONS OF
C THE ACCELERATING TEST, RESPECTIVELY.
C THE CALLING PROGRAM SHOULD CONTAIN THE FOLLOWING TYPE-
C STATEMENT
C INTEGER AO(MO,NO),A(MO,NO),BO(MO),B(MO),B1(MO),S1(MO),
C XC(NO),X(NO),S(NO),SO(NO),BC(NO),T(NO),IND(MO),V,
C XOPTS(NEST,NO)
c
C     .. Scalar Arguments ..
      INTEGER INC,M,MO,N,NAT,NEST,NESTEX,NI,NO,NOPT,V
C     ..
C     .. Array Arguments ..
      INTEGER A(MO,NO),AO(MO,NO),B(MO),B1(MO),BC(NO),BO(MO),C(NO),
     +        IND(MO),OPTS(NEST,NO),S(NO),S1(MO),SO(NO),T(NO),X(NO)
C     ..
C     .. Local Scalars ..
      INTEGER I,IJ,IP,IS,IT1,IT2,ITEST,ITSET,J,J1,K,L1,NEWV,NS,VNEG
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC IABS
C     ..
      PRINT *,'entering maxl01'
c
      INC = 0
      NESTEX = 0
      NOPT = 0
      NS = 0
      NI = 0
      NAT = 0
      DO 10 J = 1,N
          T(J) = 0
   10 CONTINUE
C COPY THE ARRAYS AO, BO.
      DO 30 I = 1,M
          B(I) = BO(I)
          DO 20 J = 1,N
              A(I,J) = AO(I,J)
   20     CONTINUE
   30 CONTINUE
C ADD THE SUPPLEMENTARY CONSTRAINT, DETERMINE THE INITIAL
C PARTIAL SOLUTION.
      VNEG = -1
      DO 40 J = 1,N
          X(J) = 2
          IF (A(1,J).LT.0) VNEG = VNEG + A(1,J)
   40 CONTINUE
      B(1) = VNEG
      V = VNEG
   50 DO 60 I = 1,M
          IND(I) = 0
   60 CONTINUE
C EXAMINE THE CURRENT SYSTEM OF CONSTRAINTS.
   70 DO 80 I = 1,M
          B1(I) = B(I)
   80 CONTINUE
      NI = NI + 1
      ITSET = 1
      DO 110 I = 1,M
          S1(I) = 0
          IF (IND(I).EQ.1) GO TO 110
          DO 90 J = 1,N
              IF (A(I,J).LT.0) B1(I) = B1(I) - A(I,J)
              S1(I) = S1(I) + IABS(A(I,J))
   90     CONTINUE
          IF (B1(I).LE.0) GO TO 100
          ITEST = 0
          GO TO 110

  100     IND(I) = 1
  110 CONTINUE
      IF (ITEST.EQ.1) GO TO 420
C THE SYSTEM CONTAINS AT LEAST ONE IRREDUNDANT INEQUALITY.
      DO 120 I = 1,M
          IF (IND(I).EQ.1) GO TO 120
          IF (S1(I)-B1(I).LT.0) GO TO 560
  120 CONTINUE
C THE SYSTEM DOES NOT CONTAIN ANY INCONSISTENT INEQUALITY.
C CONSIDER EACH INEQUALITY SEPERATELY.
      I = 1
  130 IF (IND(I).EQ.1) GO TO 360
      IF (S1(I)-B1(I).GT.0) GO TO 200
C SOME OF THE FREE VARIABLES ARE FORCED TO CERTAIN FIXED
C VALUES.
  140 DO 190 J = 1,N
          IF (A(I,J).EQ.0) GO TO 190
          NS = NS + 1
          BC(NS) = 1
          IF (A(I,J).LT.0) GO TO 160
          S(NS) = J
          X(J) = 1
          DO 150 IJ = 1,M
              B(IJ) = B(IJ) - A(IJ,J)
  150     CONTINUE
          GO TO 170

  160     S(NS) = -J
          X(J) = 0
  170     DO 180 IJ = 1,M
              A(IJ,J) = 0
  180     CONTINUE
  190 CONTINUE
      GO TO 70

  200 DO 210 J = 1,N
          C(J) = IABS(A(I,J))
  210 CONTINUE
      L1 = 1
  220 J = L1 + 1
  230 IF (C(L1).GE.C(J)) GO TO 240
      IP = C(L1)
      C(L1) = C(J)
      C(J) = IP
  240 J = J + 1
      IF (J.GT.N) GO TO 250
      GO TO 230

  250 L1 = L1 + 1
      IF (L1.LT.N) GO TO 220
  260 IF (C(L1).GT.0) GO TO 270
      L1 = L1 - 1
      GO TO 260

  270 IF (S1(I)-C(L1).LT.B1(I)) GO TO 140
      IF (S1(I)-C(1)-B(I).GE.0) GO TO 360
C ONE FREE VARIABLE IS FORCED TO A CERTAIN FIXED VALUE.
      NS = NS + 1
      BC(NS) = 1
  280 DO 290 J = 1,N
          IF (IABS(A(I,J)).EQ.C(1)) GO TO 300
  290 CONTINUE
  300 IF (A(I,J).LT.0) GO TO 330
  310 S(NS) = J
      X(J) = 1
      DO 320 IJ = 1,M
          B(IJ) = B(IJ) - A(IJ,J)
  320 CONTINUE
      GO TO 340

  330 S(NS) = -J
      X(J) = 0
  340 DO 350 IJ = 1,M
          A(IJ,J) = 0
  350 CONTINUE
      GO TO 70

  360 I = I + 1
      IF (I.LE.M) GO TO 130
      IF (NS.EQ.N) GO TO 480
C FIND A NEW BRANCHING POINT.
      DO 370 J = 1,N
          C(J) = IABS(A(1,J))
  370 CONTINUE
      DO 380 J = 2,N
          IF (C(1).GE.C(J)) GO TO 380
          C(1) = C(J)
  380 CONTINUE
      IF (C(1).EQ.0) GO TO 390
      NS = NS + 1
      BC(NS) = 0
      I = 1
      GO TO 280

  390 DO 410 J = 1,N
          DO 400 J1 = 1,NS
              IF (J.EQ.IABS(S(J1))) GO TO 410
  400     CONTINUE
          NS = NS + 1
          BC(NS) = 0
          GO TO 310

  410 CONTINUE
C THE SYSTEM OF CONSTRAINTS IS REDUNDANT. SOLVE AN
C UNCONSTRAINED PROBLEM.
  420 DO 470 J = 1,N
          IF (NS.EQ.N) GO TO 480
          IF ((X(J).NE.2) .OR. (A(1,J).EQ.0)) GO TO 470
          NS = NS + 1
          BC(NS) = 1
          IF (A(1,J).LT.0) GO TO 440
          S(NS) = J
          X(J) = 1
          DO 430 I = 1,M
              B(I) = B(I) - A(I,J)
  430     CONTINUE
          GO TO 450

  440     S(NS) = -J
          X(J) = 0
  450     DO 460 I = 1,M
              A(I,J) = 0
  460     CONTINUE
  470 CONTINUE
C FIND THE NEW VALUE OF THE OBJECTIVE FUNCTION.
C ADJUST THE ACCELERATION TEST SEQUENCE T.
  480 NEWV = 0
      DO 490 J = 1,N
          NEWV = NEWV + X(J)*AO(1,J)
  490 CONTINUE
      DO 500 J = 1,NS
          K = NS + 1 - J
          IF (BC(K).EQ.0) T(K) = 1
  500 CONTINUE
      IF (NEWV.GT.V) GO TO 510
      NOPT = NOPT + 1
      IF (NOPT.LE.NEST) GO TO 540
C THE ESTIMATED FIRST DIMENSION OF THE ARRAY OPTS IS
C EXCEEDED.
      NESTEX = 1
      RETURN
C THE NEW SOLUTION FOUND GIVES A BETTER VALUE TO THE
C OBJECTIVE FUNCTION. CHANGE THE SUPPLEMENTARY CONSTRAINT.
  510 NOPT = 1
      V = NEWV
      B(1) = V
      DO 520 J = 1,N
          IF (X(J).NE.1) GO TO 520
          B(1) = B(1) - AO(1,J)
  520 CONTINUE
      DO 530 J = 1,N
          SO(J) = S(J)
  530 CONTINUE
C MODIFY THE SET OPTS.
  540 DO 550 J = 1,N
          OPTS(NOPT,J) = X(J)
  550 CONTINUE
  560 IF (NS.EQ.0) GO TO 580
C QUESTION IF A BACKTRACKING IS POSSIBLE.
      IS = 0
      DO 570 J = 1,NS
          IS = IS + BC(J)
  570 CONTINUE
      IF (IS.LT.NS) GO TO 600
      IF (V.GT.VNEG) GO TO 590
C THE SYSTEM OF CONSTRAINTS IS INCONSISTENT. NO SOLUTIONS.
  580 INC = 1
      RETURN
C THE GIVEN PROBLEM HAS A SOLUTION. ALL SOLUTIONS HAVE BEEN
C FOUND.
  590 V = V + BO(1)
      RETURN
C THE BACKTRACKING IS POSSIBLE.
  600 DO 610 J1 = 1,NS
          K = NS + 1 - J1
          IF (BC(K).EQ.0) GO TO 620
  610 CONTINUE
  620 IF (T(K).EQ.1) GO TO 750
C BACKTRACKING.
  630 DO 740 J1 = K,NS
          DO 640 J = 1,N
              IF (J.EQ.IABS(S(J1))) GO TO 650
  640     CONTINUE
  650     IF (K.EQ.J1) GO TO 700
          IF (X(J).EQ.1) GO TO 670
          DO 660 I = 1,M
              A(I,J) = AO(I,J)
  660     CONTINUE
          GO TO 690

  670     DO 680 I = 1,M
              A(I,J) = AO(I,J)
              B(I) = B(I) + A(I,J)
  680     CONTINUE
  690     X(J) = 2
          GO TO 740

  700     S(K) = -S(K)
          BC(K) = 1
          X(J) = 1 - X(J)
          IF (X(J).EQ.0) GO TO 720
          DO 710 I = 1,M
              B(I) = B(I) - AO(I,J)
  710     CONTINUE
          GO TO 740

  720     DO 730 I = 1,M
              B(I) = B(I) + AO(I,J)
  730     CONTINUE
  740 CONTINUE
      NS = K
      GO TO 50
C THE ACCELERATING TEST.
  750 T(K) = 0
      IT1 = 0
      IT2 = 0
      DO 790 J1 = K,N
          DO 760 J = 1,N
              IF (J.EQ.IABS(SO(J1))) GO TO 770
  760     CONTINUE
  770     IF (K.EQ.J1) GO TO 780
          IF (((X(J).EQ.0).AND. (AO(1,J).GT.0)) .OR.
     +        ((X(J).EQ.1).AND. (AO(1,J).LT.0))) IT2 = IT2 +
     +        IABS(AO(1,J))
          GO TO 790

  780     IT1 = IABS(AO(1,J))
  790 CONTINUE
      IF (IT1.LE.IT2) GO TO 630
C THE APPLICATION OF THE ACCELERATING TEST WAS SUCCESSFUL.
      BC(K) = 1
      NAT = NAT + 1
      GO TO 560

      END
