      FUNCTION GAMLN ( X )

c*********************************************************************72
c
cc GAMLN computes the natural logarithm of the gamma function.
C
C         A CDC 6600 SUBROUTINE
C
C     AUTHORS
C         D.E. AMOS AND S.L. DANIEL
C         ALBUQUERQUE, NEW MEXICO, 87115
C         JANUARY, 1975
C
C     REFERENCES
C         ABRAMOWITZ, M. AND STEGUN, I.A. HANDBOOK OF MATHEMATICAL
C         FUNCTIONS. NBS APPLIED MATHEMATICS SERIES 55, U.S. GOVERNMENT
C         PRINTING OFFICE, WASHINGTON, D.C., CHAPTER 6.
C
C         AMOS, D.E., DANIEL, S.L. AND WESTON, M.K. CDC 6600
C         SUBROUTINES IBESS AND JBESS FOR BESSEL FUNCTIONS
C         I/SUB(NU)/(X) AND J/SUB(NU)/(X), X.GE.0, NU.GE.0.
C         ACM TRANS. MATH. SOFTWARE, MARCH, 1977.
C
C         HART, J.F., ET. AL. COMPUTER APPROXIMATIONS, WILEY, NEW YORK.
C         PP. 130-136, 1968.
C
C     ABSTRACT
C         GAMLN COMPUTES THE NATURAL LOG OF THE GAMMA FUNCTION FOR
C         X.GT.0. A RATIONAL CHEBYSHEV APPROXIMATION IS USED ON
C         8.LT.X.LT.1000., THE ASYMPTOTIC EXPANSION FOR X.GE.1000. AND
C         BACKWARD RECURSION FOR 0.LT.X.LT.8 FOR NON-INTEGRAL X. FOR
C         X=1.,...,8., GAMLN IS SET TO NATURAL LOGS OF FACTORIALS.
C
C     DESCRIPTION OF ARGUMENTS
C
C         INPUT
C           X      - X.GT.0
C
C         OUTPUT
C           GAMLN  - NATURAL LOG OF THE GAMMA FUNCTION AT X
C
C     ERROR CONDITIONS
C         IMPROPER INPUT ARGUMENT - A FATAL ERROR
c
      implicit none

      double precision dndx
      double precision dx
      double precision DXZ
      double precision fk
      double precision g(8)
      double precision gamln
      integer i
      integer ndx
      double precision p(5)
      double precision px
      double precision q(2)
      double precision rtwpil
      double precision rx
      double precision rxx
      double precision RXZ
      double precision sum
      double precision sxk
      double precision tx
      double precision x
      double precision XK
      double precision xlim1
      double precision xlim2
      double precision xz

      DATA XLIM1 / 8.0D+00 /
      DATA XLIM2 / 1000.0D+00 /
      DATA RTWPIL / 9.18938533204673D-01 /
      DATA  G              / 8.52516136106541D+00, 6.57925121201010D+00,
     1 4.78749174278205D+00, 3.17805383034795D+00, 1.79175946922806D+00,
     2 6.93147180559945D-01,         0.0D+00, 0.0D+00       /
      DATA  P              / 7.66345188000000D-04,-5.94095610520000D-04,
     1 7.93643110484500D-04,-2.77777775657725D-03, 8.33333333333169D-02/
      DATA  Q              /-2.77777777777778D-03, 8.33333333333333D-02/

      IF (X) 140, 140, 10
   10 DX = X - XLIM1
      IF (DX) 20, 110, 80
   20 IF (X-1.0D+00 ) 30, 130, 40
   30 XZ = X + 8.0D+00
      TX = X
      FK = -0.5D+00
      NDX = 7
      GO TO 50
   40 DX = DABS ( DX )
      NDX = DX
      DNDX = NDX
      NDX = NDX + 1
      IF ((DNDX-DX).EQ.0.0D+00 ) GO TO 120
      XZ = X + DNDX + 1.0D+00
      TX = 1.0D+00
      FK = 0.5D+00
   50 DXZ = XZ
      RXZ = 1.0D+0/DXZ
      RX = RXZ
      RXX = RX*RX
      XK = 1.0D+00
      DO I=1,NDX
        XK = XK - RXZ
        SXK = XK
        TX = TX*SXK
      end do
      SUM = (X-FK) * DLOG ( XZ ) - DLOG ( TX ) - XZ
      PX = P(1)
      DO I=2,5
        PX = PX*RXX + P(I)
      end do
      GAMLN = PX*RX + SUM + RTWPIL
      RETURN
   80 RX = 1.0D+00 / X
      RXX = RX*RX
      IF ((X-XLIM2).LT.0.0D+00 ) GO TO 90
      PX = Q(1)*RXX + Q(2)
      GAMLN = PX*RX + (X-0.5D+00) * DLOG ( X ) - X + RTWPIL
      RETURN
   90 PX = P(1)
      SUM = (X-0.5D+00)*DLOG(X) - X
      DO 100 I=2,5
        PX = PX*RXX + P(I)
  100 CONTINUE
      GAMLN = PX*RX + SUM + RTWPIL
      RETURN
  110 GAMLN = G(1)
      RETURN
  120 GAMLN = G(NDX)
      RETURN
  130 GAMLN = G(8)
      RETURN
  140 PRINT 99999, X
      STOP
99999 FORMAT (50H ARGUMENT FOR GAMLN IS LESS THAN OR EQUAL TO ZERO,,
     * 3H X=,E25.14)
      END
      SUBROUTINE IBESS ( KODE, ALPHA, N, X, Y )

c*********************************************************************72
c
cc IBESS computes a sequence of I Bessel functions.
C
C         A CDC 6600 SUBROUTINE
C
C     AUTHORS
C         D.E. AMOS AND S.L. DANIEL
C         SANDIA LABORATORIES
C         JANUARY, 1975
C
C     REFERENCES
C         ABRAMOWITZ, M. AND STEGUN, I.A. HANDBOOK OF MATHEMATICAL
C         FUNCTIONS. NBS APPLIED MATHEMATICS SERIES 55, U.S. GOVERNMENT
C         PRINTING OFFICE, WASHINGTON, D.C., CHAPTERS 9 AND 10.
C
C         AMOS, D.E., DANIEL, S.L. AND WESTON, M.K. CDC 6600
C         SUBROUTINES IBESS AND JBESS FOR BESSEL FUNCTIONS
C         I/SUB(NU)/(X) AND J/SUB(NU)/(X), X.GE.0, NU.GE.0.
C         ACM TRANS. MATH. SOFTWARE, MARCH, 1977.
C
C         OLVER, F.W.J. TABLES FOR BESSEL FUNCTIONS OF MODERATE OR
C         LARGE ORDERS. NPL MATHEMATICAL TABLES, VOL 6. HER MAJESTY-S
C         STATIONERY OFFICE, LONDON, 1962.
C
C         OLVER, F.W.J. THE ASYMPTOTIC EXPANSION OF BESSEL FUNCTIONS OF
C         LARGE ORDER. PHIL. TRANS. A, 247, PP. 328-368, 1954.
C
C     ABSTRACT
C         IBESS COMPUTES AN N MEMBER SEQUENCE OF I BESSEL FUNCTIONS
C         I/SUB(ALPHA+K-1)/(X), K=1,...,N OR SCALED BESSEL FUNCTIONS
C         EXP(-X)*I/SUB(ALPHA*K-1)/(X), K=1,...,N FOR NON-NEGATIVE ALPHA
C         AND X. A COMBINATION OF THE POWER SERIES, THE ASYMPTOTIC
C         EXPANSION FOR X TO INFINITY, AND THE UNIFORM ASYMPTOTIC
C         EXPANSION FOR NU TO INFINITY ARE APPLIED OVER SUBDIVISIONS OF
C         THE (NU,X) PLANE. FOR VALUES NOT COVERED BY ONE OF THESE
C         FORMULAE, THE ORDER IS INCREMENTED BY AN INTEGER SO THAT ONE
C         OF THESE FORMULAE APPLY. BACKWARD RECURSION IS USED TO REDUCE
C         ORDERS BY INTEGER VALUES. THE ASYMPTOTIC EXPANSION FOR X TO
C         INFINITY IS USED ONLY WHEN THE ENTIRE SEQUENCE (SPECIFICALLY
C         THE LAST MEMBER) LIES WITHIN THE REGION COVERED BY THE
C         EXPANSION. LEADING TERMS OF THESE EXPANSIONS ARE USED TO TEST
C         FOR OVER OR UNDERFLOW WHERE APPROPRIATE. IF A SEQUENCE IS
C         REQUESTED AND THE LAST MEMBER WOULD UNDERFLOW, THE RESULT IS
C         SET TO ZERO AND THE NEXT LOWER ORDER TRIED, ETC., UNTIL A
C         MEMBER COMES ON SCALE OR ALL ARE SET TO ZERO. AN OVERFLOW
C         CANNOT OCCUR WITH SCALING. IBESS CALLS FUNCTION GAMLN.
C
C     DESCRIPTION OF ARGUMENTS
C
C         INPUT
C           KODE   - A PARAMETER TO INDICATE THE SCALING OPTION
C                    KODE=1 RETURNS
C                           Y(K)=        I/SUB(ALPHA+K-1)/(X),
C                                K=1,...,N
C                    KODE=2 RETURNS
C                           Y(K)=EXP(-X)*I/SUB(ALPHA+K-1)/(X),
C                                K=1,...,N
C           ALPHA  - ORDER OF FIRST MEMBER OF THE SEQUENCE, ALPHA.GE.0
C           N      - NUMBER OF MEMBERS IN THE SEQUENCE, N.GE.1
C           X      - X.GE.0
C
C         OUTPUT
C           Y      - A VECTOR WHOSE FIRST N COMPONENTS CONTAIN
C                    VALUES FOR I/SUB(ALPHA+K-1)/(X) OR SCALED
C                    VALUES FOR EXP(-X)*I/SUB(ALPHA+K-1)/(X),
C                    K=1,...,N DEPENDING ON KODE
C
C     ERROR CONDITIONS
C         IMPROPER INPUT ARGUMENTS - A FATAL ERROR
C         OVERFLOW WITH KODE=1 - A FATAL ERROR
C         UNDERFLOW - A NON-FATAL ERROR
C
      implicit none

      integer n

      double precision ak
      double precision alpha
      double precision ap
      double precision arg
      double precision c(11,10)
      double precision c1(11,8)
      double precision c2(11,2)
      double precision ce
      double precision coef
      double precision dfn
      double precision dtm
      double precision dx
      double precision earg
      double precision elim
      double precision etx
      double precision fn
      double precision fnp1
      double precision fnu
      double precision gamln
      double precision gln
      integer i
      integer i1
      integer in
      integer inlim
      integer is
      integer j
      integer k
      integer kk
      integer km
      integer kode
      integer kp1
      integer kt
      integer nn
      integer ns
      integer nz
      double precision ra
      double precision rtpi
      double precision rttpi
      double precision s
      double precision s1
      double precision s2
      double precision sx
      double precision sxo2
      double precision t
      double precision t2
      double precision ta
      double precision tb
      double precision temp(3)
      double precision tfn
      double precision tm
      double precision tol
      double precision trx
      double precision x
      double precision xo2
      double precision xo2l
      double precision y(n)
      double precision z

      EQUIVALENCE (C(1,1),C1(1,1))
      EQUIVALENCE (C(1,9),C2(1,1))

      DATA ELIM,TOL        /        667.0D+00,       1.0D-15          /
      DATA RTPI,RTTPI      / 1.59154943091895D-01, 3.98942280401433D-01/
      DATA CE              / 3.45387763900000D+01/
      DATA INLIM           /          80         /
      DATA C1              /
     & -2.08333333333333D-01, 1.25000000000000D-01,
     &  0.0D+00,              0.0D+00,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,
     &  0.0D+00,              0.0D+00,
     &  0.0D+00,              3.34201388888889D-01,
     & -4.01041666666667D-01, 7.03125000000000D-02,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,
     &  0.0D+00,              0.0D+00,             
     & -1.02581259645062D+00, 1.84646267361111D+00,
     & -8.91210937500000D-01, 7.32421875000000D-02,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,
     &  0.0D+00,              0.0D+00,
     &  0.0D+00,              4.66958442342625D+00,
     & -1.12070026162230D+01, 8.78912353515625D+00,
     & -2.36408691406250D+00, 1.12152099609375D-01,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,
     & -2.82120725582002D+01, 8.46362176746007D+01,
     & -9.18182415432400D+01, 4.25349987453885D+01,
     & -7.36879435947963D+00, 2.27108001708984D-01,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              2.12570130039217D+02,
     & -7.65252468141182D+02, 1.05999045252800D+03,
     & -6.99579627376133D+02, 2.18190511744212D+02,
     & -2.64914304869516D+01, 5.72501420974731D-01,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,              
     & -1.91945766231841D+03, 8.06172218173731D+03,
     & -1.35865500064341D+04, 1.16553933368645D+04,
     & -5.30564697861340D+03, 1.20090291321635D+03,
     & -1.08090919788395D+02, 1.72772750258446D+00,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              2.02042913309661D+04,
     & -9.69805983886375D+04, 1.92547001232532D+05,
     & -2.03400177280416D+05, 1.22200464983017D+05,
     & -4.11926549688976D+04, 7.10951430248936D+03,
     & -4.93915304773088D+02, 6.07404200127348D+00, 
     &  0.0D+00,              0.0D+00       /

      DATA C2              /-2.42919187900551D+05, 1.31176361466298D+06,
     1-2.99801591853811D+06, 3.76327129765640D+06,-2.81356322658653D+06,
     2 1.26836527332162D+06,-3.31645172484564D+05, 4.52187689813627D+04,
     3-2.49983048181121D+03, 2.43805296995561D+01, 0.0D+00,
     4 3.28446985307204D+06,-1.97068191184322D+07, 5.09526024926646D+07,
     5-7.41051482115327D+07, 6.63445122747290D+07,-3.75671766607634D+07,
     6 1.32887671664218D+07,-2.78561812808645D+06, 3.08186404612662D+05,
     7-1.38860897537170D+04, 1.10017140269247D+02/
C
C TEST INPUT ARGUMENTS
C
      KT = 1
C
C TEST INPUT ARGUMENTS
C
      IF (N-1) 580, 10, 20
   10 KT = 2
   20 NN = N
      IF (KODE.LT.1 .OR. KODE.GT.2) GO TO 560
      IF (X) 590, 30, 80
   30 IF (ALPHA) 570, 40, 50
   40 Y(1) = 1.0D+00
      IF (N.EQ.1) RETURN
      I1 = 2
      GO TO 60
   50 I1 = 1
   60 DO I=I1,N
        Y(I) = 0.0D+00
      end do
      RETURN
   80 CONTINUE
      IF (ALPHA.LT.0.0D+00 ) GO TO 570
      DFN = DBLE ( N ) + DBLE ( ALPHA ) - 1.0D+00
      FNU = DFN
      IN = 0
      XO2 = X * 0.5D+00
      SXO2 = XO2 * XO2
      ETX = KODE - 1
      SX = ETX*X
c
C DECISION TREE FOR REGION WHERE SERIES, ASYMPTOTIC EXPANSION FOR X
C TO INFINITY AND ASYMPTOTIC EXPANSION FOR NU TO INFINITY ARE
C APPLIED.
c
      IF (SXO2.LE.(FNU+1.0D+00 )) GO TO 90
      IF (X.LE.12.0D+00 ) GO TO 110
      FN = 0.55D+00 *FNU*FNU
      FN = DMAX1 ( 17.0D+00, FN )
      IF (X.GE.FN) GO TO 430
      NS = DMAX1 ( 36.0D+00 - FNU, 0.0D+00 )
      DFN = DFN + DBLE( NS )
      FN = DFN
      IS = KT
      KM = N - 1 + NS
      IF (KM.GT.0) IS = 3
      GO TO 120
   90 FN = FNU
      FNP1 = FN + 1.0D+00
      XO2L = DLOG ( XO2 )
      IS = KT
      IF (X.LE.0.5D+00 ) GO TO 230
      NS = 0
  100 DFN = DFN + DBLE ( NS )
      FN = DFN
      FNP1 = FN + 1.0D+00
      IS = KT
      IF (N-1+NS.GT.0) IS = 3
      GO TO 230
  110 XO2L = DLOG ( XO2 )
      NS = SXO2 - FNU
      GO TO 100
  120 CONTINUE
C
C OVERFLOW TEST ON UNIFORM ASYMPTOTIC EXPANSION
C
      IF ( KODE .EQ. 2 ) GO TO 130
      IF (ALPHA.LT.1.0D+00 ) GO TO 150
      Z = X / ALPHA
      RA = DSQRT ( 1.0D+00 + Z * Z )
      GLN = DLOG ( ( 1.0D+00 + RA ) / Z )
      T = RA * ( 1.0D+00 - ETX ) + ETX / ( Z + RA )
      ARG = ALPHA*(T-GLN)
      IF (ARG.GT.ELIM) GO TO 600
      IF (KM.EQ.0) GO TO 140
  130 CONTINUE
C
C UNDERFLOW TEST ON UNIFORM ASYMPTOTIC EXPANSION
C
      Z = X / FN
      RA = DSQRT ( 1.0D+00 + Z * Z )
      GLN = DLOG((1.0D+00 + RA ) / Z )
      T = RA * ( 1.0D+00 - ETX ) + ETX / ( Z + RA )
      ARG = FN*(T-GLN)
  140 IF (ARG.LT.-ELIM) GO TO 280
      GO TO 190
  150 IF (X.GT.ELIM) GO TO 600
      GO TO 130
C
C UNIFORM ASYMPTOTIC EXPANSION FOR NU TO INFINITY
C
  160 IF (KM.NE.0) GO TO 170
      Y(1) = TEMP(3)
      RETURN
  170 TEMP(1) = TEMP(3)
      IN = NS
      KT = 1
  180 CONTINUE
      IS = 2
      DFN = DFN - 1.0D+00
      FN = DFN
      Z = X / FN
      RA = DSQRT ( 1.0D+00 + Z * Z )
      GLN = DLOG ( ( 1.0D+00 + RA ) / Z )
      T = RA * ( 1.0D+00 - ETX ) + ETX / ( Z + RA )
      ARG = FN*(T-GLN)
  190 COEF = DEXP ( ARG )
      T = 1.0D+00 / RA
      T2 = T*T
      T = T/FN
      S2 = 1.0D+00
      AP = 1.0D+00
      DO K=1,10
        KP1 = K + 1
        S1 = C(1,K)
        DO J=2,KP1
          S1 = S1*T2 + C(J,K)
        end do
        AP = AP*T
        AK = AP*S1
        S2 = S2 + AK
        IF ( DABS ( AK ) .LT. TOL ) GO TO 220
      end do
  220 CONTINUE
      TEMP(IS) = DSQRT ( T * RTPI ) * COEF * S2
      GO TO (180, 350, 500), IS
C
C SERIES FOR (X/2)**2.LE.NU+1
C
  230 CONTINUE
      GLN = GAMLN(FNP1)
      ARG = FN*XO2L - GLN - SX
      IF (ARG.LT.-ELIM) GO TO 300
      EARG = DEXP ( ARG )
  240 CONTINUE
      S = 1.0D+00
      AK = 3.0D+00
      T2 = 1.0D+00
      T = 1.0D+00
      S1 = FN
      DO K=1,17
        S2 = T2 + S1
        T = T*SXO2/S2
        S = S + T
        IF ( DABS ( T ) .LT. TOL ) GO TO 260
        T2 = T2 + AK
        AK = AK + 2.0D+00
        S1 = S1 + FN
      end do
  260 CONTINUE
      TEMP(IS) = S * EARG
      GO TO (270, 350, 490), IS
  270 EARG = EARG*FN/XO2
      DFN = DFN - 1.0D+00
      FN = DFN
      IS = 2
      GO TO 240
C
C SET UNDERFLOW VALUE AND UPDATE PARAMETERS
C
  280 Y(NN) = 0.0D+00
      NN = NN - 1
      DFN = DFN - 1.0D+00
      FN = DFN
      IF (NN-1) 340, 290, 130
  290 KT = 2
      IS = 2
      GO TO 130
  300 Y(NN) = 0.0D+00
      NN = NN - 1
      FNP1 = FN
      DFN = DFN - 1.0D+00
      FN = DFN
      IF (NN-1) 340, 310, 320
  310 KT = 2
      IS = 2
  320 IF (SXO2.LE.FNP1) GO TO 330
      GO TO 130
  330 ARG = ARG - XO2L + DLOG ( FNP1 )
      IF (ARG.LT.-ELIM) GO TO 300
      GO TO 230
  340 NZ = N - NN
      PRINT 99994, NZ, KODE, ALPHA, N, X
      RETURN
C
C BACKWARD RECURSION SECTION
C
  350 CONTINUE
      NZ = N - NN
      IF (NZ.NE.0) PRINT 99994, NZ, KODE, ALPHA, N, X
  360 GO TO (370, 420), KT
  370 CONTINUE
      S1 = TEMP(1)
      S2 = TEMP(2)
      DX = X
      TRX = 2.0D+00 / DX
      DTM = DFN*TRX
      TM = DTM
      IF (IN.EQ.0) GO TO 390
C
C BACKWARD RECUR TO INDEX ALPHA+NN-1
C
      DO I=1,IN
        S = S2
        S2 = TM*S2 + S1
        S1 = S
        DTM = DTM - TRX
        TM = DTM
      end do
      Y(NN) = S1
      IF (NN.EQ.1) RETURN
      Y(NN-1) = S2
      IF (NN.EQ.2) RETURN
      GO TO 400
  390 CONTINUE
C
C BACKWARD RECUR FROM INDEX ALPHA+NN-1 TO ALPHA
C
      Y(NN) = S1
      Y(NN-1) = S2
      IF (NN.EQ.2) RETURN
  400 K = NN + 1
      DO I=3,NN
        K = K - 1
        Y(K-2) = TM * Y(K-1) + Y(K)
        DTM = DTM - TRX
        TM = DTM
      end do
      RETURN
  420 Y(1) = TEMP(2)
      RETURN
C
C ASYMPTOTIC EXPANSION FOR X TO INFINITY
C
  430 CONTINUE
      EARG = RTTPI / DSQRT ( X )
      IF (KODE.EQ.2) GO TO 440
      IF (X.GT.ELIM) GO TO 600
      EARG = EARG * DEXP ( X )
  440 ETX = 8.0D+00*X
      IS = KT
      IN = 0
  450 DX = DFN + DFN
      DTM = DX*DX
      S1 = ETX
      TRX = S1
      DX = -(DTM-1.0D+00)/TRX
      T = DX
      TRX = 1.0D+00 + DX
      S = TRX
      S2 = 1.0D+00
      AK = 8.0D+00
      DO K=1,25
        S1 = S1 + ETX
        S2 = S2 + AK
        DX = S2
        TRX = DTM - DX
        AP = TRX
        T = -T*AP/S1
        S = S + T
        IF ( DABS ( T ) .LE. TOL ) GO TO 470
        AK = AK + 8.0D+00
      end do
  470 TEMP(IS) = S*EARG
      GO TO (480, 360), IS
  480 IS = 2
      DFN = DFN - 1.0D+00
      GO TO 450
C
C BACKWARD RECURSION WITH NORMALIZATION BY
C ASYMPTOTIC EXPANSION FOR NU TO INFINITY OR POWER SERIES.
c
  490 CONTINUE
c
C COMPUTATION OF LAST ORDER FOR SERIES NORMALIZATION
c
      KM = DMAX1 ( 3.0D+00 - FN, 0.0D+00 )
      TFN = FN + dble ( KM )
      TA = ( GLN + TFN - 0.9189385332D+00 - 0.0833333333D+00 / TFN )
     &  / ( TFN + 0.5D+00 )
      TA = XO2L - TA
      TB = - ( 1.0D+00 - 1.0D+00 / TFN ) / TFN
      IN = CE / ( - TA + DSQRT ( TA * TA - CE * TB ) ) + 1.5D+00
      IN = IN + KM
      GO TO 510
  500 CONTINUE
C
C COMPUTATION OF LAST ORDER FOR ASYMPTOTIC EXPANSION NORMALIZATION
C
      IN = CE / ( GLN + DSQRT ( GLN * GLN + T * CE ) ) + 1.5D+00
      IF (IN.GT.INLIM) GO TO 160
  510 DX = dble ( IN )
      DTM = DFN + DX
      DX = X
      TRX = 2.0D+00 / DX
      DTM = DTM*TRX
      TM = DTM
      TA = 0.0D+00
      TB = TOL
      KK = 1
  520 CONTINUE
C
C BACKWARD RECUR UNINDEXED
C
      DO I=1,IN
        S = TB
        TB = TM*TB + TA
        TA = S
        DTM = DTM - TRX
        TM = DTM
      end do
C
C NORMALIZATION
C
      IF (KK.NE.1) GO TO 540
      TA = (TA/TB)*TEMP(3)
      TB = TEMP(3)
      KK = 2
      IN = NS
      IF (NS.NE.0) GO TO 520
  540 Y(NN) = TB
      NZ = N - NN
      IF (NZ.NE.0) PRINT 99994, NZ, KODE, ALPHA, N, X
      IF (NN.EQ.1) RETURN
      TB = TM*TB + TA
      K = NN - 1
      Y(K) = TB
      IF (NN.EQ.2) RETURN
      DTM = DTM - TRX
      TM = DTM
      KM = K - 1
C
C BACKWARD RECUR INDEXED
C
      DO I=1,KM
        Y(K-1) = TM*Y(K) + Y(K+1)
        DTM = DTM - TRX
        TM = DTM
        K = K - 1
      end do

      RETURN
  560 PRINT 99999, KODE, ALPHA, N, X
      STOP
  570 PRINT 99998, KODE, ALPHA, N, X
      STOP
  580 PRINT 99997, KODE, ALPHA, N, X
      STOP
  590 PRINT 99996, KODE, ALPHA, N, X
      STOP
  600 PRINT 99995, KODE, ALPHA, N, X
      STOP
99999 FORMAT (51H0IBESS CALLED WITH SCALING OPTION, KODE, NOT 1 OR 2
     * /6H KODE=,I2,7H ALPHA=,E25.14,3H N=,I6,3H X=,E25.14)
99998 FORMAT (51H0IBESS CALLED WITH THE ORDER, ALPHA, LESS THAN ZERO
     * /6H KODE=,I2,7H ALPHA=,E25.14,3H N=,I6,3H X=,E25.14)
99997 FORMAT (34H0IBESS CALLED WITH N LESS THAN ONE/6H KODE=,
     * I2,7H ALPHA=,E25.14,3H N=,I6,3H X=,E25.14)
99996 FORMAT (35H0IBESS CALLED WITH X LESS THAN ZERO/6H KODE=,
     * I2,7H ALPHA=,E25.14,3H N=,I6,3H X=,E25.14)
99995 FORMAT (42H0OVERFLOW IN IBESS, X TOO LARGE FOR KODE=1/6H KODE=,
     * I2,7H ALPHA=,E25.14,3H N=,I6,3H X=,E25.14)
99994 FORMAT (25H0UNDERFLOW IN IBESS, LAST,I6,20H VALUE(S) OF Y ARRAY,
     * 17H WERE SET TO ZERO/6H KODE=,I2,7H ALPHA=,E25.14,3H N=,I6,
     * 3H X=,E25.14)
      END
      SUBROUTINE JAIRY ( X, RX, C, AI, DAI )

c*********************************************************************72
c
cc JAIRY computes the Airy function Ai(x) and its derivative.
C
C     CDC 6600 ROUTINE
C     1-2-74
C
C                  JAIRY COMPUTES THE AIRY FUNCTION AI(X)
C                   AND ITS DERIVATIVE DAI(X) FOR JBESS
C                                   INPUT
C
C         X - ARGUMENT, COMPUTED BY JBESS, X.LE.(ELIM2*1.5)**(2./3.)
C        RX - RX=SQRT(ABS(X)), COMPUTED BY JBESS
C         C - C=2.*(ABS(X)**1.5)/3., COMPUTED BY JBESS
C
C                                  OUTPUT
C
C        AI - VALUE OF FUNCTION AI(X)
C       DAI - VALUE OF THE DERIVATIVE DAI(X)
C
C                                WRITTEN BY
C
C                                D. E. AMOS
C                               S. L. DANIEL
C                               M. K. WESTON
C
      implicit none

      double precision a(15)
      double precision ai
      double precision ajn(19)
      double precision ajp(19)
      double precision ak1(14)
      double precision ak2(23)
      double precision ak3(14)
      double precision b(15)
      double precision c
      double precision ccv
      double precision con2
      double precision con3
      double precision con4
      double precision con5
      double precision cv
      double precision da(15)
      double precision dai
      double precision dajn(19)
      double precision dajp(19)
      double precision dak1(14)
      double precision dak2(24)
      double precision dak3(14)
      double precision db(15)
      double precision e1
      double precision e2
      double precision ec
      double precision f1
      double precision f2
      double precision fpi12
      integer i
      integer j
      integer m1
      integer m1d
      integer m2
      integer m2d
      integer m3
      integer m3d
      integer m4
      integer m4d
      integer n1
      integer n1d
      integer n2
      integer n2d
      integer n3
      integer n3d
      integer n4
      integer n4d
      double precision rtrx
      double precision rx
      double precision scv
      double precision t
      double precision temp1
      double precision temp2
      double precision tt
      double precision x

      DATA N1, N2, N3, N4 /14,23,19,15/
      DATA M1, M2, M3, M4 /12,21,17,13/
      DATA FPI12,CON2,CON3,CON4,CON5             / 1.30899693899575D+00,
     1 5.03154716196777D+00, 3.80004589867293D-01, 8.33333333333333D-01,
     2 8.66025403784439D-01/
      DATA  AK1            / 2.20423090987793D-01,-1.25290242787700D-01,
     1 1.03881163359194D-02, 8.22844152006343D-04,-2.34614345891226D-04,
     2 1.63824280172116D-05, 3.06902589573189D-07,-1.29621999359332D-07,
     3 8.22908158823668D-09, 1.53963968623298D-11,-3.39165465615682D-11,
     4 2.03253257423626D-12,-1.10679546097884D-14,-5.16169497785080D-15/
      DATA  AK2            / 2.74366150869598D-01, 5.39790969736903D-03,
     1-1.57339220621190D-03, 4.27427528248750D-04,-1.12124917399925D-04,
     2 2.88763171318904D-05,-7.36804225370554D-06, 1.87290209741024D-06,
     3-4.75892793962291D-07, 1.21130416955909D-07,-3.09245374270614D-08,
     4 7.92454705282654D-09,-2.03902447167914D-09, 5.26863056595742D-10,
     5-1.36704767639569D-10, 3.56141039013708D-11,-9.31388296548430D-12,
     6 2.44464450473635D-12,-6.43840261990955D-13, 1.70106030559349D-13,
     7-4.50760104503281D-14, 1.19774799164811D-14,-3.19077040865066D-15/
      DATA  AK3            / 2.80271447340791D-01,-1.78127042844379D-03,
     1 4.03422579628999D-05,-1.63249965269003D-06, 9.21181482476768D-08,
     2-6.52294330229155D-09, 5.47138404576546D-10,-5.24408251800260D-11,
     3 5.60477904117209D-12,-6.56375244639313D-13, 8.31285761966247D-14,
     4-1.12705134691063D-14, 1.62267976598129D-15,-2.46480324312426D-16/
      DATA  AJP            / 7.78952966437581D-02,-1.84356363456801D-01,
     1 3.01412605216174D-02, 3.05342724277608D-02,-4.95424702513079D-03,
     2-1.72749552563952D-03, 2.43137637839190D-04, 5.04564777517082D-05,
     3-6.16316582695208D-06,-9.03986745510768D-07, 9.70243778355884D-08,
     4 1.09639453305205D-08,-1.04716330588766D-09,-9.60359441344646D-11,
     5 8.25358789454134D-12, 6.36123439018768D-13,-4.96629614116015D-14,
     6-3.29810288929615D-15, 2.35798252031104D-16/
      DATA  AJN            / 3.80497887617242D-02,-2.45319541845546D-01,
     1 1.65820623702696D-01, 7.49330045818789D-02,-2.63476288106641D-02,
     2-5.92535597304981D-03, 1.44744409589804D-03, 2.18311831322215D-04,
     3-4.10662077680304D-05,-4.66874994171766D-06, 7.15218807277160D-07,
     4 6.52964770854633D-08,-8.44284027565946D-09,-6.44186158976978D-10,
     5 7.20802286505285D-11, 4.72465431717846D-12,-4.66022632547045D-13,
     6-2.67762710389189D-14, 2.36161316570019D-15/
      DATA   A             / 4.90275424742791D-01, 1.57647277946204D-03,
     1-9.66195963140306D-05, 1.35916080268815D-07, 2.98157342654859D-07,
     2-1.86824767559979D-08,-1.03685737667141D-09, 3.28660818434328D-10,
     3-2.57091410632780D-11,-2.32357655300677D-12, 9.57523279048255D-13,
     4-1.20340828049719D-13,-2.90907716770715D-15, 4.55656454580149D-15,
     5-9.99003874810259D-16/
      DATA   B             / 2.78593552803079D-01,-3.52915691882584D-03,
     1-2.31149677384994D-05, 4.71317842263560D-06,-1.12415907931333D-07,
     2-2.00100301184339D-08, 2.60948075302193D-09,-3.55098136101216D-11,
     3-3.50849978423875D-11, 5.83007187954202D-12,-2.04644828753326D-13,
     4-1.10529179476742D-13, 2.87724778038775D-14,-2.88205111009939D-15,
     5-3.32656311696166D-16/
      DATA N1D, N2D, N3D, N4D /14,24,19,15/
      DATA M1D, M2D, M3D, M4D /12,22,17,13/
      DATA  DAK1           / 2.04567842307887D-01,-6.61322739905664D-02,
     1-8.49845800989287D-03, 3.12183491556289D-03,-2.70016489829432D-04,
     2-6.35636298679387D-06, 3.02397712409509D-06,-2.18311195330088D-07,
     3-5.36194289332826D-10, 1.13098035622310D-09,-7.43023834629073D-11,
     4 4.28804170826891D-13, 2.23810925754539D-13,-1.39140135641182D-14/
      DATA  DAK2           / 2.93332343883230D-01,-8.06196784743112D-03,
     1 2.42540172333140D-03,-6.82297548850235D-04, 1.85786427751181D-04,
     2-4.97457447684059D-05, 1.32090681239497D-05,-3.49528240444943D-06,
     3 9.24362451078835D-07,-2.44732671521867D-07, 6.49307837648910D-08,
     4-1.72717621501538D-08, 4.60725763604656D-09,-1.23249055291550D-09,
     5 3.30620409488102D-10,-8.89252099772401D-11, 2.39773319878298D-11,
     6-6.48013921153450D-12, 1.75510132023731D-12,-4.76303829833637D-13,
     7 1.29498241100810D-13,-3.52679622210430D-14, 9.62005151585923D-15,
     8-2.62786914342292D-15/
      DATA  DAK3           / 2.84675828811349D-01, 2.53073072619080D-03,
     1-4.83481130337976D-05, 1.84907283946343D-06,-1.01418491178576D-07,
     2 7.05925634457153D-09,-5.85325291400382D-10, 5.56357688831339D-11,
     3-5.90889094779500D-12, 6.88574353784436D-13,-8.68588256452194D-14,
     4 1.17374762617213D-14,-1.68523146510923D-15, 2.55374773097056D-16/
      DATA  DAJP           / 6.53219131311457D-02,-1.20262933688823D-01,
     1 9.78010236263823D-03, 1.67948429230505D-02,-1.97146140182132D-03,
     2-8.45560295098867D-04, 9.42889620701976D-05, 2.25827860945475D-05,
     3-2.29067870915987D-06,-3.76343991136919D-07, 3.45663933559565D-08,
     4 4.29611332003007D-09,-3.58673691214989D-10,-3.57245881361895D-11,
     5 2.72696091066336D-12, 2.26120653095771D-13,-1.58763205238303D-14,
     6-1.12604374485125D-15, 7.31327529515367D-17/
      DATA  DAJN           / 1.08594539632967D-02, 8.53313194857091D-02,
     1-3.15277068113058D-01,-8.78420725294257D-02, 5.53251906976048D-02,
     2 9.41674060503241D-03,-3.32187026018996D-03,-4.11157343156826D-04,
     3 1.01297326891346D-04, 9.87633682208396D-06,-1.87312969812393D-06,
     4-1.50798500131468D-07, 2.32687669525394D-08, 1.59599917419225D-09,
     5-2.07665922668385D-10,-1.24103350500302D-11, 1.39631765331043D-12,
     6 7.39400971155740D-14,-7.32887475627500D-15/
      DATA   DA            / 4.91627321104601D-01, 3.11164930427489D-03,
     1 8.23140762854081D-05,-4.61769776172142D-06,-6.13158880534626D-08,
     2 2.87295804656520D-08,-1.81959715372117D-09,-1.44752826642035D-10,
     3 4.53724043420422D-11,-3.99655065847223D-12,-3.24089119830323D-13,
     4 1.62098952568741D-13,-2.40765247974057D-14, 1.69384811284491D-16,
     5 8.17900786477396D-16/
      DATA   DB            /-2.77571356944231D-01, 4.44212833419920D-03,
     1-8.42328522190089D-05,-2.58040318418710D-06, 3.42389720217621D-07,
     2-6.24286894709776D-09,-2.36377836844577D-09, 3.16991042656673D-10,
     3-4.40995691658191D-12,-5.18674221093575D-12, 9.64874015137022D-13,
     4-4.90190576608710D-14,-1.77253430678112D-14, 5.55950610442662D-15,
     5-7.11793337579530D-16/

      IF (X.LT.0.0D+00 ) GO TO 90
      IF (C.GT.5.0D+00 ) GO TO 60
      IF (X.GT.1.2D+00 ) GO TO 30

      T = (X+X-1.2D+00 )*CON4
      TT = T + T
      J = N1
      F1 = AK1(J)
      F2 = 0.0D+00
      DO I=1,M1
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + AK1(J)
        F2 = TEMP1
      end do
      AI = T*F1 - F2 + AK1(1)
      J = N1D
      F1 = DAK1(J)
      F2 = 0.0D+00
      DO I=1,M1D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DAK1(J)
        F2 = TEMP1
      end do
      DAI = -(T*F1-F2+DAK1(1))
      RETURN

   30 CONTINUE

      T = (X+X-CON2)*CON3
      TT = T + T
      J = N2
      F1 = AK2(J)
      F2 = 0.0D+00
      DO I=1,M2
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + AK2(J)
        F2 = TEMP1
      end do
      RTRX = DSQRT ( RX )
      EC = DEXP ( - C )
      AI = EC*(T*F1-F2+AK2(1))/RTRX
      J = N2D
      F1 = DAK2(J)
      F2 = 0.0D+00
      DO I=1,M2D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DAK2(J)
        F2 = TEMP1
      end do
      DAI = -EC*(T*F1-F2+DAK2(1))*RTRX
      RETURN

   60 CONTINUE

      T = 10.0D+00 / C - 1.0D+00
      TT = T + T
      J = N1
      F1 = AK3(J)
      F2 = 0.0D+00
      DO I=1,M1
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + AK3(J)
        F2 = TEMP1
      end do
      RTRX = DSQRT(RX)
      EC = DEXP ( - C )
      AI = EC*(T*F1-F2+AK3(1))/RTRX
      J = N1D
      F1 = DAK3(J)
      F2 = 0.0D+00
      DO I=1,M1D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DAK3(J)
        F2 = TEMP1
      end do
      DAI = -RTRX*EC*(T*F1-F2+DAK3(1))
      RETURN

   90 CONTINUE

      IF (C.GT.5.0D+00 ) GO TO 120
      T = 0.4D+00 * C - 1.0D+00
      TT = T + T
      J = N3
      F1 = AJP(J)
      E1 = AJN(J)
      F2 = 0.0D+00
      E2 = 0.0D+00
      DO I=1,M3
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + AJP(J)
        E1 = TT*E1 - E2 + AJN(J)
        F2 = TEMP1
        E2 = TEMP2
      end do
      AI = (T*E1-E2+AJN(1)) - X*(T*F1-F2+AJP(1))
      J = N3D
      F1 = DAJP(J)
      E1 = DAJN(J)
      F2 = 0.0D+00
      E2 = 0.0D+00
      DO I=1,M3D
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + DAJP(J)
        E1 = TT*E1 - E2 + DAJN(J)
        F2 = TEMP1
        E2 = TEMP2
      end do
      DAI = X*X*(T*F1-F2+DAJP(1)) + (T*E1-E2+DAJN(1))
      RETURN

  120 CONTINUE

      T = 10.0D+00 / C - 1.0D+00
      TT = T + T
      J = N4
      F1 = A(J)
      E1 = B(J)
      F2 = 0.0D+00
      E2 = 0.0D+00
      DO I=1,M4
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + A(J)
        E1 = TT*E1 - E2 + B(J)
        F2 = TEMP1
        E2 = TEMP2
      end do
      TEMP1 = T*F1 - F2 + A(1)
      TEMP2 = T*E1 - E2 + B(1)
      RTRX = DSQRT ( RX )
      CV = C - FPI12
      CCV = DCOS ( CV )
      SCV = DSIN ( CV )
      AI = (TEMP1*CCV-TEMP2*SCV)/RTRX
      J = N4D
      F1 = DA(J)
      E1 = DB(J)
      F2 = 0.0D+00
      E2 = 0.0D+00
      DO I=1,M4D
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + DA(J)
        E1 = TT*E1 - E2 + DB(J)
        F2 = TEMP1
        E2 = TEMP2
      end do
      TEMP1 = T*F1 - F2 + DA(1)
      TEMP2 = T*E1 - E2 + DB(1)
      E1 = CCV*CON5 + 0.5D+00 * SCV
      E2 = SCV*CON5 - 0.5D+00 * CCV
      DAI = (TEMP1*E1-TEMP2*E2)*RTRX

      RETURN
      END
      SUBROUTINE JBESS ( ALPHA, N, X, Y )

c*********************************************************************72
c
cc JBESS computes a sequence of J Bessel functions.
C
C         A CDC 6600 SUBROUTINE
C
C     AUTHORS
C         D.E. AMOS, S.L. DANIEL, AND M.K. WESTON
C         SANDIA LABORATORIES
C         JANUARY, 1975
C
C     REFERENCES
C         ABRAMOWITZ, M. AND STEGUN, I.A. HANDBOOK OF MATHEMATICAL
C         FUNCTIONS. NBS APPLIED MATHEMATICS SERIES 55, U.S. GOVERNMENT
C         PRINTING OFFICE, WASHINGTON, D.C., CHAPTERS 9 AND 10.
C
C         AMOS, D.E., DANIEL, S.L. AND WESTON, M.K. CDC 6600
C         SUBROUTINES IBESS AND JBESS FOR BESSEL FUNCTIONS
C         I/SUB(NU)/(X) AND J/SUB(NU)/(X), X.GE.0, NU.GE.0.
C         ACM TRANS. MATH. SOFTWARE, MARCH, 1977.
C
C         OLVER, F.W.J. TABLES FOR BESSEL FUNCTIONS OF MODERATE OR
C         LARGE ORDERS. NPL MATHEMATICAL TABLES, VOL 6. HER MAJESTY-S
C         STATIONERY OFFICE, LONDON, 1962.
C
C         OLVER, F.W.J. THE ASYMPTOTIC EXPANSION OF BESSEL FUNCTIONS OF
C         LARGE ORDER. PHIL. TRANS. A, 247, PP. 328-368, 1954.
C
C     ABSTRACT
C         JBESS COMPUTES AN N MEMBER SEQUENCE OF J BESSEL FUNCTIONS
C         J/SUB(ALPHA+K-1)/(X), K=1,...,N FOR NON-NEGATIVE ALPHA AND X.
C         A COMBINATION OF THE POWER SERIES, THE ASYMPTOTIC EXPANSION
C         FOR X TO INFINITY AND THE UNIFORM ASYMPTOTIC EXPANSION FOR
C         NU TO INFINITY ARE APPLIED OVER SUBDIVISIONS OF THE (NU,X)
C         PLANE. FOR VALUES OF (NU,X) NOT COVERED BY ONE OF THESE
C         FORMULAE, THE ORDER IS INCREMENTED OR DECREMENTED BY INTEGER
C         VALUES INTO A REGION WHERE ONE OF THE FORMULAE APPLY. BACKWARD
C         RECURSION IS APPLIED TO REDUCE ORDERS BY INTEGER VALUES EXCEPT
C         WHERE THE ENTIRE SEQUENCE LIES IN THE OSCILLATORY REGION. IN
C         THIS CASE FORWARD RECURSION IS STABLE AND VALUES FROM THE
C         ASYMPTOTIC EXPANSION FOR X TO INFINITY START THE RECURSION
C         WHEN IT IS EFFICIENT TO DO SO. LEADING TERMS OF THE SERIES AND
C         UNIFORM EXPANSION ARE TESTED FOR UNDERFLOW. IF A SEQUENCE IS
C         REQUESTED AND THE LAST MEMBER WOULD UNDERFLOW, THE RESULT IS
C         SET TO ZERO AND THE NEXT LOWER ORDER TRIED, ETC., UNTIL A
C         MEMBER COMES ON SCALE OR ALL MEMBERS ARE SET TO ZERO. OVERFLOW
C         CANNOT OCCUR. JBESS CALLS SUBROUTINE JAIRY AND FUNCTION GAMLN.
C
C     DESCRIPTION OF ARGUMENTS
C
C         INPUT
C           ALPHA  - ORDER OF FIRST MEMBER OF THE SEQUENCE, ALPHA.GE.0
C           N      - NUMBER OF MEMBERS IN THE SEQUENCE, N.GE.1
C           X      - X.GE.0
C
C         OUTPUT
C           Y      - A VECTOR WHOSE FIRST N COMPONENTS CONTAIN
C                    VALUES FOR J/SUB(ALPHA+K-1)/(X), K=1,...,N
C
C     ERROR CONDITIONS
C         IMPROPER INPUT ARGUMENTS - A FATAL ERROR
C         UNDERFLOW  - A NON-FATAL ERROR
C
      implicit none

      integer n

      double precision abw2
      double precision acz
      double precision ai
      double precision ak
      double precision alfa(26,4)
      double precision alfa1(26,2)
      double precision alfa2(26,2)
      double precision alpha
      double precision ap
      double precision ar(8)
      double precision arg
      double precision ary
      double precision asum
      double precision az
      double precision az32
      double precision beta(26,5)
      double precision beta1(26,2)
      double precision beta2(26,2)
      double precision beta3(26,1)
      double precision br(10)
      double precision bsum
      double precision c(11,10)
      double precision c1(11,8)
      double precision c2(11,2)
      double precision ce
      double precision con1
      double precision con2
      double precision con548
      double precision cr(10)
      double precision crz32
      double precision cz
      double precision dai
      double precision dalpha
      double precision dfn
      double precision dr(10)
      double precision dtm
      double precision dx
      double precision earg
      double precision elim1
      double precision elim2
      double precision etx
      double precision fn
      double precision fn13
      double precision fn2
      double precision fnp1
      double precision fnu
      double precision fnulim(2)
      double precision gama(26)
      double precision gamln
      double precision gln
      integer i
      integer i1
      integer in
      integer inlim
      integer inp1
      integer is
      integer j
      integer jr
      integer ju
      integer k
      integer kb
      integer kk
      integer klast
      integer km
      integer kmax(5)
      integer kp1
      integer ks
      integer ksp1
      integer kt
      integer lr
      integer lrp1
      integer nn
      integer ns
      integer nz
      double precision pdf
      double precision phi
      double precision pidt
      double precision pp(4)
      double precision ra
      double precision rcz
      double precision rden
      double precision relb
      double precision rfn2
      double precision rtary
      double precision rttp
      double precision rtwo
      double precision rtx
      double precision rtz
      double precision rzden
      double precision s
      double precision s1
      double precision s2
      double precision sa
      double precision sb
      double precision suma
      double precision sumb
      double precision sxo2
      double precision t
      double precision t1
      double precision t2
      double precision ta
      double precision tau
      double precision tb
      double precision tce
      double precision temp(3)
      double precision tfn
      double precision tm
      double precision tol
      double precision tols
      double precision trx
      double precision tx
      double precision upol(10)
      double precision w2
      double precision x
      double precision xo2
      double precision xo2l
      double precision xx
      double precision y(n)
      double precision z
      double precision z32

      EQUIVALENCE (C(1,1),C1(1,1))
      EQUIVALENCE (C(1,9),C2(1,1))
      EQUIVALENCE (ALFA(1,1),ALFA1(1,1))
      EQUIVALENCE (ALFA(1,3),ALFA2(1,1))
      EQUIVALENCE (BETA(1,1),BETA1(1,1))
      EQUIVALENCE (BETA(1,3),BETA2(1,1))
      EQUIVALENCE (BETA(1,5),BETA3(1,1))
      DATA ELIM1,ELIM2,TOL /   667.0D+00, 644.0D+00, 1.0D-15 /

      DATA PP              / 8.72909153935547D+00, 2.65693932265030D-01,
     1 1.24578576865586D-01, 7.70133747430388D-04/
C TOLS=LN(1.D-3)
      DATA TOLS            /-6.90775527898214D+00/
      DATA CON1,CON2,CON548/ 6.66666666666667D-01, 3.33333333333333D-01,
     1 1.04166666666667D-01/
      DATA RTWO,PDF,RTTP,PIDT                    / 1.34839972492648D+00,
     1 7.85398163397448D-01, 7.97884560802865D-01, 1.57079632679490D+00/
      DATA FNULIM          / 100.0D+00,         60.0D+00         /
C CE=-DLOG(TOL) , TCE=-0.75*DLOG(TOL)
      DATA CE , TCE        / 3.45387763949107D+01, 2.59040822961830D+01/
      DATA INLIM           /         150         /
      DATA   AR            / 8.35503472222222D-02, 1.28226574556327D-01,
     1 2.91849026464140D-01, 8.81627267443758D-01, 3.32140828186277D+00,
     2 1.49957629868626D+01, 7.89230130115865D+01, 4.74451538868264D+02/
      DATA   BR            /-1.45833333333333D-01,-9.87413194444444D-02,
     1-1.43312053915895D-01,-3.17227202678414D-01,-9.42429147957120D-01,
     2-3.51120304082635D+00,-1.57272636203680D+01,-8.22814390971859D+01,
     3-4.92355370523671D+02,-3.31621856854797D+03/
      DATA C1              /
     & -2.08333333333333D-01, 1.25000000000000D-01,
     &  0.0D+00,              0.0D+00,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,
     &  0.0D+00,              0.0D+00,
     &  0.0D+00,              3.34201388888889D-01,
     & -4.01041666666667D-01, 7.03125000000000D-02,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,
     &  0.0D+00,              0.0D+00,             
     & -1.02581259645062D+00, 1.84646267361111D+00,
     & -8.91210937500000D-01, 7.32421875000000D-02,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,
     &  0.0D+00,              0.0D+00,
     &  0.0D+00,              4.66958442342625D+00,
     & -1.12070026162230D+01, 8.78912353515625D+00,
     & -2.36408691406250D+00, 1.12152099609375D-01,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,
     & -2.82120725582002D+01, 8.46362176746007D+01,
     & -9.18182415432400D+01, 4.25349987453885D+01,
     & -7.36879435947963D+00, 2.27108001708984D-01,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              2.12570130039217D+02,
     & -7.65252468141182D+02, 1.05999045252800D+03,
     & -6.99579627376133D+02, 2.18190511744212D+02,
     & -2.64914304869516D+01, 5.72501420974731D-01,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              0.0D+00,              
     & -1.91945766231841D+03, 8.06172218173731D+03,
     & -1.35865500064341D+04, 1.16553933368645D+04,
     & -5.30564697861340D+03, 1.20090291321635D+03,
     & -1.08090919788395D+02, 1.72772750258446D+00,
     &  0.0D+00,              0.0D+00,              
     &  0.0D+00,              2.02042913309661D+04,
     & -9.69805983886375D+04, 1.92547001232532D+05,
     & -2.03400177280416D+05, 1.22200464983017D+05,
     & -4.11926549688976D+04, 7.10951430248936D+03,
     & -4.93915304773088D+02, 6.07404200127348D+00, 
     &  0.0D+00,              0.0D+00       /

      DATA C2              /-2.42919187900551D+05, 1.31176361466298D+06,
     1-2.99801591853811D+06, 3.76327129765640D+06,-2.81356322658653D+06,
     2 1.26836527332162D+06,-3.31645172484564D+05, 4.52187689813627D+04,
     3-2.49983048181121D+03, 2.43805296995561D+01, 0.0D+00,
     4 3.28446985307204D+06,-1.97068191184322D+07, 5.09526024926646D+07,
     5-7.41051482115327D+07, 6.63445122747290D+07,-3.75671766607634D+07,
     6 1.32887671664218D+07,-2.78561812808645D+06, 3.08186404612662D+05,
     7-1.38860897537170D+04, 1.10017140269247D+02/
      DATA ALFA1           /-4.44444444444444D-03,-9.22077922077922D-04,
     1-8.84892884892885D-05, 1.65927687832450D-04, 2.46691372741793D-04,
     2 2.65995589346255D-04, 2.61824297061501D-04, 2.48730437344656D-04,
     3 2.32721040083232D-04, 2.16362485712365D-04, 2.00738858762752D-04,
     4 1.86267636637545D-04, 1.73060775917876D-04, 1.61091705929016D-04,
     5 1.50274774160908D-04, 1.40503497391270D-04, 1.31668816545923D-04,
     6 1.23667445598253D-04, 1.16405271474738D-04, 1.09798298372713D-04,
     7 1.03772410422993D-04, 9.82626078369363D-05, 9.32120517249503D-05,
     8 8.85710852478712D-05, 8.42963105715700D-05, 8.03497548407791D-05,
     9 6.93735541354589D-04, 2.32241745182922D-04,-1.41986273556691D-05,
     A-1.16444931672049D-04,-1.50803558053049D-04,-1.55121924918096D-04,
     B-1.46809756646466D-04,-1.33815503867491D-04,-1.19744975684254D-04,
     C-1.06184319207974D-04,-9.37699549891194D-05,-8.26923045588193D-05,
     D-7.29374348155221D-05,-6.44042357721016D-05,-5.69611566009369D-05,
     D-5.04731044303562D-05,-4.48134868008883D-05,-3.98688727717599D-05,
     F-3.55400532972042D-05,-3.17414256609022D-05,-2.83996793904175D-05,
     G-2.54522720634871D-05,-2.28459297164725D-05,-2.05352753106481D-05,
     H-1.84816217627666D-05,-1.66519330021394D-05/
      DATA ALFA2           /-3.54211971457744D-04,-1.56161263945159D-04,
     1 3.04465503594936D-05, 1.30198655773243D-04, 1.67471106699712D-04,
     2 1.70222587683593D-04, 1.56501427608595D-04, 1.36339170977445D-04,
     3 1.14886692029825D-04, 9.45869093034688D-05, 7.64498419250898D-05,
     4 6.07570334965197D-05, 4.74394299290509D-05, 3.62757512005344D-05,
     5 2.69939714979225D-05, 1.93210938247939D-05, 1.30056674793963D-05,
     6 7.82620866744497D-06, 3.59257485819352D-06, 1.44040049814252D-07,
     7-2.65396769697939D-06,-4.91346867098486D-06,-6.72739296091248D-06,
     8-8.17269379678658D-06,-9.31304715093561D-06,-1.02011418798016D-05,
     9 3.78194199201773D-04, 2.02471952761816D-04,-6.37938506318862D-05,
     A-2.38598230603006D-04,-3.10916256027362D-04,-3.13680115247576D-04,
     B-2.78950273791323D-04,-2.28564082619141D-04,-1.75245280340847D-04,
     C-1.25544063060690D-04,-8.22982872820208D-05,-4.62860730588116D-05,
     D-1.72334302366962D-05, 5.60690482304602D-06, 2.31395443148287D-05,
     E 3.62642745856794D-05, 4.58006124490189D-05, 5.24595294959114D-05,
     F 5.68396208545815D-05, 5.94349820393104D-05, 6.06478527578422D-05,
     G 6.08023907788436D-05, 6.01577894539460D-05, 5.89199657344698D-05,
     H 5.72515823777593D-05, 5.52804375585853D-05/
      DATA BETA1           / 1.79988721413553D-02, 5.59964911064388D-03,
     1 2.88501402231133D-03, 1.80096606761054D-03, 1.24753110589199D-03,
     2 9.22878876572938D-04, 7.14430421727287D-04, 5.71787281789705D-04,
     3 4.69431007606482D-04, 3.93232835462917D-04, 3.34818889318298D-04,
     4 2.88952148495752D-04, 2.52211615549573D-04, 2.22280580798883D-04,
     5 1.97541838033063D-04, 1.76836855019718D-04, 1.59316899661821D-04,
     6 1.44347930197334D-04, 1.31448068119965D-04, 1.20245444949303D-04,
     7 1.10449144504599D-04, 1.01828770740567D-04, 9.41998224204238D-05,
     8 8.74130545753834D-05, 8.13466262162801D-05, 7.59002269646219D-05,
     9-1.49282953213429D-03,-8.78204709546389D-04,-5.02916549572035D-04,
     A-2.94822138512746D-04,-1.75463996970783D-04,-1.04008550460816D-04,
     B-5.96141953046458D-05,-3.12038929076098D-05,-1.26089735980230D-05,
     C-2.42892608575730D-07, 8.05996165414274D-06, 1.36507009262147D-05,
     D 1.73964125472926D-05, 1.98672978842134D-05, 2.14463263790823D-05,
     E 2.23954659232457D-05, 2.28967783814713D-05, 2.30785389811178D-05,
     F 2.30321976080909D-05, 2.28236073720349D-05, 2.25005881105292D-05,
     G 2.20981015361991D-05, 2.16418427448104D-05, 2.11507649256221D-05,
     H 2.06388749782171D-05, 2.01165241997082D-05/
      DATA BETA2           / 5.52213076721293D-04, 4.47932581552385D-04,
     1 2.79520653992021D-04, 1.52468156198447D-04, 6.93271105657044D-05,
     2 1.76258683069991D-05,-1.35744996343269D-05,-3.17972413350427D-05,
     3-4.18861861696693D-05,-4.69004889379141D-05,-4.87665447413787D-05,
     4-4.87010031186735D-05,-4.74755620890087D-05,-4.55813058138628D-05,
     5-4.33309644511266D-05,-4.09230193157750D-05,-3.84822638603221D-05,
     6-3.60857167535411D-05,-3.37793306123367D-05,-3.15888560772110D-05,
     7-2.95269561750807D-05,-2.75978914828336D-05,-2.58006174666884D-05,
     8-2.41308356761280D-05,-2.25823509518346D-05,-2.11479656768913D-05,
     9-4.74617796559960D-04,-4.77864567147321D-04,-3.20390228067038D-04,
     A-1.61105016119962D-04,-4.25778101285435D-05, 3.44571294294968D-05,
     B 7.97092684075675D-05, 1.03138236708272D-04, 1.12466775262204D-04,
     C 1.13103642108481D-04, 1.08651634848774D-04, 1.01437951597662D-04,
     D 9.29298396593364D-05, 8.40293133016090D-05, 7.52727991349134D-05,
     E 6.69632521975731D-05, 5.92564547323195D-05, 5.22169308826976D-05,
     F 4.58539485165361D-05, 4.01445513891487D-05, 3.50481730031328D-05,
     G 3.05157995034347D-05, 2.64956119950516D-05, 2.29363633690998D-05,
     H 1.97893056664022D-05, 1.70091984636413D-05/
      DATA BETA3           / 7.36465810572578D-04, 8.72790805146194D-04,
     1 6.22614862573135D-04, 2.85998154194304D-04, 3.84737672879366D-06,
     2-1.87906003636972D-04,-2.97603646594555D-04,-3.45998126832656D-04,
     3-3.53382470916038D-04,-3.35715635775049D-04,-3.04321124789040D-04,
     4-2.66722723047613D-04,-2.27654214122820D-04,-1.89922611854562D-04,
     5-1.55058918599094D-04,-1.23778240761874D-04,-9.62926147717644D-05,
     6-7.25178327714425D-05,-5.22070028895634D-05,-3.50347750511901D-05,
     7-2.06489761035552D-05,-8.70106096849767D-06, 1.13698686675100D-06,
     8 9.16426474122779D-06, 1.56477785428873D-05, 2.08223629482467D-05/
      DATA GAMA            / 6.29960524947437D-01, 2.51984209978975D-01,
     1 1.54790300415656D-01, 1.10713062416159D-01, 8.57309395527395D-02,
     2 6.97161316958684D-02, 5.86085671893714D-02, 5.04698873536311D-02,
     3 4.42600580689155D-02, 3.93720661543510D-02, 3.54283195924455D-02,
     4 3.21818857502098D-02, 2.94646240791158D-02, 2.71581677112934D-02,
     5 2.51768272973862D-02, 2.34570755306079D-02, 2.19508390134907D-02,
     6 2.06210828235646D-02, 1.94388240897881D-02, 1.83810633800683D-02,
     7 1.74293213231963D-02, 1.65685837786612D-02, 1.57865285987918D-02,
     8 1.50729501494096D-02, 1.44193250839955D-02, 1.38184805735342D-02/
C
C TEST INPUT ARGUMENTS
C
      KT = 1
      IF (N-1) 710, 10, 20
   10 KT = 2
   20 NN = N
      IF (X) 720, 30, 80
   30 IF (ALPHA) 700, 40, 50
   40 Y(1) = 1.0D+00
      IF (N.EQ.1) RETURN
      I1 = 2
      GO TO 60
   50 I1 = 1
   60 DO I=I1,N
        Y(I) = 0.0D+00
      end do
      RETURN
   80 CONTINUE
      IF (ALPHA.LT.0.0D+00 ) GO TO 700
      DFN = DBLE ( N ) + DBLE ( ALPHA ) - 1.0D+00
      FNU = DFN
      XO2 = X * 0.5D+00
      SXO2 = XO2*XO2
c
C DECISION TREE FOR REGION WHERE SERIES, ASYMPTOTIC EXPANSION FOR X
C TO INFINITY AND ASYMPTOTIC EXPANSION FOR NU TO INFINITY ARE
C APPLIED.
C
      IF (SXO2.LE.(FNU+1.0D+00 ) ) GO TO 90
      TA = DMAX1 ( 20.0D+00, FNU )
      IF (X.GT.TA) GO TO 120
      IF (X.GT.12.0D+00) GO TO 110
      XO2L = DLOG ( XO2 )
      NS = SXO2 - FNU
      GO TO 100
   90 FN = FNU
      FNP1 = FN + 1.0D+00
      XO2L = DLOG ( XO2 )
      IS = KT
      IF (X.LE.0.5D+00 ) GO TO 330
      NS = 0
  100 DFN = DFN + DBLE ( NS )
      FN = DFN
      FNP1 = FN + 1.0D+00
      IS = KT
      IF (N-1+NS.GT.0) IS = 3
      GO TO 330
  110 NS = DMAX1 ( 36.0D+00 - FNU, 0.0D+00 )
      DFN = DFN + DBLE ( NS )
      FN = DFN
      IS = KT
      IF (N-1+NS.GT.0) IS = 3
      GO TO 130
  120 CONTINUE
      RTX = DSQRT ( X )
      TAU = RTWO * RTX
      TA = TAU + FNULIM(KT)
      IF (FNU.LE.TA) GO TO 480
      FN = FNU
      IS = KT
C
C UNIFORM ASYMPTOTIC EXPANSION FOR NU TO INFINITY
C
  130 CONTINUE
      XX = X / FN
      W2 = 1.0D+00 - XX * XX
      ABW2 = DABS ( W2 )
      RA = DSQRT ( ABW2 )
      IF ( ABW2 .GT. 0.2775D+00 ) GO TO 220
C
C CASES NEAR X=FN, ABS(1.-(X/FN)**2).LE.0.2775
C COEFFICIENTS OF ASYMPTOTIC EXPANSION BY SERIES
C ZETA AND TRUNCATION FOR A(ZETA) AND B(ZETA) SERIES
C KMAX IS TRUNCATION INDEX FOR A(ZETA) AND B(ZETA) SERIES=MAX(2,SA)
C
      SA = 0.0D+00
      IF (ABW2.EQ.0.0D+00 ) GO TO 140
      SA = TOLS / DLOG ( ABW2 )
  140 SB = SA
      DO I=1,5
        KMAX(I) = DMAX1 ( SA, 2.0D+00 )
        SA = SA + SB
      end do
      KB = KMAX(5)
      KLAST = KB - 1
      SA = GAMA(KB)
      DO K=1,KLAST
        KB = KB - 1
        SA = SA*W2 + GAMA(KB)
      end do
      Z = W2*SA
      AZ = DABS ( Z )
      RTZ = DSQRT ( AZ )
      FN13 = FN ** CON2
      RTARY = RTZ * FN13
      ARY = -RTARY * RTARY
      AZ32 = AZ * RTZ * CON1
      ACZ = FN * AZ32
      IF (Z.LE.0.0D+00 ) GO TO 170
C
C TEST FOR UNDERFLOW, 1.D-280=EXP(-644.), ONE WORD LENGTH
C UP FROM UNDERFLOW LIMIT OF CDC 6600
C
      IF (ACZ.GT.ELIM2) GO TO 380
      ARY = -ARY
  170 PHI = DSQRT ( DSQRT ( SA + SA + SA + SA ) )
C
C B(ZETA) FOR S=0
C
      KB = KMAX(5)
      KLAST = KB - 1
      SB = BETA(KB,1)
      DO K=1,KLAST
        KB = KB - 1
        SB = SB*W2 + BETA(KB,1)
      end do
      KSP1 = 1
      FN2 = FN*FN
      RFN2 = 1.0D+00/FN2
      RDEN = 1.0D+00
      ASUM = 1.0D+00
      RELB = TOL * DABS ( SB )
      BSUM = SB
      DO 200 KS=1,4
        KSP1 = KSP1 + 1
        RDEN = RDEN*RFN2
C
C A(ZETA) AND B(ZETA) FOR S=1,2,3,4
C
        KB = KMAX(5-KS)
        KLAST = KB - 1
        SA = ALFA(KB,KS)
        SB = BETA(KB,KSP1)
        DO K=1,KLAST
          KB = KB - 1
          SA = SA*W2 + ALFA(KB,KS)
          SB = SB*W2 + BETA(KB,KSP1)
        end do
        TA = SA*RDEN
        TB = SB*RDEN
        ASUM = ASUM + TA
        BSUM = BSUM + TB
        IF ( DABS ( TA ) .LE. TOL .AND. DABS ( TB ) .LE. RELB ) THEN
          GO TO 210
        end if
  200 CONTINUE
  210 CONTINUE
      BSUM = BSUM/(FN*FN13)
      GO TO 300
  220 CONTINUE
      TAU = 1.0D+00 / RA
      T2 = 1.0D+00 / W2
      IF (W2.GE.0.0D+00 ) GO TO 230
c
C CASES FOR (X/FN).GT.SQRT(1.2775)
c
      AZ32 = DABS ( RA - DATAN ( RA ))
      ACZ = AZ32*FN
      CZ = -ACZ
      Z32 = 1.5D+00 * AZ32
      RTZ = Z32**CON2
      FN13 = FN**CON2
      RTARY = RTZ*FN13
      ARY = -RTARY*RTARY
      GO TO 240
  230 CONTINUE
C
C CASES FOR (X/FN).LT.SQRT(0.7225)
C
      AZ32 = DABS ( DLOG ( ( 1.0D+00 + RA ) / XX ) - RA )
C
C TEST FOR UNDERFLOW, 1.D-280 = EXP(-644.), ONE WORD LENGTH
C UP FROM UNDERFLOW LIMIT OF CDC 6600
C
      ACZ = AZ32*FN
      CZ = ACZ
      IF (ACZ.GT.ELIM2) GO TO 380
      Z32 = 1.5D+00 * AZ32
      RTZ = Z32**CON2
      FN13 = FN**CON2
      RTARY = RTZ*FN13
      ARY = RTARY*RTARY
  240 CONTINUE
      PHI = DSQRT ( ( RTZ + RTZ ) * TAU)
      TB = 1.0D+00
      ASUM = 1.0D+00
      TFN = TAU/FN
      UPOL(1) = 1.0D+00
      UPOL(2) = (C(1,1)*T2+C(2,1))*TFN
      RCZ = CON1/CZ
      CRZ32 = CON548*RCZ
      BSUM = UPOL(2) + CRZ32
      RELB = TOL* DABS ( BSUM )
      AP = TFN
      KS = 0
      KP1 = 2
      RZDEN = RCZ

      DO 280 LR=2,8,2
C
C COMPUTE TWO U POLYNOMIALS FOR NEXT A(ZETA) AND B(ZETA)
C
        LRP1 = LR + 1
        DO K=LR,LRP1
          KS = KS + 1
          KP1 = KP1 + 1
          S1 = C(1,K)
          DO J=2,KP1
            S1 = S1*T2 + C(J,K)
          end do
          AP = AP*TFN
          UPOL(KP1) = AP*S1
          CR(KS) = BR(KS)*RZDEN
          RZDEN = RZDEN*RCZ
          DR(KS) = AR(KS)*RZDEN
        end do
        SUMA = UPOL(LRP1)
        SUMB = UPOL(LR+2) + UPOL(LRP1)*CRZ32
        JU = LRP1
        DO JR=1,LR
          JU = JU - 1
          SUMA = SUMA + CR(JR)*UPOL(JU)
          SUMB = SUMB + DR(JR)*UPOL(JU)
        end do
        TB = -TB
        IF (W2.GT.0.0D+00) TB = DABS ( TB )
        ASUM = ASUM + SUMA*TB
        BSUM = BSUM + SUMB*TB
        IF ( DABS ( SUMA ).LE.TOL .AND. DABS ( SUMB ) .LE. RELB ) then
          GO TO 290
        end if
  280 CONTINUE
  290 TB = RTARY
      IF (W2.GT.0.0D+00 ) TB = -TB
      BSUM = BSUM/TB
  300 CONTINUE
      CALL JAIRY(ARY, RTARY, ACZ, AI, DAI)
      TEMP(IS) = PHI*(AI*ASUM+DAI*BSUM)/FN13
      GO TO (320, 450, 610), IS
  310 TEMP(1) = TEMP(3)
      KT = 1
  320 IS = 2
      DFN = DFN - 1.0D+00
      FN = DFN
      GO TO 130
C
C SERIES FOR (X/2)**2.LE.NU+1
C
  330 CONTINUE
      GLN = GAMLN ( FNP1 )
      ARG = FN*XO2L - GLN
      IF (ARG.LT.-ELIM1) GO TO 400
      EARG = DEXP ( ARG )
  340 CONTINUE
      S = 1.0D+00
      AK = 3.0D+00
      T2 = 1.0D+00
      T = 1.0D+00
      S1 = FN
      DO K=1,17
        S2 = T2 + S1
        T = -T*SXO2/S2
        S = S + T
        IF ( DABS ( T ) .LT. TOL ) GO TO 360
        T2 = T2 + AK
        AK = AK + 2.0D+00
        S1 = S1 + FN
      end do
  360 CONTINUE
      TEMP(IS) = S*EARG
      GO TO (370, 450, 600), IS
  370 EARG = EARG*FN/XO2
      DFN = DFN - 1.0D+00
      FN = DFN
      IS = 2
      GO TO 340
C
C SET UNDERFLOW VALUE AND UPDATE PARAMETERS
C
  380 Y(NN) = 0.0D+00
      NN = NN - 1
      DFN = DFN - 1.0D+00
      FN = DFN
      IF (NN-1) 440, 390, 130
  390 KT = 2
      IS = 2
      GO TO 130
  400 Y(NN) = 0.0D+00
      NN = NN - 1
      FNP1 = FN
      DFN = DFN - 1.0D+00
      FN = DFN
      IF (NN-1) 440, 410, 420
  410 KT = 2
      IS = 2
  420 IF (SXO2.LE.FNP1) GO TO 430
      GO TO 130
  430 ARG = ARG - XO2L + DLOG ( FNP1 )
      IF (ARG.LT.-ELIM1) GO TO 400
      GO TO 330
  440 NZ = N - NN
      PRINT 99996, NZ, ALPHA, N, X
      RETURN
C
C BACKWARD RECURSION SECTION
C
  450 CONTINUE
      NZ = N - NN
      IF (NZ.NE.0) PRINT 99996, NZ, ALPHA, N, X
      IF (KT.EQ.2) GO TO 470
C
C BACKWARD RECUR FROM INDEX ALPHA+NN-1 TO ALPHA
C
      Y(NN) = TEMP(1)
      Y(NN-1) = TEMP(2)
      IF (NN.EQ.2) RETURN
      DX = X
      TRX = 2.0D+00/DX
      DTM = DFN*TRX
      TM = DTM
      K = NN + 1
      DO I=3,NN
        K = K - 1
        Y(K-2) = TM*Y(K-1) - Y(K)
        DTM = DTM - TRX
        TM = DTM
      end do
      RETURN
  470 Y(1) = TEMP(2)
      RETURN
C
C ASYMPTOTIC EXPANSION FOR X TO INFINITY WITH FORWARD RECURSION IN
C OSCILLATORY REGION X.GT.MAX(20, NU), PROVIDED THE LAST MEMBER
C OF THE SEQUENCE IS ALSO IN THE REGION.
C
  480 CONTINUE
      IN = ALPHA - TAU + 2.0D+00
      IF (IN.LE.0) GO TO 490
      INP1 = IN + 1
      DALPHA = ALPHA - DBLE ( INP1 )
      KT = 1
      GO TO 500
  490 DALPHA = ALPHA
      IN = 0
  500 IS = KT
      ARG = X - PIDT*DALPHA - PDF
      SA = DSIN ( ARG )
      SB = DCOS ( ARG )
      RA = RTTP/RTX
      ETX = 8.0D+00 * X
  510 DX = DALPHA
      DX = DX + DX
      DTM = DX*DX
      T2 = DTM - 1.0D+00
      T2 = T2/ETX
      S2 = T2
      RELB = TOL * DABS ( T2 )
      T1 = ETX
      S1 = 1.0D+00
      FN = 1.0D+00
      AK = 8.0D+00
      DO 520 K=1,13
        T1 = T1 + ETX
        FN = FN + AK
        DX = FN
        TRX = DTM - DX
        AP = TRX
        T2 = -T2*AP/T1
        S1 = S1 + T2
        T1 = T1 + ETX
        AK = AK + 8.0D+00
        FN = FN + AK
        DX = FN
        TRX = DTM - DX
        AP = TRX
        T2 = T2*AP/T1
        S2 = S2 + T2
        IF ( DABS ( T2 ) .LE. RELB ) GO TO 530
        AK = AK + 8.0D+00
  520 CONTINUE
  530 TEMP(IS) = RA*(S1*SB-S2*SA)
      GO TO (540, 550), IS
  540 DALPHA = DALPHA + 1.0D+00
      IS = 2
      TB = SA
      SA = -SB
      SB = TB
      GO TO 510
C
C FORWARD RECURSION SECTION
C
  550 IF (KT.EQ.2) GO TO 470
      S1 = TEMP(1)
      S2 = TEMP(2)
      TX = 2.0D+00 / X
      TM = DALPHA*TX
      IF (IN.EQ.0) GO TO 570
C
C FORWARD RECUR TO INDEX ALPHA
C
      DO I=1,IN
        S = S2
        S2 = TM*S2 - S1
        TM = TM + TX
        S1 = S
      end do
      IF (NN.EQ.1) GO TO 590
      S = S2
      S2 = TM*S2 - S1
      TM = TM + TX
      S1 = S
  570 CONTINUE
C
C FORWARD RECUR FROM INDEX ALPHA TO ALPHA+N-1
C
      Y(1) = S1
      Y(2) = S2
      IF (NN.EQ.2) RETURN
      DO I=3,NN
        Y(I) = TM*Y(I-1) - Y(I-2)
        TM = TM + TX
      end do
      RETURN
  590 Y(1) = S2
      RETURN
C
C BACKWARD RECURSION WITH NORMALIZATION BY
C ASYMPTOTIC EXPANSION FOR NU TO INFINITY OR POWER SERIES.
C
  600 CONTINUE
C
C COMPUTATION OF LAST ORDER FOR SERIES NORMALIZATION
C
      KM = DMAX1 ( 3.0D+00 - FN, 0.0D+00 )
      TFN = FN + dble ( KM )
      TA = ( GLN + TFN - 0.9189385332D+00 - 0.0833333333D+00 / TFN )
     &   / ( TFN + 0.5D+00 )
      TA = XO2L - TA
      TB = -( 1.0D+00 - 1.5D+00 / TFN ) / TFN
      IN = CE / ( - TA + DSQRT ( TA * TA - CE * TB ) ) + 1.5D+00
      IN = IN + KM
      GO TO 650
  610 CONTINUE
C
C COMPUTATION OF LAST ORDER FOR ASYMPTOTIC EXPANSION NORMALIZATION
C
      GLN = AZ32 + RA
      IF ( ARY .GT. 30.0D+00 ) GO TO 630
      RDEN = (PP(4)*ARY+PP(3))*ARY + 1.0D+00
      RZDEN = PP(1) + PP(2)*ARY
      TA = RZDEN/RDEN
      IF (W2.LT.0.10D+00 ) GO TO 620
      TB = GLN / RTARY
      GO TO 640
  620 TB = ( 1.259921049D+00 + 0.1679894730D+00 * W2 ) / FN13
      GO TO 640
  630 CONTINUE
      TA = CON1*TCE/ACZ
      TA = ( ( 0.0493827160D+00 * TA - 0.1111111111D+00 ) * TA 
     &  + 0.6666666667D+00 ) * TA * ARY
      IF (W2.LT.0.10D+00 ) GO TO 620
      TB = GLN/RTARY
  640 IN = TA/TB + 1.5D+00
      IF (IN.GT.INLIM) GO TO 310
  650 DX = dble ( IN )
      DTM = DFN + DX
      DX = X
      TRX = 2.0D+00 / DX
      DTM = DTM * TRX
      TM = DTM
      TA = 0.0D+00
      TB = TOL
      KK = 1
  660 CONTINUE
C
C BACKWARD RECUR UNINDEXED
C
      DO I=1,IN
        S = TB
        TB = TM*TB - TA
        TA = S
        DTM = DTM - TRX
        TM = DTM
      end do
c
C NORMALIZATION
C
      IF (KK.NE.1) GO TO 680
      TA = (TA/TB)*TEMP(3)
      TB = TEMP(3)
      KK = 2
      IN = NS
      IF (NS.NE.0) GO TO 660
  680 Y(NN) = TB
      NZ = N - NN
      IF (NZ.NE.0) PRINT 99996, NZ, ALPHA, N, X
      IF (NN.EQ.1) RETURN
      TB = TM*TB - TA
      DTM = DTM - TRX
      TM = DTM
      K = NN - 1
      Y(K) = TB
      IF (NN.EQ.2) RETURN
      KM = K - 1
C
C BACKWARD RECUR INDEXED
C
      DO I=1,KM
        Y(K-1) = TM*Y(K) - Y(K+1)
        DTM = DTM - TRX
        TM = DTM
        K = K - 1
      end do
      RETURN
  700 PRINT 99999, ALPHA, N, X
      STOP
  710 PRINT 99998, ALPHA, N, X
      STOP
  720 PRINT 99997, ALPHA, N, X
      STOP
99999 FORMAT (51H0JBESS CALLED WITH THE ORDER, ALPHA, LESS THAN ZERO
     * /7H ALPHA=,E25.14,3H N=,I6,3H X=,E25.14)
99998 FORMAT (34H0JBESS CALLED WITH N LESS THAN ONE/7H ALPHA=,
     * E25.14,3H N=,I6,3H X=,E25.14)
99997 FORMAT (35H0JBESS CALLED WITH X LESS THAN ZERO/7H ALPHA=,
     * E25.14,3H N=,I6,3H X=,E25.14)
99996 FORMAT (25H0UNDERFLOW IN JBESS, LAST,I6,20H VALUE(S) OF Y ARRAY,
     * 17H WERE SET TO ZERO/7H ALPHA=,E25.14,3H N=,I6,3H X=,E25.14)
      END
      subroutine bessel_ix_values ( n_data, nu, x, fx )

c*********************************************************************72
c
cc BESSEL_IX_VALUES returns some values of the Ix Bessel function.
c
c  Discussion:
c
c    This set of data considers the less common case in which the
c    index of the Bessel function In is actually not an integer.
c    We may suggest this case by occasionally replacing the symbol
c    "In" by "Ix".
c
c    The modified Bessel functions In(Z) and Kn(Z) are solutions of
c    the differential equation
c
c      Z^2 W'' + Z * W' - ( Z^2 + N^2 ) * W = 0.
c
c    In Mathematica, the function can be evaluated by:
c
c      BesselI[n,x]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision NU, the order of the function.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 28 )

      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision nu
      double precision nu_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save nu_vec
      save x_vec

      data fx_vec /
     &  0.3592084175833614D+00,
     &  0.9376748882454876D+00,
     &  2.046236863089055D+00,
     &  3.053093538196718D+00,
     &  4.614822903407601D+00,
     &  26.47754749755907D+00,
     &  2778.784603874571D+00,
     &  4.327974627242893D+07,
     &  0.2935253263474798D+00,
     &  1.099473188633110D+00,
     &  21.18444226479414D+00,
     &  2500.906154942118D+00,
     &  2.866653715931464D+20,
     &  0.05709890920304825D+00,
     &  0.3970270801393905D+00,
     &  13.76688213868258D+00,
     &  2028.512757391936D+00,
     &  2.753157630035402D+20,
     &  0.4139416015642352D+00,
     &  1.340196758982897D+00,
     &  22.85715510364670D+00,
     &  2593.006763432002D+00,
     &  2.886630075077766D+20,
     &  0.03590910483251082D+00,
     &  0.2931108636266483D+00,
     &  11.99397010023068D+00,
     &  1894.575731562383D+00,
     &  2.716911375760483D+20  /
      data nu_vec /
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  1.50D+00,
     &  1.50D+00,
     &  1.50D+00,
     &  1.50D+00,
     &  1.50D+00,
     &  2.50D+00,
     &  2.50D+00,
     &  2.50D+00,
     &  2.50D+00,
     &  2.50D+00,
     &  1.25D+00,
     &  1.25D+00,
     &  1.25D+00,
     &  1.25D+00,
     &  1.25D+00,
     &  2.75D+00,
     &  2.75D+00,
     &  2.75D+00,
     &  2.75D+00,
     &  2.75D+00 /
      data x_vec /
     &   0.2D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   2.5D+00,
     &   3.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  20.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  50.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  50.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  50.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  50.0D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        nu = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        nu = nu_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine bessel_jx_values ( n_data, nu, x, fx )

c*********************************************************************72
c
cc BESSEL_JX_VALUES returns some values of the Jx Bessel function.
c
c  Discussion:
c
c    This set of data considers the less common case in which the
c    index of the Bessel function Jn is actually not an integer.
c    We may suggest this case by occasionally replacing the symbol
c    "Jn" by "Jx".
c
c    In Mathematica, the function can be evaluated by:
c
c      BesselJ[n,x]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision NU, the order of the function.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 28 )

      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision nu
      double precision nu_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save nu_vec
      save x_vec

      data fx_vec /
     &   0.3544507442114011D+00,
     &   0.6713967071418031D+00,
     &   0.5130161365618278D+00,
     &   0.3020049060623657D+00,
     &   0.06500818287737578D+00,
     &  -0.3421679847981618D+00,
     &  -0.1372637357550505D+00,
     &   0.1628807638550299D+00,
     &   0.2402978391234270D+00,
     &   0.4912937786871623D+00,
     &  -0.1696513061447408D+00,
     &   0.1979824927558931D+00,
     &  -0.1094768729883180D+00,
     &   0.04949681022847794D+00,
     &   0.2239245314689158D+00,
     &   0.2403772011113174D+00,
     &   0.1966584835818184D+00,
     &   0.02303721950962553D+00,
     &   0.3314145508558904D+00,
     &   0.5461734240402840D+00,
     &  -0.2616584152094124D+00,
     &   0.1296035513791289D+00,
     &  -0.1117432171933552D+00,
     &   0.03142623570527935D+00,
     &   0.1717922192746527D+00,
     &   0.3126634069544786D+00,
     &   0.1340289119304364D+00,
     &   0.06235967135106445D+00 /
      data nu_vec /
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  1.50D+00,
     &  1.50D+00,
     &  1.50D+00,
     &  1.50D+00,
     &  1.50D+00,
     &  2.50D+00,
     &  2.50D+00,
     &  2.50D+00,
     &  2.50D+00,
     &  2.50D+00,
     &  1.25D+00,
     &  1.25D+00,
     &  1.25D+00,
     &  1.25D+00,
     &  1.25D+00,
     &  2.75D+00,
     &  2.75D+00,
     &  2.75D+00,
     &  2.75D+00,
     &  2.75D+00 /
      data x_vec /
     &   0.2D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   2.5D+00,
     &   3.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  20.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  50.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  50.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  50.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  50.0D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        nu = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        nu = nu_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
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
