      program main

c*********************************************************************72
c
cc toms597_test() tests toms597().
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2021
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'toms597_test():'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test toms597():'

      call ribesl_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'toms597_test():'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )
      stop
      end
      subroutine ribesl_test ( )

c*********************************************************************72
c
cc ribesl_test() tests ribesl().
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 January 2016
c
C     PROGRAM TO TEST RIBESL
C
C     DATA REQUIRED
C
C        NONE
C
C     SUBPROGRAMS REQUIRED FROM THIS PACKAGE
C
C        MACHAR - AN ENVIRONMENTAL INQUIRY PROGRAM PROVIDING
C                 INFORMATION ON THE FLOATING-POINT ARITHMETIC
C                 SYSTEM.  NOTE THAT THE CALL TO MACHAR CAN
C                 BE DELETED PROVIDED THE FOLLOWING FIVE
C                 PARAMETERS ARE ASSIGNED THE VALUES INDICATED
C
C                 IBETA  - THE RADIX OF THE FLOATING-POINT SYSTEM
C                 IT     - THE NUMBER OF BASE-IBETA DIGITS IN THE
C                          SIGNIFICAND OF A FLOATING-POINT NUMBER
C                 MINEXP - THE LARGEST IN MAGNITUDE NEGATIVE
C                          INTEGER SUCH THAT  FLOAT(IBETA)**MINEXP
C                          IS A POSITIVE FLOATING-POINT NUMBER
C                 EPS    - THE SMALLEST POSITIVE FLOATING-POINT
C                          NUMBER SUCH THAT 1.0+EPS .NE. 1.0
C                 EPSNEG - THE SMALLEST POSITIVE FLOATING-POINT
C                          NUMBER SUCH THAT 1.0-EPSNEG .NE. 1.0
C
C        REN(K) - A FUNCTION SUBPROGRAM RETURNING RANDOM REAL
C                 NUMBERS UNIFORMLY DISTRIBUTED OVER (0,1)
C
C
C     STANDARD FORTRAN SUBPROGRAMS REQUIRED FOR SINGLE PRECISION
C
C         ABS, ALOG, AMAX1, FLOAT, IFIX, SQRT
C
C     STANDARD FORTRAN SUBPROGRAMS REQUIRED FOR DOUBLE PRECISION
C
C         DABS, DLOG, DMAX1, DBLE, FLOAT, IFIX, SNGL, DSQRT
C
C
C     LATEST REVISION - MAY 18, 1982
C
C     AUTHOR - W. J. CODY
C              ARGONNE NATIONAL LABORATORY
C
C------------------------------------------------------------------
      INTEGER I, IBETA, IEXP, II, IOUT, IRND, IT, IZE, J, JT, K1, K2,
     * K3, MACHEP, MAXEXP, MB, MINEXP, N, NB, NBM1, NCALC, NEGEP, NGRD
CS    REAL A,AIT,ALBETA,ALEPS,ALPHA,A1,B,BETA,C,C1,C2,DEL,DEN,DEN1,
CS   1     GAMMA,REN,EPS,EPSNEG,HALF,ONE,R6,R7,SUM,TEN,TWO,U,W,X,XL,
CS   2     XMAX,XMIN,XN,XNB,X1,X99,Y,YY,Z,ZERO,ZZ
      DOUBLE PRECISION A, AIT, ALBETA, ALEPS, ALPHA, A1, B, BETA, C,
     * C1, C2, DEL, DEN, DEN1, DGAMMA, REN, EPS, EPSNEG, HALF, ONE, R6,
     * R7, SUM, TEN, TWO, U, W, X, XL, XMAX, XMIN, XN, XNB, X1, X99, Y,
     * YY, Z, ZERO, ZZ
      DIMENSION U(160)
CS    DATA ZERO,HALF,ONE,TWO,TEN/0.0E0,0.5E0,1.0E0,2.0E0,10.0E0/
CS    DATA X99,C1,C2/-999.0E0,0.796875E0,1.0095608028653558798921E-3/
      DATA ZERO, HALF, ONE, TWO, TEN /0.0D0,0.5D0,1.0D0,2.0D0,10.0D0/
      DATA X99, C1, C2 /-999.0D0,0.796875D0,1.0095608028653558798921D-3/
      DATA IOUT /6/
C------------------------------------------------------------------
C
C   DETERMINE MACHINE PARAMETERS AND SET CONSTANTS
C
C------------------------------------------------------------------
      CALL MACHAR(IBETA, IT, IRND, NGRD, MACHEP, NEGEP, IEXP, MINEXP,
     * MAXEXP, EPS, EPSNEG, XMIN, XMAX)
CS    BETA = FLOAT(IBETA)
CS    ALBETA = ALOG(BETA)
CS    ALEPS = ALOG(EPS)
CS    AIT = FLOAT(IT)
      BETA = DBLE(FLOAT(IBETA))
      ALBETA = DLOG(BETA)
      ALEPS = DLOG(EPS)
      AIT = DBLE(FLOAT(IT))
      A = ZERO
      B = TWO
      N = 2000
CS    XN = FLOAT(N)
      XN = DBLE(FLOAT(N))
      JT = 0
      IZE = 1
      MB = 2
C-----------------------------------------------------------------
C     RANDOM ARGUMENT ACCURACY TESTS
C-----------------------------------------------------------------
      DO 60 J=1,4
        K1 = 0
        K3 = 0
        X1 = ZERO
        A1 = ZERO
        R6 = ZERO
        R7 = ZERO
        DEL = (B-A)/XN
        XL = A
CS       IF (J .EQ. 1) NB = IFIX(5.53E0 - 0.21E0*ALEPS)
CS       IF (J .EQ. 2) NB = IFIX(7.26E0 - 0.27E0*ALEPS)
CS       IF (J .EQ. 3) NB = IFIX(12.93E0 - 0.33E0*ALEPS)
CS       XNB = FLOAT(NB)
        IF (J.EQ.1) NB = IFIX(5.53E0-0.21E0*SNGL(ALEPS))
        IF (J.EQ.2) NB = IFIX(7.26E0-0.27E0*SNGL(ALEPS))
        IF (J.EQ.3) NB = IFIX(12.93E0-0.33E0*SNGL(ALEPS))
        XNB = DBLE(FLOAT(NB))
        NBM1 = NB - 1
C
        DO 50 I=1,N
          X = DEL*REN(JT) + XL
          IF (J.EQ.4) GO TO 20
C------------------------------------------------------------------
C   FIRST THREE TESTS ARE BASED ON MACLAURIN SERIES
C------------------------------------------------------------------
          ALPHA = REN(JT)
          Y = X/TWO
          YY = Y*Y
          CALL RIBESL(X, ALPHA, MB, IZE, U, NCALC)
          Z = U(1)
          DEN = XNB
          DEN1 = DEN + ALPHA
          SUM = ONE/DEN/DEN1
          DO II=1,NBM1
            DEN = DEN - ONE
            DEN1 = DEN1 - ONE
            SUM = (YY*SUM+ONE)/DEN/DEN1
          end do
          Y = Y**ALPHA
CS          ZZ = (SUM * YY * Y + Y) / GAMMA(ONE+ALPHA)
          ZZ = (SUM*YY*Y+Y)/DGAMMA(ONE+ALPHA)
          GO TO 30
C------------------------------------------------------------------
C   LAST TEST IS BASED ON ASYMPTOTIC FORM FOR ALPHA = 0.5
C------------------------------------------------------------------
   20     CALL RIBESL(X, ALPHA, MB, IZE, U, NCALC)
          Z = U(1)
CS          ZZ = C / SQRT(X)
          ZZ = C/DSQRT(X)
   30     W = (Z-ZZ)/Z
          IF (W.GT.ZERO) K1 = K1 + 1
          IF (W.LT.ZERO) K3 = K3 + 1
CS          W = ABS(W)
          W = DABS(W)
          IF (W.LE.R6) GO TO 40
          R6 = W
          X1 = X
          A1 = ALPHA
   40     R7 = R7 + W*W
          XL = XL + DEL
   50   CONTINUE
C------------------------------------------------------------------
C   GATHER AND PRINT STATISTICS FOR TEST
C------------------------------------------------------------------
        K2 = N - K3 - K1
CS       R7 = SQRT(R7/XN)
        R7 = DSQRT(R7/XN)
        IF (J.NE.4) WRITE (IOUT,99999)
        IF (J.EQ.4) WRITE (IOUT,99998)
        WRITE (IOUT,99997) N, A, B
        WRITE (IOUT,99996) K1, K2, K3
        WRITE (IOUT,99995) IT, IBETA
        W = X99
CS       IF (R6 .NE. ZERO) W = ALOG(ABS(R6))/ALBETA
        IF (R6.NE.ZERO) W = DLOG(DABS(R6))/ALBETA
        WRITE (IOUT,99994) R6, IBETA, W, X1, A1
CS       W = AMAX1(AIT+W,ZERO)
        W = DMAX1(AIT+W,ZERO)
        WRITE (IOUT,99993) IBETA, W
        W = X99
CS       IF (R7 .NE. ZERO) W = ALOG(ABS(R7))/ALBETA
        IF (R7.NE.ZERO) W = DLOG(DABS(R7))/ALBETA
        WRITE (IOUT,99992) R7, IBETA, W
CS       W = AMAX1(AIT+W,ZERO)
        W = DMAX1(AIT+W,ZERO)
        WRITE (IOUT,99993) IBETA, W
C------------------------------------------------------------------
C   INITIALIZE FOR NEXT TEST
C------------------------------------------------------------------
        A = B
        B = B + B
        IF (J.EQ.2) B = TEN
        IF (J.NE.3) GO TO 60
        C = (C1+C2)*HALF
        ALPHA = HALF
        A = -ALEPS*HALF + TWO
        B = A + TEN
        IZE = 2
   60 CONTINUE
C-----------------------------------------------------------------
C   TEST OF ERROR RETURNS
C
C   FIRST, TEST WITH BAD PARAMETERS
C-----------------------------------------------------------------
      WRITE (IOUT,99991)
      X = ONE
      ALPHA = HALF
      NB = 5
      IZE = 1
      U(1) = ZERO
      CALL RIBESL(X, ALPHA, NB, IZE, U, NCALC)
      WRITE (IOUT,99990) X, ALPHA, NB, IZE, U(1), NCALC
      X = -X
      U(1) = ZERO
      CALL RIBESL(X, ALPHA, NB, IZE, U, NCALC)
      WRITE (IOUT,99990) X, ALPHA, NB, IZE, U(1), NCALC
      X = -X
      ALPHA = ONE + HALF
      CALL RIBESL(X, ALPHA, NB, IZE, U, NCALC)
      WRITE (IOUT,99990) X, ALPHA, NB, IZE, U(1), NCALC
      ALPHA = HALF
      NB = -NB
      CALL RIBESL(X, ALPHA, NB, IZE, U, NCALC)
      WRITE (IOUT,99990) X, ALPHA, NB, IZE, U(1), NCALC
      NB = -NB
      IZE = 5
      CALL RIBESL(X, ALPHA, NB, IZE, U, NCALC)
      WRITE (IOUT,99990) X, ALPHA, NB, IZE, U(1), NCALC
C-----------------------------------------------------------------
C   LAST TESTS ARE WITH EXTREME PARAMETERS
C-----------------------------------------------------------------
      X = XMIN
      ALPHA = ZERO
      NB = 150
      IZE = 1
      U(1) = ZERO
      CALL RIBESL(X, ALPHA, NB, IZE, U, NCALC)
      WRITE (IOUT,99990) X, ALPHA, NB, IZE, U(1), NCALC
      X = TEN + TEN + TEN
      U(1) = ZERO
      CALL RIBESL(X, ALPHA, NB, IZE, U, NCALC)
      WRITE (IOUT,99990) X, ALPHA, NB, IZE, U(1), NCALC
      X = XMAX
      U(1) = ZERO
      CALL RIBESL(X, ALPHA, NB, IZE, U, NCALC)
      WRITE (IOUT,99990) X, ALPHA, NB, IZE, U(1), NCALC
      return
C-----------------------------------------------------------------
99999 FORMAT (40H1TEST OF I(X,ALPHA) VS SERIES EXPANSION //)
99998 FORMAT (44H1TEST OF EXP(-X)*I(X,0.5) VS 1/SQRT(2*X*PI) //)
99997 FORMAT (I7, 48H RANDOM ARGUMENTS WERE TESTED FROM THE INTERVAL ,
     * 1H(, F5.1, 1H,, F5.1, 1H)//)
99996 FORMAT (22H I(X,ALPHA) WAS LARGER, I6, 7H TIMES,/15X, 7H AGREED,
     * I6, 11H TIMES, AND/11X, 11HWAS SMALLER, I6, 7H TIMES.//)
99995 FORMAT (10H THERE ARE, I4, 5H BASE, I4, 22H SIGNIFICANT DIGITS IN,
     * 24H A FLOATING-POINT NUMBER//)
99994 FORMAT (30H THE MAXIMUM RELATIVE ERROR OF, E15.4, 3H = , I4,
     * 3H **, F7.2/4X, 16HOCCURRED FOR X =, E13.6, 10H  AND NU =, E13.6)
99993 FORMAT (27H THE ESTIMATED LOSS OF BASE, I4, 18H SIGNIFICANT DIGIT,
     * 4HS IS, F7.2//)
99992 FORMAT (40H THE ROOT MEAN SQUARE RELATIVE ERROR WAS, E15.4, 3H = ,
     * I4, 3H **, F7.2)
99991 FORMAT (24H1CHECK OF ERROR RETURNS ///24H THE FOLLOWING SUMMARIZE,
     * 33HS CALLS WITH INDICATED PARAMETERS//23H NCALC DIFFERENT FROM N,
     * 30HB INDICATES SOME FORM OF ERROR//26H SEE DOCUMENTATION FOR RIB,
     * 16HESL FOR DETAILS //7X, 3HARG, 12X, 5HALPHA, 6X, 2HNB, 3X, 2HIZ,
     * 7X, 3HRES, 6X, 5HNCALC//)
99990 FORMAT (2E15.7, 2I5, E15.7, I5//)
C     ---------- LAST CARD OF RIBESL TEST PROGRAM ----------
      END
      subroutine dgamma_test ( )

c*********************************************************************72
c
cc DGAMMA_TEST tests DGAMMA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 January 2016
c
C     PROGRAM TO TEST DGAMMA
C
C     DATA REQUIRED
C
C        NONE
C
C     SUBPROGRAMS REQUIRED FROM THIS PACKAGE
C
C        MACHAR - AN ENVIRONMENTAL INQUIRY PROGRAM PROVIDING
C                 INFORMATION ON THE FLOATING-POINT ARITHMETIC
C                 SYSTEM.  NOTE THAT THE CALL TO MACHAR CAN
C                 BE DELETED PROVIDED THE FOLLOWING FIVE
C                 PARAMETERS ARE ASSIGNED THE VALUES INDICATED
C
C                 IBETA  - THE RADIX OF THE FLOATING-POINT SYSTEM
C                 IT     - THE NUMBER OF BASE-IBETA DIGITS IN THE
C                          SIGNIFICAND OF A FLOATING-POINT NUMBER
C                 EPS    - THE SMALLEST POSITIVE FLOATING-POINT
C                          NUMBER SUCH THAT 1.0+EPS .NE. 1.0
C                 XMIN   - THE SMALLEST NON-VANISHING FLOATING-POINT
C                          INTEGRAL POWER OF THE RADIX
C                 XMAX   - THE LARGEST FINITE FLOATING-POINT NUMBER
C
C        REN(K) - A FUNCTION SUBPROGRAM RETURNING RANDOM REAL
C                 NUMBERS UNIFORMLY DISTRIBUTED OVER (0,1)
C
C
C     STANDARD FORTRAN SUBPROGRAMS REQUIRED FOR SINGLE PRECISION
C
C         ABS, ALOG, AMAX1, FLOAT, INT, SQRT
C
C     STANDARD FORTRAN SUBPROGRAMS REQUIRED FOR DOUBLE PRECISION
C
C         DABS, DLOG, DMAX1, DBLE, FLOAT, INT, SNGL, DSQRT
C
C
C     LATEST REVISION - FEBRUARY 3, 1982
C
C     AUTHOR - W. J. CODY
C              ARGONNE NATIONAL LABORATORY
C
C------------------------------------------------------------------
      INTEGER I, IBETA, IEXP, IOUT, IRND, IT, J, JT, K1, K2, K3,
     * MACHEP, MAXEXP, MINEXP, N, NEGEP, NGRD, NX
CS    REAL             A,AIT,ALBETA,ALNX,B,BETA,C,CL,CM,C1,C2,DEL,
CS   2     GAMMA,REN,EPS,EPSNEG,HALF,ONE,R6,R7,TEN,TWO,W,X,XC,XL,
CS   3     XMAX,XMIN,XMINV,XN,XNUM,XXN,XP,XPH,X99,XP99,Y,Z,ZERO,ZZ
      DOUBLE PRECISION A, AIT, ALBETA, ALNX, B, BETA, C, CL, CM, C1,
     * C2, DEL, DGAMMA, REN, EPS, EPSNEG, HALF, ONE, R6, R7, TEN, TWO,
     * W, X, XC, XL, XMAX, XMIN, XMINV, XN, XNUM, XXN, XP, XPH, X99,
     * XP99, Y, Z, ZERO, ZZ
CS    DATA C1/2.8209479177387814347E-1/
CS    DATA C2/9.1893853320467274178E-1/
CS    DATA ZERO,HALF,ONE,TWO,TEN,X99,XP99/0.0E0,0.5E0,1.0E0,2.0E0,
CS   1     10.0E0,-999.0E0,0.99E0/
      DATA C1 /2.8209479177387814347D-1/
      DATA C2 /9.1893853320467274178D-1/
      DATA ZERO, HALF, ONE, TWO, TEN, X99, XP99 /0.0D0,0.5D0,1.0D0,
     * 2.0D0,10.0D0,-999.0D0,0.99D0/
      DATA IOUT /6/
C------------------------------------------------------------------
C
C   DETERMINE MACHINE PARAMETERS AND SET CONSTANTS
C
C------------------------------------------------------------------
      CALL MACHAR(IBETA, IT, IRND, NGRD, MACHEP, NEGEP, IEXP, MINEXP,
     * MAXEXP, EPS, EPSNEG, XMIN, XMAX)
CS    BETA = FLOAT(IBETA)
      BETA = DBLE(FLOAT(IBETA))
CS    ALBETA = ALOG(BETA)
      ALBETA = DLOG(BETA)
CS    AIT = FLOAT(IT)
      AIT = DBLE(FLOAT(IT))
      A = ZERO
      B = ONE
      N = 2000
CS    XN = FLOAT(N)
      XN = DBLE(FLOAT(N))
      JT = 0
C-----------------------------------------------------------------
C     DETERMINE SMALLEST ARGUMENT FOR GAMMA
C-----------------------------------------------------------------
CS    CM = ALOG(XMIN)
      CM = DLOG(XMIN)
CS    CL = ALOG(XMAX)
      CL = DLOG(XMAX)
      XMINV = XMIN
      IF (-CM.GT.CL) XMINV = ONE/XMAX
C-----------------------------------------------------------------
C     DETERMINE LARGEST ARGUMENT FOR GAMMA BY NEWTON ITERATION
C-----------------------------------------------------------------
      XP = HALF*CL
      CL = C2 - CL
   10 X = XP
CS    ALNX = ALOG(X)
      ALNX = DLOG(X)
      XNUM = (X-HALF)*ALNX - X + CL
      XP = X - XNUM/(ALNX-HALF/X)
CS    IF (ABS(XP-X)/X .GE. TEN*EPS) GO TO 50
      IF (DABS(XP-X)/X.GE.TEN*EPS) GO TO 10
      CL = XP
C-----------------------------------------------------------------
C     RANDOM ARGUMENT ACCURACY TESTS
C-----------------------------------------------------------------
      DO 40 J=1,4
        K1 = 0
        K3 = 0
        XC = ZERO
        R6 = ZERO
        R7 = ZERO
        DEL = (B-A)/XN
        XL = A
C
        DO 30 I=1,N
          X = DEL*REN(JT) + XL
C-----------------------------------------------------------------
C      USE DUPLICATION FORMULA FOR X NOT CLOSE TO THE ZERO
C-----------------------------------------------------------------
          XP = (X*HALF+HALF) - HALF
          XPH = XP + HALF
          X = XP + XP
CS          NX = INT(X)
          NX = INT(SNGL(X))
CS          XXN = FLOAT(NX)
          XXN = DBLE(FLOAT(NX))
          C = (TWO**NX)*(TWO**(X-XXN))
CS          Z = GAMMA(X)
          Z = DGAMMA(X)
CS          ZZ = ((C*C1)*GAMMA(XP)) * GAMMA(XPH)
          ZZ = ((C*C1)*DGAMMA(XP))*DGAMMA(XPH)
          W = (Z-ZZ)/Z
          IF (W.GT.ZERO) K1 = K1 + 1
          IF (W.LT.ZERO) K3 = K3 + 1
CS          W = ABS(W)
          W = DABS(W)
          IF (W.LE.R6) GO TO 20
          R6 = W
          XC = X
   20     R7 = R7 + W*W
          XL = XL + DEL
   30   CONTINUE
C------------------------------------------------------------------
C   GATHER AND PRINT STATISTICS FOR TEST
C------------------------------------------------------------------
        K2 = N - K3 - K1
CS       R7 = SQRT(R7/XN)
        R7 = DSQRT(R7/XN)
        WRITE (IOUT,99999)
        WRITE (IOUT,99998) N, A, B
        WRITE (IOUT,99997) K1, K2, K3
        WRITE (IOUT,99996) IT, IBETA
        W = X99
CS       IF (R6 .NE. ZERO) W = ALOG(ABS(R6))/ALBETA
        IF (R6.NE.ZERO) W = DLOG(DABS(R6))/ALBETA
        WRITE (IOUT,99995) R6, IBETA, W, XC
CS       W = AMAX1(AIT+W,ZERO)
        W = DMAX1(AIT+W,ZERO)
        WRITE (IOUT,99994) IBETA, W
        W = X99
CS       IF (R7 .NE. ZERO) W = ALOG(ABS(R7))/ALBETA
        IF (R7.NE.ZERO) W = DLOG(DABS(R7))/ALBETA
        WRITE (IOUT,99993) R7, IBETA, W
CS       W = AMAX1(AIT+W,ZERO)
        W = DMAX1(AIT+W,ZERO)
        WRITE (IOUT,99994) IBETA, W
C------------------------------------------------------------------
C   INITIALIZE FOR NEXT TEST
C------------------------------------------------------------------
        A = B
        IF (J.EQ.1) B = A + A
        IF (J.EQ.2) B = TEN
        IF (J.EQ.3) B = CL - HALF
   40 CONTINUE
C-----------------------------------------------------------------
C    SPECIAL TESTS
C
C    FIRST TEST WITH SPECIAL ARGUMENTS
C-----------------------------------------------------------------
      WRITE (IOUT,99992)
      WRITE (IOUT,99991)
      X = -HALF
CS    Y = GAMMA(X)
      Y = DGAMMA(X)
      WRITE (IOUT,99990) X, Y
      X = XMINV/XP99
CS    Y = GAMMA(X)
      Y = DGAMMA(X)
      WRITE (IOUT,99990) X, Y
      X = ONE
CS    Y = GAMMA(X)
      Y = DGAMMA(X)
      WRITE (IOUT,99990) X, Y
      X = TWO
CS    Y = GAMMA(X)
      Y = DGAMMA(X)
      WRITE (IOUT,99990) X, Y
      X = CL*XP99
CS    Y = GAMMA(X)
      Y = DGAMMA(X)
      WRITE (IOUT,99990) X, Y
C-----------------------------------------------------------------
C   TEST OF ERROR RETURNS
C-----------------------------------------------------------------
      WRITE (IOUT,99989)
      X = -ONE
      WRITE (IOUT,99988) X
CS    Y = GAMMA(X)
      Y = DGAMMA(X)
      WRITE (IOUT,99987) Y
      X = ZERO
      WRITE (IOUT,99988) X
CS    Y = GAMMA(X)
      Y = DGAMMA(X)
      WRITE (IOUT,99987) Y
      X = XMINV*(ONE-EPS)
      WRITE (IOUT,99988) X
CS    Y = GAMMA(X)
      Y = DGAMMA(X)
      WRITE (IOUT,99987) Y
      X = CL*(ONE+EPS)
      WRITE (IOUT,99988) X
CS    Y = GAMMA(X)
      Y = DGAMMA(X)
      WRITE (IOUT,99987) Y
      WRITE (IOUT,99986)
      STOP
C-----------------------------------------------------------------
99999 FORMAT (40H1TEST OF GAMMA(X) VS DUPLICATION FORMULA//)
99998 FORMAT (I7, 48H RANDOM ARGUMENTS WERE TESTED FROM THE INTERVAL ,
     * 1H(, F5.1, 1H,, F5.1, 1H)//)
99997 FORMAT (20H GAMMA(X) WAS LARGER, I6, 7H TIMES,/13X, 7H AGREED,
     * I6, 11H TIMES, AND/9X, 11HWAS SMALLER, I6, 7H TIMES.//)
99996 FORMAT (10H THERE ARE, I4, 5H BASE, I4, 22H SIGNIFICANT DIGITS IN,
     * 24H A FLOATING-POINT NUMBER//)
99995 FORMAT (30H THE MAXIMUM RELATIVE ERROR OF, E15.4, 3H = , I4,
     * 3H **, F7.2/4X, 16HOCCURRED FOR X =, E13.6)
99994 FORMAT (27H THE ESTIMATED LOSS OF BASE, I4, 18H SIGNIFICANT DIGIT,
     * 4HS IS, F7.2//)
99993 FORMAT (40H THE ROOT MEAN SQUARE RELATIVE ERROR WAS, E15.4, 3H = ,
     * I4, 3H **, F7.2)
99992 FORMAT (14H1SPECIAL TESTS//)
99991 FORMAT (//26H TEST OF SPECIAL ARGUMENTS//)
99990 FORMAT (8H GAMMA (, E13.6, 4H) = , E13.6//)
99989 FORMAT (22H1TEST OF ERROR RETURNS///)
99988 FORMAT (39H GAMMA WILL BE CALLED WITH THE ARGUMENT, E13.6,
     * /37H THIS SHOULD TRIGGER AN ERROR MESSAGE//)
99987 FORMAT (25H GAMMA RETURNED THE VALUE, E13.6///)
99986 FORMAT (25H THIS CONCLUDES THE TESTS)
C     ---------- LAST CARD OF GAMMA TEST PROGRAM ----------
      END
      SUBROUTINE MACHAR(IBETA, IT, IRND, NGRD, MACHEP, NEGEP, IEXP,
     * MINEXP, MAXEXP, EPS, EPSNEG, XMIN, XMAX)

c*********************************************************************72
c
cc MACHAR determines characteristics of the floating point arithmetic.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 January 2016
c
      INTEGER I, IBETA, IEXP, IRND, IT, IZ, J, K, MACHEP, MAXEXP,
     * MINEXP, MX, NEGEP, NGRD
CS    REAL A,B,BETA,BETAIN,BETAM1,EPS,EPSNEG,ONE,XMAX,XMIN,Y,Z,ZERO
      DOUBLE PRECISION A, B, BETA, BETAIN, BETAM1, EPS, EPSNEG, ONE,
     * XMAX, XMIN, Y, Z, ZERO
C
C     THIS SUBROUTINE IS INTENDED TO DETERMINE THE CHARACTERISTICS
C     OF THE FLOATING-POINT ARITHMETIC SYSTEM THAT ARE SPECIFIED
C     BELOW.  THE FIRST THREE ARE DETERMINED ACCORDING TO AN
C     ALGORITHM DUE TO M. MALCOLM, CACM 15 (1972), PP. 949-951,
C     INCORPORATING SOME, BUT NOT ALL, OF THE IMPROVEMENTS
C     SUGGESTED BY M. GENTLEMAN AND S. MAROVICH, CACM 17 (1974),
C     PP. 276-277.  THE VERSION GIVEN HERE IS FOR SINGLE PRECISION.
C     CARDS CONTAINING  CD  IN COLUMNS 1 AND 2 CAN BE USED TO
C     CONVERT THE SUBROUTINE TO DOUBLE PRECISION BY REPLACING
C     EXISTING CARDS IN THE OBVIOUS MANNER.
C
C
C       IBETA   - THE RADIX OF THE FLOATING-POINT REPRESENTATION
C       IT      - THE NUMBER OF BASE IBETA DIGITS IN THE FLOATING-POINT
C                 SIGNIFICAND
C       IRND    - 0 IF FLOATING-POINT ADDITION CHOPS,
C                 1 IF FLOATING-POINT ADDITION ROUNDS
C       NGRD    - THE NUMBER OF GUARD DIGITS FOR MULTIPLICATION.  IT IS
C                 0 IF  IRND=1, OR IF  IRND=0  AND ONLY  IT  BASE  IBETA
C                   DIGITS PARTICIPATE IN THE POST NORMALIZATION SHIFT
C                   OF THE FLOATING-POINT SIGNIFICAND IN MULTIPLICATION
C                 1 IF  IRND=0  AND MORE THAN  IT  BASE  IBETA  DIGITS
C                   PARTICIPATE IN THE POST NORMALIZATION SHIFT OF THE
C                   FLOATING-POINT SIGNIFICAND IN MULTIPLICATION
C       MACHEP  - THE LARGEST NEGATIVE INTEGER SUCH THAT
C                 1.0+FLOAT(IBETA)**MACHEP .NE. 1.0, EXCEPT THAT
C                 MACHEP IS BOUNDED BELOW BY  -(IT+3)
C       NEGEPS  - THE LARGEST NEGATIVE INTEGER SUCH THAT
C                 1.0-FLOAT(IBETA)**NEGEPS .NE. 1.0, EXCEPT THAT
C                 NEGEPS IS BOUNDED BELOW BY  -(IT+3)
C       IEXP    - THE NUMBER OF BITS (DECIMAL PLACES IF IBETA = 10)
C                 RESERVED FOR THE REPRESENTATION OF THE EXPONENT
C                 (INCLUDING THE BIAS OR SIGN) OF A FLOATING-POINT
C                 NUMBER
C       MINEXP  - THE LARGEST IN MAGNITUDE NEGATIVE INTEGER SUCH THAT
C                 FLOAT(IBETA)**MINEXP IS A POSITIVE FLOATING-POINT
C                 NUMBER
C       MAXEXP  - THE LARGEST POSITIVE INTEGER EXPONENT FOR A FINITE
C                 FLOATING-POINT NUMBER
C       EPS     - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH
C                 THAT  1.0+EPS .NE. 1.0. IN PARTICULAR, IF EITHER
C                 IBETA = 2  OR  IRND = 0, EPS = FLOAT(IBETA)**MACHEP.
C                 OTHERWISE,  EPS = (FLOAT(IBETA)**MACHEP)/2
C       EPSNEG  - A SMALL POSITIVE FLOATING-POINT NUMBER SUCH THAT
C                 1.0-EPSNEG .NE. 1.0. IN PARTICULAR, IF IBETA = 2
C                 OR  IRND = 0, EPSNEG = FLOAT(IBETA)**NEGEPS.
C                 OTHERWISE,  EPSNEG = (IBETA**NEGEPS)/2.  BECAUSE
C                 NEGEPS IS BOUNDED BELOW BY -(IT+3), EPSNEG MAY NOT
C                 BE THE SMALLEST NUMBER WHICH CAN ALTER 1.0 BY
C                 SUBTRACTION.
C       XMIN    - THE SMALLEST NON-VANISHING FLOATING-POINT POWER OF THE
C                 RADIX.  IN PARTICULAR,  XMIN = FLOAT(IBETA)**MINEXP
C       XMAX    - THE LARGEST FINITE FLOATING-POINT NUMBER.  IN
C                 PARTICULAR   XMAX = (1.0-EPSNEG)*FLOAT(IBETA)**MAXEXP
C                 NOTE - ON SOME MACHINES  XMAX  WILL BE ONLY THE
C                 SECOND, OR PERHAPS THIRD, LARGEST NUMBER, BEING
C                 TOO SMALL BY 1 OR 2 UNITS IN THE LAST DIGIT OF
C                 THE SIGNIFICAND.
C
C     LATEST REVISION - OCTOBER 22, 1979
C
C     AUTHOR - W. J. CODY
C              ARGONNE NATIONAL LABORATORY
C
C-----------------------------------------------------------------
CS    ONE = FLOAT(1)
      ONE = DBLE(FLOAT(1))
CS    ZERO = 0.0E0
      ZERO = 0.0D0
C-----------------------------------------------------------------
C     DETERMINE IBETA,BETA ALA MALCOLM
C-----------------------------------------------------------------
      A = ONE
   10 A = A + A
      IF (((A+ONE)-A)-ONE.EQ.ZERO) GO TO 10
      B = ONE
   20 B = B + B
      IF ((A+B)-A.EQ.ZERO) GO TO 20
CS    IBETA = INT((A+B)-A)
      IBETA = INT(SNGL((A+B)-A))
CS    BETA = FLOAT(IBETA)
      BETA = DBLE(FLOAT(IBETA))
C-----------------------------------------------------------------
C     DETERMINE IT, IRND
C-----------------------------------------------------------------
      IT = 0
      B = ONE
   30 IT = IT + 1
      B = B*BETA
      IF (((B+ONE)-B)-ONE.EQ.ZERO) GO TO 30
      IRND = 0
      BETAM1 = BETA - ONE
      IF ((A+BETAM1)-A.NE.ZERO) IRND = 1
C-----------------------------------------------------------------
C     DETERMINE NEGEP, EPSNEG
C-----------------------------------------------------------------
      NEGEP = IT + 3
      BETAIN = ONE/BETA
      A = ONE
C
      DO 40 I=1,NEGEP
        A = A*BETAIN
   40 CONTINUE
C
      B = A
   50 IF ((ONE-A)-ONE.NE.ZERO) GO TO 60
      A = A*BETA
      NEGEP = NEGEP - 1
      GO TO 50
   60 NEGEP = -NEGEP
      EPSNEG = A
      IF ((IBETA.EQ.2) .OR. (IRND.EQ.0)) GO TO 70
      A = (A*(ONE+A))/(ONE+ONE)
      IF ((ONE-A)-ONE.NE.ZERO) EPSNEG = A
C-----------------------------------------------------------------
C     DETERMINE MACHEP, EPS
C-----------------------------------------------------------------
   70 MACHEP = -IT - 3
      A = B
   80 IF ((ONE+A)-ONE.NE.ZERO) GO TO 90
      A = A*BETA
      MACHEP = MACHEP + 1
      GO TO 80
   90 EPS = A
      IF ((IBETA.EQ.2) .OR. (IRND.EQ.0)) GO TO 100
      A = (A*(ONE+A))/(ONE+ONE)
      IF ((ONE+A)-ONE.NE.ZERO) EPS = A
C-----------------------------------------------------------------
C     DETERMINE NGRD
C-----------------------------------------------------------------
  100 NGRD = 0
      IF ((IRND.EQ.0) .AND. ((ONE+EPS)*ONE-ONE).NE.ZERO) NGRD = 1
C-----------------------------------------------------------------
C     DETERMINE IEXP, MINEXP, XMIN
C
C     LOOP TO DETERMINE LARGEST I AND K = 2**I SUCH THAT
C         (1/BETA) ** (2**(I))
C     DOES NOT UNDERFLOW
C     EXIT FROM LOOP IS SIGNALED BY AN UNDERFLOW.
C-----------------------------------------------------------------
      I = 0
      K = 1
      Z = BETAIN
  110 Y = Z
      Z = Y*Y
C-----------------------------------------------------------------
C        CHECK FOR UNDERFLOW HERE
C-----------------------------------------------------------------
      A = Z*ONE
CS       IF ((A+A .EQ. ZERO) .OR. (ABS(Z) .GE. Y)) GO TO 410
      IF ((A+A.EQ.ZERO) .OR. (DABS(Z).GE.Y)) GO TO 120
      I = I + 1
      K = K + K
      GO TO 110
  120 IF (IBETA.EQ.10) GO TO 130
      IEXP = I + 1
      MX = K + K
      GO TO 160
C-----------------------------------------------------------------
C     FOR DECIMAL MACHINES ONLY
C-----------------------------------------------------------------
  130 IEXP = 2
      IZ = IBETA
  140 IF (K.LT.IZ) GO TO 150
      IZ = IZ*IBETA
      IEXP = IEXP + 1
      GO TO 140
  150 MX = IZ + IZ - 1
C-----------------------------------------------------------------
C     LOOP TO DETERMINE MINEXP, XMIN
C     EXIT FROM LOOP IS SIGNALED BY AN UNDERFLOW.
C-----------------------------------------------------------------
  160 XMIN = Y
      Y = Y*BETAIN
C-----------------------------------------------------------------
C        CHECK FOR UNDERFLOW HERE
C-----------------------------------------------------------------
      A = Y*ONE
CS       IF (((A+A) .EQ. ZERO) .OR. (ABS(Y) .GE. XMIN)) GO TO 460
      IF (((A+A).EQ.ZERO) .OR. (DABS(Y).GE.XMIN)) GO TO 170
      K = K + 1
      GO TO 160
  170 MINEXP = -K
C-----------------------------------------------------------------
C     DETERMINE MAXEXP, XMAX
C-----------------------------------------------------------------
      IF ((MX.GT.K+K-3) .OR. (IBETA.EQ.10)) GO TO 180
      MX = MX + MX
      IEXP = IEXP + 1
  180 MAXEXP = MX + MINEXP
C-----------------------------------------------------------------
C     ADJUST FOR MACHINES WITH IMPLICIT LEADING
C     BIT IN BINARY SIGNIFICAND AND MACHINES WITH
C     RADIX POINT AT EXTREME RIGHT OF SIGNIFICAND
C-----------------------------------------------------------------
      I = MAXEXP + MINEXP
      IF ((IBETA.EQ.2) .AND. (I.EQ.0)) MAXEXP = MAXEXP - 1
      IF (I.GT.20) MAXEXP = MAXEXP - 1
      IF (A.NE.Y) MAXEXP = MAXEXP - 2
      XMAX = ONE - EPSNEG
      IF (XMAX*ONE.NE.XMAX) XMAX = ONE - BETA*EPSNEG
      XMAX = XMAX/(BETA*BETA*BETA*XMIN)
      I = MAXEXP + MINEXP + 3
      IF (I.LE.0) GO TO 200
C
      DO 190 J=1,I
        IF (IBETA.EQ.2) XMAX = XMAX + XMAX
        IF (IBETA.NE.2) XMAX = XMAX*BETA
  190 CONTINUE
C
  200 RETURN
C     ---------- LAST CARD OF MACHAR ----------
      END
      DOUBLE PRECISION FUNCTION REN(K)

c*********************************************************************72
c
cc REN is a random number generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 January 2016
C
C     RANDOM NUMBER GENERATOR - BASED ON ALGORITHM 266 BY PIKE AND
C      HILL (MODIFIED BY HANSSON), COMMUNICATIONS OF THE ACM,
C      VOL. 8, NO. 10, OCTOBER 1965.
C
C     THIS SUBPROGRAM IS INTENDED FOR USE ON COMPUTERS WITH
C      FIXED POINT WORDLENGTH OF AT LEAST 29 BITS.  IT IS
C      BEST IF THE FLOATING POINT SIGNIFICAND HAS AT MOST
C      29 BITS.
C
      INTEGER IY, J, K
      DATA IY /100001/
C
      J = K
      IY = IY*125
      IY = IY - (IY/2796203)*2796203
      REN = DBLE(FLOAT(IY))/2796203.0D0*(1.0D0+1.0D-6+1.0D-12)
      RETURN
C     ---------- LAST CARD OF REN ----------
      END
      subroutine timestamp ( )

c*********************************************************************72
c
cc timestamp() prints out the current YMDHMS date as a timestamp.
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

