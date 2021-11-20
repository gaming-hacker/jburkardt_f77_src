      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS715_PRB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 January 2016
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS715_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS715 library.'

      call anrtst ( )
      call dawtst ( )
      call dlgtst ( )
      call eitest ( )
      call erftst ( )
      call gamtst ( )
      call i0test ( )
      call i1test ( )
      call j0test ( )
      call j1test ( )
      call k0test ( )
      call k1test ( )
      call psitst ( )
      call ritest ( )
      call rjtest ( )
      call rktest ( )
      call rytest ( )
      call y0test ( )
      call y1test ( )
c
c  Terminate
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS715_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine anrtst ( )

c*********************************************************************72
c
cc ANRTST tests ANORM.
C
C  Method:
C
C     Accuracy test compares function values against local Taylor's
C     series expansions.  Derivatives for anorm(x) are expressed as
C     repeated integrals of erfc(-x/sqrt(2)).  These are generated
C     from the recurrence relation using a technique due to Gautschi
C     (see references).
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following five
C              parameters are assigned the values indicated
C
C              IBETA  - The radix of the floating-point system
C              IT     - The number of base-ibeta digits in the
C                       significant of a floating-point number
C              EPS    - The smallest positive floating-point
C                       number such that 1.0+EPS .NE. 1.0
C              XMIN   - The smallest non-vanishing floating-point
C                       integral power of the radix
C              XMAX   - The largest finite floating-point number
C
C     REN(K) - A function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  References: "Performance evaluation of programs for the error
C               and complementary error functions", W. J. Cody,
C               TOMS 16, 1990, pp. 29-37.
C
C              "Evaluation of the repeated integrals of the coerror
C               function", W. Gautschi, TOMS 3, 1977, pp. 240-252.
C
C  Latest modification: March 15, 1992
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division 
C          Argonne National Laboratory
C          Argonne, IL 60439
C
      implicit none

      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NEGEP,NGRD,N0,N1
      DOUBLE PRECISION ANORM,
     1    A,AIT,ALBETA,B,BETA,C,CONV,C1,C2,DEL,EPS,EPSCON,EPSNEG,FF,
     2    F0,HALF,ONE,R,REN,ROOT32,R1,R6,R7,SC,SIXTEN,SQRTWO,THRESH,
     3    TEN,TWO,U,V,W,X,XBIG,XC,XL,XMAX,XMIN,XN,XN1,X99,Y,Z,ZERO
      DIMENSION R1(500)
c
C   C1 = 1/sqrt(pi)
c
      DATA HALF,ZERO,ONE,TWO,TEN/0.5D0,0.0D0,1.0D0,2.0D0,10.0D0/,
     1     SIXTEN,THRESH,X99/16.0D0,0.66291D0,-999.0D0/,
     2     C1/5.6418958354775628695D-1/,ROOT32/-5.65685D0/,
     3     SQRTWO/7.0710678118654752440D-1/
      DATA IOUT/6/
c
C  Statement functions for conversion 
c
      CONV(I) = DBLE(I)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      C = ABS(AIT*ALBETA) + TEN
      A = -THRESH
      B = THRESH
      N = 2000
      XN = CONV(N)
      JT = 0
      N0 = (IBETA/2)*(IT+5)/6+4
c
C  Determine largest argument for ANORM test by Newton iteration
c
      C2 = LOG(XMIN/C1/SQRTWO)
      XBIG = SQRT(-C2)
   50 X = XBIG
         F0 = X*X
         FF = F0 *HALF + ONE/F0 + LOG(X) + C2
         F0 = X + ONE/X - TWO/(X*F0-X)
         XBIG = X - FF/F0
         IF (ABS(X-XBIG)/X .GT. TEN*EPS) GO TO 50
c
C  Random argument accuracy tests
c
      DO 300 J = 1, 3
         K1 = 0
         K3 = 0
         XC = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
            IF (J .EQ. 1) THEN
c
C  Test anorm against double series expansion
c
                  F0 = CONV(N0)
                  FF = F0+F0+ONE 
                  W = X*X
                  Z = W*HALF
                  U = ZERO
                  V = ZERO
                  DO 60 K = 1, N0
                     U = -Z/F0*(ONE+U)
                     V = W/FF*(ONE+V)
                     F0 = F0 - ONE
                     FF = FF - TWO
   60             CONTINUE
                  V = HALF + SQRTWO*C1*X*(((U*V+(U+V))+HALF)+HALF)
                  U = ANORM(X)
               ELSE
c
C  Test anorm against expansion in repeated
C   integrals of the coerror function.
c
                  Y = X - HALF
                  X = Y + HALF
                  U = ANORM(X)
                  Z = -Y/SQRT(TWO)
                  R = ZERO
                  IF (Z .LE. ONE) THEN
                        N0 = 499
                     ELSE
                        N0 = MIN(499,INT(C/(ABS(LOG(Z)))))
                  END IF
                  N1 = N0 
                  XN1 = CONV(N1+1)
                  DO 100 K=1,N0
                     R = HALF/(Z+XN1*R)
                     R1(N1) = R
                     N1 = N1 - 1
                     XN1 = XN1 - ONE
  100             CONTINUE
                  FF = C1/(Z+R1(1))
                  F0 = ANORM(Y)*EXP((-Y-HALF*HALF)*HALF)
                  SC = F0/FF
c
C  Scale things to avoid premature underflow
c
                  EPSCON = F0 
                  FF = SIXTEN*FF/EPS
                  DO 110 N1=1,N0
                     FF = R1(N1)*FF
                     R1(N1) = FF * SC
                     IF (R1(N1) .LT. EPSCON ) THEN
                        K = N1
                        GO TO 111
                     END IF
  110             CONTINUE
  111             V = SQRTWO*R1(K)
                  DO 120 N1 = 1, K-1
                     V = (V + R1(K-N1))*SQRTWO
  120             CONTINUE
c
C  Remove scaling here
c
                  V = V*EPS/SIXTEN + F0
            END IF
c
C  Accumulate results
c
            W = (U - V) / U
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  XC = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C  Gather and print statistics for test
c
         K2 = N - K3 - K1
         R7 = SQRT(R7/XN)
         IF (J .EQ. 1) THEN
               WRITE (IOUT,1000)
            ELSE 
               WRITE (IOUT,1001)
         END IF
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1
         WRITE (IOUT,1015) K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,XC
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C  Initialize for next test
c
         IF (J .EQ. 1) THEN
               B = A
               A = ROOT32
            ELSE
               B = A
               A = -AINT(XBIG*SIXTEN)/SIXTEN+HALF
         END IF
  300 CONTINUE
c
C  Special tests
C  First check values for positive arguments.
c
      WRITE (IOUT,1025)
      WRITE (IOUT,1030)
      DEL = TEN
      DO 350 I = 1, 10
         X = REN(JT) * DEL
         U = ANORM(-X)
         A = U + ANORM(X)
         Y = (A - HALF) - HALF
         WRITE (IOUT,1032) X, U, Y
  350 CONTINUE
c
C  Test with special arguments
c
      WRITE (IOUT,1040)
      Z = XMAX
      Y = ANORM(Z)
      WRITE (IOUT,1041) Z,Y
      Z = ZERO
      Y = ANORM(Z)
      WRITE (IOUT,1041) Z,Y
      Z = -XMAX
      Y = ANORM(Z)
      WRITE (IOUT,1041) Z,Y
c
C  Test of error returns
c
      WRITE (IOUT,1050)
      W = XBIG
      Z = -W*(ONE-HALF*HALF)
      WRITE (IOUT,1052) Z
      Y = ANORM(Z)
      WRITE (IOUT,1062) Y
      Z = -W*(ONE+TEN*EPS)
      WRITE (IOUT,1053) Z
      Y = ANORM(Z)
      WRITE (IOUT,1062) Y
      WRITE (IOUT,1100)
      return
c
 1000 FORMAT('1Test of anorm(x) vs double series expansion'//)
 1001 FORMAT(///' Test of anorm(x) vs Taylor series about x-1/2'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval (',
     1    F7.3,',',F7.3,')'//)
 1011 FORMAT('  ANORM(X) was larger',I6,' times,')
 1015 FORMAT(14X,' agreed',I6,' times, and'/
     1    10X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1025 FORMAT('1Special Tests'//)
 1030 FORMAT(7X,'Check of identity anorm(X) + anorm(-X) = 1.0'//
     1       9X,'X',12X,'ANORM(-x)',3X,'ANORM(x)+ANORM(-x)-1'/)
 1032 FORMAT(3(3X,E13.6)/)
 1040 FORMAT(//' Test of special arguments'//)
 1041 FORMAT(' ANORM (',E13.6,') = ',E13.6//)
 1050 FORMAT(' Test of Error Returns'///)
 1052 FORMAT(' ANORM will be called with the argument ',E13.6,/
     1    ' The result should not underflow'//)
 1053 FORMAT(' ANORM will be called with the argument ',E13.6,/
     1    ' The result may underflow'//)
 1062 FORMAT(' ANORM returned the value',E13.6///)
 1100 FORMAT(' This concludes the tests')
      END
      subroutine dawtst ( )

c*********************************************************************72
c
cc DAWTST tests DAW.
C
C  Method:
C
C     Accuracy test compare function values against a local
C     Taylor's series expansion.  Derivatives are generated
C     from the recurrence relation.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C         information on the floating-point arithmetic
C         system.  Note that the call to MACHAR can
C         be deleted provided the following five
C         parameters are assigned the values indicated
C
C              IBETA  - the radix of the floating-point system
C              IT     - the number of base-IBETA digits in the
C                       significant of a floating-point number
C              XMIN   - the smallest positive floating-point number
C              XMAX   - the largest floating-point number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  Reference: "The use of Taylor series to test accuracy of
C              function programs", W. J. Cody and L. Stoltz,
C              submitted for publication.
C
C  Latest modification: March 9, 1992
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
      implicit none

      INTEGER I,IBETA,IEXP,II,IOUT,IRND,IT,J,JT,K1,
     1    K2,K3,MACHEP,MAXEXP,MINEXP,N,NDUM,NEGEP,NGRD
      DOUBLE PRECISION  
     1    A,AIT,ALBETA,B,BETA,CONV,DAW,DEL,DELTA,EPS,EPSNEG,
     2    FORTEN,HALF,ONE,P,REN,R6,R7,SIXTEN,TWO,T1,W,X,
     3    XBIG,XKAY,XL,XMAX,XMIN,XN,X1,X99,Y,Z,ZERO,ZZ
      DIMENSION P(0:14)
c
      DATA ZERO,HALF,ONE,TWO/0.0D0,0.5D0,1.0D0,2.0D0/,
     1   FORTEN,SIXTEN,X99,DELTA/14.0D0,1.6D1,-999.0D0,6.25D-2/
      DATA IOUT/6/
c
C  Define statement functions for conversions
c
      CONV(NDUM) = DBLE(NDUM)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      AIT = CONV(IT)
      ALBETA = LOG(BETA)
      A = DELTA
      B = ONE
      JT = 0
c
C  Random argument accuracy tests based on local Taylor expansion.
c
      DO 300 J = 1, 4
         N = 2000 
         XN = CONV(N)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
c
C  Purify arguments
c
            Y = X - DELTA
            W = SIXTEN * Y
            T1 = W + Y
            Y = T1 - W
            X = Y + DELTA
c
C  Use Taylor's Series Expansion
c
            P(0) = DAW(Y)
            Z = Y + Y
            P(1) = ONE - Z * P(0)
            XKAY = TWO
            DO 100 II = 2, 14
               P(II) = -(Z*P(II-1)+XKAY*P(II-2))
               XKAY = XKAY + TWO
  100       CONTINUE
            ZZ = P(14)
            XKAY = FORTEN
            DO 110 II = 1, 14
               ZZ = ZZ*DELTA/XKAY + P(14-II)
               XKAY = XKAY - ONE
  110       CONTINUE
            Z = DAW(X)
c
C  Accumulate Results
c
            W = (Z - ZZ) / Z
            IF (W .GT. ZERO) THEN 
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN 
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C  Gather and print statistics for test
c
         K2 = N - K1 - K3
         R7 = SQRT(R7/XN)
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         W = X99
         IF (R6 .NE. ZERO) W = LOG(R6)/ALBETA
         WRITE (IOUT,1021) R6,IBETA,W,X1
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         W = X99
         IF (R7 .NE. ZERO) W = LOG(R7)/ALBETA
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C  Initialize for next test
c
         A = B
         B = B + B
         IF (J .EQ. 1) B = B + HALF
  300 CONTINUE
c
C  Special tests.  First check values for negative arguments.
c
      WRITE (IOUT,1025)
      WRITE (IOUT,1030) IBETA
      DO 350 I = 1, 10
         X = REN(J)*(TWO+TWO)
         B = DAW(X)
         A = B + DAW(-X)
         IF (A*B .NE. ZERO) A = AIT + LOG(ABS(A/B))/ALBETA
         WRITE (IOUT,1031) X,A
         X = X + DEL
  350 CONTINUE
c
C  Next, test with special arguments
c
      WRITE (IOUT,1040)
      Z = XMIN
      ZZ = DAW(Z)
      WRITE (IOUT,1041) ZZ
c
C  Test of error return for arguments > xmax.  First, determine
C    xmax
c
      IF (HALF .LT. XMIN*XMAX ) THEN
            XBIG = HALF/XMIN
         ELSE
            XBIG = XMAX
      END IF
      WRITE (IOUT,1050)
      Z = XBIG*(ONE-DELTA*DELTA)
      WRITE (IOUT,1052) Z
      ZZ = DAW(Z)
      WRITE (IOUT,1062) ZZ
      Z = XBIG
      WRITE (IOUT,1053) Z
      ZZ = DAW(Z)
      WRITE (IOUT,1062) ZZ
      W = ONE + DELTA*DELTA
      IF (W .LT. XMAX/XBIG ) THEN
            Z = XBIG*W
            WRITE (IOUT,1053) Z
            ZZ = DAW(Z)
            WRITE (IOUT,1062) ZZ
      END IF
      WRITE (IOUT,1100)
      return
c
 1000 FORMAT('1Test of Dawson''s Integral vs Taylor expansion'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval ',
     1    '(',F5.2,',',F5.2,')'//)
 1011 FORMAT('  F(X) was larger',I6,' times,'/
     1    10X,' agreed',I6,' times, and'/
     1    6X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1025 FORMAT('1Special Tests'//)
 1030 FORMAT(7X,'Estimated loss of base',i3,' significant digits in'//
     1       8X'X',10X,'F(x)+F(-x)'/)
 1031 FORMAT(3XF7.3,F16.2)
 1040 FORMAT(//' Test of special arguments'//)
 1041 FORMAT('  F(XMIN) = ',E24.17/)
 1050 FORMAT(' Test of Error Returns'///)
 1052 FORMAT(' DAW will be called with the argument',E13.6,/
     1    ' This should not underflow'//)
 1053 FORMAT(' DAW will be called with the argument',E13.6,/
     1    ' This may underflow'//)
 1062 FORMAT(' DAW returned the value',E13.6///)
 1100 FORMAT(' This concludes the tests')
      END
      subroutine dlgtst ( )

c*********************************************************************72
c
cc DLGTST tests DLGAMA.
C
C   Method:
C
C      Accuracy tests compare function values against values
C      generated with the duplication formula.
C
C   Data required
C
C      None
C
C   Subprograms required from this package
C
C      MACHAR - An environmental inquiry program providing
C               information on the floating-point arithmetic
C               system.  Note that the call to MACHAR can
C               be deleted provided the following five
C               parameters are assigned the values indicated
C
C               IBETA  - The radix of the floating-point system
C               IT     - The number of base-ibeta digits in the
C                        significant of a floating-point number
C               EPS    - The smallest positive floating-point
C                        number such that 1.0+EPS .NE. 1.0
C               XMIN   - The smallest non-vanishing floating-point
C                        integral power of the radix
C               XMAX   - The largest finite floating-point number
C
C      REN(K) - A function subprogram returning random real
C               numbers uniformly distributed over (0,1)
C
C
C    Intrinsic functions required are:
C
C         ABS, ANINT, DBLE, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs related
C              to the real gamma function", W. J. Cody, 
C              submitted for publication.
C
C  Latest modification: March 9, 1992
C
C  Authors: W. J. Cody and L. Stoltz
C           Mathematics and Computer Science Division 
C           Argonne National Laboratory
C           Argonne, IL 60439
C
      implicit none

      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NEGEP,NGRD    
      DOUBLE PRECISION DLGAMA, 
     1    A,AIT,ALBETA,ALL9,B,BETA,CL,CONV,C1,C2,C3,DEL,EPS,EPSNEG,
     2    FUNC,HALF,ONE,P875,P3125,P625,P6875,REN,R6,R7,SIXTEN,TEN,
     3    TWO,U,V,W,X,XC,XL,XMAX,XMIN,XN,XP99,Y,Z,ZERO,ZZ
c
C   C1 = 0.5 - LN(SQRT(PI))
C   C2 = LN(2)
C   C3 = LN(2) - 11/16
c
      DATA C1,C2/-7.2364942924700087072D-2,6.9314718055994530942D-1/,
     1     C3,ZERO,HALF/5.6471805599453094172D-3,0.0D0,0.5D0/,
     2     ONE,TWO,TEN,SIXTEN/1.0D0,2.0D0,10.0D0,16.0D0/,
     3     P6875,P875,P3125,P625/0.6875D0,0.875D0,1.3125D0,1.625D0/,
     4     ALL9,XP99/-999.0D0,0.99D0/
      DATA IOUT/6/
c
C  Statement functions for conversion 
c
      CONV(I) = DBLE(I)
      FUNC(X) = DLGAMA(X) 
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      A = ZERO
      B = P875
      N = 2000
      XN = CONV(N)
      JT = 0
c
C  Determine largest argument for DLGAMA by iteration
c
      CL = XP99 * XMAX
      Z = -CL / ALL9
   80 ZZ = CL / (LOG(Z)-ONE)
      IF (ABS(ZZ/Z-ONE) .GT. (TWO*BETA*EPS)) THEN
         Z = ZZ
         GO TO 80
      END IF
      CL = ZZ
c
C  Random argument accuracy tests
c
      DO 300 J = 1, 3
         K1 = 0
         K3 = 0
         XC = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
c
C  Use duplication formula
c
            IF (J .NE. 3) THEN
                  IF (J .EQ. 1) THEN
                        Z = X + HALF
                        X = Z - HALF
                        Y = X + X
                     ELSE 
                        X = X + X
                        X = X * HALF
                        Y = (X + X) - ONE
                        Z = X - HALF
                  END IF
                  U = FUNC(X)
                  W = (Y-HALF)-HALF
                  ZZ = ANINT(W*SIXTEN)/SIXTEN
                  W = W - ZZ
                  V = (((HALF-ZZ*P6875) - C1) - W*P6875)-C3*(W+ZZ)
                  V = ((V + FUNC(Y)) - FUNC(Z))
               ELSE
                  Z = X * HALF + HALF
                  Y = Z - HALF
                  X = Y + Y
                  U = FUNC(X)
                  V = (C1 + ((X-HALF)-HALF)*C2)+FUNC(Y)+FUNC(Z)-HALF
            END IF
c
C  Accumulate results
c
            W = (U - V) / U
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  XC = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C  Gather and print statistics for test
c
         K2 = N - K3 - K1
         R7 = SQRT(R7/XN)
         IF (J .EQ. 1) THEN
               WRITE (IOUT,1000)
            ELSE IF (J .EQ. 2) THEN
               WRITE (IOUT,1001)
            ELSE
               WRITE (IOUT,1002)
         END IF
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,XC
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C  Initialize for next test
c
         A = P3125
         B = P625
         IF (J .EQ. 2) THEN
               A = TWO + TWO
               B = TEN + TEN
         END IF
  300 CONTINUE
c
C  Special tests
C  First test with special arguments
c
      WRITE (IOUT,1025)
      WRITE (IOUT,1040)
      Z = EPS
      ZZ = FUNC(Z)
      WRITE (IOUT,1041) Z,ZZ
      Z = HALF
      ZZ = FUNC(Z)
      WRITE (IOUT,1041) Z,ZZ
      Z = ONE
      ZZ = FUNC(Z)
      WRITE (IOUT,1041) Z,ZZ
      Z = TWO
      ZZ = FUNC(Z)
      WRITE (IOUT,1041) Z,ZZ
c
C  Test of error returns
c
      WRITE (IOUT,1050)
      Z = XMIN
      WRITE (IOUT,1053) Z
      ZZ = FUNC(Z)
      WRITE (IOUT,1061) ZZ
      Z = CL
      WRITE (IOUT,1053) Z
      ZZ = FUNC(Z)
      WRITE (IOUT,1061) ZZ
      Z = -ONE
      WRITE (IOUT,1052) Z
      ZZ = FUNC(Z)
      WRITE (IOUT,1061) ZZ
      Z = ZERO
      WRITE (IOUT,1052) Z
      ZZ = FUNC(Z)
      WRITE (IOUT,1061) ZZ
      Z = XP99 * XMAX
      WRITE (IOUT,1052) Z
      ZZ = FUNC(Z)
      WRITE (IOUT,1061) ZZ
      WRITE (IOUT,1100)
      return
c
 1000 FORMAT('1Test of LGAMA(X) vs LN(2*SQRT(PI))-2X*LN(2)+',
     1    'LGAMA(2X)-LGAMA(X+1/2)'//)
 1001 FORMAT('1Test of LGAMA(X) vs LN(2*SQRT(PI))-(2X-1)*LN(2)+',
     1    'LGAMA(X-1/2)-LGAMA(2X-1)'//)
 1002 FORMAT('1Test of LGAMA(X) vs -LN(2*SQRT(PI))+X*LN(2)+',
     1    'LGAMA(X/2)+LGAMA(X/2+1/2)'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval (',
     1    F5.1,',',F5.1,')'//)
 1011 FORMAT('  LGAMA(X) was larger',I6,' times,'/
     1    14X,' agreed',I6,' times, and'/
     2    10X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1025 FORMAT('1Special Tests'//)
 1040 FORMAT(//' Test of special arguments'//)
 1041 FORMAT(' LGAMA (',E13.6,') = ',E13.6//)
 1050 FORMAT('1Test of Error Returns'///)
 1052 FORMAT(' LGAMA will be called with the argument',E13.6,/
     1    ' This should trigger an error message'//)
 1053 FORMAT(' LGAMA will be called with the argument',E13.6,/
     1    ' This should not trigger an error message'//)
 1061 FORMAT(' LGAMA returned the value',E13.6///)
 1100 FORMAT(' This concludes the tests')
      END
      subroutine eitest ( )

c*********************************************************************72
c
cc EITEST tests EI, EONE and EXPEI.
C
C  Method:
C
C     Accuracy test compare function values against local Taylor's
C     series expansions.  Derivatives for Ei(x) are generated from
C     the recurrence relation using a technique due to Gautschi
C     (see references).  Special argument tests are run with the
C     related functions E1(x) and exp(-x)Ei(x).
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following five
C              parameters are assigned the values indicated
C
C              IBETA  - The radix of the floating-point system
C              IT     - The number of base-ibeta digits in the
C                       significant of a floating-point number
C              XMAX   - The largest finite floating-point number
C
C     REN(K) - A function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, AINT, DBLE, LOG, MAX, REAL, SQRT
C
C  References: "The use of Taylor series to test accuracy of
C               function programs", Cody, W. J., and Stoltz, L.,
C               submitted for publication.
C
C              "Recursive computation of certain derivatives -
C               A study of error propagation", Gautschi, W., and
C               Klein, B. J., Comm. ACM 13 (1970), 7-9.
C
C              "Remark on Algorithm 282", Gautschi, W., and Klein,
C               B. J., Comm. ACM 13 (1970), 53-54.
C
C  Latest modification: March 9, 1992
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division 
C          Argonne National Laboratory
C          Argonne, IL 60439
C
      implicit none

      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NEGEP,NGRD,N1
      DOUBLE PRECISION
     1    A,AIT,ALBETA,B,BETA,CONV,C1,D,DEL,DX,EN,EI,EONE,EPS,EPSNEG,
     2    EXPEI,FIV12,FOUR,FOURTH,ONE,P0625,REM,REN,R6,R7,SIX,SUM,
     3    TEN,TWO,U,V,W,X,XBIG,XC,XDEN,XL,XLGE,XMAX,XMIN,XN,XNP1,
     4    XNUM,X0,X99,Y,Z,ZERO
      DIMENSION D(0:25)
c
      DATA ZERO,FOURTH,ONE,FOUR,SIX/0.0D0,0.25D0,1.0D0,4.0D0,6.0D0/,
     1     TEN,X0,X99,P0625/10.0D0,0.3725D0,-999.0D0,0.0625D0/,
     2     FIV12,REM/512.0D0,-7.424779065800051695596D-5/
      DATA IOUT/6/
c
C  Statement functions for conversion 
c
      CONV(I) = DBLE(I)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      DX = -P0625
      A = FOURTH + DX
      B = X0 + DX
      N = 2000
      N1 = 25
      XN = CONV(N)
      JT = 0
c
C  Random argument accuracy tests
c
      DO 300 J = 1, 8
         K1 = 0
         K3 = 0
         XC = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            Y = DEL * REN(JT) + XL
            X = Y - DX
            Y = X + DX
c
C  Test Ei against series expansion
c
            V = EI(X)
            Z = EI(Y)
            SUM = ZERO
            U = X
            CALL DSUBN(U,N1,XMAX,D)
            EN = CONV(N1)+ONE
            SUM = D(N1)*DX/EN
            DO 100 K = N1,1,-1
               EN = EN-ONE
               SUM = (SUM + D(K-1))*DX/EN
  100       CONTINUE
            U = V + SUM
c
C  Accumulate results
c
            W = Z - U
            W = W / Z
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  XC = Y
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C  Gather and print statistics for test
c
         K2 = N - K3 - K1
         R7 = SQRT(R7/XN)
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1
         WRITE (IOUT,1015) K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,XC
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C  Initialize for next test
c
         IF (J .EQ. 1) THEN
               DX = -DX
               A = X0 + DX
               B = SIX
            ELSE IF (J .LE. 4) THEN
               A = B
               B = B+B
            ELSE IF (J .EQ. 5) THEN
               A = -FOURTH
               B = -ONE
            ELSE IF (J .EQ. 6) THEN
               A = B
               B = -FOUR
            ELSE 
               A = B
               B = -TEN
         END IF
  300 CONTINUE
c
C  Special tests.  First, check accuracy near the zero of Ei(x)
c
      WRITE (IOUT,1040)
      X = (FOUR - ONE) / (FOUR + FOUR)
      Y = EI(X)
      WRITE (IOUT,1041) X,Y
      Z = ((Y - (FOUR+ONE)/(FIV12)) - REM)/Y
      IF (Z .NE. ZERO) THEN
            W = LOG(ABS(Z))/ALBETA
         ELSE
            W = X99
      END IF
      WRITE (IOUT,1042) Z,IBETA,W
      W = MAX(AIT+W,ZERO)
      WRITE (IOUT,1022) IBETA,W
c
C  Check near XBIG, the largest argument acceptable to EONE, i.e.,
C    the negative of the smallest argument acceptable to EI.
C    Determine XBIG with Newton iteration on the equation
C                  EONE(x) = XMIN.
c-
      WRITE (IOUT,1050)
      TWO = ONE+ONE
      V = SQRT(EPS)
      C1 = CONV(MINEXP) * LOG(BETA)
      XN = -C1
  320 XNUM = -XN - LOG(XN) + LOG(ONE+ONE/XN) - C1
      XDEN = -(XN*XN+XN+XN+TWO) / (XN*(XN+ONE))
      XNP1 = XN - XNUM/XDEN
      W = (XN-XNP1)/XNP1
      IF (ABS(W) .GT. V) THEN
         XN = XNP1
         GO TO 320
      END IF
      XBIG = XNP1
      X = AINT(TEN*XBIG) / TEN
      WRITE (IOUT,1052) X
      Y = EONE(X)
      WRITE (IOUT,1062) Y
      X = XBIG * (ONE+V)
      WRITE (IOUT,1053) X
      Y = EONE(X)
      WRITE (IOUT,1062) Y
c-
C  Check near XMAX, the largest argument acceptable to EI.  Determine
C    XLGE with Newton iteration on the equation
C                  EI(x) = XMAX.
c-
      C1 = CONV(MAXEXP) * LOG(BETA)
      XN = C1
  330 XNUM = XN - LOG(XN) + LOG(ONE+ONE/XN) - C1
      XDEN = (XN*XN-TWO) / (XN*(XN+ONE))
      XNP1 = XN - XNUM/XDEN
      W = (XN-XNP1)/XNP1
      IF (ABS(W) .GT. V) THEN
         XN = XNP1
         GO TO 330
      END IF
      XLGE = XNP1
      X = AINT(TEN*XLGE) / TEN
      WRITE (IOUT,1054) X
      Y = EI(X)
      WRITE (IOUT,1064) Y
      X = XLGE * (ONE+V)
      WRITE (IOUT,1055) X
      Y = EI(X)
      WRITE (IOUT,1064) Y
c-
C  Check with XHUGE, the largest acceptable argument for EXPEI
c-
      IF (XMIN*XMAX .LE. ONE) THEN
            X = XMAX
         ELSE
            X = ONE/XMIN 
      END IF
      WRITE (IOUT,1056) X
      Y = EXPEI(X)
      WRITE (IOUT,1065) Y
      X = ZERO
      WRITE (IOUT,1055) X
      Y = EI(X)
      WRITE (IOUT,1064) Y
      WRITE (IOUT,1100)
      return
c
 1000 FORMAT('1Test of Ei(x) vs series expansion'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval (',
     1    F7.3,',',F7.3,')'//)
 1011 FORMAT('     EI(X) was larger',I6,' times,')
 1015 FORMAT(14X,' agreed',I6,' times, and'/
     1    10X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1040 FORMAT(//' Test of special arguments'//)
 1041 FORMAT('   EI (',E13.6,') = ',E13.6//)
 1042 FORMAT(' The relative error is',E15.4,' = ',I4,' **',F7.2/)
 1050 FORMAT(' Test of Error Returns'///)
 1052 FORMAT(' EONE will be called with the argument',E13.6,/
     1    ' This should not underflow'//)
 1053 FORMAT(' EONE will be called with the argument',E13.6,/
     1    ' This should underflow'//)
 1054 FORMAT(' EI will be called with the argument',E13.6,/
     1    ' This should not overflow'//)
 1055 FORMAT(' EI will be called with the argument',E13.6,/
     1    ' This should overflow'//)
 1056 FORMAT(' EXPEI will be called with the argument',E13.6,/
     1    ' This should not underflow'//)
 1062 FORMAT(' EONE returned the value',E13.6///)
 1064 FORMAT(' EI returned the value',E13.6///)
 1065 FORMAT(' EXPEI returned the value',E13.6///)
 1100 FORMAT(' This concludes the tests')
      END
      subroutine erftst ( )

c*********************************************************************72
c
cc ERFTST tests ERF and related functions.
C
C  Method:
C
C     Accuracy test compare function values against local Taylor's
C     series expansions.  Derivatives for erfc(x) are expressed as
C     repeated integrals of erfc(x).  These are generated from the
C     recurrence relation using a technique due to Gautschi (see
C     references).
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following five
C              parameters are assigned the values indicated
C
C              IBETA  - The radix of the floating-point system
C              IT     - The number of base-ibeta digits in the
C                       significant of a floating-point number
C              EPS    - The smallest positive floating-point
C                       number such that 1.0+EPS .NE. 1.0
C              XMIN   - The smallest non-vanishing floating-point
C                       integral power of the radix
C              XMAX   - The largest finite floating-point number
C
C     REN(K) - A function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  References: "Performance evaluation of programs for the error
C               and complementary error functions", W. J. Cody,
C               submitted for publication.
C
C              "Evaluation of the repeated integrals of the coerror
C               function", W. Gautschi, TOMS 3, 1977, pp. 240-252.
C
C  Latest modification: March 12, 1992
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division 
C          Argonne National Laboratory
C          Argonne, IL 60439
C
      implicit none

      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NEGEP,NGRD,N0,N1
      DOUBLE PRECISION DERF,DERFC,DERFCX,
     1    A,AIT,ALBETA,B,BETA,C,CONV,C1,C2,DEL,EPS,EPSCON,EPSNEG,FF,
     2    FUNC1,FUNC2,FUNC3,F0,HALF,ONE,R,REN,R1,R6,R7,SC,SIXTEN,
     3    THRESH,TEN,TWO,U,V,W,X,XBIG,XC,XL,XMAX,XMIN,XN,XN1,X99,
     4    Z,ZERO,ZZ
      DIMENSION R1(500)
c
C   C1 = 1/sqrt(pi)
c
      DATA ZERO,HALF,ONE,TWO,TEN/0.0D0,0.5D0,1.0D0,2.0D0,10.0D0/,
     1     SIXTEN,THRESH,X99/16.0D0,0.46875D0,-999.0D0/,
     2     C1/5.6418958354775628695D-1/
      DATA IOUT/6/
c
C  Statement functions for conversion 
c
      CONV(I) = DBLE(I)
      FUNC1(X) = DERF(X)
      FUNC2(X) = DERFC(X) 
      FUNC3(X) = DERFCX(X) 
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      C = ABS(AIT*ALBETA) + TEN
      A = ZERO
      B = THRESH
      N = 2000
      XN = CONV(N)
      JT = 0
      N0 = (IBETA/2)*(IT+5)/6+4
c
C  Determine largest argument for ERFC test by Newton iteration
c
      C2 = LOG(XMIN) + LOG(ONE/C1)
      XBIG = SQRT(-C2)
   50 X = XBIG
         F0 = X*X
         FF = F0 + HALF/F0 + LOG(X) + C2
         F0 = X+X + ONE/X - ONE/(X*F0)
         XBIG = X - FF/F0
         IF (ABS(X-XBIG)/X .GT. TEN*EPS) GO TO 50
c
C  Random argument accuracy tests
c
      DO 300 J = 1, 5
         K1 = 0
         K3 = 0
         XC = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
            IF (J .EQ. 1) THEN
c
C  Test erf against double series expansion
c
                  F0 = CONV(N0)
                  FF = F0+F0+ONE 
                  Z = X*X
                  W = Z+Z
                  U = ZERO
                  V = ZERO
                  DO 60 K = 1, N0
                     U = -Z/F0*(ONE+U)
                     V = W/FF*(ONE+V)
                     F0 = F0 - ONE
                     FF = FF - TWO
   60             CONTINUE
                  V = C1*(X+X)*(((U*V+(U+V))+HALF)+HALF)
                  U = FUNC1(X)
               ELSE
c
C  Test erfc or scaled erfc against expansion in repeated
C   integrals of the coerror function.
c
                  Z = X + HALF
                  X = Z - HALF
                  R = ZERO
                  IF (X .LE. ONE) THEN
                        N0 = 499
                     ELSE
                        N0 = MIN(499,INT(C/(ABS(LOG(Z)))))
                  END IF
                  N1 = N0 
                  XN1 = CONV(N1+1)
                  DO 100 K=1,N0
                     R = HALF/(Z+XN1*R)
                     R1(N1) = R
                     N1 = N1 - 1
                     XN1 = XN1 - ONE
  100             CONTINUE
                  FF = C1/(Z+R1(1))
                  IF ((J/2)*2 .EQ. J) THEN
                        F0 = FUNC2(Z)*EXP(X+HALF*HALF)
                        U = FUNC2(X)
                     ELSE
                        F0 = FUNC3(Z) 
                        U = FUNC3(X)
                  END IF 
                  SC = F0/FF
c
C  Scale things to avoid premature underflow
c
                  EPSCON = F0 
                  FF = SIXTEN*FF/EPS
                  DO 110 N1=1,N0
                     FF = R1(N1)*FF
                     R1(N1) = FF * SC
                     IF (R1(N1) .LT. EPSCON ) THEN
                        K = N1
                        GO TO 111
                     END IF
  110             CONTINUE
  111             V = R1(K)
                  DO 120 N1 = 1, K-1
                     V = V + R1(K-N1)
  120             CONTINUE
c
C  Remove scaling here
c
                  V = V*EPS/SIXTEN + F0
            END IF
c
C  Accumulate results
c
            W = (U - V) / U
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  XC = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C  Gather and print statistics for test
c
         K2 = N - K3 - K1
         R7 = SQRT(R7/XN)
         IF (J .EQ. 1) THEN
               WRITE (IOUT,1000)
               WRITE (IOUT,1010) N,A,B
               WRITE (IOUT,1011) K1
            ELSE IF ((J/2)*2 .EQ. J) THEN
               WRITE (IOUT,1001)
               WRITE (IOUT,1010) N,A,B
               WRITE (IOUT,1012) K1
            ELSE
               WRITE (IOUT,1002)
               WRITE (IOUT,1010) N,A,B
               WRITE (IOUT,1013) K1
         END IF
         WRITE (IOUT,1015) K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,XC
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C  Initialize for next test
c
         IF (J .EQ. 1) THEN
               A = B
               B = TWO
            ELSE IF (J .EQ. 3) THEN
               A = B
               B = AINT(XBIG*SIXTEN)/SIXTEN-HALF
            ELSE IF (J .EQ. 4) THEN
               B = TEN + TEN
         END IF
  300 CONTINUE
c
C  Special tests
C  First check values for negative arguments.
c
      WRITE (IOUT,1025)
      WRITE (IOUT,1030) IBETA
      X = ZERO
      DEL = -HALF
      DO 350 I = 1, 10
         U = FUNC1(X)
         A = U + FUNC1(-X)
         IF (A*U .NE. ZERO) A = AIT + LOG(ABS(A/U))/ALBETA
         V = FUNC2(X)
         B = U + V - ONE
         IF (B .NE. ZERO) B = AIT + LOG(ABS(B))/ALBETA
         W = FUNC3(X)
         C = AINT(X*SIXTEN)/SIXTEN
         R = (X-C)*(X+C)
         C = (EXP(C*C)*EXP(R)*V-W)/W
         IF (C .NE. ZERO) C = MAX(ZERO,AIT + LOG(ABS(C))/ALBETA)
         WRITE (IOUT,1031) X,A,B,C
         X = X + DEL
  350 CONTINUE
c
C  Next, test with special arguments
c
      WRITE (IOUT,1040)
      Z = XMAX
      ZZ = FUNC1(Z)
      WRITE (IOUT,1041) Z,ZZ
      Z = ZERO
      ZZ = FUNC1(Z)
      WRITE (IOUT,1041) Z,ZZ
      ZZ = FUNC2(Z)
      WRITE (IOUT,1042) Z,ZZ
      Z = -XMAX
      ZZ = FUNC2(Z)
      WRITE (IOUT,1042) Z,ZZ
c
C  Test of error returns
c
      WRITE (IOUT,1050)
      W = XBIG
      Z = W*(ONE-HALF*HALF)
      WRITE (IOUT,1052) Z
      ZZ = FUNC2(Z)
      WRITE (IOUT,1062) ZZ
      Z = W*(ONE+TEN*EPS)
      WRITE (IOUT,1053) Z
      ZZ = FUNC2(Z)
      WRITE (IOUT,1062) ZZ
      W = XMAX
      IF (C1 .LT. XMAX*XMIN) W = C1/XMIN
      Z = W*(ONE-ONE/SIXTEN)
      WRITE (IOUT,1054) Z
      ZZ = FUNC3(Z)
      WRITE (IOUT,1064) ZZ
      W = -SQRT(LOG(XMAX/TWO))
      Z = W*(ONE-ONE/TEN)
      WRITE (IOUT,1055) Z
      ZZ = FUNC3(Z)
      WRITE (IOUT,1064) ZZ
      Z = W*(ONE+TEN*EPS)
      WRITE (IOUT,1056) Z
      ZZ = FUNC3(Z)
      WRITE (IOUT,1064) ZZ
      WRITE (IOUT,1100)
      return
c
 1000 FORMAT('1Test of erf(x) vs double series expansion'//)
 1001 FORMAT(///' Test of erfc(x) vs exp(x+1/4) SUM i^n erfc(x+1/2)'//)
 1002 FORMAT('1Test of exp(x*x) erfc(x) vs SUM i^n erfc(x+1/2)'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval (',
     1    F7.3,',',F7.3,')'//)
 1011 FORMAT('    ERF(X) was larger',I6,' times,')
 1012 FORMAT('   ERFC(X) was larger',I6,' times,')
 1013 FORMAT('  ERFCX(X) was larger',I6,' times,')
 1015 FORMAT(14X,' agreed',I6,' times, and'/
     1    10X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1025 FORMAT('1Special Tests'//)
 1030 FORMAT(7X,'Estimated loss of base',i3,'significant digits in'//
     1       3X'X',5X,'Erf(x)+Erf(-x)',3X,'Erf(x)+Erfc(x)-1',
     1       3X,'Erfcx(x)-exp(x*x)*erfc(x)'/)
 1031 FORMAT(F7.3,3F16.2)
 1040 FORMAT(//' Test of special arguments'//)
 1041 FORMAT('   ERF (',E13.6,') = ',E13.6//)
 1042 FORMAT('  ERFC (',E13.6,') = ',E13.6//)
 1050 FORMAT(' Test of Error Returns'///)
 1052 FORMAT(' ERFC will be called with the argument',E13.6,/
     1    ' This should not underflow'//)
 1053 FORMAT(' ERFC will be called with the argument',E13.6,/
     1    ' This may underflow'//)
 1054 FORMAT(' ERFCX will be called with the argument',E13.6,/
     1    ' This should not underflow'//)
 1055 FORMAT(' ERFCX will be called with the argument',E13.6,/
     1    ' This should not overflow'//)
 1056 FORMAT(' ERFCX will be called with the argument',E13.6,/
     1    ' This may overflow'//)
 1062 FORMAT(' ERFC returned the value',E13.6///)
 1064 FORMAT(' ERFCX returned the value',E13.6///)
 1100 FORMAT(' This concludes the tests')
      END
      subroutine gamtst ( )

c*********************************************************************72
c
cc GAMTST tests DGAMMA.
C
C   Method:
C
C      Accuracy tests compare function values against values
C      generated with the duplication formula.
C
C   Data required
C
C      None
C
C   Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following five
C              parameters are assigned the values indicated
C
C              IBETA  - The radix of the floating-point system
C              IT     - The number of base-ibeta digits in the 
C                       significant of a floating-point number
C              EPS    - The smallest positive floating-point
C                       number such that 1.0+EPS .NE. 1.0
C              XMIN   - The smallest non-vanishing floating-point
C                       integral power of the radix
C              XMAX   - The largest finite floating-point number
C
C     REN(K) - A function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, DBLE, INT, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs related
C              to the real gamma function", W. J. Cody, 
C              submitted for publication.
C
C  Latest modification: March 12, 1992
C
C  Authors: W. J. Cody and L. Stoltz
C           Mathematics and Computer Science Division 
C           Argonne National Laboratory
C           Argonne, IL 60439
C
      implicit none

      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NEGEP,NGRD,NX
      DOUBLE PRECISION  DGAMMA,
     1    A,AIT,ALBETA,ALNX,B,BETA,C,CL,CONV,C1,C2,DEL,EPS,
     2    EPSNEG,FUNC,HALF,ONE,REN,R6,R7,TEN,TWO,W,X,XC,XL, 
     3    XMAX,XMIN,XMINV,XN,XNUM,XXN,XP,XPH,X99,XP99,Y,Z,ZERO,ZZ 
      DATA C1,C2/2.8209479177387814347D-1,9.1893853320467274178D-1/,
     1     ZERO,HALF,ONE,TWO,TEN/0.0D0,0.5D0,1.0D0,2.0D0,10.0D0/,
     2     X99,XP99/-999.0D0,0.99D0/
      DATA IOUT/6/
c
C  Statement functions for conversion
c
      CONV(I) = DBLE(I)
      FUNC(X) = DGAMMA(X)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      A = ZERO
      B = TWO
      N = 2000
      XN = CONV(N)
      JT = 0
c
C  Determine smallest argument for GAMMA
c
      IF (XMIN*XMAX .LT. ONE) THEN
            XMINV = ONE/XMAX
         ELSE
            XMINV = XMIN
      END IF
c
C  Determine largest argument for GAMMA by Newton iteration
c
      CL = LOG(XMAX)
      XP = HALF * CL
      CL = C2 - CL
   50 X = XP
         ALNX = LOG(X)
         XNUM = (X-HALF)*ALNX - X + CL
         XP = X - XNUM/(ALNX-HALF/X)
         IF (ABS(XP-X)/X .GE. TEN*EPS) GO TO 50
      CL = XP
c
C  Random argument accuracy tests
c
      DO 300 J = 1, 4
         K1 = 0
         K3 = 0
         XC = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
c
C  Use duplication formula for X not close to the zero
c
            XPH = X * HALF + HALF
            XP = XPH - HALF
            X = XP + XP
            NX = INT(X)
            XXN = CONV(NX)
            C = (TWO ** NX) * (TWO ** (X-XXN))
            Z = FUNC(X)
            ZZ = ((C*C1)*FUNC(XP)) * FUNC(XPH)
c
C  Accumulate results
c
            W = (Z - ZZ) / Z
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  XC = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C  Gather and print statistics for test
c
         K2 = N - K3 - K1
         R7 = SQRT(R7/XN)
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,XC
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C  Initialize for next test
c
         A = B
         IF (J .EQ. 1) THEN
               B = TEN
            ELSE IF (J .EQ. 2) THEN
               B = CL - HALF
            ELSE
               A = -(TEN-HALF) * HALF
               B = A + HALF
         END IF
  300 CONTINUE
c
C  Special tests
C  First test with special arguments
c
      WRITE (IOUT,1025)
      WRITE (IOUT,1040)
      X = -HALF
      Y = FUNC(X)
      WRITE (IOUT,1041) X, Y
      X = XMINV / XP99
      Y = FUNC(X)
      WRITE (IOUT,1041) X, Y
      X = ONE
      Y = FUNC(X)
      WRITE (IOUT,1041) X, Y
      X = TWO
      Y = FUNC(X)
      WRITE (IOUT,1041) X, Y
      X = CL * XP99
      Y = FUNC(X)
      WRITE (IOUT,1041) X, Y
c
C  Test of error returns
c
      WRITE (IOUT,1050)
      X = -ONE
      WRITE (IOUT,1052) X
      Y = FUNC(X)
      WRITE (IOUT,1061) Y
      X = ZERO
      WRITE (IOUT,1052) X
      Y = FUNC(X)
      WRITE (IOUT,1061) Y
      X = XMINV * (ONE - EPS)
      WRITE (IOUT,1052) X
      Y = FUNC(X)
      WRITE (IOUT,1061) Y
      X = CL * (ONE + EPS)
      WRITE (IOUT,1052) X
      Y = FUNC(X)
      WRITE (IOUT,1061) Y
      WRITE (IOUT,1100)
      return
c
 1000 FORMAT('1Test of GAMMA(X) vs Duplication Formula'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval (',
     1    F7.3,',',F7.3,')'//)
 1011 FORMAT(' GAMMA(X) was larger',I6,' times,'/
     1    13X,' agreed',I6,' times, and'/
     2    9X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1025 FORMAT('1Special Tests'//)
 1040 FORMAT(//' Test of special arguments'//)
 1041 FORMAT(' GAMMA (',E13.6,') = ',E13.6//)
 1050 FORMAT('1Test of Error Returns'///)
 1052 FORMAT(' GAMMA will be called with the argument',E13.6,/
     1    ' This should trigger an error message'//)
 1061 FORMAT(' GAMMA returned the value',E13.6///)
 1100 FORMAT(' This concludes the tests')
      END
      subroutine i0test ( )

c*********************************************************************72
C
cc I0TEST tests BESI0 and BESEI0.
C
C  Method:
C
C     Accuracy tests compare function values against values
C     generated with the multiplication formula for small
C     arguments and values generated from a Taylor's Series
C     Expansion using Amos' Ratio Scheme for initial values
C     for large arguments.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C         information on the floating-point arithmetic
C         system.  Note that the call to MACHAR can
C         be deleted provided the following five
C         parameters are assigned the values indicated
C
C              IBETA  - the radix of the floating-point system
C              IT     - the number of base-IBETA digits in the
C                       significant of a floating-point number
C              EPS    - the smallest positive floating-point
C                       number such that 1.0+EPS .NE. 1.0
C              XMIN   - the smallest non-vanishing normalized
C                       floating-point power of the radix
C              XMAX   - the largest finite floating-point number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, DBLE, INT, LOG, MAX, REAL, SQRT
C
C  Reference: "Computation of Modified Bessel Functions and
C              Their Ratios," D. E. Amos, Math. of Comp., 
C              Volume 28, Number 24, January, 1974.
C
C             "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C             "Use of Taylor series to test accuracy of function
C              programs," W. J. Cody and L. Stoltz, submitted
C              for publication.
C
C  Latest modification: March 12, 1992
C
C  Authors:  W. J. Cody and L. Stoltz
C            Mathematics and Computer Science Division 
C            Argonne National Laboratory
C            Argonne, IL 60439
C
      implicit none

      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,J1,J2,K1,K2,K3,
     1    MACHEP,MAXEXP,MB,MBORG,MINEXP,MB2,N,NEGEP,NGRD
      DOUBLE PRECISION  
     1    A,AIT,AK,AKK,ALBETA,ARR,ATETEN,B,BESEI0,BESI0,BETA,BOT,C,
     2    CONST,CONV,D,DEL,DELTA,E,EIGHT,EPS,EPSNEG,F,HALF,HUND,ONE,
     3    OVRCHK,ONE28,REN,R6,R7,SIXTEN,SUM,TEMP,TOP,TWO,T1,T2,U,U2,
     4    W,X,XA,XB,XBAD,XJ1,XL,XLAM,XLARGE,XMAX,XMB,XMIN,XN,XNINE,
     5    X1,X99,Y,Z,ZERO,ZZ
      DIMENSION ARR(8,6),U(560),U2(560) 
      DATA ZERO,HALF,ONE,TWO,EIGHT/0.0D0,0.5D0,1.0D0,2.0D0,8.0D0/, 
     1   XNINE,SIXTEN,ATETEN,HUND/9.0D0,1.6D1,1.8D1,1.0D2/,
     2   ONE28,X99,XLAM,XLARGE/1.28D2,-999.0D0,1.03125D0,1.0D4/,
     3   C/0.9189385332D0/
      DATA  ARR/0.0D0,1.0D0,-1.0D0,1.0D0,-2.0D0,1.0D0,-3.0D0,1.0D0,
     1          -999.0D0,-999.0D0,-999.0D0,3.0D0,-12.0D0,9.0D0,-51.0D0,
     2          18.0D0,-5040.0D0,720.0D0,0.0D0,-999.0D0,-999.0D0,
     3          60.0D0,-360.0D0,345.0D0,-1320.0D0,192.0D0,-120.0D0,
     4          24.0D0,0.0D0,-999.0D0,-999.0D0,2520.0D0,-96.0D0,15.0D0,
     5          -33.0D0,7.0D0,-6.0D0,2.0D0,0.0D0,-999.0D0,-4.0D0,1.0D0,
     6          -3.0D0,1.0D0,-2.0D0,1.0D0,-1.0D0,1.0D0/
      DATA IOUT/6/
c
C  Define statement functions for conversions
c
      CONV(N) = DBLE(N)
      TOP(X) = X - HALF*LOG(X) + LOG(ONE+(ONE/EIGHT-XNINE/
     1   ONE28/X)/X)
      BOT(X) = -(SIXTEN*X+ATETEN) / (((ONE28*X+SIXTEN)*X+
     1   XNINE)*X) + ONE - HALF/X
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      AIT = CONV(IT)
      ALBETA = LOG(BETA)
      A = ZERO
      B = TWO
      JT = 0
      CONST = C + LOG(XMAX)
      DELTA = XLAM - ONE
      F = (XLAM-ONE) * (XLAM+ONE) * HALF
c
C  Random argument accuracy tests
c
      DO 300 J = 1, 4
c-
C  Calculate the number of terms needed for convergence of the 
C  series by using Newton's iteration on the asymptotic form of 
C  the multiplication theorem
c-
         XBAD = B
         D = AIT * ALBETA - C + ONE
         E = LOG(XBAD * F) + ONE
         AKK = ONE
  100    AK = AKK
            Z = D + E*AK - (AK+HALF) * LOG(AK+ONE)
            ZZ = E - (AK+HALF)/(AK+ONE) - LOG(AK+ONE)
            AKK = AK - Z/ZZ
         IF (ABS(AK-AKK) .GT. HUND*EPS*AK) GO TO 100
         MBORG = INT(AKK) + 1
         N = 2000 
         XN = CONV(N)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
c
C   Carefully purify arguments
c
            IF (J .EQ. 1) THEN
                  Y = X/XLAM
               ELSE
                  Y = X - DELTA
            END IF
            TEMP = SIXTEN * Y
            T1 = TEMP + Y
            T1 = TEMP + T1
            Y = T1 - TEMP
            Y = Y - TEMP
            IF (J .EQ. 1) THEN
                  X = Y * XLAM
               ELSE
                  X = Y + DELTA
            END IF
c
C   Use Amos' Ratio Scheme
c
            D = F*Y
            MB = MBORG + MBORG
            MB2 = MB - 1
            XMB = CONV(MB2)
            TEMP = (XMB + ONE + HALF) * (XMB + ONE + HALF)
            U2(MB) = Y / (XMB + HALF + SQRT(TEMP + Y*Y))
c
C   Generate ratios using recurrence
c
            DO 110 II = 2, MB
               OVRCHK = XMB/(Y*HALF)
               U2(MB2) = ONE / (OVRCHK + U2(MB2+1))
               XMB = XMB - ONE
               MB2 = MB2 - 1
  110       CONTINUE
            U(1) = BESI0(Y)
            IF (J .EQ. 1) THEN
c
C   Accuracy test is based on the multiplication theorem
c
                  MB = MB - MBORG
                  DO 120 II = 2, MB
                     U(II) = U(II-1) * U2(II-1)
  120             CONTINUE
c
C   Accurate Summation
c
                  MB = MB - 1
                  XMB = CONV(MB)
                  SUM = U(MB+1)
                  IND = MB
                  DO 155 II = 2, MB
                     SUM = SUM * D / XMB + U(IND)
                     IND = IND - 1
                     XMB = XMB - ONE
  155             CONTINUE
                  ZZ = SUM * D + U(IND)
               ELSE
c
C   Accuracy test is based on Taylor's Series Expansion
c
                  U(2) = U(1) * U2(1)
                  MB = 8 
                  J1 = MB
                  XJ1 = CONV(J1+1)
                  IEXP = 0
c
C   Accurate Summation
c
                  DO 180 II = 1, MB
                     J2 = 1
  160                J2 = J2 + 1
                     IF (ARR(J1,J2) .NE. X99) GO TO 160
                     J2 = J2 - 1
                     T1 = ARR(J1,J2)
                     J2 = J2 - 1
c
C   Group I0 terms in the derivative
c
                     IF (J2 .EQ. 0) GO TO 168 
  165                T1 = T1 / (Y*Y) + ARR(J1,J2)
                           J2 = J2 - 1
                     IF (J2 .GE. 1) GO TO 165
  168                IF (IEXP .EQ. 1) T1 = T1 / Y
                     J2 = 6 
  170                J2 = J2 - 1
                     IF (ARR(II,J2) .NE. X99) GO TO 170
                     J2 = J2 + 1
                     T2 = ARR(II,J2)
                     J2 = J2 + 1
                     IF (IEXP .EQ. 0) THEN 
                           IEXP = 1
                        ELSE
                           IEXP = 0
                     END IF
c
C   Group I1 terms in the derivative
c
                     IF (J2 .EQ. 7) GO TO 177 
  175                T2 = T2 / (Y*Y) + ARR(II,J2)
                           J2 = J2 + 1
                     IF (J2 .LE. 6) GO TO 175
  177                IF (IEXP .EQ. 1) T2 = T2 / Y
                     IF (J1 .EQ. 8) THEN
                           SUM = U(1)*T1 + U(2)*T2
                        ELSE
                           SUM = SUM * (DELTA/XJ1) + (U(1)*T1 + U(2)*T2)
                     END IF
                     J1 = J1 - 1
                     XJ1 = CONV(J1+1)
  180             CONTINUE
                  ZZ = SUM * DELTA + U(1)
            END IF
            Z = BESI0(X)
c
C   Accumulate Results
c
            W = (Z - ZZ) / Z
            IF (W .GT. ZERO) THEN 
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN 
                  K3 = K3 + 1
               ELSE
                  K2 = K2 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C   Gather and print statistics for test
c
         N = K1 + K2 + K3
         R7 = SQRT(R7/XN)
         IF (J .EQ. 1) THEN
               WRITE (IOUT,1000)
            ELSE
               WRITE (IOUT,1001)
         END IF
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(R6)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,X1
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(R7)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C   Initialize for next test
c
         A = B
         B = B + B
         IF (J .EQ. 1) B = B + B - HALF
  300 CONTINUE
c
C   Test of error returns
C
C   Special tests
c
      WRITE (IOUT,1030)
      WRITE (IOUT,1031)
      Y = BESI0(XMIN)
      WRITE (IOUT,1032) Y
      Y = BESI0(ZERO)
      WRITE (IOUT,1033) 0,Y
      X = -ONE * REN(JT)
      Y = BESI0(X)
      WRITE (IOUT,1034) X,Y
      X = -X 
      Y = BESI0(X)
      WRITE (IOUT,1034) X,Y
      Y = BESEI0(XMAX)
      WRITE (IOUT,1035) Y
c
C   Determine largest safe argument for unscaled functions
c
      WRITE (IOUT, 1036)
      XA = LOG(XMAX)
  330 XB = XA - (TOP(XA)-CONST) / BOT(XA)
      IF (ABS(XB-XA)/XB .LE. EPS) THEN 
            GO TO 350
         ELSE
            XA = XB
            GO TO 330
      END IF
  350 XLARGE = XB / XLAM 
      Y = BESI0(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      XLARGE = XB * XLAM
      Y = BESI0(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      WRITE (IOUT, 1037)
      return
c
 1000 FORMAT('1Test of I0(X) vs Multiplication Theorem'//)
 1001 FORMAT('1Test of I0(X) vs Taylor series'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval ',
     1    '(',F5.2,',',F5.2,')'//)
 1011 FORMAT(' I0(X) was larger',I6,' times,'/
     1    10X,' agreed',I6,' times, and'/
     1    6X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Test with extreme arguments'/)
 1032 FORMAT(' I0(XMIN) = ',E24.17/)
 1033 FORMAT(' I0(',I1,') = ',E24.17/)
 1034 FORMAT(' I0(',E24.17,' ) = ',E24.17/)
 1035 FORMAT(' E**-X * I0(XMAX) = ',E24.17/)
 1036 FORMAT(' Tests near the largest argument for unscaled functions'/)
 1037 FORMAT(' This concludes the tests.') 
      END
      subroutine i1test ( )

c*********************************************************************72
c
cc I1TEST tests BESI1 and BESEI1.
C
C  Method:
C
C     Accuracy tests compare function values against values
C     generated with the multiplication formula for small
C     arguments and values generated from a Taylor's Series
C     Expansion using Amos' Ratio Scheme for initial values
C     for large arguments.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C         information on the floating-point arithmetic
C         system.  Note that the call to MACHAR can
C         be deleted provided the following five
C         parameters are assigned the values indicated
C
C              IBETA  - the radix of the floating-point system
C              IT     - the number of base-IBETA digits in the
C                       significant of a floating-point number
C              EPS    - the smallest positive floating-point
C                       number such that 1.0+EPS .NE. 1.0
C              XMIN   - the smallest non-vanishing normalized
C                       floating-point power of the radix
C              XMAX   - the largest finite floating-point number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, DBLE, INT, LOG, MAX, REAL, SQRT
C
C  References: "Computation of Modified Bessel Functions and
C               Their Ratios," D. E. Amos, Math. of Comp., 
C               Volume 28, Number 24, January, 1974.
C
C              "Performance evaluation of programs for certain
C               Bessel functions", W. J. Cody and L. Stoltz,
C               ACM Trans. on Math. Software, Vol. 15, 1989,
C               pp 41-48.
C
C              "Use of Taylor series to test accuracy of function
C               programs," W. J. Cody and L. Stoltz, submitted
C               for publication.
C
C  Latest modification: March 13, 1992
C
C  Authors:  W. J. Cody and L. Stoltz
C            Mathematics and Computer Science Division 
C            Argonne National Laboratory
C            Argonne, IL 60439
C
      implicit none

      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,J1,J2,K1,K2,K3,
     1    MACHEP,MAXEXP,MB,MBORG,MINEXP,MB2,N,NEGEP,NGRD
      DOUBLE PRECISION  
     1    A,AIT,AK,AKK,ALBETA,ARR,B,BESEI1,BESI1,BETA,BOT,C,CONST,CONV,
     2    D,DEL,DELTA,E,EIGHT,EPS,EPSNEG,F,HALF,HUND,ONE,OVRCHK,REN,R6,
     3    R7,SIXTEN,SUM,TEMP,THREE,TOP,T1,T2,U,U2,W,X,XA,XB,XBAD,XJ1,
     4    XL,XLAM,XLARGE,XMAX,XMB,XMIN,XN,X1,X99,Y,Z,ZERO,ZZ
      DIMENSION ARR(8,7),U(560),U2(560) 
      DATA ZERO,HALF,ONE,THREE,EIGHT/0.0D0,0.5D0,1.0D0,3.0D0,8.0D0/, 
     1   SIXTEN,HUND,X99,XLAM/1.6D1,1.0D2,-999.0D0,1.03125D0/,
     2   XLARGE,C/1.0D4,0.9189385332D0/
      DATA  ARR/1.0D0,-1.0D0,1.0D0,-2.0D0,1.0D0,-3.0D0,1.0D0,-4.0D0,
     1          -999.0D0,-999.0D0,3.0D0,-12.0D0,9.0D0,-51.0D0,18.0D0,
     2          -132.0D0,40320.0D0,0.0D0,-999.0D0,-999.0D0,60.0D0,
     3          -360.0D0,345.0D0,-2700.0D0,10440.0D0,-5040.0D0,720.0D0,
     4          0.0D0,-999.0D0,-999.0D0,2520.0D0,-20160.0D0,729.0D0,
     5          -1320.0D0,192.0D0,-120.0D0,24.0D0,0.0D0,-999.0D0,
     6          -999.0D0,26.0D0,-96.0D0,15.0D0,-33.0D0,7.0D0,-6.0D0,
     7          2.0D0,0.0D0,1.0D0,-4.0D0,1.0D0,-3.0D0,1.0D0,-2.0D0,
     8          1.0D0,-1.0D0/
      DATA IOUT/6/
c
C  Define statement functions for conversions
c
      CONV(N) = DBLE(N)
      TOP(X) = X - HALF*LOG(X) + LOG(ONE-THREE/(EIGHT*X))
      BOT(X) = THREE / ((EIGHT*X-THREE)*X) + ONE - HALF/X
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      AIT = CONV(IT)
      ALBETA = LOG(BETA)
      A = ZERO
      B = ONE
      JT = 0
      CONST = C + LOG(XMAX)
      DELTA = XLAM - ONE
      F = (XLAM-ONE) * (XLAM+ONE) * HALF
c
C  Random argument accuracy tests
c
      DO 300 J = 1, 4
c-
C  Calculate the number of terms needed for convergence of the 
C  series by using Newton's iteration on the asymptotic form of 
C  the multiplication theorem
c-
         XBAD = B
         D = AIT * ALBETA - C + ONE
         E = LOG(XBAD * F) + ONE
         AKK = ONE
  100    AK = AKK
            Z = D + E*AK - (AK+HALF) * LOG(AK+ONE)
            ZZ = E - (AK+HALF)/(AK+ONE) - LOG(AK+ONE)
            AKK = AK - Z/ZZ
         IF (ABS(AK-AKK) .GT. HUND*EPS*AK) GO TO 100
         MBORG = INT(AKK) + 1
         N = 2000 
         XN = CONV(N)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
c
C   Carefully purify arguments
c
            IF (J .EQ. 1) THEN
                  Y = X/XLAM
               ELSE
                  Y = X - DELTA
            END IF
            W = SIXTEN * Y
            T1 = W + Y
            T1 = W + T1
            Y = T1 - W
            Y = Y - W
            IF (J .EQ. 1) THEN
                  X = Y * XLAM
               ELSE
                  X = Y + DELTA
            END IF
c
C   Use Amos' Ratio Scheme
c
            D = F*Y
            MB = MBORG + MBORG
            MB2 = MB - 1
            XMB = CONV(MB2)
            TEMP = (XMB + ONE + HALF) * (XMB + ONE + HALF)
            U2(MB) = Y / (XMB + HALF + SQRT(TEMP + Y*Y))
c
C   Generate ratios using recurrence
c
            DO 110 II = 2, MB
               OVRCHK = XMB/(Y*HALF)
               U2(MB2) = ONE / (OVRCHK + U2(MB2+1))
               XMB = XMB - ONE
               MB2 = MB2 - 1
  110       CONTINUE
            U(2) = BESI1(Y)
            U(1) = U(2) / U2(1)
            IF (J .EQ. 1) THEN
c
C   Accuracy test is based on the multiplication theorem
c
                  MB = MB - MBORG
                  DO 120 II = 3, MB
                     U(II) = U(II-1) * U2(II-1)
  120             CONTINUE
c
C   Accurate Summation
c
                  MB = MB - 1
                  XMB = CONV(MB-1)
                  SUM = U(MB+1)
                  IND = MB
                  DO 155 II = 2, MB
                     SUM = SUM * D / XMB + U(IND)
                     IND = IND - 1
                     XMB = XMB - ONE
  155             CONTINUE
                  ZZ = XLAM * SUM 
               ELSE
c
C   Accuracy test is based on Taylor's Series Expansion
c
                  MB = 8 
                  J1 = MB
                  XJ1 = CONV(J1+1)
                  IEXP = 1
c
C   Accurate Summation
c
                  DO 180 II = 1, MB
                     J2 = 1
  160                J2 = J2 + 1
                     IF (ARR(J1,J2) .NE. X99) GO TO 160
                     J2 = J2 - 1
                     T1 = ARR(J1,J2)
                     J2 = J2 - 1
c
C   Group I0 terms in the derivative
c
                     IF (J2 .EQ. 0) GO TO 168 
  165                T1 = T1 / (Y*Y) + ARR(J1,J2)
                           J2 = J2 - 1
                     IF (J2 .GE. 1) GO TO 165
  168                IF (IEXP .EQ. 1) T1 = T1 / Y
                     J2 = 7 
  170                J2 = J2 - 1
                     IF (ARR(II,J2) .NE. X99) GO TO 170
                     J2 = J2 + 1
                     T2 = ARR(II,J2)
                     J2 = J2 + 1
                     IF (IEXP .EQ. 0) THEN
                           IEXP = 1
                        ELSE
                           IEXP = 0
                     END IF
c
C   Group I1 terms in the derivative
c
                     IF (J2 .EQ. 8) GO TO 177 
  175                T2 = T2 / (Y*Y) + ARR(II,J2)
                           J2 = J2 + 1
                     IF (J2 .LE. 7) GO TO 175
  177                IF (IEXP .EQ. 1) T2 = T2 / Y
                     IF (J1 .EQ. 8) THEN
                           SUM = U(1)*T1 + U(2)*T2
                        ELSE
                           SUM = SUM * (DELTA/XJ1) + (U(1)*T1 + U(2)*T2)
                     END IF
                     J1 = J1 - 1
                     XJ1 = CONV(J1+1)
  180             CONTINUE
                  ZZ = SUM * DELTA + U(2)
            END IF
            Z = BESI1(X)
c
C   Accumulate Results
c
            W = (Z - ZZ) / Z
            IF (W .GT. ZERO) THEN 
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN 
                  K3 = K3 + 1
               ELSE
                  K2 = K2 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C   Gather and print statistics for test
c
         N = K1 + K2 + K3
         R7 = SQRT(R7/XN)
         IF (J .EQ. 1) THEN
               WRITE (IOUT,1000)
            ELSE
               WRITE (IOUT,1001)
         END IF
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(R6)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,X1
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(R7)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C   Initialize for next test
c
         A = B
         B = B + B
         IF (J .EQ. 1) B = B + B + THREE + HALF
  300 CONTINUE
c
C   Test of error returns
C
C   Special tests
c
      WRITE (IOUT,1030)
      WRITE (IOUT,1031)
      Y = BESI1(XMIN)
      WRITE (IOUT,1032) Y
      Y = BESI1(ZERO)
      WRITE (IOUT,1033) 0,Y
      X = -ONE * REN(JT)
      Y = BESI1(X)
      WRITE (IOUT,1034) X,Y
      X = -X 
      Y = BESI1(X)
      WRITE (IOUT,1034) X,Y
      Y = BESEI1(XMAX)
      WRITE (IOUT,1035) Y
c
C   Determine largest safe argument for unscaled functions
c
      WRITE (IOUT, 1036)
      XA = LOG(XMAX)
  330 XB = XA - (TOP(XA)-CONST) / BOT(XA)
      IF (ABS(XB-XA)/XB .LE. EPS) THEN 
            GO TO 350
         ELSE
            XA = XB
            GO TO 330
      END IF
  350 XLARGE = XB / XLAM 
      Y = BESI1(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      XLARGE = XB * XLAM
      Y = BESI1(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      WRITE (IOUT, 1037)
      return
c
 1000 FORMAT('1Test of I1(X) vs Multiplication Theorem'//)
 1001 FORMAT('1Test of I1(X) vs Taylor series'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval ',
     1    '(',F5.2,',',F5.2,')'//)
 1011 FORMAT(' I1(X) was larger',I6,' times,'/
     1    10X,' agreed',I6,' times, and'/
     1    6X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Test with extreme arguments'/)
 1032 FORMAT(' I1(XMIN) = ',E24.17/)
 1033 FORMAT(' I1(',I1,') = ',E24.17/)
 1034 FORMAT(' I1(',E24.17,' ) = ',E24.17/)
 1035 FORMAT(' E**-X * I1(XMAX) = ',E24.17/)
 1036 FORMAT(' Tests near the largest argument for unscaled functions'/)
 1037 FORMAT(' This concludes the tests.') 
      END
      subroutine j0test ( )

c*********************************************************************72
c
cc J0TEST tests BESJ0.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - an environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following six
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MAXEXP - the smallest integer such that 
C                          FLOAT(IBETA)**MAXEXP causes overflow
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 XMIN   - the smallest positive normalized
C                          floating-point power of the radix
C                 XMAX   - the largest finite floating-point
C                          number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, FLOAT, LOG, MAX, SIGN, SQRT
C
C  Reference: "The use of Taylor series to test accuracy of
C              function programs", W. J. Cody and L. Stoltz,
C              submitted for publication.
C
C  Latest modification: March 13, 1992
C
C  Authors: W. J. Cody
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
      implicit none

      INTEGER I,IBETA,IEXP,II,III,IOUT,IRND,IT,J,JJ,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NDUM,NEGEP,NGRD
      DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,B,BESJ0,BESJ1,BETA,BJ0,BJ0P,BJ1,
     2   BJ1P,CJ0,CJ1,CONV,D,DEL,DELTA,EIGHT,ELEV,EPS,EPSNEG,
     3   FOUR,ONE,REN,R6,R7,SIXTEN,SUM,T,TERM,TWENTY,TWO56,W,
     4   X,XI,XL,XMAX,XMIN,XM,XN,X1,Y,YINV,YSQ,YX,Z,ZERO,ZZ
      DIMENSION BJ0P(6,10),BJ1P(6,10),XI(2),YX(2)
c
C  Mathematical constants
c
      DATA IOUT/6/
      DATA ZERO,ONE,FOUR,DELTA/0.0D0,1.0D0,4.0E0,0.0625D0/,
     1   EIGHT,TWENTY,ALL9,TWO56/8.0D0,20.0D0,-999.0D0,256.0D0/,
     2   SIXTEN,ELEV/16.0D0,11.0D0/
c
C  Coefficients for Taylor expansion
c
      DATA BJ0P/ 0.0D0,1.8144D6,-2.3688D5,1.0845D4,-2.7D2,5.0D0,
     1          -1.8144D5,2.394D4,-1.125D3,30.0D0,-1.0D0,0.0D0,
     2           0.0D0,2.016D4,-2.7D3,1.32D2,-4.0D0,0.0D0,
     3          -2.52D3,3.45D2,-18.0D0,1.0D0,0.0D0,0.0D0,
     4           0.0D0,3.6D2,-51.0D0,3.0D0,0.0D0,0.0D0,
     5          -60.0D0,9.0D0,-1.0D0,0.0D0,0.0D0,0.0D0,
     6           0.0D0,12.0D0,-2.0D0,0.0D0,0.0D0,0.0D0,
     7          -3.0D0,1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     8           0.0D0,1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     9          -1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
      DATA BJ1P/-3.6288D6,9.2736D5,-6.201D4,1.965D3,-40.0D0,1.0D0,
     1           3.6288D5,-9.324D4,6.345D3,-2.1D2,5.0D0,0.0D0,
     2          -4.032D4,1.044D4,-7.29D2,26.0D0,-1.0D0,0.0D0,
     3           5.04D3,-1.32D3,96.0D0,-4.0D0,0.0D0,0.0D0,
     4          -7.2D2,1.92D2,-15.0D0,1.0D0,0.0D0,0.0D0,
     5           1.2D2,-33.0D0,3.0D0,0.0D0,0.0D0,0.0D0,
     6          -24.0D0,7.0D0,-1.0D0,0.0D0,0.0D0,0.0D0,
     7           6.0D0,-2.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     8          -2.0D0,1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     9           1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
c
C  Zeroes of J0
c
      DATA XI/616.0D0,1413.0D0/
      DATA YX(1)/-7.3927648221700192757D-4/,
     1     YX(2)/-1.8608651797573879013D-4/
c
C  Statement functions for conversion between integer and float
c
      CONV(NDUM) = DBLE(NDUM)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      JT = 0
      B = ZERO
c
C  Random argument accuracy tests (based on a Taylor expansion)
c
      DO 300 J = 1, 3
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         A = B
         IF (J .EQ. 1) THEN
               B = FOUR
            ELSE IF (J .EQ. 2) THEN
               B = EIGHT
            ELSE
               B = TWENTY
               N = 2000
         END IF
         XN = CONV(N)
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
c
C  Carefully purify arguments and evaluate identities
c
            Y = X - DELTA
            W = SIXTEN * Y
            Y = (W + Y) - W
            X = Y + DELTA
            SUM = ZERO
            TERM = ZERO
            BJ0 = BESJ0(Y)
            Z = BESJ0(X)
            D = DELTA
            IF (ABS(Z) .LT. ABS(BJ0)) THEN
               CJ0 = X
               X = Y
               Y = CJ0
               CJ0 = BJ0
               BJ0 = Z
               Z = CJ0
               D = -D
            END IF
            BJ1 = BESJ1(Y)
            YINV = ONE/Y
            YSQ = ONE/(Y*Y)
            XM = ELEV
c
C  Evaluate (12-II)th derivative at Y.
c
            DO 170 II = 1, 10
               CJ0 = BJ0P(1,II)
               CJ1 = BJ1P(1,II)
               JJ = (11-II)/2 + 1
               DO 160  III = 2, JJ
                  CJ0 = CJ0 * YSQ + BJ0P(III,II)
                  CJ1 = CJ1 * YSQ + BJ1P(III,II)
  160          CONTINUE
               IF ((II/2)*2 .NE. II) THEN
                     CJ0 = CJ0 * YINV
                  ELSE
                     CJ1 = CJ1 * YINV
               ENDIF
               TERM = CJ0*BJ0 + CJ1*BJ1
               SUM = (SUM + TERM) * D/XM
               XM = XM - ONE
  170       CONTINUE
            SUM = (SUM - BJ1)*D + BJ0
            ZZ = SUM
c
C  Accumulate results
c
            W = (Z - ZZ) / Z
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
               ELSE
                  K2 = K2 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
               R6 = W
               X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C  Process and output statistics
c
         N = K1 + K2 + K3
         R7 = SQRT(R7/XN)
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,X1
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
  300 CONTINUE
c
C  Special tests
c
      WRITE (IOUT,1030)
      WRITE (IOUT,1031) IBETA
      DO 330 I = 1, 2  
         X = XI(I)/TWO56
         Y = BESJ0(X)
         T = (Y-YX(I))/YX(I)
         IF (T .NE. ZERO) THEN
               W = LOG(ABS(T))/ALBETA
            ELSE
               W = ALL9
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1032) X,Y,W
  330 CONTINUE
c
C  Test of error returns
c
      WRITE (IOUT,1033)
      X = XMAX
      WRITE (IOUT,1034) X
      Y = BESJ0(X)
      WRITE (IOUT,1036) Y
      WRITE (IOUT,1100)
      return
c
 1000 FORMAT('1Test of J0(X) VS Taylor expansion'  //)
 1010 FORMAT(I7,' random arguments were tested from the interval ',
     1 1H(,F5.1,1H,,F5.1,1H)//)
 1011 FORMAT(' ABS(J0(X)) was larger',I6,' times', /
     1     15X,' agreed',I6,' times, and'/
     1   11X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.' //)
 1021 FORMAT(' The maximum relative error of',E15.4,3H = ,I4,3H **,
     1  F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1  ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    3H = ,I4,3H **,F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Accuracy near zeros'//10X,'X',15X,'BESJ0(X)',
     1    13X,'Loss of base',I3,' digits'/)
 1032 FORMAT(E20.10,E25.15,8X,F7.2/)
 1033 FORMAT(//' Test with extreme arguments'///)
 1034 FORMAT(' J0 will be called with the argument ',E17.10/
     1     ' This may stop execution.'//)
 1036 FORMAT(' J0 returned the value',E25.17/)
 1100 FORMAT(' This concludes the tests.')
      END
      subroutine j1test ( )

c*********************************************************************72
c
cc J1TEST tests BESJ1.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - an environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following six
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MAXEXP - the smallest integer such that 
C                          FLOAT(IBETA)**MAXEXP causes overflow
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 XMIN   - the smallest positive normalized
C                          floating-point power of the radix
C                 XMAX   - the largest finite floating-point
C                          number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, FLOAT, LOG, MAX, SIGN, SQRT
C
C  Reference: "The use of Taylor series to test accuracy of
C              function programs", W. J. Cody and L. Stoltz,
C              submitted for publication.
C
C  Latest modification: March 13, 1992
C
C  Authors: W. J. Cody
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
      implicit none

      INTEGER I,IBETA,IEXP,II,III,IOUT,IRND,IT,J,JJ,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NDUM,NEGEP,NGRD
      DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,B,BESJ0,BESJ1,BETA,BJ0,BJ0P,BJ1,BJ1P,
     2   CJ0,CJ1,CONV,D,DEL,DELTA,EIGHT,ELEV,EPS,EPSNEG,FOUR,ONE,
     3   REN,R6,R7,SIXTEN,SUM,T,TERM,THRTEN,TWENTY,TWO,TWO56,W,X,
     4   XI,XL,XMAX,XMIN,XM,XN,X1,Y,YINV,YSQ,YX,Z,ZERO,ZZ
      DIMENSION BJ0P(6,10),BJ1P(6,10),XI(2),YX(2)
c
C  Mathematical constants
c
      DATA IOUT/6/
      DATA ZERO,ONE,FOUR,DELTA/0.0D0,1.0D0,4.0E0,0.0625D0/,
     1   EIGHT,TWENTY,ALL9,TWO56/8.0D0,20.0D0,-999.0D0,256.0D0/,
     2   TWO,THRTEN,SIXTEN,ELEV/2.0D0,13.0D0,16.0D0,11.0D0/
c
C  Coefficients for Taylor expansion
c
      DATA BJ0P/1.99584D7,-2.58552D6,1.16235D5,-2.775D3,4.5D1,-1.0D0,
     1           0.0D0,-1.8144D6,2.3688D5,-1.0845D4,2.7D2,-5.0D0,
     2           1.8144D5,-2.394D4,1.125D3,-30.0D0,1.0D0,0.0D0,
     3           0.0D0,-2.016D4,2.7D3,-1.32D2,4.0D0,0.0D0,
     4           2.52D3,-3.45D2,18.0D0,-1.0D0,0.0D0,0.0D0,
     5           0.0D0,-3.6D2,51.0D0,-3.0D0,0.0D0,0.0D0,
     6           60.0D0,-9.0D0,1.0D0,0.0D0,0.0D0,0.0D0,
     7           0.0D0,-12.0D0,2.0D0,0.0D0,0.0D0,0.0D0,
     8           3.0D0,-1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     9           0.0D0,-1.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
      DATA BJ1P/-3.99168D7,1.016064D7,-6.7095D5,2.067D4,-3.9D2,6.0D0,
     1           3.6288D6,-9.2736D5,6.201D4,-1.965D3,40.0D0,-1.0D0,
     2          -3.6288D5,9.324D4,-6.345D3,2.1D2,-5.0D0,0.0D0,
     3           4.032D4,-1.044D4,7.29D2,-26.0D0,1.0D0,0.0D0,
     4          -5.04D3,1.32D3,-96.0D0,4.0D0,0.0D0,0.0D0,
     5           7.2D2,-1.92D2,15.0D0,-1.0D0,0.0D0,0.0D0,
     6          -1.2D2,33.0D0,-3.0D0,0.0D0,0.0D0,0.0D0,
     7           24.0D0,-7.0D0,1.0D0,0.0D0,0.0D0,0.0D0,
     8          -6.0D0,2.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     9           2.0D0,-1.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
c
C  Zeroes of J1
c
      DATA XI/981.0D0,1796.0D0/
      DATA YX(1)/-1.3100393001327972376D-4/,
     1     YX(2)/ 1.1503460702301698285D-5/
c
C  Statement functions for conversion between integer and float
c
      CONV(NDUM) = DBLE(NDUM)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      JT = 0
      B = ZERO
c
C  Random argument accuracy tests (based on a Taylor expansion)
c
      DO 300 J = 1, 4
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         A = B
         IF (J .EQ. 1) THEN
               B = ONE
            ELSE IF (J .EQ. 2) THEN
               B = FOUR
            ELSE IF (J .EQ. 3) THEN
               B = EIGHT
            ELSE
               B = TWENTY
         END IF
         XN = CONV(N)
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
            IF (J .EQ. 1) THEN
c
C  Use traditional Maclaurin series for small arguments.
c
                  Y = X/TWO
                  SUM = Y
                  XM = THRTEN
                  DO 100 II = 1,12
                     SUM = SUM*Y/XM
                     XM = XM-ONE
                     SUM = (ONE - SUM/XM)*Y
  100             CONTINUE
                  ZZ = SUM
                  Z = BESJ1(X)
               ELSE
c
C  Use local Taylor series elsewhere.  First, purify arguments.
c
                  Y = X - DELTA
                  W = SIXTEN * Y
                  Y = (W + Y) - W
                  X = Y + DELTA
                  SUM = ZERO
                  TERM = ZERO
                  BJ1 = BESJ1(Y)
                  Z = BESJ1(X)
                  D = DELTA
                  IF (ABS(Z) .LT. ABS(BJ1)) THEN
                     CJ1 = X
                     X = Y
                     Y = CJ1
                     CJ1 = BJ1
                     BJ1 = Z
                     Z = CJ1
                     D = -D
                  END IF
                  BJ0 = BESJ0(Y)
                  YINV = ONE/Y
                  YSQ = ONE/(Y*Y)
                  XM = ELEV
c
C  Evaluate (12-II)th derivative at Y.
c
                  DO 170 II = 1, 10
                     CJ0 = BJ0P(1,II)
                     CJ1 = BJ1P(1,II)
                     JJ = (12-II)/2 + 1
                     DO 160  III = 2, JJ
                        CJ0 = CJ0 * YSQ + BJ0P(III,II)
                        CJ1 = CJ1 * YSQ + BJ1P(III,II)
  160                CONTINUE
                     IF ((II/2)*2 .EQ. II) THEN
                           CJ0 = CJ0 * YINV
                        ELSE
                           CJ1 = CJ1 * YINV
                     ENDIF
                     TERM = CJ0*BJ0 + CJ1*BJ1
                     SUM = (SUM + TERM) * D/XM
                     XM = XM - ONE
  170             CONTINUE
                  SUM = (SUM + BJ0 - BJ1*YINV)*D + BJ1
                  ZZ = SUM
            END IF
c
C  Accumulate results
c
            W = (Z - ZZ) / Z
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
               ELSE
                  K2 = K2 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
               R6 = W
               X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C  Process and output statistics
c
         N = K1 + K2 + K3
         R7 = SQRT(R7/XN)
         IF (J .EQ. 1) THEN
               WRITE (IOUT,1000)
            ELSE
               WRITE (IOUT,1001)
         END IF
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,X1
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
  300 CONTINUE
c
C  Special tests
c
      WRITE (IOUT,1030)
      WRITE (IOUT,1031) IBETA
      DO 330 I = 1, 2  
         X = XI(I)/TWO56
         Y = BESJ1(X)
         T = (Y-YX(I))/YX(I)
         IF (T .NE. ZERO) THEN
               W = LOG(ABS(T))/ALBETA
            ELSE
               W = ALL9
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1032) X,Y,W
  330 CONTINUE
c
C  Test of error returns
c
      WRITE (IOUT,1033)
      X = XMAX
      WRITE (IOUT,1034) X
      Y = BESJ1(X)
      WRITE (IOUT,1036) Y
      WRITE (IOUT,1100)
      return
c
 1000 FORMAT('1Test of J1(X) VS Maclaurin expansion'  //)
 1001 FORMAT('1Test of J1(X) VS local Taylor expansion'  //)
 1010 FORMAT(I7,' random arguments were tested from the interval ',
     1 1H(,F5.1,1H,,F5.1,1H)//)
 1011 FORMAT(' ABS(J1(X)) was larger',I6,' times', /
     1     15X,' agreed',I6,' times, and'/
     1   11X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.' //)
 1021 FORMAT(' The maximum relative error of',E15.4,3H = ,I4,3H **,
     1  F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1  ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    3H = ,I4,3H **,F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Accuracy near zeros'//10X,'X',15X,'BESJ1(X)',
     1    13X,'Loss of base',I3,' digits'/)
 1032 FORMAT(E20.10,E25.15,8X,F7.2/)
 1033 FORMAT(//' Test with extreme arguments'///)
 1034 FORMAT(' J1 will be called with the argument ',E17.10/
     1     ' This may stop execution.'//)
 1036 FORMAT(' J1 returned the value',E25.17/)
 1100 FORMAT(' This concludes the tests.')
      END
      subroutine k0test ( )

c*********************************************************************72
C
cc K0TEST tests BESK0.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - an environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following three
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MAXEXP - the smallest positive power of BETA
C                          that overflows
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 XMIN   - the smallest non-vanishing normalized
C                          floating-point power of the radix, i.e.,
C                          XMIN = FLOAT(IBETA) ** MINEXP
C                 XMAX   - the largest finite floating-point number.
C                          In particular XMAX = (1.0-EPSNEG) * 
C                          FLOAT(IBETA) ** MAXEXP
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  User defined functions
C
C         BOT, TOP
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C  Latest modification: March 14, 1992
C
C  Author: Laura Stoltz 
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
      implicit none

      LOGICAL SFLAG,TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MB,MINEXP,N,NDUM,NEGEP,NGRD
      DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,AMAXEXP,ATETEN,B,BESEK0,BESK0,BESK1,BETA,
     2   BOT,C,CONST,CONV,DEL,EIGHT,EPS,EPSNEG,FIVE,HALF,ONE,ONENEG,
     3   ONE28,PI,REN,R6,R7,SUM,T,TOP,TWENTY,TWO,U,W,X,XA,XB,XDEN,
     4   XL,XLAM,XLARGE,XMAX,XMB,XMIN,XN,XNINE,X1,Y,Z,ZERO,ZZ
      DIMENSION U(0:559)
      DATA ZERO,HALF,ONE,TWO,EIGHT/0.0D0,0.5D0,1.0D0,2.0D0,8.0D0/,
     1   XNINE,ATETEN,TWENTY,ONE28/9.0D0,18.0D0,20.0D0,128.0D0/,
     2   FIVE,ONENEG,XDEN,ALL9/5.0D0,-1.0D0,16.0D0,-999.0D0/,
     3   PI/3.141592653589793D0/
      DATA IOUT/6/
      TOP(X) = -X - HALF*LOG(TWO*X) + LOG(ONE-(ONE/EIGHT-XNINE/
     1   ONE28/X)/X)
      BOT(X) = (XDEN*X-ATETEN) / (((ONE28*X-XDEN)*X+XNINE)*X)
     1   - ONE - HALF/X
c
C  Statement functions for conversion between integer and float
c

      CONV(NDUM) = DBLE(NDUM)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      AMAXEXP = CONV(MAXEXP)
      JT = 0
      B = EPS
      XLAM = (XDEN - ONE) / XDEN
      CONST = HALF * LOG(PI) - LOG(XMIN)
c
C     Random argument accuracy tests
c
      DO 300 J = 1, 3
         SFLAG = ((J .EQ. 1) .AND. (AMAXEXP/AIT .LE. FIVE))
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         IF (SFLAG) B = SQRT(EPS)
         A = B
         IF (J .EQ. 1) THEN
               B = ONE
            ELSE IF (J .EQ. 2) THEN
               B = EIGHT
            ELSE
               B = TWENTY
         END IF
         XN = CONV(N)
         DEL = (B - A) / XN
         XL = A
c
C   Accuracy test is based on the multiplication theorem
c
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
            Y = X / XLAM
            W = XDEN * Y
            Y = (W + Y) - W 
            X = Y * XLAM
            U(0) = BESK0(Y)
            U(1) = BESK1(Y)
            TFLAG = SFLAG .AND. (Y .LT. HALF)
            IF (TFLAG) THEN
                  U(0) = U(0) * EPS
                  U(1) = U(1) * EPS
            END IF
            MB = 1
            XMB = ONE
            Y = Y * HALF
            T = U(0) * EPS
            W = (ONE-XLAM) * (ONE+XLAM)
            C = W *Y 
            DO 110 II = 2, 60
               T = XMB * T / C
               Z = U(II-1)
               IF (Z .LT. T) THEN
                     GO TO 120
                  ELSE IF (U(II-1) .GT. ONE) THEN
                     IF ((XMB/Y) .GT. (XMAX/U(II-1))) THEN
                           XL = XL + DEL
                           A = XL
                           GO TO  200
                     END IF
               END IF
               U(II) = XMB/Y * U(II-1) + U(II-2)
               XMB = XMB + ONE
               MB = MB + 1
  110       CONTINUE
  120       SUM = U(MB)
            IND = MB
            DO 155 II = 1, MB
               IND = IND - 1
               SUM = SUM * W * Y / XMB + U(IND)
               XMB = XMB - ONE
  155       CONTINUE
            ZZ = SUM
            IF (TFLAG) ZZ = ZZ / EPS
            Z = BESK0(X)
            Y = Z
            IF (U(0) .GT. Y) Y= U(0)
            W = (Z - ZZ) / Y
            IF (W .GT. ZERO) THEN 
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN 
                  K3 = K3 + 1
               ELSE
                  K2 = K2 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
         N = K1 + K2 + K3
         XN = CONV(N)
         R7 = SQRT(R7/XN)
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         W = ALL9
         IF (R6 .NE. ZERO) W = LOG(R6)/ALBETA
         IF (J .EQ. 3) THEN
               WRITE (IOUT,1024) R6,IBETA,W,X1
            ELSE
               WRITE (IOUT,1021) R6,IBETA,W,X1
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         W = ALL9
         IF (R7 .NE. ZERO) W = LOG(R7)/ALBETA
         IF (J .EQ. 3) THEN
               WRITE (IOUT,1025) R7,IBETA,W
            ELSE
               WRITE (IOUT,1023) R7,IBETA,W
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
  300 CONTINUE
c
C  Special tests
c
      WRITE (IOUT,1030)
      WRITE (IOUT,1031)
      Y = BESK0(XMIN)
      WRITE (IOUT,1032) Y
      Y = BESK0(ZERO)
      WRITE (IOUT,1033) 0,Y
      X = REN(JT) * ONENEG
      Y = BESK0(X)
      WRITE (IOUT,1034) X,Y
      Y = BESEK0(XMAX)
      WRITE (IOUT,1035) Y
      XA = LOG(XMAX)
  330 XB = XA - (TOP(XA)+CONST) / BOT(XA)
      IF (ABS(XB-XA)/XB .LE. EPS) THEN 
            GO TO 350
         ELSE
            XA = XB
            GO TO 330
      END IF
  350 XLARGE = XB * XLAM 
      Y = BESK0(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      XLARGE = XB * (XNINE / EIGHT)
      Y = BESK0(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      return
c 
 1000 FORMAT('1Test of K0(X) vs Multiplication Theorem'//)
 1010 FORMAT(I7,' random arguments were tested from the interval (',
     1    F5.1,',',F5.1,')'//)
 1011 FORMAT(' ABS(K0(X)) was larger',I6,' times,'/
     1    20X,' agreed',I6,' times, and'/
     1    16X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1024 FORMAT(' The maximum absolute error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1025 FORMAT(' The root mean square absolute error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(//' Test with extreme arguments'/)
 1032 FORMAT(' K0(XMIN) = ',E24.17/)
 1033 FORMAT(' K0(',I1,') = ',E24.17/)
 1034 FORMAT(' K0(',E24.17,' ) = ',E24.17/)
 1035 FORMAT(' E**X * K0(XMAX) = ',E24.17/)
      END
      subroutine k1test ( )

c*********************************************************************72
C
cc K1TEST tests BESK1.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - an environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following three
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MAXEXP - the smallest positive power of BETA
C                          that overflows
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 XMIN   - the smallest non-vanishing normalized
C                          floating-point power of the radix, i.e.,
C                          XMIN = FLOAT(IBETA) ** MINEXP
C                 XMAX   - the largest finite floating-point number.
C                          In particular XMAX = (1.0-EPSNEG) * 
C                          FLOAT(IBETA) ** MAXEXP
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  User defined functions
C
C         BOT, TOP
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C  Latest modification: March 14, 1992
C
C  Author - Laura Stoltz 
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
c
      implicit none

      LOGICAL SFLAG,TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MB,MINEXP,N,NDUM,NEGEP,NGRD
      DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,AMAXEXP,B,BESEK1,BESK0,BESK1,BETA,BOT,C,
     2   CONST,CONV,DEL,EIGHT,EPS,EPSNEG,FIFTEN,FIVE,FOUR8,HALF,HUND,
     3   ONE,ONENEG,ONE28,PI,REN,R6,R7,SUM,T,T1,THIRTY,THREE,TOP,
     4   TWENTY,TWO,U,W,X,XA,XB,XDEN,XL,XLAM,XLARGE,XLEAST,XMAX,XMB,
     5   XMIN,XN,XNINE,X1,Y,Z,ZERO,ZZ
      DIMENSION U(0:559)
c
C  Mathematical constants
c
      DATA ZERO,HALF,ONE,TWO,EIGHT/0.0D0,0.5D0,1.0D0,2.0D0,8.0D0/,
     1   XNINE,TWENTY,ONE28/9.0D0,20.0D0,128.0D0/,
     2   FIVE,ONENEG,XDEN,ALL9/5.0D0,-1.0D0,16.0D0,-999.0D0/,
     3   THREE,FIFTEN,THIRTY,FOUR8/3.0D0,15.0D0,30.0D0,48.0D0/,
     4   PI/3.141592653589793D0/,HUND/100.0D0/
c-
C  Machine-dependent constant
c-
      DATA XLEAST/2.23D-308/
      DATA IOUT/6/
      TOP(X) = -X - HALF*LOG(TWO*X) + LOG(ONE+(THREE/EIGHT-FIFTEN/
     1   ONE28/X)/X)
      BOT(X) = - ONE - HALF/X + ((- FOUR8*X + THIRTY) / 
     1   (((ONE28*X+FOUR8)*X-FIFTEN)*X))
c-
C  Statement functions for conversion between integer and float
c-
      CONV(NDUM) = DBLE(NDUM)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      AMAXEXP = CONV(MAXEXP)
      JT = 0
      B = EPS
      XLAM = (XDEN - ONE) / XDEN
      CONST = HALF * LOG(PI) - LOG(XMIN)
c
C     Random argument accuracy tests
c
      DO 300 J = 1, 3
         SFLAG = ((J .EQ. 1) .AND. (AMAXEXP/AIT .LE. FIVE))
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         A = B
         IF (J .EQ. 1) THEN
               B = ONE
            ELSE IF (J .EQ. 2) THEN
               B = EIGHT
            ELSE
               B = TWENTY
         END IF
         XN = CONV(N)
         DEL = (B - A) / XN
         XL = A
c-
C   Accuracy test is based on the multiplication theorem
c-
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
            Y = X / XLAM
            W = XDEN * Y
            Y = (W + Y) - W 
            X = Y * XLAM
            U(0) = BESK0(Y)
            U(1) = BESK1(Y)
            TFLAG = SFLAG .AND. (Y .LT. HALF)
            IF (TFLAG) THEN
                  U(0) = U(0) * EPS
                  U(1) = U(1) * EPS
            END IF
            MB = 1
            XMB = ONE
            Y = Y * HALF
            W = (ONE-XLAM) * (ONE+XLAM)
            C = W *Y 
            T = U(0) + C * U(1)
            T1 = EPS / HUND
            DO 110 II = 2, 60
               Z = U(II-1)
               IF (Z/T1 .LT. T) THEN
                     GO TO 120
                  ELSE IF (U(II-1) .GT. ONE) THEN
                     IF ((XMB/Y) .GT. (XMAX/U(II-1))) THEN
                           XL = XL + DEL
                           A = XL
                           GO TO  200
                     END IF
               END IF
               U(II) = XMB/Y * U(II-1) + U(II-2)
               IF (T1 .GT. ONE/EPS) THEN
                     T = T * T1
                     T1 = ONE
               END IF
               T1 = XMB * T1 / C
               XMB = XMB + ONE
               MB = MB + 1
  110       CONTINUE
  120       SUM = U(MB)
            IND = MB
            MB = MB - 1
            DO 155 II = 1, MB
               XMB = XMB - ONE
               IND = IND - 1
               SUM = SUM * W * Y / XMB + U(IND)
  155       CONTINUE
            ZZ = SUM
            IF (TFLAG) ZZ = ZZ / EPS
            ZZ = ZZ * XLAM
            Z = BESK1(X)
            Y = Z
            IF (U(0) .GT. Y) Y= U(0)
            W = (Z - ZZ) / Y
            IF (W .GT. ZERO) THEN 
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN 
                  K3 = K3 + 1
               ELSE
                  K2 = K2 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
         N = K1 + K2 + K3
         XN = CONV(N)
         R7 = SQRT(R7/XN)
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         W = ALL9
         IF (R6 .NE. ZERO) W = LOG(R6)/ALBETA
         IF (J .EQ. 3) THEN
               WRITE (IOUT,1024) R6,IBETA,W,X1
            ELSE
               WRITE (IOUT,1021) R6,IBETA,W,X1
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         W = ALL9
         IF (R7 .NE. ZERO) W = LOG(R7)/ALBETA
         IF (J .EQ. 3) THEN
               WRITE (IOUT,1025) R7,IBETA,W
            ELSE
               WRITE (IOUT,1023) R7,IBETA,W
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
  300 CONTINUE
c
C  Special tests
c
      WRITE (IOUT,1030)
      WRITE (IOUT,1031)
      Y = BESK1(XLEAST)
      WRITE (IOUT,1032) Y
      Y = BESK1(XMIN)
      WRITE (IOUT,1036) Y
      Y = BESK1(ZERO)
      WRITE (IOUT,1033) 0,Y
      X = REN(JT) * ONENEG
      Y = BESK1(X)
      WRITE (IOUT,1034) X,Y
      Y = BESEK1(XMAX)
      WRITE (IOUT,1035) Y
      XA = LOG(XMAX)
  330 XB = XA - (TOP(XA)+CONST) / BOT(XA)
      IF (ABS(XB-XA)/XB .LE. EPS) THEN 
            GO TO 350
         ELSE
            XA = XB
            GO TO 330
      END IF
  350 XLARGE = XB * XLAM 
      Y = BESK1(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      XLARGE = XB * (XNINE / EIGHT)
      Y = BESK1(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
c 
C  Test of error returns 
c
      return
 1000 FORMAT('1Test of K1(X) vs Multiplication Theorem'//)
 1010 FORMAT(I7,' random arguments were tested from the interval (',
     1    F5.1,',',F5.1,')'//)
 1011 FORMAT(' ABS(K1(X)) was larger',I6,' times,'/
     1    20X,' agreed',I6,' times, and'/
     1    16X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1024 FORMAT(' The maximum absolute error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1025 FORMAT(' The root mean square absolute error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(//' Test with extreme arguments'/)
 1032 FORMAT(' K1(XLEAST) = ',E24.17/)
 1033 FORMAT(' K1(',I1,') = ',E24.17/)
 1034 FORMAT(' K1(',E24.17,' ) = ',E24.17/)
 1035 FORMAT(' E**X * K1(XMAX) = ',E24.17/)
 1036 FORMAT(' K1(XMIN) = ',E24.17/)
      END
      subroutine psitst ( )

c*********************************************************************72
C
cc PSITST tests PSI.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - an environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following five
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MINEXP - the largest in magnitude negative
C                          integer such that  FLOAT(IBETA)**MINEXP
C                          is a positive floating-point number
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 EPSNEG - the smallest positive floating-point
C                          number such that 1.0-EPSNEG .NE. 1.0
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic Fortran functions required are:
C
C         ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs related
C              to the real gamma function", W. J. Cody, 
C              submitted for publication.
C
C  Latest modification: March 14, 1992
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
      implicit none

      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NDUM,NEGEP,NGRD
      DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,B,BETA,CONV,DEL,EIGHT,EPS,
     2   EPSNEG,HALF,ONE,ONE7,ONE6,PSI,REN,R6,R7,THREE,
     3   TWENTY,Y,V0,W,X,XH,XL,XL2,XMAX,XMIN,XN,XX,X0,
     4   X01,X1,Z,ZERO,ZH,ZZ
      DATA ZERO,ONE,THREE/0.0E0,1.0E0,3.0E0/,
     1   HALF,EIGHT,TWENTY,ALL9/0.5D0,8.0D0,20.0D0,-999.0D0/,
     2   XL2/6.9314718055994530942D-1/,
     3   ONE7,ONE6/-17.625D0,-16.875D0/,
     4   X0,X01,V0/374.0D0,256.0D0,-6.7240239024288040437D-04/
      DATA IOUT/6/
c
C  Statement functions for conversion between integer and float
c
      CONV(NDUM) = DBLE(NDUM)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      JT = 0
c
C     Random argument accuracy tests
c
      DO 300 J = 1, 4
         K1 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         IF (J .EQ. 1) THEN
               A = ZERO
               B = ONE
            ELSE IF (J .EQ. 2) THEN
               A = B + B
               B = EIGHT
            ELSE IF (J .EQ. 3) THEN
               A = B
               B = TWENTY
            ELSE
               A = ONE7
               B = ONE6
               N = 500
         END IF
         XN = CONV(N)
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
c
C  Carefully purify arguments and evaluate identity
c
            XX = X * HALF
            XH = XX + HALF
            XX = XH - HALF
            X = XX + XX
            Z = PSI(X)
            ZH = PSI(XH)
            ZZ = PSI(XX)
            ZZ = (ZZ+ZH)*HALF + XL2
c
C  Accumulate results
c
            W = (ZZ - Z) / ZZ
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
               R6 = W
               X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C  Process and output statistics
c
         K2 = N - K3 - K1
         R7 = SQRT(R7/XN)
         IF (2*(J/2) .NE. J) WRITE (IOUT,1000)
         WRITE (IOUT,1001)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,X1
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
  300 CONTINUE
c
C  Special tests
c
      WRITE (IOUT,1030)
         X = X0/X01
         Y = PSI(X)
         Z = (Y-V0)/V0
         IF (Z .NE. ZERO) THEN
               W = LOG(ABS(Z))/ALBETA
            ELSE
               W = ALL9
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1031) X,Y,IBETA,W
      WRITE (IOUT,1033)
      IF (XMAX*XMIN .GE. ONE) THEN
            X = XMIN
         ELSE 
            X = ONE / XMAX
      END IF
      WRITE (IOUT,1035) X
      Y = PSI(X)
      WRITE (IOUT,1036) Y
      X = XMAX
      WRITE (IOUT,1035) X
      Y = PSI(X)
      WRITE (IOUT,1036) Y
c
C  Test of error returns
c
      WRITE (IOUT,1037)
      X = ZERO
      WRITE (IOUT,1034) X
      Y = PSI(X)
      WRITE (IOUT,1036) Y
      X = -THREE/EPS
      WRITE (IOUT,1034) X
      Y = PSI(X)
      WRITE (IOUT,1036) Y
      WRITE (IOUT,1100)
      return
c
 1000 FORMAT('1')
 1001 FORMAT(' Test of PSI(X) vs (PSI(X/2)+PSI(X/2+1/2))/2 + ln(2)'
     1 //)
 1010 FORMAT(I7,' random arguments were tested from the interval ',
     1 1H(,F5.1,1H,,F5.1,1H)//)
 1011 FORMAT(' ABS(PSI(X)) was larger',I6,' times', /
     1     21X,' agreed',I6,' times, and'/
     1   17X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.' //)
 1021 FORMAT(' The maximum relative error of',E15.4,3H = ,I4,3H **,
     1  F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1  ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    3H = ,I4,3H **,F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Accuracy near positive zero'//' PSI(',E14.7,') = ',
     1    E24.17/13X,'Loss of base',I3,' digits = ',F7.2/)
 1033 FORMAT(//' Test with extreme arguments'/)
 1034 FORMAT(' PSI will be called with the argument ',E17.10/
     1     ' This may stop execution.'/)
 1035 FORMAT(' PSI will be called with the argument ',E17.10/
     1     ' This should not stop execution.'/)
 1036 FORMAT(' PSI returned the value',E25.17//)
 1037 FORMAT(//' Test of error returns'//)
 1100 FORMAT(' This concludes the tests.')
      END
      subroutine ritest ( )

c*********************************************************************72
C
cc RITEST tests RIBESL.
C
C  Method:
C
C     Two different accuracy tests are used.  In the first interval,
C     function values are compared against values generated with the
C     multiplication formula, where the Bessel values used in the
C     multiplication formula are obtained from the function program.
C     In the remaining intervals, function values are compared
C     against values generated with a local Taylor series expansion.
C     Derivatives in the expansion are expressed in terms of the
C     first two Bessel functions, which are in turn obtained from
C     the function program.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C         information on the floating-point arithmetic
C         system.  Note that the call to MACHAR can
C         be deleted provided the following five
C         parameters are assigned the values indicated
C
C              IBETA  - the radix of the floating-point system
C              IT     - the number of base-IBETA digits in the
C                       significant of a floating-point number
C              EPS    - the smallest positive floating-point
C                       number such that 1.0+EPS .NE. 1.0
C              XMAX   - the largest finite floating-point number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, DBLE, INT, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C             "Use of Taylor series to test accuracy of function
C              programs," W. J. Cody and L. Stoltz, submitted for
C              publication.
C
C  Latest modification: March 14, 1992
C
C  Authors: W. J. Cody and L. Stoltz
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
      implicit none

      INTEGER I,IBETA,IEXP,II,III,IND,IOUT,IRND,IT,IZE,J,JT,J1,J2,K,
     1    KK,K1,K2,K3,LAST,M,MACHEP,MAXEXP,MB,MBORG,MINEXP,MVR,N,NCALC,
     2    NDUM,NDX,NDX2,NEGEP,NGRD,NK,NO1,NUM
      DOUBLE PRECISION  
     1    A,AIT,AK,AKK,ALBETA,ALPHA,ALPHSQ,A1,AR1,AR2,B,BETA,C,CONV,D,
     2    DEL,DELTA,DERIV,E,EPS,EPSNEG,F,G,HALF,HUND,ONE,REN,R6,R7,
     3    SIXTEN,SUM,TEN,TWO,T1,T2,U,U2,W,X,XBAD,XL,XLAM,XLARGE,XMAX,
     4    XMB,XMIN,XJ1,XN,X1,X99,Y,YSQ,Z,ZERO,ZZ
      DIMENSION AR1(11,6),AR2(13,9),G(5),NDX(24),NDX2(8),U(560),U2(560)
      DATA ZERO,HALF,ONE,TWO/0.0D0,0.5D0,1.0D0,2.0D0/,
     1    TEN,SIXTEN,HUND,X99/10.0D0,1.6D1,1.0D2,-999.0D0/,
     2    XLAM,XLARGE/1.03125D0,1.0D4/,
     3    C/0.9189385332D0/
c
C  Arrays related to expansion of the derivatives in terms
C   of the first two Bessel functions.
c
      DATA  NDX/9,7,5,3,1,8,6,4,2,7,5,3,1,6,4,2,5,3,1,4,2,3,1,2/
      DATA  NDX2/5,9,13,16,19,21,23,24/
      DATA  AR1/0.0D0,1.0D0,0.0D0,-1.0D0,0.0D0,1.0D0,3.0D0,0.0D0,-2.0D0,
     1          -1.2D1,0.0D0,1.0D0,0.0D0,-1.0D0,1.0D0,2.0D0,0.0D0,
     2          -2.0D0,-6.0D0,1.0D0,7.0D0,2.4D1,0.0D0,0.0D0,1.0D0,0.0D0,
     3          -3.0D0,0.0D0,2.0D0,1.1D1,0.0D0,-1.2D1,-5.0D1,0.0D0,
     4          0.0D0,0.0D0,0.0D0,1.0D0,0.0D0,0.0D0,-6.0D0,0.0D0,2.0D0,
     5          3.5D1,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,1.0D0,
     6          0.0D0,0.0D0,-1.0D1,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     7          0.0D0,0.0D0,0.0D0,0.0D0,1.0D0/
      DATA  AR2/1.0D0,9.0D0,6.0D1,0.0D0,-3.0D0,-5.1D1,-3.6D2,0.0D0,
     1          1.0D0,1.8D1,3.45D2,2.52D3,0.0D0,0.0D0,-3.0D0,-3.3D1,
     2          -1.2D2,1.0D0,1.5D1,1.92D2,7.2D2,0.0D0,-4.0D0,-9.6D1,
     3          -1.32D3,-5.04D3,0.0D0,3.0D0,7.8D1,2.74D2,0.0D0,-2.7D1,
     4          -5.7D2,-1.764D3,0.0D0,4.0D0,2.46D2,4.666D3,1.3068D4,
     5          0.0D0,0.0D0,-1.8D1,-2.25D2,0.0D0,3.0D0,1.5D2,1.624D3,
     6          0.0D0,0.0D0,-3.6D1,-1.32D3,-1.3132D4,0.0D0,0.0D0,3.0D0,
     7          8.5D1,0.0D0,0.0D0,-4.5D1,-7.35D2,0.0D0,0.0D0,6.0D0,
     8          5.5D2,6.769D3,0.0D0,0.0D0,0.0D0,-1.5D1,0.0D0,0.0D0,
     9          3.0D0,1.75D2,0.0D0,0.0D0,0.0D0,-6.0D1,-1.96D3,0.0D0,
     a          0.0D0,0.0D0,1.0D0,0.0D0,0.0D0,0.0D0,-2.1D1,0.0D0,0.0D0,
     b          0.0D0,4.0D0,3.22D2,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     c          0.0D0,1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,-2.8D1,0.0D0,0.0D0,
     d          0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     e          0.0D0,1.0D0/
      DATA IOUT/6/
c
C  Statement function for integer to float conversion
c
      CONV(NDUM) = DBLE(NDUM)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      AIT = CONV(IT)
      ALBETA = LOG(BETA)
      A = ZERO
      B = TWO
      JT = 0
      DELTA = XLAM - ONE 
      F = (DELTA) * (XLAM+ONE) * HALF
c
C  Random argument accuracy tests
c
      DO 300 J = 1, 4
c
C  Determine the number of terms needed for convergence of the series
C  used in the multiplication theorem.  Use Newton iteration on the
C  asymptotic form of the convergence check for I0(X).
c
         XBAD = B
         D = AIT * ALBETA - C + ONE
         E = LOG(XBAD * F) + ONE
         AKK = ONE
  100    AK = AKK
            Z = D + E*AK - (AK+HALF) * LOG(AK+ONE)
            ZZ = E - (AK+HALF)/(AK+ONE) - LOG(AK+ONE)
            AKK = AK - Z/ZZ
         IF (ABS(AK-AKK) .GT. HUND*EPS*AK) GO TO 100
         MBORG = INT(AKK) + 1
         N = 2000
         XN = CONV(N)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         A1 = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            MB = MBORG
            X = DEL * REN(JT) + XL
            ALPHA = REN(JT)
            IZE = 1
c
C   Carefully purify arguments
c
            IF (J .EQ. 1) THEN
                  Y = X/XLAM
               ELSE
                  Y = X - DELTA
            END IF
            W = SIXTEN * Y
            T1 = W + Y
            T1 = W + T1
            Y = T1 - W
            Y = Y - W
            IF (J .EQ. 1) THEN
                  X = Y * XLAM
               ELSE
                  X = Y + DELTA
            END IF
            CALL RIBESL(Y,ALPHA,MB,IZE,U2,NCALC)
            IF (J .EQ. 1) THEN
c
C   Accuracy test is based on the multiplication theorem
c
                  D = F*Y
                  MB = NCALC - 2
                  XMB = CONV(MB)
                  SUM = U2(MB+1)
                  IND = MB
                  DO 155 II = 2, MB
                     SUM = SUM * D / XMB + U2(IND)
                     IND = IND - 1
                     XMB = XMB - ONE
  155             CONTINUE
                  ZZ = SUM * D + U2(IND)
                  ZZ = ZZ * XLAM ** ALPHA
               ELSE
c
C   Accuracy test is based on local Taylor's series expansion
c
                  YSQ = Y * Y
                  ALPHSQ = ALPHA * ALPHA
                  MB = 8
                  J1 = MB
                  XJ1 = CONV(J1+1)
                  IEXP = 0
                  NK = 13
                  NUM = 2
                  DO 180 II = 1, MB
                     IF (NK .EQ. 0) THEN 
                           NK = 11 
                           NUM = 1
                     END IF
                     K = 9 - J1
                     IF (K .GT. 1) THEN
                           NO1 = NDX2(K-1) + 1
                        ELSE
                           NO1 = 1
                     END IF
                     MVR = NO1
                     LAST = NDX2(K)
                     K = LAST - NO1 + 1
c
C         Group I(ALPHA) terms in the derivative
c
                     DO 160 III = 1, K
                        J2 = NDX(MVR)
                        IF (NUM .EQ. 1) THEN
                              G(III) = AR1(NK,J2)
                           ELSE
                              G(III) = AR2(NK,J2)
                        END IF
                        IF (J2 .GT. 1) THEN
  157                         J2 = J2 - 1
                              IF (NUM .EQ. 1) THEN
                                    G(III) = G(III) * ALPHA + AR1(NK,J2)
                                 ELSE
                                    G(III) = G(III) * ALPHA + AR2(NK,J2)
                              END IF
                           IF (J2 .GT. 1) GO TO 157
                        END IF
                        MVR = MVR + 1
                        NK = NK - 1
  160                CONTINUE
                     T1 = G(1)
                     DO 162 III = 2, K
                        T1 = T1 / YSQ + G(III)
  162                CONTINUE
                     IF (IEXP .EQ. 1) T1 = T1 / Y
c
C         Group I(ALPHA+1) terms in the derivative
c
                     IEXP = 1 - IEXP
                     NK = NK + K
                     MVR = NO1
                     KK = K
                     DO 165 III = 1, K
                        J2 = NDX(MVR)
                        M = MOD(J2,2)
                        IF (M .EQ. 1) J2 = J2 - 1
                        IF (J2 .GE. 2) THEN
                              IF (NUM .EQ. 1) THEN
                                    G(III) = AR1(NK,J2)
                                 ELSE
                                    G(III) = AR2(NK,J2)
                              END IF
  163                         J2 = J2 - 2
                              IF (J2 .GE. 2) THEN
                                    IF (NUM .EQ. 1) THEN
                                          G(III) = G(III) * ALPHSQ +
     1                                             AR1(NK,J2)
                                       ELSE
                                          G(III) = G(III) * ALPHSQ +
     1                                             AR2(NK,J2)
                                    END IF
                                    GO TO 163
                              END IF
                           ELSE
                              KK = III - 1
                        END IF
                        MVR = MVR + 1
                        NK = NK - 1
  165                CONTINUE
                     T2 = G(1)
                     DO 167 III = 2, KK
                        T2 = T2 / YSQ + G(III)
  167                CONTINUE
                     IF (IEXP .EQ. 1) T2 = T2 / Y
                     DERIV = U2(1) * T1 + U2(2) * T2
                     IF (J1 .EQ. 8) THEN
                           SUM = DERIV
                        ELSE
                           SUM = SUM * DELTA / XJ1 + DERIV
                     END IF
                     J1 = J1 - 1
                     XJ1 = XJ1 - ONE
  180             CONTINUE
                  ZZ = SUM * DELTA + U2(1)
            END IF
            MB = 2 
            CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
            Z = U(1)
c
C   Accumulate Results
c
            W = (Z - ZZ) / Z
            IF (W .GT. ZERO) THEN 
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN 
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  X1 = X
                  A1 = ALPHA
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C   Gather and print statistics for test
c
         K2 = N - K1 - K3
         R7 = SQRT(R7/XN)
         IF (J .EQ. 1) THEN
               WRITE (IOUT,1000)
            ELSE
               WRITE (IOUT,1001)
         END IF
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(R6)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,X1,A1
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(R7)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C   Initialize for next test
c
         A = B
         B = B + B
         IF (J .EQ. 2) B = TEN
  300 CONTINUE
c
C   Test of error returns
C
C   First, test with bad parameters
c
      WRITE (IOUT, 2006)
      X = ONE 
      ALPHA = ONE + HALF
      MB = 5
      IZE = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      ALPHA = HALF
      MB = -MB
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      MB = -MB
      IZE = 5
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
c
C   Last tests are with extreme parameters
c
      X = ZERO
      ALPHA = REN(JT)
      MB = 2
      IZE = 1
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      ALPHA = ZERO
      MB = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      ALPHA = ONE
      MB = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = -ONE
      ALPHA = HALF
      MB = 5
      IZE = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2012) X
      WRITE (IOUT, 2013)
      WRITE (IOUT, 2014) NCALC,U(1)
c
C   Determine largest safe argument for scaled functions
c
      WRITE (IOUT, 2015)
      X = XLARGE * (ONE - SQRT(SQRT(EPS)))
      IZE = 2
      MB = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2012) X
      WRITE (IOUT, 2014) NCALC,U(1)
      X = XLARGE * (ONE + SQRT(SQRT(EPS)))
      MB = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2012) X
      WRITE (IOUT, 2013)
      WRITE (IOUT, 2014) NCALC,U(1)
c
C   Determine largest safe argument for unscaled functions
c
      WRITE (IOUT, 2016)
      N = INT(LOG(XMAX))
      Z = CONV(N)
      X = Z * (ONE - SQRT(SQRT(EPS)))
      IZE = 1
      MB = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2012) X
      WRITE (IOUT, 2014) NCALC,U(1)
      X = Z * (ONE + SQRT(SQRT(EPS)))
      MB = 2
      U(1) = ZERO
      CALL RIBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2012) X
      WRITE (IOUT, 2013)
      WRITE (IOUT, 2014) NCALC,U(1)
      WRITE (IOUT, 2020)
      return
c
 1000 FORMAT('1Test of I(X,ALPHA) vs Multiplication Theorem'//)
 1001 FORMAT('1Test of I(X,ALPHA) vs Taylor series'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval ',
     1    '(',F5.2,',',F5.2,')'//)
 1011 FORMAT(' I(X,ALPHA) was larger',I6,' times,'/
     1    15X,' agreed',I6,' times, and'/
     1    11X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6,' and NU =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 2006 FORMAT('1Check of Error Returns'///
     1    ' The following summarizes calls with indicated parameters'//
     2    ' NCALC different from MB indicates some form of error'//
     3    ' See documentation for RIBESL for details'//
     4    7X,'ARG',12X,'ALPHA',6X,'MB',3X,'IZ',7X,'RES',6X,'NCALC'//)
 2011 FORMAT(2E15.7,2I5,E15.7,I5//)
 2012 FORMAT(' RIBESL will be called with the argument',E13.6)
 2013 FORMAT(' This should trigger an error message.')
 2014 FORMAT(' NCALC returned the value',I5/
     1    ' and RIBESL returned the value',E13.6/)
 2015 FORMAT(' Tests near the largest argument for scaled functions'/)
 2016 FORMAT(' Tests near the largest argument for unscaled functions'/)
 2020 FORMAT(' This concludes the tests.') 
      END
      subroutine rjtest ( )

c*********************************************************************72
c
cc RJTEST tests RJBESL.
C
C  Method:
C
C     Two different accuracy tests are used.  In the first interval,
C     function values are compared against values generated with the
C     multiplication formula, where the Bessel values used in the
C     multiplication formula are obtained from the function program.
C     In the remaining intervals, function values are compared
C     against values generated with a local Taylor series expansion.
C     Derivatives in the expansion are expressed in terms of the
C     first two Bessel functions, which are in turn obtained from
C     the function program.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C         information on the floating-point arithmetic
C         system.  Note that the call to MACHAR can
C         be deleted provided the following five
C         parameters are assigned the values indicated
C
C              IBETA  - the radix of the floating-point system
C              IT     - the number of base-IBETA digits in the
C                       significant of a floating-point number
C              EPS    - the smallest positive floating-point
C                       number such that 1.0+EPS .NE. 1.0
C              XMAX   - the largest finite floating-point number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, DBLE, INT, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C             "Use of Taylor series to test accuracy of function
C              programs," W. J. Cody and L. Stoltz, submitted for
C              publication.
C
C  Latest modification: March 14, 1992
C
C  Author: W. J. Cody 
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C Acknowledgement: this program is a minor modification of the test
C          driver for RIBESL whose primary author was Laura Stoltz.
C
      implicit none

      INTEGER I,IBETA,IEXP,II,III,IND,IOUT,IRND,IT,J,JT,J1,J2,K,KK,
     1    K1,K2,K3,LAST,M,MACHEP,MAXEXP,MB,MINEXP,MVR,N,NCALC,NDUM,
     2    NDX,NDX2,NEGEP,NGRD,NK,NO1,NUM
      DOUBLE PRECISION  
     1    A,AIT,ALBETA,ALPHA,ALPHSQ,A1,AR1,AR2,B,BETA,CONV,D,DEL,
     2    DELTA,DERIV,EPS,EPSNEG,F,FXMX,G,HALF,ONE,REN,RTPI,R6,
     3    R7,SIXTEN,SUM,TEN,TWO,T1,T2,U,U2,W,X,XL,XLAM,XLARGE,
     4    XMAX,XMB,XMIN,XJ1,XN,X1,X99,Y,YSQ,Z,ZERO,ZZ
      DIMENSION AR1(11,6),AR2(13,9),G(5),NDX(24),NDX2(8),U(560),U2(560)
      DATA ZERO,HALF,ONE,TWO/0.0D0,0.5D0,1.0D0,2.0D0/,
     1    TEN,SIXTEN,X99/10.0D0,1.6D1,-999.0D0/,
     2    XLAM,XLARGE,RTPI/1.03125D0,1.0D4,0.6366D0/
c
C  Arrays related to expansion of the derivatives in terms
C   of the first two Bessel functions.
c
      DATA  NDX/9,7,5,3,1,8,6,4,2,7,5,3,1,6,4,2,5,3,1,4,2,3,1,2/
      DATA  NDX2/5,9,13,16,19,21,23,24/
      DATA AR1/0.0D0,-1.0D0,0.0D0,1.0D0,0.0D0,1.0D0,-3.0D0,0.0D0,-2.0D0,
     1         1.2D1,0.0D0,1.0D0,0.0D0,-1.0D0,-1.0D0,2.0D0,0.0D0,
     2         2.0D0,-6.0D0,1.0D0,-7.0D0,2.4D1,0.0D0,0.0D0,1.0D0,0.0D0,
     3         -3.0D0,0.0D0,-2.0D0,1.1D1,0.0D0,1.2D1,-5.0D1,0.0D0,
     4         0.0D0,0.0D0,0.0D0,1.0D0,0.0D0,0.0D0,-6.0D0,0.0D0,-2.0D0,
     5         3.5D1,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,1.0D0,
     6         0.0D0,0.0D0,-1.0D1,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     7         0.0D0,0.0D0,0.0D0,0.0D0,1.0D0/
      DATA AR2/-1.0D0,9.0D0,-6.0D1,0.0D0,3.0D0,-5.1D1,3.6D2,0.0D0,
     1          1.0D0,-1.8D1,3.45D2,-2.52D3,0.0D0,0.0D0,-3.0D0,3.3D1,
     2          -1.2D2,-1.0D0,1.5D1,-1.92D2,7.2D2,0.0D0,4.0D0,-9.6D1,
     3          1.32D3,-5.04D3,0.0D0,3.0D0,-7.8D1,2.74D2,0.0D0,-2.7D1,
     4          5.7D2,-1.764D3,0.0D0,-4.0D0,2.46D2,-4.666D3,1.3068D4,
     5          0.0D0,0.0D0,1.8D1,-2.25D2,0.0D0,3.0D0,-1.5D2,1.624D3,
     6          0.0D0,0.0D0,-3.6D1,1.32D3,-1.3132D4,0.0D0,0.0D0,-3.0D0,
     7          8.5D1,0.0D0,0.0D0,4.5D1,-7.35D2,0.0D0,0.0D0,6.0D0,
     8          -5.5D2,6.769D3,0.0D0,0.0D0,0.0D0,-1.5D1,0.0D0,0.0D0,
     9          -3.0D0,1.75D2,0.0D0,0.0D0,0.0D0,6.0D1,-1.96D3,0.0D0,
     a          0.0D0,0.0D0,1.0D0,0.0D0,0.0D0,0.0D0,-2.1D1,0.0D0,0.0D0,
     b          0.0D0,-4.0D0,3.22D2,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     c          0.0D0,1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,-2.8D1,0.0D0,0.0D0,
     d          0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     e          0.0D0,1.0D0/
      DATA IOUT/6/
c
C  Statement function for integer to float conversion
c
      CONV(NDUM) = DBLE(NDUM)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      AIT = CONV(IT)
      ALBETA = LOG(BETA)
      A = ZERO
      B = TWO
      JT = 0
      DELTA = XLAM - ONE 
      F = (DELTA) * (XLAM+ONE) * HALF
c
C  Random argument accuracy tests
c
      DO 300 J = 1, 4
         N = 2000
         XN = CONV(N)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         A1 = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
  110       ALPHA = REN(JT)
c
C   Carefully purify arguments
c
            IF (J .EQ. 1) THEN
                  Y = X/XLAM
               ELSE
                  Y = X - DELTA
            END IF
            W = SIXTEN * Y
            T1 = W + Y
            T1 = W + T1
            Y = T1 - W
            Y = Y - W
            IF (J .EQ. 1) THEN
                  X = Y * XLAM
                  MB = 11
               ELSE
                  X = Y + DELTA
                  MB = 2
            END IF
            CALL RJBESL(Y,ALPHA,MB,U2,NCALC)
            CALL RJBESL(X,ALPHA,MB,U,NCALC)
            IF (J .EQ. 1) THEN
c
C   Accuracy test is based on the multiplication theorem
c
                  D = -F*Y
                  MB = NCALC - 2
                  XMB = CONV(MB)
                  SUM = U2(MB+1)
                  IND = MB
                  DO 125 II = 2, MB
                     SUM = SUM * D / XMB + U2(IND)
                     IND = IND - 1
                     XMB = XMB - ONE
  125             CONTINUE
                  ZZ = SUM * D + U2(IND)
                  ZZ = ZZ * XLAM ** ALPHA
               ELSE
c
C   Accuracy test is based on local Taylor's series expansion
c
                  IF (ABS(U(1)) .LT. ABS(U2(1))) THEN
                     Z = X
                     X = Y
                     Y = Z
                     DELTA = X - Y
                     DO 130 II = 1, MB
                        Z = U(II)
                        U(II) = U2(II)
                        U2(II) = Z
  130                CONTINUE
                  END IF
c
C   Filter out cases where function values or derivatives are small
c
                  W = SQRT(RTPI/X)/SIXTEN
                  Z = MIN(ABS(U2(1)),ABS(U2(2)))
                  IF (Z .LT. W) GO TO 110
                  YSQ = Y * Y
                  ALPHSQ = ALPHA * ALPHA
                  MB = 8
                  J1 = MB
                  XJ1 = CONV(J1+1)
                  IEXP = 0
                  NK = 13
                  NUM = 2
                  DO 180 II = 1, MB
                     IF (NK .EQ. 0) THEN 
                           NK = 11 
                           NUM = 1
                     END IF
                     K = 9 - J1
                     IF (K .GT. 1) THEN
                           NO1 = NDX2(K-1) + 1
                        ELSE
                           NO1 = 1
                     END IF
                     MVR = NO1
                     LAST = NDX2(K)
                     K = LAST - NO1 + 1
c
C         Group J(ALPHA) terms in the derivative
c
                     DO 160 III = 1, K
                        J2 = NDX(MVR)
                        IF (NUM .EQ. 1) THEN
                              G(III) = AR1(NK,J2)
                           ELSE
                              G(III) = AR2(NK,J2)
                        END IF
                        IF (J2 .GT. 1) THEN
  157                         J2 = J2 - 1
                              IF (NUM .EQ. 1) THEN
                                    G(III) = G(III) * ALPHA + AR1(NK,J2)
                                 ELSE
                                    G(III) = G(III) * ALPHA + AR2(NK,J2)
                              END IF
                           IF (J2 .GT. 1) GO TO 157
                        END IF
                        MVR = MVR + 1
                        NK = NK - 1
  160                CONTINUE
                     T1 = G(1)
                     DO 162 III = 2, K
                        T1 = T1 / YSQ + G(III)
  162                CONTINUE
                     IF (IEXP .EQ. 1) T1 = T1 / Y
c
C         Group J(ALPHA+1) terms in the derivative
c
                     IEXP = 1 - IEXP
                     NK = NK + K
                     MVR = NO1
                     KK = K
                     DO 165 III = 1, K
                        J2 = NDX(MVR)
                        M = MOD(J2,2)
                        IF (M .EQ. 1) J2 = J2 - 1
                        IF (J2 .GE. 2) THEN
                              IF (NUM .EQ. 1) THEN
                                    G(III) = AR1(NK,J2)
                                 ELSE
                                    G(III) = AR2(NK,J2)
                              END IF
  163                         J2 = J2 - 2
                              IF (J2 .GE. 2) THEN
                                    IF (NUM .EQ. 1) THEN
                                          G(III) = G(III) * ALPHSQ +
     1                                             AR1(NK,J2)
                                       ELSE
                                          G(III) = G(III) * ALPHSQ +
     1                                             AR2(NK,J2)
                                    END IF
                                    GO TO 163
                              END IF
                           ELSE
                              KK = III - 1
                        END IF
                        MVR = MVR + 1
                        NK = NK - 1
  165                CONTINUE
                     T2 = G(1)
                     DO 167 III = 2, KK
                        T2 = T2 / YSQ + G(III)
  167                CONTINUE
                     IF (IEXP .EQ. 1) T2 = T2 / Y
                     DERIV = U2(1) * T1 - U2(2) * T2
                     IF (J1 .EQ. 8) THEN
                           SUM = DERIV
                        ELSE
                           SUM = SUM * DELTA / XJ1 + DERIV
                     END IF
                     J1 = J1 - 1
                     XJ1 = XJ1 - ONE
  180             CONTINUE
                  ZZ = SUM * DELTA + U2(1)
            END IF
            MB = 2 
            Z = U(1)
c
C   Accumulate Results
c
            W = (Z - ZZ) / Z
            IF (W .GT. ZERO) THEN 
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN 
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  X1 = X
                  A1 = ALPHA
                  FXMX = Z
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C   Gather and print statistics for test
c
         K2 = N - K1 - K3
         R7 = SQRT(R7/XN)
         IF (J .EQ. 1) THEN
               WRITE (IOUT,1000)
            ELSE
               WRITE (IOUT,1001)
         END IF
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(R6)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,X1,A1
         WRITE (IOUT,1024) FXMX
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(R7)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C   Initialize for next test
c
         A = B
         IF (J .EQ. 1) THEN
               B = TEN
            ELSE IF (J .EQ. 2) THEN
               B = B + B
            ELSE IF (J .EQ. 3) THEN
               A = A + TEN
               B = A + TEN
         END IF
  300 CONTINUE
c
C   Test of error returns
C
C   First, test with bad parameters
c
      WRITE (IOUT, 2006)
      X = ONE 
      ALPHA = ONE + HALF
      MB = 5
      CALL RJBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
      ALPHA = HALF
      MB = -MB
      CALL RJBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
c
C   Last tests are with extreme parameters
c
      X = ZERO
      ALPHA = ONE
      MB = 2
      CALL RJBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
      X = -ONE
      ALPHA = HALF
      MB = 5
      CALL RJBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
c
C   Determine largest safe argument 
c
      WRITE (IOUT, 2015)
      X = XLARGE * (ONE - SQRT(SQRT(EPS)))
      MB = 2
      CALL RJBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2012) X
      WRITE (IOUT, 2014) NCALC,U(1)
      X = XLARGE * (ONE + SQRT(SQRT(EPS)))
      MB = 2
      CALL RJBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2012) X
      WRITE (IOUT, 2013)
      WRITE (IOUT, 2014) NCALC,U(1)
      WRITE (IOUT, 2020)
      return
c
 1000 FORMAT('1Test of J(X,ALPHA) vs Multiplication Theorem'//)
 1001 FORMAT('1Test of J(X,ALPHA) vs Taylor series'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval ',
     1    '(',F5.2,',',F5.2,')'//)
 1011 FORMAT(' J(X,ALPHA) was larger',I6,' times,'/
     1    15X,' agreed',I6,' times, and'/
     1    11X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6,' and NU =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1024 FORMAT(4x,'with J(X,ALPHA) = ',E13.6)
 2006 FORMAT('1Check of Error Returns'///
     1    ' The following summarizes calls with indicated parameters'//
     2    ' NCALC different from MB indicates some form of error'//
     3    ' See documentation for RJBESL for details'//
     4    7X,'ARG',12X,'ALPHA',6X,'MB',6X,'B(1)',6X,'NCALC'//)
 2011 FORMAT(2E15.7,I5,E15.7,I5//)
 2012 FORMAT(' RJBESL will be called with the argument',E13.6)
 2013 FORMAT(' This should trigger an error message.')
 2014 FORMAT(' NCALC returned the value',I5/
     1    ' and RJBESL returned U(1) = ',E13.6/)
 2015 FORMAT(' Tests near the largest acceptable argument for RJBESL'/)
 2020 FORMAT(' This concludes the tests.') 
      END
      subroutine rktest ( )

c*********************************************************************72
C
cc RKTEST tests RKBESL.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C         information on the floating-point arithmetic
C         system.  Note that the call to MACHAR can
C         be deleted provided the following five
C         parameters are assigned the values indicated
C
C              IBETA  - the radix of the floating-point system
C              IT     - the number of base-IBETA digits in the
C                       significand of a floating-point number
C              MINEXP - the largest in magnitude negative
C                       integer such that FLOAT(IBETA)**MINEXP
C                       is a positive floating-point number
C              EPS    - the smallest positive floating-point
C                       number such that 1.0+EPS .NE. 1.0
C              EPSNEG - the smallest positive floating-point
C                       number such that 1.0-EPSNEG .NE. 1.0
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, DBLE, EXP, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C  Latest modification: March 14, 1992
C
C  Authors: W. J. Cody and L. Stoltz
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
      implicit none

      LOGICAL SFLAG,TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,IZE,IZ1,J,JT,K1,K2,
     1        K3,MACHEP,MAXEXP,MB,MINEXP,N,NCALC,NDUM,NEGEP,NGRD
      DOUBLE PRECISION 
     1    A,AIT,ALBETA,ALPHA,AMAXEXP,A1,B,BETA,C,CONV,D,DEL,EIGHT,EPS,
     2    EPSNEG,FIVE,HALF,HUND,ONE,OVRCHK,REN,R6,R7,SIXTEN,SUM,T,TEN,
     3    TINYX,T1,U,U2,W,X,XL,XLAM,XMAX,XMB,XMIN,XN,XNINE,X1,X99,Y,Z,
     4    ZERO,ZZ
      DIMENSION U(560),U2(560)
      DATA ZERO,HALF,ONE,FIVE,EIGHT/0.0D0,0.5D0,1.0D0,5.0D0,8.0D0/
      DATA XNINE,TEN,HUND/9.0D0,10.0D0,1.0D2/
      DATA X99,SIXTEN,XLAM/-999.0D0,1.6D1,0.9375D0/
      DATA C/2.2579D-1/,TINYX/1.0D-10/
      DATA IOUT/6/
c
C  Define statement function for integer to float conversion
c
      CONV(NDUM) = DBLE(NDUM)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      AIT = CONV(IT)
      ALBETA = LOG(BETA)
      AMAXEXP = CONV(MAXEXP)
      A = EPS
      B = ONE
      JT = 0
c-
C  Random argument accuracy tests
c-
      DO 300 J = 1, 3
         SFLAG = ((J .EQ. 1) .AND. (AMAXEXP/AIT .LE. FIVE))
         N = 2000
         XN = CONV(N)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         A1 = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
            ALPHA = REN(JT)
c
C   Accuracy test is based on the multiplication theorem
c
            IZE = 1
            MB = 3
c
C   Carefully purify arguments
c
            Y = X/XLAM
            W = SIXTEN * Y
            T1 = W + Y
            Y = T1 - W
            X = Y * XLAM
            CALL RKBESL(Y,ALPHA,MB,IZE,U2,NCALC)
            TFLAG = SFLAG .AND. (Y .LT. HALF)
            IF (TFLAG) THEN
                  U2(1) = U2(1) * EPS
                  U2(2) = U2(2) * EPS
            END IF
            MB = 1
            XMB = ZERO
            W = (ONE-XLAM) * (ONE+XLAM) * HALF
            D = W*Y
            T = U2(1) + D * U2(2)
            T1 = EPS / HUND
c
C   Generate terms using recurrence
c
            DO 110 II = 3, 35
               XMB = XMB + ONE
               T1 = XMB * T1 / D
               Z = U2(II-1)
               OVRCHK = (XMB+ALPHA)/(Y*HALF)
               IF (Z/T1 .LT. T) THEN
                     GO TO 120
                  ELSE IF (U2(II-1) .GT. ONE) THEN
                     IF (OVRCHK .GT. (XMAX/U2(II-1))) THEN
                           XL = XL + DEL
                           A = XL
                           GO TO 200
                     END IF
               END IF
               U2(II) = OVRCHK * U2(II-1) + U2(II-2)
               IF (T1 .GT. ONE/EPS) THEN
                     T = T * T1
                     T1 = ONE
               END IF
               MB = MB + 1
  110       CONTINUE
c
C   Accurate Summation
c
            XMB = XMB + ONE
  120       SUM = U2(MB+1)
            IND = MB
            DO 155 II = 1, MB
               SUM = SUM * D / XMB + U2(IND)
               IND = IND - 1
               XMB = XMB - ONE
  155       CONTINUE
            ZZ = SUM
            IF (TFLAG) ZZ = ZZ / EPS
            ZZ = ZZ * XLAM ** ALPHA
            MB = 2 
            CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
            Z = U(1)
            Y = Z
            IF (U2(1) .GT. Y) Y = U2(1)
c
C   Accumulate Results
c
            W = (Z - ZZ) / Y
            IF (W .GT. ZERO) THEN 
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN 
                  K3 = K3 + 1
               ELSE
                  K2 = K2 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  X1 = X
                  A1 = ALPHA
                  IZ1 = IZE
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C   Gather and print statistics for test
c
         N = K1 + K2 + K3
         R7 = SQRT(R7/XN)
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         W = X99
         IF (R6 .NE. ZERO) W = LOG(R6)/ALBETA
         IF (J .EQ. 3) THEN
               WRITE (IOUT,1024) R6,IBETA,W,X1,A1,IZ1
            ELSE
               WRITE (IOUT,1021) R6,IBETA,W,X1,A1,IZ1
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         W = X99
         IF (R7 .NE. ZERO) W = LOG(R7)/ALBETA
         IF (J .EQ. 3) THEN
               WRITE (IOUT,1025) R7,IBETA,W
            ELSE
               WRITE (IOUT,1023) R7,IBETA,W
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C   Initialize for next test
c
         A = B
         B = B + B
         IF (J .EQ. 1) B = TEN
  300 CONTINUE
c-
C   Test of error returns
C   First, test with bad parameters
c-
      WRITE (IOUT, 2006)
      X = -ONE
      ALPHA = HALF
      MB = 5
      IZE = 2
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = -X
      ALPHA = ONE + HALF
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      ALPHA = HALF
      MB = -MB
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      MB = -MB
      IZE = 5
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
c-
C   Last tests are with extreme parameters
c-
      X = XMIN
      ALPHA = ZERO
      MB = 2
      IZE = 2
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = TINYX*(ONE-SQRT(EPS))
      MB = 20
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = TINYX*(ONE+SQRT(EPS))
      MB = 20
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
c-
C   Determine largest safe argument for unscaled functions
c-
      Z = LOG(XMIN)
      W = Z-C
      ZZ = -Z-TEN
  350 Z = ZZ
         ZZ = ONE/(EIGHT*Z)
         A = Z+LOG(Z)*HALF+ZZ*(ONE-XNINE*HALF*ZZ)+W
         B = ONE+(HALF-ZZ*(ONE-XNINE*ZZ))/Z
         ZZ = Z-A/B
      IF (ABS(Z-ZZ) .GT. HUND*EPS*Z) GO TO 350
      X = ZZ*XLAM
      IZE = 1
      MB = 2
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = ZZ
      MB = 2
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = TEN / EPS
      IZE = 2
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      X = XMAX
      U(1) = ZERO
      CALL RKBESL(X,ALPHA,MB,IZE,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,IZE,U(1),NCALC
      return
c-
 1000 FORMAT('1Test of K(X,ALPHA) vs Multiplication Theorem'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval ',
     1    '(',F5.2,',',F5.2,')'//)
 1011 FORMAT(' K(X,ALPHA) was larger',I6,' times,'/
     1    15X,' agreed',I6,' times, and'/
     1    11X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6,', NU =',E13.6,
     2    ' and IZE =',I2)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1024 FORMAT(' The maximum absolute error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6,', NU =',E13.6,
     2    ' and IZE =',I2)
 1025 FORMAT(' The root mean square absolute error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 2006 FORMAT('1Check of Error Returns'///
     1    ' The following summarizes calls with indicated parameters'//
     2    ' NCALC different from MB indicates some form of error'//
     3    ' See documentation for RKBESL for details'//
     4    7X,'ARG',12X,'ALPHA',6X,'MB',3X,'IZ',7X,'RES',6X,'NCALC'//)
 2011 FORMAT(2E15.7,2I5,E15.7,I5//)
      END
      subroutine rytest ( )

c*********************************************************************72
c
cc RYTEST tests RYBESL.
C
C  Method:
C
C     Two different accuracy tests are used.  In the first interval,
C     function values are compared against values generated with the
C     multiplication formula, where the Bessel values used in the
C     multiplication formula are obtained from the function program.
C     In the remaining intervals, function values are compared
C     against values generated with a local Taylor series expansion.
C     Derivatives in the expansion are expressed in terms of the
C     first two Bessel functions, which are in turn obtained from
C     the function program.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C         information on the floating-point arithmetic
C         system.  Note that the call to MACHAR can
C         be deleted provided the following five
C         parameters are assigned the values indicated
C
C              IBETA  - the radix of the floating-point system
C              IT     - the number of base-IBETA digits in the
C                       significant of a floating-point number
C              EPS    - the smallest positive floating-point
C                       number such that 1.0+EPS .NE. 1.0
C              XMIN   - the smallest positive normalized
C                       floating-point number
C              XMAX   - the largest finite floating-point number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, DBLE, INT, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C             "Use of Taylor series to test accuracy of function
C              programs," W. J. Cody and L. Stoltz, submitted for
C              publication.
C
C  Latest modification: March 14, 1992
C
C  Author: W. J. Cody 
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C Acknowledgement: this program is a minor modification of the test
C          driver for RIBESL whose primary author was Laura Stoltz.
C
      implicit none

      INTEGER I,IBETA,IEXP,II,III,IND,IOUT,IRND,IT,J,JT,J1,J2,K,KK,
     1    K1,K2,K3,LAST,M,MACHEP,MAXEXP,MB,MINEXP,MVR,N,NCALC,NDUM,
     2    NDX,NDX2,NEGEP,NGRD,NK,NO1,NUM
      DOUBLE PRECISION  
     1    A,AIT,ALBETA,ALPHA,ALPHSQ,A1,AR1,AR2,B,BETA,CONV,D,DEL,
     2    DELTA,DERIV,EPS,EPSNEG,F,FXMX,G,HALF,ONE,ONEP25,P875,REN,
     3    R6,R7,SIXTEN,SUM,TEN,TWO,TWOBPI,T1,T2,U,U2,W,X,XJ1,XL,XLAM,
     4    XMAX,XMB,XMIN,XN,X1,X99,Y,YSQ,Z,ZERO,ZZ
      DIMENSION AR1(11,6),AR2(13,9),G(5),NDX(24),NDX2(8),U(20),U2(20)
      DATA ZERO,HALF,ONE,ONEP25,TWO/0.0D0,0.5D0,1.0D0,1.25D0,2.0D0/,
     1    P875,TEN,SIXTEN,X99/0.875D0,10.0D0,1.6D1,-999.0D0/,
     2    XLAM,TWOBPI/1.03125D0,0.6366D0/
c
C  Arrays related to expansion of the derivatives in terms
C   of the first two Bessel functions.
c
      DATA  NDX/9,7,5,3,1,8,6,4,2,7,5,3,1,6,4,2,5,3,1,4,2,3,1,2/
      DATA  NDX2/5,9,13,16,19,21,23,24/
      DATA AR1/0.0D0,-1.0D0,0.0D0,1.0D0,0.0D0,1.0D0,-3.0D0,0.0D0,-2.0D0,
     1         1.2D1,0.0D0,1.0D0,0.0D0,-1.0D0,-1.0D0,2.0D0,0.0D0,
     2         2.0D0,-6.0D0,1.0D0,-7.0D0,2.4D1,0.0D0,0.0D0,1.0D0,0.0D0,
     3         -3.0D0,0.0D0,-2.0D0,1.1D1,0.0D0,1.2D1,-5.0D1,0.0D0,
     4         0.0D0,0.0D0,0.0D0,1.0D0,0.0D0,0.0D0,-6.0D0,0.0D0,-2.0D0,
     5         3.5D1,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,1.0D0,
     6         0.0D0,0.0D0,-1.0D1,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     7         0.0D0,0.0D0,0.0D0,0.0D0,1.0D0/
      DATA AR2/-1.0D0,9.0D0,-6.0D1,0.0D0,3.0D0,-5.1D1,3.6D2,0.0D0,
     1          1.0D0,-1.8D1,3.45D2,-2.52D3,0.0D0,0.0D0,-3.0D0,3.3D1,
     2          -1.2D2,-1.0D0,1.5D1,-1.92D2,7.2D2,0.0D0,4.0D0,-9.6D1,
     3          1.32D3,-5.04D3,0.0D0,3.0D0,-7.8D1,2.74D2,0.0D0,-2.7D1,
     4          5.7D2,-1.764D3,0.0D0,-4.0D0,2.46D2,-4.666D3,1.3068D4,
     5          0.0D0,0.0D0,1.8D1,-2.25D2,0.0D0,3.0D0,-1.5D2,1.624D3,
     6          0.0D0,0.0D0,-3.6D1,1.32D3,-1.3132D4,0.0D0,0.0D0,-3.0D0,
     7          8.5D1,0.0D0,0.0D0,4.5D1,-7.35D2,0.0D0,0.0D0,6.0D0,
     8          -5.5D2,6.769D3,0.0D0,0.0D0,0.0D0,-1.5D1,0.0D0,0.0D0,
     9          -3.0D0,1.75D2,0.0D0,0.0D0,0.0D0,6.0D1,-1.96D3,0.0D0,
     a          0.0D0,0.0D0,1.0D0,0.0D0,0.0D0,0.0D0,-2.1D1,0.0D0,0.0D0,
     b          0.0D0,-4.0D0,3.22D2,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     c          0.0D0,1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,-2.8D1,0.0D0,0.0D0,
     d          0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     e          0.0D0,1.0D0/
      DATA IOUT/6/
c
C  Statement function for integer to float conversion
c
      CONV(NDUM) = DBLE(NDUM)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      AIT = CONV(IT)
      ALBETA = LOG(BETA)
      A = ZERO
      B = TWO
      JT = 0
      DELTA = XLAM - ONE 
      F = (DELTA) * (XLAM+ONE) * HALF
c
C  Random argument accuracy tests
c
      DO 300 J = 1, 4
         N = 2000
         XN = CONV(N)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         A1 = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
  110       ALPHA = REN(JT)
c
C   Carefully purify arguments
c
            IF (J .LE. 1) THEN
                  Y = X/XLAM
               ELSE
                  Y = X - DELTA
            END IF
            W = SIXTEN * Y
            T1 = W + Y
            T1 = W + T1
            Y = T1 - W
            Y = Y - W
            IF (J .LE. 1) THEN
                  X = Y * XLAM
                  MB = 15
               ELSE
                  X = Y + DELTA
                  MB = 2
            END IF
            CALL RYBESL(Y,ALPHA,MB,U2,NCALC)
            CALL RYBESL(X,ALPHA,MB,U,NCALC)
            IF (J .LE. 1) THEN
c
C   Accuracy test is based on the multiplication theorem.
C   First, filter out cases where function values are small.
c
            IF (SIGN(ONE,U(1))*SIGN(ONE,U2(1)) .LT. ZERO) GO TO 110
                  D = -F*Y
                  MB = NCALC - 1
                  XMB = CONV(MB)
                  SUM = U2(MB+1)
                  Z = SUM
                  IND = MB
                  DO 125 II = 2, MB
                     SUM = SUM * D / XMB + U2(IND)
                     Z = Z * D / XMB
                     IND = IND - 1
                     XMB = XMB - ONE
  125             CONTINUE
                  Z = Z * D
c
C   Check for convergence.  
c
                  IF (ABS(Z/U2(IND)) .GT. EPS) THEN
                     XL = XL + DEL
                     GO TO 200
                  END IF
                  ZZ = SUM * D + U2(IND)
c
C   Check for numerical stability.  
c
                  D = ABS(ZZ/U2(IND))
                  IF ((D .GT. ONEP25) .OR. (D .LT. P875)) GO TO 110
                  ZZ = ZZ * XLAM ** ALPHA
               ELSE
c
C   Accuracy test is based on local Taylor's series expansion.
C   First, filter out cases where function values or derivatives
C   are small.
c
                  W = MIN(ABS(U(1)),ABS(U2(1)),ABS(U2(2)))
                  IF (W .LT. SQRT(TWOBPI/X)/SIXTEN) GO TO 110
                  IF (ABS(U(1)) .LT. ABS(U2(1))) THEN
                     Z = X
                     X = Y
                     Y = Z
                     DELTA = X - Y
                     DO 120 II = 1, 9
                        Z = U(II)
                        U(II) = U2(II)
                        U2(II) = Z
  120                CONTINUE
                  END IF
                  YSQ = Y * Y
                  ALPHSQ = ALPHA * ALPHA
                  MB = 8
                  J1 = MB
                  XJ1 = CONV(J1+1)
                  IEXP = 0
                  NK = 13
                  NUM = 2
                  DO 180 II = 1, MB
                     IF (NK .EQ. 0) THEN 
                           NK = 11 
                           NUM = 1
                     END IF
                     K = 9 - J1
                     IF (K .GT. 1) THEN
                           NO1 = NDX2(K-1) + 1
                        ELSE
                           NO1 = 1
                     END IF
                     MVR = NO1
                     LAST = NDX2(K)
                     K = LAST - NO1 + 1
c
C         Group I(ALPHA) terms in the derivative
c
                     DO 160 III = 1, K
                        J2 = NDX(MVR)
                        IF (NUM .EQ. 1) THEN
                              G(III) = AR1(NK,J2)
                           ELSE
                              G(III) = AR2(NK,J2)
                        END IF
                        IF (J2 .GT. 1) THEN
  157                         J2 = J2 - 1
                              IF (NUM .EQ. 1) THEN
                                    G(III) = G(III) * ALPHA + AR1(NK,J2)
                                 ELSE
                                    G(III) = G(III) * ALPHA + AR2(NK,J2)
                              END IF
                           IF (J2 .GT. 1) GO TO 157
                        END IF
                        MVR = MVR + 1
                        NK = NK - 1
  160                CONTINUE
                     T1 = G(1)
                     DO 162 III = 2, K
                        T1 = T1 / YSQ + G(III)
  162                CONTINUE
                     IF (IEXP .EQ. 1) T1 = T1 / Y
c
C         Group I(ALPHA+1) terms in the derivative
c
                     IEXP = 1 - IEXP
                     NK = NK + K
                     MVR = NO1
                     KK = K
                     DO 165 III = 1, K
                        J2 = NDX(MVR)
                        M = MOD(J2,2)
                        IF (M .EQ. 1) J2 = J2 - 1
                        IF (J2 .GE. 2) THEN
                              IF (NUM .EQ. 1) THEN
                                    G(III) = AR1(NK,J2)
                                 ELSE
                                    G(III) = AR2(NK,J2)
                              END IF
  163                         J2 = J2 - 2
                              IF (J2 .GE. 2) THEN
                                    IF (NUM .EQ. 1) THEN
                                          G(III) = G(III) * ALPHSQ +
     1                                             AR1(NK,J2)
                                       ELSE
                                          G(III) = G(III) * ALPHSQ +
     1                                             AR2(NK,J2)
                                    END IF
                                    GO TO 163
                              END IF
                           ELSE
                              KK = III - 1
                        END IF
                        MVR = MVR + 1
                        NK = NK - 1
  165                CONTINUE
                     T2 = G(1)
                     DO 167 III = 2, KK
                        T2 = T2 / YSQ + G(III)
  167                CONTINUE
                     IF (IEXP .EQ. 1) T2 = T2 / Y
                     DERIV = U2(1) * T1 - U2(2) * T2
                     IF (J1 .EQ. 8) THEN
                           SUM = DERIV
                        ELSE
                           SUM = SUM * DELTA / XJ1 + DERIV
                     END IF
                     J1 = J1 - 1
                     XJ1 = XJ1 - ONE
  180             CONTINUE
                  ZZ = SUM * DELTA + U2(1)
            END IF
            MB = 2 
            Z = U(1)
c
C   Accumulate Results
c
            W = (Z - ZZ) / Z
            IF (W .GT. ZERO) THEN 
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN 
                  K3 = K3 + 1
               ELSE
                  K2 = K2 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  X1 = X
                  A1 = ALPHA
                  FXMX = Z
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C   Gather and print statistics for test
c
         N = K1 + K2 + K3
         R7 = SQRT(R7/XN)
         IF (J .LE. 1) THEN
               WRITE (IOUT,1000)
            ELSE
               WRITE (IOUT,1001)
         END IF
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         IF (N .NE. 2000) WRITE (IOUT,1012) 2000-N
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(R6)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,X1,A1
         WRITE (IOUT,1024) FXMX
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(R7)/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C   Initialize for next test
c
         A = B
         IF (J .EQ. 1) THEN
               B = TEN
            ELSE IF (J .EQ. 2) THEN
               B = B + B
            ELSE IF (J .EQ. 3) THEN
               A = B + TEN
               B = A + TEN
         END IF
  300 CONTINUE
c
C   Test of error returns
C
C   First, test with bad parameters
c
      WRITE (IOUT, 2006)
      X = ONE 
      ALPHA = ONE + HALF
      MB = 5
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
      ALPHA = HALF
      MB = -MB
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
c
C   Tests with small parameters
c
      IF (XMIN*XMAX .GT. ONE) THEN
            X = XMIN
         ELSE
            X = ONE / XMAX
      END IF
      ALPHA = ZERO
      MB = 2
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
      X = X + X + X
      MB = 2
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
      ALPHA = ONE - EPS
      MB = 2
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2011) X,ALPHA,MB,U(1),NCALC
c
C   Last tests are with large parameters
c
      WRITE (IOUT, 2015)
      X = HALF / SQRT(EPS)
      ALPHA = HALF
      MB = 2
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2012) X,ALPHA
      WRITE (IOUT, 2014) NCALC,U(1)
      X = X * SIXTEN
      MB = 2
      CALL RYBESL(X,ALPHA,MB,U,NCALC)
      WRITE (IOUT, 2012) X,ALPHA
      WRITE (IOUT, 2013)
      WRITE (IOUT, 2014) NCALC,U(1)
      WRITE (IOUT, 2020)
      return
c
 1000 FORMAT('1Test of Y(X,ALPHA) vs Multiplication Theorem'//)
 1001 FORMAT('1Test of Y(X,ALPHA) vs Taylor series'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval ',
     1    '(',F5.2,',',F5.2,')'//)
 1011 FORMAT(' Y(X,ALPHA) was larger',I6,' times,'/
     1    15X,' agreed',I6,' times, and'/
     1    11X,'was smaller',I6,' times.'//)
 1012 FORMAT(' NOTE: first ',I3,' arguments in test interval skipped'/
     1    7x,'because multiplication theorem did not converge'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6,' and NU =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1024 FORMAT(4x,'with Y(X,ALPHA) = ',E13.6)
 2006 FORMAT('1Check of Error Returns'///
     1    ' The following summarizes calls with indicated parameters'//
     2    ' NCALC different from MB indicates some form of error'//
     3    ' See documentation for RYBESL for details'//
     4    7X,'ARG',12X,'ALPHA',6X,'MB',6X,'B(1)',6X,'NCALC'//)
 2011 FORMAT(2E15.7,I5,E15.7,I5//)
 2012 FORMAT(' RYBESL will be called with the arguments',2E13.6)
 2013 FORMAT(' This should trigger an error message.')
 2014 FORMAT(' NCALC returned the value',I5/
     1    ' and RYBESL returned U(1) = ',E13.6/)
 2015 FORMAT(' Tests near the largest acceptable argument for RYBESL'/)
 2020 FORMAT(' This concludes the tests.') 
      END
      subroutine y0test ( )

c*********************************************************************72
c
cc Y0TEST tests BESY0.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - an environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following five
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MINEXP - the largest in magnitude negative
C                          integer such that  FLOAT(IBETA)**MINEXP
C                          is a positive floating-point number
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 EPSNEG - the smallest positive floating-point
C                          number such that 1.0-EPSNEG .NE. 1.0
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C  Latest modification: March 13, 1992
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
      implicit none

      LOGICAL SFLAG,TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MB,MINEXP,N,NDUM,NEGEP,NGRD
      DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,B,BESY0,BESY1,BETA,C,CONV,DEL,EIGHT,
     2   EPS,EPSNEG,FIVE5,HALF,HUND,ONE,REN,R6,R7,SIXTEN,SUM,T,
     3   T1,THREE,TWENTY,TWO56,U,W,X,XI,XL,XLAM,XMAX,XMB,XMIN,
     4   X1,Y,YX,Z,ZERO,ZZ
      DIMENSION U(560),XI(3),YX(3)
      DATA ZERO,ONE,THREE,FIVE5/0.0D0,1.0D0,3.0D0,5.5D0/,
     1   EIGHT,TWENTY,ALL9,TWO56/8.0D0,20.0D0,-999.0D0,256.0D0/,
     2   HALF,SIXTEN,XLAM,HUND/0.5D0,16.0D0,0.9375D0,100.0D0/
      DATA XI/228.0D0,1013.0D0,1814.0D0/
      DATA YX(1)/-2.6003142722933487915D-3/,
     1     YX(2)/ 2.6053454911456774983D-4/,
     2     YX(3)/-3.4079448714795552077D-5/
      DATA IOUT/6/
c
C  Statement functions for conversion between integer and float
c
      CONV(NDUM) = DBLE(NDUM)
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      JT = 0
      A = EPS
      B = THREE
      N = 2000
c
C  Random argument accuracy tests based on the multiplication theorem
c
      DO 300 J = 1, 4
         SFLAG = (J .EQ. 1) .AND. (MAXEXP/IT .LE. 5)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / CONV(N)
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
c
C  Carefully purify arguments
c
            Y = X/XLAM
            W = SIXTEN * Y
            Y = (W + Y) - W
            X = Y * XLAM
c
C  Generate Bessel functions with forward recurrence
c
            U(1) = BESY0(Y)
            U(2) = BESY1(Y)
            TFLAG = SFLAG .AND. (Y .LT. HALF)
            IF (TFLAG) THEN
               U(1) = U(1) * EPS 
               U(2) = U(2) * EPS
            END IF
            MB = 1
            XMB = ONE
            Y = Y * HALF
            W = (ONE-XLAM)*(ONE+XLAM)
            C = W * Y
            T = ABS(U(1)+C*U(2))
            T1 = EPS/HUND
            DO 110 II = 3, 60
               Z = ABS(U(II-1))
               IF (Z/T1 .LT. T) GO TO 120
               IF (Y .LT. XMB) THEN
                  IF (Z .GT. XMAX*(Y/XMB)) THEN
                     A = X
                     XL = XL + DEL
                     GO TO  200
                  END IF
               END IF
               U(II) = XMB/Y * U(II-1) - U(II-2)
               IF (T1 .GT. ONE/EPS) THEN
                  T = T * T1
                  T1 = ONE
               END IF
               T1 = XMB * T1 / C
               XMB = XMB + ONE
               MB = MB + 1
  110       CONTINUE
c
C  Evaluate Bessel series expansion
c
  120       SUM = U(MB)
            IND = MB
            DO 155 II = 2, MB
               IND = IND - 1
               XMB = XMB - ONE
               SUM = SUM * W * Y / XMB + U(IND)
  155       CONTINUE
            ZZ = SUM
            IF (TFLAG) ZZ = ZZ / EPS 
            Z = BESY0(X)
            Y = Z
            IF (ABS(U(1)) .GT. ABS(Y)) Y = U(1)
c
C  Accumulate results
c
            W = (Z - ZZ) / Y
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
               ELSE
                  K2 = K2 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
               R6 = W
               X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C  Gather and print statistics
c
         N = K1 + K2 + K3
         R7 = SQRT(R7/CONV(N))
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         W = ALL9
         IF (R6 .NE. ZERO) W = LOG(ABS(R6))/ALBETA
         IF (J .EQ. 4) THEN
               WRITE (IOUT,1024) R6,IBETA,W,X1
            ELSE
               WRITE (IOUT,1021) R6,IBETA,W,X1
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         W = ALL9
         IF (R7 .NE. ZERO) W = LOG(ABS(R7))/ALBETA
         IF (J .EQ. 4) THEN
               WRITE (IOUT,1025) R7,IBETA,W
            ELSE
               WRITE (IOUT,1023) R7,IBETA,W
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
c
C  Initialize for next test
c
         A = B
         IF (J .EQ. 1) THEN
               B = FIVE5
               N = 2000
            ELSE IF (J .EQ. 2) THEN
               B = EIGHT
               N = 2000
            ELSE
               B = TWENTY
               N = 500
         END IF
  300 CONTINUE
c
C  Special tests
c
      WRITE (IOUT,1030)
      WRITE (IOUT,1031) IBETA
      DO 330 I = 1, 3
         X = XI(I)/TWO56
         Y = BESY0(X)
         W = ALL9
         T = (Y-YX(I))/YX(I)
         IF (T .NE. ZERO) W = LOG(ABS(T))/ALBETA
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1032) X,Y,W
  330 CONTINUE
c
C  Test of error returns
c
      WRITE (IOUT,1033)
      X = XMIN
      WRITE (IOUT,1035) X
      Y = BESY0(X)
      WRITE (IOUT,1036) Y
      X = ZERO
      WRITE (IOUT,1034) X
      Y = BESY0(X)
      WRITE (IOUT,1036) Y
      X = XMAX
      WRITE (IOUT,1034) X
      Y = BESY0(X)
      WRITE (IOUT,1036) Y
      WRITE (IOUT,1100)
      return
 1000 FORMAT('1Test of Y0(X) VS Multiplication Theorem'  //)
 1010 FORMAT(I7,' random arguments were tested from the interval ',
     1 1H(,F5.1,1H,,F5.1,1H)//)
 1011 FORMAT(' ABS(Y0(X)) was larger',I6,' times', /
     1   15X,' agreed',I6,' times, and'/
     1   11X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.' //)
 1021 FORMAT(' The maximum relative error of',E15.4,3H = ,I4,3H **,
     1  F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1  ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    3H = ,I4,3H **,F7.2)
 1024 FORMAT(' The maximum absolute error of',E15.4,3H = ,I4,3H **,
     1  F7.2/4X,'occurred for X =',E13.6)
 1025 FORMAT(' The root mean square absolute error was',E15.4,
     1    3H = ,I4,3H **,F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Accuracy near zeros'//10X,'X',15X,'BESY0(X)',
     1    13X,'Loss of base',I3,' digits'/)
 1032 FORMAT(E20.10,E25.15,8X,F7.2/)
 1033 FORMAT(//' Test with extreme arguments'/)
 1034 FORMAT(' Y0 will be called with the argument ',E17.10/
     1     ' This may stop execution.'//)
 1035 FORMAT(' Y0 will be called with the argument ',E17.10/
     1     ' This should not stop execution.'//)
 1036 FORMAT(' Y0 returned the value',E25.17/)
 1100 FORMAT(' This concludes the tests.')
      END
      subroutine y1test ( )

c*********************************************************************72
c
cc Y1TEST tests BESY1.
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - an environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following six
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MAXEXP - the smallest integer such that 
C                          FLOAT(IBETA)**MAXEXP causes overflow
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 XMIN   - the smallest positive normalized
C                          floating-point power of the radix
C                 XMAX   - the largest finite floating-point
C                          number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, FLOAT, LOG, MAX, SIGN, SQRT
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C  Latest modification: March 13, 1992
C
C  Authors: George Zazi, W. J. Cody
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
      implicit none

      LOGICAL SFLAG, TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MB,MBM1,MBP1,MINEXP,N,NDUM,NEGEP,NGRD
      DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,B,BESY0,BESY1,BETA,C,CONV,DEL,EIGHT,
     2   EPS,EPSNEG,FOUR,HALF,HUND,ONE,REN,R6,R7,SIXTEN,SUM,T,
     3   TWENTY,TWO56,T1,U,W,X,XI,XL,XLAM,XMAX,XMB,XMIN,X1,Y,YX,
     4   Z,ZERO,ZZ
      DIMENSION U(560),XI(2),YX(2)
c
C  Mathematical constants
c
      DATA IOUT/6/
      DATA ZERO,ONE,FOUR/0.0D0,1.0D0,4.0E0/,
     1   EIGHT,TWENTY,ALL9,TWO56/8.0D0,20.0D0,-999.0D0,256.0D0/,
     2   HALF,SIXTEN,XLAM,HUND/0.5D0,16.0D0,0.9375D0,100.0D0/
c
C  Zeroes of Y1
c
      DATA XI/562.0D0,1390.0D0/
      DATA YX(1)/-9.5282393097722703132D-4/,
     1     YX(2)/-2.1981830080598002741D-6/
c
C  Statement functions for conversion between integer and float
c
      CONV(NDUM) = DBLE(FLOAT(NDUM))
c
C  Determine machine parameters and set constants
c
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      JT = 0
      B = EPS
c
C  Random argument accuracy tests (based on the
C    multiplication theorem)
c
      DO 300 J = 1, 3
         SFLAG = (J .EQ. 1) .AND. (MAXEXP/IT .LE. 5)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         A = B
         IF (J .EQ. 1) THEN
               B = FOUR
            ELSE IF (J .EQ. 2) THEN
               B = EIGHT
            ELSE
               B = TWENTY
               N = 500
         END IF
         DEL = (B - A) / CONV(N)
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
c
C  Carefully purify arguments and evaluate identities
c
            Y = X/XLAM
            W = SIXTEN * Y
            Y = (W + Y) - W
            X = Y * XLAM
            U(1) = BESY0(Y)
            U(2) = BESY1(Y)
            TFLAG = SFLAG .AND. (Y .LT. HALF)
            IF (TFLAG) THEN
               U(1) = U(1) * EPS
               U(2) = U(2) * EPS
            END IF
            MB = 1
            XMB = ONE
            Y = Y * HALF
            W = (ONE-XLAM)*(ONE+XLAM)
            C = W * Y
            T = ABS(XLAM*U(2))
            T1 = EPS/HUND
            DO 110 II = 3, 60
               U(II) = XMB/Y * U(II-1) - U(II-2)
               T1 = XMB * T1 / C
               XMB = XMB + ONE
               MB = MB + 1
               MBP1=MB + 1
               Z = ABS(U(II))
               IF (Z/T1 .LT. T) THEN
                     GO TO 120
                  ELSE IF (Y .LT. XMB) THEN
                     IF (Z .GT. XMAX*(Y/XMB)) THEN
                        A= X
                        XL=XL+DEL
                        GO TO  200
                     END IF
               END IF
               IF (T1 .GT. ONE/EPS) THEN
                  T = T * T1
                  T1 = ONE
               END IF
  110       CONTINUE
  120       SUM = U(MBP1)
            IND = MBP1
            MBM1 =MB-1
            DO 155 II = 1, MBM1
               IND = IND - 1
               XMB = XMB - ONE
               SUM = SUM * W * Y / XMB + U(IND)
  155       CONTINUE
            ZZ = SUM*XLAM
            IF (TFLAG) ZZ = ZZ / EPS
            Z = BESY1(X)
            Y = Z
            IF (ABS(U(2)) .GT. ABS(Y)) Y =  U(2)
c
C  Accumulate results
c
            W = (Z - ZZ) / Y
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
               ELSE
                  K2 = K2 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
               R6 = W
               X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
c
C  Process and output statistics
c
         N = K1 + K2 + K3
         R7 = SQRT(R7/CONV(N))
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = ALL9
         END IF
         IF (J .EQ. 4) THEN
               WRITE (IOUT,1024) R6,IBETA,W,X1
            ELSE
               WRITE (IOUT,1021) R6,IBETA,W,X1
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = ALL9
         END IF
         IF (J .EQ. 4) THEN
               WRITE (IOUT,1025) R7,IBETA,W
            ELSE
               WRITE (IOUT,1023) R7,IBETA,W
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
  300 CONTINUE
c
C  Special tests
c
      WRITE (IOUT,1030)
      WRITE (IOUT,1031) IBETA
      DO 330 I = 1, 2  
         X = XI(I)/TWO56
         Y = BESY1(X)
         T = (Y-YX(I))/YX(I)
         IF (T .NE. ZERO) THEN
               W = LOG(ABS(T))/ALBETA
            ELSE
               W = ALL9
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1032) X,Y,W
  330 CONTINUE
c
C  Test of error returns
c
      WRITE (IOUT,1033)
      X = XMIN
      IF (XMIN*XMAX .LT. ONE) THEN
            WRITE (IOUT,1034) X
         ELSE
            WRITE (IOUT,1035) X
      END IF
      Y = BESY1(X)
      WRITE (IOUT,1036) Y
      X = -ONE
      WRITE (IOUT,1034) X
      Y = BESY1(X)
      WRITE (IOUT,1036) Y
      X = XMAX
      WRITE (IOUT,1034) X
      Y = BESY1(X)
      WRITE (IOUT,1036) Y
      WRITE (IOUT,1100)
      return
c
 1000 FORMAT('1Test of Y1(X) VS Multiplication Theorem'  //)
 1010 FORMAT(I7,' random arguments were tested from the interval ',
     1 1H(,F5.1,1H,,F5.1,1H)//)
 1011 FORMAT(' ABS(Y1(X)) was larger',I6,' times', /
     1     15X,' agreed',I6,' times, and'/
     1   11X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.' //)
 1021 FORMAT(' The maximum relative error of',E15.4,3H = ,I4,3H **,
     1  F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1  ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    3H = ,I4,3H **,F7.2)
 1024 FORMAT(' The maximum absolute error of',E15.4,3H = ,I4,3H **,
     1  F7.2/4X,'occurred for X =',E13.6)
 1025 FORMAT(' The root mean square absolute error was',E15.4,
     1    3H = ,I4,3H **,F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Accuracy near zeros'//10X,'X',15X,'BESY1(X)',
     1    13X,'Loss of base',I3,' digits'/)
 1032 FORMAT(E20.10,E25.15,8X,F7.2/)
 1033 FORMAT(//' Test with extreme arguments'///)
 1034 FORMAT(' Y1 will be called with the argument ',E17.10/
     1     ' This may stop execution.'//)
 1035 FORMAT(' Y1 will be called with the argument ',E17.10/
     1     ' This should not stop execution.'//)
 1036 FORMAT(' Y1 returned the value',E25.17/)
 1100 FORMAT(' This concludes the tests.')
      END
