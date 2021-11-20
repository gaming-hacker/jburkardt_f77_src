      program euler_test

C     TITLE: DEMONSTRATION OF EULER'S METHOD.
C
C     THIS SOLVES THE INITIAL VALUE PROBLEM
C        Y'(X) = F(X,Y(X)) ,  X0 .LE. X .LE. B ,  Y(X0)=Y0.
C     THE FUNCTION F(X,Z) IS DEFINED BELOW, ALONG WITH THE TRUE
C     SOLUTION Y(X).  THE NUMBER OF THE PROBLEM TO BE SOLVED
C     IS SPECIFIED BY THE INPUT VARIABLE 'NUMDE', WHICH IS USED
C     IN THE FUNCTIONS 'F' AND 'Y'.  THE PROGRAM WILL REQUEST
C     THE PROBLEM PARAMETERS, ALONG WITH THE VALUES OF 'H' AND
C     'IPRINT'.  'H' IS THE STEPSIZE, AND 'IPRINT' IS THE NUMBER
C     OF STEPS BETWEEN EACH PRINTING OF THE SOLUTION.
C     USE H=0 AND NUMDE=0 TO STOP THE PROGRAM.
C      
      PARAMETER (ZERO=0.0)
      COMMON/BLOCKF/NUMDE
C
C     INPUT PROBLEM PARAMETERS.
10    PRINT *, ' NUMDE = ?' 
      PRINT *, ' GIVE ZERO TO STOP.'
      READ *, NUMDE
      IF(NUMDE .EQ. 0) STOP
      PRINT *, ' GIVE X0, B, AND Y0.'
      READ *, XZERO, B, YZERO
C
20    PRINT *, ' GIVE H AND IPRINT.'
      READ *, H, IPRINT
      IF(H .EQ. ZERO) GO TO 10
C
C     INITIALIZE.
      X0 = XZERO
      Y0 = YZERO
      PRINT 1000, NUMDE, XZERO, B, YZERO, H, IPRINT
1000  FORMAT(//,' EQUATION',I2,5X,'XZERO =',1PE9.2,5X,'B =',E9.2,
     *       5X,'YZERO =',E12.5,/,' STEPSIZE =',E10.3,5X,
     *       'PRINT PARAMETER =',I3,/)
C
C     BEGIN THE MAIN LOOP FOR COMPUTING THE SOLUTION OF
C     THE DIFFERENTIAL EQUATION.
30    DO 40 K=1,IPRINT
        X1 = X0 + H
        IF(X1 .GT. B) GO TO 20
        Y1 = Y0 + H*F(X0,Y0)
        X0 = X1
40      Y0 = Y1
C
C     CALCULATE ERROR AND PRINT RESULTS.
      TRUE = Y(X1)
      ERROR = TRUE - Y1
      PRINT 1001, X1, Y1, ERROR
1001  FORMAT(' X =',1PE10.3,5X,'Y(X) =',E17.10,5X,'ERROR =',E9.2)
      GO TO 30
      END

      FUNCTION F(X,Z) 
C     ----------
C
C     THIS DEFINES THE RIGHT SIDE OF
C     THE DIFFERENTIAL EQUATION.
C 
      PARAMETER (ONE=1.0, TWO=2.0)
      COMMON/BLOCKF/NUMDE
C
      GO TO (10,20,30), NUMDE
10    F = -Z
      RETURN
20    F = (Z + X*X - TWO)/(X + ONE)
      RETURN
30    F = COS(Z)**2
      RETURN
      END

      FUNCTION Y(X) 
C     ----------
C
C     THIS GIVES THE TRUE SOLUTION OF
C     THE INITIAL VALUE PROBLEM.
C    
      PARAMETER (ONE=1.0, TWO=2.0)
      COMMON/BLOCKF/NUMDE
C
      GO TO(10,20,30), NUMDE
10    Y = EXP(-X)
      RETURN
20    X1=X + ONE
      Y = X*X - TWO*(X1*LOG(X1) - X1)
      RETURN
30    Y = ATAN(X)
      RETURN
      END