      program divdif_test
c
C     TITLE: NEWTON DIVIDED DIFFERENCE INTERPOLATING POLYNOMIAL
C
C     THIS ROUTINE WILL DO INTERPOLATION OF  FCN(X), DEFINED BELOW.
C     THE PROGRAM READS THE DEGREE  N  AND AN INITIAL POINT  X(0).
C     THEN THE DIVIDED DIFFERENCES
C              F[X(0),X(1),...,X(J)] ,  1 .LE. J .LE. N
C     ARE CONSTRUCTED FOR USE IN EVALUATING THE INTERPOLATING
C     POLYNOMIAL.  THE INTERPOLATION NODES X(J) ARE EVENLY SPACED.
C
C     FOLLOWING THE ABOVE, AN X AT WHICH THE INTERPOLATION IS TO BE
C     DONE IS REQUESTED FROM THE USER.  FOR THIS X, THE
C     INTERPOLATING POLYNOMIALS OF DEGREE .LE. N ARE
C     CONSTRUCTED, ALONG WITH THE ERROR IN THE INTERPOLATION.
C     THE INTERPOLATION IS CARRIED OUT WITH THE NEWTON DIVIDED
C     DIFFERENCE FORM OF THE INTERPOLATING POLYNOMIAL.
C
      CHARACTER Q
      PARAMETER (MAXN=20)
      DIMENSION X(0:MAXN), F(0:MAXN), DF(0:MAXN)
C
      FCN(X) = COS(X)
C
C     INPUT PROBLEM PARAMETERS.
      PRINT *, ' DEGREE, X(0), H = ?'
      PRINT *, ' THE DEGREE SHOULD BE .LE. ', MAXN
      READ *, N, X(0), H
C
C     SET UP NODE AND FUNCTION VALUES.
      DO 10 I=0,N
        X(I) = X(0) + I*H
        F(I) = FCN(X(I))
10      DF(I) = F(I)
C
C     CALCULATE DIVIDED DIFFERENCES AND PRINT THEM.
      CALL DIVDIF(N,X,DF)
      PRINT 1000
1000  FORMAT('  I',4X,'X(I)',6X,'F(I)',8X,'DF(I)',/)
      DO 20 I=0,N
        PRINT 1001, I, X(I), F(I), DF(I)
20      CONTINUE
1001  FORMAT(I3,4X,F3.1,4X,F8.6,4X,E14.7)
C
C     BEGIN INTERPOLATION LOOP.
30    PRINT *, ' GIVE INTERPOLATION POINT Z.'
      READ *, Z
      PRINT 1002, Z
1002  FORMAT(///,' X=',F9.7,/)
C
C     CONSTRUCT INTERPOLATION POLYNOMIAL AT Z, FOR
C     ALL DEGREES I .LE. N.
      DO 50 I=1,N
        POLY = DF(I)
        DO 40 J=I-1,0,-1
40        POLY = (Z - X(J))*POLY + DF(J)
        ERROR = FCN(Z) - POLY
        PRINT 1003, I, POLY, ERROR
50      CONTINUE
1003  FORMAT(' DEGREE=',I2,4X,'INTERPOLANT=',F10.7,4X,
     *       'ERROR=',E10.3)
C
      PRINT *, ' ANOTHER INTERPOLATION POINT X? (Y/N)'
      READ (*,'(A1)') Q
      IF((Q .EQ. 'N') .OR. (Q .EQ. 'n')) THEN
          STOP
      ELSE
          GO TO 30
      END IF 
      END
