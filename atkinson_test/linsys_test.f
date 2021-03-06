      program linsys_test

C     TITLE: THIS IS A DEMO PROGRAM FOR SUBROUTINE LINSYS.
C
C     IT WILL SOLVE A LINEAR SYSTEM A*X=B, GIVEN BY THE USER.
C
      PARAMETER (MAXN=20)
      DIMENSION A(MAXN,MAXN), B(MAXN)
C
C     INPUT ORDER OF LINEAR SYSTEM.
10    PRINT *, ' GIVE THE ORDER OF THE LINEAR SYSTEM.'
      PRINT *, ' THE UPPER BOUND ON THE ORDER IS ', MAXN
      PRINT *, ' GIVE ZERO TO STOP.'
      READ *, N
      IF(N .EQ. 0) STOP
C
C     INPUT LINEAR SYSTEM.
      PRINT *, ' GIVE THE COEFFICIENTS OF THE LINEAR SYSTEM,'
      PRINT *, ' ONE EQUATION AT A TIME.'
      PRINT *, ' CONCLUDE EACH EQUATION WITH ITS RIGHT-HAND CONSTANT.'
      DO 20 I=1,N
        PRINT *, ' GIVE COEFFICIENTS OF EQUATION',I 
20      READ *, (A(I,J), J=1,N), B(I)
C
C     SOLVE THE LINEAR SYSTEM.
      CALL LINSYS(A,B,N,MAX_N,IER)
C
C     PRINT THE RESULTS.
      PRINT 1000, N, IER
1000  FORMAT(///,' N=',I2,5X,'IER=',I3,//,'  I       SOLUTION',/)
      PRINT 1001, (I,B(I), I=1,N)
1001  FORMAT(I3,1PE20.10)
      GO TO 10
      END

      SUBROUTINE LINSYS(MAT,B,N,MD,IER)
C     -----------------
C
C     THIS ROUTINE SOLVES A SYSTEM OF LINEAR EQUATIONS
C             A*X = B
C     THE METHOD USED IS GAUSSIAN ELIMINATION WITH
C     PARTIAL PIVOTING.
C
C     INPUT:
C     THE COEFFICIENT MATRIX  A  IS STORED IN THE ARRAY MAT.
C     THE RIGHT SIDE CONSTANTS ARE IN THE ARRAY  B.
C     THE ORDER OF THE LINEAR SYSTEM IS  N.
C     THE VARIABLE  MD  IS THE NUMBER OF ROWS THAT  MAT
C     IS DIMENSIONED AS HAVING IN THE CALLING PROGRAM.
C     THE SIZE OF  MAXPIV, GIVEN BELOW, MUST BE GREATER
C     THAN  N.  IF NOT, IT IS A FATAL ERROR.
C
C     OUTPUT:
C     THE ARRAY  B  CONTAINS THE SOLUTION  X.
C     MAT CONTAINS THE UPPER TRIANGULAR MATRIX  U
C     OBTAINED BY ELIMINATION.  THE ROW MULTIPLIERS
C     USED IN THE ELIMINATION ARE STORED IN THE
C     LOWER TRIANGULAR PART OF  MAT.
C     IER=0 MEANS THE MATRIX  A  WAS COMPUTATIONALLY
C     NONSINGULAR, AND THE GAUSSIAN ELIMINATION
C     WAS COMPLETED SATISFACTORILY.
C     IER=1 MEANS THAT THE MATRIX  A  WAS
C     COMPUTATIONALLY SINGULAR.
C
      INTEGER PIVOT
      REAL MULT, MAT
      PARAMETER(MAXPIV=100)
      DIMENSION MAT(MD,*), B(*), PIVOT(MAXPIV)
C
C     CHECK SIZE OF N VERSUS MAXPIV
      IF(MAXPIV .LT. N) THEN
          PRINT *, 'THE VARIABLE MAXPIV IN SUBROUTINE LINSYS MUST BE'
          PRINT *, 'INCREASED.  THIS IS A FATAL ERROR.'
          STOP
      END IF
C
C     BEGIN ELIMINATION STEPS.
      DO 40 K=1,N-1
C       CHOOSE PIVOT ROW.
        PIVOT(K) = K
        AMAX = ABS(MAT(K,K))
        DO 10 I=K+1,N
          ABSA = ABS(MAT(I,K))
          IF(ABSA .GT. AMAX) THEN
              PIVOT(K) = I
              AMAX = ABSA
          END IF
10        CONTINUE
C
        IF(AMAX .EQ. 0.0) THEN
C           COEFFICIENT MATRIX IS SINGULAR.
            IER = 1
            RETURN
        END IF
C
        IF(PIVOT(K) .NE. K) THEN
C           SWITCH ROWS K AND PIVOT(K).
            I = PIVOT(K)
            TEMP = B(K)
            B(K) = B(I)
            B(I) = TEMP
            DO 20 J=K,N
              TEMP = MAT(K,J)
              MAT(K,J) = MAT(I,J)
20            MAT(I,J) = TEMP
        END IF
C
C       PERFORM STEP #K OF ELIMINATION.
        DO 30 I=K+1,N
          MULT = MAT(I,K)/MAT(K,K)
          MAT(I,K) = MULT
          B(I) = B(I) - MULT*B(K)
          DO 30 J=K+1,N
30          MAT(I,J) = MAT(I,J) - MULT*MAT(K,J)
40        CONTINUE
C
      IF(MAT(N,N) .EQ. 0.0) THEN
C         COEFFICIENT MATRIX IS SINGULAR.
          IER = 1
          RETURN
      END IF
C
C     SOLVE FOR SOLUTION X USING BACK SUBSTITUTION.
      DO 60 I=N,1,-1
        SUM = 0.0
        DO 50 J=I+1,N
50        SUM = SUM + MAT(I,J)*B(J)
60      B(I) = (B(I) - SUM)/MAT(I,I)
      IER = 0
      RETURN
      END
