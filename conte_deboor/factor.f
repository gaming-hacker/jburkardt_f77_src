      SUBROUTINE FACTOR ( W, N, D, IPIVOT, IFLAG )    
      INTEGER IFLAG,IPIVOT(N),   I,ISTAR,J,K    
      REAL D(N),W(N,N),   AWIKOD,COLMAX,RATIO,ROWMAX,TEMP   
C******  I N P U T  ******    
C  W  ARRAY OF SIZE (N,N) CONTAINING THE MATRIX  A  OF ORDER  N  TO BE  
C     FACTORED.   
C  N  THE ORDER OF THE MATRIX 
C******  W O R K   A R E A  ******  
C  D  A REAL VECTOR OF LENGTH N, TO HOLD ROW SIZES    
C******  O U T P U T  ******  
C  W  ARRAY OF SIZE (N,N) CONTAINING THE LU FACTORIZATION OF  P*A  FOR  
C     SOME PERMUTATION MATRIX  P  SPECIFIED BY  IPIVOT .    
C  IPIVOT  INTEGER VECTOR OF LENGTH  N  INDICATING THAT ROW  IPIVOT(K)  
C     WAS USED TO ELIMINATE  X(K) , K=1,...,N . 
C  IFLAG   AN INTEGER,  
C       = 1, IF AN EVEN NUMBER OF INTERCHANGES WAS CARRIED OUT,   
C       = -1, IF AN ODD NUMBER OF INTERCHANGES WAS CARRIED OUT,   
C       = 0, IF THE UPPER TRIANGULAR FACTOR HAS ONE OR MORE ZERO DIA-   
C            GONAL ENTRIES.   
C     THUS, DETERMINANT(A) = IFLAG*W(1,1)*...*W(N,N) .
C     IF IFLAG .NE. 0, THEN THE LINEAR SYSTEM A*X = B CAN BE SOLVED FOR 
C      X  BY A    
C         CALL SUBST (W, IPIVOT, B, N, X )
C******  M E T H O D  ******  
C  THE PROGRAM FOLLOWS ALGORITHM 4.2, USING SCALED PARTIAL PIVOTING.    
C     
      IFLAG = 1   
C              INITIALIZE IPIVOT, D 
      DO 9 I=1,N  
         IPIVOT(I) = I  
         ROWMAX = 0.    
         DO 5 J=1,N     
    5       ROWMAX = AMAX1(ROWMAX,ABS(W(I,J)))  
         IF (ROWMAX .EQ. 0.) THEN   
            IFLAG = 0   
            ROWMAX = 1. 
         END IF   
    9    D(I) = ROWMAX  
      IF (N .LE. 1)                     RETURN  
C              FACTORIZATION  
      DO 20 K=1,N-1     
C                 DETERMINE PIVOT ROW, THE ROW  ISTAR .     
         COLMAX = ABS(W(K,K))/D(K)  
         ISTAR = K
         DO 13 I=K+1,N  
            AWIKOD = ABS(W(I,K))/D(I)     
            IF (AWIKOD .GT. COLMAX) THEN  
               COLMAX = AWIKOD
               ISTAR = I
            END IF
   13    CONTINUE 
         IF (COLMAX .EQ. 0.)  THEN  
            IFLAG = 0   
         ELSE     
            IF (ISTAR .GT. K)  THEN 
C                 MAKE  K  THE PIVOT ROW  BY INTERCHANGING IT WITH
C                 THE CHOSEN ROW  ISTAR . 
               IFLAG = -IFLAG 
               I = IPIVOT(ISTAR)    
               IPIVOT(ISTAR) = IPIVOT(K)  
               IPIVOT(K) = I  
               TEMP = D(ISTAR)
               D(ISTAR) = D(K)
               D(K) = TEMP    
               DO 15 J=1,N    
                  TEMP = W(ISTAR,J) 
                  W(ISTAR,J) = W(K,J)     
   15             W(K,J) = TEMP     
            END IF
C                 ELIMINATE  X(K)  FROM ROWS  K+1,...,N .  
c  16       continue 
            DO 19 I=K+1,N     
               W(I,K) = W(I,K)/W(K,K)     
               RATIO = W(I,K) 
               DO 19 J=K+1,N  
                  W(I,J) = W(I,J) - RATIO*W(K,J)
   19       CONTINUE    
         END IF   
   20 CONTINUE    
      IF (W(N,N) .EQ. 0.)   IFLAG = 0     
                                        RETURN  
      END   
