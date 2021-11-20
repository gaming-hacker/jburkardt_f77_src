      SUBROUTINE FLFACTOR ( A, N, D, W, IPIVOT, IFLAG ) 
C  ADJUSTED TO ROUND ALL CALCULATIONS, USING FL .     
      DIMENSION A(N,N), W(N,N),IPIVOT(N),D(N)   
C******  I N P U T  ******    
C  A  THE MATRIX OF ORDER N TO BE FACTORED
C  N  THE ORDER OF THE MATRIX 
C******  W O R K   A R E A  ******  
C  D  A REAL VECTOR OF LENGTH N, TO HOLD ROW SIZES    
C******  O U T P U T  ******  
C  W  ARRAY OF SIZE (N,N) CONTAINING THE LU FACTORIZATION OF  P*A  FOR  
C     SOME PERMUTATION MATRIX  P  SPECIFIED BY  IPIVOT . IF THERE IS NO 
C     FURTHER NEED FOR  A  AND STORAGE IS SCARCE, THEN  A  ITSELF CAN BE
C     USED FOR  W  IN THE CALLING SEQUENCE .    
C  IPIVOT  INTEGER VECTOR OF LENGTH  N  CONTAINING THE PIVOTING STRATEGY
C     USED. 
C  IFLAG   AN INTEGER,  
C       = 1, IF AN EVEN NUMBER OF INTERCHANGES WAS CARRIED OUT    
C       = -1, IF AN ODD NUMBER OF INTERCHANGES WAS CARRIED OUT    
C       = 0, IF THE UPPER TRIANGULAR FACTOR HAS ONE OR MORE ZERO  
C            DIAGONAL ENTRIES 
C     THUS, DET(A) = IFLAG*W(1,1)*...*W(N,N)    
C     IF IFLAG .NE. 0, THEN THE LINEAR SYSTEM A*X = B CAN BE SOLVED     
C     FOR X BY A  
C         CALL SUBST (W, IPIVOT, B, N, X )
C******  M E T H O D  ******  
C  THE PROGRAM FOLLOWS ALGORITHM 4.2, USING SCALED PARTIAL PIVOTING     
C     
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c
      IFLAG = 1   
C  INITIALIZE W, IPIVOT, D    
      DO 9 I=1,N  
         IPIVOT(I) = I  
         ROWMAX = 0.    
         DO 5 J=1,N     
            W(I,J) = A(I,J)   
    5       ROWMAX = AMAX1(ROWMAX,ABS(W(I,J)))  
         IF (ROWMAX .GT. 0.)            GO TO 9 
         IFLAG = 0
                                        RETURN  
    9    D(I) = ROWMAX  
      NM1 = N - 1 
      IF (NM1 .EQ. 0)                   RETURN  
C  FACTORIZATION  
      DO 20 K=1,NM1     
c  12    continue
         COLMAX = ABS(W(K,K))/D(K)  
         ISTAR = K
         KP1 = K + 1    
         DO 13 I=KP1,N  
            AWIKOD = ABS(W(I,K))/D(I)     
            IF (AWIKOD .LE. COLMAX)     GO TO 13
            COLMAX = AWIKOD   
            ISTAR = I   
   13       CONTINUE    
         IF (COLMAX .GT. 0.)            GO TO 14
         IFLAG = 0
                                        RETURN  
   14    IF (ISTAR .EQ. K)              GO TO 16
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
   15       W(K,J) = TEMP     
   16    DO 20 I=KP1,N  
            W(I,K) = FL(W(I,K)/W(K,K))    
            RATIO = W(I,K)    
            DO 20 J=KP1,N     
   20          W(I,J) = FL(W(I,J) - FL(RATIO*W(K,J))) 
      IF (W(N,N) .EQ. 0.)   IFLAG = 0     
                                        RETURN  
      END   
