      SUBROUTINE FLSUBST ( W, IPIVOT, B, N, X )   
C  ADJUSTED TO ROUND ALL CALCULATIONS, USING  FL .    
      DIMENSION W(N,N),IPIVOT(N),B(N), X(N)     
C******  I N P U T  ******    
C  W, IPIVOT, N  ARE AS ON OUTPUT FROM FACTOR, APPLIED TO THE     
C     MATRIX  A  OF ORDER  N .
C  B  IS AN N-VECTOR, GIVING THE RIGHT SIDE OF THE SYSTEM TO BE SOLVED  
C******  O U T P U T  ******  
C  X  IS THE N-VECTOR SATISFYING  A*X  =  B     
C******  M E T H O D  ******  
C  ALGORITHM 4.4 IS USED, I.E., THE FACTORIZATION OF  A  CONTAINED IN   
C  W  AND  IPIVOT  (AS GENERATED IN  FACTOR ) IS USED TO SOLVE  A*X = B 
C  FOR  X  BY SOLVING TWO TRIANGULAR SYSTEMS.   
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
      IF (N .GT. 1)                     GO TO 10
      X(1) = FL(B(1)/W(1,1))  
                                        RETURN  
   10 IP = IPIVOT(1)    
      X(1) = B(IP)
      DO 15 I=2,N 
         IM1 = I - 1    
         SUM = 0. 
         DO 14 J=1,IM1  
   14       SUM = FL(FL(W(I,J)*X(J)) + SUM)     
         IP = IPIVOT(I) 
   15    X(I) = FL(B(IP) - SUM)     
C     
      X(N) = FL(X(N)/W(N,N))  
      I = N 
      DO 20 NP1MI=2,N   
         IP1 = I  
         I = I - 1
         SUM = 0. 
         DO 19 J=IP1,N  
   19       SUM = FL(FL(W(I,J)*X(J)) + SUM)     
   20    X(I) = FL(FL(X(I) - SUM)/W(I,I)) 
                                        RETURN  
      END   
