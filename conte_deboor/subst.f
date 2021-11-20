      SUBROUTINE SUBST ( W, IPIVOT, B, N, X )   
      INTEGER IPIVOT(N),   I,IP,J   
      REAL B(N),W(N,N),X(N),   SUM  
C******  I N P U T  ******    
C  W, IPIVOT, N  ARE AS ON OUTPUT FROM  F A C T O R , APPLIED TO THE    
C     MATRIX  A  OF ORDER  N .
C  B  IS AN N-VECTOR, GIVING THE RIGHT SIDE OF THE SYSTEM TO BE SOLVED. 
C******  O U T P U T  ******  
C  X  IS THE N-VECTOR SATISFYING  A*X  =  B .   
C******  M E T H O D  ******  
C  ALGORITHM 4.4 IS USED, I.E., THE FACTORIZATION OF  A  CONTAINED IN   
C  W  AND  IPIVOT  (AS GENERATED IN  FACTOR ) IS USED TO SOLVE  A*X = B 
C  FOR  X  BY SOLVING TWO TRIANGULAR SYSTEMS.     
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c
      IF (N .LE. 1) THEN
         X(1) = B(1)/W(1,1)   
                                        RETURN  
      END IF
      IP = IPIVOT(1)    
      X(1) = B(IP)
      DO 15 I=2,N 
         SUM = 0. 
         DO 14 J=1,I-1  
   14       SUM = W(I,J)*X(J) + SUM 
         IP = IPIVOT(I) 
   15    X(I) = B(IP) - SUM   
C     
      X(N) = X(N)/W(N,N)
      DO 20 I=N-1,1,-1  
         SUM = 0. 
         DO 19 J=I+1,N  
   19       SUM = W(I,J)*X(J) + SUM 
   20    X(I) = (X(I) - SUM)/W(I,I) 
                                        RETURN  
      END   
