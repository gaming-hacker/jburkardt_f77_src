      SUBROUTINE HSBERG ( H, N )    
C  CONSTRUCTS AN UPPER HESSENBERG MATRIX SIMILAR TO THE INPUT MATRIX    
C  H, USING HOUSEHOLDER REFLECTIONS. INFORMATION ABOUT THESE REFLECTIONS
C  IS STORED DIRECTLY IN  H , USING ALSO COLUMNS  H(.,N+1) AND H(.,N+2) 
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c
      INTEGER N, I,J,K  
      REAL H(N,N),  ALPHIN,BETA,GAMMA     
      DO 100 K=1,N-2    
         BETA = 0.
         DO 10 I=K+1,N  
   10       BETA = BETA + H(I,K)**2 
         BETA = SIGN(1.,H(K+1,K))*SQRT(BETA)    
         H(K+1,K) = H(K+1,K) + BETA 
         ALPHIN = BETA*H(K+1,K)     
         DO 29 J=1,N    
            GAMMA = 0.  
            DO 21 I=K+1,N     
   21          GAMMA = GAMMA + H(I,K)*H(J,I)    
            GAMMA = GAMMA/ALPHIN    
            DO 25 I=K+1,N     
   25          H(J,I) = H(J,I) - GAMMA*H(I,K)   
   29    CONTINUE 
         DO 39 J=K+1,N  
            GAMMA = 0.  
            DO 31 I=K+1,N     
   31          GAMMA = GAMMA + H(I,K)*H(I,J)    
            GAMMA = GAMMA/ALPHIN    
            DO 35 I=K+1,N     
   35          H(I,J) = H(I,J) - GAMMA*H(I,K)   
   39    CONTINUE 
         H(K+1,N+1) = H(K+1,K)
         H(K+1,K) = -BETA     
         H(K+1,N+2) = ALPHIN  
  100 CONTINUE    
                                        RETURN  
      END   
