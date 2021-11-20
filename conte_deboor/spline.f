      SUBROUTINE SPLINE ( XI, C, N )
      PARAMETER ( NP1MAX = 50 )     
      INTEGER N,   M    
      REAL C(4,N+1),XI(N+1),   D(NP1MAX),DIAG(NP1MAX),G     
C******  I N P U T  ******    
C  XI(1), ..., XI(N+1)  STRICTLY INCREASING SEQUENCE OF BREAKPOINTS     
C  C(1,I), C(2,I), VALUE AND FIRST DERIVATIVE AT  XI(I), I=1,...,N+1,   
C     OF THE CUBIC SPLINE.    
C******  O U T P U T  ******  
C  C(1,I), C(2,I), C(3,I), C(4,I)  POLYNOMIAL COEFFICIENTS OF THE SPLINE
C        ON THE INTERVAL  (XI(I), XI(I+1)) ,  I=1,...,N .  
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c 
      DATA DIAG(1),D(1) /1.,0./     
      DO 10 M=2,N+1     
         D(M) = XI(M) - XI(M-1)     
   10    DIAG(M) = (C(1,M) - C(1,M-1))/D(M)     
      DO 20 M=2,N 
         C(2,M) = 3.*(D(M)*DIAG(M+1) + D(M+1)*DIAG(M))
   20    DIAG(M) = 2.*(D(M) + D(M+1))     
      DO 30 M=2,N 
         G = -D(M+1)/DIAG(M-1)
         DIAG(M) = DIAG(M) + G*D(M-1)     
   30    C(2,M) = C(2,M) + G*C(2,M-1)     
      DO 40 M=N,2,-1    
   40    C(2,M) = (C(2,M) - D(M)*C(2,M+1))/DIAG(M)  
  
      RETURN   
      END   
