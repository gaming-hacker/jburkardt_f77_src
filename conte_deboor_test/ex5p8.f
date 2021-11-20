C  EXAMPLE 5.8  GAUSS-SEIDEL ITERATION APPLIED TO SOLVING   
C     
C       X(I-1)**2 + X(I-1)*X(I+1) + X(I+1)**2  =  3*X(I)**2 , I=1,...,N 
C             X(O) = 0 ,  X(N+1) = 1
C     
      PARAMETER (N=3)
      parameter ( NP2 = N+2 )
      INTEGER I,ITER    
      REAL CHANGE(N),FNP1,X(NP2),XL1,XNEW 
C                           INITIAL GUESS 
      FNP1 = N+1  
      DO 10 I=1,NP2     
   10    X(I) = FLOAT(I-1)/FNP1     
C     
C                           ITERATION STARTS HERE     
      DO 30 ITER=1,30   
         XL1 = 0. 
         DO 20 I=1,N    
            XNEW = SQRT((X(I)**2 + X(I)*X(I+2) + X(I+2)**2)/3.)   
            CHANGE(I) = XNEW - X(I+1)     
            XL1 = XL1 + ABS(CHANGE(I))    
   20       X(I+1) = XNEW     
C     
         PRINT 620, ITER,XL1,(I,X(I+1),CHANGE(I),I=1,N)     
  620    FORMAT(' AT ITERATION',I3,', CHANGE =',E10.3/(I3,F15.9,E15.6)) 
C     
C                           CHECK FOR SMALL CORRECTION
         IF (XL1 .LT. 1.E-7)            STOP    
   30    CONTINUE 
      END   
