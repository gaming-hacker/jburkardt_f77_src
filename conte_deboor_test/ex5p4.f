C  EXAMPLE 5.4  NEWTON,S METHOD APPPIED TO SOLVING    
C     
C       X(I-1)**2 + X(I-1)*X(I+1) + X(I+1)**2  =  3*X(I)**2 , I=1,...,N 
C             X(O) = 0 ,  X(N+1) = 1
C     
      PARAMETER (N=3)
      parameter ( NP2 = N+2 )
      INTEGER I,ITER    
      REAL F(NP2),DIAG(N),SUB(N),SUP(N),X(NP2)  
C                           INITIAL GUESS 
      FNP1 = N+1  
      DO 10 I=1,NP2     
   10    X(I) = FLOAT(I-1)/FNP1     
C     
C                           ITERATION STARTS HERE     
      DO 30 ITER=1,10   
         FL1 = 0. 
         DO 20 I=1,N    
            SUB(I) = 2.*X(I) + X(I+2)     
            DIAG(I) = -6.*X(I+1)    
            SUP(I) = X(I) + 2.*X(I+2)     
            F(I) = X(I)**2 + X(I)*X(I+2) + X(I+2)**2 - 3.*X(I+1)**2     
   20       FL1 = FL1 + ABS(F(I))   
C     
         PRINT 620, ITER,FL1,(I,X(I+1),F(I),I=1,N)    
  620    FORMAT(' AT ITERATION',I3,', ERROR =',E10.3/(I3,F15.9,E15.6))  
C     
C                          SOLVE FOR NEXT NEWTON CORRECTION 
         CALL TRID ( SUB, DIAG, SUP, F, N )     
         XL1 = 0. 
         DO 25 I=1,N    
            XL1 = XL1 + ABS(F(I))   
   25       X(I+1) = X(I+1) - F(I)  
         XL1 = XL1/FLOAT(N)   
         PRINT 625, XL1 
  625    FORMAT(' CHANGE IN  X  HAS SIZE ',E15.6)     
C     
C                           CHECK FOR SMALL NEWTON CORRECTION     
         IF (XL1 .LT. 1.E-7)            STOP    
   30    CONTINUE 
      END   
