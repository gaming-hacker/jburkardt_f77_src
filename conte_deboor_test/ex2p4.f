      program ex2p4

C  PROGRAM FOR EXAMPLE 2.4    
      INTEGER I,J,K,N,NP1     
      REAL D(17),ERRMAX,H,PNOFY,X(17),Y   
C  POLYNOMIAL INTERPOLATION AT EQUALLY SPACED POINTS TO THE FUNCTION    
          F(Y)  =  1./(1. + Y*Y)    
C     
      PRINT 600   
  600 FORMAT('1   N',5X,'MAXIMUM ERROR')  
      DO 40 N=2,16,2    
         NP1 = N+1
         H = 10./FLOAT(N)     
         DO 10 I=1,NP1  
            X(I) = FLOAT(I-1)*H - 5.
            D(I) = F(X(I))    
   10    CONTINUE 
C               CALCULATE DIVIDED DIFFERENCES BY ALGORITHM 2.3    
         DO 20 K=1,N    
            DO 20 I=1,NP1-K   
               D(I) = (D(I+1) - D(I))/(X(I+K) - X(I)) 
   20    CONTINUE 
C               ESTIMATE MAXIMUM INTERPOLATION ERROR ON (-5,5)    
         ERRMAX = 0.    
         DO 30 J=1,101  
            Y = FLOAT(J-1)/10. - 5. 
C               CALCULATE PN(Y) BY ALGORITHM 2.1
            PNOFY = D(1)
            DO 29 K=2,NP1     
               PNOFY = D(K) + (Y - X(K))*PNOFY  
   29       CONTINUE    
            ERRMAX = MAX(ABS(F(Y) - PNOFY), ERRMAX)   
   30    CONTINUE 
         PRINT 630, N,ERRMAX  
  630    FORMAT(I5,E18.7)     
   40 CONTINUE    
      STOP    
      END   
