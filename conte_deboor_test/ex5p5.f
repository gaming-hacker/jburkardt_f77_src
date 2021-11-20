C  EXAMPLE 5.5  DAMPED NEWTON,S METHOD    
      INTEGER ICUT,ICUTMX,ITER,ITERMX     
      REALDIAG(2),F(2),FSIZE,HSIZE,PREVER,SQERR,SUB(2),SUP(2),X(2),X1,X2
      F1(X1,X2) = X2**2 - X1 - 3.*ALOG(ABS(X1)) 
      F2(X1,X2) = 5.*X1-2.*X1**2 + X1*X2 - 1.   
C     READ INITIAL GUESS
    1 READ 500,ITERMX,ICUTMX,X
  500 FORMAT(2I3/(F10.3))     
      F(1) = F1(X(1),X(2))    
      F(2) = F2(X(1),X(2))    
      SQERR = F(1)**2 + F(2)**2     
C     
C                   ITERATION STARTS HERE 
      DO 50 ITER=1,ITERMX     
         FSIZE = SQRT(SQERR)  
         PRINT 620,ITER,FSIZE,X,F   
  620    FORMAT(' AT BEGINNING OF ITERATION',I3,', L2-ERROR ='    
     *      ,E10.4/2F15.7/2E15.7)   
         PREVER = SQERR 
         DIAG(1) = 1. + 3./X(1)     
         SUP(1) = -2.*X(2)    
         SUB(2) = 4.*X(1) - X(2) - 5.     
         DIAG(2) = -X(1)
         CALL TRID ( SUB, DIAG, SUP, F, 2 )     
         HSIZE = SQRT(F(1)**2 + F(2)**2)  
         PRINT 621,HSIZE
  621    FORMAT(' PROPOSED STEP SIZE =',E10.4/) 
         IF (ICUTMX .LT. 1)             GO TO 31
         DO 30 ICUT=1,ICUTMX  
            X1 = X(1) + F(1)  
            X2 = X(2) + F(2)  
            SQERR = F1(X1,X2)**2 + F2(X1,X2)**2 
            IF (SQERR .LT. PREVER)      GO TO 31
            PRINT 625,ICUT,SQERR    
  625       FORMAT(' BEFORE ',I2,'. CUT, SQERR =',E15.7)    
            F(1) = F(1)/2.    
   30       F(2) = F(2)/2.    
         PRINT 630,ICUTMX     
  630    FORMAT(' FAILURE TO CUT ERROR IN ',I2,' TRIES')    
                                        GO TO 1 
   31    CONTINUE 
         X(1) = X(1) + F(1)   
         X(2) = X(2) + F(2)   
         F(1) = F1(X(1),X(2)) 
         F(2) = F2(X(1),X(2)) 
         SQERR = F(1)**2 + F(2)**2  
         IF (SQERR .LT. 1.E-14)         GO TO 60
   50    CONTINUE 
                                        GO TO 1 
   60 FSIZE = SQRT(SQERR)     
      PRINT 660,FSIZE,X,F     
  660 FORMAT(' FINAL ANSWER HAS L2-ERROR =',E10.4/2F15.7/2E15.7)  
      END   
