C  CONSTRUCTION OF THE NEWTON FORM FOR THE POLYNOMIAL OF DEGREE   
C  .LE. N , WHICH AGREES WITH  F(X)  AT  Y(I), I=1,...,NP1. 
C  SOME OR ALL OF THE INTERPOLATION POINTS MAY COINCIDE, SUBJECT  
C  ONLY TO THE FOLLOWING RESTRICTIONS.    
C  (1) IF  Y(I) = Y(I+K),  THEN  Y(I) = Y(I+1) = ... = Y(I+K) .   
C  (2) IF ALSO  Y(I-1) .NE. Y(I) , OR IF   I = 1 , THEN     
C      F(I+J) = VALUE OF J-TH DERIVATIVE OF  F(X)  AT  X = Y(I),  
C                                                         J=0,...,K.    
C     
      INTEGER I,J,K,N,NPOINT,NP1    
      REAL DX,DY,F(30),FLAST,PNOFX,REALK,X,Y(30)
      READ 500,NP1,(Y(I),F(I),I=1,NP1)    
  500 FORMAT(I2/(2F10.3))     
C              CONSTRUCT DIVIDED DIFFERENCES    
      N = NP1 - 1 
      DO 10 K=1,N 
         REALK = K
         FLAST = F(1)   
         DO 9 I=1,NP1-K 
            DY = Y(I+K) - Y(I)
            IF (DY .EQ. 0.) THEN    
               F(I) = F(I+1)/REALK  
            ELSE  
               F(I) = (F(I+1) - FLAST)/DY 
               FLAST = F(I+1) 
            END IF
    9    CONTINUE 
         F(NP1-K+1) = FLAST   
   10 CONTINUE    
C         CALCULATE PN(X) FOR VARIOUS VALUES OF X.    
      READ 501,NPOINT,X,DX    
  501 FORMAT(I3/2F10.3) 
      DO 30 J=1,NPOINT  
         PNOFX = F(1)   
         DO 29 I=2,NP1  
            PNOFX = F(I) + (X - Y(I))*PNOFX     
   29    CONTINUE 
         PRINT 629,J,X,PNOFX  
  629    FORMAT(I10,2E20.7)   
         X = X + DX     
   30 CONTINUE    
                                        STOP    
      END   
