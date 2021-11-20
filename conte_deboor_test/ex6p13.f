C  PROGRAM FOR EXAMPLE 6.13 . 
      PARAMETER (NPMAX=100 )   
      INTEGER NTERMS,   I,J,NPOINT  
      REAL B,C,D,ERROR(NPMAX),F(NPMAX),PJ(NPMAX),PJM1(NPMAX),W(NPMAX)   
     *    ,X(NPMAX)     
      COMMON /POLY/ NTERMS,B(20),C(20),D(20)    
      NPOINT = 21 
      DO 1 I=1,NPOINT   
         W(I) = 1.
         X(I) = -1. + FLOAT(I-1)/10.
    1    F(I) = FLOAT(IFIX(EXP(X(I))*100. + .5))/100. 
      NTERMS = 4  
      CALL ORTPOL( X, F, W, NPOINT, PJM1, PJ, ERROR ) 
      PRINT 601, (J,B(J),C(J),D(J),J=1,NTERMS)  
  601 FORMAT(I2,3E16.8) 
      DO 60 I=1,NPOINT  
         PJM1(I) = EXP(X(I))  
   60    PJ(I) = ORTVAL(X(I)) 
      PRINT 660, (X(I),F(I),PJ(I),ERROR(I),PJM1(I),I=1,NPOINT)    
  660 FORMAT(F5.1,F8.3,F10.5,E13.3,F10.5) 
                                        STOP    
      END   
