C  PROGRAM FOR EXAMPLE 6.16 . 
      INTEGER I,N   
      REAL C(4,17),ERRMAX,H,X(17),Y 
C     PIECEWISE CUBIC HERMITE INTERPOLATION AT EQUALLY SPACED POINTS    
C     TO THE FUNCTION   
             F(Y) = 1./(1. + Y*Y)   
C     
      PRINT 600   
  600 FORMAT('1   N',5X,'MAXIMUM ERROR')  
      DO 40 N=2,16,2    
         H = 10./FLOAT(N)     
         DO 10 I=1,N+1  
            X(I) = FLOAT(I-1)*H - 5.
            C(1,I) = F(X(I))  
C           C(2,I) = F'(X(I)) 
   10       C(2,I) = -2.*X(I)*C(1,I)**2   
         CALL CALCCF ( X, C, N )    
C                  ESTIMATE MAXIMUM INTERPOLATION ERROR ON (-5,5).
         ERRMAX = 0.    
         DO 30 I=1,101  
         Y =.1*I - 5.   
            ERRMAX = MAX(ERRMAX, ABS(F(Y)-PCUBIC(Y,X,C,N))) 
   30    CONTINUE 
   40    PRINT 640, N,ERRMAX  
  640 FORMAT(I5,E18.7)  
      STOP     
      END   
