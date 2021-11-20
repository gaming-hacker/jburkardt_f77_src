C     EXAMPLE 7.5 . CORRECTED TRAPEZOID RULE    
      INTEGER I,N 
      REAL A,B,CORTRP,H,TRAP  
      F(X) = EXP(-X*X)  
      FPRIME(X) = -2.*X*F(X)  
      DATA A,B /0., 1. /
      PRINT 600   
  600 FORMAT(9X,'N',7X,'TRAPEZOID SUM',7X,'CORR.TRAP.SUM')  
      DO 10 N = 10,15   
         H = (B - A)/FLOAT(N) 
         TRAP = (F(A) + F(B))/2.    
         DO 1 I=1,N-1   
    1       TRAP = TRAP + F(A + FLOAT(I)*H)     
         TRAP = H*TRAP  
         CORTRP = TRAP + H*H*(FPRIME(A) - FPRIME(B))/12.    
   10    PRINT 610, N,TRAP,CORTRP   
  610 FORMAT(I10,2E20.7)
                                        STOP    
      END   
