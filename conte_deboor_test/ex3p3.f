C  PROGRAM FOR EXAMPLE 3.3    
      INTEGER J   
      REAL ERROR,FTOL,XNEW,XOLD,XTOL,Y    
C  THIS PROGRAM SOLVES THE EQUATION 
C     EXP(-X) = SIN(X)  
C  BY FIXED POINT ITERATION, USING THE ITERATION FUNCTION   
      G(X) = EXP(-X) - SIN(X) + X   
C     
      DATA XTOL, FTOL /  1.E-8, 1.E-8 /   
      PRINT 600   
  600 FORMAT(9X,'XNEW',12X,'F(XNEW)',10X,'ERROR')     
      XOLD = .6   
      Y = G(XOLD) - XOLD
      PRINT 601, XOLD,Y 
  601 FORMAT(3X,3E16.8) 
      DO 10 J=1,20
         XNEW = G(XOLD) 
         Y = G(XNEW) - XNEW   
         ERROR = ABS(XNEW - XOLD)/ABS(XNEW)     
         PRINT 601, XNEW,Y,ERROR    
         IF (ERROR .LT. XTOL .OR. ABS(Y) .LT. FTOL) STOP    
         XOLD = XNEW    
   10 CONTINUE    
      PRINT 610   
  610 FORMAT(' FAILED TO CONVERGE IN 20 ITERATIONS')  
                                        STOP    
      END   
