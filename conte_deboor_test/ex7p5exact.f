C     EXPERIMENT FOR EXACT VALUE FOR EXAMPLE 7.5.     
      INTEGER I,N 
      DOUBLE PRECISION A,B,CORTRP,H,TRAP,X
      double precision f
      double precision fprime
      F(X) = DEXP(-X*X) 
      FPRIME(X) = -2.*X*F(X)  
      DATA A,B /0.D0, 1.D0 /  
      PRINT 600   
  600 FORMAT(9X,'N',7X,'TRAPEZOID SUM',7X,'CORR.TRAP.SUM')  
    1 READ 500, NBEGIN,NEND   
  500 FORMAT(2I3) 
      IF (NBEGIN .LT. 2)                STOP    
      DO 10 N = NBEGIN,NEND   
         H = (B - A)/FLOAT(N) 
         TRAP = (F(A) + F(B))/2.    
         DO 5 I=1,N-1   
    5       TRAP = TRAP + F(A + FLOAT(I)*H)     
         TRAP = H*TRAP  
         CORTRP = TRAP + H*H*(FPRIME(A) - FPRIME(B))/12.    
   10    PRINT 610, N,TRAP,CORTRP   
  610 FORMAT(I10,2E25.15)     
                                        GO TO 1 
      END   
