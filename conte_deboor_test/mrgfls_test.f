      program mrgfls_test

C  MAIN PROGRAM FOR TRYING OUT MODIFIED REGULA FALSI ROUTINE
      INTEGER IFLAG     
      REAL A,B,ERROR,FW,W     
      EXTERNAL FF 

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'mrgfls_test'
      write ( *, '(a)' ) '  Test mrgfls()'

      A = 1.
      B = 2.
      CALL MRGFLS ( FF, A, B, 1.E-7, 1.E-10, 30, W, IFLAG ) 
      IF (IFLAG .LT. 0) then
        STOP 1
      end if
      ERROR = ABS(A-B)  
      FW = FF(W)  
      PRINT 600, W,ERROR,FW   
  600 FORMAT(' THE ZERO IS ',E15.7,' PLUS/MINUS ',E15.7/    
     *       ' F(ZERO) = ',E15.7)   
      STOP 0 
      END   
      REAL FUNCTION FF(X)     
      REAL X
      FF = -1. - X*(1. - X*X) 
      PRINT 600,X,FF    
  600 FORMAT(' X, F(X) = ',2E15.7)  
      RETURN  
      END   
