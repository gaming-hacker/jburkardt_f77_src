      SUBROUTINE MRGFLS ( F, A, B, XTOL, FTOL, NTOL, W, IFLAG )

C******  I N P U T  ******    
C  F  NAME OF FUNCTION WHOSE ZERO IS SOUGHT. NAME MUST APPEAR IN AN     
C      E X T E R N A L  STATEMENT IN THE CALLING PROGRAM .  
C  A,B  ENDPOINTS OF INTERVAL WHEREIN ZERO IS SOUGHT. 
C  XTOL  DESIRED LENGTH OF OUTPUT INTERVAL
C  FTOL  DESIRED SIZE OF  F(W)
C  NTOL  NO MORE THAN  NTOL  ITERATION STEPS WILL BE CARRIED OUT. 
C******  O U T P U T  ******  
C  A,B  ENDPOINTS OF INTERVAL CONTAINING THE ZERO .   
C  W  BEST ESTIMATE OF THE ZERO .   
C  IFLAG  AN INTEGER,   
C     =-1, FAILURE, SINCE  F  HAS SAME SIGN AT INPUT POINTS  A, B .     
C     = 0, TERMINATION BECAUSE  ABS(A-B) .LE. XTOL .  
C     = 1, TERMINATION BECAUSE  ABS(F(W)) .LE. FTOL . 
C     = 2, TERMINATION BECAUSE  NTOL  ITERATION STEPS WERE CARRIED OUT .
C******  M E T H O D  ******  
C  THE MODIFIED REGULA FALSI ALGORITHM 3.3 IS USED. THIS MEANS THAT,    
C  AT EACH STEP, LINEAR INTERPOLATION BETWEEN THE POINTS  (A,FA)  AND   
C  (B,FB)  IS USED, WITH  FA*FB .LT. 0 ,TO GET A NEW POINT  (W,F(W))    
C  WHICH REPLACES ONE OF THESE IN SUCH A WAY THAT AGAIN  FA*FB .LT. 0.  
C  IN ADDITION, THE ORDINATE OF A POINT STAYING IN THE GAME FOR MORE    
C  THAN ONE STEP IS CUT IN HALF AT EACH SUBSEQUENT STEP.    
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c
      INTEGER IFLAG,NTOL,   N 
      REAL A,B,F,FTOL,W,XTOL,   FA,FB,FW,SIGNFA,PRVSFW
      FA = F(A)   
      SIGNFA = SIGN(1., FA)   
      FB = F(B)   
      IF (SIGNFA*FB .GT. 0.) THEN   
         PRINT 601,A,B  
  601    FORMAT(' F(X) IS OF SAME SIGN AT THE TWO ENDPOINTS',2E15.7)    
         IFLAG = -1     
                                        RETURN  
      END IF
C     
      W = A 
      FW = FA     
      DO 20 N=1,NTOL    
C                  CHECK IF INTERVAL IS SMALL ENOUGH. 
         IF (ABS(A-B) .LE. XTOL) THEN     
            IFLAG = 0   
                                        RETURN  
         END IF   
C                  CHECK IF FUNCTION VALUE AT  W  IS SMALL ENOUGH .     
         IF (ABS(FW) .LE. FTOL) THEN
            IFLAG = 1   
                                        RETURN  
         END IF   
C                  GET NEW GUESS  W  BY LINEAR INTERPOLATION .    
         W = (FA*B - FB*A)/(FA - FB)
         PRVSFW = SIGN(1.,FW) 
         FW = F(W)
C                           CHANGE TO NEW INTERVAL    
         IF (SIGNFA*FW .GT. 0.) THEN
            A = W 
            FA = FW     
            IF (FW*PRVSFW .GT. 0.)  FB = FB/2.  
         ELSE     
            B = W 
            FB = FW     
            IF (FW*PRVSFW .GT. 0.)  FA = FA/2.  
         END IF   
   20 CONTINUE    
      PRINT 620,NTOL    
  620 FORMAT(' NO CONVERGENCE IN ',I5,' ITERATIONS')  
      IFLAG = 2   
                                        RETURN  
      END   
