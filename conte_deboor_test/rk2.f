      program rk2

C  FORTRAN PROGRAM TO SOLVE THE FIRST ORDER DIFFERENTIAL EQUATION 
C        Y'(X) = F(X,Y) 
C  WITH INITIAL CONDITION OF  
C        Y(XBEGIN) = YBEGIN   
C  TO THE POINT  XEND , USING THE SECOND ORDER RUNGE-KUTTA METHOD.
C  A FUNCTION SUBPROGRAM CALLED 'F' MUST BE SUPPLIED. 
      INTEGER N,NSTEPS
      REAL DERIV,H,K1,K2,XBEGIN,XN,XEND,YBEGIN,YN     
    1 READ 501, XBEGIN,YBEGIN,XEND,NSTEPS 
  501 FORMAT(3F10.5,I3) 

      IF (NSTEPS .LT. 1) then
        STOP 
      end if
   
      H = (XEND - XBEGIN)/NSTEPS    
      XN = XBEGIN 
      YN = YBEGIN 
      DERIV = F(XN,YN)  
      N = 0 
      PRINT 601, N,XN,YN,DERIV
  601 FORMAT(1X,I3,3E21.9)  
  
      DO N=1,NSTEPS  
         K1 = H*F(XN,YN)
         K2 = H*F(XN+H,YN+K1) 
         YN = YN + .5*(K1+K2) 
         XN = XBEGIN + N*H    
         DERIV = F(XN,YN)     
         PRINT 601, N,XN,YN,DERIV   
      end do
                                        GO TO 1 
      END   
      REAL FUNCTION F(X,Y)    
      REAL X,Y    
      F = (1./X - Y)/X - Y*Y  
      RETURN  
      END   
