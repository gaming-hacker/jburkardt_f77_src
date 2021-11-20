      SUBROUTINE FFTSTP ( ZIN, AFTER, NOW, BEFORE, ZOUT )   
CALLED IN  F F T .
CARRIES OUT ONE STEP OF THE DISCRETE FAST FOURIER TRANSFORM.
      INTEGER AFTER,BEFORE,NOW,   IA,IB,IN,J    
      REAL ANGLE,TWOPI  
      COMPLEX ZIN(AFTER,BEFORE,NOW),ZOUT(AFTER,NOW,BEFORE),   ARG,OMEGA,
     *                                                        VALUE     
      DATA TWOPI / 6.2831853071795864769 /   
      ANGLE = TWOPI/FLOAT(NOW*AFTER)
      OMEGA = CMPLX(COS(ANGLE),-SIN(ANGLE))     
      ARG = CMPLX(1.,0.)
      DO 100 J=1,NOW    
         DO 90 IA=1,AFTER     
            DO 80 IB=1,BEFORE 
               VALUE = ZIN(IA,IB,NOW)     
               DO 70 IN=NOW-1,1,-1  
   70             VALUE = VALUE*ARG + ZIN(IA,IB,IN)   
   80          ZOUT(IA,J,IB) = VALUE
   90       ARG = ARG*OMEGA   
  100 CONTINUE    
                                        RETURN  
      END   
