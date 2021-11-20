      REAL FUNCTION PCUBIC ( XBAR, XI, C, N )   
C  RETURNS THE VALUE AT  XBAR  OF THE PIECEWISE CUBIC FUNCTION ON  N    
C  INTERVALS WITH BREAKPOINT SEQUENCE  XI  AND  COEFFICIENTS  C . 
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c
      INTEGER N,   I,J  
      REAL C(4,N),XBAR,XI(N+1),   DX
      DATA I /1/  
      IF (XBAR .GE. XI(I)) THEN     
         DO 10 J=I,N    
            IF (XBAR .LT. XI(J+1))      GO TO 30
   10    CONTINUE 
         J = N    
      ELSE  
         DO 20 J=I-1,1,-1     
            IF (XBAR .GE. XI(J))        GO TO 30
   20    CONTINUE 
         J = 1    
      END IF
   30 I = J 
      DX = XBAR - XI(I) 
      PCUBIC = C(1,I) + DX*(C(2,I) + DX*(C(3,I) + DX*C(4,I)))     
                                        RETURN  
      END   
