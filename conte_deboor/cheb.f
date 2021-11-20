      REAL FUNCTION CHEB (X)  
C  RETURNS THE VALUE OF THE POLYNOMIAL OF DEGREE .LT. NTERMS WHOSE
C  CHEBYSHEV COEFFICIENTS ARE CONTAINED IN  D .
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c 
      INTEGER NTERMS,   K     
      REAL D,X,   PREV,PREV2,TWOX   
      COMMON /POLY/ NTERMS,D(30)    

      IF ( NTERMS .EQ. 1) THEN
         CHEB = D(1)    
         RETURN  
      END IF

      TWOX = 2.*X 
      PREV2 = 0.  
      PREV = D(NTERMS)  
      IF (NTERMS .GT. 2) THEN 
         DO K=NTERMS-1,2,-1
            CHEB = D(K) + TWOX*PREV - PREV2     
            PREV2 = PREV
            PREV = CHEB 
        end do
      END IF
      CHEB = D(1) + X*PREV - PREV2  

      RETURN  
      END   
