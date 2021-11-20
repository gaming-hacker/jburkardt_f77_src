      REAL FUNCTION ORTVAL (X)
C  RETURNS THE VALUE AT  X  OF THE POLYNOMIAL OF DEGREE .LT. NTERMS     
C  GIVEN BY 
C            D(1)*P0(X) + D(2)*P1(X) + ... + D(NTERMS)*PNTERMS-1(X),    
C  WITH THE SEQUENCE  P0, P1, ...  OF ORTHOGONAL POLYNOMIALS GENERATED  
C  BY THE THREE-TERM RECURRENCE     
C     PJP1(X) = (X - B(J+1))*PJ(X) - C(J+1)*PJM1(X) , ALL J .     
C   
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c  
      COMMON /POLY/ NTERMS,B(20),C(20),D(20)    
      PREV = 0.   
      ORTVAL = D(NTERMS)
      IF (NTERMS .EQ. 1)                RETURN  
      DO K=NTERMS-1,1,-1   
         PREV2 = PREV   
         PREV = ORTVAL  
         ORTVAL = D(K) + (X - B(K))*PREV - C(K+1)*PREV2     
      end do   
                                        RETURN  
      END   
