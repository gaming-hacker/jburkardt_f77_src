      SUBROUTINE ORTPOL ( X, F, W, NPOINT, PJM1, PJ, ERROR )
C  CONSTRUCTS THE DISCRETE WEIGHTED LEAST SQUARES APPROXIMATION BY POLY-
C  NOMIALS OF DEGREE .LT. NTERMS  TO GIVEN DATA.
C******  I N P U T  ******    
C  (X(I), F(I)), I=1,...,NPOINT  GIVES THE ABSCISSAE AND ORDINATES OF   
C              THE GIVEN DATA POINTS TO BE FITTED.    
C  W  NPOINT-VECTOR CONTAINING THE POSITIVE WEIGHTS TO BE USED.   
C  NPOINT  NUMBER OF DATA POINTS.   
C******  I N P U T  VIA COMMON BLOCK  P O L Y  ****** 
C  NTERMS  GIVES THE ORDER (= DEGREE + 1) OF THE POLYNOMIAL APPROXIMANT.
C******  W O R K  A R E A S  ****** 
C  PJM1, PJ  ARRAYS OF LENGTH  NPOINT  TO CONTAIN THE VALUES AT THE X'S 
C              OF THE TWO MOST RECENT ORTHOGONAL POLYNOMIALS.     
C******  O U T P U T  ******  
C  ERROR  NPOINT-VECTOR CONTAINING THE ERROR AT THE X'S OF THE POLYNOM- 
C      IAL APPROXIMANT TO THE GIVEN DATA. 
C******  O U T P U T  VIA COMMON BLOCK  P O L Y  ******     
C  B, C  ARRAYS CONTAINING THE COEFFICIENTS FOR THE THREE-TERM RECUR-   
C      RENCE WHICH GENERATES THE ORTHOGONAL POLYNOMIALS.    
C  D  COEFFICIENTS OF THE POLYNOMIAL APPROXIMANT TO THE GIVEN DATA WITH 
C      RESPECT TO THE SEQUENCE OF ORTHOGONAL POLYNOMIALS.   
C      THE VALUE OF THE APPROXIMANT AT A POINT  Y  MAY BE OBTAINED BY A 
C      REFERENCE TO   ORTVAL(Y) .   
C******  M E T H O D  ******  
C  THE SEQUENCE  P0, P1, ..., PNTERMS-1  OF ORTHOGONAL POLYNOMIALS WITH 
C  RESPECT TO THE DISCRETE INNER PRODUCT  
C        (P,Q)  =  SUM ( P(X(I))*Q(X(I))*W(I) , I=1,...,NPOINT)   
C  IS GENERATED IN TERMS OF THEIR THREE-TERM RECURRENCE     
C        PJP1(X) = (X - B(J+1))*PJ(X) - C(J+1)*PJM1(X) ,    
C  AND THE COEFFICIENT  D(J)  OF THE WEIGHTED LEAST SQUARES APPROXIMAT- 
C  ION TO THE GIVEN DATA IS OBTAINED CONCURRENTLY AS  
C         D(J+1) = (F,PJ)/(PJ,PJ) , J=0,...,NTERMS-1 .
C  ACTUALLY, IN ORDER TO REDUCE CANCELLATION,  (F,PJ)  IS CALCULATED AS 
C  (ERROR,PJ),  WITH  ERROR = F  INITIALLY, AND, FOR EACH  J , ERROR RE-
C  DUCED BY  D(J+1)*PJ  AS SOON AS  D(J+1)  BECOMES AVAILABLE.    
C   
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c  
      INTEGER NPOINT,NTERMS,   I,J  
      REAL B,C,D,ERROR(NPOINT),F(NPOINT),PJ(NPOINT),PJM1(NPOINT), 
     *     W(NPOINT),X(NPOINT),   P,S(20) 
      COMMON /POLY/ NTERMS,B(20),C(20),D(20)    
C     
      DO 9 J=1,NTERMS   
         B(J) = 0.
         D(J) = 0.
    9    S(J) = 0.
      C(1) = 0.   
      DO 10 I=1,NPOINT  
         D(1) = D(1) + F(I)*W(I)    
         B(1) = B(1) + X(I)*W(I)    
   10    S(1) = S(1) + W(I)   
      D(1) = D(1)/S(1)  
      DO 11 I=1,NPOINT  
   11    ERROR(I) = F(I) - D(1)     
      IF (NTERMS .EQ. 1)                RETURN  
      B(1) = B(1)/S(1)  
      DO 12 I=1,NPOINT  
         PJM1(I) = 1.   
   12    PJ(I) = X(I) - B(1)  
C     
      DO 30 J=2,NTERMS  
         DO 21 I=1,NPOINT     
            P = PJ(I)*W(I)    
            D(J) = D(J) + ERROR(I)*P
            P = P*PJ(I) 
            B(J) = B(J) + X(I)*P    
   21       S(J) = S(J) + P   
         D(J) = D(J)/S(J)     
         DO 22 I=1,NPOINT     
   22       ERROR(I) = ERROR(I) - D(J)*PJ(I)    
         IF (J .EQ. NTERMS)             RETURN  
         B(J) = B(J)/S(J)     
         C(J) = S(J)/S(J-1)   
         DO 27 I=1,NPOINT     
            P = PJ(I)   
            PJ(I) = (X(I) - B(J))*PJ(I) - C(J)*PJM1(I)
   27       PJM1(I) = P 
   30 CONTINUE    
                                        RETURN  
      END   
