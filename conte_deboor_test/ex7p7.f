C  PROGRAM FOR EXAMPLE 7.7. COMPOSITE FOUR-POINT GAUSS-LEGENDRE . 
      INTEGER I,N 
      REAL A,B,H,HOVER2,P1,P2,POINT(2),S,S1,S2,WEIGHT(2),X  
      DATA POINT,WEIGHT / .3399810436, .8611363116, 
     *                    .6521451549, .3478548451 /
      F(X) = SIN(X)**2/X
      PRINT 600   
  600 FORMAT(' EXAMPLE 7.7  FOUR-POINT GAUSS-LEGENDRE'/)    
    1 READ 501, A,B,N   
  501 FORMAT(2E20.0,I5) 
      IF (N .LT. 1)                     STOP    
      H = (B - A)/FLOAT(N)    
      HOVER2 = H/2.     
      P1 = POINT(1)*HOVER2    
      P2 = POINT(2)*HOVER2    
      S1 = 0.     
      S2 = 0.     
      DO 2 I=1,N  
         X = A + FLOAT(I)*H - HOVER2
         S1 = S1 + F(-P1+X) + F(P1+X)     
    2    S2 = S2 + F(-P2+X) + F(P2+X)     
      S = HOVER2*(WEIGHT(1)*S1 + WEIGHT(2)*S2)  
      PRINT 602, A,B,N,S
  602 FORMAT(' INTEGRAL FROM A = ',1PE14.7,' TO B = ',E14.7,
     *       ' FOR N = ',I3,' IS ',E14.7) 
                                        GO TO 1 
      END   
