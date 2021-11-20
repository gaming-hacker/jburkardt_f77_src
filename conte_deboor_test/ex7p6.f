C  PROGRAM FOR EXAMPLE 7.6 . SIMPSON'S RULE .   
      INTEGER I,N 
      REAL A,B,H,HALF,HOVER2,S,X    
      F(X) = EXP(-X*X)  
      PRINT 600   
  600 FORMAT(' EXAMPLE 7.6 SIMPSON''S RULE'/)   
    1 READ 501, A,B,N   
  501 FORMAT(2E20.0,I5) 
      IF (N .LT. 2)                     STOP    
      H = (B - A)/FLOAT(N)    
      HOVER2 = H/2.     
      S = 0.
      HALF = F(A + HOVER2)    
      DO 2 I=1,N-1
         X = A + FLOAT(I)*H   
         S = S + F(X)   
    2    HALF = HALF + F(X+HOVER2)  
      S = (H/6.)*(F(A) + 4.*HALF + 2.*S + F(B)) 
      PRINT 602, A,B,N,S
  602 FORMAT(' INTEGRAL FROM A = ',1PE14.7,' TO B = ',E14.7,
     *       ' FOR N = ',I5,' IS ',E14.7) 
                                        GO TO 1 
c   4  FORMAT(2E20.0,I5) 
      END   
