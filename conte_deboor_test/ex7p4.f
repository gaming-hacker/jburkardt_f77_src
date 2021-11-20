C  EXAMPLE 7.4 . TRAPEZOID RULE .   
      INTEGER I,N 
      REAL A,B,H,T
      F(X) = EXP(-X*X)  
    1 PRINT 601   
  601 FORMAT(' EXAMPLE 7.4 TRAPEZOIDAL INTEGRATION')  
      READ 501,  A,B,N  
  501 FORMAT(2E20.0,I5) 
      IF (N .LT. 2)                     STOP    
      T = F(A)/2. 
      H = (B - A)/FLOAT(N)    
      DO 2 I=1,N-1
    2    T = F(A + FLOAT(I)*H) + T  
      T = (F(B)/2. + T)*H     
      PRINT 602, A,B,N,T
  602 FORMAT(' INTEGRAL FROM A = ',1PE14.7,' TO B = ',E14.7,
     *       ' FOR N = ',I5,' IS ',E14.7) 
                                        GO TO 1 
      END   
