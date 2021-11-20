C  NEWTON'S METHOD FOR FINDING A REAL ZERO OF A CERTAIN POLYNOMIAL.     
C  THE COEFFICIENTS ARE SUPPLIED IN A DATA STATEMENT. A FIRST GUESS     
C   X  FOR THE ZERO IS READ IN .    
      PARAMETER (N=6)     
      INTEGER J,K 
      REAL A(N),B,C,DELTAX,X  
      DATA A /-6.8, 10.8, -10.8, 7.4, -3.7, 1./ 
    1 READ 500, X 
  500 FORMAT(E16.8)     
      PRINT 601   
  601 FORMAT('1NEWTONS METHOD FOR FINDING A REAL ZERO OF A POLYNOMIAL'  
     *       //4X,'I',10X,'X',14X,'AP(0)',12X,'APP(1)'/)    
      DO 10 J=1,20
         B = A(N) 
         C = B    
         DO 5 K=N,3,-1  
            B = A(K-1) + X*B  
            C = B + X*C 
    5    CONTINUE 
         B = A(1) + X*B 
         PRINT 605,J,X,B,C    
  605    FORMAT(I5,3(1PE17.7))
         DELTAX = B/C   
         IF (ABS(DELTAX) .LT. 1.E-7 .OR. ABS(B) .LT. 1.E-7) STOP  
         X = X - DELTAX 
   10 CONTINUE    
      PRINT 610   
  610 FORMAT(' FAILED TO CONVERGE IN 20 ITERATIONS')  
                                        GO TO 1 
      END   
