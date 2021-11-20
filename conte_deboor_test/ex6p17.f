C  PROGRAM FOR EXAMPLE 6.17   
      PARAMETER ( NP1MAX = 50  )
      INTEGER I,IEND,N,N1,N2  
      REAL C(4,NP1MAX),FX,X,XI(NP1MAX)    
      READ 500, N1
  500 FORMAT(I2)  
      READ 501, (XI(I),C(1,I),I=1,N1),C(2,1),C(2,N1)  
  501 FORMAT(2E10.3)    
      N = N1 - 1  
      CALL SPLINE(XI,C,N)     
      CALL CALCCF(XI,C,N)     
C     
      READ 500, N2
      IEND = N + N2     
      READ 501, (XI(I),C(1,I),I=N1,IEND),C(2,N1),C(2,IEND)  
      N = N2 - 1  
      CALL SPLINE(XI(N1),C(1,N1),N) 
      CALL CALCCF(XI(N1),C(1,N1),N) 
C     
      N = IEND - 1
      X = XI(1)   
      DO 10 I=1,40
         FX = PCUBIC(X,XI,C,N)
         PRINT 600, I,X,FX    
  600    FORMAT(I5,F10.1,E20.9)     
   10    X = X + .5     
      STOP    
      END   
