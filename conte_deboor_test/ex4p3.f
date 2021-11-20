C  FORTRAN PROGRAM FOR EXAMPLE 4.3  
      PARAMETER (N=10)   
      INTEGER I   
      REAL A(N),B(N),C(N),D(N)
      DO 10 I=1,N 
         A(I) = -1.     
         D(I) = 2.
         C(I) = -1.     
   10    B(I) = 0.
      B(1) = 1.   
      CALL TRID ( A, D, C, B, N )   
      PRINT 610, (I,B(I),I=1,N)     
  610 FORMAT('1THE SOLUTION IS '/(I5,E15.7))    
                                        STOP    
      END   
      SUBROUTINE TRID ( SUB, DIAG, SUP, B, N )  
      INTEGER N,   I    
      REAL B(N),DIAG(N),SUB(N),SUP(N)     
C  THE TRIDIAGONAL LINEAR SYSTEM    
C     SUB(I)*X(I-1) + DIAG(I)*X(I) + SUP(I)*X(I+1) = B(I), I=1,...,N    
C  (WITH SUB(1) AND SUP(N) TAKEN TO BE ZERO) IS SOLVED BY FACTORIZATION 
C  AND SUBSTITUTION. THE FACTORIZATION IS RETURNED IN  SUB , DIAG , SUP 
C  AND THE SOLUTION IS RETURNED IN  B .   
      IF (N .LE. 1) THEN
         B(1) = B(1)/DIAG(1)  
                                        RETURN  
      END IF
      DO 11 I=2,N 
         SUB(I) = SUB(I)/DIAG(I-1)  
         DIAG(I) = DIAG(I) - SUB(I)*SUP(I-1)    
   11    B(I) = B(I) - SUB(I)*B(I-1)
      B(N) = B(N)/DIAG(N)     
      DO 12 I=N-1,1,-1  
   12    B(I) = (B(I) - SUP(I)*B(I+1))/DIAG(I)  
                                        RETURN  
      END   
