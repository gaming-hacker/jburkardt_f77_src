      SUBROUTINE TRID ( SUB, DIAG, SUP, B, N )  
      DIMENSION SUB(N),DIAG(N),SUP(N),B(N)
C  THE TRIDIAGONAL LINEAR SYSTEM    
C     SUB(I)*X(I-1) + DIAG(I)*X(I) + SUP(I)*X(I+1) = B(I), I=1,...,N    
C  (WITH SUB(1) AND SUP(N) TAKEN TO BE ZERO) IS SOLVED BY FACTORIZATION 
C  AND SUBSTITUTION. THE FACTORIZATION IS RETURNED IN  SUB , DIAG , SUP 
C  AND THE SOLUTION IS RETURNED IN  B .
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c
      IF (N .LE. 1) THEN
         B(1) = B(1)/DIAG(1)  
                                        RETURN  
      END IF
      DO 11 I=2,N 
         SUB(I) = SUB(I)/DIAG(I-1)  
         DIAG(I) = DIAG(I) - SUB(I)*SUP(I-1)    
   11    B(I) = B(I) - SUB(I)*B(I-1)

      B(N) = B(N)/DIAG(N)     
      DO I=N-1,1,-1  
        B(I) = (B(I) - SUP(I)*B(I+1))/DIAG(I)  
      end do
                                        RETURN  
      END   
