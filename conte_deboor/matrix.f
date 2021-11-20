      SUBROUTINE MATRIX ( A, OP, B, C, N )

      DIMENSION A(N,N), B(N,N), C(N,N)   
      character OP 
C  FORMS THE MATRIX  (A)OP(B) = C   
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c
      IF (OP .EQ. '*')                GO TO 30
      IF (OP .EQ. '-')                GO TO 20
      DO 11 J=1,N 
         DO 11 I=1,N    
   11       C(I,J) = A(I,J) + B(I,J)
                                        GO TO 50
   20 DO 21 J=1,N 
         DO 21 I=1,N    
   21       C(I,J) = A(I,J) - B(I,J)
                                        GO TO 50
   30 DO 32 J=1,N 
         DO 32 I=1,N    
            SUM = 0.    
            DO 31 K=1,N 
   31          SUM = A(I,K)*B(K,J) + SUM  
   32       C(I,J) = SUM
C     
   50 DO 51 I=1,N 
   51    PRINT 650,I,(C(I,J),J=1,N) 
  650 FORMAT(5H0ROW ,I2,8E15.7/(7X8E15.7))
                                        RETURN  
      END   
