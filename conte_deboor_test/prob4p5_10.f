C  PROGRAM FOR CALCULATING THE INVERSE OF A GIVEN MATRIX    
C  ADAPTED FOR PROBLEM 4.5-10 .     
      DIMENSION A(900),AINV(900),B(30),IPIVOT(30),W(900)    
    1 READ 501, N 
  501 FORMAT(I2)  
C     READ IN MATRIX ROW BY ROW     
      NSQ = N*N   
      DO 10 I=1,N 
   10    READ 510, (A(J),J=I,NSQ,N) 
  510 FORMAT(5E15.7)    
C     
      CALL FACTOR ( A, N, B, W, IPIVOT, IFLAG ) 
      IF (IFLAG .NE. 0)                 GO TO 20
      PRINT 611   
  611 FORMAT('1MATRIX IS SINGULAR')
                                        GO TO 1 
   20 DO 21 I=1,N 
   21    B(I) = 0.
      IBEG = 1    
      DO 30 J=1,N 
         B(J) = 1.
         CALL SUBST ( W, IPIVOT, B, N, AINV(IBEG) )   
         B(J) = 0.
   30    IBEG = IBEG + N
      PRINT 630   
  630 FORMAT('1THE COMPUTED INVERSE IS //')    
      DO 31 I=1,N 
   31    PRINT 631, I, (AINV(J),J=I,NSQ,N)
  631 FORMAT(5H0ROW ,I2,8E15.7/(7X8E15.7))
      CALL MATRIX( A , '*' , AINV , W, N )
      CALL MATRIX ( AINV , '*' , A , W, N )     
                                        GO TO 1 
      END   
