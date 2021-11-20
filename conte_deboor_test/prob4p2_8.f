C  TO MAKE SURE THAT THE MATRIX IN PROBLEM 4.2-8 GIVES INTERESTING RESULTS.   
      COMMON /LENGTH/ NDIGIT  
      PARAMETER ( N=4 )
      parameter ( NSQ=N*N )   
      INTEGER IPIVOT(N) 
      REAL W(N,N),D(N),B(N),X(N)    
      DO 5 I=1,N  
    5    READ 499,(W(I,J),J=1,N)    
  499 FORMAT(10F5.3)    
      READ 499,B  
      DO 10 I=1,N 
   10    PRINT 600,I,(W(I,J),J=1,N) 
  600 FORMAT(I3,8E15.7/(7X,8E15.7)) 
C     
      READ 500,NDIGIT   
  500 FORMAT(I1)  
      CALL FACTOR ( W, N, D, W, IPIVOT, IFLAG ) 
C     
      DO 21 I=1,N 
   21    PRINT 600,IPIVOT(I),(W(I,J),J=1,N)     
      IF (IFLAG .NE. 0)                 GO TO 25
      PRINT 610   
  610 FORMAT(' SOMETHING WENT WRONG.')    
                                        STOP    
C     
   25 CALL SUBST ( W, IPIVOT, B, N, X)    
      PRINT 600,IPIVOT(1),X   
                                        STOP    
      END   
