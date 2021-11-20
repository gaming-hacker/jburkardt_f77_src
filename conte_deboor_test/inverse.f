C  PROGRAM FOR CALCULATING THE INVERSE OF A GIVEN MATRIX    
C  CALLS  F A C T O R ,  S U B S T  .     
      PARAMETER (NMAX=30)
      parameter (NMAXSQ=NMAX*NMAX )
      INTEGER I,IBEG,IFLAG,IPIVOT(NMAX),J,N,NSQ 
      REAL A(NMAXSQ),AINV(NMAXSQ),B(NMAX) 
    1 READ 501, N 
  501 FORMAT(I2)  
      IF (N .LT. 1 .OR. N .GT. NMAX)    STOP    
C               READ IN MATRIX ROW BY ROW 
      NSQ = N*N   
      DO 10 I=1,N 
   10    READ 510, (A(J),J=I,NSQ,N) 
  510 FORMAT(5E15.7)    
C     
      CALL FACTOR ( A, N, B, IPIVOT, IFLAG )    
      IF (IFLAG .EQ. 0) THEN  
         PRINT 611
  611    FORMAT('1MATRIX IS SINGULAR')    
                                        GO TO 1 
      END IF
      DO 21 I=1,N 
   21    B(I) = 0.
      IBEG = 1    
      DO 30 J=1,N 
         B(J) = 1.
         CALL SUBST ( A, IPIVOT, B, N, AINV(IBEG) )   
         B(J) = 0.
   30    IBEG = IBEG + N
      PRINT 630   
  630 FORMAT('1THE COMPUTED INVERSE IS '//)     
      DO 31 I=1,N 
   31    PRINT 631, I, (AINV(J),J=I,NSQ,N)
  631 FORMAT('0ROW ',I2,8E15.7/(7X,8E15.7))     
                                        GO TO 1 
      END   
