C  MAIN PROGRAM FOR EXAMPLE 4.13. INVERSE ITERATION   
      INTEGER IFLAG,IPIVOT(3),N     
      REAL B(9),D(3),EGUESS,EVALUE,VECTOR(3),VGUESS(3),W(9) 
      DATA N,B / 3,     
     1          1., 2., 0.,   
     2          2., 1., 0.,   
     3          0., 0., -1. / 
C     
      EGUESS = 3.0165   
      DO 20 IDUMMY=1,2  
         DO 10 I=1,N    
   10       VGUESS(I) = 1.    
         CALL INVITR(B,N,EGUESS,VGUESS,W,D,IPIVOT,EVALUE,VECTOR,IFLAG)  
         IF (IFLAG .EQ. 0)              GO TO 20
         PRINT 615,EVALUE,VECTOR    
  615    FORMAT(' EIGENVALUE =',E15.7/' EIGENVECTOR ='/(4E15.7))  
   20    EGUESS = 0.    
                                        STOP    
      END   
