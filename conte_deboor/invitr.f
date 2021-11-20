      SUBROUTINE INVITR ( B, N, EGUESS, VGUESS, W, D, IPIVOT,     
     *                                   EVALUE, VECTOR, IFLAG )  
C     CALLS  F A C T O R  ,  S U B S T .  
      INTEGER IFLAG,IPIVOT(N),   I,ITER,ITERMX,J
      REAL B(N,N),D(N),EGUESS,EVALUE,VECTOR(N),VGUESS(N),W(N,N)   
     *       ,EPSLON,EVNEW,EVOLD,SQNORM   
C******  I N P U T ******     
C  B  THE MATRIX OF ORDER  N  WHOSE EIGENVALUE/VECTOR IS SOUGHT.  
C  N  ORDER OF THE MATRIX  B .
C  EGUESS  A FIRST GUESS FOR THE EIGENVALUE.    
C  VGUESS  N-VECTOR CONTAINING A FIRST GUESS FOR THE EIGENVECTOR. 
C******  W O R K  A R E A  ******   
C  W  MATRIX OF ORDER  N
C  D  VECTOR OF LENGTH  N     
C  IPIVOT  INTEGER VECTOR OF LENGTH  N    
C******  O U T P U T  ******  
C  EVALUE  COMPUTED APPROXIMATION TO EIGENVALUE 
C  VECTOR  COMPUTED APPROXIMATION TO EIGENVECTOR
C  IFLAG  AN INTEGER,   
C         = 1 OR -1 (AS SET IN FACTOR), INDICATES THAT ALL IS WELL,     
C         = 0 , INDICATES THAT SOMETHING WENT WRONG. SEE PRINTED ERROR  
C               MESSAGE .     
C******  M E T H O D  ******  
C     INVERSE ITERATION, AS DESCRIBED IN THE TEXT, IS USED. 
C******     
C  THE FOLLOWING  T E R M I N A T I O N  P A R A M E T E R S  ARE SET   
C  HERE, A TOLERANCE  E P S L O N  ON THE DIFFERENCE BETWEEN SUCCESSIVE 
C  EIGENVALUE ITERATES, AND AN UPPER BOUND  I T E R M X  ON THE NUMBER  
C  OF ITERATION STEPS.  
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c
      DATA EPSLON,ITERMX /.000001,20/     
C     
C                      PUT  B - (EGUESS)*IDENTITY   INTO  W 
      DO 10 J=1,N 
         DO 9 I=1,N     
    9       W(I,J) = B(I,J)   
   10    W(J,J) = W(J,J) - EGUESS   
      CALL FACTOR ( W, N, D, IPIVOT, IFLAG )    
      IF (IFLAG .EQ. 0)  THEN 
         PRINT 610
  610    FORMAT(' EIGENVALUE GUESS TOO CLOSE. ' 
     *         ,'NO EIGENVECTOR CALCULATED.')   
                                        RETURN  
      END IF
C                       ITERATION STARTS HERE   
      PRINT 619   
  619 FORMAT(' ITER  EIGENVALUE      EIGENVECTOR COMPONENTS'/)    
      EVOLD = 0.  
      DO 50 ITER=1,ITERMX     
C                       NORMALIZE CURRENT VECTOR GUESS
         SQNORM = 0.    
         DO 20 I=1,N    
   20       SQNORM = VGUESS(I)**2 + SQNORM
         SQNORM = SQRT(SQNORM)
         DO 21 I=1,N    
   21       VGUESS(I) = VGUESS(I)/SQNORM  
C                       GET NEXT VECTOR GUESS   
         CALL SUBST ( W, IPIVOT, VGUESS, N, VECTOR )  
C                       CALCULATE RAYLEIGH QUOTIENT   
         EVNEW = 0.     
         DO 30 I=1,N    
   30       EVNEW = VGUESS(I)*VECTOR(I) + EVNEW 
         EVALUE = EGUESS + 1./EVNEW 
C     
         PRINT 630,ITER,EVALUE,VECTOR     
  630    FORMAT(I3,E15.7,2X,3E14.7/(20X,3E14.7))
C                       STOP ITERATION IF CURRENT GUESS IS CLOSE TO     
C                       PREVIOUS GUESS FOR EIGENVALUE 
         IF ( ABS(EVNEW-EVOLD) .LE. EPSLON*ABS(EVNEW) )     
     *                                  RETURN  
         EVOLD = EVNEW  
         DO 50 I=1,N    
   50       VGUESS(I) = VECTOR(I)   
C     
      IFLAG = 0   
      PRINT 660,EPSLON,ITERMX 
  660 FORMAT(' NO CONVERGENCE TO WITHIN',E10.4,' AFTER',I3,' STEPS.')   
                                        RETURN  
      END   
