C  PROGRAM FOR EXAMPLE 7.8. ROMBERG EXTRAPOLATION .   
      REAL T(100) 
      EXTERNAL FERR     
      CALL RMBERG( FERR, 0., 1., 2, T, 6 )
                                        STOP    
      END   
      SUBROUTINE RMBERG ( F, A, B, MSTART, T, NROW )  
C  CONSTRUCTS AND PRINTS OUT THE FIRST  NROW  ROWS OF THE ROMBERG T-    
C  TABLE FOR THE INTEGRAL OF  F(X)  FROM  A  TO  B  , STARTING WITH THE 
C  TRAPEZOIDAL SUM ON  MSTART  INTERVALS. 
      INTEGER MSTART,NROW,   I,K,M  
      REAL A,B,T(NROW,NROW),   H,SUM
      M = MSTART  
      H = (B-A)/M 
      SUM = (F(A) + F(B))/2.  
      IF (M .GT. 1) THEN
         DO 10 I=1,M-1  
   10       SUM = SUM + F(A+FLOAT(I)*H)   
      END IF
      T(1,1) = SUM*H    
      PRINT 610   
  610 FORMAT('1',10X,'ROMBERG T-TABLE'//) 
      PRINT 611, T(1,1) 
  611 FORMAT(7E15.7)    
      IF (NROW .LT. 2)                  RETURN  
C     
      DO 20 K=2,NROW    
         H = H/2. 
         M = M*2  
         SUM = 0. 
         DO 11 I=1,M,2  
   11       SUM = SUM + F(A+FLOAT(I)*H)   
         T(K,1) = T(K-1,1)/2. + SUM*H     
         DO 12 J=1,K-1  
C              SAVE DIFFERENCES FOR LATER CALC. OF RATIOS   
            T(K-1,J) = T(K,J) - T(K-1,J)  
   12       T(K,J+1) = T(K,J) + T(K-1,J)/(4.**J - 1.) 
   20    PRINT 611, (T(K,J),J=1,K)  
      IF (NROW .LT. 3)                  RETURN  
C                 CALCULATE RATIOS  
      PRINT 620   
  620 FORMAT(///11X,'TABLE OF RATIOS'//)  
      DO 30 K=1,NROW-2  
         DO 25 J=1,K    
            IF (T(K+1,J) .EQ. 0.) THEN    
               RATIO= 0.
            ELSE  
               RATIO = T(K,J)/T(K+1,J)    
            END IF
   25       T(K,J) = RATIO    
   30    PRINT 630, (T(K,J),J=1,K)  
  630 FORMAT(8F10.2)    
                                        RETURN  
      END   
      REAL FUNCTION FERR(X)   
      REAL X
      FERR = EXP(-X*X)  
                                        RETURN  
      END   
