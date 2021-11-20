C  TEST OF FFT ROUTINE. INPUT ARE THE COMPLEX EXPONENTIALS, HENCE 
C  OUTPUT SHOULD BE DELTA FUNCTIONS 
      PARAMETER ( NMAX=2049 )  
      INTEGER INZEE,J,K,N     
      REAL ANGLE,ERROR,JOVERN,TWOPI 
      COMPLEX Z(NMAX),ZBAR(NMAX)    
      DATA TWOPI / 6.2831853071795864769 /   
    1 READ 500,N  
  500 FORMAT(I4)  
      IF (N .LT.2 .OR. N .GT. NMAX)     STOP    
      DO 10 J=1,N 
C         PUT EXP(-.2PI*J/N) INTO  F
         JOVERN = FLOAT(J)/FLOAT(N) 
         DO 5 K=1,N     
            ANGLE = TWOPI*FLOAT(K-1)*JOVERN     
    5       Z(K) = CMPLX(COS(ANGLE), -SIN(ANGLE))     
         INZEE = 1
         CALL FFT( Z, ZBAR, N, INZEE )    
         IF (INZEE .EQ. 1) THEN     
            PRINT 607,J,(Z(K),K=1,N)
 607        FORMAT(I5/(6E12.5))     
         ELSE     
            PRINT 607,J,(ZBAR(K),K=1,N)   
         END IF   
   10 CONTINUE    
      PRINT 610,N,ERROR 
  610 FORMAT(I4,E10.3)  
                                        GO TO 1 
      END   
