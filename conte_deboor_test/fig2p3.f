C  THIS PROGRAM PLOTS THE ERROR TERM IN POLYLNOMIAL INTERPOLATION 
C     
      DIMENSION X(20),V(1000) 
    1 READ 501,N,INDEX  
  501 FORMAT(2I3) 
      IF (N .LT. 2)                     STOP    
                                        GO TO (2,4),INDEX   
    2 DO 3 I=1,N  
   3     X(I) = FLOAT(I-1)    
                                        GO TO 6 
    4 PIOVP = 3.1415926535/FLOAT(2*N)     
      FNM1 = FLOAT(N-1)/2.    
      DO 5 I=1,N  
    5    X(I) = (1. - COS(FLOAT(2*I-1)*PIOVP))*FNM1   
    6 READ 506,XBEGIN,XEND,STEP     
  506 FORMAT(F10.5)     
      DO 50 IP=1,1000   
         XBAR = (IP-1)*STEP + XBEGIN
         IF (XBAR .GT. XEND)            GO TO 60
         VALUE = XBAR - X(1)  
         DO 20 J=2,N    
   20       VALUE = VALUE*(XBAR - X(J))   
   50    V(IP) = ABS(VALUE)   
   60 IMAX = IP - 1     
      PRINT 660,X(1),X(N),STEP,(V(I),I=1,IMAX)  
  660 FORMAT(2F15.7,E20.7/(5F9.2))  
                                        GO TO 1 
      END   
