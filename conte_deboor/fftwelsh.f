      SUBROUTINE FFTWELSH (U,DUMMY,N,INZEE)    
      INTEGER N   
      COMPLEX A,U(N),T,W
      NV2 = N/2   
      NM1 = N-1   
      J = 1 
      DO 50 I=1,NM1     
         IF (I-J)                       10,20,20
   10    T = U(J) 
         U(J)=U(I)
         U(I) = T 
   20    K = NV2  
   30    IF (K-J)                       40,50,50
   40    J = J-K  
         K = K/2  
                                        GO TO 30
   50    J = J+K  
         M = int ( 1.4427*ALOG(FLOAT(N)) )
         LE =1    
         DO 100 L=1,M   
            LE1 = LE    
            LE = LE*2   
            A = CMPLX(1.,0.)  
            ANGLE = 3.141592653589/FLOAT(LE1)   
            W = CMPLX(COS(ANGLE),SIN(ANGLE))    
            DO 100 J=1,LE1    
              DO 90 I=J,N,LE  
                  IP= I + LE1 
                  T = U(IP)*A 
                  U(IP) = U(I) - T  
   90             U(I) = U(I)+T     
  100          A = A*W  
                                        RETURN  
      END   
