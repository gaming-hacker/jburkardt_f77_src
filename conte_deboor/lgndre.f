      SUBROUTINE LGNDRE ( NP , POINT, WEIGHT )
  
C  SUPPLIES POINTS AND WEIGHTS FOR GAUSS-LEGENDRE QUADRATURE.     
C  INTEGRAL(F(X), -1 .LE. X .LE. 1)  IS APPROXIMATELY EQUAL TO    
C           SUM(F(POINT(I))*WEIGHT(I), I=1,...,NP) . 
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c 
      INTEGER NP,   I   
      REAL POINT(NP),WEIGHT(NP)     
      IF (NP .GT. 9) THEN     
         PRINT 600,NP   
  600    FORMAT(' THE GIVEN NUMBER NP =',I2,' IS GREATER THAN 9.' 
     *        /' EXECUTION STOPPED IN SUBROUTINE  L G N D R E .') 
                                        STOP 1
      END IF
                                        GO TO (1,2,3,4,5,6,7,8,9),NP    
    1 POINT(1) = 0.     
      WEIGHT(1) = 2.    
                                        GO TO 99
    2 POINT(2) = .577350269189626e0    
      WEIGHT(2) = 1.    
                                        GO TO 95
    3 POINT(2) = 0.     
      POINT(3) = .774596669241483e0    
      WEIGHT(2) = .8888888888888889e0 
      WEIGHT(3) = .5555555555555556e0 
                                        GO TO 95
    4 POINT(3) = .339981043584856e0    
      POINT(4) = .861136311594053e0    
      WEIGHT(3) = .652145154862546e0   
      WEIGHT(4) = .347854845137454e0   
                                        GO TO 95
    5 POINT(3) = 0.     
      POINT(4) = .538469310105683e0    
      POINT(5) = .906179845938664e0    
      WEIGHT(3) = .5688888888888889e0 
      WEIGHT(4) = .478628670499366e0   
      WEIGHT(5) = .236926885056189e0   
                                        GO TO 95
    6 POINT(4) = .238619186083197e0    
      POINT(5) = .661209386466265e0    
      POINT(6) = .932469514203152e0    
      WEIGHT(4) = .467913934572691e0   
      WEIGHT(5) = .360761573048139e0   
      WEIGHT(6) = .171324492379170e0   
                                        GO TO 95
    7 POINT(4) = 0.     
      POINT(5) = .405845151377397e0    
      POINT(6) = .741531185599394e0    
      POINT(7) = .949107912342759e0    
      WEIGHT(4) = .417959183673469e0   
      WEIGHT(5) = .381830050505119e0   
      WEIGHT(6) = .279705391489277e0   
      WEIGHT(7) = .129484966168870e0   
                                        GO TO 95
    8 POINT(5) = .183434642495650e0    
      POINT(6) = .525532409916329e0    
      POINT(7) = .796666477413627e0    
      POINT(8) = .960289856497536e0    
      WEIGHT(5) = .362683783378362e0   
      WEIGHT(6) = .313706645877887e0   
      WEIGHT(7) = .222381034453374e0   
      WEIGHT(8) = .101228536290376e0   
                                        GO TO 95
    9 POINT(5) = 0.     
      POINT(6) = .324253423403809e0    
      POINT(7) = .613371432700590e0    
      POINT(8) = .836031107326636e0    
      POINT(9) = .968160239507626e0    
      WEIGHT(5) = .330239355001260e0   
      WEIGHT(6) = .312347077040003e0   
      WEIGHT(7) = .260610696402935e0   
      WEIGHT(8) = .180648160694857e0   
      WEIGHT(9) = .081274388361574e0   
C     
   95 DO 96 I=1,NP/2    
         POINT(I) = -POINT(NP+1-I)  
   96    WEIGHT(I) = WEIGHT(NP+1-I) 
   99                                   RETURN  
      END   
