C     EXAMPLE 7.3 GAUSSIAN INTEGRATION    
      REAL INTGRL,P(4),WEIGHT(4)    
      F(T) = SIN(T+2.)**2/(T+2.)    
      CALL LGNDRE ( 4, P, WEIGHT )  
      INTGRL = WEIGHT(1)*(F(P(1))+F(P(4))) + WEIGHT(2)*(F(P(2))+F(P(3)))
      PRINT 600,INTGRL  
  600 FORMAT(' EXAMPLE 7.3. GAUSS QUADRATURE'/' INTEGRAL = ',1PE14.7)   
                                        STOP    
      END   
