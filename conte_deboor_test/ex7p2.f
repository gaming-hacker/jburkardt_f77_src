C     EXAMPLE 7.2 GAUSSIAN INTEGRATION    
      REAL INTGRL,P(5),WEIGHT(5)    
      F(T) = EXP(-(1.+T)**2/4.)/2.  
      CALL LGNDRE ( 5, P, WEIGHT )  
      INTGRL = WEIGHT(1)*(F(P(1))+F(P(5))) + WEIGHT(2)*(F(P(2))+F(P(4)))
     *         + WEIGHT(3)*F(P(3))  
      PRINT 600,INTGRL  
  600 FORMAT(' EXAMPLE 7.2. GAUSS QUADRATURE'/' INTEGRAL = ',1PE14.7)   
                                        STOP    
      END   
