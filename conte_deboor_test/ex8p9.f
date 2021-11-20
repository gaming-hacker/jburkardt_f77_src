C   PROGRAM TO SOLVE EXAMPLE 8.9 USING  D V E R K  (IMSL) . 
      INTEGER IER,IND,K,N,NW  
      REAL C(24),TOL,W(5,9),X,XEND,Y(4)   
      DATA N , X ,       Y         , TOL ,IND,NW
     *   / 4 , 0.,   0.,1.,0.,-2.  ,1.E-9, 1 , 5 /    
      EXTERNAL FCN2     
      DO 12 K=1,10
         XEND = FLOAT(K)/10.  
         CALL DVERK ( N, FCN2, X, Y, XEND, TOL, IND, C, NW, W, IER )    
         PRINT 600, XEND,Y(1),Y(2),C(24)  
  600    FORMAT(3X,F3.1,3X,2(E16.8,3X),F4.0)    
   12 CONTINUE    
                                        STOP    
      END   
      SUBROUTINE FCN2 ( N, X, Y, YPRIME ) 
      INTEGER N   
      REAL X, Y(N), YPRIME(N) 
      YPRIME(1) = Y(3)  
      YPRIME(2) = Y(4)  
      YPRIME(3) = Y(1)**2 - Y(2) + EXP(X) 
      YPRIME(4) = Y(1) - Y(2)**2 - EXP(X) 
                                        RETURN  
      END   
