C  USE OF  D V E R K  TO SOLVE EXAMPLE 8.2 .    
      INTEGER IER,IND,K,N,NW  
      REAL C(24),TOL,W(1,9),X,XEND,Y(1)   
      DATA N , X ,Y(1), TOL ,IND,NW 
     *   / 1 , 1.,-1. ,1.E-7, 1 , 1 /     
      EXTERNAL FCN1     
      DO 10 K=1,4 
         XEND = 1. + FLOAT(K)/4.    
         CALL DVERK ( N, FCN1, X, Y, XEND, TOL, IND, C, NW, W, IER )    
         PRINT 600, XEND,Y(1),C(24) 
  600    FORMAT(11X,F8.6,5X,E16.8,5X,F11.0)     
   10 CONTINUE    
                                        STOP    
      END   
      SUBROUTINE FCN1 ( N, X, Y, YPRIME ) 
      REAL X,Y(1),YPRIME(1)   
      YPRIME(1) = (1./X - Y(1))/X - Y(1)*Y(1)   
                                        RETURN  
      END   
