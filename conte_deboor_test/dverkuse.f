         X = XBEGIN     
         Y = YBEGIN     
         XEND = XBEGIN + K*(XM - XBEGIN)/M
         CALL DVERK( N, FCN1, X, Y, XEND, TOL, IND, C, NW, W,  IER )    
         PRINT 600, XEND,Y(1),C(24) 
  600    FORMAT(F19.6,E21.8,F1616.0)
   10 CONTINUE    
