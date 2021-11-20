C  TEST FL  
      COMMON /LENGTH/ NDIGIT  
      READ 499,NDIGIT   
  499 FORMAT(I1)  
      IF (NDIGIT .LE. 0) then
        STOP 0
      end if    
    1 READ 500,X  
  500 FORMAT(E15.8)     
      R = FL(X)   
      PRINT 600,X,R     
  600 FORMAT(E15.8,E20.8)     
                                        GO TO 1 
      END   
