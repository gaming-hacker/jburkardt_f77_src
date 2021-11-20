      REAL FUNCTION FL(X)     
C  RETURNS THE VALUE OF  X  ROUNDED TO  NDIGITS  SIGNIFICANT DIGITS.    
      COMMON /LENGTH/ NDIGIT  
      FL = ABS(X) 
      IF (FL .EQ. 0.)                   RETURN  
      DIGITS = ALOG10(FL)     
      ND = IFIX(DIGITS) 
      IF (DIGITS .GE. 0.)  ND = ND + 1    
      POWER = 10.**(NDIGIT - ND)    
      FL = SIGN(1.,X)*FLOAT(IFIX(FL*POWER + .5))/POWER
                                        RETURN  
      END   
