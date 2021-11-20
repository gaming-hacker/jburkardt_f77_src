      REAL FUNCTION TABLE (XBAR, X, F, NTABLE, TOL, IFLAG ) 

C  RETURNS AN INTERPOLATED VALUE  TABLE  AT  XBAR  FOR THE FUNCTION     
C  TABULATED AS (X(I),F(I)), I=1,...,NTABLE.    
      INTEGER IFLAG,NTABLE,   J,NEXT,NEXTL,NEXTR
      REAL F(NTABLE),TOL,X(NTABLE),XBAR,   A(20),ERROR,PSIK,XK(20)
C******  I N P U T  ******    
C  XBAR  POINT AT WHICH TO INTERPOLATE .  
C  X(I), F(I), I=1,...,NTABLE  CONTAINS THE FUNCTION TABLE .
C     A S S U M P T I O N ...  X IS ASSUMED TO BE INCREASING.     
C  NTABLE  NUMBER OF ENTRIES IN FUNCTION TABLE. 
C  TOL  DESIRED ERROR BOUND . 
C******  O U T P U T  ******  
C  TABLE  THE INTERPOLATED FUNCTION VALUE .     
C  IFLAG  AN INTEGER,   
C     = 1 , SUCCESSFUL EXECUTION ,  
C     = 2 , UNABLE TO ACHIEVE DESIRED ERROR IN 20 STEPS,    
C     = 3 , XBAR LIES OUTSIDE OF TABLE RANGE. CONSTANT EXTRAPOLATION IS 
C           USED. 
C******  M E T H O D  ******  
C  A SEQUENCE OF POLYNOMIAL INTERPOLANTS OF INCREASING DEGREE IS FORMED 
C  USING TABLE ENTRIES ALWAYS AS CLOSE TO  XBAR  AS POSSIBLE. EACH IN-  
C  TERPOLATED VALUE IS OBTAINED FROM THE PRECEDING ONE BY ADDITION OF A 
C  CORRECTION TERM (AS IN THE NEWTON FORMULA). THE PROCESS TERMINATES   
C  WHEN THIS CORRECTION IS LESS THAN  TOL  OR, ELSE, AFTER  20  STEPS. 
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2. 
C     
C            LOCATE  XBAR  IN THE X-ARRAY.
      IF (XBAR .GE. X(1) .AND. XBAR .LE. X(NTABLE)) THEN    
         DO 10 NEXT=2,NTABLE  
            IF (XBAR .LE. X(NEXT))      GO TO 12
   10    CONTINUE 
      END IF
      IF (XBAR .LT. X(1)) THEN
         TABLE = F(1)   
      ELSE  
         TABLE = F(NTABLE)    
      END IF
      PRINT 610,XBAR    
  610 FORMAT(E16.7,' NOT IN TABLE RANGE.')
      IFLAG = 3   
                                        RETURN  
   12 XK(1) = X(NEXT)   
      NEXTL = NEXT-1    
      NEXTR = NEXT+1    
      A(1) = F(NEXT)    
      TABLE = A(1)
      PSIK = 1.   
C         USE ALGORITHM 2.4, WITH THE NEXT XK ALWAYS THE TABLE    
C        ENTRY NEAREST   XBAR  OF THOSE NOT YET USED. 
      KP1MAX = MIN(20,NTABLE) 
      DO 20 KP1=2,KP1MAX
         IF (NEXTL .EQ. 0) THEN     
            NEXT = NEXTR
            NEXTR = NEXTR+1   
         ELSE IF (NEXTR .GT. NTABLE) THEN 
            NEXT = NEXTL
            NEXTL = NEXTL-1   
         ELSE IF (XBAR - X(NEXTL) .GT. X(NEXTR) - XBAR) THEN
            NEXT = NEXTR
            NEXTR = NEXTR+1   
         ELSE     
            NEXT = NEXTL
            NEXTL = NEXTL-1   
         END IF   
         XK(KP1) = X(NEXT)    
         A(KP1) = F(NEXT)     
         DO 13 J=KP1-1,1,-1   
            A(J) = (A(J+1) - A(J))/(XK(KP1) - XK(J))  
   13    CONTINUE 
C         FOR I=1,...,KP1,  A(I)  NOW CONTAINS THE DIV.DIFF. OF   
C         F(X) OF ORDER  K-I  AT  XK(I),...,XK(KP1).  
         PSIK = PSIK*(XBAR - XK(KP1-1))   
         ERROR = A(1)*PSIK    
C                   TEMPORARY PRINTOUT    
            PRINT 613,KP1,XK(KP1),TABLE,ERROR   
  613       FORMAT(I10,3E17.7)
         TABLE = TABLE + ERROR
         IF (ABS(ERROR) .LE. TOL)  THEN   
            IFLAG = 1   
                                        RETURN  
         END IF   
   20 CONTINUE    
      PRINT 620,KP1MAX  
  620 FORMAT(' NO CONVERGENCE IN ',I2,' STEPS.')
      IFLAG = 2   
                                        RETURN  
      END   
