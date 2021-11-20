      program lgndre_test

CHECKOUT LEGENDRE POINT AND WEIGHT ROUTINE BY INTEGRATING THE FIRST     
C  FEW POWERS OVER (-1,1).    

      parameter ( nmax = 19)
      parameter (KMAX=9)
      INTEGER I,K,N     
      REAL ANSWER(NMAX),ERROR(NMAX),P(KMAX),SUM,W(KMAX)     

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'lgndre_test'
      write ( *, '(a)' ) '  Test lgndre()'

      DO N=2,NMAX,2   
        ANSWER(N) = 0. 
      end do

      DO N=1,NMAX,2   
        ANSWER(N) = 2./N   
      end do  

      DO 30 K=1,KMAX    
         CALL LGNDRE( K, P, W )     
         DO 20 N=1,NMAX 
            SUM = 0.    
            IF (N .EQ. 1) THEN
               DO 9 I=1,K     
    9             SUM = SUM + W(I)  
            ELSE  
               DO 10 I=1,K    
   10             SUM = SUM + W(I)*P(I)**(N-1)  
            END IF
   20       ERROR(N) = ANSWER(N) - SUM    
   30    PRINT 630,K,(ERROR(N),N=1,NMAX)  
  630 FORMAT(I3,9E8.1/(5X,9E8.1))   
  
      STOP 0
      END   
