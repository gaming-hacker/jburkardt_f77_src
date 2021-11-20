CHAPTER 6, EXAMPLE 4. INTERPOLATION TO TAN(PI X/4) AT CHEBYSHEV POINTS  
      INTEGER I,ISTEP,J,K,N,NMK,NM1 
      REAL D(20),DX,ERRMAX,G,PNATX,STEP,TAU(20),X     
      DATA STEP, ISTEP /40., 40/    
      G(X) = TAN(0.785398164*X)     
       N = 6
C        CHOOSE INTERPOLATION POINTS  TAU(1), ..., TAU(N) , AS THE
C        CHEBYSHEV POINTS, AND SET  D(I) = G(TAU(I)), I=1,...,N.  
         NM1 = N-1
         PIOVN = 3.1415926535/FLOAT(2*N)  
               FACTOR = COS(PIOVN)  
         DO 10 I=1,N    
            TAU(I) = COS(FLOAT(2*I-1)*PIOVN)/FACTOR   
   10       D(I) = G(TAU(I))  
C        CALCULATE THE DIVIDED DIFFERENCES FOR THE NEWTON FORM.   
C     
         DO 20 K=1,NM1  
            NMK = N-K   
            DO 20 I=1,NMK     
   20          D(I) = (D(I+1)-D(I))/(TAU(I+K)-TAU(I)) 
C     
C        ESTIMATE MAX.INTERPOLATION ERROR ON (-1,1).  
         ERRMAX = 0.    
         DO 30 I=2,N    
            DX = (TAU(I)-TAU(I-1))/STEP   
            DO 30 J=1,ISTEP   
               X = TAU(I-1) + FLOAT(J)*DX 
C              EVALUATE INTERP.POL. BY NESTED MULTIPLICATION
C     
               PNATX = D(1)   
               DO 29 K=2,N    
   29             PNATX = D(K) + (X-TAU(K))*PNATX     
C     
   30          ERRMAX = AMAX1(ERRMAX,ABS(G(X)-PNATX)) 
       PRINT 640,ERRMAX 
  640 FORMAT(' MAX.ERROR = ',E15.7) 
                                        STOP    
      END   
