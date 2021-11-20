      SUBROUTINE MULLER ( FN, FNREAL, ZEROS, N, NPREV, MAXIT, EP1, EP2 )
C  DETERMINES UP TO  N  ZEROS OF THE FUNCTION SPECIFIED BY  FN , USING  
C  QUADRATIC INTERPOLATION, I.E., MUELLER'S METHOD .  
      EXTERNAL FN 
      LOGICAL FNREAL    
      INTEGER MAXIT,N,NPREV,   KOUNT
      REAL EP1,EP2,   EPS1,EPS2     
      COMPLEX ZEROS(N),   C,DEN,DIVDF1,DIVDF2,DVDF1P,FZR,FZRDFL   
     *       ,FZRPRV,H,ZERO,SQR     
      complex hprev
C******  I N P U T  ******    
C  FN  NAME OF A SUBROUTINE, OF THE FORM  FN(Z, FZ)  WHICH, FOR GIVEN   
C       Z , RETURNS  F(Z) . MUST APPEAR IN AN  E X T E R N A L  STATE-  
C      MENT IN THE CALLING PROGRAM .
C  FNREAL  A LOGICAL VARIABLE. IF .TRUE., ALL APPROXIMATIONS ARE TAKEN  
C      TO BE REAL, ALLOWING THIS ROUTINE TO BE USED EVEN IF  F(Z)  IS   
C      ONLY DEFINED FOR REAL  Z .   
C  ZEROS(1),...,ZEROS(NPREV)  CONTAINS PREVIOUSLY FOUND ZEROS (IF 
C      NPREV .GT. 0).   
C  ZEROS(NPREV+1),...,ZEROS(N)  CONTAINS FIRST GUESS FOR THE ZEROS TO BE
C      FOUND. (IF YOU KNOW NOTHING, 0  IS AS GOOD A GUESS AS ANY.)
C  MAXIT  MAXIMUM NUMBER OF FUNCTION EVALUATIONS ALLOWED PER ZERO.
C  EP1  ITERATION IS STOPPED IF ABS(H) .LT. EP1*ABS(ZR), WITH     
C       H = LATEST CHANGE IN ZERO ESTIMATE  ZERO .    
C  EP2  ALTHOUGH THE  EP1  CRITERION IS NOT MET, ITERATION IS STOPPED IF
C        ABS(F(ZERO)) .LT. EP2 .    
C  N  TOTAL NUMBER OF ZEROS TO BE FOUND . 
C  NPREV  NUMBER OF ZEROS FOUND PREVIOUSLY .    
C******  O U T P U T  ******  
C  ZEROS(NPREV+1), ..., ZEROS(N)  APPROXIMATIONS TO ZEROS .
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c
 
C     
C               INITIALIZATION
      EPS1 = MAX(EP1, 1.E-12) 
      EPS2 = MAX(EP2, 1.E-20) 
C     
      DO 100 I=NPREV+1,N
         KOUNT = 0
C               COMPUTE FIRST THREE ESTIMATES FOR ZERO AS   
C                  ZEROS(I)+5., ZEROS(I)-.5, ZEROS(I) 
    1    ZERO = ZEROS(I)
         H = .5   
         CALL DFLATE(FN, ZERO+.5, I, KOUNT, FZR, DVDF1P, ZEROS, *1)     
         CALL DFLATE(FN, ZERO-.5, I, KOUNT, FZR, FZRPRV, ZEROS, *1)     
         HPREV = -1.    
         DVDF1P = (FZRPRV - DVDF1P)/HPREV 
         CALL DFLATE(FN, ZERO, I, KOUNT, FZR, FZRDFL, ZEROS, *1)  
C        DO WHILE  KOUNT.LE.MAXIT OR  H  IS RELATIVELY BIG  
C            OR  FZR = F(ZERO)  IS NOT SMALL    
C            OR  FZRDFL = FDEFLATED(ZERO)  IS NOT SMALL OR NOT MUCH     
C             BIGGER THAN ITS PREVIOUS VALUE  FZRPRV .
   40       DIVDF1 = (FZRDFL - FZRPRV)/H  
            DIVDF2 = (DIVDF1 - DVDF1P)/(H + HPREV)    
            HPREV = H   
            DVDF1P = DIVDF1   
            C = DIVDF1 + H*DIVDF2   
            SQR = C*C - 4.*FZRDFL*DIVDF2  
            IF (FNREAL .AND. REAL(SQR) .LT. 0.)  SQR = 0.   
            SQR = SQRT(SQR)   
            IF (REAL(C)*REAL(SQR)+AIMAG(C)*AIMAG(SQR) .LT. 0.) THEN     
               DEN = C - SQR  
            ELSE  
               DEN = C + SQR  
            END IF
            IF (ABS(DEN) .LE. 0.)  DEN = 1.     
            H = -2.*FZRDFL/DEN
            FZRPRV = FZRDFL   
            ZERO = ZERO + H   
            IF (KOUNT .GT. MAXIT)       GO TO 99
C     
   70       CALL DFLATE(FN, ZERO, I, KOUNT, FZR, FZRDFL, ZEROS, *1)     
C                     CHECK FOR CONVERGENCE     
            IF (ABS(H) .LT. EPS1*ABS(ZERO)) GO TO 99  
            IF (MAX(ABS(FZR),ABS(FZRDFL)) .LT. EPS2)  GO TO 99    
C                     CHECK FOR DIVERGENCE
            IF (ABS(FZRDFL) .GE. 10.*ABS(FZRPRV))  THEN     
               H = H/2. 
               ZERO = ZERO - H
                                        GO TO 70
            ELSE  
                                        GO TO 40
            END IF
   99    ZEROS(I) = ZERO
  100 CONTINUE    
                                        RETURN  
      END   
      SUBROUTINE DFLATE ( FN, ZERO, I, KOUNT, FZERO, FZRDFL, ZEROS, * ) 
C  TO BE CALLED IN   M U L L E R    
      INTEGER I,KOUNT,  J     
      COMPLEX FZERO,FZRDFL,ZERO,ZEROS(I),  DEN  
      KOUNT = KOUNT + 1 
      CALL FN(ZERO, FZERO)    
      FZRDFL = FZERO    
      IF (I .LT. 2)                     RETURN  
      DO 10 J=2,I 
         DEN = ZERO - ZEROS(J-1)    
         IF (ABS(DEN) .EQ. 0.) THEN 
            ZEROS(I) = ZERO*1.001   
                                        RETURN 1
         ELSE     
            FZRDFL = FZRDFL/DEN     
         END IF   
   10 CONTINUE    
                                        RETURN  
      END   
