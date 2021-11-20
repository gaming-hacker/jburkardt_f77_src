      program fft_test

      integer i
      integer n

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'fft_test'
      write ( *, '(a)' ) '  Test fft()'

      n = 1
      do i = 1, 10
        n = n * 2
        call fft_test_sub ( n )
      end do

      stop 0
      end

      subroutine fft_test_sub ( n )

C  TEST OF FFT ROUTINE. INPUT ARE THE COMPLEX EXPONENTIALS, HENCE 
C  OUTPUT SHOULD BE DELTA FUNCTIONS 
      PARAMETER (NMAX=2049 )
      INTEGER INZEE,J,K,N     
      REAL ANGLE,ERROR,JOVERN,TWOPI 
      COMPLEX Z1(NMAX),Z2(NMAX)     
      DATA TWOPI / 6.2831853071795864769 /   
c   1 READ 500,N  
c 500 FORMAT(I4)  
      IF (N .LT.2 .OR. N .GT. NMAX)     STOP    
      ERROR = 0.  
      DO 10 J=1,N 
C         PUT EXP( . *2PI*J/N) INTO  F    
         JOVERN = FLOAT(J-1)/FLOAT(N)     
         DO 5 K=1,N     
            ANGLE = TWOPI*FLOAT(K-1)*JOVERN     
    5       Z1(K) = CMPLX(COS(ANGLE), SIN(ANGLE))     
         INZEE = 1
         CALL FFT( Z1, Z2, N, INZEE )     
         IF (INZEE .EQ. 1) THEN     
            DO 7 K=1,N  
    7          Z2(K) = 0.     
            Z2(J) = CMPLX(FLOAT(N),0.)    
         ELSE     
            DO 8 K=1,N  
    8          Z1(K) = 0.     
            Z1(J) = CMPLX(FLOAT(N),0.)    
         END IF   
         DO 9 K=1,N     
            ERROR = MAX(ERROR,ABS(Z1(K)-Z2(K))) 
    9    CONTINUE 
   10 CONTINUE    
      PRINT 610,N,ERROR 
  610 FORMAT(I4,E10.3)  
c                                       GO TO 1 
      return
      END   
