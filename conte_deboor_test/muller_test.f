      program muller_test

C  PROGRAM TO TRY OUT MUELLER,S METHOD .  
      COMPLEX A, ZEROS(10)    
      COMMON /MULFUN/ N,A(11)    
      EXTERNAL FN

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'muller_test'
      write ( *, '(a)' ) '  Test muller()'

      a(1) = 1.0
      a(2) = 0.0
      a(3) = 1.0
      a(4) = -3.0
      n = 4
 
c   1 READ 500,N  
c 500 FORMAT(I2)  
c     IF (N .LE. 0)                     STOP    
c     READ 501,(A(I),I=1,N)   
c 501 FORMAT(2E15.7)    

      DO I=1,N-1     
        ZEROS(I) = 0. 
      end do
 
      CALL MULLER ( FN, .FALSE., ZEROS, N-1, 0, 20, 5.E-8, 5.E-8) 
      PRINT 600,(ZEROS(I),I=1,N-1)  
  600 FORMAT(2E17.9)    
      stop 0
      END   
      SUBROUTINE FN ( ZR, FZR )     
      COMPLEX A, ZR,FZR 
      COMMON /MULFUN/ N,A(11) 
      FZR = A(1)  
      DO 10 I=2,N 
   10    FZR = FZR*ZR + A(I)  
      PRINT 600,ZR,FZR  
  600 FORMAT(2E15.7,3X,2E15.7)
                                        RETURN  
      END   
