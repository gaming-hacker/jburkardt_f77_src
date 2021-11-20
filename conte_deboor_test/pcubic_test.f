      program pcubic_test

C  PROGRAM TO TEST  PCUBIC .  
      PARAMETER (N=5)
      parameter ( NP1=N+1 )
      parameter ( NPOINT=5 )    
      INTEGER I   
      REAL C(4,NP1),XI(NP1),X(NPOINT),Y(NPOINT) 
      DATA XI /1.,2.,3.,5.,6.,7. /  
      DATA X / 7., 2.5, 5.5, 1.5, 0. /    

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'pcubic_test'
      write ( *, '(a)' ) '  Test pcubic().'

      DO I=1,N+1     
         C(1,I) = (XI(I)-3.)**3     
         C(2,I) = 3.*(XI(I)-3.)**2  
      end do

      CALL CALCCF ( XI, C, N )

      DO I=1,NPOINT  
        Y(I) = PCUBIC ( X(I), XI, C, N ) 
      end do

      PRINT 620, (X(I),Y(I),I=1,NPOINT)   
  620 FORMAT(2E17.7)  
  
      STOP 0
      END   
