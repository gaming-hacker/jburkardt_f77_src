      program table_test

      DIMENSION X(100),F(100) 
      DIMENSION XBAR(10)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'table_test'
      write ( *, '(a)' ) '  Test table().'

      NTABLE = 25 
      XBAR(1) = -2.     
      XBAR(2) = 0.
      XBAR(3) = 3.1415926535/3.     
      X(1) = -1.  
      DX = .1     

      DO I=2,NTABLE  
         X(I) = X(I-1) + DX   
         F(I) = FF(X(I))
      end do

      DO 20 J=1,3 
         FFXBAR = TABLE(XBAR(J),X,F,NTABLE,1.E-7,IFLAG)     
         FXBAR = FF(XBAR(J))  
   20    PRINT 620,XBAR(J),FFXBAR,FXBAR,IFLAG   
  620 FORMAT(3E17.7,I6) 
      STOP 0
      END   
      FUNCTION FF(X)    
      FF = SIN(X) 
      RETURN
      END   
