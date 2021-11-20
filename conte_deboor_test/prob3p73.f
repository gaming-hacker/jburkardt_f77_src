C  PROGRAM TO TRY OUT MUELLER,S METHOD .  
      COMPLEX ZEROS(10)    
      COMMON /MULFUN/ N   
      EXTERNAL FN 

      DO 10 I=1,3     
   10    ZEROS(I) = 0.  
      do 20 N=3,4
         CALL MULLER ( FN, .FALSE., ZEROS, 3, 0, 20, 5.E-8, 5.E-8) 
         PRINT 600,(ZEROS(I),I=1,N-1)  
  600    FORMAT(2E17.9)    
   20 continue
      END   
      SUBROUTINE FN ( ZR, FZR )     
c      this is the function for problem 3.7-3 . 
c      only  N  matters here
      COMPLEX ZR,FZR, piov2x 
      COMMON /MULFUN/ N 
c the series in question is of the form  \sum_n a(n), with
c  a(n) = - a(n-1) (\pi/2)^2 x^4 (4n-3)/((4n+1)(2n)(2n-1))
c  hence amenable to nested multiplication
      piov2x = (zr**2*3.1415926535/2)**2
      FZR = piov2x*(4*n-3)/((4*n+1)*(2*n)*(2*n-1))
      DO 10 j=n-1,1,-1
   10    FZR = 1 - FZR*piov2x*(4*j-3)/((4*j+1)*(2*j)*(2*j-1))
	  FZR = FZR*ZR
      PRINT 600,ZR,FZR  
  600 FORMAT(2E15.7,3X,2E15.7)
                                        RETURN  
      END   
