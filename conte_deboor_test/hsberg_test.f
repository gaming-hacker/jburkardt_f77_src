      program hsberg_test

      integer n
      parameter ( n = 5 )

      real h(n,n+2)
      real hcopy(n,n)
      real hinfo(n,n+2)
      integer i
      integer j

C  TRY OUT HESSENBERG TRANSFORMATION ROUTINE  HSBERG  

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'hsberg_test'
      write ( *, '(a)' ) '  Test hsberg()'

      do i = 1, n
        do j = 1, n
          h(i,j) = 10 * i + j
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Original matrix:'
      write ( *, '(a)' ) ''
      DO I=1,N 
         PRINT 620,(H(i,J),J=1,n)     
      end do

      CALL HSBERG ( H ,N ) 

      do i = 1, n
        do j = 1, i - 2
          hcopy(i,j) = 0.0
        end do
        do j = i - 1, n
          hcopy(i,j) = h(i,j)
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Hessenberg form:'
      write ( *, '(a)' ) ''
      DO I=1,N 
        PRINT 620,(hcopy(i,J),J=1,n)   
      end do  

      do i = 1, n
        do j = 1, i - 2
          hinfo(i,j) = h(i,j)
        end do
        do j = i - 1, n
          hinfo(i,j) = 0.0
        end do
        hinfo(i,n+1) = h(i,n+1)
        hinfo(i,n+2) = h(i,n+2)
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Hessenberg info:'
      write ( *, '(a)' ) ''
      DO I=1,N 
         PRINT 620,(hinfo(i,J),J=1,n+2)   
      end do  

  620 FORMAT(1X,7E14.6)
      stop 0
      END   
