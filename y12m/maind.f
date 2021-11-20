      program maind
c
c***********************************************************************
c
c     this program tests the subroutine of package y12m by
c     means of matrices of class d(n,c) and/or class e(n,c).
c     the program uses data consisting of 7 integers.
c     the first of these integers (in the program it is de-
c     noted by indexp) should be equal to:
c         1     if the matrices of class d(n,c) has to be used.
c         2     if the matrices of class e(n,c) has to be used.
c         3     if the matrices of both classes has to be used.
c     the next three integers (called nstart,nincr and nend
c     in the program) are used to determine the range of the
c     parameter n. nstart should be larger than 22. nincr
c     should be larger than zero. nend should be larger than
c     or equal to nstart. if these conditions are satisfied
c     then the range of n for the systems solved is:
c            n = nstart(nincr)nend
c     the last three integers (called cstart,cincr and cend
c     in the program) are used in the program to determine
c     the range of parameter c (the sparsity of the matrix
c     is changed by c). cstart should be chosen between 2 and
c     n-13. cincr should be larger than 1. cend should be
c     smaller than n-13. if these conditions are satisfied,
c     then the range of c for the systems solved is given as:
c            c = cstart(cincr)cend
c     in the version which is distributed with the package
c     the data read in line 12 are:
c         3   250  50  600  4  40  204
c     if these data are changed,then the parameter ibdim
c     skould be set equal to or larger than the value for
c     nend.
c
      integer iadim
      integer ibdim
      integer icdim
c
      parameter (ibdim=1000)
      parameter (iadim=25*ibdim)
      parameter (icdim=20*ibdim)
c
      real a(iadim)
      real aflag(8)
      real b(ibdim)
      integer c
      integer cend
      integer cincr
      integer cstart
      real errmax
      integer ha(ibdim,11)
      integer i
      integer ifail
      integer iflag(10)
      integer iha
      integer index
      integer indexp
      integer irnr(icdim)
      integer lrow
      integer n
      integer nend
      integer nincr
      integer nn
      integer nn1
      integer nonz
      integer nstart
      real pivot(ibdim)
      integer snr(iadim)
      real time
      real timer
c
      write(*,*)' '
      write(*,*)'Test for Y12M'

      iha=ibdim
      indexp=3
      nstart=250
      nincr=50
      nend=600
      cstart=4
      cincr=40
      cend=204

      write(*,*)' '
      write(*,*)'INDEXP=',indexp
      write(*,*)'NSTART=',nstart
      write(*,*)'NINCR=',nincr
      write(*,*)'NEND=',nend
      write(*,*)'CSTART=',cstart
      write(*,*)'CINCR=',cincr
      write(*,*)'CEND=',cend
 
      if(indexp.le.0.or.indexp.ge.4)then
        write(*,*)'INDEXP is out of range'
        stop
      endif
 
      if(nstart.le.21)then
        write(*,*)'NSTART is out of range'
        stop
      endif
 
      if(nincr.lt.1)then
        write(*,*)'NINCR is out of range'
        stop
      endif
 
      if(nend.lt.nstart)then
        write(*,*)'NEND is out of range'
        stop
      endif
 
      if(cstart.le.1.or.cstart.ge.nend-13)then
        write(*,*)'CSTART is out of range'
        stop
      endif
 
      if(cend.lt.cstart.or.cend.ge.nend-13)then
        write(*,*)'CEND is out of range'
        stop
      endif

      if(cincr.le.0)then
        write(*,*)'CINCR is out of range'
        stop
      endif
 
      n=nstart

      if(indexp.eq.1)then
        index=1
      elseif(indexp.eq.2)then
        index=2
      elseif(indexp.eq.3)then
        index=1
      endif

      nn=iadim
      nn1=icdim
   51 continue

      if(indexp.eq.1.or.indexp.eq.3)then
        write(*,*)' '
        write(*,*)'matrix of class d'
      elseif(indexp.eq.2)then
        write(*,*)' '
        write(*,*)'matrix of class e'
      endif

      do n=nstart,nend,nincr

        if(n.gt.ibdim)then
          write(*,*)'N=',n
          write(*,*)'is greater than IBDIM=',ibdim
          stop
        else
          write(*,*)'N=',n
        endif

        do c=cstart,cend,cincr

          write(*,*)'C=',c

          if(indexp.eq.1.or.indexp.eq.3)then
            call matrd1(n,nonz,c,nn,nn1,a,snr,irnr)
          elseif(indexp.eq.2)then
            call matre1(n,nonz,c,nn,nn1,a,snr,irnr)
          endif

          write(*,*)'the number of non-zero elements in the original',
     1     ' matrix is equal to:',nonz
 
          do i=1,n
            b(i)=0.0
          enddo
 
          do i=1,nonz
            lrow=irnr(i)
            b(lrow)=b(lrow)+a(i)
          enddo
 
          time=timer()
          call y12mae(n,nonz,a,snr,nn,irnr,nn1,pivot,ha,iha,aflag,
     &      iflag,b,ifail)
          time=timer()-time
 
          if(ifail.eq.0)then
            write(*,*)' subs after y12mae: ',time
            errmax=0.0
            do i=1,n
              errmax=max(errmax,abs(b(i)-1.0))
            enddo
            write(*,*)'Largest solution error is ',errmax
          endif

          write(*,*)'the largest number of elements in a:',iflag(8)
          write(*,*)'the number of collections in row list',iflag(6)
          write(*,*)'the number of collections in column list',iflag(7)
          write(*,*)'the largest element in original matrix',aflag(6)
          write(*,*)'the largest element in lu-matrix',aflag(7)
          write(*,*)'the growth factor is:',aflag(5)
          write(*,*)'the minimal pivotal element',aflag(8)
          write(*,*)'the drop tolerance: ',aflag(2)
          write(*,*)'the stability factor: ',aflag(1)
          write(*,*)'the error diagnostic parameter = ',ifail

        enddo

      enddo

      if(indexp.ne.3)stop
      n=nstart
      index=index+1
      if(index.eq.2)go to 51
      stop
      end
      subroutine matrd1(n,z,c,nn,nn1,a,snr,rnr)                                 
c                                                                               
c                                                                               
c  a matrix which depends on two parameters(n-the dimension of the mat-         
c  rix and c-a structure parameter)is generated by the subroutine matrd1        
c  the non-zero elements of the matrix (in any order) are stored                
c  in the first  z  positions (z  is the number of the non-zero                 
c  elements) of the one-dimensional array  a ,  their column and                
c  row numbers in the corresponding positions of array  snr  and                
c  array  rnr.                                                                  
c                                                                               
c                                                                               
      implicit real(a-b,g,p,t-v),integer(c,f,h-n,r-s,z)                         
      dimension a(nn)                                                           
c     integer*2 snr(nn),rnr(nn1)                                                
      integer snr(nn),rnr(nn1)                                                  
      do 20 i=1,n                                                               
      a(i)=1.0                                                                  
      snr(i)=i                                                                  
   20 rnr(i)=i                                                                  
      do 21 i=1,n                                                               
      r=n+i                                                                     
      s=c+i                                                                     
      a(r)=i+1                                                                  
      if(s.le.n)snr(r)=s                                                        
      if(s.gt.n)snr(r)=s-n                                                      
   21 rnr(r)=i                                                                  
      l=2*n                                                                     
      do 22 i=1,n                                                               
      r=l+i                                                                     
      s=c+i+1                                                                   
      a(r)=-i                                                                   
      if(s.le.n)snr(r)=s                                                        
      if(s.gt.n)snr(r)=s-n                                                      
   22 rnr(r)=i                                                                  
      k=3*n                                                                     
      do 23 i=1,n                                                               
      r=k+i                                                                     
      s=c+i+2                                                                   
      a(r)=16.0                                                                 
      if(s.le.n)snr(r)=s                                                        
      if(s.gt.n)snr(r)=s-n                                                      
   23 rnr(r)=i                                                                  
      rr1=10                                                                    
      rr2=4*n                                                                   
      rr3=1                                                                     
   25 continue                                                                  
      do 26 i=1,rr1                                                             
      a(rr2+i)=100.0*i                                                          
      snr(rr2+i)=n-rr1+i                                                        
      rnr(rr2+i)=rr3                                                            
26    continue                                                                  
      if(rr1.eq.1)    go to 27                                                  
      rr2=rr2+rr1                                                               
      rr1=rr1-1                                                                 
      rr3=rr3+1                                                                 
      go to 25                                                                  
   27 z=4*n+55                                                                  
      return                                                                    
      end                                                                       
      subroutine matre1(n,z,c,nn,nn1,a,snr,rnr)                                 
      implicit real(a-b,g,t-v),integer(c,f,h-n,r-s,z)                           
      real a(nn)                                                                
c     integer*2 snr(nn),rnr(nn1)                                                
      integer snr(nn),rnr(nn1)                                                  
      do 1 i=1,n                                                                
      a(i)=4.                                                                   
      snr(i)=i                                                                  
    1 rnr(i)=i                                                                  
      r=n-1                                                                     
      do 2 i=1,r                                                                
      r1=n+i                                                                    
      a(r1)=-1.                                                                 
      snr(r1)=i+1                                                               
    2 rnr(r1)=i                                                                 
      r2=2*n-1                                                                  
      do 3 i=1,r                                                                
      r1=r2+i                                                                   
      a(r1)=-1.                                                                 
      snr(r1)=i                                                                 
    3 rnr(r1)=i+1                                                               
      r=n-c                                                                     
      r2=3*n-2                                                                  
      do 4 i=1,r                                                                
      r1=r2+i                                                                   
      a(r1)=-1.                                                                 
      snr(r1)=i+c                                                               
    4 rnr(r1)=i                                                                 
      r2=4*n-2-c                                                                
      do 5 i=1,r                                                                
      r1=r2+i                                                                   
      a(r1)=-1.                                                                 
      snr(r1)=i                                                                 
    5 rnr(r1)=i+c                                                               
      z=5*n-2*c-2                                                               
      return                                                                    
      end                                                                       
