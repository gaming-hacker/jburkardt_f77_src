      program maine
c
      implicit real(a-b,g,p,t-v),integer(c,f,h-n,r-s,z)
c
      parameter (ibdim=2000, iadim=25*ibdim, icdim=20*ibdim,
     1 iddim=5*ibdim)
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
c     should be set equal to or larger than the value for
c     nend.
c     the values of aflag(1)-aflag(4),iflag(2)-iflag(5) and iflag(11)
c     are read; see the description of these parameters
c

      real a(iadim),pivot(ibdim),b(ibdim),aflag(11),
     1 b1(ibdim),x(ibdim),a1(iddim)
      real time, timer
c     integer*2 snr(iadim),rnr(icdim),ha(ibdim,13),iflag(12),
      integer snr(iadim),rnr(icdim),ha(ibdim,13),iflag(12),
     1 sn(iddim)
      data nin/5/, nout /6/
c
      iha=ibdim
      write(*,50)
50    format(' indexp,nstart,nincr,nend,cstart,cincr,cend:')
      read(nin,*)indexp,nstart,nincr,nend,cstart,cincr,cend
      write(*,*)indexp,nstart,nincr,nend,cstart,cincr,cend
52    format()
c52    format(10(1x,i5))
      write(*,60)
60    format(' (aflag(j),j=1,4),(iflag(j),j=2,5),iflag(11):')
      read(nin,*)(aflag(j),j=1,4),(iflag(j),j=2,5),iflag(11)
520   format(4(1x,f8.2),5i5)
521   format(4(1x,e8.2),5i5)
      write(*,521)(aflag(j),j=1,4),(iflag(j),j=2,5),iflag(11)
      if(indexp.gt.0.and.indexp.lt.4)go to 301
      write(*,302)
302   format(' indexp is out of range')
      go to 54
301   continue
      if(nstart.gt.21)go to 303
      write(*,304)
304   format(' nstart is out of range')
      go to 54
303   continue
      if(nincr.ge.1)go to 305
      write(*,306)
306   format(' nincr is out of range')
      go to 54
305   continue
      if(nend.ge.nstart)go to 307
      write(*,308)
308   format(' nend is out of range')
      go to 54
307   continue
      if(cstart.gt.1.and.cstart.lt.nend-13)go to 309
      write(*,310)
310   format(' cstart is out of range')
      go to 54
309   continue
      if(cend.ge.cstart.and.cend.lt.nend-13)go to 311
      write(*,312)
312   format(' cend is out of range')
      go to 54
311   continue
      if(cincr.gt.0)go to 313
      write(*,314)
314   format(' cincr is out of range')
      go to 54
313   continue
      n=nstart
      c=cstart
      if(indexp.eq.2)index=2
      if(indexp.eq.1)index=1
      if(indexp.eq.3)index=1
      nn=iadim
      nn1=icdim
   51 continue
      if(n.gt.ibdim) go to 410
      if(index.eq.1)print 41,n,c
   41 format('1','matrix of class d,with parameters n=',i4,'  c=',i4)
      if(index.eq.2)print 33,n,c
   33 format('1matrix of class e: n=',i4,' c='i4)
      if(index.eq.1) call matrd1(n,z,c,nn,nn1,a,snr,rnr)
       if(index.eq.2) call matre1(n,z,c,nn,nn1,a,snr,rnr)
      write(*,53)z
53    format('0the number of non-zero elements in the original',
     1 ' matrix is equal to:',10x,i8)
      do 1 i=1,n
      b(i)=0.0
1     continue
      do 2 i=1,z
      lrow=rnr(i)
      lcol=snr(i)
2     b(lrow)=b(lrow)+a(i)
      ifail1=0
      time=timer()
      call y12mfe(n,a,snr,nn,rnr,nn1,a1,sn,z,ha,iha,b,b1,x,
     1 pivot,aflag,iflag,ifail)
      ifail1=0
      time=-time+timer()
      if(ifail.ne.0)go to 3
      write(*,110)time
110   format(' subs after y12mfe: ',f12.2)
      t=0.0
      do 4 i=1,n
      tt=abs(x(i)-1.0)
      if(tt.gt.t)t=tt
4     continue
      write(*,102) t
102   format('0the largest error is: ',1pe10.2)
3     continue
      write(*,104)iflag(8)
  104 format('0the largest number of elements in a:',i8)
      write(*,105)iflag(6)
  105 format('0the number of collections in row list',i4)
      write(*,106)iflag(7)
  106 format('0the number of collections in column list',i4)
      write(*,107)aflag(6)
  107 format('0the largest element in original matrix',1pe10.2)
      write(*,108)aflag(7)
  108 format('0the largest element in lu-matrix',1pe10.2)
      write(*,109)aflag(5)
  109 format('0the growth factor is:',1pe10.2)
      write(*,100)aflag(8)
  100 format('0the minimal pivotal element',1pe10.2)
      write(*,204) aflag(2),aflag(1)
204   format('0the drop tolerance: ',1pe10.2,
     1 ',     the stability factor: ',1pe10.2)
      write(*,103) ifail
103   format('0the error diagnostic parameter = ',i4)
      c=c+cincr
      if(c.le.cend)go to 51
      c=cstart
      n=n+nincr
      if(n.le.nend)go to 51
      if(indexp.ne.3)go to 54
      c=cstart
      n=nstart
      index=index+1
      if(index.eq.2)go to 51
54    continue
      stop
410   continue
      write(*,420) ibdim,n
420   format(' maxn = ',i4,' is greater than n = ',i4)
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
