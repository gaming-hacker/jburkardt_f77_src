      subroutine nbterm(m,n,nrs,a,b,norm,maxprm,minprm,tr)

c***********************************************************************
c                                                             
c  NBTERM computes data needed to terminate the solution process.
c
c  Discsussion:
c
c    This routine computes three integers MAXPRM, MINPRM
c    and TR which are used by exsolg to terminate the exact      
c    solution of the system of linear equations ax=b.            
c
c    NBTERM uses several inequalities described in the companion paper.
c                                                             
c  kprime is a linear array containing 100 distinct prime     
c (input) integers in ascending order. the primes are chosen  
c         as large as possible subject to the condition that  
c         for all i and j kprime(i)*kprime(j) does not overflow 
c         an integer word. these primes are used by exsolg    
c         as moduli in the exact computation and as radii for 
c         the representation of the integer results in        
c         mixed-radix from.                                   
c
c      m  is the number of equations in the system. (i.e., m is 
c (input) the size of the first dimensions of a and b.)       
c
c      n  is the number of unknowns in the system. (i.e., n is
c (input) the size of the second dimension of a.)             
c
c    nrs  is the number of right-hand sides in the system.    
c (input) (i.e., nrs is the size of the second dimension of b.) 
c
c      a  is an integer matrix of dimension m by n which      
c (input) contains the matrix of coefficients.                
c
c      b  is an integer matrix of dimension m by nrs which    
c (input) contains the matrix of right-hand sides.            
c
c   norm  is a real array of dimension max(m,n) used to store 
c  (temp) the euclidean norms of the rows (if m>=n) or columns
c         (if m<n) of a                                       
c
c maxprm  is a number of primes sufficient for the exact      
c(output) solution of the system.                             
c
c minprm  is a number of primes sufficient for the correct    
c(output) rank-determination and the correct decision on      
c         consistency.                                        
c
c     tr  is the minimum number of consecutive zero coefficients
c(output) in the mixed-radix form of the integer results of   
c         exsolg required for the termination before maxprm   
c         primes were used.                                   
c                                                             
      integer m
      integer n
      integer nrs
c
      integer a(m,n)
      real abmax
      integer b(m,nrs)
      real fnorm
      integer iprime(100)
      real max1
      real max2
      real norm(max(m,n))
      integer kprime(100)
      integer r
      integer tr
c
      common /primeb/ kprime,iprime
c
c  Set the prime data.
c
      call setprm(iprime,kprime)
c
c  compute 
c    abmax = max(a(i,j)**2,b(i,j)**2) ,
c  fnorm = (frobenius norm of a)**2
c  and the squares of the euclidean norms of the columns or rows
c  of a.
c
      abmax = 0.0
      fnorm = 0.0
      if (m.lt.n)then

        r = m
        k = n
        do j = 1,n
          s = 0.
          do i = 1,m
            x = float(a(i,j))**2
            fnorm = fnorm + x
            s = s + x
            if (x.gt.abmax) abmax = x
          enddo
          norm(j) = s
        enddo
 
      else
 
        r = n
        k = m
        do i = 1,m
          s = 0.
          do j = 1,n
            x = float(a(i,j))**2
            fnorm = fnorm + x
            s = s + x
            if (x.gt.abmax) abmax = x
          enddo
          norm(i) = s
        enddo
 
      endif
 
      rr = float(r)
      fnorm = max(fnorm,rr)
c
c  Compute MAX1 = maximum sum of absolute values in a column of b,
c  MAX2 = maximum sum of squares in a column of b.
c
      max1 = 1.
      max2 = 1.
      do j = 1,nrs
        s1 = 0.
        s2 = 0.
        do i = 1,m
          x = float(iabs(b(i,j)))
          s1 = s1 + x
          x = x**2
          if (x.gt.abmax)abmax = x
          s2 = s2 + x
        enddo
        if (s1.gt.max1) max1 = s1
        if (s2.gt.max2) max2 = s2
      enddo
c
c  Compute the bound B1 for the determination of maxprm.
c
      alog2 = alog(2.0)
      b1 = rr*alog(fnorm/rr)
      rr1 = max(1.,float(r-1))
      x = 0.5* ((2.*rr-1.)*alog(fnorm)+alog(max2)-rr*alog(rr)-rr1*
     .    alog(rr1))
      if (x.gt.b1) b1 = x
      b1 = b1 + alog2
c
c  Compute the bound B3 for the determination of tr.
c
      b3 = alog(0.5*float(m* (n+1))*abmax)
c
c  Compute the bound b2 for the determination of minprm.
c
      b2 = 0.
      do 120 i = 1,r
         abmax = 1.
         do 110 j = 1,k
            if (norm(j).gt.abmax) go to 100
            go to 110
c           then
  100       abmax = norm(j)
            l = j
  110    continue
         if (abmax.le.1.) go to 130
c        then
c     <-----exit
c        else
         b2 = b2 + alog(abmax)
         norm(l) = 0.
  120 continue
  130 b2 = 0.5*b2 + alog(max1) + alog2
c
c  Determine the numbers maxprm, minprm and tr.
c
      maxprm = 0
      minprm = 1
      tr = 1
      sumlog = 0.
c     repeat
  140 maxprm = maxprm + 1
      sumlog = sumlog + alog(float(kprime(maxprm)))
      if (sumlog.lt.b2) minprm = minprm + 1
      if (sumlog.lt.b3) tr = tr + 1
      if (sumlog.ge.b1) return
      go to 140
c        then
c     <-----return
c     continue
      end
      subroutine driver(m,n,nrs,nnrs,dim,a,b,minprm,maxprm,tr,mode,ab,p,
     .                  v,chg,xm,r,ncol,ncol1,nrow,cons,mk,x0,code)
c
c***********************************************************************
c                                                                
c              call exsolg.                                      
c              call fradix.                                      
c              print outputs in fixed-radix form.                
c                                                                
      integer dim
      integer m
      integer minprm
      integer n
      integer nrs
c
      integer a(m,n)
      integer ab(m,nnrs)
      integer b(m,nrs)
      integer chg(dim)
      integer code(maxprm)
      integer ncol(n)
      integer ncol1(n)
      integer nrow(m)
      integer p(dim,dim)
      integer r
      integer tr
      integer v(m)
      integer x0(maxprm,n,nrs)
      integer xm(n,nrs),cons(nrs),mk(minprm,1)
      integer a1(120),c1(120),base
c
c
c  Solve the system of linear equations ax=b.
c
      call exsolg(m,n,nrs,nnrs,dim,a,b,minprm,maxprm,tr,mode,ab,p,v,chg,
     .            xm,r,ncol,nrow,cons,mk,x0,code,nocoef,ier)
c
c  Print output parameters.
c
      write(*,9001) ier,nocoef,r
      if (r.ne.0) write(*,9011) (nrow(i),i=1,r)
      if (r.ne.0) write(*,9021) (ncol(i),i=1,r)
c
c  Convert the integer results from mixed-radix to fixed-radix form.
c  Print the integer results in fixed-radix form.
c
      base = 10000
      lmax = 120
      write(*,9031)
      if (r.eq.0) go to 130
c     then
c      **no information on the null space is printed.
      if (r.eq.n) go to 10
      go to 20
c     then
   10 write(*,9041)
      go to 130
c     else
   20 write(*,9051)
      nmr = n - r
      k = r*nmr + 1
      do i = 1,minprm
        c1(i) = -mk(i,k)
      enddo
 
      call fradix(c1,minprm,a1,lmax,l0,base,ier)
 
c
c  Determine ii with ncol(ii)=i and let ncol1(i)=ii.
c
      do i = 1,n
        do ii = 1,n
          if (ncol(ii).eq.i) go to 50
c              then
c           <-----exit
        enddo
   50    ncol1(i) = ii
      enddo
 
      do j = 1,nmr
         write(*,9061) j
         do i = 1,n
            ii = ncol1(i)
            if (ii.le.r) go to 70
            go to 80
c              then
   70       k = (j-1)*r + ii
            call fradix(mk(1,k),minprm,c1,lmax,l,base,ier)
            write(*,9071) (c1(k),k=1,l)
            go to 110
c              else
   80       ii = ii - r
            if (ii.eq.j) go to 90
            go to 100
c                 then
   90       write(*,9071) (a1(k),k=1,l0)
            go to 110
c                 else
  100       write(*,9081)
  110    continue
        enddo
      enddo
 
130   continue
      write(*,*)' '
      write(*,*)'For each right hand side of the system'
      if (mode.eq.1) write(*,9101)
      if (mode.eq.2) write(*,9111)
      if (mode.eq.3) write(*,9121)
      write(*,9131)
 
      do j = 1,nrs
        write(*,9141) j
        if (cons(j).eq.1) write(*,9151)
        if (cons(j).eq.0) write(*,9161)
        write(*,9171)
        do i = 1,n
          call fradix(x0(1,i,j),nocoef,c1,lmax,l,base,ier)
          write(*,9071) (c1(k),k=1,l)
        enddo
      enddo
 
      write(*,9181)
      call fradix(code,nocoef,c1,lmax,l,base,ier)
      write(*,9071) (c1(k),k=1,l)
 
      return
 
 9001 format (/17x,7houtputs/17x,7 (1h*)//6x,6hier = ,i3,5x,9hnocoef = ,
     .  i3//6x,42hthe matrix a of coefficients has rank r = ,i3,1h.)
 9011 format (6x,44hthe row space of a is spanned by the linear ,
     .  16hindependent rows/8x,25 (i3,1h,))
 9021 format (6x,47hthe column space of a is spanned by the linear ,
     .  19hindependent columns/8x,25 (i3,1h,))
 9031 format (//6x,92 (1h=)/6x,36heach of the integers given below is ,
     .          53hin fixed-radix form. from left to right, the numbers
     .                    ,4hare /6x,
     .                    43hcoef(1),coef(2),...,coef(l), and the value
     .                    ,17hof the integer is/10x,
     .                    20hcoef(1)*base**(l-1)+,
     .                    31hcoef(2)*base**(l-2)+...+coef(l)/6x,
     .                    18hwith base = 10000./6x,92 (1h=))
 9041 format (//6x,43hthe null vector is the only element of the ,
     .  16hnull space of a.)
 9051 format (//6x,49hthe null space of a is spanned by the subsequent ,
     .  34hlinear independent integer vectors)
 9061 format (/10x,11hvector no. ,i3/)
 9071 format (14x,17i6/ (20x,16i6))
 9081 format (19x,1h0)
 9101 format (6x,46ha particular solution of r linear independent ,
     .  23hequations of the system)
 9111 format (6x,52ha particular solution or a particular least squares
     .  ,8hsolution)
 9121 format (6x,44hthe solution or least squares solution with ,
     .  22hminimum euclidean norm)
 9131 format (6x,13hwas computed.)
 9141 format (/10x,20hright-hand side no. ,i3)
 9151 format (/12x,25hthe system is consistent.)
 9161 format (/12x,27hthe system is inconsistent.)
 9171 format (12x,42hnumerators of the elements of the solution/)
 9181 format (/10x,36hcommon denominator for all solutions/)
      end
      subroutine exsolg(m,n,nrs,nnrs,dim,a,b,minprm,maxprm,tr,mode,ab,p,
     .                  v,chg,xm,r,ncol,nrow,cons,mk,x0,code,nocoef,ier)
c
c***********************************************************************
c                                                               
c  EXSOLG solves exactly the general system of linear   
c  equations ax=b, where a(m,n) and b(m,nrs) are matrices with   
c  integer entries. it computes the rank r, a set of r linear    
c independent rows and columns and the matrix k from the        
c hermitian standard form of a. it decides on the consistency   
c of the equations for each right-hand side and computes        
c a solution corresponding to the value of mode.                
c the common denominator of the elements of k and the           
c corresponding numerators are stored in the array mk. the      
c common denominator of the solution is stored in code and the  
c corresponding numerators are stored in x0. all these integer  
c values are in mixed-radix form.                               
c if the solution x is required, the user need only compute     
c x=x0/code.                                                    
c                                                               
c  kprime is a linear array containing 100 distinct prime       
c (input) integers in ascending order. the primes are chosen    
c         as large as possible subject to the condition that    
c         for all i and j kprime(i)*kprime(j) does not overflow   
c         an integer word. these primes are used as radii for   
c         the representation of the integer results in          
c         mixed-radix form.                                     
c iprime  is a linear array of integers such that for each k,   
c (input) iprime(k)* kprime(1)*kprime(2)*...*kprime(k-1) = 1      
c                                          modulo kprime(k).     
c     ap  is a linear integer array of dimension 100 which      
c(output) contains the actually used primes. this array is      
c         necessary because in rare cases some primes from the  
c         array kprime are not suitable for the special system   
c         of equations and must be discarded.                   
c      m  is the number of equations in the system. (i.e.,      
c (input) m is the size of the first dimensions of a and b.)    
c      n  is the number of unknowns in the system. (i.e., n is  
c (input) the size of the second dimension of a.)               
c    nrs  is the number of right-hand sides for which the system
c (input) is to be solved. (i.e., nrs is the size of the second 
c         dimension of b.)                                      
c   nnrs  is equal to n+nrs.                                    
c (input)                                                       
c    dim  is the size of the first and second dimension of the  
c (input) matrix p. dim must be equal to or greater than the    
c         rank r of a, min(m,n) is always sufficient.           
c      a  is an integer array of dimension m by n which contains
c (input) the matrix of coefficients of the system.             
c      b  is an integer array of dimension m by nrs which       
c (input) contains the matrix of right-hand sides of the system.
c minprm  is the minimum number of primes to be used.           
c (input)                                                       
c maxprm  is the maximum number of primes to be used. it must be
c (input) less then 101.                                        
c     tr  is the number of consecutive zero coefficients in the 
c (input) mixed-radix representation of x0 and code required to 
c         guarantee that x=x0/code is the (generalized) solution
c         of ax=b.                                              
c         suitable values for minprm, maxprm and tr can be      
c         obtained using NBTERM.                     
c   mode  is a control parameter which describes the solution   
c (input) to be computed:                                       
c         if mode = 1, a particular solution of r linear        
c            independent equations is computed. it is a         
c            particular solution of the complete system if the  
c            system is consistent.                              
c         if mode = 2, a particular solution (consistent case)  
c            or a particular least squares solution             
c            (inconsistent case) is computed.                   
c         if mode = 3, the solution or least squares solution   
c            with minimum euclidean norm is computed.           
c     ab  is an integer array of dimension m by n+nrs used for  
c  (temp) temporary storage of the augmented matrix (a,b) modulo
c         the various primes kprime(k).                          
c      p  is an integer array of dimension dim by dim used for  
c  (temp) temporary storage of certain matrix products.         
c      v  is a linear integer array of dimension m used for     
c  (temp) temporary storage of columns.                         
c    chg  is a linear integer array of dimension dim used for   
c  (temp) describing permutations of rows and columns by the    
c         routine symsol.                                    
c     xm  is an integer array of dimension n by nrs used for    
c  (temp) temporary storage of the solutions modulo the various 
c         primes kprime(k).                                      
c         the arrays ab, p, v, chg and xm are included in the   
c         argument list only to permit their dimensions to be   
c         variable.                                             
c      r  is the rank of the matrix a of coefficients.          
c(output)                                                       
c   ncol  is a linear integer array of dimension n which        
c(output) contains the indices of the columns of a in the order 
c         as they are used in the transformation. the first r   
c         elements of ncol give the indices of r linear         
c         independent columns of a.                             
c   nrow  is a linear integer array of dimension m which        
c(output) contains the indices of the rows of a in the order    
c         as they are used in the transformation. the first r   
c         elements of nrow give the indices of r linear         
c         independent rows of a.                                
c   cons  is a linear integer array of dimension nrs.           
c(output) cons(i) = 1 if the system with the i-th right-hand    
c                     side is consistent.                       
c         cons(i) = 0 if the system with the i-th right-hand    
c                     side is inconsistent.                     
c     mk  is an integer array of two dimensions. the size of the
c(output) first dimension is minprm. the size of the second     
c         dimension is at least r*(n-r)+1, int(n**2/4)+1 is     
c         always sufficient. if r is not equal n, mk contains   
c         the coefficients of the mixed-radix representations   
c         of the numerators (elements 1 to r*(n-r), columnwise) 
c         and of the common denominator (element r*(n-r)+1) of  
c         the elements of the matrix k from the hermitian       
c         standard form:                                        
c         numerator of k(i,j)                                   
c         =   mk(1,r*(j-1)+i)                                   
c           + mk(2,r*(j-1)+i)*ap(1)                             
c           .                                                   
c           .                                                   
c           .                                                   
c           + mk(minprm,r*(j-1)+i)*ap(1)*ap(2)*...*ap(minprm-1) 
c         denominator similiar.                                 
c     x0  is an integer array of dimension maxprm by n by nrs   
c(output) which contains the coefficients of the mixed-radix    
c         representations of the numerators of the elements of  
c         the solution matrix:                                  
c         x0( ,i,j)                                             
c         =   x0(1,i,j)                                         
c           + x0(2,i,j)*ap(1)                                   
c           .                                                   
c           .                                                   
c           .                                                   
c           + x0(nocoef,i,j)*ap(1)*ap(2)*...*ap(nocoef-1)       
c   code  is a linear integer array of dimension maxprm which   
c(output) contains the coefficients of the mixed-radix          
c         representation of the common denominator of the       
c         elements of the solution matrix:                      
c         code                                                  
c         =   code(1)                                           
c           + code(2)*ap(1)                                     
c           .                                                   
c           .                                                   
c           .                                                   
c           + code(nocoef)*ap(1)*ap(2)*...*ap(nocoef-1)         
c nocoef  is the number of primes actually used to represent    
c(output) x0( ,i,j) and code in mixed-radix form.               
c         nocoef <= maxprm.                                     
c    ier  is an error code which is                             
c(output) 0  if the routine has terminated by the recursive  
c            test.                                              
c         1  if the routine has terminated after maxprm      
c            or minprm (in the case mode=1) executions of the   
c            basic algorithm.                                   
c         2  if the 100 primes did not suffice.                 
c         3  if the input parameters are incorrect.             
c         if the input parameters minprm, maxprm and tr were    
c         suitable chosen (e.g. determined by the routine    
c         NBTERM) and ier <= 1, the exact solution has been     
c         computed correctly.                                   
c                                                               
c
      integer dim
      integer m
      integer n
c
      integer a(m,n)
      integer b(m,nrs),ab(m,nnrs),p(dim,dim),v(m),chg(dim),
     .        xm(n,nrs),ncol(n),nrow(m),cons(nrs),mk(minprm,1),
     .        x0(maxprm,n,nrs),code(maxprm),ap,c,dis,d0,d1,d2,piv,pp,
     .        kprime,p2,rp1,s,s1,scapro
      integer tr,r
      logical test
c
      common /primeb/ kprime(100),iprime(100)
      common /actpr/ pp,ip,p2,ap(100)
c
c  Set the prime data.
c
      call setprm(iprime,kprime)
c
      ier = 3
      if (maxprm.gt.100 .or. m.lt.1 .or. n.lt.1 .or. nrs.lt.1 .or.
     .    nnrs.ne.n+nrs) return
      ier = 2
      np1 = n + 1
c
c  Dis is the number of discarded primes.
c
      dis = 0
c
   10 r = 0
 
      do i = 1,nrs
        cons(i) = 1
      enddo
 
      do i = 1,n
        ncol(i) = i
      enddo
 
      do i = 1,m
        nrow(i) = i
      enddo
 
c
c  Nzerok is the number of consecutive zero coefficients in the
c  mixed-radix representations of the denominator and the numerators
c  of the matrix k.
c
      nzerok = 0
c
c  nozero is the number of consecutive zero coefficients in the
c  mixed-radix representations of the denominator and the numerators
c  of the computed solution.
c
      nozero = 0
 
      do 1010 iter = 1,maxprm
   50    itdis = iter + dis
         if (itdis.gt.100) return
         pp = kprime(itdis)
c
c  The system ax=b is solved modulo pp.
c
         p2 = (pp+1)/2
         iter1 = iter - 1
         iter2 = iter - 2
         d0 = 1
         d1 = 1
         d2 = 1
c
c  AB is the augmented matrix (a,b) modulo pp. the order of the
c  columns of a and of the rows of a and b is determined by a
c  pivot search for iter=1.
c
         do 80 i = 1,m
            ii = nrow(i)
            do 60 j = 1,n
               jj = ncol(j)
               ab(i,j) = mod(a(ii,jj),pp)
   60       continue
            do 70 j = 1,nrs
               jn = j + n
               ab(i,jn) = mod(b(ii,j),pp)
   70       continue
   80    continue
         s = 1
c
c        repeat ... transformation with the pivot column s
c         **the elements of the transformation matrix are stored
c         **in the array ab.
c
   90    if (s.le.r) go to 100
         go to 210
c           then
c            **take the same pivots as in the previous executions.
c
  100    piv = ab(s,s)
         if (piv.eq.0) go to 110
         go to 120
c              then
c               **drop this prime, go back and take the next prime.
  110    dis = dis + 1
         go to 50
 
  120    d0 = mod(d0*piv,pp)
         piv = inv(piv,pp)
         k = 1
         if (r.eq.m .or. mode.eq.1) k = s + 1
         do 130 j = k,nnrs
            if (j.ne.s) ab(s,j) = mod(ab(s,j)*piv,pp)
  130    continue
         ab(s,s) = piv
         if (s.gt.1) go to 140
         go to 170
c              then
  140    s1 = s - 1
         do 160 i = 1,s1
            c = ab(i,s)
            do 150 j = k,nnrs
               if (j.ne.s) ab(i,j) = mod(ab(i,j)-c*ab(s,j),pp)
  150       continue
            ab(i,s) = mod(-c*piv,pp)
  160    continue
  170    if (s.eq.m) go to 320
c              then
c        <--------exit
  180    s1 = s + 1
         do 200 i = s1,m
            c = ab(i,s)
            do 190 j = k,nnrs
               if (j.ne.s) ab(i,j) = mod(ab(i,j)-c*ab(s,j),pp)
  190       continue
            ab(i,s) = mod(-c*piv,pp)
  200    continue
         s = s1
         if (s.gt.n) go to 320
c              then
c        <--------exit
         go to 90
c           else
  210    if (iter.gt.minprm) go to 320
c              then
c        <--------exit
c
c            **look for nonzero pivots.
c
         do 310 i = s,m
            do 300 j = s,n
               if (ab(i,j).ne.0) go to 220
               go to 300
c                    then
c                     **move ab(i,j) to the position (s,s).
c
  220          if (j.ne.s) go to 230
               go to 250
c                       then
  230          do 240 ii = 1,m
                  c = ab(ii,s)
                  ab(ii,s) = ab(ii,j)
                  ab(ii,j) = c
  240          continue
               c = ncol(s)
               ncol(s) = ncol(j)
               ncol(j) = c
  250          if (i.ne.s) go to 260
               go to 280
c                       then
  260          do 270 ii = 1,nnrs
                  c = ab(s,ii)
                  ab(s,ii) = ab(i,ii)
                  ab(i,ii) = c
  270          continue
               c = nrow(s)
               nrow(s) = nrow(i)
               nrow(i) = c
  280          r = s
               if (iter.gt.1) go to 290
               go to 100
c                       then
c                        **drop all primes before and go back.
  290          dis = dis + iter - 1
               go to 10
 
  300       continue
  310    continue
c        continue
c
c     ***transformation modulo pp was successful,
c     ***now compute the solution.
c
  320    rp1 = r + 1
         ap(iter) = pp
c
c      **compute ip with ip*ap(1)*...*ap(iter-1)=1 mod pp.
c
         if (dis.eq.0 .or. iter.eq.1) go to 330
         go to 340
c        then
  330    ip = iprime(iter)
         go to 360
c        else
  340    k = 1
         do 350 i = 1,iter1
            k = mod(k*ap(i),pp)
  350    continue
         ip = inv(k,pp)
c
  360    if (r.lt.m) go to 370
         go to 420
c        then
c         **check for consistency.
c
  370    do 410 j = 1,nrs
            if (cons(j).eq.1) go to 380
            go to 410
c              then
  380       jn = j + n
            do 400 i = rp1,m
               if (ab(i,jn).ne.0) go to 390
               go to 400
c                    then
  390          cons(j) = 0
               go to 410
 
  400       continue
  410    continue
c
  420    if (r.eq.0) go to 890
c        then
c         **set all elements of the solution equal to zero
c         **and take the next prime.
c
         if (iter.le.minprm .and. r.lt.n) go to 430
         go to 480
c        then
c         **compute the next coefficients of the mixed-radix
c         **representations of the numerators and the denominator
c         **of the matrik k.
c
  430    k = 0
         test = .true.
         do 450 j = rp1,n
            do 440 i = 1,r
               k = k + 1
               call mixrad(mk(1,k),iter,ab(i,j)*d0,test)
  440       continue
  450    continue
         k = k + 1
         call mixrad(mk(1,k),iter,d0,test)
c
c         **count the consecutive zero coefficients.
c
         if (test) go to 460
         go to 470
c           then
  460    nzerok = nzerok + 1
         go to 480
c           else
  470    nzerok = 0
c
  480    if (mode.eq.1 .or. r.eq.m) go to 490
         go to 520
c        then
c         **store the transformed right-hand sides in xm.
c
  490    do 510 i = 1,r
            do 500 j = 1,nrs
               jn = j + n
               xm(i,j) = ab(i,jn)
  500       continue
  510    continue
         go to 710
c        else
c         **compute the projected and transformed right-hand sides
c         **and store them in xm.
c
  520    mr = m - r
         if (2*r.ge.m) go to 530
         go to 620
c           then
c            **use formula (a1).
c
  530    do 550 i = 1,mr
            ir = i + r
            do 540 j = i,mr
               jr = j + r
               p(i,j) = scapro(ab(ir,1),m,ab(jr,1),m,r,pp)
  540       continue
            p(i,i) = mod(p(i,i)+1,pp)
  550    continue
         call symsol(p,dim,mr,ab,m,rp1,np1,nnrs,v,chg,d1,pp,ie)
         if (ie.ne.0) go to 560
         go to 570
c              then
c               **drop this prime, go back and take the next prime.
  560    dis = dis + 1
         go to 50
 
  570    do 590 i = 1,r
            do 580 j = 1,nrs
               jn = j + n
               xm(i,j) = scapro(ab(rp1,i),i,ab(rp1,jn),1,mr,pp)
  580       continue
  590    continue
         do 610 j = 1,nrs
            call vstore(xm(1,j),1,r,v)
            jn = j + n
            do 600 i = 1,r
               c = scapro(ab(i,1),m,v,1,r,pp)
               xm(i,j) = mod(ab(i,jn)-c,pp)
  600       continue
  610    continue
         go to 710
c           else
c            **use formula (a2).
c
  620    do 640 i = 1,r
            do 630 j = i,r
               p(i,j) = scapro(ab(rp1,i),1,ab(rp1,j),1,mr,pp)
  630       continue
            p(i,i) = mod(p(i,i)+1,pp)
  640    continue
 
         do j = np1,nnrs
           call vstore(ab(rp1,j),1,mr,v)
           do i = 1,r
             ir = i + r
             ab(ir,j) = scapro(ab(rp1,i),1,v,1,mr,pp)
           enddo
         enddo
 
         call symsol(p,dim,r,ab,m,rp1,np1,nnrs,v,chg,d1,pp,ie)
         if (ie.ne.0) go to 670
         go to 680
c              then
c               **drop this prime, go back and take the next prime.
  670    dis = dis + 1
         go to 50
 
  680    do 700 i = 1,r
            do 690 j = 1,nrs
               jn = j + n
               c = scapro(ab(i,1),m,ab(rp1,jn),1,r,pp)
               xm(i,j) = mod(ab(i,jn)-c,pp)
  690       continue
  700    continue
c
  710    if (mode.eq.3 .and. r.lt.n) go to 720
         go to 890
c        then
c         **compute the minimum norm solution
c
  720    nr = n - r
         if (2*r.lt.n) go to 730
         go to 800
c           then
c            **use formula (b1).
c
  730    do 750 i = 1,r
            do 740 j = i,r
               ab(i,j) = scapro(ab(i,rp1),m,ab(j,rp1),m,nr,pp)
  740       continue
            ab(i,i) = mod(ab(i,i)+1,pp)
  750    continue
         call symsol(ab,m,r,xm,n,1,1,nrs,v,chg,d2,pp,ie)
         if (ie.ne.0) go to 760
         go to 770
c              then
c               **drop this prime, go back and take the next prime
  760    dis = dis + 1
         go to 50
 
  770    do 790 i = rp1,n
            do 780 j = 1,nrs
               xm(i,j) = scapro(ab(1,i),1,xm(1,j),1,r,pp)
  780       continue
  790    continue
         go to 890
c           else
c            **use formula (b2).
c
  800    do 820 i = 1,nr
            ir = i + r
            do 810 j = i,nr
               jr = j + r
               ab(i,j) = scapro(ab(1,ir),1,ab(1,jr),1,r,pp)
  810       continue
            ab(i,i) = mod(ab(i,i)+1,pp)
  820    continue
         do 840 i = rp1,n
            do 830 j = 1,nrs
               xm(i,j) = scapro(ab(1,i),1,xm(1,j),1,r,pp)
  830       continue
  840    continue
         call symsol(ab,m,nr,xm,n,rp1,1,nrs,v,chg,d2,pp,ie)
         if (ie.ne.0) go to 850
         go to 860
c              then
c               **drop this prime, go back and take the next prime
  850    dis = dis + 1
         go to 50
 
  860    do 880 i = 1,r
            do 870 j = 1,nrs
               c = scapro(ab(i,rp1),m,xm(rp1,j),1,nr,pp)
               xm(i,j) = mod(xm(i,j)-c,pp)
  870       continue
  880    continue
c
  890    if (mode.ge.2 .and. (r.ne.m.or.r.ne.n)) d0 = mod(d0*d0,pp)
         d0 = mod(d0*mod(d1*d2,pp),pp)
c
c      **compute the next coefficients in the mixed-radix
c      **representations of the numerators and the denominator
c      **of the solution
c
         test = .true.
         do 940 i = 1,n
            ii = ncol(i)
            if (i.gt.r .and. mode.le.2 .or. r.eq.0) go to 900
            go to 920
c           then
  900       do 910 j = 1,nrs
               x0(iter,ii,j) = 0
  910       continue
            go to 940
c           else
  920       do 930 j = 1,nrs
               call mixrad(x0(1,ii,j),iter,d0*xm(i,j),test)
  930       continue
  940    continue
         call mixrad(code,iter,d0,test)
c
c      **count the consecutive zero coefficients
c
         if (test) go to 950
         go to 960
c        then
  950    nozero = nozero + 1
         go to 970
c        else
  960    nozero = 0
         go to 990
 
  970    k = tr
         if (r.lt.n) k = max0(k,minprm-nzerok+1)
         if (iter.ge.minprm .and. nozero.ge.k) go to 980
         go to 990
c        then
c         **termination by the recursive test.
  980    nocoef = iter - nozero
         ier = 0
         return
 
  990    if (mode.eq.1 .and. iter.eq.minprm) go to 1000
         go to 1010
c        then
c         **termination after minprm primes.
 1000    nocoef = iter - nozero
         ier = 1
         return
 
 1010 continue
c   **termination after maxprm primes.
      nocoef = maxprm - nozero
      ier = 1
      return
      end
      subroutine vstore(a,ja,n,v)
c
c***********************************************************************
c                                                               
c VSTORE copies the vector of n elements               
c stored in the array a with increment ja into the vector v     *
c
      integer n
c
      integer a(*)
      integer i
      integer j1
      integer ja
      integer v(n)
c
      j1 = 1
 
      do i = 1,n
         v(i) = a(j1)
         j1 = j1 + ja
      enddo
 
      return
      end
      function scapro(a,ja,b,jb,n,p)
c
c***********************************************************************
c                                                               
c  SCAPRO computes the scalar product modulo p            
c  of two integer vectors of n elements which are stored         
c in the arrays a and b with increments ja and jb,respectively. 
c                                                               
      integer a(1)
      integer b(1)
      integer i
      integer j1
      integer j2
      integer ja
      integer jb
      integer n
      integer p
      integer scapro
c
      scapro = 0
      j1 = 1
      j2 = 1
      do i = 1,n
         scapro = mod(scapro+a(j1)*b(j2),p)
         j1 = j1 + ja
         j2 = j2 + jb
      enddo
 
      return
      end
      subroutine symsol(a,na,n,b,nb,i1,k1,k2,v,chg,dmod,p,ier)
c
c***********************************************************************
c                                                               
c  SYMSOL solves the system of equations ax=b modulo p  
c by computing                                                  *
c    a**(-1)*b mod p                                            *
c which is stored in b. additionally                            *
c    dmod = determinant(a) mod p                                *
c is computed.                                                  *
c the matrix a of coefficients is symmetric (only the upper     *
c triangle is needed) and a square-root-free cholesky           *
c decomposition is used to find the solution. the pivots are    *
c taken from the main diagonal. if no nonzero pivot is          *
c available the routine returns with the error code ier=1.   *
c to make the routine universally applicable b is allowed    *
c to be any connected part of a two-dimensional array.          *
c                                                               *
c      a  is an integer matrix of dimension na by n which       *
c (input) contains the matrix of coefficients in the first      *
c         n rows.                                               *
c     na  is the size of the first dimension of a.              *
c (input)                                                       *
c      n  is the number of equations and the number of          *
c (input) unknowns in the system.                               *
c      b  is an integer matrix of dimension nb by k2. in the    *
c (input) rows i1 to i1+n-1 and columns k1 to k2 it contains    *
c         first the right-hand sides b and finally              *
c         a**(-1)*b mod p.                                      *
c     nb  is the size of the first dimension of b.              *
c (input)                                                       *
c     i1  is the number of the first row used in b.             *
c (input)                                                       *
c     k1  is the number of the first column used in b.          *
c (input)                                                       *
c     k2  is the number of the last column used in b.           *
c (input)                                                       *
c      v  is a vector of dimension n used for temporary         *
c  (temp) storage during the back substitution.                 *
c    chg  is a vector of dimension n used for saving            *
c  (temp) symmetric permutations in a.                          *
c   dmod  contains the residuum of determinant(a) modulo p.     *
c(output)                                                       *
c      p  is the prime which is used as modul.                  *
c (input)                                                       *
c    ier  is an error code which is                             *
c(output) 0  if the solution is computed.                       *
c         1  if not n nonzero pivots in the main diagonal       *
c            could be found ; no solution is computed.          *
c         2  if the input parameters are incorrect.             *
c                                                               *
c                  *****warning*****                            *
c                                                               *
c this routine assumes that the elements of a and b are      *
c reduced modulo p.                                             *
c                                                               *
c****************************************************************
c
      integer k2
      integer n
      integer na
      integer nb
c
      integer a(na,n)
      integer b(nb,k2)
      integer chg(n)
      integer dmod
      integer i
      integer i1
      integer i2
      integer ib
      integer ier
      integer ii
      integer iib
      integer im1
      integer inv
      integer ip1
      integer j
      integer jm1
      integer jp1
      integer k
      integer k1
      integer ktemp
      integer p
      integer piv
      integer v(n)
c
      ier = 2
      i2 = i1 + n - 1
      if (n.lt.1 .or. n.gt.na .or. i1.lt.1 .or. i2.gt.nb .or. k1.lt.
     .    1 .or. k1.gt.k2) return
      ier = 1
      dmod = 1
 
      do i = 1,n
        chg(i) = i
      enddo
 
      i = 1
c
c  repeat ... i-th step of the cholesky decomposition of a
c
   20 ip1 = i + 1
      if (a(i,i).eq.0) go to 30
      go to 150
c        then
c      look for a nonzero pivot in the main diagonal.
c
   30 if (i.eq.n) return
 
      do j = ip1,n
        if (a(j,j).ne.0) go to 50
      enddo
 
c              then
c                 take a(j,j) as pivot
c
c           if no nonzero pivot was found
c           then
      return
c           else
c            **interchange the i-th and j-th row and column of a.
c
   50 if (i.gt.1) go to 60
      go to 80
c              then
   60 im1 = i - 1
 
      do k = 1,i-1
        ktemp = a(k,j)
        a(k,j) = a(k,i)
        a(k,i) = ktemp
      enddo
 
   80 if (ip1.lt.j) go to 90
      go to 110
c              then
   90 jm1 = j - 1
 
      do k = ip1,j-1
        ktemp = a(k,j)
        a(k,j) = a(i,k)
        a(i,k) = ktemp
      enddo
 
  110 if (j.lt.n) go to 120
      go to 140
c              then
  120 jp1 = j + 1
 
      do k = j+1,n
        ktemp = a(j,k)
        a(j,k) = a(i,k)
        a(i,k) = ktemp
      enddo
 
  140 ktemp = a(i,i)
      a(i,i) = a(j,j)
      a(j,j) = ktemp
      ktemp = chg(i)
      chg(i) = chg(j)
      chg(j) = ktemp
c
  150 piv = a(i,i)
      dmod = mod(dmod*piv,p)
      piv = inv(piv,p)
      a(i,i) = piv
      if (ip1.le.n) go to 160
      go to 190
c        then
  160 do 180 j = ip1,n
         ktemp = mod(a(i,j)*piv,p)
         do 170 k = j,n
            a(j,k) = mod(a(j,k)-ktemp*a(i,k),p)
  170    continue
         a(i,j) = ktemp
  180 continue
 
      i = ip1
      go to 20
c        else
c     <-----exit
c     continue
c
c   **solve the system ax=b mod p using the decomposition of a.
c
  190 do 280 k = k1,k2
         do 230 i = 1,n
            ib = chg(i) + i1 - 1
            ktemp = b(ib,k)
            if (i.gt.1) go to 200
            go to 220
c           then
  200       im1 = i - 1
            do 210 j = 1,im1
               ktemp = mod(ktemp-a(j,i)*v(j),p)
  210       continue
  220       v(i) = ktemp
  230    continue
         do 270 i = 1,n
            ii = n - i + 1
            ktemp = mod(v(ii)*a(ii,ii),p)
            if (ii.lt.n) go to 240
            go to 260
c           then
  240       ip1 = ii + 1
            do 250 j = ip1,n
               ktemp = mod(ktemp-a(ii,j)*v(j),p)
  250       continue
  260       iib = chg(ii) + i1 - 1
            b(iib,k) = ktemp
            v(ii) = ktemp
  270    continue
  280 continue
      ier = 0
      return
      end
c     function inv(b,p)
c
c****************************************************************
c                                                               *
c euclid's extended algorithm is used to find                   *
c the inverse, inv, modulo p of b.                              *
c                                                               *
c                  *****warning*****                            *
c                                                               *
c this function assumes that the greatest common divisor        *
c of b and p is 1.                                              *
c                                                               *
c****************************************************************
c
      function inv(b,p)
c
c***********************************************************************
c
      integer b
      integer inv
      integer p
      integer q
      integer r0
      integer r1
      integer r2
      integer x0
      integer x1
      integer x2
c
      r0 = p
      x0 = 0
      r1 = b
      x1 = 1
c     repeat
   10 if (iabs(r1).eq.1) go to 30
      go to 20
c        then
c     <-----exit
   20 q = r0/r1
      r2 = r0 - q*r1
      x2 = x0 - q*x1
      r0 = r1
      x0 = x1
      r1 = r2
      x1 = x2
      go to 10
c     continue
   30 inv = mod(x1,p)
      if (r1.lt.0) inv = -inv

      return
      end
      subroutine mixrad(g,iter,res,test)
c
c***********************************************************************
c                                                               
c  MIXRAD is given the residuum res modulo p and the first iter-1          
c  coefficients in the mixed-radix representation                
c g(1)+g(2)*ap(1)+g(3)*ap(1)*ap(2)+... of an integer number     *
c (where the ap(i) are the actually used primes)                *
c this subroutine computes the iter-th coefficient g(iter).     *
c                                                               *
c additional parameters:                                        *
c                                                               *
c     ip  is an integer such that ip*ap(1)*...*ap(iter-1) = 1   *
c (input)                                              modulo p.*
c     p2  is equal to (p+1)/2.                                  *
c (input)                                                       *
c   test  is not changed if g(iter) = 0 was computed;           *
c(output) otherwise test is set .false..                        *
c                                                               *
      integer iter
c
      integer ap(100)
      integer g(iter)
      integer i
      integer i1
      integer ip
      integer iter1
      integer iter2
      integer ntemp
      integer p
      integer p2
      integer res
      logical test
c
      common /actpr/p,ip,p2,ap
c
      if (iter.eq.1) go to 10
      go to 20
c     then
   10 ntemp = mod(res,p)
      go to 60
c     else
   20 iter1 = iter - 1
      ntemp = g(iter1)
      if (iter.ne.2) go to 30
      go to 50
c        then
   30 iter2 = iter - 2
 
      do i = 1,iter2
        i1 = iter1 - i
        ntemp = mod(ntemp*ap(i1)+g(i1),p)
      enddo
 
   50 ntemp = mod(ip*mod(res-ntemp,p),p)
   60 if (ntemp.ne.0) test = .false.
      g(iter) = ntemp - (ntemp/p2)*p
 
      return
      end
      subroutine fradix(c,n,a,lmax,l,b,ier)
c
c***********************************************************************
c                                                               
c given the mixed-radix integer                                 *
c                                                               *
c   c(1)+c(2)*ap(1)+ ... +c(n)*ap(1)*...*ap(n-1),               *
c                                                               *
c where ap(i) are the actually used primes and                  *
c abs(c(i)) < (p(i)+1)/2, i=1,...,n,                            *
c this routine computes (for some l <= lmax) the coefficients*
c a(1),a(2),...,a(l) in its fixed-radix representation:         *
c                                                               *
c   a(1)*b**(l-1)+a(2)*b**(l-2)+...+a(l),                       *
c                                                               *
c where abs(a(i)) < b and all a(i) have the same sign.          *
c                                                               *
c lmax must be given such that                                  *
c                                                               *
c   lmax >= (n*log(ap(n)) - log 2) / log b.                     *
c                                                               *
c ier is an error code which is 1 if the dimension parameters   *
c n,lmax are incorrect, and 0 otherwise.                        *
c                                                               *
c                     ****** warning ******                     *
c                                                               *
c this routine assumes that for all i                        *
c                                                               *
c    ap(i)*b                                                    *
c                                                               *
c does not overflow a computer word.                            *
c                                                               *
c****************************************************************
c
      integer lmax
      integer n
c
      integer a(lmax)
      integer ap(100)
      integer b
      integer c(n)
      integer i
      integer ier
      integer ip
      integer j
      integer l
      integer l2
      integer li1
      integer ni1
      integer p2
      integer pp
      integer q
      integer qtemp
c
      common /actpr/pp,ip,p2,ap
c
      ier = 1
      if (n.lt.1 .or. lmax.lt.1) return
      l = 1
      a(1) = 0
      do 40 i = 1,n
c
c  At this stage a(1)+a(2)*b+...+a(l)*b**(l-1) is the
c  fixed-radix representation of c(n-i+2)+c(n-i+3)*
c  ap(n-i+2)+...+c(n)*ap(n-i+2)*...*ap(n-1).
c
         ni1 = n - i + 1
         pp = ap(ni1)
         q = c(ni1)
c
c  Compute the first L coefficients of the
c  fixed-radix representation of c(n-i+1)+
c  ap(n-i+1)*(a(1)+a(2)*b+...+a(l)*b**(l-1)).
c
         do 10 j = 1,l
            qtemp = a(j)*pp + q
            q = qtemp/b
            a(j) = qtemp - q*b
   10    continue
c
c  repeat ... convert a(1)+a(2)*b+...+a(l)*b**(l-1)
c   ... +q*b**l to fixed-radix form.
c
   20    if (q.ne.0) go to 30
         go to 40
c           then
   30    l = l + 1
         if (l.gt.lmax) return
         qtemp = q/b
         a(l) = q - qtemp*b
         q = qtemp
         go to 20
c        continue
   40 continue
      if (l.ge.2) go to 50
      go to 70
c     then ... reorder the coefficients.
   50 l2 = l/2
      do 60 i = 1,l2
         li1 = l - i + 1
         qtemp = a(i)
         a(i) = a(li1)
         a(li1) = qtemp
   60 continue
 
70    continue

      ier = 0
 
      return
      end
      subroutine setprm(iprime,kprime)
c
c***********************************************************************
c
      integer iprime(100)
      integer kprime(100)
c
      kprime(1)=45233
      kprime(2)=45247
      kprime(3)=45259
      kprime(4)=45263
      kprime(5)=45281
      kprime(6)=45289
      kprime(7)=45293
      kprime(8)=45307
      kprime(9)=45317
      kprime(10)=45319
      kprime(11)=45329
      kprime(12)=45337
      kprime(13)=45341
      kprime(14)=45343
      kprime(15)=45361
      kprime(16)=45377
      kprime(17)=45389
      kprime(18)=45403
      kprime(19)=45413
      kprime(20)=45427
      kprime(21)=45433
      kprime(22)=45439
      kprime(23)=45481
      kprime(24)=45491
      kprime(25)=45497
      kprime(26)=45503
      kprime(27)=45523
      kprime(28)=45533
      kprime(29)=45541
      kprime(30)=45553
      kprime(31)=45557
      kprime(32)=45569
      kprime(33)=45587
      kprime(34)=45589
      kprime(35)=45599
      kprime(36)=45613
      kprime(37)=45631
      kprime(38)=45641
      kprime(39)=45659
      kprime(40)=45667
      kprime(41)=45673
      kprime(42)=45677
      kprime(43)=45691
      kprime(44)=45697
      kprime(45)=45707
      kprime(46)=45737
      kprime(47)=45751
      kprime(48)=45757
      kprime(49)=45763
      kprime(50)=45767
      kprime(51)=45779
      kprime(52)=45817
      kprime(53)=45821
      kprime(54)=45823
      kprime(55)=45827
      kprime(56)=45833
      kprime(57)=45841
      kprime(58)=45853
      kprime(59)=45863
      kprime(60)=45869
      kprime(61)=45887
      kprime(62)=45893
      kprime(63)=45943
      kprime(64)=45949
      kprime(65)=45953
      kprime(66)=45959
      kprime(67)=45971
      kprime(68)=45979
      kprime(69)=45989
      kprime(70)=46021
      kprime(71)=46027
      kprime(72)=46049
      kprime(73)=46051
      kprime(74)=46061
      kprime(75)=46073
      kprime(76)=46091
      kprime(77)=46093
      kprime(78)=46099
      kprime(79)=46103
      kprime(80)=46133
      kprime(81)=46141
      kprime(82)=46147
      kprime(83)=46153
      kprime(84)=46171
      kprime(85)=46181
      kprime(86)=46183
      kprime(87)=46187
      kprime(88)=46199
      kprime(89)=46219
      kprime(90)=46229
      kprime(91)=46237
      kprime(92)=46261
      kprime(93)=46271
      kprime(94)=46273
      kprime(95)=46279
      kprime(96)=46301
      kprime(97)=46307
      kprime(98)=46309
      kprime(99)=46327
      kprime(100)=46337
 
      iprime(1)=0
      iprime(2)=42015
      iprime(3)=28577
      iprime(4)=01108
      iprime(5)=29342
      iprime(6)=16641
      iprime(7)=10405
      iprime(8)=19447
      iprime(9)=26685
      iprime(10)=39525
      iprime(11)=14116
      iprime(12)=12753
      iprime(13)=32178
      iprime(14)=01043
      iprime(15)=08857
      iprime(16)=27911
      iprime(17)=15049
      iprime(18)=07079
      iprime(19)=33425
      iprime(20)=00804
      iprime(21)=23175
      iprime(22)=23886
      iprime(23)=44779
      iprime(24)=41942
      iprime(25)=10171
      iprime(26)=16606
      iprime(27)=10638
      iprime(28)=17371
      iprime(29)=27195
      iprime(30)=35827
      iprime(31)=42639
      iprime(32)=01829
      iprime(33)=24658
      iprime(34)=09023
      iprime(35)=37958
      iprime(36)=30638
      iprime(37)=06339
      iprime(38)=41270
      iprime(39)=40538
      iprime(40)=10157
      iprime(41)=11783
      iprime(42)=00457
      iprime(43)=32947
      iprime(44)=42170
      iprime(45)=17910
      iprime(46)=33474
      iprime(47)=20017
      iprime(48)=25086
      iprime(49)=36508
      iprime(50)=37444
      iprime(51)=35543
      iprime(52)=06993
      iprime(53)=10326
      iprime(54)=16328
      iprime(55)=26765
      iprime(56)=42083
      iprime(57)=37223
      iprime(58)=30711
      iprime(59)=09408
      iprime(60)=06635
      iprime(61)=38421
      iprime(62)=11397
      iprime(63)=32683
      iprime(64)=17333
      iprime(65)=34245
      iprime(66)=15748
      iprime(67)=35735
      iprime(68)=23492
      iprime(69)=19302
      iprime(70)=20076
      iprime(71)=45620
      iprime(72)=44978
      iprime(73)=09864
      iprime(74)=14832
      iprime(75)=16092
      iprime(76)=19457
      iprime(77)=24045
      iprime(78)=44950
      iprime(79)=32872
      iprime(80)=24309
      iprime(81)=15726
      iprime(82)=43057
      iprime(83)=37766
      iprime(84)=14046
      iprime(85)=41826
      iprime(86)=19946
      iprime(87)=41363
      iprime(88)=23967
      iprime(89)=39791
      iprime(90)=29237
      iprime(91)=18085
      iprime(92)=12952
      iprime(93)=36850
      iprime(94)=02213
      iprime(95)=30023
      iprime(96)=34871
      iprime(97)=42667
      iprime(98)=40410
      iprime(99)=32615
      iprime(100)=46136
 
      return
      end
