      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS641_PRB.
c
c     algorithm 641 collected algorithms from acm.
c     algorithm appeared in acm-trans. math. software, vol.12, no. 2,
c     jun., 1986, p. 149.
c                                                               
      call test01
      call test02
      call test03
 
      stop
      end
      subroutine test01
 
c*********************************************************************72
c
c    example 1 :
c    matrix used by rao, subramanian and krishnamurthy (1976)
c
c
      integer m
      integer n
      integer nrs

      parameter (m=8)
      parameter (n=5)
      parameter (nrs=2)

      integer a(m,n)
      integer ab(540)
      integer b(m,nrs)
      integer chg(12)
      integer code(45)
      integer cons(3)
      integer dim
      integer i
      integer j
      integer maxprm
      integer minprm
      integer mk(2565)
      integer mode
      integer ncol(15)
      integer ncol1(15)
      integer nnrs
      real norm(30)
      integer nrow(30)
      integer p(144)
      integer r
      integer tr
      integer v(30)
      integer x0(2025)
      integer xm(45)

      data a,b/22,14,-1,-3,9,9,2,4,10,7,13,-2,8,1,-6,5,2,10,-1,13,1,
     .     -7,6,0,3,0,-11,-2,-2,5,5,-2,7,8,3,4,4,-1,1,2,12,7,-14,-1,1,8,
     .     8,-1,1,0,0,0,0,0,0,0/

      dim=5
      nnrs=n + nrs
c
c  Print the matrices A and B
c
      write(*,9001)
      do i=1,m
        write(*,9011) i, (a(i,j),j=1,n)
      enddo
 
      write(*,*)' '
      write(*,*)'Matrix B of right hand sides:'
      write(*,*)' '
      do i=1,m
        write(*,9011) i, (b(i,j),j=1,nrs)
      enddo
c
c  Compute MINPRM, MAXPRM and TR.
c
      call nbterm(m,n,nrs,a,b,norm,maxprm,minprm,tr)
 
      write(*,*)' '
      write(*,*)'MINPRM=',minprm
      write(*,*)'MAXPRM=',maxprm
      write(*,*)'TR=    ',tr
c
c  Solve the system of linear equations A*x=B.
c
      mode=3
      call driver(m,n,nrs,nnrs,dim,a,b,minprm,maxprm,tr,mode,ab,p,v,
     .            chg,xm,r,ncol,ncol1,nrow,cons,mk,x0,code)
 
      return
 9001 format (1h1,16x,6hinputs/17x,6 (1h*)//6x,
     .  24hmatrix a of coefficients/)
 9011 format (5x,i3,1h),3x,9i12/ (12x,9i12))
      end
      subroutine test02

c*********************************************************************72
c
c    example 2 :
c    randomly generated rank-deficient matrix
c
c
      integer a(30,15)
      integer b(30,3)
      integer ab(540)
      integer chg(12)
      integer i
      integer j
      integer k
      integer ktemp
      integer m
      integer maxprm
      integer minprm
      integer mode
      integer n
      integer ncol(15)
      integer ncol1(15)
      integer nnrs
      real norm(30)
      integer nrs
      integer p(144)
      integer r0
      integer rl(30,12)
      integer rr(12,15)
      integer v(30),xm(45),
     .        nrow(30),cons(3),mk(2565),x0(2025),code(45),dim,r,tr

      m=30
      n=15
      nrs=3
      dim=12
      nnrs=n + nrs
c
c  Generate the matrices rl, rr and B randomly.
c  (elements from -617 to 618)
c
      r0=12
      ktemp=561
      do i=1,m
        do j=1,r0
          ktemp=mod(ktemp*2543,1237)
          rl(i,j)=ktemp - 618
        enddo
      enddo
 
      do i=1,r0
        do j=1,n
          ktemp=mod(ktemp*2543,1237)
          rr(i,j)=ktemp - 618
        enddo
      enddo
 
      do i=1,m
        do j=1,nrs
          ktemp=mod(ktemp*2543,1237)
          b(i,j)=ktemp - 618
        enddo
      enddo
c
c  Compute A=rl*rr.
c
      do i=1,m
        do j=1,n
          ktemp=0
          do k=1,r0
            ktemp=ktemp + rl(i,k)*rr(k,j)
          enddo
          a(i,j)=ktemp
        enddo
      enddo
c
c  Print the matrices A and B.
c
      write(*,9001)
      do i=1,m
        write(*,9011) i, (a(i,j),j=1,n)
      enddo
 
      write(*,*)' '
      write(*,*)'Matrix B of right hand sides:'
      write(*,*)' '
      do i=1,m
        write(*,9011) i, (b(i,j),j=1,nrs)
      enddo
 
c
c  Compute MINPRM, MAXPRM and TR.
c
      call nbterm(m,n,nrs,a,b,norm,maxprm,minprm,tr)
      write(*,*)' '
      write(*,*)'MINPRM=',minprm
      write(*,*)'MAXPRM=',maxprm
      write(*,*)'TR=    ',tr
c
c  Solve the system A*x=B.
c
      call driver(m,n,nrs,nnrs,dim,a,b,minprm,maxprm,tr,mode,ab,p,v,
     .            chg,xm,r,ncol,ncol1,nrow,cons,mk,x0,code)
 
      return
 9001 format (1h1,16x,6hinputs/17x,6 (1h*)//6x,
     .  24hmatrix a of coefficients/)
 9011 format (5x,i3,1h),3x,9i12/ (12x,9i12))
      end
      subroutine test03

c*********************************************************************72
c
c    example 3 :
c    ill-conditioned test matrix used by zielke (1977)
c    (parameter  a=100000)
c
      integer a(7,6)
      integer ab(540)
      integer b(7,2)
      integer chg(12)
      integer i
      integer ientry
      integer j
      integer k
      integer m
      integer maxprm
      integer minprm
      integer mode
      integer n
      integer nnrs
      integer nrs
      integer p(144)
      integer v(30)
      integer xm(45),ncol(15),ncol1(15),
     .        nrow(30),cons(3),mk(2565),x0(2025),code(45),dim,r,tr
      real norm(30)

      m=7
      n=6
      nrs=2
      dim=6
      nnrs=n + nrs
c
c  Generate the matrices A and B.
c
      do k=1,6
        ientry=100007 - k
        do i=1,k
          a(i,k)=ientry
          a(k,i)=ientry
        enddo
      enddo
 
      a(5,5)=100001
      a(5,6)=100000
      a(6,5)=100000
      a(6,6)=99999
 
      do j=1,4
        a(7,j)=100000
      enddo
 
      a(7,5)=99999
      a(7,6)=99998
 
      do i=1,m
        b(i,1)=1
        b(i,2)=i
      enddo
c
c  Print the matrices A and B.
c
      write(*,9001)
 
      do i=1,m
        write(*,9011) i, (a(i,j),j=1,n)
      enddo
 
      write(*,*)' '
      write(*,*)'Matrix B of right hand sides:'
      write(*,*)' '
      do i=1,m
        write(*,9011) i, (b(i,j),j=1,nrs)
      enddo
c
c  Compute MINPRM, MAXPRM and TR.
c
      call nbterm(m,n,nrs,a,b,norm,maxprm,minprm,tr)
 
      write(*,*)' '
      write(*,*)'MINPRM=',minprm
      write(*,*)'MAXPRM=',maxprm
      write(*,*)'TR=    ',tr
c
c  Solve the system of linear equations A*x=B.
c
      call driver(m,n,nrs,nnrs,dim,a,b,minprm,maxprm,tr,mode,ab,p,v,
     .  chg,xm,r,ncol,ncol1,nrow,cons,mk,x0,code)
 
      return
 9001 format (1h1,16x,6hinputs/17x,6 (1h*)//6x,
     .  24hmatrix a of coefficients/)
 9011 format (5x,i3,1h),3x,9i12/ (12x,9i12))
      end
