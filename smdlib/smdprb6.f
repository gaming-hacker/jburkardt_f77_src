!  smdprb6.f  13 May 1997
!
      program smdprb6
!
!***********************************************************************
!
      integer nx
      parameter (nx=10000)
!
      integer iseed
      integer lun
      character*1 orient
      real pagex
      real pagey
      real x(nx)
      real xphys
      real yphys
!
      write(*,*)' '
      write(*,*)'SMDPRB6'
      write(*,*)'  Demonstrate the creation of bar graphs.'
      write(*,*)' '
!
!  Define the graphics output device to be a PostScript file,
!  in portrait orientation, written to unit 7.
!
      lun=7
      orient = 'P'
 
      call postsc(lun,orient)
!
!  Set the page size.
!
      pagex = 11.0
      pagey = 14.0
 
      call page(pagex,pagey)
!
!  Move the origin.
!
      xphys = 2.5
      yphys = 2.0
 
      call origin(xphys, yphys)
!
!  Generate data once, for both tests.
!
      iseed=1952
 
      call norran(nx,x,iseed)
 
      call test01(nx,x)
 
      call gsdrvr(2, 0.0, 0.0)
 
      call test02(nx,x)
!
!  Terminate the plot.
!
      call stoplt
!
!  Terminate the graphics session.
!
      call finplt
 
      stop
      end
      subroutine test01(nx,x)
!
!***********************************************************************
!
      integer nx
!
      integer i
      integer iaxes
      integer nbar
      real x(nx)
      real xhigh
      real xlow
!
      write(*,*)' '
      write(*,*)'Test01:'
      write(*,*)'  Use BARGRA to create a bar graph from raw data.'
      write(*,*)' '
 
      xlow=x(1)
      xhigh=x(1)
      do i=2,nx
        xlow=min(xlow,x(i))
        xhigh=max(xhigh,x(i))
      end do
 
      write(*,*)'  xlow,xhigh=',xlow,xhigh
 
      nbar=21
      iaxes=0
 
      call bargra(xlow,xhigh,nbar,nx,x,'Value','Frequency',
     &  'Random Values',iaxes)
 
      return
      end
      subroutine test02(nx,x)
!
!***********************************************************************
!
      integer nx
!
      integer nbar
      parameter (nbar=21)
!
      integer i
      integer iaxes
      integer j
      real sbar(nbar)
      real x(nx)
      real xhigh
      real xlow
!
      write(*,*)' '
      write(*,*)'Test02:'
      write(*,*)'  Use BARGRA2 to create a bar graph from binned data.'
      write(*,*)' '
 
      xlow=x(1)
      xhigh=x(1)
      do i=2,nx
        xlow=min(xlow,x(i))
        xhigh=max(xhigh,x(i))
      end do
 
      write(*,*)'  xlow,xhigh=',xlow,xhigh
 
      do i=1,nbar
        sbar(i)=0.0
      end do
 
      do i=1,nx
 
        j = 1+nbar*(x(i)-xlow)/(xhigh-xlow)
 
        if (j.gt.nbar)then
          j=nbar
        end if
 
        sbar(j)=sbar(j)+1.0
 
      end do
 
      iaxes=0
 
      call bargra2(xlow,xhigh,nbar,sbar,'Value','Frequency',
     &  'Random Values',iaxes)
 
      return
      end
      subroutine norran(n,x,iseed)
!
!***********************************************************************
!
!  NORRAN generates a random sample of size N from the the normal
!  Gaussian distribution with mean=0 and standard deviation=1.
!
!  This distribution is defined for all x and has the probability
!  density function
!
!    f(x) =(1/sqrt(2*pi))*exp(-x*x/2).
!
!      input  arguments--n     =the desired integer number
!                                 of random numbers to be
!                                 generated.
!      output arguments--x     =a single precision vector
!                                (of dimension at least n)
!                                 into which the generated
!                                 random sample will be placed.
!      output--a random sample of size n
!              from the normal distribution
!              with mean=0 and standard deviation = 1.
!
!      references--box and muller, note on the generation
!                  of random normal deviates, journal of the
!                  association for computing machinery, 1958,
!                  pages 610-611.
!                --tocher, the art of simulation,
!                  1963, pages 33-34.
!                --hammersley and handscomb, monte carlo methods,
!                  1964, page 39.
!                --johnson and kotz, continuous univariate
!                  distributions--1, 1970, pages 40-111.
!
      integer n
!
      real pi
      parameter(pi=3.14159265358979)
!
      integer i
      integer iseed
      real ranval
      real u1
      real u2
      real x(n)
!
      intrinsic alog
      intrinsic cos
      external ranval
      intrinsic sin
      intrinsic sqrt
!
      do i=1,n,2
 
        u1=ranval(iseed)
        u2=ranval(iseed)
 
        x(i)=sqrt(-2.0*alog(u1))*cos(2.0*pi*u2)
 
        if(i.lt.n)then
          x(i+1)=sqrt(-2.0*alog(u1))*sin(2.0*pi*u2)
        endif
 
      end do
 
      return
      end
      function ranval(iseed)
!
!***********************************************************************
!
!     RANVAL IS THE PORTABLE RANDOM NUMBER GENERATOR OF L. SCHRAGE.
!
!     THE GENERATOR IS FULL CYCLE, THAT IS, EVERY INTEGER FROM
!     1 TO 2**31 - 2 IS GENERATED EXACTLY ONCE IN THE CYCLE.
!     IT IS COMPLETELY DESCRIBED IN TOMS 5(1979),132-138.
!
!     THE FUNCTION STATEMENT IS
!
!       REAL FUNCTION RANVAL(ISEED)
!
!     WHERE
!
!       ISEED IS A POSITIVE INTEGER VARIABLE WHICH SPECIFIES
!         THE SEED TO THE RANDOM NUMBER GENERATOR. GIVEN THE
!         INPUT SEED, RANVAL RETURNS A RANDOM NUMBER IN THE
!         OPEN INTERVAL(0,1). ON OUTPUT THE SEED IS UPDATED.
!
!     SET A=7**5, B15 = 2**15, B16 = 2**16, P = 2**31 - 1, C = 1/P.
!
      integer a
      parameter(a=16807)
!
      integer b15
      parameter(b15=32768)
!
      integer b16
      parameter(b16=65536)
!
      integer p
      parameter(p=2147483647)
!
      real c
      parameter(c=4.656612875e-10)
!
      integer fhi
      integer iseed
      integer k
      integer leftlo
      real ranval
      integer xhi
      integer xalo
!
!     THERE ARE 8 STEPS IN RANVAL.
!
!     1. GET 15 HI ORDER BITS OF ISEED.
!     2. GET 16 LO BITS OF ISEED AND FORM LO PRODUCT.
!     3. GET 15 HI ORDER BITS OF LO PRODUCT.
!     4. FORM THE 31 HIGHEST BITS OF FULL PRODUCT.
!     5. GET OVERFLO PAST 31ST BIT OF FULL PRODUCT.
!     6. ASSEMBLE ALL THE PARTS AND PRESUBSTRACT P.
!        THE PARENTHESES ARE ESSENTIAL.
!     7. ADD P BACK IN IF NECESSARY.
!     8. MULTIPLY BY 1/(2**31 - 1).
!
      xhi=iseed/b16
      xalo =(iseed-xhi*b16)*a
      leftlo=xalo/b16
      fhi=xhi*a+leftlo
      k=fhi/b15
      iseed =(((xalo-leftlo*b16)-p)+(fhi-k*b15)*b16)+k
 
      if(iseed.lt.0) then
        iseed=iseed+p
      end if
 
      ranval=c*float(iseed)
 
      return
      end
