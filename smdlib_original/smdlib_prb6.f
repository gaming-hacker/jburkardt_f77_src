c  SMDPRB6.F  21 July 1993
c
      program smdprb6
c
c***********************************************************************
c
      write(*,*)' '
      write(*,*)'SMDPRB6'
      write(*,*)'Demonstrate creation of bar graphs.'
      write(*,*)' '
c
      call psccgm('ps','smdprb6.ps')

      call page(11.0,14.0)
      call origin(2.5,2.0)

      call test01
      call gsdrvr(2,0.0,0.0)
      call test02
c
      call stoplt(0)
      call finplt

      stop
      end
      subroutine test01
c
c***********************************************************************
c
      integer nx
      parameter (nx=10000)
c
      integer i
      integer iaxes
      integer iseed
      real x(nx)
      real xhigh
      real xlow
c
      write(*,*)' '
      write(*,*)'Use BARGRA to create a bar graph from raw data.'
      write(*,*)' '
c
      iseed=1952
c
      call norran(nx,x,iseed)

      xlow=x(1)
      xhigh=x(1)
      do 10 i=1,nx
        xlow=min(xlow,x(i))
        xhigh=max(xhigh,x(i))
10    continue

      write(*,*)'xlow,xhigh=',xlow,xhigh

      nbar=21
      iaxes=0

      call bargra(xlow,xhigh,nbar,nx,x,'Value','Frequency',
     &  'Random Values',iaxes)

      return
      end
      subroutine test02
c
c***********************************************************************
c
      integer nx
      parameter (nx=10000)
c
      integer nbar
      parameter (nbar=21)
c
      integer i
      integer iaxes
      integer iseed
      integer j
      real sbar(nbar)
      real x(nx)
      real xhigh
      real xlow
c
      write(*,*)' '
      write(*,*)'Use BARGRA2 to create a bar graph from binned data.'
      write(*,*)' '
c
      iseed=1952
c
      call norran(nx,x,iseed)

      do 10 i=1,nbar
        sbar(i)=0.0
10    continue

      do 20 i=1,nx
        j=int(x(i)*(nbar-1)))+1
        if(j.ge.1.and.j.le.nbar)sbar(j)=sbar(j)+1
20    continue

      iaxes=0

      call bargra2(xlow,xhigh,nbar,sbar,'Value','Frequency',
     &  'Random Values',iaxes)

      return
      end
      subroutine norran(n,x,iseed)
c
c***********************************************************************
c
c  NORRAN generates a random sample of size N from the the normal
c  Gaussian distribution with mean=0 and standard deviation=1.
c
c  This distribution is defined for all x and has the probability 
c  density function
c
c    f(x) =(1/sqrt(2*pi))*exp(-x*x/2).
c
c      input  arguments--n     =the desired integer number
c                                 of random numbers to be
c                                 generated.
c      output arguments--x     =a single precision vector
c                                (of dimension at least n)
c                                 into which the generated
c                                 random sample will be placed.
c      output--a random sample of size n
c              from the normal distribution
c              with mean=0 and standard deviation = 1.
c
c      references--box and muller, note on the generation
c                  of random normal deviates, journal of the
c                  association for computing machinery, 1958,
c                  pages 610-611.
c                --tocher, the art of simulation,
c                  1963, pages 33-34.
c                --hammersley and handscomb, monte carlo methods,
c                  1964, page 39.
c                --johnson and kotz, continuous univariate
c                  distributions--1, 1970, pages 40-111.
c
c      written by--james j. filliben
c                  statistical engineering laboratory(205.03)
c                  national bureau of standards
c                  washington, d. c. 20234
c
      integer n
c
      real pi
      parameter(pi=3.14159265358979)
c
      integer i
      integer iseed
      real ranval
      real u1
      real u2
      real x(n)
c
      intrinsic alog
      intrinsic cos
      external ranval
      intrinsic sin
      intrinsic sqrt
c
      do 10 i=1,n,2

        u1=ranval(iseed)
        u2=ranval(iseed)

        x(i)=sqrt(-2.0*alog(u1))*cos(2.0*pi*u2)

        if(i.lt.n)then
          x(i+1)=sqrt(-2.0*alog(u1))*sin(2.0*pi*u2)
        endif

10    continue

      return
      end
      function ranval(iseed)
c
c***********************************************************************
c
c     RANVAL IS THE PORTABLE RANDOM NUMBER GENERATOR OF L. SCHRAGE.
c
c     THE GENERATOR IS FULL CYCLE, THAT IS, EVERY INTEGER FROM
c     1 TO 2**31 - 2 IS GENERATED EXACTLY ONCE IN THE CYCLE.
c     IT IS COMPLETELY DESCRIBED IN TOMS 5(1979),132-138.
c
c     THE FUNCTION STATEMENT IS
c
c       REAL FUNCTION RANVAL(ISEED)
c
c     WHERE
c
c       ISEED IS A POSITIVE INTEGER VARIABLE WHICH SPECIFIES
c         THE SEED TO THE RANDOM NUMBER GENERATOR. GIVEN THE
c         INPUT SEED, RANVAL RETURNS A RANDOM NUMBER IN THE
c         OPEN INTERVAL(0,1). ON OUTPUT THE SEED IS UPDATED.
c
c     SET A=7**5, B15 = 2**15, B16 = 2**16, P = 2**31 - 1, C = 1/P.
c
      integer a
      parameter(a=16807)
c
      integer b15
      parameter(b15=32768)
c
      integer b16
      parameter(b16=65536)
c
      integer p
      parameter(p=2147483647)
c
      real c
      parameter(c=4.656612875e-10)
c
      integer fhi
      integer iseed
      integer k
      integer leftlo,xhi,xalo
      real ranval
c
c     THERE ARE 8 STEPS IN RANVAL.
c
c     1. GET 15 HI ORDER BITS OF ISEED.
c     2. GET 16 LO BITS OF ISEED AND FORM LO PRODUCT.
c     3. GET 15 HI ORDER BITS OF LO PRODUCT.
c     4. FORM THE 31 HIGHEST BITS OF FULL PRODUCT.
c     5. GET OVERFLO PAST 31ST BIT OF FULL PRODUCT.
c     6. ASSEMBLE ALL THE PARTS AND PRESUBSTRACT P.
c        THE PARENTHESES ARE ESSENTIAL.
c     7. ADD P BACK IN IF NECESSARY.
c     8. MULTIPLY BY 1/(2**31 - 1).
c
      xhi=iseed/b16
      xalo =(iseed-xhi*b16)*a
      leftlo=xalo/b16
      fhi=xhi*a+leftlo
      k=fhi/b15
      iseed =(((xalo-leftlo*b16)-p)+(fhi-k*b15)*b16)+k
      if(iseed.lt.0) iseed=iseed+p
      ranval=c*float(iseed)
      return
      end
