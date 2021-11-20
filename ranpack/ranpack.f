c  ranpack.f  29 April 1997
c
      function cnt1(st,idf,d,ifault)
c
c***********************************************************************
c
c  Algorithm AS 5, Applied Statistics, 1968, Volume 17, Page 193
c
c  CNT1 computes the lower tail of the non-central T distribution.
c  Specifically, CNT1 computes the area from minus infinity to ST
c  under a non-central T distribution with IDF degrees of freedom
c  and noncentrality parameter D.
c
c
c  ST     Input, REAL ST, the upper limit of integration.
c
c  IDF    Input, INTEGER IDF, the number of degrees of freedom.
c         IDF must be at least 1.
c
c  D      Input, REAL D, the noncentrality parameter.  If D=0, the
c         distribution is identical to the usual Student T
c         distribution.
c
c  IFAULT Output, INTEGER IFAULT, information and error flag.
c         0 if exact method used,
c         1 if normal approximation is used, (not an error
c           condition!)
c         2 if IDF<1 (Error condition).
c
      real pi
      parameter (pi=3.14159265358979323846)

      real a
      real ak
      real b
      real cnt1
      real d
      real da
      real dnorm3
      real drb
      real f
      real fk
      real fmkm1
      real fmkm2
      real gamln4
      integer idf
      integer ifault
      integer ioe
      integer jfault
      integer k
      real rb
      real st
      real sum
      real temp
      real tha1

      external dnorm3
      intrinsic exp
      external gamln4
      intrinsic mod
      intrinsic sqrt
      external tha1

      f=idf
      ifault=0

      if (idf.lt.1)then
        ifault=2
        cnt1=0.0
        return
      elseif(idf.gt.100)then
        ifault=1
        jfault=0
        temp=gamln4(0.5*(f-1.0),jfault)-gamln4(0.5*f,jfault)
        a=sqrt(0.5*f) * exp(temp)*d
        cnt1=dnorm3((st-a)/sqrt(f*(1.0+d*d)/(f-2.0)-a*a),.false.)
      elseif(idf.eq.1)then
        a=st/sqrt(f)
        b=f/(f+st*st)
        rb=sqrt(b)
        cnt1=dnorm3(d*rb,.true.)+2.0*tha1(d*rb,a)
      else
        ioe=mod(idf,2)
        a=st/sqrt(f)
        b=f/(f+st*st)
        rb=sqrt(b)
        da=d*a
        drb=d*rb
        sum=0.0

        if (abs(drb).ge.12.5)then
          fmkm2=0.0
        else
          fmkm2=a*rb*exp(-0.5*drb*drb)*dnorm3(a*drb,.false.)/sqrt(2.0*
     &      pi)
        endif

        fmkm1=b*da*fmkm2

        if (abs(d).le.12.5) then
          fmkm1=fmkm1+b*a*exp(-0.5*d*d)/(2.0*pi)
        end if
c
        if (ioe.eq.0)then
          sum=fmkm2
        else
          sum=fmkm1
        endif
c
        if (idf.lt.4)then
          if (ioe.ne.0)then
            cnt1=dnorm3(drb,.true.)+2.0*(sum+tha1(drb,a))
          else
            cnt1=dnorm3(d,.true.)+sum*sqrt(2.0*pi)
          endif
        else
        
          ak=1.0
          fk=2.0
          
          do k=2, idf-2, 2
            fmkm2=b*(da*ak*fmkm1+fmkm2)*(fk-1.0)/fk
            ak=1.0/(ak*(fk-1.0))
            fmkm1=b*(da*ak*fmkm2+fmkm1)*fk/(fk+1.0)
            if (ioe.eq.0)then
              sum=sum+fmkm2
            else
              sum=sum+fmkm1
            endif
            ak=1.0/(ak*fk)
            fk=fk+2.0
          enddo
          
          if (ioe.ne.0)then
            cnt1=dnorm3(drb,.true.)+2.0*(sum+tha1(drb,a))
          else
            cnt1=dnorm3(d,.true.)+sum*sqrt(2.0*pi)
          endif
          
        endif
      endif

      return
      end
      function ct1(t,idf,ifault)
c
c***********************************************************************
c
c  Algorithm AS 3, Applied Statistics, 1968, Volume 17, Page 189
c
c  CT1 computes the value of the cumulative Student's T distribution,
c  the area from -INFINITY to T under a Student's central T-distribution
c  with IDF degrees of freedom.
c
c  T      Input, REAL T, the upper limit of integration.
c
c  IDF    Input, INTEGER IDF, the number of degrees of freedom.
c         IDF must be at least 1.
c
c  IFAULT Output, INTEGER IFAULT, fault indicator.
c         0 if no fault.
c         1 if IDF<1.
c
c  CT1    Output, REAL CT1, the cumulative probability of a value
c         less than or equal to T, in the given distribution.
c
      real pi
      parameter (pi=3.14159265358979323846)
c
      real a
      real b
      real c
      real ct1
      real f
      real fk
      integer idf
      integer ifault
      integer ioe
      integer k
      integer ks
      real s
      real t
c
      ifault=0
      ct1=0.0
c
      if (idf.lt.1)then
        ifault=1
        return
      endif
c
      f=idf
      a=t/sqrt(f)
      b=f/(f+t*t)
      ioe=mod(idf,2)
      s=1.0
      c=1.0
      f=1.0
      ks=2+ioe
      fk=ks
      
      if (idf.ge.4)then
      
        do k=ks, idf-2, 2
          c=c*b*(fk-1.0)/fk
          s=s+c
          if (s.eq.f) go to 20
          f=s
          fk=fk+2.0
        enddo
        
      endif
      
   20 continue
   
      if (ioe.ne.1)then
        ct1=0.5+0.5*a*sqrt(b)*s
      else
        if (idf.eq.1) s=0.0
        ct1=0.5+(a*b*s+atan(a))/pi
      endif
      
      return
      end
      function digama(x,ifault)
c
c***********************************************************************
c
c  ALGORITHM AS 103  APPL. STATIST. (1976) VOL.25, NO.3
c
c  Calculates DIGAMMA(X)=D( LOG( GAMMA(X))) / DX
c
      real half
c
      parameter (half=0.5)
c
      real c
      real d1
      real digama
      integer ifault
      real r
      real s
      real s3
      real s4
      real s5
      real x
      real y
c
c  Set constants, SN=Nth Stirling coefficient, D1 = DIGAMMA(1.0)
c
      s=1.0e-05
      c=8.5
      s3=8.333333333e-02
      s4=8.3333333333e-03
      s5=3.968253968e-03
      d1=-0.5772156649
c
      digama=0.0
c
c  Check that the argument is positive
c
      if (x.le.0.0)then
        ifault=1
        write(*,*)' '
        write(*,*)'DIGAMA - Fatal error!'
        write(*,*)'  Argument X must be positive!'
        return
      endif
c
      y=x
      ifault=0
c
c  Use approximation if argument <= S.c
c  
c  Why are these lines commented out?
c
      if (y.le.s)then
        digama=d1-1.0/y
        return
      endif
c
c  Reduce to DIGAMA(X + N) where (X + N) >= C
c
   10 continue

      if (y.lt.c)then
        digama=digama-1.0/y
        y=y+1.0
        go to 10
      endif
c
c  Use Stirling's (actually de Moivre's) expansion if argument > C
c
      r=1.0/y
      digama=digama+log(y)-half*r
      r=r*r
      digama=digama-r*(s3-r*(s4-r*s5))

      return
      end
      function dnorm1(x)
c
c***********************************************************************
c
c  Compute the cumulative normal distribution function at X.
c
c  Adapted from a polynomial approximation in:
c  Ibbetson D, Algorithm 209
c  Collected Algorithms of the CACM 1963 p. 616
c
c  Note:
c
c  This routine has six digit accuracy, so it is only useful for 
c  absolute value of X < 6.
c
      real dnorm1
      real v
      real w
      real x
      real y
c
      if (x.le.-6.0)then
        dnorm1=0.0
        return
      endif
c
      if (x.eq.0.0)then
        dnorm1=0.5
        return
      endif
c
      if (x.gt.6.0)then
        dnorm1=1.0
        return
      endif
c
      y=0.5*abs(x)
      if (y.lt.1.0)then
        w=y*y
        v=((((((((0.000124818987*w-0.001075204047)*w+0.005198775019)*w
     &    -0.019198292004)*w+0.059054035642)*w-0.151968751364)*w+
     &    0.319152932694)*w-0.531923007300)*w+0.797884560593)*y*2.0
      else
        y=y-2.0
        v=(((((((((((((-0.000045255659*y+0.000152529290)*y-
     &    0.000019538132)*y-0.000676904986)*y+0.001390604284)*y-
     &    0.000794620820)*y-0.002034254874)*y+0.006549791214)*y-
     &    0.010557625006)*y+0.011630447319)*y-0.009279453341)*y+
     &    0.005353579108)*y-0.002141268741)*y+0.000535310849)*y+
     &    0.999936657524
      endif
      
      if (x.gt.0.0)then
        dnorm1=(1.0+v)*0.5
      else
        dnorm1=(1.0-v)*0.5
      endif
      
      return
      end
      function dnorm2(x,kode,nz)
c
c***********************************************************************
c
c     WRITTEN BY D.E. AMOS AND S.L. DANIEL, OCTOBER, 1974
c
c     reference sc-dr-72-0918
c
c         dnorm2 computes the cumulative normal distribution f(x) or
c         its complement 1.-f(x). chebyshev expansions for erf(z) on
c         0.le.z.lt.2 and erfc(z) on 2.le.z.le.4 and z.gt.4 are
c         used for evaluation.  the relations
c            f(x)=.5*erfc(z)   , z=-x/sqrt(2) , x.lt.-2.*sqrt(2)
c            f(x)=.5-.5*erf(z) , z=-x/sqrt(2) ,-2*sqrt(2).le.x.lt.0
c            f(x)=.5+.5*erf(z) , z= x/sqrt(2) , 0.le.x.lt.2*sqrt(2)
c            f(x)=1.-.5*erfc(z) , z=x/sqrt(2) , 2.le.z.lt.6
c            f(x)=1. , x.ge.6*sqrt(2)
c            f(-x)=1.-f(x) ,
c         are used to complete the definition on the real line so that
c         significant digits are retained over the full exponent range.
c
c     description of arguments
c
c         input
c           x      - argument of the distribution
c           kode   - a selection parameter
c                    kode=1 returns dnorm2=f(x)
c                    kode=2 returns dnorm2=1.-f(x)
c
c         output
c           dnorm2  - answer for f(x) or 1.-f(x) depending on kode.
c           nz     - underflow flag
c                    nz=0, a normal return
c                    nz=1, underflow, dnorm2=0.0 returned
c
c  error conditions
c
c  improper input for kode- a fatal error
c  underflow - a non-fatal error, xlim=-36.5444845898331 is the
c  critical value.
c
      real a1(21)
      real a2(23)
      real a3(17)
      real ans
      real b1
      real b2
      real dnorm2
      real frtwo
      integer i
      integer iflip
      integer j
      integer kode
      integer m1
      integer m2
      integer m3
      integer n1
      integer n2
      integer n3
      integer nz
      real rtwo
      real srtwo
      real temp
      real trtwo
      real tz
      real x
      real xlim
      real xs
      real xx
      real z
c
      data a1 / 2.94268192595158e-01,-1.20794002859252e-01,-
     &  5.38155411612267e-03,9.61245872309754e-03,-1.56928442055175e-03,
     &  -3.13379686339925e-04,1.34539944432857e-04,-2.01886311941572e-06
     &  ,-6.02924420904726e-06,7.33514212717164e-07,1.68200375041707e-07
     &  ,-4.21496636122487e-08,-2.34089537886964e-09,1.54397950861409e-
     &  09,-3.83910453258562e-11,-4.18791755643448e-11,3.66254323806330e
     &  -12,8.67662501706706e-13,-1.38490737068408e-13,-
     &  1.30609215123467e-14,3.76420840390550e-15 /
c
      data a2 / 3.93098827656776e-01,1*0.,-5.72072868438617e-03,1*0.,
     &  1.18630151591342e-04,1*0.,-3.91103704629101e-06,1*0.,
     &  1.72795234431416e-07,1*0.,-9.42361518118889e-09,1*0.,
     &  6.04595735693237e-10,1*0.,-4.42225118426537e-11,1*0.,
     &  3.60747153118711e-12,1*0.,-3.22932023145379e-13,1*0.,
     &  3.13323522258447e-14,1*0.,-3.26302072101379e-15 /
c
      data a3 / 2.66657721330163e-01,8.94380310492471e-03,-
     &  1.90087646908402e-03,3.48555717528185e-04,-5.81844230476253e-05,
     &  9.06838380458210e-06,-1.33859970500872e-06,1.88850668170541e-07,
     &  -2.56245596590501e-08,3.35935312400552e-09,-4.27010392442662e-10
     &  ,5.27726756655456e-11,-6.35545872359585e-12,7.47249710210314e-13
     &  ,-8.59121451944801e-14,9.67175305486972e-15,-1.06741339515971e-
     &  15 /
c
      data rtwo,trtwo,frtwo,srtwo,xlim / 1.41421356237310e+00,
     &  2.82842712474619e+00,5.65685424949238e+00,8.48528137423857e+00,-
     &  3.65444845898331e+01 /
c
      data n1,n2,n3,m1,m2,m3 / 21,23,17,19,21,15 /
c
      dnorm2=0.0
c
      if (kode.lt.1.or.kode.gt.2)then
        write(*,*)' '
        write(*,*)'DNORM2 - Fatal error!'
        write(*,*)'  Illegal input value of KODE=',kode
        write(*,*)'  KODE must be 1 or 2.'
        return
      endif
c
      nz=0

      if (kode.eq.1)then
        xx=x
      else
        xx=-x
      endif
c
      if (xx.lt.xlim)then
        dnorm2=0.0
        nz=1
        return
      endif
c
      if (xx.ge.srtwo)then
        dnorm2=1.0
        return
      endif
c
      if (xx.le.frtwo) go to 30
      iflip=1
   10 continue
      z=frtwo/xx
      tz=z+z
      j=n2
      b1=a2(j)
      b2=0.0
      
      do i=1, m2
        j=j-1
        temp=b1
        b1=tz*b1-b2+a2(j)
        b2=temp
      enddo
   
      ans=z*b1-b2+a2(1)
      dnorm2=(exp(-xx*xx*0.5)/xx)*ans

      if (iflip.eq.1) then
        dnorm2=1.0-dnorm2
      end if

      return
c
   30 if (xx.gt.trtwo)then
        xs=xx/rtwo
        go to 60
      endif 

      if (xx.lt.-trtwo) go to 50
      z=abs(xx)/rtwo-1.0
      tz=z+z
      j=n1
      b1=a1(j)
      b2=0.0

      do i=1, m1
        j=j-1
        temp=b1
        b1=tz*b1-b2+a1(j)
        b2=temp
      enddo
      
      dnorm2=xx*(z*b1-b2+a1(1))+0.5
      return
c
   50 continue

      if (xx.le.-frtwo)then
        iflip=0
        xx=-xx
        go to 10
      endif
c
      xs=-xx/rtwo

   60 continue

      z=xs-3.0
      tz=z+z
      j=n3
      b1=a3(j)
      b2=0.0

      do i=1, m3
        j=j-1
        temp=b1
        b1=tz*b1-b2+a3(j)
        b2=temp
      enddo
      
      ans=z*b1-b2+a3(1)
      dnorm2=exp(-xs*xs)*ans/xs

      if (xx.gt.0.0) then
        dnorm2=1.0-dnorm2
      end if
      
      return
      end
      function dnorm3(x,upper)
c
c***********************************************************************
c
c  Algorithm AS66 Applied Statistics (1973) vol22 no.3
c
c  Evaluates the tail area of the standardised normal curve
c  from x to infinity if upper is .true. or
c  from minus infinity to x if upper is .false.
c
      real half
c
      parameter (half=0.5)
c
      real a1
      real a2
      real a3
      real b1
      real b2
      real c1
      real c2
      real c3
      real c4
      real c5
      real c6
      real con
      real dnorm3
      real p
      real q
      real r
      real x
      real y
      real z
      real d1,d2,d3,d4,d5
      logical upper,up
c
c  machine dependent constants
c
      real ltone,utzero
      data ltone / 7.0 /,utzero / 18.66 /
      data con / 1.28 /
      data p / 0.398942280444 /,q / 0.39990348504 /,r / 0.398942280385 /
      data a1 / 5.75885480458 /,a2 / 2.62433121679 /,a3 / 5.92885724438
     &  /
      data b1 / -29.8213557807 /,b2 / 48.6959930692 /
      data c1 / -3.8052e-8 /,c2 / 3.98064794e-4 /,c3 / -0.151679116635 /
      data c4 / 4.8385912808 /,c5 / 0.742380924027 /,c6 / 3.99019417011
     &  /
      data d1 / 1.00000615302 /,d2 / 1.98615381364 /,d3 / 5.29330324926
     &  /
      data d4 / -15.1508972451 /,d5 / 30.789933034 /
c
      up=upper
      z=x

      if (z.lt.0.0)then
        up=.not.up
        z=-z
      endif
c
      if (z.le.ltone.or.(up.and.z.le.utzero))then
        y=half*z*z
        if (z.le.con)then
          dnorm3=0.5-z*(p-q*y/(y+a1+b1/(y+a2+b2/(y+a3))))
        else
          dnorm3=r*exp(-y)/(z+c1+d1/(z+c2+d2/(z+c3+d3/(z+c4+d4/(z+c5+
     &      d5/(z+c6))))))
        endif
        if (.not.up) dnorm3=1.0-dnorm3
      else
        if (up)then
          dnorm3=0.0
        else
          dnorm3=1.0
        endif
      endif
      
      return
      end
      subroutine dnorm4(z,p,q,pdf)
c
c***********************************************************************
c
c  Normal distribution probabilities accurate to 1.e-15.
c  Z=no. of standard deviations from the mean.
c  P, Q=probabilities to the left & right of Z.   P + Q = 1.
c  PDF=the probability density.
c
c  Based upon algorithm 5666 for the error function, from:
c  Hart, J.F. et al, 'Computer Approximations', Wiley 1968
c
c  Programmer: Alan Miller
c
c  Latest revision - 30 March 1986
c
      real cutoff
      real expntl
      real p
      real p0
      real p1
      real p2
      real p3
      real p4
      real p5
      real p6
      real pdf
      real q
      real q0
      real q1
      real q2
      real q3
      real q4
      real q5
      real q6
      real q7
      real root2pi
      real z
      real zabs
c
      data p0,p1,p2,p3,p4,p5,p6 / 220.2068679123761,221.2135961699311,
     &  112.0792914978709,33.91286607838300,6.373962203531650,.
     &  7003830644436881,.3526249659989109e-01 /
      data q0,q1,q2,q3,q4,q5,q6,q7
     &   / 440.4137358247522,793.8265125199484,637.3336333788311,
     &  296.5642487796737,86.78073220294608,16.06417757920695,
     &  1.755667163182642,.8838834764831844e-1 /
      data cutoff / 7.071 /
      data root2pi / 2.506628274631001 /
c
      zabs=abs(z)
c
c  ABS(Z) > 37.
c
      if (zabs.gt.37.0)then
        pdf=0.0
        if (z.gt.0.0)then
          p=1.0
          q=0.0
        else
          p=0.0
          q=1.0
        endif
        return
      endif
c
c  ABS(Z) <= 37.
c
      expntl=exp(-0.5*zabs**2)
      pdf=expntl/root2pi
c
c  ABS(Z) < CUTOFF=10/sqrt(2).
c
      if (zabs.lt.cutoff)then
        p=expntl*((((((p6*zabs+p5)*zabs+p4)*zabs+p3)*zabs+p2)*zabs+p1)
     &    *zabs+p0)/(((((((q7*zabs+q6)*zabs+q5)*zabs+q4)*zabs+q3)*zabs+
     &    q2)*zabs+q1)*zabs+q0)
c
c  ABS(Z) >= CUTOFF.
c
      else
        p=pdf/(zabs+1.0/(zabs+2.0/(zabs+3.0/(zabs+4.0/(zabs+0.65)))))
      endif
c
      if (z.lt.0.0)then
        q=1.0-p
      else
        q=p
        p=1.0-q
      endif
      
      return
      end
      subroutine dnorm5(z,p,q,pdf)
c
c***********************************************************************
c
c  P, Q=PROBABILITIES TO THE LEFT AND RIGHT OF Z
c  FOR THE STANDARD NORMAL DISTRIBUTION.
c  PDF =THE PROBABILITY DENSITY FUNCTION
c
c  REFERENCE: ADAMS,A.G. AREAS UNDER THE NORMAL CURVE,
c  ALGORITHM 39, COMPUTER J., VOL. 12, 197-8, 1969.
c
c  LATEST REVISION - 23 JANUARY 1981
c
      real a0
      real a1
      real a2
      real a3
      real a4
      real a5
      real a6
      real a7
      real b0
      real b1
      real b10
      real b11
      real b2
      real b3
      real b4
      real b5
      real b6
      real b7
      real b8
      real b9
      real p
      real pdf
      real q
      real y
      real z
      real zabs
c
      data a0,a1,a2,a3,a4,a5,a6,a7 / 0.5,0.398942280444,0.399903438504,
     &  5.75885480458,29.8213557808,2.62433121679,48.6959930692,
     &  5.92885724438 /
c
      data b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11 /
     &   0.398942280385,3.8052e-8,1.00000615302,3.98064794e-4,
     &  1.98615381364,0.151679116635,5.29330324926,4.8385912808,
     &  15.1508972451,0.742380924027,30.789933034,3.99019417011 /
c
      zabs=abs(z)
c
c  12.7 < ZABS
c
      if (zabs.gt.12.7)then
        q=0.0
        pdf=0.0
      else
        y=a0*z*z
        pdf=exp(-y)*b0
c
c  1.28 <= ZABS <= 12.7
c
        if (zabs.gt.1.28)then
          q=pdf/(zabs-b1+b2/(zabs+b3+b4/(zabs-b5+b6/(zabs+b7-b8/(zabs+
     &      b9+b10/(zabs+b11))))))
c
c  0 <= ZABS <= 1.28
c
        else
          q=a0-zabs*(a1-a2*y/(y+a3-a4/(y+a5+a6/(y+a7))))
        endif
      endif
c
      p=1.0-q
c
c  Z negative
c
      if (z.lt.0.0)then
        p=q
        q=1.0-p
      endif
      
      return
      end
      function dnorm6(xx)
c
c***********************************************************************
c
      real pi
      parameter (pi=3.14159265358979323846)
c
      real dnorm6
      real p
      real x
      real xx
c
      x=abs(xx)
      
      if (x.le.5.5)then
        p=exp(-((83.0*x+351.0)*x+562.0)*x/(703.0+165.0*x))
      else
        p=sqrt(2.0/pi)*exp(-(0.5*x*x+0.94/(x*x)))/x
      endif

      dnorm6=0.5*p

      if (xx.gt.0.0)then
        dnorm6=1.0-dnorm6
      endif
      
      return
      end
      function exp1(iseed)
c
c***********************************************************************
c
c  Random numbers, exponential distribution.
c  EXP1 was extracted from Numerical Recipes.
c
      real exp1
      integer iseed
      real random
c
      exp1=-log(random(iseed))

      return
      end
      function exp2(ir)
c
c***********************************************************************
c
c  EXP2 was originally named SEXPO, and was extracted from ACM Algorithm
c
c  Reference:
c
c  ahrens, j.h. and dieter, u.
c  computer methods for sampling from the exponential and normal distrib
c  comm. acm, 15,10 (oct. 1972), 873 - 882.
c
c  all statement numbers correspond to the steps of algorithm
c  'sa' in the above paper (slightly modified implementation)
c
      real a
      real exp2
      integer i
      integer ir
      real q(8)
      real ran17
      real u
      real umin
      real ustar
c
      save q
c
c  q(n)=sum(log(2.0)**k/k!)    k=1,..,n ,      the highest n
c  (here 8) is determined by q(n)=1.0 within standard precision
c
      data q / .6931472,.9333737,.9888778,.9984959,.9998293,.9999833,.
     &  9999986,.9999999 /
c
      a=0.0
      u=ran17(ir)

   10 continue

      u=u+u

      if (u.le.1.0)then
        a=a+q(1)
        go to 10
      endif

      u=u-1.0

      if (u.le.q(1))then
        exp2=a+u
        return
      endif

      i=1
      ustar=ran17(ir)
      umin=ustar

   20 continue

      ustar=ran17(ir)
      if (ustar.lt.umin) umin=ustar
      i=i+1
      if (u.gt.q(i)) go to 20

      exp2=a+umin*q(1)

      return
      end
      function gam1(ia,iseed)
c
c***********************************************************************
c
c  Random numbers, gamma distribution.
c
c  Extracted from Numerical Recipes
c
      real am
      real e
      real gam1
      integer ia
      integer iseed
      integer j
      real random
      real s
      real v1
      real v2
      real x
      real y
c
      gam1=0.0

      if (ia.lt.1)then
        write(*,*)' '
        write(*,*)'GAM1 - Fatal error!'
        write(*,*)'  Illegal input value of IA<1 =',ia
        return
      endif
c
      if (ia.lt.6)then
      
        x=1.0
        do j=1,ia
          x=x*random(iseed)
        enddo
        x=-log(x)
        
      else
      
   20   continue

        v1=2.0*random(iseed)-1.0
        v2=2.0*random(iseed)-1.0

        if (v1**2+v2**2.gt.1.0) go to 20
        if (v1**2+v2**2.eq.0.0) go to 20

        y=v2/v1
        am=ia-1
        s=sqrt(2.0*am+1.0)
        x=s*y+am
        if (x.le.0.0)go to 20

        e=(1.0+y**2)*exp(am*log(x/am)-s*y)
        if (random(iseed).gt.e) go to 20

      endif
      
      gam1=x
      
      return
      end
      function gam2(ir,a)
c
c***********************************************************************
c
c  References:
c
c  ahrens, j.h. and dieter, u.
c  generating gamma variates by a modified rejection technique.
c  comm. acm, 25,1 (jan. 1982), 47 - 54.
c
c  ahrens, j.h. and dieter, u.
c  computer methods for sampling from gamma, beta, poisson and
c    binomial distributions.
c  computing, 12 (1974), 223 - 246.
c
c     input:  ir=current state of basic random number generator
c             a =parameter (mean) of the standard gamma distribution
c
c     output: GAM2=sample from the gamma-(a)-distribution
c
c     coefficients q(k) - for q0=sum(q(k)*a**(-k))
c     coefficients a(k) - for q=q0+(t*t/2)*sum(a(k)*v**k)
c     coefficients e(k) - for exp(q)-1=sum(e(k)*q**k)
c
      real a
      real a1
      real a2
      real a3
      real a4
      real a5
      real a6
      real a7
      real aa
      real aaa
      real b
      real c
      real d
      real e
      real e1
      real e2
      real e3
      real e4
      real e5
      real exp2
      real gam2
      real gau8
      integer ir
      real p
      real q
      real q0
      real q1
      real q2
      real q3
      real q4
      real q5
      real q6
      real q7
      real r
      real ran17
      real s
      real t
      real u
      real v
      real w
      real x
c
      data q1,q2,q3,q4,q5,q6,q7 / .04166669,.02083148,.00801191,.
     &  00144121,-.00007388,.00024511,.00024240 /
      data a1,a2,a3,a4,a5,a6,a7 / .3333333,-.2500030,.2000062,-.1662921,
     &  .1423657,-.1367177,.1233795 /
      data e1,e2,e3,e4,e5 / 1.,.4999897,.1668290,.0407753,.0102930 /
c
c  previous a pre-set to zero - aa is a', aaa is a"
c  sqrt32 is the squareroot of 32=5.656854249492380
c
      data aa / 0.0 /
      data aaa / 0.0 /
c
      sqrt32=5.656854249492380
c
      if (a.eq.aa) go to 10
      if (a.lt.1.0) go to 120
c
c  step  1:  recalculations of s2,s,d if a has changed
c
      aa=a
      s2=a-0.5
      s=sqrt(s2)
      d=sqrt32-12.0*s
c
c  step  2:  t=standard normal deviate, x=(s,1/2)-normal deviate.
c  immediate acceptance (i)
c
   10 continue

      t=gau8(ir)
      x=s+0.5*t
      gam2=x*x
      if (t.ge.0.0) return
c
c  step  3:  u= 0,1 -uniform sample. squeeze acceptance (s)
c
      u=ran17(ir)
      if (d*u.le.t*t*t) return
c
c  step  4:  recalculations of q0,b,si,c if necessary
c
      if (a.eq.aaa) go to 40
      aaa=a
      r=1.0/a
      q0=((((((q7*r+q6)*r+q5)*r+q4)*r+q3)*r+q2)*r+q1)*r
c
c  approximation depending on size of parameter a the constants in the
c  expressions for b, si and c were established by numerical experiments
c
      if (a.le.3.686) go to 30
      if (a.le.13.022) go to 20
c
c  case 3:  a .gt. 13.022
c
      b=1.77
      si=0.75
      c=0.1515/s
      go to 40
c
c  case 2:  3.686 .lt. a .le. 13.022
c
   20 continue

      b=1.654+0.0076*s2
      si=1.68/s+0.275
      c=0.062/s+0.024
      go to 40
c
c  case 1:  a .le. 3.686
c
   30 continue

      b=0.463+s-0.178*s2
      si=1.235
      c=0.195/s-0.079+0.016*s
c
c  step  5:  no quotient test if x not positive
c
   40 continue

      if (x.le.0.0) go to 70
c
c  step  6:  calculation of v and quotient q
c
      v=t/(s+s)

      if (abs(v).gt.0.25) then
        q=q0-s*t+0.25*t*t+(s2+s2)*log(1.0+v)
      else
        q=q0+0.5*t*t*((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v
      end if
c
c  step  7:  quotient acceptance (q)
c
      if (log(1.0-u).le.q) return
c
c     step  8:  e=standard exponential deviate
c               u= 0,1 -uniform deviate
c               t=(b,si)-double exponential (laplace) sample
c
   70 continue

      e=exp2(ir)
      u=ran17(ir)
      u=u+u-1.0
      t=b+sign(si*e,u)
c
c  step  9:  rejection if t .lt. tau(1)=-.71874483771719
c
      if (t.lt.(-.7187449)) go to 70
c
c  step 10:  calculation of v and quotient q
c
      v=t/(s+s)

      if (abs(v).gt.0.25) then
        q=q0-s*t+0.25*t*t+(s2+s2)*log(1.0+v)
      else
        q=q0+0.5*t*t*((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v
      end if
c
c     step 11:  hat acceptance (h) (if q not positive go to step 8)
c
      if (q.le.0.0) go to 70

      if (q.gt.0.5) then
        w=exp(q)-1.0
      else
        w=((((e5*q+e4)*q+e3)*q+e2)*q+e1)*q
      end if
c
c  if t is rejected, sample again at step 8
c
      if (c*abs(u).gt.w*exp(e-0.5*t*t)) go to 70
      x=s+0.5*t
      gam2=x*x
      return
c
c  Alternate method for parameters a below 1  (.3678794=exp(-1.))
c
  120 continue

      aa=0.0
      b=1.0+0.3678794*a

  130 continue

      p=b*ran17(ir)

      if (p.ge.1.0)then
        gam2=-log((b-p)/a)
        if (exp2(ir).lt.(1.0-a)*log(gam2))go to 130
        return
      endif

      gam2=exp(log(p)/a)
      if (exp2(ir).lt.gam2)go to 130
  
      return
      end
      subroutine gamic(x,alpha,rel,n,y,nz)
c
c***********************************************************************
c
c  WRITTEN BY D.E. AMOS AND S.L. DANIEL, NOVEMBER, 1974.
c
c  REFERENCE SC-DR-72 0303
c
c  ABSTRACT
c
c         GAMIC COMPUTES AN N MEMBER SEQUENCE OF INCOMPLETE GAMMA
c         FUNCTIONS NORMALIZED SO THAT AT X=INFINITY, THE INCOMPLETE
c         GAMMA FUNCTION HAS THE VALUE 1. THE SEQUENCE IS DENOTED BY
c
c         Y(K)=INCGAMMA(ALPHA+K-1,X)/GAMMA(ALPHA+K-1),  K=1,2,...,N
c
c         AND IS COMPUTED TO A RELATIVE ERROR REL OR BETTER WHERE ALPHA
c         .GT.0. IF ALPHA+N-1.GE.X, THE LAST MEMBER IS COMPUTED BY THE
c         CONFLUENT HYPERGEOMETRIC SERIES WITH THE OTHER MEMBERS
c         COMPUTED BY BACKWARD RECURSION ON A TWO-TERM FORMULA,
c
c           Y(K-1)=Y(K)+EXP((ALPHA+K-1)*ALOG(X)-X-GAMLN1(ALPHA+K)).
c
c         IF ALPHA+N-1.LT.X, AN INTEGER M IS ADDED SO THAT
c         ALPHA+N-1+M.GE.X AND THE FIRST PROCEDURE IS APPLIED. SPECIAL
c         PROCEDURES APPLY FOR ALPHA.EQ.1 OR AN UNDERFLOW OCCURS OR
c         X EXCEEDS A CRITICAL VALUE, APTEST, WHERE ALL MEMBERS ARE 1.
c         TO THE WORD LENGTH OF THE CDC 6600. GAMIC USES GAMLN1.
c
c     DESCRIPTION OF ARGUMENTS
c
c         INPUT
c           X      - ARGUMENT, X.GE.0.0
c           ALPHA  - PARAMETER, ALPHA.GT.0.0
c           REL    - RELATIVE ERROR TOLERANCE, REL=1.E-S FOR S
c                    SIGNIFICANT DIGITS
c           N      - NUMBER OF GAMMA FUNCTIONS IN THE SEQUENCE
c                    BEGINNING AT PARAMETER ALPHA, N.GE.1
c
c         OUTPUT
c           Y      - A VECTOR CONTAINING AN N MEMBER SEQUENCE
c                    Y(K)= INCGAMMA(ALPHA+K-1,X)/GAMMA(ALPHA+K-1),
c                    K=1,...,N TO A RELATIVE ERROR REL.
c           NZ     - UNDERFLOW FLAG
c                    NZ.EQ.0, A NORMAL RETURN
c                    NZ.NE.0, UNDERFLOW, Y(K)=0.0, K=N-NZ+1,N RETURNED
c
c     ERROR CONDITIONS
c         IMPROPER INPUT PARAMETERS - A FATAL ERROR
c         UNDERFLOW - A NON-FATAL ERROR.
c
      real a1
      real aa(6)
      real abar
      real abk
      real ak
      real alpha
      real am1
      real apn
      real aptest
      real ascl
      real bb(6)
      real cc(5)
      real con14
      real d
      real e
      real exe
      real f
      real gamln1
      integer i
      integer k
      integer kk
      integer n
      integer nbar
      integer nm1
      integer nn
      integer nz
      real r1mach
      real rel
      real rx
      real s1
      real s2
      real scale
      real slog
      real sum
      real x
      real xlim
      real xlog
      real xod
      real y(1)
      real yy
c
      data aa / 1.18399941922176e+00,3.30888136276861e+02,
     &  1.04930832947926e+04,3.78420325596908e+04,1.57586618187374e+02,
     &  1.30569632410551e+03 /
c
      data bb / 1.02652821626751e+00,9.29753107520368e+03,
     &  6.53848923630220e+06,2.89543295992889e+08,8.16836456953161e+03,
     &  4.12237656364399e+06 /
c
      data cc / 4.30952856710482e+05,8.27988256743362e+09,
     &  2.41944468684445e+12,4.21722873236008e+05,7.56593802747116e+09 /
      data slog / -6.67749676968273e+2 /
      data con14 / 3.32361913019165e+1 /
      data ascl / 1.e-18 /
c
      scale=r1mach(1)

      if (rel.le.0.0)then
        write(*,*)' '
        write(*,*)'GAMIC - Fatal error!'
        write(*,*)'  Illegal value of REL <=0 =',rel
        return
      endif

      if (n.lt.1)then
        write(*,*)' '
        write(*,*)'GAMIC - Fatal error!'
        write(*,*)'  Illegal value of N < 1 =',n
        return
      endif

      if (alpha.le.0.0)then
        write(*,*)' '
        write(*,*)'GAMIC - Fatal error!'
        write(*,*)'  Illegal value of ALPHA <= 0=',alpha
        return
      endif

      if (x.lt.0.0)then
        write(*,*)' '
        write(*,*)'GAMIC - Fatal error!'
        write(*,*)'  Illegal value of X <= 0.0=',x
        return
      endif
c
      nz=0
      if (x.eq.0.0)then
      
        do i=1, n
          y(i)=0.0
        enddo
        
        return
      endif
c
c     IF X.GE.XLIM(ALPHA+N-1), THEN Y(K)=1., K=1,2,...,N
c
      nn=n
      rx=1.0/x
      nbar=0
      apn=alpha+float(n)-1.0
      aptest=apn
      am1=alpha-1.0
      if (apn.le.1.0) go to 80
   20 if (x.le.aptest) go to 110
      if (aptest.gt.200.0) go to 30
      s1=((aa(1)*aptest+aa(2))*aptest+aa(3))*aptest+aa(4)
      s2=(aptest+aa(5))*aptest+aa(6)
      go to 40
   30 if (aptest.gt.10000.0) go to 50
      s1=((bb(1)*aptest+bb(2))*aptest+bb(3))*aptest+bb(4)
      s2=(aptest+bb(5))*aptest+bb(6)
   40 xlim=s1/s2
c
      if (x.ge.xlim) go to 60
      go to 100
   50 s1=((aptest+cc(1))*aptest+cc(2))*aptest+cc(3)
      s2=(aptest+cc(4))*aptest+cc(5)
      go to 40
   60 do i=1, n
        y(i)=1.0
      enddo
      
      return
c
   80 if (alpha.ne.1.0) go to 90
      if (x.gt.con14) go to 60
      if (x.lt.0.1) go to 110
      y(nn)=1.0-exp(-x)
      return
   90 nbar=1
      aptest=apn+1.0
      go to 20
  100 nbar=x-aptest+5.+float(nbar)
  110 abar=apn+float(nbar)
      xlog=log(x)
      a1=1.0
      sum=1.0
      abk=abar+1.0
  120 a1=a1*x/abk
      sum=sum+a1
      if (a1.lt.rel) go to 130
      abk=abk+1.0
      go to 120
  130 yy=sum*scale
      d=abar
      if (nbar.eq.0) go to 160
  140 continue

      do k=1, nbar
        xod=x/d
        if (xod.ge.ascl) yy=xod*yy+scale
        d=d-1.0
      enddo

      if (nz.ne.0) go to 170
  160 e=-x+d*xlog-gamln1(d+1.0)
  170 if (e.ge.slog) go to 180
      y(nn)=0.0
      nz=nz+1
      nn=nn-1
      if (nn.eq.0) return
      nbar=1
      apn=apn-1.0
      e=e+log(d*rx)
      go to 140
  180 exe=exp(e)
      y(nn)=(exe/scale)*yy
      nm1=nn-1
      if (nm1.eq.0) return
      f=exe*apn*rx
      kk=nn
      ak=float(nn)+am1
      
      do k=1, nm1
        y(kk-1)=y(kk)+f
        kk=kk-1
        ak=ak-1.0
        f=f*ak*rx
      enddo
  
      return
      end
      function gamic1(x,p,ifault)
c
c***********************************************************************
c
c  Algorithm AS 147  Appl. Statist. (1980) Vol. 29, No. 1
c
c  Computes the incomplete gamma integral for positive
c  parameters X and P using an infinite series
c
c  AS 239 should be considered as an alternative to AS 147
c
      real a
      real arg
      real c
      real f
      real gamic1
      real gamln3
      integer ifault
      real p
      real r1mach
      real x
c
      ifault=0
c
      if (x.lt.0.0)then
        ifault=1
        gamic1=0.0
        return
      endif
c
      if (x.eq.0.0)then
        gamic1=0.0
        return
      endif
c
      if (p.le.0.0)then
        ifault=2
        gamic1=0.0
        return
      endif
c
c  GAMLN3 is natural log of gamma function
c
      arg=p*log(x)-gamln3(p+1.0)-x
      if (arg.lt.log(r1mach(1)))then
        ifault=3
        gamic1=0.0
        return
      endif
c
      f=exp(arg)
      if (f.eq.0.0)then
        ifault=4
        gamic1=0.0
        return
      endif
c
c  Series begins
c
      c=1.0
      gamic1=1.0
      a=p
   10 continue
      a=a+1.0
      c=c*x/a
      gamic1=gamic1+c
      if (c/gamic1.gt.r1mach(4)) go to 10
      gamic1=gamic1*f
      return
      end
      function gamic2(x,p)
c
c***********************************************************************
c
c  Extracted from Numerical Recipes.
c
      real gamic2
      real gammcf
      real gamser
      real gln
      real p
      real x
c
      gamic2=0.0

      if (x.lt.0.0)then
        write(*,*)' '
        write(*,*)'GAMIC2 - Fatal error!'
        write(*,*)'  Illegal input, X < 0 =',x
        return
      endif

      if (p.le.0.0)then
        write(*,*)' '
        write(*,*)'GAMIC2 - Fatal error!'
        write(*,*)'  Illegal input, P <= 0.0 =',p
        return
      endif
c
      if (x.lt.p+1.0)then
        call gser(gamser,p,x,gln)
        gamic2=gamser
      else
        call gcf(gammcf,p,x,gln)
        gamic2=1.0-gammcf
      endif

      return
      end
      function gamic3(x,p,ifault)
c
c***********************************************************************
c
c  Algorithm AS 32 Journal of the Royal Statistical Society C.
c  (1970) Vol.19 No. 3
c
c  Algorithm AS 239 is recommended as an alternative.
c
c  Computes incomplete gamma ratio for positive values of arguments X an
c  Uses series expansion if P.gt.X or X.le.1, otherwise continued fracti
c
c  Revised to incorporate the recommendations of Rice & Das,
c  Applied Statistics, 34, 326, 1985, 
c
c  and of Cran, 
c  Applied Statistics,
c  38, 423, 1989.
c
c  X,
c  P      Input, REAL X, P, the arguments to the incomplete gamma functi
c         X must be nonnegative, and P must be positive.
c
c  IFAULT Output, INTEGER IFAULT, error indicator.
c         0, no error.
c         1, P <=0
c         2, X < 0
c         3, P*LOG(X) - X - LOG(GAMMA(P)) is too close to zero.
c
      real a
      real an
      real arg
      real b
      real dif
      real factor
      real g
      real gamic3
      real gamln4
      real gin
      integer i
      integer ifault
      real p
      real pn(6)
      real r1mach
      real rn
      real term
      real x
c
c  Define accuracy and initialize
c
      gin=0.0
      ifault=0
c
c  Test for admissibility of arguments
c
      if (p.le.0.0)then
        ifault=1
        gamic3=0.0
        return
      endif
c
      if (x.lt.0.0)then
        ifault=2
        gamic3=0.0
        return
      endif
c
      if (x.eq.0.0)then
        gamic3=0.0
        return
      endif
c
      g=gamln4(p,ifault)
      arg=p*log(x)-x-g
      if (arg.lt.log(r1mach(1)))then
        ifault=3
        gamic3=0.0
        return
      endif
c
      factor=exp(arg)
      if (x.gt.1.0.and.x.ge.p) go to 20
c
c  Calculation by series expansion
c
      gin=1.0
      term=1.0
      rn=p

10    continue

      rn=rn+1.0
      term=term*x/rn
      gin=gin+term
      if (term.gt.r1mach(4)) go to 10
      gin=gin*factor/p
      go to 100
c
c  Calculation by continued fraction
c
   20 a=1.0-p
      b=a+x+1.0
      term=0.0
      pn(1)=1.0
      pn(2)=x
      pn(3)=x+1.0
      pn(4)=x*b
      gin=pn(3)/pn(4)
c
   30 a=a+1.0
      b=b+2.0
      term=term+1.0
      an=a*term
      do i=1, 2
        pn(i+4)=b*pn(i+2)-an*pn(i)
      enddo
      
      if (pn(6).eq.0.0) go to 60
      rn=pn(5)/pn(6)
      dif=abs(gin-rn)
      if (dif.gt.r1mach(4)) go to 50
      if (dif.le.r1mach(4)*rn) go to 90
   50 gin=rn
   60 do i=1, 4
        pn(i)=pn(i+2)
      enddo
      
      if (abs(pn(5)).lt.r1mach(2)) go to 30
      do i=1, 4
        pn(i)=pn(i)/r1mach(2)
      enddo
      go to 30
c
   90 gin=1.0-factor*gin
  100 continue
      gamic3=gin
      return
      end
      function gamic4(x,p,ifault)
c
c***********************************************************************
c
c  ALGORITHM AS 239
c  Applied Statistics
c  (1988) VOL. 37, NO. 3
c
c  Computation of the incomplete gamma function
c
c  X,
c  P      Input, REAL X, P, the arguments of the incomplete gamma functi
c         X must be nonnegative, and P must be positive.
c
c  IFAULT Output, INTEGER IFAULT, error flag.
c         0, no error.
c         1, P<=0, or X<0.
c
c  GAMIC4 Output, REAL GAMIC4, the value of the incomplete gamma functio
c
      real elimit
      real nine
      real plimit
      real three
      real two
      real xbig
c
      parameter (elimit=-88.0)
      parameter (nine=9.0)
      parameter (plimit=1000.0)
      parameter (three=3.0)
      parameter (two=2.0)
      parameter (xbig=1.0e+8)
c
      real a
      real an
      real arg
      real b
      real c
      real dnorm3
      real gamic4
      real gamln4
      integer ifault
      real oflo
      real p
      real pn1
      real pn2
      real pn3
      real pn4
      real pn5
      real pn6
      real r1mach
      real rn
      real tol
      real x
c
      external gamln4
      external dnorm3
      external r1mach
      intrinsic sqrt
c
      oflo=sqrt(r1mach(2))
      tol=r1mach(4)
      gamic4=0.0
c
c  Check that we have valid values for X and P
c
      if (p.le.0.0.or.x.lt.0.0)then
        ifault=1
        return
      endif
      
      ifault=0
      if (x.eq.0.0) return
c
c  Use a normal approximation if P > PLIMIT
c
      if (p.gt.plimit)then
        pn1=three*sqrt(p)*((x/p)**(1.0/three)+1.0/(nine*p)-1.0)
        gamic4=dnorm3(pn1,.false.)
        return
      endif
c
c  If X is extremely large compared to P then set GAMIC4=1
c
      if (x.gt.xbig)then
        gamic4=1.0
        return
      endif
c
      if (x.le.1.0.or.x.lt.p)then
c
c  Use Pearson's series expansion.
c  (Note that P is not large enough to force overflow in GAMLN4).
c  No need to test IFAULT on exit since P > 0.
c
        arg=p*log(x)-x-gamln4(p+1.0,ifault)
        c=1.0
        gamic4=1.0
        a=p
   10   a=a+1.0
        c=c*x/a
        gamic4=gamic4+c
        if (c.gt.tol) go to 10
        arg=arg+log(gamic4)
        gamic4=0.0
        if (arg.ge.elimit) gamic4=exp(arg)
c
      else
c
c  Use a continued fraction expansion
c
        arg=p*log(x)-x-gamln4(p,ifault)
        a=1.0-p
        b=a+x+1.0
        c=0.0
        pn1=1.0
        pn2=x
        pn3=x+1.0
        pn4=x*b
        gamic4=pn3/pn4
   20   a=a+1.0
        b=b+2.0
        c=c+1.0
        an=a*c
        pn5=b*pn3-an*pn1
        pn6=b*pn4-an*pn2
        
        if (abs(pn6).gt.0.0)then
          rn=pn5/pn6
          if (abs(gamic4-rn).le.min(tol,tol*rn)) go to 30
          gamic4=rn
        endif
c
        pn1=pn3
        pn2=pn4
        pn3=pn5
        pn4=pn6
        if (abs(pn5).ge.oflo)then
c
c  Re-scale terms in continued fraction if terms are large
c
          pn1=pn1/oflo
          pn2=pn2/oflo
          pn3=pn3/oflo
          pn4=pn4/oflo
        endif
        
        go to 20
        
   30   arg=arg+log(gamic4)
        gamic4=1.0
        if (arg.ge.elimit) gamic4=1.0-exp(arg)

      endif
c
      return
      end
      subroutine gcf(gammcf,a,x,gln)
c
c***********************************************************************
c
c  Needed by GAMIC2
c
      integer itmax
      parameter (itmax=100)
c
      real a
      real a0
      real a1
      real an
      real ana
      real anf
      real b0
      real b1
      real eps
      real fac
      real g
      real gamln2
      real gammcf
      real gln
      real gold
      integer n
      real r1mach
      real x
c
      eps=r1mach(4)
      gln=gamln2(a)
      gold=0.0
      a0=1.0
      a1=x
      b0=0.0
      b1=1.0
      fac=1.0
      
      do n=1,itmax
        an=float(n)
        ana=an-a
        a0=(a1+a0*ana)*fac
        b0=(b1+b0*ana)*fac
        anf=an*fac
        a1=x*a0+anf*a1
        b1=x*b0+anf*b1
        
        if (a1.ne.0.)then
          fac=1./a1
          g=b1*fac
          
          if (abs((g-gold)/g).lt.eps)then
            gammcf=exp(-x+a*log(x)-gln)*g
            return
          endif
          
          gold=g
        endif
        
      enddo
   
      write(*,*)' '
      write(*,*)'GCF - Warning!'
      write(*,*)'  No convergence.'
      write(*,*)'  A may be too large or ITMAX too small'

      return
      end
      subroutine gser(gamser,a,x,gln)
c
c***********************************************************************
c
c  Needed by GAMIC2
c
      integer itmax
      parameter (itmax=100)
c
      real a
      real ap
      real del
      real eps
      real gamln2
      real gamser
      real gln
      integer n
      real r1mach
      real sum
      real x
c
      if (x.lt.0.0)then
        write(*,*)' '
        write(*,*)'GSER - Fatal error!'
        write(*,*)'  X < 0=',x
        gamser=0.0
        return
      endif

      if (x.eq.0.0)then
        gamser=0.0
        return
      endif
c
      eps=r1mach(4)
      gln=gamln2(a)
      ap=a
      sum=1.0/a
      del=sum
      
      do n=1, itmax
      
        ap=ap+1.0
        del=del*x/ap
        sum=sum+del
        
        if (abs(del).lt.abs(sum)*eps)then
          gamser=sum*exp(-x+a*log(x)-gln)
          return
        endif
        
      enddo
   
      write(*,*)' '
      write(*,*)'GSER - Fatal error!'
      write(*,*)'  No convergence.'
      write(*,*)'  A may be too large,'
      write(*,*)'  or ITMAX may be too small'

      return
      end
      function gamln1(x)
c
c***********************************************************************
c
c     WRITTEN BY D. E. AMOS, SEPTEMBER, 1977.
c
c     REFERENCES
c         SAND-77-1518
c
c         COMPUTER APPROXIMATIONS BY J.F.HART, ET.AL., SIAM SERIES IN
c         APPLIED MATHEMATICS, WILEY, 1968, P.135-136.
c
c         NBS HANDBOOK OF MATHEMATICAL FUNCTIONS, AMS 55, BY
c         M. ABRAMOWITZ AND I.A. STEGUN, DECEMBER. 1955, P.257.
c
c     ABSTRACT
c         GAMLN1 COMPUTES THE NATURAL LOG OF THE GAMMA FUNCTION FOR
c         X.GT.0. A RATIONAL CHEBYSHEV APPROXIMATION IS USED ON
c         8.LT.X.LT.1000., THE ASYMPTOTIC EXPANSION FOR X.GE.1000. AND
c         A RATIONAL CHEBYSHEV APPROXIMATION ON 2.LT.X.LT.3. FOR
c         0.LT.X.LT.8. AND X NON-INTEGRAL, FORWARD OR BACKWARD
c         RECURSION FILLS IN THE INTERVALS  0.LT.X.LT.2 AND
c         3.LT.X.LT.8. FOR X=1.,2.,...,100., GAMLN1 IS SET TO
c         NATURAL LOGS OF FACTORIALS.
c
c     DESCRIPTION OF ARGUMENTS
c
c         INPUT
c           X      - X.GT.0
c
c         OUTPUT
c           GAMLN1  - NATURAL LOG OF THE GAMMA FUNCTION AT X
c
c     ERROR CONDITIONS
c         IMPROPER INPUT ARGUMENT - A FATAL ERROR
c
      real dgam
      real dx
      real gamln1
      real gln(100)
      integer i
      integer ndx
      integer nxm
      real p(5)
      real pcoe(9)
      real px
      real q(2)
      real qcoe(4)
      real qx
      real rtwpil
      real rx
      real rxx
      real sum
      real t
      real x
      real xlim1
      real xlim2
      real xx
c
      data xlim1,xlim2,rtwpil / 8.,1000.,9.18938533204673e-01 /
c
      data p / 7.66345188000000e-04,-5.94095610520000e-04,
     &  7.93643110484500e-04,-2.77777775657725e-03,8.33333333333169e-02
     &  /
c
      data q / -2.77777777777778e-03,8.33333333333333e-02 /
c
      data pcoe / 2.97378664481017e-03,9.23819455902760e-03,
     &  1.09311595671044e-01,3.98067131020357e-01,2.15994312846059e+00,
     &  6.33806799938727e+00,2.07824725317921e+01,3.60367725300248e+01,
     &  6.20038380071273e+01 /
c
      data qcoe / 1.00000000000000e+00,-8.90601665949746e+00,
     &  9.82252110471399e+00,6.20038380071270e+01 /
c
      data (gln(i),i=1,60) / 2*0.,6.93147180559945e-01,1.79175946922806e
     &  +00,3.17805383034795e+00,4.78749174278205e+00,6.57925121201010e+
     &  00,8.52516136106541e+00,1.06046029027453e+01,1.28018274800815e+
     &  01,1.51044125730755e+01,1.75023078458739e+01,1.99872144956619e+
     &  01,2.25521638531234e+01,2.51912211827387e+01,2.78992713838409e+
     &  01,3.06718601060807e+01,3.35050734501369e+01,3.63954452080331e+
     &  01,3.93398841871995e+01,4.23356164607535e+01,4.53801388984769e+
     &  01,4.84711813518352e+01,5.16066755677644e+01,5.47847293981123e+
     &  01,5.80036052229805e+01,6.12617017610020e+01,6.45575386270063e+
     &  01,6.78897431371815e+01,7.12570389671680e+01,7.46582363488302e+
     &  01,7.80922235533153e+01,8.15579594561150e+01,8.50544670175815e+
     &  01,8.85808275421977e+01,9.21361756036871e+01,9.57196945421432e+
     &  01,9.93306124547874e+01,1.02968198614514e+02,1.06631760260643e+
     &  02,1.10320639714757e+02,1.14034211781462e+02,1.17771881399745e+
     &  02,1.21533081515439e+02,1.25317271149357e+02,1.29123933639127e+
     &  02,1.32952575035616e+02,1.36802722637326e+02,1.40673923648234e+
     &  02,1.44565743946345e+02,1.48477766951773e+02,1.52409592584497e+
     &  02,1.56360836303079e+02,1.60331128216631e+02,1.64320112263195e+
     &  02,1.68327445448428e+02,1.72352797139163e+02,1.76395848406997e+
     &  02,1.80456291417544e+02,1.84533828861449e+02 /
      data (gln(i),i=61,100) / 1.88628173423672e+02,1.92739047287845e+02
     &  ,1.96866181672890e+02,2.01009316399282e+02,2.05168199482641e+02,
     &  2.09342586752537e+02,2.13532241494563e+02,2.17736934113954e+02,
     &  2.21956441819130e+02,2.26190548323728e+02,2.30439043565777e+02,
     &  2.34701723442818e+02,2.38978389561834e+02,2.43268849002983e+02,
     &  2.47572914096187e+02,2.51890402209723e+02,2.56221135550010e+02,
     &  2.60564940971863e+02,2.64921649798553e+02,2.69291097651020e+02,
     &  2.73673124285694e+02,2.78067573440366e+02,2.82474292687630e+02,
     &  2.86893133295427e+02,2.91323950094270e+02,2.95766601350761e+02,
     &  3.00220948647014e+02,3.04686856765669e+02,3.09164193580147e+02,
     &  3.13652829949879e+02,3.18152639620209e+02,3.22663499126726e+02,
     &  3.27185287703775e+02,3.31717887196928e+02,3.36261181979198e+02,
     &  3.40815058870799e+02,3.45379407062267e+02,3.49954118040770e+02,
     &  3.54539085519441e+02,3.59134205369575e+02 /
c
      if (x.le.0.0)then
        gamln1=0
        write(*,*)' '
        write(*,*)'GAMLN1 - Fatal error!'
        write(*,*)'  Illegal value of X <= 0 =',x
        return
      endif
c
      ndx=x
      t=x-float(ndx)
      if (t.eq.0.0) go to 90
      dx=xlim1-x
      if (dx.lt.0.0) go to 60
c
c     RATIONAL CHEBYSHEV APPROXIMATION ON 2.LT.X.LT.3 FOR GAMMA(X)
c
      nxm=ndx-2
      px=pcoe(1)
      
      do i=2, 9
        px=t*px+pcoe(i)
      enddo
   
      qx=qcoe(1)
      
      do i=2, 4
        qx=t*qx+qcoe(i)
      enddo
      
      dgam=px/qx
      if (nxm.gt.0) go to 30
      if (nxm.eq.0) go to 50
c
c     BACKWARD RECURSION FOR 0.LT.X.LT.2
c
      dgam=dgam/(1.+t)
      if (nxm.eq.-1) go to 50
      dgam=dgam/t
      gamln1=log(dgam)
      return
c
c     FORWARD RECURSION FOR 3.LT.X.LT.8
c
   30 xx=2.+t
   
      do i=1, nxm
        dgam=dgam*xx
        xx=xx+1.0
      enddo
      
   50 gamln1=log(dgam)
      return
c
c     X.GT.XLIM1
c
   60 rx=1.0/x
      rxx=rx*rx
      if ((x-xlim2).lt.0.0) go to 70
      px=q(1)*rxx+q(2)
      gamln1=px*rx+(x-.5)*log(x)-x+rtwpil
      return
c
c     X.LT.XLIM2
c
   70 px=p(1)
      sum=(x-0.5)*log(x)-x
      
      do i=2,5
        px=px*rxx+p(i)
      enddo
   
      gamln1=px*rx+sum+rtwpil
      return
c
c     TABLE LOOK UP FOR INTEGER ARGUMENTS LESS THAN OR EQUAL 100.
c
   90 if (ndx.gt.100) go to 60
      gamln1=gln(ndx)

      return
      end
      subroutine gamtl(x,b,rel,n,y,nz)
c
c***********************************************************************
c
c     WRITTEN BY D.E. AMOS AND S.L. DANIEL, OCTOBER,1974
c
c     REFERENCE SC-DR-72 0303
c
c     ABSTRACT
c         GAMTL COMPUTES AN N MEMBER SEQUENCE OF COMPLEMENTARY
c         GAMMA FUNCTIONS
c
c         Y(K)=1.-INCGAMMA(B+K-1,X)/GAMMA(B+K-1), K=1,...,N,
c
c         TO A RELATIVE ERROR REL FOR X.GE.0 AND B.GT.0. THE CONTINUED
c         FRACTION IS EVALUATED FOR BO.GT.0., BO=B-INTEGER PART OF
c         B, FOLLOWED BY FORWARD RECURSION ON ITS TWO TERM RELATION TO
c         RAISE BO TO B+N-1.  THE CONVERGENCE IS BEST FOR LARGE
c         X .GE. MAX(1,BO). WHERE SPEED IS A CONSIDERATION, EVALUATE
c         Y(K) BY SUBTRACTING THE INCOMPLETE GAMMA FUNCTION FROM 1.
c         USING SUBROUTINE GAMIC FOR X.LT.MAX(1,B+K-1) AND GAMTL FOR
c         X.GE.MAX(1,B+K-1).
c
c     DESCRIPTION OF ARGUMENTS
c
c         INPUT
c           X      - ARGUMENT, X.GE.0.0
c           B      - PARAMETER, B.GT.0.0
c           REL    - RELATIVE ERROR REQUIREMENT,
c                    REL=1.E-S FOR S SIGNIFICANT DIGITS, 0.LE.S.LE.12
c           N      - NUMBER OF COMPLEMENTARY FUNCTIONS IN THE SEQUENCE
c                    BEGINNING AT PARAMETER B, N.GE.1
c
c         OUTPUT
c           Y      - A VECTOR CONTAINING AN N MEMBER SEQUENCE
c                    Y(K)=1.-INCGAMMA(B+K-1,X)/GAMMA(B+K-1), K=1,...,N
c                    TO A RELATIVE ERROR REL.
c           NZ     - UNDERFLOW FLAG
c                    NZ.EQ.0, A NORMAL RETURN
c                    NZ.NE.0, UNDERFLOW, Y(K)=0.0, K=1,NZ RETURNED.
c
c     ERROR CONDITIONS
c         IMPROPER INPUT - A FATAL ERROR
c         CONTINUED FRACTION DOES NOT CONVERGE - A FATAL ERROR.
c         UNDERFLOW - A NON-FATAL ERROR.
c
      real a250
      real aa(6)
      real aj
      real b
      real bb(6)
      real bo
      real bpn
      real bptest
      real cc(6)
      real dd(5)
      real elim
      real eterm
      real gamln1
      real garg
      real gboplx
      real gbx
      integer i
      integer ik
      integer j
      integer k
      integer kend
      integer kflag
      integer maxr
      integer n
      integer nb
      integer nbar
      integer nz
      real r1
      real r1mach
      real rel
      real rn
      real rnp1
      real rsave
      real rtest
      real s1
      real s2
      real scale
      real sm250
      real sn
      real sp250
      real x
      real xlim
      real y(1)
c
      data aa / 1.68859588328389e+00,1.46584889514920e+03,
     &  1.73155244984160e+05,2.30183328529532e+06,2.27877181874608e+02,
     &  3.47933198737331e+03 /
      data bb / 1.25558472503917e+00,4.44539043255409e+03,
     &  2.40212910342450e+06,2.22905539488737e+08,1.84918400518640e+03,
     &  3.17776548571970e+05 /
      data cc / 1.09010860762347e+00,2.03790209154944e+04,
     &  4.60641404764333e+07,1.36803284952888e+10,1.39702132796605e+04,
     &  1.59209661717833e+07 /
      data dd / 4.68251145610753e+05,1.11348212926478e+10,
     &  1.38713354995651e+13,4.29493174834810e+05,7.88439389479852e+09 /
      data maxr,a250 / 1280,5.75646273248511e+2 /
      data elim / 667. /
c
      sp250=sqrt(r1mach(2))
      sm250=1.0/sp250
      
      if (n.le.0)then
        write(*,*)' '
        write(*,*)'GAMTL - Fatal error!'
        write(*,*)'  Illegal value of N <= 0 =',n
        return
      endif
c
      if (b.le.0.0)then
        write(*,*)' '
        write(*,*)'GAMTL - Fatal error!'
        write(*,*)'  Illegal value of B <=0 =',b
        return
      endif
c
      if (x.lt.0.0)then
        write(*,*)' '
        write(*,*)'GAMTL - Fatal error!'
        write(*,*)'  Illegal value of X < 0 =',x
        return
      endif
c
      nz=0
      if (x.eq.0.0)then
      
        do j=1, n
          y(j)=1.0
        enddo
        
        return
      endif
c
      if (rel.lt.r1mach(4))then
        write(*,*)' '
        write(*,*)'GAMTL - Fatal error!'
        write(*,*)'  Improper value of REL < R1MACH(4).'
        write(*,*)'  REL=',rel
        write(*,*)'  R1MACH(4)=',r1mach(4)
        return
      endif
c
c  IF X.GE.XLIM(B+N-1), Y(K)=0., K=1,2,...,N DUE TO UNDERFLOW
c
      bpn=b+float(n)-1.0
      bptest=bpn
      if (bpn.le.1.0) go to 80
      if (x.le.bptest) go to 90
      if (bptest.gt.200.0) go to 20
      s1=((aa(1)*bptest+aa(2))*bptest+aa(3))*bptest+aa(4)
      s2=(bptest+aa(5))*bptest+aa(6)
      go to 50

   20 if (bptest.gt.1000.0) go to 30
      s1=((bb(1)*bptest+bb(2))*bptest+bb(3))*bptest+bb(4)
      s2=(bptest+bb(5))*bptest+bb(6)
      go to 50

   30 if (bptest.gt.10000.0) go to 40
      s1=((cc(1)*bptest+cc(2))*bptest+cc(3))*bptest+cc(4)
      s2=(bptest+cc(5))*bptest+cc(6)
      go to 50

   40 s1=((bptest+dd(1))*bptest+dd(2))*bptest+dd(3)
      s2=(bptest+dd(4))*bptest+dd(5)

   50 xlim=s1/s2
      if (x.ge.xlim) go to 60
      go to 90

   60 do i=1,n
        y(i)=0.0
      enddo

      nz=n
      return

   80 if (x.lt.elim) go to 90
      go to 60

   90 continue
      nb=b
      bo=b-float(nb)
      if (bo.gt.0.0) go to 100
      bo=1.0
      nb=nb-1
      gboplx=0.0
      scale=0.0
      r1=x
      gbx=1.0
      go to 150
c
c     COMPUTE BIGGAM (BO,X) BY CONTINUED FRACTION
c
  100 kflag=0
      nbar=int(10.0+(1.0)/(x+0.01))

  110 continue

      if (nbar.gt.maxr)then
        write(*,*)' '
        write(*,*)'GAMTL - Warning!'
        write(*,*)'  Nonconvergence, try computing 1-IncGAMMA!'
        return
      endif

      rnp1=0.0
      sn=nbar+1
      do j=1,nbar
        sn=sn-1.0
        rn=x+(sn-bo)*rnp1/(sn+rnp1)
        rnp1=rn
      enddo

      if (kflag.ne.0) go to 130
      kflag=1
      nbar=nbar+nbar
      rsave=rn
      go to 110

  130 rtest=abs((rsave-rn)/rn)
      if (rtest.le.rel) go to 140
      rsave=rn
      nbar=nbar+nbar
      go to 110

  140 continue

      gboplx=bo*log(x)-gamln1(bo)
      scale=0.
      r1=1.0/bo
      gbx=1.0/rn

  150 aj=bo
      eterm=-x+gboplx
      if (nb.le.0) go to 180
c
c  RAISE PARAMETER FROM BO TO B BY RECURSION
c  IF TERM.GT.SP250 OR .LT.SM250, SCALE AND CONTINUE
c
  160 continue

      do k=1,nb
        gbx=gbx+r1
        aj=aj+1.0
        r1=r1*x/aj

        if (r1.gt.sp250)then
          r1=r1*sm250
          gbx=gbx*sm250
          scale=scale+a250
        elseif(r1.lt.sm250)then
          r1=r1*sp250
          gbx=gbx*sp250
          scale=scale-a250
        endif

      enddo

  180 continue

      garg=eterm+log(gbx)+scale
      if (garg.gt.-elim) go to 190
c
c     UNDERFLOW VALUES SET TO ZERO
c
      nz=nz+1
      y(nz)=0.0
      if (nz.eq.n) return
      nb=1
      go to 160
c
c     COMPUTATION OF Y(K) BY RECURSION
c
  190 ik=nz+1
      y(ik)=exp(garg)
      if (n.eq.ik) return
      if (r1.le.0.0) go to 220
      r1=exp(eterm+log(r1)+scale)

  200 kend=n-1

      do k=ik,kend
        y(k+1)=y(k)+r1
        aj=aj+1.0
        r1=r1*x/aj
      enddo

      return

  220 r1=0.0
      go to 200

      end
      function gamln2(xx)
c
c***********************************************************************
c
c  Compute logarithm of gamma function.
c
      real cof(6)
      real gamln2
      integer j
      real ser
      real tmp
      real x
      real xx
c
      cof(1)=76.18009173
      cof(2)=-86.50532033
      cof(3)=24.01409822
      cof(4)=-1.231739516
      cof(5)=0.120858003e-2
      cof(6)=-0.536382e-5
c
      x=xx-1.0
      tmp=x+5.5
      tmp=(x+0.5)*log(tmp)-tmp
      ser=1.0

      do j=1, 6
        x=x+1.0
        ser=ser+cof(j)/x
      enddo

      gamln2=tmp+log(2.50662827465*ser)
      return
      end
      function gamln3(z)
c
c***********************************************************************
c
c  Uses Lanczos-type approximation to ln(gamma) for z > 0.
c
c  This algorithm gives 14 or more significant decimal digits accuracy,
c  around X=1 and X = 2.   The Lanczos series from which this algorith
c  derived is interesting in that it is a convergent series approximatio
c  the gamma function, whereas the familiar series due to De Moivre (and
c  wrongly called Stirling's approximation) is only an asymptotic approx
c  as is the true and preferable approximation due to Stirling.
c
c  Reference:
c
c  Lanczos, C.
c  'A precision approximation of the gamma function',
c  J. SIAM Numer. Anal., B, 1, 86-96, 1964.
c
c  Accuracy:
c  About 14 significant digits except for small regions in the vicinity
c  and 2.
c
c  Programmer:
c  Alan Miller
c  CSIRO Division of Mathematics & Statistics
c
c  Latest revision - 17 April 1988
c
      real a(9)
      real alstpi
      real gamln3
      integer j
      real tmp
      real z
c
      data a / 0.9999999999995183,676.5203681218835,-1259.139216722289,
     &  771.3234287757674,-176.6150291498386,12.50734324009056,-
     &  0.1385710331296526,0.9934937113930748e-05,0.1659470187408462e-06
     &   /
c
c  LOG(SQRT(2*PI))
c
      alstpi=0.9189385332046727
c
      gamln3=0.0

      if (z.le.0.0)then
        write(*,*)' '
        write(*,*)'GAMLN3 - Fatal error!'
        write(*,*)'  Illegal input value Z <=0=',z
        return
      endif
c
      gamln3=0.0
      tmp=z+7.0

      do j=9, 2, -1
        gamln3=gamln3+a(j)/tmp
        tmp=tmp-1.0
      enddo

      gamln3=gamln3+a(1)
      gamln3=log(gamln3)+alstpi-(z+6.5)+(z-0.5)*log(z+6.5)

      return
      end
      function gamln4(x,ifault)
c
c***********************************************************************
c
c  Algorithm ACM 291, Communications of the ACM, 1966, Volume 9, page 68
c
c  Evaluates the natural logarithm of GAMMA(X) for X greater than zero.
c
c  X      Input, REAL X, the point at which LOG(GAMMA(X)) is to be
c         evaluated.
c
c  IFAULT Output, INTEGER IFAULT, fault indicator.
c         0 for no fault.
c         1 if X<=0.
c
c  GAMLN4 Output, REAL GAMLN4, LOG(GAMMA(X)).
c
      real a1
      real a2
      real a3
      real a4
      real a5
      real f
      real gamln4
      integer ifault
      real x
      real y
      real z
c
      gamln4=0.0

      a1=0.918938533204673
      a2=0.000595238095238
      a3=0.000793650793651
      a4=0.002777777777778
      a5=0.083333333333333

      if (x.le.0.0)then
        ifault=1
        return
      endif

      y=x
      f=0.0

      if (y.lt.7.0)then

        f=y

   10   continue

        y=y+1.0

        if (y.lt.7.0)then
          f=f*y
          go to 10
        endif

        f=-log(f)

      endif

      z=1.0/(y*y)
      gamln4=f+(y-0.5)*log(y)-y+a1+(((-a2*z+a3)*z-a4)*z+a5)/y

      return
      end
      function gau1(gmean,xvar,iseed)
c
c***********************************************************************
c
c  GAU1 generates an approximately normal random deviate with mean GMEAN 
c  and variance XVAR.  XVAR must be strictly positive.  The algorithm 
c  used involves adding 12 uniformly random numbers.  
c
c
c  Input, REAL GMEAN, the median of the distribution.
c 
c  Input, REAL XVAR, the variance of the distribution.  XVAR must 
c  be positive.
c 
c  Input/output, INTEGER ISEED, used as a seed for the random 
c  number generator.
c
c  Output, REAL GAU1, a random gaussian variable.
c 
      integer n
      parameter (n=12)
c
      real gau1
      real gmean
      integer i
      integer iseed
      real random
      real x
      real xvar
c
      external random
      intrinsic sqrt
c
      if (xvar.le.0.0)then
        write(*,*)' '
        write(*,*)'GAU1 - Fatal error!'
        write(*,*)'  Illegal input, negative variance XVAR=',xvar
        gau1=gmean
        return
      endif
c
      x=-float(n)/2.0
      do i=1,n
        x=x+random(iseed)
      enddo

      gau1=gmean+x*sqrt(xvar)

      return
      end
      function gau2(gmean,sd,iseed)
c
c***********************************************************************
c
c  GAU2 generates an approximately normally distributed pseudorandom 
c  number.  
c
c  These random numbers are not exceptionally good - especially in 
c  the tails of the distribution, but this implementation is simple 
c  and suitable for most applications.
c
c  GAU2 calls RANDOM(ISEED).
c 
c  GAU2 was extracted from SLATEC.
c 
c  Author: Wayne Fullerton, Los Alamos National Laboratory.
c 
c  Reference:
c 
c  R. W. Hamming,
c  Numerical Methods for Scientists and Engineers,
c  McGraw-Hill, 1962, pages 34 and 389.
c 
c
c  Input, REAL GMEAN, the mean of the Gaussian distribution.
c  This is roughly the average value of the numbers to be generated.       
c 
c  Input, REAL SD, the standard deviation of the Gaussian function
c    EXP (-1/2 * (X-GMEAN)**2 / SD**2)
c  This is controls how far from the average value the numbers
c  are likely to deviate.
c
c  Input/output, INTEGER ISEED, used as a seed for the uniform
c  random number generator.
c
c  Output, REAL GAU2, the normally distributed random number.
c
      integer nval
      parameter (nval=12)
c 
      real gau2
      real gmean
      integer i
      integer iseed
      real random
      real sd
c
c  Add NVAL uniform random numbers between 0 and 1, and subtract
c  NVAL/2, to get a value that has average 0.
c
      gau2=-real(nval)/2.0

      do i=1,nval
        gau2=gau2+random(iseed)
      enddo
c
c  Shift and stretch the original value to get a new value that
c  has the appropriate mean and standard deviation.
c
      gau2=gmean+sd*gau2

      return
      end
      function gau3(iseed)
c
c***********************************************************************
c
c  GAU3 generates a normally distributed random number.
c  GAU3 does not use an approximate rule to compute Gaussian numbers.
c  Instead it uses the Box-Muller transformation on two uniform random
c  numbers to compute two normally distributed ones.  It returns one
c  on the first call, returns the other on the next, and on the third
c  call again computes two more values, returning one and holding the
c  other in reserve.  
c
c  GAU3 was extracted from Numerical Recipes.  
c
c  GAU3 calls RANDOM(ISEED).  
c
c  This version tries to avoid the trigonometric calls required by a 
c  standard Box-Muller transformation.
c 
c
c  Input/output, INTEGER ISEED, used as a seed for the random 
c  number generator.
c 
c  Output, REAL GAU3, a normally distributed gaussian random 
c  number, with zero mean and unit variance.
c 
      real fac
      real gau3
      real gset
      integer ifail
      integer iseed
      integer iset
      real r
      real random
      real v1
      real v2
c
      save gset
      save iset
c
      data iset / 0 /
c
      if (iset.eq.0)then

        ifail=0

   10   continue

        v1=2.0*random(iseed)-1.0
        v2=2.0*random(iseed)-1.0

        r=v1**2+v2**2

        if (r.ge.1.0.or.r.le.0.0)then

          ifail=ifail+1

          if (ifail.lt.10)then
            go to 10
          else
            write(*,*)' '
            write(*,*)'GAU3 - Fatal error!'
            write(*,*)'  Repeated computation of zero!'
            stop
          endif

        endif

        fac=sqrt(-2.0*log(r)/r)
        gset=v1*fac
        gau3=v2*fac
        iset=1

      else

        gau3=gset
        iset=0

      endif

      return
      end
      function gau4(gmean,sd,iseed)
c
c***********************************************************************
c
c  GAU4 uses the Box-Muller transformation on two uniform random
c  numbers to compute two normally distributed ones.  It returns one
c  on the first call, returns the other on the next, and on the third
c  call again computes two more values, returning one and holding the
c  other in reserve.  
c
c  GAU4 calls RANDOM(ISEED).
c
c  This function is a straightforward application of Box-Muller,
c  including calls to SINE and COSINE, which may slow it down in
c  comparison to GAU3.
c 
c
c  Input, REAL GMEAN, the mean of the Gaussian distribution.
c 
c  Input, REAL SD, the standard deviation of the Gaussian function
c  EXP (-1/2 * (X-GMEAN)**2 / SD**2)
c
c  Input/output, INTEGER ISEED, used as a seed for the random 
c  number generator.
c 
c  Output, REAL GAU4, a normally distributed gaussian random number.
c 
      real pi
      parameter (pi=3.14159265358979323846)
c
      real gau4
      real gmean
      real gset
      integer iseed
      integer iset
      real random
      real sd
      real v1
      real v2
c
      save gset
      save iset
c
      data gset /0.0/
      data iset / 0 /
c
      if (iset.eq.0)then

        v1=random(iseed)

        if (v1.le.0.0)then
          write(*,*)' '
          write(*,*)'GAU4 - Fatal error!'
          write(*,*)'  V1 <=0, V1=',v1
          write(*,*)'  ISEED=',iseed
        endif
        
        v2=random(iseed)

        if (v2.le.0.0)then
          write(*,*)' '
          write(*,*)'GAU4 - Fatal error!'
          write(*,*)'  V2 <=0, V2=',v2
          write(*,*)'  ISEED=',iseed
        endif

        if (v1.gt.0.0.and.v2.gt.0.0)then
          gau4=gmean+sd*sqrt(-2.0*log(v1))*cos(2.0*pi*v2)
          gset=gmean+sd*sqrt(-2.0*log(v1))*sin(2.0*pi*v2)
        else
          gau4=0.0
          gset=0.0
        endif

        iset=1
c
c  On even calls, simply use the value in GSET.
c
      else

        gau4=gset
        iset=0

      endif

      return
      end
      subroutine gau5(vector,length)
c
c***********************************************************************
c
c  GAU5 is a buffered, vectorized routine for computing normal values.
c  The program can return any number of values, but computes them in
c  groups of 1024 at a time.  The speed is significantly greater than
c  for the scalar codes GAU1, GAU2, GAU3, and GAU4 on the Cray.
c  The Box-Muller method is used.
c
c  GAU5 calls RANF() for uniform random values.  Hence, for repeatable 
c  results, call RANSET(ISEED) first.
c 
c
c  Output, REAL VECTOR(LENGTH), the normal values requested.
c  They have zero mean and unit variance.
c 
c  Input, INTEGER LENGTH, the number of values requested.
c 
      integer nbuff
      parameter (nbuff=1024)
c
      integer nbuffh
      parameter (nbuffh=nbuff/2)
c
      real pi
      parameter (pi=3.14159265358979323846)
c
      real buffer(nbuff)
      integer i
      integer ido
      integer ifirst
      integer ihave
      integer ineed
      integer length
      real ranf
      real v1(nbuffh)
      real v2(nbuffh)
      real vector(length)
c
      save buffer
      save ifirst
      save v1
      save v2
c
      data ifirst / 0 /
c
c  Copy current buffer contents into vector
c
      ihave=0
      ineed=length
      if (ifirst.eq.0) go to 30

   10 continue

      ido=min(ineed-ihave,nbuff+1-ifirst)

      do i=1, ido
        vector(i+ihave)=buffer(i+ifirst-1)
      enddo

      ifirst=ido+ifirst
      ihave=ihave+ido
      if (ihave.eq.length) return
c
c  If not done, buffer is exhausted.  Refresh it.
c
   30 continue

      do i=1, nbuffh
        v1(i)=ranf()
      enddo

      do i=1, nbuffh
        v2(i)=ranf()
      enddo

      do i=1, nbuffh
        buffer(2*i-1)=sqrt(-2.0*log(v1(i)))*cos(2.0*pi*v2(i))
        buffer(2*i)=sqrt(-2.0*log(v1(i)))*sin(2.0*pi*v2(i))
      enddo

      ifirst=1

      go to 10
      end
      subroutine gau6(vector,length)
c
c***********************************************************************
c
c  GAU6 is a buffered, vectorized routine for computing normal values.
c  The program can return any number of values, but computes them in
c  groups of 1024 at a time.  The speed is significantly greater than
c  for the scalar codes GAU1, GAU2, GAU3, and GAU4 on the Cray.
c  The approximate method used by GAU1 and GAU2 is used by GAU6.  This
c  routine is somewhat slower than GAU5, and the results are not
c  truly normal random values.
c 
c  GAU6 calls RANF() for uniform random values.  Hence, for repeatable 
c  results, call RANSET(ISEED) first.
c 
c  VECTOR Output, REAL VECTOR(LENGTH), the normal values requested.
c         They have zero mean and unit variance.
c 
c  LENGTH Input, INTEGER LENGTH, the number of values requested.
c 
      integer nbuff
      parameter (nbuff=1024)
c
      real buffer(nbuff)
      integer i
      integer ido
      integer ifirst
      integer ihave
      integer ineed
      integer j
      integer length
      real ranf
      real vector(length)
c
      save buffer
      save ifirst
c
      data ifirst / 0 /
c
c  Copy current buffer contents into vector
c
      ihave=0
      ineed=length
      if (ifirst.eq.0) go to 30

   10 continue

      ido=min(ineed-ihave,nbuff+1-ifirst)

      do i=1, ido
        vector(i+ihave)=buffer(i+ifirst-1)
      enddo

      ifirst=ido+ifirst
      ihave=ihave+ido
      if (ihave.eq.length) return
c
c  If not done, buffer is exhausted.  Refresh it.
c
   30 continue

      do j=1, nbuff
        buffer(j)=-6.0
      enddo

      do i=1, 12
        do j=1, nbuff
          buffer(j)=buffer(j)+ranf()
        enddo
      enddo

      ifirst=1
      go to 10
      end
      function gau7(iseed)
c
c***********************************************************************
c
c  GAU7 generates quasi normal random numbers, with zero mean and unit
c  standard deviation, and can be used on any computer with integers at
c  least as large as 32767.  It uses a sort of interpolation scheme.
c 
c  GAU7 was extracted from STARPAC.
c
c  GAU7 calls RANDOM(ISEED).
c
c  Reference:
c 
c  Marsaglia and Tsang,
c  A fast, easily implemented method for sampling from decreasing 
c  or symmetric unimodal density functions,
c  SIAM J SISC 1983
c 
c  ISEED  Input/output, INTEGER ISEED, on first call, set ISEED to 
c         a nonzero value to initialize the sequence.  ISEED will be 
c         set to zero before return.  On every call with ISEED zero, the 
c         next element of the sequence will be returned.
c 
c  GAU7   Output, REAL GAU7, a normally distributed random number. 
c
      real aa
      real b
      real c
      real c1
      real c2
      real gau7
      integer i
      integer i1
      integer i1mach
      logical init
      integer iseed
      integer j
      integer j0
      integer j1
      integer jseed
      integer k0
      integer k1
      integer m(17)
      integer m1
      integer m2
      integer mdig
      real pc
      real random
      real rmax
      real s
      real v(65)
      real w(65)
      real x
      real xn
      real y
c
      external i1mach
c
      save i1
      save init
      save j1
      save m
      save m1
      save m2
      save rmax
c
      data aa,b,c,rmax / 12.37586,.4878992,12.67706,3.0518509e-5 /
      data c1,c2,pc,xn / .9689279,1.301198,.1958303e-1,2.776994 /
c
      data v / .3409450,.4573146,.5397793,.6062427,.6631691,.7136975,.
     &  7596125,.8020356,.8417227,.8792102,.9148948,.9490791,.9820005,
     &  1.0138492,1.0447810,1.0749254,1.1043917,1.1332738,1.1616530,
     &  1.1896010,1.2171815,1.2444516,1.2714635,1.2982650,1.3249008,
     &  1.3514125,1.3778399,1.4042211,1.4305929,1.4569915,1.4834526,
     &  1.5100121,1.5367061,1.5635712,1.5906454,1.6179680,1.6455802,
     &  1.6735255,1.7018503,1.7306045,1.7598422,1.7896223,1.8200099,
     &  1.8510770,1.8829044,1.9155830,1.9492166,1.9839239,2.0198430,
     &  2.0571356,2.0959930,2.1366450,2.1793713,2.2245175,2.2725185,
     &  2.3239338,2.3795007,2.4402218,2.5075117,2.5834658,2.6713916,
     &  2.7769943,2.7769943,2.7769943,2.7769943 /
c
      data w / .10405134e-04,.13956560e-04,.16473259e-04,.18501623e-04,
     &  .20238931e-04,.21780983e-04,.23182241e-04,.24476931e-04,.
     &  25688121e-04,.26832186e-04,.27921226e-04,.28964480e-04,.
     &  29969191e-04,.30941168e-04,.31885160e-04,.32805121e-04,.
     &  33704388e-04,.34585827e-04,.35451919e-04,.36304851e-04,.
     &  37146564e-04,.37978808e-04,.38803170e-04,.39621114e-04,.
     &  40433997e-04,.41243096e-04,.42049621e-04,.42854734e-04,.
     &  43659562e-04,.44465208e-04,.45272764e-04,.46083321e-04,.
     &  46897980e-04,.47717864e-04,.48544128e-04,.49377973e-04,.
     &  50220656e-04,.51073504e-04,.51937936e-04,.52815471e-04,.
     &  53707761e-04,.54616606e-04,.55543990e-04,.56492112e-04,.
     &  57463436e-04,.58460740e-04,.59487185e-04,.60546402e-04,.
     &  61642600e-04,.62780711e-04,.63966581e-04,.65207221e-04,.
     &  66511165e-04,.67888959e-04,.69353880e-04,.70922996e-04,.
     &  72618816e-04,.74471933e-04,.76525519e-04,.78843526e-04,.
     &  81526890e-04,.84749727e-04,.84749727e-04,.84749727e-04,.
     &  84749727e-04 /

      data m(1),m(2),m(3),m(4),m(5),m(6),m(7),m(8),m(9),m(10),m(11),m(12
     &  ),m(13),m(14),m(15),m(16),m(17) / 30788,23052,2053,19346,10646,
     &  19427,23975,19049,10949,19693,29746,26748,2796,23890,29168,31924
     &  ,16499 /

      data m1,m2,i1,j1 / 32767,256,5,17 /

      data init /.false./
c
c FAST PART...
c
c***FIRST EXECUTABLE STATEMENT  GAU7
c
      if (iseed.ne.0.and.init) go to 40

      init=.true.

   10 continue

      i=m(i1)-m(j1)
      if (i.lt.0) i=i+m1
      m(j1)=i
      i1=i1-1
      if (i1.eq.0) i1=17
      j1=j1-1
      if (j1.eq.0) j1=17
      j=mod(i,64)+1
      gau7=i*w(j+1)
      if (((i/m2)/2)*2.eq.(i/m2)) gau7=-gau7
      if (abs(gau7).le.v(j)) return
c
c  SLOW PART; AA IS A*F(0)
c
      x=(abs(gau7)-v(j))/(v(j+1)-v(j))
      iseed=0
      y=random(iseed)
      s=x+y

      if (s.gt.c2)then
        gau7=sign(b-b*x,gau7)
        return
      endif

      if (s.le.c1)return

      if (y.gt.c-aa*exp(-.5*(b-b*x)**2))then
        gau7=sign(b-b*x,gau7)
        return
      endif

      if (exp(-.5*v(j+1)**2)+y*pc/v(j+1).le.exp(-.5*gau7**2)) return
c
c  TAIL PART; 3.855849 IS .5*XN**2
c
   20 continue

      iseed=0
      s=xn-log(random(iseed))/xn
      if (3.855849+log(random(iseed))-xn*s.gt.-.5*s**2) go to 20
      gau7=sign(s,gau7)
      return
c
c  FILL
c
   40 continue

      mdig=i1mach(8)+1
      mdig=min(mdig,16)
c
c  BE SURE THAT MDIG AT LEAST 16...
c
      if (mdig.lt.16)then
        write(*,*)' '
        write(*,*)'GAU7 - Fatal error!'
        write(*,*)'  MDIG less than 16.'
        write(*,*)'  MDIG=',mdig
        stop
      endif

      m1=2**(mdig-2)+(2**(mdig-2)-1)
      m2=2**(mdig/2)
      jseed=min(iabs(iseed),m1)
      if (mod(jseed,2).eq.0) jseed=jseed-1
      k0=mod(9069,m2)
      k1=9069/m2
      j0=mod(jseed,m2)
      j1=jseed/m2

      do i=1,17
        jseed=j0*k0
        j1=mod(jseed/m2+j0*k1+j1*k0,m2/2)
        j0=mod(jseed,m2)
        m(i)=j0+m2*j1
      enddo

      j1=17
      i1=5
      rmax=1.0/real(m1)
c
c  SEED UNIFORM (0,1) GENERATOR.  (JUST A DUMMY CALL)
c
      gau7=random(iseed)

      do i=1,65
        w(i)=rmax*v(i)
      enddo

      go to 10
      end
      function gau8(iseed)
c
c***********************************************************************
c
c  GAU8 generates quasi normal random numbers, with zero mean and unit
c  standard deviation.
c
c  GAU8 was extracted from ACM Algorithm 599.
c
c  GAU8 calls RANDOM(ISEED) for uniform random numbers.
c 
c  Reference:
c 
c  J H Ahrens and U Dieter,
c  Extensions of Forsythe's Method for Random Sampling from the 
c  Normal Distribution
c  Math. Comput., 
c  Volume 27, Number 124, October 1973, pages 927-937.
c
c  ISEED  Input/output, INTEGER ISEED, used as a seed for the random 
c         number generator.
c 
c  GAU8   Output, REAL GAU8, a normally distributed random number.
c 
      real a(32)
      real aa
      real d(31)
      real gau8
      real h(31)
      integer i
      integer iseed
      real random
      real s
      real t(31)
      real tt
      real u
      real ustar
      real w
      real y
c
c     the definitions of the constants a(k), d(k), t(k) and
c     h(k) are according to the abovementioned article
c
      data a / 0.0,.3917609e-1,.7841241e-1,.1177699,.1573107,.1970991,.
     &  2372021,.2776904,.3186394,.3601299,.4022501,.4450965,.4887764,.
     &  5334097,.5791322,.6260990,.6744898,.7245144,.7764218,.8305109,.
     &  8871466,.9467818,1.009990,1.077516,1.150349,1.229859,1.318011,
     &  1.417797,1.534121,1.675940,1.862732,2.153875 /

      data d / 5*0.0,.2636843,.2425085,.2255674,.2116342,.1999243,.
     &  1899108,.1812252,.1736014,.1668419,.1607967,.1553497,.1504094,.
     &  1459026,.1417700,.1379632,.1344418,.1311722,.1281260,.1252791,.
     &  1226109,.1201036,.1177417,.1155119,.1134023,.1114027,.1095039 /

      data t / .7673828e-3,.2306870e-2,.3860618e-2,.5438454e-2,.7050699e
     &  -2,.8708396e-2,.1042357e-1,.1220953e-1,.1408125e-1,.1605579e-1,
     &  .1815290e-1,.2039573e-1,.2281177e-1,.2543407e-1,.2830296e-1,.
     &  3146822e-1,.3499233e-1,.3895483e-1,.4345878e-1,.4864035e-1,.
     &  5468334e-1,.6184222e-1,.7047983e-1,.8113195e-1,.9462444e-1,.
     &  1123001,.1364980,.1716886,.2276241,.3304980,.5847031 /

      data h / .3920617e-1,.3932705e-1,.3950999e-1,.3975703e-1,.4007093e
     &  -1,.4045533e-1,.4091481e-1,.4145507e-1,.4208311e-1,.4280748e-1,
     &  .4363863e-1,.4458932e-1,.4567523e-1,.4691571e-1,.4833487e-1,.
     &  4996298e-1,.5183859e-1,.5401138e-1,.5654656e-1,.5953130e-1,.
     &  6308489e-1,.6737503e-1,.7264544e-1,.7926471e-1,.8781922e-1,.
     &  9930398e-1,.1155599,.1404344,.1836142,.2790016,.7010474 /
c
      u=random(iseed)

      if (u.lt.0.5)then
        s=0.0
      else
        s=1.0
      endif

      u=u+u-s
      u=32.0*u
      i=int(u)
      if (i.le.0.or.i.gt.32)go to 60
c
c  start center
c
      ustar=u-float(i)
      aa=a(i)

   10 continue

      if (ustar.le.t(i)) go to 30
      w=(ustar-t(i))*h(i)
c
c  exit   (both cases)
c
   20 continue

      y=aa+w

      if (s.ne.1.0)then
        gau8=y
      else
        gau8=-y
      endif

      return
c
c  center continued
c
   30 continue

      u=random(iseed)
      w=u*(a(i+1)-aa)
      tt=(0.5*w+aa)*w
      go to 50

   40 continue

      tt=u
      ustar=random(iseed)

   50 continue

      if (ustar.gt.tt) go to 20
      u=random(iseed)
      if (ustar.ge.u) go to 40
      ustar=random(iseed)
      go to 10
c
c  start tail
c
   60 continue

      i=6
      aa=a(32)
      go to 80

   70 continue
 
      aa=aa+d(i)
      i=i+1

   80 continue

      u=u+u
      if (u.lt.1.0)go to 70
      u=u-1.0

   90 continue

      w=u*d(i)
      tt=(0.5*w+aa)*w
      go to 110

  100 continue

      tt=u

  110 continue

      ustar=random(iseed)
      if (ustar.gt.tt) go to 20
      u=random(iseed)
      if (ustar.ge.u) go to 100
      u=random(iseed)
      go to 90

      end
      function gau9()
c
c***********************************************************************
c
c  GAU9 returns a normally distributed Gaussian random value, with zero 
c  mean and unit standard deviation.  It uses the method of Marsaglia and 
c  Tsang.  The program may be called as is, or the user may set the seed by 
c  calling GAU9S.
c
c  GAU9 was extracted from NMS, the software accompanying the book 
c  "Numerical Methods and Software", by Kahaner, Moler, and Nash.  
c
c  Reference:
c
c  Marsaglia and Tsang,
c  A fast, easily implemented method for sampling from decreasing or
c  symmetric unimodal density functions,
c  SIAM J SISC, 1983.
c
c  GAU9   Output, REAL GAU9, a normally distributed random value.
c
      real aa
      real b
      real c
      real c1
      real c2
      real gau9
      real gau9s
      integer ia
      integer ib
      integer ic
      integer id
      integer ii
      integer iii
      integer iseed
      integer j
      integer jj
      integer jjj
      real pc
      real s
      real t
      real u(17)
      real un
      real v(65)
      real vni
      real x
      real xn
      real y
c
      save ii
      save jj
      save u
c
      data aa,b,c / 12.37586,0.4878992,12.67706 /
c
      data c1,c2,pc,xn / .9689279,1.301198,.1958303e-1,2.776994 /

      data v / .3409450,.4573146,.5397793,.6062427,.6631691,.7136975,.
     &  7596125,.8020356,.8417227,.8792102,.9148948,.9490791,.9820005,
     &  1.0138492,1.0447810,1.0749254,1.1043917,1.1332738,1.1616530,
     &  1.1896010,1.2171815,1.2444516,1.2714635,1.2982650,1.3249008,
     &  1.3514125,1.3778399,1.4042211,1.4305929,1.4569915,1.4834526,
     &  1.5100121,1.5367061,1.5635712,1.5906454,1.6179680,1.6455802,
     &  1.6735255,1.7018503,1.7306045,1.7598422,1.7896223,1.8200099,
     &  1.8510770,1.8829044,1.9155830,1.9492166,1.9839239,2.0198430,
     &  2.0571356,2.0959930,2.1366450,2.1793713,2.2245175,2.2725185,
     &  2.3239338,2.3795007,2.4402218,2.5075117,2.5834658,2.6713916,
     &  2.7769943,2.7769943,2.7769943,2.7769943 /
c
c  Load data array in case user forgets to initialize.
c  This array is the result of calling UNI 100000 times with seed 305.
c
      data u / 0.8668672834288,0.3697986366357,0.8008968294805,
     &  0.4173889774680,0.8254561579836,0.9640965269077,0.4508667414265,
     &  0.6451309529668,0.1645456024730,0.2787901807898,0.06761531340295
     &  ,0.9663226330820,0.01963343943798,0.02947398211399,
     &  0.1636231515294,0.3976343250467,0.2631008574685 /
c
      data ii,jj / 17,5 /
c
c***FIRST EXECUTABLE STATEMENT  GAU9
c
c  Fast part...
c
c
c  Basic generator is Fibonacci
c
      un=u(ii)-u(jj)
      if (un.lt.0.0) un=un+1.0
      u(ii)=un
c
c  U(II) and UN are uniform on [0,1), VNI is uniform on [-1,1)
c
      vni=un+un-1.0
      ii=ii-1
      if (ii.eq.0) ii=17
      jj=jj-1
      if (jj.eq.0) jj=17
c
c  INT(UN(II)*128) in range [0,127],  J is in range [1,64]
c
      j=mod(int(u(ii)*128),64)+1
c
c  Pick sign as VNI is positive or negative
c
      gau9=vni*v(j+1)
      if (abs(gau9).le.v(j)) return
c
c  Slow part; AA is a*f(0)
c
      x=(abs(gau9)-v(j))/(v(j+1)-v(j))
c
c  Y is uniform on [0,1)
c
      y=u(ii)-u(jj)
      if (y.lt.0.0) y=y+1.0
      u(ii)=y
      ii=ii-1
      if (ii.eq.0) ii=17
      jj=jj-1
      if (jj.eq.0) jj=17
c
      s=x+y
      if (s.gt.c2) go to 20
      if (s.le.c1) return
      if (y.gt.c-aa*exp(-0.5*(b-b*x)**2)) go to 20
      if (exp(-0.5*v(j+1)**2)+y*pc/v(j+1).le.exp(-0.5*gau9**2))return
c
c  Tail part; .3601016 is 1./XN
c  Y is uniform on [0,1)
c
   10 continue
      y=u(ii)-u(jj)
      if (y.le.0.0) y=y+1.0
      u(ii)=y
      ii=ii-1
      if (ii.eq.0) ii=17
      jj=jj-1
      if (jj.eq.0) jj=17
c
      x=0.3601016*log(y)
c
c  Y is uniform on [0,1)
c
      y=u(ii)-u(jj)
      if (y.le.0.0) y=y+1.0
      u(ii)=y
      ii=ii-1
      if (ii.eq.0) ii=17
      jj=jj-1
      if (jj.eq.0) jj=17
      if (-2.0*log(y).le.x**2) go to 10
      gau9=sign(xn-x,gau9)
      return

   20 continue

      gau9=sign(b-b*x,gau9)

      return

      entry gau9s(iseed)
c
c***********************************************************************
c
c  GAU9S may be used to set the seed value for the normal random sequence
c  of values produced by GAU9.  GAU9S returns a value which is simply the
c  real value of ISEED, and hence is not itself to be used as a normal
c  random value!
c
c  ISEED  Input/output, INTEGER ISEED, used as a seed for the random 
c         number generator.
c
c  GAU9S  Output, REAL GAU9S, a real copy of the value input in ISEED.
c
      if (iseed.ne.0)then
c
c  Set up: Generate random bit pattern in array based on given seed
c
        ii=17
        jj=5
        ia=mod(abs(iseed),32707)
        ib=1111
        ic=1947

        do iii=1, 17

          s=0.0
          t=.50
c
c  Do for each of the bits of mantissa of word
c  Loop  over 64 bits, enough for all known machines in single precision
c
          do jjj=1, 64

            id=ic-ia

            if (id.lt.0)then
              id=id+32707
              s=s+t
            endif

            ia=ib
            ib=ic
            ic=id
            t=.5*t

          enddo

          u(iii)=s

        enddo

      endif
c
c  Return floating echo of ISEED
c
      gau9s=iseed
      return
      end
      function gau10(gmean,sig,iseed)
c
c***********************************************************************
c
c  GAU10 computes a value for a normal random variable on each call.
c
c  A value Y of a uniform random variable on (0,1) is computed.
c  A table of 96 percent points for a normal (0,1) random variable is 
c  linearly interpolated for RVN when Y is in the closed interval 
c  (0.02,0.98).  For Y outside this interval, RVN is computed by a low 
c  accuracy rational Chebyshev approximation for the inverse normal.  
c  Then GAU10=GMEAN + RVN*SIG.
c
c  The maximum error of the normalized variable RVN is approximately 
c  0.37 percent.
c
c  GAU10 calls RANDOM(ISEED) for uniform random numbers.
c
c  Written by D E Amos, July, 1976.
c
c  GMEAN  Input, REAL GMEAN, mean of the normal random variable.
c
c  SIG    Input, REAL SIG, standard deviation of the normal random 
c         variable.
c 
c  ISEED  Input/output, INTEGER ISEED, used as a seed for the random 
c         number generator.
c
c  GAU10  Output, REAL GAU10, value of the normal random variable
c
      real b
      real fy
      real gau10
      real gmean
      real hy
      integer iseed
      integer nhy
      real pct(51)
      real random
      real rvn
      real sgn
      real sig
      real w
      real y
c
      save b
      save pct
c
      data pct / -2.32634787404105e+00,-2.05374891063184e+00,-
     &  1.88079360815123e+00,-1.75068607125212e+00,-1.64485362695140e+00
     &  ,-1.55477359459683e+00,-1.47579102815106e+00,-1.40507156030015e+
     &  00,-1.34075503368665e+00,-1.28155156554316e+00,-
     &  1.22652812003597e+00,-1.17498679206580e+00,-1.12639112903865e+00
     &  ,-1.08031934081486e+00,-1.03643338949373e+00,-9.94457883209723e-
     &  01,-9.54165253146183e-01,-9.15365087842794e-01,-
     &  8.77896295051226e-01,-8.41621233572901e-01,-8.06421247018225e-01
     &  ,-7.72193214188668e-01,-7.38846849185197e-01,-7.06302562840079e-
     &  01,-6.74489750196074e-01,-6.43345405392903e-01,-
     &  6.12812991016618e-01,-5.82841507271208e-01,-5.53384719555666e-01
     &  ,-5.24400512708031e-01,-4.95850347347446e-01,-4.67698799114505e-
     &  01,-4.39913165673230e-01,-4.12463129441393e-01,-
     &  3.85320466407565e-01,-3.58458793251193e-01,-3.31853346436807e-01
     &  ,-3.05480788099391e-01,-2.79319034447449e-01,-2.53347103135795e-
     &  01,-2.27544976641149e-01,-2.01893479158156e-01,-
     &  1.76374164785092e-01,-1.50969215496790e-01,-1.25661346858243e-01
     &  ,-1.00433720525020e-01,-7.52698620998302e-02,-5.01535834647298e-
     &  02,-2.50689082587109e-02,0.00000000000000e+00,2.50689082587134e-
     &  02 /
c
      data b / 9.92290000000001e-01 /
c
      y=random(iseed)
      sgn=y-0.5
      y=min(y,1.0-y)
c
c  Interpolation of percentage point table.
c
      if (y.ge.0.02)then

        hy=100.0*y
        nhy=int(hy)
        fy=hy-float(nhy)

        if (nhy.gt.50.or.nhy.lt.1)then
          write(*,*)' '
          write(*,*)'GAU10 - Fatal error!'
          write(*,*)'  NHY should satisfy 1 <= NHY <= 50,'
          write(*,*)'  but NHY=',nhy
          write(*,*)'  Y=',y
          write(*,*)'  ISEED=',iseed
          stop
        endif

        rvn=(pct(nhy+1)-pct(nhy))*fy+pct(nhy)
c
c  Hastings approximation for inverse normal.
c
      else

        if (y.le.0.0)then
          y=0.5
        endif

        w=-log(y)
        w=sqrt(w+w)
        hy=0.27061*w+2.30753
        fy=(0.04481*w+b)*w+1.0
        rvn=w-hy/fy

      endif

      gau10=gmean+sign(rvn,sgn)*sig

      return
      end
      subroutine gau11(vector,length)
c
c***********************************************************************
c
c  GAU11 is a buffered, vectorized routine for computing normal values.
c  The program can return any number of values, but computes them in
c  groups of 1024 at a time.  The speed is significantly greater than
c  for the scalar codes GAU1, GAU2, GAU3, and GAU4 on the Cray.
c  The Box-Muller method is used.
c
c  GAU11 calls RANF() for uniform random values.  Hence, for repeatable 
c  results, call RANSET(ISEED) first.
c
c  GAU11 is similar to GAU5, but uses the SCILIB routine COSS to 
c  speed up the calculation even more.
c 
c  VECTOR Output, REAL VECTOR(LENGTH), the normal values requested.
c         They have zero mean and unit variance.
c 
c  LENGTH Input, INTEGER LENGTH, the number of values requested.
c 
      integer nbuff
      parameter (nbuff=1024)
c
      integer nbuffh
      parameter (nbuffh=nbuff/2)
c
      real pi
      parameter (pi=3.14159265358979323846)
c
cDIR$ VFUNCTION COSS
c
      real buffer(nbuff)
      complex coss
      integer i
      integer ido
      integer ifirst
      integer ihave
      integer ineed
      integer length
      real ranf
      complex temp
      real vector(length)
      real v1(nbuffh)
      real v2(nbuffh)
c
      save buffer
      save ifirst
      save v1
      save v2
c
      data ifirst / 0 /
c
c  Copy current buffer contents into vector
c
      ihave=0
      ineed=length
      if (ifirst.eq.0) go to 30

   10 continue

      ido=min(ineed-ihave,nbuff+1-ifirst)

      do i=1, ido
        vector(i+ihave)=buffer(i+ifirst-1)
      enddo

      ifirst=ido+ifirst
      ihave=ihave+ido
      if (ihave.eq.length) return
c
c  If not done, buffer is exhausted.  Refresh it.
c
   30 continue

      do i=1, nbuffh
        v1(i)=ranf()
      enddo

      do i=1, nbuffh
        v2(i)=ranf()
      enddo

      do i=1, nbuffh
        temp=coss(2.0*pi*v2(i))
        buffer(2*i-1)=sqrt(-2.0*log(v1(i)))*real(temp)
        buffer(2*i)=sqrt(-2.0*log(v1(i)))*aimag(temp)
      enddo

      ifirst=1
      go to 10
      end
      function gau12(seed)
c
c***********************************************************************
c
c  GAU12 computes a random number that is approximately normally
c  distributed, using the logistic equation
c
c    x(n+1)=4*x(n)*(1-x(n))
c
c  Reference:
c
c  Collins, Fanciulli, Hohlfeld, Finch, Sandri, Shtatland
c  A random number generator based on the logit transform of
c    the logistic variable,
c  Computers in Physics,
c  November/December 1992,
c  Volume 6, Number 6, pages 630-632
c
c  SEED   Input/output, REAL SEED, a "seed" value.
c
c         On first call, the user should have set SEED to a value
c         strictly between 0 and 1.
c
c         On return, SEED will have been updated to a new value,
c         which will be required if GAU12 is called again.
c
c  GAU12  Output, REAL GAU12, a real number that is approximately
c         normally distributed.
c
      real gau12
      integer iseed
      real random
      real seed
c
      intrinsic log
c
      save iseed
c
      data iseed /12345657/
c
      if (seed.le.0.0.or.seed.ge.1.0)then
        seed=random(iseed)
      endif

      seed=4.0*seed*(1.0-seed)

      if (seed.le.0.0.or.seed.ge.1.0)then
        seed=random(iseed)
      endif

      if (seed.gt.0.0.and.seed.lt.1.0)then
        gau12=log(seed/(1.0-seed))
      else
        write(*,*)' '
        write(*,*)'GAU12 - Fatal error!'
        write(*,*)'  Illegal value of SEED=',seed
        write(*,*)'  but SEED should satisfy 0 <= SEED <= 1.'
        stop
      endif

      return
      end
      function halton(iseed)
c
c***********************************************************************
c
c  HALTON uses Halton's method to generate a sequence of quasi 
c  random numbers.
c
c  These numbers are much less "random" than those generated by 
c  most random number generators.  In fact, there is only one 
c  possible sequence generated,
c
c    1/2, 1/4, 3/4, 1/8, 3/8, 5/8, 7/8, 1/16, 3/16, ..., 
c
c  However, for some purposes, such as Monte Carlo integration, 
c  this sequence can be superior to standard random number 
c  generation.  This is because this sequence is guaranteed to 
c  leave no gaps above a certain size, when used, say, to sample 
c  the unit interval.  This can result in a significant increase
c  in the rate of convergence of the approximate integral to the 
c  true value.
c
c  ISEED  Input/output, INTEGER ISEED, on first call, should be 
c         set to any nonzero value as an initialization flag.  On 
c         output, ISEED is reset to zero, and should be left at 
c         that value.
c
c  HALTON Output, REAL HALTON, the next value of the Halton 
c         sequence, a value between 0 and 1.
c
      real halton
      integer i1mach
      integer iseed
      integer k
      integer minn
      integer n
      integer ntwo
      real twon
c
      save k
      save minn
      save n
      save ntwo
      save twon
c
      if (iseed.ne.0)then

        iseed=0
        n=1
        k=1
        ntwo=2**n
        twon=1.0/real(ntwo)
        minn=i1mach(11)

      else

        if (k.eq.ntwo-1)then
          n=n+1
          if (n.gt.minn) n=1
          k=1
          ntwo=2**n
          twon=1.0/real(ntwo)
        else
          k=k+2
        endif

      endif

      halton=real(k)*twon

      return
      end
      function ibino1(pp,n,iseed)
c
c***********************************************************************
c
c  Random numbers, binomial distribution.
c  IBINO1 was extracted from Numerical Recipes.
c
      real pi
      parameter (pi=3.14159265358979323846)
c
      real am
      real em
      real en
      real g
      real gamln2
      integer ibino1
      integer iseed
      integer j
      integer n
      integer nold
      real oldg
      real p
      real pc
      real pclog
      real plog
      real pold
      real pp
      real random
      real sq
      real t
      real y
c
      save en
      save nold
      save oldg
      save pclog
      save plog
      save pold
c
      data nold / -1 /
      data pold / -1.0 /
c
      if (pp.le.0.5)then
        p=pp
      else
        p=1.0-pp
      endif

      am=n*p

      if (n.lt.25)then

        ibino1=0
        do j=1, n
          if (random(iseed).lt.p) ibino1=ibino1+1
        enddo

      elseif(am.lt.1.0)then

        g=exp(-am)
        t=1.0

        do j=0,n
          t=t*random(iseed)
          if (t.lt.g) go to 30
        enddo

        j=n

   30   continue

        ibino1=j

      else

        if (n.ne.nold)then
          en=n
          oldg=gamln2(en+1.0)
          nold=n
        endif

        if (p.ne.pold)then
          pc=1.0-p
          plog=log(p)
          pclog=log(pc)
          pold=p
        endif

        sq=sqrt(2.0*am*pc)

   40   continue

        y=tan(pi*random(iseed))
        em=sq*y+am
        if (em.lt.0.0.or.em.ge.en+1.0) go to 40

        em=int(em)
        t=1.2*sq*(1.0+y**2)*exp(oldg-gamln2(em+1.0)-gamln2(en-em+1.0)+
     &    em*plog+(en-em)*pclog)
        if (random(iseed).gt.t) go to 40

        ibino1=em

      endif

      if (p.ne.pp)then
        ibino1=n-ibino1
      endif

      return
      end
      function ibino2(p,n,iseed)
c
c***********************************************************************
c
      integer i
      integer ibino2
      integer ig
      integer igeo1
      integer iseed
      integer isum
      integer itemp
      integer j
      integer n
      real p
      real random
c
      ibino2=0

      if (p.le.0.0.or.p.ge.1.0)then
        write(*,*)' '
        write(*,*)'IBINO2 - Fatal error!'
        write(*,*)'  Illegal input value of P=',p
        write(*,*)'  but we require 0 < P < 1.'
        return
      endif
c
      if (n.lt.1)then
        write(*,*)' '
        write(*,*)'IBINO2 - Fatal error!'
        write(*,*)'  Illegal input value of N=',n
        write(*,*)'  but N must be at least 1.'
        return
      endif
c
c  If P is moderate or large, use the rejection method.
c
      if (p.ge.0.1)then
        itemp=0

        do i=1,n
          if (random(iseed).le.p) then
            itemp=itemp+1
          end if
        enddo

      else
c
c  If P is small, use the fact that the waiting time for 1 success in
c  Bernoulli trials has a geometric distribution.
c
        isum=0
        j=1

   20   continue

        ig=igeo1(p,iseed)
        isum=isum+ig+1

        if (isum.le.n)then
          j=j+1
          go to 20
        endif

        itemp=j-1

      endif

      ibino2=itemp

      return
      end
      function igeo1(p,iseed)
c
c***********************************************************************
c
c  IGEO1 returns a random number of binomial trials before success.
c
c  The process may be thought of as flipping a coin, with a 
c  probability P of heads on any one flip, and returning the number
c  of tails that were flipped before a head was reached.  
c
c  IGEO1 was extracted and adapted from ACM algorithm 564.  
c
c  The routine used there was written by James Filipak of the 
c  National Bureau of Standards.  
c
c  The distribution of values returned by IGEO1 is also called a
c  geometric distribution, since the probability of the value N is
c  (1-P)**N.
c
c  P      Input, REAL P, the probability of success on one trial.  P mus
c         be strictly greater than 0, and strictly less than 1.
c
c  ISEED  Input/output, INTEGER ISEED, a seed for the random number gene
c
c  IGEO1  Output, INTEGER IGEO1, the number of failures before the first
c         success.  In particular, IGEO=0 is possible, and signifies 
c         that a success was achieved on the first trial.
c
      integer igeo1
      integer iseed
      real p
      real random
      real x
c
      intrinsic log
      intrinsic int
      external random
c
      igeo1=0

      if (p.le.0.0.or.p.ge.1.0)then
        write(*,*)' '
        write(*,*)'IGEO1 - Fatal error!'
        write(*,*)'  Illegal value of P=',P
        write(*,*)'  but we require 0 < P < 1.'
        return
      endif
c
   10 continue

      x=random(iseed)

      if (x.eq.0.0)then
        iseed=iseed+1
        go to 10
      endif
c
      igeo1=int(log(x)/log(1.0-p))

      return
      end
      function ipoi1(xm,iseed)
c
c***********************************************************************
c
c  Random numbers, Poisson distribution.
c
      real pi
      parameter (pi=3.14159265358979323846)
c
      real alxm
      real em
      real g
      real gamln2
      integer ipoi1
      integer iseed
      real oldm
      real random
      real sq
      real t
      real tmp
      real xm
c
      save alxm
      save g
      save oldm
      save sq
c
      data oldm / -1.0 /
c
      if (xm.lt.12.0)then

        if (xm.ne.oldm)then
          oldm=xm
          g=exp(-xm)
        endif

        em=-1.0
        t=1.0

   10   continue

        em=em+1.0
        t=t*random(iseed)
        if (t.gt.g) go to 10

      else

        if (xm.ne.oldm)then
          oldm=xm
          sq=sqrt(2.0*xm)
          alxm=log(xm)
          g=xm*alxm-gamln2(xm+1.0)

        endif

   20   continue

        tmp=random(iseed)
        if (tmp.eq.1.0) go to 20

        y=tan(pi*tmp)
        em=sq*y+xm
        if (em.lt.0.0) go to 20

        em=aint(em)
        t=0.9*(1.0+y**2)*exp(em*alxm-gamln2(em+1.0)-g)
        if (random(iseed).gt.t) go to 20

      endif

      ipoi1=int(em)
      return
      end
      function ipoi2(xmu,ir)
c
c***********************************************************************
c
c     for details see:
c
c  ahrens, j.h. and dieter, u.
c               computer generation of poisson deviates
c               from modified normal distributions.
c               acm trans. math. software, 8,2 (june 1982), 163 - 179.
c
c     (slightly modified version of the program in the above article)
c
c     input:  ir=current state of basic random number generator
c             Xmu=mean mu of the poisson distribution
c     output: IPOI2=sample from the poisson-(mu)-distribution
c
c  Xmuprev=previous Xmu,
c  Xmuold=Xmu at last execution of step p or b.
c  tables: coefficients a0-a7 for step f. factorials fact
c  coefficients a(k) - for px=fk*v*v*sum(a(k)*v**k)-del
c
      real a0
      real a1
      real a2
      real a3
      real a4
      real a5
      real a6
      real a7
      real b1
      real b2
      real c
      real c0
      real c1
      real c2
      real c3
      real d
      real del
      real difmuk
      real e
      real exp2
      real fact(10)
      real fk
      real fx
      real fy
      real g
      real gau8
      integer ipoi2
      integer ir
      integer j
      integer k
      integer kflag
      integer l
      integer m
      real omega
      real p
      real p0
      real pp(35)
      real px
      real py
      real q
      real ran17
      real s
      real t
      real u
      real v
      real x
      real xmu
      real xmuold
      real xmuprev
      real xx
c
      data xmuprev,xmuold / 0.,0. /
      data a0,a1,a2,a3,a4,a5,a6,a7 / -.5,.3333333,-.2500068,.2000118,-.
     &  1661269,.1421878,-.1384794,.1250060 /
      data fact / 1.,1.,2.,6.,24.,120.,720.,5040.,40320.,362880. /
c
c     separation of cases a and b
c
      if (xmu.eq.xmuprev) go to 10
      if (xmu.lt.10.0) go to 120
c
c  Case A. (recalculation of s,d,l if Xmu has changed)
c
      xmuprev=xmu
      s=sqrt(xmu)
      d=6.0*xmu*xmu
c
c             the poisson probabilities pk exceed the discrete normal
c             probabilities fk whenever k >= m(Xmu). l=ifix(Xmu-1.1484)
c             is an upper bound to m(Xmu) for all Xmu >= 10 .
c
      l=ifix(xmu-1.1484)
c
c     step n. normal sample - GAU8(ir) for standard normal deviate
c
   10 g=xmu+s*gau8(ir)
      if (g.lt.0.0) go to 20
      ipoi2=ifix(g)
c
c  step i. immediate acceptance if IPOI2 is large enough
c
      if (ipoi2.ge.l) return
c
c  step s. squeeze acceptance - RAN17(ir) for (0,1)-sample u
c
      fk=float(ipoi2)
      difmuk=xmu-fk
      u=ran17(ir)
      if (d*u.ge.difmuk*difmuk*difmuk) return
c
c  step p. preparations for steps q and h.
c             (recalculations of parameters if necessary)
c             .3989423=(2*pi)**(-.5)  .416667e-1=1./24.  .1428571=1./7.
c             the quantities b1, b2, c3, c2, c1, c0 are for the hermite
c             approximations to the discrete normal probabilities fk.
c             c=.1069/Xmu guarantees majorization by the 'hat'-function.
c
   20 if (xmu.eq.xmuold) go to 30
      xmuold=xmu
      omega=.3989423/s
      b1=.4166667e-1/xmu
      b2=.3*b1*b1
      c3=.1428571*b1*b2
      c2=b2-15.*c3
      c1=b1-6.*b2+45.*c3
      c0=1.-b1+3.*b2-15.*c3
      c=.1069/xmu
   30 if (g.lt.0.0) go to 50
c
c  'subroutine' f is called (kflag=0 for correct return)
c
      kflag=0
      go to 70
c
c  step q. quotient acceptance (rare case)
c
   40 if (fy-u*fy.le.py*exp(px-fx)) return
c
c  step e. exponential sample - EXP2(ir) for standard exponential
c             deviate e and sample t from the laplace 'hat'
c             (if t <= -.6744 then pk < fk for all Xmu >= 10.)
c
   50 e=exp2(ir)
      u=ran17(ir)
      u=u+u-1.0
      t=1.8+sign(e,u)
      if (t.le.(-.6744)) go to 50
      ipoi2=ifix(xmu+s*t)
      fk=float(ipoi2)
      difmuk=xmu-fk
c
c  'subroutine' f is called (kflag=1 for correct return)
c
      kflag=1
      go to 70
c
c  step h. hat acceptance (e is repeated on rejection)
c
   60 if (c*abs(u).gt.py*exp(px+e)-fy*exp(fx+e)) go to 50
      return
c
c  step f. 'subroutine' f. calculation of px,py,fx,fy.
c             case IPOI2 .lt. 10 uses factorials from table fact
c
   70 if (ipoi2.ge.10) go to 80
      px=-xmu
      py=xmu**ipoi2/fact(ipoi2+1)
      go to 110
c
c             case IPOI2 .ge. 10 uses polynomial approximation
c             a0-a7 for accuracy when advisable
c             .8333333e-1=1./12.  .3989423=(2*pi)**(-.5)
c
   80 del=.8333333e-1/fk
      del=del-4.8*del*del*del
      v=difmuk/fk
      if (abs(v).le.0.25) go to 90
      px=fk*log(1.0+v)-difmuk-del
      go to 100

   90 px=fk*v*v*(((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v+a0)-
     &  del

  100 py=.3989423/sqrt(fk)

  110 x=(0.5-difmuk)/s
      xx=x*x
      fx=-0.5*xx
      fy=omega*(((c3*xx+c2)*xx+c1)*xx+c0)
      if (kflag) 40, 40, 60
c
c     Case  b. (start new table and calculate p0 if necessary)
c
  120 xmuprev=0.0
      if (xmu.eq.xmuold) go to 130
      xmuold=xmu
      m=max(1,ifix(xmu))
      l=0
      p=exp(-xmu)
      q=p
      p0=p
c
c     step u. uniform sample for inversion method
c
  130 u=ran17(ir)
      ipoi2=0
      if (u.le.p0) return
c
c     step t. table comparison until the end pp(l) of the
c             pp-table of cumulative poisson probabilities
c             (0.458=pp(9) for Xmu=10)
c
      if (l.eq.0) go to 150
      j=1
      if (u.gt.0.458) j=min(l,m)

      do k=j, l
        if (u.le.pp(k)) go to 180
      enddo

      if (l.eq.35) go to 130
c
c     step c. creation of new poisson probabilities p
c             and their cumulatives q=pp(k)
c
  150 l=l+1

      do k=l,35
        p=p*xmu/float(k)
        q=q+p
        pp(k)=q
        if (u.le.q) go to 170
      enddo

      l=35
      go to 130

  170 l=k

  180 continue

      ipoi2=k
      return
      end
      subroutine iran0(n,ia,nbits)
c
c***********************************************************************
c
c   This is a pseudo-random number generator for vector computers based
c   lagged Fibonacci scheme with lags 5 and 17:
c
c       IB(K)=IB(K-5) + IB(K-17) MOD 2**(NBITS/2)
c
c   The IB array is actually a 128 x 17 array (in order to facilitate
c   vector processing).  The array IA is obtained from IB.
c
c   This version assumes that N is a multiple of 64.  Subsequent calls
c   generate additional pseudorandom data in a continuous Fibonacci
c   sequence.  It is initialized by calling with N equal to zero.  This
c   routine should produce the same pseudorandom sequence on any system
c   that supports NBITS-bit INTEGER data.
c
c   David H. Bailey     May 2, 1988
c
      integer ibs
      integer l
      integer m
      real pi
c
      parameter (ibs=2176)
      parameter (l=2222)
      parameter (m=5**6)
      parameter (pi=3.14159265358979323846)
c
      integer i
      integer i1
      integer i2
      integer ia(*)
      integer ib(ibs)
      integer ip1
      integer ip2
      integer ipak
      integer itemp
      integer k
      integer m32
      integer n
      integer nbits
      integer ndiv
      integer nsub
c
      save ib
      save ip1
      save ip2
      save m32
c
      ipak(i1,i2)=ior(ishft(i1,nbits/2),i2)
c
c   This section is executed only during initialization.
c
      if (n.eq.0)then
        ip1=0
        ip2=1536
        itemp=int(log10(float(2**(nbits/2))))
        ib(1)=pi*10**itemp
        m32=(2**(nbits/2))-1
c
c   Use a linear congruential pseudorandom number generator to initializ
c
        do i=2,ibs
          ib(i)=iand(m*ib(i-1)+l,m32)
        enddo

      endif
c
c   For a normal call, use a vectorizable lagged Fibonacci scheme.
c   Two NBITS/2-bit results are combined to generate one NBITS-bit outpu
c
      ndiv=n/64
      nsub=ndiv*64

      do k=0,n-64,64
c
c   Both of the next two loops are vectorizable.
c
cDIR$ IVDEP
c
        do i=1,128
          ib(i+ip1)=iand(ib(i+ip1)+ib(i+ip2),m32)
        enddo

        do i=1,64
          ia(i+k)=ipak(ib(i+ip1),ib(i+ip1+64))
        enddo

        ip1=ip1+128
        if (ip1.eq.ibs) ip1=0
        ip2=ip2+128
        if (ip2.eq.ibs) ip2=0

      enddo
c
c  The next two loops are vectorizable.
c
cDIR$ IVDEP
c
      do i=1,128
        ib(i+ip1)=iand(ib(i+ip1)+ib(i+ip2),m32)
      enddo

      k=nsub
      do i=1,n-nsub
        ia(i+k)=ipak(ib(i+ip1),ib(i+ip1+64))
      enddo
c
      ip1=ip1+128
      if (ip1.eq.ibs) ip1=0
      ip2=ip2+128
      if (ip2.eq.ibs) ip2=0

      return
      end
      function irbit1(iseed)
c
c***********************************************************************
c
c  Random bit generator, extracted from Numerical Recipes.
c
      integer ib1
      integer ib18
      integer ib2
      integer ib5
c
      parameter (ib1=1)
      parameter (ib2=2)
      parameter (ib5=16)
      parameter (ib18=131072)
c
      integer irbit1
      integer iseed
      logical newbit
c
      newbit=iand(iseed,ib18).ne.0

      if (iand(iseed,ib5).ne.0) newbit=.not.newbit
      if (iand(iseed,ib2).ne.0) newbit=.not.newbit
      if (iand(iseed,ib1).ne.0) newbit=.not.newbit
      irbit1=0
      iseed=iand(ishft(iseed,1),not(ib1))

      if (newbit)then
        irbit1=1
        iseed=ior(iseed,ib1)
      endif

      return
      end
      function irbit2(iseed)
c
c***********************************************************************
c
c  Random bit generator, extracted from Numerical Recipes.
c
      integer ib1
      parameter (ib1=1)
c
      integer ib2
      parameter (ib2=2)
c
      integer ib5
      parameter (ib5=16)
c
      integer ib18
      parameter (ib18=131072)
c
      integer mask
      parameter (mask=ib1+ib2+ib5)
c
      integer irbit2
      integer iseed
c
      if (iand(iseed,ib18).ne.0)then
        iseed=ior(ishft(ieor(iseed,mask),1),ib1)
        irbit2=1
      else
        iseed=iand(ishft(iseed,1),not(ib1))
        irbit2=0
      endif

      return
      end
      function pchi1(x,nfree)
c
c***********************************************************************
c
c  PCHI1 returns the probability of a given chi-square value X,
c  with NFREE degrees of freedom.
c
c  Adapted from:
c  Hill, I. D. and Pike, M. C.  Algorithm 299
c  Collected Algorithms for the CACM 1967 p. 243
c  Updated for rounding errors based on remark in
c  ACM TOMS June 1985, page 185
c
c  BIGX is the maximum value of X for which we will take EXP(X).
c
      real bigx
      parameter (bigx=20.0)
c
c  LOG(SQRT(PI))
c
      real algspi
      parameter (algspi=0.5723649429247000870717135)
c
c  1/SQRT(PI)
c
      real pisinv
      parameter (pisinv=0.5641895835477562869480795)
c
      real a
      real c
      real dnorm1
      real e
      logical even
      integer nfree
      real pchi1
      real s
      real x
      real xx
      real y
      real z
c
      if (x.le.0.0.or.nfree.lt.1)then
        pchi1=0.0
        return
      endif
c
      if (nfree.eq.1)then
        pchi1=1.0-2.0*dnorm1(-sqrt(x))
        return
      endif
c
      if (nfree.eq.2)then
        pchi1=1.0-exp(-0.5*x)
        return
      endif
c
      a=0.5*x
      y=exp(-a)
      even=2*(nfree/2).eq.nfree
c
      if (even)then
        s=y
      else
        s=2.0*dnorm1(-sqrt(x))
      endif
c
      xx=0.5*(nfree-1.0)
      if (even)then
        z=1.0
      else
        z=0.5
      endif
      if (a.gt.bigx)then
        if (even)then
          e=0.0
        else
          e=algspi
        endif
        c=log(a)

   10   continue

        if (z.le.xx)then
          e=log(z)+e
          if (c*z-a-e.ge.-bigx) s=s+exp(c*z-a-e)
          z=z+1.0
          go to 10
        endif

        pchi1=1.0-s

      else

        if (even)then
          e=1.0
        else
          e=(pisinv/sqrt(a))
        endif

        c=0.0
   20   continue

        if (z.le.xx)then
          e=e*(a/z)
          c=c+e
          z=z+1.0
          go to 20
        endif
        pchi1=1.0-c*y-s
      endif
      return
      end
      function pchi2(x,nfree,kode,nz)
c
c***********************************************************************
c
c     WRITTEN BY D.E. AMOS AND S.L. DANIEL, OCTOBER, 1974.
c
c     REFERENCE SC-DR-73 0333
c
c         PCHI2 COMPUTES THE CUMULATIVE CHI-SQUARE DISTRIBUTION F(X) OR
c         ITS COMPLEMENT 1.-F(X) WITH N DEGREES OF FREEDOM, X.GE.0 AND
c         N.GE.1. THE RELATION OF F(X) TO THE INCOMPLETE GAMMA FUNCTION
c         (NORMALIZED TO 1. AT X=INFINITY) IS USED FOR THE COMPUTATION
c         IN SUBROUTINES GAMIC AND GAMTL, WITH THE CHANGE FROM ONE
c         SUBROUTINE TO THE OTHER AT ARGUMENT=THE PARAMETER OF THE GAMMA
c         FUNCTIONS. THIS CHANGE NOT ONLY ENSURES SIGNIFICANT DIGITS FOR
c         BOTH F(X) AND 1.-F(X), BUT MAKES THE COMPUTATION AS FAST AS
c         POSSIBLE IN EACH SUBROUTINE. A PARAMETER REL=1.E-8 IS SET IN
c         THE PROGRAM FOR 8 SIGNIFICANT DIGITS. REL CAN BE SET AS LOW AS
c         1.E-12 FOR 12 SIGNIFICANT DIGITS.
c         PCHI2 USES GAMIC, GAMTL, GAMLN1.
c
c     DESCRIPTION OF ARGUMENTS
c
c         INPUT
c           X      - ARGUMENT, X.GE.0.0
c           NFREE  - DEGREES OF FREEDOM OF THE CHI-SQUARE(N)
c                    DISTRIBUTION, NFREE.GE.1.
c           KODE   - A SELECTION PARAMETER
c                    KODE=1 RETURNS PCHI2=F(X)
c                    KODE=2 RETURNS PCHI2=1.-F(X)
c         OUTPUT
c           PCHI2 - F(X) OR 1.-F(X), DEPENDING ON KODE.
c           NZ     - UNDERFLOW FLAG
c                    NZ.EQ.0, A NORMAL RETURN.
c                    NZ.NE.0, UNDERFLOW, PCHI2=0.0 RETURNED.
c
c     ERROR CONDITIONS
c         IMPROPER INPUT - A FATAL ERROR
c         UNDERFLOW - A NON-FATAL ERROR.
c
      real fn
      real hn
      integer kode
      integer nfree
      integer nz
      real pchi2
      real r1mach
      real rel
      real x
      real xx
      real y(1)
c
      pchi2=0.0

      if (kode.lt.1.or.kode.gt.2)then
        write(*,*)' '
        write(*,*)'PCHI2 - Fatal error!'
        write(*,*)'  Improper input value of KODE=',kode
        return
      endif
c
      if (nfree.lt.1)then
        write(*,*)' '
        write(*,*)'PCHI2 - Fatal error!'
        write(*,*)'  Illegal value of NFREE < 1 =',nfree
        return
      endif

      fn=real(nfree)
c
      if (x.lt.0.0)then
        write(*,*)' '
        write(*,*)'PCHI2 - Fatal error!'
        write(*,*)'  Illegal value of X < 0.0 =',x
        return
      endif
c
      rel=r1mach(4)
      nz=0
      hn=0.5*fn
      xx=0.5*x

      if (kode.eq.1)then
        if (xx.lt.hn)then
          call gamic(xx,hn,rel,1,y,nz)
          pchi2=y(1)
        else
          call gamtl(xx,hn,rel,1,y,nz)
          pchi2=1.0-y(1)
        endif
      else
        if (xx.lt.hn)then
          call gamic(xx,hn,rel,1,y,nz)
          pchi2=1.-y(1)
        else
          call gamtl(xx,hn,rel,1,y,nz)
          pchi2=y(1)
        endif
      endif

      return
      end
      function ran0(idum)
c
c***********************************************************************
c
c  RAN0 was extracted from Numerical Recipes
c
      integer iff
      real ran0
      real v(97)
      real y
c
      save iff
      save iseed
      save v
      save y
c
      data iff / 0 /
c
      if (idum.lt.0.or.iff.eq.0)then

        iff=1
        iseed=iabs(idum)
        idum=1

        do j=1, 97
          dum=random(iseed)
        enddo

        do j=1, 97
          v(j)=random(iseed)
        enddo

        y=random(iseed)

      endif

      j=1+int(97.0*y)
      if (j.gt.97) j=97
      y=v(j)
      ran0=y
      v(j)=random(iseed)

      return
      end
      function ran1(idum)
c
c***********************************************************************
c
c  This routine was extracted from Numerical Recipes
c
      integer ia1
      integer ia2
      integer ia3
      integer ic1
      integer ic2
      integer ic3
      integer m1
      integer m2
      integer m3
      real rm1
      real rm2
c
      parameter (ia1=7141)
      parameter (ia2=8121)
      parameter (ia3=4561)
      parameter (ic1=54773)
      parameter (ic2=28411)
      parameter (ic3=51349)
      parameter (m1=259200)
      parameter (m2=134456)
      parameter (m3=243000)
      parameter (rm1=3.8580247e-6)
      parameter (rm2=7.4373773e-6)
c
      integer idum
      integer iff
      integer ix1
      integer ix2
      integer ix3
      integer j
      real r(97)
      real ran1
c
      save iff
      save ix1
      save ix2
      save ix3
      save r
c
      data iff / 0 /
c
      if (idum.lt.0.or.iff.eq.0)then

        iff=1
        ix1=mod(ic1-idum,m1)
        ix1=mod(ia1*ix1+ic1,m1)
        ix2=mod(ix1,m2)
        ix1=mod(ia1*ix1+ic1,m1)
        ix3=mod(ix1,m3)

        do j=1, 97
          ix1=mod(ia1*ix1+ic1,m1)
          ix2=mod(ia2*ix2+ic2,m2)
          r(j)=(float(ix1)+float(ix2)*rm2)*rm1
        enddo

        idum=1

      endif

      ix1=mod(ia1*ix1+ic1,m1)
      ix2=mod(ia2*ix2+ic2,m2)
      ix3=mod(ia3*ix3+ic3,m3)
      j=1+(97*ix3)/m3

      if (j.gt.97)then
        j=97
      endif

      ran1=r(j)
      r(j)=(float(ix1)+float(ix2)*rm2)*rm1

      return
      end
      function ran2(idum)
c
c***********************************************************************
c
c  This routine was extracted from Numerical Recipes
c
      integer ia
      integer ic
      integer m
      real rm
c
      parameter (ia=1366)
      parameter (ic=150889)
      parameter (m=714025)
      parameter (rm=1.4005112e-6)
c
      integer idum
      integer iff
      integer ir(97)
      integer iy
      integer j
      real ran2
c
      save iff
      save ir
      save iy
c
      data iff / 0 /
c
      if (idum.lt.0.or.iff.eq.0)then

        iff=1
        idum=mod(ic-idum,m)

        do j=1, 97
          idum=mod(ia*idum+ic,m)
          ir(j)=idum
        enddo

        idum=mod(ia*idum+ic,m)
        iy=idum

      endif

      j=1+(97*iy)/m
      if (j.gt.97) j=97
      iy=ir(j)
      ran2=iy*rm
      idum=mod(ia*idum+ic,m)
      ir(j)=idum

      return
      end
      function ran3(idum)
c
c***********************************************************************
c
c  This routine was extracted from Numerical Recipes
c
      real fac
      integer mbig
      integer mseed
      integer mz
c
      parameter (fac=1.0e-9)
      parameter (mbig=1000000000)
      parameter (mseed=161803398)
      parameter (mz=0)
c
      integer i
      integer idum
      integer iff
      integer ii
      integer inext
      integer inextp
      integer k
      integer ma(55)
      integer mk
      real ran3
c
      save iff
      save inext
      save inextp
      save ma
      save mj
      save mk
c
      data iff / 0 /
c
      if (idum.lt.0.or.iff.eq.0)then
        iff=1
        mj=mseed-iabs(idum)
        mj=mod(mj,mbig)
        ma(55)=mj
        mk=1

        do i=1, 54
          ii=mod(21*i,55)
          ma(ii)=mk
          mk=mj-mk
          if (mk.lt.mz) mk=mk+mbig
          mj=ma(ii)
        enddo

        do k=1, 4
          do i=1, 55
            ma(i)=ma(i)-ma(1+mod(i+30,55))
            if (ma(i).lt.mz) ma(i)=ma(i)+mbig
          enddo
        enddo

        inext=0
        inextp=31
        idum=1

      endif

      inext=inext+1
      if (inext.eq.56) inext=1
      inextp=inextp+1
      if (inextp.eq.56) inextp=1
      mj=ma(inext)-ma(inextp)
      if (mj.lt.mz) mj=mj+mbig
      ma(inext)=mj
      ran3=mj*fac

      return
      end
      function ran4()
c
c***********************************************************************
c
c  This routine was extracted from TSPACK.
c
      integer k
      real ran4
      real rang4
      real rans4
      real seed
      real xrand(3)
c
      save xrand
c
      data xrand / 0.53952704,53952704.0,10011.0 /
c
      xrand(1)=amod(xrand(1)*xrand(3),1.0)
      ran4=xrand(1)

      return
c
      entry rans4(seed)
c
c***********************************************************************
c
      if (seed.eq.0.0)then
        k=0
      elseif(seed.lt.0.0)then
        k=int(log10(abs(seed)))+1
      else
        k=int(log10(seed))+1
      endif

      xrand(1)=seed/(10.0**k)
      xrand(2)=seed

      return
c
      entry rang4()
c
c***********************************************************************
c
      rang4=xrand(2)

      return
      end
      function ran5()
c
c***********************************************************************
c
c  This routine was extracted from ELEFUNT.
c
      integer iy
      real ran5
c
      save iy
c
      data iy / 100001 /
c
      iy=iy*125
      iy=iy-(iy/2796203)*2796203
      ran5=float(iy)/2796203.0

      return
      end
      function ran6(iseed)
c
c***********************************************************************
c
c  This routine was extracted from ROSEPACK.
c
      integer iseed
      integer ix0
      real ran6
c
      save ix0
c
      data ix0 / 2 /
c
      if (iseed.ne.0)then
        iseed=mod(iseed,9973)
        if (iseed.ne.0) ix0=iseed
      endif
c
c  IN ORDER THAT ALL FIXED-POINT CALCULATIONS REQUIRE ONLY 20 BIT
c  ARITHMETIC, WE USE TWO CALLS ON MOD TO COMPUTE
c  IX0=MOD(3432*IX0, 9973).
c
      ix0=mod(52*mod(66*ix0,9973),9973)
      iseed=ix0
      ran6=float(ix0)/9973.0

      return
      end
      function ran7()
c
c***********************************************************************
c
c  Algorithm AS 183 from the journal "Applied Statistics".
c
c  Written by B A Wichmann and I D Hill.
c
      integer ix
      integer iy
      integer iz
      real ran7
c
      intrinsic amod
      intrinsic float
c
      save ix
      save iy
      save iz
c
      data ix / 1952 /
      data iy / 28453 /
      data iz / 17 /
c
      ix=171*mod(ix,177)-2*(ix/177)
      iy=172*mod(iy,176)-35*(iy/176)
      iz=170*mod(iz,178)-63*(iz/178)
c
      if (ix.lt.0) then
        ix=ix+30269
      end if

      if (iy.lt.0)then
        iy=iy+30307
      end if

      if (iz.lt.0) then
        iz=iz+30323
      end if
c
      ran7=amod(float(ix)/30269.0+float(iy)/30307.0+float(iz)/30323.0,
     &  1.0)
      return
      end
      function ran8(r)
c
c***********************************************************************
c
c  RAN8 generates a uniformly distributed random number.
c
      integer ia0
      integer ia1
      integer ia1ma0
      integer ic
c
      parameter (ia1=1536)
      parameter (ia0=1029)
      parameter (ia1ma0=507)
      parameter (ic=1731)
c
      integer ix0
      integer ix1
      integer iy0
      integer iy1
      real r
      real ran8
c
      save ix0
      save ix1
c
      data ix0 /0/
      data ix1 /0/
c
c  A*X=2**22*IA1*IX1+2**11*(IA1*IX1+(IA1-IA0)*(IX0-IX1)+IA0*IX0)+IA0*I
c
      if (r.eq.0.0)then
        iy0=ia0*ix0
        iy1=ia1*ix1+ia1ma0*(ix0-ix1)+iy0
        iy0=iy0+ic
        ix0=mod(iy0,2048)
        iy1=iy1+(iy0-ix0)/2048
        ix1=mod(iy1,2048)
        ran8=ix1*2048+ix0
        ran8=ran8/4194304.
      elseif(r.lt.0.0)then
        ran8=ix1*2048+ix0
        ran8=ran8/4194304.0
      else
        ix1=amod(r,1.0)*4194304.0+0.5
        ix0=mod(ix1,2048)
        ix1=(ix1-ix0)/2048
        ran8=ix1*2048+ix0
        ran8=ran8/4194304.0
      endif

      return
      end
      function ran9(t,n)
c
c***********************************************************************
c
c  RAN9 was extracted from SLATEC.
c 
c  Author: Wayne Fullerton, Los Alamos National Laboratory.
c 
c  This random number generator is portable among a wide variety of
c  computers.  It generates a random number between 0.0 and 1.0 
c  according to the algorithm presented by Bays and Durham 
c  (TOMS, 2, 59, 1976).  The motivation for using this scheme, 
c  which resembles the Maclaren-Marsaglia method, is to greatly 
c  increase the period of the random sequence.  If the period of 
c  the basic generator (RAN8) is P, then the expected mean period 
c  of the sequence generated by RAN9 is given by   
c    new mean P=SQRT (PI*FACTORIAL(N)/(8*P)),
c  where FACTORIAL(N) must be much greater than P in this 
c  asymptotic formula.  Generally, N should be around 32 if P=4.E6 
c  as for RAN8.
c 
c
c  T      Input/output, REAL T(*), an array of IABS(N)+1 random 
c         numbers from a previous invocation of RAN9.  Whenever N 
c         is positive and differs from the old N, the table is 
c         initialized.  The first IABS(N) numbers are the table 
c         discussed in the reference, and the N+1 -st value is Y.
c         This array may be saved in order to restart a sequence.
c 
c  N      Input, INTEGER N.  IABS(N) is the number of random numbers
c         in an auxiliary table.  Note though that IABS(N)+1 is the
c         number of items in array T.  If N is positive and differs
c         from its value in the previous invocation, then the table
c         is initialized for the new value of N.  If N is negative, 
c         IABS(N) is the number of items in an auxiliary table, but
c         the tables are now assumed already to be initialized.  
c         This option enables the user to save the table T at the 
c         end of a long computer run and to restart with the same 
c         sequence.  Normally, RAN9 would be called at most
c         once with negative N.  Subsequent invocations would have 
c         N positive and of the correct magnitude.
c
c  RAN9   Output, REAL RAN9, a random number between 0.0 and 1.0.
c
      real dummy
      real floatn
      integer i
      integer j
      integer n
      integer nold
      real ran8
      real ran9
      real t(*)
      real temp
c
      save nold
      save floatn
c
      intrinsic abs
c
      data nold / -1 /
c
      if (n.ne.nold)then
      
        nold=abs(n)
        floatn=nold
        
        if (n.lt.0)then
          dummy=ran8(t(nold+1))
        else
          do i=1,nold
            t(i)=ran8(0.0)
          enddo
          t(nold+1)=ran8(0.0)
        endif
        
      endif

      j=int(t(nold+1)*floatn+1.)
      t(nold+1)=t(j)
      ran9=t(j)
      temp=0.0
      t(j)=ran8(temp)

      return
      end
      function ran10(iseed)
c
c***********************************************************************
c
c  RAN10 was extracted from the LINPACK benchmark.
c
c
c  Input/output, INTEGER ISEED.
c
c  On input, if ISEED is 0, then the generator is initialized.
c  Otherwise, ISEED is assumed to be the output value of ISEED
c  from the previous call to RAN10.
c
c  On output, ISEED has a value of no interest to the user, but
c  which should be returned to RAN10 on the next call, for the
c  next number in the sequence.
c
c  Output, REAL RAN10, a pseudorandom number between 0.0 and 1.0.
c 
      integer icall
      integer iseed
      real ran10
c
      intrinsic float
      intrinsic mod
c
      save icall
c
      data icall / 0 /
c
c  The generator should be reinitialized if ISEED is 0, and
c  also if this is the first call.
c
      if (icall.eq.0.or.iseed.eq.0)then
        icall=1
        iseed=1325
      endif
c
c  Compute the new value of ISEED.
c      
      iseed=mod(3125*iseed,65536)
c
c  Set the random number.
c     
      ran10=float(iseed)/65536.0
      
      return
      end
      function ran11(t)
c
c***********************************************************************
c
c  RAN11 was extracted from the NASA Ames Kernel benchmark.
c
c  It computes X(N+1)=5**7 * X(N) MOD 2**30.
c  It will generate 2**28, or approximately 268 million, numbers
c  before repeating, assuming that the hardware multiply operation
c  is correct to 47 bits of precision.
c
      real f7
      parameter (f7=78125.0)
c
      real t30
      parameter (t30=1073741824.0)
c
c
      integer icall
      real ran11
      real t
c
      intrinsic amod
c
      save icall
c
      data icall / 0 /
c
      if (icall.eq.0.or.t.eq.0.0)then
        icall=1
        t=f7/t30
      endif
c
      t=amod(f7*t,1.0)
c
      ran11=t
      return
      end
      function ran12(iseed)
c
c***********************************************************************
c
c  -- LAPACK test routine --
c     Argonne National Lab, Courant Institute, and N.A.G. Ltd.
c     April 1, 1989
c
c     ..
c
c  Purpose
c  =======
c
c     Generate a random number uniformly distributed between 0. and 1.
c     A linear congruential sequence is used. This code is machine
c     independent, provided 12 bit integers can be added and multiplied
c     to produce 24 bit answers.  Note that ISEED(4) must be odd.
c
c  Arguments
c  =========
c
c  ISEED  - INTEGER array of dimension( 4 )
c           On entry ISEED specifies the seed of the random number
c           generator. The array elements should be between 0 and 4095;
c           if not they will be reduced mod 4096.  Also, ISEED(4) must
c           be odd.  The random number generator uses a linear
c           congruential sequence limited to small integers, and so
c           should produce machine independent random numbers. The
c           values of ISEED are changed on exit, and can be used in the
c           next call to SLAROR to continue the same random number
c           sequence.
c           Modified.
c
      integer m1
      parameter (m1=502)
c
      integer m2
      parameter (m2=1521)
c
      integer m3
      parameter (m3=4071)
c
      integer m4
      parameter (m4=2107)
c
c
      real t12
      parameter (t12=4096.0)
c
      integer i1
      integer i2
      integer i3
      integer i4
      integer iseed(4)
      real r
      real ran12
c
      intrinsic real
      intrinsic mod
c
c     The following is just multiplication of two 48-bit
c     fixed-point numbers, each of which is represented by 4 12-bit
c     pieces.  The constant, "M", is represented by M1 throught M4,
c     M1 being the high-order part, and the variable ISEED is
c     represented by ISEED(1) through ISEED(4), ISEED(1) being the
c     high-order part.  The binary point can be thought of as
c     lying between M1 and M2, and between ISEED(1) and ISEED(2).
c
c     The code is thus ISEED=MOD( M * ISEED , 4096 )
c
      i1=iseed(1)*m4+iseed(2)*m3+iseed(3)*m2+iseed(4)*m1
      i2=iseed(2)*m4+iseed(3)*m3+iseed(4)*m2
      i3=iseed(3)*m4+iseed(4)*m3
      i4=iseed(4)*m4
      iseed(4)=mod(i4,4096)
      i3=i3+i4/4096
      iseed(3)=mod(i3,4096)
      i2=i2+i3/4096
      iseed(2)=mod(i2,4096)
      iseed(1)=mod(i1+i2/4096,4096)
c
c     Compute RAN12=ISEED / 4096.0
c
      r=1.0/t12
      ran12=r*(real(iseed(1))+r*(real(iseed(2))+r*(real(iseed(3))+r*
     &  (real(iseed(4))))))

      return
      end
      function ran13(reset)
c
c***********************************************************************
c
c  Generates random numbers uniformly distributed between 0 and 1.
c  This sequence repeats quickly, and is not 'finely' distributed.
c
c  A sequence I is generated, and RAN13 is computed by:
c
c  RAN13=REAL(I)/1001.0
c
c  The sequence of values of I is bounded between 1 and 999.
c
c  The initial value of I is set internally.  It is currently set to 7.
c
c  If initial I=1,2,3,6,7 or 9, the period will be 50.
c  If initial I=4 or 8, the period will be 25.
c  If initial I=5, the period will be 10.
c  IC is used to break up the period by skipping 1 value of I in 6.
c
c  Auxiliary routine for test program for Level 3 Blas.
c
c  -- Written on 8-February-1989.
c     Jack Dongarra, Argonne National Laboratory.
c     Iain Duff, AERE Harwell.
c     Jeremy Du Croz, Numerical Algorithms Group Ltd.
c     Sven Hammarling, Numerical Algorithms Group Ltd.
c
      real ran13
      logical reset
      integer i,ic,mi
c
      save i
      save ic
      save mi
c
      if (reset)then
c
c        Initialize local variables.
c
        mi=891
        i=7
        ic=0
        reset=.false.
      endif
      ic=ic+1

   10 continue

      i=i*mi
      i=i-1000*(i/1000)

      if (ic.ge.5)then
        ic=0
        go to 10
      endif

      ran13=real(i)/1001.0
      return
      end
      function ran14(iseed)
c
c***********************************************************************
c
c***BEGIN PROLOGUE   RAN14
c***DATE WRITTEN   810915
c***REVISION DATE  830805
c***CATEGORY NO.  L6A21
c***KEYWORDS  RANDOM NUMBERS, UNIFORM RANDOM NUMBERS
c***AUTHOR    BLUE, JAMES, SCIENTIFIC COMPUTING DIVISION, NBS
c             KAHANER, DAVID, SCIENTIFIC COMPUTING DIVISION, NBS
c             MARSAGLIA, GEORGE, COMPUTER SCIENCE DEPT., WASH STATE UNIV
c
c***PURPOSE  THIS ROUTINE GENERATES QUASI UNIFORM RANDOM NUMBERS ON [0,1
c             AND CAN BE USED ON ANY COMPUTER WITH WHICH ALLOWS INTEGERS
c             AT LEAST AS LARGE AS 32767.
c***DESCRIPTION
c
c       THIS ROUTINE GENERATES QUASI UNIFORM RANDOM NUMBERS ON THE INTER
c       [0,1).  IT CAN BE USED WITH ANY COMPUTER WHICH ALLOWS
c       INTEGERS AT LEAST AS LARGE AS 32767.
c
c
c   USE
c       FIRST TIME....
c                   Z= RAN14(JD)
c                     HERE JD IS ANY  N O N - Z E R O  INTEGER.
c                     THIS CAUSES INITIALIZATION OF THE PROGRAM
c                     AND THE FIRST RANDOM NUMBER TO BE RETURNED AS Z.
c       SUBSEQUENT TIMES...
c                   Z= RAN14(JD)
c                     where JD is zero,
c                     CAUSES THE NEXT RANDOM NUMBER TO BE RETURNED AS Z.
c
c
c..................................................................
c   NOTE: USERS WHO WISH TO TRANSPORT THIS PROGRAM FROM ONE COMPUTER
c         TO ANOTHER SHOULD READ THE FOLLOWING INFORMATION.....
c
c   MACHINE DEPENDENCIES...
c      MDIG=A LOWER BOUND ON THE NUMBER OF BINARY DIGITS AVAILABLE
c              FOR REPRESENTING INTEGERS, INCLUDING THE SIGN BIT.
c              THIS VALUE MUST BE AT LEAST 16, BUT MAY BE INCREASED
c              IN LINE WITH REMARK A BELOW.
c
c   REMARKS...
c     A. THIS PROGRAM CAN BE USED IN TWO WAYS:
c        (1) TO OBTAIN REPEATABLE RESULTS ON DIFFERENT COMPUTERS,
c            SET 'MDIG' TO THE SMALLEST OF ITS VALUES ON EACH, OR,
c        (2) TO ALLOW THE LONGEST SEQUENCE OF RANDOM NUMBERS TO BE
c            GENERATED WITHOUT CYCLING (REPEATING) SET 'MDIG' TO THE
c            LARGEST POSSIBLE VALUE.
c     B. THE SEQUENCE OF NUMBERS GENERATED DEPENDS ON THE INITIAL
c          INPUT 'JD' AS WELL AS THE VALUE OF 'MDIG'.
c          IF MDIG=16 ONE SHOULD FIND THAT
c            THE FIRST EVALUATION
c              JD=305
c              Z= RAN14(JD) GIVES Z=.027832881...
c            THE SECOND EVALUATION
c              Z= RAN14(JD) GIVES   Z=.56102176...
c            THE THIRD EVALUATION
c              Z=RAN14(JD) GIVES   Z=.41456343...
c            THE THOUSANDTH EVALUATION
c              Z= RAN14(JD) GIVES   Z=.19797357...
c
c***REFERENCES  MARSAGLIA G., "COMMENTS ON THE PERFECT UNIFORM RANDOM
c                 NUMBER GENERATOR", UNPUBLISHED NOTES, WASH S. U.
c***ROUTINES CALLED  I1MACH
c***END PROLOGUE   RAN14
c
      integer i
      integer i1mach
      integer iseed
      integer j
      integer j0
      integer j1
      integer jseed
      integer k
      integer k0
      integer k1
      integer m(17)
      integer m1
      integer m2
      integer mdig
      real ran14
c
      external i1mach
c
      save i
      save j
      save m
      save m1
      save m2
c
      data m(1),m(2),m(3),m(4),m(5),m(6),m(7),m(8),m(9),m(10),m(11),m(12
     &  ),m(13),m(14),m(15),m(16),m(17) / 30788,23052,2053,19346,10646,
     &  19427,23975,19049,10949,19693,29746,26748,2796,23890,29168,31924
     &  ,16499 /
c
      data m1,m2,i,j / 32767,256,5,17 /
c
c***FIRST EXECUTABLE STATEMENT   RAN14
c
      if (iseed.eq.0) go to 20
c
c  Fill
c
      mdig=i1mach(8)+1
      mdig=min(mdig,16)
c
c  BE SURE THAT MDIG AT LEAST 16...
c
      if (mdig.lt.16)then
        write(*,*)' '
        write(*,*)'RAN14 - Fatal error!'
        write(*,*)'  MDIG is less than 16!'
        write(*,*)'  MDIG=',mdig
        ran14=0.0
        return
      endif
c
      m1=2**(mdig-2)+(2**(mdig-2)-1)
      m2=2**(mdig/2)
      jseed=min(iabs(iseed),m1)
      if (mod(jseed,2).eq.0) jseed=jseed-1
      k0=mod(9069,m2)
      k1=9069/m2
      j0=mod(jseed,m2)
      j1=jseed/m2

      do i=1, 17
        jseed=j0*k0
        j1=mod(jseed/m2+j0*k1+j1*k0,m2/2)
        j0=mod(jseed,m2)
        m(i)=j0+m2*j1
      enddo

      i=5
      j=17
      iseed=0
c
c  BEGIN MAIN LOOP HERE
c
   20 continue

      k=m(i)-m(j)
      if (k.lt.0) k=k+m1
      m(j)=k
      i=i-1
      if (i.eq.0) i=17
      j=j-1
      if (j.eq.0) j=17
      ran14=real(k)/real(m1)
      return
      end
      function ran15(i)
c
c***********************************************************************
c
c  Extracted from NCAR LOCLIB routine DCPOLY.
c
      integer i
      integer j
      real ran15
c
      i=i*2045+1497794
      j=i/1048576
      i=i-j*1048576

      ran15=real(i)/1048576.0

      return
      end
      function ran16(ix)
c
c***********************************************************************
c
c  Portable random number generator using the recursion 
c
c    IX=IX*IA mod IP
c
c  Extracted from ACM algorithm 570, "LOPSI".
c
c  IA=7**5, IB=2**15, IB16=2**16, IP=2**31-1
c
      integer ia
      parameter (ia=16807)
c
      integer ib15
      parameter (ib15=32768)
c
      integer ib16
      parameter (ib16=65536)
c
      integer ip
      parameter (ip=2147483647)
c
      integer iprhi
      integer ix
      integer ixhi
      integer k
      integer leftlo
      integer loxa
      real ran16
c
      intrinsic real
c
c  Get 15 hi order bits of IX
c
      ixhi=ix/ib16
c
c  Get 16 lo bits of IX and form lo product
c
      loxa=(ix-ixhi*ib16)*ia
c
c  Get 15 hi order bits of lo product
c
      leftlo=loxa/ib16
c
c  Form the 31 highest bits of full product
c
      iprhi=ixhi*ia+leftlo
c
c  Get overflo past 31st bit of full product
c
      k=iprhi/ib15
c
c  Assemble all the parts and presubtract IP.  The parentheses are 
c  essential.
c
      ix=(((loxa-leftlo*ib16)-ip)+(iprhi-k*ib15)*ib16)+k
c
c  Add IP back in if necessary
c
      if (ix.lt.0) ix=ix+ip
c
c  Multiply by 1/(2**31-1)
c
      ran16=real(ix)*4.656612875e-10

      return
      end
      function ran17(ir)
c
c***********************************************************************
c
c  RAN17 was originally named SUNIF, and was part of ACM TOMS
c  algorithm 599.
c
c  RAN17 returns a sample value from the [0,1] uniform distribution
c  by a multiplicative congruential generator of the form
c    r := r * factor (mod 1).
c
c  In the first call r is initialized to
c    r := ir / 2**28 ,
c  where ir must be of the form  ir=4*k+1.
c
c  Then r assumes all values  0 < (4*k+1)/2**28 < 1 during a full 
c  period of 2**26 distinct values.
c
c  the parameter ir is used only in the first call for
c  initialization.  Thereafter (when negative) ir becomes a dummy 
c  variable.
c
c  For details see:
c
c  ahrens, j.h., dieter, u. and grube, a.
c  pseudo-random numbers:  a new proposal for the choice of 
c    multiplicators
c  computing, 6 (1970), 121 - 138
c
c  factor integer of the form 8*k+5 as close as possible
c         to  2**26 * (sqrt(5)-1)/2     (golden section)
c
c  two28  2**28  (i.e. 28 significant bits for deviates)
c
      double precision factor
      parameter (factor=41475557.0)
c
      double precision two28
      parameter (two28=268435456.0) 
c
c
      integer ir
      integer ircopy
      double precision r
      real ran17
c
      intrinsic dble
      intrinsic dmod
      intrinsic mod
      intrinsic sngl
c
      save r
c
c  First call: initialization
c
      if (ir.gt.0)then
        ircopy=ir
        if (mod(ircopy,4).ne.1)then
          ircopy=4*(ircopy/4)+1
        endif
        r=dble(ircopy)/two28
        ir=-1
      endif

      r=dmod(r*factor,1.0d0)
      ran17=sngl(r)

      return
      end
      function ran18(iseed)
c
c***********************************************************************
c
c  Extracted from ACM Algorithm 647.
c
c  PORTABLE PSEUDORANDOM NUMBER GENERATOR IMPLEMENTING THE RECURSION
c
c  ISEED=16807*ISEED MOD(2**31-1)
c  UNIF=ISEED/(2**31-1)
c
c  USING ONLY 32 BITS INCLUDING SIGN
c
c  INPUT:
c
c  ISEED =INTEGER STRICTLY BETWEEN 0 AND 2** 31 -1
c
c  OUTPUTS:
c
c  ISEED=NEW PSEUDORANDOM INTEGER STRICTLY BETWEEN 0 AND 2**31-1
c
c  UNIF=UNIFORM VARIATE (FRACTION) STRICTLY BETWEEN 0 AND 1
c
c  Reference:
c
c  P. BRATLEY, B.L. FOX, AND L.E. SCHRAGE (1983)
c  "A GUIDE TO SIMULATION"
c  SPRINGER-VERLAG, PAGES 201-202
c
      integer iseed
      integer k1
      real ran18
c
      k1=iseed/127773
      iseed=16807*(iseed-k1*127773)-k1*2836
      if (iseed.lt.0) iseed=iseed+2147483647
      ran18=iseed*4.656612875e-10
c
      return
      end
      function ran19()
c
c***********************************************************************
c
c  PORTABLE PSEUDO-RANDOM NUMBER GENERATOR, 
c  FROM TONY WARNOCK, C.R.I.
c
c  USE DOUBLE PRECISION ON SHORT WORD LENGTH MACHINES
c
      double precision a
      double precision b
      real ran19
      double precision ranb
      double precision rans
c
      intrinsic int
      intrinsic sngl
c
      save ranb
      save rans
c
      data ranb / 2396745.0 /
      data rans / 2396745.0 /
c
      a=14390069.0*rans+1.0
      b=rans+14390069.0*ranb+dble(int(a*2.0**(-24)))
      ranb=b-dble(int(b*2.0**(-24))*2.0**24)
      rans=a-dble(int(a*2.0**(-24))*2.0**24)
      ran19=sngl((ranb*2.0**24+rans)*2.0**(-48))
      return
      end
      function ran21(iseed)
c
c***********************************************************************
c
c  RAN21 is the portable random number generator of L. Schrage.
c  It is "full cycle"; that is, every integer from 1 to 2**31 - 2  is 
c  generated exactly once in the cycle.  It is completely described
c  in ACM Transactions on Mathematical Software, Volume 5, 1979, 
c  pages 132-138.
c
c  ISEED  Input/output, INTEGER ISEED, is a positive variable which 
c         specifies the seed to the random number generator.  On 
c         output, the seed is updated.
c
c  RAN21  Output, REAL RAN21, a random number in (0,1).
c
      integer a
      integer b15
      integer b16
      real c
      integer p
c
      parameter (a=16807)
      parameter (b15=32768)
      parameter (b16=65536)
      parameter (c=4.656612875e-10) 
      parameter (p=2147483647)
c
      integer iseed
      integer fhi
      integer k
      integer leftlo
      real ran21
      integer xalo
      integer xhi
c
      intrinsic real
c
c  Get the 15 high order bits of ISEED.
c
      xhi=iseed/b16
c
c  Get the 16 low order bits of ISEED, and compute the low
c  part of the product.
c
      xalo=(iseed-xhi*b16)*a
c
c  Get the 15 high order bits of the low part of the product.
c
      leftlo=xalo/b16
c
c  Form the 31 highest bits of the full product.
c
      fhi=xhi*a+leftlo
c
c  Get the overflow past the 31st bit of the full product.
c
      k=fhi/b15
c
c  Assemble the parts, and presubtract P.  The parentheses
c  are essential.
c
      iseed=(((xalo-leftlo*b16)-p)+(fhi-k*b15)*b16)+k
c
c  Add P back if necessary.
c
      if (iseed.lt.0) iseed=iseed+p
c
c  Multiply by 1/(2**31 - 1)
c
      ran21=c*real(iseed)

      return
      end
      function ran22()
c
c***********************************************************************
c
c***BEGIN PROLOGUE  RAN22
c***DATE WRITTEN   810915 (YYMMDD)
c***REVISION DATE  871210 (YYMMDD)
c***CATEGORY NO.  L6A21
c***KEYWORDS  RANDOM NUMBERS, UNIFORM RANDOM NUMBERS
c***AUTHOR    KAHANER, DAVID, SCIENTIFIC COMPUTING DIVISION, NBS
c             MARSAGLIA, GEORGE, SUPERCOMPUTER RES. INST., FLORIDA ST. U
c
c***PURPOSE  THIS ROUTINE GENERATES REAL (SINGLE PRECISION) UNIFORM
c             RANDOM NUMBERS ON [0,1)
c***DESCRIPTION
c        Computes real (single precision) uniform numbers on [0,1).
c           From the book, "Numerical Methods and Software" by
c                D. Kahaner, C. Moler, S. Nash
c                Prentice Hall, 1988
c
c       USAGE:
c              To initialize the generator
c                   USEED=RAN22S(ISEED)
c               where: ISEED is any NONZERO integer
c                  will return floating point value of ISEED.
c
c               Subsequently
c                       U=RAN22()
c                  will return a real uniform on [0,1)
c
c                One initialization is necessary, but any number of eval
c                  of  RAN22 in any order, are allowed.
c
c           Note: Depending upon the value of K (see below), the output
c                       of RAN22 may differ from one machine to another.
c
c           Typical usage:
c
c               REAL U,RAN22,RAN22S,USEED
c               INTEGER ISEED
cC                 Set seed
c               ISEED=305
c               USEED=RAN22S(ISEED)
c               DO 1 I=1,1000
c                   U=RAN22()
c             1 CONTINUE
cC                 NOTE: If K=24 (the default, see below) the output val
cC                           U will be 0.1570390462475...
c               WRITE(*,*) U
c               END
c
c          NOTE ON PORTABILITY: Users can choose to run RAN22 in its def
c               mode (requiring NO user action) which will generate the
c               sequence of numbers on any computer supporting floating
c               numbers with at least 24 bit mantissas, or in a mode tha
c               will generate numbers with a longer period on computers
c               larger mantissas.
c          TO EXERCISE THIS OPTION:  B E F O R E  invoking RAN22S insert
c               the instruction        UBITS=RAN22B(K)      K >= 24
c               where K is the number of bits in the mantissa of your fl
c               point word (K=48 for Cray, Cyber 205). RAN22B returns th
c               floating point value of K that it actually used.
c                    K input as .LE. 24, then UBITS=24.
c                    K input as .GT. 24, then UBITS=FLOAT(K)
c               If K>24 the sequence of numbers generated by RAN22 may d
c               from one computer to another.
c
c***REFERENCES  MARSAGLIA G., "COMMENTS ON THE PERFECT UNIFORM RANDOM
c                 NUMBER GENERATOR", UNPUBLISHED NOTES, WASH S. U.
c***ROUTINES CALLED  (NONE)
c***END PROLOGUE RAN22
c
      real cd
      real cm
      real csave
c
      parameter (cd=7654321./16777216.)
      parameter (cm=16777213./16777216.)
      parameter (csave=362436./16777216.)
c
c  2**24=16777216
c
      real c
      integer i
      integer i1
      integer ii
      integer iseed
      integer j
      integer j1
      integer jj
      integer k
      integer k1
      integer kk
      integer l1
      integer m1
      real ran22
      real ran22b
      real ran22s
      real s
      real t
      real u(17)
c
      save c
      save i
      save j
      save k
      save u
c
c  Load the data array in case user forgets to initialize.
c  This array is the result of calling RAN22 100000 times
c  with ISEED=305 and K=64.
c
      data u / 0.8668672834288,0.3697986366357,0.8008968294805,
     &  0.4173889774680,0.8254561579836,0.9640965269077,0.4508667414265,
     &  0.6451309529668,0.1645456024730,0.2787901807898,0.06761531340295
     &  ,0.9663226330820,0.01963343943798,0.02947398211399,
     &  0.1636231515294,0.3976343250467,0.2631008574685 /
      data i,j,k,c / 17,5,24,csave /
c
c  Basic generator is Fibonacci
c
      ran22=u(i)-u(j)
      if (ran22.lt.0.0) ran22=ran22+1.0
      u(i)=ran22
      i=i-1
      if (i.eq.0) i=17
      j=j-1
      if (j.eq.0) j=17
c
c  Second generator is congruential
c
      c=c-cd
      if (c.lt.0.0) c=c+cm
c
c  Combination generator
c
      ran22=ran22-c
      if (ran22.lt.0.0) ran22=ran22+1.0
      return
c
      entry ran22s(iseed)
c
c  Set up ...
c  Convert ISEED to four smallish positive integers.
c
      i1=mod(abs(iseed),177)+1
      j1=mod(abs(iseed),167)+1
      k1=mod(abs(iseed),157)+1
      l1=mod(abs(iseed),147)+1
c
c  Generate random bit pattern in array based on given seed.
c
      do ii=1, 17
        s=0.0
        t=0.5
c
c  Do for each of the bits of mantissa of word
c  Loop  over K bits, where K is defaulted to 24 but can
c  be changed by user call to RAN22B(K)
c
        do jj=1, k
          m1=mod(mod(i1*j1,179)*k1,179)
          i1=j1
          j1=k1
          k1=m1
          l1=mod(53*l1+1,169)
          if (mod(l1*m1,64).ge.32) s=s+t
          t=.5*t
        enddo
        
        u(ii)=s
      enddo
   
      ran22s=float(iseed)
      return
c
      entry ran22b(kk)
c
      if (kk.le.24)then
        k=24
      else
        k=kk
      endif
      ran22b=float(k)
      end
      function ran23()
c
c***********************************************************************
c
c  Algorithm AS 183 Appl. Statist. (1982) vol.31, no.2
c
c  Returns a pseudo-random number rectangularly distributed
c  between 0 and 1.   The cycle length is 6.95E+12(See page 123
c  of Applied Statistics (1984) vol.33), not as claimed in the
c  original article.
c
c  IX, IY and IZ should be set to integer values between 1 and
c  30000 before the first entry.
c
c  Integer arithmetic up to 30323 is required.
c
      integer ix
      integer iy
      integer iz
      real ran23
c
      common /comr23/ ix,iy,iz
c
      save /comr23/
c
c     ix=171 * mod(ix, 177) - 2 * (ix / 177)
c     iy=172 * mod(iy, 176) - 35 * (iy / 176)
c     iz=170 * mod(iz, 178) - 63 * (iz / 178)
c     if (ix .lt. 0) ix=ix + 30269
c     if (iy .lt. 0) iy=iy + 30307
c     if (iz .lt. 0) iz=iz + 30325
c
c  If integer arithmetic up to 5212632 is available, the preceding
c  6 statements may be replaced by:
c
      ix=mod(171*ix,30269)
      iy=mod(172*iy,30307)
      iz=mod(170*iz,30323)
c
      ran23=mod(float(ix)/30269.+float(iy)/30307.+float(iz)/30323.,1.0
     &  )
      return
      end
      subroutine ran23s(ixx,iyy,izz)
c
c***********************************************************************
c
      integer ix
      integer ixx
      integer iy
      integer iyy
      integer iz
      integer izz

      common /comr23/ ix,iy,iz

      save /comr23/
c
      ix=ixx
      iy=iyy
      iz=izz

      return
      end
      function ran24()
c
c***********************************************************************
c
c  Generate uniformly distributed random numbers using the 32-bit
c  generator from figure 3 of:
c  L'Ecuyer, P. Efficient and portable combined random number
c  generators, C.A.C.M., vol. 31, 742-749 & 774-?, June 1988.
c
c  The cycle length is claimed to be 2.30584E+18
c
      integer iz,k,is1,is2
      real ran24
c
      common /comr24/ is1,is2
c
      save /comr24/
c
      k=is1/53668
      is1=40014*(is1-k*53668)-k*12211
      if (is1.lt.0) is1=is1+2147483563
c
      k=is2/52774
      is2=40692*(is2-k*52774)-k*3791
      if (is2.lt.0) is2=is2+2147483399
c
      iz=is1-is2
      if (iz.lt.1) iz=iz+2147483562
c
      ran24=iz/2147483563.
      return
      end
      subroutine ran24s(iseed1,iseed2)
c
c***********************************************************************
c
c  Set seeds for the uniform random number generator.
c
      integer is1,is2,iseed1,iseed2
      common /comr24/ is1,is2
      save /comr24/
c
      is1=iseed1
      is2=iseed2
      return
      end
      subroutine ran25(n,a)
c
c***********************************************************************
c
c  RAN25 is a pseudo-random number generator for vector computers 
c  based on a lagged Fibonacci scheme with lags 5 and 17:
c
c    B(K)=B(K-5) + B(K-17) MOD 1.
c
c  The B array is actually a 64 x 17 array in order to facilitate
c  vector processing.  The floating-point array A is obtained from B.
c
c  This version assumes that N is a multiple of 64.  Subsequent calls
c  generate additional pseudorandom data in a continuous Fibonacci
c  sequence.  It is initialized by calling with N equal to zero.
c  This routine should produce the same pseudorandom sequence on any
c  system with at least 47 mantissa bits in single precision FP data.
c
c  David H. Bailey     May 4, 1988
c
      integer ibs
      parameter (ibs=1088)
c
      double precision r23
      parameter (r23=2.**(-23))
c
      double precision rm
      parameter (rm=5.**9)
c
      double precision rl
      parameter (rl=222222.)
c
      double precision t0
      parameter (t0=3141593.)
c
      real a(n)
      double precision b(ibs)
      integer i
      integer ip1
      integer ip2
      integer k
      integer n
      double precision t
      double precision t1
      double precision t2
c
      intrinsic int
      intrinsic sngl
c
      save b
      save ip1
      save ip2
c
      if (n.eq.0)then
        ip1=0
        ip2=768
        t2=t0*r23
c
c  Initialize table using a linear congruential pseudorandom 
c  number generator.
c
c  46 bit floating-point data are generated by combining two 23-bit
c  results.
c
c  This loop is not vectorizable.
c
        do i=1, ibs
          t=rm*t2+rl
          t1=t-int(t)
          t=rm*t1+rl
          t2=t-int(t)
          b(i)=t1+r23*t2
        enddo

      endif
c
c  Pseudorandom results are generated using the lagged Fibonacci 
c  scheme
c
      do k=0, n-64, 64
c
c   This loop is vectorizable.
c
cDIR$ IVDEP
c
        do i=1, 64
          t=b(i+ip1)+b(i+ip2)
          t=t-int(t)
          b(i+ip1)=t
          a(i+k)=real(t)
        enddo
c
        ip1=ip1+64
        if (ip1.eq.ibs) ip1=0
        ip2=ip2+64
        if (ip2.eq.ibs) ip2=0

      enddo
c
      return
      end
      function ran26(iniz)
c
c***********************************************************************
c
c  RAN26 GENERATES RANDOM NUMBERS UNIFORMLY DISTRIBUTED IN (0,1)
c  EXPLOITING THOSE GENERATED BY RAN26T WITH A FURTHER RANDOMIZATION.
c  IF THE INPUT PARAMETER INIZ IS NOT 0 THE RANDOM NUMBER GENERATOR IS
c  INITIALIZED
c
      real a
      real b
      real c
      integer i0
      integer iniz
      integer irem
      integer k
      integer nrem
      real p
      real p0
      real p1
      real p2
      real r1
      real r2
      real ran26
      real x(61)
      real x0
c
      save a
      save b
      save c
      save i0
      save irem
      save nrem
      save p
      save p0
      save p1
      save p2
      save r1
      real r2
      save x
      real x0
c
      data nrem / 61 /
      data a / -1.5/
      data b /5.5/
      data c /-2.0 /
      data irem / 0 /
      data p0 / 3.0 /
      data p1,p2 / 1.0,3.0 /
      data r1,r2 / 0.250,0.750 /
c
      if (iniz.ne.0.or.irem.eq.0) go to 10
c
      i0=irem
      x0=x(i0)
c
c  NONLINEARIZATION OF X0 TO AVOID LONG-DISTANCE LINEAR RELATIONSHIP.
c
      if (x0.ge.3.5e-5)then
        x0=amod(1.0/x0,1.0)
      endif
c
c  UPDATE A COMPONENT OF THE VECTOR X ...
c
      call ran26t(nrem,x,irem)
c
c  AND FURTHER RANDOMIZE
c
      ran26=amod(x0+x(i0),1.0)
      return
c
c  INITIALIZATION OF THE RANDOM NUMBER GENERATOR
c
   10 continue

      p=p0-1.0/(float(iabs(iniz))+100.0)

      do k=1, nrem
        p=c+p*(b+p*a)
        x(k)=r1+(r2-r1)*(p-p1)/(p2-p1)
      enddo

      irem=0
      do k=1, 100
        call ran26t(nrem,x,irem)
      enddo

      ran26=x(1)
      return
      end
      subroutine ran26t(nrem,x,irem)
c
c***********************************************************************
c
c  UPDATES THE COMPONENT IREM OF THE NREM-VECTOR X WITH A RANDOM NUMBER
c  UNIFORMLY DISTRIBUTED IN (0,1) BY MEANS OF THE ALGORITHM
c  OF MITCHELL-MOORE, MODIFIED AS SUGGESTED BY BRENT, QUOTED IN
c  D.E.KNUTH, THE ART OF COMPUTER PROGRAMMING, SECOND EDITION,
c  SECOND VOLUME, SEMINUMERICAL ALGORITHMS, ADDISON-WESLEY
c  PUB. CO., READING (1981), PP. 26-28.
c
      integer n1
      parameter (n1=24)
c
      integer n2
      parameter (n2=55)
c
c
      integer nrem
c
      integer i1
      integer i2
      integer irem
      real x(nrem)
c
      save i1
      save i2
c
      if (irem.eq.0)then
        irem=nrem
        i1=nrem-n1
        i2=nrem-n2
      endif
c
      x(irem)=amod(x(i1)+x(i2),1.0)
c
      irem=1+mod(irem,nrem)
      i1=1+mod(i1,nrem)
      i2=1+mod(i2,nrem)
c
      return
      end
      function ran27(idum)
c
c***********************************************************************
c
c  RAN27 embodies the "minimal standard" random number generator of Park
c  and Miller, with Bays-Durham shuffle.
c
c  Reference:
c
c  William Press and Glennys Farrar,
c  Recursive Stratified Sampling for Multidimensional Monte Carlo 
c    Integration,
c  Computers in Physics, March/April 1990
c
c  Park and Miller,
c  Communications of the ACM, Volume 31, page 1192, 1988
c
c  IDUM   Input/output, INTEGER IDUM, initialization flag and seed.  If
c         is negative, the sequence is initialized, and IDUM is reset.
c         or not IDUM was negative on input, the value of IDUM is used t
c         compute a new random value, and IDUM is reset.  The output val
c         of IDUM should be input to the routine on the next call for
c         another random value.
c
c  RAN27  Output, REAL RAN27, a uniform random number strictly between 0
c
      integer ia
      parameter (ia=16807)
c
      integer im
      parameter (im=2147483647)
c
      real am
      parameter (am=1.0/im)
c
      integer iq
      parameter (iq=127773)
c
      integer ir
      parameter (ir=2836)
c
      integer ntab
      parameter (ntab=32)
c
      real atab
      parameter (atab=ntab-1)
c
      integer idum
      integer j
      integer k
      real ran27
      real v(ntab)
      real y
c
      save v
      save y
c
      data v / ntab*0 /
      data y / 0.5 /
c
      if (idum.le.0)then

        idum=max(-idum,1)

        do j=ntab, 1, -1
          k=idum/iq
          idum=ia*(idum-k*iq)-ir*k
          if (idum.lt.0) idum=idum+im
          v(j)=am*idum
        enddo

        y=v(1)

      endif
c
   20 continue

      k=idum/iq
      idum=ia*(idum-k*iq)-ir*k
      if (idum.lt.0) idum=idum+im
      j=1+int(atab*y)
      y=v(j)
      ran27=y
      v(j)=am*idum
      if (ran27.eq.0.0.or.ran27.eq.1.0) go to 20

      return
      end
      function ran28(iseed)
c
c***********************************************************************
c
c  This routine was extracted from PRAXIS, a routine for finding the
c  minimum of a function of N variables using the principal axis method.
c
c  Reference:
c
c  Richard Brent,
c  Algorithms for Finding Zeros and Extrema of Functions without
c  Calculating Derivatives
c
c  ISEED  Input, INTEGER ISEED, the seed from which the sequence is to
c         be generated.  As long as ISEED is held fixed, new entries in
c         a given sequence will be generated.  If a new value of ISEED
c         is entered, a new sequence is begun.  Only the remainder when
c         ISEED is divided by 8192 is significant.
c
c  RAN28  Output, REAL RAN28, a random value, uniformly distributed in
c         the interval [0,1].
c
      integer i
      logical init
      integer irand2
      integer iseed
      integer j
      integer jseed
      integer q
      integer r
      real ran28
      real rand1
      real rand3(127)
c
      save init
      save jseed
      save rand1
      save irand2
      save rand3
c
      data init / .false. /
      data jseed / -1 /
c
      if (.not.init.or.iseed.ne.jseed)then

        jseed=iseed
        r=mod(iseed,8190)+1

        do i=1,127

          rand1=-2.0**55

          do j=1,7
            r=mod(1756*r,8191)
            q=r/32
            rand1=(rand1+q)/256.0
          enddo

          rand3(128-i)=rand1

        enddo

        irand2=1
        init=.true.

      endif
c
      if (irand2.le.1)then
        irand2=128
      endif

      irand2=irand2-1
      rand1=rand1+rand3(irand2)

      if (rand1.lt.0.0)then
        rand1=rand1+0.5
      else
        rand1=rand1-0.5
      endif

      rand3(irand2)=rand1

      ran28=rand1+0.5

      return
      end
      function ran29(xseed)
c
c***********************************************************************
c
c  RAN29 computes a random number that is uniformly distributed
c  on the interval [0,1], using the logistic equation
c
c    x(n+1)=4*x(n)*(1-x(n))
c
c  Reference:
c
c  Collins, Fanciulli, Hohlfeld, Finch, Sandri, Shtatland
c  A random number generator based on the logit transform of
c    the logistic variable,
c  Computers in Physics,
c  November/December 1992,
c  Volume 6, Number 6, pages 630-632
c
c  XSEED  Input/output, REAL XSEED, a "seed" value.
c
c         On first call, the user should have set XSEED to a value
c         strictly between 0 and 1.
c
c         On return, XSEED will have been updated to a new value,
c         which will be required if RAN29 is called again.
c
c  RAN29  Output, REAL RAN29, a uniformly distributed random number.
c
      real pi
      parameter (pi=3.14159265358979323846)
c
      integer iseed
      real ran29
      real random
      real xseed
c
      save iseed
c
      data iseed /1234567/
c
      if (xseed.eq.0.or.xseed.eq.1)xseed=random(iseed)
      xseed=4*xseed*(1-xseed)
      ran29=(2.0/pi)*asin(sqrt(xseed))

      return
      end
      function ranl(iseed,x)
c
c***********************************************************************
c
c  Logarithmically distributed random numbers.
c
      integer iseed
      real random
      real ranl
      real x
c
      intrinsic exp
c
      ranl=exp(x*random(iseed))

      return
      end
      function rnoise(anoise)
c
c***********************************************************************
c
c  This routine was extracted from LLSQ.
c
      integer mi
      parameter (mi=891)
c
      integer mj
      parameter (mj=457)
c
      real aj
      real anoise
      integer i
      integer j
      real rnoise
c
      save aj
      save i
      save j
c
      data aj / 0.0 /
      data i / 5 /
      data j / 7 /
c
      if (anoise.lt.0.0)then
        i=5
        j=7
        aj=0.0
        rnoise=0.0
        return
      endif
c
c  THE SEQUENCE OF VALUES OF J  IS BOUNDED BETWEEN 1 AND 996
c  IF INITIAL J=1,2,3,4,5,6,7,8, OR 9, THE PERIOD IS 332
c
      if (anoise.gt.0.0)then
        j=j*mj
        j=j-997*(j/997)
        aj=j-498
      endif
c
c  THE SEQUENCE OF VALUES OF I  IS BOUNDED BETWEEN 1 AND 999
c  IF INITIAL I=1,2,3,6,7, OR 9,  THE PERIOD WILL BE 50
c  IF INITIAL I=4 OR 8   THE PERIOD WILL BE 25
c  IF INITIAL I=5        THE PERIOD WILL BE 10
c
      i=i*mi
      i=i-1000*(i/1000)

      rnoise=real(i-500)+aj*anoise

      return
      end
      subroutine sobol1(n,x)
c
c***********************************************************************
c
      integer maxbit
      parameter (maxbit=30)
c
      integer maxdim
      parameter (maxdim=6)
c
      integer n
c
      real fac
      integer i
      integer im
      integer in
      integer ip(maxdim)
      integer ipp
      integer iu(maxdim,maxbit)
      integer iv(maxdim*maxbit)
      integer ix(maxdim)
      integer j
      integer jj
      integer k
      integer l
      integer mdeg(maxdim)
      real x(n)
c
      equivalence (iv,iu)
c
      save fac
      save in
      save ip
      save ix
      save iv
      save mdeg
c
      data ip / 0,1,2,2,1,4 /

      data mdeg / 1,2,3,3,4,4 /
c
      if (n.lt.0)then
        iv(1)=1
        iv(2)=1
        iv(3)=1
        iv(4)=1
        iv(5)=1
        iv(6)=1
        iv(7)=3
        iv(8)=1
        iv(9)=3
        iv(10)=3
        iv(11)=1
        iv(12)=1
        iv(13)=5
        iv(14)=7
        iv(15)=7
        iv(16)=3
        iv(17)=3
        iv(18)=5
        iv(19)=15
        iv(20)=11
        iv(21)=5
        iv(22)=15
        iv(23)=13
        iv(24)=9

        do k=1, maxdim

          ix(k)=0

          do j=1, mdeg(k)
            iu(k,j)=iu(k,j)*2**(maxbit-j)
          enddo

          do j=mdeg(k)+1, maxbit

            ipp=ip(k)
            i=iu(k,j-mdeg(k))
            i=ieor(i,i/2**mdeg(k))

            do l=mdeg(k)-1, 1, -1
              if (iand(ipp,1).ne.0) i=ieor(i,iu(k,j-l))
              ipp=ipp/2
            enddo

            iu(k,j)=i

          enddo

        enddo

        fac=1.0/2.0**maxbit
        in=0

      else

        im=in

        do j=1, maxbit
          jj=j
          if (iand(im,1).eq.0) go to 60
          im=im/2
        enddo

        write(*,*)' '
        write(*,*)'SOBOL1 - Fatal error!'
        write(*,*)'  MAXBIT=',maxbit
        write(*,*)'  is too small!'

        return

   60   continue

        im=(jj-1)*maxdim
        do k=1, min(n,maxdim)
          ix(k)=ieor(ix(k),iv(im+k))
          x(k)=ix(k)*fac
        enddo

        in=in+1

      endif

      return
      end
      function tha1(x,fx)
c
c***********************************************************************
c
c  ALGORITHM AS 76  APPL. STATIST. (1974) VOL.23, NO.3
c
c  Calculates the T(H,A) function of Owen, using Gaussian quadrature.
c  Incorporates correction AS R30 (vol.28, no.1, 1979)
c
c  T(H,A)=1/(2*PI) * INTEGRAL(0 to A) EXP[(-0.5*H*H)*(1+X*X)]/(1+X*X)
c
c  X      Input, REAL X, the variable usually called "H".
c
c  FX     Input, REAL FX, the variable usually called "A".
c
c  THA1   Output, REAL THA1, the value of the Owen function T(H,A).
c
      real quart
      parameter (quart=0.25)
c
      real fx
      real fxs
      real half
      integer i
      integer ng
      real r(5)
      real r1
      real r2
      real rt
      real tha1
      real tp
      real tv1
      real tv2
      real tv3
      real tv4
      real two
      real u(5)
      real x
      real x1
      real x2
      real xs
c
      data u / 0.0744372,0.2166977,0.3397048,0.4325317,0.4869533 /
      data r / 0.1477621,0.1346334,0.1095432,0.0747257,0.0333357 /
      data ng,tp,tv1,tv2,tv3,tv4 / 5,0.159155,1.e-35,15.0,15.0,1.e-5 /
      data half,two / 0.5,2.0 /
c
c  Test for X near zero
c
      if (abs(x).lt.tv1)then
        tha1=tp*atan(fx)
        return
      endif
c
c  Test for large values of abs(X)
c
      if (abs(x).gt.tv2)then
        tha1=0.0
        return
      endif
c
c  Test for FX near zero
c
      if (abs(fx).lt.tv1)then
        tha1=0.0
        return
      endif
c
c  Test whether abs(FX) is so large that it must be truncated
c
      xs=-half*x*x
      x2=fx
      fxs=fx*fx
      if (log(1.0+fxs)-xs*fxs.lt.tv3) go to 20
c
c  Computation of truncation point by Newton iteration
c
      x1=half*fx
      fxs=quart*fxs
   10 rt=fxs+1.0
      x2=x1+(xs*fxs+tv3-log(rt))/(2.0*x1*(1.0/rt-xs))
      fxs=x2*x2

      if (abs(x2-x1).ge.tv4)then
        x1=x2
        go to 10
      endif
c
c  Gaussian quadrature
c
   20 rt=0.0

      do i=1, ng
        r1=1.0+fxs*(half+u(i))**2
        r2=1.0+fxs*(half-u(i))**2
        rt=rt+r(i)*(exp(xs*r1)/r1+exp(xs*r2)/r2)
      enddo

      tha1=rt*x2*tp
      return
      end
      function tha2(h1,h2,a1,a2)
c
c***********************************************************************
c
c  AS R55  APPL. STATIST. (1985) VOL.34, NO.1
c
c  A remark on AS 76
c  Incorporating improvements in AS R80 (Appl. Statist. (1989)
c  vol.38, no.3)
c
c  Computes T(H1/H2, A1/A2) for any real numbers H1, H2, A1 and A2
c
c  Auxiliary function required: DNORM3(= AS 66) and THA1(= AS 76)
c
c  H1,
c  H2    Input, REAL H1, H2, two quantities which define the usual
c        H parameter, H=H1/H2.  H2 may be zero.
c
c  A1,
c  A2    Input, REAL A1, A2, two quantities which define the usual
c        A parameter, A=A1/A2.  A2 may be zero.
c
c  THA2  Output, REAL THA2, Owen's T(H,A) function, THA2=T(H1/H2,A1/A2).
c
c
      real pi
      parameter (pi=3.14159265358979323846)
c
      real a
      real a1
      real a2
      real dnorm3
      real g
      real h
      real h1
      real h2
      real tha1
      real tha2
      real lam,ex,c1,c2,ah,two,
     &  pt3,seven,half,quart
c
      data two / 2.0 /,pt3 / 0.3 /,seven / 7.0
     &  /,half / 0.5 /,quart / 0.25 /
c
      if (h2.eq.0.0)then
        tha2=0.0
        return
      endif
c
      h=h1/h2
      if (a2.ne.0.0)then
        a=a1/a2
        if ((abs(h).lt.pt3).and.(abs(a).gt.7.0))then
          lam=abs(a*h)
          ex=exp(-lam*lam/2.0)
          g=dnorm3(lam,.false.)
          c1=(ex/lam+sqrt(2.0*pi)*(g-0.5))/(2.0*pi)
          c2=((lam*lam+2.0)*ex/lam**3+sqrt(2.0*pi)*(g-0.5))/(12.0*pi)
          ah=abs(h)
          tha2=quart-c1*ah+c2*ah**3
          tha2=sign(tha2,a)
        else
          tha2=tha1(h,a)
        endif

        return

      endif
c
      g=dnorm3(h,.false.)

      if (h.lt.0.0)then
        tha2=g/2.0
      else
        tha2=(1.0-g)/2.0
      endif

      if (a1.lt.0.0) tha2=-tha2

      return
      end
      function trigam(x,ifault)
c
c***********************************************************************
c
c  Algorithm as121   Appl. Statist. (1978) vol 27, no. 1.
c  Calculates trigamma(x)=d**2(log(gamma(x))) / dx**2
c
      real a
      real b
      real b2
      real b4
      real b6
      real b8
      real half
      integer ifault
      real trigam
      real x
      real y
      real z
c
      data a,b,half / 1.0e-4,5.0,0.5 /
c
c  b2, b4, b6 and b8 are Bernoulli numbers
c
      data b2,b4,b6,b8 / 0.1666666667d0,-0.03333333333d0,0.02380952381,-
     &  0.03333333333 /
c
c  check for positive value of x
c
      trigam=0.0
      ifault=1
      if (x.le.0.0) return
      ifault=0
      z=x
c
c  Use small value approximation if x .le. a
c
      if (z.le.a)then
        trigam=1.0/(z*z)
        return
      endif
c
c  Increase argument to (x+i) .ge. b
c
   10 continue

      if (z.lt.b) then
        trigam=trigam+1.0/(z*z)
        z=z+1.0
        go to 10
      end if
c
c  Apply asymptotic formula if argument .ge. b
c
      y=1.0/(z*z)
      trigam=trigam+half*y+(1.0+y*(b2+y*(b4+y*(b6+y*b8))))/z
      return
      end
      subroutine unisph(n,w)
c
c***********************************************************************
c
c  UNIT generates a random vector of N entries, which may be thought
c  of as a point on the unit sphere.  The points generated are distribut
c  uniformly on the unit sphere.
c
      integer n
c
      integer i
      real w(n)
      real ww
c
c  Get N Gaussian numbers.
c
      call gau5(w,n)
c
c  Compute normalizer
c
      ww=0.0
      do i=1, n
        ww=ww+w(i)**2
      enddo

      ww=1.0/sqrt(ww)
c
c  Normalize
c
      do i=1, n
        w(i)=ww*w(i)
      enddo

      return
      end
      function xchi1(p,nfree)
c
c***********************************************************************
c
c  Compute the chi square distributed value X with probability P.
c
      real eps
      parameter (eps=0.000001)
c
      real chimax
      real chimin
      real chival
      integer nfree
      real p
      real pchi1
      real xchi1
c
      chimax=99999.0
      chimin=0.0
c
      if (p.le.0.0)then
        xchi1=chimin
        return
      elseif(p.ge.1.0)then
        xchi1=chimax
        return
      endif
c
      chival=real(nfree)/sqrt(p)

   10 continue

      if (abs(chimax-chimin).gt.eps*(chimax+chimin))then
        if (pchi1(chival,nfree).lt.p)then
          chimin=chival
        else
          chimax=chival
        endif
        chival=0.5*(chimax+chimin)
        go to 10
      endif

      xchi1=chival
      return
      end
      function xchi2(p,v,ifault)
c
c***********************************************************************
c
c  Algorithm AS 91   Applied Statistics (1975) Vol.24, P.35
c
c  XCHI2 evaluate the percentage points of the chi-squared probability
c  distribution function.  Given a probability P and degrees of freedom
c  V, XCHI2 returns a value X so that the chi squared probability of a
c  value no greater than X is P.
c
c  P      Input, REAL P, is the probability level.  P must lie in the ra
c         0.000002 to 0.999998,
c
c  V      Input, REAL V, is the number of degrees of freedom.  Note that
c         although V is ordinarily an integer value, it need not be so f
c         this routine.  V must be positive.
c
c  IFAULT Output, INTEGER IFAULT.  Error flag.
c         0, no error.
c         1, P>0.999998
c         2, P<0.000002
c         3, error in GAMIC1.
c
c  XCHI2  Output, REAL XCHI2, a value X such that the chi squared probab
c         with V degrees of freedom of a value less than or equal to X i
c
      real half
      real two
c
      parameter (half=0.5)
      parameter (two=2.0)
c
      real aa
      real e
      real g
      real gamic1
      real gamln4
      integer if1
      integer ifault
      real p
      real pmax
      real pmin
      real v
      real xchi2
      real xmax
      real xmin
      real xnorm2
      real xval

      real three,six
      real c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,
     &  c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31,c32,c33,
     &  c34,c35,c36,c37,c38
      real a,b,c,ch,p1,p2,q,s1,s2,s3,s4,s5,s6,t,x,xx
c
      data aa,pmin,pmax / 0.6931471806,0.000002,0.999998 /
      data three,six / 3.0,6.0 /
      data c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,
     &  c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31,c32,c33,
     &  c34,c35,c36,c37,c38 / 0.01,0.222222,0.32,0.4,1.24,2.2,4.67,6.66,
     &  6.73,13.32,60.0,70.0,84.0,105.0,120.0,127.0,140.0,1175.0,210.0,
     &  252.0,2264.0,294.0,346.0,420.0,462.0,606.0,672.0,707.0,735.0,
     &  889.0,932.0,966.0,1141.0,1182.0,1278.0,1740.0,2520.0,5040.0 /
c
c  Test arguments and initialise
c
      xchi2 = -1.0
      ifault=1
      if (p.lt.pmin.or.p.gt.pmax) return
c
      if (v.le.0.0)then
        write(*,*)' '
        write(*,*)'XCHI2 - Fatal error!'
        write(*,*)'  Input V <= 0.'
        write(*,*)'  V=',v
        ifault=2
        return
      endif
c
      temp=v/2.0
      g=gamln4(temp,ifault)
      e=100.0*r1mach(4)
      ifault=0
      xx=half*v
      c=xx-1.0
c
c  starting approximation for small chi-squared
c
      if (v.lt.-c5*log(p)) then

        ch=(p*xx*exp(g+xx*aa))**(1.0/xx)

        if (ch.lt.e)then
          xchi2=ch
          return
        endif

        go to 40

      end if
c
c  starting approximation for v less than or equal to 0.32
c
   10 if (v.gt.c3) go to 30
      ch=c4
      a=log(1.0-p)

   20 q=ch
      p1=1.0+ch*(c7+ch)
      p2=ch*(c9+ch*(c8+ch))
      t=-half+(c7+2.0*ch)/p1-(c9+ch*(c10+three*ch))/p2
      ch=ch-(1.0-exp(a+g+half*ch+c*aa)*p2/p1)/t
      if (abs(q/ch-1.0).gt.c1) go to 20
      go to 40
c
c  call to algorithm AS 111 - note that p has been tested above.
c  AS 241 could be used as an alternative.
c
   30 x=xnorm2(p)
c
c  Starting approximation using Wilson and Hilferty estimate
c
      p1=c2/v
      ch=v*(x*sqrt(p1)+1.0-p1)**3
c
c  Starting approximation for p tending to 1
c
      if (ch.gt.c6*v+six) ch=-2.0*(log(1.0-p)-c*log(half*ch)+g)
c
c  call to algorithm AS 147 and calculation of seven term Taylor series
c
   40 continue

      q=ch
      p1=half*ch
      if1=0

      p2=p-gamic1(p1,xx,if1)

      if (if1.ne.0)then
        ifault=3
        return
      endif

      t=p2*exp(xx*aa+g+p1-c*log(ch))
      b=t/ch
      a=half*t-b*c
      s1=(c19+a*(c17+a*(c14+a*(c13+a*(c12+c11*a)))))/c24
      s2=(c24+a*(c29+a*(c32+a*(c33+c35*a))))/c37
      s3=(c19+a*(c25+a*(c28+c31*a)))/c37
      s4=(c20+a*(c27+c34*a)+c*(c22+a*(c30+c36*a)))/c38
      s5=(c13+c21*a+c*(c18+c26*a))/c37
      s6=(c15+c*(c23+c16*c))/c38
      ch=ch+t*(1.0+half*t*s1-b*c*(s1-b*(s2-b*(s3-b*(s4-b*(s5-b*s6)))))
     &  )
      if (abs(q-ch).gt.e*ch) go to 40
c
      xchi2=ch
      return
      end
      function xnorm1(p)
c
c***********************************************************************
c
c  Compute the normally distributed value X with given probability P.
c
      real dnorm1
      real eps
      integer it
      real p
      real pmax
      real pmin
      real pval
      real r1mach
      real xmax
      real xmin
      real xnorm1
      real xval
c
      external r1mach
c
      eps=100.0*r1mach(4)
c
      xmax=99999.0
      pmax=1.0
      xmin=-99999.0
      pmin=0.0
c
      if (p.le.0.0)then
        xnorm1=-r1mach(2)
        return
      elseif(p.ge.1.0)then
        xnorm1=r1mach(2)
        return
      endif
c
      xval=0.0
      pval=0.5
      it=0

   10 continue

      it=it+1
      if (abs(pval-p).le.eps)then
        xnorm1=xval
        return
      endif

      if (abs(xmax-xmin).gt.eps*(abs(xmax)+abs(xmin)))then
        pval=dnorm1(xval)
        if (pval.lt.p)then
          xmin=xval
          pmin=pval
        else
          xmax=xval
          pmax=pval
        endif
        xval=xmin+(p-pmin)*(xmax-xmin)/(pmax-pmin)
        go to 10
      endif

      xnorm1=xval

      return
      end
      function xnorm2(p)
c
c***********************************************************************
c
c       ALGORITHM AS 111, APPL.STATIST., VOL.26, 118-121, 1977.
c
c       PRODUCES NORMAL DEVIATE CORRESPONDING TO LOWER TAIL AREA=P.
c
c      See also AS 241 which contains alternative routines accurate to
c      about 7 and 16 decimal digits.
c
c
c
      real half
      parameter (half=0.5)
c
      real split
      parameter (split=0.42)
c
      real a0
      real a1
      real a2
      real a3
      real b1
      real b2
      real b3
      real b4
      real c0
      real c1
      real c2
      real c3
      real d1
      real d2
      real p
      real q
      real r
      real r1mach
      real xnorm2
c
      data a0,a1,a2,a3 / 2.50662823884,-18.61500062529,41.39119773534,-
     &  25.44106049637 /,b1,b2,b3,b4 / -8.47351093090,23.08336743743,-
     &  21.06224101826,3.13082909833 /,c0,c1,c2,c3 / -2.78718931138,-
     &  2.29796479134,4.85014127135,2.32121276858 /,d1,d2 /
     &   3.54388924762,1.63706781897 /
c
      if (p.le.0.0)then
        xnorm2=-r1mach(2)
        return
      endif
c
      if (p.ge.1.0)then
        xnorm2=r1mach(2)
        return
      endif
c
      q=p-half
      if (abs(q).gt.split) go to 10
c
c       0.08 < P < 0.92
c
      r=q*q
      xnorm2=q*(((a3*r+a2)*r+a1)*r+a0)/((((b4*r+b3)*r+b2)*r+b1)*r+1.0)
      return
c
c       P < 0.08 OR P > 0.92, SET R=MIN(P,1-P)
c
   10 continue

      r=p
      if (q.gt.0.0) r=1.0-p
      r=sqrt(-log(r))
      xnorm2=(((c3*r+c2)*r+c1)*r+c0)/((d2*r+d1)*r+1.0)
      if (q.lt.0.0) xnorm2=-xnorm2

      return
      end
      function xnorm3(p)
c
c***********************************************************************
c
c      ALGORITHM AS241  APPL. STATIST. (1988) VOL. 37, NO. 3, 477-
c      484.
c
c      Produces the normal deviate Z corresponding to a given lower
c      tail area of P; Z is accurate to about 1 part in 10**7.
c
c      The hash sums below are the sums of the mantissas of the
c      coefficients.   They are included for use in checking
c      transcription.
c
      real const1
      real const2
      real half
      real split1
      real split2
c
      parameter (const1=0.180625)
      parameter (const2=1.6)
      parameter (half=0.5)
      parameter (split1=0.425)
      parameter (split2=5.0)
c
      real a0
      real a1
      real a2
      real a3
      real b1
      real b2
      real b3
      real c0
      real c1
      real c2
      real c3
      real d1
      real d2
      real e0
      real e1
      real e2
      real e3
      real f1
      real f2
      real p
      real q
      real r
      real r1mach
      real xnorm3
c
c      Coefficients for P close to 0.5
c
      parameter (a0=3.3871327179e+00,a1=5.0434271938e+01,a2=
     &  1.5929113202e+02,a3=5.9109374720e+01)
      parameter (b1=1.7895169469e+01,b2=
     &  7.8757757664e+01,b3=6.7187563600e+01)
c
c      Coefficients for P not close to 0, 0.5 or 1.
c
      parameter (c0=1.4234372777e+00,c1=2.7568153900e+00,c2=
     &  1.3067284816e+00,c3=1.7023821103e-01,d1=7.3700164250e-01,d2=
     &  1.2021132975e-01)
c
c      Coefficients for P near 0 or 1.
c
      parameter (e0=6.6579051150e+00,e1=3.0812263860e+00,e2=
     &  4.2868294337e-01,e3=1.7337203997e-02,f1=2.4197894225e-01,f2=
     &  1.2258202635e-02)
c
      if (p.le.0.0)then
        xnorm3=-r1mach(2)
        return
      endif
c
      if (p.ge.1.0)then
        xnorm3=r1mach(2)
        return
      endif
c
      q=p-half

      if (abs(q).le.split1)then

        r=const1-q*q
        xnorm3=q*(((a3*r+a2)*r+a1)*r+a0)/(((b3*r+b2)*r+b1)*r+1.0)

      else

        if (q.lt.0.0)then
          r=p
        else
          r=1.0-p
        endif

        r=sqrt(-log(r))

        if (r.le.split2)then
          r=r-const2
          xnorm3=(((c3*r+c2)*r+c1)*r+c0)/((d2*r+d1)*r+1.0)
        else
          r=r-split2
          xnorm3=(((e3*r+e2)*r+e1)*r+e0)/((f2*r+f1)*r+1.0)
        endif

        if (q.lt.0.0) xnorm3=-xnorm3

      endif

      return
      end
      function xnorm4(p)
c
c***********************************************************************
c
c      ALGORITHM AS241  APPL. STATIST. (1988) VOL. 37, NO. 3
c
c      Produces the normal deviate Z corresponding to a given lower
c      tail area of P; Z is accurate to about 1 part in 10**16.
c
c      The hash sums below are the sums of the mantissas of the
c      coefficients.   They are included for use in checking
c      transcription.
c
      real const1
      real const2
      real half
      real split1
      real split2
c     
      parameter (const1=0.1806250)
      parameter (const2=1.60)
      parameter (half=0.5)
      parameter (split1=0.4250)
      parameter (split2=5.0)
c
      real a0
      real a1
      real a2
      real a3
      real a4
      real a5
      real a6
      real a7
      real b1
      real b2
      real b3
      real b4
      real b5
      real b6
      real b7
      real c0
      real c1
      real c2
      real c3
      real c4
      real c5
      real c6
      real c7
      real d1
      real d2
      real d3
      real d4
      real d5
      real d6
      real d7
      real e0
      real e1,e2,e3,e4,e5,e6,e7
      real f1,f2,f3,f4,f5,f6,f7
      real p
      real q
      real r
      real r1mach
      real xnorm4
c
c      Coefficients for P close to 0.5
c
      parameter (a0=3.38713287279636660800)
      parameter (a1=1.3314166789178437745e+2,
     &  a2=1.9715909503065514427e+3,a3=1.3731693765509461125e+4,a4=
     &  4.5921953931549871457e+4,a5=6.7265770927008700853e+4,a6=
     &  3.3430575583588128105e+4,a7=2.5090809287301226727e+3,b1=
     &  4.2313330701600911252e+1,b2=6.8718700749205790830e+2,b3=
     &  5.3941960214247511077e+3,b4=2.1213794301586595867e+4,b5=
     &  3.9307895800092710610e+4,b6=2.8729085735721942674e+4,b7=
     &  5.2264952788528545610e+3)
c
c      Coefficients for P not close to 0, 0.5 or 1.
c
      parameter (c0=1.423437110749683577340,c1=4.630337846156545295900,
     &  c2=5.769497221460691405500,c3=3.647848324763204605040,c4=
     &  1.270458252452368382580,c5=2.41780725177450611770e-1,c6=
     &  2.27238449892691845833e-2,c7=7.74545014278341407640e-4,d1=
     &  2.053191626637758821870,d2=1.676384830183803849400,d3=
     &  6.89767334985100004550e-1,d4=1.48103976427480074590e-1,d5=
     &  1.51986665636164571966e-2,d6=5.47593808499534494600e-4,d7=
     &  1.05075007164441684324e-9)
c
c      Coefficients for P near 0 or 1.
c
      parameter (e0=6.657904643501103777200,e1=5.463784911164114369900,
     &  e2=1.784826539917291335800,e3=2.96560571828504891230e-1,e4=
     &  2.65321895265761230930e-2,e5=1.24266094738807843860e-3,e6=
     &  2.71155556874348757815e-5,e7=2.01033439929228813265e-7,f1=
     &  5.99832206555887937690e-1,f2=1.36929880922735805310e-1,f3=
     &  1.48753612908506148525e-2,f4=7.86869131145613259100e-4,f5=
     &  1.84631831751005468180e-5,f6=1.42151175831644588870e-7,f7=
     &  2.04426310338993978564e-15)
c
      if (p.le.0.0)then
        xnorm4=-r1mach(2)
        return
      endif

      if (p.ge.1.0)then
        xnorm4=r1mach(2)
        return
      endif

      q=p-half

      if (abs(q).le.split1)then

        r=const1-q*q
        xnorm4=q*(((((((a7*r+a6)*r+a5)*r+a4)*r+a3)*r+a2)*r+a1)*r+a0)/
     &    (((((((b7*r+b6)*r+b5)*r+b4)*r+b3)*r+b2)*r+b1)*r+1.0)

      else

        if (q.lt.0.0)then
          r=p
        else
          r=1.0-p
        endif

        r=sqrt(-log(r))

        if (r.le.split2)then
          r=r-const2
          xnorm4=(((((((c7*r+c6)*r+c5)*r+c4)*r+c3)*r+c2)*r+c1)*r+c0)/
     &      (((((((d7*r+d6)*r+d5)*r+d4)*r+d3)*r+d2)*r+d1)*r+1.0)
        else
          r=r-split2
          xnorm4=(((((((e7*r+e6)*r+e5)*r+e4)*r+e3)*r+e2)*r+e1)*r+e0)/
     &      (((((((f7*r+f6)*r+f5)*r+f4)*r+f3)*r+f2)*r+f1)*r+1.0)
        endif

        if (q.lt.0.0)then
          xnorm4=-xnorm4
        endif

      endif

      return
      end
