      program main

c*********************************************************************72
c
cc MAIN is the main program for ZERO_PRB.
c
c  Modified:
c
c    11 April 2008
c
      write ( *, * ) ' '
      write ( *, * ) 'ZERO_PRB'
      write ( *, * ) '  FORTRAN77 version.'
      write ( *, * ) '  Tests for the zero finding routines in ZERO.'

      call test01
      call test02
      call test03
      call test04
      call test05
      call test06
      call test07
      call test08
      call test09
      call test10
      call test11
      call test12
      call test13

      write ( *, * ) ' '
      write ( *, * ) 'ZERO_PRB'
      write ( *, * ) '  Normal end of execution.'

      stop
      end
      subroutine test01
c
c***********************************************************************
c
c  call rootna
c
      external parab
c
      abserr=0.00001
      a=-2.1
      b=4.0
      write(*,*)' '
      write(*,*)'test01'
      write(*,*)'test subroutine rootna'
      write(*,*)' '
      write(*,*)'interval is ',a,b
      x=rootna(a,b,abserr,parab)
      write(*,*)' '
      write(*,*)'estimate for root=',x
      fx=parab(x)
      write(*,*)'function value at root=',fx
      return
      end
      subroutine test02
c
c***********************************************************************
c
cc test02 tests zeroin.
c
      real parab
      external parab

      abserr=0.00001
      a=-2.1
      b=4.0
      write(*,*)' '
      write(*,*)'test02'
      write(*,*)'test subroutine zeroin'

      write ( *, * ) ' '
      write ( *, * ) '  Initial interval is [ ', a, ', ', b, '].'
      write ( *, * ) '  F(A) = ', parab ( a )
      write ( *, * ) '  F(B) = ', parab ( b )
      write ( *, * ) ' '
      write ( *, * ) '  Tolerance:'
      write ( *, * ) '  ABSERR = ', abserr

      call zeroin ( a, b, parab, abserr, z, kount )

      write(*,*)' '
      write(*,*)'estimate for root Z =', z
      write(*,*)'  F(Z) = ', parab ( z )

      write(*,*)'number of steps=',kount
      return
      end
      subroutine test03
c
c***********************************************************************
c
c  call function root.  note that we must call root twice with the
c  two endpoint values to get started.
c
      character*17 name(5)
      character*17 meth
c
      name(1)='unspecified'
      name(2)='bisection'
      name(3)='secant'
      name(4)='inverse quadratic'
      name(5)='muller'
      meth='starting value'
      write(*,*)' '
      write(*,*)'test03'
      write(*,*)'test function root'
      write(*,*)' '
      write(*,*)'        root            function          method'
      write(*,*)' '
c
c  tell program we are beginning
c
      abserr=0.00001
      kount=0
      x1=-2.1
      fx1=parab(x1)
      write(*,1010)x1,fx1,meth
      xnew=root(x1,fx1,xerr,estd,ierr,kount,method)
      x2=4.0
      fx2=parab(x2)
      write(*,1010)x2,fx2,meth
      xnew=root(x2,fx2,xerr,estd,ierr,kount,method)
      do 10 i=1,30
        x=xnew
        fx=parab(xnew)
        write(*,1010)x,fx,name(method+1)
        if(abs(fx).lt.abserr)go to 20
        xnew=root(x,fx,xerr,estd,ierr,kount,method)
        if(abs(x-xnew).lt.abserr)go to 20
        if(ierr.ne.0)go to 30
   10   continue
c
c  used up iterations without convergence
c
      write(*,*)'reached iteration limit without convergence.'
      return
c
c  convergence
c
   20 continue
      write(*,*)'tolerance was satisfied.'
      write(*,*)'number of steps=',kount
      return
c
c  error return
c
   30 continue
      write(*,*)'error return, with ierr=',ierr
      return
 1010 format(1x,2g18.10,3x,a17)
 1030 format(' value of f(x)=',g18.10)
      end
      subroutine test04
c
c***********************************************************************
c
c  call rootsg
c
      character*17 name(5)
      character*17 meth
c
      abserr=0.00001
      name(1)='unspecified'
      name(2)='bisection'
      name(3)='secant'
      name(4)='inverse quadratic'
      name(5)='muller'
      meth='starting value'
      b=-2.1
      fb=parab(b)
      relerr=abserr
      c=4.0
      fc=parab(c)
      iflag=0
      write(*,*)' '
      write(*,*)'test04'
      write(*,*)'test rootsg'
      write(*,*)' '
      write(*,1010)
      write(*,*)' '
      write(*,1040)b,fb,meth
      write(*,1040)c,fc,meth
c
c  first three calls initialize, set f(c) and f(b)
c
      do 10 i=1,30
        call rootsg(t,ft,b,c,relerr,abserr,iflag,method)
        ft=parab(t)
        meth=name(method+1)
        write(*,1040)t,ft,meth
        if(abs(ft).lt.abserr)go to 30
        if(iflag.gt.0)go to 20
   10   continue
c
c  used up iterations without convergence
c
      write(*,1050)
      write(*,1060)
      return
c
c  termination or error
c
   20 continue
      write(*,*)'iflag=',iflag
c
c  convergence
c
   30 continue
      write(*,*)'tolerance satisfied.'
      return
 1010 format('        root            function          method')
 1020 format(' current interval ',g18.10,' to ',g18.10)
 1030 format(' estimate for root=',g18.10)
 1040 format(1x,2g18.10,3x,a17)
 1050 format(' used up the number of iterations')
 1060 format(' without accepting root')
      end
      subroutine test05
c
c***********************************************************************
c
c  call rootjb
c
      character*17 name(5)
      character*17 meth
c
      name(1)='unspecified'
      name(2)='bisection'
      name(3)='secant'
      name(4)='inverse quadratic'
      name(5)='muller'
      meth='starting value'
      write(*,*)' '
      write(*,*)'test05'
      write(*,*)'test rootjb'
      write(*,*)' '
      write(*,1050)
      write(*,*)' '
      abserr=0.00001
      a=-2.1
      fa=parab(a)
      write(*,1060)a,fa,meth
      b=4.0
      fb=parab(b)
      write(*,1060)b,fb,meth
      kount=0
      do 10 i=1,30
        call rootjb(a,fa,b,fb,kount,iflag,method)
        if(iflag.eq.-2.or.iflag.eq.-3)go to 20
        fa=parab(a)
        meth=name(method+1)
        write(*,1060)a,fa,meth
        if(abs(fa).le.abserr)then
          write(*,*)'tolerance satisfied.'
          write(*,1010)kount
          return
          endif
   10   continue
c
c  used up iterations without convergence
c
      write(*,1080)
      write(*,1090)
      return
c
c  error during iteration
c
   20 continue
      write(*,*)'rootjb returned nonzero iflag=',iflag
      return
 1000 format(1x,3g14.6)
 1010 format(' number of steps=',i6)
 1040 format(' current interval is ',g18.10,' to ',g18.10)
 1050 format('        root            function          method')
 1060 format(1x,2g18.10,3x,a17)
 1070 format(' value of f(x)=',g18.10)
 1080 format(' used up the number of iterations')
 1090 format(' without accepting root')
      end
      subroutine test06
c
c***********************************************************************
c
c  call fzero
c
      external parab
c
      abserr=0.00001
      relerr=abserr
      b=-2.1
      fb=parab(b)
      c=4.0
      fc=parab(c)
      r=c
      iflag=0
c
      write(*,*)' '
      write(*,*)'test06'
      write(*,*)'test subroutine fzero.'
      write(*,*)' '
      write(*,*)'        root            function'
      write(*,*)' '
      write(*,'(1x,2g18.10)')b,fb
      write(*,'(1x,2g18.10)')c,fc
c
      call fzero(parab,b,c,r,relerr,abserr,iflag)
      ft=parab(b)
      write(*,'(1x,2g18.10)')b,ft
      if(abs(ft).lt.abserr)then
        write(*,*)'tolerance satisfied.'
      else
        write(*,*)'tolerance not satisfied!'
        endif
      write(*,*)'value of iflag returned is ',iflag
      return
      end
      subroutine test07
c
c  test czero, which finds zeroes of a polynomial
c
      parameter (nd=4)
      parameter (nw=6*nd+32)
c
      complex p(nd)
      complex poly
      complex w(nw)
      complex z(nd)
c
c  polynomial is 24 - 50 * x + 35 * x**2 - 10 * x**3 + x**4
c
      p(1)=cmplx( 24.0, 0.0)
      p(2)=cmplx(-50.0, 0.0)
      p(3)=cmplx( 35.0, 0.0)
      p(4)=cmplx(-10.0, 0.0)
c
      write(*,*)' '
      write(*,*)'test07'
      write(*,*)'test czero, which finds polynomial roots, on'
      write(*,*)'24 - 50 * x + 35 * x**2 - 10 * x**3 + x**4'
      write(*,*)' '
      call czero(z,p,nd,w)
c
      itemp=int(w(1))
      write(*,*)'czero found ',itemp,' zeroes of the polynomial:'
      write(*,*)' '
      write(*,*)' value of root        value of polynomial'
      write(*,*)' '
      do 10 i=1,itemp
        poly=(((z(i)+p(4))*z(i)+p(3))*z(i)+p(2))*z(i)+p(1)
        write(*,*)z(i),poly
10      continue
      return
      end
      subroutine test08
c
c***********************************************************************
c
c  call locmin for minimum of function
c
      external parab
c
      character*17 name(3)
      character*17 meth
c
      name(1)='unspecified'
      name(2)='golden search'
      name(3)='parabolic fit'
      a=-2.1
      b=4.0
      kount=0
      write(*,*)' '
      write(*,*)'test08'
      write(*,*)'call locmin to find minimum of a function.'
      write(*,*)' '
      meth='starting value'
      fa=parab(a)
      fb=parab(b)
      abserr=0.00001
      relerr=abserr
      write(*,1020)a,fa,meth
      write(*,1020)b,fb,meth
      iflag=0
      do 10 i=1,30
        call locmin(a,b,relerr,abserr,parab,xmin,fxmin,kount,method,
     1  iflag)
        write(*,1020)xmin,fxmin,name(method+1)
        if(iflag.ne.0)go to 20
   10   continue
   20 continue
      write(*,*)'number of steps=',kount
      write(*,*)'iflag=',iflag
      return
 1000 format('        minimum         function          method')
 1020 format(1x,2g18.10,3x,a17)
      end
      function parab(x)
c
c***********************************************************************
c
c  function to seek zero or minimum of.
c
      parab=(x+5.0)*(x-2.0)
      return
      end
      subroutine test09
c
c  solve the cubic x**3 - 3 * x**2 + x - 3 = 0
c  roots are +i, -i and 3.
c
      dimension b(3)
      dimension r(6)
c
      write(*,*)' '
      write(*,*)'test09'
      write(*,*)'test cubic which can find the complex roots of a real'
      write(*,*)'monic cubic polynomial.  the roots for the example are'
      write(*,*)'+i, -i and 3.'
      b(1)=-3.0
      b(2)=1.0
      b(3)=-3.0
      call cubic(b,r)
      write(*,*)' '
      write(*,*)'complex roots computed by cubic:'
      write(*,*)' '
      write(*,*)r(1),r(2)
      write(*,*)r(3),r(4)
      write(*,*)r(5),r(6)
      return
      end
      subroutine test10
c
      parameter (mdeg=5)
      parameter (mcoef=mdeg+1)
c
      double precision coef(mcoef)
      logical fail
      double precision zeror(mcoef)
      double precision zeroi(mcoef)
c
      write(*,*)' '
      write(*,*)'test10'
      write(*,*)'test drpoly, which finds the roots of a real'
      write(*,*)'polynomial, using double precision.'
c
      do 20 i=1,2

        if(i.eq.1)then
          ndeg=2
          coef(1)=1.0
          coef(2)=-3.0
          coef(3)=2.0
        elseif(i.eq.2)then
          ndeg=4
          coef(1)=1.0
          coef(2)=0.0
          coef(3)=0.0
          coef(4)=0.0
          coef(5)=1.0
          endif

        write(*,*)' '
        write(*,*)'this test involves a polynomial of degree ',ndeg

        call drpoly(coef,ndeg,nzero,zeror,zeroi,fail)

        if(fail)then
          write(*,*)' '
          write(*,*)'drpoly could not find all the roots.'
          write(*,*)'drpoly found only ',nzero,' roots, out of a total'
          write(*,*)'number of ',ndeg,' that were sought.'
        else
          write(*,*)' '
          write(*,*)'drpoly has found the full set of roots.'
          endif

        write(*,*)' '
        write(*,*)'here are the (possibly complex) roots:'
        write(*,*)' '
        do 10 j=1,nzero
          write(*,*)zeror(j),zeroi(j)
10        continue

20      continue

      return
      end
      subroutine test11
c
      parameter (mdeg=5)
      parameter (mcoef=mdeg+1)
c
      real coef(mcoef)
      logical fail
      real zeror(mcoef)
      real zeroi(mcoef)
c
      write(*,*)' '
      write(*,*)'test11'
      write(*,*)'test rpoly, which finds the roots of a real'
      write(*,*)'polynomial, using single precision.'
c
      do 20 i=1,2

        if(i.eq.1)then
          ndeg=2
          coef(1)=1.0
          coef(2)=-3.0
          coef(3)=2.0
        elseif(i.eq.2)then
          ndeg=4
          coef(1)=1.0
          coef(2)=0.0
          coef(3)=0.0
          coef(4)=0.0
          coef(5)=1.0
          endif

        write(*,*)' '
        write(*,*)'this test involves a polynomial of degree ',ndeg

        call rpoly(coef,ndeg,nzero,zeror,zeroi,fail)

        if(fail)then
          write(*,*)' '
          write(*,*)'rpoly could not find all the roots.'
          write(*,*)'rpoly found only ',nzero,' roots, out of a total'
          write(*,*)'number of ',ndeg,' that were sought.'
        else
          write(*,*)' '
          write(*,*)'rpoly has found the full set of roots.'
          endif

        write(*,*)' '
        write(*,*)'here are the (possibly complex) roots:'
        write(*,*)' '
        do 10 j=1,nzero
          write(*,*)zeror(j),zeroi(j)
10        continue

20      continue

      return
      end
      subroutine test12
c
      parameter (mdeg=5)
      parameter (mcoef=mdeg+1)
      parameter (mwork=(mdeg+1)*14)
c
      double precision coefr(mcoef)
      double precision coefi(mcoef)
      logical fail
      double precision work(mwork)
      double precision zeror(mcoef)
      double precision zeroi(mcoef)
c
      write(*,*)' '
      write(*,*)'test12'
      write(*,*)'test dcpoly, which finds the roots of a complex'
      write(*,*)'polynomial, using double precision.'
c
      do 20 i=1,2

        if(i.eq.1)then
          ndeg=2
          coefr(1)=1.0
          coefi(1)=0.0
          coefr(2)=-3.0
          coefi(2)=0.0
          coefr(3)=2.0
          coefi(3)=0.0
        elseif(i.eq.2)then
          ndeg=4
          coefr(1)=1.0
          coefi(1)=0.0
          coefr(2)=0.0
          coefi(2)=0.0
          coefr(3)=0.0
          coefi(3)=0.0
          coefr(4)=0.0
          coefi(4)=0.0
          coefr(5)=1.0
          coefi(5)=0.0
          endif

        write(*,*)' '
        write(*,*)'this test involves a polynomial of degree ',ndeg

        call dcpoly(coefr,coefi,ndeg,work,zeror,zeroi,fail)

        if(fail)then
          write(*,*)' '
          write(*,*)'dcpoly reports that an error occurred.'
          write(*,*)'some or all of the roots may be missing.'
        else
          write(*,*)' '
          write(*,*)'dcpoly has found the full set of roots.'
          endif

        write(*,*)' '
        write(*,*)'here are the roots:'
        write(*,*)' '
        do 10 j=1,ndeg
          write(*,*)zeror(j),zeroi(j)
10        continue

20      continue

      return
      end
      subroutine test13
c
      parameter (mdeg=5)
      parameter (mcoef=mdeg+1)
      parameter (mwork=(mdeg+1)*14)
c
      real coefr(mcoef)
      real coefi(mcoef)
      logical fail
      real work(mwork)
      real zeror(mcoef)
      real zeroi(mcoef)
c
      write(*,*)' '
      write(*,*)'test13'
      write(*,*)'test cpoly, which finds the roots of a complex'
      write(*,*)'polynomial, using single precision.'
c
      do 20 i=1,2

        if(i.eq.1)then
          ndeg=2
          coefr(1)=1.0
          coefi(1)=0.0
          coefr(2)=-3.0
          coefi(2)=0.0
          coefr(3)=2.0
          coefi(3)=0.0
        elseif(i.eq.2)then
          ndeg=4
          coefr(1)=1.0
          coefi(1)=0.0
          coefr(2)=0.0
          coefi(2)=0.0
          coefr(3)=0.0
          coefi(3)=0.0
          coefr(4)=0.0
          coefi(4)=0.0
          coefr(5)=1.0
          coefi(5)=0.0
          endif

        write(*,*)' '
        write(*,*)'this test involves a polynomial of degree ',ndeg

        call cpoly(coefr,coefi,ndeg,work,zeror,zeroi,fail)

        if(fail)then
          write(*,*)' '
          write(*,*)'cpoly reports that an error occurred.'
          write(*,*)'some or all of the roots may be missing.'
        else
          write(*,*)' '
          write(*,*)'cpoly has found the full set of roots.'
          endif

        write(*,*)' '
        write(*,*)'here are the roots:'
        write(*,*)' '
        do 10 j=1,ndeg
          write(*,*)zeror(j),zeroi(j)
10        continue

20      continue

      return
      end
