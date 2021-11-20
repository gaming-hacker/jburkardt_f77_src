c  zero.for  27 august 1991
c
      subroutine zeroin ( a, b, f, t, z, kount )

c*********************************************************************72
c
cc ZEROIN seeks the root of a function inside a change-of-sign interval.
c
c  Discussion:
c
c    The (continuous) function F has values F(A) and F(B) that are 
c    of opposite sign.  Therefore, there exists at least one value
c    C between A and B at which F(C) = 0.
c
c    This routine seeks to determine the value of C.
c
c  Modified:
c
c    11 April 2008
c
c  Author:
c
c    FORTRAN77 original version by Richard Brent.
c
c  Parameters:
c
      external f

      real    a
      real    b
      real    c
      real    d
      real    e
      real    f
      real    fa
      real    fb
      real    fc
      integer kount
      real    sa
      real    sb
      real    t
      real    z

      kount = 0

      if ( fa .eq. 0.0 ) then
        z = a
        return
      end if

      if ( fb .eq. 0.0 ) then
        z = b
        return
      end if

      epmach=r1mach(4)
      z=0.0
      sa=a
      sb=b
      fa=f(sa)
      fb=f(sb)

      if((fa.gt.0.0.and.fb.gt.0.0).or.
     &   (fa.lt.0.0.and.fb.lt.0.0))then
        write(*,*)'zeroin - fatal input error.'
        write(*,*)'         no sign change between f(a) and f(b).'
        return
        endif
c
   10 continue
      c=sa
      fc=fa
      e=sb-sa
      d=e
   20 continue
      if(abs(fc).lt.abs(fb))then
        sa = sb
        sb = c
        c = sa
        fa = fb
        fb = fc
        fc = fa
        endif
      tol=2.0*epmach*abs(sb)+t
      em=0.5*(c-sb)
c
c  see if we can accept the current estimate.
c
      if((abs(em).le.tol).or.(fb.eq.0.0))then
        z=sb
        return
        endif
c
c  see if bisection is forced.
c
      if((abs(e).ge.tol).and.(abs(fa).gt.abs(fb))) go to 40
      e=em
      d=e
      go to 80
c
c  linear interpolation
c
   40 continue
      if(sa.ne.c)go to 50
      s=fb/fa
      p=2.0*em*s
      q=1.0-s
      if(p.gt.0.0)then
        q=-q
      else
        p=-p
        endif
      go to 60
c
c  inverse quadratic interpolation
c
   50 continue
      s=fb/fa
      q=fa/fc
      r=fb/fc
      p=s*(2.0*em*q*(q-r)-(sb-sa)*(r-1.0))
      q=(q-1.0)*(r-1.0)*(s-1.0)
      if(p.gt.0.0)then
        q=-q
      else
        p=-p
        endif
   60 continue
      s=e
      e=d
      if((2.0*p.ge.3.0*em*q-abs(tol*q)).or.(p.ge.abs(0.5*s*q)))goto 70
      d=p/q
      go to 80
   70 continue
      e=em
      d=e
   80 continue
      sa=sb
      fa=fb
      if(abs(d).gt.tol)then
        sb=sb+d
      else
        if(em.gt.0.0)sb=sb+tol
        if(em.le.0.0)sb=sb-tol
        endif
      fb=f(sb)
      kount=kount+1
      if((fb.gt.0.0).and.(fc.gt.0.0))go to 10
      if((fb.le.0.0).and.(fc.le.0.0))go to 10
      go to 20

      end
      function root ( x, fx, xerr, estd, ierr, kount, method )

c*********************************************************************72
c
cc root ???
c
      parameter (itmax=80)
      parameter (ibisec=30)
c
      real    epmach
      real    q(8)
      real    root
c
      save    epmach
      save    q
c
      ierr=0
      method=0
      if(fx.eq.0.0)then
        xerr=0.0
        return
        endif
c
c  if first time then initialize
c
      if(kount.eq.0)then
        q(1) = fx
        q(2) = x
        do 10 i=3,8
          q(i)=0.0
   10     continue
        root=x+fx
        estd=1.0
        xerr=r1mach(2)
        epmach=r1mach(4)
        kount=1
        return
        endif
c
c  if too many iterations then error return
c
      kount = kount + 1
      if(kount.gt.itmax)then
        ierr=1
        xerr=0.0
        return
        endif
c
c  if repeated x then error return
c
      if((kount.ge.2.and.x.eq.q(4)).or.x.eq.q(2))then
        ierr=2
        xerr=0.0
        return
        endif
c
c  push new values down in q
c
      do 30 i=1,4
        q(7-i)=q(5-i)
   30   continue
      q(1)=fx
      q(2)=x
c
c  if change in sign then store opposite value
c
      if(q(1)*q(3).lt.0.0)then
        q(7)=q(3)
        q(8)=q(4)
        endif
c
c  calculate width of change-in-sign interval
c
      xerr=r1mach(2)
      if(q(7).ne.0.0)xerr=abs(q(8)-q(2))
c
c  if more than ibisec iterations and posible then bisect
c
      if(kount.gt.ibisec.and.q(7).ne.0.0)goto 50
      w = q(4) - q(2)
      v = (q(3) - q(1))/w
c
c  if three or more points then try muller
c
      method=4
      if(q(5).eq.0.0)goto 40
      u=(q(5)-q(3))/(q(6)-q(4))
      z=(q(6)-q(2))/w
      r=(z+1.0)*v-u
      if(r.eq.0.0) goto 40
      p = 2.0*z*q(1)/r
      d = 2.0*p/(w*r)*(v-u)
      if(d.lt.(-1.0)) goto 40
      sqr = sqrt(1.0+d)
      root = q(2) - p/(1.0+sqr)
c
c  estimate derivative at root based on three points
c
      if(min(abs(w),abs(q(4)-q(6))).gt.100.0*epmach*abs(q(2)))
     1 estd=abs(r*sqr/z)
c
c  if root is in correct interval then return
c
      if(q(7).eq.0.0)return
      if(q(2).lt.root.and.root.lt.q(8))return
      if(q(8).lt.root.and.root.lt.q(2))return
c
c  estimate derivative at root based on 2 points
c
   40 continue
      if(abs(w).gt.100.0*epmach*abs(q(2)))estd=abs(v)
c
c  apply secant scheme
c
      if(q(1).eq.q(3).and.q(7).ne.0.0) goto 50
      method=2
      if(q(1).eq.q(3))then
        ierror=3
        xerr=0.0
        return
        endif
      decr=q(1)/v
      root=q(2)-decr
c
c  if decrement falls out of precision use 8*epmach step
c
      if(root.eq.q(2))then
        method=0
        root=q(2)-sign(epmach*8.0*q(2),decr)
        endif
c
c  if root is in correct interval then return
c
      if(q(7).eq.0.0)return
      if(q(2).lt.root.and.root.lt.q(8))return
      if(q(8).lt.root.and.root.lt.q(2))return
c
c  apply bisection
c
   50 continue
      method=1
      root=q(2)+(q(8)-q(2))/2.0
      return
      end
      function rootna(y,z,t,f)

c*********************************************************************72
c
cc rootna ???
c
      external f
c
      real a,b,c,d,e,f,fl,fr,l,p,q,r,s,t,u,v,w,y,z
      real d2,d3,d4,fa,fb,fc,fd,p2,p3,p4,d34,d42,d23
c
      l = y
      r = z
      if ( l .le. r ) goto 10
      a = l
      l = r
      r = a
10    fl = f(l)
      fr = f(r)
      if ( sign(fl,fr) .eq. fl ) goto 220
      q = fl/abs(fl)
      s = t + t
      u = 1.
20    u = .5*u
      a = 1. + u
      if ( a .gt. 1. ) goto 20
      u = 5.*u
      v = .5*u
30    e = r - l
      if ( e .le. s ) goto 210
      if ( e .le. u*(abs(l)+abs(r)) ) goto 210
      if ( abs(fl) .gt. abs(fr) ) goto 40
      a = l
      fa = fl
      b = r
      fb = fr
      goto 50
40    a = r
      fa = fr
      b = l
      fb = fl
50    c = a - fa*(a-b)/(fa-fb)
      p = c
      w = amax1(t,v*(abs(l)+abs(r)))
      if ( abs(c-l) .lt. w ) c = l + w
      if ( abs(c-r) .lt. w ) c = r - w
      fc = f(c)
      if ( sign(fc,q) .eq. fc ) goto 60
      r = c
      fr = fc
      goto 70
60    l = c
      fl = fc
70    w = r - l
      if ( w .le. s ) goto 250
      if ( w .le. u*(abs(l)+abs(r)) ) goto 250
      if ( abs(fc) .ge. abs(fb) ) goto 90
      if ( abs(fc) .gt. abs(fa) ) goto 80
      w = c
      c = b
      b = a
      a = w
      w = fc
      fc = fb
      fb = fa
      fa = w
      goto 90
80    w = c
      c = b
      b = w
      w = fc
      fc = fb
      fb = w
90    if ( a .lt. l ) goto 190
      if ( a .gt. r ) goto 190
      call inp(d,a,b,c,fa,fb,fc,l,r)
100   p = d
      w = amax1(t,v*(abs(l)+abs(r)))
      if ( abs(l-d) .lt. w ) goto 110
      if ( abs(r-d) .gt. w ) goto 130
110   if ( d+d .gt. l+r ) goto 120
      d = l + w
      goto 130
120   d = r - w
130   if ( d .le. l ) goto 190
      if ( d .ge. r ) goto 190
      e = .5*e
      if ( e .lt. abs(a-d) ) goto 190
      fd = f(d)
      if ( sign(fd,q) .eq. fd ) goto 140
      r = d
      fr = fd
      goto 150
140   l = d
      fl = fd
150   w = r - l
      if ( w .le. s ) goto 250
      if ( w .le. u*(abs(l)+abs(r)) ) goto 250
      w = abs(fd)
      if ( w .le. abs(fa) ) goto 170
      if ( w .le. abs(fb) ) goto 160
      if ( w .ge. abs(fc) ) goto 180
      w = d
      d = c
      c = w
      w = fd
      fd = fc
      fc = w
      goto 180
160   w = d
      d = c
      c = b
      b = w
      w = fd
      fd = fc
      fc = fb
      fb = w
      goto 180
170   w = d
      d = c
      c = b
      b = a
      a = w
      w = fd
      fd = fc
      fc = fb
      fb = fa
      fa = w
180   d4 = d - a
      d3 = c - a
      d2 = b - a
      d34 = c - d
      d42 = d - b
      d23 = b - c
      p2 = 0.
      p3 = 0.
      p4 = 0.
      if ( d34 .ne. 0. ) p2 = 1./d34
      if ( d42 .ne. 0. ) p3 = 1./d42
      if ( d23 .ne. 0. ) p4 = 1./d23
      p2 = (fb-fa)/(d2*(1.+(d2/d3)*p2*d42+(d2/d4)*p2*d23))
      p3 = (fc-fa)/(d3*(1.+(d3/d2)*p3*d34+(d3/d4)*p3*d23))
      p4 = (fd-fa)/(d4*(1.+(d4/d2)*p4*d34+(d4/d3)*p4*d42))
      p2 = p2 + p3 + p4
      if ( p2 .eq. 0. ) goto 190
      d = a - fa/p2
      goto 100
190   d = .5*(l+r)
      fd = f(d)
      if ( sign(fd,q) .eq. fd ) goto 200
      r = d
      fr = fd
      goto 30
200   l = d
      fl = fd
      goto 30
210   rootna = .5*(l+r)
      return
220   if ( fl. eq. 0. ) goto 230
      if ( fr .eq. 0. ) goto 240
      write(*,*)'rootna - fatal error'
      write(*,*)'         function has same sign at both'
      write(*,*)'         starting points.'
      stop
230   rootna = l
      return
240   rootna = r
      return
250   if ( p .lt. l ) goto 210
      if ( p .gt. r ) goto 210
      rootna = p
      return
      end
      subroutine inp(a,x,y,z,u,v,w,l,r)

c*********************************************************************72
c
cc inp ???
c
      real a,b,c,l,p,q,r,s,t,u,v,w,x,y,z
c
      s = z - x
      t = (y-x)/s
      a = (u-v)/t + (w-v)/(1.-t)
      if ( a .eq. 0. ) goto 40
      b = .5*(w-u)/a - .5
      c = u/a
      t = sqrt(abs(c))
      if ( abs(b) .lt. sign(t,c) ) goto 60
      t = amax1(t,abs(b))
      if ( t .eq. 0. ) goto 50
      q = 1./t
      p = sqrt((q*b)**2 - q*c*q)
      p = t*p
      if ( abs(p+b) .gt. abs(p-b) ) goto 10
      q = p - b
      goto 20
10    q = -(b+p)
20    p = c/q
      q = x + s*q
      p = x + s*p
      if ( q .lt. l ) goto 30
      if ( q .gt. r ) goto 30
      a = q
      return
30    a = p
      return
40    if ( u .eq. w ) goto 50
      a = x + s*u/(u-w)
      return
50    a = l
      return
60    a = x - s*b
      return
      end
      subroutine rootsg(t,ft,b,c,relerr,abserr,iflag,method)

c*********************************************************************72
c
cc rootsg ???
c
      external r1mach
c
      real     a
      real     abserr
      real     acbs
      real     acmb
      real     ae
      real     b
      real     c
      real     cmb
      real     fa
      real     fb
      real     fc
      real     ft
      integer  ic
      integer  method
      real     r1mach
      real     re
      real     relerr
      real     t
      real     tol
c
      save     a
      save     acbs
      save     acmb
      save     ae
      save     cmb
      save     fa
      save     fb
      save     fc
      save     fx
      save     ic
      save     re
      save     tol
c
      method=0
      epmach=r1mach(4)
c
      if(iflag.lt.0)then
        if(iflag.eq.-1)go to 10
        if(iflag.eq.-2)go to 20
        go to 80
        endif

      if(iflag.gt.0)then
        write(*,*)'rootsg - fatal error.'
        write(*,*)'         input value of iflag was positive!'
        write(*,*)'         input iflag=',iflag
        return
        endif
c
c  call count = 0
c
      re=max(relerr,epmach)
      ae=max(abserr,0.0)
      ic=0
      acbs=abs(b-c)
      a=c
      t=a
      iflag=-1
      return
c
c  call count = 1
c
   10 continue
      fa=ft
      t=b
      iflag=-2
      return
c
c  call count = 2
c
   20 continue
      fb=ft
      fc=fa
      ic=2
      fx=max(abs(fb),abs(fc))
   30 continue
      if(abs(fc).ge.abs(fb))go to 40
c
c  interchange b and c so that abs(f(b)).le.abs(f(c)).
c
      a=b
      fa=fb
      b=c
      fb=fc
      c=a
      fc=fa
   40 continue
      cmb=0.5*(c-b)
      acmb=abs(cmb)
      tol=re*abs(b)+ae
c
c  test stopping criterion and function count.
c
      if(acmb.le.tol)go to 90
      if(ic.ge.500)then
        iflag=5
        return
        endif
c
c  calculate new iterate implicitly as b +p/q
c  where we arrange p.ge.0.  the implicit
c  form is used to prevent overflow.
c
      p=(b-a)*fb
      q=fa-fb
      if(p.lt.0.0)then
        p=-p
        q=-q
        endif
c
c  update a, check if reduction in the size of bracketing
c  interval is satisfactory.  if not, bisect until it is.
c
      a=b
      fa=fb
      ic=ic+1
      if(ic.lt.4)go to 50
      if(8.0*acmb.ge.acbs)go to 60
      ic=0
      acbs=acmb
c
c  test for too small a change
c
   50 continue
      if(p.le.abs(q)*tol)then
        method=0
        b=b+sign(tol,cmb)
        go to 70
        endif
c
c  root ought to be between b and (c+b)/2.
c
      if(p.lt.cmb*q)then
        method=2
        b=b+p/q
        go to 70
        endif
c
c  use bisection
c
   60 continue
      method=1
      b=0.5*(c+b)
c
c  have completed computation for new iterate b
c
   70 continue
      t=b
      iflag=-3
      return
c
c  call kount greater than 2
c
   80 continue
      fb=ft
      if(fb.eq.0.0)then
        iflag=2
        return
        endif
      kount=kount+1
      if((fb.ge.0.0.and.fc.ge.0.0).or.
     &   (fb.le.0.0.and.fc.le.0.0))then
        c=a
        fc=fa
        endif
      go to 30
c
c  finished.  set iflag.
c
   90 continue
      if((fb.ge.0.0.and.fc.ge.0.0).or.
     &   (fb.le.0.0.and.fc.le.0.0))then
        iflag=4
        return
        endif
      if(abs(fb).le.fx)then
        iflag=1
        return
        endif
      iflag=3
      return
      end
      subroutine rootjb(a,fa,b,fb,kount,iflag,method)

c*********************************************************************72
c
cc rootjb ???
c
      save c
      save epmach
      save fc
      save i
      save t
c
      iflag=0
      method=0
      if(kount.eq.0)then
        i=0
        if(abs(fa).gt.abs(fb))then
          temp=a
          a=b
          b=temp
          temp=fa
          fa=fb
          fb=temp
          endif
        c=b
        fc=fb
        t=0.5*abs(a-b)
        epmach=r1mach(4)
        endif
c
      kount=kount+1
c
c  preserve change of sign interval
c
      one=1.0
      if(kount.gt.1.and.sign(one,fa).eq.sign(one,fb))then
        temp=b
        b=c
        c=temp
        temp=fb
        fb=fc
        fc=temp
        endif
c
c  check for change of sign
c
      one=1.0
      if(sign(one,fa).eq.sign(one,fb))then
        iflag=-2
        return
        endif
c
c  force abs(fa).lt.abs(fb)
c
      if(abs(fa).gt.abs(fb))then
        temp=a
        a=b
        b=temp
        temp=fa
        fa=fb
        fb=temp
        endif
c
c  test for small interval
c
      em=0.5*(b-a)
      if(abs(em).lt.epmach*abs(a)+epmach)then
        iflag=-1
        return
        endif
c
c  compute numerator and denominator for secant step
c
      if(2.0*abs(c-a).lt.abs(b-a))then
        ps=(a-c)*fa
        qs=fc-fa
      else
        ps=(a-b)*fa
        qs=fb-fa
        endif
      if(ps.lt.0.0)then
        ps=-ps
        qs=-qs
        endif
c
c  compute numerator and denominator for inverse quadratic
c
      piq=0.0
      qiq=0.0
      if(b.ne.c)then
        u=fa/fc
        v=fc/fb
        w=fa/fb
        piq=u*(2.0*em*v*(v-w)-(a-c)*(w-1.0))
        qiq=(u-1.0)*(v-1.0)*(w-1.0)
        if(piq.gt.0.0)qiq=-qiq
        piq=abs(piq)
        endif
c
c  save old minimum residual point
c
      c=a
      fc=fa
c
c  test for forced bisection
c
      i=i+1
      if(i.gt.3)then
        if(8.0*abs(em).gt.t)then
          method=1
          a=a+em
          return
        else
          i=0
          t=abs(em)
          endif
        endif
      stpmin=(abs(a)+abs(em)+1.0)*epmach
c
c  choose bisection, secant or inverse quadratic step
c
      if(piq.lt.1.5*em*qiq.and.abs(piq).gt.abs(qiq)*stpmin)then
        method=3
        a=a+piq/qiq
      else if(ps.lt.qs*em.and.abs(ps).gt.abs(qs)*stpmin)then
        method=2
        a=a+ps/qs
      else
        method=1
        a=a+em
        endif
      return
      end
      subroutine locmin(a,b,relerr,abserr,f,xmin,fxmin,kount,method,
     1 iflag)

c*********************************************************************72
c
cc locmin ???
c
      external f
c
      real    a
      real    abserr
      real    b
      real    e
      real    fv
      real    fw
      real    fxmin
      integer iflag
      integer kount
      integer method
      real    relerr
      real    sa
      real    sb
      real    sm
      real    tol
      real    v
      real    w
      real    xmin
c
      save    e
      save    fv
      save    fw
      save    sa
      save    sb
      save    sm
      save    tol
      save    v
      save    w
c
      method=0
      iflag=0

      if(kount.le.0)then
        sa=a
        sb=b
        xmin=sa+0.381966*(sb-sa)
        w=xmin
        v=w
        e=0.0
        fxmin=f(xmin)
        kount=1
        fw=fxmin
        fv=fw
        endif
c
c  main loop
c
   10 continue
      sm=0.5*(sa+sb)
      tol=relerr*abs(xmin)+abserr
c
c  check for acceptance
c
      if(abs(xmin-sm)+0.5*(sb-sa).le.2.0*tol)then
        iflag=1
        return
        endif

      if(kount.gt.1.and.abs(w-xmin).lt.tol)then
        iflag=2
        return
        endif

      if(kount.gt.1.and.abs(fw-fxmin).le.relerr*abs(fxmin)+abserr)then
        iflag=3
        return
        endif
c
      r=0.0
      q=r
      p=q
c
c  fit a parabola
c
      if(abs(e).gt.tol)then
        method=2
        r=(xmin-w)*(fxmin-fv)
        q=(xmin-v)*(fxmin-fw)
        p=(xmin-v)*q-(xmin-w)*r
        q=2.0*(q-r)
        if(q.gt.0.0)then
          p=-p
        else
          q=-q
          endif
        r=e
        e=d
        endif

      if(abs(p).ge.abs(0.5*q*r))go to 30
      if((p.le.q*(sa-xmin)).or.(p.ge.q*(sb-xmin)))go to 30
c
c  a parabolic interpolation step
c  f must not be evaluated too close to a or b
c
      d=p/q
      u=xmin+d
      if((u-sa.lt.2.0*tol).or.(sb-u.lt.2.0*tol))then
        d=tol
        if(xmin.ge.sm)d=-tol
        endif
      go to 40

   30 continue
c
c  a golden section step
c
      method=1

      if(xmin.ge.sm)then
        e=sa-xmin
      else
        e=sb-xmin
        endif

      d=0.381966*e

   40 continue

      if(abs(d).ge.tol)then
        u=xmin+d
      else
        step=sign(tol,d)
        u=xmin+tol
        endif
c
c  update a, b, v, w, and xmin
c
      fu=f(u)
      kount=kount+1

      if(fu.le.fxmin)then

        if(u.ge.xmin)then
          sa=xmin
        else
          sb=xmin
          endif

        v=w
        fv=fw
        w=xmin
        fw=fxmin
        xmin=u
        fxmin=fu
        return
        endif

      if(u.ge.xmin)then
        sb=u
      else
        sa=u
        endif

      if((fu.le.fw).or.(w.eq.xmin))then
        v=w
        fv=fw
        w=u
        fw=fu
        return
        endif

      if((fu.gt.fv).and.(v.ne.xmin).and.(v.ne.w))return

      v=u
      fv=fu
      return
      end
      subroutine czero(z,p,nd,w)

c*********************************************************************72
c
cc czero ???
c
      complex p(1),w(1),z(1),pi,r,s,u,v,x
      real a,b,c,d,e,amag,real,s1,t,t1,t2,v1,y,y0
      integer f,g,h,i,j,k,l,m,n,nc,nd,nz,n1,o,o1,o2,o3,o4,q
      n = nd
      do 10 i = 1,n
10         z(i) = p(i)
20    if ( n .le. 1 ) return
      if ( n .gt. 2 ) goto 30
      call qad(z(2),z(1))
      return
30    if ( amag(z(1)) .ne. 0.0 ) goto 50
      do 40 i = 2,n
        z(i-1) = z(i)
40      continue
      z(n) = (0.0,0.0)
      n = n - 1
      goto 20
50    nz = n
      e = 1.0
60    e = .5*e
      if( 1.0+e .gt. 1.0 ) goto 60
      e = e + e
      m = 5
      pi = cmplx(0.0,2.0*acos(-1.0))
      x = (1.0,0.0)
      y = 65536.
      y0 = 1./y
70    n1 = n + 1
      o1 = n1 + n
      o2 = o1 + n
      o3 = o2 + n1/2
      o4 = o3 + n1/2
      o = o4 - 1
      call cc(z,w(o1),w(o2),n,y,y0)
      g = o4 + n1
      nc = 32*n
      k = 10
      l = 8
      q = 0
      j = 0
      f = 0
80    f = f + 1
      q = q + j
      if ( f .lt. 6 ) goto 90
      if ( l .gt. nc ) goto 370
90    l = l + l
      call rad(z,w,w(n1),k,n,a)
      call cef(w(o4),z,w(o1),w(o2),w(o3),a,x,n,n1,t,t1,y,y0)
      t = n*t*e
      j = 1
      i = n1
      h = i
100   i = i/2
      j = j + j
      if ( i .gt. 0 ) goto 100
      if ( h+h .eq. j ) goto 120
      h = j
      j = h + o
      do 110 i = g,j
110        w(i) = (0.0,0.0)
120   j = l + o4
      call ff(w(o4),w(j),h,l)
      c = amag(w(o4))
      j = 0
      h = l - 1
      do 130 i=1,h
           d = amag(w(o4+i))
           if ( d .gt. c ) goto 130
           j = i
           c = d
130   continue
      s = a*x*cexp(j*pi/l)
      s1 = cabs(s)
      if ( s1 .lt. 1. ) s1 = 1.
      x = x*cexp(pi/(4*l))
      x = x/cabs(x)
      do 140 i=1,m
140        call jt(z,w,n,v,s,s1)
      c = 0.0
      j = 0
150   call jt(z,w,n,v,s,s1)
      v1 = amag(v)
      if ( v1 .eq. 0. ) goto 240
      if ( s1 .gt. 1. ) goto 170
      u = (1.0,0.0)
      i = n
160   i = i - 1
      u = w(i) + u*s
      if ( i .gt. 1 ) goto 160
      if ( amag(u) .eq. 0.0 ) goto 80
      goto 190
170   u = w(1)
      r = 1./s
      do 180 i = 2,n
180        u = w(i) + u*r
      if ( amag(u) .eq. 0.0 ) goto 80
      u = u*r
190   r = s - v/u
      j = j + 1
      d = amag(r-s)
      s = r
      if ( s1 .le. a ) goto 200
      call err(z,n,s1,t2)
      t2 = n*t2*e
      if ( v1 .le. t2 ) goto 240
      goto 210
200   if ( n*alog(t1/s1)+alog(t/v1) .ge. 0. ) goto 230
210   s1 = cabs(s)
      if ( s1 .lt. 1. ) s1 = 1.
      if ( j .lt. 3 ) goto 220
      if ( .5*d .gt. b+c ) goto 80
      if ( j .lt. m ) goto 220
      if ( 4.0*d .gt. b+c ) goto 80
220   b = c
      c = d
      goto 150
230   call err(z,n,s1,t2)
      t2 = n*e*t2
      if ( v1 .gt. t2 ) goto 210
240   r = z(n) + s
      i = n
250   i = i - 1
      v = z(i) + r*s
      z(i) = r
      r = v
      if ( i .gt. 1 ) goto 250
      z(n) = s
      q = q + j
      n = n - 1
      if ( n .gt. 2 ) goto 70
      call qad(z(2),z(1))
      nc = nd
      n = 1
260   l = n
      n = nd
      m = n + n
      j = 1
      k = n - 1
      do 270 i = 2,k
           w(j) = j*p(i)
           w(j+n) = j*i*p(i+1)
           w(j+m) = amag(p(j))
270        j = i
      w(k) = k*p(n)
      w(n) = n
      w(m-1) = n*k
      w(m) = 0.0
      w(m+k) = amag(p(k))
      w(m+n) = amag(p(n))
      o1 = m + 1
      n1 = n + 1
      do 340 k = l,nz
           s = z(k)
           do 280 j = 1,3
                call rfn(p,w,w(n1),w(o1),n,s,v,c,t)
                t = n*t*e
280             if ( c .le. t ) goto 330
           a = cabs(s)
           if ( a .gt. 1.0 ) goto 300
           v = (1.0,0.0)
           t = 1.
           i = n
290        v = v*s + p(i)
           t = t*a + real(w(i+m))
           i = i - 1
           if ( i .gt. 0 ) goto 290
           goto 320
300        r = 1./s
           a = 1./a
           v = (0.0,0.0)
           t = 0.
           do 310 i = 1,n
                v = v*r + p(i)
310             t = t*a + real(w(i+m))
           v = v*r + cmplx(1.0,0.0)
           t = t*a + 1.
320        a = amag(v)
           if ( a .gt. n*t*e ) goto 340
330        z(k) = s
340   continue
      do 350 i = l,n
350        w(n1-i) = z(i)
      l = n1 - l
      do 360 i = 1,l
360        z(i) = w(i)
      w(1) = nc
      return
370   nc = nd - n + 1
      write(6,*) 'since the stopping criterion has not been satisfied'
      write(6,*) 'after',q,'iterations, we stop while computing'
      write(6,*) 'root number',nc
      z(n) = s
      goto 260
      end
      real function amag(c)

c*********************************************************************72
c
cc amag ???
c
      complex c,d
      real e(2)
      equivalence (d,e)
      d = c
      amag = abs(e(1)) + abs(e(2))
      return
      end
      subroutine qad(b,c)

c*********************************************************************72
c
cc qad ???
c
      complex b,c,r,s
      real amag,t
      t = amag(b)
      r = cmplx(4.0,0.0)*c
      if ( t .gt. sqrt(amag(r)) ) goto 10
      r = csqrt(b*b-r)
      goto 20
10    s = 1.0/t
      r = t*csqrt((s*b)**2-r*s*s)
20    s = r + b
      r = r - b
      t = amag(r)
      if ( t .ge. amag(s) ) goto 30
      c = -(c+c)/s
      b = cmplx(-.5,0.0)*s
      return
30    if ( t .eq. 0.0 ) return
      c = (c+c)/r
      b = cmplx(.5,0.0)*r
      return
      end
      subroutine jt(p,q,n,v,s,s1)

c*********************************************************************72
c
cc jt ???
c
      complex p(1),q(1),r,s,t,u,v,w
      real amag,s1
      integer i,m,n
      if ( s1 .gt. 1. ) goto 70
      u = (0.0,0.0)
      v = (1.0,0.0)
      i = n
10    v = p(i) + v*s
      u = q(i) + u*s
      i = i - 1
      if ( i .gt. 0 ) goto 10
      if ( amag(u) .ne. 0.0 ) goto 50
      m = n
20    r = q(m)
      q(m) = (0.0,0.0)
      i = m
30    i = i - 1
      u = q(i) + r*s
      q(i) = r
      r = u
      if ( i .gt. 1 ) goto 30
      u = (0.0,0.0)
      i = m
40    u = q(i) + u*s
      i = i - 1
      if ( i .gt. 0 ) goto 40
      m = m - 1
      if ( amag(u) .eq. 0.0 ) goto 20
50    r = v/u
      t = p(n) - r*q(n) + s
      q(n) = (1.0,0.0)
      i = n
60    i = i - 1
      u = p(i) - r*q(i) + t*s
      q(i) = t
      t = u
      if ( i .gt. 1 ) goto 60
      return
70    w = 1./s
      u = q(1)
      v = p(1)
      do 80 i = 2,n
           u = u*w + q(i)
80         v = v*w + p(i)
      u = u*w
      v = v*w + 1.
      if ( amag(u) .ne. 0. ) goto 120
      m = n
90    r = q(m)
      q(m) = (0.0,0.0)
      i = m
100   i = i - 1
      u = q(i) + r*s
      q(i) = r
      r = u
      if ( i .gt. 1 ) goto 100
      u = (0.0,0.0)
      m = m - 1
      do 110 i = 1,n
110        u = u*w + q(i)
      u = u*w
      if ( amag(u) .eq. 0. ) goto 90
120   r = v/u
      u = p(1) - r*q(1)
      q(1) = -w*u
      do 130 i = 2,n
           u = p(i) - r*q(i) + u*w
130        q(i) = -w*u
      q(n) = (1.0,0.0)
      return
      end
      subroutine rfn(p,q,r,z,n,s,v,b,c)

c*********************************************************************72
c
cc rfn ???
c
      complex p(1),q(1),r(1),z(1),m,s,t,u,v,w
      real a,b,c,amag,real
      integer i,n
      a = cabs(s)
      if ( a .gt. 1.0 ) goto 20
      v = (1.0,0.0)
      u = (0.0,0.0)
      w = (0.0,0.0)
      c = 1.
      i = n
10    v = v*s + p(i)
      u = u*s + q(i)
      w = w*s + r(i)
      c = c*a + real(z(i))
      i = i - 1
      if ( i .gt. 0 ) goto 10
      goto 40
20    t = cmplx(1.0,0.0)/s
      a = 1./a
      v = p(1)
      u = q(1)
      w = r(1)
      c = real(z(1))
      do 30 i = 2,n
         v = v*t + p(i)
         u = u*t + q(i)
         w = w*t + r(i)
30       c = c*a + real(z(i))
      v = v*t + cmplx(1.0,0.0)
      u = u*t
      w = w*t
      c = c*a + 1.
40    b = amag(v)
      if ( b .eq. 0.0 ) return
      a = amag(u)
      if ( a .eq. 0.0 ) return
      t = v/u
      a = amag(w)
      if ( a .eq. 0.0 ) goto 50
      u = u/w
      m = u - t
      a = amag(m)
      if ( a .eq. 0.0 ) return
      m = u/m
      s = s - m*t
      return
50    s = s - t
      return
      end
      subroutine rad(p,q,r,k,n,a)

c*********************************************************************72
c
cc rad ???
c
      complex p(1),q(1),r(1),t
      real a,c,d,f,amag,s
      integer i,j,k,l,m,n
      data j/1/
      if ( j .eq. k ) goto 20
      j = 1
      l = 1
      do 10 i = 2,n
           q(l) = l*p(i)/n
10         l = i
      q(n) = 1
      c = n*cabs(p(1))
      f = 1.0/n
      a = cabs(p(1))**(1.0/float(n))
      goto 40
20    do 30 i = 1,n
30         q(i) = r(i)
40    l = j + k
50    if ( amag(q(1)) .eq. 0.0 ) goto 90
      j = j+1
      t = p(1)/q(1)
      do 60 i = 2,n
60         q(i-1) = p(i) - t*q(i)
      s = 1.0/float(j)
      f = f**(1.0-s)*cabs(t)**s
      d = f*(c/cabs(q(1)))**s
      if ( d .lt. a ) a = d
70    if ( j .lt. l ) goto 50
      k = j
      do 80 i = 1,n
80         r(i) = q(i)
      return
90    m = j
100   j = j + 1
      do 110 i = 2,n
110        q(i-1) = q(i)
      q(n) = (0.0,0.0)
      if ( amag(q(1)) .eq. 0.0 ) goto 100
      s = 1.0/j
      f = f**(float(m)*s)
      d = f*(c/cabs(q(1)))**s
      if ( d .lt. a ) a = d
      t = p(1)/q(1)
      do 120 i = 2,n
120        q(i-1) = p(i) - t*q(i)
      q(n) = (1.0,0.0)
      f = f*cabs(t)**s
      goto 70
      end
      subroutine ff(a,b,n,l)

c*********************************************************************72
c
cc ff ???
c
      complex a(1),b(1),s,t,w
      real p
      integer i,il,i1,i2,j,k,l,l1,l2,m,m0,m1,n
      m = n
      if ( l .ge. m ) goto 20
      m = m - 1
      do 10 j = l,m,l
           do 10 i = 1,l
10              a(i) = a(i) + a(i+j)
      m = l
20    p = acos(-1.0)
      w = cexp(m*cmplx(0.0,p)/l)
      l1 = l - 1
      l2 = l/2
      if ( m .eq. l ) goto 40
      do 30 j = m,l1,m
           do 30 i = 1,m
30         a(i+j) = a(i)
40    m0 = m
      m = m/2
      m1 = m - 1
      if ( m .eq. 0 ) return
      s = 1
      t = -1
      i1 = 0
      do 60 j = 1,l,m0
           il = j + m1
           i2 = i1 + l2
           do 50 i = j,il
                k = i + m
                b(i+i1) = a(i) + s*a(k)
50              b(i+i2) = a(i) + t*a(k)
           s = w*s
           t = w*t
           i1 = i1 - m
60    continue
      do 70 i = 1,l
70         a(i) = b(i)
      w = csqrt(w)
      goto 40
      end
      subroutine cc(z,f,e,n,y,y0)

c*********************************************************************72
c
cc cc ???
c
      complex f(1),z(1),x
      real amag,e(1),s,t,y,y0
      integer i,n
      do 40 i = 1,n
           s = 0.
           x = z(i)
           t = amag(x)
           if ( t .gt. 1. ) goto 20
           if ( t .gt. y0 ) goto 30
           if ( t .eq. 0. ) goto 30
10         t = t*y
           x = x*y
           s = s - 1.
           if ( t .le. y0 ) goto 10
           goto 30
20         t = t*y0
           x = x*y0
           s = s + 1.
           if ( t .gt. 1. ) goto 20
30         e(i) = s
           f(i) = x
40    continue
      return
      end
      subroutine cef(w,z,f,e,d,r,x,n,n1,t,t1,y,y0)

c*********************************************************************72
c
cc cef ???
c
      complex f(1),w(1),z(1),c,x,p
      real d(1),amag,e(1),a,b,q,r,s,t,t1,y,y0
      integer i,n,n1
      p = 1.
      q = 0.
      s = e(1)
      c = x*r
      if ( r .gt. 1. ) goto 50
      t = 1.
      t1 = 1.
      do 20 i = 1,n
           a = e(i) + q
           if ( a .gt. s ) s = a
           w(i) = p*f(i)
           d(i) = a
           t = r*t + amag(z(n1-i))
           p = p*c
           if ( amag(p) .gt. y0 ) goto 20
10         p = p*y
           q = q - 1.
           if ( amag(p) .le. y0 ) goto 10
20    continue
30    if ( q .gt. s ) s = q
      do 40 i = 1,n
           a = d(i) - s
40         w(i) = w(i)*y**a
      a = q - s
      w(n1) = p*y**a
      return
50    t1 = r
      b = 1./r
      t = 0.
      do 70 i = 1,n
           a = e(i) + q
           if ( a .gt. s ) s = a
           w(i) = p*f(i)
           d(i) = a
           t = b*t + amag(z(i))
           p = p*c
           if ( amag(p) .le. 1. ) goto 70
60         p = p*y0
           q = q + 1.
           if ( amag(p) .gt. 1. ) goto 60
70    continue
      t = b*t + 1.
      goto 30
      end
      subroutine err(z,n,s,t)

c*********************************************************************72
c
cc err ???
c
      complex z(1)
      real amag,r,s,t
      integer i,n
      if ( s .gt. 1. ) goto 20
      t = 1.
      i = n
10    t = t*s + amag(z(i))
      i = i - 1
      if ( i .gt. 0 ) goto 10
      return
20    r = 1./s
      t = amag(z(1))
      do 30 i = 2,n
30         t = t*r + amag(z(i))
      t = t*r + 1.
      return
      end
      subroutine cubic(b,r)

c*********************************************************************72
c
cc cubic ???
c
c  cubic returns the complex roots of a monic cubic polynomial with
c  real coefficients.
c
c  b      input, real b(3), the coefficients of the polynomial.
c         p(x) = x**3 + b(1)*x**2 + b(2)*x + b(3)
c
c  r      output, real r(6), the three complex roots of the polynomial,
c         returned as pairs of real numbers (r(1),r(2)), (r(3),r(4)),
c         and (r(5),r(6)).
c
      dimension  b(3),r(6)
      data  cr3s2/.86602540378444/, cr3/1.73205080756888/
c
      do 14 j=1,6
14      r(j)=0.0
      a2=b(3)*b(3)
      am3=b(3)/3.
      p=(a2/3.-b(2))/3.
      q=(b(3)*(b(2)/3.-a2/13.5)-b(1))/2.
      if(q.ne.0)go to 6
8     d=sqrt(abs(3.0*p))
      if(p.gt.0.0)then
        r(3)=d
        r(5)=-d
      else
        r(4)=d
        r(6)=-d
        endif
      go to 11
6     if(p.eq.0.0)then
        r(1)=cubert(q+q)
        r(4)=cr3s2*r(1)
        go to 10
        endif
      ps=sqrt(abs(p))
      d=q/ps/p
      if(p.lt.0.0)go to 1
3     if (abs(d)-1.0) 7,7,9
9     y=abs(d)
      ex3p=cubert(y+sqrt(y*y-1.))
      ex3m=1./ex3p
      r(1)=sign(ps*(ex3p+ex3m),q)
      r(4)=cr3s2*ps*(ex3p-ex3m)
      go to 10
1     y=-d
      if(y.le.0.0)then
        ex3m=cubert(-y+sqrt(y*y+1.))
        ex3p=1./ex3m
      else
        ex3p=cubert(y+sqrt(y*y+1.))
        ex3m=1./ex3p
        endif
      r(1)=ps*(ex3p-ex3m)
      r(4)=cr3s2*ps*(ex3p+ex3m)
10    r(3)=-r(1)/2.
      r(5)=r(3)
      r(6)=-r(4)
      go to 11
7     theta=atan(sqrt(1.-d*d)/abs(d))/3.
      cost=cos(theta)
      t1=sign(ps*cost,q)
      t2=cr3*ps*sin(theta)
      r(1)=t1+t1
      r(3)=t2-t1
      r(5)=-t1-t2
11    do 12 j=1,5,2
12      r(j)=r(j)-am3
      return
      end
      function cubert(x)

c*********************************************************************72
c
cc cubert ???
c
      cubert=sign(abs(x)**.333333333333333,x)
      return
      end
      subroutine fzero(f,b,c,r,re,ae,iflag)

c*********************************************************************72
c
cc fzero ???
c
c***begin prologue  fzero
c***date written   700901   (yymmdd)
c***revision date  860411   (yymmdd)
c***category no.  f1b
c***keywords  bisection,nonlinear,roots,zeros
c***author  shampine,l.f.,snla
c           watts,h.a.,snla
c***purpose  fzero searches for a zero of a function f(x) in a given
c            interval (b,c).  it is designed primarily for problems
c            where f(b) and f(c) have opposite signs.
c***description
c
c    from the book "numerical methods and software"
c       by  d. kahaner, c. moler, s. nash
c           prentice hall 1988
c
c     based on a method by t j dekker
c     written by l f shampine and h a watts
c
c            fzero searches for a zero of a function f(x) between
c            the given values b and c until the width of the interval
c            (b,c) has collapsed to within a tolerance specified by
c            the stopping criterion, abs(b-c) .le. 2.*(rw*abs(b)+ae).
c            the method used is an efficient combination of bisection
c            and the secant rule.
c
c     description of arguments
c
c     f,b,c,r,re and ae are input parameters
c     b,c and iflag are output parameters (flagged by an * below)
c
c        f     - name of the real valued external function.  this name
c                must be in an external statement in the calling
c                program.  f must be a function of one real argument.
c
c       *b     - one end of the interval (b,c).  the value returned for
c                b usually is the better approximation to a zero of f.
c
c       *c     - the other end of the interval (b,c)
c
c        r     - a (better) guess of a zero of f which could help in
c                speeding up convergence.  if f(b) and f(r) have
c                opposite signs, a root will be found in the interval
c                (b,r); if not, but f(r) and f(c) have opposite
c                signs, a root will be found in the interval (r,c);
c                otherwise, the interval (b,c) will be searched for a
c                possible root.  when no better guess is known, it is
c                recommended that r be set to b or c; because if r is
c                not interior to the interval (b,c), it will be ignored.
c
c        re    - relative error used for rw in the stopping criterion.
c                if the requested re is less than machine precision,
c                then rw is set to approximately machine precision.
c
c        ae    - absolute error used in the stopping criterion.  if the
c                given interval (b,c) contains the origin, then a
c                nonzero value should be chosen for ae.
c
c       *iflag - a status code.  user must check iflag after each call.
c                control returns to the user from fzero in all cases.
c
c                1  b is within the requested tolerance of a zero.
c                   the interval (b,c) collapsed to the requested
c                   tolerance, the function changes sign in (b,c), and
c                   f(x) decreased in magnitude as (b,c) collapsed.
c
c                2  f(b) = 0.  however, the interval (b,c) may not have
c                   collapsed to the requested tolerance.
c
c                3  b may be near a singular point of f(x).
c                   the interval (b,c) collapsed to the requested tol-
c                   erance and the function changes sign in (b,c), but
c                   f(x) increased in magnitude as (b,c) collapsed,i.e.
c                     abs(f(b out)) .gt. max(abs(f(b in)),abs(f(c in)))
c
c                4  no change in sign of f(x) was found although the
c                   interval (b,c) collapsed to the requested tolerance.
c                   the user must examine this case and decide whether
c                   b is near a local minimum of f(x), or b is near a
c                   zero of even multiplicity, or neither of these.
c
c                5  too many (.gt. 500) function evaluations used.
c***references  l. f. shampine and h. a. watts, *fzero, a root-solving
c                 code*, sc-tm-70-631, september 1970.
c               t. j. dekker, *finding a zero by means of successive
c                 linear interpolation*, 'constructive aspects of the
c                 fundamental theorem of algebra', edited by b. dejon
c                 p. henrici, 1969.
c***routines called  r1mach
c***end prologue  fzero
c
      external f
c
      real a,acbs,acmb,ae,aw,b,c,cmb,er,fa,fb,fc,fx,fz,p,q,r
      real re,rw,t,tol,z
      integer ic,iflag,kount
c
c     er is two times the computer unit roundoff value which is
c     defined here by the function r1mach.
c
c***first executable statement  fzero
      er = 2.0e0 * r1mach(4)
c
c     initialize
c
      z=r
      if(r.le.amin1(b,c).or.r.ge.amax1(b,c)) z=c
      rw=amax1(re,er)
      aw=amax1(ae,0.0)
      ic=0
      t=z
      fz=f(t)
      fc=fz
      t=b
      fb=f(t)
      kount=2
      if(sign(1.0e0,fz).eq.sign(1.0e0,fb)) go to 1
      c=z
      go to 2
    1 if(z.eq.c) go to 2
      t=c
      fc=f(t)
      kount=3
      if(sign(1.0e0,fz).eq.sign(1.0e0,fc)) go to 2
      b=z
      fb=fz
    2 a=c
      fa=fc
      acbs=abs(b-c)
      fx=amax1(abs(fb),abs(fc))
c
    3 if (abs(fc) .ge. abs(fb)) go to 4
c     perform interchange
      a=b
      fa=fb
      b=c
      fb=fc
      c=a
      fc=fa
c
    4 cmb=0.5*(c-b)
      acmb=abs(cmb)
      tol=rw*abs(b)+aw
c
c     test stopping criterion and function count
c
      if (acmb .le. tol) go to 10
      if(fb.eq.0.e0) go to 11
      if(kount.ge.500) go to 14
c
c     calculate new iterate implicitly as b+p/q
c     where we arrange p .ge. 0.
c     the implicit form is used to prevent overflow.
c
      p=(b-a)*fb
      q=fa-fb
      if (p .ge. 0.) go to 5
      p=-p
      q=-q
c
c     update a and check for satisfactory reduction
c     in the size of the bracketing interval.
c     if not, perform bisection.
c
    5 a=b
      fa=fb
      ic=ic+1
      if (ic .lt. 4) go to 6
      if (8.*acmb .ge. acbs) go to 8
      ic=0
      acbs=acmb
c
c     test for too small a change
c
    6 if (p .gt. abs(q)*tol) go to 7
c
c     increment by tolerance
c
      b=b+sign(tol,cmb)
      go to 9
c
c     root ought to be between b and (c+b)/2.
c
    7 if (p .ge. cmb*q) go to 8
c
c     use secant rule
c
      b=b+p/q
      go to 9
c
c     use bisection
c
    8 b=0.5*(c+b)
c
c     have completed computation for new iterate b
c
    9 t=b
      fb=f(t)
      kount=kount+1
c
c     decide whether next step is interpolation or extrapolation
c
      if (sign(1.0,fb) .ne. sign(1.0,fc)) go to 3
      c=a
      fc=fa
      go to 3
c
c
c     finished. process results for proper setting of iflag
c
   10 if (sign(1.0,fb) .eq. sign(1.0,fc)) go to 13
      if (abs(fb) .gt. fx) go to 12
      iflag = 1
      return
   11 iflag = 2
      return
   12 iflag = 3
      return
   13 iflag = 4
      return
   14 iflag = 5
      return
      end
      subroutine drpoly(coef,ndeg,nzero,zeror,zeroi,fail)

c*********************************************************************72
c
cc drpoly ???
c
c  drpoly finds the roots or "zeroes" of a real polynomial.  
c
c  drpoly uses double precision arithmetic for its calculations.  the roots of 
c  the polynomial are returned as pairs of double precision values, 
c  containing the real and imaginary parts of the roots.
c
c  drpoly is a slightly modified version of acm toms algorithm 493.
c
c  coef   input, double precision coef(ndeg+1).
c         coef contains the polynomial coefficients.  
c
c         coef(1) contains the coefficient of x**(ndeg), 
c         coef(2) contains the coefficient of x**(ndeg-1), and so on, up to
c         coef(ndeg) contains the constant term.
c
c         coef(1) may not be zero.  if the input value of coef(1) is zero,
c         the program sets fail=.true., prints an error message, and returns
c         immediately without performing any computations.
c
c  ndeg   input, integer ndeg.
c         ndeg is the degree of the polynomial.
c         for example, the polynomial 97*x*x + 14*x + 3 has degree 2.
c
c         the program imposes a limit on the maximum allowable value of ndeg.
c         currently, this maximum value is 100.
c
c  nzero  output, integer nzero.
c         nzero is the number of zeroes found by the program.
c         normally, the output value of nzero will be equal to ndeg.
c         in some cases, however, the program will not be able to find
c         the full set of zeroes, and so nzero may be less than ndeg.
c         in the worst case, nzero would be zero, representing the fact
c         that the program was unable to find any roots at all.
c
c  zeror, 
c  zeroi  output, double precision zeror(nzero), zeroi(nzero).
c         zeror and zeroi contain the real and imaginary parts of the
c         roots of the polynomial.  for instance, the location of the
c         first zero of the polynomial is zeror(1) + i * zeroi(1).
c
c  fail   output, logical fail.
c         
c         fail is .true. if a fatal error occurred in the program, in which
c         case no roots were found, or if the iterative method failed,
c         and hence only some roots were found.  the program will have
c         returned nzero roots, which may be 0, ndeg, or some intermediate
c         value.
c
c         fail is .false. if no fatal error occurred, and the iteration
c         did not fail.  in that case, all is well, and the program should
c         return nzero roots, where nzero=ndeg.
c
      integer maxord
      parameter (maxord=101)
c
      common /dcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
c
      double precision p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      double precision eta, are, mre
      integer n, nn
      double precision coef(*), temp(maxord),
     * zeror(*), zeroi(*), t, aa, bb, cc,
     * factor
      double precision pt(maxord), lo, xmax,xmin, xx, yy, cosr,
     * sinr, xxx, x, sc, bnd, xm, ff, df, dx, infin,
     * smalno, base
      integer nzero, cnt, nz, i, j, jj, nm1
      logical fail, zerok
c
c the following statements set machine constants used
c in various parts of the program. the meaning of the
c four constants are...
c
c eta     the maximum relative representation error
c         which can be described as the smallest
c         positive floating point number such that
c         1.0+eta is greater than 1.
c
c infiny  the largest floating-point number.
c
c smalno  the smallest positive floating-point number
c
c base    the base of the floating-point number system used.
c
      base = i1mach(7)
      eta = d1mach(4)
      infin = d1mach(2)
      smalno = d1mach(1)
c
c are and mre refer to the unit error in + and *
c respectively. they are assumed to be the same as eta.
c
      are = eta
      mre = eta
      lo = smalno/eta
c
c initialization of constants for shift rotation
c
      xx = .70710678
      yy = -xx
      cosr = -.069756474
      sinr = .99756405
      fail = .false.
      n = ndeg
      nzero=ndeg
      nn = n + 1
c
      if(ndeg.ge.maxord)then
        write(*,*)' '
        write(*,*)'drpoly cannot solve this problem!'
        write(*,*)'drpoly cannot handle degrees greater than ',maxord-1
        write(*,*)'however, the input value of ndeg is ',ndeg
        fail=.true.
        nzero=0
        return
        endif
c
c  the algorithm fails if the leading coefficient is zero.
c
      if(coef(1).eq.0.0)then
        write(*,*)' '
        write(*,*)'drpoly cannot solve this problem!'
        write(*,*)'the input value of coef(1) is zero.'
        write(*,*)'this is illegal.'
        write(*,*)' '
        fail = .true.
        nzero = 0
        return
        endif
c
c  remove the zeros at the origin if any
c
   10 continue
      if (coef(nn).eq.0.0)then
        j = nzero - n + 1
        zeror(j) = 0.0
        zeroi(j) = 0.0
        nn = nn - 1
        n = n - 1
        go to 10
        endif
c
c  make a copy of the coefficients
c
      do 30 i=1,nn
        p(i) = coef(i)
   30 continue
c
c start the algorithm for one zero
c
   40 continue

      if(n.lt.1)then
        return
      elseif(n.eq.1)then
        zeror(nzero) = -p(2)/p(1)
        zeroi(nzero) = 0.0
        return
      elseif(n.eq.2)then
        call dquad(p(1), p(2), p(3), zeror(nzero-1),
     *  zeroi(nzero-1), zeror(nzero), zeroi(nzero))
        return
        endif
c
c find largest and smallest moduli of coefficients.
c
      xmax = 0.
      xmin = infin
      do 70 i=1,nn
        x = abs(p(i))
        if (x.gt.xmax) xmax = x
        if (x.ne.0. .and. x.lt.xmin) xmin = x
   70 continue
c
c scale if there are large or very small coefficients
c computes a scale factor to multiply the
c coefficients of the polynomial. the scaling is done
c to avoid overflow and to avoid undetected underflow
c interfering with the convergence criterion.
c the factor is a power of the base
c
      sc = lo/xmin
      if (sc.gt.1.0) go to 80
      if (xmax.lt.10.) go to 110
      if (sc.eq.0.) sc = smalno
      go to 90
   80 if (infin/sc.lt.xmax) go to 110
   90 l = log(sc)/log(base) + .5
      factor = (base*1.0)**l
      if (factor.eq.1.0) go to 110
      do 100 i=1,nn
        p(i) = factor*p(i)
  100 continue
c
c compute lower bound on moduli of zeros.
c
  110 do 120 i=1,nn
        pt(i) = abs(p(i))
  120 continue
      pt(nn) = -pt(nn)
c
c compute upper estimate of bound
c
      x = exp((log(-pt(nn))-log(pt(1)))/n)
      if (pt(n).eq.0.) go to 130
c
c if newton step at the origin is better, use it.
c
      xm = -pt(nn)/pt(n)
      if (xm.lt.x) x = xm
c
c chop the interval (0,x) until ff .le. 0
c
  130 xm = x*.1
      ff = pt(1)
      do 140 i=2,nn
        ff = ff*xm + pt(i)
  140 continue
      if (ff.le.0.) go to 150
      x = xm
      go to 130
  150 dx = x
c
c do newton iteration until x converges to two decimal places
c
  160 if (abs(dx/x).le..005) go to 180
      ff = pt(1)
      df = ff
      do 170 i=2,n
        ff = ff*x + pt(i)
        df = df*x + ff
  170 continue
      ff = ff*x + pt(nn)
      dx = ff/df
      x = x - dx
      go to 160
  180 bnd = x
c
c compute the derivative as the intial k polynomial
c and do 5 steps with no shift
c
      nm1 = n - 1
      do 190 i=2,n
        k(i) = float(nn-i)*p(i)/float(n)
  190 continue
      k(1) = p(1)
      aa = p(nn)
      bb = p(n)
      zerok = k(n).eq.0.0
      do 230 jj=1,5
        cc = k(n)
        if (zerok) go to 210
c
c use scaled form of recurrence if value of k at 0 is nonzero
c
        t = -aa/cc
        do 200 i=1,nm1
          j = nn - i
          k(j) = t*k(j-1) + p(j)
  200   continue
        k(1) = p(1)
        zerok = abs(k(n)).le.abs(bb)*eta*10.
        go to 230
c
c use unscaled form of recurrence
c
  210   do 220 i=1,nm1
          j = nn - i
          k(j) = k(j-1)
  220   continue
        k(1) = 0.0
        zerok = k(n).eq.0.0
  230 continue
c
c save k for restarts with new shifts
c
      do 240 i=1,n
        temp(i) = k(i)
  240 continue
c
c loop to select the quadratic  corresponding to each new shift
c
      do 280 cnt=1,20
c
c quadratic corresponds to a double shift to a
c non-real point and its complex conjugate. the point
c has modulus bnd and amplitude rotated by 94 degrees
c from the previous shift
c
        xxx = cosr*xx - sinr*yy
        yy = sinr*xx + cosr*yy
        xx = xxx
        sr = bnd*xx
        si = bnd*yy
        u = -2.0*sr
        v = bnd
c
c second stage calculation, fixed quadratic
c
        call dfxshf(20*cnt, nz)
        if (nz.eq.0) go to 260
c
c the second stage jumps directly to one of the third
c stage iterations and returns here if successful.
c deflate the polynomial, store the zero or zeros and
c return to the main algorithm.
c
        j = nzero - n + 1
        zeror(j) = szr
        zeroi(j) = szi
        nn = nn - nz
        n = nn - 1
        do 250 i=1,nn
          p(i) = qp(i)
  250   continue
        if (nz.eq.1) go to 40
        zeror(j+1) = lzr
        zeroi(j+1) = lzi
        go to 40
c
c if the iteration is unsuccessful another quadratic
c is chosen after restoring k
c
  260   do 270 i=1,n
          k(i) = temp(i)
  270   continue
  280 continue
c
c return with failure if no convergence with 20 shifts
c
      fail = .true.
      nzero = nzero - n
      return
      end
      subroutine dfxshf(l2, nz)

c*********************************************************************72
c
cc dfxshf ???
c
c  dfxshf computes up to l2 fixed shift k-polynomials,
c  testing for convergence in the linear or quadratic
c  case. initiates one of the variable shift
c  iterations and returns with the number of zeros
c  found.
c
c l2 - limit of fixed shift steps
c nz - number of zeros found
c
      integer maxord
      parameter (maxord=101)
c
      common /dcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
      double precision p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      double precision eta, are, mre
      integer n, nn
      double precision svu, svv, ui, vi, s
      double precision betas, betav, oss, ovv, ss, vv, ts, tv,
     * ots, otv, tvv, tss
      integer l2, nz, type, i, j, iflag
      logical vpass, spass, vtry, stry
c
      nz = 0
      betav = .25
      betas = .25
      oss = sr
      ovv = v
c
c evaluate polynomial by synthetic division
c
      call dquads(nn, u, v, p, qp, a, b)
      call dcalcs(type)
      do 80 j=1,l2
c
c calculate next k polynomial and estimate v
c
        call dnextk(type)
        call dcalcs(type)
        call dnuest(type, ui, vi)
        vv = vi
c
c estimate s
c
        ss = 0.
        if (k(n).ne.0.0) ss = -p(nn)/k(n)
        tv = 1.
        ts = 1.
        if (j.eq.1 .or. type.eq.3) go to 70
c
c compute relative measures of convergence of s and v sequences
c
        if (vv.ne.0.) tv = abs((vv-ovv)/vv)
        if (ss.ne.0.) ts = abs((ss-oss)/ss)
c
c if decreasing, multiply two most recent convergence measures
c
        tvv = 1.
        if (tv.lt.otv) tvv = tv*otv
        tss = 1.
        if (ts.lt.ots) tss = ts*ots
c
c compare with convergence criteria
c
        vpass = tvv.lt.betav
        spass = tss.lt.betas
        if (.not.(spass .or. vpass)) go to 70
c
c  at least one sequence has passed the convergence test. 
c  store variables before iterating
c
        svu = u
        svv = v
        do 10 i=1,n
          svk(i) = k(i)
   10   continue
        s = ss
c
c choose iteration according to the fastest converging sequence
c
        vtry = .false.
        stry = .false.
        if (spass .and. ((.not.vpass) .or.
     *   tss.lt.tvv)) go to 40
   20   call dquadi(ui, vi, nz)
        if (nz.gt.0) return
c
c quadratic iteration has failed. flag that it has
c been tried and decrease the convergence criterion.
c
        vtry = .true.
        betav = betav*.25
c
c try linear iteration if it has not been tried and
c the s sequence is converging
c
        if (stry .or. (.not.spass)) go to 50
        do 30 i=1,n
          k(i) = svk(i)
   30   continue
   40   call dreali(s, nz, iflag)
        if (nz.gt.0) return
c
c linear iteration has failed. flag that it has been
c tried and decrease the convergence criterion
c
        stry = .true.
        betas = betas*.25
        if (iflag.eq.0) go to 50
c
c  if linear iteration signals an almost double real zero, attempt quadratic 
c  interation.
c
        ui = -(s+s)
        vi = s*s
        go to 20
c
c restore variables
c
   50   u = svu
        v = svv
        do 60 i=1,n
          k(i) = svk(i)
   60   continue
c
c try quadratic iteration if it has not been tried
c and the v sequence is converging
c
        if (vpass .and. (.not.vtry)) go to 20
c
c recompute qp and scalar values to continue the second stage
c
        call dquads(nn, u, v, p, qp, a, b)
        call dcalcs(type)
   70   ovv = vv
        oss = ss
        otv = tv
        ots = ts
   80 continue
      return
      end
      subroutine dquadi(uu, vv, nz)

c*********************************************************************72
c
cc dquadi ???
c
c variable-shift k-polynomial iteration for a
c quadratic factor converges only if the zeros are
c equimodular or nearly so.
c
c uu,vv - coefficients of starting quadratic
c nz - number of zero found
c
      integer maxord
      parameter (maxord=101)
c
      common /dcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
      double precision p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      double precision eta, are, mre
      integer n, nn
      double precision ui, vi, uu, vv
      double precision mp, omp, ee, relstp, t, zm
      integer nz, type, i, j
      logical tried
c
      nz = 0
      tried = .false.
      u = uu
      v = vv
      j = 0
c
c main loop
c
   10 call dquad(1.0, u, v, szr, szi, lzr, lzi)
c
c return if roots of the quadratic are real and not
c close to multiple or nearly equal and  of opposite sign
c
      if (abs(abs(szr)-abs(lzr)).gt.0.01*abs(lzr)) return
c
c evaluate polynomial by quadratic synthetic division
c
      call dquads(nn, u, v, p, qp, a, b)
      mp = abs(a-szr*b) + abs(szi*b)
c
c compute a rigorous  bound on the rounding error in evaluting p
c
      zm = sqrt(abs(v))
      ee = 2.*abs(qp(1))
      t = -szr*b
      do 20 i=2,n
        ee = ee*zm + abs(qp(i))
   20 continue
      ee = ee*zm + abs(a+t)
      ee = (5.*mre+4.*are)*ee - (5.*mre+2.*are)*(abs(a+t)+abs(b)*zm) +
     * 2.*are*abs(t)
c
c iteration has converged sufficiently if the
c polynomial value is less than 20 times this bound
c
      if (mp.gt.20.*ee) go to 30
      nz = 2
      return
   30 j = j + 1
c stop iteration after 20 steps
      if (j.gt.20) return
      if (j.lt.2) go to 50
      if (relstp.gt..01 .or. mp.lt.omp .or. tried)
     * go to 50
c a cluster appears to be stalling the convergence.
c five fixed shift steps are taken with a u,v close
c to the cluster
      if (relstp.lt.eta) relstp = eta
      relstp = sqrt(relstp)
      u = u - u*relstp
      v = v + v*relstp
      call dquads(nn, u, v, p, qp, a, b)
      do 40 i=1,5
        call dcalcs(type)
        call dnextk(type)
   40 continue
      tried = .true.
      j = 0
   50 omp = mp
c calculate next k polynomial and new u and v
      call dcalcs(type)
      call dnextk(type)
      call dcalcs(type)
      call dnuest(type, ui, vi)
c if vi is zero the iteration is not converging
      if (vi.eq.0.0) return
      relstp = abs((vi-v)/vi)
      u = ui
      v = vi
      go to 10
      end
      subroutine dreali(sss, nz, iflag)

c*********************************************************************72
c
cc dreali ???
c
c variable-shift h polynomial iteration for a real zero.
c
c sss   - starting iterate
c nz    - number of zero found
c iflag - flag to indicate a pair of zeros near real axis.
c
      integer maxord
      parameter (maxord=101)
c
      common /dcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
      double precision p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      double precision eta, are, mre
      integer n, nn
      double precision pv, kv, t, s, sss
      double precision ms, mp, omp, ee
      integer nz, iflag, i, j, nm1
      nm1 = n - 1
      nz = 0
      s = sss
      iflag = 0
      j = 0
c main loop
   10 pv = p(1)
c evaluate p at s
      qp(1) = pv
      do 20 i=2,nn
        pv = pv*s + p(i)
        qp(i) = pv
   20 continue
      mp = abs(pv)
c
c compute a rigorous bound on the error in evaluating p
c
      ms = abs(s)
      ee = (mre/(are+mre))*abs(qp(1))
      do 30 i=2,nn
        ee = ee*ms + abs(qp(i))
   30 continue
c iteration has converged sufficiently if the
c polynomial value is less than 20 times this bound
      if (mp.gt.20.*((are+mre)*ee-mre*mp)) go to 40
      nz = 1
      szr = s
      szi = 0.0
      return
   40 j = j + 1
c stop iteration after 10 steps
      if (j.gt.10) return
      if (j.lt.2) go to 50
      if (abs(t).gt..001*abs(s-t) .or. mp.le.omp)
     * go to 50
c a cluster of zeros near the real axis has been
c encountered return with iflag set to initiate a
c quadratic iteration
      iflag = 1
      sss = s
      return
c return if the polynomial value has increased
c significantly
   50 omp = mp
c compute t, the next polynomial, and the new iterate
      kv = k(1)
      qk(1) = kv
      do 60 i=2,n
        kv = kv*s + k(i)
        qk(i) = kv
   60 continue
      if (abs(kv).le.abs(k(n))*10.*eta) go to 80
c use the scaled form of the recurrence if the value
c of k at s is nonzero
      t = -pv/kv
      k(1) = qp(1)
      do 70 i=2,n
        k(i) = t*qk(i-1) + qp(i)
   70 continue
      go to 100
c use unscaled form
   80 k(1) = 0.0
      do 90 i=2,n
        k(i) = qk(i-1)
   90 continue
  100 kv = k(1)
      do 110 i=2,n
        kv = kv*s + k(i)
  110 continue
      t = 0.0
      if (abs(kv).gt.abs(k(n))*10.*eta) t = -pv/kv
      s = s + t
      go to 10
      end
      subroutine dcalcs(type)

c*********************************************************************72
c
cc dcalcs ???
c
c this routine calculates scalar quantities used to
c compute the next k polynomial and new estimates of
c the quadratic coefficients.
c type - integer variable set here indicating how the
c calculations are normalized to avoid overflow
c
      integer maxord
      parameter (maxord=101)
      common /dcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
      double precision p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      double precision eta, are, mre
      integer n, nn
      integer type
c synthetic division of k by the quadratic 1,u,v
      call dquads(n, u, v, k, qk, c, d)
      if (abs(c).gt.abs(k(n))*100.*eta) go to 10
      if (abs(d).gt.abs(k(n-1))*100.*eta) go to 10
      type = 3
c
c type=3 indicates the quadratic is almost a factor of k
c
      return
   10 if (abs(d).lt.abs(c)) go to 20
      type = 2
c type=2 indicates that all formulas are divided by d
      e = a/d
      f = c/d
      g = u*b
      h = v*b
      a3 = (a+g)*e + h*(b/d)
      a1 = b*f - a
      a7 = (f+u)*a + h
      return
   20 type = 1
c
c type=1 indicates that all formulas are divided by c
c
      e = a/c
      f = d/c
      g = u*e
      h = v*b
      a3 = a*e + (h/c+g)*b
      a1 = b - a*(d/c)
      a7 = a + g*d + h*f
      return
      end
      subroutine dnextk(type)

c*********************************************************************72
c
cc dnextk ???
c
c computes the next k polynomials using scalars computed in dcalcs
c
      integer maxord
      parameter (maxord=101)
c
      common /dcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
      double precision p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      double precision eta, are, mre
      integer n, nn
      double precision temp
      integer type
c
      if (type.eq.3) go to 40
      temp = a
      if (type.eq.1) temp = b
      if (abs(a1).gt.abs(temp)*eta*10.) go to 20
c
c if a1 is nearly zero then use a special form of the recurrence
c
      k(1) = 0.0
      k(2) = -a7*qp(1)
      do 10 i=3,n
        k(i) = a3*qk(i-2) - a7*qp(i-1)
   10 continue
      return
c
c use scaled form of the recurrence
c
   20 a7 = a7/a1
      a3 = a3/a1
      k(1) = qp(1)
      k(2) = qp(2) - a7*qp(1)
      do 30 i=3,n
        k(i) = a3*qk(i-2) - a7*qp(i-1) + qp(i)
   30 continue
      return
c
c use unscaled form of the recurrence if type is 3
c
   40 k(1) = 0.0
      k(2) = 0.0
      do 50 i=3,n
        k(i) = qk(i-2)
   50 continue
      return
      end
      subroutine dnuest(type, uu, vv)

c*********************************************************************72
c
cc dnuest ???
c
c compute new estimates of the quadratic coefficients
c using the scalars computed in dcalcs.
c
      integer maxord
      parameter (maxord=101)
c
      common /dcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
      double precision p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      double precision eta, are, mre
      integer n, nn
      double precision a4, a5, b1, b2, c1, c2, c3,
     * c4, temp, uu, vv
      integer type
c
c use formulas appropriate to setting of type.
c
      if (type.eq.3) go to 30
      if (type.eq.2) go to 10
      a4 = a + u*b + h*f
      a5 = c + (u+v*f)*d
      go to 20
   10 a4 = (a+g)*f + h
      a5 = (f+u)*c + v*d
c evaluate new quadratic coefficients.
   20 b1 = -k(n)/p(nn)
      b2 = -(k(n-1)+b1*p(n))/p(nn)
      c1 = v*b2*a1
      c2 = b1*a7
      c3 = b1*b1*a3
      c4 = c1 - c2 - c3
      temp = a5 + b1*a4 - c4
      if (temp.eq.0.0) go to 30
      uu = u - (u*(c3+c2)+v*(b1*a1+b2*a7))/temp
      vv = v*(1.+c4/temp)
      return
c if type=3 the quadratic is zeroed
   30 uu = 0.0
      vv = 0.0
      return
      end
      subroutine dquads(nn, u, v, p, q, a, b)
c
c divides p by the quadratic  1,u,v  placing the
c quotient in q and the remainder in a,b
c
      double precision p(nn), q(nn), u, v, a, b, c
      integer i
c
      b = p(1)
      q(1) = b
      a = p(2) - u*b
      q(2) = a
      do 10 i=3,nn
        c = p(i) - u*a - v*b
        q(i) = c
        b = a
        a = c
   10 continue
      return
      end
      subroutine dquad(a, b1, c, sr, si, lr, li)

c*********************************************************************72
c
cc dquad ???
c
c calculate the zeros of the quadratic a*z**2+b1*z+c.
c the quadratic formula, modified to avoid
c overflow, is used to find the larger zero if the
c zeros are real and both zeros are complex.
c the smaller real zero is found directly from the
c product of the zeros c/a.
c
      double precision a, b1, c, sr, si, lr, li, b, d, e
c
      if (a.ne.0.0) go to 20
      sr = 0.0
      if (b1.ne.0.0) sr = -c/b1
      lr = 0.0
   10 si = 0.0
      li = 0.0
      return
   20 if (c.ne.0.0) go to 30
      sr = 0.0
      lr = -b1/a
      go to 10
c
c compute discriminant avoiding overflow
c
   30 b = b1/2.0
      if (abs(b).lt.abs(c)) go to 40
      e = 1.0 - (a/b)*(c/b)
      d = sqrt(abs(e))*abs(b)
      go to 50
   40 e = a
      if (c.lt.0.0) e = -a
      e = b*(b/abs(c)) - e
      d = sqrt(abs(e))*sqrt(abs(c))
   50 if (e.lt.0.0) go to 60
c
c real zeros
c
      if (b.ge.0.0) d = -d
      lr = (-b+d)/a
      sr = 0.0
      if (lr.ne.0.0) sr = (c/lr)/a
      go to 10
c
c complex conjugate zeros
c
   60 sr = -b/a
      lr = sr
      si = abs(d/a)
      li = -si
      return
      end
      subroutine rpoly(coef,ndeg,nzero,zeror,zeroi,fail)

c*********************************************************************72
c
cc rpoly ???
c
c  rpoly finds the roots or "zeroes" of a real polynomial.  
c
c  rpoly uses real arithmetic for its calculations.  the roots of 
c  the polynomial are returned as pairs of real values, 
c  containing the real and imaginary parts of the roots.
c
c  rpoly is a slightly modified version of acm toms algorithm 493.
c
c  coef   input, real coef(ndeg+1).
c         coef contains the polynomial coefficients.  
c
c         coef(1) contains the coefficient of x**(ndeg), 
c         coef(2) contains the coefficient of x**(ndeg-1), and so on, up to
c         coef(ndeg) contains the constant term.
c
c         coef(1) may not be zero.  if the input value of coef(1) is zero,
c         the program sets fail=.true., prints an error message, and returns
c         immediately without performing any computations.
c
c  ndeg   input, integer ndeg.
c         ndeg is the degree of the polynomial.
c         for example, the polynomial 97*x*x + 14*x + 3 has degree 2.
c
c         the program imposes a limit on the maximum allowable value of ndeg.
c         currently, this maximum value is 100.
c
c  nzero  output, integer nzero.
c         nzero is the number of zeroes found by the program.
c         normally, the output value of nzero will be equal to ndeg.
c         in some cases, however, the program will not be able to find
c         the full set of zeroes, and so nzero may be less than ndeg.
c         in the worst case, nzero would be zero, representing the fact
c         that the program was unable to find any roots at all.
c
c  zeror, 
c  zeroi  output, real zeror(nzero), zeroi(nzero).
c         zeror and zeroi contain the real and imaginary parts of the
c         roots of the polynomial.  for instance, the location of the
c         first zero of the polynomial is zeror(1) + i * zeroi(1).
c
c  fail   output, logical fail.
c         
c         fail is .true. if a fatal error occurred in the program, in which
c         case no roots were found, or if the iterative method failed,
c         and hence only some roots were found.  the program will have
c         returned nzero roots, which may be 0, ndeg, or some intermediate
c         value.
c
c         fail is .false. if no fatal error occurred, and the iteration
c         did not fail.  in that case, all is well, and the program should
c         return nzero roots, where nzero=ndeg.
c
      integer maxord
      parameter (maxord=101)
c
      common /rcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
c
      real p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      real eta, are, mre
      integer n, nn
      real coef(*), temp(maxord),
     * zeror(*), zeroi(*), t, aa, bb, cc,
     * factor
      real pt(maxord), lo, xmax,xmin, xx, yy, cosr,
     * sinr, xxx, x, sc, bnd, xm, ff, df, dx, infin,
     * smalno, base
      integer nzero, cnt, nz, i, j, jj, nm1
      logical fail, zerok
c
c the following statements set machine constants used
c in various parts of the program. the meaning of the
c four constants are...
c
c eta     the maximum relative representation error
c         which can be described as the smallest
c         positive floating point number such that
c         1.0+eta is greater than 1.
c
c infiny  the largest floating-point number.
c
c smalno  the smallest positive floating-point number
c
c base    the base of the floating-point number system used.
c
      base = i1mach(7)
      eta = r1mach(4)
      infin = r1mach(2)
      smalno = r1mach(1)
c
c are and mre refer to the unit error in + and *
c respectively. they are assumed to be the same as eta.
c
      are = eta
      mre = eta
      lo = smalno/eta
c
c initialization of constants for shift rotation
c
      xx = .70710678
      yy = -xx
      cosr = -.069756474
      sinr = .99756405
      fail = .false.
      n = ndeg
      nzero=ndeg
      nn = n + 1
c
      if(ndeg.ge.maxord)then
        write(*,*)' '
        write(*,*)'rpoly cannot solve this problem!'
        write(*,*)'rpoly cannot handle degrees greater than ',maxord-1
        write(*,*)'however, the input value of ndeg is ',ndeg
        fail=.true.
        nzero=0
        return
        endif
c
c  the algorithm fails if the leading coefficient is zero.
c
      if(coef(1).eq.0.0)then
        write(*,*)' '
        write(*,*)'rpoly cannot solve this problem!'
        write(*,*)'the input value of coef(1) is zero.'
        write(*,*)'this is illegal.'
        write(*,*)' '
        fail = .true.
        nzero = 0
        return
        endif
c
c  remove the zeros at the origin if any
c
   10 continue
      if (coef(nn).eq.0.0)then
        j = nzero - n + 1
        zeror(j) = 0.0
        zeroi(j) = 0.0
        nn = nn - 1
        n = n - 1
        go to 10
        endif
c
c  make a copy of the coefficients
c
      do 30 i=1,nn
        p(i) = coef(i)
   30 continue
c
c start the algorithm for one zero
c
   40 continue

      if(n.lt.1)then
        return
      elseif(n.eq.1)then
        zeror(nzero) = -p(2)/p(1)
        zeroi(nzero) = 0.0
        return
      elseif(n.eq.2)then
        call rquad(p(1), p(2), p(3), zeror(nzero-1),
     *  zeroi(nzero-1), zeror(nzero), zeroi(nzero))
        return
        endif
c
c find largest and smallest moduli of coefficients.
c
      xmax = 0.
      xmin = infin
      do 70 i=1,nn
        x = abs(p(i))
        if (x.gt.xmax) xmax = x
        if (x.ne.0. .and. x.lt.xmin) xmin = x
   70 continue
c
c scale if there are large or very small coefficients
c computes a scale factor to multiply the
c coefficients of the polynomial. the scaling is done
c to avoid overflow and to avoid undetected underflow
c interfering with the convergence criterion.
c the factor is a power of the base
c
      sc = lo/xmin
      if (sc.gt.1.0) go to 80
      if (xmax.lt.10.) go to 110
      if (sc.eq.0.) sc = smalno
      go to 90
   80 if (infin/sc.lt.xmax) go to 110
   90 l = log(sc)/log(base) + .5
      factor = (base*1.0)**l
      if (factor.eq.1.0) go to 110
      do 100 i=1,nn
        p(i) = factor*p(i)
  100 continue
c
c compute lower bound on moduli of zeros.
c
  110 do 120 i=1,nn
        pt(i) = abs(p(i))
  120 continue
      pt(nn) = -pt(nn)
c
c compute upper estimate of bound
c
      x = exp((log(-pt(nn))-log(pt(1)))/n)
      if (pt(n).eq.0.) go to 130
c
c if newton step at the origin is better, use it.
c
      xm = -pt(nn)/pt(n)
      if (xm.lt.x) x = xm
c
c chop the interval (0,x) until ff .le. 0
c
  130 xm = x*.1
      ff = pt(1)
      do 140 i=2,nn
        ff = ff*xm + pt(i)
  140 continue
      if (ff.le.0.) go to 150
      x = xm
      go to 130
  150 dx = x
c
c do newton iteration until x converges to two decimal places
c
  160 if (abs(dx/x).le..005) go to 180
      ff = pt(1)
      df = ff
      do 170 i=2,n
        ff = ff*x + pt(i)
        df = df*x + ff
  170 continue
      ff = ff*x + pt(nn)
      dx = ff/df
      x = x - dx
      go to 160
  180 bnd = x
c
c compute the derivative as the intial k polynomial
c and do 5 steps with no shift
c
      nm1 = n - 1
      do 190 i=2,n
        k(i) = float(nn-i)*p(i)/float(n)
  190 continue
      k(1) = p(1)
      aa = p(nn)
      bb = p(n)
      zerok = k(n).eq.0.0
      do 230 jj=1,5
        cc = k(n)
        if (zerok) go to 210
c
c use scaled form of recurrence if value of k at 0 is nonzero
c
        t = -aa/cc
        do 200 i=1,nm1
          j = nn - i
          k(j) = t*k(j-1) + p(j)
  200   continue
        k(1) = p(1)
        zerok = abs(k(n)).le.abs(bb)*eta*10.
        go to 230
c
c use unscaled form of recurrence
c
  210   do 220 i=1,nm1
          j = nn - i
          k(j) = k(j-1)
  220   continue
        k(1) = 0.0
        zerok = k(n).eq.0.0
  230 continue
c
c save k for restarts with new shifts
c
      do 240 i=1,n
        temp(i) = k(i)
  240 continue
c
c loop to select the quadratic  corresponding to each new shift
c
      do 280 cnt=1,20
c
c quadratic corresponds to a double shift to a
c non-real point and its complex conjugate. the point
c has modulus bnd and amplitude rotated by 94 degrees
c from the previous shift
c
        xxx = cosr*xx - sinr*yy
        yy = sinr*xx + cosr*yy
        xx = xxx
        sr = bnd*xx
        si = bnd*yy
        u = -2.0*sr
        v = bnd
c
c second stage calculation, fixed quadratic
c
        call rfxshf(20*cnt, nz)
        if (nz.eq.0) go to 260
c
c the second stage jumps directly to one of the third
c stage iterations and returns here if successful.
c deflate the polynomial, store the zero or zeros and
c return to the main algorithm.
c
        j = nzero - n + 1
        zeror(j) = szr
        zeroi(j) = szi
        nn = nn - nz
        n = nn - 1
        do 250 i=1,nn
          p(i) = qp(i)
  250   continue
        if (nz.eq.1) go to 40
        zeror(j+1) = lzr
        zeroi(j+1) = lzi
        go to 40
c
c if the iteration is unsuccessful another quadratic
c is chosen after restoring k
c
  260   do 270 i=1,n
          k(i) = temp(i)
  270   continue
  280 continue
c
c return with failure if no convergence with 20 shifts
c
      fail = .true.
      nzero = nzero - n
      return
      end
      subroutine rfxshf(l2, nz)

c*********************************************************************72
c
cc rfxshf ???
c
c
c  rfxshf computes up to l2 fixed shift k-polynomials,
c  testing for convergence in the linear or quadratic
c  case. initiates one of the variable shift
c  iterations and returns with the number of zeros
c  found.
c
c l2 - limit of fixed shift steps
c nz - number of zeros found
c
      integer maxord
      parameter (maxord=101)
c
      common /rcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
      real p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      real eta, are, mre
      integer n, nn
      real svu, svv, ui, vi, s
      real betas, betav, oss, ovv, ss, vv, ts, tv,
     * ots, otv, tvv, tss
      integer l2, nz, type, i, j, iflag
      logical vpass, spass, vtry, stry
c
      nz = 0
      betav = .25
      betas = .25
      oss = sr
      ovv = v
c
c evaluate polynomial by synthetic division
c
      call rquads(nn, u, v, p, qp, a, b)
      call rcalcs(type)
      do 80 j=1,l2
c
c calculate next k polynomial and estimate v
c
        call rnextk(type)
        call rcalcs(type)
        call rnuest(type, ui, vi)
        vv = vi
c
c estimate s
c
        ss = 0.
        if (k(n).ne.0.0) ss = -p(nn)/k(n)
        tv = 1.
        ts = 1.
        if (j.eq.1 .or. type.eq.3) go to 70
c
c compute relative measures of convergence of s and v sequences
c
        if (vv.ne.0.) tv = abs((vv-ovv)/vv)
        if (ss.ne.0.) ts = abs((ss-oss)/ss)
c
c if decreasing, multiply two most recent convergence measures
c
        tvv = 1.
        if (tv.lt.otv) tvv = tv*otv
        tss = 1.
        if (ts.lt.ots) tss = ts*ots
c
c compare with convergence criteria
c
        vpass = tvv.lt.betav
        spass = tss.lt.betas
        if (.not.(spass .or. vpass)) go to 70
c
c  at least one sequence has passed the convergence test. 
c  store variables before iterating
c
        svu = u
        svv = v
        do 10 i=1,n
          svk(i) = k(i)
   10   continue
        s = ss
c
c choose iteration according to the fastest converging sequence
c
        vtry = .false.
        stry = .false.
        if (spass .and. ((.not.vpass) .or.
     *   tss.lt.tvv)) go to 40
   20   call rquadi(ui, vi, nz)
        if (nz.gt.0) return
c
c quadratic iteration has failed. flag that it has
c been tried and decrease the convergence criterion.
c
        vtry = .true.
        betav = betav*.25
c
c try linear iteration if it has not been tried and
c the s sequence is converging
c
        if (stry .or. (.not.spass)) go to 50
        do 30 i=1,n
          k(i) = svk(i)
   30   continue
   40   call rreali(s, nz, iflag)
        if (nz.gt.0) return
c
c linear iteration has failed. flag that it has been
c tried and decrease the convergence criterion
c
        stry = .true.
        betas = betas*.25
        if (iflag.eq.0) go to 50
c
c  if linear iteration signals an almost double real zero, attempt quadratic 
c  interation.
c
        ui = -(s+s)
        vi = s*s
        go to 20
c
c restore variables
c
   50   u = svu
        v = svv
        do 60 i=1,n
          k(i) = svk(i)
   60   continue
c
c try quadratic iteration if it has not been tried
c and the v sequence is converging
c
        if (vpass .and. (.not.vtry)) go to 20
c
c recompute qp and scalar values to continue the second stage
c
        call rquads(nn, u, v, p, qp, a, b)
        call rcalcs(type)
   70   ovv = vv
        oss = ss
        otv = tv
        ots = ts
   80 continue
      return
      end
      subroutine rquadi(uu, vv, nz)

c*********************************************************************72
c
cc rquadi ???
c
c
c variable-shift k-polynomial iteration for a
c quadratic factor converges only if the zeros are
c equimodular or nearly so.
c
c uu,vv - coefficients of starting quadratic
c nz - number of zero found
c
      integer maxord
      parameter (maxord=101)
c
      common /rcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
      real p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      real eta, are, mre
      integer n, nn
      real ui, vi, uu, vv
      real mp, omp, ee, relstp, t, zm
      integer nz, type, i, j
      logical tried
c
      nz = 0
      tried = .false.
      u = uu
      v = vv
      j = 0
c
c main loop
c
   10 call rquad(1.0, u, v, szr, szi, lzr, lzi)
c
c return if roots of the quadratic are real and not
c close to multiple or nearly equal and  of opposite sign
c
      if (abs(abs(szr)-abs(lzr)).gt.0.01*abs(lzr)) return
c
c evaluate polynomial by quadratic synthetic division
c
      call rquads(nn, u, v, p, qp, a, b)
      mp = abs(a-szr*b) + abs(szi*b)
c
c compute a rigorous  bound on the rounding error in evaluting p
c
      zm = sqrt(abs(v))
      ee = 2.*abs(qp(1))
      t = -szr*b
      do 20 i=2,n
        ee = ee*zm + abs(qp(i))
   20 continue
      ee = ee*zm + abs(a+t)
      ee = (5.*mre+4.*are)*ee - (5.*mre+2.*are)*(abs(a+t)+abs(b)*zm) +
     * 2.*are*abs(t)
c
c iteration has converged sufficiently if the
c polynomial value is less than 20 times this bound
c
      if (mp.gt.20.*ee) go to 30
      nz = 2
      return
   30 j = j + 1
c stop iteration after 20 steps
      if (j.gt.20) return
      if (j.lt.2) go to 50
      if (relstp.gt..01 .or. mp.lt.omp .or. tried)
     * go to 50
c a cluster appears to be stalling the convergence.
c five fixed shift steps are taken with a u,v close
c to the cluster
      if (relstp.lt.eta) relstp = eta
      relstp = sqrt(relstp)
      u = u - u*relstp
      v = v + v*relstp
      call rquads(nn, u, v, p, qp, a, b)
      do 40 i=1,5
        call rcalcs(type)
        call rnextk(type)
   40 continue
      tried = .true.
      j = 0
   50 omp = mp
c calculate next k polynomial and new u and v
      call rcalcs(type)
      call rnextk(type)
      call rcalcs(type)
      call rnuest(type, ui, vi)
c if vi is zero the iteration is not converging
      if (vi.eq.0.0) return
      relstp = abs((vi-v)/vi)
      u = ui
      v = vi
      go to 10
      end
      subroutine rreali(sss, nz, iflag)

c*********************************************************************72
c
cc rreali ???
c
c
c variable-shift h polynomial iteration for a real zero.
c
c sss   - starting iterate
c nz    - number of zero found
c iflag - flag to indicate a pair of zeros near real axis.
c
      integer maxord
      parameter (maxord=101)
c
      common /rcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
      real p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      real eta, are, mre
      integer n, nn
      real pv, kv, t, s, sss
      real ms, mp, omp, ee
      integer nz, iflag, i, j, nm1
      nm1 = n - 1
      nz = 0
      s = sss
      iflag = 0
      j = 0
c main loop
   10 pv = p(1)
c evaluate p at s
      qp(1) = pv
      do 20 i=2,nn
        pv = pv*s + p(i)
        qp(i) = pv
   20 continue
      mp = abs(pv)
c
c compute a rigorous bound on the error in evaluating p
c
      ms = abs(s)
      ee = (mre/(are+mre))*abs(qp(1))
      do 30 i=2,nn
        ee = ee*ms + abs(qp(i))
   30 continue
c iteration has converged sufficiently if the
c polynomial value is less than 20 times this bound
      if (mp.gt.20.*((are+mre)*ee-mre*mp)) go to 40
      nz = 1
      szr = s
      szi = 0.0
      return
   40 j = j + 1
c stop iteration after 10 steps
      if (j.gt.10) return
      if (j.lt.2) go to 50
      if (abs(t).gt..001*abs(s-t) .or. mp.le.omp)
     * go to 50
c a cluster of zeros near the real axis has been
c encountered return with iflag set to initiate a
c quadratic iteration
      iflag = 1
      sss = s
      return
c return if the polynomial value has increased
c significantly
   50 omp = mp
c compute t, the next polynomial, and the new iterate
      kv = k(1)
      qk(1) = kv
      do 60 i=2,n
        kv = kv*s + k(i)
        qk(i) = kv
   60 continue
      if (abs(kv).le.abs(k(n))*10.*eta) go to 80
c use the scaled form of the recurrence if the value
c of k at s is nonzero
      t = -pv/kv
      k(1) = qp(1)
      do 70 i=2,n
        k(i) = t*qk(i-1) + qp(i)
   70 continue
      go to 100
c use unscaled form
   80 k(1) = 0.0
      do 90 i=2,n
        k(i) = qk(i-1)
   90 continue
  100 kv = k(1)
      do 110 i=2,n
        kv = kv*s + k(i)
  110 continue
      t = 0.0
      if (abs(kv).gt.abs(k(n))*10.*eta) t = -pv/kv
      s = s + t
      go to 10
      end
      subroutine rcalcs(type)

c*********************************************************************72
c
cc rcalcs ???
c
c this routine calculates scalar quantities used to
c compute the next k polynomial and new estimates of
c the quadratic coefficients.
c type - integer variable set here indicating how the
c calculations are normalized to avoid overflow
c
      integer maxord
      parameter (maxord=101)
      common /rcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
      real p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      real eta, are, mre
      integer n, nn
      integer type
c synthetic division of k by the quadratic 1,u,v
      call rquads(n, u, v, k, qk, c, d)
      if (abs(c).gt.abs(k(n))*100.*eta) go to 10
      if (abs(d).gt.abs(k(n-1))*100.*eta) go to 10
      type = 3
c
c type=3 indicates the quadratic is almost a factor of k
c
      return
   10 if (abs(d).lt.abs(c)) go to 20
      type = 2
c type=2 indicates that all formulas are divided by d
      e = a/d
      f = c/d
      g = u*b
      h = v*b
      a3 = (a+g)*e + h*(b/d)
      a1 = b*f - a
      a7 = (f+u)*a + h
      return
   20 type = 1
c
c type=1 indicates that all formulas are divided by c
c
      e = a/c
      f = d/c
      g = u*e
      h = v*b
      a3 = a*e + (h/c+g)*b
      a1 = b - a*(d/c)
      a7 = a + g*d + h*f
      return
      end
      subroutine rnextk(type)

c*********************************************************************72
c
cc rnextk ???
c
c
c computes the next k polynomials using scalars computed in rcalcs
c
      integer maxord
      parameter (maxord=101)
c
      common /rcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
      real p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      real eta, are, mre
      integer n, nn
      real temp
      integer type
c
      if (type.eq.3) go to 40
      temp = a
      if (type.eq.1) temp = b
      if (abs(a1).gt.abs(temp)*eta*10.) go to 20
c
c if a1 is nearly zero then use a special form of the recurrence
c
      k(1) = 0.0
      k(2) = -a7*qp(1)
      do 10 i=3,n
        k(i) = a3*qk(i-2) - a7*qp(i-1)
   10 continue
      return
c
c use scaled form of the recurrence
c
   20 a7 = a7/a1
      a3 = a3/a1
      k(1) = qp(1)
      k(2) = qp(2) - a7*qp(1)
      do 30 i=3,n
        k(i) = a3*qk(i-2) - a7*qp(i-1) + qp(i)
   30 continue
      return
c
c use unscaled form of the recurrence if type is 3
c
   40 k(1) = 0.0
      k(2) = 0.0
      do 50 i=3,n
        k(i) = qk(i-2)
   50 continue
      return
      end
      subroutine rnuest(type, uu, vv)

c*********************************************************************72
c
cc rnuest ???
c
c
c compute new estimates of the quadratic coefficients
c using the scalars computed in rcalcs.
c
      integer maxord
      parameter (maxord=101)
c
      common /rcomon/ p, qp, k, qk, svk, sr, si, u,
     * v, a, b, c, d, a1, a2, a3, a6, a7, e, f, g,
     * h, szr, szi, lzr, lzi, eta, are, mre, n, nn
      real p(maxord), qp(maxord), k(maxord),
     * qk(maxord), svk(maxord), sr, si, u, v, a, b, c, d,
     * a1, a2, a3, a6, a7, e, f, g, h, szr, szi,
     * lzr, lzi
      real eta, are, mre
      integer n, nn
      real a4, a5, b1, b2, c1, c2, c3,
     * c4, temp, uu, vv
      integer type
c
c use formulas appropriate to setting of type.
c
      if (type.eq.3) go to 30
      if (type.eq.2) go to 10
      a4 = a + u*b + h*f
      a5 = c + (u+v*f)*d
      go to 20
   10 a4 = (a+g)*f + h
      a5 = (f+u)*c + v*d
c evaluate new quadratic coefficients.
   20 b1 = -k(n)/p(nn)
      b2 = -(k(n-1)+b1*p(n))/p(nn)
      c1 = v*b2*a1
      c2 = b1*a7
      c3 = b1*b1*a3
      c4 = c1 - c2 - c3
      temp = a5 + b1*a4 - c4
      if (temp.eq.0.0) go to 30
      uu = u - (u*(c3+c2)+v*(b1*a1+b2*a7))/temp
      vv = v*(1.+c4/temp)
      return
c if type=3 the quadratic is zeroed
   30 uu = 0.0
      vv = 0.0
      return
      end
      subroutine rquads(nn, u, v, p, q, a, b)

c*********************************************************************72
c
cc rquads ???
c
c
c divides p by the quadratic  1,u,v  placing the
c quotient in q and the remainder in a,b
c
      real p(nn), q(nn), u, v, a, b, c
      integer i
c
      b = p(1)
      q(1) = b
      a = p(2) - u*b
      q(2) = a
      do 10 i=3,nn
        c = p(i) - u*a - v*b
        q(i) = c
        b = a
        a = c
   10 continue
      return
      end
      subroutine rquad(a, b1, c, sr, si, lr, li)

c*********************************************************************72
c
cc rquad ???
c
c
c calculate the zeros of the quadratic a*z**2+b1*z+c.
c the quadratic formula, modified to avoid
c overflow, is used to find the larger zero if the
c zeros are real and both zeros are complex.
c the smaller real zero is found directly from the
c product of the zeros c/a.
c
      real a, b1, c, sr, si, lr, li, b, d, e
c
      if (a.ne.0.0) go to 20
      sr = 0.0
      if (b1.ne.0.0) sr = -c/b1
      lr = 0.0
   10 si = 0.0
      li = 0.0
      return
   20 if (c.ne.0.0) go to 30
      sr = 0.0
      lr = -b1/a
      go to 10
c
c compute discriminant avoiding overflow
c
   30 b = b1/2.0
      if (abs(b).lt.abs(c)) go to 40
      e = 1.0 - (a/b)*(c/b)
      d = sqrt(abs(e))*abs(b)
      go to 50
   40 e = a
      if (c.lt.0.0) e = -a
      e = b*(b/abs(c)) - e
      d = sqrt(abs(e))*sqrt(abs(c))
   50 if (e.lt.0.0) go to 60
c
c real zeros
c
      if (b.ge.0.0) d = -d
      lr = (-b+d)/a
      sr = 0.0
      if (lr.ne.0.0) sr = (c/lr)/a
      go to 10
c
c complex conjugate zeros
c
   60 sr = -b/a
      lr = sr
      si = abs(d/a)
      li = -si
      return
      end
      subroutine dcpoly(coefr,coefi,ndeg,work,zeror,zeroi,fail)

c*********************************************************************72
c
cc dcpoly ???
c
c  dcpoly finds the roots or "zeroes" of a complex polynomial.  
c
c  dcpoly uses double precision arithmetic for its calculations.  the roots of 
c  the polynomial are returned as pairs of double precision values, 
c  containing the real and imaginary parts of the roots.
c
c  coefr,
c  coefi  input, double precision coefr(ndeg+1),coefi(ndeg+1).
c         coefr and coefi contain the real and imaginary parts of the 
c         coefficients of the polynomial.  the coefficient of the
c         highest degree term is coefr(1) + i * coefi(1).
c
c  ndeg   input, integer ndeg.
c         ndeg is the degree of the polynomial.  this is the value of the
c         highest exponent in the polynomial.  for example, the polynomial
c         3 * x**2 + 7 * x + 88 has degree 2.
c
c  work   workspace, double precision work(14*(ndeg+1)).
c
c  zeror,
c  zeroi  output, double precision zeror(ndeg), zeroi(ndeg).
c         zeror and zeroi contain the real and imaginary parts of the roots
c         of the polynomial.  
c
c  fail   output, logical fail.
c
c         if fail is .true., then either the leading polynomial coefficient
c         was zero on input, or the iteration was not able to find all the
c         zeroes.
c
c         if fail is .false., then no failure occurred, and all the zeroes
c         have been computed.
c

      double precision            coefr(2)     ,coefi(2)     ,zeror(2),
     1                zeroi(2)
      logical         fail
      double precision       work(1)
c
      n = ndeg+1
      call dc1pol(coefr,coefi,ndeg,zeror,zeroi,fail,work(1),work(n+1),
     1            work(2*n+1),work(3*n+1),work(4*n+1),work(5*n+1),
     2            work(6*n+1),work(7*n+1),work(8*n+1),work(9*n+1),
     3            work(10*n+1),work(11*n+1),work(12*n+1),work(13*n+1))

      return
      end
      subroutine dc1pol(coefr,coefi,degree,zeror,zeroi,fail,hr,hi,qpr,
     *qpi,qhr,qhi,sshr,sshi,pr,pi,q,pt,svhr,svhi)
c
c*****this routine finds the zeros of any complex polynomial
c
      double precision            coefr        ,coefi,zeror      ,
     1                zeroi
      common /dcomn1/ infiny     ,constl     ,are        ,mre        ,
     1                re         ,lnbase     ,smalno     ,neta       ,
     2                zeta       ,eta
      double precision            infiny     ,constl     ,are        ,
     1                mre        ,re         ,lnbase     ,smalno     ,
     2                neta       ,zeta       ,eta
      common /dcomn5/ r
      common /dcomn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      double precision            hr         ,hi         ,qpr        ,
     1                qpi        ,qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,
     3                t2         ,q          ,pt         ,svhr       ,
     4                svhi
      common /dcomn3/ l2         ,iflag
      double precision            base       ,
     1                precis     ,exphi      ,explo      ,pr         ,
     2                pi         ,sshr       ,sshi       ,bnd        ,
     3                angle      ,tpi
      double precision            dlog       ,dsqrt      ,dcauch     ,
     1                dcos       ,dsin       ,dcrand
      integer         degree     ,r          ,cnt1       ,cnt2
      logical         fail       ,round
      dimension       q(*)       ,pt(*)      ,svhr(*)    ,svhi(*)
      dimension       coefr(*) ,coefi(*)     ,zeror(*)   ,zeroi(*)   ,
     1                sshr(*)   ,sshi(*)   ,pr(*)     ,pi(*)     ,
     2                hr(*)      ,hi(*)      ,qpr(*)     ,qpi(*)     ,
     3                qhr(*)     ,qhi(*)
      tpi=6.2831853071795864769252867664d0
      fail = .false.
      call dcmcon (base,precis,exphi,explo,round)
      lnbase = dlog(base)
      infiny = (1.0d0-base**(1.0d0-precis))*base**(exphi-1.0d0)*base
      smalno = base**(explo+10.d0)/base**10
      zeta = base**precis+1.0d0
      constl = zeta*smalno*base
      eta = base**(1.d0-precis)
      if (round) eta = .5d0*eta
      are = eta
      mre = 2.d0*dsqrt(2.d0)*eta
      re = 1.d0+are
      r = 0
c
      if(coefr(1).eq.0.0.and.coefi(1).eq.0.0)then
        write(*,*)' '
        write(*,*)'dcpoly can not solve this problem!'
        write(*,*)'the leading polynomial coefficient is zero.'
        write(*,*)'check coefr(1) and coefi(1)!'
        fail = .true.
        return
        endif
c
c  make a copy of the coefficients
c
      nn = degree+1
      do 102 i=1,nn
         pr(i) = coefr(i)
         pi(i) = coefi(i)
  102 continue
c
103   continue
      iflag = 0
      call dcscle (nn,pr,pi)
      n = nn-1
  104 if (nn .gt. 2) go to 105
      call dcdivq (-pr(2),-pi(2),pr(1),pi(1),zeror(degree),
     1             zeroi(degree))
      return
  105 if (pr(nn).ne.0.d0 .or. pi(nn).ne.0.d0) go to 106
      idnn2 = degree-nn+2
      zeror(idnn2) = 0.d0
      zeroi(idnn2) = 0.d0
      nn = nn-1
      go to 104
  106 call dcnosh(nn,pr,pi,hr,hi)
      do 107 i=1,n
         sshr(i) = hr(i)
         sshi(i) = hi(i)
  107 continue
      bnd = dcauch(nn,pr,pi,q,pt)
      do 110 cnt1=1,2
         l2 = 10
         do 108 cnt2=1,8
            angle = tpi*dcrand(-1)
            sr = bnd*dcos(angle)
            si = bnd*dsin(angle)
            call dcfxsh (nn,degree,pr,pi,zeror,zeroi,hr,hi,qpr,qpi,qhr,
     1                   qhi,svhr,svhi)
            if (iflag .eq. 1) go to 103
            l2 = l2+10
  108    continue
         do 109 i=1,n
            hr(i) = sshr(i)
            hi(i) = sshi(i)
  109    continue
  110 continue

      write(*,*)' '
      write(*,*)'dcpoly was unable to solve this problem.'
      write(*,*)'the iteration did not converge, some of the roots'
      write(*,*)'could not be computed.'
      fail = .true.

      return
      end
      double precision function dcauch (nn,pr,pi,q,pt)

c*********************************************************************72
c
cc dcauch ???
c
c*****this routine computes a lower bound on the modulus of the zeros
c
      double precision            pr         ,pi
      double precision            pt         ,x          ,xm         ,
     1                f          ,dx         ,q          ,df
      double precision     dcmodq   ,dexp     ,
     1                dlog       ,dabs
      dimension       pr(*)      ,pi(*)      ,q(*)      ,pt(*)
c
      n = nn-1
      do 101 i=1,nn
         pt(i) = dcmodq(pr(i),pi(i))
  101 continue
      pt(nn) = -pt(nn)
      x = dexp((dlog(-pt(nn))-dlog(pt(1)))/float(n))
      if (pt(n) .eq. 0.0d0) go to 102
c
c*****if newton step at the origin is better, use it as the initial gues
c
      xm = -pt(nn)/pt(n)
      if (xm .lt. x) x = xm
c
c*****chop the interval (0,x) until f)=0
c
  102 xm = x*.1d0
      f = pt(1)
      do 103 i=2,nn
         f = f*xm+pt(i)
  103 continue
      if (f .le. 0.0d0) go to 104
      x = xm
      go to 102
  104 dx = x
  105 if (dabs(dx/x) .le. .005d0) go to 108
      q(1) = pt(1)
      do 106 i=2,nn
         q(i) = q(i-1)*x+pt(i)
  106 continue
      f = q(nn)
      df = q(1)
      do 107 i=2,n
         df = df*x+q(i)
  107 continue
      dx = f/df
      x = x-dx
      go to 105
  108 dcauch = x
      return
      end
      subroutine dcdefl(nn,degree,pr,pi,zeror,zeroi,qpr,qpi)

c*********************************************************************72
c
cc dcdefl ???
c
c*****this routine replaces (pr,pi) by the deflated polynomial, reduces
c*****nn, and stores the approximate zero.
c*****this routine returns zeror(i),zeroi(i),pr(i),pi(i),nn
c
      double precision            pr         ,pi         ,zeror      ,
     1                zeroi
      common /dcomn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      double precision            qpr        ,
     1                qpi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      integer         degree
      dimension       pr(*)      ,pi(*)      ,zeror(*)   ,zeroi(*)
      dimension       qpr(*)     ,qpi(*)
c
      nd = degree-nn+2
      zeror(nd) = sr
      zeroi(nd) = si
      nn = nn-1
      do 101 i=1,nn
         pr(i) = qpr(i)
         pi(i) = qpi(i)
  101 continue
      return
      end
      double precision function dcerrv (nn,ms,mp,qpr,qpi)

c*********************************************************************72
c
cc dcerrv ???
c
c*****this routine computes an upper bound on the error in evaluating
c*****p(s).  it computes a bound on the roundoff error in using dcpolv.
c
      double precision            ms         ,mp
      common /dcomn1/ infiny     ,constl     ,are        ,mre        ,
     1                re         ,lnbase     ,smalno     ,neta       ,
     2                zeta       ,eta
      double precision            infiny     ,constl     ,are        ,
     1                mre        ,re         ,lnbase     ,smalno     ,
     2                neta       ,zeta       ,eta
      common /dcomn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      dimension       qpr(*)     ,qpi(*)
      double precision            hr         ,hi         ,qpr        ,
     1                qpi        ,qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      double precision            w1         ,w2         ,erbnd
      double precision     dcmodq
      w1 = mre
      w2 = w1+are
      erbnd = dcmodq(qpr(1),qpi(1))*w1/w2
      do 101 i=2,nn
         erbnd = erbnd*ms+dcmodq(qpr(i),qpi(i))
  101 continue
      dcerrv = erbnd*w2-mp*w1
      return
      end
      subroutine dcnosh (nn,pr,pi,hr,hi)

c*********************************************************************72
c
cc dcnosh ???
c
c*****this routine computes 5 complex h polynomials with no shift
c*****this routine returns hr(i),hi(i),tr,ti,t1,t2
c
      double precision            pr         ,pi
      common /dcomn1/ infiny     ,constl     ,are        ,mre        ,
     1                re         ,lnbase     ,smalno     ,neta       ,
     2                zeta       ,eta
      double precision            infiny     ,constl     ,are        ,
     1                mre        ,re         ,lnbase     ,smalno     ,
     2                neta       ,zeta       ,eta
      common /dcomn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      double precision            hr         ,hi         ,qpr        ,
     1                qpi        ,qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      double precision            xni
      double precision            dcmodq
      dimension       pr(*)      ,pi(*)
      dimension       hr(*)      ,hi(*)
      n = nn-1
      nm1 = n-1
      do 101 i=1,n
         xni = nn-i
         hr(i) = xni*pr(i)/float(n)
         hi(i) = xni*pi(i)/float(n)
  101 continue
      do 105 jj=1,5
         if (dcmodq(hr(n),hi(n)) .le. eta*10.d0*dcmodq(pr(n),pi(n)))
     1       go to 103
         call dcdivq (-pr(nn),-pi(nn),hr(n),hi(n),tr,ti)
         do 102 i=1,nm1
            j = nn-i
            t1 = hr(j-1)
            t2 = hi(j-1)
            hr(j) = tr*t1-ti*t2+pr(j)
            hi(j) = tr*t2+ti*t1+pi(j)
  102    continue
         hr(1) = pr(1)
         hi(1) = pi(1)
         go to 105
  103    do 104 i=1,nm1
            j = nn-i
            hr(j) = hr(j-1)
            hi(j) = hi(j-1)
  104    continue
         hr(1) = 0.d0
         hi(1) = 0.d0
  105 continue
      return
      end
      subroutine dcnexp (nn,hr,hi,qpr,qpi,qhr,qhi)

c*********************************************************************72
c
cc dcnexp ???
c
c*****this routine computes one step of the polynomial recurrence
c*****this routine returns hr(i),hi(i),t1,t2
c
      common /dcomn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      double precision            hr         ,hi         ,qpr        ,
     1                qpi        ,qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      common /dcomn4/  bool
      dimension       hr(*)      ,hi(*)      ,qpr(*)     ,qpi(*)     ,
     1                qhr(*)     ,qhi(*)
      logical         bool
c
      n = nn-1
      nm1 = n-1
      if (bool) go to 102
      do 101 i=1,nm1
         j = nn-i
         t1 = qhr(j-1)
         t2 = qhi(j-1)
         hr(j) = tr*t1-ti*t2+qpr(j)
         hi(j) = tr*t2+ti*t1+qpi(j)
  101 continue
      hr(1) = qpr(1)
      hi(1) = qpi(1)
      return
  102 hr(1) = 0.d0
      hi(1) = 0.d0
      do 103 i=1,nm1
         j = nn-i
         hr(j) = qhr(j-1)
         hi(j) = qhi(j-1)
  103 continue
      return
      end
      subroutine dcalct(nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)

c*********************************************************************72
c
cc dcalct ???
c
c*****this routine calculates -p(s)/h(s)
c*****this routine returns bool,tr,ti,qhr,qhi
c
      double precision            pr         ,pi         ,pvr        ,
     1                pvi
      common /dcomn1/ infiny     ,constl     ,are        ,mre        ,
     1                re         ,lnbase     ,smalno     ,neta       ,
     2                zeta       ,eta
      double precision            infiny     ,constl     ,are        ,
     1                mre        ,re         ,lnbase     ,smalno     ,
     2                neta       ,zeta       ,eta
      common /dcomn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      double precision            hr         ,hi         ,
     1                qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      dimension       hr(*)      ,hi(*)      ,
     1                qhr(*)     ,qhi(*)
      common /dcomn4/  bool
      double precision            hvr        ,hvi
      double precision            dcmodq
      logical         bool
      dimension       pr(*)      ,pi(*)
c
      n = nn-1
      call dcpolv (n,hr,hi,qhr,qhi,hvr,hvi)
      bool = (dcmodq(hvr,hvi).le.are*10.d0*dcmodq(hr(n),hi(n)))
      if (bool) go to 101
      call dcdivq (-pvr,-pvi,hvr,hvi,tr,ti)
      return
  101 tr = 0.0d0
      ti = 0.0d0
      return
      end
      subroutine dcpolv (nn,pr,pi,qr,qi,pvr,pvi)

c*********************************************************************72
c
cc dcpolv ???
c
c*****this routine evaluates the polynomial given by (pr,pi) of degree
c*****nn-1 at (sr,si).  the value is (pvr,pvi) and the quotient poly-
c*****nomial is (qr,qi)
c
      double precision            pr         ,pi         ,qr         ,
     1                qi         ,pvr        ,pvi
      common /dcomn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      double precision            hr         ,hi         ,
     *                qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      dimension       pr(*)      ,pi(*)      ,qr(*)      ,qi(*)
      n = nn-1
      if (n .lt. 0) go to 102
      qr(1) = pr(1)
      qi(1) = pi(1)
      do 101 i=2,nn
         t1 = qr(i-1)
         t2 = qi(i-1)
         qr(i) = sr*t1-si*t2+pr(i)
         qi(i) = sr*t2+si*t1+pi(i)
  101 continue
      pvr = qr(nn)
      pvi = qi(nn)
      return
  102 pvr = 0.0d0
      pvi = 0.0d0
      return
      end
      subroutine dcvrsh (nn,degree,pr,pi,zeror,zeroi,nsr,nsi,hr,hi,qpr,
     1                   qpi,qhr,qhi,svhr,svhi)

c*********************************************************************72
c
cc dcvrsh ???
c
c*****this routine carries out the stage three iteration
c
      double precision            pr         ,pi         ,zeror      ,
     1                zeroi      ,nsr        ,nsi
      common /dcomn1/ infiny     ,constl     ,are        ,mre        ,
     1                re         ,lnbase     ,smalno     ,neta       ,
     2                zeta       ,eta
      double precision            infiny     ,constl     ,are        ,
     1                mre        ,re         ,lnbase     ,smalno     ,
     2                neta       ,zeta       ,eta
      common /dcomn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      double precision     hr(*)     ,hi(*)     ,qpr(*)     ,
     -               qpi(*)     ,qhr(*)     ,qhi(*)     ,sr        ,
     2                si         ,tr         ,ti         ,t1         ,t2
      common /dcomn3/ l2         ,iflag
      common /dcomn4/bool
      double precision            pvr        ,pvi        ,mp         ,
     1                smod       ,r1         ,r2         ,omp        ,
     2                temp       ,relstp
      double precision            dcmodq     ,dcerrv      ,dsqrt
      integer         degree
      logical     b     ,bool
      dimension       pr(*)      ,pi(*)      ,zeror(*)   ,zeroi(*)
      dimension svhr(*),svhi(*)
c
      iflag = 0
      b = .false.
      sr = nsr
      si = nsi
      do 105 i=1,10
         call dcpolv (nn,pr,pi,qpr,qpi,pvr,pvi)
         mp = dcmodq(pvr,pvi)
         smod = dcmodq(sr,si)
         if (mp .le. 20.d0*dcerrv(nn,smod,mp,qpr,qpi)) go to 106
         if (i .le. 1) go to 103
         if (b .or. mp.lt.omp .or. relstp.ge..05d0) go to 102
c
c*****routine to handle clusters by 5 fixed shift steps into the cluster
c
         temp = relstp
         b = .true.
         if (relstp .lt. eta) temp = eta
         r1 = dsqrt(temp)
         r2 = sr*(1.d0+r1)-si*r1
         si = sr*r1+si*(1.d0+r1)
         sr = r2
         call dcpolv (nn,pr,pi,qpr,qpi,pvr,pvi)
         do 101 j=1,5
            call dcalct (nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)
            call dcnexp (nn,hr,hi,qpr,qpi,qhr,qhi)
  101    continue
         omp = infiny
         go to 104
  102    if (mp*.1d0 .gt. omp) return
  103    omp = mp
  104    call dcalct (nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)
         call dcnexp (nn,hr,hi,qpr,qpi,qhr,qhi)
         call dcalct (nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)
         relstp = dcmodq(tr,ti)/dcmodq(sr,si)
         sr = sr+tr
         si = si+ti
  105 continue
      return
  106 call dcdefl(nn,degree,pr,pi,zeror,zeroi,qpr,qpi)
      iflag = 1
      return
      end
      subroutine dcfxsh (nn,degree,pr,pi,zeror,zeroi,hr,hi,qpr,qpi,qhr,
     1                   qhi,svhr,svhi)

c*********************************************************************72
c
cc dcfxsh ???
c
c*****this routine carries out stage two calculation of h polynomials
c*****with testing.  l2 is the maximum number of steps.
c*****this routine returns iflag,zeror(i),zeroi(i)
c
      double precision            pr         ,pi         ,zeror      ,
     1                zeroi
      common /dcomn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      double precision            hr         ,hi         ,qpr        ,
     1                qpi        ,qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      common /dcomn3/ l2         ,iflag
      common /dcomn4/bool
      double precision            pvr        ,pvi        ,mp         ,
     1                smod       ,otr        ,oti        ,nsr        ,
     2                nsi        ,svhr       ,svhi       ,svsr       ,
     3                svsi
      double precision            dcmodq     ,dcerrv
      integer         degree
      logical     test,     pasd,     bool
      dimension       pr(*)      ,pi(*)      ,zeror(*)   ,zeroi(*)   ,
     -     svhr(*)     ,svhi(*)     ,qpr(*)     ,qpi(*)
      dimension     hr(*)     ,hi(*)     ,qhr(*)     ,qhi(*)
c
      n = nn-1
      call dcpolv(nn,pr,pi,qpr,qpi,pvr,pvi)
      mp = dcmodq(pvr,pvi)
      smod = dcmodq(sr,si)
      if (mp .le. 20.d0*dcerrv(nn,smod,mp,qpr,qpi)) go to 106
      test = .true.
      call dcalct(nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)
      pasd = .false.
      do 105 j=1,l2
         otr = tr
         oti = ti
         call dcnexp(nn,hr,hi,qpr,qpi,qhr,qhi)
         call dcalct(nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)
         nsr = sr+tr
         nsi = si+ti
         if (.not.test .or. j.ge.l2) go to 105
         if (dcmodq(otr-tr,oti-ti) .ge. .5d0*dcmodq(nsr,nsi)) go to 104
         if (.not.pasd) go to 103
c
c  test has been passed twice.  attempt an iteration
c
         do 101 i=1,n
            svhr(i) = hr(i)
            svhi(i) = hi(i)
  101    continue
c
         svsr = sr
         svsi = si
         call dcvrsh (nn,degree,pr,pi,zeror,zeroi,nsr,nsi,hr,hi,qpr,
     1                qpi,qhr,qhi,svhr,svhi)
         if (iflag .eq. 1) return
         do 102 i=1,n
            hr(i) = svhr(i)
            hi(i) = svhi(i)
  102    continue
         sr = svsr
         si = svsi
         test = .false.
         call dcpolv (nn,pr,pi,qpr,qpi,pvr,pvi)
         call dcalct (nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)
         go to 105
  103    pasd = .true.
         go to 105
  104    pasd = .false.
  105 continue
c
      call dcvrsh (nn,degree,pr,pi,zeror,zeroi,nsr,nsi,hr,hi,qpr,qpi,
     1             qhr,qhi,svhr,svhi)
      return
c
  106 call dcdefl(nn,degree,pr,pi,zeror,zeroi,qpr,qpi)
      iflag = 1
      return
      end
      subroutine dcmcon(base,precis,exphi,explo,round)

c*********************************************************************72
c
cc dcmcon ???
c
      double precision            base       ,precis     ,exphi      ,
     1                explo
      logical         round
c
      base = i1mach(7)
      precis = i1mach(14)
      exphi = i1mach(16)
      explo = i1mach(15)
      round = .false.
      return
      end
      double precision function dcmodq(ad,bd)
      double precision            ad         ,bd
      double precision            a          ,b
      double precision     dabs,     dsqrt
c
      a = dabs(ad)
      b = dabs(bd)
      if (a .ge. b) go to 101
      dcmodq = b*dsqrt(1.0d0+(a/b)**2)
      return
  101 if (a .le. b) go to 102
      dcmodq = a*dsqrt(1.0d0+(b/a)**2)
      return
  102 dcmodq = a*dsqrt(2.0d0)
      return
      end
      subroutine dcdivq (ard,aid,brd,bid,cr,ci)

c*********************************************************************72
c
cc dcdivq ???
c
      double precision            ard        ,aid        ,brd        ,
     1                bid        ,cr         ,ci
      double precision            base       ,precis     ,exphi      ,
     1                explo      ,infiny     ,ar         ,ai         ,
     2                br         ,bi         ,r          ,d
      double precision            dabs
      logical         round
c
      ar = ard
      ai = aid
      br = brd
      bi = bid
      if (br.ne.0.d0 .or. bi.ne.0.d0) go to 101
      call dcmcon (base,precis,exphi,explo,round)
      infiny = (1.0d0-base**(1.0d0-precis))*base**(exphi-1.0d0)*base
      cr = infiny
      ci = infiny
      go to 104
  101 if (dabs(br) .ge. dabs(bi)) go to 102
      r = br/bi
      d = bi+r*br
      cr = (ar*r+ai)/d
      ci = (ai*r-ar)/d
      go to 103
  102 continue
      r = bi/br
      d = br+r*bi
      cr = (ar+ai*r)/d
      ci = (ai-ar*r)/d
  103 continue
  104 continue
      return
      end
      double precision function dcrand(i)

c*********************************************************************72
c
cc dcrand ???
c
      common /dcomn5/ r
      integer         r          ,a
c
      r = r*2045+1497794
      a = r/1048576
      r = r-a*1048576
      dcrand = 1.d0*float(r)/1048576.d0
      return
      end
      subroutine dcscle(nn,pr,pi)
c
      double precision            pr         ,pi
      common /dcomn1/ infiny     ,constl     ,are        ,mre        ,
     1                re         ,lnbase     ,smalno     ,neta       ,
     2                zeta       ,eta
      double precision            infiny     ,constl     ,are        ,
     1                mre        ,re         ,lnbase     ,smalno     ,
     2                neta       ,zeta       ,eta
      double precision            base       ,precis     ,exphi      ,
     1                explo      ,smax        ,smin        ,xmax     ,
     2                s          ,scale
      double precision            dcmodq     ,dsqrt      ,dlog
      logical         round
      dimension       pr(*)      ,pi(*)
c
      call dcmcon (base,precis,exphi,explo,round)
      xmax = base**(exphi/2.0d0)
      smax = 0.0d0
      smin = infiny
      do 101 i=1,nn
         s = dcmodq(pr(i),pi(i))
         if (s .gt. smax) smax = s
         if (s.ne.0.0d0 .and. s.lt.smin) smin = s
  101 continue
      if (smin.ge.constl .and. smax.le.xmax) return
      s = constl/smin
      if (s .gt. 1.0d0) go to 102
      scale = 1.0d0/(dsqrt(smax)*dsqrt(smin))
      go to 103
  102 scale = s
      if (infiny/s .lt. smax) scale = 1.0d0
  103 l = dlog(scale)/lnbase+.5d0
      if (l .lt. 0) l = l-1
      if (l .eq. 0) return
      s = base**l
      do 104 i=1,nn
         pr(i) = pr(i)*s
         pi(i) = pi(i)*s
  104 continue
      return
      end
      subroutine cpoly(coefr,coefi,ndeg,work,zeror,zeroi,fail)

c*********************************************************************72
c
cc dcpoly ???
c
c  cpoly finds the roots or "zeroes" of a complex polynomial.  
c
c  cpoly uses single precision arithmetic for its calculations.  the roots of 
c  the polynomial are returned as pairs of single precision values, 
c  containing the real and imaginary parts of the roots.
c
c  coefr,
c  coefi  input, real coefr(ndeg+1),coefi(ndeg+1).
c         coefr and coefi contain the real and imaginary parts of the 
c         coefficients of the polynomial.  the coefficient of the
c         highest degree term is coefr(1) + i * coefi(1).
c
c  ndeg   input, integer ndeg.
c         ndeg is the degree of the polynomial.  this is the value of the
c         highest exponent in the polynomial.  for example, the polynomial
c         3 * x**2 + 7 * x + 88 has degree 2.
c
c  work   workspace, real work(14*(ndeg+1)).
c
c  zeror,
c  zeroi  output, real zeror(ndeg), zeroi(ndeg).
c         zeror and zeroi contain the real and imaginary parts of the roots
c         of the polynomial.  
c
c  fail   output, logical fail.
c
c         if fail is .true., then either the leading polynomial coefficient
c         was zero on input, or the iteration was not able to find all the
c         zeroes.
c
c         if fail is .false., then no failure occurred, and all the zeroes
c         have been computed.
c
      real coefr(*),coefi(*),zeror(*),zeroi(*)
      logical fail
      real work(*)
c
      n = ndeg+1
      call c1pol(coefr,coefi,ndeg,zeror,zeroi,fail,work(1),work(n+1),
     1            work(2*n+1),work(3*n+1),work(4*n+1),work(5*n+1),
     2            work(6*n+1),work(7*n+1),work(8*n+1),work(9*n+1),
     3            work(10*n+1),work(11*n+1),work(12*n+1),work(13*n+1))
      return
      end
      subroutine c1pol(coefr,coefi,degree,zeror,zeroi,fail,hr,hi,qpr,
     *qpi,qhr,qhi,sshr,sshi,pr,pi,q,pt,svhr,svhi)

c*********************************************************************72
c
cc c1pol ???
c
c
c*****this routine finds the zeros of any complex polynomial
c
      real            coefr        ,coefi,zeror      ,
     1                zeroi
      common /comn1/ infiny     ,constl     ,are        ,mre        ,
     1                re         ,lnbase     ,smalno     ,neta       ,
     2                zeta       ,eta
      real            infiny     ,constl     ,are        ,
     1                mre        ,re         ,lnbase     ,smalno     ,
     2                neta       ,zeta       ,eta
      common /comn5/ r
      common /comn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      real            hr         ,hi         ,qpr        ,
     1                qpi        ,qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,
     3                t2         ,q          ,pt         ,svhr       ,
     4                svhi
      common /comn3/ l2         ,iflag
      real            base       ,
     1                precis     ,exphi      ,explo      ,pr         ,
     2                pi         ,sshr       ,sshi       ,bnd        ,
     3                angle      ,tpi
      real            alog       ,sqrt      ,cauch     ,
     1                cos       ,sin       ,crand
      integer         degree     ,r          ,cnt1       ,cnt2
      logical         fail       ,round
      dimension       q(*)       ,pt(*)      ,svhr(*)    ,svhi(*)
      dimension       coefr(*) ,coefi(*)     ,zeror(*)   ,zeroi(*)   ,
     1                sshr(*)   ,sshi(*)   ,pr(*)     ,pi(*)     ,
     2                hr(*)      ,hi(*)      ,qpr(*)     ,qpi(*)     ,
     3                qhr(*)     ,qhi(*)
c
      tpi=6.2831853071795864769252867664
      fail = .false.
      call cmcon(base,precis,exphi,explo,round)
      lnbase = alog(base)
      infiny = (1.0-base**(1.0-precis))*base**(exphi-1.0)*base
      smalno = base**(explo+10.0)/base**10
      zeta = base**precis+1.0
      constl = zeta*smalno*base
      eta = base**(1.0-precis)
      if (round) eta = .5*eta
      are = eta
      mre = 2.0*sqrt(2.0)*eta
      re = 1.0+are
      r = 0
c
      if(coefr(1).eq.0.0.and.coefi(1).eq.0.0)then
        write(*,*)' '
        write(*,*)'cpoly can not solve this problem!'
        write(*,*)'the leading polynomial coefficient is zero.'
        write(*,*)'check coefr(1) and coefi(1)!'
        fail = .true.
        return
        endif
c
c  make a copy of the coefficients
c
      nn = degree+1
      do 102 i=1,nn
         pr(i) = coefr(i)
         pi(i) = coefi(i)
  102 continue
c
103   continue
      iflag = 0
      call cscle (nn,pr,pi)
      n = nn-1
  104 if (nn .gt. 2) go to 105
      call cdivq (-pr(2),-pi(2),pr(1),pi(1),zeror(degree),
     1             zeroi(degree))
      return
  105 if (pr(nn).ne.0.0 .or. pi(nn).ne.0.0) go to 106
      idnn2 = degree-nn+2
      zeror(idnn2) = 0.0
      zeroi(idnn2) = 0.0
      nn = nn-1
      go to 104
  106 call cnosh(nn,pr,pi,hr,hi)
      do 107 i=1,n
         sshr(i) = hr(i)
         sshi(i) = hi(i)
  107 continue
      bnd = cauch(nn,pr,pi,q,pt)
      do 110 cnt1=1,2
         l2 = 10
         do 108 cnt2=1,8
            angle = tpi*crand(-1)
            sr = bnd*cos(angle)
            si = bnd*sin(angle)
            call cfxsh (nn,degree,pr,pi,zeror,zeroi,hr,hi,qpr,qpi,qhr,
     1                   qhi,svhr,svhi)
            if (iflag .eq. 1) go to 103
            l2 = l2+10
  108    continue
         do 109 i=1,n
            hr(i) = sshr(i)
            hi(i) = sshi(i)
  109    continue
  110 continue

      write(*,*)' '
      write(*,*)'cpoly was unable to solve this problem.'
      write(*,*)'the iteration did not converge, some of the roots'
      write(*,*)'could not be computed.'
      fail = .true.

      return
      end
      real function cauch(nn,pr,pi,q,pt)

c*********************************************************************72
c
cc cauch ???
c
c  this routine computes a lower bound on the modulus of the zeros
c
      real            pr         ,pi
      real            pt         ,x          ,xm         ,
     1                f          ,dx         ,q          ,df
      real     cmodq   ,exp     ,
     1                alog       ,abs
      dimension       pr(*)      ,pi(*)      ,q(*)      ,pt(*)
c
      n = nn-1
      do 101 i=1,nn
         pt(i) = cmodq(pr(i),pi(i))
  101 continue
      pt(nn) = -pt(nn)
      x = exp((alog(-pt(nn))-alog(pt(1)))/float(n))
      if (pt(n) .eq. 0.0) go to 102
c
c  if newton step at the origin is better, use it as the initial gues
c
      xm = -pt(nn)/pt(n)
      if (xm .lt. x) x = xm
c
c  chop the interval (0,x) until f)=0
c
  102 xm = x*.1
      f = pt(1)
      do 103 i=2,nn
         f = f*xm+pt(i)
  103 continue
      if (f .le. 0.0) go to 104
      x = xm
      go to 102
  104 dx = x
  105 if (abs(dx/x) .le. .005) go to 108
      q(1) = pt(1)
      do 106 i=2,nn
         q(i) = q(i-1)*x+pt(i)
  106 continue
      f = q(nn)
      df = q(1)
      do 107 i=2,n
         df = df*x+q(i)
  107 continue
      dx = f/df
      x = x-dx
      go to 105
  108 cauch = x
      return
      end
      subroutine cdefl(nn,degree,pr,pi,zeror,zeroi,qpr,qpi)

c*********************************************************************72
c
cc cdefl ???
c
c
c  this routine replaces (pr,pi) by the deflated polynomial, reduces
c  nn, and stores the approximate zero.
c  this routine returns zeror(i),zeroi(i),pr(i),pi(i),nn
c
      real            pr         ,pi         ,zeror      ,
     1                zeroi
      common /comn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      real            qpr        ,
     1                qpi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      integer         degree
      dimension   pr(*)      ,pi(*)      ,zeror(*)   ,zeroi(*)
      dimension       qpr(*)     ,qpi(*)
c
      nd = degree-nn+2
      zeror(nd) = sr
      zeroi(nd) = si
      nn = nn-1
      do 101 i=1,nn
         pr(i) = qpr(i)
         pi(i) = qpi(i)
  101 continue
      return
      end
      real function cerrv (nn,ms,mp,qpr,qpi)

c*********************************************************************72
c
cc cerrv ???
c
c*****this routine computes an upper bound on the error in evaluating
c*****p(s).  it computes a bound on the roundoff error in using cpolv.
c
      real            ms         ,mp
      common /comn1/ infiny     ,constl     ,are        ,mre        ,
     1                re         ,lnbase     ,smalno     ,neta       ,
     2                zeta       ,eta
      real            infiny     ,constl     ,are        ,
     1                mre        ,re         ,lnbase     ,smalno     ,
     2                neta       ,zeta       ,eta
      common /comn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      dimension       qpr(*)     ,qpi(*)
      real            hr         ,hi         ,qpr        ,
     1                qpi        ,qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      real            w1         ,w2         ,erbnd
      real     cmodq
c
      w1 = mre
      w2 = w1+are
      erbnd = cmodq(qpr(1),qpi(1))*w1/w2
      do 101 i=2,nn
         erbnd = erbnd*ms+cmodq(qpr(i),qpi(i))
  101 continue
      cerrv = erbnd*w2-mp*w1
      return
      end
      subroutine cnosh (nn,pr,pi,hr,hi)

c*********************************************************************72
c
cc cnosh ???
c
c*****this routine computes 5 complex h polynomials with no shift
c*****this routine returns hr(i),hi(i),tr,ti,t1,t2
c
      real            pr         ,pi
      common /comn1/ infiny     ,constl     ,are        ,mre        ,
     1                re         ,lnbase     ,smalno     ,neta       ,
     2                zeta       ,eta
      real            infiny     ,constl     ,are        ,
     1                mre        ,re         ,lnbase     ,smalno     ,
     2                neta       ,zeta       ,eta
      common /comn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      real            hr         ,hi         ,qpr        ,
     1                qpi        ,qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      real            xni
      real            cmodq
      dimension   pr(*)      ,pi(*)
      dimension       hr(*)      ,hi(*)
c
      n = nn-1
      nm1 = n-1
      do 101 i=1,n
         xni = nn-i
         hr(i) = xni*pr(i)/float(n)
         hi(i) = xni*pi(i)/float(n)
  101 continue
      do 105 jj=1,5
         if (cmodq(hr(n),hi(n)) .le. eta*10.0*cmodq(pr(n),pi(n)))
     1       go to 103
         call cdivq (-pr(nn),-pi(nn),hr(n),hi(n),tr,ti)
         do 102 i=1,nm1
            j = nn-i
            t1 = hr(j-1)
            t2 = hi(j-1)
            hr(j) = tr*t1-ti*t2+pr(j)
            hi(j) = tr*t2+ti*t1+pi(j)
  102    continue
         hr(1) = pr(1)
         hi(1) = pi(1)
         go to 105
  103    do 104 i=1,nm1
            j = nn-i
            hr(j) = hr(j-1)
            hi(j) = hi(j-1)
  104    continue
         hr(1) = 0.0
         hi(1) = 0.0
  105 continue
      return
      end
      subroutine cnexp (nn,hr,hi,qpr,qpi,qhr,qhi)

c*********************************************************************72
c
cc cnexp ???
c
c*****this routine computes one step of the polynomial recurrence
c*****this routine returns hr(i),hi(i),t1,t2
c
      common /comn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      real            hr         ,hi         ,qpr        ,
     1                qpi        ,qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      common /comn4/  bool
      dimension       hr(*)      ,hi(*)      ,qpr(*)     ,qpi(*)     ,
     1                qhr(*)     ,qhi(*)
      logical         bool
c
      n = nn-1
      nm1 = n-1
      if (bool) go to 102
      do 101 i=1,nm1
         j = nn-i
         t1 = qhr(j-1)
         t2 = qhi(j-1)
         hr(j) = tr*t1-ti*t2+qpr(j)
         hi(j) = tr*t2+ti*t1+qpi(j)
  101 continue
      hr(1) = qpr(1)
      hi(1) = qpi(1)
      return
  102 hr(1) = 0.0
      hi(1) = 0.0
      do 103 i=1,nm1
         j = nn-i
         hr(j) = qhr(j-1)
         hi(j) = qhi(j-1)
  103 continue
      return
      end
      subroutine calct (nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)

c*********************************************************************72
c
cc calct ???
c
c*****this routine calculates -p(s)/h(s)
c*****this routine returns bool,tr,ti,qhr,qhi
c
      real            pr         ,pi         ,pvr        ,
     1                pvi
      common /comn1/ infiny     ,constl     ,are        ,mre        ,
     1                re         ,lnbase     ,smalno     ,neta       ,
     2                zeta       ,eta
      real            infiny     ,constl     ,are        ,
     1                mre        ,re         ,lnbase     ,smalno     ,
     2                neta       ,zeta       ,eta
      common /comn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      real            hr         ,hi         ,
     1                qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      dimension       hr(*)      ,hi(*)      ,
     1                qhr(*)     ,qhi(*)
      common /comn4/  bool
      real            hvr        ,hvi
      real            cmodq
      logical         bool
      dimension   pr(*)      ,pi(*)
c
      n = nn-1
      call cpolv (n,hr,hi,qhr,qhi,hvr,hvi)
      bool = (cmodq(hvr,hvi).le.are*10.0*cmodq(hr(n),hi(n)))
      if (bool) go to 101
      call cdivq (-pvr,-pvi,hvr,hvi,tr,ti)
      return
  101 tr = 0.0
      ti = 0.0
      return
      end
      subroutine cpolv (nn,pr,pi,qr,qi,pvr,pvi)

c*********************************************************************72
c
cc cpolv ???
c
c*****this routine evaluates the polynomial given by (pr,pi) of degree
c*****nn-1 at (sr,si).  the value is (pvr,pvi) and the quotient poly-
c*****nomial is (qpr,qpi)
c*****this routine returns qpr(i),qpi(i),pvr,pvi
c
      real            pr         ,pi         ,qr         ,
     1                qi         ,pvr        ,pvi
      common /comn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      real            hr         ,hi         ,qpr        ,
     1                qpi        ,qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      dimension   pr(*)      ,pi(*)      ,qr(*)      ,qi(*)
      n = nn-1
      if (n .lt. 0) go to 102
      qr(1) = pr(1)
      qi(1) = pi(1)
      do 101 i=2,nn
         t1 = qr(i-1)
         t2 = qi(i-1)
         qr(i) = sr*t1-si*t2+pr(i)
         qi(i) = sr*t2+si*t1+pi(i)
  101 continue
      pvr = qr(nn)
      pvi = qi(nn)
      return
  102 pvr = 0.0
      pvi = 0.0
      return
      end
      subroutine cvrsh(nn,degree,pr,pi,zeror,zeroi,nsr,nsi,hr,hi,qpr,
     1                   qpi,qhr,qhi,svhr,svhi)

c*********************************************************************72
c
cc cvrsh ???
c
c*****this routine carries out the stage three iteration
c
      real            pr         ,pi         ,zeror      ,
     1                zeroi      ,nsr        ,nsi
      common /comn1/ infiny     ,constl     ,are        ,mre        ,
     1                re         ,lnbase     ,smalno     ,neta       ,
     2                zeta       ,eta
      real            infiny     ,constl     ,are        ,
     1                mre        ,re         ,lnbase     ,smalno     ,
     2                neta       ,zeta       ,eta
      common /comn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      real     hr(*)     ,hi(*)     ,qpr(*)     ,
     -               qpi(*)     ,qhr(*)     ,qhi(*)     ,sr        ,
     2                si         ,tr         ,ti         ,t1         ,t2
      common /comn3/ l2         ,iflag
      common /comn4/bool
      real            pvr        ,pvi        ,mp         ,
     1                smod       ,r1         ,r2         ,omp        ,
     2                temp       ,relstp
      real            cmodq     ,cerrv      ,sqrt
      integer         degree
      logical     b     ,bool
      dimension   pr(*)      ,pi(*)      ,zeror(*)   ,zeroi(*)
      dimension svhr(*),svhi(*)
c
      iflag = 0
      b = .false.
      sr = nsr
      si = nsi
      do 105 i=1,10
         call cpolv (nn,pr,pi,qpr,qpi,pvr,pvi)
         mp = cmodq(pvr,pvi)
         smod = cmodq(sr,si)
         if (mp .le. 20.0*cerrv(nn,smod,mp,qpr,qpi)) go to 106
         if (i .le. 1) go to 103
         if (b .or. mp.lt.omp .or. relstp.ge..05) go to 102
c
c*****routine to handle clusters by 5 fixed shift steps into the cluster
c
         temp = relstp
         b = .true.
         if (relstp .lt. eta) temp = eta
         r1 = sqrt(temp)
         r2 = sr*(1.0+r1)-si*r1
         si = sr*r1+si*(1.0+r1)
         sr = r2
         call cpolv (nn,pr,pi,qpr,qpi,pvr,pvi)
         do 101 j=1,5
            call calct (nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)
            call cnexp (nn,hr,hi,qpr,qpi,qhr,qhi)
  101    continue
         omp = infiny
         go to 104
  102    if (mp*.1 .gt. omp) return
  103    omp = mp
  104    call calct (nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)
         call cnexp (nn,hr,hi,qpr,qpi,qhr,qhi)
         call calct (nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)
         relstp = cmodq(tr,ti)/cmodq(sr,si)
         sr = sr+tr
         si = si+ti
  105 continue
      return
  106 call cdefl(nn,degree,pr,pi,zeror,zeroi,qpr,qpi)
      iflag = 1
      return
      end
      subroutine cfxsh (nn,degree,pr,pi,zeror,zeroi,hr,hi,qpr,qpi,qhr,
     1                   qhi,svhr,svhi)

c*********************************************************************72
c
cc cfxsh ???
c
c*****this routine carries out stage two calculation of h polynomials
c*****with testing.  l2 is the maximum number of steps.
c*****this routine returns iflag,zeror(i),zeroi(i)
c
      real            pr         ,pi         ,zeror      ,
     1                zeroi
      common /comn2/ sr         ,si         ,tr         ,ti         ,
     1                t1         ,t2
      real            hr         ,hi         ,qpr        ,
     1                qpi        ,qhr        ,qhi        ,sr         ,
     2                si         ,tr         ,ti         ,t1         ,t2
      common /comn3/ l2         ,iflag
      common /comn4/bool
      real            pvr        ,pvi        ,mp         ,
     1                smod       ,otr        ,oti        ,nsr        ,
     2                nsi        ,svhr       ,svhi       ,svsr       ,
     3                svsi
      real            cmodq     ,cerrv
      integer         degree
      logical     test,     pasd,     bool
      dimension   pr(*)      ,pi(*)      ,zeror(*)   ,zeroi(*)   ,
     -     svhr(*)     ,svhi(*)     ,qpr(*)     ,qpi(*)
      dimension     hr(*)     ,hi(*)     ,qhr(*)     ,qhi(*)
c
      n = nn-1
      call cpolv (nn,pr,pi,qpr,qpi,pvr,pvi)
      mp = cmodq(pvr,pvi)
      smod = cmodq(sr,si)
      if (mp .le. 20.0*cerrv(nn,smod,mp,qpr,qpi)) go to 106
      test = .true.
      call calct (nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)
      pasd = .false.
      do 105 j=1,l2
         otr = tr
         oti = ti
         call cnexp (nn,hr,hi,qpr,qpi,qhr,qhi)
         call calct (nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)
         nsr = sr+tr
         nsi = si+ti
         if (.not.test .or. j.ge.l2) go to 105
         if (cmodq(otr-tr,oti-ti) .ge. .5*cmodq(nsr,nsi)) go to 104
         if (.not.pasd) go to 103
c
c*****test has been passed twice.  attempt an iteration
c
         do 101 i=1,n
            svhr(i) = hr(i)
            svhi(i) = hi(i)
  101    continue
         svsr = sr
         svsi = si
         call cvrsh (nn,degree,pr,pi,zeror,zeroi,nsr,nsi,hr,hi,qpr,
     1                qpi,qhr,qhi,svhr,svhi)
         if (iflag .eq. 1) return
         do 102 i=1,n
            hr(i) = svhr(i)
            hi(i) = svhi(i)
  102    continue
         sr = svsr
         si = svsi
         test = .false.
         call cpolv (nn,pr,pi,qpr,qpi,pvr,pvi)
         call calct (nn,pr,pi,pvr,pvi,hr,hi,qhr,qhi)
         go to 105
  103    pasd = .true.
         go to 105
  104    pasd = .false.
  105 continue
      call cvrsh (nn,degree,pr,pi,zeror,zeroi,nsr,nsi,hr,hi,qpr,qpi,
     1             qhr,qhi,svhr,svhi)
      return
  106 call cdefl (nn,degree,pr,pi,zeror,zeroi,qpr,qpi)
      iflag = 1
      return
      end
      subroutine cmcon (base,precis,exphi,explo,round)

c*********************************************************************72
c
cc cmcon ???
c
      real            base       ,precis     ,exphi      ,
     1                explo
      logical         round
c
      base = i1mach(7)
      precis = i1mach(11)
      exphi = i1mach(13)
      explo = i1mach(12)
      round = .false.
      return
      end
      real function cmodq (ad,bd)

c*********************************************************************72
c
cc cmodq ???
c
      real            ad         ,bd
      real            a          ,b
      real     abs,     sqrt
c
      a = abs(ad)
      b = abs(bd)
      if (a .ge. b) go to 101
      cmodq = b*sqrt(1.0+(a/b)**2)
      return
  101 if (a .le. b) go to 102
      cmodq = a*sqrt(1.0+(b/a)**2)
      return
  102 cmodq = a*sqrt(2.0)
      return
      end
      subroutine cdivq (ard,aid,brd,bid,cr,ci)

c*********************************************************************72
c
cc cdivq ???
c
      real            ard        ,aid        ,brd        ,
     1                bid        ,cr         ,ci
      real            base       ,precis     ,exphi      ,
     1                explo      ,infiny     ,ar         ,ai         ,
     2                br         ,bi         ,r          ,d
      real            abs
      logical         round
c
      ar = ard
      ai = aid
      br = brd
      bi = bid
      if (br.ne.0.0 .or. bi.ne.0.0) go to 101
      call cmcon (base,precis,exphi,explo,round)
      infiny = (1.0-base**(1.0-precis))*base**(exphi-1.0)*base
      cr = infiny
      ci = infiny
      go to 104
  101 if (abs(br) .ge. abs(bi)) go to 102
      r = br/bi
      d = bi+r*br
      cr = (ar*r+ai)/d
      ci = (ai*r-ar)/d
      go to 103
  102 continue
      r = bi/br
      d = br+r*bi
      cr = (ar+ai*r)/d
      ci = (ai-ar*r)/d
  103 continue
  104 continue
      return
      end
      real function crand(i)

c*********************************************************************72
c
cc crand ???
c
      common /comn5/ r
c
      integer         r          ,a
      r = r*2045+1497794
      a = r/1048576
      r = r-a*1048576
      crand = 1.0*float(r)/1048576.0
      return
      end
      subroutine cscle(nn,pr,pi)

c*********************************************************************72
c
cc cscle ???
c
      real            pr         ,pi
      common /comn1/ infiny     ,constl     ,are        ,mre        ,
     1                re         ,lnbase     ,smalno     ,neta       ,
     2                zeta       ,eta
      real            infiny     ,constl     ,are        ,
     1                mre        ,re         ,lnbase     ,smalno     ,
     2                neta       ,zeta       ,eta
      real            base       ,precis     ,exphi      ,
     1                explo      ,smax        ,smin        ,xmax       ,
     2                s          ,scale
      real            cmodq     ,sqrt      ,alog
      logical         round
      dimension   pr(*)      ,pi(*)
c
      call cmcon(base,precis,exphi,explo,round)
      xmax = base**(exphi/2.0)
      smax = 0.0
      smin = infiny
      do 101 i=1,nn
         s = cmodq(pr(i),pi(i))
         if (s .gt. smax) smax = s
         if (s.ne.0.0 .and. s.lt.smin) smin = s
  101 continue
      if (smin.ge.constl .and. smax.le.xmax) return
      s = constl/smin
      if (s .gt. 1.0) go to 102
      scale = 1.0/(sqrt(smax)*sqrt(smin))
      go to 103
  102 scale = s
      if (infiny/s .lt. smax) scale = 1.0
  103 l = alog(scale)/lnbase+.5
      if (l .lt. 0) l = l-1
      if (l .eq. 0) return
      s = base**l
      do 104 i=1,nn
         pr(i) = pr(i)*s
         pi(i) = pi(i)*s
  104 continue
      return
      end
