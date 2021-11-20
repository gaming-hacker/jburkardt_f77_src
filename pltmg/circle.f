c***********************  problem name: circle  ************************
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine a1xy(x,y,u,ux,uy,rl,itag,values)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
            double precision
     +          values(*)
             character*80
     +          su
            common /atest2/iu(100),ru(100),su(100)
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
c
        values(k0)=ux*ru(itag)
        values(kx)=ru(itag)
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine a2xy(x,y,u,ux,uy,rl,itag,values)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
            double precision
     +          values(*)
             character*80
     +          su
            common /atest2/iu(100),ru(100),su(100)
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
c
        values(k0)=uy*ru(itag)
        values(ky)=ru(itag)
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine fxy(x,y,u,ux,uy,rl,itag,values)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
            double precision
     +          values(*)
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
c
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gnxy(x,y,u,rl,itag,values)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
            double precision
     +          values(*)
            character*80
     +          su
            common /val1/k0,ku,kl,kuu,kul,klu,kll
            common /atest2/iu(100),ru(100),su(100)
c
        if(itag.gt.iu(1)) return
c
c       (x,y) is unit outward normal
c
        if(itag.lt.1) return
        call uexact(x,y,itag,r,rx,ry,rxx,ryy,rxy)
        if(itag.le.iu(1)) then
            values(k0)=(x*rx+y*ry)*ru(itag)
        else
            values(k0)=ry*ru(itag-1)
        endif
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gdxy(x,y,rl,itag,values)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
            double precision
     +          values(*)
            character*80
     +          su
            common /val2/k0,kl,kll,klb,kub,kic
            common /atest2/iu(100),ru(100),su(100)
c
        if(itag.lt.1.or.itag.gt.iu(1)) return
        call uexact(x,y,itag,r,rx,ry,rxx,ryy,rxy)
        values(k0)=r
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine p1xy(x,y,u,ux,uy,rl,itag,values)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
            double precision
     +          values(*)
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
c
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine p2xy(x,y,dx,dy,u,ux,uy,rl,itag,jtag,values)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
            double precision
     +          values(*)
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
c
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine qxy(x,y,u,ux,uy,rl,itag,values)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
            double precision
     +          values(*)
            character*80
     +          su
            common /val3/kf,kf1,kf2,ksk,kad
            common /atest2/iu(100),ru(100),su(100)
c
        call uexact(x,y,itag,r,rx,ry,rxx,ryy,rxy)
        values(kf)=r-u
        values(kf1)=rx-ux
        values(kf2)=ry-uy
        values(ksk)=r
        values(kad)=r
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine uexact(x,y,itag,u,ux,uy,uxx,uyy,uxy)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            character*80
     +          su
            common /atest2/iu(100),ru(100),su(100)
c
        u=0.0d0
        ux=0.0d0
        uy=0.0d0
        uxx=0.0d0
        uxy=0.0d0
        uyy=0.0d0
        r=dsqrt(x**2+y**2)
        if(r.le.0.0d0) return
c
        al=ru(25)
        arg=dmin1(x/r,1.0d0)
        arg=dmax1(-1.0d0,arg)
        theta=dacos(arg)
        if(itag.ge.5) theta=6.28318530717958d0-theta
c
        s=r**al*dsin(theta*al)
        sx=al*r**(al-1.0d0)*dsin(theta*(al-1.0d0))
        sy=al*r**(al-1.0d0)*dcos(theta*(al-1.0d0))
        sxx=al*(al-1.0d0)*r**(al-2.0d0)*dsin(theta*(al-2.0d0))
        syy=-sxx
        sxy=al*(al-1.0d0)*r**(al-2.0d0)*dcos(theta*(al-2.0d0))
c
        c=r**al*dcos(theta*al)
        cx=sy
        cy=-sx
        cxx=sxy
        cyy=-sxy
        cxy=syy
c
        cf=ru(itag+8)
        sf=ru(itag+16)
        u=cf*c+sf*s
        ux=cf*cx+sf*sx
        uy=cf*cy+sf*sy
        uxx=cf*cxx+sf*sxx
        uyy=cf*cyy+sf*syy
        uxy=cf*cxy+sf*sxy
c
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine cnu(ntri,af,cf,sf,vv)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
            double precision
     +          cf(8),sf(8),af(8),cd(8),sd(8),val(1000)
c
        eps=ceps(ibit)*8.0d0
        cf(1)=0.0d0
        sf(1)=1.0d0
        cd(1)=0.0d0
        sd(1)=0.0d0
        vmax=2.0d0
        n=1000
        do i=1,n
            vv=dfloat(i)*vmax/dfloat(n)
            call veval(ntri,cf,sf,af,vv,val(i),dp)
        enddo
        do i=2,n
           if(val(i-1).gt.0.0d0.and.val(i).le.0.0d0) then
                vmin=dfloat(i-1)*vmax/dfloat(n)
                vmax=dfloat(i)*vmax/dfloat(n)
                go to 10
            endif
        enddo
        vv=vmax
        return
c
   10   itmax=100
        vv=(vmin+vmax)/2.0d0
        do k=1,itmax
            call veval(ntri,cf,sf,af,vv,dd,dp)
            if(dd.gt.0.0d0) then
                vmin=vv
            else
                vmax=vv
            endif
            vv=vv-dd/dp
            if(vv.le.vmin.or.vv.ge.vmax)
     +          vv=(vmin+vmax)/2.0d0
            if(dabs(dd).le.eps) return
        enddo
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine veval(ntri,cf,sf,af,vv,dd,dp)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
            double precision
     +          cf(8),sf(8),af(8),cd(8),sd(8)
c
        pi=3.141592653589793d0
        cf(1)=0.0d0
        sf(1)=1.0d0
        cd(1)=0.0d0
        sd(1)=0.0d0
c
        do i=1,ntri-1
            if(af(i+1).eq.af(i)) then
                cf(i+1)=cf(i)
                sf(i+1)=sf(i)
                cd(i+1)=cd(i)
                sd(i+1)=sd(i)
            else
                theta=dfloat(i)*pi/4.0d0
                c=dcos(theta*vv)
                s=dsin(theta*vv)
                dc=-theta*s
                ds=theta*c
c
                ff=c*cf(i)+s*sf(i)
                dd=-s*cf(i)+c*sf(i)
                df=c*cd(i)+s*sd(i)+dc*cf(i)+ds*sf(i)
                dp=-s*cd(i)+c*sd(i)-ds*cf(i)+dc*sf(i)
c
                dd=dd*af(i)/af(i+1)
                dp=dp*af(i)/af(i+1)
c
                cf(i+1)=c*ff-s*dd
                sf(i+1)=s*ff+c*dd
                cd(i+1)=dc*ff-ds*dd+c*df-s*dp
                sd(i+1)=ds*ff+dc*dd+s*df+c*dp
            endif
        enddo
        theta=dfloat(ntri)*pi/4.0d0
        c=dcos(theta*vv)
        s=dsin(theta*vv)
        dc=-theta*s
        ds=theta*c
c
        dd=-s*cf(ntri)+c*sf(ntri)
        dp=-ds*cf(ntri)+dc*sf(ntri)-s*cd(ntri)+c*sd(ntri)
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine usrcmd
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            character*80
     +          sp,su,file(12)
            double precision
     +          psave(8)
            common /atest1/ip(100),rp(100),sp(100)
            common /atest2/iu(100),ru(100),su(100)
            save len,file
c
            data len/12/
            data (file(i),i=1,10)/
     +      'n i=1,n=a1,a=a1,t=r',
     1      'n i=2,n=a2,a=a2,t=r',
     2      'n i=3,n=a3,a=a3,t=r',
     3      'n i=4,n=a4,a=a4,t=r',
     4      'n i=5,n=a5,a=a5,t=r',
     5      'n i=6,n=a6,a=a6,t=r',
     6      'n i=7,n=a7,a=a7,t=r',
     7      'n i=8,n=a8,a=a8,t=r',
     8      'n i=1,n=ntri,a=nt,t=i',
     9      'n i=2,n=ibc,a=bc,t=i'/
            data (file(i),i=11,12)/
     +      's n=ibc,v=1,l="neumann"',
     1      's n=ibc,v=2,l="dirichlet"'/
c
        ntf=iu(1)
        ibc=iu(2)
        do i=1,8
            psave(i)=ru(i)
        enddo
c
        call usrset(file,len,iu,ru,su)
c
        iu(1)=min0(8,iu(1))
        iu(1)=max0(1,iu(1))
        if(iu(2).ne.1) iu(2)=2
        do i=1,8
            if(ru(i).le.0.0d0) ru(i)=1.0d0
        enddo
c
        if(ntf.ne.iu(1).or.ibc.ne.iu(2)) ip(41)=-1
        do i=1,8
            if(psave(i).ne.ru(i)) ip(41)=-1
        enddo
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gdata(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*)
            character*80
     +          sp(100),su(100)
            save ispd,iprob
c
c       unit circle divided into 8 equal parts.
c       this problem can be run with ntf=1,2,...8,
c       nvf=ntf+2,ncf=ntf,nbf=ntf+2
c
            data ispd,iprob/1,1/
c
        if(ip(41).eq.1) then
            sp(1)='circle'
            sp(2)='circle'
            sp(3)='circle'
            sp(4)='circle'
            sp(6)='circle_mpixxx.rw'
            sp(7)='circle.jnl'
            sp(9)='circle_mpixxx.out'
c
            iu(1)=8
            iu(2)=2
            do i=1,8
                ru(i)=1.0d0
            enddo
        endif
c
        ntf=iu(1)
        ibc=iu(2)
        nvf=ntf+2
        nbf=ntf+2
        ncf=1
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        ip(5)=1
        ip(6)=iprob
        ip(8)=ispd
c
        pi=3.141592653589793d0
        do i=1,ntf
            itnode(1,i)=1
            itnode(2,i)=i+1
            itnode(3,i)=i+2
            itnode(4,i)=0
            itnode(5,i)=i
        enddo
        vx(1)=0.0d0
        vy(1)=0.0d0
        do i=2,nvf
            arg=pi*dfloat(i-2)/4.0d0
            vx(i)=dcos(arg)
            vy(i)=dsin(arg)
        enddo
        xm(1)=0.0d0
        ym(1)=0.0d0
        do i=1,nbf
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=1
            ibndry(4,i)=ibc
            ibndry(5,i)=0
            ibndry(6,i)=i-1
        enddo
        ibndry(2,nbf)=1
        ibndry(3,1)=0
        ibndry(3,nbf)=0
        ibndry(4,1)=2
        ibndry(4,nbf)=1
c
c
c
        call cnu(ntf,ru(1),ru(9),ru(17),ru(25))
c
        sp(1)='alpha = '
        call sreal(sp(1)(9:9),nn,ru(25),3,1)
        sp(2)=sp(1)
        sp(3)=sp(1)
        sp(4)=sp(1)
        return
        end
