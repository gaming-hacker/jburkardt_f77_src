c***********************  problem name: naca    ************************
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
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
            common /atest2/iu(100),rminf,angle,
     +          uinf,dgamma,dgdm,rhoinf,dshift,eps,ru(92),su(100)
c
        r=rho(ux,uy,dr)
c
        values(k0)=ux*r
        values(kx)=r+ux*dr*ux+dshift
        values(ky)=ux*dr*uy
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
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
            common /atest2/iu(100),rminf,angle,
     +          uinf,dgamma,dgdm,rhoinf,dshift,eps,ru(92),su(100)
c
        r=rho(ux,uy,dr)
c
        values(k0)=uy*r
        values(kx)=uy*dr*ux
        values(ky)=r+uy*dr*uy+dshift
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
            common /atest2/iu(100),rminf,angle,
     +          uinf,dgamma,dgdm,rhoinf,dshift,eps,ru(92),su(100)
c
        if(rl.ne.rminf) then
            rminf=rl
            call cfd
        end if
c
        if(itag.le.0) return
        pi=3.141592653589793d0
        if(iu(1).ne.6) then
            ang=(1.0d0/8.0d0+dfloat(itag-1)/4.0d0-angle/180.0d0)*pi
        else
            ang=(dfloat(itag-1)/2.0d0-angle/180.0d0)*pi
        endif
        cc=dcos(ang)
        values(k0)=rhoinf*uinf*cc
        values(kl)=dgdm*cc
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
            common /atest2/iu(100),rminf,angle,
     +          uinf,dgamma,dgdm,rhoinf,dshift,eps,ru(92),su(100)
c
        if(rl.ne.rminf) then
            rminf=rl
            call cfd
        end if
c
        values(k0)=rhoinf*uinf*x
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
        values(k0)=u*u
        values(ku)=2.0d0*u
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
        if(itag.le.0) then
            values(k0)=ux*ux+uy*uy
            values(kx)=ux*2.0d0
            values(ky)=uy*2.0d0
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
            common /atest2/iu(100),rminf,angle,
     +          uinf,dgamma,dgdm,rhoinf,dshift,eps,ru(92),su(100)
c
        ss=ux**2+uy**2
        uu=1.0d0-ss
        if(uu.gt.0.0d0) then
            c=ss/uu
        else
            c=1.d20
        endif
        rmach=dsqrt(2*c/(dgamma-1.d0))
        values(kf)=rmach
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        double precision function rho(ux,uy,dr)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            character*80
     +          su
            common /atest2/iu(100),rminf,angle,
     +          uinf,dgamma,dgdm,rhoinf,dshift,eps,ru(92),su(100)
c
        gn=dgamma-1.0d0
        ss=ux**2+uy**2
        uu=1.0d0-ss
        if(uu.gt.eps) then
            rho=uu**(1.0d0/gn)
            dr=-2.0d0*(rho**(2.0d0-dgamma))/gn
        else
            fp=-dexp((uu-eps)/eps)
            ff=-fp*eps
            rho=ff**(1.0d0/gn)
            dr=fp*2.0d0*(rho**(2.0d0-dgamma))/gn
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
        double precision function wing(x)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
        z=x+0.5d0
        t=0.12d0
        q=(((.1015d0*z-.2843d0)*z+.3516d0)*z+.126d0)*z
        wing=5.0d0*t*(0.2969d0*dsqrt(z)-q)
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine cfd
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            character*80
     +          su
            common /atest2/iu(100),rminf,angle,
     +          uinf,dgamma,dgdm,rhoinf,dshift,eps,ru(92),su(100)
c
        a=2.0d0/(dgamma-1.0d0)
        c2=rminf**2+a
        c=dsqrt(c2)
        uinf=rminf/c
        rhoinf=rho(uinf,0.0d0,dr)
        dgdm=(dr*uinf**2+rhoinf)*a/(c2*c)
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
        subroutine usrcmd
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            character*80
     +          sp,su,file(20)
            common /atest1/ip(100),rp(100),sp(100)
            common /atest2/iu(100),ru(100),su(100)
            save len,file
c
            data len/14/
            data (file(i),i=  1, 10)/
     +      'n i= 1,n=domain,a= d,t=i',
     1      'n i= 1,n=minf  ,a= m,t=r',
     2      'n i= 2,n=angle ,a= a,t=r',
     3      'n i= 7,n=dshift,a=dd,t=r',
     4      'n i= 8,n=eps   ,a= e,t=r',
     5      'n i= 9,n=ratio ,a= r,t=r',
     6      'n i=10,n=size  ,a= s,t=r',
     7      's n=domain,v=1,l="naca 0012"',
     8      's n=domain,v=2,l="bi naca 0012"',
     9      's n=domain,v=3,l="two element airfoil"'/
            data (file(i),i= 11, 14)/
     +      's n=domain,v=4,l="three element airfoil"',
     1      's n=domain,v=5,l="three element airfoil"',
     2      's n=domain,v=6,l="ellipse"',
     3      's n=domain,v=7,l="three element airfoil"'/
c
c
        ii=iu(1)
        r=ru(9)
        s=ru(10)
c
        call usrset(file,len,iu,ru,su)
c
        ic=0
        if(ii.ne.iu(1)) ic=1
        if(r.ne.ru(9)) ic=1
        if(s.ne.ru(10)) ic=1
        if(ic.eq.0) then
            call cfd
        else
            iu(1)=max0(1,iu(1))
            iu(1)=min0(7,iu(1))
            ip(41)=-1
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
c
            save ispd,iprob,itask,size,ratio,dshift,eps
            save dgamma,angle,rminf
            data ispd,iprob,itask/1,3,0/
            data dgamma,angle,rminf/1.4d0,0.0d0,0.72d0/
            data dshift,eps/1.0d-3,1.0d-2/
            data size,ratio/8.0d0,1.0d0/
c
        if(ip(41).eq.1) then
            iu(1)=1
            ru(1)=rminf
            ru(2)=angle
            ru(4)=dgamma
            ru(7)=dshift
            ru(8)=eps
            ru(9)=ratio
            ru(10)=size
        endif
c
        call cfd
c
        if(iu(1).eq.1) call gd1(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.2) call gd2(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.3) call gd3(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.4) call gd4(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.5) call gd5(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.6) call gd6(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.7) call gd7(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
c
        ip(5)=1
        ip(6)=iprob
        ip(9)=itask
        ip(8)=ispd
        ip(26)=5
        sp(6)='naca_mpixxx.rw'
        sp(7)='naca.jnl'
        sp(9)='naca_mpixxx.out'
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gd1(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          xw(16),yw(16)
            character*80
     +          sp(100),su(100)
            save ntf,nvf,ncf,nbf,hmax,grade,xy,yw
c
            data ntf,nvf,ncf,nbf/2,42,0,44/
            data hmax,grade/0.1d0,1.75d0/
c
c       definition of naca0012
c
            data xw(1),yw(1)/.00351381d0,.00919038d0/
            data xw(2),yw(2)/.01346558d0,.01914215d0/
            data xw(3),yw(3)/.03066379d0,.02881318d0/
            data xw(4),yw(4)/.05517089d0,.03721619d0/
            data xw(5),yw(5)/.08673215d0,.04423213d0/
            data xw(6),yw(6)/.12514615d0,.05026549d0/
            data xw(7),yw(7)/.17041743d0,.05524749d0/
            data xw(8),yw(8)/.22262233d0,.05855709d0/
            data xw(9),yw(9)/.28171957d0,.06009519d0/
            data xw(10),yw(10)/.34769243d0,.05966640d0/
            data xw(11),yw(11)/.42050737d0,.05719906d0/
            data xw(12),yw(12)/.50013381d0,.05277705d0/
            data xw(13),yw(13)/.58655041d0,.04655302d0/
            data xw(14),yw(14)/.67974794d0,.03855282d0/
            data xw(15),yw(15)/.77972806d0,.02845609d0/
            data xw(16),yw(16)/.88648617d0,.01568145d0/
c
c
        sp(2)='naca 0012'
        sp(1)='naca 0012'
        sp(3)='naca 0012'
        sp(4)='naca 0012'
c
        rp(1)=ru(1)
        pi=3.141592653589793d0
c
        ip(7)=1
        size=ru(10)
        vx(1)=-0.5d0
        vy(1)=0.0d0
        vx(2)=0.5d0
        vy(2)=0.0d0
        do i=3,10
            arg=pi*dfloat(i-3)/4.0d0
            vx(i)=size*dcos(arg)
            vy(i)=size*dsin(arg)
        enddo
        do i=1,16
            vx(10+i)=xw(i)-.5d0
            vy(10+i)=yw(i)
            vx(26+i)=xw(i)-.5d0
            vy(26+i)=-yw(i)
        enddo
c
c
        do i=1,8
            ibndry(1,i)=i+2
            ibndry(2,i)=i+3
            ibndry(3,i)=0
            ibndry(4,i)=1
            ibndry(5,i)=0
            ibndry(6,i)=i
        enddo
        ibndry(2,8)=3
        ibndry(1,9)=7
        ibndry(2,9)=1
        ibndry(3,9)=0
        ibndry(4,9)=0
        ibndry(6,9)=0
        ibndry(1,10)=2
        ibndry(2,10)=3
        ibndry(3,10)=0
        ibndry(4,10)=0
        ibndry(6,10)=0
        do i=11,27
            ibndry(1,i)=i-1
            ibndry(2,i)=i
            ibndry(3,i)=0
            ibndry(4,i)=1
            ibndry(5,i)=0
            ibndry(6,i)=-1
        enddo
        ibndry(1,11)=1
        ibndry(2,27)=2
        do i=28,44
            ibndry(1,i)=i-2
            ibndry(2,i)=i-1
            ibndry(3,i)=0
            ibndry(4,i)=1
            ibndry(5,i)=0
            ibndry(6,i)=-2
        enddo
        ibndry(1,28)=1
        ibndry(2,44)=2
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
c
c       make itnode, find symmetries
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        call sklutl(2,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
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
        subroutine gd2(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          xw(16),yw(16)
            character*80
     +          sp(100),su(100)
            save ntf,nvf,ncf,nbf,hmax,grade
c
            data ntf,nvf,ncf,nbf/4,78,0,83/
            data hmax,grade/0.1d0,1.75d0/
c
c       definition of naca0012
c
            data xw(1),yw(1)/.00351381d0,.00919038d0/
            data xw(2),yw(2)/.01346558d0,.01914215d0/
            data xw(3),yw(3)/.03066379d0,.02881318d0/
            data xw(4),yw(4)/.05517089d0,.03721619d0/
            data xw(5),yw(5)/.08673215d0,.04423213d0/
            data xw(6),yw(6)/.12514615d0,.05026549d0/
            data xw(7),yw(7)/.17041743d0,.05524749d0/
            data xw(8),yw(8)/.22262233d0,.05855709d0/
            data xw(9),yw(9)/.28171957d0,.06009519d0/
            data xw(10),yw(10)/.34769243d0,.05966640d0/
            data xw(11),yw(11)/.42050737d0,.05719906d0/
            data xw(12),yw(12)/.50013381d0,.05277705d0/
            data xw(13),yw(13)/.58655041d0,.04655302d0/
            data xw(14),yw(14)/.67974794d0,.03855282d0/
            data xw(15),yw(15)/.77972806d0,.02845609d0/
            data xw(16),yw(16)/.88648617d0,.01568145d0/
c
c
        sp(2)='bi naca 0012'
        sp(1)='bi naca 0012'
        sp(3)='bi naca 0012'
        sp(4)='bi naca 0012'
c
        rp(1)=ru(1)
        size=ru(10)
c
        pi=3.141592653589793d0
c
        ip(7)=1
        vx(1)=-0.5d0
        vy(1)=0.0d0
        vx(2)=0.5d0
        vy(2)=0.0d0
        vx(75)=vx(1)
        vy(75)=0.25d0
        vx(76)=vx(2)
        vy(76)=0.25d0
        vx(77)=vx(1)
        vy(77)=-0.25d0
        vx(78)=vx(2)
        vy(78)=-0.25d0
        do i=3,10
            arg=pi*dfloat(i-3)/4.0d0
            vx(i)=size*dcos(arg)
            vy(i)=size*dsin(arg)
        enddo
        do i=1,16
            vx(10+i)=xw(i)-.5d0
            vy(10+i)=yw(i)+.25d0
            vx(26+i)=xw(i)-.5d0
            vy(26+i)=-yw(i)+.25d0
            vx(42+i)=xw(i)-.5d0
            vy(42+i)=yw(i)-.25d0
            vx(58+i)=xw(i)-.5d0
            vy(58+i)=-yw(i)-.25d0
        enddo
c
c
        do i=1,8
            ibndry(1,i)=i+2
            ibndry(2,i)=i+3
            ibndry(3,i)=0
            ibndry(4,i)=1
            ibndry(5,i)=0
            ibndry(6,i)=i
        enddo
        ibndry(2,8)=3
        ibndry(1,9)=7
        ibndry(2,9)=1
        ibndry(3,9)=0
        ibndry(4,9)=0
        ibndry(6,9)=0
        ibndry(1,10)=2
        ibndry(2,10)=3
        ibndry(3,10)=0
        ibndry(4,10)=0
        ibndry(6,10)=0
        do i=11,27
            ibndry(1,i)=i-1
            ibndry(2,i)=i
            ibndry(3,i)=0
            ibndry(4,i)=1
            ibndry(5,i)=0
            ibndry(6,i)=-1
        enddo
        ibndry(1,11)=75
        ibndry(2,27)=76
        do i=28,44
            ibndry(1,i)=i-2
            ibndry(2,i)=i-1
            ibndry(3,i)=0
            ibndry(4,i)=1
            ibndry(5,i)=0
            ibndry(6,i)=-2
        enddo
        ibndry(1,28)=75
        ibndry(2,44)=76
        do i=45,61
            ibndry(1,i)=i-3
            ibndry(2,i)=i-2
            ibndry(3,i)=0
            ibndry(4,i)=1
            ibndry(5,i)=0
            ibndry(6,i)=-3
        enddo
        ibndry(1,45)=77
        ibndry(2,61)=78
        do i=62,78
            ibndry(1,i)=i-4
            ibndry(2,i)=i-3
            ibndry(3,i)=0
            ibndry(4,i)=1
            ibndry(5,i)=0
            ibndry(6,i)=-4
        enddo
        ibndry(1,62)=77
        ibndry(2,78)=78
c
        do i=79,83
            ibndry(3,i)=0
            ibndry(4,i)=0
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
        ibndry(1,79)=1
        ibndry(2,79)=75
        ibndry(1,80)=1
        ibndry(2,80)=77
        ibndry(1,81)=2
        ibndry(2,81)=76
        ibndry(1,82)=2
        ibndry(2,82)=78
        ibndry(1,83)=2
        ibndry(2,83)=1
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
c
c       make itnode, find symmetries
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        call sklutl(2,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
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
        subroutine gd3(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(204),y(204)
            character*80
     +          sp(100),su(100)
            save ntf,nvf,ncf,nbf,hmax,grade,x,y
c
            data ntf,nvf,ncf,nbf/2,212,0,215/
            data hmax,grade/0.1d0,1.75d0/
c
            data x(  1),y(  1)/ 0.943600D+00, 0.149900D-01/
            data x(  2),y(  2)/ 0.942670D+00, 0.151900D-01/
            data x(  3),y(  3)/ 0.939880D+00, 0.158000D-01/
            data x(  4),y(  4)/ 0.935240D+00, 0.169000D-01/
            data x(  5),y(  5)/ 0.928780D+00, 0.183000D-01/
            data x(  6),y(  6)/ 0.920510D+00, 0.200800D-01/
            data x(  7),y(  7)/ 0.910470D+00, 0.224100D-01/
            data x(  8),y(  8)/ 0.898700D+00, 0.251200D-01/
            data x(  9),y(  9)/ 0.885240D+00, 0.281900D-01/
            data x( 10),y( 10)/ 0.870150D+00, 0.316200D-01/
            data x( 11),y( 11)/ 0.853490D+00, 0.353600D-01/
            data x( 12),y( 12)/ 0.835320D+00, 0.394100D-01/
            data x( 13),y( 13)/ 0.815720D+00, 0.436800D-01/
            data x( 14),y( 14)/ 0.794760D+00, 0.481400D-01/
            data x( 15),y( 15)/ 0.772530D+00, 0.527300D-01/
            data x( 16),y( 16)/ 0.749110D+00, 0.573500D-01/
            data x( 17),y( 17)/ 0.724590D+00, 0.619100D-01/
            data x( 18),y( 18)/ 0.699080D+00, 0.662800D-01/
            data x( 19),y( 19)/ 0.672670D+00, 0.703800D-01/
            data x( 20),y( 20)/ 0.645470D+00, 0.741000D-01/
            data x( 21),y( 21)/ 0.617580D+00, 0.773700D-01/
            data x( 22),y( 22)/ 0.589120D+00, 0.801500D-01/
            data x( 23),y( 23)/ 0.560190D+00, 0.824800D-01/
            data x( 24),y( 24)/ 0.530920D+00, 0.843800D-01/
            data x( 25),y( 25)/ 0.501410D+00, 0.859000D-01/
            data x( 26),y( 26)/ 0.471780D+00, 0.870400D-01/
            data x( 27),y( 27)/ 0.442160D+00, 0.878100D-01/
            data x( 28),y( 28)/ 0.412650D+00, 0.882600D-01/
            data x( 29),y( 29)/ 0.383370D+00, 0.883800D-01/
            data x( 30),y( 30)/ 0.354440D+00, 0.881800D-01/
            data x( 31),y( 31)/ 0.325980D+00, 0.876800D-01/
            data x( 32),y( 32)/ 0.298090D+00, 0.868700D-01/
            data x( 33),y( 33)/ 0.270890D+00, 0.857700D-01/
            data x( 34),y( 34)/ 0.244480D+00, 0.844000D-01/
            data x( 35),y( 35)/ 0.218970D+00, 0.827600D-01/
            data x( 36),y( 36)/ 0.194450D+00, 0.808700D-01/
            data x( 37),y( 37)/ 0.171030D+00, 0.787400D-01/
            data x( 38),y( 38)/ 0.148800D+00, 0.763800D-01/
            data x( 39),y( 39)/ 0.127840D+00, 0.738100D-01/
            data x( 40),y( 40)/ 0.117860D+00, 0.724500D-01/
            data x( 41),y( 41)/ 0.108240D+00, 0.710300D-01/
            data x( 42),y( 42)/ 0.989700D-01, 0.695800D-01/
            data x( 43),y( 43)/ 0.900700D-01, 0.680700D-01/
            data x( 44),y( 44)/ 0.817400D-01, 0.664750D-01/
            data x( 45),y( 45)/ 0.734100D-01, 0.648800D-01/
            data x( 46),y( 46)/ 0.656700D-01, 0.631700D-01/
            data x( 47),y( 47)/ 0.583200D-01, 0.613700D-01/
            data x( 48),y( 48)/ 0.513900D-01, 0.594300D-01/
            data x( 49),y( 49)/ 0.448700D-01, 0.573100D-01/
            data x( 50),y( 50)/ 0.387700D-01, 0.550600D-01/
            data x( 51),y( 51)/ 0.330900D-01, 0.526800D-01/
            data x( 52),y( 52)/ 0.278600D-01, 0.500900D-01/
            data x( 53),y( 53)/ 0.230500D-01, 0.471700D-01/
            data x( 54),y( 54)/ 0.187000D-01, 0.437300D-01/
            data x( 55),y( 55)/ 0.147900D-01, 0.397800D-01/
            data x( 56),y( 56)/ 0.113300D-01, 0.354400D-01/
            data x( 57),y( 57)/ 0.832000D-02, 0.307600D-01/
            data x( 58),y( 58)/ 0.577000D-02, 0.258200D-01/
            data x( 59),y( 59)/ 0.368000D-02, 0.206400D-01/
            data x( 60),y( 60)/ 0.206000D-02, 0.151600D-01/
            data x( 61),y( 61)/ 0.890000D-03, 0.959000D-02/
            data x( 62),y( 62)/ 0.190000D-03, 0.422000D-02/
            data x( 63),y( 63)/-0.400000D-04,-0.800000D-03/
            data x( 64),y( 64)/ 0.190000D-03,-0.552000D-02/
            data x( 65),y( 65)/ 0.890000D-03,-0.992000D-02/
            data x( 66),y( 66)/ 0.206000D-02,-0.141700D-01/
            data x( 67),y( 67)/ 0.368000D-02,-0.180400D-01/
            data x( 68),y( 68)/ 0.577000D-02,-0.217300D-01/
            data x( 69),y( 69)/ 0.832000D-02,-0.252100D-01/
            data x( 70),y( 70)/ 0.113300D-01,-0.285100D-01/
            data x( 71),y( 71)/ 0.147900D-01,-0.315800D-01/
            data x( 72),y( 72)/ 0.187000D-01,-0.344500D-01/
            data x( 73),y( 73)/ 0.230500D-01,-0.371700D-01/
            data x( 74),y( 74)/ 0.278600D-01,-0.397200D-01/
            data x( 75),y( 75)/ 0.331000D-01,-0.421500D-01/
            data x( 76),y( 76)/ 0.387700D-01,-0.444300D-01/
            data x( 77),y( 77)/ 0.448700D-01,-0.466100D-01/
            data x( 78),y( 78)/ 0.513900D-01,-0.487300D-01/
            data x( 79),y( 79)/ 0.583200D-01,-0.507300D-01/
            data x( 80),y( 80)/ 0.656700D-01,-0.526300D-01/
            data x( 81),y( 81)/ 0.734100D-01,-0.544600D-01/
            data x( 82),y( 82)/ 0.815500D-01,-0.562200D-01/
            data x( 83),y( 83)/ 0.900700D-01,-0.579100D-01/
            data x( 84),y( 84)/ 0.989700D-01,-0.595400D-01/
            data x( 85),y( 85)/ 0.108240D+00,-0.610900D-01/
            data x( 86),y( 86)/ 0.117860D+00,-0.625900D-01/
            data x( 87),y( 87)/ 0.127840D+00,-0.640200D-01/
            data x( 88),y( 88)/ 0.148800D+00,-0.667200D-01/
            data x( 89),y( 89)/ 0.171030D+00,-0.691700D-01/
            data x( 90),y( 90)/ 0.194450D+00,-0.713200D-01/
            data x( 91),y( 91)/ 0.218970D+00,-0.731700D-01/
            data x( 92),y( 92)/ 0.244480D+00,-0.747000D-01/
            data x( 93),y( 93)/ 0.270890D+00,-0.758500D-01/
            data x( 94),y( 94)/ 0.298090D+00,-0.766600D-01/
            data x( 95),y( 95)/ 0.325980D+00,-0.770400D-01/
            data x( 96),y( 96)/ 0.354440D+00,-0.770100D-01/
            data x( 97),y( 97)/ 0.383370D+00,-0.765100D-01/
            data x( 98),y( 98)/ 0.412650D+00,-0.754600D-01/
            data x( 99),y( 99)/ 0.442160D+00,-0.737800D-01/
            data x(100),y(100)/ 0.471780D+00,-0.713300D-01/
            data x(101),y(101)/ 0.501410D+00,-0.680300D-01/
            data x(102),y(102)/ 0.530920D+00,-0.639600D-01/
            data x(103),y(103)/ 0.560190D+00,-0.593200D-01/
            data x(104),y(104)/ 0.589120D+00,-0.542600D-01/
            data x(105),y(105)/ 0.617580D+00,-0.488200D-01/
            data x(106),y(106)/ 0.645470D+00,-0.431000D-01/
            data x(107),y(107)/ 0.672670D+00,-0.369900D-01/
            data x(108),y(108)/ 0.699080D+00,-0.302300D-01/
            data x(109),y(109)/ 0.724590D+00,-0.230000D-01/
            data x(110),y(110)/ 0.749110D+00,-0.153000D-01/
            data x(111),y(111)/ 0.772530D+00,-0.752000D-02/
            data x(112),y(112)/ 0.794760D+00,-0.500000D-04/
            data x(113),y(113)/ 0.815720D+00, 0.664000D-02/
            data x(114),y(114)/ 0.835320D+00, 0.119600D-01/
            data x(115),y(115)/ 0.853490D+00, 0.155600D-01/
            data x(116),y(116)/ 0.870150D+00, 0.178700D-01/
            data x(117),y(117)/ 0.885240D+00, 0.190500D-01/
            data x(118),y(118)/ 0.898700D+00, 0.193100D-01/
            data x(119),y(119)/ 0.910470D+00, 0.189500D-01/
            data x(120),y(120)/ 0.920510D+00, 0.182200D-01/
            data x(121),y(121)/ 0.928780D+00, 0.173400D-01/
            data x(122),y(122)/ 0.935240D+00, 0.161900D-01/
            data x(123),y(123)/ 0.939880D+00, 0.154900D-01/
            data x(124),y(124)/ 0.942670D+00, 0.151200D-01/
            data x(125),y(125)/ 0.120177D+01,-0.103700D+00/
            data x(126),y(126)/ 0.120069D+01,-0.102980D+00/
            data x(127),y(127)/ 0.119743D+01,-0.100940D+00/
            data x(128),y(128)/ 0.119210D+01,-0.975000D-01/
            data x(129),y(129)/ 0.118479D+01,-0.926300D-01/
            data x(130),y(130)/ 0.117547D+01,-0.868200D-01/
            data x(131),y(131)/ 0.116424D+01,-0.803000D-01/
            data x(132),y(132)/ 0.115127D+01,-0.732300D-01/
            data x(133),y(133)/ 0.113669D+01,-0.658500D-01/
            data x(134),y(134)/ 0.112076D+01,-0.582600D-01/
            data x(135),y(135)/ 0.110367D+01,-0.507100D-01/
            data x(136),y(136)/ 0.109477D+01,-0.470000D-01/
            data x(137),y(137)/ 0.108568D+01,-0.433700D-01/
            data x(138),y(138)/ 0.107641D+01,-0.398600D-01/
            data x(139),y(139)/ 0.106701D+01,-0.365000D-01/
            data x(140),y(140)/ 0.105751D+01,-0.333000D-01/
            data x(141),y(141)/ 0.104794D+01,-0.302700D-01/
            data x(142),y(142)/ 0.103835D+01,-0.274300D-01/
            data x(143),y(143)/ 0.102877D+01,-0.247700D-01/
            data x(144),y(144)/ 0.101925D+01,-0.223200D-01/
            data x(145),y(145)/ 0.100982D+01,-0.200800D-01/
            data x(146),y(146)/ 0.100050D+01,-0.181000D-01/
            data x(147),y(147)/ 0.991320D+00,-0.163900D-01/
            data x(148),y(148)/ 0.982330D+00,-0.149800D-01/
            data x(149),y(149)/ 0.973550D+00,-0.138400D-01/
            data x(150),y(150)/ 0.965040D+00,-0.129800D-01/
            data x(151),y(151)/ 0.956830D+00,-0.123700D-01/
            data x(152),y(152)/ 0.948940D+00,-0.120400D-01/
            data x(153),y(153)/ 0.941420D+00,-0.119800D-01/
            data x(154),y(154)/ 0.934300D+00,-0.121500D-01/
            data x(155),y(155)/ 0.927630D+00,-0.124900D-01/
            data x(156),y(156)/ 0.921460D+00,-0.129600D-01/
            data x(157),y(157)/ 0.915820D+00,-0.135600D-01/
            data x(158),y(158)/ 0.910710D+00,-0.142900D-01/
            data x(159),y(159)/ 0.906120D+00,-0.152300D-01/
            data x(160),y(160)/ 0.902090D+00,-0.163800D-01/
            data x(161),y(161)/ 0.898620D+00,-0.177300D-01/
            data x(162),y(162)/ 0.895730D+00,-0.192900D-01/
            data x(163),y(163)/ 0.893400D+00,-0.210900D-01/
            data x(164),y(164)/ 0.891690D+00,-0.230400D-01/
            data x(165),y(165)/ 0.890600D+00,-0.250900D-01/
            data x(166),y(166)/ 0.890270D+00,-0.269400D-01/
            data x(167),y(167)/ 0.890600D+00,-0.287700D-01/
            data x(168),y(168)/ 0.891660D+00,-0.304700D-01/
            data x(169),y(169)/ 0.893570D+00,-0.316200D-01/
            data x(170),y(170)/ 0.896290D+00,-0.323200D-01/
            data x(171),y(171)/ 0.899720D+00,-0.328000D-01/
            data x(172),y(172)/ 0.903790D+00,-0.333100D-01/
            data x(173),y(173)/ 0.908420D+00,-0.338900D-01/
            data x(174),y(174)/ 0.913600D+00,-0.345600D-01/
            data x(175),y(175)/ 0.919320D+00,-0.353300D-01/
            data x(176),y(176)/ 0.925570D+00,-0.361500D-01/
            data x(177),y(177)/ 0.932300D+00,-0.370400D-01/
            data x(178),y(178)/ 0.939500D+00,-0.379900D-01/
            data x(179),y(179)/ 0.947130D+00,-0.390100D-01/
            data x(180),y(180)/ 0.955170D+00,-0.401000D-01/
            data x(181),y(181)/ 0.963580D+00,-0.412400D-01/
            data x(182),y(182)/ 0.972320D+00,-0.424800D-01/
            data x(183),y(183)/ 0.981360D+00,-0.437700D-01/
            data x(184),y(184)/ 0.990650D+00,-0.451600D-01/
            data x(185),y(185)/ 0.100016D+01,-0.466100D-01/
            data x(186),y(186)/ 0.100984D+01,-0.481700D-01/
            data x(187),y(187)/ 0.101966D+01,-0.498000D-01/
            data x(188),y(188)/ 0.102958D+01,-0.515200D-01/
            data x(189),y(189)/ 0.103955D+01,-0.533200D-01/
            data x(190),y(190)/ 0.104953D+01,-0.552300D-01/
            data x(191),y(191)/ 0.105947D+01,-0.572100D-01/
            data x(192),y(192)/ 0.106933D+01,-0.593100D-01/
            data x(193),y(193)/ 0.107907D+01,-0.615000D-01/
            data x(194),y(194)/ 0.108866D+01,-0.637800D-01/
            data x(195),y(195)/ 0.109805D+01,-0.661400D-01/
            data x(196),y(196)/ 0.111608D+01,-0.711000D-01/
            data x(197),y(197)/ 0.113290D+01,-0.762400D-01/
            data x(198),y(198)/ 0.114827D+01,-0.814700D-01/
            data x(199),y(199)/ 0.116198D+01,-0.865200D-01/
            data x(200),y(200)/ 0.117387D+01,-0.912100D-01/
            data x(201),y(201)/ 0.118373D+01,-0.955000D-01/
            data x(202),y(202)/ 0.119153D+01,-0.990700D-01/
            data x(203),y(203)/ 0.119717D+01,-0.101650D+00/
            data x(204),y(204)/ 0.120062D+01,-0.103160D+00/
c
        sp(2)='wing'
        sp(1)='wing'
        sp(3)='wing'
        sp(4)='wing'
c
        ip(7)=71
        pi=3.141592653589793d0
        nw=9
        nf=133
c
        size=ru(10)
        do i=1,8
            arg=pi*dfloat(i-1)/4.0d0
            vx(i)=size*dcos(arg)
            vy(i)=size*dsin(arg)
        enddo
        xmin=x(1)
        xmax=x(1)
        ymin=y(1)
        ymax=y(1)
        do i=1,nvf-8
           xmax=dmax1(xmax,x(i))
           xmin=dmin1(xmin,x(i))
           ymax=dmax1(ymax,y(i))
           ymin=dmin1(ymin,y(i))
        enddo
        xx=(xmin+xmax)/2.0d0
        yy=(ymin+ymax)/2.0d0
        ss=dmax1(xmax-xmin,ymax-ymin)
        do i=9,nvf
            vx(i)=(x(i-8)-xx)/ss
            vy(i)=(y(i-8)-yy)/ss
        enddo
c
        do i=1,nbf
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            if(i.le.nvf) then
                ibndry(4,i)=1
            else
                ibndry(4,i)=0
            endif
            ibndry(5,i)=0
            if(i.lt.nw) then
                ibndry(6,i)=i
            else
                ibndry(6,i)=-1
            endif
        enddo
        ibndry(2,nw-1)=1
        ibndry(2,nf-1)=nw
        ibndry(2,nvf)=nf
c
        ibndry(1,nvf+1)=5
        ibndry(2,nvf+1)=71
        ibndry(1,nvf+2)=9
        ibndry(2,nvf+2)=161
        ibndry(1,nvf+3)=133
        ibndry(2,nvf+3)=1
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
c
c       make itnode
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
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
        subroutine gd4(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(193),y(193)
            character*80
     +          sp(100),su(100)
            save ntf,nvf,ncf,nbf,hmax,grade,x,y
c
            data ntf,nvf,ncf,nbf/2,201,0,205/
            data hmax,grade/0.1d0,1.75d0/
c
            data x(  1),y(  1)/ 0.14238694d+00, 0.52549636d+00/
            data x(  2),y(  2)/ 0.14239907d+00, 0.52486056d+00/
            data x(  3),y(  3)/ 0.14246652d+00, 0.52383298d+00/
            data x(  4),y(  4)/ 0.14251776d+00, 0.52338821d+00/
            data x(  5),y(  5)/ 0.14258587d+00, 0.52297705d+00/
            data x(  6),y(  6)/ 0.14267555d+00, 0.52259350d+00/
            data x(  7),y(  7)/ 0.14279018d+00, 0.52223974d+00/
            data x(  8),y(  8)/ 0.14293651d+00, 0.52190989d+00/
            data x(  9),y(  9)/ 0.14311521d+00, 0.52160990d+00/
            data x( 10),y( 10)/ 0.14331953d+00, 0.52134800d+00/
            data x( 11),y( 11)/ 0.14354138d+00, 0.52112484d+00/
            data x( 12),y( 12)/ 0.14378212d+00, 0.52092934d+00/
            data x( 13),y( 13)/ 0.14403769d+00, 0.52076072d+00/
            data x( 14),y( 14)/ 0.14430405d+00, 0.52061445d+00/
            data x( 15),y( 15)/ 0.14457782d+00, 0.52048606d+00/
            data x( 16),y( 16)/ 0.14485565d+00, 0.52037263d+00/
            data x( 17),y( 17)/ 0.14513819d+00, 0.52027190d+00/
            data x( 18),y( 18)/ 0.14599727d+00, 0.52001894d+00/
            data x( 19),y( 19)/ 0.18376550d+00, 0.51373845d+00/
            data x( 20),y( 20)/ 0.18985666d+00, 0.51254296d+00/
            data x( 21),y( 21)/ 0.19675030d+00, 0.51153928d+00/
            data x( 22),y( 22)/ 0.24406897d+00, 0.50666326d+00/
            data x( 23),y( 23)/ 0.31060791d+00, 0.50356334d+00/
            data x( 24),y( 24)/ 0.38332841d+00, 0.50502747d+00/
            data x( 25),y( 25)/ 0.45327878d+00, 0.51030499d+00/
            data x( 26),y( 26)/ 0.51958978d+00, 0.51920772d+00/
            data x( 27),y( 27)/ 0.57746434d+00, 0.52987391d+00/
            data x( 28),y( 28)/ 0.58425075d+00, 0.53082687d+00/
            data x( 29),y( 29)/ 0.59000003d+00, 0.53114921d+00/
            data x( 30),y( 30)/ 0.59512764d+00, 0.53169250d+00/
            data x( 31),y( 31)/ 0.59887820d+00, 0.53272384d+00/
            data x( 32),y( 32)/ 0.60137254d+00, 0.53386259d+00/
            data x( 33),y( 33)/ 0.60320872d+00, 0.53513271d+00/
            data x( 34),y( 34)/ 0.60353845d+00, 0.53771621d+00/
            data x( 35),y( 35)/ 0.60296059d+00, 0.54131466d+00/
            data x( 36),y( 36)/ 0.60300571d+00, 0.54437202d+00/
            data x( 37),y( 37)/ 0.60357350d+00, 0.54693913d+00/
            data x( 38),y( 38)/ 0.60406649d+00, 0.54854357d+00/
            data x( 39),y( 39)/ 0.60466123d+00, 0.55010843d+00/
            data x( 40),y( 40)/ 0.60535777d+00, 0.55162632d+00/
            data x( 41),y( 41)/ 0.60614204d+00, 0.55309492d+00/
            data x( 42),y( 42)/ 0.60702199d+00, 0.55450010d+00/
            data x( 43),y( 43)/ 0.60800248d+00, 0.55582470d+00/
            data x( 44),y( 44)/ 0.60907596d+00, 0.55706424d+00/
            data x( 45),y( 45)/ 0.61022371d+00, 0.55822390d+00/
            data x( 46),y( 46)/ 0.61142939d+00, 0.55931419d+00/
            data x( 47),y( 47)/ 0.61268497d+00, 0.56033880d+00/
            data x( 48),y( 48)/ 0.61398643d+00, 0.56129622d+00/
            data x( 49),y( 49)/ 0.61533439d+00, 0.56217754d+00/
            data x( 50),y( 50)/ 0.61672550d+00, 0.56297755d+00/
            data x( 51),y( 51)/ 0.61815780d+00, 0.56369096d+00/
            data x( 52),y( 52)/ 0.61962718d+00, 0.56431180d+00/
            data x( 53),y( 53)/ 0.62112886d+00, 0.56483644d+00/
            data x( 54),y( 54)/ 0.62420315d+00, 0.56562895d+00/
            data x( 55),y( 55)/ 0.62575811d+00, 0.56594610d+00/
            data x( 56),y( 56)/ 0.62732589d+00, 0.56619239d+00/
            data x( 57),y( 57)/ 0.62890446d+00, 0.56634086d+00/
            data x( 58),y( 58)/ 0.63049120d+00, 0.56637746d+00/
            data x( 59),y( 59)/ 0.63217700d+00, 0.56633490d+00/
            data x( 60),y( 60)/ 0.63928777d+00, 0.56594688d+00/
            data x( 61),y( 61)/ 0.64237547d+00, 0.56590432d+00/
            data x( 62),y( 62)/ 0.64175850d+00, 0.56613564d+00/
            data x( 63),y( 63)/ 0.64074898d+00, 0.56639910d+00/
            data x( 64),y( 64)/ 0.63777864d+00, 0.56704980d+00/
            data x( 65),y( 65)/ 0.61441934d+00, 0.57083333d+00/
            data x( 66),y( 66)/ 0.53095484d+00, 0.58100241d+00/
            data x( 67),y( 67)/ 0.44016385d+00, 0.58623964d+00/
            data x( 68),y( 68)/ 0.35042948d+00, 0.58610827d+00/
            data x( 69),y( 69)/ 0.24714592d+00, 0.58002561d+00/
            data x( 70),y( 70)/ 0.22205499d+00, 0.57699656d+00/
            data x( 71),y( 71)/ 0.21458752d+00, 0.57549435d+00/
            data x( 72),y( 72)/ 0.20755161d+00, 0.57364964d+00/
            data x( 73),y( 73)/ 0.20098640d+00, 0.57163399d+00/
            data x( 74),y( 74)/ 0.19478193d+00, 0.56941843d+00/
            data x( 75),y( 75)/ 0.18897466d+00, 0.56701398d+00/
            data x( 76),y( 76)/ 0.18352342d+00, 0.56447226d+00/
            data x( 77),y( 77)/ 0.17845722d+00, 0.56187308d+00/
            data x( 78),y( 78)/ 0.16923651d+00, 0.55659187d+00/
            data x( 79),y( 79)/ 0.16123903d+00, 0.55137855d+00/
            data x( 80),y( 80)/ 0.15775546d+00, 0.54884428d+00/
            data x( 81),y( 81)/ 0.15473247d+00, 0.54644585d+00/
            data x( 82),y( 82)/ 0.15211743d+00, 0.54412651d+00/
            data x( 83),y( 83)/ 0.14972629d+00, 0.54171610d+00/
            data x( 84),y( 84)/ 0.14759408d+00, 0.53918707d+00/
            data x( 85),y( 85)/ 0.14578351d+00, 0.53658718d+00/
            data x( 86),y( 86)/ 0.14434788d+00, 0.53393197d+00/
            data x( 87),y( 87)/ 0.14338359d+00, 0.53158128d+00/
            data x( 88),y( 88)/ 0.14286031d+00, 0.52970970d+00/
            data x( 89),y( 89)/ 0.14257507d+00, 0.52820081d+00/
            data x( 90),y( 90)/ 0.14245167d+00, 0.52710903d+00/
            data x( 91),y( 91)/ 0.14239976d+00, 0.52623296d+00/
            data x( 92),y( 92)/ 0.64350897d+00, 0.55603290d+00/
            data x( 93),y( 93)/ 0.64356840d+00, 0.55530572d+00/
            data x( 94),y( 94)/ 0.64368635d+00, 0.55459970d+00/
            data x( 95),y( 95)/ 0.64387923d+00, 0.55391252d+00/
            data x( 96),y( 96)/ 0.64416379d+00, 0.55330718d+00/
            data x( 97),y( 97)/ 0.64450228d+00, 0.55284947d+00/
            data x( 98),y( 98)/ 0.64483809d+00, 0.55251265d+00/
            data x( 99),y( 99)/ 0.64515030d+00, 0.55225414d+00/
            data x(100),y(100)/ 0.64539981d+00, 0.55209374d+00/
            data x(101),y(101)/ 0.64560950d+00, 0.55199981d+00/
            data x(102),y(102)/ 0.65000343d+00, 0.55015725d+00/
            data x(103),y(103)/ 0.65195090d+00, 0.54946691d+00/
            data x(104),y(104)/ 0.65355039d+00, 0.54898107d+00/
            data x(105),y(105)/ 0.65445668d+00, 0.54873705d+00/
            data x(106),y(106)/ 0.65544385d+00, 0.54851377d+00/
            data x(107),y(107)/ 0.65652210d+00, 0.54833364d+00/
            data x(108),y(108)/ 0.65895778d+00, 0.54804218d+00/
            data x(109),y(109)/ 0.66510695d+00, 0.54695308d+00/
            data x(110),y(110)/ 0.66893238d+00, 0.54609281d+00/
            data x(111),y(111)/ 0.67091352d+00, 0.54556215d+00/
            data x(112),y(112)/ 0.67275780d+00, 0.54498011d+00/
            data x(113),y(113)/ 0.68000346d+00, 0.54238534d+00/
            data x(114),y(114)/ 0.68109047d+00, 0.54183996d+00/
            data x(115),y(115)/ 0.68448633d+00, 0.53959823d+00/
            data x(116),y(116)/ 0.68519235d+00, 0.53916425d+00/
            data x(117),y(117)/ 0.68585521d+00, 0.53878689d+00/
            data x(118),y(118)/ 0.68608046d+00, 0.53896588d+00/
            data x(119),y(119)/ 0.68600219d+00, 0.53920758d+00/
            data x(120),y(120)/ 0.68589300d+00, 0.53945839d+00/
            data x(121),y(121)/ 0.68574667d+00, 0.53972352d+00/
            data x(122),y(122)/ 0.68555248d+00, 0.54002571d+00/
            data x(123),y(123)/ 0.68428069d+00, 0.54174429d+00/
            data x(124),y(124)/ 0.68324089d+00, 0.54303318d+00/
            data x(125),y(125)/ 0.68181133d+00, 0.54464948d+00/
            data x(126),y(126)/ 0.67793530d+00, 0.54862797d+00/
            data x(127),y(127)/ 0.67259532d+00, 0.55358279d+00/
            data x(128),y(128)/ 0.66974699d+00, 0.55597788d+00/
            data x(129),y(129)/ 0.66684467d+00, 0.55816221d+00/
            data x(130),y(130)/ 0.66362411d+00, 0.56015337d+00/
            data x(131),y(131)/ 0.65993828d+00, 0.56181592d+00/
            data x(132),y(132)/ 0.65620720d+00, 0.56289274d+00/
            data x(133),y(133)/ 0.65288752d+00, 0.56327605d+00/
            data x(134),y(134)/ 0.65006143d+00, 0.56308872d+00/
            data x(135),y(135)/ 0.64778221d+00, 0.56250918d+00/
            data x(136),y(136)/ 0.64616859d+00, 0.56174362d+00/
            data x(137),y(137)/ 0.64526492d+00, 0.56106269d+00/
            data x(138),y(138)/ 0.64485633d+00, 0.56061620d+00/
            data x(139),y(139)/ 0.64467561d+00, 0.56034172d+00/
            data x(140),y(140)/ 0.64456904d+00, 0.56012821d+00/
            data x(141),y(141)/ 0.64448071d+00, 0.55992335d+00/
            data x(142),y(142)/ 0.64389944d+00, 0.55821323d+00/
            data x(143),y(143)/ 0.64369786d+00, 0.55753338d+00/
            data x(144),y(144)/ 0.64354610d+00, 0.55678207d+00/
            data x(145),y(145)/ 0.66136104d+00, 0.51671529d+00/
            data x(146),y(146)/ 0.66146421d+00, 0.51561081d+00/
            data x(147),y(147)/ 0.66190052d+00, 0.51450264d+00/
            data x(148),y(148)/ 0.66259372d+00, 0.51342881d+00/
            data x(149),y(149)/ 0.66350609d+00, 0.51241684d+00/
            data x(150),y(150)/ 0.66900992d+00, 0.50778043d+00/
            data x(151),y(151)/ 0.69012105d+00, 0.48983982d+00/
            data x(152),y(152)/ 0.71521938d+00, 0.46596432d+00/
            data x(153),y(153)/ 0.73760015d+00, 0.44191343d+00/
            data x(154),y(154)/ 0.77687955d+00, 0.39426166d+00/
            data x(155),y(155)/ 0.77824843d+00, 0.39296392d+00/
            data x(156),y(156)/ 0.77903742d+00, 0.39230424d+00/
            data x(157),y(157)/ 0.77885532d+00, 0.39332363d+00/
            data x(158),y(158)/ 0.77832937d+00, 0.39514297d+00/
            data x(159),y(159)/ 0.77710885d+00, 0.39755410d+00/
            data x(160),y(160)/ 0.71374595d+00, 0.49894109d+00/
            data x(161),y(161)/ 0.70879847d+00, 0.50545138d+00/
            data x(162),y(162)/ 0.70390487d+00, 0.51122659d+00/
            data x(163),y(163)/ 0.70148945d+00, 0.51380640d+00/
            data x(164),y(164)/ 0.69909018d+00, 0.51613098d+00/
            data x(165),y(165)/ 0.69670445d+00, 0.51818985d+00/
            data x(166),y(166)/ 0.69435513d+00, 0.51994878d+00/
            data x(167),y(167)/ 0.69207388d+00, 0.52144128d+00/
            data x(168),y(168)/ 0.68990386d+00, 0.52269793d+00/
            data x(169),y(169)/ 0.68787146d+00, 0.52373749d+00/
            data x(170),y(170)/ 0.68599886d+00, 0.52457106d+00/
            data x(171),y(171)/ 0.68458074d+00, 0.52512252d+00/
            data x(172),y(172)/ 0.68323749d+00, 0.52557099d+00/
            data x(173),y(173)/ 0.68186861d+00, 0.52594560d+00/
            data x(174),y(174)/ 0.68048090d+00, 0.52624112d+00/
            data x(175),y(175)/ 0.67908096d+00, 0.52644932d+00/
            data x(176),y(176)/ 0.67767704d+00, 0.52655977d+00/
            data x(177),y(177)/ 0.67627919d+00, 0.52656651d+00/
            data x(178),y(178)/ 0.67489743d+00, 0.52647096d+00/
            data x(179),y(179)/ 0.67353940d+00, 0.52629191d+00/
            data x(180),y(180)/ 0.67221367d+00, 0.52603441d+00/
            data x(181),y(181)/ 0.67092770d+00, 0.52570385d+00/
            data x(182),y(182)/ 0.66968828d+00, 0.52530611d+00/
            data x(183),y(183)/ 0.66850287d+00, 0.52484566d+00/
            data x(184),y(184)/ 0.66738081d+00, 0.52432030d+00/
            data x(185),y(185)/ 0.66633016d+00, 0.52373523d+00/
            data x(186),y(186)/ 0.66536587d+00, 0.52308375d+00/
            data x(187),y(187)/ 0.66449600d+00, 0.52237707d+00/
            data x(188),y(188)/ 0.66372591d+00, 0.52162856d+00/
            data x(189),y(189)/ 0.66306305d+00, 0.52085173d+00/
            data x(190),y(190)/ 0.66250944d+00, 0.52006221d+00/
            data x(191),y(191)/ 0.66206914d+00, 0.51927567d+00/
            data x(192),y(192)/ 0.66177577d+00, 0.51860404d+00/
            data x(193),y(193)/ 0.66150331d+00, 0.51772273d+00/
c
        sp(2)='wing'
        sp(1)='wing'
        sp(3)='wing'
        sp(4)='wing'
c
        pi=3.141592653589793d0
        nw=9
        nf1=100
        nf2=153
c
        ip(7)=10
        size=ru(10)
        do i=1,8
            arg=pi*dfloat(i-1)/4.0d0
            vx(i)=size*dcos(arg)
            vy(i)=size*dsin(arg)
        enddo
        xmin=x(1)
        xmax=x(1)
        ymin=y(1)
        ymax=y(1)
        do i=1,nvf-8
           xmax=dmax1(xmax,x(i))
           xmin=dmin1(xmin,x(i))
           ymax=dmax1(ymax,y(i))
           ymin=dmin1(ymin,y(i))
        enddo
        xx=(xmin+xmax)/2.0d0
        yy=(ymin+ymax)/2.0d0
        ss=dmax1(xmax-xmin,ymax-ymin)
        do i=9,nvf
            vx(i)=(x(i-8)-xx)/ss
            vy(i)=(y(i-8)-yy)/ss
        enddo
c
        do i=1,nbf
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            if(i.le.nvf) then
                ibndry(4,i)=1
            else
                ibndry(4,i)=0
            endif
            ibndry(5,i)=0
            if(i.lt.nw) then
                ibndry(6,i)=i
            else
                ibndry(6,i)=-1
            endif
        enddo
        ibndry(2,nw-1)=1
        ibndry(2,nf1-1)=nw
        ibndry(2,nf2-1)=nf1
        ibndry(2,nvf)=nf2
c
        ibndry(1,nvf+1)=5
        ibndry(2,nvf+1)=10
        ibndry(1,nvf+2)=69
        ibndry(2,nvf+2)=144
        ibndry(1,nvf+3)=125
        ibndry(2,nvf+3)=180
        ibndry(1,nvf+4)=164
        ibndry(2,nvf+4)=1
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
c
c       make itnode
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
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
        subroutine gd5(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(165),y(165)
            character*80
     +          sp(100),su(100)
            save ntf,nvf,ncf,nbf,hmax,grade,x,y
c
            data ntf,nvf,ncf,nbf/2,173,0,177/
            data hmax,grade/0.1d0,1.75d0/
c
c       the main wing
c
            data x(  1),y(  1)/ 0.4800000D+03,-0.5630000D+01/
            data x(  2),y(  2)/ 0.4700000D+03,-0.3380000D+01/
            data x(  3),y(  3)/ 0.4600000D+03,-0.1150000D+01/
            data x(  4),y(  4)/ 0.4500000D+03, 0.1120000D+01/
            data x(  5),y(  5)/ 0.4400000D+03, 0.3450000D+01/
            data x(  6),y(  6)/ 0.4200000D+03, 0.7920000D+01/
            data x(  7),y(  7)/ 0.4000000D+03, 0.1208000D+02/
            data x(  8),y(  8)/ 0.3800000D+03, 0.1585000D+02/
            data x(  9),y(  9)/ 0.3600000D+03, 0.1921000D+02/
            data x( 10),y( 10)/ 0.3400000D+03, 0.2215000D+02/
            data x( 11),y( 11)/ 0.3200000D+03, 0.2472000D+02/
            data x( 12),y( 12)/ 0.3000000D+03, 0.2694000D+02/
            data x( 13),y( 13)/ 0.2800000D+03, 0.2883000D+02/
            data x( 14),y( 14)/ 0.2600000D+03, 0.3043000D+02/
            data x( 15),y( 15)/ 0.2400000D+03, 0.3174000D+02/
            data x( 16),y( 16)/ 0.2200000D+03, 0.3278000D+02/
            data x( 17),y( 17)/ 0.2000000D+03, 0.3354000D+02/
            data x( 18),y( 18)/ 0.1800000D+03, 0.3399000D+02/
            data x( 19),y( 19)/ 0.1600000D+03, 0.3412000D+02/
            data x( 20),y( 20)/ 0.1400000D+03, 0.3390000D+02/
            data x( 21),y( 21)/ 0.1200000D+03, 0.3330000D+02/
            data x( 22),y( 22)/ 0.1000000D+03, 0.3227000D+02/
            data x( 23),y( 23)/ 0.8000000D+02, 0.3077000D+02/
            data x( 24),y( 24)/ 0.6000000D+02, 0.2868000D+02/
            data x( 25),y( 25)/ 0.5000000D+02, 0.2731000D+02/
            data x( 26),y( 26)/ 0.4000000D+02, 0.2557000D+02/
            data x( 27),y( 27)/ 0.3000000D+02, 0.2327000D+02/
            data x( 28),y( 28)/ 0.2000000D+02, 0.1996000D+02/
            data x( 29),y( 29)/ 0.1500000D+02, 0.1767000D+02/
            data x( 30),y( 30)/ 0.1000000D+02, 0.1466000D+02/
            data x( 31),y( 31)/ 0.7500000D+01, 0.1276000D+02/
            data x( 32),y( 32)/ 0.5000000D+01, 0.1047000D+02/
            data x( 33),y( 33)/ 0.4000000D+01, 0.9380000D+01/
            data x( 34),y( 34)/ 0.3000000D+01, 0.8150000D+01/
            data x( 35),y( 35)/ 0.2500000D+01, 0.7490000D+01/
            data x( 36),y( 36)/ 0.2000000D+01, 0.6700000D+01/
            data x( 37),y( 37)/ 0.1500000D+01, 0.5850000D+01/
            data x( 38),y( 38)/ 0.1000000D+01, 0.4740000D+01/
            data x( 39),y( 39)/ 0.5000000D+00, 0.3400000D+01/
            data x( 40),y( 40)/ 0.2500000D+00, 0.2400000D+01/
            data x( 41),y( 41)/ 0.0000000D+00, 0.0000000D+00/
            data x( 42),y( 42)/ 0.2500000D+00,-0.3500000D+01/
            data x( 43),y( 43)/ 0.5000000D+00,-0.4850000D+01/
            data x( 44),y( 44)/ 0.1000000D+01,-0.6720000D+01/
            data x( 45),y( 45)/ 0.1500000D+01,-0.8070000D+01/
            data x( 46),y( 46)/ 0.2000000D+01,-0.9240000D+01/
            data x( 47),y( 47)/ 0.2500000D+01,-0.1023000D+02/
            data x( 48),y( 48)/ 0.3000000D+01,-0.1110000D+02/
            data x( 49),y( 49)/ 0.4000000D+01,-0.1261000D+02/
            data x( 50),y( 50)/ 0.5000000D+01,-0.1388000D+02/
            data x( 51),y( 51)/ 0.7500000D+01,-0.1662000D+02/
            data x( 52),y( 52)/ 0.1000000D+02,-0.1899000D+02/
            data x( 53),y( 53)/ 0.1500000D+02,-0.2296000D+02/
            data x( 54),y( 54)/ 0.2000000D+02,-0.2614000D+02/
            data x( 55),y( 55)/ 0.3000000D+02,-0.3079000D+02/
            data x( 56),y( 56)/ 0.4000000D+02,-0.3406000D+02/
            data x( 57),y( 57)/ 0.5000000D+02,-0.3654000D+02/
            data x( 58),y( 58)/ 0.6000000D+02,-0.3849000D+02/
            data x( 59),y( 59)/ 0.8000000D+02,-0.4142000D+02/
            data x( 60),y( 60)/ 0.1000000D+03,-0.4345000D+02/
            data x( 61),y( 61)/ 0.1200000D+03,-0.4484000D+02/
            data x( 62),y( 62)/ 0.1400000D+03,-0.4557000D+02/
            data x( 63),y( 63)/ 0.1600000D+03,-0.4582000D+02/
            data x( 64),y( 64)/ 0.1800000D+03,-0.4550000D+02/
            data x( 65),y( 65)/ 0.2000000D+03,-0.4467000D+02/
            data x( 66),y( 66)/ 0.2200000D+03,-0.4329000D+02/
            data x( 67),y( 67)/ 0.2400000D+03,-0.4130000D+02/
            data x( 68),y( 68)/ 0.2600000D+03,-0.3898000D+02/
            data x( 69),y( 69)/ 0.2800000D+03,-0.3609000D+02/
            data x( 70),y( 70)/ 0.3000000D+03,-0.3275000D+02/
            data x( 71),y( 71)/ 0.3200000D+03,-0.2905000D+02/
            data x( 72),y( 72)/ 0.3300000D+03,-0.2720000D+02/
            data x( 73),y( 73)/ 0.3350000D+03,-0.2560000D+02/
            data x( 74),y( 74)/ 0.3400000D+03,-0.2350000D+02/
            data x( 75),y( 75)/ 0.3450000D+03,-0.2070000D+02/
            data x( 76),y( 76)/ 0.3500000D+03,-0.1740000D+02/
            data x( 77),y( 77)/ 0.3550000D+03,-0.1400000D+02/
            data x( 78),y( 78)/ 0.3600000D+03,-0.1020000D+02/
            data x( 79),y( 79)/ 0.3650000D+03,-0.6680000D+01/
            data x( 80),y( 80)/ 0.3700000D+03,-0.3940000D+01/
            data x( 81),y( 81)/ 0.3800000D+03,-0.3000000D+00/
            data x( 82),y( 82)/ 0.3900000D+03, 0.1920000D+01/
            data x( 83),y( 83)/ 0.4000000D+03, 0.3220000D+01/
            data x( 84),y( 84)/ 0.4100000D+03, 0.3500000D+01/
            data x( 85),y( 85)/ 0.4200000D+03, 0.3200000D+01/
            data x( 86),y( 86)/ 0.4300000D+03, 0.2200000D+01/
            data x( 87),y( 87)/ 0.4400000D+03, 0.8000000D+00/
            data x( 88),y( 88)/ 0.4500000D+03,-0.1000000D+01/
            data x( 89),y( 89)/ 0.4600000D+03,-0.3200000D+01/
            data x( 90),y( 90)/ 0.4700000D+03,-0.5400000D+01/
            data x( 91),y( 91)/ 0.4800000D+03,-0.7630000D+01/
c
c       the flap
c
            data x( 92),y( 92)/ 0.5000000D+03,-0.1005000D+02/
            data x( 93),y( 93)/ 0.4900000D+03,-0.7920000D+01/
            data x( 94),y( 94)/ 0.4800000D+03,-0.5620000D+01/
            data x( 95),y( 95)/ 0.4700000D+03,-0.3380000D+01/
            data x( 96),y( 96)/ 0.4600000D+03,-0.1300000D+01/
            data x( 97),y( 97)/ 0.4500000D+03, 0.5200005D+00/
            data x( 98),y( 98)/ 0.4400000D+03, 0.2020000D+01/
            data x( 99),y( 99)/ 0.4300000D+03, 0.3060000D+01/
            data x(100),y(100)/ 0.4200000D+03, 0.3700000D+01/
            data x(101),y(101)/ 0.4100000D+03, 0.3780000D+01/
            data x(102),y(102)/ 0.4000000D+03, 0.3220000D+01/
            data x(103),y(103)/ 0.3900000D+03, 0.1920000D+01/
            data x(104),y(104)/ 0.3800000D+03,-0.3000002D+00/
            data x(105),y(105)/ 0.3700000D+03,-0.3940000D+01/
            data x(106),y(106)/ 0.3660000D+03,-0.6080000D+01/
            data x(107),y(107)/ 0.3630000D+03,-0.7940000D+01/
            data x(108),y(108)/ 0.3610000D+03,-0.9340000D+01/
            data x(109),y(109)/ 0.3590000D+03,-0.1098000D+02/
            data x(110),y(110)/ 0.3580000D+03,-0.1194000D+02/
            data x(111),y(111)/ 0.3570000D+03,-0.1304000D+02/
            data x(112),y(112)/ 0.3560000D+03,-0.1436000D+02/
            data x(113),y(113)/ 0.3555000D+03,-0.1524000D+02/
            data x(114),y(114)/ 0.3550000D+03,-0.1692000D+02/
            data x(115),y(115)/ 0.3555000D+03,-0.1884000D+02/
            data x(116),y(116)/ 0.3560000D+03,-0.1958000D+02/
            data x(117),y(117)/ 0.3570000D+03,-0.2024000D+02/
            data x(118),y(118)/ 0.3580000D+03,-0.2072000D+02/
            data x(119),y(119)/ 0.3590000D+03,-0.2076000D+02/
            data x(120),y(120)/ 0.3610000D+03,-0.2054000D+02/
            data x(121),y(121)/ 0.3630000D+03,-0.2026000D+02/
            data x(122),y(122)/ 0.3660000D+03,-0.1974000D+02/
            data x(123),y(123)/ 0.3700000D+03,-0.1903000D+02/
            data x(124),y(124)/ 0.3800000D+03,-0.1717000D+02/
            data x(125),y(125)/ 0.3900000D+03,-0.1532000D+02/
            data x(126),y(126)/ 0.4000000D+03,-0.1361000D+02/
            data x(127),y(127)/ 0.4100000D+03,-0.1199000D+02/
            data x(128),y(128)/ 0.4200000D+03,-0.1068000D+02/
            data x(129),y(129)/ 0.4300000D+03,-0.9670000D+01/
            data x(130),y(130)/ 0.4400000D+03,-0.9010000D+01/
            data x(131),y(131)/ 0.4500000D+03,-0.8710000D+01/
            data x(132),y(132)/ 0.4600000D+03,-0.8800000D+01/
            data x(133),y(133)/ 0.4700000D+03,-0.9260000D+01/
            data x(134),y(134)/ 0.4800000D+03,-0.1011000D+02/
            data x(135),y(135)/ 0.4900000D+03,-0.1121000D+02/
            data x(136),y(136)/ 0.5000000D+03,-0.1254000D+02/
c
c       the slat
c
            data x(137),y(137)/ 0.1200000D+03, 0.3330000D+02/
            data x(138),y(138)/ 0.1150000D+03, 0.3330000D+02/
            data x(139),y(139)/ 0.1000000D+03, 0.3227500D+02/
            data x(140),y(140)/ 0.8000000D+02, 0.3077000D+02/
            data x(141),y(141)/ 0.6000000D+02, 0.2868000D+02/
            data x(142),y(142)/ 0.4000000D+02, 0.2557500D+02/
            data x(143),y(143)/ 0.3000000D+02, 0.2327000D+02/
            data x(144),y(144)/ 0.2000000D+02, 0.1996000D+02/
            data x(145),y(145)/ 0.1000000D+02, 0.1466500D+02/
            data x(146),y(146)/ 0.5000000D+01, 0.1047000D+02/
            data x(147),y(147)/ 0.1500000D+01, 0.5850000D+01/
            data x(148),y(148)/ 0.0000000D+00, 0.0000000D+00/
            data x(149),y(149)/ 0.1500000D+01,-0.8075000D+01/
            data x(150),y(150)/ 0.5000000D+01,-0.1388000D+02/
            data x(151),y(151)/ 0.1000000D+02,-0.1899000D+02/
            data x(152),y(152)/ 0.1500000D+02,-0.2296000D+02/
            data x(153),y(153)/ 0.2000000D+02,-0.2614500D+02/
            data x(154),y(154)/ 0.1800000D+02,-0.2200000D+02/
            data x(155),y(155)/ 0.1760000D+02,-0.1900000D+02/
            data x(156),y(156)/ 0.1750000D+02,-0.1500000D+02/
            data x(157),y(157)/ 0.1850000D+02,-0.1000000D+02/
            data x(158),y(158)/ 0.2350000D+02, 0.0000000D+00/
            data x(159),y(159)/ 0.3000000D+02, 0.7000000D+01/
            data x(160),y(160)/ 0.4000000D+02, 0.1350000D+02/
            data x(161),y(161)/ 0.6000000D+02, 0.2200000D+02/
            data x(162),y(162)/ 0.8000000D+02, 0.2750000D+02/
            data x(163),y(163)/ 0.1000000D+03, 0.3050000D+02/
            data x(164),y(164)/ 0.1150000D+03, 0.3150000D+02/
            data x(165),y(165)/ 0.1200000D+03, 0.3180000D+02/
c
        sp(2)='wing'
        sp(1)='wing'
        sp(3)='wing'
        sp(4)='wing'
c
        pi=3.141592653589793d0
c
        ip(7)=51
        size=ru(10)
        nw=9
        nf=100
        ns=145
        do i=1,8
            arg=pi*dfloat(i-1)/4.0d0
            vx(i)=size*dcos(arg)
            vy(i)=size*dsin(arg)
        enddo
        xmin=x(1)
        xmax=x(1)
        ymin=y(1)
        ymax=y(1)
        do i=1,nvf-8
           xmax=dmax1(xmax,x(i))
           xmin=dmin1(xmin,x(i))
           ymax=dmax1(ymax,y(i))
           ymin=dmin1(ymin,y(i))
        enddo
        xx=(xmin+xmax)/2.0d0
        yy=(ymin+ymax)/2.0d0
        ss=dmax1(xmax-xmin,ymax-ymin)
        do i=9,nvf
            vx(i)=(x(i-8)-xx)/ss
            vy(i)=(y(i-8)-yy)/ss
        enddo
        t=1.0d0
        thetaf=-t*pi*2.0d0/9.0d0
        cf=dcos(thetaf)
        sf=dsin(thetaf)
        xf=2.1d-1*t
        yf=-1.0d-1*t
        xmin=vx(nf)
        xmax=vx(nf)
        ymin=vy(nf)
        ymax=vy(nf)
        do i=nf,ns-1
           xmax=dmax1(xmax,vx(i))
           xmin=dmin1(xmin,vx(i))
           ymax=dmax1(ymax,vy(i))
           ymin=dmin1(ymin,vy(i))
        enddo
        xc=(xmin+xmax)/2.0d0
        yc=(ymin+ymax)/2.0d0
        do i=nf,ns-1
            xx=vx(i)-xc
            yy=vy(i)-yc
            vx(i)=xx*cf-yy*sf+xc+xf
            vy(i)=xx*sf+yy*cf+yc+yf
        enddo
c
        thetas=t*pi/6.0d0
        cs=dcos(thetas)
        ss=dsin(thetas)
        xs=-1.9d-1*t
        ys=-4.0d-2*t
        xmin=vx(ns)
        xmax=vx(ns)
        ymin=vy(ns)
        ymax=vy(ns)
        do i=ns,nvf
           xmax=dmax1(xmax,vx(i))
           xmin=dmin1(xmin,vx(i))
           ymax=dmax1(ymax,vy(i))
           ymin=dmin1(ymin,vy(i))
        enddo
        xc=(xmin+xmax)/2.0d0
        yc=(ymin+ymax)/2.0d0
        do i=ns,nvf
            xx=vx(i)-xc
            yy=vy(i)-yc
            vx(i)=xx*cs-yy*ss+xc+xs
            vy(i)=xx*ss+yy*cs+yc+ys
        enddo
c
        do i=1,nbf
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            if(i.le.nvf) then
                ibndry(4,i)=1
            else
                ibndry(4,i)=0
            endif
            ibndry(5,i)=0
            if(i.le.8) then
                ibndry(6,i)=i
            else
                ibndry(6,i)=-1
            endif
        enddo
        ibndry(2,nw-1)=1
        ibndry(2,nf-1)=nw
        ibndry(2,ns-1)=nf
        ibndry(2,nvf)=ns
c
        ibndry(1,nvf+1)=5
        ibndry(2,nvf+1)=155
        ibndry(1,nvf+2)=173
        ibndry(2,nvf+2)=37
        ibndry(1,nvf+3)=99
        ibndry(2,nvf+3)=116
        ibndry(1,nvf+4)=100
        ibndry(2,nvf+4)=1
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
c
c       make itnode
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
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
        subroutine gd6(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          xx(34),yy(34)
            character*80
     +          sp(100),su(100)
            save ntf,nvf,ncf,nbf,hmax,grade
c
            data ntf,nvf,ncf,nbf/8,40,32,48/
            data hmax,grade/0.1d0,1.75d0/
c
        sp(2)='ellipse'
        sp(1)='ellipse'
        sp(3)='ellipse'
        sp(4)='ellipse'
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        ip(7)=1
        rp(1)=ru(1)
        rp(15)=hmax
        rp(16)=grade
        pi=3.141592653589793d0
        pi16=pi/16.0d0
c
        size=ru(10)
        ratio=ru(9)
        vx(1)=-0.5d0
        vy(1)=0.0d0
        vx(2)=0.5d0
        vy(2)=0.0d0
c
        scale=2.0d0
        vx(3)=scale*size
        vy(3)=0.0d0
        vx(4)=scale*size
        vy(4)=size
        vx(5)=0.0d0
        vy(5)=size
        vx(6)=-size
        vy(6)=size
        vx(7)=-size
        vy(7)=0.0d0
        vx(8)=-size
        vy(8)=-size
        vx(9)=0.0d0
        vy(9)=-size
        vx(10)=scale*size
        vy(10)=-size
c
        do i=1,15
            vx(10+i)=dcos(pi-i*pi16)/2.0d0
            vy(10+i)=dsin(pi-i*pi16)/(2.0d0*ratio)
            vx(25+i)=vx(10+i)
            vy(25+i)=-vy(10+i)
        enddo
        do i=1,16
            xx(i)=dcos(pi-(2*i-1)*pi16/2.0d0)/2.0d0
            yy(i)=dsin(pi-(2*i-1)*pi16/2.0d0)/(2.0d0*ratio)
            xx(16+i)=xx(i)
            yy(16+i)=-yy(i)
        enddo
c
        ibndry(1,1)=7
        ibndry(2,1)=1
        ibndry(3,1)=0
        ibndry(4,1)=0
        ibndry(5,1)=0
        ibndry(6,1)=0
        ibndry(1,2)=2
        ibndry(2,2)=3
        ibndry(3,2)=0
        ibndry(4,2)=0
        ibndry(5,2)=0
        ibndry(6,2)=0
        do i=3,10
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            ibndry(4,i)=1
            ibndry(5,i)=0
            ibndry(6,i)=i
        enddo
        ibndry(6,3)=1
        ibndry(6,6)=3
        ibndry(6,7)=3
        ibndry(6,10)=1
        ibndry(6,4)=2
        ibndry(6,5)=2
        ibndry(6,8)=4
        ibndry(6,9)=4
        ibndry(2,10)=3
        do i=11,26
            ibndry(1,i)=i-1
            ibndry(2,i)=i
            ibndry(3,i)=i-10
            ibndry(4,i)=1
            ibndry(5,i)=0
            ibndry(6,i)=-1
        enddo
        ibndry(1,11)=1
        ibndry(2,26)=2
        do i=27,42
            ibndry(1,i)=i-2
            ibndry(2,i)=i-1
            ibndry(3,i)=i-10
            ibndry(4,i)=1
            ibndry(5,i)=0
            ibndry(6,i)=-2
        enddo
        ibndry(1,27)=1
        ibndry(2,42)=2
        do i=43,48
            ibndry(3,i)=0
            ibndry(4,i)=0
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
        ibndry(1,43)=5
        ibndry(2,43)=18
        ibndry(1,44)=9
        ibndry(2,44)=33
        ibndry(1,45)=4
        ibndry(2,45)=22
        ibndry(1,46)=6
        ibndry(2,46)=14
        ibndry(1,47)=8
        ibndry(2,47)=29
        ibndry(1,48)=10
        ibndry(2,48)=37
c
        do i=1,nbf
            if(ibndry(3,i).gt.0) then
                i1=ibndry(1,i)
                i2=ibndry(2,i)
                i3=ibndry(3,i)
                call centre(vx(i1),vy(i1),vx(i2),vy(i2),
     +              xx(i3),yy(i3),xm(i3),ym(i3))
            endif
        enddo
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
c
c       make itnode, find symmetries
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        call sklutl(2,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gd7(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(165),y(165)
            character*80
     +          sp(100),su(100)
            save ntf,nvf,ncf,nbf,hmax,grade,x,y
c
            data ntf,nvf,ncf,nbf/2,157,0,161/
            data hmax,grade/0.1d0,1.75d0/
c
c       wing
c
            data x(  1),y(  1)/ 0.480000000D+03,-0.662999821D+01/
            data x(  2),y(  2)/ 0.473457031D+03,-0.526247884D+01/
            data x(  3),y(  3)/ 0.466914307D+03,-0.369342580D+01/
            data x(  4),y(  4)/ 0.460371582D+03,-0.253030702D+01/
            data x(  5),y(  5)/ 0.453828857D+03,-0.137639498D+01/
            data x(  6),y(  6)/ 0.447286133D+03,-0.148075148D+00/
            data x(  7),y(  7)/ 0.434102050D+03, 0.253743934D+01/
            data x(  8),y(  8)/ 0.420792480D+03, 0.477401899D+01/
            data x(  9),y(  9)/ 0.407403809D+03, 0.512527423D+01/
            data x( 10),y( 10)/ 0.394037842D+03, 0.273533501D+01/
            data x( 11),y( 11)/ 0.380000000D+03,-0.231984292D+01/
            data x( 12),y( 12)/ 0.370000006D+03,-0.684822106D+01/
            data x( 13),y( 13)/ 0.365000000D+03,-0.923239198D+01/
            data x( 14),y( 14)/ 0.360000000D+03,-0.116313226D+02/
            data x( 15),y( 15)/ 0.355000000D+03,-0.140056652D+02/
            data x( 16),y( 16)/ 0.350000000D+03,-0.163237781D+02/
            data x( 17),y( 17)/ 0.345000000D+03,-0.185612068D+02/
            data x( 18),y( 18)/ 0.340000000D+03,-0.206999190D+02/
            data x( 19),y( 19)/ 0.335000000D+03,-0.227274405D+02/
            data x( 20),y( 20)/ 0.330000000D+03,-0.246359824D+02/
            data x( 21),y( 21)/ 0.320000000D+03,-0.280835203D+02/
            data x( 22),y( 22)/ 0.300000000D+03,-0.335516603D+02/
            data x( 23),y( 23)/ 0.280000000D+03,-0.374062618D+02/
            data x( 24),y( 24)/ 0.260000000D+03,-0.400782232D+02/
            data x( 25),y( 25)/ 0.240000000D+03,-0.419478615D+02/
            data x( 26),y( 26)/ 0.220000000D+03,-0.432872785D+02/
            data x( 27),y( 27)/ 0.200000000D+03,-0.442582638D+02/
            data x( 28),y( 28)/ 0.180000000D+03,-0.449301318D+02/
            data x( 29),y( 29)/ 0.160000000D+03,-0.453003997D+02/
            data x( 30),y( 30)/ 0.140000000D+03,-0.453109687D+02/
            data x( 31),y( 31)/ 0.120000000D+03,-0.448567226D+02/
            data x( 32),y( 32)/ 0.100000000D+03,-0.437840957D+02/
            data x( 33),y( 33)/ 0.800000000D+02,-0.418741005D+02/
            data x( 34),y( 34)/ 0.600000000D+02,-0.387928709D+02/
            data x( 35),y( 35)/ 0.500000000D+02,-0.366466484D+02/
            data x( 36),y( 36)/ 0.400000000D+02,-0.339474624D+02/
            data x( 37),y( 37)/ 0.300000000D+02,-0.304961646D+02/
            data x( 38),y( 38)/ 0.200000000D+02,-0.259126583D+02/
            data x( 39),y( 39)/ 0.150000000D+02,-0.229490684D+02/
            data x( 40),y( 40)/ 0.999999998D+01,-0.192272206D+02/
            data x( 41),y( 41)/ 0.500000000D+01,-0.140600086D+02/
            data x( 42),y( 42)/ 0.150000000D+01,-0.801237298D+01/
            data x( 43),y( 43)/ 0.000000000D+00, 0.000000000D+00/
            data x( 44),y( 44)/ 0.150000004D+01, 0.574996748D+01/
            data x( 45),y( 45)/ 0.500014228D+01, 0.106238639D+02/
            data x( 46),y( 46)/ 0.999999997D+01, 0.147985701D+02/
            data x( 47),y( 47)/ 0.199999995D+02, 0.199110020D+02/
            data x( 48),y( 48)/ 0.300298917D+02, 0.231743258D+02/
            data x( 49),y( 49)/ 0.400000000D+02, 0.255091817D+02/
            data x( 50),y( 50)/ 0.500000737D+02, 0.273102412D+02/
            data x( 51),y( 51)/ 0.600000175D+02, 0.287496875D+02/
            data x( 52),y( 52)/ 0.800000219D+02, 0.308990033D+02/
            data x( 53),y( 53)/ 0.999999760D+02, 0.323684836D+02/
            data x( 54),y( 54)/ 0.119999727D+03, 0.333339249D+02/
            data x( 55),y( 55)/ 0.140000000D+03, 0.338853699D+02/
            data x( 56),y( 56)/ 0.159999999D+03, 0.340754563D+02/
            data x( 57),y( 57)/ 0.180000000D+03, 0.339379886D+02/
            data x( 58),y( 58)/ 0.200003656D+03, 0.334954849D+02/
            data x( 59),y( 59)/ 0.220000000D+03, 0.327628738D+02/
            data x( 60),y( 60)/ 0.240000206D+03, 0.317472882D+02/
            data x( 61),y( 61)/ 0.260000000D+03, 0.304487961D+02/
            data x( 62),y( 62)/ 0.280001707D+03, 0.288585072D+02/
            data x( 63),y( 63)/ 0.300000000D+03, 0.269585083D+02/
            data x( 64),y( 64)/ 0.320000000D+03, 0.247205067D+02/
            data x( 65),y( 65)/ 0.339999997D+03, 0.221103649D+02/
            data x( 66),y( 66)/ 0.355521039D+03, 0.198066890D+02/
            data x( 67),y( 67)/ 0.368721859D+03, 0.176465799D+02/
            data x( 68),y( 68)/ 0.381945070D+03, 0.152952884D+02/
            data x( 69),y( 69)/ 0.395206187D+03, 0.127527302D+02/
            data x( 70),y( 70)/ 0.408566650D+03, 0.100156047D+02/
            data x( 71),y( 71)/ 0.421462971D+03, 0.722427238D+01/
            data x( 72),y( 72)/ 0.434750707D+03, 0.421943290D+01/
            data x( 73),y( 73)/ 0.447547374D+03, 0.122888481D+01/
            data x( 74),y( 74)/ 0.453996094D+03,-0.305867383D+00/
            data x( 75),y( 75)/ 0.460470678D+03,-0.186165982D+01/
            data x( 76),y( 76)/ 0.466926997D+03,-0.342588974D+01/
            data x( 77),y( 77)/ 0.473471924D+03,-0.502367471D+01/
c
c       flap
c
            data x( 78),y( 78)/ 0.590689574D+03,-0.875101882D+02/
            data x( 79),y( 79)/ 0.582315185D+03,-0.815126621D+02/
            data x( 80),y( 80)/ 0.574327351D+03,-0.758292226D+02/
            data x( 81),y( 81)/ 0.566176746D+03,-0.702812757D+02/
            data x( 82),y( 82)/ 0.557582866D+03,-0.648267025D+02/
            data x( 83),y( 83)/ 0.548944062D+03,-0.597900202D+02/
            data x( 84),y( 84)/ 0.540115733D+03,-0.550830383D+02/
            data x( 85),y( 85)/ 0.531107846D+03,-0.506902836D+02/
            data x( 86),y( 86)/ 0.521922105D+03,-0.465901257D+02/
            data x( 87),y( 87)/ 0.512594386D+03,-0.427760313D+02/
            data x( 88),y( 88)/ 0.503118172D+03,-0.392013153D+02/
            data x( 89),y( 89)/ 0.493659507D+03,-0.358307699D+02/
            data x( 90),y( 90)/ 0.483932097D+03,-0.324051548D+02/
            data x( 91),y( 91)/ 0.474415369D+03,-0.289558527D+02/
            data x( 92),y( 92)/ 0.470587155D+03,-0.275931441D+02/
            data x( 93),y( 93)/ 0.467687372D+03,-0.266258565D+02/
            data x( 94),y( 94)/ 0.465755815D+03,-0.259828146D+02/
            data x( 95),y( 95)/ 0.463946586D+03,-0.251814414D+02/
            data x( 96),y( 96)/ 0.463127460D+03,-0.245277570D+02/
            data x( 97),y( 97)/ 0.462527732D+03,-0.235576829D+02/
            data x( 98),y( 98)/ 0.462241255D+03,-0.220639840D+02/
            data x( 99),y( 99)/ 0.462259720D+03,-0.209738771D+02/
            data x(100),y(100)/ 0.462404022D+03,-0.197493515D+02/
            data x(101),y(101)/ 0.463509456D+03,-0.188472124D+02/
            data x(102),y(102)/ 0.464445848D+03,-0.182076208D+02/
            data x(103),y(103)/ 0.466066020D+03,-0.173750427D+02/
            data x(104),y(104)/ 0.467495819D+03,-0.168870852D+02/
            data x(105),y(105)/ 0.468828392D+03,-0.165731108D+02/
            data x(106),y(106)/ 0.471353890D+03,-0.161757596D+02/
            data x(107),y(107)/ 0.473804576D+03,-0.159301379D+02/
            data x(108),y(108)/ 0.477347391D+03,-0.157771184D+02/
            data x(109),y(109)/ 0.481865034D+03,-0.159367047D+02/
            data x(110),y(110)/ 0.492367496D+03,-0.177136125D+02/
            data x(111),y(111)/ 0.502139335D+03,-0.207716416D+02/
            data x(112),y(112)/ 0.511405386D+03,-0.246801438D+02/
            data x(113),y(113)/ 0.520297957D+03,-0.293047743D+02/
            data x(114),y(114)/ 0.528883448D+03,-0.345920556D+02/
            data x(115),y(115)/ 0.537051283D+03,-0.403550440D+02/
            data x(116),y(116)/ 0.544986150D+03,-0.465678311D+02/
            data x(117),y(117)/ 0.552754066D+03,-0.531302305D+02/
            data x(118),y(118)/ 0.560403464D+03,-0.599345639D+02/
            data x(119),y(119)/ 0.567949695D+03,-0.668428740D+02/
            data x(120),y(120)/ 0.575494475D+03,-0.737960992D+02/
            data x(121),y(121)/ 0.583068359D+03,-0.806951931D+02/
c
c       slat
c
            data x(122),y(122)/-0.139921304D+02, 0.107932521D+02/
            data x(123),y(123)/-0.180998011D+02, 0.771087942D+01/
            data x(124),y(124)/-0.299042778D+02,-0.517306877D+00/
            data x(125),y(125)/-0.464852558D+02,-0.132745842D+02/
            data x(126),y(126)/-0.614891104D+02,-0.275727459D+02/
            data x(127),y(127)/-0.750712754D+02,-0.445354315D+02/
            data x(128),y(128)/-0.809470086D+02,-0.548201807D+02/
            data x(129),y(129)/-0.832101179D+02,-0.642385568D+02/
            data x(130),y(130)/-0.828836892D+02,-0.754105177D+02/
            data x(131),y(131)/-0.813954050D+02,-0.802887906D+02/
            data x(132),y(132)/-0.785738188D+02,-0.842687651D+02/
            data x(133),y(133)/-0.753610927D+02,-0.875867608D+02/
            data x(134),y(134)/-0.739154358D+02,-0.889208603D+02/
            data x(135),y(135)/-0.759155235D+02,-0.889208538D+02/
            data x(136),y(136)/-0.860089871D+02,-0.873667527D+02/
            data x(137),y(137)/-0.930244828D+02,-0.852155089D+02/
            data x(138),y(138)/-0.987726818D+02,-0.817898608D+02/
            data x(139),y(139)/-0.103911240D+03,-0.753624048D+02/
            data x(140),y(140)/-0.105299163D+03,-0.695069901D+02/
            data x(141),y(141)/-0.104584014D+03,-0.637671461D+02/
            data x(142),y(142)/-0.102145402D+03,-0.577060883D+02/
            data x(143),y(143)/-0.956726529D+02,-0.482979870D+02/
            data x(144),y(144)/-0.884443568D+02,-0.406693640D+02/
            data x(145),y(145)/-0.807948934D+02,-0.338967442D+02/
            data x(146),y(146)/-0.647104526D+02,-0.216852309D+02/
            data x(147),y(147)/-0.480126378D+02,-0.103790270D+02/
            data x(148),y(148)/-0.311015540D+02, 0.442777950D+00/
            data x(149),y(149)/-0.180768897D+02, 0.841042574D+01/
c
        sp(2)='wing'
        sp(1)='wing'
        sp(3)='wing'
        sp(4)='wing'
c
        pi=3.141592653589793d0
c
        ip(7)=36
        size=ru(10)
        nw=9
        nf=86
        ns=130
        do i=1,8
            arg=pi*dfloat(i-1)/4.0d0
            vx(i)=size*dcos(arg)
            vy(i)=size*dsin(arg)
        enddo
        xmin=x(1)
        xmax=x(1)
        ymin=y(1)
        ymax=y(1)
        do i=1,nvf-8
           xmax=dmax1(xmax,x(i))
           xmin=dmin1(xmin,x(i))
           ymax=dmax1(ymax,y(i))
           ymin=dmin1(ymin,y(i))
        enddo
        xx=(xmin+xmax)/2.0d0
        yy=(ymin+ymax)/2.0d0
        ss=dmax1(xmax-xmin,ymax-ymin)
        do i=9,nvf
            vx(i)=(x(i-8)-xx)/ss
            vy(i)=(y(i-8)-yy)/ss
        enddo
c
        do i=1,nbf
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            if(i.le.nvf) then
                ibndry(4,i)=1
            else
                ibndry(4,i)=0
            endif
            ibndry(5,i)=0
            if(i.le.8) then
                ibndry(6,i)=i
            else
                ibndry(6,i)=-1
            endif
        enddo
        ibndry(2,nw-1)=1
        ibndry(2,nf-1)=nw
        ibndry(2,ns-1)=nf
        ibndry(2,nvf)=ns
        ibndry(1,nvf+1)=5
        ibndry(2,nvf+1)=147
        ibndry(1,nvf+2)=130
        ibndry(2,nvf+2)=53
        ibndry(1,nvf+3)=9
        ibndry(2,nvf+3)=117
        ibndry(1,nvf+4)=86
        ibndry(2,nvf+4)=1
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
c
c       make itnode
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
c
        return
        end
