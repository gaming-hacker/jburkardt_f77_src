c***********************  problem name: burger  ************************
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
            common /atest2/iu(100),eps,ru(99),su(100)
c
        values(k0)=eps*ux
        values(kx)=eps
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
            common /atest2/iu(100),eps,ru(99),su(100)
c
c
        values(k0)=eps*uy
        values(ky)=eps
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
            character*80
     +          su
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
c
c
        values(k0)=uy+u*ux
        values(ku)=ux
        values(kx)=u
        values(ky)=1.0d0
        values(kux)=1.0d0
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
            common /val1/k0,ku,kl,kuu,kul,klu,kll
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
            common /val2/k0,kl,kll,klb,kub,kic
c
        if(x.le.0.25d0) then
            values(k0)=1.0d0
        else if(x.ge.0.75d0) then
            values(k0)=0.0d0
        else
            values(k0)=1.5d0-2.0d0*x
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
            common /val3/kf,kf1,kf2,ksk,kad
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
     +          sp,su,file(10)
            common /atest1/ip(100),rp(100),sp(100)
            common /atest2/iu(100),ru(100),su(100)
            save len,file
c
            data len/1/
            data file(1)/'n i= 1,n=eps,   a=e ,t=r'/
c
        call usrset(file,len,iu,ru,su)
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
        subroutine gdata(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100),list(25)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*)
            character*80
     +          sp(100),su(100)
            save hmax,grade
            data hmax,grade/0.1d0,1.5d0/
c
        if(ip(41).eq.1) then
            sp(2)='burger'
            sp(1)='burger'
            sp(3)='burger'
            sp(4)='burger'
            sp(6)='burger_mpixxx.rw'
            sp(7)='burger.jnl'
            sp(9)='burger_mpixxx.out'
c
c       initialize
c
        endif
c
        vx(1)=0.0d0
        vy(1)=0.0d0
        vx(2)=2.0d0
        vy(2)=0.0d0
        vx(3)=0.0d0
        vy(3)=2.0d0
c
        xm(1)=0.0d0
        ym(1)=0.0d0
c
        do i=1,3
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            ibndry(4,i)=2
            ibndry(5,i)=0
            ibndry(6,i)=i
        enddo
        ibndry(2,3)=1
        ibndry(3,2)=1
        ibndry(4,2)=1
c
        itnode(1,1)=1
        itnode(2,1)=1
        itnode(3,1)=0
        itnode(4,1)=0
        itnode(5,1)=1
c
        nvf=3
        ntf=1
        ncf=1
        nbf=3
c
        ru(1)=1.0d-3
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
        ip(26)=5
c
c       refine long curved edges
c
        call sklutl(1,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        return
        end
