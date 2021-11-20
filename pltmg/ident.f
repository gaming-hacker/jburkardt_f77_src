c***********************  problem name: ident  *************************
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
            common /atest2/irl,iu(99),a,c0,c1,c2,d,ru(95),su(100)
c
        call setrl(rl)
        values(k0)=(a**2+1.0d0)*ux
        values(kx)=a**2+1.0d0
        if(irl.eq.1) then
            values(kl)=ux*a*2.0d0
            values(klx)=a*2.0d0
            values(kll)=ux*2.0d0
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
            common /atest2/irl,iu(99),a,c0,c1,c2,d,ru(95),su(100)
c
        call setrl(rl)
        values(k0)=(a**2+1.0d0)*uy
        values(ky)=a**2+1.0d0
        if(irl.eq.1) then
            values(kl)=uy*a*2.0d0
            values(kly)=a*2.0d0
            values(kll)=uy*2.0d0
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
            common /atest2/irl,iu(99),a,c0,c1,c2,d,ru(95),su(100)
c
        call setrl(rl)
        values(k0)=c2*u**2+c1*u-c0
        values(ku)=c2*u*2.0d0+c1
        values(kuu)=c2*2.0d0
        if(irl.eq.2) then
            values(kl)=-1.0d0
        elseif(irl.eq.3) then
            values(kl)=u
            values(kul)=1.0d0
        elseif(irl.eq.4) then
            values(kl)=u**2
            values(kul)=2.0d0*u
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
            common /atest2/irl,iu(99),a,c0,c1,c2,d,ru(95),su(100)
c
        call setrl(rl)
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
            common /atest2/irl,iu(99),a,c0,c1,c2,d,ru(95),su(100)
c
        call setrl(rl)
        if(itag.eq.2) then
            values(k0)=d
            if(irl.eq.5) values(kl)=1.0d0
        endif
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
        uu=1.0d0
        e=dexp(-20.0d0*(x**2+y**2))
        values(k0)=e*(u-uu)**2
        values(ku)=e*2.0d0*(u-uu)
        values(kuu)=e*2.0d0
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
        subroutine setrl(rl)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            character*80
     +          su
            common /atest2/irl,iu(99),ru(100),su(100)
c
        if(irl.gt.0) ru(irl)=rl
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
            data len/11/
            data (file(i),i=  1, 10)/
     +      'n i= 1,n=irl,   a= i,t=i',
     1      'n i= 1,n=a,     a= a,t=r',
     2      'n i= 2,n=c0,    a=c0,t=r',
     3      'n i= 3,n=c1,    a=c1,t=r',
     4      'n i= 4,n=c2,    a=c2,t=r',
     5      'n i= 5,n=d,     a= d,t=r',
     6      's n=irl   ,v= 1,l="a"',
     7      's n=irl   ,v= 2,l="c0"',
     8      's n=irl   ,v= 3,l="c1"',
     9      's n=irl   ,v= 4,l="c2"'/
            data (file(i),i= 11, 11)/
     +      's n=irl   ,v= 5,l="d"'/
c
c
c       enter input mode
c
        irlsv=iu(1)
        call usrset(file,len,iu,ru,su)
c
        if(irlsv.ne.iu(1)) then
            ip(9)=8
            rp(1)=ru(iu(1))
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
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(13),y(13)
            character*80
     +          sp(100),su(100)
            save hmax,grade,ntf,nvf,ncf,nbf,x,y,iprob,ispd,iadapt
            data x/1.0d0,3.0d0,3.0d0,1.0d0,1.0d0,-1.0d0,
     +          -1.0d0,-3.0d0,-3.0d0,-1.0d0,-1.0d0,1.0d0,0.0d0/
            data y/-1.0d0,-1.0d0,1.0d0,1.0d0,3.0d0,3.0d0,
     +          1.0d0,1.0d0,-1.0d0,-1.0d0,-3.0d0,-3.0d0,0.0d0/
            data nvf,ntf,ncf,nbf/13,4,0,16/
            data iprob,ispd,iadapt/4,1,5/
            data hmax,grade/0.1d0,1.5d0/
c
        if(ip(41).eq.1) then
            sp(2)='ident'
            sp(1)='ident'
            sp(3)='ident'
            sp(4)='ident'
            sp(6)='ident_mpixxx.rw'
            sp(7)='ident.jnl'
            sp(9)='ident_mpixxx.out'
            ip(9)=8
            iu(1)=1
            ru(1)=1.0d0
            ru(2)=1.0d0
            ru(3)=0.0d0
            ru(4)=1.0d0
            ru(5)=0.0d0
            rp(1)=ru(iu(1))
        endif
c
c
        do i=1,nvf
            vx(i)=x(i)
            vy(i)=y(i)
        enddo
        do i=1,nbf
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            if(i.ge.nvf) then
                ibndry(1,i)=(i-nvf)*3+1
                ibndry(2,i)=nvf
            endif
            ibndry(3,i)=0
            ibndry(4,i)=1
            if(i.ge.nvf) ibndry(4,i)=0
            ibndry(5,i)=0
            ibndry(6,i)=1
            ii=i-(i/3)*3
            if(ii.eq.2.and.i.le.nvf) then
                ibndry(4,i)=2
                ibndry(6,i)=i
            endif
        enddo
        ibndry(2,nvf-1)=1
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        ip(6)=iprob
        ip(8)=ispd
        ip(26)=iadapt
        rp(15)=hmax
        rp(16)=grade
c
c       make itnode, find symmetries
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        call sklutl(2,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        return
        end
