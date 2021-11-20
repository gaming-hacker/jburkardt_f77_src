c***********************  problem name: square  ************************
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
            common /atest2/ibc(4),icont,iu(95),a1x,a1y,a1u,a2x,a2y,a2u,
     +          bux,buy,cu0,cu1,cu2,cir,zexp,eps,zsin,du0,du1,eu0,f0,
     1          ru(81),su(100)
c
        call setrl(rl)
        values(k0)=a1x*ux+a1y*uy+a1u*u
        values(ku)=a1u
        values(kx)=a1x
        values(ky)=a1y
        if(icont.eq.1) values(kl)=ux
        if(icont.eq.2) values(kl)=uy
        if(icont.eq.3) values(kl)=u
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
            common /atest2/ibc(4),icont,iu(95),a1x,a1y,a1u,a2x,a2y,a2u,
     +          bux,buy,cu0,cu1,cu2,cir,zexp,eps,zsin,du0,du1,eu0,f0,
     1          ru(81),su(100)
c
        call setrl(rl)
        values(k0)=a2x*ux+a2y*uy+a2u*u
        values(ku)=a2u
        values(kx)=a2x
        values(ky)=a2y
        if(icont.eq.4) values(kl)=ux
        if(icont.eq.5) values(kl)=uy
        if(icont.eq.6) values(kl)=u
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
            common /atest2/ibc(4),icont,iu(95),a1x,a1y,a1u,a2x,a2y,a2u,
     +          bux,buy,cu0,cu1,cu2,cir,zexp,eps,zsin,du0,du1,eu0,f0,
     1          ru(81),su(100)
c
        call setrl(rl)
        ff = - bux*ux - buy*uy - cu0 - cu1*u - cu2*u*u -f0*(y-x)
     +      - cir*(ux*(y-0.5d0)-uy*(x-0.5d0))
        if(zexp.ne.0.0d0) ff = ff - zexp*dexp(u/(1.0d0+eps*u))
        if(zsin.ne.0.0d0) ff = ff - zsin*dsin(u)
        values(k0)=ff
c
        ff = - cu1 - cu2*u*2.0d0
        if(zexp.ne.0.0d0)
     +      ff = ff - zexp*dexp(u/(1.0d0+eps*u))/(1.0d0+eps*u)**2
        if(zsin.ne.0.0d0) ff = ff - zsin*dcos(u)
        values(ku)=ff
c
        values(kx) = - bux - cir*(y-0.5d0)
        values(ky) = - buy + cir*(x-0.5d0)
c
        if(icont.eq.7) values(kl) = - ux
        if(icont.eq.8) values(kl) = - uy
        if(icont.eq.9) values(kl) = - 1.0d0
        if(icont.eq.10) values(kl) = - u
        if(icont.eq.11) values(kl) = - u*u
        if(icont.eq.12) values(kl) = - (ux*(y-0.5d0)-uy*(x-0.5d0))
        if(icont.eq.13) values(kl) = - dexp(u/(1.0d0+eps*u))
        if(icont.eq.14) values(kl) =
     +      zexp*dexp(u/(1.0d0+eps*u))*((u/(1.0d0+eps*u))**2)
        if(icont.eq.15) values(kl) = - dsin(u)
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
            common /atest2/ibc(4),icont,iu(95),a1x,a1y,a1u,a2x,a2y,a2u,
     +          bux,buy,cu0,cu1,cu2,cir,zexp,eps,zsin,du0,du1,eu0,f0,
     1          ru(81),su(100)
c
        call setrl(rl)
        values(k0)= - du0 - du1*u
        values(ku)= - du1
        if(icont.eq.16) values(kl)=-1.0d0
        if(icont.eq.17) values(kl)=-u
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
            common /atest2/ibc(4),icont,iu(95),a1x,a1y,a1u,a2x,a2y,a2u,
     +          bux,buy,cu0,cu1,cu2,cir,zexp,eps,zsin,du0,du1,eu0,f0,
     1          ru(81),su(100)
c
        call setrl(rl)
        values(k0)=-eu0
        if(icont.eq.18) values(kl)=-1.0d0
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
        r=(x-0.5d0)**2+(y-0.5d0)**2
        s=64.0d0
        d=1.0d0/16.0d0
        t=dtanh(s*(d-r))
        values(kf)=t
        values(ksk)=t
        values(kad)=t
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
            common /atest2/ibc(4),icont,iu(95),ru(100),su(100)
c
        if(icont.gt.0) ru(icont)=rl
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
            integer
     +          isv(5)
            character*80
     +          sp,su,file(80)
            common /atest1/ip(100),rp(100),sp(100)
            common /atest2/iu(100),ru(100),su(100)
            save len,file
c
            data len/56/
            data (file(i),i=  1, 10)/
     +      'n i= 1,n=left,  a= l,t=i',
     1      'n i= 2,n=top,   a= t,t=i',
     2      'n i= 3,n=right, a= r,t=i',
     3      'n i= 4,n=bottom,a= b,t=i',
     4      'n i= 5,n=icont, a= i,t=i',
     5      'n i= 1,n=a1x,   a=x1,t=r',
     6      'n i= 2,n=a1y,   a=y1,t=r',
     7      'n i= 3,n=a1u,   a=u1,t=r',
     8      'n i= 4,n=a2x,   a=x2,t=r',
     9      'n i= 5,n=a2y,   a=y2,t=r'/
            data (file(i),i= 11, 20)/
     +      'n i= 6,n=a2u,   a=u2,t=r',
     1      'n i= 7,n=bux,   a=bx,t=r',
     2      'n i= 8,n=buy,   a=by,t=r',
     3      'n i= 9,n=cu0,   a=c0,t=r',
     4      'n i=10,n=cu1,   a=c1,t=r',
     5      'n i=11,n=cu2,   a=c2,t=r',
     6      'n i=12,n=cir,   a=cr,t=r',
     7      'n i=13,n=cexp,  a=cx,t=r',
     8      'n i=14,n=eps,   a=e ,t=r',
     9      'n i=15,n=csin,  a=cs,t=r'/
            data (file(i),i= 21, 30)/
     +      'n i=16,n=du0,   a=d0,t=r',
     1      'n i=17,n=du1,   a=d1,t=r',
     2      'n i=18,n=eu0,   a=e0,t=r',
     3      'n i=19,n=f0,    a=f0,t=r',
     4      's n=left  ,v=1,l="neumann"',
     5      's n=left  ,v=2,l="dirichlet"',
     6      's n=left  ,v=0,l="periodic"',
     7      's n=top   ,v=1,l="neumann"',
     8      's n=top   ,v=2,l="dirichlet"',
     9      's n=top   ,v=0,l="periodic"'/
            data (file(i),i= 31, 40)/
     +      's n=right ,v=1,l="neumann"',
     1      's n=right ,v=2,l="dirichlet"',
     2      's n=right ,v=0,l="periodic"',
     3      's n=bottom,v=1,l="neumann"',
     4      's n=bottom,v=2,l="dirichlet"',
     5      's n=bottom,v=0,l="periodic"',
     6      's n=icont ,v= 0,l="none"',
     7      's n=icont ,v= 1,l="a1x"',
     8      's n=icont ,v= 2,l="a1y"',
     9      's n=icont ,v= 3,l="a1u"'/
            data (file(i),i= 41, 50)/
     +      's n=icont ,v= 4,l="a2x"',
     1      's n=icont ,v= 5,l="a2y"',
     2      's n=icont ,v= 6,l="a2u"',
     3      's n=icont ,v= 7,l="bux"',
     4      's n=icont ,v= 8,l="buy"',
     5      's n=icont ,v= 9,l="cu0"',
     6      's n=icont ,v=10,l="cu1"',
     7      's n=icont ,v=11,l="cu2"',
     8      's n=icont ,v=12,l="cir"',
     9      's n=icont ,v=13,l="cexp"'/
            data (file(i),i= 51, 56)/
     +      's n=icont ,v=14,l="eps"',
     1      's n=icont ,v=15,l="csin"',
     2      's n=icont ,v=16,l="du0"',
     3      's n=icont ,v=17,l="du1"',
     4      's n=icont ,v=18,l="eu0"',
     5      's n=icont ,v=19,l="f0"'/
c
c
c       save integer parameters
c
        do i=1,5
            isv(i)=iu(i)
        enddo
c
c       enter input mode
c
        call usrset(file,len,iu,ru,su)
c
c       if any of the integer parameters have changed, call gdata
c
        do i=1,4
            if(iu(i).lt.0.or.iu(i).gt.2) iu(i)=2
            if(iu(i).ne.isv(i)) ip(41)=-1
        enddo
        if(iu(5).eq.0) then
            if(isv(5).ne.0) ip(41)=-1
        else
            if(isv(5).eq.0) ip(41)=-1
            rp(1)=ru(iu(5))
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
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),
     1          w(*),x(9),y(9)
            character*80
     +          sp(100),su(100)
            save x,y,ntf,nvf,ncf,nbf,ispd
c
            data x/0.d0,0.d0,.5d0,1.d0,1.d0,1.d0,.5d0,0.d0,.5d0/
            data y/.5d0,1.d0,1.d0,1.d0,.5d0,0.d0,0.d0,0.d0,.5d0/
            data ntf,nvf,ncf,nbf,ispd/8,9,0,8,0/
c
c           common /atest2/ibc(4),icont,iu(95),a1x,a1y,a1u,a2x,a2y,a2u,
c    +          bux,buy,cu0,cu1,cu2,cir,cexp,eps,csin,du0,du1,eu0,f0,
c    1          ru(81),su(100)
c
        if(ip(41).eq.1) then
            sp(2)='square'
            sp(1)='square'
            sp(3)='square'
            sp(4)='square'
            sp(6)='square_mpixxx.rw'
            sp(7)='square.jnl'
            sp(9)='square_mpixxx.out'
c
c       initialize as laplacian with dirichlet b.c.
c
            do i=1,19
                ru(i)=0.0d0
            enddo
            ru(1)=1.0d0
            ru(5)=1.0d0
            ru(9)=1.0d0
            do i=1,4
               iu(i)=2
            enddo
            iu(5)=0
        endif
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        ip(5)=1
        ip(6)=1
        if(iu(5).ne.0) ip(6)=3
        ip(9)=0
        ip(8)=ispd
        do i=1,ntf
            itnode(1,i)=9
            itnode(2,i)=i
            itnode(3,i)=i-1
            itnode(4,i)=0
            itnode(5,i)=i
ccc         if(i.gt.4) itnode(5,i)=1
            ibndry(1,i)=i
            ibndry(2,i)=i-1
            ibndry(3,i)=0
            k=(i+1)/2
            ibndry(4,i)=iu(k)
            ibndry(5,i)=0
            ibndry(6,i)=(i+1)/2
        enddo
        itnode(3,1)=8
        ibndry(2,1)=8
c
        do i=1,nvf
            vx(i)=x(i)
            vy(i)=y(i)
        enddo
c
c       check for periodic boundary conditions
c
        if(iu(1).eq.0.and.iu(3).eq.0) then
            ibndry(4,1)=-6
            ibndry(4,6)=-1
            ibndry(4,2)=-5
            ibndry(4,5)=-2
        endif
        if(iu(2).eq.0.and.iu(4).eq.0) then
            ibndry(4,3)=-8
            ibndry(4,8)=-3
            ibndry(4,4)=-7
            ibndry(4,7)=-4
        endif
c
        return
        end
