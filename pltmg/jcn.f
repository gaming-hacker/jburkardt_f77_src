c***********************  problem name: jcn     ************************
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
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
c
        call cbeta(x,y,betax,betay,itag)
        values(k0)=ux+u*betax
        values(ku)=betax
        values(kx)=1.0d0
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
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
c
        call cbeta(x,y,betax,betay,itag)
        values(k0)=uy+u*betay
        values(ku)=betay
        values(ky)=1.0d0
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
            common /val1/k0,ku,kl,kuu,kul,klu,kll
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
            common /atest2/ival,iob,iu(98),top,bottom,du,
     +          angle,ct,st,xt(7),yt(7),ru(80),su(100)
c
        values(k0)=top
        if(itag.eq.1) values(k0)=bottom
        values(kic)=values(k0)
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
        za=0.5d0*dabs(u)
        if(za.gt.1.d5) then
            as=2.0d0*za
        else
            as=za+dsqrt(1.0d0+za*za)
        endif
c
        if(u.lt.0.0d0) then
             v=-dlog10(as)
        else
             v=dlog10(as)
        endif
c
        values(kf)=v
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine cbeta(x,y,betax,betay,itag)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            character*80
     +          su
            common /atest2/ival,iob,iu(98),top,bottom,du,
     +          angle,ct,st,xt(7),yt(7),ru(80),su(100)
c
        betax=du*(ct*xt(itag)+st*yt(itag))
        betay=du*(ct*yt(itag)-st*xt(itag))
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
            data len/7/
            data (file(i),i=  1,  7)/
     +      'n i= 1,n=obtuse,a= o,t=i',
     1      'n i= 1,n=top   ,a= t,t=r',
     2      'n i= 2,n=bottom,a= b,t=r',
     3      'n i= 3,n=du    ,a= d,t=r',
     4      'n i= 4,n=angle ,a= a,t=r',
     5      's n=obtuse,v=0,l="do not trisect elements"',
     6      's n=obtuse,v=1,l="trisect all elements"'/
c
        aa=ru(4)
c
        call usrset(file,len,iu,ru,su)
c
        if(iu(1).ne.0) ip(41)=-1
        if(ru(4).ne.aa) then
            pi=3.141592653589793d0
            theta=ru(4)*pi/180.0d0
            ru(5)=dcos(theta)
            ru(6)=dsin(theta)
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
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100),
     1          b1(23),b2(23),b4(23),ib1(7),ib2(7)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(11),y(11)
            character*80
     +           sp(100),su(100)
            save b1,b2,b4,ib1,ib2,x,y
            save ntf,nvf,ncf,nbf,ispd,iprob,hmax,grade
c
            data b1/ 1, 2, 3, 4, 1, 6, 7, 6, 8, 9,
     +               8,10,11,12,13,14,15,16,17, 7,
     1               12,13,14/
            data b2/ 2, 3, 4, 5, 6, 7,12, 8, 9,15,
     +              10,11, 5,13,14, 3,16,17, 4, 9,
     1              15,16,17/
            data b4/ 2, 1, 1, 1, 1, 0, 0, 1, 0, 0,
     +               1, 2, 1, 0, 0, 0, 0, 0, 0, 0,
     1               0, 0, 0/
c
            data ib1/ 8,6, 7,12,14,14,3/
            data ib2/11,8,20,21,15,23,2/
c
            data x/1.75d0,2.0d0 ,2.25d0,2.5d0 ,2.75d0,
     +             1.75d0,2.0d0 ,1.75d0,2.0d0 ,1.75d0,2.75d0/
            data y/0.0d0 ,0.0d0 ,0.0d0 ,0.0d0 ,0.0d0 ,
     +             -.25d0,-.25d0,-.5d0 ,-.5d0 ,-1.0d0,-1.0d0/
            data ntf,nvf,ncf,nbf,ispd,iprob/7,17,0,23,0,1/
            data hmax,grade/0.1d0,1.5d0/
c
c
        if(ip(41).eq.1) then
            iu(1)=0
            ru(1)=1.d10
            ru(2)=1.0d-5
            ru(3)=(40.0d0+15.0d0*dlog(10.0d0))
            ru(4)=0.0d0
            ru(5)=1.0d0
            ru(6)=0.0d0
        endif
c
        if(iu(1).ne.0) go to 100
c
c       set up regions
c
        sp(2)='continuity equation'
        sp(1)='continuity equation'
        sp(3)='continuity equation'
        sp(4)='continuity equation'
        sp(6)='jcn.rw'
        sp(7)='jcn.jnl'
        sp(9)='jcn.out'
c
        ip(5)=1
        ip(6)=iprob
        ip(8)=ispd
        ip(26)=5
        rp(15)=hmax
        rp(16)=grade
c
        ss=2.99563d-2
        pi=3.141592653589793d0
c
        do i=1,11
            vx(i)=x(i)*ss
            vy(i)=y(i)*ss
        enddo
        do i=1,3
            arg=(12.0d0+dfloat(i))*pi/8.0d0
            c=dcos(arg)*ss
            s=dsin(arg)*ss
            vx(11+i)=vx(2)+0.25d0*c
            vy(11+i)=0.25d0*s
            vx(14+i)=vx(2)+0.5d0*c
            vy(14+i)=0.5d0*s
        enddo
c
c       constants for a1xy and a2xy
c
        ru(7)=0.0d0
        ru(14)=0.0d0
c
        ru(8)=0.0d0
        ru(15)=1.0d0/(vy(9)-vy(7))
c
        xx=(vx(9)-vx(7)+vx(15)-vx(12))/2.0d0
        yy=(vy(9)-vy(7)+vy(15)-vy(12))/2.0d0
        dd=1.0d0/(xx*xx+yy*yy)
        ru(9)=xx*dd
        ru(16)=yy*dd
c
        xx=(vx(15)-vx(12)+vx(16)-vx(13))/2.0d0
        yy=(vy(15)-vy(12)+vy(16)-vy(13))/2.0d0
        dd=1.0d0/(xx*xx+yy*yy)
        ru(10)=xx*dd
        ru(17)=yy*dd
c
        xx=(vx(16)-vx(13)+vx(17)-vx(14))/2.0d0
        yy=(vy(16)-vy(13)+vy(17)-vy(14))/2.0d0
        dd=1.0d0/(xx*xx+yy*yy)
        ru(11)=xx*dd
        ru(18)=yy*dd
c
        xx=(vx(17)-vx(14)+vx(4)-vx(3))/2.0d0
        yy=(vy(17)-vy(14)+vy(4)-vy(3))/2.0d0
        dd=1.0d0/(xx*xx+yy*yy)
        ru(12)=xx*dd
        ru(19)=yy*dd
c
        ru(13)=0.0d0
        ru(20)=0.0d0
c
        do i=1,nbf
            ibndry(1,i)=b1(i)
            ibndry(2,i)=b2(i)
            ibndry(3,i)=0
            ibndry(4,i)=b4(i)
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
        ibndry(6,12)=1
        ibndry(6,1)=3
c
        do i=1,ntf
            itnode(1,i)=ib1(i)
            itnode(2,i)=ib2(i)
            itnode(3,i)=0
            itnode(4,i)=0
            itnode(5,i)=i
        enddo
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        return
c
c       make obtuse angles
c
  100   iu(1)=0
        ntf=ip(1)
        nt0=ntf
        nvf=ip(2)
        ncf=ip(3)
        nbf=ip(4)
        maxt=ip(21)
        maxv=ip(22)
        if (ntf*3.gt.maxt) return
        if (nvf+ntf.gt.maxv) return
        do it=1,nt0
            iv1=itnode(1,it)
            iv2=itnode(2,it)
            iv3=itnode(3,it)
            iv4=itnode(4,it)
            iv5=itnode(5,it)
c
            nvf=nvf+1
            vx(nvf)=(vx(iv1)+vx(iv2)+vx(iv3))/3.0d0
            vy(nvf)=(vy(iv1)+vy(iv2)+vy(iv3))/3.0d0
c
            itnode(1,ntf+1)=nvf
            itnode(2,ntf+1)=iv2
            itnode(3,ntf+1)=iv3
            itnode(4,ntf+1)=iv4
            itnode(5,ntf+1)=iv5
            itnode(1,ntf+2)=iv1
            itnode(2,ntf+2)=nvf
            itnode(3,ntf+2)=iv3
            itnode(4,ntf+2)=iv4
            itnode(5,ntf+2)=iv5
            itnode(3,it)=nvf
            ntf=ntf+2
        enddo
        ip(1)=ntf
        ip(2)=nvf
        return
        end
