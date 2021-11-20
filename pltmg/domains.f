c***********************  problem name: domains ************************
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
            common /atest2/iu(100),a1x,a1y,a1u,a2x,a2y,a2u,
     +          bux,buy,cu0,cu1,ru(90),su(100)
c
        values(k0)=a1x*ux+a1y*uy+a1u*u
        values(ku)=a1u
        values(kx)=a1x
        values(ky)=a1y
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
            common /atest2/iu(100),a1x,a1y,a1u,a2x,a2y,a2u,
     +          bux,buy,cu0,cu1,ru(90),su(100)
c
        values(k0)=a2x*ux+a2y*uy+a2u*u
        values(ku)=a2u
        values(kx)=a2x
        values(ky)=a2y
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
            common /atest2/iu(100),a1x,a1y,a1u,a2x,a2y,a2u,
     +          bux,buy,cu0,cu1,ru(90),su(100)
c
        values(k0)= - bux*ux - buy*uy - cu0 - cu1*u
        values(ku)= - cu1
        values(kx) = - bux
        values(ky) = - buy
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
            common /val2/k0,kl,kll,klb,kub,kic
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
        values(k0)=1.0d0
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
        s=x**2+y**2
        values(kf)=dfloat(itag)
        values(ksk)=s
        values(kad)=s
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
     +          sp,su,file(50)
            common /atest1/ip(100),rp(100),sp(100)
            common /atest2/iu(100),ru(100),su(100)
            save len,file
c
            data len/31/
            data (file(i),i=  1, 10)/
     +      'n i= 1,n=domain,a= d,t=i',
     1      'n i= 1,n=a1x,   a=x1,t=r',
     2      'n i= 2,n=a1y,   a=y1,t=r',
     3      'n i= 3,n=a1u,   a=u1,t=r',
     4      'n i= 4,n=a2x,   a=x2,t=r',
     5      'n i= 5,n=a2y,   a=y2,t=r',
     6      'n i= 6,n=a2u,   a=u2,t=r',
     7      'n i= 7,n=bux,   a=bx,t=r',
     8      'n i= 8,n=buy,   a=by,t=r',
     9      'n i= 9,n=cu0,   a=c0,t=r'/
            data (file(i),i= 11, 20)/
     +      'n i=10,n=cu1,   a=c1,t=r',
     1      's n=domain,v=1,l="texas"',
     2      's n=domain,v=2,l="doughnut"',
     3      's n=domain,v=3,l="cmos device"',
     4      's n=domain,v=4,l="lake superior"',
     5      's n=domain,v=5,l="hole"',
     6      's n=domain,v=6,l="at&t logo"',
     7      's n=domain,v=7,l="north sea"',
     8      's n=domain,v=8,l="airfoil"',
     9      's n=domain,v=9,l="planter"'/
            data (file(i),i= 21, 30)/
     +      's n=domain,v=10,l="fan"',
     1      's n=domain,v=11,l="arc"',
     2      's n=domain,v=12,l="spiral"',
     3      's n=domain,v=13,l="ucsd logo"',
     4      's n=domain,v=14,l="nozzle"',
     5      's n=domain,v=15,l="pltmg"',
     6      's n=domain,v=16,l="crack"',
     7      's n=domain,v=17,l="monterey bay"',
     9      's n=domain,v=18,l="siam logo"',
     +      's n=domain,v=19,l="mexico"'/
            data (file(i),i= 31, 31)/
     +      's n=domain,v=20,l="california"'/
c
        iu1=iu(1)
        call usrset(file,len,iu,ru,su)
c
        if(iu1.ne.iu(1)) ip(41)=-1
        ip(41)=-1
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine box(nvf,vx,vy,ncf,xm,ym)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            double precision
     +          vx(*),vy(*),xm(*),ym(*)
c
c       rescale domain to lie inside the unit square
c
        xmin=vx(1)
        xmax=xmin
        ymin=vy(1)
        ymax=ymin
        do i=1,nvf
            xmin=dmin1(xmin,vx(i))
            xmax=dmax1(xmax,vx(i))
            ymin=dmin1(ymin,vy(i))
            ymax=dmax1(ymax,vy(i))
        enddo
c
c       compute scaled coordinates
c
        xmid=(xmin+xmax)/2.0d0
        ymid=(ymin+ymax)/2.0d0
        scale=1.0d0/dmax1(xmax-xmin,ymax-ymin)
        do i=1,nvf
            vx(i)=0.5d0+scale*(vx(i)-xmid)
            vy(i)=0.5d0+scale*(vy(i)-ymid)
        enddo
        if(ncf.le.0) return
        do i=1,ncf
            xm(i)=0.5d0+scale*(xm(i)-xmid)
            ym(i)=0.5d0+scale*(ym(i)-ymid)
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
            data ispd,iprob/1,1/
c
        if(ip(41).eq.1) then
            iu(1)=15
c
c       initialize as laplacian with dirichlet b.c.
c
            do i=1,10
                ru(i)=0.0d0
            enddo
            ru(1)=1.0d0
            ru(5)=1.0d0
            ru(9)=1.0d0
            ip(8)=1
        endif
c
        ip(5)=1
        ip(6)=iprob
        ip(26)=5
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
        if(iu(1).eq.8) call gd8(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.9) call gd9(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.10) call gd10(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.11) call gd11(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.12) call gd12(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.13) call gd13(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.14) call gd14(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.15) call gd15(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.16) call gd16(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.17) call gd17(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.18) call gd18(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.19) call gd19(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
        if(iu(1).eq.20) call gd20(vx,vy,xm,ym,itnode,
     +      ibndry,ip,rp,sp,iu,ru,su,w)
c
        sp(6)='domains_mpixxx.rw'
        sp(7)='domains.jnl'
        sp(9)='domains_mpixxx.out'
c
        nvf=ip(2)
        ncf=ip(3)
        call box(nvf,vx,vy,ncf,xm,ym)
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
     1          x(12),y(12),xc(5),yc(5)
            character*80
     +          sp(100),su(100)
            save x,y,xc,yc,hmax,grade,ntf,nvf,ncf,nbf
c
c       domain in the shape of texas.
c       dirichlet boundary conditions on all boundary edges
c
            data x/ 0.2d0, 5.0d0, 5.0d0, 9.0d0, 9.0d0,15.8d0,
     +             15.9d0,16.0d0,13.0d0, 9.0d0, 6.2d0, 4.0d0/
            data y/ 9.7d0, 9.7d0,16.0d0,16.0d0,12.5d0,12.1d0,
     +             10.2d0, 6.2d0, 0.0d0, 3.2d0, 6.0d0, 6.0d0/
            data xc/ 5.1d0,16.9d0,10.6d0,13.3d0, 8.2d0/
            data yc/5.35d0, 8.2d0, 1.2d0, 4.2d0, 5.0d0/
            data ntf,nvf,ncf,nbf/1,12,5,12/
            data hmax,grade/0.1d0,1.5d0/
c
        sp(2)='texas'
        sp(1)='texas'
        sp(3)='texas'
        sp(4)='texas'
c
        itnode(1,1)=1
        itnode(2,1)=12
        itnode(3,1)=0
        itnode(4,1)=0
        itnode(5,1)=1
        do i=1,nvf
            vx(i)=x(i)
            vy(i)=y(i)
        enddo
        do i=1,nbf
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            ibndry(4,i)=2
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
        ibndry(2,nbf)=1
        ibndry(3,7)=2
        ibndry(3,8)=4
        ibndry(3,9)=3
        ibndry(3,10)=5
        ibndry(3,11)=1
        do i=1,nbf
            if(ibndry(3,i).gt.0) then
                i1=ibndry(1,i)
                i2=ibndry(2,i)
                i3=ibndry(3,i)
                call centre(vx(i1),vy(i1),vx(i2),vy(i2),
     +              xc(i3),yc(i3),xm(i3),ym(i3))
           endif
        enddo
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
c
c       refine long curved edges
c
        call sklutl(1,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
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
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*)
            character*80
     +          sp(100),su(100)
            save hmax,grade,ntf,nvf,ncf,nbf
c
c       annular region between circles of radius one and six
c       dirichlet boundary conditions on inner circle
c       neumann boundary conditions on outer circle
c       there are two irregular octagons concentric with the
c       circles. there are 24 regions, and three groups of
c       eight regions are similar.
c
            data ntf,nvf,ncf,nbf/24,32,1,56/
            data hmax,grade/0.1d0,1.5d0/
c
        sp(2)='doughnut'
        sp(1)='doughnut'
        sp(3)='doughnut'
        sp(4)='doughnut'
c
        pi=3.14159265358979d0
        pi4=pi/4.0d0
        pi8=pi/8.0d0
c
        do i=1,8
            r2=2.0d0
            if((i/2)*2.eq.i) r2=4.0d0
            r3=3.0d0
            if((i/2)*2.eq.i) r3=5.0d0
            r4=6.0d0
            ang=dfloat(i-1)*pi4
            k=(i-1)*4+1
            vx(k)=dcos(ang)
            vy(k)=dsin(ang)
            vx(k+1)=r2*vx(k)
            vy(k+1)=r2*vy(k)
            vx(k+2)=r3*vx(k)
            vy(k+2)=r3*vy(k)
            vx(k+3)=r4*vx(k)
            vy(k+3)=r4*vy(k)
        enddo
        xm(1)=0.0d0
        ym(1)=0.0d0
c
        do i=1,32
            ibndry(1,i)=i
            ibndry(2,i)=i+4
            if(i+4.gt.32) ibndry(2,i)=i+4-32
            ibndry(3,i)=0
            ibndry(4,i)=0
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
        do i=1,8
            k=(i-1)*4+1
            ibndry(3,k)=1
            ibndry(4,k)=2
            ibndry(3,k+3)=1
            ibndry(4,k+3)=1
            do j=1,3
                k=32+(i-1)*3+j
                l=(i-1)*4+j
                ibndry(1,k)=l
                ibndry(2,k)=l+1
                ibndry(3,k)=0
                ibndry(4,k)=0
                ibndry(5,k)=0
                ibndry(6,k)=0
            enddo
        enddo
c
c       comput itnode, find symmetry
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        call sklutl(2,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
c
c       label regions
c
        itnode(5,1)=3
        itnode(5,5)=2
        itnode(5,9)=1
        do i=1,ntf
            j=iabs(itnode(3,i))
            if(j.gt.0) itnode(5,i)=itnode(5,j)
        enddo
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
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100),
     1          b1(35),b2(35),b4(35)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(30),y(30)
            character*80
     +          sp(100),su(100)
            save x,y,b1,b2,b4
            save hmax,grade,ntf,nvf,ncf,nbf
c
c       an irregular region composed of six subregions, two quite small,
c       which define a typical cmos device
c       the data was provided by wolfgang fichtner and donald j. rose
c
            data ntf,nvf,ncf,nbf/6,30,0,35/
            data hmax,grade/0.1d0,1.5d0/
            data x/
     +           0.0d0 , 1.8d0 , 2.4d0 , 2.55d0, 3.15d0,
     1           4.05d0, 7.35d0, 7.8d0 , 8.25d0,11.85d0,
     2          12.6d0 ,12.75d0,13.2d0 ,15.0d0 ,15.0d0 ,
     3          12.6d0 ,11.85d0, 8.25d0, 7.8d0 , 7.35d0,
     4           3.15d0, 2.55d0, 7.35d0,10.0d0 , 0.0d0 ,
     5          0.0d0 ,10.0d0 ,15.0d0 ,  0.0d0, 15.0d0 /
            data y/
     +          -0.11d0,-0.12d0,-0.31d0, 0.25d0, 0.37d0,
     1           0.41d0, 0.40d0, 0.21d0,0.185d0, 0.14d0,
     2           0.01d0,-0.55d0,-0.39d0,-0.35d0,-0.63d0,
     3          -0.63d0,-0.81d0,-0.83d0,-0.8d0 ,-0.58d0,
     4          -0.56d0,-0.39d0,-1.5d0 ,-1.5d0 ,-0.39d0,
     5          -7.5d0 ,-7.5d0 ,-7.5d0 ,-10.0d0,-10.0d0/
c
            data b1/ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16,
     +           17,18,19,20,21,22,22,25,20,23,24,25,26,27,15,28,29,26/
            data b2/ 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,12,17,
     +           18,19,20,21,22, 3,25, 1,23,24,27,26,27,28,28,30,30,29/
            data b4/ 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 0, 0, 0,
     +            0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 2, 1/
c
        sp(2)='cmos device'
        sp(1)='cmos device'
        sp(3)='cmos device'
        sp(4)='cmos device'
c
        do i=1,nvf
            vx(i) = x(i)*2.9563d-2
            vy(i) = y(i)*2.9563d-2
        enddo
        do i=1,nbf
            ibndry(1,i)=b1(i)
            ibndry(2,i)=b2(i)
            ibndry(3,i)=0
            ibndry(4,i)=b4(i)
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
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
     1          x(303),y(303)
            character*80
     +          sp(100),su(100)
            save x,y,hmax,grade,ntf,nvf,ncf,nbf
c
c       region in the shape of lake superior
c       there are five subregions and six islands
c       the data was provided by r. bruce simpson
c
            data ntf,nvf,ncf,nbf/5,303,0,313/
            data hmax,grade/0.1d0,1.5d0/
        data (x(i),i=1,40)/
     +  -8.9154148d0,-8.8847065d0,-8.7270575d0,-8.6780329d0,
     1  -8.2763872d0,-7.7589436d0,-7.3399811d0,-7.2064877d0,
     2  -6.9871612d0,-6.9190874d0,-6.6594362d0,-6.4705076d0,
     3  -6.3982458d0,-6.1776032d0,-5.9683638d0,-6.0500631d0,
     4  -6.2751036d0,-6.2083449d0,-6.3593874d0,-6.4379783d0,
     5  -6.4389687d0,-6.3308568d0,-5.9216681d0,-5.8853531d0,
     6  -5.5306988d0,-5.4571118d0,-5.2776995d0,-4.6882658d0,
     7  -4.4325552d0,-3.9867787d0,-3.6232784d0,-3.5156848d0,
     8  -3.1151142d0,-2.6031885d0,-2.4201615d0,-2.0576785d0,
     9  -1.9469492d0,-1.6564457d0,-1.4385855d0,-1.1857181d0/
        data (x(i),i=41,80)/
     +  -0.8965760d0,-0.4656316d0,-0.0715676d0, 0.3221563d0,
     1   0.6089023d0, 0.6092879d0, 0.3942456d0, 0.2866788d0,
     2   0.0717038d0, 0.0896864d0, 0.1076576d0,-0.1436793d0,
     3  -0.3235837d0,-0.4676923d0,-0.9391942d0,-1.0495193d0,
     4  -1.0882363d0,-1.0158452d0,-0.9424102d0,-0.6152380d0,
     5  -0.5061946d0,-0.2890742d0,-0.5066670d0,-0.6160022d0,
     6  -0.3979720d0,-0.1808403d0, 0.2894343d0, 0.5431103d0,
     7   0.9058832d0, 0.9432856d0, 1.0534256d0, 1.1641909d0,
     8   1.3109204d0, 1.3117251d0, 1.4214690d0, 1.9317214d0,
     9   2.2226055d0, 2.3340521d0, 2.4073553d0, 2.4449496d0/
        data (x(i),i=81,120)/
     +   2.5909064d0, 2.6249835d0, 2.7343488d0, 2.8454552d0,
     1   2.9366469d0, 2.9201944d0, 2.9931922d0, 3.3135431d0,
     2   3.3863590d0, 3.9960330d0, 4.0705566d0, 4.6490631d0,
     3   4.9039979d0, 5.4495530d0, 5.9850464d0, 6.1653805d0,
     4   6.3476152d0, 6.6355958d0, 6.5309086d0, 6.4623699d0,
     5   6.4803085d0, 6.4094977d0, 6.4100089d0, 6.4882669d0,
     6   6.7068648d0, 6.8546906d0, 6.9296956d0, 7.3625917d0,
     7   7.4195042d0, 7.4810257d0, 7.6622000d0, 7.7339420d0,
     8   7.6927729d0, 7.6199031d0, 7.5788198d0, 7.4649954d0,
     9   7.4604096d0, 7.6668096d0, 7.7709460d0, 7.7298813d0/
        data (x(i),i=121,160)/
     +   7.6707368d0, 7.6356735d0, 7.6246142d0, 7.5532117d0,
     1   7.5280862d0, 7.5345392d0, 7.7131429d0, 7.8603272d0,
     2   7.9228745d0, 7.7348900d0, 7.3713822d0, 7.2698317d0,
     3   7.1885796d0, 7.1152658d0, 7.0441689d0, 6.9336958d0,
     4   6.9676142d0, 7.0648074d0, 7.0897107d0, 7.2917361d0,
     5   7.3230152d0, 7.2074218d0, 7.0987911d0, 7.0562592d0,
     6   6.8349671d0, 6.5426011d0, 6.3538117d0, 6.6493349d0,
     7   6.5385165d0, 6.7052994d0, 6.7009764d0, 5.7441173d0,
     8   5.5686946d0, 4.7886071d0, 4.4636817d0, 4.2075763d0,
     9   3.6075780d0, 3.3873644d0, 3.2804384d0, 3.1762848d0/
        data (x(i),i=161,200)/
     +   3.0381961d0, 2.8286881d0, 2.8598070d0, 2.6845531d0,
     1   2.5468001d0, 2.4437671d0, 2.3390419d0, 2.1983123d0,
     2   2.1616242d0, 2.0918984d0, 2.0235167d0, 1.9540682d0,
     3   1.5705080d0, 1.5002116d0, 1.2547448d0, 0.7309650d0,
     4   0.4522013d0, 0.1738657d0,-0.0694533d0,-0.1737495d0,
     5  -0.2084645d0,-0.3476150d0,-0.4518238d0,-0.5558115d0,
     6  -0.5215963d0,-0.5221188d0,-0.4879606d0,-0.2092654d0,
     7  -0.2097517d0,-0.5257651d0,-0.5604465d0,-0.7716285d0,
     8  -0.9473082d0,-0.9833619d0,-1.1593398d0,-1.2287946d0,
     9  -1.1574372d0,-0.6994015d0,-0.5933112d0,-0.6973173d0/
        data (x(i),i=201,240)/
     +  -0.9759166d0,-1.1175557d0,-1.1194069d0,-1.2951661d0,
     1  -1.5483098d0,-1.9366363d0,-1.8637764d0,-1.7582839d0,
     2  -1.7202965d0,-1.5427314d0,-1.5417155d0,-1.5762336d0,
     3  -1.9985176d0,-2.3506579d0,-2.5645144d0,-2.5351899d0,
     4  -2.7562921d0,-3.7980607d0,-4.3373508d0,-4.9834948d0,
     5  -5.9256530d0,-7.0382285d0,-7.4517045d0,-8.2033901d0,
     6  -8.7235365d0,-6.0865688d0,-6.0818601d0,-5.7929716d0,
     7  -5.6808643d0,-5.5966616d0,-5.6635571d0,-5.8318772d0,
     8  -5.8345900d0,-5.7920160d0,-5.8726344d0,-5.9152017d0,
     9  -6.0222359d0,-5.7677011d0,-5.7297702d0,-5.4914570d0/
        data (x(i),i=241,280)/
     +  -5.4498162d0,-5.5318775d0,-5.5335989d0,-5.5683675d0,
     1  -5.7324457d0,-5.3444929d0,-5.3193455d0,-5.3166866d0,
     2  -5.2924027d0,-5.1911650d0,-5.2196560d0,-5.2257104d0,
     3  -5.2706485d0,-2.5904889d0,-2.3756721d0,-2.0900009d0,
     4  -1.8405570d0,-1.7340945d0,-1.6276655d0,-1.5563954d0,
     5  -1.3075202d0,-0.9532116d0,-0.8828910d0,-0.9185046d0,
     6  -1.2022868d0,-1.1676824d0,-1.5239785d0,-1.7011793d0,
     7  -1.9150532d0,-2.0937130d0,-2.0588911d0,-1.8814086d0,
     8  -2.1664250d0,-2.4524965d0,-2.5231726d0,-2.6285090d0,
     9  -0.1743299d0, 0.0348428d0, 0.1393711d0, 0.4880418d0/
        data (x(i),i=281,303)/
     +   0.6273773d0, 0.7325480d0, 0.8201638d0, 0.6632222d0,
     1   0.5583180d0, 0.3839078d0, 0.2267794d0, 0.0349007d0,
     2  -0.0349355d0,-0.0174793d0,-0.1048759d0,-0.1745905d0,
     3   4.3813114d0, 4.3785076d0, 4.5535316d0, 4.8364964d0,
     4   4.9818983d0, 5.0886197d0, 5.1614199d0, 5.1274772d0,
     5   4.9866867d0, 4.8814216d0, 4.5964503d0/
        data (y(i),i=1,40)/
     +   1.6615920d0, 1.5538653d0, 1.4396306d0, 1.3310252d0,
     1   1.3642085d0, 1.5521208d0, 1.5866412d0, 1.7400111d0,
     2   1.7838988d0, 1.6751018d0, 1.8237880d0, 2.0285108d0,
     3   2.0257761d0, 2.1236043d0, 1.9041636d0, 1.6420918d0,
     4   1.4382319d0, 1.2768143d0, 1.1233594d0, 0.9672913d0,
     5   0.9408290d0, 0.9103191d0, 1.1606956d0, 1.1594465d0,
     6   0.8827500d0, 0.9068911d0, 0.8218321d0, 1.0696110d0,
     7   1.1157840d0, 1.5284986d0, 1.5733560d0, 1.5181578d0,
     8   1.6162773d0, 1.9253443d0, 2.0814581d0, 2.1825509d0,
     9   2.3929844d0, 2.6016250d0, 2.8113568d0, 2.9681802d0/
        data (y(i),i=41,80)/
     +   3.2839699d0, 3.4939342d0, 3.6520867d0, 3.5994537d0,
     1   3.4943945d0, 3.3885374d0, 3.3878930d0, 3.4141383d0,
     2   3.3345153d0, 3.2286668d0, 3.1757488d0, 3.0169899d0,
     3   2.8584538d0, 2.7529347d0, 2.0668111d0, 1.7498742d0,
     4   1.3531376d0, 1.3262275d0, 1.4845979d0, 1.7477533d0,
     5   1.9061816d0, 2.0115316d0, 1.7473959d0, 1.5360392d0,
     6   1.8000363d0, 1.8525960d0, 1.7998173d0, 1.6681145d0,
     7   1.5373304d0, 1.3258125d0, 1.1147327d0, 0.8507940d0,
     8   0.6930545d0, 0.5871977d0, 0.5351345d0, 0.5400872d0,
     9   0.5965222d0, 0.4391798d0, 0.4137156d0, 0.3348350d0/
        data (y(i),i=81,120)/
     +   0.3369578d0, 0.4962917d0, 0.4979874d0, 0.3938967d0,
     1   0.3954205d0, 0.2892562d0, 0.2905022d0, 0.7199108d0,
     2   0.7213257d0, 1.1315466d0, 1.0538628d0, 1.1744760d0,
     3   1.1550887d0, 1.1450567d0, 1.4278414d0, 1.4607402d0,
     4   1.4409063d0, 1.5048352d0, 1.3948027d0, 1.2862027d0,
     5   0.8098819d0, 0.7542354d0, 0.7012564d0, 0.5981826d0,
     6   0.6065329d0, 0.5593290d0, 0.5093111d0, 0.6332439d0,
     7   0.5826584d0, 0.4262426d0, 0.4606821d0, 0.4903887d0,
     8   0.5946095d0, 0.5913803d0, 0.6956243d0, 0.7967085d0,
     9   0.9025555d0, 1.1767640d0, 1.2875088d0, 1.3917131d0/
        data (y(i),i=121,160)/
     +   1.4951115d0, 1.4670289d0, 1.3074532d0, 1.2777792d0,
     1   1.4357493d0, 1.7011603d0, 1.7621690d0, 1.7158544d0,
     2   1.9308907d0, 2.0813403d0, 2.1182086d0, 1.9547751d0,
     3   2.1633925d0, 2.1868122d0, 2.1573400d0, 2.2058041d0,
     4   2.2602055d0, 2.5292990d0, 2.7954185d0, 3.1221592d0,
     5   3.2295635d0, 3.4101255d0, 3.4319942d0, 3.5892427d0,
     6   3.7390778d0, 3.8863685d0, 4.1440578d0, 4.7917061d0,
     7   4.8932843d0, 5.1650314d0, 5.2708721d0, 5.2345815d0,
     8   5.1755509d0, 5.1509480d0, 5.3536510d0, 5.6646352d0,
     9   7.1858878d0, 7.6576362d0, 7.7613006d0, 7.7327189d0/
        data (y(i),i=161,200)/
     +   7.6506119d0, 7.6467633d0, 7.8590922d0, 7.9089847d0,
     1   7.8008409d0, 7.6933494d0, 7.6917801d0, 7.7691860d0,
     2   7.9010277d0, 7.9000964d0, 7.7933407d0, 7.7660065d0,
     3   7.7353015d0, 7.7875576d0, 7.9442315d0, 8.1526823d0,
     4   8.2575016d0, 8.3098812d0, 8.5215149d0, 8.4157381d0,
     5   8.4422445d0, 8.3630953d0, 8.3898230d0, 8.4695473d0,
     6   8.3106441d0, 8.1518583d0, 7.9400339d0, 7.8335662d0,
     7   7.4630661d0, 7.0403595d0, 7.1463351d0, 6.9354925d0,
     8   6.8835006d0, 6.7249255d0, 6.6731634d0, 6.7795396d0,
     9   6.9378047d0, 7.4115276d0, 7.7286749d0, 7.8878837d0/
        data (y(i),i=201,240)/
     +   7.9422774d0, 7.6256237d0, 7.3609824d0, 7.2564440d0,
     1   6.4117608d0, 6.3100700d0, 6.5209551d0, 6.5197735d0,
     2   6.7840347d0, 6.9939694d0, 7.0998254d0, 7.1530938d0,
     3   6.9989867d0, 6.8978705d0, 6.6893725d0, 6.3183274d0,
     4   5.7395725d0, 5.0191441d0, 4.7676754d0, 4.5740476d0,
     5   4.1282315d0, 3.1643631d0, 2.7580044d0, 2.1245258d0,
     6   1.8639193d0, 1.4314222d0, 1.5637339d0, 1.7391469d0,
     7   1.8413432d0, 1.7590827d0, 1.7083083d0, 1.6609927d0,
     8   1.5816056d0, 1.5536638d0, 1.5299217d0, 1.5578835d0,
     9   1.4026510d0, 1.9502324d0, 2.0019357d0, 2.1000333d0/
        data (y(i),i=241,280)/
     +   2.0457127d0, 1.9688923d0, 1.9159672d0, 1.9594866d0,
     1   1.9225485d0, 2.1748121d0, 2.2852740d0, 2.3699543d0,
     2   2.4539566d0, 2.4614151d0, 2.3563406d0, 2.2770641d0,
     3   2.2254789d0, 5.0486274d0, 5.1777287d0, 5.3326516d0,
     4   5.4619961d0, 5.4872999d0, 5.5126729d0, 5.5649114d0,
     5   5.7215257d0, 5.8778610d0, 5.8245349d0, 5.7718015d0,
     6   5.6148539d0, 5.5087409d0, 5.2470098d0, 5.2487521d0,
     7   5.1452403d0, 5.0415487d0, 4.9881725d0, 4.9860468d0,
     8   4.9101486d0, 4.7818141d0, 4.8093510d0, 4.8904085d0,
     9   7.8864527d0, 7.9922180d0, 7.9922752d0, 7.9135695d0/
        data (y(i),i=281,303)/
     +   7.9405222d0, 7.8086476d0, 7.7296796d0, 7.7024851d0,
     1   7.7550130d0, 7.7280331d0, 7.7806611d0, 7.7275753d0,
     2   7.5687895d0, 7.4629297d0, 7.4629631d0, 7.6482739d0,
     3   4.4510627d0, 4.5569129d0, 4.6675730d0, 4.7285905d0,
     4   4.6269722d0, 4.6302257d0, 4.5795088d0, 4.5254769d0,
     5   4.4681988d0, 4.4120893d0, 4.4039326d0/
c
c
        sp(2)='lake superior'
        sp(1)='lake superior'
        sp(3)='lake superior'
        sp(4)='lake superior'
c
        do i=1,303
            ibndry(1,i)=i-1
            ibndry(2,i)=i
            ibndry(3,i)=0
            ibndry(4,i)=2
            ibndry(5,i)=0
            ibndry(6,i)=0
            vx(i)=x(i)
            vy(i)=y(i)
        enddo
        ibndry(1,1)=225
        ibndry(1,226)=237
        ibndry(1,238)=245
        ibndry(1,246)=253
        ibndry(1,254)=276
        ibndry(1,277)=292
        ibndry(1,293)=303
        do i=304,313
            ibndry(3,i)=0
            ibndry(4,i)=0
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
        ibndry(1,304)=153
        ibndry(2,304)=299
        ibndry(1,305)=154
        ibndry(2,305)=296
        ibndry(1,306)=189
        ibndry(2,306)=291
        ibndry(1,307)=188
        ibndry(2,307)=277
        ibndry(1,308)=254
        ibndry(2,308)=217
        ibndry(1,309)=216
        ibndry(2,309)=256
        ibndry(1,310)=221
        ibndry(2,310)=249
        ibndry(1,311)=246
        ibndry(2,311)=241
        ibndry(1,312)=229
        ibndry(2,312)=245
        ibndry(1,313)=237
        ibndry(2,313)=18
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
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*)
            character*80
     +          sp(100),su(100)
            save ntf,nbf,nvf,ncf,hmax
            data ntf,nbf,nvf,ncf/2,12,10,1/
            data hmax/0.5d0/
c
        sp(2)='hole'
        sp(1)='hole'
        sp(3)='hole'
        sp(4)='hole'
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
c
        vx(1)=-2.d0
        vy(1)=2.d0
        vx(2)=-2.d0
        vy(2)=-2.d0
        vx(3)=0.d0
        vy(3)=-2.d0
        vx(4)=2.d0
        vy(4)=-2.d0
        vx(5)=2.d0
        vy(5)=2.d0
        vx(6)=0.d0
        vy(6)=2.d0
        vx(7)=0.d0
        vy(7)=1.d0
        vx(8)=-1.d0
        vy(8)=0.d0
        vx(9)=0.0d0
        vy(9)=-1.0d0
        vx(10)=1.d0
        vy(10)=0.d0
c
        xm(1)=0.d0
        ym(1)=0.d0
c
        do i=1,nbf
            ibndry(1,i)=i-1
            ibndry(2,i)=i
            ibndry(3,i)=0
            if(i.ge.8.and.i.le.11) ibndry(3,i)=1
            ibndry(4,i)=1
            if(i.ge.2.and.i.le.5) ibndry(4,i)=2
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
c
        ibndry(1,1)=6
        ibndry(1,12)=3
        ibndry(2,11)=7
        ibndry(2,12)=9
        ibndry(4,7)=0
        ibndry(4,12)=0
c
        itnode(1,1)=6
        itnode(2,1)=1
        itnode(3,1)=0
        itnode(4,1)=0
        itnode(5,1)=1
        itnode(1,2)=6
        itnode(2,2)=7
        itnode(3,2)=-1
        itnode(4,2)=0
        itnode(5,2)=2
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
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100),
     1          b1(159),b2(159),b3(159),b4(159),b5(159),c1(25),
     2          c2(25),c4(25)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(135),y(135),xp(93),yp(93)
            character*80
     +          sp(100),su(100)
            save ntf,ncf,nbf,nvf,x,y,xp,yp,b1,b2,b3,b4,b5,
     +          c1,c2,c4,hmax,grade
c
c       an irregular region composed of 25 subregions, in the shape of,
c       the at&t logo.
c       the data was provided by r. kent smith
c
c
            data ntf,nvf,nbf,ncf/25,135,159,93/
c
            data (x(i),i=1,40)/
     +       -2.9088581d0, -0.2115240d0,  3.0000000d0,  0.0000000d0,
     1       -4.0919900d0, -3.1212461d0, -2.4240000d0, -1.1470000d0,
     2       -0.5369100d0,  2.0187540d0,  2.7160001d0,  4.3160391d0,
     3        3.3551569d0,  2.1120000d0,  1.0151761d0, -0.5109150d0,
     4       -1.3640000d0, -2.1440001d0, -2.9284019d0, -3.6108899d0,
     5       -4.9742250d0, -4.1352282d0, -3.2990000d0, -0.4100000d0,
     6        0.1129340d0,  2.4291310d0,  3.4749999d0,  5.0882888d0,
     7        4.5282569d0,  2.8659999d0,  2.2388289d0,  0.1792850d0,
     8       -0.8660000d0, -3.1520000d0, -3.8492460d0, -4.6628761d0,
     9       -5.5230289d0, -4.4090900d0, -3.7990000d0, -0.0510000d0/
            data (x(i),i=41,80)/
     +        0.5761710d0,  2.4727149d0,  3.5179999d0,  5.6014829d0,
     1        5.2477179d0,  3.4890001d0,  2.4437151d0,  0.5216990d0,
     2       -0.2100000d0, -3.6550000d0, -4.5265570d0, -5.3460388d0,
     3       -5.8688860d0, -4.8302279d0, -3.9940000d0, -0.1400000d0,
     4        0.6444020d0,  2.7805979d0,  3.5650001d0,  5.8897629d0,
     5        5.6731110d0,  3.6570001d0,  2.7162440d0,  0.5342280d0,
     6       -0.3020000d0, -3.9630001d0, -4.6602459d0, -5.7378292d0,
     7       -5.9963450d0, -4.5582852d0, -3.5130000d0, -0.4560000d0,
     8        0.3284020d0,  5.9990859d0,  5.9261298d0,  0.5425570d0,
     9       -0.3290000d0, -3.7790000d0, -4.6505570d0, -5.9552770d0/
            data (x(i),i=81,120)/
     +       -5.9416080d0, -4.1115570d0, -3.2400000d0, -0.7110000d0,
     1       -0.0137540d0,  2.1172869d0,  3.0760000d0,  5.9416080d0,
     2        5.9990859d0,  3.5500000d0,  2.6784430d0, -0.1595980d0,
     3       -0.9440000d0, -3.2820001d0, -3.9795649d0, -5.9963450d0,
     4       -5.7063389d0, -3.0675910d0, -1.1144090d0,  1.3666790d0,
     5        2.1340001d0,  5.7063389d0,  5.8897629d0,  2.8210001d0,
     6        2.2629480d0, -1.0966530d0, -3.4503360d0, -5.8897629d0,
     7       -5.2477179d0, -2.6954920d0, -1.6485080d0,  0.7010050d0,
     8        1.0500000d0,  5.2477179d0,  5.6014829d0,  1.9870000d0,
     9        1.4635080d0, -1.5375080d0, -2.5844920d0, -5.6014829d0/
            data (x(i),i=121,135)/
     +       -4.5282569d0,  4.5282569d0,  5.0882888d0, -5.0882888d0,
     1       -3.3551569d0,  3.3551569d0,  4.3160391d0, -4.3160391d0,
     2       -3.0000000d0,  3.0000000d0,  0.0000000d0,  0.0000000d0,
     3       40.0000000d0,  0.0000000d0,-40.0000000d0/
            data (y(i),i=1,40)/
     +        5.2477179d0,  5.2805071d0,  5.1961522d0,  6.0000000d0,
     1        4.3881221d0,  4.4595580d0,  4.4899998d0,  4.4899998d0,
     2        4.4633632d0,  4.2354422d0,  4.2049999d0,  4.1679502d0,
     3        4.9742250d0,  4.9850001d0,  4.9179149d0,  4.7371769d0,
     4        4.6849999d0,  4.6849999d0,  4.7192478d0,  4.7918129d0,
     5        3.3551569d0,  3.4211750d0,  3.4649999d0,  3.4649999d0,
     6        3.4421680d0,  3.2256639d0,  3.1800001d0,  3.1795161d0,
     7        3.9363539d0,  3.9600000d0,  3.9271309d0,  3.7097809d0,
     8        3.6550000d0,  3.6550000d0,  3.6854420d0,  3.7759221d0,
     9        2.3443871d0,  2.4183631d0,  2.4449999d0,  2.4449999d0/
            data (y(i),i=41,80)/
     +        2.4121311d0,  2.2247810d0,  2.1700001d0,  2.1502080d0,
     1        2.9088581d0,  2.9500000d0,  2.8952191d0,  2.6783471d0,
     2        2.6400001d0,  2.6400001d0,  2.6780529d0,  2.7239430d0,
     3        1.2474700d0,  1.3811750d0,  1.4250000d0,  1.4250000d0,
     4        1.3907520d0,  1.1892480d0,  1.1550000d0,  1.1448539d0,
     5        1.9534090d0,  1.9349999d0,  1.8856970d0,  1.6688250d0,
     6        1.6250000d0,  1.6250000d0,  1.6554420d0,  1.7542300d0,
     7        0.2093970d0,  0.3702190d0,  0.4250000d0,  0.4250000d0,
     8        0.3907520d0,  0.1047140d0,  0.9386070d0,  0.6630530d0,
     9        0.6250000d0,  0.6250000d0,  0.6630530d0,  0.7312160d0/
            data (y(i),i=81,120)/
     +       -0.8350390d0, -0.6480530d0, -0.6100000d0, -0.6100000d0,
     1       -0.6404420d0, -0.8331420d0, -0.8750000d0, -0.8350390d0,
     2       -0.1047140d0, -0.0950000d0, -0.1330530d0, -0.3657520d0,
     3       -0.4000000d0, -0.4000000d0, -0.3756410d0, -0.2093970d0,
     4       -1.8541020d0, -1.6861030d0, -1.6861030d0, -1.8732049d0,
     5       -1.9000000d0, -1.8541020d0, -1.1448539d0, -1.1200000d0,
     6       -1.1394880d0, -1.3924609d0, -1.3719200d0, -1.1448539d0,
     7       -2.9088581d0, -2.8201380d0, -2.8201380d0, -2.9089079d0,
     8       -2.9150000d0, -2.9088581d0, -2.1502080d0, -2.1350000d0,
     9       -2.1441381d0, -2.2548621d0, -2.2548621d0, -2.1502080d0/
            data (y(i),i=121,135)/
     +       -3.9363539d0, -3.9363539d0, -3.1795161d0, -3.1795161d0,
     1       -4.9742250d0, -4.9742250d0, -4.1679502d0, -4.1679502d0,
     2       -5.1961522d0, -5.1961522d0, -6.0000000d0, 40.0000000d0,
     3        0.0000000d0,-40.0000000d0,  0.0000000d0/
            data (xp(i),i=1,40)/
     +       -1.5328120d0,  1.5529140d0, -1.5022800d0, -2.7729549d0,
     1       -0.8416640d0,  2.3670449d0,  3.8567259d0,  1.5625629d0,
     2       -0.9366600d0, -2.5365739d0, -3.8567259d0, -3.7176881d0,
     3       -0.1482840d0,  2.9515669d0,  4.8231411d0,  2.5519841d0,
     4       -0.3426400d0, -3.5009551d0, -4.8231411d0, -4.1043358d0,
     5        0.2630160d0,  2.9946401d0,  5.4378471d0,  2.9656401d0,
     6        0.1563520d0, -4.0911942d0, -5.4378471d0, -4.4126878d0,
     7        0.2525740d0,  3.1724260d0,  5.7955551d0,  3.1859760d0,
     8        0.1166880d0, -4.3119550d0, -5.8088861d0, -4.0363598d0,
     9       -0.0634260d0,  5.9771681d0,  0.1071940d0, -4.2151942d0/
            data (xp(i),i=41,80)/
     +       -5.9815040d0, -3.6761940d0, -0.3620450d0,  2.5961871d0,
     1        5.9815040d0,  3.1138060d0, -0.5514260d0, -3.6309950d0,
     2       -5.9771681d0, -2.0910001d0,  1.7501060d0,  5.8088861d0,
     3        2.5418041d0, -2.2738979d0, -5.8088861d0, -2.1719999d0,
     4        0.8754760d0,  5.4378471d0,  1.7252140d0, -2.0610001d0,
     5       -5.4378471d0,  4.8231411d0, -4.8231411d0,  3.8567259d0,
     6       -3.8567259d0,  1.5529140d0, -1.5529140d0,  3.1795161d0,
     7       -3.2678339d0,  4.4236641d0, -4.3881221d0,  5.1697750d0,
     8       -5.1697750d0,  5.6381559d0, -5.6381559d0,  5.9088469d0,
     9       -5.9177141d0,  6.0000000d0, -6.0000000d0,  5.9177141d0/
            data (xp(i),i=81,93)/
     +       -5.9177141d0,  5.6558490d0, -5.6558490d0,  5.1697750d0,
     1       -5.1697750d0,  4.4236641d0, -4.4236641d0,  3.1795161d0,
     2       -3.1795161d0, 28.2842712d0, 28.2842712d0,-28.2842712d0,
     3      -28.2842712d0/
            data (yp(i),i=1,40)/
     +        5.3786950d0,  5.7955551d0,  5.8088861d0,  4.4823861d0,
     1        4.4833379d0,  4.2126141d0,  4.5962672d0,  4.9682131d0,
     2        4.6980562d0,  4.6935658d0,  4.5962672d0,  3.4540360d0,
     3        3.4592891d0,  3.1914210d0,  3.5689371d0,  3.9517770d0,
     4        3.6687050d0,  3.6626141d0,  3.5689371d0,  2.4383380d0,
     5        2.4367771d0,  2.1837051d0,  2.5357101d0,  2.9362950d0,
     6        2.6495931d0,  2.6495180d0,  2.5357101d0,  1.4140360d0,
     7        1.4164340d0,  1.1635660d0,  1.5529140d0,  1.9226660d0,
     8        1.6359640d0,  1.6326140d0,  1.5022800d0,  0.4112950d0,
     9        0.4164340d0,  0.5229340d0,  0.6345180d0,  0.6345180d0/
            data (yp(i),i=41,80)/
     +        0.4707550d0, -0.6195180d0, -0.6176140d0, -0.8645300d0,
     1       -0.4707550d0, -0.1045180d0, -0.3914340d0, -0.3939080d0,
     2       -0.5229340d0, -1.6520000d0, -1.8932990d0, -1.5022800d0,
     3       -1.1248730d0, -1.4284290d0, -1.5022800d0, -2.8110001d0,
     4       -2.9134769d0, -2.5357101d0, -2.1372850d0, -2.2639999d0,
     5       -2.5357101d0, -3.5689371d0, -3.5689371d0, -4.5962672d0,
     6       -4.5962672d0, -5.7955551d0, -5.7955551d0,  5.0882888d0,
     7        5.0320230d0,  4.0535412d0,  4.0919900d0,  3.0452299d0,
     8        3.0452299d0,  2.0521209d0,  2.0521209d0,  1.0418890d0,
     9        0.9902860d0,  0.0000000d0,  0.0000000d0, -0.9902860d0/
            data (yp(i),i=81,93)/
     +       -0.9902860d0, -2.0028410d0, -2.0028410d0, -3.0452299d0,
     1       -3.0452299d0, -4.0535412d0, -4.0535412d0, -5.0882888d0,
     2       -5.0882888d0, 28.2842712d0,-28.2842712d0,-28.2842712d0,
     3       28.2842712d0/
c
            data (b1(i),i=1,100)/
     +        1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
     1       11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     2       21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
     3       31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
     4       41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
     5       51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
     6       61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
     7       71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
     8       81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
     9       91, 92, 93, 94, 95, 96, 97, 98, 99,100/
            data (b1(i),i=101,159)/
     +      101,102,103,104,105,106,107,108,109,110,
     1      111,112,113,114,115,116,117,118,119,120,
     2      121,122,123,124,125,126,127,128,129,130,
     3      131,  3,  1, 12,  5, 28, 21, 44, 37, 60,
     4       53, 74, 69, 88, 81,102, 97,114,109,122,
     5      121,126,125,132,133,134,135,132,134/
            data (b2(i),i=1,100)/
     +        2,  3,  4,  1,  6,  7,  8,  9, 10, 11,
     1       12, 13, 14, 15, 16, 17, 18, 19, 20,  5,
     2       22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
     3       32, 33, 34, 35, 36, 21, 38, 39, 40, 41,
     4       42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
     5       52, 37, 54, 55, 56, 57, 58, 59, 60, 61,
     6       62, 63, 64, 65, 66, 67, 68, 53, 70, 71,
     7       72, 73, 74, 75, 76, 77, 78, 79, 80, 69,
     8       82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
     9       92, 93, 94, 95, 96, 81, 98, 99,100,101/
            data (b2(i),i=101,159)/
     +      102,103,104,105,106,107,108, 97,110,111,
     1      112,113,114,115,116,117,118,119,120,109,
     2      122,123,124,121,126,127,128,125,130,131,
     3      129, 13, 20, 29, 36, 45, 52, 61, 68, 75,
     4       80, 89, 96,103,108,115,120,123,124,127,
     5      128,130,129,133,134,135,132,  4,131/
            data (b3(i),i=1,100)/
     +        1,  0,  2,  3,  0,  4,  0,  5,  0,  6,
     1        0,  7,  0,  8,  0,  9,  0, 10,  0, 11,
     2        0, 12,  0, 13,  0, 14,  0, 15,  0, 16,
     3        0, 17,  0, 18,  0, 19,  0, 20,  0, 21,
     4        0, 22,  0, 23,  0, 24,  0, 25,  0, 26,
     5        0, 27,  0, 28,  0, 29,  0, 30,  0, 31,
     6        0, 32,  0, 33,  0, 34,  0, 35,  0, 36,
     7        0, 37,  0, 38,  0, 39,  0, 40,  0, 41,
     8        0, 42,  0, 43,  0, 44,  0, 45,  0, 46,
     9        0, 47,  0, 48,  0, 49,  0, 50,  0, 51/
            data (b3(i),i=101,159)/
     +        0, 52,  0, 53,  0, 54,  0, 55,  0, 56,
     1        0, 57,  0, 58,  0, 59,  0, 60,  0, 61,
     2        0, 62,  0, 63,  0, 64,  0, 65,  0, 66,
     3       67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
     4       77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
     5       87, 88, 89, 90, 91, 92, 93,  0,  0/
            data (b4(i),i=1,100)/
     +        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     1        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     2        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     3        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     4        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     5        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     6        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     7        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     8        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     9        0,  0,  0,  0,  0,  0,  0,  0,  0,  0/
            data (b4(i),i=101,159)/
     +        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     1        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     2        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     3        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     4        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     5        0,  0,  0,  2,  2,  2,  2,  0,  0/
            data (b5(i),i=1,100)/
     +        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     1        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     2        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     3        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     4        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     5        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     6        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     7        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     8        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     9        0,  0,  0,  0,  0,  0,  0,  0,  0,  0/
            data (b5(i),i=101,159)/
     +        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     1        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     2        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     3        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     4        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     5        0,  0,  0,  1,  1,  1,  1,  0,  0/
c
            data (c1(i),i=1,25)/
     +        4,  1, 20,  5, 36, 21, 52, 37, 68, 53,
     1       80, 69, 96, 81,108, 97,120,109,124,121,
     2      128,125,131,134,134/
            data (c2(i),i=1,25)/
     +        4,133, 20,135, 36,137, 52,139, 68,141,
     1       80,143, 96,145,108,147,120,149,124,151,
     2      128,153,130,155,159/
            data (c4(i),i=1,25)/
     +        1,  2,  1,  2,  1,  2,  1,  2,  1,  2,
     1        1,  2,  1,  2,  1,  2,  1,  2,  1,  2,
     2        1,  2,  1,  3,  3/
c
            data hmax,grade/0.05d0,1.5d0/
c
        sp(2)='at&t logo'
        sp(1)='at&t logo'
        sp(3)='at&t logo'
        sp(4)='at&t logo'
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
c
        do i=1,nvf
            vx(i)=x(i)
            vy(i)=y(i)
        enddo
c
        do i=1,nbf
            ibndry(1,i)=b1(i)
            ibndry(2,i)=b2(i)
            ibndry(3,i)=b3(i)
            ibndry(4,i)=b4(i)
            ibndry(5,i)=0
            ibndry(6,i)=b5(i)
c
            if(ibndry(3,i).gt.0) then
                i1=ibndry(1,i)
                i2=ibndry(2,i)
                i3=ibndry(3,i)
                call centre(vx(i1),vy(i1),vx(i2),vy(i2),
     +              xp(i3),yp(i3),xm(i3),ym(i3))
           endif
        enddo
c
        do i=1,ntf
            itnode(1,i)=c1(i)
            itnode(2,i)=c2(i)
            itnode(3,i)=0
            itnode(5,i)=c4(i)
        enddo
c
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
        subroutine gd7(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          vxp(499),vyp(499)
            character*80
     +          sp(100),su(100)
            save vxp,vyp,hmax,grade,ntf,nvf,ncf,nbf
c
c       domain in the shape of the north sea.
c       the domain contains 29 islands.
c       the skeleton has four subregions.
c
c       data for this domain was provided by gabriel wittum and
c       wolfgang hoffman from the university of stuttgart.
c
            data ntf,nvf,ncf,nbf/4,499,0,531/
            data hmax,grade/0.1d0,1.5d0/
c
            data vxp(  1),vyp(  1)/ 0.882d+00, 0.100d+01/
            data vxp(  2),vyp(  2)/ 0.803d+00, 0.100d+01/
            data vxp(  3),vyp(  3)/ 0.000d+00, 0.100d+01/
            data vxp(  4),vyp(  4)/ 0.000d+00, 0.000d+00/
            data vxp(  5),vyp(  5)/ 0.126d+00, 0.000d+00/
            data vxp(  6),vyp(  6)/ 0.351d+00, 0.000d+00/
            data vxp(  7),vyp(  7)/ 0.353d+00, 0.900d-02/
            data vxp(  8),vyp(  8)/ 0.381d+00, 0.120d-01/
            data vxp(  9),vyp(  9)/ 0.384d+00, 0.240d-01/
            data vxp( 10),vyp( 10)/ 0.378d+00, 0.300d-01/
            data vxp( 11),vyp( 11)/ 0.364d+00, 0.420d-01/
            data vxp( 12),vyp( 12)/ 0.370d+00, 0.520d-01/
            data vxp( 13),vyp( 13)/ 0.386d+00, 0.700d-01/
            data vxp( 14),vyp( 14)/ 0.395d+00, 0.820d-01/
            data vxp( 15),vyp( 15)/ 0.414d+00, 0.970d-01/
            data vxp( 16),vyp( 16)/ 0.427d+00, 0.106d+00/
            data vxp( 17),vyp( 17)/ 0.444d+00, 0.109d+00/
            data vxp( 18),vyp( 18)/ 0.460d+00, 0.112d+00/
            data vxp( 19),vyp( 19)/ 0.468d+00, 0.109d+00/
            data vxp( 20),vyp( 20)/ 0.471d+00, 0.112d+00/
            data vxp( 21),vyp( 21)/ 0.496d+00, 0.112d+00/
            data vxp( 22),vyp( 22)/ 0.504d+00, 0.106d+00/
            data vxp( 23),vyp( 23)/ 0.521d+00, 0.106d+00/
            data vxp( 24),vyp( 24)/ 0.532d+00, 0.109d+00/
            data vxp( 25),vyp( 25)/ 0.542d+00, 0.121d+00/
            data vxp( 26),vyp( 26)/ 0.553d+00, 0.124d+00/
            data vxp( 27),vyp( 27)/ 0.589d+00, 0.124d+00/
            data vxp( 28),vyp( 28)/ 0.600d+00, 0.127d+00/
            data vxp( 29),vyp( 29)/ 0.603d+00, 0.130d+00/
            data vxp( 30),vyp( 30)/ 0.619d+00, 0.127d+00/
            data vxp( 31),vyp( 31)/ 0.625d+00, 0.136d+00/
            data vxp( 32),vyp( 32)/ 0.652d+00, 0.136d+00/
            data vxp( 33),vyp( 33)/ 0.655d+00, 0.130d+00/
            data vxp( 34),vyp( 34)/ 0.668d+00, 0.127d+00/
            data vxp( 35),vyp( 35)/ 0.682d+00, 0.124d+00/
            data vxp( 36),vyp( 36)/ 0.677d+00, 0.115d+00/
            data vxp( 37),vyp( 37)/ 0.677d+00, 0.100d+00/
            data vxp( 38),vyp( 38)/ 0.682d+00, 0.880d-01/
            data vxp( 39),vyp( 39)/ 0.688d+00, 0.850d-01/
            data vxp( 40),vyp( 40)/ 0.699d+00, 0.820d-01/
            data vxp( 41),vyp( 41)/ 0.704d+00, 0.700d-01/
            data vxp( 42),vyp( 42)/ 0.712d+00, 0.550d-01/
            data vxp( 43),vyp( 43)/ 0.718d+00, 0.480d-01/
            data vxp( 44),vyp( 44)/ 0.723d+00, 0.330d-01/
            data vxp( 45),vyp( 45)/ 0.726d+00, 0.210d-01/
            data vxp( 46),vyp( 46)/ 0.721d+00, 0.150d-01/
            data vxp( 47),vyp( 47)/ 0.712d+00, 0.120d-01/
            data vxp( 48),vyp( 48)/ 0.704d+00, 0.000d+00/
            data vxp( 49),vyp( 49)/ 0.781d+00, 0.000d+00/
            data vxp( 50),vyp( 50)/ 0.781d+00, 0.900d-02/
            data vxp( 51),vyp( 51)/ 0.770d+00, 0.900d-02/
            data vxp( 52),vyp( 52)/ 0.756d+00, 0.120d-01/
            data vxp( 53),vyp( 53)/ 0.748d+00, 0.120d-01/
            data vxp( 54),vyp( 54)/ 0.748d+00, 0.240d-01/
            data vxp( 55),vyp( 55)/ 0.742d+00, 0.360d-01/
            data vxp( 56),vyp( 56)/ 0.753d+00, 0.450d-01/
            data vxp( 57),vyp( 57)/ 0.756d+00, 0.580d-01/
            data vxp( 58),vyp( 58)/ 0.764d+00, 0.670d-01/
            data vxp( 59),vyp( 59)/ 0.775d+00, 0.700d-01/
            data vxp( 60),vyp( 60)/ 0.784d+00, 0.640d-01/
            data vxp( 61),vyp( 61)/ 0.789d+00, 0.580d-01/
            data vxp( 62),vyp( 62)/ 0.797d+00, 0.480d-01/
            data vxp( 63),vyp( 63)/ 0.805d+00, 0.390d-01/
            data vxp( 64),vyp( 64)/ 0.819d+00, 0.330d-01/
            data vxp( 65),vyp( 65)/ 0.830d+00, 0.300d-01/
            data vxp( 66),vyp( 66)/ 0.844d+00, 0.240d-01/
            data vxp( 67),vyp( 67)/ 0.852d+00, 0.210d-01/
            data vxp( 68),vyp( 68)/ 0.849d+00, 0.150d-01/
            data vxp( 69),vyp( 69)/ 0.841d+00, 0.000d+00/
            data vxp( 70),vyp( 70)/ 0.860d+00, 0.000d+00/
            data vxp( 71),vyp( 71)/ 0.863d+00, 0.120d-01/
            data vxp( 72),vyp( 72)/ 0.863d+00, 0.180d-01/
            data vxp( 73),vyp( 73)/ 0.858d+00, 0.270d-01/
            data vxp( 74),vyp( 74)/ 0.852d+00, 0.450d-01/
            data vxp( 75),vyp( 75)/ 0.841d+00, 0.670d-01/
            data vxp( 76),vyp( 76)/ 0.833d+00, 0.790d-01/
            data vxp( 77),vyp( 77)/ 0.830d+00, 0.910d-01/
            data vxp( 78),vyp( 78)/ 0.830d+00, 0.103d+00/
            data vxp( 79),vyp( 79)/ 0.830d+00, 0.118d+00/
            data vxp( 80),vyp( 80)/ 0.836d+00, 0.133d+00/
            data vxp( 81),vyp( 81)/ 0.841d+00, 0.145d+00/
            data vxp( 82),vyp( 82)/ 0.847d+00, 0.158d+00/
            data vxp( 83),vyp( 83)/ 0.852d+00, 0.176d+00/
            data vxp( 84),vyp( 84)/ 0.852d+00, 0.194d+00/
            data vxp( 85),vyp( 85)/ 0.852d+00, 0.206d+00/
            data vxp( 86),vyp( 86)/ 0.863d+00, 0.215d+00/
            data vxp( 87),vyp( 87)/ 0.871d+00, 0.227d+00/
            data vxp( 88),vyp( 88)/ 0.877d+00, 0.233d+00/
            data vxp( 89),vyp( 89)/ 0.888d+00, 0.239d+00/
            data vxp( 90),vyp( 90)/ 0.896d+00, 0.239d+00/
            data vxp( 91),vyp( 91)/ 0.907d+00, 0.230d+00/
            data vxp( 92),vyp( 92)/ 0.915d+00, 0.221d+00/
            data vxp( 93),vyp( 93)/ 0.926d+00, 0.212d+00/
            data vxp( 94),vyp( 94)/ 0.932d+00, 0.197d+00/
            data vxp( 95),vyp( 95)/ 0.948d+00, 0.197d+00/
            data vxp( 96),vyp( 96)/ 0.959d+00, 0.200d+00/
            data vxp( 97),vyp( 97)/ 0.973d+00, 0.203d+00/
            data vxp( 98),vyp( 98)/ 0.984d+00, 0.206d+00/
            data vxp( 99),vyp( 99)/ 0.989d+00, 0.206d+00/
            data vxp(100),vyp(100)/ 0.100d+01, 0.206d+00/
            data vxp(101),vyp(101)/ 0.100d+01, 0.242d+00/
            data vxp(102),vyp(102)/ 0.989d+00, 0.248d+00/
            data vxp(103),vyp(103)/ 0.984d+00, 0.258d+00/
            data vxp(104),vyp(104)/ 0.978d+00, 0.258d+00/
            data vxp(105),vyp(105)/ 0.973d+00, 0.267d+00/
            data vxp(106),vyp(106)/ 0.970d+00, 0.270d+00/
            data vxp(107),vyp(107)/ 0.970d+00, 0.276d+00/
            data vxp(108),vyp(108)/ 0.970d+00, 0.282d+00/
            data vxp(109),vyp(109)/ 0.964d+00, 0.288d+00/
            data vxp(110),vyp(110)/ 0.964d+00, 0.297d+00/
            data vxp(111),vyp(111)/ 0.959d+00, 0.303d+00/
            data vxp(112),vyp(112)/ 0.953d+00, 0.312d+00/
            data vxp(113),vyp(113)/ 0.951d+00, 0.321d+00/
            data vxp(114),vyp(114)/ 0.948d+00, 0.327d+00/
            data vxp(115),vyp(115)/ 0.953d+00, 0.333d+00/
            data vxp(116),vyp(116)/ 0.967d+00, 0.336d+00/
            data vxp(117),vyp(117)/ 0.973d+00, 0.333d+00/
            data vxp(118),vyp(118)/ 0.978d+00, 0.330d+00/
            data vxp(119),vyp(119)/ 0.984d+00, 0.327d+00/
            data vxp(120),vyp(120)/ 0.986d+00, 0.330d+00/
            data vxp(121),vyp(121)/ 0.992d+00, 0.333d+00/
            data vxp(122),vyp(122)/ 0.995d+00, 0.336d+00/
            data vxp(123),vyp(123)/ 0.992d+00, 0.358d+00/
            data vxp(124),vyp(124)/ 0.986d+00, 0.364d+00/
            data vxp(125),vyp(125)/ 0.986d+00, 0.370d+00/
            data vxp(126),vyp(126)/ 0.984d+00, 0.376d+00/
            data vxp(127),vyp(127)/ 0.981d+00, 0.379d+00/
            data vxp(128),vyp(128)/ 0.978d+00, 0.388d+00/
            data vxp(129),vyp(129)/ 0.978d+00, 0.394d+00/
            data vxp(130),vyp(130)/ 0.970d+00, 0.394d+00/
            data vxp(131),vyp(131)/ 0.964d+00, 0.391d+00/
            data vxp(132),vyp(132)/ 0.956d+00, 0.391d+00/
            data vxp(133),vyp(133)/ 0.951d+00, 0.394d+00/
            data vxp(134),vyp(134)/ 0.948d+00, 0.397d+00/
            data vxp(135),vyp(135)/ 0.945d+00, 0.403d+00/
            data vxp(136),vyp(136)/ 0.942d+00, 0.409d+00/
            data vxp(137),vyp(137)/ 0.942d+00, 0.415d+00/
            data vxp(138),vyp(138)/ 0.940d+00, 0.418d+00/
            data vxp(139),vyp(139)/ 0.940d+00, 0.430d+00/
            data vxp(140),vyp(140)/ 0.942d+00, 0.439d+00/
            data vxp(141),vyp(141)/ 0.948d+00, 0.445d+00/
            data vxp(142),vyp(142)/ 0.948d+00, 0.461d+00/
            data vxp(143),vyp(143)/ 0.948d+00, 0.470d+00/
            data vxp(144),vyp(144)/ 0.948d+00, 0.488d+00/
            data vxp(145),vyp(145)/ 0.942d+00, 0.491d+00/
            data vxp(146),vyp(146)/ 0.937d+00, 0.491d+00/
            data vxp(147),vyp(147)/ 0.932d+00, 0.488d+00/
            data vxp(148),vyp(148)/ 0.926d+00, 0.488d+00/
            data vxp(149),vyp(149)/ 0.923d+00, 0.491d+00/
            data vxp(150),vyp(150)/ 0.915d+00, 0.494d+00/
            data vxp(151),vyp(151)/ 0.910d+00, 0.494d+00/
            data vxp(152),vyp(152)/ 0.901d+00, 0.491d+00/
            data vxp(153),vyp(153)/ 0.899d+00, 0.488d+00/
            data vxp(154),vyp(154)/ 0.893d+00, 0.479d+00/
            data vxp(155),vyp(155)/ 0.888d+00, 0.479d+00/
            data vxp(156),vyp(156)/ 0.879d+00, 0.485d+00/
            data vxp(157),vyp(157)/ 0.871d+00, 0.494d+00/
            data vxp(158),vyp(158)/ 0.868d+00, 0.506d+00/
            data vxp(159),vyp(159)/ 0.866d+00, 0.518d+00/
            data vxp(160),vyp(160)/ 0.866d+00, 0.524d+00/
            data vxp(161),vyp(161)/ 0.877d+00, 0.524d+00/
            data vxp(162),vyp(162)/ 0.882d+00, 0.527d+00/
            data vxp(163),vyp(163)/ 0.893d+00, 0.530d+00/
            data vxp(164),vyp(164)/ 0.896d+00, 0.533d+00/
            data vxp(165),vyp(165)/ 0.893d+00, 0.539d+00/
            data vxp(166),vyp(166)/ 0.888d+00, 0.542d+00/
            data vxp(167),vyp(167)/ 0.882d+00, 0.542d+00/
            data vxp(168),vyp(168)/ 0.879d+00, 0.552d+00/
            data vxp(169),vyp(169)/ 0.888d+00, 0.558d+00/
            data vxp(170),vyp(170)/ 0.896d+00, 0.564d+00/
            data vxp(171),vyp(171)/ 0.907d+00, 0.564d+00/
            data vxp(172),vyp(172)/ 0.921d+00, 0.567d+00/
            data vxp(173),vyp(173)/ 0.929d+00, 0.570d+00/
            data vxp(174),vyp(174)/ 0.937d+00, 0.573d+00/
            data vxp(175),vyp(175)/ 0.945d+00, 0.567d+00/
            data vxp(176),vyp(176)/ 0.953d+00, 0.567d+00/
            data vxp(177),vyp(177)/ 0.962d+00, 0.567d+00/
            data vxp(178),vyp(178)/ 0.967d+00, 0.573d+00/
            data vxp(179),vyp(179)/ 0.978d+00, 0.588d+00/
            data vxp(180),vyp(180)/ 0.984d+00, 0.591d+00/
            data vxp(181),vyp(181)/ 0.989d+00, 0.597d+00/
            data vxp(182),vyp(182)/ 0.992d+00, 0.597d+00/
            data vxp(183),vyp(183)/ 0.100d+01, 0.594d+00/
            data vxp(184),vyp(184)/ 0.100d+01, 0.642d+00/
            data vxp(185),vyp(185)/ 0.986d+00, 0.652d+00/
            data vxp(186),vyp(186)/ 0.981d+00, 0.661d+00/
            data vxp(187),vyp(187)/ 0.973d+00, 0.664d+00/
            data vxp(188),vyp(188)/ 0.970d+00, 0.670d+00/
            data vxp(189),vyp(189)/ 0.964d+00, 0.685d+00/
            data vxp(190),vyp(190)/ 0.964d+00, 0.691d+00/
            data vxp(191),vyp(191)/ 0.956d+00, 0.691d+00/
            data vxp(192),vyp(192)/ 0.945d+00, 0.691d+00/
            data vxp(193),vyp(193)/ 0.951d+00, 0.697d+00/
            data vxp(194),vyp(194)/ 0.948d+00, 0.709d+00/
            data vxp(195),vyp(195)/ 0.942d+00, 0.721d+00/
            data vxp(196),vyp(196)/ 0.940d+00, 0.724d+00/
            data vxp(197),vyp(197)/ 0.932d+00, 0.730d+00/
            data vxp(198),vyp(198)/ 0.921d+00, 0.739d+00/
            data vxp(199),vyp(199)/ 0.918d+00, 0.758d+00/
            data vxp(200),vyp(200)/ 0.899d+00, 0.767d+00/
            data vxp(201),vyp(201)/ 0.899d+00, 0.773d+00/
            data vxp(202),vyp(202)/ 0.904d+00, 0.779d+00/
            data vxp(203),vyp(203)/ 0.901d+00, 0.788d+00/
            data vxp(204),vyp(204)/ 0.901d+00, 0.797d+00/
            data vxp(205),vyp(205)/ 0.896d+00, 0.812d+00/
            data vxp(206),vyp(206)/ 0.888d+00, 0.815d+00/
            data vxp(207),vyp(207)/ 0.888d+00, 0.824d+00/
            data vxp(208),vyp(208)/ 0.874d+00, 0.842d+00/
            data vxp(209),vyp(209)/ 0.871d+00, 0.848d+00/
            data vxp(210),vyp(210)/ 0.874d+00, 0.852d+00/
            data vxp(211),vyp(211)/ 0.874d+00, 0.870d+00/
            data vxp(212),vyp(212)/ 0.882d+00, 0.873d+00/
            data vxp(213),vyp(213)/ 0.885d+00, 0.879d+00/
            data vxp(214),vyp(214)/ 0.885d+00, 0.885d+00/
            data vxp(215),vyp(215)/ 0.890d+00, 0.885d+00/
            data vxp(216),vyp(216)/ 0.888d+00, 0.915d+00/
            data vxp(217),vyp(217)/ 0.893d+00, 0.915d+00/
            data vxp(218),vyp(218)/ 0.893d+00, 0.930d+00/
            data vxp(219),vyp(219)/ 0.877d+00, 0.961d+00/
            data vxp(220),vyp(220)/ 0.879d+00, 0.970d+00/
            data vxp(221),vyp(221)/ 0.882d+00, 0.970d+00/
            data vxp(222),vyp(222)/ 0.901d+00, 0.655d+00/
            data vxp(223),vyp(223)/ 0.901d+00, 0.642d+00/
            data vxp(224),vyp(224)/ 0.893d+00, 0.621d+00/
            data vxp(225),vyp(225)/ 0.885d+00, 0.618d+00/
            data vxp(226),vyp(226)/ 0.874d+00, 0.618d+00/
            data vxp(227),vyp(227)/ 0.866d+00, 0.630d+00/
            data vxp(228),vyp(228)/ 0.866d+00, 0.648d+00/
            data vxp(229),vyp(229)/ 0.877d+00, 0.648d+00/
            data vxp(230),vyp(230)/ 0.882d+00, 0.658d+00/
            data vxp(231),vyp(231)/ 0.901d+00, 0.661d+00/
            data vxp(232),vyp(232)/ 0.896d+00, 0.358d+00/
            data vxp(233),vyp(233)/ 0.896d+00, 0.345d+00/
            data vxp(234),vyp(234)/ 0.901d+00, 0.333d+00/
            data vxp(235),vyp(235)/ 0.890d+00, 0.352d+00/
            data vxp(236),vyp(236)/ 0.819d+00, 0.285d+00/
            data vxp(237),vyp(237)/ 0.808d+00, 0.276d+00/
            data vxp(238),vyp(238)/ 0.805d+00, 0.282d+00/
            data vxp(239),vyp(239)/ 0.814d+00, 0.291d+00/
            data vxp(240),vyp(240)/ 0.841d+00, 0.264d+00/
            data vxp(241),vyp(241)/ 0.841d+00, 0.258d+00/
            data vxp(242),vyp(242)/ 0.830d+00, 0.255d+00/
            data vxp(243),vyp(243)/ 0.833d+00, 0.267d+00/
            data vxp(244),vyp(244)/ 0.789d+00, 0.209d+00/
            data vxp(245),vyp(245)/ 0.797d+00, 0.209d+00/
            data vxp(246),vyp(246)/ 0.795d+00, 0.206d+00/
            data vxp(247),vyp(247)/ 0.795d+00, 0.197d+00/
            data vxp(248),vyp(248)/ 0.784d+00, 0.200d+00/
            data vxp(249),vyp(249)/ 0.789d+00, 0.212d+00/
            data vxp(250),vyp(250)/ 0.726d+00, 0.142d+00/
            data vxp(251),vyp(251)/ 0.729d+00, 0.136d+00/
            data vxp(252),vyp(252)/ 0.721d+00, 0.136d+00/
            data vxp(253),vyp(253)/ 0.718d+00, 0.127d+00/
            data vxp(254),vyp(254)/ 0.710d+00, 0.127d+00/
            data vxp(255),vyp(255)/ 0.710d+00, 0.136d+00/
            data vxp(256),vyp(256)/ 0.721d+00, 0.145d+00/
            data vxp(257),vyp(257)/ 0.636d+00, 0.179d+00/
            data vxp(258),vyp(258)/ 0.655d+00, 0.173d+00/
            data vxp(259),vyp(259)/ 0.633d+00, 0.176d+00/
            data vxp(260),vyp(260)/ 0.622d+00, 0.170d+00/
            data vxp(261),vyp(261)/ 0.616d+00, 0.179d+00/
            data vxp(262),vyp(262)/ 0.625d+00, 0.182d+00/
            data vxp(263),vyp(263)/ 0.592d+00, 0.173d+00/
            data vxp(264),vyp(264)/ 0.605d+00, 0.170d+00/
            data vxp(265),vyp(265)/ 0.584d+00, 0.158d+00/
            data vxp(266),vyp(266)/ 0.575d+00, 0.158d+00/
            data vxp(267),vyp(267)/ 0.573d+00, 0.161d+00/
            data vxp(268),vyp(268)/ 0.564d+00, 0.158d+00/
            data vxp(269),vyp(269)/ 0.562d+00, 0.155d+00/
            data vxp(270),vyp(270)/ 0.559d+00, 0.155d+00/
            data vxp(271),vyp(271)/ 0.559d+00, 0.161d+00/
            data vxp(272),vyp(272)/ 0.567d+00, 0.173d+00/
            data vxp(273),vyp(273)/ 0.490d+00, 0.145d+00/
            data vxp(274),vyp(274)/ 0.496d+00, 0.155d+00/
            data vxp(275),vyp(275)/ 0.537d+00, 0.158d+00/
            data vxp(276),vyp(276)/ 0.542d+00, 0.148d+00/
            data vxp(277),vyp(277)/ 0.537d+00, 0.148d+00/
            data vxp(278),vyp(278)/ 0.529d+00, 0.152d+00/
            data vxp(279),vyp(279)/ 0.518d+00, 0.152d+00/
            data vxp(280),vyp(280)/ 0.518d+00, 0.148d+00/
            data vxp(281),vyp(281)/ 0.504d+00, 0.152d+00/
            data vxp(282),vyp(282)/ 0.504d+00, 0.139d+00/
            data vxp(283),vyp(283)/ 0.490d+00, 0.136d+00/
            data vxp(284),vyp(284)/ 0.477d+00, 0.142d+00/
            data vxp(285),vyp(285)/ 0.482d+00, 0.133d+00/
            data vxp(286),vyp(286)/ 0.468d+00, 0.133d+00/
            data vxp(287),vyp(287)/ 0.460d+00, 0.136d+00/
            data vxp(288),vyp(288)/ 0.460d+00, 0.145d+00/
            data vxp(289),vyp(289)/ 0.447d+00, 0.139d+00/
            data vxp(290),vyp(290)/ 0.444d+00, 0.133d+00/
            data vxp(291),vyp(291)/ 0.430d+00, 0.130d+00/
            data vxp(292),vyp(292)/ 0.403d+00, 0.127d+00/
            data vxp(293),vyp(293)/ 0.397d+00, 0.121d+00/
            data vxp(294),vyp(294)/ 0.389d+00, 0.124d+00/
            data vxp(295),vyp(295)/ 0.381d+00, 0.124d+00/
            data vxp(296),vyp(296)/ 0.395d+00, 0.139d+00/
            data vxp(297),vyp(297)/ 0.315d+00, 0.112d+00/
            data vxp(298),vyp(298)/ 0.337d+00, 0.115d+00/
            data vxp(299),vyp(299)/ 0.364d+00, 0.115d+00/
            data vxp(300),vyp(300)/ 0.351d+00, 0.109d+00/
            data vxp(301),vyp(301)/ 0.332d+00, 0.103d+00/
            data vxp(302),vyp(302)/ 0.315d+00, 0.100d+00/
            data vxp(303),vyp(303)/ 0.296d+00, 0.100d+00/
            data vxp(304),vyp(304)/ 0.293d+00, 0.109d+00/
            data vxp(305),vyp(305)/ 0.304d+00, 0.940d-01/
            data vxp(306),vyp(306)/ 0.310d+00, 0.850d-01/
            data vxp(307),vyp(307)/ 0.304d+00, 0.760d-01/
            data vxp(308),vyp(308)/ 0.296d+00, 0.760d-01/
            data vxp(309),vyp(309)/ 0.293d+00, 0.820d-01/
            data vxp(310),vyp(310)/ 0.290d+00, 0.910d-01/
            data vxp(311),vyp(311)/ 0.230d+00, 0.610d-01/
            data vxp(312),vyp(312)/ 0.249d+00, 0.730d-01/
            data vxp(313),vyp(313)/ 0.260d+00, 0.730d-01/
            data vxp(314),vyp(314)/ 0.266d+00, 0.640d-01/
            data vxp(315),vyp(315)/ 0.271d+00, 0.610d-01/
            data vxp(316),vyp(316)/ 0.271d+00, 0.580d-01/
            data vxp(317),vyp(317)/ 0.249d+00, 0.550d-01/
            data vxp(318),vyp(318)/ 0.247d+00, 0.480d-01/
            data vxp(319),vyp(319)/ 0.241d+00, 0.480d-01/
            data vxp(320),vyp(320)/ 0.249d+00, 0.390d-01/
            data vxp(321),vyp(321)/ 0.233d+00, 0.390d-01/
            data vxp(322),vyp(322)/ 0.222d+00, 0.520d-01/
            data vxp(323),vyp(323)/ 0.222d+00, 0.610d-01/
            data vxp(324),vyp(324)/ 0.203d+00, 0.240d-01/
            data vxp(325),vyp(325)/ 0.200d+00, 0.210d-01/
            data vxp(326),vyp(326)/ 0.195d+00, 0.120d-01/
            data vxp(327),vyp(327)/ 0.192d+00, 0.120d-01/
            data vxp(328),vyp(328)/ 0.192d+00, 0.300d-01/
            data vxp(329),vyp(329)/ 0.197d+00, 0.300d-01/
            data vxp(330),vyp(330)/ 0.203d+00, 0.300d-01/
            data vxp(331),vyp(331)/ 0.153d+00, 0.300d-01/
            data vxp(332),vyp(332)/ 0.156d+00, 0.270d-01/
            data vxp(333),vyp(333)/ 0.170d+00, 0.270d-01/
            data vxp(334),vyp(334)/ 0.175d+00, 0.180d-01/
            data vxp(335),vyp(335)/ 0.156d+00, 0.180d-01/
            data vxp(336),vyp(336)/ 0.153d+00, 0.210d-01/
            data vxp(337),vyp(337)/ 0.145d+00, 0.180d-01/
            data vxp(338),vyp(338)/ 0.145d+00, 0.240d-01/
            data vxp(339),vyp(339)/ 0.148d+00, 0.300d-01/
            data vxp(340),vyp(340)/ 0.137d+00, 0.150d-01/
            data vxp(341),vyp(341)/ 0.137d+00, 0.900d-02/
            data vxp(342),vyp(342)/ 0.134d+00, 0.900d-02/
            data vxp(343),vyp(343)/ 0.134d+00, 0.600d-02/
            data vxp(344),vyp(344)/ 0.126d+00, 0.600d-02/
            data vxp(345),vyp(345)/ 0.123d+00, 0.150d-01/
            data vxp(346),vyp(346)/ 0.638d+00, 0.427d+00/
            data vxp(347),vyp(347)/ 0.628d+00, 0.422d+00/
            data vxp(348),vyp(348)/ 0.627d+00, 0.421d+00/
            data vxp(349),vyp(349)/ 0.627d+00, 0.418d+00/
            data vxp(350),vyp(350)/ 0.625d+00, 0.418d+00/
            data vxp(351),vyp(351)/ 0.633d+00, 0.433d+00/
            data vxp(352),vyp(352)/ 0.633d+00, 0.427d+00/
            data vxp(353),vyp(353)/ 0.970d+00, 0.639d+00/
            data vxp(354),vyp(354)/ 0.975d+00, 0.639d+00/
            data vxp(355),vyp(355)/ 0.986d+00, 0.636d+00/
            data vxp(356),vyp(356)/ 0.984d+00, 0.621d+00/
            data vxp(357),vyp(357)/ 0.981d+00, 0.618d+00/
            data vxp(358),vyp(358)/ 0.975d+00, 0.606d+00/
            data vxp(359),vyp(359)/ 0.967d+00, 0.600d+00/
            data vxp(360),vyp(360)/ 0.948d+00, 0.600d+00/
            data vxp(361),vyp(361)/ 0.942d+00, 0.603d+00/
            data vxp(362),vyp(362)/ 0.940d+00, 0.606d+00/
            data vxp(363),vyp(363)/ 0.942d+00, 0.615d+00/
            data vxp(364),vyp(364)/ 0.940d+00, 0.624d+00/
            data vxp(365),vyp(365)/ 0.942d+00, 0.627d+00/
            data vxp(366),vyp(366)/ 0.951d+00, 0.630d+00/
            data vxp(367),vyp(367)/ 0.956d+00, 0.639d+00/
            data vxp(368),vyp(368)/ 0.964d+00, 0.639d+00/
            data vxp(369),vyp(369)/ 0.858d+00, 0.600d+00/
            data vxp(370),vyp(370)/ 0.852d+00, 0.597d+00/
            data vxp(371),vyp(371)/ 0.852d+00, 0.606d+00/
            data vxp(372),vyp(372)/ 0.858d+00, 0.606d+00/
            data vxp(373),vyp(373)/ 0.825d+00, 0.600d+00/
            data vxp(374),vyp(374)/ 0.825d+00, 0.594d+00/
            data vxp(375),vyp(375)/ 0.830d+00, 0.594d+00/
            data vxp(376),vyp(376)/ 0.833d+00, 0.588d+00/
            data vxp(377),vyp(377)/ 0.833d+00, 0.582d+00/
            data vxp(378),vyp(378)/ 0.830d+00, 0.573d+00/
            data vxp(379),vyp(379)/ 0.825d+00, 0.576d+00/
            data vxp(380),vyp(380)/ 0.814d+00, 0.588d+00/
            data vxp(381),vyp(381)/ 0.819d+00, 0.600d+00/
            data vxp(382),vyp(382)/ 0.827d+00, 0.603d+00/
            data vxp(383),vyp(383)/ 0.830d+00, 0.645d+00/
            data vxp(384),vyp(384)/ 0.830d+00, 0.630d+00/
            data vxp(385),vyp(385)/ 0.825d+00, 0.624d+00/
            data vxp(386),vyp(386)/ 0.819d+00, 0.633d+00/
            data vxp(387),vyp(387)/ 0.822d+00, 0.648d+00/
            data vxp(388),vyp(388)/ 0.855d+00, 0.670d+00/
            data vxp(389),vyp(389)/ 0.858d+00, 0.667d+00/
            data vxp(390),vyp(390)/ 0.858d+00, 0.664d+00/
            data vxp(391),vyp(391)/ 0.849d+00, 0.661d+00/
            data vxp(392),vyp(392)/ 0.847d+00, 0.667d+00/
            data vxp(393),vyp(393)/ 0.838d+00, 0.673d+00/
            data vxp(394),vyp(394)/ 0.822d+00, 0.664d+00/
            data vxp(395),vyp(395)/ 0.814d+00, 0.664d+00/
            data vxp(396),vyp(396)/ 0.819d+00, 0.679d+00/
            data vxp(397),vyp(397)/ 0.888d+00, 0.721d+00/
            data vxp(398),vyp(398)/ 0.888d+00, 0.715d+00/
            data vxp(399),vyp(399)/ 0.882d+00, 0.712d+00/
            data vxp(400),vyp(400)/ 0.877d+00, 0.709d+00/
            data vxp(401),vyp(401)/ 0.866d+00, 0.709d+00/
            data vxp(402),vyp(402)/ 0.858d+00, 0.706d+00/
            data vxp(403),vyp(403)/ 0.852d+00, 0.700d+00/
            data vxp(404),vyp(404)/ 0.847d+00, 0.703d+00/
            data vxp(405),vyp(405)/ 0.844d+00, 0.709d+00/
            data vxp(406),vyp(406)/ 0.849d+00, 0.715d+00/
            data vxp(407),vyp(407)/ 0.858d+00, 0.715d+00/
            data vxp(408),vyp(408)/ 0.866d+00, 0.718d+00/
            data vxp(409),vyp(409)/ 0.871d+00, 0.718d+00/
            data vxp(410),vyp(410)/ 0.879d+00, 0.721d+00/
            data vxp(411),vyp(411)/ 0.890d+00, 0.724d+00/
            data vxp(412),vyp(412)/ 0.907d+00, 0.733d+00/
            data vxp(413),vyp(413)/ 0.893d+00, 0.736d+00/
            data vxp(414),vyp(414)/ 0.907d+00, 0.742d+00/
            data vxp(415),vyp(415)/ 0.784d+00, 0.748d+00/
            data vxp(416),vyp(416)/ 0.781d+00, 0.739d+00/
            data vxp(417),vyp(417)/ 0.786d+00, 0.730d+00/
            data vxp(418),vyp(418)/ 0.795d+00, 0.715d+00/
            data vxp(419),vyp(419)/ 0.795d+00, 0.706d+00/
            data vxp(420),vyp(420)/ 0.800d+00, 0.706d+00/
            data vxp(421),vyp(421)/ 0.800d+00, 0.700d+00/
            data vxp(422),vyp(422)/ 0.792d+00, 0.700d+00/
            data vxp(423),vyp(423)/ 0.789d+00, 0.697d+00/
            data vxp(424),vyp(424)/ 0.781d+00, 0.703d+00/
            data vxp(425),vyp(425)/ 0.775d+00, 0.709d+00/
            data vxp(426),vyp(426)/ 0.770d+00, 0.718d+00/
            data vxp(427),vyp(427)/ 0.767d+00, 0.727d+00/
            data vxp(428),vyp(428)/ 0.770d+00, 0.730d+00/
            data vxp(429),vyp(429)/ 0.767d+00, 0.739d+00/
            data vxp(430),vyp(430)/ 0.789d+00, 0.761d+00/
            data vxp(431),vyp(431)/ 0.836d+00, 0.782d+00/
            data vxp(432),vyp(432)/ 0.841d+00, 0.782d+00/
            data vxp(433),vyp(433)/ 0.844d+00, 0.788d+00/
            data vxp(434),vyp(434)/ 0.852d+00, 0.788d+00/
            data vxp(435),vyp(435)/ 0.855d+00, 0.785d+00/
            data vxp(436),vyp(436)/ 0.860d+00, 0.785d+00/
            data vxp(437),vyp(437)/ 0.863d+00, 0.779d+00/
            data vxp(438),vyp(438)/ 0.860d+00, 0.773d+00/
            data vxp(439),vyp(439)/ 0.863d+00, 0.767d+00/
            data vxp(440),vyp(440)/ 0.866d+00, 0.764d+00/
            data vxp(441),vyp(441)/ 0.860d+00, 0.761d+00/
            data vxp(442),vyp(442)/ 0.858d+00, 0.752d+00/
            data vxp(443),vyp(443)/ 0.858d+00, 0.739d+00/
            data vxp(444),vyp(444)/ 0.822d+00, 0.739d+00/
            data vxp(445),vyp(445)/ 0.816d+00, 0.748d+00/
            data vxp(446),vyp(446)/ 0.811d+00, 0.748d+00/
            data vxp(447),vyp(447)/ 0.803d+00, 0.752d+00/
            data vxp(448),vyp(448)/ 0.803d+00, 0.764d+00/
            data vxp(449),vyp(449)/ 0.803d+00, 0.773d+00/
            data vxp(450),vyp(450)/ 0.808d+00, 0.782d+00/
            data vxp(451),vyp(451)/ 0.816d+00, 0.785d+00/
            data vxp(452),vyp(452)/ 0.836d+00, 0.785d+00/
            data vxp(453),vyp(453)/ 0.822d+00, 0.973d+00/
            data vxp(454),vyp(454)/ 0.808d+00, 0.970d+00/
            data vxp(455),vyp(455)/ 0.805d+00, 0.964d+00/
            data vxp(456),vyp(456)/ 0.814d+00, 0.961d+00/
            data vxp(457),vyp(457)/ 0.814d+00, 0.955d+00/
            data vxp(458),vyp(458)/ 0.808d+00, 0.952d+00/
            data vxp(459),vyp(459)/ 0.800d+00, 0.945d+00/
            data vxp(460),vyp(460)/ 0.792d+00, 0.936d+00/
            data vxp(461),vyp(461)/ 0.789d+00, 0.927d+00/
            data vxp(462),vyp(462)/ 0.786d+00, 0.921d+00/
            data vxp(463),vyp(463)/ 0.786d+00, 0.903d+00/
            data vxp(464),vyp(464)/ 0.789d+00, 0.885d+00/
            data vxp(465),vyp(465)/ 0.795d+00, 0.876d+00/
            data vxp(466),vyp(466)/ 0.803d+00, 0.870d+00/
            data vxp(467),vyp(467)/ 0.814d+00, 0.864d+00/
            data vxp(468),vyp(468)/ 0.822d+00, 0.864d+00/
            data vxp(469),vyp(469)/ 0.830d+00, 0.864d+00/
            data vxp(470),vyp(470)/ 0.819d+00, 0.855d+00/
            data vxp(471),vyp(471)/ 0.811d+00, 0.852d+00/
            data vxp(472),vyp(472)/ 0.803d+00, 0.845d+00/
            data vxp(473),vyp(473)/ 0.795d+00, 0.852d+00/
            data vxp(474),vyp(474)/ 0.792d+00, 0.858d+00/
            data vxp(475),vyp(475)/ 0.784d+00, 0.867d+00/
            data vxp(476),vyp(476)/ 0.781d+00, 0.864d+00/
            data vxp(477),vyp(477)/ 0.781d+00, 0.855d+00/
            data vxp(478),vyp(478)/ 0.773d+00, 0.852d+00/
            data vxp(479),vyp(479)/ 0.770d+00, 0.848d+00/
            data vxp(480),vyp(480)/ 0.770d+00, 0.833d+00/
            data vxp(481),vyp(481)/ 0.770d+00, 0.818d+00/
            data vxp(482),vyp(482)/ 0.767d+00, 0.809d+00/
            data vxp(483),vyp(483)/ 0.767d+00, 0.803d+00/
            data vxp(484),vyp(484)/ 0.770d+00, 0.803d+00/
            data vxp(485),vyp(485)/ 0.767d+00, 0.788d+00/
            data vxp(486),vyp(486)/ 0.764d+00, 0.776d+00/
            data vxp(487),vyp(487)/ 0.759d+00, 0.785d+00/
            data vxp(488),vyp(488)/ 0.762d+00, 0.806d+00/
            data vxp(489),vyp(489)/ 0.762d+00, 0.812d+00/
            data vxp(490),vyp(490)/ 0.764d+00, 0.870d+00/
            data vxp(491),vyp(491)/ 0.767d+00, 0.885d+00/
            data vxp(492),vyp(492)/ 0.773d+00, 0.906d+00/
            data vxp(493),vyp(493)/ 0.781d+00, 0.933d+00/
            data vxp(494),vyp(494)/ 0.795d+00, 0.964d+00/
            data vxp(495),vyp(495)/ 0.803d+00, 0.979d+00/
            data vxp(496),vyp(496)/ 0.814d+00, 0.979d+00/
            data vxp(497),vyp(497)/ 0.814d+00, 0.976d+00/
            data vxp(498),vyp(498)/ 0.822d+00, 0.976d+00/
            data vxp(499),vyp(499)/ 0.825d+00, 0.973d+00/
c
c
        sp(2)='north sea'
        sp(1)='north sea'
        sp(3)='north sea'
        sp(4)='north sea'
c
        do i=1,nvf
            vx(i)=vxp(i)
            vy(i)=vyp(i)
        enddo
        do i=1,499
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            ibndry(4,i)=2
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
        ibndry(2,221)=1
        ibndry(2,231)=222
        ibndry(2,235)=232
        ibndry(2,239)=236
        ibndry(2,243)=240
        ibndry(2,249)=244
        ibndry(2,256)=250
        ibndry(2,262)=257
        ibndry(2,272)=263
        ibndry(2,283)=273
        ibndry(2,288)=284
        ibndry(2,296)=289
        ibndry(2,304)=297
        ibndry(2,310)=305
        ibndry(2,323)=311
        ibndry(2,330)=324
        ibndry(2,339)=331
        ibndry(2,345)=340
        ibndry(2,352)=346
        ibndry(2,368)=353
        ibndry(2,372)=369
        ibndry(2,382)=373
        ibndry(2,387)=383
        ibndry(2,393)=388
        ibndry(2,396)=394
        ibndry(2,411)=397
        ibndry(2,414)=412
        ibndry(2,430)=415
        ibndry(2,452)=431
        ibndry(2,499)=453
        do i=500,531
            ibndry(3,i)=0
            ibndry(4,i)=0
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
        ibndry(1,500)=5
        ibndry(2,500)=344
        ibndry(1,501)=340
        ibndry(2,501)=337
        ibndry(1,502)=334
        ibndry(2,502)=327
        ibndry(1,503)=330
        ibndry(2,503)=321
        ibndry(1,504)=313
        ibndry(2,504)=308
        ibndry(1,505)=305
        ibndry(2,505)=303
        ibndry(1,506)=299
        ibndry(2,506)=295
        ibndry(1,507)=289
        ibndry(2,507)=287
        ibndry(1,508)=285
        ibndry(2,508)=283
        ibndry(1,509)=276
        ibndry(2,509)=270
        ibndry(1,510)=264
        ibndry(2,510)=261
        ibndry(1,511)=258
        ibndry(2,511)=255
        ibndry(1,512)=250
        ibndry(2,512)=248
        ibndry(1,513)=245
        ibndry(2,513)=242
        ibndry(1,514)=243
        ibndry(2,514)=236
        ibndry(1,515)=238
        ibndry(2,515)=349
        ibndry(1,516)=239
        ibndry(2,516)=235
        ibndry(1,517)=234
        ibndry(2,517)=114
        ibndry(1,518)=355
        ibndry(2,518)=184
        ibndry(1,519)=362
        ibndry(2,519)=224
        ibndry(1,520)=226
        ibndry(2,520)=372
        ibndry(1,521)=370
        ibndry(2,521)=376
        ibndry(1,522)=382
        ibndry(2,522)=385
        ibndry(1,523)=387
        ibndry(2,523)=395
        ibndry(1,524)=396
        ibndry(2,524)=393
        ibndry(1,525)=388
        ibndry(2,525)=403
        ibndry(1,526)=411
        ibndry(2,526)=412
        ibndry(1,527)=413
        ibndry(2,527)=443
        ibndry(1,528)=448
        ibndry(2,528)=430
        ibndry(1,529)=429
        ibndry(2,529)=486
        ibndry(1,530)=2
        ibndry(2,530)=495
        ibndry(1,531)=351
        ibndry(2,531)=379
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
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gd8(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*)
     1          ,x(242),y(242)
            character*80
     +          sp(100),su(100)
            save nvf,ntf,nbf,ncf,x,y
c
c       domain in the shape of a three piece airfoil
c       the outer boundary is an irregular polygon
c       data for this domain was provided by tony chan from ucla
c
            data nvf,ntf,nbf,ncf/242,2,246,0/
            data hmax,grade/0.1d0,1.5d0/
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
            data x(194),y(194)/ 0.00000000d+00, 0.28819495d+00/
            data x(195),y(195)/ 0.43966208d-01, 0.25921881d+00/
            data x(196),y(196)/ 0.67964263d-01, 0.17930219d+00/
            data x(197),y(197)/ 0.13709681d-01, 0.11062408d+00/
            data x(198),y(198)/ 0.45169536d-01, 0.25816517d-01/
            data x(199),y(199)/ 0.12964404d+00, 0.21003967d-01/
            data x(200),y(200)/ 0.23699667d+00, 0.32177549d-01/
            data x(201),y(201)/ 0.35846743d+00, 0.00000000d+00/
            data x(202),y(202)/ 0.49931961d+00, 0.41386276d-01/
            data x(203),y(203)/ 0.54918855d+00, 0.44396643d-01/
            data x(204),y(204)/ 0.62591928d+00, 0.65661035d-01/
            data x(205),y(205)/ 0.70021564d+00, 0.22311412d-01/
            data x(206),y(206)/ 0.80595642d+00, 0.13935482d-01/
            data x(207),y(207)/ 0.90486634d+00, 0.58995541d-01/
            data x(208),y(208)/ 0.97141540d+00, 0.14212923d+00/
            data x(209),y(209)/ 0.98587966d+00, 0.22880019d+00/
            data x(210),y(210)/ 0.97171891d+00, 0.32607010d+00/
            data x(211),y(211)/ 0.96469915d+00, 0.37979934d+00/
            data x(212),y(212)/ 0.94215643d+00, 0.42545268d+00/
            data x(213),y(213)/ 0.97760570d+00, 0.47650883d+00/
            data x(214),y(214)/ 0.98377573d+00, 0.52510858d+00/
            data x(215),y(215)/ 0.97785521d+00, 0.56944507d+00/
            data x(216),y(216)/ 0.94749039d+00, 0.61905968d+00/
            data x(217),y(217)/ 0.96022832d+00, 0.68079990d+00/
            data x(218),y(218)/ 0.10000000d+01, 0.75031382d+00/
            data x(219),y(219)/ 0.98384994d+00, 0.82811260d+00/
            data x(220),y(220)/ 0.93533903d+00, 0.90708894d+00/
            data x(221),y(221)/ 0.84627432d+00, 0.96236145d+00/
            data x(222),y(222)/ 0.76200396d+00, 0.98998755d+00/
            data x(223),y(223)/ 0.70150429d+00, 0.92286539d+00/
            data x(224),y(224)/ 0.60601121d+00, 0.91732824d+00/
            data x(225),y(225)/ 0.53807300d+00, 0.96716952d+00/
            data x(226),y(226)/ 0.44991389d+00, 0.10000000d+01/
            data x(227),y(227)/ 0.34940988d+00, 0.98966223d+00/
            data x(228),y(228)/ 0.27384916d+00, 0.94458503d+00/
            data x(229),y(229)/ 0.17683885d+00, 0.98585039d+00/
            data x(230),y(230)/ 0.85920475d-01, 0.96512699d+00/
            data x(231),y(231)/ 0.23324331d-01, 0.93042862d+00/
            data x(232),y(232)/ 0.25133478d-01, 0.86095876d+00/
            data x(233),y(233)/ 0.16017893d-01, 0.79044342d+00/
            data x(234),y(234)/ 0.27293537d-01, 0.71037453d+00/
            data x(235),y(235)/ 0.94843647d-02, 0.64085394d+00/
            data x(236),y(236)/ 0.24800429d-01, 0.58418435d+00/
            data x(237),y(237)/ 0.17816991d-01, 0.54199851d+00/
            data x(238),y(238)/ 0.29185828d-01, 0.50188088d+00/
            data x(239),y(239)/ 0.11547797d-01, 0.47742245d+00/
            data x(240),y(240)/ 0.24702650d-01, 0.44864386d+00/
            data x(241),y(241)/ 0.53696376d-02, 0.40575615d+00/
            data x(242),y(242)/ 0.15980145d-03, 0.34338608d+00/
c
        sp(2)='airfoil'
        sp(1)='airfoil'
        sp(3)='airfoil'
        sp(4)='airfoil'
c
        do i=1,nvf
            vx(i)=x(i)
            vy(i)=y(i)
        enddo
        do i=1,nbf
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            ibndry(4,i)=2
            if(i.gt.nvf) ibndry(4,i)=0
            ibndry(5,i)=0
            ibndry(6,i)=1
        enddo
        ibndry(2,91)=1
        ibndry(2,144)=92
        ibndry(2,193)=145
        ibndry(2,242)=194
c
        ibndry(1,243)=237
        ibndry(2,243)=2
        ibndry(1,244)=61
        ibndry(2,244)=136
        ibndry(1,245)=117
        ibndry(2,245)=172
        ibndry(1,246)=156
        ibndry(2,246)=212
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
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gd9(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100),
     1          ib1(39),ib2(39),ib3(39),ib4(39)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(30),y(30)
            character*80
     +          sp(100),su(100)
            save nvf,ntf,nbf,ncf,ib1,ib2,ib3,ib4
c
c       a domain composed of nine subregions
c       data for this problem was supplied by hans mittelmann
c
            data nvf,ntf,nbf,ncf/30,9,39,1/
            data x/-30.0d0,-30.0d0,-30.0d0,-30.0d0,36.0d0,76.0d0,
     +             41.0d0,71.0d0,47.0d0,71.0d0,36.0d0,41.0d0,
     1             47.0d0,71.0d0,90.0d0,90.0d0,90.0d0,76.0d0,
     2             5.75d0,7.75d0,0.0d0,0.0d0,-5.75d0,-7.75d0,
     3             0.0d0,0.0d0,5.0d0,0.0d0,-5.0d0,0.0d0/
            data y/0.0d0,-44.0d0,32.0d0,36.0d0,0.0d0,0.0d0,
     +             5.0d0,5.0d0,11.0d0,11.0d0,32.0d0,36.0d0,
     1             36.0d0,36.0d0,36.0d0,32.0d0,-44.0d0,32.0d0,
     2             0.0d0,0.0d0,5.75d0,7.75d0,0.0d0,0.0d0,
     3             -5.75d0,-7.75d0,0.0d0,5.0d0,0.0d0,-5.0d0/
            data ib1/ 4,12,13,14, 3,18, 9, 7, 5, 4, 3,11,12,
     +               13,14,18,15,16,10, 2, 1, 1,24,24,22,20,
     1               20,26,23,21,19,25,19,23,29,27,27,28,19/
            data ib2/12,13,14,15,11,16,10, 8, 6, 3, 1, 5, 7,
     +                9,10, 6,16,17, 8,17, 2,24,23,22,20, 5,
     1               26,24,21,19,25,23,20,29,30,30,28,29,27/
            data ib3/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     +               1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,0/
            data ib4/1,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,1,1,0,0,0,0,
     +               0,0,0,0,0,0,0,0,0,2,2,2,2,0/
c
        sp(2)='plant'
        sp(1)='plant'
        sp(3)='plant'
        sp(4)='plant'
c
        do i=1,nbf
            ibndry(1,i)=ib1(i)
            ibndry(2,i)=ib2(i)
            ibndry(3,i)=ib3(i)
            ibndry(4,i)=ib4(i)
            ibndry(5,i)=0
            if(i.le.4) then
                ibndry(6,i)=1
            else
                ibndry(6,i)=0
            endif
        enddo
        do i=1,ncf
            xm(i)=0.0d0
            ym(i)=0.0d0
        enddo
        do i=1,nvf
            vx(i)=x(i)
            vy(i)=y(i)
        enddo
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
c
c       make itnode, divide long curved edges
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        call sklutl(1,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
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
        subroutine gd10(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*)
            character*80
     +          sp(100),su(100)
            save hmax,grade
c
c       circular region with many curved interior edges
c       dirichlet boundary conditions on all boundary edges
c
            data hmax,grade/0.1d0,1.5d0/
c
        sp(2)='fan'
        sp(1)='fan'
        sp(3)='fan'
        sp(4)='fan'
c
        pi=3.141592653589793d0
        r=1.0d0
        r2=r/dsqrt(2.0d0)
        ntf=7
        nvf=ntf+1
        ncf=ntf+1
        nbf=2*ntf
        vx(1)=0.0d0
        vy(1)=0.0d0
        xm(1)=0.0d0
        ym(1)=0.0d0
        dt=2.0d0*pi/dfloat(ntf)
        do i=1,ntf
            ang=dfloat(i-1)*dt
            angm=ang-pi/4.0d0
            vx(i+1)=r*dcos(ang)
            vy(i+1)=r*dsin(ang)
            xm(i+1)=r2*dcos(angm)
            ym(i+1)=r2*dsin(angm)
            k=2*i-1
            ibndry(1,k)=1
            ibndry(2,k)=i+1
            ibndry(3,k)=i+1
            ibndry(4,k)=0
            ibndry(5,k)=0
            ibndry(6,k)=k
            ibndry(1,k+1)=i+1
            ibndry(2,k+1)=i+2
            if(i.eq.ntf) ibndry(2,k+1)=2
            ibndry(3,k+1)=1
            ibndry(4,k+1)=2
            ibndry(5,k+1)=0
            ibndry(6,k+1)=k+1
        enddo
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
c
c       make itnode, refine long edges, find symmetries
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        call sklutl(1,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
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
        subroutine gd11(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*)
            character*80
     +          sp(100),su(100)
            save hmax,grade
c
c       domain which is decomposed into several distinct pieces,
c       and making extensive use of linked edges.
c
            data hmax,grade/0.1d0,1.5d0/
c
        sp(2)='arc'
        sp(1)='arc'
        sp(3)='arc'
        sp(4)='arc'
c
        pi=3.141592653589793d0
        r1=1.0d0
        r2=2.0d0
        ncf=1
        ntf=6
        nvf=4*ntf
        nbf=nvf
        dt=pi/dfloat(ntf)
        eps=dt/20.0d0
c
        xm(1)=0.0d0
        ym(1)=0.0d0
        do i=1,nbf
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            ibndry(4,i)=2
            ibndry(5,i)=0
            ibndry(6,i)=i
        enddo
        do i=1,ntf
            k=(i-1)*4
            t1=dfloat(i-1)*dt+eps
            t2=dfloat(i)*dt-eps
            c1=dcos(t1)
            c2=dcos(t2)
            s1=dsin(t1)
            s2=dsin(t2)
            vx(k+1)=r1*c1
            vy(k+1)=r1*s1
            vx(k+2)=r2*c1
            vy(k+2)=r2*s1
            vx(k+3)=r2*c2
            vy(k+3)=r2*s2
            vx(k+4)=r1*c2
            vy(k+4)=r1*s2
            ibndry(2,k+4)=k+1
            ibndry(3,k+2)=1
            ibndry(3,k+4)=1
            if(i.gt.1) ibndry(4,k+1)=-(k-1)
            if(i.lt.ntf) ibndry(4,k+3)=-(k+5)
        enddo
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
c
c       make itnode, refine long edges, find symmetries
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        call sklutl(1,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
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
        subroutine gd12(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*)
            character*80
     +          sp(100),su(100)
            save hmax,grade,ntf,nvf,ncf,nbf
c
c       a spiral shaped domain for testing tgen
c
            data ntf,nvf,ncf,nbf/1,0,2,0/
            data hmax,grade/0.1d0,1.5d0/
c
        sp(2)='spiral'
        sp(1)='spiral'
        sp(3)='spiral'
        sp(4)='spiral'
c
        nloops=6
        nvf=8*nloops+2
        nbf=nvf
c
        xm(1)=0.0d0
        ym(1)=0.0d0
        xm(2)=1.0d0
        ym(2)=0.0d0
c
        do k=1,nbf
            ibndry(1,k)=k
            ibndry(2,k)=k+2
            ibndry(3,k)=1
            ibndry(4,k)=2
            ibndry(5,k)=0
            ibndry(6,k)=k
        enddo
c
        do i=1,nloops
            k=(i-1)*8
            m=2*i+1
            vx(k+1)=-dfloat(m-1)
            vy(k+1)=0.0d0
            vx(k+2)=-dfloat(m)
            vy(k+2)=0.0d0
            vx(k+3)=1.0d0
            vy(k+3)=dfloat(m)
            vx(k+4)=1.0d0
            vy(k+4)=dfloat(m+1)
            vx(k+5)=dfloat(m+1)
            vy(k+5)=0.0d0
            vx(k+6)=dfloat(m+2)
            vy(k+6)=0.0d0
            vx(k+7)=0.0d0
            vy(k+7)=-dfloat(m+1)
            vx(k+8)=0.0d0
            vy(k+8)=-dfloat(m+2)
            do j=k+1,k+4
                ibndry(3,j)=2
            enddo
        enddo
        m=2*nloops+3
        k=nloops*8
        vx(k+1)=-dfloat(m-1)
        vy(k+1)=0.0d0
        vx(k+2)=-dfloat(m)
        vy(k+2)=0.0d0
        ibndry(1,k+1)=1
        ibndry(2,k+1)=2
        ibndry(3,k+1)=0
        ibndry(1,k+2)=k+1
        ibndry(2,k+2)=k+2
        ibndry(3,k+2)=0
c
        itnode(1,1)=1
        itnode(2,1)=1
        itnode(3,1)=0
        itnode(5,1)=1
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
c
c       refine long edges
c
        call sklutl(1,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
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
        subroutine gd13(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(200),y(200)
            character*80
     +          sp(100),su(100)
c
c       this domain is a facsimile of the newly adopted ucsd logo.
c       dirichlet boundary conditions are imposed.
c
            save hmax,grade,ntf,nvf,ncf,nbf,x,y
c
            data x( 1),y( 1)/32.0d0,52.0d0/
            data x( 2),y( 2)/52.0d0,52.0d0/
            data x( 3),y( 3)/52.0d0,49.0d0/
            data x( 4),y( 4)/45.0d0,49.0d0/
            data x( 5),y( 5)/46.0d0,50.0d0/
            data x( 6),y( 6)/32.0d0,50.0d0/
c
            data x( 7),y( 7)/25.0d0,48.2d0/
            data x( 8),y( 8)/58.5d0,48.2d0/
            data x( 9),y( 9)/58.5d0,45.0d0/
            data x(10),y(10)/46.0d0,45.0d0/
            data x(11),y(11)/47.0d0,46.0d0/
            data x(12),y(12)/25.0d0,46.0d0/
c
            data x(13),y(13)/16.5d0,44.0d0/
            data x(14),y(14)/65.8d0,44.0d0/
            data x(15),y(15)/65.8d0,41.0d0/
            data x(16),y(16)/47.0d0,41.0d0/
            data x(17),y(17)/48.0d0,42.0d0/
            data x(18),y(18)/16.5d0,42.0d0/
c
            data x(19),y(19)/10.0d0,40.0d0/
            data x(20),y(20)/68.0d0,40.0d0/
            data x(21),y(21)/68.0d0,37.0d0/
            data x(22),y(22)/60.0d0,36.0d0/
            data x(23),y(23)/55.0d0,35.0d0/
            data x(24),y(24)/52.0d0,34.5d0/
            data x(25),y(25)/49.0d0,33.0d0/
            data x(26),y(26)/47.0d0,31.0d0/
            data x(27),y(27)/46.0d0,29.0d0/
c
            data x(28),y(28)/10.0d0,38.0d0/
            data x(29),y(29)/46.0d0,27.0d0/
            data x(30),y(30)/50.0d0,26.0d0/
            data x(31),y(31)/53.0d0,25.0d0/
            data x(32),y(32)/55.0d0,24.0d0/
            data x(33),y(33)/56.3d0,23.0d0/
            data x(34),y(34)/57.0d0,21.9d0/
            data x(35),y(35)/56.0d0,21.0d0/
            data x(36),y(36)/54.0d0,20.7d0/
            data x(37),y(37)/51.0d0,21.0d0/
            data x(38),y(38)/28.0d0,28.0d0/
            data x(39),y(39)/31.0d0,28.1d0/
            data x(40),y(40)/31.6d0,28.3d0/
            data x(41),y(41)/32.0d0,28.8d0/
            data x(42),y(42)/31.9d0,29.3d0/
            data x(43),y(43)/31.6d0,29.6d0/
            data x(44),y(44)/31.0d0,30.0d0/
c
            data x(45),y(45)/10.0d0,15.0d0/
            data x(46),y(46)/ 4.0d0,15.0d0/
            data x(47),y(47)/ 6.8d0,14.0d0/
            data x(48),y(48)/ 6.8d0, 3.0d0/
            data x(49),y(49)/ 7.5d0, 1.0d0/
            data x(50),y(50)/10.0d0, 0.0d0/
            data x(51),y(51)/20.0d0, 0.0d0/
            data x(52),y(52)/22.0d0, 1.3d0/
            data x(53),y(53)/23.5d0, 2.5d0/
            data x(54),y(54)/23.5d0,15.0d0/
            data x(55),y(55)/19.0d0,15.0d0/
            data x(56),y(56)/21.5d0,14.0d0/
            data x(57),y(57)/21.5d0, 3.0d0/
            data x(58),y(58)/20.5d0, 2.0d0/
            data x(59),y(59)/19.0d0, 1.0d0/
            data x(60),y(60)/12.0d0, 1.0d0/
            data x(61),y(61)/10.5d0, 2.0d0/
            data x(62),y(62)/10.0d0, 3.0d0/
c
            data x(63),y(63)/39.0d0,11.0d0/
            data x(64),y(64)/39.0d0,14.5d0/
            data x(65),y(65)/37.0d0,15.0d0/
            data x(66),y(66)/31.0d0,15.0d0/
            data x(67),y(67)/28.0d0,14.5d0/
            data x(68),y(68)/25.5d0,13.0d0/
            data x(69),y(69)/24.5d0,10.0d0/
            data x(70),y(70)/24.5d0, 5.0d0/
            data x(71),y(71)/25.5d0, 3.0d0/
            data x(72),y(72)/27.0d0, 1.5d0/
            data x(73),y(73)/30.0d0, 0.0d0/
            data x(74),y(74)/38.0d0, 0.0d0/
            data x(75),y(75)/39.0d0, 1.0d0/
            data x(76),y(76)/33.0d0, 1.0d0/
            data x(77),y(77)/30.0d0, 2.0d0/
            data x(78),y(78)/28.5d0, 3.0d0/
            data x(79),y(79)/27.0d0, 5.0d0/
            data x(80),y(80)/27.0d0,10.0d0/
            data x(81),y(81)/28.0d0,12.0d0/
            data x(82),y(82)/29.5d0,13.0d0/
            data x(83),y(83)/31.0d0,14.0d0/
            data x(84),y(84)/35.0d0,14.0d0/
            data x(85),y(85)/38.0d0,11.5d0/
c
            data x(86),y(86)/54.5d0,11.0d0/
            data x(87),y(87)/54.5d0,14.5d0/
            data x(88),y(88)/52.5d0,15.0d0/
            data x(89),y(89)/44.5d0,15.0d0/
            data x(90),y(90)/42.0d0,14.0d0/
            data x(91),y(91)/41.0d0,13.0d0/
            data x(92),y(92)/41.0d0,10.0d0/
            data x(93),y(93)/42.0d0, 8.5d0/
            data x(94),y(94)/50.0d0, 5.0d0/
            data x(95),y(95)/52.0d0, 3.0d0/
            data x(96),y(96)/50.0d0, 1.0d0/
            data x(97),y(97)/42.0d0, 1.0d0/
            data x(98),y(98)/42.0d0, 0.0d0/
            data x(99),y(99)/51.0d0, 0.0d0/
            data x(100),y(100)/54.0d0, 1.0d0/
            data x(101),y(101)/55.5d0, 2.5d0/
            data x(102),y(102)/55.5d0, 5.0d0/
            data x(103),y(103)/54.5d0, 6.0d0/
            data x(104),y(104)/50.0d0, 8.0d0/
            data x(105),y(105)/44.5d0,10.0d0/
            data x(106),y(106)/43.5d0,12.0d0/
            data x(107),y(107)/45.0d0,14.0d0/
            data x(108),y(108)/50.5d0,14.0d0/
            data x(109),y(109)/53.5d0,11.5d0/
c
            data x(110),y(110)/61.0d0,15.0d0/
            data x(111),y(111)/55.0d0,15.0d0/
            data x(112),y(112)/57.8d0,14.0d0/
            data x(113),y(113)/57.8d0, 0.0d0/
            data x(114),y(114)/61.0d0, 0.0d0/
            data x(115),y(115)/61.0d0,14.0d0/
            data x(116),y(116)/67.0d0,14.0d0/
            data x(117),y(117)/70.0d0,13.0d0/
            data x(118),y(118)/71.5d0,10.5d0/
            data x(119),y(119)/71.5d0, 4.5d0/
            data x(120),y(120)/70.0d0, 2.0d0/
            data x(121),y(121)/67.0d0, 1.0d0/
            data x(122),y(122)/62.0d0, 1.0d0/
            data x(123),y(123)/62.0d0, 0.0d0/
            data x(124),y(124)/69.0d0, 0.0d0/
            data x(125),y(125)/72.5d0, 1.5d0/
            data x(126),y(126)/73.7d0, 3.0d0/
            data x(127),y(127)/74.7d0, 5.0d0/
            data x(128),y(128)/74.7d0,10.0d0/
            data x(129),y(129)/73.7d0,12.0d0/
            data x(130),y(130)/72.5d0,13.5d0/
            data x(131),y(131)/69.0d0,15.0d0/
c
            data x(132),y(132)/-33.2d0,-40.0d0/
            data x(133),y(133)/114.7d0,-40.0d0/
            data x(134),y(134)/114.7d0,92.0d0/
            data x(135),y(135)/-33.2d0,92.0d0/
c
            data ntf,nvf,ncf,nbf/9,135,0,145/
            data hmax,grade/0.1d0,1.5d0/
c
        sp(2)='ucsd'
        sp(1)='ucsd'
        sp(3)='ucsd'
        sp(4)='ucsd'
c
        do i=1,nvf
            vx(i)=x(i)
            vy(i)=y(i)
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            ibndry(4,i)=2
            ibndry(5,i)=0
            ibndry(6,i)=i
        enddo
        ibndry(2,6)=1
        ibndry(2,12)=7
        ibndry(2,18)=13
        ibndry(2,27)=19
        ibndry(2,44)=28
        ibndry(2,62)=45
        ibndry(2,85)=63
        ibndry(2,109)=86
        ibndry(2,131)=110
        ibndry(2,135)=132
c
        do i=nvf+1,nbf
            ibndry(3,i)=0
            ibndry(4,i)=0
            ibndry(5,i)=0
            ibndry(6,i)=i
        enddo
        ibndry(1,136)=132
        ibndry(2,136)=49
        ibndry(1,137)=54
        ibndry(2,137)=68
        ibndry(1,138)=75
        ibndry(2,138)=97
        ibndry(1,139)=100
        ibndry(2,139)=113
        ibndry(1,140)=110
        ibndry(2,140)=35
        ibndry(1,141)=29
        ibndry(2,141)=27
        ibndry(1,142)=19
        ibndry(2,142)=18
        ibndry(1,143)=13
        ibndry(2,143)=12
        ibndry(1,144)=6
        ibndry(2,144)=7
        ibndry(1,145)=1
        ibndry(2,145)=135
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
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gd14(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          xx(16),yy(16)
            character*80
     +          sp(100),su(100)
c
c        a domain in the shape of a nozzle.
c        four similar regions, several curved edges.
c
            save ntf,nvf,ncf,nbf
            data ntf,nvf,ncf,nbf/4,29,16,32/
c
        sp(2)='nozzle'
        sp(1)='nozzle'
        sp(3)='nozzle'
        sp(4)='nozzle'
c
        rp(1)=ru(8)
c
        vx(1)=0.0d0
        vy(1)=0.0d0
        vx(2)=4.0d0
        vy(2)=0.0d0
        vx(3)=4.0d0
        vy(3)=1.0d0
        vx(4)=3.0d0
        vy(4)=1.0d0
        vx(5)=2.5d0
        vy(5)=f(2.5d0)
        vx(6)=2.0d0
        vy(6)=f(2.0d0)
        vx(7)=1.5d0
        vy(7)=f(1.5d0)
        vx(8)=1.0d0
        vy(8)=0.5d0
        vx(9)=0.0d0
        vy(9)=0.5d0
        do i=10,16
            vx(i)=-vx(18-i)
            vy(i)=vy(18-i)
        enddo
        do i=17,29
            vx(i)=vx(32-i)
            vy(i)=-vy(32-i)
        enddo
c
        xx(1)=2.75d0
        yy(1)=f(2.75d0)
        xx(2)=2.25d0
        yy(2)=f(2.25d0)
        xx(3)=1.75d0
        yy(3)=f(1.75d0)
        xx(4)=1.25d0
        yy(4)=f(1.25d0)
        do i=1,4
            xx(4+i)=-xx(5-i)
            yy(4+i)=yy(5-i)
            xx(8+i)=-xx(i)
            yy(8+i)=-yy(i)
            xx(12+i)=xx(5-i)
            yy(12+i)=-yy(5-i)
        enddo
c
        do i=1,29
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            ibndry(4,i)=1
            ibndry(5,i)=0
            ibndry(6,i)=i
        enddo
        do i=1,4
            ibndry(3,3+i)=i
            ibndry(3,9+i)=i+4
            ibndry(3,17+i)=i+8
            ibndry(3,23+i)=i+12
        enddo
        ibndry(4,1)=0
        ibndry(4,2)=2
        ibndry(4,15)=2
        ibndry(4,16)=2
        ibndry(4,29)=2
        ibndry(2,29)=2
        do i=30,32
            ibndry(1,i)=1
            ibndry(2,i)=9
            ibndry(3,i)=0
            ibndry(4,i)=0
            ibndry(5,i)=0
            ibndry(6,i)=i
        enddo
        ibndry(2,30)=16
        ibndry(2,32)=23
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
        double precision function f(x)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
        f=(((-.25d0*x+1.5d0)*x-2.25d0)*x+2.d0)/2.d0
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gd15(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(200),y(200)
            character*80
     +          sp(100),su(100)
            save hmax,grade,ntr,nvr,ncr,nbr,x,y
c
            data x( 1),y( 1)/ 0.0d0, 7.0d0/
            data x( 2),y( 2)/ 4.0d0,10.0d0/
            data x( 3),y( 3)/ 8.0d0,16.0d0/
            data x( 4),y( 4)/11.0d0,20.0d0/
            data x( 5),y( 5)/13.0d0,26.0d0/
            data x( 6),y( 6)/14.0d0,30.0d0/
            data x( 7),y( 7)/ 9.0d0,27.0d0/
            data x( 8),y( 8)/ 6.0d0,31.0d0/
            data x( 9),y( 9)/10.0d0,34.0d0/
            data x(10),y(10)/18.0d0,38.0d0/
            data x(11),y(11)/23.0d0,38.0d0/
            data x(12),y(12)/21.0d0,34.0d0/
            data x(13),y(13)/26.0d0,35.5d0/
            data x(14),y(14)/30.0d0,37.0d0/
            data x(15),y(15)/35.0d0,36.0d0/
            data x(16),y(16)/38.0d0,33.0d0/
            data x(17),y(17)/37.0d0,28.0d0/
            data x(18),y(18)/33.0d0,24.0d0/
c
            data x(19),y(19)/40.0d0,27.0d0/
            data x(20),y(20)/43.0d0,30.0d0/
            data x(21),y(21)/42.5d0,34.0d0/
            data x(22),y(22)/42.0d0,37.0d0/
            data x(23),y(23)/43.0d0,40.0d0/
            data x(24),y(24)/45.5d0,45.0d0/
            data x(25),y(25)/49.0d0,50.0d0/
            data x(26),y(26)/52.5d0,54.5d0/
            data x(27),y(27)/56.0d0,57.5d0/
            data x(28),y(28)/58.0d0,55.0d0/
            data x(29),y(29)/59.0d0,50.5d0/
            data x(30),y(30)/58.5d0,45.0d0/
            data x(31),y(31)/57.0d0,40.0d0/
            data x(32),y(32)/54.0d0,35.0d0/
            data x(33),y(33)/50.0d0,30.0d0/
c
            data x(34),y(34)/53.0d0,26.5d0/
            data x(35),y(35)/57.0d0,28.0d0/
            data x(36),y(36)/61.0d0,32.0d0/
            data x(37),y(37)/63.0d0,35.0d0/
            data x(38),y(38)/58.0d0,35.0d0/
            data x(39),y(39)/61.0d0,39.0d0/
            data x(40),y(40)/65.0d0,40.0d0/
            data x(41),y(41)/69.0d0,48.0d0/
            data x(42),y(42)/73.0d0,50.0d0/
            data x(43),y(43)/71.5d0,41.0d0/
            data x(44),y(44)/77.5d0,42.0d0/
            data x(45),y(45)/75.5d0,36.0d0/
            data x(46),y(46)/70.0d0,35.0d0/
            data x(47),y(47)/70.0d0,30.0d0/
            data x(48),y(48)/71.0d0,26.0d0/
c
            data x(49),y(49)/74.0d0,25.0d0/
            data x(50),y(50)/76.0d0,27.0d0/
            data x(51),y(51)/79.0d0,30.0d0/
            data x(52),y(52)/81.5d0,34.0d0/
            data x(53),y(53)/83.0d0,39.0d0/
            data x(54),y(54)/89.0d0,40.0d0/
            data x(55),y(55)/86.5d0,36.0d0/
            data x(56),y(56)/90.0d0,38.5d0/
            data x(57),y(57)/95.0d0,40.0d0/
            data x(58),y(58)/97.5d0,38.0d0/
            data x(59),y(59)/100.0d0,39.0d0/
            data x(60),y(60)/104.0d0,39.0d0/
            data x(61),y(61)/107.5d0,37.0d0/
            data x(62),y(62)/108.5d0,30.0d0/
            data x(63),y(63)/110.0d0,27.0d0/
c
            data x(64),y(64)/113.0d0,26.0d0/
            data x(65),y(65)/111.5d0,30.0d0/
            data x(66),y(66)/111.5d0,32.0d0/
            data x(67),y(67)/113.0d0,36.0d0/
            data x(68),y(68)/117.0d0,38.0d0/
            data x(69),y(69)/120.0d0,38.5d0/
            data x(70),y(70)/123.0d0,38.0d0/
            data x(71),y(71)/125.0d0,36.5d0/
            data x(72),y(72)/125.0d0,38.0d0/
            data x(73),y(73)/130.0d0,39.0d0/
            data x(74),y(74)/131.0d0,30.0d0/
            data x(75),y(75)/130.0d0,24.0d0/
            data x(76),y(76)/129.0d0,20.0d0/
            data x(77),y(77)/142.0d0,22.0d0/
            data x(78),y(78)/136.0d0,15.0d0/
            data x(79),y(79)/127.0d0,13.0d0/
            data x(80),y(80)/124.0d0,10.0d0/
            data x(81),y(81)/117.0d0, 5.0d0/
            data x(82),y(82)/110.0d0, 1.0d0/
            data x(83),y(83)/100.0d0,-0.5d0/
            data x(84),y(84)/ 90.0d0,-1.5d0/
            data x(85),y(85)/ 80.0d0,-1.0d0/
            data x(86),y(86)/ 70.0d0,-0.5d0/
            data x(87),y(87)/ 60.0d0, 0.0d0/
            data x(88),y(88)/ 50.0d0, 1.5d0/
            data x(89),y(89)/ 40.0d0, 4.5d0/
            data x(90),y(90)/ 35.0d0, 7.0d0/
            data x(91),y(91)/ 32.0d0,10.0d0/
            data x(92),y(92)/ 31.0d0,12.0d0/
            data x(93),y(93)/ 32.0d0,14.0d0/
            data x(94),y(94)/ 35.0d0,15.0d0/
            data x(95),y(95)/ 40.0d0,16.0d0/
            data x(96),y(96)/ 50.0d0,16.1d0/
            data x(97),y(97)/ 60.0d0,16.2d0/
            data x(98),y(98)/ 70.0d0,16.3d0/
            data x(99),y(99)/ 80.0d0,16.5d0/
            data x(100),y(100)/ 90.0d0,16.6d0/
            data x(101),y(101)/100.0d0,16.7d0/
            data x(102),y(102)/110.0d0,16.8d0/
            data x(103),y(103)/120.0d0,17.0d0/
            data x(104),y(104)/116.0d0,21.5d0/
c
            data x(105),y(105)/110.0d0,20.0d0/
            data x(106),y(106)/105.0d0,20.0d0/
            data x(107),y(107)/102.5d0,22.0d0/
            data x(108),y(108)/102.0d0,27.0d0/
            data x(109),y(109)/101.0d0,31.5d0/
            data x(110),y(110)/100.0d0,33.0d0/
            data x(111),y(111)/ 98.0d0,30.0d0/
            data x(112),y(112)/ 95.0d0,21.0d0/
            data x(113),y(113)/ 89.5d0,20.0d0/
            data x(114),y(114)/ 90.5d0,24.0d0/
            data x(115),y(115)/ 91.5d0,29.0d0/
            data x(116),y(116)/ 90.5d0,31.0d0/
            data x(117),y(117)/ 87.5d0,29.0d0/
            data x(118),y(118)/ 84.0d0,21.0d0/
            data x(119),y(119)/ 78.0d0,20.0d0/
            data x(120),y(120)/ 79.0d0,22.0d0/
            data x(121),y(121)/ 75.0d0,21.0d0/
c
            data x(122),y(122)/70.0d0,20.0d0/
            data x(123),y(123)/66.0d0,22.0d0/
            data x(124),y(124)/62.0d0,24.5d0/
            data x(125),y(125)/57.0d0,21.0d0/
            data x(126),y(126)/52.0d0,20.0d0/
            data x(127),y(127)/47.0d0,22.0d0/
c
            data x(128),y(128)/44.0d0,25.0d0/
            data x(129),y(129)/40.0d0,22.5d0/
            data x(130),y(130)/35.0d0,21.0d0/
            data x(131),y(131)/30.0d0,20.0d0/
            data x(132),y(132)/24.0d0,19.0d0/
            data x(133),y(133)/18.0d0,20.0d0/
            data x(134),y(134)/15.0d0,15.0d0/
            data x(135),y(135)/11.0d0,10.0d0/
            data x(136),y(136)/ 6.0d0, 6.0d0/
c
            data x(137),y(137)/19.0d0,26.0d0/
            data x(138),y(138)/22.0d0,29.0d0/
            data x(139),y(139)/28.0d0,32.0d0/
            data x(140),y(140)/31.0d0,31.0d0/
            data x(141),y(141)/28.0d0,28.0d0/
            data x(142),y(142)/22.0d0,25.0d0/
c
            data x(143),y(143)/48.0d0,35.0d0/
            data x(144),y(144)/48.5d0,40.0d0/
            data x(145),y(145)/52.0d0,46.0d0/
            data x(146),y(146)/55.0d0,50.0d0/
            data x(147),y(147)/55.0d0,46.0d0/
            data x(148),y(148)/52.0d0,40.0d0/
c
            data x(149),y(149)/117.0d0,33.0d0/
            data x(150),y(150)/120.0d0,34.0d0/
            data x(151),y(151)/124.0d0,31.0d0/
            data x(152),y(152)/126.0d0,27.0d0/
            data x(153),y(153)/122.0d0,26.0d0/
            data x(154),y(154)/117.5d0,30.0d0/
c
            data x(155),y(155)/118.0d0,13.0d0/
            data x(156),y(156)/110.0d0, 7.0d0/
            data x(157),y(157)/100.0d0, 5.0d0/
            data x(158),y(158)/ 90.0d0, 4.5d0/
            data x(159),y(159)/ 80.0d0, 5.0d0/
            data x(160),y(160)/ 70.0d0, 6.0d0/
            data x(161),y(161)/ 60.0d0, 7.0d0/
            data x(162),y(162)/ 50.0d0, 8.0d0/
            data x(163),y(163)/ 46.0d0, 9.5d0/
            data x(164),y(164)/ 50.0d0,11.0d0/
            data x(165),y(165)/ 60.0d0,12.0d0/
            data x(166),y(166)/ 70.0d0,12.2d0/
            data x(167),y(167)/ 80.0d0,12.4d0/
            data x(168),y(168)/ 90.0d0,12.6d0/
            data x(169),y(169)/100.0d0,12.7d0/
            data x(170),y(170)/110.0d0,12.9d0/
c
            data ntr,nvr,ncr,nbr/5,170,0,178/
            data hmax,grade/0.1d0,1.5d0/
c
        sp(2)='pltmg'
        sp(1)='pltmg'
        sp(3)='pltmg'
        sp(4)='pltmg'
c
        do i=1,nvr
            vx(i)=x(i)
            vy(i)=y(i)
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            ibndry(4,i)=2
            ibndry(5,i)=0
            ibndry(6,i)=i
        enddo
        ibndry(2,136)=1
        ibndry(2,142)=137
        ibndry(2,148)=143
        ibndry(2,154)=149
        ibndry(2,170)=155
        do i=nvr+1,nbr
            ibndry(3,i)=0
            ibndry(4,i)=0
            ibndry(5,i)=0
            ibndry(6,i)=i
        enddo
        ibndry(1,171)=5
        ibndry(2,171)=137
        ibndry(1,172)=16
        ibndry(2,172)=140
        ibndry(1,173)=20
        ibndry(2,173)=143
        ibndry(1,174)=27
        ibndry(2,174)=146
        ibndry(1,175)=67
        ibndry(2,175)=149
        ibndry(1,176)=75
        ibndry(2,176)=152
        ibndry(1,177)=102
        ibndry(2,177)=170
        ibndry(1,178)=82
        ibndry(2,178)=156
c
        ip(1)=ntr
        ip(2)=nvr
        ip(3)=ncr
        ip(4)=nbr
        rp(15)=hmax
        rp(16)=grade
c
c       make itnode
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gd16(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*)
            character*80
     +          sp(100),su(100)
            save hmax,grade
c
c       domain which is has many cracks
c
            data hmax,grade/0.1d0,1.5d0/
c
        sp(2)='crack'
        sp(1)='crack'
        sp(3)='crack'
        sp(4)='crack'
c
        pi=3.141592653589793d0
        r1=1.0d0
        r2=2.0d0
        ncf=1
        ncrack=8
        nvf=1+3*ncrack
        nbf=4*ncrack
        ntf=ncrack
c
        xm(1)=0.0d0
        ym(1)=0.0d0
        vx(1)=0.0d0
        vy(1)=0.0d0
        do i=1,ncrack
            ang=2.0d0*pi*dfloat(i-1)/dfloat(ncrack)
            c=dcos(ang)
            s=dsin(ang)
            ii=3*i
            vx(ii-1)=r2*c
            vy(ii-1)=r2*s
            vx(ii)=r1*c
            vy(ii)=r1*s
            vx(ii+1)=r2*c
            vy(ii+1)=r2*s
c
            jj=4*i-3
c
            ibndry(1,jj)=1
            ibndry(2,jj)=ii
            ibndry(3,jj)=0
            ibndry(4,jj)=0
            ibndry(5,jj)=0
            ibndry(6,jj)=0
c
            ibndry(1,jj+1)=ii
            ibndry(2,jj+1)=ii-1
            ibndry(3,jj+1)=0
            ibndry(4,jj+1)=1
            ibndry(5,jj+1)=0
            ibndry(6,jj+1)=1
c
            ibndry(1,jj+2)=ii
            ibndry(2,jj+2)=ii+1
            ibndry(3,jj+2)=0
            ibndry(4,jj+2)=2
            ibndry(5,jj+2)=0
            ibndry(6,jj+2)=2
c
            ibndry(1,jj+3)=ii+1
            ibndry(2,jj+3)=ii+2
            ibndry(3,jj+3)=1
            ibndry(4,jj+3)=2
            ibndry(5,jj+3)=0
            ibndry(6,jj+3)=3
        enddo
        ibndry(2,nbf)=2
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
c
c       make itnode, refine long arcs, find symmetries
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        call sklutl(1,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
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
        subroutine gd17(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100),
     1          list(1250)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(302),y(302)
            character*80
     +          sp(100),su(100)
            save x,y,hmax,grade,ntf,nvf,ncf,nbf
c
c       region in the shape of monterey bay
c       the data was provided by francios lekien
c
            data ntf,nvf,ncf,nbf/1,300,0,300/
            data hmax,grade/0.1d0,1.5d0/
c
         data x( 1),y( 1)/ -122.324501d0,  36.5658989d0/
         data x( 2),y( 2)/ -122.324501d0,  36.9738998d0/
         data x( 3),y( 3)/ -122.135002d0,  36.9738998d0/
         data x( 4),y( 4)/ -122.135002d0,  36.9732018d0/
         data x( 5),y( 5)/ -122.134003d0,  36.9712982d0/
         data x( 6),y( 6)/ -122.133003d0,  36.9724998d0/
         data x( 7),y( 7)/ -122.128998d0,  36.9720993d0/
         data x( 8),y( 8)/ -122.127998d0,  36.9710999d0/
         data x( 9),y( 9)/ -122.125999d0,  36.9700012d0/
         data x( 10),y( 10)/ -122.125d0,  36.9706993d0/
         data x( 11),y( 11)/ -122.122002d0,  36.9706993d0/
         data x( 12),y( 12)/ -122.120003d0,  36.969799d0/
         data x( 13),y( 13)/ -122.120003d0,  36.9674988d0/
         data x( 14),y( 14)/ -122.116997d0,  36.9674988d0/
         data x( 15),y( 15)/ -122.115997d0,  36.9678993d0/
         data x( 16),y( 16)/ -122.113998d0,  36.9672012d0/
         data x( 17),y( 17)/ -122.113998d0,  36.9659004d0/
         data x( 18),y( 18)/ -122.111d0,  36.9654999d0/
         data x( 19),y( 19)/ -122.109001d0,  36.9639015d0/
         data x( 20),y( 20)/ -122.107002d0,  36.9625015d0/
         data x( 21),y( 21)/ -122.105003d0,  36.9613991d0/
         data x( 22),y( 22)/ -122.102997d0,  36.9613991d0/
         data x( 23),y( 23)/ -122.100998d0,  36.960701d0/
         data x( 24),y( 24)/ -122.098999d0,  36.9604988d0/
         data x( 25),y( 25)/ -122.097d0,  36.9604988d0/
         data x( 26),y( 26)/ -122.094002d0,  36.9598007d0/
         data x( 27),y( 27)/ -122.089996d0,  36.9590988d0/
         data x( 28),y( 28)/ -122.088997d0,  36.9590988d0/
         data x( 29),y( 29)/ -122.086998d0,  36.957901d0/
         data x( 30),y( 30)/ -122.084999d0,  36.9584007d0/
         data x( 31),y( 31)/ -122.083d0,  36.9572983d0/
         data x( 32),y( 32)/ -122.081001d0,  36.957901d0/
         data x( 33),y( 33)/ -122.080002d0,  36.9585991d0/
         data x( 34),y( 34)/ -122.078003d0,  36.957901d0/
         data x( 35),y( 35)/ -122.075996d0,  36.9575005d0/
         data x( 36),y( 36)/ -122.075996d0,  36.9564018d0/
         data x( 37),y( 37)/ -122.073997d0,  36.9564018d0/
         data x( 38),y( 38)/ -122.070999d0,  36.9547005d0/
         data x( 39),y( 39)/ -122.069d0,  36.9538002d0/
         data x( 40),y( 40)/ -122.065002d0,  36.9538002d0/
         data x( 41),y( 41)/ -122.060997d0,  36.9538994d0/
         data x( 42),y( 42)/ -122.059998d0,  36.9547005d0/
         data x( 43),y( 43)/ -122.057999d0,  36.9544983d0/
         data x( 44),y( 44)/ -122.056d0,  36.9533997d0/
         data x( 45),y( 45)/ -122.052002d0,  36.9544983d0/
         data x( 46),y( 46)/ -122.050003d0,  36.9544983d0/
         data x( 47),y( 47)/ -122.046997d0,  36.9544983d0/
         data x( 48),y( 48)/ -122.044998d0,  36.9552002d0/
         data x( 49),y( 49)/ -122.042d0,  36.9552002d0/
         data x( 50),y( 50)/ -122.040001d0,  36.9570999d0/
         data x( 51),y( 51)/ -122.039001d0,  36.957901d0/
         data x( 52),y( 52)/ -122.035004d0,  36.9572983d0/
         data x( 53),y( 53)/ -122.033997d0,  36.9564018d0/
         data x( 54),y( 54)/ -122.030998d0,  36.9567986d0/
         data x( 55),y( 55)/ -122.028d0,  36.9564018d0/
         data x( 56),y( 56)/ -122.024002d0,  36.9556999d0/
         data x( 57),y( 57)/ -122.024002d0,  36.9588013d0/
         data x( 58),y( 58)/ -122.026001d0,  36.9612999d0/
         data x( 59),y( 59)/ -122.026001d0,  36.9639015d0/
         data x( 60),y( 60)/ -122.025002d0,  36.9665985d0/
         data x( 61),y( 61)/ -122.022003d0,  36.9678993d0/
         data x( 62),y( 62)/ -122.016998d0,  36.9678993d0/
         data x( 63),y( 63)/ -122.014d0,  36.9686012d0/
         data x( 64),y( 64)/ -122.012001d0,  36.9686012d0/
         data x( 65),y( 65)/ -122.008003d0,  36.9679985d0/
         data x( 66),y( 66)/ -122.004997d0,  36.9679985d0/
         data x( 67),y( 67)/ -122.001999d0,  36.9668007d0/
         data x( 68),y( 68)/ -122.001999d0,  36.9651985d0/
         data x( 69),y( 69)/ -122.d0,  36.9660988d0/
         data x( 70),y( 70)/ -121.997002d0,  36.9660988d0/
         data x( 71),y( 71)/ -121.992996d0,  36.9645004d0/
         data x( 72),y( 72)/ -121.987999d0,  36.9625015d0/
         data x( 73),y( 73)/ -121.985001d0,  36.9625015d0/
         data x( 74),y( 74)/ -121.980003d0,  36.9625015d0/
         data x( 75),y( 75)/ -121.977997d0,  36.960701d0/
         data x( 76),y( 76)/ -121.975998d0,  36.9588013d0/
         data x( 77),y( 77)/ -121.973999d0,  36.9588013d0/
         data x( 78),y( 78)/ -121.973d0,  36.9588013d0/
         data x( 79),y( 79)/ -121.970001d0,  36.9604988d0/
         data x( 80),y( 80)/ -121.968002d0,  36.9626999d0/
         data x( 81),y( 81)/ -121.964996d0,  36.9645996d0/
         data x( 82),y( 82)/ -121.961998d0,  36.9654999d0/
         data x( 83),y( 83)/ -121.959999d0,  36.9679985d0/
         data x( 84),y( 84)/ -121.958d0,  36.9705009d0/
         data x( 85),y( 85)/ -121.955002d0,  36.9732018d0/
         data x( 86),y( 86)/ -121.953003d0,  36.9752007d0/
         data x( 87),y( 87)/ -121.947998d0,  36.9766006d0/
         data x( 88),y( 88)/ -121.945d0,  36.9780006d0/
         data x( 89),y( 89)/ -121.941002d0,  36.9802017d0/
         data x( 90),y( 90)/ -121.939003d0,  36.9819984d0/
         data x( 91),y( 91)/ -121.936996d0,  36.9819984d0/
         data x( 92),y( 92)/ -121.933998d0,  36.9819984d0/
         data x( 93),y( 93)/ -121.931999d0,  36.9827003d0/
         data x( 94),y( 94)/ -121.929001d0,  36.9822006d0/
         data x( 95),y( 95)/ -121.925003d0,  36.9804993d0/
         data x( 96),y( 96)/ -121.919998d0,  36.9790993d0/
         data x( 97),y( 97)/ -121.914001d0,  36.9772987d0/
         data x( 98),y( 98)/ -121.911003d0,  36.975399d0/
         data x( 99),y( 99)/ -121.910004d0,  36.9738998d0/
         data x( 100),y( 100)/ -121.906998d0,  36.9732018d0/
         data x( 101),y( 101)/ -121.903d0,  36.9720993d0/
         data x( 102),y( 102)/ -121.900002d0,  36.9706993d0/
         data x( 103),y( 103)/ -121.897003d0,  36.969101d0/
         data x( 104),y( 104)/ -121.892998d0,  36.9673004d0/
         data x( 105),y( 105)/ -121.891998d0,  36.9654999d0/
         data x( 106),y( 106)/ -121.889d0,  36.9631996d0/
         data x( 107),y( 107)/ -121.887001d0,  36.9611015d0/
         data x( 108),y( 108)/ -121.884003d0,  36.9588013d0/
         data x( 109),y( 109)/ -121.883003d0,  36.9575005d0/
         data x( 110),y( 110)/ -121.882004d0,  36.9572983d0/
         data x( 111),y( 111)/ -121.879997d0,  36.9556999d0/
         data x( 112),y( 112)/ -121.876999d0,  36.9533997d0/
         data x( 113),y( 113)/ -121.876999d0,  36.9519997d0/
         data x( 114),y( 114)/ -121.875d0,  36.9491005d0/
         data x( 115),y( 115)/ -121.872002d0,  36.9473d0/
         data x( 116),y( 116)/ -121.869003d0,  36.9440994d0/
         data x( 117),y( 117)/ -121.865997d0,  36.9411011d0/
         data x( 118),y( 118)/ -121.862999d0,  36.9361d0/
         data x( 119),y( 119)/ -121.859001d0,  36.9322014d0/
         data x( 120),y( 120)/ -121.857002d0,  36.9287987d0/
         data x( 121),y( 121)/ -121.855003d0,  36.9248009d0/
         data x( 122),y( 122)/ -121.852997d0,  36.9213982d0/
         data x( 123),y( 123)/ -121.849998d0,  36.9179993d0/
         data x( 124),y( 124)/ -121.848d0,  36.9147987d0/
         data x( 125),y( 125)/ -121.847d0,  36.9118004d0/
         data x( 126),y( 126)/ -121.843002d0,  36.9076996d0/
         data x( 127),y( 127)/ -121.838997d0,  36.9026985d0/
         data x( 128),y( 128)/ -121.835999d0,  36.8979988d0/
         data x( 129),y( 129)/ -121.834d0,  36.8937988d0/
         data x( 130),y( 130)/ -121.830002d0,  36.8890991d0/
         data x( 131),y( 131)/ -121.828003d0,  36.8844986d0/
         data x( 132),y( 132)/ -121.825996d0,  36.8806d0/
         data x( 133),y( 133)/ -121.822998d0,  36.8759003d0/
         data x( 134),y( 134)/ -121.820999d0,  36.8722992d0/
         data x( 135),y( 135)/ -121.819d0,  36.8685989d0/
         data x( 136),y( 136)/ -121.816002d0,  36.8638992d0/
         data x( 137),y( 137)/ -121.814003d0,  36.8600006d0/
         data x( 138),y( 138)/ -121.811996d0,  36.8569984d0/
         data x( 139),y( 139)/ -121.808998d0,  36.8554001d0/
         data x( 140),y( 140)/ -121.807999d0,  36.8522987d0/
         data x( 141),y( 141)/ -121.807999d0,  36.8507004d0/
         data x( 142),y( 142)/ -121.806d0,  36.8484001d0/
         data x( 143),y( 143)/ -121.805d0,  36.8450012d0/
         data x( 144),y( 144)/ -121.804001d0,  36.8424988d0/
         data x( 145),y( 145)/ -121.804001d0,  36.839901d0/
         data x( 146),y( 146)/ -121.803001d0,  36.8372993d0/
         data x( 147),y( 147)/ -121.800003d0,  36.8353996d0/
         data x( 148),y( 148)/ -121.799004d0,  36.8330994d0/
         data x( 149),y( 149)/ -121.799004d0,  36.8297997d0/
         data x( 150),y( 150)/ -121.796997d0,  36.8264999d0/
         data x( 151),y( 151)/ -121.793999d0,  36.8232002d0/
         data x( 152),y( 152)/ -121.792d0,  36.8204002d0/
         data x( 153),y( 153)/ -121.790001d0,  36.8172989d0/
         data x( 154),y( 154)/ -121.790001d0,  36.8147011d0/
         data x( 155),y( 155)/ -121.789001d0,  36.8112984d0/
         data x( 156),y( 156)/ -121.789001d0,  36.8098984d0/
         data x( 157),y( 157)/ -121.788002d0,  36.8073997d0/
         data x( 158),y( 158)/ -121.788002d0,  36.8045006d0/
         data x( 159),y( 159)/ -121.789001d0,  36.8008995d0/
         data x( 160),y( 160)/ -121.791d0,  36.797699d0/
         data x( 161),y( 161)/ -121.792999d0,  36.7935982d0/
         data x( 162),y( 162)/ -121.793999d0,  36.7904015d0/
         data x( 163),y( 163)/ -121.794998d0,  36.7867012d0/
         data x( 164),y( 164)/ -121.794998d0,  36.7849998d0/
         data x( 165),y( 165)/ -121.795998d0,  36.7811012d0/
         data x( 166),y( 166)/ -121.796997d0,  36.7771988d0/
         data x( 167),y( 167)/ -121.799004d0,  36.7723999d0/
         data x( 168),y( 168)/ -121.800003d0,  36.769001d0/
         data x( 169),y( 169)/ -121.800003d0,  36.7652016d0/
         data x( 170),y( 170)/ -121.801003d0,  36.7617989d0/
         data x( 171),y( 171)/ -121.802002d0,  36.7602005d0/
         data x( 172),y( 172)/ -121.804001d0,  36.7574005d0/
         data x( 173),y( 173)/ -121.805d0,  36.7536011d0/
         data x( 174),y( 174)/ -121.805d0,  36.7496986d0/
         data x( 175),y( 175)/ -121.804001d0,  36.7484016d0/
         data x( 176),y( 176)/ -121.805d0,  36.7458992d0/
         data x( 177),y( 177)/ -121.806999d0,  36.7416d0/
         data x( 178),y( 178)/ -121.806999d0,  36.7372017d0/
         data x( 179),y( 179)/ -121.806999d0,  36.7328987d0/
         data x( 180),y( 180)/ -121.806999d0,  36.7271996d0/
         data x( 181),y( 181)/ -121.806999d0,  36.7182999d0/
         data x( 182),y( 182)/ -121.807999d0,  36.7094994d0/
         data x( 183),y( 183)/ -121.808998d0,  36.7013016d0/
         data x( 184),y( 184)/ -121.810997d0,  36.6959d0/
         data x( 185),y( 185)/ -121.813004d0,  36.6892014d0/
         data x( 186),y( 186)/ -121.815002d0,  36.6836014d0/
         data x( 187),y( 187)/ -121.817001d0,  36.6769981d0/
         data x( 188),y( 188)/ -121.819d0,  36.6706009d0/
         data x( 189),y( 189)/ -121.821999d0,  36.6655998d0/
         data x( 190),y( 190)/ -121.824997d0,  36.6587982d0/
         data x( 191),y( 191)/ -121.829002d0,  36.6519012d0/
         data x( 192),y( 192)/ -121.832001d0,  36.6483002d0/
         data x( 193),y( 193)/ -121.834999d0,  36.6436005d0/
         data x( 194),y( 194)/ -121.838997d0,  36.6371994d0/
         data x( 195),y( 195)/ -121.842003d0,  36.6321983d0/
         data x( 196),y( 196)/ -121.847d0,  36.6260986d0/
         data x( 197),y( 197)/ -121.851997d0,  36.6208d0/
         data x( 198),y( 198)/ -121.859001d0,  36.6142998d0/
         data x( 199),y( 199)/ -121.862999d0,  36.6114998d0/
         data x( 200),y( 200)/ -121.867996d0,  36.6090012d0/
         data x( 201),y( 201)/ -121.873001d0,  36.6072006d0/
         data x( 202),y( 202)/ -121.876999d0,  36.6060982d0/
         data x( 203),y( 203)/ -121.882004d0,  36.6049004d0/
         data x( 204),y( 204)/ -121.886002d0,  36.6049004d0/
         data x( 205),y( 205)/ -121.889d0,  36.6054001d0/
         data x( 206),y( 206)/ -121.892998d0,  36.6058006d0/
         data x( 207),y( 207)/ -121.893997d0,  36.6069984d0/
         data x( 208),y( 208)/ -121.893997d0,  36.6110001d0/
         data x( 209),y( 209)/ -121.896004d0,  36.6128998d0/
         data x( 210),y( 210)/ -121.897003d0,  36.615799d0/
         data x( 211),y( 211)/ -121.898003d0,  36.6176987d0/
         data x( 212),y( 212)/ -121.900002d0,  36.6194992d0/
         data x( 213),y( 213)/ -121.902d0,  36.6214981d0/
         data x( 214),y( 214)/ -121.903999d0,  36.6237984d0/
         data x( 215),y( 215)/ -121.903999d0,  36.6245003d0/
         data x( 216),y( 216)/ -121.904999d0,  36.6241989d0/
         data x( 217),y( 217)/ -121.906998d0,  36.6245003d0/
         data x( 218),y( 218)/ -121.908997d0,  36.6236d0/
         data x( 219),y( 219)/ -121.910004d0,  36.6236d0/
         data x( 220),y( 220)/ -121.910004d0,  36.6249008d0/
         data x( 221),y( 221)/ -121.911003d0,  36.6260986d0/
         data x( 222),y( 222)/ -121.913002d0,  36.6260986d0/
         data x( 223),y( 223)/ -121.914001d0,  36.6269989d0/
         data x( 224),y( 224)/ -121.915001d0,  36.6278d0/
         data x( 225),y( 225)/ -121.916d0,  36.6285019d0/
         data x( 226),y( 226)/ -121.915001d0,  36.6292d0/
         data x( 227),y( 227)/ -121.915001d0,  36.6296997d0/
         data x( 228),y( 228)/ -121.916d0,  36.6296997d0/
         data x( 229),y( 229)/ -121.917999d0,  36.6304016d0/
         data x( 230),y( 230)/ -121.919998d0,  36.6315002d0/
         data x( 231),y( 231)/ -121.919998d0,  36.6329002d0/
         data x( 232),y( 232)/ -121.921997d0,  36.6338005d0/
         data x( 233),y( 233)/ -121.921997d0,  36.6357994d0/
         data x( 234),y( 234)/ -121.922997d0,  36.6367989d0/
         data x( 235),y( 235)/ -121.924004d0,  36.6379013d0/
         data x( 236),y( 236)/ -121.926003d0,  36.6388016d0/
         data x( 237),y( 237)/ -121.928001d0,  36.6399002d0/
         data x( 238),y( 238)/ -121.93d0,  36.6403999d0/
         data x( 239),y( 239)/ -121.931d0,  36.6403999d0/
         data x( 240),y( 240)/ -121.931999d0,  36.6403999d0/
         data x( 241),y( 241)/ -121.934998d0,  36.6403999d0/
         data x( 242),y( 242)/ -121.936996d0,  36.6375999d0/
         data x( 243),y( 243)/ -121.936996d0,  36.6351013d0/
         data x( 244),y( 244)/ -121.935997d0,  36.6321983d0/
         data x( 245),y( 245)/ -121.936996d0,  36.6310005d0/
         data x( 246),y( 246)/ -121.936996d0,  36.6299019d0/
         data x( 247),y( 247)/ -121.939003d0,  36.6299019d0/
         data x( 248),y( 248)/ -121.939003d0,  36.6281013d0/
         data x( 249),y( 249)/ -121.939003d0,  36.6268997d0/
         data x( 250),y( 250)/ -121.941002d0,  36.6268997d0/
         data x( 251),y( 251)/ -121.941002d0,  36.6245003d0/
         data x( 252),y( 252)/ -121.942001d0,  36.6231003d0/
         data x( 253),y( 253)/ -121.941002d0,  36.6217003d0/
         data x( 254),y( 254)/ -121.941002d0,  36.6211014d0/
         data x( 255),y( 255)/ -121.942001d0,  36.6203995d0/
         data x( 256),y( 256)/ -121.944d0,  36.6176987d0/
         data x( 257),y( 257)/ -121.947998d0,  36.6142998d0/
         data x( 258),y( 258)/ -121.950996d0,  36.6128998d0/
         data x( 259),y( 259)/ -121.950996d0,  36.6119003d0/
         data x( 260),y( 260)/ -121.953003d0,  36.6119003d0/
         data x( 261),y( 261)/ -121.954002d0,  36.6125984d0/
         data x( 262),y( 262)/ -121.956001d0,  36.6142006d0/
         data x( 263),y( 263)/ -121.958d0,  36.6125984d0/
         data x( 264),y( 264)/ -121.958d0,  36.6110992d0/
         data x( 265),y( 265)/ -121.959d0,  36.6095009d0/
         data x( 266),y( 266)/ -121.959999d0,  36.6091995d0/
         data x( 267),y( 267)/ -121.959999d0,  36.6076012d0/
         data x( 268),y( 268)/ -121.960999d0,  36.6050987d0/
         data x( 269),y( 269)/ -121.962997d0,  36.6030998d0/
         data x( 270),y( 270)/ -121.960999d0,  36.6007996d0/
         data x( 271),y( 271)/ -121.961998d0,  36.5992012d0/
         data x( 272),y( 272)/ -121.961998d0,  36.5971985d0/
         data x( 273),y( 273)/ -121.963997d0,  36.5960999d0/
         data x( 274),y( 274)/ -121.964996d0,  36.5956001d0/
         data x( 275),y( 275)/ -121.963997d0,  36.5934982d0/
         data x( 276),y( 276)/ -121.963997d0,  36.5900993d0/
         data x( 277),y( 277)/ -121.966003d0,  36.5881004d0/
         data x( 278),y( 278)/ -121.968002d0,  36.5858002d0/
         data x( 279),y( 279)/ -121.970001d0,  36.5858002d0/
         data x( 280),y( 280)/ -121.973999d0,  36.5858002d0/
         data x( 281),y( 281)/ -121.974998d0,  36.5848999d0/
         data x( 282),y( 282)/ -121.974998d0,  36.5835991d0/
         data x( 283),y( 283)/ -121.974998d0,  36.5824013d0/
         data x( 284),y( 284)/ -121.975998d0,  36.5824013d0/
         data x( 285),y( 285)/ -121.975998d0,  36.5830994d0/
         data x( 286),y( 286)/ -121.977997d0,  36.5830994d0/
         data x( 287),y( 287)/ -121.977997d0,  36.5819016d0/
         data x( 288),y( 288)/ -121.975998d0,  36.5807991d0/
         data x( 289),y( 289)/ -121.974998d0,  36.5789986d0/
         data x( 290),y( 290)/ -121.973d0,  36.5780983d0/
         data x( 291),y( 291)/ -121.972d0,  36.5760994d0/
         data x( 292),y( 292)/ -121.971001d0,  36.5744019d0/
         data x( 293),y( 293)/ -121.970001d0,  36.573101d0/
         data x( 294),y( 294)/ -121.969002d0,  36.5718994d0/
         data x( 295),y( 295)/ -121.967003d0,  36.5712013d0/
         data x( 296),y( 296)/ -121.963997d0,  36.5703011d0/
         data x( 297),y( 297)/ -121.962997d0,  36.5685005d0/
         data x( 298),y( 298)/ -121.959999d0,  36.5677986d0/
         data x( 299),y( 299)/ -121.958d0,  36.5674019d0/
         data x( 300),y( 300)/ -121.957001d0,  36.5658989d0/
c
        sp(2)='monterey bay'
        sp(1)='monterey bay'
        sp(3)='monterey bay'
        sp(4)='monterey bay'
c
        do i=1,nvf
            ibndry(1,i)=i-1
            ibndry(2,i)=i
            ibndry(3,i)=0
            ibndry(4,i)=2
            ibndry(5,i)=0
            ibndry(6,i)=0
            vx(i)=x(i)
            vy(i)=y(i)
        enddo
        ibndry(1,1)=nvf
c
c       make itnode
c
        itnode(1,1)=1
        itnode(2,1)=1
        itnode(3,1)=0
        itnode(4,1)=0
        itnode(5,1)=1
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        rp(15)=hmax
        rp(16)=grade
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gd18(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(100),y(100)
            character*80
     +          sp(100),su(100)
            save hmax,grade,ntf,nvf,ncf,nbf
c
c       siam logo
c
            data ntf,nvf,ncf,nbf/2,89,0,92/
            data hmax,grade/0.1d0,1.5d0/
            data x(  1),y(  1)/ 0.0d0,0.0d0/
            data x(  2),y(  2)/ 3.0d0,0.0d0/
            data x(  3),y(  3)/ 3.2d0,0.2d0/
            data x(  4),y(  4)/ 3.4d0,0.0d0/
            data x(  5),y(  5)/ 5.7d0,0.0d0/
            data x(  6),y(  6)/ 5.9d0,0.2d0/
            data x(  7),y(  7)/ 6.1d0,0.0d0/
            data x(  8),y(  8)/ 8.9d0,0.0d0/
            data x(  9),y(  9)/ 8.9d0,0.2d0/
            data x( 10),y( 10)/ 9.1d0,0.0d0/
            data x( 11),y( 11)/10.7d0,0.0d0/
            data x( 12),y( 12)/10.9d0,0.2d0/
            data x( 13),y( 13)/10.9d0,2.6d0/
            data x( 14),y( 14)/11.4d0,2.6d0/
            data x( 15),y( 15)/11.6d0,2.4d0/
            data x( 16),y( 16)/11.6d0,0.0d0/
            data x( 17),y( 17)/12.5d0,0.0d0/
            data x( 18),y( 18)/12.5d0,2.6d0/
            data x( 19),y( 19)/13.0d0,2.6d0/
            data x( 20),y( 20)/13.2d0,2.4d0/
            data x( 21),y( 21)/13.2d0,0.2d0/
            data x( 22),y( 22)/13.4d0,0.0d0/
            data x( 23),y( 23)/15.8d0,0.0d0/
            data x( 24),y( 24)/15.8d0,0.7d0/
            data x( 25),y( 25)/14.1d0,0.7d0/
            data x( 26),y( 26)/13.9d0,0.9d0/
            data x( 27),y( 27)/13.9d0,3.1d0/
            data x( 28),y( 28)/13.7d0,3.3d0/
            data x( 29),y( 29)/12.7d0,3.3d0/
            data x( 30),y( 30)/12.5d0,3.1d0/
            data x( 31),y( 31)/12.3d0,3.3d0/
            data x( 32),y( 32)/11.1d0,3.3d0/
            data x( 33),y( 33)/10.9d0,3.1d0/
            data x( 34),y( 34)/10.9d0,3.3d0/
            data x( 35),y( 35)/10.2d0,3.3d0/
            data x( 36),y( 36)/10.2d0,0.9d0/
            data x( 37),y( 37)/10.0d0,0.7d0/
            data x( 38),y( 38)/ 9.1d0,0.7d0/
            data x( 39),y( 39)/ 8.9d0,0.9d0/
            data x( 40),y( 40)/ 8.9d0,3.1d0/
            data x( 41),y( 41)/ 8.7d0,3.3d0/
            data x( 42),y( 42)/ 6.1d0,3.3d0/
            data x( 43),y( 43)/ 6.1d0,2.7d0/
            data x( 44),y( 44)/ 8.2d0,2.7d0/
            data x( 45),y( 45)/ 8.2d0,0.8d0/
            data x( 46),y( 46)/ 8.1d0,0.7d0/
            data x( 47),y( 47)/ 6.7d0,0.7d0/
            data x( 48),y( 48)/ 6.6d0,0.8d0/
            data x( 49),y( 49)/ 6.6d0,1.3d0/
            data x( 50),y( 50)/ 6.7d0,1.4d0/
            data x( 51),y( 51)/ 8.0d0,1.4d0/
            data x( 52),y( 52)/ 8.0d0,2.1d0/
            data x( 53),y( 53)/ 6.0d0,2.1d0/
            data x( 54),y( 54)/ 5.8d0,1.9d0/
            data x( 55),y( 55)/ 5.8d0,0.9d0/
            data x( 56),y( 56)/ 5.6d0,0.7d0/
            data x( 57),y( 57)/ 5.0d0,0.7d0/
            data x( 58),y( 58)/ 4.8d0,0.9d0/
            data x( 59),y( 59)/ 4.8d0,3.3d0/
            data x( 60),y( 60)/ 4.1d0,3.3d0/
            data x( 61),y( 61)/ 4.1d0,0.9d0/
            data x( 62),y( 62)/ 3.9d0,0.7d0/
            data x( 63),y( 63)/ 3.5d0,0.7d0/
            data x( 64),y( 64)/ 3.3d0,0.9d0/
            data x( 65),y( 65)/ 3.3d0,1.9d0/
            data x( 66),y( 66)/ 3.1d0,2.1d0/
            data x( 67),y( 67)/ 0.9d0,2.1d0/
            data x( 68),y( 68)/ 0.8d0,2.2d0/
            data x( 69),y( 69)/ 0.8d0,2.6d0/
            data x( 70),y( 70)/ 0.9d0,2.7d0/
            data x( 71),y( 71)/ 3.3d0,2.7d0/
            data x( 72),y( 72)/ 3.3d0,3.3d0/
            data x( 73),y( 73)/ 0.3d0,3.3d0/
            data x( 74),y( 74)/ 0.1d0,3.1d0/
            data x( 75),y( 75)/ 0.1d0,1.6d0/
            data x( 76),y( 76)/ 0.3d0,1.4d0/
            data x( 77),y( 77)/ 2.5d0,1.4d0/
            data x( 78),y( 78)/ 2.6d0,1.3d0/
            data x( 79),y( 79)/ 2.6d0,0.8d0/
            data x( 80),y( 80)/ 2.5d0,0.7d0/
            data x( 81),y( 81)/ 0.0d0,0.7d0/
c
            data x( 82),y( 82)/ 4.8d0,4.0d0/
            data x( 83),y( 83)/ 5.0d0,5.1d0/
            data x( 84),y( 84)/ 3.9d0,5.1d0/
            data x( 85),y( 85)/ 4.1d0,4.0d0/
c
            data x( 86),y( 86)/-6.0d0,-9.0d0/
            data x( 87),y( 87)/22.0d0,-9.0d0/
            data x( 88),y( 88)/22.0d0,14.0d0/
            data x( 89),y( 89)/-6.0d0,14.0d0/
c
        sp(2)='siam'
        sp(1)='siam'
        sp(3)='siam'
        sp(4)='siam'
c
        do i=1,nvf
            vx(i)=x(i)
            vy(i)=y(i)
        enddo
        do i=1,nbf
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            ibndry(4,i)=2
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
        ibndry(2,81)=1
        ibndry(2,85)=82
        ibndry(2,89)=86
        ibndry(1,90)=86
        ibndry(2,90)=1
        ibndry(1,91)=60
        ibndry(2,91)=85
        ibndry(1,92)=84
        ibndry(2,92)=89
        do i=nvf+1,nbf
            ibndry(4,i)=0
        enddo
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
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gd19(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(500),y(500)
            character*80
     +          sp(100),su(100)
            save hmax,grade,ntf,nvf,ncf,nbf
c
c       domain in the shape of mexico
c       data courtesy of steffen boerm
c
            data ntf,nvf,ncf,nbf/1,426,0,426/
            data hmax,grade/0.1d0,1.5d0/
c
            data x(  1),y(  1)/364.51599d0,420.57598d0/
            data x(  2),y(  2)/365.76298d0,419.42498d0/
            data x(  3),y(  3)/362.88398d0,409.35099d0/
            data x(  4),y(  4)/360.10198d0,403.88099d0/
            data x(  5),y(  5)/357.51199d0,391.88699d0/
            data x(  6),y(  6)/357.03199d0,392.07998d0/
            data x(  7),y(  7)/359.23799d0,403.88099d0/
            data x(  8),y(  8)/361.44499d0,408.58198d0/
            data x(  9),y(  9)/360.38999d0,409.35099d0/
            data x( 10),y( 10)/359.62298d0,407.81498d0/
            data x( 11),y( 11)/357.60699d0,408.48699d0/
            data x( 12),y( 12)/355.97698d0,406.85598d0/
            data x( 13),y( 13)/356.64799d0,405.22399d0/
            data x( 14),y( 14)/355.97698d0,402.05899d0/
            data x( 15),y( 15)/357.79899d0,400.42599d0/
            data x( 16),y( 16)/356.55198d0,398.31599d0/
            data x( 17),y( 17)/356.74398d0,394.66899d0/
            data x( 18),y( 18)/355.39999d0,392.36698d0/
            data x( 19),y( 19)/355.59199d0,391.69499d0/
            data x( 20),y( 20)/357.22399d0,390.73599d0/
            data x( 21),y( 21)/356.35998d0,386.60998d0/
            data x( 22),y( 22)/356.26399d0,375.28898d0/
            data x( 23),y( 23)/356.83898d0,365.59698d0/
            data x( 24),y( 24)/356.83898d0,361.46998d0/
            data x( 25),y( 25)/354.63299d0,356.67298d0/
            data x( 26),y( 26)/355.88099d0,351.87500d0/
            data x( 27),y( 27)/357.89599d0,344.19999d0/
            data x( 28),y( 28)/363.07699d0,338.92298d0/
            data x( 29),y( 29)/364.70799d0,336.81199d0/
            data x( 30),y( 30)/364.22799d0,334.31698d0/
            data x( 31),y( 31)/363.17298d0,334.22099d0/
            data x( 32),y( 32)/363.07699d0,334.98899d0/
            data x( 33),y( 33)/362.69299d0,338.53799d0/
            data x( 34),y( 34)/358.27899d0,342.76098d0/
            data x( 35),y( 35)/359.91099d0,337.29299d0/
            data x( 36),y( 36)/362.59599d0,334.31698d0/
            data x( 37),y( 37)/363.36499d0,333.35699d0/
            data x( 38),y( 38)/367.29798d0,323.85998d0/
            data x( 39),y( 39)/376.22198d0,311.48098d0/
            data x( 40),y( 40)/382.26699d0,303.22999d0/
            data x( 41),y( 41)/386.48799d0,293.82598d0/
            data x( 42),y( 42)/391.86199d0,287.39898d0/
            data x( 43),y( 43)/394.64399d0,285.09498d0/
            data x( 44),y( 44)/405.48699d0,284.51998d0/
            data x( 45),y( 45)/409.41999d0,282.02398d0/
            data x( 46),y( 46)/412.20298d0,281.35299d0/
            data x( 47),y( 47)/416.13699d0,275.50000d0/
            data x( 48),y( 48)/419.87899d0,274.06098d0/
            data x( 49),y( 49)/424.77198d0,276.45999d0/
            data x( 50),y( 50)/432.92799d0,280.39299d0/
            data x( 51),y( 51)/440.21998d0,281.92799d0/
            data x( 52),y( 52)/441.65899d0,281.16198d0/
            data x( 53),y( 53)/444.63398d0,282.60198d0/
            data x( 54),y( 54)/450.19898d0,284.70999d0/
            data x( 55),y( 55)/453.36499d0,286.82099d0/
            data x( 56),y( 56)/461.71398d0,288.26098d0/
            data x( 57),y( 57)/462.86499d0,287.58898d0/
            data x( 58),y( 58)/461.90499d0,285.57398d0/
            data x( 59),y( 59)/464.20799d0,284.80699d0/
            data x( 60),y( 60)/464.87998d0,283.46398d0/
            data x( 61),y( 61)/470.63699d0,284.51998d0/
            data x( 62),y( 62)/474.76298d0,287.68499d0/
            data x( 63),y( 63)/475.05099d0,288.64498d0/
            data x( 64),y( 64)/472.17199d0,291.61898d0/
            data x( 65),y( 65)/474.66598d0,294.11499d0/
            data x( 66),y( 66)/482.43798d0,300.44699d0/
            data x( 67),y( 67)/483.78199d0,303.42098d0/
            data x( 68),y( 68)/483.78199d0,309.17799d0/
            data x( 69),y( 69)/487.61898d0,313.49499d0/
            data x( 70),y( 70)/486.17999d0,322.80198d0/
            data x( 71),y( 71)/487.23599d0,330.95799d0/
            data x( 72),y( 72)/489.53799d0,334.89399d0/
            data x( 73),y( 73)/498.46199d0,339.78698d0/
            data x( 74),y( 74)/500.28498d0,340.07299d0/
            data x( 75),y( 75)/517.07598d0,344.48899d0/
            data x( 76),y( 76)/520.72198d0,347.17399d0/
            data x( 77),y( 77)/537.12998d0,346.50399d0/
            data x( 78),y( 78)/541.25599d0,346.59799d0/
            data x( 79),y( 79)/542.40699d0,347.55799d0/
            data x( 80),y( 80)/542.88598d0,348.03898d0/
            data x( 81),y( 81)/541.92799d0,348.90299d0/
            data x( 82),y( 82)/544.42199d0,349.86099d0/
            data x( 83),y( 83)/547.01298d0,348.51799d0/
            data x( 84),y( 84)/549.98699d0,344.86999d0/
            data x( 85),y( 85)/549.31498d0,342.85598d0/
            data x( 86),y( 86)/549.98699d0,340.74598d0/
            data x( 87),y( 87)/548.45199d0,335.65998d0/
            data x( 88),y( 88)/541.63999d0,326.64099d0/
            data x( 89),y( 89)/539.91099d0,319.25399d0/
            data x( 90),y( 90)/539.91099d0,315.31999d0/
            data x( 91),y( 91)/537.80198d0,313.49499d0/
            data x( 92),y( 92)/536.65098d0,313.40098d0/
            data x( 93),y( 93)/536.26599d0,312.24899d0/
            data x( 94),y( 94)/537.80198d0,310.71299d0/
            data x( 95),y( 95)/540.87199d0,311.67298d0/
            data x( 96),y( 96)/541.73498d0,311.19299d0/
            data x( 97),y( 97)/541.15998d0,309.56199d0/
            data x( 98),y( 98)/538.37699d0,307.45098d0/
            data x( 99),y( 99)/537.51298d0,305.53199d0/
            data x(100),y(100)/537.89698d0,304.95599d0/
            data x(101),y(101)/540.68099d0,306.58898d0/
            data x(102),y(102)/540.96798d0,305.43598d0/
            data x(103),y(103)/539.81698d0,301.69299d0/
            data x(104),y(104)/536.07499d0,287.39898d0/
            data x(105),y(105)/535.78698d0,282.98298d0/
            data x(106),y(106)/535.11399d0,283.08099d0/
            data x(107),y(107)/535.21099d0,287.29998d0/
            data x(108),y(108)/532.23599d0,290.56298d0/
            data x(109),y(109)/532.71499d0,294.97698d0/
            data x(110),y(110)/532.04499d0,296.12998d0/
            data x(111),y(111)/531.56498d0,295.64999d0/
            data x(112),y(112)/530.89299d0,294.01699d0/
            data x(113),y(113)/527.91799d0,291.04399d0/
            data x(114),y(114)/524.08099d0,288.16398d0/
            data x(115),y(115)/523.60099d0,286.14999d0/
            data x(116),y(116)/519.37899d0,278.08999d0/
            data x(117),y(117)/515.54098d0,279.52899d0/
            data x(118),y(118)/513.81399d0,277.99398d0/
            data x(119),y(119)/513.62199d0,275.21199d0/
            data x(120),y(120)/510.55198d0,274.92498d0/
            data x(121),y(121)/509.20799d0,275.40499d0/
            data x(122),y(122)/481.57398d0,273.29299d0/
            data x(123),y(123)/481.86199d0,263.12199d0/
            data x(124),y(124)/475.24198d0,262.16299d0/
            data x(125),y(125)/474.76298d0,261.49198d0/
            data x(126),y(126)/481.28698d0,255.83099d0/
            data x(127),y(127)/487.52398d0,252.18299d0/
            data x(128),y(128)/489.15499d0,248.72898d0/
            data x(129),y(129)/492.89698d0,246.90599d0/
            data x(130),y(130)/493.18499d0,240.66999d0/
            data x(131),y(131)/469.67698d0,238.84599d0/
            data x(132),y(132)/461.52198d0,223.39799d0/
            data x(133),y(133)/464.30398d0,219.46499d0/
            data x(134),y(134)/463.63198d0,217.83499d0/
            data x(135),y(135)/463.24798d0,212.74798d0/
            data x(136),y(136)/463.24798d0,209.10299d0/
            data x(137),y(137)/453.17399d0,217.54499d0/
            data x(138),y(138)/451.15798d0,220.23399d0/
            data x(139),y(139)/438.01399d0,230.11499d0/
            data x(140),y(140)/431.10598d0,235.96798d0/
            data x(141),y(141)/427.07598d0,238.27099d0/
            data x(142),y(142)/421.51098d0,239.99798d0/
            data x(143),y(143)/420.64698d0,238.65499d0/
            data x(144),y(144)/417.19299d0,238.84599d0/
            data x(145),y(145)/414.12199d0,240.57398d0/
            data x(146),y(146)/414.40899d0,241.34298d0/
            data x(147),y(147)/412.29798d0,242.20498d0/
            data x(148),y(148)/410.28399d0,241.34298d0/
            data x(149),y(149)/402.32099d0,233.37799d0/
            data x(150),y(150)/386.00898d0,227.04598d0/
            data x(151),y(151)/382.36299d0,226.46998d0/
            data x(152),y(152)/376.50999d0,228.77198d0/
            data x(153),y(153)/373.05499d0,230.30799d0/
            data x(154),y(154)/372.09599d0,231.93798d0/
            data x(155),y(155)/360.10198d0,232.32299d0/
            data x(156),y(156)/356.26399d0,233.66598d0/
            data x(157),y(157)/351.65899d0,236.54499d0/
            data x(158),y(158)/345.99699d0,238.46299d0/
            data x(159),y(159)/341.39099d0,242.49398d0/
            data x(160),y(160)/337.93699d0,242.49398d0/
            data x(161),y(161)/331.21998d0,244.22198d0/
            data x(162),y(162)/323.44898d0,246.23599d0/
            data x(163),y(163)/319.41999d0,249.88099d0/
            data x(164),y(164)/315.29299d0,250.36099d0/
            data x(165),y(165)/299.65399d0,256.40599d0/
            data x(166),y(166)/289.38699d0,263.89099d0/
            data x(167),y(167)/284.68499d0,268.68798d0/
            data x(168),y(168)/282.19099d0,269.26298d0/
            data x(169),y(169)/280.75099d0,268.68798d0/
            data x(170),y(170)/279.11999d0,268.10998d0/
            data x(171),y(171)/257.05198d0,275.97898d0/
            data x(172),y(172)/251.96598d0,282.21699d0/
            data x(173),y(173)/249.85598d0,284.90199d0/
            data x(174),y(174)/240.26098d0,290.56298d0/
            data x(175),y(175)/237.38198d0,292.00299d0/
            data x(176),y(176)/231.52899d0,294.88099d0/
            data x(177),y(177)/221.93399d0,308.12399d0/
            data x(178),y(178)/219.82299d0,314.07099d0/
            data x(179),y(179)/219.15098d0,316.18399d0/
            data x(180),y(180)/222.31698d0,317.33399d0/
            data x(181),y(181)/225.67599d0,318.29299d0/
            data x(182),y(182)/226.63499d0,320.59698d0/
            data x(183),y(183)/223.18099d0,322.89999d0/
            data x(184),y(184)/221.83799d0,323.28199d0/
            data x(185),y(185)/222.70199d0,325.10499d0/
            data x(186),y(186)/225.96398d0,327.79299d0/
            data x(187),y(187)/227.01998d0,330.95799d0/
            data x(188),y(188)/227.69099d0,335.56399d0/
            data x(189),y(189)/223.75698d0,338.63499d0/
            data x(190),y(190)/220.20799d0,346.98298d0/
            data x(191),y(191)/220.59098d0,348.90299d0/
            data x(192),y(192)/222.79699d0,346.88798d0/
            data x(193),y(193)/223.46899d0,347.55799d0/
            data x(194),y(194)/220.78298d0,351.58699d0/
            data x(195),y(195)/221.07099d0,355.52299d0/
            data x(196),y(196)/218.76799d0,356.00199d0/
            data x(197),y(197)/215.69799d0,360.60699d0/
            data x(198),y(198)/213.77899d0,362.43099d0/
            data x(199),y(199)/214.25898d0,363.58099d0/
            data x(200),y(200)/203.60798d0,374.80799d0/
            data x(201),y(201)/198.42599d0,382.96398d0/
            data x(202),y(202)/196.60299d0,386.22698d0/
            data x(203),y(203)/191.70999d0,388.62500d0/
            data x(204),y(204)/188.92698d0,394.18998d0/
            data x(205),y(205)/185.85699d0,394.18998d0/
            data x(206),y(206)/182.40299d0,397.83599d0/
            data x(207),y(207)/182.11499d0,400.42599d0/
            data x(208),y(208)/180.77099d0,403.01799d0/
            data x(209),y(209)/180.77099d0,404.45799d0/
            data x(210),y(210)/176.16598d0,407.43099d0/
            data x(211),y(211)/172.80799d0,409.92599d0/
            data x(212),y(212)/169.54499d0,413.09298d0/
            data x(213),y(213)/168.00999d0,414.72299d0/
            data x(214),y(214)/166.85899d0,413.09298d0/
            data x(215),y(215)/160.81399d0,416.45098d0/
            data x(216),y(216)/159.94999d0,416.64199d0/
            data x(217),y(217)/159.94999d0,418.27299d0/
            data x(218),y(218)/158.22299d0,421.72898d0/
            data x(219),y(219)/159.37500d0,424.98999d0/
            data x(220),y(220)/161.86999d0,428.06098d0/
            data x(221),y(221)/161.67698d0,429.50000d0/
            data x(222),y(222)/161.29399d0,433.62599d0/
            data x(223),y(223)/158.31898d0,437.36698d0/
            data x(224),y(224)/155.82499d0,436.69599d0/
            data x(225),y(225)/152.84999d0,437.84698d0/
            data x(226),y(226)/151.31498d0,443.41299d0/
            data x(227),y(227)/147.66899d0,445.61898d0/
            data x(228),y(228)/145.07899d0,446.48298d0/
            data x(229),y(229)/141.81599d0,449.93699d0/
            data x(230),y(230)/140.66499d0,456.07798d0/
            data x(231),y(231)/141.33599d0,460.10899d0/
            data x(232),y(232)/137.30599d0,461.16299d0/
            data x(233),y(233)/136.44299d0,460.68399d0/
            data x(234),y(234)/135.96299d0,460.20399d0/
            data x(235),y(235)/133.37199d0,461.83399d0/
            data x(236),y(236)/130.39799d0,464.71299d0/
            data x(237),y(237)/127.90399d0,470.08699d0/
            data x(238),y(238)/122.24198d0,474.88398d0/
            data x(239),y(239)/120.41899d0,476.89898d0/
            data x(240),y(240)/120.41899d0,479.20199d0/
            data x(241),y(241)/116.67698d0,484.19099d0/
            data x(242),y(242)/116.19699d0,488.41299d0/
            data x(243),y(243)/113.89498d0,489.94799d0/
            data x(244),y(244)/113.98999d0,492.25099d0/
            data x(245),y(245)/108.61698d0,502.32598d0/
            data x(246),y(246)/108.42498d0,506.93199d0/
            data x(247),y(247)/106.69799d0,511.34498d0/
            data x(248),y(248)/104.10699d0,517.10198d0/
            data x(249),y(249)/104.97099d0,522.57099d0/
            data x(250),y(250)/104.97099d0,524.87399d0/
            data x(251),y(251)/101.99699d0,526.98498d0/
            data x(252),y(252)/ 99.88598d0,527.17698d0/
            data x(253),y(253)/ 94.51199d0,533.12500d0/
            data x(254),y(254)/ 92.20999d0,534.27699d0/
            data x(255),y(255)/ 89.42698d0,532.74198d0/
            data x(256),y(256)/ 83.95799d0,536.67599d0/
            data x(257),y(257)/ 80.40798d0,538.49899d0/
            data x(258),y(258)/ 80.02398d0,538.78698d0/
            data x(259),y(259)/ 78.39299d0,539.07398d0/
            data x(260),y(260)/ 76.18598d0,527.17698d0/
            data x(261),y(261)/ 76.66598d0,524.39498d0/
            data x(262),y(262)/ 77.81698d0,522.28298d0/
            data x(263),y(263)/ 78.87298d0,506.83499d0/
            data x(264),y(264)/ 81.46299d0,501.94198d0/
            data x(265),y(265)/ 83.86199d0,499.63899d0/
            data x(266),y(266)/ 90.48298d0,492.34698d0/
            data x(267),y(267)/ 94.51199d0,485.43899d0/
            data x(268),y(268)/ 94.79998d0,483.32798d0/
            data x(269),y(269)/ 98.25498d0,482.17599d0/
            data x(270),y(270)/100.36499d0,474.69198d0/
            data x(271),y(271)/104.10699d0,472.38899d0/
            data x(272),y(272)/104.87500d0,468.45498d0/
            data x(273),y(273)/105.54598d0,466.82499d0/
            data x(274),y(274)/105.54598d0,464.52198d0/
            data x(275),y(275)/106.98599d0,459.05198d0/
            data x(276),y(276)/111.20799d0,454.54299d0/
            data x(277),y(277)/112.35899d0,450.79998d0/
            data x(278),y(278)/115.62098d0,447.63499d0/
            data x(279),y(279)/116.77299d0,447.15499d0/
            data x(280),y(280)/116.77299d0,444.37199d0/
            data x(281),y(281)/117.92399d0,441.87799d0/
            data x(282),y(282)/118.69198d0,438.42298d0/
            data x(283),y(283)/120.32299d0,436.79199d0/
            data x(284),y(284)/120.32299d0,439.57499d0/
            data x(285),y(285)/119.55499d0,441.58898d0/
            data x(286),y(286)/120.03498d0,442.35798d0/
            data x(287),y(287)/123.58499d0,438.71099d0/
            data x(288),y(288)/123.77699d0,436.21598d0/
            data x(289),y(289)/124.92898d0,435.06498d0/
            data x(290),y(290)/124.44898d0,433.62599d0/
            data x(291),y(291)/126.07998d0,430.17098d0/
            data x(292),y(292)/126.46398d0,426.71699d0/
            data x(293),y(293)/125.31298d0,424.41398d0/
            data x(294),y(294)/128.38299d0,417.98599d0/
            data x(295),y(295)/130.87699d0,416.25799d0/
            data x(296),y(296)/131.54899d0,411.84498d0/
            data x(297),y(297)/134.71598d0,405.70399d0/
            data x(298),y(298)/135.86698d0,402.63398d0/
            data x(299),y(299)/134.81098d0,396.87699d0/
            data x(300),y(300)/135.96299d0,393.03898d0/
            data x(301),y(301)/140.56898d0,389.96798d0/
            data x(302),y(302)/141.71998d0,391.02398d0/
            data x(303),y(303)/141.91198d0,393.32699d0/
            data x(304),y(304)/143.73498d0,392.84698d0/
            data x(305),y(305)/147.76499d0,387.56999d0/
            data x(306),y(306)/149.58799d0,386.89799d0/
            data x(307),y(307)/150.25999d0,383.44398d0/
            data x(308),y(308)/151.69898d0,380.08599d0/
            data x(309),y(309)/155.72898d0,375.00000d0/
            data x(310),y(310)/154.09698d0,370.20199d0/
            data x(311),y(311)/150.06698d0,367.51699d0/
            data x(312),y(312)/147.95698d0,365.50000d0/
            data x(313),y(313)/146.61399d0,365.21299d0/
            data x(314),y(314)/143.83099d0,368.18699d0/
            data x(315),y(315)/141.62399d0,377.68798d0/
            data x(316),y(316)/138.55299d0,380.37298d0/
            data x(317),y(317)/135.29098d0,382.38798d0/
            data x(318),y(318)/125.79199d0,391.98399d0/
            data x(319),y(319)/118.59599d0,398.98699d0/
            data x(320),y(320)/117.63598d0,398.98699d0/
            data x(321),y(321)/117.15699d0,401.38598d0/
            data x(322),y(322)/116.00498d0,400.61898d0/
            data x(323),y(323)/114.37399d0,403.40098d0/
            data x(324),y(324)/113.03099d0,403.88099d0/
            data x(325),y(325)/111.39999d0,408.48699d0/
            data x(326),y(326)/113.51098d0,421.15199d0/
            data x(327),y(327)/109.48098d0,431.12998d0/
            data x(328),y(328)/106.98599d0,432.28298d0/
            data x(329),y(329)/ 93.55299d0,443.70098d0/
            data x(330),y(330)/ 92.11399d0,443.02799d0/
            data x(331),y(331)/ 89.61898d0,442.93299d0/
            data x(332),y(332)/ 86.16499d0,447.34599d0/
            data x(333),y(333)/ 84.05398d0,448.01799d0/
            data x(334),y(334)/ 81.46299d0,450.31999d0/
            data x(335),y(335)/ 76.85798d0,452.71899d0/
            data x(336),y(336)/ 75.89799d0,454.63798d0/
            data x(337),y(337)/ 68.70199d0,463.75399d0/
            data x(338),y(338)/ 69.37399d0,464.13798d0/
            data x(339),y(339)/ 74.45899d0,462.89099d0/
            data x(340),y(340)/ 78.58499d0,464.04199d0/
            data x(341),y(341)/ 80.40798d0,461.83399d0/
            data x(342),y(342)/ 82.51899d0,461.35499d0/
            data x(343),y(343)/ 85.30099d0,460.20399d0/
            data x(344),y(344)/ 85.78099d0,461.64299d0/
            data x(345),y(345)/ 85.01399d0,462.31399d0/
            data x(346),y(346)/ 82.99899d0,462.79399d0/
            data x(347),y(347)/ 80.69599d0,465.28898d0/
            data x(348),y(348)/ 82.71099d0,467.11199d0/
            data x(349),y(349)/ 83.18998d0,469.22299d0/
            data x(350),y(350)/ 83.76599d0,472.67698d0/
            data x(351),y(351)/ 85.39698d0,476.13099d0/
            data x(352),y(352)/ 80.69599d0,482.36799d0/
            data x(353),y(353)/ 77.24099d0,486.39799d0/
            data x(354),y(354)/ 69.75799d0,494.93798d0/
            data x(355),y(355)/ 66.59098d0,497.33599d0/
            data x(356),y(356)/ 60.44999d0,502.90098d0/
            data x(357),y(357)/ 60.44999d0,504.72499d0/
            data x(358),y(358)/ 59.10699d0,506.64399d0/
            data x(359),y(359)/ 59.20298d0,512.97599d0/
            data x(360),y(360)/ 56.03599d0,516.14199d0/
            data x(361),y(361)/ 55.46099d0,524.20199d0/
            data x(362),y(362)/ 52.67799d0,526.50498d0/
            data x(363),y(363)/ 52.38999d0,531.11099d0/
            data x(364),y(364)/ 49.60798d0,535.71598d0/
            data x(365),y(365)/ 48.45599d0,537.92298d0/
            data x(366),y(366)/ 48.26499d0,540.22599d0/
            data x(367),y(367)/ 49.70399d0,543.10499d0/
            data x(368),y(368)/ 48.55198d0,545.31199d0/
            data x(369),y(369)/ 46.53698d0,546.46299d0/
            data x(370),y(370)/ 45.28999d0,551.06799d0/
            data x(371),y(371)/ 42.69999d0,555.67399d0/
            data x(372),y(372)/ 43.17898d0,557.49699d0/
            data x(373),y(373)/ 42.89099d0,558.07299d0/
            data x(374),y(374)/ 42.98799d0,557.97698d0/
            data x(375),y(375)/ 81.46299d0,556.92098d0/
            data x(376),y(376)/ 79.54399d0,552.98699d0/
            data x(377),y(377)/ 82.61499d0,551.45199d0/
            data x(378),y(378)/139.51298d0,525.83399d0/
            data x(379),y(379)/173.95899d0,524.20199d0/
            data x(380),y(380)/183.07499d0,523.81799d0/
            data x(381),y(381)/183.93798d0,525.64199d0/
            data x(382),y(382)/184.12998d0,531.68598d0/
            data x(383),y(383)/211.37998d0,531.11099d0/
            data x(384),y(384)/215.69799d0,526.21699d0/
            data x(385),y(385)/219.43899d0,522.76298d0/
            data x(386),y(386)/225.48399d0,516.62199d0/
            data x(387),y(387)/229.60998d0,512.68798d0/
            data x(388),y(388)/236.42199d0,507.69898d0/
            data x(389),y(389)/238.24598d0,502.22999d0/
            data x(390),y(390)/240.16499d0,499.44699d0/
            data x(391),y(391)/240.45199d0,493.69099d0/
            data x(392),y(392)/244.19499d0,487.74198d0/
            data x(393),y(393)/246.97698d0,486.39799d0/
            data x(394),y(394)/250.62298d0,483.13598d0/
            data x(395),y(395)/253.40599d0,482.84799d0/
            data x(396),y(396)/256.95599d0,480.06498d0/
            data x(397),y(397)/259.06698d0,479.58599d0/
            data x(398),y(398)/263.00099d0,476.89898d0/
            data x(399),y(399)/265.49499d0,477.18699d0/
            data x(400),y(400)/270.19699d0,483.80699d0/
            data x(401),y(401)/272.30799d0,489.37199d0/
            data x(402),y(402)/275.18598d0,491.29199d0/
            data x(403),y(403)/277.48899d0,491.48298d0/
            data x(404),y(404)/279.59999d0,493.40199d0/
            data x(405),y(405)/286.50898d0,491.57899d0/
            data x(406),y(406)/292.26499d0,491.38699d0/
            data x(407),y(407)/299.75000d0,485.24699d0/
            data x(408),y(408)/305.98599d0,478.43399d0/
            data x(409),y(409)/312.12699d0,464.90599d0/
            data x(410),y(410)/312.79998d0,462.79399d0/
            data x(411),y(411)/316.34799d0,460.39498d0/
            data x(412),y(412)/320.57099d0,453.67898d0/
            data x(413),y(413)/321.62500d0,452.04798d0/
            data x(414),y(414)/324.40899d0,450.99299d0/
            data x(415),y(415)/325.65499d0,448.97698d0/
            data x(416),y(416)/324.98498d0,446.38699d0/
            data x(417),y(417)/326.80699d0,442.93299d0/
            data x(418),y(418)/326.61599d0,440.62998d0/
            data x(419),y(419)/330.54998d0,433.52899d0/
            data x(420),y(420)/332.75698d0,428.25199d0/
            data x(421),y(421)/337.55398d0,427.19699d0/
            data x(422),y(422)/340.33599d0,424.89399d0/
            data x(423),y(423)/345.42098d0,424.03099d0/
            data x(424),y(424)/349.06799d0,421.53498d0/
            data x(425),y(425)/357.41499d0,420.86499d0/
            data x(426),y(426)/361.15699d0,418.84999d0/
c
        sp(2)='mexico'
        sp(1)='mexico'
        sp(3)='mexico'
        sp(4)='mexico'
c
        do i=1,nvf
            vx(i)=x(i)
            vy(i)=y(i)
        enddo
        do i=1,nbf
            ibndry(1,i)=i
            ibndry(2,i)=i+1
            ibndry(3,i)=0
            ibndry(4,i)=2
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
        ibndry(2,nbf)=1
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
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine gd20(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100),
     1          b1(2000),b2(2000)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(2000),y(2000)
            character*80
     +          sp(100),su(100)
            save hmax,grade,ntf,nvf,ncf,nbf
c
c       domain in the shape of california
c
            data ntf,nvf,ncf,nbf/1,1867,0,1924/
            data nbound/506/
            data hmax,grade/0.1d0,1.5d0/
c
            data x(   1),y(   1)/227.76d0,206.88d0/
            data x(   2),y(   2)/228.96d0,212.64d0/
            data x(   3),y(   3)/228.24d0,216.72d0/
            data x(   4),y(   4)/224.40d0,218.88d0/
            data x(   5),y(   5)/223.44d0,218.88d0/
            data x(   6),y(   6)/223.20d0,217.92d0/
            data x(   7),y(   7)/222.72d0,218.16d0/
            data x(   8),y(   8)/218.64d0,220.80d0/
            data x(   9),y(   9)/216.72d0,222.96d0/
            data x(  10),y(  10)/218.64d0,229.68d0/
            data x(  11),y(  11)/218.40d0,230.16d0/
            data x(  12),y(  12)/218.64d0,231.60d0/
            data x(  13),y(  13)/217.68d0,234.00d0/
            data x(  14),y(  14)/216.72d0,234.72d0/
            data x(  15),y(  15)/214.80d0,234.72d0/
            data x(  16),y(  16)/213.84d0,235.44d0/
            data x(  17),y(  17)/212.16d0,235.68d0/
            data x(  18),y(  18)/207.84d0,240.96d0/
            data x(  19),y(  19)/205.20d0,245.76d0/
            data x(  20),y(  20)/199.68d0,247.92d0/
            data x(  21),y(  21)/199.20d0,249.60d0/
            data x(  22),y(  22)/198.24d0,250.80d0/
            data x(  23),y(  23)/197.76d0,254.16d0/
            data x(  24),y(  24)/196.56d0,255.60d0/
            data x(  25),y(  25)/196.56d0,255.84d0/
            data x(  26),y(  26)/191.76d0,260.88d0/
            data x(  27),y(  27)/190.56d0,266.40d0/
            data x(  28),y(  28)/189.12d0,268.08d0/
            data x(  29),y(  29)/186.96d0,269.04d0/
            data x(  30),y(  30)/183.36d0,276.00d0/
            data x(  31),y(  31)/180.48d0,279.36d0/
            data x(  32),y(  32)/177.60d0,281.52d0/
            data x(  33),y(  33)/175.44d0,282.48d0/
            data x(  34),y(  34)/174.48d0,284.40d0/
            data x(  35),y(  35)/172.32d0,286.08d0/
            data x(  36),y(  36)/172.80d0,287.76d0/
            data x(  37),y(  37)/172.32d0,288.96d0/
            data x(  38),y(  38)/172.56d0,290.40d0/
            data x(  39),y(  39)/171.12d0,297.60d0/
            data x(  40),y(  40)/170.64d0,298.56d0/
            data x(  41),y(  41)/171.84d0,298.80d0/
            data x(  42),y(  42)/171.84d0,300.24d0/
            data x(  43),y(  43)/169.68d0,301.92d0/
            data x(  44),y(  44)/171.84d0,305.28d0/
            data x(  45),y(  45)/173.76d0,303.36d0/
            data x(  46),y(  46)/173.76d0,303.12d0/
            data x(  47),y(  47)/174.72d0,303.12d0/
            data x(  48),y(  48)/175.92d0,304.32d0/
            data x(  49),y(  49)/177.60d0,307.44d0/
            data x(  50),y(  50)/178.08d0,310.56d0/
            data x(  51),y(  51)/178.32d0,311.28d0/
            data x(  52),y(  52)/179.04d0,314.40d0/
            data x(  53),y(  53)/180.00d0,314.88d0/
            data x(  54),y(  54)/179.04d0,314.64d0/
            data x(  55),y(  55)/178.32d0,317.28d0/
            data x(  56),y(  56)/175.92d0,321.84d0/
            data x(  57),y(  57)/174.00d0,324.48d0/
            data x(  58),y(  58)/172.80d0,324.72d0/
            data x(  59),y(  59)/170.88d0,323.52d0/
            data x(  60),y(  60)/169.44d0,324.24d0/
            data x(  61),y(  61)/167.28d0,323.28d0/
            data x(  62),y(  62)/164.88d0,324.00d0/
            data x(  63),y(  63)/159.60d0,328.08d0/
            data x(  64),y(  64)/156.72d0,332.88d0/
            data x(  65),y(  65)/154.56d0,333.84d0/
            data x(  66),y(  66)/154.56d0,334.80d0/
            data x(  67),y(  67)/153.84d0,335.76d0/
            data x(  68),y(  68)/153.60d0,336.96d0/
            data x(  69),y(  69)/152.16d0,337.44d0/
            data x(  70),y(  70)/151.68d0,338.64d0/
            data x(  71),y(  71)/151.44d0,341.28d0/
            data x(  72),y(  72)/152.64d0,347.52d0/
            data x(  73),y(  73)/150.96d0,351.84d0/
            data x(  74),y(  74)/150.72d0,354.96d0/
            data x(  75),y(  75)/149.52d0,356.16d0/
            data x(  76),y(  76)/148.56d0,355.92d0/
            data x(  77),y(  77)/147.60d0,358.08d0/
            data x(  78),y(  78)/147.84d0,361.44d0/
            data x(  79),y(  79)/149.04d0,362.40d0/
            data x(  80),y(  80)/149.04d0,367.92d0/
            data x(  81),y(  81)/148.32d0,374.40d0/
            data x(  82),y(  82)/148.08d0,374.40d0/
            data x(  83),y(  83)/146.40d0,376.80d0/
            data x(  84),y(  84)/145.92d0,376.80d0/
            data x(  85),y(  85)/145.68d0,377.28d0/
            data x(  86),y(  86)/144.00d0,378.24d0/
            data x(  87),y(  87)/143.04d0,379.44d0/
            data x(  88),y(  88)/141.84d0,379.92d0/
            data x(  89),y(  89)/140.64d0,379.20d0/
            data x(  90),y(  90)/139.68d0,379.68d0/
            data x(  91),y(  91)/139.44d0,380.88d0/
            data x(  92),y(  92)/137.28d0,382.56d0/
            data x(  93),y(  93)/135.60d0,385.44d0/
            data x(  94),y(  94)/132.24d0,387.60d0/
            data x(  95),y(  95)/130.32d0,387.84d0/
            data x(  96),y(  96)/129.12d0,387.60d0/
            data x(  97),y(  97)/128.16d0,386.40d0/
            data x(  98),y(  98)/129.12d0,385.44d0/
            data x(  99),y(  99)/126.48d0,385.92d0/
            data x( 100),y( 100)/129.60d0,392.40d0/
            data x( 101),y( 101)/130.32d0,395.52d0/
            data x( 102),y( 102)/128.64d0,400.08d0/
            data x( 103),y( 103)/129.36d0,399.60d0/
            data x( 104),y( 104)/129.84d0,400.80d0/
            data x( 105),y( 105)/128.64d0,402.96d0/
            data x( 106),y( 106)/127.20d0,404.40d0/
            data x( 107),y( 107)/125.76d0,403.68d0/
            data x( 108),y( 108)/125.04d0,405.12d0/
            data x( 109),y( 109)/125.52d0,407.76d0/
            data x( 110),y( 110)/123.12d0,412.56d0/
            data x( 111),y( 111)/118.08d0,416.16d0/
            data x( 112),y( 112)/115.68d0,419.04d0/
            data x( 113),y( 113)/114.48d0,419.76d0/
            data x( 114),y( 114)/114.48d0,420.72d0/
            data x( 115),y( 115)/113.04d0,422.16d0/
            data x( 116),y( 116)/113.28d0,422.64d0/
            data x( 117),y( 117)/110.16d0,427.92d0/
            data x( 118),y( 118)/107.04d0,430.32d0/
            data x( 119),y( 119)/106.32d0,432.00d0/
            data x( 120),y( 120)/103.44d0,434.40d0/
            data x( 121),y( 121)/103.44d0,435.12d0/
            data x( 122),y( 122)/101.76d0,436.80d0/
            data x( 123),y( 123)/101.04d0,438.24d0/
            data x( 124),y( 124)/ 98.16d0,441.84d0/
            data x( 125),y( 125)/ 97.92d0,443.52d0/
            data x( 126),y( 126)/ 98.64d0,443.76d0/
            data x( 127),y( 127)/ 99.84d0,445.44d0/
            data x( 128),y( 128)/100.56d0,448.80d0/
            data x( 129),y( 129)/ 99.60d0,453.60d0/
            data x( 130),y( 130)/ 98.88d0,454.08d0/
            data x( 131),y( 131)/ 98.88d0,455.04d0/
            data x( 132),y( 132)/ 97.44d0,457.44d0/
            data x( 133),y( 133)/ 97.68d0,459.12d0/
            data x( 134),y( 134)/ 96.48d0,462.24d0/
            data x( 135),y( 135)/ 96.96d0,463.44d0/
            data x( 136),y( 136)/ 96.24d0,463.68d0/
            data x( 137),y( 137)/ 96.72d0,464.40d0/
            data x( 138),y( 138)/ 95.76d0,466.32d0/
            data x( 139),y( 139)/ 96.72d0,470.88d0/
            data x( 140),y( 140)/ 96.48d0,471.60d0/
            data x( 141),y( 141)/ 99.12d0,477.84d0/
            data x( 142),y( 142)/ 98.40d0,480.72d0/
            data x( 143),y( 143)/ 98.88d0,484.08d0/
            data x( 144),y( 144)/ 98.64d0,485.76d0/
            data x( 145),y( 145)/ 97.20d0,487.92d0/
            data x( 146),y( 146)/ 96.72d0,493.92d0/
            data x( 147),y( 147)/ 94.56d0,496.08d0/
            data x( 148),y( 148)/ 93.60d0,498.96d0/
            data x( 149),y( 149)/ 92.40d0,499.92d0/
            data x( 150),y( 150)/ 91.92d0,501.60d0/
            data x( 151),y( 151)/ 91.68d0,502.08d0/
            data x( 152),y( 152)/ 90.00d0,504.00d0/
            data x( 153),y( 153)/ 89.28d0,505.20d0/
            data x( 154),y( 154)/ 87.60d0,506.40d0/
            data x( 155),y( 155)/ 87.60d0,508.80d0/
            data x( 156),y( 156)/ 86.40d0,510.72d0/
            data x( 157),y( 157)/ 83.28d0,512.64d0/
            data x( 158),y( 158)/ 82.08d0,514.56d0/
            data x( 159),y( 159)/ 76.80d0,519.84d0/
            data x( 160),y( 160)/ 76.08d0,520.80d0/
            data x( 161),y( 161)/ 76.56d0,522.72d0/
            data x( 162),y( 162)/ 77.04d0,523.92d0/
            data x( 163),y( 163)/ 76.56d0,527.52d0/
            data x( 164),y( 164)/ 74.88d0,530.88d0/
            data x( 165),y( 165)/ 76.32d0,534.24d0/
            data x( 166),y( 166)/ 76.32d0,534.96d0/
            data x( 167),y( 167)/ 79.92d0,541.92d0/
            data x( 168),y( 168)/ 81.84d0,545.28d0/
            data x( 169),y( 169)/ 84.00d0,548.64d0/
            data x( 170),y( 170)/ 88.32d0,555.84d0/
            data x( 171),y( 171)/ 90.24d0,561.84d0/
            data x( 172),y( 172)/ 90.48d0,564.48d0/
            data x( 173),y( 173)/ 90.24d0,564.96d0/
            data x( 174),y( 174)/ 90.00d0,565.20d0/
            data x( 175),y( 175)/ 89.04d0,565.44d0/
            data x( 176),y( 176)/ 88.80d0,570.00d0/
            data x( 177),y( 177)/ 90.00d0,570.72d0/
            data x( 178),y( 178)/ 92.16d0,577.20d0/
            data x( 179),y( 179)/ 94.08d0,583.44d0/
            data x( 180),y( 180)/ 94.80d0,588.48d0/
            data x( 181),y( 181)/ 94.32d0,593.04d0/
            data x( 182),y( 182)/ 92.40d0,600.96d0/
            data x( 183),y( 183)/ 92.40d0,603.84d0/
            data x( 184),y( 184)/ 91.68d0,605.04d0/
            data x( 185),y( 185)/ 89.76d0,605.76d0/
            data x( 186),y( 186)/ 88.08d0,607.68d0/
            data x( 187),y( 187)/ 89.52d0,610.08d0/
            data x( 188),y( 188)/ 90.72d0,614.40d0/
            data x( 189),y( 189)/ 90.96d0,620.16d0/
            data x( 190),y( 190)/ 97.20d0,619.44d0/
            data x( 191),y( 191)/101.76d0,618.96d0/
            data x( 192),y( 192)/102.00d0,618.96d0/
            data x( 193),y( 193)/109.44d0,618.48d0/
            data x( 194),y( 194)/121.20d0,617.76d0/
            data x( 195),y( 195)/128.16d0,617.04d0/
            data x( 196),y( 196)/130.56d0,617.04d0/
            data x( 197),y( 197)/131.76d0,617.04d0/
            data x( 198),y( 198)/147.84d0,616.08d0/
            data x( 199),y( 199)/154.80d0,615.60d0/
            data x( 200),y( 200)/178.08d0,614.40d0/
            data x( 201),y( 201)/179.76d0,614.16d0/
            data x( 202),y( 202)/180.48d0,614.16d0/
            data x( 203),y( 203)/181.68d0,613.92d0/
            data x( 204),y( 204)/191.04d0,613.20d0/
            data x( 205),y( 205)/199.20d0,612.72d0/
            data x( 206),y( 206)/201.12d0,612.48d0/
            data x( 207),y( 207)/202.80d0,612.24d0/
            data x( 208),y( 208)/209.76d0,612.00d0/
            data x( 209),y( 209)/215.28d0,611.52d0/
            data x( 210),y( 210)/216.00d0,611.52d0/
            data x( 211),y( 211)/218.64d0,611.52d0/
            data x( 212),y( 212)/250.56d0,609.84d0/
            data x( 213),y( 213)/258.96d0,609.36d0/
            data x( 214),y( 214)/267.60d0,609.12d0/
            data x( 215),y( 215)/268.56d0,609.12d0/
            data x( 216),y( 216)/270.24d0,609.12d0/
            data x( 217),y( 217)/271.68d0,609.12d0/
            data x( 218),y( 218)/271.44d0,606.00d0/
            data x( 219),y( 219)/271.44d0,603.60d0/
            data x( 220),y( 220)/270.24d0,564.96d0/
            data x( 221),y( 221)/270.00d0,562.08d0/
            data x( 222),y( 222)/270.00d0,558.24d0/
            data x( 223),y( 223)/269.76d0,552.48d0/
            data x( 224),y( 224)/269.76d0,551.76d0/
            data x( 225),y( 225)/269.28d0,537.12d0/
            data x( 226),y( 226)/269.04d0,529.68d0/
            data x( 227),y( 227)/268.80d0,525.60d0/
            data x( 228),y( 228)/268.56d0,516.00d0/
            data x( 229),y( 229)/268.56d0,513.36d0/
            data x( 230),y( 230)/268.32d0,508.08d0/
            data x( 231),y( 231)/268.08d0,498.72d0/
            data x( 232),y( 232)/268.08d0,498.00d0/
            data x( 233),y( 233)/267.84d0,497.04d0/
            data x( 234),y( 234)/267.84d0,492.96d0/
            data x( 235),y( 235)/267.60d0,491.28d0/
            data x( 236),y( 236)/267.36d0,482.16d0/
            data x( 237),y( 237)/267.12d0,481.20d0/
            data x( 238),y( 238)/267.12d0,480.48d0/
            data x( 239),y( 239)/267.12d0,479.76d0/
            data x( 240),y( 240)/267.12d0,478.80d0/
            data x( 241),y( 241)/267.12d0,478.08d0/
            data x( 242),y( 242)/266.88d0,475.68d0/
            data x( 243),y( 243)/266.88d0,473.04d0/
            data x( 244),y( 244)/266.88d0,472.08d0/
            data x( 245),y( 245)/266.88d0,471.36d0/
            data x( 246),y( 246)/266.88d0,470.64d0/
            data x( 247),y( 247)/266.64d0,468.00d0/
            data x( 248),y( 248)/266.64d0,462.96d0/
            data x( 249),y( 249)/266.40d0,462.24d0/
            data x( 250),y( 250)/266.40d0,461.52d0/
            data x( 251),y( 251)/266.40d0,460.56d0/
            data x( 252),y( 252)/266.40d0,459.60d0/
            data x( 253),y( 253)/266.40d0,458.88d0/
            data x( 254),y( 254)/266.40d0,457.92d0/
            data x( 255),y( 255)/266.16d0,455.28d0/
            data x( 256),y( 256)/266.16d0,454.56d0/
            data x( 257),y( 257)/265.92d0,449.76d0/
            data x( 258),y( 258)/265.92d0,440.40d0/
            data x( 259),y( 259)/265.68d0,437.04d0/
            data x( 260),y( 260)/268.32d0,434.64d0/
            data x( 261),y( 261)/270.00d0,432.96d0/
            data x( 262),y( 262)/273.84d0,429.60d0/
            data x( 263),y( 263)/277.92d0,425.52d0/
            data x( 264),y( 264)/279.36d0,424.32d0/
            data x( 265),y( 265)/284.64d0,419.28d0/
            data x( 266),y( 266)/285.36d0,418.80d0/
            data x( 267),y( 267)/285.84d0,418.32d0/
            data x( 268),y( 268)/290.40d0,414.00d0/
            data x( 269),y( 269)/309.36d0,396.24d0/
            data x( 270),y( 270)/310.32d0,395.28d0/
            data x( 271),y( 271)/312.24d0,393.60d0/
            data x( 272),y( 272)/316.32d0,389.76d0/
            data x( 273),y( 273)/319.92d0,386.40d0/
            data x( 274),y( 274)/334.08d0,373.20d0/
            data x( 275),y( 275)/335.52d0,371.76d0/
            data x( 276),y( 276)/349.68d0,358.56d0/
            data x( 277),y( 277)/350.88d0,357.36d0/
            data x( 278),y( 278)/359.04d0,349.68d0/
            data x( 279),y( 279)/360.96d0,348.00d0/
            data x( 280),y( 280)/361.92d0,346.80d0/
            data x( 281),y( 281)/366.24d0,342.96d0/
            data x( 282),y( 282)/368.88d0,340.56d0/
            data x( 283),y( 283)/373.20d0,336.48d0/
            data x( 284),y( 284)/373.68d0,336.00d0/
            data x( 285),y( 285)/376.80d0,333.12d0/
            data x( 286),y( 286)/377.04d0,332.88d0/
            data x( 287),y( 287)/386.64d0,323.76d0/
            data x( 288),y( 288)/387.60d0,323.04d0/
            data x( 289),y( 289)/401.04d0,310.32d0/
            data x( 290),y( 290)/401.76d0,309.60d0/
            data x( 291),y( 291)/403.68d0,307.92d0/
            data x( 292),y( 292)/407.28d0,304.56d0/
            data x( 293),y( 293)/415.20d0,297.12d0/
            data x( 294),y( 294)/415.92d0,296.40d0/
            data x( 295),y( 295)/420.72d0,292.32d0/
            data x( 296),y( 296)/433.44d0,280.08d0/
            data x( 297),y( 297)/440.64d0,273.36d0/
            data x( 298),y( 298)/441.84d0,272.16d0/
            data x( 299),y( 299)/447.12d0,267.36d0/
            data x( 300),y( 300)/450.72d0,264.00d0/
            data x( 301),y( 301)/454.80d0,260.16d0/
            data x( 302),y( 302)/460.80d0,254.64d0/
            data x( 303),y( 303)/463.44d0,252.00d0/
            data x( 304),y( 304)/483.60d0,233.28d0/
            data x( 305),y( 305)/484.08d0,232.80d0/
            data x( 306),y( 306)/488.88d0,228.24d0/
            data x( 307),y( 307)/492.96d0,224.64d0/
            data x( 308),y( 308)/496.56d0,221.04d0/
            data x( 309),y( 309)/506.64d0,211.68d0/
            data x( 310),y( 310)/509.76d0,208.80d0/
            data x( 311),y( 311)/511.92d0,206.64d0/
            data x( 312),y( 312)/512.16d0,205.92d0/
            data x( 313),y( 313)/512.16d0,202.32d0/
            data x( 314),y( 314)/512.16d0,199.44d0/
            data x( 315),y( 315)/514.32d0,197.28d0/
            data x( 316),y( 316)/516.24d0,193.44d0/
            data x( 317),y( 317)/519.12d0,191.04d0/
            data x( 318),y( 318)/519.60d0,190.56d0/
            data x( 319),y( 319)/521.04d0,188.16d0/
            data x( 320),y( 320)/520.80d0,187.20d0/
            data x( 321),y( 321)/522.48d0,184.56d0/
            data x( 322),y( 322)/522.00d0,183.84d0/
            data x( 323),y( 323)/523.44d0,182.16d0/
            data x( 324),y( 324)/524.64d0,179.04d0/
            data x( 325),y( 325)/524.64d0,175.44d0/
            data x( 326),y( 326)/526.80d0,175.68d0/
            data x( 327),y( 327)/528.72d0,174.24d0/
            data x( 328),y( 328)/529.20d0,173.28d0/
            data x( 329),y( 329)/531.36d0,172.08d0/
            data x( 330),y( 330)/531.84d0,171.36d0/
            data x( 331),y( 331)/532.08d0,171.12d0/
            data x( 332),y( 332)/534.24d0,170.16d0/
            data x( 333),y( 333)/536.16d0,168.00d0/
            data x( 334),y( 334)/536.88d0,165.12d0/
            data x( 335),y( 335)/535.44d0,164.88d0/
            data x( 336),y( 336)/532.32d0,160.80d0/
            data x( 337),y( 337)/529.44d0,159.60d0/
            data x( 338),y( 338)/527.76d0,157.68d0/
            data x( 339),y( 339)/526.56d0,157.44d0/
            data x( 340),y( 340)/525.84d0,156.72d0/
            data x( 341),y( 341)/523.68d0,156.00d0/
            data x( 342),y( 342)/522.72d0,154.32d0/
            data x( 343),y( 343)/522.72d0,150.96d0/
            data x( 344),y( 344)/521.28d0,150.24d0/
            data x( 345),y( 345)/521.76d0,149.52d0/
            data x( 346),y( 346)/519.84d0,147.36d0/
            data x( 347),y( 347)/518.64d0,147.12d0/
            data x( 348),y( 348)/518.64d0,146.88d0/
            data x( 349),y( 349)/518.40d0,145.20d0/
            data x( 350),y( 350)/519.36d0,144.24d0/
            data x( 351),y( 351)/518.64d0,143.76d0/
            data x( 352),y( 352)/519.84d0,142.08d0/
            data x( 353),y( 353)/518.40d0,141.12d0/
            data x( 354),y( 354)/519.36d0,136.80d0/
            data x( 355),y( 355)/519.84d0,135.36d0/
            data x( 356),y( 356)/519.60d0,134.40d0/
            data x( 357),y( 357)/520.32d0,132.72d0/
            data x( 358),y( 358)/519.84d0,132.00d0/
            data x( 359),y( 359)/518.88d0,131.52d0/
            data x( 360),y( 360)/518.64d0,130.56d0/
            data x( 361),y( 361)/519.36d0,130.08d0/
            data x( 362),y( 362)/518.64d0,129.60d0/
            data x( 363),y( 363)/519.12d0,127.20d0/
            data x( 364),y( 364)/518.40d0,126.00d0/
            data x( 365),y( 365)/519.12d0,124.08d0/
            data x( 366),y( 366)/514.80d0,118.56d0/
            data x( 367),y( 367)/514.32d0,116.88d0/
            data x( 368),y( 368)/513.36d0,115.68d0/
            data x( 369),y( 369)/512.40d0,115.92d0/
            data x( 370),y( 370)/509.76d0,115.20d0/
            data x( 371),y( 371)/510.96d0,112.08d0/
            data x( 372),y( 372)/509.52d0,109.20d0/
            data x( 373),y( 373)/510.24d0,108.24d0/
            data x( 374),y( 374)/512.16d0,107.28d0/
            data x( 375),y( 375)/511.92d0,105.60d0/
            data x( 376),y( 376)/512.40d0,104.88d0/
            data x( 377),y( 377)/512.16d0,100.80d0/
            data x( 378),y( 378)/510.96d0, 96.96d0/
            data x( 379),y( 379)/511.68d0, 96.72d0/
            data x( 380),y( 380)/512.16d0, 95.76d0/
            data x( 381),y( 381)/512.88d0, 93.84d0/
            data x( 382),y( 382)/513.36d0, 93.84d0/
            data x( 383),y( 383)/514.08d0, 94.56d0/
            data x( 384),y( 384)/515.04d0, 93.60d0/
            data x( 385),y( 385)/516.48d0, 93.60d0/
            data x( 386),y( 386)/517.68d0, 94.08d0/
            data x( 387),y( 387)/519.84d0, 93.84d0/
            data x( 388),y( 388)/521.28d0, 91.68d0/
            data x( 389),y( 389)/521.52d0, 90.24d0/
            data x( 390),y( 390)/522.24d0, 90.72d0/
            data x( 391),y( 391)/522.72d0, 90.24d0/
            data x( 392),y( 392)/522.24d0, 88.32d0/
            data x( 393),y( 393)/522.96d0, 86.40d0/
            data x( 394),y( 394)/522.96d0, 83.28d0/
            data x( 395),y( 395)/522.24d0, 82.56d0/
            data x( 396),y( 396)/519.84d0, 80.16d0/
            data x( 397),y( 397)/520.08d0, 78.00d0/
            data x( 398),y( 398)/518.64d0, 77.52d0/
            data x( 399),y( 399)/517.44d0, 76.80d0/
            data x( 400),y( 400)/515.76d0, 76.32d0/
            data x( 401),y( 401)/511.68d0, 77.04d0/
            data x( 402),y( 402)/510.72d0, 75.60d0/
            data x( 403),y( 403)/510.00d0, 75.60d0/
            data x( 404),y( 404)/483.84d0, 72.96d0/
            data x( 405),y( 405)/465.12d0, 71.04d0/
            data x( 406),y( 406)/452.64d0, 69.60d0/
            data x( 407),y( 407)/444.72d0, 68.88d0/
            data x( 408),y( 408)/443.52d0, 68.64d0/
            data x( 409),y( 409)/442.80d0, 68.64d0/
            data x( 410),y( 410)/441.84d0, 68.64d0/
            data x( 411),y( 411)/435.12d0, 67.92d0/
            data x( 412),y( 412)/433.68d0, 67.68d0/
            data x( 413),y( 413)/429.36d0, 67.20d0/
            data x( 414),y( 414)/426.96d0, 66.96d0/
            data x( 415),y( 415)/415.92d0, 66.00d0/
            data x( 416),y( 416)/415.20d0, 65.76d0/
            data x( 417),y( 417)/394.08d0, 63.60d0/
            data x( 418),y( 418)/394.08d0, 63.84d0/
            data x( 419),y( 419)/393.84d0, 65.04d0/
            data x( 420),y( 420)/393.60d0, 68.40d0/
            data x( 421),y( 421)/391.92d0, 71.52d0/
            data x( 422),y( 422)/390.48d0, 72.72d0/
            data x( 423),y( 423)/388.56d0, 72.24d0/
            data x( 424),y( 424)/388.32d0, 71.28d0/
            data x( 425),y( 425)/388.08d0, 72.24d0/
            data x( 426),y( 426)/387.84d0, 73.44d0/
            data x( 427),y( 427)/387.84d0, 73.68d0/
            data x( 428),y( 428)/387.84d0, 78.24d0/
            data x( 429),y( 429)/386.40d0, 81.12d0/
            data x( 430),y( 430)/388.08d0, 83.28d0/
            data x( 431),y( 431)/386.88d0, 90.00d0/
            data x( 432),y( 432)/383.76d0, 98.64d0/
            data x( 433),y( 433)/381.12d0,102.24d0/
            data x( 434),y( 434)/376.56d0,108.96d0/
            data x( 435),y( 435)/372.00d0,112.32d0/
            data x( 436),y( 436)/371.28d0,113.04d0/
            data x( 437),y( 437)/368.64d0,116.40d0/
            data x( 438),y( 438)/367.20d0,117.12d0/
            data x( 439),y( 439)/365.76d0,117.12d0/
            data x( 440),y( 440)/362.64d0,121.68d0/
            data x( 441),y( 441)/358.32d0,124.56d0/
            data x( 442),y( 442)/355.68d0,125.52d0/
            data x( 443),y( 443)/350.88d0,129.36d0/
            data x( 444),y( 444)/348.24d0,132.48d0/
            data x( 445),y( 445)/346.56d0,132.48d0/
            data x( 446),y( 446)/342.48d0,132.48d0/
            data x( 447),y( 447)/338.64d0,131.52d0/
            data x( 448),y( 448)/337.44d0,131.28d0/
            data x( 449),y( 449)/337.44d0,132.00d0/
            data x( 450),y( 450)/334.80d0,133.44d0/
            data x( 451),y( 451)/332.64d0,133.68d0/
            data x( 452),y( 452)/331.92d0,135.60d0/
            data x( 453),y( 453)/333.60d0,137.28d0/
            data x( 454),y( 454)/333.60d0,139.44d0/
            data x( 455),y( 455)/330.24d0,146.88d0/
            data x( 456),y( 456)/327.36d0,150.24d0/
            data x( 457),y( 457)/326.88d0,150.48d0/
            data x( 458),y( 458)/324.72d0,150.72d0/
            data x( 459),y( 459)/316.80d0,150.24d0/
            data x( 460),y( 460)/314.88d0,149.76d0/
            data x( 461),y( 461)/313.92d0,148.56d0/
            data x( 462),y( 462)/311.52d0,150.72d0/
            data x( 463),y( 463)/307.20d0,151.44d0/
            data x( 464),y( 464)/300.00d0,154.80d0/
            data x( 465),y( 465)/298.32d0,154.80d0/
            data x( 466),y( 466)/294.96d0,157.68d0/
            data x( 467),y( 467)/294.48d0,157.44d0/
            data x( 468),y( 468)/294.00d0,158.64d0/
            data x( 469),y( 469)/293.76d0,158.40d0/
            data x( 470),y( 470)/292.32d0,162.00d0/
            data x( 471),y( 471)/292.08d0,163.44d0/
            data x( 472),y( 472)/292.56d0,163.20d0/
            data x( 473),y( 473)/291.60d0,164.64d0/
            data x( 474),y( 474)/289.20d0,165.60d0/
            data x( 475),y( 475)/287.04d0,167.52d0/
            data x( 476),y( 476)/286.32d0,167.52d0/
            data x( 477),y( 477)/283.20d0,170.64d0/
            data x( 478),y( 478)/282.00d0,170.88d0/
            data x( 479),y( 479)/278.40d0,173.04d0/
            data x( 480),y( 480)/276.72d0,173.52d0/
            data x( 481),y( 481)/272.88d0,173.52d0/
            data x( 482),y( 482)/271.20d0,172.32d0/
            data x( 483),y( 483)/267.12d0,173.76d0/
            data x( 484),y( 484)/265.44d0,173.76d0/
            data x( 485),y( 485)/264.96d0,173.04d0/
            data x( 486),y( 486)/263.28d0,173.28d0/
            data x( 487),y( 487)/261.36d0,174.72d0/
            data x( 488),y( 488)/259.68d0,174.96d0/
            data x( 489),y( 489)/257.04d0,176.40d0/
            data x( 490),y( 490)/253.20d0,176.64d0/
            data x( 491),y( 491)/250.80d0,177.36d0/
            data x( 492),y( 492)/243.60d0,177.60d0/
            data x( 493),y( 493)/235.92d0,176.16d0/
            data x( 494),y( 494)/235.20d0,176.64d0/
            data x( 495),y( 495)/233.76d0,180.24d0/
            data x( 496),y( 496)/233.04d0,181.20d0/
            data x( 497),y( 497)/230.40d0,182.88d0/
            data x( 498),y( 498)/228.00d0,182.88d0/
            data x( 499),y( 499)/226.80d0,184.32d0/
            data x( 500),y( 500)/229.44d0,191.52d0/
            data x( 501),y( 501)/228.00d0,194.64d0/
            data x( 502),y( 502)/229.44d0,200.40d0/
            data x( 503),y( 503)/228.24d0,201.60d0/
            data x( 504),y( 504)/227.76d0,202.80d0/
            data x( 505),y( 505)/226.80d0,203.04d0/
            data x( 506),y( 506)/227.04d0,204.72d0/
            data x( 507),y( 507)/107.76d0,587.28d0/
            data x( 508),y( 508)/107.52d0,582.96d0/
            data x( 509),y( 509)/112.32d0,582.72d0/
            data x( 510),y( 510)/113.04d0,583.20d0/
            data x( 511),y( 511)/112.80d0,584.40d0/
            data x( 512),y( 512)/114.00d0,584.88d0/
            data x( 513),y( 513)/114.72d0,586.56d0/
            data x( 514),y( 514)/113.52d0,587.76d0/
            data x( 515),y( 515)/113.28d0,591.12d0/
            data x( 516),y( 516)/111.84d0,591.60d0/
            data x( 517),y( 517)/112.08d0,594.48d0/
            data x( 518),y( 518)/110.64d0,594.72d0/
            data x( 519),y( 519)/111.84d0,596.64d0/
            data x( 520),y( 520)/112.56d0,596.88d0/
            data x( 521),y( 521)/112.32d0,599.04d0/
            data x( 522),y( 522)/113.52d0,600.00d0/
            data x( 523),y( 523)/113.76d0,601.68d0/
            data x( 524),y( 524)/113.04d0,602.88d0/
            data x( 525),y( 525)/112.80d0,605.28d0/
            data x( 526),y( 526)/113.52d0,606.00d0/
            data x( 527),y( 527)/112.32d0,608.16d0/
            data x( 528),y( 528)/113.76d0,608.40d0/
            data x( 529),y( 529)/115.20d0,611.28d0/
            data x( 530),y( 530)/116.64d0,610.80d0/
            data x( 531),y( 531)/117.60d0,612.00d0/
            data x( 532),y( 532)/119.04d0,612.24d0/
            data x( 533),y( 533)/120.24d0,614.88d0/
            data x( 534),y( 534)/120.24d0,616.56d0/
            data x( 535),y( 535)/207.36d0,564.72d0/
            data x( 536),y( 536)/208.80d0,600.72d0/
            data x( 537),y( 537)/212.40d0,564.24d0/
            data x( 538),y( 538)/161.76d0,567.36d0/
            data x( 539),y( 539)/160.80d0,568.80d0/
            data x( 540),y( 540)/161.52d0,573.12d0/
            data x( 541),y( 541)/162.00d0,574.32d0/
            data x( 542),y( 542)/163.20d0,575.04d0/
            data x( 543),y( 543)/162.00d0,576.48d0/
            data x( 544),y( 544)/160.56d0,576.48d0/
            data x( 545),y( 545)/160.08d0,577.44d0/
            data x( 546),y( 546)/159.12d0,577.92d0/
            data x( 547),y( 547)/158.40d0,575.76d0/
            data x( 548),y( 548)/156.00d0,575.76d0/
            data x( 549),y( 549)/155.28d0,573.84d0/
            data x( 550),y( 550)/152.88d0,572.88d0/
            data x( 551),y( 551)/151.92d0,571.44d0/
            data x( 552),y( 552)/150.48d0,570.96d0/
            data x( 553),y( 553)/148.56d0,569.28d0/
            data x( 554),y( 554)/147.60d0,569.52d0/
            data x( 555),y( 555)/146.88d0,569.28d0/
            data x( 556),y( 556)/144.72d0,569.52d0/
            data x( 557),y( 557)/142.80d0,568.08d0/
            data x( 558),y( 558)/141.60d0,568.32d0/
            data x( 559),y( 559)/141.84d0,566.64d0/
            data x( 560),y( 560)/140.88d0,564.24d0/
            data x( 561),y( 561)/140.88d0,563.04d0/
            data x( 562),y( 562)/142.80d0,561.84d0/
            data x( 563),y( 563)/143.76d0,559.44d0/
            data x( 564),y( 564)/143.04d0,557.76d0/
            data x( 565),y( 565)/141.84d0,557.52d0/
            data x( 566),y( 566)/140.88d0,558.72d0/
            data x( 567),y( 567)/137.76d0,558.48d0/
            data x( 568),y( 568)/137.04d0,560.88d0/
            data x( 569),y( 569)/135.12d0,561.84d0/
            data x( 570),y( 570)/134.64d0,562.80d0/
            data x( 571),y( 571)/129.12d0,563.28d0/
            data x( 572),y( 572)/126.72d0,567.12d0/
            data x( 573),y( 573)/125.76d0,567.36d0/
            data x( 574),y( 574)/124.56d0,568.56d0/
            data x( 575),y( 575)/123.12d0,568.08d0/
            data x( 576),y( 576)/122.16d0,569.76d0/
            data x( 577),y( 577)/120.48d0,573.12d0/
            data x( 578),y( 578)/120.96d0,574.08d0/
            data x( 579),y( 579)/120.96d0,575.28d0/
            data x( 580),y( 580)/120.24d0,576.00d0/
            data x( 581),y( 581)/120.48d0,577.44d0/
            data x( 582),y( 582)/119.76d0,578.40d0/
            data x( 583),y( 583)/120.00d0,580.56d0/
            data x( 584),y( 584)/118.80d0,582.00d0/
            data x( 585),y( 585)/111.12d0,502.56d0/
            data x( 586),y( 586)/114.48d0,544.80d0/
            data x( 587),y( 587)/113.76d0,546.24d0/
            data x( 588),y( 588)/114.00d0,551.04d0/
            data x( 589),y( 589)/111.84d0,556.08d0/
            data x( 590),y( 590)/113.28d0,555.84d0/
            data x( 591),y( 591)/114.72d0,557.04d0/
            data x( 592),y( 592)/115.44d0,555.84d0/
            data x( 593),y( 593)/118.08d0,554.88d0/
            data x( 594),y( 594)/118.56d0,556.08d0/
            data x( 595),y( 595)/119.52d0,556.32d0/
            data x( 596),y( 596)/119.52d0,557.52d0/
            data x( 597),y( 597)/121.44d0,559.92d0/
            data x( 598),y( 598)/121.68d0,560.88d0/
            data x( 599),y( 599)/120.72d0,563.04d0/
            data x( 600),y( 600)/119.52d0,563.52d0/
            data x( 601),y( 601)/119.52d0,564.72d0/
            data x( 602),y( 602)/120.48d0,564.72d0/
            data x( 603),y( 603)/121.20d0,568.80d0/
            data x( 604),y( 604)/210.48d0,521.76d0/
            data x( 605),y( 605)/202.80d0,522.24d0/
            data x( 606),y( 606)/200.16d0,522.48d0/
            data x( 607),y( 607)/196.32d0,521.76d0/
            data x( 608),y( 608)/194.40d0,522.72d0/
            data x( 609),y( 609)/186.96d0,523.20d0/
            data x( 610),y( 610)/183.36d0,522.00d0/
            data x( 611),y( 611)/180.24d0,522.96d0/
            data x( 612),y( 612)/176.16d0,522.24d0/
            data x( 613),y( 613)/172.80d0,521.52d0/
            data x( 614),y( 614)/172.80d0,520.56d0/
            data x( 615),y( 615)/170.64d0,521.28d0/
            data x( 616),y( 616)/167.04d0,520.56d0/
            data x( 617),y( 617)/162.00d0,521.28d0/
            data x( 618),y( 618)/161.28d0,521.04d0/
            data x( 619),y( 619)/157.68d0,522.48d0/
            data x( 620),y( 620)/151.68d0,518.88d0/
            data x( 621),y( 621)/148.32d0,520.32d0/
            data x( 622),y( 622)/148.32d0,520.80d0/
            data x( 623),y( 623)/147.60d0,521.52d0/
            data x( 624),y( 624)/142.80d0,520.32d0/
            data x( 625),y( 625)/142.08d0,520.80d0/
            data x( 626),y( 626)/139.92d0,518.64d0/
            data x( 627),y( 627)/137.28d0,519.12d0/
            data x( 628),y( 628)/133.44d0,517.92d0/
            data x( 629),y( 629)/133.68d0,520.56d0/
            data x( 630),y( 630)/134.64d0,520.80d0/
            data x( 631),y( 631)/137.04d0,523.44d0/
            data x( 632),y( 632)/137.04d0,524.64d0/
            data x( 633),y( 633)/139.68d0,525.12d0/
            data x( 634),y( 634)/140.16d0,526.08d0/
            data x( 635),y( 635)/141.36d0,526.08d0/
            data x( 636),y( 636)/142.56d0,528.24d0/
            data x( 637),y( 637)/143.76d0,528.72d0/
            data x( 638),y( 638)/144.00d0,529.68d0/
            data x( 639),y( 639)/146.16d0,529.92d0/
            data x( 640),y( 640)/146.88d0,531.60d0/
            data x( 641),y( 641)/148.80d0,532.08d0/
            data x( 642),y( 642)/149.76d0,533.28d0/
            data x( 643),y( 643)/150.72d0,533.04d0/
            data x( 644),y( 644)/150.48d0,535.92d0/
            data x( 645),y( 645)/149.28d0,536.40d0/
            data x( 646),y( 646)/149.76d0,537.12d0/
            data x( 647),y( 647)/148.80d0,539.52d0/
            data x( 648),y( 648)/150.24d0,540.00d0/
            data x( 649),y( 649)/150.72d0,542.64d0/
            data x( 650),y( 650)/153.12d0,544.80d0/
            data x( 651),y( 651)/153.12d0,547.20d0/
            data x( 652),y( 652)/156.24d0,551.28d0/
            data x( 653),y( 653)/155.76d0,552.24d0/
            data x( 654),y( 654)/156.48d0,555.36d0/
            data x( 655),y( 655)/159.84d0,557.52d0/
            data x( 656),y( 656)/160.08d0,559.92d0/
            data x( 657),y( 657)/159.60d0,560.88d0/
            data x( 658),y( 658)/160.56d0,561.84d0/
            data x( 659),y( 659)/163.44d0,562.32d0/
            data x( 660),y( 660)/163.92d0,565.68d0/
            data x( 661),y( 661)/222.00d0,521.28d0/
            data x( 662),y( 662)/221.52d0,510.48d0/
            data x( 663),y( 663)/222.48d0,510.48d0/
            data x( 664),y( 664)/222.48d0,509.04d0/
            data x( 665),y( 665)/226.56d0,507.60d0/
            data x( 666),y( 666)/228.00d0,506.40d0/
            data x( 667),y( 667)/229.20d0,506.88d0/
            data x( 668),y( 668)/230.16d0,509.28d0/
            data x( 669),y( 669)/231.36d0,509.28d0/
            data x( 670),y( 670)/232.56d0,511.20d0/
            data x( 671),y( 671)/233.52d0,511.44d0/
            data x( 672),y( 672)/234.96d0,513.36d0/
            data x( 673),y( 673)/235.68d0,513.36d0/
            data x( 674),y( 674)/237.36d0,512.16d0/
            data x( 675),y( 675)/238.08d0,512.88d0/
            data x( 676),y( 676)/240.72d0,511.44d0/
            data x( 677),y( 677)/242.88d0,511.20d0/
            data x( 678),y( 678)/242.88d0,510.00d0/
            data x( 679),y( 679)/244.08d0,510.00d0/
            data x( 680),y( 680)/244.08d0,509.52d0/
            data x( 681),y( 681)/245.76d0,509.04d0/
            data x( 682),y( 682)/245.76d0,508.08d0/
            data x( 683),y( 683)/246.72d0,508.08d0/
            data x( 684),y( 684)/246.96d0,506.40d0/
            data x( 685),y( 685)/248.16d0,506.40d0/
            data x( 686),y( 686)/248.40d0,504.72d0/
            data x( 687),y( 687)/249.84d0,504.24d0/
            data x( 688),y( 688)/250.08d0,503.04d0/
            data x( 689),y( 689)/251.28d0,502.56d0/
            data x( 690),y( 690)/251.28d0,501.84d0/
            data x( 691),y( 691)/252.24d0,501.84d0/
            data x( 692),y( 692)/252.96d0,501.12d0/
            data x( 693),y( 693)/257.04d0,500.40d0/
            data x( 694),y( 694)/257.04d0,499.20d0/
            data x( 695),y( 695)/258.72d0,498.96d0/
            data x( 696),y( 696)/258.96d0,494.88d0/
            data x( 697),y( 697)/259.68d0,494.88d0/
            data x( 698),y( 698)/260.40d0,492.48d0/
            data x( 699),y( 699)/262.08d0,491.04d0/
            data x( 700),y( 700)/262.08d0,490.56d0/
            data x( 701),y( 701)/262.80d0,490.56d0/
            data x( 702),y( 702)/262.80d0,488.88d0/
            data x( 703),y( 703)/263.28d0,488.88d0/
            data x( 704),y( 704)/262.32d0,480.72d0/
            data x( 705),y( 705)/261.60d0,479.04d0/
            data x( 706),y( 706)/260.88d0,479.04d0/
            data x( 707),y( 707)/260.88d0,477.36d0/
            data x( 708),y( 708)/266.64d0,477.36d0/
            data x( 709),y( 709)/266.64d0,478.08d0/
            data x( 710),y( 710)/138.24d0,499.20d0/
            data x( 711),y( 711)/138.24d0,497.52d0/
            data x( 712),y( 712)/138.96d0,496.56d0/
            data x( 713),y( 713)/138.48d0,495.60d0/
            data x( 714),y( 714)/137.28d0,495.12d0/
            data x( 715),y( 715)/136.56d0,491.76d0/
            data x( 716),y( 716)/137.28d0,489.36d0/
            data x( 717),y( 717)/176.88d0,486.96d0/
            data x( 718),y( 718)/175.92d0,487.92d0/
            data x( 719),y( 719)/176.16d0,488.40d0/
            data x( 720),y( 720)/175.68d0,489.60d0/
            data x( 721),y( 721)/177.12d0,492.00d0/
            data x( 722),y( 722)/188.64d0,491.52d0/
            data x( 723),y( 723)/190.32d0,492.48d0/
            data x( 724),y( 724)/190.56d0,493.92d0/
            data x( 725),y( 725)/192.72d0,496.32d0/
            data x( 726),y( 726)/195.12d0,496.08d0/
            data x( 727),y( 727)/196.08d0,499.20d0/
            data x( 728),y( 728)/197.52d0,499.92d0/
            data x( 729),y( 729)/197.76d0,502.32d0/
            data x( 730),y( 730)/201.84d0,502.56d0/
            data x( 731),y( 731)/202.80d0,503.76d0/
            data x( 732),y( 732)/204.48d0,504.00d0/
            data x( 733),y( 733)/204.48d0,505.20d0/
            data x( 734),y( 734)/204.72d0,507.60d0/
            data x( 735),y( 735)/206.16d0,507.36d0/
            data x( 736),y( 736)/207.84d0,508.56d0/
            data x( 737),y( 737)/207.84d0,510.48d0/
            data x( 738),y( 738)/208.56d0,511.44d0/
            data x( 739),y( 739)/208.08d0,513.36d0/
            data x( 740),y( 740)/209.52d0,514.08d0/
            data x( 741),y( 741)/207.84d0,515.76d0/
            data x( 742),y( 742)/204.00d0,516.72d0/
            data x( 743),y( 743)/203.28d0,519.12d0/
            data x( 744),y( 744)/203.76d0,520.80d0/
            data x( 745),y( 745)/111.12d0,501.12d0/
            data x( 746),y( 746)/137.52d0,500.40d0/
            data x( 747),y( 747)/137.76d0,503.52d0/
            data x( 748),y( 748)/136.80d0,504.96d0/
            data x( 749),y( 749)/137.28d0,505.68d0/
            data x( 750),y( 750)/137.04d0,509.04d0/
            data x( 751),y( 751)/136.32d0,509.52d0/
            data x( 752),y( 752)/137.28d0,511.68d0/
            data x( 753),y( 753)/137.28d0,514.80d0/
            data x( 754),y( 754)/135.60d0,516.24d0/
            data x( 755),y( 755)/133.92d0,516.48d0/
            data x( 756),y( 756)/238.32d0,478.08d0/
            data x( 757),y( 757)/238.08d0,477.12d0/
            data x( 758),y( 758)/236.88d0,476.64d0/
            data x( 759),y( 759)/233.76d0,479.52d0/
            data x( 760),y( 760)/231.36d0,479.28d0/
            data x( 761),y( 761)/230.88d0,481.20d0/
            data x( 762),y( 762)/230.40d0,480.96d0/
            data x( 763),y( 763)/228.72d0,482.88d0/
            data x( 764),y( 764)/226.08d0,481.44d0/
            data x( 765),y( 765)/224.64d0,477.12d0/
            data x( 766),y( 766)/222.24d0,475.44d0/
            data x( 767),y( 767)/219.12d0,473.28d0/
            data x( 768),y( 768)/218.40d0,473.28d0/
            data x( 769),y( 769)/218.40d0,474.24d0/
            data x( 770),y( 770)/216.72d0,474.48d0/
            data x( 771),y( 771)/216.72d0,476.16d0/
            data x( 772),y( 772)/216.00d0,476.40d0/
            data x( 773),y( 773)/215.28d0,477.84d0/
            data x( 774),y( 774)/214.56d0,478.08d0/
            data x( 775),y( 775)/214.56d0,478.80d0/
            data x( 776),y( 776)/213.60d0,478.80d0/
            data x( 777),y( 777)/213.60d0,480.24d0/
            data x( 778),y( 778)/212.88d0,480.24d0/
            data x( 779),y( 779)/212.88d0,481.20d0/
            data x( 780),y( 780)/211.92d0,481.20d0/
            data x( 781),y( 781)/211.92d0,481.92d0/
            data x( 782),y( 782)/211.20d0,482.16d0/
            data x( 783),y( 783)/211.20d0,482.88d0/
            data x( 784),y( 784)/210.48d0,482.88d0/
            data x( 785),y( 785)/210.48d0,483.60d0/
            data x( 786),y( 786)/209.04d0,484.56d0/
            data x( 787),y( 787)/208.56d0,486.24d0/
            data x( 788),y( 788)/207.84d0,486.48d0/
            data x( 789),y( 789)/207.36d0,488.16d0/
            data x( 790),y( 790)/205.20d0,489.12d0/
            data x( 791),y( 791)/205.20d0,490.80d0/
            data x( 792),y( 792)/204.48d0,490.80d0/
            data x( 793),y( 793)/204.48d0,491.76d0/
            data x( 794),y( 794)/205.44d0,491.52d0/
            data x( 795),y( 795)/205.20d0,497.28d0/
            data x( 796),y( 796)/206.16d0,498.24d0/
            data x( 797),y( 797)/206.16d0,499.44d0/
            data x( 798),y( 798)/207.12d0,499.44d0/
            data x( 799),y( 799)/207.84d0,501.36d0/
            data x( 800),y( 800)/207.36d0,501.36d0/
            data x( 801),y( 801)/207.36d0,502.56d0/
            data x( 802),y( 802)/206.16d0,503.04d0/
            data x( 803),y( 803)/206.16d0,505.20d0/
            data x( 804),y( 804)/182.40d0,462.72d0/
            data x( 805),y( 805)/181.20d0,458.40d0/
            data x( 806),y( 806)/193.68d0,457.68d0/
            data x( 807),y( 807)/193.92d0,456.96d0/
            data x( 808),y( 808)/194.64d0,458.40d0/
            data x( 809),y( 809)/195.36d0,457.44d0/
            data x( 810),y( 810)/196.80d0,457.68d0/
            data x( 811),y( 811)/198.48d0,458.16d0/
            data x( 812),y( 812)/199.68d0,457.92d0/
            data x( 813),y( 813)/200.64d0,458.88d0/
            data x( 814),y( 814)/203.76d0,459.12d0/
            data x( 815),y( 815)/205.20d0,460.56d0/
            data x( 816),y( 816)/206.16d0,462.96d0/
            data x( 817),y( 817)/207.12d0,463.92d0/
            data x( 818),y( 818)/207.36d0,465.60d0/
            data x( 819),y( 819)/208.32d0,466.08d0/
            data x( 820),y( 820)/209.04d0,468.72d0/
            data x( 821),y( 821)/211.92d0,468.24d0/
            data x( 822),y( 822)/211.92d0,468.96d0/
            data x( 823),y( 823)/213.60d0,468.96d0/
            data x( 824),y( 824)/215.52d0,468.48d0/
            data x( 825),y( 825)/216.00d0,470.40d0/
            data x( 826),y( 826)/217.92d0,471.12d0/
            data x( 827),y( 827)/217.92d0,472.08d0/
            data x( 828),y( 828)/219.12d0,472.08d0/
            data x( 829),y( 829)/138.96d0,434.64d0/
            data x( 830),y( 830)/135.84d0,436.08d0/
            data x( 831),y( 831)/135.84d0,436.80d0/
            data x( 832),y( 832)/135.12d0,437.28d0/
            data x( 833),y( 833)/133.44d0,437.52d0/
            data x( 834),y( 834)/133.44d0,438.72d0/
            data x( 835),y( 835)/132.24d0,440.16d0/
            data x( 836),y( 836)/132.00d0,443.04d0/
            data x( 837),y( 837)/130.32d0,443.04d0/
            data x( 838),y( 838)/130.32d0,443.76d0/
            data x( 839),y( 839)/129.12d0,444.72d0/
            data x( 840),y( 840)/128.88d0,447.12d0/
            data x( 841),y( 841)/127.44d0,447.60d0/
            data x( 842),y( 842)/128.64d0,453.60d0/
            data x( 843),y( 843)/130.08d0,453.36d0/
            data x( 844),y( 844)/131.76d0,456.72d0/
            data x( 845),y( 845)/132.48d0,456.72d0/
            data x( 846),y( 846)/132.24d0,459.12d0/
            data x( 847),y( 847)/131.76d0,459.12d0/
            data x( 848),y( 848)/131.76d0,461.76d0/
            data x( 849),y( 849)/131.04d0,462.72d0/
            data x( 850),y( 850)/130.56d0,466.80d0/
            data x( 851),y( 851)/129.60d0,466.80d0/
            data x( 852),y( 852)/130.56d0,472.32d0/
            data x( 853),y( 853)/132.48d0,472.56d0/
            data x( 854),y( 854)/132.48d0,473.04d0/
            data x( 855),y( 855)/136.32d0,472.80d0/
            data x( 856),y( 856)/136.32d0,473.52d0/
            data x( 857),y( 857)/138.48d0,473.28d0/
            data x( 858),y( 858)/138.72d0,476.88d0/
            data x( 859),y( 859)/221.28d0,471.12d0/
            data x( 860),y( 860)/219.60d0,469.68d0/
            data x( 861),y( 861)/220.08d0,468.48d0/
            data x( 862),y( 862)/220.80d0,468.00d0/
            data x( 863),y( 863)/221.28d0,461.52d0/
            data x( 864),y( 864)/223.20d0,462.72d0/
            data x( 865),y( 865)/224.64d0,462.48d0/
            data x( 866),y( 866)/231.36d0,463.68d0/
            data x( 867),y( 867)/234.24d0,465.12d0/
            data x( 868),y( 868)/235.68d0,467.04d0/
            data x( 869),y( 869)/237.84d0,468.24d0/
            data x( 870),y( 870)/240.96d0,468.00d0/
            data x( 871),y( 871)/241.92d0,467.28d0/
            data x( 872),y( 872)/242.88d0,465.60d0/
            data x( 873),y( 873)/242.88d0,464.64d0/
            data x( 874),y( 874)/244.32d0,463.44d0/
            data x( 875),y( 875)/160.32d0,437.52d0/
            data x( 876),y( 876)/157.44d0,440.40d0/
            data x( 877),y( 877)/157.92d0,442.56d0/
            data x( 878),y( 878)/156.72d0,443.04d0/
            data x( 879),y( 879)/155.28d0,444.96d0/
            data x( 880),y( 880)/154.32d0,445.44d0/
            data x( 881),y( 881)/154.80d0,446.64d0/
            data x( 882),y( 882)/154.32d0,449.28d0/
            data x( 883),y( 883)/155.52d0,450.24d0/
            data x( 884),y( 884)/155.28d0,452.16d0/
            data x( 885),y( 885)/153.84d0,453.12d0/
            data x( 886),y( 886)/153.84d0,454.32d0/
            data x( 887),y( 887)/151.44d0,454.08d0/
            data x( 888),y( 888)/149.04d0,455.52d0/
            data x( 889),y( 889)/147.36d0,455.04d0/
            data x( 890),y( 890)/145.92d0,457.20d0/
            data x( 891),y( 891)/143.52d0,458.88d0/
            data x( 892),y( 892)/142.80d0,460.56d0/
            data x( 893),y( 893)/144.48d0,463.20d0/
            data x( 894),y( 894)/142.56d0,464.88d0/
            data x( 895),y( 895)/144.48d0,464.40d0/
            data x( 896),y( 896)/145.44d0,476.40d0/
            data x( 897),y( 897)/138.96d0,483.84d0/
            data x( 898),y( 898)/138.24d0,483.84d0/
            data x( 899),y( 899)/138.24d0,486.00d0/
            data x( 900),y( 900)/137.04d0,486.00d0/
            data x( 901),y( 901)/177.60d0,486.96d0/
            data x( 902),y( 902)/177.36d0,485.52d0/
            data x( 903),y( 903)/179.04d0,483.36d0/
            data x( 904),y( 904)/180.00d0,483.60d0/
            data x( 905),y( 905)/180.96d0,480.72d0/
            data x( 906),y( 906)/180.24d0,480.48d0/
            data x( 907),y( 907)/180.24d0,479.76d0/
            data x( 908),y( 908)/178.32d0,479.28d0/
            data x( 909),y( 909)/179.76d0,478.32d0/
            data x( 910),y( 910)/178.56d0,477.84d0/
            data x( 911),y( 911)/179.04d0,476.16d0/
            data x( 912),y( 912)/178.08d0,474.24d0/
            data x( 913),y( 913)/178.32d0,473.76d0/
            data x( 914),y( 914)/178.08d0,472.08d0/
            data x( 915),y( 915)/179.04d0,471.36d0/
            data x( 916),y( 916)/184.56d0,471.60d0/
            data x( 917),y( 917)/183.12d0,470.16d0/
            data x( 918),y( 918)/183.36d0,466.56d0/
            data x( 919),y( 919)/177.12d0,462.96d0/
            data x( 920),y( 920)/177.36d0,464.64d0/
            data x( 921),y( 921)/171.60d0,465.12d0/
            data x( 922),y( 922)/171.36d0,462.96d0/
            data x( 923),y( 923)/158.16d0,433.68d0/
            data x( 924),y( 924)/156.96d0,436.08d0/
            data x( 925),y( 925)/158.64d0,437.52d0/
            data x( 926),y( 926)/183.36d0,436.32d0/
            data x( 927),y( 927)/183.60d0,438.72d0/
            data x( 928),y( 928)/185.28d0,440.16d0/
            data x( 929),y( 929)/184.32d0,440.88d0/
            data x( 930),y( 930)/183.60d0,444.24d0/
            data x( 931),y( 931)/181.68d0,444.72d0/
            data x( 932),y( 932)/180.48d0,447.60d0/
            data x( 933),y( 933)/179.52d0,448.56d0/
            data x( 934),y( 934)/180.48d0,449.28d0/
            data x( 935),y( 935)/179.28d0,451.20d0/
            data x( 936),y( 936)/180.00d0,452.40d0/
            data x( 937),y( 937)/179.28d0,455.04d0/
            data x( 938),y( 938)/237.60d0,456.24d0/
            data x( 939),y( 939)/235.20d0,455.28d0/
            data x( 940),y( 940)/231.12d0,452.40d0/
            data x( 941),y( 941)/230.40d0,451.92d0/
            data x( 942),y( 942)/229.20d0,451.20d0/
            data x( 943),y( 943)/228.48d0,450.72d0/
            data x( 944),y( 944)/227.28d0,449.04d0/
            data x( 945),y( 945)/225.84d0,448.56d0/
            data x( 946),y( 946)/225.60d0,448.32d0/
            data x( 947),y( 947)/221.76d0,444.96d0/
            data x( 948),y( 948)/221.04d0,441.84d0/
            data x( 949),y( 949)/219.12d0,439.44d0/
            data x( 950),y( 950)/217.68d0,439.20d0/
            data x( 951),y( 951)/214.80d0,440.88d0/
            data x( 952),y( 952)/213.12d0,439.92d0/
            data x( 953),y( 953)/210.96d0,439.92d0/
            data x( 954),y( 954)/208.56d0,441.36d0/
            data x( 955),y( 955)/209.28d0,451.92d0/
            data x( 956),y( 956)/209.76d0,453.12d0/
            data x( 957),y( 957)/210.00d0,452.88d0/
            data x( 958),y( 958)/210.24d0,454.56d0/
            data x( 959),y( 959)/212.88d0,455.52d0/
            data x( 960),y( 960)/213.36d0,457.68d0/
            data x( 961),y( 961)/214.08d0,458.16d0/
            data x( 962),y( 962)/215.04d0,457.68d0/
            data x( 963),y( 963)/216.48d0,460.32d0/
            data x( 964),y( 964)/218.64d0,461.04d0/
            data x( 965),y( 965)/221.04d0,460.80d0/
            data x( 966),y( 966)/213.60d0,422.40d0/
            data x( 967),y( 967)/216.24d0,426.72d0/
            data x( 968),y( 968)/216.24d0,427.68d0/
            data x( 969),y( 969)/216.72d0,427.92d0/
            data x( 970),y( 970)/216.72d0,429.12d0/
            data x( 971),y( 971)/218.16d0,430.08d0/
            data x( 972),y( 972)/218.16d0,432.24d0/
            data x( 973),y( 973)/218.64d0,432.48d0/
            data x( 974),y( 974)/218.40d0,432.96d0/
            data x( 975),y( 975)/219.12d0,433.68d0/
            data x( 976),y( 976)/222.72d0,435.12d0/
            data x( 977),y( 977)/223.44d0,434.88d0/
            data x( 978),y( 978)/223.68d0,436.56d0/
            data x( 979),y( 979)/227.04d0,435.60d0/
            data x( 980),y( 980)/227.52d0,436.80d0/
            data x( 981),y( 981)/228.24d0,436.80d0/
            data x( 982),y( 982)/228.72d0,437.76d0/
            data x( 983),y( 983)/229.68d0,438.48d0/
            data x( 984),y( 984)/232.32d0,438.72d0/
            data x( 985),y( 985)/233.76d0,437.28d0/
            data x( 986),y( 986)/235.20d0,437.52d0/
            data x( 987),y( 987)/235.44d0,435.84d0/
            data x( 988),y( 988)/239.04d0,434.40d0/
            data x( 989),y( 989)/240.24d0,432.96d0/
            data x( 990),y( 990)/242.88d0,434.16d0/
            data x( 991),y( 991)/244.80d0,435.84d0/
            data x( 992),y( 992)/246.24d0,439.44d0/
            data x( 993),y( 993)/257.76d0,438.72d0/
            data x( 994),y( 994)/259.20d0,439.44d0/
            data x( 995),y( 995)/259.20d0,440.40d0/
            data x( 996),y( 996)/259.68d0,440.64d0/
            data x( 997),y( 997)/193.92d0,455.28d0/
            data x( 998),y( 998)/193.44d0,455.04d0/
            data x( 999),y( 999)/193.20d0,454.08d0/
            data x(1000),y(1000)/193.44d0,453.36d0/
            data x(1001),y(1001)/193.92d0,453.36d0/
            data x(1002),y(1002)/193.44d0,451.68d0/
            data x(1003),y(1003)/194.16d0,450.96d0/
            data x(1004),y(1004)/194.40d0,445.92d0/
            data x(1005),y(1005)/195.12d0,445.44d0/
            data x(1006),y(1006)/193.92d0,443.28d0/
            data x(1007),y(1007)/194.40d0,440.64d0/
            data x(1008),y(1008)/195.12d0,440.16d0/
            data x(1009),y(1009)/194.64d0,439.68d0/
            data x(1010),y(1010)/194.88d0,435.36d0/
            data x(1011),y(1011)/196.56d0,438.24d0/
            data x(1012),y(1012)/199.20d0,439.44d0/
            data x(1013),y(1013)/202.32d0,439.68d0/
            data x(1014),y(1014)/207.12d0,442.08d0/
            data x(1015),y(1015)/146.88d0,423.60d0/
            data x(1016),y(1016)/146.64d0,420.96d0/
            data x(1017),y(1017)/145.68d0,419.76d0/
            data x(1018),y(1018)/146.16d0,418.08d0/
            data x(1019),y(1019)/148.56d0,416.40d0/
            data x(1020),y(1020)/149.04d0,414.96d0/
            data x(1021),y(1021)/150.00d0,414.96d0/
            data x(1022),y(1022)/150.72d0,411.84d0/
            data x(1023),y(1023)/152.40d0,410.88d0/
            data x(1024),y(1024)/152.64d0,410.16d0/
            data x(1025),y(1025)/151.68d0,408.96d0/
            data x(1026),y(1026)/154.32d0,405.84d0/
            data x(1027),y(1027)/153.84d0,405.12d0/
            data x(1028),y(1028)/155.52d0,403.68d0/
            data x(1029),y(1029)/156.24d0,401.76d0/
            data x(1030),y(1030)/155.76d0,400.08d0/
            data x(1031),y(1031)/156.48d0,399.84d0/
            data x(1032),y(1032)/156.48d0,398.64d0/
            data x(1033),y(1033)/157.44d0,398.16d0/
            data x(1034),y(1034)/157.92d0,395.28d0/
            data x(1035),y(1035)/156.96d0,394.80d0/
            data x(1036),y(1036)/156.72d0,393.60d0/
            data x(1037),y(1037)/155.28d0,393.84d0/
            data x(1038),y(1038)/155.28d0,393.36d0/
            data x(1039),y(1039)/164.64d0,393.12d0/
            data x(1040),y(1040)/163.92d0,394.56d0/
            data x(1041),y(1041)/164.88d0,396.72d0/
            data x(1042),y(1042)/164.16d0,398.64d0/
            data x(1043),y(1043)/165.12d0,398.88d0/
            data x(1044),y(1044)/164.16d0,399.36d0/
            data x(1045),y(1045)/165.36d0,399.60d0/
            data x(1046),y(1046)/164.64d0,402.24d0/
            data x(1047),y(1047)/171.36d0,402.24d0/
            data x(1048),y(1048)/170.88d0,404.16d0/
            data x(1049),y(1049)/168.72d0,408.24d0/
            data x(1050),y(1050)/169.92d0,413.04d0/
            data x(1051),y(1051)/168.96d0,418.32d0/
            data x(1052),y(1052)/168.48d0,419.28d0/
            data x(1053),y(1053)/167.76d0,419.04d0/
            data x(1054),y(1054)/167.76d0,420.96d0/
            data x(1055),y(1055)/166.56d0,421.92d0/
            data x(1056),y(1056)/166.08d0,423.60d0/
            data x(1057),y(1057)/165.36d0,423.60d0/
            data x(1058),y(1058)/162.96d0,432.00d0/
            data x(1059),y(1059)/161.28d0,432.48d0/
            data x(1060),y(1060)/159.84d0,432.00d0/
            data x(1061),y(1061)/157.68d0,433.44d0/
            data x(1062),y(1062)/158.88d0,431.52d0/
            data x(1063),y(1063)/158.88d0,430.56d0/
            data x(1064),y(1064)/157.68d0,430.08d0/
            data x(1065),y(1065)/157.44d0,428.64d0/
            data x(1066),y(1066)/154.56d0,425.04d0/
            data x(1067),y(1067)/146.16d0,425.52d0/
            data x(1068),y(1068)/144.24d0,425.76d0/
            data x(1069),y(1069)/142.08d0,431.28d0/
            data x(1070),y(1070)/127.20d0,435.36d0/
            data x(1071),y(1071)/127.20d0,434.64d0/
            data x(1072),y(1072)/124.80d0,434.64d0/
            data x(1073),y(1073)/124.56d0,432.96d0/
            data x(1074),y(1074)/114.24d0,433.68d0/
            data x(1075),y(1075)/114.00d0,432.00d0/
            data x(1076),y(1076)/108.00d0,432.48d0/
            data x(1077),y(1077)/107.04d0,431.52d0/
            data x(1078),y(1078)/185.04d0,435.12d0/
            data x(1079),y(1079)/183.84d0,433.68d0/
            data x(1080),y(1080)/184.56d0,432.72d0/
            data x(1081),y(1081)/185.52d0,432.24d0/
            data x(1082),y(1082)/186.72d0,432.96d0/
            data x(1083),y(1083)/187.92d0,432.24d0/
            data x(1084),y(1084)/187.92d0,429.12d0/
            data x(1085),y(1085)/189.36d0,428.40d0/
            data x(1086),y(1086)/189.12d0,426.96d0/
            data x(1087),y(1087)/190.08d0,426.24d0/
            data x(1088),y(1088)/189.84d0,425.52d0/
            data x(1089),y(1089)/190.80d0,426.24d0/
            data x(1090),y(1090)/190.56d0,426.72d0/
            data x(1091),y(1091)/191.28d0,426.48d0/
            data x(1092),y(1092)/191.28d0,427.20d0/
            data x(1093),y(1093)/191.76d0,426.96d0/
            data x(1094),y(1094)/192.24d0,427.68d0/
            data x(1095),y(1095)/193.68d0,426.72d0/
            data x(1096),y(1096)/192.96d0,424.80d0/
            data x(1097),y(1097)/198.72d0,424.32d0/
            data x(1098),y(1098)/198.72d0,425.52d0/
            data x(1099),y(1099)/199.20d0,425.28d0/
            data x(1100),y(1100)/199.68d0,435.36d0/
            data x(1101),y(1101)/202.32d0,435.36d0/
            data x(1102),y(1102)/261.60d0,408.96d0/
            data x(1103),y(1103)/261.84d0,420.00d0/
            data x(1104),y(1104)/267.60d0,424.56d0/
            data x(1105),y(1105)/269.76d0,428.64d0/
            data x(1106),y(1106)/271.20d0,429.12d0/
            data x(1107),y(1107)/270.96d0,431.28d0/
            data x(1108),y(1108)/281.28d0,398.40d0/
            data x(1109),y(1109)/281.28d0,399.36d0/
            data x(1110),y(1110)/282.48d0,400.08d0/
            data x(1111),y(1111)/282.00d0,401.52d0/
            data x(1112),y(1112)/284.64d0,402.72d0/
            data x(1113),y(1113)/284.16d0,403.92d0/
            data x(1114),y(1114)/285.60d0,406.56d0/
            data x(1115),y(1115)/283.44d0,412.56d0/
            data x(1116),y(1116)/282.48d0,414.00d0/
            data x(1117),y(1117)/282.72d0,416.88d0/
            data x(1118),y(1118)/171.84d0,413.04d0/
            data x(1119),y(1119)/173.76d0,411.60d0/
            data x(1120),y(1120)/177.36d0,414.00d0/
            data x(1121),y(1121)/180.96d0,414.24d0/
            data x(1122),y(1122)/184.32d0,413.04d0/
            data x(1123),y(1123)/187.68d0,413.76d0/
            data x(1124),y(1124)/188.64d0,412.56d0/
            data x(1125),y(1125)/188.16d0,400.56d0/
            data x(1126),y(1126)/192.48d0,400.56d0/
            data x(1127),y(1127)/194.16d0,402.48d0/
            data x(1128),y(1128)/194.88d0,402.72d0/
            data x(1129),y(1129)/196.08d0,405.12d0/
            data x(1130),y(1130)/195.36d0,407.28d0/
            data x(1131),y(1131)/196.80d0,407.76d0/
            data x(1132),y(1132)/196.80d0,408.96d0/
            data x(1133),y(1133)/195.36d0,409.92d0/
            data x(1134),y(1134)/194.64d0,411.36d0/
            data x(1135),y(1135)/195.12d0,412.08d0/
            data x(1136),y(1136)/196.08d0,412.32d0/
            data x(1137),y(1137)/195.84d0,412.80d0/
            data x(1138),y(1138)/196.80d0,413.52d0/
            data x(1139),y(1139)/196.32d0,414.72d0/
            data x(1140),y(1140)/197.28d0,416.40d0/
            data x(1141),y(1141)/196.56d0,417.12d0/
            data x(1142),y(1142)/195.36d0,416.88d0/
            data x(1143),y(1143)/194.64d0,419.76d0/
            data x(1144),y(1144)/193.44d0,419.52d0/
            data x(1145),y(1145)/191.52d0,421.92d0/
            data x(1146),y(1146)/214.32d0,421.92d0/
            data x(1147),y(1147)/215.04d0,422.64d0/
            data x(1148),y(1148)/218.64d0,410.40d0/
            data x(1149),y(1149)/219.60d0,411.12d0/
            data x(1150),y(1150)/220.56d0,410.88d0/
            data x(1151),y(1151)/222.48d0,411.60d0/
            data x(1152),y(1152)/222.96d0,410.64d0/
            data x(1153),y(1153)/224.16d0,411.60d0/
            data x(1154),y(1154)/224.88d0,411.12d0/
            data x(1155),y(1155)/225.36d0,412.32d0/
            data x(1156),y(1156)/225.84d0,411.60d0/
            data x(1157),y(1157)/226.80d0,413.04d0/
            data x(1158),y(1158)/228.48d0,413.28d0/
            data x(1159),y(1159)/229.44d0,412.56d0/
            data x(1160),y(1160)/232.32d0,412.08d0/
            data x(1161),y(1161)/234.24d0,410.40d0/
            data x(1162),y(1162)/237.60d0,409.44d0/
            data x(1163),y(1163)/241.68d0,409.68d0/
            data x(1164),y(1164)/248.16d0,411.60d0/
            data x(1165),y(1165)/251.04d0,411.60d0/
            data x(1166),y(1166)/254.64d0,414.00d0/
            data x(1167),y(1167)/255.60d0,415.92d0/
            data x(1168),y(1168)/258.72d0,416.16d0/
            data x(1169),y(1169)/259.92d0,418.56d0/
            data x(1170),y(1170)/259.68d0,419.28d0/
            data x(1171),y(1171)/260.16d0,420.24d0/
            data x(1172),y(1172)/233.76d0,371.04d0/
            data x(1173),y(1173)/245.52d0,359.04d0/
            data x(1174),y(1174)/246.24d0,360.00d0/
            data x(1175),y(1175)/245.52d0,361.20d0/
            data x(1176),y(1176)/245.76d0,361.68d0/
            data x(1177),y(1177)/247.44d0,361.20d0/
            data x(1178),y(1178)/248.40d0,359.76d0/
            data x(1179),y(1179)/249.36d0,360.00d0/
            data x(1180),y(1180)/247.92d0,364.08d0/
            data x(1181),y(1181)/249.36d0,364.56d0/
            data x(1182),y(1182)/250.56d0,364.32d0/
            data x(1183),y(1183)/250.80d0,365.04d0/
            data x(1184),y(1184)/251.52d0,364.56d0/
            data x(1185),y(1185)/252.72d0,366.24d0/
            data x(1186),y(1186)/255.12d0,366.24d0/
            data x(1187),y(1187)/255.84d0,367.92d0/
            data x(1188),y(1188)/258.00d0,366.96d0/
            data x(1189),y(1189)/260.16d0,369.60d0/
            data x(1190),y(1190)/261.12d0,369.36d0/
            data x(1191),y(1191)/261.36d0,368.64d0/
            data x(1192),y(1192)/262.56d0,368.40d0/
            data x(1193),y(1193)/266.16d0,365.52d0/
            data x(1194),y(1194)/267.84d0,365.28d0/
            data x(1195),y(1195)/269.52d0,366.00d0/
            data x(1196),y(1196)/270.24d0,365.28d0/
            data x(1197),y(1197)/271.44d0,365.76d0/
            data x(1198),y(1198)/272.16d0,365.04d0/
            data x(1199),y(1199)/276.72d0,366.96d0/
            data x(1200),y(1200)/278.16d0,368.88d0/
            data x(1201),y(1201)/282.00d0,370.80d0/
            data x(1202),y(1202)/282.72d0,372.48d0/
            data x(1203),y(1203)/283.44d0,372.96d0/
            data x(1204),y(1204)/284.88d0,372.96d0/
            data x(1205),y(1205)/287.52d0,370.32d0/
            data x(1206),y(1206)/288.24d0,371.04d0/
            data x(1207),y(1207)/289.20d0,370.80d0/
            data x(1208),y(1208)/288.96d0,370.32d0/
            data x(1209),y(1209)/289.92d0,370.56d0/
            data x(1210),y(1210)/290.88d0,368.88d0/
            data x(1211),y(1211)/292.08d0,369.12d0/
            data x(1212),y(1212)/294.96d0,366.00d0/
            data x(1213),y(1213)/296.16d0,363.60d0/
            data x(1214),y(1214)/296.88d0,363.60d0/
            data x(1215),y(1215)/300.00d0,366.48d0/
            data x(1216),y(1216)/299.28d0,367.92d0/
            data x(1217),y(1217)/300.24d0,369.12d0/
            data x(1218),y(1218)/299.28d0,369.60d0/
            data x(1219),y(1219)/300.00d0,371.28d0/
            data x(1220),y(1220)/299.04d0,372.96d0/
            data x(1221),y(1221)/297.60d0,373.20d0/
            data x(1222),y(1222)/297.36d0,373.92d0/
            data x(1223),y(1223)/295.44d0,375.12d0/
            data x(1224),y(1224)/294.96d0,376.56d0/
            data x(1225),y(1225)/295.20d0,377.28d0/
            data x(1226),y(1226)/296.16d0,376.80d0/
            data x(1227),y(1227)/297.36d0,377.76d0/
            data x(1228),y(1228)/298.08d0,375.36d0/
            data x(1229),y(1229)/300.48d0,376.32d0/
            data x(1230),y(1230)/297.60d0,378.96d0/
            data x(1231),y(1231)/297.84d0,380.40d0/
            data x(1232),y(1232)/296.88d0,383.52d0/
            data x(1233),y(1233)/294.24d0,384.00d0/
            data x(1234),y(1234)/293.28d0,385.68d0/
            data x(1235),y(1235)/290.88d0,386.64d0/
            data x(1236),y(1236)/290.64d0,388.08d0/
            data x(1237),y(1237)/288.48d0,389.52d0/
            data x(1238),y(1238)/289.92d0,390.48d0/
            data x(1239),y(1239)/290.40d0,391.68d0/
            data x(1240),y(1240)/289.44d0,391.92d0/
            data x(1241),y(1241)/289.68d0,393.60d0/
            data x(1242),y(1242)/288.72d0,394.56d0/
            data x(1243),y(1243)/287.28d0,394.08d0/
            data x(1244),y(1244)/286.56d0,391.44d0/
            data x(1245),y(1245)/287.04d0,390.00d0/
            data x(1246),y(1246)/286.80d0,386.40d0/
            data x(1247),y(1247)/283.68d0,387.60d0/
            data x(1248),y(1248)/283.20d0,389.04d0/
            data x(1249),y(1249)/281.04d0,390.24d0/
            data x(1250),y(1250)/281.76d0,391.92d0/
            data x(1251),y(1251)/282.48d0,392.16d0/
            data x(1252),y(1252)/282.00d0,393.12d0/
            data x(1253),y(1253)/282.24d0,393.84d0/
            data x(1254),y(1254)/281.04d0,395.04d0/
            data x(1255),y(1255)/281.04d0,396.72d0/
            data x(1256),y(1256)/280.80d0,397.44d0/
            data x(1257),y(1257)/279.36d0,399.84d0/
            data x(1258),y(1258)/278.16d0,403.20d0/
            data x(1259),y(1259)/277.68d0,403.44d0/
            data x(1260),y(1260)/273.60d0,402.72d0/
            data x(1261),y(1261)/273.12d0,401.76d0/
            data x(1262),y(1262)/270.48d0,400.32d0/
            data x(1263),y(1263)/264.00d0,404.64d0/
            data x(1264),y(1264)/262.32d0,405.60d0/
            data x(1265),y(1265)/261.36d0,405.36d0/
            data x(1266),y(1266)/259.92d0,408.72d0/
            data x(1267),y(1267)/259.20d0,408.00d0/
            data x(1268),y(1268)/253.20d0,408.00d0/
            data x(1269),y(1269)/249.84d0,406.80d0/
            data x(1270),y(1270)/247.68d0,406.56d0/
            data x(1271),y(1271)/245.76d0,407.28d0/
            data x(1272),y(1272)/244.08d0,406.32d0/
            data x(1273),y(1273)/243.84d0,406.08d0/
            data x(1274),y(1274)/242.16d0,405.84d0/
            data x(1275),y(1275)/241.20d0,404.64d0/
            data x(1276),y(1276)/237.12d0,403.20d0/
            data x(1277),y(1277)/235.92d0,400.08d0/
            data x(1278),y(1278)/231.12d0,398.40d0/
            data x(1279),y(1279)/230.16d0,397.44d0/
            data x(1280),y(1280)/228.24d0,397.20d0/
            data x(1281),y(1281)/224.40d0,393.84d0/
            data x(1282),y(1282)/222.48d0,394.08d0/
            data x(1283),y(1283)/220.56d0,393.36d0/
            data x(1284),y(1284)/219.60d0,393.84d0/
            data x(1285),y(1285)/218.16d0,398.40d0/
            data x(1286),y(1286)/222.00d0,385.44d0/
            data x(1287),y(1287)/239.28d0,375.60d0/
            data x(1288),y(1288)/239.52d0,377.28d0/
            data x(1289),y(1289)/240.48d0,377.52d0/
            data x(1290),y(1290)/240.72d0,378.24d0/
            data x(1291),y(1291)/240.00d0,378.96d0/
            data x(1292),y(1292)/241.68d0,380.40d0/
            data x(1293),y(1293)/242.88d0,380.88d0/
            data x(1294),y(1294)/245.52d0,385.20d0/
            data x(1295),y(1295)/245.52d0,386.16d0/
            data x(1296),y(1296)/246.72d0,386.88d0/
            data x(1297),y(1297)/247.44d0,388.80d0/
            data x(1298),y(1298)/248.88d0,390.00d0/
            data x(1299),y(1299)/256.56d0,401.04d0/
            data x(1300),y(1300)/257.76d0,402.00d0/
            data x(1301),y(1301)/260.40d0,402.72d0/
            data x(1302),y(1302)/130.56d0,403.92d0/
            data x(1303),y(1303)/133.20d0,404.16d0/
            data x(1304),y(1304)/140.16d0,396.96d0/
            data x(1305),y(1305)/145.20d0,395.28d0/
            data x(1306),y(1306)/147.60d0,395.76d0/
            data x(1307),y(1307)/148.08d0,394.08d0/
            data x(1308),y(1308)/149.76d0,393.12d0/
            data x(1309),y(1309)/150.96d0,390.96d0/
            data x(1310),y(1310)/157.20d0,388.32d0/
            data x(1311),y(1311)/216.96d0,397.92d0/
            data x(1312),y(1312)/216.24d0,398.64d0/
            data x(1313),y(1313)/214.80d0,398.16d0/
            data x(1314),y(1314)/211.68d0,396.00d0/
            data x(1315),y(1315)/208.80d0,395.76d0/
            data x(1316),y(1316)/206.40d0,396.48d0/
            data x(1317),y(1317)/205.68d0,395.52d0/
            data x(1318),y(1318)/203.28d0,395.04d0/
            data x(1319),y(1319)/200.88d0,395.28d0/
            data x(1320),y(1320)/199.92d0,395.76d0/
            data x(1321),y(1321)/199.92d0,396.72d0/
            data x(1322),y(1322)/197.52d0,396.96d0/
            data x(1323),y(1323)/196.08d0,395.28d0/
            data x(1324),y(1324)/196.08d0,394.08d0/
            data x(1325),y(1325)/194.88d0,393.60d0/
            data x(1326),y(1326)/194.64d0,391.20d0/
            data x(1327),y(1327)/192.24d0,389.52d0/
            data x(1328),y(1328)/192.00d0,389.04d0/
            data x(1329),y(1329)/192.96d0,388.32d0/
            data x(1330),y(1330)/192.48d0,388.08d0/
            data x(1331),y(1331)/190.56d0,388.56d0/
            data x(1332),y(1332)/189.60d0,387.84d0/
            data x(1333),y(1333)/188.40d0,388.08d0/
            data x(1334),y(1334)/186.96d0,385.44d0/
            data x(1335),y(1335)/183.12d0,384.00d0/
            data x(1336),y(1336)/181.20d0,384.48d0/
            data x(1337),y(1337)/179.52d0,386.88d0/
            data x(1338),y(1338)/191.04d0,394.08d0/
            data x(1339),y(1339)/188.88d0,393.36d0/
            data x(1340),y(1340)/187.68d0,391.44d0/
            data x(1341),y(1341)/186.48d0,387.84d0/
            data x(1342),y(1342)/184.80d0,387.84d0/
            data x(1343),y(1343)/182.40d0,386.40d0/
            data x(1344),y(1344)/180.00d0,387.12d0/
            data x(1345),y(1345)/178.80d0,386.16d0/
            data x(1346),y(1346)/177.36d0,385.92d0/
            data x(1347),y(1347)/170.64d0,387.36d0/
            data x(1348),y(1348)/166.56d0,385.68d0/
            data x(1349),y(1349)/164.64d0,387.12d0/
            data x(1350),y(1350)/152.88d0,383.28d0/
            data x(1351),y(1351)/153.12d0,380.40d0/
            data x(1352),y(1352)/155.52d0,377.76d0/
            data x(1353),y(1353)/156.72d0,373.68d0/
            data x(1354),y(1354)/159.36d0,367.44d0/
            data x(1355),y(1355)/192.72d0,372.00d0/
            data x(1356),y(1356)/192.00d0,355.92d0/
            data x(1357),y(1357)/193.68d0,354.96d0/
            data x(1358),y(1358)/194.64d0,354.96d0/
            data x(1359),y(1359)/195.60d0,352.56d0/
            data x(1360),y(1360)/206.64d0,362.40d0/
            data x(1361),y(1361)/207.12d0,362.40d0/
            data x(1362),y(1362)/207.84d0,363.84d0/
            data x(1363),y(1363)/208.56d0,364.32d0/
            data x(1364),y(1364)/209.28d0,363.60d0/
            data x(1365),y(1365)/209.52d0,364.80d0/
            data x(1366),y(1366)/210.00d0,364.32d0/
            data x(1367),y(1367)/210.72d0,365.28d0/
            data x(1368),y(1368)/212.40d0,365.76d0/
            data x(1369),y(1369)/212.88d0,366.72d0/
            data x(1370),y(1370)/214.56d0,366.24d0/
            data x(1371),y(1371)/215.28d0,366.96d0/
            data x(1372),y(1372)/215.76d0,366.24d0/
            data x(1373),y(1373)/216.48d0,366.48d0/
            data x(1374),y(1374)/216.96d0,367.20d0/
            data x(1375),y(1375)/217.44d0,366.96d0/
            data x(1376),y(1376)/218.40d0,367.44d0/
            data x(1377),y(1377)/219.84d0,366.24d0/
            data x(1378),y(1378)/221.52d0,366.48d0/
            data x(1379),y(1379)/153.60d0,376.08d0/
            data x(1380),y(1380)/150.72d0,375.12d0/
            data x(1381),y(1381)/150.48d0,374.64d0/
            data x(1382),y(1382)/148.56d0,374.88d0/
            data x(1383),y(1383)/160.56d0,378.72d0/
            data x(1384),y(1384)/162.72d0,376.80d0/
            data x(1385),y(1385)/164.16d0,373.44d0/
            data x(1386),y(1386)/165.60d0,373.20d0/
            data x(1387),y(1387)/166.08d0,372.48d0/
            data x(1388),y(1388)/170.64d0,371.76d0/
            data x(1389),y(1389)/172.56d0,370.08d0/
            data x(1390),y(1390)/171.84d0,369.36d0/
            data x(1391),y(1391)/172.32d0,368.16d0/
            data x(1392),y(1392)/174.00d0,367.20d0/
            data x(1393),y(1393)/193.68d0,373.44d0/
            data x(1394),y(1394)/193.44d0,374.16d0/
            data x(1395),y(1395)/191.76d0,374.40d0/
            data x(1396),y(1396)/192.96d0,378.00d0/
            data x(1397),y(1397)/192.00d0,381.60d0/
            data x(1398),y(1398)/192.48d0,382.80d0/
            data x(1399),y(1399)/192.24d0,383.04d0/
            data x(1400),y(1400)/193.20d0,383.52d0/
            data x(1401),y(1401)/192.72d0,384.72d0/
            data x(1402),y(1402)/192.00d0,384.96d0/
            data x(1403),y(1403)/192.72d0,387.12d0/
            data x(1404),y(1404)/318.96d0,347.52d0/
            data x(1405),y(1405)/318.00d0,348.48d0/
            data x(1406),y(1406)/315.36d0,347.76d0/
            data x(1407),y(1407)/315.12d0,349.44d0/
            data x(1408),y(1408)/312.72d0,352.08d0/
            data x(1409),y(1409)/311.28d0,353.04d0/
            data x(1410),y(1410)/309.84d0,352.56d0/
            data x(1411),y(1411)/307.92d0,354.24d0/
            data x(1412),y(1412)/307.68d0,356.40d0/
            data x(1413),y(1413)/306.24d0,358.08d0/
            data x(1414),y(1414)/306.00d0,360.00d0/
            data x(1415),y(1415)/303.84d0,362.40d0/
            data x(1416),y(1416)/300.24d0,363.12d0/
            data x(1417),y(1417)/299.04d0,362.16d0/
            data x(1418),y(1418)/297.84d0,363.12d0/
            data x(1419),y(1419)/297.12d0,362.64d0/
            data x(1420),y(1420)/166.56d0,354.96d0/
            data x(1421),y(1421)/167.76d0,353.52d0/
            data x(1422),y(1422)/169.20d0,352.56d0/
            data x(1423),y(1423)/173.76d0,352.80d0/
            data x(1424),y(1424)/174.96d0,351.84d0/
            data x(1425),y(1425)/177.84d0,353.52d0/
            data x(1426),y(1426)/195.12d0,351.84d0/
            data x(1427),y(1427)/196.08d0,350.64d0/
            data x(1428),y(1428)/195.60d0,348.96d0/
            data x(1429),y(1429)/196.32d0,347.52d0/
            data x(1430),y(1430)/198.24d0,346.80d0/
            data x(1431),y(1431)/197.76d0,345.12d0/
            data x(1432),y(1432)/198.24d0,342.72d0/
            data x(1433),y(1433)/195.84d0,341.04d0/
            data x(1434),y(1434)/195.84d0,339.12d0/
            data x(1435),y(1435)/197.28d0,336.96d0/
            data x(1436),y(1436)/197.76d0,333.84d0/
            data x(1437),y(1437)/198.24d0,333.12d0/
            data x(1438),y(1438)/199.92d0,335.04d0/
            data x(1439),y(1439)/201.84d0,334.08d0/
            data x(1440),y(1440)/203.04d0,334.08d0/
            data x(1441),y(1441)/203.28d0,334.80d0/
            data x(1442),y(1442)/205.92d0,332.16d0/
            data x(1443),y(1443)/218.64d0,343.44d0/
            data x(1444),y(1444)/217.68d0,344.88d0/
            data x(1445),y(1445)/218.16d0,345.36d0/
            data x(1446),y(1446)/217.20d0,346.08d0/
            data x(1447),y(1447)/217.68d0,346.80d0/
            data x(1448),y(1448)/250.08d0,348.72d0/
            data x(1449),y(1449)/250.08d0,346.80d0/
            data x(1450),y(1450)/254.40d0,339.12d0/
            data x(1451),y(1451)/254.40d0,337.44d0/
            data x(1452),y(1452)/258.48d0,334.80d0/
            data x(1453),y(1453)/259.92d0,332.64d0/
            data x(1454),y(1454)/273.60d0,345.60d0/
            data x(1455),y(1455)/278.88d0,345.36d0/
            data x(1456),y(1456)/278.88d0,348.00d0/
            data x(1457),y(1457)/280.56d0,347.76d0/
            data x(1458),y(1458)/280.56d0,349.68d0/
            data x(1459),y(1459)/282.00d0,349.68d0/
            data x(1460),y(1460)/282.24d0,353.76d0/
            data x(1461),y(1461)/294.00d0,341.04d0/
            data x(1462),y(1462)/294.00d0,340.56d0/
            data x(1463),y(1463)/293.52d0,340.32d0/
            data x(1464),y(1464)/294.00d0,339.84d0/
            data x(1465),y(1465)/293.04d0,338.64d0/
            data x(1466),y(1466)/293.52d0,335.76d0/
            data x(1467),y(1467)/292.80d0,334.08d0/
            data x(1468),y(1468)/293.28d0,333.36d0/
            data x(1469),y(1469)/291.60d0,331.20d0/
            data x(1470),y(1470)/290.40d0,330.00d0/
            data x(1471),y(1471)/288.72d0,330.96d0/
            data x(1472),y(1472)/288.24d0,329.76d0/
            data x(1473),y(1473)/287.04d0,329.76d0/
            data x(1474),y(1474)/286.32d0,327.84d0/
            data x(1475),y(1475)/285.12d0,330.24d0/
            data x(1476),y(1476)/284.16d0,329.04d0/
            data x(1477),y(1477)/282.96d0,330.00d0/
            data x(1478),y(1478)/282.48d0,329.76d0/
            data x(1479),y(1479)/282.00d0,328.56d0/
            data x(1480),y(1480)/283.44d0,327.36d0/
            data x(1481),y(1481)/282.48d0,326.64d0/
            data x(1482),y(1482)/282.48d0,325.68d0/
            data x(1483),y(1483)/280.32d0,325.68d0/
            data x(1484),y(1484)/279.12d0,323.04d0/
            data x(1485),y(1485)/278.88d0,324.24d0/
            data x(1486),y(1486)/278.16d0,324.24d0/
            data x(1487),y(1487)/277.92d0,322.08d0/
            data x(1488),y(1488)/276.24d0,322.08d0/
            data x(1489),y(1489)/274.56d0,320.64d0/
            data x(1490),y(1490)/274.08d0,319.92d0/
            data x(1491),y(1491)/274.32d0,318.72d0/
            data x(1492),y(1492)/271.92d0,316.08d0/
            data x(1493),y(1493)/271.68d0,314.88d0/
            data x(1494),y(1494)/270.24d0,313.20d0/
            data x(1495),y(1495)/269.04d0,313.92d0/
            data x(1496),y(1496)/268.32d0,313.44d0/
            data x(1497),y(1497)/267.12d0,313.92d0/
            data x(1498),y(1498)/264.48d0,312.72d0/
            data x(1499),y(1499)/263.28d0,312.48d0/
            data x(1500),y(1500)/262.56d0,312.96d0/
            data x(1501),y(1501)/260.40d0,311.76d0/
            data x(1502),y(1502)/257.76d0,312.24d0/
            data x(1503),y(1503)/254.64d0,310.80d0/
            data x(1504),y(1504)/253.68d0,311.04d0/
            data x(1505),y(1505)/252.96d0,309.60d0/
            data x(1506),y(1506)/251.76d0,310.56d0/
            data x(1507),y(1507)/250.80d0,309.36d0/
            data x(1508),y(1508)/248.64d0,309.60d0/
            data x(1509),y(1509)/248.40d0,309.12d0/
            data x(1510),y(1510)/246.00d0,310.80d0/
            data x(1511),y(1511)/244.56d0,310.56d0/
            data x(1512),y(1512)/244.32d0,312.24d0/
            data x(1513),y(1513)/241.20d0,314.88d0/
            data x(1514),y(1514)/240.48d0,316.80d0/
            data x(1515),y(1515)/241.20d0,317.76d0/
            data x(1516),y(1516)/238.56d0,323.52d0/
            data x(1517),y(1517)/237.36d0,324.24d0/
            data x(1518),y(1518)/237.12d0,325.68d0/
            data x(1519),y(1519)/240.24d0,328.32d0/
            data x(1520),y(1520)/244.80d0,330.00d0/
            data x(1521),y(1521)/246.96d0,330.00d0/
            data x(1522),y(1522)/249.12d0,331.44d0/
            data x(1523),y(1523)/257.52d0,331.92d0/
            data x(1524),y(1524)/167.04d0,352.80d0/
            data x(1525),y(1525)/166.08d0,353.28d0/
            data x(1526),y(1526)/165.60d0,352.32d0/
            data x(1527),y(1527)/164.16d0,352.56d0/
            data x(1528),y(1528)/162.72d0,351.36d0/
            data x(1529),y(1529)/162.00d0,347.28d0/
            data x(1530),y(1530)/162.96d0,345.12d0/
            data x(1531),y(1531)/162.24d0,344.88d0/
            data x(1532),y(1532)/162.48d0,344.16d0/
            data x(1533),y(1533)/163.44d0,344.16d0/
            data x(1534),y(1534)/163.92d0,342.96d0/
            data x(1535),y(1535)/163.68d0,338.64d0/
            data x(1536),y(1536)/159.36d0,339.12d0/
            data x(1537),y(1537)/159.36d0,337.44d0/
            data x(1538),y(1538)/156.00d0,337.68d0/
            data x(1539),y(1539)/336.96d0,306.00d0/
            data x(1540),y(1540)/336.96d0,308.64d0/
            data x(1541),y(1541)/336.00d0,310.56d0/
            data x(1542),y(1542)/337.44d0,311.52d0/
            data x(1543),y(1543)/336.96d0,313.20d0/
            data x(1544),y(1544)/337.44d0,313.68d0/
            data x(1545),y(1545)/334.80d0,319.44d0/
            data x(1546),y(1546)/334.08d0,323.52d0/
            data x(1547),y(1547)/331.92d0,324.72d0/
            data x(1548),y(1548)/329.76d0,326.64d0/
            data x(1549),y(1549)/327.36d0,327.36d0/
            data x(1550),y(1550)/327.12d0,328.08d0/
            data x(1551),y(1551)/324.24d0,328.56d0/
            data x(1552),y(1552)/323.04d0,332.64d0/
            data x(1553),y(1553)/322.80d0,334.32d0/
            data x(1554),y(1554)/323.52d0,335.76d0/
            data x(1555),y(1555)/322.56d0,337.68d0/
            data x(1556),y(1556)/321.60d0,339.12d0/
            data x(1557),y(1557)/320.40d0,338.64d0/
            data x(1558),y(1558)/318.48d0,340.08d0/
            data x(1559),y(1559)/319.20d0,341.52d0/
            data x(1560),y(1560)/318.24d0,343.44d0/
            data x(1561),y(1561)/318.72d0,344.88d0/
            data x(1562),y(1562)/319.20d0,344.88d0/
            data x(1563),y(1563)/319.68d0,345.60d0/
            data x(1564),y(1564)/206.16d0,322.08d0/
            data x(1565),y(1565)/204.72d0,323.28d0/
            data x(1566),y(1566)/205.44d0,324.24d0/
            data x(1567),y(1567)/204.96d0,325.92d0/
            data x(1568),y(1568)/205.92d0,326.40d0/
            data x(1569),y(1569)/205.92d0,327.12d0/
            data x(1570),y(1570)/206.64d0,327.84d0/
            data x(1571),y(1571)/205.20d0,329.28d0/
            data x(1572),y(1572)/206.40d0,331.20d0/
            data x(1573),y(1573)/179.28d0,318.96d0/
            data x(1574),y(1574)/180.24d0,318.96d0/
            data x(1575),y(1575)/181.20d0,319.92d0/
            data x(1576),y(1576)/183.36d0,320.64d0/
            data x(1577),y(1577)/183.36d0,320.16d0/
            data x(1578),y(1578)/184.32d0,319.92d0/
            data x(1579),y(1579)/185.52d0,320.40d0/
            data x(1580),y(1580)/186.00d0,319.20d0/
            data x(1581),y(1581)/186.96d0,320.16d0/
            data x(1582),y(1582)/189.12d0,319.20d0/
            data x(1583),y(1583)/188.64d0,320.88d0/
            data x(1584),y(1584)/187.20d0,321.60d0/
            data x(1585),y(1585)/186.00d0,321.36d0/
            data x(1586),y(1586)/185.52d0,323.04d0/
            data x(1587),y(1587)/183.84d0,323.76d0/
            data x(1588),y(1588)/184.08d0,324.48d0/
            data x(1589),y(1589)/182.88d0,325.20d0/
            data x(1590),y(1590)/183.12d0,325.92d0/
            data x(1591),y(1591)/181.68d0,328.32d0/
            data x(1592),y(1592)/179.28d0,329.28d0/
            data x(1593),y(1593)/177.84d0,330.96d0/
            data x(1594),y(1594)/170.88d0,334.32d0/
            data x(1595),y(1595)/168.24d0,338.16d0/
            data x(1596),y(1596)/166.32d0,339.36d0/
            data x(1597),y(1597)/219.36d0,308.64d0/
            data x(1598),y(1598)/209.52d0,314.40d0/
            data x(1599),y(1599)/207.36d0,318.96d0/
            data x(1600),y(1600)/205.44d0,319.68d0/
            data x(1601),y(1601)/205.20d0,320.40d0/
            data x(1602),y(1602)/196.80d0,322.32d0/
            data x(1603),y(1603)/195.60d0,324.00d0/
            data x(1604),y(1604)/194.64d0,323.52d0/
            data x(1605),y(1605)/193.44d0,324.00d0/
            data x(1606),y(1606)/190.08d0,320.16d0/
            data x(1607),y(1607)/189.84d0,318.96d0/
            data x(1608),y(1608)/352.80d0,250.56d0/
            data x(1609),y(1609)/352.56d0,251.76d0/
            data x(1610),y(1610)/353.04d0,252.72d0/
            data x(1611),y(1611)/352.80d0,254.64d0/
            data x(1612),y(1612)/353.52d0,255.60d0/
            data x(1613),y(1613)/354.24d0,255.60d0/
            data x(1614),y(1614)/354.24d0,258.96d0/
            data x(1615),y(1615)/352.56d0,260.16d0/
            data x(1616),y(1616)/353.28d0,261.60d0/
            data x(1617),y(1617)/351.84d0,263.52d0/
            data x(1618),y(1618)/351.12d0,267.60d0/
            data x(1619),y(1619)/350.40d0,268.32d0/
            data x(1620),y(1620)/350.16d0,270.72d0/
            data x(1621),y(1621)/350.88d0,272.64d0/
            data x(1622),y(1622)/348.72d0,275.04d0/
            data x(1623),y(1623)/348.24d0,277.20d0/
            data x(1624),y(1624)/348.00d0,279.36d0/
            data x(1625),y(1625)/348.48d0,281.04d0/
            data x(1626),y(1626)/348.96d0,281.52d0/
            data x(1627),y(1627)/348.96d0,282.48d0/
            data x(1628),y(1628)/348.00d0,282.72d0/
            data x(1629),y(1629)/346.32d0,285.36d0/
            data x(1630),y(1630)/347.04d0,285.84d0/
            data x(1631),y(1631)/347.28d0,286.80d0/
            data x(1632),y(1632)/346.32d0,287.52d0/
            data x(1633),y(1633)/343.92d0,287.52d0/
            data x(1634),y(1634)/343.68d0,288.72d0/
            data x(1635),y(1635)/342.24d0,290.16d0/
            data x(1636),y(1636)/342.72d0,292.56d0/
            data x(1637),y(1637)/342.00d0,294.00d0/
            data x(1638),y(1638)/340.32d0,295.20d0/
            data x(1639),y(1639)/341.04d0,297.36d0/
            data x(1640),y(1640)/338.40d0,300.24d0/
            data x(1641),y(1641)/338.88d0,301.20d0/
            data x(1642),y(1642)/337.92d0,301.44d0/
            data x(1643),y(1643)/337.20d0,302.40d0/
            data x(1644),y(1644)/338.16d0,302.88d0/
            data x(1645),y(1645)/338.40d0,303.84d0/
            data x(1646),y(1646)/328.32d0,306.00d0/
            data x(1647),y(1647)/328.32d0,305.52d0/
            data x(1648),y(1648)/308.64d0,306.24d0/
            data x(1649),y(1649)/308.40d0,300.96d0/
            data x(1650),y(1650)/293.76d0,301.68d0/
            data x(1651),y(1651)/293.52d0,296.64d0/
            data x(1652),y(1652)/285.84d0,296.88d0/
            data x(1653),y(1653)/280.80d0,292.08d0/
            data x(1654),y(1654)/276.72d0,288.24d0/
            data x(1655),y(1655)/276.24d0,288.96d0/
            data x(1656),y(1656)/274.56d0,288.96d0/
            data x(1657),y(1657)/272.64d0,288.48d0/
            data x(1658),y(1658)/272.16d0,287.28d0/
            data x(1659),y(1659)/263.04d0,287.52d0/
            data x(1660),y(1660)/262.56d0,275.04d0/
            data x(1661),y(1661)/255.12d0,268.56d0/
            data x(1662),y(1662)/254.40d0,268.56d0/
            data x(1663),y(1663)/254.40d0,267.84d0/
            data x(1664),y(1664)/245.28d0,259.92d0/
            data x(1665),y(1665)/233.52d0,293.76d0/
            data x(1666),y(1666)/233.28d0,284.40d0/
            data x(1667),y(1667)/229.92d0,283.44d0/
            data x(1668),y(1668)/228.96d0,282.48d0/
            data x(1669),y(1669)/229.20d0,281.04d0/
            data x(1670),y(1670)/226.32d0,283.68d0/
            data x(1671),y(1671)/225.84d0,283.68d0/
            data x(1672),y(1672)/228.00d0,278.88d0/
            data x(1673),y(1673)/227.76d0,277.68d0/
            data x(1674),y(1674)/227.28d0,277.20d0/
            data x(1675),y(1675)/225.36d0,277.44d0/
            data x(1676),y(1676)/220.56d0,282.96d0/
            data x(1677),y(1677)/219.36d0,282.96d0/
            data x(1678),y(1678)/218.16d0,284.16d0/
            data x(1679),y(1679)/216.48d0,282.24d0/
            data x(1680),y(1680)/215.04d0,283.20d0/
            data x(1681),y(1681)/214.80d0,282.24d0/
            data x(1682),y(1682)/213.12d0,281.52d0/
            data x(1683),y(1683)/213.12d0,282.24d0/
            data x(1684),y(1684)/212.40d0,282.24d0/
            data x(1685),y(1685)/212.40d0,285.12d0/
            data x(1686),y(1686)/204.96d0,294.00d0/
            data x(1687),y(1687)/204.00d0,294.00d0/
            data x(1688),y(1688)/203.76d0,295.68d0/
            data x(1689),y(1689)/200.40d0,295.92d0/
            data x(1690),y(1690)/201.12d0,297.36d0/
            data x(1691),y(1691)/199.68d0,299.04d0/
            data x(1692),y(1692)/200.16d0,302.16d0/
            data x(1693),y(1693)/199.44d0,303.84d0/
            data x(1694),y(1694)/198.48d0,304.80d0/
            data x(1695),y(1695)/196.80d0,304.80d0/
            data x(1696),y(1696)/196.32d0,306.00d0/
            data x(1697),y(1697)/193.68d0,306.72d0/
            data x(1698),y(1698)/192.96d0,308.40d0/
            data x(1699),y(1699)/194.64d0,308.88d0/
            data x(1700),y(1700)/193.44d0,311.04d0/
            data x(1701),y(1701)/192.00d0,311.28d0/
            data x(1702),y(1702)/187.20d0,316.32d0/
            data x(1703),y(1703)/249.84d0,252.96d0/
            data x(1704),y(1704)/249.60d0,254.64d0/
            data x(1705),y(1705)/248.40d0,254.88d0/
            data x(1706),y(1706)/247.92d0,256.32d0/
            data x(1707),y(1707)/248.64d0,257.52d0/
            data x(1708),y(1708)/248.64d0,258.00d0/
            data x(1709),y(1709)/247.20d0,259.68d0/
            data x(1710),y(1710)/243.36d0,263.28d0/
            data x(1711),y(1711)/240.96d0,264.00d0/
            data x(1712),y(1712)/240.00d0,263.52d0/
            data x(1713),y(1713)/237.84d0,265.92d0/
            data x(1714),y(1714)/236.40d0,267.12d0/
            data x(1715),y(1715)/235.68d0,266.88d0/
            data x(1716),y(1716)/235.68d0,267.36d0/
            data x(1717),y(1717)/233.04d0,269.28d0/
            data x(1718),y(1718)/232.56d0,271.44d0/
            data x(1719),y(1719)/230.64d0,271.68d0/
            data x(1720),y(1720)/229.44d0,273.60d0/
            data x(1721),y(1721)/229.44d0,275.28d0/
            data x(1722),y(1722)/230.16d0,275.52d0/
            data x(1723),y(1723)/231.36d0,277.44d0/
            data x(1724),y(1724)/229.20d0,279.84d0/
            data x(1725),y(1725)/281.52d0,252.00d0/
            data x(1726),y(1726)/250.80d0,252.96d0/
            data x(1727),y(1727)/282.96d0,291.84d0/
            data x(1728),y(1728)/282.96d0,287.04d0/
            data x(1729),y(1729)/285.36d0,286.80d0/
            data x(1730),y(1730)/285.12d0,279.36d0/
            data x(1731),y(1731)/282.72d0,279.36d0/
            data x(1732),y(1732)/357.12d0,250.56d0/
            data x(1733),y(1733)/356.88d0,251.28d0/
            data x(1734),y(1734)/370.32d0,251.04d0/
            data x(1735),y(1735)/433.92d0,250.80d0/
            data x(1736),y(1736)/459.12d0,251.28d0/
            data x(1737),y(1737)/459.36d0,252.00d0/
            data x(1738),y(1738)/368.40d0,195.12d0/
            data x(1739),y(1739)/370.08d0,195.36d0/
            data x(1740),y(1740)/370.32d0,242.64d0/
            data x(1741),y(1741)/371.28d0,242.64d0/
            data x(1742),y(1742)/371.28d0,244.32d0/
            data x(1743),y(1743)/369.60d0,244.32d0/
            data x(1744),y(1744)/369.60d0,246.00d0/
            data x(1745),y(1745)/370.32d0,246.00d0/
            data x(1746),y(1746)/228.24d0,206.40d0/
            data x(1747),y(1747)/229.68d0,206.88d0/
            data x(1748),y(1748)/230.88d0,206.40d0/
            data x(1749),y(1749)/234.96d0,207.84d0/
            data x(1750),y(1750)/237.60d0,207.60d0/
            data x(1751),y(1751)/240.72d0,205.44d0/
            data x(1752),y(1752)/243.84d0,202.56d0/
            data x(1753),y(1753)/244.56d0,203.52d0/
            data x(1754),y(1754)/244.32d0,204.96d0/
            data x(1755),y(1755)/242.64d0,208.56d0/
            data x(1756),y(1756)/246.48d0,209.28d0/
            data x(1757),y(1757)/248.40d0,209.04d0/
            data x(1758),y(1758)/249.60d0,209.76d0/
            data x(1759),y(1759)/250.08d0,211.20d0/
            data x(1760),y(1760)/252.00d0,213.12d0/
            data x(1761),y(1761)/254.88d0,214.08d0/
            data x(1762),y(1762)/255.60d0,212.88d0/
            data x(1763),y(1763)/258.96d0,211.20d0/
            data x(1764),y(1764)/262.08d0,210.48d0/
            data x(1765),y(1765)/263.52d0,209.28d0/
            data x(1766),y(1766)/264.48d0,209.52d0/
            data x(1767),y(1767)/266.40d0,207.60d0/
            data x(1768),y(1768)/270.48d0,205.68d0/
            data x(1769),y(1769)/274.08d0,205.44d0/
            data x(1770),y(1770)/275.28d0,204.24d0/
            data x(1771),y(1771)/277.44d0,203.52d0/
            data x(1772),y(1772)/280.32d0,200.88d0/
            data x(1773),y(1773)/283.20d0,201.12d0/
            data x(1774),y(1774)/283.44d0,211.20d0/
            data x(1775),y(1775)/282.72d0,211.20d0/
            data x(1776),y(1776)/282.72d0,211.92d0/
            data x(1777),y(1777)/279.36d0,211.92d0/
            data x(1778),y(1778)/279.84d0,217.20d0/
            data x(1779),y(1779)/274.56d0,216.96d0/
            data x(1780),y(1780)/274.56d0,222.00d0/
            data x(1781),y(1781)/267.84d0,222.24d0/
            data x(1782),y(1782)/268.08d0,227.28d0/
            data x(1783),y(1783)/264.72d0,227.28d0/
            data x(1784),y(1784)/264.72d0,232.32d0/
            data x(1785),y(1785)/259.20d0,232.56d0/
            data x(1786),y(1786)/259.44d0,234.48d0/
            data x(1787),y(1787)/258.48d0,234.24d0/
            data x(1788),y(1788)/258.48d0,235.20d0/
            data x(1789),y(1789)/257.76d0,235.20d0/
            data x(1790),y(1790)/257.76d0,235.92d0/
            data x(1791),y(1791)/257.04d0,235.92d0/
            data x(1792),y(1792)/257.04d0,236.88d0/
            data x(1793),y(1793)/255.36d0,237.84d0/
            data x(1794),y(1794)/255.60d0,242.88d0/
            data x(1795),y(1795)/250.56d0,243.12d0/
            data x(1796),y(1796)/284.64d0,200.88d0/
            data x(1797),y(1797)/287.52d0,200.88d0/
            data x(1798),y(1798)/287.52d0,199.68d0/
            data x(1799),y(1799)/292.56d0,199.68d0/
            data x(1800),y(1800)/292.32d0,198.48d0/
            data x(1801),y(1801)/294.00d0,198.24d0/
            data x(1802),y(1802)/294.00d0,195.84d0/
            data x(1803),y(1803)/306.72d0,195.36d0/
            data x(1804),y(1804)/306.72d0,194.16d0/
            data x(1805),y(1805)/311.04d0,193.92d0/
            data x(1806),y(1806)/310.56d0,195.60d0/
            data x(1807),y(1807)/283.92d0,172.56d0/
            data x(1808),y(1808)/283.44d0,174.00d0/
            data x(1809),y(1809)/283.92d0,174.24d0/
            data x(1810),y(1810)/322.32d0,164.64d0/
            data x(1811),y(1811)/322.32d0,162.48d0/
            data x(1812),y(1812)/320.64d0,162.24d0/
            data x(1813),y(1813)/320.64d0,158.16d0/
            data x(1814),y(1814)/314.88d0,158.40d0/
            data x(1815),y(1815)/307.44d0,153.12d0/
            data x(1816),y(1816)/368.40d0,180.00d0/
            data x(1817),y(1817)/369.36d0,164.64d0/
            data x(1818),y(1818)/365.04d0,149.28d0/
            data x(1819),y(1819)/363.36d0,149.28d0/
            data x(1820),y(1820)/363.36d0,148.32d0/
            data x(1821),y(1821)/362.64d0,148.32d0/
            data x(1822),y(1822)/361.68d0,146.64d0/
            data x(1823),y(1823)/362.64d0,144.96d0/
            data x(1824),y(1824)/367.92d0,140.64d0/
            data x(1825),y(1825)/367.92d0,141.60d0/
            data x(1826),y(1826)/368.88d0,141.60d0/
            data x(1827),y(1827)/368.88d0,143.76d0/
            data x(1828),y(1828)/371.04d0,143.76d0/
            data x(1829),y(1829)/371.04d0,146.40d0/
            data x(1830),y(1830)/373.44d0,147.36d0/
            data x(1831),y(1831)/373.44d0,150.00d0/
            data x(1832),y(1832)/382.08d0,149.76d0/
            data x(1833),y(1833)/382.08d0,149.04d0/
            data x(1834),y(1834)/389.28d0,149.04d0/
            data x(1835),y(1835)/389.28d0,148.08d0/
            data x(1836),y(1836)/403.44d0,148.32d0/
            data x(1837),y(1837)/403.44d0,150.00d0/
            data x(1838),y(1838)/480.72d0,150.72d0/
            data x(1839),y(1839)/480.48d0,153.36d0/
            data x(1840),y(1840)/367.68d0,139.92d0/
            data x(1841),y(1841)/368.40d0,139.92d0/
            data x(1842),y(1842)/372.24d0,134.64d0/
            data x(1843),y(1843)/374.40d0,134.16d0/
            data x(1844),y(1844)/374.40d0,131.52d0/
            data x(1845),y(1845)/377.52d0,131.04d0/
            data x(1846),y(1846)/380.40d0,128.40d0/
            data x(1847),y(1847)/376.08d0,122.16d0/
            data x(1848),y(1848)/375.60d0,122.16d0/
            data x(1849),y(1849)/375.60d0,119.52d0/
            data x(1850),y(1850)/382.56d0,119.52d0/
            data x(1851),y(1851)/382.32d0,118.80d0/
            data x(1852),y(1852)/388.56d0,116.16d0/
            data x(1853),y(1853)/388.56d0,115.44d0/
            data x(1854),y(1854)/444.24d0,115.20d0/
            data x(1855),y(1855)/346.80d0,133.44d0/
            data x(1856),y(1856)/348.00d0,134.40d0/
            data x(1857),y(1857)/347.76d0,135.84d0/
            data x(1858),y(1858)/349.20d0,137.76d0/
            data x(1859),y(1859)/349.44d0,139.44d0/
            data x(1860),y(1860)/352.32d0,141.36d0/
            data x(1861),y(1861)/353.28d0,142.56d0/
            data x(1862),y(1862)/353.52d0,144.96d0/
            data x(1863),y(1863)/372.24d0,116.64d0/
            data x(1864),y(1864)/374.16d0,116.64d0/
            data x(1865),y(1865)/375.60d0,117.60d0/
            data x(1866),y(1866)/443.52d0, 95.04d0/
            data x(1867),y(1867)/444.48d0, 95.04d0/
            data b1(   1),b2(   1)/    1,    2/
            data b1(   2),b2(   2)/    2,    3/
            data b1(   3),b2(   3)/    3,    4/
            data b1(   4),b2(   4)/    4,    5/
            data b1(   5),b2(   5)/    5,    6/
            data b1(   6),b2(   6)/    6,    7/
            data b1(   7),b2(   7)/    7,    8/
            data b1(   8),b2(   8)/    8,    9/
            data b1(   9),b2(   9)/    9,   10/
            data b1(  10),b2(  10)/   10,   11/
            data b1(  11),b2(  11)/   11,   12/
            data b1(  12),b2(  12)/   12,   13/
            data b1(  13),b2(  13)/   13,   14/
            data b1(  14),b2(  14)/   14,   15/
            data b1(  15),b2(  15)/   15,   16/
            data b1(  16),b2(  16)/   16,   17/
            data b1(  17),b2(  17)/   17,   18/
            data b1(  18),b2(  18)/   18,   19/
            data b1(  19),b2(  19)/   19,   20/
            data b1(  20),b2(  20)/   20,   21/
            data b1(  21),b2(  21)/   21,   22/
            data b1(  22),b2(  22)/   22,   23/
            data b1(  23),b2(  23)/   23,   24/
            data b1(  24),b2(  24)/   24,   25/
            data b1(  25),b2(  25)/   25,   26/
            data b1(  26),b2(  26)/   26,   27/
            data b1(  27),b2(  27)/   27,   28/
            data b1(  28),b2(  28)/   28,   29/
            data b1(  29),b2(  29)/   29,   30/
            data b1(  30),b2(  30)/   30,   31/
            data b1(  31),b2(  31)/   31,   32/
            data b1(  32),b2(  32)/   32,   33/
            data b1(  33),b2(  33)/   33,   34/
            data b1(  34),b2(  34)/   34,   35/
            data b1(  35),b2(  35)/   35,   36/
            data b1(  36),b2(  36)/   36,   37/
            data b1(  37),b2(  37)/   37,   38/
            data b1(  38),b2(  38)/   38,   39/
            data b1(  39),b2(  39)/   39,   40/
            data b1(  40),b2(  40)/   40,   41/
            data b1(  41),b2(  41)/   41,   42/
            data b1(  42),b2(  42)/   42,   43/
            data b1(  43),b2(  43)/   43,   44/
            data b1(  44),b2(  44)/   44,   45/
            data b1(  45),b2(  45)/   45,   46/
            data b1(  46),b2(  46)/   46,   47/
            data b1(  47),b2(  47)/   47,   48/
            data b1(  48),b2(  48)/   48,   49/
            data b1(  49),b2(  49)/   49,   50/
            data b1(  50),b2(  50)/   50,   51/
            data b1(  51),b2(  51)/   51,   52/
            data b1(  52),b2(  52)/   52,   53/
            data b1(  53),b2(  53)/   53,   54/
            data b1(  54),b2(  54)/   54,   55/
            data b1(  55),b2(  55)/   55,   56/
            data b1(  56),b2(  56)/   56,   57/
            data b1(  57),b2(  57)/   57,   58/
            data b1(  58),b2(  58)/   58,   59/
            data b1(  59),b2(  59)/   59,   60/
            data b1(  60),b2(  60)/   60,   61/
            data b1(  61),b2(  61)/   61,   62/
            data b1(  62),b2(  62)/   62,   63/
            data b1(  63),b2(  63)/   63,   64/
            data b1(  64),b2(  64)/   64,   65/
            data b1(  65),b2(  65)/   65,   66/
            data b1(  66),b2(  66)/   66,   67/
            data b1(  67),b2(  67)/   67,   68/
            data b1(  68),b2(  68)/   68,   69/
            data b1(  69),b2(  69)/   69,   70/
            data b1(  70),b2(  70)/   70,   71/
            data b1(  71),b2(  71)/   71,   72/
            data b1(  72),b2(  72)/   72,   73/
            data b1(  73),b2(  73)/   73,   74/
            data b1(  74),b2(  74)/   74,   75/
            data b1(  75),b2(  75)/   75,   76/
            data b1(  76),b2(  76)/   76,   77/
            data b1(  77),b2(  77)/   77,   78/
            data b1(  78),b2(  78)/   78,   79/
            data b1(  79),b2(  79)/   79,   80/
            data b1(  80),b2(  80)/   80,   81/
            data b1(  81),b2(  81)/   81,   82/
            data b1(  82),b2(  82)/   82,   83/
            data b1(  83),b2(  83)/   83,   84/
            data b1(  84),b2(  84)/   84,   85/
            data b1(  85),b2(  85)/   85,   86/
            data b1(  86),b2(  86)/   86,   87/
            data b1(  87),b2(  87)/   87,   88/
            data b1(  88),b2(  88)/   88,   89/
            data b1(  89),b2(  89)/   89,   90/
            data b1(  90),b2(  90)/   90,   91/
            data b1(  91),b2(  91)/   91,   92/
            data b1(  92),b2(  92)/   92,   93/
            data b1(  93),b2(  93)/   93,   94/
            data b1(  94),b2(  94)/   94,   95/
            data b1(  95),b2(  95)/   95,   96/
            data b1(  96),b2(  96)/   96,   97/
            data b1(  97),b2(  97)/   97,   98/
            data b1(  98),b2(  98)/   98,   99/
            data b1(  99),b2(  99)/   99,  100/
            data b1( 100),b2( 100)/  100,  101/
            data b1( 101),b2( 101)/  101,  102/
            data b1( 102),b2( 102)/  102,  103/
            data b1( 103),b2( 103)/  103,  104/
            data b1( 104),b2( 104)/  104,  105/
            data b1( 105),b2( 105)/  105,  106/
            data b1( 106),b2( 106)/  106,  107/
            data b1( 107),b2( 107)/  107,  108/
            data b1( 108),b2( 108)/  108,  109/
            data b1( 109),b2( 109)/  109,  110/
            data b1( 110),b2( 110)/  110,  111/
            data b1( 111),b2( 111)/  111,  112/
            data b1( 112),b2( 112)/  112,  113/
            data b1( 113),b2( 113)/  113,  114/
            data b1( 114),b2( 114)/  114,  115/
            data b1( 115),b2( 115)/  115,  116/
            data b1( 116),b2( 116)/  116,  117/
            data b1( 117),b2( 117)/  117,  118/
            data b1( 118),b2( 118)/  118,  119/
            data b1( 119),b2( 119)/  119,  120/
            data b1( 120),b2( 120)/  120,  121/
            data b1( 121),b2( 121)/  121,  122/
            data b1( 122),b2( 122)/  122,  123/
            data b1( 123),b2( 123)/  123,  124/
            data b1( 124),b2( 124)/  124,  125/
            data b1( 125),b2( 125)/  125,  126/
            data b1( 126),b2( 126)/  126,  127/
            data b1( 127),b2( 127)/  127,  128/
            data b1( 128),b2( 128)/  128,  129/
            data b1( 129),b2( 129)/  129,  130/
            data b1( 130),b2( 130)/  130,  131/
            data b1( 131),b2( 131)/  131,  132/
            data b1( 132),b2( 132)/  132,  133/
            data b1( 133),b2( 133)/  133,  134/
            data b1( 134),b2( 134)/  134,  135/
            data b1( 135),b2( 135)/  135,  136/
            data b1( 136),b2( 136)/  136,  137/
            data b1( 137),b2( 137)/  137,  138/
            data b1( 138),b2( 138)/  138,  139/
            data b1( 139),b2( 139)/  139,  140/
            data b1( 140),b2( 140)/  140,  141/
            data b1( 141),b2( 141)/  141,  142/
            data b1( 142),b2( 142)/  142,  143/
            data b1( 143),b2( 143)/  143,  144/
            data b1( 144),b2( 144)/  144,  145/
            data b1( 145),b2( 145)/  145,  146/
            data b1( 146),b2( 146)/  146,  147/
            data b1( 147),b2( 147)/  147,  148/
            data b1( 148),b2( 148)/  148,  149/
            data b1( 149),b2( 149)/  149,  150/
            data b1( 150),b2( 150)/  150,  151/
            data b1( 151),b2( 151)/  151,  152/
            data b1( 152),b2( 152)/  152,  153/
            data b1( 153),b2( 153)/  153,  154/
            data b1( 154),b2( 154)/  154,  155/
            data b1( 155),b2( 155)/  155,  156/
            data b1( 156),b2( 156)/  156,  157/
            data b1( 157),b2( 157)/  157,  158/
            data b1( 158),b2( 158)/  158,  159/
            data b1( 159),b2( 159)/  159,  160/
            data b1( 160),b2( 160)/  160,  161/
            data b1( 161),b2( 161)/  161,  162/
            data b1( 162),b2( 162)/  162,  163/
            data b1( 163),b2( 163)/  163,  164/
            data b1( 164),b2( 164)/  164,  165/
            data b1( 165),b2( 165)/  165,  166/
            data b1( 166),b2( 166)/  166,  167/
            data b1( 167),b2( 167)/  167,  168/
            data b1( 168),b2( 168)/  168,  169/
            data b1( 169),b2( 169)/  169,  170/
            data b1( 170),b2( 170)/  170,  171/
            data b1( 171),b2( 171)/  171,  172/
            data b1( 172),b2( 172)/  172,  173/
            data b1( 173),b2( 173)/  173,  174/
            data b1( 174),b2( 174)/  174,  175/
            data b1( 175),b2( 175)/  175,  176/
            data b1( 176),b2( 176)/  176,  177/
            data b1( 177),b2( 177)/  177,  178/
            data b1( 178),b2( 178)/  178,  179/
            data b1( 179),b2( 179)/  179,  180/
            data b1( 180),b2( 180)/  180,  181/
            data b1( 181),b2( 181)/  181,  182/
            data b1( 182),b2( 182)/  182,  183/
            data b1( 183),b2( 183)/  183,  184/
            data b1( 184),b2( 184)/  184,  185/
            data b1( 185),b2( 185)/  185,  186/
            data b1( 186),b2( 186)/  186,  187/
            data b1( 187),b2( 187)/  187,  188/
            data b1( 188),b2( 188)/  188,  189/
            data b1( 189),b2( 189)/  189,  190/
            data b1( 190),b2( 190)/  190,  191/
            data b1( 191),b2( 191)/  191,  192/
            data b1( 192),b2( 192)/  192,  193/
            data b1( 193),b2( 193)/  193,  194/
            data b1( 194),b2( 194)/  194,  195/
            data b1( 195),b2( 195)/  195,  196/
            data b1( 196),b2( 196)/  196,  197/
            data b1( 197),b2( 197)/  197,  198/
            data b1( 198),b2( 198)/  198,  199/
            data b1( 199),b2( 199)/  199,  200/
            data b1( 200),b2( 200)/  200,  201/
            data b1( 201),b2( 201)/  201,  202/
            data b1( 202),b2( 202)/  202,  203/
            data b1( 203),b2( 203)/  203,  204/
            data b1( 204),b2( 204)/  204,  205/
            data b1( 205),b2( 205)/  205,  206/
            data b1( 206),b2( 206)/  206,  207/
            data b1( 207),b2( 207)/  207,  208/
            data b1( 208),b2( 208)/  208,  209/
            data b1( 209),b2( 209)/  209,  210/
            data b1( 210),b2( 210)/  210,  211/
            data b1( 211),b2( 211)/  211,  212/
            data b1( 212),b2( 212)/  212,  213/
            data b1( 213),b2( 213)/  213,  214/
            data b1( 214),b2( 214)/  214,  215/
            data b1( 215),b2( 215)/  215,  216/
            data b1( 216),b2( 216)/  216,  217/
            data b1( 217),b2( 217)/  217,  218/
            data b1( 218),b2( 218)/  218,  219/
            data b1( 219),b2( 219)/  219,  220/
            data b1( 220),b2( 220)/  220,  221/
            data b1( 221),b2( 221)/  221,  222/
            data b1( 222),b2( 222)/  222,  223/
            data b1( 223),b2( 223)/  223,  224/
            data b1( 224),b2( 224)/  224,  225/
            data b1( 225),b2( 225)/  225,  226/
            data b1( 226),b2( 226)/  226,  227/
            data b1( 227),b2( 227)/  227,  228/
            data b1( 228),b2( 228)/  228,  229/
            data b1( 229),b2( 229)/  229,  230/
            data b1( 230),b2( 230)/  230,  231/
            data b1( 231),b2( 231)/  231,  232/
            data b1( 232),b2( 232)/  232,  233/
            data b1( 233),b2( 233)/  233,  234/
            data b1( 234),b2( 234)/  234,  235/
            data b1( 235),b2( 235)/  235,  236/
            data b1( 236),b2( 236)/  236,  237/
            data b1( 237),b2( 237)/  237,  238/
            data b1( 238),b2( 238)/  238,  239/
            data b1( 239),b2( 239)/  239,  240/
            data b1( 240),b2( 240)/  240,  241/
            data b1( 241),b2( 241)/  241,  242/
            data b1( 242),b2( 242)/  242,  243/
            data b1( 243),b2( 243)/  243,  244/
            data b1( 244),b2( 244)/  244,  245/
            data b1( 245),b2( 245)/  245,  246/
            data b1( 246),b2( 246)/  246,  247/
            data b1( 247),b2( 247)/  247,  248/
            data b1( 248),b2( 248)/  248,  249/
            data b1( 249),b2( 249)/  249,  250/
            data b1( 250),b2( 250)/  250,  251/
            data b1( 251),b2( 251)/  251,  252/
            data b1( 252),b2( 252)/  252,  253/
            data b1( 253),b2( 253)/  253,  254/
            data b1( 254),b2( 254)/  254,  255/
            data b1( 255),b2( 255)/  255,  256/
            data b1( 256),b2( 256)/  256,  257/
            data b1( 257),b2( 257)/  257,  258/
            data b1( 258),b2( 258)/  258,  259/
            data b1( 259),b2( 259)/  259,  260/
            data b1( 260),b2( 260)/  260,  261/
            data b1( 261),b2( 261)/  261,  262/
            data b1( 262),b2( 262)/  262,  263/
            data b1( 263),b2( 263)/  263,  264/
            data b1( 264),b2( 264)/  264,  265/
            data b1( 265),b2( 265)/  265,  266/
            data b1( 266),b2( 266)/  266,  267/
            data b1( 267),b2( 267)/  267,  268/
            data b1( 268),b2( 268)/  268,  269/
            data b1( 269),b2( 269)/  269,  270/
            data b1( 270),b2( 270)/  270,  271/
            data b1( 271),b2( 271)/  271,  272/
            data b1( 272),b2( 272)/  272,  273/
            data b1( 273),b2( 273)/  273,  274/
            data b1( 274),b2( 274)/  274,  275/
            data b1( 275),b2( 275)/  275,  276/
            data b1( 276),b2( 276)/  276,  277/
            data b1( 277),b2( 277)/  277,  278/
            data b1( 278),b2( 278)/  278,  279/
            data b1( 279),b2( 279)/  279,  280/
            data b1( 280),b2( 280)/  280,  281/
            data b1( 281),b2( 281)/  281,  282/
            data b1( 282),b2( 282)/  282,  283/
            data b1( 283),b2( 283)/  283,  284/
            data b1( 284),b2( 284)/  284,  285/
            data b1( 285),b2( 285)/  285,  286/
            data b1( 286),b2( 286)/  286,  287/
            data b1( 287),b2( 287)/  287,  288/
            data b1( 288),b2( 288)/  288,  289/
            data b1( 289),b2( 289)/  289,  290/
            data b1( 290),b2( 290)/  290,  291/
            data b1( 291),b2( 291)/  291,  292/
            data b1( 292),b2( 292)/  292,  293/
            data b1( 293),b2( 293)/  293,  294/
            data b1( 294),b2( 294)/  294,  295/
            data b1( 295),b2( 295)/  295,  296/
            data b1( 296),b2( 296)/  296,  297/
            data b1( 297),b2( 297)/  297,  298/
            data b1( 298),b2( 298)/  298,  299/
            data b1( 299),b2( 299)/  299,  300/
            data b1( 300),b2( 300)/  300,  301/
            data b1( 301),b2( 301)/  301,  302/
            data b1( 302),b2( 302)/  302,  303/
            data b1( 303),b2( 303)/  303,  304/
            data b1( 304),b2( 304)/  304,  305/
            data b1( 305),b2( 305)/  305,  306/
            data b1( 306),b2( 306)/  306,  307/
            data b1( 307),b2( 307)/  307,  308/
            data b1( 308),b2( 308)/  308,  309/
            data b1( 309),b2( 309)/  309,  310/
            data b1( 310),b2( 310)/  310,  311/
            data b1( 311),b2( 311)/  311,  312/
            data b1( 312),b2( 312)/  312,  313/
            data b1( 313),b2( 313)/  313,  314/
            data b1( 314),b2( 314)/  314,  315/
            data b1( 315),b2( 315)/  315,  316/
            data b1( 316),b2( 316)/  316,  317/
            data b1( 317),b2( 317)/  317,  318/
            data b1( 318),b2( 318)/  318,  319/
            data b1( 319),b2( 319)/  319,  320/
            data b1( 320),b2( 320)/  320,  321/
            data b1( 321),b2( 321)/  321,  322/
            data b1( 322),b2( 322)/  322,  323/
            data b1( 323),b2( 323)/  323,  324/
            data b1( 324),b2( 324)/  324,  325/
            data b1( 325),b2( 325)/  325,  326/
            data b1( 326),b2( 326)/  326,  327/
            data b1( 327),b2( 327)/  327,  328/
            data b1( 328),b2( 328)/  328,  329/
            data b1( 329),b2( 329)/  329,  330/
            data b1( 330),b2( 330)/  330,  331/
            data b1( 331),b2( 331)/  331,  332/
            data b1( 332),b2( 332)/  332,  333/
            data b1( 333),b2( 333)/  333,  334/
            data b1( 334),b2( 334)/  334,  335/
            data b1( 335),b2( 335)/  335,  336/
            data b1( 336),b2( 336)/  336,  337/
            data b1( 337),b2( 337)/  337,  338/
            data b1( 338),b2( 338)/  338,  339/
            data b1( 339),b2( 339)/  339,  340/
            data b1( 340),b2( 340)/  340,  341/
            data b1( 341),b2( 341)/  341,  342/
            data b1( 342),b2( 342)/  342,  343/
            data b1( 343),b2( 343)/  343,  344/
            data b1( 344),b2( 344)/  344,  345/
            data b1( 345),b2( 345)/  345,  346/
            data b1( 346),b2( 346)/  346,  347/
            data b1( 347),b2( 347)/  347,  348/
            data b1( 348),b2( 348)/  348,  349/
            data b1( 349),b2( 349)/  349,  350/
            data b1( 350),b2( 350)/  350,  351/
            data b1( 351),b2( 351)/  351,  352/
            data b1( 352),b2( 352)/  352,  353/
            data b1( 353),b2( 353)/  353,  354/
            data b1( 354),b2( 354)/  354,  355/
            data b1( 355),b2( 355)/  355,  356/
            data b1( 356),b2( 356)/  356,  357/
            data b1( 357),b2( 357)/  357,  358/
            data b1( 358),b2( 358)/  358,  359/
            data b1( 359),b2( 359)/  359,  360/
            data b1( 360),b2( 360)/  360,  361/
            data b1( 361),b2( 361)/  361,  362/
            data b1( 362),b2( 362)/  362,  363/
            data b1( 363),b2( 363)/  363,  364/
            data b1( 364),b2( 364)/  364,  365/
            data b1( 365),b2( 365)/  365,  366/
            data b1( 366),b2( 366)/  366,  367/
            data b1( 367),b2( 367)/  367,  368/
            data b1( 368),b2( 368)/  368,  369/
            data b1( 369),b2( 369)/  369,  370/
            data b1( 370),b2( 370)/  370,  371/
            data b1( 371),b2( 371)/  371,  372/
            data b1( 372),b2( 372)/  372,  373/
            data b1( 373),b2( 373)/  373,  374/
            data b1( 374),b2( 374)/  374,  375/
            data b1( 375),b2( 375)/  375,  376/
            data b1( 376),b2( 376)/  376,  377/
            data b1( 377),b2( 377)/  377,  378/
            data b1( 378),b2( 378)/  378,  379/
            data b1( 379),b2( 379)/  379,  380/
            data b1( 380),b2( 380)/  380,  381/
            data b1( 381),b2( 381)/  381,  382/
            data b1( 382),b2( 382)/  382,  383/
            data b1( 383),b2( 383)/  383,  384/
            data b1( 384),b2( 384)/  384,  385/
            data b1( 385),b2( 385)/  385,  386/
            data b1( 386),b2( 386)/  386,  387/
            data b1( 387),b2( 387)/  387,  388/
            data b1( 388),b2( 388)/  388,  389/
            data b1( 389),b2( 389)/  389,  390/
            data b1( 390),b2( 390)/  390,  391/
            data b1( 391),b2( 391)/  391,  392/
            data b1( 392),b2( 392)/  392,  393/
            data b1( 393),b2( 393)/  393,  394/
            data b1( 394),b2( 394)/  394,  395/
            data b1( 395),b2( 395)/  395,  396/
            data b1( 396),b2( 396)/  396,  397/
            data b1( 397),b2( 397)/  397,  398/
            data b1( 398),b2( 398)/  398,  399/
            data b1( 399),b2( 399)/  399,  400/
            data b1( 400),b2( 400)/  400,  401/
            data b1( 401),b2( 401)/  401,  402/
            data b1( 402),b2( 402)/  402,  403/
            data b1( 403),b2( 403)/  403,  404/
            data b1( 404),b2( 404)/  404,  405/
            data b1( 405),b2( 405)/  405,  406/
            data b1( 406),b2( 406)/  406,  407/
            data b1( 407),b2( 407)/  407,  408/
            data b1( 408),b2( 408)/  408,  409/
            data b1( 409),b2( 409)/  409,  410/
            data b1( 410),b2( 410)/  410,  411/
            data b1( 411),b2( 411)/  411,  412/
            data b1( 412),b2( 412)/  412,  413/
            data b1( 413),b2( 413)/  413,  414/
            data b1( 414),b2( 414)/  414,  415/
            data b1( 415),b2( 415)/  415,  416/
            data b1( 416),b2( 416)/  416,  417/
            data b1( 417),b2( 417)/  417,  418/
            data b1( 418),b2( 418)/  418,  419/
            data b1( 419),b2( 419)/  419,  420/
            data b1( 420),b2( 420)/  420,  421/
            data b1( 421),b2( 421)/  421,  422/
            data b1( 422),b2( 422)/  422,  423/
            data b1( 423),b2( 423)/  423,  424/
            data b1( 424),b2( 424)/  424,  425/
            data b1( 425),b2( 425)/  425,  426/
            data b1( 426),b2( 426)/  426,  427/
            data b1( 427),b2( 427)/  427,  428/
            data b1( 428),b2( 428)/  428,  429/
            data b1( 429),b2( 429)/  429,  430/
            data b1( 430),b2( 430)/  430,  431/
            data b1( 431),b2( 431)/  431,  432/
            data b1( 432),b2( 432)/  432,  433/
            data b1( 433),b2( 433)/  433,  434/
            data b1( 434),b2( 434)/  434,  435/
            data b1( 435),b2( 435)/  435,  436/
            data b1( 436),b2( 436)/  436,  437/
            data b1( 437),b2( 437)/  437,  438/
            data b1( 438),b2( 438)/  438,  439/
            data b1( 439),b2( 439)/  439,  440/
            data b1( 440),b2( 440)/  440,  441/
            data b1( 441),b2( 441)/  441,  442/
            data b1( 442),b2( 442)/  442,  443/
            data b1( 443),b2( 443)/  443,  444/
            data b1( 444),b2( 444)/  444,  445/
            data b1( 445),b2( 445)/  445,  446/
            data b1( 446),b2( 446)/  446,  447/
            data b1( 447),b2( 447)/  447,  448/
            data b1( 448),b2( 448)/  448,  449/
            data b1( 449),b2( 449)/  449,  450/
            data b1( 450),b2( 450)/  450,  451/
            data b1( 451),b2( 451)/  451,  452/
            data b1( 452),b2( 452)/  452,  453/
            data b1( 453),b2( 453)/  453,  454/
            data b1( 454),b2( 454)/  454,  455/
            data b1( 455),b2( 455)/  455,  456/
            data b1( 456),b2( 456)/  456,  457/
            data b1( 457),b2( 457)/  457,  458/
            data b1( 458),b2( 458)/  458,  459/
            data b1( 459),b2( 459)/  459,  460/
            data b1( 460),b2( 460)/  460,  461/
            data b1( 461),b2( 461)/  461,  462/
            data b1( 462),b2( 462)/  462,  463/
            data b1( 463),b2( 463)/  463,  464/
            data b1( 464),b2( 464)/  464,  465/
            data b1( 465),b2( 465)/  465,  466/
            data b1( 466),b2( 466)/  466,  467/
            data b1( 467),b2( 467)/  467,  468/
            data b1( 468),b2( 468)/  468,  469/
            data b1( 469),b2( 469)/  469,  470/
            data b1( 470),b2( 470)/  470,  471/
            data b1( 471),b2( 471)/  471,  472/
            data b1( 472),b2( 472)/  472,  473/
            data b1( 473),b2( 473)/  473,  474/
            data b1( 474),b2( 474)/  474,  475/
            data b1( 475),b2( 475)/  475,  476/
            data b1( 476),b2( 476)/  476,  477/
            data b1( 477),b2( 477)/  477,  478/
            data b1( 478),b2( 478)/  478,  479/
            data b1( 479),b2( 479)/  479,  480/
            data b1( 480),b2( 480)/  480,  481/
            data b1( 481),b2( 481)/  481,  482/
            data b1( 482),b2( 482)/  482,  483/
            data b1( 483),b2( 483)/  483,  484/
            data b1( 484),b2( 484)/  484,  485/
            data b1( 485),b2( 485)/  485,  486/
            data b1( 486),b2( 486)/  486,  487/
            data b1( 487),b2( 487)/  487,  488/
            data b1( 488),b2( 488)/  488,  489/
            data b1( 489),b2( 489)/  489,  490/
            data b1( 490),b2( 490)/  490,  491/
            data b1( 491),b2( 491)/  491,  492/
            data b1( 492),b2( 492)/  492,  493/
            data b1( 493),b2( 493)/  493,  494/
            data b1( 494),b2( 494)/  494,  495/
            data b1( 495),b2( 495)/  495,  496/
            data b1( 496),b2( 496)/  496,  497/
            data b1( 497),b2( 497)/  497,  498/
            data b1( 498),b2( 498)/  498,  499/
            data b1( 499),b2( 499)/  499,  500/
            data b1( 500),b2( 500)/  500,  501/
            data b1( 501),b2( 501)/  501,  502/
            data b1( 502),b2( 502)/  502,  503/
            data b1( 503),b2( 503)/  503,  504/
            data b1( 504),b2( 504)/  504,  505/
            data b1( 505),b2( 505)/  505,  506/
            data b1( 506),b2( 506)/    1,  506/
            data b1( 507),b2( 507)/  180,  507/
            data b1( 508),b2( 508)/  507,  508/
            data b1( 509),b2( 509)/  508,  509/
            data b1( 510),b2( 510)/  509,  510/
            data b1( 511),b2( 511)/  510,  511/
            data b1( 512),b2( 512)/  511,  512/
            data b1( 513),b2( 513)/  512,  513/
            data b1( 514),b2( 514)/  513,  514/
            data b1( 515),b2( 515)/  514,  515/
            data b1( 516),b2( 516)/  515,  516/
            data b1( 517),b2( 517)/  516,  517/
            data b1( 518),b2( 518)/  517,  518/
            data b1( 519),b2( 519)/  518,  519/
            data b1( 520),b2( 520)/  519,  520/
            data b1( 521),b2( 521)/  520,  521/
            data b1( 522),b2( 522)/  521,  522/
            data b1( 523),b2( 523)/  522,  523/
            data b1( 524),b2( 524)/  523,  524/
            data b1( 525),b2( 525)/  524,  525/
            data b1( 526),b2( 526)/  525,  526/
            data b1( 527),b2( 527)/  526,  527/
            data b1( 528),b2( 528)/  527,  528/
            data b1( 529),b2( 529)/  528,  529/
            data b1( 530),b2( 530)/  529,  530/
            data b1( 531),b2( 531)/  530,  531/
            data b1( 532),b2( 532)/  531,  532/
            data b1( 533),b2( 533)/  532,  533/
            data b1( 534),b2( 534)/  533,  534/
            data b1( 535),b2( 535)/  194,  534/
            data b1( 536),b2( 536)/  535,  536/
            data b1( 537),b2( 537)/  208,  536/
            data b1( 538),b2( 538)/  221,  537/
            data b1( 539),b2( 539)/  535,  537/
            data b1( 540),b2( 540)/  535,  538/
            data b1( 541),b2( 541)/  538,  539/
            data b1( 542),b2( 542)/  539,  540/
            data b1( 543),b2( 543)/  540,  541/
            data b1( 544),b2( 544)/  541,  542/
            data b1( 545),b2( 545)/  542,  543/
            data b1( 546),b2( 546)/  543,  544/
            data b1( 547),b2( 547)/  544,  545/
            data b1( 548),b2( 548)/  545,  546/
            data b1( 549),b2( 549)/  546,  547/
            data b1( 550),b2( 550)/  547,  548/
            data b1( 551),b2( 551)/  548,  549/
            data b1( 552),b2( 552)/  549,  550/
            data b1( 553),b2( 553)/  550,  551/
            data b1( 554),b2( 554)/  551,  552/
            data b1( 555),b2( 555)/  552,  553/
            data b1( 556),b2( 556)/  553,  554/
            data b1( 557),b2( 557)/  554,  555/
            data b1( 558),b2( 558)/  555,  556/
            data b1( 559),b2( 559)/  556,  557/
            data b1( 560),b2( 560)/  557,  558/
            data b1( 561),b2( 561)/  558,  559/
            data b1( 562),b2( 562)/  559,  560/
            data b1( 563),b2( 563)/  560,  561/
            data b1( 564),b2( 564)/  561,  562/
            data b1( 565),b2( 565)/  562,  563/
            data b1( 566),b2( 566)/  563,  564/
            data b1( 567),b2( 567)/  564,  565/
            data b1( 568),b2( 568)/  565,  566/
            data b1( 569),b2( 569)/  566,  567/
            data b1( 570),b2( 570)/  567,  568/
            data b1( 571),b2( 571)/  568,  569/
            data b1( 572),b2( 572)/  569,  570/
            data b1( 573),b2( 573)/  570,  571/
            data b1( 574),b2( 574)/  571,  572/
            data b1( 575),b2( 575)/  572,  573/
            data b1( 576),b2( 576)/  573,  574/
            data b1( 577),b2( 577)/  574,  575/
            data b1( 578),b2( 578)/  575,  576/
            data b1( 579),b2( 579)/  576,  577/
            data b1( 580),b2( 580)/  577,  578/
            data b1( 581),b2( 581)/  578,  579/
            data b1( 582),b2( 582)/  579,  580/
            data b1( 583),b2( 583)/  580,  581/
            data b1( 584),b2( 584)/  581,  582/
            data b1( 585),b2( 585)/  582,  583/
            data b1( 586),b2( 586)/  583,  584/
            data b1( 587),b2( 587)/  509,  584/
            data b1( 588),b2( 588)/  152,  585/
            data b1( 589),b2( 589)/  585,  586/
            data b1( 590),b2( 590)/  586,  587/
            data b1( 591),b2( 591)/  587,  588/
            data b1( 592),b2( 592)/  588,  589/
            data b1( 593),b2( 593)/  589,  590/
            data b1( 594),b2( 594)/  590,  591/
            data b1( 595),b2( 595)/  591,  592/
            data b1( 596),b2( 596)/  592,  593/
            data b1( 597),b2( 597)/  593,  594/
            data b1( 598),b2( 598)/  594,  595/
            data b1( 599),b2( 599)/  595,  596/
            data b1( 600),b2( 600)/  596,  597/
            data b1( 601),b2( 601)/  597,  598/
            data b1( 602),b2( 602)/  598,  599/
            data b1( 603),b2( 603)/  599,  600/
            data b1( 604),b2( 604)/  600,  601/
            data b1( 605),b2( 605)/  601,  602/
            data b1( 606),b2( 606)/  602,  603/
            data b1( 607),b2( 607)/  576,  603/
            data b1( 608),b2( 608)/  537,  604/
            data b1( 609),b2( 609)/  604,  605/
            data b1( 610),b2( 610)/  605,  606/
            data b1( 611),b2( 611)/  606,  607/
            data b1( 612),b2( 612)/  607,  608/
            data b1( 613),b2( 613)/  608,  609/
            data b1( 614),b2( 614)/  609,  610/
            data b1( 615),b2( 615)/  610,  611/
            data b1( 616),b2( 616)/  611,  612/
            data b1( 617),b2( 617)/  612,  613/
            data b1( 618),b2( 618)/  613,  614/
            data b1( 619),b2( 619)/  614,  615/
            data b1( 620),b2( 620)/  615,  616/
            data b1( 621),b2( 621)/  616,  617/
            data b1( 622),b2( 622)/  617,  618/
            data b1( 623),b2( 623)/  618,  619/
            data b1( 624),b2( 624)/  619,  620/
            data b1( 625),b2( 625)/  620,  621/
            data b1( 626),b2( 626)/  621,  622/
            data b1( 627),b2( 627)/  622,  623/
            data b1( 628),b2( 628)/  623,  624/
            data b1( 629),b2( 629)/  624,  625/
            data b1( 630),b2( 630)/  625,  626/
            data b1( 631),b2( 631)/  626,  627/
            data b1( 632),b2( 632)/  627,  628/
            data b1( 633),b2( 633)/  628,  629/
            data b1( 634),b2( 634)/  629,  630/
            data b1( 635),b2( 635)/  630,  631/
            data b1( 636),b2( 636)/  631,  632/
            data b1( 637),b2( 637)/  632,  633/
            data b1( 638),b2( 638)/  633,  634/
            data b1( 639),b2( 639)/  634,  635/
            data b1( 640),b2( 640)/  635,  636/
            data b1( 641),b2( 641)/  636,  637/
            data b1( 642),b2( 642)/  637,  638/
            data b1( 643),b2( 643)/  638,  639/
            data b1( 644),b2( 644)/  639,  640/
            data b1( 645),b2( 645)/  640,  641/
            data b1( 646),b2( 646)/  641,  642/
            data b1( 647),b2( 647)/  642,  643/
            data b1( 648),b2( 648)/  643,  644/
            data b1( 649),b2( 649)/  644,  645/
            data b1( 650),b2( 650)/  645,  646/
            data b1( 651),b2( 651)/  646,  647/
            data b1( 652),b2( 652)/  647,  648/
            data b1( 653),b2( 653)/  648,  649/
            data b1( 654),b2( 654)/  649,  650/
            data b1( 655),b2( 655)/  650,  651/
            data b1( 656),b2( 656)/  651,  652/
            data b1( 657),b2( 657)/  652,  653/
            data b1( 658),b2( 658)/  653,  654/
            data b1( 659),b2( 659)/  654,  655/
            data b1( 660),b2( 660)/  655,  656/
            data b1( 661),b2( 661)/  656,  657/
            data b1( 662),b2( 662)/  657,  658/
            data b1( 663),b2( 663)/  658,  659/
            data b1( 664),b2( 664)/  659,  660/
            data b1( 665),b2( 665)/  538,  660/
            data b1( 666),b2( 666)/  604,  661/
            data b1( 667),b2( 667)/  661,  662/
            data b1( 668),b2( 668)/  662,  663/
            data b1( 669),b2( 669)/  663,  664/
            data b1( 670),b2( 670)/  664,  665/
            data b1( 671),b2( 671)/  665,  666/
            data b1( 672),b2( 672)/  666,  667/
            data b1( 673),b2( 673)/  667,  668/
            data b1( 674),b2( 674)/  668,  669/
            data b1( 675),b2( 675)/  669,  670/
            data b1( 676),b2( 676)/  670,  671/
            data b1( 677),b2( 677)/  671,  672/
            data b1( 678),b2( 678)/  672,  673/
            data b1( 679),b2( 679)/  673,  674/
            data b1( 680),b2( 680)/  674,  675/
            data b1( 681),b2( 681)/  675,  676/
            data b1( 682),b2( 682)/  676,  677/
            data b1( 683),b2( 683)/  677,  678/
            data b1( 684),b2( 684)/  678,  679/
            data b1( 685),b2( 685)/  679,  680/
            data b1( 686),b2( 686)/  680,  681/
            data b1( 687),b2( 687)/  681,  682/
            data b1( 688),b2( 688)/  682,  683/
            data b1( 689),b2( 689)/  683,  684/
            data b1( 690),b2( 690)/  684,  685/
            data b1( 691),b2( 691)/  685,  686/
            data b1( 692),b2( 692)/  686,  687/
            data b1( 693),b2( 693)/  687,  688/
            data b1( 694),b2( 694)/  688,  689/
            data b1( 695),b2( 695)/  689,  690/
            data b1( 696),b2( 696)/  690,  691/
            data b1( 697),b2( 697)/  691,  692/
            data b1( 698),b2( 698)/  692,  693/
            data b1( 699),b2( 699)/  693,  694/
            data b1( 700),b2( 700)/  694,  695/
            data b1( 701),b2( 701)/  695,  696/
            data b1( 702),b2( 702)/  696,  697/
            data b1( 703),b2( 703)/  697,  698/
            data b1( 704),b2( 704)/  698,  699/
            data b1( 705),b2( 705)/  699,  700/
            data b1( 706),b2( 706)/  700,  701/
            data b1( 707),b2( 707)/  701,  702/
            data b1( 708),b2( 708)/  702,  703/
            data b1( 709),b2( 709)/  703,  704/
            data b1( 710),b2( 710)/  704,  705/
            data b1( 711),b2( 711)/  705,  706/
            data b1( 712),b2( 712)/  706,  707/
            data b1( 713),b2( 713)/  707,  708/
            data b1( 714),b2( 714)/  708,  709/
            data b1( 715),b2( 715)/  241,  709/
            data b1( 716),b2( 716)/  710,  711/
            data b1( 717),b2( 717)/  711,  712/
            data b1( 718),b2( 718)/  712,  713/
            data b1( 719),b2( 719)/  713,  714/
            data b1( 720),b2( 720)/  714,  715/
            data b1( 721),b2( 721)/  715,  716/
            data b1( 722),b2( 722)/  716,  717/
            data b1( 723),b2( 723)/  717,  718/
            data b1( 724),b2( 724)/  718,  719/
            data b1( 725),b2( 725)/  719,  720/
            data b1( 726),b2( 726)/  720,  721/
            data b1( 727),b2( 727)/  721,  722/
            data b1( 728),b2( 728)/  722,  723/
            data b1( 729),b2( 729)/  723,  724/
            data b1( 730),b2( 730)/  724,  725/
            data b1( 731),b2( 731)/  725,  726/
            data b1( 732),b2( 732)/  726,  727/
            data b1( 733),b2( 733)/  727,  728/
            data b1( 734),b2( 734)/  728,  729/
            data b1( 735),b2( 735)/  729,  730/
            data b1( 736),b2( 736)/  730,  731/
            data b1( 737),b2( 737)/  731,  732/
            data b1( 738),b2( 738)/  732,  733/
            data b1( 739),b2( 739)/  733,  734/
            data b1( 740),b2( 740)/  734,  735/
            data b1( 741),b2( 741)/  735,  736/
            data b1( 742),b2( 742)/  736,  737/
            data b1( 743),b2( 743)/  737,  738/
            data b1( 744),b2( 744)/  738,  739/
            data b1( 745),b2( 745)/  739,  740/
            data b1( 746),b2( 746)/  740,  741/
            data b1( 747),b2( 747)/  741,  742/
            data b1( 748),b2( 748)/  742,  743/
            data b1( 749),b2( 749)/  743,  744/
            data b1( 750),b2( 750)/  605,  744/
            data b1( 751),b2( 751)/  585,  745/
            data b1( 752),b2( 752)/  710,  745/
            data b1( 753),b2( 753)/  710,  746/
            data b1( 754),b2( 754)/  746,  747/
            data b1( 755),b2( 755)/  747,  748/
            data b1( 756),b2( 756)/  748,  749/
            data b1( 757),b2( 757)/  749,  750/
            data b1( 758),b2( 758)/  750,  751/
            data b1( 759),b2( 759)/  751,  752/
            data b1( 760),b2( 760)/  752,  753/
            data b1( 761),b2( 761)/  753,  754/
            data b1( 762),b2( 762)/  754,  755/
            data b1( 763),b2( 763)/  628,  755/
            data b1( 764),b2( 764)/  707,  756/
            data b1( 765),b2( 765)/  756,  757/
            data b1( 766),b2( 766)/  757,  758/
            data b1( 767),b2( 767)/  758,  759/
            data b1( 768),b2( 768)/  759,  760/
            data b1( 769),b2( 769)/  760,  761/
            data b1( 770),b2( 770)/  761,  762/
            data b1( 771),b2( 771)/  762,  763/
            data b1( 772),b2( 772)/  763,  764/
            data b1( 773),b2( 773)/  764,  765/
            data b1( 774),b2( 774)/  765,  766/
            data b1( 775),b2( 775)/  766,  767/
            data b1( 776),b2( 776)/  767,  768/
            data b1( 777),b2( 777)/  768,  769/
            data b1( 778),b2( 778)/  769,  770/
            data b1( 779),b2( 779)/  770,  771/
            data b1( 780),b2( 780)/  771,  772/
            data b1( 781),b2( 781)/  772,  773/
            data b1( 782),b2( 782)/  773,  774/
            data b1( 783),b2( 783)/  774,  775/
            data b1( 784),b2( 784)/  775,  776/
            data b1( 785),b2( 785)/  776,  777/
            data b1( 786),b2( 786)/  777,  778/
            data b1( 787),b2( 787)/  778,  779/
            data b1( 788),b2( 788)/  779,  780/
            data b1( 789),b2( 789)/  780,  781/
            data b1( 790),b2( 790)/  781,  782/
            data b1( 791),b2( 791)/  782,  783/
            data b1( 792),b2( 792)/  783,  784/
            data b1( 793),b2( 793)/  784,  785/
            data b1( 794),b2( 794)/  785,  786/
            data b1( 795),b2( 795)/  786,  787/
            data b1( 796),b2( 796)/  787,  788/
            data b1( 797),b2( 797)/  788,  789/
            data b1( 798),b2( 798)/  789,  790/
            data b1( 799),b2( 799)/  790,  791/
            data b1( 800),b2( 800)/  791,  792/
            data b1( 801),b2( 801)/  792,  793/
            data b1( 802),b2( 802)/  793,  794/
            data b1( 803),b2( 803)/  794,  795/
            data b1( 804),b2( 804)/  795,  796/
            data b1( 805),b2( 805)/  796,  797/
            data b1( 806),b2( 806)/  797,  798/
            data b1( 807),b2( 807)/  798,  799/
            data b1( 808),b2( 808)/  799,  800/
            data b1( 809),b2( 809)/  800,  801/
            data b1( 810),b2( 810)/  801,  802/
            data b1( 811),b2( 811)/  802,  803/
            data b1( 812),b2( 812)/  733,  803/
            data b1( 813),b2( 813)/  804,  805/
            data b1( 814),b2( 814)/  805,  806/
            data b1( 815),b2( 815)/  806,  807/
            data b1( 816),b2( 816)/  807,  808/
            data b1( 817),b2( 817)/  808,  809/
            data b1( 818),b2( 818)/  809,  810/
            data b1( 819),b2( 819)/  810,  811/
            data b1( 820),b2( 820)/  811,  812/
            data b1( 821),b2( 821)/  812,  813/
            data b1( 822),b2( 822)/  813,  814/
            data b1( 823),b2( 823)/  814,  815/
            data b1( 824),b2( 824)/  815,  816/
            data b1( 825),b2( 825)/  816,  817/
            data b1( 826),b2( 826)/  817,  818/
            data b1( 827),b2( 827)/  818,  819/
            data b1( 828),b2( 828)/  819,  820/
            data b1( 829),b2( 829)/  820,  821/
            data b1( 830),b2( 830)/  821,  822/
            data b1( 831),b2( 831)/  822,  823/
            data b1( 832),b2( 832)/  823,  824/
            data b1( 833),b2( 833)/  824,  825/
            data b1( 834),b2( 834)/  825,  826/
            data b1( 835),b2( 835)/  826,  827/
            data b1( 836),b2( 836)/  827,  828/
            data b1( 837),b2( 837)/  767,  828/
            data b1( 838),b2( 838)/  829,  830/
            data b1( 839),b2( 839)/  830,  831/
            data b1( 840),b2( 840)/  831,  832/
            data b1( 841),b2( 841)/  832,  833/
            data b1( 842),b2( 842)/  833,  834/
            data b1( 843),b2( 843)/  834,  835/
            data b1( 844),b2( 844)/  835,  836/
            data b1( 845),b2( 845)/  836,  837/
            data b1( 846),b2( 846)/  837,  838/
            data b1( 847),b2( 847)/  838,  839/
            data b1( 848),b2( 848)/  839,  840/
            data b1( 849),b2( 849)/  840,  841/
            data b1( 850),b2( 850)/  841,  842/
            data b1( 851),b2( 851)/  842,  843/
            data b1( 852),b2( 852)/  843,  844/
            data b1( 853),b2( 853)/  844,  845/
            data b1( 854),b2( 854)/  845,  846/
            data b1( 855),b2( 855)/  846,  847/
            data b1( 856),b2( 856)/  847,  848/
            data b1( 857),b2( 857)/  848,  849/
            data b1( 858),b2( 858)/  849,  850/
            data b1( 859),b2( 859)/  850,  851/
            data b1( 860),b2( 860)/  851,  852/
            data b1( 861),b2( 861)/  852,  853/
            data b1( 862),b2( 862)/  853,  854/
            data b1( 863),b2( 863)/  854,  855/
            data b1( 864),b2( 864)/  855,  856/
            data b1( 865),b2( 865)/  856,  857/
            data b1( 866),b2( 866)/  857,  858/
            data b1( 867),b2( 867)/  766,  859/
            data b1( 868),b2( 868)/  859,  860/
            data b1( 869),b2( 869)/  860,  861/
            data b1( 870),b2( 870)/  861,  862/
            data b1( 871),b2( 871)/  862,  863/
            data b1( 872),b2( 872)/  863,  864/
            data b1( 873),b2( 873)/  864,  865/
            data b1( 874),b2( 874)/  865,  866/
            data b1( 875),b2( 875)/  866,  867/
            data b1( 876),b2( 876)/  867,  868/
            data b1( 877),b2( 877)/  868,  869/
            data b1( 878),b2( 878)/  869,  870/
            data b1( 879),b2( 879)/  870,  871/
            data b1( 880),b2( 880)/  871,  872/
            data b1( 881),b2( 881)/  872,  873/
            data b1( 882),b2( 882)/  873,  874/
            data b1( 883),b2( 883)/  249,  874/
            data b1( 884),b2( 884)/  875,  876/
            data b1( 885),b2( 885)/  876,  877/
            data b1( 886),b2( 886)/  877,  878/
            data b1( 887),b2( 887)/  878,  879/
            data b1( 888),b2( 888)/  879,  880/
            data b1( 889),b2( 889)/  880,  881/
            data b1( 890),b2( 890)/  881,  882/
            data b1( 891),b2( 891)/  882,  883/
            data b1( 892),b2( 892)/  883,  884/
            data b1( 893),b2( 893)/  884,  885/
            data b1( 894),b2( 894)/  885,  886/
            data b1( 895),b2( 895)/  886,  887/
            data b1( 896),b2( 896)/  887,  888/
            data b1( 897),b2( 897)/  888,  889/
            data b1( 898),b2( 898)/  889,  890/
            data b1( 899),b2( 899)/  890,  891/
            data b1( 900),b2( 900)/  891,  892/
            data b1( 901),b2( 901)/  892,  893/
            data b1( 902),b2( 902)/  893,  894/
            data b1( 903),b2( 903)/  894,  895/
            data b1( 904),b2( 904)/  895,  896/
            data b1( 905),b2( 905)/  858,  896/
            data b1( 906),b2( 906)/  858,  897/
            data b1( 907),b2( 907)/  897,  898/
            data b1( 908),b2( 908)/  898,  899/
            data b1( 909),b2( 909)/  899,  900/
            data b1( 910),b2( 910)/  716,  900/
            data b1( 911),b2( 911)/  717,  901/
            data b1( 912),b2( 912)/  901,  902/
            data b1( 913),b2( 913)/  902,  903/
            data b1( 914),b2( 914)/  903,  904/
            data b1( 915),b2( 915)/  904,  905/
            data b1( 916),b2( 916)/  905,  906/
            data b1( 917),b2( 917)/  906,  907/
            data b1( 918),b2( 918)/  907,  908/
            data b1( 919),b2( 919)/  908,  909/
            data b1( 920),b2( 920)/  909,  910/
            data b1( 921),b2( 921)/  910,  911/
            data b1( 922),b2( 922)/  911,  912/
            data b1( 923),b2( 923)/  912,  913/
            data b1( 924),b2( 924)/  913,  914/
            data b1( 925),b2( 925)/  914,  915/
            data b1( 926),b2( 926)/  915,  916/
            data b1( 927),b2( 927)/  916,  917/
            data b1( 928),b2( 928)/  917,  918/
            data b1( 929),b2( 929)/  804,  918/
            data b1( 930),b2( 930)/  804,  919/
            data b1( 931),b2( 931)/  919,  920/
            data b1( 932),b2( 932)/  920,  921/
            data b1( 933),b2( 933)/  921,  922/
            data b1( 934),b2( 934)/  895,  922/
            data b1( 935),b2( 935)/  923,  924/
            data b1( 936),b2( 936)/  924,  925/
            data b1( 937),b2( 937)/  875,  925/
            data b1( 938),b2( 938)/  875,  926/
            data b1( 939),b2( 939)/  926,  927/
            data b1( 940),b2( 940)/  927,  928/
            data b1( 941),b2( 941)/  928,  929/
            data b1( 942),b2( 942)/  929,  930/
            data b1( 943),b2( 943)/  930,  931/
            data b1( 944),b2( 944)/  931,  932/
            data b1( 945),b2( 945)/  932,  933/
            data b1( 946),b2( 946)/  933,  934/
            data b1( 947),b2( 947)/  934,  935/
            data b1( 948),b2( 948)/  935,  936/
            data b1( 949),b2( 949)/  936,  937/
            data b1( 950),b2( 950)/  805,  937/
            data b1( 951),b2( 951)/  255,  938/
            data b1( 952),b2( 952)/  938,  939/
            data b1( 953),b2( 953)/  939,  940/
            data b1( 954),b2( 954)/  940,  941/
            data b1( 955),b2( 955)/  941,  942/
            data b1( 956),b2( 956)/  942,  943/
            data b1( 957),b2( 957)/  943,  944/
            data b1( 958),b2( 958)/  944,  945/
            data b1( 959),b2( 959)/  945,  946/
            data b1( 960),b2( 960)/  946,  947/
            data b1( 961),b2( 961)/  947,  948/
            data b1( 962),b2( 962)/  948,  949/
            data b1( 963),b2( 963)/  949,  950/
            data b1( 964),b2( 964)/  950,  951/
            data b1( 965),b2( 965)/  951,  952/
            data b1( 966),b2( 966)/  952,  953/
            data b1( 967),b2( 967)/  953,  954/
            data b1( 968),b2( 968)/  954,  955/
            data b1( 969),b2( 969)/  955,  956/
            data b1( 970),b2( 970)/  956,  957/
            data b1( 971),b2( 971)/  957,  958/
            data b1( 972),b2( 972)/  958,  959/
            data b1( 973),b2( 973)/  959,  960/
            data b1( 974),b2( 974)/  960,  961/
            data b1( 975),b2( 975)/  961,  962/
            data b1( 976),b2( 976)/  962,  963/
            data b1( 977),b2( 977)/  963,  964/
            data b1( 978),b2( 978)/  964,  965/
            data b1( 979),b2( 979)/  863,  965/
            data b1( 980),b2( 980)/  966,  967/
            data b1( 981),b2( 981)/  967,  968/
            data b1( 982),b2( 982)/  968,  969/
            data b1( 983),b2( 983)/  969,  970/
            data b1( 984),b2( 984)/  970,  971/
            data b1( 985),b2( 985)/  971,  972/
            data b1( 986),b2( 986)/  972,  973/
            data b1( 987),b2( 987)/  973,  974/
            data b1( 988),b2( 988)/  974,  975/
            data b1( 989),b2( 989)/  975,  976/
            data b1( 990),b2( 990)/  976,  977/
            data b1( 991),b2( 991)/  977,  978/
            data b1( 992),b2( 992)/  978,  979/
            data b1( 993),b2( 993)/  979,  980/
            data b1( 994),b2( 994)/  980,  981/
            data b1( 995),b2( 995)/  981,  982/
            data b1( 996),b2( 996)/  982,  983/
            data b1( 997),b2( 997)/  983,  984/
            data b1( 998),b2( 998)/  984,  985/
            data b1( 999),b2( 999)/  985,  986/
            data b1(1000),b2(1000)/  986,  987/
            data b1(1001),b2(1001)/  987,  988/
            data b1(1002),b2(1002)/  988,  989/
            data b1(1003),b2(1003)/  989,  990/
            data b1(1004),b2(1004)/  990,  991/
            data b1(1005),b2(1005)/  991,  992/
            data b1(1006),b2(1006)/  992,  993/
            data b1(1007),b2(1007)/  993,  994/
            data b1(1008),b2(1008)/  994,  995/
            data b1(1009),b2(1009)/  995,  996/
            data b1(1010),b2(1010)/  258,  996/
            data b1(1011),b2(1011)/  807,  997/
            data b1(1012),b2(1012)/  997,  998/
            data b1(1013),b2(1013)/  998,  999/
            data b1(1014),b2(1014)/  999, 1000/
            data b1(1015),b2(1015)/ 1000, 1001/
            data b1(1016),b2(1016)/ 1001, 1002/
            data b1(1017),b2(1017)/ 1002, 1003/
            data b1(1018),b2(1018)/ 1003, 1004/
            data b1(1019),b2(1019)/ 1004, 1005/
            data b1(1020),b2(1020)/ 1005, 1006/
            data b1(1021),b2(1021)/ 1006, 1007/
            data b1(1022),b2(1022)/ 1007, 1008/
            data b1(1023),b2(1023)/ 1008, 1009/
            data b1(1024),b2(1024)/ 1009, 1010/
            data b1(1025),b2(1025)/ 1010, 1011/
            data b1(1026),b2(1026)/ 1011, 1012/
            data b1(1027),b2(1027)/ 1012, 1013/
            data b1(1028),b2(1028)/ 1013, 1014/
            data b1(1029),b2(1029)/  954, 1014/
            data b1(1030),b2(1030)/ 1015, 1016/
            data b1(1031),b2(1031)/ 1016, 1017/
            data b1(1032),b2(1032)/ 1017, 1018/
            data b1(1033),b2(1033)/ 1018, 1019/
            data b1(1034),b2(1034)/ 1019, 1020/
            data b1(1035),b2(1035)/ 1020, 1021/
            data b1(1036),b2(1036)/ 1021, 1022/
            data b1(1037),b2(1037)/ 1022, 1023/
            data b1(1038),b2(1038)/ 1023, 1024/
            data b1(1039),b2(1039)/ 1024, 1025/
            data b1(1040),b2(1040)/ 1025, 1026/
            data b1(1041),b2(1041)/ 1026, 1027/
            data b1(1042),b2(1042)/ 1027, 1028/
            data b1(1043),b2(1043)/ 1028, 1029/
            data b1(1044),b2(1044)/ 1029, 1030/
            data b1(1045),b2(1045)/ 1030, 1031/
            data b1(1046),b2(1046)/ 1031, 1032/
            data b1(1047),b2(1047)/ 1032, 1033/
            data b1(1048),b2(1048)/ 1033, 1034/
            data b1(1049),b2(1049)/ 1034, 1035/
            data b1(1050),b2(1050)/ 1035, 1036/
            data b1(1051),b2(1051)/ 1036, 1037/
            data b1(1052),b2(1052)/ 1037, 1038/
            data b1(1053),b2(1053)/ 1038, 1039/
            data b1(1054),b2(1054)/ 1039, 1040/
            data b1(1055),b2(1055)/ 1040, 1041/
            data b1(1056),b2(1056)/ 1041, 1042/
            data b1(1057),b2(1057)/ 1042, 1043/
            data b1(1058),b2(1058)/ 1043, 1044/
            data b1(1059),b2(1059)/ 1044, 1045/
            data b1(1060),b2(1060)/ 1045, 1046/
            data b1(1061),b2(1061)/ 1046, 1047/
            data b1(1062),b2(1062)/ 1047, 1048/
            data b1(1063),b2(1063)/ 1048, 1049/
            data b1(1064),b2(1064)/ 1049, 1050/
            data b1(1065),b2(1065)/ 1050, 1051/
            data b1(1066),b2(1066)/ 1051, 1052/
            data b1(1067),b2(1067)/ 1052, 1053/
            data b1(1068),b2(1068)/ 1053, 1054/
            data b1(1069),b2(1069)/ 1054, 1055/
            data b1(1070),b2(1070)/ 1055, 1056/
            data b1(1071),b2(1071)/ 1056, 1057/
            data b1(1072),b2(1072)/ 1057, 1058/
            data b1(1073),b2(1073)/ 1058, 1059/
            data b1(1074),b2(1074)/ 1059, 1060/
            data b1(1075),b2(1075)/  923, 1060/
            data b1(1076),b2(1076)/  923, 1061/
            data b1(1077),b2(1077)/ 1061, 1062/
            data b1(1078),b2(1078)/ 1062, 1063/
            data b1(1079),b2(1079)/ 1063, 1064/
            data b1(1080),b2(1080)/ 1064, 1065/
            data b1(1081),b2(1081)/ 1065, 1066/
            data b1(1082),b2(1082)/ 1015, 1066/
            data b1(1083),b2(1083)/ 1015, 1067/
            data b1(1084),b2(1084)/ 1067, 1068/
            data b1(1085),b2(1085)/ 1068, 1069/
            data b1(1086),b2(1086)/  829, 1069/
            data b1(1087),b2(1087)/  829, 1070/
            data b1(1088),b2(1088)/ 1070, 1071/
            data b1(1089),b2(1089)/ 1071, 1072/
            data b1(1090),b2(1090)/ 1072, 1073/
            data b1(1091),b2(1091)/ 1073, 1074/
            data b1(1092),b2(1092)/ 1074, 1075/
            data b1(1093),b2(1093)/ 1075, 1076/
            data b1(1094),b2(1094)/ 1076, 1077/
            data b1(1095),b2(1095)/  119, 1077/
            data b1(1096),b2(1096)/  926, 1078/
            data b1(1097),b2(1097)/ 1078, 1079/
            data b1(1098),b2(1098)/ 1079, 1080/
            data b1(1099),b2(1099)/ 1080, 1081/
            data b1(1100),b2(1100)/ 1081, 1082/
            data b1(1101),b2(1101)/ 1082, 1083/
            data b1(1102),b2(1102)/ 1083, 1084/
            data b1(1103),b2(1103)/ 1084, 1085/
            data b1(1104),b2(1104)/ 1085, 1086/
            data b1(1105),b2(1105)/ 1086, 1087/
            data b1(1106),b2(1106)/ 1087, 1088/
            data b1(1107),b2(1107)/ 1088, 1089/
            data b1(1108),b2(1108)/ 1089, 1090/
            data b1(1109),b2(1109)/ 1090, 1091/
            data b1(1110),b2(1110)/ 1091, 1092/
            data b1(1111),b2(1111)/ 1092, 1093/
            data b1(1112),b2(1112)/ 1093, 1094/
            data b1(1113),b2(1113)/ 1094, 1095/
            data b1(1114),b2(1114)/ 1095, 1096/
            data b1(1115),b2(1115)/ 1096, 1097/
            data b1(1116),b2(1116)/ 1097, 1098/
            data b1(1117),b2(1117)/ 1098, 1099/
            data b1(1118),b2(1118)/ 1099, 1100/
            data b1(1119),b2(1119)/ 1100, 1101/
            data b1(1120),b2(1120)/ 1013, 1101/
            data b1(1121),b2(1121)/ 1102, 1103/
            data b1(1122),b2(1122)/ 1103, 1104/
            data b1(1123),b2(1123)/ 1104, 1105/
            data b1(1124),b2(1124)/ 1105, 1106/
            data b1(1125),b2(1125)/ 1106, 1107/
            data b1(1126),b2(1126)/  261, 1107/
            data b1(1127),b2(1127)/ 1108, 1109/
            data b1(1128),b2(1128)/ 1109, 1110/
            data b1(1129),b2(1129)/ 1110, 1111/
            data b1(1130),b2(1130)/ 1111, 1112/
            data b1(1131),b2(1131)/ 1112, 1113/
            data b1(1132),b2(1132)/ 1113, 1114/
            data b1(1133),b2(1133)/ 1114, 1115/
            data b1(1134),b2(1134)/ 1115, 1116/
            data b1(1135),b2(1135)/ 1116, 1117/
            data b1(1136),b2(1136)/  265, 1117/
            data b1(1137),b2(1137)/ 1050, 1118/
            data b1(1138),b2(1138)/ 1118, 1119/
            data b1(1139),b2(1139)/ 1119, 1120/
            data b1(1140),b2(1140)/ 1120, 1121/
            data b1(1141),b2(1141)/ 1121, 1122/
            data b1(1142),b2(1142)/ 1122, 1123/
            data b1(1143),b2(1143)/ 1123, 1124/
            data b1(1144),b2(1144)/ 1124, 1125/
            data b1(1145),b2(1145)/ 1125, 1126/
            data b1(1146),b2(1146)/ 1126, 1127/
            data b1(1147),b2(1147)/ 1127, 1128/
            data b1(1148),b2(1148)/ 1128, 1129/
            data b1(1149),b2(1149)/ 1129, 1130/
            data b1(1150),b2(1150)/ 1130, 1131/
            data b1(1151),b2(1151)/ 1131, 1132/
            data b1(1152),b2(1152)/ 1132, 1133/
            data b1(1153),b2(1153)/ 1133, 1134/
            data b1(1154),b2(1154)/ 1134, 1135/
            data b1(1155),b2(1155)/ 1135, 1136/
            data b1(1156),b2(1156)/ 1136, 1137/
            data b1(1157),b2(1157)/ 1137, 1138/
            data b1(1158),b2(1158)/ 1138, 1139/
            data b1(1159),b2(1159)/ 1139, 1140/
            data b1(1160),b2(1160)/ 1140, 1141/
            data b1(1161),b2(1161)/ 1141, 1142/
            data b1(1162),b2(1162)/ 1142, 1143/
            data b1(1163),b2(1163)/ 1143, 1144/
            data b1(1164),b2(1164)/ 1144, 1145/
            data b1(1165),b2(1165)/ 1096, 1145/
            data b1(1166),b2(1166)/  966, 1097/
            data b1(1167),b2(1167)/  966, 1146/
            data b1(1168),b2(1168)/ 1146, 1147/
            data b1(1169),b2(1169)/ 1147, 1148/
            data b1(1170),b2(1170)/ 1148, 1149/
            data b1(1171),b2(1171)/ 1149, 1150/
            data b1(1172),b2(1172)/ 1150, 1151/
            data b1(1173),b2(1173)/ 1151, 1152/
            data b1(1174),b2(1174)/ 1152, 1153/
            data b1(1175),b2(1175)/ 1153, 1154/
            data b1(1176),b2(1176)/ 1154, 1155/
            data b1(1177),b2(1177)/ 1155, 1156/
            data b1(1178),b2(1178)/ 1156, 1157/
            data b1(1179),b2(1179)/ 1157, 1158/
            data b1(1180),b2(1180)/ 1158, 1159/
            data b1(1181),b2(1181)/ 1159, 1160/
            data b1(1182),b2(1182)/ 1160, 1161/
            data b1(1183),b2(1183)/ 1161, 1162/
            data b1(1184),b2(1184)/ 1162, 1163/
            data b1(1185),b2(1185)/ 1163, 1164/
            data b1(1186),b2(1186)/ 1164, 1165/
            data b1(1187),b2(1187)/ 1165, 1166/
            data b1(1188),b2(1188)/ 1166, 1167/
            data b1(1189),b2(1189)/ 1167, 1168/
            data b1(1190),b2(1190)/ 1168, 1169/
            data b1(1191),b2(1191)/ 1169, 1170/
            data b1(1192),b2(1192)/ 1170, 1171/
            data b1(1193),b2(1193)/ 1103, 1171/
            data b1(1194),b2(1194)/ 1172, 1173/
            data b1(1195),b2(1195)/ 1173, 1174/
            data b1(1196),b2(1196)/ 1174, 1175/
            data b1(1197),b2(1197)/ 1175, 1176/
            data b1(1198),b2(1198)/ 1176, 1177/
            data b1(1199),b2(1199)/ 1177, 1178/
            data b1(1200),b2(1200)/ 1178, 1179/
            data b1(1201),b2(1201)/ 1179, 1180/
            data b1(1202),b2(1202)/ 1180, 1181/
            data b1(1203),b2(1203)/ 1181, 1182/
            data b1(1204),b2(1204)/ 1182, 1183/
            data b1(1205),b2(1205)/ 1183, 1184/
            data b1(1206),b2(1206)/ 1184, 1185/
            data b1(1207),b2(1207)/ 1185, 1186/
            data b1(1208),b2(1208)/ 1186, 1187/
            data b1(1209),b2(1209)/ 1187, 1188/
            data b1(1210),b2(1210)/ 1188, 1189/
            data b1(1211),b2(1211)/ 1189, 1190/
            data b1(1212),b2(1212)/ 1190, 1191/
            data b1(1213),b2(1213)/ 1191, 1192/
            data b1(1214),b2(1214)/ 1192, 1193/
            data b1(1215),b2(1215)/ 1193, 1194/
            data b1(1216),b2(1216)/ 1194, 1195/
            data b1(1217),b2(1217)/ 1195, 1196/
            data b1(1218),b2(1218)/ 1196, 1197/
            data b1(1219),b2(1219)/ 1197, 1198/
            data b1(1220),b2(1220)/ 1198, 1199/
            data b1(1221),b2(1221)/ 1199, 1200/
            data b1(1222),b2(1222)/ 1200, 1201/
            data b1(1223),b2(1223)/ 1201, 1202/
            data b1(1224),b2(1224)/ 1202, 1203/
            data b1(1225),b2(1225)/ 1203, 1204/
            data b1(1226),b2(1226)/ 1204, 1205/
            data b1(1227),b2(1227)/ 1205, 1206/
            data b1(1228),b2(1228)/ 1206, 1207/
            data b1(1229),b2(1229)/ 1207, 1208/
            data b1(1230),b2(1230)/ 1208, 1209/
            data b1(1231),b2(1231)/ 1209, 1210/
            data b1(1232),b2(1232)/ 1210, 1211/
            data b1(1233),b2(1233)/ 1211, 1212/
            data b1(1234),b2(1234)/ 1212, 1213/
            data b1(1235),b2(1235)/ 1213, 1214/
            data b1(1236),b2(1236)/ 1214, 1215/
            data b1(1237),b2(1237)/ 1215, 1216/
            data b1(1238),b2(1238)/ 1216, 1217/
            data b1(1239),b2(1239)/ 1217, 1218/
            data b1(1240),b2(1240)/ 1218, 1219/
            data b1(1241),b2(1241)/ 1219, 1220/
            data b1(1242),b2(1242)/ 1220, 1221/
            data b1(1243),b2(1243)/ 1221, 1222/
            data b1(1244),b2(1244)/ 1222, 1223/
            data b1(1245),b2(1245)/ 1223, 1224/
            data b1(1246),b2(1246)/ 1224, 1225/
            data b1(1247),b2(1247)/ 1225, 1226/
            data b1(1248),b2(1248)/ 1226, 1227/
            data b1(1249),b2(1249)/ 1227, 1228/
            data b1(1250),b2(1250)/ 1228, 1229/
            data b1(1251),b2(1251)/ 1229, 1230/
            data b1(1252),b2(1252)/ 1230, 1231/
            data b1(1253),b2(1253)/ 1231, 1232/
            data b1(1254),b2(1254)/ 1232, 1233/
            data b1(1255),b2(1255)/ 1233, 1234/
            data b1(1256),b2(1256)/ 1234, 1235/
            data b1(1257),b2(1257)/ 1235, 1236/
            data b1(1258),b2(1258)/ 1236, 1237/
            data b1(1259),b2(1259)/ 1237, 1238/
            data b1(1260),b2(1260)/ 1238, 1239/
            data b1(1261),b2(1261)/ 1239, 1240/
            data b1(1262),b2(1262)/ 1240, 1241/
            data b1(1263),b2(1263)/ 1241, 1242/
            data b1(1264),b2(1264)/ 1242, 1243/
            data b1(1265),b2(1265)/ 1243, 1244/
            data b1(1266),b2(1266)/ 1244, 1245/
            data b1(1267),b2(1267)/ 1245, 1246/
            data b1(1268),b2(1268)/ 1246, 1247/
            data b1(1269),b2(1269)/ 1247, 1248/
            data b1(1270),b2(1270)/ 1248, 1249/
            data b1(1271),b2(1271)/ 1249, 1250/
            data b1(1272),b2(1272)/ 1250, 1251/
            data b1(1273),b2(1273)/ 1251, 1252/
            data b1(1274),b2(1274)/ 1252, 1253/
            data b1(1275),b2(1275)/ 1253, 1254/
            data b1(1276),b2(1276)/ 1254, 1255/
            data b1(1277),b2(1277)/ 1255, 1256/
            data b1(1278),b2(1278)/ 1108, 1256/
            data b1(1279),b2(1279)/ 1108, 1257/
            data b1(1280),b2(1280)/ 1257, 1258/
            data b1(1281),b2(1281)/ 1258, 1259/
            data b1(1282),b2(1282)/ 1259, 1260/
            data b1(1283),b2(1283)/ 1260, 1261/
            data b1(1284),b2(1284)/ 1261, 1262/
            data b1(1285),b2(1285)/ 1262, 1263/
            data b1(1286),b2(1286)/ 1263, 1264/
            data b1(1287),b2(1287)/ 1264, 1265/
            data b1(1288),b2(1288)/ 1102, 1265/
            data b1(1289),b2(1289)/ 1102, 1266/
            data b1(1290),b2(1290)/ 1266, 1267/
            data b1(1291),b2(1291)/ 1267, 1268/
            data b1(1292),b2(1292)/ 1268, 1269/
            data b1(1293),b2(1293)/ 1269, 1270/
            data b1(1294),b2(1294)/ 1270, 1271/
            data b1(1295),b2(1295)/ 1271, 1272/
            data b1(1296),b2(1296)/ 1272, 1273/
            data b1(1297),b2(1297)/ 1273, 1274/
            data b1(1298),b2(1298)/ 1274, 1275/
            data b1(1299),b2(1299)/ 1275, 1276/
            data b1(1300),b2(1300)/ 1276, 1277/
            data b1(1301),b2(1301)/ 1277, 1278/
            data b1(1302),b2(1302)/ 1278, 1279/
            data b1(1303),b2(1303)/ 1279, 1280/
            data b1(1304),b2(1304)/ 1280, 1281/
            data b1(1305),b2(1305)/ 1281, 1282/
            data b1(1306),b2(1306)/ 1282, 1283/
            data b1(1307),b2(1307)/ 1283, 1284/
            data b1(1308),b2(1308)/ 1284, 1285/
            data b1(1309),b2(1309)/ 1172, 1286/
            data b1(1310),b2(1310)/ 1172, 1287/
            data b1(1311),b2(1311)/ 1287, 1288/
            data b1(1312),b2(1312)/ 1288, 1289/
            data b1(1313),b2(1313)/ 1289, 1290/
            data b1(1314),b2(1314)/ 1290, 1291/
            data b1(1315),b2(1315)/ 1291, 1292/
            data b1(1316),b2(1316)/ 1292, 1293/
            data b1(1317),b2(1317)/ 1293, 1294/
            data b1(1318),b2(1318)/ 1294, 1295/
            data b1(1319),b2(1319)/ 1295, 1296/
            data b1(1320),b2(1320)/ 1296, 1297/
            data b1(1321),b2(1321)/ 1297, 1298/
            data b1(1322),b2(1322)/ 1298, 1299/
            data b1(1323),b2(1323)/ 1299, 1300/
            data b1(1324),b2(1324)/ 1300, 1301/
            data b1(1325),b2(1325)/ 1263, 1301/
            data b1(1326),b2(1326)/  105, 1302/
            data b1(1327),b2(1327)/ 1302, 1303/
            data b1(1328),b2(1328)/ 1303, 1304/
            data b1(1329),b2(1329)/ 1304, 1305/
            data b1(1330),b2(1330)/ 1305, 1306/
            data b1(1331),b2(1331)/ 1306, 1307/
            data b1(1332),b2(1332)/ 1307, 1308/
            data b1(1333),b2(1333)/ 1308, 1309/
            data b1(1334),b2(1334)/ 1309, 1310/
            data b1(1335),b2(1335)/ 1038, 1310/
            data b1(1336),b2(1336)/ 1148, 1285/
            data b1(1337),b2(1337)/ 1285, 1311/
            data b1(1338),b2(1338)/ 1311, 1312/
            data b1(1339),b2(1339)/ 1312, 1313/
            data b1(1340),b2(1340)/ 1313, 1314/
            data b1(1341),b2(1341)/ 1314, 1315/
            data b1(1342),b2(1342)/ 1315, 1316/
            data b1(1343),b2(1343)/ 1316, 1317/
            data b1(1344),b2(1344)/ 1317, 1318/
            data b1(1345),b2(1345)/ 1318, 1319/
            data b1(1346),b2(1346)/ 1319, 1320/
            data b1(1347),b2(1347)/ 1320, 1321/
            data b1(1348),b2(1348)/ 1321, 1322/
            data b1(1349),b2(1349)/ 1322, 1323/
            data b1(1350),b2(1350)/ 1323, 1324/
            data b1(1351),b2(1351)/ 1324, 1325/
            data b1(1352),b2(1352)/ 1325, 1326/
            data b1(1353),b2(1353)/ 1326, 1327/
            data b1(1354),b2(1354)/ 1327, 1328/
            data b1(1355),b2(1355)/ 1328, 1329/
            data b1(1356),b2(1356)/ 1329, 1330/
            data b1(1357),b2(1357)/ 1330, 1331/
            data b1(1358),b2(1358)/ 1331, 1332/
            data b1(1359),b2(1359)/ 1332, 1333/
            data b1(1360),b2(1360)/ 1333, 1334/
            data b1(1361),b2(1361)/ 1334, 1335/
            data b1(1362),b2(1362)/ 1335, 1336/
            data b1(1363),b2(1363)/ 1336, 1337/
            data b1(1364),b2(1364)/ 1126, 1338/
            data b1(1365),b2(1365)/ 1338, 1339/
            data b1(1366),b2(1366)/ 1339, 1340/
            data b1(1367),b2(1367)/ 1340, 1341/
            data b1(1368),b2(1368)/ 1341, 1342/
            data b1(1369),b2(1369)/ 1342, 1343/
            data b1(1370),b2(1370)/ 1343, 1344/
            data b1(1371),b2(1371)/ 1337, 1344/
            data b1(1372),b2(1372)/ 1337, 1345/
            data b1(1373),b2(1373)/ 1345, 1346/
            data b1(1374),b2(1374)/ 1346, 1347/
            data b1(1375),b2(1375)/ 1347, 1348/
            data b1(1376),b2(1376)/ 1348, 1349/
            data b1(1377),b2(1377)/ 1310, 1349/
            data b1(1378),b2(1378)/ 1310, 1350/
            data b1(1379),b2(1379)/ 1350, 1351/
            data b1(1380),b2(1380)/ 1351, 1352/
            data b1(1381),b2(1381)/ 1352, 1353/
            data b1(1382),b2(1382)/ 1353, 1354/
            data b1(1383),b2(1383)/   80, 1354/
            data b1(1384),b2(1384)/ 1355, 1356/
            data b1(1385),b2(1385)/ 1356, 1357/
            data b1(1386),b2(1386)/ 1357, 1358/
            data b1(1387),b2(1387)/ 1358, 1359/
            data b1(1388),b2(1388)/ 1359, 1360/
            data b1(1389),b2(1389)/ 1360, 1361/
            data b1(1390),b2(1390)/ 1361, 1362/
            data b1(1391),b2(1391)/ 1362, 1363/
            data b1(1392),b2(1392)/ 1363, 1364/
            data b1(1393),b2(1393)/ 1364, 1365/
            data b1(1394),b2(1394)/ 1365, 1366/
            data b1(1395),b2(1395)/ 1366, 1367/
            data b1(1396),b2(1396)/ 1367, 1368/
            data b1(1397),b2(1397)/ 1368, 1369/
            data b1(1398),b2(1398)/ 1369, 1370/
            data b1(1399),b2(1399)/ 1370, 1371/
            data b1(1400),b2(1400)/ 1371, 1372/
            data b1(1401),b2(1401)/ 1372, 1373/
            data b1(1402),b2(1402)/ 1373, 1374/
            data b1(1403),b2(1403)/ 1374, 1375/
            data b1(1404),b2(1404)/ 1375, 1376/
            data b1(1405),b2(1405)/ 1376, 1377/
            data b1(1406),b2(1406)/ 1377, 1378/
            data b1(1407),b2(1407)/ 1286, 1378/
            data b1(1408),b2(1408)/ 1284, 1286/
            data b1(1409),b2(1409)/ 1351, 1379/
            data b1(1410),b2(1410)/ 1379, 1380/
            data b1(1411),b2(1411)/ 1380, 1381/
            data b1(1412),b2(1412)/ 1381, 1382/
            data b1(1413),b2(1413)/   81, 1382/
            data b1(1414),b2(1414)/ 1352, 1383/
            data b1(1415),b2(1415)/ 1383, 1384/
            data b1(1416),b2(1416)/ 1384, 1385/
            data b1(1417),b2(1417)/ 1385, 1386/
            data b1(1418),b2(1418)/ 1386, 1387/
            data b1(1419),b2(1419)/ 1387, 1388/
            data b1(1420),b2(1420)/ 1388, 1389/
            data b1(1421),b2(1421)/ 1389, 1390/
            data b1(1422),b2(1422)/ 1390, 1391/
            data b1(1423),b2(1423)/ 1391, 1392/
            data b1(1424),b2(1424)/ 1355, 1392/
            data b1(1425),b2(1425)/ 1355, 1393/
            data b1(1426),b2(1426)/ 1393, 1394/
            data b1(1427),b2(1427)/ 1394, 1395/
            data b1(1428),b2(1428)/ 1395, 1396/
            data b1(1429),b2(1429)/ 1396, 1397/
            data b1(1430),b2(1430)/ 1397, 1398/
            data b1(1431),b2(1431)/ 1398, 1399/
            data b1(1432),b2(1432)/ 1399, 1400/
            data b1(1433),b2(1433)/ 1400, 1401/
            data b1(1434),b2(1434)/ 1401, 1402/
            data b1(1435),b2(1435)/ 1402, 1403/
            data b1(1436),b2(1436)/ 1330, 1403/
            data b1(1437),b2(1437)/  280, 1404/
            data b1(1438),b2(1438)/ 1404, 1405/
            data b1(1439),b2(1439)/ 1405, 1406/
            data b1(1440),b2(1440)/ 1406, 1407/
            data b1(1441),b2(1441)/ 1407, 1408/
            data b1(1442),b2(1442)/ 1408, 1409/
            data b1(1443),b2(1443)/ 1409, 1410/
            data b1(1444),b2(1444)/ 1410, 1411/
            data b1(1445),b2(1445)/ 1411, 1412/
            data b1(1446),b2(1446)/ 1412, 1413/
            data b1(1447),b2(1447)/ 1413, 1414/
            data b1(1448),b2(1448)/ 1414, 1415/
            data b1(1449),b2(1449)/ 1415, 1416/
            data b1(1450),b2(1450)/ 1416, 1417/
            data b1(1451),b2(1451)/ 1417, 1418/
            data b1(1452),b2(1452)/ 1418, 1419/
            data b1(1453),b2(1453)/ 1214, 1419/
            data b1(1454),b2(1454)/ 1354, 1420/
            data b1(1455),b2(1455)/ 1420, 1421/
            data b1(1456),b2(1456)/ 1421, 1422/
            data b1(1457),b2(1457)/ 1422, 1423/
            data b1(1458),b2(1458)/ 1423, 1424/
            data b1(1459),b2(1459)/ 1424, 1425/
            data b1(1460),b2(1460)/ 1359, 1425/
            data b1(1461),b2(1461)/ 1359, 1426/
            data b1(1462),b2(1462)/ 1426, 1427/
            data b1(1463),b2(1463)/ 1427, 1428/
            data b1(1464),b2(1464)/ 1428, 1429/
            data b1(1465),b2(1465)/ 1429, 1430/
            data b1(1466),b2(1466)/ 1430, 1431/
            data b1(1467),b2(1467)/ 1431, 1432/
            data b1(1468),b2(1468)/ 1432, 1433/
            data b1(1469),b2(1469)/ 1433, 1434/
            data b1(1470),b2(1470)/ 1434, 1435/
            data b1(1471),b2(1471)/ 1435, 1436/
            data b1(1472),b2(1472)/ 1436, 1437/
            data b1(1473),b2(1473)/ 1437, 1438/
            data b1(1474),b2(1474)/ 1438, 1439/
            data b1(1475),b2(1475)/ 1439, 1440/
            data b1(1476),b2(1476)/ 1440, 1441/
            data b1(1477),b2(1477)/ 1441, 1442/
            data b1(1478),b2(1478)/ 1442, 1443/
            data b1(1479),b2(1479)/ 1443, 1444/
            data b1(1480),b2(1480)/ 1444, 1445/
            data b1(1481),b2(1481)/ 1445, 1446/
            data b1(1482),b2(1482)/ 1446, 1447/
            data b1(1483),b2(1483)/ 1173, 1447/
            data b1(1484),b2(1484)/ 1173, 1448/
            data b1(1485),b2(1485)/ 1448, 1449/
            data b1(1486),b2(1486)/ 1449, 1450/
            data b1(1487),b2(1487)/ 1450, 1451/
            data b1(1488),b2(1488)/ 1451, 1452/
            data b1(1489),b2(1489)/ 1452, 1453/
            data b1(1490),b2(1490)/ 1453, 1454/
            data b1(1491),b2(1491)/ 1454, 1455/
            data b1(1492),b2(1492)/ 1455, 1456/
            data b1(1493),b2(1493)/ 1456, 1457/
            data b1(1494),b2(1494)/ 1457, 1458/
            data b1(1495),b2(1495)/ 1458, 1459/
            data b1(1496),b2(1496)/ 1459, 1460/
            data b1(1497),b2(1497)/ 1212, 1460/
            data b1(1498),b2(1498)/ 1411, 1461/
            data b1(1499),b2(1499)/ 1461, 1462/
            data b1(1500),b2(1500)/ 1462, 1463/
            data b1(1501),b2(1501)/ 1463, 1464/
            data b1(1502),b2(1502)/ 1464, 1465/
            data b1(1503),b2(1503)/ 1465, 1466/
            data b1(1504),b2(1504)/ 1466, 1467/
            data b1(1505),b2(1505)/ 1467, 1468/
            data b1(1506),b2(1506)/ 1468, 1469/
            data b1(1507),b2(1507)/ 1469, 1470/
            data b1(1508),b2(1508)/ 1470, 1471/
            data b1(1509),b2(1509)/ 1471, 1472/
            data b1(1510),b2(1510)/ 1472, 1473/
            data b1(1511),b2(1511)/ 1473, 1474/
            data b1(1512),b2(1512)/ 1474, 1475/
            data b1(1513),b2(1513)/ 1475, 1476/
            data b1(1514),b2(1514)/ 1476, 1477/
            data b1(1515),b2(1515)/ 1477, 1478/
            data b1(1516),b2(1516)/ 1478, 1479/
            data b1(1517),b2(1517)/ 1479, 1480/
            data b1(1518),b2(1518)/ 1480, 1481/
            data b1(1519),b2(1519)/ 1481, 1482/
            data b1(1520),b2(1520)/ 1482, 1483/
            data b1(1521),b2(1521)/ 1483, 1484/
            data b1(1522),b2(1522)/ 1484, 1485/
            data b1(1523),b2(1523)/ 1485, 1486/
            data b1(1524),b2(1524)/ 1486, 1487/
            data b1(1525),b2(1525)/ 1487, 1488/
            data b1(1526),b2(1526)/ 1488, 1489/
            data b1(1527),b2(1527)/ 1489, 1490/
            data b1(1528),b2(1528)/ 1490, 1491/
            data b1(1529),b2(1529)/ 1491, 1492/
            data b1(1530),b2(1530)/ 1492, 1493/
            data b1(1531),b2(1531)/ 1493, 1494/
            data b1(1532),b2(1532)/ 1494, 1495/
            data b1(1533),b2(1533)/ 1495, 1496/
            data b1(1534),b2(1534)/ 1496, 1497/
            data b1(1535),b2(1535)/ 1497, 1498/
            data b1(1536),b2(1536)/ 1498, 1499/
            data b1(1537),b2(1537)/ 1499, 1500/
            data b1(1538),b2(1538)/ 1500, 1501/
            data b1(1539),b2(1539)/ 1501, 1502/
            data b1(1540),b2(1540)/ 1502, 1503/
            data b1(1541),b2(1541)/ 1503, 1504/
            data b1(1542),b2(1542)/ 1504, 1505/
            data b1(1543),b2(1543)/ 1505, 1506/
            data b1(1544),b2(1544)/ 1506, 1507/
            data b1(1545),b2(1545)/ 1507, 1508/
            data b1(1546),b2(1546)/ 1508, 1509/
            data b1(1547),b2(1547)/ 1509, 1510/
            data b1(1548),b2(1548)/ 1510, 1511/
            data b1(1549),b2(1549)/ 1511, 1512/
            data b1(1550),b2(1550)/ 1512, 1513/
            data b1(1551),b2(1551)/ 1513, 1514/
            data b1(1552),b2(1552)/ 1514, 1515/
            data b1(1553),b2(1553)/ 1515, 1516/
            data b1(1554),b2(1554)/ 1516, 1517/
            data b1(1555),b2(1555)/ 1517, 1518/
            data b1(1556),b2(1556)/ 1518, 1519/
            data b1(1557),b2(1557)/ 1519, 1520/
            data b1(1558),b2(1558)/ 1520, 1521/
            data b1(1559),b2(1559)/ 1521, 1522/
            data b1(1560),b2(1560)/ 1522, 1523/
            data b1(1561),b2(1561)/ 1453, 1523/
            data b1(1562),b2(1562)/ 1421, 1524/
            data b1(1563),b2(1563)/ 1524, 1525/
            data b1(1564),b2(1564)/ 1525, 1526/
            data b1(1565),b2(1565)/ 1526, 1527/
            data b1(1566),b2(1566)/ 1527, 1528/
            data b1(1567),b2(1567)/ 1528, 1529/
            data b1(1568),b2(1568)/ 1529, 1530/
            data b1(1569),b2(1569)/ 1530, 1531/
            data b1(1570),b2(1570)/ 1531, 1532/
            data b1(1571),b2(1571)/ 1532, 1533/
            data b1(1572),b2(1572)/ 1533, 1534/
            data b1(1573),b2(1573)/ 1534, 1535/
            data b1(1574),b2(1574)/ 1535, 1536/
            data b1(1575),b2(1575)/ 1536, 1537/
            data b1(1576),b2(1576)/ 1537, 1538/
            data b1(1577),b2(1577)/   64, 1538/
            data b1(1578),b2(1578)/ 1539, 1540/
            data b1(1579),b2(1579)/ 1540, 1541/
            data b1(1580),b2(1580)/ 1541, 1542/
            data b1(1581),b2(1581)/ 1542, 1543/
            data b1(1582),b2(1582)/ 1543, 1544/
            data b1(1583),b2(1583)/ 1544, 1545/
            data b1(1584),b2(1584)/ 1545, 1546/
            data b1(1585),b2(1585)/ 1546, 1547/
            data b1(1586),b2(1586)/ 1547, 1548/
            data b1(1587),b2(1587)/ 1548, 1549/
            data b1(1588),b2(1588)/ 1549, 1550/
            data b1(1589),b2(1589)/ 1550, 1551/
            data b1(1590),b2(1590)/ 1551, 1552/
            data b1(1591),b2(1591)/ 1552, 1553/
            data b1(1592),b2(1592)/ 1553, 1554/
            data b1(1593),b2(1593)/ 1554, 1555/
            data b1(1594),b2(1594)/ 1555, 1556/
            data b1(1595),b2(1595)/ 1556, 1557/
            data b1(1596),b2(1596)/ 1557, 1558/
            data b1(1597),b2(1597)/ 1558, 1559/
            data b1(1598),b2(1598)/ 1559, 1560/
            data b1(1599),b2(1599)/ 1560, 1561/
            data b1(1600),b2(1600)/ 1561, 1562/
            data b1(1601),b2(1601)/ 1562, 1563/
            data b1(1602),b2(1602)/ 1404, 1563/
            data b1(1603),b2(1603)/ 1564, 1565/
            data b1(1604),b2(1604)/ 1565, 1566/
            data b1(1605),b2(1605)/ 1566, 1567/
            data b1(1606),b2(1606)/ 1567, 1568/
            data b1(1607),b2(1607)/ 1568, 1569/
            data b1(1608),b2(1608)/ 1569, 1570/
            data b1(1609),b2(1609)/ 1570, 1571/
            data b1(1610),b2(1610)/ 1571, 1572/
            data b1(1611),b2(1611)/ 1442, 1572/
            data b1(1612),b2(1612)/   55, 1573/
            data b1(1613),b2(1613)/ 1573, 1574/
            data b1(1614),b2(1614)/ 1574, 1575/
            data b1(1615),b2(1615)/ 1575, 1576/
            data b1(1616),b2(1616)/ 1576, 1577/
            data b1(1617),b2(1617)/ 1577, 1578/
            data b1(1618),b2(1618)/ 1578, 1579/
            data b1(1619),b2(1619)/ 1579, 1580/
            data b1(1620),b2(1620)/ 1580, 1581/
            data b1(1621),b2(1621)/ 1581, 1582/
            data b1(1622),b2(1622)/ 1582, 1583/
            data b1(1623),b2(1623)/ 1583, 1584/
            data b1(1624),b2(1624)/ 1584, 1585/
            data b1(1625),b2(1625)/ 1585, 1586/
            data b1(1626),b2(1626)/ 1586, 1587/
            data b1(1627),b2(1627)/ 1587, 1588/
            data b1(1628),b2(1628)/ 1588, 1589/
            data b1(1629),b2(1629)/ 1589, 1590/
            data b1(1630),b2(1630)/ 1590, 1591/
            data b1(1631),b2(1631)/ 1591, 1592/
            data b1(1632),b2(1632)/ 1592, 1593/
            data b1(1633),b2(1633)/ 1593, 1594/
            data b1(1634),b2(1634)/ 1594, 1595/
            data b1(1635),b2(1635)/ 1595, 1596/
            data b1(1636),b2(1636)/ 1534, 1596/
            data b1(1637),b2(1637)/ 1518, 1597/
            data b1(1638),b2(1638)/ 1597, 1598/
            data b1(1639),b2(1639)/ 1598, 1599/
            data b1(1640),b2(1640)/ 1599, 1600/
            data b1(1641),b2(1641)/ 1600, 1601/
            data b1(1642),b2(1642)/ 1564, 1601/
            data b1(1643),b2(1643)/ 1564, 1602/
            data b1(1644),b2(1644)/ 1602, 1603/
            data b1(1645),b2(1645)/ 1603, 1604/
            data b1(1646),b2(1646)/ 1604, 1605/
            data b1(1647),b2(1647)/ 1605, 1606/
            data b1(1648),b2(1648)/ 1606, 1607/
            data b1(1649),b2(1649)/ 1582, 1607/
            data b1(1650),b2(1650)/ 1608, 1609/
            data b1(1651),b2(1651)/ 1609, 1610/
            data b1(1652),b2(1652)/ 1610, 1611/
            data b1(1653),b2(1653)/ 1611, 1612/
            data b1(1654),b2(1654)/ 1612, 1613/
            data b1(1655),b2(1655)/ 1613, 1614/
            data b1(1656),b2(1656)/ 1614, 1615/
            data b1(1657),b2(1657)/ 1615, 1616/
            data b1(1658),b2(1658)/ 1616, 1617/
            data b1(1659),b2(1659)/ 1617, 1618/
            data b1(1660),b2(1660)/ 1618, 1619/
            data b1(1661),b2(1661)/ 1619, 1620/
            data b1(1662),b2(1662)/ 1620, 1621/
            data b1(1663),b2(1663)/ 1621, 1622/
            data b1(1664),b2(1664)/ 1622, 1623/
            data b1(1665),b2(1665)/ 1623, 1624/
            data b1(1666),b2(1666)/ 1624, 1625/
            data b1(1667),b2(1667)/ 1625, 1626/
            data b1(1668),b2(1668)/ 1626, 1627/
            data b1(1669),b2(1669)/ 1627, 1628/
            data b1(1670),b2(1670)/ 1628, 1629/
            data b1(1671),b2(1671)/ 1629, 1630/
            data b1(1672),b2(1672)/ 1630, 1631/
            data b1(1673),b2(1673)/ 1631, 1632/
            data b1(1674),b2(1674)/ 1632, 1633/
            data b1(1675),b2(1675)/ 1633, 1634/
            data b1(1676),b2(1676)/ 1634, 1635/
            data b1(1677),b2(1677)/ 1635, 1636/
            data b1(1678),b2(1678)/ 1636, 1637/
            data b1(1679),b2(1679)/ 1637, 1638/
            data b1(1680),b2(1680)/ 1638, 1639/
            data b1(1681),b2(1681)/ 1639, 1640/
            data b1(1682),b2(1682)/ 1640, 1641/
            data b1(1683),b2(1683)/ 1641, 1642/
            data b1(1684),b2(1684)/ 1642, 1643/
            data b1(1685),b2(1685)/ 1643, 1644/
            data b1(1686),b2(1686)/ 1644, 1645/
            data b1(1687),b2(1687)/ 1539, 1645/
            data b1(1688),b2(1688)/ 1539, 1646/
            data b1(1689),b2(1689)/ 1646, 1647/
            data b1(1690),b2(1690)/ 1647, 1648/
            data b1(1691),b2(1691)/ 1648, 1649/
            data b1(1692),b2(1692)/ 1649, 1650/
            data b1(1693),b2(1693)/ 1650, 1651/
            data b1(1694),b2(1694)/ 1651, 1652/
            data b1(1695),b2(1695)/ 1652, 1653/
            data b1(1696),b2(1696)/ 1653, 1654/
            data b1(1697),b2(1697)/ 1654, 1655/
            data b1(1698),b2(1698)/ 1655, 1656/
            data b1(1699),b2(1699)/ 1656, 1657/
            data b1(1700),b2(1700)/ 1657, 1658/
            data b1(1701),b2(1701)/ 1658, 1659/
            data b1(1702),b2(1702)/ 1659, 1660/
            data b1(1703),b2(1703)/ 1660, 1661/
            data b1(1704),b2(1704)/ 1661, 1662/
            data b1(1705),b2(1705)/ 1662, 1663/
            data b1(1706),b2(1706)/ 1663, 1664/
            data b1(1707),b2(1707)/ 1597, 1665/
            data b1(1708),b2(1708)/ 1665, 1666/
            data b1(1709),b2(1709)/ 1666, 1667/
            data b1(1710),b2(1710)/ 1667, 1668/
            data b1(1711),b2(1711)/ 1668, 1669/
            data b1(1712),b2(1712)/ 1669, 1670/
            data b1(1713),b2(1713)/ 1670, 1671/
            data b1(1714),b2(1714)/ 1671, 1672/
            data b1(1715),b2(1715)/ 1672, 1673/
            data b1(1716),b2(1716)/ 1673, 1674/
            data b1(1717),b2(1717)/ 1674, 1675/
            data b1(1718),b2(1718)/ 1675, 1676/
            data b1(1719),b2(1719)/ 1676, 1677/
            data b1(1720),b2(1720)/ 1677, 1678/
            data b1(1721),b2(1721)/ 1678, 1679/
            data b1(1722),b2(1722)/ 1679, 1680/
            data b1(1723),b2(1723)/ 1680, 1681/
            data b1(1724),b2(1724)/ 1681, 1682/
            data b1(1725),b2(1725)/ 1682, 1683/
            data b1(1726),b2(1726)/ 1683, 1684/
            data b1(1727),b2(1727)/ 1684, 1685/
            data b1(1728),b2(1728)/ 1685, 1686/
            data b1(1729),b2(1729)/ 1686, 1687/
            data b1(1730),b2(1730)/ 1687, 1688/
            data b1(1731),b2(1731)/ 1688, 1689/
            data b1(1732),b2(1732)/ 1689, 1690/
            data b1(1733),b2(1733)/ 1690, 1691/
            data b1(1734),b2(1734)/ 1691, 1692/
            data b1(1735),b2(1735)/ 1692, 1693/
            data b1(1736),b2(1736)/ 1693, 1694/
            data b1(1737),b2(1737)/ 1694, 1695/
            data b1(1738),b2(1738)/ 1695, 1696/
            data b1(1739),b2(1739)/ 1696, 1697/
            data b1(1740),b2(1740)/ 1697, 1698/
            data b1(1741),b2(1741)/ 1698, 1699/
            data b1(1742),b2(1742)/ 1699, 1700/
            data b1(1743),b2(1743)/ 1700, 1701/
            data b1(1744),b2(1744)/ 1701, 1702/
            data b1(1745),b2(1745)/ 1580, 1702/
            data b1(1746),b2(1746)/   24, 1703/
            data b1(1747),b2(1747)/ 1703, 1704/
            data b1(1748),b2(1748)/ 1704, 1705/
            data b1(1749),b2(1749)/ 1705, 1706/
            data b1(1750),b2(1750)/ 1706, 1707/
            data b1(1751),b2(1751)/ 1707, 1708/
            data b1(1752),b2(1752)/ 1708, 1709/
            data b1(1753),b2(1753)/ 1664, 1709/
            data b1(1754),b2(1754)/ 1664, 1710/
            data b1(1755),b2(1755)/ 1710, 1711/
            data b1(1756),b2(1756)/ 1711, 1712/
            data b1(1757),b2(1757)/ 1712, 1713/
            data b1(1758),b2(1758)/ 1713, 1714/
            data b1(1759),b2(1759)/ 1714, 1715/
            data b1(1760),b2(1760)/ 1715, 1716/
            data b1(1761),b2(1761)/ 1716, 1717/
            data b1(1762),b2(1762)/ 1717, 1718/
            data b1(1763),b2(1763)/ 1718, 1719/
            data b1(1764),b2(1764)/ 1719, 1720/
            data b1(1765),b2(1765)/ 1720, 1721/
            data b1(1766),b2(1766)/ 1721, 1722/
            data b1(1767),b2(1767)/ 1722, 1723/
            data b1(1768),b2(1768)/ 1723, 1724/
            data b1(1769),b2(1769)/ 1669, 1724/
            data b1(1770),b2(1770)/ 1725, 1726/
            data b1(1771),b2(1771)/ 1703, 1726/
            data b1(1772),b2(1772)/ 1653, 1727/
            data b1(1773),b2(1773)/ 1727, 1728/
            data b1(1774),b2(1774)/ 1728, 1729/
            data b1(1775),b2(1775)/ 1729, 1730/
            data b1(1776),b2(1776)/ 1730, 1731/
            data b1(1777),b2(1777)/ 1725, 1731/
            data b1(1778),b2(1778)/ 1608, 1725/
            data b1(1779),b2(1779)/ 1608, 1732/
            data b1(1780),b2(1780)/ 1732, 1733/
            data b1(1781),b2(1781)/ 1733, 1734/
            data b1(1782),b2(1782)/ 1734, 1735/
            data b1(1783),b2(1783)/ 1735, 1736/
            data b1(1784),b2(1784)/ 1736, 1737/
            data b1(1785),b2(1785)/  303, 1737/
            data b1(1786),b2(1786)/ 1738, 1739/
            data b1(1787),b2(1787)/ 1739, 1740/
            data b1(1788),b2(1788)/ 1740, 1741/
            data b1(1789),b2(1789)/ 1741, 1742/
            data b1(1790),b2(1790)/ 1742, 1743/
            data b1(1791),b2(1791)/ 1743, 1744/
            data b1(1792),b2(1792)/ 1744, 1745/
            data b1(1793),b2(1793)/ 1734, 1745/
            data b1(1794),b2(1794)/    1, 1746/
            data b1(1795),b2(1795)/ 1746, 1747/
            data b1(1796),b2(1796)/ 1747, 1748/
            data b1(1797),b2(1797)/ 1748, 1749/
            data b1(1798),b2(1798)/ 1749, 1750/
            data b1(1799),b2(1799)/ 1750, 1751/
            data b1(1800),b2(1800)/ 1751, 1752/
            data b1(1801),b2(1801)/ 1752, 1753/
            data b1(1802),b2(1802)/ 1753, 1754/
            data b1(1803),b2(1803)/ 1754, 1755/
            data b1(1804),b2(1804)/ 1755, 1756/
            data b1(1805),b2(1805)/ 1756, 1757/
            data b1(1806),b2(1806)/ 1757, 1758/
            data b1(1807),b2(1807)/ 1758, 1759/
            data b1(1808),b2(1808)/ 1759, 1760/
            data b1(1809),b2(1809)/ 1760, 1761/
            data b1(1810),b2(1810)/ 1761, 1762/
            data b1(1811),b2(1811)/ 1762, 1763/
            data b1(1812),b2(1812)/ 1763, 1764/
            data b1(1813),b2(1813)/ 1764, 1765/
            data b1(1814),b2(1814)/ 1765, 1766/
            data b1(1815),b2(1815)/ 1766, 1767/
            data b1(1816),b2(1816)/ 1767, 1768/
            data b1(1817),b2(1817)/ 1768, 1769/
            data b1(1818),b2(1818)/ 1769, 1770/
            data b1(1819),b2(1819)/ 1770, 1771/
            data b1(1820),b2(1820)/ 1771, 1772/
            data b1(1821),b2(1821)/ 1772, 1773/
            data b1(1822),b2(1822)/ 1773, 1774/
            data b1(1823),b2(1823)/ 1774, 1775/
            data b1(1824),b2(1824)/ 1775, 1776/
            data b1(1825),b2(1825)/ 1776, 1777/
            data b1(1826),b2(1826)/ 1777, 1778/
            data b1(1827),b2(1827)/ 1778, 1779/
            data b1(1828),b2(1828)/ 1779, 1780/
            data b1(1829),b2(1829)/ 1780, 1781/
            data b1(1830),b2(1830)/ 1781, 1782/
            data b1(1831),b2(1831)/ 1782, 1783/
            data b1(1832),b2(1832)/ 1783, 1784/
            data b1(1833),b2(1833)/ 1784, 1785/
            data b1(1834),b2(1834)/ 1785, 1786/
            data b1(1835),b2(1835)/ 1786, 1787/
            data b1(1836),b2(1836)/ 1787, 1788/
            data b1(1837),b2(1837)/ 1788, 1789/
            data b1(1838),b2(1838)/ 1789, 1790/
            data b1(1839),b2(1839)/ 1790, 1791/
            data b1(1840),b2(1840)/ 1791, 1792/
            data b1(1841),b2(1841)/ 1792, 1793/
            data b1(1842),b2(1842)/ 1793, 1794/
            data b1(1843),b2(1843)/ 1794, 1795/
            data b1(1844),b2(1844)/ 1726, 1795/
            data b1(1845),b2(1845)/ 1796, 1797/
            data b1(1846),b2(1846)/ 1797, 1798/
            data b1(1847),b2(1847)/ 1798, 1799/
            data b1(1848),b2(1848)/ 1799, 1800/
            data b1(1849),b2(1849)/ 1800, 1801/
            data b1(1850),b2(1850)/ 1801, 1802/
            data b1(1851),b2(1851)/ 1802, 1803/
            data b1(1852),b2(1852)/ 1803, 1804/
            data b1(1853),b2(1853)/ 1804, 1805/
            data b1(1854),b2(1854)/ 1805, 1806/
            data b1(1855),b2(1855)/ 1738, 1806/
            data b1(1856),b2(1856)/  478, 1807/
            data b1(1857),b2(1857)/ 1807, 1808/
            data b1(1858),b2(1858)/ 1808, 1809/
            data b1(1859),b2(1859)/ 1796, 1809/
            data b1(1860),b2(1860)/ 1773, 1796/
            data b1(1861),b2(1861)/ 1805, 1810/
            data b1(1862),b2(1862)/ 1810, 1811/
            data b1(1863),b2(1863)/ 1811, 1812/
            data b1(1864),b2(1864)/ 1812, 1813/
            data b1(1865),b2(1865)/ 1813, 1814/
            data b1(1866),b2(1866)/ 1814, 1815/
            data b1(1867),b2(1867)/  463, 1815/
            data b1(1868),b2(1868)/ 1738, 1816/
            data b1(1869),b2(1869)/ 1816, 1817/
            data b1(1870),b2(1870)/ 1817, 1818/
            data b1(1871),b2(1871)/ 1818, 1819/
            data b1(1872),b2(1872)/ 1819, 1820/
            data b1(1873),b2(1873)/ 1820, 1821/
            data b1(1874),b2(1874)/ 1821, 1822/
            data b1(1875),b2(1875)/ 1822, 1823/
            data b1(1876),b2(1876)/ 1823, 1824/
            data b1(1877),b2(1877)/ 1824, 1825/
            data b1(1878),b2(1878)/ 1825, 1826/
            data b1(1879),b2(1879)/ 1826, 1827/
            data b1(1880),b2(1880)/ 1827, 1828/
            data b1(1881),b2(1881)/ 1828, 1829/
            data b1(1882),b2(1882)/ 1829, 1830/
            data b1(1883),b2(1883)/ 1830, 1831/
            data b1(1884),b2(1884)/ 1831, 1832/
            data b1(1885),b2(1885)/ 1832, 1833/
            data b1(1886),b2(1886)/ 1833, 1834/
            data b1(1887),b2(1887)/ 1834, 1835/
            data b1(1888),b2(1888)/ 1835, 1836/
            data b1(1889),b2(1889)/ 1836, 1837/
            data b1(1890),b2(1890)/ 1837, 1838/
            data b1(1891),b2(1891)/ 1838, 1839/
            data b1(1892),b2(1892)/  342, 1839/
            data b1(1893),b2(1893)/ 1824, 1840/
            data b1(1894),b2(1894)/ 1840, 1841/
            data b1(1895),b2(1895)/ 1841, 1842/
            data b1(1896),b2(1896)/ 1842, 1843/
            data b1(1897),b2(1897)/ 1843, 1844/
            data b1(1898),b2(1898)/ 1844, 1845/
            data b1(1899),b2(1899)/ 1845, 1846/
            data b1(1900),b2(1900)/ 1846, 1847/
            data b1(1901),b2(1901)/ 1847, 1848/
            data b1(1902),b2(1902)/ 1848, 1849/
            data b1(1903),b2(1903)/ 1849, 1850/
            data b1(1904),b2(1904)/ 1850, 1851/
            data b1(1905),b2(1905)/ 1851, 1852/
            data b1(1906),b2(1906)/ 1852, 1853/
            data b1(1907),b2(1907)/ 1853, 1854/
            data b1(1908),b2(1908)/  445, 1855/
            data b1(1909),b2(1909)/ 1855, 1856/
            data b1(1910),b2(1910)/ 1856, 1857/
            data b1(1911),b2(1911)/ 1857, 1858/
            data b1(1912),b2(1912)/ 1858, 1859/
            data b1(1913),b2(1913)/ 1859, 1860/
            data b1(1914),b2(1914)/ 1860, 1861/
            data b1(1915),b2(1915)/ 1861, 1862/
            data b1(1916),b2(1916)/ 1823, 1862/
            data b1(1917),b2(1917)/  436, 1863/
            data b1(1918),b2(1918)/ 1863, 1864/
            data b1(1919),b2(1919)/ 1864, 1865/
            data b1(1920),b2(1920)/ 1849, 1865/
            data b1(1921),b2(1921)/  408, 1866/
            data b1(1922),b2(1922)/ 1866, 1867/
            data b1(1923),b2(1923)/ 1854, 1867/
            data b1(1924),b2(1924)/  367, 1854/
c
        sp(2)='california'
        sp(1)='california'
        sp(3)='california'
        sp(4)='california'
c
        do i=1,nvf
            vx(i)=x(i)
            vy(i)=y(i)
        enddo
        do i=1,nbf
            ibndry(1,i)=b1(i)
            ibndry(2,i)=b2(i)
            ibndry(3,i)=0
            ibndry(4,i)=2
            if(i.gt.nbound) ibndry(4,i)=0
            ibndry(5,i)=0
            ibndry(6,i)=0
        enddo
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
        return
        end
