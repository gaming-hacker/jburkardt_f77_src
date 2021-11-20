c***********************  problem name: mnsurf  ************************
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
            common /atest2/iu(100),ru(100),su(100)
c
        theta=ru(1)
c
        if(itag.eq.2.or.itag.eq.4) values(1)=4.0d0*x*(1.0d0-x)
        if (itag.eq.2) then
           values(klb)=1.0d0
        else if(itag.eq.3) then
            s=(1.0d0-theta)/2.0d0
            b=(x-s)/(0.25d0-s)
            values(klb)=2.0d0*b-1.0d0
        else if(itag.eq.4) then
            s=(1.0d0+theta)/2.0d0
            b=(y-s)/(0.75d0-s)
            values(klb)=2.0d0*b-1.0d0
        else if(itag.eq.5) then
            s=(1.0d0+theta)/2.0d0
            b=(x-s)/(0.75d0-s)
            values(klb)=2.0d0*b-1.0d0
        else if(itag.eq.6) then
            s=(1.0d0-theta)/2.0d0
            b=(y-s)/(0.25d0-s)
            values(klb)=2.0d0*b-1.0d0
        else
           values(klb)=-1.0d0
        endif
        values(kub)=1.5d0
        values(kic)=(values(klb)+values(kub))/2.0d0
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
            character*80
     +          su
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
            common /atest2/iu(100),ru(100),su(100)
c
        dshift=ru(2)
        ss=dsqrt(1.0d0+ux**2+uy**2)
        values(k0)=ss
        values(kx)=ux/ss
        values(ky)=uy/ss
        values(kxx)=(1.0d0+uy**2)/ss**3+dshift
        values(kxy)=-ux*uy/ss**3
        values(kyy)=(1.0d0+ux**2)/ss**3+dshift
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
            data len/2/
            data (file(i),i=  1, 2)/
     +      'n i= 1,n=theta, a=t ,t=r',
     1      'n i= 2,n=dshift,a=d ,t=r'/
c
c       enter input mode
c
        rsv=ru(1)
        call usrset(file,len,iu,ru,su)
        if(ru(1).ne.rsv) ip(41)=-1
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
     1          x(9),y(9)
            character*80
     +          sp(100),su(100)
            save x,y,ntf,nvf,ncf,nbf,ispd,iprob,ifirst,iadapt,
     +          hmax,grade
c
            data x/0.d0,0.d0,.5d0,1.d0,1.d0,1.d0,.5d0,0.d0,.5d0/
            data y/.5d0,1.d0,1.d0,1.d0,.5d0,0.d0,0.d0,0.d0,.5d0/
            data ntf,nvf,ncf,nbf,ispd/24,25,0,48,1/
            data iprob,ifirst,iadapt/2,1,5/
            data hmax,grade/0.1d0,1.5d0/
c
c           common /atest2/ix,iy,ixl,iyl,ixu,iyu,iu(94),
c    +          a1x,a1y,a1u,a2x,a2y,a2u,bux,buy,cu1,
c    1          bdlw,bdup,cflw,cfup,ru(87),su(100)
c
        if(ip(41).eq.1) then
            sp(1)='minimal surface'
            sp(2)='minimal surface'
            sp(3)='minimal surface'
            sp(4)='minimal surface'
            sp(6)='mnsurf.rw'
            sp(7)='mnsurf.jnl'
            sp(9)='mnsurf.out'
c
            ru(1)=0.55d0
            ru(2)=1.0d-2
        endif
c
        rp(3)=1.0d-1
c
        do i=1,9
            vx(i)=x(i)
            vy(i)=y(i)
        enddo
        theta=ru(1)
        do i=1,8
            vx(9+i)=(x(i)+x(9))/2.0d0
            vy(9+i)=(y(i)+y(9))/2.0d0
            vx(17+i)=theta*x(i)+(1.0d0-theta)*x(9)
            vy(17+i)=theta*y(i)+(1.0d0-theta)*y(9)
        enddo
        do i=1,nbf
            do j=1,6
                ibndry(j,i)=0
            enddo
        enddo
        do i=1,8
            ibndry(1,i)=i
            ibndry(2,i)=i-1
            ibndry(4,i)=2
            ibndry(6,i)=(i+1)/2
c
            ibndry(1,i+8)=i
            ibndry(2,i+8)=i+17
            ibndry(1,i+16)=i+17
            ibndry(2,i+16)=i+9
            ibndry(1,i+24)=i+9
            ibndry(2,i+24)=9
            ibndry(1,i+32)=i+17
            ibndry(2,i+32)=i+16
            ibndry(1,i+40)=i+9
            ibndry(2,i+40)=i+8
        enddo
        ibndry(2,1)=8
        ibndry(2,33)=25
        ibndry(2,41)=17
c
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        ip(5)=ifirst
        ip(6)=iprob
        ip(8)=ispd
        rp(15)=hmax
        rp(16)=grade
        ip(26)=iadapt
c
c       make itnode, find symmetries
c
        call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
        call sklutl(2,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
c
        do i=1,ntf
           itnode(5,i)=1
        enddo
        do i=9,16
           itnode(5,i)=2
        enddo
        itnode(5,5)=3
        itnode(5,6)=3
        itnode(5,7)=4
        itnode(5,18)=4
        itnode(5,21)=5
        itnode(5,22)=5
        itnode(5,8)=6
        itnode(5,20)=6
c
        return
        end
