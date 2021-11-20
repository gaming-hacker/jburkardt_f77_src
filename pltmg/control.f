c***********************  problem name: control  ***********************
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
c
        values(k0)=ux
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
            character*80
     +          su
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
c
        values(k0)=uy
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
            character*80
     +          su
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
c
        values(k0)=-rl
        values(kl)=-1.0d0
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
            common /atest2/ix,iy,ixl,iyl,ixu,iyu,iu(94),
     +          beta,dgamma,dshift,bdlw,bdup,cflw,cfup,ru(93),su(100)
c
        pi=3.141592653589793d0
        axl=pi*dfloat(ixl)*x
        ayl=pi*dfloat(iyl)*y
        axu=pi*dfloat(ixu)*x
        ayu=pi*dfloat(iyu)*y
c
        values(klb)=bdlw+cflw*dsin(axl)*dsin(ayl)
        values(kub)=bdup+cfup*dsin(axu)*dsin(ayu)
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
            common /atest2/ix,iy,ixl,iyl,ixu,iyu,iu(94),
     +          beta,dgamma,dshift,bdlw,bdup,cflw,cfup,ru(93),su(100)
c
        call uexact(x,y,itag,r,rx,ry,rxx,ryy,rxy)
        values(k0)=beta*(u-r)**2+dgamma*rl**2
        values(ku)=2.0d0*beta*(u-r)
        values(kl)=2.0d0*dgamma*rl
        values(kuu)=2.0d0*beta
        values(kll)=2.0d0*dgamma+dshift
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
        call uexact(x,y,itag,r,rx,ry,rxx,ryy,rxy)
        values(kf)=r
        values(kf1)=rx
        values(kf2)=ry
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
            common /atest2/ix,iy,ixl,iyl,ixu,iyu,iu(94),
     +          beta,dgamma,dshift,bdlw,bdup,cflw,cfup,ru(93),su(100)
c
        pi=3.141592653589793d0
        ax=dfloat(ix)*pi
        ay=dfloat(iy)*pi
        sx=dsin(ax*x)
        sy=dsin(ay*y)
        cx=dcos(ax*x)
        cy=dcos(ay*y)
        u=sx*sy
        ux=ax*cx*sy
        uy=ay*sx*cy
        uxx=-ax**2*u
        uyy=-ay**2*u
        uxy=ax*ay*cx*cy
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
     +          sp,su,file(30)
            common /atest1/ip(100),rp(100),sp(100)
            common /atest2/iu(100),ru(100),su(100)
            save len,file
c
            data len/19/
            data (file(i),i=  1, 10)/
     +      'n i= 1,n=ix,    a=ix,t=i',
     1      'n i= 2,n=iy,    a=iy,t=i',
     2      'n i= 3,n=ixl,   a=xl,t=i',
     3      'n i= 4,n=iyl,   a=yl,t=i',
     4      'n i= 5,n=ixu,   a=xu,t=i',
     5      'n i= 6,n=iyu,   a=yu,t=i',
     6      'n i= 1,n=beta,  a=b ,t=r',
     7      'n i= 2,n=gamma, a=g ,t=r',
     8      'n i= 3,n=dshift,a=d ,t=r',
     9      'n i= 4,n=bdlw,  a=bl,t=r'/
            data (file(i),i= 11, 13)/
     6      'n i= 5,n=bdup,  a=bu,t=r',
     7      'n i= 6,n=cflw,  a=cl,t=r',
     8      'n i= 7,n=cfup,  a=cu,t=r'/
c
c       enter input mode
c
        call usrset(file,len,iu,ru,su)
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
            data ntf,nvf,ncf,nbf,ispd/8,9,0,8,1/
c
c           common /atest2/ix,iy,ixl,iyl,ixu,iyu,iu(94),
c    +          beta,gamma,dshift,bdlw,bdup,cflw,cfup,ru(93),su(100)
c
c
        if(ip(41).eq.1) then
            sp(2)='control'
            sp(1)='control'
            sp(3)='control'
            sp(4)='control'
            sp(6)='control_mpixxx.rw'
            sp(7)='control.jnl'
            sp(9)='control_mpixxx.out'
            iu(1)=5
            iu(2)=5
            iu(3)=1
            iu(4)=1
            iu(5)=1
            iu(6)=1
            ru(1)=1.0d0
            ru(2)=1.0d-4
            ru(3)=0.0d0
            ru(4)=1.0d0
            ru(5)=10.0d0
            ru(6)=0.0d0
            ru(7)=0.0d0
        endif
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        ip(5)=1
        ip(6)=5
        ip(8)=ispd
        rp(3)=0.1d0
c**
        ip(26)=4
        ip(27)=4
c**
        do i=1,ntf
            itnode(1,i)=9
            itnode(2,i)=i
            itnode(3,i)=i-1
            itnode(4,i)=0
            itnode(5,i)=i
c
            ibndry(1,i)=i
            ibndry(2,i)=i-1
            ibndry(3,i)=0
            ibndry(4,i)=2
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
        return
        end
