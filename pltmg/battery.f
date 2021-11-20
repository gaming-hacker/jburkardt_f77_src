c***********************  problem name: battery ************************
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
     +          values(*),coeff(5)
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
            save coeff
            data coeff/25.0d0,7.0d0,5.0d0,0.2d0,0.05d0/
c
        values(k0)=coeff(itag)*ux
        values(kx)=coeff(itag)
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
     +          values(*),coeff(5)
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
            save coeff
            data coeff/25.0d0,0.8d0,0.0001d0,0.2d0,0.05d0/
c
c
        values(k0)=coeff(itag)*uy
        values(ky)=coeff(itag)
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
     +          values(*),coeff(5)
            common /val0/k0,ku,kx,ky,kl,kuu,kux,kuy,kul,
     +          kxu,kxx,kxy,kxl,kyu,kyx,kyy,kyl,klu,klx,kly,kll
            save coeff
            data coeff/0.0d0,-1.0d0,-1.0d0,0.0d0,0.0d0/
c
c
        values(k0)=coeff(itag)
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
     +          values(*),g(4),al(4)
            common /val1/k0,ku,kl,kuu,kul,klu,kll
            save g,al
            data g/0.0d0,1.0d0,2.0d0,3.0d0/
            data al/0.0d0,3.0d0,2.0d0,1.0d0/
c
        values(k0)=g(itag)-al(itag)*u
        values(ku)=-al(itag)
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
     +          itnode(5,*),ibndry(6,*),ip(100),iu(100),label(32),
     1          it1(32),it2(32)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*),
     1          x(5),y(9)
            character*80
     +          sp(100),su(100)
            save hmax,grade,ntf,nvf,ncf,nbf,nx,ny,label,it1,it2
c
c       thermal battery problem
c       data courtesy of l. demcowicz
c
            data nx,ny/5,9/
            data x/0.0d0,6.1d0,6.5d0,8.0d0,8.4d0/
            data y/0.0d0,0.8d0,1.6d0,3.6d0,12.0d0,
     +          18.8d0,21.2d0,23.2d0,24.0d0/
            data ntf,nvf,ncf,nbf/32,45,0,76/
            data hmax,grade/0.1d0,1.5d0/
            data label/2,3,3,2,5,5,1,1,1,1,4,5,4,4,4,4,
     +          1,5,5,5,5,5,5,1,1,1,1,1,1,1,1,1/
            data it1/16,16,21,26,31,11,36,6,2,42,7,37,12,17,22,27,
     +          8,8,13,18,23,28,33,38,4,44,9,14,19,24,29,34/
            data it2/11,44,45,46,47,6,48,1,50,37,51,32,52,53,54,55,
     +          3,60,61,62,63,64,65,66,68,39,69,70,71,72,73,74/

c
c                      1 1 1 1    8 10 24 28
c                      5 5 5 1    6 12 23 32
c                      2 4 5 1    1 16 22 31
c                      3 4 5 1    2 15 21 30
c                      3 4 5 1    3 14 20 29
c                      2 4 5 1    4 13 19 28
c                      5 4 5 1    5 11 18 27
c                      1 1 1 1    7  9 17 25
c
        if(ip(41).eq.1) then
            sp(2)='battery'
            sp(1)='battery'
            sp(3)='battery'
            sp(4)='battery'
            sp(6)='battery_mpixxx.rw'
            sp(7)='battery.jnl'
            sp(9)='battery_mpixxx.out'
        endif
c
        nvf=0
        do j=1,ny
            do i=1,nx
                nvf=nvf+1
                vx(nvf)=x(i)
                vy(nvf)=y(j)
            enddo
        enddo
c
        nbf=0
        do j=1,ny-1
            do i=1,nx
                nn=(j-1)*nx+i
                nbf=nbf+1
                ibndry(1,nbf)=nn
                ibndry(2,nbf)=nn+nx
                ibndry(3,nbf)=0
                ibndry(4,nbf)=0
                if(i.eq.1.or.i.eq.nx) ibndry(4,nbf)=1
                ibndry(5,nbf)=0
                ibndry(6,nbf)=0
                if(i.eq.1) ibndry(6,nbf)=1
                if(i.eq.nx) ibndry(6,nbf)=3
            enddo
        enddo
c
        do i=1,nx-1
            do j=1,ny
                nn=(j-1)*nx+i
                nbf=nbf+1
                ibndry(1,nbf)=nn
                ibndry(2,nbf)=nn+1
                ibndry(3,nbf)=0
                ibndry(4,nbf)=0
                if(j.eq.1.or.j.eq.ny) ibndry(4,nbf)=1
                ibndry(5,nbf)=0
                ibndry(6,nbf)=0
                if(j.eq.1) ibndry(6,nbf)=4
                if(j.eq.ny) ibndry(6,nbf)=2
            enddo
        enddo
c
        ip(1)=ntf
        ip(2)=nvf
        ip(3)=ncf
        ip(4)=nbf
        ip(26)=5
        rp(15)=hmax
        rp(16)=grade
c
c       make itnode (saved output as it1/it2 because
c       we get different ordering for single and double
c       and label depends on the ordering)
c
cc      call sklutl(0,vx,vy,xm,ym,itnode,ibndry,ip,w,iflag)
c
c       label regions
c
        do i=1,ntf
            itnode(1,i)=it1(i)
            itnode(2,i)=it2(i)
            itnode(5,i)=label(i)
        enddo
        return
        end








