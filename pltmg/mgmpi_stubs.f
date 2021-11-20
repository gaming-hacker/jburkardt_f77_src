c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine mpiutl(isw)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            common /atest6/nproc,myid,mpisw
c
c       initialize
c
        if(isw.eq.1) then
            nproc=1 
            myid=0 
            mpisw=-1
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
        subroutine star0(cmd)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            character*80
     +          cmd
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
        subroutine expth(ip,ipath,ipath0,counts,offset)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          ipath(4,*),ipath0(4,*),ip(100),counts(*),offset(*) 
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
        subroutine exbdy(ipath,gf,nn,num,vin,vout,counts,offset)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          ipath(4,*),counts(*),offset(*)
            double precision
     +          vin(*),vout(*),gf(nn,*)
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine pl2ip(t,num)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            double precision
     +          t(*)
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine bcast(vx,vy,xm,ym,ibndry,itnode,ip,rp,sp,iu,ru,su,w)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          ibndry(6,*),itnode(5,*),ip(100),iu(100)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*)
            character*80
     +          sp(100),su(100)
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
        subroutine collct(vx,vy,ibndry,itnode,ip,sp,w,buff)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          ibndry(6,*),itnode(5,*),ip(100) 
            double precision
     +          vx(*),vy(*),w(*)
            character*1
     +          buff(*)
            character*80
     +          sp(100)
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
        subroutine exstat(pstat,buf)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            double precision
     +          pstat(10,*),buf(*)
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine exbox(bmin,bmax,num)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            double precision
     +          bmin(*),bmax(*)
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine exdist(kdist,num)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          kdist(*)
        return
        end
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        subroutine exqual(num,val)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          num(4)
            double precision
     +          val(2)
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
        subroutine exibox(ibmin,ibmax,num)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          ibmin(*),ibmax(*)
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
        subroutine exflag(iflag)
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
        subroutine exstep(step)
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
        subroutine extim(time,atime,ptime)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            double precision
     +          time(3,*),ptime(*),atime(3,*)
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
        subroutine exsze(jd,isw)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          jd(3)
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
        subroutine glbpix(vx,vy,ibndry,itnode,icolor,ut,vt,jp,buff,isw)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            integer
     +          ibndry(6,*),itnode(5,*),jp(25),icolor(*) 
            double precision
     +          vx(*),vy(*),ut(3,*),vt(3,*)
            character*1
     +          buff(*)
c
        return
        end

