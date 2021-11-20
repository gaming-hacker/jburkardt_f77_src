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
            include "mpif.h"
            common /atest6/nproc,myid,mpisw
c
c       initialize
c
        if(isw.eq.1) then
            call MPI_INIT(ierr)
            call MPI_COMM_RANK( MPI_COMM_WORLD, id, ierr)
            call MPI_COMM_SIZE( MPI_COMM_WORLD, np, ierr)
            nproc=np
            myid=id
            if(nproc.le.1) then
                mpisw=-1
            else
                mpisw=1
            endif
        else
c
c       quit
c
            call MPI_FINALIZE(ierr)
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
            include "mpif.h"
            character*6
     +          cmdtyp
            character*80
     +          list,cmd,cmd0
            common /atest3/mode,jnlsw,jnlr,jnlw,ibatch
            common /atest4/jcmd,cmdtyp,list
            common /atest6/nproc,myid,mpisw
            save mpijnl,lowera,lowerz
            data mpijnl/0/
            data lowera,lowerz/97,122/
c
c       master
c
        if(myid.eq.0) then
            cmd0=cmd
            if(mode.eq.jnlsw) mpijnl=0
            if(mpijnl.eq.1.and.cmdtyp.ne.'mpicmd') return
            if(mpisw.lt.0) then
                if(cmdtyp.eq.'popup '.or.cmdtyp.eq.'file  '
     +              .or.cmdtyp.eq.'journl') then
                    ii=ichar(cmd0(1:1))
                    if(ii.ge.lowera.and.ii.le.lowerz) then
                        cmd0(1:1)=char(ii-32)
                    endif
                endif
            else
                if(cmdtyp.eq.'journl') mpijnl=1
            endif
            call MPI_BCAST(cmd0, 80, MPI_CHARACTER,
     +          0,MPI_COMM_WORLD,ierr)
        else
c
c       slave
c
            if(jnlsw.eq.1.and.cmdtyp.ne.'mpicmd') return
            call MPI_BCAST(cmd, 80, MPI_CHARACTER,
     +          0,MPI_COMM_WORLD,ierr)
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
        subroutine expth(ip,ipath,ipath0,counts,offset)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            include "mpif.h"
            integer
     +          ipath(4,*),ipath0(4,*),ip(100),counts(*),offset(*),
     1          id(3),jd(3)
            common /atest6/nproc,myid,mpisw
c
c       condense data structure
c
        maxpth=ip(81)
        ishift=nproc+1
        ipath(1,1)=ipath(1,nproc+2)
        ipath(2,1)=ipath(2,nproc+2)
        ipath(3,1)=ipath(3,nproc+2)-ishift
        ipath(4,1)=ipath(4,nproc+2)-ishift
        do j=ipath(3,1),ipath(4,1)
            do k=1,4
                ipath(k,j)=ipath(k,j+ishift)
            enddo
            if(ipath(4,j).gt.0) ipath(4,j)=ipath(4,j)-ishift
        enddo
        len=ipath(4,1)*4
c
        call MPI_ALLGATHER(len, 1, MPI_INTEGER, counts, 1,
     +      MPI_INTEGER, MPI_COMM_WORLD, ierr)
c
        offset(1)=0
        do irgn=1,nproc-1
            offset(irgn+1)=offset(irgn)+counts(irgn)
        enddo
        isum=(offset(nproc)+counts(nproc))/4+nproc+2
        if(isum.gt.maxpth) then
            ip(25)=72
            return
        endif
c
        call MPI_ALLGATHERV(ipath, len, MPI_INTEGER,
     +      ipath0, counts,offset,MPI_INTEGER,
     1      MPI_COMM_WORLD, ierr)
c
        ipath(1,nproc+2)=0
        ipath(2,nproc+2)=0
        ipath(3,nproc+2)=nproc+3
        ipath(4,nproc+2)=nproc+2
        do irgn=1,nproc
            ii=offset(irgn)/4
c
            ipath(1,nproc+2)=max0(ipath(1,nproc+2),ipath0(1,1+ii))
c
            n0=ipath(2,nproc+2)
            ipath(1,irgn)=n0+1
            ipath(2,irgn)=n0+ipath0(2,1+ii)
            ipath(2,nproc+2)=ipath(2,irgn)
c
            ipath(3,irgn)=ipath(4,nproc+2)+1
            jb0=ipath(3,irgn)-ipath0(3,1+ii)
            ipath(4,irgn)=ipath0(4,1+ii)+jb0
            ipath(4,nproc+2)=ipath(4,irgn)
c
            do j=ipath(3,irgn),ipath(4,irgn)
                do k=1,4
                    ipath(k,j)=ipath0(k,j-jb0+ii)
                enddo
                if(ipath(4,j).gt.0) then
                    ipath(4,j)=ipath(4,j)+jb0
                endif
                ipath(1,j)=ipath(1,j)+n0
                ipath(2,j)=ipath(2,j)+n0
            enddo
        enddo
c
        ip(71)=ipath(2,nproc+2)
        ip(72)=ipath(4,nproc+2)
c
c       global dimensions
c
        id(1)=ip(31)
        id(2)=ip(32)
        id(3)=ip(33)
        call MPI_ALLREDUCE( id, jd, 3, MPI_INTEGER,
     +      MPI_SUM,MPI_COMM_WORLD,ierr)
        ip(38)=jd(1)
        ip(39)=jd(2)
        ip(40)=jd(3)
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
            include "mpif.h"
            integer
     +          ipath(4,*),counts(*),offset(*)
            double precision
     +          vin(*),vout(*),gf(nn,*)
            common /atest6/nproc,myid,mpisw
c
c       communicate interface data
c
        do irgn=1,nproc
            ii=ipath(1,irgn)-1
            nvdd=ipath(2,irgn)-ii
            counts(irgn)=num*nvdd
            offset(irgn)=num*ii
        enddo
c
        irgn=myid+1
        ii=ipath(1,irgn)-1
        nvdd=ipath(2,irgn)-ii
        len=counts(irgn)
        do j=1,num
            jj=(j-1)*nvdd
            do i=1,nvdd
                vin(i+jj)=gf(i+ii,j)
            enddo
        enddo
c
        call MPI_ALLGATHERV( vin, len, MPI_DOUBLE_PRECISION,
     +      vout, counts,offset,MPI_DOUBLE_PRECISION,
     1      MPI_COMM_WORLD, ierr)
c
        do irgn=1,nproc
            ii=ipath(1,irgn)-1
            nvdd=ipath(2,irgn)-ii
            do j=1,num
                jj=(j-1)*nvdd+offset(irgn)
                 do i=1,nvdd
                     gf(i+ii,j)=vout(i+jj)
                 enddo
             enddo
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
        subroutine pl2ip(t,num)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            include "mpif.h"
            double precision
     +          t(*),temp(100)
            common /atest6/nproc,myid,mpisw
c
c       compute all inner products/norms
c
        do i=1,num
            temp(i)=t(i)
        enddo
c
        call MPI_ALLREDUCE( temp, t, num, MPI_DOUBLE_PRECISION,
     +      MPI_SUM,MPI_COMM_WORLD,ierr)
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
            include "mpif.h"
            integer
     +          ibndry(6,*),itnode(5,*),ip(100),iu(100),itemp(20)
            double precision
     +          vx(*),vy(*),xm(*),ym(*),rp(100),ru(100),w(*)
            character*8
     +          keywrd
            character*80
     +          sp(100),su(100)
            save keywrd
            data keywrd/'rwdouble'/
            common /atest6/nproc,myid,mpisw
c
c       inital fan-out of coarse grid data
c
        if(myid.eq.0) then
            itemp( 1)=ip(1)
            itemp( 2)=ip(2)
            itemp( 3)=ip(3)
            itemp( 4)=ip(4)
            itemp( 5)=ip(20)
            itemp( 6)=ip(22)
            itemp( 7)=ip(77)
            itemp( 8)=ip(92)
            itemp( 9)=ip(93)
            itemp(10)=ip(94)
            itemp(11)=ip(96)
            itemp(12)=ip(99)
        endif
        call MPI_BCAST(itemp, 12,  MPI_INTEGER,
     +      0,MPI_COMM_WORLD,ierr)
c
c       check buffer storage
c
        iint=4
        if(keywrd(3:3).eq.'d') then
            iflt=8
        else
            iflt=4
        endif
        ntf=itemp(1)
        nvf=itemp(2)
        ncf=itemp(3)
        nbf=itemp(4)
        lenw=itemp(5)
        maxv=itemp(6)
        ngf=itemp(7)
        jtime=itemp(8)
        jhist=itemp(9)
        jpath=itemp(10)
        jstat=itemp(11)
        iz=itemp(12)
        lenz=(lenw-iz)*iflt
        leni=(200+5*ntf+6*nbf)*iint
        lenr=(200+2*ncf+(ngf+2)*nvf+606+660+150+10*nproc)*iflt
        lens=200*80
        if(leni+lenr+lens+16.gt.lenz) then
            sp(11)='bcast: insufficient storge'
            return
        else
            sp(11)='bcast: ok'
        endif
        ip(73)=0
        ip(74)=0
c
c       master node
c
        if(myid.eq.0) then
c
c       integers
c
            len=0
            call MPI_PACK(ip,100,MPI_INTEGER,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(iu,100,MPI_INTEGER,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(itnode,5*ntf,MPI_INTEGER,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(ibndry,6*nbf,MPI_INTEGER,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
c
c       floats
c
            call MPI_PACK(rp,100,MPI_DOUBLE_PRECISION,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(ru,100,MPI_DOUBLE_PRECISION,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            if(ncf.gt.0) then
                call MPI_PACK(xm,ncf,MPI_DOUBLE_PRECISION,
     +              w(iz),lenz,len,MPI_COMM_WORLD,ierr)
                call MPI_PACK(ym,ncf,MPI_DOUBLE_PRECISION,
     +              w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            endif
            call MPI_PACK(vx,nvf,MPI_DOUBLE_PRECISION,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(vy,nvf,MPI_DOUBLE_PRECISION,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            do k=1,ngf
                iptr=1+(k-1)*maxv
                call MPI_PACK(w(iptr),nvf,MPI_DOUBLE_PRECISION,
     +              w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            enddo
            call MPI_PACK(w(jpath),606,MPI_DOUBLE_PRECISION,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(w(jhist),660,MPI_DOUBLE_PRECISION,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(w(jtime),150,MPI_DOUBLE_PRECISION,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(w(jstat),10*nproc,MPI_DOUBLE_PRECISION,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
c
c       characters
c
            call MPI_PACK(sp,8000,MPI_CHARACTER,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(su,8000,MPI_CHARACTER,
     +          w(iz),lenz,len,MPI_COMM_WORLD,ierr)
        endif
c
c       send
c
        call MPI_BCAST(len, 1, MPI_INTEGER,
     +      0,MPI_COMM_WORLD,ierr)
        call MPI_BCAST(w(iz), len, MPI_PACKED,
     +      0,MPI_COMM_WORLD,ierr)
c
c       slave node
c
        if(myid.ne.0) then
c
c       integers
c
            lenz=len
            len=0
c
            call MPI_UNPACK(w(iz),lenz,len,
     +          ip, 100, MPI_INTEGER,
     1          MPI_COMM_WORLD,ierr)
            ip(48)=mpisw
            ip(49)=nproc
            ip(50)=myid+1
            call MPI_UNPACK(w(iz),lenz,len,
     +          iu, 100, MPI_INTEGER,
     1          MPI_COMM_WORLD,ierr)
            call MPI_UNPACK(w(iz),lenz,len,
     +          itnode, 5*ntf, MPI_INTEGER,
     1          MPI_COMM_WORLD,ierr)
            call MPI_UNPACK(w(iz),lenz,len,
     +          ibndry, 6*nbf, MPI_INTEGER,
     1          MPI_COMM_WORLD,ierr)
c
c       floats
c
            call MPI_UNPACK(w(iz),lenz,len,
     +          rp, 100, MPI_DOUBLE_PRECISION,
     1          MPI_COMM_WORLD,ierr)
            call MPI_UNPACK(w(iz),lenz,len,
     +          ru, 100, MPI_DOUBLE_PRECISION,
     1          MPI_COMM_WORLD,ierr)
            if(ncf.gt.0) then
                call MPI_UNPACK(w(iz),lenz,len,
     +              xm, ncf, MPI_DOUBLE_PRECISION,
     1              MPI_COMM_WORLD,ierr)
                call MPI_UNPACK(w(iz),lenz,len,
     +              ym, ncf, MPI_DOUBLE_PRECISION,
     1              MPI_COMM_WORLD,ierr)
            endif
            call MPI_UNPACK(w(iz),lenz,len,
     +          vx, nvf, MPI_DOUBLE_PRECISION,
     1          MPI_COMM_WORLD,ierr)
            call MPI_UNPACK(w(iz),lenz,len,
     +          vy, nvf, MPI_DOUBLE_PRECISION,
     1          MPI_COMM_WORLD,ierr)
            do k=1,ngf
                iptr=1+(k-1)*maxv
                call MPI_UNPACK(w(iz),lenz,len,
     +              w(iptr), nvf, MPI_DOUBLE_PRECISION,
     1              MPI_COMM_WORLD,ierr)
            enddo
            call MPI_UNPACK(w(iz),lenz,len,
     +          w(jpath), 606, MPI_DOUBLE_PRECISION,
     1          MPI_COMM_WORLD,ierr)
            call MPI_UNPACK(w(iz),lenz,len,
     +          w(jhist), 660, MPI_DOUBLE_PRECISION,
     1          MPI_COMM_WORLD,ierr)
            call MPI_UNPACK(w(iz),lenz,len,
     +          w(jtime), 150, MPI_DOUBLE_PRECISION,
     1          MPI_COMM_WORLD,ierr)
            call MPI_UNPACK(w(iz),lenz,len,
     +          w(jstat), 10*nproc , MPI_DOUBLE_PRECISION,
     1          MPI_COMM_WORLD,ierr)
c
c       characters
c
            call MPI_UNPACK(w(iz),lenz,len,
     +          sp, 8000, MPI_CHARACTER,
     1          MPI_COMM_WORLD,ierr)
            call MPI_UNPACK(w(iz),lenz,len,
     +          su, 8000, MPI_CHARACTER,
     1          MPI_COMM_WORLD,ierr)
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
        subroutine collct(vx,vy,ibndry,itnode,ip,sp,w,buff)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            include "mpif.h"
            integer
     +          ibndry(6,*),itnode(5,*),ip(100),ib(4),
     1          status(MPI_STATUS_SIZE)
            double precision
     +          vx(*),vy(*),w(*)
            character*1
     +          buff(*)
            character*8
     +          keywrd
            character*80
     +          sp(100)
            save keywrd
            data keywrd/'rwdouble'/
            common /atest6/nproc,myid,mpisw
c
c       collect global solution on master process
c
        iint=4
        if(keywrd(3:3).eq.'d') then
            iflt=8
        else
            iflt=4
        endif
        lenw=ip(20)
        maxt=ip(21)
        maxv=ip(22)
        maxb=ip(24)
        ngf=ip(77)
        iz=ip(99)
c
c       this is a really conservative storage estimate!
c
        lenz=(lenw-iz)*iflt
        leni=(4+5*maxt+6*maxb)*iint
        lenr=((ngf+2)*maxv+maxt)*iflt
        if(leni+lenr.gt.lenz) then
            sp(11)='collct: insufficient buffer storge'
            return
        else
            sp(11)='collct: ok'
        endif
        maxt=ip(21)
        maxv=ip(22)
        maxb=ip(24)
        itag=7
c
        if(myid.eq.0) then
            itest=0
            ngf=ip(77)
            maxv=ip(22)
            iee=ip(97)
c
c       integers
c
            do i=1,nproc-1
                ntf=ip(1)
                nvf=ip(2)
                nbf=ip(4)
c
                call MPI_RECV(len, 1, MPI_INTEGER,
     +              MPI_ANY_SOURCE, itag, MPI_COMM_WORLD,
     1              status, ierr)
                idsrc=status(MPI_SOURCE)
                call MPI_RECV(buff, len, MPI_PACKED,
     +              idsrc,itag, MPI_COMM_WORLD,
     1              status, ierr)
c
                lenz=len
                len=0
                call MPI_UNPACK(buff,lenz,len,
     +              ib, 4, MPI_INTEGER,
     1              MPI_COMM_WORLD,ierr)
c
                ntf0=ib(1)
                nvf0=ib(2)
                nbf0=ib(3)
                idbc=ib(4)
c
c       check storage (must do all mpi stuff anyway)
c
                if(ntf+ntf0.gt.maxt) then
                    sp(11)='collct: insufficient maxt storage'
                    itest=1
                else if(nbf+nbf0.gt.maxb) then
                    sp(11)='collct: insufficient maxb storage'
                    itest=1
                else if(nvf+nvf0.gt.maxv) then
                    sp(11)='collct: insufficient maxv storage'
                    itest=1
                endif
c
                if(itest.eq.0) then
                    call MPI_UNPACK(buff,lenz,len,
     +                  itnode(1,ntf+1),5*ntf0, MPI_INTEGER,
     1                  MPI_COMM_WORLD,ierr)
                    call MPI_UNPACK(buff,lenz,len,
     +                  ibndry(1,nbf+1),6*nbf0, MPI_INTEGER,
     1                  MPI_COMM_WORLD,ierr)
c
c       update itnode and ibndry
c
                    do k=ntf+1,ntf+ntf0
                        do j=1,3
                            itnode(j,k)=itnode(j,k)+nvf
                        enddo
                    enddo
                    do k=nbf+1,nbf+nbf0
                        do j=1,2
                            ibndry(j,k)=ibndry(j,k)+nvf
                        enddo
                        if(ibndry(4,k).lt.0) then
                            ibndry(4,k)=ibndry(4,k)-nbf
                        endif
                    enddo
c
c       floats
c
                    call MPI_UNPACK(buff,lenz,len,
     +                  vx(nvf+1),nvf0,MPI_DOUBLE_PRECISION,
     1                  MPI_COMM_WORLD,ierr)
                    call MPI_UNPACK(buff,lenz,len,
     +                  vy(nvf+1),nvf0,MPI_DOUBLE_PRECISION,
     1                  MPI_COMM_WORLD,ierr)
                    do k=1,ngf
                        iptr=1+(k-1)*maxv+nvf
                        call MPI_UNPACK(buff,lenz,len,
     +                      w(iptr),nvf0,MPI_DOUBLE_PRECISION,
     1                      MPI_COMM_WORLD,ierr)
                    enddo
                    call MPI_UNPACK(buff,lenz,len,
     +                  w(iee+ntf),ntf0,MPI_DOUBLE_PRECISION,
     1                  MPI_COMM_WORLD,ierr)
c
                    ip(1)=ntf+ntf0
                    ip(2)=nvf+nvf0
                    ip(4)=nbf+nbf0
                    if(idbc.ne.0) ip(7)=nvf+idbc
                endif
            enddo
        else
c
c       slave node
c
            ntf=ip(1)
            nvf=ip(2)
            nbf=ip(4)
            idbc=ip(7)
            ngf=ip(77)
            maxv=ip(22)
            iee=ip(97)
c
c       integers
c
            ib(1)=ntf
            ib(2)=nvf
            ib(3)=nbf
            ib(4)=idbc
            len=0
            call MPI_PACK(ib,4,MPI_INTEGER,
     +          buff,lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(itnode,5*ntf,MPI_INTEGER,
     +          buff,lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(ibndry,6*nbf,MPI_INTEGER,
     +          buff,lenz,len,MPI_COMM_WORLD,ierr)
c
c       floats
c
            call MPI_PACK(vx,nvf,MPI_DOUBLE_PRECISION,
     +          buff,lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(vy,nvf,MPI_DOUBLE_PRECISION,
     +          buff,lenz,len,MPI_COMM_WORLD,ierr)
            do k=1,ngf
                iptr=1+(k-1)*maxv
                call MPI_PACK(w(iptr),nvf,MPI_DOUBLE_PRECISION,
     +              buff,lenz,len,MPI_COMM_WORLD,ierr)
            enddo
            call MPI_PACK(w(iee),ntf,MPI_DOUBLE_PRECISION,
     +          buff,lenz,len,MPI_COMM_WORLD,ierr)
c
            call MPI_SEND(len, 1, MPI_INTEGER,
     +          0, itag, MPI_COMM_WORLD, ierr)
            call MPI_SEND(buff, len, MPI_PACKED,
     +          0, itag, MPI_COMM_WORLD, ierr)
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
        subroutine exstat(pstat,buf)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            include "mpif.h"
            double precision
     +          pstat(10,*),buf(*),sendb(2)
            common /atest6/nproc,myid,mpisw
c
c       compute all inner products/norms
c
        sendb(1)=pstat(3,myid+1)
        sendb(2)=pstat(4,myid+1)
c
        call MPI_ALLGATHER( sendb, 2, MPI_DOUBLE_PRECISION,
     +      buf, 2, MPI_DOUBLE_PRECISION,
     1      MPI_COMM_WORLD, ierr)
c
        do i=1,nproc
            ii=2*i-1
            pstat(3,i)=buf(ii)
            pstat(4,i)=buf(ii+1)
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
        subroutine exbox(bmin,bmax,num)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            include "mpif.h"
            double precision
     +          temp(5),bmin(*),bmax(*)
            common /atest6/nproc,myid,mpisw
c
c       compute the global bounding box
c
        do i=1,num
            temp(i)=bmin(i)
        enddo
        call MPI_ALLREDUCE( temp, bmin, num, MPI_DOUBLE_PRECISION,
     +      MPI_MIN ,MPI_COMM_WORLD, ierr)
c
        do i=1,num
            temp(i)=bmax(i)
        enddo
        call MPI_ALLREDUCE( temp, bmax, num, MPI_DOUBLE_PRECISION,
     +      MPI_MAX ,MPI_COMM_WORLD, ierr)
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
        subroutine exdist(kdist,num)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            include "mpif.h"
            integer
     +          kdist(*),itemp(22)
            common /atest6/nproc,myid,mpisw
c
c       compute all inner products/norms
c
        do i=1,num
            itemp(i)=kdist(i)
        enddo
c
        call MPI_ALLREDUCE( itemp, kdist, num, MPI_INTEGER,
     +      MPI_SUM, MPI_COMM_WORLD, ierr)
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
            include "mpif.h"
            integer
     +          num(4),itemp(4)
            double precision
     +          temp(2),val(2)
            common /atest6/nproc,myid,mpisw
c
c       compute global mesh quality statistics (binit)
c
        do i=1,4
            itemp(i)=num(i)
        enddo
        call MPI_ALLREDUCE( itemp, num, 4, MPI_INTEGER,
     +      MPI_SUM ,MPI_COMM_WORLD, ierr)
c
        do i=1,2
            temp(i)=val(i)
        enddo
        call MPI_ALLREDUCE( temp, val, 1, MPI_DOUBLE_PRECISION,
     +      MPI_MIN ,MPI_COMM_WORLD, ierr)
        call MPI_ALLREDUCE( temp(2), val(2), 1, MPI_DOUBLE_PRECISION,
     +      MPI_SUM ,MPI_COMM_WORLD, ierr)
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
            include "mpif.h"
            integer
     +          itemp(5),ibmin(*),ibmax(*)
            common /atest6/nproc,myid,mpisw
c
c       compute min and max color (binit)
c
        do i=1,num
            itemp(i)=ibmin(i)
        enddo
        call MPI_ALLREDUCE( itemp, ibmin, num, MPI_INTEGER,
     +      MPI_MIN , MPI_COMM_WORLD, ierr)
c
        do i=1,num
            itemp(i)=ibmax(i)
        enddo
        call MPI_ALLREDUCE( itemp, ibmax, num, MPI_INTEGER,
     +      MPI_MAX , MPI_COMM_WORLD, ierr)
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
            include "mpif.h"
            common /atest6/nproc,myid,mpisw
c
c       global dimensions
c
        itemp=iflag
        call MPI_ALLREDUCE( itemp, iflag, 1, MPI_INTEGER,
     +      MPI_MAX,MPI_COMM_WORLD,ierr)
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
            include "mpif.h"
            common /atest6/nproc,myid,mpisw
c
c       compute the maximum step (tpickd)
c
        temp=step
        call MPI_ALLREDUCE( temp, step, 1, MPI_DOUBLE_PRECISION,
     +      MPI_MIN ,MPI_COMM_WORLD, ierr)
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
     +          time(3,*),ptime(*),temp(3,50),atime(3,*),sendb(2)
            include "mpif.h"
            save len
            data len/50/
            common /atest6/nproc,myid,mpisw
c
c     exchange times
c
        sum=0.0d0
        do i=1,len
           sum=sum+time(2,i)
           temp(1,i)=time(1,i)
           temp(2,i)=time(2,i)
           temp(3,i)=time(3,i)
        enddo
c
        sendb(1)=sum
        call MPI_ALLGATHER( sendb, 1, MPI_DOUBLE_PRECISION,
     +      ptime, 1, MPI_DOUBLE_PRECISION,
     1      MPI_COMM_WORLD, ierr)
c
        num=3*len
        call MPI_ALLREDUCE( temp, atime, num, MPI_DOUBLE_PRECISION,
     +      MPI_SUM,MPI_COMM_WORLD,ierr)
c
        do i=1,len
            atime(1,i)=atime(1,i)/dfloat(nproc)
            atime(2,i)=atime(2,i)/dfloat(nproc)
            atime(3,i)=atime(3,i)/dfloat(nproc)
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
        subroutine exsze(jd,isw)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            include "mpif.h"
            integer
     +          id(3),jd(3)
            common /atest6/nproc,myid,mpisw
c
c       global dimensions
c
        id(1)=jd(1)
        id(2)=jd(2)
        id(3)=jd(3)
        if(isw.eq.0) then
            call MPI_ALLREDUCE( id, jd, 3, MPI_INTEGER,
     +          MPI_MAX,MPI_COMM_WORLD,ierr)
        else
            call MPI_ALLREDUCE( id, jd, 3, MPI_INTEGER,
     +          MPI_SUM,MPI_COMM_WORLD,ierr)
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
        subroutine glbpix(vx,vy,ibndry,itnode,icolor,ut,vt,jp,buff,isw)
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
            include "mpif.h"
            integer
     +          ibndry(6,*),itnode(5,*),jp(25),ib(3),icolor(*),
     1          status(MPI_STATUS_SIZE)
            double precision
     +          vx(*),vy(*),ut(3,*),vt(3,*)
            character*1
     +          buff(*)
            character*8
     +          keywrd
            save keywrd
            data keywrd/'grdouble'/
            common /atest6/nproc,myid,mpisw
c
c       collect global graph on master process
c
        itag=4
c
        if(myid.eq.0) then
c
c       integers
c
            do i=1,nproc-1
                ntf=jp(1)
                nvf=jp(2)
                nbf=jp(3)
c
                call MPI_RECV(len, 1, MPI_INTEGER,
     +              MPI_ANY_SOURCE, itag, MPI_COMM_WORLD,
     1              status, ierr)
                idsrc=status(MPI_SOURCE)
                call MPI_RECV(buff, len, MPI_PACKED,
     +              idsrc,itag, MPI_COMM_WORLD,
     1              status, ierr)
c
                lenz=len
                len=0
                call MPI_UNPACK(buff,lenz,len,
     +              ib, 3, MPI_INTEGER,
     1              MPI_COMM_WORLD,ierr)
c
                ntf0=ib(1)
                nvf0=ib(2)
                nbf0=ib(3)
c
                call MPI_UNPACK(buff,lenz,len,
     +              itnode(1,ntf+1),5*ntf0, MPI_INTEGER,
     1              MPI_COMM_WORLD,ierr)
                call MPI_UNPACK(buff,lenz,len,
     +              ibndry(1,nbf+1),6*nbf0, MPI_INTEGER,
     1              MPI_COMM_WORLD,ierr)
                if(isw.eq.0) then
                    call MPI_UNPACK(buff,lenz,len,
     +                  icolor(ntf+1),ntf0, MPI_INTEGER,
     1                  MPI_COMM_WORLD,ierr)
                endif
c
c       update itnode and ibndry
c
                do k=ntf+1,ntf+ntf0
                    do j=1,3
                        itnode(j,k)=itnode(j,k)+nvf
                    enddo
                enddo
                do k=nbf+1,nbf+nbf0
                    do j=1,2
                        ibndry(j,k)=ibndry(j,k)+nvf
                    enddo
                    if(ibndry(4,k).lt.0) then
                        ibndry(4,k)=ibndry(4,k)-nbf
                    endif
                enddo
c
c       floats
c
                call MPI_UNPACK(buff,lenz,len,
     +              vx(nvf+1),nvf0,MPI_DOUBLE_PRECISION,
     1              MPI_COMM_WORLD,ierr)
                call MPI_UNPACK(buff,lenz,len,
     +              vy(nvf+1),nvf0,MPI_DOUBLE_PRECISION,
     1              MPI_COMM_WORLD,ierr)
                if(isw.ne.0) then
                    call MPI_UNPACK(buff,lenz,len,
     +                  ut(1,ntf+1),3*ntf0,MPI_DOUBLE_PRECISION,
     1                  MPI_COMM_WORLD,ierr)
                    call MPI_UNPACK(buff,lenz,len,
     +                  vt(1,ntf+1),3*ntf0,MPI_DOUBLE_PRECISION,
     1                  MPI_COMM_WORLD,ierr)
                endif
c
                jp(1)=ntf+ntf0
                jp(2)=nvf+nvf0
                jp(3)=nbf+nbf0
            enddo
        else
c
c       slave node
c
            iint=4
            if(keywrd(3:3).eq.'d') then
                iflt=8
            else
                iflt=4
            endif
c
c       integers
c
            ntf=jp(1)
            nvf=jp(2)
            nbf=jp(3)
c
            lenz=(5*ntf+6*nbf+3)*iint+2*nvf*iflt
            if(isw.eq.0) lenz=lenz+ntf*iint
            if(isw.ne.0) lenz=lenz+ntf*iflt*6
c
c
            ib(1)=ntf
            ib(2)=nvf
            ib(3)=nbf
            len=0
            call MPI_PACK(ib,3,MPI_INTEGER,
     +          buff,lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(itnode,5*ntf,MPI_INTEGER,
     +          buff,lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(ibndry,6*nbf,MPI_INTEGER,
     +          buff,lenz,len,MPI_COMM_WORLD,ierr)
            if(isw.eq.0) then
                call MPI_PACK(icolor,ntf,MPI_INTEGER,
     +              buff,lenz,len,MPI_COMM_WORLD,ierr)
            endif
c
c       floats
c
            call MPI_PACK(vx,nvf,MPI_DOUBLE_PRECISION,
     +          buff,lenz,len,MPI_COMM_WORLD,ierr)
            call MPI_PACK(vy,nvf,MPI_DOUBLE_PRECISION,
     +          buff,lenz,len,MPI_COMM_WORLD,ierr)
            if(isw.ne.0) then
                call MPI_PACK(ut,3*ntf,MPI_DOUBLE_PRECISION,
     +              buff,lenz,len,MPI_COMM_WORLD,ierr)
                call MPI_PACK(vt,3*ntf,MPI_DOUBLE_PRECISION,
     +              buff,lenz,len,MPI_COMM_WORLD,ierr)
            endif
c
            call MPI_SEND(len, 1, MPI_INTEGER,
     +          0, itag, MPI_COMM_WORLD, ierr)
            call MPI_SEND(buff, len, MPI_PACKED,
     +          0, itag, MPI_COMM_WORLD, ierr)
        endif
c
        return
        end
