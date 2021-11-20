c*************************** file: atest.f *****************************
c-----------------------------------------------------------------------
c
c             piecewise linear triangle multi grid package
c
c                    edition 9.0 - - - march, 2004
c
c-----------------------------------------------------------------------
        program atest
c
c       storage allocation
c
            implicit double precision (a-h,o-z)
            implicit integer (i-n)
c
            parameter (maxv=150000,maxt=2*maxv,maxc=500,maxb=maxv/5,
     +          maxja=30*maxv,maxa=2*maxja,lenw=80*maxv)
c
            integer
     +          itnode(5,maxt),ibndry(6,maxb),ja(maxja)
            double precision
     +          w(lenw),vx(maxv),vy(maxv),xm(maxc),ym(maxc),a(maxa)
            character*80
     +          sp,su
c
            common /atest1/ip(100),rp(100),sp(100)
            common /atest2/iu(100),ru(100),su(100)
            common /atest5/idevce
            common /atest6/nproc,myid,mpisw
c
            external a1xy,a2xy,fxy,gnxy,gdxy,p1xy,p2xy,qxy
c
c       mode  =  1    run in batch mode
c             =  0    use x-windows interface
c             = -1    use terminal window interface
c             = -2    mpi slave node interface
c
        call menu(ip,rp,sp)
        mode=0
        if(myid.ne.0) mode=-2
        ngraph=0
c
c       initialize the iu, ru and su arrays
c
        do i=1,100
            iu(i)=0
            ru(i)=0.0d0
            su(i)=' '
        enddo
c
c       storage parameters
c
        ip(18)=maxja
        ip(19)=maxa
        ip(20)=lenw
        ip(21)=maxt
        ip(22)=maxv
        ip(23)=maxc
        ip(24)=maxb
c
c       parameters for atest
c
        ip(41)=1
        ip(42)=mode
        ip(43)=ngraph
        ip(48)=mpisw
        ip(49)=nproc
        ip(50)=myid+1
        sp(21)='localhost'
c
c       initialize input arrays
c
   30   call gdata(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,iu,ru,su,w)
        call stor(ip)
        ip(41)=0
        if(ip(5).eq.0) ip(5)=1
        call dschek(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,w)
c
c       get command (some commands change mpi parameters in ip array)
c
   50   ip(48)=mpisw
        ip(49)=nproc
        ip(50)=myid+1
        call menu(ip,rp,sp)
c
c       equation solution
c
        if(sp(12)(1:6).eq.'pltmg ') then
            call pltmg(vx,vy,xm,ym,itnode,ibndry,ja,a,ip,rp,sp,w,
     +          a1xy,a2xy,fxy,gnxy,gdxy,p1xy,p2xy)
c
c       mesh generation
c
        else if(sp(12)(1:6).eq.'trigen') then
            call trigen(vx,vy,xm,ym,itnode,ibndry,ja,a,ip,rp,sp,
     +          iu,ru,su,w,qxy)
c
c       plot function
c
        else if(sp(12)(1:6).eq.'triplt') then
            idevce=ip(44)
            call triplt(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,w,qxy)
c
c       graph output data
c
        else if(sp(12)(1:6).eq.'gphplt') then
            idevce=ip(45)
            call gphplt(ip,rp,sp,w)
c
c       plot input data
c
        else if(sp(12)(1:6).eq.'inplt ') then
            idevce=ip(46)
            call inplt(vx,vy,xm,ym,itnode,ibndry,ip,rp,sp,w)
c
c       plot matrix
c
        else if(sp(12)(1:6).eq.'mtxplt') then
            idevce=ip(47)
            call mtxplt(ja,a,ip,rp,sp,vx,vy,w)
c
c       read file
c
        else if(sp(12)(1:6).eq.'read  ') then
            call rdwrt(sp(6),1,vx,vy,xm,ym,
     +          ibndry,itnode,ja,a,ip,rp,sp,iu,ru,su,w)
c
c       write file
c
        else if(sp(12)(1:6).eq.'write ') then
            call rdwrt(sp(6),0,vx,vy,xm,ym,
     +          ibndry,itnode,ja,a,ip,rp,sp,iu,ru,su,w)
c
c       user supplied command
c
        else if(sp(12)(1:6).eq.'usrcmd') then
            sp(11)='usrcmd: ok'
            call usrcmd
            if(ip(41).ne.0) go to 30
c
c       shell
c
        else if(sp(12)(1:6).eq.'shell ') then
            call cshex(sp(5))
c
c       quit
c
        else if(sp(12)(1:6).eq.'quit  ') then
            stop
        endif
        go to 50
c
        end
