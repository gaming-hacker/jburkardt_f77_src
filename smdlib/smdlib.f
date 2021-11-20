!  smdlib.f  17 July 1997
!
      subroutine alpha1(lalpha,lchar)
!
!*********************************************************************72
!
!! ALPHA1 sets the mix alphabet set.
!
!  (level 1-3, p/s)
!
!  input:   lalpha = character set
!  lchar  = font change indicator
!
      character*1 czalfl
      character*5 czalfn
      character*5 czalfs
      integer kzbegn
      integer kzlevl
      character*(*) lalpha
      character*(*) lchar
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
 
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfl(1)=lchar(1:1)
        czalfn(1)=lalpha(1:5)
        call capchr(czalfn(1))
      else
        call errmes('ALPHA1',1,3)
      end if
 
      return
      end
      subroutine alpha2(lalpha,lchar)
!
!*********************************************************************72
!
!! ALPHA2 sets the mix alphabet set.
!  (level 1-3, p/s)
!
!  input:   lalpha = character set
!  lchar  = font change indicator
!
      character*1 czalfl
      character*5 czalfn
      character*5 czalfs
      integer kzbegn
      integer kzlevl
      character*(*) lalpha
      character*(*) lchar
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
 
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfl(2)=lchar(1:1)
        czalfn(2)=lalpha(1:5)
        call capchr(czalfn(2))
        if(czalfl(1).eq.' ') then
          call defalf('STAND')
        end if
      else
        call errmes('ALPHA2',1,3)
      end if
 
      return
      end
      subroutine alpha3(lalpha,lchar)
!
!*********************************************************************72
!
!! ALPHA3 sets the mix alphabet set.
!  (level 1-3, p/s)
!
!  input:   lalpha = character set
!  lchar  = font change indicator
!
      character*1 czalfl
      character*5 czalfn
      character*5 czalfs
      integer kzbegn
      integer kzlevl
      character*(*) lalpha
      character*(*) lchar
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfl(3)=lchar(1:1)
        czalfn(3)=lalpha(1:5)
        call capchr(czalfn(3))
        if(czalfl(1).eq.' ') then
          call defalf('STAND')
        end if
      else
        call errmes('ALPHA3',1,3)
      end if
 
      return
      end
      subroutine alpha4(lalpha,lchar)
!
!*********************************************************************72
!
!! ALPHA4 sets the mix alphabet set.
!  (level 1-3, p/s)
!
!  input:   lalpha = character set
!  lchar  = font change indicator
!
      character*1 czalfl
      character*5 czalfn
      character*5 czalfs
      integer kzbegn
      integer kzlevl
      character*(*) lalpha
      character*(*) lchar
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfl(4)=lchar(1:1)
        czalfn(4)=lalpha(1:5)
        call capchr(czalfn(4))
        if(czalfl(1).eq.' ') then
          call defalf('STAND')
        end if
      else
        call errmes('ALPHA4',1,3)
      end if
 
      return
      end
      subroutine alpha5(lalpha,lchar)
!
!*********************************************************************72
!
!! ALPHA5 sets the mix alphabet set.
!  (level 1-3, p/s)
!
!  input:   lalpha = character set
!  lchar  = font change indicator
!
      character*1 czalfl
      character*5 czalfn
      character*5 czalfs
      integer kzbegn
      integer kzlevl
      character*(*) lalpha
      character*(*) lchar
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfl(5)=lchar(1:1)
        czalfn(5)=lalpha(1:5)
        call capchr(czalfn(5))
        if(czalfl(1).eq.' ') then
          call defalf('STAND')
        end if
      else
        call errmes('ALPHA5',1,3)
      end if
 
      return
      end
      subroutine alpha6(lalpha,lchar)
!
!*********************************************************************72
!
!! ALPHA6 sets the mix alphabet set.
!  (level 1-3, p/s)
!
!  input:   lalpha = character set
!  lchar  = font change indicator
!
      character*1 czalfl
      character*5 czalfn
      character*5 czalfs
      integer kzbegn
      integer kzlevl
      character*(*) lalpha
      character*(*) lchar
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfl(6)=lchar(1:1)
        czalfn(6)=lalpha(1:5)
        call capchr(czalfn(6))
        if(czalfl(1).eq.' ') then
          call defalf('STAND')
        end if
      else
        call errmes('ALPHA6',1,3)
      end if
 
      return
      end
      subroutine angle(ang)
!
!*********************************************************************72
!
!! ANGLE sets the text angle.
!  level 1-3, p/s
!
!  input:   ang = text angle
!
      real ang
      integer kzbegn
      integer kzlevl
      integer kzstrm
      integer kztmln
      real uuhite
      real zzangl
      real zzhite
!
      save /clevel/
      save /cstrng/
!
      common /clevel/ kzlevl,kzbegn
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
!
      if(kzlevl.eq.1)then
        zzangl=ang
        call gssetc(uuhite,zzangl)
      elseif(kzlevl.eq.2.or.kzlevl.eq.3)then
        zzangl=ang
        call gssetc(zzhite,zzangl)
      else
        call errmes('ANGLE',1,3)
      end if
 
      return
      end
      subroutine axes2d(xmin,xstp,xmax,ymin,ystp,ymax)
!
!*********************************************************************72
!
!! AXES2D sets up a linear coordinate system.
!  (level 2, raise to level 3
!
!  input:   
!  xmin,ymin = lower limits of x and y
!  xmax,ymax = upper limits of x and y
!  xstp,ystp = step size in x and y
!  if = 'scal', a step size is assigned
!
      integer numbyt
      parameter (numbyt=4)
!
      character*(numbyt) cxstr
      character*(numbyt) cystr
      integer iaxes
      integer kzbegn
      integer kzlevl
      integer kztiln
      integer kztitl
      integer kzxaln
      integer kzxlab
      integer kzxnfl
      integer kzyaln
      integer kzylab
      integer kzynfl
      logical logx
      logical logy
      real udx
      real udy
      real ux0
      real uy0
      real xmax
      real xmin
      real xstep
      real xstp
      real ymax
      real ymin
      real ystep
      real ystp
      real zzxmax
      real zzxmin
      real zzxstp
      real zzymax
      real zzymin
      real zzystp
!
      save /ccoord/
      save /clabel/
      save /clevel/
      save /pltcom/
!
      common /ccoord/ zzxmin,zzxmax,zzxstp,zzymin,zzymax,zzystp
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /clevel/ kzlevl,kzbegn
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
!
!  Initialize iaxes to define x and y axis and draw title.
!
      if(kzlevl.eq.2)then
        logx=.false.
        logy=.false.
        iaxes=19
        xstep=xstp
        ystep=ystp
        call win2ch(xstep,cxstr)
        call win2ch(ystep,cystr)
        call capchr(cxstr)
 
        if(cxstr(1:4).eq.'SCAL')then
          iaxes=iaxes+4
        else
          zzxstp=xstp
        end if
 
        call capchr(cystr)
 
        if(cystr(1:4).eq.'SCAL')then
          iaxes=iaxes+8
        else
          zzystp=ystp
        end if
 
        call zmapit(xmin,xmax,ymin,ymax,kzxlab,kzxaln,
     &    kzylab,kzyaln,0.0,0.0,iaxes)

        kzlevl=3
 
      else
 
        call errmes('AXES2D',2,0)
 
      end if
 
      return
      end
      subroutine axis(blow,bhigh,maxtks,lshort,lraggd,bmin,bmax,
     &   btmin,btmax,btick,ipwr)
!
!*********************************************************************72
!
!! AXIS is mainly for internal use.
!
!  Its purpose is to determine a suitable
!  "tick" distance over the range specified between
!  alow and ahigh.   it outputs the axis range bmin,bmax
!  and the tick distance btick stripped of their power of
!  ten.   the power of ten is returned in the var. ipwr.
!
      real fuzz
      real toocls
!
      parameter (fuzz=0.001)
      parameter (toocls=0.8)
!
      integer jticks(6)
      logical ldivds
      logical lisneg
      logical lraggd
      logical lshort
!
!  if a ragged axis is "too close" to the next tick, then extend it.
!  the "too close" parameter is the variable toocls
!
      save jticks
!
      data jticks /1,2,5,4,3,10/
!
      maxtks = max (1, maxtks)
      mintks = max (1, maxtks/2)
      bmax = bhigh
      bmin = blow
      lisneg = .false.

      if ( bmax .lt. bmin ) then
        bmax = blow
        bmin = bhigh
        lisneg = .true.
      end if
!
!  make sure we have enough range, if not, increase ahigh
!
!30    continue
 
      range = bmax - bmin
      temp = max ( abs(bmin), abs(bmax) )
      if (temp .eq. 0.0) temp = 10.0
 
      if (range/temp .lt. 5.0e-3) then
        bmin = bmin - 5.0e-3*temp
        bmax = bmax + 5.0e-3*temp
      end if
!
!  strip the range of its power of ten
!
      ipwr=int(alog10(bmax-bmin)-2)
      tenx = 10.0**ipwr
 
50    continue
 
      astrt = aint(bmin/tenx)
      afin = aint(bmax/tenx+0.999)
      if(afin*tenx .lt. bmax) afin = afin + 1
      range = afin - astrt
 
      if(range .gt. 10*maxtks) then
        ipwr = ipwr + 1
        tenx=tenx*10.0
        go to 50
      end if
!
!  search for a suitable tick
!
      btick = 0
 
      do i=1,6

        tick = jticks(i)
        ntick = int(range/tick+0.999)

        if (ntick .ge. mintks .and. ntick .le. maxtks) then
          if(ldivds(astrt,tick) .and. ldivds(afin,tick)) go to 150
          if(btick .eq. 0) btick = tick
        end if

      end do
!
!  use best non-perfect tick
!
      go to 160
!
!  found a good tick
!
150   continue

      btick=jticks(i)

160   continue
 
      if(btick .eq. 10.0) then
        btick = 1.0
        ipwr = ipwr + 1
        tenx = 10.0*tenx
      end if
 
165   continue

      tick = btick*tenx
!
!  figure out tick limits
!
      btmin = btick*aint(bmin/tick)
      if(btmin*tenx .lt. bmin) btmin = btmin + btick
      btmax = btick*aint(bmax/tick)
      if(btmax*tenx .gt. bmax) btmax = btmax - btick
      nintvl = int((btmax-btmin)/btick)
!
!  if user absolutely must have ragged axis, then force it.
!
      if(lshort .and. lraggd) go to 180
!
!  check individually
!
      if(lshort .and. (nintvl .gt. 0) .and.
     &   ((btmin-bmin/tenx)/btick .le. toocls) ) go to 170
        if((btmin-bmin/tenx) .gt. fuzz) btmin = btmin - btick
        bmin = btmin*tenx
170   continue

      if(lshort .and. (nintvl .gt. 0) .and.
     &   ((bmax/tenx-btmax)/btick .le. toocls) ) go to 180
        if((bmax/tenx-btmax) .gt. fuzz) btmax = btmax + btick
        bmax = btmax*tenx
180   continue

!
!  switch back to backwards
!
      if (lisneg) then
        btick = -btick
        temp = bmin
        bmin = bmax
        bmax = temp
        temp = btmin
        btmin = btmax
        btmax = temp
      end if 

!200   continue
 
      return
      end
      subroutine bargra(xlow,xhigh,nbar,nx,x,sxlab,sylab,
     &  stitle,itype)
!
!*********************************************************************72
!
!! BARGRA makes a bar graph from an array of real data.
!
!  BARGRA does not blank the screen before drawing.  Therefore,
!  if a previous plot has been made, the user should call
!
!  call gsdrvr(2,0.0,0.0)
!
!  to clear the screen first.
!
!  The MAPIT routine uses its own rules for the actual lowest and
!  highest values on the axes.  they always include the user's
!  values.  if you wish to move the bar graph away from the left
!  or right y axes do the following:
!
!  let s = (xh - xl) / nbar
!
!  where
!
!  xh = max x(i)
!
!  and
!
!  xl = min x(i)
!
!  now set
!
!  xlow = xl - n * s
!  xhigh = xh + m * s
!
!  where n and m are chosen at your discretion.
!
!
!
!  input, real xlow, xhigh, the lower and upper limits
!  for the horizontal axis.  it must be the case that
!  for each data value x(i), xlow <= x(i) <= xhigh.
!
!  input, integer nbar, the number of bars to draw.
!  nbar must be at least 1, and no more than 200.
!
!  input, integer nx, the number of data values in x.
!
!  input, real x(nx), the data from which the bar graph
!  is to be drawn.
!
!  input, character*(*) sxlab, the x axis label.
!
!  input, character*(*) sylab, the y axis label.
!
!  input, character*(*) stitle, the plot title.
!
!  input, integer itype, the axis flag.
!
!  0, x and y axes normal.
!  1, x logarithmic, y normal.
!  2, x normal, y logarithmic.
!  3, x and y logarithmic.
!  256, x and y normal, but axes are ragged.
!
      integer maxc
      parameter (maxc=200)
!
      real count(maxc)
      real dumx(3)
      real dumy(8)
      integer i
      integer itype
      integer j
      integer nx
      integer nbar
      character*(*) stitle
      character*(*) sxlab
      character*(*) sylab
      real vx0
      real vx1
      real vy0
      real vy1
      real x0
      real xhigh
      real xlow
      real x(nx)
      real y0
      real yhigh
      real ylow
 
      if(xlow.ge.xhigh)then
        write(*,*)' '
        write(*,*)'BARGRA - Fatal error!'
        write(*,*)'  XLOW is greater than or equal to XHIGH.'
        stop
      end if
 
      if(nbar.le.0)then
        write(*,*)' '
        write(*,*)'BARGRA - Fatal error!'
        write(*,*)'  Number of bars requested is too small.'
        write(*,*)'  Requested number = ',nbar
        write(*,*)'  but must be at least 1!'
        stop
      elseif(nbar.gt.maxc)then
        write(*,*)' '
        write(*,*)'BARGRA - Fatal error!'
        write(*,*)'  Number of bars requested is too large.'
        write(*,*)'  Requested number = ',nbar
        write(*,*)'  Maximum allowed=   ',maxc
        stop
      end if
 
      ylow=0.0
      yhigh=1.0
!
!  set the counters for each bar to zero, and then find where
!  each item of data falls.
!
      do i=1,nbar
        count(i)=0.0
      end do
 
      do i=1,nx
        j=1+int(nbar*(x(i)-xlow)/(xhigh-xlow))
        if(j.ge.1.and.j.le.nbar)then
          count(j)=count(j)+1.0
        end if
      end do
 
      call minmax(count,nbar,ylow,yhigh)

      ylow=0.0
      yhigh=yhigh+0.1*yhigh
!
!  the graph is to take up the whole screen.
!
      call mapsiz(0.0,100.0,0.0,100.0,0.0)
!
!  give the coordinate ranges and titles and axis type.
!
      call mapit(xlow,xhigh,ylow,yhigh,sxlab,sylab,stitle,itype)
 
      x0=xlow
      y0=0.0
      call scale(x0,y0,vx0,vy0)
      call gsmove(vx0,vy0)
 
      do i=1,nbar
 
        if(nbar.gt.1)then
          x0=((nbar-i)*xlow+(i-1)*xhigh)/real(nbar-1)
        else
          x0=0.5*(xlow+xhigh)
        end if
 
        y0=count(i)
        call scale(x0,y0,vx1,vy1)
        call gsdraw(vx0,vy1)
        call gsdraw(vx1,vy1)
        call gsdraw(vx1,vy0)
        vx0=vx1
 
      end do
 
      call gsdrvr(5,dumx,dumy)
 
      return
      end
      subroutine bargra2(xlow,xhigh,nbar,sbar,sxlab,sylab,
     &  stitle,itype)
!
!*********************************************************************72
!
!! BARGRA2 makes a bar graph from a set of binned data.
!
!  BARGRA2 does not blank the screen before drawing.  therefore,
!  if a previous plot has been made, the user should call
!
!    call gsdrvr(2,0.0,0.0)
!
!  to clear the screen first.
!
!
!  input, real xlow, xhigh, the data values associated
!  with the first and last bars.
!
!  input, integer nbar, the number of bars to draw.
!  nbar must be at least 1, and no more than 200.
!
!  input, real sbar(nbar), the height of the bars.
!
!  input, character*(*) sxlab, the x axis label.
!
!  input, character*(*) sylab, the y axis label.
!
!  input, character*(*) stitle, the plot title.
!
!  input, integer itype, the axis flag.
!
!  0, x and y axes normal.
!  1, x logarithmic, y normal.
!  2, x normal, y logarithmic.
!  3, x and y logarithmic.
!  256, x and y normal, but axes are ragged.
!
      integer nbar
!
      real dumx(3)
      real dumy(8)
      integer i
      integer itype
      real sbar(nbar)
      character*(*) stitle
      character*(*) sxlab
      character*(*) sylab
      real vx0
      real vx1
      real vy0
      real vy1
      real x0
      real xhigh
      real xlow
      real y0
      real yhigh
      real ylow
      real yspan
 
      if(nbar.le.0)then
        write(*,*)' '
        write(*,*)'BARGRA2 - Fatal error!'
        write(*,*)'  Number of bars requested is too small.'
        write(*,*)'  Requested number = ',nbar
        write(*,*)'  but must be at least 1!'
        stop
      end if
 
      call minmax(sbar,nbar,ylow,yhigh)
 
      if(ylow.gt.0.0)then
        ylow=0.0
      end if
 
      yspan=yhigh-ylow
 
      yhigh=yhigh+0.05*yspan
!
!  the graph is to take up the whole screen.
!
      call mapsiz(0.0,100.0,0.0,100.0,0.0)
!
!  give the coordinate ranges and titles and axis type.
!
      call mapit(xlow,xhigh,ylow,yhigh,sxlab,sylab,stitle,itype)
 
      x0=xlow
      y0=0.0
      call scale(x0,y0,vx0,vy0)
      call gsmove(vx0,vy0)
 
      do i=1,nbar
        if(nbar.gt.1)then
          x0=((nbar-i)*xlow+(i-1)*xhigh)/real(nbar-1)
        else
          x0=0.5*(xlow+xhigh)
        end if
        y0=sbar(i)
        call scale(x0,y0,vx1,vy1)
        call gsdraw(vx0,vy1)
        call gsdraw(vx1,vy1)
        call gsdraw(vx1,vy0)
        vx0=vx1
      end do
 
      call gsdrvr(5,dumx,dumy)
 
      return
      end
      subroutine bgnsub(ititle,ntitle,ixlab,nxlab,iylab,nylab,
     &                   ax,ay)
!
!*********************************************************************72
!
!! BGNSUB draws frame, sets subplot area and other system variables.
!  (level 1, raised to 2)
!
!  input:   ititle = title character string
!  ntitle = number of characters in ititle
!  ixlab  = x axis label
!  nxlab  = number of characters in ixlab
!  iylab  = y axis label
!  nylab  = number of characters in iylab
!  ax,ay  = distance of lower left corner of
!  subplot area from that of page
!
      parameter (kzyes=111)
      parameter (kzno=222)
      parameter (kzclip=12)
      parameter (kzdown=13)
      parameter (kzscrn=14)
      parameter (kzabrt=15)
      parameter (zzin=2.54)
      parameter (kqms=1200)
      parameter (kln03=3)
      parameter (kpost=910)
!
      integer ititle(*)
      integer ixlab(*)
      integer iylab(*)
!
      save /carea/
      save /cborch/
      save /ciount/
      save /clabel/
      save /clevel/
      save /cline/
      save /cpage/
      save /cphysr/
      save /cstrng/
      save /csymbo/
      save /cunit/
      save /gcclip/
      save /gcdchr/
!
      common /clevel/ kzlevl,kzbegn
      common /csymbo/ kzsym,kznsym,zzsmsz,uusmsz
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cborch/ kzbrdr,kzchek
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /ciount/ kziunt, kzount
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
!
 10   format(1x,3a)
!
!  check level
!
      if(kzlevl.eq.1)then
!
!  load axis labels and title
!
        if(nxlab.eq.0)then
          kzxaln=0
        else
          kzxaln=80
          call zcopys(ixlab,nxlab,kzxlab,kzxaln)
          if(kzxaln.lt.0)then
            kzxaln=-kzxaln
            call errmes('BGNSUB',80,6)
          end if
        end if
        if(nylab.eq.0)then
          kzyaln=0
        else
          kzyaln=80
          call zcopys(iylab,nylab,kzylab,kzyaln)
          if(kzyaln.lt.0)then
            kzyaln=-kzyaln
            call errmes('BGNSUB',80,7)
          end if
        end if
        if(ntitle.eq.0)then
          kztiln=0
        else
          kztiln=80
          call zcopys(ititle,ntitle,kztitl,kztiln)
          if(kztiln.lt.0)then
            kztiln=-kztiln
            call errmes('BGNSUB',80,8)
          end if
        end if
!
!  Scale QMS
!
        kdev=int(devid)
        if((kdev.eq.kqms.or.kdev.eq.kln03.or.kdev.eq.kpost)
     &                .and.kzauto.eq.kzyes)then
          if(uupagx.lt.uupagy)then
            call zlasap
          else
            call zlasal
          end if
        end if
!
!  erase screen
!
        if(kzbegn.ne.kzyes)then
!  call gsdrvr(2,dummy,dummy)
          kzbegn=kzyes
        end if
!
!  convert to system unit
!
        rx=xlencm/uupagx
        ry=ylencm/uupagy
        if(rx.lt.ry)then
          zzpagr=rx
        else
          zzpagr=ry
        end if
        if(kzscal.eq.kzdown)then
          if(zzpagr.gt.1.0)then
            zzpagr=1.0
          end if
          zzpagx=uupagx*zzpagr
          zzpagy=uupagy*zzpagr
        elseif(kzscal.eq.kzscrn)then
          zzpagx=uupagx*zzpagr
          zzpagy=uupagy*zzpagr
        elseif(kzscal.eq.kzclip)then
          zzpagx=uupagx
          zzpagy=uupagy
          zzpagr=1.0
        elseif(kzscal.eq.kzabrt)then
          if(rx.lt.1.0.or.ry.lt.1.0)then
            kzlevl=1
            return
          else
            zzpagx=uupagx
            zzpagy=uupagy
            zzpagr=1.0
          end if
        end if
 
        if(kzlthk.eq.kzyes) then
          zzlthk=uulthk*zzpagr
        end if
 
        zzsmsz=uusmsz*zzpagr
        zzgrce=uugrce*zzpagr
        zzhite=uuhite*zzpagr
        call gssetc(zzhite,zzangl)
        zzsmsz=uusmsz*zzpagr
        uuyaxs=ay*zzunit
        uuxaxs=ax*zzunit
        zzxaxs=uuxaxs*zzpagr
        zzyaxs=uuyaxs*zzpagr
        zzfrme=uufrme*zzpagr
!
!  check if ORIGIN is called
!
        if(kzor.ne.kzyes)then
          dx=uupagx-uuxaxs
          dy=uupagy-uuyaxs
          if(dx.gt.1.0)then
            uuxor=dx/2.0
          else
            uuxor=0.5*zzin
            if(dx.lt.0.5)then
            call errmes('BGNSUB',0,4)
            end if
          end if
          if(dy.gt.1.0)then
            uuyor=dy/2.0
          else
            uuyor=0.5*zzin
            if(dy.lt.0.5)then
            call errmes('BGNSUB',0,5)
            end if
          end if
        end if
!
!  Calculate frame
!
        zzxor=uuxor*zzpagr
        zzyor=uuyor*zzpagr
        pxmin=(xlencm-zzpagx)/2.0
        pymin=(ylencm-zzpagy)/2.0
        pxmax=pxmin+zzpagx
        pymax=pymin+zzpagy
        xcm0=pxmin
        xcm1=pxmax
        ycm0=pymin
        ycm1=pymax
!
!  check if NOBORD is called
!
        if(kzbrdr.ne.kzno)then
          call dsmove(pxmin,pymin)
          call dsdraw(pxmin,pymax)
          call dsdraw(pxmax,pymax)
          call dsdraw(pxmax,pymin)
          call dsdraw(pxmin,pymin)
        end if
!
!  set plot area parameters
!
        zzxlft=pxmin+zzxor
        zzxaxr=(pxmax-zzxlft)/zzxaxs
        if(zzxaxr.gt.1.0)then
          zzxaxr=1.0
          zzxrgt=zzxlft+zzxaxs
        else
          zzxrgt=pxmax
        end if

        zzybot=pymin+zzyor
        zzyaxr=(pymax-zzybot)/zzyaxs

        if(zzyaxr.gt.1.0)then
          zzyaxr=1.0
          zzytop=zzybot+zzyaxs
        else
          zzytop=pymax
        end if

        tick=0.6*zzhite
        call zmaprm(zzxlft,zzxrgt,zzybot,zzytop,zzhite,tick)
        kzlevl=2
      else
        call errmes('BGNSUB',1,0)
      end if
 
      return
      end
      subroutine blank1(xpos1,xpos2,ypos1,ypos2,iframe)
!
!*********************************************************************72
!
!! BLANK1 defines blank area 1.
!  level 2,3 p/s
!
!  input:   xpos1,xpos2 = x limits in inches
!  from physical origin
!  ypos1,ypos2 = y limits in inches
!  from physical origin
!  iframe      = number of frames to be
!  drawn around blank area
!
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
!
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
        id=1
        call zblank(xpos1,xpos2,ypos1,ypos2,id,iframe)
      else
        call errmes('BLANK1',2,3)
      end if
 
      return
      end
      subroutine blank2(xpos1,xpos2,ypos1,ypos2,iframe)
!
!*********************************************************************72
!
!! BLANK2 defines blank area 2.
!  (level 2,3 p/s)
!
!  input:   xpos1,xpos2 = x limits in inches
!  from physical origin
!  ypos1,ypos2 = y limits in inches
!  from physical origin
!  iframe      = number of frames to be
!  drawn around blank area
!
!
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
!
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
        id=2
        call zblank(xpos1,xpos2,ypos1,ypos2,id,iframe)
      else
        call errmes('BLANK2',2,3)
      end if
 
      return
      end
      subroutine blank3(xpos1,xpos2,ypos1,ypos2,iframe)
!
!*********************************************************************72
!
!! BLANK3 defines blank area 3.
!  (level 2,3 p/s)
!
!  input:   xpos1,xpos2 = x limits in inches
!  from physical origin
!  ypos1,ypos2 = y limits in inches
!  from physical origin
!  iframe      = number of frames to be
!  drawn around blank area
!
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
!
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
        id=3
        call zblank(xpos1,xpos2,ypos1,ypos2,id,iframe)
      else
        call errmes('BLANK3',2,3)
      end if
 
      return
      end
      subroutine blank4(xpos1,xpos2,ypos1,ypos2,iframe)
!
!*********************************************************************72
!
!! BLANK4 defines blank area 4.
!  (level 2,3 p/s)
!
!  input:   xpos1,xpos2 = x limits in inches
!  from physical origin
!  ypos1,ypos2 = y limits in inches
!  from physical origin
!  iframe      = number of frames to be
!  drawn around blank area
!
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
!
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
        id=4
        call zblank(xpos1,xpos2,ypos1,ypos2,id,iframe)
      else
        call errmes('BLANK4',2,3)
      end if
 
      return
      end
      subroutine blanka(xorg,yorg,wide,high,frm)
!
!*********************************************************************72
!
!! BLANK5 defines blank area 5.
!  (level 2,3 p/s)
!
!  input:   xorg,yorg = coordinate of lower left
!  corner, inches from origin
!  wide      = width of rectangle, inches
!  high      = height of rectangle, inches
!  frm       = frame width, negative sign
!  means thicken towards outside,
!  absolute value indicates frame
!  width:
!  if = 0.0 then no frame
!  if > 0.0 and < 1.0 then
!  frame width = frm inches
!  if >= 1.0 then
!  frame width = frm * 0.01"
!
      parameter (kzmxbs=1000)
      parameter (kzmxbl=200)
!
      save /cblank/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /cblank/ zzblnk(kzmxbl,4),zzblks(kzmxbs,4),kzblcn,kzbscn
!
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
        kzblcn=kzblcn+1
        if(kzblcn.gt.kzmxbl)then
          call errmes('BLANKA',0,4)
          kzblcn=kzmxbl
        else
          xpos1=xorg
          ypos1=yorg
          xpos2=xorg+wide
          ypos2=yorg+high
          absfrm=abs(frm)
!
!  no frame
!
          if(absfrm.lt.0.00001)then
            iframe=0
!
!  frame = frm
!
          elseif(absfrm.lt.1.0)then
            temp=absfrm/0.01
            iframe=int(temp)
!
!  frame = frm times 0.01
!
          else
            iframe=int(absfrm)
          end if
          if(frm.lt.0.0) iframe=-iframe
          call zblank(xpos1,xpos2,ypos1,ypos2,kzblcn,iframe)
        end if
      else
        call errmes('BLANKA',2,3)
      end if
 
      return
      end
      subroutine blanks
!
!*********************************************************************72
!
!! BLANKS sets flag to blank curve at symbol area.
!  level 1-3, p/s
!
      integer kzmxbs
      integer kzmxbl

      parameter (kzmxbs=1000)
      parameter (kzmxbl=200)

      save /clevel/
      save /cblank/
!
      common /clevel/ kzlevl,kzbegn
      common /cblank/ zzblnk(kzmxbl,4),zzblks(kzmxbs,4),kzblcn,kzbscn
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        if(kzbscn.lt.0) kzbscn=0
      else
        call errmes('BLANKS',1,3)
      end if
 
      return
      end
      subroutine btext(ip,nlines,xpos,ypos)
!
!*********************************************************************72
!
!! BTEXT writes a centered packed character array.
!  (level 2,3)
!
!  input:   ip     = packed array of characters
!  nlines = number of lines in packed array
!  xpos   = x value from physical origin in inches
!  ypos   = y value from physical origin in inches
!
      parameter (kzyes=111)
!
      character*1 cflag(6)
      character*5 cfont(6)
      character*5 cstyle
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5
      integer ip(*)
!
      save /cdevic/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cpage/
      save /cphysr/
      save /cstrng/
      save /cunit/
      save /gcclip/
!
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /clevel/ kzlevl,kzbegn
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
!
      equivalence (ival,rval)
!
      save jtext
      save jhite
      save jyrat
!
      data jtext,jhite,jyrat /1,-2,-1/
!
!  save old text parameters
!
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
        ohite=zzhite
        oangle=zzangl
!
!  calculate virtual coordinates and length of a line of characters
!
        vx=zzxor+xcm0+xpos*zzunit*zzpagr
        vy=zzyor+ycm0+ypos*zzunit*zzpagr
 
        if(ip(1).eq.kzyes)then
          iplen=ip(2)/1000
          llen=ip(2)-iplen*1000
        else
          llen=40
        end if
 
        nword=(llen-1)/kzbyte+5
        xlen=xbtext(ip,nlines)*zzunit*zzpagr
        ylen=ybtext(ip,nlines)*zzunit*zzpagr
        icur=2+nword
        ival=ip(icur)
        ave=rval
        ival=ip(icur+jyrat)
        yrat=rval
        if(yrat.lt.1.0) yrat=1.0
        vx=vx-(xlen/2.0)
        vy=vy+ylen+(yrat-1.0)*ave/2.0
!
!  for each line in array: retrieve line space parameter, character
!  height, and character font, set to current and draw to output device
!
        icur=2
 
        do i=1,nlines
 
          icur=icur+nword
          ival=ip(icur+jyrat)
          yrat=rval
          if(yrat.lt.1.0) yrat=1.0
          ival=ip(icur+jhite)
          hite=rval
          delta=(yrat-1.0)*ave/2.0
          vy=vy-delta-hite
 
          do k=1,6
            cflag(k)=czlgac(i,k)
            cfont(k)=czlgaf(i,k)
          end do
 
          cstyle=czlgas(i)
          call gssetc(hite,0.0)
          call dsmove(vx,vy)
          call ztext(ip(icur-nword+jtext),100,cflag,cfont,cstyle)
          vy=vy-delta
 
        end do
 
        call gssetc(ohite,oangle)
!
!  wrong level
!
      else
        call errmes('BTEXT',2,3)
      end if
 
      return
      end
      subroutine btextl(ip,nlines,xpos,ypos)
!
!*********************************************************************72
!
!! BTEXTL writes packed character array, left justified.
!  (level 2,3)
!
!  input:   ip     = packed array of characters
!  nlines = number of lines in packed array
!  xpos   = x value from physical origin in inches
!  ypos   = y value from physical origin in inches
!
      parameter (kzyes=111)
!
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5
      integer ip(*)
      character*5 cflag(6)*1,cfont(6)
      character*5 cstyle
!
      save /cdevic/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cpage/
      save /cphysr/
      save /cstrng/
      save /gcclip/
!
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /clevel/ kzlevl,kzbegn
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
 
      equivalence (ival,rval)
!
      save jtext
      save jhite
      save jyrat
!
      data jtext,jhite,jyrat /1,-2,-1/
!
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
!
!  save old text parameters
!
        ohite=zzhite
        oangle=zzangl
!
!  calculate virtual coordinates and length of a line of characters
!
        vx=zzxor+xcm0+xpos*zzunit*zzpagr
        vy=zzyor+ycm0+ypos*zzunit*zzpagr
 
        if(ip(1).eq.kzyes)then
          iplen=ip(2)/1000
          llen=ip(2)-iplen*1000
        else
          llen=40
        end if
 
        nword=(llen-1)/kzbyte+5
        ylen=ybtext(ip,nlines)*zzunit*zzpagr
        icur=2+nword
        ival=ip(icur)
        ave=rval
        ival=ip(icur+jyrat)
        yrat=rval
        if(yrat.lt.1.0) yrat=1.0
        vy=vy+ylen+(yrat-1.0)*ave/2.0
!
!  for each line in array: retrieve line space parameter, character
!  height, and character font, set to current and draw to output device
!
        icur=2
 
        do i=1,nlines
 
          icur=icur+nword
          ival=ip(icur+jyrat)
          yrat=rval
          if(yrat.lt.1.0) yrat=1.0
          ival=ip(icur+jhite)
          hite=rval
          delta=(yrat-1.0)*ave/2.0
          vy=vy-delta-hite
 
          do k=1,6
            cflag(k)=czlgac(i,k)
            cfont(k)=czlgaf(i,k)
          end do
 
          cstyle=czlgas(i)
          call gssetc(hite,0.0)
          call dsmove(vx,vy)
          call ztext(ip(icur-nword+jtext),100,
     &                 cflag,cfont,cstyle)
          vy=vy-delta
 
        end do
 
        call gssetc(ohite,oangle)
!
!  wrong level
!
      else
        call errmes('BTEXTL',2,3)
      end if
 
      return
      end
      subroutine btextr(ip,nlines,xpos,ypos)
!
!*********************************************************************72
!
!! BTEXTR writes packed character array, right justified.
!  (level 2,3)
!
!  input:   ip     = packed array of characters
!  nlines = number of lines in packed array
!  xpos   = x value from physical origin in inches
!  ypos   = y value from physical origin in inches
!
      parameter (kzyes=111)
!
      save /cdevic/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cpage/
      save /cphysr/
      save /cstrng/
      save /cunit/
      save /gcclip/
!
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /clevel/ kzlevl,kzbegn
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5
!
      integer ip(*)
      character*5 cflag(6)*1,cfont(6),cstyle
      equivalence (ival,rval)
!
      save jtext
      save jhite
      save jyrat
!
      data jtext,jhite,jyrat /1,-2,-1/
!
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
!
!  save old text parameters
!
        ohite=zzhite
        oangle=zzangl
!
!  calculate virtual coordinates and length of a line of characters
!
        vx=zzxor+xcm0+xpos*zzunit*zzpagr
        vy=zzyor+ycm0+ypos*zzunit*zzpagr
        if(ip(1).eq.kzyes)then
          iplen=ip(2)/1000
          llen=ip(2)-iplen*1000
        else
          llen=40
        end if
 
        nword=(llen-1)/kzbyte+5
        xlen=xbtext(ip,nlines)*zzunit*zzpagr
        ylen=ybtext(ip,nlines)*zzunit*zzpagr
        icur=2+nword
        ival=ip(icur)
        ave=rval
        ival=ip(icur+jyrat)
        yrat=rval
        if(yrat.lt.1.0) yrat=1.0
        vx=vx-xlen
        vy=vy+ylen+(yrat-1.0)*ave/2.0
!
!  for each line in array: retrieve line space parameter, character
!  height, and character font, set to current and draw to output device
!
        icur=2
 
        do i=1,nlines
 
          icur=icur+nword
          ival=ip(icur+jyrat)
          yrat=rval
          if(yrat.lt.1.0) yrat=1.0
          ival=ip(icur+jhite)
          hite=rval
          delta=(yrat-1.0)*ave/2.0
          vy=vy-delta-hite
 
          do k=1,6
            cflag(k)=czlgac(i,k)
            cfont(k)=czlgaf(i,k)
          end do
 
          cstyle=czlgas(i)
          call gssetc(hite,0.0)
          call dsmove(vx,vy)
          call ztext(ip(icur-nword+jtext),100,
     &                 cflag,cfont,cstyle)
          vy=vy-delta
 
        end do
 
        call gssetc(ohite,oangle)
!
!  wrong level
!
      else
        call errmes('BTEXTR',2,3)
      end if
 
      return
      end
      subroutine camrot
!
!***********************************************************************
!
!! CAMROT makes up a camera rotation matrix.
!
!  rotation is done so that z prime axis is directed from the
!  camera to the aiming point.   note also that the primed
!  coordinate system is left-handed if epslon=-1.
!
!  this is so that the picture comes out right when projected
!  on the primed coordinate system.
!
      real au(3)
      real av(3)
      real aw(3)
      real flim(2)
      integer limit(2)
!
      save /comdp/
!
      common/comdp/xmin,xmax,ymin,ymax,zmin,zmax,axisr(2),plotx,
     &   ploty,pltorg(2),camxyz(3),mx,ny,fmx,fny,camwkg(6),xorg(3),
     &   gx(3),fx(2),kscale,zorg,center(2),pqlmt,
     &   amtx(3,3),focall
!
      equivalence(u,camxyz(1)),(v,camxyz(2)),(w,camxyz(3)),
     &   (mx,limit(1)),(fmx,flim(1))
!
!  handedness parameter, -1 for left-handed usually
!
      epslon=-1.0
 
      s = 0.0
      do j = 1,3
        av(j) = 0.0
        aw(j) = 0.0
        au(j) = camwkg(j+3)-camwkg(j)
        s = s + au(j)**2
      end do
 
      s = sqrt(s)
 
      do j = 1,3
        au(j) = au(j)/s
      end do
 
      sigma = sqrt(au(1)**2 + au(2)**2)
!
!  prepare looking straight up or down
!
      av(1) = 1.0
      aw(2) = -epslon
      if(au(3) .gt. 0.0) aw(2) = -aw(2)
      if(sigma .lt. 1.0e-3) go to 4
!
!  x axis
!
      av(1) = au(2)/sigma
      av(2) = -au(1)/sigma
      av(3) = 0.0
!
!  y axis
!
      aw(1) = epslon*au(1)*au(3)/sigma
      aw(2) = epslon*au(2)*au(3)/sigma
      aw(3) = -epslon*sigma
!
!  transfer axis direction cosines to rotation matrix rows
!
4     do j = 1,3
        amtx(1,j) = av(j)
        amtx(2,j) = aw(j)
        amtx(3,j) = au(j)
      end do
 
      return
      end
      subroutine capchr(string)
!
!***********************************************************************
!
!! CAPCHR capitalizes the entries of a character string.
!
!
!  Input/output, CHARACTER*(*) STRING, is the string of characters to
!  be transformed.
!
      integer i
      integer itemp
      integer nchar
      character*(*) string
!
      intrinsic char
      intrinsic ichar
      intrinsic len
!
      nchar = len(string)
 
      do i=1, nchar
 
        itemp=ichar(string(i:i))
 
        if ( 97 .le. itemp .and. itemp .le. 122 ) then
          string(i:i) = char(itemp-32)
        end if
 
      end do
 
      return
      end
      subroutine cartog
!
!***********************************************************************
!
!! CARTOG sets the character type to cartographer.
!  level 1-3, p/s
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      character czalfl*1,czalfn*5,czalfs*5
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfs='CARTO'
      else
        call errmes('CARTOG',1,3)
      end if
 
      return
      end
      subroutine chndot
!
!***********************************************************************
!
!! CHNDOT sets the line style to chain-dot.
!  level 1-3, p/s
!
      parameter (kzcdot=4)
!
      save /clevel/
      save /cline/
!
      common /clevel/ kzlevl,kzbegn
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
 
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzlsty=kzcdot
        call dsltyp(kzlsty)
      else
        call errmes('CHNDOT',1,3)
      end if
 
      return
      end
      subroutine chndsh
!
!***********************************************************************
!
!! CHNDSH sets the line style to dash.
!  level 1-3, p/s
!
      parameter (kzcdsh=5)
!
      save /clevel/
      save /cline/
!
      common /clevel/ kzlevl,kzbegn
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
 
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzlsty=kzcdsh
        call dsltyp(kzlsty)
      else
        call errmes('CHNDSH',1,3)
      end if
 
      return
      end
      subroutine clline(x1,y1,x2,y2)
!
!***********************************************************************
!
!! CLLINE draws the line from (x1,y1) to (x2,y2) with clipping.
!
      real area(4)
      real xvlen
      real xvstrt
      real yvlen
      real yvstrt
 
      save /pltsiz/
!
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      call gssclp(xvstrt,xvstrt+xvlen,yvstrt,yvstrt+yvlen,area)
      call scale(x1,y1,vx,vy)
      call gsmove(vx,vy)
      call scale(x2,y2,vx,vy)
      call gsdraw(vx,vy)
      call gsrclp(area)
 
      return
      end
      subroutine clrset(color)
!
!***********************************************************************
!
!! CLRSET sets the current plotting color.
!  (level 1-3)
!
!  input:   icolor = color name, 'WHIT', 'RED', etc.
!
      integer kzmaxc
      parameter (kzmaxc=255)
!
      character*(*) color
      character*8 czcolr
!
      save /clevel/
      save /colorc/
      save /colorn/
      save /gcdchr/
!
      common /clevel/ kzlevl,kzbegn
      common /colorn/ kzshd,zzhspc,zzcolr(kzmaxc,3),kznclr,kzccol
      common /colorc/ czcolr
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
!
      save one,zero
      data one,zero /1.0,0.0/
!
      czcolr = color
 
      call capchr(czcolr)
 
      if(kzlevl.ge.1.and.kzlevl.le.3)then
!
!  actually setting colors for tk4115 and tk4510
!
        if ( ksyand(idvbts,32) .ne. 0) then

          if(czcolr(1:3).eq.'RED')then
            call hrdhsi(1.0,one,one)
          elseif(czcolr.eq.'GREE')then
            call hrdhsi(2.0,one,one)
          elseif(czcolr.eq.'BLUE')then
            call hrdhsi(3.0,one,one)
          elseif(czcolr.eq.'YELL')then
            call hrdhsi(1.5,one,one)
          elseif(czcolr.eq.'MAGE')then
            call hrdhsi(0.5,one,one)
          elseif(czcolr.eq.'CYAN')then
            call hrdhsi(2.5,one,one)
          elseif(czcolr.eq.'BLAC'.or.czcolr.eq.'BACK')then
            call hrdhsi(zero,one,zero)
          elseif(czcolr.eq.'WHIT'.or.czcolr.eq.'FORE')then
            call hrdhsi(4.0,zero,one)
          end if

        else
!
!  select color from existing color table
!
          if(czcolr(1:3).eq.'RED')then
            call gscolr(2,ierr)
          elseif(czcolr(1:4).eq.'GREE')then
            call gscolr(3,ierr)
          elseif(czcolr(1:4).eq.'BLUE')then
            call gscolr(4,ierr)
          elseif(czcolr(1:4).eq.'YELL')then
            call gscolr(5,ierr)
          elseif(czcolr(1:4).eq.'MAGE')then
            call gscolr(6,ierr)
          elseif(czcolr(1:4).eq.'CYAN')then
            call gscolr(7,ierr)
          elseif(czcolr(1:4).eq.'BLAC')then
            call gscolr(0,ierr)
          elseif(czcolr(1:4).eq.'BACK')then
            call gscolr(0,ierr)
          elseif(czcolr(1:4).eq.'WHIT')then
            call gscolr(1,ierr)
          elseif(czcolr(1:4).eq.'FORE')then
            call gscolr(1,ierr)
          else
            write(*,*)' '
            write(*,*)'CLRSET - Warning!'
            write(*,*)'  Unrecognized color requested: ',czcolr
          end if
 
        end if
 
      else
 
        call errmes('CLRSET',1,3)
 
      end if
 
      return
      end
      subroutine cmplx2
!
!***********************************************************************
!
!! CMPLX2 sets the character type to complex2.
!  level 1-3, p/s
!
      character czalfl*1,czalfn*5
      character*5 czalfs
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfs='CMPLX'
      else
        call errmes('CMPLX2',1,3)
      end if
 
      return
      end
      subroutine complx
!
!***********************************************************************
!
!! COMPLX sets the character type to complex.
!  level 1-3, p/s
!
      character czalfl*1,czalfn*5
      character*5 czalfs
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfs='COMPL'
      else
        call errmes('COMPLX',1,3)
      end if
 
      return
      end
      subroutine contor(z,nz,iz,mx,my,x1,xmx,y1,ymy,nl,cl)
!
!***********************************************************************
!
!! CONTOR produces a contour plot of a function Z(X,Y).
!
!  Here, Z is actually stored as a table of values:
!
!    z(i,j) = f(x(i),y(j)).
!
!  Here it is assumed that the x and y values are equally spaced.
!  hence the user does not supply the x and y arrays, but rather
!  simply the first and last x and y values, and the number of
!  rows and columns of data.
!
!  before calling contor, you must call mapit to establish the
!  coordinate axes, and have the x axis at least large enough to
!  cover the range of the x data, and the y axis cover the range
!  of the y data.
!
!
!  Input, real z(nz,my), the values of the function to contour:
!
!  z(i,j) = f(xi,yj)
!
!  where:
!
!  xi = x1 + (i-1)*(xmx-x1)/(mx-1)
!  yj = y1 + (j-1)*(ymx-y1)/(my-1)
!
!  Input, integer nz, integer constant or variable.
!  The first dimension of the array z - not necessarily
!  equal to mx, but mx <= nz.
!
!  Work space, integer iz(mx,my).
!
!  Input, integer mx, the number of x grid points.
!
!  Input, integer my, the number of y grid points.
!
!  Input, real x1, the minimum x value.
!
!  Input, real xmx, the maximum x value.
!
!  Input, real y1, the minimum y value.
!
!  Input, real ymy, the maximum y value.
!
!  Input, integer nl, the number of contour levels.
!
!  Input, real cl(nl), the contour levels to draw.
!
      integer mx
      integer my
      integer nl
      integer nz
!
      real z(nz,my)
      real cl(nl)
      real dx
      real dy
      integer i
      integer iz(mx,my)
      integer j
      integer nloop
      integer nx
      integer ny
      real x1
      real xl
      real y1
!
      save /contr/
!
      common /contr/ clevel,iold,jold,in,jn,
     &   nx,ny,xl,dx,yl,dy
!
!  initialize routine
!
      xl = x1
      yl = y1
      dx = xmx-x1
      dy = ymy-y1
      nx=mx
      ny=my
      nloop=min1(float(nx)/2.0+.5,float(ny)/2.0+.5)
!
!  start searching for plus-minus transitions to start a contour on.
!
      do nc=1,nl
!
!  zero array showing where we have been
!
        do j=1,ny
          do i=1,nx
            iz(i,j)=0
          end do
        end do
 
        clevel=cl(nc)
 
        do icir=1,nloop
 
          iu=nx+1-icir
          ju=ny+1-icir
 
          do j=icir,ju-1
            call look(z,icir,j,1,iz,nz,nx)
          end do
 
          do i=icir,iu-1
            call look(z,i,ju,2,iz,nz,nx)
          end do
 
          do j=ju,icir+1,-1
            call look(z,iu,j,3,iz,nz,nx)
          end do
 
          do i=iu,icir+1,-1
            call look(z,i,icir,4,iz,nz,nx)
          end do
 
        end do
 
      end do
 
      return
      end
      subroutine crvwid(thick)
!
!***********************************************************************
!
!! CRVWID sets the line thickness in inches.
!  level 1-3, p/s
!
!  input:   thick = line thickness in inches
!
      integer kzyes
      parameter (kzyes=111)
!
      save /clevel/
      save /cline/
      save /cpage/
      save /cunit/
!
      common /clevel/ kzlevl,kzbegn
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
!
      if(kzlevl.eq.1)then
        uulthk=thick*zzunit
        zzlthk=uulthk
        kzlthk=kzyes
      elseif(kzlevl.eq.2.or.kzlevl.eq.3)then
        uulthk=thick*zzunit
        zzlthk=uulthk*zzpagr
        kzlthk=kzyes
      else
        call errmes('CRVWID',1,3)
      end if
 
      return
      end
      function cszmap()
!
!***********************************************************************
!
!! CSZMAP returns the character size that MAP used or will use.
!
      real cszmap
!
      save /pltprm/
!
      common /pltprm/ cxsize, cysize, tickln, yvini
 
      cszmap = cysize
 
      return
      end
      subroutine cubspl
!
!***********************************************************************
!
!! CUBSPL sets the interpolation flag to use cubic splines.
!  level 1-3, p/s
!
      parameter (kzcspl=3)
!
      save /clevel/
      save /cline/
!
      common /clevel/ kzlevl,kzbegn
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzltyp=kzcspl
      else
        call errmes('CUBSPL',1,3)
      end if

      return
      end
      subroutine cursor(x,y,key)
!
!***********************************************************************
!
!! CURSOR displays and reads the graphics cursor and returns its position.
!
      character key
      logical logx
      logical logy
!
      save /pltcom/
      save /pltsiz/
!
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
!  get cursor position in virtual coordinates.
!
      call gsgin(x,y,key,ierr)
 
      if(ierr .lt. 0) then
        x = xvstrt
        y = yvstrt
      end if
 
      x = (x-xvstrt)*udx/xvlen + ux0
      if(logx) x = 10.0**x
      y = (y-yvstrt)*udy/yvlen + uy0
      if(logy) y = 10.0**y
 
      return
      end
      subroutine curve(x,y,npts,npbsym)
!
!***********************************************************************
!
!! CURVE draws a curve according to the interpolation flag.
!  (level 3)
!
!  input:   x,y    = input array of x and y values
!  npts   = number of input x y pairs
!  npbsym = symbol flag
!  if npbsym > 0, connect points and
!  draw symbols
!  if npbsym = 0, connect points only
!  if npbsym < 0, draw symbol only
!
      parameter (kzyes=111)
      parameter (kzmxbs=1000)
      parameter (kzmxbl=200)
      parameter (kzlin=2)
      parameter (kzcspl=3)
      parameter (kzpspl=4)
!
      character czlgac*1,czlgaf*5,czlgas*5
      character*1 czlgtc
      character*5 czlgtf
      character czlgts*5
      logical linilt
      logical lposnd
      real x(*)
      real y(*)
!
      save /cblank/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cline/
      save /csymbo/
      save /dcltyp/
      save /gcdchr/
!
      common /clevel/ kzlevl,kzbegn
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /cblank/ zzblnk(kzmxbl,4),zzblks(kzmxbs,4),kzblcn,kzbscn
      common /csymbo/ kzsym,kznsym,zzsmsz,uusmsz
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
!
!  update legend line counter
!
      if(kzlevl.eq.3)then
 
        kzlgcn=kzlgcn+1
 
        if(kzlgcn.gt.50)then
          kzlger=kzyes
          kzlgcn=50
        end if
!
!  draw symbol if desired, save old line style and draw symbol in solid
!
        if(npbsym.ne.0)then
 
          kzlgsm(kzlgcn)=kzsym
 
          if(npbsym.ge.0)then
            nsym=npbsym
          else
            nsym=-npbsym
          end if
 
          ioldlt=ilntyp
          ilntyp=1
          half=zzsmsz*0.55
          imark=1
 
          do i=1,npts,nsym
            imark=i
            call scale(x(i),y(i),vx,vy)
            call dsmove(vx,vy)
            call dsymbo(kzsym,zzsmsz)
!
!  activate blank area, if desired
!
            if(kzbscn.ge.0)then

              kzbscn=kzbscn+1
 
              if(kzbscn.gt.kzmxbs)then
                call errmes('CURVE',0,5)
                kzbscn=kzmxbs
              else
                zzblks(kzbscn,1)=vx+half
                zzblks(kzbscn,2)=vx-half
                zzblks(kzbscn,3)=vy+half
                zzblks(kzbscn,4)=vy-half
              end if
 
            end if
 
          end do
!
!  if no marker drawn at end of line, draw one
!
          if(imark.ne.npts)then
            call scale(x(npts),y(npts),vx,vy)
            call dsmove(vx,vy)
            call dsymbo(kzsym,zzsmsz)
 
            if(kzbscn.ge.0)then
              kzbscn=kzbscn+1
 
              if(kzbscn.gt.kzmxbs)then
                call errmes('CURVE',0,5)
                kzbscn=kzmxbs
              else
                zzblks(kzbscn,1)=vx+half
                zzblks(kzbscn,2)=vx-half
                zzblks(kzbscn,3)=vy+half
                zzblks(kzbscn,4)=vy-half
              end if
 
            end if
 
          end if
!
!  restore line style
!
          ilntyp=ioldlt
        end if
 
        kzsym=kzsym+1
        if(kzsym.gt.kznsym) kzsym=0
 
        if(npbsym.ge.0)then
 
          if(kzlgln.eq.kzyes)then
            kzlglt(kzlgcn)=ilntyp
            if(kzlthk.eq.kzyes)then
              zzlgth(kzlgcn)=zzlthk
              kzlglt(kzlgcn)=kzlglt(kzlgcn)+10
            end if
 
          end if
!
!  draw connected line with no curve smoothing
!
          if(kzltyp.eq.kzlin.or.npts.le.3)then

            if(npts.le.3.and.kzltyp.ne.kzlin)then
              call errmes('CURVE',0,4)
            end if

            call zcurve(x,y,npts)
!
!  draw cubic spline curve
!
          elseif(kzltyp.eq.kzcspl.and.npts.le.102)then
            call zcurcs(x,y,npts)
!
!  draw parametric cubic spline curve
!
          elseif(kzltyp.eq.kzpspl.and.npts.le.51)then
            call zcurps(x,y,npts)
!
!  draw parametric polynomial curve
!
          else
            call zcurpp('W',x,y,npts)
          end if
        end if
!
!  The package is not at the appropriate level
!
      else
        write(*,*)' '
        write(*,*)'CURVE - Fatal error!'
        write(*,*)'  The package is not at the correct level.'
        write(*,*)'  The current level is ',kzlevl
        write(*,*)'  The package must be at level 3 to use CURVE.'
      end if
 
      return
      end
      subroutine curves(x,y,npts,isymno,symsiz,npbsym)
!
!***********************************************************************
!
!! CURVES draws a line through a set of points, with clipping and symbols.
!
!  it then adds the desired symbol (isymno) to the plot spaced "npbsym" 
!  points apart.
!
      integer npts
!
      real area(4)
      logical linilt
      logical lposnd
      real x(npts)
      real y(npts)
!
      save /gcltyp/
      save /pltsiz/
!
      common /gcltyp/ ilntyp, dleft, dist(4,3), linilt, lposnd
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      call gssclp(xvstrt,xvstrt+xvlen,yvstrt,yvstrt+yvlen,area)
 
      call scale(x(1),y(1),vx,vy)
 
      call gsmove(vx,vy)
 
      do i=2,npts
        call scale(x(i),y(i),vx,vy)
        call gsdraw(vx,vy)
      end do
!
!  now add symbols if desired
!
!  save line type, and do symbols in solid lines
!
      if(isymno .gt. 0) then
 
        ioldlt = ilntyp
        ilntyp = 1
 
        do i=1, npts, npbsym
          call scale(x(i),y(i),vx,vy)
          call gsmove(vx,vy)
          call symbol(isymno,symsiz)
        end do
!
!  restore line type
!
        ilntyp = ioldlt
 
      end if
 
      call gsrclp(area)

      return
      end
      subroutine curvey(xmin,xmax,y,npts,isymno,symsiz,npbsym)
!
!***********************************************************************
!
!  CURVEY draws the curve that passes through a set of points (x(i), y(i)).  
!
!  Here it is assumed that the x values are
!  equally spaced.  hence, no x array is needed, just the range
!  of x.
!
!  curvey will automatically clip portions of the curve that
!  go outside the limits of the current graph.
!
!  curvey is callable from level 3.
!
!
!  input, real xmin, xmax, the minimum and maximum values
!  of the x variable.
!
!  input, real y(npts), the y values of the curve at each
!  of the equally spaced x values.
!
!  input, real symsiz, the size of the marker to be used
!  to mark the data points.
!
!  input, integer npbsym, the interval between data points
!  to be marked.  point 1 will be marked, and point
!  1+npbsym, and so on.
!
!  set npbsym=1 to mark all points.
!
!  set npbsym=0 for no marking.
!
      integer npts
!
      real area(4)
      logical linilt
      logical lposnd
      real y(npts)
!
      save /gcltyp/
      save /pltsiz/
!
      common /gcltyp/ ilntyp, dleft, dist(4,3), linilt, lposnd
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      call gssclp(xvstrt,xvstrt+xvlen,yvstrt,yvstrt+yvlen,area)
 
      call scale(xmin,y(1),vx,vy)
 
      call gsmove(vx,vy)
 
      dx = (xmax-xmin)/(npts-1)
      xnew = xmin
 
      do i=2,npts
        xnew = xmin + (i-1)*dx
        call scale(xnew,y(i),vx,vy)
        call gsdraw(vx,vy)
      end do
!
!  now add symbols if desired
!
      if(isymno .gt. 0.and.npbsym.gt.0)then
        ioldlt = ilntyp
        ilntyp = 1
 
        do i=1,npts,npbsym
          xnew = xmin + (i-1)*dx
          call scale(xnew,y(i),vx,vy)
          call gsmove(vx,vy)
          call symbol(isymno,symsiz)
        end do
 
        ilntyp = ioldlt
      end if
 
      call gsrclp(area)
 
      return
      end
      subroutine dash
!
!***********************************************************************
!
!! DASH sets the line style to dash.
!  level 1-3, p/s
!
      parameter (kzdash=3)
!
      save /clevel/
      save /cline/
!
      common /clevel/ kzlevl,kzbegn
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
 
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzlsty=kzdash
        call dsltyp(kzlsty)
      else
        call errmes('DASH',1,3)
      end if
 
      return
      end
      subroutine ddevsl(newdev,lun,ierr)
!
!***********************************************************************
!
!! DDEVSL selects a graphics output device.
!
!  if a graphics output device was already selected,
!  ddevsl automatically closes the old device before
!  opening the new one.
!
!  input, integer newdev, selects the new device, by number.
!
!  1 tektronix 4014
!  2 tektronix 4115b
!  3 QMS printer, landscape
!  4 QMS printer, portrait
!  5 vt-240
!  6 tektronix 4510
!  7 sel. 100xl
!  8 pc (st-240)
!  9 vector file
!  10 ln03 plus - landscape
!  11 ln03 plus - portrait
!  12 tektronix 4107
!  13 film recorder lab
!  14 postscript file, landscape
!  15 postscript file, portrait
!  20 arcgraph system
!  21 psc cgm graphics
!
!  input, integer lun, selects a fortran unit for possible
!  use by the graphics output device.
!
!  output, integer ierr, an error flag.
!  0 means no error occurred.
!
      real devchr(8)
      real devid
      real dfdist(13,15)
      real dummy
      integer idev
      integer ierr
      logical lcurnt
      logical linilt
      logical lposnd
      integer lun
      integer newdev
      real xlencm
      real ylencm
!
!  external statements to invoke block data subprograms.
!
      external blkdat
      external blksys
      external inidat
!
      save /cline/
      save /dcltyp/
      save /gcclip/
      save /gcdchr/
      save /gccpar/
      save /gccpos/
      save /gcdprm/
      save /gcdsel/
      save /gcvpos/
!
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
      common /gcdsel/ idev
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
      common /gccpar/ csize, ccos, csin
      common /gccpos/ xapos, yapos, ivis, lcurnt
      common /gcvpos/ xvpos, yvpos
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
!
!  dfdist defines default line styles.
!
      save dfdist
!
      data dfdist /
     &   0.08, 0.08, 0.08, 0.08,8*0.0,4.0,
     &   0.20, 0.15, 0.20, 0.15,8*0.0,4.0,
     &   0.75, 0.10, 0.05, 0.10,8*0.0,4.0,
     &   1.00, 0.25, 0.25, 0.25,8*0.0,4.0,143*0.0/
!
      dummy = 0.0
!
!  release the current device, if any.
!
      if (idev .ne. 0) then
        call gsdrvr(6,dummy,dummy)
      end if
!
!  initialize the new device.
!
      if (newdev .le. 0) then
        ierr = -1
        idev = 0
        return
      end if
 
      idev = newdev
!
!  get the device characteristics (and see if device there)
!
      devchr(8) = 1.0
      call gsdrvr(7,devchr,dummy)
 
      if (devchr(1) .eq. 0.0) then
        ierr = -1
        idev = 0
        return
      end if
!
!  initialize the device for graphics
!
      call gsdrvr(1,float(lun),dummy)
      ierr = int(dummy)
 
      if (ierr .ne. 0) then
        idev = 0
        return
      end if
!
!  set device characteristics for later use
!
      devid  = devchr(1)
      xlencm = devchr(2)
      ylencm = devchr(3)
      xres   = devchr(4)
      yres   = devchr(5)
      ndclrs = int(devchr(6))
      idvbts = int(devchr(7))
      nfline = int(devchr(8))
      xclipd = xlencm + 0.499/devchr(4)
      yclipd = ylencm + 0.499/devchr(5)
!
!  now init the parameters.
!
      xs = 1.0
      ys = 1.0
      xt = 0.0
      yt = 0.0
      rcos = 1.0
      rsin = 0.0
      vxl = 0.0
      vxh = xlencm
      vyl = 0.0
      vyh = ylencm
      csize = 0.3
      ccos = 1.0
      csin = 0.0
      ivis = 0
      xcm0 = 0.0
      ycm0 = 0.0
      xcm1 = xclipd
      ycm1 = yclipd
      ilntyp = 1
 
      do i=1,4
        do j=1,13
          dist(j,i) = dfdist(j,i)
        end do
      end do
 
      lcurnt = .false.
 
      return
      end
      subroutine defalf(lalpha)
!
!***********************************************************************
!
!! DEFALF sets the base alphabet set.
!  (level 1-3, p/s)
!
!  input:   lalpha = character set
!
      character czalfl*1,czalfn*5,czalfs*5
      character*(*) lalpha
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfl(1)=')'
        czalfn(1)=lalpha(1:5)
        call capchr(czalfn(1))
      else
        call errmes('DEFALF',1,3)
      end if
 
      return
      end
      subroutine devsel(newdev,lun,ierr)
!
!***********************************************************************
!
!! DEVSEL...
!
      external blkdat
      external blksys
      external inidat
!
      real devchr(8)
      real gdcomn(5)
      real dfdist(4,3)
      logical lcurnt
      logical linilt
      logical lposnd
!
      save /gcclip/
      save /gccpos/
      save /gcdchr/
      save /gccpar/
      save /gccpos/
      save /gcdprm/
      save /gcdsel/
      save /gcltyp/
!
      common /gcdsel/ idev
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
      common /gccpar/ csize, ccos, csin
      common /gcvpos/ xvpos, yvpos
      common /gccpos/ xapos, yapos, ivis, lcurnt
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd
      common /gcltyp/ ilntyp, dleft, dist(4,3), linilt, lposnd
!
!  define default line styles
!
      equivalence (devid,gdcomn(1))
!
      save dfdist
!
      data dfdist /
     &    0.5,  0.5,  0.5,  0.5,
     &   0.25, 0.25, 0.25, 0.25,
     &    0.5, 0.25, 0.25, 0.25/
!
!  Release current device
!
      if(idev .ne. 0) then
        call gsdrvr(6,dummy,dummy)
      end if
!
!  now init. the new device
!
      if(newdev .le. 0) then
        ierr = -1
        idev = 0
        return
      end if
 
      idev = newdev
!
!  get the device characteristics (and see if device there)
!
      devchr(8) = 1.0
      call gsdrvr(7,devchr,dummy)
 
      if(devchr(1) .eq. 0.0) then
        ierr = -1
        idev = 0
        return
      end if
!
!  initialize the device for graphics
!
      call gsdrvr(1,float(lun),dummy)
      ierr = int(dummy)
 
      if(ierr .ne. 0) then
        idev = 0
        return
      end if
!
!  set device characteristics for later use
!
      do i=1,5
        gdcomn(i) = devchr(i)
      end do
 
      ndclrs = int(devchr(6))
      idvbts = int(devchr(7))
      nfline = int(devchr(8))
      xclipd = xlencm + 0.499/devchr(4)
      yclipd = ylencm + 0.499/devchr(5)
!
!  now init the parameters
!
      xs = 1.0
      ys = 1.0
      xt = 0.0
      yt = 0.0
      rcos = 1.0
      rsin = 0.0
      vxl = 0.0
      vxh = xlencm
      vyl = 0.0
      vyh = ylencm
      csize = goodcs(0.3)
      ccos = 1.0
      csin = 0.0
      ivis = 0
      xcm0 = 0.0
      ycm0 = 0.0
      xcm1 = xclipd
      ycm1 = yclipd
      ilntyp = 1
 
      do i=1,3
        do j=1,4
          dist(j,i) = dfdist(j,i)
        end do
      end do
 
      lcurnt = .false.
 
      return
      end
      subroutine dhatch(xvert, yvert, numpts, phi, cmspac, iflags,
     &  xx,yy)

!***********************************************************************
!
!! DHATCH provides shading for a general polygonal region.  
!
!  there is absolutely no assumption made about convexity.  a polygon is 
!  specified  by its vertices, given in either a clockwise or 
!  counter-clockwise order.  
!
!  The density of the shading lines (or points) and the angle for the shading 
!  lines are both determined by the parameters passed to the routine.
!
!  the input parameters are interpreted as follows:
!
!  xvert  -  an array of x coordinates for the polygon(s) vertices
!
!  yvert  -  an array of y coordinates for the polygon(s) vertices
!
!  note: an x value >=1e38 signals a new polygon.   this allows
!  filling areas that have holes where the holes are
!  defined as polygons.   it also allows multiple
!  polygons to be filled in one call to hatch.
!
!  numpts -  the number of vertices in the polygon(s) including
!  the seperator(s) if any.
!
!  phi    -  the angle for the shading, measured counter-clockwise
!  in degrees from the positive x-axis
!
!  cmspac -  the distance in virtual coordinates (cm. usually)
!  between shading lines.   this value may be rounded
!  a bit, so some cummulative error may be apparent.
!
!  iflags -  general flags controlling hatch
!  0 ==>  boundary not drawn, input is virtual coord.
!  1 ==>  boundary drawn, input is virtual coord.
!  2 ==>  boundary not drawn, input is world coord.
!  3 ==>  boundary drawn, input is world coord.
!
!  xx     -  a work array at least "numpts" long.
!
!  yy     -  a second work array at least "numpts" long.
!
      integer numpts
!
      logical lmove
      real xintcp(20)
      real xvert(numpts)
      real xx(numpts)
      real yvert(numpts)
      real yy(numpts)
!
      save /gcdchr/
!
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &       ndclrs, idvbts, nfline, xclipd, yclipd
!
!  this routine has to maintain an internal array of the transformed
!  coordinates.  this requires the passing of the two working arrays
!  called "xx" and "yy".
!  this routine also needs to store the intersections of the hatch
!  lines with the polygon.   this is done in "xintcp".
!
      save bignum,fact,pi180
!
!  x >= 'BIGNUM' signals the end of a polygon in the input.
!
      data bignum /1e38/
      data fact /16.0/
      data pi180 /0.017453292/
!
!  check for valid number of vertices.
!
      if(numpts .lt. 3) return
!
!  convert all of the points to integer coordinates so that the shading
!  lines are horizontal.  this requires a rotation for the general case.
!  the transformation from virtual to internal coordinates has the two
!  or three phases:
!
!  (1)  convert world to virtual coord. if input in world coord.
!
!  (2)  rotate clockwise through the angle phi so shading is horizontal,
!
!  (3)  scale to integers in the range
!  [0...2*fact*(device_maxy_coordinate)], forcing coordinates
!  to be odd integers.
!
!  the coordinates are all odd so that later tests will never have an
!  outcome of "equal" since all shading lines have even coordinates.
!  this greatly simplifies some of the logic.
!
!  at the same time the pre-processing is being done, the input is checked
!  for multiple polygons.  if the x-coordinate of a vertex is >= 'BIGNUM'
!  then the point is not a vertex, but rather it signifies the end of a
!  particular polygon.  an implied edge exists between the first and last
!  vertices in each polygon.  a polygon must have at least three vertices.
!  illegal polygons are removed from the internal lists.
!
!  compute trigonometric functions for the angle of rotation.
!
      cosphi = cos(pi180*phi)
      sinphi = sin(pi180*phi)
!
!  first convert from world to virtual coord. if necessary and eliminate
!  any polygons with two or fewer vertices
!
      itail = 1
      ihead = 0
!
!  Allocate another point in the vertex list.
!
      do i = 1, numpts
        ihead = ihead + 1
!
!  a xvert >= 'BIGNUM' is a special flag.
!
        if ( xvert(i) .ge. bignum) then

          xx(ihead) = bignum
          if((ihead-itail) .lt. 2) ihead = itail - 1
          itail = ihead + 1
          go to 120

!
!  convert from world to virtual coord. if input is world coord.
!
        else
!uni            if((iflags .and. 2) .eq. 0) go to 115
                if(ksyand(iflags,2).eq.0) go to 115
                  call scale(xvert(i),yvert(i),xx(ihead),yy(ihead))
                  go to 120

115             continue
                  xx(ihead) = xvert(i)
                  yy(ihead) = yvert(i)

        end if

120     continue

      end do
 
      if((ihead-itail) .lt. 2) ihead = itail - 1
      nvert = ihead
!
!  draw boundaries if desired
!
!uni      if((iflags .and. 1) .eq. 0) go to 138
        if(ksyand(iflags,1) .eq. 0) go to 138
      ihead = 0
      itail = 1
      lmove = .true.

130   continue

      ihead = ihead + 1

      if(ihead .le. nvert) then
        if(xx(ihead) .ne. bignum) go to 135
      end if

133   continue

             call dsdraw(xx(itail),yy(itail))
             itail = ihead + 1
             lmove = .true.
             go to 139
135         continue
            if(lmove) go to 137
             call dsdraw(xx(ihead),yy(ihead))
             go to 139
137         continue
            call dsmove(xx(ihead),yy(ihead))
            lmove = .false.
139         continue
            if(ihead .le. nvert) go to 130
138   continue
!
!  rotate to make shading lines horizontal
!
      ymin = bignum
      ymax = -bignum
      yscale = yres*fact
      yscal2 = 2.0*yscale
      do i = 1, nvert
            if(xx(i) .eq. bignum) go to 140
!
!  perform the rotation to achieve horizontal shading lines.
!
            xv1 = xx(i)
            xx(i) = +cosphi*xv1 + sinphi*yy(i)
            yy(i) = -sinphi*xv1 + cosphi*yy(i)
!
!  convert to integers after scaling, and make vertices odd. in y
!
            yy(i) = 2.0*aint(yscale*yy(i)+0.5)+1.0
            ymin = min (ymin,yy(i))
            ymax = max (ymax,yy(i))
140         continue
      end do
!
!  Make shading start on a multiple of the step size.
!
      step = 2.0*aint(yres*cmspac*fact)
      ymin = aint(ymin/step) * step
      ymax = aint(ymax/step) * step
!
!  after all of the coordinates for the vertices have been pre-processed
!  the appropriate shading lines are drawn.  these are intersected with
!  the edges of the polygon and the visible portions are drawn.
!
      y = ymin
150         continue
            if(y .gt. ymax) go to 250
!
!  initially there are no known intersections.
!
            icount = 0
            ibase = 1
            ivert = 1
160               continue
                  itail = ivert
                  ivert = ivert + 1
                  ihead = ivert
                  if(ihead .gt. nvert) go to 165
                  if(xx(ihead) .ne. bignum) go to 170
!
!  there is an edge from vertex n to vertex 1.
!
165                 ihead = ibase
                    ibase = ivert + 1
                    ivert = ivert + 1
170               continue
!
!  see if the two endpoints lie on
!  opposite sides of the shading line.
!
                  yhead =  y - yy(ihead)
                  ytail =  y - yy(itail)
                  if(yhead*ytail .ge. 0.0) go to 180
!
!  they do.  this is an intersection.  compute x.
!
                  icount = icount + 1
                  delx = xx(ihead) - xx(itail)
                  dely = yy(ihead) - yy(itail)
                  xintcp(icount) = (delx/dely) * yhead + xx(ihead)
180               continue
                  if( ivert .le. nvert ) go to 160
!
!  sort the x intercept values.  use a bubblesort because there
!  aren't very many of them (usually only two).
!
            if(icount .eq. 0) go to 240
            do i = 2, icount
              xkey = xintcp(i)
              k = i - 1
 
              do j = 1, k
                if(xintcp(j) .gt. xkey)then
                  xtemp = xkey
                  xkey = xintcp(j)
                  xintcp(j) = xtemp
                end if
              end do
 
              xintcp(i) = xkey
            end do
!
!  all of the x coordinates for the shading segments along the
!  current shading line are now known and are in sorted order.
!  all that remains is to draw them.  process the x coordinates
!  two at a time.
!
            yr = y/yscal2
            do i = 1, icount, 2
!
!  convert back to virtual coordinates.
!  rotate through an angle of -phi to original orientation.
!  then unscale from grid to virtual coord.
!
                  xv1 = + cosphi*xintcp(i) - sinphi*yr
                  yv1 = + sinphi*xintcp(i) + cosphi*yr
                  xv2 = + cosphi*xintcp(i+1) - sinphi*yr
                  yv2 = + sinphi*xintcp(i+1) + cosphi*yr
!
!  draw the segment of the shading line.
!
                  call dsmove(xv1,yv1)
                  call dsdraw(xv2,yv2)
              end do
240         continue
            y = y + step
            go to 150
250   continue
      return
      end
      subroutine dot
!
!***********************************************************************
!
!! DOT sets the line style to dot.
!  level 1-3, p/s
!
      integer kzdot
      parameter (kzdot=2)
!
      save /clevel/
      save /cline/
!
      common /clevel/ kzlevl,kzbegn
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
 
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzlsty=kzdot
        call dsltyp(kzlsty)
      else
        call errmes('DOT',1,3)
      end if
 
      return
      end
      subroutine drawpq(z,izdim1)
!
!***********************************************************************
!
!! DRAWPQ draws the visible part of segment PC-QC.
!
!  This is the sort of program that gives GO TO statements a bad name!
!
      integer izdim1
!
      real flim(2)
      integer limit(2)
      integer phip
      integer phiq
      integer phia
      real z(izdim1,2)
!
      save /comdp/
!
      common/comdp/xmin,xmax,ymin,ymax,zmin,zmax,axisr(2),plotx,
     &   ploty,pltorg(2),camxyz(3),mx,ny,fmx,fny,camwkg(6),xorg(3),
     &   gx(3),fx(2),kscale,zorg,center(2),pqlmt,
     &   amtx(3,3),focall
!
      equivalence(u,camxyz(1)),(v,camxyz(2)),(w,camxyz(3)),
     &   (mx,limit(1)),(fmx,flim(1))
!
      common/comdpa/pc(3),qc(3),p(3),q(3),enda(6),endb(6),oldq(3),
     &   pw(3),qw(3),t(6),pk(3),qk(3),phip,phiq,phia,ibeam,icolor
!
      kflag=0
      p(1) = pc(1)
      p(2) = pc(2)
      p(3) = pc(3)
      q(1) = qc(1)
      q(2) = qc(2)
      q(3) = qc(3)
!
!  test if p visible
!
2     if (phip .eq. 0) go to 30
!
!  yes, test q
!
7     if (phip*phiq) 10,4,3
!
!  both visible   segment drawable, plot   exit
!
3     kgoto = 0
      go to 300
!
!  q is invisible, find last visible point on segment pq
!
4     jgoto = 1
      go to 200
!
!  give up if not found in maxcut1 bisections
!
5     if(kflag .ne. 0) go to 6
!
!  next point
!
      ibeam = 0
      return
!
!  point found
!
6     q(1) = enda(1)
      q(2) = enda(2)
      q(3) = enda(3)
      go to 3
!
!  gap in segment, find last point to connect p.
!
10    jgoto = 2
      go to 200
!
!  if not found (cannot find point with same visibility fn). try 2nd
!
11    if(kflag .eq. 0) go to 15
!
!  save old q, reset point   plot this piece.
!
      oldq(1) = q(1)
      oldq(2) = q(2)
      oldq(3) = q(3)
      q(1) = enda(1)
      q(2) = enda(2)
      q(3) = enda(3)
!
!  draw first part of segment and come back here
!
      kgoto = 2
      go to 300
!
!  restore q   find lower limit of upper segment.
!  limits for search
!
12    continue
 
      p(1) = q(1)
      p(2) = q(2)
      p(3) = q(3)
      q(1) = oldq(1)
      q(2) = oldq(2)
      q(3) = oldq(3)
!
!  beam off first
!
15    ibeam = 0
      jgoto = 3
      go to 201
!
!  if segment too short, give up.
!
13    if(kflag .eq. 0) then
        return
      end if
!
!  lower end now newly found point.
!
14    p(1) = enda(1)
      p(2) = enda(2)
      p(3) = enda(3)
      go to 3
!
!  p invisible, check q. if invisible, advance.
!
30    ibeam = 0

      if(phiq .eq. 0) then
        return
      end if
!
!  find p
!
      jgoto = 4
      go to 201
!
!  if no point, give up.
!
31    if(kflag.eq.0)then
        return
      else
        go to 14
      end if
!
!  p visible, q invisible, find q.
!  endb = invisible end of interval, enda = visible
!
200   endb(1) = q(1)
      endb(2) = q(2)
      endb(3) = q(3)
      enda(1) = p(1)
      enda(2) = p(2)
      enda(3) = p(3)
!
!  required ivis function
!  in case of gap in segment, consider point visible if its visib.
!  function matches this one and update enda, else endb.
!
      phia = phip
      go to 205
!
!  p invisible, q visible. find p.
!
201   endb(1) = p(1)
      endb(2) = p(2)
      endb(3) = p(3)
      enda(1) = q(1)
      enda(2) = q(2)
      enda(3) = q(3)
      phia = phiq

205   continue
      kflag = 0
!
!  get projected length of segment
!
      pk(1) = xmin + (enda(1)-1.0)*gx(1) - camwkg(1)
      pk(2) = ymin + (enda(2)-1.0)*gx(2) - camwkg(2)
      pk(3) = enda(3)*gx(3) + zorg - camwkg(3)
      call rotate(pk,amtx,enda(4))
      pk(1) = xmin + (endb(1)-1.0)*gx(1) - camwkg(1)
      pk(2) = ymin + (endb(2)-1.0)*gx(2) - camwkg(2)
      pk(3) = endb(3)*gx(3) + zorg - camwkg(3)
      call rotate(pk,amtx,endb(4))
!
!  next step
!
210   continue

      t(1) = (enda(1)+endb(1))/2.0
      t(2) = (enda(2)+endb(2))/2.0
      t(3) = (enda(3)+endb(3))/2.0
      t(4) = (enda(4)+endb(4))/2.0
      t(5) = (enda(5)+endb(5))/2.0
      t(6) = (enda(6)+endb(6))/2.0
      mflag = ivis(t(1),t(2),t(3),z,izdim1)
      if(mflag .eq. phia) go to 220
!
!  not visible, reset invisible end.
!
      endb(1) = t(1)
      endb(2) = t(2)
      endb(3) = t(3)
      endb(4) = t(4)
      endb(5) = t(5)
      endb(6) = t(6)
!
!  check segment length (use max of x, y differences)
!
216   sl = focall* max (abs(enda(4)/enda(6)-endb(4)/endb(6)),
     &    abs(enda(5)/enda(6)-endb(5)/endb(6)))
      if(sl .ge. pqlmt) go to 210
      go to (5,11,13,31), jgoto
!
!  record visible, update enda
!
220   kflag = mflag
      enda(1) = t(1)
      enda(2) = t(2)
      enda(3) = t(3)
      enda(4) = t(4)
      enda(5) = t(5)
      enda(6) = t(6)
      go to 216
!
!  draw p to q
!
!  if beam is on, just move it to q.
!
300   continue
 
!
!  move to p, beam off.
!
      if(ibeam .le. 0) then
        pk(1) = xmin + (p(1)-1.0)*gx(1) - camwkg(1)
        pk(2) = ymin + (p(2)-1.0)*gx(2) - camwkg(2)
        pk(3) = p(3)*gx(3) + zorg - camwkg(3)
        call rotate(pk,amtx,pw)
        pw(1) = (pw(1)/pw(3)-xorg(1))*focall + pltorg(1) + center(1)
        pw(2) = (pw(2)/pw(3)-xorg(2))*focall + pltorg(2) + center(2)
        call gsmove(pw(1),pw(2))
      end if
!
!  Move to q, beam on. beam is left and at point q.
!
      qk(1) = xmin + (q(1)-1.0)*gx(1) - camwkg(1)
      qk(2) = ymin + (q(2)-1.0)*gx(2) - camwkg(2)
      qk(3) = q(3)*gx(3) + zorg - camwkg(3)
      call rotate(qk,amtx,qw)
      qw(1) = (qw(1)/qw(3)-xorg(1))*focall + pltorg(1) + center(1)
      qw(2) = (qw(2)/qw(3)-xorg(2))*focall + pltorg(2) + center(2)
      call gsdraw(qw(1),qw(2))
      ibeam = 1
 
      if(kgoto .ne. 0) go to 12
 
      return
      end
      subroutine dsdraw(x,y)
!
!***********************************************************************
!
!! DSDRAW...
!
      integer gsivis
      integer ivis
      logical lcurnt
      real rx(300,2)
      real ry(300,2)
      real xapos
      real xvpos
      real yapos
      real yvpos
!
      save /gccpos/
      save /gcvpos/
!
      common /gcvpos/ xvpos, yvpos
      common /gccpos/ xapos, yapos, ivis, lcurnt
!
!  check if line goes through any blank area
!
      call zdraw(xvpos,yvpos,x,y,rx,ry,numr)
!
!  draw and update current position
!
      do i=1,numr
 
        call dsmove(rx(i,1),ry(i,1))
        xvpos = rx(i,2)
        yvpos = ry(i,2)
        call gsrst(xvpos,yvpos,x1,y1)
        ivis1 = gsivis(x1,y1)
        call dsdrw2(xapos,yapos,ivis,x1,y1,ivis1)
        xapos = x1
        yapos = y1
        ivis = ivis1
 
      end do
 
      xvpos=x
      yvpos=y
      call gsrst(xvpos,yvpos,x1,y1)
      ivis1=gsivis(x1,y1)
      xapos=x1
      yapos=y1
      ivis=ivis1
 
      return
      end
      subroutine dsdrw2(x0,y0,ivis0,x1,y1,ivis1)
!
!***********************************************************************
!
!! DSDRW2 clips a line to a clipping box.   
!
!  pass on only visible line 
!  segments to dsdrw3 to be drawn in the current line type.   this routine also
!  worries about whether the graphics device will require a "move"
!  before the "draw" is done.
!
      logical linilt
      logical lposnd
      logical ldid1
!
      save /dcltyp/
      save /gcclip/
!
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
!
      if(ksyand(ivis0,ivis1) .ne. 0) return
 
      if(ivis0 .ne. 0) then
        lposnd = .false.
        linilt = .true.
      end if
!
!  calculate the number of clips necessary
!
      nclips = 0
      if(ivis0 .ne. 0) nclips = 1
      if(ivis1 .ne. 0) nclips = nclips + 1
!
!  line totally visible, just draw it
!
      if(nclips .eq. 0) then
        call dsdrw3(x0,y0,x1,y1)
        return
      end if
!
!  find the intersection(s) with the clipping box edges
!
      ldid1 = .false.
      ist = 1
      dx = x1-x0
      if(dx .eq. 0.0) ist = 3
      ifn = 4
      dy = y1-y0
      if(dy .eq. 0.0) ifn = 2
      if(ist .gt. ifn) return
      ivisc = ksyor(ivis0,ivis1)
      ibit = 2**(ist-1)
 
      do i = ist, ifn
 
        if(ksyand(ivisc,ibit) .eq. 0) go to 200
 
        if(i .le. 2) then
          xi = xcm0
          if(i .eq. 2) xi = xcm1
          yi = y0 + (xi-x0)*dy/dx
          if(yi .lt. ycm0 .or. yi .gt. ycm1) go to 200
        else
          yi = ycm0
          if(i .eq. 4) yi = ycm1
          xi = x0 + (yi-y0)*dx/dy
          if(xi .lt. xcm0 .or. xi .gt. xcm1) go to 200
        end if
!
!  got an intersection.   if it's the only one, the draw the line.
!
        if (nclips .le. 1) then
 
          if(ivis0 .ne. 0) then
            call dsdrw3(xi,yi,x1,y1)
          else
            call dsdrw3(x0,y0,xi,yi)
          end if
 
          return
 
        end if
!
!  two clips necessary.   if we already have one, draw the double clipped
!  line, else save first clip and wait for last.
!  note, if double clipped, it doesn't matter in which direction it
!  is drawn.
!
        if (ldid1) then
          call dsdrw3(x2,y2,xi,yi)
          return
        end if
 
        x2 = xi
        y2 = yi
        ldid1 = .true.
 
200     continue
 
        ibit = 2*ibit
 
      end do
!
!  segment is not visible if we drop thru to here
!
      return
      end
      subroutine dsdrw3(x0,y0,x1,y1)
!
!***********************************************************************
!
!! DSDRW3 draws a line from (x0,y0) to (x1,y1) in absolute coordinates.
!
!  assumes that clipping has already been done.   to suppress unnecessary
!  "moves", this is the only routine that should call gsdrvr(3,,,).
!  the line is drawn in the current line type.   this routine does not
!  set the absolute position (xapos,yapos).   it is up to the caller to
!  do so if necessary.
!
      logical linilt
      logical lposnd
!
      save /cline/
      save /dcltyp/
!
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
!
      if (ilntyp .le. 1) then
 
        if(.not. lposnd) then
          call gsdrvr(3,x0,y0)
        end if
!
!  ??? is "go to 220" equivalent to a return???
!
!  maybe not...try these two lines...
!
        call gsdrvr(4,x1,y1)
        lposnd = .true.
!
!  end of dubious insertion.
!
        return
 
      end if
!
!  segment line to make current line type
!
      if(.not. lposnd) call gsdrvr(3,x0,y0)
      if(.not. linilt) go to 100
      inxtl = 1
      dleft = dist(1,ilntyp-1)
      linilt = .false.
!  if(.not. lposnd) call gsdrvr(3,x0,y0)
100   dx = x1-x0
      dy = y1-y0
      dl = sqrt(dx**2+dy**2)
!
!  see if this segment is shorter that dist. left on line type
!
      if(dl .le. dleft) go to 200
!
!  segment is longer, so advance to line type break
!
      s = dleft/dl
      x0 = s*dx+x0
      y0 = s*dy+y0
!
!  see if this part of the line type is drawn or skipped
!
      if(ksyand(inxtl,1) .eq. 0) then
        call gsdrvr(3,x0,y0)
      else
        call gsdrvr(4,x0,y0)
      end if
!
!  now go to next portion of line type
!
      inxtl = inxtl + 1
      if(inxtl .gt. dist(13,ilntyp-1)) inxtl = 1
      dleft = dist(inxtl,ilntyp-1)
      go to 100
!
!  draw last of line if drawn
!
200   continue
 
      dleft = dleft - dl
 
      if(ksyand(inxtl,1) .eq. 0) then
        lposnd = .false.
      else
        call gsdrvr(4,x1,y1)
        lposnd = .true.
      end if
 
      return
      end
      subroutine dsfill(x,y,n,tx,ty)

!***********************************************************************
!
!! DSFILL provides polygon fill support.
!
      parameter (kzmaxc=255)
      parameter (kzmxbs=1000)
      parameter (kzmxbl=200)

      integer gsivis
      logical lblank
      logical linilt
      logical lposnd
      real tx(n)
      real ty(n)
      real x(n)
      real y(n)

      save /cblank/
      save /colorn/
      save /dcltyp/
      save /gcdchr/
      save /gcdprm/

      common /cblank/ zzblnk(kzmxbl,4),zzblks(kzmxbs,4),kzblcn,kzbscn
      common /colorn/ kzshd,zzhspc,zzcolr(kzmaxc,3),kznclr,kzccol
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &       ndclrs, idvbts, nfline, xclipd, yclipd
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd

      if(n .lt. 3) return
!
!  convert to absolute coord.
!
      do i=1,n
        call gsrst(x(i),y(i),tx(i),ty(i))
      end do
 
      call minmax(ty,n,ymin,ymax)
      call minmax(tx,n,xmin,xmax)
!
!  if clipping needed or if no hardware polygon fill, use software
!
      if((gsivis(xmin,ymin) .ne. 0) .or.
     &   (gsivis(xmax,ymax) .ne. 0) .or.
     &   (ksyand(idvbts,256) .eq. 0)) go to 900
!
!  if polygon overlaps any blank area, do software fill
!
      if(kzbscn.gt.0)then
 
        do j=1,kzbscn
 
          do i=1,n-1
            call zfill(x(i),y(i),x(i+1),y(i+1),zzblnk(j,1),
     &             zzblnk(j,2),zzblnk(j,3),zzblnk(j,4),lblank)
            if(lblank) go to 900
          end do
 
          call zfill(x(1),y(1),x(n),y(n),zzblnk(j,1),
     &           zzblnk(j,2),zzblnk(j,3),zzblnk(j,4),lblank)
          if(lblank) go to 900
 
        end do
 
      end if
 
      if(kzblcn.gt.4)then
 
        do j=5,kzblcn
 
          do i=1,n-1
            call zfill(x(i),y(i),x(i+1),y(i+1),zzblnk(j,1),
     &             zzblnk(j,2),zzblnk(j,3),zzblnk(j,4),lblank)
            if(lblank) go to 900
          end do
 
          call zfill(x(1),y(1),x(n),y(n),zzblnk(j,1),
     &           zzblnk(j,2),zzblnk(j,3),zzblnk(j,4),lblank)
          if(lblank) go to 900
 
        end do
 
      end if
 
      do j=1,4
 
        if(zzblnk(j,1).gt.-1000.0)then
 
          do i=1,n-1
            call zfill(x(i),y(i),x(i+1),y(i+1),zzblnk(j,1),
     &             zzblnk(j,2),zzblnk(j,3),zzblnk(j,4),lblank)
            if(lblank) go to 900
          end do
 
          call zfill(x(1),y(1),x(n),y(n),zzblnk(j,1),
     &           zzblnk(j,2),zzblnk(j,3),zzblnk(j,4),lblank)
          if(lblank) go to 900
        end if
 
      end do
!
!  if can handle concave polygons, just call driver
!
      if((ksyand(idvbts,512) .eq. 0) .or.
     &   (n .eq. 3)) go to 150
!
!  if here, driver can handle convex non-intersecting polygons only,
!  so make sure this polygon is convex and non-self-intersecting.
!
      dx1 = x(1)-x(n)
      dy1 = y(1)-y(n)
!
!  old non-zero delta-y
!
      dy = dy1
!
!  number of times delta-y changes sign
!
      nchngs = 0
      l = 1
      costh = 0.0
110   continue
!
!  convexity test
!
        dx2 = x(l+1)-x(l)
        dy2 = y(l+1)-y(l)
        a = dx1*dy2-dx2*dy1
        if(a*costh .lt. 0.0) go to 900
        if(costh .eq. 0.0) costh = a
!
!  self intersection check - relys on "convexity" check
!
        if(dy .ne. 0.0) go to 120
          dy = dy2
          go to 130
120     continue
 
          if(dy2*dy .lt. 0.0) then
            dy = dy2
            nchngs = nchngs + 1
            if(nchngs .ge. 3) go to 900
          end if
 
130       continue
            dx1 = dx2
            dy1 = dy2
            l = l + 1
            if(l .lt. n) go to 110
150   continue
      call gsdrvr(1024+n,tx,ty)
      return
!
!  software fill, fill with solid lines
!
900   continue
!
!  locate long side and do cross hatch in same direction
!
      dx=x(1)-x(2)
      dy=y(1)-y(2)
      rlen=dx*dx+dy*dy
      dx1=x(2)-x(3)
      dy1=y(2)-y(3)
      rlen1=dx1*dx1+dy1*dy1
 
      if(rlen1.gt.rlen)then
        dy=dy1
      end if
 
      if(abs(dy).lt.0.000001)then
        angle=0.0
      else
        angle=90.0
      end if
 
      call dhatch(x,y,n,angle,zzhspc,1,tx,ty)
 
      return
      end
      function dslens(istrng)
!
!***********************************************************************
!
!! DSLENS returns the length in virtual coordinates of a string.
!
!  The current character size is assumed.
!
      parameter (maxfnt = 18)
      parameter (mxstrk = maxfnt*9000)
!
      integer bwidth
      integer bxy
      character*1 cstr(160)
      integer istrng(*)
!
      save /gccpar/
      save /gcfont/
!
      common /gccpar/ csize, ccos, csin
      common /gcfont/ icfnsl, mxslot,
     &   islfnt(maxfnt), ihight(maxfnt),
     &   indx(95*maxfnt+1), bwidth(95*maxfnt), bxy(mxstrk)
!
      if(icfnsl .eq. 1)then
 
        dslens = 9.0*leng(istrng)*csize
 
      else
 
        call zin2ch(istrng,cstr)
        dslens = 0.0
        ioff = 95*(icfnsl-2) - 32
 
        do i=1,leng(istrng)
 
          jchar = ichar(cstr(i))
 
          if(jchar .le. 32 .or. jchar .ge. 128)then
            jchar = 65
          elseif(bwidth(jchar+ioff) .le. 0)then
            jchar = 65
          end if
 
          iwidth = bwidth(jchar+ioff)
          dslens = dslens + csize*iwidth
        end do
 
      end if
 
      return
      end
      subroutine dsltyp(itype)
!
!***********************************************************************
!
!! DSLTYP sets the current line type.
!
      save /cline/
      save /dcltyp/
!
      logical linilt
      logical lposnd
!
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
!
      ilntyp = itype
      if(ilntyp .le. 0 .or. (ilntyp .gt. kzlncn+1)) ilntyp = 1
      linilt = .true.

      return
      end
      subroutine dsmove(x,y)
!
!***********************************************************************
!
!! DSMOVE moves to the point (x,y).
!
!  input: x,y - coordinate of point to move to
!
      integer gsivis
      logical linilt, lposnd
      logical lcurnt
!
      save /dcltyp/
      save /gccpos/
      save /gcvpos/
!
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
      common /gcvpos/ xvpos, yvpos
      common /gccpos/ xapos, yapos, ivis, lcurnt
!
!  reset line style to beginning of pattern and show moved
!
      lposnd = .false.
!
!  transform virtual coord. to absolute coord.
!
      xvpos = x
      yvpos = y
      call gsrst(xvpos,yvpos,xapos,yapos)
      ivis = gsivis(xapos,yapos)
      return
      end
      subroutine dspstr(istrng)
!
!***********************************************************************
!
!! DSPSTR strokes out a character string.
!
!  The character string is a byte array with 0 as a terminator, and is
!  stroked at the current position.
!
      integer istrng(*)
      character*1 cstrng(80)
      logical linilt, lposnd
!
      save /dcltyp/
      save /gccoff/
      save /gcvpos/
!
      common /gcvpos/ xvpos, yvpos
      common /gccoff/ xoff, yoff
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
!
!  convert input to character array
!
      call zin2ch(istrng,cstrng)
!
!  don't draw characters in linetypes
!
      iold = ilntyp
      ilntyp = 1
      nbyte = 0
 100  nbyte = nbyte + 1
!
!  save the (0,0) position of the character
!
      xoff = xvpos
      yoff = yvpos
!
!  get the character to stroke
!
      iichar = ichar(cstrng(nbyte))
!
!  stroke the character
!
      if (iichar.ne.0)then
        call dsstrk(iichar)
        go to 100
      end if
!
!  return line type to that of before
!
      ilntyp = iold
      return
      end
      subroutine dsstrk(khar)
!
!***********************************************************************
!
!! DSSTRK strokes out a character.
!
      parameter (maxfnt = 18)
      parameter (mxstrk = maxfnt*9000)
!
      integer bwidth, bxy
      logical lmove
!
      save /gcfont/
!
      common /gcfont/ icfnsl, mxslot,
     &   islfnt(maxfnt), ihight(maxfnt),
     &   indx(95*maxfnt+1), bwidth(95*maxfnt), bxy(mxstrk)
!
!  space fill all non-printing and non-defined with space size of
!  a capital "a".
!
      jchar = (khar-32) + 95*(icfnsl-1)
      if(khar .le. 32 .or. khar .ge. 128) go to 800
      if(icfnsl .gt. 1)then
        if(bwidth(jchar-95) .le. 0) go to 800
      end if
!
!  stroke this character
!
      index = indx(jchar)
      idone = indx(jchar+1)
!
!  first position is an assumed move
!
      lmove = .true.
!
!  get the scaled and rotated next position on the character
!
100   continue
150   if(bxy(index) .ne. -64) go to 160
      lmove = .true.
      index = index + 1
      go to 100
 
160   x=bxy(index)
      y=bxy(index+1)
      call gscclc(x,y,dx,dy)
      index = index + 2
      if(lmove)then
        call dsmove(dx,dy)
      else
        call dsdraw(dx,dy)
      end if
      lmove = .false.
      if(index .lt. idone) go to 100
!
!  all done with the character, move to next character position
!
200   continue
 
      if(icfnsl .eq. 1)then
        width = 9.0
      else
        width = bwidth(jchar-95)
      end if
 
      call gscclc(width,0.0,dx,dy)
      call dsmove(dx,dy)
      return
!
!  use capital "a" for size of space and all non-printing and non-defined
!
800   continue

      jchar = (65-32) + 95*(icfnsl-1)
      go to 200
      end
      subroutine dsymbo(isymno,symsiz)
!
!***********************************************************************
!
!! DSYMBO places a symbol at the current location with a given size.
!
!  internal variables:
!  symmov - (no. of moves) array of consecutive x,y locations to
!  which line is drawn
!  isymst - (no. of symbols + 1) array start of symbol move locations
!
!  notes:
!  cjw 7/86 new symbol definitions:
!  0>  square
!  1>  octagon
!  2>  triangle
!  3>  plus sign
!  4>  'X'
!  5>  diamond
!  6>  upside down triangle
!  7>  square with an 'X' in it
!  8>  'X' with a horizontal line across it
!  9>  diamond with a plus sign in it
!  10>  octagon with a plus sign in it
!  11>  double hour glass
!  12>  square with a plus sign in it
!  13>  octagon with a 'X' in it
!  14>  square with a triangle in it
!  15>  pentagon with a plus sign in it
!  16>  pentagon
!  17>  five pointed star
!  18>  square with a diamond in it
!
      logical linilt
      logical lposnd
      real symmov(105)
      integer isymst(0:18,4)
!
      save symmov,isymst,nsym
      save /dcltyp/
      save /gcvpos/
!
      common /gcvpos/ xvpos, yvpos
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
!
! square: (1)
! octagon: (12)
! triangle: (31)
! up side down triangle: (40)
! horizontal line: (49)
! vertical line: (54)
! reversed slash: (59)
! slash: (64)
! diamond: (69)
! star: (80)
! pentagon: (93)
!
      data symmov/
     & 0.5,-0.5, 0.5,0.5,  -0.5,0.5,  -0.5,-0.5,  0.5,-0.5,  1000.0,
     & 0.2071,-0.5, 0.5,-0.2071, 0.5,0.2071, 0.2071,0.5, -0.2071,0.5,
     & -0.5,0.2071, -0.5,-0.2071, -0.2071,-0.5, 0.2071,-0.5, 1000.0,
     & 0.5,-0.366,  0.0,0.5,  -0.5,-0.366,  0.5,-0.366, 1000.0,
     & 0.5,0.366, 0.0,-0.5,  -0.5,0.366,  0.5,0.366, 1000.0,
     & -0.5,0.0,  0.5,0.0,  1000.0,
     & 0.0,0.5,  0.0,-0.5,  1000.0,
     & -0.5,0.5,  0.5,-0.5, 1000.0,
     & -0.5,-0.5,  0.5,0.5,  1000.0,
     & 0.5,0.0,  0.0,0.5,  -0.5,0.0,  0.0,-0.5,  0.5,0.0,  1000.0,
     & 0.5,0.2, -0.5,0.2, 0.3,-0.5, 0.0,0.5, -0.3,-0.5,
     & 0.5,0.2, 1000.0,
     & 0.5,0.0,  0.0,0.5,  -0.5,0.0,  -0.25,-0.5,  0.25,-0.5,
     & 0.5,0.0, 1000.0/
!
      data isymst/
     &  1,12,31,54,64,69,40,59,49,69,12,40,49,12, 1,93,93,80,69,
     &  0, 0, 0,49,59, 0, 0,64,64,54,54,31,54,64,31,54, 0, 0, 1,
     &  0, 0, 0, 0, 0, 0, 0, 1,59,49,49, 0, 1,59, 0,49, 0, 0, 0,
     &  1, 1, 1, 2, 2, 1, 1, 3, 3, 3, 3, 2, 3, 3, 2, 3, 1, 1, 2/
!
      data nsym /18/
!
!  save current location
!
      x0 = xvpos
      y0 = yvpos
!
!  draw symbol in proper size
!
      ioltyp=ilntyp
      ilntyp=1
 
      if(isymno.ge.0  .and.  isymno.le.nsym)then
 
        do line=1,isymst(isymno,4)
          iptr = isymst (isymno,line)
          call dsmove(x0+symsiz*symmov(iptr),
     &                  y0+symsiz*symmov(iptr+1))
          do i=1,12
            iptr=iptr+2
            if(symmov(iptr).gt.999.0) go to 200
            call dsdraw(x0+symsiz*symmov(iptr),
     &                  y0+symsiz*symmov(iptr+1))
          end do
  200   continue
        end do
 
      end if
 
      call dsmove(x0,y0)
      ilntyp=ioltyp
 
      return
      end
      subroutine duplex
!
!***********************************************************************
!
!! DUPLEX sets the character type to duplex.
!
!  level 1-3, p/s
!
      character czalfl*1,czalfn*5,czalfs*5
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
 
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfs='DUPLE'
      else
        call errmes('DUPLEX',1,3)
      end if
 
      return
      end
      subroutine endsub
!
!***********************************************************************
!
!! ENDSUB terminates the current subplot.
!  level 1-3,change to 1
!
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        call zinits
      else
        call errmes('ENDSUB',1,3)
      end if
 
      return
      end
      subroutine entoff(ientry)
!
!***********************************************************************
!
!! ENTOFF deactivates an entry in the legend block.
!
!  input:   ientry = entry id
!
!
!
      parameter (kzno=222)
!
      save /clevel/
      save /clgndn/
!
      common /clevel/ kzlevl,kzbegn
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        if(ientry.gt.50)then
          call errmes('ENTOFF',ientry,4)
        else
          kzlgen(ientry)=kzno
        end if
      else
        call errmes('ENTOFF',1,3)
      end if
 
      return
      end
      subroutine enton(ientry)
!
!***********************************************************************
!
!! ENTON reactivates an entry in the legend block.
!
!  input:   ientry = entry id
!
      parameter (kzyes=111)
!
      save /clevel/
      save /clgndn/
!
      common /clevel/ kzlevl,kzbegn
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        if(ientry.gt.50)then
          call errmes('ENTON',ientry,4)
        else
          kzlgen(ientry)=kzyes
        end if
      else
        call errmes('ENTON',1,3)
      end if
      return
      end
      subroutine errmes(str,level1,level2)
!
!***********************************************************************
!
!! ERRMES writes an error message.
!
!  input:   str    = name of calling routine
!  level1 = correct level 1 of calling
!  level2 = correct level 2 of calling, or
!  error flag
!
      parameter (kzcspl=3)
      parameter (kzpspl=4)
      parameter (kzpply=5)
!
      character*6 str1
      character*(*) str
!
      save /carea/
      save /cdevic/
      save /clevel/
      save /cline/
!
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /clevel/ kzlevl,kzbegn
!
!  incorrect level
!
 10   format(' ### error detected in routine -',a6,'-')

      write(*,*)' '
      if(str(1:5).ne.'BLANK'.or.level2.le.4)then
        write(*,10)str
      end if
 
      if(level2.eq.0)then
        write(*,20)level1
 20     format(' ### not called at level',i2)
      elseif(level2.le.3)then
        write(*,30)(i,i=level1,level2)
 30     format(' ### not called at levels',5i2)
      elseif(str.eq.'ENTON')then
        if(level2.eq.4)then
          write(*,35)level1
 35       format(' ### error in variable, ientry =',i5)
        end if
      elseif(str.eq.'SCALOG')then
        write(*,*)'### range too small.'
      elseif(str.eq.'SETSUB')then
        if(level2.eq.4)then
          write(*,50)zzxaxs
 50       format(' ### x-axis length =',f10.2)
        elseif(level2.eq.5)then
          write(*,60)zzyaxs
 60       format(' ### y-axis length =',f10.2)
        end if
      elseif(str(1:5).eq.'BLANK')then
        str1(1:5)=str(1:5)
        str1(6:6)=char(level1+48)
        write(*,10)str1
        write(*,*)'### iframe too large'
      elseif(str.eq.'BLANKA')then
        write(*,*)'### maximum number of blank area exceeded'
      elseif(str(1:5).eq.'CURVE')then
        if(level2.eq.4)then
          write(*,*)' ### number of points less than or equal to 3'
          if(kzlsty.eq.kzcspl)then
            write(*,*)' ### no cubspl interpolation done'
          elseif(kzlsty.eq.kzpspl)then
            write(*,*)' ### no parametric cubspl interpolation done'
          elseif(kzlsty.eq.kzpply)then
            write(*,*)' ### no polynomial interpolation done'
          end if
        elseif(level2.eq.5)then
          write(*,*)' ### number of blank symbols exceeds 1000'
        else
          write(*,*)'curve reports an error.'
        end if
      elseif(str.eq.'ENTOFF')then
        if(level2.eq.4)then
          write(*,115)level1
 115      format(' ### error in variable, ientry =',i5)
        end if
      elseif(str.eq.'HEADIN')then
        if(level2.eq.4)then
          write(*,160)level1
 160      format(' ### error in variable, npaklin =',i5)
        elseif(level2.eq.5)then
          write(*,170)level1
 170      format(' ### line number',i2,' is truncated')
        end if
      elseif(str.eq.'HRDROT')then
        write(*,180)level1
 180    format(' ### illegal value in variable, str =',a5)
      elseif(str.eq.'HRDSCL')then
        write(*,180)level1
      elseif(str.eq.'HRDDEF')then
        write(*,190)level1
 190    format(' ### illegal value in variable, func =,', a5)
      elseif(str.eq.'TXTBLK')then
        write(*,*)' ### maximum paklin for use of txtblk exceeded'
      elseif(str.eq.'PAKLIN')then
        if(level2.eq.4)then
          write(*,*)'### error in array size or line size'
        elseif(level2.eq.5)then
          write(*,170)level1
        end if
      elseif(str.eq.'MAXLIN')then
        if(level2.eq.4)then
          write(*,210)level1
 210      format(' ### illegal line length =',i6)
        elseif(level2.eq.5)then
          write(*,*)'### array is too small or too large'
        end if
      elseif(str.eq.'LEGHDG')then
        write(*,230)level1
 230    format(' ### txtblk bgnsub exceeds allowable length,',i3,
     &    /' ### and is truncated')
      elseif(str(1:4).eq.'PAGE')then
        write(*,240)level1
 240    format(' ### page is called at level',i2,',',
     &    /' ### not set for current plot')
      elseif(str(1:5).eq.'RESET')then
        write(*,250)level1
 250    format(' ### error in resetting -',a6,' -')
      elseif(str.eq.'BGNSUB')then
        if(level2.eq.4)then
          write(*,50)zzxaxs
        elseif(level2.eq.5)then
          write(*,60)zzyaxs
        elseif(level2.eq.6)then
          write(*,260)level1
 260      format(' ### x-axis label exceeds allowable length,'
     &    ,i3, /' ### and is truncated')
        elseif(level2.eq.7)then
          write(*,270)level1
 270      format(' ### y-axis label exceeds allowable length,'
     &    ,i3, /' ### and is truncated')
        elseif(level2.eq.8)then
          write(*,280)level1
 280      format(' ### plot bgnsub exceeds allowable length,'
     &    ,i3, /' ### and is truncated')
        end if
      elseif(str.eq.'TK4100')then
        write(*,285)level1
 285    format(' ### device ',i4,' not supported at this time,',
     &    /' ### assumed to be a tek 4107')
      elseif(str.eq.'XTRLNX')then
        write(*,260)level1
      elseif(str.eq.'XTRLGX')then
        write(*,260)level1
      elseif(str.eq.'XLABEL')then
        write(*,260)level1
      elseif(str.eq.'XTRLNY')then
        write(*,270)level1
      elseif(str.eq.'XTRLGY')then
        write(*,270)level1
      elseif(str.eq.'YLABEL')then
        write(*,270)level1
      elseif(str.eq.'ZMAPIT')then
        if(level2.eq.4)then
          write(*,290)level1
 290      format(' ### parameter for xmarks has an illegal',
     &      ' value of ',i8)
        elseif(level2.eq.5)then
          write(*,300)level1
 300      format(' ### parameter for ymarks has an illegal',
     &      ' value of ',i8)
        end if
      end if
 
      write(*,*)'current level=',kzlevl
 
      return
      end
      subroutine extrma(xv,yv,zv,xa,xb,ya,yb,ierr)
!
!***********************************************************************
!
!! EXTRMA...
!
      real flim(2)
      integer limit(2)
      real xs(3)
      real xc(3)
!
      save /comdp/
!
      common/comdp/xmin,xmax,ymin,ymax,zmin,zmax,axisr(2),plotx,
     &   ploty,pltorg(2),camxyz(3),mx,ny,fmx,fny,camwkg(6),xorg(3),
     &   gx(3),fx(2),kscale,zorg,center(2),pqlmt,
     &   amtx(3,3),focall
      equivalence(u,camxyz(1)),(v,camxyz(2)),(w,camxyz(3)),
     &   (mx,limit(1)),(fmx,flim(1))
!
      xs(1) = xv
      xs(2) = yv
      xs(3) = zv
 
      call rotate(xs,amtx,xc)
!
!  quit if point is behind camera
!
      if(xc(3).le.0.0)then
        ierr=-1
        return
      end if
 
      xc(1) = xc(1)/xc(3)
      xc(2) = xc(2)/xc(3)
      xa = min(xa,xc(1))
      xb = max(xb,xc(1))
      ya = min(ya,xc(2))
      yb = max(yb,xc(2))
      ierr = 0
 
      return
      end
      subroutine fcontr(z,nz,iz,mx,my,x1,xmx,y1,ymy,nl,cl)
!
!***********************************************************************
!
!! FCONTR produces a contour plot of a function F(X,Y) stored as a table.
!
!    z(i,j) = f(x(i),y(j)).   
!
!  it is assumed that a call to "mapit" has already been made to establish 
!  the coordinate axis (x,y), with x limits covering the range
!  x1 to xmx, and y limits covering the range y1 to ymy.
!
!  fast version, for use with crts only
!
!  arguments:
!
!  input
!
!  z  * type: real array.
!  * the values of the function to contour:
!  z(i,j) = f(xi,yj) where:
!  xi = x1 + (i-1)*(xmx-x1)/(mx-1)
!  yj = y1 + (j-1)*(ymx-y1)/(my-1)
!
!  nz  * type: integer constant or variable.
!  * the first dimension of the array z - not necessarily
!  equal to mx, but mx <= nz.
!
!  iz  * type: anything - a dummy for compatibility
!  * not used!!!
!
!  mx   * type: integer constant or variable.
!  * the number of x grid points.
!
!  my  * type: integer constant or variable.
!  * the number of y grid points.
!
!  x1  * type: real constant or variable.
!  * the minimum x value.
!
!  xmx  * type: real constant or variable.
!  * the maximum x value.
!
!  y1  * type: real constant or variable.
!  * the minimum y value.
!
!  ymy  * type: real constant or variable.
!  * the maximum y value.
!
!  nl  * type: integer constant or variable.
!  * the number of contour levels.
!
!  cl  * type: real array.
!  * the coutour levels to draw.   (same units as
!  f() or z().)
!
      integer my
      integer nl
      integer nz
!
      real cl(nl)
      real z(nz,my)
      real zb(4)
      real zmax
      real zmin
!
      save /contrf/
!
      common /contrf/ x0,y0,dx,dy
!
!  calc. some scaling constants needed
!
      dx = (xmx-x1)/(mx-1)
      dy = (ymy-y1)/(my-1)
      x0 = x1-dx
      y0 = y1-dy
!
!  move thru array looking for contour segments in each box.
!
      do j=1,my-1
 
        j2 = j+1
        zb(3) = z(1,j2)
        zb(4) = z(1,j)
 
        do i=1,mx-1
 
          i2 = i+1
          zb(1) = zb(4)
          zb(2) = zb(3)
          zb(3) = z(i2,j2)
          zb(4) = z(i2,j)
!
!  test for all points equal -- skip if true
!
          if( zb(1) .eq. zb(2) .and. zb(1) .eq. zb(3)
     &      .and. zb(1) .eq. zb(4) ) goto 90
!
!  find extremes of box
!
          zmin = 1.0e30
          zmax = -zmin
          do l=1, 4
            if ( zb(l) .lt. zmin ) zmin = zb(l)
            if ( zb(l) .gt. zmax ) zmax = zb(l)
          end do
!
!  if a contour falls within the box, plot it.
!
          do k=1,nl
            if( cl(k) .ge. zmin .and. cl(k) .le. zmax ) then
              call fsgmnt(i,j,zb,cl(k))
            end if
          end do
 
90        continue
 
        end do
 
      end do
 
      return
      end
      subroutine finplt
!
!***********************************************************************
!
!! FINPLT releases the graphic device.
!  (level 1-3, change to 0)
!
      save /clevel/
      save /cdevic/
      save /gcdsel/
!
      common /clevel/ kzlevl,kzbegn
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /gcdsel/ idev
!
      character*11 nplot
!
      if(idev .ne. 0) then
        call gsdrvr(6,dummy,dummy)
      end if
 
      idev = 0
      kzlevl=0
 
      if(kznplt.eq.1)then
        write(*,*)'1 plot completed.'
      else
        write(*,*)nplot,' plots completed.'
      end if
 
      return
      end
      subroutine frame
!
!***********************************************************************
!
!! FRAME draws a frame around plot area.
!  (level 2,3)
!
      real zzin
      parameter (zzin=2.54)
!
      save /carea/
      save /clevel/
      save /cpage/
      save /cunit/
!
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /clevel/ kzlevl,kzbegn
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
!
!  check level
!
      if((kzlevl.eq.2).or.(kzlevl.eq.3))then
 
        delta=0.01*zzin*zzpagr
        numfrm=int(zzfrme/delta+0.99)
        offset=(zzfrme-delta)/2.0
        x1=zzxlft+offset
        x2=zzxrgt-offset
        y1=zzybot+offset
        y2=zzytop-offset
 
        do i=0,numfrm-1
 
          del=delta*i
          tx1=x1-del
          tx2=x2+del
          ty1=y1-del
          ty2=y2+del
          call dsmove(tx1,ty1)
          call dsdraw(tx2,ty1)
          call dsdraw(tx2,ty2)
          call dsdraw(tx1,ty2)
          call dsdraw(tx1,ty1)
        end do
 
      else
 
        call errmes('FRAME',2,3)
 
      end if
 
      return
      end
      subroutine frmwid(thknes)
!
!***********************************************************************
!
!! FRMWID sets the frame thickness.
!  (level 1-3, p/s)
!
!  input:   thknes = thickness of frame in inches
!
      save /carea/
      save /clevel/
      save /cpage/
      save /cunit/
!
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /clevel/ kzlevl,kzbegn
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
!
      if(kzlevl.eq.1)then
        uufrme=thknes*zzunit
      elseif(kzlevl.eq.2.or.kzlevl.eq.3)then
        uufrme=thknes*zzunit
        zzfrme=uufrme*zzpagr
      else
        call errmes('FRMWID',1,3)
      end if
 
      return
      end
      subroutine fsgmnt(ix,jy,zb,clevel)
!
!***********************************************************************
!
!! FSGMNT looks for a contour segment.
!
!  The search is in the box defined by
!  the points (ix,jy,zb1), (ix,jy+1,zb2), (ix+1,jy+1,zb3)
!  and (ix+1,jy,zb4).   if found, the segment is drawn.
!
      real clevel
      integer ioff(4)
      integer joff(4)
      logical lfirst
      real zb(4)
!
      save /contrf/
      save ioff
      save joff
!
      common /contrf/ x0,y0,dx,dy
!
      data ioff /0,0,1,1/
      data joff /0,1,1,0/
!
      lfirst = .true.
      iprevs = 4
      zold = zb(iprevs)
      zdiff = clevel - zold
 
      do i=1,4
 
        znew = zb(i)
        diff = clevel - znew
 
        if(sign(1.0,zdiff) .eq. sign(1.0,diff)) go to 90
 
        temp = znew-zold
 
        if(temp .eq. 0.0) then
          pctchg = 0.0
        else
          pctchg = zdiff/temp
        end if
 
        x = ix + ioff(iprevs) + (ioff(i)-ioff(iprevs))*pctchg
        y = jy + joff(iprevs) + (joff(i)-joff(iprevs))*pctchg
        call scale(x*dx+x0,y*dy+y0,vx,vy)
 
        if(.not.lfirst) then
          call gsdraw(vx,vy)
          lfirst = .true.
        else
          call gsmove(vx,vy)
          lfirst = .false.
        end if
 
        zdiff = diff
        zold = znew
        iprevs = i
!
!  ???  is this where statement 90 was?
!
90      continue
 
      end do
 
      return
      end
      subroutine fulmap
!
!***********************************************************************
!
!! FULMAP sets the plotting area to the whole screen.
!
      save /gcdprm/
!
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
!
      call pltbox(vxl,vxh,vyl,vyh)
 
      return
      end
      subroutine gdlgi(ifxn,xa,ya)
!
!***********************************************************************
!
!! GDLGI is the CGM generator interface.
!
      parameter (maxpts=20)
!
      real dchar(8)
      real xa(8)
      real xray(maxpts)
      real ya(3)
      real yray(maxpts)
!
      save dchar
      save iptcnt
      save xlast
      save xray
      save ylast
      save yray
!
!  make nice names for the devices resolution in x and y
!  ("xgupcm" is x graphics units per centimeter)
!
      equivalence (dchar(4),xgupcm), (dchar(5),ygupcm)
      data dchar /7777.0,27.94,21.59,200.0,200.0,4095.0,127.0,1.0/
!
!  first verify we got a graphics function we can handle
!
      if(ifxn .le. 0 .or. ifxn .gt. 9) return
!
!  now dispatch to the proper code to handle that function
!
      go to (100,200,300,400,500,600,700,800,900) ifxn
!
!  initialize the device
!
  100 continue
!
!  initialize the cgm generator
!
      ya(1)=0.0
      xlast=-1
      ylast=-1
      iptcnt=0
      return
!
!  get fresh plotting surface
!
  200 continue
!
!  make sure that all of the primitives have been flushed before
!  we end the picture
!
      if(iptcnt.gt.1)then
        call plylin(iptcnt,xray,yray)
        iptcnt=0
      end if
 
      call newfrm()
      call linclr(1)
      return
!
!  move
!
  300 continue
!
!  convert cm. to virtual coordinates and then convert the
!  virtual coordinates to cgm coordinates
!
      xnow = xa(1) * 0.035791
      ynow = ya(1) * 0.035791
!
!  try to eliminate any moves that are not necessary
!
      if((xnow.ne.xlast).or.(ynow.ne.ylast))then
!
!  if we have a polyline already, get rid of it
!
        if(iptcnt.gt.1)then
          call plylin(iptcnt,xray,yray)
          iptcnt=0
        end if
!
!  store the move coordinate in the first position
!
        iptcnt=1
        xray(iptcnt)=xnow
        yray(iptcnt)=ynow
        xlast=xnow
        ylast=ynow
 
      end if
      return
!
!  draw
!
  400 continue
      xlast = xa(1) * 0.035791
      ylast = ya(1) * 0.035791
!
! if there are already maxpts points stored, get rid of them
!
      if(iptcnt.ge.maxpts)then
        call plylin(iptcnt,xray,yray)
        iptcnt=1
        xray(iptcnt)=xray(maxpts)
        yray(iptcnt)=yray(maxpts)
      end if
 
      iptcnt=iptcnt+1
      xray(iptcnt)= xlast
      yray(iptcnt)= ylast
      return
!
!  flush graphics command buffer
!
  500 continue
      return
!
!  release the device
!
  600 continue
!
!  if a polyline is active, end it
!
      if(iptcnt.gt.1)then
        call plylin(iptcnt,xray,yray)
      end if
!
!  if a picture is active, end it
!
      call grfcls()
      return
!
!  return device characteristics
!
!  dchar(1) = device id
!  (2) = x length in cm
!  (3) = y length in cm
!  (4) = x resolution per cm
!  (5) = y resolution per cm
!  (6) = device colors
!  (7) = idvbts -- device bits
!  (8) = nfline
!
  700 continue
 
      do i=1,8
        xa(i) = dchar(i)
      end do
 
      return
!
!  select current drawing color
!
  800 continue
      return
!
!  perform graphics input
!
  900 continue
      return
      end
      subroutine gdlsdb
!
!***********************************************************************
!
!! GDLSDB writes out the characters in a buffer.
!
      parameter (ibfsiz=80)
      character buffer
!
      save /gdlsr/
!
      common /gdlsr/ nxtchr, buffer(ibfsiz)
!
      if(nxtchr .eq. 1) return
      write (*,'(132a)') (buffer(i), i=1,nxtchr-1)
      nxtchr = 1

      return
      end
      subroutine gdlsin(string)
!
!***********************************************************************
!
!! GDLSIN...
!
      integer ibfsiz
      parameter (ibfsiz=80)
!
      character buffer
      integer i
      integer l
      integer numchr
      character*(*) string
!
      save /gdlsr/
!
      common /gdlsr/ nxtchr, buffer(ibfsiz)
!
      l = numchr(string)
      if((nxtchr+l) .gt. ibfsiz) call gdlsdb
 
      do i = 1, l
        buffer(nxtchr) = string(i:i)
        nxtchr = nxtchr + 1
      end do
 
      return
      end
      subroutine gdlsob
!
!***********************************************************************
!
!! GDLSOB initializes the buffer to zero.
!
      parameter (ibfsiz=80)
!
      character buffer
!
      save /gdlsr/
!
      common /gdlsr/ nxtchr, buffer(ibfsiz)
!
      nxtchr = 1

      return
      end
      subroutine gdpost(ifxn,xa,ya)
!
!***********************************************************************
!
!! GDPOST is the PostScript driver.  
!
!  The hard copy device has 300 dots/inch
!
      real botmar
      real dpi
      real lftmar
      real psres
      real rtmar
      real topmar
!
      parameter (dpi = 300.0)
      parameter (psres = 72.0)
      parameter (lftmar = 0.5)
      parameter (rtmar = 0.25)
      parameter (topmar = 0.5)
      parameter (botmar = 0.25)
      parameter (paphgt = 11.0)
      parameter (papwid = 8.5)
!
!  derived parameters
!
      parameter (usewid = papwid-lftmar-rtmar)
      parameter (usehgt = paphgt-topmar-botmar)
      parameter (widcm = 2.54*usewid)
      parameter (hgtcm = 2.54*usehgt)
      parameter (res = dpi/2.54)
      parameter (prescm = psres/2.54)
      parameter (xoff = psres*lftmar)
      parameter (yoff = psres*botmar)
      parameter (maxpnt = 900)
!
      character*15 coord
      character*1 eof
      real dchar(8)
      logical lnoplt
      logical lwide
      real xa(8)
      real ya(3)
!
!  make nice names for the devices res in x and y
!  ("xgupcm" is x graphics units per centimeter)
!
      equivalence (dchar(4),xgupcm), (dchar(5),ygupcm)
!
!  diglib device characteristics words
!
      save dchar,coord
!
      data dchar /910.0, widcm, hgtcm, res, res, 1.0, 27.0, 4.0/
      data coord /'               '/
!
      eof = char(4)
      lwide = .false.
10    continue
!
!  first verify we got a graphics function we can handle
!
      if(ifxn .le. 0 .or. ifxn .gt. 7) return
!
!  now dispatch to the proper code to handle that function
!
      go to (100,200,300,400,500,600,700) ifxn
!
!  ifxn=1, initialize the device.
!
100   continue
 
      lun = int(xa(1))
!
!  show initialization worked, i.e. we opened the file.
!
      ya(1) = 0.0
      call gdpsob
      call gdpsib
 
      call gdpsin('%!PS-Adobe-2.0')
      call gdpsdb
      call gdpsin('Erasepage Initgraphics 1 Setlinecap 1 Setlinejoin ')
      call gdpsin('/M {moveto} def /L {lineto} def ')
      call gdpsdb
 
190   continue
      lnoplt = .true.
      npoint = 0
      return
!
!  get fresh plotting surface
!
200   continue
      if(.not. lnoplt)then
        call gdpsin('Stroke Showpage ')
      end if
      call gdpsin('Newpath ')
      go to 190
!
!  ****
!  move
!  ****
!
300   continue
!
!  draw
!
400   continue
      npoint = npoint + 1
      if(npoint .gt. maxpnt)then
        call gdpsin('Stroke Newpath ')
        if(ifxn .eq. 4)then
          call gdpsin(coord)
          call gdpsin('M ')
        end if
        npoint = 1
      end if
 
      if(lwide)then
        x = prescm*ya(1)+xoff
        y = prescm*(hgtcm-xa(1))+yoff
      else
        x = prescm*xa(1)+xoff
        y = prescm*ya(1)+yoff
      end if
 
      write(coord,451) x,y
451   format(f6.1,1x,f6.1,1x)
      coord(15:15) = ' '
      call gdpsin(coord)
 
      if(ifxn .eq. 3)then
        call gdpsin('M ')
      else
        call gdpsin('L ')
      end if
 
      lnoplt = .false.
      return
!
!  flush graphics command buffer
!
500   continue
      return
!
!  release the device
!
600   continue
      if(.not. lnoplt)then
        call gdpsin('Stroke Showpage ')
        call gdpsin(eof)
        call gdpsin(' ')
        call gdpsdb
      end if
!w      close (unit=lun)
      return
!
!  return device characteristics
!
700   continue
 
      do i=1,8
        xa(i) = dchar(i)
      end do
 
      if(lwide)then
        xa(2) = dchar(3)
        xa(3) = dchar(2)
      end if
      return
!
!  handle file open error
!
      entry gdpswd(ifxn,xa,ya)
!
!***********************************************************************
!
!! GDPSWD changes the PostScript device from landscape to portrait
!
      lwide = .true.

      go to 10
      end
      subroutine gdpsdb
!
!***********************************************************************
!
!! GDPSDB dumps the buffer
!
      parameter (ibfsiz =80)
      character*1 buffer
!
      save /gdlsr/
!
      common /gdlsr/ nxtchr, buffer(ibfsiz)
!
      if (nxtchr .gt. 1) then
        write(*,'(132a1)') (buffer(i), i=1,nxtchr-1), char(13)
      end if

      nxtchr = 1
 
      return
      end
      subroutine gdpsib
!
!***********************************************************************
!
!! GDPSIB initializes the buffer
!
      parameter (ibfsiz =80)
      character*1 buffer
!
      save /gdlsr/
!
      common /gdlsr/ nxtchr, buffer(ibfsiz)
!
      nxtchr = 1
 
      return
      end
      subroutine gdpsin(string)
!
!***********************************************************************
!
!! GDPSIN inserts data into the buffer
!
      character*(*) string
      parameter (ibfsiz =80)
      character*1 buffer
!
      save /gdlsr/
!
      common /gdlsr/ nxtchr, buffer(ibfsiz)
!
      l = len(string)
 
      if((nxtchr+l) .gt. ibfsiz) then
        call gdpsdb
      end if
 
      do i = 1, l
        buffer(nxtchr) = string(i:i)
        nxtchr = nxtchr + 1
      end do
 
      return
      end
      subroutine gdpsob
!
!***********************************************************************
!
!! GDPSOB opens the buffer.
!
      integer ibfsiz
      parameter (ibfsiz=80)
!
      character*1 buffer
      integer nxtchr
!
      save /gdlsr/
!
      common /gdlsr/ nxtchr, buffer(ibfsiz)
!
      nxtchr = 1
 
      return
      end
      subroutine gdqmsl(ifxn,xa,ya)
!
!***********************************************************************
!
!! GDQMSL is the QMS 800 and 1200 laser printer driver.
!
!  - multiple commands, single line
!
      character*13 coord
      real dchar(8)
      logical lnopl
      logical lpenup
      logical lwide
      real xa(8)
      real ya(8)
!
!  make nice names for the devices resolution in x and y
!  ("xgupcm" is x graphics units per centimeter)
!
      equivalence (dchar(4),xgupcm), (dchar(5),ygupcm)
!
      save dchar
      save nplot
!
      data dchar /1200.0, 27.94, 21.59, 118.11, 118.11, 1.0, 27.0, 3.0/
      data nplot / 0 /
!
      lwide = .true.
   10 continue
!
!  first verify we got a graphics function we can handle
!
      if(ifxn .le. 0 .or. ifxn .gt. 7) return
!
!  now dispatch to the proper code to handle that function
!
      go to (100,200,300,400,500,600,700) ifxn
!
!  initialize the device
!
  100 continue

      lun = int(xa(1))
      ya(1) = 0.0

  190 continue

      call gdlsob
      nplot = nplot + 1
      call gdlsin('^py^-')
      call gdlsdb
      call gdlsin('^iol^ij00000^it00000^ib08000^f^igv^pw03')
      call gdlsdb
      lnopl = .true.
      lpenup = .false.
      return
!
!  get fresh plotting surface
!
  200 continue
      if(lnopl) return
      call gdlsdb
      go to 190
!
!  move
!
  300 continue
      if(lpenup) go to 450
      lpenup = .true.
      coord(1:2) = '^u'
      go to 450
!
!  draw
!
  400 continue
      if(.not. lpenup) go to 450
      coord(1:2) = '^d'
      lpenup = .false.
  450 continue
      if(lwide)then
         ix = int((10.0*xgupcm*xa(1)/3.0)+0.5)
         iy = int((10.0*ygupcm*(dchar(3)-ya(1))/3.0)+0.5)
      else
         ix = int((10.0*xgupcm*ya(1)/3.0) + 0.5)
         iy = int((10.0*ygupcm*xa(1)/3.0) + 0.5)
      end if
      write (coord(3:13),451) ix,iy
  451 format(i5.5,':',i5.5)
      call gdlsin(coord)
      lnopl = .false.
      return
!
!  flush graphics command buffer
!
  500 continue
      call gdlsdb
      call gdlsin('^,')
      call gdlsdb
      return
!
!  release the device
!
  600 continue
      call gdlsin('^ige^o^iop^ij00500^it01200^ib10500')
      call gdlsin('^pn^-')
      call gdlsdb
      return
!
!  return device characteristics
!
  700 continue
 
      do i = 1,8
        xa(i) = dchar(i)
      end do
 
      if(.not. lwide)then
         xa(2) = dchar(3)
         xa(3) = dchar(2)
      end if
      return
!
!  handle file open error
!
 9000 continue
      ya(1) = 3.0
      return
 
      entry gdqmsp(ifxn,xa,ya)
!
!***********************************************************************
!
!! GDQMSP...
!
      lwide = .false.
      go to 10
      end
      function goodcs(approx)
!
!***********************************************************************
!
!! GOODCS calculates a minimum readable character size for a given device.
!
      save /gcdchr/
      save /gcdprm/
!
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
!
      size = (gschit()/yres)/ys
!
!  now scale up this minimum size so that characters are about
!  the size desired.
!
      n = int(approx/size + 0.25)
!
!  must be at least n=1
!
      if(n .eq. 0) n=1
!
!  now return our answer
!
      goodcs = n*size
 
      return
      end
      subroutine gothic
!
!***********************************************************************
!
!! GOTHIC sets the character type to gothic.
!  level 1-3, p/s
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs

      character czalfl*1,czalfn*5,czalfs*5

      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfs='GOTHI'
      else
        call errmes('GOTHIC',1,3)
      end if
 
      return
      end
      subroutine grafin(x,y,lflag)
!
!***********************************************************************
!
!! GRAFIN displays and reads the graphics cursor.
!
!  It returns its position in world coordinates.
!
      logical lflag
      logical logx, logy
!
      save /pltcom/
      save /pltsiz/
!
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
!  get cursor position in virtual coordinates.
!
      call gscrsr(x,y,lflag,ierr)
      if(ierr .ne. 0) return

      x = (x-xvstrt)*udx/xvlen + ux0
      if(logx) x = 10.0**x

      y = (y-yvstrt)*udy/yvlen + uy0
      if(logy) y = 10.0**y

      return
      end
      subroutine grid(ix,iy)
!
!***********************************************************************
!
!! GRID draws grid lines.
!  (level 3)
!
!  input:   ix,iy = number of grid lines to be drawn
!  per axes step size, if less than
!  than zero, it is the number of
!  steps per line.
!
      integer kzlog
      parameter (kzlog=3)
!
      integer maxgrd
      parameter (maxgrd=200)
!
      save /carea/
      save /caxis/
      save /ccoord/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
      common /ccoord/ zzxmin,zzxmax,zzxstp,zzymin,zzymax,zzystp
!
      if(kzlevl.eq.3)then
!
!  draw border around plot area
!
        call zframe(zzxlft,zzxrgt,zzybot,zzytop,0.0)
!
!  x grid lines
!
        if(ix.ne.0)then
          icount=1
!
!  log x
!
          if(kzxtyp.eq.kzlog)then
            delta=alog10(zzxstp)
            idel=nint(delta)
!
!  calculate exponential increment
!
            if(zzxmin.le.0.0)then
              wx=0.0
            else
              wx=alog10(zzxmin)
            end if
!
!  calculate line density
!
            if(ix.gt.0)then
              nsub=ix
              imod=1
            else
              nsub=1
              imod=-ix
            end if
 
            ngrid=int((alog10(zzxmax)-wx)/delta)
!
!  if number of grid lines exceeds maxgrd,
!  cut it down to a reasonable number
!
            max=(ngrid+1)*9*nsub*idel
            jmod=max/maxgrd+1
            if(jmod.gt.imod) imod=jmod
 
            do i=1,ngrid+1
 
              div=10.0**wx
!
!  process cycle by cycle, 'idel' is
!  the number of cycle per axis label
!
              do m=1,idel
 
                ipwr=nint(wx+m)
                diff=(10.0**ipwr-10.0**(ipwr-1))/9.0
!
!  9 grid lines per cycle
!
                do j=1,9
 
                  subdiv=div
                  div=div+diff
!
!  increase line density
!
                  if(nsub.gt.1)then
                    diff1=diff/nsub
 
                    do l=1,nsub-1
 
                      icount=icount+1
                      subdiv=subdiv+diff1
 
                      if(mod(icount,imod).eq.0)then
                        call scale(subdiv,zzymin,vx,vy)
                        if(vx.gt.zzxrgt) go to 600
                        call dsmove(vx,zzybot)
                        call dsdraw(vx,zzytop)
                      end if
 
                    end do
 
                  end if
 
                  icount=icount+1
 
                  if(mod(icount,imod).eq.0)then
                    call scale(div,zzymin,vx,vy)
                    if(vx.gt.zzxrgt) go to 600
                    call dsmove(vx,zzybot)
                    call dsdraw(vx,zzytop)
                  end if
 
                end do
 
              end do
 
              wx=wx+delta
 
            end do
!
!  linear x
!
          else
 
            if(ix.gt.0)then
               delta=zzxstp/ix
            else
              delta=-zzxstp*ix
            end if
 
            wx=zzxmin
            ngrid=int((zzxmax-zzxmin)/delta)
            imod=ngrid/maxgrd+1
 
            do i=1,ngrid
 
              wx=wx+delta
              icount=icount+1
 
              if(mod(icount,imod).eq.0)then
                call scale(wx,zzymin,vx,vy)
                call dsmove(vx,zzybot)
                call dsdraw(vx,zzytop)
              end if
 
            end do
 
          end if
 
        end if
!
!  y grid lines
!
 600    continue
 
        if(iy.ne.0)then
!
!  log y
!
          icount=1
 
          if(kzytyp.eq.kzlog)then
            delta=alog10(zzystp)
            idel=nint(delta)
!
!  calculate exponential increment
!
            if(zzymin.le.0.0)then
              wy=0.0
            else
              wy=alog10(zzymin)
            end if
!
!  calculate line density
!
            if(iy.gt.0)then
              nsub=iy
              imod=1
            else
              nsub=1
              imod=-iy
            end if
!
!  if number of grid lines exceeds maxgrd,
!  cut it down to a reasonable number
!
            ngrid=int((alog10(zzymax)-wy)/delta)
            max=(ngrid+1)*9*nsub*idel
            jmod=max/maxgrd+1
            if(jmod.gt.imod) imod=jmod
 
            do i=1,ngrid+1
 
              div=10.0**wy
!
!  process cycle by cycle, 'idel' is
!  the number of cycle per axis label
!
              do m=1,idel
                ipwr=nint(wy+m)
                diff=(10.0**ipwr-10.0**(ipwr-1))/9.0
!
!  9 grid lines per cycle
!
                do j=1,9
                  subdiv=div
                  div=div+diff
                  if(nsub.gt.1)then
                  diff1=diff/nsub
!
!  increase line density
!
                    do l=1,nsub-1
 
                      icount=icount+1
                      subdiv=subdiv+diff1
 
                      if(mod(icount,imod).eq.0)then
                        call scale(zzxmin,subdiv,vx,vy)
                        if(vy.gt.zzytop) go to 1200
                        call dsmove(zzxlft,vy)
                        call dsdraw(zzxrgt,vy)
                      end if
 
                    end do
 
                  end if
 
                  icount=icount+1
 
                  if(mod(icount,imod).eq.0)then
                    call scale(zzxmin,div,vx,vy)
                    if(vy.gt.zzytop) go to 1200
                    call dsmove(zzxlft,vy)
                    call dsdraw(zzxrgt,vy)
                  end if
 
                end do
 
              end do
 
              wy=wy+delta
 
            end do
!
!  linear y
!
          else
            if(iy.gt.0)then
              delta=zzystp/iy
            else
              delta=-zzystp*iy
            end if
            wy=zzymin
            ngrid=int((zzymax-zzymin)/delta)
            imod=ngrid/maxgrd+1
 
            do i=1,ngrid
 
              wy=wy+delta
              icount=icount+1
 
              if(mod(icount,imod).eq.0)then
                call scale(zzxlft,wy,vx,vy)
                call dsmove(zzxlft,vy)
                call dsdraw(zzxrgt,vy)
              end if
 
            end do
 
          end if
 
        end if
 
 1200   continue
!
!  if not at right level
!
      else
        call errmes('GRID',3,0)
      end if
 
      return
      end
      subroutine gscclc(x,y,dx,dy)
!
!***********************************************************************
!
!! GSCCLC does the character sizing and rotation.
!
      real ccos
      real csin
      real csize
      real dx
      real dy
      real x
      real xoff
      real xs
      real y
      real yoff
      real ys
!
      save /gccoff/
      save /gccpar/
!
      common /gccpar/ csize, ccos, csin
      common /gccoff/ xoff, yoff
!
      xs = x*csize
      ys = y*csize
      dx = ccos*xs + csin*ys + xoff
      dy = ccos*ys - csin*xs + yoff
 
      return
      end
      subroutine gsccmp(ix,iy,xoff,yoff,x,y)
!
!***********************************************************************
!
!! GSCCMP...
!
      logical lcurnt
!
      save /gccpar/
      save /gccpos/
!
      common /gccpar/ csize, ccos, csin
      common /gccpos/ xapos, yapos, ivis, lcurnt
!
!  scale to proper size
!
      xs = csize*ix
      ys = csize*iy
!
!  rotate and translate
!
      x = ccos*xs + csin*ys + xoff
      y = ccos*ys - csin*xs + yoff
 
      return
      end
      function gschit()
!
!***********************************************************************
!
!! GSCHIT...
!
      parameter (maxfnt = 18)
      parameter (mxstrk = maxfnt*9000)
!
      integer bwidth
      integer bxy
!
      save /gcfont/
!
      common /gcfont/ icfnsl, mxslot,
     &   islfnt(maxfnt), ihight(maxfnt),
     &   indx(95*maxfnt+1), bwidth(95*maxfnt), bxy(mxstrk)
!
      gschit = ihight(icfnsl)
 
      return
      end
      subroutine gscolr(icolor,ierr)
!
!***********************************************************************
!
!! GSCOLOR selects color "icolor" on current device.
!
      save /gcdchr/
!
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd
!
      logical lnobkg
      ierr = 0
!
!  lnobkg set to true if no background color exists on this device
!
      lnobkg = ksyand(idvbts,4) .eq. 0
!
!  first, error if background color requested and device does not
!  support background color write.
!
      if(icolor .eq. 0 .and. lnobkg) then
        ierr=-1
        return
      end if
!
!  second, error if color requested is larger than the number of
!  foreground colors available on this device
!
      if(icolor .gt. ndclrs) then
        ierr=-1
        return
      end if
!
!  if only 1 foreground color and no background color, then
!  driver will not support set color, and of course, the
!  color must be color 1 to have gotten this far, so just return
!
      if(ndclrs .eq. 1 .and. lnobkg) return
!
!  all is ok, so set the requested color
!
      call gsdrvr(8,float(icolor),dummy)
 
      return
      end
      subroutine gscrsr(x,y,ibutn,ierr)
!
!***********************************************************************
!
!! GSCRSR tries to get graphic input from the currently selected device.   
!
!  if the device is not capable of it, ierr=-1, else ierr=0 and:
!  x = x position of cursor in virtual coordinates
!  y = y position of cursor in virtual coordinates
!  ibutn = new button state
!
      real array(3)
!
      save /gcdchr/
      save /gcdprm/
!
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
!
!  see if device supports cursor
!
      if(ksyand(idvbts,1024) .eq. 0) go to 900
!
!  now ask for cursor from device driver
!
      call gsdrvr(12,array,dummy)
!
!  convert absolute cm. coord. to virtual coordinates
!
      call gsirst(array(2),array(3),x,y)
!
!  get button state
!
      ibutn = int(array(1))
120   continue
      ierr = 0
      return
!
!  device doesn't support gin
!
900   ierr = -1
      return
      end
      subroutine gsdlns(iltype,on1,off1,on2,off2)
!
!***********************************************************************
!
!! GSDLNS defines the line style.
!
      logical linilt
      logical lposnd
!
      save /gcltyp/
!
      common /gcltyp/ ilntyp, dleft, dist(4,3), linilt, lposnd
!
      if(iltype .lt. 2 .or. iltype .gt. 4) return
      index = iltype-1
      dist(1,index) = on1
      dist(2,index) = off1
      dist(3,index) = on2
      dist(4,index) = off2
 
      return
      end
      subroutine gsdnam(idev,bname)
!
!***********************************************************************
!
!! GSDNAM returns the name of the specified device device.
!
      character*(*) bname
      integer idev
!
      if(idev .eq. 1) then
        bname ='Tek. 4014'
      else if(idev .eq. 2) then
        bname ='Tek. 4115b'
      else if(idev .eq. 3) then
        bname ='QMS-L'
      else if(idev .eq. 4) then
        bname ='QMS-P'
      else if(idev .eq. 5) then
        bname ='VT-240'
      else if(idev .eq. 6) then
        bname ='Tek. 4510'
      else if(idev .eq. 7) then
        bname ='Sel. 100xl'
      else if(idev .eq. 8) then
        bname ='PC (st-240)'
      else if(idev .eq. 9) then
        bname ='Vector file'
      else if(idev .eq.10) then
        bname ='LN03 plus - l'
      else if(idev .eq.11) then
        bname ='LN03 plus - p'
      else if(idev .eq.12) then
        bname ='Tek. 4107'
      else if(idev .eq.13) then
        bname ='Film recorder lab'
      else if(idev .eq.20) then
        bname ='Arcgraph system'
      else if(idev .eq.21) then
        bname ='CGMLIB'
      else
        bname = 'Undefined device!'
      end if
 
      return
      end
      subroutine gsdraw(x,y)
!
!***********************************************************************
!
!! GSDRAW...
!
      integer gsivis
      logical lcurnt
!
      save /gccpos/
      save /gcvpos/
!
      common /gcvpos/ xvpos, yvpos
      common /gccpos/ xapos, yapos, ivis, lcurnt
!
!  transform virt. coor. to screen coord.
!
      xvpos = x
      yvpos = y
      call gsrst(xvpos,yvpos,x1,y1)
      ivis1 = gsivis(x1,y1)
      call gsdrw2(xapos,yapos,ivis,x1,y1,ivis1)
      xapos = x1
      yapos = y1
      ivis = ivis1
 
      return
      end
      subroutine gsdrgb(icolor,red,grn,blu,ierr)
!
!***********************************************************************
!
!! GSDRGB defines an RGB color.
!
      real rgb(3)
 
      save /gcdchr/
!
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd
!
      if( ksyand(idvbts,64) .eq. 0 .or.
     &   (icolor .gt. ndclrs) .or.
     &   (icolor .lt. 0)) then
        ierr=-1
        return
      end if
 
      ierr = 0
      rgb(1) = red
      rgb(2) = grn
      rgb(3) = blu
      call gsdrvr(10,float(icolor),rgb)
 
      return
      end
      subroutine gsdrvr(ifxn,x,y)
!
!***********************************************************************
!
!! GSDRVR...
!
!  note: to add new device, please make sure the new devid is
!  different from those of existing devices.
!
      external blkdat
!
      integer maxdev
      parameter (maxdev=21)
!
      save /gcdsel/
!
      common /gcdsel/ idev
!
!  see if device exists
!
      if(idev .gt. 0 .and. idev .le. maxdev) go to 10
!
!  non-existant device, so see if user is just inquiring
!
      if(ifxn .ne. 7) return
!
!  return device type equal zero if inquiring about
!  non-existant device.
!
      x = 0.0
      return
!
!  dispatch to the proper driver
!
   10 continue
!  devid
!  -------
!  device 1 is tek. 4014                         4014.0
!  device 2 is tek. 4115                         4115.0
!  device 3 is QMS laser - landscape             1200.0
!  device 4 is QMS laser - portrait              1200.0
!  device 5 is vt-240                             240.0
!  device 6 is tek. 4510 rasterizer              4510.0
!  device 7 is selanar hirez                      100.0
!  device 8 is pc (smarterm-240 emulation)         24.0
!  device 9 is vector save                       9999.0
!  device 10 is ln03 plus laser - landscape         3.0
!  device 11 is ln03 plus laser - portrait          3.0
!  device 12 is tek. 4107                        4107.0
!  device 13 is film recorder lab                  35.0
!  device 14 is postscript - landscape            910.0
!  device 15 is postscript - portrait             910.0
!  device 20 is local graphics interface         7777.0
!
!  if(idev .eq. 1) call gd4014(ifxn,x,y)
!  if(idev .eq. 2) call gd4115(ifxn,x,y)
      if(idev .eq. 3) call gdqmsl(ifxn,x,y)
      if(idev .eq. 4) call gdqmsp(ifxn,x,y)
!  if(idev .eq. 5) call gdv240(ifxn,x,y)
!  if(idev .eq. 6) call gd4510(ifxn,x,y)
!  if(idev .eq. 7) call gdslnr(ifxn,x,y)
!  if(idev .eq. 8) call gdst24(ifxn,x,y)
!  if(idev .eq. 9) call gdsave(ifxn,x,y)
!  if(idev .eq.10) call gdln3l(ifxn,x,y)
!  if(idev .eq.11) call gdln3p(ifxn,x,y)
!  if(idev .eq.12) call gd4107(ifxn,x,y)
!  if(idev .eq.13) call gdfrlb(ifxn,x,y)
      if(idev .eq.14) call gdpost(ifxn,x,y)
      if(idev .eq.15) call gdpswd(ifxn,x,y)
!  if(idev .eq.13) call pcega(ifxn,x,y)
      if(idev .eq.20) call gdlgi(ifxn,x,y)
!
      return
      end
      subroutine gsdrw2(x0,y0,ivis0,x1,y1,ivis1)
!
!***********************************************************************
!
!! GSDRW2 clips a line to clipping box.   
!
!  pass on only visible line segments to
!  gsdrw3 to be drawn in the current line type.   this routine also
!  worries about whether the graphics device will require a "move"
!  before the "draw" is done.
!
      save /gcclip/
      save /gcltyp/
!
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      logical linilt, lposnd
      common /gcltyp/ ilntyp, dleft, dist(4,3), linilt, lposnd
!
      logical ldid1
!
      if(ksyand(ivis0,ivis1) .ne. 0) return
      if(ivis0 .eq. 0) go to 10
       lposnd = .false.
       linilt = .true.
10    continue
!
!  calculate the number of clips necessary
!
      nclips = 0
      if(ivis0 .ne. 0) nclips = 1
      if(ivis1 .ne. 0) nclips = nclips + 1
      if(nclips .ne. 0) go to 100
!
!  line totally visible, just draw it
!
      call gsdrw3(x0,y0,x1,y1)
      return
!
!  find the intersection(s) with the clipping box edges
!
100   continue
      ldid1 = .false.
      ist = 1
      dx = x1-x0
      if(dx .eq. 0.0) ist = 3
      ifn = 4
      dy = y1-y0
      if(dy .eq. 0.0) ifn = 2
      if(ist .gt. ifn) return
      ivisc = ksyor(ivis0,ivis1)
      ibit = 2**(ist-1)
 
      do i = ist, ifn
      if(ksyand(ivisc,ibit) .eq. 0) go to 200
      if(i .gt. 2) go to 110
       xi = xcm0
       if(i .eq. 2) xi = xcm1
       yi = y0 + (xi-x0)*dy/dx
       if(yi .lt. ycm0 .or. yi .gt. ycm1) go to 200
       go to 120
110       continue
       yi = ycm0
       if(i .eq. 4) yi = ycm1
       xi = x0 + (yi-y0)*dx/dy
       if(xi .lt. xcm0 .or. xi .gt. xcm1) go to 200
120   continue
!
!  got an intersection.   if it's the only one, then draw the line.
!
      if(nclips .gt. 1) go to 140
       if(ivis0 .eq. 0) go to 130
        call gsdrw3(xi,yi,x1,y1)
        return
130       continue
        call gsdrw3(x0,y0,xi,yi)
        return
140       continue
!
!  two clips necessary.   if we already have one, draw the double clipped
!  line, else save first clip and wait for last.
!  note, if double clipped, it doesn't matter in which direction it
!  is drawn.
!
       if(.not. ldid1) go to 180
        call gsdrw3(x2,y2,xi,yi)
        return
180        continue
        x2 = xi
        y2 = yi
        ldid1 = .true.
200    continue
      ibit = 2*ibit
      end do
!
!  segment is not visible if we drop thru to here
!
      return
      end
      subroutine gsdrw3(x0,y0,x1,y1)
!
!***********************************************************************
!
!! GSDRW3 draws a line from (x0,y0) to (x1,y1) in absolute coordinates.
!
!  assumes that clipping has already been done.   to suppress unnecessary
!  "moves", this is the only routine that should call gsdrvr(3,,,).
!  the line is drawn in the current line type.   this routine does not
!  set the absolute position (xapos,yapos).   it is up to the caller to
!  do so if necessary.
!
      logical linilt, lposnd
!
      save /gcltyp/
!
      common /gcltyp/ ilntyp, dleft, dist(4,3), linilt, lposnd
!
      if(ilntyp .gt. 1) go to 50
      if(.not. lposnd) call gsdrvr(3,x0,y0)
      go to 220
!
!  segment line to make current line type
!
50    continue
      if(.not. linilt) go to 100
      inxtl = 1
      dleft = dist(1,ilntyp-1)
      linilt = .false.
      if(.not. lposnd) call gsdrvr(3,x0,y0)
100   dx = x1-x0
      dy = y1-y0
      dl = sqrt(dx**2+dy**2)
!
!  see if this segment is shorter that dist. left on line type
!
      if(dl .le. dleft) go to 200
!
!  segment is longer, so advance to line type break
!
      s = dleft/dl
      x0 = s*dx+x0
      y0 = s*dy+y0
!
!  see if this part of the line type is drawn or skipped
!
      if(ksyand(inxtl,1) .ne. 0) go to 120
       call gsdrvr(3,x0,y0)
       go to 140
120       continue
       call gsdrvr(4,x0,y0)
140   continue
!
!  now go to next portion of line type
!
      inxtl = inxtl + 1
      if(inxtl .gt. 4) inxtl = 1
      dleft = dist(inxtl,ilntyp-1)
      go to 100
!
!  draw last of line if drawn
!
200   continue
      dleft = dleft - dl
      if(ksyand(inxtl,1) .ne. 0) go to 220
       lposnd = .false.
       go to 240
220       continue
       call gsdrvr(4,x1,y1)
       lposnd = .true.
240   continue
      return
      end
      subroutine gsetdp(angle,xscale,yscale,xtran,ytran)
!
!***********************************************************************
!
!! GSETDP ...
!
      real pio180
      parameter (pio180=1.745329252e-2)
!
      save /gcdprm/
!
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
!
!  set scale and translation factors
!
      xs = xscale
      ys = yscale
      xt = xtran
      yt = ytran
!
!  set rotation factors
!
      rad = -angle*pio180
      rcos = cos(rad)
      rsin = sin(rad)
      return
      end
      subroutine gsfill(x,y,n,tx,ty)

!***********************************************************************
!
!! GSFILL fills a polygon.
!
      integer n

      integer gsivis
      logical left
      logical linilt
      logical lposnd
      real tx(n)
      real ty(n)
      real x(n)
      real xins(40)
      real y(n)
 
      save /gcdchr/
      save /gcdprm/
      save /gcltyp/

      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
      common /gcltyp/ ilntyp, dleft, dist(4,3), linilt, lposnd

      save fact

      data fact /16.0/
!
!  define arithmetic statement function to mapping vertices
!
      ymap(yyy) = 2.0*aint(yscale*yyy+0.5)+1.0
!
      if(n .lt. 3) return
!
!  convert to absolute coord.
!
      do i=1,n
        call gsrst(x(i),y(i),tx(i),ty(i))
      end do
 
      call minmax(ty,n,ymin,ymax)
      call minmax(tx,n,xmin,xmax)
!
!  if clipping needed or if no hardware polygon fill, use software
!
      if((gsivis(xmin,ymin) .ne. 0) .or.
     &   (gsivis(xmax,ymax) .ne. 0) .or.
     &   (ksyand(idvbts,256) .eq. 0)) go to 200
!
!  if can handle concave polygons, just call driver
!
      if((ksyand(idvbts,512) .eq. 0) .or.
     &   (n .eq. 3)) go to 150
!
!  if here, driver can handle convex non-intersecting polygons only,
!  so make sure this polygon is convex and non-self-intersecting.
!
      dx1 = x(1)-x(n)
      dy1 = y(1)-y(n)
!
!  dy = dy1  @old non-zero delta-y
!
      dy = dy1
!
!  nchngs = 0  @number of times delta-y changes sign
!
      nchngs = 0
      l = 1
      costh = 0.0
110   continue
!
!  convexity test
!
       dx2 = x(l+1)-x(l)
       dy2 = y(l+1)-y(l)
       a = dx1*dy2-dx2*dy1
       if(a*costh .lt. 0.0) go to 200
       if(costh .eq. 0.0) costh = a
!
!  self intersection check - relys on "convexity" check
!
       if(dy .ne. 0.0) go to 120
        dy = dy2
        go to 130
120    continue
       if(dy2*dy .ge. 0.0) go to 130
        dy = dy2
        nchngs = nchngs + 1
        if(nchngs .ge. 3) go to 200
130    continue
       dx1 = dx2
       dy1 = dy2
       l = l + 1
       if(l .lt. n) go to 110
150   continue
      call gsdrvr(1024+n,tx,ty)
      return
!
!  software fill
!
200   continue
!
!  filling a polygon is very simple if and only if the vertices of
!  the polygon never lie on a scan line.   we can force this to happen
!  by the following trick: make all vertices lie just barely above
!  the scan line they should lie on.   this is done by mapping the
!  vertices to a grid that is "fact" times the device resolution,
!  and then doubling the grid density, and offsetting the vertices
!  by 1.   because we do this, we must outline the polygon.
!
!  fill with solid lines
!
      linold = ilntyp
      ilntyp = 1
!
      left = .true.
      yscale = ys*yres*fact
      dlines = 2.0*fact
      call minmax(y,n,ymin,ymax)
      ymin = aint(ymap(ymin)/dlines)*dlines+dlines
      ymax = aint(ymap(ymax)/dlines)*dlines
      yscan = ymin
210   continue
       inisec = 0
!
!  do each side of the polygon. put any x intersections
!  with the scan line y=yscan in xins
!
       ybegin = ymap(y(n))
       xbegin = x(n)
       do l = 1, n
        yend = ymap(y(l))
        dy = yscan-ybegin
        if(dy*(yscan-yend) .gt. 0.0) go to 390
!
!  insert an intersection
!
        inisec = inisec + 1
        xins(inisec) = dy*(x(l)-xbegin)/(yend-ybegin)+xbegin
!
390     continue
        ybegin = yend
        xbegin = x(l)
      end do
!
!  fill if there were any intersections
!
       if(inisec .eq. 0) go to 500
!
!  first we must sort on x intersection.
!  use bubble sort because usually only 2.
!  when "left" is true, ascending sort, false is descending sort.
!
       do i =  1, inisec-1
        xkey = xins(i)
        do j = i+1, inisec
         if(.not. left) go to 420
         if(xkey .ge. xins(j)) go to 430
410      continue
         temp = xkey
         xkey = xins(j)
         xins(j) = temp
         go to 430
420      if(xkey .gt. xins(j)) go to 410
430      continue
        end do
        xins(i) = xkey
      end do
!
!  draw fill lines now
!
       yy = yscan/(2.0*yscale)
       do i = 1, inisec, 2
        call gsmove(xins(i),yy)
        call gsdraw(xins(i+1),yy)
      end do
 
500    continue
      yscan = yscan + dlines
      left = .not. left
      if(yscan .le. ymax) go to 210
!
!  finally, outline the polygon
!
      call gsmove(x(n),y(n))
      do l=1,n
       call gsdraw(x(l),y(l))
      end do
!
!  restore line type
!
      ilntyp = linold
      return
      end
      subroutine gsfont(newfnt,ierr)
!
!***********************************************************************
!
!! GSFONT selects a new font, loading it if necessary
!
      parameter (lun=19)
      parameter (maxfnt = 18,maxtot = 18)
      parameter (mxstrk=maxfnt*9000)
!
      parameter (numbyt=4)
!
      integer bwidth
      integer bxy
      character*80 filnam
      integer ibias(maxtot)
      integer ios
      character*6 machin
!
      save /gccpar/
      save /gcfont/
!
      common /gcfont/ icfnsl, mxslot,
     & islfnt(maxfnt), ihight(maxfnt),
     & indx(95*maxfnt+1), bwidth(95*maxfnt), bxy(mxstrk)
      common /gccpar/ csize, ccos, csin
!
  400 format(//,
     & ' ***** note: insufficient storage for font [',a,'].',//)
!
      if(newfnt .le. 0 .or. newfnt .gt. maxtot)then
        write(*,*)' '
        write(*,*)'GSFONT - Warning'
        write(*,*)'  The selected font is out of range!'
        ierr = -1
        return
      end if
 
      islot = 1
   10 continue
      if(islfnt(islot) .eq. newfnt) go to 40
      islot = islot + 1
      if(islot .le. mxslot) go to 10
!
!  load new font
!
      if(islot .gt. maxfnt) islot = 2
      mxslot = islot
      islfnt(islot) = newfnt
 
      call system2('SYST',machin)
 
      filnam='ss_mep:[mp31879.bin]smdfont.dat'
 
!------------------------begin vax vms specific code--------------------
      open (unit=lun,file=filnam,status='old',access='direct',
     & form='unformatted',recl=512*numbyt,err=999,iostat=ios)
!-------------------------end vax vms specific code---------------------
!-----------------------------begin generic code------------------------
!
! the recl parameter is specified in bytes
!
!  open (unit=lun,file=filnam,status='old',access='direct',
!  . form='unformatted',recl=512*numbyt,err=999)
!------------------------------end generic code-------------------------
      ibias(1) = 0
      read(lun,rec=1) load,(ibias(i),i=2,load)
!
!  calculate offsets into tables
!
      ist = 1 + 95*(islot-1)
      iend = ist + 95
      ioff = indx(ist)-1
      read(lun,rec=1+ibias(newfnt)) nmchar, ihight(islot),
     & (indx(i),i=ist,iend),(bwidth(i), i=ist-95,iend-96)
!
!  make sure it all fits
!
      if((ioff+nmchar) .gt. mxstrk)then
        write(*,400)
        close (unit=lun)
        ierr = -1
        return
      end if
!
!  now add offset to indexes
!
      do i=ist,iend
        if(indx(i) .gt. 0) indx(i) = indx(i) + ioff
      end do
!
!  read in the strokes
!
      iblcks = (nmchar+511)/512
      jst = ioff+1
 
      do i=1,iblcks-1
        read(lun,rec=i+1+ibias(newfnt)) (bxy(j), j=jst,jst+511)
        jst = jst + 512
      end do
 
      if(jst .le. nmchar+ioff)then
        read(lun,rec=iblcks+1+ibias(newfnt)) (bxy(j), j=jst,nmchar+ioff)
      end if
 
      close (unit=lun)
!
!  select the new font
!
   40 continue
      oldh = gschit()
      icfnsl = islot
      csize = oldh*csize/gschit()
      ierr = 0
      return
!
!  font file not found
!
  999 continue
      write(*,*)' '
      write(*,*)'GSFONT - Fatal error!'
      write(*,*)'  open error, iostat = ',ios
      write(*,*)'  could not find or open font file:'
      write(*,'(1x,a)')filnam
      ierr = -2
      stop
      end
      subroutine gsgin(x,y,bchar,ierr)
!
!***********************************************************************
!
!! GSGIN tries to get graphic input (gin) from the currently selected device.   
!
!  if the device is not capable
!  of gin, ierr=-1.   for gin devices, ierr=0 and:
!  x = x position of cursor in absolute screen cm.
!  y = y position of cursor in absolute screen cm.
!  bchar = character stuck at terminal to signal cursor has
!  been positioned (character).
!
      real array(3)
      character bchar
      character space
! 
      save /gcdchr/
      save /gcdprm/
!
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     & ndclrs, idvbts, nfline, xclipd, yclipd
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
!
      save space
!
      data space /' '/
!
!  see if device supports gin
!
      if(ksyand(idvbts,128) .eq. 0) go to 30
!
!  now ask for gin from device driver
!
      call gsdrvr(9,array,dummy)
!
!  convert absolute cm. coord. to virtual cm. coordinates
!
      call gsirst(array(2),array(3),x,y)
!
!  get character as 7 bit ascii
!
      item = int(array(1))
      if(item .lt. 0 .or. item .gt. 127)then
      bchar = space
      else
      bchar = char(item)
      end if
      ierr = 0
      return
!
!  device doesn't support gin
!
   30 continue
      ierr = -1
      return
      end
      function gshght()
!
!***********************************************************************
!
!! GSHGHT returns the current character height in virtual coordinates.
!
      save /gccpar/
!
      common /gccpar/ csize, ccos, csin
!
      gshght = csize/gschit()
      return
      end
      subroutine gsinpt(x,y,lflag,ierr)
!
!***********************************************************************
!
!! GSINPT does a generic graphics input.
!
      logical lflag
!
      character char, space
!
      save space
!
      data space /' '/
!
      call gscrsr(x,y,ibutn,ierr)
      if(ierr .ne. 0) go to 100
       lflag = (ksyand(ibutn,1) .eq. 1)
       return
100   continue
      call gsgin(x,y,char,ierr)
      if(ierr .ne. 0) return
      lflag = (char .eq. space)
      return
      end
      subroutine gsirst(xa,ya,xv,yv)
!
!***********************************************************************
!
!! GSIRST carries out an inverse rotate, scale, and then translate,
!
!  It takes absolute coordinates into virtual coordinates.
!
      save /gcdprm/
!
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
!
!  convert absolute cm. coord. to virtual cm. coordinates
!
      xtemp = (xa-xt)/xs
      yv = (ya-yt)/ys
      xv = rcos*xtemp-rsin*yv
      yv = rcos*yv+rsin*xtemp
      return
      end
      function gsivis(x,y)
!
!***********************************************************************
!
!! GSIVIS ...
!
      integer gsivis
!
      save /gcclip/
!
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
!
!
      gsivis = 0
      if(x .lt. xcm0-0.00001) gsivis = 1
      if(x .gt. xcm1+0.00001) gsivis = gsivis + 2
      if(y .lt. ycm0-0.00001) gsivis = gsivis + 4
      if(y .gt. ycm1+0.00001) gsivis = gsivis + 8
      return
      end
      function gslens(bstrng)
!
!***********************************************************************
!
!! GSLENS returns the length in virtual coordinates of a string.
!
!  the current character size is assumed.
!
      parameter (maxfnt = 18)
      parameter (mxstrk = maxfnt*9000)
!
      character bstrng*(*)
      integer bwidth
      integer bxy
!
      save /gccpar/
      save /gcfont/
!
      common /gccpar/ csize, ccos, csin
 
      common /gcfont/ icfnsl, mxslot,
     &   islfnt(maxfnt), ihight(maxfnt),
     &   indx(95*maxfnt+1), bwidth(95*maxfnt), bxy(mxstrk)
!
      if (icfnsl .eq. 1) then
 
        gslens = 9.0*numchr(bstrng)*csize
 
      else
 
        gslens = 0.0
        ioff = 95*(icfnsl-2) - 32
 
        do i=1,numchr(bstrng)
          jchar = ichar(bstrng(i:i))
          if(jchar .le. 32 .or. jchar .ge. 128
     &        .or. bwidth(jchar+ioff) .le. 0) jchar = 65
          iwidth = bwidth(jchar+ioff)
          gslens = gslens + csize*iwidth
        end do
 
      end if
 
      return
      end
      subroutine gsltyp(itype)
!
!***********************************************************************
!
!! GSLTYP sets the current line type.
!
      logical linilt, lposnd
!
      save /gcltyp/
!
      common /gcltyp/ ilntyp, dleft, dist(4,3), linilt, lposnd
!
      ilntyp = itype
      if(ilntyp .le. 0 .or. (ilntyp .gt. 4)) ilntyp = 1
      linilt = .true.
      return
      end
      subroutine gsmove(x,y)
!
!***********************************************************************
!
!! GSMOVE moves to the point (x,y).
!
      integer gsivis
      logical linilt, lposnd
      logical lcurnt
!
      save /gccpos/
      save /gcltyp/
      save /gcvpos/
!
      common /gcltyp/ ilntyp, dleft, dist(4,3), linilt, lposnd
      common /gcvpos/ xvpos, yvpos
      common /gccpos/ xapos, yapos, ivis, lcurnt
!
!  reset line style to beginning of pattern and show moved
!
      linilt = .true.
      lposnd = .false.
!
!  transform virtual coord. to absolute coord.
!
      xvpos = x
      yvpos = y
      call gsrst(xvpos,yvpos,xapos,yapos)
      ivis = gsivis(xapos,yapos)
 
      return
      end
      subroutine gspoly(x,y,n)

!***********************************************************************
!
!! GSPOLY draws a polygon.
!
      integer n

      integer i
      real x(n)
      real y(n)

      call gsmove(x(n),y(n))
 
      do i = 1, n
       call gsdraw(x(i),y(i))
      end do
 
      return
      end
      subroutine gspstr(bstrng)
!
!***********************************************************************
!
!! GSPSTR strokes out a character string.
!
!  "bstrng" ia a byte array with 0 as a terminator.
!  The string is drawn at the current position.
!
      character bstrng*(*)
!
      save /gccoff/
      save /gcltyp/
      save /gcvpos/
!
      common /gcvpos/ xvpos, yvpos
      common /gccoff/ xoff, yoff
      logical linilt, lposnd
      common /gcltyp/ ilntyp, dleft, dist(4,3), linilt, lposnd
!
!  don't draw characters in linetypes
!
      iold = ilntyp
      ilntyp = 1
!
      nbyte = 0
      nchar = numchr(bstrng)
100   nbyte = nbyte + 1
      if(nbyte .gt. nchar) go to 200
!
!  save the (0,0) position of the character
!
      xoff = xvpos
      yoff = yvpos
!
!  get the character to stroke
!
      jchar = ichar(bstrng(nbyte:nbyte))
!  if(jchar .eq. 0) go to 200
!
!  stroke the character
!
      call gsstrk(jchar)
      go to 100
!
!  return line type to that of before
!
200   continue
      ilntyp = iold
      return
      end
      subroutine gsrclp(area)
!
!***********************************************************************
!
!! GSRCLP restores a saved absolute clipping window saved by GSSCLP.   
!
!  no error checking is performed here!!!
!
      real area(4)
!
      save /gcclip/
!
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
!
      xcm0 = area(1)
      xcm1 = area(2)
      ycm0 = area(3)
      ycm1 = area(4)
      return
      end
      subroutine gsrst(xv,yv,xa,ya)
!
!***********************************************************************
!
!! GSRST rotates, scales, and then translates coordinates.
!
!  It takes virtual coordinates into screen coordinates.
!
      save /gcdprm/
!
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
 
      xtemp = xv
      xa = xs*(rcos*xtemp+rsin*yv) + xt
      ya = ys*(rcos*yv-rsin*xtemp) + yt
      return
      end
      subroutine gssclp(vx0,vx1,vy0,vy1,area)
!
!***********************************************************************
!
!! GSSCLP saves the current absolute clipping window.
!
!  It sets a new absolute clipping window given virtual coordinates.
!  it makes sure that the clipping window never lies outside the
!  physical device.
!
      real area(4)
 
      save /gcclip/
      save /gcdchr/
!
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd
!
      area(1) = xcm0
      area(2) = xcm1
      area(3) = ycm0
      area(4) = ycm1
!
      call gsrst(vx0,vy0,ax0,ay0)
      call gsrst(vx1,vy1,ax1,ay1)
      xcm0 = max ( min (ax0,ax1), 0.0)
      ycm0 = max ( min (ay0,ay1), 0.0)
      xcm1 = min (xclipd, max (ax0,ax1))
      ycm1 = min (yclipd, max (ay0,ay1))
      return
      end
      subroutine gssetc(size,angle)
!
!***********************************************************************
!
!! GSSETC sets the character height and angle.
!
      real pio180
      parameter (pio180=1.745329252e-2)
!
      real angle
      real csize
      real size
!
      save /gccpar/
!
      common /gccpar/ csize, ccos, csin
!
!  set up size multiplier
!
      csize = size/gschit()
!
!  calculate the rotation factors
!
      rad = -pio180*angle
      ccos = cos(rad)
      csin = sin(rad)
      return
      end
      subroutine gsstrk(ichar)
!
!***********************************************************************
!
!! GSSTRK strokes out a character.
!
      parameter (maxfnt = 18)
      parameter (mxstrk = maxfnt*9000)
!
      integer bwidth, bxy
      logical lmove
!
      save /gcfont/
!
      common /gcfont/ icfnsl, mxslot,
     &   islfnt(maxfnt), ihight(maxfnt),
     &   indx(95*maxfnt+1), bwidth(95*maxfnt), bxy(mxstrk)
!
!  space fill all non-printing and non-defined with space size of
!  a capital "a".
!
      jchar = (ichar-32) + 95*(icfnsl-1)
      if(ichar .le. 32 .or. ichar .ge. 128) go to 800
      if(icfnsl .gt. 1)then
       if(bwidth(jchar-95) .le. 0) go to 800
      end if
!
!  stroke this character
!
      index = indx(jchar)
      idone = indx(jchar+1)
!
!  first position is an assumed move
!
      lmove = .true.
!
!  get the scaled and rotated next position on the character
!
100    continue
150    if(bxy(index) .ne. -64) go to 160
       lmove = .true.
       index = index + 1
       go to 100
!
160    x=bxy(index)
       y=bxy(index+1)
       call gscclc(x,y,dx,dy)
       index = index + 2
       if(lmove)then
        call gsmove(dx,dy)
           else
        call gsdraw(dx,dy)
       end if
       lmove = .false.
       if(index .lt. idone) go to 100
!
!  all done with the character, move to next character position
!
200   continue
      if(icfnsl .eq. 1)then
       width = 9.0
          else
       width = bwidth(jchar-95)
      end if
      call gscclc(width,0.0,dx,dy)
      call gsmove(dx,dy)
      return
!
!  use capital "a" for size of space and all non-printing and non-defined
!
800   continue
      jchar = (65-32) + 95*(icfnsl-1)
      go to 200
      end
      subroutine gswndo(uxl,uxh,uyl,uyh,xoff,yoff,xawdth,yahigh)
!
!***********************************************************************
!
!! GSWNDO provides window/viewport mechanism.
!
      save /gcclip/
      save /gcdchr/
      save /gcdprm/
!
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
!
      rcos = 1.0
      rsin = 0.0
      xs = xawdth/(uxh-uxl)
      ys = yahigh/(uyh-uyl)
      xt = xoff - xs*uxl
      yt = yoff - ys*uyl
      xcm0 =  max ( min (xoff,xoff+xawdth),0.0)
      ycm0 =  max ( min (yoff,yoff+yahigh),0.0)
      xcm1 =  min (xclipd, max (xoff,xoff+xawdth))
      ycm1 =  min (yclipd, max (yoff,yoff+yahigh))
!
!  save virtual coordinate extent
!
      vxl = uxl
      vxh = uxh
      vyl = uyl
      vyh = uyh
      return
      end
      function gsxlcm()

!***********************************************************************
!
!! GSXLCM returns the x axis length of the current device in centimeters.
!
      save /gcdchr/

      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd

      gsxlcm = xlencm
      return
      end
      function gsylcm()

!***********************************************************************
!
!! GSYLCM returns the y axis length of the current device in centimeters.
!
      save /gcdchr/

      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd

      gsylcm = ylencm
      return
      end
      subroutine hatch(xvert, yvert, numpts, phi, cmspac, iflags,
     &   xx, yy)

!***********************************************************************
!
!! HATCH provides shading for a general polygonal region.  
!
!  there is 
!  absolutely no assumption made about convexity.  a polygon is specified 
!  by its vertices, given in either a clockwise or counter-clockwise order.  
!  the density of the shading lines (or points) and the angle for the 
!  shading lines are both determined by the parameters passed to the routine.
!
!  the input parameters are interpreted as follows:
!
!  xvert    -  an array of x coordinates for the polygon(s) vertices
!
!  yvert    -  an array of y coordinates for the polygon(s) vertices
!
!  note: an x value >=1e38 signals a new polygon.   this allows
!  filling areas that have holes where the holes are
!  defined as polygons.   it also allows multiple
!  polygons to be filled in one call to hatch.
!
!  numpts  -  the number of vertices in the polygon(s) including
!  the separator(s) if any.
!
!  phi      -  the angle for the shading, measured counter-clockwise
!  in degrees from the positive x-axis
!
!  cmspac   -  the distance in virtual coordinates (cm. usually)
!  between shading lines.   this value may be rounded
!  a bit, so some cumulative error may be apparent.
!
!  iflags   -  general flags controlling hatch
!  0 ==>  boundary not drawn, input is virtual coord.
!  1 ==>  boundary drawn, input is virtual coord.
!  2 ==>  boundary not drawn, input is world coord.
!  3 ==>  boundary drawn, input is world coord.
!
!  xx   -  a work array atleast "numpts" long.
!
!  yy   -  a second work array atleast "numpts" long.
!
      logical lmove
      real xintcp(20)
      real xvert(numpts)
      real xx(numpts)
      real yvert(numpts)
      real yy(numpts)
!
      save /gcdchr/
!
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd
!
!  this routine has to maintain an internal array of the transformed
!  coordinates.  this requires the passing of the two working arrays
!  called "xx" and "yy".
!  this routine also needs to store the intersections of the hatch
!  lines with the polygon.   this is done in "xintcp".
!
!
!  x >= 'BIGNUM' signals the end of a polygon in the input.
!
      save bignum,fact,pi180
!
      data bignum /1e38/
      data fact /16.0/
      data pi180 /0.017453292/
!
!  check for valid number of vertices.
!
      if(numpts .lt. 3) return
!
!  convert all of the points to integer coordinates so that the shading
!  lines are horizontal.  this requires a rotation for the general case.
!  the transformation from virtual to internal coordinates has the two
!  or three phases:
!
!  (1)  convert world to virtual coord. if input in world coord.
!
!  (2)  rotate clockwise through the angle phi so shading is horizontal,
!
!  (3)  scale to integers in the range
!  [0...2*fact*(device_maxy_coordinate)], forcing coordinates
!  to be odd integers.
!
!  the coordinates are all odd so that later tests will never have an
!  outcome of "equal" since all shading lines have even coordinates.
!  this greatly simplifies some of the logic.
!
!  at the same time the pre-processing is being done, the input is checked
!  for multiple polygons.  if the x-coordinate of a vertex is >= 'BIGNUM'
!  then the point is not a vertex, but rather it signifies the end of a
!  particular polygon.  an implied edge exists between the first and last
!  vertices in each polygon.  a polygon must have at least three vertices.
!  illegal polygons are removed from the internal lists.
!
!
!  compute trigonometric functions for the angle of rotation.
!
      cosphi = cos(pi180*phi)
      sinphi = sin(pi180*phi)
!
!  first convert from world to virtual coord. if necessary and eliminate
!  any polygons with two or fewer vertices
!
      itail = 1
      ihead = 0
      do i = 1, numpts
!
!  allocate another point in the vertex list.
!
       ihead = ihead + 1
!
!  a xvert >= 'BIGNUM' is a special flag.
!
       if(xvert(i) .lt. bignum) go to 110
        xx(ihead) = bignum
        if((ihead-itail) .lt. 2) ihead = itail - 1
        itail = ihead + 1
        go to 120
110    continue
!
!  convert from world to virtual coord. if input is world coord.
!
!  if((iflags .and. 2) .eq. 0) go to 115
      if(iflags .eq. 1) go to 115
      if(iflags .eq. 4) go to 115
        call scale(xvert(i),yvert(i),xx(ihead),yy(ihead))
        go to 120
115        continue
        xx(ihead) = xvert(i)
        yy(ihead) = yvert(i)
120    continue
      end do
 
      if((ihead-itail) .lt. 2) ihead = itail - 1
      nvert = ihead
!
!  draw boundary(s) if desired
!
!  if((iflags .and. 1) .eq. 0) go to 138
      if(iflags .eq. 2) go to 138
      if(iflags .eq. 4) go to 138
      ihead = 0
      itail = 1
      lmove = .true.
130   continue
      ihead = ihead + 1
      if(ihead .gt. nvert) go to 133
      if(xx(ihead) .ne. bignum) go to 135
133   continue
      call gsdraw(xx(itail),yy(itail))
      itail = ihead + 1
      lmove = .true.
      go to 139
135   continue
      if(lmove) go to 137
      call gsdraw(xx(ihead),yy(ihead))
      go to 139
137   continue
      call gsmove(xx(ihead),yy(ihead))
      lmove = .false.
139   continue
      if(ihead .le. nvert) go to 130
138   continue
!
!  rotate to make shading lines horizontal
!
      ymin = bignum
      ymax = -bignum
      yscale = yres*fact
      yscal2 = 2.0*yscale
      do i = 1, nvert
       if(xx(i) .eq. bignum) go to 140
!
!  perform the rotation to achieve horizontal shading lines.
!
       xv1 = xx(i)
       xx(i) = +cosphi*xv1 + sinphi*yy(i)
       yy(i) = -sinphi*xv1 + cosphi*yy(i)
!
!  convert to integers after scaling, and make vertices odd. in y
!
       yy(i) = 2.0*aint(yscale*yy(i)+0.5)+1.0
       ymin =  min (ymin,yy(i))
       ymax =  max (ymax,yy(i))
140    continue
      end do
!
!  make shading start on a multiple of the step size.
!
      step = 2.0*aint(yres*cmspac*fact)
      ymin = aint(ymin/step) * step
      ymax = aint(ymax/step) * step
!
!  after all of the coordinates for the vertices have been pre-processed
!  the appropriate shading lines are drawn.  these are intersected with
!  the edges of the polygon and the visible portions are drawn.
!
      y = ymin
150    continue
       if(y .gt. ymax) go to 250
!
!  initially there are no known intersections.
!
       icount = 0
       ibase = 1
       ivert = 1
160     continue
        itail = ivert
        ivert = ivert + 1
        ihead = ivert
        if(ihead .gt. nvert) go to 165
        if(xx(ihead) .ne. bignum) go to 170
!
!  there is an edge from vertex n to vertex 1.
!
165       ihead = ibase
          ibase = ivert + 1
          ivert = ivert + 1
170     continue
!
!  see if the two endpoints lie on
!  opposite sides of the shading line.
!
        yhead =  y - yy(ihead)
        ytail =  y - yy(itail)
        if(yhead*ytail .ge. 0.0) go to 180
!
!  they do.  this is an intersection.  compute x.
!
        icount = icount + 1
        delx = xx(ihead) - xx(itail)
        dely = yy(ihead) - yy(itail)
        xintcp(icount) = (delx/dely) * yhead + xx(ihead)
180     continue
        if( ivert .le. nvert ) go to 160
!
!  sort the x intercept values.  use a bubblesort because there
!  aren't very many of them (usually only two).
!
      if(icount .eq. 0) go to 240
 
      do i = 2, icount
        xkey = xintcp(i)
        k = i - 1
        do j = 1, k
          if(xintcp(j) .gt. xkey) then
            xtemp = xkey
            xkey = xintcp(j)
            xintcp(j) = xtemp
          end if
        end do
        xintcp(i) = xkey
      end do
!
!  all of the x coordinates for the shading segments along the
!  current shading line are now known and are in sorted order.
!  all that remains is to draw them.  process the x coordinates
!  two at a time.
!
       yr = y/yscal2
       do i = 1, icount, 2
!
!  convert back to virtual coordinates.
!  rotate through an angle of -phi to original orientation.
!  then unscale from grid to virtual coord.
!
        xv1 = + cosphi*xintcp(i) - sinphi*yr
        yv1 = + sinphi*xintcp(i) + cosphi*yr
        xv2 = + cosphi*xintcp(i+1) - sinphi*yr
        yv2 = + sinphi*xintcp(i+1) + cosphi*yr
!
!  draw the segment of the shading line.
!
        call gsmove(xv1,yv1)
        call gsdraw(xv2,yv2)
      end do
 
240    continue
       y = y + step
       go to 150
250   continue
      return
      end
      subroutine headin(lhead,ihead,htmult,nlines)
!
!***********************************************************************
!
!! HEADIN writes heading lines above plot area.
!  (level 2,3)
!
!  input:   lhead  = heading text line
!  ihead  = number of characters in lhead
!  htmult = character size in multiple of
!  current character size
!  nlines = total number of lines to be plotted
!
!  character array to store the heading text
!
!  kzhdmx = total number of lines in heading, max = 4
!  kzhdnl = current line
!  kzhdtx = (nl-1)*18+1, text
!  (nl-1)*18+17, number of characters
!  (nl-1)*18+18, number of underlines
!  zzhdsz = char size
!
!  -------------------------------------------
!  kzhdtx: | text ...           | nchar | nunderlines|
!  -------------------------------------------
!  ...
!  ...
!
      save /carea/
      save /cheadc/
      save /cheadn/
      save /clevel/
      save /cmxalf/
      save /cphysr/
      save /cstrng/
      save /gcclip/
!
      common /clevel/ kzlevl,kzbegn
      common /cheadn/ kzhdnl,kzhdmx,kzhdtx(72),zzhdsz(4)
      common /cheadc/ czhdac(4,6),czhdaf(4,6),czhdas(4)
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      character czhdac*1,czhdaf*5,czhdas*5
      character czalfl*1,czalfn*5,czalfs*5
!
      integer lhead(*)
      character cflag(6)*1,cfont(6)*5,cstyle*5
      equivalence (multi,rmulti)
!
      save jtext,jnchar,juline,maxchr
!
      data jtext,jnchar,juline /1,17,18/
      data maxchr /64/
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
!
!  save max number of line if this is the first call
!
        if(kzhdmx.eq.0)then
          nl=abs(nlines)
          if(nl.gt.4.or.nl.eq.0)then
            kzhdmx=-4
            call errmes('HEADIN',nlines,4)
          else
            kzhdmx=nl
          end if
        end if
!
!  pack heading lines
!
        if(kzhdmx.gt.0)then
          kzhdnl=kzhdnl+1
          icur=(kzhdnl-1)*18
          if(kzhdnl.le.kzhdmx)then
            kzhdtx(icur+juline)=0
!
!  store char size
!
            if(htmult.lt.0.0)then
              kzhdtx(icur+juline)=kzhdtx(icur+juline)+1
              zzhdsz(kzhdnl)=-htmult*zzhite
            else
              if(htmult.lt.0.000001)then
                rmulti=htmult
                zzhdsz(kzhdnl)=zzhite*multi/2.0
              else
                zzhdsz(kzhdnl)=htmult*zzhite
              end if
            end if
!
!  adjust heading text height, if needed
!
            ih=abs(ihead)
            rlen=zxmess(lhead,ih,czalfl,czalfn,czalfs)*
     &             abs(zzhdsz(kzhdnl))/zzhite
            alen=zzxrgt-zzxlft
            if(rlen.gt.alen)then
              zzhdsz(kzhdnl)=zzhdsz(kzhdnl)*alen/rlen
            end if
!
!  store heading text
!
            if(ihead.lt.0)then
              kzhdtx(icur+juline)=kzhdtx(icur+juline)+1
            end if
            kzhdtx(icur+jnchar)=ih
            itemp=maxchr
            call zcopys(lhead,ih,kzhdtx(icur+jtext),itemp)
            if(itemp.lt.0) call errmes('HEADIN',kzhdnl,5)
            call zloalf(cflag,cfont,cstyle,czalfl,czalfn,czalfs)
            do k=1,6
              czhdac(kzhdnl,k)=cflag(k)
              czhdaf(kzhdnl,k)=cfont(k)
            end do
            czhdas(kzhdnl)=cstyle
!
!  draw if all lines are packed
!
            if(kzhdnl.eq.kzhdmx)then
              yloc=ycm0+zzyor+zzyaxs*zzyaxr+
     &           max (zzhdsz(1),0.1*zzyaxs*zzyaxr)*(1.2-kzhdmx*0.2)
              xlen=zzxaxs*zzxaxr
 
              do i=kzhdmx,1,-1
                icur=(i-1)*18
!
!  retrieve font
!
                call gssetc(zzhdsz(i),0.0)
                nchar=kzhdtx(icur+jnchar)
 
                do k=1,6
                  cflag(k)=czhdac(i,k)
                  cfont(k)=czhdaf(i,k)
                end do
 
                cstyle=czhdas(i)
                rlen=zxmess(kzhdtx(icur+jtext),nchar,
     &              cflag,cfont,cstyle)
                xp1=xcm0+zzxor+(xlen-rlen)*0.5
                xp2=xp1+rlen
!
!  draw second underline
!
                if(kzhdtx(icur+juline).ge.2)then
                  call dsmove(xp1,yloc)
                  call dsdraw(xp2,yloc)
                  yloc=yloc+0.07
                end if
!
!  draw first underline
!
                if(kzhdtx(icur+juline).ge.1)then
                  call dsmove(xp1,yloc)
                  call dsdraw(xp2,yloc)
                  yloc=yloc+0.14
                end if
!
!  draw text
!
                call zgetfn(cfont(1),cstyle,khar)
                if(khar.eq.1)then
                  call dsmove(xp1+zzhdsz(i)*0.15,yloc)
                else
                  call dsmove(xp1,yloc)
                end if
                call ztext(kzhdtx(icur+jtext),nchar,
     &          cflag,cfont,cstyle)
                yloc=yloc+1.5*zzhdsz(i)
              end do
              kzhdmx=-4
            end if
          end if
        end if
!
!  wrong level
!
      else
        call errmes('HEADIN',2,3)
      end if
 
      return
      end
      subroutine height(hite)
!
!***********************************************************************
!
!! HEIGHT sets the character height.
!  level 1-3, p/s
!
!  input:   hite = character height
!
      save /clevel/
      save /cpage/
      save /cstrng/
      save /cunit/
!
      common /clevel/ kzlevl,kzbegn
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
!
      if(kzlevl.eq.1)then
        uuhite=hite*zzunit
        call gssetc(uuhite,0.0)
      elseif(kzlevl.eq.2.or.kzlevl.eq.3)then
        uuhite=hite*zzunit
        zzhite=hite*zzunit*zzpagr
        call gssetc(zzhite,zzangl)
      else
        call errmes('HEIGHT',1,3)
      end if
 
      return
      end
      subroutine hrddef(switch,func)
!
!***********************************************************************
!
!! HRDDEF turns on hardware function.
!  (level 1-3)
!
!  input:   switch = value indicating on/off
!  func   = hardware function, only autoprint
!  are supported currently
!
      parameter (kzyes=111)
!
      character func*6
!
      save /cdevic/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        if(func(1:4).eq.'copy')then
          if(switch.gt.0.5) kzcopy=kzyes
        else
          call errmes('HRDDEF',istr,4)
        end if
      else
        call errmes('HRDDEF',1,3)
      end if
 
      return
      end
      subroutine hrdhsi(hue,sat,aint)
!
!***********************************************************************
!
!! HRDHSI sets hue, saturation and intensity for current color.
!  level 1-3, p/s
!
!  input:   hue  = hue (from 0.0 to 4.0)
!  sat  = saturation (from 0.0 to 1.0)
!  aint = intensity (from 0.0 to 1.0)
!
      parameter (kzmaxc=255)
!
      real epslon
      parameter (epslon=0.01)
!
      character czcolr*8
      real xa(8)
      real ya(3)
!
      save /clevel/
      save /colorc/
      save /colorn/
      save /gcdchr/
      save /gcvpos/
!
      common /clevel/ kzlevl,kzbegn
      common /colorn/ kzshd,zzhspc,zzcolr(kzmaxc,3),kznclr,kzccol
      common /colorc/ czcolr
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
      common /gcvpos/ xvpos, yvpos
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
!
!  convert input
!
        if(sat.ge.0.0.and.sat.le.1.0)then
          sat1=sat*100.0
        else
          sat1=100.0
        end if
 
        if(aint.ge.0.0.and.aint.le.1.0)then
          aint1=aint*50.0
        else
          aint1=50.0
        end if
 
        if(hue.lt.-epslon)then
          hue1=0.0
          sat1=100.0
          aint1=0.0
        elseif(hue.lt.0.25)then
          if(sat1.lt.99.0) sat1=0.0
          hue1=hue
        elseif(hue.lt.0.5)then
          hue1=0.5
        elseif(hue.lt.3.0)then
          hue1=hue
        elseif(hue.lt.3.5)then
          hue1=hue-3.0
        elseif(hue.lt.3.75)then
          hue1=0.5
        elseif(hue.le.4.0)then
          hue1=0.0
          if(aint1.gt.48.0) aint1=100.0
        else
          hue1=0.0
          sat1=100.0
          aint1=0.0
        end if
 
        hue1=hue1*120.0
!
!  check if color is previously defined
!
        do i=1,kznclr-1
 
          dhue=zzcolr(i,1)-hue1
 
          if(abs(dhue).lt.epslon)then
 
            dsat=zzcolr(i,2)-sat1
 
            if(abs(dsat).lt.epslon)then
 
              daint=zzcolr(i,3)-aint1
 
              if(abs(daint).lt.epslon)then
 
                if(i.le.ndclrs)then
                  kzccol=i
                  elseif(i.gt.ndclrs)then
                  kzccol=mod(i,ndclrs)+1
                end if
 
                call gscolr(kzccol,ierr)
                return
 
              end if
 
            end if
 
          end if
 
        end do
!
!  if not previously defined, define new color
!
        if(kznclr.le.ndclrs)then
          xa(1)=real(kznclr)
          kzccol=kznclr
          ya(1)=hue1
          ya(2)=aint1
          ya(3)=sat1
          zzcolr(kznclr,1)=hue1
          zzcolr(kznclr,2)=sat1
          zzcolr(kznclr,3)=aint1
          call gsdrvr(11,xa,ya)
          kznclr=kznclr+1
!
!  if maximum number of colors exceeded, pick a color randomly
!
        else
          if(kznclr.le.kzmaxc)then
            zzcolr(kznclr,1)=hue1
            zzcolr(kznclr,2)=sat1
            zzcolr(kznclr,3)=aint1
          end if
          kzccol=mod(kznclr,ndclrs)+1
          kznclr=kznclr+1
        end if
        call gscolr(kzccol,ierr)
!
!  wrong level
!
      else
        call errmes('HRDHSI',1,3)
      end if
 
      return
      end
      subroutine hrdinp(xdata,ydata)
!
!***********************************************************************
!
!! HRDINP performs graphic input.
!  (level 3)
!
!  output:  xdata,ydata = x and y values obtained in
!  currently defined coordinate system
!
      character ch
!
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
!
      if(kzlevl.eq.3)then
        call cursor(xdata,ydata,ch)
      else
        call errmes('HRDINP',3,0)
      end if
!
      return
      end
      subroutine hrdrgb(red,grn,blu)
!
!***********************************************************************
!
!! HRDRGB sets RGB components for current color.
!  level 1-3, p/s
!
!  input:   red  = red component   (0.0 - 1.0)
!  grn  = green component (0.0 - 1.0)
!  blu  = blue component  (0.0 - 1.0)
!
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
!
!  the index is unimportant because a vlt is not used
!
        index=1
        call gsdrgb(index,red,grn,blu,ierr)
!
!  wrong level
!
      else
        call errmes('HRDRGB',1,3)
      end if
 
      return
      end
      subroutine hrdrot(str)
!
!***********************************************************************
!
!! HRDROT rotates the plot orientation for laser printer.
!  (level 1, p/s)
!
!  input:   str = plot flag
!  if = 'AUTO', compare page x and y length
!  to decide tall or wide
!  if = 'MOVIE', set to portrait
!  if = 'COMIC', set to landscape
!
      parameter (kzyes=111)
!
      save /clevel/
      save /cpage/
      save /gcdchr/
!
      common /clevel/ kzlevl,kzbegn
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
!
      character str*(*),string*4
!
      save kqms,kln03,kpost
!
      data kqms,kln03,kpost /1200,3,910/
!
      if(kzlevl.eq.1)then
        kdev=int(devid)
        if(len(str).ge.4)then
          if(kdev.eq.kqms.or.kdev.eq.kln03.or.kdev.eq.kpost)then
            if(str(1:4).eq.'AUTO')then
              kzauto=kzyes
            elseif(str(1:4).eq.'MOVI')then
              call zlasap
            elseif(str(1:4).eq.'COMI')then
              call zlasal
            else
              string=str(1:4)
              call wch2in(string,istr)
              call errmes('HRDROT',istr,4)
            end if
          end if
        else
          string=str(1:4)
          call wch2in(string,istr)
          call errmes('HRDROT',istr,4)
        end if
      else
        call errmes('HRDROT',1,0)
      end if
 
      return
      end
      subroutine hrdscl(istr)
!
!***********************************************************************
!
!! HRDSCL scales the plot.
!  (level 1, p/s)
!
!  input:   str =  scale flag
!  if = 'DOWN', scale down if plot bounds
!  exceed device limits
!  if = 'CLIP', clip all out of bound
!  entities
!  if = 'ABORT', abort current plot if
!  plot not within bounds of device
!  if = 'SCREEN', scale up or down to use
!  full screen
!  if = 'NONE', no clipping done
!
      parameter (kzclip=12)
      parameter (kzdown=13)
      parameter (kzscrn=14)
      parameter (kzabrt=15)
!
      parameter (numbyt=4)
      character*(numbyt) str
!
      save /carea/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
!
      itemp=istr
      call win2ch(itemp,str)
      if(kzlevl.eq.1)then
        if(len(str).ge.4)then
          if(str(1:4).eq.'DOWN')then
            kzscal=kzdown
          elseif(str(1:4).eq.'CLIP'.or.str(1:4).eq.'NONE')then
            kzscal=kzclip
          elseif(str(1:4).eq.'ABOR')then
            kzscal=kzabrt
          elseif(str(1:4).eq.'SCRE')then
            kzscal=kzscrn
          else
            call errmes('HRDSCL',istr,4)
          end if
        else
          call errmes('HRDSCL',istr,4)
        end if
      else
        call errmes('HRDSCL',1,0)
      end if
 
      return
      end
      subroutine hrdshd
!
!***********************************************************************
!
!! HRDSHD sets a flag for hardware shade.
!  level 1-3, p/s
!
      parameter (kzyes=111)
      parameter (kzmaxc=255)
!
      save /clevel/
      save /colorc/
      save /colorn/
!
      common /clevel/ kzlevl,kzbegn
      common /colorn/ kzshd,zzhspc,zzcolr(kzmaxc,3),kznclr,kzccol
      common /colorc/ czcolr
      character czcolr*8
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzshd=kzyes
      else
        call errmes('HRDSHD',1,3)
      end if
      return
      end
      function ilabsz()
!
!***********************************************************************
!
!! ILABSZ returns the maximum length that LINLAB will return.
!
      ilabsz = 6
      return
      end
      subroutine intaxs
!
!***********************************************************************
!
!! INTAXS sets the X and Y axes numbers to integers.
!  level 1-3, p/s
!
      parameter (kzint=1)
!
      save /caxis/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzxtyp=kzint
        kzytyp=kzint
      else
        call errmes('INTAXS',1,3)
      end if
      return
      end
      subroutine intgrx
!
!***********************************************************************
!
!! INTGRX sets X axis numbers to integers.
!  level 1-3, p/s
!
      parameter (kzint=1)
!
      save /caxis/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzxtyp=kzint
      else
        call errmes('INTGRX',1,3)
      end if
      return
      end
      subroutine intgry
!
!***********************************************************************
!
!! INTGRY sets Y axis numbers to integers.
!  level 1-3, p/s
!
      parameter (kzint=1)
!
      save /caxis/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzytyp=kzint
      else
        call errmes('INTGRY',1,3)
      end if
      return
      end
      subroutine intno(ival,xpos,ypos)
!
!***********************************************************************
!
!! INTNO writes a character string on screen.
!  (level 2-3)
!
!  nput:   xpos,ypos = x and y position in inches
!  from physical origin
!  if = 'ABUT', use current cursor
!  position
!
      parameter (numbyt=4)
!
      character czalfl*1,czalfn*5,czalfs*5
      integer imessa(16/numbyt)
      character cmess*16,khar*(numbyt)
!
      save /clevel/
      save /cmxalf/
      save /cmess/
      save /cpage/
      save /cphysr/
      save /cunit/
      save /gcclip/
      save /gcvpos/
!
      common /clevel/ kzlevl,kzbegn
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /gcvpos/ xvpos, yvpos
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
!
 10   format(i11)
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
!
!  calculate virtual coordinate
!
        temp=xpos
        call win2ch(temp,khar)

        if(khar.eq.'ABUT')then
          vx=zzabux
        else
          vx=xcm0+zzxor+xpos*zzunit*zzpagr
        end if

        temp=ypos
        call win2ch(temp,khar)

        if(khar.eq.'ABUT')then
          vy=zzabuy
        else
          vy=ycm0+zzyor+ypos*zzunit*zzpagr
        end if
!
!  draw integer
!
        call dsmove(vx,vy)
        write(cmess,10)ival
!
!  call sybyt4('N',imessa(3),4,0)
!
!  it is time to pack the characters into an integer array
!
        iloop=(12-1)/numbyt+1
        icnt=1
 
        do i=1,iloop
          call wch2in(cmess(icnt:icnt+numbyt-1),imessa(i))
          icnt=icnt+numbyt
        end do
 
        icnt=mod(12-1,numbyt)+1
        call sybyt4('N',imessa(iloop),icnt,0)
        call zstrbl(imessa)
        ln=leng(imessa)
        call ztext(imessa,ln,czalfl,czalfn,czalfs)
        zzabux=xvpos
        zzabuy=yvpos
      else
        call errmes('INTNO',3,0)
      end if
 
      return
      end
      function ivis(xi,eta,zeta,z,izdim1)
!
!***********************************************************************
!
!! IVIS determines if a point is visible with respect to a surface.
!
!  point is given by xi, eta, zin
!  and visibility is tested with respect to surface z(x,y)
!  xi, eta coordinates expressed as indices of array z, but need not
!  be integers in general. for entry ivis, they must be.
!
      real flim(2)
      integer limit(2)
      logical lsolid
      real z(izdim1,2)
!
      save /comdp/
!
      common/comdp/xmin,xmax,ymin,ymax,zmin,zmax,axisr(2),plotx,
     &   ploty,pltorg(2),camxyz(3),mx,ny,fmx,fny,camwkg(6),xorg(3),
     &   gx(3),fx(2),kscale,zorg,center(2),pqlmt,
     &   amtx(3,3),focall

      common /comdp1/ lsolid
!
      equivalence(u,camxyz(1)),(v,camxyz(2)),(w,camxyz(3)),
     &   (mx,limit(1)),(fmx,flim(1))

      equivalence (cx,cy), (dxi,deta), (xiw,etaw),
     &  (xiend,etaend), (kdxi,kdeta), (kxiend,ketend),
     &  (dx,dy)
!
!  initial p function
!
5     ivis = 0
      r = u-xi
      s = v-eta
      t = w-zeta
!
!  test if we check along x
!
      if(abs(r) .lt. 1.0) go to 20
!
!  constants for y(x),z(x)
!
      cy = s/r
      cz = t/r
      dxi = sign(1.0,r)
!
!  initial point. take aint(xi) if .ne. xi and steps in right directi
!
      xiw = aint(xi)
      if((xiw-xi)*dxi .le. 0.0) xiw = xiw+dxi
!
!  skip if off limits (we are on edge of plot region)
!
      if((xiw-1.0)*(xiw-fmx) .gt. 0.0) go to 20
!
!  final point. take aint(u) if it moves opposite dxi, else round
!
      xiend = aint(u)
      if((xiend-u)*dxi .ge. 0.0) xiend = xiend-dxi
!
!  but do not go beyond edges
!
      xiend =  max (1.0, min (xiend,fmx))
!
!  after testing, re-order these statements
!
      j = ifix(xiw)
      kdxi = ifix(dxi)
      kxiend = ifix(xiend)
      xw = xiw-u
!
!  if limits cross, no test
!
      if((xiend-xiw)*dxi .le. 0.0) go to 20
!
!  get y(x)
!
3     yw = v + xw*cy
!
!  if y is off limits, done
!
      if((yw-1.0)*(yw-fny)) 21,25,20
!
!  on edge exactly, no interpolation
!
25    k = ifix(yw)
      if(w + xw*cz - z(j,k)) 4,10,7
!
!  index for lower y of interval
!
21    k = ifix(yw)
      dy = yw-float(k)
!
!  test z of line - z of surface. accept zero difference.
!
      if((w + xw*cz)-(z(j,k) + dy*(z(j,k+1)-z(j,k)))) 4,10,7
!
!  negative. ok if ivis neg. or zero, else reject
!
4     if(ivis) 10,6,40
!
!  ivis was zero, set neg.
!
6     ivis = -1
      go to 10
!
!  plus. ok if ivis + or zero, else, reject
!
7     if(ivis) 40,8,10
!
!  set plus
!
8     ivis = 1
!
!  test if done. advance if not
!
10    if(j .eq. kxiend) go to 20
      j = j+kdxi
      xw = xw+dxi
      go to 3
!
!  check if we test in y direction
!
20    if(abs(s) .lt. 1.0) go to 45
!
!  constants for x(y),z(y)
!
      cx = r/s
      cz = t/s
      deta = sign(1.0,s)
      etaw = aint(eta)
      if((etaw-eta)*deta .le. 0.0) etaw = etaw+deta
!
!  check whether on limits
!
      if((etaw-1.0)*(etaw-fny) .gt. 0.0) go to 45
      etaend = aint(v)
      if((etaend-v)*deta .ge. 0.0) etaend = etaend-deta
      etaend =  max (1.0, min (fny,etaend))
      k = ifix(etaw)
      kdeta = ifix(deta)
      yw = etaw-v
      ketend = ifix(etaend)
!
!  if limits cross, no test, but test single point if we have already
!  tested x
!
      a = etaend-etaw
      if(a*deta .lt. 0.0) go to 45
      if(a .eq. 0.0 .and. ivis .eq. 0) go to 45
!
!  get x(y)
!
23    xw = u + yw*cx
!
!  if x off limits, done
!
      if((xw-1.0)*(xw-fmx)) 44,46,45
46    j = ifix(xw)
      if(w + yw*cz - z(j,k)) 24,30,27
44    j = ifix(xw)
      dx = xw-float(j)
      if((w + yw*cz) - (z(j,k)+dx*(z(j+1,k)-z(j,k)))) 24,30,27
!
!  neg., ivis must be neg or zero else rejct
!
24    if(ivis) 30,26,40
!
!  set ivis neg
!
26    ivis = -1
      go to 30
!
!  pos, ivis must be zero or + else reject
!
27    if(ivis) 40,28,30
28    ivis = 1
!
!  test if done, advance if not.
!
30    if(k .eq. ketend) go to 45
      k = k+kdeta
      yw = yw+deta
      go to 23
!
!  reject this point, return zero.
!
40    ivis = 0
      return
!
!  accept. return +/- 1
!  if ivis zero, camera was right over xi, eta.
!
45    if(ivis .eq. 0) ivis = isign(1,ifix(t))
      if(lsolid .and. (ivis .eq. -1)) go to 40
      return
      end
      integer function ksyand(ival1,ival2)
!
!***********************************************************************
!
!! KSYAND performs the AND operation on two integer words.
!
!-----------------------begin cray specific code------------------------
!     ksyand=ival1.and.ival2
!------------------------end cray specific code-------------------------
!---------------------begin vax vms specific code-----------------------
      ksyand=iand(ival1,ival2)
!----------------------end vax vms specific code------------------------
!---------------------begin convex specific code------------------------
!     ksyand=iand(ival1,ival2)
!----------------------end convex specific code-------------------------
!--------------------begin alliant specific code------------------------
!     ksyand=iand(ival1,ival2)
!---------------------end alliant specific code-------------------------
!--------------------begin sgi iris specific code-----------------------
!     ksyand=iand(ival1,ival2)
!---------------------end sgi iris specific code------------------------
!-------------------------begin generic code----------------------------
!     ksyand=and(ival1,ival2)
!--------------------------end generic code-----------------------------
      return
      end
      integer function ksyor(ival1,ival2)
!
!***********************************************************************
!
!! KSYOR performs the OR operation on two integer words.
!
!-----------------------begin cray specific code------------------------
!     ksyor=ival1.or.ival2
!------------------------end cray specific code-------------------------
!---------------------begin vax vms specific code-----------------------
      ksyor=ior(ival1,ival2)
!----------------------end vax vms specific code------------------------
!---------------------begin convex specific code------------------------
!     ksyor=ior(ival1,ival2)
!----------------------end convex specific code-------------------------
!--------------------begin alliant specific code------------------------
!     ksyor=ior(ival1,ival2)
!---------------------end alliant specific code-------------------------
!--------------------begin sgi iris specific code-----------------------
!     ksyor=ior(ival1,ival2)
!---------------------end sgi iris specific code------------------------
!-------------------------begin generic code----------------------------
!     ksyor=or(ival1,ival2)
!--------------------------end generic code-----------------------------
      return
      end
      integer function ksyshl(item,jtem)
!
!***********************************************************************
!
!! KSYSHL shifts an integer word left.
!
!-----------------------begin cray specific code------------------------
!     ksyshl=ishft(item,jtem)
!------------------------end cray specific code-------------------------
!---------------------begin vax vms specific code-----------------------
      ksyshl=ishft(item,jtem)
!----------------------end vax vms specific code------------------------
!---------------------begin convex specific code------------------------
!     ksyshl=ishft(item,jtem)
!----------------------end convex specific code-------------------------
!--------------------begin alliant specific code------------------------
!     ksyshl=ishft(item,jtem)
!---------------------end alliant specific code-------------------------
!--------------------begin sgi iris specific code-----------------------
!     ksyshl=ishft(item,jtem)
!---------------------end sgi iris specific code------------------------
!-------------------------begin generic code----------------------------
!     ksyshl=lshift(item,jtem)
!--------------------------end generic code-----------------------------
      return
      end
      integer function ksyshr(item,jtem)
!
!***********************************************************************
!
!! KSYSHR shifts an integer word right.
!
!-----------------------begin cray specific code------------------------
!     ksyshr=ishftc(item,64-jtem,64)
!------------------------end cray specific code-------------------------
!---------------------begin vax vms specific code-----------------------
      ksyshr=ishftc(item,32-jtem,32)
!----------------------end vax vms specific code------------------------
!---------------------begin convex specific code------------------------
!     ksyshr=ishftc(item,32-jtem,32)
!----------------------end convex specific code-------------------------
!--------------------begin alliant specific code------------------------
!     ksyshr=ishftc(item,32-jtem,32)
!---------------------end alliant specific code-------------------------
!--------------------begin sgi iris specific code-----------------------
!     ksyshr=ishft(item,-jtem)
!---------------------end sgi iris specific code------------------------
!-------------------------begin generic code----------------------------
!     ksyshr=rshift(item,jtem)
!--------------------------end generic code-----------------------------
      return
      end
      integer function ksyxor(item,jtem)
!
!***********************************************************************
!
!! KSYXOR performs the EXCLUSIVE OR of two integer words.
!
      integer item
      integer jtem
      integer ksyxor
!
!  For Cray UNICOS, VAX/VMS:
!
      ksyxor = ieor ( item, jtem )
!
!  Generic code:
!
!     ksyxor = xor ( item, jtem )

      return
      end
      subroutine laxis(alow,ahigh,maxtck,bmin,bmax,btick)
!
!***********************************************************************
!
!! LAXIS finds a suitable tick for logarithmic axes.
!
      smlrel = 1.1754944E-38
!
      blow = alog10( max (smlrel, min (ahigh,alow)))
      bhigh = alog10( max (alow,ahigh,100.0*smlrel))
      range = bhigh-blow
      if ( range .le. 0.01 ) range = 1.0
      istrt = 1
      imax = 5
30    continue
 
      do i=istrt,imax,istrt
        ntcks = int(range/i + 0.999)
        if(ntcks .le. maxtck) go to 60
      end do
 
      istrt = 10
      imax = 80
      go to 30
60    btick = i
      bmin = btick*aint(blow/btick)
      bmax = btick*aint(bhigh/btick)
      if((bmin-blow)/range .gt. 0.001) bmin = bmin - btick
      if((bhigh-bmax)/range .gt. 0.001) bmax = bmax + btick
      return
      end
      subroutine lbtext (text, line1, line2, deltay, xcornr, ycornr,
     &                   hite)
!
!***********************************************************************
!
!! LBTEXT displays a left justified text block. 
!  (level 2-3)
!
!  lbtext composes a left justified text block and displays it at a specified
!  position.  character height and line spacing are assumed constant within
!  the indicated lines of text.
!
!  lbtext was developed as part of the txtleg alternative to the original
!  legend utility (txtblk), and may be used in turn as an alternative to
!  btextl.
!
!  arguments:
!  arg       dim     type    i/o/s     description
!  text    (*) * (*)   c        i       character array containing text
!  with trailing '$'s.  elements
!  line1:line2 will be displayed.
!  line1       -       i        i       first line of text to display.
!  line2       -       i        i       last line of text to display.
!  deltay      -       r        i       space between lines in inches.
!  xcornr      -       r        i       horizontal position of lower left
!  hand corner of text block in inches.
!  ycornr      -       r        i       vertical position of lower left hand
!  corner of text block in inches.
!  hite        -       r        i       text character height in inches.
!
!  method:
!  lbtext works with a simple array of variable length character strings,
!  unlike btextl's packed data structure.  position units are converted
!  from plot inches to virtual units using the utility common block
!  variables.  dsmove and ztext do the rest.
!
      character  text (*) * (*)
      integer line1
      integer line2
      real deltay, hite, xcornr, ycornr
      integer kzlevl, kzbegn, kzor
      real zzunit, zzpagx, zzpagy, zzpagr, uupagx, uupagy
      real kzpage, kzauto, zzxor, zzyor, uuxor, uuyor, xcm0, xcm1
      real ycm0, ycm1
      character  czalfl * 1, czalfn * 5, czalfs * 5
      integer i
      real factor
      real gap
      real vxc
      real vy
      real vyc
!
      save /clevel/
      save /cmxalf/
      save /cpage/
      save /cphysr/
      save /cunit/
      save /gcclip/
!
      common /clevel/ kzlevl, kzbegn
      common /cmxalf/ czalfl (6), czalfn (6), czalfs
      common /cunit/  zzunit
      common /cpage/  zzpagx, zzpagy, zzpagr, uupagx, uupagy, kzpage,
     &                kzauto
      common /cphysr/ zzxor, zzyor, kzor, uuxor, uuyor
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
!
      if(kzlevl .lt. 2) then
        call errmes ('LBTEXT', 2, 3)
        return
      end if
!
!  convert inches to graphical units.
!
      factor = zzunit * zzpagr
      vxc = xcornr * factor + zzxor + xcm0
      vyc = ycornr * factor + zzyor + ycm0
      gap = (deltay + hite) * factor
!
!  start with upper left hand corner of text block.
!
      vy  = vyc + (line2 - line1) * gap
 
      do i = line1, line2
 
         call dsmove (vxc, vy)
!-----------------------------begin generic code------------------------
!  call ztext (text (i), 100, czalfl, czalfn, czalfs)
!------------------------------end generic code-------------------------
!
!----------------------------begin unicos specific code-----------------
!  ktext = text(i)
!  read (ktext,'(20a8)') itext
!  call ztext (itext, 100, czalfl, czalfn, czalfs)
!------------------------------end unicos specific code-----------------
!
!------------------------begin vax vms specific code--------------------
         call ztext (%ref(text (i)), 100, czalfl, czalfn, czalfs)
!-------------------------end vax vms specific code---------------------
         vy = vy - gap
      end do
 
      return
 
      end
      function ldivds(anumer,adenom)
!
!***********************************************************************
!
!! LDIVDS ...
!
      logical ldivds
!
      if(anumer/adenom .eq. aint(anumer/adenom)) go to 10
      ldivds = .false.
      return
10    ldivds = .true.
      return
      end
      subroutine leghdg(legnam,ilgchr)
!
!***********************************************************************
!
!! LEGHDG changes the legend title.
!  (level 1-3, p/s)
!
!  input:   legnam = title text
!  ilgchr = number of characters in legnam
!
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cmxalf/
      save /cstrng/
      save /gcdchr/
!
      common /clevel/ kzlevl,kzbegn
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      character czalfl*1,czalfn*5,czalfs*5
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5
!
      integer legnam(*)
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzlgtl=20
        call zcopys(legnam,ilgchr,kzlgti,kzlgtl)
        if(kzlgtl.lt.0)then
          kzlgtl=-kzlgtl
          call errmes('LEGHDG',20,4)
        end if
        if(kzlevl.eq.1)then
          uulgtz=uuhite
        else
          zzlgtz=zzhite
        end if
        zzlgtr=zzsrat
        call zloalf(czlgtc,czlgtf,czlgts,czalfl,czalfn,czalfs)
      else
        call errmes('LEGHDG',1,3)
      end if
      return
      end
      function leng(lstr)
!
!***********************************************************************
!
!! LENG finds the character length of a Hollerith string.
!
!  input:   lstr = input string
!
!  output:  leng = length of string in number of characters
!
      parameter (numbyt=4)
!
      integer lstr(*)
!
      i=1
      j=1
 
      do k=1,100
 
        call sybyt4('X',lstr(i),j,num)

        if(num.eq.0)then
          leng=(i-1)*numbyt+j-1
          return
        else
          j=j+1
          if(j.eq.(numbyt+1))then
            j=1
            i=i+1
          end if
        end if
 
      end do
 
      leng=(i-1)*numbyt+j-1

      return
      end
      subroutine lindef(tleng,nmrksp,ratray)
!
!***********************************************************************
!
!! LINDEF defines a new line style.
!  level 1-3, p/s
!
!  input:   tleng  = overall length in inches
!  mnrksp = total number of marks and spaces
!  (limited to maximum of 12 in dissim)
!  ratray = array containing ratios of marks and
!  spaces to overall length
!
      real zzin
      parameter (zzin=2.54)
!
      logical linilt, lposnd, lnew
      real ratray(*)
!
      save /clevel/
      save /cline/
      save /cpage/
      save /cunit/
      save /dcltyp/
!
      common /clevel/ kzlevl,kzbegn
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzlncn=kzlncn+1
        if(kzlncn.gt.15) kzlncn=15
        sum=0.0
        nval=min(12,abs(nmrksp))
 
        do i=1,nval
          sum=sum+ratray(i)
        end do
 
        if(kzlevl.eq.1)then
          factor=tleng*zzin
        else
          if(mod(nmrksp,2).eq.1) sum=sum+ratray(2)
          factor=tleng*zzin*zzpagr/sum
        end if
!
!  define marks and spaces in dist()
!
        do i=1,nval
          dist(i,kzlncn)=ratray(i)*factor
        end do
 
        if(mod(nval,2).eq.1)then
          nval=nval+1
          dist(nval,kzlncn)=dist(2,kzlncn)
        end if
!
!  enforce minimum allowable space
!
        do i=1,nval
          if(dist(i,kzlncn).lt.0.0254)then
            dist(i,kzlncn)=0.0254
          end if
        end do
 
        dist(13,kzlncn)=nval
!
!  for pen down first
!
        if(nmrksp.lt.0)then
          temp=dist(1,kzlncn)
          do i=1,nval-1
            dist(i,kzlncn)=dist(i+1,kzlncn)
          end do
          dist(nval,kzlncn)=temp
        end if
!
!  check if line style exists
!
        lnew=.true.
        if(kzlncn.ne.5)then
 
          do j=5,kzlncn-1
 
            do i=1,13
              lnew=lnew.and.
     &             (abs(dist(i,kzlncn)-dist(i,j)).lt.0.00005)
            end do
 
            if(lnew)then
              kzlncn=j
              go to 700
            end if
 
          end do
 
        end if
 
 700    continue
        ilntyp=kzlncn+1
        linilt=.true.
      else
        call errmes('LINDEF',1,3)
      end if
      return
      end
      subroutine linlab(num,iexp,strng,lrmtex)
!
!***********************************************************************
!
!! LINLAB...
!
      character bzero(4)
      logical lrmtex
      character*(*) strng
!
      save bzero
!
      data bzero /'0', '.', '0', '0'/
!
      lrmtex = .true.
      nval = num
      if(iexp .ge. -2 .and. iexp .le. 2) lrmtex = .false.
      if(iexp .gt. 0 .and. (.not. lrmtex)) nval = nval*10**iexp
      call numstr(nval,strng)
 
      if((nval .eq. 0) .or. lrmtex .or. (iexp .ge. 0)) return
!
!  number is in range 10**-1 or 10**-2, so format pretty
!
      n = -iexp
      l = numchr(strng)
      izbgn = 1
      nin = 3
      if(n .eq. l) nin = 2
!
!  if n<l then we need only insert a decimal point
!
      if(n .lt. l) then
        izbgn = 2
        nin = 1
      end if
!
!  allow room for decimal point and zero(s) if necessary
!
      istart = 1
      if(num .lt. 0) istart = istart + 1
      ibegin = istart + max(0,l-n)
 
      do i = 0, min(n,l)
        strng(istart+l+nin-i:istart+l+nin-i) =
     &    strng(istart+l-i:istart+l-i)
      end do
!
!  insert leading zeros if necessary, or just decimal point
!
       do i=0,nin-1
         strng(ibegin+i:ibegin+i) = bzero(izbgn+i)
       end do
 
      return
      end
      subroutine loglab(num,strng)
!
!***********************************************************************
!
!! LOGLAB...
!
      character strng(*)
      character llabs(6,8)
!
      save llabs
!
      data llabs / '.','0','0','1',' ',' ',
     &             '.','0','1',' ',' ',' ',
     &             '.','1',' ',' ',' ',' ',
     &             '1',' ',' ',' ',' ',' ',
     &             '1','0',' ',' ',' ',' ',
     &             '1','0','0',' ',' ',' ',
     &             '1','0','0','0',' ',' ',
     &             '1','0','0','0','0',' '/
!
      if(num .le. -4 .or. num .ge. 5) then

        strng(1) = '1'
        strng(2) = 'E'
        call numstr(num,strng(3))

      else
 
        do j=1,6
          strng(j) = llabs(j,4+num)
        end do

      end if
 
      return
      end
      subroutine loglog(xorig,xcyc,yorig,ycyc)
!
!***********************************************************************
!
!! LOGLOG defines a log-log coordinate system.
!  (level 2, raise to level 3)
!
!  input:   xorig,yorig = lower bounds of x and y
!  values respectively
!  xcyc,ycyc   = cycle length in inches
!  for x and y
!
      parameter (kzlog=3)
!
      logical logx, logy
!
      save /carea/
      save /caxis/
      save /clabel/
      save /clevel/
      save /cpage/
      save /cunit/
      save /pltcom/
!
      common /clevel/ kzlevl,kzbegn
      common /cunit/  zzunit
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
!
      if(kzlevl.eq.2)then
        logx=.false.
        logy=.false.
        xaxis=zzxaxs/zzunit/zzpagr
        yaxis=zzyaxs/zzunit/zzpagr
        xpos=0.0
        ypos=0.0
        iaxes=3
        kzxtyp=kzlog
        kzytyp=kzlog
        call zdrlog(xorig,xcyc,yorig,ycyc,xaxis,yaxis,xpos,ypos,iaxes)
        kzlevl=3
      else
        call errmes('LOGLOG',2,0)
      end if
 
      return
      end
      subroutine look(z,ii,jj,m,iz,nz,izdim)
!
!***********************************************************************
!
!! LOOK looks for a contour starting at the point (ii,jj).
!
!  The contour should be oriented such that the point (ii,jj) is
!  greater than the current contouring level, and its neighbor
!  (specified by m) is less than the current contouring level.
!
      integer iz(izdim,2)
      dimension z(nz,2)
!
      save /contr/
!
      common /contr/ clevel,iold,jold,in,jn,
     &   nx,ny,xl,dx,yl,dy
      dimension idmode(3,4)
!
      save idmode
!
      data idmode/4,1,2,  1,2,3,  2,3,4,  3,4,1/
!
      iold=ii
      jold=jj
      mode=m
      call newp(1,mode)
!
!  look for contour starting here.   the "old" point is always the
!  positive one, so the test is easy.
!
      if(z(iold,jold) .ge. clevel .and. z(in,jn) .lt. clevel) go to 20
      return
!
!  check for contour previously thru here - "segmnt" returns the point
!  we mark when ever a contour passes thru the points "(iold,jold)"
!  and "(in,jn)".   "segmnt" also returns the mark that should be
!  placed given the orientation of the contour.
!
20    call segmnt(ici,icj,iseg)
 
      item = iz(ici,icj)
 
      if(iseg .eq. 1)then
        item = item - item/2*2
        if(item .eq. 1) return
      else
        item = item/2*2
        if(item .eq. 2) return
      end if
!
!  new contour.   trace it till it ends by looping back on itself, or
!  running off the grid.
!
      call zpnt(xx,yy,z,nz)
      call scale(xx,yy,vx,vy)
      call gsmove(vx,vy)
      iold=in
      jold=jn
30    continue
 
      do n=2,4
        call newp(n,mode)
        if(in .lt. 1 .or. in .gt. nx) return
        if(jn .lt. 1 .or. jn .gt. ny) return
        if(sign(1.0,z(iold,jold)-clevel) .ne.
     &    sign(1.0,z(in,jn)-clevel)) go to 60
        iold=in
        jold=jn
      end do
!
!  it is impossible to just fall thru due to the algorithm
!
       write(*,*)'LOOK - Fatal error!'
       stop
60     continue
!
!  found the next intersection.   see if it has already been marked.
!  if so, then we are done, else, mark it, draw to it, and continue on.
!
       call segmnt(ici,icj,iseg)
      item = iz(ici,icj)
 
      if(iseg .eq. 1)then
        item = item - item/2*2
        if(item .eq. 1) return
      else
        item = item/2*2
        if(item .eq. 2) return
      end if
 
      if(iseg .eq. 1)then
        iz(ici,icj) = iz(ici,icj)/2*2 + iseg
      else
        iz(ici,icj) = iz(ici,icj) - iz(ici,icj)/2*2 + iseg
      end if
 
      call zpnt(xx,yy,z,nz)
      call scale(xx,yy,vx,vy)
      call gsdraw(vx,vy)
      mode=idmode(n-1,mode)
      go to 30
      end
      subroutine mapit(xlow,xhigh,ylow,yhigh,xlab,ylab,title,iaxes)
!
!***********************************************************************
!
!! MAPIT draws the axes, and labels the axes and the plot.
!
      real delmx
      integer iaxes
      logical logt
      logical logx
      logical logxx
      logical logy
      logical logyy
      logical lraggd
      logical lrmtex
      logical lshort
      character nequiv(14)
      character numbr*14
      character*(*) title
      real tminld
      real xhigh
      character*(*) xlab
      real xlow
      real yhigh
      character*(*) ylab
      real ylow
      real zlog(8)
!
      save /clevel/
      save /pltclp/
      save /pltcom/
      save /pltprm/
      save /pltsiz/
      save shortf
      save tminld
      save zlog
!
      common /clevel/ kzlevl,kzbegn
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      common /pltclp/ xmin,xmax,ymin,ymax
      common /pltprm/ cxsize, cysize, tickln, yvini
!
      equivalence(numbr,nequiv)
!
      data zlog /0.3010, 0.4771, 0.6021, 0.6990, 0.7782, 0.8451,
     &   0.9031, 0.9542 /
      data tminld /0.1/
      data shortf /2.0/
!
      delmx=0.0
      yvlen = yvini
!
!  Set logx and logy to false for our usage of scale
!
      logx = .false.
      logy = .false.
!
!  see what type of axes are desired
!
      logxx = ksyand(iaxes,1) .ne. 0
      logyy = ksyand(iaxes,2) .ne. 0
      lraggd = ksyand(iaxes,256) .ne. 0
!
!  Do the axes scaling
!
      cxsize = gslens('0')
      cysize = gslens('0')

      numtk = min ( 10, int( xvlen / ((ilabsz()+1.0)*cxsize) ) )

      if ( .not. logxx) then
        lshort = ksyand(iaxes,16) .ne. 0
        call axis(xlow,xhigh,numtk,lshort,lraggd,xmin,xmax,xtmin,xtmax,
     &    xtick,ixpwr)
      else
        call laxis(xlow,xhigh,numtk,xmin,xmax,xtick)
        xtmin = xmin
        xtmax = xmax
        ixpwr = 0
      end if

      numtk = min (10,int(yvlen/(3.0*cysize)) )

      if ( .not. logyy ) then
        lshort = ksyand(iaxes,32) .ne. 0
        call axis(ylow,yhigh,numtk,lshort,lraggd,ymin,ymax,ytmin,ytmax,
     &    ytick,iypwr)
      else
        call laxis(ylow,yhigh,numtk,ymin,ymax,ytick)
        ytmin = ymin
        ytmax = ymax
        iypwr = 0
      end if
!
!  set up scaling factors for scale
!
      ux0 = xmin
      udx = xmax - xmin
      uy0 = ymin
      udy = ymax - ymin
!
!  draw y axes
!
      call gssetc(cysize,0.0)
      logt = .false.

      if( logyy .and. ytick .eq. 1.0) then
        call scale(xmin,ymin,vx,temp)
        call scale(xmin,ymin+1.0-zlog(8),vx,vy)
        if((vy-temp) .ge. tminld) logt = .true.
      end if
!
!  draw y axis line
!
      tenexp = 10.0**iypwr
      x = xmin
      ticksp = max (0.0,tickln)
      if(ksyand(iaxes,64) .ne. 0) yvlen = yvlen - ticksp
      tcksgn = -tickln
100   continue
      call scale(x,ymax,vx,vy)
      call gsmove(vx,vy)
      call scale(x,ymin,vx,vy)
      call gsdraw(vx,vy)
!
!  draw and label y axis ticks
!
      y = ytmin
      n = int((ytmax-ytmin)/ytick + 1.1)
110   continue
      call scale(x,y*tenexp,vx,vy)
      call gsmove(vx,vy)
      call gsdraw(vx+tcksgn,vy)
      if(x .eq. xmax) go to 185
      if(ksyand(iaxes,1024) .ne. 0) go to 183
!
!  place the appropiate label
!
      if ( .not.logyy) then
        call linlab(int(y),iypwr,numbr,lrmtex)
      else
        call loglab(int(y),numbr)
      end if

      del = gslens(numbr) + cxsize*0.25
      delmx = max (del,delmx)
      call gsmove(vx-ticksp-del,vy-cysize/2.0)
      call gspstr(numbr)
!
!  add grid line at tick if desired
!
183   continue
      if(ksyand(iaxes,8) .eq. 0) go to 185
      call gsltyp(3)
      call gsmove(vx,vy)
      call scale(xmax,y*tenexp,vx,vy)
      call gsdraw(vx,vy)
      call gsltyp(1)
185   continue
!
!  do extra ticking if extra ticks will be far enough apart
!
      if((.not. logt) .or. (y .eq. ytmax)) go to 200
 
      do j = 1, 8
        call scale(x,y+zlog(j),vx,vy)
        call gsmove(vx,vy)
        call gsdraw(vx+tcksgn/shortf,vy)
      end do
 
200   continue
      y = y + ytick
      n = n-1
      if(n .gt. 0) go to 110
      if(x .eq. xmax) go to 300
!
!  if linear axis, place remote exponent if needed
!
      if(logyy .or. (.not. lrmtex)) go to 260
      if(ksyand(iaxes,1024) .ne. 0) go to 260
      call scale(xmin,(ytmin+ytick/2.0)*tenexp,vx,vy)
      numbr(1:1) = 'E'
      call numstr(iypwr,nequiv(2))
      call gsmove(vx-(0.5*cxsize+gslens(numbr)),vy-cysize/2.0)
      call gspstr(numbr)
!
!  now place y label
!
260   call scale(xmin,(ymin+ymax)/2.0,vx,vy)
      call gsmove(vx-delmx-ticksp-cysize,
     &   vy-gslens(ylab)/2.0)
      call gssetc(cysize,90.0)
      call gspstr(ylab)
      call gssetc(cysize,0.0)
      if(ksyand(iaxes,128) .eq. 0) go to 300
      x = xmax
      tcksgn = tickln
      go to 100
300   continue
!
!  draw x axis
!
      logt = .false.

      if ( logxx .and. xtick .eq. 1.0) then
        call scale(xmin,ymin,temp,vy)
        call scale(xmin+1.0-zlog(8),ymin,vx,vy)
        if((vx-temp) .ge. tminld) logt = .true.
      end if
!
!  draw x axis line
!
      y = ymin
      tcksgn = -tickln
      tenexp = 10.0**ixpwr
      ticksp = max (0.5*cysize,tickln)
320   continue
      call scale(xmin,y,vx,vy)
      call gsmove(vx,vy)
      call scale(xmax,y,vx,vy)
      call gsdraw(vx,vy)
!
!  draw and label x axis ticks
!
      x = xtmin
      n = int((xtmax-xtmin)/xtick + 1.1)
400   continue
      call scale(x*tenexp,y,vx,vy)
      call gsmove(vx,vy)
      call gsdraw(vx,vy+tcksgn)
      if(y .eq. ymax) go to 430
      if(ksyand(iaxes,512) .ne. 0) go to 423
      if(logxx) go to 410
      call linlab(int(x),ixpwr,numbr,lrmtex)
      go to 420
410   call loglab(int(x),numbr)
420   call gsmove(vx-gslens(numbr)/2.0,vy-ticksp-1.5*cysize)
      call gspstr(numbr)
!
!  add grid line at tick if desired
!
423   continue
      if(ksyand(iaxes,4) .eq. 0) go to 430
      call gsltyp(3)
      call gsmove(vx,vy)
      call scale(x*tenexp,ymax,vx,vy)
      call gsdraw(vx,vy)
      call gsltyp(1)
430   continue
!
!  do extra ticking if extra ticks will be far enough apart
!
      if((.not. logt) .or. (x .eq. xtmax)) go to 490
 
      do j = 1, 8
        call scale(x+zlog(j),y,vx,vy)
        call gsmove(vx,vy)
        call gsdraw(vx,vy+tcksgn/shortf)
      end do
 
490   continue
      x = x + xtick
      n = n-1
      if(n .gt. 0) go to 400
      if(y .eq. ymax) go to 590
!
!  now place remote exponent if needed on linear axis
!
      if(logxx .or. (.not. lrmtex)) go to 520
      if(ksyand(iaxes,512) .ne. 0) go to 520
      call scale(xmin,ymin,vx,vy)
      numbr(1:1) = 'E'
      call numstr(ixpwr,nequiv(2))
      call gsmove(vx+3*cxsize,vy-ticksp-2.75*cysize)
      call gspstr(numbr)
!
!  now place x axis label
!
520   call scale((xmin+xmax)/2.0,ymin,vx,vy)
      call gsmove(vx-gslens(xlab)/2.0,vy-ticksp-4.0*cysize)
      call gspstr(xlab)
      if(ksyand(iaxes,64) .eq. 0) go to 590
      y = ymax
      tcksgn = tickln
      go to 320
590   continue
!
!  place title
!
      call scale((xmin+xmax)/2.0,ymax,vx,vy)
      tcksgn = 0.0
      if(ksyand(iaxes,64) .ne. 0) tcksgn = ticksp
      call gsmove(vx-gslens(title)/2.0,vy+tcksgn+cysize)
      call gspstr(title)
!
!  make sure "pltclp" contains limits picked by mapit.   only
!  maintained for callers info.
!
      if(logxx)then
        xmin = 10.0**xmin
        xmax = 10.0**xmax
        logx = .true.
      end if
!
      if(logyy)then
        ymin = 10.0**ymin
        ymax = 10.0**ymax
        logy = .true.
      end if
!
!  jvb
!
      kzlevl=3
!
      return
      end
      subroutine mapprm(xleft,xright,ybot,ytop,csize,tkln,lraxis)
!
!***********************************************************************
!
!! MAPPRM ...
!
      logical lraxis
!
      save /gcdprm/
      save /pltprm/
      save /pltsiz/
!
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      common /pltprm/ cxsize, cysize, tickln, yvini
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
!
      xbordr = 0.25/xs
      ybordr = 0.25/ys
      call gssetc(csize,0.0)
      cxsize = gslens('0')
      cysize = csize
      tickln = tkln
      ticksp = max (0.0,tickln)
      tlabln = ilabsz()+0.25
      xvstrt = xleft + ticksp + tlabln*cxsize + 2.0*cysize + xbordr
      xvlen = xright - xvstrt - (tlabln/2.0)*cxsize - xbordr
      if(lraxis) xvlen = xvlen - (ticksp + tlabln*cxsize + 2.0*cysize)
      ticksp = max (0.5*cysize,tickln)
      yvstrt = ybot + ticksp + 4.25*cysize + ybordr
      yvini = ytop - yvstrt - 2.0*cysize - ybordr
 
      return
      end
      subroutine mapset(xleft,xright,ybot,ytop,csize,tkln,lraxis)
!
!***********************************************************************
!
!! MAPSET...
!
      logical lraxis
!
      save /gcdchr/
!
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd
!
      call mapprm(xleft*xlencm/100.0,xright*xlencm/100.0,
     &   ybot*ylencm/100.0,ytop*ylencm/100.0,csize,tkln,lraxis)
 
      return
      end
      subroutine mapsiz(xlpct,xrpct,ybpct,ytpct,chrsiz)
!
!***********************************************************************
!
!! MAPSIZ defines the range of the user coordinate system.
!
!  This range will be used in subsequent calls to mapit.
!
!  input, real xlpct, xrpct, are the locations of the
!  minimum and maximum x coordinates, expressed as percentages
!  of the available plotting area.
!
!  input, real ybpct, ytpct, are the locations of the
!  minimum and maximum y coordinates, expressed as percentages
!  of the available plotting area.
!
!  input, real chrsiz, is the size of characters to use.
!  chrsiz may be given as 0, in which case mapsiz will choose
!  an appropriate value, via a call to goodcs.
!
      save /gcdprm/
!
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
!
      xleft = vxl + (vxh-vxl)*xlpct/100.0
      xright = vxl + (vxh-vxl)*xrpct/100.0
      ybot = vyl + (vyh-vyl)*ybpct/100.0
      ytop = vyl + (vyh-vyl)*ytpct/100.0
      csize = chrsiz
      if(csize .eq. 0.0)
     &   csize = goodcs( max (0.3, min (ytop-ybot,xright-xleft)/80.0))
      call mapprm(xleft,xright,ybot,ytop,csize,0.9*csize,.false.)
      return
      end
      subroutine mapsml(xlow,xhigh,ylow,yhigh,xlab,ylab,title,iaxes)
!
!***********************************************************************
!
!! MAPSML is a simplified version of MAPIT.
!
!  the following options have been commented out:
!
!  option  comment chars  added line cmnt chars
!  ------  -------------  ---------------------
!  grid lines  cc   @!
!  log axes  ccc   @!!
!  boxed plot  cccc   @!!!
!
      character xlab*2, ylab*2, title*2
      character numbr*14
      character nequiv(14)
      logical logx, logy
      logical lrmtex, lshort, lraggd
!
      save /pltclp/
      save /pltcom/
      save /pltprm/
      save /pltsiz/
!
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      common /pltclp/ xmin,xmax,ymin,ymax
      common /pltprm/ cxsize, cysize, tickln, yvini
!
      equivalence(numbr,nequiv)
!
      yvlen = yvini
!
!  set logx and logy to false for our usage of scale
!
      logx = .false.
      logy = .false.
!
!  see what type of axes are desired
!
      lraggd = ksyand(iaxes,256) .ne. 0
!
!  do the axes scaling
!
      numtk = min ( 10, int(xvlen/((ilabsz()+1.0)*cxsize)) )
      lshort = ksyand(iaxes,16) .ne. 0
      call axis(xlow,xhigh,numtk,lshort,lraggd,xmin,xmax,xtmin,xtmax,
     &   xtick,ixpwr)
      numtk = min0(10,int(yvlen/(3.0*cysize)))
      lshort = ksyand(iaxes,32) .ne. 0
      call axis(ylow,yhigh,numtk,lshort,lraggd,ymin,ymax,ytmin,ytmax,
     &   ytick,iypwr)
!
!  set up temporary scaling factors
!
      ux0 = xmin
      udx = xmax - xmin
      uy0 = ymin
      udy = ymax - ymin
!
!  draw y axes
!
      call gssetc(cysize,0.0)
!
!  draw y axis line
!
      mxlab = 3
      tenexp = 10.0**iypwr
      x = xmin
      ticksp = max (0.0,tickln)
      tcksgn = -tickln
100   continue
      call scale(x,ymax,vx,vy)
      call gsmove(vx,vy)
      call scale(x,ymin,vx,vy)
      call gsdraw(vx,vy)
!
!  draw and label y axis ticks
!
      y = ytmin
      n =int( (ytmax-ytmin)/ytick + 1.1 )
110   continue
      call scale(x,y*tenexp,vx,vy)
      call gsmove(vx,vy)
      call gsdraw(vx+tcksgn,vy)
      if(ksyand(iaxes,1024) .ne. 0) go to 183
!
!  place the appropiate label
!
      call linlab(int(y),iypwr,numbr,lrmtex)
180   continue
      ln = numchr(numbr)
      mxlab = max(mxlab,ln)
      call gsmove(vx-ticksp-cxsize*(ln+0.25),vy-cysize/2.0)
      call gspstr(numbr)
!
!  add grid line at tick if desired
!
183   continue
185   continue
!
!  do extra ticking if extra ticks will be far enough apart
!
200   continue
      y = y + ytick
      n = n-1
      if(n .gt. 0) go to 110
!
!  if linear axis, place remote exponent if needed
!
      if(.not. lrmtex) go to 260
      if(ksyand(iaxes,1024) .ne. 0) go to 260
      call scale(xmin,(ytmin+ytick/2.0)*tenexp,vx,vy)
      call zscopy('E',numbr)
      call numstr(iypwr,nequiv(2))
      call gsmove(vx-cxsize*(numchr(numbr)+0.5),vy-cysize/2.0)
      call gspstr(numbr)
!
!  now place y label
!
260   call scale(xmin,(ymin+ymax)/2.0,vx,vy)
      call gsmove(vx-(mxlab+0.25)*cxsize-ticksp-cysize,
     &   vy-cxsize*numchr(ylab)/2.0)
      call gssetc(cysize,90.0)
      call gspstr(ylab)
      call gssetc(cysize,0.0)
300   continue
!
!  draw x axis
!
!  draw x axis line
!
      y = ymin
      tcksgn = -tickln
      tenexp = 10.0**ixpwr
      ticksp = max (0.5*cysize,tickln)
320   continue
      call scale(xmin,y,vx,vy)
      call gsmove(vx,vy)
      call scale(xmax,y,vx,vy)
      call gsdraw(vx,vy)
!
!  draw and label x axis ticks
!
      x = xtmin
      n = int((xtmax-xtmin)/xtick + 1.1)
400   continue
      call scale(x*tenexp,y,vx,vy)
      call gsmove(vx,vy)
      call gsdraw(vx,vy+tcksgn)
      if(ksyand(iaxes,512) .ne. 0) go to 423
      call linlab(int(x),ixpwr,numbr,lrmtex)
420   call gsmove(vx-cxsize*numchr(numbr)/2.0,vy-ticksp-1.5*cysize)
      call gspstr(numbr)
!
!  add grid line at tick if desired
!
423   continue
!
!  do extra ticking if extra ticks will be far enough apart
!
      x = x + xtick
      n = n-1
      if(n .gt. 0) go to 400
!
!  now place remote exponent if needed on linear axis
!
      if(.not. lrmtex) go to 520
      if(ksyand(iaxes,512) .ne. 0) go to 520
      call scale(xmin,ymin,vx,vy)
      call zscopy('E',numbr)
      call numstr(ixpwr,nequiv(2))
      call gsmove(vx+3*cxsize,vy-ticksp-2.75*cysize)
      call gspstr(numbr)
!
!  now place x axis lable
!
520   call scale((xmin+xmax)/2.0,ymin,vx,vy)
      call gsmove(vx-cxsize*numchr(xlab)/2.0,vy-ticksp-4.0*cysize)
      call gspstr(xlab)
!
!  place title
!
      call scale((xmin+xmax)/2.0,ymax,vx,vy)
      tcksgn = 0.0
      call gsmove(vx-cxsize*numchr(title)/2.0,vy+tcksgn+cysize)
      call gspstr(title)
!
!  make sure "pltclp" contains limits picked by mapit.   only maintained
!  for callers info.
!
!
!  tell scale about log axis scaling now
!
      return
      end
      subroutine mapsz2(xlpct,xrpct,ybpct,ytpct,chrsiz)
!
!***********************************************************************
!
!! MAPSZ2 ...
!
      save /gcdprm/
!
      common /gcdprm/ xs, ys, xt, yt, rcos, rsin, vxl, vxh, vyl, vyh
!
      xleft = vxl + (vxh-vxl)*xlpct/100.0
      xright = vxl + (vxh-vxl)*xrpct/100.0
      ybot = vyl + (vyh-vyl)*ybpct/100.0
      ytop = vyl + (vyh-vyl)*ytpct/100.0
      csize = chrsiz
      if(csize .eq. 0.0)
     &   csize = goodcs( max (0.3, min (ytop-ybot,xright-xleft)/80.0))
      call mapprm(xleft,xright,ybot,ytop,csize,0.9*csize,.true.)
      return
      end
      subroutine margin(gracem)
!
!***********************************************************************
!
!! MARGIN sets the grace margin.
!  (level 1-3, p/s)
!
!  input:   gracem = grace margin
!
      save /carea/
      save /clevel/
      save /cpage/
      save /cunit/
!
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /cunit/  zzunit
      common /clevel/ kzlevl,kzbegn
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
!
      if(kzlevl.eq.1)then
        uugrce=gracem*zzunit
      elseif(kzlevl.eq.2.or.kzlevl.eq.3)then
        uugrce=gracem*zzunit
        zzgrce=gracem*zzunit*zzpagr
      else
        call errmes('MARGIN',1,3)
      end if
      return
      end
      subroutine marker(isym)
!
!***********************************************************************
!
!! MARKER defines the marker type.
!  level 1-3, p/s
!
!  input:   isym = marker id, from 0 to 18
!
      save /clevel/
      save /csymbo/
!
      common /clevel/ kzlevl,kzbegn
      common /csymbo/ kzsym,kznsym,zzsmsz,uusmsz
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzsym=isym
      else
        call errmes('MARKER',1,3)
      end if
      return
      end
      function maxlin(ip,iplen,llen)
!
!***********************************************************************
!
!! MAXLIN sets the line length of packed array of characters.
!  (level 1-3)
!
!  input:   ip    = character array
!  iplen = dimension of ip
!  llen  = number of characters in longest lines
!
!  output:  maxlin = maximum number of lines that can be
!  packed in ip
!
      integer kzyes
      parameter (kzyes=111)
!
      integer ip(*)
!
      save /cdevic/
      save /clevel/
!
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /clevel/ kzlevl,kzbegn
!
      save maxip,maxlen
!
      data maxip /20000/
      data maxlen /300/
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        ip(1)=kzyes
!
!  illegal line length
!
        if(llen.lt.0.or.llen.gt.maxlen)then
          ip(2)=kzyes
          maxlin=0
          call errmes('MAXLIN',llen,4)
          return
        end if
        nword=(llen-1)/kzbyte+5
!
!  array too small or too large
!
        if(nword.gt.iplen.or.iplen.gt.maxip)then
          ip(2)=kzyes
          maxlin=0
          call errmes('MAXLIN',iplen,5)
          return
        end if
        ip(2)=iplen*1000+llen
        maxlin=(iplen-2)/nword
      else
        maxlin=0
        call errmes('MAXLIN',1,3)
      end if
      return
      end
      subroutine messag ( lmess, imess, xpos, ypos)

!*********************************************************************72
!
!! MESSAG writes a Hollerith string on screen.
!  (level 2-3)
!
!  input:   lmess     = Hollerith string
!  imess     = number of character in lmess
!  xpos,ypos = distance from physical origin
!  in inches for x and y
!
      parameter (numbyt=4)
!
      integer lmess(*)
      character czalfl*1,czalfn*5,czalfs*5,khar*(numbyt)
!
      save /clevel/
      save /cmess/
      save /cpage/
      save /cphysr/
      save /cunit/
      save /gcclip/
      save /gcvpos/
!
      common /clevel/ kzlevl,kzbegn
      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /gcvpos/ xvpos, yvpos
!
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
!
!  calculate virtual coordinates
!
        temp=xpos
        call win2ch(temp,khar)

        if(khar(1:4).eq.'ABUT')then
          vx=zzabux
        else
          vx=xcm0+zzxor+xpos*zzunit*zzpagr
        end if

        temp=ypos
        call win2ch(temp,khar)

        if(khar(1:4).eq.'ABUT')then
          vy=zzabuy
        else
          vy=ycm0+zzyor+ypos*zzunit*zzpagr
        end if
!
!  draw text
!
        call dsmove(vx,vy)
        call ztext(lmess,imess,czalfl,czalfn,czalfs)
        zzabux=xvpos
        zzabuy=yvpos
!
!  wrong level
!
      else
        call errmes('MESSAG',3,0)
      end if
 
      return
      end
      subroutine minmax(array,npts,bmin,bmax)
!
!*********************************************************************72
!
!! MINMAX finds the minimum and maximum of the array.
!
      integer npts
      real array(npts)
!
      bmin = array(1)
      bmax = bmin
 
      do i=2,npts
        bmin = min(bmin,array(i))
        bmax = max(bmax,array(i))
      end do
 
      return
      end
      subroutine mixalf(lalpha)
!
!*********************************************************************72
!
!! MIXALF sets the mix alphabet set.
!  (level 1-3, p/s)
!
!  input:   lalpha = character set
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      character czalfl*1,czalfn*5,czalfs*5
!
      character*(*) lalpha
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfl(2)='('
        czalfn(2)=lalpha(1:5)
        call capchr(czalfn(2))
        if(czalfl(1).eq.' ') then
          call defalf('STAND')
        end if
      else
        call errmes('MIXALF',1,3)
      end if

      return
      end
      subroutine movori(xorel,yorel)
!
!***********************************************************************
!
!! MOVORI moves the physical origin relative to its current position.
!  level 1, p/s
!
!  input:   xorel,yorel = new position from current
!  physical origin in inches
!
      parameter (kzyes=111)
!
      save /clevel/
      save /cphysr/
      save /cunit/
!
      common /clevel/ kzlevl,kzbegn
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cunit/  zzunit
!
      uuxor=uuxor+xorel*zzunit
      uuyor=uuyor+yorel*zzunit
      kzor=kzyes
!
!  wrong level
!
      if(kzlevl.ne.1)then
        call errmes('MOVORI',1,0)
      end if
      return
      end
      function nchray(dummy)
!
!***********************************************************************
!
!! NCHRAY returns the number of characters in a character*1 array.
!
!  input:      dummy      input array
!
      character*1 dummy(*)
!
      do i=1,160
        ii=i
        if(ichar(dummy(ii)) .eq. 0) go to 200
      end do
 
      nchray=0
      return
 200  continue
      nchray=ii-1
      return
      end
      subroutine newp(i,m)
!
!***********************************************************************
!
!! NEWP...
!
      save /contr/
!
      common /contr/ clevel,iold,jold,in,jn,
     &   nx,ny,xl,dx,yl,dy
      dimension ideli(4),jdelj(4)
!
      save ideli,jdelj
      data ideli,jdelj / 0,1,0,-1,   1,0,-1,0/
!
      index=mod(2+i+m,4)+1
      in=iold+ideli(index)
      jn=jold+jdelj(index)

      return
      end
      subroutine nobord
!
!***********************************************************************
!
!! NOBORD sets the border drawing flag.
!  level 1, p/s
!
      parameter (kzno=222)
!
      save /cborch/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /cborch/ kzbrdr,kzchek
!
      if(kzlevl.eq.1)then
        kzbrdr=kzno
      else
        call errmes('NOBORD',1,0)
      end if
      return
      end
      subroutine nochek
!
!***********************************************************************
!
!! NOCHEK sets the out-of-range checking flag.
!  level 1-3, p/s
!
      parameter (kzno=222)
!
      save /cborch/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /cborch/ kzbrdr,kzchek
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzchek=kzno
      else
        call errmes('NOCHEK',1,3)
      end if
      return
      end
      subroutine noxlbl
!
!***********************************************************************
!
!! NOXLBL suppresses the X axis numbers.
!  level 1-3, p/s
!
!
      parameter (kzyes=111)
!
      save /caxis/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzxnon=kzyes
      else
        call errmes('NOXLBL',1,3)
      end if
      return
      end
      subroutine noylbl
!
!***********************************************************************
!
!! NOYLBL suppresses the Y ayis numbers.
!  level 1-3, p/s
!
      parameter (kzyes=111)
!
      save /caxis/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzynon=kzyes
      else
        call errmes('NOYLBL',1,3)
      end if
      return
      end
      function numchr ( string )
!
!***********************************************************************
!
!! NUMCHR returns the position of the last nonblank in a character string.
!
      integer i
      integer num
      integer numchr
      character*(*) string
!
      num = len(string)
!
      do i=num, 1, -1
        numchr = i
        if ( string(i:i) .ne. ' ') return
      end do
 
      numchr = 0
      return
      end
      subroutine numstr(jval,bstrng)
!
!***********************************************************************
!
!! NUMSTR converts an integer to a string with no leading spaces.
!
!  The string is null-terminated.
!
      parameter (maxc=15)
!
      character*(*) bstrng
      integer jval
      character number*15
!
      nc = 0
      write(number,'(i15)') jval
!
!  clear the input string with blanks
!
      ncin=len(bstrng)

      do i=1,ncin
        bstrng(i:i) = ' '
      end do
 
      do i=1,maxc
 
        if(number(i:i) .ne. ' ') then
          nc=nc+1
          bstrng(nc:nc) = number(i:i)
        end if
 
      end do
 
      return
      end
      function nzchar(lmess,cflag)
!
!***********************************************************************
!
!! NZCHAR returns the number of characters in a self-terminating string.
! 
!  NZCHAR takes account of mix alphabet.
!
!  input:   lmess  = string
!  cflag  = mix font flags
!
      save /cstrng/
!
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
!
      integer lmess1(40),lmess(*)
      character*1 ch(160),cflag(6)
      logical finis,first
!
!  mix alphabet not active, write string
!
      if(cflag(1).eq.' ')then
        call zcostr(lmess,100,lmess1,imess1)
        nzchar=leng(lmess1)+kztmln
!
!  process using flags set by mix
!  alphabet routines
!
      else
        call zin2ch(lmess,ch)
        iptr=1
        icur=1
        first=.true.
        do i=1,100
!
!  look for string terminator
!
          finis=.true.
          do j=1,kztmln
            call sybyt4('X',kzstrm,j,khar)
            finis=finis.and.(ichar(ch(i+j-1)).eq.khar)
          end do
!
!  if found, return
!
          if(iptr.eq.1.and.finis)then
            nzchar=i+kztmln-1
            return
          end if
!
!  look for mixalf flags
!
 300      continue
          if(ch(i).eq.cflag(icur).and.ch(i).ne.' '.and.
     &          (icur.ne.iptr.or.first))then
            first=.false.
            iptr=icur
            icur=icur+1
            if(icur.gt.6) icur=1
            go to 200
          else
            icur=icur+1
            if(icur.gt.6) icur=1
            if(icur.ne.iptr) go to 300
!
!  increment nzchar
!
          end if
 200    continue
      end do
!
!  end while
!
        nzchar=-5

      end if

      return
      end
      subroutine origin(x,y)
!
!***********************************************************************
!
!! ORIGIN defines the physical origin.
!  level 1, p/s
!
!  input:   x,y = x and y value from lower left corner
!  of page in inches
!
      parameter (kzyes=111)
!
      save /clevel/
      save /cphysr/
      save /cunit/
!
      common /clevel/ kzlevl,kzbegn
      common /cunit/  zzunit
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
!
      uuxor=x*zzunit
      uuyor=y*zzunit
      kzor=kzyes
      if(kzlevl.ne.1)then
        call errmes('ORIGIN',1,0)
      end if
      return
      end
      subroutine page(x,y)
!
!***********************************************************************
!
!! PAGE defines the page size.
!
!  level 1, p/s.
!
!  x,
!  y     input, real x, y, the page size in inches.
!
      integer kzyes
      parameter (kzyes=111)
!
      save /clevel/
      save /cpage/
      save /cunit/
!
      common /clevel/ kzlevl,kzbegn
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cunit/  zzunit
!
      uupagx=x*zzunit
      uupagy=y*zzunit
      kzpage=kzyes
 
      if(kzlevl.ne.1)call errmes('PAGE',kzlevl,4)
 
      return
      end
      subroutine paklin(istr,ip,iseq)
!
!***********************************************************************
!
!! PAKLIN inserts a line into a packed array.
!  (level 1-3)
!
!  input:   istr = character string to be packed to array
!  ip   = character array
!  iseq = sequence of istr in ip
!
!  output:  ip = modified character array
!
!  ---------------------------------------------------------
!  | p | p |   text ...       |      | hite | yrat |  ave  |
!  ---------------------------------------------------------
!
!  -------------------------------------------------
!  |   text ...       |      | hite | yrat | symsp |
!  -------------------------------------------------
!
!  ...
!  ...
!  ...
!
!  -------------------------------------------------
!  |   text ...       |      | hite | yrat |       |
!  -------------------------------------------------
!  |<--------            nword            -------->|
!
      parameter (kzyes=111)
!
      save /cdevic/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cmxalf/
      save /cstrng/
!
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /clevel/ kzlevl,kzbegn
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      character czalfl*1,czalfn*5,czalfs*5
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5
!
      dimension istr(*),ip(*)
      character cflag(6)*1
      character*5 cfont(6),cstyle
      equivalence (iyrat,yrat),(hite,ihite)
!
      save jtext,jhite,jyrat
      data jtext,jhite,jyrat/1,-2,-1/
      hite=zzhite
      yrat=zzsrat
      if(kzlevl.ge.1.and.kzlevl.le.3)then
!
!  array not big enough
!
        if(ip(2).eq.kzyes)then
          call errmes('PAKLIN',0,4)
          return
        end if
!
!  calculate line length
!
        if(ip(1).eq.kzyes)then
          iplen=ip(2)/1000
          llen=ip(2)-iplen*1000
        else
          llen=40
        end if
!
!  copy to array the current spacing parameter character height, 
!  character font and the character string
!
        nword=(llen-1)/kzbyte+5
        icur=(iseq-1)*nword+2
        call zcopys(istr,100,ip(icur+jtext),llen)
        if(llen.lt.0) call errmes('PAKLIN',iseq,5)
        icur1=icur+nword
        ip(icur1+jhite)=ihite
        ip(icur1+jyrat)=iyrat
        call zloalf(cflag,cfont,cstyle,czalfl,czalfn,czalfs)
 
        do i=1,6
          czlgac(iseq,i)=cflag(i)
          czlgaf(iseq,i)=cfont(i)
        end do
 
        czlgas(iseq)=cstyle
      else
        call errmes('PAKLIN',1,3)
      end if
      return
      end
      subroutine plegnd (text, line1, line2, dy, xcornr, ycornr, leginf,
     &                   hite, seg, gap, box)
!
!***********************************************************************
!
!! PLEGND is a portable plot legend utility.
!  (level 3)
!
!  purpose:
!
!  plegnd composes a plot legend and displays it at a specified position.
!  if the number of legend entries is large and the texts are suitably short,
!  plegnd may be called more than once to position different sections of the
!  legend alongside each other.  (such positioning is up to the calling
!  program.)  character height and line spacing are assumed constant for
!  each call to plegnd.  an optional box may be drawn around the legend.
!
!  plegnd was developed for the following reasons:
!
!  (1) rather than underlining the text with the line pattern, the following
!  form is provided:           ---o---    description
!  (2) character-type text is expected instead of an awkward packed-integer
!  array data structure;
!  (3) any implicit connection between curve-drawing and updating the legend
!  is avoided, since this has required work-arounds in the past.
!
!  warning:  if the legend is to be drawn outside the plotting area, a
!  suitable grace margin should be defined before the call to plegnd.
!
!  arguments:
!
!  arg        dim     type  i/o/s  description
!  text    (*) * (*)   c      i    character array containing legend
!  text with trailing '$'s.  elements
!  line1:line2 will be displayed in the
!  current color (as opposed to the color
!  associated with each line/symbol).
!  line1       -       i      i    first line of legend text to display.
!  line2       -       i      i    last line of legend text to display.
!  dy          -       r      i    space between lines in inches.
!  xcornr      -       r      i    horizontal position of lower left hand
!  corner of legend in inches.
!  ycornr      -       r      i    vertical position of lower left hand
!  corner of legend in inches.
!  leginf    (3, *)    i      i    integer array containing codes for
!  (1) line type, (2) symbol, and (3) color.
!  see polylin for the code definitions.
!  hite        -       r      i    text character height in inches.
!  seg         -       r      i    length of legend line segment in inches.
!  gap         -       r      i    length of gap between legend line segment
!  and text in inches.
!  box         -       l      i    draws box around legend if .true.
!
!  method:
!  the symbol/line drawing is handled separately from the text writing.
!  the latter task was modularized when it was found to be a potentially
!  reusable function (subroutine lbtext) while the former is done with the
!  standard curve drawing routine, polylin, after appropriate conversion
!  of units from inches to data units via xinvrs and yinvrs.
!
!  procedures:
!  lbtext    writes left justified block of text
!  xdimtb    finds length of imaginary rectangle around a text block
!  xinvrs    converts location of a point from inches to data units
!  yinvrs       "              "      "          "         "
!  polylin   draws a curve using lines and/or symbols
!
!-----------------------------------------------------------------------
!
      logical box
      character text (*) * (*)
      integer leginf (3, *), line1, line2
      real dy, hite, xcornr, ycornr
      real       half
      parameter (half = 0.5)
      integer    color, i, line, symbol
      real       gap, mar, seg, x (5), xblen, y (5), yblen, ypos, ier
      real       xdimtb, xinvrs, yinvrs
      external   xdimtb, xinvrs, yinvrs
!
!  display the legend text in current color.  units are inches.
!
      call lbtext (text, line1, line2, dy, xcornr + seg + gap, ycornr,
     &             hite)
!
!  draw a symbol and/or line segment for each legend entry.
!  polylin applies if positions in inches are converted to data units.
!  any grace margin needed for a legend outside the plot area is left
!  to the application program, since there is no way of restoring the
!  input setting here if it is temporarily adjusted.
!
!  note that the polar plot case (not to mention general rotated coordinate
!  systems) forces all conversions from inches to stay inside the loop
!  (where some would be constant otherwise).
!
!  start with the upper left corner.
!
      ypos = ycornr + (line2 - line1) * (hite + dy) + half * hite

      do i = line1, line2
 
         x (1)  = xinvrs (xcornr)
         y (1)  = yinvrs (ypos)
         x (2)  = xinvrs (xcornr + seg)
         y (2)  = yinvrs (ypos)
         line   = leginf (1, i)
         symbol = leginf (2, i)
         color  = leginf (3, i)
 
         if(line .ne. 2)then
!
!  draw the line segment.
!
            call polylin (2, x, y, line, -1, color, ier)
 
         end if
 
         if(symbol .gt. -1)then
!
!  draw the symbol in the middle of the segment.
!
            x (1) = xinvrs (xcornr + half * seg)
            y (1) = yinvrs (ypos)
 
            call polylin (1, x (1), y (1), 2, symbol, color, ier)
 
         end if
 
         ypos = ypos - (hite + dy)
 
      end do

      if(box)then
!
!  pick a reasonable box margin.
!
         mar = .6 * gap
!
!  calculate the dimensions of the legend block (inches).
!
         xblen = seg + gap + xdimtb (text, line1, line2)
         yblen = (line2 - line1) * (hite + dy) + hite
!
!  draw the box starting from the lower left corner.
!
         x (1) = xinvrs (xcornr - mar)
         y (1) = yinvrs (ycornr - mar)
         x (2) = xinvrs (xcornr + mar + xblen)
         y (2) = yinvrs (ycornr - mar)
         x (3) = xinvrs (xcornr + mar + xblen)
         y (3) = yinvrs (ycornr + mar + yblen)
         x (4) = xinvrs (xcornr - mar)
         y (4) = yinvrs (ycornr + mar + yblen)
         x (5) = x (1)
         y (5) = y (1)

         call polylin (5, x, y, 3, -1, 0, ier)

      end if

      return
      end
      subroutine pltbox(xleft,xright,ybot,ytop)
!
!***********************************************************************
!
!! PLTBOX...
!
      real csize
      real goodcs
      real xleft
      real xright
      real ybot
      real ytop
!
      csize = goodcs( min (ytop-ybot,xright-xleft)/80.0)
      call mapprm(xleft,xright,ybot,ytop,csize,0.9*csize,.false.)

      return
      end
      subroutine pltbx2(xleft,xright,ybot,ytop)
!
!***********************************************************************
!
!! PLTBX2...
!
      real csize
      real goodcs
      real xleft
      real xright
      real ybot
      real ytop
!
      csize = goodcs( min (ytop-ybot,xright-xleft)/80.0)

      call mapprm(xleft,xright,ybot,ytop,csize,0.9*csize,.true.)

      return
      end
      subroutine pointc(x,y,npts)
!
!***********************************************************************
!
!! POINTC plots the visible points on the screen.
!
      integer npts
!
      real area(4)
      integer i
      real x(npts)
      real y(npts)
!
      save /pltsiz/
!
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      call gssclp(xvstrt,xvstrt+xvlen,yvstrt,yvstrt+yvlen,area)
 
      do i=1,npts
        call scale(x(i),y(i),vx,vy)
        call gsmove(vx,vy)
        call symbol(3,0.3)
      end do
 
      call gsrclp(area)

      return
      end
      subroutine points(x,y,npts)
!
!***********************************************************************
!
!! POINTS is used to plot points on the screen.
!
      dimension x(npts),y(npts)
!
      do i=1, npts
        call scale(x(i),y(i),vx,vy)
        call gsmove(vx,vy)
        call symbol(3,0.3)
      end do
 
      return
      end
      subroutine poly3
!
!***********************************************************************
!
!! POLY3 sets the interpolation flag to polynomial.
!  level 1-3, p/s
!
      parameter (kzpply=5)
!
      save /clevel/
      save /cline/
!
      common /clevel/ kzlevl,kzbegn
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzltyp=kzpply
      else
        call errmes('POLY3',1,3)
      end if
 
      return
      end
      subroutine polylin(n, x, y, line, symbol, color, ier)
!
!***********************************************************************
!
!! POLYLIN connects points with a line of given pattern, symbol, color.
!  (level 3)
!
!  description and usage:
!
!  polylin is a high-level "curve" drawing routine with a generic
!  programmer interface.  it isolates at least some of the details of
!  the specific graphics library in use.  (plot set-up functions still
!  have to be translated elsewhere when switching to another library.)
!
!  note that any effect polylin might have on the plot legend (via
!  its call to curve) is up to the application.  the original
!  approach would initiate this by calling savlin, maxlin, and paklin prior
!  to any call to polylin.  the alternative legend handling via plegnd
!  eliminates any "hidden" connections between polylin/curve and the legend,
!  so that awkward work-arounds for (say) certain curve fitting methods are
!  no longer needed.
!
!  arguments:
!
!  name    dimension  type  i/o/s  description
!  n                   i    i      number of points to be plotted.
!
!  x          n        r    i      array of abscissas.
!
!  y          n        r    i      array of ordinates.
!
!  line       n        i    i      line type code.
!  1 = connected symbols
!  2 = symbols alone
!  3 = solid line
!  4 = dots
!  5 = dashes
!  6 = chaindots
!  7 = chaindashes
!  8 = longdashes (user-defined type)
!  9 = unused
!  10 = unused
!  11 = unused
!  12 = unused
!  13 = thick solid line
!  14 = thick dots
!  15 = thick dashes
!  16 = thick chaindots
!  17 = thick chaindashes
!  18 = thick longdashes
!  19 = unused
!  20 = unused
!  21 = unused
!  22 = unused
!
!  symbol     n        i    i      symbol type code.  (it is not clear
!  that values > 18 wrap around ...)
!  < 0 - no symbol
!  0 - square
!  1 - octagon
!  2 - triangle
!  3 - '+'
!  4 - 'X'
!  5 - diamond
!  6 - upside down triangle
!  7 - square with an 'X' in it
!  8 - 'X' plus a horizontal line
!  9 - diamond with a '+' in it
!  10 - octagon with a '+' in it
!  11 - double hour glass
!  12 - square with a '+' in it
!  13 - octagon with a 'X' in it
!  14 - square with a triangle in it
!  15 - pentagon with a '+' in it
!  16 - pentagon
!  17 - five pointed star
!  18 - square with a diamond in it
!
!  color               i    i      color code:
!  0 = white
!  1 = black
!  2 = magenta
!  3 = red
!  4 = yellow
!  5 = green
!  6 = cyan
!  7 = blue
!
!  ier                 i      o    error flag:
!  0 = no problems
!  1 = n is not positive
!  2 = no line and no symbol asked for
!
!-------------------------------------------------------------------------------
!
      integer n
!
      integer color
      character*7 colors(0:7)
      integer ier 
      integer line
      real ratio(2)
      integer symbol
      integer type
      real x(n) 
      real y(n)
!
      save colors
      save ratio
!
      data colors
     &  /'WHITE',
     &   'BLACK',
     &   'MAGENTA',
     &   'RED',
     &   'YELLOW',
     &   'GREEN',
     &   'CYAN',
     &   'BLUE'/

      data ratio
     &  /0.7, 0.3/
!
      ier = 0
!
!  simple-minded error checking.  these are not necessarily fatal, so
!  just return with error flag set.
!
      if(n .le. 0)then
!
!  no plot data?
!
         ier = 1
      else if (line .eq. 2 .and. symbol .lt. 0)then
!
!  invisible line!?
!
         ier = 2
      end if

      if(ier .ne. 0) return
!
!  preset to "defaults" (in case of bad inputs?).  note that 'WHITE'
!  actually shows up as black in hardcopy.  resetting 'DOT' takes care
!  of all the patterns.
!
      call reset('CLRSET')
      call reset('CRVWID')
      call reset('DOT')
!
!  set line pattern.
!
!  define "longdash" type:
!
      if(line .eq. 8 .or. line .eq. 18)then
         call lindef (0.2, 2, ratio)
      end if
!
!  <put further user definitions here.  four more allowed.>
!
      if(line .le. 12)then
         call dsltyp (line - 2)
      else
         call crvwid (0.02)
         call dsltyp (line - 12)
      end if
!
!  set point marker type.
!
!  note that marker will look for a custom symbol if called with a
!  negative argument, so we have to test first.
!
      if(symbol .ge. 0)then
!
         call marker (symbol)
!
!  set curve's type flag for symbols and/or lines.
!
         if(line .ne. 2)then
!
!  line plus symbols at every 1th point.
!
            type = +1
         else
!
!  symbols alone.
!
            type = -1
         end if
      else
!
!  no symbol - line only
!
         type = 0
      end if
!
!  set line color.
!
      if(color .ge. 1 .and. color .le. 7)then
         call clrset (colors (color))
      end if
!
!  plot the curve, then reset everything.
!
      call curve(x, y, n, type)
!
      call reset ('CLRSET')
      call reset ('CRVWID')
      call reset ('DOT')
!
!  termination.
!
      return
      end
      subroutine postsc(iunit,orient)
!
!****************************************************************************
!
!! POSTSC initializes the postscript device driver.
!
!  POSTC calls ZINIT to initialize all system variables.
!
!
!  Input, integer IUNIT, the fortran unit number on which the postscript 
!  output file should be opened.  IUNIT should have a value between 0 and 99, 
!  and should probably not equal 0, 5, or 6.
!
!  Input, character*1 ORIENT, sets the page orientation:
!  'L' for landscape,
!  'P' for portrait.
!
      integer ierr
      integer iunit
      integer lun
      integer newdrv
      character*1 orient
!
      if ( orient(1:1).eq.'l'.or.orient(1:1).eq.'L')then
        newdrv=14
      else if (orient(1:1).eq.'p'.or.orient(1:1).eq.'P')then
        newdrv=15
      else
        write(*,*)' '
        write(*,*)'POSTC - Fatal error!'
        write(*,*)'  Illegal input option for orientation:'
        write(*,'(1x,a)')orient
        write(*,*)'  Legal options are "l" or "L" or "p" or "P".'
        ierr=1
        return
      end if
 
      if(iunit.lt.0.or.iunit.gt.99)then
        lun=30
      else
        lun=iunit
      end if
 
      call ddevsl(newdrv,lun,ierr)
 
      call zinit
 
      return
      end
      subroutine prmspl
!
!***********************************************************************
!
!! PRMSPL sets the interpolation flag to parametric spline.
!  level 1-3, p/s
!
      integer kzpspl
      parameter (kzpspl=4)
!
      save /clevel/
      save /cline/
!
      common /clevel/ kzlevl,kzbegn
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzltyp=kzpspl
      else
        call errmes('PRMSPL',1,3)
      end if
 
      return
      end
      subroutine psccgm ( dev, filnam )
!
!***********************************************************************
!
!! PSCCGM selects PSC CGM graphics.
!
!  dev    name of device to be used
!
!  filnam for output to file, the file name.
!
      character*(*) dev
      character*10 devnam
      character*(*) filnam
      external inidat
!
      newdev=20
      lun=4
!
      devnam=dev
      call capchr(devnam)

      if(devnam.eq.' ')then
        devnam='CGMB'
      elseif(devnam.eq.'CGM')then
        devnam='CGMB'
      end if

      call device(devnam)

      if(devnam.eq.'CGMB'.or.
     &   devnam.eq.'CGMC'.or.
     &   devnam.eq.'PS')then
        call outfil ( filnam )
      end if

      call setctb(3)
      call grfini()
      call linclr(1)
      call ddevsl(newdev,lun,ierr)
      call zinit
 
      return
      end
      subroutine ptcsym(x,y,npts,isymno)
!
!***********************************************************************
!
!! PTCSYM plots visible points on screen using symbol ISYMNO.
!
      dimension x(npts),y(npts)
!
      save /pltsiz/
!
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      dimension area(4)
!
      call gssclp(xvstrt,xvstrt+xvlen,yvstrt,yvstrt+yvlen,area)
 
      do i=1,npts
         call scale (x(i),y(i),vx,vy)
         call gsmove(vx,vy)
         call symbol(isymno,0.2)
      end do
 
      call gsrclp(area)
 
      return
      end
      subroutine purjoy(z,izdim1,iz,kx,ky,camloc,xylim,
     &   xlab,ylab,zlab,csize,marplt)
!
!***********************************************************************
!
!! PURJOY plots a function z=f(x,y) as a lined surface.
!
!  the function must be defined on a regular grid.   this routine
!  will optionally remove hidden lines.
!
!  arguments:
!
!  input
!
!  z  * type: real array.
!  * the function values: z(i,j)=f(xi,yj), where
!  xi = xmin + (i-1)*(xmax-xmin)/(kx-1)
!  yj = ymin + (j-1)*(ymax-ymin)/(ky-1)
!
!  izdim1  * type: integer constant or variable.
!  * the first dimension of the z array - not
!  necessarily the number of x values.
!
!  iz  * type: byte array.
!  * a working array of bytes dimensioned atleast
!  kx*ky long.
!
!  kx  * type: integer constant or variable.
!  * the number of x values in the z array.
!  kx <= izdim1 ofcourse.
!
!  ky  * type: integer constant or variable.
!  * the number of y values in the z array.
!
!  camloc  * type: real array.
!  * the relative location of the viewer in space.
!  the viewer always faces toward the center
!  of the surface.
!  camloc(1) = distance from surface in units
!  the same as those of z.
!  camloc(2) = angle between the viewer and the
!  x axis in degrees.   usually, multiples of
!  30 or 45 degrees are best.
!  camloc(3) = angle between the viewer and the
!  xy plane located at z=(zmin+zmax)/2 in
!  degrees.   thus 90 degrees is directly above
!  the surface - an unexciting picture!  usually
!  the angle is selected near 45 degrees.
!
!  xylim  * type: real two dimensional array dimensioned (2,6).
!  * general parameters:
!  xylim(1,1) = xmin ==> the minimum value of x.
!  xylim(2,1) = xmax ==> the maximum value of x.
!  xylim(1,2) = ymin ==> the minimum value of y.
!  xylim(2,2) = ymax ==> the maximum value of y.
!  note: z(i,j) = f(xi,yj) where:
!  xi = xmin + (i-1)*(xmax-xmin)/(kx-1)
!  yj = ymin + (j-1)*(ymax-ymin)/(ky-1)
!  xylim(1,3) = zmin ==> the minimum value of z.
!  xylim(2,3) = zmax ==> the maximum value of z.
!  these z values define the range of z values
!  to fit on the screen.   it is strongly
!  advised that zmin and zmax bound z(i,j).
!  xylim(1,4) = x/z axis length ratio.   if this
!  parameter is 0, then x and z are assumed to
!  have the same units, so their relative
!  lengths will be in proportion to their
!  ranges.   if this parameter is nonzero, then
!  the x axis will be xylim(1,4) times as long
!  as the z axis.
!  xylim(2,4) = y/z axis length ratio.   same as
!  xylim(1,4), but for y axis.
!  xylim(1,5) = plot width in virtual coordinates
!  xylim(2,5) = plot height in virtual coord.
!  note: the plot is expanded/contracted until
!  it all fits within the box defined by
!  xylim(1,5) and xylim(2,5).
!  xylim(1,6) = virtual x coord. of the lower
!  left corner of the plot box.
!  xylim(2,6) = virtual y coord. of the lower
!  left corner of the box.
!
!  xlab  * type: string constant or variable.
!  * the x axis label.
!
!  ylab  * type: string constant or variable.
!  * the y axis label.
!
!  zlab  * type: string constant or variable.
!  * the z axis label.
!
!  csize  * type: real constant or variable.
!  * the character size in virtual coord. for the tick
!  mark labels and the axis labels.
!
!  marplt  * type: integer constant or variable.
!  * hidden line flag:
!  0 ==> draw all lines, hidden or not.
!  1 ==> suppress all lines hidden by the surface, but
!  display both the top and bottom of the surface
!  3 ==> suppress all lines hidden by the surface, and
!  all lines showing the bottom of the surface.
!  add 4 to marplt if you do not want the axes nor the
!  ticks labeled.   this is useful on small plots.
!
      save /comdp/
      save /comdp1/
      save /dbase/
      save /gcdchr/
!
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &   ndclrs, idvbts, nfline, xclipd, yclipd
      dimension z(izdim1,ky), camloc(3), xylim(2,6)
      character xlab*2, ylab*2, zlab*2
      dimension iz(kx,ky)
!
      common/comdp/xmin,xmax,ymin,ymax,zmin,zmax,axisr(2),plotx,
     &   ploty,pltorg(2),camxyz(3),mx,ny,fmx,fny,camwkg(6),xorg(3),
     &   gx(3),fx(2),kscale,zorg,center(2),pqlmt,
     &   amtx(3,3),focall
      dimension limit(2),flim(2)
      equivalence(u,camxyz(1)),(v,camxyz(2)),(w,camxyz(3)),
     &   (mx,limit(1)),(fmx,flim(1))
!
      logical lsolid
      common /comdp1/ lsolid
      dimension xmina(2,6)
      logical llable
      equivalence(xmin,xmina(1,1))
      common /dbase/vx,vy,voldx,voldy,cxsize,cysize
!
!  pick up xy limits, box sizes, etc.
!
      do j=1,6
        xmina(1,j) = xylim(1,j)
        xmina(2,j) = xylim(2,j)
      end do
!
!  now set up limits if axis ratios are requested
!
      if(axisr(1) .eq. 0.0) go to 260
      do i=1,2
        xmina(1,i)=axisr(i)*zmin
      end do
 
260   if(axisr(2) .eq. 0.0) go to 266
 
      do i=1,2
        xmina(2,i)=axisr(i)*zmax
      end do
!
!  set tolerance for visible tests = half plotter step size
!
266   pqlmt = min (0.5/xres,0.5/yres)
!
!  convert r, phi, theta to dx, dy, dz
!
      rad = 3.14159/180.0
      phi = camloc(2)*rad
      theta = camloc(3)*rad
      camwkg(1)=camloc(1)*cos(phi)*cos(theta)
      camwkg(2)=camloc(1)*sin(phi)*cos(theta)
      camwkg(3)=camloc(1)*sin(theta)
!
!  pick up camera data
!
      do j=1,3
        camwkg(j+3)=(xmina(1,j)+xmina(2,j))/2.0
        camwkg(j)=camwkg(j+3)+camwkg(j)
      end do
 
      call camrot
      mx=kx
      fmx=float(kx)
      ny=ky
      fny=float(ny)
!
!  option for scaling z
!  scale factors to convert user values to indices
!
      gx(1) = (xmax-xmin)/(fmx-1.0)
      gx(2) = (ymax-ymin)/(fny-1.0)
!
!  find z scale factor
!
      gx(3)=1.0
      zorg=0.0
!
!  find scale factors for plot
!
      cysize = csize
      cxsize = 9.0*cysize/8.0
      xa=1.0e30
      xb=-1.0e30
      ya=1.0e30
      yb=-1.0e30
      if(camwkg(3) .lt. camwkg(6)) go to 16
      dx=float(mx-1)/20.0
      dy=float(ny-1)/20.0
      if=mx
!  xz = xmax
      ib=1
      jf=ny
!  yz = ymin
      jb=1
      if(camwkg(1) .ge. camwkg(4)) go to 120
      if=1
!  xz = xmin
      ib=mx
      dx=-dx
120   if(camwkg(2) .ge. camwkg(5)) go to 130
      jf=1
!  yz = ymax
      jb=ny
      dy=-dy
130   frx=if
      bkx=ib
      fry=jf
      bky=jb
      vx = xmin + (frx-1.0)*gx(1) - camwkg(1)
      vy = ymin + (bky-1.0-dy)*gx(2) - camwkg(2)
      call extrma(vx,vy,zmax-camwkg(3),xa,xb,ya,yb,ierr)
      if(ierr .ne. 0) go to 50
      temp = zmin - camwkg(3)
      call extrma(vx,vy,temp,xa,xb,ya,yb,ierr)
      if(ierr .ne. 0) go to 50
      vy = ymin + (fry-1.0+dy)*gx(2) - camwkg(2)
      call extrma(vx,vy,temp,xa,xb,ya,yb,ierr)
      if(ierr .ne. 0) go to 50
      call extrma(xmin+(bkx-1.0)*gx(1)-camwkg(1),vy,temp,
     &   xa,xb,ya,yb,ierr)
      if(ierr .ne. 0) go to 50
      vx = vx + dx*gx(1)
      call extrma(vx,ymin+(bky-1.0)*gx(2)-camwkg(2),temp,
     &   xa,xb,ya,yb,ierr)
      if(ierr .ne. 0) go to 50
      call extrma(vx,vy-dy*gx(2),temp,xa,xb,ya,yb,ierr)
      if(ierr .ne. 0) go to 50
16    continue
 
      do j=1,ny
        vy = ymin + (j-1)*gx(2) - camwkg(2)
        do i=1,mx
          vx = xmin + (i-1)*gx(1) - camwkg(1)
          call extrma(vx,vy,z(i,j)-camwkg(3),xa,xb,ya,yb,ierr)
          if(ierr .ne. 0) go to 50
        end do
      end do
!
!  scale x and y ranges to fit on plot
!
      temp = 12.5*cxsize
      llable = .true.
!
!
      mtemp = marplt
!
!
!  if((mtemp .and. 4) .ne. 0) llable = .false.
      if(mtemp .ge. 4)then
      mtemp = mtemp - 4
      llable = .false.
      else
      end if
      if(.not. llable) temp = 0.0
      fx(1) = (plotx-temp)/(xb-xa)
      temp = 2.0*cysize
      if(.not. llable) temp = 0.0
      fx(2) = (ploty-temp)/(yb-ya)
!
!  choose minimum focal length of the two
!
      focall = min (fx(1),fx(2))
!
!  set x,y origins (before scaling to focal length)
!
      xorg(1) = xa
      xorg(2) = ya
!
!  sizes in x,y (not including out-of-box poiints that get in pic)
!
      xb = (xb-xa)*focall
      yb = (yb-ya)*focall
!
!  center for now, but later make optional
!
      center(1) = (plotx-xb)/2.0
      center(2) = (ploty-yb)/2.0
!
!  camera location expressed as xy indices
!
      u = 1.0+(fmx-1.0)*(camwkg(1)-xmin)/(xmax-xmin)
      v = 1.0+(fny-1.0)*(camwkg(2)-ymin)/(ymax-ymin)
!
!  for visibility checking, scale camera z coordinate opposite to the
!  way z will be scaled for plotting - rather than scaling all the
!  z-s on the surface when checking.
!
      w = (camwkg(3)-zorg)/gx(3)
!
!  calculate visibilities
!
!  if lsb of marplt is set, supress all hidden lines
!
!  if((mtemp .and. 1) .ne. 0) go to 7
!
      if(mtemp .eq. 1) go to 7
      if(mtemp .eq. 3) go to 7
 
      do k = 1,ny
        do j = 1,mx
          iz(j,k)=0
        end do
      end do
 
      go to 40
7     lsolid = .false.
!
!  if((mtemp .and. 2) .ne. 0) lsolid = .true.
!
      if(mtemp .eq. 2) lsolid = .true.
      if(mtemp .eq. 3) lsolid = .true.
      dok = 1,ny
        eta = float(k)
        do j =1,mx
          l = ivis(float(j),eta,z(j,k),z,izdim1)+1
          iz(j,k)=l
        end do
      end do
!
!  now plot
!
 40   call qdrw3d(z,izdim1,iz,kx)
      if(camwkg(3) .lt. camwkg(6)) go to 45
      call gssetc(cysize,0.0)
      call gscolr(1,jer)
      call xyprm(frx,bky,zmax,0)
      voldx=vx
      voldy=vy
      vxt=vx
      vyt=vy
      call xyprm(frx,bky-dy,zmax,1)
      if(llable) call tickl(zmax,-0.5)
      call gsmove(vxt,vyt)
      call xyprm(frx,bky,zmin,1)
      voldx=vx
      voldy=vy
      call xyprm(frx,bky-dy,zmin,1)
      if(.not. llable) go to 140
      call tickl(zmin,0.25)
      temp = max (voldx,vxt)+1.5*cysize
      if(vx .lt. voldx) temp = min (voldx,vxt)-0.5*cysize
      call gsmove(temp,(voldy+vyt-cxsize*numchr(zlab))/2.0)
      call gssetc(cysize,90.0)
      call gspstr(zlab)
      call gssetc(cysize,0.0)
140   call gsmove(voldx,voldy)
      call xyprm(frx+dx,bky,zmin,1)
      if(llable) call tickl(xylim(1+jb/ny,2),-0.5)
      call gsmove(voldx,voldy)
      call xyprm(frx,fry+dy,zmin,1)
      if(.not. llable) go to 150
      call tickl(xylim(1+if/mx,1),-0.5)
      temp = cxsize*(numchr(ylab)+0.25)
      if(vx .lt. voldx) temp = -0.25*cxsize
      call gsmove((vx+voldx)/2.0-temp,(vy+voldy)/2.0-cysize)
      call gspstr(ylab)
150   call xyprm(frx,fry,z(if,jf),-1)
      call gsmove(vx,vy)
      call xyprm(frx,fry,zmin,1)
      voldx=vx
      voldy=vy
      call xyprm(frx+dx,fry,zmin,1)
      if(llable) call tickl(xylim(1+jf/ny,2),-0.5)
      call gsmove(voldx,voldy)
      call xyprm(bkx,fry,zmin,1)
      if(.not. llable) go to 160
      temp = cxsize*(numchr(xlab)+0.25)
      if(vx .gt. voldx) temp = -0.25*cxsize
      call gsmove((vx+voldx)/2.0-temp,(vy+voldy)/2.0-cysize)
      call gspstr(xlab)
160   voldx=vx
      voldy=vy
      call gsmove(vx,vy)
      call xyprm(bkx,fry+dy,zmin,1)
      if(llable) call tickl(xylim(1+ib/mx,1),-0.5)
      call gsmove(voldx,voldy)
      call xyprm(bkx,fry,z(ib,jf),1)
45      return
!
!  point on the surface is behind the camera. quit.
!
  50  write(*,603)
      return
!
!  z is a flat plane, do not draw (for now)
!
  60  continue
      write(*,*)' '
      write(*,*)'PURJOY - Warning!'
      write(*,*)'  All function values are equal, no contours!'
      return
!
! 503 format(' z multiplier',e15.6,', z origin shift',e15.6)
!504     format('0x limits',2f10.3/' y limits',2f10.3/' z limits',2f10.3/
!  1' z cutoff',2e15.6/
!  2                 ' plot size',2f10.3/' plot origin',2f10.3)
! 602 format('0focal lengths to fill x,y plotter space',2e15.6,
!  1 ', lesser value chosen'/'0picture size in x,y =',2f9.3,
!  2 ', requested sizes',2f9.3/' centers = ',2g14.7)
  603 format('0part of surface is behind the camera, unable to plot. sor
     &ry.')
      end
      subroutine qdrw3d(z,izdim1,iz,kx)

!***********************************************************************
!
!! QDRW3D draws a plot.
!
      dimension z(izdim1,2)
      dimension iz(kx,2)
!
!  common storage descriptor
!
      save /comdp/
      save /comdpa/

      common/comdp/xmin,xmax,ymin,ymax,zmin,zmax,axisr(2),plotx,
     &   ploty,pltorg(2),camxyz(3),mx,ny,fmx,fny,camwkg(6),xorg(3),
     &   gx(3),fx(2),kscale,zorg,center(2),pqlmt,
     &   amtx(3,3),focall
      dimension limit(2),flim(2)
      equivalence(u,camxyz(1)),(v,camxyz(2)),(w,camxyz(3)),
     &   (mx,limit(1)),(fmx,flim(1))
!  end cde
!
!
!  cde package for draw3d,drawpq
      common/comdpa/pc(3),qc(3),p(3),q(3),enda(6),endb(6),oldq(3),
     &   pw(3),qw(3),t(6),pk(3),qk(3),phip,phiq,phia,ibeam,icolor
      integer phip,phiq,phia
!  end of cde package
!  end of cde package
!
!  save z dimension in common to pass along through drawpq to ivis
!  scan along x first at constant y
!
!  index of coordinate being stepped along a line
      kscan = 1
!  index of coordinate being held fixed
      kfix = 2
!  set fixed coordinate   increment
      pc(kfix) = 1.0
      delfix = 1.0
!  set roving coordinate   increment initially
      delscn = 1.0
      qc(kscan) = 1.0
!  begin scanning a line
101   qc(kfix) = pc(kfix)
      ibeam = 0
!  next point in line scan
102   pc(kscan) = qc(kscan)
      qc(kscan) = pc(kscan) + delscn
!  working indices
      jpc = ifix(pc(1))
      kpc = ifix(pc(2))
      jqc = ifix(qc(1))
      kqc = ifix(qc(2))
!  phi functions
      pc(3)=z(jpc,kpc)
      qc(3)=z(jqc,kqc)
      phip=iz(jpc,kpc)-1
      phiq=iz(jqc,kqc)-1
200   call drawpq(z,izdim1)
!  test if line is done
      if((qc(kscan)-1.0)*(qc(kscan)-flim(kscan)) .lt. 0.0) go to 102
!  line done. advance fixed coordinate.
      pc(kfix) = pc(kfix) + delfix
!  test if fixed coordinate now off limits
      if((pc(kfix)-1.0)*(pc(kfix)-flim(kfix)) .gt. 0.0) go to 55
!  flip increment. scan begins at qc of previous line.
      delscn = -delscn
      go to 101
!
!  test if we have done y scan yet.
!
55    if(kscan .eq. 2) return
!  no, scan y direction at fixed x.
      kscan = 2
      kfix = 1
!  start fixed x at x of last traverse
      pc(1) = qc(1)
!  then step x in opposite direction
      delfix = -delscn
!  we ended up at max. y, so first y scan goes backwards
      delscn = -1.0
!  initial y for first line
      qc(2) = fny
      go to 101
      end
      subroutine qms(iunit)
!
!***********************************************************************
!
!! QMS initializes the QMS laser printer.
!
!  input:   iunit = output logical unit, if not legal
!  zero then use default, 31
!
      integer iunit
      integer newdev
!
      newdev=3
 
      if(iunit.le.0.or.iunit.gt.99)then
        lun=31
      else
        lun=iunit
      end if
!
!  select the device.
!
      call ddevsl(newdev,lun,ierr)
!
!  initialize all data.
!
      call zinit
 
      return
      end
      subroutine realno(anum,iplace,xpos,ypos)
!
!***********************************************************************
!
!! REALNO writes a real number on the screen.
!  (level 2-3)
!
!  input:   anum      = real number
!  iplace    = format flag
!  if > 100, free point, iplace-100 digits
!  total
!  if > 0, floating point, with iplace
!  decimal places after decimal point
!  if < 0, exponent form, iplace decimal
!  places after decimal point
!
!  xpos,ypos = distance from physical origin in inches
!
      parameter (numbyt=4)
!
      integer lmessa(80/numbyt)
      character czalfl*1
      character czalfn*5
      character czalfs*5
      character khar*(numbyt)
!
      save /clevel/
      save /cmxalf/
      save /cmess/
      save /cpage/
      save /cphysr/
      save /cstrng/
      save /cunit/
      save /gcclip/
      save /gccpar/
      save /gcvpos/
!
      common /clevel/ kzlevl,kzbegn
      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /gcvpos/ xvpos, yvpos
      common /gccpar/ csize, ccos, csin
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
!
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
!
!  calculate virtual coordinates
!
        temp=xpos
        call win2ch(temp,khar)

        if(khar(1:4).eq.'ABUT')then
          vx=zzabux
        else
          vx=xcm0+zzxor+xpos*zzunit*zzpagr
        end if

        temp=ypos
        call win2ch(temp,khar)

        if(khar(1:4).eq.'ABUT')then
          vy=zzabuy
        else
          vy=ycm0+zzyor+ypos*zzunit*zzpagr
        end if
!
!  draw character string
!
        call dsmove(vx,vy)
        call zreal(anum,iplace,lmessa,lpower)
        ln=leng(lmessa)
        call ztext(lmessa,ln,czalfl,czalfn,czalfs)
        zzabux=xvpos
        zzabuy=yvpos
!
!  draw exponent if desired
!
        if(lpower.ne.0)then
          tx=0.7*zzhite*csin
          ty=0.7*zzhite*ccos
          call dsmove(zzabux+tx,zzabuy+ty)
          call gssetc(0.6*zzhite,zzangl)
          ln=leng(lpower)
          call ztext(lpower,ln,czalfl,czalfn,czalfs)
          zzabux=xvpos-tx
          zzabuy=yvpos-ty
          call gssetc(zzhite,zzangl)
        end if
!
!  wrong level
!
      else
        call errmes('REALNO',3,0)
      end if
      return
      end
      subroutine relint(ival,xval,yval)
!
!***********************************************************************
!
!! RELINT converts an integer to a string with no leading spaces.
!
!  input:   ival      = input integer
!  xval,yval = x and y values in current coordinate
!  system
!
      parameter (numbyt=4)
!
      character czalfl*1,czalfn*5,czalfs*5
      integer imessa(16/numbyt)
      character cmess*16,kharx*(numbyt),khary*(numbyt)
!
      save /clevel/
      save /cmxalf/
      save /cmess/
      save /gcvpos/
!
      common /clevel/ kzlevl,kzbegn
      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /gcvpos/ xvpos, yvpos
!
 10   format(i11)
      if(kzlevl.eq.3)then
        tempx=xval
!
!  calculate virtual coordinate
!
        call win2ch(tempx,kharx)

        if(kharx(1:4).eq.'ABUT')then
          xt=1.0
        else
          xt=xval
        end if

        tempy=yval
        call win2ch(tempy,khary)

        if(khary(1:4).eq.'ABUT')then
          yt=1.0
        else
          yt=yval
        end if

        call scale(xt,yt,vx,vy)
        if(kharx(1:4).eq.'ABUT') vx=zzabux
        if(khary(1:4).eq.'ABUT') vy=zzabuy
!
!  draw integer
!
        call dsmove(vx,vy)
        write(cmess,'(i11)')ival
        cmess(12:12)=char(0)
 
        do i=1,16/numbyt
          call wch2in(cmess((i-1)*numbyt+1:i*numbyt),imessa(i))
        end do
 
        call zstrbl(imessa)
        ln=leng(imessa)
        call ztext(imessa,ln,czalfl,czalfn,czalfs)
        zzabux=xvpos
        zzabuy=yvpos
      else
        call errmes('RELINT',3,0)
      end if

      return
      end
      subroutine relmsg(lmess,imess,xval,yval)
!
!***********************************************************************
!
!! RELMSG writes a Hollerith string on the screen.
!  (level 3)
!
!  input:   lmess     = character string
!  imess     = number of characters in lmess
!  xval,yval = x and y values in current coordinate
!  system
!
      parameter (numbyt=4)
!
      integer lmess(*)
      character czalfl*1,czalfn*5,czalfs*5,kharx*(numbyt)
      character khary*(numbyt)
!
      save /clevel/
      save /cmess/
      save /cmxalf/
      save /gcvpos/
!
      common /clevel/ kzlevl,kzbegn
      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /gcvpos/ xvpos, yvpos
!
      if(kzlevl.eq.3)then
!
!  calculate virtual coordinate
!
        tempx=xval
        call win2ch(tempx,kharx)
        if(kharx(1:4).eq.'ABUT')then
          xt=1.0
        else
          xt=xval
        end if
        tempy=yval
        call win2ch(tempy,khary)
        if(khary(1:4).eq.'ABUT')then
          yt=1.0
        else
          yt=yval
        end if
        call scale(xt,yt,vx,vy)
        if(kharx(1:4).eq.'ABUT') vx=zzabux
        if(khary(1:4).eq.'ABUT') vy=zzabuy
!
!  draw messag
!
        call dsmove(vx,vy)
        call ztext(lmess,imess,czalfl,czalfn,czalfs)
        zzabux=xvpos
        zzabuy=yvpos
      else
        call errmes('RELMSG',3,0)
      end if
      return
      end
      subroutine relrno(anum,iplace,xval,yval)
!
!***********************************************************************
!
!! RELRNO converts a real number to a string.
!
!  input:   anum      = input real number
!  iplace    = format flag
!  if > 100,free point, iplace-100 digits
!  total
!  if > 0, floating point, with iplace
!  decimal places after decimal point
!  if < 0, exponent form, iplace decimal
!  places after decimal point
!  xval,yval = x and y value in current coordinate
!  system
!
      parameter (numbyt=4)
!
      integer lmessa(80/numbyt)
      character czalfl*1,czalfn*5,czalfs*5,kharx*(numbyt)
      character khary*(numbyt)
!
      save /clevel/
      save /cmess/
      save /cmxalf/
      save /cstrng/
      save /gccpar/
      save /gcvpos/
!
      common /clevel/ kzlevl,kzbegn
      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /gcvpos/ xvpos, yvpos
      common /gccpar/ csize, ccos, csin
!
      if(kzlevl.eq.3)then
!
!  calculate virtual coordinates
!
        tempx=xval
        call win2ch(tempx,kharx)
        if(kharx(1:4).eq.'ABUT')then
          xt=1.0
        else
          xt=xval
        end if
        tempy=yval
        call win2ch(tempy,khary)
        if(khary(1:4).eq.'ABUT')then
          yt=1.0
        else
          yt=yval
        end if
        call scale(xt,yt,vx,vy)
        if(kharx(1:4).eq.'ABUT') vx=zzabux
        if(khary(1:4).eq.'ABUT') vy=zzabuy
!
!  draw character string
!
        call dsmove(vx,vy)
        call zreal(anum,iplace,lmessa,lpower)
        ln=leng(lmessa)
        call ztext(lmessa,ln,czalfl,czalfn,czalfs)
        zzabux=xvpos
        zzabuy=yvpos
!
!  if exponential desired, draw it
!
        if(lpower.ne.0)then
          tx=0.7*zzhite*csin
          ty=0.7*zzhite*ccos
          call dsmove(zzabux+tx,zzabuy+ty)
          call gssetc(0.6*zzhite,zzangl)
          ln=leng(lpower)
          call ztext(lpower,ln,czalfl,czalfn,czalfs)
          zzabux=xvpos-tx
          zzabuy=yvpos-ty
          call gssetc(zzhite,zzangl)
        end if
!
!  wrong level
!
      else
        call errmes('RELRNO',3,0)
      end if
      return
      end
      subroutine relvec(xfrom,yfrom,xto,yto,ivec)

!*********************************************************************72
!
!! RELVEC draws a vector with or without an arrow head.
!  level 3
!
!  input:   xfrom,yfrom = first point of vector
!  xto,yto     = second point of vector
!  ivec(wxyz)  = arrow type and size flag
!
      common /clevel/ kzlevl,kzbegn
!
      if(kzlevl.eq.3)then
        call scale(xfrom,yfrom,vxfrom,vyfrom)
        call scale(xto,yto,vxto,vyto)
        call zvectr(vxfrom,vyfrom,vxto,vyto,ivec)
      else
        call errmes('RELVEC',3,0)
      end if
      return
      end
      subroutine reset(param)

!*********************************************************************72
!
!! RESET resets to default.
!
!  no effect on 'setout' and hardware calls
!
!  input:   param = parameter indicator
!
      parameter (kzyes=111)
      parameter (kzno=222)
      parameter (kzdown=13)
      parameter (kzlin=2)
      parameter (kzsoli=1)
      parameter (kzreal=2)
      parameter (kzmxbs=1000)
      parameter (kzmxbl=200)
      parameter (kzmaxc=255)
      parameter (zzin=2.54)
!
      save /carea/
      save /caxis/
      save /cblank/
      save /cborch/
      save /cdevic/
      save /clabel/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cline/
      save /cmxalf/
      save /colorc/
      save /colorn/
      save /cpage/
      save /csetunt/
      save /cstrng/
      save /dcltyp/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
      common /cblank/ zzblnk(kzmxbl,4),zzblks(kzmxbs,4),kzblcn,kzbscn
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /colorn/ kzshd,zzhspc,zzcolr(kzmaxc,3),kznclr,kzccol
      common /colorc/ czcolr
      common /cborch/ kzbrdr,kzchek
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /csetunt/  zzsetunt
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
!
      logical linilt, lposnd
      character czcolr*8
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5
      character czalfl*1,czalfn*5,czalfs*5
!
      character*(*) param
      character sub4*4
      character*6 sub
!
      ncmax = len(param)
      if(ncmax .gt. 6) ncmax = 6
      sub=' '
      sub(1:ncmax) = param
      call capchr(sub)
!
!  for all reset
!
      if(sub(1:3).eq.'ALL')then
        zzangl=0.0
        zzhite=0.14*zzin*zzpagr
        call gssetc(zzhite,zzangl)
        kzlsty=kzsoli
        call dsltyp(kzlsty)
 
        do i=1,kzblcn
          do j=1,4
            zzblnk(i,j)=-1100.0
          end do
        end do
 
        kzblcn=4
        if(kzbscn.gt.0)then
          do i=1,kzbscn
            do j=1,4
              zzblks(i,j)=-1100.0
            end do
          end do
          kzbscn=-1
        end if
        uugrce=0.5*zzsetunt
        zzgrce=uugrce*zzpagr
!
!  kzshd=kzno
!
        zzsrat=1.5
        czalfl(1)=' '
        czalfn(1)='STAND'
        czalfs='DEFAU'
        call zscopy('TXTBLK',kzlgti)
        kzlgln=kzno
        zzlgtz=-1.0
!
!  kzlgtf=1
!
        zzlgtr=-1.0
        kzchek=kzyes
        kzbrdr=kzyes
        call sizmrk(1.0)
        czcolr='WHITE'
        call gscolr(1,ierr)
        call wch2in ( '$', kzstrm )
        kztmln=1
        kzltyp=kzlin
        kzlthk=kzno
        zzsetunt=zzin
        zzxlba=0.0
        kzxtyp=kzreal
        kzytyp=kzreal
        zzylba=90.0
        kzxnon=kzno
        kzynon=kzno
        kzxtck=1
        kzytck=1
        uufrme=0.01*zzin
        zzfrme=uufrme*zzpagr
        call gsfont(1,ierr)

      elseif(sub(1:5).eq.'ANGLE')then
        zzangl=0.0
        call gssetc(zzhite,zzangl)
      elseif(sub.eq.'BLANK1')then
        do i=1,4
          zzblnk(1,i)=-1100.0
        end do
      elseif(sub.eq.'BLANK2')then
        do i=1,4
          zzblnk(2,i)=-1100.0
        end do
      elseif(sub.eq.'BLANK3')then
        do i=1,4
          zzblnk(3,i)=-1100.0
        end do
      elseif(sub.eq.'BLANK4')then
        do i=1,4
          zzblnk(4,i)=-1100.0
        end do
      elseif(sub.eq.'BLNKAL')then
        do i=1,kzblcn
          do j=1,4
            zzblnk(i,j)=-1100.0
          end do
        end do
 
        kzblcn=4
 
        if(kzbscn.gt.0)then
 
          do i=1,kzbscn
            do j=1,4
              zzblks(i,j)=-1100.0
            end do
          end do
 
          kzbscn=-1
 
        end if
      elseif(sub.eq.'BLANKS')then
        if(kzbscn.gt.0)then
          do i=1,kzbscn
            do j=1,4
              zzblks(i,j)=-1100.0
            end do
          end do
          kzbscn=-1
        end if
      elseif(sub(1:3).eq.'DOT'.or.sub.eq.'LINDEF'.or.
     &           sub(1:4).eq.'DASH'.or.sub.eq.'CHNDOT'.or.
     &           sub.eq.'CHNDSH')then
        kzlsty=kzsoli
        call dsltyp(kzlsty)
      elseif(sub.eq.'MARGIN')then
        uugrce=0.5*zzsetunt
        zzgrce=uugrce*zzpagr
      elseif(sub.eq.'HEIGHT')then
        uuhite=0.14*zzin
        zzhite=uuhite*zzpagr
        call gssetc(zzhite,zzangl)
      elseif(sub.eq.'HRDSHD')then
        kzshd=kzno
      elseif(sub.eq.'HRDSCL')then
        kzscal=kzdown
      elseif(sub.eq.'SAVLIN')then
        kzlgln=kzno
      elseif(sub.eq.'VSPACE')then
        zzsrat=1.5
      elseif(sub.eq.'DEFALF'.or.sub.eq.'ALPHA1')then
        czalfl(1)=' '
        czalfn(1)='STAND'
      elseif(sub.eq.'MIXALF'.or.sub.eq.'ALPHA2')then
        czalfl(2)=' '
      elseif(sub.eq.'ALPHA3')then
        czalfl(3)=' '
      elseif(sub.eq.'ALPHA4')then
        czalfl(4)=' '
      elseif(sub.eq.'ALPHA5')then
        czalfl(5)=' '
      elseif(sub.eq.'ALPHA6')then
        czalfl(6)=' '
      elseif(sub.eq.'LEGHDG')then
        call zscopy('TXTBLK',kzlgti)
        zzlgtz=-1.0
!  kzlgtf=1
        zzlgtr=-1.0
      elseif(sub.eq.'NOCHEK')then
        kzchek=kzyes
      elseif(sub.eq.'NOBORD')then
        kzbrdr=kzyes
      elseif(sub.eq.'SIZMRK')then
        call sizmrk(1.0)
      elseif(sub.eq.'CLRSET')then
        czcolr='WHITE'
        call gscolr(1,ierr)
      elseif(sub.eq.'SETOUT')then
        kzsum=6
      elseif(sub.eq.'TRMCHR')then
        call wch2in ( '$', kzstrm )
        kztmln=1
      elseif(sub.eq.'CUBSPL'.or.sub.eq.'PRMSPL'.or.
     &           sub(1:5).eq.'POLY3')then
        kzltyp=kzlin
      elseif(sub.eq.'CRVWID')then
        kzlthk=kzno
      elseif(sub.eq.'FRMWID')then
        uufrme=0.01*zzin
        zzfrme=uufrme*zzpagr
      elseif(sub.eq.'TRIPLX'.or.
     &           sub.eq.'CARTOG'.or.sub.eq.'SIMPLX'.or.
     &           sub.eq.'CMPLX2'.or.sub.eq.'COMPLX'.or.
     &           sub.eq.'GOTHIC'.or.sub.eq.'DUPLEX')then
        czalfs='DEFAU'
      else if(sub(1:4).eq.'SETUNT')then
        zzsetunt=zzin
      elseif(sub.eq.'XANGLE')then
        zzxlba=0.0
      elseif(sub.eq.'INTGRX')then
        kzxtyp=kzreal
      elseif(sub.eq.'XLABEL')then
        kzxaln=0
      elseif(sub.eq.'NOXLBL')then
        kzxnon=kzno
      elseif(sub.eq.'XMARKS')then
        kzxtck=1
      elseif(sub.eq.'YANGLE')then
        zzylba=90.0
      elseif(sub.eq.'INTGRY')then
        kzytyp=kzreal
      elseif(sub.eq.'YLABEL')then
        kzyaln=0
      elseif(sub.eq.'NOYLBL')then
        kzynon=kzno
      elseif(sub.eq.'YMARKS')then
        kzytck=1
      else
        sub4=sub(1:4)
        call wch2in(sub4,isub)
        call errmes('RESET',isub,4)
      end if

      return
      end
      subroutine rotate(xin,a,xout)

!*********************************************************************72
!
!! ROTATE rotates vector XIN by matrix A to get XOUT.
!
      real a(9)
      real xin(3)
      real xout(3)
!
      xout(1) = a(1)*xin(1) + a(4)*xin(2) + a(7)*xin(3)
      xout(2) = a(2)*xin(1) + a(5)*xin(2) + a(8)*xin(3)
      xout(3) = a(3)*xin(1) + a(6)*xin(2) + a(9)*xin(3)
      return
      end
      subroutine rstmap(area)
!
!***********************************************************************
!
!! RSTMAP restores the mapping parameters saved by SAVMAP.
!
      real area(14)
      logical logx, logy
!
      save /pltclp/
      save /pltcom/
      save /pltsiz/
!
      common /pltclp/ xmin,xmax,ymin,ymax
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
!  restore the mapit clipping limits
!
      xmin = area(1)
      xmax = area(2)
      ymin = area(3)
      ymax = area(4)
!
!  save world to virtual coord. transformation const.
!
      ux0 = area(5)
      udx = area(6)
      uy0 = area(7)
      udy = area(8)
      logx = .false.
      if(area(9) .ne. 0.0) logx = .true.
      logy = .false.
      if(area(10) .ne. 0.0) logy = .true.
!
!  restore virt. coord. of axes
!
      xvstrt = area(11)
      yvstrt = area(12)
      xvlen = area(13)
      yvlen = area(14)
!
!  all done
!
      return
      end
      subroutine savlin
!
!***********************************************************************
!
!! SAVLIN sets a flag to save the line style every time CURVE is called.
!  (level 1-3, p/s)
!
      parameter (kzyes=111)
!
      save /clevel/
      save /clgndc/
      save /clgndn/
!
      common /clevel/ kzlevl,kzbegn
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5

      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzlgln=kzyes
      else
        call errmes('SAVLIN',1,3)
      end if

      return
      end
      subroutine savmap(area)
!
!***********************************************************************
!
!! SAVMAP saves the status from the last MAPPRM-MAPIT calls.
!
!  when used in conjuction "rstmap", the user can switch around between
!  multiple graphic regions on the screen created with "mapit".
!
      real area(15)
      logical logx, logy
!
      save /pltclp/
      save /pltcom/
      save /pltsiz/
!
      common /pltclp/ xmin,xmax,ymin,ymax
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
!  save the mapit clipping limits
!
      area(1) = xmin
      area(2) = xmax
      area(3) = ymin
      area(4) = ymax
!
!  save world to virtual coord. transformation const.
!
      area(5) = ux0
      area(6) = udx
      area(7) = uy0
      area(8) = udy
      area(9) = 0.0
      if(logx) area(9) = 1.0
      area(10) = 0.0
      if(logy) area(10) = 1.0
!
!  now save virt. coord. location of axes
!
      area(11) = xvstrt
      area(12) = yvstrt
      area(13) = xvlen
      area(14) = yvlen
!
!  all done
!
      return
      end
      subroutine scale(x,y,vx,vy)
!
!***********************************************************************
!
!! SCALE converts from world coordinates to virtual coordinates.
!
!  X,Y, input, the world coordinates.
!  VX, VY, output, the corresponding virtual coordinates.
!
      real smllog
      parameter (smllog=-100.0)
!
      logical logx
      logical logy
!
      save /pltcom/
      save /pltsiz/
!
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      xx = x

      if (logx) then

        if(x .gt. 0.0) then
          xx = alog10(x)
        else
          xx = smllog
        end if
 
      end if

      yy = y
 
      if ( logy ) then

        if(y .gt. 0.0) then
          yy = alog10(y)
        else
          yy = smllog
        end if

      end if

      vx = xvstrt + xvlen*(xx-ux0)/udx
      vy = yvstrt + yvlen*(yy-uy0)/udy

      return
      end
      subroutine scalin(amin,amax,axmax,orig,step,raxis)
!
!***********************************************************************
!
!! SCALIN calculates rounded linear axis limits.
!  level 1-3
!
!  input:   amin,amax = range of data
!  axmax     = maximum axis length (inches)
!
!  output:  orig = rounded value at origin
!  step = rounded step size
!  axis = rounded minimum axis length
!  in inches
!
      logical l1,l2
!
      save /clevel/
      save /cpage/
      save /cunit/
      save /pltprm/
      save /pltsiz/
!
      common /clevel/ kzlevl,kzbegn
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      common /pltprm/ cxsize, cysize, tickln, yvini
 
      l1=.false.
      l2=.false.

      if(kzlevl.ge.1.and.kzlevl.le.3)then

        if(kzlevl.eq.1)then
          size=0.3556
        else
          size=cxsize
        end if

        numtk=min0(10,int(axmax*zzunit*zzpagr/
     &                      ((ilabsz()+1.0)*size)))
        call axis(amin,amax,numtk,l1,l2,bmin,bmax,
     &              btmin,btmax,btick,ipow)
        orig=bmin
        step=btick*10.0**ipow
        perin=step*axmax/(bmax-bmin)
        if(perin.gt.1.8) step=step/2.0
        raxis=(bmax-bmin)/step
      else
        call errmes('SCALIN',1,3)
      end if

      return
      end
      subroutine scalog(alow,ahi,axis,origin,cycle)
!
!***********************************************************************
!
!  SCALOG calculates the origin and cycle for logarithmic axes.
!  (level 1-3)
!
!  input:   alow   = lower limit of data
!  ahi    = higher limit of data
!  axis   = axis length
!
!  output:  origin = calculated lower limit of data
!  cycle  = cycle length in inches
!
      save /clevel/
      save /cunit/
      save /pltprm/
!
      common /cunit/  zzunit
      common /clevel/ kzlevl,kzbegn
      common /pltprm/ cxsize, cysize, tickln, yvini
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        raxis=axis*zzunit
!
!  cxsize may not be initialized
!  maxtk=min0(10,int(raxis/((ilabsz()+1.0)*cxsize)))
!
        maxtk=min0(10,int(raxis/((ilabsz()+1.0)*gslens('0'))))
        call laxis(alow,ahi,maxtk,bmin,bmax,btick)
        origin=bmin
        delta=bmax-bmin
        if(delta.lt.0.00005)then
          cycle=-1.0
          call errmes('SCALOG',0,4)
        else
          cycle=axis/delta
        end if
      else
        call errmes('SCALOG',1,3)
      end if
      return
      end
      subroutine segmnt(ici,icj,iseg)
!
!***********************************************************************
!
!! SEGMNT...
!
      save /contr/
!
      common /contr/ clevel,iold,jold,in,jn,
     &   nx,ny,xl,dx,yl,dy

      ici=min(iold,in)
      icj=min(jold,jn)
      iseg=1
      if(iold .eq. in) iseg=2

      return
      end
      subroutine setout(isum)
!
!***********************************************************************
!
!! SETOUT sets the output device.
!  (level 1-3, p/s)
!
!  input
!  isum = output unit for plot summary
!
      save /cdevic/
!
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
!
      kzsum=isum

      return
      end
      subroutine setsub(ax,ay)
!
!***********************************************************************
!
!! SETSUB defines a subplot area and system parameters, and draws a frame.
!  (level 1, raised to 2)
!
!  input:   ax,ay = lengths of x and y axes
!
      parameter (kzyes=111)
      parameter (kzno=222)
      parameter (kzclip=12)
      parameter (kzdown=13)
      parameter (kzscrn=14)
      parameter (kzabrt=15)
      parameter (zzin=2.54)
!
      save /carea/
      save /cborch/
      save /ciount/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cline/
      save /cpage/
      save /cphysr/
      save /csymbo/
      save /cstrng/
      save /cunit/
      save /gcclip/
      save /gcdchr/
!
      common /clevel/ kzlevl,kzbegn
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /csymbo/ kzsym,kznsym,zzsmsz,uusmsz
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /cunit/  zzunit
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cborch/ kzbrdr,kzchek
      common /ciount/ kziunt, kzount
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5
!
      save kqms,kln03,kpost
      data kqms,kln03,kpost /1200,3,910/
!
!  check level
!
      if(kzlevl.eq.1)then
!
!  rotate plot if desired
!
        kdev=int(devid)
        if((kdev.eq.kqms.or.kdev.eq.kln03.or.kdev.eq.kpost)
     &         .and.kzauto.eq.kzyes)then
          if(uupagx.lt.uupagy)then
            call zlasap
          else
            call zlasal
          end if
        end if
!
!  erase screen for new plot
!
        if(kzbegn.ne.kzyes)then
!  call gsdrvr(2,dummy,dummy)
          kzbegn=kzyes
        end if
!
!  convert to system unit
!
        rx=xlencm/uupagx
        ry=ylencm/uupagy
        if(rx.lt.ry)then
          zzpagr=rx
        else
          zzpagr=ry
        end if
        if(kzscal.eq.kzdown)then
          if(zzpagr.gt.1.0)then
            zzpagr=1.0
          end if
          zzpagx=uupagx*zzpagr
          zzpagy=uupagy*zzpagr
        elseif(kzscal.eq.kzscrn)then
          zzpagx=uupagx*zzpagr
          zzpagy=uupagy*zzpagr
        elseif(kzscal.eq.kzclip)then
          zzpagx=uupagx
          zzpagy=uupagy
          zzpagr=1.0
        elseif(kzscal.eq.kzabrt)then
          if(rx.lt.1.0.or.ry.lt.1.0)then
            kzlevl=1
            return
          else
            zzpagx=uupagx
            zzpagy=uupagy
            zzpagr=1.0
          end if
        end if

        if(kzlthk.eq.kzyes) zzlthk=uulthk*zzpagr
        zzsmsz=uusmsz*zzpagr
        zzgrce=uugrce*zzpagr
        zzhite=uuhite*zzpagr
        call gssetc(zzhite,zzangl)
!w        zzlgtz=uulgtz*zzpagr
        zzsmsz=uusmsz*zzpagr
        uuyaxs=ay*zzunit
        uuxaxs=ax*zzunit
        zzxaxs=uuxaxs*zzpagr
        zzyaxs=uuyaxs*zzpagr
        zzfrme=uufrme*zzpagr
!
!  check if ORIGIN is called
!
        if(kzor.ne.kzyes)then
          dx=uupagx-uuxaxs
          dy=uupagy-uuyaxs
          if(dx.gt.1.0)then
            uuxor=dx/2.0
          else
            uuxor=0.5*zzin
            if(dx.lt.0.5)then
              call errmes('SETSUB',0,4)
            end if
          end if
          if(dy.gt.1.0)then
            uuyor=dy/2.0
          else
            uuyor=0.5*zzin
            if(dy.lt.0.5)then
              call errmes('SETSUB',0,5)
            end if
          end if
        end if
        zzxor=uuxor*zzpagr
        zzyor=uuyor*zzpagr
!
!  calculate frame
!
        pxmin=(xlencm-zzpagx)/2.0
        pymin=(ylencm-zzpagy)/2.0
        pxmax=pxmin+zzpagx
        pymax=pymin+zzpagy
        xcm0=pxmin
        xcm1=pxmax
        ycm0=pymin
        ycm1=pymax
!
!  check if NOBORD is called
!
        if(kzbrdr.ne.kzno)then
          call dsmove(pxmin,pymin)
          call dsdraw(pxmin,pymax)
          call dsdraw(pxmax,pymax)
          call dsdraw(pxmax,pymin)
          call dsdraw(pxmin,pymin)
        end if
!
!  set plot area parameters
!
        zzxlft=pxmin+zzxor
        zzxaxr=(pxmax-zzxlft)/zzxaxs
        if(zzxaxr.gt.1.0)then
          zzxaxr=1.0
          zzxrgt=zzxlft+zzxaxs
        else
          zzxrgt=pxmax
        end if
        zzybot=pymin+zzyor
        zzyaxr=(pymax-zzybot)/zzyaxs
        if(zzyaxr.gt.1.0)then
          zzyaxr=1.0
          zzytop=zzybot+zzyaxs
        else
          zzytop=pymax
        end if
        tick=0.6*zzhite
        call zmaprm(zzxlft,zzxrgt,zzybot,zzytop,zzhite,tick)
        kzlevl=2
      else
        call errmes('SETSUB',1,0)
      end if

      return
      end
      subroutine setunt(scale)
!
!***********************************************************************
!
!! SETUNT sets the input unit converting scale.
!
!  (level 1-3, p/s)
!
!  input:   scale = scale flag
!  if = 'in', in inches, default
!  if = 'cm' or 'cent', centimeter
!  if = 'MM' or 'MILL',millimeter
!
      parameter (zzin=2.54)
      parameter (zzcm=1.00)
      parameter (zzmm=10.0)
      parameter (numbyt=4)
!
      character*(numbyt) cscale
      real rscale
      real scale
!
      save /cunit/
!
      common /cunit/  zzunit
!
      rscale=scale
      call win2ch(rscale,cscale)
 
      call capchr(cscale)

      if(cscale(1:2).eq.'IN')then
        zzunit=zzin
      elseif(cscale(1:2).eq.'CM'.or.cscale.eq.'CENT')then
        zzunit=zzcm
      elseif(cscale(1:2).eq.'MM'.or.cscale.eq.'MILL')then
        zzunit=zzmm
      else
        zzunit=scale
      end if

      return
      end
      subroutine shade(x,y,npts,angle,garay)

!***********************************************************************
!
!! SHADE carries out polygon fill.
!
!  level 3
!
!  input:   x,y     = x and y values of polygon vertices
!  npts    = number of x y pairs in x and y
!  angle   = cross hatching line angle
!  garay   = gap array, only one value is used
!  in dissim
!
      parameter (kzyes=111)
      parameter (kzmaxc=255)
      parameter (maxpts=1000)
      parameter (max1=maxpts+1)
      parameter (max2=maxpts+2)

      character czcolr*8
      real x(*)
      real y(*),xx(max2),yy(max2),tx(max2),ty(max2)

      save /clevel/
      save /colorc/
      save /colorn/
      save /cpage/
      save /cunit/

      common /clevel/ kzlevl,kzbegn
      common /colorn/ kzshd,zzhspc,zzcolr(kzmaxc,3),kznclr,kzccol
      common /colorc/ czcolr
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto

      if(kzlevl.eq.3)then
!
!  if hardware shade is turned on
!
        if(kzshd.eq.kzyes)then
          nround=npts/maxpts
!
!  if number of points is greater than 'maxpts', do it piece by piece
!
          if(nround.gt.0)then
            j1=1-maxpts
            call scale(x(1),y(1),xx(max1),yy(max1))
            do j=1,nround
              j1=j1+maxpts
              j2=j1+maxpts-1
              j11=j1-1
 
              do k=j1,j2
                k1=k-j11
                call scale(x(k),y(k),xx(k1),yy(k1))
              end do
 
              if(j1.le.1)then
                call dsfill(xx,yy,maxpts,tx,ty)
              else
                xx(max2)=xx(j1-1)
                yy(max2)=yy(j1-1)
                call dsfill(xx,yy,max2,tx,ty)
              end if
            end do
 
            if(j2.lt.npts)then
              xx(1)=xx(max1)
              yy(1)=yy(max1)
              xx(2)=xx(maxpts)
              yy(2)=yy(maxpts)
            j3=j2-2
 
              do i=j2+1,npts
                i1=i-j3
                call scale(x(i),y(i),xx(i1),yy(i1))
              end do
 
              call dsfill(xx,yy,npts-j3,tx,ty)
            end if
!
!  if number of points is less or equal to 'maxpts' do it in one piece
!
          else
            do i=1,npts
              call scale(x(i),y(i),xx(i),yy(i))
            end do
            call dsfill(xx,yy,npts,tx,ty)
          end if
!
!  hardware shade not turned on, do software fill
!
        else
          if(npts.le.max2)then
            gap=garay*zzunit*zzpagr
            call dhatch(x,y,npts,angle,gap,2,tx,ty)
          else
            call errmes('SHADE',0,4)
          end if
        end if
      else
        call errmes('SHADE',3,0)
      end if
      return
      end
      subroutine simplx
!
!***********************************************************************
!
!! SIMPLX sets the character type to simplex.
!  level 1-3, p/s
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      character czalfl*1,czalfn*5,czalfs*5
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfs='SIMPL'
      else
        call errmes('SIMPLX',1,3)
      end if
 
      return
      end
      function slngth(lmess,imess)
!
!***********************************************************************
!
!! SLNGTH returns the length of a string in inches.
!  (level 1-3)
!
!  input:   lmess = character string
!  imess = number of character in lmess
!
!  output:  slngth = length of input string in inches
!
      character czalfl*1,czalfn*5,czalfs*5
      character*(*) lmess(*)
      dimension lmessa(38)
!
      save /clevel/
      save /cmxalf/
      save /cpage/
      save /cunit/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cunit/  zzunit
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
!
!  if length excluding trailing blanks is desired
!
        if(imess.lt.0)then
          call zstrbt(lmess)
        end if
!
!  call routine to calculate slngth
!
        slngth=zxmess(lmess,abs(imess),czalfl,czalfn,czalfs)
        if(kzlevl.eq.1)then
          slngth=slngth/zzunit
        else
          slngth=slngth/zzunit/zzpagr
        end if
        length=leng(lmessa)
!
!  if input is in character format
!
        if(length.eq.1.and.length.ne.imess)then
          slngth=slngth*real(imess)
        end if
!
!  wrong level
!
      else
        slngth=0.0
        call errmes('SLNGTH',1,3)
      end if
 
      return
      end
      subroutine sizmrk(fac)
!
!***********************************************************************
!
!! SIZMRK sets the symbol size.
!  level 1-3, p/s
!
!  input:   fac = multiple of basic size, which is 0.08 inch
!
      parameter (zzin=2.54)
!
      save /clevel/
      save /cpage/
      save /csymbo/
!
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /clevel/ kzlevl,kzbegn
      common /csymbo/ kzsym,kznsym,zzsmsz,uusmsz
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        uusmsz=fac*0.08*zzin
        if(kzlevl.eq.2.or.kzlevl.eq.3)then
          zzsmsz=fac*0.08*zzin*zzpagr
        end if
      else
        call errmes('SIZMRK',1,3)
      end if
      return
      end
      subroutine stoplt
!
!*********************************************************************72
!
!! STOPLT terminates the current plot.
!  (level 2,3, change to 1)
!
      parameter (kzyes=111)
!
      save /cdevic/
      save /ciount/
      save /gcdchr/
!
      common /ciount/ kziunt, kzount
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
      dummy = 0.0
!
!  determine auto-copy option setting.
!
      if(kzcopy.eq.kzyes) dummy = 1.0
 
      call gsdrvr(5,dummy,dummy)
 
      kznplt=kznplt+1
 
      call zinit
 
      return
      end
      subroutine syaxis(ylow,yhigh,ylab,iaxes)
!
!*********************************************************************72
!
!! SYAXIS...
!
      character ylab*2
      character numbr*14
      character nequiv(14)
      logical logyy, logt, lrmtex, lshort, lraggd
      dimension zlog(8)
      logical logx, logy
!
      save /pltclp/
      save /pltcom/
      save /pltprm/
      save /pltsiz/
!
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      common /pltclp/ xmin,xmax,ymin,ymax
      common /pltprm/ cxsize, cysize, tickln, yvini
!
!  external len
      equivalence(numbr,nequiv)
!
      save zlog,tminld,shortf
!
      data zlog /0.3010, 0.4771, 0.6021, 0.6990, 0.7782, 0.8451,
     &   0.9031, 0.9542 /
      data tminld /0.1/
      data shortf /2.0/
!
      delmx=0
      yvlen = yvini
!
!  set logy to false for our usage of scale
!
      logy = .false.
!
!  see what type of axis is desired
!
      logyy = ksyand(iaxes,2) .ne. 0
      lraggd = ksyand(iaxes,256) .ne. 0
!
!  do the axes scaling
!
      numtk = min0(10,int(yvlen/(3.0*cysize)))

      if(.not.logyy) then
        lshort = ksyand(iaxes,32) .ne. 0
        call axis(ylow,yhigh,numtk,lshort,lraggd,ymin,ymax,ytmin,ytmax,
     &    ytick,iypwr)
      else
        call laxis(ylow,yhigh,numtk,ymin,ymax,ytick)
        ytmin = ymin
        ytmax = ymax
        iypwr = 0
      end if
!
!  set up temporary scaling factors
!
      uy0 = ymin
      udy = ymax - ymin
!
!  draw y axes
!
      call gssetc(cysize,0.0)
      logt = .false.
      if(.not. logyy .or. ytick .ne. 1.0) go to 90
      call scale(xmin,ymin,vx,temp)
      call scale(xmin,ymin+1.0-zlog(8),vx,vy)
      if((vy-temp) .ge. tminld) logt = .true.
90    continue
!
!  draw y axis line
!
      tenexp = 10.0**iypwr
      x = xmax
      ticksp = max (0.0,tickln)
      if(ksyand(iaxes,64) .ne. 0) yvlen = yvlen - ticksp
      tcksgn = -tickln
100   continue
      call scale(x,ymax,vx,vy)
      call gsmove(vx,vy)
      call scale(x,ymin,vx,vy)
      call gsdraw(vx,vy)
!
!  draw and label y axis ticks
!
      y = ytmin
      n = (ytmax-ytmin)/ytick + 1.1
110   continue
      call scale(x,y*tenexp,vx,vy)
      call gsmove(vx,vy)
      call gsdraw(vx+tickln,vy)
!
!  place the appropiate label
!
      if(ksyand(iaxes,1024) .ne. 0) go to 183
      if(logyy) go to 160
      call linlab(int(y),iypwr,numbr,lrmtex)
      go to 180
160   call loglab(int(y),numbr)
180   del = gslens(numbr)
      delmx = max (del,delmx)
      call gsmove(vx+ticksp+0.5*cxsize,vy-cysize/2.0)
      call gspstr(numbr)
183   continue
!
!  add grid line at tick if desired
!
      if(ksyand(iaxes,8) .eq. 0) go to 185
      call gsltyp(3)
      call gsmove(vx,vy)
      call scale(xmin,y*tenexp,vx,vy)
      call gsdraw(vx,vy)
      call gsltyp(1)
185   continue
!
!  do extra ticking if extra ticks will be far enough apart
!
      if((.not. logt) .or. (y .eq. ytmax)) go to 200
      do j = 1, 8
        call scale(x,y+zlog(j),vx,vy)
        call gsmove(vx,vy)
        call gsdraw(vx+tickln/shortf,vy)
      end do
 
200   continue
      y = y + ytick
      n = n-1
      if(n .gt. 0) go to 110
!
!  if linear axis, place remote exponent if needed
!
      if(logyy .or. (.not. lrmtex)) go to 260
      if(ksyand(iaxes,1024) .ne. 0) go to 260
      call scale(xmax,(ytmin+ytick/2.0)*tenexp,vx,vy)
      call zscopy('E',numbr)
      call numstr(iypwr,nequiv(2))
      call gsmove(vx+0.5*cxsize,vy-cysize/2.0)
      call gspstr(numbr)
!
!  now place y label
!
260   call scale(x,(ymin+ymax)/2.0,vx,vy)
      call gsmove(vx+0.5*cxsize+delmx+ticksp+1.5*cysize,
     &   vy-gslens(ylab)/2.0)
      call gssetc(cysize,90.0)
      call gspstr(ylab)
      call gssetc(cysize,0.0)
300   continue
!
!  tell user the scaling limits
!
      if(.not. logyy) go to 320
       ymin = 10.0**ymin
       ymax = 10.0**ymax
320   continue
!
!  tell scale about log axis scaling now
!
      logy = logyy
      return
      end
      subroutine symbol(isymno,symsiz)
!
!*********************************************************************72
!
!! SYMBOL displays a symbol of a given size, at a given plot location.
!
! internal variables:
!  symmov     (no. of moves) array of consecutive x,y locations to
!  which line is drawn
!  isymst     (no. of symbols + 1) array start of symbol move locations
!
!  Symbol definitions:
!  1>  square
!  2>  octagon
!  3>  triangle
!  4>  plus sign
!  5>  'X'
!  6>  diamond
!  7>  upside down triangle
!  8>  square with an 'X' in it
!  9>  'X' with a horizontal line across it
!  10>  diamond with a plus sign in it
!  11>  octagon with a plus sign in it
!  12>  double hour glass
!  13>  square with a plus sign in it
!  14>  octagon with a 'X' in it
!  15>  square with a triangle in it
!  16>  pentagon with a plus sign in it
!  17>  pentagon
!  18>  five pointed star
!  19>  square with a diamond in it
!
      save /gcvpos/
!
      common /gcvpos/ xvpos, yvpos
      dimension symmov(105),isymst(1:19,4)
!
      save symmov,isymst,nsym
!
!  square: (1)
!  octagon: (12)
!  triangle: (31)
!  upside down triangle: (40)
!  horizontal line: (49)
!  vertical line: (54)
!  reversed slash: (59)
!  slash: (64)
!  diamond: (69)
!  star: (80)
!  pentagon: (93)
!
      data symmov/
     & 0.5,-0.5, 0.5,0.5,  -0.5,0.5,  -0.5,-0.5,  0.5,-0.5,  1000.0,
     & 0.2071,-0.5, 0.5,-0.2071, 0.5,0.2071, 0.2071,0.5, -0.2071,0.5,
     & -0.5,0.2071, -0.5,-0.2071, -0.2071,-0.5, 0.2071,-0.5, 1000.0,
     & 0.5,-0.5,  0.0,0.5,  -0.5,-0.5,  0.5,-0.5, 1000.0,
     & 0.5,0.5,   0.0,-0.5,  -0.5,0.5,  0.5,0.5, 1000.0,
     & -0.5,0.0,  0.5,0.0,  1000.0,
     & 0.0,0.5,  0.0,-0.5,  1000.0,
     & -0.5,0.5,  0.5,-0.5, 1000.0,
     & -0.5,-0.5,  0.5,0.5,  1000.0,
     & 0.5,0.0,  0.0,0.5,  -0.5,0.0,  0.0,-0.5,  0.5,0.0,  1000.0,
     & 0.5,0.2, -0.5,0.2, 0.3,-0.5, 0.0,0.5, -0.3,-0.5,
     & 0.5,0.2, 1000.0,
     & 0.5,0.0,  0.0,0.5,  -0.5,0.0,  -0.25,-0.5,  0.25,-0.5,
     & 0.5,0.0, 1000.0/
      data isymst/
     &  1,12,31,54,64,69,40,59,49,69,12,40,49,12, 1,93,93,80,69,
     &  0, 0, 0,49,59, 0, 0,64,64,54,54,31,54,64,31,54, 0, 0, 1,
     &  0, 0, 0, 0, 0, 0, 0, 1,59,49,49, 0, 1,59, 0,49, 0, 0, 0,
     &  1, 1, 1, 2, 2, 1, 1, 3, 3, 3, 3, 2, 3, 3, 2, 3, 1, 1, 2/
      data nsym /19/
!
!  save current location
!
        x0 = xvpos
        y0 = yvpos
!
!  draw symbol in proper size
!
        if(isymno.ge.1  .and.  isymno.le.nsym)then
          do line=1,isymst(isymno,4)
          iptr = isymst (isymno,line)
            call gsmove(x0+symsiz*symmov(iptr),
     &           y0+symsiz*symmov(iptr+1))
          do i=1,12
            iptr=iptr+2

              if(symmov(iptr).gt.999.0) go to 200
              call gsdraw(x0+symsiz*symmov(iptr),
     &                    y0+symsiz*symmov(iptr+1))
            end do
  200     continue

        end do
      end if
 
      call gsmove(x0,y0)
 
      return
      end
      subroutine system2(func,str1)
!
!*********************************************************************72
!
!! SYSTEM2 returns the current host system information.
!
!  SYSTEM2 also performs system calls.
!
!  installation notes:
!  1) this is a system specific routine. all statements
!  for current system should be un-commented at the
!  time of installation. hint: save a backup copy and
!  use global substitute.
!  2) if you cannot find any statement for your system,
!  un-comment the 'unknown' ones. or you can add the
!  part for your system. we do appreciate it if you
!  could send your modified version to either
!  j.e. rogers/es2, johnson space center, houston,
!  texas 77058 or c.j. wong/b14, lockheed emsco,
!  p.o. box 58561, houston, texas 77258. we will
!  include it in the next release.
!
!  system id - return system type
!  calling sequence: call system2('SYSTEM',str1)
!  input:  func = 'SYSTEM'
!  output: str1 - system type, character*6
!
!  current date - return current date
!  calling sequence: call system2('date',str1)
!  input:  func = 'DATE'
!  output: str1 - current date, character*8
!
!  sybyt4 - extract or insert character from integer array
!  calling sequence: call sybyt4(cflag,inp,ipos,jchar)
!  input:  cflag = operation identifier
!  if = 'X', to extract
!  if = 'N', to insert
!  inp   = integer array to be extracted from
!  ipos  = byte position of interest
!  jchar = ascii equivalence of char to be inserted
!  output: inp   = modified integer array after insertion
!  jchar = ascii equivalence of extracted char
!
!  syfchk - perform file name verification
!  calling sequence: call syfchk(str1,str2)
!  input:  str1 - input file name and output file name
!  str2 - file suffix
!  this checking can either be dummied or can contain
!  any type of format checking desired.
!  a) for the vax, the file name is checked to determine
!  if an extension has been used with the name.  if it
!  has, the name is returned intact; however, if no
!  extension has been specified, the one contained in
!  the argument "suffix" is appended to the name.
!
!  syfnsh - close system output channel
!  calling sequence: call syfnsh(relstr)
!  input:  relstr = string required to close output channel
!
!  syinit - initialize system input channel
!  calling sequence: call syinit(khar,endstr,ierr)
!  input:  khar   =
!  endstr = character string required at end of
!  each buffer dump
!  ierr   = status of operation
!
!  sywait - perform system wait
!  calling sequence: call sywait(milli)
!  input:  milli - number of milliseconds to wait
!
      parameter (ibfsiz=79)
!
      parameter (numbyt=4)
!
      character buffer
      character trmchr
      character begstr
      character cflag
      character*8 date
      character*(*) endstr
      character*4 func
      integer inp(*)
      character*(*) khar
      character kode*11
      logical luterm
      character ndstng
      character*(*) relstr
      character*(*) str1
      character*(*) str2
      character*10 time
!
      save /cbyte4/
      save /cdevic/
      save /gbbufc/
      save /gbbufr/
!
      common /gbbufr/ ibfptr, ndlnth, ibegln,iochan, luterm
      common /gbbufc/ buffer(ibfsiz),ndstng(10),begstr(10),trmchr
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /cbyte4/ kzmask(8),kzcpos(8)
!
!  return system type
!
      call capchr(func)

      if(func.eq.'SYST')then

        str1 = 'Mac OSX'
!
!  return current date
!
      else if ( func.eq.'DATE' ) then

        call date_and_time ( date, time )
        read ( date, '(i4,i2,i2)' ) iyear, imon, iday

      end if

      return

      entry sybyt4(cflag,inp,ipos,jchar)
!
!*********************************************************************72
!
!! SYBYT4 extracts or inserts a character from or into a 4 byte integer.
!
!
!  note:    1. this subroutine is tested on
!  vax/vms, and ibm/pc. kzmask and kzcpos are
!  in common block 'cbyte4', initialized in
!  'zinit'.
!  2. kzmask is the mask to zero out the byte to
!  which a character is to be inserted. for
!  example, when an integer is 'and'ed with a
!  mask of 7f7f7f00 hex, the least significant
!  byte is zeroed.
!  3. kznpos is position of the byte of interest.
!  for extraction, kznpos is the bits to shift
!  right to place the desired byte in the least
!  significant position. for insertion, kznpos
!  is the bits to shift left to place the byte
!  to be inserted in the right position.
!
!
!  calculate the exact location
!
      if(ipos.gt.kzbyte)then
        is=(ipos-1)/kzbyte
        iptr=is+1
        ip=ipos-is*kzbyte
      else
        iptr=1
        ip=ipos
      end if
!
!  to extract a character
!
      if(cflag.eq.'X')then
!-------------------------begin vax vms specific code----------------------
         jchar=ibits(inp(iptr),(ip-1)*8,7)
!--------------------------end vax vms specific code-----------------------
!-------------------------begin cray specific code-------------------------
!  jchar=ibits(inp(iptr),56-(ip-1)*8,7)
!--------------------------end cray specific code--------------------------
!-------------------------begin iris specific code-------------------------
!  jchar=ibits(inp(iptr),24-(ip-1)*8,7)
!--------------------------end iris specific code--------------------------
!
!-------------------------begin cms-dos specific code----------------------
!  if(inp(iptr).lt.0)then
!  is=ksyand(maxint,inp(iptr))
!  else
!  is=inp(iptr)
!  end if
!  jchar=ksyand(is/kzcpos(ip),127)
!--------------------------end cms-dos specific code-----------------------
!------------------------begin vax unix specific code----------------------
!  if(inp(iptr).lt.0)then
!  is=ksyand(maxint,inp(iptr))
!  else
!  is=inp(iptr)
!  end if
!  jchar=ksyand(ksyshr(is,(ip-1)*8),127)
!-------------------------end vax unix specific code-----------------------
!-----------------------------begin generic code---------------------------
!  if(inp(iptr).lt.0)then
!  is=ksyand(maxint,inp(iptr))
!  else
!  is=inp(iptr)
!  end if
!  jchar=ksyand(ksyshr(is,(numbyt-ip)*8),127)
!------------------------------end generic code----------------------------
!
!  to insert a character
!
      else
!-------------------------begin vax vms specific code----------------------
         call mvbits(jchar,0,7,inp(iptr),(ip-1)*8)
!--------------------------end vax vms specific code-----------------------
!-------------------------begin cray specific code-------------------------
!  call mvbits(jchar,0,7,inp(iptr),56-(ip-1)*8)
!--------------------------end cray specific code--------------------------
!-------------------------begin iris specific code-------------------------
!  call mvbits(jchar,0,7,inp(iptr),24-(ip-1)*8)
!--------------------------end iris specific code--------------------------
!-------------------------begin cms-dos specific code----------------------
!  inp(iptr)=ksyand(inp(iptr),kzmask(ip))+jchar*kzcpos(ip)
!--------------------------end cms-dos specific code-----------------------
!------------------------begin vax unix specific code----------------------
!  inp(iptr)=ksyor(ksyand(inp(iptr),kzmask(ip)),
!  .                   ksyshl(jchar,(ip-1)*8))
!-------------------------end vax unix specific code-----------------------
!-----------------------------begin generic code---------------------------
!  inp(iptr)=ksyor(ksyand(inp(iptr),kzmask(numbyt-(ip-1))),
!  .                   ksyshl(jchar,(numbyt-ip)*8))
!------------------------------end generic code----------------------------
      end if
      return

      entry syfchk(str1,str2)
!
!***********************************************************************
!
!! SYFCHK performs file name verification.
!
!
!-----------------------------begin generic code---------------------------
      nc = numchr(str1)
      ln = numchr(str2)
      is = 0
 
      do i=1,nc
        if(str1(i:i) .eq. ']') is = i
      end do
 
      is = is + 1
 
      do i=is,nc
        if(str1(i:i) .eq. '.')then
          if(nc .gt. i) return
          nc = i
          go to 40
        end if
      end do
 
      nc = nc + 1
      str1(nc:nc) = '.'
   40 continue
      str1(i+1:i+ln) = str2(1:ln)
!------------------------------end generic code----------------------------
      return

      entry syfnsh(relstr)
!
!***********************************************************************
!
!! SYFNSH releases the i/o channal to the output device.
!
!
      if(numchr(relstr) .ne. 0)then
!
!       call gbmpty
!       call gbinst(relstr)
!       call gbmpty
!
      end if
 
      return
      entry syinit(khar,endstr,ierr)
!
!***********************************************************************
!
!! SYINIT initializes the graphics drivers buffering routines.
!
! 
      ierr = 0
      trmchr = khar
      ndlnth = numchr(endstr)
 
      do i=1,ndlnth
        ndstng(i) = endstr(i:i)
      end do
 
      begstr(1) = char(0)
      ibegln = 0
      return

      entry sywait(milli)
!
!***********************************************************************
!
!! SYWAIT performs a system wait.
!
 500  format('  0 ::',f5.2)
      id = 0
      sec = milli/1000.
      if(sec .le. 0.) return
  600 continue
      one = sec
      if(one .gt. 59.99) one = 59.99
      write(kode,500) one
!
!  vax/vms device driver code
!
!  istat = sys$bintim(kode,iwait)
!  if(.not. istat) call lib$stop(%val(istat))
!  istat = sys$setimr(%val(id),iwait,,)
!  if(.not. istat) call lib$stop(%val(istat))
!  istat = sys$waitfr(%val(id))
!  if(.not. istat) call lib$stop(%val(istat))
!
      sec = sec - 59.99
      if(sec .gt. 0) go to 600
 
      return
      end
      subroutine tickl(anum,up)
!
!***********************************************************************
!
!! TICKL...
!
      save /dbase/
!
      common /dbase/vx,vy,voldx,voldy,cxsize,cysize
      character numbr*6
!
      write(numbr,'(f6.2)') anum
 
      do i=1,6
        if(numbr(1:1) .ne. ' ') go to 30
        numbr(1:5) = numbr(2:6)
        numbr(6:6) = ' '
      end do
 
30    continue
      is = numchr(numbr)
      temp = cxsize*((7-is)+0.25)
      if(vx .gt. voldx) temp = -0.25*cxsize
      call gsmove(vx-temp,vy+up*cysize)
      call gspstr(numbr)

      return
      end
      subroutine traccy(xmin,xmax,y,npts)
!
!***********************************************************************
!
!! TRACCY traces the line from x(1),y(1) to x(npts),y(npts) with clipping.
!
!  use this routine when clipping is desired and the
!  independant variable is implied by the subscript
!  using equal intervals from xmin to xmax.
!
      integer npts
!
      real y(npts)
!
      save /pltsiz/
!
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      dimension area(4)
!
      call gssclp(xvstrt,xvstrt+xvlen,yvstrt,yvstrt+yvlen,area)
      call scale(xmin,y(1),vx,vy)
      call gsmove(vx,vy)
      dx = (xmax-xmin)/(npts-1)
 
      do i=2,npts
        call scale(xmin+(i-1)*dx,y(i),vx,vy)
        call gsdraw(vx,vy)
      end do
 
      call gsrclp(area)
      return
      end
      subroutine trace(x,y,npts)
!
!***********************************************************************
!
!! TRACE plots data on the screen as a continous line.
!
      integer npts
!
      real x(npts)
      real y(npts)
!
      call scale(x(1),y(1),vx,vy)
      call gsmove(vx,vy)
!
      do i=2,npts
        call scale(x(i),y(i),vx,vy)
        call gsdraw(vx,vy)
      end do
 
      return
      end
      subroutine tracec(x,y,npts)
!
!***********************************************************************
!
!! TRACEC traces the line from x(1),y(1) to x(npts),y(npts) with clipping.
!
      integer npts
!
      real area(4)
      real x(npts)
      real y(npts)
!
      save /pltsiz/
!
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      call gssclp(xvstrt,xvstrt+xvlen,yvstrt,yvstrt+yvlen,area)
      call scale(x(1),y(1),vx,vy)
      call gsmove(vx,vy)
 
      do i=2,npts
        call scale(x(i),y(i),vx,vy)
        call gsdraw(vx,vy)
      end do
 
      call gsrclp(area)
      return
      end
      subroutine tracey(xmin,xmax,y,npts)
!
!***********************************************************************
!
!! TRACEY plots data as a continous line, given the y array and xmin and xmax.
!
      dimension y(2)
      call scale(xmin,y(1),vx,vy)
      call gsmove(vx,vy)
      xinc = (xmax-xmin)/(npts-1)
      x = xmin
!
      do i=2,npts
        x = x + xinc
        call scale(x,y(i),vx,vy)
        call gsdraw(vx,vy)
      end do
 
      return
      end
      subroutine triplx
!
!***********************************************************************
!
!! TRIPLX sets the character type to triplex.
!  level 1-3, p/s
!
      save /clevel/
      save /cmxalf/
!
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      character czalfl*1,czalfn*5,czalfs*5
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        czalfs='TRIPL'
      else
        call errmes('TRIPLX',1,3)
      end if
 
      return
      end
      subroutine trmchr(istr,length)
!
!***********************************************************************
!
!! TRMCHR sets the character string terminator.
!  level 1-3, p/s
!
!  input:   istr   = new string terminator
!  length = number of characters in istr
!
      save /clevel/
      save /cstrng/
!
      common /clevel/ kzlevl,kzbegn
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzstrm=istr
        kztmln=length
      else
        call errmes('TRMCHR',1,3)
      end if
 
      return
      end
      subroutine txtblk(ip,nlines,xpos,ypos)
!
!***********************************************************************
!
!! TXTBLK writes a packed character array and plot symbols in legend format.
!  It can also draw a line if desired.
!  (level 3)
!
!  input:   ip     = packed array of characters
!  nlines = number of lines in packed array
!  xpos   = x value from physical origin in inches
!  ypos   = y value from physical origin in inches
!
      parameter (kzyes=111)
!
      save /cdevic/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cline/
      save /cpage/
      save /cphysr/
      save /cstrng/
      save /csymbo/
      save /cunit/
      save /gcclip/
!
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /clevel/ kzlevl,kzbegn
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /csymbo/ kzsym,kznsym,zzsmsz,uusmsz
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5
      character ceqsgn*1
!
      dimension ip(*),x(4),y(4)
      character*5 cflag(6)*1,cfont(6),cstyle
      logical wasthk,lshift
!
      equivalence (ival,rval)
!
      save ceqsgn,jtext,jhite,jyrat,jave,jsymsp
      data ceqsgn /'='/
      data jtext,jhite,jyrat,jave,jsymsp /1,-2,-1,2*0/
      if(kzlevl.eq.3)then
!
!  save old text parameters
!
        if(kzlger.eq.kzyes) call errmes('ERRMES',0,4)
        ohite=zzhite
        oangle=zzangl
        if(kzlthk.eq.kzyes)then
          oldthk=zzlthk/zzunit/zzpagr
          wasthk=.true.
        else
          wasthk=.false.
        end if
        call reset('CRVWID')
!
!  calculate virtual coordinates and length of character string
!
        vx=zzxor+xcm0+xpos*zzunit*zzpagr
        vy=zzyor+ycm0+ypos*zzunit*zzpagr
        if(ip(1).eq.kzyes)then
          iplen=ip(2)/1000
          llen=ip(2)-iplen*1000
        else
          llen=40
        end if
 
        nword=(llen-1)/kzbyte+5
        xlen=xlnleg(ip,nlines)*zzunit*zzpagr
        ylen=ylnleg(ip,nlines)
        ylen=zzlgyl
        icur=2+nword
        ival=ip(icur+jave)
        ave=rval
        ival=ip(icur+nword+jsymsp)
        symsp=rval
!
!  plot title of legend
!
        if(zzlgtz.lt.0.0)then
          call gssetc(zzhite,0.0)
          vy=vy+ylen-zzhite
        else
          call gssetc(zzlgtz,0.0)
          vy=vy+ylen-zzlgtz
        end if
 
        rlen=zxmess(kzlgti,kzlgtl,czlgtc,czlgtf,czlgts)
        call dsmove(vx+(xlen-rlen)*0.5,vy)
        call ztext(kzlgti,kzlgtl,czlgtc,czlgtf,czlgts)
        if(zzlgtr.lt.-0.0)then
          vy=vy-ave*.25
        else
          vy=vy-ave*(max(0.0,zzlgtr-1.0))/2.0
        end if
!
!  for each line in array:
!  retrieve line space parameter, character
!  height, and character font, set to current
!  and draw to output device
!
        icur=2
!
!  need lines in legend block
!
        if(kzlgln.eq.kzyes)then
          iolsty=kzlsty
!
!  see if at least one symbol drawn
!
          lshift=.true.
 
          do i=1,nlines
            lshift=lshift.and.(kzlgsm(i).lt.0)
          end do
 
          if(lshift) symsp=symsp/2.0

          do i=1,nlines
            icur=icur+nword
            if(kzlgen(i).eq.kzyes.and.
     &          (kzlglt(i).ne.0.or.kzlgsm(i).ge.0))then
              ival=ip(icur+jyrat)
              yrat=rval
              if(yrat.lt.1.0) yrat=1.0
              ival=ip(icur+jhite)
              hite=rval
              delta=(yrat-1.0)*ave/2.0
              vy=vy-delta-hite
              if(kzlgsm(i).ge.0)then
                call dsmove(vx+0.375*symsp,vy+hite*0.5)
                call dsymbo(kzlgsm(i),zzsmsz)
              end if
!
!  draw thick line
!
              vy=vy-delta
              if(kzlglt(i).gt.10)then
                if(zzlgth(i).gt.delta)then
                  call crvwid(delta/zzpagr/zzunit)
                else
                  call crvwid(zzlgth(i)/zzpagr/zzunit)
                end if
                call dsltyp(kzlglt(i)-10)
                x(1)=vx
                x(2)=vx+xlen
                y(1)=vy
                y(2)=vy
                call zcurve(x,y,-2)
                call reset('CRVWID')
!
!  draw regular line
!
              elseif(kzlglt(i).ne.0)then
                call dsltyp(kzlglt(i))
                call dsmove(vx,vy)
                call dsdraw(vx+xlen,vy)
              end if
!
!  draw text
!
              do k=1,6
                cflag(k)=czlgac(i,k)
                cfont(k)=czlgaf(i,k)
              end do
 
              cstyle=czlgas(i)
              call gssetc(hite,0.0)
              call dsmove(vx+symsp,vy+delta)
              call ztext(ip(icur-nword+jtext),100,cflag,cfont,cstyle)
            end if
          end do
 
          call dsltyp(iolsty)
!
!  text in legend block only
!
        else
 
          do i=1,nlines
 
            icur=icur+nword
            if(kzlgen(i).eq.kzyes)then
              if(kzlgsm(i).ge.0)then
                ival=ip(icur+jyrat)
                yrat=rval
                if(yrat.lt.1.0) yrat=1.0
                ival=ip(icur+jhite)
                hite=rval
                delta=(yrat-1.0)*ave/2.0
                vy=vy-delta-hite
                call dsmove(vx+0.25*symsp,vy+hite*0.5)
                call dsymbo(kzlgsm(i),zzsmsz)
                call gssetc(hite,0.0)
                call dsmove(vx+symsp*0.5,vy)
 
                do k=1,6
                  cflag(k)=czlgac(i,k)
                  cfont(k)=czlgaf(i,k)
                end do
 
                cstyle=czlgas(i)
                call wch2in(ceqsgn,ieqsgn)
                call ztext(ieqsgn,1,cflag,cfont,cstyle)
                call dsmove(vx+symsp,vy)
                call ztext(ip(icur-nword+jtext),100,cflag,cfont,cstyle)
                vy=vy-delta
              end if
            end if
          end do
 
        end if
        call gssetc(ohite,oangle)
        if(wasthk) call crvwid(oldthk)
!
!  wrong level
!
      else
        call errmes('TXTBLK',2,3)
      end if
      return
      end
      subroutine vector(xfrom,yfrom,xto,yto,ivec)
!
!***********************************************************************
!
!! VECTOR draws a vector with or without an arrow head.
!
!  level 2-3
!
!  input:   xfrom,yfrom = first point of vector
!  xto,yto     = second point of vector
!  ivec(wxyz)  = arrow type and size flag
!
      save /clevel/
      save /cpage/
      save /cphysr/
      save /cunit/
      save /gcclip/
!
      common /clevel/ kzlevl,kzbegn
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cunit/  zzunit
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
!
      if(kzlevl.eq.2.or.kzlevl.eq.3)then
        vxfrom=xcm0+zzxor+xfrom*zzunit*zzpagr
        vyfrom=ycm0+zzyor+yfrom*zzunit*zzpagr
        vxto=xcm0+zzxor+xto*zzunit*zzpagr
        vyto=ycm0+zzyor+yto*zzunit*zzpagr
        call zvectr(vxfrom,vyfrom,vxto,vyto,ivec)
      else
        call errmes('VECTOR',3,0)
      end if
 
      return
      end
      subroutine vspace(yratio)
!
!***********************************************************************
!
!! VSPACE sets line space ratio for character packing.
!  (level 1-3)
!
!  input:   yratio = space ratio
!
      save /clevel/
      save /clgndn/
!
      common /clevel/ kzlevl,kzbegn
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        zzsrat=yratio
      else
        call errmes('VSPACE',1,3)
      end if
 
      return
      end
      subroutine wch2in(strng,iword)

!***********************************************************************
!
!! WCH2IN stores the contents of a string into an integer variable.
!
      integer numbyt
      parameter (numbyt=4)
!
      integer i
      integer iword
      integer khar
      character*1 strng(numbyt)
!
      do i=1,numbyt
        khar=ichar(strng(i))
        call sybyt4('N',iword,i,khar)
      end do
 
      return
      end
      subroutine win2ch(iword,strng)
!
!***********************************************************************
!
!! WIN2CH stores the contents of an integer word into a character variable.
!
      integer numbyt
      parameter (numbyt=4)
!
      integer i
      character*1 strng(numbyt)
!
      do i=1,numbyt
        call sybyt4('X',iword,i,khar)
        strng(i)=char(khar)
      end do
 
      return
      end
      subroutine xangle(ang)
!
!***********************************************************************
!
!! XANGLE sets the angle of x axis values.
!
!  level 1-3, p/s
!
!  input:   ang = text angle for axis labels
!
      save /caxis/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        temp=mod(ang,360.0)
        if(temp.lt.0.0) temp=360.0+temp
        if(temp.gt.90.0.and.temp.le.270.0)then
          zzxlba=abs(temp-180.0)
        else
          zzxlba=temp
        end if
      else
        call errmes('XANGLE',1,3)
      end if
      return
      end
      function xbtext(ip,nlines)
!
!***********************************************************************
!
!! XBTEXT returns the x length of a packed array in inches.
!
!  (level 1-3)
!
!  input:   ip     = packed character array
!  nlines = number of lines in ip
!
!  output:  xbtext = length in inches
!
      parameter (kzyes=111)
!
      save /cdevic/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cpage/
      save /cstrng/
      save /cunit/
!
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /clevel/ kzlevl,kzbegn
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5
!
      dimension ip(*)
      character*1 cflag(6)
      character*5 cfont(6),cstyle
      equivalence (rval,ival)
!
      save jtext,jhite
      data jtext,jhite /1,-2/
      if(kzlevl.ge.1.and.kzlevl.le.3)then
!
!  save old set up
!
        ohite=zzhite
        oangle=zzangl
        if(ip(1).eq.kzyes)then
          iplen=ip(2)/1000
          llen=ip(2)-iplen*1000
        else
          llen=40
        end if
        nword=(llen-1)/kzbyte+5
!
!  init maximum for line length
!
        icur=2+nword
 
        do i=1,6
          cflag(i)=czlgac(1,i)
          cfont(i)=czlgaf(1,i)
        end do
 
        cstyle=czlgas(1)
        ival=ip(icur+jhite)
        call gssetc(rval,0.0)
        rmax=zxmess(ip(3),100,cflag,cfont,cstyle)
!
!  find maximum line length
!
        do i=2,nlines
 
          icur=icur+nword
          ival=ip(icur+jhite)
          call gssetc(rval,0.0)
          do k=1,6
            cflag(k)=czlgac(i,k)
            cfont(k)=czlgaf(i,k)
          end do
          cstyle=czlgas(i)
          rlen=zxmess(ip(icur-nword+jtext),100,
     &                  cflag,cfont,cstyle)
          if(rlen.gt.rmax) rmax=rlen
 
        end do
!
!  assign 'Xbtext' as maximum line length in current unit and
!  restore old set up
!
        xbtext=rmax/zzunit/zzpagr
        zzhite=ohite
        zzangl=oangle
        call gssetc(ohite,oangle)
!
!  wrong level
!
      else
        xbtext=0.0
        call errmes('XBTEXT',1,3)
      end if
      return
      end
      function xcoord(xval)
!
!***********************************************************************
!
!! XCOORD returns the X position in inches.
!
!  (level 3)
!
!  input:   xval = x value in current coordinate system
!
      parameter (numbyt=4)
!
      character khar*(numbyt)
      real xval
!
      save /clevel/
      save /cpage/
      save /cphysr/
      save /cunit/
      save /gcclip/
      save /pltsiz/
!
      common /clevel/ kzlevl,kzbegn
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      if(kzlevl.eq.3)then

        rx=xval
        call win2ch(rx,khar)
        call capchr(khar)

        if(khar(1:4).eq.'LEFT')then
          xcoord=-zzxor
        elseif(khar(1:4).eq.'RIGH')then
          xcoord=xcm1-xcm0-zzxor
        else
          yt=1.0
          call scale(xval,yt,vx,vy)
          xcoord=vx-xcm0-zzxor
        end if

        xcoord=xcoord/zzunit/zzpagr
 
      else
        xcoord=0.0
        call errmes('XCOORD',3,0)
      end if
 
      return
      end
      real function xdimtb(text,line1,line2)
!
!***********************************************************************
!
!! XDIMTB calculates the length of a text block in inches.
!
!  xdimtb may be called at levels 1, 2 or 3.
!
!  xdimtb finds the length in inches of an imaginary rectangle
!  enclosing a block of text.  this routine may be used as an
!  alternative to xbtext, for handling data in character array
!  format.
!
!  the character size and font are assumed to be set outside the
!  routine and hence constant within the indicated lines of text.
!
!  xdimtb was introduced when it was realized that txtleg needs
!  such a function, both prior to calling txtleg, for positioning
!  the legend and within txtleg, for the optional box.
!
!  the size of the block is the length of the longest line in the
!  block.  the slngth utility, with its self counting option is
!  used for finding the length of a string.
!
!  text   input, character*(*) text(*), the text block, with
!  trailing '$' signs used to mark the end of strings.
!  elements line1:line2 are processed.
!
!  line1,
!  line2  input, integer line1, line2, the first and last lines
!  of text to be processed.
!
!  xdimtb output, real xdimtb, the length of the block in inches.
!
      integer i
      integer kzbegn
      integer kzlevl
      integer line1
      integer line2
      real slngth
      character*(*) text(*)
!
      save /clevel/
!
      common /clevel/ kzlevl, kzbegn
!
      external  slngth
!
      if(kzlevl.lt.1)then
        write(*,*)' '
        write(*,*)'XDIMTB - Fatal error!'
        write(*,*)'  XDIMTB must be called at level 1, 2 or 3.'
        write(*,*)'  However, the current level is ',kzlevl
        stop
      else
 
        xdimtb=0.0
 
        do i=line1,line2
          xdimtb = max(xdimtb,slngth(text(i),100))
        end do
 
      end if
 
      return
      end
      real function xinvrs(xinch)
!
!***********************************************************************
!
!! XINVRS converts the location of a point from inches to data units. 
!  (level 3)
!
!  purpose:
!  xinvrs may be used (along with its analog yinvrs) to convert the
!  location of a point given in inches from the physical origin to a
!  value in units of the current coordinate system.
!
!  arguments:
!  name    dimension   type  i/o/s   description
!  xinch       -        r      i     x position from the physical origin
!
!  method:
!  the routine makes the conversion by the use of parameters passed via
!  internal common blocks.  this requires that the plot parameters be
!  defined before this routine is called.
!
!  conversions are made for both linear and log axis systems.
!
!  notes:
!  1) a polar system conversion will be developed when appropriate.
!  2) two arguments are needed to handle the general case of a rotated
!  coordinate system.
!
!  author:
!  michael d. wong, sterling software, nasa ames research center
!
!-------------------------------------------------------------------------------
!
      real xinch
      integer kzpage, kzauto, kzor, kzlevl, kzbegn
      logical logx, logy
      real
     &   udx, udy, ux0, uy0, uupagx, uupagy, uuxor, uuyor, xcm0,
     &   xcm1, xvstrt, xvlen, ycm0, ycm1, yvlen, yvstrt, zzpagx,
     &   zzpagy, zzpagr, zzunit, zzxor, zzyor
!
      save /clevel/
      save /cpage/
      save /cphysr/
      save /cunit/
      save /gcclip/
      save /pltcom/
      save /pltsiz/
!
      common /clevel/ kzlevl, kzbegn
      common /cpage/  zzpagx, zzpagy, zzpagr, uupagx, uupagy,
     &                kzpage, kzauto
      common /cphysr/ zzxor, zzyor, kzor, uuxor, uuyor
      common /cunit/  zzunit
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      if(kzlevl .ne. 3) go to 900
 
      xinvrs = (((zzxor + xcm0 + (xinch * zzunit * zzpagr)) - xvstrt)
     &         * udx / xvlen) + ux0
 
      if(logx) xinvrs = 10 ** (xinvrs)
 
      return
 
  900 call errmes ('XINVRS', 3, 3)
 
      return
      end
      subroutine xlabel(lxname,ixname)
!
!***********************************************************************
!
!! XLABEL defines the X axis label.
!
!  level 2 or 3, p/s
!
!  input:   lxname = character string
!  ixname = number of characters in lxname
!
      parameter (kzyes=111)
!
      save /clabel/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
!
      dimension lxname(2)
!
      if((kzlevl.eq.2).or.(kzlevl.eq.3))then
        kzxnfl=kzyes
        if(ixname.eq.0)then
          kzxaln=0
        else
          kzxaln=80
          call zcopys(lxname,ixname,kzxlab,kzxaln)
          if(kzxaln.lt.0)then
            kzxaln=-kzxaln
            call errmes('XLABEL',80,4)
          end if
        end if
      else
        call errmes('XLABEL',2,3)
      end if

      return
      end
      function xlnleg(ip,nlines)
!
!***********************************************************************
!
!! XLNLEG returns the X length of a packed array in inches.
!
!  (level 1-3)
!
!  input:   ip     = packed character array
!  nlines = number of lines in ip
!
!  output:  xlnleg = length in inches
!
      parameter (kzyes=111)
!
      save /cdevic/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cmxalf/
      save /cpage/
      save /cstrng/
      save /cunit/
!
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /clevel/ kzlevl,kzbegn
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /cunit/  zzunit
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      character czalfl*1,czalfn*5,czalfs*5
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5
!
      dimension ip(*)
      character*1 cflag(6),cm4*4
      character*5 cfont(6),cstyle
      equivalence (rval,ival)
!
      save jtext,jhite,cm4
      data jtext,jhite /1,-2/
      data cm4 /'MMMM'/
      if(kzlevl.ge.1.and.kzlevl.le.3)then
!
!  save old set up
!
        ohite=zzhite
        oangle=zzangl
        if(ip(1).eq.kzyes)then
          iplen=ip(2)/1000
          llen=ip(2)-iplen*1000
        else
          llen=40
        end if
        nword=(llen-1)/kzbyte+5
!
!  init maximum for line length
!
        icur=2
        smax=0.0
        rmax=0.0
!
!  calculate legend title length
!
        if(zzlgtz.lt.0.0)then
          call gssetc(zzhite,0.0)
        else
          call gssetc(zzlgtz,0.0)
        end if
        rtitle=zxmess(kzlgti,kzlgtl,czalfl,czalfn,czalfs)
!
!  find maximum line length for all activated entries in packed array
!
        do i=1,nlines
          icur=icur+nword
          if(kzlgen(i).eq.kzyes)then
            ival=ip(icur+jhite)
            call gssetc(rval,0.0)
            do k=1,6
              cflag(k)=czlgac(i,k)
              cfont(k)=czlgaf(i,k)
            end do
 
            cstyle=czlgas(i)
            call wch2in(cm4,m4)
            slen=zxmess(m4,4,cflag,cfont,cstyle)
            rlen=zxmess(ip(icur-nword+jtext),100,
     &                  cflag,cfont,cstyle)
            if(rlen.gt.rmax) rmax=rlen
            if(slen.gt.smax) smax=slen
          end if
        end do
!
!  assign xlnleg as maximum line length in current unit and
!  restore old set up
!
        if(smax+rmax.gt.rtitle)then
          xlnleg=(smax+rmax)/zzunit/zzpagr
        else
          xlnleg=rtitle/zzunit/zzpagr
        end if
        rval=smax
        ip(2+2*nword)=ival
        zzhite=ohite
        zzangl=oangle
        call gssetc(ohite,oangle)
!
!  wrong level
!
      else
        call errmes('XLNLEG',1,3)
      end if
      return
      end
      subroutine xlog(xorign,xcycle,yorign,ystep)
!
!***********************************************************************
!
!! XLOG sets up a coordinate system for log x and linear y.
!
!  (level 2, raise to level 3)
!
!  input:   xorign = lower limit of x axis
!  xcycle = cycle length per inches
!  yorign = lower limit of y axis
!  ystep  = step size per inch for yaxis
!
      parameter (kzlog=3)
!
      save /carea/
      save /caxis/
      save /ccoord/
      save /clabel/
      save /clevel/
      save /cpage/
      save /cunit/
      save /pltcom/
!
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
      common /ccoord/ zzxmin,zzxmax,zzxstp,zzymin,zzymax,zzystp
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /clevel/ kzlevl,kzbegn
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /cunit/  zzunit
      logical logx, logy
      if(kzlevl.eq.2)then
!
!  init iaxes to define linear y axis and draw title
!
        logx=.false.
        logy=.false.
        iaxes=19
        xmin=xorign
        ymin=yorign
        xmax=xmin*10.0**(xcycle*zzxaxs/zzunit/zzpagr)
        ymax=ymin+ystep*zzyaxs/zzunit/zzpagr
        zzxstp=(xmax-xmin)/2.0
        zzystp=ystep
        call zmapit(xmin,xmax,ymin,ymax,0,0,
     &              kzylab,kzyaln,0.0,0.0,iaxes)
!
!  define log x axis
!
        dummy=0.0
        kzxtyp=kzlog
        lflag=1
        xpos=0.0
        ypos=0.0
        call scalog(xmin,xmax,zzxaxs,xor,xcyc)
 
        call zdrlog(xor,xcyc,dummy,dummy,zzxaxs,dummy,xpos,ypos,lflag)
 
        kzlevl=3
      else
        call errmes('XLOG',2,0)
      end if
      return
      end
      subroutine xmarks(iticks)
!
!***********************************************************************
!
!! XMARKS sets the number of ticks on the X axis.
!
!  level 1-3, p/s
!
!  input:   iticks = number of ticks per step
!
      save /caxis/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzxtck=iticks
      else
        call errmes('XMARKS',1,3)
      end if
      return
      end
      subroutine xtrlgx(xor,xcyc,xaxis,labx,lxlab,xpos,ypos)
!
!***********************************************************************
!
!! XTRLGX sets up a log X axis.
!
!  (level 3)
!
!  input:   xor       = lower limit of x axis
!  xcyc      = cycle length in inches
!  xaxis     = x axis length in inches
!  labx      = x axis label
!  lxlab     = number of characters in labx
!  xpos,ypos = location of subplot from lower
!  left corner of page
!
      parameter (kzlog=3)
!
      save /carea/
      save /caxis/
      save /clabel/
      save /clevel/
      save /cpage/
      save /cphysr/
      save /cunit/
      save /gcclip/
      save /pltcom/
      save /pltprm/
      save /pltsiz/
!
      common /clevel/ kzlevl,kzbegn
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cunit/  zzunit
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      common /pltprm/ cxsize, cysize, tickln, yvini
      logical logx, logy
!
      dimension labx(*)
!
      if(kzlevl.eq.3)then
        logx=.false.
        dummy=0.0
        kzxtyp=kzlog
        lflag=1
        if(lxlab.eq.0)then
          kzxaln=0
        else
          kzxaln=80
          call zcopys(labx,lxlab,kzxlab,kzxaln)
          if(kzxaln.lt.0)then
            kzxaln=-kzxaln
            call errmes('XTRLGX',80,4)
          end if
        end if
        zzxaxs=xaxis*zzunit*zzpagr
        zzxlft=xcm0+zzxor
        zzxaxr=(xcm1-zzxlft)/zzxaxs
        if(zzxaxr.gt.1.0)then
          zzxaxr=1.0
          zzxrgt=zzxlft+zzxaxs
        else
          zzxrgt=xcm1
        end if
!  xvstrt=zzxlft
        xvlen=zzxrgt-zzxlft
        call zdrlog(xor,xcyc,dummy,dummy,xaxis,dummy,xpos,ypos,lflag)
      else
        call errmes('XTRLGX',3,0)
      end if
 
      return
      end
      subroutine xtrlgy(yor,ycyc,yaxis,laby,lylab,xpos,ypos)
!
!***********************************************************************
!
!! XTRLGY sets up a log Y axis.
!
!  (level 3)
!
!  input:   yor       = lower limit of y axis
!  ycyc      = cycle length in inches
!  yaxis     = y axis length in inches
!  laby      = y axis label
!  lylab     = number of characters in laby
!  xpos,ypos = location of subplot from lower
!  left corner of page
!
      parameter (kzlog=3)
!
      save /carea/
      save /caxis/
      save /clabel/
      save /clevel/
      save /cpage/
      save /cphysr/
      save /cunit/
      save /gcclip/
      save /pltcom/
      save /pltprm/
      save /pltsiz/
!
      common /clevel/ kzlevl,kzbegn
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cunit/  zzunit
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      common /pltprm/ cxsize, cysize, tickln, yvini
      logical logx, logy
!
      dimension laby(*)
      if(kzlevl.eq.3)then
        logy=.false.
        dummy=0.0
        kzytyp=kzlog
        iaxes=2
        if(lylab.eq.0)then
          kzyaln=0
        else
          kzyaln=80
          call zcopys(laby,lylab,kzylab,kzyaln)
          if(kzyaln.lt.0)then
            kzyaln=-kzyaln
            call errmes('XTRLGY',80,4)
          end if
        end if
        zzyaxs=yaxis*zzunit*zzpagr
        zzybot=ycm0+zzyor
        zzyaxr=(ycm1-zzybot)/zzyaxs
        if(zzyaxr.gt.1.0)then
          zzyaxr=1.0
          zzytop=zzybot+zzyaxs
        else
          zzytop=ycm1
        end if
!  yvstrt=zzybot
        yvini=zzytop-zzybot
        call zdrlog(dummy,dummy,yor,ycyc,dummy,yaxis,xpos,ypos,iaxes)
      else
        call errmes('XTRLGY',3,0)
      end if
 
      return
      end
      subroutine xtrlnx(xmin,xstp,xmax,xaxis,lxname,ixname,
     &                    xpos,ypos)
!
!***********************************************************************
!
!! XTRLNX sets up a linear X coordinate system.
!
!  level 3
!
!  input:   xmin,xmax     = range of x axis
!  xstp          = step size, if = 'SCAL',
!  a step size is assigned
!  xaxis         = length of axis in inches
!  lxname,ixname = axis label and length
!  xpos,ypos     = offset from origin
!
      save /carea/
      save /ccoord/
      save /clabel/
      save /clevel/
      save /cpage/
      save /cphysr/
      save /cunit/
      save /gcclip/
      save /pltsiz/
!
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /clevel/ kzlevl,kzbegn
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cunit/  zzunit
      common /ccoord/ zzxmin,zzxmax,zzxstp,zzymin,zzymax,zzystp
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      integer lxname(*)
      character cxstr*4
!
!  equivalence (cxstr,xstep)
!
!  check level
!
      if(kzlevl.eq.3)then
!
!  init iaxes to define x and y axis and draw title
!
        iaxes=1
        xstep=xstp
!
        call win2ch(xstep,cxstr)
!
        call capchr(cxstr)

        if(cxstr.eq.'SCAL')then
          iaxes=iaxes+4
        else
          zzxstp=xstp
        end if

        zzxaxs=xaxis*zzunit*zzpagr
        zzxlft=xcm0+zzxor
        zzxaxr=(xcm1-zzxlft)/zzxaxs

        if(zzxaxr.gt.1.0)then
          zzxaxr=1.0
          zzxrgt=zzxlft+zzxaxs
        else
          zzxrgt=xcm1
        end if

        xvstrt=zzxlft
        xvlen=zzxrgt-zzxlft
        kzxaln=80
        call zcopys(lxname,ixname,kzxlab,kzxaln)

        if(kzxaln.lt.0)then
          kzxaln=-kzxaln
          call errmes('XTRLNX',80,4)
        end if

        call zmapit(xmin,xmax,dum,dum,kzxlab,kzxaln,
     &              kzylab,kzyaln,xpos,ypos,iaxes)
      else
        call errmes('XTRLNX',3,0)
      end if

      return
      end
      subroutine xtrlny(ymin,ystp,ymax,yaxis,lyname,iyname,
     &                    xpos,ypos)
!
!***********************************************************************
!
!! XTRLNY sets up a linear Y coordinate system.
!
!  (level 3)
!
!  input:   ymin,ymax     = range of y axis
!  ystp          = step size, if = 'SCAL',
!  a step size is assigned
!  yaxis         = length of axis in inches
!  lyname,iyname = axis label and length
!  xpos,ypos     = offset from origin
!
      parameter (numbyt=4)
!
      integer lyname(*)
      character*(numbyt) cystr
!
      save /carea/
      save /ccoord/
      save /clabel/
      save /clevel/
      save /cpage/
      save /cphysr/
      save /cunit/
      save /gcclip/
      save /pltsiz/
!
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /ccoord/ zzxmin,zzxmax,zzxstp,zzymin,zzymax,zzystp
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /clevel/ kzlevl,kzbegn
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cunit/  zzunit
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      common /pltprm/ cxsize, cysize, tickln, yvini
!
!  check level
!
      if(kzlevl.eq.3)then
!
!  init iaxes to define x and y axis
!  and draw title
!
        iaxes=2
        iystr=int(ystp)
        call win2ch(iystr,cystr)
        call capchr(cystr)

        if(cystr(1:4).eq.'SCAL')then
          iaxes=iaxes+8
        else
          zzystp=ystp
        end if

        zzyaxs=yaxis*zzunit*zzpagr
        zzybot=ycm0+zzyor
        zzyaxr=(ycm1-zzybot)/zzyaxs

        if(zzyaxr.gt.1.0)then
          zzyaxr=1.0
          zzytop=zzybot+zzyaxs
        else
          zzytop=ycm1
        end if

        yvstrt=zzybot
        yvini=zzytop-zzybot
        kzyaln=80
        call zcopys(lyname,iyname,kzylab,kzyaln)

        if(kzyaln.lt.0)then
          kzyaln=-kzyaln
          call errmes('XTRLNY',80,4)
        end if

        call zmapit(dum,dum,ymin,ymax,kzxlab,kzxaln,
     &              kzylab,kzyaln,xpos,ypos,iaxes)
      else
        call errmes('XTRLNY',3,0)
      end if

      return
      end
      subroutine xyprm(x,y,zeta,iline)
!
!***********************************************************************
!
!! XYPRM...
!
      save /comdp/
      save /dbase/
!
      common/comdp/xmin,xmax,ymin,ymax,zmin,zmax,axisr(2),plotx,
     &   ploty,pltorg(2),camxyz(3),mx,ny,fmx,fny,camwkg(6),xorg(3),
     &   gx(3),fx(2),kscale,zorg,center(2),pqlmt,
     &   amtx(3,3),focall
      dimension limit(2),flim(2)
      equivalence(u,camxyz(1)),(v,camxyz(2)),(w,camxyz(3)),
     &   (mx,limit(1)),(fmx,flim(1))
!
      common /dbase/vx,vy,voldx,voldy,cxsize,cysize
      dimension xs(3),xc(3)
!
      xs(1)=xmin+(x-1.0)*gx(1)-camwkg(1)
      xs(2)=ymin+(y-1.0)*gx(2)-camwkg(2)
      xs(3)=zorg + zeta*gx(3)-camwkg(3)
      call rotate(xs,amtx,xc)
      vx=(xc(1)/xc(3)-xorg(1))*focall+pltorg(1)+center(1)
      vy=(xc(2)/xc(3)-xorg(2))*focall+pltorg(2)+center(2)
      if(iline) 30, 20, 10
10    call gsdraw(vx,vy)
      go to 30
20    call gsmove(vx,vy)
30    return
      end
      subroutine yangle(ang)
!
!***********************************************************************
!
!! YANGLE sets the angle of Y axis values.
!
!  level 1-3, p/s
!
!  input:   ang = text angle for axis label
!
      save /caxis/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        temp=mod(ang,360.0)
        if(temp.lt.0.0) temp=360.0+temp
        if(temp.gt.90.0.and.temp.le.270.0)then
          zzylba=abs(temp-180.0)
        else
          zzylba=temp
        end if
      else
        call errmes('YANGLE',1,3)
      end if
      return
      end
      function ybtext(ip,nlines)
!
!***********************************************************************
!
!! YBTEXT returns the Y length of a packed array in inches.
!
!  (level 1-3)
!
!  input:   ip     = packed character array
!  nlines = number of lines in ip
!
!  output:  ybtext = length in inches
!
      parameter (kzyes=111)
!
      save /cdevic/
      save /clevel/
      save /cpage/
      save /cunit/
!
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /clevel/ kzlevl,kzbegn
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cunit/  zzunit
!
      dimension ip(*)
      equivalence (ival,rval)
!
      save jhite,jyrat
      data jhite,jyrat /-2,-1/
      if(kzlevl.ge.1.or.kzlevl.le.3)then
!
!  retrieve array info from ip
!
        if(ip(1).eq.kzyes)then
          iplen=ip(2)/1000
          llen=ip(2)-iplen*1000
        else
          llen=40
        end if
        nword=(llen-1)/kzbyte+5
!
!  find inter-line space for 1-st and last line
!
        icur=2+nword
        ival=ip(icur+jyrat)
        yrat1=rval
        if(yrat1.lt.1.0) yrat1=1.0
        ival=ip(icur+(nlines-1)*nword+jyrat)
        yratn=rval
        if(yratn.lt.1.0) yratn=1.0
!
!  loop to sum char height and inter-line space factor
!
        icur=2
        sum=0.0
        ratsum=0.0
        do i=1,nlines
          icur=icur+nword
          ival=ip(icur+jhite)
          sum=sum+rval
          ival=ip(icur+jyrat)
          yrat=rval
          if(yrat.lt.1.0) yrat=1.0
          ratsum=ratsum+yrat
        end do
 
        ave=sum/nlines
        ybtext=(ratsum-(yrat1+yratn-2.0)/2.0)*ave/zzunit/zzpagr
        rval=ave
        ip(nword+2)=ival
!
!  wrong level
!
      else
        ybtext=0.0
        call errmes('YBTEXT',2,3)
      end if
      return
      end
      function ycoord(yval)
!
!***********************************************************************
!
!! YCOORD returns the Y position in inches.
!
!  (level 3)
!
!  input:   yval = y value in current coordinate system
!
!  output:  ycoord = length in inches from physical
!  origin
!
      parameter (numbyt=4)
!
      character khar*(numbyt)
      real yval
!
      save /clevel/
      save /cpage/
      save /cphysr/
      save /cunit/
      save /gcclip/
      save /pltsiz/
!
      common /clevel/ kzlevl,kzbegn
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cunit/  zzunit
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      if(kzlevl.eq.3)then
        ry=yval
        call win2ch(ry,khar)
        call capchr(khar)

        if(khar(1:4).eq.'LOWE')then
          ycoord=-zzyor
        elseif(khar(1:4).eq.'UPPE')then
          ycoord=ycm1-ycm0-zzyor
        else
          xt=1.0
          call scale(xt,yval,vx,vy)
          ycoord=vy-ycm0-zzyor
        end if

        ycoord=ycoord/zzunit/zzpagr

      else
        ycoord=0.0
        call errmes('YCOORD',3,0)
      end if

      return
      end
      real function yinvrs(yinch)
!
!***********************************************************************
!
!! YINVRS converts the location of a point from inches to data units. 
!
!  (level 3)
!
!  purpose:
!  yinvrs may be used (along with its analog xinvrs) to convert the
!  location of a point given in inches from the physical origin to a
!  value in units of the current coordinate system.
!
!  arguments:
!  name    dimension   type  i/o/s   description
!  yinch       -        r      i     y position from the physical origin
!
!  method:
!  the routine makes the conversion by the use of parameters passed via
!  internal common blocks.  this requires that the plot parameters be
!  defined before this routine is called.
!
!  conversions are made for both linear and log axis systems.
!
!  notes:
!  1) a polar system conversion will be developed when appropriate.
!  2) two arguments are needed to handle the general case of a rotated
!  coordinate system.
!
!  author:
!  michael d. wong, sterling software, nasa ames research center
!
!-------------------------------------------------------------------------------
!
!  arguments
!  ---------
!
      real yinch
!
      integer
     &   kzpage, kzauto, kzor, kzlevl, kzbegn
      logical
     &   logx, logy
      real
     &   udx, udy, ux0, uy0, uupagx, uupagy, uuxor, uuyor, xcm0,
     &   xcm1, xvstrt, xvlen, ycm0, ycm1, yvlen, yvstrt, zzpagx,
     &   zzpagy, zzpagr, zzunit, zzxor, zzyor
!
      save /clevel/
      save /cphysr/
      save /cpage/
      save /cunit/
      save /gcclip/
      save /pltcom/
      save /pltsiz/
!
      common /clevel/ kzlevl, kzbegn
      common /cpage/  zzpagx, zzpagy, zzpagr, uupagx, uupagy,
     &                kzpage, kzauto
      common /cphysr/ zzxor, zzyor, kzor, uuxor, uuyor
      common /cunit/  zzunit
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
      if(kzlevl .ne. 3) then

        call errmes ('YINVRS', 3, 3)

      else
 
        yinvrs = (((zzyor + ycm0+(yinch * zzunit * zzpagr)) - yvstrt)
     &         * udy / yvlen) + uy0
 
        if(logy) yinvrs = 10 ** (yinvrs)

      end if
 
      return
      end
      subroutine ylabel(lyname,iyname)
!
!***********************************************************************
!
!! YLABEL sets the Y axis label.
!
!  level 2 or 3, p/s
!  callable from level 2 or 3
!
!  input:   lyname = y-axis label
!  iyname = number of characters in lyname
!
      parameter (kzyes=111)
!
      save /clabel/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      dimension lyname(2)
!
      if((kzlevl.eq.2).or.(kzlevl.eq.3))then
        kzynfl=kzyes
        if(iyname.eq.0)then
          kzyaln=0
        else
          kzyaln=80
          call zcopys(lyname,iyname,kzylab,kzyaln)
          if(kzyaln.lt.0)then
            kzyaln=-kzyaln
            call errmes('YLABEL',80,4)
          end if
        end if
      else
!  callable from level 2 or 3
!  call errmes('YLABEL',2,0)
        call errmes('YLABEL',2,3)
      end if
      return
      end
      function ylnleg(ip,nlines)
!
!***********************************************************************
!
!! YLNLEG returns the Y length of a packed array in inches.
!  (level 1-3)
!
!  input:   ip     = packed character array
!  nlines = number of lines in ip
!
!  output:  ylnleg = length in inches
!
      parameter (kzyes=111)
      parameter (kzno=222)
!
      save /cdevic/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cpage/
      save /cstrng/
      save /cunit/
!
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /clevel/ kzlevl,kzbegn
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /cunit/  zzunit
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5
!
      dimension ip(*)
      equivalence (ival,rval)
!
      save jhite,jyrat
      data jhite,jyrat /-2,-1/
      if(kzlevl.ge.1.and.kzlevl.le.3)then
!
!  retrieve array info from ip
!
        if(ip(1).eq.kzyes)then
          iplen=ip(2)/1000
          llen=ip(2)-iplen*1000
        else
          llen=40
        end if
        nword=(llen-1)/kzbyte+5
!
!  find inter-line space for 1-st and last line
!
        if(zzlgtr.lt.1.0) zzlgtr=1.0
!
!  loop to sum char height and inter-line space factor
!
        icur=2
        kzlgbl=0
        blksum=0.0
        if(zzlgtz.lt.0.0)then
          hitsum=zzhite
        else
          hitsum=zzlgtz
        end if
        if(zzlgtr.lt.0.0)then
          ratsum=1.5
        else
          ratsum=zzlgtr
        end if
        do i=1,nlines
          icur=icur+nword
          ival=ip(icur+jhite)
          hitsum=hitsum+rval
          ival=ip(icur+jyrat)
          yrat=rval
          if(yrat.lt.1.0) yrat=1.0
!
!  ignore if current legend line has
!  1. no point connected, no symbol, savlin on
!  or 2. no symbol, savlin off
!  or 3. no text entered
!
          if((kzlglt(i).eq.0.and.kzlgsm(i).lt.0.and.
     &         kzlgln.eq.kzyes).or.
     &        (kzlgsm(i).lt.0.and.kzlgln.eq.kzno).or.
     &        (kzlgen(i).eq.kzno))then
            blksum=blksum+yrat
!w            kzlgbl=kzlgbl+1
          end if
          ratsum=ratsum+yrat
        end do
 
        ave=hitsum/(nlines+1)
!w        ylnleg=(ratsum-(zzlgtr-1.0)/2.0)*ave/zzunit/zzpagr
!w        zzlgyl=(ratsum-(zzlgtr-1.0)/2.0-blksum)*ave
        ylnleg=ratsum*ave/zzunit/zzpagr
        zzlgyl=(ratsum-blksum)*ave
!
!  save average size for call from routine 'txtblk'
!
        rval=ave
        ip(nword+2)=ival
!
!  wrong level
!
      else
        ylnleg=0.0
        call errmes('YLNLEG',2,3)
      end if
      return
      end
      subroutine ylog(xorign,xstep,yorign,ycycle)
!
!***********************************************************************
!
!! YLOG sets up the coordinate system for linear x and log y.
!
!  (level 2, raise to level 3)
!
!  input:   xorign = lower limit of x axis
!  xstep  = step size per inch for xaxis
!  yorign = lower limit of y axis
!  ycycle = cycle length per inches
!
      parameter (kzlog=3)
!
      save /carea/
      save /caxis/
      save /ccoord/
      save /clabel/
      save /clevel/
      save /cpage/
      save /cunit/
      save /pltcom/
!
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
      common /ccoord/ zzxmin,zzxmax,zzxstp,zzymin,zzymax,zzystp
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /clevel/ kzlevl,kzbegn
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /cunit/  zzunit
      logical logx, logy
!
      if(kzlevl.eq.2)then
!
!  init iaxes to define linear x axis and draw title
!
        logx=.false.
        logy=.false.
        iaxes=19
        xmin=xorign
        ymin=yorign
        ymax=ymin*10.0**(ycycle*zzyaxs/zzunit/zzpagr)
        xmax=xmin+xstep*zzxaxs/zzunit/zzpagr
        zzystp=(ymax-ymin)/2.0
        zzxstp=xstep
        call zmapit(xmin,xmax,ymin,ymax,kzxlab,kzxaln,
     &              0,0,0.0,0.0,iaxes)
!
!  define log y axis
!
        dummy=0.0
        kzytyp=kzlog
        lflag=2
        xpos=0.0
        ypos=0.0
        call scalog(ymin,ymax,zzyaxs,yor,ycyc)
        call zdrlog(dummy,dummy,yor,ycyc,dummy,zzyaxs,xpos,ypos,lflag)
        kzlevl=3
      else
        call errmes('YLOG',2,0)
      end if
 
      return
      end
      subroutine ymarks(iticks)
!
!***********************************************************************
!
!! YMARKS sets the number of ticks on the Y axis.
!
!  level 1-3, p/s
!
!  input:   iticks = number of ticks per step
!  if = 0, no tick drawn
!
      save /caxis/
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
!
      if(kzlevl.ge.1.and.kzlevl.le.3)then
        kzytck=iticks
      else
        call errmes('YMARKS',1,3)
      end if
      return
      end
      subroutine zaxis(blow,bhigh,step,btmin,btmax,btick,ipwr)
!
!***********************************************************************
!
!! ZAXIS determines a suitable tick distance over a range.
!
!  The range is specified as between
!  alow and ahigh.   it outputs the axis range bmin,bmax
!  and the tick distance btick stripped of their power of
!  ten.   the power of ten is returned in the var. ipwr.
!
      logical lisneg
!
      if(bhigh .ge. blow)then
        bmax = bhigh
        bmin = blow
        lisneg = .false.
      else
        bmax = blow
        bmin = bhigh
        lisneg = .true.
      end if
      temp = max (abs(bmin),abs(bmax))
      ipwr=int(alog10(temp))
!
!  if(ipwr.gt.1) ipwr=ipwr-1
!
      tenexp=10.0**ipwr
      btick=step/tenexp
      btmin = bmin/tenexp
      btmax = bmax/tenexp
!
!  switch back to backwards
!
      if (lisneg) then
        call zzswap(btmin,btmax)
      end if
 
      return
      end
      subroutine zblank(xpos1,xpos2,ypos1,ypos2,id,iframe)
!
!***********************************************************************
!
!! ZBLANK sets a blank area.
!
!  input:   xpos1 xpos2 = x limits of blank area
!  ypos1 ypos2 = y limits of blnak area
!  id          = area id, max = 4
!  iframe      = thickness of frame
!
      parameter (kzmxbs=1000)
      parameter (kzmxbl=200)
!
      save /cblank/
      save /cpage/
      save /cphysr/
      save /cunit/
      save /gcclip/
!
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cunit/  zzunit
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cblank/ zzblnk(kzmxbl,4),zzblks(kzmxbs,4),kzblcn,kzbscn
!
      r=zzunit*zzpagr
      if(iframe.lt.0)then
        jframe=-iframe
        delta=-0.01*r
      else
        jframe=iframe
        delta=0.01*r
      end if
      numfrm=jframe
!
!  force min and max for iframe
!
      if(jframe.gt.4.and.id.lt.5)then
        numfrm=4
        call errmes('BLNK',id,5)
      end if
!
!  calculate virtual coordinates
!
      x1=xpos1*r+xcm0+zzxor
      x2=xpos2*r+xcm0+zzxor
      y1=ypos1*r+ycm0+zzyor
      y2=ypos2*r+ycm0+zzyor
      if(xpos1.gt.xpos2)then
        zzblnk(id,1)=x1
        zzblnk(id,2)=x2
      else
        zzblnk(id,1)=x2
        zzblnk(id,2)=x1
      end if
      if(ypos1.gt.ypos2)then
        zzblnk(id,3)=y1
        zzblnk(id,4)=y2
      else
        zzblnk(id,3)=y2
        zzblnk(id,4)=y1
      end if
!
!  draw frames
!
      if(numfrm.ne.0)then
        do i=1,numfrm
          del=-delta*(i-1)
          call zframe(zzblnk(id,2),zzblnk(id,1),zzblnk(id,4),
     &             zzblnk(id,3),del)
        end do
 
      end if
      return
      end
      subroutine zbyte4(cflag,inp,ipos,khar)
!
!***********************************************************************
!
!! ZBYTE4 extracts a character from a 4 byte integer, or inserts one.
!
!  note:    1. this routine is tested on
!  vax/vms, and ibm/pc. kzmask and kzcpos are
!  in common block 'cbyte4', initialized in
!  'zinit'.
!  2. kzmask is the mask to zero out the byte to
!  which a character is to be inserted. for
!  example, when an integer is 'and'ed with a
!  mask of 7f7f7f00 hex, the least significant
!  byte is zeroed.
!  3. kznpos is position of the byte of interest.
!  for extraction, kznpos is the bits to shift
!  right to place the desired byte in the least
!  significant position. for insertion, kznpos
!  is the bits to shift left to place the byte
!  to be inserted in the right position.
!
      save /cbyte4/
      save /cdevic/
!
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /cbyte4/ kzmask(8),kzcpos(8)
      character cflag
      integer inp(*)
!
!  calculate the exact location
!
      if(ipos.gt.kzbyte)then
        itemp=(ipos-1)/kzbyte
        iptr=itemp+1
        ip=ipos-itemp*kzbyte
      else
        iptr=1
        ip=ipos
      end if
!
!  to extract a character
!
      if(cflag.eq.'X')then
        khar=ksyand(inp(iptr)/kzcpos(ip),127)
!
!  to insert a character
!
      else
        inp(iptr)=ksyand(inp(iptr),kzmask(ip))+khar*kzcpos(ip)
      end if
      return
      end
      subroutine zch2in(cstr,lstr)
!
!***********************************************************************
!
!! ZCH2IN converts a character array to an integer array.
!
!  input:   cstr      character array
!
!  output:  lstr      integer array
!
      integer lstr(*)
      character*1 cstr(1)
      length=nchray(cstr)
 
      do i=1,length+1
        khar=ichar(cstr(i))
        call sybyt4('N',lstr,i,khar)
      end do
 
      return
      end
      subroutine zcopys(lfrom,nfrom,lto,nto)
!
!***********************************************************************
!
!! ZCOPYS copies a string.
!
!  input:   lfrom = original string
!  nfrom = number of characters in lfrom
!
!  output:  lto   = destination string
!  nto   = number of characters in nto; if return
!  negative value, string truncated
!
      parameter (numbyt=4)
      parameter (maxchr=160)
      parameter (maxint=maxchr/numbyt)
!
      integer lfrom(*),lto(*),kfrom(maxint)
      character sfrom*160
      character czalfl*1,czalfn*5,czalfs*5
!
      save /cmxalf/
!
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
!
      save kfrom
!
      data kfrom /maxint*0/
!
!  for self terminating string
!
      if(abs(nfrom).eq.100)then
        nchar=nzchar(lfrom,czalfl)
        if(nchar.lt.0) nchar=nto
        if(nchar.ge.nto)then

          num=(nto+(numbyt-1))/numbyt
 
          do i=1,num
            lto(i)=lfrom(i)
          end do
 
          call sybyt4('N',lto(nto),4,0)
          nto=-(nto-1)
        else
!
!  num=(nchar+3)/4
!
          num=(nchar+(numbyt-1))/numbyt
          do i=1,num
            lto(i)=lfrom(i)
          end do
          nto=100
        end if
!
!  all others
!
      else
        if(nfrom.ge.nto)then
!
!  num=(nto+3)/4
!
          num=(nto+(numbyt-1))/numbyt
          loc=nto
          nto=-nto
        else
!
!  num=(nfrom+3)/4
!
          num=(nfrom+(numbyt-1))/numbyt
          nto=nfrom
          loc=nfrom+1
        end if

        k=1
 
        do i=1,num
          call win2ch(kfrom(i),sfrom(k:k+numbyt-1))
          k=k+numbyt
        end do
 
        sfrom(loc:loc)=char(0)
        k=1
 
        do i=1,num
          call wch2in(sfrom(k:k+numbyt-1),lto(i))
          k=k+numbyt
        end do
 
      end if
      return
      end
      subroutine zcostr(lfrom,nf,lto,nt)
!
!***********************************************************************
!
!! ZCOSTR copies a string, and calculates its length if not provided.
!
!  input:   lfrom = input string
!  nf    = number of characters in lfrom
!
!  output:  lto = output string
!  nt  = number of characters in lto
!
      parameter (maxchr=160)
      parameter (numbyt=4)
      parameter (maxint=maxchr/numbyt)
!
      integer lfrom(*),lto(*)
      character sfrom*160,s1*1,s2*2,s3*3,s4*4,sterm*(numbyt)
!
      save /cstrng/
!
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
!
      jterm=kzstrm
      call win2ch(jterm,sterm)
!
!  if string terminator is used, locate terminator and replace it
!  with a null character
!
      if(abs(nf).eq.100)then
        k=1
        do i=1,maxint
          call win2ch(lfrom(i),sfrom(k:k+numbyt-1))
          k=k+numbyt
        end do
 
        if(kztmln.eq.1)then
          s1=sterm(1:1)
          loc=index(sfrom,s1)
        elseif(kztmln.eq.2)then
          s2=sterm(1:2)
          loc=index(sfrom,s2)
        elseif(kztmln.eq.3)then
          s3=sterm(1:3)
          loc=index(sfrom,s3)
        elseif(kztmln.eq.4)then
          s4=sterm
          loc=index(sfrom,s4)
        end if
        if(loc.gt.maxchr)then
          loc=maxchr
        end if
        if(loc.ge.nf+1)then
          loc=nf+1
        end if
        sfrom(loc:loc)=char(0)
        k=1
 
        do i=1,maxint
          call wch2in(sfrom(k:k+numbyt-1),lto(i))
          k=k+numbyt
        end do
!
!  if terminator is not used
!
      else
        num=(nf+(numbyt-1))/numbyt
        k=1
        do i=1,num
          call win2ch(lfrom(i),sfrom(k:k+numbyt-1))
          k=k+numbyt
        end do
        loc=nf+1
        if(loc.gt.maxchr)then
          loc=maxchr
        end if
        sfrom(loc:loc)=char(0)
        k=1
        do i=1,maxint
          call wch2in(sfrom(k:k+numbyt-1),lto(i))
          k=k+numbyt
        end do
 
      end if
 
      nt=loc-1
 
      return
      end
      subroutine zcurcs(xx,yy,npts)
!
!***********************************************************************
!
!! ZCURCS draws a curve using cubic spline interpolation.
!
!  input:   xx,yy = array of points
!  npts  = number of coordinate pairs in xx
!  and yy
!
      parameter (kzyes=111)
      parameter (kzno=222)
      parameter (kzcspl=3)
!
      real area(4)
      logical bad
      real c(4,104)
      logical logx
      logical logy
      real tau(104)
      real x(102)
      real xx(*)
      real y(102)
      real yy(*)
!
      save /cline/
      save /cmess/
      save /pltcom/
      save /pltsiz/
!
      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
!
!  if non-increasing x exists, use parametric polynomial interpolation
!
      do i=1,npts
        call scale(xx(i),yy(i),x(i),y(i))
      end do
 
      call zgsclp(area)
 
      do i=1,npts-1
        if(x(i+1).le.x(i))then
          if(npts.le.51)then
            call zcurps(xx,yy,npts)
          else
            call zcurpp('V',x,y,npts)
          end if
          return
        end if
      end do
!
!  add extra points at both ends to control boundary slope
!
      tau(1)=2.0*x(1)-x(2)
      c(1,1)=2.0*y(1)-y(2)
      tau(npts+2)=2.0*x(npts)-x(npts-1)
      c(1,npts+2)=2.0*y(npts)-y(npts-1)
      do i=2,npts+1
        tau(i)=x(i-1)
        c(1,i)=y(i-1)
      end do
!
!  calculate cubic spline matrix c
!
      call zcuspl(tau,c,npts+2,0,0,bad)
      if(bad)then
          if(npts.le.51)then
          call zcurps(xx,yy,npts)
        else
          call zcurpp('V',x,y,npts)
        end if
        return
      end if
!
!  draw the interpolated curve
!
      call dsmove(x(1),y(1))
      kzlbeg=kzyes
      kzmov=kzno
      zzprx=x(1)
      zzpry=y(1)
      zzprx1=x(1)
      zzpry1=y(1)
      zzprx2=x(1)
      zzpry2=y(1)
      do i=2,npts
        dt=x(i)-x(i-1)
        call zcurev(kzcspl,x(i-1),y(i-1),x(i),y(i),0.0,dt,
     &              0.0,0.0,1.0,x(i-1),
     &              c(4,i),c(3,i),c(2,i),c(1,i))
      end do
 
      call gsrclp(area)
      return
      end
      subroutine zcurdr(x,y,ilnold)
!
!***********************************************************************
!
!! ZCURDR draws a line from (x0,y0) to (x1,y1) in absolute coordinates. 
!
!  assumes that clipping has already been done.   to suppress unnecessary
!  "moves", this is the only routine that should
!  call gsdrvr(3,,,). the line is drawn in the
!  current line type.   this routine does not
!  set the absolute position (xapos,yapos). it
!  is up to the caller to do so if necessary.
!
!  input:   x,y  = arrays of points
!  npts = number of coordinate pairs in x and y
!
      parameter (kzyes=111)
      parameter (kzno=222)
      parameter (kzsoli=1)

      save /cline/
      save /cmess/
      save /dcltyp/

      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      logical linilt, lposnd

      dimension x(*),y(*),tx(4),ty(4),hx(4),hy(4)
!
!  locate long side and do cross hatch in same direction
!
      dx=x(1)-x(2)
      dy=y(1)-y(2)
      rlen=dx*dx+dy*dy
      dx1=x(2)-x(3)
      dy1=y(2)-y(3)
      rlen1=dx1*dx1+dy1*dy1

      if(rlen1.gt.rlen)then
        dx=dx1
        dy=dy1
      end if
!
!  for solid line, cross hatch the input polygon
!
      if(ilnold.eq.kzsoli)then
          call dsfill(x,y,4,tx,ty)
!
!  otherwise, do it according to current line style
!
      else
        x0=x(1)
        y0=y(1)
        x1=x(4)
        y1=y(4)
        hx(1)=x(1)
        hy(1)=y(1)
        hx(4)=x(4)
        hy(4)=y(4)
!
!  segment line to make current line type
!
        if(kzlbeg.ne.kzno)then
          inxtl = 1
          dleft = dist(1,ilnold-1)
          kzlbeg = kzno
        end if

100     continue
        dx = x(2)-x0
        dy = y(2)-y0
        dl = sqrt(dx**2+dy**2)
!
!  see if this segment is shorter that dist. left on line type
!
        if(dl .gt. dleft)then
!
!  segment is longer, so advance to line type break
!
          s = dleft/dl
          x0 = s*dx+x0
          y0 = s*dy+y0
          x1 = s*dx+x1
          y1 = s*dy+y1
!
!  see if this part of the line type is drawn or skipped
!
          if(ksyand(inxtl,1) .ne. 0)then
            dx1=hx(1)-x0
            dy1=hy(1)-y0
            rlen1=dx1*dx1+dy1*dy1
            dx1=hx(4)-x0
            dy1=hy(4)-y0
            rlen2=dx1*dx1+dy1*dy1
            if(rlen1.lt.rlen2)then
              hx(2)=x0
              hy(2)=y0
              hx(3)=x1
              hy(3)=y1
            else
              hx(2)=x1
              hy(2)=y1
              hx(3)=x0
              hy(3)=y0
            end if
              call dsfill(hx,hy,4,tx,ty)
            hx(1)=hx(2)
            hy(1)=hy(2)
            hx(4)=hx(3)
            hy(4)=hy(3)
          else
            hx(1)=x0
            hy(1)=y0
            hx(4)=x1
            hy(4)=y1
          end if
!
!  now go to next portion of line type
!
          inxtl = inxtl + 1
          if(inxtl .gt. dist(13,ilnold-1)) inxtl = 1
          dleft = dist(inxtl,ilnold-1)
          go to 100
!
!  draw last of line if drawn
!
        end if
        dleft = dleft - dl
        if(ksyand(inxtl,1) .ne. 0)then
          dx1=hx(1)-x(2)
          dy1=hy(1)-y(2)
          rlen1=dx1*dx1+dy1*dy1
          dx1=hx(4)-x(2)
          dy1=hy(4)-y(2)
          rlen2=dx1*dx1+dy1*dy1
          if(rlen1.lt.rlen2)then
            hx(2)=x(2)
            hy(2)=y(2)
            hx(3)=x(3)
            hy(3)=y(3)
          else
            hx(2)=x(3)
            hy(2)=y(3)
            hx(3)=x(2)
            hy(3)=y(2)
          end if
            call dsfill(hx,hy,4,tx,ty)
          kzmov = kzyes
        else
          kzmov = kzno
        end if
      end if
      return
      end
      subroutine zcurev(iflag,x1,y1,x2,y2,t,dt,ax1,bx1,cx,dx,
     &                    ay1,by1,cy,dy)
!
!***********************************************************************
!
!! ZCUREV draws a segment by increment tau.
!
!  input:   iflag = line style
!  x1,y1 = starting point
!  x2,y2 = ending point
!  t     = starting value of tau
!  dt    = increment of tau
!  ax1,bx1,cx,dx = parametric coef. of x
!  ay1,by1,cy,dy = parametric coef. of y
!
!  note:    iflag = kzcspl or kzpspl, for parametric
!  spline interpolation
!  iflag = kzpply, for parametric polynomial
!  interpolation
!
      parameter (kzno=222)
      parameter (kzcspl=3)
      parameter (kzpspl=4)
!
      save /cline/
      save /pltcom/
      save /pltsiz/
!
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      logical logx, logy
!
      if(iflag.eq.kzcspl.or.iflag.eq.kzpspl)then
        ax=ax1/6.0
        bx=bx1/2.0
        ay=ay1/6.0
        by=by1/2.0
      else
        ax=ax1
        bx=bx1
        ay=ay1
        by=by1
      end if
      dlen=sqrt((x1-x2)**2+(y1-y2)**2)
      idiv=int(dlen/0.3)+1
      dtau=dt/real(idiv)
      h=t
      if(kzlthk.eq.kzno)then
        do j=1,idiv
          h=h+dtau
          xt=dx+h*(cx+h*(bx+h*ax))
          yt=dy+h*(cy+h*(by+h*ay))
          call dsdraw(xt,yt)
        end do
      else
        do j=1,idiv
          h=h+dtau
          xt=dx+h*(cx+h*(bx+h*ax))
          yt=dy+h*(cy+h*(by+h*ay))
          call zcurth(xt,yt)
        end do
 
      end if
      return
      end
      subroutine zcurpp(cflag,x,y,npts)
!
!***********************************************************************
!
!! ZCURPP draws a curve using parametric cubic polynomial interpolation.
!
!  input:   cflag = flag for coordinate system
!  if 'W', input are in world coordinate
!  units, otherwise virtual coordinate
!  x,y   = array of points
!  npts  = number of points in x and y
!
      parameter (kzyes=111)
      parameter (kzno=222)
      parameter (kzpply=5)
!
      save /cline/
      save /cmess/
      save /pltcom/
      save /pltsiz/
!
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      logical logx, logy
!
      dimension x(*),y(*)
      dimension area(4)
      character cflag
      call zgsclp(area)
!
!  if inputs are in world coor
!
      if(cflag.eq.'W')then
!
!  do curve connecting first 3 points
!
        call scale(x(1),y(1),vx1,vy1)
        call scale(x(2),y(2),vx2,vy2)
        call scale(x(3),y(3),vx3,vy3)
        call scale(x(4),y(4),vx4,vy4)
        call zppoly(vx1,vx2,vx3,vx4,ax,bx,cx,dx)
        call zppoly(vy1,vy2,vy3,vy4,ay,by,cy,dy)
        call dsmove(vx1,vy1)
        kzlbeg=kzyes
        kzmov=kzno
        zzprx=vx1
        zzpry=vy1
        zzprx1=vx1
        zzpry1=vy1
        zzprx2=vx1
        zzpry2=vy1
        call zcurev(kzpply,vx1,vy1,vx2,vy2,0.0,1.0,
     &              ax,bx,cx,dx,ay,by,cy,dy)
        call zcurev(kzpply,vx2,vy2,vx3,vy3,1.0,1.0,
     &              ax,bx,cx,dx,ay,by,cy,dy)
        vx1=vx2
        vx2=vx3
        vx3=vx4
        vy1=vy2
        vy2=vy3
        vy3=vy4
!
!  process from 4-th point on
!
        if(npts.ge.5)then
          do i=3,npts-2
            call scale(x(i+2),y(i+2),vx4,vy4)
            call zppoly(vx1,vx2,vx3,vx4,ax,bx,cx,dx)
            call zppoly(vy1,vy2,vy3,vy4,ay,by,cy,dy)
            call zcurev(kzpply,vx2,vy2,vx3,vy3,1.0,1.0,
     &                  ax,bx,cx,dx,ay,by,cy,dy)
            vx1=vx2
            vx2=vx3
            vx3=vx4
            vy1=vy2
            vy2=vy3
            vy3=vy4
          end do
        end if
!
!  do last segment
!
        call zcurev(kzpply,vx2,vy2,vx3,vy3,
     &                2.0,1.0,ax,bx,cx,dx,ay,by,cy,dy)
!
!  if inputs are in virtual coor
!
      else
!
!  connect first 3 points
!
        call zppoly(x(1),x(2),x(3),x(4),ax,bx,cx,dx)
        call zppoly(y(1),y(2),y(3),y(4),ay,by,cy,dy)
        call dsmove(x(1),y(1))
        kzlbeg=kzyes
        kzmov=kzno
        zzprx=x(1)
        zzpry=y(1)
        zzprx1=x(1)
        zzpry1=y(1)
        zzprx2=x(1)
        zzpry2=y(1)
        call zcurev(kzpply,x(1),y(1),x(2),y(2),0.0,1.0,
     &              ax,bx,cx,dx,ay,by,cy,dy)
        call zcurev(kzpply,x(2),y(2),x(3),y(3),1.0,1.0,
     &              ax,bx,cx,dx,ay,by,cy,dy)
!
!  do 4-th point and on
!
        if(npts.ge.5)then
          do i=3,npts-2
            call zppoly(x(i-1),x(i),x(i+1),x(i+2),ax,bx,cx,dx)
            call zppoly(y(i-1),y(i),y(i+1),y(i+2),ay,by,cy,dy)
            call zcurev(kzpply,x(i),y(i),x(i+1),y(i+1),1.0,1.0,
     &                    ax,bx,cx,dx,ay,by,cy,dy)
          end do
        end if
!
!  last segment
!
        call zcurev(kzpply,x(npts-1),y(npts-1),x(npts),y(npts),
     &                2.0,1.0,ax,bx,cx,dx,ay,by,cy,dy)
      end if
      call gsrclp(area)
      return
      end
      subroutine zcurps(xx,yy,npts)
!
!***********************************************************************
!
!! ZCURPS draws a curve using cubic spline interpolation.
!
!  input:            xx,yy      arrays of points
!  npts      number of coordinate pairs in xx
!  and yy
!
      parameter (kzyes=111)
      parameter (kzno=222)
      parameter (kzpspl=4)
!
      save /cline/
      save /cmess/
      save /pltcom/
      save /pltsiz/
!
      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      logical logx, logy
!
      dimension tau(53),cx(4,53),cy(4,53),xx(*),yy(*)
      dimension area(4),x(51),y(51)
      logical bad
!
!  add extra points at both ends to control boundary slope
!
      do i=1,npts
        call scale(xx(i),yy(i),x(i),y(i))
      end do
 
      call zgsclp(area)
      tau(1)=0.0
      cx(1,1)=2.0*x(1)-x(2)
      cy(1,1)=2.0*y(1)-y(2)
      tau(npts+2)=real(npts+1)
      cx(1,npts+2)=2.0*x(npts)-x(npts-1)
      cy(1,npts+2)=2.0*y(npts)-y(npts-1)
      do i=2,npts+1
        cx(1,i)=x(i-1)
        cy(1,i)=y(i-1)
        tau(i)=real(i-1)
      end do
 
      call zcuspl(tau,cx,npts+2,0,0,bad)
      call zcuspl(tau,cy,npts+2,0,0,bad)
      call dsmove(x(1),y(1))
      kzlbeg=kzyes
      kzmov=kzno
      zzprx=x(1)
      zzpry=y(1)
      zzprx1=x(1)
      zzpry1=y(1)
      zzprx2=x(1)
      zzpry2=y(1)
      do i=2,npts
        call zcurev(kzpspl,x(i-1),y(i-1),x(i),y(i),0.0,1.0,
     &                cx(4,i),cx(3,i),cx(2,i),cx(1,i),
     &                cy(4,i),cy(3,i),cy(2,i),cy(1,i))
      end do
 
      call gsrclp(area)
      return
      end
      subroutine zcurth(xx,yy)
!
!***********************************************************************
!
!! ZCURTH draws a thick line.
!
!  input: xx,yy - coordinate to draw to (absolute)
!
      parameter (kzyes=111)
      parameter (kzlin=2)
      parameter (kzmaxc=255)
!
      save /cline/
      save /cmess/
      save /colorc/
      save /colorn/
      save /dcltyp/
      save /gccpos/
      save /gcvpos/
!
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /colorn/ kzshd,zzhspc,zzcolr(kzmaxc,3),kznclr,kzccol
      common /colorc/ czcolr
      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
      common /gcvpos/ xvpos, yvpos
      common /gccpos/ xapos, yapos, ivis, lcurnt
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
      character czcolr*8
      logical lcurnt
      logical linilt, lposnd
!
      integer gsivis
      dimension x(4),y(4),tx(4),ty(4),ax(4),ay(4)
!
      save epslon
      data epslon /0.0001/
!
!  calculate 4 corners of area to be filled
!
      ilnold=ilntyp
      ilntyp=1
      thick2=zzlthk/2.0
      x1=xvpos
      y1=yvpos
      x2=xx
      y2=yy
      dirx2=x2-x1
      diry2=y2-y1
!
!  for vertical line
!
      if(abs(dirx2).lt.epslon)then
        ax(1)=x1+thick2
        ax(2)=ax(1)
        ax(3)=x1-thick2
        ax(4)=ax(3)
        ay(1)=y1
        ay(2)=y2
        ay(3)=y2
        ay(4)=y1
!
!  non-vertical line
!
      else
        tanval=diry2/dirx2
        theta=atan(tanval)
        if(dirx2.lt.0.0)then
          cost=-cos(theta)
          sint=-sin(theta)
        else
          cost=cos(theta)
          sint=sin(theta)
        end if
        if(diry2.lt.0.0)then
          dy=-thick2*cost
          dx=thick2*sint
        else
          dy=thick2*cost
          dx=-thick2*sint
        end if
        ax(1)=x1+dx
        ax(4)=x1-dx
        ay(1)=y1+dy
        ay(4)=y1-dy
        ax(2)=x2+dx
        ax(3)=x2-dx
        ay(2)=y2+dy
        ay(3)=y2-dy
      end if
!
!  fill in inter-line space
!
      if(kzmov.eq.kzyes)then
        dirpx=zzprx1-x1
        dirpy=zzpry1-y1
        dotprd=dirpx*dirx2+dirpy*diry2
        if(dotprd.gt.0.0)then
          x(3)=zzprx2
          y(3)=zzpry2
        else
          x(3)=zzprx1
          y(3)=zzpry1
        end if
        dirx1=zzprx-x1
        diry1=zzpry-y1
        dirpx=ax(1)-x1
        dirpy=ay(1)-y1
        dotprd=dirpx*dirx1+dirpy*diry1
        if(dotprd.gt.0.0)then
          x(1)=ax(4)
          y(1)=ay(4)
        else
          x(1)=ax(1)
          y(1)=ay(1)
        end if
        x(2)=x1
        y(2)=y1
        if(kzltyp.eq.kzlin)then
          numpt=4
          if(abs(dirx1).lt.epslon)then
            if(abs(dirx2).lt.epslon)then
              numpt=0
            elseif(abs(diry2).lt.epslon)then
              x(4)=x(3)
              y(4)=y(1)
            else
              s2=diry2/dirx2
              y(4)=(x(3)-x(1))*s2+y(1)
              x(4)=(y(4)-y(1))/s2+x(1)
            end if
          elseif(abs(diry1).lt.epslon)then
            y(4)=y(3)
            if(abs(dirx2).lt.epslon)then
              x(4)=x(1)
            elseif(abs(diry2).lt.epslon)then
              numpt=0
            else
              s2=diry2/dirx2
              x(4)=(y(4)-y(1))/s2+x(1)
            end if
          else
            s1=diry1/dirx1
            if(abs(dirx2).lt.epslon)then
              x(4)=x(1)
              y(4)=(x(4)-x(3))*s1+y(3)
            elseif(abs(diry2).lt.epslon)then
              y(4)=y(1)
              x(4)=(y(4)-y(3))/s1+x(3)
            else
              s2=diry2/dirx2
              ds=s2-s1
              if(abs(ds).lt.epslon)then
                numpt=0
              else
                b1=y(3)-x(3)*s1
                b2=y(1)-x(1)*s2
                x(4)=(b1-b2)/ds
                y(4)=x(4)*s1+b1
              end if
            end if
          end if
          if(numpt.ne.0) call dhatch(x,y,4,90.0,zzhspc,1,tx,ty)
!
!  draw smooth corner in inter-line space
!
        else
          call dhatch(x,y,3,90.0,zzhspc,1,tx,ty)
        end if
      end if
      call zcurdr(ax,ay,ilnold)
      zzprx=x1
      zzpry=y1
      zzprx1=ax(2)
      zzpry1=ay(2)
      zzprx2=ax(3)
      zzpry2=ay(3)
      xvpos = xx
      yvpos = yy
      call gsrst(xvpos,yvpos,xa1,ya1)
      ivis1 = gsivis(xa1,ya1)
      xapos = xa1
      yapos = ya1
      ivis = ivis1
      ilntyp=ilnold
      return
      end
      subroutine zcurve(x,y,numpts)
!
!***********************************************************************
!
!! ZCURVE draws a line with appropriate clipping.
!
!  The line is from x(1),y(1) to x(npts),y(npts).
!
      parameter (kzyes=111)
      parameter (kzno=222)
!
      save /cline/
      save /cmess/
!
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
!
      dimension area(4)
      dimension x(*), y(*)
      call zgsclp(area)
      npts=abs(numpts)
      if(npts .gt. 1)then
        if(numpts.lt.0)then
          vx=x(1)
          vy=y(1)
        else
          call scale(x(1),y(1),vx,vy)
        end if
        call dsmove(vx,vy)
        kzmov=kzno
        kzlbeg=kzyes
        zzprx=vx
        zzpry=vy
        zzprx1=vx
        zzpry1=vy
        zzprx2=vx
        zzpry2=vy
!
!  in virtual coordinates
!
        if(numpts.lt.0)then
          if(kzlthk.eq.kzno)then
            do i=2,npts
              call dsdraw(x(i),y(i))
            end do
          else
            do i=2,npts
              call zcurth(x(i),y(i))
            end do
          end if
!
!  in world coordinates
!
        else
          if(kzlthk.eq.kzno)then
            do i=2,npts
              call scale(x(i),y(i),vx,vy)
              call dsdraw(vx,vy)
            end do
          else
            do i=2,npts
              call scale(x(i),y(i),vx,vy)
              call zcurth(vx,vy)
            end do
          end if
        end if
      end if
      call gsrclp(area)
      return
      end
      subroutine zcuspl ( tau, c, n, ibcbeg, ibcend ,bad)
!
!***********************************************************************
!
!! ZCUSPL determines a cubic interpolating spline.
!
!  from  * a practical guide to splines *  by c. de boor
!
!  input
!
!  n = number of data points. assumed to be .ge. 2.
!  (tau(i), c(1,i), i=1,...,n) = abscissae and ordinates of the
!  data points. tau is assumed to be strictly increasing.
!  ibcbeg, ibcend = boundary condition indicators, and
!  c(2,1), c(2,n) = boundary condition information. specifically,
!  ibcbeg = 0  means no boundary condition at tau(1) is given.
!  in this case, the not-a-knot condition is used, i.e. the
!  jump in the third derivative across tau(2) is forced to
!  zero, thus the first and the second cubic polynomial pieces
!  are made to coincide.)
!  ibcbeg = 1  means that the slope at tau(1) is made to equal
!  c(2,1), supplied by input.
!  ibcbeg = 2  means that the second derivative at tau(1) is
!  made to equal c(2,1), supplied by input.
!  ibcend = 0, 1, or 2 has analogous meaning concerning the
!  boundary condition at tau(n), with the additional infor-
!  mation taken from c(2,n).
!
!  output
!
!  c(j,i), j=1,...,4; i=1,...,l (= n-1) = the polynomial coefficients
!  of the cubic interpolating spline with interior knots (or
!  joints) tau(2), ..., tau(n-1). precisely, in the interval
!  interval (tau(i), tau(i+1)), the spline f is given by
!  f(x) = c(1,i)+h*(c(2,i)+h*(c(3,i)+h*c(4,i)/3.)/2.)
!  where h = x - tau(i). the function program *ppvalu* may be
!  used to evaluate f or its derivatives from tau,c, l = n-1,
!  and k=4.
!
      logical bad
      integer ibcbeg,ibcend,n,   i,j,l,m
      real c(4,n),tau(n),   divdf1,divdf3,dtau,g
!
!  a tridiagonal linear system for the unknown slopes s(i) of
!  f at tau(i), i=1,...,n, is generated and then solved by gauss elim-
!  ination, with s(i) ending up in c(2,i), all i.
!  c(3,.) and c(4,.) are used initially for temporary storage.
!
      save epslon
      data epslon /0.000001/
      bad = .false.
      l = n-1
!
!  compute first differences of tau sequence and store in c(3,.). also,
!  compute first divided difference of data and store in c(4,.).
!
      do m=2,n
         c(3,m) = tau(m) - tau(m-1)
         if(abs(c(3,m)).le.epslon)then
           bad=.true.
           return
         end if
         c(4,m) = (c(1,m) - c(1,m-1))/c(3,m)
      end do
!
!  construct first equation from the boundary condition, of the form
!  c(4,1)*s(1) + c(3,1)*s(2) = c(2,1)
!
      if(ibcbeg-1)                     11,15,16
   11 if(n .gt. 2)                     go to 12
!
!  no condition at left end and n = 2.
!
      c(4,1) = 1.
      c(3,1) = 1.
      c(2,1) = 2.*c(4,2)
      go to 25
!
!  not-a-knot condition at left end and n .gt. 2.
!
   12 c(4,1) = c(3,3)
      c(3,1) = c(3,2) + c(3,3)
      if(abs(c(3,1)).le.epslon)then
        bad=.true.
        return
      end if
      c(2,1) =((c(3,2)+2.*c(3,1))*c(4,2)*c(3,3)+c(3,2)**2*c(4,3))/c(3,1)
      go to 19
!
!  slope prescribed at left end.
!
   15 c(4,1) = 1.
      c(3,1) = 0.
      go to 18
!
!  second derivative prescribed at left end.
!
   16 c(4,1) = 2.
      c(3,1) = 1.
      if(abs(c(2,1)).le.epslon)then
        bad=.true.
        return
      end if
      c(2,1) = 3.*c(4,2) - c(3,2)/2.*c(2,1)
   18 if(n .eq. 2)                      go to 25
!
!  if there are interior knots, generate the corresp. equations and car-
!  ry out the forward pass of gauss elimination, after which the m-th
!  equation reads    c(4,m)*s(m) + c(3,m)*s(m+1) = c(2,m).
!
   19 continue
 
      do m=2,l
         if(abs(c(4,m-1)).le.epslon)then
           bad=.true.
           return
         end if
         g = -c(3,m+1)/c(4,m-1)
         c(2,m) = g*c(2,m-1) + 3.*(c(3,m)*c(4,m+1)+c(3,m+1)*c(4,m))
         c(4,m) = g*c(3,m-1) + 2.*(c(3,m) + c(3,m+1))
      end do
!
!  construct last equation from the second boundary condition, of the form
!  (-g*c(4,n-1))*s(n-1) + c(4,n)*s(n) = c(2,n)
!  if slope is prescribed at right end, one can go directly to back-
!  substitution, since c array happens to be set up just right for it
!  at this point.
!
      if(ibcend-1)                     21,30,24
   21 if(n .eq. 3 .and. ibcbeg .eq. 0) go to 22
!
!  not-a-knot and n .ge. 3, and either n.gt.3 or  also not-a-knot at
!  left end point.
!
      g = c(3,n-1) + c(3,n)
      if(abs(c(3,n-1)).le.epslon.or.abs(g).le.epslon.or.
     &       abs(c(4,n-1)).le.epslon)then
        bad=.true.
        return
      end if
      c(2,n) = ((c(3,n)+2.*g)*c(4,n)*c(3,n-1)
     &            + c(3,n)**2*(c(1,n-1)-c(1,n-2))/c(3,n-1))/g
      g = -g/c(4,n-1)
      c(4,n) = c(3,n-1)
                                        go to 29
!
!  either (n=3 and not-a-knot also at left) or (n=2 and not not-a-
!  knot at left end point).
!
   22 c(2,n) = 2.*c(4,n)
      c(4,n) = 1.
                                        go to 28
!
!  second derivative prescribed at right endpoint.
!
 24   if(abs(c(2,n)).le.epslon)then
        bad=.true.
        return
      end if
      c(2,n) = 3.*c(4,n) + c(3,n)/2.*c(2,n)
      c(4,n) = 2.
                                        go to 28
   25 if(ibcend-1)                     26,30,24
   26 if(ibcbeg .gt. 0)                go to 22
!
!  not-a-knot at right endpoint and at left endpoint and n = 2.
!
      c(2,n) = c(4,n)
                                        go to 30
 28   if(abs(c(4,n-1)).le.epslon)then
        bad=.true.
        return
      end if
      g = -1./c(4,n-1)
!
!  complete forward pass of gauss elimination.
!
   29 c(4,n) = g*c(3,n-1) + c(4,n)
      if(abs(c(4,n)).le.epslon)then
        bad=.true.
        return
      end if
      c(2,n) = (g*c(2,n-1) + c(2,n))/c(4,n)
!
!  Carry out back substitution
!
   30 j = l
         if(abs(c(4,j)).le.epslon)then
           bad=.true.
           return
         end if
   40    c(2,j) = (c(2,j) - c(3,j)*c(2,j+1))/c(4,j)
         j = j - 1
         if(j .gt. 0)                  go to 40
!
!  generate cubic coefficients in each interval, i.e., the deriv.s
!  at its left endpoint, from value and slope at its endpoints.
!
      do i=2,n
         if(abs(c(3,i)).le.epslon)then
           bad=.true.
           return
         end if
         dtau = c(3,i)
         divdf1 = (c(1,i) - c(1,i-1))/dtau
         divdf3 = c(2,i-1) + c(2,i) - 2.*divdf1
         c(3,i-1) = 2.*(divdf1 - c(2,i-1) - divdf3)/dtau
         c(4,i-1) = (divdf3/dtau)*(6./dtau)
      end do
 
      return
      end
      subroutine zdispt(x1,y1,x2,y2,rlen,xp,yp)
!
!***********************************************************************
!
!! ZDISPT finds a point RLEN units from (x2,y2) on the line to (x1,y1).
!
!  input:   x1,y1 = point 1
!  x2,y2 = point 2
!  rlen  = distance from point2
!  if positive, distance measured towards
!  point 1, otherwise, away from point 1
!
!  output:  xp,yp = located point
!
!
      save epslon
      data epslon /0.0001/
!
      dx=x2-x1
      dy=y2-y1
!
!  for vertical lines
!
      if(abs(dx).lt.epslon)then
        xp=x1
        yp=y2-rlen*dy/abs(dy)
!
!  for non-vertical lines
!
      else
        tanval=dy/dx
        theta=atan(tanval)
        if(dx.lt.0.0)then
          cost=-cos(theta)
          sint=-sin(theta)
        else
          cost=cos(theta)
          sint=sin(theta)
        end if
        yp=y2-sint*rlen
        xp=x2-cost*rlen
      end if
      return
      end
      subroutine zdraw(ax1,ay1,ax2,ay2,rx,ry,numr)
!
!***********************************************************************
!
!! ZDRAW checks if a line goes through any blanked areas. 
!
!  if part(s) of line is blanked, return unclipped portions
!
!  input:   ax1,ay1 = coordinates of point 1
!  ax2,ay2 = coordinates of point 2
!
!  output:  rx,ry = array for coordinates of clipped lines
!  numr  = number of coordinates in array rx and ry
!
      parameter (kzmxbs=1000)
      parameter (kzmxbl=200)
!
      save /cblank/
!
      common /cblank/ zzblnk(kzmxbl,4),zzblks(kzmxbs,4),kzblcn,kzbscn
      dimension rx(300,2),ry(300,2)
      logical visibo(300)
!
!  init visibo to false
!
      do k=1,300
        visibo(k)=.false.
      end do
!
!  load input line into rx and ry
!
      rx(1,1)=ax1
      rx(1,2)=ax2
      ry(1,1)=ay1
      ry(1,2)=ay2
      numr=1
      visibo(1)=.true.
!
!  go thru each and every blank symbol
!  to check if line is blocked
!
      if(kzbscn.gt.0)then
        do i=1,kzbscn
          if(zzblks(i,1).gt.-1000.0.and.numr.gt.0)then
            call zdraw3(rx,ry,visibo,numr,zzblks(i,1),
     &               zzblks(i,2),zzblks(i,3),zzblks(i,4))
          end if
        end do
      end if
!
!  go thru each and every blank area
!  to check if line is blocked
!
      do i=1,kzblcn
        if(zzblnk(i,1).gt.-1000.0.and.numr.gt.0)then
            call zdraw3(rx,ry,visibo,numr,zzblnk(i,1),
     &               zzblnk(i,2),zzblnk(i,3),zzblnk(i,4))
        end if
      end do
 
      return
      end
      subroutine zdraw1(x1,y1,x2,y2,rx,ry,visibo,iptr)
!
!***********************************************************************
!
!! ZDRAW1 copies two endpoints to arrays, rx, ry.
!
!  input:   x1,y1 = coord for point 1
!  x2,y2 = coord for point 2
!  iptr  = current pointer of rx and ry
!
!  output:  rx,ry  = arrays of coord
!  visibo = logical array for visibility of corresponding
!  point in rx and ry
!
      real rx(300,2)
      real ry(300,2)
      logical visibo(*)
!
      iptr=iptr+1
      rx(iptr,1)=x1
      rx(iptr,2)=x2
      ry(iptr,1)=y1
      ry(iptr,2)=y2
      visibo(iptr)=.true.
 
      return
      end
      subroutine zdraw2(type,x1,y1,x2,y2,bound,rmax,rmin,xf,yf,iptr)
!
!***********************************************************************
!
!! ZDRAW2 finds the intersection of a line and a vertical or horizontal line.
!
!  The vertical line is x=bound or the horizontal line is y=bound
!
!  input:   type      = indicator of slope for line 1
!  if = 'X' then find intersection with
!  a vertical line, else find intersection
!  with a horizontal line.
!  x1,y1     = point 1 of line 2
!  x2,y2     = point 2 of line 2
!  bound     = location of line 1
!  rmax,rmin = endpoints of line 1
!
!  output:  xf,yf = intersection coord
!  iptr  = counter to increment if xf and yf located
!
      real epslon
      parameter (epslon=0.00001)
!
      real xf(*),yf(*)
      character type
!
      ip=iptr
      slope=(y2-y1)/(x2-x1)
!
!  find intersection with a vertical line
!  x=bound
!
      if(type.eq.'X')then
 
        if(y1.gt.y2)then
          ymax=y1+epslon
          ymin=y2-epslon
        else
          ymax=y2+epslon
          ymin=y1-epslon
        end if
 
        yt=slope*(bound-x1)+y1
 
        if(yt.le.ymax.and.yt.ge.ymin.and.
     &        yt.le.rmax.and.yt.ge.rmin)then
          iptr=iptr+1
          yf(iptr)=yt
          xf(iptr)=bound
        end if
!
!  find intersection with a horizontal line
!  y=bound
!
      else
 
        if(x1.gt.x2)then
          xmax=x1+epslon
          xmin=x2-epslon
        else
          xmax=x2+epslon
          xmin=x1-epslon
        end if
        xt=(bound-y1)/slope+x1
        if(xt.le.xmax.and.xt.ge.xmin.and.
     &        xt.le.rmax.and.xt.ge.rmin)then
          iptr=iptr+1
          xf(iptr)=xt
          yf(iptr)=bound
        end if
      end if
 
      if(iptr.ne.ip)then
        do i=1,ip
          if(abs(xf(i)-xf(iptr)).lt.epslon.and.
     &          abs(yf(i)-yf(iptr)).lt.epslon)then
              iptr=iptr-1
            return
          end if
        end do
 
      end if
 99   continue
      return
      end
      subroutine zdraw3(rx,ry,visibo,numr,rxmax,rxmin,rymax,rymin)
!
!***********************************************************************
!
!! ZDRAW3 checks if a line goes throgh a blank area. 
!
!  if part(s) of line is blanked, return unclipped
!  portions
!
!  input:   rx,ry  = arrays of coordinate pairs
!  visibo = logical array indicating visibility of
!  corresponding points
!  numr   = number of points in rx and ry
!  rxmax,rxmin,rymax,rymin = bounds of blank area
!
!  output:  rx,ry = array for coordinates of clipped lines
!  numr  = number of coordinates in array rx and ry
!
      real epslon
      parameter (epslon = 0.0001)
!
      dimension rx(300,2),ry(300,2),xf(10)
      real yf(10)
      logical visibo(300)
!
!  if blank area exists, retrieve boundaries
!
      iptr=numr
 
      do j=1,numr
        x1=rx(j,1)
        x2=rx(j,2)
        y1=ry(j,1)
        y2=ry(j,2)
        dx=x2-x1
        dy=y2-y1
!
!  for vertical lines
!
        if(abs(dx).lt.epslon)then
 
          if(y1.gt.y2) then
            call zzswap(y1,y2)
          end if
 
          if(x1.lt.rxmax.and.x1.gt.rxmin)then
            if(y1.lt.rymin)then
              if(y2.gt.rymin)then
                call zdraw1(x1,y1,x1,rymin,rx,ry,visibo,iptr)
                visibo(j)=.false.
                if(y2.gt.rymax)then
                  call zdraw1(x1,rymax,x1,y2,rx,ry,visibo,iptr)
                end if
              end if
            elseif(y1.lt.rymax)then
              visibo(j)=.false.
              if(y2.gt.rymax)then
                call zdraw1(x1,rymax,x1,y2,rx,ry,visibo,iptr)
              end if
            end if
          end if
!
!  for horizontal lines
!
        elseif(abs(dy).lt.epslon)then
 
          if(y1.lt.rymax.and.y1.gt.rymin)then
 
            if(x1.gt.x2) then
              call zzswap(x1,x2)
            end if
 
            if(x1.lt.rxmin)then
 
              if(x2.gt.rxmin)then
 
                call zdraw1(x1,y1,rxmin,y1,rx,ry,visibo,iptr)
                visibo(j)=.false.
 
                if(x2.gt.rxmax)then
                  call zdraw1(rxmax,y1,x2,y1,rx,ry,visibo,iptr)
                end if
 
              end if
 
            elseif(x1.lt.rxmax)then
 
              visibo(j)=.false.
 
              if(x2.gt.rxmax)then
                call zdraw1(rxmax,y1,x2,y1,rx,ry,visibo,iptr)
              end if
 
            end if
 
          end if
!
!  for all other lines
!
        else
 
          ic=0
          call zdraw2('X',x1,y1,x2,y2,rxmin,rymax,rymin,xf,yf,ic)
          call zdraw2('X',x1,y1,x2,y2,rxmax,rymax,rymin,xf,yf,ic)
          call zdraw2('Y',x1,y1,x2,y2,rymin,rxmax,rxmin,xf,yf,ic)
          call zdraw2('Y',x1,y1,x2,y2,rymax,rxmax,rxmin,xf,yf,ic)
 
          if(ic.eq.0)then
 
            xmid=(x1+x2)/2.0
            ymid=(y1+y2)/2.0
 
            if(xmid.gt.rxmin.and.xmid.lt.rxmax.and.
     &          ymid.gt.rymin.and.ymid.lt.rymax)then
              visibo(j)=.false.
            end if
 
          elseif(ic.eq.1)then
 
            visibo(j)=.false.
 
            if(abs(x1-rxmin).lt.epslon.or.
     &          abs(x1-rxmax).lt.epslon.or.
     &          abs(y1-rymin).lt.epslon.or.
     &          abs(y1-rymax).lt.epslon)then
 
              if(x2.gt.rxmin.and.x2.lt.rxmax.and.
     &            y2.gt.rymin.and.y2.lt.rymax)then
                call zdraw1(xf(1),yf(1),x1,y1,rx,ry,visibo,iptr)
              else
                call zdraw1(x2,y2,xf(1),yf(1),rx,ry,visibo,iptr)
              end if
 
            else
 
              if(x1.gt.rxmin.and.x1.lt.rxmax.and.
     &            y1.gt.rymin.and.y1.lt.rymax)then
                call zdraw1(xf(1),yf(1),x2,y2,rx,ry,visibo,iptr)
              else
                call zdraw1(x1,y1,xf(1),yf(1),rx,ry,visibo,iptr)
              end if
 
            end if
 
          elseif(ic.eq.2)then
 
            visibo(j)=.false.
 
            if(xf(1).gt.xf(2))then
              call zzswap(xf(1),xf(2))
              call zzswap(yf(1),yf(2))
            end if
 
            if(dx.gt.0.0)then
              call zdraw1(x1,y1,xf(1),yf(1),rx,ry,visibo,iptr)
              call zdraw1(xf(2),yf(2),x2,y2,rx,ry,visibo,iptr)
            else
              call zdraw1(x1,y1,xf(2),yf(2),rx,ry,visibo,iptr)
              call zdraw1(xf(1),yf(1),x2,y2,rx,ry,visibo,iptr)
            end if
 
          end if
 
        end if
 
      end do
!
!  garbage clean up in arrays rx and ry
!
      ioff=0
 
      do k=1,iptr
 
        if(.not.visibo(k))then
 
          ioff=ioff+1
 
        else
 
          do l=1,2
            rx(k-ioff,l)=rx(k,l)
            ry(k-ioff,l)=ry(k,l)
            visibo(k)=.false.
            visibo(k-ioff)=.true.
          end do
 
        end if
 
      end do
 
      iptr=iptr-ioff
      numr=iptr
      return
      end
      subroutine zdrlog(xor,xcyc,yor,ycyc,xaxis,yaxis,xpos,ypos,iaxes)
!
!***********************************************************************
!
!! ZDRLOG ...
!
      parameter (kzno=222)
!
      integer numbr(14)
      logical logx, logy
      logical logxx, logyy, logt
      logical lhead,lfirst
      dimension qlog(9)
!
      save /caxis/
      save /ccoord/
      save /clabel/
      save /cmxalf/
      save /cpage/
      save /cstrng/
      save /cunit/
      save /pltclp/
      save /pltcom/
      save /pltprm/
      save /pltsiz/
!
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
      common /ccoord/ zzxmin,zzxmax,zzxstp,zzymin,zzymax,zzystp
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /cunit/  zzunit
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      common /pltclp/ xmin,xmax,ymin,ymax
      common /pltprm/ cxsize, cysize, tickln, yvini
      character czalfl*1,czalfn*5,czalfs*5
!
      save pio180,qlog,tminld
!
      data pio180 /1.745329252e-2/
      data qlog /0.3010, 0.4771, 0.6021, 0.6990, 0.7782, 0.8451,
     &   0.9031, 0.9542, 1.0/
!
!  minimum distance between short ticks (1 mm)
!
      data tminld /0.1/
!
!  calculate axis label and origin offset
!
!w      call zscopy('10',iten)
      tickln=0.6*zzhite
      call sybyt4('N',iten,2,48)
      call sybyt4('N',iten,1,49)
      call gssetc(0.9*zzhite,0.0)
      tenlen=zxmess(iten,2,czalfl,czalfn,czalfs)*1.4
      dx=xpos*zzunit*zzpagr
      dy=ypos*zzunit*zzpagr
!
!  protect mapprm info this way
!
      yvlen = yvini
!
!  see what type of axes are desired
!
      logxx=ksyand(iaxes,1).ne.0
      logyy=ksyand(iaxes,2).ne.0
      lhead=ksyand(iaxes,16).ne.0
!
!  do the axes scaling
!
      if(logxx)then
        xmin=xor
        xmax=xaxis/xcyc+xmin
        zzxmin=10.0**xor
        zzxmax=10.0**(xaxis/xcyc+xor)
        xtmin = xmin
        xtmax = xmax
        xl=10.0**xmin
        xh=10.0**xmax
        raxis=xaxis*zzunit
        numtks=min0(10,int(raxis/((ilabsz()+1.0)*cxsize)))
        call laxis(xl,xh,numtks,xmn,xmx,xtick)
        zzxstp=10.0**xtick
        ux0 = xmin
        udx = xmax - xmin
      end if
 
      if(logyy)then
        ymin=yor
        ymax=yaxis/ycyc+ymin
        zzymin=10.0**yor
        zzymax=10.0**(yaxis/ycyc+yor)
        ytmin = ymin
        ytmax = ymax
        yl=10.0**ymin
        yh=10.0**ymax
        raxis=yaxis*zzunit
!
!  JVB
!  Replaced "cxsize" by "gslens('0')
!
        numtks=min0(10,int(raxis/((ilabsz()+1.0)*gslens('0'))))
        call laxis(yl,yh,numtks,ymn,ymx,ytick)
        zzystp=10.0**ytick
        uy0 = ymin
        udy = ymax - ymin
      end if
!
!  draw x axis
!
      if(logxx.and.kzxaln.ne.0)then
        logt = .false.
        if(xtick .eq. 1.0)then
          call scale(xmin,ymin,temp,vy)
          call scale(xmin+1.0-qlog(8),ymin,vx,vy)
          if((vx-temp) .ge. tminld) logt = .true.
        end if
!
!  draw x axis line
!
        y = ymin
        call scale(xmin,y,vx,vy)
        vxold=vx
        lfirst=.true.
!
!  draw and label x axis ticks
!
        angx=cos(zzxlba*pio180)*tenlen
        angy=sin(zzxlba*pio180)*tenlen
        angmax=0.0
        epslon=xtick*0.1
        x = xtmin
 
 400    continue
 
        if(kzxtck.ne.0)then
          call dsmove(vxold,vy)
          if(lfirst)then
            lfirst=.false.
            call dsdraw(vx+dx,vy-tickln*1.2+dy)
          else
            call scale(x,y,vx,vy)
            call dsdraw(vx+dx,vy+dy)
            call dsdraw(vx+dx,vy-tickln*1.2+dy)
            vxold=vx
          end if
        else
          call scale(x,y,vx,vy)
        end if
 
        if(kzxnon.eq.kzno)then
          if(zzxlba.lt.3.0)then
            angx1=angx*0.5
            angy1=2.4*zzhite
            angmax=angy1
          elseif(zzxlba.lt.270.0)then
            angx1=angx-0.45*zzhite
            angy1=angy+1.3*zzhite+1.1*zzhite*cos(zzylba*pio180)
            if(angmax.lt.angy1) angmax=angy1
          else
            angx1=0.45*zzhite
            angy1=1.3*zzhite+1.1*zzhite*cos(zzylba*pio180)
            temp=angy1-angy
            if(angmax.lt.temp) angmax=temp
          end if
 
          call gssetc(0.9*zzhite,zzxlba)
          numbr(1)=-1
          call znmstr(x,numbr)
          call dsmove(vx-angx1+dx,vy-angy1+dy)
          call ztext(iten,2,czalfl,czalfn,czalfs)
          call gssetc(0.7*zzhite,zzxlba)
          call dsmove(vx-angx1+angx/1.4-0.5*zzhite*
     &                  sin(zzxlba*pio180)+dx,
     &                  vy-angy1+angy/1.4+0.5*zzhite*
     &                  cos(zzxlba*pio180)+dy)
          ln=leng(numbr)
          call ztext(numbr,ln,czalfl,czalfn,czalfs)
        else
          angmax=2.4*zzhite
        end if
!
!  do extra ticking if extra ticks will be far enough apart
!
        if(x+epslon .lt. xtmax)then
          if(logt)then
 
            do j = 1, 9
              if(kzxtck.gt.1)then
                rincr=1.0/kzxtck
                do k=1,kzxtck-1
                  call scale(x+alog10(j+rincr*k),y,vx,vy)
                  call dsmove(vx+dx,vy+dy)
                  call dsdraw(vx+dx,vy-tickln*0.6+dy)
                end do
              end if
              if(kzxtck.ne.0.and.j.ne.9)then
                call scale(x+qlog(j),y,vx,vy)
                call dsmove(vx+dx,vy+dy)
                call dsdraw(vx+dx,vy-tickln*0.9+dy)
              end if
            end do
 
          else
            if(xtick.gt.1.0)then
 
              do j=1,int(xtick)
                if(j.ne.int(xtick))then
                  call scale(x+j,ymin,vx,vy)
                  if(kzxtck.ne.0)then
                    call dsmove(vx+dx,vy+dy)
                    call dsdraw(vx+dx,vy+dy-tickln*1.2)
                  end if
                  if(kzxnon.eq.kzno)then
                    call gssetc(0.9*zzhite,zzxlba)
                    numbr(1)=-1
                    call znmstr(x+real(j),numbr)
                    call dsmove(vx-angx1+dx,vy-angy1+dy)
                    call ztext(iten,2,czalfl,czalfn,czalfs)
                    call gssetc(0.7*zzhite,zzxlba)
                    call dsmove(vx-angx1+angx/1.4-0.5*zzhite*
     &                          sin(zzxlba*pio180)+dx,
     &                          vy-angy1+angy/1.4+0.5*zzhite*
     &                          cos(zzxlba*pio180)+dy)
                    ln=leng(numbr)
                    call ztext(numbr,ln,czalfl,czalfn,czalfs)
                  end if
                end if
                if(kzxtck.gt.1)then
                  rincr=10.0/kzxtck
 
                  do k=1,kzxtck-1
                    call scale(x+j-1.0+alog10(rincr*k),y,vx,vy)
                    call dsmove(vx+dx,vy+dy)
                    call dsdraw(vx+dx,vy-tickln*0.6+dy)
                  end do
 
                end if
              end do
 
            end if
          end if
        end if
        x = x + xtick
        if(x-epslon.le.xmax) go to 400
        call gssetc(zzhite,0.0)
!
!  now place x axis label
!
        call scale((xmin+xmax)/2.0,ymin,vx,vy)
        rlen=zxmess(kzxlab,kzxaln,czalfl,czalfn,czalfs)
        call dsmove(vx-rlen/2.0+dx,vy-angmax-1.7*zzhite+dy)
        call ztext(kzxlab,kzxaln,czalfl,czalfn,czalfs)
        if(kzxnfl.eq.kzno) kzxaln=0
      end if
!
!  draw y axes
!
      if(logyy.and.kzyaln.ne.0)then
        call gssetc(0.7*zzhite,90.0)
        logt = .false.
        if(ytick .eq. 1.0)then
          call scale(xmin,ymin,vx,temp)
          call scale(xmin,ymin+1.0-qlog(8),vx,vy)
          if((vy-temp) .ge. tminld) logt = .true.
        end if
!
!  draw y axis line
!
        x = xmin
        call scale(x,ymin,vx,vy)
        vyold=vy
        lfirst=.true.
!
!  draw and label y axis ticks
!
        angx=cos(zzylba*pio180)*tenlen
        angy=sin(zzylba*pio180)*tenlen
        angmax=0.0
        epslon=ytick*0.1
        y = ytmin

 110    continue
        if(kzytck.ne.0)then
          call dsmove(vx+dx,vyold+dy)
          if(lfirst)then
            lfirst=.false.
          else
            call scale(x,y,vx,vy)
            call dsdraw(vx+dx,vy+dy)
          end if
          call dsdraw(vx-tickln*1.2+dx,vy+dy)
          vyold=vy
        else
          call scale(x,y,vx,vy)
        end if
!
!  place the appropiate label
!
        if(kzynon.eq.kzno)then
          if(zzylba.gt.87.0.and.zzylba.lt.93.0)then
            angy1=angy*0.5
            angx1=1.3*zzhite
            angmax=2.5*zzhite
          elseif(zzylba.le.270)then
            angy1=angy+0.45*zzhite
            angx1=angx+1.3*zzhite
            temp=angx1+1.1*zzhite*sin(zzylba*pio180)
            if(angmax.lt.temp) angmax=temp
          else
            angy1=angy+0.45*zzhite
            angx1=angx+1.3*zzhite-1.1*zzhite*sin(zzylba*pio180)
            if(angmax.lt.angx1) angmax=angx1
          end if
          call gssetc(0.9*zzhite,zzylba)
          numbr(1)=-1
          call znmstr(y,numbr)
          call dsmove(vx-angx1+dx,vy-angy1+dy)
          call ztext(iten,2,czalfl,czalfn,czalfs)
          call gssetc(0.7*zzhite,zzylba)
          call dsmove(vx-angx1+angx/1.4-0.5*zzhite*
     &                    sin(zzylba*pio180)+dx,
     &                    vy-angy1+angy/1.4+0.5*zzhite*
     &                    cos(zzylba*pio180)+dy)
          ln=leng(numbr)
          call ztext(numbr,ln,czalfl,czalfn,czalfs)
        else
          angmax=2.5*zzhite
        end if
!
!  do extra ticking if extra ticks will be far enough apart
!
        if(y+epslon.lt. ytmax)then
          if(logt)then
            do j = 1, 9
              if(kzytck.gt.1)then
                rincr=1.0/kzytck
                do k=1,kzytck-1
                  call scale(x,y+alog10(j+rincr*k),vx,vy)
                  call dsmove(vx+dx,vy+dy)
                  call dsdraw(vx-tickln*0.6+dx,vy+dy)
                end do
              end if
              if(kzytck.ne.0.and.j.ne.9)then
                call scale(x,y+qlog(j),vx,vy)
                call dsmove(vx+dx,vy+dy)
                call dsdraw(vx-tickln*0.9+dx,vy+dy)
              end if
            end do
          else
            if(ytick.gt.1.0)then
              do j=1,int(ytick)
                if(j.ne.int(ytick))then
                  call scale(xmin,y+j,vx,vy)
                  if(kzytck.ne.0)then
                    call dsmove(vx+dx,vy+dy)
                    call dsdraw(vx-tickln*1.2+dx,vy+dy)
                  end if
                  if(kzynon.eq.kzno)then
                    call gssetc(0.9*zzhite,zzylba)
                    numbr(1)=-1
                    call znmstr(y+real(j),numbr)
                    call dsmove(vx-angx1+dx,vy-angy1+dy)
                    call ztext(iten,2,czalfl,czalfn,czalfs)
                    call gssetc(0.7*zzhite,zzylba)
                    call dsmove(vx-angx1+angx/1.4-0.5*zzhite*
     &                            sin(zzylba*pio180)+dx,
     &                            vy-angy1+angy/1.4+0.5*zzhite*
     &                            cos(zzylba*pio180)+dy)
                    ln=leng(numbr)
                    call ztext(numbr,ln,czalfl,czalfn,czalfs)
                  end if
                end if
                if(kzytck.gt.1)then
                  rincr=10.0/kzytck
                  do k=1,kzytck-1
                    call scale(x,y+j-1.0+alog10(rincr*k),vx,vy)
                    call dsmove(vx+dx,vy+dy)
                    call dsdraw(vx-tickln*0.6+dx,vy+dy)
                  end do
                end if
              end do
            end if
          end if
        end if
        y = y + ytick
!
        if(y-epslon.le.ymax) go to 110
!
!  now place y label
!
        call gssetc(zzhite,90.0)
        call scale(xmin,(ymin+ymax)/2.0,vx,vy)
        rlen=zxmess(kzylab,kzyaln,czalfl,czalfn,czalfs)
        call dsmove(vx-angmax-0.7*zzhite+dx, vy-rlen/2.0+dy)
        call ztext(kzylab,kzyaln,czalfl,czalfn,czalfs)
        call gssetc(zzhite,0.0)
        if(kzynfl.eq.kzno) kzyaln=0
      end if
!
!  write title of plot
!
      if(lhead.and.kztiln.ne.0)then
        call gssetc(1.46*zzhite,0.0)
        space= max (zzhite,0.1*yvlen)
        call scale((xmin+xmax)/2.0,ymax,vx,vy)
        rlen=zxmess(kztitl,kztiln,czalfl,czalfn,czalfs)
        call dsmove(vx-rlen/2.0+dx,vy+space+dy)
        call ztext(kztitl,kztiln,czalfl,czalfn,czalfs)
        call gssetc(zzhite,0.0)
      end if
!
!  make sure "pltclp" contains limits
!  picked by mapit.   only maintained
!  for callers info.
!
      if(logxx)then
        xmin = 10.0**xmin
        xmax = 10.0**xmax
        logx = .true.
      end if
      if(logyy)then
        ymin = 10.0**ymin
        ymax = 10.0**ymax
        logy = .true.
      end if
      return
      end
      subroutine zfill(ax1,ay1,ax2,ay2,rxmax,rxmin,rymax,rymin,lblank)
!
!***********************************************************************
!
!! ZFILL checks if a line goes through a blank area. 
!
!  if part(s) of line is blanked, return status.
!
!  input:   x1,y1  = point 1
!  x2,y2  = point 2
!  rxmax,rxmin,rymax,rymin = bounds of blank area
!
!  output:  lblank = logical status flag, 'true' if the line
!  goes through a blank area
!
      logical lblank
!
      save epslon
      data epslon /0.0001/
!
!  if blank area exists, retrieve boundaries
!
      lblank=.false.
      x1=ax1
      y1=ay1
      x2=ax2
      y2=ay2
      dx=x2-x1
      dy=y2-y1
!
!  for vertical lines
!
      if(abs(dx).lt.epslon)then
        if(x1.lt.rxmax.and.x1.gt.rxmax)then
          if((y1.gt.rymin.and.y1.lt.rymin).or.
     &     (y2.gt.rymin.and.y2.lt.rymax))then
              lblank=.true.
              return
            end if
        end if
!
!  for horizontal lines
!
      elseif(abs(dy).lt.epslon)then
        if(y1.lt.rymax.and.y1.gt.rymin)then
          if((x1.gt.rxmin.and.x1.lt.rxmax).or.
     &        (x2.gt.rxmin.and.x2.lt.rxmax))then
              lblank=.true.
              return
        end if
        end if
!
!  for all other lines
!
      else
        ic=0
        call zdraw2('X',x1,y1,x2,y2,rxmin,rymax,rymin,xf,yf,ic)
        call zdraw2('X',x1,y1,x2,y2,rxmax,rymax,rymin,xf,yf,ic)
        call zdraw2('Y',x1,y1,x2,y2,rymin,rxmax,rxmin,xf,yf,ic)
        call zdraw2('Y',x1,y1,x2,y2,rymax,rxmax,rxmin,xf,yf,ic)
        if(ic.eq.0)then
          xmid=(x1+x2)/2.0
          ymid=(y1+y2)/2.0
          if(xmid.gt.rxmin.and.xmid.lt.rxmax.and.
     &        ymid.gt.rymin.and.ymid.lt.rymax)then
            lblank=.true.
            return
          end if
        elseif(ic.eq.1.or.ic.eq.2)then
          lblank=.true.
          return
        end if
      end if
      return
      end
      subroutine zframe(xa,xb,ya,yb,delta)
!
!***********************************************************************
!
!! ZFRAME draws a frame with an offset.
!
!  input:   xa,xb,ya,yb = frame boundaries
!  delta       = offset of frame
!
      parameter (kzsoli=1)
      logical linilt, lposnd
!
      save /dcltyp/
!
      common /dcltyp/ ilntyp, dleft, dist(13,15), linilt, lposnd
!
      x1=xa-delta
      x2=xb+delta
      y1=ya-delta
      y2=yb+delta
      ioline=ilntyp
      ilntyp=kzsoli
      call dsmove(x1,y1)
      call dsdraw(x1,y2)
      call dsdraw(x2,y2)
      call dsdraw(x2,y1)
      call dsdraw(x1,y1)
      ilntyp=ioline
      return
      end
      subroutine zgetfn(cfont,cstyle,ifont)
!
!***********************************************************************
!
!! ZGETFN assigns a font.
!
!  The foint is assigned according to current font style and input 
!  character set.
!
!  input:   cfont  = font type
!  cstyle = character style
!
!  output:  ifont = font id associated with cfont and cstyle
!
      character cfont*(*),cstyle*(*)
      integer lisfnt(40)
!  -cfont-
!  stand itali greek scrip math     -cstyle-
!  default
!
      save lisfnt
!
!  cartog
!  simplx
!  cmplx2
!  complx
!  duplex
!  triplx
!  gothic
!
      data lisfnt /     1,   18,   13,    5,   16,
     &                 11,   18,   13,    5,   16,
     &                 15,   18,   13,    5,   16,
     &                 12,   14,    8,    6,   16,
     &                 12,   14,   17,    6,   16,
     &                  3,   14,   17,    6,   16,
     &                  2,    4,   17,    6,   16,
     &                  9,    7,    8,   10,   16/
!
      if(cstyle(1:5).eq.'CARTO')then
        ii=1
      elseif(cstyle(1:5).eq.'SIMPL')then
        ii=2
      elseif(cstyle(1:5).eq.'CMPLX')then
        ii=3
      elseif(cstyle(1:5).eq.'COMPL')then
        ii=4
      elseif(cstyle(1:5).eq.'DUPLE')then
        ii=5
      elseif(cstyle(1:5).eq.'TRIPL')then
        ii=6
      elseif(cstyle(1:5).eq.'GOTHI')then
        ii=7
      else
        ii=0
      end if

      if(cfont(1:5).eq.'ITALI'.or.cfont(1:5).eq.'L/CIT')then
        jj=2
      elseif(cfont(1:5).eq.'GREEK'.or.cfont(1:5).eq.'L/CGR')then
        jj=3
      elseif(cfont(1:5).eq.'SCRIP'.or.cfont(1:5).eq.'L/CSC')then
        jj=4
      elseif(cfont(1:5).eq.'MATHE')then
        jj=5
      else
        jj=1
      end if
 
      ifont=lisfnt(ii*5+jj)
 
      return
      end
      subroutine zgsclp(area)
!
!***********************************************************************
!
!! ZGSCLP saves the current absolute clipping window and sets a new one.
!
!  The new absolute clipping window is set given virtual coordinates.
!  it makes sure that the clipping window never lies outside the
!  physical device.
!
      real area(4)
!
      save /carea/
      save /gcclip/
      save /gcdchr/
      save /pltsiz/
!
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
!
      area(1) = xcm0
      area(2) = xcm1
      area(3) = ycm0
      area(4) = ycm1
      tvx0=xvstrt-zzgrce
      tvx1=xvstrt+xvlen+zzgrce
      tvy0=yvstrt-zzgrce
      tvy1=yvstrt+yvlen+zzgrce
!
      call gsrst(tvx0,tvy0,ax0,ay0)
      call gsrst(tvx1,tvy1,ax1,ay1)
      xcm0 = max ( min (ax0,ax1),0.0)
      ycm0 = max ( min (ay0,ay1),0.0)
      xcm1 = min (xclipd, max (ax0,ax1))
      ycm1 = min (yclipd, max (ay0,ay1))
      return
      end
      subroutine zin2ch(lstr,cstr)
!
!***********************************************************************
!
!! ZIN2CH converts an integer array to a character array.
!
!  input:   lstr = integer array
!
!  output:  cstr = character array
!
      integer lstr(*)
      character*1 cstr(*)
!
      length=leng(lstr)
 
      do i=1,length+1
        call sybyt4('X',lstr,i,khar)
        cstr(i)=char(khar)
      end do
 
      return
      end
      subroutine zinit
!
!***********************************************************************
!
!! ZINIT initializes all system variables.  
!
!  It should be called immediately after a call to ddevsl that selects 
!  a graphics output device.
!
!  zinit is called at level 0, and moves the graphics state to level 1.
!
      parameter (kzyes=111)
      parameter (kzno=222)
      parameter (zzin=2.54)
      parameter (kzreal=2)
      parameter (kzlin=2)
      parameter (kzsoli=1)
      parameter (kzmxbs=1000)
      parameter (kzmxbl=200)
      parameter (kzmaxc=255)
      character czcolr*8,czalfl*1,czalfn*5,czalfs*5,czlgac*1,
     &          czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5,
     &          czhdac*1,czhdaf*5,czhdas*5
!
      save /carea/
      save /caxis/
      save /cblank/
      save /cborch/
      save /cdevic/
      save /cheadc/
      save /cheadn/
      save /clabel/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cline/
      save /cmess/
      save /cmxalf/
      save /colorc/
      save /colorn/
      save /cpage/
      save /cphysr/
      save /cstrng/
      save /csymbo/
      save /cunit/
      save /gcdchr/
!
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
      common /cblank/ zzblnk(kzmxbl,4),zzblks(kzmxbs,4),kzblcn,kzbscn
      common /cborch/ kzbrdr,kzchek
      common /colorn/ kzshd,zzhspc,zzcolr(kzmaxc,3),kznclr,kzccol
      common /colorc/ czcolr
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /cheadn/ kzhdnl,kzhdmx,kzhdtx(72),zzhdsz(4)
      common /cheadc/ czhdac(4,6),czhdaf(4,6),czhdas(4)
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /clevel/ kzlevl,kzbegn
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /cmess/  zzabux,zzabuy,kzlbeg,kzmov
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cphysr/ zzxor,zzyor,kzor,uuxor,uuyor
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /csymbo/ kzsym,kznsym,zzsmsz,uusmsz
      common /cunit/  zzunit
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
!
!  set level
!
      kzlevl=1
      kzbegn=kzno
!
!  color to white
!
      czcolr='BLACK'
!
!  no hardwareshading.
!
      kzshd=kzno
      kznclr=2
      zzcolr(1,1)=0.0
      zzcolr(1,2)=0.0
      zzcolr(1,3)=100.0
      call gscolr(1,ierr)
!
!  linear real axis, and no axis label suppressed
!
      kzxtyp=kzreal
      kzytyp=kzreal
      kzxnon=kzno
      kzynon=kzno
      kzxtck=1
      kzytck=1
      zzxlba=0.0
      zzylba=90.0
!
!  line style set up, no interpolation and line thickness
!
      kzlncn=4
      kzlsty=kzsoli
      call dsltyp(kzlsty)
      kzltyp=kzlin
      zzlthk=0.01*zzin
      kzlthk=kzno
      zzhspc=real(nfline)/yres*.75
!
!  "unit" set up
!
      zzunit=zzin
!
!  "page" set up
!
      kzauto=kzno
      if(kzpage.ne.kzyes)then
        uupagx=8.5*zzin
        uupagy=11.0*zzin
        zzpagx=uupagx
        zzpagy=uupagy
        zzpagr=1.0
      end if
!
!  symbol parameter set up
!
      kzsym=0
      kznsym=18
      uusmsz=0.08*zzin
      zzsmsz=uusmsz
!
!  "angle" and "chrsiz" and
!  character font type set up
!
      zzangl=0.0
      uuhite=0.14*zzin
      zzhite=uuhite
      call gssetc(uuhite,0.0)
!
!
!  "txtblk" set up
!
      call zscopy('LEGEND',kzlgti)
      kzlgtl=6
      kzlgln=kzno
      kzlgcn=0
      kzlger=kzno
      uulgtz=uuhite
      zzlgtz=-1.0
      zzlgtr=-1.0
!
!  string terminator set up
!
      call wch2in ( '$', kzstrm )
      kztmln=1
      kzhdnl=0
      kzhdmx=0
!
!  abutment of character strings
!
      zzabux=0.0
      zzabuy=0.0
!
!  "margin" set up
!
      uugrce=0.5*zzin
      zzgrce=uugrce
!  "frmwid", "nochek" and "nobord" set up
!
      uufrme=0.03*zzin
      zzfrme=uufrme
      kzchek=kzyes
      kzbrdr=kzyes
!
!  "xlabel", "ylabel" and "headin" set up
!
      kzxnfl=kzno
      kzxaln=0
      kzyaln=0
      kztiln=0
!
!  "origin" set up
!
      kzor=kzno
!
!  "setout" set up
!
      kzsum=6
      kzcopy=kzno
!
!  "blanki" and story height space set up
!
      kzbscn=-1
      kzblcn=4
      zzsrat=1.5
 
      do i=1,kzmxbl
        do j=1,4
          zzblnk(i,j)=-1100.0
        end do
      end do
 
      do i=1,kzmxbs
        do j=1,4
          zzblks(i,j)=-1100.0
        end do
      end do
 
      return
      end
      subroutine zinits
!
!***********************************************************************
!
!! ZINITS initializes all system variables.
!
!  (level 0, set level to 1)
!
      parameter (kzno=222)
      parameter (zzin=2.54)
!
      save /cborch/
      save /cheadc/
      save /cheadn/
      save /clabel/
      save /clevel/
      save /clgndc/
      save /clgndn/
      save /cstrng/
      save /csymbo/
!
      common /clevel/ kzlevl,kzbegn
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /csymbo/ kzsym,kznsym,zzsmsz,uusmsz
      common /cheadn/ kzhdnl,kzhdmx,kzhdtx(72),zzhdsz(4)
      common /cheadc/ czhdac(4,6),czhdaf(4,6),czhdas(4)
      common /cborch/ kzbrdr,kzchek
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      character czhdac*1,czhdaf*5,czhdas*5
      character czlgac*1,czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5
!
      do i=1,50
        kzlgsm(i)=-1
        kzlglt(i)=0
        do k=1,6
          czlgac(i,k)=' '
          czlgaf(i,k)='     '
        end do
        czlgas(i)='     '
      end do
 
      czlgtc(1)=' '
      czlgtf(1)='STAND'
      czlgts='DEFAU'
      do i=2,6
        czlgtc(i)=' '
        czlgtf(i)='     '
      end do
 
      do i=1,4
        do k=1,6
          czhdac(i,k)=' '
          czhdaf(i,k)='     '
        end do
        czhdas(i)='     '
      end do
!
!  set level
!
      kzlevl=1
!
!  symbol parameter set up
!
      kzsym=0
      uusmsz=0.08*zzin
!
!  "txtblk" set up
!
      call zscopy('LEGEND',kzlgti)
      kzlgtl=6
      kzlgln=kzno
      kzlgcn=0
      kzlger=kzno
      uulgtz=uuhite
      zzlgtr=1.5
!
!  string terminator set up
!
      kzhdnl=0
      kzhdmx=0
!
!  "margin" set up
!
      kzbrdr=kzno
!
!  "xlabel", "ylabel" and "headin" set up
!
      kztiln=0
      return
      end
      subroutine zlasal
!
!***********************************************************************
!
!! ZLASAL initializes the QMS laserprinter for landscape output.
!
      save /gcdchr/
      save /gcdsel/
!
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
      common /gcdsel/ idev
!
      save kqms,kln03,kpost
      data kqms,kln03,kpost /1200,3,910/
      kdev=int(devid)
      if(kdev.eq.kqms)then
        idev=3
      elseif(kdev.eq.kln03)then
        idev=10
      elseif(kdev.eq.kpost)then
        idev=14
      end if
 
      if(xlencm.lt.ylencm)then
        call zzswap(xlencm,ylencm)
        call zzswap(xres,yres)
        call zzswap(xclipd,yclipd)
      end if
 
      return
      end
      subroutine zlasap
!
!***********************************************************************
!
!! ZLASAP initializes the QMS printer for portrait output.
!
      save /gcdchr/
      save /gcdsel/
!
      common /gcdchr/ devid, xlencm, ylencm, xres, yres,
     &                ndclrs, idvbts, nfline, xclipd, yclipd
      common /gcdsel/ idev
!
      save kqms,kln03,kpost
      data kqms,kln03,kpost /1200,3,910/
      kdev=int(devid)
      if(kdev.eq.kqms)then
        idev=4
      elseif(kdev.eq.kln03)then
        idev=11
      elseif(kdev.eq.kpost)then
        idev=15
      end if
 
      if(xlencm.gt.ylencm)then
        call zzswap(xlencm,ylencm)
        call zzswap(xres,yres)
        call zzswap(xclipd,yclipd)
      end if
 
      return
      end
      subroutine zlnlab(cflag,rnum,iexp,istrng,lrmtex)
!
!***********************************************************************
!
!! ZLNLAB ...
!
      logical lrmtex
      integer istrng(*)
      integer itemp(20)
      character*1 cflag,strng(8),bminus, bzero(6),ctemp(80)
!
      save bminus
      data bminus /'-'/
      bzero(1)='0'
      bzero(2)='.'
      bzero(3)='0'
      bzero(4)='0'
      bzero(5)='0'
      bzero(6)=char(0)
      lrmtex = .true.
      num=int(rnum)
!
!  work with absolute value as it is easier to put sign in now
!
      if(rnum .ge. 0.0)then
        val = rnum
        nval=num
        istart = 1
      else
        val = -rnum
        nval= -num
        istart = 2
        strng(1) = bminus
      end if
!
      if(cflag.eq.'R')then

        if(iexp .ge. -3 .and. iexp .le. 3)then
          lrmtex = .false.
          val = val*10.0**iexp
        end if

        itemp(1)=1
        call znmstr(val,itemp)
        call zin2ch(itemp,ctemp)
        nchr=nchray(ctemp)
        do k=1,nchr+1
          strng(istart+k-1)=ctemp(k)
        end do
 
      else
        if(iexp .ge. -4 .and. iexp .le. 4) lrmtex = .false.
        if(iexp .gt. 0 .and. (.not. lrmtex))then
          val = val*10.0**iexp
          nval=int(val+0.001)
        end if

        temp=val-real(nval)

        if(abs(temp).gt.0.01)then
          itemp(1)=1
          call znmstr(val,itemp)
        else
          itemp(1)=-1
          call znmstr(val,itemp)
        end if

        call zin2ch(itemp,ctemp)
        nchr=nchray(ctemp)

        do k=1,nchr+1
          strng(istart+k-1)=ctemp(k)
        end do
 
        if((nval .eq. 0) .or. lrmtex .or. (iexp .ge. 0)) go to 800
!
!  number is in range 10**-1 or 10**-2, so format pretty
!
        n = -iexp
        l=nchr
!  l = len(strng(istart))
        izbgn = 1
        nin = 5
        if(n .eq. l) nin = 4
!
!  if n<l then we need only insert a decimal point
!
        if(n .ge. l) go to 40
        izbgn = 2
        nin = 1
40      continue
!
!  allow room for decimal point and zero(s) if necessary
!
        ibegin = istart + max(0,l-n)
        do i = 0, min(n,l)
          strng(istart+l+nin-i) = strng(istart+l-i)
        end do
!
!  insert leading zeros if necessary, or just decimal point
!
        do i=0,nin-1
          strng(ibegin+i) = bzero(izbgn+i)
        end do
!
!  all done
!
800     continue
      end if
      call zch2in(strng,istrng)
      return
      end
      subroutine zloalf(cflag,cfont,cstyle,cflag1,cfont1,csty1)
!
!***********************************************************************
!
!! ZLOALF loads the current mixed alphabets.
!
!  input:   cflag1 = font indicators
!  cfont1 = font setting
!  csty1  = character style
!
!  output:  cflag  = destination font indicators
!  cfont  = destination font setting
!  cstyle = destination character style
!
      character*1 cflag(6),cflag1(6)
      character*5 cfont(6),cstyle,cfont1(6),csty1
!
      do i=1,6
        cflag(i)=cflag1(i)
        cfont(i)=cfont1(i)
      end do
 
      cstyle=csty1
      return
      end
      subroutine zmapit(xl,xh,yl,yh,lxlab,ixlen,lylab,iylen,
     &                  xpos,ypos,iaxes)
!
!***********************************************************************
!
!! ZMAPIT defines the X or Y axis or both in a predefined plot area.
!
!  input:   xl,xh       = range of x axis
!  yl,yh       = range of y axis
!  lxlab,lylab = axis labels
!  ixlen,iylen = respective axis length
!  xpos,ypos   = offset of origin
!  iaxes       = axis flag
!  if bit 0 set, define x axis
!  if bit 1 set, define y axis
!  if bit 2 set, scale x axis
!  if bit 3 set, scale y axis
!  if bit 5 set, draw title of plot
!
      parameter (kzno=222)
      parameter (kzreal=2)
!
      save /carea/
      save /caxis/
      save /ccoord/
      save /clabel/
      save /cmxalf/
      save /cpage/
      save /cstrng/
      save /cunit/
      save /pltclp/
      save /pltcom/
      save /pltprm/
      save /pltsiz/
!
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /caxis/  kzxtyp,kzytyp,kzxnon,kzynon,kzxtck,kzytck,
     &                zzxlba,zzylba
      common /ccoord/ zzxmin,zzxmax,zzxstp,zzymin,zzymax,zzystp
      common /clabel/ kzxlab(20),kzxaln,kzylab(20),kzyaln,kztitl(20),
     &                kztiln,kzxnfl,kzynfl
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /cunit/  zzunit
      common /pltcom/ ux0, udx, uy0, udy, logx, logy
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      common /pltclp/ xmin,xmax,ymin,ymax
      common /pltprm/ cxsize, cysize, tickln, yvini
      character czalfl*1,czalfn*5,czalfs*5
      logical logx, logy
!
      integer numbr(4), numbr1(4), lxlab(20),lylab(20)
      logical lxx, lyy, lhead, lrmtex, lshort, lraggd
      logical lxscal,lyscal,lfirst
!
      save pio180
!
      data pio180 /1.745329252e-2/
!
!  minimum distance between short ticks (1 mm)
!
!  check if x, y axis definition or both
!  also check if title is required
!
      tickln=0.6*zzhite
      lxx=ksyand(iaxes,1).ne.0
      lyy=ksyand(iaxes,2).ne.0
      lhead=ksyand(iaxes,16).ne.0
      lshort= .false.
      lraggd= .false.
!
!  offset according to xpos and ypos
!
      dx=xpos*zzunit*zzpagr
      dy=ypos*zzunit*zzpagr
!
!  define x plot parameters
!
      if(lxx)then
        xlow=xl
        xhigh=xl+(xh-xl)*zzxaxr
!
!  do the axes scaling
!
        lxscal= ksyand(iaxes,4) .ne. 0
        if(lxscal)then
          numtks=min0(10,int(xvlen/((ilabsz()+1.0)*gslens('0'))))
          call axis(xlow,xhigh,numtks,lshort,lraggd,xmin,xmax,
     &                xtmin,xtmax,xtick,ixpwr)
          zzxstp=xtick*10.0**ixpwr
          if((zzxstp*zzxaxs*zzxaxr/(xmax-xmin)).gt.4.5)then
            zzxstp=zzxstp/2.0
            xtick=xtick/2.0
          end if
        else
          call zaxis(xlow,xhigh,zzxstp,xtmin,xtmax,
     &                 xtick,ixpwr)
          xmax=xhigh
          xmin=xlow
        end if
        zzxmin=xmin
        zzxmax=xmax
!
!  set up scaling factors
!
        ux0 = xmin
        udx = xmax - xmin
      end if
!
!  define y plot parameters
!
      if(lyy)then
        ylow=yl
        yhigh=yl+(yh-yl)*zzyaxr
!
!  protect mapprm info this way
!
        yvlen = yvini
!
!  do the axes scaling
!
        lyscal= ksyand(iaxes,8) .ne. 0
        if(lyscal)then
          numtks=min0(10,int(yvlen/((ilabsz()+1.0)*gslens('0'))))
          call axis(ylow,yhigh,numtks,lshort,lraggd,ymin,ymax,
     &                ytmin,ytmax,ytick,iypwr)
          zzystp=ytick*10.0**iypwr
          if((zzystp*zzyaxs*zzyaxr/(ymax-ymin)).gt.4.5)then
            zzystp=zzystp/2.0
            ytick=ytick/2.0
          end if
        else
          call zaxis(ylow,yhigh,zzystp,ytmin,ytmax,
     &                ytick,iypwr)
          ymax=yhigh
          ymin=ylow
        end if
        zzymin=ymin
        zzymax=ymax
!
!  set up scaling factors
!
        uy0 = ymin
        udy = ymax - ymin
      end if
!
!  draw x axis
!
      if(lxx.and.ixlen.ne.0)then
        call gssetc(0.7*zzhite,zzxlba)
!
!  calculate origin values
!
        y = ymin
        tenexp = 10.0**ixpwr
        call scale(xmin,y,vx,vy)
        vxold=vx
        epslon=0.1*xtick*tenexp
!
!  loop to draw and label x axis ticks
!
        cosa=cos(zzxlba*pio180)
        sina=sin(zzxlba*pio180)
        angmax=0.0
        x = xtmin
        lfirst=.true.
!
 100    continue
        call scale(x*tenexp,y,vx,vy)
!
!  draw ticks according to "ymarks"
!
        if(kzxtck.ne.0)then
          call dsmove(vxold+dx,vy+dy)
          call dsdraw(vx+dx,vy+dy)
          call dsdraw(vx+dx,vy-tickln+dy)
          if(kzxtck.lt.0)then
            call errmes('ZMAPIT',kzxtck,4)
          elseif(kzxtck.gt.1)then
            tkincr=(vx-vxold)/kzxtck
            do j=1,kzxtck-1
              call dsmove(vxold+dx+tkincr*j,vy+dy)
              call dsdraw(vxold+dx+tkincr*j,vy-tickln*0.6+dy)
            end do
          end if
        end if
        vxold=vx
!
!  if "noxlbl" is not called
!
        if(kzxnon.eq.kzno)then
          if(kzxtyp.eq.kzreal)then
            call zlnlab('R',x,ixpwr,numbr,lrmtex)
          else
            call zlnlab('I',x,ixpwr,numbr,lrmtex)
          end if
!
!  tilt numberings according to "xangle"
!
          ln=leng(numbr)
          rlen=zxmess(numbr,ln,czalfl,czalfn,czalfs)
          angx=rlen*cosa
          angy=rlen*sina
          if(zzxlba.lt.3.0)then
            angx=angx/2.0
            angy=1.7*zzhite
            if(angmax.lt.angy) angmax=angy
          elseif(zzxlba.lt.270.0)then
            angx=angx-0.35*zzhite
            angy=angy+1.7*zzhite
            if(angmax.lt.angy) angmax=angy
          else
            if(angmax.lt.(1.7*zzhite-angy))
     &            angmax=1.7*zzhite-angy
            angx=0.35*zzhite
            angy=1.7*zzhite
          end if
          if(lfirst)then
            call dsmove(vx+dx,vy-angy+dy)
            lfirst=.false.
          else
            call dsmove(vx-angx+dx,vy-angy+dy)
          end if
          call ztext(numbr,ln,czalfl,czalfn,czalfs)
        else
          angmax=1.7*zzhite
        end if
        x = x + xtick
        xact=x*tenexp-epslon
        if((xact.le.xmax.and.xtick.gt.0.0)
     &        .or.(xact.ge.xmax.and.xtick.lt.0.0)) go to 100
!
!  end loop
!
!  now place remote exponent
!  if needed on linear axis
!
        if(lrmtex.and.(kzxnon.eq.kzno))then
          call gssetc(0.7*zzhite,0.0)
          call scale((x-xtick)*tenexp,ymin,vx,vy)
!w          call zscopy('*10',lexpo)
          call sybyt4('N',lexpo,3,48)
          call sybyt4('N',lexpo,2,49)
          call sybyt4('N',lexpo,1,42)
          numbr1(1)=-1
          call znmstr(real(ixpwr),numbr1)
          rlen=zxmess(lexpo,3,czalfl,czalfn,czalfs)*0.9/1.4
          call dsmove(vx+rlen+dx,vy-angmax-1.2*zzhite+dy)
          ln=leng(numbr1)
          call ztext(numbr1,ln,czalfl,czalfn,czalfs)
          call gssetc(zzhite*0.9,0.0)
          call dsmove(vx-rlen+dx,vy-angmax-1.7*zzhite+dy)
          call ztext(lexpo,3,czalfl,czalfn,czalfs)
        end if
!
!  now place x axis label
!
        call gssetc(zzhite,0.0)
        call scale((xmin+xmax)/2.0,ymin,vx,vy)
        rlen=zxmess(lxlab,ixlen,czalfl,czalfn,czalfs)
        call dsmove(vx-rlen/2.0+dx,
     &                vy-angmax-1.7*zzhite+dy)
        call ztext(lxlab,ixlen,czalfl,czalfn,czalfs)
        if(kzxnfl.eq.kzno) kzxaln=0
      end if
!
!  ********** draw y axes **********
!
      if(lyy.and.iylen.ne.0)then
        call gssetc(0.7*zzhite,zzylba)
!
!  draw y axis line
!
        x = xmin
        tenexp = 10.0**iypwr
        call scale(x,ymin,vx,vy)
        vyold=vy
        epslon=0.1*ytick*tenexp
!
!  loop to draw and label y axis
!
        cosa=cos(zzylba*pio180)
        sina=sin(zzylba*pio180)
        angmax=0.0
        y = ytmin
        lfirst=.true.
!
 300    continue
        call scale(x,y*tenexp,vx,vy)
!
!  draw ticks according to "ymarks"
!
        if(kzytck.ne.0)then
          call dsmove(vx+dx,vyold+dy)
          call dsdraw(vx+dx,vy+dy)
          call dsdraw(vx+dx-tickln,vy+dy)
          if(kzytck.lt.0)then
            call errmes('ZMAPIT',kzytck,5)
          elseif(kzytck.gt.1)then
            tkincr=(vy-vyold)/kzytck
            do j=1,kzytck-1
              call dsmove(vx+dx,vyold+tkincr*j+dy)
              call dsdraw(vx+dx-tickln*0.6,vyold+tkincr*j+dy)
            end do
 
          end if
        end if
        vyold=vy
!
!  if "noylbl" is not called
!
        if(kzynon.eq.kzno)then
          if(kzytyp.eq.kzreal)then
            call zlnlab('R',y,iypwr,numbr,lrmtex)
          else
            call zlnlab('I',y,iypwr,numbr,lrmtex)
          end if
!
!  tilt numberings according to "yangle"
!
          ln=leng(numbr)
          rlen=zxmess(numbr,ln,czalfl,czalfn,czalfs)
          angx=rlen*cosa+zzhite
          angy=rlen*sina
          if(lfirst)then
            if(zzylba.lt.270.0)then
              call dsmove(vx-angx+dx,vy+dy)
            else
              call dsmove(vx-angx+dx,vy-angy+dy)
            end if
            lfirst=.false.
          else
            if(zzylba.lt.93.0.and.zzylba.gt.87.0)then
              call dsmove(vx-angx+dx,vy-rlen/2.0+dy)
            elseif(zzylba.lt.270)then
              call dsmove(vx-angx+dx,vy-angy-0.35*zzhite+dy)
            else
              call dsmove(vx-angx+dx,vy-angy+0.35*zzhite+dy)
            end if
          end if
          if(angmax.lt.angx) angmax=angx
          call ztext(numbr,ln,czalfl,czalfn,czalfs)
        else
          angmax=zzhite
        end if
        y = y + ytick
        yact=y*tenexp-epslon
        if((yact.le.ymax.and.ytick.gt.0.0)
     &         .or.(yact.ge.ymax.and.ytick.lt.0.0)) go to 300
!
!  end loop
!
!  if linear axis, place remote
!  exponent if needed
!
        if(lrmtex.and.(kzynon.eq.kzno))then
          call gssetc(0.7*zzhite,90.0)
          call scale(xmin,(y-ytick)*tenexp,vx,vy)
          numbr1(1)=-1
          call znmstr(real(iypwr),numbr1)
!w          call zscopy('*10',lexpo)
          call sybyt4('N',lexpo,3,48)
          call sybyt4('N',lexpo,2,49)
          call sybyt4('N',lexpo,1,42)
          rlen=zxmess(lexpo,3,czalfl,czalfn,czalfs)*0.9/1.4
          call dsmove(vx-angmax-1.7*zzhite+dx,vy+rlen+dy)
          ln=leng(numbr1)
          call ztext(numbr1,ln,czalfl,czalfn,czalfs)
          call gssetc(0.9*zzhite,90.0)
          call dsmove(vx-angmax-1.2*zzhite+dx,vy-rlen+dy)
          call ztext(lexpo,3,czalfl,czalfn,czalfs)
        end if
!
!  now place y label
!
        call gssetc(zzhite,90.0)
        call scale(xmin,(ymin+ymax)/2.0,vx,vy)
        rlen=zxmess(lylab,iylen,czalfl,czalfn,czalfs)
        call dsmove(vx-angmax-1.4*zzhite+dx,vy-rlen/2.0+dy)
        call ztext(lylab,iylen,czalfl,czalfn,czalfs)
        call gssetc(zzhite,0.0)
        if(kzynfl.eq.kzno) kzyaln=0
      end if
!
!  ********** place title **********
!
      if(lhead.and.kztiln.ne.0)then
        call gssetc(1.46*zzhite,0.0)
        space= max (zzhite,0.1*yvlen)
        call scale((xmin+xmax)/2.0,ymax,vx,vy)
        rlen=zxmess(kztitl,kztiln,czalfl,czalfn,czalfs)
        call dsmove(vx-rlen/2.0+dx,vy+space+dy)
        call ztext(kztitl,kztiln,czalfl,czalfn,czalfs)
        call gssetc(zzhite,0.0)
      end if
      return
      end
      subroutine zmaprm(xleft,xright,ybot,ytop,csize,tkln)
!
!***********************************************************************
!
!! ZMAPRM...
!
      save /cmxalf/
      save /pltprm/
      save /pltsiz/
!
      common /pltsiz/ xvstrt, yvstrt, xvlen, yvlen
      common /pltprm/ cxsize, cysize, tickln, yvini
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
!
      character czalfl*1,czalfn*5,czalfs*5
!
      call gssetc(csize,0.0)
      cxsize = zxmess('0',1,czalfl,czalfn,czalfs)
      cysize = csize
      tickln = tkln
      xvstrt=xleft
      xvlen=xright-xleft
      yvstrt=ybot
      yvini=ytop-ybot
      return
      end
      subroutine znmstr(val,bstrng)
!
!***********************************************************************
!
!! ZNMSTR converts a real number to a string with no leading spaces.
!
!  input:   val = input value
!
!  output:  bstrng = output string
!
      parameter (numbyt=4)
      parameter (nmwrd=16/numbyt)
!
      integer bstrng(*)
      character str*(numbyt*nmwrd)
      integer number(nmwrd)
!
      if(bstrng(1).ge.0)then

        if(val.lt.1.0)then
          write(str,'(f11.4)')val
        elseif(val.lt.10.0)then
          write(str,'(f11.3)')val
        elseif(val.lt.100.0)then
          write(str,'(f11.2)')val
        else
          write(str,'(f11.1)')val
        end if

        str(12:12) = char(0)
        k=11
 100    continue
        if(str(k:k).eq.'0'.and.str(k-1:k-1).ne.'.')then
          str(k:k)=char(0)
          k=k-1
          if(k.ge.1) go to 100
        else
          go to 200
        end if
 200    continue
      else
        write(str,'(i11)')nint(val)
        str(12:12) = char(0)
      end if
 
      do i=1,nmwrd
        call wch2in(str((i-1)*numbyt+1:i*numbyt),number(i))
      end do
 
      call zstrbl(number)
      call zscopy(number,bstrng)
      return
      end
      subroutine zpnt(x,y,z,nz)
!
!***********************************************************************
!
!! ZPNT...
!
      dimension z(nz,2)
!
      save /contr/
!
      common /contr/ clevel,iold,jold,in,jn,
     &   nx,ny,xl,dx,yl,dy
!
      a=z(in,jn)-z(iold,jold)
!
!  if no change in z's, pick old point so as to stay to right
!
      if(a .ne. 0.0) then
        a=(clevel-z(iold,jold))/a
      end if

      x=a*(in-iold)+iold
      y=a*(jn-jold)+jold
!
!  now convert indexs to x,y values
!
      x=(x-1.0)*dx/(nx-1)+xl
      y=(y-1.0)*dy/(ny-1)+yl
      return
      end
      subroutine zppoly(y1,y2,y3,y4,a,b,c,d)
!
!***********************************************************************
!
!! ZPPOLY performs parametric cubic polynomial interpolation.
!
      a=( -1.0*y1 + 3.0*y2 -3.0*y3 +    y4)/6.0
      b=(  2.0*y1 - 5.0*y2 +4.0*y3 -    y4)/2.0
      c=(-11.0*y1 +18.0*y2 -9.0*y3 +2.0*y4)/6.0
      d=       y1

      return
      end
      subroutine zreal(tnum,iplace,lmess,lpower)
!
!***********************************************************************
!
!! ZREAL converts a real number to a character string.
!
!  input:   tnum   = input real number
!  iplace = format flag
!  if > 100.0 then fixed length format
!  if < 100.0 and > 0.0 then floating point
!  format with iplace decimal places
!  if < 0.0 then exponential number with
!  -iplace decimal point
!
!  output:  lmess = character string of tnum
!  lpower = character string of power, if any
!
      parameter (numbyt=4)
      parameter (numwrd=80/numbyt)
      integer lmess(*),lmessa(numwrd),lmessb(numwrd),lpow(8/numbyt)
      character cmessa*80,cmessb*80,cpower*8
!
      save epslon

      data epslon /1.0e-20/
!
      lpower=0
      rnum=abs(tnum)
      npow = 0
!
!  calculate power of input value
!
      if(rnum.le.epslon) go to 600
      if(rnum .lt. 0.9999995)then
 200    continue
        rnum = rnum*10.0
        npow = npow-1
        if(rnum .lt. 0.9999995) go to 200
      elseif(rnum.gt.9.999995)then
 100    continue
        rnum = rnum/10.0
        npow = npow+1
        if(rnum.gt.9.999995) go to 100
      end if
!
!  roundoff input value
!
 600  continue
      if(rnum.lt.0.0000005)then
        rnum=0.0
      else
        rnum=rnum+0.0000005
      end if
      anum=rnum*10.0**npow
      if(tnum.lt.0.0)then
        cmessb(1:1)='-'
      else
        cmessb(1:1)=' '
      end if
!
!  fixed length format
!
      if(iplace.gt.100)then
        ip=iplace-100
        ip1=ip+1
        ip2=ip+2
        ip3=ip+3
        ip5=ip+5
        ip6=ip+6
        num=10
!
!  floating point number of fixed length
!
        if(abs(npow).lt.ip)then
          write(cmessa,10)anum
 10       format(1x,f79.39)
 
          do i=1,numwrd
            call wch2in(cmessa((i-1)*numbyt+1:i*numbyt),lmessa(i))
          end do
 
          call zstrbl(lmessa)
 
          do i=1,numwrd
            call win2ch(lmessa(i),cmessa((i-1)*numbyt+1:i*numbyt))
          end do
 
          cmessb(2:ip2)=cmessa(1:ip1)
          cmessb(ip3:ip3)=char(0)
          if(npow.ge.7) num=9
          if(ip2.ge.num)then
            do i=ip2,num,-1
              if(cmessb(i:i).ne.'.') cmessb(i:i)='0'
            end do
          end if
 
          do i=1,numwrd
            call wch2in(cmessb((i-1)*numbyt+1:i*numbyt),lmessb(i))
          end do
          call zscopy(lmessb,lmess)
!
!  number with exponetial in fixed format
!
        else
 
          write(cmessa,10)rnum
 
          do i=1,numwrd
            call wch2in(cmessa((i-1)*numbyt+1:i*numbyt),lmessa(i))
          end do
 
          call zstrbl(lmessa)
 
          do i=1,numwrd
            call win2ch(lmessa(i),cmessa((i-1)*numbyt+1:i*numbyt))
          end do
 
          cmessb(2:ip2)=cmessa(1:ip1)
          cmessb(ip3:ip5)='*10'
          cmessb(ip6:ip6)=char(0)
          if(ip2.ge.num)then
            do i=ip2,num,-1
              if(cmessb(i:i).ne.'.') cmessb(i:i)='0'
            end do
          end if
 
          do i=1,numwrd
            call wch2in(cmessb((i-1)*numbyt+1:i*numbyt),lmessb(i))
          end do
 
          call zscopy(lmessb,lmess)
          write(cpower,20)npow
 20       format(1x,i3)
 
          do i=1,8/numbyt
            call wch2in(cpower((i-1)*numbyt+1:i*numbyt),lpow(i))
          end do
 
          call zstrbl(lpow)
 
          do i=1,8/numbyt
            call win2ch(lpow(i),cpower((i-1)*numbyt+1:i*numbyt))
          end do
 
          cpower(4:4)=char(0)
 
          do i=1,8/numbyt
            call wch2in(cpower((i-1)*numbyt+1:i*numbyt),lpow(i))
          end do
 
          call zstrbt(lpow)
          call zscopy(lpow,lpower)
        end if
!
!  floating point number
!
      elseif(iplace.ge.0)then
        write(cmessa,10)anum
 
        do i=1,numwrd
          call wch2in(cmessa((i-1)*numbyt+1:i*numbyt),lmessa(i))
        end do
 
        call zstrbl(lmessa)
 
        do i=1,numwrd
          call win2ch(lmessa(i),cmessa((i-1)*numbyt+1:i*numbyt))
        end do
 
        do j=1,40
          if(cmessa(j:j).eq.'.') go to 500
        end do
 
 500    continue
 
        ip=iplace+j+1
        cmessb(2:ip)=cmessa(1:ip-1)
        cmessb(ip+1:ip+1)=char(0)
 
        if(npow.ge.7)then
          num=9
        else
          num=10
        end if
 
        if(npow.le.0)then
          ip=ip+npow
          ioff=-npow
        else
          ioff=0
        end if
 
        if(ip.ge.num)then
          do i=ip+ioff,num+ioff,-1
            if(cmessb(i:i).ne.'.') cmessb(i:i)='0'
          end do
        end if
 
        do i=1,numwrd
          call wch2in(cmessb((i-1)*numbyt+1:i*numbyt),lmessb(i))
        end do
 
        call zscopy(lmessb,lmess)
!
!  number with exponential
!
      else
        ip=-iplace
        write(cmessa,10)rnum
 
        do i=1,numwrd
          call wch2in(cmessa((i-1)*numbyt+1:i*numbyt),lmessa(i))
        end do
 
        call zstrbl(lmessa)
 
        do i=1,numwrd
          call win2ch(lmessa(i),cmessa((i-1)*numbyt+1:i*numbyt))
        end do
 
        do j=1,40
          if(cmessa(j:j).eq.'.') go to 800
        end do
 
 800    continue
        ip=ip+j+1
        cmessb(2:ip)=cmessa(1:ip-1)
        cmessb(ip+1:ip+3)='*10'
        cmessb(ip+4:ip+4)=char(0)
 
        if(ip.ge.10)then
          do i=ip,10,-1
            if(cmessb(i:i).ne.'.')cmessb(i:i)='0'
          end do
        end if
 
        do i=1,numwrd
          call wch2in(cmessb((i-1)*numbyt+1:i*numbyt),lmessb(i))
        end do
 
        call zscopy(lmessb,lmess)
        write(cpower,20)npow
 
        do i=1,8/numbyt
          call wch2in(cpower((i-1)*numbyt+1:i*numbyt),lpow(i))
        end do
 
        call zstrbl(lpow)
        cpower(4:4)=char(0)
 
        do i=1,8/numbyt
          call wch2in(cpower((i-1)*numbyt+1:i*numbyt),lpow(i))
        end do
 
        call zstrbt(lpow)
        call zscopy(lpow,lpower)
 
      end if
 
      return
      end
      subroutine zscopy(kin,kout)
!
!***********************************************************************
!
!! ZSCOPY copies a string.
!
!  input:   kin = original string
!
!  output:  kout = output string
!
!----------------------------begin 32-bit specific code-------------------
      parameter (numbyt=4)
!-----------------------------end 32-bit specific code--------------------
!----------------------------begin 64-bit specific code-------------------
!  parameter (numbyt=8)
!-----------------------------end 64-bit specific code--------------------
      dimension kin(*)
      dimension kout(*)
!
      last=leng(kin)
!
      num=last/numbyt
 
      do i=1,num
        kout(i)=kin(i)
      end do
 
      iend=mod(last,numbyt)
      num=num+1
 
      do i=1,iend
        call sybyt4('X',kin(num),i,nchar)
        call sybyt4('N',kout(num),i,nchar)
      end do
 
      call sybyt4('N',kout(num),iend+1,0)
 
      return
      end
      subroutine zstop
!
!***********************************************************************
!
!! ZSTOP stops for fatal errors.
!
      save /clevel/
!
      common /clevel/ kzlevl,kzbegn
!
      write(*,*)' '
      write(*,*)'ZSTOP - Fatal error!'
      write(*,*)'  Stopped at level ',kzlevl

      return
      end
      subroutine zstrbl(istr)
!
!***********************************************************************
!
!! ZSTRBL strips leading blanks in a string.
!
!  input:   istr = input string
!
!
!  character cmess*152
!  dimension istr(*),imessa(38)
!  equivalence (imessa,cmess)
!-------------------------begin 32-bit specific code----------------------
      parameter (numbyt=4)
!--------------------------end 32-bit specific code-----------------------
!-------------------------begin 64-bit specific code----------------------
!  parameter (numbyt=8)
!--------------------------end 64-bit specific code-----------------------
      character cmess*160
      dimension istr(*),imessa(160/numbyt)
!
      length=leng(istr)
      call zscopy(istr,imessa)
!
      do i=1,(length-1)/numbyt+1
        call win2ch(imessa(i),cmess((i-1)*numbyt+1:i*numbyt))
      end do
 
      i=1
 1    continue
      if(i.ge.length.or.cmess(i:i).ne.' ')then
        do j=i,length+1
          k=j-i+1
          cmess(k:k)=cmess(j:j)
        end do
!
! null out the remainder of the string that may have been moved
! and then convert character string back to integer array
!
        do i=k+1,length
          cmess(i:i)=char(0)
        end do
 
        do i=1,(length-1)/numbyt+1
          call wch2in(cmess((i-1)*numbyt+1:i*numbyt),imessa(i))
        end do
 
        call zscopy(imessa,istr)
        return
      else
        i=i+1
      end if
      go to 1
      end
      subroutine zstrbt(lmess)
!
!***********************************************************************
!
!! ZSTRBT strips trailing blanks.
!
!  input:   lmess = input string
!
!-------------------------begin 32-bit specific code----------------------
      parameter (numbyt=4)
!--------------------------end 32-bit specific code-----------------------
!-------------------------begin 64-bit specific code----------------------
!  parameter (numbyt=8)
!--------------------------end 64-bit specific code-----------------------
      dimension lmess(*)
      imess=leng(lmess)
!
      indx=(imess-1)/numbyt+1
      ipos=imess-numbyt*(indx-1)
!
      do i=ipos,1,-1
        call sybyt4('X',lmess(indx),i,khar)
        if(khar.eq.32)then
          call sybyt4('N',lmess(indx),i,0)
        else
          return
        end if
      end do
 
      do i=indx-1,1,-1
        do j=numbyt,1,-1
          call sybyt4('X',lmess(i),j,khar)
          if(khar.eq.32)then
            call sybyt4('N',lmess(i),j,0)
          else
            return
          end if
        end do
      end do
 
      return
      end
      subroutine zzswap(a,b)
!
!***********************************************************************
!
!! ZZSWAP swaps two real numbers.
!
!  input:   a,b      numbers to be swapped
!
!  this routine was originally named "zswap", but was
!  renamed "zzswap" to avoid a name conflict on the vax.
!
      real a
      real b
      real t
!
      t=a
      a=b
      b=t
 
      return
      end
      subroutine ztext(lmess,imess,cflag,cfont,cstyle)
!
!***********************************************************************
!
!! ZTEXT writes a graphic text string in mixed alphabets.
!
!  input:   lmess  = input string
!  imess  = number of characters in lmes
!  cflag  = font indicators
!  cfont  = font setting
!  cstyle = character style
!
      save /cstrng/
!
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
!
      integer lmess1(40),lmess(*),kfont(6)
      character*1 ch(160),buf(160),cflag(6)
      character*5 cfont(6),cstyle
      logical finis,first
!
!  mix alphabet not active, write string
!
      if(cflag(1).eq.' ')then
        call zgetfn(cfont(1),cstyle,kfont(1))
        call gsfont(kfont(1),ierr)
        call zcostr(lmess,imess,lmess1,imess1)
        call dspstr(lmess1)
!
!  process using flags set by mix
!  alphabet routines
!
      else
        do i=1,6
          call zgetfn(cfont(i),cstyle,kfont(i))
        end do
 
        call zin2ch(lmess,ch)
        iptr=1
        icur=1
        nbuf=0
        first=.true.
        call gsfont(kfont(1),ierr)
        do i=1,imess
!
!  look for string terminator
!
          if(imess.eq.100)then
            finis=.true.
            do j=1,kztmln
              call sybyt4('X',kzstrm,j,khar)
              finis=finis.and.(ichar(ch(i+j-1)).eq.khar)
            end do
!
!  if found, empty buffer and return
!
            if(iptr.eq.1.and.finis)then
                if(nbuf.gt.0)then
                buf(nbuf+1)=char(0)
                call zch2in(buf,lmess1)
                call dspstr(lmess1)
                end if
              return
            end if
          end if
!
!  look for mixalf flags
!
 300          continue
          if(ch(i).eq.cflag(icur).and.ch(i).ne.' '.and.
     &          (icur.ne.iptr.or.first))then
            if(nbuf.gt.0)then
              buf(nbuf+1)=char(0)
              call zch2in(buf,lmess1)
              call dspstr(lmess1)
              nbuf=0
            end if
            call gsfont(kfont(icur),ierr)
            first=.false.
            iptr=icur
            icur=icur+1
            if(icur.gt.6) icur=1
            go to 200
          else
            icur=icur+1
            if(icur.gt.6) icur=1
            if(icur.ne.iptr) go to 300
!
!  save text in buffer
!
            nbuf=nbuf+1
            buf(nbuf)=ch(i)
          end if
 200      continue
        end do
!
!  end while
!
        if(nbuf.ne.0)then
          buf(nbuf+1)=char(0)
          call zch2in(buf,lmess1)
          call dspstr(lmess1)
        end if
      end if
      return
      end
      subroutine zvectr(vxfrom,vyfrom,vxto,vyto,ivec)
!
!***********************************************************************
!
!! ZVECTR draws a vector with or without an arrow head.
!
!  input:   vxfrom,vyfrom = first point of vector
!  vxto,vyto     = second point of vector
!  ivec(wxyz)    = arrow type and size flag
!
      save /cline/
      save /cstrng/
      save /gcclip/
!
      common /cline/  kzlsty,kzltyp,zzlthk,kzlthk,zzprx,zzpry,
     &                zzprx1,zzprx2,zzpry1,zzpry2,uulthk,kzlncn
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
      common /gcclip/ xcm0, xcm1, ycm0, ycm1
!
      dimension x(4),y(4),tx(4),ty(4)
!
      save epslon,runit
      data epslon /0.0001/
      data runit /0.12/
      iflag1=mod(ivec,10)
      iflag2=mod(ivec/10,10)
      iflag3=mod(ivec/100,10)
      iflag4=ivec/1000
!
!  if arrow head desired
!
      if(iflag1.ne.0)then
!
!  set arrow size
!
        if(iflag3.eq.0)then
          rlen=zzhite
        else
          rlen=real(iflag3)*runit
        end if
!
!  set cross line length
!
        cross=real(iflag4+1)*rlen/6.0
        dx=vxto-vxfrom
        dy=vyto-vyfrom
!
!  locate vertices of arrowhead
!
        if(abs(dx).lt.epslon)then
          xm=0.0
          ym=rlen*dy/abs(dy)
          fx2=cross
          fx3=-cross
          fy2=0.0
          fy3=0.0
        else
          tanval=dy/dx
          theta=atan(tanval)
          if(dx.lt.0.0)then
            cost=-cos(theta)
            sint=-sin(theta)
          else
            cost=cos(theta)
            sint=sin(theta)
          end if
          ym=sint*rlen
          xm=cost*rlen
          if(dy.lt.0.0)then
            dely=-cross*cost
            delx=cross*sint
          else
            dely=cross*cost
            delx=-cross*sint
          end if
          fx2=delx
          fx3=-delx
          fy2=dely
          fy3=-dely
        end if
        x(1)=vxto-xm+fx2
        x(2)=vxto
        x(3)=vxto-xm+fx3
        x(4)=x(1)
        y(1)=vyto-ym+fy2
        y(2)=vyto
        y(3)=vyto-ym+fy3
        y(4)=y(1)
        call zdispt(vxto,vyto,vxfrom,vyfrom,rlen,xp,yp)
        numpt=4
!
!  find endpoints of vector
!
        if(iflag2.eq.1)then
          if(iflag1.eq.1)then
            call dsmove(vxfrom,vyfrom)
          else
            call dsmove(xp,yp)
          end if
          call dsdraw(vxto-xm,vyto-ym)
        else
          if(iflag2.eq.2) numpt=3
          call dsmove(vxfrom,vyfrom)
          call dsdraw(vxto,vyto)
        end if
!
!  draw arrowhead
!
        call dsltyp(1)
        if(iflag2.eq.0)then
          call dsfill(x,y,3,tx,ty)
        else
          call dsmove(x(1),y(1))
          do j=2,numpt
            call dsdraw(x(j),y(j))
          end do
 
        end if
        call dsltyp(kzlsty)
!
!  if second arrowhead desired,
!  locate vertices
!
        if(iflag1.eq.2.or.iflag1.eq.3)then
          if(iflag1.eq.2)then
            xx=xp
            yy=yp
            xtip=vxfrom
            ytip=vyfrom
          else
            xx=vxfrom
            yy=vyfrom
            xtip=xp
            ytip=yp
          end if
          x(1)=xx+fx2
          x(2)=xtip
          x(3)=xx+fx3
          x(4)=x(1)
          y(1)=yy+fy2
          y(2)=ytip
          y(3)=yy+fy3
          y(4)=y(1)
!
!  draw second arrowhead
!
          call dsltyp(1)
          if(iflag2.eq.0)then
            call dsfill(x,y,3,tx,ty)
          else
            call dsmove(x(1),y(1))
            do j=2,numpt
              call dsdraw(x(j),y(j))
            end do
          end if
          call dsltyp(kzlsty)
        end if
!
!  if vector only, draw vector
!
      else
        call dsmove(vxfrom,vyfrom)
        call dsdraw(vxto,vyto)
      end if
      return
      end
      function zxmess(lmess,imess,cflag,cfont,cstyle)
!
!***********************************************************************
!
!! ZXMESS writes a graphic text string in mixed alphabets.
!
!  input:      lmess      input string
!  imess      number of characters in lmess
!  cflag      font indicators
!  cfont      font setting
!  cstyle      character style
!
      save /cstrng/
!
      common /cstrng/ zzhite,uuhite,zzangl,kzstrm,kztmln
!
      integer lmess1(40)
      integer lmess(*)
      integer kfont(6)
      character*1 ch(160),buf(160),cflag(6)
      character*5 cfont(6),cstyle
      logical finis,first
!
!  mix alphabet not active, write string
!
      if(cflag(1).eq.' ')then
        call zgetfn(cfont(1),cstyle,kfont(1))
        call gsfont(kfont(1),ierr)
        imess1=160
        call zcostr(lmess,imess,lmess1,imess1)
        zxmess=dslens(lmess1)
!
!  process using flags set by mix
!  alphabet routines
!
      else
        do i=1,6
          call zgetfn(cfont(i),cstyle,kfont(i))
        end do
 
        call zin2ch(lmess,ch)
        iptr=1
        icur=1
        nbuf=0
        first=.true.
        zxmess=0.0
        call gsfont(kfont(1),ierr)
        do i=1,imess
!
!  look for string terminator
!
          if(imess.eq.100)then
            finis=.true.
 
            do j=1,kztmln
              call sybyt4('X',kzstrm,j,khar)
              finis=finis.and.(ichar(ch(i+j-1)).eq.khar)
            end do
!
!  if found, empty buffer and return
!
            if(iptr.eq.1.and.finis)then
                if(nbuf.gt.0)then
                buf(nbuf+1)=char(0)
                call zch2in(buf,lmess1)
                zxmess=zxmess+dslens(lmess1)
                end if
              return
            end if
          end if
!
!  look for mixalf flags
!
 300      continue
          if(ch(i).eq.cflag(icur).and.ch(i).ne.' '.and.
     &          (icur.ne.iptr.or.first))then
            if(nbuf.gt.0)then
              buf(nbuf+1)=char(0)
              call zch2in(buf,lmess1)
              zxmess=zxmess+dslens(lmess1)
              nbuf=0
            end if
            call gsfont(kfont(icur),ierr)
            first=.false.
            iptr=icur
            icur=icur+1
            if(icur.gt.6) icur=1
            go to 200
          else
            icur=icur+1
            if(icur.gt.6) icur=1
            if(icur.ne.iptr) go to 300
!
!  save text in buffer
!
            nbuf=nbuf+1
            buf(nbuf)=ch(i)
          end if
 200      continue
        end do
!
!  end while
!
        if(nbuf.ne.0)then
          buf(nbuf+1)=char(0)
          call zch2in(buf,lmess1)
          zxmess=zxmess+dslens(lmess1)
        end if
      end if
      return
      end
      block data inidat
!
!***********************************************************************
!
!  data initialization
!
      parameter (kzno=222)
      parameter (kzyes=111)
      parameter (kzdown=13)
!-------------------------begin 32-bit specific code----------------------
      integer numbyt
      parameter (numbyt=4)
!--------------------------end 32-bit specific code-----------------------
!-------------------------begin 64-bit specific code----------------------
!  parameter (numbyt=8)
!--------------------------end 64-bit specific code-----------------------
      character czalfl*1,czalfn*5,czalfs*5,czlgac*1,
     &          czlgaf*5,czlgas*5,czlgtc*1,czlgtf*5,czlgts*5,
     &          czhdac*1,czhdaf*5,czhdas*5
!
      save /carea/
      save /cdevic/
      save /cheadn/
      save /cheadc/
      save /ciunit/
      save /clgndn/
      save /clgndc/
      save /cmxalf/
      save /cpage/
!
      common /carea/  zzxaxs,zzyaxs,zzxaxr,zzyaxr,uuxaxs,uuyaxs,
     &                zzxlft,zzxrgt,zzybot,zzytop,kzscal,
     &                zzgrce,uugrce,zzfrme,uufrme
      common /cdevic/ kzsum,kzcopy,kzbyte,kznplt
      common /cheadn/ kzhdnl,kzhdmx,kzhdtx(72),zzhdsz(4)
      common /cheadc/ czhdac(4,6),czhdaf(4,6),czhdas(4)
      common /ciunit/ kziunt,kzount
      common /clgndn/ kzlgln,kzlgcn,kzlger,kzlgsm(50),kzlglt(50),
     &                kzlgti(5),kzlgtl,zzlgth(50),zzlgtz,uulgtz,
     &                zzlgtr,zzlgyl,kzlgbl,zzsrat,kzlgen(50)
      common /clgndc/ czlgac(50,6),czlgaf(50,6),czlgas(50),czlgtc(6),
     &                czlgtf(6),czlgts
      common /cmxalf/ czalfl(6),czalfn(6),czalfs
      common /cpage/  zzpagx,zzpagy,zzpagr,uupagx,uupagy,kzpage,kzauto
!
      data kzbyte,kznplt,kzpage,kzscal /numbyt,0,kzno,kzdown/
      data kziunt,kzount /5,6/
      data kzlgsm,kzlglt/50*-1,50*0/
      data czlgtc,czlgtf,czlgts /6*' ','STAND',5*'     ',
     &                             'DEFAU'/
      data czlgac,czlgaf,czlgas /300*' ',350*'     '/
      data kzlgen /50*kzyes/
      data czhdac,czhdaf,czhdas /24*' ',28*'     '/
      data czalfl,czalfn,czalfs /6*' ','STAND',5*'     ','DEFAU'/
      end
      block data blksys
!
!***********************************************************************
!
!  init character mask for 'zbyte4'
!
!  - return mask for 'zbyte4'
!  calling sequence: call symask(mask,npos)
!  input: mask - mask to zero out byte of interest
!  npos - position of byte of interest
!  this routine provides communication between integer
!  and character. data returned is tested good for
!  vax/vms and ibm pc.
!
      save /cbyte4/
!
      common /cbyte4/ kzmask(8),kzcpos(8)
!4x9bitsreverse      data kzmask/33357439,17045716607,
!4x9bitsreverse     .       17078943871,17079008768,0,0,0,0/
!4x9bitsreverse      data kzcpos/134217728,262144,512,1,0,0,0,0/
!
!-------------------------begin vax vms specific code---------------------
      data kzmask/2139062016,2139029631,2130739071,8355711,4*0/
      data kzcpos/1,256,65536,16777216,4*0/
!--------------------------end vax vms specific code----------------------
!--------------------------begin cray specific code-----------------------
!  data kzmask/1777777777777777777400b,1777777777777777600377b,
!  .            1777777777777700177777b,1777777777740077777777b,
!  .            1777777760037777777777b,1777770017777777777777b,
!  .            1774007777777777777777b,0003777777777777777777b/
!  data kzcpos/8*0/
!---------------------------end cray specific code------------------------
!-----------------------------begin generic code--------------------------
!  data kzmask/2139062016,2139029631,2130739071,8355711,4*0/
!  data kzcpos/1,256,65536,16777216,4*0/
!------------------------------end generic code---------------------------
      end
      block data blkdat
!
!***********************************************************************
!
!  this blockdata routine initializes the named common blocks
!
      integer maxfnt
      parameter (maxfnt = 18)
!
      integer mxstrk
      parameter (mxstrk = maxfnt*9000)
!
      integer numz
      parameter (numz = maxfnt-1)
!
      integer bwidth
      integer bxy
!
      save /gccpar/
      save /gcfont/
!
      common /gcfont/ icfnsl, mxslot,
     &   islfnt(maxfnt), ihight(maxfnt),
     &   indx(95*maxfnt+1), bwidth(95*maxfnt), bxy(mxstrk)
      common /gccpar/ csize, ccos, csin
!
      data icfnsl /1/
      data mxslot /1/
      data islfnt /1,numz*0/
      data ihight(1) /8/
!
       data indx(  1),indx(  2),indx(  3),indx(  4)/   1,  17,  27,  47/
       data indx(  5),indx(  6),indx(  7),indx(  8)/  73, 100, 125, 138/
       data indx(  9),indx( 10),indx( 11),indx( 12)/ 147, 156, 171, 181/
       data indx( 13),indx( 14),indx( 15),indx( 16)/ 194, 199, 210, 215/
       data indx( 17),indx( 18),indx( 19),indx( 20)/ 236, 248, 269, 297/
       data indx( 21),indx( 22),indx( 23),indx( 24)/ 306, 325, 346, 359/
       data indx( 25),indx( 26),indx( 27),indx( 28)/ 395, 416, 438, 462/
       data indx( 29),indx( 30),indx( 31),indx( 32)/ 469, 479, 486, 514/
       data indx( 33),indx( 34),indx( 35),indx( 36)/ 543, 560, 587, 604/
       data indx( 37),indx( 38),indx( 39),indx( 40)/ 621, 635, 646, 667/
       data indx( 41),indx( 42),indx( 43),indx( 44)/ 681, 696, 712, 725/
       data indx( 45),indx( 46),indx( 47),indx( 48)/ 732, 747, 758, 777/
       data indx( 49),indx( 50),indx( 51),indx( 52)/ 791, 815, 834, 859/
       data indx( 53),indx( 54),indx( 55),indx( 56)/ 869, 882, 893, 909/
       data indx( 57),indx( 58),indx( 59),indx( 60)/ 926, 942, 955, 964/
       data indx( 61),indx( 62),indx( 63),indx( 64)/ 969, 978, 985, 990/
       data indx( 65),indx( 66),indx( 67),indx( 68)/1003,1029,1050,1067/
       data indx( 69),indx( 70),indx( 71),indx( 72)/1089,1108,1124,1152/
       data indx( 73),indx( 74),indx( 75),indx( 76)/1167,1190,1214,1228/
       data indx( 77),indx( 78),indx( 79),indx( 80)/1240,1261,1276,1295/
       data indx( 81),indx( 82),indx( 83),indx( 84)/1317,1339,1352,1375/
       data indx( 85),indx( 86),indx( 87),indx( 88)/1391,1407,1418,1442/
       data indx( 89),indx( 90),indx( 91),indx( 92)/1451,1473,1482,1501/
       data indx( 93),indx( 94),indx( 95),indx( 96)/1506,1525,1538,1555/
       data bxy(   1),bxy(   2),bxy(   3),bxy(   4)/-64,  3,  8,  3/
       data bxy(   5),bxy(   6),bxy(   7),bxy(   8)/  3,-64,  3,  0/
       data bxy(   9),bxy(  10),bxy(  11),bxy(  12)/  2,  0,  2,  1/
       data bxy(  13),bxy(  14),bxy(  15),bxy(  16)/  3,  1,  3,  0/
       data bxy(  17),bxy(  18),bxy(  19),bxy(  20)/-64,  2,  8,  2/
       data bxy(  21),bxy(  22),bxy(  23),bxy(  24)/  6,-64,  4,  8/
       data bxy(  25),bxy(  26),bxy(  27),bxy(  28)/  4,  6,-64,  2/
       data bxy(  29),bxy(  30),bxy(  31),bxy(  32)/  8,  2,  0,-64/
       data bxy(  33),bxy(  34),bxy(  35),bxy(  36)/  4,  8,  4,  0/
       data bxy(  37),bxy(  38),bxy(  39),bxy(  40)/-64,  6,  5,  0/
       data bxy(  41),bxy(  42),bxy(  43),bxy(  44)/  5,-64,  0,  3/
       data bxy(  45),bxy(  46),bxy(  47),bxy(  48)/  6,  3,-64,  6/
       data bxy(  49),bxy(  50),bxy(  51),bxy(  52)/  7,  1,  7,  0/
       data bxy(  53),bxy(  54),bxy(  55),bxy(  56)/  6,  0,  5,  1/
       data bxy(  57),bxy(  58),bxy(  59),bxy(  60)/  4,  5,  4,  6/
       data bxy(  61),bxy(  62),bxy(  63),bxy(  64)/  3,  6,  2,  5/
       data bxy(  65),bxy(  66),bxy(  67),bxy(  68)/  1,  0,  1,-64/
       data bxy(  69),bxy(  70),bxy(  71),bxy(  72)/  3,  8,  3,  0/
       data bxy(  73),bxy(  74),bxy(  75),bxy(  76)/-64,  1,  8,  0/
       data bxy(  77),bxy(  78),bxy(  79),bxy(  80)/  7,  1,  6,  2/
       data bxy(  81),bxy(  82),bxy(  83),bxy(  84)/  7,  1,  8,-64/
       data bxy(  85),bxy(  86),bxy(  87),bxy(  88)/  6,  7,  0,  1/
       data bxy(  89),bxy(  90),bxy(  91),bxy(  92)/-64,  5,  2,  4/
       data bxy(  93),bxy(  94),bxy(  95),bxy(  96)/  1,  5,  0,  6/
       data bxy(  97),bxy(  98),bxy(  99),bxy( 100)/  1,  5,  2,-64/
       data bxy( 101),bxy( 102),bxy( 103),bxy( 104)/  6,  3,  3,  0/
       data bxy( 105),bxy( 106),bxy( 107),bxy( 108)/  1,  0,  0,  1/
       data bxy( 109),bxy( 110),bxy( 111),bxy( 112)/  0,  2,  4,  6/
       data bxy( 113),bxy( 114),bxy( 115),bxy( 116)/  4,  7,  3,  8/
       data bxy( 117),bxy( 118),bxy( 119),bxy( 120)/  1,  8,  0,  7/
       data bxy( 121),bxy( 122),bxy( 123),bxy( 124)/  0,  6,  6,  0/
       data bxy( 125),bxy( 126),bxy( 127),bxy( 128)/-64,  4,  7,  3/
       data bxy( 129),bxy( 130),bxy( 131),bxy( 132)/  7,  3,  8,  4/
       data bxy( 133),bxy( 134),bxy( 135),bxy( 136)/  8,  4,  7,  2/
       data bxy( 137),bxy( 138),bxy( 139),bxy( 140)/  5,-64,  4,  8/
       data bxy( 141),bxy( 142),bxy( 143),bxy( 144)/  2,  6,  2,  2/
       data bxy( 145),bxy( 146),bxy( 147),bxy( 148)/  4,  0,-64,  2/
       data bxy( 149),bxy( 150),bxy( 151),bxy( 152)/  8,  4,  6,  4/
       data bxy( 153),bxy( 154),bxy( 155),bxy( 156)/  2,  2,  0,-64/
       data bxy( 157),bxy( 158),bxy( 159),bxy( 160)/  1,  2,  5,  6/
       data bxy( 161),bxy( 162),bxy( 163),bxy( 164)/-64,  3,  7,  3/
       data bxy( 165),bxy( 166),bxy( 167),bxy( 168)/  1,-64,  1,  6/
       data bxy( 169),bxy( 170),bxy( 171),bxy( 172)/  5,  2,-64,  3/
       data bxy( 173),bxy( 174),bxy( 175),bxy( 176)/  7,  3,  1,-64/
       data bxy( 177),bxy( 178),bxy( 179),bxy( 180)/  0,  4,  6,  4/
       data bxy( 181),bxy( 182),bxy( 183),bxy( 184)/-64,  3,  0,  2/
       data bxy( 185),bxy( 186),bxy( 187),bxy( 188)/  0,  2,  1,  3/
       data bxy( 189),bxy( 190),bxy( 191),bxy( 192)/  1,  3,  0,  1/
       data bxy( 193),bxy( 194),bxy( 195),bxy( 196)/ -2,-64,  0,  4/
       data bxy( 197),bxy( 198),bxy( 199),bxy( 200)/  6,  4,-64,  3/
       data bxy( 201),bxy( 202),bxy( 203),bxy( 204)/  0,  2,  0,  2/
       data bxy( 205),bxy( 206),bxy( 207),bxy( 208)/  1,  3,  1,  3/
       data bxy( 209),bxy( 210),bxy( 211),bxy( 212)/  0,-64,  0,  1/
       data bxy( 213),bxy( 214),bxy( 215),bxy( 216)/  6,  7,-64,  6/
       data bxy( 217),bxy( 218),bxy( 219),bxy( 220)/  7,  6,  1,  5/
       data bxy( 221),bxy( 222),bxy( 223),bxy( 224)/  0,  1,  0,  0/
       data bxy( 225),bxy( 226),bxy( 227),bxy( 228)/  1,  0,  7,  1/
       data bxy( 229),bxy( 230),bxy( 231),bxy( 232)/  8,  5,  8,  6/
       data bxy( 233),bxy( 234),bxy( 235),bxy( 236)/  7,  0,  1,-64/
       data bxy( 237),bxy( 238),bxy( 239),bxy( 240)/  1,  6,  3,  8/
       data bxy( 241),bxy( 242),bxy( 243),bxy( 244)/  3,  0,-64,  1/
       data bxy( 245),bxy( 246),bxy( 247),bxy( 248)/  0,  5,  0,-64/
       data bxy( 249),bxy( 250),bxy( 251),bxy( 252)/  0,  7,  1,  8/
       data bxy( 253),bxy( 254),bxy( 255),bxy( 256)/  5,  8,  6,  7/
       data bxy( 257),bxy( 258),bxy( 259),bxy( 260)/  6,  6,  4,  4/
       data bxy( 261),bxy( 262),bxy( 263),bxy( 264)/  2,  4,  0,  2/
       data bxy( 265),bxy( 266),bxy( 267),bxy( 268)/  0,  0,  6,  0/
       data bxy( 269),bxy( 270),bxy( 271),bxy( 272)/-64,  0,  7,  1/
       data bxy( 273),bxy( 274),bxy( 275),bxy( 276)/  8,  5,  8,  6/
       data bxy( 277),bxy( 278),bxy( 279),bxy( 280)/  7,  6,  5,  5/
       data bxy( 281),bxy( 282),bxy( 283),bxy( 284)/  4,  1,  4,-64/
       data bxy( 285),bxy( 286),bxy( 287),bxy( 288)/  5,  4,  6,  3/
       data bxy( 289),bxy( 290),bxy( 291),bxy( 292)/  6,  1,  5,  0/
       data bxy( 293),bxy( 294),bxy( 295),bxy( 296)/  1,  0,  0,  1/
       data bxy( 297),bxy( 298),bxy( 299),bxy( 300)/-64,  5,  0,  5/
       data bxy( 301),bxy( 302),bxy( 303),bxy( 304)/  8,  0,  3,  6/
       data bxy( 305),bxy( 306),bxy( 307),bxy( 308)/  3,-64,  0,  1/
       data bxy( 309),bxy( 310),bxy( 311),bxy( 312)/  1,  0,  4,  0/
       data bxy( 313),bxy( 314),bxy( 315),bxy( 316)/  6,  2,  6,  3/
       data bxy( 317),bxy( 318),bxy( 319),bxy( 320)/  4,  5,  0,  5/
       data bxy( 321),bxy( 322),bxy( 323),bxy( 324)/  0,  8,  6,  8/
       data bxy( 325),bxy( 326),bxy( 327),bxy( 328)/-64,  5,  8,  2/
       data bxy( 329),bxy( 330),bxy( 331),bxy( 332)/  8,  0,  6,  0/
       data bxy( 333),bxy( 334),bxy( 335),bxy( 336)/  1,  1,  0,  5/
       data bxy( 337),bxy( 338),bxy( 339),bxy( 340)/  0,  6,  1,  6/
       data bxy( 341),bxy( 342),bxy( 343),bxy( 344)/  3,  5,  4,  0/
       data bxy( 345),bxy( 346),bxy( 347),bxy( 348)/  4,-64,  0,  7/
       data bxy( 349),bxy( 350),bxy( 351),bxy( 352)/  0,  8,  6,  8/
       data bxy( 353),bxy( 354),bxy( 355),bxy( 356)/  6,  7,  2,  3/
       data bxy( 357),bxy( 358),bxy( 359),bxy( 360)/  2,  0,-64,  6/
       data bxy( 361),bxy( 362),bxy( 363),bxy( 364)/  7,  5,  8,  1/
       data bxy( 365),bxy( 366),bxy( 367),bxy( 368)/  8,  0,  7,  0/
       data bxy( 369),bxy( 370),bxy( 371),bxy( 372)/  5,  1,  4,  5/
       data bxy( 373),bxy( 374),bxy( 375),bxy( 376)/  4,  6,  3,  6/
       data bxy( 377),bxy( 378),bxy( 379),bxy( 380)/  1,  5,  0,  1/
       data bxy( 381),bxy( 382),bxy( 383),bxy( 384)/  0,  0,  1,  0/
       data bxy( 385),bxy( 386),bxy( 387),bxy( 388)/  3,  1,  4,-64/
       data bxy( 389),bxy( 390),bxy( 391),bxy( 392)/  5,  4,  6,  5/
       data bxy( 393),bxy( 394),bxy( 395),bxy( 396)/  6,  7,-64,  1/
       data bxy( 397),bxy( 398),bxy( 399),bxy( 400)/  0,  4,  0,  6/
       data bxy( 401),bxy( 402),bxy( 403),bxy( 404)/  2,  6,  7,  5/
       data bxy( 405),bxy( 406),bxy( 407),bxy( 408)/  8,  1,  8,  0/
       data bxy( 409),bxy( 410),bxy( 411),bxy( 412)/  7,  0,  5,  1/
       data bxy( 413),bxy( 414),bxy( 415),bxy( 416)/  4,  6,  4,-64/
       data bxy( 417),bxy( 418),bxy( 419),bxy( 420)/  3,  4,  2,  4/
       data bxy( 421),bxy( 422),bxy( 423),bxy( 424)/  2,  5,  3,  5/
       data bxy( 425),bxy( 426),bxy( 427),bxy( 428)/  3,  4,-64,  3/
       data bxy( 429),bxy( 430),bxy( 431),bxy( 432)/  0,  2,  0,  2/
       data bxy( 433),bxy( 434),bxy( 435),bxy( 436)/  1,  3,  1,  3/
       data bxy( 437),bxy( 438),bxy( 439),bxy( 440)/  0,-64,  3,  4/
       data bxy( 441),bxy( 442),bxy( 443),bxy( 444)/  2,  4,  2,  5/
       data bxy( 445),bxy( 446),bxy( 447),bxy( 448)/  3,  5,  3,  4/
       data bxy( 449),bxy( 450),bxy( 451),bxy( 452)/-64,  3,  0,  2/
       data bxy( 453),bxy( 454),bxy( 455),bxy( 456)/  0,  2,  1,  3/
       data bxy( 457),bxy( 458),bxy( 459),bxy( 460)/  1,  3,  0,  1/
       data bxy( 461),bxy( 462),bxy( 463),bxy( 464)/ -2,-64,  4,  8/
       data bxy( 465),bxy( 466),bxy( 467),bxy( 468)/  0,  4,  4,  0/
       data bxy( 469),bxy( 470),bxy( 471),bxy( 472)/-64,  5,  5,  1/
       data bxy( 473),bxy( 474),bxy( 475),bxy( 476)/  5,-64,  1,  3/
       data bxy( 477),bxy( 478),bxy( 479),bxy( 480)/  5,  3,-64,  2/
       data bxy( 481),bxy( 482),bxy( 483),bxy( 484)/  8,  6,  4,  2/
       data bxy( 485),bxy( 486),bxy( 487),bxy( 488)/  0,-64,  0,  7/
       data bxy( 489),bxy( 490),bxy( 491),bxy( 492)/  1,  8,  5,  8/
       data bxy( 493),bxy( 494),bxy( 495),bxy( 496)/  6,  7,  6,  5/
       data bxy( 497),bxy( 498),bxy( 499),bxy( 500)/  5,  4,  3,  4/
       data bxy( 501),bxy( 502),bxy( 503),bxy( 504)/  3,  3,-64,  3/
       data bxy( 505),bxy( 506),bxy( 507),bxy( 508)/  0,  2,  0,  2/
       data bxy( 509),bxy( 510),bxy( 511),bxy( 512)/  1,  3,  1,  3/
       data bxy( 513),bxy( 514),bxy( 515),bxy( 516)/  0,-64,  4,  3/
       data bxy( 517),bxy( 518),bxy( 519),bxy( 520)/  4,  5,  3,  5/
       data bxy( 521),bxy( 522),bxy( 523),bxy( 524)/  2,  4,  2,  3/
       data bxy( 525),bxy( 526),bxy( 527),bxy( 528)/  5,  3,  6,  4/
       data bxy( 529),bxy( 530),bxy( 531),bxy( 532)/  6,  7,  5,  8/
       data bxy( 533),bxy( 534),bxy( 535),bxy( 536)/  2,  8,  0,  6/
       data bxy( 537),bxy( 538),bxy( 539),bxy( 540)/  0,  2,  2,  0/
       data bxy( 541),bxy( 542),bxy( 543),bxy( 544)/  5,  0,  0,  0/
       data bxy( 545),bxy( 546),bxy( 547),bxy( 548)/  0,  6,  2,  8/
       data bxy( 549),bxy( 550),bxy( 551),bxy( 552)/  4,  8,  6,  6/
       data bxy( 553),bxy( 554),bxy( 555),bxy( 556)/  6,  0,-64,  0/
       data bxy( 557),bxy( 558),bxy( 559),bxy( 560)/  3,  6,  3,  0/
       data bxy( 561),bxy( 562),bxy( 563),bxy( 564)/  0,  5,  0,  6/
       data bxy( 565),bxy( 566),bxy( 567),bxy( 568)/  1,  6,  3,  5/
       data bxy( 569),bxy( 570),bxy( 571),bxy( 572)/  4,  1,  4,-64/
       data bxy( 573),bxy( 574),bxy( 575),bxy( 576)/  5,  4,  6,  5/
       data bxy( 577),bxy( 578),bxy( 579),bxy( 580)/  6,  7,  5,  8/
       data bxy( 581),bxy( 582),bxy( 583),bxy( 584)/  0,  8,  1,  8/
       data bxy( 585),bxy( 586),bxy( 587),bxy( 588)/  1,  0,-64,  6/
       data bxy( 589),bxy( 590),bxy( 591),bxy( 592)/  7,  5,  8,  2/
       data bxy( 593),bxy( 594),bxy( 595),bxy( 596)/  8,  0,  6,  0/
       data bxy( 597),bxy( 598),bxy( 599),bxy( 600)/  2,  2,  0,  5/
       data bxy( 601),bxy( 602),bxy( 603),bxy( 604)/  0,  6,  1,  0/
       data bxy( 605),bxy( 606),bxy( 607),bxy( 608)/  0,  4,  0,  6/
       data bxy( 609),bxy( 610),bxy( 611),bxy( 612)/  2,  6,  6,  4/
       data bxy( 613),bxy( 614),bxy( 615),bxy( 616)/  8,  0,  8,-64/
       data bxy( 617),bxy( 618),bxy( 619),bxy( 620)/  1,  8,  1,  0/
       data bxy( 621),bxy( 622),bxy( 623),bxy( 624)/-64,  6,  0,  0/
       data bxy( 625),bxy( 626),bxy( 627),bxy( 628)/  0,  0,  8,  6/
       data bxy( 629),bxy( 630),bxy( 631),bxy( 632)/  8,-64,  3,  4/
       data bxy( 633),bxy( 634),bxy( 635),bxy( 636)/  0,  4,  0,  0/
       data bxy( 637),bxy( 638),bxy( 639),bxy( 640)/  0,  8,  6,  8/
       data bxy( 641),bxy( 642),bxy( 643),bxy( 644)/-64,  3,  4,  0/
       data bxy( 645),bxy( 646),bxy( 647),bxy( 648)/  4,-64,  6,  7/
       data bxy( 649),bxy( 650),bxy( 651),bxy( 652)/  5,  8,  2,  8/
       data bxy( 653),bxy( 654),bxy( 655),bxy( 656)/  0,  6,  0,  2/
       data bxy( 657),bxy( 658),bxy( 659),bxy( 660)/  2,  0,  5,  0/
       data bxy( 661),bxy( 662),bxy( 663),bxy( 664)/  6,  1,  6,  3/
       data bxy( 665),bxy( 666),bxy( 667),bxy( 668)/  3,  3,  0,  0/
       data bxy( 669),bxy( 670),bxy( 671),bxy( 672)/  0,  8,-64,  0/
       data bxy( 673),bxy( 674),bxy( 675),bxy( 676)/  4,  6,  4,-64/
       data bxy( 677),bxy( 678),bxy( 679),bxy( 680)/  6,  8,  6,  0/
       data bxy( 681),bxy( 682),bxy( 683),bxy( 684)/-64,  1,  0,  5/
       data bxy( 685),bxy( 686),bxy( 687),bxy( 688)/  0,-64,  3,  0/
       data bxy( 689),bxy( 690),bxy( 691),bxy( 692)/  3,  8,-64,  1/
       data bxy( 693),bxy( 694),bxy( 695),bxy( 696)/  8,  5,  8,-64/
       data bxy( 697),bxy( 698),bxy( 699),bxy( 700)/  0,  1,  1,  0/
       data bxy( 701),bxy( 702),bxy( 703),bxy( 704)/  3,  0,  4,  1/
       data bxy( 705),bxy( 706),bxy( 707),bxy( 708)/  4,  8,-64,  2/
       data bxy( 709),bxy( 710),bxy( 711),bxy( 712)/  8,  6,  8,  0/
       data bxy( 713),bxy( 714),bxy( 715),bxy( 716)/  0,  0,  8,-64/
       data bxy( 717),bxy( 718),bxy( 719),bxy( 720)/  6,  8,  0,  2/
       data bxy( 721),bxy( 722),bxy( 723),bxy( 724)/  2,  4,  6,  0/
       data bxy( 725),bxy( 726),bxy( 727),bxy( 728)/-64,  0,  8,  0/
       data bxy( 729),bxy( 730),bxy( 731),bxy( 732)/  0,  6,  0,  0/
       data bxy( 733),bxy( 734),bxy( 735),bxy( 736)/  0,  0,  8,  3/
       data bxy( 737),bxy( 738),bxy( 739),bxy( 740)/  5,  3,  4,-64/
       data bxy( 741),bxy( 742),bxy( 743),bxy( 744)/  3,  5,  6,  8/
       data bxy( 745),bxy( 746),bxy( 747),bxy( 748)/  6,  0,  0,  0/
       data bxy( 749),bxy( 750),bxy( 751),bxy( 752)/  0,  8,  6,  2/
       data bxy( 753),bxy( 754),bxy( 755),bxy( 756)/-64,  6,  8,  6/
       data bxy( 757),bxy( 758),bxy( 759),bxy( 760)/  0,-64,  6,  2/
       data bxy( 761),bxy( 762),bxy( 763),bxy( 764)/  6,  6,  4,  8/
       data bxy( 765),bxy( 766),bxy( 767),bxy( 768)/  2,  8,  0,  6/
       data bxy( 769),bxy( 770),bxy( 771),bxy( 772)/  0,  2,  2,  0/
       data bxy( 773),bxy( 774),bxy( 775),bxy( 776)/  4,  0,  6,  2/
       data bxy( 777),bxy( 778),bxy( 779),bxy( 780)/  0,  0,  0,  8/
       data bxy( 781),bxy( 782),bxy( 783),bxy( 784)/  5,  8,  6,  7/
       data bxy( 785),bxy( 786),bxy( 787),bxy( 788)/  6,  5,  5,  4/
       data bxy( 789),bxy( 790),bxy( 791),bxy( 792)/  0,  4,-64,  6/
       data bxy( 793),bxy( 794),bxy( 795),bxy( 796)/  2,  6,  6,  4/
       data bxy( 797),bxy( 798),bxy( 799),bxy( 800)/  8,  2,  8,  0/
       data bxy( 801),bxy( 802),bxy( 803),bxy( 804)/  6,  0,  2,  2/
       data bxy( 805),bxy( 806),bxy( 807),bxy( 808)/  0,  4,  0,  6/
       data bxy( 809),bxy( 810),bxy( 811),bxy( 812)/  2,-64,  3,  3/
       data bxy( 813),bxy( 814),bxy( 815),bxy( 816)/  6,  0,  0,  0/
       data bxy( 817),bxy( 818),bxy( 819),bxy( 820)/  0,  8,  5,  8/
       data bxy( 821),bxy( 822),bxy( 823),bxy( 824)/  6,  7,  6,  5/
       data bxy( 825),bxy( 826),bxy( 827),bxy( 828)/  5,  4,  0,  4/
       data bxy( 829),bxy( 830),bxy( 831),bxy( 832)/-64,  2,  4,  6/
       data bxy( 833),bxy( 834),bxy( 835),bxy( 836)/  0,-64,  6,  7/
       data bxy( 837),bxy( 838),bxy( 839),bxy( 840)/  5,  8,  1,  8/
       data bxy( 841),bxy( 842),bxy( 843),bxy( 844)/  0,  7,  0,  5/
       data bxy( 845),bxy( 846),bxy( 847),bxy( 848)/  1,  4,  5,  4/
       data bxy( 849),bxy( 850),bxy( 851),bxy( 852)/  6,  3,  6,  1/
       data bxy( 853),bxy( 854),bxy( 855),bxy( 856)/  5,  0,  1,  0/
       data bxy( 857),bxy( 858),bxy( 859),bxy( 860)/  0,  1,-64,  0/
       data bxy( 861),bxy( 862),bxy( 863),bxy( 864)/  8,  6,  8,-64/
       data bxy( 865),bxy( 866),bxy( 867),bxy( 868)/  3,  0,  3,  8/
       data bxy( 869),bxy( 870),bxy( 871),bxy( 872)/-64,  6,  8,  6/
       data bxy( 873),bxy( 874),bxy( 875),bxy( 876)/  1,  5,  0,  1/
       data bxy( 877),bxy( 878),bxy( 879),bxy( 880)/  0,  0,  1,  0/
       data bxy( 881),bxy( 882),bxy( 883),bxy( 884)/  8,-64,  0,  8/
       data bxy( 885),bxy( 886),bxy( 887),bxy( 888)/  0,  6,  3,  0/
       data bxy( 889),bxy( 890),bxy( 891),bxy( 892)/  6,  6,  6,  8/
       data bxy( 893),bxy( 894),bxy( 895),bxy( 896)/-64,  0,  8,  0/
       data bxy( 897),bxy( 898),bxy( 899),bxy( 900)/  0,  3,  3,  3/
       data bxy( 901),bxy( 902),bxy( 903),bxy( 904)/  4,-64,  3,  3/
       data bxy( 905),bxy( 906),bxy( 907),bxy( 908)/  6,  0,  6,  8/
       data bxy( 909),bxy( 910),bxy( 911),bxy( 912)/  0,  0,  0,  1/
       data bxy( 913),bxy( 914),bxy( 915),bxy( 916)/  6,  7,  6,  8/
       data bxy( 917),bxy( 918),bxy( 919),bxy( 920)/-64,  0,  8,  0/
       data bxy( 921),bxy( 922),bxy( 923),bxy( 924)/  7,  6,  1,  6/
       data bxy( 925),bxy( 926),bxy( 927),bxy( 928)/  0,-64,  0,  8/
       data bxy( 929),bxy( 930),bxy( 931),bxy( 932)/  0,  7,  3,  4/
       data bxy( 933),bxy( 934),bxy( 935),bxy( 936)/  6,  7,  6,  8/
       data bxy( 937),bxy( 938),bxy( 939),bxy( 940)/-64,  3,  4,  3/
       data bxy( 941),bxy( 942),bxy( 943),bxy( 944)/  0,-64,  0,  8/
       data bxy( 945),bxy( 946),bxy( 947),bxy( 948)/  6,  8,  6,  7/
       data bxy( 949),bxy( 950),bxy( 951),bxy( 952)/  0,  1,  0,  0/
       data bxy( 953),bxy( 954),bxy( 955),bxy( 956)/  6,  0,-64,  4/
       data bxy( 957),bxy( 958),bxy( 959),bxy( 960)/  8,  2,  8,  2/
       data bxy( 961),bxy( 962),bxy( 963),bxy( 964)/  0,  4,  0,-64/
       data bxy( 965),bxy( 966),bxy( 967),bxy( 968)/  0,  7,  6,  1/
       data bxy( 969),bxy( 970),bxy( 971),bxy( 972)/-64,  3,  8,  5/
       data bxy( 973),bxy( 974),bxy( 975),bxy( 976)/  8,  5,  0,  3/
       data bxy( 977),bxy( 978),bxy( 979),bxy( 980)/  0,-64,  0,  5/
       data bxy( 981),bxy( 982),bxy( 983),bxy( 984)/  3,  8,  6,  5/
       data bxy( 985),bxy( 986),bxy( 987),bxy( 988)/-64,  0, -2,  6/
       data bxy( 989),bxy( 990),bxy( 991),bxy( 992)/ -2,-64,  2,  7/
       data bxy( 993),bxy( 994),bxy( 995),bxy( 996)/  3,  7,  3,  8/
       data bxy( 997),bxy( 998),bxy( 999),bxy(1000)/  2,  8,  2,  7/
       data bxy(1001),bxy(1002),bxy(1003),bxy(1004)/  4,  5,-64,  5/
       data bxy(1005),bxy(1006),bxy(1007),bxy(1008)/  3,  1,  3,  0/
       data bxy(1009),bxy(1010),bxy(1011),bxy(1012)/  2,  0,  1,  1/
       data bxy(1013),bxy(1014),bxy(1015),bxy(1016)/  0,  4,  0,  5/
       data bxy(1017),bxy(1018),bxy(1019),bxy(1020)/  1,-64,  1,  5/
       data bxy(1021),bxy(1022),bxy(1023),bxy(1024)/  4,  5,  5,  4/
       data bxy(1025),bxy(1026),bxy(1027),bxy(1028)/  5,  1,  6,  0/
       data bxy(1029),bxy(1030),bxy(1031),bxy(1032)/  0,  0,  0,  8/
       data bxy(1033),bxy(1034),bxy(1035),bxy(1036)/-64,  0,  3,  2/
       data bxy(1037),bxy(1038),bxy(1039),bxy(1040)/  5,  5,  5,  6/
       data bxy(1041),bxy(1042),bxy(1043),bxy(1044)/  4,  6,  1,  5/
       data bxy(1045),bxy(1046),bxy(1047),bxy(1048)/  0,  2,  0,  0/
       data bxy(1049),bxy(1050),bxy(1051),bxy(1052)/  2,-64,  5,  4/
       data bxy(1053),bxy(1054),bxy(1055),bxy(1056)/  4,  5,  1,  5/
       data bxy(1057),bxy(1058),bxy(1059),bxy(1060)/  0,  4,  0,  1/
       data bxy(1061),bxy(1062),bxy(1063),bxy(1064)/  1,  0,  4,  0/
       data bxy(1065),bxy(1066),bxy(1067),bxy(1068)/  5,  1,-64,  5/
       data bxy(1069),bxy(1070),bxy(1071),bxy(1072)/  8,  5,  0,-64/
       data bxy(1073),bxy(1074),bxy(1075),bxy(1076)/  5,  2,  3,  0/
       data bxy(1077),bxy(1078),bxy(1079),bxy(1080)/  1,  0,  0,  1/
       data bxy(1081),bxy(1082),bxy(1083),bxy(1084)/  0,  4,  1,  5/
       data bxy(1085),bxy(1086),bxy(1087),bxy(1088)/  3,  5,  5,  3/
       data bxy(1089),bxy(1090),bxy(1091),bxy(1092)/-64,  0,  3,  6/
       data bxy(1093),bxy(1094),bxy(1095),bxy(1096)/  3,  6,  4,  5/
       data bxy(1097),bxy(1098),bxy(1099),bxy(1100)/  5,  1,  5,  0/
       data bxy(1101),bxy(1102),bxy(1103),bxy(1104)/  4,  0,  1,  1/
       data bxy(1105),bxy(1106),bxy(1107),bxy(1108)/  0,  5,  0,-64/
       data bxy(1109),bxy(1110),bxy(1111),bxy(1112)/  2,  0,  2,  7/
       data bxy(1113),bxy(1114),bxy(1115),bxy(1116)/  3,  8,  4,  8/
       data bxy(1117),bxy(1118),bxy(1119),bxy(1120)/  5,  7,-64,  0/
       data bxy(1121),bxy(1122),bxy(1123),bxy(1124)/  4,  4,  4,-64/
       data bxy(1125),bxy(1126),bxy(1127),bxy(1128)/  5,  2,  3,  0/
       data bxy(1129),bxy(1130),bxy(1131),bxy(1132)/  1,  0,  0,  1/
       data bxy(1133),bxy(1134),bxy(1135),bxy(1136)/  0,  4,  1,  5/
       data bxy(1137),bxy(1138),bxy(1139),bxy(1140)/  3,  5,  5,  3/
       data bxy(1141),bxy(1142),bxy(1143),bxy(1144)/-64,  5,  5,  5/
       data bxy(1145),bxy(1146),bxy(1147),bxy(1148)/ -3,  4, -4,  1/
       data bxy(1149),bxy(1150),bxy(1151),bxy(1152)/ -4,  0, -3,  0/
       data bxy(1153),bxy(1154),bxy(1155),bxy(1156)/  0,  0,  8,-64/
       data bxy(1157),bxy(1158),bxy(1159),bxy(1160)/  0,  3,  2,  5/
       data bxy(1161),bxy(1162),bxy(1163),bxy(1164)/  5,  5,  6,  4/
       data bxy(1165),bxy(1166),bxy(1167),bxy(1168)/  6,  0,-64,  2/
       data bxy(1169),bxy(1170),bxy(1171),bxy(1172)/  0,  4,  0,-64/
       data bxy(1173),bxy(1174),bxy(1175),bxy(1176)/  3,  0,  3,  5/
       data bxy(1177),bxy(1178),bxy(1179),bxy(1180)/  2,  5,-64,  2/
       data bxy(1181),bxy(1182),bxy(1183),bxy(1184)/  7,  3,  7,  3/
       data bxy(1185),bxy(1186),bxy(1187),bxy(1188)/  8,  2,  8,  2/
       data bxy(1189),bxy(1190),bxy(1191),bxy(1192)/  7,-64,  5,  7/
       data bxy(1193),bxy(1194),bxy(1195),bxy(1196)/  5,  8,  4,  8/
       data bxy(1197),bxy(1198),bxy(1199),bxy(1200)/  4,  7,  5,  7/
       data bxy(1201),bxy(1202),bxy(1203),bxy(1204)/-64,  4,  5,  5/
       data bxy(1205),bxy(1206),bxy(1207),bxy(1208)/  5,  5, -3,  4/
       data bxy(1209),bxy(1210),bxy(1211),bxy(1212)/ -4,  2, -4,  1/
       data bxy(1213),bxy(1214),bxy(1215),bxy(1216)/ -3,  0,  0,  0/
       data bxy(1217),bxy(1218),bxy(1219),bxy(1220)/  8,-64,  4,  5/
       data bxy(1221),bxy(1222),bxy(1223),bxy(1224)/  0,  1,-64,  2/
       data bxy(1225),bxy(1226),bxy(1227),bxy(1228)/  3,  5,  0,-64/
       data bxy(1229),bxy(1230),bxy(1231),bxy(1232)/  2,  0,  4,  0/
       data bxy(1233),bxy(1234),bxy(1235),bxy(1236)/-64,  3,  0,  3/
       data bxy(1237),bxy(1238),bxy(1239),bxy(1240)/  8,  2,  8,  0/
       data bxy(1241),bxy(1242),bxy(1243),bxy(1244)/  0,  0,  5,  2/
       data bxy(1245),bxy(1246),bxy(1247),bxy(1248)/  5,  3,  4,  3/
       data bxy(1249),bxy(1250),bxy(1251),bxy(1252)/  0,-64,  3,  4/
       data bxy(1253),bxy(1254),bxy(1255),bxy(1256)/  4,  5,  5,  5/
       data bxy(1257),bxy(1258),bxy(1259),bxy(1260)/  6,  4,  6,  0/
       data bxy(1261),bxy(1262),bxy(1263),bxy(1264)/  0,  0,  0,  5/
       data bxy(1265),bxy(1266),bxy(1267),bxy(1268)/-64,  0,  3,  2/
       data bxy(1269),bxy(1270),bxy(1271),bxy(1272)/  5,  4,  5,  5/
       data bxy(1273),bxy(1274),bxy(1275),bxy(1276)/  4,  5,  0,-64/
       data bxy(1277),bxy(1278),bxy(1279),bxy(1280)/  5,  4,  4,  5/
       data bxy(1281),bxy(1282),bxy(1283),bxy(1284)/  1,  5,  0,  4/
       data bxy(1285),bxy(1286),bxy(1287),bxy(1288)/  0,  1,  1,  0/
       data bxy(1289),bxy(1290),bxy(1291),bxy(1292)/  4,  0,  5,  1/
       data bxy(1293),bxy(1294),bxy(1295),bxy(1296)/  5,  4,-64,  0/
       data bxy(1297),bxy(1298),bxy(1299),bxy(1300)/ -4,  0,  5,-64/
       data bxy(1301),bxy(1302),bxy(1303),bxy(1304)/  0,  3,  2,  5/
       data bxy(1305),bxy(1306),bxy(1307),bxy(1308)/  4,  5,  5,  4/
       data bxy(1309),bxy(1310),bxy(1311),bxy(1312)/  5,  1,  4,  0/
       data bxy(1313),bxy(1314),bxy(1315),bxy(1316)/  2,  0,  0,  2/
       data bxy(1317),bxy(1318),bxy(1319),bxy(1320)/-64,  5, -4,  5/
       data bxy(1321),bxy(1322),bxy(1323),bxy(1324)/  5,-64,  5,  2/
       data bxy(1325),bxy(1326),bxy(1327),bxy(1328)/  3,  0,  1,  0/
       data bxy(1329),bxy(1330),bxy(1331),bxy(1332)/  0,  1,  0,  4/
       data bxy(1333),bxy(1334),bxy(1335),bxy(1336)/  1,  5,  3,  5/
       data bxy(1337),bxy(1338),bxy(1339),bxy(1340)/  5,  3,  0,  0/
       data bxy(1341),bxy(1342),bxy(1343),bxy(1344)/  0,  5,-64,  0/
       data bxy(1345),bxy(1346),bxy(1347),bxy(1348)/  3,  2,  5,  4/
       data bxy(1349),bxy(1350),bxy(1351),bxy(1352)/  5,  5,  4,-64/
       data bxy(1353),bxy(1354),bxy(1355),bxy(1356)/-64,-64,  0,  1/
       data bxy(1357),bxy(1358),bxy(1359),bxy(1360)/  1,  0,  4,  0/
       data bxy(1361),bxy(1362),bxy(1363),bxy(1364)/  5,  1,  4,  2/
       data bxy(1365),bxy(1366),bxy(1367),bxy(1368)/  1,  3,  0,  4/
       data bxy(1369),bxy(1370),bxy(1371),bxy(1372)/  1,  5,  4,  5/
       data bxy(1373),bxy(1374),bxy(1375),bxy(1376)/  5,  4,-64,  2/
       data bxy(1377),bxy(1378),bxy(1379),bxy(1380)/  8,  2,  1,  3/
       data bxy(1381),bxy(1382),bxy(1383),bxy(1384)/  0,  4,  0,  5/
       data bxy(1385),bxy(1386),bxy(1387),bxy(1388)/  1,-64,  0,  5/
       data bxy(1389),bxy(1390),bxy(1391),bxy(1392)/  4,  5,-64,  0/
       data bxy(1393),bxy(1394),bxy(1395),bxy(1396)/  5,  0,  1,  1/
       data bxy(1397),bxy(1398),bxy(1399),bxy(1400)/  0,  4,  0,  5/
       data bxy(1401),bxy(1402),bxy(1403),bxy(1404)/  1,-64,  5,  5/
       data bxy(1405),bxy(1406),bxy(1407),bxy(1408)/  5,  0,-64,  0/
       data bxy(1409),bxy(1410),bxy(1411),bxy(1412)/  5,  0,  3,  3/
       data bxy(1413),bxy(1414),bxy(1415),bxy(1416)/  0,  6,  3,  6/
       data bxy(1417),bxy(1418),bxy(1419),bxy(1420)/  5,-64,  0,  5/
       data bxy(1421),bxy(1422),bxy(1423),bxy(1424)/  0,  1,  1,  0/
       data bxy(1425),bxy(1426),bxy(1427),bxy(1428)/  2,  0,  3,  1/
       data bxy(1429),bxy(1430),bxy(1431),bxy(1432)/  3,  4,-64,  3/
       data bxy(1433),bxy(1434),bxy(1435),bxy(1436)/  1,  4,  0,  5/
       data bxy(1437),bxy(1438),bxy(1439),bxy(1440)/  0,  6,  1,  6/
       data bxy(1441),bxy(1442),bxy(1443),bxy(1444)/  5,  0,  0,  5/
       data bxy(1445),bxy(1446),bxy(1447),bxy(1448)/  5,-64,  0,  5/
       data bxy(1449),bxy(1450),bxy(1451),bxy(1452)/  5,  0,-64,  0/
       data bxy(1453),bxy(1454),bxy(1455),bxy(1456)/  5,  0,  1,  1/
       data bxy(1457),bxy(1458),bxy(1459),bxy(1460)/  0,  4,  0,  5/
       data bxy(1461),bxy(1462),bxy(1463),bxy(1464)/  1,-64,  5,  5/
       data bxy(1465),bxy(1466),bxy(1467),bxy(1468)/  5, -3,  4, -4/
       data bxy(1469),bxy(1470),bxy(1471),bxy(1472)/  1, -4,  0, -3/
       data bxy(1473),bxy(1474),bxy(1475),bxy(1476)/-64,  0,  5,  5/
       data bxy(1477),bxy(1478),bxy(1479),bxy(1480)/  5,  0,  0,  5/
       data bxy(1481),bxy(1482),bxy(1483),bxy(1484)/  0,-64,  3,  8/
       data bxy(1485),bxy(1486),bxy(1487),bxy(1488)/  2,  8,  1,  7/
       data bxy(1489),bxy(1490),bxy(1491),bxy(1492)/  1,  5,  0,  4/
       data bxy(1493),bxy(1494),bxy(1495),bxy(1496)/  1,  3,  1,  1/
       data bxy(1497),bxy(1498),bxy(1499),bxy(1500)/  2,  0,  3,  0/
       data bxy(1501),bxy(1502),bxy(1503),bxy(1504)/-64,  3,  8,  3/
       data bxy(1505),bxy(1506),bxy(1507),bxy(1508)/  0,-64,  3,  8/
       data bxy(1509),bxy(1510),bxy(1511),bxy(1512)/  4,  8,  5,  7/
       data bxy(1513),bxy(1514),bxy(1515),bxy(1516)/  5,  5,  6,  4/
       data bxy(1517),bxy(1518),bxy(1519),bxy(1520)/  5,  3,  5,  1/
       data bxy(1521),bxy(1522),bxy(1523),bxy(1524)/  4,  0,  3,  0/
       data bxy(1525),bxy(1526),bxy(1527),bxy(1528)/-64,  0,  4,  1/
       data bxy(1529),bxy(1530),bxy(1531),bxy(1532)/  5,  2,  5,  4/
       data bxy(1533),bxy(1534),bxy(1535),bxy(1536)/  3,  5,  3,  6/
       data bxy(1537),bxy(1538),bxy(1539),bxy(1540)/  4,  0,  0,  0/
       data bxy(1541),bxy(1542),bxy(1543),bxy(1544)/  8,  6,  8,  6/
       data bxy(1545),bxy(1546),bxy(1547),bxy(1548)/  0,  0,  0,  6/
       data bxy(1549),bxy(1550),bxy(1551),bxy(1552)/  8,-64,  0,  8/
       data bxy(1553),bxy(1554),bxy(1555),bxy(1556)/  6,  0,  0,  0/
      end
