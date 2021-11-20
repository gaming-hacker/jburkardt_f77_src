c  SMDLIB.F  20 July 1993
c
      subroutine ALPHA1(LALPHA,LCHAR)
c
c***********************************************************************
c
C     SET MIX ALPHABET SET
C              (LEVEL 1-3, P/S)
C
C     INPUT:   LALPHA = character SET
C              LCHAR  = FONT CHANGE INDICATOR
C
      character CZALFL*1,CZALFN*5,CZALFS*5
      character*(*) LALPHA,LCHAR
      integer kzbegn
      integer kzlevl
c
      save clevel
      save cmxalf
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS

      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFL(1)=LCHAR(1:1)
        CZALFN(1)=LALPHA(1:5)
        call CAPCHR(CZALFN(1))
      else
        call ERRMES('ALPHA1',1,3)
      endif

      return
      end
      subroutine ALPHA2(LALPHA,LCHAR)
c
c***********************************************************************
c
C     SET MIX ALPHABET SET
C              (LEVEL 1-3, P/S)
C
C     INPUT:   LALPHA = character SET
C              LCHAR  = FONT CHANGE INDICATOR
C
      character CZALFL*1,CZALFN*5,CZALFS*5
      character*(*) LALPHA,LCHAR
      integer kzbegn
      integer kzlevl
c
      save clevel
      save cmxalf
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS

      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFL(2)=LCHAR(1:1)
        CZALFN(2)=LALPHA(1:5)
        call CAPCHR(CZALFN(2))
        if(CZALFL(1).EQ.' ') CALL DEFALF('STAND')
      else
        call ERRMES('ALPHA2',1,3)
      endif
      return
      end
      subroutine ALPHA3(LALPHA,LCHAR)
c
c***********************************************************************
c
C     SET MIX ALPHABET SET
C              (LEVEL 1-3, P/S)
C
C     INPUT:   LALPHA = character SET
C              LCHAR  = FONT CHANGE INDICATOR
C
      integer kzbegn
      integer kzlevl
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      character*(*) LALPHA,LCHAR
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFL(3)=LCHAR(1:1)
        CZALFN(3)=LALPHA(1:5)
        call CAPCHR(CZALFN(3))
        if(CZALFL(1).EQ.' ') CALL DEFALF('STAND')
      else
        call ERRMES('ALPHA3',1,3)
      endif
      return
      end
      subroutine ALPHA4(LALPHA,LCHAR)
c
c***********************************************************************
c
C     SET MIX ALPHABET SET
C              (LEVEL 1-3, P/S)
C
C     INPUT:   LALPHA = character SET
C              LCHAR  = FONT CHANGE INDICATOR
C
      integer kzbegn
      integer kzlevl
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      character*(*) LALPHA,LCHAR
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFL(4)=LCHAR(1:1)
        CZALFN(4)=LALPHA(1:5)
        call CAPCHR(CZALFN(4))
        if(CZALFL(1).EQ.' ') CALL DEFALF('STAND')
      else
        call ERRMES('ALPHA4',1,3)
      endif
      return
      end
      subroutine ALPHA5(LALPHA,LCHAR)
C
C
C
C
C     SET MIX ALPHABET SET
C              (LEVEL 1-3, P/S)
C
C     INPUT:   LALPHA = character SET
C              LCHAR  = FONT CHANGE INDICATOR
C
      integer kzbegn
      integer kzlevl
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      character*(*) LALPHA,LCHAR
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFL(5)=LCHAR(1:1)
        CZALFN(5)=LALPHA(1:5)
        call CAPCHR(CZALFN(5))
        if(CZALFL(1).EQ.' ') CALL DEFALF('STAND')
      else
        call ERRMES('ALPHA5',1,3)
      endif
      return
      end
      subroutine ALPHA6(LALPHA,LCHAR)
C
C
C
C
C     SET MIX ALPHABET SET
C              (LEVEL 1-3, P/S)
C
C     INPUT:   LALPHA = character SET
C              LCHAR  = FONT CHANGE INDICATOR
C
      integer kzbegn
      integer kzlevl
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      character*(*) LALPHA,LCHAR
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFL(6)=LCHAR(1:1)
        CZALFN(6)=LALPHA(1:5)
        call CAPCHR(CZALFN(6))
        if(CZALFL(1).EQ.' ') CALL DEFALF('STAND')
      else
        call ERRMES('ALPHA6',1,3)
      endif
      return
      end
      subroutine ANGLE(ANG)
c
c***********************************************************************
c
C      SET TEXT ANGLE
C               LEVEL 1-3, P/S
C
C      INPUT:   ANG = TEXT ANGLE
C
      real ang
      integer kzbegn
      integer kzlevl
      integer kzstrm
      integer kztmln
      real uuhite
      real zzangl
      real zzhite
c
      save clevel
      save cstrng
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
C
      if(KZLEVL.EQ.1)then
        ZZANGL=ANG
        call GSSETC(UUHITE,ZZANGL)
      elseif(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
        ZZANGL=ANG
        call GSSETC(ZZHITE,ZZANGL)
      else
        call ERRMES('ANGLE',1,3)
      endif
      return
      end
      subroutine AXES2D(XMIN,XSTP,XMAX,YMIN,YSTP,YMAX)
C
c***********************************************************************
C
C     SET UP LINEAR COORDINATE SYSTEM
C             (LEVEL 2, RAISE TO LEVEL 3
C
C     INPUT:   XMIN,YMIN = LOWER LIMITS OF X AND Y
C              XMAX,YMAX = UPPER LIMITS OF X AND Y
C              XSTP,YSTP = STEP SIZE IN X AND Y
C                          IF = 'SCAL', A STEP SIZE IS
C                          ASSIGNED
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
c
      character*(NUMBYT) CXSTR,CYSTR
      LOGICAL LOGX, LOGY
c
      save ccoord
      save clabel
      save clevel
      save pltcom
c
      common /CCOORD/ ZZXMIN,ZZXMAX,ZZXSTP,ZZYMIN,ZZYMAX,ZZYSTP
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /CLEVEL/ KZLEVL,KZBEGN
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY

C
      if(KZLEVL.EQ.2)then
C
C  INIT IAXES TO DEFINE X AND Y AXIS AND DRAW TITLE
C
        LOGX=.FALSE.
        LOGY=.FALSE.
        IAXES=19
        XSTEP=XSTP
        YSTEP=YSTP
c
C:  REQUIRED TO ELIMINATE EQUIVALENCING PROBLEM
c
        call WIN2CH(XSTEP,CXSTR)
        call WIN2CH(YSTEP,CYSTR)
        call CAPCHR(CXSTR)
        if(CXSTR(1:4).EQ.'SCAL')then
          IAXES=IAXES+4
        else
          ZZXSTP=XSTP
        endif
        call CAPCHR(CYSTR)
        if(CYSTR(1:4).EQ.'SCAL')then
          IAXES=IAXES+8
        else
          ZZYSTP=YSTP
        endif
        call ZMAPIT(XMIN,XMAX,YMIN,YMAX,KZXLAB,KZXALN,
     *              KZYLAB,KZYALN,0.0,0.0,IAXES)
        KZLEVL=3
      else
        call ERRMES('AXES2D',2,0)
      endif
      return
      end
      subroutine AXIS(BLOW,BHIGH,MAXTKS,LSHORT,LRAGGD,BMIN,BMAX,
     1   BTMIN,BTMAX,BTICK,IPWR)
C
c***********************************************************************
c
C     THIS ROUTINE IS MAINLY FOR INTERNAL USE,
C     ITS FUNCTION IS TO DETERMINE A SUITABLE
C     "TICK" DISTANCE OVER THE RANGE SPECIFIED BETWEEN
C     ALOW AND AHIGH.   IT OUTPUTS THE AXIS RANGE BMIN,BMAX
C     AND THE TICK DISTANCE BTICK STRIPPED OF THEIR POWER OF
C     TEN.   THE POWER OF TEN IS returnED IN THE VAR. IPWR.
C
      real toocls
      parameter (toocls=0.8)
c
      DIMENSION JTICKS(6)
      LOGICAL LDIVDS
      LOGICAL LSHORT, LRAGGD
      LOGICAL LISNEG
C
C  IF A RAGGED AXIS IS "TOO CLOSE" TO THE NEXT TICK, THEN EXTEND IT.
C  THE "TOO CLOSE" parameter IS THE VARIABLE TOOCLS
C
      SAVE FUZZ,JTICKS
C
      data FUZZ /0.001/
      data JTICKS /1,2,5,4,3,10/
C
      MAXTKS = MAX0(1,MAXTKS)
      MINTKS = MAX0(1,MAXTKS/2)
      BMAX = BHIGH
      BMIN = BLOW
      LISNEG = .FALSE.
      if(BMAX .GE. BMIN) go to 30
      BMAX = BLOW
      BMIN = BHIGH
      LISNEG = .TRUE.
C
C  MAKE SURE WE HAVE ENOUGH RANGE, IF NOT, INCREASE AHIGH
C
30    RANGE = BMAX - BMIN
      TEMP = AMAX1(ABS(BMIN),ABS(BMAX))
      if(TEMP .EQ. 0.0) TEMP = 10.0
      if(RANGE/TEMP .GE. 5.0E-3) go to 40
       BMIN = BMIN - 5.0E-3*TEMP
       BMAX = BMAX + 5.0E-3*TEMP
40    continue
C
C  STRIP THE RANGE OF ITS POWER OF TEN
C
      IPWR=INT(ALOG10(BMAX-BMIN)-2)
      TENX = 10.0**IPWR
50    continue
      ASTRT = AINT(BMIN/TENX)
      AFIN = AINT(BMAX/TENX+0.999)
      if(AFIN*TENX .LT. BMAX) AFIN = AFIN + 1
      RANGE = AFIN - ASTRT
      if(RANGE .LE. 10*MAXTKS) go to 75
      IPWR = IPWR + 1
      TENX=TENX*10.0
      go to 50
75    continue
C
C  SEARCH FOR A SUITABLE TICK
C
      BTICK = 0
      DO 100 I=1,6
      TICK = JTICKS(I)
      NTICK = INT(RANGE/TICK+0.999)
      if(NTICK .LT. MINTKS .OR. NTICK .GT. MAXTKS) go to 100
      if(LDIVDS(ASTRT,TICK) .AND. LDIVDS(AFIN,TICK)) go to 150
      if(BTICK .EQ. 0) BTICK = TICK
100   continue
C
C  USE BEST NON-PERFECT TICK
C
      go to 160
C
C  FOUND A GOOD TICK
C
150   BTICK=JTICKS(I)
160   continue
      if(BTICK .NE. 10.0) go to 165
        BTICK = 1.0
        IPWR = IPWR + 1
        TENX = 10.0*TENX
165   TICK = BTICK*TENX
C
C  FIGURE OUT TICK LIMITS
C
      BTMIN = BTICK*AINT(BMIN/TICK)
      if(BTMIN*TENX .LT. BMIN) BTMIN = BTMIN + BTICK
      BTMAX = BTICK*AINT(BMAX/TICK)
      if(BTMAX*TENX .GT. BMAX) BTMAX = BTMAX - BTICK
      NINTVL = int((BTMAX-BTMIN)/BTICK)
C
C  IF USER ABSOLUTELY MUST HAVE RAGGED AXIS, THEN FORCE IT.
C
      if(LSHORT .AND. LRAGGD) go to 180
C
C  CHECK INDIVIDUALLY
C
      if(LSHORT .AND. (NINTVL .GT. 0) .AND.
     1   ((BTMIN-BMIN/TENX)/BTICK .LE. TOOCLS) ) go to 170
        if((BTMIN-BMIN/TENX) .GT. FUZZ) BTMIN = BTMIN - BTICK
        BMIN = BTMIN*TENX
170   continue
      if(LSHORT .AND. (NINTVL .GT. 0) .AND.
     1   ((BMAX/TENX-BTMAX)/BTICK .LE. TOOCLS) ) go to 180
        if((BMAX/TENX-BTMAX) .GT. FUZZ) BTMAX = BTMAX + BTICK
        BMAX = BTMAX*TENX
180   continue
      if(.NOT. LISNEG) go to 200
C
C  SWITCH BACK TO BACKWARDS
C
      BTICK = -BTICK
      TEMP = BMIN
      BMIN = BMAX
      BMAX = TEMP
      TEMP = BTMIN
      BTMIN = BTMAX
      BTMAX = TEMP
200   continue
      return
      end
      subroutine bargra(xlow,xhigh,nbar,nx,x,sxlab,sylab,
     &  stitle,itype)
c
c***********************************************************************
c
c  BARGRA makes a bar graph from an array of real data.
c
c  BARGRA does not blank the screen before drawing.  Therefore,
c  if a previous plot has been made, the user should call
c
c    CALL GSDRVR(2,0.0,0.0)
c
c  to clear the screen first.
c
c  The MAPIT routine uses its own rules for the actual lowest and
c  highest values on the axes.  They always include the user's
c  values.  If you wish to move the bar graph away from the left
c  or right y axes do the following:
c
c    Let S = (XH - XL) / NBAR
c
c  where
c
c    XH = max X(i)
c
c  and
c
c    XL = min X(i)
c
c  Now set
c
c    XLOW = XL - N * S
c    XHIGH = XH + M * S
c
c  where N and M are chosen at your discretion.
c
c  XLOW,
c  XHIGH  Input, REAL XLOW, XHIGH, the lower and upper limits
c         for the horizontal axis.  It must be the case that
c         for each data value X(I), XLOW <= X(I) <= XHIGH.
c
c  NBAR   Input, integer NBAR, the number of bars to draw.
c         NBAR must be at least 1, and no more than 200.
c
c  NX     Input, integer NX, the number of data values in X.
c
c  X      Input, REAL X(NX), the data from which the bar graph
c         is to be drawn.
c
c  SXLAB  Input, character*(*) SXLAB, the X axis label.
c
c  SYLAB  Input, character*(*) SYLAB, the Y axis label.
c
c  STITLE Input, character*(*) STITLE, the plot title.
c
c  ITYPE  Input, integer ITYPE, the axis flag.
c
c         0, X and Y axes normal.
c         1, X logarithmic, Y normal.
c         2, X normal, Y logarithmic.
c         3, X and Y logarithmic.
c         256, X and Y normal, but axes are ragged.
c
      integer maxc
      parameter (maxc=200)
c
      real count(maxc)
      real dumx(3)
      real dumy(8)
      integer i
      integer itype
      integer j
      integer nx
      integer nbar
      real step
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
        write(*,*)'BARGRA - Fatal error!'
        write(*,*)'XLOW is greater than or equal to XHIGH.'
        stop
      endif

      if(nbar.le.0)then
        write(*,*)'BARGRA - Fatal error!'
        write(*,*)'Number of bars requested is too small.'
        write(*,*)'Requested number = ',nbar
        write(*,*)'But must be at least 1!'
        stop
      elseif(nbar.gt.maxc)then
        write(*,*)'BARGRA - Fatal error!'
        write(*,*)'Number of bars requested is too large.'
        write(*,*)'Requested number = ',nbar
        write(*,*)'Maximum allowed=   ',maxc
        stop
      endif
 
      ylow=0.0
      yhigh=1.0
      step=(xhigh-xlow)/real(nbar)
c
c  Set the counters for each bar to zero, and then find where
c  each item of data falls. 
c
      do 100 i=1,nbar
        count(i)=0.0
 100  continue
c
      do 200 i=1,nx
        j=1+int(nbar*(x(i)-xlow)/(xhigh-xlow))
        if(j.ge.1.and.j.le.nbar)then
          count(j)=count(j)+1.0
        endif
 200  continue
 
      call minmax(count,nbar,ylow,yhigh)
      ylow=0.0
      yhigh=yhigh+0.1*yhigh
c
c  The graph is to take up the whole screen.
c
      call mapsiz(0.0,100.0,0.0,100.0,0.0)
c
c  Give the coordinate ranges and titles and axis type.
c
      call mapit(xlow,xhigh,ylow,yhigh,sxlab,sylab,stitle,itype)
 
      x0=xlow
      y0=0.0
      call scale(x0,y0,vx0,vy0)
      call gsmove(vx0,vy0)
 
      do 400 i=1,nbar

        if(nbar.gt.1)then
          x0=((nbar-i)*xlow+(i-1)*xhigh)/real(nbar-1)
        else
          x0=0.5*(xlow+xhigh)
        endif

        y0=count(i)
        call scale(x0,y0,vx1,vy1)
        call gsdraw(vx0,vy1)
        call gsdraw(vx1,vy1)
        call gsdraw(vx1,vy0)
        vx0=vx1
 400  continue
 
      call gsdrvr(5,dumx,dumy)
      return
      end
      subroutine bargra2(xlow,xhigh,nbar,sbar,sxlab,sylab,
     &  stitle,itype)
c
c***********************************************************************
c
c  BARGRA2 makes a bar graph from a set of binned data.
c
c  BARGRA2 does not blank the screen before drawing.  Therefore,
c  if a previous plot has been made, the user should call
c
c    CALL GSDRVR(2,0.0,0.0)
c
c  to clear the screen first.
c
c  XLOW,
c  XHIGH  Input, REAL XLOW, XHIGH, the data values associated
c         with the first and last bars.
c
c  NBAR   Input, integer NBAR, the number of bars to draw.
c         NBAR must be at least 1, and no more than 200.
c
c  SBAR   Input, REAL SBAR(NBAR), the height of the bars.
c
c  SXLAB  Input, character*(*) SXLAB, the X axis label.
c
c  SYLAB  Input, character*(*) SYLAB, the Y axis label.
c
c  STITLE Input, character*(*) STITLE, the plot title.
c
c  ITYPE  Input, integer ITYPE, the axis flag.
c
c         0, X and Y axes normal.
c         1, X logarithmic, Y normal.
c         2, X normal, Y logarithmic.
c         3, X and Y logarithmic.
c         256, X and Y normal, but axes are ragged.
c
      integer nbar
c
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
        write(*,*)'BARGRA2 - Fatal error!'
        write(*,*)'Number of bars requested is too small.'
        write(*,*)'Requested number = ',nbar
        write(*,*)'But must be at least 1!'
        stop
      endif
c
      call minmax(sbar,nbar,ylow,yhigh)
      if(ylow.gt.0.0)then
        ylow=0.0
      endif

      yspan=yhigh-ylow

      yhigh=yhigh+0.05*yspan
c
c  The graph is to take up the whole screen.
c
      call mapsiz(0.0,100.0,0.0,100.0,0.0)
c
c  Give the coordinate ranges and titles and axis type.
c
      call mapit(xlow,xhigh,ylow,yhigh,sxlab,sylab,stitle,itype)
 
      x0=xlow
      y0=0.0
      call scale(x0,y0,vx0,vy0)
      call gsmove(vx0,vy0)
 
      do 400 i=1,nbar
        if(nbar.gt.1)then
          x0=((nbar-i)*xlow+(i-1)*xhigh)/real(nbar-1)
        else
          x0=0.5*(xlow+xhigh)
        endif
        y0=sbar(i)
        call scale(x0,y0,vx1,vy1)
        call gsdraw(vx0,vy1)
        call gsdraw(vx1,vy1)
        call gsdraw(vx1,vy0)
        vx0=vx1
 400  continue
 
      call gsdrvr(5,dumx,dumy)
      return
      end
      subroutine BGNSUB(ITITLE,NTITLE,IXLAB,NXLAB,IYLAB,NYLAB,
     *                   AX,AY)
c
c***********************************************************************
C
C      DRAW FRAME, SET SUBPLOT AREA AND OTHER
C               SYSTEM VARIABLES
C               (LEVEL 1, RAISED TO 2)
C
C      INPUT:   ITITLE = TITLE character STRING
C               NTITLE = NUMBER OF characterS IN ITITLE
C               IXLAB  = X AXIS LABEL
C               NXLAB  = NUMBER OF characterS IN IXLAB
C               IYLAB  = Y AXIS LABEL
C               NYLAB  = NUMBER OF characterS IN IYLAB
C               AX,AY  = DISTANCE OF LOWER LEFT CORNER OF
C                        SUBPLOT AREA FROM THAT OF PAGE
C
      parameter (KZYES=111)
      parameter (KZNO=222)
      parameter (KZCLIP=12)
      parameter (KZDOWN=13)
      parameter (KZSCRN=14)
      parameter (KZABRT=15)
      parameter (ZZIN=2.54)
      parameter (KQMS=1200)
      parameter (KLN03=3)
      parameter (KPOST=910)
c
      save carea
      save cborch
      save ciount
      save clabel
      save clevel
      save cline
      save cpage
      save cphysr
      save cstrng
      save csymbo
      save cunit
      save gcclip
      save gcdchr
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CSYMBO/ KZSYM,KZNSYM,ZZSMSZ,UUSMSZ
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CBORCH/ KZBRDR,KZCHEK
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CIOUNT/ KZIUNT, KZOUNT
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
C
      DIMENSION ITITLE(*),IXLAB(*),IYLAB(*)
C
 10   FORMAT(1X,3A)
C
C  CHECK LEVEL
C
      if(KZLEVL.EQ.1)then
C
C                        LOAD AXIS LABELS AND TITLE
C
        if(NXLAB.EQ.0)then
          KZXALN=0
        else
          KZXALN=80
          call ZCOPYS(IXLAB,NXLAB,KZXLAB,KZXALN)
          if(KZXALN.LT.0)then
            KZXALN=-KZXALN
            call ERRMES('BGNSUB',80,6)
          endif
        endif
        if(NYLAB.EQ.0)then
          KZYALN=0
        else
          KZYALN=80
          call ZCOPYS(IYLAB,NYLAB,KZYLAB,KZYALN)
          if(KZYALN.LT.0)then
            KZYALN=-KZYALN
            call ERRMES('BGNSUB',80,7)
          endif
        endif
        if(NTITLE.EQ.0)then
          KZTILN=0
        else
          KZTILN=80
          call ZCOPYS(ITITLE,NTITLE,KZTITL,KZTILN)
          if(KZTILN.LT.0)then
            KZTILN=-KZTILN
            call ERRMES('BGNSUB',80,8)
          endif
        endif
C
C                        SCALE QMS
C
        KDEV=INT(DEVID)
        if((KDEV.EQ.KQMS.OR.KDEV.EQ.KLN03.OR.KDEV.EQ.KPOST)
     *                .AND.KZAUTO.EQ.KZYES)then
          if(UUPAGX.LT.UUPAGY)then
            call ZLASAP
          else
            call ZLASAL
          endif
        endif
C
C                        ERASE SCREEN
C
        if(KZBEGN.NE.KZYES)then
C          call GSDRVR(2,DUMMY,DUMMY)
          KZBEGN=KZYES
        endif
C
C                        CONVERT TO SYSTEM UNIT
C
        RX=XLENCM/UUPAGX
        RY=YLENCM/UUPAGY
        if(RX.LT.RY)then
          ZZPAGR=RX
        else
          ZZPAGR=RY
        endif
        if(KZSCAL.EQ.KZDOWN)then
          if(ZZPAGR.GT.1.0)then
            ZZPAGR=1.0
          endif
          ZZPAGX=UUPAGX*ZZPAGR
          ZZPAGY=UUPAGY*ZZPAGR
        elseif(KZSCAL.EQ.KZSCRN)then
          ZZPAGX=UUPAGX*ZZPAGR
          ZZPAGY=UUPAGY*ZZPAGR
        elseif(KZSCAL.EQ.KZCLIP)then
          ZZPAGX=UUPAGX
          ZZPAGY=UUPAGY
          ZZPAGR=1.0
        elseif(KZSCAL.EQ.KZABRT)then
          if(RX.LT.1.0.OR.RY.LT.1.0)then
            KZLEVL=1
            return
          else
            ZZPAGX=UUPAGX
            ZZPAGY=UUPAGY
            ZZPAGR=1.0
          endif
        endif
        if(KZLTHK.EQ.KZYES) ZZLTHK=UULTHK*ZZPAGR
        ZZSMSZ=UUSMSZ*ZZPAGR
        ZZGRCE=UUGRCE*ZZPAGR
        ZZHITE=UUHITE*ZZPAGR
        call GSSETC(ZZHITE,ZZANGL)
        ZZSMSZ=UUSMSZ*ZZPAGR
        UUYAXS=AY*ZZUNIT
        UUXAXS=AX*ZZUNIT
        ZZXAXS=UUXAXS*ZZPAGR
        ZZYAXS=UUYAXS*ZZPAGR
        ZZFRME=UUFRME*ZZPAGR
C
C  CHECK IF 'ORIGIN' IS CALLED
C
        if(KZOR.NE.KZYES)then
          DX=UUPAGX-UUXAXS
          DY=UUPAGY-UUYAXS
          if(DX.GT.1.0)then
            UUXOR=DX/2.0
          else
            UUXOR=0.5*ZZIN
            if(DX.LT.0.5)then
            call ERRMES('BGNSUB',0,4)
            endif
          endif
          if(DY.GT.1.0)then
            UUYOR=DY/2.0
          else
            UUYOR=0.5*ZZIN
            if(DY.LT.0.5)then
            call ERRMES('BGNSUB',0,5)
            endif
          endif
        endif
C
C  CALCULATE FRAME
C
        ZZXOR=UUXOR*ZZPAGR
        ZZYOR=UUYOR*ZZPAGR
        PXMIN=(XLENCM-ZZPAGX)/2.0
        PYMIN=(YLENCM-ZZPAGY)/2.0
        PXMAX=PXMIN+ZZPAGX
        PYMAX=PYMIN+ZZPAGY
        XCM0=PXMIN
        XCM1=PXMAX
        YCM0=PYMIN
        YCM1=PYMAX
C
C  CHECK IF 'NOBORD' IS CALLED
C
        if(KZBRDR.NE.KZNO)then
          call DSMOVE(PXMIN,PYMIN)
          call DSDRAW(PXMIN,PYMAX)
          call DSDRAW(PXMAX,PYMAX)
          call DSDRAW(PXMAX,PYMIN)
          call DSDRAW(PXMIN,PYMIN)
        endif
C
C  SET PLOT AREA parameterS
C
        ZZXLFT=PXMIN+ZZXOR
        ZZXAXR=(PXMAX-ZZXLFT)/ZZXAXS
        if(ZZXAXR.GT.1.0)then
          ZZXAXR=1.0
          ZZXRGT=ZZXLFT+ZZXAXS
        else
          ZZXRGT=PXMAX
        endif
        ZZYBOT=PYMIN+ZZYOR
        ZZYAXR=(PYMAX-ZZYBOT)/ZZYAXS
        if(ZZYAXR.GT.1.0)then
          ZZYAXR=1.0
          ZZYTOP=ZZYBOT+ZZYAXS
        else
          ZZYTOP=PYMAX
        endif
        TICK=0.6*ZZHITE
        call ZMAPRM(ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,ZZHITE,TICK)
        KZLEVL=2
      else
        call ERRMES('BGNSUB',1,0)
      endif
      return
      end
      subroutine BLANK1(XPOS1,XPOS2,YPOS1,YPOS2,IFRAME)
C
C
C
C      DEFINE BLANK AREA 1
C               LEVEL 2,3 P/S
C
C      INPUT:   XPOS1,XPOS2 = X LIMITS IN INCHES
C                             FROM PHYSICAL ORIGIN
C               YPOS1,YPOS2 = Y LIMITS IN INCHES
C                             FROM PHYSICAL ORIGIN
C               IFRAME      = NUMBER OF FRAMES TO BE
C                             DRAWN AROUND BLANK AREA
C
      common /CLEVEL/ KZLEVL,KZBEGN
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
        ID=1
        call ZBLANK(XPOS1,XPOS2,YPOS1,YPOS2,ID,IFRAME)
      else
        call ERRMES('BLANK1',2,3)
      endif
      return
      end
      subroutine BLANK2(XPOS1,XPOS2,YPOS1,YPOS2,IFRAME)
C
C
C
C      DEFINE BLANK AREA 2
C               (LEVEL 2,3 P/S)
C
C      INPUT:   XPOS1,XPOS2 = X LIMITS IN INCHES
C                             FROM PHYSICAL ORIGIN
C               YPOS1,YPOS2 = Y LIMITS IN INCHES
C                             FROM PHYSICAL ORIGIN
C               IFRAME      = NUMBER OF FRAMES TO BE
C                             DRAWN AROUND BLANK AREA
C
C
      common /CLEVEL/ KZLEVL,KZBEGN
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
        ID=2
        call ZBLANK(XPOS1,XPOS2,YPOS1,YPOS2,ID,IFRAME)
      else
        call ERRMES('BLANK2',2,3)
      endif
      return
      end
      subroutine BLANK3(XPOS1,XPOS2,YPOS1,YPOS2,IFRAME)
C
C
C
C      DEFINE BLANK AREA 3
C               (LEVEL 2,3 P/S)
C
C      INPUT:   XPOS1,XPOS2 = X LIMITS IN INCHES
C                             FROM PHYSICAL ORIGIN
C               YPOS1,YPOS2 = Y LIMITS IN INCHES
C                             FROM PHYSICAL ORIGIN
C               IFRAME      = NUMBER OF FRAMES TO BE
C                             DRAWN AROUND BLANK AREA
C
      common /CLEVEL/ KZLEVL,KZBEGN
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
        ID=3
        call ZBLANK(XPOS1,XPOS2,YPOS1,YPOS2,ID,IFRAME)
      else
        call ERRMES('BLANK3',2,3)
      endif
      return
      end
      subroutine BLANK4(XPOS1,XPOS2,YPOS1,YPOS2,IFRAME)
C
C
C
C      DEFINE BLANK AREA 4
C               (LEVEL 2,3 P/S)
C
C      INPUT:   XPOS1,XPOS2 = X LIMITS IN INCHES
C                             FROM PHYSICAL ORIGIN
C               YPOS1,YPOS2 = Y LIMITS IN INCHES
C                             FROM PHYSICAL ORIGIN
C               IFRAME      = NUMBER OF FRAMES TO BE
C                             DRAWN AROUND BLANK AREA
C
      common /CLEVEL/ KZLEVL,KZBEGN
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
        ID=4
        call ZBLANK(XPOS1,XPOS2,YPOS1,YPOS2,ID,IFRAME)
      else
        call ERRMES('BLANK4',2,3)
      endif
      return
      end
      subroutine BLANKA(XORG,YORG,WIDE,HIGH,FRM)
C
C
C
C      DEFINE BLANK AREA 5
C               (LEVEL 2,3 P/S)
C
C      INPUT:   XORG,YORG = COORDINATE OF LOWER LEFT
C                           CORNER, INCHES FROM ORIGIN
C               WIDE      = WIDTH OF RECTANGLE, INCHES
C               HIGH      = HEIGHT OF RECTANGLE, INCHES
C               FRM       = FRAME WIDTH, NEGATIVE SIGN
C                           MEANS THICKEN TOWARDS OUTSIDE,
C                           ABSOLUTE VALUE INDICATES FRAME
C                           WIDTH:
C                            IF = 0.0 THEN NO FRAME
C                            IF > 0.0 AND < 1.0 THEN
C                            FRAME WIDTH = FRM INCHES
C                            IF >= 1.0 THEN
C                            FRAME WIDTH = FRM * 0.01"
C
      parameter (KZMXBS=1000)
      parameter (KZMXBL=200)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CBLANK/ ZZBLNK(KZMXBL,4),ZZBLKS(KZMXBS,4),KZBLCN,KZBSCN
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
        KZBLCN=KZBLCN+1
        if(KZBLCN.GT.KZMXBL)then
          call ERRMES('BLANKA',0,4)
          KZBLCN=KZMXBL
        else
          XPOS1=XORG
          YPOS1=YORG
          XPOS2=XORG+WIDE
          YPOS2=YORG+HIGH
          ABSFRM=ABS(FRM)
C
C                        NO FRAME
C
          if(ABSFRM.LT.0.00001)then
            IFRAME=0
C
C                        FRAME = FRM
C
          elseif(ABSFRM.LT.1.0)then
            TEMP=ABSFRM/0.01
            IFRAME=INT(TEMP)
C
C                        FRAME = FRM TIMES 0.01
C
          else
            IFRAME=INT(ABSFRM)
          endif
          if(FRM.LT.0.0) IFRAME=-IFRAME
          call ZBLANK(XPOS1,XPOS2,YPOS1,YPOS2,KZBLCN,IFRAME)
        endif
      else
        call ERRMES('BLANKA',2,3)
      endif
      return
      end
      subroutine BLANKS
C
C
C
C
C      SET FLAG TO BLANK CURVE AT SYMBOL AREA
C               LEVEL 1-3, P/S
C
      parameter (KZMXBS=1000)
      parameter (KZMXBL=200)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CBLANK/ ZZBLNK(KZMXBL,4),ZZBLKS(KZMXBS,4),KZBLCN,KZBSCN
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        if(KZBSCN.LT.0) KZBSCN=0
      else
        call ERRMES('BLANKS',1,3)
      endif
      return
      end
      subroutine BTEXT(IP,NLINES,XPOS,YPOS)
C
C     Author: Eric A. Hibbard, NASA Ames Research Center, CA
C
C     WRITE PACKED character ARRAY, CENTERED
C              (LEVEL 2,3)
C
C     INPUT:   IP     = PACKED ARRAY OF characterS
C              NLINES = NUMBER OF LINES IN PACKED ARRAY
C              XPOS   = X VALUE FROM PHYSICAL ORIGIN IN INCHES
C              YPOS   = Y VALUE FROM PHYSICAL ORIGIN IN INCHES
C
      parameter (KZYES=111)
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
C
      DIMENSION IP(*)
      character*5 CFLAG(6)*1,CFONT(6),CSTYLE
      EQUIVALENCE (IVAL,RVAL)
C
      SAVE JTEXT,JHITE,JYRAT
C
      data JTEXT,JHITE,JYRAT /1,-2,-1/
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
C
C  SAVE OLD TEXT parameterS
C
        OHITE=ZZHITE
        OANGLE=ZZANGL
C
C  CALCULATE VIRTUAL COORDINATES AND
C  LENGTH OF A LINE OF characterS
C
        VX=ZZXOR+XCM0+XPOS*ZZUNIT*ZZPAGR
        VY=ZZYOR+YCM0+YPOS*ZZUNIT*ZZPAGR
        if(IP(1).EQ.KZYES)then
          IPLEN=IP(2)/1000
          LLEN=IP(2)-IPLEN*1000
        else
          LLEN=40
        endif
        NWORD=(LLEN-1)/KZBYTE+5
        XLEN=XBTEXT(IP,NLINES)*ZZUNIT*ZZPAGR
        YLEN=YBTEXT(IP,NLINES)*ZZUNIT*ZZPAGR
        ICUR=2+NWORD
        IVAL=IP(ICUR)
        AVE=RVAL
        IVAL=IP(ICUR+JYRAT)
        YRAT=RVAL
        if(YRAT.LT.1.0) YRAT=1.0
        VX=VX-(XLEN/2.0)
        VY=VY+YLEN+(YRAT-1.0)*AVE/2.0
C
C                        FOR EACH LINE IN ARRAY:
C                        RETRIEVE LINE SPACE parameter, character
C                        HEIGHT, AND character FONT, SET TO CURRENT
C                        AND DRAW TO OUTPUT DEVICE
C
        ICUR=2
        DO 100 I=1,NLINES
          ICUR=ICUR+NWORD
          IVAL=IP(ICUR+JYRAT)
          YRAT=RVAL
          if(YRAT.LT.1.0) YRAT=1.0
          IVAL=IP(ICUR+JHITE)
          HITE=RVAL
          DELTA=(YRAT-1.0)*AVE/2.0
          VY=VY-DELTA-HITE
          DO 800 K=1,6
            CFLAG(K)=CZLGAC(I,K)
            CFONT(K)=CZLGAF(I,K)
 800      continue
          CSTYLE=CZLGAS(I)
          call GSSETC(HITE,0.0)
          call DSMOVE(VX,VY)
          call ZTEXT(IP(ICUR-NWORD+JTEXT),100,
     *                 CFLAG,CFONT,CSTYLE)
          VY=VY-DELTA
 100    continue
        call GSSETC(OHITE,OANGLE)
C
C  WRONG LEVEL
C
      else
        call ERRMES('BTEXT',2,3)
      endif
      return
      end
      subroutine BTEXTL(IP,NLINES,XPOS,YPOS)
C
C     WRITE PACKED character ARRAY, LEFT JUSTIFIED
C              (LEVEL 2,3)
C
C     INPUT:   IP     = PACKED ARRAY OF characterS
C              NLINES = NUMBER OF LINES IN PACKED ARRAY
C              XPOS   = X VALUE FROM PHYSICAL ORIGIN IN INCHES
C              YPOS   = Y VALUE FROM PHYSICAL ORIGIN IN INCHES
C
      parameter (KZYES=111)
c
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
      DIMENSION IP(*)
      character*5 CFLAG(6)*1,CFONT(6),CSTYLE
c
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1

      EQUIVALENCE (IVAL,RVAL)
C
      SAVE JTEXT,JHITE,JYRAT
C
      data JTEXT,JHITE,JYRAT /1,-2,-1/
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
C
C  SAVE OLD TEXT parameterS
C
        OHITE=ZZHITE
        OANGLE=ZZANGL
C
C  CALCULATE VIRTUAL COORDINATES AND
C  LENGTH OF A LINE OF characterS
C
        VX=ZZXOR+XCM0+XPOS*ZZUNIT*ZZPAGR
        VY=ZZYOR+YCM0+YPOS*ZZUNIT*ZZPAGR
        if(IP(1).EQ.KZYES)then
          IPLEN=IP(2)/1000
          LLEN=IP(2)-IPLEN*1000
        else
          LLEN=40
        endif
        NWORD=(LLEN-1)/KZBYTE+5
        YLEN=YBTEXT(IP,NLINES)*ZZUNIT*ZZPAGR
        ICUR=2+NWORD
        IVAL=IP(ICUR)
        AVE=RVAL
        IVAL=IP(ICUR+JYRAT)
        YRAT=RVAL
        if(YRAT.LT.1.0) YRAT=1.0
        VY=VY+YLEN+(YRAT-1.0)*AVE/2.0
C
C                        FOR EACH LINE IN ARRAY:
C                        RETRIEVE LINE SPACE parameter, character
C                        HEIGHT, AND character FONT, SET TO CURRENT
C                        AND DRAW TO OUTPUT DEVICE
C
        ICUR=2
        DO 100 I=1,NLINES
          ICUR=ICUR+NWORD
          IVAL=IP(ICUR+JYRAT)
          YRAT=RVAL
          if(YRAT.LT.1.0) YRAT=1.0
          IVAL=IP(ICUR+JHITE)
          HITE=RVAL
          DELTA=(YRAT-1.0)*AVE/2.0
          VY=VY-DELTA-HITE
          DO 800 K=1,6
            CFLAG(K)=CZLGAC(I,K)
            CFONT(K)=CZLGAF(I,K)
 800      continue
          CSTYLE=CZLGAS(I)
          call GSSETC(HITE,0.0)
          call DSMOVE(VX,VY)
          call ZTEXT(IP(ICUR-NWORD+JTEXT),100,
     *                 CFLAG,CFONT,CSTYLE)
          VY=VY-DELTA
 100    continue
        call GSSETC(OHITE,OANGLE)
C
C                        WRONG LEVEL
C
      else
        call ERRMES('BTEXTL',2,3)
      endif
      return
      end
      subroutine BTEXTR(IP,NLINES,XPOS,YPOS)
C
C     Author: Eric A. Hibbard, NASA Ames Research Center, CA
C
C
C     WRITE PACKED character ARRAY, RIGHT JUSTIFIED
C              (LEVEL 2,3)
C
C     INPUT:   IP     = PACKED ARRAY OF characterS
C              NLINES = NUMBER OF LINES IN PACKED ARRAY
C              XPOS   = X VALUE FROM PHYSICAL ORIGIN IN INCHES
C              YPOS   = Y VALUE FROM PHYSICAL ORIGIN IN INCHES
C
      parameter (KZYES=111)
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
C
      DIMENSION IP(*)
      character*5 CFLAG(6)*1,CFONT(6),CSTYLE
      EQUIVALENCE (IVAL,RVAL)
C
      SAVE JTEXT,JHITE,JYRAT
C
      data JTEXT,JHITE,JYRAT /1,-2,-1/
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
C
C                        SAVE OLD TEXT parameterS
C
        OHITE=ZZHITE
        OANGLE=ZZANGL
C
C                        CALCULATE VIRTUAL COORDINATES AND
C                        LENGTH OF A LINE OF characterS
C
        VX=ZZXOR+XCM0+XPOS*ZZUNIT*ZZPAGR
        VY=ZZYOR+YCM0+YPOS*ZZUNIT*ZZPAGR
        if(IP(1).EQ.KZYES)then
          IPLEN=IP(2)/1000
          LLEN=IP(2)-IPLEN*1000
        else
          LLEN=40
        endif
        NWORD=(LLEN-1)/KZBYTE+5
        XLEN=XBTEXT(IP,NLINES)*ZZUNIT*ZZPAGR
        YLEN=YBTEXT(IP,NLINES)*ZZUNIT*ZZPAGR
        ICUR=2+NWORD
        IVAL=IP(ICUR)
        AVE=RVAL
        IVAL=IP(ICUR+JYRAT)
        YRAT=RVAL
        if(YRAT.LT.1.0) YRAT=1.0
        VX=VX-XLEN
        VY=VY+YLEN+(YRAT-1.0)*AVE/2.0
C
C                        FOR EACH LINE IN ARRAY:
C                        RETRIEVE LINE SPACE parameter, character
C                        HEIGHT, AND character FONT, SET TO CURRENT
C                        AND DRAW TO OUTPUT DEVICE
C
        ICUR=2
        DO 100 I=1,NLINES
          ICUR=ICUR+NWORD
          IVAL=IP(ICUR+JYRAT)
          YRAT=RVAL
          if(YRAT.LT.1.0) YRAT=1.0
          IVAL=IP(ICUR+JHITE)
          HITE=RVAL
          DELTA=(YRAT-1.0)*AVE/2.0
          VY=VY-DELTA-HITE
          DO 800 K=1,6
            CFLAG(K)=CZLGAC(I,K)
            CFONT(K)=CZLGAF(I,K)
 800      continue
          CSTYLE=CZLGAS(I)
          call GSSETC(HITE,0.0)
          call DSMOVE(VX,VY)
          call ZTEXT(IP(ICUR-NWORD+JTEXT),100,
     *                 CFLAG,CFONT,CSTYLE)
          VY=VY-DELTA
 100    continue
        call GSSETC(OHITE,OANGLE)
C
C  WRONG LEVEL
C
      else
        call ERRMES('BTEXTR',2,3)
      endif
      return
      end
      subroutine CAMROT
c
c***********************************************************************
c
C     MAKE UP CAMERA ROTATION MATRIX
C
C     ROTATION IS DONE SO THAT Z PRIME AXIS IS DIRECTED FROM THE
C     CAMERA TO THE AIMING POINT.   NOTE ALSO THAT THE PRIMED
C     COORDINATE SYSTEM IS LEFT-HANDED IF EPSLON=-1.
c
C     THIS IS SO THAT THE PICTURE COMES OUT RIGHT WHEN PROJECTED
C     ON THE PRIMED COORDINATE SYSTEM.
C
      common/COMDP/XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,AXISR(2),PLOTX,
     1   PLOTY,PLTORG(2),CAMXYZ(3),MX,NY,FMX,FNY,CAMWKG(6),XORG(3),
     2   GX(3),FX(2),KSCALE,ZORG,CENTER(2),PQLMT,
     3   AMTX(3,3),FOCALL
      DIMENSION LIMIT(2),FLIM(2)
      EQUIVALENCE(U,CAMXYZ(1)),(V,CAMXYZ(2)),(W,CAMXYZ(3)),
     1   (MX,LIMIT(1)),(FMX,FLIM(1))
C
      DIMENSION AU(3),AV(3),AW(3)
c
C  HANDEDNESS parameter, -1 FOR LEFT-HANDED USUALLY
C
      EPSLON=-1.0
C
      S = 0.0
      DO 1 J = 1,3
        AV(J) = 0.0
        AW(J) = 0.0
        AU(J) = CAMWKG(J+3)-CAMWKG(J)
        S = S + AU(J)**2
1     continue
      S = SQRT(S)
c
      DO 2 J = 1,3
        AU(J) = AU(J)/S
2     continue
      SIGMA = SQRT(AU(1)**2 + AU(2)**2)
c
C  PREPARE LOOKING STRAIGHT UP OR DOWN
c
      AV(1) = 1.0
      AW(2) = -EPSLON
      IF(AU(3) .GT. 0.0) AW(2) = -AW(2)
      IF(SIGMA .LT. 1.0E-3) go to 4
C
C  X AXIS
c
      AV(1) = AU(2)/SIGMA
      AV(2) = -AU(1)/SIGMA
      AV(3) = 0.0
C
C  Y AXIS
C
      AW(1) = EPSLON*AU(1)*AU(3)/SIGMA
      AW(2) = EPSLON*AU(2)*AU(3)/SIGMA
      AW(3) = -EPSLON*SIGMA
C
C  TRANSFER AXIS DIRECTION COSINES TO ROTATION MATRIX ROWS
C
4     DO 3 J = 1,3
        AMTX(1,J) = AV(J)
        AMTX(2,J) = AW(J)
        AMTX(3,J) = AU(J)
3     continue
      return
      end
      subroutine CAPCHR(STRING)
C
C***********************************************************************
C
C  CAPCHR accepts a STRING of characters and replaces any lowercase 
C  letters by uppercase ones.   
C
C  STRING Input/output, character*(*) STRING, the string of characters
C         to be transformed.
C
      integer   I
      integer   ITEMP
      integer   NCHAR
      character STRING*(*)
C
      INTRINSIC CHAR
      INTRINSIC ICHAR
      INTRINSIC LEN
      INTRINSIC LGE
      INTRINSIC LLE
C
      NCHAR=LEN(STRING)
      DO 10 I=1,NCHAR
C
        IF(LGE(STRING(I:I),'a').AND.LLE(STRING(I:I),'z'))THEN
          ITEMP=ICHAR(STRING(I:I))+ICHAR('A')-ICHAR('a')
          STRING(I:I)=CHAR(ITEMP)
          endif
10      continue
      return
      end
      subroutine CARTOG
C
C
C
C      SET character TYPE TO TRIPLEX
C               LEVEL 1-3, P/S
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFS='CARTO'
      else
        call ERRMES('CARTOG',1,3)
      endif
      return
      end
      subroutine CHNDOT
C
C      SET LINE STYLE TO CHAIN-DOT
C               LEVEL 1-3, P/S
C
      parameter (KZCDOT=4)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZLSTY=KZCDOT
        call DSLTYP(KZLSTY)
      else
        call ERRMES('CHNDOT',1,3)
      endif
      return
      end
      subroutine CHNDSH
C
C      SET LINE STYLE TO DASH
C               LEVEL 1-3, P/S
C
      parameter (KZCDSH=5)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZLSTY=KZCDSH
        call DSLTYP(KZLSTY)
      else
        call ERRMES('CHNDSH',1,3)
      endif
      return
      end
      subroutine CLLINE(X1,Y1,X2,Y2)
C
C     THIS ROUTINE DRAWS THE LINE FROM X1,Y1 TO X2,Y2 WITH
C     THE APPROPIATE CLIPPING
C
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
      DIMENSION AREA(4)
C
      call GSSCLP(XVSTRT,XVSTRT+XVLEN,YVSTRT,YVSTRT+YVLEN,AREA)
      call SCALE(X1,Y1,VX,VY)
      call GSMOVE(VX,VY)
      call SCALE(X2,Y2,VX,VY)
      call GSDRAW(VX,VY)
      call GSRCLP(AREA)
      return
      end
      subroutine CMPLX2
C
C
C
C      SET character TYPE TO TRIPLEX
C               LEVEL 1-3, P/S
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFS='CMPLX'
      else
        call ERRMES('CMPLX2',1,3)
      endif
      return
      end
      subroutine COMPLX
C
C
C
C      SET character TYPE TO TRIPLEX
C               LEVEL 1-3, P/S
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFS='COMPL'
      else
        call ERRMES('COMPLX',1,3)
      endif
      return
      end
      subroutine CONTOR(Z,NZ,IZ,MX,MY,X1,XMX,Y1,YMY,NL,CL)
c
c***********************************************************************
c
c  CONTOR will produce a contour plot of a function defined
c  by a table of values:
c
c    Z(I,J) = F(X(I),Y(J)).
c
c  Here it is assumed that the X and Y values are equally spaced.
c  Hence the user does not supply the X and Y arrays, but rather
c  simply the first and last X and Y values, and the number of
c  rows and columns of data.
c
c  Before calling CONTOR, you must call MAPIT to establish the
c  coordinate axes, and have the X axis at least large enough to
c  cover the range of the X data, and the Y axis cover the range
c  of the Y data.
c
c  Z      Input, REAL Z(NZ,MY).
c         The values of the function to contour:
c
c         Z(I,J) = F(XI,YJ) where:
c
c         XI = X1 + (I-1)*(XMX-X1)/(MX-1)
c         YJ = Y1 + (J-1)*(YMX-Y1)/(MY-1)
c
c  NZ     Input, integer NZ, integer constant or variable.
c         The first dimension of the array Z - not necessarily
c         equal to MX, but MX <= NZ.
c
c  IZ     Input, integer IZ(MX,MY).
c         Used internally for working storage.
c
c  MX     Input, integer MX.
c         The number of X grid points.
c
c  MY     Input, integer MY.
c         The number of Y grid points.
c
c  X1     Input, REAL X1.
c         The minimum X value.
c
c  XMX    Input, REAL XMX.
c         The maximum X value.
c
c  Y1     Input, REAL Y1.
c         The minimum Y value.
c
c  YMY    Input, REAL YMY.
c         The maximum Y value.
c
c  NL     Input, integer NL.
c         The number of contour levels.
c
c  CL     Input, REAL CL(NL).
c         The contour levels to draw.
c
      integer mx
      integer my
      integer nl
c
      REAL Z(NZ,MY)
      REAL CL(NL)
      integer IZ(MX,MY)
c
      save contr
c
      common /CONTR/ CLEVEL,IOLD,JOLD,IN,JN,
     1   NX,NY,XL,DX,YL,DY
C
C  INITIALIZE ROUTINE
C
      XL = X1
      YL = Y1
      DX = XMX-X1
      DY = YMY-Y1
      NX=MX
      NY=MY
      NLOOP=MIN1(FLOAT(NX)/2.0+.5,FLOAT(NY)/2.0+.5)
c
C  START SEARCHING FOR PLUS-MINUS TRANSITIONS
C  TO START A CONTOR ON.
c
      DO 60 NC=1,NL
C
C  ZERO ARRAY SHOWING WHERE WE HAVE BEEN
C
        DO 2 J=1,NY
          DO 1 I=1,NX
            IZ(I,J)=0
1         continue
2       continue
c
        CLEVEL=CL(NC)
c
        DO 50 ICIR=1,NLOOP
c
          IU=NX+1-ICIR
          JU=NY+1-ICIR
          DO 10 J=ICIR,JU-1
            call LOOK(Z,ICIR,J,1,IZ,NZ,NX)
10        continue
          DO 20 I=ICIR,IU-1
            call LOOK(Z,I,JU,2,IZ,NZ,NX)
20        continue
          DO 30 J=JU,ICIR+1,-1
            call LOOK(Z,IU,J,3,IZ,NZ,NX)
30        continue
          DO 40 I=IU,ICIR+1,-1
            call LOOK(Z,I,ICIR,4,IZ,NZ,NX)
40        continue
c
50      continue
c
60    continue
c
      return
      end
      subroutine CRVWID(THICK)
C
C
C
C      SET LINE THICKNESS IN INCHES
C               LEVEL 1-3, P/S
C
C      INPUT:   THICK = LINE THICKNESS IN INCHES
C
      parameter (KZYES=111)
c
      save clevel
      save cline
      save cpage
      save cunit
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
C
      if(KZLEVL.EQ.1)then
        UULTHK=THICK*ZZUNIT
        ZZLTHK=UULTHK
        KZLTHK=KZYES
      elseif(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
        UULTHK=THICK*ZZUNIT
        ZZLTHK=UULTHK*ZZPAGR
        KZLTHK=KZYES
      else
        call ERRMES('CRVWID',1,3)
      endif
      return
      end
      FUNCTION CSZMAP()
c
      save pltprm
c
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI
C
C     return character SIZE MAP USED/WILL USE
C
      CSZMAP = CYSIZE
      return
      end
      subroutine CUBSPL
C
C
C
C      SET INTERPOLATION FLAG
C               LEVEL 1-3, P/S
C
      parameter (KZCSPL=3)
c
      save clevel
      save cline
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZLTYP=KZCSPL
      else
        call ERRMES('CUBSPL',1,3)
      endif
      return
      end
      subroutine CURSOR(X,Y,KEY)
C
C     DISPLAY AND READ THE GRAPHICS CURSOR AND return ITS POSITION
C     IN USER COORDINATES.
C
      character KEY
      LOGICAL LOGX, LOGY
c
      save pltcom
      save pltsiz
c
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
C  GET CURSOR POSITION IN VIRTUAL COORDINATES.
C
      call GSGIN(X,Y,KEY,IERR)
      if(IERR .GE. 0) go to 50
      X = XVSTRT
      Y = YVSTRT
50    X = (X-XVSTRT)*UDX/XVLEN + UX0
      if(LOGX) X = 10.0**X
      Y = (Y-YVSTRT)*UDY/YVLEN + UY0
      if(LOGY) Y = 10.0**Y
      return
      end
      subroutine CURVE(X,Y,NPTS,NPBSYM)
c
c***********************************************************************
c
C  DRAW CURVE ACCORDING TO INTERPOLATION FLAG
C           (LEVEL 3)
C
C  INPUT:   X,Y    = INPUT ARRAY OF X AND Y VALUES
C              NPTS   = NUMBER OF INPUT X Y PAIRS
C              NPBSYM = SYMBOL FLAG
C                        IF NPBSYM > 0, CONNECT POINTS AND
C                         DRAW SYMBOLS
C                        IF NPBSYM = 0, CONNECT POINTS ONLY
C                        IF NPBSYM < 0, DRAW SYMBOL ONLY
C
      parameter (KZYES=111)
      parameter (KZMXBS=1000)
      parameter (KZMXBL=200)
      parameter (KZLIN=2)
      parameter (KZCSPL=3)
      parameter (KZPSPL=4)
c
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5
      character CZLGTS*5
      LOGICAL LINILT, LPOSND
      DIMENSION X(*)
      real Y(*)
c
      save cblank
      save clevel
      save clgndc
      save clgndn
      save cline
      save csymbo
      save dcltyp
      save gcdchr
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      common /CBLANK/ ZZBLNK(KZMXBL,4),ZZBLKS(KZMXBS,4),KZBLCN,KZBSCN
      common /CSYMBO/ KZSYM,KZNSYM,ZZSMSZ,UUSMSZ
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
C
C  UPDATE LEGEND LINE COUNTER
C
      if(KZLEVL.EQ.3)then
        KZLGCN=KZLGCN+1
        if(KZLGCN.GT.50)then
          KZLGER=KZYES
          KZLGCN=50
        endif
C
C  DRAW SYMBOL IF DESIRED, SAVE OLD
C  LINE STYLE AND DRAW SYMBOL IN SOLID
C
        if(NPBSYM.NE.0)then
          KZLGSM(KZLGCN)=KZSYM
          if(NPBSYM.GE.0)then
            NSYM=NPBSYM
          else
            NSYM=-NPBSYM
          endif
          IOLDLT=ILNTYP
          ILNTYP=1
          HALF=ZZSMSZ*0.55
          IMARK=1
          DO 300 I=1,NPTS,NSYM
            IMARK=I
            call SCALE(X(I),Y(I),VX,VY)
            call DSMOVE(VX,VY)
            call DSYMBO(KZSYM,ZZSMSZ)
C
C  ACTIVATE BLANK AREA, IF DESIRED
C
            if(KZBSCN.GE.0)then
              KZBSCN=KZBSCN+1
              if(KZBSCN.GT.KZMXBS)then
                call ERRMES('CURVE',0,5)
                KZBSCN=KZMXBS
              else
                ZZBLKS(KZBSCN,1)=VX+HALF
                ZZBLKS(KZBSCN,2)=VX-HALF
                ZZBLKS(KZBSCN,3)=VY+HALF
                ZZBLKS(KZBSCN,4)=VY-HALF
              endif
            endif
 300      continue
C
C  IF NO MARKER DRAWN AT END OF LINE, DRAW ONE
C
          if(IMARK.NE.NPTS)then
            call SCALE(X(NPTS),Y(NPTS),VX,VY)
            call DSMOVE(VX,VY)
            call DSYMBO(KZSYM,ZZSMSZ)
            if(KZBSCN.GE.0)then
              KZBSCN=KZBSCN+1
              if(KZBSCN.GT.KZMXBS)then
                call ERRMES('CURVE',0,5)
                KZBSCN=KZMXBS
              else
                ZZBLKS(KZBSCN,1)=VX+HALF
                ZZBLKS(KZBSCN,2)=VX-HALF
                ZZBLKS(KZBSCN,3)=VY+HALF
                ZZBLKS(KZBSCN,4)=VY-HALF
              endif
            endif
          endif
C
C  RESTORE LINE STYLE
C
          ILNTYP=IOLDLT
        endif
        KZSYM=KZSYM+1
        if(KZSYM.GT.KZNSYM) KZSYM=0
        if(NPBSYM.GE.0)then
          if(KZLGLN.EQ.KZYES)then
            KZLGLT(KZLGCN)=ILNTYP
            if(KZLTHK.EQ.KZYES)then
              ZZLGTH(KZLGCN)=ZZLTHK
              KZLGLT(KZLGCN)=KZLGLT(KZLGCN)+10
            endif
          endif
C
C  DRAW CONNECTED LINE WITH NO CURVE SMOOTHING
C
          if(KZLTYP.EQ.KZLIN.OR.NPTS.LE.3)then
            if(NPTS.LE.3.AND.KZLTYP.NE.KZLIN)then
              call ERRMES('CURVE',0,4)
            endif
            call ZCURVE(X,Y,NPTS)
C
C  DRAW CUBIC SPLINE CURVE
C
          elseif(KZLTYP.EQ.KZCSPL.AND.NPTS.LE.102)then
            call ZCURCS(X,Y,NPTS)
C
C  DRAW PARAMETRIC CUBIC SPLINE CURVE
C
          elseif(KZLTYP.EQ.KZPSPL.AND.NPTS.LE.51)then
            call ZCURPS(X,Y,NPTS)
C
C  DRAW PARAMETRIC POLYNOMIAL CURVE
C
          else
            call ZCURPP('W',X,Y,NPTS)
          endif
        endif
c
c  The package is not at the appropriate level
c
      else
        write(*,*)'CURVE - Fatal error.'
        write(*,*)'The package is not at the correct level.'
        write(*,*)'The current level is ',KZLEVL
        write(*,*)'The package must be at level 3 to use CURVE.'
      endif
C
      return
      end
      subroutine CURVES(X,Y,NPTS,ISYMNO,SYMSIZ,NPBSYM)
      DIMENSION X(NPTS), Y(NPTS)
C
C  CURVES TRACES THE LINE FROM X(1),Y(1) TO
C  X(NPTS),Y(NPTS) WITH APPROPIATE CLIPPING.
C  IT THEN ADDS THE DESIRED SYMBOL (ISYMNO) TO THE PLOT SPACED
C  "NPBSYM" POINTS APART.
C
      DIMENSION AREA(4)
C
      LOGICAL LINILT, LPOSND
      common /GCLTYP/ ILNTYP, DLEFT, DIST(4,3), LINILT, LPOSND
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
      call GSSCLP(XVSTRT,XVSTRT+XVLEN,YVSTRT,YVSTRT+YVLEN,AREA)
      call SCALE(X(1),Y(1),VX,VY)
      call GSMOVE(VX,VY)
      if(NPTS .LE. 1) go to 110
10    DO 100 I=2,NPTS
        call SCALE(X(I),Y(I),VX,VY)
        call GSDRAW(VX,VY)
100   continue
C
C  NOW ADD SYMBOLS IF DESIRED
C
110   continue
      if(ISYMNO .LE. 0) go to 800
C
C  SAVE LINE TYPE, AND DO SYMBOLS IN SOLID LINES
C
      IOLDLT = ILNTYP
      ILNTYP = 1
      DO 200 I=1, NPTS, NPBSYM
        call SCALE(X(I),Y(I),VX,VY)
        call GSMOVE(VX,VY)
        call SYMBOL(ISYMNO,SYMSIZ)
200   continue
C
C  RESTORE LINE TYPE
C
      ILNTYP = IOLDLT
800   continue
      call GSRCLP(AREA)
      return
      end
      subroutine CURVEY(XMIN,XMAX,Y,NPTS,ISYMNO,SYMSIZ,NPBSYM)
c
c***********************************************************************
c
c  CURVEY draws the curve that passes through a set of points
c  (X(I), Y(I)).  Here it is assumed that the X values are
c  equally spaced.  Hence, no X array is needed, just the range
c  of X.
c
c  CURVEY will automatically clip portions of the curve that
c  go outside the limits of the current graph.
c
c  CURVEY is callable from level 3.
c
c  XMIN,
c  XMAX   Input, REAL XMIN, XMAX, the minimum and maximum values
c         of the X variable.
c
c  Y      Input, REAL Y(NPTS), the Y values of the curve at each
c         of the equally spaced X values.
c
c  SYMSIZ Input, REAL SYMSIZ, the size of the marker to be used
c         to mark the data points.
c
c  NPBSYM Input, integer NPBSYM, the interval between data points
c         to be marked.  Point 1 will be marked, and point
c         1+NPBSYM, and so on.
c
c         Set NPBSYM=1 to mark all points.
c
c         Set NPBSYM=0 for no marking.
c
      integer npts
c
      real area(4)
      real y(npts)
C
      LOGICAL LINILT, LPOSND
      common /GCLTYP/ ILNTYP, DLEFT, DIST(4,3), LINILT, LPOSND
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
      call GSSCLP(XVSTRT,XVSTRT+XVLEN,YVSTRT,YVSTRT+YVLEN,AREA)
      call SCALE(XMIN,Y(1),VX,VY)
      call GSMOVE(VX,VY)
      DX = (XMAX-XMIN)/(NPTS-1)
      XNEW = XMIN
      DO 100 I=2,NPTS
        XNEW = XMIN + (I-1)*DX
        call SCALE(XNEW,Y(I),VX,VY)
        call GSDRAW(VX,VY)
100   continue
C
C  NOW ADD SYMBOLS IF DESIRED
C
      IF(ISYMNO .GT. 0.and.npbsym.gt.0)then
        IOLDLT = ILNTYP
        ILNTYP = 1
        DO 200 I=1,NPTS,NPBSYM
          XNEW = XMIN + (I-1)*DX
          call SCALE(XNEW,Y(I),VX,VY)
          call GSMOVE(VX,VY)
          call SYMBOL(ISYMNO,SYMSIZ)
200     continue
        ILNTYP = IOLDLT
      endif
c
      call GSRCLP(AREA)
      return
      end
      subroutine DASH
C
C
C
C      SET LINE STYLE TO DASH
C               LEVEL 1-3, P/S
C
      parameter (KZDASH=3)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZLSTY=KZDASH
        call DSLTYP(KZLSTY)
      else
        call ERRMES('DASH',1,3)
      endif
      return
      end
      subroutine DDEVSL(NEWDEV,LUN,IERR)
C
      DIMENSION DEVCHR(8)
      DIMENSION DFDIST(13,15)
      LOGICAL LCURNT
      LOGICAL LINILT, LPOSND
c
      save cline
      save dcltyp
      save gcclip
      save gcdchr
      save gcdpar
      save gcdpos
      save gcdprm
      save gcdsel
      save gcvpos
c
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
      common /GCDSEL/ IDEV
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
      common /GCCPAR/ CSIZE, CCOS, CSIN
      common /GCCPOS/ XAPOS, YAPOS, IVIS, LCURNT
      common /GCVPOS/ XVPOS, YVPOS
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
C
C      DEFINE DEFAULT LINE STYLES
C
      SAVE DFDIST
C
      data DFDIST /
     1   0.08, 0.08, 0.08, 0.08,8*0.0,4.0,
     2   0.20, 0.15, 0.20, 0.15,8*0.0,4.0,
     3   0.75, 0.10, 0.05, 0.10,8*0.0,4.0,
     4   1.00, 0.25, 0.25, 0.25,8*0.0,4.0,143*0.0/
C
C      RELEASE CURRENT DEVICE
C
      if(IDEV .NE. 0) CALL GSDRVR(6,DUMMY,DUMMY)
C
C      NOW INIT. THE NEW DEVICE
C
      if(NEWDEV .LE. 0) go to 900
      IDEV = NEWDEV
C
C      GET THE DEVICE characterISTICS (AND SEE IF DEVICE THERE)
C
      DEVCHR(8) = 1.0
      call GSDRVR(7,DEVCHR,DUMMY)
      if(DEVCHR(1) .EQ. 0.0) go to 900
C
C      INITIALIZE THE DEVICE FOR GRAPHICS
C
      call GSDRVR(1,FLOAT(LUN),DUMMY)
      IERR = INT(DUMMY)
      if(IERR .NE. 0) go to 910
C
C      SET DEVICE characterISTICS FOR LATER USE
C
      DEVID  = DEVCHR(1)
      XLENCM = DEVCHR(2)
      YLENCM = DEVCHR(3)
      XRES   = DEVCHR(4)
      YRES   = DEVCHR(5)
      NDCLRS = INT(DEVCHR(6))
      IDVBTS = INT(DEVCHR(7))
      NFLINE = INT(DEVCHR(8))
      XCLIPD = XLENCM + 0.499/DEVCHR(4)
      YCLIPD = YLENCM + 0.499/DEVCHR(5)
C
C      NOW INIT THE parameterS
C
      XS = 1.0
      YS = 1.0
      XT = 0.0
      YT = 0.0
      RCOS = 1.0
      RSIN = 0.0
      VXL = 0.0
      VXH = XLENCM
      VYL = 0.0
      VYH = YLENCM
      CSIZE = 0.3
      CCOS = 1.0
      CSIN = 0.0
      IVIS = 0
      XCM0 = 0.0
      YCM0 = 0.0
      XCM1 = XCLIPD
      YCM1 = YCLIPD
      ILNTYP = 1
      DO 120 I=1,4
        DO 110 J=1,13
          DIST(J,I) = DFDIST(J,I)
110     continue
120   continue
      LCURNT = .FALSE.
      return
C
C      NON-EXISTANT DEVICE SELECTED, REPORT ERROR AND DESELECT DEVICE
C
 900  IERR = -1
C
C      DEVICE INITIALIZATION FAILED, DESELCT DEVICE
C
 910  IDEV = 0
      return
      end
      subroutine DEFALF(LALPHA)
C
C
C
C     SET BASE ALPHABET SET
C              (LEVEL 1-3, P/S)
C
C     INPUT:   LALPHA = character SET
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      character*(*) LALPHA
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFL(1)=')'
        CZALFN(1)=LALPHA(1:5)
        call CAPCHR(CZALFN(1))
      else
        call ERRMES('DEFALF',1,3)
      endif
      return
      end
      subroutine DEVSEL(NEWDEV,LUN,IERR)
C
      common /GCDSEL/ IDEV
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
      common /GCCPAR/ CSIZE, CCOS, CSIN
      common /GCVPOS/ XVPOS, YVPOS
      LOGICAL LCURNT
      common /GCCPOS/ XAPOS, YAPOS, IVIS, LCURNT
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      LOGICAL LINILT, LPOSND
      common /GCLTYP/ ILNTYP, DLEFT, DIST(4,3), LINILT, LPOSND
      DIMENSION DEVCHR(8), GDCOMN(5)
      DIMENSION DFDIST(4,3)
C
C     DEFINE DEFAULT LINE STYLES
C
      EQUIVALENCE (DEVID,GDCOMN(1))
C
      SAVE DFDIST
C
      data DFDIST /
     1    0.5,  0.5,  0.5,  0.5,
     2   0.25, 0.25, 0.25, 0.25,
     3    0.5, 0.25, 0.25, 0.25/
C
C     RELEASE CURRENT DEVICE
C
      if(IDEV .NE. 0) CALL GSDRVR(6,DUMMY,DUMMY)
C
C     NOW INIT. THE NEW DEVICE
C
      if(NEWDEV .LE. 0) go to 900
      IDEV = NEWDEV
C
C     GET THE DEVICE characterISTICS (AND SEE IF DEVICE THERE)
C
      DEVCHR(8) = 1.0
      call GSDRVR(7,DEVCHR,DUMMY)
C
      if(DEVCHR(1) .EQ. 0.0) go to 900
C     INITIALIZE THE DEVICE FOR GRAPHICS
C
      call GSDRVR(1,FLOAT(LUN),DUMMY)
      IERR = INT(DUMMY)
      if(IERR .NE. 0) go to 910
C
C     SET DEVICE characterISTICS FOR LATER USE
C
      DO 100 I=1,5
  100 GDCOMN(I) = DEVCHR(I)
      NDCLRS = INT(DEVCHR(6))
      IDVBTS = INT(DEVCHR(7))
      NFLINE = INT(DEVCHR(8))
      XCLIPD = XLENCM + 0.499/DEVCHR(4)
      YCLIPD = YLENCM + 0.499/DEVCHR(5)
C
C     NOW INIT THE parameterS
C
      XS = 1.0
      YS = 1.0
      XT = 0.0
      YT = 0.0
      RCOS = 1.0
      RSIN = 0.0
      VXL = 0.0
      VXH = XLENCM
      VYL = 0.0
      VYH = YLENCM
      CSIZE = GOODCS(0.3)
      CCOS = 1.0
      CSIN = 0.0
      IVIS = 0
      XCM0 = 0.0
      YCM0 = 0.0
      XCM1 = XCLIPD
      YCM1 = YCLIPD
      ILNTYP = 1
       DO 120 I=1,3
        DO 110 J=1,4
        DIST(J,I) = DFDIST(J,I)
  110  continue
  120 continue
      LCURNT = .FALSE.
      return
C
C     NON-EXISTANT DEVICE SELECTED, REPORT ERROR AND DESELECT DEVICE
C
  900 IERR = -1
C
C     DEVICE INITIALIZATION FAILED, DESELCT DEVICE
C
  910 IDEV = 0
      return
      end
      subroutine DHATCH(XVERT, YVERT, NUMPTS, PHI, CMSPAC, IFLAGS,
     &  XX,YY)

C      ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C      H A T C H
C      by Kelly Booth and modified by Hal Brand
C
C      PROVIDE SHADING FOR A GENERAL POLYGONAL REGION.  THERE IS ABSOLUTELY NO
C      ASSUMPTION MADE ABOUT CONVEXITY.  A POLYGON IS SPECIFIED BY ITS VERTICES,
C      GIVEN IN EITHER A CLOCKWISE OR COUNTER-CLOCKWISE ORDER.  THE DENSITY OF
C      THE SHADING LINES (OR POINTS) AND THE ANGLE FOR THE SHADING LINES ARE
C      BOTH DETERMINED BY THE parameterS PASSED TO THE ROUTINE.
C
C      THE INPUT parameterS ARE INTERPRETED AS FOLLOWS:
C
C       XVERT  -  AN ARRAY OF X COORDINATES FOR THE POLYGON(S) VERTICES
C
C       YVERT  -  AN ARRAY OF Y COORDINATES FOR THE POLYGON(S) VERTICES
C
C            NOTE: AN X VALUE >=1E38 SIGNALS A NEW POLYGON.   THIS ALLOWS
C                  FILLING AREAS THAT HAVE HOLES WHERE THE HOLES ARE
C                  DEFINED AS POLYGONS.   IT ALSO ALLOWS MULTIPLE
C                  POLYGONS TO BE FILLED IN ONE CALL TO HATCH.
C
C       NUMPTS -  THE NUMBER OF VERTICES IN THE POLYGON(S) INCLUDING
C                  THE SEPERATOR(S) IF ANY.
C
C       PHI    -  THE ANGLE FOR THE SHADING, MEASURED COUNTER-CLOCKWISE
C                  IN DEGREES FROM THE POSITIVE X-AXIS
C
C       CMSPAC -  THE DISTANCE IN VIRTUAL COORDINATES (CM. USUALLY)
C                  BETWEEN SHADING LINES.   THIS VALUE MAY BE ROUNDED
C                  A BIT, SO SOME CUMMULATIVE ERROR MAY BE APPARENT.
C
C       IFLAGS -  GENERAL FLAGS CONTROLLING HATCH
C                  0 ==>  BOUNDARY NOT DRAWN, INPUT IS VIRTUAL COORD.
C                  1 ==>  BOUNDARY DRAWN, INPUT IS VIRTUAL COORD.
C                  2 ==>  BOUNDARY NOT DRAWN, INPUT IS WORLD COORD.
C                  3 ==>  BOUNDARY DRAWN, INPUT IS WORLD COORD.
C
C       XX     -  A WORK ARRAY AT LEAST "NUMPTS" LONG.
C
C       YY     -  A SECOND WORK ARRAY AT LEAST "NUMPTS" LONG.
C
      DIMENSION XVERT(NUMPTS)
      real YVERT(NUMPTS), XX(NUMPTS), YY(NUMPTS)
C
      save gcdchr
c
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1       NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
C
C      THIS ROUTINE HAS TO MAINTAIN AN INTERNAL ARRAY OF THE TRANSFORMED
C      COORDINATES.  THIS REQUIRES THE PASSING OF THE TWO WORKING ARRAYS
C      callED "XX" AND "YY".
C      THIS ROUTINE ALSO NEEDS TO STORE THE INTERSECTIONS OF THE HATCH
C      LINES WITH THE POLYGON.   THIS IS DONE IN "XINTCP".
C
      REAL XINTCP(20)
      LOGICAL LMOVE
C
      SAVE BIGNUM,FACT,PI180
C
C      X >= 'BIGNUM' SIGNALS THE END OF A POLYGON IN THE INPUT.
C
      data BIGNUM /1E38/
      data FACT /16.0/
      data PI180 /0.017453292/
C
C------------------------------------------------------------------------
C
C      CHECK FOR VALID NUMBER OF VERTICES.
C
      if(NUMPTS .LT. 3) return
C
C      CONVERT ALL OF THE POINTS TO integer COORDINATES SO THAT THE SHADING
C      LINES ARE HORIZONTAL.  THIS REQUIRES A ROTATION FOR THE GENERAL CASE.
C      THE TRANSFORMATION FROM VIRTUAL TO INTERNAL COORDINATES HAS THE TWO
C      OR THREE PHASES:
C
C      (1)  CONVERT WORLD TO VIRTUAL COORD. IF INPUT IN WORLD COORD.
C
C      (2)  ROTATE CLOCKWISE THROUGH THE ANGLE PHI SO SHADING IS HORIZONTAL,
C
C      (3)  SCALE TO integerS IN THE RANGE
C            [0...2*FACT*(DEVICE_MAXY_COORDINATE)], FORCING COORDINATES
C            TO BE ODD integerS.
C
C      THE COORDINATES ARE ALL ODD SO THAT LATER TESTS WILL NEVER HAVE AN
C      OUTCOME OF "EQUAL" SINCE ALL SHADING LINES HAVE EVEN COORDINATES.
C      THIS GREATLY SIMPLIFIES SOME OF THE LOGIC.
C
C      AT THE SAME TIME THE PRE-PROCESSING IS BEING DONE, THE INPUT IS CHECKED
C      FOR MULTIPLE POLYGONS.  IF THE X-COORDINATE OF A VERTEX IS >= 'BIGNUM'
C      THEN THE POINT IS NOT A VERTEX, BUT RATHER IT SIGNIFIES THE END OF A
C      PARTICULAR POLYGON.  AN IMPLIED EDGE EXISTS BETWEEN THE FIRST AND LAST
C      VERTICES IN EACH POLYGON.  A POLYGON MUST HAVE AT LEAST THREE VERTICES.
C      ILLEGAL POLYGONS ARE REMOVED FROM THE INTERNAL LISTS.
C
C      COMPUTE TRIGONOMETRIC FUNCTIONS FOR THE ANGLE OF ROTATION.
C
      COSPHI = COS(PI180*PHI)
      SINPHI = SIN(PI180*PHI)
C
C      FIRST CONVERT FROM WORLD TO VIRTUAL COORD. IF NECESSARY AND ELIMINATE
C      ANY POLYGONS WITH TWO OR FEWER VERTICES
C
      ITAIL = 1
      IHEAD = 0
      DO 120 I = 1, NUMPTS
C
C            ALLOCATE ANOTHER POINT IN THE VERTEX LIST.
C
            IHEAD = IHEAD + 1
C
C            A XVERT >= 'BIGNUM' IS A SPECIAL FLAG.
C
            if(XVERT(I) .LT. BIGNUM) go to 110
             XX(IHEAD) = BIGNUM
             if((IHEAD-ITAIL) .LT. 2) IHEAD = ITAIL - 1
             ITAIL = IHEAD + 1
             go to 120
110         continue
C
C            CONVERT FROM WORLD TO VIRTUAL COORD. IF INPUT IS WORLD COORD.
C
CUNI            if((IFLAGS .AND. 2) .EQ. 0) go to 115
                if(KSYAND(IFLAGS,2).EQ.0) go to 115
                  call SCALE(XVERT(I),YVERT(I),XX(IHEAD),YY(IHEAD))
                  go to 120
115             continue
                  XX(IHEAD) = XVERT(I)
                  YY(IHEAD) = YVERT(I)
120         continue
      if((IHEAD-ITAIL) .LT. 2) IHEAD = ITAIL - 1
      NVERT = IHEAD
C
C      DRAW BOUNDARY(S) IF DESIRED
C
CUNI      if((IFLAGS .AND. 1) .EQ. 0) go to 138
        if(KSYAND(IFLAGS,1) .EQ. 0) go to 138
      IHEAD = 0
      ITAIL = 1
      LMOVE = .TRUE.
130         continue
            IHEAD = IHEAD + 1
            if(IHEAD .GT. NVERT) go to 133
            if(XX(IHEAD) .NE. BIGNUM) go to 135
133          continue
             call DSDRAW(XX(ITAIL),YY(ITAIL))
             ITAIL = IHEAD + 1
             LMOVE = .TRUE.
             go to 139
135         continue
            if(LMOVE) go to 137
             call DSDRAW(XX(IHEAD),YY(IHEAD))
             go to 139
137         continue
            call DSMOVE(XX(IHEAD),YY(IHEAD))
            LMOVE = .FALSE.
139         continue
            if(IHEAD .LE. NVERT) go to 130
138   continue
C
C      ROTATE TO MAKE SHADING LINES HORIZONTAL
C
      YMIN = BIGNUM
      YMAX = -BIGNUM
      YSCALE = YRES*FACT
      YSCAL2 = 2.0*YSCALE
      DO 140 I = 1, NVERT
            if(XX(I) .EQ. BIGNUM) go to 140
C
C  PERFORM THE ROTATION TO ACHIEVE HORIZONTAL SHADING LINES.
C
            XV1 = XX(I)
            XX(I) = +COSPHI*XV1 + SINPHI*YY(I)
            YY(I) = -SINPHI*XV1 + COSPHI*YY(I)
C
C  CONVERT TO integerS AFTER SCALING, AND MAKE VERTICES ODD. IN Y
C
            YY(I) = 2.0*AINT(YSCALE*YY(I)+0.5)+1.0
            YMIN = AMIN1(YMIN,YY(I))
            YMAX = AMAX1(YMAX,YY(I))
140         continue
C
C      MAKE SHADING START ON A MULTIPLE OF THE STEP SIZE.
C
      STEP = 2.0*AINT(YRES*CMSPAC*FACT)
      YMIN = AINT(YMIN/STEP) * STEP
      YMAX = AINT(YMAX/STEP) * STEP
C
C      AFTER ALL OF THE COORDINATES FOR THE VERTICES HAVE BEEN PRE-PROCESSED
C      THE APPROPRIATE SHADING LINES ARE DRAWN.  THESE ARE INTERSECTED WITH
C      THE EDGES OF THE POLYGON AND THE VISIBLE PORTIONS ARE DRAWN.
C
      Y = YMIN
150         continue
            if(Y .GT. YMAX) go to 250
C
C  INITIALLY THERE ARE NO KNOWN INTERSECTIONS.
C
            ICOUNT = 0
            IBASE = 1
            IVERT = 1
160               continue
                  ITAIL = IVERT
                  IVERT = IVERT + 1
                  IHEAD = IVERT
                  if(IHEAD .GT. NVERT) go to 165
                  if(XX(IHEAD) .NE. BIGNUM) go to 170
C
C          THERE IS AN EDGE FROM VERTEX N TO VERTEX 1.
C
165                 IHEAD = IBASE
                    IBASE = IVERT + 1
                    IVERT = IVERT + 1
170               continue
C
C        SEE IF THE TWO ENDPOINTS LIE ON
C                  OPPOSITE SIDES OF THE SHADING LINE.
C
                  YHEAD =  Y - YY(IHEAD)
                  YTAIL =  Y - YY(ITAIL)
                  if(YHEAD*YTAIL .GE. 0.0) go to 180
C
C                  THEY DO.  THIS IS AN INTERSECTION.  COMPUTE X.
C
                  ICOUNT = ICOUNT + 1
                  DELX = XX(IHEAD) - XX(ITAIL)
                  DELY = YY(IHEAD) - YY(ITAIL)
                  XINTCP(ICOUNT) = (DELX/DELY) * YHEAD + XX(IHEAD)
180               continue
                  if( IVERT .LE. NVERT ) go to 160
C
C            SORT THE X INTERCEPT VALUES.  USE A BUBBLESORT BECAUSE THERE
C            AREN'T VERY MANY OF THEM (USUALLY ONLY TWO).
C
            if(ICOUNT .EQ. 0) go to 240
            DO 200 I = 2, ICOUNT
                  XKEY = XINTCP(I)
                  K = I - 1
                  DO 190 J = 1, K
                        if(XINTCP(J) .LE. XKEY)go to 190
                        XTEMP = XKEY
                        XKEY = XINTCP(J)
                        XINTCP(J) = XTEMP
190                     continue
                  XINTCP(I) = XKEY
200               continue
C
C  ALL OF THE X COORDINATES FOR THE SHADING SEGMENTS ALONG THE
C  CURRENT SHADING LINE ARE NOW KNOWN AND ARE IN SORTED ORDER.
C  ALL THAT REMAINS IS TO DRAW THEM.  PROCESS THE X COORDINATES
C  TWO AT A TIME.
C
            YR = Y/YSCAL2
            DO 230 I = 1, ICOUNT, 2
C
C                  CONVERT BACK TO VIRTUAL COORDINATES.
C                  ROTATE THROUGH AN ANGLE OF -PHI TO ORIGINAL ORIENTATION.
C                  THEN UNSCALE FROM GRID TO VIRTUAL COORD.
C
                  XV1 = + COSPHI*XINTCP(I) - SINPHI*YR
                  YV1 = + SINPHI*XINTCP(I) + COSPHI*YR
                  XV2 = + COSPHI*XINTCP(I+1) - SINPHI*YR
                  YV2 = + SINPHI*XINTCP(I+1) + COSPHI*YR
C
C                  DRAW THE SEGMENT OF THE SHADING LINE.
C
                  call DSMOVE(XV1,YV1)
                  call DSDRAW(XV2,YV2)
230               continue
240         continue
            Y = Y + STEP
            go to 150
250   continue
      return
      end
      subroutine DOT
C
C
C
C      SET LINE STYLE TO DOT
C               LEVEL 1-3, P/S
C
      parameter (KZDOT=2)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZLSTY=KZDOT
        call DSLTYP(KZLSTY)
      else
        call ERRMES('DOT',1,3)
      endif
      return
      end
      subroutine DRAWPQ(Z,IZDIM1)
C     DRAW VISIBLE PART OF SEGMENT PC-QC
C
      DIMENSION Z(IZDIM1,2)
C
C     COMMON STORAGE DESCRIPTOR
      common/COMDP/XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,AXISR(2),PLOTX,
     1   PLOTY,PLTORG(2),CAMXYZ(3),MX,NY,FMX,FNY,CAMWKG(6),XORG(3),
     2   GX(3),FX(2),KSCALE,ZORG,CENTER(2),PQLMT,
     3   AMTX(3,3),FOCALL
      DIMENSION LIMIT(2),FLIM(2)
      EQUIVALENCE(U,CAMXYZ(1)),(V,CAMXYZ(2)),(W,CAMXYZ(3)),
     1   (MX,LIMIT(1)),(FMX,FLIM(1))
C     END CDE
C
C
C     CDE PACKAGE FOR QDRW3D,DRAWPQ
      common/COMDPA/PC(3),QC(3),P(3),Q(3),ENDA(6),ENDB(6),OLDQ(3),
     1   PW(3),QW(3),T(6),PK(3),QK(3),PHIP,PHIQ,PHIA,IBEAM,ICOLOR
      integer PHIP,PHIQ,PHIA
C     END OF CDE PACKAGE
C
      KFLAG=0
      P(1) = PC(1)
      P(2) = PC(2)
      P(3) = PC(3)
      Q(1) = QC(1)
      Q(2) = QC(2)
      Q(3) = QC(3)
C     TEST IF P VISIBLE
2     IF(PHIP .EQ. 0) go to 30
C     YES, TEST Q
7     IF(PHIP*PHIQ)10,4,3
C     BOTH VISIBLE   SEGMENT DRAWABLE, PLOT   EXIT
3     Kgo to = 0
      go to 300
C     Q IS INVISIBLE, FIND LAST VISIBLE POINT ON SEGMENT PQ
4     Jgo to = 1
      go to 200
C     GIVE UP IF NOT FOUND IN MAXCUT1 BISECTIONS
5     IF(KFLAG .NE. 0) go to 6
C     NEXT POINT
      IBEAM = 0
      return
C     POINT FOUND
6     Q(1) = ENDA(1)
      Q(2) = ENDA(2)
      Q(3) = ENDA(3)
      go to 3
C
C     GAP IN SEGMENT, FIND LAST POINT TO CONNECT P.
10    Jgo to = 2
      go to 200
C     IF NOT FOUND (CANNOT FIND POINT WITH SAME VISIBILITY FN). TRY 2ND
11    IF(KFLAG .EQ. 0) go to 15
C     SAVE OLD Q, RESET POINT   PLOT THIS PIECE.
      OLDQ(1) = Q(1)
      OLDQ(2) = Q(2)
      OLDQ(3) = Q(3)
      Q(1) = ENDA(1)
      Q(2) = ENDA(2)
      Q(3) = ENDA(3)
C     DRAW FIRST PART OF SEGMENT AND COME BACK HERE
      Kgo to = 2
      go to 300
C     RESTORE Q   FIND LOWER LIMIT OF UPPER SEGMENT.
C     LIMITS FOR SEARCH
12    P(1) = Q(1)
      P(2) = Q(2)
      P(3) = Q(3)
      Q(1) = OLDQ(1)
      Q(2) = OLDQ(2)
      Q(3) = OLDQ(3)
C     BEAM OFF FIRST
15    IBEAM = 0
      Jgo to = 3
      go to 201
C     IF SEGMENT TOO SHORT, GIVE UP.
13    IF(KFLAG .EQ. 0) go to 50
C     LOWER END NOW NEWLY FOUND POINT.
14    P(1) = ENDA(1)
      P(2) = ENDA(2)
      P(3) = ENDA(3)
      go to 3
C     P INVISIBLE, CHECK Q. IF INVISIBLE, ADVANCE.
30    IBEAM = 0
      IF(PHIQ .EQ. 0) go to 50
C     FIND P
      Jgo to = 4
      go to 201
C     IF NO POINT, GIVE UP.
31    IF(KFLAG) 14,50,14
C
C
C     P VISIBLE, Q INVISIBLE, FIND Q.
C     ENDB = INVISIBLE END OF INTERVAL, ENDA = VISIBLE
200   ENDB(1) = Q(1)
      endB(2) = Q(2)
      endB(3) = Q(3)
      endA(1) = P(1)
      endA(2) = P(2)
      endA(3) = P(3)
C     REQUIRED IVIS FUNCTION
C     IN CASE OF GAP IN SEGMENT, CONSIDER POINT VISIBLE IF ITS VISIB.
C     FUNCTION MATCHES THIS ONE AND UPDATE ENDA, else ENDB.
      PHIA = PHIP
      go to 205
C     P INVISIBLE, Q VISIBLE. FIND P.
201   ENDB(1) = P(1)
      endB(2) = P(2)
      endB(3) = P(3)
      endA(1) = Q(1)
      endA(2) = Q(2)
      endA(3) = Q(3)
      PHIA = PHIQ
205   KFLAG = 0
C     GET PROJECTED LENGTH OF SEGMENT
      PK(1) = XMIN + (ENDA(1)-1.0)*GX(1) - CAMWKG(1)
      PK(2) = YMIN + (ENDA(2)-1.0)*GX(2) - CAMWKG(2)
      PK(3) = ENDA(3)*GX(3) + ZORG - CAMWKG(3)
      call ROTATE(PK,AMTX,ENDA(4))
      PK(1) = XMIN + (ENDB(1)-1.0)*GX(1) - CAMWKG(1)
      PK(2) = YMIN + (ENDB(2)-1.0)*GX(2) - CAMWKG(2)
      PK(3) = ENDB(3)*GX(3) + ZORG - CAMWKG(3)
      call ROTATE(PK,AMTX,ENDB(4))
C     NEXT STEP
210   T(1) = (ENDA(1)+ENDB(1))/2.0
      T(2) = (ENDA(2)+ENDB(2))/2.0
      T(3) = (ENDA(3)+ENDB(3))/2.0
      T(4) = (ENDA(4)+ENDB(4))/2.0
      T(5) = (ENDA(5)+ENDB(5))/2.0
      T(6) = (ENDA(6)+ENDB(6))/2.0
      MFLAG = IVIS(T(1),T(2),T(3),Z,IZDIM1)
      IF(MFLAG .EQ. PHIA) go to 220
C     NOT VISIBLE, RESET INVISIBLE END.
      endB(1) = T(1)
      endB(2) = T(2)
      endB(3) = T(3)
      endB(4) = T(4)
      endB(5) = T(5)
      endB(6) = T(6)
C     CHECK SEGMENT LENGTH (USE MAX OF X, Y DIFFERENCES)
216   SL = FOCALL*AMAX1(ABS(ENDA(4)/ENDA(6)-ENDB(4)/ENDB(6)),
     1    ABS(ENDA(5)/ENDA(6)-ENDB(5)/ENDB(6)))
      IF(SL .GE. PQLMT) go to 210
      go to (5,11,13,31), Jgo to
C     RECORD VISIBLE, UPDATE ENDA
220   KFLAG = MFLAG
      endA(1) = T(1)
      endA(2) = T(2)
      endA(3) = T(3)
      endA(4) = T(4)
      endA(5) = T(5)
      endA(6) = T(6)
      go to 216
C
C
C     DRAW P TO Q
C
C     IF BEAM IS ON, JUST MOVE IT TO Q.
300   IF(IBEAM .GT. 0) go to 310
C     MOVE TO P, BEAM OFF.
      PK(1) = XMIN + (P(1)-1.0)*GX(1) - CAMWKG(1)
      PK(2) = YMIN + (P(2)-1.0)*GX(2) - CAMWKG(2)
      PK(3) = P(3)*GX(3) + ZORG - CAMWKG(3)
      call ROTATE(PK,AMTX,PW)
      PW(1) = (PW(1)/PW(3)-XORG(1))*FOCALL + PLTORG(1) + CENTER(1)
      PW(2) = (PW(2)/PW(3)-XORG(2))*FOCALL + PLTORG(2) + CENTER(2)
      call GSMOVE(PW(1),PW(2))
C     MOVE TO Q, BEAM ON. BEAM IS LEFT AND AT POINT Q.
310   QK(1) = XMIN + (Q(1)-1.0)*GX(1) - CAMWKG(1)
      QK(2) = YMIN + (Q(2)-1.0)*GX(2) - CAMWKG(2)
      QK(3) = Q(3)*GX(3) + ZORG - CAMWKG(3)
      call ROTATE(QK,AMTX,QW)
      QW(1) = (QW(1)/QW(3)-XORG(1))*FOCALL + PLTORG(1) + CENTER(1)
      QW(2) = (QW(2)/QW(3)-XORG(2))*FOCALL + PLTORG(2) + CENTER(2)
      call GSDRAW(QW(1),QW(2))
      IBEAM = 1
      IF(Kgo to .NE. 0) go to 12
C
50    return
      end
      subroutine DSDRAW(X,Y)
C
      common /GCVPOS/ XVPOS, YVPOS
      common /GCCPOS/ XAPOS, YAPOS, IVIS, LCURNT
      LOGICAL LCURNT
      integer GSIVIS
      DIMENSION RX(300,2),RY(300,2)
C
C  CHECK IF LINE GOES THROUGH ANY BLANK AREA
C
      call ZDRAW(XVPOS,YVPOS,X,Y,RX,RY,NUMR)
C
C  DRAW AND UPDATE CURRENT POSTION
C
      if(NUMR.GT.0)then
        DO 100 I=1,NUMR
          call DSMOVE(RX(I,1),RY(I,1))
          XVPOS = RX(I,2)
          YVPOS = RY(I,2)
          call GSRST(XVPOS,YVPOS,X1,Y1)
          IVIS1 = GSIVIS(X1,Y1)
          call DSDRW2(XAPOS,YAPOS,IVIS,X1,Y1,IVIS1)
          XAPOS = X1
          YAPOS = Y1
          IVIS = IVIS1
 100    continue
      endif
      XVPOS=X
      YVPOS=Y
      call GSRST(XVPOS,YVPOS,X1,Y1)
      IVIS1=GSIVIS(X1,Y1)
      XAPOS=X1
      YAPOS=Y1
      IVIS=IVIS1
      return
      end
      subroutine DSDRW2(X0,Y0,IVIS0,X1,Y1,IVIS1)
C
C
C
C
C
C      CLIP LINE TO CLIPPING BOX.   PASS ON ONLY VISIBLE LINE SEGMENTS TO
C       DSDRW3 TO BE DRAWN IN THE CURRENT LINE TYPE.   THIS ROUTINE ALSO
C       WORRIES ABOUT WHETHER THE GRAPHICS DEVICE WILL REQUIRE A "MOVE"
C       BEFORE THE "DRAW" IS DONE.
C
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
C
      LOGICAL LINILT, LPOSND
      LOGICAL LDID1
C
      if(KSYAND(IVIS0,IVIS1) .NE. 0) return
      if(IVIS0 .EQ. 0) go to 10
        LPOSND = .FALSE.
        LINILT = .TRUE.
10    continue
C
C      CALCULATE THE NUMBER OF CLIPS NECESSARY
C
      NCLIPS = 0
      if(IVIS0 .NE. 0) NCLIPS = 1
      if(IVIS1 .NE. 0) NCLIPS = NCLIPS + 1
      if(NCLIPS .NE. 0) go to 100
C
C      LINE TOTALLY VISIBLE, JUST DRAW IT
C
      call DSDRW3(X0,Y0,X1,Y1)
      return
C
C      FIND THE INTERSECTION(S) WITH THE CLIPPING BOX EDGES
C
100   continue
      LDID1 = .FALSE.
      IST = 1
      DX = X1-X0
      if(DX .EQ. 0.0) IST = 3
      IFN = 4
      DY = Y1-Y0
      if(DY .EQ. 0.0) IFN = 2
      if(IST .GT. IFN) return
      IVISC = KSYOR(IVIS0,IVIS1)
      IBIT = 2**(IST-1)
      DO 210 I = IST, IFN
      if(KSYAND(IVISC,IBIT) .EQ. 0) go to 200
      if(I .GT. 2) go to 110
        XI = XCM0
        if(I .EQ. 2) XI = XCM1
        YI = Y0 + (XI-X0)*DY/DX
        if(YI .LT. YCM0 .OR. YI .GT. YCM1) go to 200
        go to 120
110   continue
        YI = YCM0
        if(I .EQ. 4) YI = YCM1
        XI = X0 + (YI-Y0)*DX/DY
        if(XI .LT. XCM0 .OR. XI .GT. XCM1) go to 200
120   continue
C
C      GOT AN INTERSECTION.   IF IT'S THE ONLY ONE, THE DRAW THE LINE.
C
      if(NCLIPS .GT. 1) go to 140
        if(IVIS0 .EQ. 0) go to 130
          call DSDRW3(XI,YI,X1,Y1)
          return
130     continue
          call DSDRW3(X0,Y0,XI,YI)
          return
140   continue
C
C      TWO CLIPS NECESSARY.   IF WE ALREADY HAVE ONE, DRAW THE DOUBLE CLIPPED
C       LINE, else SAVE FIRST CLIP AND WAIT FOR LAST.
C       NOTE, IF DOUBLE CLIPPED, IT DOESN'T MATTER IN WHICH DIRECTION IT
C       IS DRAWN.
C
        if(.NOT. LDID1) go to 180
          call DSDRW3(X2,Y2,XI,YI)
          return
180     continue
          X2 = XI
          Y2 = YI
          LDID1 = .TRUE.
200     continue
      IBIT = 2*IBIT
210   continue
C
C      SEGMENT IS NOT VISIBLE IF WE DROP THRU TO HERE
C
      return
      end
      subroutine DSDRW3(X0,Y0,X1,Y1)
C
C
C
C
C
C      DRAW A LINE FROM (X0,Y0) TO (X1,Y1) IN ABSOLUTE COORDINATES.
C       ASSUMES THAT CLIPPING HAS ALREADY BEEN DONE.   TO SUPPRESS UNNECESSARY
C       "MOVES", THIS IS THE ONLY ROUTINE THAT SHOULD CALL GSDRVR(3,,,).
C       THE LINE IS DRAWN IN THE CURRENT LINE TYPE.   THIS ROUTINE DOES NOT
C       SET THE ABSOLUTE POSITION (XAPOS,YAPOS).   IT IS UP TO THE CALLER TO
C       DO SO IF NECESSARY.
C
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
      LOGICAL LINILT, LPOSND
C
      if(ILNTYP .GT. 1) go to 50
      if(.NOT. LPOSND) CALL GSDRVR(3,X0,Y0)
      go to 220
C
C      SEGMENT LINE TO MAKE CURRENT LINE TYPE
C
50    continue
      if(.NOT. LPOSND) CALL GSDRVR(3,X0,Y0)
      if(.NOT. LINILT) go to 100
      INXTL = 1
      DLEFT = DIST(1,ILNTYP-1)
      LINILT = .FALSE.
C      if(.NOT. LPOSND) CALL GSDRVR(3,X0,Y0)
100   DX = X1-X0
      DY = Y1-Y0
      DL = SQRT(DX**2+DY**2)
C
C  SEE IF THIS SEGMENT IS SHORTER THAT DIST. LEFT ON LINE TYPE
C
      if(DL .LE. DLEFT) go to 200
C
C  SEGMENT IS LONGER, SO ADVANCE TO LINE TYPE BREAK
C
      S = DLEFT/DL
      X0 = S*DX+X0
      Y0 = S*DY+Y0
C
C  SEE IF THIS PART OF THE LINE TYPE IS DRAWN OR SKIPPED
C
      if(KSYAND(INXTL,1) .NE. 0) go to 120
        call GSDRVR(3,X0,Y0)
        go to 140
120   continue
        call GSDRVR(4,X0,Y0)
140   continue
C
C  NOW go to NEXT PORTION OF LINE TYPE
C
      INXTL = INXTL + 1
      if(INXTL .GT. DIST(13,ILNTYP-1)) INXTL = 1
      DLEFT = DIST(INXTL,ILNTYP-1)
      go to 100
C
C  DRAW LAST OF LINE IF DRAWN
C
200   continue
      DLEFT = DLEFT - DL
      if(KSYAND(INXTL,1) .NE. 0) go to 220
        LPOSND = .FALSE.
        go to 240
220   continue
        call GSDRVR(4,X1,Y1)
        LPOSND = .TRUE.
240   continue
      return
      end
      subroutine DSFILL(X,Y,N,TX,TY)
C
C
C
C
C      POLYGON FILL SUPPORT
C      DERIVED FROM "HATCH" ALGORITHM BY KELLY BOOTH
C
      parameter (KZMAXC=255)
      parameter (KZMXBS=1000)
      parameter (KZMXBL=200)
      common /CBLANK/ ZZBLNK(KZMXBL,4),ZZBLKS(KZMXBS,4),KZBLCN,KZBSCN
      common /COLORN/ KZSHD,ZZHSPC,ZZCOLR(KZMAXC,3),KZNCLR,KZCCOL
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1       NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
C
      LOGICAL LINILT, LPOSND,LBLANK
      DIMENSION X(N),Y(N), TX(N),TY(N)
      integer GSIVIS
C
C      DEFINE ARITHMETIC STATEMENT FUNCTION TO MAPPING VERTICES
      YMAP(YYY) = 2.0*AINT(YSCALE*YYY+0.5)+1.0
C
      if(N .LT. 3) return
C
C      CONVERT TO ABSOLUTE COORD.
C
      DO 10 I=1,N
        call GSRST(X(I),Y(I),TX(I),TY(I))
10    continue
      call MINMAX(TY,N,YMIN,YMAX)
      call MINMAX(TX,N,XMIN,XMAX)
C
C      IF CLIPPING NEEDED OR IF NO HARDWARE POLYGON FILL, USE SOFTWARE
C
      if((GSIVIS(XMIN,YMIN) .NE. 0) .OR.
     1   (GSIVIS(XMAX,YMAX) .NE. 0) .OR.
     2   (KSYAND(IDVBTS,256) .EQ. 0)) go to 900
C
C                        IF POLYGON OVERLAPS ANY BLANK AREA,
C                        DO SOFTWARE FILL
C
      if(KZBSCN.GT.0)then
        DO 100 J=1,KZBSCN
          DO 200 I=1,N-1
            call ZFILL(X(I),Y(I),X(I+1),Y(I+1),ZZBLNK(J,1),
     *             ZZBLNK(J,2),ZZBLNK(J,3),ZZBLNK(J,4),LBLANK)
            if(LBLANK) go to 900
 200      continue
          call ZFILL(X(1),Y(1),X(N),Y(N),ZZBLNK(J,1),
     *           ZZBLNK(J,2),ZZBLNK(J,3),ZZBLNK(J,4),LBLANK)
          if(LBLANK) go to 900
 100    continue
      endif
      if(KZBLCN.GT.4)then
        DO 300 J=5,KZBLCN
          DO 400 I=1,N-1
            call ZFILL(X(I),Y(I),X(I+1),Y(I+1),ZZBLNK(J,1),
     *             ZZBLNK(J,2),ZZBLNK(J,3),ZZBLNK(J,4),LBLANK)
            if(LBLANK) go to 900
 400      continue
          call ZFILL(X(1),Y(1),X(N),Y(N),ZZBLNK(J,1),
     *           ZZBLNK(J,2),ZZBLNK(J,3),ZZBLNK(J,4),LBLANK)
          if(LBLANK) go to 900
 300    continue
      endif
      DO 500 J=1,4
        if(ZZBLNK(J,1).GT.-1000.0)then
          DO 600 I=1,N-1
            call ZFILL(X(I),Y(I),X(I+1),Y(I+1),ZZBLNK(J,1),
     *             ZZBLNK(J,2),ZZBLNK(J,3),ZZBLNK(J,4),LBLANK)
            if(LBLANK) go to 900
 600      continue
          call ZFILL(X(1),Y(1),X(N),Y(N),ZZBLNK(J,1),
     *           ZZBLNK(J,2),ZZBLNK(J,3),ZZBLNK(J,4),LBLANK)
          if(LBLANK) go to 900
        endif
 500  continue
C
C      IF CAN HANDLE CONCAVE POLYGONS, JUST CALL DRIVER
C
      if((KSYAND(IDVBTS,512) .EQ. 0) .OR.
     1   (N .EQ. 3)) go to 150
C
C      IF HERE, DRIVER CAN HANDLE CONVEX NON-INTERSECTING POLYGONS ONLY,
C       SO MAKE SURE THIS POLYGON IS CONVEX AND NON-SELF-INTERSECTING.
C
      DX1 = X(1)-X(N)
      DY1 = Y(1)-Y(N)
C            OLD NON-ZERO DELTA-Y
      DY = DY1
C            NUMBER OF TIMES DELTA-Y CHANGES SIGN
      NCHNGS = 0
      L = 1
      COSTH = 0.0
110   continue
C
C            CONVEXITY TEST
C
        DX2 = X(L+1)-X(L)
        DY2 = Y(L+1)-Y(L)
        A = DX1*DY2-DX2*DY1
        if(A*COSTH .LT. 0.0) go to 900
        if(COSTH .EQ. 0.0) COSTH = A
C
C            SELF INTERSECTION CHECK - RELYS ON "CONVEXITY" CHECK
C
        if(DY .NE. 0.0) go to 120
          DY = DY2
          go to 130
120     continue
          if(DY2*DY .GE. 0.0) go to 130
            DY = DY2
            NCHNGS = NCHNGS + 1
            if(NCHNGS .GE. 3) go to 900
130       continue
            DX1 = DX2
            DY1 = DY2
            L = L + 1
            if(L .LT. N) go to 110
150   continue
      call GSDRVR(1024+N,TX,TY)
      return
C
C  SOFTWARE FILL, FILL WITH SOLID LINES
C
900   continue
C
C  LOCATE LONG SIDE AND DO CROSS HATCH
C  IN SAME DIRECTION
C
      DX=X(1)-X(2)
      DY=Y(1)-Y(2)
      RLEN=DX*DX+DY*DY
      DX1=X(2)-X(3)
      DY1=Y(2)-Y(3)
      RLEN1=DX1*DX1+DY1*DY1
      if(RLEN1.GT.RLEN)then
        DY=DY1
      endif
      if(ABS(DY).LT.0.000001)then
        ANGLE=0.0
      else
        ANGLE=90.0
      endif
      call DHATCH(X,Y,N,ANGLE,ZZHSPC,1,TX,TY)
      return
      end
      FUNCTION DSLENS(ISTRNG)
C
C
C
      integer ISTRNG(*)
      character*1 CSTR(160)
C
C     This function returns the length in virtual coordinates of
C        the string ISTRNG.   The current character size is assumed.
C
      parameter (MAXFNT = 18)
      parameter (MXSTRK = MAXFNT*9000)
C
      common /GCCPAR/ CSIZE, CCOS, CSIN
      common /GCFONT/ ICFNSL, MXSLOT,
     1   ISLFNT(MAXFNT), IHIGHT(MAXFNT),
     2   INDX(95*MAXFNT+1), BWIDTH(95*MAXFNT), BXY(MXSTRK)
      integer BWIDTH, BXY
C
      if(ICFNSL .EQ. 1)then
       DSLENS = 9.0*LENG(ISTRNG)*CSIZE
      else
       call ZIN2CH(ISTRNG,CSTR)
       DSLENS = 0.0
       IOFF = 95*(ICFNSL-2) - 32
       DO 100 I=1,LENG(ISTRNG)
        JCHAR = ICHAR(CSTR(I))
        if(JCHAR .LE. 32 .OR. JCHAR .GE. 128)then
          JCHAR = 65
        elseif(BWIDTH(JCHAR+IOFF) .LE. 0)then
          JCHAR = 65
        endif
        IWIDTH = BWIDTH(JCHAR+IOFF)
        DSLENS = DSLENS + CSIZE*IWIDTH
100    continue
      endif
      return
      end
      subroutine DSLTYP(ITYPE)
C
C
C
C
C
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      LOGICAL LINILT, LPOSND
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
C
C      SET THE CURRENT LINE TYPE
C
      ILNTYP = ITYPE
      if(ILNTYP .LE. 0 .OR. (ILNTYP .GT. KZLNCN+1)) ILNTYP = 1
      LINILT = .TRUE.
      return
      end
      subroutine DSMOVE(X,Y)
C
C     TO MOVE TO THE POINT (X,Y).
C
C     INPUT: X,Y - COORDINATE OF POINT TO MOVE TO
C
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
      common /GCVPOS/ XVPOS, YVPOS
      common /GCCPOS/ XAPOS, YAPOS, IVIS, LCURNT
      integer GSIVIS
      LOGICAL LINILT, LPOSND
      LOGICAL LCURNT
C
C      RESET LINE STYLE TO BEGINNING OF PATTERN AND SHOW MOVED
C
      LPOSND = .FALSE.
C
C      TRANSFORM VIRTUAL COORD. TO ABSOLUTE COORD.
C
      XVPOS = X
      YVPOS = Y
      call GSRST(XVPOS,YVPOS,XAPOS,YAPOS)
      IVIS = GSIVIS(XAPOS,YAPOS)
      return
      end
      subroutine DSPSTR(ISTRNG)
C
      integer ISTRNG(*)
      character*1 CSTRNG(80)
C
C      THIS ROUTINE STROKES OUT THE character STRING "BSTRNG" (A BYTE
C      ARRAY WITH 0 AS A TERMINATOR) AT THE CURRENT POSITION.
C
      common /GCVPOS/ XVPOS, YVPOS
      common /GCCOFF/ XOFF, YOFF
      LOGICAL LINILT, LPOSND
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
C
C                        CONVERT INPUT TO character ARRAY
C
      call ZIN2CH(ISTRNG,CSTRNG)
C
C      DON'T DRAW characterS IN LINETYPES
C
      IOLD = ILNTYP
      ILNTYP = 1
      NBYTE = 0
 100  NBYTE = NBYTE + 1
C
C      SAVE THE (0,0) POSITION OF THE character
C
      XOFF = XVPOS
      YOFF = YVPOS
C
C      GET THE character TO STROKE
C
      IICHAR = ICHAR(CSTRNG(NBYTE))
      if(IICHAR .EQ. 0) go to 200
C
C      STROKE THE character
C
      call DSSTRK(IICHAR)
      go to 100
C
C      return LINE TYPE TO THAT OF BEFORE
C
 200  continue
      ILNTYP = IOLD
      return
      end
      subroutine DSSTRK(KHAR)
C
C     THIS ROUTINE STROKES OUT A character.
C
      parameter (MAXFNT = 18)
      parameter (MXSTRK = MAXFNT*9000)
C
      integer BWIDTH, BXY
      common /GCFONT/ ICFNSL, MXSLOT,
     1   ISLFNT(MAXFNT), IHIGHT(MAXFNT),
     2   INDX(95*MAXFNT+1), BWIDTH(95*MAXFNT), BXY(MXSTRK)
      LOGICAL LMOVE
C
C     SPACE FILL ALL NON-PRINTING AND NON-DEFINED WITH SPACE SIZE OF
C      A CAPITAL "A".
C
      JCHAR = (KHAR-32) + 95*(ICFNSL-1)
      if(KHAR .LE. 32 .OR. KHAR .GE. 128) go to 800
      if(ICFNSL .GT. 1)then
        if(BWIDTH(JCHAR-95) .LE. 0) go to 800
      endif
C
C     STROKE THIS character
C
      INDEX = INDX(JCHAR)
      IDONE = INDX(JCHAR+1)
C
C     FIRST POSITION IS AN ASSUMED MOVE
C
      LMOVE = .TRUE.
C
C     GET THE SCALED AND ROTATED NEXT POSITION ON THE character
C
100   continue
150   if(BXY(INDEX) .NE. -64) go to 160
      LMOVE = .TRUE.
      INDEX = INDEX + 1
      go to 100
C
160   X=BXY(INDEX)
      Y=BXY(INDEX+1)
      call GSCCLC(X,Y,DX,DY)
      INDEX = INDEX + 2
      if(LMOVE)then
        call DSMOVE(DX,DY)
      else
        call DSDRAW(DX,DY)
      endif
      LMOVE = .FALSE.
      if(INDEX .LT. IDONE) go to 100
C
C     ALL DONE WITH THE character, MOVE TO NEXT CHARACTER POSITION
C
200   continue
      if(ICFNSL .EQ. 1)then
        WIDTH = 9.0
      else
        WIDTH = BWIDTH(JCHAR-95)
      endif
      call GSCCLC(WIDTH,0.0,DX,DY)
      call DSMOVE(DX,DY)
      return
C
C     USE CAPITAL "A" FOR SIZE OF SPACE AND ALL NON-PRINTING AND NON-DEFINED
C
800   continue
      JCHAR = (65-32) + 95*(ICFNSL-1)
      go to 200
      end
      subroutine DSYMBO(ISYMNO,SYMSIZ)
C
C
C
C      Purpose: PLACE THE DESIRED SYMBOL ('ISYMNO') AT CURRENT
C               LOCATION WITH A SIZE OF SYMSIZ.
C
C      INTERNAL VARIABLES:
C         SYMMOV - (NO. OF MOVES) ARRAY OF CONSECUTIVE X,Y LOCATIONS TO
C                  WHICH LINE IS DRAWN
C         ISYMST - (NO. OF SYMBOLS + 1) ARRAY START OF SYMBOL MOVE LOCATIONS
C
C       NOTES:
C         CJW 7/86 NEW SYMBOL DEFINITIONS:
C         0>  SQUARE
C         1>  OCTAGON
C         2>  TRIANGLE
C         3>  PLUS SIGN
C         4>  'X'
C         5>  DIAMOND
C         6>  UPSIDE DOWN TRIANGLE
C         7>  SQUARE WITH AN 'X' IN IT
C         8>  'X' WITH A HORIZONTAL LINE ACROSS IT
C         9>  DIAMOND WITH A PLUS SIGN IN IT
C        10>  OCTAGON WITH A PLUS SIGN IN IT
C        11>  DOUBLE HOUR GLASS
C        12>  SQUARE WITH A PLUS SIGN IN IT
C        13>  OCTAGON WITH A 'X' IN IT
C        14>  SQUARE WITH A TRIANGLE IN IT
C        15>  PENTAGON WITH A PLUS SIGN IN IT
C        16>  PENTAGON
C        17>  FIVE POINTED STAR
C        18>  SQUARE WITH A DIAMOND IN IT
C
      common /GCVPOS/ XVPOS, YVPOS
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
C
      LOGICAL LINILT, LPOSND
      DIMENSION SYMMOV(105),ISYMST(0:18,4)
C
      SAVE SYMMOV,ISYMST,NSYM
c
C SQUARE: (1)
C OCTAGON: (12)
C TRIANGLE: (31)
C UP SIDE DOWN TRIANGLE: (40)
C HORIZONTAL LINE: (49)
C VERTICAL LINE: (54)
C REVERSED SLASH: (59)
C SLASH: (64)
C DIAMOND: (69)
C STAR: (80)
C PENTAGON: (93)
C
      data SYMMOV/
     1 0.5,-0.5, 0.5,0.5,  -0.5,0.5,  -0.5,-0.5,  0.5,-0.5,  1000.0,
     2 0.2071,-0.5, 0.5,-0.2071, 0.5,0.2071, 0.2071,0.5, -0.2071,0.5,
     & -0.5,0.2071, -0.5,-0.2071, -0.2071,-0.5, 0.2071,-0.5, 1000.0,
     3 0.5,-0.366,  0.0,0.5,  -0.5,-0.366,  0.5,-0.366, 1000.0,
     4 0.5,0.366, 0.0,-0.5,  -0.5,0.366,  0.5,0.366, 1000.0,
     5 -0.5,0.0,  0.5,0.0,  1000.0,
     6 0.0,0.5,  0.0,-0.5,  1000.0,
     7 -0.5,0.5,  0.5,-0.5, 1000.0,
     8 -0.5,-0.5,  0.5,0.5,  1000.0,
     9 0.5,0.0,  0.0,0.5,  -0.5,0.0,  0.0,-0.5,  0.5,0.0,  1000.0,
     A 0.5,0.2, -0.5,0.2, 0.3,-0.5, 0.0,0.5, -0.3,-0.5,
     & 0.5,0.2, 1000.0,
     B 0.5,0.0,  0.0,0.5,  -0.5,0.0,  -0.25,-0.5,  0.25,-0.5,
     & 0.5,0.0, 1000.0/
C
      data ISYMST/
     &  1,12,31,54,64,69,40,59,49,69,12,40,49,12, 1,93,93,80,69,
     &  0, 0, 0,49,59, 0, 0,64,64,54,54,31,54,64,31,54, 0, 0, 1,
     &  0, 0, 0, 0, 0, 0, 0, 1,59,49,49, 0, 1,59, 0,49, 0, 0, 0,
     &  1, 1, 1, 2, 2, 1, 1, 3, 3, 3, 3, 2, 3, 3, 2, 3, 1, 1, 2/
C
      data NSYM /18/
C
C  SAVE CURRENT LOCATION
C
      X0 = XVPOS
      Y0 = YVPOS
C
C  DRAW SYMBOL IN PROPER SIZE
C
      IOLTYP=ILNTYP
      ILNTYP=1
      if(ISYMNO.GE.0  .AND.  ISYMNO.LE.NSYM)then
        DO 200 LINE=1,ISYMST(ISYMNO,4)
          IPTR = ISYMST (ISYMNO,LINE)
          call DSMOVE(X0+SYMSIZ*SYMMOV(IPTR),
     *                  Y0+SYMSIZ*SYMMOV(IPTR+1))
          DO 100 I=1,12
            IPTR=IPTR+2
            if(SYMMOV(IPTR).GT.999.0) go to 200
            call DSDRAW(X0+SYMSIZ*SYMMOV(IPTR),
     *                  Y0+SYMSIZ*SYMMOV(IPTR+1))
  100     continue
  200   continue
      endif
      call DSMOVE(X0,Y0)
      ILNTYP=IOLTYP
      return
      end
      subroutine DUPLEX
C
C
C
C      SET character TYPE TO TRIPLEX
C               LEVEL 1-3, P/S
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFS='DUPLE'
      else
        call ERRMES('DUPLEX',1,3)
      endif
      return
      end
      subroutine ENDSUB(IPLOT)
C
C
C
C      TERMINATE CURENT SUBPLOT
C               LEVEL 1-3,CHANGE TO 1
C
C      INPUT:   IPLOT = NOT USED IN DISSIM
C
      common /CLEVEL/ KZLEVL,KZBEGN
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        call ZINITS
      else
        call ERRMES('ENDSUB',1,3)
      endif
      return
      end
      subroutine ENTOFF(IENTRY)
C
C     DEACTIVATED AN ENTRY IN THE LEGEND BLOCK
C
C     INPUT:   IENTRY = ENTRY ID
C
C
C
      parameter (KZNO=222)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        if(IENTRY.GT.50)then
          call ERRMES('ENTOFF',IENTRY,4)
        else
          KZLGEN(IENTRY)=KZNO
        endif
      else
        call ERRMES('ENTOFF',1,3)
      endif
      return
      end
      subroutine ENTON(IENTRY)
C
C     REACTIVATED AN ENTRY IN THE LEGEND BLOCK
C
C     INPUT:   IENTRY = ENTRY ID
C
C
C
      parameter (KZYES=111)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        if(IENTRY.GT.50)then
          call ERRMES('ENTON',IENTRY,4)
        else
          KZLGEN(IENTRY)=KZYES
        endif
      else
        call ERRMES('ENTON',1,3)
      endif
      return
      end
      subroutine ERRMES(STR,LEVEL1,LEVEL2)
c
c***********************************************************************
c
C     WRITES ERROR MESSAGE
C
C     INPUT:   STR    = NAME OF CALLING ROUTINE
C              LEVEL1 = CORRECT LEVEL 1 OF CALLING
C              LEVEL2 = CORRECT LEVEL 2 OF CALLING, OR
C                       ERROR FLAG
C
      parameter (KZCSPL=3)
      parameter (KZPSPL=4)
      parameter (KZPPLY=5)
c
      character*6 STR1
      character*(*) STR
c
      save carea
      save cdevic
      save clevel
      save cline
c
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CLEVEL/ KZLEVL,KZBEGN
C
C  INCORRECT LEVEL
C
 10   FORMAT(1X,' ### ERROR DETECTED IN ROUTINE -',A6,'-')
c
      IF(KZERR.LE.0.OR.KZERR.GE.100)THEN
        WRITE(*,*)'ERRMES - Warning!'
        WRITE(*,*)'Bad value of KZERR=',KZERR
        WRITE(*,*)'Resetting to 6!'
        KZERR=6
      endif

      if(kzerr.ne.0)then
        WRITE(kzerr,*)' '
        if(STR(1:5).NE.'BLANK'.OR.LEVEL2.LE.4)
     *      WRITE(KZERR,10)STR
        if(LEVEL2.EQ.0)then
          WRITE(KZERR,20)LEVEL1
 20       FORMAT(1X,' ### NOT CALLED AT LEVEL',I2)
        elseif(LEVEL2.LE.3)then
          WRITE(KZERR,30)(I,I=LEVEL1,LEVEL2)
 30       FORMAT(1X,' ### NOT CALLED AT LEVELS',5I2)
C
C                        FOR ENTOFF
C
        elseif(STR.EQ.'ENTON')then
          if(LEVEL2.EQ.4)then
            WRITE(KZERR,35)LEVEL1
 35         FORMAT(1X,' ### ERROR IN VARIALBLE, IENTRY =',I5)
          endif
C
C                        FOR SCALOG
C
        elseif(STR.EQ.'SCALOG')then
          WRITE(KZERR,40)
 40       FORMAT(1X,' ### RANGE TOO SMALL')
C
C                        FOR SETSUB
C
        elseif(STR.EQ.'SETSUB')then
          if(LEVEL2.EQ.4)then
            WRITE(KZERR,50)ZZXAXS
 50         FORMAT(1X,' ### X-AXIS LENGTH =',F10.2)
          elseif(LEVEL2.EQ.5)then
            WRITE(KZERR,60)ZZYAXS
 60         FORMAT(1X,' ### Y-AXIS LENGTH =',F10.2)
          endif
C
C                        FOR BLNKi
C
        elseif(STR(1:5).EQ.'BLANK')then
          STR1(1:5)=STR(1:5)
          STR1(6:6)=CHAR(LEVEL1+48)
          WRITE(KZERR,10)STR1
          WRITE(KZERR,80)
 80       FORMAT(1X,' ### IFRAME TOO LARGE')
C
C                        FOR BLANKA
C
        elseif(STR.EQ.'BLANKA')then
          WRITE(KZERR,90)
  90      FORMAT(1X,' ### MAXIMUM NUMBER OF BLANK AREA EXCEEDED')
C
C                        FOR CURVE
C
        elseif(STR(1:5).EQ.'CURVE')then
          if(LEVEL2.EQ.4)then
            WRITE(KZERR,120)
 120        FORMAT(1X,' ### NUMBER OF POINTS LESS THAN OR',
     *                ' EQUAL TO 3')
            if(KZLSTY.EQ.KZCSPL)then
              WRITE(KZERR,130)
 130          FORMAT(1X,' ### NO CUBSPL INTERPOLATION DONE')
            elseif(KZLSTY.EQ.KZPSPL)then
              WRITE(KZERR,140)
 140          FORMAT(1X,' ### NO PARAMETRIC CUBSPL INTERPOLATION',
     *                ' DONE')
            elseif(KZLSTY.EQ.KZPPLY)then
              WRITE(KZERR,150)
 150          FORMAT(1X,' ### NO POLYNOMIAL INTERPOLATION DONE')
            endif
          elseif(LEVEL2.EQ.5)then
            WRITE(KZERR,110)
 110        FORMAT(1X,' ### NUMBER OF BLANK SYMBOLS EXCEEDS 1000')
          else
            WRITE(*,*)'CURVE reports an error.'
          endif
C
C                        FOR ENTOFF
C
        elseif(STR.EQ.'ENTOFF')then
          if(LEVEL2.EQ.4)then
            WRITE(KZERR,115)LEVEL1
 115        FORMAT(1X,' ### ERROR IN VARIALBLE, IENTRY =',I5)
          endif
C
C                        FOR HEADIN
C
        elseif(STR.EQ.'HEADIN')then
          if(LEVEL2.EQ.4)then
            WRITE(KZERR,160)LEVEL1
 160        FORMAT(1X,' ### ERROR IN VARIABLE, NPAKLIN =',I5)
          elseif(LEVEL2.EQ.5)then
            WRITE(KZERR,170)LEVEL1
 170        FORMAT(1X,' ### LINE NUMBER',I2,' IS TRUNCATED')
          endif
C
C                        FOR HRDROT
C
        elseif(STR.EQ.'HRDROT')then
          WRITE(KZERR,180)LEVEL1
 180      FORMAT(1X,' ### ILLEGAL VALUE IN VARIABLE, STR =',
     *                   A5)
C
C                        FOR HRDSCL
C
        elseif(STR.EQ.'HRDSCL')then
          WRITE(KZERR,180)LEVEL1
C
C                        FOR HRDDEF
C
        elseif(STR.EQ.'HRDDEF')then
          WRITE(KZERR,190)LEVEL1
 190      FORMAT(1X,' ### ILLEGAL VALUE IN VARIABLE, FUNC =,',
     *                   A5)
C
C                        FOR TXTBLK
C
        elseif(STR.EQ.'TXTBLK')then
          WRITE(KZERR,100)
 100      FORMAT(1X,' ### MAXIMUM ALLOWABLE PAKLIN FOR USE OF'
     *                 ,' TXTBLK EXCEEDED')
C
C                        FOR PAKLIN
C
        elseif(STR.EQ.'PAKLIN')then
          if(LEVEL2.EQ.4)then
            WRITE(KZERR,200)
 200        FORMAT(1X,' ### ERROR IN ARRAY SIZE OR LINE SIZE')
          elseif(LEVEL2.EQ.5)then
            WRITE(KZERR,170)LEVEL1
          endif
C
C                        FOR MAXLIN
C
        elseif(STR.EQ.'MAXLIN')then
          if(LEVEL2.EQ.4)then
            WRITE(KZERR,210)LEVEL1
 210        FORMAT(1X,' ### ILLEGAL LINE LENGTH =',I6)
          elseif(LEVEL2.EQ.5)then
            WRITE(KZERR,220)LEVEL1
 220        FORMAT(1X,' ### ARRAY IS TOO SMALL OR TOO LARGE')
          endif
C
C                        FOR LEGHDG
C
        elseif(STR.EQ.'LEGHDG')then
          WRITE(KZERR,230)LEVEL1
 230      FORMAT(1X,' ### TXTBLK BGNSUB EXCEEDS ALLOWABLE LENGTH,',I3,
     *            /1X,' ### AND IS TRUNCATED')
C
C                        FOR PAGE
C
        elseif(STR(1:4).EQ.'PAGE')then
          WRITE(KZERR,240)LEVEL1
 240      FORMAT(1X,' ### PAGE IS CALLED AT LEVEL',I2,',',
     *          /1X,' ### NOT SET FOR CURRENT PLOT')
C
C                        FOR RESET
C
        elseif(STR(1:5).EQ.'RESET')then
          WRITE(KZERR,250)LEVEL1
 250      FORMAT(1X,' ### ERROR IN RESETTING -',A6,' -')
C
C                        FOR BGNSUB
C
        elseif(STR.EQ.'BGNSUB')then
          if(LEVEL2.EQ.4)then
            WRITE(KZERR,50)ZZXAXS
          elseif(LEVEL2.EQ.5)then
            WRITE(KZERR,60)ZZYAXS
          elseif(LEVEL2.EQ.6)then
            WRITE(KZERR,260)LEVEL1
 260        FORMAT(1X,' ### X-AXIS LABEL EXCEEDS ALLOWABLE LENGTH,'
     *       ,I3, /1X,' ### AND IS TRUNCATED')
          elseif(LEVEL2.EQ.7)then
            WRITE(KZERR,270)LEVEL1
 270        FORMAT(1X,' ### Y-AXIS LABEL EXCEEDS ALLOWABLE LENGTH,'
     *       ,I3, /1X,' ### AND IS TRUNCATED')
          elseif(LEVEL2.EQ.8)then
            WRITE(KZERR,280)LEVEL1
 280        FORMAT(1X,' ### PLOT BGNSUB EXCEEDS ALLOWABLE LENGTH,'
     *       ,I3, /1X,' ### AND IS TRUNCATED')
          endif
C
C                        FOR TK4100
C
        elseif(STR.EQ.'TK4100')then
          WRITE(KZERR,285)LEVEL1
 285      FORMAT(1X,' ### DEVICE ',I4,' NOT SUPPORTED AT THIS TIME,',
     *          /1X,' ### ASSUMED TO BE A TEK 4107')
C
C                        FOR XTRLNX
C
        elseif(STR.EQ.'XTRLNX')then
          WRITE(KZERR,260)LEVEL1
C
C                        FOR XTRLNX
C
        elseif(STR.EQ.'XTRLNX')then
          WRITE(KZERR,260)LEVEL1
C
C                        FOR XLABEL
C
        elseif(STR.EQ.'XLABEL')then
          WRITE(KZERR,260)LEVEL1
C
C                        FOR XTRLNY
C
        elseif(STR.EQ.'XTRLNY')then
          WRITE(KZERR,270)LEVEL1
C
C                        FOR XTRLGY
C
        elseif(STR.EQ.'XTRLGY')then
          WRITE(KZERR,270)LEVEL1
C
C                        FOR YLABEL
C
        elseif(STR.EQ.'YLABEL')then
          WRITE(KZERR,270)LEVEL1
C
C                        FOR ZMAPIT
C
        elseif(STR.EQ.'ZMAPIT')then
          if(LEVEL2.EQ.4)then
            WRITE(KZERR,290)LEVEL1
 290        FORMAT(1X,' ### parameter FOR XMARKS HAS AN ILLEGAL',
     *                ' VALUE OF ',I8)
          elseif(LEVEL2.EQ.5)then
            WRITE(KZERR,300)LEVEL1
 300        FORMAT(1X,' ### parameter FOR YMARKS HAS AN ILLEGAL',
     *                ' VALUE OF ',I8)
          endif
        endif
        WRITE(*,*)' '
        endif
C
      IF(KZERR.GT.0)WRITE(*,*)'Current level=',KZLEVL

      return
      end
      subroutine EXTRMA(XV,YV,ZV,XA,XB,YA,YB,IERR)
C
C***********************************************************************
C
      save comdp
c
      common/COMDP/XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,AXISR(2),PLOTX,
     1   PLOTY,PLTORG(2),CAMXYZ(3),MX,NY,FMX,FNY,CAMWKG(6),XORG(3),
     2   GX(3),FX(2),KSCALE,ZORG,CENTER(2),PQLMT,
     3   AMTX(3,3),FOCALL
      DIMENSION LIMIT(2),FLIM(2)
      EQUIVALENCE(U,CAMXYZ(1)),(V,CAMXYZ(2)),(W,CAMXYZ(3)),
     1   (MX,LIMIT(1)),(FMX,FLIM(1))
C
      DIMENSION XS(3), XC(3)
C
      XS(1) = XV
      XS(2) = YV
      XS(3) = ZV
      call ROTATE(XS,AMTX,XC)
c
C  QUIT IF POINT IS BEHIND CAMERA
c
      IF(XC(3).LE.0.0)THEN
        IERR=-1
        return
      endif
C
      XC(1) = XC(1)/XC(3)
      XC(2) = XC(2)/XC(3)
      XA = AMIN1(XA,XC(1))
      XB = AMAX1(XB,XC(1))
      YA = AMIN1(YA,XC(2))
      YB = AMAX1(YB,XC(2))
      IERR = 0
      return
      end
      subroutine FCONTR(Z,NZ,IZ,MX,MY,X1,XMX,Y1,YMY,NL,CL)
C
C***********************************************************************
C
C  FCONTR WILL PRODUCE A CONTOUR PLOT OF THE FUNCTION
C  DEFINED BY Z(I,J) = F(X(I),Y(J)).   IT IS ASSUMED THAT
C  A CALL TO "MAPIT" HAS ALREADY BEEN MADE TO ESTABLISH THE
C  COORDINATE AXIS (X,Y), WITH X LIMITS COVERING THE RANGE
C     X1 TO XMX, AND Y LIMITS COVERING THE RANGE Y1 TO YMY.
C
C     FAST VERSION FOR USE WITH CRTS ONLY
C
C  Arguments:
C
C  Input
C
C     Z  * Type: real array.
C       * The values of the function to contour:
C          Z(I,J) = F(Xi,Yj) where:
C            Xi = X1 + (i-1)*(XMX-X1)/(MX-1)
C            Yj = Y1 + (j-1)*(YMX-Y1)/(MY-1)
C
C     NZ  * Type: integer constant or variable.
C       * The first dimension of the array Z - not necessarily
C        equal to MX, but MX <= NZ.
C
C     IZ  * Type: Anything - a dummy for compatibility
C       * Not used!!!
C
C     MX   * Type: integer constant or variable.
C       * The number of X grid points.
C
C     MY  * Type: integer constant or variable.
C       * The number of Y grid points.
C
C     X1  * Type: real constant or variable.
C       * The minimum X value.
C
C     XMX  * Type: real constant or variable.
C       * The maximum X value.
C
C     Y1  * Type: real constant or variable.
C       * The minimum Y value.
C
C     YMY  * Type: real constant or variable.
C       * The maximum Y value.
C
C     NL  * Type: integer constant or variable.
C       * The number of contour levels.
C
C     CL  * Type: real array.
C       * The coutour levels to draw.   (Same units as
C        F() or Z().)
C
      DIMENSION Z(NZ,MY)
      DIMENSION CL(NL)
      DIMENSION ZB(4)
C
      save contrf
c
      common /CONTRF/ X0,Y0,DX,DY
C
      REAL MIN, MAX
C
C     CALC. SOME SCALING CONSTANTS NEEDED
C
      DX = (XMX-X1)/(MX-1)
      DY = (YMY-Y1)/(MY-1)
      X0 = X1-DX
      Y0 = Y1-DY
C
C     MOVE THRU ARRAY LOOKING FOR CONTOUR SEGMENTS IN EACH BOX.
C
      DO 100 J=1,MY-1
       J2 = J+1
       ZB(3) = Z(1,J2)
       ZB(4) = Z(1,J)
       DO 90 I=1,MX-1
        I2 = I+1
        ZB(1) = ZB(4)
        ZB(2) = ZB(3)
        ZB(3) = Z(I2,J2)
        ZB(4) = Z(I2,J)
C       Test for all points equal -- skip if true
        if( zb(1) .eq. zb(2) .and. zb(1) .eq. zb(3)
     1   .and. zb(1) .eq. zb(4) ) goto 90
C
C       Find extremes of box
c
        min = 1.0E30
        max = -min
        DO 45 l=1, 4
         if ( zb(l) .lt. min ) min = zb(l)
         if ( zb(l) .gt. max ) max = zb(l)
   45 continue
C
C       If a contour falls within the box, plot it.
C
        DO 50 K=1,NL
C
         if( cl(k) .ge. min .and. cl(k) .le. max )
     1    CALL FSGMNT(I,J,ZB,CL(K))
50      continue
90     continue
100   continue
      return
      end
      subroutine FINPLT
C
C***********************************************************************
C
C     RELEASE GRAPHIC DEVICE
C              (LEVEL 1-3, CHANGE TO 0)
C
      save cdevic
      save cdevic
      save gcdsel
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /GCDSEL/ IDEV
      character*11 NPLOT
C
        if(IDEV .NE. 0) CALL GSDRVR(6,DUMMY,DUMMY)
        IDEV = 0
        KZLEVL=0
        if(KZSUM.GT.0)then
          if(KZNPLT.EQ.1)then
            WRITE(NPLOT,63)KZNPLT
 63         FORMAT(I5,' plot ')
          else
            WRITE(NPLOT,64)KZNPLT
 64         FORMAT(I4,' plots ')
          endif
          WRITE(KZSUM,65)NPLOT
 65       FORMAT(1X,A,'completed.')
        endif
      return
      end
      subroutine FRAME
C
C***********************************************************************
C
C     DRAW FRAME AROUND PLOT AREA
C              (LEVEL 2,3)
C
      parameter (ZZIN=2.54)
c
      save carea
      save clevel
      save cpage
      save cunit
c
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
C
C  CHECK LEVEL
C
      if((KZLEVL.EQ.2).OR.(KZLEVL.EQ.3))THEN
        DELTA=0.01*ZZIN*ZZPAGR
        NUMFRM=INT(ZZFRME/DELTA+0.99)
        OFFSET=(ZZFRME-DELTA)/2.0
        X1=ZZXLFT+OFFSET
        X2=ZZXRGT-OFFSET
        Y1=ZZYBOT+OFFSET
        Y2=ZZYTOP-OFFSET
        DO 100 I=0,NUMFRM-1
          DEL=DELTA*I
          TX1=X1-DEL
          TX2=X2+DEL
          TY1=Y1-DEL
          TY2=Y2+DEL
          call DSMOVE(TX1,TY1)
          call DSDRAW(TX2,TY1)
          call DSDRAW(TX2,TY2)
          call DSDRAW(TX1,TY2)
          call DSDRAW(TX1,TY1)
 100    continue
      else
        call ERRMES('FRAME',2,3)
      endif
      return
      end
      subroutine FRMWID(THKNES)
C
C***********************************************************************
C
C     SET UP FRAME THICKNESS
C              (LEVEL 1-3, P/S)
C
C     INPUT:   THKNES = THICKNESS OF FRAME IN INCHES
C
      save carea
      save clevel
      save cpage
      save cunit
c
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
C
      if(KZLEVL.EQ.1)THEN
        UUFRME=THKNES*ZZUNIT
      elseif(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
        UUFRME=THKNES*ZZUNIT
        ZZFRME=UUFRME*ZZPAGR
      else
        call ERRMES('FRMWID',1,3)
      endif
      return
      end
      subroutine FSGMNT(IX,JY,ZB,CLEVEL)
C
C***********************************************************************
C
C     This subroutine looks for a contour segment in the box defined by
C     the points (IX,JY,ZB1), (IX,JY+1,ZB2), (IX+1,JY+1,ZB3)
C     and (IX+1,JY,ZB4).   If found, the segment is drawn.
C
      DIMENSION ZB(4)
      DIMENSION IOFF(4), JOFF(4)
      LOGICAL LFIRST
c
      save contrf
      save ioff
      save joff
c
      common /CONTRF/ X0,Y0,DX,DY
C
      data IOFF /0,0,1,1/
      data JOFF /0,1,1,0/
C
      LFIRST = .TRUE.
      IPREVS = 4
      ZOLD = ZB(IPREVS)
      ZDIFF = CLEVEL - ZOLD
      DO 100 I=1,4
       ZNEW = ZB(I)
       DIFF = CLEVEL - ZNEW
       if(SIGN(1.0,ZDIFF) .EQ. SIGN(1.0,DIFF)) go to 90
       TEMP = ZNEW-ZOLD
       if(TEMP .NE. 0.0) go to 30
        PCTCHG = 0.0
        go to 40
30         continue
        PCTCHG = ZDIFF/TEMP
40     continue
       X = IX + IOFF(IPREVS) + (IOFF(I)-IOFF(IPREVS))*PCTCHG
       Y = JY + JOFF(IPREVS) + (JOFF(I)-JOFF(IPREVS))*PCTCHG
       call SCALE(X*DX+X0,Y*DY+Y0,VX,VY)
       if(LFIRST) go to 50
        call GSDRAW(VX,VY)
        LFIRST = .TRUE.
        go to 90
50         continue
        call GSMOVE(VX,VY)
        LFIRST = .FALSE.
90     continue
       ZDIFF = DIFF
       ZOLD = ZNEW
       IPREVS = I
100   continue
      return
      end
      subroutine FULMAP
C
C***********************************************************************
C
      save gcdprm
c
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
C
C     SET THE PLOTING AREA TO THE WHOLE SCREEN
C
      call PLTBOX(VXL,VXH,VYL,VYH)
      return
      end
      subroutine GDLGI(IFXN,XA,YA)
C
C***********************************************************************
C
C  CGM GENERATOR INTERFACE 
C
      parameter (MAXPTS=20)
      DIMENSION XA(8), YA(3)
C
      SAVE IPTCNT,XRAY,YRAY,XLAST,YLAST
      DIMENSION DCHAR(8),XRAY(MAXPTS),YRAY(MAXPTS)
C
C  MAKE NICE NAMES FOR THE DEVICES RESOLUTION IN X AND Y
C  ("XGUPCM" IS X GRAPHICS UNITS PER CENTIMETER)
C
      EQUIVALENCE (DCHAR(4),XGUPCM), (DCHAR(5),YGUPCM)
      data DCHAR /7777.0,27.94,21.59,200.0,200.0,4095.0,127.0,1.0/
C
C  FIRST VERIFY WE GOT A GRAPHICS FUNCTION WE CAN HANDLE
C
      if(IFXN .LE. 0 .OR. IFXN .GT. 9) return
C
C  NOW DISPATCH TO THE PROPER CODE TO HANDLE THAT FUNCTION
C
      go to (100,200,300,400,500,600,700,800,900) IFXN
C
C  INITIALIZE THE DEVICE
C
  100 continue
C
C  INITIALIZE THE CGM GENERATOR
C
      YA(1)=0.0
      XLAST=-1
      YLAST=-1
      IPTCNT=0
      return
C
C  GET FRESH PLOTTING SURFACE
C
  200 continue
C
C MAKE SURE THAT ALL OF THE PRIMITIVES HAVE BEEN FLUSHED BEFORE
C WE END THE PICTURE
C
      IF(IPTCNT.GT.1)then
        call Plylin(IPTCNT,XRAY,YRAY)
        IPTCNT=0
      endif
      call Newfrm()
      call Linclr(1)
      return
C
C  MOVE
C
  300 continue
C
C  CONVERT CM. TO VIRTUAL COORDINATES AND THEN CONVERT THE
C  VIRTUAL COORDINATES TO CGM COORDINATES
C
      XNOW = XA(1) * 0.035791
      YNOW = YA(1) * 0.035791
C
C  TRY TO ELIMINATE ANY MOVES THAT ARE NOT NECESSARY
C
      IF((XNOW.NE.XLAST).OR.(YNOW.NE.YLAST))then
C
C  IF WE HAVE A POLYLINE ALREADY, GET RID OF IT
C
        IF(IPTCNT.GT.1)then
          call Plylin(IPTCNT,XRAY,YRAY)
          IPTCNT=0
        endif
C
C  STORE THE MOVE COORDINATE IN THE FIRST POSITION
C
        IPTCNT=1
        XRAY(IPTCNT)=XNOW
        YRAY(IPTCNT)=YNOW
        XLAST=XNOW
        YLAST=YNOW
C
      endif
      return
C
C  DRAW
C
  400 continue
      XLAST = XA(1) * 0.035791
      YLAST = YA(1) * 0.035791
C
C IF THERE ARE ALREADY MAXPTS POINTS STORED, GET RID OF THEM
C
      IF(IPTCNT.GE.MAXPTS)then
        call Plylin(IPTCNT,XRAY,YRAY)
        IPTCNT=1
        XRAY(IPTCNT)=XRAY(MAXPTS)
        YRAY(IPTCNT)=YRAY(MAXPTS)
      endif
      IPTCNT=IPTCNT+1
      XRAY(IPTCNT)= XLAST
      YRAY(IPTCNT)= YLAST
      return
C
C  FLUSH GRAPHICS COMMAND BUFFER
C
  500 continue
      return
C
C  RELEASE THE DEVICE
C
  600 continue
C
C  IF A POLYLINE IS ACTIVE, END IT
C
      IF(IPTCNT.GT.1)then
        call Plylin(IPTCNT,XRAY,YRAY)
      endif
C
C  IF A PICTURE IS ACTIVE, END IT
C
      call Grfcls()
      return
C
C  return DEVICE characterISTICS
C
C    DCHAR(1) = DEVICE ID
C         (2) = X LENGTH IN CM
C         (3) = Y LENGTH IN CM
C         (4) = X RESOLUTION PER CM
C         (5) = Y RESOLUTION PER CM
C         (6) = DEVICE COLORS
C         (7) = IDVBTS -- DEVICE BITS
C         (8) = NFLINE
C
  700 continue
      DO 720 I=1,8
      XA(I) = DCHAR(I)
  720 continue
      return
C
C  SELECT CURRENT DRAWING COLOR
C
  800 continue
      return
C
C  PERFORM GRAPHICS INPUT
C
  900 continue
      return
      end
      subroutine GDLSDB
C
      parameter (IBFSIZ=80)
      character BUFFER
      common /GDLSR/ NXTCHR, LUNOUT, BUFFER(IBFSIZ)
C
      if(NXTCHR .EQ. 1) return
      WRITE (LUNOUT,11) (BUFFER(I), I=1,NXTCHR-1)
   11 FORMAT(132A)
      NXTCHR = 1
      return
      end
      subroutine GDLSIN(STRING)
      character*(*) STRING
C
      parameter (IBFSIZ=80)
      character BUFFER
      common /GDLSR/ NXTCHR, LUNOUT, BUFFER(IBFSIZ)
C
      L = NUMCHR(STRING)
      if((NXTCHR+L) .GT. IBFSIZ) CALL GDLSDB
      DO 100 I = 1, L
      BUFFER(NXTCHR) = STRING(I:I)
      NXTCHR = NXTCHR + 1
  100 continue
      return
      end
      subroutine GDLSOB(LUN)
C
      parameter (IBFSIZ=80)
      character BUFFER
      common /GDLSR/ NXTCHR, LUNOUT, BUFFER(IBFSIZ)
C
      LUNOUT = LUN
      NXTCHR = 1
      return
      end
      subroutine GDPOST(IFXN,XA,YA)
      DIMENSION XA(8), YA(3)
C
C  post script driver - hard copy device has 300 dots/inch
C
C-----------------------------------------------------------------------
C
C  paper definitions (inches)
C
      parameter (DPI = 300.0)
      parameter (PSRES = 72.0)
      REAL LFTMAR
      parameter (LFTMAR = 0.5)
      parameter (RTMAR = 0.25)
      parameter (TOPMAR = 0.5)
      parameter (BOTMAR = 0.25)
      parameter (PAPHGT = 11.0)
      parameter (PAPWID = 8.5)
C
C  derived parameters
C
      parameter (USEWID = PAPWID-LFTMAR-RTMAR)
      parameter (USEHGT = PAPHGT-TOPMAR-BOTMAR)
      parameter (WIDCM = 2.54*USEWID)
      parameter (HGTCM = 2.54*USEHGT)
      parameter (RES = DPI/2.54)
      parameter (PRESCM = PSRES/2.54)
      parameter (XOFF = PSRES*LFTMAR)
      parameter (YOFF = PSRES*BOTMAR)
      parameter (MAXPNT = 900)
C
C  declare vars need for driver operation
C
      LOGICAL LNOPLT, LWIDE
      character*15 COORD
      character*1 EOF
      DIMENSION DCHAR(8)
C
C  make nice names for the devices res in x and y
C   ("xgupcm" is x graphics units per centimeter)
C
      EQUIVALENCE (DCHAR(4),XGUPCM), (DCHAR(5),YGUPCM)
C
C  diglib device characteristics words
C
      SAVE DCHAR,COORD
C
      data DCHAR /910.0, WIDCM, HGTCM, RES, RES, 1.0, 27.0, 4.0/
      data COORD /'               '/
C
      EOF = CHAR(4)
      LWIDE = .FALSE.
10    continue
C
C  first verify we got a graphics function we can handle
C
      if(IFXN .LE. 0 .OR. IFXN .GT. 7) return
C
C  now dispatch to the proper code to handle that function
C
      go to (100,200,300,400,500,600,700) IFXN
C
C  initialize the device
C
100   continue
      LUN = INT(XA(1))
C
C  show initialization worked, i.e. we opened the file.
C
      YA(1) = 0.0
      call GDPSOB(LUN)
      call GDPSIB
      call GDPSIN(EOF)
      call GDPSIN(' ')
      call GDPSIN('erasepage initgraphics 1 setlinecap 1 setlinejoin ')
      call GDPSIN('/m {moveto} def /l {lineto} def ')
      call GDPSDB
190   continue
      LNOPLT = .TRUE.
      NPOINT = 0
      return
C
C  **************************
C  get fresh plotting surface
C  **************************
C
200   continue
      if(.NOT. LNOPLT)then
        call GDPSIN('stroke showpage ')
      endif
      call GDPSIN('newpath ')
      go to 190
C
C  ****
C  move
C  ****
C
300   continue
C
C  ****
C  draw
C  ****
C
400   continue
      NPOINT = NPOINT + 1
      if(NPOINT .GT. MAXPNT)then
        call GDPSIN('stroke newpath ')
        if(IFXN .EQ. 4)then
          call GDPSIN(COORD)
          call GDPSIN('m ')
        endif
        NPOINT = 1
      endif
      if(LWIDE)then
        X = PRESCM*YA(1)+XOFF
        Y = PRESCM*(HGTCM-XA(1))+YOFF
      else
        X = PRESCM*XA(1)+XOFF
        Y = PRESCM*YA(1)+YOFF
      endif
      WRITE(COORD,451) X,Y
451   FORMAT(F6.1,1X,F6.1,1X)
      COORD(15:15) = ' '
      call GDPSIN(COORD)
      if(IFXN .EQ. 3)then
        call GDPSIN('m ')
      else
        call GDPSIN('l ')
      endif
      LNOPLT = .FALSE.
      return
C
C  *****************************
C  flush graphics command buffer
C  *****************************
C
500   continue
      return
C
C  ******************
C  release the device
C  ******************
C
600   continue
      if(.NOT. LNOPLT)then
        call GDPSIN('stroke showpage ')
        call GDPSIN(EOF)
        call GDPSIN(' ')
        call GDPSDB
      endif
cw      CLOSE (UNIT=LUN)
      return
C
C  *****************************
C  return device characteristics
C  *****************************
C
700   continue
      DO 720 I=1,8
        XA(I) = DCHAR(I)
720   continue
      if(LWIDE)then
        XA(2) = DCHAR(3)
        XA(3) = DCHAR(2)
      endif
      return
C
C  handle file open error
C
C
C  this routine changes it from landscape to portrait
C
      ENTRY GDPSWD(IFXN,XA,YA)
      LWIDE = .TRUE.
      go to 10
      end
      subroutine GDPSDB
C
C  this routine dumps the buffer
C
      parameter (IBFSIZ =80)
      character*1 CR
      character*1 BUFFER
      common /GDLSR/ NXTCHR, LUNOUT, BUFFER(IBFSIZ)
C
      CR = CHAR(13)
C
      if(NXTCHR .EQ. 1) return
      WRITE(LUNOUT,1000) (BUFFER(I), I=1,NXTCHR-1), CR
 1000 FORMAT(132A1)
      NXTCHR = 1
      return
      end
      subroutine GDPSIB
C
C  ***********************************************************************
C  this routine initializes the buffer
C
      parameter (IBFSIZ =80)
      character*1 BUFFER
      common /GDLSR/ NXTCHR, LUNOUT, BUFFER(IBFSIZ)
C
      NXTCHR = 1
      return
      end
      subroutine GDPSIN(STRING)
      character*(*) STRING
C
C  **********************************************************************
C  this routine inserts data into buffer
C
      parameter (IBFSIZ =80)
      character*1 BUFFER
      common /GDLSR/ NXTCHR, LUNOUT, BUFFER(IBFSIZ)
C
      L = LEN(STRING)
      if((NXTCHR+L) .GT. IBFSIZ) CALL GDPSDB
      DO 100 I = 1, L
        BUFFER(NXTCHR) = STRING(I:I)
        NXTCHR = NXTCHR + 1
100     continue
      return
      end
      subroutine GDPSOB(LUN)
C
C  ***********************************************************************
C  this routine opens the buffer
C
      parameter (IBFSIZ=80)
      character*1 BUFFER
      common /GDLSR/ NXTCHR, LUNOUT, BUFFER(IBFSIZ)
      LUNOUT = LUN
      NXTCHR = 1
      return
      end
      subroutine GDQMSL(IFXN,XA,YA)
      DIMENSION XA(8), YA(3)
C
C     QMS 800 AND 1200 LASER PRINTER DRIVER - MULTIPLE COMMANDS, SINGLE LINE
C
C     DECLARE VARS NEED FOR DRIVER OPERATION
C
      LOGICAL LNOPL, LWIDE, LPENUP
      character*13 COORD
C
      DIMENSION DCHAR(8)
C
C      MAKE NICE NAMES FOR THE DEVICES RESOLUTION IN X AND Y
C       ("XGUPCM" IS X GRAPHICS UNITS PER CENTIMETER)
C
      EQUIVALENCE (DCHAR(4),XGUPCM), (DCHAR(5),YGUPCM)
C
      SAVE DCHAR,NPLOT
C
      data DCHAR /1200.0, 27.94, 21.59, 118.11, 118.11, 1.0, 27.0, 3.0/
      data NPLOT / 0 /
C
      LWIDE = .TRUE.
   10 continue
C
C     FIRST VERIFY WE GOT A GRAPHICS FUNCTION WE CAN HANDLE
C
      if(IFXN .LE. 0 .OR. IFXN .GT. 7) return
C
C      NOW DISPATCH TO THE PROPER CODE TO HANDLE THAT FUNCTION
C
      go to (100,200,300,400,500,600,700) IFXN
C
C  INITIALIZE THE DEVICE
C
  100 continue
      LUN = INT(XA(1))
      YA(1) = 0.0
  190 continue
      call GDLSOB(LUN)
      NPLOT = NPLOT + 1
      call GDLSIN('^PY^-')
      call GDLSDB
      call GDLSIN('^IOL^IJ00000^IT00000^IB08000^F^IGV^PW03')
      call GDLSDB
      LNOPL = .TRUE.
      LPENUP = .FALSE.
      return
C
C  GET FRESH PLOTTING SURFACE
C
  200 continue
      if(LNOPL) return
      call GDLSDB
      go to 190
C
C  MOVE
C
  300 continue
      if(LPENUP) go to 450
      LPENUP = .TRUE.
      COORD(1:2) = '^U'
      go to 450
C
C      ****
C      DRAW
C      ****
C
  400 continue
      if(.NOT. LPENUP) go to 450
      COORD(1:2) = '^D'
      LPENUP = .FALSE.
  450 continue
      if(LWIDE)then
         IX = INT((10.0*XGUPCM*XA(1)/3.0)+0.5)
         IY = INT((10.0*YGUPCM*(DCHAR(3)-YA(1))/3.0)+0.5)
      else
         IX = INT((10.0*XGUPCM*YA(1)/3.0) + 0.5)
         IY = INT((10.0*YGUPCM*XA(1)/3.0) + 0.5)
      endif
      WRITE (COORD(3:13),451) IX,IY
  451 FORMAT(I5.5,':',I5.5)
      call GDLSIN(COORD)
      LNOPL = .FALSE.
      return
C
C      *****************************
C      FLUSH GRAPHICS COMMAND BUFFER
C      *****************************
C
  500 continue
      call GDLSDB
      call GDLSIN('^,')
      call GDLSDB
      return
C
C      ******************
C      RELEASE THE DEVICE
C      ******************
C
  600 continue
      call GDLSIN('^IGE^O^IOP^IJ00500^IT01200^IB10500')
      call GDLSIN('^PN^-')
      call GDLSDB
      return
C
C      *****************************
C      return DEVICE characterISTICS
C      *****************************
C
  700 continue
      DO 720 I=1,8
      XA(I) = DCHAR(I)
  720 continue
      if(.NOT. LWIDE)then
         XA(2) = DCHAR(3)
         XA(3) = DCHAR(2)
      endif
      return
C
C      HANDLE FILE OPEN ERROR
C
 9000 continue
      YA(1) = 3.0
      return
C
C      ***********************************************************
C
      ENTRY GDQMSP(IFXN,XA,YA)
      LWIDE = .FALSE.
      go to 10
      end
      FUNCTION GOODCS(APPROX)
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
C
C     CALCULATE MINIMUM VIRTUAL COORD. SIZE OF CHARS THAT ARE READALBE WITH
C      THE DEVICES RESOLUTION.
C
      SIZE = (GSCHIT()/YRES)/YS
C
C     NOW SCALE UP THIS MINIMUM SIZE SO THAT characterS ARE ABOUT
C      THE SIZE DESIRED.
C
      N = INT(APPROX/SIZE + 0.25)
C
C     MUST BE AT LEAST N=1
C
      if(N .EQ. 0) N=1
C
C     NOW return OUR ANSWER
C
      GOODCS = N*SIZE
      return
      end
      subroutine GOTHIC
C
C
C
C      SET character TYPE TO TRIPLEX
C               LEVEL 1-3, P/S
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFS='GOTHI'
      else
        call ERRMES('GOTHIC',1,3)
      endif
      return
      end
      subroutine GRAFIN(X,Y,LFLAG)
      LOGICAL LFLAG
C
C     DISPLAY AND READ THE GRAPHICS CURSOR AND return ITS POSITION
C     IN WORLD COORDINATES.
C
      LOGICAL LOGX, LOGY
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
C     GET CURSOR POSITION IN VIRTUAL COORDINATES.
C
      call GSCRSR(X,Y,LFLAG,IERR)
      if(IERR .NE. 0) return
      X = (X-XVSTRT)*UDX/XVLEN + UX0
      if(LOGX) X = 10.0**X
      Y = (Y-YVSTRT)*UDY/YVLEN + UY0
      if(LOGY) Y = 10.0**Y
      return
      end
      subroutine GRID(IX,IY)
C
C      DRAW GRID LINES
C               (LEVEL 3)
C
C      INPUT:   IX,IY = NUMBER OF GRID LINES TO BE DRAWN
C                       PER AXES STEP SIZE, IF LESS THAN
C                       THAN ZERO, IT IS THE NUMBER OF
C                       STEPS PER LINE.
C
      parameter (KZLOG=3)
c
      integer MAXGRD
      parameter (MAXGRD=200)
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
      common /CCOORD/ ZZXMIN,ZZXMAX,ZZXSTP,ZZYMIN,ZZYMAX,ZZYSTP
C
      if(KZLEVL.EQ.3)then
C
C  DRAW BORDER AROUND PLOT AREA
C
        call ZFRAME(ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,0.0)
C
C  X GRID LINES
C
        if(IX.NE.0)then
          ICOUNT=1
C
C  LOG X
C
          if(KZXTYP.EQ.KZLOG)then
            DELTA=ALOG10(ZZXSTP)
            IDEL=NINT(DELTA)
C
C  CALCULATE EXPONENTIAL INCREMENT
C
            if(ZZXMIN.LE.0.0)then
              WX=0.0
            else
              WX=ALOG10(ZZXMIN)
            endif
C
C  CALCULATE LINE DENSITY
C
            if(IX.GT.0)then
              NSUB=IX
              IMOD=1
            else
              NSUB=1
              IMOD=-IX
            endif
            NGRID=INT((ALOG10(ZZXMAX)-WX)/DELTA)
C
C  IF NUMBER OF GRID LINES EXCEEDS MAXGRD,
C  CUT IT DOWN TO A REASONABLE NUMBER
C
            MAX=(NGRID+1)*9*NSUB*IDEL
            JMOD=MAX/MAXGRD+1
            if(JMOD.GT.IMOD) IMOD=JMOD
            DO 400 I=1,NGRID+1
              DIV=10.0**WX
C
C  PROCESS CYCLE BY CYCLE, 'IDEL' IS
C  THE NUMBER OF CYCLE PER AXIS LABEL
C
              DO 300 M=1,IDEL
                IPWR=NINT(WX+M)
                DIFF=(10.0**IPWR-10.0**(IPWR-1))/9.0
C
C  9 GRID LINES PER CYCLE
C
                DO 200 J=1,9
                  SUBDIV=DIV
                  DIV=DIV+DIFF
C
C  INCREASE LINE DENSITY
C
                  if(NSUB.GT.1)then
                    DIFF1=DIFF/NSUB
                    DO 100 L=1,NSUB-1
                      ICOUNT=ICOUNT+1
                      SUBDIV=SUBDIV+DIFF1
                      if(MOD(ICOUNT,IMOD).EQ.0)then
                        call SCALE(SUBDIV,ZZYMIN,VX,VY)
                        if(VX.GT.ZZXRGT) go to 600
                        call DSMOVE(VX,ZZYBOT)
                        call DSDRAW(VX,ZZYTOP)
                      endif
 100                continue
                  endif
                  ICOUNT=ICOUNT+1
                  if(MOD(ICOUNT,IMOD).EQ.0)then
                    call SCALE(DIV,ZZYMIN,VX,VY)
                    if(VX.GT.ZZXRGT) go to 600
                    call DSMOVE(VX,ZZYBOT)
                    call DSDRAW(VX,ZZYTOP)
                  endif
 200            continue
 300          continue
              WX=WX+DELTA
 400        continue
C
C  LINEAR X
C
          else
            if(IX.GT.0)then
               DELTA=ZZXSTP/IX
            else
              DELTA=-ZZXSTP*IX
            endif
            WX=ZZXMIN
            NGRID=INT((ZZXMAX-ZZXMIN)/DELTA)
            IMOD=NGRID/MAXGRD+1
            DO 500 I=1,NGRID
              WX=WX+DELTA
              ICOUNT=ICOUNT+1
              if(MOD(ICOUNT,IMOD).EQ.0)then
                call SCALE(WX,ZZYMIN,VX,VY)
                call DSMOVE(VX,ZZYBOT)
                call DSDRAW(VX,ZZYTOP)
              endif
 500        continue
          endif
        endif
C
C  Y GRID LINES
C
 600    continue
        if(IY.NE.0)then
C
C  LOG Y
C
          ICOUNT=1
          if(KZYTYP.EQ.KZLOG)then
            DELTA=ALOG10(ZZYSTP)
            IDEL=NINT(DELTA)
C
C  CALCULATE EXPONENTIAL INCREMENT
C
            if(ZZYMIN.LE.0.0)then
              WY=0.0
            else
              WY=ALOG10(ZZYMIN)
            endif
C
C  CALCULATE LINE DENSITY
C
            if(IY.GT.0)then
              NSUB=IY
              IMOD=1
            else
              NSUB=1
              IMOD=-IY
            endif
C
C  IF NUMBER OF GRID LINES EXCEEDS MAXGRD,
C  CUT IT DOWN TO A REASONABLE NUMBER
C
            NGRID=INT((ALOG10(ZZYMAX)-WY)/DELTA)
            MAX=(NGRID+1)*9*NSUB*IDEL
            JMOD=MAX/MAXGRD+1
            if(JMOD.GT.IMOD) IMOD=JMOD
            DO 1000 I=1,NGRID+1
              DIV=10.0**WY
C
C  PROCESS CYCLE BY CYCLE, 'IDEL' IS
C  THE NUMBER OF CYCLE PER AXIS LABEL
C
              DO 900 M=1,IDEL
                IPWR=NINT(WY+M)
                DIFF=(10.0**IPWR-10.0**(IPWR-1))/9.0
C
C  9 GRID LINES PER CYCLE
C
                DO 800 J=1,9
                  SUBDIV=DIV
                  DIV=DIV+DIFF
                  if(NSUB.GT.1)then
                  DIFF1=DIFF/NSUB
C
C  INCREASE LINE DENSITY
C
                    DO 700 L=1,NSUB-1
                      ICOUNT=ICOUNT+1
                      SUBDIV=SUBDIV+DIFF1
                      if(MOD(ICOUNT,IMOD).EQ.0)then
                        call SCALE(ZZXMIN,SUBDIV,VX,VY)
                        if(VY.GT.ZZYTOP) go to 1200
                        call DSMOVE(ZZXLFT,VY)
                        call DSDRAW(ZZXRGT,VY)
                      endif
 700                continue
                  endif
                  ICOUNT=ICOUNT+1
                  if(MOD(ICOUNT,IMOD).EQ.0)then
                    call SCALE(ZZXMIN,DIV,VX,VY)
                    if(VY.GT.ZZYTOP) go to 1200
                    call DSMOVE(ZZXLFT,VY)
                    call DSDRAW(ZZXRGT,VY)
                    endif
 800           continue
 900          continue
              WY=WY+DELTA
 1000       continue
C
C  LINEAR Y
C
          else
            if(IY.GT.0)then
              DELTA=ZZYSTP/IY
            else
              DELTA=-ZZYSTP*IY
            endif
            WY=ZZYMIN
            NGRID=INT((ZZYMAX-ZZYMIN)/DELTA)
            IMOD=NGRID/MAXGRD+1
            DO 1100 I=1,NGRID
              WY=WY+DELTA
              ICOUNT=ICOUNT+1
              if(MOD(ICOUNT,IMOD).EQ.0)then
                call SCALE(ZZXLFT,WY,VX,VY)
                call DSMOVE(ZZXLFT,VY)
                call DSDRAW(ZZXRGT,VY)
              endif
 1100       continue
          endif
        endif
 1200   continue
C
C  IF NOT AT RIGHT LEVEL
C
      else
        call ERRMES('GRID',3,0)
      endif
      return
      end
      subroutine GSCCLC(X,Y,DX,DY)
C
C  THIS ROUTINE DOES THE character SIZING AND ROTATION.
C
      common /GCCPAR/ CSIZE, CCOS, CSIN
      common /GCCOFF/ XOFF, YOFF
C
      XS = X*CSIZE
      YS = Y*CSIZE
      DX = CCOS*XS + CSIN*YS + XOFF
      DY = CCOS*YS - CSIN*XS + YOFF
      return
      end
      subroutine GSCCMP(IX,IY,XOFF,YOFF,X,Y)
C
      common /GCCPAR/ CSIZE, CCOS, CSIN
      LOGICAL LCURNT
      common /GCCPOS/ XAPOS, YAPOS, IVIS, LCURNT
C
C     SCALE TO PROPER SIZE
C
      XS = CSIZE*IX
      YS = CSIZE*IY
C
C     ROTATE AND TRANSLATE
C
      X = CCOS*XS + CSIN*YS + XOFF
      Y = CCOS*YS - CSIN*XS + YOFF
      return
      end
      FUNCTION GSCHIT()
      parameter (MAXFNT = 18)
      parameter (MXSTRK = MAXFNT*9000)
C
      integer BWIDTH, BXY
      common /GCFONT/ ICFNSL, MXSLOT,
     1   ISLFNT(MAXFNT), IHIGHT(MAXFNT),
     2   INDX(95*MAXFNT+1), BWIDTH(95*MAXFNT), BXY(MXSTRK)
C
      GSCHIT = IHIGHT(ICFNSL)
      return
      end
      subroutine GSCOLR(ICOLOR,IERR)
c
c***********************************************************************
c
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
C
C     SELECT COLOR "ICOLOR" ON CURRENT DEVICE
C
      LOGICAL LNOBKG
      IERR = 0
C
C     LNOBKG SET TO TRUE IF NO BACKGROUND COLOR EXISTS ON THIS DEVICE
C
      LNOBKG = KSYAND(IDVBTS,4) .EQ. 0
C
C     FIRST, ERROR IF BACKGROUND COLOR REQUESTED AND DEVICE DOES NOT
C     SUPPORT BACKGROUND COLOR WRITE.
C
      if(ICOLOR .EQ. 0 .AND. LNOBKG) go to 900
C
C     SECOND, ERROR IF COLOR REQUESTED IS LARGER THAN THE NUMBER OF
C     FOREGROUND COLORS AVAILABLE ON THIS DEVICE
C
      if(ICOLOR .GT. NDCLRS) go to 900
C
C     IF ONLY 1 FOREGROUND COLOR AND NO BACKGROUND COLOR, THEN
C     DRIVER WILL NOT SUPPORT SET COLOR, AND OF COURSE, THE
C     COLOR MUST BE COLOR 1 TO HAVE GOTTEN THIS FAR, SO JUST return
C
      if(NDCLRS .EQ. 1 .AND. LNOBKG) return
C
C     ALL IS OK, SO SET THE REQUESTED COLOR
C
100   CALL GSDRVR(8,FLOAT(ICOLOR),DUMMY)
      return
900   IERR = -1
      return
      end
      subroutine GSCRSR(X,Y,IBUTN,IERR)
c
c***********************************************************************
c
C     THIS ROUTINE TRIES TO GET GRAPHIC INPUT FROM
C     THE CURRENTLY SELECTED DEVICE.   IF THE DEVICE IS NOT CAPABLE
C     OF IT, IERR=-1, else IERR=0 AND:
C       X = X POSITION OF CURSOR IN VIRTUAL COORDINATES
C       Y = Y POSITION OF CURSOR IN VIRTUAL COORDINATES
C       IBUTN = NEW BUTTON STATE
C
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
      DIMENSION ARRAY(3)
C
C     SEE IF DEVICE SUPPORTS CURSOR
C
      if(KSYAND(IDVBTS,1024) .EQ. 0) go to 900
C
C     NOW ASK FOR CURSOR FROM DEVICE DRIVER
C
      call GSDRVR(12,ARRAY,DUMMY)
C
C     CONVERT ABSOLUTE CM. COORD. TO VIRTUAL COORDINATES
C
      call GSIRST(ARRAY(2),ARRAY(3),X,Y)
C
C     GET BUTTON STATE
C
      IBUTN = INT(ARRAY(1))
120   continue
      IERR = 0
      return
C
C     DEVICE DOESN'T SUPPORT GIN
C
900   IERR = -1
      return
      end
      subroutine GSDLNS(ILTYPE,ON1,OFF1,ON2,OFF2)
c
c***********************************************************************
c
C     Define LiNe Style
C
      LOGICAL LINILT, LPOSND
      common /GCLTYP/ ILNTYP, DLEFT, DIST(4,3), LINILT, LPOSND
C
      if(ILTYPE .LT. 2 .OR. ILTYPE .GT. 4) return
      INDEX = ILTYPE-1
      DIST(1,INDEX) = ON1
      DIST(2,INDEX) = OFF1
      DIST(3,INDEX) = ON2
      DIST(4,INDEX) = OFF2
      return
      end
      subroutine GSDNAM(IDEV,BNAME)
c
c***********************************************************************
c
C
C     THIS ROUTINE returnS THE NAME OF THE SPECIFIED DEVICE.
C      DEVICE.
C  DEVICE NAMES ARE LIMITED TO 40 characterS
C
      parameter (MAXDEV=21)
      character*(*) BNAME
c
      NC = NUMCHR(BNAME)

      DO 10 I=1,NC
        BNAME(I:I) = ' '
   10 continue

      IF((IDEV .LE. 0) .OR. (IDEV .GT. MAXDEV)) return
      IF(IDEV .EQ. 1) BNAME ='Tek. 4014'
      IF(IDEV .EQ. 2) BNAME ='Tek. 4115B'
      IF(IDEV .EQ. 3) BNAME ='QMS-L'
      IF(IDEV .EQ. 4) BNAME ='QMS-P'
      IF(IDEV .EQ. 5) BNAME ='VT-240'
      IF(IDEV .EQ. 6) BNAME ='Tek. 4510'
      IF(IDEV .EQ. 7) BNAME ='Sel. 100XL'
      IF(IDEV .EQ. 8) BNAME ='PC (ST-240)'
      IF(IDEV .EQ. 9) BNAME ='Vector File'
      IF(IDEV .EQ.10) BNAME ='LN03 Plus - L'
      IF(IDEV .EQ.11) BNAME ='LN03 Plus - P'
      IF(IDEV .EQ.12) BNAME ='Tek. 4107'
      IF(IDEV .EQ.13) BNAME ='Film Recorder Lab'
C     IF(IDEV .EQ.13) BNAME ='IBM PC w/ EGA board'
      IF(IDEV .EQ.20) BNAME ='ARCGraph System'
      IF(IDEV .EQ.21) BNAME ='CGMLIB'
C
      return
      end
      subroutine GSDRAW(X,Y)
c
c***********************************************************************
c
      integer GSIVIS
      LOGICAL LCURNT
c
      save gccpos
      save gcvpos
c
      common /GCVPOS/ XVPOS, YVPOS
      common /GCCPOS/ XAPOS, YAPOS, IVIS, LCURNT
C
C     TRANSFORM VIRT. COOR. TO SCREEN COORD.
C
      XVPOS = X
      YVPOS = Y
      call GSRST(XVPOS,YVPOS,X1,Y1)
      IVIS1 = GSIVIS(X1,Y1)
      call GSDRW2(XAPOS,YAPOS,IVIS,X1,Y1,IVIS1)
      XAPOS = X1
      YAPOS = Y1
      IVIS = IVIS1
      return
      end
      subroutine GSDRGB(ICOLOR,RED,GRN,BLU,IERR)
c
c***********************************************************************
c
      save gcdchr
c
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      DIMENSION RGB(3)
C
C     DEFINE A COLOR
C
      if( KSYAND(IDVBTS,64) .EQ. 0 .OR.
     1   (ICOLOR .GT. NDCLRS) .OR.
     2   (ICOLOR .LT. 0)) go to 900
      IERR = 0
      RGB(1) = RED
      RGB(2) = GRN
      RGB(3) = BLU
      call GSDRVR(10,FLOAT(ICOLOR),RGB)
      return
900   IERR = -1
      return
      end
      subroutine GSDRVR(IFXN,X,Y)
c
c***********************************************************************
c
C  NOTE: TO ADD NEW DEVICE, PLEASE MAKE SURE THE NEW DEVID IS 
c  DIFFERENT FROM THOSE OF EXISTING DEVICES.
C
      EXTERNAL BLKDAT
c
      integer maxdev
      parameter (MAXDEV=21)
c
      save gcdsel
c
      common /GCDSEL/ IDEV
C
C     SEE IF DEVICE EXISTS
C
      if(IDEV .GT. 0 .AND. IDEV .LE. MAXDEV) go to 10
C
C     NON-EXISTANT DEVICE, SO SEE IF USER IS JUST INQUIRING
C
      if(IFXN .NE. 7) return
C
C     return DEVICE TYPE EQUAL ZERO IF INQUIRING ABOUT
C      NON-EXISTANT DEVICE.
C
      X = 0.0
      return
C
C     DISPATCH TO THE PROPER DRIVER
C
   10 continue
C                                                   DEVID
C                                                  -------
C     DEVICE 1 IS TEK. 4014                         4014.0
C     DEVICE 2 IS TEK. 4115                         4115.0
C     DEVICE 3 IS QMS LASER - LANDSCAPE             1200.0
C     DEVICE 4 IS QMS LASER - PORTRAIT              1200.0
C     DEVICE 5 IS VT-240                             240.0
C     DEVICE 6 IS TEK. 4510 RASTERIZER              4510.0
C     DEVICE 7 IS SELANAR HIREZ                      100.0
C     DEVICE 8 IS PC (SMARTERM-240 EMULATION)         24.0
C     DEVICE 9 IS VECTOR SAVE                       9999.0
C     DEVICE 10 IS LN03 PLUS LASER - LANDSCAPE         3.0
C     DEVICE 11 IS LN03 PLUS LASER - PORTRAIT          3.0
C     DEVICE 12 IS TEK. 4107                        4107.0
C     DEVICE 13 IS FILM RECORDER LAB                  35.0
C     DEVICE 14 IS POSTSCRIPT - LANDSCAPE            910.0
C     DEVICE 15 IS POSTSCRIPT - PORTRAIT             910.0
C     DEVICE 20 IS LOCAL GRAPHICS INTERFACE         7777.0
C
C     IF(IDEV .EQ. 1) CALL GD4014(IFXN,X,Y)
C     IF(IDEV .EQ. 2) CALL GD4115(IFXN,X,Y)
      IF(IDEV .EQ. 3) CALL GDQMSL(IFXN,X,Y)
      IF(IDEV .EQ. 4) CALL GDQMSP(IFXN,X,Y)
C     IF(IDEV .EQ. 5) CALL GDV240(IFXN,X,Y)
C     IF(IDEV .EQ. 6) CALL GD4510(IFXN,X,Y)
C     IF(IDEV .EQ. 7) CALL GDSLNR(IFXN,X,Y)
C     IF(IDEV .EQ. 8) CALL GDST24(IFXN,X,Y)
C     IF(IDEV .EQ. 9) CALL GDSAVE(IFXN,X,Y)
C     IF(IDEV .EQ.10) CALL GDLN3L(IFXN,X,Y)
C     IF(IDEV .EQ.11) CALL GDLN3P(IFXN,X,Y)
C     IF(IDEV .EQ.12) CALL GD4107(IFXN,X,Y)
C     IF(IDEV .EQ.13) CALL GDFRLB(IFXN,X,Y)
      IF(IDEV .EQ.14) CALL GDPOST(IFXN,X,Y)
      IF(IDEV .EQ.15) CALL GDPSWD(IFXN,X,Y)
C     IF(IDEV .EQ.13) CALL PCEGA(IFXN,X,Y)
      IF(IDEV .EQ.20) CALL GDLGI(IFXN,X,Y)
C
      return
      end
      subroutine GSDRW2(X0,Y0,IVIS0,X1,Y1,IVIS1)
c
c***********************************************************************
c
C     CLIP LINE TO CLIPPING BOX.   PASS ON ONLY VISIBLE LINE SEGMENTS TO
C      GSDRW3 TO BE DRAWN IN THE CURRENT LINE TYPE.   THIS ROUTINE ALSO
C      WORRIES ABOUT WHETHER THE GRAPHICS DEVICE WILL REQUIRE A "MOVE"
C      BEFORE THE "DRAW" IS DONE.
C
      save gcclip
      save gcltyp
c
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      LOGICAL LINILT, LPOSND
      common /GCLTYP/ ILNTYP, DLEFT, DIST(4,3), LINILT, LPOSND
C
      LOGICAL LDID1
C
      if(KSYAND(IVIS0,IVIS1) .NE. 0) return
      if(IVIS0 .EQ. 0) go to 10
       LPOSND = .FALSE.
       LINILT = .TRUE.
10    continue
C
C     CALCULATE THE NUMBER OF CLIPS NECESSARY
C
      NCLIPS = 0
      if(IVIS0 .NE. 0) NCLIPS = 1
      if(IVIS1 .NE. 0) NCLIPS = NCLIPS + 1
      if(NCLIPS .NE. 0) go to 100
C
C     LINE TOTALLY VISIBLE, JUST DRAW IT
C
      call GSDRW3(X0,Y0,X1,Y1)
      return
C
C     FIND THE INTERSECTION(S) WITH THE CLIPPING BOX EDGES
C
100   continue
      LDID1 = .FALSE.
      IST = 1
      DX = X1-X0
      if(DX .EQ. 0.0) IST = 3
      IFN = 4
      DY = Y1-Y0
      if(DY .EQ. 0.0) IFN = 2
      if(IST .GT. IFN) return
      IVISC = KSYOR(IVIS0,IVIS1)
      IBIT = 2**(IST-1)
      DO 210 I = IST, IFN
      if(KSYAND(IVISC,IBIT) .EQ. 0) go to 200
      if(I .GT. 2) go to 110
       XI = XCM0
       if(I .EQ. 2) XI = XCM1
       YI = Y0 + (XI-X0)*DY/DX
       if(YI .LT. YCM0 .OR. YI .GT. YCM1) go to 200
       go to 120
110       continue
       YI = YCM0
       if(I .EQ. 4) YI = YCM1
       XI = X0 + (YI-Y0)*DX/DY
       if(XI .LT. XCM0 .OR. XI .GT. XCM1) go to 200
120   continue
C
C     GOT AN INTERSECTION.   IF IT'S THE ONLY ONE, THEn DRAW THE LINE.
C
      if(NCLIPS .GT. 1) go to 140
       if(IVIS0 .EQ. 0) go to 130
        call GSDRW3(XI,YI,X1,Y1)
        return
130       continue
        call GSDRW3(X0,Y0,XI,YI)
        return
140       continue
C
C     TWO CLIPS NECESSARY.   IF WE ALREADY HAVE ONE, DRAW THE DOUBLE CLIPPED
C      LINE, else SAVE FIRST CLIP AND WAIT FOR LAST.
C      NOTE, IF DOUBLE CLIPPED, IT DOESN'T MATTER IN WHICH DIRECTION IT
C      IS DRAWN.
C
       if(.NOT. LDID1) go to 180
        call GSDRW3(X2,Y2,XI,YI)
        return
180        continue
        X2 = XI
        Y2 = YI
        LDID1 = .TRUE.
200    continue
      IBIT = 2*IBIT
210   continue
C
C     SEGMENT IS NOT VISIBLE IF WE DROP THRU TO HERE
C
      return
      end
      subroutine GSDRW3(X0,Y0,X1,Y1)
c
c***********************************************************************
c
C     DRAW A LINE FROM (X0,Y0) TO (X1,Y1) IN ABSOLUTE COORDINATES.
C      ASSUMES THAT CLIPPING HAS ALREADY BEEN DONE.   TO SUPPRESS UNNECESSARY
C      "MOVES", THIS IS THE ONLY ROUTINE THAT SHOULD CALL GSDRVR(3,,,).
C      THE LINE IS DRAWN IN THE CURRENT LINE TYPE.   THIS ROUTINE DOES NOT
C      SET THE ABSOLUTE POSITION (XAPOS,YAPOS).   IT IS UP TO THE CALLER TO
C      DO SO IF NECESSARY.
C
      LOGICAL LINILT, LPOSND
c
      save gcltyp
c
      common /GCLTYP/ ILNTYP, DLEFT, DIST(4,3), LINILT, LPOSND
C
      if(ILNTYP .GT. 1) go to 50
      if(.NOT. LPOSND) CALL GSDRVR(3,X0,Y0)
      go to 220
C
C     SEGMENT LINE TO MAKE CURRENT LINE TYPE
C
50    continue
      if(.NOT. LINILT) go to 100
      INXTL = 1
      DLEFT = DIST(1,ILNTYP-1)
      LINILT = .FALSE.
      if(.NOT. LPOSND) CALL GSDRVR(3,X0,Y0)
100   DX = X1-X0
      DY = Y1-Y0
      DL = SQRT(DX**2+DY**2)
C
C     SEE IF THIS SEGMENT IS SHORTER THAT DIST. LEFT ON LINE TYPE
C
      if(DL .LE. DLEFT) go to 200
C
C     SEGMENT IS LONGER, SO ADVANCE TO LINE TYPE BREAK
C
      S = DLEFT/DL
      X0 = S*DX+X0
      Y0 = S*DY+Y0
C
C     SEE IF THIS PART OF THE LINE TYPE IS DRAWN OR SKIPPED
C
      if(KSYAND(INXTL,1) .NE. 0) go to 120
       call GSDRVR(3,X0,Y0)
       go to 140
120       continue
       call GSDRVR(4,X0,Y0)
140   continue
C
C     NOW go to NEXT PORTION OF LINE TYPE
C
      INXTL = INXTL + 1
      if(INXTL .GT. 4) INXTL = 1
      DLEFT = DIST(INXTL,ILNTYP-1)
      go to 100
C
C     DRAW LAST OF LINE IF DRAWN
C
200   continue
      DLEFT = DLEFT - DL
      if(KSYAND(INXTL,1) .NE. 0) go to 220
       LPOSND = .FALSE.
       go to 240
220       continue
       call GSDRVR(4,X1,Y1)
       LPOSND = .TRUE.
240   continue
      return
      end
      subroutine GSETDP(ANGLE,XSCALE,YSCALE,XTRAN,YTRAN)
c
c***********************************************************************
c
      real pio180
      parameter (pio180=1.745329252E-2)
c
      save gcdprm
c
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
C
C     SET SCALE AND TRANSLATION FACTORS
C
      XS = XSCALE
      YS = YSCALE
      XT = XTRAN
      YT = YTRAN
C
C     SET ROTATION FACTORS
C
      RAD = -ANGLE*PIO180
      RCOS = COS(RAD)
      RSIN = SIN(RAD)
      return
      end
      subroutine GSFILL(X,Y,N,TX,TY)
c
c***********************************************************************
c
      DIMENSION X(N),Y(N), TX(N),TY(N)
C
C     POLYGON FILL SUPPORT
C     DERIVED FROM "HATCH" ALGORITHM BY KELLY BOOTH
C
      save gcdchr
      save gcdprm
      save gcltyp
c
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
      LOGICAL LINILT, LPOSND
      common /GCLTYP/ ILNTYP, DLEFT, DIST(4,3), LINILT, LPOSND
C
      DIMENSION XINS(40)
      integer GSIVIS
      LOGICAL LEFT
C
      SAVE FACT
C
      data FACT /16.0/
C
C  DEFINE ARITHMETIC STATEMENT FUNCTION TO MAPPING VERTICES
C
      YMAP(YYY) = 2.0*AINT(YSCALE*YYY+0.5)+1.0
C
      if(N .LT. 3) return
C
C     CONVERT TO ABSOLUTE COORD.
C
      DO 10 I=1,N
       call GSRST(X(I),Y(I),TX(I),TY(I))
10     continue
      call MINMAX(TY,N,YMIN,YMAX)
      call MINMAX(TX,N,XMIN,XMAX)
C
C     IF CLIPPING NEEDED OR IF NO HARDWARE POLYGON FILL, USE SOFTWARE
C
      if((GSIVIS(XMIN,YMIN) .NE. 0) .OR.
     1   (GSIVIS(XMAX,YMAX) .NE. 0) .OR.
     2   (KSYAND(IDVBTS,256) .EQ. 0)) go to 200
C
C     IF CAN HANDLE CONCAVE POLYGONS, JUST CALL DRIVER
C
      if((KSYAND(IDVBTS,512) .EQ. 0) .OR.
     1   (N .EQ. 3)) go to 150
C
C     IF HERE, DRIVER CAN HANDLE CONVEX NON-INTERSECTING POLYGONS ONLY,
C      SO MAKE SURE THIS POLYGON IS CONVEX AND NON-SELF-INTERSECTING.
C
      DX1 = X(1)-X(N)
      DY1 = Y(1)-Y(N)
C      DY = DY1  @OLD NON-ZERO DELTA-Y
      DY = DY1
C      NCHNGS = 0  @NUMBER OF TIMES DELTA-Y CHANGES SIGN
      NCHNGS = 0
      L = 1
      COSTH = 0.0
110   continue
C
C      CONVEXITY TEST
C
       DX2 = X(L+1)-X(L)
       DY2 = Y(L+1)-Y(L)
       A = DX1*DY2-DX2*DY1
       if(A*COSTH .LT. 0.0) go to 200
       if(COSTH .EQ. 0.0) COSTH = A
C
C      SELF INTERSECTION CHECK - RELYS ON "CONVEXITY" CHECK
C
       if(DY .NE. 0.0) go to 120
        DY = DY2
        go to 130
120    continue
       if(DY2*DY .GE. 0.0) go to 130
        DY = DY2
        NCHNGS = NCHNGS + 1
        if(NCHNGS .GE. 3) go to 200
130    continue
       DX1 = DX2
       DY1 = DY2
       L = L + 1
       if(L .LT. N) go to 110
150   continue
      call GSDRVR(1024+N,TX,TY)
      return
C
C     **********
C     SOFTWARE FILL
C     **********
C
200   continue
C
C     FILLING A POLYGON IS VERY SIMPLE IF AND ONLY IF THE VERTICES OF
C      THE POLYGON NEVER LIE ON A SCAN LINE.   WE CAN FORCE THIS TO HAPPEN
C      BY THE FOLLOWING TRICK: MAKE ALL VERTICES LIE JUST BARELY ABOVE
C      THE SCAN LINE THEY SHOULD LIE ON.   THIS IS DONE BY MAPPING THE
C      VERTICES TO A GRID THAT IS "FACT" TIMES THE DEVICE RESOLUTION,
C      AND THEN DOUBLING THE GRID DENSITY, AND OFFSETTING THE VERTICES
C      BY 1.   BECAUSE WE DO THIS, WE MUST OUTLINE THE POLYGON.
C
C     FILL WITH SOLID LINES
C
      LINOLD = ILNTYP
      ILNTYP = 1
C
      LEFT = .TRUE.
      YSCALE = YS*YRES*FACT
      DLINES = 2.0*FACT
      call MINMAX(Y,N,YMIN,YMAX)
      YMIN = AINT(YMAP(YMIN)/DLINES)*DLINES+DLINES
      YMAX = AINT(YMAP(YMAX)/DLINES)*DLINES
      YSCAN = YMIN
210   continue
       INISEC = 0
C
C      DO EACH SIDE OF THE POLYGON. PUT ANY X INTERSECTIONS
C      WITH THE SCAN LINE Y=YSCAN IN XINS
C
       YBEGIN = YMAP(Y(N))
       XBEGIN = X(N)
       DO 400 L = 1, N
        YEND = YMAP(Y(L))
        DY = YSCAN-YBEGIN
        if(DY*(YSCAN-YEND) .GT. 0.0) go to 390
C
C       INSERT AN INTERSECTION
C
        INISEC = INISEC + 1
        XINS(INISEC) = DY*(X(L)-XBEGIN)/(YEND-YBEGIN)+XBEGIN
C
390     continue
        YBEGIN = YEND
        XBEGIN = X(L)
400     continue
C
C      FILL IF THERE WERE ANY INTERSECTIONS
C
       if(INISEC .EQ. 0) go to 500
C
C      FIRST WE MUST SORT ON X INTERSECTION.
C      USE BUBBLE SORT BECAUSE USUALLY ONLY 2.
C      WHEN "LEFT" IS TRUE, ASCENDING SORT, FALSE IS DESCENDING SORT.
C
       DO 450 I =  1, INISEC-1
        XKEY = XINS(I)
        DO 430 J = I+1, INISEC
         if(.NOT. LEFT) go to 420
         if(XKEY .GE. XINS(J)) go to 430
410      continue
         TEMP = XKEY
         XKEY = XINS(J)
         XINS(J) = TEMP
         go to 430
420      if(XKEY .GT. XINS(J)) go to 410
430      continue
        XINS(I) = XKEY
450     continue
C
C      DRAW FILL LINES NOW
C
       YY = YSCAN/(2.0*YSCALE)
       DO 460 I = 1, INISEC, 2
        call GSMOVE(XINS(I),YY)
        call GSDRAW(XINS(I+1),YY)
460     continue
500    continue
      YSCAN = YSCAN + DLINES
      LEFT = .NOT. LEFT
      if(YSCAN .LE. YMAX) go to 210
C
C     FINALLY, OUTLINE THE POLYGON
C
      call GSMOVE(X(N),Y(N))
      DO 510 L=1,N
       call GSDRAW(X(L),Y(L))
510    continue
C
C     RESTORE LINE TYPE
C
      ILNTYP = LINOLD
      return
      end
      subroutine GSFONT(NEWFNT,IERR)
c
c***********************************************************************
c
C     THIS ROUTINE SELECTS A NEW FONT, LOADING IT IF NECESSARY
C
      parameter (LUN=19)
      parameter (MAXFNT = 18,MAXTOT = 18)
      parameter (MXSTRK=MAXFNT*9000)
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      integer BWIDTH, BXY
C
      save gccpar
      save gcfont
c
      common /GCFONT/ ICFNSL, MXSLOT,
     . ISLFNT(MAXFNT), IHIGHT(MAXFNT),
     . INDX(95*MAXFNT+1), BWIDTH(95*MAXFNT), BXY(MXSTRK)
      common /GCCPAR/ CSIZE, CCOS, CSIN
      character*6 MACHIN
C
C     character*18 FILNAM
      character*80 FILNAM
C
      DIMENSION IBIAS(MAXTOT)
C
  200 FORMAT(//,
     . ' ***** NOTE: SELECTED FONT ID=',I2,', OUT OF RANGE!',//)
  300 FORMAT(//,
     . ' ***** NOTE: Font File [',A,'] not found!',//)
  400 FORMAT(//,
     . ' ***** NOTE: INSUFFICIENT STORAGE FOR FONT [',A,'].',//)
C
      if(NEWFNT .LE. 0 .OR. NEWFNT .GT. MAXTOT)then
       WRITE(*,200) NEWFNT
       IERR = -1
       return
      endif
C
      ISLOT = 1
   10 continue
      if(ISLFNT(ISLOT) .EQ. NEWFNT) go to 40
      ISLOT = ISLOT + 1
      if(ISLOT .LE. MXSLOT) go to 10
C
C     ***** LOAD NEW FONT ***
C
      IF(ISLOT .GT. MAXFNT) ISLOT = 2
      MXSLOT = ISLOT
      ISLFNT(ISLOT) = NEWFNT
C
C
      call SYSTEM2('SYST',MACHIN)
      FILNAM='/usr/people/burkardt/bin/smdfonts.dat'
      KOUNT = 0
    1 continue
      KOUNT = KOUNT + 1
C------------------------BEGIN VAX VMS SPECIFIC CODE--------------------
C     OPEN (UNIT=LUN,FILE=FILNAM,STATUS='OLD',ACCESS='DIRECT',
C    . FORM='UNFORMATTED',RECL=512,ERR=999)
C-------------------------END VAX VMS SPECIFIC CODE---------------------
C-----------------------------BEGIN GENERIC CODE------------------------
C
C THE RECL parameter IS SPECIFIED IN BYTES
C
      OPEN (UNIT=LUN,FILE=FILNAM,STATUS='OLD',ACCESS='DIRECT',
     . FORM='UNFORMATTED',RECL=512*NUMBYT,ERR=999)
C------------------------------END GENERIC CODE-------------------------
      IBIAS(1) = 0
      READ(LUN,REC=1) LOAD,(IBIAS(I),I=2,LOAD)
C
C  CALCULATE OFFSETS INTO TABLES
C
      IST = 1 + 95*(ISLOT-1)
      IEND = IST + 95
      IOFF = INDX(IST)-1
      READ(LUN,REC=1+IBIAS(NEWFNT)) NMCHAR, IHIGHT(ISLOT),
     . (INDX(I),I=IST,IEND),(BWIDTH(I), I=IST-95,IEND-96)
C
C     MAKE SURE IT ALL FITS
C
      if((IOFF+NMCHAR) .GT. MXSTRK)then
        WRITE(*,400)
        CLOSE (UNIT=LUN)
        IERR = -1
        return
      endif
C
C     NOW ADD OFFSET TO INDEXES
C
      DO 20 I=IST,IEND
      if(INDX(I) .GT. 0) INDX(I) = INDX(I) + IOFF
   20 continue
C
C     READ IN THE STROKES
C
      IBLCKS = (NMCHAR+511)/512
      JST = IOFF+1
      DO 30 I=1,IBLCKS-1
        READ(LUN,REC=I+1+IBIAS(NEWFNT)) (BXY(J), J=JST,JST+511)
        JST = JST + 512
   30 continue
      if(JST .LE. NMCHAR+IOFF)then
        READ(LUN,REC=IBLCKS+1+IBIAS(NEWFNT)) (BXY(J), J=JST,NMCHAR+IOFF)
      endif
      CLOSE (UNIT=LUN)
C
C     ***** SELECT THE NEW FONT *****
C
   40 continue
      OLDH = GSCHIT()
      ICFNSL = ISLOT
      CSIZE = OLDH*CSIZE/GSCHIT()
      IERR = 0
      return
C
C     FONT FILE NOT FOUND
C
  999 continue
      IF(KOUNT .LE. 10) go to 1
      WRITE(*,300) FILNAM
      IERR = -2
      return
      end
      subroutine GSGIN(X,Y,BCHAR,IERR)
c
c***********************************************************************
c
      character BCHAR
C
C     THIS ROUTINE TRIES TO GET GRAPHIC INPUT (GIN) FROM
C     THE CURRENTLY SELECTED DEVICE.   IF THE DEVICE IS NOT CAPABLE
C     OF GIN, IERR=-1.   FOR GIN DEVICES, IERR=0 AND:
C       X = X POSITION OF CURSOR IN ABSOLUTE SCREEN CM.
C       Y = Y POSITION OF CURSOR IN ABSOLUTE SCREEN CM.
C       BCHAR = character STUCK AT TERMINAL TO SIGNAL CURSOR HAS
C       BEEN POSITIONED (character).
C
      save gcdchr
      save gcdprm
c
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     . NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
      DIMENSION ARRAY(3)
      character SPACE
C
      SAVE SPACE
C
      data SPACE /' '/
C
C     SEE IF DEVICE SUPPORTS GIN
C
      if(KSYAND(IDVBTS,128) .EQ. 0) go to 30
C
C     NOW ASK FOR GIN FROM DEVICE DRIVER
C
      call GSDRVR(9,ARRAY,DUMMY)
C
C     CONVERT ABSOLUTE CM. COORD. TO VIRTUAL CM. COORDINATES
C
      call GSIRST(ARRAY(2),ARRAY(3),X,Y)
C
C     GET character AS 7 BIT ASCII
C
      ITEM = INT(ARRAY(1))
      if(ITEM .LT. 0 .OR. ITEM .GT. 127)then
      BCHAR = SPACE
      else
      BCHAR = CHAR(ITEM)
      endif
      IERR = 0
      return
C
C     DEVICE DOESN'T SUPPORT GIN
C
   30 continue
      IERR = -1
      return
      end
      FUNCTION GSHGHT()
c
c***********************************************************************
c
C     THIS FUNCTIONS returnS THE CURRENT character HEIGHT IN VIRTUAL
C      COORDINATES.
C
      save gccpar
c
      common /GCCPAR/ CSIZE, CCOS, CSIN
C
      GSHGHT = CSIZE/GSCHIT()
      return
      end
      subroutine GSINPT(X,Y,LFLAG,IERR)
c
c***********************************************************************
c
      LOGICAL LFLAG
C
C     DO A GENERIC GRAPHICS INPUT
C
      character CHAR, SPACE
C
      SAVE SPACE
C
      data SPACE /' '/
C
      call GSCRSR(X,Y,IBUTN,IERR)
      if(IERR .NE. 0) go to 100
       LFLAG = (KSYAND(IBUTN,1) .EQ. 1)
       return
100   continue
      call GSGIN(X,Y,CHAR,IERR)
      if(IERR .NE. 0) return
      LFLAG = (CHAR .EQ. SPACE)
      return
      end
      subroutine GSIRST(XA,YA,XV,YV)
c
c***********************************************************************
c
C     INVERSE ROTATE, SCALE, AND THEN TRANSLATE
C     (TAKE ABSOLUTE COORD. INTO VIRTUAL COORD.)
C
      save gcdprm
c
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
C
C     CONVERT ABSOLUTE CM. COORD. TO VIRTUAL CM. COORDINATES
C
      XTEMP = (XA-XT)/XS
      YV = (YA-YT)/YS
      XV = RCOS*XTEMP-RSIN*YV
      YV = RCOS*YV+RSIN*XTEMP
      return
      end
      FUNCTION GSIVIS(X,Y)
c
c***********************************************************************
c
      integer GSIVIS
C
      save gcclip
c
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
C
C
      GSIVIS = 0
      if(X .LT. XCM0-0.00001) GSIVIS = 1
      if(X .GT. XCM1+0.00001) GSIVIS = GSIVIS + 2
      if(Y .LT. YCM0-0.00001) GSIVIS = GSIVIS + 4
      if(Y .GT. YCM1+0.00001) GSIVIS = GSIVIS + 8
      return
      end
      FUNCTION GSLENS(BSTRNG)
c
c***********************************************************************
c
C  GSLENS returns the length in virtual coordinates of
C  the string BSTRNG.   The current character size is assumed.
C
      character BSTRNG*(*)
c
      save gccpar
      save gcfont
c
      common /GCCPAR/ CSIZE, CCOS, CSIN
      parameter (MAXFNT = 18)
      parameter (MXSTRK = MAXFNT*9000)
C
      integer BWIDTH, BXY
      common /GCFONT/ ICFNSL, MXSLOT,
     1   ISLFNT(MAXFNT), IHIGHT(MAXFNT),
     2   INDX(95*MAXFNT+1), BWIDTH(95*MAXFNT), BXY(MXSTRK)
C
      if(ICFNSL .EQ. 1)then
        GSLENS = 9.0*NUMCHR(BSTRNG)*CSIZE
      else
        GSLENS = 0.0
        IOFF = 95*(ICFNSL-2) - 32
        DO 100 I=1,NUMCHR(BSTRNG)
          JCHAR = ICHAR(BSTRNG(I:I))
          if(JCHAR .LE. 32 .OR. JCHAR .GE. 128
     1        .OR. BWIDTH(JCHAR+IOFF) .LE. 0) JCHAR = 65
          IWIDTH = BWIDTH(JCHAR+IOFF)
          GSLENS = GSLENS + CSIZE*IWIDTH
100     continue
      endif
      return
      end
      subroutine GSLTYP(ITYPE)
c
c***********************************************************************
c
      LOGICAL LINILT, LPOSND
      common /GCLTYP/ ILNTYP, DLEFT, DIST(4,3), LINILT, LPOSND
C
C     SET THE CURRENT LINE TYPE
C
      ILNTYP = ITYPE
      if(ILNTYP .LE. 0 .OR. (ILNTYP .GT. 4)) ILNTYP = 1
      LINILT = .TRUE.
      return
      end
      subroutine GSMOVE(X,Y)
c
c***********************************************************************
c
C     MOVE to THE POINT (X,Y).
C
      integer GSIVIS
      LOGICAL LINILT, LPOSND
      LOGICAL LCURNT
c
      save gccpos
      save gcltyp
      save gcvpos
c
      common /GCLTYP/ ILNTYP, DLEFT, DIST(4,3), LINILT, LPOSND
      common /GCVPOS/ XVPOS, YVPOS
      common /GCCPOS/ XAPOS, YAPOS, IVIS, LCURNT
C
C     RESET LINE STYLE TO BEGINNING OF PATTERN AND SHOW MOVED
C
      LINILT = .TRUE.
      LPOSND = .FALSE.
C
C     TRANSFORM VIRTUAL COORD. TO ABSOLUTE COORD.
C
      XVPOS = X
      YVPOS = Y
      call GSRST(XVPOS,YVPOS,XAPOS,YAPOS)
      IVIS = GSIVIS(XAPOS,YAPOS)
      return
      end
      subroutine GSPOLY(X,Y,N)
c
c***********************************************************************
c
      integer n
c
      integer i
      real x(n)
      real y(n)
C
C     POLYGON SUPPORT
C
      call GSMOVE(X(N),Y(N))
      DO 100 I = 1, N
       call GSDRAW(X(I),Y(I))
100    continue
      return
      end
      subroutine GSPSTR(BSTRNG)
c
c***********************************************************************
c
      character BSTRNG*(*)
C
C     THIS ROUTINE STROKES OUT THE character STRING "BSTRNG" (A BYTE
C     ARRAY WITH 0 AS A TERMINATOR) AT THE CURRENT POSITION.
C
      save gccoff
      save gcltyp
      save gcvpos
c
      common /GCVPOS/ XVPOS, YVPOS
      common /GCCOFF/ XOFF, YOFF
      LOGICAL LINILT, LPOSND
      common /GCLTYP/ ILNTYP, DLEFT, DIST(4,3), LINILT, LPOSND
C
C     DON'T DRAW characterS IN LINETYPES
C
      IOLD = ILNTYP
      ILNTYP = 1
C
      NBYTE = 0
      NCHAR = NUMCHR(BSTRNG)
100   NBYTE = NBYTE + 1
      IF(NBYTE .GT. NCHAR) go to 200
C
C     SAVE THE (0,0) POSITION OF THE character
C
      XOFF = XVPOS
      YOFF = YVPOS
C
C     GET THE character TO STROKE
C
      JCHAR = ICHAR(BSTRNG(NBYTE:NBYTE))
C      if(JCHAR .EQ. 0) go to 200
C
C     STROKE THE character
C
      call GSSTRK(JCHAR)
      go to 100
C
C     return LINE TYPE TO THAT OF BEFORE
C
200   continue
      ILNTYP = IOLD
      return
      end
      subroutine GSRCLP(AREA)
c
c***********************************************************************
c
      DIMENSION AREA(4)
C
C  THIS ROUTINE RESTORES A SAVED ABSOLUTE CLIPPING WINDOW PREVIOUSLY
C  SAVED BY "GSSCLP".   NO ERROR CHECKING IS PERFORMED HERE!!!
C
      save gcclip
c
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
C
      XCM0 = AREA(1)
      XCM1 = AREA(2)
      YCM0 = AREA(3)
      YCM1 = AREA(4)
      return
      end
      subroutine GSRST(XV,YV,XA,YA)
c
c***********************************************************************
c
C     ROTATE, SCALE, AND THEN TRANSLATE COORDINATES
C     (TAKE VIRT. COORD. INTO SCREEN COORD.)
C
      save gcdprm
c
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH

      XTEMP = XV
      XA = XS*(RCOS*XTEMP+RSIN*YV) + XT
      YA = YS*(RCOS*YV-RSIN*XTEMP) + YT
      return
      end
      subroutine GSSCLP(VX0,VX1,VY0,VY1,AREA)
c
c***********************************************************************
c
      DIMENSION AREA(4)
C
C     THIS ROUTINE SAVES THE CURRENT ABSOLUTE CLIPPING WINDOW AND
C      SETS A NEW ABSOLUTE CLIPPING WINDOW GIVEN VIRTUAL COORDINATES.
C      IT MAKES SURE THAT THE CLIPPING WINDOW NEVER LIES OUTSIDE THE
C      PHYSICAL DEVICE.
C
      save gcclip
      save gcdchr
c
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
C
      AREA(1) = XCM0
      AREA(2) = XCM1
      AREA(3) = YCM0
      AREA(4) = YCM1
C
      call GSRST(VX0,VY0,AX0,AY0)
      call GSRST(VX1,VY1,AX1,AY1)
      XCM0 = AMAX1(AMIN1(AX0,AX1),0.0)
      YCM0 = AMAX1(AMIN1(AY0,AY1),0.0)
      XCM1 = AMIN1(XCLIPD,AMAX1(AX0,AX1))
      YCM1 = AMIN1(YCLIPD,AMAX1(AY0,AY1))
      return
      end
      subroutine GSSETC(SIZE,ANGLE)
c
c***********************************************************************
c
      save gccpar
c
      common /GCCPAR/ CSIZE, CCOS, CSIN
C
      SAVE PIO180
C
      data PIO180 /1.745329252E-2/
C
C     SET UP SIZE MULTIPLIER
C
      CSIZE = SIZE/GSCHIT()
C
C     CALCULATE THE ROTATION FACTORS
C
      RAD = -PIO180*ANGLE
      CCOS = COS(RAD)
      CSIN = SIN(RAD)
      return
      end
      subroutine GSSTRK(ICHAR)
c
c***********************************************************************
c
C     THIS ROUTINE STROKES OUT A character.
C
      parameter (MAXFNT = 18)
      parameter (MXSTRK = MAXFNT*9000)
C
      integer BWIDTH, BXY
      LOGICAL LMOVE
c
      save gcfont
c
      common /GCFONT/ ICFNSL, MXSLOT,
     1   ISLFNT(MAXFNT), IHIGHT(MAXFNT),
     2   INDX(95*MAXFNT+1), BWIDTH(95*MAXFNT), BXY(MXSTRK)
C
C     SPACE FILL ALL NON-PRINTING AND NON-DEFINED WITH SPACE SIZE OF
C      A CAPITAL "A".
C
      JCHAR = (ICHAR-32) + 95*(ICFNSL-1)
      if(ICHAR .LE. 32 .OR. ICHAR .GE. 128) go to 800
      if(ICFNSL .GT. 1)then
       if(BWIDTH(JCHAR-95) .LE. 0) go to 800
      endif
C
C     STROKE THIS character
C
      INDEX = INDX(JCHAR)
      IDONE = INDX(JCHAR+1)
C
C     FIRST POSITION IS AN ASSUMED MOVE
C
      LMOVE = .TRUE.
C
C     GET THE SCALED AND ROTATED NEXT POSITION ON THE character
C
100    continue
150    if(BXY(INDEX) .NE. -64) go to 160
       LMOVE = .TRUE.
       INDEX = INDEX + 1
       go to 100
C
160    X=BXY(INDEX)
       Y=BXY(INDEX+1)
       call GSCCLC(X,Y,DX,DY)
       INDEX = INDEX + 2
       if(LMOVE)then
        call GSMOVE(DX,DY)
           else
        call GSDRAW(DX,DY)
       endif
       LMOVE = .FALSE.
       if(INDEX .LT. IDONE) go to 100
C
C     ALL DONE WITH THE character, MOVE TO NEXT CHARACTER POSITION
C
200   continue
      if(ICFNSL .EQ. 1)then
       WIDTH = 9.0
          else
       WIDTH = BWIDTH(JCHAR-95)
      endif
      call GSCCLC(WIDTH,0.0,DX,DY)
      call GSMOVE(DX,DY)
      return
C
C     USE CAPITAL "A" FOR SIZE OF SPACE AND ALL NON-PRINTING AND NON-DEFINED
C
800   continue
      JCHAR = (65-32) + 95*(ICFNSL-1)
      go to 200
      end
      subroutine GSWNDO(UXL,UXH,UYL,UYH,XOFF,YOFF,XAWDTH,YAHIGH)
c
c***********************************************************************
c
C     THIS ROUTINE PROVIDES WINDOW/VIEWPORT MECHANISM.
C
      save gcclip
      save gcdchr
      save gcdprm
c
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
C
C
      RCOS = 1.0
      RSIN = 0.0
      XS = XAWDTH/(UXH-UXL)
      YS = YAHIGH/(UYH-UYL)
      XT = XOFF - XS*UXL
      YT = YOFF - YS*UYL
      XCM0 = AMAX1(AMIN1(XOFF,XOFF+XAWDTH),0.0)
      YCM0 = AMAX1(AMIN1(YOFF,YOFF+YAHIGH),0.0)
      XCM1 = AMIN1(XCLIPD,AMAX1(XOFF,XOFF+XAWDTH))
      YCM1 = AMIN1(YCLIPD,AMAX1(YOFF,YOFF+YAHIGH))
C
C     SAVE VIRTUAL COORDINATE EXTENT
C
      VXL = UXL
      VXH = UXH
      VYL = UYL
      VYH = UYH
      return
      end
      FUNCTION GSXLCM()
C
C     THIS FUNCTION returnS THE X AXIS LENGTH OF THE CURRENT DEVICE
C     IN CENTIMETERS.
C
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
C
      GSXLCM = XLENCM
      return
      end
      FUNCTION GSYLCM()
C
C     THIS FUNCTION returnS THE Y AXIS LENGTH OF THE CURRENT DEVICE
C     IN CENTIMETERS.
C
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
C
      GSYLCM = YLENCM
      return
      end
      subroutine HATCH(XVERT, YVERT, NUMPTS, PHI, CMSPAC, IFLAGS,
     1   XX, YY)
      DIMENSION XVERT(NUMPTS), YVERT(NUMPTS), XX(NUMPTS), YY(NUMPTS)
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     H A T C H
C     by Kelly Booth and modified by Hal Brand
C
C     PROVIDE SHADING FOR A GENERAL POLYGONAL REGION.  THERE IS ABSOLUTELY NO
C     ASSUMPTION MADE ABOUT CONVEXITY.  A POLYGON IS SPECIFIED BY ITS VERTICES,
C     GIVEN IN EITHER A CLOCKWISE OR COUNTER-CLOCKWISE ORDER.  THE DENSITY OF
C     THE SHADING LINES (OR POINTS) AND THE ANGLE FOR THE SHADING LINES ARE
C     BOTH DETERMINED BY THE parameterS PASSED TO THE ROUTINE.
C
C     THE INPUT parameterS ARE INTERPRETED AS FOLLOWS:
C
C      XVERT    -  AN ARRAY OF X COORDINATES FOR THE POLYGON(S) VERTICES
C
C      YVERT    -  AN ARRAY OF Y COORDINATES FOR THE POLYGON(S) VERTICES
C
C      NOTE: AN X VALUE >=1E38 SIGNALS A NEW POLYGON.   THIS ALLOWS
C       FILLING AREAS THAT HAVE HOLES WHERE THE HOLES ARE
C       DEFINED AS POLYGONS.   IT ALSO ALLOWS MULTIPLE
C       POLYGONS TO BE FILLED IN ONE CALL TO HATCH.
C
C      NUMPTS  -  THE NUMBER OF VERTICES IN THE POLYGON(S) INCLUDING
C       THE SEPARATOR(S) IF ANY.
C
C      PHI      -  THE ANGLE FOR THE SHADING, MEASURED COUNTER-CLOCKWISE
C       IN DEGREES FROM THE POSITIVE X-AXIS
C
C      CMSPAC   -  THE DISTANCE IN VIRTUAL COORDINATES (CM. USUALLY)
C       BETWEEN SHADING LINES.   THIS VALUE MAY BE ROUNDED
C       A BIT, SO SOME CUMULATIVE ERROR MAY BE APPARENT.
C
C      IFLAGS   -  GENERAL FLAGS CONTROLLING HATCH
C       0 ==>  BOUNDARY NOT DRAWN, INPUT IS VIRTUAL COORD.
C       1 ==>  BOUNDARY DRAWN, INPUT IS VIRTUAL COORD.
C       2 ==>  BOUNDARY NOT DRAWN, INPUT IS WORLD COORD.
C       3 ==>  BOUNDARY DRAWN, INPUT IS WORLD COORD.
C
C      XX   -  A WORK ARRAY ATLEAST "NUMPTS" LONG.
C
C      YY   -  A SECOND WORK ARRAY ATLEAST "NUMPTS" LONG.
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
C
C     THIS ROUTINE HAS TO MAINTAIN AN INTERNAL ARRAY OF THE TRANSFORMED
C     COORDINATES.  THIS REQUIRES THE PASSING OF THE TWO WORKING ARRAYS
C     CALLED "XX" AND "YY".
C     THIS ROUTINE ALSO NEEDS TO STORE THE INTERSECTIONS OF THE HATCH
C     LINES WITH THE POLYGON.   THIS IS DONE IN "XINTCP".
C
      REAL XINTCP(20)
      LOGICAL LMOVE
C
C     X >= 'BIGNUM' SIGNALS THE END OF A POLYGON IN THE INPUT.
C
      SAVE BIGNUM,FACT,PI180
C
      data BIGNUM /1E38/
      data FACT /16.0/
      data PI180 /0.017453292/
C
C     CHECK FOR VALID NUMBER OF VERTICES.
C
      if(NUMPTS .LT. 3) return
C
C     CONVERT ALL OF THE POINTS TO integer COORDINATES SO THAT THE SHADING
C     LINES ARE HORIZONTAL.  THIS REQUIRES A ROTATION FOR THE GENERAL CASE.
C     THE TRANSFORMATION FROM VIRTUAL TO INTERNAL COORDINATES HAS THE TWO
C     OR THREE PHASES:
C
C     (1)  CONVERT WORLD TO VIRTUAL COORD. IF INPUT IN WORLD COORD.
C
C     (2)  ROTATE CLOCKWISE THROUGH THE ANGLE PHI SO SHADING IS HORIZONTAL,
C
C     (3)  SCALE TO integerS IN THE RANGE
C      [0...2*FACT*(DEVICE_MAXY_COORDINATE)], FORCING COORDINATES
C      TO BE ODD integerS.
C
C     THE COORDINATES ARE ALL ODD SO THAT LATER TESTS WILL NEVER HAVE AN
C     OUTCOME OF "EQUAL" SINCE ALL SHADING LINES HAVE EVEN COORDINATES.
C     THIS GREATLY SIMPLIFIES SOME OF THE LOGIC.
C
C     AT THE SAME TIME THE PRE-PROCESSING IS BEING DONE, THE INPUT IS CHECKED
C     FOR MULTIPLE POLYGONS.  IF THE X-COORDINATE OF A VERTEX IS >= 'BIGNUM'
C     THEN THE POINT IS NOT A VERTEX, BUT RATHER IT SIGNIFIES THE END OF A
C     PARTICULAR POLYGON.  AN IMPLIED EDGE EXISTS BETWEEN THE FIRST AND LAST
C     VERTICES IN EACH POLYGON.  A POLYGON MUST HAVE AT LEAST THREE VERTICES.
C     ILLEGAL POLYGONS ARE REMOVED FROM THE INTERNAL LISTS.
C
C
C     COMPUTE TRIGONOMETRIC FUNCTIONS FOR THE ANGLE OF ROTATION.
C
      COSPHI = COS(PI180*PHI)
      SINPHI = SIN(PI180*PHI)
C
C     FIRST CONVERT FROM WORLD TO VIRTUAL COORD. IF NECESSARY AND ELIMINATE
C     ANY POLYGONS WITH TWO OR FEWER VERTICES
C
      ITAIL = 1
      IHEAD = 0
      DO 120 I = 1, NUMPTS
C
C      ALLOCATE ANOTHER POINT IN THE VERTEX LIST.
C
       IHEAD = IHEAD + 1
C
C      A XVERT >= 'BIGNUM' IS A SPECIAL FLAG.
C
       if(XVERT(I) .LT. BIGNUM) go to 110
        XX(IHEAD) = BIGNUM
        if((IHEAD-ITAIL) .LT. 2) IHEAD = ITAIL - 1
        ITAIL = IHEAD + 1
        go to 120
110    continue
C
C      CONVERT FROM WORLD TO VIRTUAL COORD. IF INPUT IS WORLD COORD.
C
C       if((IFLAGS .AND. 2) .EQ. 0) go to 115
      IF(IFLAGS .EQ. 1) go to 115
      IF(IFLAGS .EQ. 4) go to 115
        call SCALE(XVERT(I),YVERT(I),XX(IHEAD),YY(IHEAD))
        go to 120
115        continue
        XX(IHEAD) = XVERT(I)
        YY(IHEAD) = YVERT(I)
120    continue
      if((IHEAD-ITAIL) .LT. 2) IHEAD = ITAIL - 1
      NVERT = IHEAD
C
C     DRAW BOUNDARY(S) IF DESIRED
C
C      if((IFLAGS .AND. 1) .EQ. 0) go to 138
      IF(IFLAGS .EQ. 2) go to 138
      IF(IFLAGS .EQ. 4) go to 138
      IHEAD = 0
      ITAIL = 1
      LMOVE = .TRUE.
130    continue
       IHEAD = IHEAD + 1
       if(IHEAD .GT. NVERT) go to 133
       if(XX(IHEAD) .NE. BIGNUM) go to 135
133     continue
        call GSDRAW(XX(ITAIL),YY(ITAIL))
        ITAIL = IHEAD + 1
        LMOVE = .TRUE.
        go to 139
135    continue
       if(LMOVE) go to 137
        call GSDRAW(XX(IHEAD),YY(IHEAD))
        go to 139
137    continue
       call GSMOVE(XX(IHEAD),YY(IHEAD))
       LMOVE = .FALSE.
139    continue
       if(IHEAD .LE. NVERT) go to 130
138   continue
C
C     ROTATE TO MAKE SHADING LINES HORIZONTAL
C
      YMIN = BIGNUM
      YMAX = -BIGNUM
      YSCALE = YRES*FACT
      YSCAL2 = 2.0*YSCALE
      DO 140 I = 1, NVERT
       if(XX(I) .EQ. BIGNUM) go to 140
C
C      PERFORM THE ROTATION TO ACHIEVE HORIZONTAL SHADING LINES.
C
       XV1 = XX(I)
       XX(I) = +COSPHI*XV1 + SINPHI*YY(I)
       YY(I) = -SINPHI*XV1 + COSPHI*YY(I)
C
C      CONVERT TO integerS AFTER SCALING, AND MAKE VERTICES ODD. IN Y
C
       YY(I) = 2.0*AINT(YSCALE*YY(I)+0.5)+1.0
       YMIN = AMIN1(YMIN,YY(I))
       YMAX = AMAX1(YMAX,YY(I))
140    continue
C
C     MAKE SHADING START ON A MULTIPLE OF THE STEP SIZE.
C
      STEP = 2.0*AINT(YRES*CMSPAC*FACT)
      YMIN = AINT(YMIN/STEP) * STEP
      YMAX = AINT(YMAX/STEP) * STEP
C
C     AFTER ALL OF THE COORDINATES FOR THE VERTICES HAVE BEEN PRE-PROCESSED
C     THE APPROPRIATE SHADING LINES ARE DRAWN.  THESE ARE INTERSECTED WITH
C     THE EDGES OF THE POLYGON AND THE VISIBLE PORTIONS ARE DRAWN.
C
      Y = YMIN
150    continue
       if(Y .GT. YMAX) go to 250
C
C      INITIALLY THERE ARE NO KNOWN INTERSECTIONS.
C
       ICOUNT = 0
       IBASE = 1
       IVERT = 1
160     continue
        ITAIL = IVERT
        IVERT = IVERT + 1
        IHEAD = IVERT
        if(IHEAD .GT. NVERT) go to 165
        if(XX(IHEAD) .NE. BIGNUM) go to 170
C
C         THERE IS AN EDGE FROM VERTEX N TO VERTEX 1.
C
165       IHEAD = IBASE
          IBASE = IVERT + 1
          IVERT = IVERT + 1
170     continue
C
C       SEE IF THE TWO ENDPOINTS LIE ON
C       OPPOSITE SIDES OF THE SHADING LINE.
C
        YHEAD =  Y - YY(IHEAD)
        YTAIL =  Y - YY(ITAIL)
        if(YHEAD*YTAIL .GE. 0.0) go to 180
C
C       THEY DO.  THIS IS AN INTERSECTION.  COMPUTE X.
C
        ICOUNT = ICOUNT + 1
        DELX = XX(IHEAD) - XX(ITAIL)
        DELY = YY(IHEAD) - YY(ITAIL)
        XINTCP(ICOUNT) = (DELX/DELY) * YHEAD + XX(IHEAD)
180     continue
        if( IVERT .LE. NVERT ) go to 160
C
C      SORT THE X INTERCEPT VALUES.  USE A BUBBLESORT BECAUSE THERE
C      AREN'T VERY MANY OF THEM (USUALLY ONLY TWO).
C
       if(ICOUNT .EQ. 0) go to 240
       DO 200 I = 2, ICOUNT
        XKEY = XINTCP(I)
        K = I - 1
        DO 190 J = 1, K
         if(XINTCP(J) .LE. XKEY) go to 190
         XTEMP = XKEY
         XKEY = XINTCP(J)
         XINTCP(J) = XTEMP
190      continue
        XINTCP(I) = XKEY
200     continue
C
C      ALL OF THE X COORDINATES FOR THE SHADING SEGMENTS ALONG THE
C      CURRENT SHADING LINE ARE NOW KNOWN AND ARE IN SORTED ORDER.
C      ALL THAT REMAINS IS TO DRAW THEM.  PROCESS THE X COORDINATES
C      TWO AT A TIME.
C
       YR = Y/YSCAL2
       DO 230 I = 1, ICOUNT, 2
C
C       CONVERT BACK TO VIRTUAL COORDINATES.
C       ROTATE THROUGH AN ANGLE OF -PHI TO ORIGINAL ORIENTATION.
C       THEN UNSCALE FROM GRID TO VIRTUAL COORD.
C
        XV1 = + COSPHI*XINTCP(I) - SINPHI*YR
        YV1 = + SINPHI*XINTCP(I) + COSPHI*YR
        XV2 = + COSPHI*XINTCP(I+1) - SINPHI*YR
        YV2 = + SINPHI*XINTCP(I+1) + COSPHI*YR
C
C       DRAW THE SEGMENT OF THE SHADING LINE.
C
        call GSMOVE(XV1,YV1)
        call GSDRAW(XV2,YV2)
230     continue
240    continue
       Y = Y + STEP
       go to 150
250   continue
      return
      end
      subroutine HEADIN(LHEAD,IHEAD,HTMULT,NLINES)
C
C
C
C     WRITES HEADING LINES ABOVE PLOT AREA
C              (LEVEL 2,3)
C
C     INPUT:   LHEAD  = HEADING TEXT LINE
C              IHEAD  = NUMBER OF characterS IN LHEAD
C              HTMULT = character SIZE IN MULTIPLE OF
C                       CURRENT character SIZE
C              NLINES = TOTAL NUMBER OF LINES TO BE PLOTTED
C
C     character ARRAY TO STORE THE HEADING TEXT
C
C           KZHDMX = TOTAL NUMBER OF LINES IN HEADING, MAX = 4
C           KZHDNL = CURRENT LINE
C           KZHDTX = (NL-1)*18+1, TEXT
C                    (NL-1)*18+17, NUMBER OF characterS
C                    (NL-1)*18+18, NUMBER OF UNDERLINES
C           ZZHDSZ = CHAR SIZE
C
C                   -------------------------------------------
C           KZHDTX: | TEXT ...           | NCHAR | NUNDERLINES|
C                   -------------------------------------------
C                                           ...
C                                           ...
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CHEADN/ KZHDNL,KZHDMX,KZHDTX(72),ZZHDSZ(4)
      common /CHEADC/ CZHDAC(4,6),CZHDAF(4,6),CZHDAS(4)
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     .                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     .                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZHDAC*1,CZHDAF*5,CZHDAS*5
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      integer LHEAD(*)
      character CFLAG(6)*1,CFONT(6)*5,CSTYLE*5
      EQUIVALENCE (MULTI,RMULTI)
C
      SAVE JTEXT,JNCHAR,JULINE,MAXCHR
C
      data JTEXT,JNCHAR,JULINE /1,17,18/
      data MAXCHR /64/
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
C
C  SAVE MAX NUMBER OF LINE IF THIS IS THE FIRST CALL
C
        if(KZHDMX.EQ.0)then
          NL=ABS(NLINES)
          if(NL.GT.4.OR.NL.EQ.0)then
            KZHDMX=-4
            call ERRMES('HEADIN',NLINES,4)
          else
            KZHDMX=NL
          endif
        endif
C
C                        PACK HEADING LINES
C
        if(KZHDMX.GT.0)then
          KZHDNL=KZHDNL+1
          ICUR=(KZHDNL-1)*18
          if(KZHDNL.LE.KZHDMX)then
            KZHDTX(ICUR+JULINE)=0
C
C                        STORE CHAR SIZE
C
            if(HTMULT.LT.0.0)then
              KZHDTX(ICUR+JULINE)=KZHDTX(ICUR+JULINE)+1
              ZZHDSZ(KZHDNL)=-HTMULT*ZZHITE
            else
              if(HTMULT.LT.0.000001)then
                RMULTI=HTMULT
                ZZHDSZ(KZHDNL)=ZZHITE*MULTI/2.0
              else
                ZZHDSZ(KZHDNL)=HTMULT*ZZHITE
              endif
            endif
C
C                        ADJUST HEADING TEXT HEIGHT, IF NEEDED
C
            IH=ABS(IHEAD)
            RLEN=ZXMESS(LHEAD,IH,CZALFL,CZALFN,CZALFS)*
     .             ABS(ZZHDSZ(KZHDNL))/ZZHITE
            ALEN=ZZXRGT-ZZXLFT
            if(RLEN.GT.ALEN)then
              ZZHDSZ(KZHDNL)=ZZHDSZ(KZHDNL)*ALEN/RLEN
            endif
C
C                        STORE HEADING TEXT
C
            if(IHEAD.LT.0)then
              KZHDTX(ICUR+JULINE)=KZHDTX(ICUR+JULINE)+1
            endif
            KZHDTX(ICUR+JNCHAR)=IH
            ITEMP=MAXCHR
            call ZCOPYS(LHEAD,IH,KZHDTX(ICUR+JTEXT),ITEMP)
            if(ITEMP.LT.0) CALL ERRMES('HEADIN',KZHDNL,5)
            call ZLOALF(CFLAG,CFONT,CSTYLE,CZALFL,CZALFN,CZALFS)
            DO 300 K=1,6
              CZHDAC(KZHDNL,K)=CFLAG(K)
              CZHDAF(KZHDNL,K)=CFONT(K)
 300        continue
            CZHDAS(KZHDNL)=CSTYLE
C
C                        DRAW IF ALL LINES ARE PACKED
C
            if(KZHDNL.EQ.KZHDMX)then
              YLOC=YCM0+ZZYOR+ZZYAXS*ZZYAXR+
     .          AMAX1(ZZHDSZ(1),0.1*ZZYAXS*ZZYAXR)*(1.2-KZHDMX*0.2)
              XLEN=ZZXAXS*ZZXAXR
              DO 100 I=KZHDMX,1,-1
                ICUR=(I-1)*18
C
C                        RETRIEVE FONT
C
                call GSSETC(ZZHDSZ(I),0.0)
                NCHAR=KZHDTX(ICUR+JNCHAR)
                DO 200 K=1,6
                  CFLAG(K)=CZHDAC(I,K)
                  CFONT(K)=CZHDAF(I,K)
 200            continue
                CSTYLE=CZHDAS(I)
                RLEN=ZXMESS(KZHDTX(ICUR+JTEXT),NCHAR,
     .              CFLAG,CFONT,CSTYLE)
                XP1=XCM0+ZZXOR+(XLEN-RLEN)*0.5
                XP2=XP1+RLEN
C
C                        DRAW SECOND UNDERLINE
C
                if(KZHDTX(ICUR+JULINE).GE.2)then
                  call DSMOVE(XP1,YLOC)
                  call DSDRAW(XP2,YLOC)
                  YLOC=YLOC+0.07
                endif
C
C                        DRAW FIRST UNDERLINE
C
                if(KZHDTX(ICUR+JULINE).GE.1)then
                  call DSMOVE(XP1,YLOC)
                  call DSDRAW(XP2,YLOC)
                  YLOC=YLOC+0.14
                endif
C
C                        DRAW TEXT
C
                call ZGETFN(CFONT(1),CSTYLE,KHAR)
                if(KHAR.EQ.1)then
                  call DSMOVE(XP1+ZZHDSZ(I)*0.15,YLOC)
                else
                  call DSMOVE(XP1,YLOC)
                endif
                call ZTEXT(KZHDTX(ICUR+JTEXT),NCHAR,
     *          CFLAG,CFONT,CSTYLE)
                YLOC=YLOC+1.5*ZZHDSZ(I)
 100          continue
              KZHDMX=-4
            endif
          endif
        endif
C
C                        WRONG LEVEL
C
      else
        call ERRMES('HEADIN',2,3)
      endif
      return
      end
      subroutine HEIGHT(HITE)
C
C      SET character HEIGHT
C               LEVEL 1-3, P/S
C
C      INPUT:   HITE = character HEIGHT
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      if(KZLEVL.EQ.1)then
        UUHITE=HITE*ZZUNIT
        call GSSETC(UUHITE,0.0)
      elseif(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
        UUHITE=HITE*ZZUNIT
        ZZHITE=HITE*ZZUNIT*ZZPAGR
        call GSSETC(ZZHITE,ZZANGL)
      else
        call ERRMES('HEIGHT',1,3)
      endif
      return
      end
      subroutine HRDDEF(SWITCH,FUNC)
C
C     TURNS ON HARDWARE FUNCTION
C              (LEVEL 1-3)
C
C     INPUT:   SWITCH = VALUE INDICATING ON/OFF
C               FUNC   = HARDWARE FUNCTION, ONLY AUTOPRINT
C                        ARE SUPPORTED CURRENTLY
C
      parameter (KZYES=111)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      character FUNC*6
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        if(FUNC(1:4).EQ.'COPY')then
          if(SWITCH.GT.0.5) KZCOPY=KZYES
        else
          call ERRMES('HRDDEF',ISTR,4)
        endif
      else
        call ERRMES('HRDDEF',1,3)
      endif
      return
      end
      subroutine HRDHSI(HUE,SAT,AINT)
C
C
C
C
C      SET COLOR HUE, SATURATION AND INTENSITY FOR CURRENT
C               COLOR, LEVEL 1-3, P/S
C
C      INPUT:   HUE  = HUE (FROM 0.0 TO 4.0)
C               SAT  = SATURATION (FROM 0.0 TO 1.0)
C               AINT = INTENSITY (FROM 0.0 TO 1.0)
C
      parameter (KZMAXC=255)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /COLORN/ KZSHD,ZZHSPC,ZZCOLR(KZMAXC,3),KZNCLR,KZCCOL
      common /COLORC/ CZCOLR
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      common /GCVPOS/ XVPOS, YVPOS
      character CZCOLR*8
C
      DIMENSION XA(8),YA(3)
C
      SAVE EPSLON
C
      data EPSLON /0.01/
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
C
C                        CONVERT INPUT 
C
        if(SAT.GE.0.0.AND.SAT.LE.1.0)then
          SAT1=SAT*100.0
        else
          SAT1=100.0
        endif
        if(AINT.GE.0.0.AND.AINT.LE.1.0)then
          AINT1=AINT*50.0
        else
          AINT1=50.0
        endif
        if(HUE.LT.-EPSLON)then
          HUE1=0.0
          SAT1=100.0
          AINT1=0.0
        elseif(HUE.LT.0.25)then
          if(SAT1.LT.99.0) SAT1=0.0
          HUE1=HUE
        elseif(HUE.LT.0.5)then
          HUE1=0.5
        elseif(HUE.LT.3.0)then
          HUE1=HUE
        elseif(HUE.LT.3.5)then
          HUE1=HUE-3.0
        elseif(HUE.LT.3.75)then
          HUE1=0.5
        elseif(HUE.LE.4.0)then
          HUE1=0.0
          if(AINT1.GT.48.0) AINT1=100.0
        else
          HUE1=0.0
          SAT1=100.0
          AINT1=0.0
        endif
        HUE1=HUE1*120.0
C
C  CHECK IF COLOR IS PREVIOUSLY DEFINED
C
        DO 100 I=1,KZNCLR-1
          DHUE=ZZCOLR(I,1)-HUE1
          if(ABS(DHUE).LT.EPSLON)then
            DSAT=ZZCOLR(I,2)-SAT1
            if(ABS(DSAT).LT.EPSLON)then
              DAINT=ZZCOLR(I,3)-AINT1
              if(ABS(DAINT).LT.EPSLON)then
                if(I.LE.NDCLRS)then
                  KZCCOL=I
                  elseif(I.GT.NDCLRS)then
                  KZCCOL=MOD(I,NDCLRS)+1
                endif
                call GSCOLR(KZCCOL,IERR)
                return
              endif
            endif
          endif
 100    continue
C
C  IF NOT PREVIOUSLY DEFINED, DEFINE NEW COLOR
C
        if(KZNCLR.LE.NDCLRS)then
          XA(1)=REAL(KZNCLR)
          KZCCOL=KZNCLR
          YA(1)=HUE1
          YA(2)=AINT1
          YA(3)=SAT1
          ZZCOLR(KZNCLR,1)=HUE1
          ZZCOLR(KZNCLR,2)=SAT1
          ZZCOLR(KZNCLR,3)=AINT1
          call GSDRVR(11,XA,YA)
          KZNCLR=KZNCLR+1
C
C  IF MAXIMUM NUMBER OF COLORS EXCEEDED, PICK A COLOR RANDOMLY
C
        else
          if(KZNCLR.LE.KZMAXC)then
            ZZCOLR(KZNCLR,1)=HUE1
            ZZCOLR(KZNCLR,2)=SAT1
            ZZCOLR(KZNCLR,3)=AINT1
          endif
          KZCCOL=MOD(KZNCLR,NDCLRS)+1
          KZNCLR=KZNCLR+1
        endif
        call GSCOLR(KZCCOL,IERR)
C
C  WRONG LEVEL
C
      else
        call ERRMES('HRDHSI',1,3)
      endif
      return
      end
      subroutine HRDINP(XDATA,YDATA)
C
C     PERFORM GRAPHIC INPUT
C              (LEVEL 3)
C
C     OUTPUT:  XDATA,YDATA = X AND Y VALUES OBTAINED IN
C                             CURRENTLY DEFINED COORDINATE SYSTEM
C
      common /CLEVEL/ KZLEVL,KZBEGN
      character CH
      if(KZLEVL.EQ.3)then
        call CURSOR(XDATA,YDATA,CH)
      else
        call ERRMES('HRDINP',3,0)
      endif
      return
      end
      subroutine HRDRGB(RED,GRN,BLU)
C
C  SET PRIMARY COLOR COMPONENTS FOR CURRENT COLOR,
C               LEVEL 1-3, P/S
C
C      INPUT:   RED  = Red Component   (0.0 - 1.0)
C               GRN  = Green Component (0.0 - 1.0)
C               BLU  = Blue Component  (0.0 - 1.0)
C
      common /CLEVEL/ KZLEVL,KZBEGN
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
C
C  THE INDEX IS UNIMPORTANT BECAUSE A VLT IS NOT USED
C
        INDEX=1
        call GSDRGB(INDEX,RED,GRN,BLU,IERR)
C
C  WRONG LEVEL
C
      else
        call ERRMES('HRDRGB',1,3)
      endif
      return
      end
      subroutine HRDROT(STR)
C
C      ROTATE PLOT ORIENTATION FOR LASER PRINTER
C               (LEVEL 1, P/S)
C
C      INPUT:   STR = PLOT FLAG
C                     IF = 'AUTO', COMPARE PAGE X AND Y LENGTH
C                       TO DECIDE TALL OR WIDE
C                     IF = 'MOVIE', SET TO PORTRAIT
C                     IF = 'COMIC', SET TO LANDSCAPE
C
      parameter (KZYES=111)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
C
      character STR*(*),STRING*4
C
      SAVE KQMS,KLN03,KPOST
C
      data KQMS,KLN03,KPOST /1200,3,910/
      if(KZLEVL.EQ.1)then
        KDEV=INT(DEVID)
        if(LEN(STR).GE.4)then
          if(KDEV.EQ.KQMS.OR.KDEV.EQ.KLN03.OR.KDEV.EQ.KPOST)then
            if(STR(1:4).EQ.'AUTO')then
              KZAUTO=KZYES
            elseif(STR(1:4).EQ.'MOVI')then
              call ZLASAP
            elseif(STR(1:4).EQ.'COMI')then
              call ZLASAL
            else
              STRING=STR(1:4)
              call WCH2IN(STRING,ISTR)
              call ERRMES('HRDROT',ISTR,4)
            endif
          endif
        else
          STRING=STR(1:4)
              call WCH2IN(STRING,ISTR)
          call ERRMES('HRDROT',ISTR,4)
        endif
      else
        call ERRMES('HRDROT',1,0)
      endif
      return
      end
      subroutine HRDSCL(ISTR)
C
C     SCALE PLOT
C              (LEVEL 1, P/S)
C
C     INPUT:   STR =  SCALE FLAG
C                     IF = 'DOWN', SCALE DOWN IF PLOT BOUNDS
C                       EXCEED DEVICE LIMITS
C                     IF = 'CLIP', CLIP ALL OUT OF BOUND
C                       ENTITIES
C                     IF = 'ABORT', ABORT CURRENT PLOT IF
C                       PLOT NOT WITHIN BOUNDS OF DEVICE
C                     IF = 'SCREEN', SCALE UP OR DOWN TO USE
C                       FULL SCREEN
C                     IF = 'NONE', NO CLIPPING DONE
C
      parameter (KZCLIP=12)
      parameter (KZDOWN=13)
      parameter (KZSCRN=14)
      parameter (KZABRT=15)
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
C:  CHANGED TO ACCOMMADATE 8 BYTE WORD
C     character*4 STR
      character*(NUMBYT) STR
      ITEMP=ISTR
C:  CHANGED TO ELIMINATE EQUIVALENCING PROBLEM
      call WIN2CH(ITEMP,STR)
      if(KZLEVL.EQ.1)then
        if(LEN(STR).GE.4)then
C:  CHANGED TO ACCOMMADATE 8 BYTE WORD
C         if(STR.EQ.'DOWN')then
C           KZSCAL=KZDOWN
C         elseif(STR.EQ.'CLIP'.OR.STR.EQ.'NONE')then
C           KZSCAL=KZCLIP
C         elseif(STR.EQ.'ABOR')then
C           KZSCAL=KZABRT
C         elseif(STR.EQ.'SCRE')then
          if(STR(1:4).EQ.'DOWN')then
            KZSCAL=KZDOWN
          elseif(STR(1:4).EQ.'CLIP'.OR.STR(1:4).EQ.'NONE')then
            KZSCAL=KZCLIP
          elseif(STR(1:4).EQ.'ABOR')then
            KZSCAL=KZABRT
          elseif(STR(1:4).EQ.'SCRE')then
            KZSCAL=KZSCRN
          else
            call ERRMES('HRDSCL',ISTR,4)
          endif
        else
          call ERRMES('HRDSCL',ISTR,4)
        endif
      else
        call ERRMES('HRDSCL',1,0)
      endif
      return
      end
      subroutine HRDSHD
C
C
C
C      SET FLAG FOR HARDWARE SHADE
C               LEVEL 1-3, P/S
C
      parameter (KZYES=111)
      parameter (KZMAXC=255)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /COLORN/ KZSHD,ZZHSPC,ZZCOLR(KZMAXC,3),KZNCLR,KZCCOL
      common /COLORC/ CZCOLR
      character CZCOLR*8
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZSHD=KZYES
      else
        call ERRMES('HRDSHD',1,3)
      endif
      return
      end
C
C
      FUNCTION ILABSZ()
C
C     THIS FUNCTION returnS THE MAXIMUM LENGTH THAT "LINLAB" WILL RETURN.
C
      ILABSZ = 6
      return
      end
      subroutine INTAXS
C
C
C
C      SET X AND Y AXES NUMBERS TO integerS
C               LEVEL 1-3, P/S
C
      parameter (KZINT=1)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZXTYP=KZINT
        KZYTYP=KZINT
      else
        call ERRMES('INTAXS',1,3)
      endif
      return
      end
      subroutine INTGRX
C
C
C
C      SET X AXIS NUMBERS TO integerS
C               LEVEL 1-3, P/S
C
      parameter (KZINT=1)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZXTYP=KZINT
      else
        call ERRMES('INTGRX',1,3)
      endif
      return
      end
      subroutine INTGRY
C
C
C
C      SET Y AXIS NUMBERS TO integerS
C               LEVEL 1-3, P/S
C
      parameter (KZINT=1)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZYTYP=KZINT
      else
        call ERRMES('INTGRY',1,3)
      endif
      return
      end
      subroutine INTNO(IVAL,XPOS,YPOS)
C
C
C
C
C     WRITE character STRING ON SCREEN
C              (LEVEL 2-3)
C
C      NPUT:   XPOS,YPOS = X AND Y POSITION IN INCHES
C                          FROM PHYSICAL ORIGIN
C                          IF = 'ABUT', USE CURRENT CURSOR
C                          POSITION
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /GCVPOS/ XVPOS, YVPOS
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      character CZALFL*1,CZALFN*5,CZALFS*5
C
C:  CHANGED TO ACCOMMADATE AN 8 BYTE WORD
C     DIMENSION IMESSA(3)
C     character CMESS*12,KHAR*4
C     EQUIVALENCE (TEMP,KHAR),(CMESS,IMESSA)
      DIMENSION IMESSA(16/NUMBYT)
      character CMESS*16,KHAR*(NUMBYT)
 10   FORMAT(I11)
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
C
C                        CALCULATE VIRTUAL COORDINATE
C
        TEMP=XPOS
C:  CHANGED TO ELIMINATE THE EQUIVALENCING PROBLEM
        call WIN2CH(TEMP,KHAR)
        if(KHAR.EQ.'ABUT')then
          VX=ZZABUX
        else
          VX=XCM0+ZZXOR+XPOS*ZZUNIT*ZZPAGR
        endif
        TEMP=YPOS
C:  CHANGED TO ELIMINATE THE EQUIVALENCING PROBLEM
        call WIN2CH(TEMP,KHAR)
        if(KHAR.EQ.'ABUT')then
          VY=ZZABUY
        else
          VY=YCM0+ZZYOR+YPOS*ZZUNIT*ZZPAGR
        endif
C
C                        DRAW integer
C
        call DSMOVE(VX,VY)
        WRITE(CMESS,10)IVAL
C
C       call SYBYT4('N',IMESSA(3),4,0)
C
C IT IS TIME TO PACK THE characterS INTO AN integer ARRAY
C
        ILOOP=(12-1)/NUMBYT+1
        ICNT=1
        DO 100 I=1,ILOOP
          call WCH2IN(CMESS(ICNT:ICNT+NUMBYT-1),IMESSA(I))
          ICNT=ICNT+NUMBYT
100     continue
        ICNT=MOD(12-1,NUMBYT)+1
        call SYBYT4('N',IMESSA(ILOOP),ICNT,0)
        call ZSTRBL(IMESSA)
        LN=LENG(IMESSA)
        call ZTEXT(IMESSA,LN,CZALFL,CZALFN,CZALFS)
        ZZABUX=XVPOS
        ZZABUY=YVPOS
      else
        call ERRMES('INTNO',3,0)
      endif
      return
      end
      FUNCTION IVIS(XI,ETA,ZETA,Z,IZDIM1)
c
c***********************************************************************
c
C     DETERMINE IF POINT XI, ETA IS VISIBLE
C     POINT IS GIVEN BY XI, ETA, ZIN
C     AND VISIBILITY IS TESTED WITH RESPECT TO SURFACE Z(X,Y)
C      XI, ETA COORDINATES EXPRESSED AS INDICES OF ARRAY Z, BUT NEED NOT
C     BE integerS IN GENERAL. FOR ENTRY IVIS, THEY MUST BE.
C
      DIMENSION Z(IZDIM1,2)
C
      common/COMDP/XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,AXISR(2),PLOTX,
     1   PLOTY,PLTORG(2),CAMXYZ(3),MX,NY,FMX,FNY,CAMWKG(6),XORG(3),
     2   GX(3),FX(2),KSCALE,ZORG,CENTER(2),PQLMT,
     3   AMTX(3,3),FOCALL
      DIMENSION LIMIT(2),FLIM(2)
      EQUIVALENCE(U,CAMXYZ(1)),(V,CAMXYZ(2)),(W,CAMXYZ(3)),
     1   (MX,LIMIT(1)),(FMX,FLIM(1))
C
      LOGICAL LSOLID
      common /COMDP1/ LSOLID
      EQUIVALENCE (CX,CY), (DXI,DETA), (XIW,ETAW),
     1  (XIEND,ETAEND), (KDXI,KDETA), (KXIEND,KETEND),
     2  (DX,DY)
C
C  INITIAL P FUNCTION
C
5     IVIS = 0
      R = U-XI
      S = V-ETA
      T = W-ZETA
C
C  TEST IF WE CHECK ALONG X
C
      IF(ABS(R) .LT. 1.0) go to 20
C
C  CONSTANTS FOR Y(X),Z(X)
C
      CY = S/R
      CZ = T/R
      DXI = SIGN(1.0,R)
C
C  INITIAL POINT. TAKE AINT(XI) IF .NE. XI AND STEPS IN RIGHT DIRECTI
C
      XIW = AINT(XI)
      IF((XIW-XI)*DXI .LE. 0.0) XIW = XIW+DXI
C     SKIP IF OFF LIMITS (WE ARE ON EDGE OF PLOT REGION)
      IF((XIW-1.0)*(XIW-FMX) .GT. 0.0) go to 20
C     FINAL POINT. TAKE AINT(U) IF IT MOVES OPPOSITE DXI, else ROUND
      XIEND = AINT(U)
      IF((XIEND-U)*DXI .GE. 0.0) XIEND = XIEND-DXI
C     BUT DO NOT GO BEYOND EDGES
      XIEND = AMAX1(1.0,AMIN1(XIEND,FMX))
C
C              AFTER TESTING, RE-PRDER THESE STATEMENTS
      J = IFIX(XIW)
      KDXI = IFIX(DXI)
      KXIEND = IFIX(XIEND)
      XW = XIW-U
C
C  IF LIMITS CROSS, NO TEST
c
      IF((XIEND-XIW)*DXI .LE. 0.0) go to 20
C     GET Y(X)
3     YW = V + XW*CY
C     IF Y IS OFF LIMITS, DONE
      IF((YW-1.0)*(YW-FNY)) 21,25,20
C     ON EDGE EXACTLY, NO INTERPOLATION
25    K = IFIX(YW)
      IF(W + XW*CZ - Z(J,K)) 4,10,7
C     INDEX FOR LOWER Y OF INTERVAL
21    K = IFIX(YW)
      DY = YW-FLOAT(K)
C     TEST Z OF LINE - Z OF SURFACE. ACCEPT ZERO DIFFERENCE.
      IF((W + XW*CZ)-(Z(J,K) + DY*(Z(J,K+1)-Z(J,K)))) 4,10,7
C     NEGATIVE. OK IF IVIS NEG. OR ZERO, else REJECT
4     IF(IVIS) 10,6,40
C     IVIS WAS ZERO, SET NEG.
6     IVIS = -1
      go to 10
C     PLUS. OK IF IVIS + OR ZERO, else, REJECT
7     IF(IVIS) 40,8,10
C     SET PLUS
8     IVIS = 1
C     TEST IF DONE. ADVANCE IF NOT
10    IF(J .EQ. KXIEND) go to 20
      J = J+KDXI
      XW = XW+DXI
      go to 3
C
C     CHECK IF WE TEST IN Y DIRECTION
20    IF(ABS(S) .LT. 1.0) go to 45
C     CONSTANTS FOR X(Y),Z(Y)
      CX = R/S
      CZ = T/S
      DETA = SIGN(1.0,S)
      ETAW = AINT(ETA)
      IF((ETAW-ETA)*DETA .LE. 0.0) ETAW = ETAW+DETA
C     CHECK WHETHER ON LIMITS
      IF((ETAW-1.0)*(ETAW-FNY) .GT. 0.0) go to 45
      ETAEND = AINT(V)
      IF((ETAEND-V)*DETA .GE. 0.0) ETAEND = ETAEND-DETA
      ETAEND = AMAX1(1.0,AMIN1(FNY,ETAEND))
      K = IFIX(ETAW)
      KDETA = IFIX(DETA)
      YW = ETAW-V
      KETEND = IFIX(ETAEND)
C     IF LIMITS CROSS, NO TEST, BUT TEST SINGLE POINT IF WE HAVE ALREADY
C     TESTED X
      A = ETAEND-ETAW
      IF(A*DETA .LT. 0.0) go to 45
      IF(A .EQ. 0.0 .AND. IVIS .EQ. 0) go to 45
C     GET X(Y)
23    XW = U + YW*CX
C     IF X OFF LIMITS, DONE
      IF((XW-1.0)*(XW-FMX)) 44,46,45
46    J = IFIX(XW)
      IF(W + YW*CZ - Z(J,K)) 24,30,27
44    J = IFIX(XW)
      DX = XW-FLOAT(J)
      IF((W + YW*CZ) - (Z(J,K)+DX*(Z(J+1,K)-Z(J,K)))) 24,30,27
C     NEG., IVIS MUST BE NEG OR ZERO else REJCT
24    IF(IVIS) 30,26,40
C     SET IVIS NEG
26    IVIS = -1
      go to 30
C     POS, IVIS MUST BE ZERO OR + else REJECT
27    IF(IVIS) 40,28,30
28    IVIS = 1
C     TEST IF DONE, ADVANCE IF NOT.
30    IF(K .EQ. KETEND) go to 45
      K = K+KDETA
      YW = YW+DETA
      go to 23
C
C     REJECT THIS POINT, return ZERO.
40    IVIS = 0
      return
C
C     ACCEPT. return +/- 1
C     IF IVIS ZERO, CAMERA WAS RIGHT OVER XI, ETA.
45    IF(IVIS .EQ. 0) IVIS = ISIGN(1,IFIX(T))
      if(LSOLID .AND. (IVIS .EQ. -1)) go to 40
      return
      end
      subroutine LAXIS(ALOW,AHIGH,MAXTCK,BMIN,BMAX,BTICK)
c
c***********************************************************************
c
C     THIS ROUTINE FINDS A SUITABLE TICK FOR LOG AXES
C
      smlrel=r1mach(1)
C
      BLOW = ALOG10(AMAX1(SMLREL,AMIN1(AHIGH,ALOW)))
      BHIGH = ALOG10(AMAX1(ALOW,AHIGH,1E2*SMLREL))
      RANGE = BHIGH-BLOW
      if(RANGE .LE. 1E-2) RANGE = 1.0
      ISTRT = 1
      IMAX = 5
30    DO 50 I=ISTRT,IMAX,ISTRT
        NTCKS = INT(RANGE/I + 0.999)
        if(NTCKS .LE. MAXTCK) go to 60
50    continue
      ISTRT = 10
      IMAX = 80
      go to 30
60    BTICK = I
      BMIN = BTICK*AINT(BLOW/BTICK)
      BMAX = BTICK*AINT(BHIGH/BTICK)
      if((BMIN-BLOW)/RANGE .GT. 0.001) BMIN = BMIN - BTICK
      if((BHIGH-BMAX)/RANGE .GT. 0.001) BMAX = BMAX + BTICK
      return
      end
      subroutine LBTEXT (TEXT, LINE1, LINE2, DELTAY, XCORNR, YCORNR,
     >                   HITE)
c
c***********************************************************************
c
C
C ONE LINER:
C     Displays a left justified text block. (Level 2-3)
C
C PURPOSE:
C     LBTEXT composes a left justified text block and displays it at a specified
C     position.  Character height and line spacing are assumed constant within
C     the indicated lines of text.
C
C     LBTEXT was developed as part of the TXTLEG alternative to the original
C     legend utility (TXTBLK), and may be used in turn as an alternative to
C     BTEXTL.
C
C ARGUMENTS:
C      ARG       DIM     TYPE    I/O/S     DESCRIPTION
C     TEXT    (*) * (*)   C        I       Character array containing text
C                                          with trailing '$'s.  Elements
C                                          LINE1:LINE2 will be displayed.
C     LINE1       -       I        I       First line of text to display.
C     LINE2       -       I        I       Last line of text to display.
C     DELTAY      -       R        I       Space between lines in inches.
C     XCORNR      -       R        I       Horizontal position of lower left
C                                          hand corner of text block in inches.
C     YCORNR      -       R        I       Vertical position of lower left hand
C                                          corner of text block in inches.
C     HITE        -       R        I       Text character height in inches.
C
C METHOD:
C     LBTEXT works with a simple array of variable length character strings,
C     unlike BTEXTL's packed data structure.  Position units are converted
C     from plot inches to virtual units using the utility common block
C     variables.  DSMOVE and ZTEXT do the rest.
C
C HISTORY:
C     04/17/89    M.D. Wong      Initial design and coding
C
C AUTHOR:  Michael Wong, Sterling Software, Palo Alto, CA.
C
      character  TEXT (*) * (*)
      integer    LINE1, LINE2
      REAL       DELTAY, HITE, XCORNR, YCORNR
C
C     Global Variables
C     ----------------
C
      integer    KZLEVL, KZBEGN, KZOR
      REAL       ZZUNIT, ZZPAGX, ZZPAGY, ZZPAGR, UUPAGX, UUPAGY,
     >           KZPAGE, KZAUTO, ZZXOR, ZZYOR, UUXOR, UUYOR, XCM0, XCM1,
     >           YCM0, YCM1
      character  CZALFL * 1, CZALFN * 5, CZALFS * 5
C
      common /CLEVEL/ KZLEVL, KZBEGN
      common /CMXALF/ CZALFL (6), CZALFN (6), CZALFS
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX, ZZPAGY, ZZPAGR, UUPAGX, UUPAGY, KZPAGE,
     >                KZAUTO
      common /CPHYSR/ ZZXOR, ZZYOR, KZOR, UUXOR, UUYOR
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
C
C     Local Variables
C     ---------------
C
      integer    I, ITEXT(20)
      REAL       FACTOR, GAP, VXC, VY, VYC
      character*160 KTEXT
C
C     Execution
C     ---------
C
C     Error handling.
C
      if(KZLEVL .LT. 2) go to 900
C
C     Convert inches to graphical units.
C
      FACTOR = ZZUNIT * ZZPAGR
      VXC = XCORNR * FACTOR + ZZXOR + XCM0
      VYC = YCORNR * FACTOR + ZZYOR + YCM0
      GAP = (DELTAY + HITE) * FACTOR
C
C     Start with upper left hand corner of text block.
C
      VY  = VYC + (LINE2 - LINE1) * GAP
C
      DO 200 I = LINE1, LINE2
         call DSMOVE (VXC, VY)
C-----------------------------BEGIN GENERIC CODE------------------------
C         call ZTEXT (TEXT (I), 100, CZALFL, CZALFN, CZALFS)
C------------------------------END GENERIC CODE-------------------------
C
C----------------------------BEGIN UNICOS SPECIFIC CODE-----------------
          KTEXT = TEXT(I)
          READ (KTEXT,'(20A8)') ITEXT
          call ZTEXT (ITEXT, 100, CZALFL, CZALFN, CZALFS)
C------------------------------END UNICOS SPECIFIC CODE-----------------
C
C------------------------BEGIN VAX VMS SPECIFIC CODE--------------------
C        call ZTEXT (%REF(TEXT (I)), 100, CZALFL, CZALFN, CZALFS)
C-------------------------END VAX VMS SPECIFIC CODE---------------------
         VY = VY - GAP
  200 continue
C
      go to 999
C
  900 CALL ERRMES ('LBTEXT', 2, 3)
C
  999 return
      end
C
      FUNCTION LDIVDS(ANUMER,ADENOM)
      LOGICAL LDIVDS
      if(ANUMER/ADENOM .EQ. AINT(ANUMER/ADENOM)) go to 10
      LDIVDS = .FALSE.
      return
10    LDIVDS = .TRUE.
      return
      end
      subroutine LEGHDG(LEGNAM,ILGCHR)
C
C
C
C
C      CHANGE LEGEND TITLE
C               (LEVEL 1-3, P/S)
C
C      INPUT:   LEGNAM = TITLE TEXT
C               ILGCHR = NUMBER OF characterS IN LEGNAM
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
C
      integer LEGNAM(*)
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZLGTL=20
        call ZCOPYS(LEGNAM,ILGCHR,KZLGTI,KZLGTL)
        if(KZLGTL.LT.0)then
          KZLGTL=-KZLGTL
          call ERRMES('LEGHDG',20,4)
        endif
        if(KZLEVL.EQ.1)then
          UULGTZ=UUHITE
        else
          ZZLGTZ=ZZHITE
        endif
        ZZLGTR=ZZSRAT
        call ZLOALF(CZLGTC,CZLGTF,CZLGTS,CZALFL,CZALFN,CZALFS)
      else
        call ERRMES('LEGHDG',1,3)
      endif
      return
      end
      FUNCTION LENG(LSTR)
C
C
C
C        FIND LENGTH OF INPUT character STRING
C
C        INPUT:   LSTR = INPUT STRING
C
C        OUTPUT:  LENG = LENGTH OF STRING IN NUMBER OF characterS
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      DIMENSION LSTR(*)
C
      I=1
      J=1
      DO 100 K=1,100
        call SYBYT4('X',LSTR(I),J,NUM)
        if(NUM.EQ.0)then
          go to 200
        else
          J=J+1
          if(J.EQ.(NUMBYT+1))then
            J=1
            I=I+1
          endif
        endif
 100  continue
 200  continue
      LENG=(I-1)*NUMBYT+J-1
      return
      end
      subroutine LINDEF(TLENG,NMRKSP,RATRAY)
C
C
C
C      TO DEFINE A NEW LINE STYLE
C               LEVEL 1-3, P/S
C
C      INPUT:   TLENG  = OVERALL LENGTH IN INCHES
C               MNRKSP = TOTAL NUMBER OF MARKS AND SPACES
C                        (LIMITED TO MAXIMUM OF 12 IN DISSIM)
C               RATRAY = ARRAY CONTAINING RATIOS OF MARKS AND
C                        SPACES TO OVERALL LENGTH
C
      parameter (ZZIN=2.54)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      LOGICAL LINILT, LPOSND, LNEW
C
      DIMENSION RATRAY(*)
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZLNCN=KZLNCN+1
        if(KZLNCN.GT.15) KZLNCN=15
        SUM=0.0
        NVAL=MIN(12,ABS(NMRKSP))
        DO 100 I=1,NVAL
          SUM=SUM+RATRAY(I)
 100    continue
        if(KZLEVL.EQ.1)then
          FACTOR=TLENG*ZZIN
        else
          if(MOD(NMRKSP,2).EQ.1) SUM=SUM+RATRAY(2)
          FACTOR=TLENG*ZZIN*ZZPAGR/SUM
        endif
C
C                        DEFINE MARKS AND SPACES IN DIST()
C
        DO 200 I=1,NVAL
          DIST(I,KZLNCN)=RATRAY(I)*FACTOR
 200    continue
        if(MOD(NVAL,2).EQ.1)then
          NVAL=NVAL+1
          DIST(NVAL,KZLNCN)=DIST(2,KZLNCN)
        endif
C
C  ENFORCE MINIMUM ALLOWABLE SPACE
C
        DO 300 I=1,NVAL
          if(DIST(I,KZLNCN).LT.0.0254)then
            DIST(I,KZLNCN)=0.0254
          endif
 300    continue
        DIST(13,KZLNCN)=NVAL
C
C                        FOR PEN DOWN FIRST
C
        if(NMRKSP.LT.0)then
          TEMP=DIST(1,KZLNCN)
          DO 400 I=1,NVAL-1
            DIST(I,KZLNCN)=DIST(I+1,KZLNCN)
 400      continue
          DIST(NVAL,KZLNCN)=TEMP
        endif
C
C                        CHECK IF LINE STYLE EXISTS
C
        LNEW=.TRUE.
        if(KZLNCN.NE.5)then
          DO 600 J=5,KZLNCN-1
            DO 500 I=1,13
              LNEW=LNEW.AND.
     *             (ABS(DIST(I,KZLNCN)-DIST(I,J)).LT.0.00005)
 500        continue
            if(LNEW)then
              KZLNCN=J
              go to 700
            endif
 600      continue
        endif
 700    continue
        ILNTYP=KZLNCN+1
        LINILT=.TRUE.
      else
        call ERRMES('LINDEF',1,3)
      endif
      return
      end
      subroutine LINLAB(NUM,IEXP,STRNG,LRMTEX)
c
c***********************************************************************
c
      LOGICAL LRMTEX
      character*(*) STRNG
C
      character BZERO(4)
C
      SAVE BZERO
C
      data BZERO /'0', '.', '0', '0'/
C
      LRMTEX = .TRUE.
       NVAL = NUM
      if(IEXP .GE. -2 .AND. IEXP .LE. 2) LRMTEX = .FALSE.
      if(IEXP .GT. 0 .AND. (.NOT. LRMTEX)) NVAL = NVAL*10**IEXP
      call NUMSTR(NVAL,STRNG)
      if((NVAL .EQ. 0) .OR. LRMTEX .OR. (IEXP .GE. 0)) go to 800
C
C  NUMBER IS IN RANGE 10**-1 OR 10**-2, SO FORMAT PRETTY
C
      N = -IEXP
      L = NUMCHR(STRNG)
      IZBGN = 1
      NIN = 3
      if(N .EQ. L) NIN = 2
C
C  IF N<L THEN WE NEED ONLY INSERT A DECIMAL POINT
C
      if(N .GE. L) go to 40
       IZBGN = 2
       NIN = 1
40    continue
C
C  ALLOW ROOM FOR DECIMAL POINT AND ZERO(S) IF NECESSARY
C
      ISTART = 1
      IF(NUM .LT. 0) ISTART = ISTART + 1
      IBEGIN = ISTART + MAX0(0,L-N)
      DO 50 I = 0, MIN0(N,L)
       STRNG(ISTART+L+NIN-I:ISTART+L+NIN-I)=STRNG(ISTART+L-I:ISTART+L-I)
50    continue
C
C  INSERT LEADING ZEROS IF NECESSARY, OR JUST DECIMAL POINT
C
       DO 60 I=0,NIN-1
       STRNG(IBEGIN+I:IBEGIN+I) = BZERO(IZBGN+I)
60     continue
C
C  ALL DONE
C
800   return
      end
      subroutine LOGLAB(NUM,STRNG)
c
c***********************************************************************
c
      character STRNG(*), LLABS(6,8)
C
      SAVE LLABS
C
      data LLABS / '.','0','0','1',' ',' ',
     1             '.','0','1',' ',' ',' ',
     2             '.','1',' ',' ',' ',' ',
     3             '1',' ',' ',' ',' ',' ',
     4             '1','0',' ',' ',' ',' ',
     5             '1','0','0',' ',' ',' ',
     6             '1','0','0','0',' ',' ',
     7             '1','0','0','0','0',' '/
      if(NUM .GT. -4 .AND. NUM .LT. 5) go to 100
      STRNG(1) = '1'
      STRNG(2) = 'E'
      call NUMSTR(NUM,STRNG(3))
      return
  100 continue
      DO 10 J=1,6
        STRNG(J) = LLABS(J,4+NUM)
   10 continue
      return
      end
      subroutine LOGLOG(XORIG,XCYC,YORIG,YCYC)
c
c***********************************************************************
c
C      DEFINE A LOG-LOG COORDINATE SYSTEM
C               (LEVEL 2, RAISE TO LEVEL 3)
C
C      INPUT:   XORIG,YORIG = LOWER BOUNDS OF X AND Y
C                             VALUES RESPECTIVELY
C               XCYC,YCYC   = CYCLE LENGTH IN INCHES
C                             FOR X AND Y
C
      parameter (KZLOG=3)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CUNIT/  ZZUNIT
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      LOGICAL LOGX, LOGY
C
      if(KZLEVL.EQ.2)then
        LOGX=.FALSE.
        LOGY=.FALSE.
        XAXIS=ZZXAXS/ZZUNIT/ZZPAGR
        YAXIS=ZZYAXS/ZZUNIT/ZZPAGR
        XPOS=0.0
        YPOS=0.0
        IAXES=3
        KZXTYP=KZLOG
        KZYTYP=KZLOG
        call ZDRLOG(XORIG,XCYC,YORIG,YCYC,XAXIS,YAXIS,KZXLAB,KZXALN,
     *                    KZYLAB,KZYALN,XPOS,YPOS,IAXES)
        KZLEVL=3
      else
        call ERRMES('LOGLOG',2,0)
      endif
      return
      end
      subroutine LOOK(Z,II,JJ,M,IZ,NZ,IZDIM)
c
c***********************************************************************
c
C  LOOK looks for a contour starting at the point (II,JJ)
C  with the contour being oriented such that the point (II,JJ) is
C  greater than the current contouring level, and its neighbor
C  (specified by M) is less than the current contouring level.
C
      integer IZ(IZDIM,2)
      DIMENSION Z(NZ,2)
c
      save contr
c
      common /CONTR/ CLEVEL,IOLD,JOLD,IN,JN,
     1   NX,NY,XL,DX,YL,DY
      DIMENSION IDMODE(3,4)
C
      SAVE IDMODE
C
      data IDMODE/4,1,2,  1,2,3,  2,3,4,  3,4,1/
C
      IOLD=II
      JOLD=JJ
      MODE=M
      call NEWP(1,MODE)
C
C  LOOK FOR CONTOUR STARTING HERE.   THE "OLD" POINT IS ALWAYS THE
C  POSITIVE ONE, SO THE TEST IS EASY.
C
      if(Z(IOLD,JOLD) .GE. CLEVEL .AND. Z(IN,JN) .LT. CLEVEL) go to 20
      return
C
C  CHECK FOR CONTOUR PREVIOUSLY THRU HERE - "SEGMNT" returnS THE POINT
C  WE MARK WHEN EVER A CONTOUR PASSES THRU THE POINTS "(IOLD,JOLD)"
C  AND "(IN,JN)".   "SEGMNT" ALSO returnS THE MARK THAT SHOULD BE
C  PLACED GIVEN THE ORIENTATION OF THE CONTOUR.
C
20    CALL SEGMNT(ICI,ICJ,ISEG)
      ITEM = IZ(ICI,ICJ)
      IF(ISEG .EQ. 1)then
        ITEM = ITEM - ITEM/2*2
        IF(ITEM .EQ. 1) return
      else
        ITEM = ITEM/2*2
        IF(ITEM .EQ. 2) return
      endif
C
C  NEW CONTOUR.   TRACE IT TILL IT ENDS BY LOOPING BACK ON ITSELF, OR
C  RUNNING OFF THE GRID.
C
      call ZPNT(XX,YY,Z,NZ)
      call SCALE(XX,YY,VX,VY)
      call GSMOVE(VX,VY)
      IOLD=IN
      JOLD=JN
30     continue
       DO 50 N=2,4
        call NEWP(N,MODE)
        if(IN .LT. 1 .OR. IN .GT. NX) return
        if(JN .LT. 1 .OR. JN .GT. NY) return
        if(SIGN(1.0,Z(IOLD,JOLD)-CLEVEL) .NE.
     1    SIGN(1.0,Z(IN,JN)-CLEVEL)) go to 60
        IOLD=IN
        JOLD=JN
50      continue
C
C  IT IS IMPOSSIBLE TO JUST FALL THRU DUE TO THE ALGORITHM
C
       WRITE(*,*)'LOOK - Fatal error!'
       STOP
60     continue
C
C  FOUND THE NEXT INTERSECTION.   SEE IF IT HAS ALREADY BEEN MARKED.
C  IF SO, THEN WE ARE DONE, else, MARK IT, DRAW TO IT, AND continue ON.
C
       call SEGMNT(ICI,ICJ,ISEG)
      ITEM = IZ(ICI,ICJ)
      IF(ISEG .EQ. 1)then
        ITEM = ITEM - ITEM/2*2
        IF(ITEM .EQ. 1) return
      else
        ITEM = ITEM/2*2
        IF(ITEM .EQ. 2) return
      endif
      IF(ISEG .EQ. 1)then
        IZ(ICI,ICJ) = IZ(ICI,ICJ)/2*2 + ISEG
      else
        IZ(ICI,ICJ) = IZ(ICI,ICJ) - IZ(ICI,ICJ)/2*2 + ISEG
      endif
       call ZPNT(XX,YY,Z,NZ)
       call SCALE(XX,YY,VX,VY)
       call GSDRAW(VX,VY)
       MODE=IDMODE(N-1,MODE)
       go to 30
      end
      subroutine MAPIT(XLOW,XHIGH,YLOW,YHIGH,XLAB,YLAB,TITLE,IAXES)
c
c***********************************************************************
c
      character XLAB*(*), YLAB*(*), TITLE*(*)
      LOGICAL LOGXX, LOGYY, LOGT, LRMTEX, LSHORT, LRAGGD
      character NUMBR*14
      character NEQUIV(14)
      LOGICAL LOGX, LOGY
      real zlog(8)
c
      save clevel
      save pltclp
      save pltcom
      save pltprm
      save pltsiz
      save shortf
      save tminld
      save zlog
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      common /PLTCLP/ XMIN,XMAX,YMIN,YMAX
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI
C
      EQUIVALENCE(NUMBR,NEQUIV)
C
      data ZLOG /0.3010, 0.4771, 0.6021, 0.6990, 0.7782, 0.8451,
     1   0.9031, 0.9542 /
      data TMINLD /0.1/
      data SHORTF /2.0/
C
      DELMX=0
      YVLEN = YVINI
C
C  SET LOGX AND LOGY TO FALSE FOR OUR USAGE OF SCALE
C
      LOGX = .FALSE.
      LOGY = .FALSE.
C
C  SEE WHAT TYPE OF AXES ARE DESIRED
C
      LOGXX = KSYAND(IAXES,1) .NE. 0
      LOGYY = KSYAND(IAXES,2) .NE. 0
      LRAGGD = KSYAND(IAXES,256) .NE. 0
C
C  DO THE AXES SCALING
C
      NUMTK = MIN0(10,INT(XVLEN/((ILABSZ()+1.0)*CXSIZE)))
      if(LOGXX) go to 20
      LSHORT = KSYAND(IAXES,16) .NE. 0
      call AXIS(XLOW,XHIGH,NUMTK,LSHORT,LRAGGD,XMIN,XMAX,XTMIN,XTMAX,
     1   XTICK,IXPWR)
      go to 40
20    CALL LAXIS(XLOW,XHIGH,NUMTK,XMIN,XMAX,XTICK)
      XTMIN = XMIN
      XTMAX = XMAX
      IXPWR = 0
40    NUMTK = MIN0(10,INT(YVLEN/(3.0*CYSIZE)))
      if(LOGYY) go to 60
      LSHORT = KSYAND(IAXES,32) .NE. 0
      call AXIS(YLOW,YHIGH,NUMTK,LSHORT,LRAGGD,YMIN,YMAX,YTMIN,YTMAX,
     1   YTICK,IYPWR)
      go to 80
60    CALL LAXIS(YLOW,YHIGH,NUMTK,YMIN,YMAX,YTICK)
      YTMIN = YMIN
      YTMAX = YMAX
      IYPWR = 0
80    continue
C
C  SET UP SCALING FACTORS FOR SCALE
C
      UX0 = XMIN
      UDX = XMAX - XMIN
      UY0 = YMIN
      UDY = YMAX - YMIN
C
C  DRAW Y AXES
C
      call GSSETC(CYSIZE,0.0)
      LOGT = .FALSE.
      if(.NOT. LOGYY .OR. YTICK .NE. 1.0) go to 90
      call SCALE(XMIN,YMIN,VX,TEMP)
      call SCALE(XMIN,YMIN+1.0-ZLOG(8),VX,VY)
      if((VY-TEMP) .GE. TMINLD) LOGT = .TRUE.
90    continue
C
C  DRAW Y AXIS LINE
C
      TENEXP = 10.0**IYPWR
      X = XMIN
      TICKSP = AMAX1(0.0,TICKLN)
      if(KSYAND(IAXES,64) .NE. 0) YVLEN = YVLEN - TICKSP
      TCKSGN = -TICKLN
100   continue
      call SCALE(X,YMAX,VX,VY)
      call GSMOVE(VX,VY)
      call SCALE(X,YMIN,VX,VY)
      call GSDRAW(VX,VY)
C
C  DRAW AND LABEL Y AXIS TICKS
C
      Y = YTMIN
      N = INT((YTMAX-YTMIN)/YTICK + 1.1)
110   continue
      call SCALE(X,Y*TENEXP,VX,VY)
      call GSMOVE(VX,VY)
      call GSDRAW(VX+TCKSGN,VY)
      if(X .EQ. XMAX) go to 185
      if(KSYAND(IAXES,1024) .NE. 0) go to 183
C
C  PLACE THE APPROPIATE LABEL
C
      if(LOGYY) go to 160
      call LINLAB(INT(Y),IYPWR,NUMBR,LRMTEX)
      go to 180
  160 continue
      call LOGLAB(INT(Y),NUMBR)
180   DEL = GSLENS(NUMBR) + CXSIZE*0.25
      DELMX = AMAX1(DEL,DELMX)
      call GSMOVE(VX-TICKSP-DEL,VY-CYSIZE/2.0)
      call GSPSTR(NUMBR)
C
C  ADD GRID LINE AT TICK IF DESIRED
C
183   continue
      if(KSYAND(IAXES,8) .EQ. 0) go to 185
      call GSLTYP(3)
      call GSMOVE(VX,VY)
      call SCALE(XMAX,Y*TENEXP,VX,VY)
      call GSDRAW(VX,VY)
      call GSLTYP(1)
185   continue
C
C  DO EXTRA TICKING IF EXTRA TICKS WILL BE FAR ENOUGH APART
C
      if((.NOT. LOGT) .OR. (Y .EQ. YTMAX)) go to 200
      DO 190 J = 1, 8
        call SCALE(X,Y+ZLOG(J),VX,VY)
        call GSMOVE(VX,VY)
190     CALL GSDRAW(VX+TCKSGN/SHORTF,VY)
200   continue
      Y = Y + YTICK
      N = N-1
      if(N .GT. 0) go to 110
      if(X .EQ. XMAX) go to 300
C
C  IF LINEAR AXIS, PLACE REMOTE EXPONENT IF NEEDED
C
      if(LOGYY .OR. (.NOT. LRMTEX)) go to 260
      if(KSYAND(IAXES,1024) .NE. 0) go to 260
      call SCALE(XMIN,(YTMIN+YTICK/2.0)*TENEXP,VX,VY)
      NUMBR(1:1) = 'E'
      call NUMSTR(IYPWR,NEQUIV(2))
      call GSMOVE(VX-(0.5*CXSIZE+GSLENS(NUMBR)),VY-CYSIZE/2.0)
      call GSPSTR(NUMBR)
C
C  NOW PLACE Y LABEL
C
260   CALL SCALE(XMIN,(YMIN+YMAX)/2.0,VX,VY)
      call GSMOVE(VX-DELMX-TICKSP-CYSIZE,
     1   VY-GSLENS(YLAB)/2.0)
      call GSSETC(CYSIZE,90.0)
      call GSPSTR(YLAB)
      call GSSETC(CYSIZE,0.0)
      if(KSYAND(IAXES,128) .EQ. 0) go to 300
      X = XMAX
      TCKSGN = TICKLN
      go to 100
300   continue
C
C  DRAW X AXIS
C
      LOGT = .FALSE.
      if(.NOT. LOGXX .OR. XTICK .NE. 1.0) go to 310
      call SCALE(XMIN,YMIN,TEMP,VY)
      call SCALE(XMIN+1.0-ZLOG(8),YMIN,VX,VY)
      if((VX-TEMP) .GE. TMINLD) LOGT = .TRUE.
310   continue
C
C  DRAW X AXIS LINE
C
      Y = YMIN
      TCKSGN = -TICKLN
      TENEXP = 10.0**IXPWR
      TICKSP = AMAX1(0.5*CYSIZE,TICKLN)
320   continue
      call SCALE(XMIN,Y,VX,VY)
      call GSMOVE(VX,VY)
      call SCALE(XMAX,Y,VX,VY)
      call GSDRAW(VX,VY)
C
C  DRAW AND LABEL X AXIS TICKS
C
      X = XTMIN
      N = INT((XTMAX-XTMIN)/XTICK + 1.1)
400   continue
      call SCALE(X*TENEXP,Y,VX,VY)
      call GSMOVE(VX,VY)
      call GSDRAW(VX,VY+TCKSGN)
      if(Y .EQ. YMAX) go to 430
      if(KSYAND(IAXES,512) .NE. 0) go to 423
      if(LOGXX) go to 410
      call LINLAB(INT(X),IXPWR,NUMBR,LRMTEX)
      go to 420
410   CALL LOGLAB(INT(X),NUMBR)
420   CALL GSMOVE(VX-GSLENS(NUMBR)/2.0,VY-TICKSP-1.5*CYSIZE)
      call GSPSTR(NUMBR)
C
C  ADD GRID LINE AT TICK IF DESIRED
C
423   continue
      if(KSYAND(IAXES,4) .EQ. 0) go to 430
      call GSLTYP(3)
      call GSMOVE(VX,VY)
      call SCALE(X*TENEXP,YMAX,VX,VY)
      call GSDRAW(VX,VY)
      call GSLTYP(1)
430   continue
C
C  DO EXTRA TICKING IF EXTRA TICKS WILL BE FAR ENOUGH APART
C
      if((.NOT. LOGT) .OR. (X .EQ. XTMAX)) go to 490
      DO 450 J = 1, 8
        call SCALE(X+ZLOG(J),Y,VX,VY)
        call GSMOVE(VX,VY)
        call GSDRAW(VX,VY+TCKSGN/SHORTF)
450     continue
490   continue
      X = X + XTICK
      N = N-1
      if(N .GT. 0) go to 400
      if(Y .EQ. YMAX) go to 590
C
C  NOW PLACE REMOTE EXPONENT IF NEEDED ON LINEAR AXIS
C
      if(LOGXX .OR. (.NOT. LRMTEX)) go to 520
      if(KSYAND(IAXES,512) .NE. 0) go to 520
      call SCALE(XMIN,YMIN,VX,VY)
      NUMBR(1:1) = 'E'
      call NUMSTR(IXPWR,NEQUIV(2))
      call GSMOVE(VX+3*CXSIZE,VY-TICKSP-2.75*CYSIZE)
      call GSPSTR(NUMBR)
C
C  NOW PLACE X AXIS LABEL
C
520   CALL SCALE((XMIN+XMAX)/2.0,YMIN,VX,VY)
      call GSMOVE(VX-GSLENS(XLAB)/2.0,VY-TICKSP-4.0*CYSIZE)
      call GSPSTR(XLAB)
      if(KSYAND(IAXES,64) .EQ. 0) go to 590
      Y = YMAX
      TCKSGN = TICKLN
      go to 320
590   continue
C
C  PLACE TITLE
C
      call SCALE((XMIN+XMAX)/2.0,YMAX,VX,VY)
      TCKSGN = 0.0
      if(KSYAND(IAXES,64) .NE. 0) TCKSGN = TICKSP
      call GSMOVE(VX-GSLENS(TITLE)/2.0,VY+TCKSGN+CYSIZE)
      call GSPSTR(TITLE)
C
C  MAKE SURE "PLTCLP" CONTAINS LIMITS PICKED BY MAPIT.   ONLY
c  MAINTAINED FOR CALLERS INFO.
C
      IF(LOGXX)THEN
        XMIN = 10.0**XMIN
        XMAX = 10.0**XMAX
        LOGX = .TRUE.
      endif
c
      IF(LOGYY)THEN
        YMIN = 10.0**YMIN
        YMAX = 10.0**YMAX
        LOGY = .TRUE.
      endif
c
c  JVB
c
      KZLEVL=3
c
      return
      end
      subroutine MAPPRM(XLEFT,XRIGHT,YBOT,YTOP,CSIZE,TKLN,LRAXIS)
c
      LOGICAL LRAXIS
c
      save gcdprm
      save pltprm
      save pltsiz
c
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
C
      XBORDR = 0.25/XS
      YBORDR = 0.25/YS
      call GSSETC(CSIZE,0.0)
      CXSIZE = GSLENS('0')
      CYSIZE = CSIZE
      TICKLN = TKLN
      TICKSP = AMAX1(0.0,TICKLN)
      TLABLN = ILABSZ()+0.25
      XVSTRT = XLEFT + TICKSP + TLABLN*CXSIZE + 2.0*CYSIZE + XBORDR
      XVLEN = XRIGHT - XVSTRT - (TLABLN/2.0)*CXSIZE - XBORDR
      if(LRAXIS) XVLEN = XVLEN - (TICKSP + TLABLN*CXSIZE + 2.0*CYSIZE)
      TICKSP = AMAX1(0.5*CYSIZE,TICKLN)
      YVSTRT = YBOT + TICKSP + 4.25*CYSIZE + YBORDR
      YVINI = YTOP - YVSTRT - 2.0*CYSIZE - YBORDR
      return
      end
      subroutine MAPSET(XLEFT,XRIGHT,YBOT,YTOP,CSIZE,TKLN,LRAXIS)
c
c***********************************************************************
c
      LOGICAL LRAXIS
c
      save gcdchr
C
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
C
      call MAPPRM(XLEFT*XLENCM/100.0,XRIGHT*XLENCM/100.0,
     1   YBOT*YLENCM/100.0,YTOP*YLENCM/100.0,CSIZE,TKLN,LRAXIS)

      return
      end
      subroutine MAPSIZ(XLPCT,XRPCT,YBPCT,YTPCT,CHRSIZ)
c
c***********************************************************************
c
c  MAPSIZ defines the range of the user coordinate system that will
c  be used in subsequent calls to MAPIT.
c
c  XLPCT,
c  XRPCT  Input, REAL XLPCT, XRPCT, are the locations of the
c         minimum and maximum X coordinates, expressed as percentages
c         of the available plotting area.
c
c  YBPCT,
c  YTPCT  Input, REAL YBPCT, YTPCT, are the locations of the
c         minimum and maximum Y coordinates, expressed as percentages
c         of the available plotting area.
c
c  CHRSIZ Input, REAL CHRSIZ, is the size of characters to use.
c         CHRSIZ may be given as 0, in which case MAPSIZ will choose
c         an appropriate value, via a call to GOODCS.
c
      save gcdprm
c
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
C
      XLEFT = VXL + (VXH-VXL)*XLPCT/100.0
      XRIGHT = VXL + (VXH-VXL)*XRPCT/100.0
      YBOT = VYL + (VYH-VYL)*YBPCT/100.0
      YTOP = VYL + (VYH-VYL)*YTPCT/100.0
      CSIZE = CHRSIZ
      if(CSIZE .EQ. 0.0)
     1   CSIZE = GOODCS(AMAX1(0.3,AMIN1(YTOP-YBOT,XRIGHT-XLEFT)/80.0))
      call MAPPRM(XLEFT,XRIGHT,YBOT,YTOP,CSIZE,0.9*CSIZE,.FALSE.)
      return
      end
      subroutine MAPSML(XLOW,XHIGH,YLOW,YHIGH,XLAB,YLAB,TITLE,IAXES)
c
c***********************************************************************
c
C     Cut down version of MAPIT for those users who only need MAPIT to do
C     simple things.
C
C     The following options have been commented out:
C
C     OPTION  COMMENT CHARS  ADDED LINE CMNT CHARS
C     ------  -------------  ---------------------
C     GRID LINES  CC   @!
C     LOG AXES  CCC   @!!
C     BOXED PLOT  CCCC   @!!!
C
      character XLAB*2, YLAB*2, TITLE*2
      character NUMBR*14
      character NEQUIV(14)
      LOGICAL LOGX, LOGY
      LOGICAL LRMTEX, LSHORT, LRAGGD
c
      save pltclp
      save pltcom
      save pltprm
      save pltsiz
c
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      common /PLTCLP/ XMIN,XMAX,YMIN,YMAX
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI
C
      EQUIVALENCE(NUMBR,NEQUIV)
C
      YVLEN = YVINI
C
C  SET LOGX AND LOGY TO FALSE FOR OUR USAGE OF SCALE
C
      LOGX = .FALSE.
      LOGY = .FALSE.
C
C  SEE WHAT TYPE OF AXES ARE DESIRED
C
      LRAGGD = KSYAND(IAXES,256) .NE. 0
C
C  DO THE AXES SCALING
C
      NUMTK = MIN0(10,INT(XVLEN/((ILABSZ()+1.0)*CXSIZE)))
      LSHORT = KSYAND(IAXES,16) .NE. 0
      call AXIS(XLOW,XHIGH,NUMTK,LSHORT,LRAGGD,XMIN,XMAX,XTMIN,XTMAX,
     1   XTICK,IXPWR)
      NUMTK = MIN0(10,INT(YVLEN/(3.0*CYSIZE)))
      LSHORT = KSYAND(IAXES,32) .NE. 0
      call AXIS(YLOW,YHIGH,NUMTK,LSHORT,LRAGGD,YMIN,YMAX,YTMIN,YTMAX,
     1   YTICK,IYPWR)
C
C  SET UP TEMPORARY SCALING FACTORS
C
      UX0 = XMIN
      UDX = XMAX - XMIN
      UY0 = YMIN
      UDY = YMAX - YMIN
C
C  DRAW Y AXES
C
      call GSSETC(CYSIZE,0.0)
C
C  DRAW Y AXIS LINE
C
      MXLAB = 3
      TENEXP = 10.0**IYPWR
      X = XMIN
      TICKSP = AMAX1(0.0,TICKLN)
      TCKSGN = -TICKLN
100   continue
      call SCALE(X,YMAX,VX,VY)
      call GSMOVE(VX,VY)
      call SCALE(X,YMIN,VX,VY)
      call GSDRAW(VX,VY)
C
C  DRAW AND LABEL Y AXIS TICKS
C
      Y = YTMIN
      N =INT( (YTMAX-YTMIN)/YTICK + 1.1 )
110   continue
      call SCALE(X,Y*TENEXP,VX,VY)
      call GSMOVE(VX,VY)
      call GSDRAW(VX+TCKSGN,VY)
      if(KSYAND(IAXES,1024) .NE. 0) go to 183
C
C  PLACE THE APPROPIATE LABEL
C
      call LINLAB(INT(Y),IYPWR,NUMBR,LRMTEX)
180   LN = NUMCHR(NUMBR)
      MXLAB = MAX0(MXLAB,LN)
      call GSMOVE(VX-TICKSP-CXSIZE*(LN+0.25),VY-CYSIZE/2.0)
      call GSPSTR(NUMBR)
C
C  ADD GRID LINE AT TICK IF DESIRED
C
183   continue
185   continue
C
C  DO EXTRA TICKING IF EXTRA TICKS WILL BE FAR ENOUGH APART
C
200   continue
      Y = Y + YTICK
      N = N-1
      if(N .GT. 0) go to 110
C
C  IF LINEAR AXIS, PLACE REMOTE EXPONENT IF NEEDED
C
      if(.NOT. LRMTEX) go to 260
      if(KSYAND(IAXES,1024) .NE. 0) go to 260
      call SCALE(XMIN,(YTMIN+YTICK/2.0)*TENEXP,VX,VY)
      call ZSCOPY('E',NUMBR)
      call NUMSTR(IYPWR,NEQUIV(2))
      call GSMOVE(VX-CXSIZE*(NUMCHR(NUMBR)+0.5),VY-CYSIZE/2.0)
      call GSPSTR(NUMBR)
C
C  NOW PLACE Y LABEL
C
260   CALL SCALE(XMIN,(YMIN+YMAX)/2.0,VX,VY)
      call GSMOVE(VX-(MXLAB+0.25)*CXSIZE-TICKSP-CYSIZE,
     1   VY-CXSIZE*NUMCHR(YLAB)/2.0)
      call GSSETC(CYSIZE,90.0)
      call GSPSTR(YLAB)
      call GSSETC(CYSIZE,0.0)
300   continue
C
C  DRAW X AXIS 
C
C  DRAW X AXIS LINE
C
      Y = YMIN
      TCKSGN = -TICKLN
      TENEXP = 10.0**IXPWR
      TICKSP = AMAX1(0.5*CYSIZE,TICKLN)
320   continue
      call SCALE(XMIN,Y,VX,VY)
      call GSMOVE(VX,VY)
      call SCALE(XMAX,Y,VX,VY)
      call GSDRAW(VX,VY)
C
C  DRAW AND LABEL X AXIS TICKS
C
      X = XTMIN
      N = INT((XTMAX-XTMIN)/XTICK + 1.1)
400   continue
      call SCALE(X*TENEXP,Y,VX,VY)
      call GSMOVE(VX,VY)
      call GSDRAW(VX,VY+TCKSGN)
      if(KSYAND(IAXES,512) .NE. 0) go to 423
      call LINLAB(INT(X),IXPWR,NUMBR,LRMTEX)
420   CALL GSMOVE(VX-CXSIZE*NUMCHR(NUMBR)/2.0,VY-TICKSP-1.5*CYSIZE)
      call GSPSTR(NUMBR)
C
C  ADD GRID LINE AT TICK IF DESIRED
C
423   continue
C
C  DO EXTRA TICKING IF EXTRA TICKS WILL BE FAR ENOUGH APART
C
      X = X + XTICK
      N = N-1
      if(N .GT. 0) go to 400
C
C     NOW PLACE REMOTE EXPONENT IF NEEDED ON LINEAR AXIS
C
      if(.NOT. LRMTEX) go to 520
      if(KSYAND(IAXES,512) .NE. 0) go to 520
      call SCALE(XMIN,YMIN,VX,VY)
      call ZSCOPY('E',NUMBR)
      call NUMSTR(IXPWR,NEQUIV(2))
      call GSMOVE(VX+3*CXSIZE,VY-TICKSP-2.75*CYSIZE)
      call GSPSTR(NUMBR)
C
C     NOW PLACE X AXIS LABLE
C
520   CALL SCALE((XMIN+XMAX)/2.0,YMIN,VX,VY)
      call GSMOVE(VX-CXSIZE*NUMCHR(XLAB)/2.0,VY-TICKSP-4.0*CYSIZE)
      call GSPSTR(XLAB)
C
C     ********** PLACE TITLE **********
C
      call SCALE((XMIN+XMAX)/2.0,YMAX,VX,VY)
      TCKSGN = 0.0
      call GSMOVE(VX-CXSIZE*NUMCHR(TITLE)/2.0,VY+TCKSGN+CYSIZE)
      call GSPSTR(TITLE)
C
C     MAKE SURE "PLTCLP" CONTAINS LIMITS PICKED BY MAPIT.   ONLY MAINTAINED
C     FOR CALLERS INFO.
C
C
C  TELL SCALE ABOUT LOG AXIS SCALING NOW
C
      return
      end
      subroutine MAPSZ2(XLPCT,XRPCT,YBPCT,YTPCT,CHRSIZ)
c
c***********************************************************************
c
      save gcdprm
c
      common /GCDPRM/ XS, YS, XT, YT, RCOS, RSIN, VXL, VXH, VYL, VYH
C
      XLEFT = VXL + (VXH-VXL)*XLPCT/100.0
      XRIGHT = VXL + (VXH-VXL)*XRPCT/100.0
      YBOT = VYL + (VYH-VYL)*YBPCT/100.0
      YTOP = VYL + (VYH-VYL)*YTPCT/100.0
      CSIZE = CHRSIZ
      if(CSIZE .EQ. 0.0)
     1   CSIZE = GOODCS(AMAX1(0.3,AMIN1(YTOP-YBOT,XRIGHT-XLEFT)/80.0))
      call MAPPRM(XLEFT,XRIGHT,YBOT,YTOP,CSIZE,0.9*CSIZE,.TRUE.)
      return
      end
      subroutine MARGIN(GRACEM)
c
c***********************************************************************
c
C      SET GRACE MARGIN
C               (LEVEL 1-3, P/S)
C
C      INPUT:   GRACEM = GRACE MARGIN
C
      save carea
      save clevel
      save cpage
      save cunit
c
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CUNIT/  ZZUNIT
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
c
      if(KZLEVL.EQ.1)then
        UUGRCE=GRACEM*ZZUNIT
      elseif(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
        UUGRCE=GRACEM*ZZUNIT
        ZZGRCE=GRACEM*ZZUNIT*ZZPAGR
      else
        call ERRMES('MARGIN',1,3)
      endif
      return
      end
      subroutine MARKER(ISYM)
c
c***********************************************************************
c
C      DEFINE MARKER TYPE
C               LEVEL 1-3, P/S
C
C      INPUT:   ISYM = MARKER ID, FROM 0 TO 18
C
      save clevel
      save csymbo
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CSYMBO/ KZSYM,KZNSYM,ZZSMSZ,UUSMSZ
c
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZSYM=ISYM
      else
        call ERRMES('MARKER',1,3)
      endif
      return
      end
      FUNCTION MAXLIN(IP,IPLEN,LLEN)
C
C
C
C
C     SET LINE LENGTH OF PACKED ARRAY OF characterS
C              (LEVEL 1-3)
C
C     INPUT:   IP    = character ARRAY
C              IPLEN = DIMENSION OF IP
C              LLEN  = NUMBER OF characterS IN LONGEST LINES
C
C     OUTPUT:  MAXLIN = MAXIMUM NUMBER OF LINES THAT CAN BE
C                       PACKED IN IP
C
      integer kzyes
      parameter (KZYES=111)
c
      integer IP(*)
c
      save cdevic
      save clevel
c
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CLEVEL/ KZLEVL,KZBEGN
C
      SAVE MAXIP,MAXLEN
c
      data MAXIP /20000/
      data MAXLEN /300/
c
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        IP(1)=KZYES
C
C                        ILLEGAL LINE LENGTH
C
        if(LLEN.LT.0.OR.LLEN.GT.MAXLEN)then
          IP(2)=KZYES
          MAXLIN=0
          call ERRMES('MAXLIN',LLEN,4)
          return
        endif
        NWORD=(LLEN-1)/KZBYTE+5
C
C                        ARRAY TOO SMALL OR TOO LARGE
C
        if(NWORD.GT.IPLEN.OR.IPLEN.GT.MAXIP)then
          IP(2)=KZYES
          MAXLIN=0
          call ERRMES('MAXLIN',IPLEN,5)
          return
        endif
        IP(2)=IPLEN*1000+LLEN
        MAXLIN=(IPLEN-2)/NWORD
      else
        MAXLIN=0
        call ERRMES('MAXLIN',1,3)
      endif
      return
      end
      subroutine MESSAG(LMESS,IMESS,XPOS,YPOS)
C
C
C
C
C      WRITE character STRING ON SCREEN
C               (LEVEL 2-3)
C
C      INPUT:   LMESS     = character STRING
C               IMESS     = NUMBER OF character IN LMESS
C               XPOS,YPOS = DISTANCE FROM PHYSICAL ORIGIN
C                           IN INCHES FOR X AND Y
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /GCVPOS/ XVPOS, YVPOS
C
      DIMENSION LMESS(*)
C:  CHANGED TO ACCOMMADATE AN 8 BYTE WORD
C     character CZALFL*1,CZALFN*5,CZALFS*5,KHAR*4
C     EQUIVALENCE (TEMP,KHAR)
      character CZALFL*1,CZALFN*5,CZALFS*5,KHAR*(NUMBYT)
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
C
C                        CALCULATE VIRTUAL COORDINATES
C
        TEMP=XPOS
C:  CHANGED TO ELIMINATE THE EQUIVALENCING PROBLEM
C       if(KHAR.EQ.'ABUT')then
        call WIN2CH(TEMP,KHAR)
        if(KHAR(1:4).EQ.'ABUT')then
          VX=ZZABUX
        else
          VX=XCM0+ZZXOR+XPOS*ZZUNIT*ZZPAGR
        endif
        TEMP=YPOS
C:  CHANGED TO ELIMINATE THE EQUIVALENCING PROBLEM
C       if(KHAR.EQ.'ABUT')then
        call WIN2CH(TEMP,KHAR)
        if(KHAR(1:4).EQ.'ABUT')then
          VY=ZZABUY
        else
          VY=YCM0+ZZYOR+YPOS*ZZUNIT*ZZPAGR
        endif
C
C                        DRAW TEXT
C
        call DSMOVE(VX,VY)
        call ZTEXT(LMESS,IMESS,CZALFL,CZALFN,CZALFS)
        ZZABUX=XVPOS
        ZZABUY=YVPOS
C
C                        WRONG LEVEL
C
      else
        call ERRMES('MESSAG',3,0)
      endif
      return
      end
      subroutine minmax(array,npts,bmin,bmax)
c
c***********************************************************************
c
C     FIND MINIMUM AND MAXIMUM OF THE ARRAY
C
      integer npts
      real array(npts)
c
      BMIN = ARRAY(1)
      BMAX = BMIN
      DO 10 I=2,NPTS
        BMIN = MIN(BMIN,ARRAY(I))
        BMAX = MAX(BMAX,ARRAY(I))
  10  continue
      return
      end
      subroutine MIXALF(LALPHA)
C
C
C
C
C     SET MIX ALPHABET SET
C              (LEVEL 1-3, P/S)
C
C     INPUT:   LALPHA = character SET
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      character*(*) LALPHA
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFL(2)='('
        CZALFN(2)=LALPHA(1:5)
        call CAPCHR(CZALFN(2))
        if(CZALFL(1).EQ.' ') CALL DEFALF('STAND')
      else
        call ERRMES('MIXALF',1,3)
      endif
      return
      end
      subroutine MOVORI(XOREL,YOREL)
c
c***********************************************************************
c
C      DEFINE PHYSICAL ORIGIN BY OFFSET FROM
C               CURRENTLY DEFINED PHYSICAL ORIGIN
C               LEVEL 1, P/S
C
C      INPUT:   XOREL,YOREL = NEW POSITION FROM CURRENT
C                             PHYSICAL ORIGIN IN INCHES
C
      parameter (KZYES=111)
c
      save clevel
      save cphysr
      save cunit
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CUNIT/  ZZUNIT
C
      UUXOR=UUXOR+XOREL*ZZUNIT
      UUYOR=UUYOR+YOREL*ZZUNIT
      KZOR=KZYES
C
C                        WRONG LEVEL
C
      if(KZLEVL.NE.1)then
        call ERRMES('MOVORI',1,0)
      endif
      return
      end
      subroutine SIZMRK(FAC)
C
C
C
C      SET SYMBOL SIZE
C               LEVEL 1-3, P/S
C
C      INPUT:   FAC = MULTIPLE OF BASIC SIZE, WHICH IS 0.08 INCH
C
      parameter (ZZIN=2.54)
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CSYMBO/ KZSYM,KZNSYM,ZZSMSZ,UUSMSZ
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        UUSMSZ=FAC*0.08*ZZIN
        if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
          ZZSMSZ=FAC*0.08*ZZIN*ZZPAGR
        endif
      else
        call ERRMES('SIZMRK',1,3)
      endif
      return
      end
      FUNCTION NCHRAY(DUMMY)
C
C
C
C
C
C           returnS THE NUMBER OF characterS IN A
C                  character*1 ARRAY
C
C      INPUT:      DUMMY      INPUT ARRAY
C
      character*1 DUMMY(*)
      DO 100 I=1,160
        II=I
        IF(ICHAR(DUMMY(II)) .EQ. 0) go to 200
 100  continue
      NCHRAY=0
      return
 200  continue
      NCHRAY=II-1
      return
      end
C
C
C
      subroutine NEWP(I,M)
      common /CONTR/ CLEVEL,IOLD,JOLD,IN,JN,
     1   NX,NY,XL,DX,YL,DY
      DIMENSION IDELI(4),JDELJ(4)
C
      SAVE IDELI,JDELJ
      data IDELI,JDELJ / 0,1,0,-1,   1,0,-1,0/
      INDEX=MOD(2+I+M,4)+1
      IN=IOLD+IDELI(INDEX)
      JN=JOLD+JDELJ(INDEX)
      return
      end
      subroutine NOBORD
C
C
C
C      SET BORDER DRAWING FLAG
C               LEVEL 1, P/S
C
      parameter (KZNO=222)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CBORCH/ KZBRDR,KZCHEK
C
      if(KZLEVL.EQ.1)then
        KZBRDR=KZNO
      else
        call ERRMES('NOBORD',1,0)
      endif
      return
      end
      subroutine NOCHEK
C
C
C
C      SET OUT OF RANGE CHECKING FLAG
C               LEVEL 1-3, P/S
C
      parameter (KZNO=222)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CBORCH/ KZBRDR,KZCHEK
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZCHEK=KZNO
      else
        call ERRMES('NOCHEK',1,3)
      endif
      return
      end
      subroutine NOXLBL
C
C
C
C
C
C LEVEL 1-3, P/S
C
C      SUPPRESS X AXIS NUMBERS
C
      parameter (KZYES=111)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZXNON=KZYES
      else
        call ERRMES('NOXLBL',1,3)
      endif
      return
      end
      subroutine NOYLBL
C
C
C
C      SUPPRESS Y AYIS NUMBERS
C               LEVEL 1-3, P/S
C
      parameter (KZYES=111)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZYNON=KZYES
      else
        call ERRMES('NOYLBL',1,3)
      endif
      return
      end
      FUNCTION NUMCHR(DUMMY)
      character*(*) DUMMY
C
      NUM = LEN(DUMMY)
      DO 10 I=NUM,1,-1
        NUMCHR = I
        IF(DUMMY(I:I) .NE. ' ') return
   10 continue
      NUMCHR = 0
      return
      end
      subroutine NUMSTR(JVAL,BSTRNG)
      parameter (MAXC=15)
C
C     character BSTRNG(*)
      character*(*) BSTRNG
C
C     THIS ROUTINE CONVERTS "JVAL" TO A STRING WITH NO
C      LEADING SPACES AND TERMINATED BY A "NULL".
C
      character NUMBER*15
C
  100 FORMAT(I15)
      NC = 0
      WRITE(NUMBER,100) JVAL
C
C
C CLEAR THE INPUT STRING WITH BLANKS
C
      NCIN=LEN(BSTRNG)
      DO 1 I=1,NCIN
        BSTRNG(I:I) = ' '
    1 continue
      DO 10 I=1,MAXC
        IF(NUMBER(I:I) .EQ. ' ') go to 10
        NC=NC+1
        BSTRNG(NC:NC) = NUMBER(I:I)
   10 continue
      return
      end
      FUNCTION NZCHAR(LMESS,CFLAG)
C
C
C
C      returnS NUMBER OF characterS IN A SELF
C               TERMINATING STRING TAKING ACCOUNT OF
C               MIX ALPHABET
C
C      INPUT:   LMESS  = STRING
C               CFLAG  = MIX FONT FLAGS
C
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
C
      integer LMESS1(40),LMESS(*)
      character*1 CH(160),CFLAG(6)
      LOGICAL FINIS,FIRST
C
C                        MIX ALPHABET NOT ACTIVE, WRITE STRING
C
      if(CFLAG(1).EQ.' ')then
        call ZCOSTR(LMESS,100,LMESS1,IMESS1)
        NZCHAR=LENG(LMESS1)+KZTMLN
C
C                        PROCESS USING FLAGS SET BY MIX
C                        ALPHABET ROUTINES
C
      else
        call ZIN2CH(LMESS,CH)
        IPTR=1
        ICUR=1
        FIRST=.TRUE.
        DO 200 I=1,100
C
C                        LOOK FOR STRING TERMINATOR
C
          FINIS=.TRUE.
          DO 500 J=1,KZTMLN
            call SYBYT4('X',KZSTRM,J,KHAR)
            FINIS=FINIS.AND.(ICHAR(CH(I+J-1)).EQ.KHAR)
 500      continue
C
C                        IF FOUND, return
C
          if(IPTR.EQ.1.AND.FINIS)then
            NZCHAR=I+KZTMLN-1
            return
          endif
C
C                        LOOK FOR MIXALF FLAGS
C
 300      continue
          if(CH(I).EQ.CFLAG(ICUR).AND.CH(I).NE.' '.AND.
     *          (ICUR.NE.IPTR.OR.FIRST))then
            FIRST=.FALSE.
            IPTR=ICUR
            ICUR=ICUR+1
            if(ICUR.GT.6) ICUR=1
            go to 200
          else
            ICUR=ICUR+1
            if(ICUR.GT.6) ICUR=1
            if(ICUR.NE.IPTR) go to 300
C
C                        INCREMENT NZCHAR
C
          endif
 200    continue
C
C                        end WHILE
C
        NZCHAR=-5
      endif
      return
      end
      subroutine ORIGIN(X,Y)
c
c***********************************************************************
c
C      DEFINE PHYSICAL ORIGIN
C               LEVEL 1, P/S
C
C      INPUT:   X,Y = X AND Y VALUE FROM LOWER LEFT CORNER
C                     OF PAGE IN INCHES
C
      parameter (KZYES=111)
c
      save clevel
      save cphysr
      save cunit
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CUNIT/  ZZUNIT
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
C
      UUXOR=X*ZZUNIT
      UUYOR=Y*ZZUNIT
      KZOR=KZYES
      if(KZLEVL.NE.1)then
        call ERRMES('ORIGIN',1,0)
      endif
      return
      end
      subroutine page(x,y)
c
c***********************************************************************
c
c  PAGE defines the page size.
c
c  Level 1, P/S.
c
c  X,
c  Y     Input, REAL X, Y, the page size in inches.
c
      integer kzyes
      parameter (kzyes=111)
c
      save clevel
      save cpage
      save cunit
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CUNIT/  ZZUNIT
C
      UUPAGX=X*ZZUNIT
      UUPAGY=Y*ZZUNIT
      KZPAGE=KZYES

      IF(KZLEVL.NE.1)CALL ERRMES('PAGE',KZLEVL,4)

      return
      end
      subroutine PAKLIN(ISTR,IP,ISEQ)
C
C
C
C
C     INSERT A LINE TO A PACKED ARRAY
C              (LEVEL 1-3)
C
C     INPUT:   ISTR = character STRING TO BE PACKED TO ARRAY
C              IP   = character ARRAY
C              ISEQ = SEQUENCE OF ISTR IN IP
C
C     OUTPUT:  IP = MODIFIED character ARRAY
C
C  ---------------------------------------------------------
C  | P | P |   TEXT ...       |      | HITE | YRAT |  AVE  |
C  ---------------------------------------------------------
C
C          -------------------------------------------------
C          |   TEXT ...       |      | HITE | YRAT | SYMSP |
C          -------------------------------------------------
C
C                             ...
C                             ...
C                             ...
C
C          -------------------------------------------------
C          |   TEXT ...       |      | HITE | YRAT |       |
C          -------------------------------------------------
C          |<--------            NWORD            -------->|
C
      parameter (KZYES=111)
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
C
      DIMENSION ISTR(*),IP(*)
      character CFLAG(6)*1
      character*5 CFONT(6),CSTYLE
      EQUIVALENCE (IYRAT,YRAT),(HITE,IHITE)
C
      SAVE JTEXT,JHITE,JYRAT
      data JTEXT,JHITE,JYRAT/1,-2,-1/
      HITE=ZZHITE
      YRAT=ZZSRAT
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
C
C                        ARRAY NOT BIG ENOUGH
C
        if(IP(2).EQ.KZYES)then
          call ERRMES('PAKLIN',0,4)
          return
        endif
C
C                        CALCULATE LINE LENGTH
C
        if(IP(1).EQ.KZYES)then
          IPLEN=IP(2)/1000
          LLEN=IP(2)-IPLEN*1000
        else
          LLEN=40
        endif
C
C                        COPY TO ARRAY THE CURRENT SPACING parameter
C                        character HEIGHT, CHARACTER FONT AND THE
C                        character STRING
C
        NWORD=(LLEN-1)/KZBYTE+5
        ICUR=(ISEQ-1)*NWORD+2
        call ZCOPYS(ISTR,100,IP(ICUR+JTEXT),LLEN)
        if(LLEN.LT.0) CALL ERRMES('PAKLIN',ISEQ,5)
        ICUR1=ICUR+NWORD
        IP(ICUR1+JHITE)=IHITE
        IP(ICUR1+JYRAT)=IYRAT
        call ZLOALF(CFLAG,CFONT,CSTYLE,CZALFL,CZALFN,CZALFS)
        DO 100 I=1,6
          CZLGAC(ISEQ,I)=CFLAG(I)
          CZLGAF(ISEQ,I)=CFONT(I)
 100    continue
        CZLGAS(ISEQ)=CSTYLE
      else
        call ERRMES('PAKLIN',1,3)
      endif
      return
      end
      subroutine PLEGND (TEXT, LINE1, LINE2, DY, XCORNR, YCORNR, LEGINF,
     >                   HITE, SEG, GAP, BOX)
C
C ONE-LINER:  Portable plot legend utility (Level 3)
C
C
C PURPOSE:
C
C        PLEGND composes a plot legend and displays it at a specified position.
C     If the number of legend entries is large and the texts are suitably short,
C     PLEGND may be called more than once to position different sections of the
C     legend alongside each other.  (Such positioning is up to the calling
C     program.)  Character height and line spacing are assumed constant for
C     each call to PLEGND.  An optional box may be drawn around the legend.
C
C        PLEGND was developed 
C      for the following reasons:
C
C     (1) rather than underlining the text with the line pattern, the following
C         form is provided:           ---o---    Description
C     (2) character-type text is expected instead of an awkward packed-integer
C         array data structure;
C     (3) any implicit connection between curve-drawing and updating the legend
C         is avoided, since this has required work-arounds in the past.
C
C        WARNING:  If the legend is to be drawn outside the plotting area, a
C     suitable grace margin should be defined before the call to PLEGND.
C
C ARGUMENTS:
C
C     ARG        DIM     TYPE  I/O/S  DESCRIPTION
C     TEXT    (*) * (*)   C      I    Character array containing legend
C                                     text with trailing '$'s.  Elements
C                                     LINE1:LINE2 will be displayed in the
C                                     current color (as opposed to the color
C                                     associated with each line/symbol).
C     LINE1       -       I      I    First line of legend text to display.
C     LINE2       -       I      I    Last line of legend text to display.
C     DY          -       R      I    Space between lines in inches.
C     XCORNR      -       R      I    Horizontal position of lower left hand
C                                     corner of legend in inches.
C     YCORNR      -       R      I    Vertical position of lower left hand
C                                     corner of legend in inches.
C     LEGINF    (3, *)    I      I    Integer array containing codes for
C                                     (1) line type, (2) symbol, and (3) color.
C                                     See POLYLIN for the code definitions.
C     HITE        -       R      I    Text character height in inches.
C     SEG         -       R      I    Length of legend line segment in inches.
C     GAP         -       R      I    Length of gap between legend line segment
C                                     and text in inches.
C     BOX         -       L      I    Draws box around legend if .TRUE.
C
C METHOD:
C        The symbol/line drawing is handled separately from the text writing.
C     The latter task was modularized when it was found to be a potentially
C     reusable function (subroutine LBTEXT) while the former is done with the
C     standard curve drawing routine, POLYLIN, after appropriate conversion
C     of units from inches to data units via XINVRS and YINVRS.
C
C ERROR HANDLING:  There is no check for calling PLEGND at the wrong level.
C
C PROCEDURES:
C     LBTEXT    Writes left justified block of text
C     XDIMTB    Finds length of imaginary rectangle around a text block
C     XINVRS    Converts location of a point from inches to data units
C     YINVRS       "              "      "          "         "
C     POLYLIN   Draws a curve using lines and/or symbols
C
C
C HISTORY:
C
C AUTHOR:  Michael Wong, Sterling Software, Palo Alto, CA.
C
C-----------------------------------------------------------------------
C
      character  TEXT (*) * (*)
      integer    LEGINF (3, *), LINE1, LINE2
      REAL       DY, HITE, XCORNR, YCORNR
      LOGICAL    BOX
      REAL       HALF
      parameter (HALF = 0.5)
      integer    COLOR, I, LINE, SYMBOL
      REAL       GAP, MAR, SEG, X (5), XBLEN, Y (5), YBLEN, YPOS, IER
      REAL       XDIMTB, XINVRS, YINVRS
      EXTERNAL   XDIMTB, XINVRS, YINVRS
C
C     Display the legend text in current color.  Units are inches.
C
      call LBTEXT (TEXT, LINE1, LINE2, DY, XCORNR + SEG + GAP, YCORNR,
     >             HITE)
C
C     Draw a symbol and/or line segment for each legend entry.
C     POLYLIN applies if positions in inches are converted to data units.
C     Any grace margin needed for a legend outside the plot area is left
C     to the application program, since there is no way of restoring the
C     input setting here if it is temporarily adjusted.
C
C     Note that the polar plot case (not to mention general rotated coordinate
C     systems) forces all conversions from inches to stay inside the loop
C     (where some would be constant otherwise).
C
C     Start with the upper left corner.
C
      YPOS = YCORNR + (LINE2 - LINE1) * (HITE + DY) + HALF * HITE
C
      DO 200, I = LINE1, LINE2
C
         X (1)  = XINVRS (XCORNR, YPOS)
         Y (1)  = YINVRS (XCORNR, YPOS)
         X (2)  = XINVRS (XCORNR + SEG, YPOS)
         Y (2)  = YINVRS (XCORNR + SEG, YPOS)
         LINE   = LEGINF (1, I)
         SYMBOL = LEGINF (2, I)
         COLOR  = LEGINF (3, I)
C
         if(LINE .NE. 2)then
C
C           Draw the line segment.
C
            call POLYLIN (2, X, Y, LINE, -1, COLOR, IER)
C
         end IF
C
         if(SYMBOL .GT. -1)then
C
C  Draw the symbol in the middle of the segment.
C
            X (1) = XINVRS (XCORNR + HALF * SEG, YPOS)
            Y (1) = YINVRS (XCORNR + HALF * SEG, YPOS)
C
            call POLYLIN (1, X (1), Y (1), 2, SYMBOL, COLOR, IER)
C
         end IF
C
         YPOS = YPOS - (HITE + DY)
C
  200 continue
C
C
      if(BOX)then
C
C        Pick a reasonable box margin.
C
         MAR = .6 * GAP
C
C        Calculate the dimensions of the legend block (inches).
C
         XBLEN = SEG + GAP + XDIMTB (TEXT, LINE1, LINE2)
         YBLEN = (LINE2 - LINE1) * (HITE + DY) + HITE
C
C        Draw the box starting from the lower left corner.
C
         X (1) = XINVRS (XCORNR - MAR, YCORNR - MAR)
         Y (1) = YINVRS (XCORNR - MAR, YCORNR - MAR)
         X (2) = XINVRS (XCORNR + MAR + XBLEN, YCORNR - MAR)
         Y (2) = YINVRS (XCORNR + MAR + XBLEN, YCORNR - MAR)
         X (3) = XINVRS (XCORNR + MAR + XBLEN, YCORNR + MAR + YBLEN)
         Y (3) = YINVRS (XCORNR + MAR + XBLEN, YCORNR + MAR + YBLEN)
         X (4) = XINVRS (XCORNR - MAR, YCORNR + MAR + YBLEN)
         Y (4) = YINVRS (XCORNR - MAR, YCORNR + MAR + YBLEN)
         X (5) = X (1)
         Y (5) = Y (1)
C
         call POLYLIN (5, X, Y, 3, -1, 0, IER)
C
      end IF
C
      return
      end
      subroutine PLTBOX(XLEFT,XRIGHT,YBOT,YTOP)
C
C
      CSIZE = GOODCS(AMIN1(YTOP-YBOT,XRIGHT-XLEFT)/80.0)
      call MAPPRM(XLEFT,XRIGHT,YBOT,YTOP,CSIZE,0.9*CSIZE,.FALSE.)
      return
      end
      subroutine PLTBX2(XLEFT,XRIGHT,YBOT,YTOP)
C
C
      CSIZE = GOODCS(AMIN1(YTOP-YBOT,XRIGHT-XLEFT)/80.0)
      call MAPPRM(XLEFT,XRIGHT,YBOT,YTOP,CSIZE,0.9*CSIZE,.TRUE.)
      return
      end
      subroutine POINTC(X,Y,NPTS)
      DIMENSION X(NPTS), Y(NPTS)
C
C     THIS ROUTINE PLOTS THE VISIBLE POINTS ON THE SCREEN
C
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
      DIMENSION AREA(4)
C
      call GSSCLP(XVSTRT,XVSTRT+XVLEN,YVSTRT,YVSTRT+YVLEN,AREA)
      DO 100 I=1,NPTS
      call SCALE(X(I),Y(I),VX,VY)
      call GSMOVE(VX,VY)
      call SYMBOL(3,0.3)
100   continue
      call GSRCLP(AREA)
      return
      end
      subroutine POINTS(X,Y,NPTS)
C
C     THIS ROUTINE IS USED TO PLOT POINTS ON THE SCREEN
C
      DIMENSION X(NPTS),Y(NPTS)
      DO 100 I=1, NPTS
      call SCALE(X(I),Y(I),VX,VY)
      call GSMOVE(VX,VY)
100   CALL SYMBOL(3,0.3)
      return
      end
      subroutine POLY3
C
C
C
C      SET INTERPOLATION FLAG
C               LEVEL 1-3, P/S
C
      parameter (KZPPLY=5)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZLTYP=KZPPLY
      else
        call ERRMES('POLY3',1,3)
      endif
      return
      end
      subroutine POLYLIN(N, X, Y, LINE, SYMBOL, COLOR, IER)
C
C One-liner:  Connect-the-dots with given pattern, symbol, color (level 3)
C ----------
C
C Description and usage:
C ----------------------
C
C        POLYLIN is a high-level "curve" drawing routine with a generic
C     programmer interface.  It isolates at least some of the details of
C     the specific graphics library in use.  (Plot set-up functions still
C     have to be translated elsewhere when switching to another library.)
C
C        Note that any effect POLYLIN might have on the plot legend (via
C     its call to CURVE) is up to the application.  The original 
C     approach would initiate this by calling SAVLIN, MAXLIN, and PAKLIN prior
C     to any call to POLYLIN.  The alternative legend handling via PLEGND
C     eliminates any "hidden" connections between POLYLIN/CURVE and the legend,
C     so that awkward work-arounds for (say) certain curve fitting methods are
C     no longer needed.
C
C Arguments:
C ----------
C
C     Name    Dimension  Type  I/O/S  Description
C     N                   I    I      Number of points to be plotted.
C
C     X          N        R    I      Array of abscissas.
C
C     Y          N        R    I      Array of ordinates.
C
C     LINE       N        I    I      Line type code.
C                                        1 = connected symbols
C                                        2 = symbols alone
C                                        3 = solid line
C                                        4 = dots
C                                        5 = dashes
C                                        6 = chaindots
C                                        7 = chaindashes
C                                        8 = longdashes (user-defined type)
C                                        9 = unused
C                                       10 = unused
C                                       11 = unused
C                                       12 = unused
C                                       13 = thick solid line
C                                       14 = thick dots
C                                       15 = thick dashes
C                                       16 = thick chaindots
C                                       17 = thick chaindashes
C                                       18 = thick longdashes
C                                       19 = unused
C                                       20 = unused
C                                       21 = unused
C                                       22 = unused
C
C     SYMBOL     N        I    I      Symbol type code.  (It is not clear
C                                     that values > 18 wrap around ...)
C                                      < 0 - no symbol
C                                        0 - Square
C                                        1 - Octagon
C                                        2 - Triangle
C                                        3 - '+'
C                                        4 - 'X'
C                                        5 - Diamond
C                                        6 - Upside down triangle
C                                        7 - Square with an 'X' in it
C                                        8 - 'X' plus a horizontal line
C                                        9 - Diamond with a '+' in it
C                                       10 - Octagon with a '+' in it
C                                       11 - Double hour glass
C                                       12 - Square with a '+' in it
C                                       13 - Octagon with a 'X' in it
C                                       14 - Square with a triangle in it
C                                       15 - Pentagon with a '+' in it
C                                       16 - Pentagon
C                                       17 - Five pointed star
C                                       18 - Square with a diamond in it
C
C     COLOR               I    I      Color code:
C                                        0 = white
C                                        1 = black
C                                        2 = magenta
C                                        3 = red
C                                        4 = yellow
C                                        5 = green
C                                        6 = cyan
C                                        7 = blue
C
C     IER                 I      O    Error flag:
C                                        0 = no problems
C                                        1 = N is not positive
C                                        2 = no line and no symbol asked for
C
C
C Author:       Robert Kennelly, Sterling Software/NASA Ames
C -------
C
C History:
C --------
C-------------------------------------------------------------------------------
c
      integer COLOR, IER, LINE, N, SYMBOL
      REAL X (N), Y (N)
C
C     Local variables.
C
      integer TYPE
      REAL RATIO (2)
      character COLORS (0:7) * 7
c
      save colors,ratio
C
C     Storage.
C
      data COLORS
     >  /'WHITE',
     >   'BLACK',
     >   'MAGENTA',
     >   'RED',
     >   'YELLOW',
     >   'GREEN',
     >   'CYAN',
     >   'BLUE'/
      data RATIO
     >  /0.7, 0.3/
C
C     Execution.
C
      IER = 0
C
C     Simple-minded error checking.  These are not necessarily fatal, so
C     just return with error flag set.
C
      if(N .LE. 0)then
C
C        No plot data?
C
         IER = 1
      elseif(LINE .EQ. 2 .AND. SYMBOL .LT. 0)then
C
C        Invisible line!?
C
         IER = 2
      end IF
C
      if(IER .NE. 0) go to 999
C
C     Preset to "defaults" (in case of bad inputs?).  Note that 'WHITE'
C     actually shows up as black in hardcopy.  Resetting 'DOT' takes care
C     of all the patterns.
C
      call RESET ('CLRSET')
      call RESET ('CRVWID')
      call RESET ('DOT')
C
C     Set line pattern.
C     -----------------
C
C
      if(LINE .EQ. 8 .OR. LINE .EQ. 18)then
C        Define "longdash" type:
         call LINDEF (0.2, 2, RATIO)
      end IF
C
C     <Put further user definitions here.  Four more allowed.>
C
      if(LINE .LE. 12)then
         call DSLTYP (LINE - 2)
      else
         call CRVWID (0.02)
         call DSLTYP (LINE - 12)
      end IF
C
C     Set point marker type.
C     ----------------------
C
C     Note that MARKER will look for a custom symbol if called with a
C     negative argument, so we have to test first.
C
      if(SYMBOL .GE. 0)then
C
         call MARKER (SYMBOL)
C
C        Set CURVE's TYPE flag for symbols and/or lines.
C
         if(LINE .NE. 2)then
C
C           Line plus symbols at every 1th point.
C
            TYPE = +1
         else
C
C           Symbols alone.
C
            TYPE = -1
         end IF
      else
C
C        No symbol - line only
C
         TYPE = 0
      end IF
C
C     Set line color.
C     ---------------
C
      if(COLOR .GE. 1 .AND. COLOR .LE. 7)then
         call CLRSET (COLORS (COLOR))
      end IF
C
C     Plot the curve, then reset everything.
C     --------------------------------------
C
      call CURVE(X, Y, N, TYPE)
C
      call RESET ('CLRSET')
      call RESET ('CRVWID')
      call RESET ('DOT')
C
C     Termination.
C     ------------
C
  999 return
      end
      subroutine POSTSC(IUNIT)
C
C****************************************************************************
C
C  POSTSC - INITIALIZES THE POSTSCRIPT DEVICE DRIVER 
C
C  WRITTEN BY MIKE GOZA  3/31/88
C
C****************************************************************************
C
      integer NEWDRV, LUN, IERR
C
      NEWDRV = 14
      if(IUNIT.LT.0.OR.IUNIT.GT.99)then
        LUN=30
      else
        LUN=IUNIT
      endif
C
      call DDEVSL(NEWDRV,LUN,IERR)
      call ZINIT
C
      return
      end
      subroutine PRMSPL
C
C
C
C      SET INTERPOLATION FLAG
C               LEVEL 1-3, P/S
C
      parameter (KZPSPL=4)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZLTYP=KZPSPL
      else
        call ERRMES('PRMSPL',1,3)
      endif
      return
      end
      subroutine PSCCGM(DEV,FILNAM)
C
C  DEV    NAME OF DEVICE TO BE USED
c
c  FILNAM For output to file, the file name.
C
      character*(*) DEV
      character*10 DEVNAM
      character*(*) FILNAM
      EXTERNAL INIDAT
C
      NEWDEV=20
      LUN=4
C
      DEVNAM=DEV
c
      IF(DEVNAM.EQ.' ')THEN
        DEVNAM='cgmb'
      elseIF(DEVNAM.EQ.'cgm')THEN
        DEVNAM='cgmb'
      endif
c
      call Device(DEVNAM)
c
      IF(DEVNAM.EQ.'cgmb'.OR.
     &   DEVNAM.EQ.'cgmc'.OR.
     &   DEVNAM.EQ.'ps')THEN
        call outfil(filnam)
      endif
c
      call Setctb(3)
      call Grfini()
      call Linclr(1)
      call DDEVSL(NEWDEV,LUN,IERR)
      call ZINIT
      return
      end
      subroutine PTCSYM(X,Y,NPTS,ISYMNO)
C
C  ROUTINE TO PLOT VISIBLE POINTS ON SCREEN
C  USING SYMBOL ISYMNO
C  WRITTEN BY SUE PIOTROWSKI...AUGUST 13, 1985
C
      DIMENSION X(NPTS),Y(NPTS)
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
      DIMENSION AREA(4)
C
      call GSSCLP(XVSTRT,XVSTRT+XVLEN,YVSTRT,YVSTRT+YVLEN,AREA)
      DO 100 I=1,NPTS
         call SCALE (X(I),Y(I),VX,VY)
         call GSMOVE(VX,VY)
         call SYMBOL(ISYMNO,0.2)
  100 continue
      call GSRCLP(AREA)
      return
      end
      subroutine PURJOY(Z,IZDIM1,IZ,KX,KY,CAMLOC,XYLIM,
     1   XLAB,YLAB,ZLAB,CSIZE,MARPLT)
C
C  This subroutine will plot a function Z=F(X,Y) as a lined surface.
C  The function must be defined on a regular grid.   This routine
c  will optionally remove hidden lines.
C
C  Arguments:
C
C  Input
C
C     Z  * Type: real array.
C       * The function values: Z(I,J)=F(Xi,Yj), where
C        Xi = XMIN + (i-1)*(XMAX-XMIN)/(KX-1)
C        Yj = YMIN + (j-1)*(YMAX-YMIN)/(KY-1)
C
C     IZDIM1  * Type: integer constant or variable.
C       * The first dimension of the Z array - not
C        necessarily the number of X values.
C
C     IZ  * Type: byte array.
C       * A working array of bytes dimensioned atleast
C        KX*KY long.
C
C     KX  * Type: integer constant or variable.
C       * The number of X values in the Z array.
C        KX <= IZDIM1 ofcourse.
C
C     KY  * Type: integer constant or variable.
C       * The number of Y values in the Z array.
C
C     CAMLOC  * Type: real array.
C       * The relative location of the viewer in space.
C        The viewer always faces toward the center
C        of the surface.
C        CAMLOC(1) = distance from surface in units
C         the same as those of Z.
C        CAMLOC(2) = angle between the viewer and the
C         X axis in degrees.   Usually, multiples of
C         30 or 45 degrees are best.
C        CAMLOC(3) = angle between the viewer and the
C         XY plane located at Z=(ZMIN+ZMAX)/2 in
C         degrees.   Thus 90 degrees is directly above
C         the surface - an unexciting picture!   Usually
C         the angle is selected near 45 degrees.
C
C     XYLIM  * Type: real two dimensional array dimensioned (2,6).
C       * General parameters:
C        XYLIM(1,1) = XMIN ==> the minimum value of X.
C        XYLIM(2,1) = XMAX ==> the maximum value of X.
C        XYLIM(1,2) = YMIN ==> the minimum value of Y.
C        XYLIM(2,2) = YMAX ==> the maximum value of Y.
C         Note: Z(I,J) = F(Xi,Yj) where:
C           Xi = XMIN + (i-1)*(XMAX-XMIN)/(KX-1)
C           Yj = YMIN + (j-1)*(YMAX-YMIN)/(KY-1)
C        XYLIM(1,3) = ZMIN ==> the minimum value of Z.
C        XYLIM(2,3) = ZMAX ==> the maximum value of Z.
C         These Z values define the range of Z values
C         to fit on the screen.   It is strongly
C         advised that ZMIN and ZMAX bound Z(I,J).
C        XYLIM(1,4) = X/Z axis length ratio.   If this
C         parameter is 0, then X and Z are assumed to
C         have the same units, so their relative
C         lengths will be in proportion to their
C         ranges.   If this parameter is nonzero, then
C         the X axis will be XYLIM(1,4) times as long
C         as the Z axis.
C        XYLIM(2,4) = Y/Z axis length ratio.   Same as
C         XYLIM(1,4), but for Y axis.
C        XYLIM(1,5) = plot width in virtual coordinates
C        XYLIM(2,5) = plot height in virtual coord.
C         Note: The plot is expanded/contracted until
C         it all fits within the box defined by
C         XYLIM(1,5) and XYLIM(2,5).
C        XYLIM(1,6) = virtual X coord. of the lower
C         left corner of the plot box.
C        XYLIM(2,6) = virtual Y coord. of the lower
C         left corner of the box.
C
C     XLAB  * Type: string constant or variable.
C       * The X axis lable.
C
C     YLAB  * Type: string constant or variable.
C       * The Y axis lable.
C
C     ZLAB  * Type: string constant or variable.
C       * The Z axis lable.
C
C     CSIZE  * Type: real constant or variable.
C       * The character size in virtual coord. for the tick
C        mark lables and the axis lables.
C
C     MARPLT  * Type: integer constant or variable.
C       * Hidden line flag:
C         0 ==> draw all lines, hidden or not.
C         1 ==> suppress all lines hidden by the surface, but
C        display both the top and bottom of the surface
C         3 ==> suppress all lines hidden by the surface, and
C        all lines showing the bottom of the surface.
C         Add 4 to MARPLT if you do not want the axes nor the
C          ticks labled.   This is useful on small plots.
C
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1   NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      DIMENSION Z(IZDIM1,KY), CAMLOC(3), XYLIM(2,6)
      character XLAB*2, YLAB*2, ZLAB*2
      DIMENSION IZ(KX,KY)
C
      common/COMDP/XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,AXISR(2),PLOTX,
     1   PLOTY,PLTORG(2),CAMXYZ(3),MX,NY,FMX,FNY,CAMWKG(6),XORG(3),
     2   GX(3),FX(2),KSCALE,ZORG,CENTER(2),PQLMT,
     3   AMTX(3,3),FOCALL
      DIMENSION LIMIT(2),FLIM(2)
      EQUIVALENCE(U,CAMXYZ(1)),(V,CAMXYZ(2)),(W,CAMXYZ(3)),
     1   (MX,LIMIT(1)),(FMX,FLIM(1))
C
      LOGICAL LSOLID
      common /COMDP1/ LSOLID
      DIMENSION XMINA(2,6)
      LOGICAL LLABLE
      EQUIVALENCE(XMIN,XMINA(1,1))
      common /DBASE/VX,VY,VOLDX,VOLDY,CXSIZE,CYSIZE
C
C     PICK UP XY LIMITS, BOX SIZES, ETC.
C
      DO 9 J=1,6
      XMINA(1,J) = XYLIM(1,J)
9     XMINA(2,J) = XYLIM(2,J)
C
C     NOW SET UP LIMITS IF AXIS RATIOS ARE REQUESTED
C
      if(AXISR(1) .EQ. 0.0) go to 260
      DO 255 I=1,2
255   XMINA(1,I)=AXISR(I)*ZMIN
260   if(AXISR(2) .EQ. 0.0) go to 266
      DO 265 I=1,2
265   XMINA(2,I)=AXISR(I)*ZMAX
C
C  SET TOLERANCE FOR VISIBLE TESTS = HALF PLOTTER STEP SIZE
C
266   PQLMT = AMIN1(0.5/XRES,0.5/YRES)
C
C  CONVERT R, PHI, THETA TO DX, DY, DZ
C
      RAD = 3.14159/180.0
      PHI = CAMLOC(2)*RAD
      THETA = CAMLOC(3)*RAD
      CAMWKG(1)=CAMLOC(1)*COS(PHI)*COS(THETA)
      CAMWKG(2)=CAMLOC(1)*SIN(PHI)*COS(THETA)
      CAMWKG(3)=CAMLOC(1)*SIN(THETA)
C
C  PICK UP CAMERA DATA
C
      DO 3 J=1,3
      CAMWKG(J+3)=(XMINA(1,J)+XMINA(2,J))/2.0
3     CAMWKG(J)=CAMWKG(J+3)+CAMWKG(J)
      call CAMROT
      MX=KX
      FMX=FLOAT(KX)
      NY=KY
      FNY=FLOAT(NY)
C
C  OPTION FOR SCALING Z
C  SCALE FACTORS TO CONVERT USER VALUES TO INDICES
C
      GX(1) = (XMAX-XMIN)/(FMX-1.0)
      GX(2) = (YMAX-YMIN)/(FNY-1.0)
C
C  FIND Z SCALE FACTOR
C
      GX(3)=1.0
      ZORG=0.0
C
C  FIND SCALE FACTORS FOR PLOT
C
      CYSIZE = CSIZE
      CXSIZE = 9.0*CYSIZE/8.0
      XA=1.0E30
      XB=-1.0E30
      YA=1.0E30
      YB=-1.0E30
      if(CAMWKG(3) .LT. CAMWKG(6)) go to 16
      DX=FLOAT(MX-1)/20.0
      DY=FLOAT(NY-1)/20.0
      IF=MX
C     XZ = XMAX
      IB=1
      JF=NY
C     YZ = YMIN
      JB=1
      if(CAMWKG(1) .GE. CAMWKG(4)) go to 120
      IF=1
C     XZ = XMIN
      IB=MX
      DX=-DX
120   if(CAMWKG(2) .GE. CAMWKG(5)) go to 130
      JF=1
C     YZ = YMAX
      JB=NY
      DY=-DY
130   FRX=IF
      BKX=IB
      FRY=JF
      BKY=JB
      VX = XMIN + (FRX-1.0)*GX(1) - CAMWKG(1)
      VY = YMIN + (BKY-1.0-DY)*GX(2) - CAMWKG(2)
      call EXTRMA(VX,VY,ZMAX-CAMWKG(3),XA,XB,YA,YB,IERR)
      if(IERR .NE. 0) go to 50
      TEMP = ZMIN - CAMWKG(3)
      call EXTRMA(VX,VY,TEMP,XA,XB,YA,YB,IERR)
      if(IERR .NE. 0) go to 50
      VY = YMIN + (FRY-1.0+DY)*GX(2) - CAMWKG(2)
      call EXTRMA(VX,VY,TEMP,XA,XB,YA,YB,IERR)
      if(IERR .NE. 0) go to 50
      call EXTRMA(XMIN+(BKX-1.0)*GX(1)-CAMWKG(1),VY,TEMP,
     1   XA,XB,YA,YB,IERR)
      if(IERR .NE. 0) go to 50
      VX = VX + DX*GX(1)
      call EXTRMA(VX,YMIN+(BKY-1.0)*GX(2)-CAMWKG(2),TEMP,
     1   XA,XB,YA,YB,IERR)
      if(IERR .NE. 0) go to 50
      call EXTRMA(VX,VY-DY*GX(2),TEMP,XA,XB,YA,YB,IERR)
      if(IERR .NE. 0) go to 50
16    DO 20 J=1,NY
      VY = YMIN + (J-1)*GX(2) - CAMWKG(2)
      DO 20 I=1,MX
      VX = XMIN + (I-1)*GX(1) - CAMWKG(1)
      call EXTRMA(VX,VY,Z(I,J)-CAMWKG(3),XA,XB,YA,YB,IERR)
      if(IERR .NE. 0) go to 50
20    continue
C
C     SCALE X AND Y RANGES TO FIT ON PLOT
C
      TEMP = 12.5*CXSIZE
      LLABLE = .TRUE.
C
C
      MTEMP = MARPLT
C
C
C      if((MTEMP .AND. 4) .NE. 0) LLABLE = .FALSE.
      IF(MTEMP .GE. 4)then
      MTEMP = MTEMP - 4
      LLABLE = .FALSE.
      else
      endif
      if(.NOT. LLABLE) TEMP = 0.0
      FX(1) = (PLOTX-TEMP)/(XB-XA)
      TEMP = 2.0*CYSIZE
      if(.NOT. LLABLE) TEMP = 0.0
      FX(2) = (PLOTY-TEMP)/(YB-YA)
C     CHOOSE MINIMUM FOCAL LENGTH OF THE TWO
      FOCALL = AMIN1(FX(1),FX(2))
C  SET X,Y ORIGINS (BEFORE SCALING TO FOCAL LENGTH)
      XORG(1) = XA
      XORG(2) = YA
C     SIZES IN X,Y (NOT INCLUDING OUT-OF-BOX POIINTS THAT GET IN PIC)
      XB = (XB-XA)*FOCALL
      YB = (YB-YA)*FOCALL
C     CENTER FOR NOW, BUT LATER MAKE OPTIONAL
      CENTER(1) = (PLOTX-XB)/2.0
      CENTER(2) = (PLOTY-YB)/2.0
C
C     CAMERA LOCATION EXPRESSED AS XY INDICES
      U = 1.0+(FMX-1.0)*(CAMWKG(1)-XMIN)/(XMAX-XMIN)
      V = 1.0+(FNY-1.0)*(CAMWKG(2)-YMIN)/(YMAX-YMIN)
C
C  FOR VISIBILITY CHECKING, SCALE CAMERA Z COORDINATE OPPOSITE TO THE
C  WAY Z WILL BE SCALED FOR PLOTTING - RATHER THAN SCALING ALL THE
C  Z-S ON THE SURFACE WHEN CHECKING.
C
      W = (CAMWKG(3)-ZORG)/GX(3)
C
C  CALCULATE VISIBILITIES
C
C  IF LSB OF MARPLT IS SET, SUPRESS ALL HIDDEN LINES
C
C      if((MTEMP .AND. 1) .NE. 0) go to 7
      IF(MTEMP .EQ. 1) go to 7
      IF(MTEMP .EQ. 3) go to 7
      DO 8 K = 1,NY
      DO 8 J = 1,MX
8     IZ(J,K)=0
      go to 40
7     LSOLID = .FALSE.
C      if((MTEMP .AND. 2) .NE. 0) LSOLID = .TRUE.
      IF(MTEMP .EQ. 2) LSOLID = .TRUE.
      IF(MTEMP .EQ. 3) LSOLID = .TRUE.
      DO 1 K = 1,NY
      ETA = FLOAT(K)
      DO 1 J =1,MX
       L = IVIS(FLOAT(J),ETA,Z(J,K),Z,IZDIM1)+1
1     IZ(J,K)=L
C
C     NOW PLOT
C  CONFLICT WITH AN ARCGRAPH ROUTINE
C40   CALL DRAW3D(Z,IZDIM1,IZ,KX)
 40   CALL QDRW3D(Z,IZDIM1,IZ,KX)
      if(CAMWKG(3) .LT. CAMWKG(6)) go to 45
      call GSSETC(CYSIZE,0.0)
      call GSCOLR(1,JER)
      call XYPRM(FRX,BKY,ZMAX,0)
      VOLDX=VX
      VOLDY=VY
      VXT=VX
      VYT=VY
      call XYPRM(FRX,BKY-DY,ZMAX,1)
      if(LLABLE) CALL TICKL(ZMAX,-0.5)
      call GSMOVE(VXT,VYT)
      call XYPRM(FRX,BKY,ZMIN,1)
      VOLDX=VX
      VOLDY=VY
      call XYPRM(FRX,BKY-DY,ZMIN,1)
      if(.NOT. LLABLE) go to 140
      call TICKL(ZMIN,0.25)
      TEMP = AMAX1(VOLDX,VXT)+1.5*CYSIZE
      if(VX .LT. VOLDX) TEMP = AMIN1(VOLDX,VXT)-0.5*CYSIZE
      call GSMOVE(TEMP,(VOLDY+VYT-CXSIZE*NUMCHR(ZLAB))/2.0)
      call GSSETC(CYSIZE,90.0)
      call GSPSTR(ZLAB)
      call GSSETC(CYSIZE,0.0)
140   CALL GSMOVE(VOLDX,VOLDY)
      call XYPRM(FRX+DX,BKY,ZMIN,1)
      if(LLABLE) CALL TICKL(XYLIM(1+JB/NY,2),-0.5)
      call GSMOVE(VOLDX,VOLDY)
      call XYPRM(FRX,FRY+DY,ZMIN,1)
      if(.NOT. LLABLE) go to 150
      call TICKL(XYLIM(1+IF/MX,1),-0.5)
      TEMP = CXSIZE*(NUMCHR(YLAB)+0.25)
      if(VX .LT. VOLDX) TEMP = -0.25*CXSIZE
      call GSMOVE((VX+VOLDX)/2.0-TEMP,(VY+VOLDY)/2.0-CYSIZE)
      call GSPSTR(YLAB)
150   CALL XYPRM(FRX,FRY,Z(IF,JF),-1)
      call GSMOVE(VX,VY)
      call XYPRM(FRX,FRY,ZMIN,1)
      VOLDX=VX
      VOLDY=VY
      call XYPRM(FRX+DX,FRY,ZMIN,1)
      if(LLABLE) CALL TICKL(XYLIM(1+JF/NY,2),-0.5)
      call GSMOVE(VOLDX,VOLDY)
      call XYPRM(BKX,FRY,ZMIN,1)
      if(.NOT. LLABLE) go to 160
      TEMP = CXSIZE*(NUMCHR(XLAB)+0.25)
      if(VX .GT. VOLDX) TEMP = -0.25*CXSIZE
      call GSMOVE((VX+VOLDX)/2.0-TEMP,(VY+VOLDY)/2.0-CYSIZE)
      call GSPSTR(XLAB)
160   VOLDX=VX
      VOLDY=VY
      call GSMOVE(VX,VY)
      call XYPRM(BKX,FRY+DY,ZMIN,1)
      if(LLABLE) CALL TICKL(XYLIM(1+IB/MX,1),-0.5)
      call GSMOVE(VOLDX,VOLDY)
      call XYPRM(BKX,FRY,Z(IB,JF),1)
45      return
C
C  POINT ON THE SURFACE IS BEHIND THE CAMERA. QUIT.
C
  50  WRITE(*,603)
      return
C
C  Z IS A FLAT PLANE, DO NOT DRAW (FOR NOW)
C
  60  WRITE(*,604)
      return
C
C 503 FORMAT(' Z MULTIPLIER',E15.6,', Z ORIGIN SHIFT',E15.6)
C504     FORMAT('0X LIMITS',2F10.3/' Y LIMITS',2F10.3/' Z LIMITS',2F10.3/
C     1' Z CUTOFF',2E15.6/
C     2                 ' PLOT SIZE',2F10.3/' PLOT ORIGIN',2F10.3)
C 602 FORMAT('0FOCAL LENGTHS TO FILL X,Y PLOTTER SPACE',2E15.6,
C    1 ', LESSER VALUE CHOSEN'/'0PICTURE SIZE IN X,Y =',2F9.3,
C    2 ', REQUESTED SIZES',2F9.3/' CENTERS = ',2G14.7)
  603 FORMAT('0PART OF SURFACE IS BEHIND THE CAMERA, UNABLE TO PLOT. SOR
     1RY.')
  604 FORMAT('0FUNCTION IS LEVEL PLANE, NO USE PLOTTING IT')
      end
      subroutine QDRW3D(Z,IZDIM1,IZ,KX)
C
C     DRAW PLOT
C
      DIMENSION Z(IZDIM1,2)
      DIMENSION IZ(KX,2)
C
C     COMMON STORAGE DESCRIPTOR
      common/COMDP/XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,AXISR(2),PLOTX,
     1   PLOTY,PLTORG(2),CAMXYZ(3),MX,NY,FMX,FNY,CAMWKG(6),XORG(3),
     2   GX(3),FX(2),KSCALE,ZORG,CENTER(2),PQLMT,
     3   AMTX(3,3),FOCALL
      DIMENSION LIMIT(2),FLIM(2)
      EQUIVALENCE(U,CAMXYZ(1)),(V,CAMXYZ(2)),(W,CAMXYZ(3)),
     1   (MX,LIMIT(1)),(FMX,FLIM(1))
C     END CDE
C
C
C     CDE PACKAGE FOR DRAW3D,DRAWPQ
      common/COMDPA/PC(3),QC(3),P(3),Q(3),ENDA(6),ENDB(6),OLDQ(3),
     1   PW(3),QW(3),T(6),PK(3),QK(3),PHIP,PHIQ,PHIA,IBEAM,ICOLOR
      integer PHIP,PHIQ,PHIA
C     END OF CDE PACKAGE
C     END OF CDE PACKAGE
C
C     SAVE Z DIMENSION IN COMMON TO PASS ALONG THROUGH DRAWPQ TO IVIS
C     SCAN ALONG X FIRST AT CONSTANT Y
C
C     INDEX OF COORDINATE BEING STEPPED ALONG A LINE
      KSCAN = 1
C     INDEX OF COORDINATE BEING HELD FIXED
      KFIX = 2
C     SET FIXED COORDINATE   INCREMENT
      PC(KFIX) = 1.0
      DELFIX = 1.0
C     SET ROVING COORDINATE   INCREMENT INITIALLY
      DELSCN = 1.0
      QC(KSCAN) = 1.0
C     BEGIN SCANNING A LINE
101   QC(KFIX) = PC(KFIX)
      IBEAM = 0
C     NEXT POINT IN LINE SCAN
102   PC(KSCAN) = QC(KSCAN)
      QC(KSCAN) = PC(KSCAN) + DELSCN
C     WORKING INDICES
      JPC = IFIX(PC(1))
      KPC = IFIX(PC(2))
      JQC = IFIX(QC(1))
      KQC = IFIX(QC(2))
C     PHI FUNCTIONS
      PC(3)=Z(JPC,KPC)
      QC(3)=Z(JQC,KQC)
      PHIP=IZ(JPC,KPC)-1
      PHIQ=IZ(JQC,KQC)-1
200   CALL DRAWPQ(Z,IZDIM1)
C     TEST IF LINE IS DONE
      IF((QC(KSCAN)-1.0)*(QC(KSCAN)-FLIM(KSCAN)) .LT. 0.0) go to 102
C     LINE DONE. ADVANCE FIXED COORDINATE.
      PC(KFIX) = PC(KFIX) + DELFIX
C     TEST IF FIXED COORDINATE NOW OFF LIMITS
      IF((PC(KFIX)-1.0)*(PC(KFIX)-FLIM(KFIX)) .GT. 0.0) go to 55
C     FLIP INCREMENT. SCAN BEGINS AT QC OF PREVIOUS LINE.
      DELSCN = -DELSCN
      go to 101
C     TEST IF WE HAVE DONE Y SCAN YET.
55    IF(KSCAN .EQ. 2) return
C     NO, SCAN Y DIRECTION AT FIXED X.
      KSCAN = 2
      KFIX = 1
C     START FIXED X AT X OF LAST TRAVERSE
      PC(1) = QC(1)
C     THEN STEP X IN OPPOSITE DIRECTION
      DELFIX = -DELSCN
C     WE ENDED UP AT MAX. Y, SO FIRST Y SCAN GOES BACKWARDS
      DELSCN = -1.0
C     INITIAL Y FOR FIRST LINE
      QC(2) = FNY
      go to 101
      end
      subroutine QMS(IUNIT,N)
C
C
C
C
C
C      INITIALIZE LASER PRINTER-WIDE
C
C      INPUT:   IUNIT = OUTPUT LOGICAL UNIT, IF NOT LEGAL
C                       ZERO THEN USE DEFAULT, 31
C               N     = NOT USED IN DISSIM
C
      NEWDEV=3
      if(IUNIT.LE.0.OR.IUNIT.GT.99)then
        LUN=31
      else
        LUN=IUNIT
      endif
      call DDEVSL(NEWDEV,LUN,IERR)
      call ZINIT
      return
      end
      subroutine REALNO(ANUM,IPLACE,XPOS,YPOS)
C
C
C
C
C      WRITE REAL NUMBER ON SCREEN
C               (LEVEL 2-3)
C
C     INPUT:   ANUM      = REAL NUMBER
C              IPLACE    = FORMAT FLAG
C                          IF > 100, FREE POINT, IPLACE-100 DIGITS
C                            TOTAL
C                          IF > 0, FLOATING POINT, WITH IPLACE
C                            DECIMAL PLACES AFTER DECIMAL POINT
C                          IF < 0, EXPONENT FORM, IPLACE DECIMAL
C                            PLACES AFTER DECIMAL POINT
C              XPOS,YPOS = DISTANCE FROM PHYSICAL ORIGIN IN
C                          INCHES
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /GCVPOS/ XVPOS, YVPOS
      common /GCCPAR/ CSIZE, CCOS, CSIN
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
C
C:  CHANGED TO ACCOMMADATE AN 8 BYTE WORD
C     DIMENSION LMESSA(20)
C     character CZALFL*1,CZALFN*5,CZALFS*5,KHAR*4
C     EQUIVALENCE (TEMP,KHAR)
      DIMENSION LMESSA(80/NUMBYT)
      character CZALFL*1,CZALFN*5,CZALFS*5,KHAR*(NUMBYT)
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
C
C                        CALCULATE VIRTUAL COORDINATES
C
        TEMP=XPOS
C:  CHANGED TO ELIMINATE THE EQUIVALENCING PROBLEM
C       if(KHAR.EQ.'ABUT')then
        call WIN2CH(TEMP,KHAR)
        if(KHAR(1:4).EQ.'ABUT')then
          VX=ZZABUX
        else
          VX=XCM0+ZZXOR+XPOS*ZZUNIT*ZZPAGR
        endif
        TEMP=YPOS
C:  CHANGED TO ELIMINATE THE EQUIVALENCING PROBLEM
C       if(KHAR.EQ.'ABUT')then
        call WIN2CH(TEMP,KHAR)
        if(KHAR(1:4).EQ.'ABUT')then
          VY=ZZABUY
        else
          VY=YCM0+ZZYOR+YPOS*ZZUNIT*ZZPAGR
        endif
C
C                        DRAW character STRING
C
        call DSMOVE(VX,VY)
        call ZREAL(ANUM,IPLACE,LMESSA,LPOWER)
        LN=LENG(LMESSA)
        call ZTEXT(LMESSA,LN,CZALFL,CZALFN,CZALFS)
        ZZABUX=XVPOS
        ZZABUY=YVPOS
C
C                        DRAW EXPONENT IF DESIRED
C
        if(LPOWER.NE.0)then
          TX=0.7*ZZHITE*CSIN
          TY=0.7*ZZHITE*CCOS
          call DSMOVE(ZZABUX+TX,ZZABUY+TY)
          call GSSETC(0.6*ZZHITE,ZZANGL)
          LN=LENG(LPOWER)
          call ZTEXT(LPOWER,LN,CZALFL,CZALFN,CZALFS)
          ZZABUX=XVPOS-TX
          ZZABUY=YVPOS-TY
          call GSSETC(ZZHITE,ZZANGL)
        endif
C
C                        WRONG LEVEL
C
      else
        call ERRMES('REALNO',3,0)
      endif
      return
      end
      subroutine RELINT(IVAL,XVAL,YVAL)
C
C
C
C
C     THIS ROUTINE CONVERTS "IVAL" TO A STRING
C              WITH NO LEADING OR TRAILING SPACES.
C
C     INPUT:   IVAL      = INPUT integer
C              XVAL,YVAL = X AND Y VALUES IN CURRENT COORDINATE
C                          SYSTEM
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /GCVPOS/ XVPOS, YVPOS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
C:  CHANGED TO ACCOMMADATE AN 8 BYTE WORD
C     DIMENSION IMESSA(3)
C     character CMESS*12,KHARX*4,KHARY*4
C     EQUIVALENCE (TEMPX,KHARX),(TEMPY,KHARY),(CMESS,IMESSA)
      DIMENSION IMESSA(16/NUMBYT)
      character CMESS*16,KHARX*(NUMBYT),KHARY*(NUMBYT)
C
 10   FORMAT(I11)
      if(KZLEVL.EQ.3)then
        TEMPX=XVAL
C
C                        CALCULATE VIRTUAL COORDINATE
C
C:  CHANGED TO ELIMINATE EQUIVALENCING PROBLEM
C       if(KHARX.EQ.'ABUT')then
        call WIN2CH(TEMPX,KHARX)
        if(KHARX(1:4).EQ.'ABUT')then
          XT=1.0
        else
          XT=XVAL
        endif
        TEMPY=YVAL
C:  CHANGED TO ELIMINATE EQUIVALENCING PROBLEM
C       if(KHARY.EQ.'ABUT')then
        call WIN2CH(TEMPY,KHARY)
        if(KHARY(1:4).EQ.'ABUT')then
          YT=1.0
        else
          YT=YVAL
        endif
        call SCALE(XT,YT,VX,VY)
C:  CHANGED TO ACCOMMADATE AN 8 BYTE WORD
C       if(KHARX.EQ.'ABUT') VX=ZZABUX
C       if(KHARY.EQ.'ABUT') VY=ZZABUY
        if(KHARX(1:4).EQ.'ABUT') VX=ZZABUX
        if(KHARY(1:4).EQ.'ABUT') VY=ZZABUY
C
C                        DRAW integer
C
        call DSMOVE(VX,VY)
        WRITE(CMESS,10)IVAL
        CMESS(12:12)=CHAR(0)
C
        DO 5000 I=1,16/NUMBYT
          call WCH2IN(CMESS((I-1)*NUMBYT+1:I*NUMBYT),IMESSA(I))
5000    continue
        call ZSTRBL(IMESSA)
        LN=LENG(IMESSA)
        call ZTEXT(IMESSA,LN,CZALFL,CZALFN,CZALFS)
        ZZABUX=XVPOS
        ZZABUY=YVPOS
      else
        call ERRMES('RELINT',3,0)
      endif
      return
      end
      subroutine RELMSG(LMESS,IMESS,XVAL,YVAL)
C
C
C
C
C     WRITE character STRING ON SCREEN
C              (LEVEL 3)
C
C     INPUT:   LMESS     = character STRING
C              IMESS     = NUMBER OF characterS IN LMESS
C              XVAL,YVAL = X AND Y VALUES IN CURRENT COORDINATE
C                          SYSTEM
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /GCVPOS/ XVPOS, YVPOS
C
      DIMENSION LMESS(*)
      character CZALFL*1,CZALFN*5,CZALFS*5,KHARX*(NUMBYT),KHARY*(NUMBYT)
      if(KZLEVL.EQ.3)then
C
C  CALCULATE VIRTUAL COORDINATE
C
        TEMPX=XVAL
        call WIN2CH(TEMPX,KHARX)
        if(KHARX(1:4).EQ.'ABUT')then
          XT=1.0
        else
          XT=XVAL
        endif
        TEMPY=YVAL
        call WIN2CH(TEMPY,KHARY)
        if(KHARY(1:4).EQ.'ABUT')then
          YT=1.0
        else
          YT=YVAL
        endif
        call SCALE(XT,YT,VX,VY)
        if(KHARX(1:4).EQ.'ABUT') VX=ZZABUX
        if(KHARY(1:4).EQ.'ABUT') VY=ZZABUY
C
C  DRAW MESSAG
C
        call DSMOVE(VX,VY)
        call ZTEXT(LMESS,IMESS,CZALFL,CZALFN,CZALFS)
        ZZABUX=XVPOS
        ZZABUY=YVPOS
      else
        call ERRMES('RELMSG',3,0)
      endif
      return
      end
      subroutine RELRNO(ANUM,IPLACE,XVAL,YVAL)
C
C     THIS ROUTINE CONVERTS "ANUM" TO A STRING
C
C     INPUT:   ANUM      = INPUT REAL NUMBER
C              IPLACE    = FORMAT FLAG
C                          IF > 100,FREE POINT, IPLACE-100 DIGITS
C                          TOTAL
C                          IF > 0, FLOATING POINT, WITH IPLACE
C                          DECIMAL PLACES AFTER DECIMAL POINT
C                          IF < 0, EXPONENT FORM, IPLACE DECIMAL
C                          PLACES AFTER DECIMAL POINT
C              XVAL,YVAL = X AND Y VALUE IN CURRENT COORDINATE
C                          SYSTEM
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /GCVPOS/ XVPOS, YVPOS
      common /GCCPAR/ CSIZE, CCOS, CSIN
C
      DIMENSION LMESSA(80/NUMBYT)
      character CZALFL*1,CZALFN*5,CZALFS*5,KHARX*(NUMBYT),
     .KHARY*(NUMBYT)
C
      if(KZLEVL.EQ.3)then
C
C  CALCULATE VIRTUAL COORDINATES
C
        TEMPX=XVAL
        call WIN2CH(TEMPX,KHARX)
        if(KHARX(1:4).EQ.'ABUT')then
          XT=1.0
        else
          XT=XVAL
        endif
        TEMPY=YVAL
        call WIN2CH(TEMPY,KHARY)
        if(KHARY(1:4).EQ.'ABUT')then
          YT=1.0
        else
          YT=YVAL
        endif
        call SCALE(XT,YT,VX,VY)
        if(KHARX(1:4).EQ.'ABUT') VX=ZZABUX
        if(KHARY(1:4).EQ.'ABUT') VY=ZZABUY
C
C                        DRAW character STRING
C
        call DSMOVE(VX,VY)
        call ZREAL(ANUM,IPLACE,LMESSA,LPOWER)
        LN=LENG(LMESSA)
        call ZTEXT(LMESSA,LN,CZALFL,CZALFN,CZALFS)
        ZZABUX=XVPOS
        ZZABUY=YVPOS
C
C  IF EXPONENTIAL DESIRED, DRAW IT
C
        if(LPOWER.NE.0)then
          TX=0.7*ZZHITE*CSIN
          TY=0.7*ZZHITE*CCOS
          call DSMOVE(ZZABUX+TX,ZZABUY+TY)
          call GSSETC(0.6*ZZHITE,ZZANGL)
          LN=LENG(LPOWER)
          call ZTEXT(LPOWER,LN,CZALFL,CZALFN,CZALFS)
          ZZABUX=XVPOS-TX
          ZZABUY=YVPOS-TY
          call GSSETC(ZZHITE,ZZANGL)
        endif
C
C                        WRONG LEVEL
C
      else
        call ERRMES('RELRNO',3,0)
      endif
      return
      end
      subroutine RELVEC(XFROM,YFROM,XTO,YTO,IVEC)
C
C      DRAW A VECTOR W/ OR W/O ARROW HEAD
C               LEVEL 3
C
C      INPUT:   XFROM,YFROM = FIRST POINT OF VECTOR
C               XTO,YTO     = SECOND POINT OF VECTOR
C               IVEC(WXYZ)  = ARROW TYPE AND SIZE FLAG
C
      common /CLEVEL/ KZLEVL,KZBEGN
C
      if(KZLEVL.EQ.3)then
        call SCALE(XFROM,YFROM,VXFROM,VYFROM)
        call SCALE(XTO,YTO,VXTO,VYTO)
        call ZVECTR(VXFROM,VYFROM,VXTO,VYTO,IVEC)
      else
        call ERRMES('RELVEC',3,0)
      endif
      return
      end
      subroutine RESET(PARAM)
C
C     RESET TO DEFAULT
C              NO EFFECT ON 'SETOUT' AND HARDWARE CALLS
C
C     INPUT:   PARAM = parameter INDICATOR
C
      parameter (KZYES=111)
      parameter (KZNO=222)
      parameter (KZDOWN=13)
      parameter (KZLIN=2)
      parameter (KZSOLI=1)
      parameter (KZREAL=2)
      parameter (KZMXBS=1000)
      parameter (KZMXBL=200)
      parameter (KZMAXC=255)
      parameter (ZZIN=2.54)
C
      save carea
      save caxis
      save cblank
      save cborch
      save clabel
      save clevel
      save clgndc
      save clgndn
      save cline
      save cmxalf
      save colorc
      save colorn
      save cpage
      save csetunt
      save cstrng
      save dcltyp
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
      common /CBLANK/ ZZBLNK(KZMXBL,4),ZZBLKS(KZMXBS,4),KZBLCN,KZBSCN
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      common /COLORN/ KZSHD,ZZHSPC,ZZCOLR(KZMAXC,3),KZNCLR,KZCCOL
      common /COLORC/ CZCOLR
      common /CBORCH/ KZBRDR,KZCHEK
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CSETUNT/  ZZSETUNT
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
C
      LOGICAL LINILT, LPOSND
      character CZCOLR*8
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      character*(*) PARAM
      character SUB4*4
      character*6 SUB
C
      NCMAX = LEN(PARAM)
      IF(NCMAX .GT. 6) NCMAX = 6
      SUB=' '
      SUB(1:NCMAX) = PARAM
      call CAPCHR(SUB)
C
C  FOR ALL RESET
C
      if(SUB(1:3).EQ.'ALL')then
        ZZANGL=0.0
        ZZHITE=0.14*ZZIN*ZZPAGR
        call GSSETC(ZZHITE,ZZANGL)
        KZLSTY=KZSOLI
        call DSLTYP(KZLSTY)
        DO 100 I=1,KZBLCN
          DO 200 J=1,4
            ZZBLNK(I,J)=-1100.0
 200      continue
 100    continue
        KZBLCN=4
        if(KZBSCN.GT.0)then
          DO 1800 I=1,KZBSCN
            DO 900 J=1,4
              ZZBLKS(I,J)=-1100.0
 900        continue
 1800     continue
          KZBSCN=-1
        endif
        UUGRCE=0.5*ZZSETUNT
        ZZGRCE=UUGRCE*ZZPAGR
C        KZSHD=KZNO
        ZZSRAT=1.5
        CZALFL(1)=' '
        CZALFN(1)='STAND'
        CZALFS='DEFAU'
        call ZSCOPY('TXTBLK',KZLGTI)
        KZLGLN=KZNO
        ZZLGTZ=-1.0
C       KZLGTF=1
        ZZLGTR=-1.0
        KZCHEK=KZYES
        KZBRDR=KZYES
        call SIZMRK(1.0)
        CZCOLR='WHITE'
        call GSCOLR(1,IERR)
        KZSTRM='$'
        KZTMLN=1
        KZLTYP=KZLIN
        KZLTHK=KZNO
        ZZSETUNT=ZZIN
        ZZXLBA=0.0
        KZXTYP=KZREAL
        KZYTYP=KZREAL
        ZZYLBA=90.0
        KZXNON=KZNO
        KZYNON=KZNO
        KZXTCK=1
        KZYTCK=1
        UUFRME=0.01*ZZIN
        ZZFRME=UUFRME*ZZPAGR
        call GSFONT(1,IERR)
C
C                        FOR ANGLE
C
      elseif(SUB(1:5).EQ.'ANGLE')then
        ZZANGL=0.0
        call GSSETC(ZZHITE,ZZANGL)
C
C                        FOR BLNKi'S
C
      elseif(SUB.EQ.'BLANK1')then
        DO 300 I=1,4
          ZZBLNK(1,I)=-1100.0
 300    continue
      elseif(SUB.EQ.'BLANK2')then
        DO 400 I=1,4
          ZZBLNK(2,I)=-1100.0
 400    continue
      elseif(SUB.EQ.'BLANK3')then
        DO 500 I=1,4
          ZZBLNK(3,I)=-1100.0
 500    continue
      elseif(SUB.EQ.'BLANK4')then
        DO 600 I=1,4
          ZZBLNK(4,I)=-1100.0
 600    continue
      elseif(SUB.EQ.'BLNKAL')then
        DO 700 I=1,KZBLCN
          DO 800 J=1,4
            ZZBLNK(I,J)=-1100.0
 800      continue
 700    continue
        KZBLCN=4
        if(KZBSCN.GT.0)then
          DO 1000 I=1,KZBSCN
            DO 1200 J=1,4
              ZZBLKS(I,J)=-1100.0
 1200       continue
 1000     continue
          KZBSCN=-1
        endif
C
C  FOR BLANKS
C
      elseif(SUB.EQ.'BLANKS')then
        if(KZBSCN.GT.0)then
          DO 1300 I=1,KZBSCN
            DO 1400 J=1,4
              ZZBLKS(I,J)=-1100.0
 1400       continue
 1300     continue
          KZBSCN=-1
        endif
C
C  FOR DOT, LINDEF, DASH, CHNDOT, AND CHNDSH
C
      elseif(SUB(1:3).EQ.'DOT'   .OR.SUB.EQ.'LINDEF'.OR.
     *           SUB(1:4).EQ.'DASH'.OR.SUB.EQ.'CHNDOT'.OR.
     *           SUB.EQ.'CHNDSH')then
        KZLSTY=KZSOLI
        call DSLTYP(KZLSTY)
C
C  FOR MARGIN
C
      elseif(SUB.EQ.'MARGIN')then
        UUGRCE=0.5*ZZSETUNT
        ZZGRCE=UUGRCE*ZZPAGR
C
C                        FOR HEIGHT
C
      elseif(SUB.EQ.'HEIGHT')then
        UUHITE=0.14*ZZIN
        ZZHITE=UUHITE*ZZPAGR
        call GSSETC(ZZHITE,ZZANGL)
C
C                        FOR HRDSHD
C
      elseif(SUB.EQ.'HRDSHD')then
        KZSHD=KZNO
C
C                        FOR HRDSCL
C
      elseif(SUB.EQ.'HRDSCL')then
        KZSCAL=KZDOWN
C
C                        FOR SAVLIN
C
      elseif(SUB.EQ.'SAVLIN')then
        KZLGLN=KZNO
C
C                        FOR VSPACE
C
      elseif(SUB.EQ.'VSPACE')then
        ZZSRAT=1.5
C
C                        FOR DEFALF AND ALPHA1
C
      elseif(SUB.EQ.'DEFALF'.OR.SUB.EQ.'ALPHA1')then
        CZALFL(1)=' '
        CZALFN(1)='STAND'
C
C                        FOR MIXALF AND ALPHA2
C
      elseif(SUB.EQ.'MIXALF'.OR.SUB.EQ.'ALPHA2')then
        CZALFL(2)=' '
C
C                        FOR ALPHA3
C
      elseif(SUB.EQ.'ALPHA3')then
        CZALFL(3)=' '
C
C                        FOR ALPHA4
C
      elseif(SUB.EQ.'ALPHA4')then
        CZALFL(4)=' '
C
C                        FOR ALPHA5
C
      elseif(SUB.EQ.'ALPHA5')then
        CZALFL(5)=' '
C
C                        FOR ALPHA6
C
      elseif(SUB.EQ.'ALPHA6')then
        CZALFL(6)=' '
C
C                        FOR LEGHDG
C
      elseif(SUB.EQ.'LEGHDG')then
        call ZSCOPY('TXTBLK',KZLGTI)
        ZZLGTZ=-1.0
C       KZLGTF=1
        ZZLGTR=-1.0
C
C                        FOR NOCHEK
C
      elseif(SUB.EQ.'NOCHEK')then
        KZCHEK=KZYES
C
C                        FOR NOBORD
C
      elseif(SUB.EQ.'NOBORD')then
        KZBRDR=KZYES
C
C                        FOR SIZMRK
C
      elseif(SUB.EQ.'SIZMRK')then
        call SIZMRK(1.0)
C
C                        FOR CLRSET
C
      elseif(SUB.EQ.'CLRSET')then
        CZCOLR='WHITE'
        call GSCOLR(1,IERR)
C
C                          FOR SETOUT
C
      elseif(SUB.EQ.'SETOUT')then
        KZERR=6
        KZSUM=6
C
C                        FOR TRMCHR
C
      elseif(SUB.EQ.'TRMCHR')then
        KZSTRM='$'
        KZTMLN=1
C
C                        FOR CUBSPL, PRMSPL AND POLY3
C
      elseif(SUB.EQ.'CUBSPL'.OR.SUB.EQ.'PRMSPL'.OR.
     *           SUB(1:5).EQ.'POLY3')then
        KZLTYP=KZLIN
C
C                        FOR CRVWID
C
      elseif(SUB.EQ.'CRVWID')then
        KZLTHK=KZNO
C
C                        FOR FRMWID
C
      elseif(SUB.EQ.'FRMWID')then
        UUFRME=0.01*ZZIN
        ZZFRME=UUFRME*ZZPAGR
C
C                        FOR TRIPLX,CARTOG,SIMPLX,CMPLX2,
C                        COMPLX,DUPLEX,GOTHIC
C
      elseif(SUB.EQ.'TRIPLX'.OR.
     *           SUB.EQ.'CARTOG'.OR.SUB.EQ.'SIMPLX'.OR.
     *           SUB.EQ.'CMPLX2'.OR.SUB.EQ.'COMPLX'.OR.
     *           SUB.EQ.'GOTHIC'.OR.SUB.EQ.'DUPLEX')then
        CZALFS='DEFAU'
C
C                        FOR SETUNT
C
      else IF(SUB(1:4).EQ.'SETUNT')then
        ZZSETUNT=ZZIN
C
C                        FOR XANGLE
C
      elseif(SUB.EQ.'XANGLE')then
        ZZXLBA=0.0
C
C                        FOR INTGRX
C
      elseif(SUB.EQ.'INTGRX')then
        KZXTYP=KZREAL
C
C                        FOR XLABEL
C
      elseif(SUB.EQ.'XLABEL')then
        KZXALN=0
C
C                        FOR NOXLBL
C
      elseif(SUB.EQ.'NOXLBL')then
        KZXNON=KZNO
C
C                        FOR XMARKS
C
      elseif(SUB.EQ.'XMARKS')then
        KZXTCK=1
C
C                        FOR YANGLE
C
      elseif(SUB.EQ.'YANGLE')then
        ZZYLBA=90.0
C
C                        FOR INTGRY
C
      elseif(SUB.EQ.'INTGRY')then
        KZYTYP=KZREAL
C
C                        FOR YLABEL
C
      elseif(SUB.EQ.'YLABEL')then
        KZYALN=0
C
C                        FOR NOYLBL
C
      elseif(SUB.EQ.'NOYLBL')then
        KZYNON=KZNO
C
C                        FOR YMARKS
C
      elseif(SUB.EQ.'YMARKS')then
        KZYTCK=1
      else
        SUB4=SUB(1:4)
        call WCH2IN(SUB4,ISUB)
        call ERRMES('RESET',ISUB,4)
      endif
      return
      end
C
      subroutine ROTATE(XIN,A,XOUT)
C     ROTATE VECTOR XIN BY MATRIX A TO GET XOUT
C
C
      DIMENSION XIN(3),A(9),XOUT(3)
      XOUT(1) = A(1)*XIN(1) + A(4)*XIN(2) + A(7)*XIN(3)
      XOUT(2) = A(2)*XIN(1) + A(5)*XIN(2) + A(8)*XIN(3)
      XOUT(3) = A(3)*XIN(1) + A(6)*XIN(2) + A(9)*XIN(3)
      return
      end
      subroutine RSTMAP(AREA)
C
C     THIS subroutine RESTORES THE MAPPING parameterS SAVED BY "SAVMAP".
C
      DIMENSION AREA(14)
      LOGICAL LOGX, LOGY
c
      save pltclp
      save pltcom
      save pltsiz
c
      common /PLTCLP/ XMIN,XMAX,YMIN,YMAX
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
C     RESTORE THE MAPIT CLIPPING LIMITS
C
      XMIN = AREA(1)
      XMAX = AREA(2)
      YMIN = AREA(3)
      YMAX = AREA(4)
C
C     SAVE WORLD TO VIRTUAL COORD. TRANSFORMATION CONST.
C
      UX0 = AREA(5)
      UDX = AREA(6)
      UY0 = AREA(7)
      UDY = AREA(8)
      LOGX = .FALSE.
      if(AREA(9) .NE. 0.0) LOGX = .TRUE.
      LOGY = .FALSE.
      if(AREA(10) .NE. 0.0) LOGY = .TRUE.
C
C     RESTORE VIRT. COORD. OF AXES
C
      XVSTRT = AREA(11)
      YVSTRT = AREA(12)
      XVLEN = AREA(13)
      YVLEN = AREA(14)
C
C     ALL DONE
C
      return
      end
      subroutine SAVLIN
C
C
C
C
C      SET FLAG TO SAVE LINE STYLE EVERY
C               TIME CURVE IS CALLED
C               (LEVEL 1-3, P/S)
C
      parameter (KZYES=111)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZLGLN=KZYES
      else
        call ERRMES('SAVLIN',1,3)
      endif
      return
      end
      subroutine SAVMAP(AREA)
C
C     THIS subroutine SAVES THE STATUS FROM THE LAST MAPPRM-MAPIT CALLS.
C     WHEN USED IN CONJUCTION "RSTMAP", THE USER CAN SWITCH AROUND BETWEEN
C     MULTIPLE GRAPHIC REGIONS ON THE SCREEN CREATED WITH "MAPIT".
C
      DIMENSION AREA(15)
      LOGICAL LOGX, LOGY
c
      save pltclp
      save pltcom
      save pltsiz
c
      common /PLTCLP/ XMIN,XMAX,YMIN,YMAX
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
C     SAVE THE MAPIT CLIPPING LIMITS
C
      AREA(1) = XMIN
      AREA(2) = XMAX
      AREA(3) = YMIN
      AREA(4) = YMAX
C
C     SAVE WORLD TO VIRTUAL COORD. TRANSFORMATION CONST.
C
      AREA(5) = UX0
      AREA(6) = UDX
      AREA(7) = UY0
      AREA(8) = UDY
      AREA(9) = 0.0
      if(LOGX) AREA(9) = 1.0
      AREA(10) = 0.0
      if(LOGY) AREA(10) = 1.0
C
C     NOW SAVE VIRT. COORD. LOCATION OF AXES
C
      AREA(11) = XVSTRT
      AREA(12) = YVSTRT
      AREA(13) = XVLEN
      AREA(14) = YVLEN
C
C     ALL DONE
C
      return
      end
      subroutine SCALE(X,Y,VX,VY)
c
c***********************************************************************
c
C     THIS subroutine CONVERTS THE POINT (X,Y) FROM WORLD COORDINATES
C     TO THE POINT (VX,VY) IN VIRTUAL COORDINATES.
C
      real smllog
      parameter (smllog=-100.0)
c
      LOGICAL LOGX
      logical LOGY
c
      save pltcom
      save pltsiz
c
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
      XX = X
      if(.NOT. LOGX) go to 10
       if(X .LE. 0.0) go to 5
        XX = ALOG10(X)
        go to 10
5          continue
        XX = SMLLOG
10    continue
      YY = Y
      if(.NOT. LOGY) go to 20
       if(Y .LE. 0.0) go to 15
        YY = ALOG10(Y)
        go to 20
15         continue
        YY = SMLLOG
20    continue
      VX = XVSTRT + XVLEN*(XX-UX0)/UDX
      VY = YVSTRT + YVLEN*(YY-UY0)/UDY
      return
      end
      subroutine SCALIN(AMIN,AMAX,AXMAX,ORIG,STEP,RAXIS)
c
c***********************************************************************
c
C      CALCULATE ROUNDED LINEAR AXIS LIMITS
C               LEVEL 1-3
C
C      INPUT:   AMIN,AMAX = RANGE OF DATA
C               AXMAX     = MAXIMUM AXIS LENGTH (INCHES)
C
C      OUTPUT:  ORIG = ROUNDED VALUE AT ORIGIN
C               STEP = ROUNDED STEP SIZE
C               AXIS = ROUNDED MINIMUM AXIS LENGTH
C                           IN INCHES
C
      LOGICAL L1,L2
c
      save clevel
      save cpage
      save cunit
      save pltprm
      save pltsiz
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI

      L1=.FALSE.
      L2=.FALSE.
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        if(KZLEVL.EQ.1)then
          SIZE=0.3556
        else
          SIZE=CXSIZE
        endif
        NUMTK=MIN0(10,INT(AXMAX*ZZUNIT*ZZPAGR/
     *                      ((ILABSZ()+1.0)*SIZE)))
        call AXIS(AMIN,AMAX,NUMTK,L1,L2,BMIN,BMAX,
     *              BTMIN,BTMAX,BTICK,IPOW)
        ORIG=BMIN
        STEP=BTICK*10.0**IPOW
        PERIN=STEP*AXMAX/(BMAX-BMIN)
        if(PERIN.GT.1.8) STEP=STEP/2.0
        RAXIS=(BMAX-BMIN)/STEP
      else
        call ERRMES('SCALIN',1,3)
      endif
      return
      end
      subroutine SCALOG(ALOW,AHI,AXIS,ORIGIN,CYCLE)
C
C
C
C
C      CALCULATE ORIGIN AND CYCLE FOR LOG AXES
C               (LEVEL 1-3)
C
C      INPUT:   ALOW   = LOWER LIMIT OF DATA
C               AHI    = HIGHER LIMIT OF DATA
C               AXIS   = AXIS LENGTH
C
C      OUTPUT:  ORIGIN = CALCULATED LOWER LIMIT OF DATA
C               CYCLE  = CYCLE LENGTH IN INCHES
C
      common /CUNIT/  ZZUNIT
      common /CLEVEL/ KZLEVL,KZBEGN
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        RAXIS=AXIS*ZZUNIT
C  CXSIZE MAY NOT BE INITIALIZED
C       MAXTK=MIN0(10,INT(RAXIS/((ILABSZ()+1.0)*CXSIZE)))
        MAXTK=MIN0(10,INT(RAXIS/((ILABSZ()+1.0)*GSLENS('0'))))
        call LAXIS(ALOW,AHI,MAXTK,BMIN,BMAX,BTICK)
        ORIGIN=BMIN
        DELTA=BMAX-BMIN
        if(DELTA.LT.0.00005)then
          CYCLE=-1.0
          call ERRMES('SCALOG',0,4)
        else
          CYCLE=AXIS/DELTA
        endif
      else
        call ERRMES('SCALOG',1,3)
      endif
      return
      end
      subroutine SEGMNT(ICI,ICJ,ISEG)
      common /CONTR/ CLEVEL,IOLD,JOLD,IN,JN,
     1   NX,NY,XL,DX,YL,DY
      ICI=MIN0(IOLD,IN)
      ICJ=MIN0(JOLD,JN)
      ISEG=1
      if(IOLD .EQ. IN) ISEG=2
      return
      end
      subroutine CLRSET(ICOLOR)
C
C     SET CURRENT PLOTTING COLOR
C              (LEVEL 1-3)
C
C     INPUT:   ICOLOR = COLOR NAME, 'WHIT', 'RED', ETC.
C              IN integer FORMAT
C
      parameter (KZMAXC=255)
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      common /CLEVEL/ KZLEVL,KZBEGN
      common /COLORN/ KZSHD,ZZHSPC,ZZCOLR(KZMAXC,3),KZNCLR,KZCCOL
      common /COLORC/ CZCOLR
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      character CZCOLR*8
      character*(NUMBYT) LCOLOR
C
      SAVE ONE,ZERO
      data ONE,ZERO /1.0,0.0/
      JCOLOR=ICOLOR
      call WIN2CH(JCOLOR,LCOLOR)
      CZCOLR=LCOLOR
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
C
C                        ACTUALLY SETTING COLORS FOR TK4115
C                        AND TK4510
C
        if(KSYAND(IDVBTS,32) .NE. 0)then
          if(CZCOLR(1:3).EQ.'RED')then
              call HRDHSI(1.0,ONE,ONE)
          elseif(CZCOLR.EQ.'GREE')then
            call HRDHSI(2.0,ONE,ONE)
          elseif(CZCOLR.EQ.'BLUE')then
            call HRDHSI(3.0,ONE,ONE)
          elseif(CZCOLR.EQ.'YELL')then
            call HRDHSI(1.5,ONE,ONE)
          elseif(CZCOLR.EQ.'MAGE')then
            call HRDHSI(0.5,ONE,ONE)
          elseif(CZCOLR.EQ.'CYAN')then
            call HRDHSI(2.5,ONE,ONE)
          elseif(CZCOLR.EQ.'BLAC'.OR.CZCOLR.EQ.'BACK')then
            call HRDHSI(ZERO,ONE,ZERO)
          elseif(CZCOLR.EQ.'WHIT'.OR.CZCOLR.EQ.'FORE')then
              call HRDHSI(4.0,ZERO,ONE)
          endif
        else
C
C  SELECT COLOR FROM EXISTING COLOR TABLE
C
          if(CZCOLR(1:3).EQ.'RED')then
            call GSCOLR(2,IERR)
          elseif(CZCOLR.EQ.'GREE')then
            call GSCOLR(3,IERR)
          elseif(CZCOLR.EQ.'BLUE')then
            call GSCOLR(4,IERR)
          elseif(CZCOLR.EQ.'YELL')then
            call GSCOLR(5,IERR)
          elseif(CZCOLR.EQ.'MAGE')then
            call GSCOLR(6,IERR)
          elseif(CZCOLR.EQ.'CYAN')then
              call GSCOLR(7,IERR)
          elseif(CZCOLR.EQ.'BLAC'.OR.CZCOLR.EQ.'BACK')then
            call GSCOLR(0,IERR)
          elseif(CZCOLR.EQ.'WHIT'.OR.CZCOLR.EQ.'FORE')then
              call GSCOLR(1,IERR)
          endif
        endif
      else
        call ERRMES('CLRSET',1,3)
      endif
      return
      end
      subroutine SETOUT(IERR,ISUM)
C
C
C
C     SET OUTPUT DEVICE
C              (LEVEL 1-3, P/S)
C
C     INPUT:   IERR = OUTPUT UNIT FOR ERROR MESSAGES
C              ISUM = OUTPUT UNIT FOR PLOT SUMMARY
C
cw      common /CLEVEL/ KZLEVL,KZBEGN
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
C
cw      if(KZLEVL.GE.0.AND.KZLEVL.LE.3)then
        KZERR=IERR
        KZSUM=ISUM
cw      else
cw        call ERRMES('SETOUT',1,3)
cw      endif
      return
      end
      subroutine SETSUB(AX,AY)
C
C
C
C
C      DEFINE A SUBPLOT AREA AND SYSTEM parameterS,
C               AND DRAW FRAME
C               (LEVEL 1, RAISED TO 2)
C
C      INPUT:   AX,AY = LENGTHS OF X AND Y AXES
C
      parameter (KZYES=111)
      parameter (KZNO=222)
      parameter (KZCLIP=12)
      parameter (KZDOWN=13)
      parameter (KZSCRN=14)
      parameter (KZABRT=15)
      parameter (ZZIN=2.54)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /CSYMBO/ KZSYM,KZNSYM,ZZSMSZ,UUSMSZ
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      common /CUNIT/  ZZUNIT
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CBORCH/ KZBRDR,KZCHEK
      common /CIOUNT/ KZIUNT, KZOUNT
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
C
      SAVE KQMS,KLN03,KPOST
      data KQMS,KLN03,KPOST /1200,3,910/
C
C                        CHECK LEVEL
C
      if(KZLEVL.EQ.1)then
C
C                        ROTATE PLOT IF DESIRED
C
        KDEV=INT(DEVID)
        if((KDEV.EQ.KQMS.OR.KDEV.EQ.KLN03.OR.KDEV.EQ.KPOST)
     *         .AND.KZAUTO.EQ.KZYES)then
          if(UUPAGX.LT.UUPAGY)then
            call ZLASAP
          else
            call ZLASAL
          endif
        endif
C
C                        ERASE SCREEN FOR NEW PLOT
C
        if(KZBEGN.NE.KZYES)then
C          call GSDRVR(2,DUMMY,DUMMY)
          KZBEGN=KZYES
        endif
C
C                        CONVERT TO SYSTEM UNIT
C
        RX=XLENCM/UUPAGX
        RY=YLENCM/UUPAGY
        if(RX.LT.RY)then
          ZZPAGR=RX
        else
          ZZPAGR=RY
        endif
        if(KZSCAL.EQ.KZDOWN)then
          if(ZZPAGR.GT.1.0)then
            ZZPAGR=1.0
          endif
          ZZPAGX=UUPAGX*ZZPAGR
          ZZPAGY=UUPAGY*ZZPAGR
        elseif(KZSCAL.EQ.KZSCRN)then
          ZZPAGX=UUPAGX*ZZPAGR
          ZZPAGY=UUPAGY*ZZPAGR
        elseif(KZSCAL.EQ.KZCLIP)then
          ZZPAGX=UUPAGX
          ZZPAGY=UUPAGY
          ZZPAGR=1.0
        elseif(KZSCAL.EQ.KZABRT)then
          if(RX.LT.1.0.OR.RY.LT.1.0)then
            KZLEVL=1
            return
          else
            ZZPAGX=UUPAGX
            ZZPAGY=UUPAGY
            ZZPAGR=1.0
          endif
        endif
        if(KZLTHK.EQ.KZYES) ZZLTHK=UULTHK*ZZPAGR
        ZZSMSZ=UUSMSZ*ZZPAGR
        ZZGRCE=UUGRCE*ZZPAGR
        ZZHITE=UUHITE*ZZPAGR
        call GSSETC(ZZHITE,ZZANGL)
CW        ZZLGTZ=UULGTZ*ZZPAGR
        ZZSMSZ=UUSMSZ*ZZPAGR
        UUYAXS=AY*ZZUNIT
        UUXAXS=AX*ZZUNIT
        ZZXAXS=UUXAXS*ZZPAGR
        ZZYAXS=UUYAXS*ZZPAGR
        ZZFRME=UUFRME*ZZPAGR
C
C                        CHECK IF 'ORIGIN' IS CALLED
C
        if(KZOR.NE.KZYES)then
          DX=UUPAGX-UUXAXS
          DY=UUPAGY-UUYAXS
          if(DX.GT.1.0)then
            UUXOR=DX/2.0
          else
            UUXOR=0.5*ZZIN
            if(DX.LT.0.5)then
              call ERRMES('SETSUB',0,4)
            endif
          endif
          if(DY.GT.1.0)then
            UUYOR=DY/2.0
          else
            UUYOR=0.5*ZZIN
            if(DY.LT.0.5)then
              call ERRMES('SETSUB',0,5)
            endif
          endif
        endif
        ZZXOR=UUXOR*ZZPAGR
        ZZYOR=UUYOR*ZZPAGR
C
C                        CALCULATE FRAME
C
        PXMIN=(XLENCM-ZZPAGX)/2.0
        PYMIN=(YLENCM-ZZPAGY)/2.0
        PXMAX=PXMIN+ZZPAGX
        PYMAX=PYMIN+ZZPAGY
        XCM0=PXMIN
        XCM1=PXMAX
        YCM0=PYMIN
        YCM1=PYMAX
C
C                        CHECK IF 'NOBORD' IS CALLED
C
        if(KZBRDR.NE.KZNO)then
          call DSMOVE(PXMIN,PYMIN)
          call DSDRAW(PXMIN,PYMAX)
          call DSDRAW(PXMAX,PYMAX)
          call DSDRAW(PXMAX,PYMIN)
          call DSDRAW(PXMIN,PYMIN)
        endif
C
C                        SET PLOT AREA parameterS
C
        ZZXLFT=PXMIN+ZZXOR
        ZZXAXR=(PXMAX-ZZXLFT)/ZZXAXS
        if(ZZXAXR.GT.1.0)then
          ZZXAXR=1.0
          ZZXRGT=ZZXLFT+ZZXAXS
        else
          ZZXRGT=PXMAX
        endif
        ZZYBOT=PYMIN+ZZYOR
        ZZYAXR=(PYMAX-ZZYBOT)/ZZYAXS
        if(ZZYAXR.GT.1.0)then
          ZZYAXR=1.0
          ZZYTOP=ZZYBOT+ZZYAXS
        else
          ZZYTOP=PYMAX
        endif
        TICK=0.6*ZZHITE
        call ZMAPRM(ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,ZZHITE,TICK)
        KZLEVL=2
      else
        call ERRMES('SETSUB',1,0)
      endif
      return
      end
      subroutine SETUNT(SCALE)
C
C
C
C
C     SET INPUT UNIT CONVERTING SCALE
C              (LEVEL 1-3, P/S)
C
C     INPUT:   SCALE = SCALE FLAG
C                      IF = 'IN', IN INCHES, DEFAULT
C                      IF = 'CM' OR 'CENT', CENTIMETER
C                      IF = 'MM' OR 'MILL',MILLIMETER
C
      parameter (ZZIN=2.54)
      parameter (ZZCM=1.00)
      parameter (ZZMM=10.0)
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      common /CUNIT/  ZZUNIT
C
C:  CHANGED TO ACCOMMADATE AN 8 BYTE WORD
C     character*4 CSCALE
C     EQUIVALENCE (RSCALE,CSCALE)
      character*(NUMBYT) CSCALE
C
      RSCALE=SCALE
C:  CHANGED TO ELIMINATE EQUIVALENCING PROBLEM
      call WIN2CH(RSCALE,CSCALE)
C
      call CAPCHR(CSCALE)
      if(CSCALE(1:2).EQ.'IN')then
        ZZUNIT=ZZIN
      elseif(CSCALE(1:2).EQ.'CM'.OR.CSCALE.EQ.'CENT')then
        ZZUNIT=ZZCM
      elseif(CSCALE(1:2).EQ.'MM'.OR.CSCALE.EQ.'MILL')then
        ZZUNIT=ZZMM
      else
        ZZUNIT=SCALE
      endif
      return
      end
      subroutine SHADE(X,Y,NPTS,ANGLE,GARAY,NGAPS,NN1,NN2)
C
C
C
C
C     POLYGON FILL
C              LEVEL 3
C
C     INPUT:   X,Y     = X AND Y VALUES OF POLYGON VERTICES
C              NPTS    = NUMBER OF X Y PAIRS IN X AND Y
C              ANGLE   = CROSS HATCHING LINE ANGLE
C              GARAY   = GAP ARRAY, ONLY ONE VALUE IS USED
C                        IN DISSIM
C              NGAPS   = NUMBER OF VALUES IN GARAY, NOT
C                        USED IN DISSIM
C              NN1,NN2 = NOT USED IN DISSIM
C
      parameter (KZYES=111)
      parameter (KZMAXC=255)
      parameter (MAXPTS=1000)
      parameter (MAX1=MAXPTS+1)
      parameter (MAX2=MAXPTS+2)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /COLORN/ KZSHD,ZZHSPC,ZZCOLR(KZMAXC,3),KZNCLR,KZCCOL
      common /COLORC/ CZCOLR
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      character CZCOLR*8
C
      DIMENSION X(*),Y(*),XX(MAX2),YY(MAX2),TX(MAX2),TY(MAX2)
      if(KZLEVL.EQ.3)then
C
C                        IF HARDWARE SHADE IS TURNED ON
C
        if(KZSHD.EQ.KZYES)then
          NROUND=NPTS/MAXPTS
C
C                        IF NUMBER OF POINTS IS GREATER THAN
C                        'MAXPTS', DO IT PIECE BY PIECE
C
          if(NROUND.GT.0)then
            J1=1-MAXPTS
            call SCALE(X(1),Y(1),XX(MAX1),YY(MAX1))
            DO 200 J=1,NROUND
              J1=J1+MAXPTS
              J2=J1+MAXPTS-1
              J11=J1-1
              DO 300 K=J1,J2
                K1=K-J11
                call SCALE(X(K),Y(K),XX(K1),YY(K1))
 300              continue
              if(J1.LE.1)then
                call DSFILL(XX,YY,MAXPTS,TX,TY)
              else
                XX(MAX2)=XX(J1-1)
                YY(MAX2)=YY(J1-1)
                call DSFILL(XX,YY,MAX2,TX,TY)
              endif
 200            continue
            if(J2.LT.NPTS)then
              XX(1)=XX(MAX1)
              YY(1)=YY(MAX1)
              XX(2)=XX(MAXPTS)
              YY(2)=YY(MAXPTS)
            J3=J2-2
              DO 400 I=J2+1,NPTS
                I1=I-J3
                call SCALE(X(I),Y(I),XX(I1),YY(I1))
 400              continue
              call DSFILL(XX,YY,NPTS-J3,TX,TY)
            endif
C
C                        IF NUMBER OF POINTS IS LESS OR EQUAL
C                        TO 'MAXPTS' DO IT IN ONE PIECE
C
          else
            DO 100 I=1,NPTS
              call SCALE(X(I),Y(I),XX(I),YY(I))
 100            continue
            call DSFILL(XX,YY,NPTS,TX,TY)
          endif
C
C                        HARDWARE SHADE NOT TURNED ON, DO
C                        SOFTWARE FILL
C
        else
          if(NPTS.LE.MAX2)then
            GAP=GARAY*ZZUNIT*ZZPAGR
            call DHATCH(X,Y,NPTS,ANGLE,GAP,2,TX,TY)
          else
            call ERRMES('SHADE',0,4)
          endif
        endif
      else
        call ERRMES('SHADE',3,0)
      endif
      return
      end
      subroutine SIMPLX
C
C
C
C      SET character TYPE TO TRIPLEX
C               LEVEL 1-3, P/S
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFS='SIMPL'
      else
        call ERRMES('SIMPLX',1,3)
      endif
      return
      end
      FUNCTION SLNGTH(LMESS,IMESS)
C
C     return LENGTH OF STRING IN INCHES
C              (LEVEL 1-3)
C
C     INPUT:   LMESS = character STRING
C              IMESS = NUMBER OF character IN LMESS
C
C     OUTPUT:  SLNGTH = LENGTH OF INPUT STRING IN INCHES
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CUNIT/  ZZUNIT
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      character*(*) LMESS(*)
      DIMENSION LMESSA(38)
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
C
C  IF LENGTH EXCLUDING TRAILING BLANKS IS DESIRED
C
        IF(IMESS.LT.0)THEN
          call ZSTRBT(LMESS)
        endif
C
C  CALL ROUTINE TO CALCULATE SLNGTH
C
        SLNGTH=ZXMESS(LMESS,ABS(IMESS),CZALFL,CZALFN,CZALFS)
        if(KZLEVL.EQ.1)then
          SLNGTH=SLNGTH/ZZUNIT
        else
          SLNGTH=SLNGTH/ZZUNIT/ZZPAGR
        endif
        LENGTH=LENG(LMESSA)
C
C  IF INPUT IS IN character FORMAT
C
        if(LENGTH.EQ.1.AND.LENGTH.NE.IMESS)then
          SLNGTH=SLNGTH*REAL(IMESS)
        endif
C
C  WRONG LEVEL
C
      else
        SLNGTH=0.0
        call ERRMES('SLNGTH',1,3)
      endif
c
      return
      end
      subroutine STOPLT(IPLOT)
c
c***********************************************************************
c
C     TERMINATE CURRENT PLOT
C              (LEVEL 2,3, CHANGE TO 1)
C
C     INPUT:   IPLOT = NOT USED IN DISSIM
C
      parameter (KZYES=111)
c
      save cdevic
      save ciount
      save gcdchr
c
      common /CIOUNT/ KZIUNT, KZOUNT
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      DUMMY = 0.0
C
C     DETERMINE AUTO-COPY OPTION SETTING.
C
      if(KZCOPY.EQ.KZYES) DUMMY = 1.0
      call GSDRVR(5,DUMMY,DUMMY)
C
      KZNPLT=KZNPLT+1
      call ZINIT
      return
      end
      subroutine SYAXIS(YLOW,YHIGH,YLAB,IAXES)
      LOGICAL LOGX, LOGY
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      common /PLTCLP/ XMIN,XMAX,YMIN,YMAX
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI
C
C      EXTERNAL LEN
      character YLAB*2
      character NUMBR*14
      character NEQUIV(14)
      EQUIVALENCE(NUMBR,NEQUIV)
      LOGICAL LOGYY, LOGT, LRMTEX, LSHORT, LRAGGD
      DIMENSION ZLOG(8)
C
      SAVE ZLOG,TMINLD,SHORTF
C
      data ZLOG /0.3010, 0.4771, 0.6021, 0.6990, 0.7782, 0.8451,
     1   0.9031, 0.9542 /
      data TMINLD /0.1/
      data SHORTF /2.0/
C
      DELMX=0
      YVLEN = YVINI
C
C
C     SET LOGY TO FALSE FOR OUR USAGE OF SCALE
C
      LOGY = .FALSE.
C
C     SEE WHAT TYPE OF AXIS IS DESIRED
C
      LOGYY = KSYAND(IAXES,2) .NE. 0
      LRAGGD = KSYAND(IAXES,256) .NE. 0
C
C     DO THE AXES SCALING
C
      NUMTK = MIN0(10,INT(YVLEN/(3.0*CYSIZE)))
      if(LOGYY) go to 60
      LSHORT = KSYAND(IAXES,32) .NE. 0
      call AXIS(YLOW,YHIGH,NUMTK,LSHORT,LRAGGD,YMIN,YMAX,YTMIN,YTMAX,
     1   YTICK,IYPWR)
      go to 80
60    CALL LAXIS(YLOW,YHIGH,NUMTK,YMIN,YMAX,YTICK)
      YTMIN = YMIN
      YTMAX = YMAX
      IYPWR = 0
80    continue
C
C     SET UP TEMPORARY SCALING FACTORS
C
      UY0 = YMIN
      UDY = YMAX - YMIN
C
C     ********** DRAW Y AXES **********
C
      call GSSETC(CYSIZE,0.0)
      LOGT = .FALSE.
      if(.NOT. LOGYY .OR. YTICK .NE. 1.0) go to 90
      call SCALE(XMIN,YMIN,VX,TEMP)
      call SCALE(XMIN,YMIN+1.0-ZLOG(8),VX,VY)
      if((VY-TEMP) .GE. TMINLD) LOGT = .TRUE.
90    continue
C
C     DRAW Y AXIS LINE
C
      TENEXP = 10.0**IYPWR
      X = XMAX
C      TICKSP = AMAX1(0.0,TICKLN) @TICK SPACING
      TICKSP = AMAX1(0.0,TICKLN)
      if(KSYAND(IAXES,64) .NE. 0) YVLEN = YVLEN - TICKSP
      TCKSGN = -TICKLN
100   continue
      call SCALE(X,YMAX,VX,VY)
      call GSMOVE(VX,VY)
      call SCALE(X,YMIN,VX,VY)
      call GSDRAW(VX,VY)
C
C     DRAW AND LABEL Y AXIS TICKS
C
      Y = YTMIN
      N = (YTMAX-YTMIN)/YTICK + 1.1
110   continue
      call SCALE(X,Y*TENEXP,VX,VY)
      call GSMOVE(VX,VY)
      call GSDRAW(VX+TICKLN,VY)
C
C     PLACE THE APPROPIATE LABEL
C
      if(KSYAND(IAXES,1024) .NE. 0) go to 183
      if(LOGYY) go to 160
      call LINLAB(INT(Y),IYPWR,NUMBR,LRMTEX)
      go to 180
160   CALL LOGLAB(INT(Y),NUMBR)
180   DEL = GSLENS(NUMBR)
      DELMX = AMAX1(DEL,DELMX)
      call GSMOVE(VX+TICKSP+0.5*CXSIZE,VY-CYSIZE/2.0)
      call GSPSTR(NUMBR)
183   continue
C
C     ADD GRID LINE AT TICK IF DESIRED
C
      if(KSYAND(IAXES,8) .EQ. 0) go to 185
      call GSLTYP(3)
      call GSMOVE(VX,VY)
      call SCALE(XMIN,Y*TENEXP,VX,VY)
      call GSDRAW(VX,VY)
      call GSLTYP(1)
185   continue
C
C     DO EXTRA TICKING IF EXTRA TICKS WILL BE FAR ENOUGH APART
C
      if((.NOT. LOGT) .OR. (Y .EQ. YTMAX)) go to 200
      DO 190 J = 1, 8
      call SCALE(X,Y+ZLOG(J),VX,VY)
      call GSMOVE(VX,VY)
190   CALL GSDRAW(VX+TICKLN/SHORTF,VY)
200   continue
      Y = Y + YTICK
      N = N-1
      if(N .GT. 0) go to 110
C
C     IF LINEAR AXIS, PLACE REMOTE EXPONENT IF NEEDED
C
      if(LOGYY .OR. (.NOT. LRMTEX)) go to 260
      if(KSYAND(IAXES,1024) .NE. 0) go to 260
      call SCALE(XMAX,(YTMIN+YTICK/2.0)*TENEXP,VX,VY)
      call ZSCOPY('E',NUMBR)
      call NUMSTR(IYPWR,NEQUIV(2))
      call GSMOVE(VX+0.5*CXSIZE,VY-CYSIZE/2.0)
      call GSPSTR(NUMBR)
C
C     NOW PLACE Y LABEL
C
260   CALL SCALE(X,(YMIN+YMAX)/2.0,VX,VY)
      call GSMOVE(VX+0.5*CXSIZE+DELMX+TICKSP+1.5*CYSIZE,
     1   VY-GSLENS(YLAB)/2.0)
      call GSSETC(CYSIZE,90.0)
      call GSPSTR(YLAB)
      call GSSETC(CYSIZE,0.0)
300   continue
C
C     TELL USER THE SCALING LIMITS
C
      if(.NOT. LOGYY) go to 320
       YMIN = 10.0**YMIN
       YMAX = 10.0**YMAX
320   continue
C
C  TELL SCALE ABOUT LOG AXIS SCALING NOW
C
      LOGY = LOGYY
      return
      end
      integer FUNCTION SYBOOL(ITEM,JTEM)
C
      SYBOOL=0
C
C                        ENTRY POINT FOR LOGICAL 'AND'
C
      ENTRY KSYAND(IVAL1,IVAL2)
C-----------------------BEGIN CRAY SPECIFIC CODE------------------------
      KSYAND=IVAL1.AND.IVAL2
C------------------------END CRAY SPECIFIC CODE-------------------------
C---------------------BEGIN VAX VMS SPECIFIC CODE-----------------------
C     KSYAND=IAND(IVAL1,IVAL2)
C----------------------END VAX VMS SPECIFIC CODE------------------------
C---------------------BEGIN CONVEX SPECIFIC CODE------------------------
C     KSYAND=IAND(IVAL1,IVAL2)
C----------------------END CONVEX SPECIFIC CODE-------------------------
C--------------------BEGIN ALLIANT SPECIFIC CODE------------------------
C     KSYAND=IAND(IVAL1,IVAL2)
C---------------------END ALLIANT SPECIFIC CODE-------------------------
C--------------------BEGIN SGI IRIS SPECIFIC CODE-----------------------
C     KSYAND=IAND(IVAL1,IVAL2)
C---------------------END SGI IRIS SPECIFIC CODE------------------------
C-------------------------BEGIN GENERIC CODE----------------------------
C     KSYAND=AND(IVAL1,IVAL2)
C--------------------------END GENERIC CODE-----------------------------
      return
C
C                        ENTRY POINT FOR LOGICAL 'OR'
C
      ENTRY KSYOR(IVAL1,IVAL2)
C-----------------------BEGIN CRAY SPECIFIC CODE------------------------
      KSYOR=IVAL1.OR.IVAL2
C------------------------END CRAY SPECIFIC CODE-------------------------
C---------------------BEGIN VAX VMS SPECIFIC CODE-----------------------
C     KSYOR=IOR(IVAL1,IVAL2)
C----------------------END VAX VMS SPECIFIC CODE------------------------
C---------------------BEGIN CONVEX SPECIFIC CODE------------------------
C     KSYOR=IOR(IVAL1,IVAL2)
C----------------------END CONVEX SPECIFIC CODE-------------------------
C--------------------BEGIN ALLIANT SPECIFIC CODE------------------------
C     KSYOR=IOR(IVAL1,IVAL2)
C---------------------END ALLIANT SPECIFIC CODE-------------------------
C--------------------BEGIN SGI IRIS SPECIFIC CODE-----------------------
C     KSYOR=IOR(IVAL1,IVAL2)
C---------------------END SGI IRIS SPECIFIC CODE------------------------
C-------------------------BEGIN GENERIC CODE----------------------------
C     KSYOR=OR(IVAL1,IVAL2)
C--------------------------END GENERIC CODE-----------------------------
      return
C
C                        ENTRY POINT FOR LOGICAL 'XOR'
C
      ENTRY KSYXOR(ITEM,JTEM)
C---------------------BEGIN VAX VMS SPECIFIC CODE-----------------------
C     KSYXOR=IEOR(ITEM,JTEM)
C----------------------END VAX VMS SPECIFIC CODE------------------------
C---------------------BEGIN CRAY SPECIFIC CODE--------------------------
      KSYXOR=IEOR(ITEM,JTEM)
C----------------------END CRAY SPECIFIC CODE---------------------------
C-------------------------BEGIN GENERIC CODE----------------------------
C     KSYXOR=XOR(ITEM,JTEM)
C--------------------------END GENERIC CODE-----------------------------
      return
C
C                        ENTRY POINT FOR LOGICAL SHIFT LEFT
C
      ENTRY KSYSHL(ITEM,JTEM)
C-----------------------BEGIN CRAY SPECIFIC CODE------------------------
      KSYSHL=ISHFT(ITEM,JTEM)
C------------------------END CRAY SPECIFIC CODE-------------------------
C---------------------BEGIN VAX VMS SPECIFIC CODE-----------------------
C     KSYSHL=ISHFT(ITEM,JTEM)
C----------------------END VAX VMS SPECIFIC CODE------------------------
C---------------------BEGIN CONVEX SPECIFIC CODE------------------------
C     KSYSHL=ISHFT(ITEM,JTEM)
C----------------------END CONVEX SPECIFIC CODE-------------------------
C--------------------BEGIN ALLIANT SPECIFIC CODE------------------------
C     KSYSHL=ISHFT(ITEM,JTEM)
C---------------------END ALLIANT SPECIFIC CODE-------------------------
C--------------------BEGIN SGI IRIS SPECIFIC CODE-----------------------
C     KSYSHL=ISHFT(ITEM,JTEM)
C---------------------END SGI IRIS SPECIFIC CODE------------------------
C-------------------------BEGIN GENERIC CODE----------------------------
C     KSYSHL=LSHIFT(ITEM,JTEM)
C--------------------------END GENERIC CODE-----------------------------
      return
C
C                        ENTRY POINT FOR LOGICAL SHIFT RIGHT
C
      ENTRY KSYSHR(ITEM,JTEM)
C-----------------------BEGIN CRAY SPECIFIC CODE------------------------
      KSYSHR=ISHFTC(ITEM,64-JTEM,64)
C------------------------END CRAY SPECIFIC CODE-------------------------
C---------------------BEGIN VAX VMS SPECIFIC CODE-----------------------
C     KSYSHR=ISHFTC(ITEM,32-JTEM,32)
C----------------------END VAX VMS SPECIFIC CODE------------------------
C---------------------BEGIN CONVEX SPECIFIC CODE------------------------
C     KSYSHR=ISHFTC(ITEM,32-JTEM,32)
C----------------------END CONVEX SPECIFIC CODE-------------------------
C--------------------BEGIN ALLIANT SPECIFIC CODE------------------------
C     KSYSHR=ISHFTC(ITEM,32-JTEM,32)
C---------------------END ALLIANT SPECIFIC CODE-------------------------
C--------------------BEGIN SGI IRIS SPECIFIC CODE-----------------------
C     KSYSHR=ISHFT(ITEM,-JTEM)
C---------------------END SGI IRIS SPECIFIC CODE------------------------
C-------------------------BEGIN GENERIC CODE----------------------------
C     KSYSHR=RSHIFT(ITEM,JTEM)
C--------------------------END GENERIC CODE-----------------------------
      return
      end
      subroutine SYMBOL(ISYMNO,SYMSIZ)
C
C PLACE THE DESIRED SYMBOL ('ISYMNO') AT CURRENT LOCATION
C WITH A SIZE OF SYMSIZ.
C
C INTERNAL VARIABLES:
C   SYMMOV     (NO. OF MOVES) ARRAY OF CONSECUTIVE X,Y LOCATIONS TO
C              WHICH LINE IS DRAWN
C   ISYMST     (NO. OF SYMBOLS + 1) ARRAY START OF SYMBOL MOVE LOCATIONS
C
C CJW 7/86 NEW SYMBOL DEFINITIONS:
C        1>  SQUARE
C        2>  OCTAGON
C        3>  TRIANGLE
C        4>  PLUS SIGN
C        5>  'X'
C        6>  DIAMOND
C        7>  UPSIDE DOWN TRIANGLE
C        8>  SQUARE WITH AN 'X' IN IT
C        9>  'X' WITH A HORIZONTAL LINE ACROSS IT
C       10>  DIAMOND WITH A PLUS SIGN IN IT
C       11>  OCTAGON WITH A PLUS SIGN IN IT
C       12>  DOUBLE HOUR GLASS
C       13>  SQUARE WITH A PLUS SIGN IN IT
C       14>  OCTAGON WITH A 'X' IN IT
C       15>  SQUARE WITH A TRIANGLE IN IT
C       16>  PENTAGON WITH A PLUS SIGN IN IT
C       17>  PENTAGON
C       18>  FIVE POINTED STAR
C       19>  SQUARE WITH A DIAMOND IN IT
C
      common /GCVPOS/ XVPOS, YVPOS
      DIMENSION SYMMOV(105),ISYMST(1:19,4)
C
      SAVE SYMMOV,ISYMST,NSYM
C
C  SQUARE: (1)
C  OCTAGON: (12)
C  TRIANGLE: (31)
C  UPSIDE DOWN TRIANGLE: (40)
C  HORIZONTAL LINE: (49)
C  VERTICAL LINE: (54)
C  REVERSED SLASH: (59)
C  SLASH: (64)
C  DIAMOND: (69)
C  STAR: (80)
C  PENTAGON: (93)
C
      data SYMMOV/
     1 0.5,-0.5, 0.5,0.5,  -0.5,0.5,  -0.5,-0.5,  0.5,-0.5,  1000.0,
     2 0.2071,-0.5, 0.5,-0.2071, 0.5,0.2071, 0.2071,0.5, -0.2071,0.5,
     & -0.5,0.2071, -0.5,-0.2071, -0.2071,-0.5, 0.2071,-0.5, 1000.0,
     3 0.5,-0.5,  0.0,0.5,  -0.5,-0.5,  0.5,-0.5, 1000.0,
     4 0.5,0.5,   0.0,-0.5,  -0.5,0.5,  0.5,0.5, 1000.0,
     5 -0.5,0.0,  0.5,0.0,  1000.0,
     6 0.0,0.5,  0.0,-0.5,  1000.0,
     7 -0.5,0.5,  0.5,-0.5, 1000.0,
     8 -0.5,-0.5,  0.5,0.5,  1000.0,
     9 0.5,0.0,  0.0,0.5,  -0.5,0.0,  0.0,-0.5,  0.5,0.0,  1000.0,
     A 0.5,0.2, -0.5,0.2, 0.3,-0.5, 0.0,0.5, -0.3,-0.5,
     & 0.5,0.2, 1000.0,
     B 0.5,0.0,  0.0,0.5,  -0.5,0.0,  -0.25,-0.5,  0.25,-0.5,
     & 0.5,0.0, 1000.0/
      data ISYMST/
     &  1,12,31,54,64,69,40,59,49,69,12,40,49,12, 1,93,93,80,69,
     &  0, 0, 0,49,59, 0, 0,64,64,54,54,31,54,64,31,54, 0, 0, 1,
     &  0, 0, 0, 0, 0, 0, 0, 1,59,49,49, 0, 1,59, 0,49, 0, 0, 0,
     &  1, 1, 1, 2, 2, 1, 1, 3, 3, 3, 3, 2, 3, 3, 2, 3, 1, 1, 2/
      data NSYM /19/
C
C                                           SAVE CURRENT LOCATION
C
        X0 = XVPOS
        Y0 = YVPOS
C
C                                           DRAW SYMBOL IN PROPER SIZE
C
C     PRINT 13,ISYMNO,SYMSIZ,X0,Y0
C13    FORMAT(' SYMNO=',I6,'  SYM SIZ=',F10.4,' POINT=',2F10.4)
        if(ISYMNO.GE.1  .AND.  ISYMNO.LE.NSYM)then
          DO 200 LINE=1,ISYMST(ISYMNO,4)
          IPTR = ISYMST (ISYMNO,LINE)
            call GSMOVE(X0+SYMSIZ*SYMMOV(IPTR),
     *           Y0+SYMSIZ*SYMMOV(IPTR+1))
C     PRINT 11,SYMMOV(IPTR),SYMMOV(IPTR+1),IPTR
C11    FORMAT(' MOVETO ',2F10.4,I5)
C12    FORMAT('            DRAWTO ',2F10.4,I5)
          DO 100 I=1,12
            IPTR=IPTR+2
              if(SYMMOV(IPTR).GT.999.0) go to 200
C     PRINT 12,SYMMOV(IPTR),SYMMOV(IPTR+1),IPTR
              call GSDRAW(X0+SYMSIZ*SYMMOV(IPTR),
     *                    Y0+SYMSIZ*SYMMOV(IPTR+1))
  100       continue
  200     continue
      endif
        call GSMOVE(X0,Y0)
C      call ENDPLT
C     PRINT *,'  END SYMBOL'
        return
        end
      subroutine SYSTEM2(FUNC,STR1)
C
C     THIS ROUTINE IS USED TO return THE CURRENT HOST
C     SYSTEM INFORMATION OR PERFORM SYSTEM CALLS.
C
C     INSTALLATION NOTES:
C          1) THIS IS A SYSTEM SPECIFIC ROUTINE. ALL STATEMENTS
C             FOR CURRENT SYSTEM SHOULD BE UN-COMMENTED AT THE
C             TIME OF INSTALLATION. HINT: SAVE A BACKUP COPY AND
C             USE GLOBAL SUBSTITUTE.
C          2) IF YOU CANNOT FIND ANY STATEMENT FOR YOUR SYSTEM,
C             UN-COMMENT THE 'UNKNOWN' ONES. OR YOU CAN ADD THE
C             PART FOR YOUR SYSTEM. WE DO APPRECIATE IT IF YOU
C             COULD SEND YOUR MODIFIED VERSION TO EITHER
C             J.E. ROGERS/ES2, JOHNSON SPACE CENTER, HOUSTON,
C             TEXAS 77058 OR C.J. WONG/B14, LOCKHEED EMSCO,
C             P.O. BOX 58561, HOUSTON, TEXAS 77258. WE WILL
C             INCLUDE IT IN THE NEXT RELEASE.
C
C     SYSTEM ID - return SYSTEM TYPE
C          callING SEQUENCE: CALL SYSTEM2('SYSTEM',STR1)
C          INPUT:  FUNC = 'SYSTEM'
C          OUTPUT: STR1 - SYSTEM TYPE, character*6
C
C     CURRENT DATE - return CURRENT DATE
C          callING SEQUENCE: CALL SYSTEM2('DATE',STR1)
C          INPUT:  FUNC = 'DATE'
C          OUTPUT: STR1 - CURRENT DATE, character*8
C
C     SYBYT4 - EXTRACT OR INSERT character FROM integer ARRAY
C          callING SEQUENCE: CALL SYBYT4(CFLAG,INP,IPOS,JCHAR)
C          INPUT:  CFLAG = OPERATION IDENTIFIER
C                          IF = 'X', TO EXTRACT
C                          IF = 'N', TO INSERT
C                  INP   = integer ARRAY TO BE EXTRACTED FROM
C                  IPOS  = BYTE POSITION OF INTEREST
C                  JCHAR = ASCII EQUIVALENCE OF CHAR TO BE INSERTED
C          OUTPUT: INP   = MODIFIED integer ARRAY AFTER INSERTION
C                  JCHAR = ASCII EQUIVALENCE OF EXTRACTED CHAR
C
C     SYFCHK - PERFORM FILE NAME VERIFICATION
C          callING SEQUENCE: CALL SYFCHK(STR1,STR2)
C          INPUT:  STR1 - INPUT FILE NAME AND OUTPUT FILE NAME
C                  STR2 - FILE SUFFIX
C          THIS CHECKING CAN EITHER BE DUMMIED OR CAN CONTAIN
C          ANY TYPE OF FORMAT CHECKING DESIRED.
C          A) FOR THE VAX, THE FILE NAME IS CHECKED TO DETERMINE
C             IF AN EXTENSION HAS BEEN USED WITH THE NAME.  IF IT
C             HAS, THE NAME IS returnED INTACT; HOWEVER, IF NO
C             EXTENSION HAS BEEN SPECIFIED, THE ONE CONTAINED IN
C             THE ARGUMENT "SUFFIX" IS APPENDED TO THE NAME.
C
C     SYFNSH - CLOSE SYSTEM OUTPUT CHANNEL
C          callING SEQUENCE: CALL SYFNSH(RELSTR)
C          INPUT:  RELSTR = STRING REQUIRED TO CLOSE OUTPUT CHANNEL
C
C     SYINIT - INITIALIZE SYSTEM INPUT CHANNEL
C          callING SEQUENCE: CALL SYINIT(KHAR,ENDSTR,TTNAME,IERR)
C          INPUT:  KHAR   =
C                  endSTR = character STRING REQUIRED AT END OF
C                           EACH BUFFER DUMP
C                  TTNAME = DEVICE TO WHICH OUTPUT GOES
C                  IERR   = STATUS OF OPERATION
C
C     SYSJCL - ALLOW USER TO EXECUTE SYSTEM COMMANDS DURING PROGRAM
C              EXECUTION.
C           callING SEQUENCE: CALL SYSJCL(KOMAND)
C           INPUT:  KOMAND = A TEXT STRING OF SYSTEM COMMAND
C
C     SYSTTY - SEND BUFFER TO OUTPUT CHANNEL
C           callING SEQUENCE: CALL SYSTTY(TTYBUF,IBUFLN)
C           INPUT:  TTYBUF = TEXT TO BE OUTPUT
C                   IBUFLN = NUMBER OF character TO BE OUTPUT
C
C     SYSUSR - EXTRACT USER'S NAME or ID.
C           callING SEQUENCE: CALL SYSUSR(USRNAM)
C           INPUT:  NONE
C           OUTPUT: USRNAM = USER ID
C
C     SYWAIT - PERFORM SYSTEM WAIT
C          callING SEQUENCE: CALL SYWAIT(MILLI)
C          INPUT:  MILLI - NUMBER OF MILLISECONDS TO WAIT
C
C     SYBOOL - PERFORM LOGICAL FUNCTIONS
C          callING SEQUENCE: IVAL=KSYAND(ITEM,JTEM)
C                            IVAL=KSYOR(ITEM,JTEM)
C                            IVAL=KSYXOR(ITEM,JTEM)
C          INPUT:  ITEM,JTEM = integer VALUES
C          OUTPUT: KSYAND = RESULT OF LOGICAL 'AND'
C                  KSYOR  = RESULT OF LOGICAL 'OR'
C                  KSYXOR = RESULT OF LOGICAL 'EXCLUSIVE OR'
C          NOTE:
C            1. FOR SYSTEM OTHER THAN VAX/VMS OR PC USING
C               MICROSOFT FORTRAN, 2'S COMPLIMENT AND 32-BIT WORD
C               IS ASSUMED, I.E. SAME AS THAT OF VAX/VMS.
C
      parameter (IBFSIZ=79)
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      character*(*) STR1,STR2,KOMAND,USRNAM
      character*(*) RELSTR,KHAR,ENDSTR,TTNAME,TTYBUF
      character KODE*11,STR8*8,FUNC*4,DATE*8,TIME*8,CFLAG
      integer IWAIT(2),INP(*)
      LOGICAL LUTERM
      character BUFFER, NDSTNG, TRMCHR, BEGSTR
      common /GBBUFR/ IBFPTR, NDLNTH, IBEGLN,IOCHAN, LUTERM
      common /GBBUFC/ BUFFER(IBFSIZ),NDSTNG(10),BEGSTR(10),TRMCHR
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CBYTE4/ KZMASK(8),KZCPOS(8)
C
      SAVE MAXINT
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
C                        FOR 32 BIT WORD, 7F FF FF FF HEX
      data MAXINT/2147483647/
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     DATA MAXINT/1777777777777777777777B/
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
C
C  VAX/VMS DEVICE DRIVER CODE
C     integer*2 IOSB(4),HLFWRD(2)
C     integer*4 SYS$WAITFR,SYS$BINTIM,SYS$SETIMR,ITMLST(4)
C     integer*4 SYS$DASSGN,SYS$ASSIGN,SYS$QIOW
C     EQUIVALENCE(ITMLST,HLFWRD)
C     INCLUDE '($IODEF)'
C     INCLUDE '($JPIDEF)'
C     INCLUDE '($SSDEF)'
C
C                        return SYSTEM TYPE
C
      call CAPCHR(FUNC)
      if(FUNC.EQ.'SYST')then
C
C
C-------------------------BEGIN VAX VMS SPECIFIC CODE----------------------
C       STR1 = 'DECVAX'
C--------------------------END VAX VMS SPECIFIC CODE-----------------------
C
C-------------------------BEGIN CMS-DOS SPECIFIC CODE----------------------
C       STR1 = 'MS-DOS'
C--------------------------END CMS-DOS SPECIFIC CODE-----------------------
C
C-----------------------BEGIN CRAY SPECIFIC CODE--------------------
C       STR1 = 'UNICOS'
C------------------------END CRAY SPECIFIC CODE---------------------
C
C-----------------------BEGIN IRIS SPECIFIC CODE--------------------
        STR1 = 'IRIS'
C------------------------END IRIS SPECIFIC CODE---------------------
C
C-----------------------------BEGIN GENERIC CODE---------------------------
C       STR1 = 'UNKNOW'
C------------------------------END GENERIC CODE----------------------------
C
C                        return CURRENT DATE
C
      elseif(FUNC.EQ.'DATE')then
C
C-------------------------BEGIN VAX VMS SPECIFIC CODE----------------------
C       call IDATE(IMON,IDAY,IYEAR)
C       WRITE(STR1,51)IMON,IDAY,IYEAR
C51     FORMAT(I2.2,'/',I2.2,'/',I2.2)
C--------------------------END VAX VMS SPECIFIC CODE-----------------------
C
C-------------------------BEGIN CMS-DOS SPECIFIC CODE----------------------
CMS-DOS        call GETDAT(IYEAR,IMON,IDAY)
CMS-DOS        IYEAR=MOD(IYEAR,100)
CMS-DOS        WRITE(STR1,51)IMON,IDAY,IYEAR
CMS-DOS 51     FORMAT(I2.2,'/',I2.2,'/',I2.2)
C--------------------------END CMS-DOS SPECIFIC CODE-----------------------
C
C-----------------------------BEGIN GENERIC CODE---------------------------
        STR1='        '
C------------------------------END GENERIC CODE----------------------------
C
      endif
      return
C
C
      ENTRY SYBYT4(CFLAG,INP,IPOS,JCHAR)
C
C     Author:
C
C
C     EXTRACT A character FROM OR INSERT A
C              character TO A 4 BYTE integer
C
C     NOTE:    1. THIS subroutine IS TESTED GOOD ON
C                 VAX/VMS, AND IBM/PC. KZMASK AND KZCPOS ARE
C                 IN COMMON BLOCK 'CBYTE4', INITIALIZED IN
C                 'ZINIT'.
C              2. KZMASK IS THE MASK TO ZERO OUT THE BYTE TO
C                 WHICH A character IS TO BE INSERTED. FOR
C                 EXAMPLE, WHEN AN integer IS 'AND'ED WITH A
C                 MASK OF 7F7F7F00 HEX, THE LEAST SIGNIFICANT
C                 BYTE IS ZEROED.
C              3. KZNPOS IS POSITION OF THE BYTE OF INTEREST.
C                 FOR EXTRACTION, KZNPOS IS THE BITS TO SHIFT
C                 RIGHT TO PLACE THE DESIRED BYTE IN THE LEAST
C                 SIGNIFICANT POSITION. FOR INSERTION, KZNPOS
C                 IS THE BITS TO SHIFT LEFT TO PLACE THE BYTE
C                 TO BE INSERTED IN THE RIGHT POSITION.
C
C
C                        CALCULATE THE EXACT LOCATION
C
      if(IPOS.GT.KZBYTE)then
        IS=(IPOS-1)/KZBYTE
        IPTR=IS+1
        IP=IPOS-IS*KZBYTE
      else
        IPTR=1
        IP=IPOS
      endif
C
C                        TO EXTRACT A character
C
      if(CFLAG.EQ.'X')then
C-------------------------BEGIN VAX VMS SPECIFIC CODE----------------------
C        JCHAR=IBITS(INP(IPTR),(IP-1)*8,7)
C--------------------------END VAX VMS SPECIFIC CODE-----------------------
C-------------------------BEGIN CRAY SPECIFIC CODE-------------------------
C        JCHAR=IBITS(INP(IPTR),56-(IP-1)*8,7)
C--------------------------END CRAY SPECIFIC CODE--------------------------
C-------------------------BEGIN IRIS SPECIFIC CODE-------------------------
         JCHAR=IBITS(INP(IPTR),24-(IP-1)*8,7)
C--------------------------END IRIS SPECIFIC CODE--------------------------
C
C-------------------------BEGIN CMS-DOS SPECIFIC CODE----------------------
C        if(INP(IPTR).LT.0)then
C          IS=KSYAND(MAXINT,INP(IPTR))
C        else
C          IS=INP(IPTR)
C        endif
C        JCHAR=KSYAND(IS/KZCPOS(IP),127)
C--------------------------END CMS-DOS SPECIFIC CODE-----------------------
C------------------------BEGIN VAX UNIX SPECIFIC CODE----------------------
C        if(INP(IPTR).LT.0)then
C          IS=KSYAND(MAXINT,INP(IPTR))
C        else
C          IS=INP(IPTR)
C        endif
C        JCHAR=KSYAND(KSYSHR(IS,(IP-1)*8),127)
C-------------------------END VAX UNIX SPECIFIC CODE-----------------------
C-----------------------------BEGIN GENERIC CODE---------------------------
C       if(INP(IPTR).LT.0)then
C         IS=KSYAND(MAXINT,INP(IPTR))
C       else
C         IS=INP(IPTR)
C       endif
C       JCHAR=KSYAND(KSYSHR(IS,(NUMBYT-IP)*8),127)
C------------------------------END GENERIC CODE----------------------------
C
C                        TO INSERT A character
C
      else
C-------------------------BEGIN VAX VMS SPECIFIC CODE----------------------
C        call MVBITS(JCHAR,0,7,INP(IPTR),(IP-1)*8)
C--------------------------END VAX VMS SPECIFIC CODE-----------------------
C-------------------------BEGIN CRAY SPECIFIC CODE-------------------------
C        call MVBITS(JCHAR,0,7,INP(IPTR),56-(IP-1)*8)
C--------------------------END CRAY SPECIFIC CODE--------------------------
C-------------------------BEGIN IRIS SPECIFIC CODE-------------------------
         call MVBITS(JCHAR,0,7,INP(IPTR),24-(IP-1)*8)
C--------------------------END IRIS SPECIFIC CODE--------------------------
C-------------------------BEGIN CMS-DOS SPECIFIC CODE----------------------
C        INP(IPTR)=KSYAND(INP(IPTR),KZMASK(IP))+JCHAR*KZCPOS(IP)
C--------------------------END CMS-DOS SPECIFIC CODE-----------------------
C------------------------BEGIN VAX UNIX SPECIFIC CODE----------------------
C        INP(IPTR)=KSYOR(KSYAND(INP(IPTR),KZMASK(IP)),
C    .                   KSYSHL(JCHAR,(IP-1)*8))
C-------------------------END VAX UNIX SPECIFIC CODE-----------------------
C-----------------------------BEGIN GENERIC CODE---------------------------
C        INP(IPTR)=KSYOR(KSYAND(INP(IPTR),KZMASK(NUMBYT-(IP-1))),
C    .                   KSYSHL(JCHAR,(NUMBYT-IP)*8))
C------------------------------END GENERIC CODE----------------------------
      endif
      return
C
C                        PERFORM FILE NAME VERIFICATION
C
      ENTRY SYFCHK(STR1,STR2)
C
C-----------------------------BEGIN GENERIC CODE---------------------------
      NC = NUMCHR(STR1)
      LN = NUMCHR(STR2)
      IS = 0
      DO 20 I=1,NC
      IF(STR1(I:I) .EQ. ']') IS = I
   20 continue
      IS = IS + 1
      DO 30 I=IS,NC
        IF(STR1(I:I) .EQ. '.')then
          IF(NC .GT. I) return
          NC = I
          go to 40
        endif
   30 continue
      NC = NC + 1
      STR1(NC:NC) = '.'
   40 continue
      STR1(I+1:I+LN) = STR2(1:LN)
C------------------------------END GENERIC CODE----------------------------
      return
C
C      THIS routine RELEASES THE I/O CHANNAL TO THE OUTPUT DEVICE
C
      ENTRY SYFNSH(RELSTR)
      if(NUMCHR(RELSTR) .NE. 0)then
C
C        call GBMPTY
C        call GBINST(RELSTR)
C        call GBMPTY
      endif
 
      return
C
C      THIS routine INITIALIZES THE GRAPHICS DRIVERS BUFFERING
C      routines
C
      ENTRY SYINIT(KHAR,ENDSTR,TTNAME,IERR)
 
      IERR = 0
      TRMCHR = KHAR
      NDLNTH = NUMCHR(ENDSTR)
      DO 100 I=1,NDLNTH
        NDSTNG(I) = ENDSTR(I:I)
 100  continue
      BEGSTR(1) = CHAR(0)
      IBEGLN = 0
      return
C
C     ALLOW USER TO EXECUTE SYSTEM COMMANDS
C     DURING PROGRAM EXECUTION.
C
      ENTRY SYSJCL(KOMAND)
 
      return
C
C      THIS ROUTINE EMPTIES THE BUFFER IF IT HAS ANYTHING
C
      ENTRY SYSTTY(TTYBUF,IBUFLN)
C
C        DO THE QIOW TO THE OUTPUT DEVICE
C
10    continue
C  VAX/VMS DEVICE DRIVER CODE
C     ISTAT = SYS$QIOW(%VAL(0), %VAL(IOCHAN),
C    .                 %VAL(IO$_WRITEVBLK+IO$M_NOFORMAT),
C    .                 IOSB, , , %REF(TTYBUF), %VAL(IBUFLN),
C    .                 , , , )
C     WRITE(*,200) (TTYBUF(I),I=1,IBUFLN)
  200 FORMAT(2X,130A)
      return
C
C     EXTRACT USER'S NAME or ID.
C
      ENTRY SYSUSR(USRNAM)
C
C     THIS ROUTINE WILL EXTRACT THE USER'S "USERNAME"
C     FROM THE SYSTEM AND return IT IN THE character
C     ARGUMENT "USRNAM".
C
      return
C
C                        PERFORM SYSTEM WAIT
C
      ENTRY SYWAIT(MILLI)
C
 500  FORMAT('  0 ::',F5.2)
      ID = 0
      SEC = MILLI/1000.
      IF(SEC .LE. 0.) return
  600 continue
      ONE = SEC
      IF(ONE .GT. 59.99) ONE = 59.99
      WRITE(KODE,500) ONE
C  VAX/VMS DEVICE DRIVER CODE
C     ISTAT = SYS$BINTIM(KODE,IWAIT)
C     IF(.NOT. ISTAT) CALL LIB$STOP(%VAL(ISTAT))
C     ISTAT = SYS$SETIMR(%VAL(ID),IWAIT,,)
C     IF(.NOT. ISTAT) CALL LIB$STOP(%VAL(ISTAT))
C     ISTAT = SYS$WAITFR(%VAL(ID))
C     IF(.NOT. ISTAT) CALL LIB$STOP(%VAL(ISTAT))
      SEC = SEC - 59.99
      IF(SEC .GT. 0) go to 600
      return
      end
      subroutine TICKL(ANUM,UP)
      common /DBASE/VX,VY,VOLDX,VOLDY,CXSIZE,CYSIZE
      character NUMBR*6
      WRITE(NUMBR,10) ANUM
10    FORMAT(F6.2)
      DO 20 I=1,6
      IF(NUMBR(1:1) .NE. ' ') go to 30
      NUMBR(1:5) = NUMBR(2:6)
      NUMBR(6:6) = ' '
20    continue
30    continue
      IS = NUMCHR(NUMBR)
      TEMP = CXSIZE*((7-IS)+0.25)
      if(VX .GT. VOLDX) TEMP = -0.25*CXSIZE
      call GSMOVE(VX-TEMP,VY+UP*CYSIZE)
      call GSPSTR(NUMBR)
      return
      end
      subroutine TRACCY(XMIN,XMAX,Y,NPTS)
      DIMENSION Y(NPTS)
C
C     THIS ROUTINE TRACES THE LINE FROM X(1),Y(1) TO
C     X(NPTS),Y(NPTS) WITH APPROPIATE CLIPPING.
C     USE THIS ROUTINE WHEN CLIPPING IS DESIRED AND THE
C     INDEPENDANT VARIABLE IS IMPLIED BY THE SUBSCRIPT
C     USING EQUAL INTERVALS FROM XMIN TO XMAX.
C
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
      DIMENSION AREA(4)
C
      call GSSCLP(XVSTRT,XVSTRT+XVLEN,YVSTRT,YVSTRT+YVLEN,AREA)
      call SCALE(XMIN,Y(1),VX,VY)
      call GSMOVE(VX,VY)
10    DX = (XMAX-XMIN)/(NPTS-1)
      DO 100 I=2,NPTS
      call SCALE(XMIN+(I-1)*DX,Y(I),VX,VY)
      call GSDRAW(VX,VY)
100   continue
      call GSRCLP(AREA)
      return
      end
      subroutine TRACE(X,Y,NPTS)
C
C     THIS ROUTINE IS USED TO PLOT DATA ON THE SCREEN AS
C     A CONTINOUS LINE.
C
      DIMENSION X(2), Y(2)
      call SCALE(X(1),Y(1),VX,VY)
      call GSMOVE(VX,VY)
      DO 100 I=2,NPTS
      call SCALE(X(I),Y(I),VX,VY)
100   CALL GSDRAW(VX,VY)
      return
      end
      subroutine TRACEC(X,Y,NPTS)
      DIMENSION X(NPTS), Y(NPTS)
C
C     THIS ROUTINE TRACES THE LINE FROM X(1),Y(1) TO
C     X(NPTS),Y(NPTS) WITH APPROPIATE CLIPPING.
C
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
      DIMENSION AREA(4)
C
      call GSSCLP(XVSTRT,XVSTRT+XVLEN,YVSTRT,YVSTRT+YVLEN,AREA)
      call SCALE(X(1),Y(1),VX,VY)
      call GSMOVE(VX,VY)
      DO 100 I=2,NPTS
      call SCALE(X(I),Y(I),VX,VY)
      call GSDRAW(VX,VY)
100   continue
      call GSRCLP(AREA)
      return
      end
      subroutine TRACEY(XMIN,XMAX,Y,NPTS)
C
C     THIS subroutine IS USED TO PLOT DATA ON THE SCREEN AS
C     A CONTINOUS LINE, GIVEN ONLY THE Y ARRAY AND XMIN AND XMAX.
C
      DIMENSION Y(2)
      call SCALE(XMIN,Y(1),VX,VY)
      call GSMOVE(VX,VY)
      XINC = (XMAX-XMIN)/(NPTS-1)
      X = XMIN
      DO 100 I=2,NPTS
      X = X + XINC
      call SCALE(X,Y(I),VX,VY)
100   CALL GSDRAW(VX,VY)
      return
      end
      subroutine TRIPLX
C
C
C
C      SET character TYPE TO TRIPLEX
C               LEVEL 1-3, P/S
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        CZALFS='TRIPL'
      else
        call ERRMES('TRIPLX',1,3)
      endif
      return
      end
      subroutine TRMCHR(ISTR,LENGTH)
C
C
C
C      SET character STRING TERMINATOR
C               LEVEL 1-3, P/S
C
C      INPUT:   ISTR   = NEW STRING TERMINATOR
C               LENGTH = NUMBER OF characterS IN ISTR
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZSTRM=ISTR
        KZTMLN=LENGTH
      else
        call ERRMES('TRMCHR',1,3)
      endif
      return
      end
      subroutine TXTBLK(IP,NLINES,XPOS,YPOS)
C
C
C
C
C      WRITE PACKED character ARRAY AND PLOT SYMBOLS
C               IN LEGEND FORMAT, ALSO DRAW LINE IF DESIRED
C               (LEVEL 3)
C
C      INPUT:   IP     = PACKED ARRAY OF characterS
C               NLINES = NUMBER OF LINES IN PACKED ARRAY
C               XPOS   = X VALUE FROM PHYSICAL ORIGIN IN INCHES
C               YPOS   = Y VALUE FROM PHYSICAL ORIGIN IN INCHES
C
      parameter (KZYES=111)
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /CSYMBO/ KZSYM,KZNSYM,ZZSMSZ,UUSMSZ
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
      character CEQSGN*1
C
      DIMENSION IP(*),X(4),Y(4)
      character*5 CFLAG(6)*1,CFONT(6),CSTYLE
      LOGICAL WASTHK,LSHIFT
C
      EQUIVALENCE (IVAL,RVAL)
C
      SAVE CEQSGN,JTEXT,JHITE,JYRAT,JAVE,JSYMSP
      data CEQSGN /'='/
      data JTEXT,JHITE,JYRAT,JAVE,JSYMSP /1,-2,-1,2*0/
      if(KZLEVL.EQ.3)then
C
C                        SAVE OLD TEXT parameterS
C
        if(KZLGER.EQ.KZYES) CALL ERRMES('ERRMES',0,4)
        OHITE=ZZHITE
        OANGLE=ZZANGL
        if(KZLTHK.EQ.KZYES)then
          OLDTHK=ZZLTHK/ZZUNIT/ZZPAGR
          WASTHK=.TRUE.
        else
          WASTHK=.FALSE.
        endif
        call RESET('CRVWID')
C
C                        CALCULATE VIRTUAL COORDINATES AND
C                        LENGTH OF character STRING
C
        VX=ZZXOR+XCM0+XPOS*ZZUNIT*ZZPAGR
        VY=ZZYOR+YCM0+YPOS*ZZUNIT*ZZPAGR
        if(IP(1).EQ.KZYES)then
          IPLEN=IP(2)/1000
          LLEN=IP(2)-IPLEN*1000
        else
          LLEN=40
        endif
        NWORD=(LLEN-1)/KZBYTE+5
        XLEN=XLNLEG(IP,NLINES)*ZZUNIT*ZZPAGR
        YLEN=YLNLEG(IP,NLINES)
        YLEN=ZZLGYL
        ICUR=2+NWORD
        IVAL=IP(ICUR+JAVE)
        AVE=RVAL
        IVAL=IP(ICUR+NWORD+JSYMSP)
        SYMSP=RVAL
C
C                        PLOT TITLE OF LEGEND
C
        if(ZZLGTZ.LT.0.0)then
          call GSSETC(ZZHITE,0.0)
          VY=VY+YLEN-ZZHITE
        else
          call GSSETC(ZZLGTZ,0.0)
          VY=VY+YLEN-ZZLGTZ
        endif
        RLEN=ZXMESS(KZLGTI,KZLGTL,CZLGTC,CZLGTF,CZLGTS)
        call DSMOVE(VX+(XLEN-RLEN)*0.5,VY)
        call ZTEXT(KZLGTI,KZLGTL,CZLGTC,CZLGTF,CZLGTS)
        if(ZZLGTR.LT.-0.0)then
          VY=VY-AVE*.25
        else
          VY=VY-AVE*(MAX(0.0,ZZLGTR-1.0))/2.0
        endif
C
C                        FOR EACH LINE IN ARRAY:
C                        RETRIEVE LINE SPACE parameter, character
C                        HEIGHT, AND character FONT, SET TO CURRENT
C                        AND DRAW TO OUTPUT DEVICE
C
        ICUR=2
C
C                        NEED LINES IN LEGEND BLOCK
C
        if(KZLGLN.EQ.KZYES)then
          IOLSTY=KZLSTY
C
C                        SEE IF AT LEAST ONE SYMBOL DRAWN
C
          LSHIFT=.TRUE.
          DO 100 I=1,NLINES
            LSHIFT=LSHIFT.AND.(KZLGSM(I).LT.0)
 100      continue
          if(LSHIFT) SYMSP=SYMSP/2.0
C
          DO 300 I=1,NLINES
            ICUR=ICUR+NWORD
            if(KZLGEN(I).EQ.KZYES.AND.
     *          (KZLGLT(I).NE.0.OR.KZLGSM(I).GE.0))then
              IVAL=IP(ICUR+JYRAT)
              YRAT=RVAL
              if(YRAT.LT.1.0) YRAT=1.0
              IVAL=IP(ICUR+JHITE)
              HITE=RVAL
              DELTA=(YRAT-1.0)*AVE/2.0
              VY=VY-DELTA-HITE
              if(KZLGSM(I).GE.0)then
                call DSMOVE(VX+0.375*SYMSP,VY+HITE*0.5)
                call DSYMBO(KZLGSM(I),ZZSMSZ)
              endif
C
C                        DRAW THICK LINE
C
              VY=VY-DELTA
              if(KZLGLT(I).GT.10)then
                if(ZZLGTH(I).GT.DELTA)then
                  call CRVWID(DELTA/ZZPAGR/ZZUNIT)
                else
                  call CRVWID(ZZLGTH(I)/ZZPAGR/ZZUNIT)
                endif
                call DSLTYP(KZLGLT(I)-10)
                X(1)=VX
                X(2)=VX+XLEN
                Y(1)=VY
                Y(2)=VY
                call ZCURVE(X,Y,-2)
                call RESET('CRVWID')
C
C                        DRAW REGULAR LINE
C
              elseif(KZLGLT(I).NE.0)then
                call DSLTYP(KZLGLT(I))
                call DSMOVE(VX,VY)
                call DSDRAW(VX+XLEN,VY)
              endif
C
C                        DRAW TEXT
C
              DO 200 K=1,6
                CFLAG(K)=CZLGAC(I,K)
                CFONT(K)=CZLGAF(I,K)
 200          continue
              CSTYLE=CZLGAS(I)
              call GSSETC(HITE,0.0)
              call DSMOVE(VX+SYMSP,VY+DELTA)
              call ZTEXT(IP(ICUR-NWORD+JTEXT),100,CFLAG,CFONT,CSTYLE)
            endif
 300      continue
          call DSLTYP(IOLSTY)
C
C                        TEXT IN LEGEND BLOCK ONLY
C
        else
          DO 500 I=1,NLINES
            ICUR=ICUR+NWORD
            if(KZLGEN(I).EQ.KZYES)then
              if(KZLGSM(I).GE.0)then
                IVAL=IP(ICUR+JYRAT)
                YRAT=RVAL
                if(YRAT.LT.1.0) YRAT=1.0
                IVAL=IP(ICUR+JHITE)
                HITE=RVAL
                DELTA=(YRAT-1.0)*AVE/2.0
                VY=VY-DELTA-HITE
                call DSMOVE(VX+0.25*SYMSP,VY+HITE*0.5)
                call DSYMBO(KZLGSM(I),ZZSMSZ)
                call GSSETC(HITE,0.0)
                call DSMOVE(VX+SYMSP*0.5,VY)
                DO 400 K=1,6
                  CFLAG(K)=CZLGAC(I,K)
                  CFONT(K)=CZLGAF(I,K)
 400            continue
                CSTYLE=CZLGAS(I)
                call WCH2IN(CEQSGN,IEQSGN)
                call ZTEXT(IEQSGN,1,CFLAG,CFONT,CSTYLE)
                call DSMOVE(VX+SYMSP,VY)
                call ZTEXT(IP(ICUR-NWORD+JTEXT),100,CFLAG,CFONT,CSTYLE)
                VY=VY-DELTA
              endif
            endif
 500      continue
        endif
        call GSSETC(OHITE,OANGLE)
        if(WASTHK) CALL CRVWID(OLDTHK)
C
C                        WRONG LEVEL
C
      else
        call ERRMES('TXTBLK',2,3)
      endif
      return
      end
      subroutine VECTOR(XFROM,YFROM,XTO,YTO,IVEC)
C
C
C
C      DRAW A VECTOR W/ OR W/O ARROW HEAD
C               LEVEL 2-3
C
C      INPUT:   XFROM,YFROM = FIRST POINT OF VECTOR
C               XTO,YTO     = SECOND POINT OF VECTOR
C               IVEC(WXYZ)  = ARROW TYPE AND SIZE FLAG
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CUNIT/  ZZUNIT
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
C
      if(KZLEVL.EQ.2.OR.KZLEVL.EQ.3)then
        VXFROM=XCM0+ZZXOR+XFROM*ZZUNIT*ZZPAGR
        VYFROM=YCM0+ZZYOR+YFROM*ZZUNIT*ZZPAGR
        VXTO=XCM0+ZZXOR+XTO*ZZUNIT*ZZPAGR
        VYTO=YCM0+ZZYOR+YTO*ZZUNIT*ZZPAGR
        call ZVECTR(VXFROM,VYFROM,VXTO,VYTO,IVEC)
      else
        call ERRMES('VECTOR',3,0)
      endif
      return
      end
      subroutine VSPACE(YRATIO)
C
C
C
C      SET LINE SPACE RATIO FOR character PACKING
C               (LEVEL 1-3)
C
C      INPUT:   YRATIO = SPACE RATIO
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        ZZSRAT=YRATIO
      else
        call ERRMES('VSPACE',1,3)
      endif
      return
      end
      subroutine WCH2IN(STRNG,IWORD)
C
C  THE PURPOSE OF THIS ROUTINE IS TO STORE THE CONTENTS OF A STRING INTO
C  AN integer VARIABLE.
C
C----------------------------BEGIN 32-BIT SPECIFIC CODE-----------------
      parameter (NUMBYT=4)
C-----------------------------END 32-BIT SPECIFIC CODE------------------
C----------------------------BEGIN 64-BIT WORD SIZE CODE-----------------
C     parameter (NUMBYT=8)
C-----------------------------END 64-BIT WORD SIZE CODE------------------
      character*1 STRNG(NUMBYT)
C
      DO 100 I=1,NUMBYT
        KHAR=ICHAR(STRNG(I))
        call SYBYT4('N',IWORD,I,KHAR)
 100  continue
      return
      end
      subroutine WIN2CH(IWORD,STRNG)
C
C  THE PURPOSE OF THIS ROUTINE IS TO STORE THE CONTENTS OF an
C  integer WORD INTO A character VARIABLE.
C
C----------------------------BEGIN 32-BIT SPECIFIC CODE-----------------
      parameter (NUMBYT=4)
C-----------------------------END 32-BIT SPECIFIC CODE------------------
C----------------------------BEGIN 64-BIT WORD SIZE CODE-----------------
C     parameter (NUMBYT=8)
C-----------------------------END 64-BIT WORD SIZE CODE------------------
      character*1 STRNG(NUMBYT)
C
      DO 100 I=1,NUMBYT
        call SYBYT4('X',IWORD,I,KHAR)
        STRNG(I)=CHAR(KHAR)
 100  continue
      return
      end
      subroutine XANGLE(ANG)
C
C
C
C      SETS ANGLE OF X AXIS VALUES
C               LEVEL 1-3, P/S
C
C      INPUT:   ANG = TEXT ANGLE FOR AXIS LABELS
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        TEMP=MOD(ANG,360.0)
        if(TEMP.LT.0.0) TEMP=360.0+TEMP
        if(TEMP.GT.90.0.AND.TEMP.LE.270.0)then
          ZZXLBA=ABS(TEMP-180.0)
        else
          ZZXLBA=TEMP
        endif
      else
        call ERRMES('XANGLE',1,3)
      endif
      return
      end
      FUNCTION XBTEXT(IP,NLINES)
C
C
C
C
C     return THE X LENGTH OF A PACKED ARRAY IN INCHES
C              (LEVEL 1-3)
C
C     INPUT:   IP     = PACKED character ARRAY
C              NLINES = NUMBER OF LINES IN IP
C
C     OUTPUT:  XBTEXT = LENGTH IN INCHES
C
      parameter (KZYES=111)
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
C
      DIMENSION IP(*)
      character*1 CFLAG(6)
      character*5 CFONT(6),CSTYLE
      EQUIVALENCE (RVAL,IVAL)
C
      SAVE JTEXT,JHITE
      data JTEXT,JHITE /1,-2/
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
C
C                        SAVE OLD SET UP
C
        OHITE=ZZHITE
        OANGLE=ZZANGL
        if(IP(1).EQ.KZYES)then
          IPLEN=IP(2)/1000
          LLEN=IP(2)-IPLEN*1000
        else
          LLEN=40
        endif
        NWORD=(LLEN-1)/KZBYTE+5
C
C                        INIT MAXIMUM FOR LINE LENGTH
C
        ICUR=2+NWORD
        DO 200 I=1,6
          CFLAG(I)=CZLGAC(1,I)
          CFONT(I)=CZLGAF(1,I)
 200    continue
        CSTYLE=CZLGAS(1)
        IVAL=IP(ICUR+JHITE)
        call GSSETC(RVAL,0.0)
        RMAX=ZXMESS(IP(3),100,CFLAG,CFONT,CSTYLE)
C
C                        FIND MAXIMUM LINE LENGTH
C
        DO 100 I=2,NLINES
          ICUR=ICUR+NWORD
          IVAL=IP(ICUR+JHITE)
          call GSSETC(RVAL,0.0)
          DO 300 K=1,6
            CFLAG(K)=CZLGAC(I,K)
            CFONT(K)=CZLGAF(I,K)
 300      continue
          CSTYLE=CZLGAS(I)
          RLEN=ZXMESS(IP(ICUR-NWORD+JTEXT),100,
     *                  CFLAG,CFONT,CSTYLE)
          if(RLEN.GT.RMAX) RMAX=RLEN
 100    continue
C
C                        ASSIGN 'XBTEXT' AS MAXIMUM LINE
C                        LENGTH IN CURRENT UNIT AND
C                        RESTORE OLD SET UP
C
        XBTEXT=RMAX/ZZUNIT/ZZPAGR
        ZZHITE=OHITE
        ZZANGL=OANGLE
        call GSSETC(OHITE,OANGLE)
C
C                        WRONG LEVEL
C
      else
        XBTEXT=0.0
        call ERRMES('XBTEXT',1,3)
      endif
      return
      end
      FUNCTION XCOORD(XVAL,YVAL)
C
C
C
C
C     return X POSITION IN INCHES
C              (LEVEL 3)
C
C     INPUT:   XVAL,YVAL = X AND Y VALUES IN CURRENT
C                          COORDINATE SYSTEM
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
C:  CHANGED TO ACCOMMADATE AN 8 BYTE WORD
C     character KHAR*4
C     EQUIVALENCE (RX,KHAR)
      character KHAR*(NUMBYT)
      if(KZLEVL.EQ.3)then
        RX=XVAL
        call WIN2CH(RX,KHAR)
        call CAPCHR(KHAR)
        if(KHAR(1:4).EQ.'LEFT')then
          XCOORD=-ZZXOR
        elseif(KHAR(1:4).EQ.'RIGH')then
          XCOORD=XCM1-XCM0-ZZXOR
        else
          YT=1.0
          call SCALE(XVAL,YT,VX,VY)
          XCOORD=VX-XCM0-ZZXOR
        endif
        XCOORD=XCOORD/ZZUNIT/ZZPAGR
      else
        XCOORD=0.0
        call ERRMES('XCOORD',3,0)
      endif
      return
      end
      REAL FUNCTION XDIMTB(TEXT,LINE1,LINE2)
c
c***********************************************************************
c
c  XDIMTB calculates length of a text block in inches.
c
c  XDIMTB may be called at levels 1, 2 or 3.
c
c  XDIMTB finds the length in inches of an imaginary rectangle
c  enclosing a block of text.  This routine may be used as an
c  alternative to XBTEXT, for handling data in character array
c  format.
c
c  The character size and font are assumed to be set outside the
c  routine and hence constant within the indicated lines of text.
c
c  XDIMTB was introduced when it was realized that TXTLEG needs
c  such a function, both prior to calling TXTLEG, for positioning
c  the legend and within TXTLEG, for the optional box.
c
c  The size of the block is the length of the longest line in the
c  block.  The SLNGTH utility, with its self counting option is
c  used for finding the length of a string.
c
c  TEXT   Input, character*(*) TEXT(*), the text block, with
c         trailing '$' signs used to mark the end of strings.
c         Elements LINE1:LINE2 are processed.
c
c  LINE1,
c  LINE2  Input, integer LINE1, LINE2, the first and last lines
c         of text to be processed.
c
c  XDIMTB Output, REAL XDIMTB, the length of the block in inches.
c
      character TEXT (*) * (*)
      integer   LINE1, LINE2
      integer   KZLEVL, KZBEGN
      common /CLEVEL/ KZLEVL, KZBEGN
      integer   I
      REAL      SLNGTH
      EXTERNAL  SLNGTH
C
      IF(KZLEVL.LT.1)THEN
        write(*,*)'XDIMTB - Fatal error!'
        write(*,*)'XDIMTB must be called at level 1, 2 or 3.'
        write(*,*)'However, current level is ',KZLEVL
        stop
      else
        XDIMTB=0.0
        DO 100 I=LINE1,LINE2
          XDIMTB = MAX(XDIMTB,SLNGTH(TEXT(I),100))
  100   continue
      endif
      return
      end
      REAL FUNCTION XINVRS(XINCH,YINCH)
C
C ONE-LINER:
C     Converts the location of a point from inches to data units. (Level 3)
C
C PURPOSE:
C        XINVRS may be used (along with its analog YINVRS) to convert the
C     location of a point given in inches from the physical origin to a
C     value in units of the current coordinate system.
C
C ARGUMENTS:
C     Name    Dimension   Type  I/O/S   Description
C     XINCH       -        R      I     X position from the physical origin
C     YINCH       -        R      I     Y position from the physical origin
C
C METHOD:
C        The routine makes the conversion by the use of parameters passed via
C     internal common blocks.  This requires that the plot parameters be
C     defined before this routine is called.
C
C     Conversions are made for both linear and log axis systems.
C
C     Notes:
C        1) A polar system conversion will be developed when appropriate.
C        2) Two arguments are needed to handle the general case of a rotated
C           coordinate system.
C
C ERROR HANDLING:
C     Returns error message if called at the wrong level.
C
C ENVIRONMENT:
C     Digital VAX-11/780 VMS Version 4.4 (FORTRAN 77)
C
C AUTHOR:
C     Michael D. Wong, Sterling Software, NASA Ames Research Center
C
C HISTORY:
C-------------------------------------------------------------------------------
C
C     Arguments
C     ---------
C
      REAL
     >   XINCH, YINCH
C
C     Global variables
C     ----------------
C
      integer
     >   KZPAGE, KZAUTO, KZOR, KZLEVL, KZBEGN
      LOGICAL
     >   LOGX, LOGY
      REAL
     >   UDX, UDY, UX0, UY0, UUPAGX, UUPAGY, UUXOR, UUYOR, XCM0,
     >   XCM1, XVSTRT, XVLEN, YCM0, YCM1, YVLEN, YVSTRT, ZZPAGX,
     >   ZZPAGY, ZZPAGR, ZZUNIT, ZZXOR, ZZYOR
C
      common /CLEVEL/ KZLEVL, KZBEGN
      common /CPAGE/  ZZPAGX, ZZPAGY, ZZPAGR, UUPAGX, UUPAGY,
     >                KZPAGE, KZAUTO
      common /CPHYSR/ ZZXOR, ZZYOR, KZOR, UUXOR, UUYOR
      common /CUNIT/  ZZUNIT
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
C     Execution
C     ---------
C
      if(KZLEVL .NE. 3) go to 900
C
      XINVRS = (((ZZXOR + XCM0 + (XINCH * ZZUNIT * ZZPAGR)) - XVSTRT)
     >         * UDX / XVLEN) + UX0
C
      if(LOGX) XINVRS = 10 ** (XINVRS)
C
      go to 999
C
  900 CALL ERRMES ('XINVRS', 3, 3)
C
  999 return
      end
      subroutine XLABEL(LXNAME,IXNAME)
C
C
C
C
C      DEFINE X AXIS LABEL
C               LEVEL 2 OR 3, P/S
C
C      INPUT:   LXNAME = character STRING
C               IXNAME = NUMBER OF characterS IN LXNAME
C
      parameter (KZYES=111)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
C
      DIMENSION LXNAME(2)
C  CALLABLE FROM LEVEL 2 OR 3
C     if(KZLEVL.EQ.2)then
      if((KZLEVL.EQ.2).OR.(KZLEVL.EQ.3))then
        KZXNFL=KZYES
        if(IXNAME.EQ.0)then
          KZXALN=0
        else
          KZXALN=80
          call ZCOPYS(LXNAME,IXNAME,KZXLAB,KZXALN)
          if(KZXALN.LT.0)then
            KZXALN=-KZXALN
            call ERRMES('XLABEL',80,4)
          endif
        endif
      else
C  CALLABLE FROM LEVEL 2 OR 3
C       call ERRMES('XLABEL',2,0)
        call ERRMES('XLABEL',2,3)
      endif
      return
      end
      FUNCTION XLNLEG(IP,NLINES)
C
C
C
C
C     return THE X LENGTH OF A PACKED ARRAY IN INCHES
C              (LEVEL 1-3)
C
C     INPUT:   IP     = PACKED character ARRAY
C              NLINES = NUMBER OF LINES IN IP
C
C     OUTPUT:  XLNLEG = LENGTH IN INCHES
C
      parameter (KZYES=111)
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CUNIT/  ZZUNIT
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     .                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     .                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     .                CZLGTF(6),CZLGTS
      character CZALFL*1,CZALFN*5,CZALFS*5
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
C
      DIMENSION IP(*)
      character*1 CFLAG(6),CM4*4
      character*5 CFONT(6),CSTYLE
      EQUIVALENCE (RVAL,IVAL)
C
      SAVE JTEXT,JHITE,CM4
      data JTEXT,JHITE /1,-2/
      data CM4 /'MMMM'/
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
C
C                        SAVE OLD SET UP
C
        OHITE=ZZHITE
        OANGLE=ZZANGL
        if(IP(1).EQ.KZYES)then
          IPLEN=IP(2)/1000
          LLEN=IP(2)-IPLEN*1000
        else
          LLEN=40
        endif
        NWORD=(LLEN-1)/KZBYTE+5
C
C  INIT MAXIMUM FOR LINE LENGTH
C
        ICUR=2
        SMAX=0.0
        RMAX=0.0
C
C  CALCULATE LEGEND TITLE LENGTH
C
        if(ZZLGTZ.LT.0.0)then
          call GSSETC(ZZHITE,0.0)
        else
          call GSSETC(ZZLGTZ,0.0)
        endif
        RTITLE=ZXMESS(KZLGTI,KZLGTL,CZALFL,CZALFN,CZALFS)
C
C                        FIND MAXIMUM LINE LENGTH FOR ALL
C                        ACTIVATED ENTRIES IN PACKED ARRAY
C
        DO 200 I=1,NLINES
          ICUR=ICUR+NWORD
          if(KZLGEN(I).EQ.KZYES)then
            IVAL=IP(ICUR+JHITE)
            call GSSETC(RVAL,0.0)
            DO 100 K=1,6
              CFLAG(K)=CZLGAC(I,K)
              CFONT(K)=CZLGAF(I,K)
 100        continue
            CSTYLE=CZLGAS(I)
            call WCH2IN(CM4,M4)
            SLEN=ZXMESS(M4,4,CFLAG,CFONT,CSTYLE)
            RLEN=ZXMESS(IP(ICUR-NWORD+JTEXT),100,
     *                  CFLAG,CFONT,CSTYLE)
            if(RLEN.GT.RMAX) RMAX=RLEN
            if(SLEN.GT.SMAX) SMAX=SLEN
          endif
 200    continue
C
C  ASSIGN XLNLEG AS MAXIMUM LINE LENGTH IN CURRENT UNIT AND
C  RESTORE OLD SET UP
C
        if(SMAX+RMAX.GT.RTITLE)then
          XLNLEG=(SMAX+RMAX)/ZZUNIT/ZZPAGR
        else
          XLNLEG=RTITLE/ZZUNIT/ZZPAGR
        endif
        RVAL=SMAX
        IP(2+2*NWORD)=IVAL
        ZZHITE=OHITE
        ZZANGL=OANGLE
        call GSSETC(OHITE,OANGLE)
C
C                        WRONG LEVEL
C
      else
        call ERRMES('XLNLEG',1,3)
      endif
      return
      end
      subroutine XLOG(XORIGN,XCYCLE,YORIGN,YSTEP)
C
C
C
C      SET UP COORDINATE SYSTEM FOR LOG X AND LINEAR Y
C               (LEVEL 2, RAISE TO LEVEL 3)
C
C      INPUT:   XORIGN = LOWER LIMIT OF X AXIS
C               XCYCLE = CYCLE LENGTH PER INCHES
C               YORIGN = LOWER LIMIT OF Y AXIS
C               YSTEP  = STEP SIZE PER INCH FOR YAXIS
C
      parameter (KZLOG=3)
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
      common /CCOORD/ ZZXMIN,ZZXMAX,ZZXSTP,ZZYMIN,ZZYMAX,ZZYSTP
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /CUNIT/  ZZUNIT
      LOGICAL LOGX, LOGY
      if(KZLEVL.EQ.2)then
C
C                        INIT IAXES TO DEFINE LINEAR Y AXIS
C                        AND DRAW TITLE
C
        LOGX=.FALSE.
        LOGY=.FALSE.
        IAXES=19
        XMIN=XORIGN
        YMIN=YORIGN
        XMAX=XMIN*10.0**(XCYCLE*ZZXAXS/ZZUNIT/ZZPAGR)
        YMAX=YMIN+YSTEP*ZZYAXS/ZZUNIT/ZZPAGR
        ZZXSTP=(XMAX-XMIN)/2.0
        ZZYSTP=YSTEP
        call ZMAPIT(XMIN,XMAX,YMIN,YMAX,0,0,
     *              KZYLAB,KZYALN,0.0,0.0,IAXES)
C
C                        DEFINE LOG X AXIS
C
        DUMMY=0.0
        KZXTYP=KZLOG
        LFLAG=1
        XPOS=0.0
        YPOS=0.0
        call SCALOG(XMIN,XMAX,ZZXAXS,XOR,XCYC)
C  RESOLVE CONFLICT WITH SGI IRIS 4D FORTRAN LIBRARIES
C       call ZLOG(XOR,XCYC,DUMMY,DUMMY,ZZXAXS,DUMMY,
C    *         KZXLAB,KZXALN,0,0,XPOS,YPOS,LFLAG)
        call ZDRLOG(XOR,XCYC,DUMMY,DUMMY,ZZXAXS,DUMMY,
     *         KZXLAB,KZXALN,0,0,XPOS,YPOS,LFLAG)
        KZLEVL=3
      else
        call ERRMES('XLOG',2,0)
      endif
      return
      end
      subroutine XMARKS(ITICKS)
C
C
C
C      SET NUMBER OF TICKS ON X-AXIS
C               LEVEL 1-3, P/S
C
C      INPUT:   ITICKS = NUMBER OF TICKS PER STEP
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZXTCK=ITICKS
      else
        call ERRMES('XMARKS',1,3)
      endif
      return
      end
      subroutine XTRLGX(XOR,XCYC,XAXIS,LABX,LXLAB,XPOS,YPOS)
C
C
C
C
C      SET UP LOG X AXIS
C               (LEVEL 3)
C
C      INPUT:   XOR       = LOWER LIMIT OF X AXIS
C               XCYC      = CYCLE LENGTH IN INCHES
C               XAXIS     = X AXIS LENGTH IN INCHES
C               LABX      = X AXIS LABEL
C               LXLAB     = NUMBER OF characterS IN LABX
C               XPOS,YPOS = LOCATION OF SUBPLOT FROM LOWER
C                           LEFT CORNER OF PAGE
C
      parameter (KZLOG=3)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CUNIT/  ZZUNIT
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI
      LOGICAL LOGX, LOGY
C
      DIMENSION LABX(*)
      if(KZLEVL.EQ.3)then
        LOGX=.FALSE.
        DUMMY=0.0
        KZXTYP=KZLOG
        LFLAG=1
        if(LXLAB.EQ.0)then
          KZXALN=0
        else
          KZXALN=80
          call ZCOPYS(LABX,LXLAB,KZXLAB,KZXALN)
          if(KZXALN.LT.0)then
            KZXALN=-KZXALN
            call ERRMES('XTRLGX',80,4)
          endif
        endif
        ZZXAXS=XAXIS*ZZUNIT*ZZPAGR
        ZZXLFT=XCM0+ZZXOR
        ZZXAXR=(XCM1-ZZXLFT)/ZZXAXS
        if(ZZXAXR.GT.1.0)then
          ZZXAXR=1.0
          ZZXRGT=ZZXLFT+ZZXAXS
        else
          ZZXRGT=XCM1
        endif
C        XVSTRT=ZZXLFT
        XVLEN=ZZXRGT-ZZXLFT
C  RESOLVE CONFLICT WITH SGI IRIS 4D FORTRAN LIBRARIES
C       call ZLOG(XOR,XCYC,DUMMY,DUMMY,XAXIS,DUMMY,
C    *         KZXLAB,KZXALN,0,0,XPOS,YPOS,LFLAG)
        call ZDRLOG(XOR,XCYC,DUMMY,DUMMY,XAXIS,DUMMY,
     *         KZXLAB,KZXALN,0,0,XPOS,YPOS,LFLAG)
      else
        call ERRMES('XTRLGX',3,0)
      endif
      return
      end
      subroutine XTRLGY(YOR,YCYC,YAXIS,LABY,LYLAB,XPOS,YPOS)
C
C
C
C
C      SET UP LOG Y AXIS
C               (LEVEL 3)
C
C      INPUT:   YOR       = LOWER LIMIT OF Y AXIS
C               YCYC      = CYCLE LENGTH IN INCHES
C               YAXIS     = Y AXIS LENGTH IN INCHES
C               LABY      = Y AXIS LABEL
C               LYLAB     = NUMBER OF characterS IN LABY
C               XPOS,YPOS = LOCATION OF SUBPLOT FROM LOWER
C                           LEFT CORNER OF PAGE
C
      parameter (KZLOG=3)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CUNIT/  ZZUNIT
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI
      LOGICAL LOGX, LOGY
C
      DIMENSION LABY(*)
      if(KZLEVL.EQ.3)then
        LOGY=.FALSE.
        DUMMY=0.0
        KZYTYP=KZLOG
        IAXES=2
        if(LYLAB.EQ.0)then
          KZYALN=0
        else
          KZYALN=80
          call ZCOPYS(LABY,LYLAB,KZYLAB,KZYALN)
          if(KZYALN.LT.0)then
            KZYALN=-KZYALN
            call ERRMES('XTRLGY',80,4)
          endif
        endif
        ZZYAXS=YAXIS*ZZUNIT*ZZPAGR
        ZZYBOT=YCM0+ZZYOR
        ZZYAXR=(YCM1-ZZYBOT)/ZZYAXS
        if(ZZYAXR.GT.1.0)then
          ZZYAXR=1.0
          ZZYTOP=ZZYBOT+ZZYAXS
        else
          ZZYTOP=YCM1
        endif
C        YVSTRT=ZZYBOT
        YVINI=ZZYTOP-ZZYBOT
C  RESOLVE CONFLICT WITH SGI IRIS 4D FORTRAN LIBRARIES
C       call ZLOG(DUMMY,DUMMY,YOR,YCYC,DUMMY,YAXIS,0,0,
C    *         KZYLAB,KZYALN,XPOS,YPOS,IAXES)
        call ZDRLOG(DUMMY,DUMMY,YOR,YCYC,DUMMY,YAXIS,0,0,
     *         KZYLAB,KZYALN,XPOS,YPOS,IAXES)
      else
        call ERRMES('XTRLGY',3,0)
      endif
      return
      end
      subroutine XTRLNX(XMIN,XSTP,XMAX,XAXIS,LXNAME,IXNAME,
     *                    XPOS,YPOS)
C
C
C
C     SET UP LINEAR COORDINATE SYSTEM
C              LEVEL 3
C
C     INPUT:   XMIN,XMAX     = RANGE OF X AXIS
C              XSTP          = STEP SIZE, IF = 'SCAL',
C                              A STEP SIZE IS ASSIGNED
C              XAXIS         = LENGTH OF AXIS IN INCHES
C              LXNAME,IXNAME = AXIS LABEL AND LENGTH
C              XPOS,YPOS     = OFFSET FROM ORIGIN
C
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CUNIT/  ZZUNIT
      common /CCOORD/ ZZXMIN,ZZXMAX,ZZXSTP,ZZYMIN,ZZYMAX,ZZYSTP
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
      integer LXNAME(*)
      character CXSTR*4
C
C     EQUIVALENCE (CXSTR,XSTEP)
C
C
C                        CHECK LEVEL
C
      if(KZLEVL.EQ.3)then
C
C                        INIT IAXES TO DEFINE X AND Y AXIS
C                        AND DRAW TITLE
C
        IAXES=1
        XSTEP=XSTP
C
        call WIN2CH(XSTEP,CXSTR)
C
        call CAPCHR(CXSTR)
        if(CXSTR.EQ.'SCAL')then
          IAXES=IAXES+4
        else
          ZZXSTP=XSTP
        endif
        ZZXAXS=XAXIS*ZZUNIT*ZZPAGR
        ZZXLFT=XCM0+ZZXOR
        ZZXAXR=(XCM1-ZZXLFT)/ZZXAXS
        if(ZZXAXR.GT.1.0)then
          ZZXAXR=1.0
          ZZXRGT=ZZXLFT+ZZXAXS
        else
          ZZXRGT=XCM1
        endif
        XVSTRT=ZZXLFT
        XVLEN=ZZXRGT-ZZXLFT
        KZXALN=80
        call ZCOPYS(LXNAME,IXNAME,KZXLAB,KZXALN)
        if(KZXALN.LT.0)then
          KZXALN=-KZXALN
          call ERRMES('XTRLNX',80,4)
        endif
        call ZMAPIT(XMIN,XMAX,DUM,DUM,KZXLAB,KZXALN,
     *              KZYLAB,KZYALN,XPOS,YPOS,IAXES)
      else
        call ERRMES('XTRLNX',3,0)
      endif
      return
      end
      subroutine XTRLNY(YMIN,YSTP,YMAX,YAXIS,LYNAME,IYNAME,
     *                    XPOS,YPOS)
C
C
C
C
C     SET UP LINEAR COORDINATE SYSTEM
C              (LEVEL 3)
C
C     INPUT:   YMIN,YMAX     = RANGE OF Y AXIS
C              YSTP          = STEP SIZE, IF = 'SCAL',
C                              A STEP SIZE IS ASSIGNED
C              YAXIS         = LENGTH OF AXIS IN INCHES
C              LYNAME,IYNAME = AXIS LABEL AND LENGTH
C              XPOS,YPOS     = OFFSET FROM ORIGIN
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CCOORD/ ZZXMIN,ZZXMAX,ZZXSTP,ZZYMIN,ZZYMAX,ZZYSTP
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CUNIT/  ZZUNIT
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI
C
      integer LYNAME(*)
C:  CHANGED TO ACCOMMADATE AN 8 BYTE WORD
C     character*4 CYSTR
C     EQUIVALENCE (IYSTR,CYSTR)
      character*(NUMBYT) CYSTR
C
C                        CHECK LEVEL
C
      if(KZLEVL.EQ.3)then
C
C                        INIT IAXES TO DEFINE X AND Y AXIS
C                        AND DRAW TITLE
C
        IAXES=2
        IYSTR=INT(YSTP)
        call WIN2CH(IYSTR,CYSTR)
        call CAPCHR(CYSTR)
        if(CYSTR(1:4).EQ.'SCAL')then
C
          IAXES=IAXES+8
        else
          ZZYSTP=YSTP
        endif
        ZZYAXS=YAXIS*ZZUNIT*ZZPAGR
        ZZYBOT=YCM0+ZZYOR
        ZZYAXR=(YCM1-ZZYBOT)/ZZYAXS
        if(ZZYAXR.GT.1.0)then
          ZZYAXR=1.0
          ZZYTOP=ZZYBOT+ZZYAXS
        else
          ZZYTOP=YCM1
        endif
        YVSTRT=ZZYBOT
        YVINI=ZZYTOP-ZZYBOT
        KZYALN=80
        call ZCOPYS(LYNAME,IYNAME,KZYLAB,KZYALN)
        if(KZYALN.LT.0)then
          KZYALN=-KZYALN
          call ERRMES('XTRLNY',80,4)
        endif
        call ZMAPIT(DUM,DUM,YMIN,YMAX,KZXLAB,KZXALN,
     *              KZYLAB,KZYALN,XPOS,YPOS,IAXES)
      else
        call ERRMES('XTRLNY',3,0)
      endif
      return
      end
C
      subroutine XYPRM(X,Y,ZETA,ILINE)
C
C     COMMON STORAGE DESCRIPTOR
      common/COMDP/XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,AXISR(2),PLOTX,
     1   PLOTY,PLTORG(2),CAMXYZ(3),MX,NY,FMX,FNY,CAMWKG(6),XORG(3),
     2   GX(3),FX(2),KSCALE,ZORG,CENTER(2),PQLMT,
     3   AMTX(3,3),FOCALL
      DIMENSION LIMIT(2),FLIM(2)
      EQUIVALENCE(U,CAMXYZ(1)),(V,CAMXYZ(2)),(W,CAMXYZ(3)),
     1   (MX,LIMIT(1)),(FMX,FLIM(1))
C     END CDE
C
      common /DBASE/VX,VY,VOLDX,VOLDY,CXSIZE,CYSIZE
      DIMENSION XS(3),XC(3)
      XS(1)=XMIN+(X-1.0)*GX(1)-CAMWKG(1)
      XS(2)=YMIN+(Y-1.0)*GX(2)-CAMWKG(2)
      XS(3)=ZORG + ZETA*GX(3)-CAMWKG(3)
      call ROTATE(XS,AMTX,XC)
      VX=(XC(1)/XC(3)-XORG(1))*FOCALL+PLTORG(1)+CENTER(1)
      VY=(XC(2)/XC(3)-XORG(2))*FOCALL+PLTORG(2)+CENTER(2)
      if(ILINE) 30, 20, 10
10    CALL GSDRAW(VX,VY)
      go to 30
20    CALL GSMOVE(VX,VY)
30    return
      end
      subroutine YANGLE(ANG)
C
C
C
C      SET ANGLE OF Y AXIS VALUES
C               LEVEL 1-3, P/S
C
C      INPUT:   ANG = TEXT ANGLE FOR AXIS LABEL
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        TEMP=MOD(ANG,360.0)
        if(TEMP.LT.0.0) TEMP=360.0+TEMP
        if(TEMP.GT.90.0.AND.TEMP.LE.270.0)then
          ZZYLBA=ABS(TEMP-180.0)
        else
          ZZYLBA=TEMP
        endif
      else
        call ERRMES('YANGLE',1,3)
      endif
      return
      end
      FUNCTION YBTEXT(IP,NLINES)
C
C
C
C
C     return Y LENGTH OF A PACKED ARRAY IN INCHES
C             (LEVEL 1-3)
C
C     INPUT:   IP     = PACKED character ARRAY
C              NLINES = NUMBER OF LINES IN IP
C
C     OUTPUT:  YBTEXT = LENGTH IN INCHES
C
      parameter (KZYES=111)
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CUNIT/  ZZUNIT
C
      DIMENSION IP(*)
      EQUIVALENCE (IVAL,RVAL)
C
      SAVE JHITE,JYRAT
      data JHITE,JYRAT /-2,-1/
      if(KZLEVL.GE.1.OR.KZLEVL.LE.3)then
C
C                        RETRIEVE ARRAY INFO FROM IP
C
        if(IP(1).EQ.KZYES)then
          IPLEN=IP(2)/1000
          LLEN=IP(2)-IPLEN*1000
        else
          LLEN=40
        endif
        NWORD=(LLEN-1)/KZBYTE+5
C
C                        FIND INTER-LINE SPACE FOR 1-ST
C                        AND LAST LINE
C
        ICUR=2+NWORD
        IVAL=IP(ICUR+JYRAT)
        YRAT1=RVAL
        if(YRAT1.LT.1.0) YRAT1=1.0
        IVAL=IP(ICUR+(NLINES-1)*NWORD+JYRAT)
        YRATN=RVAL
        if(YRATN.LT.1.0) YRATN=1.0
C
C                        LOOP TO SUM CHAR HEIGHT AND
C                        INTER-LINE SPACE FACTOR
C
        ICUR=2
        SUM=0.0
        RATSUM=0.0
        DO 100 I=1,NLINES
          ICUR=ICUR+NWORD
          IVAL=IP(ICUR+JHITE)
          SUM=SUM+RVAL
          IVAL=IP(ICUR+JYRAT)
          YRAT=RVAL
          if(YRAT.LT.1.0) YRAT=1.0
          RATSUM=RATSUM+YRAT
 100        continue
        AVE=SUM/NLINES
        YBTEXT=(RATSUM-(YRAT1+YRATN-2.0)/2.0)*AVE/ZZUNIT/ZZPAGR
        RVAL=AVE
        IP(NWORD+2)=IVAL
C
C                        WRONG LEVEL
C
      else
        YBTEXT=0.0
        call ERRMES('YBTEXT',2,3)
      endif
      return
      end
      FUNCTION YCOORD(XVAL,YVAL)
C
C
C
C
C     return Y POSITION IN INCHES
C              (LEVEL 3)
C
C     INPUT:   XVAL,YVAL = X AND Y VALUES IN CURRENT
C                          COORDINATE SYSTEM
C
C     OUTPUT:  YCOORD = LENGTH IN INCHES FROM PHYSICAL
C                      ORIGIN
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CUNIT/  ZZUNIT
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
C:  CHANGED TO ACCOMMADATE AN 8 BYTE WORD
C     character KHAR*4
C     EQUIVALENCE (RY,KHAR)
      character KHAR*(NUMBYT)
      if(KZLEVL.EQ.3)then
        RY=YVAL
        call WIN2CH(RY,KHAR)
        call CAPCHR(KHAR)
        if(KHAR(1:4).EQ.'LOWE')then
          YCOORD=-ZZYOR
        elseif(KHAR(1:4).EQ.'UPPE')then
          YCOORD=YCM1-YCM0-ZZYOR
        else
          XT=1.0
          call SCALE(XT,YVAL,VX,VY)
          YCOORD=VY-YCM0-ZZYOR
        endif
        YCOORD=YCOORD/ZZUNIT/ZZPAGR
      else
        YCOORD=0.0
        call ERRMES('YCOORD',3,0)
      endif
      return
      end
      REAL FUNCTION YINVRS (XINCH, YINCH)
C
C ONE-LINER:
C     Converts the location of a point from inches to data units. (Level 3)
C
C PURPOSE:
C        YINVRS may be used (along with its analog XINVRS) to convert the
C     location of a point given in inches from the physical origin to a
C     value in units of the current coordinate system.
C
C ARGUMENTS:
C     Name    Dimension   Type  I/O/S   Description
C     XINCH       -        R      I     X position from the physical origin
C     YINCH       -        R      I     Y position from the physical origin
C
C METHOD:
C        The routine makes the conversion by the use of parameters passed via
C     internal common blocks.  This requires that the plot parameters be
C     defined before this routine is called.
C
C     Conversions are made for both linear and log axis systems.
C
C     Notes:
C        1) A polar system conversion will be developed when appropriate.
C        2) Two arguments are needed to handle the general case of a rotated
C           coordinate system.
C
C ERROR HANDLING:
C     Returns error message if called at the wrong level.
C
C ENVIRONMENT:
C     Digital VAX-11/780 VMS Version 4.4 (FORTRAN 77)
C
C AUTHOR:
C     Michael D. Wong, Sterling Software, NASA Ames Research Center
C
C HISTORY:
C-------------------------------------------------------------------------------
C
C     Arguments
C     ---------
C
      REAL
     >   XINCH, YINCH
C
C     Global variables
C     ----------------
C
      integer
     >   KZPAGE, KZAUTO, KZOR, KZLEVL, KZBEGN
      LOGICAL
     >   LOGX, LOGY
      REAL
     >   UDX, UDY, UX0, UY0, UUPAGX, UUPAGY, UUXOR, UUYOR, XCM0,
     >   XCM1, XVSTRT, XVLEN, YCM0, YCM1, YVLEN, YVSTRT, ZZPAGX,
     >   ZZPAGY, ZZPAGR, ZZUNIT, ZZXOR, ZZYOR
C
      common /CLEVEL/ KZLEVL, KZBEGN
      common /CPAGE/  ZZPAGX, ZZPAGY, ZZPAGR, UUPAGX, UUPAGY,
     >                KZPAGE, KZAUTO
      common /CPHYSR/ ZZXOR, ZZYOR, KZOR, UUXOR, UUYOR
      common /CUNIT/  ZZUNIT
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
C
C     Execution
C     ---------
C
      if(KZLEVL .NE. 3) go to 900
C
      YINVRS = (((ZZYOR + YCM0 + (YINCH * ZZUNIT * ZZPAGR)) - YVSTRT)
     >         * UDY / YVLEN) + UY0
C
      if(LOGY) YINVRS = 10 ** (YINVRS)
C
      go to 999
C
  900 CALL ERRMES ('YINVRS', 3, 3)
C
  999 return
      end
      subroutine YLABEL(LYNAME,IYNAME)
C
C
C
C
C      SET Y-AXIS LABEL
C               LEVEL 2 OR 3, P/S
C
C      INPUT:   LYNAME = Y-AXIS LABEL
C               IYNAME = NUMBER OF characterS IN LYNAME
C
      parameter (KZYES=111)
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      DIMENSION LYNAME(2)
C  CALLABLE FROM LEVEL 2 OR 3
C     if(KZLEVL.EQ.2)then
      if((KZLEVL.EQ.2).OR.(KZLEVL.EQ.3))then
        KZYNFL=KZYES
        if(IYNAME.EQ.0)then
          KZYALN=0
        else
          KZYALN=80
          call ZCOPYS(LYNAME,IYNAME,KZYLAB,KZYALN)
          if(KZYALN.LT.0)then
            KZYALN=-KZYALN
            call ERRMES('YLABEL',80,4)
          endif
        endif
      else
C  CALLABLE FROM LEVEL 2 OR 3
C       call ERRMES('YLABEL',2,0)
        call ERRMES('YLABEL',2,3)
      endif
      return
      end
      FUNCTION YLNLEG(IP,NLINES)
C
C
C
C
C     return Y LENGTH OF A PACKED ARRAY IN INCHES
C              (LEVEL 1-3)
C
C     INPUT:   IP     = PACKED character ARRAY
C              NLINES = NUMBER OF LINES IN IP
C
C     OUTPUT:  YLNLEG = LENGTH IN INCHES
C
      parameter (KZYES=111)
      parameter (KZNO=222)
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     .                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     .                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     .                CZLGTF(6),CZLGTS
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CUNIT/  ZZUNIT
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
C
      DIMENSION IP(*)
      EQUIVALENCE (IVAL,RVAL)
C
      SAVE JHITE,JYRAT
      data JHITE,JYRAT /-2,-1/
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
C
C                        RETRIEVE ARRAY INFO FROM IP
C
        if(IP(1).EQ.KZYES)then
          IPLEN=IP(2)/1000
          LLEN=IP(2)-IPLEN*1000
        else
          LLEN=40
        endif
        NWORD=(LLEN-1)/KZBYTE+5
C
C                        FIND INTER-LINE SPACE FOR 1-ST
C                        AND LAST LINE
C
        if(ZZLGTR.LT.1.0) ZZLGTR=1.0
C
C                        LOOP TO SUM CHAR HEIGHT AND
C                        INTER-LINE SPACE FACTOR
C
        ICUR=2
        KZLGBL=0
        BLKSUM=0.0
        if(ZZLGTZ.LT.0.0)then
          HITSUM=ZZHITE
        else
          HITSUM=ZZLGTZ
        endif
        if(ZZLGTR.LT.0.0)then
          RATSUM=1.5
        else
          RATSUM=ZZLGTR
        endif
        DO 100 I=1,NLINES
          ICUR=ICUR+NWORD
          IVAL=IP(ICUR+JHITE)
          HITSUM=HITSUM+RVAL
          IVAL=IP(ICUR+JYRAT)
          YRAT=RVAL
          if(YRAT.LT.1.0) YRAT=1.0
C
C                        IGNORE IF CURRENT LEGEND LINE HAS
C                           1. NO POINT CONNECTED, NO SYMBOL,
C                              SAVLIN ON
C                        OR 2. NO SYMBOL, SAVLIN OFF
C                        OR 3. NO TEXT ENTERED
C
          if((KZLGLT(I).EQ.0.AND.KZLGSM(I).LT.0.AND.
     .         KZLGLN.EQ.KZYES).OR.
     .        (KZLGSM(I).LT.0.AND.KZLGLN.EQ.KZNO).OR.
     .        (KZLGEN(I).EQ.KZNO))then
            BLKSUM=BLKSUM+YRAT
CW            KZLGBL=KZLGBL+1
          endif
          RATSUM=RATSUM+YRAT
 100    continue
        AVE=HITSUM/(NLINES+1)
CW        YLNLEG=(RATSUM-(ZZLGTR-1.0)/2.0)*AVE/ZZUNIT/ZZPAGR
CW        ZZLGYL=(RATSUM-(ZZLGTR-1.0)/2.0-BLKSUM)*AVE
        YLNLEG=RATSUM*AVE/ZZUNIT/ZZPAGR
        ZZLGYL=(RATSUM-BLKSUM)*AVE
C
C                        SAVE AVERAGE SIZE FOR CALL FROM
C                        ROUTINE 'TXTBLK'
C
        RVAL=AVE
        IP(NWORD+2)=IVAL
C
C                        WRONG LEVEL
C
      else
        YLNLEG=0.0
        call ERRMES('YLNLEG',2,3)
      endif
      return
      end
      subroutine YLOG(XORIGN,XSTEP,YORIGN,YCYCLE)
C
C
C
C      SET UP COORDINATE SYSTEM FOR LINEAR X AND LOG Y
C               (LEVEL 2, RAISE TO LEVEL 3)
C
C      INPUT:   XORIGN = LOWER LIMIT OF X AXIS
C               XSTEP  = STEP SIZE PER INCH FOR XAXIS
C               YORIGN = LOWER LIMIT OF Y AXIS
C               YCYCLE = CYCLE LENGTH PER INCHES
C
      parameter (KZLOG=3)
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
      common /CCOORD/ ZZXMIN,ZZXMAX,ZZXSTP,ZZYMIN,ZZYMAX,ZZYSTP
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /CUNIT/  ZZUNIT
      LOGICAL LOGX, LOGY
      if(KZLEVL.EQ.2)then
C
C                        INIT IAXES TO DEFINE LINEAR X AXIS
C                        AND DRAW TITLE
C
        LOGX=.FALSE.
        LOGY=.FALSE.
        IAXES=19
        XMIN=XORIGN
        YMIN=YORIGN
        YMAX=YMIN*10.0**(YCYCLE*ZZYAXS/ZZUNIT/ZZPAGR)
        XMAX=XMIN+XSTEP*ZZXAXS/ZZUNIT/ZZPAGR
        ZZYSTP=(YMAX-YMIN)/2.0
        ZZXSTP=XSTEP
        call ZMAPIT(XMIN,XMAX,YMIN,YMAX,KZXLAB,KZXALN,
     *              0,0,0.0,0.0,IAXES)
C
C                        DEFINE LOG Y AXIS
C
        DUMMY=0.0
        KZYTYP=KZLOG
        LFLAG=2
        XPOS=0.0
        YPOS=0.0
        call SCALOG(YMIN,YMAX,ZZYAXS,YOR,YCYC)
C  RESOLVE CONFLICT WITH SGI IRIS 4D FORTRAN LIBRARIES
C       call ZLOG(DUMMY,DUMMY,YOR,YCYC,DUMMY,ZZYAXS,
C    *         0,0,KZYLAB,KZYALN,XPOS,YPOS,LFLAG)
        call ZDRLOG(DUMMY,DUMMY,YOR,YCYC,DUMMY,ZZYAXS,
     *         0,0,KZYLAB,KZYALN,XPOS,YPOS,LFLAG)
        KZLEVL=3
      else
        call ERRMES('YLOG',2,0)
      endif
      return
      end
      subroutine YMARKS(ITICKS)
C
C
C
C      PURPSOE: SET NUMBER OF TICKS ON Y-AXIS
C               LEVEL 1-3, P/S
C
C      INPUT:   ITICKS = NUMBER OF TICKS PER STEP
C                        IF = 0, NO TICK DRAWN
C
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
C
      if(KZLEVL.GE.1.AND.KZLEVL.LE.3)then
        KZYTCK=ITICKS
      else
        call ERRMES('YMARKS',1,3)
      endif
      return
      end
      subroutine ZAXIS(BLOW,BHIGH,STEP,BTMIN,BTMAX,BTICK,IPWR)
C
C
C
C
C
C      THIS ROUTINE IS MAINLY FOR INTERNAL USE,
C      ITS FUNCTION IS TO DETERMINE A SUITABLE
C      "TICK" DISTANCE OVER THE RANGE SPECIFIED BETWEEN
C      ALOW AND AHIGH.   IT OUTPUTS THE AXIS RANGE BMIN,BMAX
C      AND THE TICK DISTANCE BTICK STRIPPED OF THEIR POWER OF
C      TEN.   THE POWER OF TEN IS returnED IN THE VAR. IPWR.
C
      LOGICAL LISNEG
C
      if(BHIGH .GE. BLOW)then
        BMAX = BHIGH
        BMIN = BLOW
        LISNEG = .FALSE.
      else
        BMAX = BLOW
        BMIN = BHIGH
        LISNEG = .TRUE.
      endif
      TEMP = AMAX1(ABS(BMIN),ABS(BMAX))
      IPWR=INT(ALOG10(TEMP))
C      if(IPWR.GT.1) IPWR=IPWR-1
      TENEXP=10.0**IPWR
      BTICK=STEP/TENEXP
      BTMIN = BMIN/TENEXP
      BTMAX = BMAX/TENEXP
C
C                        SWITCH BACK TO BACKWARDS
C
      if(LISNEG) CALL ZSWAP(BTMIN,BTMAX)
      return
      end
      subroutine ZBLANK(XPOS1,XPOS2,YPOS1,YPOS2,ID,IFRAME)
C
C
C
C
C      SET A BLANK AREA
C
C      INPUT:   XPOS1 XPOS2 = X LIMITS OF BLANK AREA
C               YPOS1 YPOS2 = Y LIMITS OF BLNAK AREA
C               ID          = AREA ID, MAX = 4
C               IFRAME      = THICKNESS OF FRAME
C
      parameter (KZMXBS=1000)
      parameter (KZMXBL=200)
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CUNIT/  ZZUNIT
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CBLANK/ ZZBLNK(KZMXBL,4),ZZBLKS(KZMXBS,4),KZBLCN,KZBSCN
C
      R=ZZUNIT*ZZPAGR
      if(IFRAME.LT.0)then
        JFRAME=-IFRAME
        DELTA=-0.01*R
      else
        JFRAME=IFRAME
        DELTA=0.01*R
      endif
      NUMFRM=JFRAME
C
C                        FORCE MIN AND MAX FOR IFRAME
C
      if(JFRAME.GT.4.AND.ID.LT.5)then
        NUMFRM=4
        call ERRMES('BLNK',ID,5)
      endif
C
C                        CALCULATE VIRTUAL COORDINATES
C
      X1=XPOS1*R+XCM0+ZZXOR
      X2=XPOS2*R+XCM0+ZZXOR
      Y1=YPOS1*R+YCM0+ZZYOR
      Y2=YPOS2*R+YCM0+ZZYOR
      if(XPOS1.GT.XPOS2)then
        ZZBLNK(ID,1)=X1
        ZZBLNK(ID,2)=X2
      else
        ZZBLNK(ID,1)=X2
        ZZBLNK(ID,2)=X1
      endif
      if(YPOS1.GT.YPOS2)then
        ZZBLNK(ID,3)=Y1
        ZZBLNK(ID,4)=Y2
      else
        ZZBLNK(ID,3)=Y2
        ZZBLNK(ID,4)=Y1
      endif
C
C                        DRAW FRAMES
C
      if(NUMFRM.NE.0)then
        DO 100 I=1,NUMFRM
          DEL=-DELTA*(I-1)
          call ZFRAME(ZZBLNK(ID,2),ZZBLNK(ID,1),ZZBLNK(ID,4),
     *             ZZBLNK(ID,3),DEL)
 100        continue
      endif
      return
      end
      subroutine ZBYTE4(CFLAG,INP,IPOS,KHAR)
C
C
C
C
C     EXTRACT A character FROM OR INSERT A
C              character TO A 4 BYTE integer
C
C     NOTE:    1. THIS ROUTINE IS TESTED GOOD ON
C                 VAX/VMS, AND IBM/PC. KZMASK AND KZCPOS ARE
C                 IN COMMON BLOCK 'CBYTE4', INITIALIZED IN
C                 'ZINIT'.
C              2. KZMASK IS THE MASK TO ZERO OUT THE BYTE TO
C                 WHICH A character IS TO BE INSERTED. FOR
C                 EXAMPLE, WHEN AN integer IS 'AND'ED WITH A
C                 MASK OF 7F7F7F00 HEX, THE LEAST SIGNIFICANT
C                 BYTE IS ZEROED.
C              3. KZNPOS IS POSITION OF THE BYTE OF INTEREST.
C                 FOR EXTRACTION, KZNPOS IS THE BITS TO SHIFT
C                 RIGHT TO PLACE THE DESIRED BYTE IN THE LEAST
C                 SIGNIFICANT POSITION. FOR INSERTION, KZNPOS
C                 IS THE BITS TO SHIFT LEFT TO PLACE THE BYTE
C                 TO BE INSERTED IN THE RIGHT POSITION.
C
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CBYTE4/ KZMASK(8),KZCPOS(8)
      character CFLAG
      integer INP(*)
C
C                        CALCULATE THE EXACT LOCATION
C
      if(IPOS.GT.KZBYTE)then
        ITEMP=(IPOS-1)/KZBYTE
        IPTR=ITEMP+1
        IP=IPOS-ITEMP*KZBYTE
      else
        IPTR=1
        IP=IPOS
      endif
C
C                        TO EXTRACT A character
C
      if(CFLAG.EQ.'X')then
        KHAR=KSYAND(INP(IPTR)/KZCPOS(IP),127)
C
C                        TO INSERT A character
C
      else
        INP(IPTR)=KSYAND(INP(IPTR),KZMASK(IP))+KHAR*KZCPOS(IP)
      endif
      return
      end
      subroutine ZCH2IN(CSTR,LSTR)
C
C
C
C
C
C      CONVERTS character ARRAY TO AN integer
C               ARRAY
C
C      INPUT:   CSTR      character ARRAY
C
C      OUTPUT:  LSTR      integer ARRAY
C
      integer LSTR(*)
      character*1 CSTR(1)
      LENGTH=NCHRAY(CSTR)
      DO 100 I=1,LENGTH+1
        KHAR=ICHAR(CSTR(I))
        call SYBYT4('N',LSTR,I,KHAR)
 100  continue
      return
      end
      subroutine ZCOPYS(LFROM,NFROM,LTO,NTO)
C
C
C
C
C
C      COPY A STRING
C
C      INPUT:   LFROM = ORIGINAL STRING
C               NFROM = NUMBER OF characterS IN LFROM
C
C      OUTPUT:  LTO   = DESTINATION STRING
C               NTO   = NUMBER OF characterS IN NTO; IF return
C                       NEGATIVE VALUE, STRING TRUNCATED
C
C
C     parameter (MAXINT=40)
C     parameter (MAXCHR=160)
C----------------------------BEGIN 32-BIT SPECIFIC CODE-------------------
      parameter (NUMBYT=4)
C-----------------------------END 32-BIT SPECIFIC CODE--------------------
C----------------------------BEGIN 64-BIT SPECIFIC CODE-------------------
C     parameter (NUMBYT=8)
C-----------------------------END 64-BIT SPECIFIC CODE--------------------
      parameter (MAXCHR=160)
      parameter (MAXINT=MAXCHR/NUMBYT)
C
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      DIMENSION LFROM(*),LTO(*),KFROM(MAXINT)
      character SFROM*160
C
C     EQUIVALENCE (KFROM,SFROM)
C
C
      SAVE KFROM
      data KFROM/MAXINT*0/
C
C                        FOR SELF TERMINATING STRING
C
      if(ABS(NFROM).EQ.100)then
        NCHAR=NZCHAR(LFROM,CZALFL)
        if(NCHAR.LT.0) NCHAR=NTO
        if(NCHAR.GE.NTO)then
C
C         NUM=(NTO+3)/4
          NUM=(NTO+(NUMBYT-1))/NUMBYT
          DO 100 I=1,NUM
            LTO(I)=LFROM(I)
 100      continue
          call SYBYT4('N',LTO(NTO),4,0)
          NTO=-(NTO-1)
        else
C
C         NUM=(NCHAR+3)/4
          NUM=(NCHAR+(NUMBYT-1))/NUMBYT
          DO 200 I=1,NUM
            LTO(I)=LFROM(I)
 200      continue
          NTO=100
        endif
C
C                        ALL OTHERS
C
      else
        if(NFROM.GE.NTO)then
C
C         NUM=(NTO+3)/4
          NUM=(NTO+(NUMBYT-1))/NUMBYT
          LOC=NTO
          NTO=-NTO
        else
C
C         NUM=(NFROM+3)/4
          NUM=(NFROM+(NUMBYT-1))/NUMBYT
          NTO=NFROM
          LOC=NFROM+1
        endif
C
C       DO 300 I=1,NUM
C         KFROM(I)=LFROM(I)
        K=1
        DO 300 I=1,NUM
          call WIN2CH(KFROM(I),SFROM(K:K+NUMBYT-1))
          K=K+NUMBYT
C
 300    continue
        SFROM(LOC:LOC)=CHAR(0)
C
C       call ZSCOPY(KFROM,LTO)
        K=1
        DO 400 I=1,NUM
          call WCH2IN(SFROM(K:K+NUMBYT-1),LTO(I))
          K=K+NUMBYT
 400    continue
C
      endif
      return
      end
      subroutine ZCOSTR(LFROM,NF,LTO,NT)
C
C
C
C
C      COPY A STRING, AND CALCULATE ITS LENGTH
C               IF NOT PROVIDED
C
C      INPUT:   LFROM = INPUT STRING
C               NF    = NUMBER OF characterS IN LFROM
C
C      OUTPUT:  LTO = OUTPUT STRING
C               NT  = NUMBER OF characterS IN LTO
C
      parameter (MAXCHR=160)
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE----------------------
      parameter (MAXINT=MAXCHR/NUMBYT)
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
C
      DIMENSION LFROM(*),LTO(*)
      character SFROM*160,S1*1,S2*2,S3*3,S4*4,STERM*(NUMBYT)
      JTERM=KZSTRM
      call WIN2CH(JTERM,STERM)
C
C                        IF STRING TERMINATOR IS USED,
C                        LOCATE TERMINATOR AND REPLACE IT
C                        WITH A NULL character
C
      if(ABS(NF).EQ.100)then
        K=1
        DO 100 I=1,MAXINT
          call WIN2CH(LFROM(I),SFROM(K:K+NUMBYT-1))
          K=K+NUMBYT
 100    continue
        if(KZTMLN.EQ.1)then
          S1=STERM(1:1)
          LOC=INDEX(SFROM,S1)
        elseif(KZTMLN.EQ.2)then
          S2=STERM(1:2)
          LOC=INDEX(SFROM,S2)
        elseif(KZTMLN.EQ.3)then
          S3=STERM(1:3)
          LOC=INDEX(SFROM,S3)
        elseif(KZTMLN.EQ.4)then
          S4=STERM
          LOC=INDEX(SFROM,S4)
        endif
        if(LOC.GT.MAXCHR)then
          LOC=MAXCHR
        endif
        if(LOC.GE.NF+1)then
          LOC=NF+1
        endif
        SFROM(LOC:LOC)=CHAR(0)
        K=1
        DO 150 I=1,MAXINT
          call WCH2IN(SFROM(K:K+NUMBYT-1),LTO(I))
          K=K+NUMBYT
 150    continue
C
C                        IF TERMINATOR IS NOT USED
C
      else
        NUM=(NF+(NUMBYT-1))/NUMBYT
        K=1
        DO 200 I=1,NUM
          call WIN2CH(LFROM(I),SFROM(K:K+NUMBYT-1))
          K=K+NUMBYT
 200    continue
        LOC=NF+1
        if(LOC.GT.MAXCHR)then
          LOC=MAXCHR
        endif
        SFROM(LOC:LOC)=CHAR(0)
        K=1
        DO 250 I=1,MAXINT
          call WCH2IN(SFROM(K:K+NUMBYT-1),LTO(I))
          K=K+NUMBYT
 250    continue
      endif
      NT=LOC-1
      return
      end
      subroutine ZCURCS(XX,YY,NPTS)
C
C
C
C      DRAW CURVE USING CUBIC SPLINE INTERPOLATION
C
C      INPUT:   XX,YY = ARRAY OF POINTS
C               NPTS  = NUMBER OF COORDINATE PAIRS IN XX
C                       AND YY
C
      parameter (KZYES=111)
      parameter (KZNO=222)
      parameter (KZCSPL=3)
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      LOGICAL LOGX, LOGY
C
      DIMENSION TAU(104),C(4,104),XX(*),YY(*)
      DIMENSION AREA(4),X(102),Y(102)
      LOGICAL BAD
C
C                        IF NON-INCREASING X EXISTS, USE
C                        PARAMETRIC POLYNOMIAL INTERPOLATION
C
      DO 300 I=1,NPTS
        call SCALE(XX(I),YY(I),X(I),Y(I))
 300  continue
      call ZGSCLP(AREA)
      DO 90 I=1,NPTS-1
        if(X(I+1).LE.X(I))then
          if(NPTS.LE.51)then
            call ZCURPS(XX,YY,NPTS)
          else
            call ZCURPP('V',X,Y,NPTS)
          endif
          return
        endif
 90   continue
C
C                        ADD EXTRA POINTS AT BOTH ENDS TO
C                        CONTROL BOUNDARY SLOPE
C
      TAU(1)=2.0*X(1)-X(2)
      C(1,1)=2.0*Y(1)-Y(2)
      TAU(NPTS+2)=2.0*X(NPTS)-X(NPTS-1)
      C(1,NPTS+2)=2.0*Y(NPTS)-Y(NPTS-1)
      DO 100 I=2,NPTS+1
        TAU(I)=X(I-1)
        C(1,I)=Y(I-1)
 100  continue
C
C                        CALCULATE CUBIC SPLINE MATRIX C
C
      call ZCUSPL(TAU,C,NPTS+2,0,0,BAD)
      if(BAD)then
          if(NPTS.LE.51)then
          call ZCURPS(XX,YY,NPTS)
        else
          call ZCURPP('V',X,Y,NPTS)
        endif
        return
      endif
C
C                        DRAW THE INTERPOLATED CURVE
C
      call DSMOVE(X(1),Y(1))
      KZLBEG=KZYES
      KZMOV=KZNO
      ZZPRX=X(1)
      ZZPRY=Y(1)
      ZZPRX1=X(1)
      ZZPRY1=Y(1)
      ZZPRX2=X(1)
      ZZPRY2=Y(1)
      DO 200 I=2,NPTS
        DT=X(I)-X(I-1)
        call ZCUREV(KZCSPL,X(I-1),Y(I-1),X(I),Y(I),0.0,DT,
     *              0.0,0.0,1.0,X(I-1),
     *              C(4,I),C(3,I),C(2,I),C(1,I))
 200  continue
      call GSRCLP(AREA)
      return
      end
      subroutine ZCURDR(X,Y,ILNOLD)
C
C
C
C
C     DRAW A LINE FROM (X0,Y0) TO (X1,Y1) IN
C              ABSOLUTE COORDINATES. ASSUMES THAT CLIPPING
C              HAS ALREADY BEEN DONE.   TO SUPPRESS UNNECESSARY
C              "MOVES", THIS IS THE ONLY ROUTINE THAT SHOULD
C              call GSDRVR(3,,,). THE LINE IS DRAWN IN THE
C              CURRENT LINE TYPE.   THIS ROUTINE DOES NOT
C              SET THE ABSOLUTE POSITION (XAPOS,YAPOS). IT
C              IS UP TO THE CALLER TO DO SO IF NECESSARY.
C
C     INPUT:   X,Y  = ARRAYS OF POINTS
C              NPTS = NUMBER OF COORDINATE PAIRS IN X AND Y
C
      parameter (KZYES=111)
      parameter (KZNO=222)
      parameter (KZSOLI=1)
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      LOGICAL LINILT, LPOSND
C
      DIMENSION X(*),Y(*),TX(4),TY(4),HX(4),HY(4)
C
C                        LOCATE LONG SIDE AND DO CROSS HATCH
C                        IN SAME DIRECTION
C
      DX=X(1)-X(2)
      DY=Y(1)-Y(2)
      RLEN=DX*DX+DY*DY
      DX1=X(2)-X(3)
      DY1=Y(2)-Y(3)
      RLEN1=DX1*DX1+DY1*DY1
      if(RLEN1.GT.RLEN)then
        DX=DX1
        DY=DY1
      endif
C
C                        FOR SOLID LINE, CROSS HATCH THE
C                        INPUT POLYGON
C
      if(ILNOLD.EQ.KZSOLI)then
          call DSFILL(X,Y,4,TX,TY)
C
C                        OTHERWISE, DO IT ACCORDING TO
C                        CURRENT LINE STYLE
C
      else
        X0=X(1)
        Y0=Y(1)
        X1=X(4)
        Y1=Y(4)
        HX(1)=X(1)
        HY(1)=Y(1)
        HX(4)=X(4)
        HY(4)=Y(4)
C
C      SEGMENT LINE TO MAKE CURRENT LINE TYPE
C
        if(KZLBEG.NE.KZNO)then
          INXTL = 1
          DLEFT = DIST(1,ILNOLD-1)
          KZLBEG = KZNO
        endif
100     continue
        DX = X(2)-X0
        DY = Y(2)-Y0
        DL = SQRT(DX**2+DY**2)
C
C      SEE IF THIS SEGMENT IS SHORTER THAT DIST. LEFT ON LINE TYPE
C
        if(DL .GT. DLEFT)then
C
C      SEGMENT IS LONGER, SO ADVANCE TO LINE TYPE BREAK
C
          S = DLEFT/DL
          X0 = S*DX+X0
          Y0 = S*DY+Y0
          X1 = S*DX+X1
          Y1 = S*DY+Y1
C
C      SEE IF THIS PART OF THE LINE TYPE IS DRAWN OR SKIPPED
C
          if(KSYAND(INXTL,1) .NE. 0)then
            DX1=HX(1)-X0
            DY1=HY(1)-Y0
            RLEN1=DX1*DX1+DY1*DY1
            DX1=HX(4)-X0
            DY1=HY(4)-Y0
            RLEN2=DX1*DX1+DY1*DY1
            if(RLEN1.LT.RLEN2)then
              HX(2)=X0
              HY(2)=Y0
              HX(3)=X1
              HY(3)=Y1
            else
              HX(2)=X1
              HY(2)=Y1
              HX(3)=X0
              HY(3)=Y0
            endif
              call DSFILL(HX,HY,4,TX,TY)
            HX(1)=HX(2)
            HY(1)=HY(2)
            HX(4)=HX(3)
            HY(4)=HY(3)
          else
            HX(1)=X0
            HY(1)=Y0
            HX(4)=X1
            HY(4)=Y1
          endif
C
C      NOW go to NEXT PORTION OF LINE TYPE
C
          INXTL = INXTL + 1
          if(INXTL .GT. DIST(13,ILNOLD-1)) INXTL = 1
          DLEFT = DIST(INXTL,ILNOLD-1)
          go to 100
C
C      DRAW LAST OF LINE IF DRAWN
C
        endif
        DLEFT = DLEFT - DL
        if(KSYAND(INXTL,1) .NE. 0)then
          DX1=HX(1)-X(2)
          DY1=HY(1)-Y(2)
          RLEN1=DX1*DX1+DY1*DY1
          DX1=HX(4)-X(2)
          DY1=HY(4)-Y(2)
          RLEN2=DX1*DX1+DY1*DY1
          if(RLEN1.LT.RLEN2)then
            HX(2)=X(2)
            HY(2)=Y(2)
            HX(3)=X(3)
            HY(3)=Y(3)
          else
            HX(2)=X(3)
            HY(2)=Y(3)
            HX(3)=X(2)
            HY(3)=Y(2)
          endif
            call DSFILL(HX,HY,4,TX,TY)
          KZMOV = KZYES
        else
          KZMOV = KZNO
        endif
      endif
      return
      end
      subroutine ZCUREV(IFLAG,X1,Y1,X2,Y2,T,DT,AX1,BX1,CX,DX,
     *                    AY1,BY1,CY,DY)
C
C
C
C
C
C      DRAW SEGMENT BY INCREMENT TAU
C
C      INPUT:   IFLAG = LINE STYLE
C               X1,Y1 = STARTING POINT
C               X2,Y2 = ENDING POINT
C               T     = STARTING VALUE OF TAU
C               DT    = INCREMENT OF TAU
C               AX1,BX1,CX,DX = PARAMETRIC COEF. OF X
C               AY1,BY1,CY,DY = PARAMETRIC COEF. OF Y
C
C      NOTE:    IFLAG = KZCSPL OR KZPSPL, FOR PARAMETRIC
C                               SPLINE INTERPOLATION
C               IFLAG = KZPPLY, FOR PARAMETRIC POLYNOMIAL
C                               INTERPOLATION
C
      parameter (KZNO=222)
      parameter (KZCSPL=3)
      parameter (KZPSPL=4)
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      LOGICAL LOGX, LOGY
C
      if(IFLAG.EQ.KZCSPL.OR.IFLAG.EQ.KZPSPL)then
        AX=AX1/6.0
        BX=BX1/2.0
        AY=AY1/6.0
        BY=BY1/2.0
      else
        AX=AX1
        BX=BX1
        AY=AY1
        BY=BY1
      endif
      DLEN=SQRT((X1-X2)**2+(Y1-Y2)**2)
      IDIV=INT(DLEN/0.3)+1
      DTAU=DT/REAL(IDIV)
      H=T
      if(KZLTHK.EQ.KZNO)then
        DO 100 J=1,IDIV
          H=H+DTAU
          XT=DX+H*(CX+H*(BX+H*AX))
          YT=DY+H*(CY+H*(BY+H*AY))
          call DSDRAW(XT,YT)
 100    continue
      else
        DO 200 J=1,IDIV
          H=H+DTAU
          XT=DX+H*(CX+H*(BX+H*AX))
          YT=DY+H*(CY+H*(BY+H*AY))
          call ZCURTH(XT,YT)
 200    continue
      endif
      return
      end
      subroutine ZCURPP(CFLAG,X,Y,NPTS)
C
C
C
C      DRAW CURVE USING PARAMETRIC CUBIC POLYNOMIAL
C               INTERPOLATION
C
C      INPUT:   CFLAG = FLAG FOR COORDINATE SYSTEM
C                       IF 'W', INPUT ARE IN WORLD COORDINATE
C                       UNITS, OTHERWISE VIRTUAL COORDINATE
C               X,Y   = ARRAY OF POINTS
C               NPTS  = NUMBER OF POINTS IN X AND Y
C
      parameter (KZYES=111)
      parameter (KZNO=222)
      parameter (KZPPLY=5)
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      LOGICAL LOGX, LOGY
C
      DIMENSION X(*),Y(*)
      DIMENSION AREA(4)
      character CFLAG
      call ZGSCLP(AREA)
C
C                        IF INPUTS ARE IN WORLD COOR
C
      if(CFLAG.EQ.'W')then
C
C                        DO CURVE CONNECTING FIRST 3 POINTS
C
        call SCALE(X(1),Y(1),VX1,VY1)
        call SCALE(X(2),Y(2),VX2,VY2)
        call SCALE(X(3),Y(3),VX3,VY3)
        call SCALE(X(4),Y(4),VX4,VY4)
        call ZPPOLY(VX1,VX2,VX3,VX4,AX,BX,CX,DX)
        call ZPPOLY(VY1,VY2,VY3,VY4,AY,BY,CY,DY)
        call DSMOVE(VX1,VY1)
        KZLBEG=KZYES
        KZMOV=KZNO
        ZZPRX=VX1
        ZZPRY=VY1
        ZZPRX1=VX1
        ZZPRY1=VY1
        ZZPRX2=VX1
        ZZPRY2=VY1
        call ZCUREV(KZPPLY,VX1,VY1,VX2,VY2,0.0,1.0,
     *              AX,BX,CX,DX,AY,BY,CY,DY)
        call ZCUREV(KZPPLY,VX2,VY2,VX3,VY3,1.0,1.0,
     *              AX,BX,CX,DX,AY,BY,CY,DY)
        VX1=VX2
        VX2=VX3
        VX3=VX4
        VY1=VY2
        VY2=VY3
        VY3=VY4
C
C                        PROCESS FROM 4-TH POINT ON
C
        if(NPTS.GE.5)then
          DO 100 I=3,NPTS-2
            call SCALE(X(I+2),Y(I+2),VX4,VY4)
            call ZPPOLY(VX1,VX2,VX3,VX4,AX,BX,CX,DX)
            call ZPPOLY(VY1,VY2,VY3,VY4,AY,BY,CY,DY)
            call ZCUREV(KZPPLY,VX2,VY2,VX3,VY3,1.0,1.0,
     *                  AX,BX,CX,DX,AY,BY,CY,DY)
            VX1=VX2
            VX2=VX3
            VX3=VX4
            VY1=VY2
            VY2=VY3
            VY3=VY4
 100      continue
        endif
C
C                        DO LAST SEGMENT
C
        call ZCUREV(KZPPLY,VX2,VY2,VX3,VY3,
     *                2.0,1.0,AX,BX,CX,DX,AY,BY,CY,DY)
C
C                        IF INPUTS ARE IN VIRTUAL COOR
C
      else
C
C                        CONNECT FIRST 3 POINTS
C
        call ZPPOLY(X(1),X(2),X(3),X(4),AX,BX,CX,DX)
        call ZPPOLY(Y(1),Y(2),Y(3),Y(4),AY,BY,CY,DY)
        call DSMOVE(X(1),Y(1))
        KZLBEG=KZYES
        KZMOV=KZNO
        ZZPRX=X(1)
        ZZPRY=Y(1)
        ZZPRX1=X(1)
        ZZPRY1=Y(1)
        ZZPRX2=X(1)
        ZZPRY2=Y(1)
        call ZCUREV(KZPPLY,X(1),Y(1),X(2),Y(2),0.0,1.0,
     *              AX,BX,CX,DX,AY,BY,CY,DY)
        call ZCUREV(KZPPLY,X(2),Y(2),X(3),Y(3),1.0,1.0,
     *              AX,BX,CX,DX,AY,BY,CY,DY)
C
C                        DO 4-TH POINT AND ON
C
        if(NPTS.GE.5)then
          DO 200 I=3,NPTS-2
            call ZPPOLY(X(I-1),X(I),X(I+1),X(I+2),AX,BX,CX,DX)
            call ZPPOLY(Y(I-1),Y(I),Y(I+1),Y(I+2),AY,BY,CY,DY)
            call ZCUREV(KZPPLY,X(I),Y(I),X(I+1),Y(I+1),1.0,1.0,
     *                    AX,BX,CX,DX,AY,BY,CY,DY)
 200      continue
        endif
C
C                        LAST SEGMENT
C
        call ZCUREV(KZPPLY,X(NPTS-1),Y(NPTS-1),X(NPTS),Y(NPTS),
     *                2.0,1.0,AX,BX,CX,DX,AY,BY,CY,DY)
      endif
      call GSRCLP(AREA)
      return
      end
      subroutine ZCURPS(XX,YY,NPTS)
C
C
C
C           DRAW CURVE USING CUBIC SPLINE INTERPOLATION
C
C      INPUT:            XX,YY      ARRAYS OF POINTS
C                  NPTS      NUMBER OF COORDINATE PAIRS IN XX
C                        AND YY
C
      parameter (KZYES=111)
      parameter (KZNO=222)
      parameter (KZPSPL=4)
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      LOGICAL LOGX, LOGY
C
      DIMENSION TAU(53),CX(4,53),CY(4,53),XX(*),YY(*)
      DIMENSION AREA(4),X(51),Y(51)
      LOGICAL BAD
C
C                        ADD EXTRA POINTS AT BOTH ENDS TO
C                        CONTROL BOUNDARY SLOPE
C
      DO 300 I=1,NPTS
        call SCALE(XX(I),YY(I),X(I),Y(I))
 300  continue
      call ZGSCLP(AREA)
      TAU(1)=0.0
      CX(1,1)=2.0*X(1)-X(2)
      CY(1,1)=2.0*Y(1)-Y(2)
      TAU(NPTS+2)=REAL(NPTS+1)
      CX(1,NPTS+2)=2.0*X(NPTS)-X(NPTS-1)
      CY(1,NPTS+2)=2.0*Y(NPTS)-Y(NPTS-1)
      DO 100 I=2,NPTS+1
        CX(1,I)=X(I-1)
        CY(1,I)=Y(I-1)
        TAU(I)=REAL(I-1)
 100  continue
      call ZCUSPL(TAU,CX,NPTS+2,0,0,BAD)
      call ZCUSPL(TAU,CY,NPTS+2,0,0,BAD)
      call DSMOVE(X(1),Y(1))
      KZLBEG=KZYES
      KZMOV=KZNO
      ZZPRX=X(1)
      ZZPRY=Y(1)
      ZZPRX1=X(1)
      ZZPRY1=Y(1)
      ZZPRX2=X(1)
      ZZPRY2=Y(1)
      DO 200 I=2,NPTS
        call ZCUREV(KZPSPL,X(I-1),Y(I-1),X(I),Y(I),0.0,1.0,
     *                CX(4,I),CX(3,I),CX(2,I),CX(1,I),
     *                CY(4,I),CY(3,I),CY(2,I),CY(1,I))
 200  continue
      call GSRCLP(AREA)
      return
      end
      subroutine ZCURTH(XX,YY)
C
C
C
C
C     TO DRAW THICK LINE
C
C     INPUT: XX,YY - COORDINATE TO DRAW TO (ABSOLUTE)
C
      parameter (KZYES=111)
      parameter (KZLIN=2)
      parameter (KZMAXC=255)
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /COLORN/ KZSHD,ZZHSPC,ZZCOLR(KZMAXC,3),KZNCLR,KZCCOL
      common /COLORC/ CZCOLR
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
      common /GCVPOS/ XVPOS, YVPOS
      common /GCCPOS/ XAPOS, YAPOS, IVIS, LCURNT
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
      character CZCOLR*8
      LOGICAL LCURNT
      LOGICAL LINILT, LPOSND
C
      integer GSIVIS
      DIMENSION X(4),Y(4),TX(4),TY(4),AX(4),AY(4)
C
      SAVE EPSLON
      data EPSLON /0.0001/
C
C                        CALCULATE 4 CORNERS OF AREA TO BE
C                        FILLED
C
      ILNOLD=ILNTYP
      ILNTYP=1
      THICK2=ZZLTHK/2.0
      X1=XVPOS
      Y1=YVPOS
      X2=XX
      Y2=YY
      DIRX2=X2-X1
      DIRY2=Y2-Y1
C
C                        FOR VERTICAL LINE
C
      if(ABS(DIRX2).LT.EPSLON)then
        AX(1)=X1+THICK2
        AX(2)=AX(1)
        AX(3)=X1-THICK2
        AX(4)=AX(3)
        AY(1)=Y1
        AY(2)=Y2
        AY(3)=Y2
        AY(4)=Y1
C
C                        NON-VERTICAL LINE
C
      else
        TANVAL=DIRY2/DIRX2
        THETA=ATAN(TANVAL)
        if(DIRX2.LT.0.0)then
          COST=-COS(THETA)
          SINT=-SIN(THETA)
        else
          COST=COS(THETA)
          SINT=SIN(THETA)
        endif
        if(DIRY2.LT.0.0)then
          DY=-THICK2*COST
          DX=THICK2*SINT
        else
          DY=THICK2*COST
          DX=-THICK2*SINT
        endif
        AX(1)=X1+DX
        AX(4)=X1-DX
        AY(1)=Y1+DY
        AY(4)=Y1-DY
        AX(2)=X2+DX
        AX(3)=X2-DX
        AY(2)=Y2+DY
        AY(3)=Y2-DY
      endif
C
C                        FILL IN INTER-LINE SPACE
C
      if(KZMOV.EQ.KZYES)then
        DIRPX=ZZPRX1-X1
        DIRPY=ZZPRY1-Y1
        DOTPRD=DIRPX*DIRX2+DIRPY*DIRY2
        if(DOTPRD.GT.0.0)then
          X(3)=ZZPRX2
          Y(3)=ZZPRY2
        else
          X(3)=ZZPRX1
          Y(3)=ZZPRY1
        endif
        DIRX1=ZZPRX-X1
        DIRY1=ZZPRY-Y1
        DIRPX=AX(1)-X1
        DIRPY=AY(1)-Y1
        DOTPRD=DIRPX*DIRX1+DIRPY*DIRY1
        if(DOTPRD.GT.0.0)then
          X(1)=AX(4)
          Y(1)=AY(4)
        else
          X(1)=AX(1)
          Y(1)=AY(1)
        endif
        X(2)=X1
        Y(2)=Y1
        if(KZLTYP.EQ.KZLIN)then
          NUMPT=4
          if(ABS(DIRX1).LT.EPSLON)then
            if(ABS(DIRX2).LT.EPSLON)then
              NUMPT=0
            elseif(ABS(DIRY2).LT.EPSLON)then
              X(4)=X(3)
              Y(4)=Y(1)
            else
              S2=DIRY2/DIRX2
              Y(4)=(X(3)-X(1))*S2+Y(1)
              X(4)=(Y(4)-Y(1))/S2+X(1)
            endif
          elseif(ABS(DIRY1).LT.EPSLON)then
            Y(4)=Y(3)
            if(ABS(DIRX2).LT.EPSLON)then
              X(4)=X(1)
            elseif(ABS(DIRY2).LT.EPSLON)then
              NUMPT=0
            else
              S2=DIRY2/DIRX2
              X(4)=(Y(4)-Y(1))/S2+X(1)
            endif
          else
            S1=DIRY1/DIRX1
            if(ABS(DIRX2).LT.EPSLON)then
              X(4)=X(1)
              Y(4)=(X(4)-X(3))*S1+Y(3)
            elseif(ABS(DIRY2).LT.EPSLON)then
              Y(4)=Y(1)
              X(4)=(Y(4)-Y(3))/S1+X(3)
            else
              S2=DIRY2/DIRX2
              DS=S2-S1
              if(ABS(DS).LT.EPSLON)then
                NUMPT=0
              else
                B1=Y(3)-X(3)*S1
                B2=Y(1)-X(1)*S2
                X(4)=(B1-B2)/DS
                Y(4)=X(4)*S1+B1
              endif
            endif
          endif
          if(NUMPT.NE.0) CALL DHATCH(X,Y,4,90.0,ZZHSPC,1,TX,TY)
C
C                        DRAW SMOOTH CORNER IN INTER-LINE
C                        SPACE
C
        else
          call DHATCH(X,Y,3,90.0,ZZHSPC,1,TX,TY)
        endif
      endif
      call ZCURDR(AX,AY,ILNOLD)
      ZZPRX=X1
      ZZPRY=Y1
      ZZPRX1=AX(2)
      ZZPRY1=AY(2)
      ZZPRX2=AX(3)
      ZZPRY2=AY(3)
      XVPOS = XX
      YVPOS = YY
      call GSRST(XVPOS,YVPOS,XA1,YA1)
      IVIS1 = GSIVIS(XA1,YA1)
      XAPOS = XA1
      YAPOS = YA1
      IVIS = IVIS1
      ILNTYP=ILNOLD
      return
      end
      subroutine ZCURVE(X,Y,NUMPTS)
C
C
C
C
C     THIS ROUTINE TRACES THE LINE FROM X(1),Y(1) TO
C     X(NPTS),Y(NPTS) WITH APPROPIATE CLIPPING.
C
      parameter (KZYES=111)
      parameter (KZNO=222)
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
C
      DIMENSION AREA(4)
      DIMENSION X(*), Y(*)
      call ZGSCLP(AREA)
      NPTS=ABS(NUMPTS)
      if(NPTS .GT. 1)then
        if(NUMPTS.LT.0)then
          VX=X(1)
          VY=Y(1)
        else
          call SCALE(X(1),Y(1),VX,VY)
        endif
        call DSMOVE(VX,VY)
        KZMOV=KZNO
        KZLBEG=KZYES
        ZZPRX=VX
        ZZPRY=VY
        ZZPRX1=VX
        ZZPRY1=VY
        ZZPRX2=VX
        ZZPRY2=VY
C
C                        IN VIRTUAL COORDINATES
C
        if(NUMPTS.LT.0)then
          if(KZLTHK.EQ.KZNO)then
            DO 100 I=2,NPTS
              call DSDRAW(X(I),Y(I))
100         continue
          else
            DO 200 I=2,NPTS
              call ZCURTH(X(I),Y(I))
200         continue
          endif
C
C                        IN WORLD COORDINATES
C
        else
          if(KZLTHK.EQ.KZNO)then
            DO 300 I=2,NPTS
              call SCALE(X(I),Y(I),VX,VY)
              call DSDRAW(VX,VY)
300         continue
          else
            DO 400 I=2,NPTS
              call SCALE(X(I),Y(I),VX,VY)
              call ZCURTH(VX,VY)
400         continue
          endif
        endif
      endif
      call GSRCLP(AREA)
      return
      end
      subroutine ZCUSPL ( TAU, C, N, IBCBEG, IBCEND ,BAD)
C
C
C
C
C  FROM  * A PRACTICAL GUIDE TO SPLINES *  BY C. DE BOOR
C     ************************  INPUT  ***************************
C     N = NUMBER OF DATA POINTS. ASSUMED TO BE .GE. 2.
C     (TAU(I), C(1,I), I=1,...,N) = ABSCISSAE AND ORDINATES OF THE
C        data POINTS. TAU IS ASSUMED TO BE STRICTLY INCREASING.
C     IBCBEG, IBCEND = BOUNDARY CONDITION INDICATORS, AND
C     C(2,1), C(2,N) = BOUNDARY CONDITION INFORMATION. SPECIFICALLY,
C        IBCBEG = 0  MEANS NO BOUNDARY CONDITION AT TAU(1) IS GIVEN.
C           IN THIS CASE, THE NOT-A-KNOT CONDITION IS USED, I.E. THE
C           JUMP IN THE THIRD DERIVATIVE ACROSS TAU(2) IS FORCED TO
C           ZERO, THUS THE FIRST AND THE SECOND CUBIC POLYNOMIAL PIECES
C           ARE MADE TO COINCIDE.)
C        IBCBEG = 1  MEANS THAT THE SLOPE AT TAU(1) IS MADE TO EQUAL
C           C(2,1), SUPPLIED BY INPUT.
C        IBCBEG = 2  MEANS THAT THE SECOND DERIVATIVE AT TAU(1) IS
C           MADE TO EQUAL C(2,1), SUPPLIED BY INPUT.
C        IBCEND = 0, 1, OR 2 HAS ANALOGOUS MEANING CONCERNING THE
C           BOUNDARY CONDITION AT TAU(N), WITH THE ADDITIONAL INFOR-
C           MATION TAKEN FROM C(2,N).
C     ***********************  OUTPUT  **************************
C     C(J,I), J=1,...,4; I=1,...,L (= N-1) = THE POLYNOMIAL COEFFICIENTS
C        OF THE CUBIC INTERPOLATING SPLINE WITH INTERIOR KNOTS (OR
C        JOINTS) TAU(2), ..., TAU(N-1). PRECISELY, IN THE INTERVAL
C        INTERVAL (TAU(I), TAU(I+1)), THE SPLINE F IS GIVEN BY
C           F(X) = C(1,I)+H*(C(2,I)+H*(C(3,I)+H*C(4,I)/3.)/2.)
C        WHERE H = X - TAU(I). THE FUNCTION PROGRAM *PPVALU* MAY BE
C        USED TO EVALUATE F OR ITS DERIVATIVES FROM TAU,C, L = N-1,
C        AND K=4.
      LOGICAL BAD
      integer IBCBEG,IBCEND,N,   I,J,L,M
      REAL C(4,N),TAU(N),   DIVDF1,DIVDF3,DTAU,G
C****** A TRIDIAGONAL LINEAR SYSTEM FOR THE UNKNOWN SLOPES S(I) OF
C  F  AT TAU(I), I=1,...,N, IS GENERATED AND THEN SOLVED BY GAUSS ELIM-
C  INATION, WITH S(I) ENDING UP IN C(2,I), ALL I.
C     C(3,.) AND C(4,.) ARE USED INITIALLY FOR TEMPORARY STORAGE.
C
      SAVE EPSLON
      data EPSLON /0.000001/
      BAD = .FALSE.
      L = N-1
COMPUTE FIRST DIFFERENCES OF TAU SEQUENCE AND STORE IN C(3,.). ALSO,
COMPUTE FIRST DIVIDED DIFFERENCE OF DATA AND STORE IN C(4,.).
      DO 10 M=2,N
         C(3,M) = TAU(M) - TAU(M-1)
         if(ABS(C(3,M)).LE.EPSLON)then
           BAD=.TRUE.
           return
         endif
   10    C(4,M) = (C(1,M) - C(1,M-1))/C(3,M)
CONSTRUCT FIRST EQUATION FROM THE BOUNDARY CONDITION, OF THE FORM
C             C(4,1)*S(1) + C(3,1)*S(2) = C(2,1)
      if(IBCBEG-1)                     11,15,16
   11 if(N .GT. 2)                     go to 12
C     NO CONDITION AT LEFT END AND N = 2.
      C(4,1) = 1.
      C(3,1) = 1.
      C(2,1) = 2.*C(4,2)
                                        go to 25
C     NOT-A-KNOT CONDITION AT LEFT END AND N .GT. 2.
   12 C(4,1) = C(3,3)
      C(3,1) = C(3,2) + C(3,3)
      if(ABS(C(3,1)).LE.EPSLON)then
        BAD=.TRUE.
        return
      endif
      C(2,1) =((C(3,2)+2.*C(3,1))*C(4,2)*C(3,3)+C(3,2)**2*C(4,3))/C(3,1)
                                        go to 19
C     SLOPE PRESCRIBED AT LEFT END.
   15 C(4,1) = 1.
      C(3,1) = 0.
                                        go to 18
C     SECOND DERIVATIVE PRESCRIBED AT LEFT END.
   16 C(4,1) = 2.
      C(3,1) = 1.
      if(ABS(C(2,1)).LE.EPSLON)then
        BAD=.TRUE.
        return
      endif
      C(2,1) = 3.*C(4,2) - C(3,2)/2.*C(2,1)
   18 IF(N .EQ. 2)                      go to 25
C  IF THERE ARE INTERIOR KNOTS, GENERATE THE CORRESP. EQUATIONS AND CAR-
C  RY OUT THE FORWARD PASS OF GAUSS ELIMINATION, AFTER WHICH THE M-TH
C  EQUATION READS    C(4,M)*S(M) + C(3,M)*S(M+1) = C(2,M).
   19 DO 20 M=2,L
         if(ABS(C(4,M-1)).LE.EPSLON)then
           BAD=.TRUE.
           return
         endif
         G = -C(3,M+1)/C(4,M-1)
         C(2,M) = G*C(2,M-1) + 3.*(C(3,M)*C(4,M+1)+C(3,M+1)*C(4,M))
   20    C(4,M) = G*C(3,M-1) + 2.*(C(3,M) + C(3,M+1))
CONSTRUCT LAST EQUATION FROM THE SECOND BOUNDARY CONDITION, OF THE FORM
C           (-G*C(4,N-1))*S(N-1) + C(4,N)*S(N) = C(2,N)
C     IF SLOPE IS PRESCRIBED AT RIGHT END, ONE CAN GO DIRECTLY TO BACK-
C     SUBSTITUTION, SINCE C ARRAY HAPPENS TO BE SET UP JUST RIGHT FOR IT
C     AT THIS POINT.
      if(IBCEND-1)                     21,30,24
   21 if(N .EQ. 3 .AND. IBCBEG .EQ. 0) go to 22
C     NOT-A-KNOT AND N .GE. 3, AND EITHER N.GT.3 OR  ALSO NOT-A-KNOT AT
C     LEFT END POINT.
      G = C(3,N-1) + C(3,N)
      if(ABS(C(3,N-1)).LE.EPSLON.OR.ABS(G).LE.EPSLON.OR.
     *       ABS(C(4,N-1)).LE.EPSLON)then
        BAD=.TRUE.
        return
      endif
      C(2,N) = ((C(3,N)+2.*G)*C(4,N)*C(3,N-1)
     *            + C(3,N)**2*(C(1,N-1)-C(1,N-2))/C(3,N-1))/G
      G = -G/C(4,N-1)
      C(4,N) = C(3,N-1)
                                        go to 29
C     EITHER (N=3 AND NOT-A-KNOT ALSO AT LEFT) OR (N=2 AND NOT NOT-A-
C     KNOT AT LEFT END POINT).
   22 C(2,N) = 2.*C(4,N)
      C(4,N) = 1.
                                        go to 28
C     SECOND DERIVATIVE PRESCRIBED AT RIGHT ENDPOINT.
 24   if(ABS(C(2,N)).LE.EPSLON)then
        BAD=.TRUE.
        return
      endif
      C(2,N) = 3.*C(4,N) + C(3,N)/2.*C(2,N)
      C(4,N) = 2.
                                        go to 28
   25 if(IBCEND-1)                     26,30,24
   26 if(IBCBEG .GT. 0)                go to 22
C     NOT-A-KNOT AT RIGHT ENDPOINT AND AT LEFT ENDPOINT AND N = 2.
      C(2,N) = C(4,N)
                                        go to 30
 28   if(ABS(C(4,N-1)).LE.EPSLON)then
        BAD=.TRUE.
        return
      endif
      G = -1./C(4,N-1)
COMPLETE FORWARD PASS OF GAUSS ELIMINATION.
   29 C(4,N) = G*C(3,N-1) + C(4,N)
      if(ABS(C(4,N)).LE.EPSLON)then
        BAD=.TRUE.
        return
      endif
      C(2,N) = (G*C(2,N-1) + C(2,N))/C(4,N)
CARRY OUT BACK SUBSTITUTION
   30 J = L
         if(ABS(C(4,J)).LE.EPSLON)then
           BAD=.TRUE.
           return
         endif
   40    C(2,J) = (C(2,J) - C(3,J)*C(2,J+1))/C(4,J)
         J = J - 1
         if(J .GT. 0)                  go to 40
C****** GENERATE CUBIC COEFFICIENTS IN EACH INTERVAL, I.E., THE DERIV.S
C  AT ITS LEFT ENDPOINT, FROM VALUE AND SLOPE AT ITS ENDPOINTS.
      DO 50 I=2,N
         if(ABS(C(3,I)).LE.EPSLON)then
           BAD=.TRUE.
           return
         endif
         DTAU = C(3,I)
         DIVDF1 = (C(1,I) - C(1,I-1))/DTAU
         DIVDF3 = C(2,I-1) + C(2,I) - 2.*DIVDF1
         C(3,I-1) = 2.*(DIVDF1 - C(2,I-1) - DIVDF3)/DTAU
   50    C(4,I-1) = (DIVDF3/DTAU)*(6./DTAU)
                                        return
      end
      subroutine ZDISPT(X1,Y1,X2,Y2,RLEN,XP,YP)
C
C
C
C
C
C      CALCULATE A POINT AT A DISTANCE RLEN FROM
C               POINT (X2,Y2) OF A LINE CONNECTING (X1,Y1)
C               AND (X2,Y2)
C
C      INPUT:   X1,Y1 = POINT 1
C               X2,Y2 = POINT 2
C               RLEN  = DISTANCE FROM POINT2
C                       IF POSITIVE, DISTANCE MEASURED TOWARDS
C                       POINT 1, OTHERWISE, AWAY FROM POINT 1
C
C      OUTPUT:  XP,YP = LOCATED POINT
C
C
      SAVE EPSLON
      data EPSLON /0.0001/
      DX=X2-X1
      DY=Y2-Y1
C
C                        FOR VERTICAL LINES
C
      if(ABS(DX).LT.EPSLON)then
        XP=X1
        YP=Y2-RLEN*DY/ABS(DY)
C
C                        FOR NON-VERTICAL LINES
C
      else
        TANVAL=DY/DX
        THETA=ATAN(TANVAL)
        if(DX.LT.0.0)then
          COST=-COS(THETA)
          SINT=-SIN(THETA)
        else
          COST=COS(THETA)
          SINT=SIN(THETA)
        endif
        YP=Y2-SINT*RLEN
        XP=X2-COST*RLEN
      endif
      return
      end
      subroutine ZDRAW(AX1,AY1,AX2,AY2,RX,RY,NUMR)
C
C
C
C
C     CHECK IF LINE GOES THRU ONE OR MORE BLANK AREA. IF
C              PART(S) OF LINE IS BLANKED, return UNCLIPPED PORTIONS
C
C     INPUT:   AX1,AY1 = COORDINATES OF POINT 1
C              AX2,AY2 = COORDINATES OF POINT 2
C
C     OUTPUT:  RX,RY = ARRAY FOR COORDINATES OF CLIPPED LINES
C              NUMR  = NUMBER OF COORDINATES IN ARRAY RX AND RY
C
      parameter (KZMXBS=1000)
      parameter (KZMXBL=200)
      common /CBLANK/ ZZBLNK(KZMXBL,4),ZZBLKS(KZMXBS,4),KZBLCN,KZBSCN
      DIMENSION RX(300,2),RY(300,2)
      LOGICAL VISIBO(300)
C
C                        INIT VISIBO TO FALSE
C
      DO 100 K=1,300
        VISIBO(K)=.FALSE.
 100  continue
C
C                        LOAD INPUT LINE INTO RX AND RY
C
      RX(1,1)=AX1
      RX(1,2)=AX2
      RY(1,1)=AY1
      RY(1,2)=AY2
      NUMR=1
      VISIBO(1)=.TRUE.
C
C                        GO THRU EACH AND EVERY BLANK SYMBOL
C                        TO CHECK IF LINE IS BLOCKED
C
      if(KZBSCN.GT.0)then
        DO 800 I=1,KZBSCN
          if(ZZBLKS(I,1).GT.-1000.0.AND.NUMR.GT.0)then
            call ZDRAW3(RX,RY,VISIBO,NUMR,ZZBLKS(I,1),
     *               ZZBLKS(I,2),ZZBLKS(I,3),ZZBLKS(I,4))
          endif
 800    continue
      endif
C
C                        GO THRU EACH AND EVERY BLANK AREA
C                        TO CHECK IF LINE IS BLOCKED
C
      DO 500 I=1,KZBLCN
        if(ZZBLNK(I,1).GT.-1000.0.AND.NUMR.GT.0)then
            call ZDRAW3(RX,RY,VISIBO,NUMR,ZZBLNK(I,1),
     *               ZZBLNK(I,2),ZZBLNK(I,3),ZZBLNK(I,4))
        endif
500   continue
      return
      end
      subroutine ZDRAW1(X1,Y1,X2,Y2,RX,RY,VISIBO,IPTR)
C
C
C
C
C
C      COPY TWO ENDPOINTS TO ARRAYS, RX, RY
C
C      INPUT:   X1,Y1 = COORD FOR POINT 1
C               X2,Y2 = COORD FOR POINT 2
C               IPTR  = CURRENT POINTER OF RX AND RY
C
C      OUTPUT:  RX,RY  = ARRAYS OF COORD
C               VISIBO = LOGICAL ARRAY FOR VISIBILITY OF CORRESPONDING
C                        POINT IN RX AND RY
C
      DIMENSION RX(300,2),RY(300,2)
      LOGICAL VISIBO(*)
      IPTR=IPTR+1
      RX(IPTR,1)=X1
      RX(IPTR,2)=X2
      RY(IPTR,1)=Y1
      RY(IPTR,2)=Y2
      VISIBO(IPTR)=.TRUE.
      return
      end
      subroutine ZDRAW2(TYPE,X1,Y1,X2,Y2,BOUND,RMAX,RMIN,XF,YF,IPTR)
C
C
C
C
C     FIND INTERSECTION POINT OF A LINE AND A VERTICAL
C              LINE X=BOUND OR A HORIZONTAL LINE Y=BOUND
C
C     INPUT:   TYPE      = INDICATOR OF SLOPE FOR LINE 1
C                          IF = 'X' THEN FIND INTERSECTION WITH
C                          A VERTICAL LINE, else FIND INTERSECTION
C                          WITH A HORIZONTAL LINE.
C              X1,Y1     = POINT 1 OF LINE 2
C              X2,Y2     = POINT 2 OF LINE 2
C              BOUND     = LOCATION OF LINE 1
C              RMAX,RMIN = ENDPOINTS OF LINE 1
C
C     OUTPUT:  XF,YF = INTERSECTION COORD
C              IPTR  = COUNTER TO INCREMENT IF XF AND YF LOCATED
C
      DIMENSION XF(*),YF(*)
      character TYPE
C
      SAVE EPSLON
      data EPSLON/0.00001/
      IP=IPTR
      SLOPE=(Y2-Y1)/(X2-X1)
C
C                        FIND INTERSECTION WITH A VERTICAL LINE
C                        X=BOUND
C
      if(TYPE.EQ.'X')then
        if(Y1.GT.Y2)THEN
          YMAX=Y1+EPSLON
          YMIN=Y2-EPSLON
        else
          YMAX=Y2+EPSLON
          YMIN=Y1-EPSLON
        endif
        YT=SLOPE*(BOUND-X1)+Y1
        if(YT.LE.YMAX.AND.YT.GE.YMIN.AND.
     *        YT.LE.RMAX.AND.YT.GE.RMIN)then
          IPTR=IPTR+1
          YF(IPTR)=YT
          XF(IPTR)=BOUND
        endif
C
C                        FIND INTERSECTION WITH A HORIZONTAL LINE
C                        Y=BOUND
C
      else
        if(X1.GT.X2)then
          XMAX=X1+EPSLON
          XMIN=X2-EPSLON
        else
          XMAX=X2+EPSLON
          XMIN=X1-EPSLON
        endif
        XT=(BOUND-Y1)/SLOPE+X1
        if(XT.LE.XMAX.AND.XT.GE.XMIN.AND.
     *        XT.LE.RMAX.AND.XT.GE.RMIN)then
          IPTR=IPTR+1
          XF(IPTR)=XT
          YF(IPTR)=BOUND
        endif
      endif
      if(IPTR.NE.IP)then
        DO 100 I=1,IP
          if(ABS(XF(I)-XF(IPTR)).LT.EPSLON.AND.
     *          ABS(YF(I)-YF(IPTR)).LT.EPSLON)then
              IPTR=IPTR-1
            go to 99
          endif
 100    continue
      endif
 99   continue
      return
      end
      subroutine ZDRAW3(RX,RY,VISIBO,NUMR,RXMAX,RXMIN,RYMAX,RYMIN)
C
C
C
C
C     CHECK IF LINE GOES THROGH A BLANK AREA. IF
C              PART(S) OF LINE IS BLANKED, return UNCLIPPED
C              PORTIONS
C
C     INPUT:   RX,RY  = ARRAYS OF COORDINATE PAIRS
C              VISIBO = LOGICAL ARRAY INDICATING VISIBILITY OF
C                       CORRESPONDING POINTS
C              NUMR   = NUMBER OF POINTS IN RX AND RY
C              RXMAX,RXMIN,RYMAX,RYMIN = BOUNDS OF BLANK AREA
C
C     OUTPUT:  RX,RY = ARRAY FOR COORDINATES OF CLIPPED LINES
C              NUMR  = NUMBER OF COORDINATES IN ARRAY RX AND RY
C
      DIMENSION RX(300,2),RY(300,2),XF(10),YF(10)
      LOGICAL VISIBO(300)
C
      SAVE EPSLON
      data EPSLON /0.0001/
C
C                        IF BLANK AREA EXISTS, RETRIEVE BOUNDARIES
C
      IPTR=NUMR
      DO 100 J=1,NUMR
        X1=RX(J,1)
        X2=RX(J,2)
        Y1=RY(J,1)
        Y2=RY(J,2)
        DX=X2-X1
        DY=Y2-Y1
C
C                        FOR VERTICAL LINES
C
        if(ABS(DX).LT.EPSLON)then
          if(Y1.GT.Y2) CALL ZSWAP(Y1,Y2)
          if(X1.LT.RXMAX.AND.X1.GT.RXMIN)then
            if(Y1.LT.RYMIN)then
              if(Y2.GT.RYMIN)then
                call ZDRAW1(X1,Y1,X1,RYMIN,RX,RY,VISIBO,IPTR)
                VISIBO(J)=.FALSE.
                if(Y2.GT.RYMAX)then
                  call ZDRAW1(X1,RYMAX,X1,Y2,RX,RY,VISIBO,IPTR)
                endif
              endif
            elseif(Y1.LT.RYMAX)then
              VISIBO(J)=.FALSE.
              if(Y2.GT.RYMAX)then
                call ZDRAW1(X1,RYMAX,X1,Y2,RX,RY,VISIBO,IPTR)
              endif
            endif
          endif
C
C                        FOR HORIZONTAL LINES
C
        elseif(ABS(DY).LT.EPSLON)then
          if(Y1.LT.RYMAX.AND.Y1.GT.RYMIN)then
            if(X1.GT.X2) CALL ZSWAP(X1,X2)
            if(X1.LT.RXMIN)then
              if(X2.GT.RXMIN)then
                call ZDRAW1(X1,Y1,RXMIN,Y1,RX,RY,VISIBO,IPTR)
                VISIBO(J)=.FALSE.
                if(X2.GT.RXMAX)then
                  call ZDRAW1(RXMAX,Y1,X2,Y1,RX,RY,VISIBO,IPTR)
                endif
              endif
            elseif(X1.LT.RXMAX)then
              VISIBO(J)=.FALSE.
              if(X2.GT.RXMAX)then
                call ZDRAW1(RXMAX,Y1,X2,Y1,RX,RY,VISIBO,IPTR)
              endif
            endif
          endif
C
C                        FOR ALL OTHER LINES
C
        else
          IC=0
          call ZDRAW2('X',X1,Y1,X2,Y2,RXMIN,RYMAX,RYMIN,XF,YF,IC)
          call ZDRAW2('X',X1,Y1,X2,Y2,RXMAX,RYMAX,RYMIN,XF,YF,IC)
          call ZDRAW2('Y',X1,Y1,X2,Y2,RYMIN,RXMAX,RXMIN,XF,YF,IC)
          call ZDRAW2('Y',X1,Y1,X2,Y2,RYMAX,RXMAX,RXMIN,XF,YF,IC)
          if(IC.EQ.0)then
            XMID=(X1+X2)/2.0
            YMID=(Y1+Y2)/2.0
            if(XMID.GT.RXMIN.AND.XMID.LT.RXMAX.AND.
     *          YMID.GT.RYMIN.AND.YMID.LT.RYMAX)then
              VISIBO(J)=.FALSE.
            endif
          elseif(IC.EQ.1)then
            VISIBO(J)=.FALSE.
            if(ABS(X1-RXMIN).LT.EPSLON.OR.
     *          ABS(X1-RXMAX).LT.EPSLON.OR.
     *          ABS(Y1-RYMIN).LT.EPSLON.OR.
     *          ABS(Y1-RYMAX).LT.EPSLON)then
              if(X2.GT.RXMIN.AND.X2.LT.RXMAX.AND.
     *            Y2.GT.RYMIN.AND.Y2.LT.RYMAX)then
                call ZDRAW1(XF(1),YF(1),X1,Y1,RX,RY,VISIBO,IPTR)
              else
                call ZDRAW1(X2,Y2,XF(1),YF(1),RX,RY,VISIBO,IPTR)
              endif
            else
              if(X1.GT.RXMIN.AND.X1.LT.RXMAX.AND.
     *            Y1.GT.RYMIN.AND.Y1.LT.RYMAX)then
                call ZDRAW1(XF(1),YF(1),X2,Y2,RX,RY,VISIBO,IPTR)
              else
                call ZDRAW1(X1,Y1,XF(1),YF(1),RX,RY,VISIBO,IPTR)
              endif
            endif
          elseif(IC.EQ.2)then
            VISIBO(J)=.FALSE.
            if(XF(1).GT.XF(2))then
              call ZSWAP(XF(1),XF(2))
              call ZSWAP(YF(1),YF(2))
            endif
            if(DX.GT.0.0)then
              call ZDRAW1(X1,Y1,XF(1),YF(1),RX,RY,VISIBO,IPTR)
              call ZDRAW1(XF(2),YF(2),X2,Y2,RX,RY,VISIBO,IPTR)
            else
              call ZDRAW1(X1,Y1,XF(2),YF(2),RX,RY,VISIBO,IPTR)
              call ZDRAW1(XF(1),YF(1),X2,Y2,RX,RY,VISIBO,IPTR)
            endif
          endif
        endif
 100  continue
C
C                        GARBAGE CLEAN UP IN ARRAYS RX AND RY
C
      IOFF=0
      DO 300 K=1,IPTR
        if(.NOT.VISIBO(K))then
          IOFF=IOFF+1
        else
          DO 200 L=1,2
            RX(K-IOFF,L)=RX(K,L)
            RY(K-IOFF,L)=RY(K,L)
            VISIBO(K)=.FALSE.
            VISIBO(K-IOFF)=.TRUE.
 200      continue
        endif
 300  continue
      IPTR=IPTR-IOFF
      NUMR=IPTR
      return
      end
      subroutine ZDRLOG(XOR,XCYC,YOR,YCYC,XAXIS,YAXIS,IXLAB,LIXLAB,
     *                    IYLAB,LIYLAB,XPOS,YPOS,IAXES)
C
C
C
      parameter (KZNO=222)
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
      common /CCOORD/ ZZXMIN,ZZXMAX,ZZXSTP,ZZYMIN,ZZYMAX,ZZYSTP
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CUNIT/  ZZUNIT
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      common /PLTCLP/ XMIN,XMAX,YMIN,YMAX
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI
      character CZALFL*1,CZALFN*5,CZALFS*5
      LOGICAL LOGX, LOGY
C
      DIMENSION IXLAB(2), IYLAB(2), NUMBR(14)
      LOGICAL LOGXX, LOGYY, LOGT
      LOGICAL LHEAD,LFIRST
      DIMENSION QLOG(9)
C
C
      SAVE PIO180,QLOG,TMINLD
      data PIO180 /1.745329252E-2/
      data QLOG /0.3010, 0.4771, 0.6021, 0.6990, 0.7782, 0.8451,
     1   0.9031, 0.9542, 1.0/
C
C  MINIMUM DISTANCE BETWEEN SHORT TICKS (1 MM)
C
      data TMINLD /0.1/
C
C                        CALCULATE AXIS LABEL AND ORIGIN OFFSET
C
CW      call ZSCOPY('10',ITEN)
      TICKLN=0.6*ZZHITE
      call SYBYT4('N',ITEN,2,48)
      call SYBYT4('N',ITEN,1,49)
      call GSSETC(0.9*ZZHITE,0.0)
      TENLEN=ZXMESS(ITEN,2,CZALFL,CZALFN,CZALFS)*1.4
      DX=XPOS*ZZUNIT*ZZPAGR
      DY=YPOS*ZZUNIT*ZZPAGR
C
C                        PROTECT MAPPRM INFO THIS WAY
C
      YVLEN = YVINI
C
C                        SEE WHAT TYPE OF AXES ARE DESIRED
C
      LOGXX=KSYAND(IAXES,1).NE.0
      LOGYY=KSYAND(IAXES,2).NE.0
      LHEAD=KSYAND(IAXES,16).NE.0
C
C                        DO THE AXES SCALING
C
      if(LOGXX)then
        XMIN=XOR
        XMAX=XAXIS/XCYC+XMIN
        ZZXMIN=10.0**XOR
        ZZXMAX=10.0**(XAXIS/XCYC+XOR)
        XTMIN = XMIN
        XTMAX = XMAX
        XL=10.0**XMIN
        XH=10.0**XMAX
        RAXIS=XAXIS*ZZUNIT
        NUMTKS=MIN0(10,INT(RAXIS/((ILABSZ()+1.0)*CXSIZE)))
        call LAXIS(XL,XH,NUMTKS,XMN,XMX,XTICK)
        ZZXSTP=10.0**XTICK
        UX0 = XMIN
        UDX = XMAX - XMIN
      endif
      if(LOGYY)then
        YMIN=YOR
        YMAX=YAXIS/YCYC+YMIN
        ZZYMIN=10.0**YOR
        ZZYMAX=10.0**(YAXIS/YCYC+YOR)
        YTMIN = YMIN
        YTMAX = YMAX
        YL=10.0**YMIN
        YH=10.0**YMAX
        RAXIS=YAXIS*ZZUNIT
C
C  JVB
C
C  Replaced "CXSIZE" by "GSLENS('0')
C
        NUMTKS=MIN0(10,INT(RAXIS/((ILABSZ()+1.0)*GSLENS('0'))))
        call LAXIS(YL,YH,NUMTKS,YMN,YMX,YTICK)
        ZZYSTP=10.0**YTICK
        UY0 = YMIN
        UDY = YMAX - YMIN
      endif
C
C                        ********** DRAW X AXIS **********
C
      if(LOGXX.AND.KZXALN.NE.0)then
        LOGT = .FALSE.
        if(XTICK .EQ. 1.0)then
          call SCALE(XMIN,YMIN,TEMP,VY)
          call SCALE(XMIN+1.0-QLOG(8),YMIN,VX,VY)
          if((VX-TEMP) .GE. TMINLD) LOGT = .TRUE.
        endif
C
C                        DRAW X AXIS LINE
C
        Y = YMIN
        call SCALE(XMIN,Y,VX,VY)
        VXOLD=VX
        LFIRST=.TRUE.
C
C                        DRAW AND LABEL X AXIS TICKS
C
        ANGX=COS(ZZXLBA*PIO180)*TENLEN
        ANGY=SIN(ZZXLBA*PIO180)*TENLEN
        ANGMAX=0.0
        EPSLON=XTICK*0.1
        X = XTMIN
C
 400    continue
        if(KZXTCK.NE.0)then
          call DSMOVE(VXOLD,VY)
          if(LFIRST)then
            LFIRST=.FALSE.
            call DSDRAW(VX+DX,VY-TICKLN*1.2+DY)
          else
            call SCALE(X,Y,VX,VY)
            call DSDRAW(VX+DX,VY+DY)
            call DSDRAW(VX+DX,VY-TICKLN*1.2+DY)
            VXOLD=VX
          endif
        else
          call SCALE(X,Y,VX,VY)
        endif
        if(KZXNON.EQ.KZNO)then
          if(ZZXLBA.LT.3.0)then
            ANGX1=ANGX*0.5
            ANGY1=2.4*ZZHITE
            ANGMAX=ANGY1
          elseif(ZZXLBA.LT.270.0)then
            ANGX1=ANGX-0.45*ZZHITE
            ANGY1=ANGY+1.3*ZZHITE+1.1*ZZHITE*COS(ZZYLBA*PIO180)
            if(ANGMAX.LT.ANGY1) ANGMAX=ANGY1
          else
            ANGX1=0.45*ZZHITE
            ANGY1=1.3*ZZHITE+1.1*ZZHITE*COS(ZZYLBA*PIO180)
            TEMP=ANGY1-ANGY
            if(ANGMAX.LT.TEMP) ANGMAX=TEMP
          endif
          call GSSETC(0.9*ZZHITE,ZZXLBA)
          NUMBR(1)=-1
          call ZNMSTR(X,NUMBR)
          call DSMOVE(VX-ANGX1+DX,VY-ANGY1+DY)
          call ZTEXT(ITEN,2,CZALFL,CZALFN,CZALFS)
          call GSSETC(0.7*ZZHITE,ZZXLBA)
          call DSMOVE(VX-ANGX1+ANGX/1.4-0.5*ZZHITE*
     *                  SIN(ZZXLBA*PIO180)+DX,
     *                  VY-ANGY1+ANGY/1.4+0.5*ZZHITE*
     *                  COS(ZZXLBA*PIO180)+DY)
          LN=LENG(NUMBR)
          call ZTEXT(NUMBR,LN,CZALFL,CZALFN,CZALFS)
        else
          ANGMAX=2.4*ZZHITE
        endif
C
C                        DO EXTRA TICKING IF EXTRA TICKS
C                        WILL BE FAR ENOUGH APART
C
        if(X+EPSLON .LT. XTMAX)then
          if(LOGT)then
            DO 450 J = 1, 9
              if(KZXTCK.GT.1)then
                RINCR=1.0/KZXTCK
                DO 200 K=1,KZXTCK-1
                  call SCALE(X+ALOG10(J+RINCR*K),Y,VX,VY)
                  call DSMOVE(VX+DX,VY+DY)
                  call DSDRAW(VX+DX,VY-TICKLN*0.6+DY)
 200            continue
              endif
              if(KZXTCK.NE.0.AND.J.NE.9)then
                call SCALE(X+QLOG(J),Y,VX,VY)
                call DSMOVE(VX+DX,VY+DY)
                call DSDRAW(VX+DX,VY-TICKLN*0.9+DY)
              endif
450         continue
          else
            if(XTICK.GT.1.0)then
              DO 460 J=1,INT(XTICK)
                if(J.NE.INT(XTICK))then
                  call SCALE(X+J,YMIN,VX,VY)
                  if(KZXTCK.NE.0)then
                    call DSMOVE(VX+DX,VY+DY)
                    call DSDRAW(VX+DX,VY+DY-TICKLN*1.2)
                  endif
                  if(KZXNON.EQ.KZNO)then
                    call GSSETC(0.9*ZZHITE,ZZXLBA)
                    NUMBR(1)=-1
                    call ZNMSTR(X+REAL(J),NUMBR)
                    call DSMOVE(VX-ANGX1+DX,VY-ANGY1+DY)
                    call ZTEXT(ITEN,2,CZALFL,CZALFN,CZALFS)
                    call GSSETC(0.7*ZZHITE,ZZXLBA)
                    call DSMOVE(VX-ANGX1+ANGX/1.4-0.5*ZZHITE*
     *                          SIN(ZZXLBA*PIO180)+DX,
     *                          VY-ANGY1+ANGY/1.4+0.5*ZZHITE*
     *                          COS(ZZXLBA*PIO180)+DY)
                    LN=LENG(NUMBR)
                    call ZTEXT(NUMBR,LN,CZALFL,CZALFN,CZALFS)
                  endif
                endif
                if(KZXTCK.GT.1)then
                  RINCR=10.0/KZXTCK
                  DO 470 K=1,KZXTCK-1
                    call SCALE(X+J-1.0+ALOG10(RINCR*K),Y,VX,VY)
                    call DSMOVE(VX+DX,VY+DY)
                    call DSDRAW(VX+DX,VY-TICKLN*0.6+DY)
 470              continue
                endif
 460          continue
            endif
          endif
        endif
        X = X + XTICK
        if(X-EPSLON.LE.XMAX) go to 400
        call GSSETC(ZZHITE,0.0)
C
C                        NOW PLACE X AXIS LABEL
C
        call SCALE((XMIN+XMAX)/2.0,YMIN,VX,VY)
        RLEN=ZXMESS(KZXLAB,KZXALN,CZALFL,CZALFN,CZALFS)
        call DSMOVE(VX-RLEN/2.0+DX,VY-ANGMAX-1.7*ZZHITE+DY)
        call ZTEXT(KZXLAB,KZXALN,CZALFL,CZALFN,CZALFS)
        if(KZXNFL.EQ.KZNO) KZXALN=0
      endif
C
C      ********** DRAW Y AXES **********
C
      if(LOGYY.AND.KZYALN.NE.0)then
        call GSSETC(0.7*ZZHITE,90.0)
        LOGT = .FALSE.
        if(YTICK .EQ. 1.0)then
          call SCALE(XMIN,YMIN,VX,TEMP)
          call SCALE(XMIN,YMIN+1.0-QLOG(8),VX,VY)
          if((VY-TEMP) .GE. TMINLD) LOGT = .TRUE.
        endif
C
C                        DRAW Y AXIS LINE
C
        X = XMIN
        call SCALE(X,YMIN,VX,VY)
        VYOLD=VY
        LFIRST=.TRUE.
C
C                        DRAW AND LABEL Y AXIS TICKS
C
        ANGX=COS(ZZYLBA*PIO180)*TENLEN
        ANGY=SIN(ZZYLBA*PIO180)*TENLEN
        ANGMAX=0.0
        EPSLON=YTICK*0.1
        Y = YTMIN
C
 110    continue
        if(KZYTCK.NE.0)then
          call DSMOVE(VX+DX,VYOLD+DY)
          if(LFIRST)then
            LFIRST=.FALSE.
          else
            call SCALE(X,Y,VX,VY)
            call DSDRAW(VX+DX,VY+DY)
          endif
          call DSDRAW(VX-TICKLN*1.2+DX,VY+DY)
          VYOLD=VY
        else
          call SCALE(X,Y,VX,VY)
        endif
C
C                        PLACE THE APPROPIATE LABEL
C
        if(KZYNON.EQ.KZNO)then
          if(ZZYLBA.GT.87.0.AND.ZZYLBA.LT.93.0)then
            ANGY1=ANGY*0.5
            ANGX1=1.3*ZZHITE
            ANGMAX=2.5*ZZHITE
          elseif(ZZYLBA.LE.270)then
            ANGY1=ANGY+0.45*ZZHITE
            ANGX1=ANGX+1.3*ZZHITE
            TEMP=ANGX1+1.1*ZZHITE*SIN(ZZYLBA*PIO180)
            if(ANGMAX.LT.TEMP) ANGMAX=TEMP
          else
            ANGY1=ANGY+0.45*ZZHITE
            ANGX1=ANGX+1.3*ZZHITE-1.1*ZZHITE*SIN(ZZYLBA*PIO180)
            if(ANGMAX.LT.ANGX1) ANGMAX=ANGX1
          endif
          call GSSETC(0.9*ZZHITE,ZZYLBA)
          NUMBR(1)=-1
          call ZNMSTR(Y,NUMBR)
          call DSMOVE(VX-ANGX1+DX,VY-ANGY1+DY)
          call ZTEXT(ITEN,2,CZALFL,CZALFN,CZALFS)
          call GSSETC(0.7*ZZHITE,ZZYLBA)
          call DSMOVE(VX-ANGX1+ANGX/1.4-0.5*ZZHITE*
     *                    SIN(ZZYLBA*PIO180)+DX,
     *                    VY-ANGY1+ANGY/1.4+0.5*ZZHITE*
     *                    COS(ZZYLBA*PIO180)+DY)
          LN=LENG(NUMBR)
          call ZTEXT(NUMBR,LN,CZALFL,CZALFN,CZALFS)
        else
          ANGMAX=2.5*ZZHITE
        endif
C
C                        DO EXTRA TICKING IF EXTRA TICKS
C                        WILL BE FAR ENOUGH APART
C
        if(Y+EPSLON.LT. YTMAX)then
          if(LOGT)then
            DO 190 J = 1, 9
              if(KZYTCK.GT.1)then
                RINCR=1.0/KZYTCK
                DO 180 K=1,KZYTCK-1
                  call SCALE(X,Y+ALOG10(J+RINCR*K),VX,VY)
                  call DSMOVE(VX+DX,VY+DY)
                  call DSDRAW(VX-TICKLN*0.6+DX,VY+DY)
 180            continue
              endif
              if(KZYTCK.NE.0.AND.J.NE.9)then
                call SCALE(X,Y+QLOG(J),VX,VY)
                call DSMOVE(VX+DX,VY+DY)
                call DSDRAW(VX-TICKLN*0.9+DX,VY+DY)
              endif
190         continue
          else
            if(YTICK.GT.1.0)then
              DO 170 J=1,INT(YTICK)
                if(J.NE.INT(YTICK))then
                  call SCALE(XMIN,Y+J,VX,VY)
                  if(KZYTCK.NE.0)then
                    call DSMOVE(VX+DX,VY+DY)
                    call DSDRAW(VX-TICKLN*1.2+DX,VY+DY)
                  endif
                  if(KZYNON.EQ.KZNO)then
                    call GSSETC(0.9*ZZHITE,ZZYLBA)
                    NUMBR(1)=-1
                    call ZNMSTR(Y+REAL(J),NUMBR)
                    call DSMOVE(VX-ANGX1+DX,VY-ANGY1+DY)
                    call ZTEXT(ITEN,2,CZALFL,CZALFN,CZALFS)
                    call GSSETC(0.7*ZZHITE,ZZYLBA)
                    call DSMOVE(VX-ANGX1+ANGX/1.4-0.5*ZZHITE*
     *                            SIN(ZZYLBA*PIO180)+DX,
     *                            VY-ANGY1+ANGY/1.4+0.5*ZZHITE*
     *                            COS(ZZYLBA*PIO180)+DY)
                    LN=LENG(NUMBR)
                    call ZTEXT(NUMBR,LN,CZALFL,CZALFN,CZALFS)
                  endif
                endif
                if(KZYTCK.GT.1)then
                  RINCR=10.0/KZYTCK
                  DO 160 K=1,KZYTCK-1
                    call SCALE(X,Y+J-1.0+ALOG10(RINCR*K),VX,VY)
                    call DSMOVE(VX+DX,VY+DY)
                    call DSDRAW(VX-TICKLN*0.6+DX,VY+DY)
 160              continue
                endif
 170          continue
            endif
          endif
        endif
        Y = Y + YTICK
C
        if(Y-EPSLON.LE.YMAX) go to 110
C
C                        NOW PLACE Y LABEL
C
        call GSSETC(ZZHITE,90.0)
        call SCALE(XMIN,(YMIN+YMAX)/2.0,VX,VY)
        RLEN=ZXMESS(KZYLAB,KZYALN,CZALFL,CZALFN,CZALFS)
        call DSMOVE(VX-ANGMAX-0.7*ZZHITE+DX, VY-RLEN/2.0+DY)
        call ZTEXT(KZYLAB,KZYALN,CZALFL,CZALFN,CZALFS)
        call GSSETC(ZZHITE,0.0)
        if(KZYNFL.EQ.KZNO) KZYALN=0
      endif
C
C                        WRITE TITLE OF PLOT
C
      if(LHEAD.AND.KZTILN.NE.0)then
        call GSSETC(1.46*ZZHITE,0.0)
        SPACE=AMAX1(ZZHITE,0.1*YVLEN)
        call SCALE((XMIN+XMAX)/2.0,YMAX,VX,VY)
        RLEN=ZXMESS(KZTITL,KZTILN,CZALFL,CZALFN,CZALFS)
        call DSMOVE(VX-RLEN/2.0+DX,VY+SPACE+DY)
        call ZTEXT(KZTITL,KZTILN,CZALFL,CZALFN,CZALFS)
        call GSSETC(ZZHITE,0.0)
      endif
C
C                        MAKE SURE "PLTCLP" CONTAINS LIMITS
C                        PICKED BY MAPIT.   ONLY MAINTAINED
C                        FOR CALLERS INFO.
C
      if(LOGXX)then
        XMIN = 10.0**XMIN
        XMAX = 10.0**XMAX
        LOGX = .TRUE.
      endif
      if(LOGYY)then
        YMIN = 10.0**YMIN
        YMAX = 10.0**YMAX
        LOGY = .TRUE.
      endif
      return
      end
      subroutine ZFILL(AX1,AY1,AX2,AY2,RXMAX,RXMIN,RYMAX,RYMIN,LBLANK)
C
C
C
C
C     CHECK IF LINE GOES THROUGH A BLANK AREA. IF
C              PART(S) OF LINE IS BLANKED, return STATUS.
C
C     INPUT:   X1,Y1  = POINT 1
C              X2,Y2  = POINT 2
C              RXMAX,RXMIN,RYMAX,RYMIN = BOUNDS OF BLANK AREA
C
C     OUTPUT:  LBLANK = LOGICAL STATUS FLAG, 'TRUE' IF THE LINE
C                       GOES THROUGH A BLANK AREA
C
      LOGICAL LBLANK
C
      SAVE EPSLON
      data EPSLON /0.0001/
C
C                        IF BLANK AREA EXISTS, RETRIEVE BOUNDARIES
C
      LBLANK=.FALSE.
      X1=AX1
      Y1=AY1
      X2=AX2
      Y2=AY2
      DX=X2-X1
      DY=Y2-Y1
C
C                        FOR VERTICAL LINES
C
      if(ABS(DX).LT.EPSLON)then
        if(X1.LT.RXMAX.AND.X1.GT.RXMAX)then
          if((Y1.GT.RYMIN.AND.Y1.LT.RYMIN).OR.
     *     (Y2.GT.RYMIN.AND.Y2.LT.RYMAX))then
              LBLANK=.TRUE.
              return
            endif
        endif
C
C                        FOR HORIZONTAL LINES
C
      elseif(ABS(DY).LT.EPSLON)then
        if(Y1.LT.RYMAX.AND.Y1.GT.RYMIN)then
          if((X1.GT.RXMIN.AND.X1.LT.RXMAX).OR.
     *        (X2.GT.RXMIN.AND.X2.LT.RXMAX))then
              LBLANK=.TRUE.
              return
        endif
        endif
C
C                        FOR ALL OTHER LINES
C
      else
        IC=0
        call ZDRAW2('X',X1,Y1,X2,Y2,RXMIN,RYMAX,RYMIN,XF,YF,IC)
        call ZDRAW2('X',X1,Y1,X2,Y2,RXMAX,RYMAX,RYMIN,XF,YF,IC)
        call ZDRAW2('Y',X1,Y1,X2,Y2,RYMIN,RXMAX,RXMIN,XF,YF,IC)
        call ZDRAW2('Y',X1,Y1,X2,Y2,RYMAX,RXMAX,RXMIN,XF,YF,IC)
        if(IC.EQ.0)then
          XMID=(X1+X2)/2.0
          YMID=(Y1+Y2)/2.0
          if(XMID.GT.RXMIN.AND.XMID.LT.RXMAX.AND.
     *        YMID.GT.RYMIN.AND.YMID.LT.RYMAX)then
            LBLANK=.TRUE.
            return
          endif
        elseif(IC.EQ.1.OR.IC.EQ.2)then
          LBLANK=.TRUE.
          return
        endif
      endif
      return
      end
      subroutine ZFRAME(XA,XB,YA,YB,DELTA)
C
C
C
C
C
C      DRAW FRAME WITH AN OFFSET OF DELTA
C
C      INPUT:   XA,XB,YA,YB = FRAME BOUNDARIES
C               DELTA       = OFFSET OF FRAME
C
      parameter (KZSOLI=1)
      LOGICAL LINILT, LPOSND
      common /DCLTYP/ ILNTYP, DLEFT, DIST(13,15), LINILT, LPOSND
C
      X1=XA-DELTA
      X2=XB+DELTA
      Y1=YA-DELTA
      Y2=YB+DELTA
      IOLINE=ILNTYP
      ILNTYP=KZSOLI
      call DSMOVE(X1,Y1)
      call DSDRAW(X1,Y2)
      call DSDRAW(X2,Y2)
      call DSDRAW(X2,Y1)
      call DSDRAW(X1,Y1)
      ILNTYP=IOLINE
      return
      end
      subroutine ZGETFN(CFONT,CSTYLE,IFONT)
C
C
C
C     ASSIGNS A FONT ACCORDING TO CURRENT FONT
C              STYLE AND INPUT character SET
C
C     INPUT:   CFONT  = FONT TYPE
C              CSTYLE = character STYLE
C
C     OUTPUT:  IFONT = FONT ID ASSOCIATED WITH CFONT AND CSTYLE
C
      character CFONT*(*),CSTYLE*(*)
      integer LISFNT(40)
C                                -CFONT-
C                    STAND ITALI GREEK SCRIP MATH     -CSTYLE-
C                                                      DEFAULT
C
      SAVE LISFNT
C
C  CARTOG
C  SIMPLX
C  CMPLX2
C  COMPLX
C  DUPLEX
C  TRIPLX
C  GOTHIC
C
      data LISFNT /     1,   18,   13,    5,   16,
     *                 11,   18,   13,    5,   16,
     *                 15,   18,   13,    5,   16,
     *                 12,   14,    8,    6,   16,
     *                 12,   14,   17,    6,   16,
     *                  3,   14,   17,    6,   16,
     *                  2,    4,   17,    6,   16,
     *                  9,    7,    8,   10,   16/
      if(CSTYLE(1:5).EQ.'CARTO')then
        II=1
      elseif(CSTYLE(1:5).EQ.'SIMPL')then
        II=2
      elseif(CSTYLE(1:5).EQ.'CMPLX')then
        II=3
      elseif(CSTYLE(1:5).EQ.'COMPL')then
        II=4
      elseif(CSTYLE(1:5).EQ.'DUPLE')then
        II=5
      elseif(CSTYLE(1:5).EQ.'TRIPL')then
        II=6
      elseif(CSTYLE(1:5).EQ.'GOTHI')then
        II=7
      else
        II=0
      endif
      if(CFONT(1:5).EQ.'ITALI'.OR.CFONT(1:5).EQ.'L/CIT')then
        JJ=2
      elseif(CFONT(1:5).EQ.'GREEK'.OR.CFONT(1:5).EQ.'L/CGR')then
        JJ=3
      elseif(CFONT(1:5).EQ.'SCRIP'.OR.CFONT(1:5).EQ.'L/CSC')then
        JJ=4
      elseif(CFONT(1:5).EQ.'MATHE')then
        JJ=5
      else
        JJ=1
      endif
      IFONT=LISFNT(II*5+JJ)
      return
      end
      subroutine ZGSCLP(AREA)
C
C
C
C
      DIMENSION AREA(4)
C
C      THIS ROUTINE SAVES THE CURRENT ABSOLUTE CLIPPING WINDOW AND
C      SETS A NEW ABSOLUTE CLIPPING WINDOW GIVEN VIRTUAL COORDINATES.
C      IT MAKES SURE THAT THE CLIPPING WINDOW NEVER LIES OUTSIDE THE
C      PHYSICAL DEVICE.
C
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
C
      AREA(1) = XCM0
      AREA(2) = XCM1
      AREA(3) = YCM0
      AREA(4) = YCM1
      TVX0=XVSTRT-ZZGRCE
      TVX1=XVSTRT+XVLEN+ZZGRCE
      TVY0=YVSTRT-ZZGRCE
      TVY1=YVSTRT+YVLEN+ZZGRCE
C
      call GSRST(TVX0,TVY0,AX0,AY0)
      call GSRST(TVX1,TVY1,AX1,AY1)
      XCM0 = AMAX1(AMIN1(AX0,AX1),0.0)
      YCM0 = AMAX1(AMIN1(AY0,AY1),0.0)
      XCM1 = AMIN1(XCLIPD,AMAX1(AX0,AX1))
      YCM1 = AMIN1(YCLIPD,AMAX1(AY0,AY1))
      return
      end
      subroutine ZIN2CH(LSTR,CSTR)
C
C
C
C
C
C      CONVERTS AN integer ARRAY TO A character
C               ARRAY
C
C      INPUT:   LSTR = integer ARRAY
C
C      OUTPUT:  CSTR = character ARRAY
C
      integer LSTR(*)
      character*1 CSTR(*)
      LENGTH=LENG(LSTR)
      DO 100 I=1,LENGTH+1
        call SYBYT4('X',LSTR,I,KHAR)
        CSTR(I)=CHAR(KHAR)
 100  continue
      return
      end
      subroutine ZINIT
C
C
C
C     TO INITIALIZE ALL SYSTEM VARIABLES
C              (LEVEL 0, SET LEVEL TO 1)
C
      parameter (KZYES=111)
      parameter (KZNO=222)
      parameter (ZZIN=2.54)
      parameter (KZREAL=2)
      parameter (KZLIN=2)
      parameter (KZSOLI=1)
      parameter (KZMXBS=1000)
      parameter (KZMXBL=200)
      parameter (KZMAXC=255)
      character CZCOLR*8,CZALFL*1,CZALFN*5,CZALFS*5,CZLGAC*1,
     *          CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5,
     *          CZHDAC*1,CZHDAF*5,CZHDAS*5
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
      common /CBLANK/ ZZBLNK(KZMXBL,4),ZZBLKS(KZMXBS,4),KZBLCN,KZBSCN
      common /CBORCH/ KZBRDR,KZCHEK
      common /COLORN/ KZSHD,ZZHSPC,ZZCOLR(KZMAXC,3),KZNCLR,KZCCOL
      common /COLORC/ CZCOLR
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CHEADN/ KZHDNL,KZHDMX,KZHDTX(72),ZZHDSZ(4)
      common /CHEADC/ CZHDAC(4,6),CZHDAF(4,6),CZHDAS(4)
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /CMESS/  ZZABUX,ZZABUY,KZLBEG,KZMOV
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CPHYSR/ ZZXOR,ZZYOR,KZOR,UUXOR,UUYOR
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CSYMBO/ KZSYM,KZNSYM,ZZSMSZ,UUSMSZ
      common /CUNIT/  ZZUNIT
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
C
C                        SET LEVEL
C
      KZLEVL=1
      KZBEGN=KZNO
C
C                        COLOR TO WHITE AND NO HARDWARE SHADE
C
      CZCOLR='WHITE'
      KZSHD=KZNO
      KZNCLR=2
      ZZCOLR(1,1)=0.0
      ZZCOLR(1,2)=0.0
      ZZCOLR(1,3)=100.0
      call GSCOLR(1,IERR)
C
C                        LINEAR REAL AXIS,
C                        AND NO AXIS LABEL SUPPRESSED
C
      KZXTYP=KZREAL
      KZYTYP=KZREAL
      KZXNON=KZNO
      KZYNON=KZNO
      KZXTCK=1
      KZYTCK=1
      ZZXLBA=0.0
      ZZYLBA=90.0
C
C                        LINE STYLE SET UP, NO INTERPOLATION
C                        AND LINE THICKNESS
C
      KZLNCN=4
      KZLSTY=KZSOLI
      call DSLTYP(KZLSTY)
      KZLTYP=KZLIN
      ZZLTHK=0.01*ZZIN
      KZLTHK=KZNO
      ZZHSPC=REAL(NFLINE)/YRES*.75
C
C                        "UNIT" SET UP
C
      ZZUNIT=ZZIN
C
C                        "PAGE" SET UP
C
      KZAUTO=KZNO
      if(KZPAGE.NE.KZYES)then
        UUPAGX=8.5*ZZIN
        UUPAGY=11.0*ZZIN
        ZZPAGX=UUPAGX
        ZZPAGY=UUPAGY
        ZZPAGR=1.0
      endif
C
C                         SYMBOL parameter SET UP
C
      KZSYM=0
      KZNSYM=18
      UUSMSZ=0.08*ZZIN
      ZZSMSZ=UUSMSZ
C
C                        "ANGLE" AND "CHRSIZ" AND
C                        character FONT TYPE SET UP
C
      ZZANGL=0.0
      UUHITE=0.14*ZZIN
      ZZHITE=UUHITE
      call GSSETC(UUHITE,0.0)
C
C
C                        "TXTBLK" SET UP
C
      call ZSCOPY('LEGEND',KZLGTI)
      KZLGTL=6
      KZLGLN=KZNO
      KZLGCN=0
      KZLGER=KZNO
      UULGTZ=UUHITE
      ZZLGTZ=-1.0
      ZZLGTR=-1.0
C
C                        STRING TERMINATOR SET UP
C
      KZSTRM='$'
      KZTMLN=1
      KZHDNL=0
      KZHDMX=0
C
C                        ABUTMENT OF character STRINGS
C
      ZZABUX=0.0
      ZZABUY=0.0
C
C                        "MARGIN" SET UP
C
      UUGRCE=0.5*ZZIN
      ZZGRCE=UUGRCE
C                        "FRMWID", "NOCHEK" AND "NOBORD" SET UP
C
      UUFRME=0.03*ZZIN
      ZZFRME=UUFRME
      KZCHEK=KZYES
      KZBRDR=KZYES
C
C                        "XLABEL", "YLABEL" AND "HEADIN" SET UP
C
      KZXNFL=KZNO
      KZXALN=0
      KZYALN=0
      KZTILN=0
C
C                        "ORIGIN" SET UP
C
      KZOR=KZNO
C
C                        "SETOUT" SET UP
C
C     if(KZNPLT.EQ.0)then
        KZERR=6
        KZSUM=6
C     endif
      KZCOPY=KZNO
C
C                        "BLANKi" AND STORY HEIGHT SPACE SET UP
C
      KZBSCN=-1
      KZBLCN=4
      ZZSRAT=1.5
      DO 200 I=1,KZMXBL
        DO 100 J=1,4
          ZZBLNK(I,J)=-1100.0
 100        continue
 200      continue
      DO 1200 I=1,KZMXBS
        DO 1100 J=1,4
          ZZBLKS(I,J)=-1100.0
 1100        continue
 1200      continue
      return
      end
      subroutine ZINITS
C
C
C
C
C      TO INITIALIZE ALL SYSTEM VARIABLES
C               (LEVEL 0, SET LEVEL TO 1)
C
      parameter (KZNO=222)
      parameter (ZZIN=2.54)
c
      save cborch
      save cheadc
      save cheadn
      save clabel
      save clevel
      save clgndc
      save clgndn
      save cstrng
      save csymbo
c
      common /CLEVEL/ KZLEVL,KZBEGN
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CSYMBO/ KZSYM,KZNSYM,ZZSMSZ,UUSMSZ
      common /CHEADN/ KZHDNL,KZHDMX,KZHDTX(72),ZZHDSZ(4)
      common /CHEADC/ CZHDAC(4,6),CZHDAF(4,6),CZHDAS(4)
      common /CBORCH/ KZBRDR,KZCHEK
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      character CZHDAC*1,CZHDAF*5,CZHDAS*5
      character CZLGAC*1,CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5
C
      DO 100 I=1,50
        KZLGSM(I)=-1
        KZLGLT(I)=0
        DO 200 K=1,6
          CZLGAC(I,K)=' '
          CZLGAF(I,K)='     '
 200    continue
        CZLGAS(I)='     '
 100  continue
      CZLGTC(1)=' '
      CZLGTF(1)='STAND'
      CZLGTS='DEFAU'
      DO 300 I=2,6
        CZLGTC(I)=' '
        CZLGTF(I)='     '
 300  continue
      DO 400 I=1,4
        DO 500 K=1,6
          CZHDAC(I,K)=' '
          CZHDAF(I,K)='     '
 500    continue
        CZHDAS(I)='     '
 400  continue
C
C                        SET LEVEL
C
      KZLEVL=1
C
C                         SYMBOL parameter SET UP
C
      KZSYM=0
      UUSMSZ=0.08*ZZIN
C
C                        "TXTBLK" SET UP
C
      call ZSCOPY('LEGEND',KZLGTI)
      KZLGTL=6
      KZLGLN=KZNO
      KZLGCN=0
      KZLGER=KZNO
      UULGTZ=UUHITE
      ZZLGTR=1.5
C
C                        STRING TERMINATOR SET UP
C
      KZHDNL=0
      KZHDMX=0
C
C                        "MARGIN" SET UP
C
      KZBRDR=KZNO
C
C                        "XLABEL", "YLABEL" AND "HEADIN" SET UP
C
      KZTILN=0
      return
      end
      subroutine ZLASAL
C
C
C
C
C          INIT DEVICE FOR QMS LANDSCAPE, I.E. WIDE
C
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      common /GCDSEL/ IDEV
C
      SAVE KQMS,KLN03,KPOST
      data KQMS,KLN03,KPOST /1200,3,910/
      KDEV=INT(DEVID)
      if(KDEV.EQ.KQMS)then
        IDEV=3
      elseif(KDEV.EQ.KLN03)then
        IDEV=10
      elseif(KDEV.EQ.KPOST)then
        IDEV=14
      endif
      if(XLENCM.LT.YLENCM)then
        call ZSWAP(XLENCM,YLENCM)
        call ZSWAP(XRES,YRES)
        call ZSWAP(XCLIPD,YCLIPD)
      endif
      return
      end
      subroutine ZLASAP
C
C
C
C
C     INIT DEVICE FOR QMS PORTRAIT, I.E. TALL
C
      common /GCDCHR/ DEVID, XLENCM, YLENCM, XRES, YRES,
     1                NDCLRS, IDVBTS, NFLINE, XCLIPD, YCLIPD
      common /GCDSEL/ IDEV
C
      SAVE KQMS,KLN03,KPOST
      data KQMS,KLN03,KPOST /1200,3,910/
      KDEV=INT(DEVID)
      if(KDEV.EQ.KQMS)then
        IDEV=4
      elseif(KDEV.EQ.KLN03)then
        IDEV=11
      elseif(KDEV.EQ.KPOST)then
        IDEV=15
      endif
      if(XLENCM.GT.YLENCM)then
        call ZSWAP(XLENCM,YLENCM)
        call ZSWAP(XRES,YRES)
        call ZSWAP(XCLIPD,YCLIPD)
      endif
      return
      end
      subroutine ZLNLAB(CFLAG,RNUM,IEXP,ISTRNG,LRMTEX)
C
C
C
C
      LOGICAL LRMTEX
      integer ISTRNG(*),ITEMP(20)
      character*1 CFLAG,STRNG(8),BMINUS, BZERO(6),CTEMP(80)
C
C
      SAVE BMINUS
      data BMINUS /'-'/
      BZERO(1)='0'
      BZERO(2)='.'
      BZERO(3)='0'
      BZERO(4)='0'
      BZERO(5)='0'
      BZERO(6)=CHAR(0)
      LRMTEX = .TRUE.
      NUM=INT(RNUM)
C
C      WORK WITH ABSOLUTE VALUE AS IT IS EASIER TO PUT SIGN IN NOW
C
      if(RNUM .GE. 0.0)then
        VAL = RNUM
        NVAL=NUM
        ISTART = 1
      else
        VAL = -RNUM
        NVAL= -NUM
        ISTART = 2
        STRNG(1) = BMINUS
      endif
C
      if(CFLAG.EQ.'R')then
        if(IEXP .GE. -3 .AND. IEXP .LE. 3)then
          LRMTEX = .FALSE.
          VAL = VAL*10.0**IEXP
        endif
        ITEMP(1)=1
        call ZNMSTR(VAL,ITEMP)
        call ZIN2CH(ITEMP,CTEMP)
        NCHR=NCHRAY(CTEMP)
        DO 400 K=1,NCHR+1
          STRNG(ISTART+K-1)=CTEMP(K)
 400    continue
      else
        if(IEXP .GE. -4 .AND. IEXP .LE. 4) LRMTEX = .FALSE.
        if(IEXP .GT. 0 .AND. (.NOT. LRMTEX))then
          VAL = VAL*10.0**IEXP
          NVAL=INT(VAL+0.001)
        endif
        TEMP=VAL-REAL(NVAL)
        if(ABS(TEMP).GT.0.01)then
          ITEMP(1)=1
          call ZNMSTR(VAL,ITEMP)
        else
          ITEMP(1)=-1
          call ZNMSTR(VAL,ITEMP)
        endif
        call ZIN2CH(ITEMP,CTEMP)
        NCHR=NCHRAY(CTEMP)
        DO 700 K=1,NCHR+1
          STRNG(ISTART+K-1)=CTEMP(K)
 700    continue
        if((NVAL .EQ. 0) .OR. LRMTEX .OR. (IEXP .GE. 0)) go to 800
C
C      NUMBER IS IN RANGE 10**-1 OR 10**-2, SO FORMAT PRETTY
C
        N = -IEXP
        L=NCHR
C  L = LEN(STRNG(ISTART))
        IZBGN = 1
        NIN = 5
        if(N .EQ. L) NIN = 4
C
C      IF N<L THEN WE NEED ONLY INSERT A DECIMAL POINT
C
        if(N .GE. L) go to 40
        IZBGN = 2
        NIN = 1
40      continue
C
C      ALLOW ROOM FOR DECIMAL POINT AND ZERO(S) IF NECESSARY
C
        IBEGIN = ISTART + MAX0(0,L-N)
        DO 50 I = 0, MIN0(N,L)
          STRNG(ISTART+L+NIN-I) = STRNG(ISTART+L-I)
50      continue
C
C      INSERT LEADING ZEROS IF NECESSARY, OR JUST DECIMAL POINT
C
        DO 60 I=0,NIN-1
          STRNG(IBEGIN+I) = BZERO(IZBGN+I)
60      continue
C
C      ALL DONE
C
800     continue
      endif
      call ZCH2IN(STRNG,ISTRNG)
      return
      end
      subroutine ZLOALF(CFLAG,CFONT,CSTYLE,CFLAG1,CFONT1,CSTY1)
C
C
C
C
C
C      LOAD CURRENT MIXED ALPHABETS
C
C      INPUT:   CFLAG1 = FONT INDICATORS
C               CFONT1 = FONT SETTING
C               CSTY1  = character STYLE
C
C      OUTPUT:  CFLAG  = DESTINATION FONT INDICATORS
C               CFONT  = DESTINATION FONT SETTING
C               CSTYLE = DESTINATION character STYLE
C
      character*1 CFLAG(6),CFLAG1(6)
      character*5 CFONT(6),CSTYLE,CFONT1(6),CSTY1
      DO 100 I=1,6
        CFLAG(I)=CFLAG1(I)
        CFONT(I)=CFONT1(I)
 100  continue
      CSTYLE=CSTY1
      return
      end
      subroutine ZMAPIT(XL,XH,YL,YH,LXLAB,IXLEN,LYLAB,IYLEN,
     *                  XPOS,YPOS,IAXES)
C
C
C
C
C     THIS ROUTINE DEFINES X, Y AXIS OR BOTH IN
C              A PREDEFINED PLOT AREA.
C
C     INPUT:   XL,XH       = RANGE OF X AXIS
C              YL,YH       = RANGE OF Y AXIS
C              LXLAB,LYLAB = AXIS LABELS
C              IXLEN,IYLEN = RESPECTIVE AXIS LENGTH
C              XPOS,YPOS   = OFFSET OF ORIGIN
C              IAXES       = AXIS FLAG
C                            IF BIT 0 SET, DEFINE X AXIS
C                            IF BIT 1 SET, DEFINE Y AXIS
C                            IF BIT 2 SET, SCALE X AXIS
C                            IF BIT 3 SET, SCALE Y AXIS
C                            IF BIT 5 SET, DRAW TITLE OF PLOT
C
      parameter (KZNO=222)
      parameter (KZREAL=2)
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CAXIS/  KZXTYP,KZYTYP,KZXNON,KZYNON,KZXTCK,KZYTCK,
     *                ZZXLBA,ZZYLBA
      common /CCOORD/ ZZXMIN,ZZXMAX,ZZXSTP,ZZYMIN,ZZYMAX,ZZYSTP
      common /CLABEL/ KZXLAB(20),KZXALN,KZYLAB(20),KZYALN,KZTITL(20),
     *                KZTILN,KZXNFL,KZYNFL
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /CUNIT/  ZZUNIT
      common /PLTCOM/ UX0, UDX, UY0, UDY, LOGX, LOGY
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      common /PLTCLP/ XMIN,XMAX,YMIN,YMAX
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI
      character CZALFL*1,CZALFN*5,CZALFS*5
      LOGICAL LOGX, LOGY
C
      integer NUMBR(4), NUMBR1(4), LXLAB(20),LYLAB(20)
      LOGICAL LXX, LYY, LHEAD, LRMTEX, LSHORT, LRAGGD
      LOGICAL LXSCAL,LYSCAL,LFIRST
C
      SAVE PIO180
C
      data PIO180 /1.745329252E-2/
C
C  MINIMUM DISTANCE BETWEEN SHORT TICKS (1 MM)
C
C                        CHECK IF X, Y AXIS DEFINITION OR BOTH
C                        ALSO CHECK IF TITLE IS REQUIRED
C
      TICKLN=0.6*ZZHITE
      LXX=KSYAND(IAXES,1).NE.0
      LYY=KSYAND(IAXES,2).NE.0
      LHEAD=KSYAND(IAXES,16).NE.0
      LSHORT= .FALSE.
      LRAGGD= .FALSE.
C
C                        OFFSET ACCORDING TO XPOS AND YPOS
C
      DX=XPOS*ZZUNIT*ZZPAGR
      DY=YPOS*ZZUNIT*ZZPAGR
C
C                        DEFINE X PLOT parameterS
C
      if(LXX)then
        XLOW=XL
        XHIGH=XL+(XH-XL)*ZZXAXR
C
C                        DO THE AXES SCALING
C
        LXSCAL= KSYAND(IAXES,4) .NE. 0
        if(LXSCAL)then
          NUMTKS=MIN0(10,INT(XVLEN/((ILABSZ()+1.0)*GSLENS('0'))))
          call AXIS(XLOW,XHIGH,NUMTKS,LSHORT,LRAGGD,XMIN,XMAX,
     *                XTMIN,XTMAX,XTICK,IXPWR)
          ZZXSTP=XTICK*10.0**IXPWR
          if((ZZXSTP*ZZXAXS*ZZXAXR/(XMAX-XMIN)).GT.4.5)then
            ZZXSTP=ZZXSTP/2.0
            XTICK=XTICK/2.0
          endif
        else
          call ZAXIS(XLOW,XHIGH,ZZXSTP,XTMIN,XTMAX,
     *                 XTICK,IXPWR)
          XMAX=XHIGH
          XMIN=XLOW
        endif
        ZZXMIN=XMIN
        ZZXMAX=XMAX
C
C                        SET UP SCALING FACTORS
C
        UX0 = XMIN
        UDX = XMAX - XMIN
      endif
C
C                        DEFINE Y PLOT parameterS
C
      if(LYY)then
        YLOW=YL
        YHIGH=YL+(YH-YL)*ZZYAXR
C
C                        PROTECT MAPPRM INFO THIS WAY
C
        YVLEN = YVINI
C
C                        DO THE AXES SCALING
C
        LYSCAL= KSYAND(IAXES,8) .NE. 0
        if(LYSCAL)then
          NUMTKS=MIN0(10,INT(YVLEN/((ILABSZ()+1.0)*GSLENS('0'))))
          call AXIS(YLOW,YHIGH,NUMTKS,LSHORT,LRAGGD,YMIN,YMAX,
     *                YTMIN,YTMAX,YTICK,IYPWR)
          ZZYSTP=YTICK*10.0**IYPWR
          if((ZZYSTP*ZZYAXS*ZZYAXR/(YMAX-YMIN)).GT.4.5)then
            ZZYSTP=ZZYSTP/2.0
            YTICK=YTICK/2.0
          endif
        else
          call ZAXIS(YLOW,YHIGH,ZZYSTP,YTMIN,YTMAX,
     *                YTICK,IYPWR)
          YMAX=YHIGH
          YMIN=YLOW
        endif
        ZZYMIN=YMIN
        ZZYMAX=YMAX
C
C                        SET UP SCALING FACTORS
C
        UY0 = YMIN
        UDY = YMAX - YMIN
      endif
C
C  DRAW X AXIS
C
      if(LXX.AND.IXLEN.NE.0)then
        call GSSETC(0.7*ZZHITE,ZZXLBA)
C
C                        CALCULATE ORIGIN VALUES
C
        Y = YMIN
        TENEXP = 10.0**IXPWR
        call SCALE(XMIN,Y,VX,VY)
        VXOLD=VX
        EPSLON=0.1*XTICK*TENEXP
C
C                        LOOP TO DRAW AND LABEL X AXIS TICKS
C
        COSA=COS(ZZXLBA*PIO180)
        SINA=SIN(ZZXLBA*PIO180)
        ANGMAX=0.0
        X = XTMIN
        LFIRST=.TRUE.
C
 100    continue
        call SCALE(X*TENEXP,Y,VX,VY)
C
C                        DRAW TICKS ACCORDING TO "YMARKS"
C
        if(KZXTCK.NE.0)then
          call DSMOVE(VXOLD+DX,VY+DY)
          call DSDRAW(VX+DX,VY+DY)
          call DSDRAW(VX+DX,VY-TICKLN+DY)
          if(KZXTCK.LT.0)then
            call ERRMES('ZMAPIT',KZXTCK,4)
          elseif(KZXTCK.GT.1)then
            TKINCR=(VX-VXOLD)/KZXTCK
            DO 200 J=1,KZXTCK-1
              call DSMOVE(VXOLD+DX+TKINCR*J,VY+DY)
              call DSDRAW(VXOLD+DX+TKINCR*J,VY-TICKLN*0.6+DY)
 200        continue
          endif
        endif
        VXOLD=VX
C
C                        IF "NOXLBL" IS NOT CALLED
C
        if(KZXNON.EQ.KZNO)then
          if(KZXTYP.EQ.KZREAL)then
            call ZLNLAB('R',X,IXPWR,NUMBR,LRMTEX)
          else
            call ZLNLAB('I',X,IXPWR,NUMBR,LRMTEX)
          endif
C
C                        TILT NUMBERINGS ACCORDING TO "XANGLE"
C
          LN=LENG(NUMBR)
          RLEN=ZXMESS(NUMBR,LN,CZALFL,CZALFN,CZALFS)
          ANGX=RLEN*COSA
          ANGY=RLEN*SINA
          if(ZZXLBA.LT.3.0)then
            ANGX=ANGX/2.0
            ANGY=1.7*ZZHITE
            if(ANGMAX.LT.ANGY) ANGMAX=ANGY
          elseif(ZZXLBA.LT.270.0)then
            ANGX=ANGX-0.35*ZZHITE
            ANGY=ANGY+1.7*ZZHITE
            if(ANGMAX.LT.ANGY) ANGMAX=ANGY
          else
            if(ANGMAX.LT.(1.7*ZZHITE-ANGY))
     *            ANGMAX=1.7*ZZHITE-ANGY
            ANGX=0.35*ZZHITE
            ANGY=1.7*ZZHITE
          endif
          if(LFIRST)then
            call DSMOVE(VX+DX,VY-ANGY+DY)
            LFIRST=.FALSE.
          else
            call DSMOVE(VX-ANGX+DX,VY-ANGY+DY)
          endif
          call ZTEXT(NUMBR,LN,CZALFL,CZALFN,CZALFS)
        else
          ANGMAX=1.7*ZZHITE
        endif
        X = X + XTICK
        XACT=X*TENEXP-EPSLON
        if((XACT.LE.XMAX.AND.XTICK.GT.0.0)
     *        .OR.(XACT.GE.XMAX.AND.XTICK.LT.0.0)) go to 100
C
C                        end LOOP
C
C                        NOW PLACE REMOTE EXPONENT
C                        IF NEEDED ON LINEAR AXIS
C
        if(LRMTEX.AND.(KZXNON.EQ.KZNO))then
          call GSSETC(0.7*ZZHITE,0.0)
          call SCALE((X-XTICK)*TENEXP,YMIN,VX,VY)
CW          call ZSCOPY('*10',LEXPO)
          call SYBYT4('N',LEXPO,3,48)
          call SYBYT4('N',LEXPO,2,49)
          call SYBYT4('N',LEXPO,1,42)
          NUMBR1(1)=-1
          call ZNMSTR(REAL(IXPWR),NUMBR1)
          RLEN=ZXMESS(LEXPO,3,CZALFL,CZALFN,CZALFS)*0.9/1.4
          call DSMOVE(VX+RLEN+DX,VY-ANGMAX-1.2*ZZHITE+DY)
          LN=LENG(NUMBR1)
          call ZTEXT(NUMBR1,LN,CZALFL,CZALFN,CZALFS)
          call GSSETC(ZZHITE*0.9,0.0)
          call DSMOVE(VX-RLEN+DX,VY-ANGMAX-1.7*ZZHITE+DY)
          call ZTEXT(LEXPO,3,CZALFL,CZALFN,CZALFS)
        endif
C
C                        NOW PLACE X AXIS LABEL
C
        call GSSETC(ZZHITE,0.0)
        call SCALE((XMIN+XMAX)/2.0,YMIN,VX,VY)
        RLEN=ZXMESS(LXLAB,IXLEN,CZALFL,CZALFN,CZALFS)
        call DSMOVE(VX-RLEN/2.0+DX,
     *                VY-ANGMAX-1.7*ZZHITE+DY)
        call ZTEXT(LXLAB,IXLEN,CZALFL,CZALFN,CZALFS)
        if(KZXNFL.EQ.KZNO) KZXALN=0
      endif
C
C                        ********** DRAW Y AXES **********
C
      if(LYY.AND.IYLEN.NE.0)then
        call GSSETC(0.7*ZZHITE,ZZYLBA)
C
C                        DRAW Y AXIS LINE
C
        X = XMIN
        TENEXP = 10.0**IYPWR
        call SCALE(X,YMIN,VX,VY)
        VYOLD=VY
        EPSLON=0.1*YTICK*TENEXP
C
C                        LOOP TO DRAW AND LABEL Y AXIS
C
        COSA=COS(ZZYLBA*PIO180)
        SINA=SIN(ZZYLBA*PIO180)
        ANGMAX=0.0
        Y = YTMIN
        LFIRST=.TRUE.
C
 300    continue
        call SCALE(X,Y*TENEXP,VX,VY)
C
C                        DRAW TICKS ACCORDING TO "YMARKS"
C
        if(KZYTCK.NE.0)then
          call DSMOVE(VX+DX,VYOLD+DY)
          call DSDRAW(VX+DX,VY+DY)
          call DSDRAW(VX+DX-TICKLN,VY+DY)
          if(KZYTCK.LT.0)then
            call ERRMES('ZMAPIT',KZYTCK,5)
          elseif(KZYTCK.GT.1)then
            TKINCR=(VY-VYOLD)/KZYTCK
            DO 400 J=1,KZYTCK-1
              call DSMOVE(VX+DX,VYOLD+TKINCR*J+DY)
              call DSDRAW(VX+DX-TICKLN*0.6,VYOLD+TKINCR*J+DY)
 400        continue
          endif
        endif
        VYOLD=VY
C
C                        IF "NOYLBL" IS NOT CALLED
C
        if(KZYNON.EQ.KZNO)then
          if(KZYTYP.EQ.KZREAL)then
            call ZLNLAB('R',Y,IYPWR,NUMBR,LRMTEX)
          else
            call ZLNLAB('I',Y,IYPWR,NUMBR,LRMTEX)
          endif
C
C                        TILT NUMBERINGS ACCORDING TO "YANGLE"
C
          LN=LENG(NUMBR)
          RLEN=ZXMESS(NUMBR,LN,CZALFL,CZALFN,CZALFS)
          ANGX=RLEN*COSA+ZZHITE
          ANGY=RLEN*SINA
          if(LFIRST)then
            if(ZZYLBA.LT.270.0)then
              call DSMOVE(VX-ANGX+DX,VY+DY)
            else
              call DSMOVE(VX-ANGX+DX,VY-ANGY+DY)
            endif
            LFIRST=.FALSE.
          else
            if(ZZYLBA.LT.93.0.AND.ZZYLBA.GT.87.0)then
              call DSMOVE(VX-ANGX+DX,VY-RLEN/2.0+DY)
            elseif(ZZYLBA.LT.270)then
              call DSMOVE(VX-ANGX+DX,VY-ANGY-0.35*ZZHITE+DY)
            else
              call DSMOVE(VX-ANGX+DX,VY-ANGY+0.35*ZZHITE+DY)
            endif
          endif
          if(ANGMAX.LT.ANGX) ANGMAX=ANGX
          call ZTEXT(NUMBR,LN,CZALFL,CZALFN,CZALFS)
        else
          ANGMAX=ZZHITE
        endif
        Y = Y + YTICK
        YACT=Y*TENEXP-EPSLON
        if((YACT.LE.YMAX.AND.YTICK.GT.0.0)
     *         .OR.(YACT.GE.YMAX.AND.YTICK.LT.0.0)) go to 300
C
C                        end LOOP
C
C                        IF LINEAR AXIS, PLACE REMOTE
C                        EXPONENT IF NEEDED
C
        if(LRMTEX.AND.(KZYNON.EQ.KZNO))then
          call GSSETC(0.7*ZZHITE,90.0)
          call SCALE(XMIN,(Y-YTICK)*TENEXP,VX,VY)
          NUMBR1(1)=-1
          call ZNMSTR(REAL(IYPWR),NUMBR1)
CW          call ZSCOPY('*10',LEXPO)
          call SYBYT4('N',LEXPO,3,48)
          call SYBYT4('N',LEXPO,2,49)
          call SYBYT4('N',LEXPO,1,42)
          RLEN=ZXMESS(LEXPO,3,CZALFL,CZALFN,CZALFS)*0.9/1.4
          call DSMOVE(VX-ANGMAX-1.7*ZZHITE+DX,VY+RLEN+DY)
          LN=LENG(NUMBR1)
          call ZTEXT(NUMBR1,LN,CZALFL,CZALFN,CZALFS)
          call GSSETC(0.9*ZZHITE,90.0)
          call DSMOVE(VX-ANGMAX-1.2*ZZHITE+DX,VY-RLEN+DY)
          call ZTEXT(LEXPO,3,CZALFL,CZALFN,CZALFS)
        endif
C
C                        NOW PLACE Y LABEL
C
        call GSSETC(ZZHITE,90.0)
        call SCALE(XMIN,(YMIN+YMAX)/2.0,VX,VY)
        RLEN=ZXMESS(LYLAB,IYLEN,CZALFL,CZALFN,CZALFS)
        call DSMOVE(VX-ANGMAX-1.4*ZZHITE+DX,VY-RLEN/2.0+DY)
        call ZTEXT(LYLAB,IYLEN,CZALFL,CZALFN,CZALFS)
        call GSSETC(ZZHITE,0.0)
        if(KZYNFL.EQ.KZNO) KZYALN=0
      endif
C
C                        ********** PLACE TITLE **********
C
      if(LHEAD.AND.KZTILN.NE.0)then
        call GSSETC(1.46*ZZHITE,0.0)
        SPACE=AMAX1(ZZHITE,0.1*YVLEN)
        call SCALE((XMIN+XMAX)/2.0,YMAX,VX,VY)
        RLEN=ZXMESS(KZTITL,KZTILN,CZALFL,CZALFN,CZALFS)
        call DSMOVE(VX-RLEN/2.0+DX,VY+SPACE+DY)
        call ZTEXT(KZTITL,KZTILN,CZALFL,CZALFN,CZALFS)
        call GSSETC(ZZHITE,0.0)
      endif
      return
      end
      subroutine ZMAPRM(XLEFT,XRIGHT,YBOT,YTOP,CSIZE,TKLN)
C
C
C
C
      common /PLTSIZ/ XVSTRT, YVSTRT, XVLEN, YVLEN
      common /PLTPRM/ CXSIZE, CYSIZE, TICKLN, YVINI
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      character CZALFL*1,CZALFN*5,CZALFS*5
C
      call GSSETC(CSIZE,0.0)
      CXSIZE = ZXMESS('0',1,CZALFL,CZALFN,CZALFS)
      CYSIZE = CSIZE
      TICKLN = TKLN
      XVSTRT=XLEFT
      XVLEN=XRIGHT-XLEFT
      YVSTRT=YBOT
      YVINI=YTOP-YBOT
      return
      end
      subroutine ZNMSTR(VAL,BSTRNG)
C
C
C
C
      integer BSTRNG(*)
C
C     THIS ROUTINE CONVERTS "VAL" TO A STRING
C              WITH NO LEADING OR TRAILING SPACES.
C
C     INPUT:   VAL = INPUT VALUE
C
C     OUTPUT:  BSTRNG = OUTPUT STRING
C
C  ELIMINATE EQUIVALENCING PROBLEM
C     character STR*12
C     integer NUMBER(3)
C     EQUIVALENCE (STR,NUMBER)
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      parameter (NMWRD=16/NUMBYT)
      character STR*(NUMBYT*NMWRD)
      integer NUMBER(NMWRD)
C
 11   FORMAT(F11.1)
 12   FORMAT(F11.2)
 13   FORMAT(F11.3)
 14   FORMAT(F11.4)
 15   FORMAT(I11)
      if(BSTRNG(1).GE.0)then
        if(VAL.LT.1.0)then
          WRITE(STR,14)VAL
        elseif(VAL.LT.10.0)then
          WRITE(STR,13)VAL
        elseif(VAL.LT.100.0)then
          WRITE(STR,12)VAL
        else
          WRITE(STR,11)VAL
        endif
        STR(12:12) = CHAR(0)
        K=11
 100    continue
        if(STR(K:K).EQ.'0'.AND.STR(K-1:K-1).NE.'.')then
          STR(K:K)=CHAR(0)
          K=K-1
          if(K.GE.1) go to 100
        else
          go to 200
        endif
 200    continue
      else
C  CHANGE FROM TRUNCATION TO NEAREST integer
C       WRITE(STR,15)INT(VAL+0.001)
        WRITE(STR,15)NINT(VAL)
        STR(12:12) = CHAR(0)
      endif
C
      DO 300 I=1,NMWRD
        call WCH2IN(STR((I-1)*NUMBYT+1:I*NUMBYT),NUMBER(I))
300   continue
      call ZSTRBL(NUMBER)
      call ZSCOPY(NUMBER,BSTRNG)
      return
      end
C
C
C
      subroutine ZPNT(X,Y,Z,NZ)
      DIMENSION Z(NZ,2)
      common /CONTR/ CLEVEL,IOLD,JOLD,IN,JN,
     1   NX,NY,XL,DX,YL,DY
      A=Z(IN,JN)-Z(IOLD,JOLD)
C     IF NO CHANGE IN Z'S, PICK OLD POINT SO AS TO STAY TO RIGHT
      if(A .EQ. 0.0) go to 10
      A=(CLEVEL-Z(IOLD,JOLD))/A
10    X=A*(IN-IOLD)+IOLD
      Y=A*(JN-JOLD)+JOLD
C     NOW CONVERT INDEXS TO X,Y VALUES
      X=(X-1.0)*DX/(NX-1)+XL
      Y=(Y-1.0)*DY/(NY-1)+YL
      return
      end
      subroutine ZPPOLY(Y1,Y2,Y3,Y4,A,B,C,D)
C
C  PERFORMS PARAMETRIC CUBIC POLYNOMIAL INTERPOLATION
C
      A=( -1.0*Y1 + 3.0*Y2 -3.0*Y3 +    Y4)/6.0
      B=(  2.0*Y1 - 5.0*Y2 +4.0*Y3 -    Y4)/2.0
      C=(-11.0*Y1 +18.0*Y2 -9.0*Y3 +2.0*Y4)/6.0
      D=       Y1
      return
      end
      subroutine ZREAL(TNUM,IPLACE,LMESS,LPOWER)
C
C      CONVERTS A REAL NUMBER TO A character STRING
C
C      INPUT:   TNUM   = INPUT REAL NUMBER
C               IPLACE = FORMAT FLAG
C                        IF > 100.0 THEN FIXED LENGTH FORMAT
C                        IF < 100.0 AND > 0.0 THEN FLOATING POINT
C                          FORMAT WITH IPLACE DECIMAL PLACES
C                        IF < 0.0 THEN EXPONENTIAL NUMBER WITH
C                          -IPLACE DECIMAL POINT
C
C      OUTPUT:  LMESS = character STRING OF TNUM
C               LPOWER = character STRING OF POWER, IF ANY
C
C
C     DIMENSION LMESS(*),LMESSA(20),LMESSB(20),LPOW(2)
C     character CMESSA*80,CMESSB*80,CPOWER*8
C     EQUIVALENCE (CMESSA,LMESSA),(CMESSB,LMESSB),(CPOWER,LPOW)
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      parameter (NUMWRD=80/NUMBYT)
      DIMENSION LMESS(*),LMESSA(NUMWRD),LMESSB(NUMWRD),LPOW(8/NUMBYT)
      character CMESSA*80,CMESSB*80,CPOWER*8
C
C
      SAVE EPSLON
      data EPSLON /1.0E-20/
      LPOWER=0
      RNUM=ABS(TNUM)
      NPOW = 0
C
C                        CALCULATE POWER OF INPUT VALUE
C
      if(RNUM.LE.EPSLON) go to 600
      IF(RNUM .LT. 0.9999995)then
 200    continue
        RNUM = RNUM*10.0
        NPOW = NPOW-1
        IF(RNUM .LT. 0.9999995) go to 200
      elseif(RNUM.GT.9.999995)then
 100    continue
        RNUM = RNUM/10.0
        NPOW = NPOW+1
        if(RNUM.GT.9.999995) go to 100
      endif
C
C                        ROUNDOFF INPUT VALUE
C
 600  continue
      if(RNUM.LT.0.0000005)then
        RNUM=0.0
      else
        RNUM=RNUM+0.0000005
      endif
      ANUM=RNUM*10.0**NPOW
      if(TNUM.LT.0.0)then
        CMESSB(1:1)='-'
      else
        CMESSB(1:1)=' '
      endif
C
C                        FIXED LENGTH FORMAT
C
      if(IPLACE.GT.100)then
        IP=IPLACE-100
        IP1=IP+1
        IP2=IP+2
        IP3=IP+3
        IP5=IP+5
        IP6=IP+6
        NUM=10
C
C                        FLOATING POINT NUMBER OF FIXED LENGTH
C
        if(ABS(NPOW).LT.IP)then
          WRITE(CMESSA,10)ANUM
 10       FORMAT(1X,F79.39)
C
          DO 5000 I=1,NUMWRD
            call WCH2IN(CMESSA((I-1)*NUMBYT+1:I*NUMBYT),LMESSA(I))
5000      continue
          call ZSTRBL(LMESSA)
C
          DO 5001 I=1,NUMWRD
            call WIN2CH(LMESSA(I),CMESSA((I-1)*NUMBYT+1:I*NUMBYT))
5001      continue
          CMESSB(2:IP2)=CMESSA(1:IP1)
          CMESSB(IP3:IP3)=CHAR(0)
          if(NPOW.GE.7) NUM=9
          if(IP2.GE.NUM)then
            DO 400 I=IP2,NUM,-1
              if(CMESSB(I:I).NE.'.') CMESSB(I:I)='0'
 400        continue
          endif
C
          DO 5010 I=1,NUMWRD
            call WCH2IN(CMESSB((I-1)*NUMBYT+1:I*NUMBYT),LMESSB(I))
5010      continue
          call ZSCOPY(LMESSB,LMESS)
C
C                        NUMBER WITH EXPONETIAL IN FIXED FORMAT
C
        else
          WRITE(CMESSA,10)RNUM
C
          DO 5020 I=1,NUMWRD
            call WCH2IN(CMESSA((I-1)*NUMBYT+1:I*NUMBYT),LMESSA(I))
5020      continue
          call ZSTRBL(LMESSA)
C
          DO 5021 I=1,NUMWRD
            call WIN2CH(LMESSA(I),CMESSA((I-1)*NUMBYT+1:I*NUMBYT))
5021      continue
          CMESSB(2:IP2)=CMESSA(1:IP1)
          CMESSB(IP3:IP5)='*10'
          CMESSB(IP6:IP6)=CHAR(0)
          if(IP2.GE.NUM)then
            DO 300 I=IP2,NUM,-1
              if(CMESSB(I:I).NE.'.') CMESSB(I:I)='0'
 300        continue
          endif
C
          DO 5030 I=1,NUMWRD
            call WCH2IN(CMESSB((I-1)*NUMBYT+1:I*NUMBYT),LMESSB(I))
5030      continue
          call ZSCOPY(LMESSB,LMESS)
          WRITE(CPOWER,20)NPOW
 20       FORMAT(1X,I3)
C
          DO 5080 I=1,8/NUMBYT
            call WCH2IN(CPOWER((I-1)*NUMBYT+1:I*NUMBYT),LPOW(I))
5080      continue
          call ZSTRBL(LPOW)
C
          DO 5081 I=1,8/NUMBYT
            call WIN2CH(LPOW(I),CPOWER((I-1)*NUMBYT+1:I*NUMBYT))
5081      continue
          CPOWER(4:4)=CHAR(0)
          DO 5090 I=1,8/NUMBYT
            call WCH2IN(CPOWER((I-1)*NUMBYT+1:I*NUMBYT),LPOW(I))
5090      continue
          call ZSTRBT(LPOW)
          call ZSCOPY(LPOW,LPOWER)
        endif
C
C   FLOATING POINT NUMBER
C
      elseif(IPLACE.GE.0)then
        WRITE(CMESSA,10)ANUM
C
          DO 5040 I=1,NUMWRD
            call WCH2IN(CMESSA((I-1)*NUMBYT+1:I*NUMBYT),LMESSA(I))
5040      continue
        call ZSTRBL(LMESSA)
C
          DO 5041 I=1,NUMWRD
            call WIN2CH(LMESSA(I),CMESSA((I-1)*NUMBYT+1:I*NUMBYT))
5041      continue
        DO 1200 J=1,40
          if(CMESSA(J:J).EQ.'.') go to 500
 1200   continue
 500    continue
        IP=IPLACE+J+1
        CMESSB(2:IP)=CMESSA(1:IP-1)
        CMESSB(IP+1:IP+1)=CHAR(0)
        if(NPOW.GE.7)then
          NUM=9
        else
          NUM=10
        endif
        if(NPOW.LE.0)then
          IP=IP+NPOW
          IOFF=-NPOW
        else
          IOFF=0
        endif
        if(IP.GE.NUM)then
          DO 1000 I=IP+IOFF,NUM+IOFF,-1
            if(CMESSB(I:I).NE.'.') CMESSB(I:I)='0'
 1000     continue
        endif
C
          DO 5050 I=1,NUMWRD
            call WCH2IN(CMESSB((I-1)*NUMBYT+1:I*NUMBYT),LMESSB(I))
5050      continue
        call ZSCOPY(LMESSB,LMESS)
C
C                        NUMBER WITH EXPONENTIAL
C
      else
        IP=-IPLACE
        WRITE(CMESSA,10)RNUM
C
        DO 5060 I=1,NUMWRD
          call WCH2IN(CMESSA((I-1)*NUMBYT+1:I*NUMBYT),LMESSA(I))
5060    continue
        call ZSTRBL(LMESSA)
C
        DO 5061 I=1,NUMWRD
          call WIN2CH(LMESSA(I),CMESSA((I-1)*NUMBYT+1:I*NUMBYT))
5061    continue
        DO 700 J=1,40
          if(CMESSA(J:J).EQ.'.') go to 800
 700    continue
 800    continue
        IP=IP+J+1
        CMESSB(2:IP)=CMESSA(1:IP-1)
        CMESSB(IP+1:IP+3)='*10'
        CMESSB(IP+4:IP+4)=CHAR(0)
        if(IP.GE.10)then
          DO 1100 I=IP,10,-1
            if(CMESSB(I:I).NE.'.')CMESSB(I:I)='0'
 1100     continue
        endif
C
        DO 5070 I=1,NUMWRD
          call WCH2IN(CMESSB((I-1)*NUMBYT+1:I*NUMBYT),LMESSB(I))
5070    continue
        call ZSCOPY(LMESSB,LMESS)
        WRITE(CPOWER,20)NPOW
C
        DO 5100 I=1,8/NUMBYT
          call WCH2IN(CPOWER((I-1)*NUMBYT+1:I*NUMBYT),LPOW(I))
5100    continue
        call ZSTRBL(LPOW)
        CPOWER(4:4)=CHAR(0)
C
        DO 5110 I=1,8/NUMBYT
          call WCH2IN(CPOWER((I-1)*NUMBYT+1:I*NUMBYT),LPOW(I))
5110    continue
C
        call ZSTRBT(LPOW)
        call ZSCOPY(LPOW,LPOWER)
      endif
      return
      end
      subroutine ZSCOPY(KIN,KOUT)
C
C
C
C
C      COPIES A STRING
C
C      INPUT:   KIN = ORIGINAL STRING
C
C      OUTPUT:  KOUT = OUTPUT STRING
C
C----------------------------BEGIN 32-BIT SPECIFIC CODE-------------------
      parameter (NUMBYT=4)
C-----------------------------END 32-BIT SPECIFIC CODE--------------------
C----------------------------BEGIN 64-BIT SPECIFIC CODE-------------------
C     parameter (NUMBYT=8)
C-----------------------------END 64-BIT SPECIFIC CODE--------------------
      DIMENSION KIN(*),KOUT(*)
C
      LAST=LENG(KIN)
C
C     NUM=LAST/4
      NUM=LAST/NUMBYT
      if(NUM.NE.0)then
        DO 100 I=1,NUM
          KOUT(I)=KIN(I)
 100    continue
      endif
C
C     IEND=MOD(LAST,4)
      IEND=MOD(LAST,NUMBYT)
      NUM=NUM+1
      if(IEND.NE.0)then
        DO 200 I=1,IEND
          call SYBYT4('X',KIN(NUM),I,NCHAR)
          call SYBYT4('N',KOUT(NUM),I,NCHAR)
 200    continue
      endif
      call SYBYT4('N',KOUT(NUM),IEND+1,0)
      return
      end
      subroutine ZSTOP
C
C
C
C  STOP FOR FATAL ERRORS
C
      common /CLEVEL/ KZLEVL,KZBEGN
C
        PRINT *,' ### STOPPED AT LEVEL ',KZLEVL
      return
      end
      subroutine ZSTPLT(IPLOT)
C
C
C
C     TERMINATE CURRENT PLOT
C              (LEVEL 2,3, CHANGE TO 1)
C
C     INPUT:   IPLOT = NOT USED IN DISSIM
C
      parameter (KZYES=111)
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      DUMMY = -10.0
C
C     DETERMINE AUTO-COPY OPTION SETTING.
C
      if(KZCOPY.EQ.KZYES) DUMMY = 1.0
      call GSDRVR(5,DUMMY,DUMMY)
C
      KZNPLT=KZNPLT+1
      call ZINIT
      return
      end
      subroutine ZSTRBL(ISTR)
C
C
C
C
C
C      STRIPS LEADING BLANK IN A STRING
C
C      INPUT:   ISTR = INPUT STRING
C
C
C     character CMESS*152
C     DIMENSION ISTR(*),IMESSA(38)
C     EQUIVALENCE (IMESSA,CMESS)
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      character CMESS*160
      DIMENSION ISTR(*),IMESSA(160/NUMBYT)
C
      LENGTH=LENG(ISTR)
      call ZSCOPY(ISTR,IMESSA)
C
      DO 5000 I=1,(LENGTH-1)/NUMBYT+1
        call WIN2CH(IMESSA(I),CMESS((I-1)*NUMBYT+1:I*NUMBYT))
5000  continue
      I=1
 1    continue
      if(I.GE.LENGTH.OR.CMESS(I:I).NE.' ')then
        DO 200 J=I,LENGTH+1
          K=J-I+1
          CMESS(K:K)=CMESS(J:J)
 200    continue
C
C
C NULL OUT THE REMAINDER OF THE STRING THAT MAY HAVE BEEN MOVED
C AND THEN CONVERT character STRING BACK TO integer ARRAY
C
        DO 300 I=K+1,LENGTH
          CMESS(I:I)=CHAR(0)
300     continue
        DO 5100 I=1,(LENGTH-1)/NUMBYT+1
          call WCH2IN(CMESS((I-1)*NUMBYT+1:I*NUMBYT),IMESSA(I))
5100    continue
        call ZSCOPY(IMESSA,ISTR)
        return
      else
        I=I+1
      endif
      go to 1
      end
      subroutine ZSTRBT(LMESS)
C
C
C
C      STRIP TRAILING BLANKS
C
C      INPUT:   LMESS = INPUT STRING
C
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      DIMENSION LMESS(*)
      IMESS=LENG(LMESS)
C
      INDX=(IMESS-1)/NUMBYT+1
      IPOS=IMESS-NUMBYT*(INDX-1)
c
      DO 100 I=IPOS,1,-1
        call SYBYT4('X',LMESS(INDX),I,KHAR)
        if(KHAR.EQ.32)then
          call SYBYT4('N',LMESS(INDX),I,0)
        else
          return
        endif
 100  continue
c
      DO 200 I=INDX-1,1,-1
        DO 300 J=NUMBYT,1,-1
          call SYBYT4('X',LMESS(I),J,KHAR)
          if(KHAR.EQ.32)then
            call SYBYT4('N',LMESS(I),J,0)
          else
            return
          endif
 300    continue
 200  continue
      return
      end
      subroutine ZSWAP(A,B)
C
C      THIS ROUTINE SWAPS TWO REAL NUMBERS,
C               A AND B
C      INPUT:   A,B      NUMBERS TO BE SWAPPED
C
      T=A
      A=B
      B=T
      return
      end
      subroutine ZTEXT(LMESS,IMESS,CFLAG,CFONT,CSTYLE)
C
C
C
C      WRITE GRAPHIC TEXT STRING IN MIXED ALPHABETS
C
C      INPUT:   LMESS  = INPUT STRING
C               IMESS  = NUMBER OF characterS IN LMES
C               CFLAG  = FONT INDICATORS
C               CFONT  = FONT SETTING
C               CSTYLE = character STYLE
C
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      integer LMESS1(40),LMESS(*),KFONT(6)
      character*1 CH(160),BUF(160),CFLAG(6)
      character*5 CFONT(6),CSTYLE
      LOGICAL FINIS,FIRST
C
C                        MIX ALPHABET NOT ACTIVE, WRITE STRING
C
      if(CFLAG(1).EQ.' ')then
        call ZGETFN(CFONT(1),CSTYLE,KFONT(1))
        call GSFONT(KFONT(1),IERR)
        call ZCOSTR(LMESS,IMESS,LMESS1,IMESS1)
        call DSPSTR(LMESS1)
C
C                        PROCESS USING FLAGS SET BY MIX
C                        ALPHABET ROUTINES
C
      else
        DO 100 I=1,6
          call ZGETFN(CFONT(I),CSTYLE,KFONT(I))
 100        continue
        call ZIN2CH(LMESS,CH)
        IPTR=1
        ICUR=1
        NBUF=0
        FIRST=.TRUE.
        call GSFONT(KFONT(1),IERR)
        DO 200 I=1,IMESS
C
C                        LOOK FOR STRING TERMINATOR
C
          if(IMESS.EQ.100)then
            FINIS=.TRUE.
            DO 500 J=1,KZTMLN
              call SYBYT4('X',KZSTRM,J,KHAR)
              FINIS=FINIS.AND.(ICHAR(CH(I+J-1)).EQ.KHAR)
 500            continue
C
C                        IF FOUND, EMPTY BUFFER AND return
C
            if(IPTR.EQ.1.AND.FINIS)then
                if(NBUF.GT.0)then
                BUF(NBUF+1)=CHAR(0)
                call ZCH2IN(BUF,LMESS1)
                call DSPSTR(LMESS1)
                endif
              return
            endif
          endif
C
C                        LOOK FOR MIXALF FLAGS
C
 300          continue
          if(CH(I).EQ.CFLAG(ICUR).AND.CH(I).NE.' '.AND.
     *          (ICUR.NE.IPTR.OR.FIRST))then
            if(NBUF.GT.0)then
              BUF(NBUF+1)=CHAR(0)
              call ZCH2IN(BUF,LMESS1)
              call DSPSTR(LMESS1)
              NBUF=0
            endif
            call GSFONT(KFONT(ICUR),IERR)
            FIRST=.FALSE.
            IPTR=ICUR
            ICUR=ICUR+1
            if(ICUR.GT.6) ICUR=1
            go to 200
          else
            ICUR=ICUR+1
            if(ICUR.GT.6) ICUR=1
            if(ICUR.NE.IPTR) go to 300
C
C                        SAVE TEXT IN BUFFER
C
            NBUF=NBUF+1
            BUF(NBUF)=CH(I)
          endif
 200      continue
C
C  END WHILE
C
        if(NBUF.NE.0)then
          BUF(NBUF+1)=CHAR(0)
          call ZCH2IN(BUF,LMESS1)
          call DSPSTR(LMESS1)
        endif
      endif
      return
      end
      subroutine ZVECTR(VXFROM,VYFROM,VXTO,VYTO,IVEC)
C
C
C
C      DRAW A VECTOR W/ OR W/O ARROW HEAD
C
C      INPUT:   VXFROM,VYFROM = FIRST POINT OF VECTOR
C               VXTO,VYTO     = SECOND POINT OF VECTOR
C               IVEC(WXYZ)    = ARROW TYPE AND SIZE FLAG
C
      common /CLINE/  KZLSTY,KZLTYP,ZZLTHK,KZLTHK,ZZPRX,ZZPRY,
     *                ZZPRX1,ZZPRX2,ZZPRY1,ZZPRY2,UULTHK,KZLNCN
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
      common /GCCLIP/ XCM0, XCM1, YCM0, YCM1
C
      DIMENSION X(4),Y(4),TX(4),TY(4)
C
      SAVE EPSLON,RUNIT
      data EPSLON /0.0001/
      data RUNIT /0.12/
      IFLAG1=MOD(IVEC,10)
      IFLAG2=MOD(IVEC/10,10)
      IFLAG3=MOD(IVEC/100,10)
      IFLAG4=IVEC/1000
C
C                        IF ARROW HEAD DESIRED
C
      if(IFLAG1.NE.0)then
C
C                        SET ARROW SIZE
C
        if(IFLAG3.EQ.0)then
          RLEN=ZZHITE
        else
          RLEN=REAL(IFLAG3)*RUNIT
        endif
C
C                        SET CROSS LINE LENGTH
C
        CROSS=REAL(IFLAG4+1)*RLEN/6.0
        DX=VXTO-VXFROM
        DY=VYTO-VYFROM
C
C                        LOCATE VERTICES OF ARROWHEAD
C
        if(ABS(DX).LT.EPSLON)then
          XM=0.0
          YM=RLEN*DY/ABS(DY)
          FX2=CROSS
          FX3=-CROSS
          FY2=0.0
          FY3=0.0
        else
          TANVAL=DY/DX
          THETA=ATAN(TANVAL)
          if(DX.LT.0.0)then
            COST=-COS(THETA)
            SINT=-SIN(THETA)
          else
            COST=COS(THETA)
            SINT=SIN(THETA)
          endif
          YM=SINT*RLEN
          XM=COST*RLEN
          if(DY.LT.0.0)then
            DELY=-CROSS*COST
            DELX=CROSS*SINT
          else
            DELY=CROSS*COST
            DELX=-CROSS*SINT
          endif
          FX2=DELX
          FX3=-DELX
          FY2=DELY
          FY3=-DELY
        endif
        X(1)=VXTO-XM+FX2
        X(2)=VXTO
        X(3)=VXTO-XM+FX3
        X(4)=X(1)
        Y(1)=VYTO-YM+FY2
        Y(2)=VYTO
        Y(3)=VYTO-YM+FY3
        Y(4)=Y(1)
        call ZDISPT(VXTO,VYTO,VXFROM,VYFROM,RLEN,XP,YP)
        NUMPT=4
C
C                        FIND ENDPOINTS OF VECTOR
C
        if(IFLAG2.EQ.1)then
          if(IFLAG1.EQ.1)then
            call DSMOVE(VXFROM,VYFROM)
          else
            call DSMOVE(XP,YP)
          endif
          call DSDRAW(VXTO-XM,VYTO-YM)
        else
          if(IFLAG2.EQ.2) NUMPT=3
          call DSMOVE(VXFROM,VYFROM)
          call DSDRAW(VXTO,VYTO)
        endif
C
C                        DRAW ARROWHEAD
C
        call DSLTYP(1)
        if(IFLAG2.EQ.0)then
          call DSFILL(X,Y,3,TX,TY)
        else
          call DSMOVE(X(1),Y(1))
          DO 100 J=2,NUMPT
            call DSDRAW(X(J),Y(J))
 100      continue
        endif
        call DSLTYP(KZLSTY)
C
C                        IF SECOND ARROWHEAD DESIRED,
C                        LOCATE VERTICES
C
        if(IFLAG1.EQ.2.OR.IFLAG1.EQ.3)then
          if(IFLAG1.EQ.2)then
            XX=XP
            YY=YP
            XTIP=VXFROM
            YTIP=VYFROM
          else
            XX=VXFROM
            YY=VYFROM
            XTIP=XP
            YTIP=YP
          endif
          X(1)=XX+FX2
          X(2)=XTIP
          X(3)=XX+FX3
          X(4)=X(1)
          Y(1)=YY+FY2
          Y(2)=YTIP
          Y(3)=YY+FY3
          Y(4)=Y(1)
C
C                        DRAW SECOND ARROWHEAD
C
          call DSLTYP(1)
          if(IFLAG2.EQ.0)then
            call DSFILL(X,Y,3,TX,TY)
          else
            call DSMOVE(X(1),Y(1))
            DO 200 J=2,NUMPT
              call DSDRAW(X(J),Y(J))
 200        continue
          endif
          call DSLTYP(KZLSTY)
        endif
C
C                        IF VECTOR ONLY, DRAW VECTOR
C
      else
        call DSMOVE(VXFROM,VYFROM)
        call DSDRAW(VXTO,VYTO)
      endif
      return
      end
      FUNCTION ZXMESS(LMESS,IMESS,CFLAG,CFONT,CSTYLE)
C
C
C
C
C           WRITE GRAPHIC TEXT STRING IN MIXED ALPHABETS
C
C      INPUT:      LMESS      INPUT STRING
C            IMESS      NUMBER OF characterS IN LMESS
C            CFLAG      FONT INDICATORS
C            CFONT      FONT SETTING
C            CSTYLE      character STYLE
C
      common /CSTRNG/ ZZHITE,UUHITE,ZZANGL,KZSTRM,KZTMLN
C
      integer LMESS1(40)
      integer LMESS(*)
      integer KFONT(6)
      character*1 CH(160),BUF(160),CFLAG(6)
      character*5 CFONT(6),CSTYLE
      LOGICAL FINIS,FIRST
C
C                        MIX ALPHABET NOT ACTIVE, WRITE STRING
C
      if(CFLAG(1).EQ.' ')then
        call ZGETFN(CFONT(1),CSTYLE,KFONT(1))
        call GSFONT(KFONT(1),IERR)
        IMESS1=160
        call ZCOSTR(LMESS,IMESS,LMESS1,IMESS1)
        ZXMESS=DSLENS(LMESS1)
C
C                        PROCESS USING FLAGS SET BY MIX
C                        ALPHABET ROUTINES
C
      else
        DO 100 I=1,6
          call ZGETFN(CFONT(I),CSTYLE,KFONT(I))
 100    continue
        call ZIN2CH(LMESS,CH)
        IPTR=1
        ICUR=1
        NBUF=0
        FIRST=.TRUE.
        ZXMESS=0.0
        call GSFONT(KFONT(1),IERR)
        DO 200 I=1,IMESS
C
C                        LOOK FOR STRING TERMINATOR
C
          if(IMESS.EQ.100)then
            FINIS=.TRUE.
            DO 500 J=1,KZTMLN
              call SYBYT4('X',KZSTRM,J,KHAR)
              FINIS=FINIS.AND.(ICHAR(CH(I+J-1)).EQ.KHAR)
 500        continue
C
C                        IF FOUND, EMPTY BUFFER AND return
C
            if(IPTR.EQ.1.AND.FINIS)then
                if(NBUF.GT.0)then
                BUF(NBUF+1)=CHAR(0)
                call ZCH2IN(BUF,LMESS1)
                ZXMESS=ZXMESS+DSLENS(LMESS1)
                endif
              return
            endif
          endif
C
C                        LOOK FOR MIXALF FLAGS
C
 300      continue
          if(CH(I).EQ.CFLAG(ICUR).AND.CH(I).NE.' '.AND.
     *          (ICUR.NE.IPTR.OR.FIRST))then
            if(NBUF.GT.0)then
              BUF(NBUF+1)=CHAR(0)
              call ZCH2IN(BUF,LMESS1)
              ZXMESS=ZXMESS+DSLENS(LMESS1)
              NBUF=0
            endif
            call GSFONT(KFONT(ICUR),IERR)
            FIRST=.FALSE.
            IPTR=ICUR
            ICUR=ICUR+1
            if(ICUR.GT.6) ICUR=1
            go to 200
          else
            ICUR=ICUR+1
            if(ICUR.GT.6) ICUR=1
            if(ICUR.NE.IPTR) go to 300
C
C                        SAVE TEXT IN BUFFER
C
            NBUF=NBUF+1
            BUF(NBUF)=CH(I)
          endif
 200    continue
C
C                        end WHILE
C
        if(NBUF.NE.0)then
          BUF(NBUF+1)=CHAR(0)
          call ZCH2IN(BUF,LMESS1)
          ZXMESS=ZXMESS+DSLENS(LMESS1)
        endif
      endif
      return
      end
      BLOCK DATA INIDAT
C
C  Data initialization
c
      parameter (KZNO=222)
      parameter (KZYES=111)
      parameter (KZDOWN=13)
C-------------------------BEGIN 32-BIT SPECIFIC CODE----------------------
      parameter (NUMBYT=4)
C--------------------------END 32-BIT SPECIFIC CODE-----------------------
C-------------------------BEGIN 64-BIT SPECIFIC CODE----------------------
C     parameter (NUMBYT=8)
C--------------------------END 64-BIT SPECIFIC CODE-----------------------
      character CZALFL*1,CZALFN*5,CZALFS*5,CZLGAC*1,
     *          CZLGAF*5,CZLGAS*5,CZLGTC*1,CZLGTF*5,CZLGTS*5,
     *          CZHDAC*1,CZHDAF*5,CZHDAS*5
      common /CAREA/  ZZXAXS,ZZYAXS,ZZXAXR,ZZYAXR,UUXAXS,UUYAXS,
     *                ZZXLFT,ZZXRGT,ZZYBOT,ZZYTOP,KZSCAL,
     *                ZZGRCE,UUGRCE,ZZFRME,UUFRME
      common /CDEVIC/ KZERR,KZSUM,KZCOPY,KZBYTE,KZNPLT
      common /CHEADN/ KZHDNL,KZHDMX,KZHDTX(72),ZZHDSZ(4)
      common /CHEADC/ CZHDAC(4,6),CZHDAF(4,6),CZHDAS(4)
      common /CIUNIT/ KZIUNT,KZOUNT
      common /CLGNDN/ KZLGLN,KZLGCN,KZLGER,KZLGSM(50),KZLGLT(50),
     *                KZLGTI(5),KZLGTL,ZZLGTH(50),ZZLGTZ,UULGTZ,
     *                ZZLGTR,ZZLGYL,KZLGBL,ZZSRAT,KZLGEN(50)
      common /CLGNDC/ CZLGAC(50,6),CZLGAF(50,6),CZLGAS(50),CZLGTC(6),
     *                CZLGTF(6),CZLGTS
      common /CMXALF/ CZALFL(6),CZALFN(6),CZALFS
      common /CPAGE/  ZZPAGX,ZZPAGY,ZZPAGR,UUPAGX,UUPAGY,KZPAGE,KZAUTO
C
      data KZBYTE,KZNPLT,KZPAGE,KZSCAL /NUMBYT,0,KZNO,KZDOWN/
      data KZIUNT,KZOUNT /5,6/
      data KZLGSM,KZLGLT/50*-1,50*0/
      data CZLGTC,CZLGTF,CZLGTS /6*' ','STAND',5*'     ',
     *                             'DEFAU'/
      data CZLGAC,CZLGAF,CZLGAS /300*' ',350*'     '/
      data KZLGEN /50*KZYES/
      data CZHDAC,CZHDAF,CZHDAS /24*' ',28*'     '/
      data CZALFL,CZALFN,CZALFS /6*' ','STAND',5*'     ','DEFAU'/
      end
      BLOCK DATA BLKSYS
C
C                        INIT character MASK FOR 'ZBYTE4'
C
C     - return MASK FOR 'ZBYTE4'
C          callING SEQUENCE: CALL SYMASK(MASK,NPOS)
C          INPUT: MASK - MASK TO ZERO OUT BYTE OF INTEREST
C                 NPOS - POSITION OF BYTE OF INTEREST
C          THIS ROUTINE PROVIDES COMMUNICATION BETWEEN integer
C          AND character. DATA RETURNED IS TESTED GOOD FOR
C          VAX/VMS AND IBM PC.
      common /CBYTE4/ KZMASK(8),KZCPOS(8)
C4x9BITSREVERSE      data KZMASK/33357439,17045716607,
C4x9BITSREVERSE     .       17078943871,17079008768,0,0,0,0/
C4x9BITSREVERSE      data KZCPOS/134217728,262144,512,1,0,0,0,0/
C
C-------------------------BEGIN VAX VMS SPECIFIC CODE---------------------
C     DATA KZMASK/2139062016,2139029631,2130739071,8355711,4*0/
C     DATA KZCPOS/1,256,65536,16777216,4*0/
C--------------------------END VAX VMS SPECIFIC CODE----------------------
C--------------------------BEGIN CRAY SPECIFIC CODE-----------------------
C     DATA KZMASK/1777777777777777777400B,1777777777777777600377B,
C    .            1777777777777700177777B,1777777777740077777777B,
C    .            1777777760037777777777B,1777770017777777777777B,
C    .            1774007777777777777777B,0003777777777777777777B/
C     DATA KZCPOS/8*0/
C---------------------------END CRAY SPECIFIC CODE------------------------
C-----------------------------BEGIN GENERIC CODE--------------------------
      data KZMASK/2139062016,2139029631,2130739071,8355711,4*0/
      data KZCPOS/1,256,65536,16777216,4*0/
C------------------------------END GENERIC CODE---------------------------
      end
      BLOCK DATA BLKDAT
C
C     THIS BLOCKDATA ROUTINE INITIALIZES THE NAMED COMMON BLOCKS
C
      integer maxfnt
      parameter (MAXFNT = 18)
c
      integer mxstrk
      parameter (MXSTRK = MAXFNT*9000)
c
      integer numz
      parameter (NUMZ = MAXFNT-1)
C
      integer BWIDTH
      integer BXY
c
      save gccpar
      save gcfont
c
      common /GCFONT/ ICFNSL, MXSLOT,
     1   ISLFNT(MAXFNT), IHIGHT(MAXFNT),
     2   INDX(95*MAXFNT+1), BWIDTH(95*MAXFNT), BXY(MXSTRK)
      common /GCCPAR/ CSIZE, CCOS, CSIN
c
      data ICFNSL /1/
      data MXSLOT /1/
      data ISLFNT /1,NUMZ*0/
      data IHIGHT(1) /8/
C
       data INDX(  1),INDX(  2),INDX(  3),INDX(  4)/   1,  17,  27,  47/
       data INDX(  5),INDX(  6),INDX(  7),INDX(  8)/  73, 100, 125, 138/
       data INDX(  9),INDX( 10),INDX( 11),INDX( 12)/ 147, 156, 171, 181/
       data INDX( 13),INDX( 14),INDX( 15),INDX( 16)/ 194, 199, 210, 215/
       data INDX( 17),INDX( 18),INDX( 19),INDX( 20)/ 236, 248, 269, 297/
       data INDX( 21),INDX( 22),INDX( 23),INDX( 24)/ 306, 325, 346, 359/
       data INDX( 25),INDX( 26),INDX( 27),INDX( 28)/ 395, 416, 438, 462/
       data INDX( 29),INDX( 30),INDX( 31),INDX( 32)/ 469, 479, 486, 514/
       data INDX( 33),INDX( 34),INDX( 35),INDX( 36)/ 543, 560, 587, 604/
       data INDX( 37),INDX( 38),INDX( 39),INDX( 40)/ 621, 635, 646, 667/
       data INDX( 41),INDX( 42),INDX( 43),INDX( 44)/ 681, 696, 712, 725/
       data INDX( 45),INDX( 46),INDX( 47),INDX( 48)/ 732, 747, 758, 777/
       data INDX( 49),INDX( 50),INDX( 51),INDX( 52)/ 791, 815, 834, 859/
       data INDX( 53),INDX( 54),INDX( 55),INDX( 56)/ 869, 882, 893, 909/
       data INDX( 57),INDX( 58),INDX( 59),INDX( 60)/ 926, 942, 955, 964/
       data INDX( 61),INDX( 62),INDX( 63),INDX( 64)/ 969, 978, 985, 990/
       data INDX( 65),INDX( 66),INDX( 67),INDX( 68)/1003,1029,1050,1067/
       data INDX( 69),INDX( 70),INDX( 71),INDX( 72)/1089,1108,1124,1152/
       data INDX( 73),INDX( 74),INDX( 75),INDX( 76)/1167,1190,1214,1228/
       data INDX( 77),INDX( 78),INDX( 79),INDX( 80)/1240,1261,1276,1295/
       data INDX( 81),INDX( 82),INDX( 83),INDX( 84)/1317,1339,1352,1375/
       data INDX( 85),INDX( 86),INDX( 87),INDX( 88)/1391,1407,1418,1442/
       data INDX( 89),INDX( 90),INDX( 91),INDX( 92)/1451,1473,1482,1501/
       data INDX( 93),INDX( 94),INDX( 95),INDX( 96)/1506,1525,1538,1555/
       data BXY(   1),BXY(   2),BXY(   3),BXY(   4)/-64,  3,  8,  3/
       data BXY(   5),BXY(   6),BXY(   7),BXY(   8)/  3,-64,  3,  0/
       data BXY(   9),BXY(  10),BXY(  11),BXY(  12)/  2,  0,  2,  1/
       data BXY(  13),BXY(  14),BXY(  15),BXY(  16)/  3,  1,  3,  0/
       data BXY(  17),BXY(  18),BXY(  19),BXY(  20)/-64,  2,  8,  2/
       data BXY(  21),BXY(  22),BXY(  23),BXY(  24)/  6,-64,  4,  8/
       data BXY(  25),BXY(  26),BXY(  27),BXY(  28)/  4,  6,-64,  2/
       data BXY(  29),BXY(  30),BXY(  31),BXY(  32)/  8,  2,  0,-64/
       data BXY(  33),BXY(  34),BXY(  35),BXY(  36)/  4,  8,  4,  0/
       data BXY(  37),BXY(  38),BXY(  39),BXY(  40)/-64,  6,  5,  0/
       data BXY(  41),BXY(  42),BXY(  43),BXY(  44)/  5,-64,  0,  3/
       data BXY(  45),BXY(  46),BXY(  47),BXY(  48)/  6,  3,-64,  6/
       data BXY(  49),BXY(  50),BXY(  51),BXY(  52)/  7,  1,  7,  0/
       data BXY(  53),BXY(  54),BXY(  55),BXY(  56)/  6,  0,  5,  1/
       data BXY(  57),BXY(  58),BXY(  59),BXY(  60)/  4,  5,  4,  6/
       data BXY(  61),BXY(  62),BXY(  63),BXY(  64)/  3,  6,  2,  5/
       data BXY(  65),BXY(  66),BXY(  67),BXY(  68)/  1,  0,  1,-64/
       data BXY(  69),BXY(  70),BXY(  71),BXY(  72)/  3,  8,  3,  0/
       data BXY(  73),BXY(  74),BXY(  75),BXY(  76)/-64,  1,  8,  0/
       data BXY(  77),BXY(  78),BXY(  79),BXY(  80)/  7,  1,  6,  2/
       data BXY(  81),BXY(  82),BXY(  83),BXY(  84)/  7,  1,  8,-64/
       data BXY(  85),BXY(  86),BXY(  87),BXY(  88)/  6,  7,  0,  1/
       data BXY(  89),BXY(  90),BXY(  91),BXY(  92)/-64,  5,  2,  4/
       data BXY(  93),BXY(  94),BXY(  95),BXY(  96)/  1,  5,  0,  6/
       data BXY(  97),BXY(  98),BXY(  99),BXY( 100)/  1,  5,  2,-64/
       data BXY( 101),BXY( 102),BXY( 103),BXY( 104)/  6,  3,  3,  0/
       data BXY( 105),BXY( 106),BXY( 107),BXY( 108)/  1,  0,  0,  1/
       data BXY( 109),BXY( 110),BXY( 111),BXY( 112)/  0,  2,  4,  6/
       data BXY( 113),BXY( 114),BXY( 115),BXY( 116)/  4,  7,  3,  8/
       data BXY( 117),BXY( 118),BXY( 119),BXY( 120)/  1,  8,  0,  7/
       data BXY( 121),BXY( 122),BXY( 123),BXY( 124)/  0,  6,  6,  0/
       data BXY( 125),BXY( 126),BXY( 127),BXY( 128)/-64,  4,  7,  3/
       data BXY( 129),BXY( 130),BXY( 131),BXY( 132)/  7,  3,  8,  4/
       data BXY( 133),BXY( 134),BXY( 135),BXY( 136)/  8,  4,  7,  2/
       data BXY( 137),BXY( 138),BXY( 139),BXY( 140)/  5,-64,  4,  8/
       data BXY( 141),BXY( 142),BXY( 143),BXY( 144)/  2,  6,  2,  2/
       data BXY( 145),BXY( 146),BXY( 147),BXY( 148)/  4,  0,-64,  2/
       data BXY( 149),BXY( 150),BXY( 151),BXY( 152)/  8,  4,  6,  4/
       data BXY( 153),BXY( 154),BXY( 155),BXY( 156)/  2,  2,  0,-64/
       data BXY( 157),BXY( 158),BXY( 159),BXY( 160)/  1,  2,  5,  6/
       data BXY( 161),BXY( 162),BXY( 163),BXY( 164)/-64,  3,  7,  3/
       data BXY( 165),BXY( 166),BXY( 167),BXY( 168)/  1,-64,  1,  6/
       data BXY( 169),BXY( 170),BXY( 171),BXY( 172)/  5,  2,-64,  3/
       data BXY( 173),BXY( 174),BXY( 175),BXY( 176)/  7,  3,  1,-64/
       data BXY( 177),BXY( 178),BXY( 179),BXY( 180)/  0,  4,  6,  4/
       data BXY( 181),BXY( 182),BXY( 183),BXY( 184)/-64,  3,  0,  2/
       data BXY( 185),BXY( 186),BXY( 187),BXY( 188)/  0,  2,  1,  3/
       data BXY( 189),BXY( 190),BXY( 191),BXY( 192)/  1,  3,  0,  1/
       data BXY( 193),BXY( 194),BXY( 195),BXY( 196)/ -2,-64,  0,  4/
       data BXY( 197),BXY( 198),BXY( 199),BXY( 200)/  6,  4,-64,  3/
       data BXY( 201),BXY( 202),BXY( 203),BXY( 204)/  0,  2,  0,  2/
       data BXY( 205),BXY( 206),BXY( 207),BXY( 208)/  1,  3,  1,  3/
       data BXY( 209),BXY( 210),BXY( 211),BXY( 212)/  0,-64,  0,  1/
       data BXY( 213),BXY( 214),BXY( 215),BXY( 216)/  6,  7,-64,  6/
       data BXY( 217),BXY( 218),BXY( 219),BXY( 220)/  7,  6,  1,  5/
       data BXY( 221),BXY( 222),BXY( 223),BXY( 224)/  0,  1,  0,  0/
       data BXY( 225),BXY( 226),BXY( 227),BXY( 228)/  1,  0,  7,  1/
       data BXY( 229),BXY( 230),BXY( 231),BXY( 232)/  8,  5,  8,  6/
       data BXY( 233),BXY( 234),BXY( 235),BXY( 236)/  7,  0,  1,-64/
       data BXY( 237),BXY( 238),BXY( 239),BXY( 240)/  1,  6,  3,  8/
       data BXY( 241),BXY( 242),BXY( 243),BXY( 244)/  3,  0,-64,  1/
       data BXY( 245),BXY( 246),BXY( 247),BXY( 248)/  0,  5,  0,-64/
       data BXY( 249),BXY( 250),BXY( 251),BXY( 252)/  0,  7,  1,  8/
       data BXY( 253),BXY( 254),BXY( 255),BXY( 256)/  5,  8,  6,  7/
       data BXY( 257),BXY( 258),BXY( 259),BXY( 260)/  6,  6,  4,  4/
       data BXY( 261),BXY( 262),BXY( 263),BXY( 264)/  2,  4,  0,  2/
       data BXY( 265),BXY( 266),BXY( 267),BXY( 268)/  0,  0,  6,  0/
       data BXY( 269),BXY( 270),BXY( 271),BXY( 272)/-64,  0,  7,  1/
       data BXY( 273),BXY( 274),BXY( 275),BXY( 276)/  8,  5,  8,  6/
       data BXY( 277),BXY( 278),BXY( 279),BXY( 280)/  7,  6,  5,  5/
       data BXY( 281),BXY( 282),BXY( 283),BXY( 284)/  4,  1,  4,-64/
       data BXY( 285),BXY( 286),BXY( 287),BXY( 288)/  5,  4,  6,  3/
       data BXY( 289),BXY( 290),BXY( 291),BXY( 292)/  6,  1,  5,  0/
       data BXY( 293),BXY( 294),BXY( 295),BXY( 296)/  1,  0,  0,  1/
       data BXY( 297),BXY( 298),BXY( 299),BXY( 300)/-64,  5,  0,  5/
       data BXY( 301),BXY( 302),BXY( 303),BXY( 304)/  8,  0,  3,  6/
       data BXY( 305),BXY( 306),BXY( 307),BXY( 308)/  3,-64,  0,  1/
       data BXY( 309),BXY( 310),BXY( 311),BXY( 312)/  1,  0,  4,  0/
       data BXY( 313),BXY( 314),BXY( 315),BXY( 316)/  6,  2,  6,  3/
       data BXY( 317),BXY( 318),BXY( 319),BXY( 320)/  4,  5,  0,  5/
       data BXY( 321),BXY( 322),BXY( 323),BXY( 324)/  0,  8,  6,  8/
       data BXY( 325),BXY( 326),BXY( 327),BXY( 328)/-64,  5,  8,  2/
       data BXY( 329),BXY( 330),BXY( 331),BXY( 332)/  8,  0,  6,  0/
       data BXY( 333),BXY( 334),BXY( 335),BXY( 336)/  1,  1,  0,  5/
       data BXY( 337),BXY( 338),BXY( 339),BXY( 340)/  0,  6,  1,  6/
       data BXY( 341),BXY( 342),BXY( 343),BXY( 344)/  3,  5,  4,  0/
       data BXY( 345),BXY( 346),BXY( 347),BXY( 348)/  4,-64,  0,  7/
       data BXY( 349),BXY( 350),BXY( 351),BXY( 352)/  0,  8,  6,  8/
       data BXY( 353),BXY( 354),BXY( 355),BXY( 356)/  6,  7,  2,  3/
       data BXY( 357),BXY( 358),BXY( 359),BXY( 360)/  2,  0,-64,  6/
       data BXY( 361),BXY( 362),BXY( 363),BXY( 364)/  7,  5,  8,  1/
       data BXY( 365),BXY( 366),BXY( 367),BXY( 368)/  8,  0,  7,  0/
       data BXY( 369),BXY( 370),BXY( 371),BXY( 372)/  5,  1,  4,  5/
       data BXY( 373),BXY( 374),BXY( 375),BXY( 376)/  4,  6,  3,  6/
       data BXY( 377),BXY( 378),BXY( 379),BXY( 380)/  1,  5,  0,  1/
       data BXY( 381),BXY( 382),BXY( 383),BXY( 384)/  0,  0,  1,  0/
       data BXY( 385),BXY( 386),BXY( 387),BXY( 388)/  3,  1,  4,-64/
       data BXY( 389),BXY( 390),BXY( 391),BXY( 392)/  5,  4,  6,  5/
       data BXY( 393),BXY( 394),BXY( 395),BXY( 396)/  6,  7,-64,  1/
       data BXY( 397),BXY( 398),BXY( 399),BXY( 400)/  0,  4,  0,  6/
       data BXY( 401),BXY( 402),BXY( 403),BXY( 404)/  2,  6,  7,  5/
       data BXY( 405),BXY( 406),BXY( 407),BXY( 408)/  8,  1,  8,  0/
       data BXY( 409),BXY( 410),BXY( 411),BXY( 412)/  7,  0,  5,  1/
       data BXY( 413),BXY( 414),BXY( 415),BXY( 416)/  4,  6,  4,-64/
       data BXY( 417),BXY( 418),BXY( 419),BXY( 420)/  3,  4,  2,  4/
       data BXY( 421),BXY( 422),BXY( 423),BXY( 424)/  2,  5,  3,  5/
       data BXY( 425),BXY( 426),BXY( 427),BXY( 428)/  3,  4,-64,  3/
       data BXY( 429),BXY( 430),BXY( 431),BXY( 432)/  0,  2,  0,  2/
       data BXY( 433),BXY( 434),BXY( 435),BXY( 436)/  1,  3,  1,  3/
       data BXY( 437),BXY( 438),BXY( 439),BXY( 440)/  0,-64,  3,  4/
       data BXY( 441),BXY( 442),BXY( 443),BXY( 444)/  2,  4,  2,  5/
       data BXY( 445),BXY( 446),BXY( 447),BXY( 448)/  3,  5,  3,  4/
       data BXY( 449),BXY( 450),BXY( 451),BXY( 452)/-64,  3,  0,  2/
       data BXY( 453),BXY( 454),BXY( 455),BXY( 456)/  0,  2,  1,  3/
       data BXY( 457),BXY( 458),BXY( 459),BXY( 460)/  1,  3,  0,  1/
       data BXY( 461),BXY( 462),BXY( 463),BXY( 464)/ -2,-64,  4,  8/
       data BXY( 465),BXY( 466),BXY( 467),BXY( 468)/  0,  4,  4,  0/
       data BXY( 469),BXY( 470),BXY( 471),BXY( 472)/-64,  5,  5,  1/
       data BXY( 473),BXY( 474),BXY( 475),BXY( 476)/  5,-64,  1,  3/
       data BXY( 477),BXY( 478),BXY( 479),BXY( 480)/  5,  3,-64,  2/
       data BXY( 481),BXY( 482),BXY( 483),BXY( 484)/  8,  6,  4,  2/
       data BXY( 485),BXY( 486),BXY( 487),BXY( 488)/  0,-64,  0,  7/
       data BXY( 489),BXY( 490),BXY( 491),BXY( 492)/  1,  8,  5,  8/
       data BXY( 493),BXY( 494),BXY( 495),BXY( 496)/  6,  7,  6,  5/
       data BXY( 497),BXY( 498),BXY( 499),BXY( 500)/  5,  4,  3,  4/
       data BXY( 501),BXY( 502),BXY( 503),BXY( 504)/  3,  3,-64,  3/
       data BXY( 505),BXY( 506),BXY( 507),BXY( 508)/  0,  2,  0,  2/
       data BXY( 509),BXY( 510),BXY( 511),BXY( 512)/  1,  3,  1,  3/
       data BXY( 513),BXY( 514),BXY( 515),BXY( 516)/  0,-64,  4,  3/
       data BXY( 517),BXY( 518),BXY( 519),BXY( 520)/  4,  5,  3,  5/
       data BXY( 521),BXY( 522),BXY( 523),BXY( 524)/  2,  4,  2,  3/
       data BXY( 525),BXY( 526),BXY( 527),BXY( 528)/  5,  3,  6,  4/
       data BXY( 529),BXY( 530),BXY( 531),BXY( 532)/  6,  7,  5,  8/
       data BXY( 533),BXY( 534),BXY( 535),BXY( 536)/  2,  8,  0,  6/
       data BXY( 537),BXY( 538),BXY( 539),BXY( 540)/  0,  2,  2,  0/
       data BXY( 541),BXY( 542),BXY( 543),BXY( 544)/  5,  0,  0,  0/
       data BXY( 545),BXY( 546),BXY( 547),BXY( 548)/  0,  6,  2,  8/
       data BXY( 549),BXY( 550),BXY( 551),BXY( 552)/  4,  8,  6,  6/
       data BXY( 553),BXY( 554),BXY( 555),BXY( 556)/  6,  0,-64,  0/
       data BXY( 557),BXY( 558),BXY( 559),BXY( 560)/  3,  6,  3,  0/
       data BXY( 561),BXY( 562),BXY( 563),BXY( 564)/  0,  5,  0,  6/
       data BXY( 565),BXY( 566),BXY( 567),BXY( 568)/  1,  6,  3,  5/
       data BXY( 569),BXY( 570),BXY( 571),BXY( 572)/  4,  1,  4,-64/
       data BXY( 573),BXY( 574),BXY( 575),BXY( 576)/  5,  4,  6,  5/
       data BXY( 577),BXY( 578),BXY( 579),BXY( 580)/  6,  7,  5,  8/
       data BXY( 581),BXY( 582),BXY( 583),BXY( 584)/  0,  8,  1,  8/
       data BXY( 585),BXY( 586),BXY( 587),BXY( 588)/  1,  0,-64,  6/
       data BXY( 589),BXY( 590),BXY( 591),BXY( 592)/  7,  5,  8,  2/
       data BXY( 593),BXY( 594),BXY( 595),BXY( 596)/  8,  0,  6,  0/
       data BXY( 597),BXY( 598),BXY( 599),BXY( 600)/  2,  2,  0,  5/
       data BXY( 601),BXY( 602),BXY( 603),BXY( 604)/  0,  6,  1,  0/
       data BXY( 605),BXY( 606),BXY( 607),BXY( 608)/  0,  4,  0,  6/
       data BXY( 609),BXY( 610),BXY( 611),BXY( 612)/  2,  6,  6,  4/
       data BXY( 613),BXY( 614),BXY( 615),BXY( 616)/  8,  0,  8,-64/
       data BXY( 617),BXY( 618),BXY( 619),BXY( 620)/  1,  8,  1,  0/
       data BXY( 621),BXY( 622),BXY( 623),BXY( 624)/-64,  6,  0,  0/
       data BXY( 625),BXY( 626),BXY( 627),BXY( 628)/  0,  0,  8,  6/
       data BXY( 629),BXY( 630),BXY( 631),BXY( 632)/  8,-64,  3,  4/
       data BXY( 633),BXY( 634),BXY( 635),BXY( 636)/  0,  4,  0,  0/
       data BXY( 637),BXY( 638),BXY( 639),BXY( 640)/  0,  8,  6,  8/
       data BXY( 641),BXY( 642),BXY( 643),BXY( 644)/-64,  3,  4,  0/
       data BXY( 645),BXY( 646),BXY( 647),BXY( 648)/  4,-64,  6,  7/
       data BXY( 649),BXY( 650),BXY( 651),BXY( 652)/  5,  8,  2,  8/
       data BXY( 653),BXY( 654),BXY( 655),BXY( 656)/  0,  6,  0,  2/
       data BXY( 657),BXY( 658),BXY( 659),BXY( 660)/  2,  0,  5,  0/
       data BXY( 661),BXY( 662),BXY( 663),BXY( 664)/  6,  1,  6,  3/
       data BXY( 665),BXY( 666),BXY( 667),BXY( 668)/  3,  3,  0,  0/
       data BXY( 669),BXY( 670),BXY( 671),BXY( 672)/  0,  8,-64,  0/
       data BXY( 673),BXY( 674),BXY( 675),BXY( 676)/  4,  6,  4,-64/
       data BXY( 677),BXY( 678),BXY( 679),BXY( 680)/  6,  8,  6,  0/
       data BXY( 681),BXY( 682),BXY( 683),BXY( 684)/-64,  1,  0,  5/
       data BXY( 685),BXY( 686),BXY( 687),BXY( 688)/  0,-64,  3,  0/
       data BXY( 689),BXY( 690),BXY( 691),BXY( 692)/  3,  8,-64,  1/
       data BXY( 693),BXY( 694),BXY( 695),BXY( 696)/  8,  5,  8,-64/
       data BXY( 697),BXY( 698),BXY( 699),BXY( 700)/  0,  1,  1,  0/
       data BXY( 701),BXY( 702),BXY( 703),BXY( 704)/  3,  0,  4,  1/
       data BXY( 705),BXY( 706),BXY( 707),BXY( 708)/  4,  8,-64,  2/
       data BXY( 709),BXY( 710),BXY( 711),BXY( 712)/  8,  6,  8,  0/
       data BXY( 713),BXY( 714),BXY( 715),BXY( 716)/  0,  0,  8,-64/
       data BXY( 717),BXY( 718),BXY( 719),BXY( 720)/  6,  8,  0,  2/
       data BXY( 721),BXY( 722),BXY( 723),BXY( 724)/  2,  4,  6,  0/
       data BXY( 725),BXY( 726),BXY( 727),BXY( 728)/-64,  0,  8,  0/
       data BXY( 729),BXY( 730),BXY( 731),BXY( 732)/  0,  6,  0,  0/
       data BXY( 733),BXY( 734),BXY( 735),BXY( 736)/  0,  0,  8,  3/
       data BXY( 737),BXY( 738),BXY( 739),BXY( 740)/  5,  3,  4,-64/
       data BXY( 741),BXY( 742),BXY( 743),BXY( 744)/  3,  5,  6,  8/
       data BXY( 745),BXY( 746),BXY( 747),BXY( 748)/  6,  0,  0,  0/
       data BXY( 749),BXY( 750),BXY( 751),BXY( 752)/  0,  8,  6,  2/
       data BXY( 753),BXY( 754),BXY( 755),BXY( 756)/-64,  6,  8,  6/
       data BXY( 757),BXY( 758),BXY( 759),BXY( 760)/  0,-64,  6,  2/
       data BXY( 761),BXY( 762),BXY( 763),BXY( 764)/  6,  6,  4,  8/
       data BXY( 765),BXY( 766),BXY( 767),BXY( 768)/  2,  8,  0,  6/
       data BXY( 769),BXY( 770),BXY( 771),BXY( 772)/  0,  2,  2,  0/
       data BXY( 773),BXY( 774),BXY( 775),BXY( 776)/  4,  0,  6,  2/
       data BXY( 777),BXY( 778),BXY( 779),BXY( 780)/  0,  0,  0,  8/
       data BXY( 781),BXY( 782),BXY( 783),BXY( 784)/  5,  8,  6,  7/
       data BXY( 785),BXY( 786),BXY( 787),BXY( 788)/  6,  5,  5,  4/
       data BXY( 789),BXY( 790),BXY( 791),BXY( 792)/  0,  4,-64,  6/
       data BXY( 793),BXY( 794),BXY( 795),BXY( 796)/  2,  6,  6,  4/
       data BXY( 797),BXY( 798),BXY( 799),BXY( 800)/  8,  2,  8,  0/
       data BXY( 801),BXY( 802),BXY( 803),BXY( 804)/  6,  0,  2,  2/
       data BXY( 805),BXY( 806),BXY( 807),BXY( 808)/  0,  4,  0,  6/
       data BXY( 809),BXY( 810),BXY( 811),BXY( 812)/  2,-64,  3,  3/
       data BXY( 813),BXY( 814),BXY( 815),BXY( 816)/  6,  0,  0,  0/
       data BXY( 817),BXY( 818),BXY( 819),BXY( 820)/  0,  8,  5,  8/
       data BXY( 821),BXY( 822),BXY( 823),BXY( 824)/  6,  7,  6,  5/
       data BXY( 825),BXY( 826),BXY( 827),BXY( 828)/  5,  4,  0,  4/
       data BXY( 829),BXY( 830),BXY( 831),BXY( 832)/-64,  2,  4,  6/
       data BXY( 833),BXY( 834),BXY( 835),BXY( 836)/  0,-64,  6,  7/
       data BXY( 837),BXY( 838),BXY( 839),BXY( 840)/  5,  8,  1,  8/
       data BXY( 841),BXY( 842),BXY( 843),BXY( 844)/  0,  7,  0,  5/
       data BXY( 845),BXY( 846),BXY( 847),BXY( 848)/  1,  4,  5,  4/
       data BXY( 849),BXY( 850),BXY( 851),BXY( 852)/  6,  3,  6,  1/
       data BXY( 853),BXY( 854),BXY( 855),BXY( 856)/  5,  0,  1,  0/
       data BXY( 857),BXY( 858),BXY( 859),BXY( 860)/  0,  1,-64,  0/
       data BXY( 861),BXY( 862),BXY( 863),BXY( 864)/  8,  6,  8,-64/
       data BXY( 865),BXY( 866),BXY( 867),BXY( 868)/  3,  0,  3,  8/
       data BXY( 869),BXY( 870),BXY( 871),BXY( 872)/-64,  6,  8,  6/
       data BXY( 873),BXY( 874),BXY( 875),BXY( 876)/  1,  5,  0,  1/
       data BXY( 877),BXY( 878),BXY( 879),BXY( 880)/  0,  0,  1,  0/
       data BXY( 881),BXY( 882),BXY( 883),BXY( 884)/  8,-64,  0,  8/
       data BXY( 885),BXY( 886),BXY( 887),BXY( 888)/  0,  6,  3,  0/
       data BXY( 889),BXY( 890),BXY( 891),BXY( 892)/  6,  6,  6,  8/
       data BXY( 893),BXY( 894),BXY( 895),BXY( 896)/-64,  0,  8,  0/
       data BXY( 897),BXY( 898),BXY( 899),BXY( 900)/  0,  3,  3,  3/
       data BXY( 901),BXY( 902),BXY( 903),BXY( 904)/  4,-64,  3,  3/
       data BXY( 905),BXY( 906),BXY( 907),BXY( 908)/  6,  0,  6,  8/
       data BXY( 909),BXY( 910),BXY( 911),BXY( 912)/  0,  0,  0,  1/
       data BXY( 913),BXY( 914),BXY( 915),BXY( 916)/  6,  7,  6,  8/
       data BXY( 917),BXY( 918),BXY( 919),BXY( 920)/-64,  0,  8,  0/
       data BXY( 921),BXY( 922),BXY( 923),BXY( 924)/  7,  6,  1,  6/
       data BXY( 925),BXY( 926),BXY( 927),BXY( 928)/  0,-64,  0,  8/
       data BXY( 929),BXY( 930),BXY( 931),BXY( 932)/  0,  7,  3,  4/
       data BXY( 933),BXY( 934),BXY( 935),BXY( 936)/  6,  7,  6,  8/
       data BXY( 937),BXY( 938),BXY( 939),BXY( 940)/-64,  3,  4,  3/
       data BXY( 941),BXY( 942),BXY( 943),BXY( 944)/  0,-64,  0,  8/
       data BXY( 945),BXY( 946),BXY( 947),BXY( 948)/  6,  8,  6,  7/
       data BXY( 949),BXY( 950),BXY( 951),BXY( 952)/  0,  1,  0,  0/
       data BXY( 953),BXY( 954),BXY( 955),BXY( 956)/  6,  0,-64,  4/
       data BXY( 957),BXY( 958),BXY( 959),BXY( 960)/  8,  2,  8,  2/
       data BXY( 961),BXY( 962),BXY( 963),BXY( 964)/  0,  4,  0,-64/
       data BXY( 965),BXY( 966),BXY( 967),BXY( 968)/  0,  7,  6,  1/
       data BXY( 969),BXY( 970),BXY( 971),BXY( 972)/-64,  3,  8,  5/
       data BXY( 973),BXY( 974),BXY( 975),BXY( 976)/  8,  5,  0,  3/
       data BXY( 977),BXY( 978),BXY( 979),BXY( 980)/  0,-64,  0,  5/
       data BXY( 981),BXY( 982),BXY( 983),BXY( 984)/  3,  8,  6,  5/
       data BXY( 985),BXY( 986),BXY( 987),BXY( 988)/-64,  0, -2,  6/
       data BXY( 989),BXY( 990),BXY( 991),BXY( 992)/ -2,-64,  2,  7/
       data BXY( 993),BXY( 994),BXY( 995),BXY( 996)/  3,  7,  3,  8/
       data BXY( 997),BXY( 998),BXY( 999),BXY(1000)/  2,  8,  2,  7/
       data BXY(1001),BXY(1002),BXY(1003),BXY(1004)/  4,  5,-64,  5/
       data BXY(1005),BXY(1006),BXY(1007),BXY(1008)/  3,  1,  3,  0/
       data BXY(1009),BXY(1010),BXY(1011),BXY(1012)/  2,  0,  1,  1/
       data BXY(1013),BXY(1014),BXY(1015),BXY(1016)/  0,  4,  0,  5/
       data BXY(1017),BXY(1018),BXY(1019),BXY(1020)/  1,-64,  1,  5/
       data BXY(1021),BXY(1022),BXY(1023),BXY(1024)/  4,  5,  5,  4/
       data BXY(1025),BXY(1026),BXY(1027),BXY(1028)/  5,  1,  6,  0/
       data BXY(1029),BXY(1030),BXY(1031),BXY(1032)/  0,  0,  0,  8/
       data BXY(1033),BXY(1034),BXY(1035),BXY(1036)/-64,  0,  3,  2/
       data BXY(1037),BXY(1038),BXY(1039),BXY(1040)/  5,  5,  5,  6/
       data BXY(1041),BXY(1042),BXY(1043),BXY(1044)/  4,  6,  1,  5/
       data BXY(1045),BXY(1046),BXY(1047),BXY(1048)/  0,  2,  0,  0/
       data BXY(1049),BXY(1050),BXY(1051),BXY(1052)/  2,-64,  5,  4/
       data BXY(1053),BXY(1054),BXY(1055),BXY(1056)/  4,  5,  1,  5/
       data BXY(1057),BXY(1058),BXY(1059),BXY(1060)/  0,  4,  0,  1/
       data BXY(1061),BXY(1062),BXY(1063),BXY(1064)/  1,  0,  4,  0/
       data BXY(1065),BXY(1066),BXY(1067),BXY(1068)/  5,  1,-64,  5/
       data BXY(1069),BXY(1070),BXY(1071),BXY(1072)/  8,  5,  0,-64/
       data BXY(1073),BXY(1074),BXY(1075),BXY(1076)/  5,  2,  3,  0/
       data BXY(1077),BXY(1078),BXY(1079),BXY(1080)/  1,  0,  0,  1/
       data BXY(1081),BXY(1082),BXY(1083),BXY(1084)/  0,  4,  1,  5/
       data BXY(1085),BXY(1086),BXY(1087),BXY(1088)/  3,  5,  5,  3/
       data BXY(1089),BXY(1090),BXY(1091),BXY(1092)/-64,  0,  3,  6/
       data BXY(1093),BXY(1094),BXY(1095),BXY(1096)/  3,  6,  4,  5/
       data BXY(1097),BXY(1098),BXY(1099),BXY(1100)/  5,  1,  5,  0/
       data BXY(1101),BXY(1102),BXY(1103),BXY(1104)/  4,  0,  1,  1/
       data BXY(1105),BXY(1106),BXY(1107),BXY(1108)/  0,  5,  0,-64/
       data BXY(1109),BXY(1110),BXY(1111),BXY(1112)/  2,  0,  2,  7/
       data BXY(1113),BXY(1114),BXY(1115),BXY(1116)/  3,  8,  4,  8/
       data BXY(1117),BXY(1118),BXY(1119),BXY(1120)/  5,  7,-64,  0/
       data BXY(1121),BXY(1122),BXY(1123),BXY(1124)/  4,  4,  4,-64/
       data BXY(1125),BXY(1126),BXY(1127),BXY(1128)/  5,  2,  3,  0/
       data BXY(1129),BXY(1130),BXY(1131),BXY(1132)/  1,  0,  0,  1/
       data BXY(1133),BXY(1134),BXY(1135),BXY(1136)/  0,  4,  1,  5/
       data BXY(1137),BXY(1138),BXY(1139),BXY(1140)/  3,  5,  5,  3/
       data BXY(1141),BXY(1142),BXY(1143),BXY(1144)/-64,  5,  5,  5/
       data BXY(1145),BXY(1146),BXY(1147),BXY(1148)/ -3,  4, -4,  1/
       data BXY(1149),BXY(1150),BXY(1151),BXY(1152)/ -4,  0, -3,  0/
       data BXY(1153),BXY(1154),BXY(1155),BXY(1156)/  0,  0,  8,-64/
       data BXY(1157),BXY(1158),BXY(1159),BXY(1160)/  0,  3,  2,  5/
       data BXY(1161),BXY(1162),BXY(1163),BXY(1164)/  5,  5,  6,  4/
       data BXY(1165),BXY(1166),BXY(1167),BXY(1168)/  6,  0,-64,  2/
       data BXY(1169),BXY(1170),BXY(1171),BXY(1172)/  0,  4,  0,-64/
       data BXY(1173),BXY(1174),BXY(1175),BXY(1176)/  3,  0,  3,  5/
       data BXY(1177),BXY(1178),BXY(1179),BXY(1180)/  2,  5,-64,  2/
       data BXY(1181),BXY(1182),BXY(1183),BXY(1184)/  7,  3,  7,  3/
       data BXY(1185),BXY(1186),BXY(1187),BXY(1188)/  8,  2,  8,  2/
       data BXY(1189),BXY(1190),BXY(1191),BXY(1192)/  7,-64,  5,  7/
       data BXY(1193),BXY(1194),BXY(1195),BXY(1196)/  5,  8,  4,  8/
       data BXY(1197),BXY(1198),BXY(1199),BXY(1200)/  4,  7,  5,  7/
       data BXY(1201),BXY(1202),BXY(1203),BXY(1204)/-64,  4,  5,  5/
       data BXY(1205),BXY(1206),BXY(1207),BXY(1208)/  5,  5, -3,  4/
       data BXY(1209),BXY(1210),BXY(1211),BXY(1212)/ -4,  2, -4,  1/
       data BXY(1213),BXY(1214),BXY(1215),BXY(1216)/ -3,  0,  0,  0/
       data BXY(1217),BXY(1218),BXY(1219),BXY(1220)/  8,-64,  4,  5/
       data BXY(1221),BXY(1222),BXY(1223),BXY(1224)/  0,  1,-64,  2/
       data BXY(1225),BXY(1226),BXY(1227),BXY(1228)/  3,  5,  0,-64/
       data BXY(1229),BXY(1230),BXY(1231),BXY(1232)/  2,  0,  4,  0/
       data BXY(1233),BXY(1234),BXY(1235),BXY(1236)/-64,  3,  0,  3/
       data BXY(1237),BXY(1238),BXY(1239),BXY(1240)/  8,  2,  8,  0/
       data BXY(1241),BXY(1242),BXY(1243),BXY(1244)/  0,  0,  5,  2/
       data BXY(1245),BXY(1246),BXY(1247),BXY(1248)/  5,  3,  4,  3/
       data BXY(1249),BXY(1250),BXY(1251),BXY(1252)/  0,-64,  3,  4/
       data BXY(1253),BXY(1254),BXY(1255),BXY(1256)/  4,  5,  5,  5/
       data BXY(1257),BXY(1258),BXY(1259),BXY(1260)/  6,  4,  6,  0/
       data BXY(1261),BXY(1262),BXY(1263),BXY(1264)/  0,  0,  0,  5/
       data BXY(1265),BXY(1266),BXY(1267),BXY(1268)/-64,  0,  3,  2/
       data BXY(1269),BXY(1270),BXY(1271),BXY(1272)/  5,  4,  5,  5/
       data BXY(1273),BXY(1274),BXY(1275),BXY(1276)/  4,  5,  0,-64/
       data BXY(1277),BXY(1278),BXY(1279),BXY(1280)/  5,  4,  4,  5/
       data BXY(1281),BXY(1282),BXY(1283),BXY(1284)/  1,  5,  0,  4/
       data BXY(1285),BXY(1286),BXY(1287),BXY(1288)/  0,  1,  1,  0/
       data BXY(1289),BXY(1290),BXY(1291),BXY(1292)/  4,  0,  5,  1/
       data BXY(1293),BXY(1294),BXY(1295),BXY(1296)/  5,  4,-64,  0/
       data BXY(1297),BXY(1298),BXY(1299),BXY(1300)/ -4,  0,  5,-64/
       data BXY(1301),BXY(1302),BXY(1303),BXY(1304)/  0,  3,  2,  5/
       data BXY(1305),BXY(1306),BXY(1307),BXY(1308)/  4,  5,  5,  4/
       data BXY(1309),BXY(1310),BXY(1311),BXY(1312)/  5,  1,  4,  0/
       data BXY(1313),BXY(1314),BXY(1315),BXY(1316)/  2,  0,  0,  2/
       data BXY(1317),BXY(1318),BXY(1319),BXY(1320)/-64,  5, -4,  5/
       data BXY(1321),BXY(1322),BXY(1323),BXY(1324)/  5,-64,  5,  2/
       data BXY(1325),BXY(1326),BXY(1327),BXY(1328)/  3,  0,  1,  0/
       data BXY(1329),BXY(1330),BXY(1331),BXY(1332)/  0,  1,  0,  4/
       data BXY(1333),BXY(1334),BXY(1335),BXY(1336)/  1,  5,  3,  5/
       data BXY(1337),BXY(1338),BXY(1339),BXY(1340)/  5,  3,  0,  0/
       data BXY(1341),BXY(1342),BXY(1343),BXY(1344)/  0,  5,-64,  0/
       data BXY(1345),BXY(1346),BXY(1347),BXY(1348)/  3,  2,  5,  4/
       data BXY(1349),BXY(1350),BXY(1351),BXY(1352)/  5,  5,  4,-64/
       data BXY(1353),BXY(1354),BXY(1355),BXY(1356)/-64,-64,  0,  1/
       data BXY(1357),BXY(1358),BXY(1359),BXY(1360)/  1,  0,  4,  0/
       data BXY(1361),BXY(1362),BXY(1363),BXY(1364)/  5,  1,  4,  2/
       data BXY(1365),BXY(1366),BXY(1367),BXY(1368)/  1,  3,  0,  4/
       data BXY(1369),BXY(1370),BXY(1371),BXY(1372)/  1,  5,  4,  5/
       data BXY(1373),BXY(1374),BXY(1375),BXY(1376)/  5,  4,-64,  2/
       data BXY(1377),BXY(1378),BXY(1379),BXY(1380)/  8,  2,  1,  3/
       data BXY(1381),BXY(1382),BXY(1383),BXY(1384)/  0,  4,  0,  5/
       data BXY(1385),BXY(1386),BXY(1387),BXY(1388)/  1,-64,  0,  5/
       data BXY(1389),BXY(1390),BXY(1391),BXY(1392)/  4,  5,-64,  0/
       data BXY(1393),BXY(1394),BXY(1395),BXY(1396)/  5,  0,  1,  1/
       data BXY(1397),BXY(1398),BXY(1399),BXY(1400)/  0,  4,  0,  5/
       data BXY(1401),BXY(1402),BXY(1403),BXY(1404)/  1,-64,  5,  5/
       data BXY(1405),BXY(1406),BXY(1407),BXY(1408)/  5,  0,-64,  0/
       data BXY(1409),BXY(1410),BXY(1411),BXY(1412)/  5,  0,  3,  3/
       data BXY(1413),BXY(1414),BXY(1415),BXY(1416)/  0,  6,  3,  6/
       data BXY(1417),BXY(1418),BXY(1419),BXY(1420)/  5,-64,  0,  5/
       data BXY(1421),BXY(1422),BXY(1423),BXY(1424)/  0,  1,  1,  0/
       data BXY(1425),BXY(1426),BXY(1427),BXY(1428)/  2,  0,  3,  1/
       data BXY(1429),BXY(1430),BXY(1431),BXY(1432)/  3,  4,-64,  3/
       data BXY(1433),BXY(1434),BXY(1435),BXY(1436)/  1,  4,  0,  5/
       data BXY(1437),BXY(1438),BXY(1439),BXY(1440)/  0,  6,  1,  6/
       data BXY(1441),BXY(1442),BXY(1443),BXY(1444)/  5,  0,  0,  5/
       data BXY(1445),BXY(1446),BXY(1447),BXY(1448)/  5,-64,  0,  5/
       data BXY(1449),BXY(1450),BXY(1451),BXY(1452)/  5,  0,-64,  0/
       data BXY(1453),BXY(1454),BXY(1455),BXY(1456)/  5,  0,  1,  1/
       data BXY(1457),BXY(1458),BXY(1459),BXY(1460)/  0,  4,  0,  5/
       data BXY(1461),BXY(1462),BXY(1463),BXY(1464)/  1,-64,  5,  5/
       data BXY(1465),BXY(1466),BXY(1467),BXY(1468)/  5, -3,  4, -4/
       data BXY(1469),BXY(1470),BXY(1471),BXY(1472)/  1, -4,  0, -3/
       data BXY(1473),BXY(1474),BXY(1475),BXY(1476)/-64,  0,  5,  5/
       data BXY(1477),BXY(1478),BXY(1479),BXY(1480)/  5,  0,  0,  5/
       data BXY(1481),BXY(1482),BXY(1483),BXY(1484)/  0,-64,  3,  8/
       data BXY(1485),BXY(1486),BXY(1487),BXY(1488)/  2,  8,  1,  7/
       data BXY(1489),BXY(1490),BXY(1491),BXY(1492)/  1,  5,  0,  4/
       data BXY(1493),BXY(1494),BXY(1495),BXY(1496)/  1,  3,  1,  1/
       data BXY(1497),BXY(1498),BXY(1499),BXY(1500)/  2,  0,  3,  0/
       data BXY(1501),BXY(1502),BXY(1503),BXY(1504)/-64,  3,  8,  3/
       data BXY(1505),BXY(1506),BXY(1507),BXY(1508)/  0,-64,  3,  8/
       data BXY(1509),BXY(1510),BXY(1511),BXY(1512)/  4,  8,  5,  7/
       data BXY(1513),BXY(1514),BXY(1515),BXY(1516)/  5,  5,  6,  4/
       data BXY(1517),BXY(1518),BXY(1519),BXY(1520)/  5,  3,  5,  1/
       data BXY(1521),BXY(1522),BXY(1523),BXY(1524)/  4,  0,  3,  0/
       data BXY(1525),BXY(1526),BXY(1527),BXY(1528)/-64,  0,  4,  1/
       data BXY(1529),BXY(1530),BXY(1531),BXY(1532)/  5,  2,  5,  4/
       data BXY(1533),BXY(1534),BXY(1535),BXY(1536)/  3,  5,  3,  6/
       data BXY(1537),BXY(1538),BXY(1539),BXY(1540)/  4,  0,  0,  0/
       data BXY(1541),BXY(1542),BXY(1543),BXY(1544)/  8,  6,  8,  6/
       data BXY(1545),BXY(1546),BXY(1547),BXY(1548)/  0,  0,  0,  6/
       data BXY(1549),BXY(1550),BXY(1551),BXY(1552)/  8,-64,  0,  8/
       data BXY(1553),BXY(1554),BXY(1555),BXY(1556)/  6,  0,  0,  0/
      end
