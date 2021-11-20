c  SMDPRB1.F  09 May 1993
c
      program smdprb1
c
c***********************************************************************
c
c  This program demonstrates semi-log and smooth curve plotting
c  using SMDLIB.
c
      integer npts
      parameter (npts=5)
c
      real cycle
      integer iline
      integer istory(100)
      integer maxlin
      real orig
      real rlen
      real slngth
      real x(npts)
      real xblank
      real xbtext
      real xlen
      real xmax
      real xmin
      real y(npts)
      real yblank
      real ybtext
      real ymax
      real ymin
c
      write(*,*)' '
      write(*,*)'SMDPRB1'
      write(*,*)'A sample program for SMDLIB.'
      write(*,*)' '
c
c  Set points on the curves
c
      x(1)=500010.0
      x(2)=1000000.0
      x(3)=2500000.0
      x(4)=3500000.0
      x(5)=4500000.0
c
      y(1)=1.0
      y(2)=10000.0
      y(3)=10.0
      y(4)=30000.0
      y(5)=100.0
c
c  Set maximum and minimum values to define axes.
c
      xmin=100000.0
      xmax=4600000.0
      ymin=1.0
      ymax=46.0*46.0*46.0
c
c  Define device to be CGM binary metafile.
c
      call psccgm('cgmb','smdprb1.cgm')
c
c  Define plot area
c
      call page(11.0,14.0)
      call origin(2.5,3.0)
c
c  Define axes
c
      call bgnsub('Curve smoothing (semi-log)$',100,
     &      'Linear X axis$',100,' ',0,7.0,8.0)
      call scalog(ymin,ymax,8.0,orig,cycle)
c
c  Calling DUPLEX results in an ENORMOUS output file!  Don't do it!
c
c     call duplex
      call axes2d(xmin,'scal',xmax,ymin,'scal',ymax)
      call xtrlgy(orig,cycle,8.0,'Log Y axis$',100,0.0,0.0)
c
c  Packed text for story plotting
c
      call height(0.12)
      iline=maxlin(istory,400,50)
      call paklin('Green-solid, Polynomial interpolation$',istory,1)
      call paklin('Yellow-dot, Parametric spline interpolation$',
     &           istory,2)
      call paklin('Cyan-chndot, Spline interpolation$',istory,3)
      call paklin('White-dash, No interpolation$',istory,4)
      yblank=ybtext(istory,4)
      xblank=xbtext(istory,4)
c
c  Blank area for later story plotting.
c
      call blank1(1.9,2.1+xblank,0.2,0.4+yblank,3)
      call setclr('red')
      call grid(1,1)
c
c  Plot curve 1 - green
c
      call poly3
      call setclr('green')
      call curve(x,y,npts,1)
c
c  Plot curve 2 - yellow
c
      call prmspl
      call setclr('yellow')
      call dot
      call curve(x,y,npts,1)
c
c  Plot curve 3 - cyan
c
      call cubspl
      call setclr('cyan')
      call chndot
      call curve(x,y,npts,1)
c
c  Plot curve 4 - white
c
      call reset('cubspl')
      call setclr('foreground')
      call dash
      call curve(x,y,npts,1)
c
c  Write story text
c
      call reset('blank1')
      call setclr('magenta')
      call btextl(istory,4,2.0,0.3)
c
c  Write descriptive note.
c
      call endsub(0)
      call movori(0.0,-2.0)
      call setsub(7.0,8.0)
      call setclr('foreground')
      call height(0.20)
      xlen=slngth('Figure 1, sample of demo 1$',100)
      rlen=(7.0-xlen)/2.0
      call messag('Figure 1, sample of demo 1$',100,rlen,0.5)
c
c  Terminate plot, release device.
c
      call stoplt(0)
      call finplt
      stop
      end
