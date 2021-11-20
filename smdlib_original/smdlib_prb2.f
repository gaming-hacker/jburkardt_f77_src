c  SMDPRB2.F  21 July 1993
c
      program smdprb2
c
c***********************************************************************
c
c  This program demonstrates line styles and TxTBLK plotting.
c
      real rlen
      real slngth
      real x1(4)
      real x2(4)
      real x3(4)
      real x4(4)
      real x5(4)
      real xlen
      real y1(4)
      real y2(4)
      real y3(4)
      real y4(4)
      real y5(4)
c
      data x1,y1/1.0,1.0,2.0,2.0,0.0,4.1,4.1,0.0/
      data x2,y2/3.0,3.0,4.0,4.0,0.0,8.8,8.8,0.0/
      data x3,y3/5.0,5.0,6.0,6.0,0.0,5.0,5.0,0.0/
      data x4,y4/7.0,7.0,8.0,8.0,0.0,0.9,0.9,0.0/
      data x5,y5/9.0,9.0,10.0,10.0,0.0,2.6,2.6,0.0/
c
      write(*,*)' '
      write(*,*)'SMDPRB2'
      write(*,*)'Demonstrate line styles and TXTBLK plotting.'
      write(*,*)' '
C
C  Select output device
C
      call psccgm('cgmb','smdprb2.cgm')
C
C  Define plot area
C 
      call page(10.0,13.0)
      call origin(2.5,2.0)
C
C  Define axes
C
      call setsub(6.5,8.0)
      call noxlbl
      call xlabel('X axis ((No Numbering))$',100)
      call ylabel('Y axis',6)
      call ymarks(2)
      call yangle(60.0)
C
C  Define mixed alphabets for heading
C
      call triplx
      call alpha1('STAND',')')
      call alpha2('ITALIC','(')
      call alpha3('GREEK','[')
      call alpha4('SCRIPT',']')
      call headin('(Bar Chart )and ]Sub-plot)$',-100,-2.0,4)
      call headin(' $',100,1.0,4)
      call headin('Integer / (Real Axis)$',-100,1.5,4)
      call duplex
      call headin('Ticks and Text Angle Variations$',100,1.0,4)
      call reset('DUPLEX')
C
C  Define coordinate system.
C  Note: Mixed alphabet still active for axis labels.
C
      call axes2d(0.0,11.0,11.0,0.0,1.0,10.0)
C
C  Deactive mixed alphabets.
C
      call reset('TRIPLX')
      call reset('ALPHA1')
      call reset('ALPHA2')
      call reset('ALPHA3')
      call reset('ALPHA4')
C
C  Blank area for subplot.
C
      call blanka(4.1,3.8,2.9,4.0,0.03)
      call grid(0,1)
      call hrdshd
C
C  Shade area 1 - Green
C
      call setclr('GREEN')
      call shade(x1,y1,4,90.0,0.02,1,0,0)
      call relrno(4.1,1,1.0,4.6)
C
C  Shade area 2 - Magenta
C
      call setclr('MAGENTA')
      call shade(x2,y2,4,90.0,0.02,1,0,0)
      call relrno(8.8,1,3.0,9.3)
C
C  Shade area 3 - Yellow
C
      call setclr('YELLOW')
      call shade(x3,y3,4,90.0,0.02,1,0,0)
      call relrno(5.0,1,5.0,5.5)
C
C  Shade area 4 - Blue
C
      call setclr('BLUE')
      call shade(x4,y4,4,90.0,0.02,1,0,0)
      call relrno(0.9,1,7.0,1.4)
C
C  Shade area 5 - Red
C
      call setclr('RED')
      call shade(x5,y5,4,90.0,0.02,1,0,0)
      call relrno(2.6,1,9.0,3.1)
      call reset('HRDSHD')
      call reset('CLRSET')
      call reset('BLNKAL')
      call endsub(0)
C
C  Create a subplot.
C
      x1(1)=1.0
      x1(2)=2.0
      x1(3)=3.0
      x1(4)=4.0
      y1(1)=1.0
      y1(2)=2.0
      y1(3)=1.0
      y1(4)=4.0
C
C  Define axes parameters
C
      call movori(4.75,4.5)
      call setsub(2.0,2.5)
      call reset('NOXLBL')
      call intgrx
      call xmarks(4)
      call xlabel('Integer X axis$',100)
      call ylabel('Real Y axis$',100)
      call reset('YANGLE')
C
C  Write heading in mixed alphabet.
C
      call cartog
      call alpha1('STAND',']')
      call alpha2('GREEK','[')
      call headin('Subplot$',100,-1.0,2)
      call headin('(circ = [2qp])$',100,1.0,2)
      call reset('CARTOG')
      call reset('ALPHA1')
      call reset('ALPHA2')
C
C  Define coordinate system.
C
      call axes2d(1.0,1.0,4.0,0.0,1.0,4.0)
      call setclr('CYAN')
      call cubspl
      call curve(x1,y1,4,1)
      call setclr('GREEN')
      call relvec(3.0,3.0,3.5,2.5,1221)
      call defalf('ITALI')
      call mixalf('GREEK')
      call relmsg('(b)-SPLINE$',100,1.5,3.0)
C
C  Write figure title.
C
      call endsub(0)
      call movori(-4.75,-5.5)
      call setsub(6.5,1.0)
      call height(0.2)
      xlen=slngth('SMDPRB2$',100)
      rlen=(6.5-xlen)/2.0
      call messag('SMDPRB2$',100,rlen,0.0)
C
C  Terminate plot
C
      call stoplt(0)
C
C  Terminate graphics
C
      call finplt
      stop
      end
