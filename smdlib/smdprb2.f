!  smdprb2.f  12 May 1997
!
      program smdprb2
!
!***********************************************************************
!
!  This program demonstrates line styles and TXTBLK plotting.
!
      integer lun
      character*1 orient
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
!
      data x1,y1/1.0,1.0,2.0,2.0,0.0,4.1,4.1,0.0/
      data x2,y2/3.0,3.0,4.0,4.0,0.0,8.8,8.8,0.0/
      data x3,y3/5.0,5.0,6.0,6.0,0.0,5.0,5.0,0.0/
      data x4,y4/7.0,7.0,8.0,8.0,0.0,0.9,0.9,0.0/
      data x5,y5/9.0,9.0,10.0,10.0,0.0,2.6,2.6,0.0/
!
      write(*,*)' '
      write(*,*)'SMDPRB2'
      write(*,*)'Demonstrate line styles and TXTBLK plotting.'
      write(*,*)' '
!
!  Define the graphics output device to be a PostScript file,
!  in portrait orientation, written to unit 7.
!
      lun=7
      orient = 'P'
 
      call postsc(lun,orient)
!
!  Define plot area
!
      call page(10.0,13.0)
      call origin(2.5,2.0)
!
!  Define axes
!
      call setsub(6.5,8.0)
      call noxlbl
      call xlabel('X axis ((No Numbering))$',100)
      call ylabel('Y axis',6)
      call ymarks(2)
      call yangle(60.0)
!
!  Define mixed alphabets for heading
!
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
!
!  Define coordinate system.
!  Note: Mixed alphabet still active for axis labels.
!
      call axes2d(0.0,11.0,11.0,0.0,1.0,10.0)
!
!  Deactive mixed alphabets.
!
      call reset('TRIPLX')
      call reset('ALPHA1')
      call reset('ALPHA2')
      call reset('ALPHA3')
      call reset('ALPHA4')
!
!  Blank area for subplot.
!
      call blanka(4.1,3.8,2.9,4.0,0.03)
      call grid(0,1)
      call hrdshd
!
!  Shade area 1 - Green
!
      call clrset('GREEN')
      call shade(x1,y1,4,90.0,0.02,1,0,0)
      call relrno(4.1,1,1.0,4.6)
!
!  Shade area 2 - Magenta
!
      call clrset('MAGENTA')
      call shade(x2,y2,4,90.0,0.02,1,0,0)
      call relrno(8.8,1,3.0,9.3)
!
!  Shade area 3 - Yellow
!
      call clrset('YELLOW')
      call shade(x3,y3,4,90.0,0.02,1,0,0)
      call relrno(5.0,1,5.0,5.5)
!
!  Shade area 4 - Blue
!
      call clrset('BLUE')
      call shade(x4,y4,4,90.0,0.02,1,0,0)
      call relrno(0.9,1,7.0,1.4)
!
!  Shade area 5 - Red
!
      call clrset('RED')
      call shade(x5,y5,4,90.0,0.02,1,0,0)
      call relrno(2.6,1,9.0,3.1)
      call reset('HRDSHD')
      call reset('CLRSET')
      call reset('BLNKAL')
      call endsub
!
!  Create a subplot.
!
      x1(1)=1.0
      x1(2)=2.0
      x1(3)=3.0
      x1(4)=4.0
      y1(1)=1.0
      y1(2)=2.0
      y1(3)=1.0
      y1(4)=4.0
!
!  Define axes parameters
!
      call movori(4.75,4.5)
      call setsub(2.0,2.5)
      call reset('NOXLBL')
      call intgrx
      call xmarks(4)
      call xlabel('Integer X axis$',100)
      call ylabel('Real Y axis$',100)
      call reset('YANGLE')
!
!  Write heading in mixed alphabet.
!
      call cartog
      call alpha1('STAND',']')
      call alpha2('GREEK','[')
      call headin('Subplot$',100,-1.0,2)
      call headin('(circ = [2qp])$',100,1.0,2)
      call reset('CARTOG')
      call reset('ALPHA1')
      call reset('ALPHA2')
!
!  Define coordinate system.
!
      call axes2d(1.0,1.0,4.0,0.0,1.0,4.0)
      call clrset('CYAN')
      call cubspl
      call curve(x1,y1,4,1)
      call clrset('GREEN')
      call relvec(3.0,3.0,3.5,2.5,1221)
      call defalf('ITALI')
      call mixalf('GREEK')
      call relmsg('(b)-SPLINE$',100,1.5,3.0)
!
!  Write figure title.
!
      call endsub
      call movori(-4.75,-5.5)
      call setsub(6.5,1.0)
      call height(0.2)
      xlen=slngth('SMDPRB2$',100)
      rlen=(6.5-xlen)/2.0
      call messag('SMDPRB2$',100,rlen,0.0)
!
!  Terminate plot
!
      call stoplt
!
!  Terminate graphics
!
      call finplt
 
      stop
      end
