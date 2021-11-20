c  SMDPRB5.F  09 May 1993
c
      program smdprb5
c
c***********************************************************************
c
c  This program demonstrates line styles and TXTBLK plotting.
c
      real dy
      real gap
      real hite
      integer i
      integer leginf(3,13)
      real ray(12)
      real rlen
      real seg
      real slngth
      character*30 text(13)
      real x1(10)
      real xdimtb
      real xlen
      real y1(10)
      real y2(10)
      real y3(10)
      real y4(10)
      real y5(10)
      real y6(10)
      real ylen
      real z1(10)
      real z2(10)
      real z3(10)
      real z4(10)
      real z5(10)
      real z6(10)
      real z7(10)
c
      external slngth
      external xdimtb
c
      write(*,*)' '
      write(*,*)'SMDPRB5'
      write(*,*)'Demonstrate line styles and TXTBLK plotting.'
      write(*,*)' '
c
c  Calculate points for curves
c
      do 100 i=1,4
c
        if (i.eq.4) then
          x1(i)=8.0
          z7(i)=z7(3)
        else
          x1(i)=real(i)
          z7(i)=x1(i)/2.0
        endif
c
        z6(i)=z7(i)+1.0
        z5(i)=z6(i)+1.0
        z4(i)=z5(i)+1.0
        z3(i)=z4(i)+1.0
        z2(i)=z3(i)+1.0
        z1(i)=z2(i)+1.0
        y6(i)=z1(i)+1.0
        y5(i)=y6(i)+1.0
        y4(i)=y5(i)+1.0
        y3(i)=y4(i)+1.0
        y2(i)=y3(i)+1.0
        y1(i)=y2(i)+1.0
 100  continue
c
c  Select device
c
      call psccgm('cgmb','smdprb5.cgm')
c
c  Define plot area
c
      call page(11.0,13.0)
      call origin(3.0,2.5)
c
c  Define axes
c
      call setsub(7.0,8.0)
      call xlabel('X axis$',100)
      call ylabel('Y axis$',100)
      call yangle(0.0)
      call defalf('italic')
      call headin('Line styles and legend.$',100,2.0,1)
      call reset('defalf')
      call axes2d(0.0,2.0,10.0,0.0,2.0,14.0)
      call savlin
      call cartog
      call vspace(1.8)
      call leghdg('Legend and symbols',18)
c
      write(*,*)'check point arnold'
c
c  Load the legend
c
      text(1)='Solid$'
      leginf(1,1)=3
      leginf(2,1)=1
      leginf(3,1)=5
      text(2)='Dot, blanked @ symbols$'
      leginf(1,2)=4
      leginf(2,2)=1
      leginf(3,2)=2
      text(3)='Dash, big symbols$'
      leginf(1,3)=5
      leginf(2,3)=1
      leginf(3,3)=4
      text(4)='Chndot, no symbol$'
      leginf(1,4)=6
      leginf(2,4)=0
      leginf(3,4)=7
      text(5)='Chdash$'
      leginf(1,5)=7
      leginf(2,5)=1
      leginf(3,5)=0
      text(6)='User defined$'
      leginf(1,6)=1
      leginf(2,6)=1
      leginf(3,6)=3
      text(7)='Symbols only$'
      leginf(1,7)=2
      leginf(2,7)=-1
      leginf(3,7)=6
      text(8)='Thick solid$'
      leginf(1,8)=13
      leginf(2,8)=1
      leginf(3,8)=5
      text(9)='Thick dot$'
      leginf(1,9)=14
      leginf(2,9)=1
      leginf(3,9)=2
      text(10)='Thick dash$'
      leginf(1,10)=15
      leginf(2,10)=1
      leginf(3,10)=4
      text(11)='Thick chndot$'
      leginf(1,11)=16
      leginf(2,11)=1
      leginf(3,11)=7
      text(12)='Thick chndsh$'
      leginf(1,12)=17
      leginf(2,12)=1
      leginf(3,12)=0
      text(13)='Thick user defined$'
      leginf(1,13)=1
      leginf(2,13)=1
      leginf(3,13)=3
      call vspace(2.0)
      call reset('cartog')
c
      xlen=xdimtb(text,1,13)
      hite=0.25
      dy=0.01
      ylen=(hite+dy)*13.0
      call hrdrgb(0.0,1.0,1.0)
c     call setclr('CYAN')
      seg=1.0
      gap=0.1
      call blanka(3.0,1.2,xlen+gap+seg+0.2,ylen+0.2,0.01)
      call setclr('foreground')
      call frame
c
c  Plot curve 1 - Green
c
      call hrdrgb(0.0,1.0,0.0)
c     call setclr('GREEN')
      call curve(x1,y1,4,1)
c
c  Plot curve 2 - Magenta
c
      call blanks
      call hrdrgb(1.0,0.0,1.0)
c     call setclr('MAGENTA')
      call dot
      call curve(x1,y2,4,1)
      call reset('blanks')
c
c  Plot curve 3 - Yellow
c
      call sizmrk(3.0)
      call hrdrgb(1.0,1.0,0.0)
c     call setclr('YELLOW')
      call dash
      call curve(x1,y3,4,1)
      call reset('sizmrk')
c
c  Plot curve 4 - Blue
c
      call chndot
      call hrdrgb(0.0,0.0,1.0)
c     call setclr('BLUE')
      call curve(x1,y4,4,0)
c
c  Plot curve 5 - White
c
      call chndsh
      call hrdrgb(1.0,1.0,1.0)
c     call setclr('White')
      call curve(x1,y5,4,1)
c
c  Plot curve 6 - Red
c
      ray(1)=1.0
      ray(2)=3.0
      call lindef(0.2,2,ray)
      call hrdrgb(1.0,0.0,0.0)
c     call setclr('RED')
      call curve(x1,y6,4,1)
c
c  Plot curve 7 - Cyan
c
      call hrdrgb(0.0,1.0,1.0)
c     call setclr('CYAN')
      call curve(x1,z1,4,-1)
c
c  Plot curve 8 - Green
c
      call crvwid(0.04)
      call reset('lindef')
      call hrdrgb(0.0,1.0,0.0)
c     call setclr('GREEN')
      call curve(x1,z2,4,1)
c
c  Plot curve 9 - Magenta
c
      call hrdrgb(1.0,0.0,1.0)
c     call setclr('MAGENTA')
      call dot
      call curve(x1,z3,4,1)
c
c  Plot curve 10 - Yellow
c
      call hrdrgb(1.0,1.0,0.0)
c     call setclr('YELLOW')
      call dash
      call curve(x1,z4,4,1)
c
c  Plot curve 11 - Blue
c
      call chndot
      call hrdrgb(0.0,0.0,1.0)
c     call setclr('BLUE')
      call curve(x1,z5,4,1)
c
c  Plot curve 12 - White
c
      call chndsh
      call hrdrgb(1.0,1.0,1.0)
c     call setclr('White')
      call curve(x1,z6,4,1)
c
c  Plot curve 13 - Red
c
      ray(1)=25.0
      ray(2)=5.0
      ray(3)=3.0
      ray(4)=5.0
      ray(5)=3.0
      ray(6)=5.0
      ray(7)=3.0
      ray(8)=5.0
      call lindef(0.9,8,ray)
      call hrdrgb(1.0,0.0,0.0)
c     call setclr('RED')
      call curve(x1,z7,4,1)
      call reset('crvwid')
c
c  Write story text.
c
      call hrdrgb(1.0,1.0,1.0)
c     call setclr('White')
      call reset('blnkal')
c     call TXTBLK(IP,13,3.1,1.3)
      call plegnd(text,1,13,dy,3.1,1.3,leginf,hite,seg,gap,
     & .false.)
c
c  Write figure title
c
      call endsub(0)
      call movori(0.0,-1.5)
      call setsub(7.0,1.5)
      call height(0.2)
      rlen=slngth('Sample of line styles$',100)
      xlen=(7.0-rlen)/2.0
      call messag('Sample of line styles$',100,xlen,0.0)
c
c  Terminate plot and release device
c
      call stoplt(0)
      call finplt
      stop
      end
