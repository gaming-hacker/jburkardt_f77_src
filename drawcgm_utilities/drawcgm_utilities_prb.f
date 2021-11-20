      program main

c*********************************************************************72
c
cc MAIN is the main program for DRAWCGM_UTILITIES_PRB.
c
c  Discussion:
c
c    DRAWCGM_UTILITIES_PRB demonstrates the routines in DRAWCGM_UTILITIES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      character*4 dev
      character*30 filnam
      integer itable

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DRAWCGM_UTILITIES_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Tests for DRAWCGM_UTILITIES.'
c
c  Choose the output device to be used.
c
      dev = 'ps'
c
c  Set the output device.
c  Choices include 'cgmb', 'ps' or 'xws'.
c
      call device ( dev )
c
c  Choose a name for the output file.
c
      if ( dev .eq. 'cgmb' ) then
        filnam = 'drawcgm_utilities.cgm'
        call outfil ( filnam )
      else if ( dev .eq. 'ps' ) then
        filnam = 'drawcgm_utilities.ps'
        call outfil ( filnam )
      end if
c
c  Set the color table in use.
c
      itable = 1
      call setctb ( itable )
c
c  For X Window output, pause after each picture.
c
      if ( dev .eq. 'xws' ) then
        call stpaus
      end if
c
c  Initialize DRAWCGM.
c
      call grfini
 
      call test01
      call newfrm
      call test02
      call newfrm
      call test03
      call newfrm
      call test04
      call newfrm
      call test05
      call newfrm
      call test06
      call newfrm
      call test07
      call newfrm
      call test08
      call newfrm
      call test09
      call newfrm
      call test10
      call newfrm
      call test11
      call newfrm
      call test12
      call newfrm
      call test13
      call newfrm
      call test14
      call newfrm
      call test15
      call newfrm
      call test16
      call newfrm
      call test17
c
c  Call the DRAWCGM "finish up" routine.
c
      call grfcls
 
      if ( dev .ne. 'cgmb' ) then
        call file_delete ( 'CGMOUT' )
      end if
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DRAWCGM_UTILITIES_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01

c*********************************************************************72
c
cc TEST01 demonstrates the CIRKLE routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real angle
      real cwide
      logical filled
      character*1 flush
      integer icolor
      real pwide
      real radius
      character*80 string
      real x
      real xval
      real y
      real yval

      write(*,*)' '
      write(*,*)'TEST01'
      write(*,*)'Show how CIRKLE makes filled in disks.'
      write(*,*)' '
c
c  Choose the line color.
c
      icolor = 1
      call linclr ( icolor )
c
c  Make the coordinate system 0 to 1.
c
      call scl01
c
c  Draw a box around the picture.
c
      call box01
c
c  Draw three disks.
c
      icolor = 2
      call filclr ( icolor )
 
      x = 0.5
      y = 0.5
      radius = 0.25
      filled = .true.
 
      call cirkle ( x, y, radius, filled )
 
      x = 0.25
      y = 0.75
      radius = 0.125
      filled = .true.
 
      call cirkle ( x, y, radius, filled )
 
      x = 0.75
      y = 0.75
      radius = 0.125
      filled = .true.
 
      call cirkle ( x, y, radius, filled )
c
c  Draw new disks inside the old ones, of a different color.
c
      icolor = 0
      call filclr ( icolor )
 
      x = 0.5625
      y = 0.5625
      radius = 0.03125
      filled = .true.
 
      call cirkle ( x, y, radius, filled )
 
      x = 0.4375
      y = 0.5625
      radius = 0.03125
      filled = .true.
 
      call cirkle ( x, y, radius, filled )
 
      x = 0.5
      y = 0.40625
      radius = 0.03125
      filled = .true.
 
      call cirkle ( x, y, radius, filled )
c
c  Write a title.
c
      angle = 0
      cwide = 0.05
      pwide = 1
      string = 'Mortimer Mouse'
      xval = 0.5
      yval = 0.1
      flush = 'Center'
 
      call chrplt ( angle, cwide, pwide, string, xval, yval, flush )
 
      return
      end
      subroutine test02

c*********************************************************************72
c
cc TEST02 demonstrates the CIRKLE routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real angle
      real cwide
      logical filled
      character*1 flush
      integer icolor
      real pwide
      character*80 string
      real xval
      real yval

      write(*,*)' '
      write(*,*)'TEST02'
      write(*,*)'Show how CIRKLE makes open circles.'
      write(*,*)' '
c
c  Choose the line color.
c
      icolor = 1
      call linclr ( icolor )

      call scl01
c
c  Draw a box around the picture.
c
      call box01
c
c  Draw three disks.
c
      icolor = 1
      call linclr ( icolor )

      filled = .false.

      call cirkle(0.5,0.5,0.25, filled )
      call cirkle(0.25,0.75,0.125, filled )
      call cirkle(0.75,0.75,0.125, filled )
c
c  Draw new circles inside the old ones.
c
      call cirkle(0.5625,0.5625,0.03125, filled )
      call cirkle(0.4375,0.5625,0.03125, filled )
      call cirkle(0.5000,0.40625,0.03125, filled )
c
c  Write a title.
c
      angle = 0
      cwide = 0.05
      pwide = 1
      string = 'Modine Mouse'
      xval = 0.5
      yval = 0.1
      flush = 'Center'
 
      call chrplt ( angle, cwide, pwide, string, xval, yval, flush )
 
      return
      end
      subroutine test03

c*********************************************************************72
c
cc TEST03 demonstrates the PGRID routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real pi
      parameter ( pi = 3.1415926535 )

      real degrad
      parameter ( degrad = pi/180.0 )

      integer ndata
      parameter ( ndata = 50 )

      real angle
      real angmax
      real angmin
      real cwide
      character*1 flush
      integer i
      integer icolor
      integer nang
      integer nr
      real pwide
      real radius
      real rmax
      real rmin
      character*80 string
      real xdata(ndata)
      real xorig
      real xval
      real ydata(ndata)
      real yorig
      real yval

      write(*,*)' '
      write(*,*)'TEST03'
      write(*,*)'This program demonstrates how to use the DRAWCGM'
      write(*,*)'graphics package to create a simple line drawing'
      write(*,*)'incorporating a polar grid drawn by PGRID.'
      write(*,*)' '
c
c  Set the line color
c
      icolor = 2
      call linclr ( icolor )
c
c  Make sure coordinate system is set to default 0,1 system
c
      call scl01
c
c  Draw a box around the default 0,1 picture
c
      call box01
c
c  Set up the X and Y vectors of data in a DO loop.
c
      angmin = 0.0
      angmax = 80.0
 
      do i = 1,ndata
        angle = degrad*(real(ndata-i)*angmin+real(i-1)*angmax)
     &    /real(ndata-1)
        radius = sin(2.0*angle)
        xdata(i) = radius*cos(angle)
        ydata(i) = radius*sin(angle)
      end do
c
c  Draw the curve
c
      call plylin(ndata,xdata,ydata)
c
c  Draw a polar grid system from R = 0.2 to R=1.0, and from 10 to 60 degrees.
c
      xorig = 0.0
      yorig = 0.0
      rmin = 0.2
      rmax = 1.0
      nr = 5
      angmin = 10.0
      angmax = 60.0
      nang = 11

      call pgrid ( xorig, yorig, rmin, rmax, nr, angmin, angmax, nang )
c
c  Write a title.
c
      angle = 0
      cwide = 0.05
      pwide = 1
      string = 'Test of PGrid'
      xval = 0.5
      yval = 0.9
      flush = 'Center'
 
      call chrplt(angle,cwide,pwide,string,xval,yval,flush)
 
      return
      end
      subroutine test04

c*********************************************************************72
c
cc TEST04 demonstrates the CARC and WEDGE routines.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real angle
      real angmax
      real angmin
      real cwide
      character*1 flush
      integer i
      integer icolor
      real pwide
      real radius
      character*80 string
      real xcenter
      real xmax
      real xmin
      real xval
      real ycenter
      real ymax
      real ymin
      real yval
      real zero

      zero = 0.0
 
      write(*,*)' '
      write(*,*)'TEST04'
      write(*,*)'This program demonstrates how to use the CARC and'
      write(*,*)'WEDGE routines for circular arcs and wedges.'
      write(*,*)' '
c
c  Set the line color.
c
      icolor = 1
      call linclr ( icolor )
c
c  Set coordinate system to [-2,2] by [-2,2].
c
      xmin = -2.0
      xmax = 2.0
      ymin = -2.0
      ymax = 2.0
      call scale ( xmin, xmax, ymin, ymax )
c
c  Draw a box around the picture.
c
      call box ( xmin, xmax, ymin, ymax )
c
c  Draw a coordinate cross.
c
      call movcgm(xmin,zero)
      call drwcgm(xmax,zero)
      call movcgm(zero,ymin)
      call drwcgm(zero,ymax)
c
c  Draw a series of circular arcs in the southwest and northeast
c  quadrants.
c
      do i = 1,8
 
        xcenter = 0.0
        ycenter = 0.0
        radius = sqrt(i*0.25)
        angmin = 0.0
        angmax = 90.0
        call carc(xcenter,ycenter,radius,angmin,angmax)
 
        xcenter = 0.0
        ycenter = 0.0
        radius = sqrt(i*0.25)
        angmin = 180.0
        angmax = 270.0
        call carc(xcenter,ycenter,radius,angmin,angmax)
 
      end do
c
c  Put a wedge in the northwest corner.
c
      icolor = 1
      call filclr ( icolor )
      radius = 1.5
      xcenter = -0.5
      ycenter =  0.5
      angmin = 115.0
      angmax = 160.0
 
      call wedge(xcenter,ycenter,radius,angmin,angmax)
c
c  Label the plot.
c
      angle = 0
      cwide = 0.05
      pwide = 1.0
      string = 'Test of CARC and WEDGE'
      xval = 0.1
      yval = -1.0
      flush = 'Center'
 
      call chrplt(angle,cwide,pwide,string,xval,yval,flush)
 
      return
      end
      subroutine test05

c*********************************************************************72
c
cc TEST05 demonstrates the ELLIPSE routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real angle
      integer icolor
      real rmajor
      real rminor
      real size
      real xcenter
      real xval
      real ycenter
      real yval

      write(*,*)' '
      write(*,*)'TEST05'
      write(*,*)'This program demonstrates how to use the DRAWCGM'
      write(*,*)'graphics package to create an ellipse.'
      write(*,*)' '
c
c  Set the line color.
c
      icolor = 2
      call linclr ( icolor )
c
c  Set coordinate system to [-3,3] by [-3,3].
c
      call scale(-3.0,3.0,-3.0,3.0)
c
c  Draw a box around the picture.
c
      call box(-3.0,3.0,-3.0,3.0)
c
c  Draw three ellipses.
c
      xcenter = 0.0
      ycenter = 0.0
      rmajor = 2.5
      rminor = 1.0
      angle = 90.0
      call ellipse(xcenter,ycenter,rmajor,rminor,angle)
 
      angle = 30.0
      call ellipse(xcenter,ycenter,rmajor,rminor,angle)
 
      angle = -30.0
      call ellipse(xcenter,ycenter,rmajor,rminor,angle)
c
c  We want to draw a circle of radius 0.50.  But just to prove that a
c  circle is an ellipse that's behaving, let's use the ELLIPSE routine
c  to draw it!
c
      xcenter = 0.0
      ycenter = 0.0
      rmajor = 0.5
      rminor = 0.5
      angle = 0.0
 
      call ellipse(xcenter,ycenter,rmajor,rminor,angle)
c
c  Label the plot.
c
      xval = -2.5
      yval = 2.5
      icolor = 2
      size = 0.30
 
      call label(xval, yval, 'Demonstration of ELLIPSE', icolor, size)
 
      return
      end
      subroutine test06

c*********************************************************************72
c
cc TEST06 demonstrates the OVAL routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real angle
      integer icolor
      real rmajor
      real rminor
      real size
      real xcenter
      real xval
      real ycenter
      real yval

      write(*,*)' '
      write(*,*)'TEST06'
      write(*,*)'This program demonstrates how to use the DRAWCGM'
      write(*,*)'graphics package to create an ellipse.'
      write(*,*)' '
c
c  Set the line color.
c
      icolor = 2
      call linclr ( icolor )
c
c  Set coordinate system to [-3,3] by [-3,3].
c
      call scale(-3.0,3.0,-3.0,3.0)
c
c  Draw a box around the picture.
c
      call box(-3.0,3.0,-3.0,3.0)
c
c  Draw three ellipses.
c
      xcenter = 0.0
      ycenter = 0.0
      rmajor = 2.5
      rminor = 1.0

      icolor = 1
      call filclr ( icolor )
      angle = 90.0
      call oval(xcenter,ycenter,rmajor,rminor,angle)
 
      angle = 30.0
      call oval(xcenter,ycenter,rmajor,rminor,angle)
 
      angle = -30.0
      call oval(xcenter,ycenter,rmajor,rminor,angle)
c
c  We want to draw a circle of radius 0.5.  But just to prove that
c  a circle is an ellipse that's behaving, let's use the ELLIPSE
c  routine to draw it!
c
c  PS: We actually want to draw a circular "hole" inside the shape
c  formed by the three ovals we've already drawn.  To do this, we
c  must draw the ovals first, and then the circle.  If we reversed
c  the order, the circle would be hidden by the ovals, because each
c  new filled shape completely covers the area it encircles.
c
      icolor = 0
      call filclr ( icolor )
      xcenter = 0.0
      ycenter = 0.0
      rmajor = 0.5
      rminor = 0.5
      angle = 0.0
      call oval(xcenter,ycenter,rmajor,rminor,angle)
c
c  Label the plot
c
      xval = -2.5
      yval = 2.5
      icolor = 2
      size = 0.30
      call label(xval, yval, 'Demonstration of OVAL', icolor, size)
 
      return
      end
      subroutine test07

c*********************************************************************72
c
cc TEST07 demonstrates the SQUARE and DIAMOND routines.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real angle
      real cwide
      character*1 flush
      integer i
      integer n
      real pwide
      real side
      character*80 string
      real x
      real y

      n = 10
      write(*,*)' '
      write(*,*)'TEST07'
      write(*,*)'Test SQUARE and DIAMOND routines.'
      write(*,*)' '

      call scl01

      call box01

      x = 0.75
      y = 0.25
      do i = 1,n
        side = 0.5*real(i)/real(n)
        call square(side,x,y)
      end do

      x = 0.25
      y = 0.75
      do i = 1,n
        side = 0.5*real(i)/real(n)
        call diamond(side,x,y)
      end do
c
c  Label the plot.
c
      angle = 0
      cwide = 0.02
      string = 'Test of Square and Diamond'
      pwide = 1.0
      x = 0.6
      y = 0.7
      flush = 'Left'
 
      call chrplt(angle,cwide,pwide,string,x,y,flush)
 
      return
      end
      subroutine test08

c*********************************************************************72
c
cc TEST08 demonstrates the CHRPLT routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real angle
      real cwide
      character*1 flush
      real pwide
      real x
      real y

      write(*,*)' '
      write(*,*)'TEST08'
      write(*,*)'Test the CHRPLT routine for plotting characters.'
      write(*,*)' '

      call scl01
      call box01

      angle = 0.0
      cwide = 0.025
      pwide = 1
      x = 0.03
      y = 0.04
      flush = 'left'
 
      call chrplt(angle,cwide,pwide,'This is a test',x,y,flush)
 
      angle = 45.0
      cwide = 0.05
      x = 0.3
      y = 0.1
      flush = 'left'
 
      call chrplt(angle,cwide,pwide,'I am at 45 degrees!',x,y,flush)
 
      return
      end
      subroutine test09

c*********************************************************************72
c
cc TEST09 demonstrates the CLIP and DSHLIN routines.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real angle
      real cwide
      real dsize
      character*1 flush
      integer idraw
      integer nval
      real pwide
      character*80 string
      real x
      real x0
      real x1
      real xa
      real xb
      real xc
      real xd
      real xval(2)
      real y
      real y0
      real y1
      real ya
      real yb
      real yc
      real yd
      real yval(2)

      write(*,*)' '
      write(*,*)'TEST09'
      write(*,*)'Test the CLIP routine for clipping lines.'
      write(*,*)' '

      call scl01
      call box01

      x0 = 0.25
      x1 = 0.75
      y0 = 0.30
      y1 = 0.80
      call box(x0,x1,y0,y1)
c
c  Draw some dashed lines.
c
      nval = 2
      xval(1) = 0.0
      yval(1) = 0.5
      xval(2) = 1.0
      yval(2) = 0.5
      dsize = 0.025
      call dshlin(nval,xval,yval,dsize)

      nval = 2
      xval(1) = 0.0
      yval(1) = 1.0
      xval(2) = 1.0
      yval(2) = 0.0
      call dshlin(nval,xval,yval,dsize)

      nval = 2
      xval(1) = 0.0
      yval(1) = 0.0
      xval(2) = 1.0
      yval(2) = 1.0
      call dshlin(nval,xval,yval,dsize)

      xa = 0.0
      ya = 0.5
      xb = 1.0
      yb = 0.5
      call clip(xa,ya,xb,yb,xc,yc,xd,yd,idraw,x0,y0,x1,y1)
      if ( idraw.eq.1 ) then
        call movcgm(xc,yc)
        call drwcgm(xd,yd)
      else
        write(*,*)'? No line to draw?'
        write(*,*)'xa,ya',xa,ya
        write(*,*)'xb,yb',xb,yb
        write(*,*)'xc,yc',xc,yc
        write(*,*)'xd,yd',xd,yd
        write(*,*)'x0,y0',x0,y0
        write(*,*)'x1,y1',x1,y1
        write(*,*)'IDRAW',idraw
      end if

      xa = 0.0
      ya = 1.0
      xb = 1.0
      yb = 0.0
      call clip(xa,ya,xb,yb,xc,yc,xd,yd,idraw,x0,y0,x1,y1)
      if ( idraw.eq.1 ) then
        call movcgm(xc,yc)
        call drwcgm(xd,yd)
      else
        write(*,*)'? No line to draw??'
      end if

      xa = 0.0
      ya = 0.0
      xb = 1.0
      yb = 1.0
      call clip(xa,ya,xb,yb,xc,yc,xd,yd,idraw,x0,y0,x1,y1)
      if ( idraw.eq.1 ) then
        call movcgm(xc,yc)
        call drwcgm(xd,yd)
      else
        write(*,*)'? No line to draw???'
      end if

      angle = 0.0
      cwide = 0.02
      pwide = 1
      string = 'Test of CLIP'
      x = 0.5
      y = 0.9
      flush = 'Center'
 
      call chrplt(angle,cwide,pwide,string,x,y,flush)
 
      return
      end
      subroutine test10

c*********************************************************************72
c
cc TEST10 demonstrates the SPLXY routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ndat
      parameter (ndat = 5)

      integer nval
      parameter (nval = 30)

      real angle
      real cwide
      real csize
      real cubic
      character*1 flush
      integer i
      real pwide
      character*80 string
      real work(4,nval)
      real x
      real x1
      real x2
      real xdat(ndat)
      real xmax
      real xmin
      real xrange
      real xval(nval)
      real y
      real y1
      real y2
      real ydat(ndat)
      real ymax
      real ymin
      real yrange
      real yval(nval)

      external cubic

      write(*,*)' '
      write(*,*)'TEST10'
      write(*,*)'Test SPLXY, spline through X, Y data routine.'
      write(*,*)' '
c
c  Set data.
c
      xdat(1) = 0.0
      xdat(2) = 3.0
      xdat(3) = 4.0
      xdat(4) = 7.0
      xdat(5) = 10.0
      do i = 1,ndat
        ydat(i) = cubic(xdat(i))
      end do
c
c  Set evaluation points to be evenly spaced between 0 and 10.0.
c
      xmin = 0
      xmax = 10
      call reven(nval,xval,xmax,xmin)
c
c  Compute and evaluate the spline through the data.
c
      call splxy(ndat,nval,work,xdat,xval,ydat,yval)
c
c  Get the range of the data.
c
      xrange = xmax-xmin
      xmin = xmin-0.05*xrange
      xmax = xmax+0.05*xrange
      xrange = xmax-xmin
      call rrange(nval,yval,ymax,ymin)
      yrange = ymax-ymin
      ymin = ymin-0.05*yrange
      ymax = ymax+0.05*yrange
      yrange = ymax-ymin
c
c  Compute scale.
c
      do i = 1,ndat
        xdat(i) = (xdat(i)-xmin)/(xmax-xmin)
        ydat(i) = (ydat(i)-ymin)/(ymax-ymin)
      end do
 
      do i = 1,nval
        xval(i) = (xval(i)-xmin)/(xmax-xmin)
        yval(i) = (yval(i)-ymin)/(ymax-ymin)
      end do
 
      call scl01
      call box01
c
c  Draw the curve.
c
      call plylin(nval,xval,yval)
c
c  Draw the X axis.
c
      x1 = (0.0-xmin)/(xmax-xmin)
      y1 = (0.0-ymin)/(ymax-ymin)
      call movcgm(x1,y1)
      x2 = (10.0-xmin)/(xmax-xmin)
      y2 = y1
      call drwcgm(x2,y2)
c
c  Mark the data points.
c
      csize = 0.02
      do i = 1,ndat
        call cirkle ( xdat(i),ydat(i),csize,.true.)
      end do
c
c  Label the plot.
c
      angle = 0.0
      cwide = 0.02
      pwide = 1
      string = 'Test of SPLXY'
      x = 0.5
      y = 0.5
      flush = 'Center'
 
      call chrplt(angle,cwide,pwide,string,x,y,flush)
      return
      end
      function cubic(x)

c*********************************************************************72
c
cc CUBIC is a simple cubic polynomial in X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real cubic
      real x

      cubic = (x-5)*(x-8.5)*(x-2.5)

      return
      end
      subroutine test11

c*********************************************************************72
c
cc TEST11 draws a color wheel.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nval
      parameter (nval = 4)

      real angle
      real angle1
      real angle2
      real cwide
      real degrad
      character*1 flush
      integer i
      real pi
      real pwide
      character*80 string
      real x
      real xval(nval)
      real y
      real yval(nval)

      write(*,*)' '
      write(*,*)'TEST11'
      write(*,*)' '
      write(*,*)'Draw a color wheel.'

      pi = 3.1415926535
      degrad = pi/180.0
      call scale(-1.0,1.0,-1.0,1.0)
 
      call linclr(1)
      call box(-1.0,1.0,-1.0,1.0)
 
      xval(1) = 0.0
      yval(1) = 0.0
      xval(4) = 0.0
      yval(4) = 0.0
 
      do i = 0,255
 
        call filclr(i)
 
        angle1 = (90-360*real(i)/256.0)
        angle2 = (90-360*real(i+1)/256.0)
 
        xval(2) = cos(degrad*angle1)
        yval(2) = sin(degrad*angle1)
        xval(3) = cos(degrad*angle2)
        yval(3) = sin(degrad*angle2)
 
        call plygon(4,xval,yval)
 
      end do
 
      do i = 0,255
 
        angle1 = (90-360*real(i)/256.0)
        angle2 = (90-360*real(i+1)/256.0)
 
        xval(2) = cos(degrad*angle1)
        yval(2) = sin(degrad*angle1)
        xval(3) = cos(degrad*angle2)
        yval(3) = sin(degrad*angle2)
 
        call plylin(4,xval,yval)
 
      end do
c
c  Label the plot.
c
      angle = 0.0
      cwide = 0.02
      pwide = 1
      string = 'Color wheel'
      x = 0.7
      y = 0.9
      flush = 'Center'
 
      call chrplt(angle,cwide,pwide,string,x,y,flush)

      return
      end
      subroutine test12

c*********************************************************************72
c
cc TEST12 demonstrates the CBOX routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real angle
      real cwide
      character*1 flush
      real pwide
      character*80 string
      real x
      real y

      write(*,*)' '
      write(*,*)'TEST12'
      write(*,*)' '
      write(*,*)'Draw a color chart.'
      write(*,*)' '
 
      call scl01
 
      call cbox
c
c  Label the plot.
c
      angle = 0.0
      cwide = 0.02
      pwide = 1
      string = 'Color chart'
      x = 0.5
      y = 0.95
      flush = 'Center'
 
      call chrplt(angle,cwide,pwide,string,x,y,flush)
      return
      end
      subroutine test13

c*********************************************************************72
c
cc TEST13 demonstrates the TRIHATCH routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real angle
      real cwide
      character*1 flush
      integer nhatch
      real pwide
      character*80 string
      real x
      real xmax
      real xmin
      real xpmax
      real xpmin
      real xrange
      real xval(3)
      real y
      real ymax
      real ymin
      real ypmax
      real ypmin
      real yrange
      real yval(3)
c
      write(*,*)' '
      write(*,*)'TEST13'
      write(*,*)'Test TRIHATCH,'
      write(*,*)'which draws hatch marks in a triangle.'
      write(*,*)' '
c
      xmin = 0.0
      xmax = 4.0

      ymin = 0.0
      ymax = 4.0

      xrange = xmax - xmin
      yrange = ymax - ymin

      xpmin = xmin-0.05*xrange
      xpmax = xmax+0.05*xrange

      ypmin = ymin-0.05*yrange
      ypmax = ymax+0.05*yrange
c
c  Set the plot scale to be a little bigger than the data range.
c
      call scale ( xpmin, xpmax, ypmin, ypmax )
c
c  Draw a box along the data range.
c
      call box ( xmin, xmax, ymin, ymax )

      nhatch = 1
      xval(1) = 0.0
      xval(2) = 2.0
      xval(3) = 1.0
      yval(1) = 0.0
      yval(2) = 0.0
      yval(3) = 2.0
      call trihatch(xval,yval,nhatch)
c
      nhatch = 2
      xval(1) = 2.0
      xval(2) = 4.0
      xval(3) = 3.0
      yval(1) = 0.0
      yval(2) = 0.0
      yval(3) = 2.0
      call trihatch(xval,yval,nhatch)
c
      nhatch = 3
      xval(1) = 1.0
      xval(2) = 3.0
      xval(3) = 2.0
      yval(1) = 2.0
      yval(2) = 2.0
      yval(3) = 4.0
      call trihatch(xval,yval,nhatch)
c
      nhatch = 4
      xval(1) = 1.0
      xval(2) = 3.0
      xval(3) = 2.0
      yval(1) = 2.0
      yval(2) = 2.0
      yval(3) = 0.0
      call trihatch(xval,yval,nhatch)
c
c  Label the plot.
c
      angle = 0
      cwide = 0.02
      string = 'Test of TriHatch'
      pwide = 1.0
      x = 0.5
      y = 0.9
      flush = 'Center'
 
      call chrplt(angle,cwide,pwide,string,x,y,flush)
 
      return
      end
      subroutine test14

c*********************************************************************72
c
cc TEST14 demonstrates the ARROW routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real angle
      real cwide
      real del
      real delnrm
      real delx
      real dely
      character*1 flush
      integer i
      integer j
      integer n
      real pwide
      character*80 string
      real x
      real xstart
      real xtip
      real y
      real ystart
      real ytip
c
      write(*,*)' '
      write(*,*)'TEST14'
      write(*,*)'Test ARROW,'
      write(*,*)'which draws arrows at points.'
      write(*,*)' '
      call scl01
      call box01
c
      n = 10
      del = 0.04
 
      do i = 1,n
        do j = 1,n
          xstart = i/real(n+1)
          ystart = j/real(n+1)
 
          delx = del
          dely = -delx/(xstart**2)
          delnrm = sqrt(delx**2+dely**2)
          delx = del*delx/delnrm
          dely = del*dely/delnrm
 
          xtip = xstart+delx
          ytip = ystart+dely
 
          call arrow(xstart,ystart,xtip,ytip)
          write(*,*)xstart,ystart,xtip,ytip
        end do
      end do
c
c  Label the plot.
c
      angle = 0
      cwide = 0.15
      string = 'Test of Arrow'
      pwide = 1.0
      x = 0.5
      y = 0.9
      flush = 'Center'
 
      call chrplt(angle,cwide,pwide,string,x,y,flush)
 
      return
      end
      subroutine test15

c*********************************************************************72
c
cc TEST15 demonstrates the CGRID2 routine.
c
c  The value of NDATA controls how many points of the curve we will draw.
c  To get a smoother curve, increase NDATA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ndata
      parameter (ndata = 101)
c
c  NBOX is used to set aside space for 5 points we use to draw a
c  box around the region.
c
      integer nbox
      parameter (nbox = 5)
c
c  We only need PI to define the range of our X coordinate.
c
      real pi
      parameter ( pi = 3.1415926535 )
c
      integer i
      integer icolor
      integer nx
      integer ny
      real size
      real x(ndata)
      real xbox(nbox)
      real xhi
      real xlo
      real xmax
      real xmin
      real xval
      real y(ndata)
      real ybox(nbox)
      real yhi
      real ylo
      real ymax
      real ymin
      real yval
c
      write(*,*)' '
      write(*,*)'TEST15'
      write(*,*)'This program demonstrates how to use the DRAWCGM'
      write(*,*)'graphics package to create a simple line drawing'
      write(*,*)'incorporating a Cartesian grid drawn by CGRID2.'
      write(*,*)' '
c
c  Set up the X and Y vectors of data in a DO loop.
c
      xmin = 1.0
      xmax = 2.0*pi
 
      do i = 1,ndata
        x(i) = (real(ndata-i)*xmin+real(i-1)*xmax) / real(ndata-1)
        y(i) = sin(sin(x(i)))+2.0
c
c  Record the maximum and minimum Y values, for use in setting up the axes,
c  and the box around the picture.  Note that the maximum value of
c  sin(sin(X)) is sin(1.0), which is not 1, but rather .84...!
c
        if ( i.eq.1 ) then
          ymax = y(i)
          ymin = y(i)
        else
          ymax = max(ymax,y(i))
          ymin = min(ymin,y(i))
        end if
 
      end do
c
c  Choose the line color to be used.
c
      icolor = 2
      call linclr ( icolor )
c
c  Create a box around all the data.  Allow a margin of 1.0 on the
c  top and right, and 2.0 on the bottom and left.  We also use this
c  box data to tell SETSCL the maximum coordinate ranges we will
c  be using.
c
      xbox(1) = xmin-2.0
      ybox(1) = ymin-2.0
      xbox(2) = xmax+1.0
      ybox(2) = ymin-2.0
      xbox(3) = xmax+1.0
      ybox(3) = ymax+1.0
      xbox(4) = xmin-2.0
      ybox(4) = ymax+1.0
      xbox(5) = xmin-2.0
      ybox(5) = ymin-2.0
      call setscl(xbox,ybox,nbox)

      call plylin(nbox,xbox,ybox)
c
c  Draw the line of NDATA points X(I), Y(I).
c
      call plylin ( ndata, x, y )
c
c  Draw a single line segment from (0,0) to (2*PI,0), representing the X axis
c
      xval = 0.0
      yval = 0.0

      call movcgm ( xval, yval )

      xval = xmax
      yval = 0.0

      call drwcgm ( xval, yval )
c
c  Draw tick marks on the X axis.
c
      do i = 1,6
        xval = i
        yval = 0.0
        call movcgm(xval,yval+0.1)
        call drwcgm(xval,yval-0.1)
      end do
c
c  Draw a single line segment from (0.0, 0.0) to (0.0, 3.0), representing
c  the Y axis.
c
      call movcgm(0.0, 0.0)
      call drwcgm(0.0, 3.0)
c
c  Draw tick marks on the Y axis.
c
      do i = 1, 3

        xval = 0.0
        yval = real ( i )

        call movcgm ( xval-0.1, yval )

        call drwcgm ( xval+0.1, yval )

      end do
c
c  Draw a Cartesian grid.
c
      xlo = 1.0
      xhi = 6.0
      nx = 21
      ylo = 1.0
      yhi = 3.0
      ny = 9

      call cgrid2 ( xlo, xhi, nx, ylo, yhi, ny )
c
c  Write a title
c
      xval = 2.0
      yval = 0.5
      icolor = 2
      size = 0.20
      call label(xval, yval, 'Y = sin(sin(X))+2', icolor, size)
 
      xval = 2.0
      yval = 0.25
      icolor = 2
      size = 0.20
      call label(xval, yval, 'Demonstration of CGRID2', icolor, size)
 
      return
      end
      subroutine test16

c*********************************************************************72
c
cc TEST16 demonstrates the DSNLIN routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nval
      parameter (nval = 9)
c
      real angle
      real cwide
      real dshsiz
      character*1 flush
      real pwide
      character*80 string
      real x
      real xval(nval)
      real y
      real yval(nval)
c
      write(*,*)' '
      write(*,*)'TEST16'
      write(*,*)'This test uses DSHLIN to draw dashed lines.'
      write(*,*)' '
c
      call scale(0.0,6.0,0.0,6.0)
      call box(0.0,6.0,0.0,6.0)
 
      xval(1) = 1.0
      yval(1) = 1.0
      xval(2) = 2.0
      yval(2) = 1.0
      xval(3) = 3.0
      yval(3) = 2.0
      xval(4) = 3.0
      yval(4) = 3.0
      xval(5) = 2.0
      yval(5) = 3.0
      xval(6) = 2.0
      yval(6) = 4.0
      xval(7) = 3.0
      yval(7) = 5.0
      xval(8) = 4.0
      yval(8) = 5.0
      xval(9) = 4.0
      yval(9) = 1.0
 
      dshsiz = 0.2
 
      call dshlin(nval,xval,yval,dshsiz)
c
c  Label the plot.
c
      angle = 0
      cwide = 0.15
      string = 'Test of DshLin'
      pwide = 1.0
      x = 0.1
      y = -1.0
      flush = 'Center'
 
      call chrplt(angle,cwide,pwide,string,x,y,flush)
 
      return
      end
      subroutine test17

c*********************************************************************72
c
cc TEST17 demonstrates the CARC routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real pi
      real zero
c
      parameter ( pi = 3.1415926535 )
      parameter (zero = 0.0)
c
      real angmax
      real angmin
      integer i
      integer icolor
      integer n
      real radius
      real theta
      real thi
      real tlo
      real x1
      real x2
      real xcenter
      real xhi
      real xlo
      real xmax
      real xmin
      real y1
      real y2
      real ycenter
      real yhi
      real ylo
      real ymax
      real ymin
c
      write(*,*)' '
      write(*,*)'TEST17'
      write(*,*)'  A mathematical illustration.'
      write(*,*)' '
c
c  Choose the line color.
c
      icolor = 1
      call linclr ( icolor )
c
c  Set the minimum and maximum X and Y values that we intend to use.
c
      xlo = -1.0
      xhi = 1.0
      ylo = 0.0
      yhi = 1.0
c
c  Set up the coordinate system, which will be
c    -1.2 < =  X <= 1.2
c     -.2 < =  Y <= 1.1
c
      xmin = xlo-0.1*(xhi-xlo)
      xmax = xhi+0.1*(xhi-xlo)
      ymin = ylo-0.2*(yhi-ylo)
      ymax = yhi+0.1*(yhi-ylo)
 
      call scale ( xmin, xmax, ymin, ymax )
c
c  Draw a circular arc.
c
      xcenter = 0.0
      ycenter = 0.0
      radius = 1.0
      angmin = 0.0
      angmax = 180.0
 
      call carc(xcenter,ycenter,radius,angmin,angmax)
c
c  Now draw N rays.
c
      tlo = pi
      thi = 0.0
      n = 21

      do i = 1, n
 
        theta = ((n-i)*tlo+(i-1)*thi)/(n-1)

        x1 = radius*cos(theta)
        y1 = radius*sin(theta)

        call movcgm(zero,zero)

        call drwcgm(x1,y1)

        call drwcgm(x1,zero)
 
        y1 = -0.075
        call movcgm ( x1, y1 )

        y2 = -0.125
        call drwcgm ( x1, y2 )

      end do
 
      x1 = -1.0
      y1 = -0.100
      call movcgm ( x1, y1 )

      x2 =  1.0
      y2 = -0.100
      call drwcgm ( x2, y2 )
 
      return
      end
