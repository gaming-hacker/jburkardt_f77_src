c  smdprb1.f  03 October 2000
c
      program main

c*********************************************************************72
c
cc MAIN is the main program for SMDLIB_PRB1.
c
c    SMDPRB1 demonstrates semi-log and smooth curve plotting.
c
      integer npts
      parameter (npts=5)
 
      integer nstory
      parameter (nstory=100)
c
      real cycle
      real hite
      integer iframe
      integer imark
      integer istory(nstory)
      integer isym
      integer ixgrid
      integer iygrid
      integer lun
      integer maxc
      integer maxlin
      integer maxln
      integer nline
      character*1 orient
      real orig
      real pagex
      real pagey
      real slen
      real slngth
      real x(npts)
      real xblank
      real xbtext
      real xleft
      real xlen
      real xmax
      real xmin
      real xorel
      real xphys
      real xpos
      real xrite
      real xstp
      real y(npts)
      real yblank
      real ybtext
      real ylen
      real ylow
      real ymax
      real ymin
      real yorel
      real yphys
      real ypos
      real ystp
      real yup
c
      write ( *, * ) ' '
      write ( *, * ) 'SMDLIB_PRB1:'
      write ( *, * ) '  A sample program for SMDLIB.'
      write ( *, * ) ' '
c
c  Set points on the curves
c
      x(1) = 500010.0
      x(2) = 1000000.0
      x(3) = 2500000.0
      x(4) = 3500000.0
      x(5) = 4500000.0

      y(1) = 1.0
      y(2) = 10000.0
      y(3) = 10.0
      y(4) = 30000.0
      y(5) = 100.0
c
c  Set the maximum and minimum X and Y values defining the axes.
c
      xmin = 100000.0
      xmax = 4600000.0
      ymin = 1.0
      ymax = 46.0**3
c
c  Define the graphics output device to be a PostScript file,
c  in portrait orientation, written to unit 7.
c
      lun = 7
      orient = 'P'
 
      call postsc ( lun, orient )
c
c  Define the size of the page.  Although our paper is
c  presumably 8.5 by 11, we will use a "logical" paper of
c  11 by 14, and then shift the origin.
c
      write ( *, * ) 'About to call PAGE'
      pagex = 11.0
      pagey = 14.0
 
      call page ( pagex, pagey )
c
c  Now define the page origin to be at 2.5, 3.0.
c
      write ( *, * ) 'About to call ORIGIN'
      xphys = 2.5
      yphys = 3.0
 
      call origin ( xphys, yphys )
c
c  Set the current color to black.
c
c     call clrset ( 'black' )
c
c  Trying anything to get stuff to show up.
c
      call poly3
      call clrset ( 'green' )
c
c  Define the axes labels and lengths.
c
      xlen = 7.0
      ylen = 8.0
 
      call bgnsub ( 'Curve smoothing (semi-log)$', 100,
     &  'Linear X axis$', 100, ' ', 0, xlen, ylen )
c
c  Compute rounded axis limits for the logarithmic axis.
c
      call scalog ( ymin, ymax, ylen, orig, cycle )
c
c  Set up the axes.
c  The bizarre assignments of the real variables XSTP and
c  YSTP to Hollerith values are better left undiscussed.
c
      call wch2in ( 'scal', xstp )
      call wch2in ( 'scal', ystp )
 
      call axes2d ( xmin, xstp, xmax, ymin, ystp, ymax )
c
c  Draw a secondary axis of logarithmic type, a distance
c  of (0.0, 0.0) inches from the main axis.
c
      xpos = 0.0
      ypos = 0.0
 
      call xtrlgy ( orig, cycle, ylen, 'Log Y axis$', 100, xpos, ypos )
c
c  Set the text height for strings and labels.
c
      hite = 0.12
      call height ( hite )
c
c  Figure out the maximum number of lines of text that can
c  be packed into the array ISTORY, assuming that we
c  never use more than MAXC=50 characters in one line.
c
      maxc = 50
 
      maxln = maxlin ( istory, nstory, maxc )
      write ( *, * ) ' '
      write ( *, * ) 
     &  'Maximum number of story lines that can be packed is ',
     &  maxln
c
c  Now store strings to be used during plotting.
c
      call paklin ( 'Green-solid, Polynomial interpolation$', istory, 
     &  1 )
 
      call paklin ( 'Yellow-dot, Parametric spline interpolation$',
     &  istory, 2 )
 
      call paklin ( 'Cyan-chndot, Spline interpolation$', istory, 3 )
 
      call paklin ( 'White-dash, No interpolation$', istory, 4 )
c
c  Figure out the height and width required to display
c  the first four lines in ISTORY.
c
      nline = 4
 
      yblank = ybtext ( istory, nline )
      xblank = xbtext ( istory, nline )
c
c  Define blank area #1, where the story will later
c  be plotted.
c
c  IFRAME=3 asks for a frame around the box, of three
c  thicknesses.
c
      xleft = 1.9
      xrite = 2.1 + xblank
      ylow = 0.2
      yup = 0.4 + yblank
      iframe = 3
 
      call blank1 ( xleft, xrite, ylow, yup, iframe )
c
c  Set the current color to red.
c
      call clrset ( 'red' )
c
c  Draw a grid, making 1 grid line per each division on the X and Y axes.
c
      ixgrid = 1
      iygrid = 1
 
      call grid ( ixgrid, iygrid )
c
c  Plot curve 1,
c    using third order parametric polynomial interpolation,
c    using a green line, and
c    do not connect the points, show markers only.
c
      call poly3
      call clrset ( 'green' )
 
      imark = -1
 
      call curve ( x, y, npts, imark )
c
c  Plot curve 2,
c    using a parameterized spline,
c    using a yellow line,
c    connecting points, but with no markers.
c
      call prmspl
      call clrset ( 'yellow' )
      call dot
 
      imark = 0
 
      call curve ( x, y, npts, imark )
c
c  Plot curve 3,
c    using cubic splines,
c    using a cyan line,
c    using a chain dotted line,
c    connecting points and marking points.
c
      call cubspl
      call clrset ( 'cyan' )
      call chndot
 
      imark = 1
 
      call curve ( x, y, npts, imark )
c
c  Turn off the cubic spline option.
c
      call reset ( 'cubspl' )
c
c  Change the marker symbol to a triangle.
c
      isym = 3
 
      call marker ( isym )
c
c  Plot curve 4,
c    using a white line,
c    using a dashed line,
c    connecting points and marking points.
c
      call clrset ( 'foreground' )
      call dash
 
      imark = 2
 
      call curve ( x, y, npts, imark )
c
c  "Unblank" blank area number 1, so that we can plot in it.
c
      call reset ( 'blank1' )
c
c  Set the color to magenta.
c
      call clrset ( 'magenta' )
c
c  Write a block of text, the "story", consisting of
c  4 lines, centered at XPOS, YPOS.
c
      nline = 4
      xpos = 2.0
      ypos = 0.3
 
      call btextl ( istory, nline, xpos, ypos )
c
c  End the subplot.
c
      call endsub
c
c  Translate the physical origin along the vector
c  (XOREL,YOREL) from its previous position.
c
      xorel = 0.0
      yorel = - 2.0
 
      call movori ( xorel, yorel )
c
c  Specify the size of the subplot area.
c
      xlen = 7.0
      ylen = 8.0
 
      call setsub ( xlen, ylen )
c
c  Use the 'Foreground' color, that is, white.
c  (Is this going to show up, then?)
c
      call clrset ( 'foreground' )
c
c  Specify a larger text height for the next label.
c
      hite = 0.20
 
      call height ( hite )
c
c  Compute the physical length of the label.
c
      slen = slngth ( 'SMDPRB1 Sample Graph$', 100 )
c
c  Plot the label.
c
      xpos = ( xlen - slen ) / 2.0
      ypos = 0.5
 
      call messag ( 'SMDPRB1 Sample Graph$', 100, xpos, ypos )
c
c  Terminate this plot.
c
      call stoplt
c
c  Release the device.
c
      call finplt
 
      stop
      end
