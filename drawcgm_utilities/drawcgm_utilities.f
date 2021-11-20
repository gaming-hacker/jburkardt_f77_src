      subroutine arrow ( xstart, ystart, xtip, ytip )

c*********************************************************************72
c
cc ARROW draws an arrow at any point on a graph.
c
c  Discussion:
c
c    The arrow will stretch between two user specified points.
c
c    The "head" of the arrow may be fatter or thinner than expected
c    if the X and Y scales of the graph are not in the same
c    proportions.
c
c
c                       left
c                       |\
c                       | \
c      start ------- base  tip
c                       | /
c                       |/
c                       rite
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real XSTART, YSTART, the starting point for the arrow.
c
c    Input, real XTIP, YTIP, the end point for the arrow.
c
      implicit none

      real alpha
      real del
      real dist
      real pi
      parameter ( pi = 3.1415926535 )
      real theta
      real xbase
      real xleft
      real xrite
      real xstart
      real xtip
      real ybase
      real yleft
      real yrite
      real ystart
      real ytip
c
c  Return immediately if the ends of the arrow coincide.
c
      if ( xstart .eq. xtip .and. ystart .eq. ytip ) then
        return
      end if
 
      theta = 0.5 * pi - atan2(2.0,1.0)
      dist = sqrt((xtip-xstart)**2 + (ytip-ystart)**2 )
      alpha = atan2 ( ytip-ystart, xtip-xstart )
      del = sqrt(5.0) * dist / 3.0
 
      call movcgm(xstart,ystart)
 
      xbase=xstart + dist * cos(alpha) * 2.0 / 3.0
      ybase=ystart + dist * sin(alpha) * 2.0 / 3.0
      call drwcgm(xbase,ybase)
 
      xleft = xstart + del * cos(alpha-theta)
      yleft = ystart + del * sin(alpha-theta)
      call drwcgm(xleft,yleft)
 
      call drwcgm(xtip,ytip)
 
      xrite = xstart + del * cos(alpha+theta)
      yrite = ystart + del * sin(alpha+theta)
      call drwcgm ( xrite, yrite )
 
      call drwcgm ( xbase, ybase )
 
      return
      end
      subroutine arrow2 ( xstart, ystart, xtip, ytip, hsize )

c*********************************************************************72
c
cc ARROW2 draws an arrow with the arrowhead size being specified.
c
c  Discussion:
c
c    The arrow will stretch between two user specified points.
c
c    The "head" of the arrow may be fatter or thinner than expected
c    if the X and Y scales of the graph are not in the same
c    proportions.
c
c                       left
c                       |\
c                       | \
c      start ------- base  tip
c                       | /
c                       |/
c                       rite
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
c  Parameters:
c
c    Input, real XSTART, YSTART, the starting point for the arrow.
c
c    Input, real XTIP, YTIP, the end point for the arrow.
c
c    Input, real HSIZE, the relative "size" of the arrow.
c    Normally, HSIZE would be something like 0.25.  A
c    value of 0 means no arrowhead is drawn, and a value
c    of 1 means the arrow is entirely arrowhead, and no body.
c
      implicit none

      real alpha
      real hlen
      real hsize
      real xbase
      real xdel
      real xleft
      real xrite
      real xstart
      real xtip
      real ybase
      real ydel
      real yleft
      real yrite
      real ystart
      real ytip

      if ( xstart .eq. xtip .and. ystart .eq. ytip ) then
        return
      end if
 
      xbase = hsize*xstart + (1.0-hsize)*xtip
      ybase = hsize*ystart + (1.0-hsize)*ytip
 
      alpha = atan2 ( ytip-ystart, xtip-xstart )
      hlen = hsize * sqrt ( (xstart-xtip)**2+(ystart-ytip)**2 )
 
      xdel = hlen * sin(alpha)
      ydel = hlen * cos(alpha)
 
      xleft = xbase - xdel
      yleft = ybase + ydel
 
      xrite = xbase + xdel
      yrite = ybase - ydel
 
      call movcgm ( xstart, ystart )
      call drwcgm ( xbase, ybase )
      call drwcgm ( xleft, yleft )
      call drwcgm ( xtip, ytip )
      call drwcgm ( xrite, yrite )
      call drwcgm ( xbase, ybase )
 
      return
      end
      subroutine box ( xmin, xmax, ymin, ymax )

c*********************************************************************72
c
cc BOX draws a rectangle with specified corners.
c
c  Discussion:
c
c    The rectangle drawn by BOX has the corners:
c
c      (XMIN,YMAX)   (XMAX,YMAX)
c
c      (XMIN,YMIN)   (XMAX,YMIN)
c
c    BOX can be used to place a rectangle anywhere in the picture.
c    However, BOX may also be used to place a rectangle around the
c    entire picture, producing a "frame".
c
c    The DRAWCGM routine PLYLIN is used to draw the box, and hence
c    the color of the line may be set by calling the DRAWCGM routine
c    LINCLR first.
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
c  Parameters:
c
c    Input, real XMIN, XMAX, the minimum and maximum X
c    coordinates of the box.
c
c    Input, real YMIN, YMAX, the minimum and maximum Y
c    coordinates of the box.
c
      implicit none

      integer npoints
      parameter ( npoints = 5 )

      real x(npoints)
      real xmax
      real xmin
      real y(npoints)
      real ymax
      real ymin

      x(1) = xmin
      y(1) = ymin
 
      x(2) = xmax
      y(2) = ymin
 
      x(3) = xmax
      y(3) = ymax
 
      x(4) = xmin
      y(4) = ymax
 
      x(5) = xmin
      y(5) = ymin
 
      call plylin ( npoints, x, y )
 
      return
      end
      subroutine box01 ( )

c*********************************************************************72
c
cc BOX01 draws the unit square.
c
c  Discussion:
c
c    If the user is employing the standard DRAWCGM coordinate system,
c    then BOX01 will draw a box around the entire picture.  You could
c    get the same effect by calling:
c
c      call BOX(0.0,1.0,0.0,1.0)
c
c    so BOX01 is simply a "convenience" routine.
c
c    The square drawn by BOX01 has the corners:
c
c      (0,1)  (1,1)
c
c      (0,0)  (1,0)
c
c    The DRAWCGM routine PLYLIN is used to draw the box, and hence
c    the color of the line may be set by calling the DRAWCGM routine
c    LINCLR first.
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
c  Parameters:
c
c    None
c
      implicit none

      integer npoints
      parameter ( npoints = 5 )

      real x(npoints)
      real y(npoints)

      x(1) = 0.0
      y(1) = 0.0
 
      x(2) = 1.0
      y(2) = 0.0
 
      x(3) = 1.0
      y(3) = 1.0
 
      x(4) = 0.0
      y(4) = 1.0
 
      x(5) = 0.0
      y(5) = 0.0
 
      call plylin ( npoints, x, y )
 
      return
      end
      subroutine carc ( xcenter, ycenter, radius, angmin, angmax )

c*********************************************************************72
c
cc CARC draws a circular arc of a given angular size and radius.
c
c  Discussion:
c
c    CARC calls the DRAWCGM routine PLYLIN to draw the circular arc.
c
c    To control the color of the line with which the circle is drawn,
c    simply call the DRAWCGM routine LINCLR before calling CARC.
c
c    if the X and Y coordinate systems do not have the same
c    scale, the arc drawn will be distorted.  This can happen if the
c    routine SETWCD has been used to set the X and Y dimensions to
c    different extents.
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
c  Parameters:
c
c    Input, real XCENTER, YCENTER, the X and Y coordinates of
c    the center of the circle on which the arc lies.
c
c    Input, real RADIUS, the radius of the circle on which the arc lies.
c
c    Input, real ANGMIN, ANGMAX, the minimum and maximum
c    angles of the circle.  ANGMIN and ANGMAX are both
c    measured in degrees.  ANGMIN and ANGMAX determine the
c    portion of the circle to be drawn.  If ANGMIN=0.0 and
c    ANGMAX=90.0, for instance, an arc of 90 degrees will be drawn.
c
      implicit none

      integer nval
      parameter ( nval = 65 )

      real pi
      parameter ( pi = 3.1415926535 )

      real degrad
      parameter ( degrad = pi / 180.0 )

      real angle
      real angmax
      real angmin
      integer i
      real radius
      real xcenter
      real xval(nval)
      real ycenter
      real yval(nval)

      if ( radius .le. 0.0 ) then
        return
      end if
c
c  Set up the data defining the circular arc, using NVAL equally
c  spaced points along the circumference.
c
      do i = 1, nval
 
        if ( nval .eq. 1 ) then
          angle = 0.5 * ( angmax + angmin )
        else
          angle = ( (nval-i)*angmin + (i-1)*angmax ) / real(nval-1)
        end if
 
        xval(i) = xcenter + radius * cos(degrad*angle)
        yval(i) = ycenter + radius * sin(degrad*angle)
 
      end do
c
c  Draw the circular arc.
c
      call plylin ( nval, xval, yval )
 
      return
      end
      subroutine cbox ( )

c*********************************************************************72
c
cc CBOX draws a 16 by 16 box of colors in the current color table.
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
c  Parameters:
c
c    None
c
      implicit none

      integer npoly
      parameter ( npoly = 5 )

      real angle
      real cwide
      character*6 flush
      real grace
      integer i
      integer icolor
      integer ierror
      integer j
      real pwide
      character*3 string
      real x(npoly)
      real x1max
      real x1min
      real x2max
      real x2min
      real xtemp
      real y(npoly)
      real y1max
      real y1min
      real y2max
      real y2min
      real ytemp
c
c  Set the coordinate system to be 0 < =  X <= 16.0,
c  and 0.0 < =  Y <= 16.0
c
      x2min = 0.0
      x2max = 16.0
      y2min = 0.0
      y2max = 16.0

      grace = 0.05
 
      x1min = x2min - grace * (x2max-x2min)
      x1max = x2max + grace * (x2max-x2min)
      y1min = y2min - grace * (y2max-y2min)
      y1max = y2max + grace * (y2max-y2min)

      ierror = 0
      call setwcd ( x1min, y1min, x1max, y1max, ierror )
c
c  Draw the color boxes.
c
      icolor = 0
 
      do i = 1, 16
 
        y(1) = 16-i
        y(2) = 16-i
        y(3) = 17-i
        y(4) = 17-i
 
        do j = 1, 16
 
          call filclr ( icolor )
          icolor = icolor + 1
 
          x(1) = j-1
          x(2) = j
          x(3) = j
          x(4) = j-1
 
          call plygon ( 4, x, y )
 
        end do
      end do
c
c  Draw black lines around the boxes.
c
      icolor = 1
      call linclr ( icolor )
 
      do i = 1, 16
 
        y(1) = 16-i
        y(2) = 17-i
        y(3) = 17-i
        y(4) = 16-i
        y(5) = y(1)
 
        do j = 1, 16
 
          x(1) = j-1
          x(2) = j-1
          x(3) = j
          x(4) = j
          x(5) = x(1)
 
          call plylin ( 5, x, y )
 
        end do
      end do
c
c  Print numeric indices.
c
      icolor = 1
      call linclr ( icolor )
 
      icolor = 0

      do i = 1, 16
 
        y(1) = 16-i
        y(2) = 17-i
        y(3) = 17-i
        y(4) = 16-i
        y(5) = y(1)
 
        do j = 1, 16

          x(1) = j-1
          x(2) = j-1
          x(3) = j
          x(4) = j
          x(5) = x(1)

          write ( string, '(i3)' ) icolor

          angle = 0.0
          cwide = 0.015 * ( x2max - x2min )
          pwide = x2max - x2min
          xtemp = real(j) - 0.5
          ytemp = 16.5 - real(i)
          flush = 'center'

          call chrplt ( angle, cwide, pwide, string, xtemp, ytemp,
     &      flush )

          icolor = icolor + 1
 
          call plylin ( 5, x, y )
 
        end do

      end do

      return
      end
      subroutine cgrid2 ( xmin, xmax, nx, ymin, ymax, ny )

c*********************************************************************72
c
cc CGRID2 draws evenly spaced Cartesian X and Y grid lines.
c
c  Discussion:
c
c    The color used to draw the grid lines may be changed by calling
c    LINCLR before calling this routine.
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
c  Parameters:
c
c    Input, real XMIN, XMAX, are the horizontal limits of the location
c    of the grid.
c
c    XMIN and XMAX should be "within" your picture.  If you are using
c    the simple DRAWCGM coordinate system, which uses 0 as the minimum
c    X value and 1 as the maximum X value, then XMIN should be 0 or
c    greater, and XMAX should be 1 or less.
c
c    However, if you have used SETWCD or SETSCL to allow a different
c    range of X values, then XMIN and XMAX may be any values in that
c    range.
c
c    Similar remarks apply to the YMIN and YMAX values.
c
c    Input, integer NX, the number of grid lines to draw along the
c    X direction.
c
c    Input, real YMIN, YMAX, are the vertical limits of the location
c    of the grid.
c
c    Input, integer NY, the number of grid lines to draw along the
c    Y direction.
c
      implicit none

      integer i
      integer nx
      integer ny
      real xmax
      real xmin
      real xval
      real ymax
      real ymin
      real yval

      do i = 1, nx
 
        if ( nx .ne. 1 ) then
          xval = ( (nx-i)*xmin + (i-1)*xmax ) / real(nx-1)
        else
          xval = 0.5 * ( xmin + xmax )
        end if
 
        call movcgm ( xval, ymin )
        call drwcgm ( xval, ymax )
 
      end do
 
      do i = 1, ny
 
        if ( ny .ne. 1 ) then
          yval = ( (ny-i)*ymin + (i-1)*ymax ) / real(ny-1)
        else
          yval = 0.5 * (ymin+ymax)
        end if
 
        call movcgm ( xmin, yval )
        call drwcgm ( xmax, yval )
 
      end do
 
      return
      end
      subroutine chrplt ( angle, cwide, pwide, string, x, y, flush )

c*********************************************************************72
c
cc CHRPLT plots a character string onto a graphics image.
c
c  Discussion:
c
c    The string may be plotted at any angle and at any size.  The plot is
c    assumed to be of size PWIDE by PHITE, although PHITE itself is
c    not input.
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
c  Parameters:
c
c    Input, real ANGLE, the angle in degrees at which the
c    string is to be drawn.  0 is typical.  90 degrees would
c    cause the string to be written from top to bottom.
c
c    Input, real CWIDE, the width of the characters. This
c    is measured in the same units as the plot width PWIDE.
c    For PWIDE=1, a plot size of 0.025 would be reasonable,
c    since 40 characters would fit, but 2.0 would be nonsense.
c
c    Input, real PWIDE, the width of the plot, in the same
c    units as CWIDE.
c
c    Input, character*(*) STRING, contains the information to
c    be plotted.
c
c    Input, real X, Y.  The coordinates of a point which
c    determines where the string is drawn.  The string will
c    be drawn starting at, centered or, or ending at (X,Y),
c    depending on the value of FLUSH.
c
c    Input, character*(*) FLUSH, a string which tells
c    CHRPLT how to place the string.  Only the first
c    character of FLUSH is examined, and the case of
c    the character is not important.
c    'L' - the string will be drawn flush left.
c    'C' - the string will be centered.
c    'R' - the string will be drawn flush right.
c
      implicit none

      real pi
      parameter ( pi = 3.1415926535 )

      real degrad
      parameter ( degrad = pi / 180.0 )

      real angle
      real ca
      character*1 chrtmp
      real cwide
      character*(*) flush
      integer i
      integer iascii
      integer icr
      integer ifont(1617)
      integer ip
      integer ipen
      integer ipoint(95)
      integer iv
      integer lenchr
      integer nchar
      integer nmax
      integer nvec
      real pwide
      logical rotate
      real sa
      real scl2
      character*(*) string
      real x
      real xb
      real xc
      real xcopy
      real xnew
      real xold
      real xrot
      real xt
      real y
      real yb
      real yc
      real ycopy
      real ynew
      real yold
      real yrot
      real yt

      external lenchr

      save ifont
      save ipoint
c
c  IPOINT is a pointer array into IFONT.
c
c  IPOINT(I) records where the "strokes" for character I begin
c  in the IFONT array.
c
      data (ipoint(i),i=1,95) /
     &   1,   3,  26,  45,  66, 102, 130, 156, 166, 186, 206, 222, 233,
     & 249, 255, 267, 273, 293, 306, 328, 353, 363, 383, 411, 423, 457,
     & 483, 506, 533, 541, 552, 560, 587, 625, 638, 665, 683, 699, 714,
     & 727, 754, 770, 786, 805, 818, 826, 838, 848, 868, 884, 909, 930,
     & 956, 967, 981, 989,1001,1012,1025,1035,1045,1051,1061,1069,1075,
     &1081,1108,1131,1149,1172,1194,1214,1243,1260,1284,1307,1323,1336,
     &1364,1381,1401,1424,1447,1464,1486,1499,1516,1524,1536,1547,1560,
     &1570,1586,1592,1608/
c
c  IFONT contains the strokes defining the various symbols.
c
      data (ifont(i),i=   1, 396)/
     & 1, 0, 2,10,11, 9,22,10,23,11,22,10,11, 0, 9, 7, 9, 9,11, 9,11, 7,
     & 9, 7, 0, 2, 8,17, 7,23, 9,23, 8,17, 0,14,17,13,23,15,23,14,17, 0,
     & 4, 9,23, 7, 7, 0,13,23,11, 7, 0, 5,17,15,17, 0, 5,13,15,13, 0, 3,
     &15,19,13,21, 9,21, 7,19, 7,17, 9,15,13,15,15,13,15,11,13, 9, 9, 9,
     & 7,11, 0, 9,23, 9, 7, 0,13,23,13, 7, 0, 3, 5,23, 9,23, 9,19, 5,19,
     & 5,23, 0,17,23, 5, 7, 0,13, 7,13,11,17,11,17, 7,13, 7, 0, 1,17, 7,
     & 7,17, 7,19, 9,21,13,21,15,19,15,17, 5,13, 5,11, 9, 7,13, 7,17,15,
     & 0, 1,10,17, 9,23,11,23,10,17, 0, 1,12,23,11,21,10,19, 9,17, 9,15,
     & 9,13,10,11,11, 9,12, 7, 0, 1,12,23,13,21,14,19,15,17,15,15,15,13,
     &14,11,13, 9,12, 7, 0, 3, 7,15,15,15, 0,13,19, 9,11, 0, 9,19,13,11,
     & 0, 2, 7,15,15,15, 0,11,19,11,11, 0, 1,11, 7, 9, 7, 9, 9,11, 9,11,
     & 7,11, 6,10, 4, 0, 1, 7,15,15,15, 0, 1, 9, 7, 9, 9,11, 9,11, 7, 9,
     & 7, 0, 1,15,23, 7, 7, 0, 1, 9,23,13,23,15,19,15,11,13, 7, 9, 7, 7,
     &11, 7,19, 9,23, 0, 2, 7,21, 9,23, 9, 7, 0, 7, 7,11, 7, 0, 1, 5,21,
     & 9,23,15,23,17,21,17,19,15,17, 7,13, 5,10, 5, 7,17, 7, 0, 2, 5,23,
     &17,23,15,17,13,15, 9,15, 0,13,15,17,13,17,10,14, 7, 8, 7, 5,10, 0,
     & 1,13, 7,13,23, 5,13,17,13, 0, 1,17,23, 5,23, 5,17,13,17,17,15,17,
     &11,13, 7, 9, 7, 5,11, 0, 1,17,19,13,23, 9,23, 5,19, 5,13, 9,15,13/
      data (ifont(i),i= 397, 792)/
     &15,17,13,17,11,13, 7, 9, 7, 5,11, 5,13, 0, 1, 5,19, 5,23,17,23,11,
     &15,11, 7, 0, 1, 8,15, 6,17, 6,21, 8,23,14,23,16,21,16,17,14,15, 8,
     &15, 5,13, 5, 9, 8, 7,14, 7,17, 9,17,13,14,15, 0, 1,17,17,15,15, 7,
     &15, 5,17, 5,21, 7,23,15,23,17,21,17,11,15, 7, 7, 7, 5,11, 0, 2, 9,
     &13, 9,15,11,15,11,13, 9,13, 0, 9, 7, 9, 9,11, 9,11, 7, 9, 7, 0, 2,
     & 9,13, 9,15,11,15,11,13, 9,13, 0,11, 7, 9, 7, 9, 9,11, 9,11, 7,11,
     & 6,10, 4, 0, 1,17,21, 5,15,17, 9, 0, 2, 7,15,15,15, 0, 7, 9,15, 9,
     & 0, 1, 5,21,17,15, 5, 9, 0, 2, 7,21, 9,23,13,23,15,21,15,19,11,15,
     &11,11, 0,10, 7,10, 9,12, 9,12, 7,10, 7, 0, 1,13, 7, 9, 7, 5,11, 5,
     &19, 9,23,13,23,17,19,17,11,15, 9,13,11,12,10,10,10, 9,11, 9,15,10,
     &16,12,16,13,15,13,11, 0, 2, 5, 7,11,23,17, 7, 0, 8,15,14,15, 0, 2,
     & 5, 7, 5,23,15,23,17,21,17,17,15,15, 5,15, 0,15,15,17,13,17, 9,15,
     & 7, 5, 7, 0, 1,17,19,13,23, 9,23, 5,19, 5,11, 9, 7,13, 7,17,11, 0,
     & 1, 5, 7, 5,23,13,23,17,19,17,11,13, 7, 5, 7, 0, 2,17,23, 5,23, 5,
     & 7,17, 7, 0, 5,15,12,15, 0, 2, 5, 7, 5,23,17,23, 0, 5,15,12,15, 0,
     & 2,17,19,13,23, 9,23, 5,19, 5,11, 9, 7,13, 7,17,11,17,15,13,15, 0,
     &17,11,17, 7, 0, 3, 5, 7, 5,23, 0, 5,15,17,15, 0,17,23,17, 7, 0, 3,
     & 9,23,13,23, 0,11,23,11, 7, 0, 9, 7,13, 7, 0, 2,15,23,15,11,12, 7/
      data (ifont(i),i= 793,1188)/
     & 8, 7, 5,11, 5,13, 0,13,23,17,23, 0, 2, 5, 7, 5,23, 0,17,23, 5,15,
     &17, 7, 0, 1, 5,23, 5, 7,17, 7, 0, 1, 5, 7, 5,23,11,11,17,23,17, 7,
     & 0, 1, 5, 7, 5,23,17, 7,17,23, 0, 1,17,19,13,23, 9,23, 5,19, 5,11,
     & 9, 7,13, 7,17,11,17,19, 0, 1, 5, 7, 5,23,13,23,17,21,17,17,13,15,
     & 5,15, 0, 2,17,19,13,23, 9,23, 5,19, 5,11, 9, 7,13, 7,17,11,17,19,
     & 0,13,11,17, 7, 0, 2, 5, 7, 5,23,13,23,17,21,17,17,13,15, 5,15, 0,
     &13,15,17, 7, 0, 1,17,19,13,23, 9,23, 5,20, 5,18, 9,15,13,15,17,12,
     &17,10,13, 7, 9, 7, 5,10, 0, 2, 5,23,17,23, 0,11,23,11, 7, 0, 1, 5,
     &23, 5,10, 8, 7,14, 7,17,10,17,23, 0, 1, 5,23,11, 7,17,23, 0, 1, 5,
     &23, 8, 7,11,17,14, 7,17,23, 0, 2, 5,23,17, 7, 0,17,23, 5, 7, 0, 2,
     & 5,23,11,13,17,23, 0,11,13,11, 7, 0, 1, 5,23,17,23, 5, 7,17, 7, 0,
     & 1,11,23, 7,23, 7, 7,11, 7, 0, 1, 7,23,15, 7, 0, 1, 7,23,11,23,11,
     & 7, 7, 7, 0, 1, 7,21,11,23,15,21, 0, 1, 5, 3,17, 3, 0, 1, 9,23,13,
     &19, 0, 2, 7,14, 9,15,13,15,15,14,15, 7, 0,15,12, 9,12, 7,11, 7, 8,
     & 9, 7,13, 7,15, 8, 0, 2, 7,23, 7, 7, 0, 7,13, 9,15,13,15,15,13,15,
     & 9,13, 7, 9, 7, 7, 9, 0, 1,15,13,13,15, 9,15, 7,13, 7, 9, 9, 7,13,
     & 7,15, 9, 0, 2,15,13,13,15, 9,15, 7,13, 7, 9, 9, 7,13, 7,15, 9, 0,
     &15,23,15, 7, 0, 1, 7,11,15,11,15,13,13,15, 9,15, 7,13, 7, 9, 9, 7/
      data (ifont(i),i=1189,1584)/
     &13, 7,15, 9, 0, 3, 9, 7, 9,23,13,23,13,22, 0, 8,15,12,15, 0, 8, 7,
     &11, 7, 0, 2,15,13,13,15, 9,15, 7,13, 7, 9, 9, 7,13, 7,15, 9, 0,15,
     &13,15, 3,13, 1, 9, 1, 7, 3, 0, 2, 7, 7, 7,23, 0, 7,14, 9,15,13,15,
     &15,14,15, 7, 0, 3, 9,15,11,15,11, 7, 0, 9, 7,13, 7, 0, 9,17, 9,19,
     &11,19,11,17, 9,17, 0, 2, 9,15,11,15,11, 1, 7, 1, 7, 3, 0, 9,17,11,
     &17,11,19, 9,19, 9,17, 0, 3, 7, 7, 7,23, 0,15,15, 7,10, 0, 9,11,15,
     & 7, 0, 2, 9,23,11,23,11, 7, 0, 9, 7,13, 7, 0, 3, 7,15, 7, 7, 0, 7,
     &14, 8,15,10,15,11,14,11, 7, 0,11,14,12,15,14,15,15,14,15, 7, 0, 2,
     & 7, 7, 7,15, 0, 7,14, 9,15,13,15,15,14,15, 7, 0, 1, 7,13, 9,15,13,
     &15,15,13,15, 9,13, 7, 9, 7, 7, 9, 7,13, 0, 2, 7,13, 9,15,13,15,15,
     &13,15, 9,13, 7, 9, 7, 7, 9, 0, 7,14, 7, 1, 0, 2,15,13,13,15, 9,15,
     & 7,13, 7, 9, 9, 7,13, 7,15, 9, 0,15,14,15, 1, 0, 2, 7,15, 9,15, 9,
     & 7, 0, 9,13,11,15,13,15,15,13, 0, 1,15,13,13,15, 9,15, 7,13, 9,11,
     &13,11,15, 9,13, 7, 9, 7, 7, 9, 0, 2, 9,23, 9, 7,11, 7, 0, 7,17,11,
     &17, 0, 2, 7,15, 7, 9, 9, 7,13, 7,15, 9, 0,15,15,15, 7, 0, 1, 7,15,
     &11, 7,15,15, 0, 1, 7,15, 9, 7,11,11,13, 7,15,15, 0, 2, 7,15,15, 7,
     & 0, 7, 7,15,15, 0, 2, 7,15,11, 7, 0,15,15,10, 5, 7, 1, 0, 1, 7,15,
     &15,15, 7, 7,15, 7, 0, 1,11,23, 7,23, 9,17, 7,15, 9,13, 7, 7,11, 7/
      data (ifont(i),i=1585,1617)/
     & 0, 1, 9,23, 9, 7, 0, 1, 7,23,11,23, 9,17,11,15, 9,13,11, 7, 7, 7,
     & 0, 1, 5,21, 7,23,15,21,17,23, 0/

      nchar=lenchr(string)
 
      if (pwide.le.0)then
        write(*,*)'ChrPlt - Serious error!'
        write(*,*)'  The plot width PWIDE is negative!'
        write(*,*)'  PWIDE=',pwide
        return
      end if
c
c  Chop titles that are too long.  To do this, we need to know the
c  width of the plot (PWIDE) in same units as CWIDE.
c
      nmax=ifix(pwide/cwide)
      if (nchar.gt.nmax)nchar=nmax
c
c  Shift string if centering or right flush option used.
c
      if (flush(1:1).eq.'l'.or.flush(1:1).eq.'L')then
        xcopy=x
        ycopy=y
      elseif (flush(1:1).eq.'c'.or.flush(1:1).eq.'C')then
        xcopy=x-0.5*nchar*cwide*cos(angle*degrad)
        ycopy=y-0.5*nchar*cwide*sin(angle*degrad)
      elseif (flush(1:1).eq.'r'.or.flush(1:1).eq.'R')then
        xcopy=x-nchar*cwide*cos(angle*degrad)
        ycopy=y-nchar*cwide*sin(angle*degrad)
      else
        xcopy=x
        ycopy=y
      end if
c
c  Note that screen coordinates are used.
c  Thus a width of 0.1 is intended to mean 1/10 of screen size.
c
c  Set the scale factor for character height.
c
      scl2=cwide/16.0
c
c  Set the starting point for the line of text, the lower left
c  corner of the first character.
c
c  Set the origin about which rotation is performed.
c
      xb=xcopy
      xrot=xcopy
      yb=ycopy
      yrot=ycopy
c
c  Get trig functions if rotation required, converting from
c  degrees to radians.
c
      if (angle.eq.0.0)then
        rotate=.false.
      else
        ca=cos(angle*degrad)
        sa=sin(angle*degrad)
        rotate=.true.
      end if
c
c  Loop over all characters in the string.
c
      do icr=1, nchar
 
        xold=x
        yold=y
        xnew=x
        ynew=y
c
c  Get the ASCII code for the character and shift by 31 so that
c  the first printable character becomes code 1.
c
        chrtmp=string(icr:icr)
        iascii=ichar(chrtmp)-31
c
c  Replace any nonprintable characters with blanks.
c
        if ((iascii.lt.1).or.(iascii.gt.95))iascii=1
c
c  Get the pointer to this character in font table.
c
        ip=ipoint(iascii)
c
c  Get the number of "vectors" required to draw the character.
c  Here "vectors" means the number of times the pen is lowered, not
c  the number of pen strokes.
c
c  For blanks, this number is 1, due to the way the
c  algorithm is coded.
c
        nvec=ifont(ip)
c
c  Loop over all required pen movements.
c
        do iv=1,nvec
          ipen=3
          ip=ip+1
10        continue
          if (ifont(ip).eq.0)go to 20
          xc=xb+(ifont(ip)-1)*scl2
          yc=yb+(ifont(ip+1)-7)*scl2
c
c  Apply rotation if necessary.
c
          if (rotate)then
            xt=xc-xrot
            yt=yc-yrot
            xc=ca*xt-sa*yt+xrot
            yc=sa*xt+ca*yt+yrot
          end if
c
c  Plot the pen stroke.
c
          if (ipen.eq.3)then
            xnew=xc
            ynew=yc
          else
            xold=xnew
            yold=ynew
            xnew=xc
            ynew=yc
c
c  Call the user supplied routine to draw a line from
c  (XOLD,YOLD) to (XNEW,YNEW).
c
            call movcgm(xold,yold)
            call drwcgm(xnew,ynew)
 
          end if
 
          ipen=2
          ip=ip+2
          go to 10
 
20        continue
 
        end do
c
c  Advance the base to compensate for character just drawn.
c
        xb = xb + cwide
 
      end do
 
      return
      end
      subroutine cirkle ( xcentr, ycentr, radius, filled )

c*********************************************************************72
c
cc CIRKLE draws an open or filled circle of a given radius.
c
c  Discussion:
c
c    CIRKLE calls PLYLIN to draw an open circle, or PLYGON to draw a
c    filled circle.
c
c    To control the color of the circle, simply call the DRAWCGM
c    routine LINCLR before drawing open circles or FILCLR before
c    drawing closed circles.
c
c    If the X and Y coordinate systems do not have the same scale,
c    the circle drawn will be "flattened".  This can happen if the
c    routine SETWCD has been used to set the X and Y dimensions to
c    different extents.
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
c  Parameters:
c
c    Input, real XCENTR, YCENTR, the X and Y coordinates of
c    the center of the circle.
c
c    Input, real RADIUS, the radius of the circle.
c
c    Input, LOGICAL FILLED,
c    .TRUE. if the circle is to be filled.
c    .FALSE. if the circle is to be open.
c
c
c  To make a "smoother" circle, try increasing the value of NPTS.
c
      implicit none

      integer npts
      parameter ( npts = 64 )

      real angle
      integer i
      logical filled
      real pi
      parameter ( pi = 3.1415926535E+00 )
      real radius
      real xcentr
      real xpoint(npts)
      real ycentr
      real ypoint(npts)
c
c  Set up the data defining the circle, using NPTS equally spaced
c  points along the circumference.  The first and the last points
c  are the same.
c
      do i = 1, npts
        angle = 2.0E+00 * pi * real ( i - 1 ) / real ( npts - 1 )
        xpoint(i) = xcentr + radius * sin(angle)
        ypoint(i) = ycentr + radius * cos(angle)
      end do
c
c  Draw the circle.
c
      if ( filled ) then
        call plygon ( npts-1, xpoint, ypoint )
      else
        call plylin ( npts, xpoint, ypoint )
      end if
 
      return
      end
      subroutine clip(xa,ya,xb,yb,xc,yc,xd,yd,idraw,x0,y0,x1,y1)

c*********************************************************************72
c
cc CLIP determines the portion of a line that lies within a rectangle.
c
c  Discussion:
c
c    CLIP accepts a line defined by a pair of points, and a rectangle
c    defined by its corners, and computes the portion of the line which
c    is interior to the rectangle.
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
c  Parameters:
c
c    Input, real XA, YA, XB, YB, the X and Y coordinates of the
c    endpoints of the line.
c
c    Output, real XC, YC, XD, YD, the X and Y coordinates of the
c    "clipped" line.  However, if IDRAW is 0, this should be ignored,
c    since no portion of the line is within the rectangle.
c
c    Output, integer IDRAW.
c    0 if no portion of the line is within the rectangle.
c    1 if there is a portion of the line within the rectangle, in
c    which case that portion is defined by (XC,YC) and (XD,YD).
c
c    Input, real X0, Y0, X1, Y1, the X and Y coordinates of the
c    rectangle's lower left, and upper right corners.
c
      implicit none

      integer idraw
      real x0
      real x1
      real xa
      real xb
      real xc
      real xd
      real xval(2)
      real y0
      real y1
      real ya
      real yb
      real yc
      real yd
      real yval(2)

      call clip1 ( xa, ya, xb, yb, xval, yval, idraw, x0, y0, x1, y1 )
 
      xc = xval(1)
      yc = yval(1)
      xd = xval(2)
      yd = yval(2)
 
      return
      end
      subroutine clip1 ( xa, ya, xb, yb, xval, yval, idraw, x0, y0,
     &  x1, y1 )

c*********************************************************************72
c
cc CLIP1 is used by CLIP.
c
c  Discussion:
c
c    CLIP1 clips the line from (XA,YA) to (XB,YB), returning the clipped values
c    in (XVAL(1),YVAL(1)), (XVAL(2),YVAL(2)).
c
c    The clipping box is bounded by (X0,Y0) and (X1,Y1).
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
c  Parameters:
c
c    Input, real XA, YA, XB, YB, the X and Y coordinates of the
c    endpoints of the line.
c
c    Output, real XVAL(2), YVAL(2), the X and Y coordinates of the
c    "clipped" line.  However, if IDRAW is 0, this should be ignored,
c    since no portion of the line is within the rectangle.
c
c    Output, integer IDRAW.
c    0 if no portion of the line is within the rectangle.
c    1 if there is a portion of the line within the rectangle, in
c    which case that portion is defined by (XC,YC) and (XD,YD).
c
c    Input, real X0, Y0, X1, Y1, the X and Y coordinates of the
c    rectangle's lower left, and upper right corners.
c
      implicit none

      integer idraw
      integer next
      real x
      real x0
      real x00
      real x1
      real x11
      real xa
      real xb
      real xval(2)
      real y
      real y0
      real y00
      real y1
      real y11
      real ya
      real yb
      real yval(2)

      idraw = 1
 
      x00=min(x0,x1)
      x11=max(x0,x1)
      y00=min(y0,y1)
      y11=max(y0,y1)
 
      next=0
c
c  See if (XA,YA) is within the box.
c
      if (
     &  x00 .le. xa .and.
     &  xa .le. x11 .and.
     &  y00 .le. ya .and.
     &  ya .le. y11 ) then
        next = next+1
        xval(next) = xa
        yval(next) = ya
      end if
c
c  See if (XB,YB) is within the box.
c
      if (
     &  x00 .le. xb .and. xb .le. x11 .and.
     &  y00 .le. yb .and. yb .le. y11 ) then
        next=next+1
        xval(next)=xb
        yval(next)=yb
      end if
c
c  If both endpoints are interior to the box, no clipping is needed.
c
      if ( next .eq. 2 ) then
        return
      end if
c
c  Try to clip with respect to each of the four borders.
c
      call clip2(xa,ya,xb,yb,x00,y00,y11,y,xval,yval,next)
 
      if ( next .eq. 2 ) then
        return
      end if
 
      call clip2(xa,ya,xb,yb,x11,y00,y11,y,xval,yval,next)
 
      if ( next .eq. 2 ) then
        return
      end if
 
      call clip2(ya,xa,yb,xb,y00,x00,x11,x,yval,xval,next)
 
      if ( next .eq. 2 ) then
        return
      end if
 
      call clip2(ya,xa,yb,xb,y11,x00,x11,x,yval,xval,next)
 
      if ( next .lt. 2 ) then
        idraw = 0
      end if
 
      return
      end
      subroutine clip2(xa,ya,xb,yb,x00,y00,y11,y,xval,yval,next)
c
c*********************************************************************72
c
cc CLIP2 is used by CLIP1.
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
c  Parameters:
c
      implicit none

      integer next
      real t
      real x00
      real xa
      real xb
      real xval(2)
      real y
      real y00
      real y11
      real ya
      real yb
      real yval(2)

      if ( xb .eq. xa ) then
        return
      end if
c
c  The value of T measures where the border value X00 lies;
c  0.0 <= T <= 1.0 implies XA <= X00 <= XB.
c
      t = (x00-xa) / (xb-xa)
c
c  If 0.0 <= T <= 1.0, then the border intersects the line.
c  Find the Y coordinate of that intersection.
c
      if ( t .ge. 0.0 .and. t .le. 1.0 ) then
 
        y = (1.0-t) * ya + t * yb
c
c  If the Y coordinate also lies between the Y borders, then the
c  intersection of the line and the border line is inside the clipping
c  box, and we have found one end of the visible, clipped line.
c
        if ( y00 .le. y .and. y .le. y11 ) then
          next = next + 1
          xval(next) = x00
          yval(next) = y
        end if
 
      end if
 
      return
      end
      subroutine cubspl(tau,c,n,ibcbeg,ibcend)

c*********************************************************************72
c
cc CUBSPL defines a cubic spline for given data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Parameters:
c
c    Input, real TAU(N), the abscissas or X values of the
c    data points.  The entries of TAU are assumed to be
c    strictly increasing.
c
c    Input, integer N, the number of data points.  N is
c    assumed to be at least 2.
c
c    Input/output, real C(4,N).
c    On input, if IBCBEG or IBCBEG is 1 or 2, then C(2,1)
c    or C(2,N) should have been set to the desired derivative
c    values, as described further under IBCBEG and IBCEND.
c    On output, C contains the polynomial coefficients of
c    the cubic interpolating spline with interior knots
c    TAU(2) through TAU(N-1).
c    In the interval interval (TAU(I), TAU(I+1)), the spline
c    F is given by
c      F(X) = C(1,I)+H*C(2,I)+(1/2)*H*H*C(3,I)
c             +(1/6)*H*H*H*C(4,I)
c    where H = X - TAU(I).  The routine PPVALU may be used to
c    evaluate F or its derivatives from TAU, C, L = N-1,
c    and K=4.
c
c    Input, integer IBCBEG, IBCEND, boundary condition indicators.
c    IBCBEG=0 means no boundary condition at TAU(1) is given.
c    In this case, the "not-a-knot condition" is used.  That
c    is, the jump in the third derivative across TAU(2) is
c    forced to zero.  Thus the first and the second cubic
c    polynomial pieces are made to coincide.
c    IBCBEG=1 means that the slope at TAU(1) is to equal the
c    input value C(2,1).
c    IBCBEG=2 means that the second derivative at TAU(1) is
c    to equal C(2,1).
c    IBCEND=0, 1, or 2 has analogous meaning concerning the
c    boundary condition at TAU(N), with the additional
c    information taken from C(2,N).
c
      implicit none

      integer n

      real c(4,n)
      real divdf1
      real divdf3
      real dtau
      real g
      integer i
      integer ibcbeg
      integer ibcend
      integer j
      integer l
      integer m
      real tau(n)
c
c  A tridiagonal linear system for the unknown slopes S(I) of
c  F at TAU(I), I=1,..., N, is generated and then solved by Gauss
c  elimination, with S(I) ending up in C(2,I), for all I.
c
c  C(3,*) and C(4,*) are used initially for temporary storage.
c
      l=n-1
c
c  Compute first differences of the TAU sequence and store in
c  C(3,*).
c
c  Also, compute first divided difference of data and store in
c  C(4,*).
c
      do m=2,n
        c(3,m)=tau(m)-tau(m-1)
        c(4,m)=(c(1,m)-c(1,m-1))/c(3,m)
      end do
c
c  Construct the first equation from the boundary condition, of
c  the form:
c
c    c(4,1)*s(1) + c(3,1)*s(2) = c(2,1)
c
      if (ibcbeg.eq.1)then
        c(4,1)=1.0
        c(3,1)=0.0
        go to 60
      end if
 
      if (ibcbeg.gt.1)go to 50
c
c  No condition at left end and N = 2.
c
      if (n.le.2)then
        c(4,1)=1.0
        c(3,1)=1.0
        c(2,1)=2.0*c(4,2)
        go to 120
      end if
c
c  Not-a-knot condition at left end and N is greater than 2.
c
      c(4,1)=c(3,3)
      c(3,1)=c(3,2)+c(3,3)
      c(2,1)=((c(3,2)+2.0*c(3,1))*c(4,2)*c(3,3)+c(3,2)**2*c(4,3))
     &  /c(3,1)
      go to 70
c
c  Second derivative prescribed at left end.
c
50    continue
 
      c(4,1)=2.0
      c(3,1)=1.0
      c(2,1)=3.0*c(4,2)-c(3,2)/2.0*c(2,1)
 
60    continue
 
      if (n.eq.2)go to 120
c
c  If there are interior knots, generate the corresponding
c  equations and carry out the forward pass of Gauss elimination,
c  after which the M-th equation reads:
c
c    c(4,m)*s(m) + c(3,m)*s(m+1)=c(2,m).
c
70    continue
 
      do m=2,l
        g=-c(3,m+1)/c(4,m-1)
        c(2,m)=g*c(2,m-1)+3.0*(c(3,m)*c(4,m+1)+c(3,m+1)*c(4,m))
        c(4,m)=g*c(3,m-1)+2.0*(c(3,m)+c(3,m+1))
      end do
c
c  Construct last equation from the second boundary condition, of
c  the form
c
c    (-g*c(4,n-1))*s(n-1) + c(4,n)*s(n)=c(2,n)
c
c  If slope is prescribed at right end, one can go directly to
c  back-substitution, since the C array happens to be set up just
c  right for it at this point.
c
      if (ibcend.eq.1)go to 160
      if (ibcend.gt.1)go to 110
 
   90 if (n.eq.3.and.ibcbeg.eq.0) go to 100
c
c  Not-a-knot and N .ge. 3, and either N.gt.3 or also not-a-knot
c  at left end point.
c
      g=c(3,n-1)+c(3,n)
      c(2,n)=((c(3,n)+2.0*g)*c(4,n)*c(3,n-1)+c(3,n)**2
     &  *(c(1,n-1)-c(1,n-2))/c(3,n-1))/g
      g=-g/c(4,n-1)
      c(4,n)=c(3,n-1)
      c(4,n)=g*c(3,n-1)+c(4,n)
      c(2,n)=(g*c(2,n-1)+c(2,n))/c(4,n)
      go to 160
c
c  Either (n=3 and not-a-knot also at left) or (n=2 and not not-a-
c  knot at left end point).
c
100   continue
 
      c(2,n)=2.0*c(4,n)
      c(4,n)=1.0
      g=-1.0/c(4,n-1)
      c(4,n)=g*c(3,n-1)+c(4,n)
      c(2,n)=(g*c(2,n-1)+c(2,n))/c(4,n)
      go to 160
c
c  Second derivative prescribed at right endpoint.
c
110   continue
 
      c(2,n)=3.0*c(4,n)+c(3,n)/2.0*c(2,n)
      c(4,n)=2.0
      g=-1.0/c(4,n-1)
      c(4,n)=g*c(3,n-1)+c(4,n)
      c(2,n)=(g*c(2,n-1)+c(2,n))/c(4,n)
      go to 160
 
120   continue
 
      if (ibcend.eq.1)go to 160
 
      if (ibcend.gt.1)then
        c(2,n)=3.0*c(4,n)+c(3,n)/2.0*c(2,n)
        c(4,n)=2.0
        g=-1.0/c(4,n-1)
        c(4,n)=g*c(3,n-1)+c(4,n)
        c(2,n)=(g*c(2,n-1)+c(2,n))/c(4,n)
        go to 160
      end if
 
      if (ibcbeg.gt.0) go to 100
c
c  Not-a-knot at right endpoint and at left endpoint and N=2.
c
      c(2,n)=c(4,n)
c
c  Carry out the back substitution
c
160   continue
 
      do j=l,1,-1
        c(2,j)=(c(2,j)-c(3,j)*c(2,j+1))/c(4,j)
      end do
c
c  Generate cubic coefficients in each interval, that is, the
c  derivativies at its left endpoint, from value and slope at its
c  endpoints.
c
      do i=2,n
        dtau=c(3,i)
        divdf1=(c(1,i)-c(1,i-1))/dtau
        divdf3=c(2,i-1)+c(2,i)-2.0*divdf1
        c(3,i-1)=2.0*(divdf1-c(2,i-1)-divdf3)/dtau
        c(4,i-1)=(divdf3/dtau)*(6.0/dtau)
      end do
 
      return
      end
      subroutine diamnd ( xcentr, ycentr, radius, filled )

c*********************************************************************72
c
cc DIAMND draws an open or filled diamond of a given size.
c
c  Discussion:
c
c    DIAMND calls PLYLIN to draw an open diamond, or PLYGON to draw a
c    filled diamond.
c
c    To control the color of the diamond, simply call the DRAWCGM
c    routine LINCLR before drawing open diamonds or FILCOR before
c    drawing closed diamonds.
c
c    Note: if the X and Y coordinate systems do not have the same scale,
c    the diamond drawn will be "flattened".  This can happen if the
c    routine SETWCD has been used to set the X and Y dimensions to
c    different extents.
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
c  Parameters:
c
c    Input, real XCENTR, YCENTR, the X and Y coordinates of
c    the center of the diamond.
c
c    Input, real RADIUS, the radius of the diamond.
c
c    Input, LOGICAL FILLED, .TRUE. if the diamond is to be filled.
c
      implicit none

      logical filled
      real radius
      real xcentr
      real xpoint(5)
      real ycentr
      real ypoint(5)

      xpoint(1) = xcentr + radius
      ypoint(1) = ycentr
 
      xpoint(2) = xcentr
      ypoint(2) = ycentr + radius
 
      xpoint(3) = xcentr - radius
      ypoint(3) = ycentr
 
      xpoint(4) = xcentr
      ypoint(4) = ycentr - radius
 
      if ( filled ) then
 
        call plygon ( 4, xpoint, ypoint )
 
      else
 
        xpoint(5) = xcentr + radius
        ypoint(5) = ycentr
 
        call plylin ( 5, xpoint, ypoint )
 
      end if
 
      return
      end
      subroutine diamond ( hite, x, y )

c*********************************************************************72
c
cc DIAMOND draws a diamond of height HITE centered at the point X, Y.
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
c  Parameters:
c
c    Input, real HITE, the height of the diamond.
c
c    Input, real X, Y, the location of the center of the diamond.
c
      implicit none

      real rad
      real hite
      real x
      real y

      rad = 0.5*hite
 
      call movcgm ( x,     y+rad )
      call drwcgm ( x+rad, y )
      call drwcgm ( x,     y-rad )
      call drwcgm ( x-rad, y )
      call drwcgm ( x,     y+rad )
      call movcgm ( x,     y )
 
      return
      end
      subroutine dshlin ( n, x, y, dshsiz )

c*********************************************************************72
c
cc DSHLIN draws a dashed line connecting a series of points.
c
c  Discussion:
c
c    If the X and Y coordinates use different scale
c    factors, then dashes at different angles will seem to
c    have different lengths.
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
c  Parameters:
c
c    Input, integer N, the number of points to be connected.
c
c    Input, real X(N), Y(N), the X and Y coordinates of the points.
c
c    Input, real DSHSIZ, the length, in the X, Y coordinate
c    system, of the dashed lines.  If it is negative or zero,
c    an error occurs.
c
      implicit none

      integer n

      real dist
      real dshsiz
      integer i
      integer j
      integer ndash
      real x(n)
      real xnew
      real xold
      real xxnew
      real xxold
      real y(n)
      real ynew
      real yold
      real yynew
      real yyold
c
c  Make sure that DSHSIZ is positive.
c
      if ( dshsiz .le. 0.0 ) then
        write(*,*)' '
        write(*,*)'DshLin - Fatal error!'
        write(*,*)'  The parameter DSHSIZ must be positive.'
        write(*,*)'  but the input value is ', dshsiz
        return
      end if
 
      xnew = x(1)
      ynew = y(1)
 
      do i = 2, n
 
        xold = xnew
        yold = ynew
        xnew = x(i)
        ynew = y(i)
        dist = sqrt ( (xnew-xold)**2 + (ynew-yold)**2 )
 
        if ( dist .gt. 0.0 ) then
 
          ndash = int (dist/dshsiz) + 1
 
          if ( mod(ndash,4) .ne. 0 ) then
            ndash = ndash + 4 - mod(ndash,4)
          end if
 
          if ( ndash .le. 3 ) then
            ndash = 4
          end if
c
c  Desired pattern is:
c
c  X0 - dash - blank - blank - dash - dash - blank - blank - dash - X1
c
          do j = 1, ndash
 
            if (mod(j,4).eq.0.or.mod(j,4).eq.1)then
              xxold = ( (ndash+1-j)*xold + (j-1)*xnew) / real(ndash)
              yyold = ( (ndash+1-j)*yold + (j-1)*ynew) / real(ndash)
              xxnew = ( (ndash-j)*xold + j*xnew) / real(ndash)
              yynew = ( (ndash-j)*yold + j*ynew) / real(ndash)
              call movcgm(xxold,yyold)
              call drwcgm(xxnew,yynew)
            end if
 
          end do
 
        end if
 
      end do
 
      return
      end
      subroutine ellipse ( xcenter, ycenter, rmajor, rminor, angle )

c*********************************************************************72
c
cc ELLIPSE draws an ellipse for a picture being created by DRAWCGM.
c
c  Discussion:
c
c    The ellipse is not "filled in".  The DRAWCGM routine PLYLIN is
c    used to draw the curve.  Hence, the color of the line used to
c    draw the ellipse can be chosen by calling LINCLR before calling
c    ELLIPSE.
c
c    ELLIPSE requires the user to provide the center of the ellipse,
c    the length of the long and short sides of the ellipse, and the
c    angle at which the ellipse is tilted.
c
c    Note that the parametric equation of an ellipse with center at
c    (0,0) and horizontal axis A and vertical axis B is:
c
c      R*R = (A*A * B*B) / (A*A*SIN(THETA)**2 + B*B*COS(THETA)**2)
c
c    From this fact, we can derive information about the location of
c    points on a "tilted" ellipse.
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
c  Parameters:
c
c    Input, real XCENTER, YCENTER, the location of the center
c    of the ellipse.
c
c    Input, real RMAJOR, RMINOR, the length of the major and minor
c    radiuses of the ellipse.  That is, the distance from the center
c    of the ellipse to the tip of the "long" end, and to the short
c    end.
c
c    Input, real ANGLE, the angle at which the ellipse is tilted.
c    ANGLE is to be measured in degrees, and NOT in radians!
c    A value of 0.0 for ANGLE means that the ellipse is lying
c    "flat" like a pancake, with the long end horizontal.
c    A value of 30.0 would tilt the ellipse toward 8 and 2 o'clock.
c
      implicit none

      integer nval
      parameter ( nval = 257 )

      real pi
      parameter ( pi = 3.1415926535 )

      real degrad
      parameter ( degrad = pi / 180.0 )

      real angle
      integer i
      real radius
      real rmajor
      real rminor
      real theta
      real x(nval)
      real xcenter
      real y(nval)
      real ycenter

      do i = 1, nval
 
        theta = 360.0 * real ( i-1 ) / real ( nval-1 )
 
        radius = ( rmajor * rminor ) / sqrt
     &    ( rmajor**2 * cos(degrad*theta)**2
     &    + rminor**2 * sin(degrad*theta)**2 )
 
        x(i) = xcenter + radius * cos ( degrad*(angle+theta) )
        y(i) = ycenter + radius * sin ( degrad*(angle+theta) )
 
      end do
 
      call plylin ( nval, x, y )
 
      return
      end
      subroutine file_delete ( file_name )

c*********************************************************************72
c
cc FILE_DELETE deletes a file if it exists.
c
c  Discussion:
c
c    You might want to call this routine to get rid of any old copy
c    of a file, before trying to open a new copy with the OPEN argument:
c
c      status = 'new'.
c
c    It's not always safe to open a file with " STATUS = 'UNKNOWN' ".
c
c    For instance, on the SGI, the most recent version of the FORTRAN
c    compiler seems to go crazy when I open an unformatted direct
c    access file this way.  It creates an enormous file (of somewhat
c    random size).  The problem goes away if I delete any old copy
c    using this routine, and then open a fresh copy with
c    " STATUS = 'NEW' ".  It's a scary world.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) FILE_NAME, the name of the file to be deleted.
c
      implicit none

      character*80 ctemp
      character*(*) file_name
      logical lfile
      integer s_len_trim
      integer unit
c
c  Does the file exist?
c
      inquire (
     &  file = file_name,
     &  exist = lfile )

      if ( .not. lfile ) then
        return
      end if
c
c  Can we get a FORTRAN unit number?
c
      call get_unit ( unit )

      if ( unit .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILE_DELETE - Error!'
        write ( *, '(a)' ) '  A free FORTRAN unit could not be found.'
        return
      end if
c
c  Can we open the file?
c  
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FILE_DELETE:'
      write ( *, '(a)' ) '  Open "' // trim ( file_name ) // '".'

      open (
     &  unit = unit,
     &  file = file_name,
     &  status = 'old',
     &  err = 10 )
c
c  Can we close the file with "Delete" status?
c
      write ( *, '(a)' ) '  Delete "' // trim ( file_name ) // '".'

      close (
     &  unit = unit,
     &  status = 'delete',
     &  err = 20 )

      return

10    continue

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FILE_DELETE - Error!'
      write ( *, '(a)' ) 
     &  '  Could not open "' // trim ( file_name ) // '".'
      return

20    continue

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FILE_DELETE - Error!'
      write ( *, '(a)' ) 
     &  '  Could not delete "' // trim ( file_name ) // '".'

      return
      end
      function gauss ( ratio )

c*********************************************************************72
c
cc GAUSS is a simple function used by SETTAB.
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
c  Parameters:
c
c    Input, real RATIO, determines the "spread" of the gaussian function.
c
c    Output, real GAUSS, the value EXP(-RATIO**2).
c
      implicit none

      real gauss
      real ratio

      gauss = exp ( -ratio**2 )
 
      return
      end
      subroutine get_unit ( unit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is an integer between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If UNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, UNIT is an integer between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer UNIT, the free unit number.
c
      implicit none

      integer i
      integer unit

      unit = 0

      do i = 1, 99

        if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

          open ( unit = i, err = 10, status = 'scratch' )
          close ( unit = i )

          unit = i

          return
        end if

10      continue

      end do

      return
      end
      subroutine interv(xt,lxt,x,left,mflag)
c
c*********************************************************************72
c
cc INTERV seeks to bracket a value within a vector of increasing values.
c
c  Discussion:
c
c    INTERV computes LEFT, the maximum value of I so that
c
c      1 <= I <= XT
c    and
c     XT(I) <= X.
c
c    The routine is designed to be efficient in the common situation
c    that it is called repeatedly, with X taken from an increasing
c    or decreasing sequence.
c
c    This will happen when a piecewise polynomial is to be graphed.
c    The first guess for LEFT is therefore taken to be the value
c    returned at the previous call and stored in the local variable
c    ILO.
c
c    A first check ascertains that ILO.LT.LXT.  This is necessary
c    since the present call may have nothing to do with the previous
c    call.  Then, if XT(ILO) <= X < XT(ILO+1), we set LEFT=ILO
c    and are done after just three comparisons.
c
c    Otherwise, we repeatedly double the difference istep = ihi - ilo
c    while also moving  ilo  and  ihi  in the direction of x, until
c      xt(ilo) .le. x .lt. xt(ihi) ,
c    after which we use bisection to get, in addition, ilo+1 = ihi.
c    left = ilo  is then returned.
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
c  Parameters:
c
c    Input, real XT(LXT), a nondecreasing sequence of values.
c
c    Input, integer LXT, the dimension of XT.
c
c    Input, real X, the point whose location with respect to
c    the sequence XT is to be determined.
c
c    Output, integer LEFT, MFLAG, whose value is
c    1     -1      if               x .lt.  xt(1)
c    i      0      if   xt(i)  .le. x .lt. xt(i+1)
c    lxt    1      if  xt(lxt) .le. x
c    In particular, mflag = 0 is the 'usual' case.  mflag.ne.0
c    indicates that x lies outside the half open interval
c    xt(1).le.y.lt.xt(lxt).  the asymmetric treatment of the
c    interval is due to the decision to make all piecewise
c    polynomials continuous from the right.
c
      implicit none

      integer lxt

      integer left
      integer mflag
      integer ihi
      integer ilo
      integer istep
      integer middle
      real x
      real xt(lxt)

      save ilo

      data ilo / 1 /

      ihi=ilo+1
 
      if (ihi.ge.lxt)then
 
        if (x.ge.xt(lxt))then
          mflag=1
          left=lxt
          return
        end if
 
        if (lxt.le.1)then
          mflag=-1
          left=1
          return
        end if
 
        ilo=lxt-1
        ihi=lxt
 
      end if
 
      if (x.ge.xt(ihi)) go to 40
 
      if (x.ge.xt(ilo))then
        mflag=0
        left=ilo
        return
      end if
c
c  Now X is less than XT(ILO).  Decrease ILO to capture X.
c
      istep=1
 
20    continue
 
      ihi=ilo
      ilo=ihi-istep
 
      if (ilo.gt.1)then
        if (x.ge.xt(ilo))go to 70
        istep=istep*2
        go to 20
      end if
 
      ilo=1
 
      if (x.lt.xt(1))then
        mflag=-1
        left=1
        return
      end if
 
      go to 70
c
c  Now X is greater than or equal to XT(IHI).  Increase IHI to
c  capture X.
c
   40 continue
 
      istep=1
 
50    continue
 
      ilo=ihi
      ihi=ilo+istep
 
      if (ihi.lt.lxt)then
        if (x.lt.xt(ihi))go to 70
        istep=istep*2
        go to 50
      end if
 
      if (x.ge.xt(lxt))then
        mflag=1
        left=lxt
        return
      end if
 
      ihi=lxt
c
c  Now xt(ilo) .le. x .lt. xt(ihi).  Narrow the interval.
c
   70 continue
 
      middle=(ilo+ihi)/2
 
      if (middle.eq.ilo)then
        mflag=0
        left=ilo
        return
      end if
c
c  It is assumed that MIDDLE=ILO in case IHI=ILO+1.
c
      if (x.lt.xt(middle))then
        ihi=middle
      else
        ilo=middle
      end if
 
      go to 70
 
      end
      subroutine ixtox ( ix, ixmax, ixmin, x, xmax, xmin )

c*********************************************************************72
c
cc IXTOX maps integer IX in [IXMIN, IXMAX] to real X in [XMIN, XMAX].
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
c  Parameters:
c
c    Input, integer IX, the integer to be converted.
c
c    Input, integer IXMAX, IXMIN, the integer range.
c    IXMAX and IXMIN must not be equal.
c
c    Output, real X, the corresponding real number.
c
c    Input, real XMAX, XMIN, the real range.
c
      implicit none

      integer ix
      integer ixmax
      integer ixmin
      real x
      real xmax
      real xmin

      if ( ixmax .eq. ixmin ) then
        write ( *, * ) ' '
        write ( *, * ) 'IXTOX - Fatal error!'
        write ( *, * ) '  IXMAX = IXMIN, resulting in zero denominator.'
        write ( *, * ) '  IXMAX = ', ixmax
        write ( *, * ) '  IXMIN = ', ixmin
        stop
      end if
 
      x = ( ( ixmax - ix ) * xmin + ( ix - ixmin ) * xmax ) 
     &  / real ( ixmax - ixmin )
 
      return
      end
      function lenchr(string)

c*********************************************************************72
c
cc LENCHR returns the length of STRING up to the last nonblank character.
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
c  Parameters:
c
c    Input, character*(*) STRING, the string to be measured.
c
c    Output, integer LENCHR, the location of the last nonblank
c    character in STRING.
c
      implicit none

      integer i
      integer lenchr
      character*(*) string

      do i=len(string),1,-1
 
        if (string(i:i).ne.' ')then
          lenchr=i
          return
        end if
 
      end do
 
      lenchr=0
 
      return
      end
      subroutine oval ( xcenter, ycenter, rmajor, rminor, angle )

c*********************************************************************72
c
cc OVAL draws a "filled in" ellipse for a picture being created by DRAWCGM.
c
c  Discussion:
c
c    The DRAWCGM routine PLYGON is used to draw the filled-in
c    elliptical polygon.  Hence, the color of the ellipse can be
c    chosen by calling FILCLR before calling OVAL.
c
c    OVAL requires the user to provide the center of the ellipse,
c    the length of the long and short sides of the ellipse, and the
c    angle at which the ellipse is tilted.
c
c    Note that the parametric equation of an ellipse with center at (0,0)
c    and horizontal axis A and vertical axis B is:
c
c      R*R = (A*A * B*B) / (A*A*SIN(THETA)**2 + B*B*COS(THETA)**2)
c
c    From this fact, we can derive information about the location of
c    points on a "tilted" ellipse.
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
c  Parameters:
c
c    Input, real XCENTER, YCENTER, the location of the center of
c    the ellipse.
c
c    Input, real RMAJOR, RMINOR, the length of the major and minor
c    radiuses of the ellipse.  That is, the distance from the center
c    of the ellipse to the tip of the "long" end, and to the short
c    end.
c
c    Input, real ANGLE, the angle at which the ellipse is tilted.
c    ANGLE is to be measured in degrees, and NOT in radians!
c    A value of 0.0 for ANGLE means that the ellipse is lying
c    "flat" like a pancake, with the long end horizontal.
c    A value of 30.0 would tilt the ellipse toward 8 and 2 o'clock.
c
      implicit none

      integer npoints
      parameter ( npoints = 256 )

      real pi
      parameter ( pi = 3.1415926535 )

      real degrad
      parameter ( degrad = pi / 180.0 )

      real angle
      integer i
      real radius
      real rmajor
      real rminor
      real theta
      real x(npoints)
      real xcenter
      real y(npoints)
      real ycenter

      do i = 1, npoints
 
        theta = 360.0 * real(i-1) / real(npoints)
 
        radius = ( rmajor * rminor ) / sqrt
     &    ( rmajor**2 * cos(degrad*theta)**2
     &    + rminor**2 * sin(degrad*theta)**2 )
 
        x(i) = xcenter + radius * cos(degrad*(angle+theta))
        y(i) = ycenter + radius * sin(degrad*(angle+theta))
 
      end do
 
      call plygon ( npoints, x, y )
 
      return
      end
      subroutine pgrid ( xcenter, ycenter, rmin, rmax, nr, angmin,
     &  angmax, nang )

c*********************************************************************72
c
cc PGRID draws evenly spaced grid lines for polar coordinates.
c
c  Discussion:
c
c    You may change the color used to draw the grid lines by calling
c    the DRAWCGM routine LINCLR before calling this routine.
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
c  Parameters:
c
c    Input, real XCENTER, YCENTER, are the X and Y coordinates
c    of the origin of the polar grid.  The point (XCENTER,YCENTER)
c    will correspond to a radial value of 0.
c
c    Input, real RMIN, RMAX, are the radial limits of the grid.
c    RMIN should be zero or greater.
c    RMAX should be "within" your picture.  For instance, if you
c    are using the simple DRAWCGM coordinate system, which uses 0
c    as the minimum X value and 1 as the maximum X value, then
c    XCENTER-RMAX should be no less than 0,
c    XCENTER+RMAX should be no greater than 1,
c    YCENTER-RMAX should be no less than 0,
c    YCENTER+RMAX should be no greater than 1.
c    If you have used SETWCD or SETSCL to allow a different range of
c    X and Y values, then RMAX must be selected in a similar way.
c
c    Input, integer NR, the number of circular grid lines to draw,
c    between RMIN and RMAX.  The value of NR is assumed to include
c    a grid circle drawn at R=RMIN.  However, if RMIN=0, this circle,
c    which will actually be just a point, won't be drawn.
c
c    Input, real ANGMIN, ANGMAX, are the angular limits of the grid.
c    Note that ANGMIN and ANGMAX are measured in DEGREES, and not
c    RADIANS!  This is strictly for user convenience, since it is
c    so much easier to say "90.0" rather than "3.14159265.../2.0",
c    for instance.
c    Be careful not to directly pass an integer constant as the
c    value of either argument.  ANGMIN and ANGMAX should be real
c    variables, or real constants.
c
c    Input, integer NANG, the number of grid lines drawn outward from
c    the origin, and evenly spaced through the angular range.
c
      implicit none

      real pi
      parameter ( pi = 3.1415926535 )

      real degrad
      parameter ( degrad = pi / 180.0 )

      real angle
      real angmax
      real angmin
      integer i
      integer nang
      integer nr
      real radius
      real rmax
      real rmin
      real xcenter
      real xval
      real ycenter
      real yval
c
c  Draw the circular grid lines.
c
      do i = 1, nr
 
        if ( nr .eq. 1 ) then
          radius = 0.5 * ( rmin + rmax )
        else
          radius = ( real(nr-i) * rmin + real(i-1) * rmax ) / real(nr-1)
        end if
 
        if ( radius .gt. 0.0 ) then
          call carc ( xcenter, ycenter, radius, angmin, angmax )
        end if
 
      end do
c
c  Draw the radial grid lines.
c
      do i = 1, nang
 
        if ( nang .ne. 1 ) then
          angle = ( real(nang-i)*angmin + real(i-1)*angmax )
     &      / real(nang-1)
        else
          angle = 0.5 * ( angmin + angmax )
        end if
 
        xval = xcenter + rmin * cos(degrad*angle)
        yval = ycenter + rmin * sin(degrad*angle)
        call movcgm ( xval, yval )
 
        xval = xcenter + rmax * cos(degrad*angle)
        yval = ycenter + rmax * sin(degrad*angle)
        call drwcgm ( xval, yval )
 
      end do
 
      return
      end
      function ppvalu(break,coef,l,k,x,jderiv)

c*********************************************************************72
c
cc PPVALU evaluates a piecewise polynomial function or its derivatives.
c
c  Discussion:
c
c    PPVALU calculates the value at X of the JDERIV-th derivative of
c    the piecewise polynomial function F from its piecewise
c    polynomial representation.
c
c    The interval index I, appropriate for X, is found through a
c    call to INTERV.  The formula above for the JDERIV-th derivative
c    of F is then evaluated by nested multiplication.
c
c    The J-th derivative of F is given by:
c
c      (d**j)f(x) = coef(j+1,i)
c                 + h*(coef(j+2,i)
c                 + h*(...(coef(k-1,i) +
c                 + h*coef(k,i)/(k-j-1))/(k-j-2) ... )/2)/1
c
c    with
c
c      h = x - break(i),
c
c    and
c
c      i  =  max( 1 , max( j ,  break(j) .le. x , 1 .le. j .le. l ) ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Parameters:
c
c    Input, real BREAK(L), REAL COEF(K,L), integer L, form the piecewise
c    polynomial representation of the function F to be evaluated.
c
c    Input, integer K, the order of the polynomial pieces
c    that make up the function F.  The most usual value for
c    K is 4, signifying a piecewise cubic polynomial.
c
c    Input, real X, the point at which to evaluate F or one
c    of its derivatives.
c
c    Input, integer JDERIV, the order of the derivative to be
c    evaluated.  If JDERIV is 0, then F itself is evaluated,
c    which is actually the most common case.  It is assumed
c    that JDERIV is zero or positive.
c
c    Output, real PPVALU, the value of the JDERIV-th
c    derivative of F at X.
c
      implicit none

      integer k
      integer l

      real break(l)
      real coef(k,l)
      real fmmjdr
      real h
      integer i
      integer jderiv
      integer m
      integer ndummy
      real ppvalu
      real x

      ppvalu=0.0
 
      fmmjdr=k-jderiv
c
c  Derivatives of order K or higher are identically zero.
c
      if (k.le.jderiv)return
c
c  Find the index I of the largest breakpoint to the left of X.
c
      call interv(break,l,x,i,ndummy)
c
c  Evaluate the JDERIV-th derivative of the I-th polynomial piece
c  at X.
c
      h=x-break(i)
      m=k
 
10    continue
 
      ppvalu=(ppvalu/fmmjdr)*h+coef(m,i)
      m=m-1
      fmmjdr=fmmjdr-1
      if (fmmjdr.gt.0.0)go to 10
 
      return
      end
      subroutine reven ( n, x, xhi, xlo )

c*********************************************************************72
c
cc REVEN returns N real values, evenly spaced between XLO and XHI.
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
c  Parameters:
c
c    Input, integer N, the number of values.
c
c    Output, real X(N), N evenly spaced values.
c    Unless N = 1, X(1) = XLO and X(N) = XHI.
c    If N = 1, then X(1) = 0.5*(XLO+XHI).
c
c    Input, real XHI, XLO, the high and low values.
c
      implicit none

      integer n

      integer i
      real x(n)
      real xhi
      real xlo

      if ( n .eq. 1 ) then
 
        x(1) = 0.5 * ( xlo + xhi )
 
      else
 
        do i = 1, n
          x(i) = ( ( n - i ) * xlo + ( i - 1 ) * xhi ) / real ( n - 1 )
        end do
 
      end if
 
      return
      end
      subroutine rrange ( nval, x, xmax, xmin )

c*********************************************************************72
c
cc RRANGE computes the range of a real array.
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
c  Parameters:
c
c    Input, integer NVAL, the number of entries in the array.
c
c    Input, real X(NVAL), the array.
c
c    Output, real XMAX, XMIN, the largest and smallest entries in 
c    the array.
c
      implicit none

      integer nval

      integer i
      real x(nval)
      real xmax
      real xmin

      xmax = x(1)
      xmin = x(1)
 
      do i = 2, nval
        xmax = max ( xmax, x(i) )
        xmin = min ( xmin, x(i) )
      end do
 
      return
      end
      subroutine scale ( xmin, xmax, ymin, ymax )

c*********************************************************************72
c
cc SCALE resets the current coordinate system for the picture.
c
c  Discussion:
c
c    After calling SCALE, the coordinate ranges become
c
c     XMIN <= X <= XMAX
c     YMIN <= Y <= YMAX
c
c    You can create a new coordinate system at any time by calling
c    SCALE again, or by calling SCL01 to restore the original 0, 1
c    coordinate system, or using SETSCL or SETWCD.
c
c    Since this routine calls SETSCL, the picture will have the same
c    "aspect ratio" or proportions as the coordinate system.  In
c    other words,
c
c      call scale(1.0, 3.0, 10.0, 11.0)
c
c    will produce a picture that shows up twice as wide (3.0 - 1.0)
c    as it is tall (11.0 - 10.0).
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
c  Parameters:
c
c    Input, real XMIN, XMAX, the minimum and maximum values of
c    X to be allowed.
c
c    Input, real YMIN, YMAX, the minimum and maximum values of
c    Y to be allowed.
c
      implicit none

      integer nbox
      parameter ( nbox = 2 )

      real xbox(nbox)
      real xmax
      real xmin
      real ybox(nbox)
      real ymax
      real ymin

      xbox(1) = xmin
      ybox(1) = ymin
 
      xbox(2) = xmax
      ybox(2) = ymax
 
      call setscl ( xbox, ybox, nbox )
 
      return
      end
      subroutine scl01 ( )

c*********************************************************************72
c
cc SCL01 restores the coordinate system to the default 0, 1 system.
c
c  Discussion:
c
c    Although DRAWCGM starts out using the 0, 1 system, a call to
c    SETWCD or SETSCL will change that original coordinate system.  The
c    new coordinate system remains in effect "forever", unless another
c    call to SETWCD or SETSCL is made, or unless SCL01 restores the
c    coordinate system to the old 0, 1 system.
c
c    After SCL01 is called, DRAWCGM assumes that all subsequent coordinate
c    information will be such that both X and Y lie between 0 and 1.
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
c  Parameters:
c
c    None
c
      implicit none

      integer nbox
      parameter ( nbox = 2 )

      real xbox(nbox)
      real ybox(nbox)

      xbox(1) = 0.0
      ybox(1) = 0.0
 
      xbox(2) = 1.0
      ybox(2) = 1.0
 
      call setscl ( xbox, ybox, nbox )
 
      return
      end
      subroutine settab ( icmax, icmin, itable )

c*********************************************************************72
c
cc SETTAB sets up the color table.
c
c  Discussion:
c
c    SETTAB replaces the unreliable DRAWCGM routine SETCTB.
c
c    SETTAB sets the colors between ICMIN and ICMAX, which
c    should typically be 2 and 255.
c
c    SETTAB will also set the values of color 0 to white, and
c    color 1 to black.
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
c  Parameters:
c
c    Input, integer ICMAX, the maximum color index to be set.
c
c    Input, integer ICMIN, the minimum color index to be set.
c
c    Input, integer ITABLE, the desired table.
c    1: low black to high white
c    2: low white to high black.
c    3: low blue to high yellow
c    4: low red, high blue, with bands between.
c    5: low red, yellow, green, blue, high white.
c    5: low white, blue, green, yellow, high red.
c    7: low blue to high red
c    8: linear table between 2 user colors.
c    9: linear table between n user colors.
c
      implicit none

      real pi
      parameter ( pi = 3.1415926535 )

      real bhi
      real blo
      real bval
      real ghi
      real glo
      real gval
      integer i
      integer icmax
      integer icmin
      integer icol1
      integer icol2
      integer itable
      integer ival
      real rhi
      real rlo
      real rval
      real temp
      real theta

      icmin = max ( icmin, 2 )
      icmax = min ( icmax, 255 )

1996  continue
c
c  1: Low black to high white
c  2: Low white to high black
c  3: Low blue to high yellow.
c  4: Low red, high blue, with bands between.
c  5: Low red, yellow, green, blue, high white.
c  6: Low white, blue, green, yellow, high red.
c  7: Low blue to high red
c
      if ( itable .ge. 1 .and. itable .le. 7 ) then

        do i = icmin, icmax

          if ( icmin .eq. icmax ) then
            temp = 0.5
          else
            temp = real ( i - icmin ) / real ( icmax - icmin )
          end if

          if ( itable .eq. 1 ) then
            bval = temp
            gval = temp
            rval = temp
          else if ( itable .eq. 2 ) then
            bval = 1.0 - temp
            gval = 1.0 - temp
            rval = 1.0 - temp
          else if ( itable .eq. 3 ) then
            rval = temp
            gval = temp
            bval = 1.0 - temp
          else if ( itable .eq. 4 ) then
            theta = 0.5 * pi * temp
            rval = cos(theta)**2
            bval = sin(theta)**2
            gval = 0.8 * sin(10.0*theta)**6
          else if ( itable .eq. 5 ) then
            theta = 4.0 * temp
            rval = exp(-(theta-1.0)**2)+exp(-(theta-4.0)**2)
            gval = exp(-(theta-2.0)**2)+exp(-(theta-4.0)**2)
            bval = exp(-(theta-3.0)**2)+exp(-(theta-4.0)**2)
            rval = max ( rval, 0.0 )
            rval = min ( rval, 1.0 )
            gval = max ( gval, 0.0 )
            gval = min ( gval, 1.0 )
            bval = max ( bval, 0.0 )
            bval = min ( bval, 1.0 )
          else if ( itable .eq. 6 ) then
            theta = 4.0*temp
            rval = exp(-(theta-1.0)**2)+exp(-(theta-4.0)**2)
            gval = exp(-(theta-2.0)**2)+exp(-(theta-4.0)**2)
            bval = exp(-(theta-3.0)**2)+exp(-(theta-4.0)**2)
            rval = max ( rval, 0.0 )
            rval = min ( rval, 1.0 )
            gval = max ( gval, 0.0 )
            gval = min ( gval, 1.0 )
            bval = max ( bval, 0.0 )
            bval = min ( bval, 1.0 )
          else if ( itable .eq. 7 ) then
            rval = temp
            gval = 0.0
            bval = 1.0 - temp
          end if

          ival = i
          call setclr ( ival, rval, gval, bval )

        end do
c
c  8: Interpolate between two values.
c
      else if ( itable .eq. 8 ) then
 
        write(*,*)' '
        write(*,*)'SETTAB - Input request:'
        write(*,*)' '
        write(*,*)'  Enter  Rlo, Glo, Blo,   Rhi, Ghi, Bhi'
        write(*,*)'  Note: 0,0,0 is black, and 1,1,1 is white!'
        write(*,*)' '
 
        read ( *, *, end = 1952, err = 1964 ) 
     &    rlo, glo, blo, rhi, ghi, bhi

        rlo = max ( rlo, 0.0 )
        rhi = min ( rhi, 1.0 )
        glo = max ( glo, 0.0 )
        ghi = min ( ghi, 1.0 )
        blo = max ( blo, 0.0 )
        bhi = min ( bhi, 1.0 )

        do i = icmin, icmax

          if ( icmin .eq. icmax ) then
            temp = 0.5
          else
            temp = real ( i - icmin ) / real ( icmax - icmin )
          end if

          rval = rlo * ( 1.0 - temp ) + rhi * temp
          gval = glo * ( 1.0 - temp ) + ghi * temp
          bval = blo * ( 1.0 - temp ) + bhi * temp

          ival = i
          call setclr ( ival, rval, gval, bval )

        end do
c
c  9: Interpolate between several values.
c
      else if ( itable .eq. 9 ) then
 
        icol1 = icmin
        write(*,*)' '
        write(*,*)'SETTAB - Input request:'
        write(*,*)' '
        write(*,*)'  Enter (R, G, B) for color index ',icol1
        write(*,*)'      (0, 0, 0) is black.'
        write(*,*)'      (1, 1, 1) is white.'
        read ( *, * ) rlo, glo, blo

        rlo = max ( rlo, 0.0 )
        glo = max ( glo, 0.0 )
        blo = max ( blo, 0.0 )
 
10      continue
 
        write(*,*)'  Enter index of next color to set'
        write(*,*)'  between ',icol1+1,' and ',icmax
        read ( *, * ) icol2
 
        if ( icol2 .le. icol1 .or. icol2 .gt. icmax ) then
          write(*,*)' '
          write(*,*)'SETTAB - Warning!'
          write(*,*)'  Your color index was not accepted!'
          go to 10
        end if
 
        write(*,*)' '
        write(*,*)'Enter (R, G, B) for color index ',icol2
        read ( *, * ) rhi, ghi, bhi
 
        rhi = min ( rhi, 1.0 )
        ghi = min ( ghi, 1.0 )
        bhi = min ( bhi, 1.0 )

        do i = icol1, icol2

          if ( icol1 .eq. icol2 ) then
            temp = 0.5
          else
            temp = real ( i - icol1 ) / real ( icol2 - icol1 )
          end if

          rval = rlo * ( 1.0 - temp ) + rhi * temp
          gval = glo * ( 1.0 - temp ) + ghi * temp
          bval = blo * ( 1.0 - temp ) + bhi * temp

          ival = i
          call setclr ( ival, rval, gval, bval )

        end do
 
        if ( icol2 .lt. icmax ) then
          icol1 = icol2
          rlo = rhi
          glo = ghi
          blo = bhi
          go to 10
        end if
c
c  Unknown table.
c
      else

        write(*,*)' '
        write(*,*)'SETTAB - Fatal error!'
        write(*,*)'  Legal color table indices are '
        write(*,*)'  between 1 and 9.  Your value was ', itable

      end if
c
c  Background color 0 is to be white.
c
      ival = 0
      rval = 1.0
      gval = 1.0
      bval = 1.0
      call setclr ( ival, rval, gval, bval )
c
c  Foreground color 1 is to be black.
c
      ival = 1
      rval = 0.0
      gval = 0.0
      bval = 0.0
      call setclr ( ival, rval, gval, bval )

      return
 
1952  continue

      write(*,*)' '
      write(*,*)'SETTAB - Fatal error!'
      write(*,*)'  Unexpected end of file!'
      stop
 
1964  continue

      write(*,*)' '
      write(*,*)'SETTAB - Warning!'
      write(*,*)'  Illegal format for input data!'
      write(*,*)'  Try again!'
      go to 1996
      end
      subroutine splxy(ndat,nval,work,xdat,xval,ydat,yval)

c*********************************************************************72
c
cc SPLXY computes a cubic spline interpolant to (X,Y) data.
c
c  Discussion:
c
c    It is assumed that the X values are distinct and monotonic.
c
c    The "not-a-knot" augmenting boundary condition is used.
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
c  Parameters:
c
c    Input, integer NDAT, the number of data points.
c
c    Input, integer NVAL, the number of points at which
c    the resulting spline is to be evaluated.
c
c    Workspace, real WORK(4,NDAT).
c
c    Input, real XDAT(NDAT), the X values of the data.
c
c    Input, real XVAL(NVAL), the X values at which the
c    resulting spline is to be evaluated.
c
c    Input, real YDAT(NDAT), the Y values of the data.
c
c    Output, real YVAL(NVAL), the value of the interpolating
c    cubic spline at the points xval.
c
      implicit none

      integer ndat
      integer nval

      integer i
      integer ibcbeg
      integer ibcend
      integer jderiv
      integer norder
      integer npiece
      real ppvalu
      real work(4,ndat)
      real xdat(ndat)
      real xval(nval)
      real ydat(ndat)
      real yval(nval)

      external ppvalu
c
c  Set up the cubic spline.
c
      do i=1,ndat
        work(1,i)=ydat(i)
      end do
 
      ibcbeg=0
      ibcend=0
      call cubspl(xdat,work,ndat,ibcbeg,ibcend)
c
c  Evaluate the cubic spline.
c
      jderiv=0
      norder=4
      npiece=ndat-1
      do i=1,nval
        yval(i)=ppvalu(xdat,work,npiece,norder,xval(i),jderiv)
      end do
 
      return
      end
      subroutine square ( side, x, y )

c*********************************************************************72
c
cc SQUARE draws a square centered at a given point.
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
c  Parameters:
c
c    Input, real SIDE, the length of one side of the square.
c
c    Input, real X, Y, the location of the center of the square.
c
      implicit none

      real rad
      real side
      real x
      real y

      rad = 0.5 * side
 
      call movcgm ( x+rad, y+rad )
      call drwcgm ( x+rad, y-rad )
      call drwcgm ( x-rad, y-rad )
      call drwcgm ( x-rad, y+rad )
      call drwcgm ( x+rad, y+rad )
      call movcgm ( x, y )
 
      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ', 
     &  'May      ', 'June     ', 'July     ', 'August   ', 
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *, 
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) 
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
      subroutine trihatch ( xval, yval, nhatch )

c*********************************************************************72
c
cc TRIHATCH draws a triangle filled with diagonal hatch marks.
c
c  Discussion:
c
c    The lines are drawn by calls to MOVCGM and DRWCGM, so a call
c    to LINCLR beforehand may be used to set the color of the lines.
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
c  Parameters:
c
c    Input, real XVAL(3), YVAL(3), the X and Y coordinates
c    of the three sides of the triangle.
c
c    Input, integer NHATCH, the number of hatch lines to
c    be drawn.  Each side of the triangle, and NHATCH equally
c    spaced lines parallel to that side, and interior to
c    the triangle will be drawn, for a total of 3*(NHATCH+1) lines.
c
      implicit none

      integer i
      integer i1
      integer i2
      integer i3
      integer j
      integer nhatch
      real xhi
      real xlo
      real xval(3)
      real yhi
      real ylo
      real yval(3)

      do i = 1, 3
 
        i1 = i
 
        i2 = i + 1
        if ( i2 .gt. 3 ) then
          i2 = i2 - 3
        end if
 
        i3 = i + 2
        if ( i3 .gt. 3 ) then
          i3 = i3 - 3
        end if
 
        do j = 0, nhatch
 
          xlo = (j*xval(i1)+(nhatch+1-j)*xval(i2)) / real(nhatch+1)
          ylo = (j*yval(i1)+(nhatch+1-j)*yval(i2)) / real(nhatch+1)
          call movcgm ( xlo, ylo )
 
          xhi = (j*xval(i1)+(nhatch+1-j)*xval(i3)) / real(nhatch+1)
          yhi = (j*yval(i1)+(nhatch+1-j)*yval(i3)) / real(nhatch+1)
          call drwcgm ( xhi, yhi )
 
        end do
 
      end do
 
      return
      end
      subroutine wedge ( xcenter, ycenter, radius, angmin, angmax )

c*********************************************************************72
c
cc WEDGE draws a circular wedge of a given angular size and radius.
c
c  Discussion:
c
c    WEDGE calls the DRAWCGM routine PLYGON to draw the circular wedge.
c
c    To control the color of the filled in area, simply call the DRAWCGM
c    routine FILCLR before calling WEDGE.
c
c    If the X and Y coordinate systems do not have the same scale,
c    the wedge drawn will be distorted.  This can happen if the routine
c    SETWCD has been used to set the X and Y dimensions to different
c    extents.
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
c  Parameters:
c
c    Input, real XCENTER, YCENTER, the X and Y coordinates of
c    the center of the circle on which the wedge lies.
c
c    Input, real RADIUS, the radius of the circle on which
c    the wedge lies.
c
c    Input, real ANGMIN, ANGMAX, the minimum and maximum angles
c    of the circle.  ANGMIN and ANGMAX are both measured in degrees.
c    ANGMIN and ANGMAX determine the portion of the circle to be
c    drawn.  If ANGMIN=0.0 and ANGMAX=90.0, for instance, a wedge
c    of 90 degrees will be drawn.
c
      implicit none

      integer nval
      parameter ( nval = 65 )

      real pi
      parameter ( pi = 3.1415926535 )

      real degrad
      parameter ( degrad = pi / 180.0 )

      real angle
      real angmax
      real angmin
      integer i
      real radius
      real xcenter
      real xval(nval)
      real ycenter
      real yval(nval)

      if ( radius .le. 0.0 ) then
        return
      end if
c
c  Set up the data defining the circular arc, using NVAL equally
c  spaced points along the circumference.
c
      do i = 1, nval-1
 
        angle = degrad * ( real(nval-1-i)*angmin + real(i-1) * angmax )
     &    / real(nval-2)
 
        xval(i) = xcenter + radius * cos(angle)
        yval(i) = ycenter + radius * sin(angle)
 
      end do
 
      xval(nval) = xcenter
      yval(nval) = ycenter
c
c  Draw the circular wedge.
c
      call plygon ( nval, xval, yval )
 
      return
      end
      subroutine xtoix ( ix, ixmax, ixmin, x, xmax, xmin )

c*********************************************************************72
c
cc XTOIX maps real X in [XMIN, XMAX] to integer IX in [IXMIN, IXMAX].
c
c  Discussion:
c
c    The formula used is:
c
c      IX = IXMIN + (IXMAX-IXMIN) * (X-XMIN)/(XMAX-XMIN)
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
c  Parameters:
c
c    Output, integer IX, the corresponding integer value.
c
c    Input, integer IXMAX, IXMIN, the integer range.
c
c    Input, real X, the real number to be converted.
c
c    Input, real XMAX, XMIN, the real range.  XMAX and XMIN
c    must not be equal.
c
      implicit none

      integer ix
      integer ixmax
      integer ixmin
      real x
      real xmax
      real xmin

      if ( xmax .eq. xmin ) then
        write ( *, * ) ' '
        write ( *, * ) 'XTOIX - Fatal error!'
        write ( *, * ) '  XMAX = XMIN, making a zero divisor!'
        write ( *, * ) '  XMAX = ', xmax
        write ( *, * ) '  XMIN = ', xmin
        stop
      end if
 
      ix = nint ( real ( 
     &  ( ( xmax - x ) * ixmin + ( x - xmin ) * ixmax ) 
     &  / ( xmax - xmin ) ) )

      if ( ix .gt. ixmax ) then 
        ix = ixmax
      end if

      if ( ix .lt. ixmin ) then
        ix = ixmin
      end if
 
      return
      end
