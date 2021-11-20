C  THIS IS MTH:GRAFIK.FOR AS OF 19 NOVEMBER 1986,
C  CONTAINING VECTOR AND CONTOUR PLOTTING PROGRAMS.
C
C  LINKAGE:
C
C  DEC-10: (USER MAIN), MTH:GRAFIK, MTH:ANYPLT, PRG:TEKLIB/LIB, PRG:PLTLIB/LIB
C  MTH750: (USER MAIN), MTH:GRAFIK, MTH:ANYPLT, MTH:P10PLT, MTH:MTHPLT
C  CISVMS: (USER MAIN), MTH:GRAFIK, MTH:ANYPLT, MTH:P10PLT, PRG:PLTLIB/LIB
C
C  GRAFIK CONTAINS:
C
C  VCPLOT:  MAKES A PLOT OF A VECTOR QUANTITY.
C  COPLOT:  MAKES A CONTOUR PLOT OF A SCALAR QUANTITY.
C
C  IF YOU ARE USING THE CALCOMP OPTION,
C  ONCE VCPLOT OR COPLOT HAS BEEN CALLED, THE FILE WHOSE NAME WAS
C  GIVEN IN PLOTNM WILL HAVE BEEN CREATED.  ASSUMING
C  THE FILE NAME WAS 'PLOTFL.DAT', THEN IT MAY BE VIEWED IMMEDIATELY
C  USING THE TEKPLT PROGRAM BY TYPING:
C
C  .R TEKPLT     (ON MTH750:  RUN MTH:TEKPLT)
C  ENTER NAME OF FILE>PLOTFL.DAT
C  (PLOT WILL APPEAR ON SCOPE)
C
C  OR, ON THE CIS VAX, SEND IT TO THE PLOTTER VIA
C  .PLOT PLOTFL.DAT
C  OR, ON THE MTH VAX, PLOT IT USING THE PRTPLT PROGRAM,
C  WHICH IS DESCRIBED IN MTH:MTHPLT.HLP.
C
C  BE CAREFUL CHOOSING AN EXTENSION FOR YOUR PLOTFILE NAME.
C  FILES WITH EXTENSION 'PLT' ARE ASSUMED TO BE PLOTFILES
C  AND THE SYSTEM WILL SOMETIMES PLOT THEM AUTOMATICALLY,
C  AND WHENEVER A 'PLT' FILE IS PLOTTED, IT IS REMOVED FROM
C  YOUR ACCOUNT.
C
C  THE SIZE OF THE PLOT PRODUCED BY THE PLOTTER
C  WILL BE 6 INCHES SQUARE IF YOU USE THE DEFAULT OPTIONS.
C  YOU CAN CALL SUBROUTINE SETSIZ TO CHANGE THESE VALUES,
C  AND THEREAFTER, YOU CAN CALL SUBROUTINE DEFSIZ TO RESTORE
C  THE ORIGINAL DEFAULT VALUES.  THE CALL STATEMENTS ARE
C
C  CALL SETSIZ(XMARGL,XMARGR,XSIZE,YMARGB,YMARGT,YSIZE)
C
C  AND
C
C  CALL DEFSIZ
C
C  THE ACTUAL GRAPH WILL APPEAR IN THE BOX OF SIZE
C  XSIZE BY YSIZE.  THERE WILL BE LEFT ANR RIGHT X MARGINS
C  OF SIZE XMARGL AND XMARGR, AND TOP AND BOTTOM Y MARGINS
C  OF SIZE YMARGT AND YMARGB.  THE DEFAULTS ARE XMARGL=XMARGR=1,
C  YMARGT=YMARGB=1, XSIZE=YSIZE=5.
C
      SUBROUTINE COPLOT(NROW,NCOL,X,Y,VAL,FLAG,NLEVEL,VLEVEL,IOPT,XB,
     & YB,JOIN,NB,PLOTNM,TITLE,NCHAR,MAXROW,CALCOM,PLOT10)

c*********************************************************************72
C
cc COPLOT makes a contour plot of a scalar quantity.
c
c  Discussion:
c
C    COPLOT makes a contour plot of a scalar quantity,
c    and can add line segments to the plot to represent a frame,
c    internal obstacles, or other shapes.
C
C    BECAUSE OF THE WAY COPLOT INTERPOLATES THE INPUT DATA, THE
C    DATA SHOULD BE GIVEN ON A GRID OF POINTS, THAT IS, FOR
C    ROWS AND COLUMNS OF POINTS.  IT IS POSSIBLE TO FLAG CERTAIN
C    GRID POINTS WHICH CANNOT BE ASSIGNED A DATA VALUE, BY ASSIGNING
C    A FLAG VALUE TO THOSE POINTS CALLED 'FLAG'.
C    IF 'FLAG' IS NEGATIVE, THOSE POINTS ARE IGNORED.
C    IF 'FLAG' IS POSITIVE, THE PROGRAM TRIES TO INTERPOLATE DATA AT
C    THOSE POINTS.
C
C    FOR A GIVEN PAIR I AND J, THE THREE NUMBERS X(I), Y(J) AND VAL(I,J)
C    CORRESPOND TO THE SCALAR VALUE VAL(I,J) AT THE POINT (X(I),Y(J)).
C
C    THIS PROGRAM WAS WRITTEN BY JOHN BURKARDT, BASED ON
C    CODING CONTAINED IN THE DUVAL PLOTTING PROGRAM 'PAKAJ5'.
C
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
C
C  NROW   - THE NUMBER OF ROWS OF POINTS IN THE REGION.
C
C  NCOL   - THE NUMBER OF COLUMNS OF POINTS IN THE REGION.
C
C  X      - A VECTOR OF LENGTH NCOL, WHOSE I-TH ENTRY IS THE X POSITION
C           OF ALL POINTS IN THE I-TH COLUMN.
C
C  Y      - A VECTOR OF LENGTH NROW, WHOSE I-TH ENTRY IS THE Y POSITION
C           OF ALL POINTS IN THE I-TH ROW.
C
C  VAL    - AN ARRAY OF DIMENSION (NROW,NCOL).  VAL(I,J) IS THE VALUE
C           OF THE SCALAR QUANTITY TO BE PLOTTED AT THE POINT IN THE
C           I-TH ROW AND J-TH COLUMN.  BUT NOTE THAT IF VAL(I,J)=FLAG,
C           THEN THE POINT IN THE I-TH ROW AND J-TH COLUMN IS NOT USED
C           UNLESS FLAG.GE.0.0 AND COPLOT CAN INTERPOLATE A VALUE THERE.
C
C           NOTE THAT YOU MUST SPECIFY THE ACTUAL FIRST DIMENSION OF THE
C           ARRAY VAL WITH THE ARGUMENT MAXROW.  SEE BELOW.
C
C  FLAG   - A FLAG VALUE FOR THE ARRAY VAL.  IF FLAG.LT.0.0, THEN
C           ANY POINT WITH VAL(I,J)=FLAG IS IGNORED, AND NO CONTOURS
C           ARE DRAWN NEAR THAT POINT.  IF FLAG.GE.0.0, THEN ANY
C           POINT WITH VAL(I,J)=FLAG MAY HAVE A VALUE ASSIGNED
C           TO IT BY INTERPOLATION FROM OTHER NEARBY DATA POINTS.
C           FLAG VALUES SHOULD BE ASSIGNED TO POINTS
C           WHERE NO VALUE OF THE CONTOUR QUANTITY IS AVAILABLE.
C           FLAG SHOULD BE A NUMBER THAT IS NOT A VALUE OF ANY
C           LEGITIMATE DATA POINT.
C
C  NLEVEL - THE NUMBER OF CONTOUR LEVELS WANTED.  NLEVEL SHOULD BE
C           AT LEAST 1.
C
C  VLEVEL - A VECTOR OF A CERTAIN DIMENSION WHICH DEPENDS ON
C           THE VALUE OF IOPT CHOSEN.  IF IOPT=0 OR 1, NO INFORMATION
C           NEED BE STORED IN VLEVEL.  IT IS NOT USED.
C           IF IOPT=2 OR 3, THERE SHOULD BE 2 VALUES STORED IN VLEVEL.
C           IF IOPT=4, THERE SHOULD BE NLEVEL VALUES STORED IN VLEVEL.
C
C  IOPT   - THE CONTOUR PLOTTING OPTION.
C           THE USER CAN REQUEST CENTERED OR STAGGERED LEVELS,
C           BETWEEN THE MINIMUM AND MAXIMUM VALUES STORED IN THE
C           VAL ARRAY, OR SUPPLIED BY THE USER, OR THE USER CAN
C           SPECIFY ALL OF THE LEVELS.
C           CENTERED LEVELS INCLUDE THE ENDPOINTS, STAGGERED LEVELS
C           DO NOT.  FOR EXAMPLE:
C           IF THE MINIMUM FUNCTION VALUE WAS 3 AND THE MAXIMUM WAS
C           9, THEN IF 4 STAGGERED LEVELS WERE TO BE DRAWN, THESE
C           LEVELS WOULD CORRESPOND TO THE VALUES 5,6,7,8.
C           FOUR CENTERED LEVELS WOULD CORRESPOND TO THE VALUES
C           3, 5, 7, 9.
C           ASSUME THAT VMIN AND VMAX ARE THE MINIMUM AND MAXIMUM
C           VALUES ASSUMED BY THE FUNCTION AS GIVEN IN THE ARRAY
C           VAL (EXCLUDING VALUES EQUAL TO FLAG).  THEN
C
C           IOPT=0 MEANS THE PROGRAM IS TO DRAW NLEVEL EVENLY SPACED
C                  CENTERED CONTOUR LEVELS BETWEEN VMIN AND VMAX.
C                  (VMIN AND VMAX ARE NOT INCLUDED)
C
C           IOPT=1 MEANS THE PROGRAM IS TO DRAW NLEVEL EVENLY SPACED
C                  STAGGERED CONTOUR LEVELS, INCLUDING THE ENDPOINTS
C                  VMIN AND VMAX.
C
C           IOPT=2 MEANS THE PROGRAM IS TO DRAW NLEVEL EVENLY SPACED
C                  CENTERED CONTOUR LEVELS, EVENLY SPACED BETWEEN
C                  VLEVEL(1) AND VLEVEL(2).
C
C           IOPT=3 MEANS THE PROGRAM IS TO DRAW NLEVEL EVENLY SPACED
C                  STAGGERED CONTOUR LEVELS, INCLUDING THE ENDPOINTS
C                  STORED IN VLEVEL(1) AND VLEVEL(2).
C
C           IOPT=4 MEANS THE PROGRAM IS TO DRAW NLEVEL CONTOUR LEVELS
C                  WHOSE VALUES ARE STORED IN VLEVEL(1) THROUGH
C                  VLEVEL(NLEVEL).
C
C  XB     - A VECTOR OF LENGTH NB, CONTAINING THE X COORDINATES
C           OF CERTAIN ENDPOINTS OF LINE SEGMENTS TO BE DRAWN.
C           THESE POINTS MAY REPRESENT BOUNDARIES OR INTERNAL OBSTACLES
C           OR ANY SORT OF FIGURE OR SHAPE.
C
C  YB     - A VECTOR OF LENGTH NB, CONTAINING THE Y COORDINATES
C           OF CERTAIN ENDPOINTS OF LINE SEGMENTS TO BE DRAWN.
C           THESE POINTS MAY REPRESENT BOUNDARIES OR INTERNAL OBSTACLES
C           OR ANY SORT OF FIGURE OR SHAPE.
C
C  JOIN   - A VECTOR OF LENGTH NB, CONTAINING 1'S AND
C           0'S.  IF (JOIN(I).EQ.1) THEN THE PROGRAM WILL DRAW
C           A LINE FROM (XB(I),YB(I)) TO (XB(I+1),YB(I+1)).
C           IF (JOIN(NB).EQ.1), THE PROGRAM WILL DRAW
C           A LINE FROM (XB(NB),YB(NB)) TO (XB(1),YB(1)).
C
C  NB     - THE NUMBER OF POINTS USED TO DRAW THE LINE SEGMENTS.
C           THE DIMENSION OF XB, YB AND JOIN.  IF NB.LE.1,
C           NO SUCH LINES WILL BE DRAWN.
C
C  PLOTNM - A CHARACTER*30 VARIABLE CONTAINING A NAME FOR THE
C           PLOT FILE TO BE CREATED IF CALCOMP ROUTINES ARE USED.
C           FOR EXAMPLE:  CHARACTER*30 PLOTNM
C                         PLOTNM='PLOTFL.DAT'
C
C  TITLE  - A CHARACTER*80 VARIABLE CONTAINING CHARACTER DATA TO
C           BE USED AS A TITLE.
C
C  NCHAR  - THE NUMBER OF CHARACTERS IN THE TITLE.  NCHAR
C           SHOULD NOT BE MORE THAN 80.
C
C  MAXROW - THE ACTUAL FIRST DIMENSION OF THE ARRAY VAL
C           IF YOU HAVE AN 11 BY 7 GRID, BUT YOU STORE
C           THE INFORMATION IN A 16 BY 9 MATRIX, YOU MUST
C           PASS THE VALUE MAXROW=16 IN ORDER FOR THE PROGRAM TO
C           RETRIEVE THE INFORMATION CORRECTLY.
C
C  CALCOM - A LOGICAL VARIABLE WHICH IS .TRUE. IF YOU ARE GOING
C           TO USE THE CALCOMP PLOTTER ROUTINES.
C
C  PLOT10 - A LOGICAL VARIABLE WHICH IS .TRUE. IF YOU ARE GOING
C           TO USE THE PLOT10 PLOTTER ROUTINES TO DRAW THE PLOTS
C           IMMEDIATELY ON YOUR 4010 TERMINAL.
C
      logical   calcom
      logical   debug
      dimension join(nb)
      character plotnm*30
      logical   plot10
      character title*80
      dimension val(maxrow,ncol)
      dimension vlevel(nlevel)
      dimension x(ncol)
      dimension xb(nb)
      dimension y(nrow)
      dimension yb(nb)

      character carray*80
      logical   lplt1
      logical   lplt2
      logical   lplt3
      logical   lplt4

      common /anycom/ iarray(80),iplt1,iplt2,ixplt1,ixplt2,iyplt1,
     1                iyplt2,lplt1,lplt2,lplt3,lplt4,marray,narray,
     2                plt1,plt2,xplt1,xplt2,yplt1,yplt2
      common /anychr/ carray
      common /option/ xmrgl,xmrgr,xmxrng,ymrgb,ymrgt,ymxrng
C
C  Initialize plotter
c
      lplt1=calcom
      lplt2=plot10
      debug=(.not.plot10).and.(.not.calcom)
      lplt3=debug
      lplt4=.false.
      icom=0
      ixplt1=5
      ixplt2=585
      iyplt1=5
      iyplt2=758
      call anyplt(icom)
      icom=2
      carray=plotnm
      call anyplt(icom)
C
C  Define a plot box by finding largest and smallest
c  X and Y coordinates
c
      xmax=x(1)
      xmin=xmax
      ymax=y(1)
      ymin=ymax
      vmax=flag
      vmin=flag
      do i=1,ncol
        if(xmax.lt.x(i))xmax=x(i)
        if(xmin.gt.x(i))xmin=x(i)
      end do

      do i=1,nrow
        if(ymax.lt.y(i))ymax=y(i)
        if(ymin.gt.y(i))ymin=y(i)
      end do
c
c  Try to assign values to some data points
c  which are set to flag value for now
c
      if(flag.ge.0.0)call smooth(x,ncol,y,nrow,val,flag,maxrow)
c
c  Get maximum and minimum values of function
c
      do i=1,nrow
        do j=1,ncol
          vtemp=val(i,j)
          if(vtemp.ne.flag) then
            if(vmax.eq.flag)vmax=vtemp
            if(vmin.eq.flag)vmin=vtemp
            vmax=max(vtemp,vmax)
            vmin=min(vtemp,vmin)
          end if
        end do
      end do

      if(nb.gt.1)then
        do i=1,nb
          if(xmax.lt.xb(i))xmax=xb(i)
          if(xmin.gt.xb(i))xmin=xb(i)
          if(ymax.lt.yb(i))ymax=yb(i)
          if(ymin.gt.yb(i))ymin=yb(i)
        end do
      end if

      xrange=xmax-xmin
      if(xrange.le.0.0)xrange=xmxrng
      yrange=ymax-ymin
      if(yrange.le.0.0)yrange=ymxrng
      xskal=xmxrng/xrange
      yskal=ymxrng/yrange
      pskal=min(xskal,yskal)
C
C  Scale plot region so that graph is contained in
c  box of width XMXRNG and height YMXRNG, and whole plot is
c  in box of size XMRGL+XMXRNG+XMRGR by YMRGT+YMXRNG+YMRGB
C
      delx=0.5*(xmxrng-pskal*xrange)
      dely=0.5*(ymxrng-pskal*yrange)
      x1min=pskal*xmin-delx-xmrgl
      x1rng=xmxrng+xmrgl+xmrgr
      x1max=x1min+x1rng
      y1min=pskal*ymin-dely-ymrgb
      y1rng=ymxrng+ymrgb+ymrgt
      y1max=y1min+y1rng
c
c  Set size of graph
c
      icom=3
      xplt1=x1min
      xplt2=x1rng
      yplt1=y1min
      yplt2=y1rng
      call anyplt(icom)
c
c  Frame plot
c
      icom=4
      xplt1=x1min
      yplt1=y1min
      call anyplt(icom)
      icom=5
      xplt1=x1max
      call anyplt(icom)
      yplt1=y1max
      call anyplt(icom)
      xplt1=x1min
      call anyplt(icom)
      yplt1=y1min
      call anyplt(icom)
      icom=4
      xplt1=xmin*pskal
      yplt1=ymin*pskal
      call anyplt(icom)
      icom=5
      xplt1=xmax*pskal
      call anyplt(icom)
      yplt1=ymax*pskal
      call anyplt(icom)
      xplt1=xmin*pskal
      call anyplt(icom)
      yplt1=ymin*pskal
      call anyplt(icom)
c
c  Label plot
c
      marray=nchar
      xplt2=0.08
      yplt2=0.0
      xplt1=x1min+0.5*x1rng-0.5*xplt2*real(nchar)
      yplt1=y1max-2.0*xplt2
      carray=title
      icom=7
      if(nchar.gt.0)call anyplt(icom)
C
C  if ( 1 < NB ), draw the boundary
C
      if(nb.gt.1)then
        do i=1,nb
          if(join(i).eq.1)then
            icom=4
            xplt1=pskal*xb(i)
            yplt1=pskal*yb(i)
            call anyplt(icom)
            ip1=i+1
            if(i.eq.nb)ip1=1
            icom=5
            xplt1=pskal*xb(ip1)
            yplt1=pskal*yb(ip1)
            call anyplt(icom)
          end if
        end do
      end if
C
C  Figure out how to generate levels
c  and repeatedly call contour drawing routine
c  for each level
C
      if(iopt.lt.0.or.iopt.gt.4)iopt=0

      if(iopt.eq.0)then
        del=(vmax-vmin)/real(nlevel+1)
        vlo=vmin+del
        vhi=vmax-del
      end if

      if(iopt.eq.1)then
        vlo=vmin
        vhi=vmax
      end if

      if(iopt.eq.2)then
        del=(vlevel(2)-vlevel(1))/real(nlevel+1)
        vlo=vlevel(1)+del
        vhi=vlevel(2)-del
      end if

      if(iopt.eq.3)then
        del=(vlevel(2)-vlevel(1))/real(nlevel-1)
        vlo=vlevel(1)
        vhi=vlevel(2)
      end if

      if(iopt.eq.4)go to 80
C
C  Generate even levels
C
      if(nlevel.eq.1)then
        vlevel(1)=0.5*(vlo+vhi)
        go to 80
      end if

      do i=1,nlevel
        vlevel(i)=(real(i-1)*vlo+real(nlevel-i)*vhi)/real(nlevel-1)
      end do

   80 continue
      call relief(nrow,ncol,val,x,y,vlevel,nlevel,flag,pskal,maxrow)
      icom=9
      call anyplt(icom)

      if(calcom) then
        write(6,1020)plotnm
      end if

      return
 1000 format(a1)
 1020 format(' COPLOT created plotfile ',a30)
      end
      subroutine defsiz ( )

c*********************************************************************72
c
cc DEFSIZ restores the default plot sizes.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      COMMON /OPTION/ XMRGL,XMRGR,XMXRNG,YMRGB,YMRGT,YMXRNG

      XMRGL=0.5
      XMRGR=0.5
      XMXRNG=5.0
      YMRGT=0.5
      YMRGB=0.5
      YMXRNG=5.0

      RETURN
      END
      SUBROUTINE QUAD(X1,X2,X3,X4,Y1,Y2,Y3,Y4,V1,V2,V3,V4,CONVAL,
     & PSKAL)

c*********************************************************************72
C
cc QUAD divides a quadrilateral into triangles for contour searches.
c
c  Discussion:
c
C    ILLUSTRATION OF THE DISSECTION CARRIED OUT BY QUAD
C    GIVEN THE PREVIOUS DISSECTION BY RELIEF.  
c
c    EACH CALL TO QUAD PRODUCES ONE OF THE SUBSQUARES.
C
C    N1------N6------N2
C    |       |       |
C    |   N10 |   N11 |
C    |       |       |
C    N5------N9------N7
C    |       |       |
C    |   N12 |   N13 |
C    |       |       |
C    N4------N8------N3
C
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      real conval
      real pskal
      real v1
      real v2
      real v3
      real v4
      real v5
      real vt1
      real vt2
      real vt3
      real vt4
      real x1
      real x2
      real x3
      real x4
      real x5
      real y1
      real y2
      real y3
      real y4
      real y5

      VT1 = V1 - CONVAL
      VT2 = V2 - CONVAL
      VT3 = V3 - CONVAL
      VT4 = V4 - CONVAL

      IF ( 0.0 .lt. VT1 * VT2 ) then
        IF ( 0.0 .lt. VT1 * VT3 ) then
          IF ( 0.0 .lt. VT1 * VT4 ) then
            return
          end if
        end if
      end if

      X5=.25*(X1+X2+X3+X4)
      Y5=.25*(Y1+Y2+Y3+Y4)
      V5=.25*(V1+V2+V3+V4)

      CALL TRIANG(X1,X2,X5,Y1,Y2,Y5,V1,V2,V5,PSKAL,CONVAL)
      CALL TRIANG(X2,X3,X5,Y2,Y3,Y5,V2,V3,V5,PSKAL,CONVAL)
      CALL TRIANG(X3,X4,X5,Y3,Y4,Y5,V3,V4,V5,PSKAL,CONVAL)
      CALL TRIANG(X4,X1,X5,Y4,Y1,Y5,V4,V1,V5,PSKAL,CONVAL)

      RETURN
      END
      SUBROUTINE RELIEF(NROW,NCOL,VAL,X,Y,VLEVEL,NLEVEL,FLAG,PSKAL,
     & MAXROW)

c*********************************************************************72
C
cc RELIEF interpolates data on a subdivision of a square.
c
c  Discussion:
c
C    THE FOLLOWING DIAGRAM ILLUSTRATES THE DISSECTION OF THE
C    REGION THAT RELIEF CARRIES OUT:
C
C     NODE  = N1     N6     N2 = NODEE
C  
C             N5     N9     N7
C
C     NODES = N4     N8     N3 = NODESE
C
C    THE LOCATIONS AND INTERPOLATED VALUES AT N5, N6, N7, N8 AND N9
C    ARE COMPUTED, AND THEN FURTHER DISSECTION IS CARRIED OUT
C    BY QUAD.
C
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      integer maxrow
      integer ncol
      integer nlevel

      DIMENSION VAL(MAXROW,NCOL)
      DIMENSION VLEVEL(NLEVEL)
      DIMENSION X(NCOL)
      DIMENSION Y(NROW)

      NROWM1=NROW-1
      NCOLM1=NCOL-1

      DO LEVELI=1,NLEVEL

        CONVAL=VLEVEL(LEVELI)
        YPOS=Y(1)

        DO I=1,NROWM1

          IP1=I+1
          YNPOS=Y(IP1)
          XPOS=X(1)

          DO J=1,NCOLM1

            JP1=J+1
            XNPOS=X(JP1)
C
C  SET COORDINATES OF THE SQUARE
C
            XMID=0.5*(XPOS+XNPOS)
            YMID=.5*(YPOS+YNPOS)
            VS1=VAL(I,J)
            IF(VS1.EQ.FLAG)GO TO 10
            VS2=VAL(I,JP1)
            IF(VS2.EQ.FLAG)GO TO 10
            VS3=VAL(IP1,JP1)
            IF(VS3.EQ.FLAG)GO TO 10
            VS4=VAL(IP1,J)
            IF(VS4.EQ.FLAG)GO TO 10
            VS5=.5*(VS1+VS4)
            VS6=.5*(VS1+VS2)
            VS7=.5*(VS2+VS3)
            VS8=.5*(VS3+VS4)
            VS9=.25*(VS1+VS2+VS3+VS4)
            CALL QUAD(XPOS,XMID,XMID,XPOS,YPOS,YPOS,YMID,YMID,VS1,VS6,
     &      VS9,VS5,CONVAL,PSKAL)
            CALL QUAD(XNPOS,XMID,XMID,XNPOS,YPOS,YPOS,YMID,YMID,VS2,VS6,
     &      VS9,VS7,CONVAL,PSKAL)
            CALL QUAD(XNPOS,XMID,XMID,XNPOS,YNPOS,YNPOS,YMID,YMID,VS3,
     &      VS8,VS9,VS7,CONVAL,PSKAL)
            CALL QUAD(XPOS,XMID,XMID,XPOS,YNPOS,YNPOS,YMID,YMID,VS4,VS8,
     &      VS9,VS5,CONVAL,PSKAL)

   10       continue
            XPOS=XNPOS
          end do

          YPOS=YNPOS

        end do

      end do

      RETURN
      END
      SUBROUTINE SETSIZ(XMARGL,XMARGR,XSIZE,YMARGB,YMARGT,YSIZE)

c*********************************************************************72
C
cc SETSIZ allows the user to change the size of the plot.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      COMMON /OPTION/ XMRGL,XMRGR,XMXRNG,YMRGB,YMRGT,YMXRNG

      IF(XSIZE.LT.1.0)THEN
        WRITE(6,1000)XSIZE
      ELSE
        XMXRNG=XSIZE
      end if

      IF(YSIZE.LT.1.0)THEN
        WRITE(6,1010)YSIZE
      ELSE
        YMXRNG=YSIZE
      end if

      IF(XMARGL.LT.0.0)THEN
        WRITE(6,1020)XMARGL
      ELSE
        XMRGL=XMARGL
      end if

      IF(XMARGR.LT.0.0)THEN
        WRITE(6,1030)XMARGR
      ELSE
        XMRGR=XMARGR
      end if

      IF(YMARGB.LT.0.0)THEN
        WRITE(6,1040)YMARGB
      ELSE
        YMRGB=YMARGB
      end if

      IF(YMARGT.LT.0.0)THEN
        WRITE(6,1050)YMARGT
      ELSE
        YMRGT=YMARGT
      end if

      RETURN
 1000 FORMAT(' SETSCL - XSIZE=',G14.6,' IS TOO SMALL')
 1010 FORMAT(' SETSCL - YSIZE=',G14.6,' IS TOO SMALL')
 1020 FORMAT(' SETSCL - XMARGL=',G14.6,' NEGATIVE')
 1030 FORMAT(' SETSCL - XMARGR=',G14.6,' NEGATIVE')
 1040 FORMAT(' SETSCL - YMARGB=',G14.6,' NEGATIVE')
 1050 FORMAT(' SETSCL - YMARGT=',G14.6,' NEGATIVE')
      END
      SUBROUTINE SMOOTH(X,NCOL,Y,NROW,VAL,FLAG,MAXROW)

c*********************************************************************72
c
cc SMOOTH interpolates missing data.
c
c  Discussion:
c
C    THIS ROUTINE ATTEMPTS TO INTERPOLATE VALUES AT NODES
C    WHOSE CURRENT VALUE IS A FLAG VALUE.  IT WILL INTERPOLATE
C    IF IT CAN FIND FOUR NEIGHBORS, NORTH, SOUTH, EAST AND WEST,
C    OR EAST AND WEST, OR NORTH AND SOUTH.
c
C    TWO PASSES ARE MADE.
C
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      integer maxrow
      integer ncol
      integer nrow

      integer i
      integer ie
      integer in
      integer is
      integer iw
      integer j
      real VAL(MAXROW,NCOL)
      real X(NCOL)
      real Y(NROW)


      DO IPASS=1,2

        DO I=1,NROW

          DO 110 J=1,NCOL

            IF(VAL(I,J).NE.FLAG)GO TO 110
C
C  SEEK 4 NEIGHBORS
C
            IN=I
   10       CONTINUE
            IN=IN-1
            IF(IN.LT.1)GO TO 20
            IF(VAL(IN,J).EQ.FLAG)GO TO 10
   20       CONTINUE
            IS=I
   30       CONTINUE
            IS=IS+1
            IF(IS.GT.NROW)GO TO 40
            IF(VAL(IS,J).EQ.FLAG)GO TO 30
   40       CONTINUE
            IE=J
   50       IE=IE+1
            IF(IE.GT.NCOL)GO TO 60
            IF(VAL(I,IE).EQ.FLAG)GO TO 50
   60       IW=J
   70       IW=IW-1
            IF(IW.LT.1)GO TO 80
            IF(VAL(I,IW).EQ.FLAG)GO TO 70
   80       CONTINUE
C
C  CASE 1, FOUND IN, IS, IE, IW
C
            IF(IN.LT.1.OR.IS.GT.NROW)GO TO 90
            IF(IW.LT.1.OR.IE.GT.NCOL)GO TO 100
C
C  COMPUTE SPACINGS
C
            HE=X(IE)-X(J)
            HW=X(J)-X(IW)
            HN=Y(IN)-Y(I)
            HS=Y(I)-Y(IS)
            VN=VAL(IN,J)
            VS=VAL(IS,J)
            VE=VAL(I,IE)
            VW=VAL(I,IW)
            VALUE=(HE+HW)*HE*HW*HS*VN
     &           +(HE+HW)*HE*HW*HN*VS
     &           +(HN+HS)*HN*HS*HW*VE
     &           +(HN+HS)*HN*HS*HE*VW
            BOT=(HE*HW+HN*HS)*(HE+HW)*(HS+HN)
            VALUE=VALUE/BOT
            VAL(I,J)=VALUE
            GO TO 110
C
C  CASE 2, NORTH OR SOUTH MISSING, EAST AND WEST OK
C
   90       IF(IW.LT.1.OR.IE.GT.NCOL)GO TO 110
            HE=X(IE)-X(J)
            HW=X(J)-X(IW)
            VE=VAL(I,IE)
            VW=VAL(I,IW)
            VALUE=(HE*VW+HW*VE)/(HE+HW)
            VAL(I,J)=VALUE
            GO TO 110
C
C  CASE 3, EAST OR WEST MISSING, NORTH AND SOUTH OK
C
  100       HN=Y(IN)-Y(I)
            HS=Y(I)-Y(IS)
            VN=VAL(IN,J)
            VS=VAL(IS,J)
            VALUE=(HN*VS+HS*VN)/(HN+HS)
            VAL(I,J)=VALUE
            GO TO 110
  110     CONTINUE

        end do
      end do

      RETURN
      END
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
      SUBROUTINE TRIANG(X1,X2,X3,Y1,Y2,Y3,V1,V2,V3,PSKAL,CONVAL)

c*********************************************************************72
C
cc TRIANG searches for contour lines in a triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 March 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      CHARACTER*80 CARRAY
      LOGICAL   LPLT1
      LOGICAL   LPLT2
      LOGICAL   LPLT3
      LOGICAL   LPLT4
      real px(3)
      real py(3)
      real x1
      real x2
      real x3
      real x4

      COMMON /ANYCOM/ IARRAY(80),IPLT1,IPLT2,IXPLT1,IXPLT2,IYPLT1,
     1                IYPLT2,LPLT1,LPLT2,LPLT3,LPLT4,MARRAY,NARRAY,
     2                PLT1,PLT2,XPLT1,XPLT2,YPLT1,YPLT2
      COMMON /ANYCHR/ CARRAY

      KROSS=0
      VT1=V1-CONVAL
      VT2=V2-CONVAL
      VT3=V3-CONVAL
C
C  CHECK FOR CONTOUR LINE CROSSING (X1,Y1), (X3,Y3)
C
      IF(VT1.EQ.0.0.AND.VT3.EQ.0.0)THEN
        ICOM=4
        XPLT1=X1*PSKAL
        YPLT1=Y1*PSKAL
        CALL ANYPLT(ICOM)
        ICOM=5
        XPLT1=X3*PSKAL
        YPLT1=Y3*PSKAL
        CALL ANYPLT(ICOM)
      end if

      IF((VT1*VT3.LT.0.0).OR.
     &   (VT1.EQ.0.0.AND.VT3.NE.0.0).OR.
     &   (VT1.NE.0.0.AND.VT3.EQ.0.0))THEN
        KROSS=KROSS+1
        ALP=(CONVAL-V1)/(V3-V1)
        PX(KROSS)=X1+(X3-X1)*ALP
        PY(KROSS)=Y1+(Y3-Y1)*ALP
      end if
C
C  CHECK FOR CONTOUR LINE CROSSING (X2,Y2) (X3,Y3)
C
      IF(VT2.EQ.0.0.AND.VT3.EQ.0.0)THEN
        ICOM=4
        XPLT1=X2*PSKAL
        YPLT1=Y2*PSKAL
        CALL ANYPLT(ICOM)
        ICOM=5
        XPLT1=X3*PSKAL
        YPLT1=Y3*PSKAL
        CALL ANYPLT(ICOM)
      end if

      IF((VT2*VT3.LT.0.0).OR.
     &   (VT2.EQ.0.0.AND.VT3.NE.0.0).OR.
     &   (VT2.NE.0.0.AND.VT3.EQ.0.0))THEN
        KROSS=KROSS+1
        ALP=(CONVAL-V3)/(V2-V3)
        PX(KROSS)=X3+(X2-X3)*ALP
        PY(KROSS)=Y3+(Y2-Y3)*ALP
      end if
C
C  CHECK FOR CONTOUR LINE CROSSING (X1,Y1) (X2,Y2)
C
      IF(VT2.EQ.0.0.AND.VT1.EQ.0.0)THEN
        ICOM=4
        XPLT1=X2*PSKAL
        YPLT1=Y2*PSKAL
        CALL ANYPLT(ICOM)
        ICOM=5
        XPLT1=X1*PSKAL
        YPLT1=Y1*PSKAL
        CALL ANYPLT(ICOM)
      end if

      IF((VT2*VT1.LT.0.0).OR.
     &   (VT2.EQ.0.0.AND.VT1.NE.0.0).OR.
     &   (VT2.NE.0.0.AND.VT1.EQ.0.0))THEN
        KROSS=KROSS+1
        ALP=(CONVAL-V2)/(V1-V2)
        PX(KROSS)=X2+(X1-X2)*ALP
        PY(KROSS)=Y2+(Y1-Y2)*ALP
      end if
C
C  DRAW VALUES IMMEDIATELY
C
      IF ( 2 .le. KROSS ) then

        ICOM=4
        XPLT1=PX(1)*PSKAL
        YPLT1=PY(1)*PSKAL
        CALL ANYPLT(ICOM)

        ICOM=5
        XPLT1=PX(2)*PSKAL
        YPLT1=PY(2)*PSKAL
        CALL ANYPLT(ICOM)

      end if

      RETURN
      END
      SUBROUTINE VCPLOT(X,Y,V1,V2,NVAL,XB,YB,JOIN,NB,PLOTNM,
     & TITLE,NCHAR,VSCALE,CALCOM,PLOT10)

c*********************************************************************72
C
cc VCPLOT plots a vector quantity.
c
c  Discussion:
c
c    The routine can also add line segments to the plot to represent
c    a frame, internal obstacles, or other shapes.
c
C    FOR A GIVEN INDEX I, THE FOUR VECTORS X, Y, V1, AND V2 SHOULD
C    CORRESPOND TO THE VECTOR QUANTITY (V1(I),V2(I)) AT (X(I),Y(I)).
C
C    THIS PROGRAM WAS WRITTEN BY JOHN BURKARDT, BASED ON
C    CODING CONTAINED IN THE DUVAL PLOTTING PROGRAM 'PAKAJ5'.
C
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
C
C    Input, real X(NVAL), THE X COORDINATES OF THE POINTS WHERE THE 
c    VECTOR IS GIVEN.
C
C    Input, real Y(NVAL), THE Y COORDINATES OF THE POINTS WHERE THE 
c    VECTOR IS GIVEN.
C
C  V1     - A VECTOR OF LENGTH NVAL, CONTAINING THE X
C           COMPONENTS OF THE VECTOR QUANTITY.
C
C  V2     - A VECTOR OF LENGTH NVAL, CONTAINING THE Y
C           COMPONENTS OF THE VECTOR QUANTITY.
C
C  NVAL   - THE DIMENSION OF X, Y, V1 AND V2, THE NUMBER OF POINTS
C           AT WHICH THE VECTOR QUANTITY IS GIVEN.
C
C  XB     - A VECTOR OF LENGTH NB, CONTAINING THE X COORDINATES
C           OF CERTAIN ENDPOINTS OF LINE SEGMENTS TO BE DRAWN.
C           THESE POINTS MAY REPRESENT BOUNDARIES OR INTERNAL OBSTACLES
C           OR ANY SORT OF FIGURE OR SHAPE.
C
C  YB     - A VECTOR OF LENGTH NB, CONTAINING THE Y COORDINATES
C           OF CERTAIN ENDPOINTS OF LINE SEGMENTS TO BE DRAWN.
C           THESE POINTS MAY REPRESENT BOUNDARIES OR INTERNAL OBSTACLES
C           OR ANY SORT OF FIGURE OR SHAPE.
C
C  JOIN   - A VECTOR OF LENGTH NB, CONTAINING 1'S AND
C           0'S.  IF (JOIN(I).EQ.1) THEN THE PROGRAM WILL DRAW
C           A LINE FROM (XB(I),YB(I)) TO (XB(I+1),YB(I+1)).
C           IF (JOIN(NB).EQ.1), THE PROGRAM WILL DRAW
C           A LINE FROM (XB(NB),YB(NB)) TO (XB(1),YB(1)).
C
C  NB     - THE NUMBER OF POINTS USED TO DRAW THE LINE SEGMENTS.
C           THE DIMENSION OF XB, YB AND JOIN.  IF NB.LE.1, NO
C           SUCH LINES ARE DRAWN.
C
C  PLOTNM - A CHARACTER*30 VARIABLE CONTAINING A NAME FOR THE
C           PLOT FILE TO BE CREATED IF CALCOMP ROUTINES ARE USED.
C           FOR EXAMPLE:  CHARACTER*30 PLOTNM
C                         PLOTNM='PLOTFL.DAT'
C
C  TITLE  - A CHARACTER*80 VARIABLE CONTAINING CHARACTER DATA TO
C           BE USED AS A TITLE.
C
C  NCHAR  - THE NUMBER OF CHARACTERS IN THE TITLE.  NCHAR
C           SHOULD NOT BE MORE THAN 80.
C
C  VSCALE - A SCALE FACTOR FOR THE VECTORS.  NORMALLY, VSCALE=1.
C           NEGATIVE VALUES REVERSE THE VECTORS.  SMALL
C           VALUES SHRINK, AND LARGE VALUES MAGNIFY THE VECTORS.
C
C  CALCOM - A LOGICAL VARIABLE.  IF CALCOM=.TRUE., A CALCOMP
C           FILE WILL BE CREATED.
C           YOU MUST SET CALCOM=.FALSE. IF CALCOMP PLOTTING
C           ROUTINES ARE NOT AVAILABLE.
C
C  PLOT10 - A LOGICAL VARIABLE.  IF PLOT10=.TRUE., A PLOT10
C           PLOT WILL BE DRAWN ON YOUR 4010 SCREEN AS THE
C           PROGRAM RUNS.
C
      LOGICAL   CALCOM
      LOGICAL   DEBUG
      CHARACTER ISAY*1
      DIMENSION JOIN(NB)
      CHARACTER PLOTNM*30
      LOGICAL   PLOT10
      CHARACTER TITLE*80
      DIMENSION V1(NVAL)
      DIMENSION V2(NVAL)
      DIMENSION X(NVAL)
      DIMENSION XB(NB)
      DIMENSION Y(NVAL)
      DIMENSION YB(NB)

      CHARACTER CARRAY*80
      LOGICAL   LPLT1
      LOGICAL   LPLT2
      LOGICAL   LPLT3
      LOGICAL   LPLT4

      COMMON /ANYCOM/ IARRAY(80),IPLT1,IPLT2,IXPLT1,IXPLT2,IYPLT1,
     1                IYPLT2,LPLT1,LPLT2,LPLT3,LPLT4,MARRAY,NARRAY,
     2                PLT1,PLT2,XPLT1,XPLT2,YPLT1,YPLT2
      COMMON /ANYCHR/ CARRAY
      COMMON /OPTION/ XMRGL,XMRGR,XMXRNG,YMRGB,YMRGT,YMXRNG
C
C  INITIALIZE PLOTTER
C
      LPLT1=CALCOM
      LPLT2=PLOT10
      DEBUG=(.NOT.PLOT10).AND.(.NOT.CALCOM)
      LPLT3=DEBUG
      LPLT4=.FALSE.
      ICOM=0
      IXPLT1=5
      IXPLT2=585
      IYPLT1=5
      IYPLT2=758
      CALL ANYPLT(ICOM)
      ICOM=2
      CARRAY=PLOTNM
      CALL ANYPLT(ICOM)
C
C  SET CONSTANTS
C
      PI=4.0*ATAN2(1.0,1.0)
      THETA=0.5*PI-ATAN2(2.0,1.0)
C
C  DEFINE A PLOT BOX BY FINDING LARGEST AND SMALLEST
C  X AND Y COORDINATES
C
      XMAX=X(1)
      XMIN=XMAX
      YMAX=Y(1)
      YMIN=YMAX
      V1MAX=V1(1)
      V2MAX=V2(1)

      DO I=1,NVAL
        IF(XMAX.LT.X(I))XMAX=X(I)
        IF(XMAX.LT.X(I)+VSCALE*V1(I))XMAX=X(I)+VSCALE*V1(I)
        IF(XMIN.GT.X(I))XMIN=X(I)
        IF(XMIN.GT.X(I)+VSCALE*V1(I))XMIN=X(I)+VSCALE*V1(I)
        IF(YMAX.LT.Y(I))YMAX=Y(I)
        IF(YMAX.LT.Y(I)+VSCALE*V2(I))YMAX=Y(I)+VSCALE*V2(I)
        IF(YMIN.GT.Y(I))YMIN=Y(I)
        IF(YMIN.GT.Y(I)+VSCALE*V2(I))YMIN=Y(I)+VSCALE*V2(I)
      end do

      IF(NB.GT.1)THEN
        DO I=1,NB
          IF(XMAX.LT.XB(I))XMAX=XB(I)
          IF(XMIN.GT.XB(I))XMIN=XB(I)
          IF(YMAX.LT.YB(I))YMAX=YB(I)
          IF(YMIN.GT.YB(I))YMIN=YB(I)
        end do
      end if

      XRANGE=XMAX-XMIN
      IF(XRANGE.LE.0.0)XRANGE=XMXRNG
      YRANGE=YMAX-YMIN
      IF(YRANGE.LE.0.0)YRANGE=YMXRNG
      XSKAL=XMXRNG/XRANGE
      YSKAL=YMXRNG/YRANGE
      PSKAL=MIN(XSKAL,YSKAL)
C
C  DEFINE SCALED PLOT REGION SO THAT GRAPH IS CONTAINED IN
C  BOX OF WIDTH XMXRNG AND HEIGHT YMXRNG, AND WHOLE PLOT IS
C  IN BOX OF SIZE XMRGL+XMXRNG+XMRGR BY YMRGT+YMXRNG+YMRGB
C
      DELX=0.5*(XMXRNG-PSKAL*XRANGE)
      DELY=0.5*(YMXRNG-PSKAL*YRANGE)
      X1MIN=PSKAL*XMIN-DELX-XMRGL
      X1RNG=XMXRNG+XMRGL+XMRGR
      X1MAX=X1MIN+X1RNG
      Y1MIN=PSKAL*YMIN-DELY-YMRGB
      Y1RNG=YMXRNG+YMRGB+YMRGT
      Y1MAX=Y1MIN+Y1RNG
C
C  SET SIZE OF GRAPH
C
      ICOM=3
      XPLT1=X1MIN
      XPLT2=X1RNG
      YPLT1=Y1MIN
      YPLT2=Y1RNG
      CALL ANYPLT(ICOM)
C
C  FRAME PLOT
C
      ICOM=4
      XPLT1=X1MIN
      YPLT1=Y1MIN
      CALL ANYPLT(ICOM)
      ICOM=5
      XPLT1=X1MAX
      CALL ANYPLT(ICOM)
      YPLT1=Y1MAX
      CALL ANYPLT(ICOM)
      XPLT1=X1MIN
      CALL ANYPLT(ICOM)
      YPLT1=Y1MIN
      CALL ANYPLT(ICOM)
      ICOM=4
      XPLT1=XMIN*PSKAL
      YPLT1=YMIN*PSKAL
      CALL ANYPLT(ICOM)
      ICOM=5
      XPLT1=XMAX*PSKAL
      CALL ANYPLT(ICOM)
      YPLT1=YMAX*PSKAL
      CALL ANYPLT(ICOM)
      XPLT1=XMIN*PSKAL
      CALL ANYPLT(ICOM)
      YPLT1=YMIN*PSKAL
      CALL ANYPLT(ICOM)
C
C  LABEL PLOT
C
      MARRAY=NCHAR
      XPLT2=0.12
      YPLT2=0.0
      XPLT1=X1MIN+0.5*X1RNG-0.5*XPLT2*REAL(NCHAR)
      YPLT1=Y1MAX-2.0*XPLT2
      CARRAY=TITLE
      ICOM=7
      IF(NCHAR.GT.0)CALL ANYPLT(ICOM)
C
C  IF (NB.GT.1) DRAW THE BOUNDARY
C
      IF(NB.GT.1)THEN
        DO I=1,NB
          IF(JOIN(I).EQ.1)THEN
            ICOM=4
            XPLT1=PSKAL*XB(I)
            YPLT1=PSKAL*YB(I)
            CALL ANYPLT(ICOM)
            IP1=I+1
            IF(I.EQ.NB)IP1=1
            ICOM=5
            XPLT1=PSKAL*XB(IP1)
            YPLT1=PSKAL*YB(IP1)
            CALL ANYPLT(ICOM)
            end if
        end do
      end if
C
C  MOVE TO A NODE AND DRAW THE ARROW
C
      DO I=1,NVAL
        UU=VSCALE*V1(I)
        VV=VSCALE*V2(I)
        XTIP=X(I)+UU
        YTIP=Y(I)+VV
        DIST=SQRT(UU*UU+VV*VV)
        ALPHA=0.0
        IF(DIST.NE.0.0)ALPHA=ATAN2(VV,UU)
        DEL=SQRT(5.0)*DIST/3.0
        X1=X(I)+DEL*COS(ALPHA-THETA)
        Y1=Y(I)+DEL*SIN(ALPHA-THETA)
        X2=X(I)+DIST*COS(ALPHA)*2.0/3.0
        Y2=Y(I)+DIST*SIN(ALPHA)*2.0/3.0
        X3=X(I)+DEL*COS(ALPHA+THETA)
        Y3=Y(I)+DEL*SIN(ALPHA+THETA)
        ICOM=4
        XPLT1=PSKAL*X(I)
        YPLT1=PSKAL*Y(I)
        CALL ANYPLT(ICOM)
        ICOM=5
        XPLT1=X2*PSKAL
        YPLT1=Y2*PSKAL
        CALL ANYPLT(ICOM)
        XPLT1=X1*PSKAL
        YPLT1=Y1*PSKAL
        CALL ANYPLT(ICOM)
        XPLT1=XTIP*PSKAL
        YPLT1=YTIP*PSKAL
        CALL ANYPLT(ICOM)
        XPLT1=X3*PSKAL
        YPLT1=Y3*PSKAL
        CALL ANYPLT(ICOM)
        XPLT1=X2*PSKAL
        YPLT1=Y2*PSKAL
        CALL ANYPLT(ICOM)
      end do

      ICOM=9
      CALL ANYPLT(ICOM)
      IF(CALCOM)WRITE(6,1020)PLOTNM

      RETURN
 1000 FORMAT(A1)
 1020 FORMAT(' VCPLOT CREATED PLOTFILE ',A30)
      END
      BLOCKDATA

c*********************************************************************72
c
cc BLOCKDATA initializes common block data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 March 2009
c
c  Author:
c
c    John Burkardt
c
      COMMON /OPTION/ XMRGL,XMRGR,XMXRNG,YMRGB,YMRGT,YMXRNG

      DATA XMRGL  /0.5/
      DATA XMRGR  /0.5/
      DATA XMXRNG /5.0/
      DATA YMRGT  /0.5/
      DATA YMRGB  /0.5/
      DATA YMXRNG /5.0/

      END
