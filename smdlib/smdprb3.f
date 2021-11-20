!  smdprb3.f  03 July 1997
!
      program smdprb3
!
!***********************************************************************
!
      integer n
      parameter (n=101)
!
      real chrsiz
      integer i
      integer iaxes
      integer ierror
      integer lun
      character*1 orient
      real x
      real xlpct
      real xrpct
      real ybpct
      real ytpct
      real xmax
      real xmin
      real y1(n)
      real y2(n)
      real ymax1
      real ymax2
      real ymin1
      real ymin2
!
      write(*,*)' '
      write(*,*)'SMDPRB3'
      write(*,*)'  Demonstrate display of simple curves with'
      write(*,*)'  standard or semi-log axes.'
      write(*,*)' '
!
!  Set the data for two curves that share the same evenly
!  spaced X values.
!
      xmin=1.0
      xmax=10.0

      do i=1,n
        x=((n-i)*xmin+(i-1)*xmax)/real(n)
        y1(i)=x**2
        y2(i)=x**3
      end do
!
!  Find the Y range of the two curves
!
      call minmax(y1,n,ymin1,ymax1)
      call minmax(y2,n,ymin2,ymax2)
!
!  Define the graphics output device to be a PostScript file,
!  in portrait orientation, written to unit 7.
!
      lun=7
      orient = 'P'
 
      call postsc(lun,orient)
!
!  First subplot will appear in the box [0, .485] by [.525, 1]
!
      xlpct=0.0
      xrpct=48.5
      ybpct=52.5
      ytpct=100.0
      chrsiz=0.0

      call mapsiz(xlpct,xrpct,ybpct,ytpct,chrsiz)
!
!  Now specify the coordinates to be used in the subplot, and
!  the labels for the axes, and the plot title, and the axis type.
!
      iaxes=0
      call mapit(xmin,xmax,ymin1,ymax1,'X axis','Y axis','Plot 1',iaxes)

      call gscolr(2,ierror)

      call curvey(xmin,xmax,y1,n,1,0.3,10)
!
!  Second subplot
!
      call gscolr(1,ierror)
      call mapsiz(51.5,100.0,52.5,100.0,0.0)
      iaxes=0
      call mapit(xmin,xmax,ymin2,ymax2,'X axis','Y axis','plot 2',iaxes)
      call gscolr(3,ierror)
      call curvey(xmin,xmax,y2,n,2,0.03,5)
!
!  Third subplot
!
      call gscolr(1,ierror)
      call mapsiz(0.0,100.0,0.0,47.5,0.0)
!     iaxes=2
!
!  ??? Having problems with mapit when last argument is 2 ???
!  That represents a log/log plot.
!
      iaxes=0
      call mapit(xmin,xmax,ymin2,ymax2,'X axis','Y axis','Plot 3',iaxes)
      call gscolr(2,ierror)
      call curvey(xmin,xmax,y1,n,3,0.03,10)
      call gscolr(3,ierror)
      call curvey(xmin,xmax,y2,n,4,0.03,5)
!
!  End plot
!
      call stoplt
!
!  End graphics
!
      call finplt
 
      stop
      end
