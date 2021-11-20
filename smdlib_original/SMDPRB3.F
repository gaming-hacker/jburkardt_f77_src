C  SMDPRB3.F  09 November 1992
C
      program smdprb3
c
c***********************************************************************
c
      integer n
      parameter (n=101)
c
      real chrsiz
      integer i
      integer iaxes
      integer ierror
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
c
      write(*,*)' '
      write(*,*)'SMDPRB3'
      write(*,*)'Demonstrate display of simple curves with'
      write(*,*)'standard or semi-log axes.'
      write(*,*)' '
c
c  Set the data for two curves that share the same X values.
c
      xmin=1
      xmax=10
      do 100 i=1,n
        x=((n-i)*xmin+(i-1)*xmax)/real(n) 
        y1(i)=x**2  
        y2(i)=x**3  
100   continue  
c
c  Find the range of the two curves
c
      call minmax(y1,n,ymin1,ymax1)
      call minmax(y2,n,ymin2,ymax2)
c   
c  Choose the output device
c
      call psccgm('cgmb','smdprb3.cgm')
c
c  First subplot will appear in the box [0, .485] by [.525, 1]
c
      xlpct=0
      xrpct=48.5
      ybpct=52.5
      ytpct=100.0
      chrsiz=0
      call mapsiz(xlpct,xrpct,ybpct,ytpct,chrsiz)  
c
c  Now specify the coordinates to be used in the subplot, and
c  the labels for the axes, and the plot title, and the axis type.
c
      iaxes=0
      call mapit(xmin,xmax,ymin1,ymax1,'X axis','Y axis','Plot 1',iaxes) 
      call gscolr(2,ierror)   
      call curvey(xmin,xmax,y1,n,1,0.3,10)
c   
c  Second subplot
c
      call gscolr(1,ierror)   
      call mapsiz(51.5,100.0,52.5,100.0,0.0)
      iaxes=0
      call mapit(xmin,xmax,ymin2,ymax2,'X axis','Y axis','plot 2',iaxes) 
      call gscolr(3,ierror)   
      call curvey(xmin,xmax,y2,n,2,0.03,5) 
c
c  Third subplot
c   
      call gscolr(1,ierror)   
      call mapsiz(0.0,100.0,0.0,47.5,0.0)   
c     iaxes=2
c
c  ??? Having problems with mapit when last argument is 2 ???
c  That represents a log/log plot.
c
      iaxes=0
      call mapit(xmin,xmax,ymin2,ymax2,'X axis','Y axis','Plot 3',iaxes) 
      call gscolr(2,ierror)   
      call curvey(xmin,xmax,y1,n,3,0.03,10)
      call gscolr(3,ierror)   
      call curvey(xmin,xmax,y2,n,4,0.03,5) 
c
c  End plot
c
      call stoplt(0) 
c
c  End graphics
c
      call finplt
      stop  
      end   
