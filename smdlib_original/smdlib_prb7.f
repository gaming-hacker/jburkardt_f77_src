C  SMDPRB7.F  11 May 1993
C
      program smdprb7
c
c***********************************************************************
c
c  Demonstrate the use of the SMDLIB routine CONTOR to create
c  a contour plot.
c
      integer nx
      parameter (nx=30)
c
      integer ny
      parameter (ny=nx)
c
      integer nz
      parameter (nz=nx)
c
      integer nl
      parameter (nl=30)
c
      real chrsiz
      real cl(nl)
      integer i
      integer iaxes
      integer ierror
      integer iz(nx,ny)
      integer j
      real x
      real xlpct
      real xmax
      real xmin
      real xrpct
      real y
      real ybpct
      real ymax
      real ymin
      real ytpct
      real z(nz,ny)
      real zmax
      real zmin
c
      write(*,*)' '
      write(*,*)'SMDPRB7'
      write(*,*)'Demonstrate use of CONTOR for contour plots.'
      write(*,*)' '
c
c  Set the minimum and maximum coordinates of the region.
c
      xmin=0
      xmax=1
      ymin=0
      ymax=1
c   
c  Choose the output device.
c
      call psccgm('ps','smdprb7.ps')
c
      xlpct=0.0
      xrpct=100.0
      ybpct=0.0
      ytpct=100.0
      chrsiz=0.0
      call mapsiz(xlpct,xrpct,ybpct,ytpct,chrsiz)  
c
c  Set the values of Z, which depends on X and Y.
c
      do 20 i=1,nx
        x=((nx-i)*xmin+(i-1)*xmax)/real(nx-1)
        do 10 j=1,ny
          y=((ny-j)*ymin+(j-1)*ymax)/real(ny-1)

          z(i,j)=sin(10*(x-0.5))*(y+0.5)*cos(x*y)

          if(i.eq.1.and.j.eq.1)then
            zmin=z(1,1)
            zmax=z(1,1)
          else
            zmin=min(zmin,z(i,j))
            zmax=max(zmax,z(i,j))
          endif

10      continue
20    continue
c
c  Set the contour levels.
c
      do 30 i=1,nl
        cl(i)=((nl-i)*zmin+(i-1)*zmax)/real(nl-1)
30    continue
c
c  Now specify the coordinates to be used in the subplot, and
c  the labels for the axes, and the plot title, and the axis type.
c
      iaxes=0
      call mapit(xmin,xmax,ymin,ymax,'X axis','Y axis','Plot 1',iaxes) 
      call gscolr(2,ierror)   
      call contor(z,nz,iz,nx,ny,xmin,xmax,ymin,ymax,nl,cl)
c
c  End plot.
c
      call stoplt(0) 
c
c  End graphics.
c
      call finplt
      stop  
      end   
