!  smdprb7.f  12 May 1997
!
      program smdprb7
!
!***********************************************************************
!
!  Demonstrate the use of the SMDLIB routine CONTOR to create
!  a contour plot.
!
      integer nx
      parameter (nx=30)
!
      integer ny
      parameter (ny=nx)
!
      integer nz
      parameter (nz=nx)
!
      integer nl
      parameter (nl=30)
!
      real chrsiz
      real cl(nl)
      integer i
      integer iaxes
      integer ierror
      integer iz(nx,ny)
      integer j
      integer lun
      character*1 orient
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
!
      write(*,*)' '
      write(*,*)'SMDPRB7'
      write(*,*)'  Demonstrate use of CONTOR for contour plots.'
      write(*,*)' '
!
!  Set the minimum and maximum coordinates of the region.
!
      xmin=0.0
      xmax=1.0
      ymin=0.0
      ymax=1.0
!
!  Define the graphics output device to be a PostScript file,
!  in portrait orientation, written to unit 7.
!
      lun=7
      orient = 'P'
 
      call postsc(lun,orient)
!
      xlpct=0.0
      xrpct=100.0
      ybpct=0.0
      ytpct=100.0
      chrsiz=0.0
 
      call mapsiz(xlpct,xrpct,ybpct,ytpct,chrsiz)
!
!  Set the values of Z, which depends on X and Y.
!
      do i=1,nx
 
        x=((nx-i)*xmin+(i-1)*xmax)/real(nx-1)
 
        do j=1,ny
 
          y=((ny-j)*ymin+(j-1)*ymax)/real(ny-1)
 
          z(i,j)=sin(10.0*(x-0.5))*(y+0.5)*cos(x*y)
 
          if(i.eq.1.and.j.eq.1)then
            zmin=z(1,1)
            zmax=z(1,1)
          else
            zmin=min(zmin,z(i,j))
            zmax=max(zmax,z(i,j))
          endif
 
        end do
 
      end do
!
!  Set the contour levels.
!
      do i=1,nl
        cl(i)=((nl-i)*zmin+(i-1)*zmax)/real(nl-1)
      end do
!
!  Now specify the coordinates to be used in the subplot, and
!  the labels for the axes, and the plot title, and the axis type.
!
      iaxes=0
      call mapit(xmin,xmax,ymin,ymax,'X axis','Y axis','Plot 1',iaxes)
 
      call gscolr(2,ierror)
 
      call contor(z,nz,iz,nx,ny,xmin,xmax,ymin,ymax,nl,cl)
!
!  Terminate the plot.
!
      call stoplt
!
!  Terminate the graphics session.
!
      call finplt
 
      stop
      end
