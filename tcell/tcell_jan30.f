      program main
!
!***********************************************************************
!  
!! MAIN runs the Navier-Stokes code for primitive variables in a T-cell region.
!
!
!  Discussion:
!
!    The fluid flow problem is formulated in terms of
!    primitive variables - u,v, and p.
!
!    u_t - laplacian u + (u.grad)u + grad p = f
!                                div u = 0
!
!    Boundary conditions:  (u,v)=(0,0) on top
!                          (u,v)=0 on left, right and bottom
!
!    This version uses finite element techniques
!    with piecewise linear functions on triangles to approximate
!    the pressure and quadratics on triangles for the velocity
!    (Taylor-Hood element), isoparametric element
!
!  Input files:
!
!    FUINI.DAT contains the initial values of the solution coefficients.
!
!  Modified:
!
!    30 January 2004
!
!  Author:
!
!    Hyung-Chun Lee,
!    Department of Mathematics,
!    Ajou University, Korea
!
! input= te.dat
! output=up.dat,ue.dat,curls
!
      implicit double precision(a-h,o-z)
!
      parameter(nx=41)
      parameter(ny=41)
      parameter(mx=2*nx-1)
      parameter(my=2*ny-1)
      parameter(maxel=  2*(nx-1)*(ny-1) )
      parameter(maxnd=  mx*my)
      parameter(maxun=  2*mx*my+nx*ny)
      parameter(minun=  27*ny)
!
      double precision a(minun,maxun)
      double precision area(maxel)
      integer bc_type
      double precision f(maxun)
      character*20 filename
      double precision g(maxun)
      double precision gg(maxun)
      double precision gp(maxun)
      integer indx(maxnd,2)
      integer insc(maxnd)
      integer ipivot(maxun)
      integer node(maxel,6)
      double precision uold(maxun)
      double precision xc(maxnd)
      double precision xm(maxel,3)
      double precision yc(maxnd)
      double precision ym(maxel,3)
!
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TCELL:'
      write ( *, '(a)' ) '  Solve the Navier Stokes fluid flow'
      write ( *, '(a)' ) '  equations in a TCELL region,'
      write ( *, '(a)' ) '  using finite elements.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Maximum number of nodes =    ', maxnd
      write ( *, '(a,i6)' ) '  Maximum number of elements = ', maxel
!
!  BC_TYPE selects the boundary conditions, by controlling the value of ALPHA.
!  Legal values are 1, for a step function, 2 for a "hat" function, 3 for a sinusoid.
!
      bc_type = 1
!
!  FILENAME is the name to be used for the first output file.
!  Each subsequent output file created by the program will have an
!  incremented name.
!
      filename = 'up000.txt'
!
!  Set functions for the input profile.
!
      iwrite = 0
!
!   input
!
      xlngth=1.0d0
      ylngth=1.0d0
      reynld=1.d0
      mrow1=minun
      nsim=3
      nsteps=10
      tolns=1.d-6
      tolopt=1.d-6
      pi=4.d0*datan(1.d0)
!
!  2.  setgrd constructs grid, numbers unknowns, calculates areas,
!      and points for midpoint quadrature rule
!      SETGRD sets bandwidth and neqn1
!
      call setgrd(xc,yc,area,xm,ym,xlngth,ylngth,
     &    node,indx,insc,nlband,nband,
     &    nx,ny,nelemn,np,nnodes,nuk,nquad,neqn1,
     &    iwrite,maxnd,maxel)

      write ( *, '(a,i6)' ) '  Number of nodes =    ', np
      write ( *, '(a,i6)' ) '  Number of elements = ', nelemn

      call grid_write ( maxel, maxnd, xc, yc, node, nelemn, np )

!      write(6,1050) neqn1,nelemn
      nuband=nlband
      nrow1=nlband+nlband+nuband+1
      ncol1=neqn1
      ny2=ny+ny-1
!      write(6,1100) nlband,nuband,nband,nrow1,ncol1,np
!
!      write(6,1040)
!      write(6,1040)
!      write(6,1300) nx
!      write(6,1310) ny
!      write(6,1410) reynld
!      write(6,1040)
!      write(6,1040)
!
      deltat=0.0002d0
      rdel=1.d0/deltat

      do i=1,neqn1
        f(i)=0.d0
      end do
!
!  Read the initial data.
!
      open ( unit = 10, file = 'fuini.dat' )

      do i = 1, neqn1
        read ( 10, * ) uold(i)
      end do

      close ( unit = 10 )

      do iter = 1, 500

        if ( bc_type == 1 ) then

          if ( iter .le. 250 ) then
            alpha = 5.d0
          else 
            alpha = 1.d0
          endif

        else if ( bc_type == 2 ) then

          if ( iter .le. 250 ) then
            alpha = 80.d0*dble(iter)*deltat+1.d0
          else 
            alpha = -80.d0*dble(iter)*deltat+9.d0
          endif

        else if ( bc_type == 3 ) then

          alpha = 2.d0*sin(dble(iter)*0.01*pi)   

        end if

        do i=1,neqn1
          g(i)=f(i)
        end do

        do i=1,neqn1
          f(i)=0.d0
        end do

        call nstoke(xc,yc,area,xm,ym,
     &     a,f,g,uold,reynld,tolns,xlngth,ylngth,
     &     node,indx,insc,ipivot,mrow1,
     &     nlband,nuband,nband,nrow1,ncol1,
     &     nelemn,np,nnodes,nuk,nquad,neqn1,
     &     nsteps,nsim,iwrite,maxnd,maxel,rdel,alpha)
!
! Save u=(gx,gy) to 'ue.dat' for 't.f'
!
        do i=1,neqn1
          uold(i)=f(i)
        end do
!
!  Increment the filename, and 
!  save u=(gx,gy) to 'up???.dat'
!
        call file_name_inc ( filename )

        write ( *, * ) 'Creating ', filename

        open ( unit = 1, file = filename )

        do ic = 1, np

          iuku=indx(ic,1)
          iukv=indx(ic,2)
          iukp=insc(ic)

          if(iuku.eq.0) then
            gx=0.d0
            if(iukp.eq.0) then
              pp=0.d0
            else
              pp=f(iukp)
            end if
            go to 54
          else if(iuku.eq.-1) then
            gx=alpha*ubdry(1,ic,xc,yc,ylngth)
            if(iukp.eq.0) then
              pp=0.d0
            else
              pp=f(iukp)
            end if
            go to 54
          else
            gx=f(iuku)
          end if

          if(iukp.eq.0) then
            pp=0.d0
          else
            pp=f(iukp)
          end if

54        continue

          if(iukv.le.0) then
            gy=0.d0
          else
            gy=f(iukv)
          end if

          if(iukp.eq.0) then
            pp=0.d0
          else
            pp=f(iukp)
          end if

          g(ic)=gx
          gg(ic)=gy
          gp(ic)=pp
          write(1,3031) gx,gy

        end do

        close (unit=1)

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TCELL:'
      write ( *, '(a)' ) '  Normal end of execution.'

3031  format(2e25.15)
3033  format(6d10.3)
1040  format(1x)
1050  format(' number of unknowns=',i4,'   number of triangles= ',i4)
1100  format(' nlband=',i4,' nuband=', i4,' nband=',i4/
     &   ' nrow1=',i4,' ncol1=',i4,' # of pts=',i4)
1300  format(' nx=  ',i4)
1310  format(' ny = ',i4)
1410  format(' reynld=',f14.7)
      stop
      end
      subroutine setgrd(xc,yc,area,xm,ym,xlngth,ylngth,
     &    node,indx,insc,nlband,nband,
     &    nx,ny,nelemn,np,nnodes,nuk,nquad,neqn1,
     &    iwrite,maxnd,maxel)
!
!***********************************************************************
!
!! SETGRD sets up the grid for the problem.
!
!
!  Discussion:
!
!    We are using quadratics for the velocity and linears for the 
!    pressure
!
!    Input needed is nx,ny,xlngth,ylngth,write
!    Computes arrays node,area,xc,yc,xm,ym,index,insc
!    Computes neqn1, bandwidth information
!
!  Modified:
!
!    18 June 2002
!
!  Author:
!
!    Hyung-Chun Lee,
!    Department of Mathematics,
!    Ajou University, Korea
!
      implicit double precision(a-h,o-z)

      double precision area(*)
      double precision xc(*)
      double precision yc(*)
      double precision xm(maxel,*),ym(maxel,*)
      integer node(maxel,*),indx(maxnd,*),insc(*)
!
!  set parameters for Taylor-Hood element
!
      nnodes=6
      nuk=3
!
!  construct grid
!  coordinates and ordering of unknowns
!
      nym1=ny-1
      nxm1=nx-1
      nrow=nx+nxm1
      ncol=ny+nym1
      hx=xlngth/dble(nxm1)
      hx1=hx/2.d0
      hy=ylngth/dble(nym1)
      hy1=hy/2.d0
!
      i=0
      ip=0
      it1=-1
      xx=0.d0-hx1
!
      iquater1=(nrow-1)/4-2
      iquater2=3*(nrow-1)/4
      do 40 ic=1,iquater1
        xx=xx+hx1
        icnt=mod(ic,2)
        do 35 jc=1,ny
          jcnt=mod(jc,2)
          yy=.5d0+hy1*dble(jc-1)
          ip=ip+1
          xc(ip)=xx
          yc(ip)=yy
          if(icnt.eq.1.and.jcnt.eq.1) go to 5
          go to 10
   5      if(jc.eq.ny) go to 10
          it1=it1+2
          nelemn=it1+1
          ip1=ip+ny
          ip2=ip+ny+ny
          node(it1,1)=ip
          node(it1,2)=ip2+2
          node(it1,3)=ip2
          node(it1,4)=ip1+1
          node(it1,5)=ip2+1
          node(it1,6)=ip1
          node(nelemn,1)=ip
          node(nelemn,2)=ip2+2
          node(nelemn,3)=ip+2
          node(nelemn,4)=ip1+1
          node(nelemn,5)=ip1+2
          node(nelemn,6)=ip+1
   10     continue
!
! Index for Dirichlet B.C.
!
        if(jc.eq.1.or.jc.eq.ny) go to 20
        if(ic.eq.1) go to 20
          i=i+2
          indx(ip,1)=i-1
          indx(ip,2)=i
          go to 29
   20     continue
          indx(ip,1)=-1
          indx(ip,2)=-1
   29     continue
          if(jcnt.eq.0.or.icnt.eq.0) go to 30
          i=i+1
          insc(ip)=i
          go to 35
   30     insc(ip)=0
   35   continue
   40 continue
!
      do 140 ic=iquater1+1,iquater1+2
        xx=xx+hx1
        icnt=mod(ic,2)
        do 135 jc=1,ny
          jcnt=mod(jc,2)
          yy=.5d0+hy1*dble(jc-1)
          ip=ip+1
          xc(ip)=xx
          yc(ip)=yy
          if(icnt.eq.1.and.jcnt.eq.1) go to 15
          go to 110
   15     if(jc.eq.ny) go to 110
          it1=it1+2
          nelemn=it1+1
          ip1=ip+ny
          ip2=ip+ny+ncol
          node(it1,1)=ip
          node(it1,2)=ip2+2
          node(it1,3)=ip2
          node(it1,4)=ip1+1
          node(it1,5)=ip2+1
          node(it1,6)=ip1
          node(nelemn,1)=ip
          node(nelemn,2)=ip2+2
          node(nelemn,3)=ip+2
          node(nelemn,4)=ip1+1
          node(nelemn,5)=ip1+2
          node(nelemn,6)=ip+1
  110     continue
!
! Index for Dirichlet B.C.
!
        if(jc.eq.1.or.jc.eq.ny) go to 120
        i=i+2
          indx(ip,1)=i-1
          indx(ip,2)=i
          go to 129
  120     continue
          indx(ip,1)=-1
          indx(ip,2)=-1
  129     continue
          if(jcnt.eq.0.or.icnt.eq.0) go to 130
          i=i+1
          insc(ip)=i
          go to 135
  130     insc(ip)=0
  135   continue
  140 continue
!
      do 240 ic=iquater1+3,iquater2
        xx=xx+hx1
        icnt=mod(ic,2)
        do 235 jc=1,ncol
          jcnt=mod(jc,2)
          yy=hy1*dble(jc-1)
          ip=ip+1
          xc(ip)=xx
          yc(ip)=yy
          if(icnt.eq.1.and.jcnt.eq.1) go to 25
          go to 210
   25     if(jc.eq.ncol) go to 210
          it1=it1+2
          nelemn=it1+1
          ip1=ip+ncol
          ip2=ip+ncol+ncol
          node(it1,1)=ip
          node(it1,2)=ip2+2
          node(it1,3)=ip2
          node(it1,4)=ip1+1
          node(it1,5)=ip2+1
          node(it1,6)=ip1
          node(nelemn,1)=ip
          node(nelemn,2)=ip2+2
          node(nelemn,3)=ip+2
          node(nelemn,4)=ip1+1
          node(nelemn,5)=ip1+2
          node(nelemn,6)=ip+1
  210     continue
!
! Index for Dirichlet B.C.
!
        if(jc.eq.1.or.jc.eq.ncol) go to 220
        if(ic.eq.iquater1+3.and.jc.le.ny) go to 220
        i=i+2
          indx(ip,1)=i-1
          indx(ip,2)=i
          go to 229
  220     continue
          indx(ip,1)=-1
          indx(ip,2)=-1
  229     continue
          if(jcnt.eq.0.or.icnt.eq.0) go to 230
          i=i+1
          insc(ip)=i
          go to 235
  230     insc(ip)=0
  235   continue
  240 continue
!
        xx=xx+hx1
        icnt=mod(iquater2+1,2)
        do 255 jc=1,ny-1
          jcnt=mod(jc,2)
          yy=hy1*dble(jc-1)
          ip=ip+1
          xc(ip)=xx
          yc(ip)=yy
          indx(ip,1)=-1
          indx(ip,2)=-1
          if(jcnt.eq.0.or.icnt.eq.0) go to 930
          i=i+1
          insc(ip)=i
          go to 255
  930     insc(ip)=0
  255   continue
!
       xx=xx-hx1
      do 340 ic=iquater2+1,nrow
        xx=xx+hx1
        icnt=mod(ic,2)
        do 335 jc=1,ny
          jcnt=mod(jc,2)
          yy=.5d0+hy1*dble(jc-1)
          ip=ip+1
          xc(ip)=xx
          yc(ip)=yy
          if(icnt.eq.1.and.jcnt.eq.1) go to 435
          go to 310
  435     if(ic.eq.nrow.or.jc.eq.ny) go to 310
          it1=it1+2
          nelemn=it1+1
          ip1=ip+ny
          ip2=ip+ny+ny
          node(it1,1)=ip
          node(it1,2)=ip2+2
          node(it1,3)=ip2
          node(it1,4)=ip1+1
          node(it1,5)=ip2+1
          node(it1,6)=ip1
          node(nelemn,1)=ip
          node(nelemn,2)=ip2+2
          node(nelemn,3)=ip+2
          node(nelemn,4)=ip1+1
          node(nelemn,5)=ip1+2
          node(nelemn,6)=ip+1
  310     continue
!
! Index for Dirichlet B.C.
!
        if(jc.eq.1.or.jc.eq.ny) go to 320
!        if(ic.eq.nrow) go to 320
        i=i+2
          indx(ip,1)=i-1
          indx(ip,2)=i
          go to 329
  320     continue
          indx(ip,1)=-1
          indx(ip,2)=-1
  329     continue
          if(jcnt.eq.0.or.icnt.eq.0) go to 330
          i=i+1
          insc(ip)=i
          go to 335
  330     insc(ip)=0
  335   continue
  340 continue
!
      np=ip
      neqn1=i
!      write(6,*) 'Do you want the information of grid ? (Yes:1, No:2)'
!      read(5,*) iw
       iw=2
       if (iw.eq.1)
     &    write(iwrite,1089) (i,xc(i),yc(i),indx(i,1),
     &                  indx(i,2),insc(i),i=1,np)
       if(iw.eq.1)
     .    write(iwrite,1099) (it,(node(it,i),i=1,6),it=1,nelemn)

!  set quadrature information
!  quadrature rule is midpoint
!  coordinates and weights are set by routine QD7PT
!
      nquad=3

      do it=1,nelemn
        xm(it,1)=0.5d0
        xm(it,2)=0.5d0
        xm(it,3)=0.0d0
        ym(it,1)=0.0d0
        ym(it,2)=0.5d0
        ym(it,3)=0.5d0
        area(it)=0.5d0
      end do

!       if (iwrite.gt.1)
!     &          write(iwrite,2325) ((xm(n,k),ym(n,k),k=1,3),n=1,nelemn)
!2325  format(' qdpt',2d12.5)
!
!  half band width
!
      nlband=0
      do 90 it=1,nelemn
        do 90 iq=1,nnodes
          ip=node(it,iq)
          do 80 iuk=1,nuk
            if(iuk.eq.3) then
             i=insc(ip)
          else
             i=indx(ip,iuk)
            end if
            if(i.le.0) go to 80
            do 70 iqq=1,nnodes
              ipp=node(it,iqq)
              do 70 iukk=1,nuk
                if(iukk.eq.nuk) then
                    j=insc(ipp)
                  else
                    j=indx(ipp,iukk)
                end if
                if(i.gt.j) go to 70
                ij=j-i
                if(ij.gt.nlband) nlband=ij
   70           continue
   80       continue
   90     continue
      nband=nlband+nlband+1
!
 1099 format(7i6)
 1089 format(i5,2f12.5,3i5)
 2001 format(' unknown numbers along line:', 7i5)
!
      return
      end
      subroutine grid_write ( me, mn, xn, yn, node, ne, nn )
!
!***********************************************************************
!
!! GRID_WRITE writes the node and element information to a file.
!
!
!  Discussion:
!
!    The routine writes a file named "ELENODE.DAT".
!
!    The first line of this file contains NN and NE, the number of nodes
!    and the number of elements.
!
!    Each of the next NN lines lists a node index, and an X and Y coordinate.
!
!    Each of the next NE lines lists an element index, and the six nodes
!    that make up that element, in a particular order.
!
!  Modified:
!
!    30 January 2004
!
!  Author: 
!
!    Hyung-Chun Lee Ph.D
!    Department of Mathematics
!    Ajou University, Korea
!
!  Parameters:
!
!    Input, integer ME, the maximum number of elements.
!
!    Input, integer MN, the maximum number of nodes.
!
!    Input, double precision XN(NN), YN(NN), the coordinates of the nodes.
!
!    Input, integer NODE(ME,6), the indices of the nodes that comprise each element.
!
!    Input, integer NE, the number of elements.
!
!    Input, integer NN, the number of nodes.
!
      implicit none

      integer me
      integer mn
!
      integer i
      integer j
      integer ne
      integer nn
      integer node(me,6)
      double precision xn(mn)
      double precision yn(mn)
!
      open ( unit = 10, file = 'elenode.dat', status = 'unknown' )

      write ( 10, * ) nn, ne

      do i = 1, nn
        write ( 10, * ) i, xn(i), yn(i)
      end do

      do i = 1, ne
        write ( 10, '(7i6)' ) i, ( node(i,j), j = 1, 6 )
      end do

      close ( unit = 10 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GRID_WRITE - Wrote the file "elenode.dat".'

      return
      end
      subroutine nstoke(xc,yc,area,xm,ym,
     &      a,f,g,uold,reynld,tolns,xlngth,ylngth,
     &      node,indx,insc,ipivot,mrow1,
     &      nlband,nuband,nband,nrow1,ncol1,
     &      nelemn,np,nnodes,nuk,nquad,neqn1,
     &      nsteps,nsim,iwrite,maxnd,maxel,rdel,alpha)
!
!**********************************************************************
!
!! NSTOKE solves the Navier-Stokes equation using taylor-hood element
!
!
!  Modified:
!
!    18 June 2002
!
!  Author:
!
!    Hyung-Chun Lee,
!    Department of Mathematics,
!    Ajou University, Korea
!
      implicit double precision(a-h,o-z)
!
      double precision xc(*),yc(*)
      double precision area(*),xm(maxel,*),ym(maxel,*)
!
      double precision a(mrow1,*)
      double precision f(*)
      double precision g(*)
      integer indx(maxnd,*)
      integer insc(*)
      integer ipivot(*)
      integer node(maxel,*)
      double precision un(2)
      double precision unx(2)
      double precision uny(2)
      double precision uold(*)
!
!  zero arrays
!  g array contains the previous iterate
!  f array contains the right hand side initially and then the current
!  iterate is overwritten on f
!
      visc=1.d0/reynld
!
!  matrix assembly triangle by triangle
!  nsim is the number of simple iterations performed
!
      csim=0.d0
      do 700  iter=1, nsteps
!        open(12,file='uenew.dat',status='unknown')
        niter=iter
        if(iter.gt.nsim) csim=1.d0
!
        do 120 i=1,nrow1
          do 120 j=1,ncol1
120           a(i,j)=0.d0
!
!        write(6,1066) iter
        do 240 it=1,nelemn
          arr=area(it)/3.d0
          do 230 iquad=1,nquad
            y=ym(it,iquad)
            x=xm(it,iquad)
            call trans(it,x,y,det,xix,xiy,etax,etay,xc,yc,node,maxel)
            ar=arr*det

            do 130 kk=1,2
              un(kk)=0.d0
              uny(kk)=0.d0
              unx(kk)=0.d0
 130        continue
            do 140 iq=1,nnodes
              call refqbf(x,y,iq,bb,tbx,tby)
              bx=tbx*xix+tby*etax
              by=tbx*xiy+tby*etay
              ip=node(it,iq)
              do 150 iuk=1,2
                iun=indx(ip,iuk)
                if(iun.eq.0) then
                  go to 140
                else if(iun.gt.0) then
                  un(iuk)=un(iuk)+bb*g(iun)
                  unx(iuk)=unx(iuk)+bx*g(iun)
                  uny(iuk)=uny(iuk)+by*g(iun)
                else
                  ubc=alpha*ubdry(iuk,ip,xc,yc,ylngth)
                  un(iuk)=un(iuk)+bb*ubc
                  unx(iuk)=unx(iuk)+bx*ubc
                  uny(iuk)=uny(iuk)+by*ubc
                end if
 150              continue
 140          continue
!           
            do 220 iq=1,nnodes
              ip=node(it,iq)
              call refqbf(x,y,iq,bb,tbx,tby)
              bx=tbx*xix+tby*etax
              by=tbx*xiy+tby*etay
              bbl=refbsp(x,y,iq)
              do 210 iuk=1,nuk
                i=insc(ip)
                if(iuk.ne.3) i=indx(ip,iuk)
                if(i.le.0) go to 210
                if(iuk.eq.1) f(i)=f(i)
     &                   +csim*( (un(1)*unx(1)+un(2)*uny(1))*bb )*ar
     &                     +rdel*uold(i)*bb*ar
                if(iuk.eq.2) f(i)=f(i)
     &                   +csim*( (un(1)*unx(2)+un(2)*uny(2))*bb )*ar
     &                     +rdel*uold(i)*bb*ar
                do 200 iqq=1,nnodes
                  ipp=node(it,iqq)
                    call refqbf(x,y,iqq,bbb,tbbx,tbby)
                    bbx=tbbx*xix+tbby*etax
                    bby=tbbx*xiy+tbby*etay
                    bbbl=refbsp(x,y,iqq)
                  do 190 iukk=1,nuk
                    j=insc(ipp)
                    if(iukk.ne.3) j=indx(ipp,iukk)
                    if(j.eq.0) go to 190
                      aij=0.d0
                    if(i.eq.neqn1) go to 190
                    if(iuk.eq.1) then
                      if(iukk.eq.1) aij=visc*(by*bby+bx*bbx)
     &                   +(bbb*unx(1)*bb)*csim
     &                   +bb*bbx*un(1)
     &                   +bb*bby*un(2) + rdel*(bb*bbb)
                      if(iukk.eq.2) aij=csim*(bb*bbb*uny(1))
                      if(iukk.eq.3) aij=-bx*bbbl
                        else if(iuk.eq.2) then
                      if(iukk.eq.2) aij=(visc*(by*bby+bx*bbx)
     &                       +(bb*bbb*uny(2))*csim
     &                   +bb*bby*un(2)
     &                     +bb*bbx*un(1)) + rdel*(bb*bbb)
                      if(iukk.eq.1) aij= csim*(bb*bbb*unx(2))
                      if(iukk.eq.3) aij=-by*bbbl
                      else
                      if(iukk.eq.1) aij=bbx*bbl
                      if(iukk.eq.2) aij=bby*bbl
                    end if
 180              continue
                  if(j.lt.0) go to 185
                    iuse=i-j+nband
                    a(iuse,j)=a(iuse,j)+aij*ar
                    go to 190
!
!  add terms to rhs for inhomogeneous boundary condition
!
 185                      continue
                    f(i)=f(i)-ar*alpha*ubdry(iukk,ipp,xc,yc,ylngth)*aij
 190              continue
 200            continue
 210          continue
 220        continue
 230      continue
 240    continue
        f(neqn1)=0.d0
        do j=neqn1-nlband,neqn1-1
           i=neqn1-j+nband
           a(i,j)=0.d0
        end do
        a(nband,neqn1)=1.d0
!
!  solve system
!
        job=0
        call dgbfa(a,mrow1,neqn1,nlband,nuband,ipivot,info)
        call dgbsl(a,mrow1,neqn1,nlband,nuband,ipivot,f,job)
        if(info.eq.0) go to 509
!       write(6, 1020) info
!
 509    continue
!
!  check for convergence
!
        diff=0.d0
        do 620 i=1,neqn1
          diff=diff+(g(i)-f(i))**2
 620    continue
        diff=sqrt(diff)
        write(6,1045) iter,diff
!        write(iwrite,1045) iter,diff
        if(diff.le.tolns) go to 750
        do 630 i=1,neqn1
          g(i)=f(i)
          f(i)=0.d0
 630    continue
!      close(11)
!      close(12)
700   continue

! 750  write(6,1050) niter
750    continue
!
!
1020  format('info =',i5)
1045  format('  for iteration no.',i3,' difference in iterates is '
     &      ,2d14.8)
1050  format(/,' solutions converged in',i5,' iterations ',//)
1060  format(/,' solution calculated for reynolds number ',f8.2)
1066  format(//'  iteration number',i2)
!
      return
      end
      function ubdry ( iuk, ip, xc, yc, ylngth )
!
!***********************************************************************
!
!! UBDRY evaluates the boundary conditions.
!
!  Modified:
!
!    18 June 2002
!
!  Author:
!
!    Hyung-Chun Lee,
!    Department of Mathematics,
!    Ajou University, Korea
!
      integer iuk
      integer ip
      double precision xc(*),yc(*)
      double precision ylngth
      double precision ubdry
!
!   internal variable
!
      double precision y
!
      x=xc(ip)
      y=yc(ip)
      if(x.eq.0) then
        if(iuk.eq.1) ubdry=16.d0*(y-.5d0)*(1.d0-y)
        if(iuk.eq.2) ubdry=0.d0
      else
        ubdry=0.d0
      end if
      return
      end
      function refbsp ( x, y, iq )
!
!***********************************************************************
!
!! REFBSP evaluates a linear basis functions on the reference triangle.
!
!  Modified:
!
!    18 June 2002
!
!  Author:
!
!    Hyung-Chun Lee,
!    Department of Mathematics,
!    Ajou University, Korea
!
      implicit double precision(a-h,o-z)

      integer iq
      double precision x
      double precision y
!
      if(iq.eq.1) refbsp=1.d0-x-y
      if(iq.eq.2) refbsp=x
      if(iq.eq.3) refbsp=y

      return
      end
      subroutine refqbf ( x, y, in, bb, bx, by )
!
!************************************************************************
!
!! REFQBF evaluates quadratic basis functions on reference triangle
!
!
!  Modified:
!
!    18 June 2002
!
!  Author:
!
!    Hyung-Chun Lee,
!    Department of Mathematics,
!    Ajou University, Korea
!
      double precision x
      double precision y
      double precision bb
      double precision bx
      double precision by
      integer in
!
      if(in.eq.1) then
      bb=(1.d0-x-y)*(1.d0-2.d0*x-2.d0*y)
      bx=-3.d0+4.d0*x+4.d0*y
      by=-3.d0+4.d0*x+4.d0*y
      else if(in.eq.2) then
      bb=x*(2.d0*x-1.d0)
      bx=4.d0*x-1.d0
      by=0.d0
      else if(in.eq.3) then
      bb=y*(2.d0*y-1.d0)
      bx=0.d0
      by=4.d0*y-1.d0
      else if(in.eq.4) then
      bb=4.d0*x*(1.d0-x-y)
      bx=4.d0*(1.d0-2.d0*x-y)
      by=-4.d0*x
      else if(in.eq.5) then
      bb=4.d0*x*y
      bx=4.d0*y
      by=4.d0*x
      else if(in.eq.6) then
      bb=4.d0*y*(1.d0-x-y)
      bx=-4.d0*y
      by=4.d0*(1.d0-x-2.d0*y)
      end if
      return
      end
      subroutine trans ( it, xq, yq, det, pj11, pj21, pj12, pj22,
     &  xc, yc, node, maxel )
!
! ***********************************************************************
!
!! TRANS transforms data between the reference and physical elements.
!
!
!  Modified:
!
!    18 June 2002
!
!  Author:
!
!    Hyung-Chun Lee,
!    Department of Mathematics,
!    Ajou University, Korea
!
      implicit double precision(a-h,o-z)
!
      double precision xc(*)
      double precision yc(*)
      integer node(maxel,*)
!
      i1=node(it,1)
      i2=node(it,2)
      i3=node(it,3)
      i4=node(it,4)
      i5=node(it,5)
      i6=node(it,6)

      x1=xc(i1)
      y1=yc(i1)
      x2=xc(i2)
      y2=yc(i2)
      x3=xc(i3)
      y3=yc(i3)
      x4=xc(i4)
      y4=yc(i4)
      x5=xc(i5)
      y5=yc(i5)
      x6=xc(i6)
      y6=yc(i6)
!
!  Compute partial derivatives at point (xq,yq)
!
      f1x=x1*(-3.d0+4.d0*xq+4.d0*yq)
     &       +x2*(4.d0*xq-1.d0)
     &       +x4*4.d0*(1.d0-2.d0*xq-yq)
     &       +x5*4.d0*yq + x6*4.d0*(-yq)
      f1y=x1*(-3.d0+4.d0*xq+4.d0*yq)
     &       +x3*(4.d0*yq-1.d0)
     &       +x4*4.d0*(-xq) + x5*4.d0*xq
     &       +x6*4.d0*(1.d0-xq-2.d0*yq)
      f2x=y1*(-3.d0+4.d0*xq+4.d0*yq)
     &       +y2*(4.d0*xq-1.d0)
     &       +y4*4.d0*(1.d0-2.d0*xq-yq)
     &       +y5*4.d0*yq + y6*4.d0*(-yq)
      f2y=y1*(-3.d0+4.d0*xq+4.d0*yq)
     &       +y3*(4.d0*yq-1.d0)
     &       +y4*4.d0*(-xq) + y5*4.d0*xq
     &       +y6*4.d0*(1.d0-xq-2.d0*yq)
!
!  Compute determinant of transformation evaluated at point (xq,yq)
!
      det=f1x*f2y-f1y*f2x
!
!  Compute j11, j22, j21, j22
!
      pj11=f2y/det
      pj12=-f2x/det
      pj21=-f1y/det
      pj22=f1x/det
      det=dabs(det)

      return
      end
      subroutine dgbfa(abd,lda,n,ml,mu,ipvt,info)
!
! ********************************************************
!
!! DGBFA factors a double precision band matrix by elimination.
!
!     dgbfa is usually called by dgbco, but it can be called
!     directly with a saving in time if  rcond  is not needed.
!
!     on entry
!
!        abd     double precision(lda, n)
!                contains the matrix in band storage.  the columns
!                of the matrix are stored in the columns of  abd  and
!                the diagonals of the matrix are stored in rows
!                ml+1 through 2*ml+mu+1 of  abd .
!                see the comments below for details.
!
!        lda     integer
!                the leading dimension of the array  abd .
!                lda must be .ge. 2*ml + mu + 1 .
!
!        n       integer
!                the order of the original matrix.
!
!        ml      integer
!                number of diagonals below the main diagonal.
!                0 .le. ml .lt. n .
!
!        mu      integer
!                number of diagonals above the main diagonal.
!                0 .le. mu .lt. n .
!                more efficient if  ml .le. mu .
!     on return
!
!        abd     an upper triangular matrix in band storage and
!                the multipliers which were used to obtain it.
!                the factorization can be written  a = l*u  where
!                l  is a product of permutation and unit lower
!                triangular matrices and  u  is upper triangular.
!
!        ipvt    integer(n)
!                an integer vector of pivot indices.
!
!        info    integer
!                = 0  normal value.
!                = k  if  u(k,k) .eq. 0.0 .  this is not an error
!                     condition for this subroutine, but it does
!                     indicate that dgbsl will divide by zero if
!                     called.  use  rcond  in dgbco for a reliable
!                     indication of singularity.
!
!     band storage
!
!           if  a  is a band matrix, the following program segment
!           will set up the input.
!
!                   ml = (band width below the diagonal)
!                   mu = (band width above the diagonal)
!                   m = ml + mu + 1
!                   do 20 j = 1, n
!                      i1 = max0(1, j-mu)
!                      i2 = min0(n, j+ml)
!                      do 10 i = i1, i2
!                         k = i - j + m
!                         abd(k,j) = a(i,j)
!                10    continue
!                20 continue
!
!           this uses rows  ml+1  through  2*ml+mu+1  of  abd .
!           in addition, the first  ml  rows in  abd  are used for
!           elements generated during the triangularization.
!           the total number of rows needed in  abd  is  2*ml+mu+1 .
!           the  ml+mu by ml+mu  upper left triangle and the
!           ml by ml  lower right triangle are not referenced.
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!     subroutines and functions
!
!     blas daxpy,dscal,idamax
!     fortran max0,min0
!
      integer lda,n,ml,mu,ipvt(1),info
      double precision abd(lda,1)
!
      double precision t
      integer i,idamax,i0,j,ju,jz,j0,j1,k,kp1,l,lm,m,mm,nm1
!
!
      m = ml + mu + 1
      info = 0
!
!     zero initial fill-in columns
!
      j0 = mu + 2
      j1 = min0(n,m) - 1
      if (j1 .lt. j0) go to 30
      do 20 jz = j0, j1
         i0 = m + 1 - jz
         do 10 i = i0, ml
            abd(i,jz) = 0.0d0
   10    continue
   20 continue
   30 continue
      jz = j1
      ju = 0
!
!     gaussian elimination with partial pivoting
!
      nm1 = n - 1
      if (nm1 .lt. 1) go to 130
      do 120 k = 1, nm1
         kp1 = k + 1
!
!        zero next fill-in column
!
         jz = jz + 1
         if (jz .gt. n) go to 50
         if (ml .lt. 1) go to 50
            do 40 i = 1, ml
               abd(i,jz) = 0.0d0
   40       continue
   50    continue
!
!        find l = pivot index
!
         lm = min0(ml,n-k)
         l = idamax(lm+1,abd(m,k),1) + m - 1
         ipvt(k) = l + k - m
!
!        zero pivot implies this column already triangularized
!
         if (abd(l,k) .eq. 0.0d0) go to 100
!
!           interchange if necessary
!
            if (l .eq. m) go to 60
               t = abd(l,k)
               abd(l,k) = abd(m,k)
               abd(m,k) = t
   60       continue
!
!           compute multipliers
!
            t = -1.0d0/abd(m,k)
            call dscal(lm,t,abd(m+1,k),1)
!
!           row elimination with column indexing
!
            ju = min0(max0(ju,mu+ipvt(k)),n)
            mm = m
            if (ju .lt. kp1) go to 90
            do 80 j = kp1, ju
               l = l - 1
               mm = mm - 1
               t = abd(l,j)
               if (l .eq. mm) go to 70
                  abd(l,j) = abd(mm,j)
                  abd(mm,j) = t
   70          continue
               call daxpy(lm,t,abd(m+1,k),1,abd(mm+1,j),1)
   80       continue
   90       continue
         go to 110
  100    continue
            info = k
  110    continue
  120 continue
  130 continue
      ipvt(n) = n
      if (abd(m,n) .eq. 0.0d0) info = n
      return
      end
      subroutine dgbsl(abd,lda,n,ml,mu,ipvt,b,job)
!
!***********************************************************************
!
!! DGBSL solves the double precision band system
!     a * x = b  or  trans(a) * x = b
!     using the factors computed by dgbco or dgbfa.
!
!     on entry
!
!        abd     double precision(lda, n)
!                the output from dgbco or dgbfa.
!
!        lda     integer
!                the leading dimension of the array  abd .
!
!        n       integer
!                the order of the original matrix.
!
!        ml      integer
!                number of diagonals below the main diagonal.
!
!        mu      integer
!                number of diagonals above the main diagonal.
!
!        ipvt    integer(n)
!                the pivot vector from dgbco or dgbfa.
!
!        b       double precision(n)
!                the right hand side vector.
!
!        job     integer
!                = 0         to solve  a*x = b ,
!                = nonzero   to solve  trans(a)*x = b , where
!                            trans(a)  is the transpose.
!
!     on return
!
!        b       the solution vector  x .
!
!     error condition
!
!        a division by zero will occur if the input factor contains a
!        zero on the diagonal.  technically this indicates singularity
!        but it is often caused by improper arguments or improper
!        setting of lda .  it will not occur if the subroutines are
!        called correctly and if dgbco has set rcond .gt. 0.0
!        or dgbfa has set info .eq. 0 .
!
!     to compute  inverse(a) * c  where  c  is a matrix
!     with  p  columns
!           call dgbco(abd,lda,n,ml,mu,ipvt,rcond,z)
!           if (rcond is too small) go to ...
!           do 10 j = 1, p
!              call dgbsl(abd,lda,n,ml,mu,ipvt,c(1,j),0)
!        10 continue
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!     subroutines and functions
!
!     blas daxpy,ddot
!     fortran min0
!
      integer lda,n,ml,mu,ipvt(1),job
      double precision abd(lda,1),b(1)
!
      double precision ddot,t
      integer k,kb,l,la,lb,lm,m,nm1
!
      m = mu + ml + 1
      nm1 = n - 1
      if (job .ne. 0) go to 50
!
!        job = 0 , solve  a * x = b
!        first solve l*y = b
!
         if (ml .eq. 0) go to 30
         if (nm1 .lt. 1) go to 30
            do 20 k = 1, nm1
               lm = min0(ml,n-k)
               l = ipvt(k)
               t = b(l)
               if (l .eq. k) go to 10
                  b(l) = b(k)
                  b(k) = t
   10          continue
               call daxpy(lm,t,abd(m+1,k),1,b(k+1),1)
   20       continue
   30    continue
!
!        now solve  u*x = y
!
         do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/abd(m,k)
            lm = min0(k,m) - 1
            la = m - lm
            lb = k - lm
            t = -b(k)
            call daxpy(lm,t,abd(la,k),1,b(lb),1)
   40    continue
      go to 100
   50 continue
!
!        job = nonzero, solve  trans(a) * x = b
!        first solve  trans(u)*y = b
!
         do 60 k = 1, n
            lm = min0(k,m) - 1
            la = m - lm
            lb = k - lm
            t = ddot(lm,abd(la,k),1,b(lb),1)
            b(k) = (b(k) - t)/abd(m,k)
   60    continue
!
!        now solve trans(l)*x = y
!
         if (ml .eq. 0) go to 90
         if (nm1 .lt. 1) go to 90
            do 80 kb = 1, nm1
               k = n - kb
               lm = min0(ml,n-k)
               b(k) = b(k) + ddot(lm,abd(m+1,k),1,b(k+1),1)
               l = ipvt(k)
               if (l .eq. k) go to 70
                  t = b(l)
                  b(l) = b(k)
                  b(k) = t
   70          continue
   80       continue
   90    continue
  100 continue
      return
      end
      subroutine dgefa(a,lda,n,ipvt,info)
!
!***********************************************************************
!
!! DGEFA factors a double precision matrix by gaussian elimination.
!
!     dgefa is usually called by dgeco, but it can be called
!     directly with a saving in time if  rcond  is not needed.
!     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
!
!     on entry
!
!        a       double precision(lda, n)
!                the matrix to be factored.
!
!        lda     integer
!                the leading dimension of the array  a .
!
!        n       integer
!                the order of the matrix  a .
!
!     on return
!
!        a       an upper triangular matrix and the multipliers
!                which were used to obtain it.
!                the factorization can be written  a = l*u  where
!                l  is a product of permutation and unit lower
!                triangular matrices and  u  is upper triangular.
!
!        ipvt    integer(n)
!                an integer vector of pivot indices.
!
!        info    integer
!                = 0  normal value.
!                = k  if  u(k,k) .eq. 0.0 .  this is not an error
!                     condition for this subroutine, but it does
!                     indicate that dgesl or dgedi will divide by zero
!                     if called.  use  rcond  in dgeco for a reliable
!                     indication of singularity.
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!     subroutines and functions
!
!     blas daxpy,dscal,idamax
!
      integer lda,n,ipvt(1),info
      double precision a(lda,1)
!
      double precision t
      integer idamax,j,k,kp1,l,nm1
!
!
!     gaussian elimination with partial pivoting
!
      info = 0
      nm1 = n - 1
      if (nm1 .lt. 1) go to 70
      do 60 k = 1, nm1
         kp1 = k + 1
!
!        find l = pivot index
!
         l = idamax(n-k+1,a(k,k),1) + k - 1
         ipvt(k) = l
!
!        zero pivot implies this column already triangularized
!
         if (a(l,k) .eq. 0.0d0) go to 40
!
!           interchange if necessary
!
            if (l .eq. k) go to 10
               t = a(l,k)
               a(l,k) = a(k,k)
               a(k,k) = t
   10       continue
!
!           compute multipliers
!
            t = -1.0d0/a(k,k)
            call dscal(n-k,t,a(k+1,k),1)
!
!           row elimination with column indexing
!
            do 30 j = kp1, n
               t = a(l,j)
               if (l .eq. k) go to 20
                  a(l,j) = a(k,j)
                  a(k,j) = t
   20          continue
               call daxpy(n-k,t,a(k+1,k),1,a(k+1,j),1)
   30       continue
         go to 50
   40    continue
            info = k
   50    continue
   60 continue
   70 continue
      ipvt(n) = n
      if (a(n,n) .eq. 0.0d0) info = n
      return
      end
      subroutine dgesl(a,lda,n,ipvt,b,job)
!
!***********************************************************************
!
!! DGESL solves the double precision system
!     a * x = b  or  trans(a) * x = b
!     using the factors computed by dgeco or dgefa.
!
!     on entry
!
!        a       double precision(lda, n)
!                the output from dgeco or dgefa.
!
!        lda     integer
!                the leading dimension of the array  a .
!
!        n       integer
!                the order of the matrix  a .
!
!        ipvt    integer(n)
!                the pivot vector from dgeco or dgefa.
!
!        b       double precision(n)
!                the right hand side vector.
!
!        job     integer
!                = 0         to solve  a*x = b ,
!                = nonzero   to solve  trans(a)*x = b  where
!                            trans(a)  is the transpose.
!
!     on return
!
!        b       the solution vector  x .
!
!     error condition
!
!        a division by zero will occur if the input factor contains a
!        zero on the diagonal.  technically this indicates singularity
!        but it is often caused by improper arguments or improper
!        setting of lda .  it will not occur if the subroutines are
!        called correctly and if dgeco has set rcond .gt. 0.0
!        or dgefa has set info .eq. 0 .
!
!     to compute  inverse(a) * c  where  c  is a matrix
!     with  p  columns
!           call dgeco(a,lda,n,ipvt,rcond,z)
!           if (rcond is too small) go to ...
!           do 10 j = 1, p
!              call dgesl(a,lda,n,ipvt,c(1,j),0)
!        10 continue
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!     subroutines and functions
!
!     blas daxpy,ddot
!
      integer lda,n,ipvt(1),job
      double precision a(lda,1),b(1)
!
      double precision ddot,t
      integer k,kb,l,nm1
!
      nm1 = n - 1
      if (job .ne. 0) go to 50
!
!        job = 0 , solve  a * x = b
!        first solve  l*y = b
!
         if (nm1 .lt. 1) go to 30
         do 20 k = 1, nm1
            l = ipvt(k)
            t = b(l)
            if (l .eq. k) go to 10
               b(l) = b(k)
               b(k) = t
   10       continue
            call daxpy(n-k,t,a(k+1,k),1,b(k+1),1)
   20    continue
   30    continue
!
!        now solve  u*x = y
!
         do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/a(k,k)
            t = -b(k)
            call daxpy(k-1,t,a(1,k),1,b(1),1)
   40    continue
      go to 100
   50 continue
!
!        job = nonzero, solve  trans(a) * x = b
!        first solve  trans(u)*y = b
!
         do 60 k = 1, n
            t = ddot(k-1,a(1,k),1,b(1),1)
            b(k) = (b(k) - t)/a(k,k)
   60    continue
!
!        now solve trans(l)*x = y
!
         if (nm1 .lt. 1) go to 90
         do 80 kb = 1, nm1
            k = n - kb
            b(k) = b(k) + ddot(n-k,a(k+1,k),1,b(k+1),1)
            l = ipvt(k)
            if (l .eq. k) go to 70
               t = b(l)
               b(l) = b(k)
               b(k) = t
   70       continue
   80    continue
   90    continue
  100 continue
      return
      end
      SUBROUTINE DSCAL(N,DA,DX,INCX)
!
!***********************************************************************
!
!! DSCAL SCALES A VECTOR BY A CONSTANT.
!     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!
      DOUBLE PRECISION DA,DX(1)
      INTEGER I,INCX,M,MP1,N,NINCX
!
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GO TO 20
!
!   CODE FOR INCREMENT NOT EQUAL TO 1
!
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
      DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
!
!   CODE FOR INCREMENT EQUAL TO 1
!
!
!   CLEAN-UP LOOP
!
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I + 1) = DA*DX(I + 1)
        DX(I + 2) = DA*DX(I + 2)
        DX(I + 3) = DA*DX(I + 3)
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE
      RETURN
      END
      INTEGER FUNCTION IDAMAX(N,DX,INCX)
!
!***********************************************************************
!
!! IDAMAX FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!
      DOUBLE PRECISION DX(1),DMAX
      INTEGER I,INCX,IX,N
!
      IDAMAX = 0
      IF( N .LT. 1 ) RETURN
      IDAMAX = 1
      IF(N.EQ.1)RETURN
      IF(INCX.EQ.1)GO TO 20
!
!   CODE FOR INCREMENT NOT EQUAL TO 1
!
      IX = 1
      DMAX = DABS(DX(1))
      IX = IX + INCX
      DO 10 I = 2,N
       IF(DABS(DX(IX)).LE.DMAX) GO TO 5
        IDAMAX = I
        DMAX = DABS(DX(IX))
    5   IX = IX + INCX
   10 CONTINUE
      RETURN
!
!   CODE FOR INCREMENT EQUAL TO 1
!
   20 DMAX = DABS(DX(1))
      DO 30 I = 2,N
        IF(DABS(DX(I)).LE.DMAX) GO TO 30
        IDAMAX = I
        DMAX = DABS(DX(I))
   30 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
!
!***********************************************************************
!
!! DDOT FORMS THE DOT PRODUCT OF TWO VECTORS.
!     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!
      DOUBLE PRECISION DX(1),DY(1),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
!
      DDOT = 0.0D0
      DTEMP = 0.0D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
!
!    CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
!    NOT EQUAL TO 1
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = DTEMP + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      DDOT = DTEMP
      RETURN
!
!    CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!    CLEAN-UP LOOP
!
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
      DTEMP = DTEMP + DX(I)*DY(I) + DX(I + 1)*DY(I + 1) +
     &  DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
   60 DDOT = DTEMP
      RETURN
      END
      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
!
!***********************************************************************
!
!! DAXPY: CONSTANT TIMES A VECTOR PLUS A VECTOR.
!
!     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!
      DOUBLE PRECISION DX(1),DY(1),DA
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
!
      IF(N.LE.0)RETURN
      IF (DA .EQ. 0.0D0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
!
!    CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
!    NOT EQUAL TO 1
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DY(IY) + DA*DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
!
!   CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!   CLEAN-UP LOOP
!
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
   50 CONTINUE
      RETURN
      END
      function ch_is_digit ( c )
!
!*******************************************************************************
!
!! CH_IS_DIGIT returns .TRUE. if a character is a decimal digit.
!
!
!  Modified:
!
!    15 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the character to be analyzed.
!
!    Output, logical CH_IS_DIGIT, .TRUE. if C is a digit, .FALSE. otherwise.
!
      implicit none
!
      character c
      logical ch_is_digit
!
      if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then
        ch_is_digit = .true.
      else
        ch_is_digit = .false.
      end if

      return
      end
      subroutine ch_to_digit ( c, digit )
!
!*******************************************************************************
!
!! CH_TO_DIGIT returns the integer value of a base 10 digit.
!
!
!  Example:
!
!     C   DIGIT
!    ---  -----
!    '0'    0
!    '1'    1
!    ...  ...
!    '9'    9
!    ' '    0
!    'X'   -1
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the decimal digit, '0' through '9' or blank
!    are legal.
!
!    Output, integer DIGIT, the corresponding integer value.  If C was
!    'illegal', then DIGIT is -1.
!
      implicit none
!
      character c
      integer digit
!
      if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

        digit = ichar ( c ) - 48

      else if ( c .eq. ' ' ) then

        digit = 0

      else

        digit = -1

      end if

      return
      end
      subroutine digit_inc ( c )
!
!*******************************************************************************
!
!! DIGIT_INC increments a decimal digit.
!
!
!  Example:
!
!    Input  Output
!    -----  ------
!    '0'    '1'
!    '1'    '2'
!    ...
!    '8'    '9'
!    '9'    '0'
!    'A'    'A'
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character C, a digit to be incremented.
!
      implicit none
!
      character c
      integer digit

      call ch_to_digit ( c, digit )

      if ( digit .eq. -1 ) then
        return
      end if

      digit = digit + 1

      if ( digit .eq. 10 ) then
        digit = 0
      end if

      call digit_to_ch ( digit, c )

      return
      end
      subroutine digit_to_ch ( digit, c )
!
!*******************************************************************************
!
!! DIGIT_TO_CH returns the character representation of a decimal digit.
!
!
!  Example:
!
!    DIGIT   C
!    -----  ---
!      0    '0'
!      1    '1'
!    ...    ...
!      9    '9'
!     17    '*'
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer DIGIT, the digit value between 0 and 9.
!
!    Output, character C, the corresponding character, or '*' if DIGIT
!    was illegal.
!
      implicit none
!
      character c
      integer digit
!
      if ( 0 .le. digit .and. digit .le. 9 ) then

        c = char ( digit + 48 )

      else

        c = '*'

      end if

      return
      end
      subroutine file_name_inc ( file_name )
!
!*******************************************************************************
!
!! FILE_NAME_INC generates the next filename in a series.
!
!
!  Discussion:
!
!    It is assumed that the digits in the name, whether scattered or
!    connected, represent a number that is to be increased by 1 on
!    each call.  If this number is all 9's on input, the output number
!    is all 0's.  Non-numeric letters of the name are unaffected, and
!    if the name contains no digits, then nothing is done.
!
!  Examples:
!
!      Input          Output
!      -----          ------
!      a7to11.txt     a7to12.txt
!      a7to99.txt     a8to00.txt
!      a9to99.txt     a0to00.txt
!      cat.txt        cat.txt
!
!  Modified:
!
!    09 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) FILE_NAME.
!    On input, a character string to be incremented.
!    On output, the incremented string.
!
      implicit none
!
      character c
      logical ch_is_digit
      character*(*) file_name
      integer i
      integer lens
!
      lens = len ( file_name )

      do i = lens, 1, -1

        c = file_name(i:i)

        if ( ch_is_digit ( c ) ) then

          call digit_inc ( c )

          file_name(i:i) = c

          if ( c .ne. '0' ) then
            return
          end if

        end if

      end do

      return
      end
