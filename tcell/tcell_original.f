c***********************************************************************
c  
c     NAVIER-STOKES Code   using primitive variables on
c     a Tcell
c
c    2002. 06. 18.  Hyung-Chun Lee
c                   Department of Mathematics
c                   Ajou University
c
c***********************************************************************
c
c  The fluid flow problem is formulated in terms of
c  primitive variables - u,v, and p.
c
c  u_t - laplacian u + (u.grad)u + grad p = f
c                              div u = 0
c
c  Boundary conditions:  (u,v)=(0,0) on top
c                        (u,v)=0 on left, right and bottom
c
c  This version uses finite element techniques
c  with piecewise linear functions on triangles to approximate
c  the pressure and quadratics on triangles for the velocity
c  (Taylor-Hood element), isoparametric element
c
c*********************************************************************
c input= te.dat
c output=up.dat,ue.dat,curls
c
      parameter(nx=41)
      parameter(ny=41)
      parameter(mx=2*nx-1)
      parameter(my=2*ny-1)
      parameter(maxel=  2*(nx-1)*(ny-1) )
      parameter(maxnd=  mx*my)
      parameter(maxun=  2*mx*my+nx*ny)
      parameter(minun=  27*ny)
c
      implicit double precision(a-h,o-z)
c
      double precision xc(maxnd),yc(maxnd)
      double precision area(maxel),xm(maxel,3),ym(maxel,3)
      double precision a(minun,maxun),f(maxun)
      double precision g(maxun),gg(maxun),gp(maxun),uold(maxun)
      integer node(maxnd,6),indx(maxnd,2),insc(maxnd)
      integer ipivot(maxun)
      character*20 filename

      filename = 'up000.txt'
c
c  Set functions for input profile
c
c   set input data
c
c       open(unit=0,file='xy.dat')
       open(unit=7,file='up.dat')
       open(unit=10,file='fuini.dat')
c 
       iwrite=0
c
c   input
c
       xlngth=1.0d0
       ylngth=1.0d0
c
       reynld=1.d0
       mrow1=minun
       nsim=3
       nsteps=10
       tolns=1.d-6
       tolopt=1.d-6
	pi=4.d0*datan(1.d0)
c
c  2.  setgrd constructs grid, numbers unknowns, calculates areas,
c      and points for midpoint quadrature rule
c      SETGRD sets bandwidth and neqn1
c
      call setgrd(xc,yc,area,xm,ym,xlngth,ylngth,
     .    node,indx,insc,nlband,nband,
     .    nx,ny,nelemn,np,nnodes,nuk,nquad,neqn1,
     .    iwrite,maxnd,maxel)
c
c      write(6,1050) neqn1,nelemn
      nuband=nlband
      nrow1=nlband+nlband+nuband+1
      ncol1=neqn1
      ny2=ny+ny-1
c      write(6,1100) nlband,nuband,nband,nrow1,ncol1,np
c
c      write(6,1040)
c      write(6,1040)
c      write(6,1300) nx
c      write(6,1310) ny
c      write(6,1410) reynld
c      write(6,1040)
c      write(6,1040)
c
c
	deltat=0.0002d0
	rdel=1.d0/deltat
	do 6 i=1,neqn1
	   f(i)=0.d0
	   read(10,*) uold(i)
   6  continue
c     
	do 1000 iter=1,500
        open(unit=20,file='fu.dat')

!
!      	  alpha=2.d0*sin(dble(iter)*0.01*pi)   
!
!
	  if(iter.le.250) then
      	  alpha=80.d0*dble(iter)*deltat+1.d0
c          alpha=5.d0
	  else 
	      alpha=-80.d0*dble(iter)*deltat+9.d0
c          alpha=1.d0
	  endif
c
      do 45 i=1,neqn1
        g(i)=f(i)
 	  f(i)=0.d0
 45   continue
c
      call nstoke(xc,yc,area,xm,ym,
     .     a,f,g,uold,reynld,tolns,xlngth,ylngth,
     .     node,indx,insc,ipivot,mrow1,
     .     nlband,nuband,nband,nrow1,ncol1,
     .     nelemn,np,nnodes,nuk,nquad,neqn1,
     .     nsteps,nsim,iwrite,maxnd,maxel,rdel,alpha)
c
c Save u=(gx,gy) to 'ue.dat' for 't.f'
c
      do 46 i=1,neqn1
          uold(i)=f(i)
  46  continue
c
c  save u=(gx,gy) to 'up.dat'
c       
         call file_name_inc ( filename )
         write ( *, * ) 'Creating ', filename
         open (unit=1,file=filename)
      do 55 ic =1,np
	iuku=indx(ic,1)
	iukv=indx(ic,2)
        iukp=insc(ic)
	if(iuku.eq.0)	then
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
54	if(iukv.le.0) then
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
c	write(0,3031) xc(ic),yc(ic)
55    continue
      close (unit=1)
c	endif
      write(20,*) iter
      do 1001 i=1,neqn1
	   write(20,*) f(i)
1001  continue
      close(20)
1000  continue
c
3031  format(2e25.15)
3033  format(6d10.3)
1040  format(1x)
1050  format(' number of unknowns=',i4,'   number of triangles= ',i4)
1100  format(' nlband=',i4,' nuband=', i4,' nband=',i4/
     .   ' nrow1=',i4,' ncol1=',i4,' # of pts=',i4)
1300  format(' nx=  ',i4)
1310  format(' ny = ',i4)
1410  format(' reynld=',f14.7)
c
      stop
      end
c
c
c **********************************************************************
c
      subroutine setgrd(xc,yc,area,xm,ym,xlngth,ylngth,
     .    node,indx,insc,nlband,nband,
     .    nx,ny,nelemn,np,nnodes,nuk,nquad,neqn1,
     .    iwrite,maxnd,maxel)
c
c***********************************************************************
c
c  Sets up the grid for the problem assuming we are using quadratics
c     for the velocity and linears for the pressure
c  Input needed is nx,ny,xlngth,ylngth,write
c  Computes arrays node,area,xc,yc,xm,ym,index,insc
c  Computes neqn1, bandwidth information
c
      implicit double precision(a-h,o-z)
      double precision xc(*),yc(*)
      double precision area(*),xm(maxel,*),ym(maxel,*)
      integer node(maxnd,*),indx(maxnd,*),insc(*)
c
c  set parameters for Taylor-Hood element
c
      nnodes=6
      nuk=3
c
c  construct grid
c  coordinates and ordering of unknowns
c
      nym1=ny-1
      nxm1=nx-1
      nrow=nx+nxm1
      ncol=ny+nym1
      hx=xlngth/dble(nxm1)
      hx1=hx/2.d0
      hy=ylngth/dble(nym1)
      hy1=hy/2.d0
c
      i=0
      ip=0
      it1=-1
      xx=0.d0-hx1
c
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
c
c Index for Dirichlet B.C.
c
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
c
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
c
c Index for Dirichlet B.C.
c
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
c
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
c
c Index for Dirichlet B.C.
c
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
c
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
c
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
c
c Index for Dirichlet B.C.
c
	  if(jc.eq.1.or.jc.eq.ny) go to 320
c	  if(ic.eq.nrow) go to 320
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
c
      np=ip
      neqn1=i
c      write(6,*) 'Do you want the information of grid ? (Yes:1, No:2)'
c      read(5,*) iw
       iw=2
       if (iw.eq.1)
     .    write(iwrite,1089) (i,xc(i),yc(i),indx(i,1),
     .                  indx(i,2),insc(i),i=1,np)
       if(iw.eq.1)
     .    write(iwrite,1099) (it,(node(it,i),i=1,6),it=1,nelemn)

c  set quadrature information
c  quadrature rule is midpoint
c  coordinates and weights are set by routine QD7PT
c
      nquad=3
      do 60 it=1,nelemn
	xm(it,1)=0.5d0
	xm(it,2)=0.5d0
	xm(it,3)=0.0d0
	ym(it,1)=0.0d0
	ym(it,2)=0.5d0
	ym(it,3)=0.5d0
	area(it)=0.5d0
   60   continue
c	 if (iwrite.gt.1)
c     .	    write(iwrite,2325) ((xm(n,k),ym(n,k),k=1,3),n=1,nelemn)
c2325  format(' qdpt',2d12.5)
c
c  half band width
c
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
c
 1099 format(7i6)
 1089 format(i5,2f12.5,3i5)
 2001 format(' unknown numbers along line:', 7i5)
c
      return
      end
c
c**********************************************************************
c
      subroutine nstoke(xc,yc,area,xm,ym,
     .      a,f,g,uold,reynld,tolns,xlngth,ylngth,
     .      node,indx,insc,ipivot,mrow1,
     .      nlband,nuband,nband,nrow1,ncol1,
     .      nelemn,np,nnodes,nuk,nquad,neqn1,
     .      nsteps,nsim,iwrite,maxnd,maxel,rdel,alpha)
c
c**********************************************************************
c  solves the navier stokes equation using taylor-hood element
c**********************************************************************
c
      implicit double precision(a-h,o-z)
c
      double precision xc(*),yc(*)
      double precision area(*),xm(maxel,*),ym(maxel,*)
c
      double precision a(mrow1,*)
      double precision f(*)
      double precision g(*)
      double precision uold(*)
      integer ipivot(*)
      integer node(maxnd,*),indx(maxnd,*),insc(*)
c
      double precision un(2)
      double precision uny(2)
      double precision unx(2)
c
c  zero arrays
c  g array contains the previous iterate
c  f array contains the right hand side initially and then the current
c  iterate is overwritten on f
c  T contains the Temperature for RHS
c
      visc=1.d0/reynld
c
c  matrix assembly triangle by triangle
c  nsim is the number of simple iterations performed
c
      csim=0.d0
      do 700  iter=1, nsteps
c        open(12,file='uenew.dat',status='unknown')
        niter=iter
	  if(iter.gt.nsim) csim=1.d0
c
        do 120 i=1,nrow1
          do 120 j=1,ncol1
120	     a(i,j)=0.d0
c
c        write(6,1066) iter
        do 240 it=1,nelemn
	    arr=area(it)/3.d0
          do 230 iquad=1,nquad
            y=ym(it,iquad)
            x=xm(it,iquad)
	      call trans(it,x,y,det,xix,xiy,etax,etay,xc,yc,node,maxnd)
            ar=arr*det
c
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
 150	        continue
 140	    continue
c	     
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
     .		       +csim*( (un(1)*unx(1)+un(2)*uny(1))*bb )*ar
     .			   +rdel*uold(i)*bb*ar
                if(iuk.eq.2) f(i)=f(i)
     .		       +csim*( (un(1)*unx(2)+un(2)*uny(2))*bb )*ar
     .			   +rdel*uold(i)*bb*ar
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
     .                   +(bbb*unx(1)*bb)*csim
     .                   +bb*bbx*un(1)
     .                   +bb*bby*un(2) + rdel*(bb*bbb)
                      if(iukk.eq.2) aij=csim*(bb*bbb*uny(1))
                      if(iukk.eq.3) aij=-bx*bbbl
          		  else if(iuk.eq.2) then
                      if(iukk.eq.2) aij=(visc*(by*bby+bx*bbx)
     .	                 +(bb*bbb*uny(2))*csim
     .                   +bb*bby*un(2)
     .		         +bb*bbx*un(1)) + rdel*(bb*bbb)
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
c
c  add terms to rhs for inhomogeneous boundary condition
c
 185		          continue
                    f(i)=f(i)-ar*alpha*ubdry(iukk,ipp,xc,yc,ylngth)*aij
 190              continue
 200            continue
 210          continue
 220        continue
 230      continue
 240    continue
	  f(neqn1)=0.d0
        do 250 j=neqn1-nlband,neqn1-1
           i=neqn1-j+nband
           a(i,j)=0.d0
 250    continue
	  a(nband,neqn1)=1.d0
c
c  solve system
c
        job=0
        call dgbfa(a,mrow1,neqn1,nlband,nuband,ipivot,info)
	  call dgbsl(a,mrow1,neqn1,nlband,nuband,ipivot,f,job)
        if(info.eq.0) go to 509
c       write(6, 1020) info
c
 509    continue
c
c  check for convergence
c
        diff=0.d0
        do 620 i=1,neqn1
          diff=diff+(g(i)-f(i))**2
 620    continue
        diff=sqrt(diff)
        write(6,1045) iter,diff
c        write(iwrite,1045) iter,diff
        if(diff.le.tolns) go to 750
        do 630 i=1,neqn1
          g(i)=f(i)
	    f(i)=0.d0
 630    continue
c      close(11)
c      close(12)
700   continue

c 750  write(6,1050) niter
750    continue
c
c
1020  format('info =',i5)
1045  format('  for iteration no.',i3,' difference in iterates is '
     .	,2d14.8)
1050  format(/,' solutions converged in',i5,' iterations ',//)
1060  format(/,' solution calculated for reynolds number ',f8.2)
1066  format(//'  iteration number',i2)
c
      return
      end
c*********************************************************************
c
      function ubdry(iuk,ip,xc,yc,ylngth)
c
c***********************************************************************
c
      integer iuk
      integer ip
      double precision xc(*),yc(*)
      double precision ylngth
      double precision ubdry
c
c   internal variable
c
      double precision y
c
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
c
c***********************************************************************
c
      function refbsp(x,y,iq)
c
c***********************************************************************
c
      implicit double precision(a-h,o-z)
      double precision x
      double precision y
      integer iq
c
      if(iq.eq.1) refbsp=1.d0-x-y
      if(iq.eq.2) refbsp=x
      if(iq.eq.3) refbsp=y
      return
      end
c
c************************************************************************
c
      subroutine refqbf(x,y,in,bb,bx,by)
c
c************************************************************************
c     quadratic basis functions on reference triangle
c************************************************************************
c
      double precision x
      double precision y
      double precision bb
      double precision bx
      double precision by
      integer in
c
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
c
c ***********************************************************************
c
      subroutine trans(it,xq,yq,det,pj11,pj21,pj12,pj22,
     .                 xc,yc,node,maxnd)
c ***********************************************************************
c
      implicit double precision(a-h,o-z)
      double precision xc(*)
      double precision yc(*)
      integer node(maxnd,*)
c
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
c
c  Compute partial derivatives at point (xq,yq)
c
      f1x=x1*(-3.d0+4.d0*xq+4.d0*yq)
     1	 +x2*(4.d0*xq-1.d0)
     2	 +x4*4.d0*(1.d0-2.d0*xq-yq)
     3	 +x5*4.d0*yq + x6*4.d0*(-yq)
      f1y=x1*(-3.d0+4.d0*xq+4.d0*yq)
     1	 +x3*(4.d0*yq-1.d0)
     2	 +x4*4.d0*(-xq) + x5*4.d0*xq
     3	 +x6*4.d0*(1.d0-xq-2.d0*yq)
      f2x=y1*(-3.d0+4.d0*xq+4.d0*yq)
     1	 +y2*(4.d0*xq-1.d0)
     2	 +y4*4.d0*(1.d0-2.d0*xq-yq)
     3	 +y5*4.d0*yq + y6*4.d0*(-yq)
      f2y=y1*(-3.d0+4.d0*xq+4.d0*yq)
     1	 +y3*(4.d0*yq-1.d0)
     2	 +y4*4.d0*(-xq) + y5*4.d0*xq
     3	 +y6*4.d0*(1.d0-xq-2.d0*yq)
c
c  Compute determinant of transformation evaluated at point (xq,yq)
c
      det=f1x*f2y-f1y*f2x
c
c  Compute j11, j22, j21, j22
c
      pj11=f2y/det
      pj12=-f2x/det
      pj21=-f1y/det
      pj22=f1x/det
      det=dabs(det)
      return
      end
c
c ********************************************************
        subroutine dgbfa(abd,lda,n,ml,mu,ipvt,info)
c ********************************************************
c
      integer lda,n,ml,mu,ipvt(1),info
      double precision abd(lda,1)
c
c     dgbfa factors a double precision band matrix by elimination.
c
c     dgbfa is usually called by dgbco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c
c     on entry
c
c        abd     double precision(lda, n)
c                contains the matrix in band storage.  the columns
c                of the matrix are stored in the columns of  abd  and
c                the diagonals of the matrix are stored in rows
c                ml+1 through 2*ml+mu+1 of  abd .
c                see the comments below for details.
c
c        lda     integer
c                the leading dimension of the array  abd .
c                lda must be .ge. 2*ml + mu + 1 .
c
c        n       integer
c                the order of the original matrix.
c
c        ml      integer
c                number of diagonals below the main diagonal.
c                0 .le. ml .lt. n .
c
c        mu      integer
c                number of diagonals above the main diagonal.
c                0 .le. mu .lt. n .
c                more efficient if  ml .le. mu .
c     on return
c
c        abd     an upper triangular matrix in band storage and
c                the multipliers which were used to obtain it.
c                the factorization can be written  a = l*u  where
c                l  is a product of permutation and unit lower
c                triangular matrices and  u  is upper triangular.
c
c        ipvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if  u(k,k) .eq. 0.0 .  this is not an error
c                     condition for this subroutine, but it does
c                     indicate that dgbsl will divide by zero if
c                     called.  use  rcond  in dgbco for a reliable
c                     indication of singularity.
c
c     band storage
c
c           if  a  is a band matrix, the following program segment
c           will set up the input.
c
c                   ml = (band width below the diagonal)
c                   mu = (band width above the diagonal)
c                   m = ml + mu + 1
c                   do 20 j = 1, n
c                      i1 = max0(1, j-mu)
c                      i2 = min0(n, j+ml)
c                      do 10 i = i1, i2
c                         k = i - j + m
c                         abd(k,j) = a(i,j)
c                10    continue
c                20 continue
c
c           this uses rows  ml+1  through  2*ml+mu+1  of  abd .
c           in addition, the first  ml  rows in  abd  are used for
c           elements generated during the triangularization.
c           the total number of rows needed in  abd  is  2*ml+mu+1 .
c           the  ml+mu by ml+mu  upper left triangle and the
c           ml by ml  lower right triangle are not referenced.
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,dscal,idamax
c     fortran max0,min0
c
c     internal variables
c
      double precision t
      integer i,idamax,i0,j,ju,jz,j0,j1,k,kp1,l,lm,m,mm,nm1
c
c
      m = ml + mu + 1
      info = 0
c
c     zero initial fill-in columns
c
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
c
c     gaussian elimination with partial pivoting
c
      nm1 = n - 1
      if (nm1 .lt. 1) go to 130
      do 120 k = 1, nm1
         kp1 = k + 1
c
c        zero next fill-in column
c
         jz = jz + 1
         if (jz .gt. n) go to 50
         if (ml .lt. 1) go to 50
            do 40 i = 1, ml
               abd(i,jz) = 0.0d0
   40       continue
   50    continue
c
c        find l = pivot index
c
         lm = min0(ml,n-k)
         l = idamax(lm+1,abd(m,k),1) + m - 1
         ipvt(k) = l + k - m
c
c        zero pivot implies this column already triangularized
c
         if (abd(l,k) .eq. 0.0d0) go to 100
c
c           interchange if necessary
c
            if (l .eq. m) go to 60
               t = abd(l,k)
               abd(l,k) = abd(m,k)
               abd(m,k) = t
   60       continue
c
c           compute multipliers
c
            t = -1.0d0/abd(m,k)
            call dscal(lm,t,abd(m+1,k),1)
c
c           row elimination with column indexing
c
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
      integer lda,n,ml,mu,ipvt(1),job
      double precision abd(lda,1),b(1)
c
c     dgbsl solves the double precision band system
c     a * x = b  or  trans(a) * x = b
c     using the factors computed by dgbco or dgbfa.
c
c     on entry
c
c        abd     double precision(lda, n)
c                the output from dgbco or dgbfa.
c
c        lda     integer
c                the leading dimension of the array  abd .
c
c        n       integer
c                the order of the original matrix.
c
c        ml      integer
c                number of diagonals below the main diagonal.
c
c        mu      integer
c                number of diagonals above the main diagonal.
c
c        ipvt    integer(n)
c                the pivot vector from dgbco or dgbfa.
c
c        b       double precision(n)
c                the right hand side vector.
c
c        job     integer
c                = 0         to solve  a*x = b ,
c                = nonzero   to solve  trans(a)*x = b , where
c                            trans(a)  is the transpose.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains a
c        zero on the diagonal.  technically this indicates singularity
c        but it is often caused by improper arguments or improper
c        setting of lda .  it will not occur if the subroutines are
c        called correctly and if dgbco has set rcond .gt. 0.0
c        or dgbfa has set info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dgbco(abd,lda,n,ml,mu,ipvt,rcond,z)
c           if (rcond is too small) go to ...
c           do 10 j = 1, p
c              call dgbsl(abd,lda,n,ml,mu,ipvt,c(1,j),0)
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,ddot
c     fortran min0
c
c     internal variables
c
      double precision ddot,t
      integer k,kb,l,la,lb,lm,m,nm1
c
      m = mu + ml + 1
      nm1 = n - 1
      if (job .ne. 0) go to 50
c
c        job = 0 , solve  a * x = b
c        first solve l*y = b
c
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
c
c        now solve  u*x = y
c
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
c
c        job = nonzero, solve  trans(a) * x = b
c        first solve  trans(u)*y = b
c
         do 60 k = 1, n
            lm = min0(k,m) - 1
            la = m - lm
            lb = k - lm
            t = ddot(lm,abd(la,k),1,b(lb),1)
            b(k) = (b(k) - t)/abd(m,k)
   60    continue
c
c        now solve trans(l)*x = y
c
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
      integer lda,n,ipvt(1),info
      double precision a(lda,1)
c
c     dgefa factors a double precision matrix by gaussian elimination.
c
c     dgefa is usually called by dgeco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the matrix to be factored.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix and the multipliers
c                which were used to obtain it.
c                the factorization can be written  a = l*u  where
c                l  is a product of permutation and unit lower
c                triangular matrices and  u  is upper triangular.
c
c        ipvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if  u(k,k) .eq. 0.0 .  this is not an error
c                     condition for this subroutine, but it does
c                     indicate that dgesl or dgedi will divide by zero
c                     if called.  use  rcond  in dgeco for a reliable
c                     indication of singularity.
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,dscal,idamax
c
c     internal variables
c
      double precision t
      integer idamax,j,k,kp1,l,nm1
c
c
c     gaussian elimination with partial pivoting
c
      info = 0
      nm1 = n - 1
      if (nm1 .lt. 1) go to 70
      do 60 k = 1, nm1
         kp1 = k + 1
c
c        find l = pivot index
c
         l = idamax(n-k+1,a(k,k),1) + k - 1
         ipvt(k) = l
c
c        zero pivot implies this column already triangularized
c
         if (a(l,k) .eq. 0.0d0) go to 40
c
c           interchange if necessary
c
            if (l .eq. k) go to 10
               t = a(l,k)
               a(l,k) = a(k,k)
               a(k,k) = t
   10       continue
c
c           compute multipliers
c
            t = -1.0d0/a(k,k)
            call dscal(n-k,t,a(k+1,k),1)
c
c           row elimination with column indexing
c
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
      integer lda,n,ipvt(1),job
      double precision a(lda,1),b(1)
c
c     dgesl solves the double precision system
c     a * x = b  or  trans(a) * x = b
c     using the factors computed by dgeco or dgefa.
c
c     on entry
c
c        a       double precision(lda, n)
c                the output from dgeco or dgefa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        ipvt    integer(n)
c                the pivot vector from dgeco or dgefa.
c
c        b       double precision(n)
c                the right hand side vector.
c
c        job     integer
c                = 0         to solve  a*x = b ,
c                = nonzero   to solve  trans(a)*x = b  where
c                            trans(a)  is the transpose.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains a
c        zero on the diagonal.  technically this indicates singularity
c        but it is often caused by improper arguments or improper
c        setting of lda .  it will not occur if the subroutines are
c        called correctly and if dgeco has set rcond .gt. 0.0
c        or dgefa has set info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dgeco(a,lda,n,ipvt,rcond,z)
c           if (rcond is too small) go to ...
c           do 10 j = 1, p
c              call dgesl(a,lda,n,ipvt,c(1,j),0)
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,ddot
c
c     internal variables
c
      double precision ddot,t
      integer k,kb,l,nm1
c
      nm1 = n - 1
      if (job .ne. 0) go to 50
c
c        job = 0 , solve  a * x = b
c        first solve  l*y = b
c
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
c
c        now solve  u*x = y
c
         do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/a(k,k)
            t = -b(k)
            call daxpy(k-1,t,a(1,k),1,b(1),1)
   40    continue
      go to 100
   50 continue
c
c        job = nonzero, solve  trans(a) * x = b
c        first solve  trans(u)*y = b
c
         do 60 k = 1, n
            t = ddot(k-1,a(1,k),1,b(1),1)
            b(k) = (b(k) - t)/a(k,k)
   60    continue
c
c        now solve trans(l)*x = y
c
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
            SUBROUTINE  DSCAL(N,DA,DX,INCX)
C
C     SCALES A VECTOR BY A CONSTANT.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DA,DX(1)
      INTEGER I,INCX,M,MP1,N,NINCX
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C   CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
      DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
C
C   CODE FOR INCREMENT EQUAL TO 1
C
C
C   CLEAN-UP LOOP
C
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
C
C     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(1),DMAX
      INTEGER I,INCX,IX,N
C
      IDAMAX = 0
      IF( N .LT. 1 ) RETURN
      IDAMAX = 1
      IF(N.EQ.1)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C   CODE FOR INCREMENT NOT EQUAL TO 1
C
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
C
C   CODE FOR INCREMENT EQUAL TO 1
C
   20 DMAX = DABS(DX(1))
      DO 30 I = 2,N
        IF(DABS(DX(I)).LE.DMAX) GO TO 30
        IDAMAX = I
        DMAX = DABS(DX(I))
   30 CONTINUE
      RETURN
      END


      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
C
C     FORMS THE DOT PRODUCT OF TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(1),DY(1),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      DDOT = 0.0D0
      DTEMP = 0.0D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C    CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C    NOT EQUAL TO 1
C
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
C
C    CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C    CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
      DTEMP = DTEMP + DX(I)*DY(I) + DX(I + 1)*DY(I + 1) +
     *  DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
   60 DDOT = DTEMP
      RETURN
      END


      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
C
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(1),DY(1),DA
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF (DA .EQ. 0.0D0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C    CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C    NOT EQUAL TO 1
C
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
C
C   CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C   CLEAN-UP LOOP
C
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
