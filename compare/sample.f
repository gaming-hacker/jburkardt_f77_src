c  SAMTEST.F  10 November 1995
c
      program samtest
c
c***********************************************************************
c
      integer maxnx
      integer maxny
c
      parameter (maxnx=81)
      parameter (maxny=25)
c
      integer maxelm
      integer maxeqn
      integer maxnp
      integer maxparb
      integer maxqud
c
      parameter (maxelm=2*(maxnx-1)*(maxny-1))
      parameter (maxeqn=2*(2*maxnx-1)*(2*maxny-1)+maxnx*maxny)
      parameter (maxnp=(2*maxnx-1)*(2*maxny-1))
      parameter (maxparb=5)
      parameter (maxqud=7)
c
      double precision areac(maxqud,maxelm)
      double precision areaf(maxqud,maxelm)
      character*40 filec
      character*40 filef
      double precision gc(maxeqn)
      double precision gcf(maxeqn)
      double precision gf(maxeqn)
      integer i
      integer ibs
      integer ieqn
      integer indxc(3,maxnp)
      integer indxf(3,maxnp)
      integer isotric(maxelm)
      integer isotrif(maxelm)
      integer nelemc
      integer nelemf
      integer neqnc
      integer neqnf
      integer nodec(6,maxelm)
      integer nodef(6,maxelm)
      integer npc
      integer npf
      integer nparb
      integer nquadc
      integer nquadf
      integer nxc
      integer nxf
      integer nyc
      integer nyf
      double precision pval(maxnp)
      double precision splbmp(4,maxparb+2,0:maxparb)
      double precision taubmp(maxparb+2)
      double precision uval(maxnp)
      double precision vval(maxnp)
      double precision xbl
      double precision xbr
      double precision xcc(maxnp)
      double precision xcf(maxnp)
      double precision ycc(maxnp)
      double precision ycf(maxnp)
c
      write(*,*)' '
      write(*,*)'SAMTEST'
      write(*,*)'10 November 1995'
      write(*,*)' '
      write(*,*)'MAXNX= ',maxnx
      write(*,*)'MAXNY= ',maxny
      write(*,*)'MAXELM=',maxelm
      write(*,*)'MAXEQN=',maxeqn
      write(*,*)'MAXNP= ',maxnp
      write(*,*)' '
      write(*,*)'Compare two problem solutions on different meshes.'
c
c  Get fine mesh input file name.
c
      write(*,*)' '
      write(*,*)'Enter filename for fine mesh data input:'
      read(*,'(a)')filef
      write(*,*)'Fine mesh data is taken from '//filef

      open(unit=17,file=filef,form='formatted',status='old')
c
c  Read scalars and vectors describing the fine mesh.
c
      call reads(ibs,nelemf,neqnf,npf,nparb,nquadf,nxf,nyf,xbl,xbr)

      call readv(areaf,gf,indxf,isotrif,nelemf,neqnf,nodef,npf,
     &  nparb,nquadf,splbmp,taubmp,xcf,ycf)

      close(unit=17)
c
c  Get coarse mesh input file name.
c
      write(*,*)' '
      write(*,*)'Enter filename for coarse mesh data input:'
      read(*,'(a)')filec
      write(*,*)'Coarse mesh data is taken from '//filec

      open(unit=17,file=filec,form='formatted',status='old')
c
c  Read scalars and vectors describing the coarse mesh.
c
      call reads(ibs,nelemc,neqnc,npc,nparb,nquadc,nxc,nyc,xbl,xbr)

      call readv(areac,gc,indxc,isotric,nelemc,neqnc,nodec,npc,
     &  nparb,nquadc,splbmp,taubmp,xcc,ycc)

      close(unit=17)
c
c  Compute the refinement factor.
c
      write(*,*)'The refinement factor is ',real(nxf-1)/real(nxc-1)
c
c  Evaluate the coarse mesh solution at the nodes of the fine mesh.
c
      call sample(gc,indxc,isotric,nelemc,neqnc,nodec,npc,
     &  npf,nxc,nyc,pval,uval,vval,xcc,xcf,ycc,ycf)
c
c  Set up a solution vector for the transferred coarse data.
c
      ieqn=0
      do i=1,npf
        ieqn=ieqn+1
        gcf(ieqn)=uval(i)
        ieqn=ieqn+1
        gcf(ieqn)=vval(i)
        if(indxf(3,i).gt.0)then
          ieqn=ieqn+1
          gcf(ieqn)=pval(i)
        endif
      enddo
c
c  Compute the L2 difference on the fine mesh.
c
      call l2dif(areaf,gcf,gf,indxf,isotrif,nelemf,neqnf,nodef,npf,
     &  nquadf,xcf,ycf)
c
c  Compute the little L2 difference on the fine mesh.
c
      call ll2dif(gcf,gf,indxf,neqnf,npf)
c
c  Compute the little L-infinity difference on the fine mesh.
c
      call llinfdif(gcf,gf,indxf,neqnf,npf)

      stop
      end
      subroutine sample(g,indx,isotri,nelem,neqn,node,np,nval,nx,ny,
     &  pval,uval,vval,xc,xval,yc,yval)
c
c***********************************************************************
c
c  SAMPLE evaluates a finite element flow solution, whose coefficients
c  are contained in G, at a set of data points (XVAL(I), YVAL(I)),
c  and returns these values in (UVAL(I), VVAL(I), PVAL(I)).
c
c
c  SAMPLE can be used to "plot" a solution by evaluating it at an
c  arbitrary set of points.
c
c  SAMPLE can be used to extend a solution to a finer mesh, or
c  restrict it to a coarser mesh, by sampling the solution at the
c  nodes of the new mesh.
c
c  SAMPLE can be used to compare solutions on two different meshes,
c  by sampling the solution associated with the coarse mesh at the
c  nodes of the fine mesh.
c
c
c  G      Input, double precision G(NEQN).
c
c         G is the computed solution vector, in which are stored
c         pressures and velocities.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry G(K).
c 
c         If INDX(I,J) is positive, then that means that a degree of
c         freedom for variable I (U, V or P) is associated with node
c         J, and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine variable I at node J.
c
c  ISOTRI Input, integer ISOTRI(NELEM).
c
c         0, the element is NOT isoparametric, and the nodes never move.
c         That means that the quadrature points are only computed once.
c 
c         1, the element is NOT isoparametric, but the nodes may move.
c         Quadrature point locations must be updated on each step.
c         This could occur for elements above, but not touching, the bump.
c
c         2, the element is isoparametric.
c 
c  NELEM  Input, integer NELEM, the number of elements in the mesh.
c
c  NEQN   Input, integer NEQN, the number of equations and coefficients
c         associated with the mesh.
c
c  NODE   integer NODE(6,MAXELM).
c
c         NODE(I,J) contains, for an element J, the global node index of 
c         the element node whose local number is I.
c
c         The local ordering of the nodes is suggested by this diagram:
c
c               2
c              /|
c             4 5
c            /  |
c           1-6-3
c
c  NP     Input, integer NP.
c
c         The number of nodes.  NP=(2*NX-1)*(2*NY-1).
c 
c  NVAL   Input, integer NVAL, the number of points (XVAL(I), YVAL(I))
c         at which the solution is to be sampled.
c
c  NX     Input, integer NX.
c
c         NX controls the spacing of nodes and elements in
c         the X direction.  There are 2*NX-1 nodes along various
c         lines in the X direction.
c
c         Roughly speaking, NX-1 (or 2*(NX-1)) is the number of elements 
c         along a line in the X direction.
c 
c  NY     Input, integer NY.
c
c         NY controls the spacing of nodes and elements in
c         the Y direction.  There are 2*NY-1 nodes along various
c         lines in the Y direction.
c
c         Roughly speaking, NY-1 (or 2*(NY-1)) is the number of elements
c         along a line in the Y direction.
c 
c  PVAL   Output, double precision PVAL(NVAL).
c         The value of the pressure at the points (XVAL(I),YVAL(I)).
c
c  UVAL   Output, double precision UVAL(NVAL).
c         The value of the horizontal velocity at the points 
c         (XVAL(I),YVAL(I)).
c
c  VVAL   Output, double precision VVAL(NVAL).
c         The value of the vertical velocity at the points 
c         (XVAL(I),YVAL(I)).
c
c  XBL    Input, double precision XBL.
c
c         The X coordinate of the left corner of the bump.
c 
c  XBR    Input, double precision XBR.
c
c         The X coordinate of the right corner of the bump.
c
c  XC     Input, double precision XC(NP).
c
c         The X coordinates of the nodes.
c 
c  XVAL   Input, double precision XVAL(NVAL), the X coordinates of the
c         points at which the solution is to be sampled.
c
c  YC     Input, double precision YC(NP).
c
c         The Y coordinates of the nodes.
c
c  YVAL   Input, double precision YVAL(NVAL), the Y coordinates of the
c         points at which the solution is to be sampled.
c 
      integer nelem
      integer neqn
      integer np
      integer nval
c
      double precision eta
      double precision g(neqn)
      integer icol2
      integer ielem
      integer indx(3,np)
      integer irow2
      integer isotri(nelem)
      integer ival
      integer node(6,nelem)
      integer nx
      integer ny
      double precision p
      double precision pval(nval)
      double precision u
      double precision uval(nval)
      double precision v
      double precision vval(nval)
      double precision x
      double precision xc(np)
      double precision xemax
      double precision xemin
      double precision xmax
      double precision xmin
      double precision xsi
      double precision xsi2
      double precision xval(nval)
      double precision y
      double precision yc(np)
      double precision yemax
      double precision yemin
      double precision ymax
      double precision ymin
      double precision yval(nval)
c
c  Set the minimum and maximum coordinates of the region, except for
c  YMIN, which varies.
c
      xmin=0.0
      xmax=10.0
      ymax=3.0

      do ival=1,nval
c
c  Step 1: even if the element is isoperimetric, the vertical sides are 
c  always straight.  So we can "lock in" the X coordinate for sure to 
c  give us the element column.
c
c  Also compute the minimum and maximum X coordinates in the element.
c
        x=xval(ival)

        icol2=1+int((nx-1)*(x-xmin)/(xmax-xmin))
        if(icol2.gt.nx-1)then
          icol2=nx-1
        endif

        xemax=((nx-1-icol2)*xmin+icol2*xmax)/real(nx-1)
c
c  Step 2: Given the X coordinate of a point, we can compute the
c  Y coordinates of all the element boundaries once we know YMIN,
c  the height of the bottom boundary.
c
c  Note: there is a flaw here.  While the boundary can be cubic,
c  the elements are quadratic.  YMIN should be calculated based
c  on the nodes of the element, which lie on the bump.
c
        ielem=(icol2-1)*2*(ny-1)+2
        call bumpy(ielem,nelem,node,np,xc,x,xsi,yc,ymin)
c
c  Step 3: Now compute the element IELEM that contains the point (X,Y).
c  We only have to check the column of elements that contain the
c  given X value.
c
c  We consider these elements a pair at a time.  
c
c  We compute the Y coordinates YEMIN, YEMID and YEMAX where the
c  bottom of the lower element, the boundary of the two elements,
c  and the top of the higher element are intersected by the line
c  X = XVAL(I).
c
c  If YVAL(I) lies in either interval, we've found the element.
c
        y=yval(ival)

        yemax=ymin

        do irow2=1,ny-1

          ielem=(icol2-1)*2*(ny-1)+irow2*2

          yemin=yemax

          eta=xsi
          call xofxsi(eta,ielem,nelem,node,np,xemax,xc,xsi,yemax,yc)

          if(yemin.le.y.and.y.le.yemax)go to 10

          if(irow2.eq.1.and.y.le.yemax)then
            y=yemin
            go to 10
          endif

          ielem=ielem-1

          yemin=yemax

          xsi2=1.0-xsi
          eta=0.0
          call xofxsi(eta,ielem,nelem,node,np,xemax,xc,xsi2,yemax,yc)

          if(yemin.le.y.and.y.le.yemax)go to 10

          if(yemin.le.y.and.irow2.eq.ny-1)then
            y=yemax
            go to 10
          endif
          
        enddo

        write(*,*)' '
        write(*,*)'SAMPLE - Fatal error!'
        write(*,*)'  We could not locate an element containing X,Y.'
        write(*,*)'  This occurred for point IVAL=',ival
        write(*,*)'  XVAL(IVAL), YVAL(IVAL)=',xval(ival),yval(ival)
        write(*,*)'  YMIN(X), YMAX(X)=',ymin,ymax
        stop

10      continue
c
c  Step 4: If the element is isoperimetric, we have to compute the 
c  (XSI,ETA) coordinates of the point (X,Y) in element IELEM.
c
        if(isotri(ielem).eq.2)then

          call xsiofx(eta,ielem,nelem,node,np,xc,xsi,x,yc,y)

        else

          xsi=0.0
          eta=0.0

        endif
c
c  Step 5: Now we can evaluate the solution at the point (X,Y).
c
        call uval0(eta,g,ielem,indx,isotri,nelem,neqn,node,np,p,
     &    u,v,xc,x,xsi,yc,y)

        pval(ival)=p
        uval(ival)=u
        vval(ival)=v

      enddo

      return
      end
      subroutine uval0(eta,g,ielem,indx,isotri,nelem,neqn,node,np,p,
     &  u,v,xc,x,xsi,yc,y)
c
c***********************************************************************
c
c  UVAL0 evaluates the velocities and pressure at an arbitrary point 
c  in a given element.
c
c  If the element is not isoparametric, then UVAL0 requires the
c  physical X and Y coordinates of the point.
c
c  If the element is isoparametric, UVAL0 requires the XSI, ETA
c  coordinates of the point.  
c
c  ETA    Input, double precision ETA, the ETA coordinate of the point,
c         needed only if the element is isoparametric.
c
c  G      Input, double precision G(NEQN).
c
c         G is the computed solution vector, in which are stored
c         pressures and velocities.
c
c  IELEM  Input, integer IELEM, the element in which the point lies
c         at which the quantities are desired.
c
c  X      Input, double precision X, the X coordinate of the point.
c
c  XSI    Input, double precision XSI, the XSI coordinate of the point,
c         needed only if the element is isoparametric.
c
c  Y      Input, double precision Y, the Y coordinate of the point.
c
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer neqn
      integer np
c
      double precision eta
      double precision g(neqn)
      integer ielem
      integer indx(3,np)
      integer ip
      integer iq
      integer isotri(nelem)
      integer iun
      integer node(6,nelem)
      double precision p
      double precision q
      double precision u
      double precision v
      double precision w
      double precision xc(np)
      double precision x
      double precision xsi
      double precision yc(np)
      double precision y
c
      p=0.0
      u=0.0
      v=0.0
 
      do iq=1,6
c
c  Evaluate the basis functions W and Q at X, Y.
c
        if(isotri(ielem).eq.0.or.isotri(ielem).eq.1)then
 
          call qbf0(ielem,iq,w,nelem,node,np,xc,x,yc,y)
 
          call bsp0(q,ielem,iq,nelem,node,np,xc,x,yc,y)
 
        else
 
          call refqbf0(w,eta,iq,xsi)
 
          call refbsp0(q,iq,eta,xsi)
 
        endif
c
c  Compute the coefficients at the node at XP, YP.
c
        ip=node(iq,ielem)

        iun=indx(1,ip)
        u=u+g(iun)*w

        iun=indx(2,ip)
        v=v+g(iun)*w

        iun=indx(3,ip)
        if(iun.gt.0)then
          p=p+g(iun)*q
        endif
 
      enddo
 
      return
      end
      subroutine bsp0(q,ielem,iq,nelem,node,np,xc,xq,yc,yq)
c
c***********************************************************************
c
c  BSP0 computes the value of the linear basis functions associated 
c  with pressure.
c
c
c  Here is a picture of a typical finite element associated with
c  pressure:
c
c      2
c     /|
c    / |
c   /  |
c  1---3
c
c  Q      Output, double precision Q, the value of the IQ-th basis
c         function at the point with global coordinates (XQ,YQ).
c
c  IELEM  Input, integer IELEM, the global element number about which
c         we are inquiring.
c
c  IQ     Input, integer IQ, the index of the desired basis
c         function.  This is also the node of the reference
c         triangle which is associated with the basis function.
c
c         Basis function IQ is 1 at node IQ, and zero at the
c         other two nodes.
c
c  NELEM  Input, integer NELEM, the number of elements.
c
c  NODE   Input, integer NODE(6,MAXELM).  NODE(J,I) is
c         the global node number of the J-th node in the I-th
c         element.
c
c  NP     Input, integer NP, the number of nodes.
c
c  XC     Input, double precision XC(NP), the global X coordinates
c         of the element nodes.
c
c  XQ     Input, double precision XQ, the global X coordinate of
c         the point in which we are interested.
c
c  YC     Input, double precision YC(NP), the global Y coordinates
c         of the element nodes.
c
c  YQ     Input, double precision YQ, the global Y coordinate of
c         the point in which we are interested.
c
      implicit double precision(a-h,o-z)
c
      integer nelem
      integer np
c
      double precision q
      double precision d
      integer i1
      integer i2
      integer i3
      integer ielem
      integer iq
      integer iq1
      integer iq2
      integer iq3
      integer node(6,nelem)
      double precision xc(np)
      double precision xq
      double precision yc(np)
      double precision yq
c
      if(iq.lt.1.or.iq.gt.6)then
        write(*,*)' '
        write(*,*)'BSP0 - Fatal error!'
        write(*,*)'  The requested basis function is IQ=',iq
        write(*,*)'  but only values from 1 to 6 are legal.'
        stop
      elseif(iq.ge.4.and.iq.le.6)then
        q=0.0
        return
      endif
 
      iq1=iq
      iq2=mod(iq,3)+1
      iq3=mod(iq+1,3)+1
 
      i1=node(iq1,ielem)
      i2=node(iq2,ielem)
      i3=node(iq3,ielem)
 
      d= (xc(i2)-xc(i1))*(yc(i3)-yc(i1))
     &  -(xc(i3)-xc(i1))*(yc(i2)-yc(i1))
 
      q=(d+(yc(i2)-yc(i3))*(xq-xc(i1))+(xc(i3)-xc(i2))*(yq-yc(i1)))/d
 
      return
      end
      subroutine interv(xt,lxt,x,left,mflag)
c
c***********************************************************************
c
c  INTERV computes LEFT, the maximum value of I so that
c
c    1 <= I <= XT
c
c  and
c
c    XT(I) <= X.
c
c  The routine is designed to be efficient in the common situation
c  that it is called repeatedly, with X taken from an increasing
c  or decreasing sequence.
c
c  This will happen when a piecewise polynomial is to be graphed.
c  The first guess for LEFT is therefore taken to be the value
c  returned at the previous call and stored in the local variable
c  ILO.
c
c  A first check ascertains that ILO.LT.LXT.  This is necessary
c  since the present call may have nothing to do with the previous
c  call.  Then, if XT(ILO) <= X < XT(ILO+1), we set LEFT=ILO
c  and are done after just three comparisons.
c
c  Otherwise, we repeatedly double the difference ISTEP=IHI-ILO
c  while also moving ILO and IHI in the direction of X, until
c    XT(ILO) <= X < XT(IHI)
c  after which we use bisection to get, in addition, ILO+1=IHI.
c  LEFT=ILO is then returned.
c
c  XT     Input, real XT(LXT), a nondecreasing sequence of 
c         values.
c
c  LXT    Input, integer LXT, the dimension of XT.
c
c  X      Input, real X, the point whose location with 
c         respect to the sequence XT is to be determined.
c
c  LEFT,
c  MFLAG  Output, integer LEFT, integer MFLAG, whose value is
c
c         1     -1      if               X .lt.  XT(1)
c         I      0      if   XT(I)  .le. X .lt. XT(I+1)
c         LXT    1      if  XT(LXT) .le. X
c
c        In particular, MFLAG=0 is the 'usual' case.  MFLAG.ne.0
c        indicates that X lies outside the half open interval
c        XT(1).le.Y.lt.XT(LXT).  The asymmetric treatment of the
c        interval is due to the decision to make all piecewise
c        polynomials continuous from the right.
c
      integer lxt
c
      integer left
      integer mflag
      integer ihi
      integer ilo
      integer istep
      integer middle
      double precision x
      double precision xt(lxt)
c
      save ilo
c
      data ilo / 1 /
c
      ihi=ilo+1
c
      if(ihi.ge.lxt)then

        if(x.ge.xt(lxt))go to 110

        if(lxt.le.1)then
          mflag=-1
          left=1
          return
        endif

        ilo=lxt-1
        ihi=lxt

      endif
c
      if (x .ge. xt(ihi))go to 40

      if(x.ge.xt(ilo))then
        mflag=0
        left=ilo
        return
      endif
c
c  Now X < XT(ILO).  Decrease ILO to capture X.
c
      istep = 1

   31 continue

      ihi = ilo
      ilo = ihi - istep

      if(ilo .gt. 1)then
        if (x .ge. xt(ilo))go to 50
        istep = istep*2
        go to 31
      endif

      ilo = 1

      if(x.lt.xt(1))then
        mflag=-1
        left=1
        return
      endif

      go to 50
c
c  Now X => XT(IHI).  Increase IHI to capture X.
c
   40 continue

      istep = 1

   41 continue

      ilo = ihi
      ihi = ilo + istep

      if(ihi.lt.lxt)then
        if(x.lt.xt(ihi))go to 50
        istep=istep*2
        go to 41
      endif

      if (x .ge. xt(lxt))go to 110

      ihi = lxt
c
c  Now XT(ILO) <= X < XT(IHI).  Narrow the interval.
c
   50 continue

      middle = (ilo + ihi)/2

      if(middle.eq.ilo)then
        mflag=0
        left=ilo
        return
      endif
c
c  It is assumed that MIDDLE = ILO in case IHI = ILO+1.
c
      if(x.ge.xt(middle))then
        ilo=middle
      else
        ihi=middle
      endif

      go to 50
c
c  Set output and return.
c
  110 continue

      mflag = 1

      if(x.eq.xt(lxt))then
        mflag=0
      endif

      do left=lxt,1,-1
        if(xt(left).lt.xt(lxt))return
      enddo

      return
      end
      subroutine l2dif(area,g1,g2,indx,isotri,nelem,neqn,node,np,
     &  nquad,xc,yc)
c
c***********************************************************************
c
c  L2DIF computes the L2 norm of the differences between the values
c  of U, V, and P computed for two different mesh spacings H1 and H2.
c
c  L2DIF can be used to estimate the rate of convergence of a 
c  sequence of solutions, G1, G2, G3, ..., GN, by comparing each
c  solution G to GN.
c
c  L2DIF can also do the same computation for sensitivities; simply
c  pass in the appropriate column of the sensitivity coefficient vectors
c  for the G arguments.
c
c  In order to do the integration with some ease, it is assumed that
c  H2 is a refinement of H1.
c
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer neqn
      integer np
c
      double precision area(nquad,nelem)
      double precision eta
      double precision etaq(7)
      double precision flarea
      double precision g1(neqn)
      double precision g2(neqn)
      integer i
      integer ielem
      integer indx(3,np)
      integer iquad
      integer isotri(nelem)
      integer node(6,nelem)
      integer nquad
      double precision p1
      double precision p1nrm
      double precision p2
      double precision p2nrm
      double precision pdif
      double precision u1
      double precision u1nrm
      double precision u2
      double precision u2nrm
      double precision udif
      double precision v1
      double precision v1nrm
      double precision v2
      double precision v2nrm
      double precision vdif
      double precision x
      double precision xc(np)
      double precision xsi
      double precision xsiq(7)
      double precision y
      double precision yc(np)
c
      if(nquad.eq.3)then
        xsiq(1)=0.5
        etaq(1)=0.5
        xsiq(2)=1.0
        etaq(2)=0.5
        xsiq(3)=0.5
        etaq(3)=0.0
      elseif(nquad.eq.7)then
        xsiq(1)=1.0/3.0
        etaq(1)=1.0/3.0

        xsiq(2)=(6.0+sqrt(15.0))/21.0
        etaq(2)=(6.0+sqrt(15.0))/21.0

        xsiq(3)=(9.0-2.0*sqrt(15.0))/21.0
        etaq(3)=(6.0+sqrt(15.0))/21.0

        xsiq(4)=(6.0+sqrt(15.0))/21.0
        etaq(4)=(9.0-2.0*sqrt(15.0))/21.0

        xsiq(5)=(6.0-sqrt(15.0))/21.0
        etaq(5)=(6.0-sqrt(15.0))/21.0

        xsiq(6)=(9.0+2.0*sqrt(15.0))/21.0
        etaq(6)=(6.0-sqrt(15.0))/21.0

        xsiq(7)=(6.0-sqrt(15.0))/21.0
        etaq(7)=(9.0+2.0*sqrt(15.0))/21.0

        do i=1,nquad
          xsiq(i)=1.0-xsiq(i)
        enddo

      endif
c
c  Compute the current flow area, based on the finer mesh.
c
      flarea=0.0
      do ielem=1,nelem
        do i=1,nquad
          flarea=flarea+area(i,ielem)
        enddo
      enddo
c
c  Now compute the integral by going over each element.
c  We pick an element IELEM2, and must figure out which
c  coarser element IELEM1 contains the same points (and more).
c
      udif=0.0
      vdif=0.0
      pdif=0.0

      u1nrm=0.0
      v1nrm=0.0
      p1nrm=0.0

      u2nrm=0.0
      v2nrm=0.0
      p2nrm=0.0

      do ielem=1,nelem

        do iquad=1,nquad

          xsi=xsiq(iquad)
          eta=etaq(iquad)

          call xofxsi(eta,ielem,nelem,node,np,x,xc,xsi,y,yc)

          call uval0(eta,g1,ielem,indx,isotri,nelem,neqn,
     &       node,np,p1,u1,v1,xc,x,xsi,yc,y)

          call uval0(eta,g2,ielem,indx,isotri,nelem,neqn,
     &       node,np,p2,u2,v2,xc,x,xsi,yc,y)

          udif=udif+area(iquad,ielem)*(u1-u2)**2
          vdif=vdif+area(iquad,ielem)*(v1-v2)**2
          pdif=pdif+area(iquad,ielem)*(p1-p2)**2

          u1nrm=u1nrm+area(iquad,ielem)*u1**2
          v1nrm=v1nrm+area(iquad,ielem)*v1**2
          p1nrm=p1nrm+area(iquad,ielem)*p1**2

          u2nrm=u2nrm+area(iquad,ielem)*u2**2
          v2nrm=v2nrm+area(iquad,ielem)*v2**2
          p2nrm=p2nrm+area(iquad,ielem)*p2**2

        enddo
      enddo
c
c  Take the square root, and normalize by the area.
c
      udif=sqrt(udif)/flarea
      vdif=sqrt(vdif)/flarea
      pdif=sqrt(pdif)/flarea

      u1nrm=sqrt(u1nrm)/flarea
      v1nrm=sqrt(v1nrm)/flarea
      p1nrm=sqrt(p1nrm)/flarea

      u2nrm=sqrt(u2nrm)/flarea
      v2nrm=sqrt(v2nrm)/flarea
      p2nrm=sqrt(p2nrm)/flarea

      write(*,*)' '
      write(*,*)'L2DIF:'
      write(*,*)' '
      write(*,*)'Area          = ',flarea
      write(*,*)' '
      write(*,*)'L2(U1-U2)/Area = ',udif
      write(*,*)'L2(V1-V2)/Area = ',vdif
      write(*,*)'L2(P1-P2)/Area = ',pdif
      write(*,*)' '
      write(*,*)'L2(U1)/Area = ',u1nrm
      write(*,*)'L2(V1)/Area = ',v1nrm
      write(*,*)'L2(P1)/Area = ',p1nrm
      write(*,*)' '
      write(*,*)'L2(U2)/Area = ',u2nrm
      write(*,*)'L2(V2)/Area = ',v2nrm
      write(*,*)'L2(P2)/Area = ',p2nrm

      return
      end
      subroutine ll2dif(g1,g2,indx,neqn,np)
c
c***********************************************************************
c
c  LL2DIF computes the little L2 norm of the differences 
c  between the values of U, V, and P computed for two different mesh 
c  spacings H1 and H2.
c
c  LL2DIF can be used to estimate the rate of convergence of a 
c  sequence of solutions, G1, G2, G3, ..., GN, by comparing each
c  solution G to GN.
c
c  LL2DIF can also do the same computation for sensitivities; simply
c  pass in the appropriate column of the sensitivity coefficient vectors
c  for the G arguments.
c
c  In order to do the integration with some ease, it is assumed that
c  H2 is a refinement of H1.
c
      implicit double precision (a-h,o-z)
c
      integer neqn
      integer np
c
      double precision g1(neqn)
      double precision g2(neqn)
      integer i
      integer indx(3,np)
      integer j
      integer npp
      double precision p1
      double precision p1nrm
      double precision p2
      double precision p2nrm
      double precision pdfnrm
      double precision u1
      double precision u1nrm
      double precision u2
      double precision u2nrm
      double precision udfnrm
      double precision v1
      double precision v1nrm
      double precision v2
      double precision v2nrm
      double precision vdfnrm
c
c  Compute the norms entrywise.
c
      udfnrm=0.0
      vdfnrm=0.0
      pdfnrm=0.0

      u1nrm=0.0
      v1nrm=0.0
      p1nrm=0.0

      u2nrm=0.0
      v2nrm=0.0
      p2nrm=0.0

      npp=0

      do i=1,np
        
        j=indx(1,i)
        u1=g1(j)
        u2=g2(j)

        u1nrm=u1nrm+u1**2
        u2nrm=u2nrm+u2**2
        udfnrm=udfnrm+(u1-u2)**2

        j=indx(2,i)
        v1=g1(j)
        v2=g2(j)

        v1nrm=v1nrm+v1**2
        v2nrm=v2nrm+v2**2
        vdfnrm=vdfnrm+(v1-v2)**2

        j=indx(3,i)

        if(j.gt.0)then

          p1=g1(j)
          p2=g2(j)

          p1nrm=p1nrm+p1**2
          p2nrm=p2nrm+p2**2
          pdfnrm=pdfnrm+(p1-p2)**2

          npp=npp+1

        endif

      enddo
c
c  Take the square root, and normalize.
c
      udfnrm=sqrt(udfnrm)/real(np)
      vdfnrm=sqrt(vdfnrm)/real(np)
      pdfnrm=sqrt(pdfnrm)/real(npp)

      u1nrm=sqrt(u1nrm)/real(np)
      v1nrm=sqrt(v1nrm)/real(np)
      p1nrm=sqrt(p1nrm)/real(npp)

      u2nrm=sqrt(u2nrm)/real(np)
      v2nrm=sqrt(v2nrm)/real(np)
      p2nrm=sqrt(p2nrm)/real(npp)

      write(*,*)' '
      write(*,*)'LL2DIF:'
      write(*,*)' '
      write(*,*)'Number of U and V nodes Np = ',np
      write(*,*)'Number of P nodes Npp = ',npp
      write(*,*)' '
      write(*,*)'l2(U1-U2)/Np  = ',udfnrm
      write(*,*)'l2(V1-V2)/Np  = ',vdfnrm
      write(*,*)'l2(P1-P2)/Npp = ',pdfnrm
      write(*,*)' '
      write(*,*)'l2(U1)/Np     = ',u1nrm
      write(*,*)'l2(V1)/Np     = ',v1nrm
      write(*,*)'l2(P1)/Npp    = ',p1nrm
      write(*,*)' '
      write(*,*)'l2(U2)/Np     = ',u2nrm
      write(*,*)'l2(V2)/Np     = ',v2nrm
      write(*,*)'l2(P2)/Npp    = ',p2nrm

      return
      end
      subroutine llinfdif(g1,g2,indx,neqn,np)
c
c***********************************************************************
c
c  LLINFDIF computes the little L infinity norm of the differences 
c  between the values of U, V, and P computed for two different mesh 
c  spacings H1 and H2.
c
c  LLINFDIF can be used to estimate the rate of convergence of a 
c  sequence of solutions, G1, G2, G3, ..., GN, by comparing each
c  solution G to GN.
c
c  LLINFDIF can also do the same computation for sensitivities; simply
c  pass in the appropriate column of the sensitivity coefficient vectors
c  for the G arguments.
c
c  In order to do the integration with some ease, it is assumed that
c  H2 is a refinement of H1.
c
      implicit double precision (a-h,o-z)
c
      integer neqn
      integer np
c
      double precision g1(neqn)
      double precision g2(neqn)
      integer i
      integer indx(3,np)
      integer ip1nrm
      integer ip2nrm
      integer ipdfnrm
      integer iu1nrm
      integer iu2nrm
      integer iudfnrm
      integer iv1nrm
      integer iv2nrm
      integer ivdfnrm
      integer j
      integer npp
      double precision p1
      double precision p1nrm
      double precision p2
      double precision p2nrm
      double precision pdfnrm
      double precision pdif
      double precision u1
      double precision u1nrm
      double precision u2
      double precision u2nrm
      double precision udfnrm
      double precision udif
      double precision v1
      double precision v1nrm
      double precision v2
      double precision v2nrm
      double precision vdfnrm
      double precision vdif
c
c  Compute the norms entrywise.
c
      udfnrm=0.0
      vdfnrm=0.0
      pdfnrm=0.0

      u1nrm=0.0
      v1nrm=0.0
      p1nrm=0.0

      u2nrm=0.0
      v2nrm=0.0
      p2nrm=0.0

      iudfnrm=0
      ivdfnrm=0
      ipdfnrm=0

      iu1nrm=0
      iv1nrm=0
      ip1nrm=0

      iu2nrm=0
      iv2nrm=0
      ip2nrm=0

      npp=0

      do i=1,np
        
        j=indx(1,i)
        u1=g1(j)
        u2=g2(j)
        udif=abs(u1-u2)

        if(abs(u1).gt.u1nrm)then
          u1nrm=abs(u1)
          iu1nrm=i
        endif
        
        if(abs(u2).gt.u2nrm)then
          u2nrm=abs(u2)
          iu2nrm=i
        endif

        if(abs(udif).gt.udfnrm)then
          udfnrm=abs(udif)
          iudfnrm=i
        endif

        j=indx(2,i)
        v1=g1(j)
        v2=g2(j)
        vdif=abs(v1-v2)

        if(abs(v1).gt.v1nrm)then
          v1nrm=abs(v1)
          iv1nrm=i
        endif
        
        if(abs(v2).gt.v2nrm)then
          v2nrm=abs(v2)
          iv2nrm=i
        endif

        if(abs(vdif).gt.vdfnrm)then
          vdfnrm=abs(vdif)
          ivdfnrm=i
        endif

        j=indx(3,i)
        if(j.gt.0)then
          p1=g1(j)
          p2=g2(j)
          pdif=abs(p1-p2)

          if(abs(p1).gt.p1nrm)then
            p1nrm=abs(p1)
            ip1nrm=i
          endif
        
          if(abs(p2).gt.p2nrm)then
            p2nrm=abs(p2)
            ip2nrm=i
          endif

          if(abs(pdif).gt.pdfnrm)then
            pdfnrm=abs(pdif)
            ipdfnrm=i
          endif

          npp=npp+1

        endif

      enddo
c
      write(*,*)' '
      write(*,*)'LLINFDIF:'
      write(*,*)' '
      write(*,*)'Number of U and V nodes Np = ',np
      write(*,*)'Number of P nodes Npp = ',npp
      write(*,*)' '
      write(*,*)'linf(U1-U2)  = ',udfnrm,iudfnrm
      write(*,*)'linf(V1-V2)  = ',vdfnrm,ivdfnrm
      write(*,*)'linf(P1-P2)  = ',pdfnrm,ipdfnrm
      write(*,*)' '
      write(*,*)'linf(U1)     = ',u1nrm,iu1nrm
      write(*,*)'linf(V1)     = ',v1nrm,iv1nrm
      write(*,*)'linf(P1)     = ',p1nrm,ip1nrm
      write(*,*)' '
      write(*,*)'linf(U2)     = ',u2nrm,iu2nrm
      write(*,*)'linf(V2)     = ',v2nrm,iv2nrm
      write(*,*)'linf(P2)     = ',p2nrm,ip2nrm

      return
      end
      subroutine plval(nvec,xval,xvec,yval,yvec)
c
c***********************************************************************
c
c  PLVAL evaluates a piecewise linear function at a given point.
c
c  Note that if XVAL falls to the left of XVEC(1), then YVAL=YVEC(1),
c  and similarly, if XVAL is greater than XVEC(NVEC), YVAL=YVEC(NVEC).
c
c
c  NVEC   Input, integer NVEC, the number of abscissas and coefficients
c         that define the piecewise linear.  NVEC must be at least 1.
c
c  XVAL   Input, double precision XVAL, the point at which the function
c         is to be evaluated.
c
c  XVEC   Input, double precision XVEC(NVEC), the abscissas of the
c         function.  These should be distinct and in ascending order.
c
c  YVAL   Output, double precision YVAL, the value of the piecewise
c         linear function at the point XVAL.
c
c  YVEC   Input, double precision YVEC(NVEC), the value of the piecewise
c         function at each of the abscissas.
c
      implicit double precision (a-h,o-z)
c
      integer nvec
c
      integer i
      integer ival
      double precision xval
      double precision xvec(nvec)
      double precision yval
      double precision yvec(nvec)
c
c  Step 1: Check if XVAL lies outside the intervals.
c
      if(xval.le.xvec(1))then
        yval=yvec(1)
        return
      elseif(xval.ge.xvec(nvec))then
        yval=yvec(nvec)
        return
      endif
c
c  Step 2: Find index I so that XVEC(I) <= XVAL < XVEC(I+1)
c
      do i=1,nvec-1
 
        if(xvec(i).le.xval.and.xval.le.xvec(i+1))then
          ival=i
          go to 10
        endif
 
      enddo
 
      write(*,*)' '
      write(*,*)'PLVal - Fatal error!'
      write(*,*)'  Could not bracket XVAL=',xval
      stop
 
10    continue
c
c  Step 3: Evaluate the linear function at XVAL.
c
      i=ival
 
      if(xval.eq.xvec(i+1))then
        yval=yvec(i+1)
      elseif(xval.eq.xvec(i))then
        yval=yvec(i)
      else
        yval=( yvec(i)*(xvec(i+1)-xval)
     &      +yvec(i+1)*(xval-xvec(i)) ) / (xvec(i+1)-xvec(i))
      endif
 
      return
      end
      subroutine ppvalu(break,coef,l,k,x,jderiv,value)
c
c***********************************************************************
c
c  PPVALU calculates the value at X of the JDERIV-th derivative of
c  the piecewise polynomial function F from its piecewise
c  polynomial representation.
c
c  The interval index I, appropriate for X, is found through a
c  call to INTERV.  The formula above for the JDERIV-th derivative
c  of F is then evaluated by nested multiplication.
c
c  The J-th derivative of F is given by:
c
c    (d**j)f(x) = coef(j+1,i)
c               + h*(coef(j+2,i)
c               + h*(...(coef(k-1,i) +
c               + h*coef(k,i)/(k-j-1))/(k-j-2) ... )/2)/1
c
c  with
c
c    H = X - BREAK(I)
c
c  and
c
c    i  =  max( 1 , max( j ,  break(j) .le. x , 1 .le. j .le. l ) ).
c
c
c  BREAK,
c  COEF,
c  L      Input, double precision BREAK(L+1), REAL COEF(*), integer L, for
c         piecewise polynomial representation of the function F to
c         be evaluated.
c
c  K      Input, integer K, the order of the polynomial pieces
c         that make up the function F.  The most usual value for
c         K is 4, signifying a piecewise cubic polynomial.
c
c  X      Input, double precision X, the point at which to evaluate F or
c         of its derivatives.
c
c  JDERIV Input, integer JDERIV, the order of the derivative to be
c         evaluated.  If JDERIV is 0, then F itself is evaluated,
c         which is actually the most common case.  It is assumed
c         that JDERIV is zero or positive.
c
c  VALUE  Output, double precision VALUE, the value of the JDERIV-th
c         derivative of F at X.
c
      implicit double precision(a-h,o-z)
c
      integer k
      integer l
c
      double precision break(l)
      double precision coef(k,l)
      double precision fmmjdr
      double precision h
      integer i
      integer jderiv
      integer m
      integer ndummy
      double precision value
      double precision x
c
      value=0.0
 
      fmmjdr=k-jderiv
c
c  Derivatives of order K or higher are identically zero.
c
      if(k.le.jderiv)return
c
c  Find the index I of the largest breakpoint to the left of X.
c
      call interv(break,l+1,x,i,ndummy)
c
c  Evaluate the JDERIV-th derivative of the I-th polynomial piece
c  at X.
c
      h=x-break(i)
      m=k
 
10    continue
 
      value=(value/fmmjdr)*h+coef(m,i)
      m=m-1
      fmmjdr=fmmjdr-1
      if(fmmjdr.gt.0.0)go to 10
 
      return
      end
      subroutine pqval(nvec,xval,xvec,yval,yvec)
c
c***********************************************************************
c
c  PQVAL evaluates a piecewise quadratic function at a given point.
c
c
c  The piecewise quadratic is defined by NVEC values, where NVEC 
c  is odd, and at least 3.  The function is defined by specifying
c  a list of nodes XVEC(I), and specifying its value YVEC(I) at each 
c  node.  
c
c  The function will be a quadratic polynomial over each of 
c  (NVEC-1)/2 intervals that are made up a set of three consecutive
c  nodes, with the first one odd.  Thus, XVEC(1), XVEC(2) and XVEC(3)
c  lie in the first interval.
c
c  At the odd nodes, the quadratic that defines the function may
c  change, but the function remains continuous there, though not
c  differentiable.
c
c  Note that if XVAL falls to the left of XVEC(1), then YVAL=YVEC(1),
c  and similarly, if XVAL is greater than XVEC(NVEC), YVAL=YVEC(NVEC).
c
c  NVEC   Input, integer NVEC, the number of abscissas and coefficients
c         that define the piecewise quadratic.  
c
c         NVEC must be odd, and at least 3.
c
c  XVAL   Input, double precision XVAL, the point at which the function
c         is be evaluated.
c
c  XVEC   Input, double precision XVEC(NVEC), the abscissas of the
c         function.  These should be distinct and in ascending order.
c
c  YVAL   Output, double precision YVAL, the value of the piecewise
c         quadratic function at the point XVAL.
c
c  YVEC   Input, double precision YVEC(NVEC), the value of the
c         piecewise quadratic function at each of the abscissas.
c
      implicit double precision (a-h,o-z)
c
      integer nvec
c
      integer i
      integer ival
      double precision xval
      double precision xvec(nvec)
      double precision yval
      double precision yvec(nvec)
c
c  Step 0: Check data.
c
      if(nvec.lt.3)then
        write(*,*)' '
        write(*,*)'PQVal - Fatal error!'
        write(*,*)'  Value of NVEC=',nvec
        write(*,*)'  but NVEC must be at least 3.'
        stop
      endif
 
      if(mod(nvec,2).ne.1)then
        write(*,*)' '
        write(*,*)'PQVal - Fatal error!'
        write(*,*)'  Even value of NVEC = ',nvec
        stop
      endif
c
c  Step 1: Find odd index I so that XVEC(I) <= XVAL < XVEC(I+2)
c
      if(xval.le.xvec(1))then
        yval=yvec(1)
        return
      elseif(xval.ge.xvec(nvec))then
        yval=yvec(nvec)
        return
      endif
 
      do i=1,nvec-2,2
 
        if(xvec(i).le.xval.and.xval.le.xvec(i+2))then
          ival=i
          go to 10
        endif
 
      enddo
 
      write(*,*)' '
      write(*,*)'PQVal - Fatal error!'
      write(*,*)'  Could not bracket XVAL=',xval
      write(*,*)'  There are ',nvec,' nodes.'
      write(*,*)'  First node is at ',xvec(1)
      write(*,*)'  Last node is at  ',xvec(nvec)
 
      do i=1,nvec
        write(*,*)xvec(i)
      enddo
      stop
 
10    continue
c
c  Step 2: Evaluate the quadratic function at XVAL.
c
      i=ival
 
      yval=yvec(i)*(xval-xvec(i+1)) * (xval-xvec(i+2))
     &    /((xvec(i)-xvec(i+1))*(xvec(i)-xvec(i+2)))
     &    +yvec(i+1)*(xval-xvec(i)) * (xval-xvec(i+2))
     &    /((xvec(i+1)-xvec(i))*(xvec(i+1)-xvec(i+2)))
     &    +yvec(i+2)*(xval-xvec(i)) * (xval-xvec(i+1))
     &    /((xvec(i+2)-xvec(i))*(xvec(i+2)-xvec(i+1)))
 
      return
      end
      subroutine qbf0(ielem,in,w,nelem,node,np,xc,xq,yc,yq)
c
c***********************************************************************
c
c  QBF0 evaluates a particular quadratic basis function at a point
c  in a nonisoparametric element.
c
c      ^
c      |        2
c      |       /|
c   Y  |      4 5
c      |     /  |
c      |    1-6-3
c      |
c      +------------>
c             X
c
c
c  IELEM  Input, integer IELEM, the number of the element we are
c         examining.  This will be a value between 1 and NELEM.
c
c  IN     Input, integer IN, the number of the basis function we
c         want.  This will be a value between 1 and 6.  Functions
c         1 through 3 are associated with corners, 4 though 6
c         with sides.
c
c  W      Output, double precision W, the value of the
c         IN-th basis function at the given point.
c
c  NELEM  Input, integer NELEM, the number of elements.
c
c  NODE   Input, integer NODE(6,MAXELM), contains the numbers
c         of the nodes that make up each element.  Element number
c         I is associated with nodes NODE(1,I) through NODE(6,I).
c
c  NP     Input, integer NP, the number of nodes.
c
c  XC     Input, double precision XC(NP), the X coordinates of the
c         nodes.
c
c  XQ     Input, double precision XQ, the X coordinate of the point
c         where the basis function is to be evaluated.
c
c  YC     Input, double precision YC(NP), the Y coordinates of the nodes
c
c  YQ     Input, double precision YQ, the Y coordinate of the point wher
c         the basis function is to be evaluated.
c
      implicit double precision(a-h,o-z)
c
      integer nelem
      integer np
c
      double precision c
      double precision d
      integer i1
      integer i2
      integer i3
      integer ielem
      integer in
      integer in1
      integer in2
      integer in3
      integer node(6,nelem)
      double precision s
      double precision t
      double precision w
      double precision xc(np)
      double precision xq
      double precision yc(np)
      double precision yq
c
c  Case 1: We are inquiring about a basis function associated
c  with a corner.
c
c  Notice that the basis function W is zero exactly if
c  T is 0 or T is 1/2.
c
c  IN1, IN2, and IN3 are the local node numbers of the three
c  corner nodes, and I1, I2 and I3 are the corresponding
c  global node numbers, which are used to look up the X and
c  Y coordinates of the nodes.
c
      if(1.le.in.and.in.le.3)then
 
        in1=in
        in2=mod(in,3)+1
        in3=mod(in+1,3)+1
 
        i1=node(in1,ielem)
        i2=node(in2,ielem)
        i3=node(in3,ielem)
 
        d=(xc(i2)-xc(i1))*(yc(i3)-yc(i1))
     &   -(xc(i3)-xc(i1))*(yc(i2)-yc(i1))
 
        t=1.0+( (xq    -xc(i1))*(yc(i2)-yc(i3))
     &       +(xc(i3)-xc(i2))*(yq    -yc(i1)) )/d
 
        w=t*(2.0*t-1.0)
c
c  Case 2: We are inquiring about a basis function associated
c  with a midpoint.
c
      elseif(in.ge.4.and.in.le.6)then
 
        in1=in-3
        in2=mod(in-3,3)+1
        in3=mod(in-2,3)+1
 
        i1=node(in1,ielem)
        i2=node(in2,ielem)
        i3=node(in3,ielem)
 
        d=    (xc(i2)-xc(i1))*(yc(i3)-yc(i1))
     &       -(xc(i3)-xc(i1))*(yc(i2)-yc(i1))
 
        c=    (xc(i3)-xc(i2))*(yc(i1)-yc(i2))
     &       -(xc(i1)-xc(i2))*(yc(i3)-yc(i2))
 
        t=1.0+( (xq    -xc(i1))*(yc(i2)-yc(i3))
     &    +(xc(i3)-xc(i2))*(yq    -yc(i1)) )/d
 
        s=1.0+( (xq    -xc(i2))*(yc(i3)-yc(i1))
     &    +(xc(i1)-xc(i3))*(yq    -yc(i2)) )/c
 
        w=4.0 * s*t
 
      else
 
        write(*,*)' '
        write(*,*)'QBF0 - Fatal error!'
        write(*,*)'  Request for basis function IN=',in
        write(*,*)'  but IN must be between 1 and 6.'
        stop
 
      endif
 
      return
      end
      subroutine reads(ibs,nelem,neqn,np,nparb,nquad,nx,ny,xbl,xbr)
c
c***********************************************************************
c
      integer ibs
      integer nelem
      integer neqn
      integer np
      integer nparb
      integer nquad
      integer nx
      integer ny
      character*30 string
      double precision xbl
      double precision xbr
c
      read(17,'(a)',err=10)string
      read(17,*,err=10)ibs
      read(17,'(a)',err=10)string
      read(17,*,err=10)nelem
      read(17,'(a)',err=10)string
      read(17,*,err=10)neqn
      read(17,'(a)',err=10)string
      read(17,*,err=10)np
      read(17,'(a)',err=10)string
      read(17,*,err=10)nparb
      read(17,'(a)',err=10)string
      read(17,*,err=10)nquad
      read(17,'(a)',err=10)string
      read(17,*,err=10)nx
      read(17,'(a)',err=10)string
      read(17,*,err=10)ny
      read(17,'(a)',err=10)string
      read(17,*,err=10)xbl
      read(17,'(a)',err=10)string
      read(17,*,err=10)xbr

      return
10    continue
      write(*,*)' '
      write(*,*)'READS - Error while reading '//string
      stop
      end
      subroutine readv(area,g,indx,isotri,nelem,neqn,node,np,
     &  nparb,nquad,splbmp,taubmp,xc,yc)
c
c***********************************************************************
c
      integer nelem
      integer neqn
      integer np
      integer nparb
      integer nquad
c
      double precision area(nquad,nelem)
      double precision g(neqn)
      integer i
      integer indx(3,np)
      integer isotri(nelem)
      integer j
      integer k
      integer node(6,nelem)
      double precision splbmp(4,nparb+2,0:nparb)
      character*30 string
      double precision taubmp(nparb+2)
      double precision xc(np)
      double precision yc(np)
c
      read(17,'(a)',err=10)string
      do i=1,nquad
        do j=1,nelem
          read(17,*,err=10)area(i,j)
        enddo
      enddo

      read(17,'(a)',err=10)string
      do i=1,neqn
        read(17,*,err=10)g(i)
      enddo

      read(17,'(a)',err=10)string
      do i=1,3
        do j=1,np
          read(17,*,err=10)indx(i,j)
        enddo
      enddo

      read(17,'(a)',err=10)string
      do i=1,nelem
        read(17,*,err=10)isotri(i)
      enddo

      read(17,'(a)',err=10)string
      do i=1,6
        do j=1,nelem
          read(17,*,err=10)node(i,j)
        enddo
      enddo

      read(17,'(a)',err=10)string
      do i=1,4
        do j=1,nparb+2
          do k=0,nparb
            read(17,*,err=10)splbmp(i,j,k)
          enddo
        enddo
      enddo

      read(17,'(a)',err=10)string
      do i=1,nparb+2
        read(17,*,err=10)taubmp(i)
      enddo

      read(17,'(a)',err=10)string
      do i=1,np
        read(17,*,err=10)xc(i)
      enddo

      read(17,'(a)',err=10)string
      do i=1,np
        read(17,*,err=10)yc(i)
      enddo

      return
10    continue
      write(*,*)' '
      write(*,*)'Error while reading '//string
      stop
      end
      subroutine refbsp0(q,iq,eta,xsi)
c
c***********************************************************************
c
c  REFBSP0 evaluates one of the three linear basis functions at a 
c  particular point (X,Y) in a particular element, by referring to 
c  the corresponding points (XSI,ETA) in the reference triangle.
c
c  Here is a graph of the (XSI, ETA) reference triangle we will
c  use.
c
c        ^
c        |
c      1 +        2
c        |       /|
c  ETA   |      / |
c        |     /  |
c      0 +    1---3
c        |
c        +----+---+--->
c             0   1
c
c              XSI
c
c
c  Q      Output, double precision Q, the value of the basis
c         function at the point (ETA,XSI).
c
c  IQ     Input, integer IQ, the local node number, between 1 and
c         3, whose basis function is being evaluated.
c
c  ETA,
c  XSI    Input, double precision ETA, XSI, the local coordinates of the
c         at which the basis information is desired.
c
      implicit double precision(a-h,o-z)
c
      double precision eta
      integer iq
      double precision q
      double precision xsi
c
      if(iq.eq.1)then
        q=1.0-xsi
      elseif(iq.eq.2)then
        q=eta
      elseif(iq.eq.3)then
        q=xsi-eta
      elseif(iq.ge.4.and.iq.le.6)then
        q=0.0
      else
        write(*,*)'RefBSP0 - Fatal error!'
        write(*,*)'  Request for basis function IQ=',iq
        write(*,*)'  but IQ must be between 1 and 6.'
        stop
      endif
 
      return
      end
      subroutine refqbf0(w,eta,iq,xsi)
c
c***********************************************************************
c
c  REFQBF0 evaluates one of the six quadratic basis functions
c  at a particular point in a particular element, by referring
c  to the reference triangle.
c
c  The point we are interested in is referred to by its coordinates
c  in the reference triangle.  That is, we are given coordinates
c  (XSI, ETA), even though, physically, we are interested
c  in points in (X, Y) space.
c
c    d F(X,Y)/dX     (d XSI/dX  d ETA/dX )   ( d F(XSI, ETA)/d XSI )
c    d F(X,Y)/dY  =  (d XSI/dY  d ETA/dY ) * ( d F(XSI, ETA)/d ETA )
c
c  Here is a graph of the (XSI, ETA) reference triangle we will
c  use.
c
c        ^
c        |
c      1 +        2
c        |       /|
c  ETA   |      4 5
c        |     /  |
c      0 +    1-6-3
c        |
c        +----+---+--->
c             0   1
c
c              XSI
c
c  W      Output, double precision W,the value of the basis
c         function at (XSI,ETA).
c
c  ETA    Input, double precision ETA, the ETA coordinate of the point.
c
c  IQ     Input, integer IQ, the local node number, between 1 and
c         6, whose basis function is being evaluated.
c
c  XSI    Input, double precision XSI, the XSI coordinate of the point.
c
      implicit double precision (a-h,o-z)
c
      double precision eta
      integer iq
      double precision w
      double precision xsi
c
c  Evaluate W, the quadratic basis function.
c
c  Basis 1 is zero if XSI=0.5 or XSI=1.
c
      if(iq.eq.1)then
        w=(2.0*xsi-1.0) * (xsi-1.0)
c
c  Basis 2 is zero if ETA=0 or ETA=0.5.
c
      elseif(iq.eq.2)then
        w=eta * (2.0*eta-1.0)
c
c  Basis 3 is zero if XSI=ETA, or XSI=ETA+0.5
c
      elseif(iq.eq.3)then
        w=(xsi-eta) * (2.0*xsi-2.0*eta-1.0)
c
c  Basis 4 is zero if ETA=0 or XSI=1.
c
      elseif(iq.eq.4)then
        w=4.0 * eta * (1.0-xsi)
c
c  Basis 5 is zero if ETA=0 or XSI=ETA.
c
      elseif(iq.eq.5)then
        w=4.0 * eta * (xsi-eta)
c
c  Basis 6 is zero if XSI=ETA or XSI=1.
c
      elseif(iq.eq.6)then
        w=4.0 * (xsi-eta) * (1.0-xsi)
c
c  Stop if we were given an unexpected value of IQ.
c
      else
        write(*,*)' '
        write(*,*)'RefQBF0 - Fatal error!'
        write(*,*)'  A basis function index must be between 1 and 6,'
        write(*,*)'  but you input the value IQ=',iq
        stop
      endif

      return
      end
      subroutine xofxsi(eta,ielem,nelem,node,np,x,xc,xsi,y,yc)
c
c***********************************************************************
c
c  XOFXSI is given the XSI, ETA coordinates of a point in an
c  isoparametric element and determines its X, Y coordinates.
c
c  Here is a graph of the (XSI, ETA) reference triangle we will use.
c
c        ^
c        |
c      1 +        2
c        |       /|
c  ETA   |      4 5
c        |     /  |
c      0 +    1-6-3
c        |
c        +----+---+--->
c             0   1
c
c            XSI
c
c
c  ETA    Input, double precision ETA, the ETA coordinate of the point.
c
c  IELEM  Input, integer IELEM, the number of the isoparametric
c         element we are examining.
c
c  MAXELM Input, integer MAXELM, the maximum number of elements.
c
c  NODE   Input, integer NODE(6,MAXELM), contains the numbers
c         of the nodes that make up each element.  
c
c  NP     Input, integer NP, the number of nodes.
c
c  X      Output, double precision X, the X coordinate of the point.
c
c  XC     Input, double precision XC(NP), the X coordinates of the 
c         nodes.
c
c  XSI    Input, double precision XSI, the XSI coordinate of the point.
c
c  Y      Output, double precision Y, the Y coordinate of the point.
c
c  YC     Input, double precision YC(NP), the Y coordinates of the 
c         nodes.
c
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer np
c
      double precision a1
      double precision a2
      double precision b1
      double precision b2
      double precision c1
      double precision c2
      double precision d1
      double precision d2
      double precision e1
      double precision e2
      double precision eta
      double precision f1
      double precision f2
      integer i
      integer ielem
      integer node(6,nelem)
      double precision x
      double precision xn(6)
      double precision xc(np)
      double precision xsi
      double precision y
      double precision yn(6)
      double precision yc(np)
c
c  Pick off the X, Y coordinates of the nodes and store them
c  in two short lists.
c
      do i=1,6
        xn(i)=xc(node(i,ielem))
        yn(i)=yc(node(i,ielem))
      enddo
c
c  Set the coefficients in the transformation
c
c    (XSI,ETA) --> (X,Y).
c
c  The mapping has the form:
c
c    X(ETA,XSI) = A1 * XSI**2 + B1 * XSI*ETA + C1 * ETA**2
c               + D1 * XSI    + E1 * ETA     + F1
c
c    Y(ETA,XSI) = A2 * XSI**2 + B2 * XSI*ETA + C2 * ETA**2
c               + D2 * XSI    + E2 * ETA     + F2
c
      a1= 2.0*xn(1)+2.0*xn(3)-4.0*xn(6)
      b1=-4.0*xn(3)-4.0*xn(4)+4.0*xn(5)+4.0*xn(6)
      c1= 2.0*xn(2)+2.0*xn(3)-4.0*xn(5)
      d1=-3.0*xn(1)    -xn(3)+4.0*xn(6)
      e1=    -xn(2)    +xn(3)+4.0*xn(4)-4.0*xn(6)
      f1=     xn(1)
 
      a2= 2.0*yn(1)+2.0*yn(3)-4.0*yn(6)
      b2=-4.0*yn(3)-4.0*yn(4)+4.0*yn(5)+4.0*yn(6)
      c2= 2.0*yn(2)+2.0*yn(3)-4.0*yn(5)
      d2=-3.0*yn(1)    -yn(3)+4.0*yn(6)
      e2=    -yn(2)    +yn(3)+4.0*yn(4)-4.0*yn(6)
      f2=     yn(1)
 
      x=a1*xsi**2 + b1*xsi*eta + c1*eta**2 + d1*xsi + e1*eta + f1
 
      y=a2*xsi**2 + b2*xsi*eta + c2*eta**2 + d2*xsi + e2*eta + f2
 
      return
      end
      subroutine xsiofx(eta,ielem,nelem,node,np,xc,xsi,xval,yc,yval)
c
c***********************************************************************
c
c  XSIOFX is given the X and Y coordinates of an arbitrary point
c  in a given isoparametric element IELEM and computes the 
c  corresponding XSI, ETA coordinates of that point.
c
c  Newton iteration is used to solve the nonlinear equations that
c  determine the relationship between (X,Y) and (XSI,ETA).
c
c
c  The point (XSI,ETA) is within the master element if and
c  only if
c
c    0 <= ETA <= XSI <= 1
c
c  The point (XVAL,YVAL) is within the image of the master element
c  if and only if (XSI,ETA) is within the master element.
c
c  In general, it is a fairly difficult task to determine which
c  element IELEM contains an arbitrary point (X,Y).
c
c
c  The computation of (XSI,ETA) begins by considering the linear
c  mapping that sends the corners of the reference element to
c  the corners of the image element:
c
c    X = (1-XSI) X(1) + ETA X(2) + (XSI-ETA) X(3)
c    Y = (1-XSI) Y(1) + ETA Y(2) + (XSI-ETA) Y(3)
c
c  This mapping can be written as:
c
c    X - X(1)   ( (X(3)-X(1))  (X(2)-X(3)) )    XSI
c             =                               *
c    Y - Y(1)   ( (Y(3)-Y(1))  (Y(2)-Y(3)) )    ETA
c
c  And then inverted as:
c
c    XSI           ( Y(2)-Y(3)  X(3)-X(2) )    (X - X(1))
c         = 1/DET                            * 
c    ETA           ( Y(1)-Y(3)  X(3)-X(1) )    (Y - Y(1))
c
c  where
c
c    DET = (X(3)-X(1)*(Y(2)-Y(3)) - (X(2)-X(3))*(Y(3)-Y(1))
c
c
c  Thus, given a point (X,Y), we can use the inverted mapping
c  to give us a linear estimate of the corresponding (XSI,ETA)
c  to use in beginning our Newton iteration.
c
c
c  ETA    Output, double precision ETA, the value of ETA that
c         produces the image point (XVAL,YVAL).
c
c  IELEM  Input, integer IELEM, the element which contains the
c         point (X,Y).
c
c  NELEM  Input, integer NELEM, the number of elements.
c
c  NODE   Input, integer NODE(6,NELEM), contains, for each element
c         the indices of the 6 nodes that form it.
c
c  NP     Input, integer NP, the number of nodes.
c
c  XC     Input, double precision XC(NP), the X coordinates of the
c         nodes.
c
c  XSI    Output, double precision, the value of XSI that
c         produces the image point (XVAL,YVAL).
c
c  XVAL   Input, double precision XVAL, the X value of the point
c         whose (XSI,ETA) coordinates are desired.
c
c  YC     Input, double precision YC(NP), the Y coordinates of the
c         nodes.
c
c  YVAL   Input, double precision YVAL, the Y value of the point
c         whose (XSI,ETA) coordinates are desired.
c
      implicit double precision (a-h,o-z)
c
      double precision tolnew
      double precision toltri
c
      parameter (tolnew=1.0d-06)
      parameter (toltri=1.0d-05)
c
      integer maxstp
      parameter (maxstp=20)
c
      integer nelem
      integer np
c
      double precision a1
      double precision a2
      double precision b1
      double precision b2
      double precision c1
      double precision c2
      double precision d1
      double precision d2
      double precision det
      double precision drdeta
      double precision drdxsi
      double precision dsdeta
      double precision dsdxsi
      double precision e1
      double precision e2
      double precision error
      double precision eta
      double precision f1
      double precision f2
      integer i
      integer ielem
      integer istep
      integer j
      integer node(6,nelem)
      double precision r
      double precision s
      double precision temp
      double precision x(6)
      double precision xc(np)
      double precision ximage
      double precision xsi
      double precision xval
      double precision y(6)
      double precision yc(np)
      double precision yimage
      double precision yval
c
c  Pick off the X, Y coordinates of the nodes of the element,
c  and store them in two short lists.
c
      do i=1,6
        x(i)=xc(node(i,ielem))
        y(i)=yc(node(i,ielem))
      enddo
c
c  Compute a starting estimate for XSI and ETA by considering the
c  much simpler linear mapping derived from the three node triangle 
c  based on the corner nodes X(1), X(2), X(3).
c
      det=(x(3)-x(1))*(y(2)-y(3))-(x(2)-x(3))*(y(3)-y(1))

      xsi=( (y(2)-y(3))*(xval-x(1))+(x(3)-x(2))*(yval-y(1)) ) / det
      eta=( (y(1)-y(3))*(xval-x(1))+(x(3)-x(1))*(yval-y(1)) ) / det
c
c  We will NOT require the initial XSI and ETA to lie within the
c  reference triangle, since the quadratic element may be significantly
c  distorted.
c
c  Set the coefficients in the transformation
c
c    (XSI,ETA) --> (X,Y).
c
c  The mapping has the form:
c
c    X(XSI,ETA) = A1 * XSI**2 + B1 * XSI*ETA + C1 * ETA**2
c               + D1 * XSI    + E1 * ETA     + F1
c
c    Y(XSI,ETA) = A2 * XSI**2 + B2 * XSI*ETA + C2 * ETA**2
c               + D2 * XSI    + E2 * ETA     + F2
c
      a1= 2.0*x(1)+2.0*x(3)-4.0*x(6)
      b1=-4.0*x(3)-4.0*x(4)+4.0*x(5)+4.0*x(6)
      c1= 2.0*x(2)+2.0*x(3)-4.0*x(5)
      d1=-3.0*x(1)-    x(3)+4.0*x(6)
      e1=-    x(2)+    x(3)+4.0*x(4)-4.0*x(6)
      f1=     x(1)
 
      a2= 2.0*y(1)+2.0*y(3)-4.0*y(6)
      b2=-4.0*y(3)-4.0*y(4)+4.0*y(5)+4.0*y(6)
      c2= 2.0*y(2)+2.0*y(3)-4.0*y(5)
      d2=-3.0*y(1)-    y(3)+4.0*y(6)
      e2=-    y(2)+    y(3)+4.0*y(4)-4.0*y(6)
      f2=     y(1)
c
c  Evaluate the residuals, X(XSI,ETA)-XVAL, and Y(XSI,ETA)-YVAL.
c
      istep=0
 
10    continue
 
      ximage=a1*xsi*xsi+b1*xsi*eta+c1*eta*eta+d1*xsi+e1*eta+f1
      yimage=a2*xsi*xsi+b2*xsi*eta+c2*eta*eta+d2*xsi+e2*eta+f2

      r=ximage-xval
      s=yimage-yval
c
c  Compute the maximum error.
c
      error=max(abs(r),abs(s))
 
      if(error.lt.tolnew)then

        if(xsi.gt.1.0)then

          if(xsi-1.0.le.toltri)then
            xsi=1.0
          else
            write(*,*)' '
            write(*,*)'Bad xsi=',xsi
            write(*,*)'    eta=',eta

            write(*,*)'Element is ',ielem
            write(*,*)'XVAL,YVAL=',xval,yval
            write(*,*)' '
            write(*,*)'XC,YC:'
            write(*,*)' '
            do i=1,6
              j=node(i,ielem)
              write(*,*)xc(j),yc(j)
            enddo

            stop

          endif

        endif

        if(eta.lt.0.0)then
          if(eta.ge.-toltri)then
            eta=0.0
          else
            write(*,*)' '
            write(*,*)'Bad eta=',eta
            write(*,*)'    XSI=',xsi

            write(*,*)'Element is ',ielem
            write(*,*)'XVAL,YVAL=',xval,yval
            write(*,*)' '
            write(*,*)'XC,YC:'
            write(*,*)' '
            do i=1,6
              j=node(i,ielem)
              write(*,*)xc(j),yc(j)
            enddo
 
            stop
          endif

        endif

        if(eta.gt.xsi)then
          if(eta-xsi.le.toltri)then
            temp=(eta+xsi)/2.0
            eta=temp
            xsi=temp
          else
            write(*,*)' '
            write(*,*)'Bad xsi=',xsi
            write(*,*)'Bad eta=',eta

            write(*,*)'Element is ',ielem
            write(*,*)'XVAL,YVAL=',xval,yval
            write(*,*)' '
            write(*,*)'XC,YC:'
            write(*,*)' '
            do i=1,6
              j=node(i,ielem)
              write(*,*)xc(j),yc(j)
            enddo

            stop

          endif

        endif

        return
      endif
 
      istep=istep+1
 
      if(istep.gt.maxstp)then
        write(*,*)' '
        write(*,*)'XsiOfX - Fatal error!'
        write(*,*)'  The Newton iteration is not converging.'
        write(*,*)'  The iteration has taken ',istep,' steps.'
        write(*,*)' '
        write(*,*)'  Xval, Yval=',xval,yval
        write(*,*)'  Current Xsi, Eta=',xsi,eta
        write(*,*)'  X(xsi,eta), Y(xsi,eta)=',ximage,yimage
        write(*,*)'  Xval, Yval is in element ',ielem
        write(*,*)'  whose nodes are:'
        do i=1,6
          write(*,*)i,node(i,ielem),xc(node(i,ielem)),yc(node(i,ielem))
        enddo
        stop
      endif
c
c  For the next Newton step, compute the partial derivatives
c  of the residual with respect to XSI and ETA.
c
      drdxsi=2.0*a1*xsi+b1*eta+d1
      drdeta=b1*xsi+2.0*c1*eta+e1
      dsdxsi=2.0*a2*xsi+b2*eta+d2
      dsdeta=b2*xsi+2.0*c2*eta+e2
c
c  Compute the determinant of the jacobian.
c
      det=drdxsi*dsdeta-dsdxsi*drdeta
 
      if(abs(det).le.1.0e-10)then
        write(*,*)'XsiOfX - Fatal Error!'
        write(*,*)'  The Newton Jacobian is singular.'
        write(*,*)'  The determinant is ',det
        write(*,*)'  Xval, Yval=',xval,yval
        write(*,*)'  Current Xsi, Eta=',xsi,eta
        write(*,*)'  X(xsi,eta), Y(xsi,eta)=',ximage,yimage
        write(*,*)'  dRdXsi, dRdEta=',drdxsi,drdeta
        write(*,*)'  dSdXsi, dSdEta=',dsdxsi,dsdeta
        stop
      endif
c
c  Perform the Newton update.
c
      xsi=xsi+(drdeta*s-dsdeta*r)/det
      eta=eta+(dsdxsi*r-drdxsi*s)/det

      go to 10
      end
      subroutine bumpy(ielem,nelem,node,np,xc,xval,xsi,yc,yval)
c
c***********************************************************************
c
c  BUMPY is given the X coordinate of a point along the bump,
c  and the element in which that point lies.
c
      implicit double precision (a-h,o-z)
c
      double precision tol
      parameter (tol=1.0d-06)
c
      integer maxstp
      parameter (maxstp=20)
c
      integer nelem
      integer np
c
      double precision a1
      double precision a2
      double precision d1
      double precision d2
      double precision det
      double precision drdxsi
      double precision f1
      double precision f2
      integer i
      integer ielem
      integer istep
      integer node(6,nelem)
      double precision r
      double precision x(6)
      double precision xc(np)
      double precision ximage
      double precision xsi
      double precision xval
      double precision y(6)
      double precision yc(np)
      double precision yval
c
c  Pick off the X, Y coordinates of the nodes of the element,
c  and store them in two short lists.
c
      do i=1,6
        x(i)=xc(node(i,ielem))
        y(i)=yc(node(i,ielem))
      enddo
c
c  Make a linear estimate for YVAL.
c
      yval=y(1)+(y(3)-y(1))*(xval-x(1))/(x(3)-x(1))
c
c  Compute a starting estimate for XSI and ETA by considering the
c  much simpler linear mapping derived from the three node triangle 
c  based on the corner nodes X(1), X(2), X(3).
c
      det=(x(3)-x(1))*(y(2)-y(3))-(x(2)-x(3))*(y(3)-y(1))

      xsi=( (y(2)-y(3))*(xval-x(1))+(x(3)-x(2))*(yval-y(1)) ) / det
c
c  Force (XSI,ETA) to lie within the reference triangle.
c
      if(xsi.gt.1.0)then
        xsi=1.0
      elseif(xsi.lt.0.0)then
        xsi=0.0
      endif
c
c  Set the coefficients in the transformation
c
c    (XSI,0) --> (X,Y).
c
c  The mapping has the form:
c
c    X(XSI,0) = A1 * XSI**2 + D1 * XSI    + F1
c
c    Y(XSI,0) = A2 * XSI**2 + D2 * XSI    + F2
c
      a1= 2.0*x(1)+2.0*x(3)-4.0*x(6)
      d1=-3.0*x(1)-    x(3)+4.0*x(6)
      f1=     x(1)
c
c  Evaluate the residual, X(XSI,0)-XVAL.
c
      istep=0
 
10    continue
 
      ximage=a1*xsi*xsi+d1*xsi+f1

      r=ximage-xval
c
c  Compute the maximum error.
c
      if(abs(r).lt.tol)then

        if(xsi.gt.1.0)then
          write(*,*)'Bad xsi=',xsi,' reset to 1.0'
          xsi=1.0
        elseif(xsi.lt.0.0)then
          write(*,*)'Bad xsi=',xsi,' reset to 0.0'
          xsi=0.0
        endif

        a2= 2.0*y(1)+2.0*y(3)-4.0*y(6)
        d2=-3.0*y(1)-    y(3)+4.0*y(6)
        f2=     y(1)

        yval=a2*xsi*xsi+d2*xsi+f2

        return
      endif
 
      istep=istep+1
 
      if(istep.gt.maxstp)then
        write(*,*)' '
        write(*,*)'BumpY - Fatal error!'
        write(*,*)'  The Newton iteration is not converging.'
        write(*,*)'  The iteration has taken ',istep,' steps.'
        write(*,*)' '
        write(*,*)'  Xval, Yval=',xval,yval
        write(*,*)'  Current Xsi=',xsi
        write(*,*)'  X(xsi,0)=',ximage
        write(*,*)'  Xval, Yval is in element ',ielem
        write(*,*)'  whose nodes are:'
        do i=1,6
          write(*,*)i,node(i,ielem),xc(node(i,ielem)),yc(node(i,ielem))
        enddo
        stop
      endif
c
c  For the next Newton step, compute the partial derivatives
c  of the residual with respect to XSI and ETA.
c
      drdxsi=2.0*a1*xsi+d1

      if(abs(drdxsi).le.1.0e-10)then
        write(*,*)'BumpY - Fatal Error!'
        write(*,*)'  The derivative is too small.'
        write(*,*)'  Xval, Yval=',xval,yval
        write(*,*)'  Current Xsi=',xsi
        write(*,*)'  X(xsi,0)=',ximage
        write(*,*)'  dRdXsi=',drdxsi
        stop
      endif
c
c  Perform the Newton update.
c
      xsi=xsi-r/drdxsi
 
      go to 10
      end
