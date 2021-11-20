c  COMPARE.F  02 November 1995
c
      program main

c*********************************************************************72
c
cc MAIN is the main program for COMPARE.
c
c  Discussion:
c
c    COMPARE is a program which computes the norm of the difference
c    of two finite element functions, defined on a coarse and fine
c    mesh, respectively, over the same region.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
      integer maxnx
      integer maxny

      parameter (maxnx=81)
      parameter (maxny=25)

      integer maxelm
      integer maxeqn
      integer maxnp
      integer maxparb
      integer maxqud

      parameter (maxelm=2*(maxnx-1)*(maxny-1))
      parameter (maxeqn=2*(2*maxnx-1)*(2*maxny-1)+maxnx*maxny)
      parameter (maxnp=(2*maxnx-1)*(2*maxny-1))
      parameter (maxparb=5)
      parameter (maxqud=7)

      double precision area(maxqud,maxelm)
      double precision aream(maxqud,maxelm)
      character*40 filec
      character*40 filef
      double precision g(maxeqn)
      double precision gm(maxeqn)
      double precision gm2(maxeqn)
      integer ibs
      integer indx(3,maxnp)
      integer indxm(3,maxnp)
      integer isotri(maxelm)
      integer isotrim(maxelm)
      integer m
      integer nelem
      integer nelemm
      integer neqn
      integer neqnm
      integer node(6,maxelm)
      integer nodem(6,maxelm)
      integer np
      integer npm
      integer nparb
      integer nquad
      integer nquadm
      integer nx
      integer nxm
      integer ny
      integer nym
      double precision splbmp(4,maxparb+2,0:maxparb)
      double precision taubmp(maxparb+2)
      double precision xbl
      double precision xbr
      double precision xc(maxnp)
      double precision xcm(maxnp)
      double precision yc(maxnp)
      double precision ycm(maxnp)

      call timestamp ( )

      write(*,*)' '
      write(*,*)'COMPARE'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write(*,*)'  02 November 1995'
      write(*,*)' '
      write(*,*)'  Compare two flow solutions on different meshes.'
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
      call reads(ibs,nelemm,neqnm,npm,nparb,nquadm,nxm,nym,xbl,xbr)

      call readv(aream,gm,indxm,isotrim,nelemm,neqnm,nodem,npm,
     &  nparb,nquadm,splbmp,taubmp,xcm,ycm)

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
      call reads(ibs,nelem,neqn,np,nparb,nquad,nx,ny,xbl,xbr)

      call readv(area,g,indx,isotri,nelem,neqn,node,np,
     &  nparb,nquad,splbmp,taubmp,xc,yc)

      close(unit=17)
c
c  Compute M.
c
      m=(nxm-1)/(nx-1)
      write(*,*)'The refinement factor is M=',m
c
c  Extend the coarse mesh solution to the fine mesh.
c
      call extend(g,gm2,ibs,indx,indxm,isotri,m,nelem,neqn,neqnm,
     &  node,np,npm,nparb,nx,ny,splbmp,taubmp,xbl,xbr,xc,yc)
c
c  Compute the L2 difference on the fine mesh.
c
      call l2dif(aream,gm,gm2,indxm,isotrim,nelemm,neqnm,nodem,npm,
     &  nquadm,xcm,ycm)
c
c  Compute the little L2 difference on the fine mesh.
c
      call ll2dif(gm,gm2,indxm,neqnm,npm)
c
c  Compute the little L-infinity difference on the fine mesh.
c
      call llinfdif(gm,gm2,indxm,neqnm,npm)
c
c  Print the difference on the fine mesh.
c
c     call dif(gm,gm2,indxm,neqnm,npm)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COMPARE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine bsp0(q,ielem,iq,nelem,node,np,xc,xq,yc,yq)

c*********************************************************************72
c
cc BSP0 computes linear basis functions associated with pressure.
c
c  Discussion:
c
c    Here is a picture of a typical finite element associated with
c    pressure:
c
c        2
c       /|
c      / |
c     /  |
c    1---3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision Q, the value of the IQ-th basis
c    function at the point with global coordinates (XQ,YQ).
c
c    Input, integer IELEM, the global element number about which
c    we are inquiring.
c
c    Input, integer IQ, the index of the desired basis
c    function.  This is also the node of the reference
c    triangle which is associated with the basis function.
c    Basis function IQ is 1 at node IQ, and zero at the
c    other two nodes.
c
c    Input, integer NELEM, the number of elements.
c
c    Input, integer NODE(6,MAXELM).  NODE(J,I) is
c    the global node number of the J-th node in the I-th
c    element.
c
c    Input, integer NP, the number of nodes.
c
c    Input, double precision XC(NP), the global X coordinates
c    of the element nodes.
c
c    Input, double precision XQ, the global X coordinate of
c    the point in which we are interested.
c
c    Input, double precision YC(NP), the global Y coordinates
c    of the element nodes.
c
c    Input, double precision YQ, the global Y coordinate of
c    the point in which we are interested.
c
      implicit double precision(a-h,o-z)

      integer nelem
      integer np

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

      if(iq.lt.1.or.iq.gt.6)then
        write(*,*)' '
        write(*,*)'BSP0 - Fatal error!'
        write(*,*)'  The requested basis function is IQ=',iq
        write(*,*)'  but only values from 1 to 6 are legal.'
        stop
      elseif(iq.ge.4.and.iq.le.6)then
        q=0.0
        return
      end if
 
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
      subroutine dif(g1,g2,indx,neqn,np)

c*********************************************************************72
c
cc DIF prints the difference between two solutions at every node.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NEQN, the number of equations and unknowns.
c
c    Input, integer NP, the number of nodes.
c
      implicit double precision (a-h,o-z)

      integer neqn
      integer np

      double precision g1(neqn)
      double precision g2(neqn)
      integer i
      integer indx(3,np)
      integer j
      double precision p1
      double precision p2
      double precision pdif
      double precision u1
      double precision u2
      double precision udif
      double precision v1
      double precision v2
      double precision vdif

      do i=1,5
        
        j=indx(1,i)
        u1=g1(j)
        u2=g2(j)
        udif=abs(u1-u2)
        
        j=indx(2,i)
        v1=g1(j)
        v2=g2(j)
        vdif=abs(v1-v2)

        j=indx(3,i)
        if(j.gt.0)then
          p1=g1(j)
          p2=g2(j)
          pdif=abs(p1-p2)
        else 
          pdif=0.0
          p1=0.0
          p2=0.0
        end if

        write(*,*)' '
        write(*,*)'Node ',i
        write(*,'(a,3g14.6)')'u1,u2,udif',u1,u2,udif
        write(*,'(a,3g14.6)')'v1,v2,vdif',v1,v2,vdif
        if(indx(3,i).gt.0)then
          write(*,'(a,3g14.6)')'p1,p2,pdif',p1,p2,pdif
        end if

      end do

      return
      end
      subroutine extend(g,gm,ibs,indx,indxm,isotri,m,nelem,neqn,neqnm,
     &  node,np,npm,nparb,nx,ny,splbmp,taubmp,xbl,xbr,xc,yc)

c*********************************************************************72
c
cc EXTEND extends a coarse solution to a finer mesh.
c
c  Discussion:
c
c    M is the relative fineness of the finer mesh.  M is 2 if the
c    finer mesh has twice as many elements in both the X and Y directions.
c
c    EXTEND will only work when M is exactly an integer.  This is a
c    drawback I am trying to work around.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision G(NEQN), the solution computed on the
c    coarse mesh.
c
c  GM     Output, double precision GM(NEQNM), the solution as evaluated
c         on the finer mesh.
c
c  IBS    Input, integer IBS, boundary shape computation switch.
c
c  INDX   Input, integer INDX(3,NP).
c
c  INDXM  Input, integer INDXM(3,NPM).
c
c  ISOTRI Input, integer ISOTRI(NELEM).
c
c  M      Input, integer M, the relative fineness of the finer mesh.
c         M=1 means the same mesh is used.  M=2 means the finer mesh
c         is twice as fine, and so on.
c
c  NELEM  Input, integer NELEM, the number of elements in the coarse mesh.
c
c  NEQN   Input, integer NEQN, the number of equations and coefficients
c         associated with the coarse mesh.
c
c  NEQNM  Input, integer NEQNM, the number of equations and coefficients
c         associated with the finer mesh.
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
c  NPM    Input, integer NPM, the number of nodes on the finer mesh.
c
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c  NX     Input, integer NX.
c
c         NX controls the spacing of nodes and elements in
c         the X direction.  There are 2*NX-1 nodes along various
c         lines in the X direction.
c
c         Roughly speaking, NX (or 2*NX) is the number of elements along
c         a line in the X direction.
c 
c  NY     Input, integer NY.
c
c         NY controls the spacing of nodes and elements in
c         the Y direction.  There are 2*NY-1 nodes along various
c         lines in the Y direction.
c
c         Roughly speaking, NY (or 2*NY) is the number of elements along
c         a line in the Y direction.
c 
c  SPLBMP Input, double precision SPLBMP(4,NPARB+2,0:NPARB).
c
c         SPLBMP contains the spline coefficients for the bump
c         in SPLBMP(*,*,0).
c
c  TAUBMP Input, double precision TAUBMP(NPARB+2).
c
c         TAUBMP contains the location of the spline abscissas for
c         the bump.  There are NPARB+2 of them, because the end values
c         of the spline are constrained to have particular values.
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
c  YC     Input, double precision YC(NP).
c
c         The Y coordinates of the nodes.
c
      integer nelem
      integer neqn
      integer neqnm
      integer np
      integer npm
      integer nparb

      double precision eps
      double precision eta
      double precision g(neqn)
      double precision gm(neqnm)
      integer i
      integer ibs
      integer icol
      integer icol2
      integer icolm
      integer ielem
      integer ii
      integer iihi
      integer indx(3,np)
      integer indxm(3,npm)
      integer ip
      integer ipm
      integer irow
      integer irow2
      integer irowm
      integer isotri(nelem)
      integer j
      integer jcolm
      integer jderiv
      integer jj
      integer jjhi
      integer jjj
      integer jrowm
      integer m
      integer node(6,nelem)
      integer nx
      integer ny
      integer nym
      double precision p
      double precision splbmp(4,nparb+2,0:nparb)
      double precision taubmp(nparb+2)
      double precision u
      double precision v
      double precision x
      double precision xbl
      double precision xbr
      double precision xc(np)
      double precision xmax
      double precision xsi
      double precision y
      double precision yc(np)
      double precision ymax
      double precision ymin

      eps=0.000001
      xmax=10.0
      ymax=3.0

      icol=0
      icolm=0

      nym=m*(ny-1)+1

      do i=1,2*nx-1

        icol=icol+1

        if(i.lt.2*nx-1)then
          iihi=m
        else
          iihi=1
        end if

        do ii=1,iihi

          icolm=icolm+1
          irow=0
          irowm=0

          do j=1,2*ny-1

            irow=irow+1

            if(j.lt.2*ny-1)then
              jjhi=m
            else
              jjhi=1
            end if

            do jj=1,jjhi

              irowm=irowm+1

              x=dble(icolm-1)*xmax/dble(2*m*(nx-1))
              if(abs(x-xbl).le.eps)then
                x=xbl
              elseif(abs(x-xbr).le.eps)then
                x=xbr
              end if

              if(x.le.xbl)then
                ymin=0.0
              elseif(x.ge.xbl.and.x.le.xbr)then
                if(ibs.eq.1)then
                  call plval(nparb+2,x,taubmp,ymin,splbmp)
                elseif(ibs.eq.2)then
                  call pqval(nparb+2,x,taubmp,ymin,splbmp)
                elseif(ibs.eq.3)then
                  jderiv=0
                  call ppvalu(taubmp,splbmp(1,1,0),nparb+1,4,x,
     &              jderiv,ymin)
                end if
              else
                ymin=0.0
              end if

              y=((2*m*ny-1-irowm)*ymin+(irowm-1)*ymax)/dble(2*m*(ny-1))
c
c  Need to figure out the element, ETA and XSI.
c
              icol2=(icol+1)/2
              if(icol2.gt.nx-1)then
                icol2=nx-1
              end if

              irow2=(irow+1)/2
              if(irow2.gt.ny-1)then
                irow2=ny-1
              end if
c
c  Compute the number of the lower-numbered of the two elements
c  in the box.
c
              jrowm=mod(irowm-1,2*m)+1
              jcolm=mod(icolm-1,2*m)+1

              if(irow.eq.2*ny-1)then
                jrowm=2*m+1
              end if

              if(icol.eq.2*nx-1)then
                jcolm=2*m+1
              end if

              ielem=2*(icol2-1)*(ny-1)+2*(irow2-1)+1

              if(jcolm.ge.jrowm)then
                ielem=ielem+1
              end if
c
c  Compute the XSI and ETA locations of the sampling point.
c
              if(mod(ielem,2).eq.0)then
                xsi=dble(jcolm-1)/dble(2*m)
                eta=dble(jrowm-1)/dble(2*m)
              else
                xsi=dble(2*m+1-jcolm)/dble(2*m)
                eta=dble(2*m+1-jrowm)/dble(2*m)
              end if
c
c  Compute the value of U, V, and P at the point.
c
              call uval0(eta,g,ielem,indx,isotri,nelem,neqn,
     &          node,np,p,u,v,xc,x,xsi,yc,y)
c
c  Store the solution values as coefficients.
c
              ip=(icol-1)*(2*ny-1)+irow
              ipm=(icolm-1)*(2*nym-1)+irowm


              if(ipm.gt.npm)then
                write(*,*)'IPM out of bounds.'
                write(*,*)'  npm=',npm
                write(*,*)'  np=',np
                write(*,*)'  neqn=',neqn
                write(*,*)'  neqnm=',neqnm
                write(*,*)'  ipm=',ipm
                write(*,*)'  ip= ',ip
                write(*,*)'  ny,nym=',ny,nym
                write(*,*)'  icol,irow=',icol,irow
                write(*,*)'  icolm,irowm=',icolm,irowm
                stop
              end if

              jjj=indxm(1,ipm)
              gm(jjj)=u
              jjj=indxm(2,ipm)
              gm(jjj)=v
              jjj=indxm(3,ipm)
              if(jjj.gt.0)then
                gm(jjj)=p
              end if

            end do
          end do
        end do
      end do

      return
      end
      subroutine interv(xt,lxt,x,left,mflag)

c*********************************************************************72
c
cc INTERV brackets a value between sorted data points.
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
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
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

      integer left
      integer mflag
      integer ihi
      integer ilo
      integer istep
      integer middle
      double precision x
      double precision xt(lxt)

      save ilo

      data ilo / 1 /

      ihi=ilo+1

      if(ihi.ge.lxt)then

        if(x.ge.xt(lxt))go to 110

        if(lxt.le.1)then
          mflag=-1
          left=1
          return
        end if

        ilo=lxt-1
        ihi=lxt

      end if

      if (x .ge. xt(ihi))go to 40

      if(x.ge.xt(ilo))then
        mflag=0
        left=ilo
        return
      end if
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
      end if

      ilo = 1

      if(x.lt.xt(1))then
        mflag=-1
        left=1
        return
      end if

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
      end if

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
      end if
c
c  It is assumed that MIDDLE = ILO in case IHI = ILO+1.
c
      if(x.ge.xt(middle))then
        ilo=middle
      else
        ihi=middle
      end if

      go to 50
c
c  Set output and return.
c
  110 continue

      mflag = 1

      if(x.eq.xt(lxt))then
        mflag=0
      end if

      do left=lxt,1,-1
        if(xt(left).lt.xt(lxt))return
      end do

      return
      end
      subroutine l2dif(area,g1,g2,indx,isotri,nelem,neqn,node,np,
     &  nquad,xc,yc)
c
c*********************************************************************72
c
cc L2DIF computes the L2 norm of the differences between the values
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
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NEQN, the number of equations and unknowns.
c
      implicit double precision (a-h,o-z)

      integer nelem
      integer neqn
      integer np

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
        end do

      end if
c
c  Compute the current flow area, based on the finer mesh.
c
      flarea=0.0
      do ielem=1,nelem
        do i=1,nquad
          flarea=flarea+area(i,ielem)
        end do
      end do
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

        end do
      end do
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

c*********************************************************************72
c
cc LL2DIF computes the little L2 norm of solution differences.
c
c  Discussion:
c
c    The differences are taken between the values of U, V, and P computed 
c    for two different mesh spacings H1 and H2.
c
c    LL2DIF can be used to estimate the rate of convergence of a 
c    sequence of solutions, G1, G2, G3, ..., GN, by comparing each
c    solution G to GN.
c
c    LL2DIF can also do the same computation for sensitivities; simply
c    pass in the appropriate column of the sensitivity coefficient vectors
c    for the G arguments.
c
c    In order to do the integration with some ease, it is assumed that
c    H2 is a refinement of H1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NEQN, the number of equations and unknowns.
c
c    Input, integer NP, the number of nodes.
c
      implicit double precision (a-h,o-z)

      integer neqn
      integer np

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

      npp=0

      do i=1,np
        
        j=indx(1,i)
        u1=g1(j)
        u2=g2(j)
        udif=abs(u1-u2)

        u1nrm=u1nrm+u1**2
        u2nrm=u2nrm+u2**2
        udfnrm=udfnrm+udif**2

        j=indx(2,i)
        v1=g1(j)
        v2=g2(j)
        vdif=abs(v1-v2)

        v1nrm=v1nrm+v1**2
        v2nrm=v2nrm+v2**2
        vdfnrm=vdfnrm+vdif**2

        j=indx(3,i)

        if(j.gt.0)then

          p1=g1(j)
          p2=g2(j)
          pdif=abs(p1-p2)

          p1nrm=p1nrm+p1**2
          p2nrm=p2nrm+p2**2
          pdfnrm=pdfnrm+pdif**2

          npp=npp+1

        end if

      end do
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

c*********************************************************************72
c
cc LLINFDIF computes the little L infinity norm of the differences 
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
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NEQN, the number of equations and unknowns.
c
c    Input, integer NP, the number of nodes.
c
      implicit double precision (a-h,o-z)

      integer neqn
      integer np

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
        end if
        
        if(abs(u2).gt.u2nrm)then
          u2nrm=abs(u2)
          iu2nrm=i
        end if

        if(abs(udif).gt.udfnrm)then
          udfnrm=abs(udif)
          iudfnrm=i
        end if

        j=indx(2,i)
        v1=g1(j)
        v2=g2(j)
        vdif=abs(v1-v2)

        if(abs(v1).gt.v1nrm)then
          v1nrm=abs(v1)
          iv1nrm=i
        end if
        
        if(abs(v2).gt.v2nrm)then
          v2nrm=abs(v2)
          iv2nrm=i
        end if

        if(abs(vdif).gt.vdfnrm)then
          vdfnrm=abs(vdif)
          ivdfnrm=i
        end if

        j=indx(3,i)
        if(j.gt.0)then
          p1=g1(j)
          p2=g2(j)
          pdif=abs(p1-p2)

          if(abs(p1).gt.p1nrm)then
            p1nrm=abs(p1)
            ip1nrm=i
          end if
        
          if(abs(p2).gt.p2nrm)then
            p2nrm=abs(p2)
            ip2nrm=i
          end if

          if(abs(pdif).gt.pdfnrm)then
            pdfnrm=abs(pdif)
            ipdfnrm=i
          end if

          npp=npp+1

        end if

      end do

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
      subroutine plot(g,indx,neqn,np,xc,yc)

c*********************************************************************72
c
cc PLOT prints out the solution at the nodes.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NEQN, the number of equations and unknowns.
c
c    Input, integer NP, the number of nodes.
c
      integer neqn
      integer np

      double precision g(neqn)
      integer indx(3,np)
      integer ip
      integer j
      double precision p
      double precision u
      double precision v
      double precision x
      double precision xc(np)
      double precision y
      double precision yc(np)

      do ip=1,np

        x=xc(ip)
        y=yc(ip)
        j=indx(1,ip)
        u=g(j)
        v=g(indx(2,ip))
        if(indx(3,ip).gt.0)then
          p=g(indx(3,ip))
        else
          p=0.0
        end if

        if(indx(3,ip).gt.0)then
          write(*,'(i4,5g12.4)')ip,x,y,u,v,p
        else
          write(*,'(i4,5g12.4)')ip,x,y,u,v
        end if

      end do

      return
      end
      subroutine plval(nvec,xval,xvec,yval,yvec)

c*********************************************************************72
c
cc PLVAL evaluates a piecewise linear function at a given point.
c
c  Discussion:
c
c    Note that if XVAL falls to the left of XVEC(1), then YVAL=YVEC(1),
c    and similarly, if XVAL is greater than XVEC(NVEC), YVAL=YVEC(NVEC).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NVEC, the number of abscissas and coefficients
c    that define the piecewise linear.  NVEC must be at least 1.
c
c    Input, double precision XVAL, the point at which the function
c    is to be evaluated.
c
c    Input, double precision XVEC(NVEC), the abscissas of the
c    function.  These should be distinct and in ascending order.
c
c    Output, double precision YVAL, the value of the piecewise
c    linear function at the point XVAL.
c
c    Input, double precision YVEC(NVEC), the value of the piecewise
c    function at each of the abscissas.
c
      implicit double precision (a-h,o-z)

      integer nvec

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
      end if
c
c  Step 2: Find index I so that XVEC(I) <= XVAL < XVEC(I+1)
c
      do i=1,nvec-1
 
        if(xvec(i).le.xval.and.xval.le.xvec(i+1))then
          ival=i
          go to 10
        end if
 
      end do
 
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
      end if
 
      return
      end
      subroutine ppvalu(break,coef,l,k,x,jderiv,value)

c*********************************************************************72
c
cc PPVALU evaluates a piecewise polynomial function.
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
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision BREAK(L+1), REAL COEF(*), integer L, for
c    piecewise polynomial representation of the function F to
c    be evaluated.
c
c    Input, integer K, the order of the polynomial pieces
c    that make up the function F.  The most usual value for
c    K is 4, signifying a piecewise cubic polynomial.
c
c    Input, double precision X, the point at which to evaluate F or
c    of its derivatives.
c
c    Input, integer JDERIV, the order of the derivative to be
c    evaluated.  If JDERIV is 0, then F itself is evaluated,
c    which is actually the most common case.  It is assumed
c    that JDERIV is zero or positive.
c
c    Output, double precision VALUE, the value of the JDERIV-th
c    derivative of F at X.
c
      implicit double precision(a-h,o-z)

      integer k
      integer l

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

c*********************************************************************72
c
cc PQVAL evaluates a piecewise quadratic function at a given point.
c
c  Discussion:
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
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
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

      integer nvec

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
      end if
 
      if(mod(nvec,2).ne.1)then
        write(*,*)' '
        write(*,*)'PQVal - Fatal error!'
        write(*,*)'  Even value of NVEC = ',nvec
        stop
      end if
c
c  Step 1: Find odd index I so that XVEC(I) <= XVAL < XVEC(I+2)
c
      if(xval.le.xvec(1))then
        yval=yvec(1)
        return
      elseif(xval.ge.xvec(nvec))then
        yval=yvec(nvec)
        return
      end if
 
      do i=1,nvec-2,2
 
        if(xvec(i).le.xval.and.xval.le.xvec(i+2))then
          ival=i
          go to 10
        end if
 
      end do
 
      write(*,*)' '
      write(*,*)'PQVal - Fatal error!'
      write(*,*)'  Could not bracket XVAL=',xval
      write(*,*)'  There are ',nvec,' nodes.'
      write(*,*)'  First node is at ',xvec(1)
      write(*,*)'  Last node is at  ',xvec(nvec)
 
      do i=1,nvec
        write(*,*)xvec(i)
      end do
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

c*********************************************************************72
c
cc QBF0 evaluates a quadratic basis function in a nonisoparametric element.
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
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  IELEM  Input, integer IELEM, the number of the element we are
c         examining.  This will be a value between 1 and NELEM.
c
c  IN     Input, integer IN, the number of the basis function we
c         want.  This will be a value between 1 and 6.  Functions
c         1 through 3 are associated with corners, 4 though 6
c         with sides.
c
c  W      Output, double precision W, DWDX, DWDY, the value of the
c         IN-th basis  function and its X and Y derivatives, at the
c         given point.
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

      integer nelem
      integer np

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
 
      end if
 
      return
      end
      subroutine reads(ibs,nelem,neqn,np,nparb,nquad,nx,ny,xbl,xbr)

c*********************************************************************72
c
cc READS reads scalar data from a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NP, the number of nodes.
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

      read(17,'(a)')string
      read(17,*)ibs
      read(17,'(a)')string
      read(17,*)nelem
      read(17,'(a)')string
      read(17,*)neqn
      read(17,'(a)')string
      read(17,*)np
      read(17,'(a)')string
      read(17,*)nparb
      read(17,'(a)')string
      read(17,*)nquad
      read(17,'(a)')string
      read(17,*)nx
      read(17,'(a)')string
      read(17,*)ny
      read(17,'(a)')string
      read(17,*)xbl
      read(17,'(a)')string
      read(17,*)xbr

      return
      end
      subroutine readv(area,g,indx,isotri,nelem,neqn,node,np,
     &  nparb,nquad,splbmp,taubmp,xc,yc)

c*********************************************************************72
c
cc READV reads vector data from a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NP, the number of nodes.
c
      integer nelem
      integer neqn
      integer np
      integer nparb
      integer nquad

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

      read(17,'(a)')string
      do i=1,nquad
        do j=1,nelem
          read(17,*)area(i,j)
        end do
      end do

      read(17,'(a)')string
      do i=1,neqn
        read(17,*)g(i)
      end do

      read(17,'(a)')string
      do i=1,3
        do j=1,np
          read(17,*)indx(i,j)
        end do
      end do

      read(17,'(a)')string
      do i=1,nelem
        read(17,*)isotri(i)
      end do

      read(17,'(a)')string
      do i=1,6
        do j=1,nelem
          read(17,*)node(i,j)
        end do
      end do

      read(17,'(a)')string
      do i=1,4
        do j=1,nparb+2
          do k=0,nparb
            read(17,*)splbmp(i,j,k)
          end do
        end do
      end do

      read(17,'(a)')string
      do i=1,nparb+2
        read(17,*)taubmp(i)
      end do

      read(17,'(a)')string
      do i=1,np
        read(17,*)xc(i)
      end do

      read(17,'(a)')string
      do i=1,np
        read(17,*)yc(i)
      end do

      return
      end
      subroutine refbsp0(q,iq,eta,xsi)

c*********************************************************************72
c
cc REFBSP0 evaluates linear basis functions in a reference triangle.
c
c  REFBSP0 evaluates one of the three linear basis functions
c  at a particular point (X,Y) in a particular element, by referring 
c  to the corresponding points (XSI,ETA) in the reference triangle.
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
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
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

      double precision eta
      integer iq
      double precision q
      double precision xsi

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
      end if
 
      return
      end
      subroutine refqbf0(w,eta,iq,xsi)

c*********************************************************************72
c
cc REFQBF0 evaluates quadratic basis functions in a reference triangle.
c
c  REFQBF0 evaluates one of the six quadratic basis functions
c  at a particular point in a particular element, by referring to 
c  the reference triangle.
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
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  W      Output, double precision W, the value of the basis
c         function at the point (XSI,ETA).
c
c  ETA    Input, double precision ETA, the ETA coordinate of the point.
c
c  IQ     Input, integer IQ, the local node number, between 1 and
c         6, whose basis function is being evaluated.
c
c  XSI    Input, double precision XSI, the XSI coordinate of the point.
c
      implicit double precision (a-h,o-z)

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
        w= (2.0*xsi-1.0) * (xsi-1.0)
c
c  Basis 2 is zero if ETA=0 or ETA=0.5.
c
      elseif(iq.eq.2)then
        w= eta * (2.0*eta-1.0)
c
c  Basis 3 is zero if XSI=ETA, or XSI=ETA+0.5
c
      elseif(iq.eq.3)then
        w= (xsi-eta) * (2.0*xsi-2.0*eta-1.0)
c
c  Basis 4 is zero if ETA=0 or XSI=1.
c
      elseif(iq.eq.4)then
        w= 4.0 * eta * (1.0-xsi)
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
      end if

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
      subroutine uval0(eta,g,ielem,indx,isotri,nelem,neqn,
     &  node,np,p,u,v,xc,x,xsi,yc,y)

c*********************************************************************72
c
cc UVAL0 evaluates the velocities and pressure in a given element.
c
c  If the element is not isoparametric, then UVAL0 requires the
c  physical X and Y coordinates of the point.
c
c  If the element is isoparametric, UVAL0 requires the XSI, ETA
c  coordinates of the point.  
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision ETA, the ETA coordinate of the point,
c    needed only if the element is isoparametric.
c
c    Input, double precision G(NEQN).
c    G is the computed solution vector, in which are stored
c    pressures and velocities.
c
c    Input, integer IELEM, the element in which the point lies
c    at which the quantities are desired.
c
c    Input, integer NP, the number of nodes.
c
c    Input, double precision X, the X coordinate of the point.
c
c    Input, double precision XSI, the XSI coordinate of the point,
c    needed only if the element is isoparametric.
c
c    Input, double precision Y, the Y coordinate of the point.
c
      implicit double precision (a-h,o-z)

      integer nelem
      integer neqn
      integer np

      double precision coef
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
 
        end if
c
c  Compute the coefficients at the node at XP, YP.
c
        ip=node(iq,ielem)

        iun=indx(1,ip)
        coef=g(iun)
        u=u+coef*w

        iun=indx(2,ip)
        coef=g(iun)
        v=v+coef*w

        iun=indx(3,ip)
        if(iun.gt.0)then
          coef=g(iun)
          p=p+coef*q
        end if
 
      end do
 
      return
      end
      subroutine xofxsi(eta,ielem,nelem,node,np,x,xc,xsi,y,yc)

c*********************************************************************72
c
cc XOFXSI computes the (X,Y) coordinates of a (XSI,ETA) reference.
c
c  Discussion:
c
c    XOFXSI is given the XSI, ETA coordinates of a point in an
c    isoparametric element and determines its X, Y coordinates.
c
c  Diagram:
c
c          ^
c          |
c        1 +        2
c          |       /|
c    ETA   |      4 5
c          |     /  |
c        0 +    1-6-3
c          |
c          +----+---+--->
c               0   1
c
c              XSI
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision ETA, the ETA coordinate of the point.
c
c    Input, integer IELEM, the number of the isoparametric
c    element we are examining.
c
c    Input, integer MAXELM, the maximum number of elements.
c
c    Input, integer NODE(6,MAXELM), contains the numbers
c    of the nodes that make up each element.  
c
c    Input, integer NP, the number of nodes.
c
c    Output, double precision X, the X coordinate of the point.
c
c    Input, double precision XC(NP), the X coordinates of the 
c    nodes.
c
c    Input, double precision XSI, the XSI coordinate of the point.
c
c    Output, double precision Y, the Y coordinate of the point.
c
c    Input, double precision YC(NP), the Y coordinates of the 
c    nodes.
c
      implicit double precision (a-h,o-z)

      integer nelem
      integer np

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
      end do
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
