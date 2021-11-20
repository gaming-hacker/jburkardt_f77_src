c  Netode.f  21 April 1995
c
      program main
c
c*********************************************************************72
c
c  NETODE is a two fluid network ODE problem solved using LSODI.
c
c  We assume a network of nodes and links in which a 
c  two-phase (gas and liquid) fluid is flowing.
c
c
c  This version of NETODE calls the version of LSODI that is
c  available in ODEPACK, the latest edition of the Hindmarsh codes.
c  ODEPACK is available from NETLIB, via the commands:
c
c    ftp netlib.att.com
c    login: anonymous
c    cd netlib
c    cd odepack
c    binary
c    mget *
c    quit
c
c  The UNCOMPRESS program must be used to uncompress the files ending
c  with the extension ".Z".
c
c
c  ALGLNK  The value of the gas fraction on link I.
c
c  ALLLNK  The value of the liquid fraction on link I.
c
c  ATOL    The absolute error tolerance used by LSODI.
c
c  CRSLNK  The cross-sectional area of a link.
c
c  FRICGW  The gas/wall friction coefficient.
c
c  FRICGL  The gas/liquid friction coefficient.
c
c  FRICLW  The liquid/wall friction coefficient.
c
c  GRAVIT  The gravitational vector.
c
c  HOTNOD  The heat source at a node.
c
c  ITASK   1, LSODI is to return only upon reaching TOUT.
c          2, LSODI returns on each step.
c
c  ITOL    1, ATOL and RTOL are both scalars.
c
c  LIW     The storage used for IWORK.
c
c  LNKNOD  If we implicitly order the links by their starting node,
c          and, for equal starting nodes, by link number, then 
c          LNKNOD(1,*) lists the links in this order.
c
c          Similarly, LNKNOD(2,*) works for end nodes.
c
c          This is the only variable whose name ends in "NOD" which
c          is actually a link-based quantity.
c
c  LNKOPT  0, use the donor cell strategy for link quantities.
c          1, use cell averaging for link quantities.
c
c  LRW     The storage used for RWORK.
c
c  MAXIW   The maximum storage for IWORK.
c
c  MAXRW   The maximum storage for RWORK.
c
c  MF      12, Adams formulas, approx jacobian, full storage.
c          22, BDF formulas, approx jacobian, full storage.
c  
c  NABNOD  NABNOD(1,I) is the number of links for which node
c          I is the beginning node.  Similarly, NABNOD(2,I)
c          is the number of links for which node I is the end node.
c
c  NODLNK  NODLNK(1,I) and NODLNK(2,I) are the numbers of the
c          beginning and end nodes for link I.
c
c  NSTEP   The number of intermediate solutions that LSODI is
c          requested to compute between the current time and the
c          target time.
c
c  NUMEQN  The number of equations.
c
c  NUMLNK  The number of links.
c
c  NUMNOD  The number of nodes.
c
c  POSNOD  The X, Y and Z coordinates of each node.
c
c  RHCRIT  The critical density.
c
c  RTOL    The relative error tolerance used by LSODI.
c
c  SCRIT   The critical value of sigma.
c
c  VOLLNK  The volume associated with a link.
c
c  VOLNOD  The volume associated with a node.
c
c  WCRIT   The critical value of W.
c
c  Y       Real Y(NUMEQN), the unknowns, as used by LSODI.
c
c          The first NUMNOD quantities are ALPHA(I), the gas
c          fraction at node I.
c
c          The next NUMNOD quantities are PRESS(I), the pressure
c          at node I.
c
c          The next NUMNOD quantities are HGAS(I), the gas
c          enthalpy at node I.
c
c          The next NUMNOD quantities are HLIQ(I), the liquid
c          enthalpy at node I.
c
c          The next NUMLNK quantities are VGAS(I), the gas
c          velocity at link I.
c
c          The next NUMLNK quantities are VLIQ(I), the liquid
c          velocity at link I.
c
c  Derived quantities include:
c
c    RhGas   Gas density, determined from HGas and Press.
c    RhLiq   Liquid density, determined from HLiq and Press.
c    Alpha   Void fraction on links.
c    Grav    The gravitational potential energy at a node.
c            Grav(node) = G * Y(Node)
c    HGas    Gas enthalpy at a link.
c    HLiq    Liquid enthalpy at a link.
c    PrsLnk  Pressure on a link.
c
      integer maxeqn
      integer maxiw
      integer maxlnk
      integer maxnod
      integer maxrw
c
      parameter (maxiw=125)
      parameter (maxlnk=15)
      parameter (maxnod=15)
      parameter (maxrw=2500)
c
      parameter (maxeqn=4*maxnod+2*maxlnk)
c
      real alglnk(maxlnk)
      real alllnk(maxlnk)
      real amgnod(maxnod)
      real amlnod(maxnod)
      real atol(1)
      real boxlnk(2,maxlnk)
      real crslnk(maxlnk)
      real fricgl
      real fricgw
      real friclw
      real gamnod(maxnod)
      real gravit(3)
      real hglnk(maxlnk)
      real hllnk(maxlnk)
      real hotnod(maxnod)
      integer i
      integer iopt
      integer isave(36)
      character*1 isay
      integer istate
      integer itask
      integer itol
      integer iwork(maxiw)
      integer liw
      integer lnknod(2,maxlnk)
      integer lnkopt
      integer lrw
      integer mf
      integer nabnod(2,maxnod)
      integer nodlnk(2,maxlnk)
      integer nstep
      integer numeqn
      integer numlnk
      integer numnod
      real posnod(3,maxnod)
      real potnod(maxnod)
      real pr1lnk(maxlnk)
      real pr2lnk(maxlnk)
      real prslnk(maxlnk)
      real rghlnk(maxlnk)
      real rghnod(maxnod)
      real rgplnk(maxlnk)
      real rgpnod(maxnod)
      real rhcrit
      real rhglnk(maxlnk)
      real rhgnod(maxnod)
      real rhllnk(maxlnk)
      real rhlnod(maxnod)
      real rlhlnk(maxlnk)
      real rlhnod(maxnod)
      real rlplnk(maxlnk)
      real rlpnod(maxnod)
      real rsave(218)
      real rtol(1)
      real rwork(maxrw)
      real scrit
      real time
      character*80 title
      real tout
      real ttarg
      real vollnk(maxlnk)
      real volnod(maxnod)
      real wcrit
      real y(maxeqn)
      real ydot(maxeqn)
c
      external addnet
      external jacnet
      external resnet
c
      common /problm/
     &  alglnk,alllnk,amgnod,amlnod,boxlnk,crslnk,fricgl,fricgw,friclw,
     &  gamnod,gravit,hglnk,hllnk,hotnod,lnknod,lnkopt,nabnod,nodlnk,
     &  numeqn,numlnk,numnod,posnod,potnod,prslnk,rghlnk,rghnod,
     &  rgplnk,rgpnod,rhcrit,rhglnk,rhgnod,rhllnk,rhlnod,rlhlnk,
     &  rlhnod,rlplnk,rlpnod,scrit,vollnk,volnod,wcrit,pr1lnk,pr2lnk
c
      call input
c
c  Is the problem data to be read from a file?
c
      write(*,*)' '
      write(*,*)'  Is the problem definition in a file?'
      read(*,'(a1)')isay

      if(isay.eq.'y'.or.isay.eq.'Y')then

        call reader(boxlnk,crslnk,fricgl,fricgw,friclw,gravit,
     &    hotnod,isave,istate,iwork,liw,lnkopt,lrw,
     &    maxeqn,maxiw,maxlnk,maxnod,maxrw,nodlnk,numeqn,numlnk,
     &    numnod,
     &    posnod,rhcrit,rsave,rwork,scrit,time,title,vollnk,volnod,
     &    wcrit,y,ydot)

      else

        call gonet(boxlnk,crslnk,fricgl,fricgw,friclw,gravit,hotnod,
     &    isave,istate,iwork,liw,lnkopt,lrw,maxeqn,maxiw,maxlnk,maxnod,
     &    maxrw,nodlnk,numeqn,numlnk,numnod,posnod,rhcrit,rsave,rwork,
     &    scrit,time,title,vollnk,volnod,wcrit,y,ydot)

        call writer(boxlnk,crslnk,fricgl,fricgw,friclw,gravit,
     &    hotnod,isave,istate,iwork,liw,lnkopt,
     &    lrw,maxeqn,maxlnk,maxnod,nodlnk,numeqn,numlnk,numnod,
     &    posnod,rhcrit,rsave,rwork,scrit,time,title,vollnk,volnod,
     &    wcrit,y,ydot)

      endif
c
c  Construct the inverse node-link maps.
c
      call nlmap(lnknod,nabnod,nodlnk,numlnk,numnod)
c
c  Edit the starting point
c
      call edinet(numeqn,time,title,y,ydot)
c
c  Stop or go on?
c
      write(*,*)' '
      write(*,*)'Want to go on and solve the ODE?'
      read(*,'(a1)')isay
      if(isay.ne.'y'.and.isay.ne.'Y')then
        write(*,*)' '
        write(*,*)'NETODE is stopping now.'
        stop
      endif
c
c  Error tolerances
c
      itol=1
      write(*,*)'Enter absolute and relative error tolerances.'
      read(*,*)atol(1),rtol(1)
c
c  Get target time.
c
      write(*,*)' '
      write(*,*)'Enter the target time'
      read(*,*)ttarg

      write(*,*)' '
      write(*,*)'Enter number of steps to take.'
      read(*,*)nstep

      write(*,*)' '
      write(*,*)'Enter LSODI method MF:'
      write(*,*)'  12 for Adams formulas, or '
      write(*,*)'  22 for BDF formulas.'
      read(*,*)mf
      if(mf.ne.12)then
        mf=22
      endif

      write(*,*)'Enter ITASK'
      write(*,*)'  1 to see final solution only,'
      write(*,*)'  2 to see intermediate solutions.'
      read(*,*)itask
      if(itask.ne.2)then
        itask=1
      endif
c
c  No optional inputs to LSODI.
c
      iopt=0
c
c  Call LSODI.
c
      do i=1,nstep
 
        tout=(i-1)*ttarg/real(nstep)
 
        call lsodi(resnet,addnet,jacnet,numeqn,y,ydot,time,tout,itol,
     &    rtol,atol,itask,istate,iopt,rwork,lrw,iwork,liw,mf)
 
        if(istate.ne.2)then
          write(*,*)' '
          write(*,*)'NETODE - Fatal error!'
          write(*,*)'  LSODI integration halted.'
          write(*,*)'  Value of ISTATE=',istate
          stop
        endif
c
c  Print out information about current solution.
c
        call edinet(numeqn,time,title,y,ydot)
 
      enddo
c
c  Save problem information on disk for possible restart?
c
      call writer(boxlnk,crslnk,fricgl,fricgw,friclw,gravit,
     &  hotnod,isave,istate,iwork,liw,lnkopt,
     &  lrw,maxeqn,maxlnk,maxnod,nodlnk,numeqn,numlnk,numnod,
     &  posnod,rhcrit,rsave,rwork,scrit,time,title,vollnk,volnod,
     &  wcrit,y,ydot)
 
      write(*,*)' '
      write(*,*)'LSODI statistics:'
      write(*,*)' '
      write(*,*)'Last LSODI stepsize=',rwork(11)
      write(*,*)'Next LSODI stepsize=',rwork(12)
      write(*,*)'Actual value of T = ',rwork(13)
      write(*,*)'Number of LSODI steps =',iwork(11)
      write(*,*)'Number of residuals =  ',iwork(12)
      write(*,*)'Number of jacobians =  ',iwork(13)
      write(*,*)'Last order used =      ',iwork(14)
      write(*,*)'Next order to use =    ',iwork(15)
      write(*,*)'Needed real workspace =',iwork(17)
      write(*,*)'Needed integer space = ',iwork(18)
 
      stop
      end
      subroutine addnet(n,time,y,ml,mu,pa,m0)
c
c*********************************************************************72
c
c  ADDNET adds the matrix A(T,Y) to the input matrix PA.  
c
c  Here, we assume that the differential system being solved has
c  the form 
c
c    A(T,Y)*YDOT = G(T,Y)
c
      integer maxlnk
      integer maxnod
      real pcon1
c
      parameter (maxlnk=15)
      parameter (maxnod=15)
      parameter (pcon1=0.18509)
c
      integer m0
      integer n
c
      real algas
      real algas1
      real algas2
      real alglnk(maxlnk)
      real alliq
      real alllnk(maxlnk)
      real amgnod(maxnod)
      real amlnod(maxnod)
      real box1
      real box2
      real boxlnk(2,maxlnk)
      real crslnk(maxlnk)
      real drgdh
      real drgdh1
      real drgdh2
      real drgdp
      real drgdp1
      real drgdp2
      real drldh
      real drldh1
      real drldh2
      real drldp
      real drldp1
      real drldp2
      real fricgl
      real fricgw
      real friclw
      real gamnod(maxnod)
      real gravit(3)
      real hgas
      real hgas1
      real hgas2
      real hgass
      real hglnk(maxlnk)
      real hliq
      real hliq1
      real hliq2
      real hliqs
      real hllnk(maxlnk)
      real hotnod(maxnod)
      integer i
      integer ialgas
      integer ieqn
      integer igdonr
      integer ihgas
      integer ihliq
      integer ildonr
      integer ipress
      integer ivar
      integer ivgas
      integer ivliq
      integer j
      integer jalgas
      integer jhgas
      integer jhliq
      integer jpress
      integer kalgas
      integer khgas
      integer khliq
      integer kpress
      integer lnknod(2,maxlnk)
      integer lnkopt
      integer ml
      integer mu
      integer nabnod(2,maxnod)
      integer node1
      integer node2
      integer nodlnk(2,maxlnk)
      integer numeqn
      integer numlnk
      integer numnod
      real pa(m0,n)
      real posnod(3,maxnod)
      real potnod(maxnod)
      real pr1lnk(maxlnk)
      real pr2lnk(maxlnk)
      real press
      real press1
      real press2
      real prslnk(maxlnk)
      real rghlnk(maxlnk)
      real rghnod(maxnod)
      real rgplnk(maxlnk)
      real rgpnod(maxnod)
      real rhcrit
      real rhgas
      real rhgas1
      real rhgas2
      real rhglnk(maxlnk)
      real rhgnod(maxnod)
      real rhliq
      real rhliq1
      real rhliq2
      real rhllnk(maxlnk)
      real rhlnod(maxnod)
      real rlhlnk(maxlnk)
      real rlhnod(maxnod)
      real rlplnk(maxlnk)
      real rlpnod(maxnod)
      real scrit
      real time
      real vgas
      real vliq
      real vollnk(maxlnk)
      real volnod(maxnod)
      real wcrit
      real y(n)
c
      common /problm/
     &  alglnk,alllnk,amgnod,amlnod,boxlnk,crslnk,fricgl,fricgw,friclw,
     &  gamnod,gravit,hglnk,hllnk,hotnod,lnknod,lnkopt,nabnod,nodlnk,
     &  numeqn,numlnk,numnod,posnod,potnod,prslnk,rghlnk,rghnod,
     &  rgplnk,rgpnod,rhcrit,rhglnk,rhgnod,rhllnk,rhlnod,rlhlnk,
     &  rlhnod,rlplnk,rlpnod,scrit,vollnk,volnod,wcrit,pr1lnk,pr2lnk
c
      do i=1,numnod
 
        ialgas=i
        ipress=ialgas+numnod
        ihgas=ipress+numnod
        ihliq=ihgas+numnod
 
        algas=y(ialgas)
        alliq=1.0-algas
        press=y(ipress)
        hgas=y(ihgas)
        hliq=y(ihliq)
c
c  Evaluate RHGAS, DRGDP, DRGDH, HGASS,
c           RHLIQ, DRLDP, DRLDH, HLIQS.
c
        call dense(drgdh,drgdp,drldh,drldp,hgas,hgass,hliq,hliqs,press,
     &    rhgas,rhliq)
 
        rhgnod(i)=rhgas
        rgpnod(i)=drgdp
        rghnod(i)=drgdh

        rhlnod(i)=rhliq
        rlpnod(i)=drldp
        rlhnod(i)=drldh

        if(hotnod(i).gt.0.0.and.hliq.ge.hliqs)then
          gamnod(i)=alliq*hotnod(i)/(hgass-hliqs)
        elseif(hotnod(i).lt.0.0.and.hliq.le.hgass)then
          gamnod(i)=algas*hotnod(i)/(hgass-hliqs)
        else
          gamnod(i)=0.0
        endif
 
        potnod(i)=0.0
        do j=1,3
          potnod(i)=potnod(i)+gravit(j)*posnod(j,i)
        enddo
 
      enddo
c
c  Get pressure and other variables at every link.
c
      do i=1,numlnk
        ivgas=4*numnod+i
        ivliq=ivgas+numlnk
        vgas=y(ivgas)
        vliq=y(ivliq)
c
c  Get the two end nodes of link I.
c
        node1=nodlnk(1,i)
        node2=nodlnk(2,i)
        box1=boxlnk(1,i)
        box2=boxlnk(2,i)
c
c  Get the indices and values of node based quantities.
c
        jalgas=node1
        jpress=jalgas+numnod
        jhgas=jpress+numnod
        jhliq=jhgas+numnod
        algas1=y(jalgas)
        press1=y(jpress)
        hgas1=y(jhgas)
        hliq1=y(jhliq)
        rhgas1=rhgnod(node1)
        rhliq1=rhlnod(node1)
        drgdh1=rghnod(node1)
        drgdp1=rgpnod(node1)
        drldh1=rlhnod(node1)
        drldp1=rlpnod(node1)
c
c  Same for node 2.
c
        kalgas=node2
        kpress=kalgas+numnod
        khgas=kpress+numnod
        khliq=khgas+numnod
        algas2=y(kalgas)
        press2=y(kpress)
        hgas2=y(khgas)
        hliq2=y(khliq)
        rhgas2=rhgnod(node2)
        rhliq2=rhlnod(node2)
        drgdh2=rghnod(node2)
        drgdp2=rgpnod(node2)
        drldh2=rlhnod(node2)
        drldp2=rlpnod(node2)
c
c  Evaluate link quantities which depend on nodal values.
c
        call dolink(algas,algas1,algas2,alliq,box1,box2,drgdh,
     &    drgdh1,drgdh2,drgdp,drgdp1,drgdp2,drldh,drldh1,drldh2,drldp,
     &    drldp1,drldp2,hgas,hgas1,hgas2,hliq,hliq1,hliq2,lnkopt,press,
     &    press1,press2,rhgas,rhgas1,rhgas2,rhliq,rhliq1,rhliq2,vgas,
     &    vliq)
 
        alglnk(i)=algas
        alllnk(i)=alliq
        hglnk(i)=hgas
        hllnk(i)=hliq
        pr1lnk(i)=press1
        pr2lnk(i)=press2
        prslnk(i)=press
        rhglnk(i)=rhgas
        rhllnk(i)=rhliq
        rghlnk(i)=drgdh
        rgplnk(i)=drgdp
        rlhlnk(i)=drldh
        rlplnk(i)=drldp
      enddo
c
c  Add the jacobian terms to the input matrix.
c
      ieqn=0
 
      do i=1,numnod
c
c  Read off the nodal values.
c
        ialgas=i
        ipress=ialgas+numnod
        ihgas=ipress+numnod
        ihliq=ihgas+numnod
 
        algas=y(ialgas)
        alliq=1.0-algas
        press=y(ipress)
        hgas=y(ihgas)
        hliq=y(ihliq)
        rhgas=rhgnod(i)
        rhliq=rhlnod(i)
        drgdp=rgpnod(i)
        drgdh=rghnod(i)
        drldp=rlpnod(i)
        drldh=rhlnod(i)
c
c  1.  Gas continuity equation at node I.
c
        ieqn=ieqn+1

        ivar=ialgas
        pa(ieqn,ivar)=pa(ieqn,ivar)+rhgas

        ivar=ipress
        pa(ieqn,ivar)=pa(ieqn,ivar)+algas*drgdp
 
        ivar=ihgas
        pa(ieqn,ivar)=pa(ieqn,ivar)+algas*drgdh
c
c  2.  Liquid continuity equation at node I.
c
        ieqn=ieqn+1

        ivar=ialgas
        pa(ieqn,ivar)=pa(ieqn,ivar)-rhliq

        ivar=ipress
        pa(ieqn,ivar)=pa(ieqn,ivar)+alliq*drldp
 
        ivar=ihliq
        pa(ieqn,ivar)=pa(ieqn,ivar)+alliq*drldh
c
c  3.  Gas energy equation at node I.
c
        ieqn=ieqn+1

        ivar=ialgas
        pa(ieqn,ivar)=pa(ieqn,ivar)-pcon1*press

        ivar=ipress
        pa(ieqn,ivar)=pa(ieqn,ivar)-pcon1*algas
 
        ivar=ihgas
        pa(ieqn,ivar)=pa(ieqn,ivar)+algas*rhgas
c
c  4.  Liquid energy equation at node I.
c
        ieqn=ieqn+1

        ivar=ialgas
        pa(ieqn,ivar)=pa(ieqn,ivar)+pcon1*press

        ivar=ipress
        pa(ieqn,ivar)=pa(ieqn,ivar)-pcon1*alliq
 
        ivar=ihliq
        pa(ieqn,ivar)=pa(ieqn,ivar)+alliq*rhliq
 
      enddo
c
c  Now do equations at links.
c
      do i=1,numlnk
 
        ivgas=4*numnod+i
        ivliq=ivgas+numlnk
        vgas=y(ivgas)
        vliq=y(ivliq)
        node1=nodlnk(1,i)
        node2=nodlnk(2,i)
        box1=boxlnk(1,i)
        box2=boxlnk(2,i)
        if(vgas.ge.0.0)then
          igdonr=node1
        else
          igdonr=node2
        endif
        if(vliq.ge.0.0)then
          ildonr=node1
        else
          ildonr=node2
        endif
        jalgas=node1
        jpress=jalgas+numnod
        jhgas=jpress+numnod
        jhliq=jhgas+numnod
        kalgas=node2
        kpress=kalgas+numnod
        khgas=kpress+numnod
        khliq=khgas+numnod
c
c  5.  Gas momentum equation at link I.
c
        ieqn=ieqn+1

        ivar=ivgas
        pa(ieqn,ivar)=pa(ieqn,ivar)+alglnk(i)*rhglnk(i)
 
        if(lnkopt.eq.1)then
 
          ivar=jalgas
          pa(ieqn,ivar)=pa(ieqn,ivar)+0.5*vgas*rhglnk(i)*box1
     &      /(box1+box2)
 
          ivar=kalgas
          pa(ieqn,ivar)=pa(ieqn,ivar)+0.5*vgas*rhglnk(i)*box2
     &      /(box1+box2)
 
          ivar=jpress
          pa(ieqn,ivar)=pa(ieqn,ivar)+0.25*vgas*alglnk(i)*rgplnk(i)
 
          ivar=kpress
          pa(ieqn,ivar)=pa(ieqn,ivar)+0.25*vgas*alglnk(i)*rgplnk(i)
 
          if(igdonr.eq.node1)then
            ivar=jhgas
          else
            ivar=khgas
          endif
          pa(ieqn,ivar)=pa(ieqn,ivar)+0.5*vgas*alglnk(i)*rghlnk(i)

        endif
c
c  6.  Liquid momentum equation at link I.
c
        ieqn=ieqn+1

        ivar=ivliq
        pa(ieqn,ivar)=pa(ieqn,ivar)+alllnk(i)*rhllnk(i)
c
c  TAP: Should these be JALLIQ and KALLIQ?
c
        if(lnkopt.eq.1)then
 
          ivar=jalgas
          pa(ieqn,ivar)=pa(ieqn,ivar)+0.5*vliq*rhllnk(i)*box1
     &      /(box1+box2)
 
          ivar=kalgas
          pa(ieqn,ivar)=pa(ieqn,ivar)+0.5*vliq*rhllnk(i)*box2
     &      /(box1+box2)
 
          ivar=jpress
          pa(ieqn,ivar)=pa(ieqn,ivar)+0.25*vliq*alllnk(i)*rlplnk(i)
 
          ivar=kpress
          pa(ieqn,ivar)=pa(ieqn,ivar)+0.25*vliq*alllnk(i)*rlplnk(i)
 
          if(ildonr.eq.node1)then
            ivar=jhliq
          else
            ivar=khliq
          endif
          pa(ieqn,ivar)=pa(ieqn,ivar)+0.5*vliq*alllnk(i)*rlhlnk(i)

        endif
 
      enddo
 
      return
      end
      subroutine cooker(algasd,alliqd,drgdh,drgdp,drldh,drldp,hgas,
     &  hliq,ierror,press,press1,press2,rhgas,rhgasd,rhliq,rhliqd)
c
c*********************************************************************72
c
c  COOKER computes the Newton iterates of pressure for fixed values
c  of HGAS and HLIQ in an attempt to solve the nonlinear equation:
c
c    ALGASD * RHGASD * RHOL(PRESS,HLIQ) 
c  + ALLIQD * RHLIQD * RHOG(PRESS,HGAS)
c  - RHOL(PRESS,HLIQ) * RHOG(PRESS,HGAS)= 0
c
      real algasd
      real alliqd
      real drgdh
      real drgdp
      real drldh
      real drldp
      real fn
      real fp
      real hgas
      real hgass
      real hliq
      real hliqs
      integer ierror
      integer istep
      real press
      real press1
      real press2
      real rhgas
      real rhgasd
      real rhliq
      real rhliqd
c
      ierror=0
      istep=0
c
c  Initial estimate for pressure.
c
      press=0.5*(press1+press2)
 
10    continue
c
c  Get the densities RHGAS and RHLIQ for the current pressure PRESS and
c  fixed enthalpies HGAS and HLIQ, and the partial derivatives
c  DRLDP and DRGDP.
c
      call dense(drgdh,drgdp,drldh,drldp,hgas,hgass,hliq,hliqs,press,
     &  rhgas,rhliq)
c
c  Evaluate the function.
c
      fn=algasd*rhgasd*rhliq+alliqd*rhliqd*rhgas-rhgas*rhliq
 
      if(abs(fn).le.0.005.and.istep.gt.3)then
        return
      endif
c
c  Evaluate the derivative of the function.
c
      fp=(algasd*rhgasd-rhgas)*drldp+(alliqd*rhliqd-rhliq)*drgdp
 
      press=press-fn/fp
 
      if(abs(fn/fp).le.(0.001+0.0001*abs(press)).and.istep.gt.3)then
        call dense(drgdh,drgdp,drldh,drldp,hgas,hgass,hliq,hliqs,press,
     &    rhgas,rhliq)
        return
      endif
 
      if(istep.le.5)go to 10
c
c  No convergence
c
      ierror=1
      call dense(drgdh,drgdp,drldh,drldp,hgas,hgass,hliq,hliqs,press,
     &  rhgas,rhliq)

      write(*,*)' '
      write(*,*)'Cooker - Newton not converged after ',istep,' steps.'
      write(*,*)'  Newton residual FN=',fn
      write(*,*)'  Newton derivative FP=',fp
      write(*,*)'  Pressure set to ',press

      return
      end
      subroutine dense(drgdh,drgdp,drldh,drldp,hgas,hgass,hliq,hliqs,
     &  press,rhgas,rhliq)
c
c*********************************************************************72
c
c  DENSE is given a press PRESS and an enthalpy H, and computes the
c  density RH, and derivatives DRDH, DRDP, and the saturation
c  enthalpy HS.
c
c  The functional form used for density is:
c
c  Let
c
c    H=MAX(HGASS,HGAS),
c    P=PRESS
c
c  Then
c
c    RHGAS = P / ( (D2+D5*H)*P**2 + (D1+D4*H)*P + (D3+H*D6))
c
c    RHLIQ = EXP(-Z), for HLIQ <= HLIQS,
c    RHLIQ = RHLIQ(HLIQS,PRESS), for HLIQ > HLIQS.
c
      real atab(9)
      real bot
      real btab(9)
      real ctab(3,5)
      real drgdh
      real drgdp
      real drldh
      real drldp
      real dtab(6)
      real dzdh
      real dzdp
      real etab(5)
      real etabp(5)
      real hcopy
      real hgas
      real hgass
      real hliq
      real hliqs
      integer j
      real pcopy
      real press
      real rhgas
      real rhliq
      real z
c
      data atab /
     &   0.182609E+03, 0.144140E+01, -0.387216E-02, 0.651417E-05,
     &  -0.638144E-08, 0.369701E-11, -0.124626E-14, 0.225589E-18,
     &  -0.169253E-22/
 
      data btab /
     &   0.115216E+04, 0.460395E+00, -0.159024E-02, 0.286502E-05,
     &  -0.299850E-08, 0.185137E-11, -0.664224E-15, 0.127776E-18,
     &  -0.101790E-22 /
 
      data ctab /
     &  -0.41345E+01, -0.59428E-05,  0.15681E-08,
     &   0.13252E-04,  0.63377E-07, -0.40711E-10,
     &   0.15812E-05, -0.39974E-09,  0.25401E-12,
     &  -0.21959E-08,  0.69391E-12, -0.52372E-15,
     &  -0.21683E-11, -0.36159E-15,  0.32503E-18 /
 
      data dtab /
     &  -0.81735849E-03, 0.12378514E-04, -0.10339904E+04,
     &  -0.62941689E-05, -0.87292160E-08, 0.12460225E+01 /
c
c  For the given pressure PRESS, compute the saturated gas and 
c  liquid enthalpies HGASS and HLIQS.
c
      if(press.gt.3200.0)then
        pcopy=3200.0
      elseif(press.lt.0.0)then
        pcopy=0.0
      else
        pcopy=press
      endif

      call poly(pcopy,atab,9,hliqs)
      call poly(pcopy,btab,9,hgass)
 
      do j=1,5
        call poly(pcopy,ctab(1,j),3,etab(j))
        call poly(pcopy,ctab(1,j),3,etabp(j))
      enddo
c
c  Compute RHGAS, and its H and P partial derivatives.
c
      if(hgas.gt.1800.0)then
        hcopy=1800.0
      elseif(hgas.lt.10.0)then
        hcopy=10.0
      else
        hcopy=hgas
      endif

      hcopy=max(hcopy,hgass)

      bot=((dtab(2)+dtab(5)*hcopy)*press+
     &  dtab(1)+dtab(4)*hcopy)*press+dtab(3)+hcopy*dtab(6)

      rhgas=pcopy/bot
      drgdp=(dtab(3)+hcopy*dtab(6)-pcopy*(dtab(2)+hcopy*dtab(5)))/bot**2
      drgdh=-((dtab(5)*pcopy+dtab(4))*pcopy+dtab(6))*pcopy/bot**2
c
c  Compute RHLIQ, and its H and P partial derivatives.
c
      if(hliq.gt.1800.0)then
        hcopy=1800.0
      elseif(hliq.lt.10.0)then
        hcopy=10.0
      else
        hcopy=hliq
      endif

      hcopy=min(hcopy,hliqs)

      call poly(hcopy,etab,5,z)
      call dpoly(hcopy,etab,5,dzdh)
      call dpoly(hcopy,etabp,5,dzdp)

      rhliq=exp(-z)
      drldh=-rhliq*dzdh
      drldp=-rhliq*dzdp

      return
      end
      subroutine dolink(algas,algas1,algas2,alliq,box1,box2,drgdh,
     &  drgdh1,drgdh2,drgdp,drgdp1,drgdp2,drldh,drldh1,drldh2,drldp,
     &  drldp1,drldp2,hgas,hgas1,hgas2,hliq,hliq1,hliq2,lnkopt,press,
     &  press1,press2,rhgas,rhgas1,rhgas2,rhliq,rhliq1,rhliq2,vgas,
     &  vliq)
c
c*********************************************************************72
c
c  DOLINK defines link quantities which must be computed by considering
c  the values of those quantities at the two nodes that define the link.
c
      real algas
      real algas1
      real algas2
      real alliq
      real box1
      real box2
      real drgdh
      real drgdh1
      real drgdh2
      real drgdp
      real drgdp1
      real drgdp2
      real drldh
      real drldh1
      real drldh2
      real drldp
      real drldp1
      real drldp2
      real hgas
      real hgas1
      real hgas2
      real hgass
      real hliq
      real hliq1
      real hliq2
      real hliqs
      integer lnkopt
      real press
      real press1
      real press2
      real rhgas
      real rhgas1
      real rhgas2
      real rhliq
      real rhliq1
      real rhliq2
      real vgas
      real vliq
c
c  Node 1 is gas and liquid donor.
c
      if(vgas.ge.0.0.and.vliq.ge.0.0)then

        hgas=hgas1
        hliq=hliq1

        if(lnkopt.eq.0)then
          algas=algas1
          alliq=1.0-algas
          drgdh=drgdh1
          drgdp=drgdp1
          drldh=drldh1
          drldp=drldp1
          press=press1
          rhgas=rhgas1
          rhliq=rhliq1
        else
          algas=(algas1*box1+algas2*box2)/(box1+box2)
          alliq=1.0-algas
          press=(press1+press2)/2.0
          call dense(drgdh,drgdp,drldh,drldp,hgas,hgass,hliq,hliqs,
     &      press,rhgas,rhliq)
        endif
c
c  Node 2 is gas and liquid donor.
c
      elseif(vgas.lt.0.0.and.vliq.lt.0.0)then

        hgas=hgas2
        hliq=hliq2

        if(lnkopt.eq.0)then
          algas=algas2
          alliq=1.0-algas
          drgdh=drgdh2
          drgdp=drgdp2
          drldh=drldh2
          drldp=drldp2
          press=press2
          rhgas=rhgas2
          rhliq=rhliq2
        else
          algas=(algas1*box1+algas2*box2)/(box1+box2)
          alliq=1.0-algas
          press=(press1+press2)/2.0
          call dense(drgdh,drgdp,drldh,drldp,hgas,hgass,hliq,hliqs,
     &      press,rhgas,rhliq)
        endif
c
c  Node 1 is gas donor, node 2 is liquid donor.
c
      elseif(vgas.ge.0.0.and.vliq.lt.0.0)then

        hgas=hgas1
        hliq=hliq2

        if(lnkopt.eq.0)then
          algas=algas1
          alliq=1.0-algas2
          drgdh=drgdh1
          drgdp=drgdp1
          drldh=drldh2
          drldp=drldp2
          press=(press1+press2)/2.0
          rhgas=rhgas1
c
c  TAP: RHLIQ=RHLIQ1 feels like it should be RHLIQ=RHLIQ2.
c  So I "corrected" it.
c
          rhliq=rhliq2
        else
          algas=(algas1*box1+algas2*box2)/(box1+box2)
          alliq=1.0-algas
          press=(press1+press2)/2.0
          call dense(drgdh,drgdp,drldh,drldp,hgas,hgass,hliq,hliqs,
     &      press,rhgas,rhliq)
        endif
c
c  Node 1 is liquid donor, node 2 is gas donor.
c
      elseif(vgas.lt.0.0.and.vliq.ge.0.0)then

        hgas=hgas2
        hliq=hliq1

        if(lnkopt.eq.0)then
          algas=algas2
          alliq=1.0-algas1
          drgdh=drgdh2
          drgdp=drgdp2
          drldh=drldh1
          drldp=drldp1
          press=(press1+press2)/2.0
          rhgas=rhgas2
          rhliq=rhliq1
        else
          algas=(algas1*box1+algas2*box2)/(box1+box2)
          alliq=1.0-algas
          press=(press1+press2)/2.0
          call dense(drgdh,drgdp,drldh,drldp,hgas,hgass,hliq,hliqs,
     &      press,rhgas,rhliq)
        endif

      endif

      return
      end
      subroutine dpoly(base,coef,ncoef,prime)
c
c*********************************************************************72
c
c  DPOLY computes the derivative of a polynomial of the form
c
c    POLY(X) = Sum (I=1 to NCOEF) COEF(I)*BASE**(I-1)
c
      integer ncoef

      real base
      real coef(ncoef)
      integer i
      real prime

      prime=0.0

      if(ncoef.le.1)return

      prime=(ncoef-1)*coef(ncoef)

      do i=1,ncoef-2
        prime=(ncoef-1-i)*coef(ncoef-i)+prime*base
      enddo

      return
      end
      subroutine edinet(numeqn,time,title,y,ydot)
c
c*********************************************************************72
c
c  EDINET prints out information about the current solution.
c
      integer numeqn
c
      integer i
      real time
      character*80 title
      real y(numeqn)
      real ydot(numeqn)
c
      write(*,*)' '
      write(*,'(1x,a)')title
      write(*,*)' '
      write(*,*)'Current time is ',time
      write(*,*)' '
      write(*,*)'Solution Y and time derivative YDOT:'
      write(*,*)' '
      do i=1,numeqn
        write(*,*)i,y(i),ydot(i)
      enddo

      return
      end
      subroutine gonet(boxlnk,crslnk,fricgl,fricgw,friclw,gravit,hotnod,
     &  isave,istate,iwork,liw,lnkopt,lrw,maxeqn,maxiw,maxlnk,maxnod,
     &  maxrw,nodlnk,numeqn,numlnk,numnod,posnod,rhcrit,rsave,rwork,
     &  scrit,time,title,vollnk,volnod,wcrit,y,ydot)
c
c*********************************************************************72
c
c  GONET gets the information that initializes the problem, either
c  from user specification, or from a restart file.
c
      integer maxiw
      integer maxeqn
      integer maxlnk
      integer maxnod
      integer maxrw
c
      real boxlnk(2,maxlnk)
      real crslnk(maxlnk)
      real fricgl
      real fricgw
      real friclw
      real gravit(3)
      real hotnod(maxnod)
      integer i
      integer inode
      integer isave(36)
      integer istate
      integer iwork(maxiw)
      integer jnode
      integer liw
      integer lnkopt
      integer lrw
      integer nodlnk(2,maxlnk)
      integer numeqn
      integer numlnk
      integer numnod
      real posnod(3,maxnod)
      real rhcrit
      real rsave(218)
      real rwork(maxrw)
      real scrit
      real time
      character*80 title
      real vollnk(maxlnk)
      real volnod(maxnod)
      real wcrit
      real y(maxeqn)
      real ydot(maxeqn)
c
      write(*,*)' '
      write(*,*)'GoNet:'
      write(*,*)'  Get NetODE problem definition.'
c
c  Problem title.
c
      write(*,*)' '
      write(*,*)'Enter a title for the problem.'
      read(*,'(a)')title
c
c  Get the number of nodes, and the values of the node
c  based quantities.
c
      write(*,*)' '
      write(*,*)'Enter the number of nodes.'
      write(*,*)'between 2 and ',maxnod
      read(*,*)numnod

      write(*,*)' '
      write(*,*)'Enter X, Y, Z, heat source and volume'
      write(*,*)'for each node.'

      do i=1,numnod
        read(*,*)posnod(1,i),posnod(2,i),posnod(3,i),hotnod(i),volnod(i)
      enddo
c
c  Get links.
c
      write(*,*)' '
      write(*,*)'Define links between pairs of nodes, by specifying'
      write(*,*)'the two nodes.  To finish, use 0, 0 for the nodes.'

      numlnk=0
10    continue
      read(*,*)inode,jnode
      if(inode.ne.0.and.jnode.ne.0)then
        numlnk=numlnk+1
        nodlnk(1,numlnk)=inode
        nodlnk(2,numlnk)=jnode
        go to 10
      endif
c
c  Get the value of link quantities.
c
      write(*,*)' '
      write(*,*)'Enter volume, cross-sectional area, and 2 box values'
      write(*,*)'for each link.'
      do i=1,numlnk
        read(*,*)vollnk(i),crslnk(i),boxlnk(1,i),boxlnk(2,i)
      enddo
c
c  Get constitutive quantities.
c
      write(*,*)' '
      write(*,*)'Enter gas-liquid friction coefficient.'
      read(*,*)fricgl
      write(*,*)' '
      write(*,*)'Enter gas-wall friction coefficient.'
      read(*,*)fricgw
      write(*,*)' '
      write(*,*)'Enter liquid-wall friction coefficient.'
      read(*,*)friclw

      write(*,*)' '
      write(*,*)'Enter 3 components of gravity vector:'
      read(*,*)gravit(1),gravit(2),gravit(3)

      write(*,*)'Enter 0 for donor cell, 1 for cell average.'
      read(*,*)lnkopt

      write(*,*)'Enter critical density, Rho-Crit'
      read(*,*)rhcrit

      write(*,*)'Enter critical S'
      read(*,*)scrit

      write(*,*)'Enter critical W'
      read(*,*)wcrit
c
c  Compute number of equations
c
      numeqn=4*numnod+2*numlnk
      time=0.0
      liw=maxiw
      lrw=maxrw
      do i=1,liw
        iwork(i)=0
      enddo
      do i=1,lrw
        rwork(i)=0.0
      enddo
      do i=1,36
        isave(i)=0
      enddo
      do i=1,218
        rsave(i)=0.0
      enddo

      write(*,*)' '
      write(*,*)'Enter initial values of Y'
      write(*,*)' '
      do i=1,numeqn
        read(*,*)y(i)
      enddo

      write(*,*)' '
      write(*,*)'Enter initial values of YDOT'
      write(*,*)' '
      do i=1,numeqn
        read(*,*)ydot(i)
      enddo

      write(*,*)' '
      write(*,*)'Enter 0 if you believe YDOT is not consistent with Y,'
      write(*,*)'      1 otherwise.'
      read(*,*)istate
      if(istate.ne.1)then
        istate=0
      endif
      
      return
      end
      subroutine jacnet(numeqn,time,y,ydot,ml,mu,pa,m0)
c
c*********************************************************************72
c
c  JACNET, if supplied, is supposed to compute the jacobian of the
c  residual, which has the form:
c
c    D RES/D Y= d ( G(Y,TIME)-A(Y,TIME)*YDOT) /D Y
c
c  We do not supply the jacobian, and this is simply a dummy routine.
c
      integer m0
      integer numeqn
c
      integer ml
      integer mu
      real pa(m0,numeqn)
      real time
      real y(numeqn)
      real ydot(numeqn)
c
      write(*,*)' '
      write(*,*)'JACNET - Fatal error!'
      write(*,*)'  This dummy routine should NEVER be called!'

      stop
      end
      subroutine nlmap(lnknod,nabnod,nodlnk,numlnk,numnod)
c
c*********************************************************************72
c
c  NLMAP constructs the arrays NABNOD and LNKNOD.
c
      integer numlnk
      integer numnod
c
      integer i
      integer j
      integer k
      integer lnknod(2,numlnk)
      integer n1
      integer n2
      integer nabnod(2,numnod)
      integer nlink
      integer nodlnk(2,numlnk)
c
c  NABNOD records how many links a given node starts or ends.
c
      do i=1,numnod
        nabnod(1,i)=0
        nabnod(2,i)=0
      enddo

      do i=1,numlnk
        n1=nodlnk(1,i)
        nabnod(1,n1)=nabnod(1,n1)+1
        n2=nodlnk(2,i)
        nabnod(2,n2)=nabnod(2,n2)+1
      enddo
c
c  If we implicitly order the links by starting node, and then
c  by link number, LNKNOD(1,*) records this ordering.
c
      do k=1,2

        nlink=0

        do i=1,numnod

          do j=1,numlnk
            if(nodlnk(k,j).eq.i)then
              nlink=nlink+1
              lnknod(k,nlink)=j
            endif
          enddo

        enddo

      enddo

      return
      end
      subroutine poly(base,coef,ncoef,pvalu)
c
c*********************************************************************72
c
cc POLY computes the value of a polynomial of the form
c
c    POLY(X) = Sum (I=1 to NCOEF) COEF(I)*BASE**(I-1)
c
      integer ncoef
c
      real base
      real coef(ncoef)
      integer i
      real pvalu
c
      pvalu=coef(ncoef)
      do i=1,ncoef-1
        pvalu=coef(ncoef-i)+base*pvalu
      enddo
 
      return
      end
      subroutine reader(boxlnk,crslnk,fricgl,fricgw,friclw,gravit,
     &  hotnod,isave,istate,iwork,liw,lnkopt,lrw,
     &  maxeqn,maxiw,maxlnk,maxnod,maxrw,nodlnk,numeqn,numlnk,
     &  numnod,posnod,
     &  rhcrit,rsave,rwork,scrit,time,title,vollnk,volnod,wcrit,y,ydot)
c
c*********************************************************************72
c
cc READER ...
c
      integer maxiw
      integer maxeqn
      integer maxlnk
      integer maxnod
      integer maxrw
c
      real boxlnk(2,maxlnk)
      real crslnk(maxlnk)
      character*80 filer
      real fricgl
      real fricgw
      real friclw
      real gravit(3)
      real hotnod(maxnod)
      integer i
      integer isave(36)
      integer istate
      integer iwork(maxiw)
      integer job
      integer liw
      integer lnkopt
      integer lrw
      integer nodlnk(2,maxlnk)
      integer numeqn
      integer numlnk
      integer numnod
      real posnod(3,maxnod)
      real rhcrit
      real rsave(218)
      real rwork(maxrw)
      real scrit
      real time
      character*80 title
      real vollnk(maxlnk)
      real volnod(maxnod)
      real wcrit
      real y(maxeqn)
      real ydot(maxeqn)
c
      write(*,*)' '
      write(*,*)'Enter name of restart file to be read:'
      read(*,'(a)')filer
c
      open(unit=2,file=filer,status='old',err=10)

      read(2,'(a80)')title

      read(2,*)numeqn
      read(2,*)numlnk
      read(2,*)numnod
      read(2,*)liw
      read(2,*)lrw

      do i=1,numlnk
        read(2,*)boxlnk(1,i)
      enddo

      do i=1,numlnk
        read(2,*)boxlnk(2,i)
      enddo

      do i=1,numlnk
        read(2,*)crslnk(i)
      enddo

      read(2,*)fricgl
      read(2,*)fricgw
      read(2,*)friclw

      do i=1,3
        read(2,*)gravit(i)
      enddo

      do i=1,numnod
        read(2,*)hotnod(i)
      enddo

      do i=1,36
        read(2,*)isave(i)
      enddo

      read(2,*)istate

      do i=1,liw
        read(2,*)iwork(i)
      enddo

      read(2,*)lnkopt

      do i=1,numlnk
        read(2,*)nodlnk(1,i)
      enddo

      do i=1,numlnk
        read(2,*)nodlnk(2,i)
      enddo

      do i=1,numnod
        read(2,*)posnod(1,i)
      enddo

      do i=1,numnod
        read(2,*)posnod(2,i)
      enddo

      do i=1,numnod
        read(2,*)posnod(3,i)
      enddo

      read(2,*)rhcrit

      do i=1,218
        read(2,*)rsave(i)
      enddo

      do i=1,lrw
        read(2,*)rwork(i)
      enddo

      read(2,*)scrit
      read(2,*)time

      do i=1,numlnk
        read(2,*)vollnk(i)
      enddo

      do i=1,numnod
        read(2,*)volnod(i)
      enddo

      read(2,*)wcrit

      do i=1,numeqn
        read(2,*)y(i)
      enddo

      do i=1,numeqn
        read(2,*)ydot(i)
      enddo

      close(unit=2)
c
c  Pass new data to LSODI.
c
      job=2
      call srcom(rsave,isave,job)

      return
c
c  Error opening the file.
c
10    continue
      write(*,*)' '
      write(*,*)'Reader - Fatal error!'
      write(*,*)'  The input file could not be read!'
      stop
      end
      subroutine resnet(n,time,y,ydot,res,ires)
c
c*********************************************************************72
c
c  RESNET computes the residual 
c
c    RES=G(Y,TIME)-A(Y,TIME)*YDOT
c
      integer maxlnk
      integer maxnod
      real pcon1
      real pcon2
      real sqrtpi
c
      parameter (maxlnk=15)
      parameter (maxnod=15)
      parameter (pcon1=0.18509)
      parameter (pcon2=4629.168)
      parameter (sqrtpi=1.77245)
c
      integer n
c
      real res(n)
      real y(n)
      real ydot(n)
c
      real algas
      real algas1
      real algas2
      real algasl
      real alglnk(maxlnk)
      real alliq
      real alliql
      real alllnk(maxlnk)
      real amgnod(maxnod)
      real amlnod(maxnod)
      real box1
      real box2
      real boxlnk(2,maxlnk)
      real crslnk(maxlnk)
      real drgdh
      real drgdh1
      real drgdh2
      real drgdp
      real drgdp1
      real drgdp2
      real drldh
      real drldh1
      real drldh2
      real drldp
      real drldp1
      real drldp2
      real fgl
      real fgw
      real flw
      real fricgl
      real fricgw
      real friclw
      real gamma1
      real gamma2
      real gamnod(maxnod)
      real gravit(3)
      real hgas
      real hgas1
      real hgas2
      real hgasl
      real hgass
      real hglnk(maxlnk)
      real hliq
      real hliq1
      real hliq2
      real hliql
      real hliqs
      real hllnk(maxlnk)
      real hotnod(maxnod)
      integer i
      integer ialgas
      integer ieqn
      integer igdonr
      integer ihgas
      integer ihliq
      integer ildonr
      integer ipress
      integer ires
      integer ivgas
      integer ivliq
      integer ix1
      integer ix2
      integer j
      integer jalgas
      integer jhgas
      integer jhliq
      integer jpress
      integer kalgas
      integer khgas
      integer khliq
      integer kpress
      integer link
      integer lnknod(2,maxlnk)
      integer lnkopt
      integer nabnod(2,maxnod)
      integer node1
      integer node2
      integer nodlnk(2,maxlnk)
      integer numeqn
      integer numlnk
      integer numnod
      real plink
      real posnod(3,maxnod)
      real poten1
      real poten2
      real potnod(maxnod)
      real pr1lnk(maxlnk)
      real pr2lnk(maxlnk)
      real press
      real press1
      real press2
      real prslnk(maxlnk)
      real rghlnk(maxlnk)
      real rghnod(maxnod)
      real rgplnk(maxlnk)
      real rgpnod(maxnod)
      real rhcrit
      real rhgas
      real rhgas1
      real rhgas2
      real rhgasl
      real rhglnk(maxlnk)
      real rhgnod(maxnod)
      real rhliq
      real rhliq1
      real rhliq2
      real rhliql
      real rhllnk(maxlnk)
      real rhlnod(maxnod)
      real rlhlnk(maxlnk)
      real rlhnod(maxnod)
      real rlplnk(maxlnk)
      real rlpnod(maxnod)
      real scrit
      real sum1
      real sum2
      real time
      real vgas
      real vliq
      real vollnk(maxlnk)
      real volnod(maxnod)
      real wcrit
c
      common /problm/
     &  alglnk,alllnk,amgnod,amlnod,boxlnk,crslnk,fricgl,fricgw,friclw,
     &  gamnod,gravit,hglnk,hllnk,hotnod,lnknod,lnkopt,nabnod,nodlnk,
     &  numeqn,numlnk,numnod,posnod,potnod,prslnk,rghlnk,rghnod,
     &  rgplnk,rgpnod,rhcrit,rhglnk,rhgnod,rhllnk,rhlnod,rlhlnk,
     &  rlhnod,rlplnk,rlpnod,scrit,vollnk,volnod,wcrit,pr1lnk,pr2lnk
c
      do i=1,numnod
 
        ialgas=i
        ipress=ialgas+numnod
        ihgas=ipress+numnod
        ihliq=ihgas+numnod
 
        algas=y(ialgas)
        alliq=1.0-algas
        press=y(ipress)
        hgas=y(ihgas)
        hliq=y(ihliq)
c
c  Evaluate RHGAS, DRGDP, DRGDH, HGASS,
c           RHLIQ, DRLDP, DRLDH, HLIQS.
c
        call dense(drgdh,drgdp,drldh,drldp,hgas,hgass,hliq,hliqs,press,
     &    rhgas,rhliq)
 
        rhgnod(i)=rhgas
        rhlnod(i)=rhliq
        rgpnod(i)=drgdp
        rghnod(i)=drgdh
        rlpnod(i)=drldp
        rlhnod(i)=drldh

        if(hotnod(i).gt.0.0.and.hliq.ge.hliqs)then
          gamnod(i)=alliq*hotnod(i)/(hgass-hliqs)
        elseif(hotnod(i).lt.0.0.and.hliq.le.hgass)then
          gamnod(i)=algas*hotnod(i)/(hgass-hliqs)
        else
          gamnod(i)=0.0
        endif
 
        potnod(i)=0.0
        do j=1,3
          potnod(i)=potnod(i)+gravit(j)*posnod(j,i)
        enddo
 
      enddo
c
c  Get pressure and other variables at every link.
c
      do i=1,numlnk
        ivgas=4*numnod+i
        ivliq=ivgas+numlnk
        vgas=y(ivgas)
        vliq=y(ivliq)
c
c  Get the two end nodes of link I.
c
        node1=nodlnk(1,i)
        node2=nodlnk(2,i)
        box1=boxlnk(1,i)
        box2=boxlnk(2,i)
c
c  Get the indices and values of node based quantities.
c
        jalgas=node1
        jpress=jalgas+numnod
        jhgas=jpress+numnod
        jhliq=jhgas+numnod
        algas1=y(jalgas)
        press1=y(jpress)
        hgas1=y(jhgas)
        hliq1=y(jhliq)
        rhgas1=rhgnod(node1)
        rhliq1=rhlnod(node1)
        drgdh1=rghnod(node1)
        drgdp1=rgpnod(node1)
        drldh1=rlhnod(node1)
        drldp1=rlpnod(node1)
c
c  Same for node 2.
c
        kalgas=node2
        kpress=kalgas+numnod
        khgas=kpress+numnod
        khliq=khgas+numnod
        algas2=y(kalgas)
        press2=y(kpress)
        hgas2=y(khgas)
        hliq2=y(khliq)
        rhgas2=rhgnod(node2)
        rhliq2=rhlnod(node2)
        drgdh2=rghnod(node2)
        drgdp2=rgpnod(node2)
        drldh2=rlhnod(node2)
        drldp2=rlpnod(node2)
c
c  Evaluate link quantities which depend on nodal values.
c
        call dolink(algas,algas1,algas2,alliq,box1,box2,drgdh,
     &    drgdh1,drgdh2,drgdp,drgdp1,drgdp2,drldh,drldh1,drldh2,drldp,
     &    drldp1,drldp2,hgas,hgas1,hgas2,hliq,hliq1,hliq2,lnkopt,press,
     &    press1,press2,rhgas,rhgas1,rhgas2,rhliq,rhliq1,rhliq2,vgas,
     &    vliq)
 
        alglnk(i)=algas
        alllnk(i)=alliq
        hglnk(i)=hgas
        hllnk(i)=hliq
        pr1lnk(i)=press1
        pr2lnk(i)=press2
        prslnk(i)=press
        rhglnk(i)=rhgas
        rhllnk(i)=rhliq
        rghlnk(i)=drgdh
        rgplnk(i)=drgdp
        rlhlnk(i)=drldh
        rlplnk(i)=drldp
      enddo
c
c  Ready to compute the residual.
c
      ieqn=0
      ix1=0
      ix2=0

      do i=1,numnod
c
c  Read off the nodal values.
c
        ialgas=i
        ipress=ialgas+numnod
        ihgas=ipress+numnod
        ihliq=ihgas+numnod
 
        algas=y(ialgas)
        alliq=1.0-algas
        press=y(ipress)
        hgas=y(ihgas)
        hliq=y(ihliq)
        rhgas=rhgnod(i)
        rhliq=rhlnod(i)
        drgdp=rgpnod(i)
        drgdh=rghnod(i)
        drldp=rlpnod(i)
        drldh=rhlnod(i)
c
c  1.  Gas continuity equation at node I.
c
        sum1=rhgas*ydot(ialgas)+algas*drgdp*ydot(ipress)
     &    +algas*drgdh*ydot(ihgas)

        sum2=gamnod(i)

        do j=1,nabnod(1,i)

          link=lnknod(1,ix1+j)
          vgas=y(4*numnod+link)

          if(vgas.ne.0.0)then
            algasl=alglnk(link)
            rhgasl=rhglnk(link)
            sum2=sum2-crslnk(link)*algasl*rhgasl*vgas/volnod(i)
          endif

        enddo

        do j=1,nabnod(2,i)

          link=lnknod(2,ix2+j)
          vgas=y(4*numnod+link)

          if(vgas.ne.0.0)then
            algasl=alglnk(link)
            rhgasl=rhglnk(link)
            sum2=sum2+crslnk(link)*algasl*rhgasl*vgas/volnod(i)
          endif

        enddo

        amgnod(i)=sum2

        ieqn=ieqn+1
        res(ieqn)=sum2-sum1
c
c  2.  Liquid continuity equation at node I.
c
c  TAP: why is this YDOT(IALGAS) rather than YDOT(IALLIQ)?
c
        sum1=-rhliq*ydot(ialgas)+alliq*drldp*ydot(ipress)
     &    +alliq*drldh*ydot(ihliq)

        sum2=-gamnod(i)

        do j=1,nabnod(1,i)

          link=lnknod(1,ix1+j)
          vliq=y(4*numnod+numlnk+link)

          if(vliq.ne.0.0)then
            alliql=alllnk(link)
            rhliql=rhllnk(link)
            sum2=sum2-crslnk(link)*alliql*rhliql*vliq/volnod(i)
          endif

        enddo

        do j=1,nabnod(2,i)

          link=lnknod(2,ix2+j)
          vgas=y(4*numnod+numlnk+link)

          if(vliq.ne.0.0)then
            alliql=alllnk(link)
            rhliql=rhllnk(link)
            sum2=sum2+crslnk(link)*alliql*rhliql*vliq/volnod(i)
          endif

        enddo

        amlnod(i)=sum2

        ieqn=ieqn+1
        res(ieqn)=sum2-sum1
c
c  3.  Gas energy equation at node I.
c
        sum1=-pcon1*press*ydot(ialgas)-algas*pcon1*ydot(ipress)
     &    +algas*rhgas*ydot(ihgas)
        sum2=algas*hotnod(i)+0.5*(hgas+hliq-pcon1*press*
     &    (rhgas+rhliq)/(rhgas*rhliq))*gamnod(i) - hgas*amgnod(i)
        
        do j=1,nabnod(1,i)

          link=lnknod(1,ix1+j)
          vgas=y(4*numnod+link)

          if(vgas.ne.0.0)then

            if(lnkopt.eq.0)then
              if(vgas.gt.0.0)then
                plink=pr1lnk(link)
              else
                plink=pr2lnk(link)
              endif
            else
              plink=prslnk(link)
            endif

            algasl=alglnk(link)
            rhgasl=rhglnk(link)
            hgasl=hglnk(link)
            sum2=sum2-crslnk(link)*algasl*
     &        (rhgasl*hgasl+pcon1*(press-plink))*vgas/volnod(i)
          endif

        enddo

        do j=1,nabnod(2,i)

          link=lnknod(2,ix2+j)
          vgas=y(4*numnod+link)

          if(vgas.ne.0.0)then

            if(lnkopt.eq.0)then
              if(vgas.gt.0.0)then
                plink=pr1lnk(link)
              else
                plink=pr2lnk(link)
              endif
            else
              plink=prslnk(link)
            endif

            algasl=alglnk(link)
            rhgasl=rhglnk(link)
            hgasl=hglnk(link)
            sum2=sum2+crslnk(link)*algasl*
     &        (rhgasl*hgasl+pcon1*(press-plink))*vgas/volnod(i)
          endif

        enddo

        ieqn=ieqn+1
        res(ieqn)=sum2-sum1
c
c  4.  Liquid energy equation at node I.
c
        sum1=+pcon1*press*ydot(ialgas)-alliq*pcon1*ydot(ipress)
     &    +alliq*rhliq*ydot(ihliq)
        sum2=alliq*hotnod(i)-0.5*(hgas+hliq-pcon1*press*
     &    (rhgas+rhliq)/(rhgas*rhliq))*gamnod(i) - hliq*amlnod(i)
        
        do j=1,nabnod(1,i)

          link=lnknod(1,ix1+j)
          vliq=y(4*numnod+numlnk+link)

          if(vliq.ne.0.0)then

            if(lnkopt.eq.0)then
              if(vliq.gt.0.0)then
                plink=pr1lnk(link)
              else
                plink=pr2lnk(link)
              endif
            else
              plink=prslnk(link)
            endif

            alliql=alllnk(link)
            rhliql=rhllnk(link)
            hliql=hllnk(link)
            sum2=sum2-crslnk(link)*alliql*
     &        (rhliql*hliql+pcon1*(press-plink))*vliq/volnod(i)
          endif

        enddo

        do j=1,nabnod(2,i)

          link=lnknod(2,ix2+j)
          vliq=y(4*numnod+numlnk+link)

          if(vliq.ne.0.0)then

            if(lnkopt.eq.0)then
              if(vliq.gt.0.0)then
                plink=pr1lnk(link)
              else
                plink=pr2lnk(link)
              endif
            else
              plink=prslnk(link)
            endif

            alliql=alllnk(link)
            rhliql=rhllnk(link)
            hliql=hllnk(link)
            sum2=sum2+crslnk(link)*alliql*
     &        (rhliql*hliql+pcon1*(press-plink))*vliq/volnod(i)
          endif

        enddo

        ieqn=ieqn+1
        res(ieqn)=sum2-sum1

      enddo
c
c  Now do equations at links.
c
      do i=1,numlnk
 
        ivgas=4*numnod+i
        ivliq=ivgas+numlnk
        vgas=y(ivgas)
        vliq=y(ivliq)

        node1=nodlnk(1,i)
        node2=nodlnk(2,i)
        box1=boxlnk(1,i)
        box2=boxlnk(2,i)
        if(vgas.ge.0.0)then
          igdonr=node1
        else
          igdonr=node2
        endif
        if(vliq.ge.0.0)then
          ildonr=node1
        else
          ildonr=node2
        endif
        jalgas=node1
        jpress=jalgas+numnod
        jhgas=jpress+numnod
        jhliq=jhgas+numnod
        algas1=y(jalgas)
        hgas1=y(jhgas)
        hliq1=y(jhliq)
        rhgas1=rhgnod(node1)
        rhliq1=rhlnod(node1)
        gamma1=gamnod(node1)
        poten1=potnod(node1)

        kalgas=node2
        kpress=kalgas+numnod
        khgas=kpress+numnod
        khliq=khgas+numnod
        algas2=y(kalgas)
        hgas2=y(khgas)
        hliq2=y(khliq)
        rhgas2=rhgnod(node2)
        rhliq2=rhlnod(node2)
        gamma2=gamnod(node2)
        poten2=potnod(node2)

        algas=alglnk(i)
        alliq=alllnk(i)
        rhgas=rhglnk(i)
        rhliq=rhllnk(i)
c
c  Evaluate friction terms.
c
        fgw=sqrtpi*rhgas*fricgw*algas/(4.0*crslnk(i))
        fgl=3.0*rhcrit*rhgas*fricgl*algas*(vgas-vliq)**2 /
     &    (4.0*0.06147*wcrit*scrit)
        flw=sqrtpi*rhliq*friclw*alliq/(4.0*crslnk(i))
c
c  5.  Gas momentum equation at link I.
c
        sum1=algas*rhgas*ydot(ivgas)

        sum2=0.25*vliq*(gamma1+gamma2)-fgw*abs(vgas)*vgas
     &    -fgl*abs(vgas-vliq)*(vgas-vliq)

        sum2=sum2-algas*crslnk(i)*(pcon2*(press2-press1)
     &    +rhgas*(poten2-poten1))/vollnk(i)

        if(lnkopt.eq.0)then
          sum2=sum2-0.5*vgas*amgnod(igdonr)
        else
          sum1=sum1+0.5*vgas*rhgas*
     &      (box1*ydot(jalgas)+box2*ydot(kalgas))/(box1+box2)
          sum1=sum1+0.25*vgas*algas*rgplnk(i)*
     &      (ydot(jpress)+ydot(kpress))
          if(igdonr.eq.node1)then
            sum1=sum1+0.5*vgas*algas*rghlnk(i)*ydot(jhgas)
          else
            sum1=sum1+0.5*vgas*algas*rghlnk(i)*ydot(khgas)
          endif
        endif

        ieqn=ieqn+1
        res(ieqn)=sum2-sum1
c
c  6.  Liquid momentum equation at link I.
c
        sum1=alliq*rhliq*ydot(ivliq)

        sum2=0.25*vgas*(gamma1+gamma2)-flw*abs(vliq)*vliq
     &    +fgl*abs(vgas-vliq)*(vgas-vliq)

        sum2=sum2-alliq*crslnk(i)*(pcon2*(press2-press1)
     &    +rhliq*(poten2-poten1))/vollnk(i)

        if(lnkopt.eq.0)then
          sum2=sum2-0.5*vliq*amlnod(ildonr)
        else
          sum1=sum1-0.5*vliq*rhliq*
     &      (box1*ydot(jalgas)+box2*ydot(kalgas))/(box1+box2)
          sum1=sum1+0.25*vliq*alliq*rlplnk(i)*
     &      (ydot(jpress)+ydot(kpress))
          if(ildonr.eq.node1)then
            sum1=sum1+0.5*vliq*alliq*rlhlnk(i)*ydot(jhliq)
          else
            sum1=sum1+0.5*vliq*alliq*rlhlnk(i)*ydot(khliq)
          endif
        endif

        ieqn=ieqn+1
        res(ieqn)=sum2-sum1

      enddo
 
      return
      end
      subroutine writer(boxlnk,crslnk,fricgl,fricgw,friclw,gravit,
     &  hotnod,isave,istate,iwork,liw,lnkopt,lrw,
     &  maxeqn,maxlnk,maxnod,nodlnk,numeqn,numlnk,numnod,posnod,
     &  rhcrit,rsave,rwork,scrit,time,title,vollnk,volnod,wcrit,
     &  y,ydot)
c
c*********************************************************************72
c
cc WRITER ...
c
      integer liw
      integer lrw
      integer maxeqn
      integer maxlnk
      integer maxnod
c
      real boxlnk(2,maxlnk)
      real crslnk(maxlnk)
      character*80 filew
      real fricgl
      real fricgw
      real friclw
      real gravit(3)
      real hotnod(maxnod)
      integer i
      integer isave(36)
      integer istate
      integer iwork(liw)
      integer job
      integer lnkopt
      integer nodlnk(2,maxlnk)
      integer numeqn
      integer numlnk
      integer numnod
      real posnod(3,maxnod)
      real rhcrit
      real rsave(218)
      real rwork(lrw)
      real scrit
      real time
      character*80 title
      real vollnk(maxlnk)
      real volnod(maxnod)
      real wcrit
      real y(maxeqn)
      real ydot(maxeqn)
c
      write(*,*)' '
      write(*,*)'Enter name of restart file to create,'
      write(*,*)'or RETURN for no restart file.'
      read(*,'(a)')filew
c
c  Get data from LSODI.
c
      job=1
      call srcom(rsave,isave,job)
c
      open(unit=2,file=filew,status='unknown',err=10)
      close(unit=2,dispose='delete')
c
      open(unit=2,file=filew,status='new',err=10)

      write(2,'(a80)')title

      write(2,*)numeqn
      write(2,*)numlnk
      write(2,*)numnod
      write(2,*)liw
      write(2,*)lrw

      do i=1,numlnk
        write(2,*)boxlnk(1,i)
      enddo

      do i=1,numlnk
        write(2,*)boxlnk(2,i)
      enddo

      do i=1,numlnk
        write(2,*)crslnk(i)
      enddo

      write(2,*)fricgl
      write(2,*)fricgw
      write(2,*)friclw

      do i=1,3
        write(2,*)gravit(i)
      enddo

      do i=1,numnod
        write(2,*)hotnod(i)
      enddo

      do i=1,36
        write(2,*)isave(i)
      enddo

      write(2,*)istate

      do i=1,liw
        write(2,*)iwork(i)
      enddo

      write(2,*)lnkopt

      do i=1,numlnk
        write(2,*)nodlnk(1,i)
      enddo

      do i=1,numlnk
        write(2,*)nodlnk(2,i)
      enddo

      do i=1,numnod
        write(2,*)posnod(1,i)
      enddo

      do i=1,numnod
        write(2,*)posnod(2,i)
      enddo

      do i=1,numnod
        write(2,*)posnod(3,i)
      enddo

      write(2,*)rhcrit

      do i=1,218
        write(2,*)rsave(i)
      enddo

      do i=1,lrw
        write(2,*)rwork(i)
      enddo

      write(2,*)scrit

      write(2,*)time

      do i=1,numlnk
        write(2,*)vollnk(i)
      enddo

      do i=1,numnod
        write(2,*)volnod(i)
      enddo

      write(2,*)wcrit

      do i=1,numeqn
        write(2,*)y(i)
      enddo

      do i=1,numeqn
        write(2,*)ydot(i)
      enddo

      close(unit=2)

      return
c
c  Error opening the file.
c
10    continue
      write(*,*)' '
      write(*,*)'Writer - Warning!'
      write(*,*)'  The output file could not be created.'
      write(*,*)'  Your data was NOT saved!'
      return
      end
      subroutine input(disjac,fileg,filet,ibump,icon,icorr,idfd,ids,
     &  ierror,
     &  ifds,igrad,igrid,ijac,iopt,iplot,ipred,ishapb,ishapbt,ishapf,
     &  ishapft,istep1,istep2,itar,itype,
     &  iwrite,jjac,jstep1,jstep2,kjac,maxnew,maxpar,maxsim,
     &  maxstp,npar,
     &  nparb,nparf,nquad,nstep3,nx,ny,para1,para2,para3,partar,remax,
     &  stpmax,tolnew,tolopt,tolsim,wateb,wateb1,wateb2,
     &  watei,
     &  watep,water,wateu,watev,xbleft,xbltar,xbrite,xbrtar,xlngth,
     &  xprof,ybleft,ybltar,ybrite,ybrtar,ylngth)
c
c*********************************************************************72
c
c  INPUT takes symbolic commands of the form "name=value" and
c  carries them out, assigning values to program variables.
c
c
c  For information on the meaning and legal values of the variables,
c  please refer to the glossary!
c
      implicit double precision (a-h,o-z)
c
      integer maxpar
c
      double precision disjac
      character*30 fileg
      character*30 filet
      integer ibump
      integer icon
      integer icorr
      integer idfd
      integer ids
      integer ierror
      integer ifds
      integer igrad
      integer igrid
      integer ijac
      integer iopt(maxpar)
      integer iplot
      integer ipred
      integer ishapb
      integer ishapbt
      integer ishapf
      integer ishapft
      integer istep1
      integer istep2
      integer itar
      integer itype
      integer ival
      integer iwrite
      integer jjac
      integer jstep1
      integer jstep2
      integer kjac
      integer lchar
      integer lchar2
      integer lenchr
      logical leqi
      integer maxnew
      integer maxsim
      integer maxstp
      character*80 name
      integer npar
      integer nparb
      integer nparf
      integer nquad
      integer nrec
      integer nstep3
      integer nx
      integer ny
      double precision para1(maxpar)
      double precision para2(maxpar)
      double precision para3(maxpar)
      double precision partar(maxpar)
      double precision remax
      character*80 rhs
      double precision stpmax
      double precision tolnew
      double precision tolopt
      double precision tolsim
      double precision value
      double precision wateb
      double precision wateb1
      double precision wateb2
      double precision watei
      double precision watep
      double precision water
      double precision wateu
      double precision watev
      double precision xbleft
      double precision xbltar
      double precision xbrite
      double precision xbrtar
      double precision xlngth
      double precision xprof
      double precision ybleft
      double precision ybltar
      double precision ybrite
      double precision ybrtar
      double precision ylngth
c
      intrinsic int
      external lenchr
      intrinsic lge
      intrinsic lle
c
      write(*,*)' '
      write(*,*)'Input: Read control information from user.'
      write(*,*)' '
 
      nrec=0
c
c  Read the next line of input.
c
10    continue
 
      call namels(name,ierror,rhs,value)
 
      lchar=lenchr(name)
 
      if(ierror.eq.0)then
c
c  Echo the input line.  If the input quantity had an "integer"
c  name, then print it as an integer.
c
        if(leqi(name(1:4),'file'))then
          lchar2=lenchr(rhs)
          write(*,'(a,''='',a)')name(1:lchar),rhs(1:lchar2)
        elseif((lge(name(1:1),'I').and.lle(name(1:1),'N')).or.
     &    (lge(name(1:1),'i').and.lle(name(1:1),'n')))then
          write(*,'(a,''='',i14)')name(1:lchar),int(value)
        else
          write(*,'(a,''='',g14.6)')name(1:lchar),value
        endif
 
      if(leqi(name,'disjac'))then
        disjac=value
      elseif(leqi(name,'fileg'))then
        fileg=rhs(1:30)
      elseif(leqi(name,'filet'))then
        filet=rhs(1:30)
      elseif(leqi(name,'ibump'))then
        ibump=int(value)
      elseif(leqi(name,'icon'))then
        icon=int(value)
      elseif(leqi(name,'icorr'))then
        icorr=int(value)
      elseif(leqi(name,'icost'))then
        write(*,*)'ICOST is not used any more.'
      elseif(leqi(name,'idfd'))then
        idfd=int(value)
      elseif(leqi(name,'ids'))then
        ids=int(value)
      elseif(leqi(name,'ifds'))then
        ifds=int(value)
      elseif(leqi(name,'igrad'))then
        igrad=int(value)
      elseif(leqi(name,'igrid'))then
        igrid=int(value)
      elseif(leqi(name,'ijac'))then
        ijac=int(value)
      elseif(leqi(name(1:5),'iopt('))then
 
        call chrcti(name(6:),ival,ierror,lchar)
 
        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'Input - Fatal error!'
          write(*,*)'  ChrCTI returned nonzero error flag!'
          stop
        endif
 
        if(ival.lt.1.or.ival.gt.maxpar)then
          write(*,*)' '
          write(*,*)'Input - Fatal error!'
          write(*,*)'  Index of IOPT is out of bounds!'
          stop
        endif
 
        iopt(ival)=int(value)
 
      elseif(leqi(name,'iplot'))then
        iplot=int(value)
      elseif(leqi(name,'ipred'))then
        ipred=int(value)
      elseif(leqi(name,'ishapb'))then
        ishapb=int(value)
      elseif(leqi(name,'ishapbt'))then
        ishapbt=int(value)
      elseif(leqi(name,'ishapf'))then
        ishapf=int(value)
      elseif(leqi(name,'ishapft'))then
        ishapft=int(value)
      elseif(leqi(name,'istep1'))then
        istep1=int(value)
      elseif(leqi(name,'istep2'))then
        istep2=int(value)
      elseif(leqi(name,'itar'))then
        itar=int(value)
      elseif(leqi(name,'itype'))then
        itype=int(value)
      elseif(leqi(name,'iwrite'))then
        iwrite=int(value)
      elseif(leqi(name,'jjac'))then
        jjac=int(value)
      elseif(leqi(name,'jstep1'))then
        jstep1=int(value)
      elseif(leqi(name,'jstep2'))then
        jstep2=int(value)
      elseif(leqi(name,'kjac'))then
        kjac=int(value)
      elseif(leqi(name,'maxnew'))then
        maxnew=int(value)
      elseif(leqi(name,'maxsim'))then
        maxsim=int(value)
      elseif(leqi(name,'maxstp'))then
        maxstp=int(value)
      elseif(leqi(name,'nparb'))then
        nparb=int(value)
        npar=nparf+nparb+1
      elseif(leqi(name,'nparf'))then
        nparf=int(value)
        npar=nparf+nparb+1
      elseif(leqi(name,'nquad'))then
        nquad=int(value)
      elseif(leqi(name,'nx'))then
        nx=int(value)
      elseif(leqi(name,'ny'))then
        ny=int(value)
      elseif(leqi(name,'nstep3'))then
        nstep3=int(value)
      elseif(leqi(name(1:6),'para1('))then
 
        call chrcti(name(7:),ival,ierror,lchar)
 
        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'Input - Fatal error!'
          write(*,*)'  ChrCTI returned nonzero error flag!'
          stop
        endif
 
        if(ival.lt.1.or.ival.gt.maxpar)then
          write(*,*)' '
          write(*,*)'Input - Fatal error!'
          write(*,*)'  Index of PARA1 is out of bounds!'
          stop
        endif
 
        para1(ival)=value
 
      elseif(leqi(name(1:6),'para2('))then
 
        call chrcti(name(7:),ival,ierror,lchar)
 
        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'Input - Fatal error!'
          write(*,*)'  ChrCTI returned nonzero error flag!'
          stop
        endif
 
        if(ival.lt.1.or.ival.gt.maxpar)then
          write(*,*)' '
          write(*,*)'Input - Fatal error!'
          write(*,*)'  Index of PARA2 is out of bounds!'
          stop
        endif
 
        para2(ival)=value
 
      elseif(leqi(name(1:6),'para3('))then
 
        call chrcti(name(7:),ival,ierror,lchar)
 
        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'Input - Fatal error!'
          write(*,*)'  ChrCTI returned nonzero error flag!'
          stop
        endif
 
        if(ival.lt.1.or.ival.gt.maxpar)then
          write(*,*)' '
          write(*,*)'Input - Fatal error!'
          write(*,*)'  Index of PARA3 is out of bounds!'
          stop
        endif
 
        para3(ival)=value
 
      elseif(leqi(name(1:7),'partar('))then
 
        call chrcti(name(8:),ival,ierror,lchar)
 
        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'Input - Fatal error!'
          write(*,*)'  ChrCTI returned nonzero error flag!'
          stop
        endif
 
        if(ival.lt.1.or.ival.gt.maxpar)then
          write(*,*)' '
          write(*,*)'Input - Fatal error!'
          write(*,*)'  Index of PARTAR is out of bounds!'
          stop
        endif
 
        partar(ival)=value
 
      elseif(leqi(name,'remax'))then
        remax=value
      elseif(leqi(name,'stpmax'))then
        stpmax=value
      elseif(leqi(name,'tolnew'))then
        tolnew=value
      elseif(leqi(name,'tolopt'))then
        tolopt=value
      elseif(leqi(name,'tolsim'))then
        tolsim=value
      elseif(leqi(name,'wateu'))then
        wateu=value
      elseif(leqi(name,'watev'))then
        watev=value
      elseif(leqi(name,'watep'))then
        watep=value
      elseif(leqi(name,'water'))then
        water=value
      elseif(leqi(name,'watei'))then
        watei=value
      elseif(leqi(name,'wateb'))then
        wateb=value
      elseif(leqi(name,'wateb1'))then
        wateb1=value
      elseif(leqi(name,'wateb2'))then
        wateb2=value
      elseif(leqi(name,'xbleft'))then
        xbleft=value
      elseif(leqi(name,'xbltar'))then
        xbltar=value
      elseif(leqi(name,'xbrite'))then
        xbrite=value
      elseif(leqi(name,'xbrtar'))then
        xbrtar=value
      elseif(leqi(name,'xlngth'))then
        xlngth=value
      elseif(leqi(name,'xprof'))then
        xprof=value
      elseif(leqi(name,'ybleft'))then
        ybleft=value
      elseif(leqi(name,'ybltar'))then
        ybltar=value
      elseif(leqi(name,'ybrite'))then
        ybrite=value
      elseif(leqi(name,'ybrtar'))then
        ybrtar=value
      elseif(leqi(name,'ylngth'))then
        ylngth=value
c
c  Unknown name.
c
        else
          write(*,*)' '
          write(*,*)'Input - Unknown variable!'
          write(*,*)'  Variable name='//name(1:lchar)
          write(*,*)'  Assigned value=',value
          write(*,*)' '
        endif
c
c  IERROR=2, possible "STOP" or "GO" statement.
c
      elseif(ierror.eq.2)then
        if(leqi(name,'go'))then
          write(*,*)' '
          write(*,*)'GO command!'
          ierror=0
          return
        elseif(leqi(name,'stop'))then
          ierror=1
          write(*,*)'STOP command!'
          return
        else
          write(*,*)' '
          write(*,*)'Input - Fatal error!'
          write(*,*)'  NameLS error of type ',ierror
          write(*,*)'  Card follows:'
          write(*,'(a)')name
          write(*,*)' '
          stop
        endif
c
c  IERROR=1, blank line.
c
      elseif(ierror.eq.1)then
        write(*,*)' '
c
c  IERROR=3, or 4, miscellaneous error.
c
      elseif(ierror.eq.3.or.ierror.eq.4)then
        write(*,*)' '
        write(*,*)'Input - Warning!'
        write(*,*)'  NameLS error of type ',ierror
        write(*,*)' '
c
c  IERROR=6, comment.
c
      elseif(ierror.eq.6)then
        write(*,'(a)')name(1:lchar)
c
c  IERROR=5, hard end of input.
c
      elseif(ierror.eq.5)then
        write(*,*)' '
        write(*,*)'Input - Warning!'
        write(*,*)'  "Hard" end of input.'
        write(*,*)'  A total of ',nrec,' records were read.'
        write(*,*)' '
        return
c
c  IERROR=7, "soft" end of input.
c
      elseif(ierror.eq.7)then
        write(*,*)' '
        write(*,*)'Input - Warning!'
        write(*,*)'  "Soft" end of input.'
        write(*,*)'  A total of ',nrec,' records were read.'
        return
c
c  Unrecognized error.
c
      else
        write(*,*)' '
        write(*,*)'Input - Fatal error!'
        write(*,*)'  Unrecognized error from NameLS.'
        write(*,*)'  IERROR=',ierror
        write(*,*)'  Forcing a STOP!'
        stop
      endif
 
      nrec=nrec+1
      go to 10
      end
      function lenchr(string)
c
c*********************************************************************72
c
c  LENCHR returns the length of STRING up to the last nonblank character.
c
c  STRING Input, CHARACTER*(*) STRING, the string to be measured.
c
c  LENCHR Output, integer LENCHR, the location of the last nonblank
c         character in STRING.
c
      implicit double precision(a-h,o-z)
c
      integer i
      integer lenchr
      character*(*) string
c
      intrinsic char
      intrinsic len
c
      do i=len(string),1,-1
 
        if(string(i:i).ne.' '.and.
     &     string(i:i).ne.char(0))then
          lenchr=i
          return
        endif
 
      enddo
 
      lenchr=0
 
      return
      end
      function leqi(strng1,strng2)
c
c*********************************************************************72
c
c  LEQI is a case insensitive comparison of two strings for
c  equality.  Thus, LEQI('Anjana','ANJANA') is .TRUE.
c
c
c  STRNG1,
c  STRNG2 Input, CHARACTER*(*) STRNG1, STRNG2, the strings to
c         compare.
c
c  LEQI   Output, LOGICAL LEQI, the result of the comparison.
c
      implicit double precision(a-h,o-z)
c
      integer i
      integer len1
      integer len2
      integer lenc
      logical leqi
      character*1 s1
      character*1 s2
      character*(*) strng1
      character*(*) strng2
c
      intrinsic len
      intrinsic min
c
      leqi=.false.
 
      len1=len(strng1)
      len2=len(strng2)
      lenc=min(len1,len2)
 
      do i=1,lenc
        s1=strng1(i:i)
        s2=strng2(i:i)
        call capchr(s1)
        call capchr(s2)
        if(s1.ne.s2)return
      enddo
 
      if(len1.gt.lenc.and.strng1(lenc+1:len1).ne.' ')return
      if(len2.gt.lenc.and.strng2(lenc+1:len2).ne.' ')return
      leqi=.true.
 
      return
      end
      subroutine namels(name,ierror,rhs,value)
c
c*********************************************************************72
c
c  NAMELS reads a line of user input which is similar in form
c  to NAMELIST input, and returns the name of the variable
c  and its value.
c
c  NAMELS is a simple program, and can only handle simple input.
c  In particular, it cannot handle:
c
c    multiple assignments on one line,
c    a single assignment extended over multiple lines,
c    assignments to character or complex variables,
c    assignments to arrays.
c
c  Typical input would be of the form:
c
c    name = value
c
c  including, for instance:
c
c    a = 1.0
c    n=-17
c    scale = +5.3E-2
c
c  Spaces are ignored, and case is not important.  Integer values
c  will be returned as double precision, but this is never a
c  problem as long as the integers are "small".
c
c  If a line begins with the character "#", it is assumed to be
c  a comment, and is ignored.  IERROR is returned as 6.
c
c  If a line begins with the characters "end-of-input", it is
c  assumed to be an "end-of-input" marker, and IERROR is returned
c  as 7.
c
c
c  NAME   Output, character*(*) NAME.
c
c         NAME contains the left hand side of the assignment
c         statement.
c
c         Normally, this will be the name of a variable.
c
c         If the input line was blank, then NAME will equal ' '.
c
c         If an error occurred while trying to process the
c         input line, NAME will contain the text of the line..
c
c         If the line began with "#", then NAME will contain the
c         text of the line.
c
c         If the line equals "end-of-input", then NAME will contain
c         the text of the line.
c
c  IERROR Output, integer IERROR.
c
c         0, no errors were detected.
c         1, the line was blank.
c         2, the line did not contain an "=" sign.
c         3, the line did not contain a variable name to the
c            left of the "=" sign.
c         4, the right hand side of the assignment did not make
c            sense.
c         5, end of input.
c         6, the line began with "#", signifying a comment.
c            The text of the line is returned in NAME.
c         7, the line began with "end-of-input".
c
c  VALUE  Output, double precision VALUE.
c
c         VALUE contains the right hand side of the assignment
c         statement.
c
c         Normally, this will be a double precision value.
c
c         But if the input line was blank, or if an error occurred
c         while trying to process the input line, or if input
c         terminated, then VALUE will simply be set to 0.
c
      integer ierror
      integer lchar
      integer lenchr
      logical leqi
      character*80 line
      character*(*) name
      integer nchar
      character*80 rhs
      real value
c
      intrinsic index
      external lenchr
      external leqi
c
c  Set default values
c
      ierror=0
      name=' '
      value=0.0
c
c  Read a line
c
      read(*,'(a)',end=20)line
c
c  Empty lines are OK
c
      if(lenchr(line).le.0)then
        ierror=1
        return
      endif
c
c  Check for comment.
c
      if(line(1:1).eq.'#')then
        ierror=6
        name=line
        return
      endif
c
c  Check for "end-of-line".
c
      if(leqi(line,'end-of-input'))then
        ierror=7
        name=line
        return
      endif
c
c  Does the line contain an = sign?
c
      if(index(line,'=').le.0)then
        ierror=2
        value=0
        name=line
        return
      endif
c
c  Find the name of the variable to be assigned.
c
      call chrup2(line,name,'=')
      call chrdb1(name)
      if(lenchr(name).le.0)then
        write(*,*)' '
        write(*,*)'NameLS - Warning!'
        write(*,*)'  The following input line was ignored, because'
        write(*,*)'  there was no variable name on the left hand'
        write(*,*)'  side of the assignment statement:'
        write(*,'(a)')line
        write(*,*)' '
        ierror=3
        return
      endif
c
c  Read the value, as a double precision number.
c
      nchar=index(line,'=')
 
      rhs=line(nchar+1:)
 
      if(leqi(name(1:4),'file'))return
 
      call chrctd(line(nchar+1:),value,ierror,lchar)
      if(ierror.ne.0)then
        write(*,*)' '
        write(*,*)'NameLS - Warning!'
        write(*,*)'  The following input line was ignored, because'
        write(*,*)'  the right hand side of the assignment statement'
        write(*,*)'  did not seem to make sense:'
        write(*,'(a)')line
        write(*,*)' '
        ierror=4
      endif
      return
c
c  On end of input, return.
c
20    continue
      write(*,*)' '
      write(*,*)'NameLS - Reached end of input.'
      write(*,*)' '
      ierror=5
 
      return
      end
      subroutine chrcti(string,intval,ierror,lchar)
c
c*********************************************************************72
c
c  CHRCTI accepts a STRING of characters and reads an integer
c  from STRING into INTVAL.  The STRING must begin with an integer
c  but that may be followed by other information.
c
c  CHRCTI will read as many characters as possible until it reaches
c  the end of the STRING, or encounters a character which cannot be
c  part of the number.
c
c  Legal input is
c
c    blanks,
c    initial sign,
c    integer part,
c    blanks,
c    final comma,
c
c  with most quantities optional.
c
c
c  STRING Input, CHARACTER*(*) STRING, the string containing the
c         data to be read.  Reading will begin at position 1 and
c         terminate at the end of the string, or when no more
c         characters can be read to form a legal integer.  Blanks,
c         commas, or other nonnumeric data will, in particular,
c         cause the conversion to halt.
c
c         Sample results:
c
c         STRING            INTVAL
c
c         '1'               1
c         '     1   '       1
c         '1A'              1
c         '12,34,56'        12
c         '  34 7'          34
c         '-1E2ABCD'        -100
c         '-1X2ABCD'        -1
c         ' 2E-1'           0
c         '23.45'           23
c
c  INTVAL Output, integer INTVAL, the integer read from the string.
c
c  IERROR Output, integer IERROR, error flag.
c         0 if no errors,
c         Value of IHAVE when error occurred otherwise.
c
c  LCHAR  Output, integer LCHAR, number of characters read from
c         STRING to form the number.
c
      character*1 chrtmp
      integer ierror
      integer ihave
      integer intval
      integer isgn
      integer iterm
      integer itop
      integer lchar
      integer nchar
      integer ndig
      character*(*) string
c
      intrinsic len
      intrinsic lge
      intrinsic lle
c
      nchar=len(string)
 
      ierror=0
      intval=0
      lchar=-1
      isgn=1
      itop=0
      ihave=1
      iterm=0
10    continue
      lchar=lchar+1
      chrtmp=string(lchar+1:lchar+1)
 
      if(chrtmp.eq.' ')then
        if(ihave.eq.2)then
          iterm=1
        elseif(ihave.eq.3)then
          ihave=11
        endif
      elseif(chrtmp.eq.',')then
        if(ihave.ne.1)then
          iterm=1
          ihave=12
          lchar=lchar+1
        endif
      elseif(chrtmp.eq.'-')then
        if(ihave.eq.1)then
          ihave=2
          isgn=-1
        else
          iterm=1
        endif
      elseif(chrtmp.eq.'+')then
        if(ihave.eq.1)then
          ihave=2
        else
          iterm=1
        endif
      elseif(lge(chrtmp,'0').and.lle(chrtmp,'9').and.ihave.lt.11)then
        ihave=3
        read(chrtmp,'(i1)')ndig
        itop=10*itop+ndig
      else
        iterm=1
      endif
 
      if(iterm.ne.1.and.lchar+1.lt.nchar)go to 10
      if(iterm.ne.1.and.lchar+1.eq.nchar)lchar=nchar
c
c  Number seems to have terminated.  Have we got a legal number?
c
      if(ihave.eq.1.or.ihave.eq.2)then
        ierror=ihave
        write(*,*)' '
        write(*,*)'ChrCTI - Fatal error!'
        write(*,*)'  IERROR=',ierror
        write(*,*)'  Illegal or nonnumeric input:'
        write(*,'(1x,a)')string
        return
      endif
c
c  Number seems OK.  Form it.
c
      intval=isgn*itop
      return
      end
      subroutine chrdb1(string)
c
c*********************************************************************72
c
c  CHRDB1 accepts a string of characters and removes all
c  blanks and nulls, left justifying the remainder and padding with
c  blanks.
c
c
c  STRING Input/output, CHARACTER*(*) STRING, the string to be
c         transformed.
c
      character*1 chrtmp
      integer i
      integer j
      integer nchar
      character*(*) string
c
      intrinsic char
      intrinsic len
c
      nchar=len(string)
 
      j=0
 
      do i=1,nchar
 
        chrtmp=string(i:i)
        string(i:i)=' '
 
        if(chrtmp.ne.' '.and.chrtmp.ne.char(0))then
          j=j+1
          string(j:j)=chrtmp
        endif
 
      enddo
 
      return
      end
      subroutine chrup2(string,strng2,strng3)
c
c*********************************************************************72
c
c  CHRUP2 copies STRING into STRNG2, up to, but not including, the
c  first occurrence of the string STRNG3.  Setting STRING='ABCDEFGH'
c  and STRNG3='EF' results in STRNG2='ABCD'.
c
c
c  STRING Input, CHARACTER*(*) STRING, the string to be copied.
c
c  STRNG2 Output, CHARACTER*(*) STRNG2, the copied portion of
c         STRING.
c
c  STRNG3 Input, CHARACTER*(*) STRNG3, the 'flag' string at which
c         the copy stops.
c
      integer i
      integer len1
      integer len2
      integer len3
      character*(*) string
      character*(*) strng2
      character*(*) strng3
c
      intrinsic len
c
      len1=len(string)
      len2=len(strng2)
      len3=len(strng3)
 
      strng2=' '
      i=0
10    continue
      i=i+1
      if(i.gt.len1)return
      if(i.gt.len2)return
 
      if(i+len3-1.le.len1)then
        if(string(i:i+len3-1).eq.strng3)return
      endif
 
      strng2(i:i)=string(i:i)
      go to 10
 
      end
      subroutine capchr(string)
c
c*********************************************************************72
c
c  CAPCHR accepts a STRING of characters and replaces any lowercase
c  letters by uppercase ones.
c
c
c  STRING Input/output, CHARACTER*(*) STRING, the string of
c         characters to be transformed.
c
      integer i
      integer itemp
      integer nchar
      character*(*) string
c
      intrinsic char
      intrinsic ichar
      intrinsic len
c
      nchar=len(string)
 
      do i=1,nchar
 
        itemp=ichar(string(i:i))
        if(97.le.itemp.and.itemp.le.122)then
          string(i:i)=char(itemp-32)
        endif
 
      enddo
 
      return
      end
      subroutine chrctd(string,dval,ierror,lchar)
c
c*********************************************************************72
c
c  CHRCTD accepts a string of characters, and tries to extract a
c  double precision real number from the initial part of the
c  string.
c
c  CHRCTD will read as many characters as possible until it reaches
c  the end of the string, or encounters a character which cannot be
c  part of the number.
c
c  Legal input is:
c
c     1 blanks,
c     2 '+' or '-' sign,
c     3 integer part,
c     4 decimal point,
c     5 fraction part,
c     6 'E' or 'e' or 'D' or 'd', exponent marker,
c     7 exponent sign,
c     8 exponent integer part,
c     9 exponent decimal point,
c    10 exponent fraction part,
c    11 blanks,
c    12 final comma,
c
c  with most quantities optional.
c
c  Examples:
c
c    STRING            DVAL
c
c    '1'               1.0
c    '     1   '       1.0
c    '1A'              1.0
c    '12,34,56'        12.0
c    '  34 7'          34.0
c    '-1E2ABCD'        -100.0
c    '-1X2ABCD'        -1.0
c    ' 2E-1'           0.2
c    '23.45'           23.45
c    '-4.2E+2'         -420.0
c    '17d2'            1700.0
c    '-14e-2'         -0.14
c    'e2'              100.0
c    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
c
c
c  STRING Input, CHARACTER*(*) STRING, the string containing the
c         data to be read.  Reading will begin at position 1 and
c         terminate at the end of the string, or when no more
c         characters can be read to form a legal real.  Blanks,
c         commas, or other nonnumeric data will, in particular,
c         cause the conversion to halt.
c
c  DVAL   Output, double precision DVAL, the value that was read
c         from the string.
c
c  IERROR Output, integer IERROR, error flag.
c
c         0, no errors occurred.
c
c         1, 2, 6 or 7, the input number was garbled.  The
c         value of IERROR is the last type of input successfully
c         read.  For instance, 1 means initial blanks, 2 means
c         a plus or minus sign, and so on.
c
c  LCHAR  Output, integer LCHAR, the number of characters read from
c         STRING to form the number, including any terminating
c         characters such as a trailing comma or blanks.
c
      implicit double precision (a-h,o-z)
c
      character*1 chrtmp
      double precision dval
      integer ierror
      integer ihave
      integer isgn
      integer iterm
      integer jbot
      integer jsgn
      integer jtop
      integer lchar
      logical leqi
      integer nchar
      integer ndig
      double precision rbot
      double precision rexp
      double precision rtop
      character*(*) string
c
      intrinsic dble
      intrinsic len
      external leqi
      intrinsic lge
      intrinsic lle
c
      nchar=len(string)
 
      ierror=0
      dval=0.0d0
      lchar=-1
      isgn=1
      rtop=0.0d0
      rbot=1.0d0
      jsgn=1
      jtop=0
      jbot=1
      ihave=1
      iterm=0
 
10    continue
      lchar=lchar+1
      chrtmp=string(lchar+1:lchar+1)
c
c  Blank character.
c
      if(chrtmp.eq.' ')then
        if(ihave.eq.2.or.ihave.eq.6.or.ihave.eq.7)then
          iterm=1
        elseif(ihave.gt.1)then
          ihave=11
        endif
c
c  Comma
c
      elseif(chrtmp.eq.',')then
        if(ihave.ne.1)then
          iterm=1
          ihave=12
          lchar=lchar+1
        endif
c
c  Minus sign.
c
      elseif(chrtmp.eq.'-')then
        if(ihave.eq.1)then
          ihave=2
          isgn=-1
        elseif(ihave.eq.6)then
          ihave=7
          jsgn=-1
        else
          iterm=1
        endif
c
c  Plus sign.
c
      elseif(chrtmp.eq.'+')then
        if(ihave.eq.1)then
          ihave=2
        elseif(ihave.eq.6)then
          ihave=7
        else
          iterm=1
        endif
c
c  Decimal point.
c
      elseif(chrtmp.eq.'.')then
        if(ihave.lt.4)then
          ihave=4
        elseif(ihave.ge.6.and.ihave.le.8)then
          ihave=9
        else
          iterm=1
        endif
c
c  Exponent marker.
c
      elseif(leqi(chrtmp,'e').or.leqi(chrtmp,'d') )then
        if(ihave.lt.6)then
          ihave=6
        else
          iterm=1
        endif
c
c  Digit.
c
      elseif(ihave.lt.11.and.
     &  lge(chrtmp,'0').and.lle(chrtmp,'9') )then
 
        if(ihave.le.2)then
          ihave=3
        elseif(ihave.eq.4)then
          ihave=5
        elseif(ihave.eq.6.or.ihave.eq.7)then
          ihave=8
        elseif(ihave.eq.9)then
          ihave=10
        endif
 
        read(chrtmp,'(i1)')ndig
 
        if(ihave.eq.3)then
          rtop=10*rtop+ndig
        elseif(ihave.eq.5)then
          rtop=10*rtop+ndig
          rbot=10*rbot
        elseif(ihave.eq.8)then
          jtop=10*jtop+ndig
        elseif(ihave.eq.10)then
          jtop=10*jtop+ndig
          jbot=10*jbot
        endif
c
c  Anything else is regarded as a terminator.
c
      else
        iterm=1
      endif
c
c  If we haven't seen a terminator, and we haven't examined the
c  entire string, go get the next character.
c
      if(iterm.ne.1.and.lchar+1.lt.nchar)go to 10
c
c  If we haven't seen a terminator, and we have examined the
c  entire string, then we're done, and LCHAR is equal to NCHAR.
c
      if(iterm.ne.1.and.lchar+1.eq.nchar)lchar=nchar
c
c  Number seems to have terminated.  Have we got a legal number?
c  Not if we terminated in states 1, 2, 6 or 7!
c
      if(ihave.eq.1.or.ihave.eq.2.or.ihave.eq.6.or.ihave.eq.7)then
        ierror=ihave
        write(*,*)' '
        write(*,*)'ChrCTD - Fatal error!'
        write(*,*)'  Illegal or nonnumeric input!'
        return
      endif
c
c  Number seems OK.  Form it.
c
      if(jtop.eq.0)then
        rexp=1.0d0
      else
        if(jbot.eq.1)then
          rexp=10.0d0**(jsgn*jtop)
        else
          rexp=dble(jsgn*jtop)
          rexp=rexp/dble(jbot)
          rexp=10.0d0**rexp
        endif
      endif
 
      dval=dble(isgn)*rexp*rtop/rbot
 
      return
      end
