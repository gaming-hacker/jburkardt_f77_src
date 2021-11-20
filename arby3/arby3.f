c  arby3.f  14 August 1996
c
      program arbymain
c 
c***********************************************************************
c
cc ARBY3 solves a fluid flow problem using reduced basis techniques.
c
      implicit double precision (a-h,o-z)
c
c  Set parameters that are independent.
c
      integer liv
      integer maxbcrb
      integer maxferb
      integer maxnx
      integer maxny
      integer maxparb
      integer maxparf

      parameter (liv=60)
      parameter (maxbcrb=3)
      parameter (maxferb=6)
      parameter (maxnx=31)
      parameter (maxny=31)
      parameter (maxparb=5)
      parameter (maxparf=5)
c
c  Set parameters that are dependent on parameters.
c
c  The assignment of LDAFL should really read (ldafl=29*min(nx,ny)).
c
      integer ldafl
      integer maxcofrb
      integer maxelm
      integer maxnfl
      integer maxnp
      integer maxpar

      parameter (ldafl=29*maxny)
      parameter (maxcofrb=maxbcrb+maxferb)
      parameter (maxelm=2*(maxnx-1)*(maxny-1))
      parameter (maxnfl=2*(2*maxnx-1)*(2*maxny-1)+maxnx*maxny)
      parameter (maxnp=(2*maxnx-1)*(2*maxny-1))
      parameter (maxpar=maxparb+maxparf+1)
c
c  Set parameters that are dependent on parameters that are dependent
c  on parameters.
c
      integer lv
 
      parameter (lv=78+maxpar*(maxpar+21)/2)
c
      double precision afl(ldafl,maxnfl)
      double precision arb(maxcofrb,maxcofrb)
      double precision area(3,maxelm)
      character*9 chtime
      character*80 command
      double precision cost
      double precision cost0
      double precision costb
      double precision costp
      double precision costu
      double precision costv
      double precision detlog
      double precision detman
      double precision difcof(maxcofrb)
      character*30 disfil
      double precision dopt(maxpar)
      double precision dpar
      double precision drey
      logical dvneq
      logical echo
      double precision epsdif
      character*2 eqn(maxnfl)
      double precision estart
      double precision estop
      double precision etaq(3)
      double precision factj
      double precision gfl(maxnfl)
      double precision gflafl(maxnfl)
      double precision gflnrm
      double precision gflopt(maxnfl)
      double precision gflrb(maxnfl)
      double precision gflsav(maxnfl)
      double precision gflsen(maxnfl)
      double precision gfltar(maxnfl)
      double precision gfltay(maxnfl)
      double precision gfltmp(maxnfl)
      double precision grb(maxcofrb)
      double precision grbarb(maxcofrb)
      double precision grbopt(maxcofrb)
      double precision grbsav(maxcofrb)
      double precision grbsen(maxcofrb)
      double precision grbtay(maxcofrb)
      character*20 gridx
      character*20 gridy
      double precision gsen(maxcofrb)
      double precision hx
      double precision hy
      integer i
      integer ibs
      integer ibump
      integer icolrb(maxcofrb)
      integer ierror
      integer ifs
      integer ihi
      integer ijac
      integer ilo
      integer indx(3,maxnp)
      integer info
      integer iopt(maxpar)
      integer ipar
      integer ipivfl(maxnfl)
      integer ipivrb(maxcofrb)
      integer iprint
      integer isotri(maxelm)
      integer itemp
      integer ival
      integer ival1
      integer ival2
      integer ivopt(liv)
      integer iwrite
      integer j
      integer jhi
      integer jlo
      integer jtay
      integer klo
      integer lchar
      integer lenc
      integer lenchr
      logical leqi
      integer maxnew
      integer maxopt
      integer maxsim
      integer mhi
      integer mlo
      integer nbcrb
      integer ncofrb
      integer nelem
      integer neqnfl
      integer nferb
      integer nhi
      integer nlband
      integer nlo
      integer node(6,maxelm)
      integer nodelm(maxnp)
      integer np
      integer npar
      integer nparb
      integer nparf
      integer npe
      integer nprof(2*maxny-1)
      integer nsenfl
      integer ntay
      integer numdif
      integer numnew
      integer numopt
      integer numsim
      integer nx
      integer ny
      double precision p(maxnp)
      double precision par(maxpar)
      double precision parafl(maxpar)
      double precision pararb(maxpar)
      double precision pardif(maxpar)
      double precision paropt(maxpar)
      double precision parrb(maxpar)
      double precision parsav(maxpar)
      double precision parsen(maxpar)
      double precision partar(maxpar)
      double precision phifl(3,6,10,maxelm)
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision rb(maxnfl,maxcofrb)
      character*20 region
      double precision resfl(maxnfl)
      double precision resflsav(maxnfl)
      double precision resfltmp(maxnfl)
      double precision resrb(maxcofrb)
      double precision reynld
      double precision reytay
      double precision rbase(maxcofrb,maxcofrb)
      double precision rmax
      double precision senfl(maxnfl,maxcofrb)
      double precision senrb(maxcofrb,maxcofrb)
      double precision splbmp(maxparb+2)
      double precision splflo(maxparf+2)
      double precision taubmp(maxparb+2)
      double precision tauflo(maxparf+2)
      character*30 tecfil
      double precision temp
      double precision tolnew
      double precision tolopt
      double precision tolsim
      double precision u(maxnp)
      double precision v(maxnp)
      double precision value
      double precision vopt(lv)
      double precision wateb
      double precision watep
      double precision wateu
      double precision watev
      double precision wquad(3)
      double precision xbl
      double precision xbr
      double precision xc(maxnp)
      double precision xmax
      double precision xmin
      double precision xopt(maxpar)
      double precision xprof
      double precision xquad(3,maxelm)
      double precision xrange
      double precision xsiq(3)
      double precision ybl
      double precision ybr
      double precision yc(maxnp)
      double precision ymax
      double precision ymin
      double precision yquad(3,maxelm)
      double precision yrange
c
      external leqi
c
      iprint=1
c
c  Get initial CPU clock reading.
c
      call cpu_time ( estart )

      echo=.false.

      call hello(maxnx,maxny)
c
c  Open the file in which we record the user input.
c
      open(unit=17,file='arby.in',status='unknown')
c
      write(*,*)' '
      write(*,*)'ARBY3 - Init: Initialize all data.'

      call init(afl,arb,area,command,cost,costb,costp,costu,
     &  costv,difcof,disfil,drey,epsdif,eqn,etaq,gfl,gflafl,
     &  gflrb,gflsav,gflsen,gfltar,gfltay,grb,grbarb,grbsav,
     &  grbsen,grbtay,gridx,gridy,hx,hy,ibs,ibump,icolrb,ierror,
     &  ifs,ihi,ijac,ilo,indx,iopt,ipar,
     &  ipivfl,ipivrb,isotri,iwrite,jhi,jlo,
     &  ldafl,maxcofrb,maxelm,maxnew,maxnfl,maxnp,
     &  maxny,maxopt,maxpar,maxparb,maxparf,maxsim,
     &  nbcrb,ncofrb,nelem,neqnfl,nferb,
     &  nlband,node,nodelm,np,npar,nparb,nparf,npe,nprof,nsenfl,ntay,
     &  numnew,numopt,numsim,nx,
     &  ny,par,parafl,pararb,pardif,parrb,parsav,parsen,partar,
     &  phifl,phirb,rbase,rb,region,resfl,resflsav,resrb,reynld,
     &  reytay,senfl,
     &  senrb,splbmp,splflo,taubmp,tauflo,tecfil,tolnew,tolopt,
     &  tolsim,value,wateb,watep,wateu,watev,wquad,xbl,xbr,
     &  xc,xmax,xmin,xprof,xquad,xrange,xsiq,
     &  ybl,ybr,yc,ymax,ymin,yquad,yrange)
 
c
c
c  Read the next command from the user
c
10    continue

      if(iprint.gt.0)then
        write(*,*)' '
        write(*,*)'Enter command:'
      endif

11    continue

      read(*,'(a)',end=110,err=120)command
      lenc=lenchr(command)
      if(echo)then
        write(*,'(a)')command(1:lenc)
      endif
      write(17,'(a)')command(1:lenc)
c
c  Check for output-suppressing semicolon at end.
c
      if(lenc.gt.0)then
        if(command(lenc:lenc).eq.';')then
          iprint=0
          command(lenc:lenc)=' '
          lenc=lenc-1
        else
          iprint=1
        endif
      endif
 
15    continue

      if(command(1:1).eq.'#')then
        go to 11
      elseif(command.eq.' ')then
        go to 11
      endif
 
      if(iprint.gt.0)then
        write(*,*)' '
      endif
c
c  COMPARE
c
      if(leqi(command,'compare'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Compare:'
          write(*,*)'  Compare full solutions GFL and GFLSAV.'
        endif

        call fxfl(area,eqn,gfl,ifs,indx,nelem,neqnfl,node,np,npar,
     &    nparb,nparf,par,phifl,region,resfl,splflo,tauflo,
     &    xrange,yrange)
 
        call fxfl(area,eqn,gflsav,ifs,indx,nelem,neqnfl,node,np,npar,
     &    nparb,nparf,par,phifl,region,resflsav,splflo,tauflo,
     &    xrange,yrange)
 
        do i=1,neqnfl
          gfltmp(i)=gfl(i)-gflsav(i)
          resfltmp(i)=resfl(i)-resflsav(i)
        enddo
 
        call nrmflo(gfltmp,indx,neqnfl,np,resfltmp)
c
c  COSTFL
c
      elseif(leqi(command,'costfl'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Cost FL:'
          write(*,*)'  Evaluate the cost function for GFL.'
          write(*,*)' '
        endif

        call getcst(cost,costb,costp,costu,costv,gfl,gfltar,
     &    indx,neqnfl,np,nparb,nprof,ny,splbmp,
     &    taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)
 
        write(*,*)'Cost of GFL:',cost
c
c  COSTRB
c
      elseif(leqi(command,'costrb'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Cost RB:'
          write(*,*)'  Evaluate the cost function for GRB.'
          write(*,*)' '
        endif

        call grb2fl(gfl,grb,maxnfl,ncofrb,neqnfl,rb)
 
        call getcst(cost,costb,costp,costu,costv,gfl,gfltar,
     &    indx,neqnfl,np,nparb,nprof,ny,splbmp,
     &    taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)
 
        write(*,*)'Cost of GRB:',cost
c
c  DETFPFL
c
      elseif(leqi(command,'detfpfl'))then
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - DET FP FL'
          write(*,*)'  Compute determinant of full jacobian.'
        endif
 
        call fpfl(afl,area,eqn,gfl,indx,ldafl,maxelm,nelem,neqnfl,
     &    nlband,node,np,npar,nparb,nparf,par,phifl)

        call dfacfl(afl,ldafl,neqnfl,nlband,nlband,ipivfl,info)
 
        call ddetfl(afl,detlog,detman,ipivfl,ldafl,neqnfl,nlband,nlband)
 
        write(*,*)'Determinant = ',detman,' * 10 ** ',detlog
c
c  DETFPRB
c
      elseif(leqi(command,'detfprb'))then
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - DET FP RB'
          write(*,*)'  Compute the reduced jacobian,'
          write(*,*)'  and its determinant.'
        endif
 
        call fprb(arb,area,grb,indx,maxcofrb,maxelm,maxnfl,nbcrb,
     &    ncofrb,nelem,nferb,node,np,nx,ny,phirb,rb,reynld,xc,xrange,yc,
     &    yrange)
 
        call dfacrb(arb,maxcofrb,ncofrb,ipivrb,info)
 
        call ddetrb(arb,detlog,detman,ipivrb,maxcofrb,ncofrb)
 
        write(*,*)'Determinant = ',detman,' * 10 ** ',detlog
c
c  DIFFPRB
c
      elseif(leqi(command,'diffprb'))then
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - DIF FP RB'
          write(*,*)'  Estimate reduced jacobian by finite diff.'
        endif
 
        do i=1,npar
          pararb(i)=par(i)
        enddo
 
        do i=1,ncofrb
          grbarb(i)=grb(i)
        enddo
 
        do i=1,maxcofrb
          do j=1,maxcofrb
            arb(i,j)=0.0
          enddo
        enddo
 
        call diffprb(arb,area,epsdif,grb,indx,maxcofrb,maxelm,
     &    maxnfl,nbcrb,ncofrb,nelem,nferb,node,np,npar,nparf,nx,
     &    ny,par,phirb,rb,resrb,reynld,tauflo,xc,xrange,yc,yrange)
c
c  DIFSENFL
c
      elseif(leqi(command,'difsenfl'))then

        if(ipar.le.0.or.ipar.gt.npar)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  Cancelling the DIFSENFL command.'
          write(*,*)'  IPAR = ',ipar
          write(*,*)'  but IPAR must be at least 0'
          write(*,*)'  and no more than NPAR=',npar
          go to 10
        endif
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Dif Sen FL:'
          write(*,*)'  Estimate full solution sensitivity '
          write(*,*)'  with respect to parameter ',ipar
          write(*,*)'  via finite differences.'
        endif

        ipar=npar
        dpar=drey
 
        do i=1,npar
          parsen(i)=par(i)
        enddo
 
        do i=1,neqnfl
          gflsen(i)=gfl(i)
        enddo
 
        call difsenfl(afl,area,difcof,dpar,eqn,gfl,gflafl,ifs,
     &    ijac,indx,ipar,ipivfl,iwrite,ldafl,maxcofrb,maxelm,
     &    maxnew,maxnfl,
     &    ncofrb,nelem,neqnfl,nlband,node,np,npar,nparb,nparf,
     &    par,parafl,phifl,region,resfl,senfl,splflo,tauflo,
     &    tolnew,xrange,yrange)
c
c  DIFSENRB
c
      elseif(leqi(command,'difsenrb'))then

        if(ipar.le.0.or.ipar.gt.npar)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  Cancelling the DIFSENRB command.'
          write(*,*)'  IPAR = ',ipar
          write(*,*)'  but IPAR must be at least 0'
          write(*,*)'  and no more than NPAR=',npar
          go to 10
        endif
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Dif Sen RB:'
          write(*,*)'  Estimate reduced solution sensitivity '
          write(*,*)'  with respect to parameter ',ipar
          write(*,*)'  via finite differences.'
        endif

        ipar=npar
        dpar=drey
 
        do i=1,npar
          parsen(i)=par(i)
        enddo
 
        do i=1,ncofrb
          grbsen(i)=grb(i)
        enddo
 
        call difsenrb(arb,area,difcof,dpar,grb,grbarb,
     &    indx,ipar,ipivrb,iwrite,maxcofrb,maxelm,maxnew,maxnfl,nbcrb,
     &    ncofrb,nelem,nferb,node,np,npar,nparf,nx,ny,par,
     &    pararb,phirb,rb,resrb,senrb,tauflo,tolnew,xc,xrange,yc,yrange)
c
c  DisPlot
c
      elseif(leqi(command,'displot'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - DisPlot: Write data to DISPLAY plot file.'
        endif

        call intprs(gfl,indx,nelem,neqnfl,node,np,p)

        do i=1,np
          u(i)=gfl(indx(1,i))
          v(i)=gfl(indx(2,i))
        enddo

        call wrdis(disfil,eqn,indx,isotri,maxcofrb,maxnfl,
     &    ncofrb,nelem,neqnfl,node,np,npar,npe,nprof,nx,ny,p,par,rb,
     &    u,v,xc,xprof,yc)
c
c  DREY=
c
      elseif(leqi(command(1:4),'drey'))then

        if(leqi(command(1:5),'drey='))then
          read(command(6:),*)drey
        else
          write(*,*)'Enter value for DREY:'
          read(*,*)drey
          write(17,*)drey
        endif

        if(iprint.gt.0)then
          write(*,*)'DREY set to ',drey
        endif
c
c  EPSDIF=
c
      elseif(leqi(command(1:6),'epsdif'))then

        if(leqi(command(1:7),'epsdif='))then
          read(command(8:),*)epsdif
        else
          write(*,*)'Enter value for EPSDIF:'
          read(*,*)epsdif
          write(17,*)epsdif
        endif

        if(iprint.gt.0)then
          write(*,*)'EPSDIF set to ',epsdif
        endif
c
c  ECHO
c
      elseif(leqi(command,'echo'))then

        echo=.not.echo
        if(echo)then
          write(*,'(a)')command
          if(iprint.gt.0)then
            write(*,*)'User commands will be echoed.'
          endif
        else
          if(iprint.gt.0)then
            write(*,*)'User commands will not be echoed.'
          endif
        endif
c
c  EXPAND GRB
c
      elseif(leqi(command,'expand grb'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Expand GRB:'
          write(*,*)'  Compute GFL = RB * GRB'
        endif

       call grb2fl(gfl,grb,maxnfl,ncofrb,neqnfl,rb)
c
c  DISFIL=
c
      elseif(leqi(command(1:6),'disfil'))then

        if(leqi(command(1:7),'disfil='))then
          disfil=command(8:)
        else
          write(*,*)'Enter the DISPLAY output file name:'
          read(*,'(a)')disfil
          write(17,'(a)')disfil
        endif

        if(iprint.gt.0)then
          write(*,*)'DISPLAY output file name set to '//disfil
        endif
c
c  FILTEC=
c
      elseif(leqi(command(1:6),'tecfil'))then

        if(leqi(command(1:7),'tecfil='))then
          tecfil=command(8:)
        else
          write(*,*)'Enter the TECPLOT output file name:'
          read(*,'(a)')tecfil
          write(17,'(a)')tecfil
        endif

        if(iprint.gt.0)then
          write(*,*)'TECPLOT output file name set to '//tecfil
        endif
c
c  FPFL
c
      elseif(leqi(command,'fpfl'))then
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - FP FL - Evaluate full jacobian.'
        endif
 
        call fpfl(afl,area,eqn,gfl,indx,ldafl,maxelm,nelem,neqnfl,
     &    nlband,node,np,npar,nparb,nparf,par,phifl)
c
c  FPIRB
c
      elseif(leqi(command,'fpirb'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - FP I RB:'
          write(*,*)'  Evaluate FP indirectly at reduced solution GRB,'
          write(*,*)'  by expanding GRB to GFL, evaluating FP(GFL),'
          write(*,*)'  and then reducing.'
        endif

        call fpirb(afl,arb,area,eqn,grb,indx,ldafl,maxcofrb,
     &    maxelm,maxnfl,nbcrb,ncofrb,nelem,neqnfl,nferb,nlband,node,
     &    np,npar,nparb,nparf,nx,ny,par,phifl,rb,xc,xrange,yc,yrange)
c
c  FPRB
c
      elseif(leqi(command,'fprb'))then
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - FP RB - Evaluate reduced jacobian.'
        endif

        do i=1,npar
          pararb(i)=par(i)
        enddo
 
        do i=1,ncofrb
          grbarb(i)=grb(i)
        enddo
 
        call fprb(arb,area,grb,indx,maxcofrb,maxelm,maxnfl,nbcrb,
     &    ncofrb,nelem,nferb,node,np,nx,ny,phirb,rb,reynld,xc,xrange,yc,
     &    yrange)
c
c  FPRB=0
c
      elseif(leqi(command,'fprb=0'))then
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - FPRB=0 - Zero out the reduced jacobian.'
        endif
 
        do i=1,maxcofrb
          do j=1,maxcofrb
            arb(i,j)=0.0
          enddo
        enddo
c
c  FXFL
c
      elseif(leqi(command,'fxfl'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - FX FL:'
          write(*,*)'  Evaluate FXFL at full solution GFL.'
        endif

        call fxfl(area,eqn,gfl,ifs,indx,nelem,neqnfl,node,np,npar,
     &    nparb,nparf,par,phifl,region,resfl,splflo,tauflo,
     &    xrange,yrange)
c
c  FXIRB
c
      elseif(leqi(command,'fxirb'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - FX IRB:'
          write(*,*)'  Evaluate FX indirectly at reduced solution GRB,'
          write(*,*)'  by expanding GRB to GFL, evaluating F(GFL),'
          write(*,*)'  and then reducing.'
        endif

        call fxirb(area,eqn,grb,ifs,indx,maxcofrb,maxnfl,nbcrb,
     &    ncofrb,nelem,neqnfl,nferb,node,np,npar,nparb,nparf,
     &    nx,ny,par,phifl,rb,region,resrb,splflo,tauflo,xc,xrange,
     &    yc,yrange)
c
c  FXRB
c
      elseif(leqi(command,'fxrb'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - FX RB:'
          write(*,*)'  Evaluate FX directly at reduced solution GRB.'
        endif

        reynld=par(nparf+nparb+1)

        call fxrb(area,grb,indx,maxcofrb,maxelm,maxnfl,nbcrb,ncofrb,
     &    nelem,nferb,node,np,npar,nparf,nx,ny,par,phirb,rb,
     &    resrb,reynld,tauflo,xc,xrange,yc,yrange)
c
c  FXRB=0
c
      elseif(leqi(command,'fxrb=0'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - FXRB=0'
        endif
        do i=1,ncofrb
          resrb(i)=0.0
        enddo
c
c  GETGSEN
c
      elseif(leqi(command,'getgsen'))then
 
        call getgsen(grb,gsen,icolrb,maxcofrb,nbcrb,ncofrb,nsenfl,
     &    rbase)
c
c  GETRB
c
      elseif(leqi(command,'getrb'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Get RB:'
          write(*,*)'  Compute reduced basis RB at current GFL.'
        endif

        if(ncofrb.lt.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  The GETRB command is being cancelled,'
          write(*,*)'  since NCOFRB is ',ncofrb
          write(*,*)'  Please use the "NCOFRB=" command first!'
        endif
 
        if(dvneq(npar,par,parsen))then
          write(*,*)' '
          write(*,*)'ARBY3 - Error!'
          write(*,*)'  Please compute the sensitivities first,'
          write(*,*)'  using the GETSENFL command!'
          go to 10
        endif
c
c  Insert boundary condition basis into first columns of RB.
c  THIS CODE NEEDS TO BE IMPROVED.
c
        if(nbcrb.eq.1)then
          do i=1,neqnfl
            rb(i,1)=gfl(i)
          enddo
        endif
c
c  Note: boundary condition space for reduced solution is
c  spanned by NBCRB BC vectors.
c
        call getrb(gfl,gflrb,icolrb,maxcofrb,maxnfl,nbcrb,ncofrb,
     &    neqnfl,nferb,npar,nsenfl,par,parrb,rb,rbase,senfl,senrb)

        do i=1,ncofrb
          grb(i)=0.0
        enddo
c
c  Compute PHIRB, the reduced basis functions at quadrature points.
c
        if(iprint.gt.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Note:'
          write(*,*)'  Automatically evaluating PHIRB.'
        endif
 
        call setprb(eqn,indx,maxcofrb,maxelm,maxnfl,nelem,neqnfl,
     &    ncofrb,node,np,phifl,phirb,rb)
c
c  GETSENFL
c
      elseif(leqi(command,'getsenfl'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Get Sen FL:'
          write(*,*)'  Compute full solution sensitivities with '
          write(*,*)'  respect to the parameter REYNLD, up to order'
          write(*,*)'  NSENFL=',nsenfl
        endif

        if(nsenfl.lt.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  The GETSENFL command is being cancelled,'
          write(*,*)'  since NSENFL is ',nsenfl
          write(*,*)'  Please use the "NSENFL=" command first!'
        else
          do i=1,npar
            parsen(i)=par(i)
          enddo
 
          do i=1,neqnfl
            gflsen(i)=gfl(i)
          enddo
 
          call getsenfl(afl,area,eqn,gfl,indx,ipivfl,ldafl,
     &      maxcofrb,maxnfl,nelem,neqnfl,nlband,node,np,npar,
     &      nsenfl,par,phifl,resfl,senfl)

        endif
c
c  GETSENRB
c
      elseif(leqi(command,'getsenrb'))then
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Get Sen RB'
          write(*,*)'  Compute reduced basis sensitivities.'
        endif
 
        call getsenrb(maxcofrb,maxnfl,ncofrb,neqnfl,nsenfl,rb,
     &    senfl,senrb)
c
c  GFL=0, GFLSAV, GFLTAY, TAYLOR
c
      elseif(leqi(command(1:4),'gfl='))then

        if(command(5:5).eq.'0')then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFL=0'
            write(*,*)'  Set full solution estimate GFL to zero.'
          endif
 
          do i=1,neqnfl
            gfl(i)=0.0
          enddo
 
        elseif(leqi(command(5:10),'gflsav'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFL=GFLSAV'
            write(*,*)'  Set full solution estimate GFL to GFLSAV.'
          endif

          do i=1,neqnfl
            gfl(i)=gflsav(i)
          enddo
 
        elseif(leqi(command(5:10),'gfltay'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFL=GFLTAY'
            write(*,*)'  Set full solution estimate GFL to GFLTAY.'
          endif

          do i=1,neqnfl
            gfl(i)=gfltay(i)
          enddo
 
        elseif(leqi(command(5:10),'taylor'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFL=TAYLOR'
            write(*,*)'  Set full solution GFL to Taylor prediction,'
            write(*,*)'  based at REYTAY=',reytay
            write(*,*)'  using NTAY=',ntay,' terms.'
          endif

          do i=1,neqnfl
            gfl(i)=gfltay(i)
            do j=1,ntay
              call fact(j,factj)
              temp=((reynld-reytay)**j)/factj
              gfl(i)=gfl(i)+temp*senfl(i,j)
            enddo
          enddo
 
        endif
c
c  GFLSAV=GFL
c
      elseif(leqi(command,'gflsav=gfl'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - GFLSAV=GFL'
          write(*,*)'  Save current full solution.'
        endif
 
        do i=1,npar
          parsav(i)=par(i)
        enddo

        do i=1,neqnfl
          gflsav(i)=gfl(i)
        enddo
 
        do i=1,neqnfl
          resflsav(i)=resfl(i)
        enddo
c
c  GFLTAY=0, GFL, GFLSAV
c
      elseif(leqi(command(1:7),'gfltay='))then

        if(command(8:8).eq.'0')then
          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFLTAY=0'
            write(*,*)'  Set Taylor base full solution to zero.'
          endif
 
          do i=1,neqnfl
            gfltay(i)=0.0
          enddo
 
        elseif(leqi(command(8:13),'gflsav'))then
 
          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFLTAY=GFLSAV'
            write(*,*)'  Set Taylor base full solution to GFLSAV.'
          endif
 
          do i=1,neqnfl
            gfltay(i)=gflsav(i)
          enddo
 
        elseif(leqi(command(8:10),'gfl'))then
 
          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFLTAY=GFL'
            write(*,*)'  Set Taylor base full solution to GFL.'
          endif
 
          do i=1,neqnfl
            gfltay(i)=gfl(i)
          enddo
 
        endif
c
c  GFLTMP=0/GFL/GFL-GFLSAV/GFL-GFLTAR/GFLSAV/GFLSAV-GFLTAY
c
      elseif(leqi(command(1:7),'gfltmp='))then
 
        if(command(8:8).eq.'0')then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFLTMP=0'
          endif
 
          do i=1,neqnfl
            gfltmp(i)=0.0
          enddo
 
        elseif(leqi(command(8:),'gfl'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFLTMP=GFL'
          endif
 
          do i=1,neqnfl
            gfltmp(i)=gfl(i)
          enddo
 
        elseif(leqi(command(8:),'gfl-gflsav'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFLTMP=GFL-GFLSAV'
          endif
 
          do i=1,neqnfl
            gfltmp(i)=gfl(i)-gflsav(i)
          enddo
 
        elseif(leqi(command(8:),'gfl-gfltar'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFLTMP=GFL-GFLTAR'
          endif
 
          do i=1,neqnfl
            gfltmp(i)=gfl(i)-gfltar(i)
          enddo
 
        elseif(leqi(command(8:),'gflsav'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFLTMP=GFLSAV'
          endif
 
          do i=1,neqnfl
            gfltmp(i)=gflsav(i)
          enddo
 
        elseif(leqi(command(8:),'gflsav-gfltay'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GFLTMP=GFLSAV-GFLTAY'
          endif

          do i=1,neqnfl
            gfltmp(i)=gflsav(i)-gfltay(i)
          enddo
        else
          write(*,*)'ARBY3 - Error'
          write(*,*)'  Did not understand your command!'
          go to 10
        endif
c
c  GRB(*)=*
c
      elseif(leqi(command(1:4),'grb('))then
 
        call chrcti(command(5:),ival,ierror,lchar)
 
        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  ChrCTI returned nonzero error flag!'
          go to 10
        endif
 
        if(ival.lt.1.or.ival.gt.ncofrb)then
          write(*,*)' '
          write(*,*)'INPUT - Warning!'
          write(*,*)'  Index IVAL of GRB is out of bounds!'
          write(*,*)'  IVAL=',ival
          go to 10
        endif
 
        call chrctd(command(5+lchar+2:),value,ierror,lchar)

        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  ChrCTD returned nonzero error flag!'
          go to 10
        endif

        grb(ival)=value
 
        if(iprint.gt.0)then
          write(*,*)'GRB(',ival,') set to ',grb(ival)
        endif
c
c  GRB=0, GRBSAV, GRBTAY, TAYLOR
c
      elseif(leqi(command(1:4),'grb='))then

        if(leqi(command(1:5),'grb=0'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GRB=0'
            write(*,*)'  Set reduced solution to 0.'
          endif

          do i=1,ncofrb
            grb(i)=0.0
          enddo

        elseif(leqi(command,'grb=grbsav'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GRB=GRBSAV'
            write(*,*)'  Set reduced solution to saved value.'
          endif

          do i=1,ncofrb
            grb(i)=grbsav(i)
          enddo

        elseif(leqi(command,'grb=grbtay'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GRB=GRBTAY'
            write(*,*)'  Set reduced solution to Taylor base.'
          endif

          do i=1,ncofrb
            grb(i)=grbtay(i)
          enddo

        elseif(leqi(command,'grb=taylor'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GRB=TAYLOR'
            write(*,*)'  Set reduced solution to Taylor prediction,'
            write(*,*)'  based at REYTAY, GRBTAY plus sensitivities.'
          endif

          do i=1,ncofrb
            grb(i)=0.0
            do j=1,ntay
              jtay=j-1
              call fact(jtay,factj)
              temp=((reynld-reytay)**jtay)/factj
              grb(i)=grb(i)+temp*senrb(i,j)
            enddo
          enddo
 
        elseif(leqi(command(1:5),'grb=('))then

          if(iprint.gt.0)then 
            write(*,*)'ARBY3 - GRB=(v0,v1,...,vNeqnRB)'
          endif

          klo=6
          do i=1,ncofrb
            call chrctd(command(klo:),temp,ierror,lchar)
            if(ierror.ne.0)then
              write(*,*)'ARBY3 - Warning!'
              write(*,*)'  There was an error reading your data.'
              grb(i)=0.0
            else
              grb(i)=temp
            endif
            klo=klo+lchar
          enddo
 
        endif
c
c  GRBSAV=0, GRB
c
      elseif(leqi(command(1:7),'grbsav='))then

        if(command(8:8).eq.'0')then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GRBSAV=0'
          endif

          do i=1,ncofrb
            grbsav(i)=0.0
          enddo

        elseif(leqi(command,'grbsav=grb'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GRBSAV=GRB'
          endif

          do i=1,ncofrb
            grbsav(i)=grb(i)
          enddo
        endif
c
c  GRBTAY=0, GRB, GRBSAV
c
      elseif(leqi(command(1:7),'grbtay='))then

        if(command(8:8).eq.'0')then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GRBTAY=0'
            write(*,*)'  Set Taylor base reduced solution to zero.'
          endif
 
          do i=1,ncofrb
            grbtay(i)=0.0
          enddo
 
        elseif(leqi(command(8:13),'grbsav'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GRBTAY=GRBSAV'
            write(*,*)'  Set Taylor base reduced solution to GRBSAV.'
          endif
 
          do i=1,ncofrb
            grbtay(i)=grbsav(i)
          enddo
 
        elseif(leqi(command(8:10),'grb'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - GRBTAY=GRB'
            write(*,*)'  Set Taylor base reduced solution to GRB.'
          endif
 
          do i=1,ncofrb
            grbtay(i)=grb(i)
          enddo
 
        endif
c
c  GRIDX=
c
      elseif(leqi(command(1:5),'gridx'))then

        if(leqi(command(1:6),'gridx='))then
          gridx=command(7:)
        else
          write(*,*)'Enter GRIDX option: UNIFORM, COS, SINSQ:'
          read(*,'(a)')gridx
          write(17,'(a)')gridx
        endif

        if(iprint.gt.0)then
          write(*,*)'The GRIDX option set to '//gridx
          write(*,*)'Remember to use the SETGEO command'
          write(*,*)'before trying to solve your system!'
        endif
c
c  GRIDY=
c
      elseif(leqi(command(1:5),'gridy'))then

        if(leqi(command(1:6),'gridy='))then
          gridy=command(7:)
        else
          write(*,*)'Enter GRIDY option: UNIFORM, COS, SINSQ:'
          read(*,'(a)')gridy
          write(17,'(a)')gridy
        endif

        if(iprint.gt.0)then
          write(*,*)'The GRIDY option set to '//gridy
          write(*,*)'Remember to use the SETGEO command'
          write(*,*)'before trying to solve your system!'
        endif
c
c  HELLO
c
      elseif(leqi(command,'hello'))then

        write(*,*)'ARBY3 - Hello'
        write(*,*)' '

        call hello(maxnx,maxny)
c
c  HELP
c
      elseif(leqi(command,'help'))then

        call help
c
c  IBS=
c
      elseif(leqi(command(1:3),'ibs'))then

        if(leqi(command(1:4),'ibs='))then
          read(command(5:),*)ibs
        else
          write(*,*)'Enter value for IBS:'
          read(*,*)ibs
          write(17,*)ibs
        endif

        if(iprint.gt.0)then
          write(*,*)'IBS set to ',ibs
        endif
c
c  IBUMP=
c
      elseif(leqi(command(1:5),'ibump'))then

        if(leqi(command(1:6),'ibump='))then
          read(command(7:),*)ibump
        else
          write(*,*)'Enter value for IBUMP:'
          read(*,*)ibump
          write(17,*)ibump
        endif

        if(iprint.gt.0)then
          write(*,*)'IBUMP set to ',ibump
        endif
c
c  IFS=
c
      elseif(leqi(command(1:3),'ifs'))then

        if(leqi(command(1:4),'ifs='))then
          read(command(5:),*)ifs
        else
          write(*,*)'Enter value for IFS:'
          read(*,*)ifs
          write(17,*)ifs
        endif

        if(iprint.gt.0)then
          write(*,*)'IFS set to ',ifs
        endif
c
c  IHI=
c
      elseif(leqi(command(1:3),'ihi'))then

        if(leqi(command(1:4),'ihi='))then
          if(leqi(command,'ihi=ncofrb'))then
            ihi=ncofrb
          elseif(leqi(command,'ihi=neqnfl'))then
            ihi=neqnfl
          elseif(leqi(command,'ihi=np'))then
            ihi=np
          else
            read(command(5:),*)ihi
          endif
        else
          write(*,*)'Enter value for IHI:'
          read(*,*)ihi
          write(17,*)ihi
        endif

        if(iprint.gt.0)then
          write(*,*)'IHI set to ',ihi
        endif
c
c  IJAC=
c
      elseif(leqi(command(1:4),'ijac'))then

        if(leqi(command(1:5),'ijac='))then
          read(command(6:),*)ijac
        else
          write(*,*)'Enter value for IJAC:'
          read(*,*)ijac
          write(17,*)ijac
        endif

        if(iprint.gt.0)then
          write(*,*)'IJAC set to ',ijac
        endif
c
c  ILO=
c
      elseif(leqi(command(1:3),'ilo'))then

        if(leqi(command(1:4),'ilo='))then
          read(command(5:),*)ilo
        else
          write(*,*)'Enter value for ILO:'
          read(*,*)ilo
          write(17,*)ilo
        endif

        if(iprint.gt.0)then
          write(*,*)'ILO set to ',ilo
        endif
c
c  INIT
c
      elseif(leqi(command,'init'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Init'
          write(*,*)'  Initialize all data to zero.'
          write(*,*)' '
        endif

        call init(afl,arb,area,command,cost,costb,costp,costu,
     &    costv,difcof,disfil,drey,epsdif,eqn,etaq,gfl,gflafl,
     &    gflrb,gflsav,gflsen,gfltar,gfltay,grb,grbarb,grbsav,
     &    grbsen,grbtay,gridx,gridy,hx,hy,ibs,ibump,icolrb,ierror,
     &    ifs,ihi,ijac,ilo,indx,iopt,ipar,
     &    ipivfl,ipivrb,isotri,iwrite,jhi,jlo,
     &    ldafl,maxcofrb,maxelm,maxnew,maxnfl,maxnp,
     &    maxny,maxopt,maxpar,maxparb,maxparf,maxsim,
     &    nbcrb,ncofrb,nelem,neqnfl,nferb,
     &    nlband,node,nodelm,np,npar,nparb,nparf,npe,nprof,nsenfl,ntay,
     &    numnew,numopt,numsim,nx,
     &    ny,par,parafl,pararb,pardif,parrb,parsav,parsen,partar,
     &    phifl,phirb,rbase,rb,region,resfl,resflsav,resrb,reynld,
     &    reytay,senfl,
     &    senrb,splbmp,splflo,taubmp,tauflo,tecfil,tolnew,tolopt,
     &    tolsim,value,wateb,watep,wateu,watev,wquad,xbl,xbr,
     &    xc,xmax,xmin,xprof,xquad,xrange,xsiq,
     &    ybl,ybr,yc,ymax,ymin,yquad,yrange)
c
c  IOPT(*)=
c
        elseif(leqi(command(1:5),'iopt('))then
 
          call chrcti(command(6:),ival1,ierror,lchar)
 
          if(ierror.ne.0)then
            write(*,*)' '
            write(*,*)'INPUT - Fatal error!'
            write(*,*)'  ChrCTI returned nonzero error flag!'
            stop
          endif
 
          if(ival1.lt.1.or.ival1.gt.maxpar)then
            write(*,*)' '
            write(*,*)'INPUT - Fatal error!'
            write(*,*)'  Index IVAL1 of IOPT is out of bounds!'
            write(*,*)'  IVAL1 = ',ival1
            stop
          endif

          call chrcti(command(6+lchar+2:),ival2,ierror,lchar)
 
          iopt(ival1)=ival2

          if(iprint.gt.0)then
            write(*,*)'IOPT(',ival1,' ) set to ',ival2
          endif
c
c  IWRITE=
c
      elseif(leqi(command(1:6),'iwrite'))then

        if(leqi(command(1:7),'iwrite='))then
          read(command(8:),*)iwrite
        else
          write(*,*)'Enter value for IWRITE:'
          read(*,*)iwrite
          write(17,*)iwrite
        endif

        if(iprint.gt.0)then
          write(*,*)'IWRITE set to ',iwrite
        endif
c
c  JHI=
c
      elseif(leqi(command(1:3),'jhi'))then

        if(leqi(command(1:4),'jhi='))then
          if(leqi(command,'jhi=ncofrb'))then
            jhi=ncofrb
          elseif(leqi(command,'jhi=neqnfl'))then
            jhi=neqnfl
          elseif(leqi(command,'jhi=nsenfl'))then
            jhi=nsenfl
          else
            read(command(5:),*)jhi
          endif
        else
          write(*,*)'Enter value for JHI:'
          read(*,*)jhi
          write(17,*)jhi
        endif

        if(iprint.gt.0)then
          write(*,*)'JHI set to ',jhi
        endif
c
c  JLO=
c
      elseif(leqi(command(1:3),'jlo'))then

        if(leqi(command(1:4),'jlo='))then
          read(command(5:),*)jlo
        else
          write(*,*)'Enter value for JLO:'
          read(*,*)jlo
          write(17,*)jlo
        endif

        if(iprint.gt.0)then
          write(*,*)'JLO set to ',jlo
        endif
c
c  L2NORM GFL/GFLSAV/GFLTAR/GFLTAY/GFLTMP
c
      elseif(leqi(command(1:6),'l2norm'))then
 
        if(leqi(command(8:),'gfl'))then
          call l2norm(gfl,gflnrm,indx,nelem,neqnfl,node,np,xc,yc)
          write(*,*)'ARBY3 - L2Norm of GFL=',gflnrm
        elseif(leqi(command(8:),'gflsav'))then
          call l2norm(gflsav,gflnrm,indx,nelem,neqnfl,node,np,xc,yc)
          write(*,*)'ARBY3 - L2Norm of GFLSAV=',gflnrm
        elseif(leqi(command(8:),'gfltar'))then
          call l2norm(gfltar,gflnrm,indx,nelem,neqnfl,node,np,xc,yc)
          write(*,*)'ARBY3 - L2Norm of GFLTAR=',gflnrm
        elseif(leqi(command(8:),'gfltay'))then
          call l2norm(gfltay,gflnrm,indx,nelem,neqnfl,node,np,xc,yc)
          write(*,*)'ARBY3 - L2Norm of GFLTAY=',gflnrm
        elseif(leqi(command(8:),'gfltmp'))then
          call l2norm(gfltmp,gflnrm,indx,nelem,neqnfl,node,np,xc,yc)
          write(*,*)'ARBY3 - L2Norm of GFLTMP=',gflnrm
        else
          write(*,*)' '
          write(*,*)'ARBY3 - Error!'
          write(*,*)'  Legal choices were GFL/GFLSAV/GFLTAY/GFLTMP.'
          write(*,*)'  Your choice was '//command(8:)
          go to 10
        endif
 
c
c  MAXNEW=
c
      elseif(leqi(command(1:6),'maxnew'))then

        if(leqi(command(1:7),'maxnew='))then
          read(command(8:),*)maxnew
        else
          write(*,*)'Enter value for MAXNEW:'
          read(*,*)maxnew
          write(17,*)maxnew
        endif

        if(iprint.gt.0)then
          write(*,*)'MAXNEW set to ',maxnew
        endif
c
c  MAXOPT=
c
      elseif(leqi(command(1:6),'maxopt'))then

        if(leqi(command(1:7),'maxopt='))then
          read(command(8:),*)maxopt
        else
          write(*,*)'Enter value for MAXOPT:'
          read(*,*)maxopt
          write(17,*)maxopt
        endif

        if(iprint.gt.0)then
          write(*,*)'MAXOPT set to ',maxopt
        endif
c
c  MAXSIM=
c
      elseif(leqi(command(1:6),'maxsim'))then

        if(leqi(command(1:7),'maxsim='))then
          read(command(8:),*)maxsim
        else
          write(*,*)'Enter value for MAXSIM:'
          read(*,*)maxsim
          write(17,*)maxsim
        endif

        if(iprint.gt.0)then
          write(*,*)'MAXSIM set to ',maxsim
        endif
c
c  NBCRB=
c
      elseif(leqi(command(1:5),'nbcrb'))then

        if(leqi(command(1:6),'nbcrb='))then
          read(command(7:),*)nbcrb
        else
          write(*,*)'Enter value for NBCRB:'
          read(*,*)nbcrb
          write(17,*)nbcrb
        endif

        if(iprint.gt.0)then
          write(*,*)'NBCRB set to ',nbcrb
        endif
c
c  NEWTFL
c  Apply Newton's method to full solution estimate.
c
      elseif(leqi(command,'newtfl'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - NewtFL'
          write(*,*)'  Apply Newton to full solution estimate GFL.'
        endif

        call newtfl(afl,area,eqn,gfl,gflafl,ierror,ifs,ijac,
     &    indx,ipivfl,iwrite,ldafl,maxelm,maxnew,nelem,neqnfl,nlband,
     &    node,np,npar,nparb,nparf,numnew,par,parafl,phifl,
     &    region,resfl,rmax,splflo,tauflo,tolnew,xrange,yrange)

        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Fatal error!'
          write(*,*)'  NEWTFL failed!'
          write(*,*)'  The parameters at which failure occurred:'
          write(*,*)' '
          call prpar(iopt,npar,nparb,nparf,par)
          ierror=1
          go to 10
        else
          if(iwrite.le.1)then
            write(*,*)'  Newton step ',numsim,' residual norm =',rmax
          endif
        endif
c
c  NEWTRB
c  Apply Newton's method to reduced solution estimate.
c
      elseif(leqi(command,'newtrb'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - NewtRB'
          write(*,*)'  Apply Newton to reduced solution estimate GRB.'
        endif

        if(ncofrb.le.0)go to 10
 
        call newtrb(arb,area,grb,grbarb,ierror,indx,ipivrb,
     &    iwrite,maxcofrb,maxelm,maxnew,maxnfl,nbcrb,ncofrb,nelem,
     &    nferb,node,np,npar,nparf,nx,ny,par,pararb,phirb,
     &    rb,resrb,rmax,tauflo,tolnew,xc,xrange,yc,yrange)
 
        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Fatal error!'
          write(*,*)'  NEWTRB failed!'
          write(*,*)'  The parameters at which failure occurred:'
          call prpar(iopt,npar,nparb,nparf,par)
          ierror=1
          go to 10
        else
          if(iwrite.le.1)then
            write(*,*)'  Final Newton residual was MxNorm(FXRB)=',rmax
          endif
        endif
c
c  NPARB=
c
      elseif(leqi(command(1:5),'nparb'))then

        if(leqi(command(1:6),'nparb='))then
          read(command(7:),*)nparb
        else
          write(*,*)'Enter value for NPARB:'
          read(*,*)nparb
          write(17,*)nparb
        endif

        if(iprint.gt.0)then
          write(*,*)'NPARB set to ',nparb
        endif
c
c  NPARF=
c
      elseif(leqi(command(1:5),'nparf'))then

        if(leqi(command(1:6),'nparf='))then
          read(command(7:),*)nparf
        else
          write(*,*)'Enter value for NPARF:'
          read(*,*)nparf
          write(17,*)nparf
        endif

        if(iprint.gt.0)then
          write(*,*)'NPARF set to ',nparf
        endif
c
c  NSENFL=#
c
      elseif(leqi(command(1:6),'nsenfl'))then

        if(leqi(command(1:7),'nsenfl='))then
          read(command(8:),*)itemp
        else
          write(*,*)'Enter value for NSENFL:'
          read(*,*)itemp
          write(17,*)itemp
        endif

        if(itemp.lt.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  NSENFL must be at least 0!'
          write(*,*)'  but your value was ',itemp
        else
          nsenfl=itemp
          if(iprint.gt.0)then
            write(*,*)'ARBY3 - NSENFL set to ',nsenfl
          endif
        endif
c
c  NTAY=#
c  NTAY=NCOFRB
c
      elseif(leqi(command(1:4),'ntay'))then

        if(leqi(command(1:5),'ntay='))then
          if(leqi(command(6:11),'ncofrb'))then
            itemp=ncofrb
          else
            read(command(6:),*)itemp
          endif
        else
          write(*,*)'Enter value for NTAY:'
          read(*,*)itemp
          write(17,*)itemp
        endif

        if(itemp.lt.0.or.itemp.gt.ncofrb)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  NTAY must be between 0 and NCOFRB= ',ncofrb
          write(*,*)'  but your value was ',itemp
        else
          ntay=itemp
          if(iprint.gt.0)then
            write(*,*)'ARBY3 - NTAY set to ',ntay
          endif
        endif
c
c  NX=
c
      elseif(leqi(command(1:2),'nx'))then

        if(leqi(command(1:3),'nx='))then
          read(command(4:),*)itemp
        else
          write(*,*)'Enter value for NX:'
          read(*,*)itemp
          write(17,*)itemp
        endif

        if(itemp.lt.2)then
          write(*,*)'ARBY3 - Unacceptable input.'
          write(*,*)'  NX must be at least 2.'
          write(*,*)'  Your value was ',itemp
        elseif(itemp.gt.maxnx)then
          write(*,*)'ARBY3 - Unacceptable input.'
          write(*,*)'  NX must be no more than MAXNX=',maxnx
          write(*,*)'  Your value was ',itemp
        else
          nx=itemp
          if(iprint.gt.0)then
            write(*,*)'NX set to ',nx
            write(*,*)'Remember to use the SETLOG and SETGEO commands'
            write(*,*)'before trying to solve your systems!'
          endif
        endif
c
c  NY=
c
      elseif(leqi(command(1:2),'ny'))then

        if(leqi(command(1:3),'ny='))then
          read(command(4:),*)itemp
        else
          write(*,*)'Enter value for NY:'
          read(*,*)itemp
          write(17,*)itemp
        endif

        if(itemp.lt.2)then
          write(*,*)'ARBY3 - Unacceptable input.'
          write(*,*)'  NY must be at least 2.'
          write(*,*)'  Your value was ',itemp
        elseif(itemp.gt.maxny)then
          write(*,*)'ARBY3 - Unacceptable input.'
          write(*,*)'  NY must be no more than MAXNY=',maxny
          write(*,*)'  Your value was ',itemp
        else
          ny=itemp
          if(iprint.gt.0)then
            write(*,*)'NY set to ',ny
            write(*,*)'Remember to use the SETLOG and SETGEO commands'
            write(*,*)'before trying to solve your system!'
          endif
        endif
c
c  OPTFL
c
      elseif(leqi(command,'optfl'))then

        write(*,*)'ARBY3 - OPT FL:'
        write(*,*)'  Optimize the full system.'
        write(*,*)'  NO WAY, JOSE!  NOT READY YET!'

c
c  OPTDIFFL
c
      elseif(leqi(command,'optdiffl'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - OptDifFl:'
          write(*,*)'  Optimize the cost of the full system;'
          write(*,*)'  The optimization code will approximate cost'
          write(*,*)'  gradients by finite differences.'
          write(*,*)'  Initial estimate is (PAR,GFL,COST).'
          write(*,*)' '
          write(*,*)'ARBY3 - Note!'
          write(*,*)'  You must already have issued the TARGET command!'
        endif
 
        call optdiffl(afl,area,cost,dopt,eqn,etaq,gfl,gflafl,
     &    gflopt,gfltar,gridx,gridy,ibs,ierror,ifs,ijac,indx,iopt,
     &    ipivfl,isotri,ivopt,iwrite,ldafl,liv,lv,maxelm,maxnew,maxnfl,
     &    maxnp,maxny,maxopt,maxpar,maxparb,maxparf,maxsim,nelem,
     &    neqnfl,nlband,node,nodelm,np,npar,nparb,nparf,nprof,numdif,
     &    numopt,nx,ny,par,parafl,paropt,phifl,region,resfl,splbmp,
     &    splflo,taubmp,tauflo,tolnew,tolopt,tolsim,vopt,wateb,watep,
     &    wateu,watev,wquad,xbl,xbr,xc,xopt,xquad,xrange,xsiq,ybl,
     &    ybr,yc,yquad,yrange)
 
        if(ierror.eq.0)then
          do i=1,npar
            par(i)=paropt(i)
          enddo
          do i=1,neqnfl
            gfl(i)=gflopt(i)
          enddo
          write(*,*)' '
          write(*,*)'Optimizing parameters:'
          call prpar(iopt,npar,nparb,nparf,par)
          write(*,*)' '
          write(*,*)'Optimal cost=',cost
          write(*,*)' '
          write(*,*)'Number of standard full solutions:',numopt
          write(*,*)'Number of auxilliary solutions:   ',numdif
        else
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  The optimization was unsuccessful.'
        endif
c
c  OPTDIFRB
c
      elseif(leqi(command,'optdifrb'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - OptDifRB:'
          write(*,*)'  Optimize the cost of the reduced system;'
          write(*,*)'  The optimization code will approximate cost'
          write(*,*)'  gradients by finite differences.'
          write(*,*)'  Initial estimate is (PAR,GRB,COST).'
          write(*,*)' '
          write(*,*)'ARBY3 - Note!'
          write(*,*)'  You must already have issued the commands:'
          write(*,*)'  SETLOG, SETGEO, NEWTFL, GETSEN, GETRB, TARGET!'
        endif
 
        call optdifrb(arb,area,cost,dopt,gfltar,gfltmp,
     &    grb,grbarb,grbopt,ierror,indx,iopt,ipivrb,
     &    ivopt,iwrite,liv,lv,maxcofrb,maxelm,maxnew,maxnfl,maxnp,
     &    maxny,maxopt,maxpar,maxparb,maxsim,nbcrb,ncofrb,nelem,neqnfl,
     &    nferb,node,np,npar,nparb,nparf,nprof,numdif,numopt,nx,ny,par,
     &    pararb,paropt,phirb,rb,resrb,splbmp,tauflo,taubmp,tolnew,
     &    tolopt,tolsim,vopt,wateb,watep,wateu,watev,xbl,xbr,xc,xopt,
     &    xrange,ybl,ybr,yc,yrange)

        if(ierror.eq.0)then
          do i=1,npar
            par(i)=paropt(i)
          enddo
          do i=1,ncofrb
            grb(i)=grbopt(i)
          enddo
          write(*,*)' '
          write(*,*)'Optimizing parameters:'
          call prpar(iopt,npar,nparb,nparf,par)
          write(*,*)' '
          write(*,*)'Optimal cost=',cost
          write(*,*)' '
          write(*,*)'Number of standard full solutions:',numopt
          write(*,*)'Number of auxilliary solutions:   ',numdif
        else
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  The optimization was unsuccessful.'
        endif
c
c  PAR(*)=*
c
      elseif(leqi(command(1:4),'par('))then
 
        call chrcti(command(5:),ival,ierror,lchar)
 
        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  ChrCTI returned nonzero error flag!'
          go to 10
        endif
 
        if(ival.lt.1.or.ival.gt.maxpar)then
          write(*,*)' '
          write(*,*)'INPUT - Warning!'
          write(*,*)'  Index IVAL of PAR is out of bounds!'
          write(*,*)'  IVAL=',ival
          go to 10
        endif
 
        call chrctd(command(5+lchar+2:),value,ierror,lchar)

        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  ChrCTD returned nonzero error flag!'
          go to 10
        endif

        par(ival)=value
 
        if(iprint.gt.0)then
          write(*,*)'PAR(',ival,') set to ',par(ival)
        endif
c
c  PARTAR(*)=*
c
      elseif(leqi(command(1:7),'partar('))then
 
        call chrcti(command(8:),ival,ierror,lchar)
 
        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  ChrCTI returned nonzero error flag!'
          go to 10
        endif
 
        if(ival.lt.1.or.ival.gt.maxpar)then
          write(*,*)' '
          write(*,*)'INPUT - Warning!'
          write(*,*)'  Index IVAL of PARTAR is out of bounds!'
          write(*,*)'  IVAL=',ival
          go to 10
        endif
 
        call chrctd(command(8+lchar+2:),value,ierror,lchar)

        if(ierror.ne.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  ChrCTD returned nonzero error flag!'
          go to 10
        endif

        partar(ival)=value
        par(ival)=value
 
        if(iprint.gt.0)then
          write(*,*)'PARTAR(',ival,') set to ',partar(ival)
        endif
c
c  PICFL
c
      elseif(leqi(command,'picfl'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - PicFL:'
          write(*,*)'  Apply Picard to full solution estimate GFL.'
        endif

        call picfl(afl,area,eqn,gfl,ierror,ifs,indx,ipivfl,iwrite,
     &    ldafl,maxsim,nelem,neqnfl,nlband,node,np,npar,nparb,nparf,
     &    numsim,par,phifl,region,resfl,rmax,splflo,tauflo,
     &    tolsim,xc,xrange,yc,yrange)
 
        if(iwrite.le.1)then
          write(*,*)'  Picard step ',numsim,' residual norm =',rmax
        endif
c
c  PICRB
c
      elseif(leqi(command,'picrb'))then

        if(iprint.gt.0)then   
          write(*,*)'ARBY3 - PicRB:'
          write(*,*)'  Apply Picard to reduced solution estimate GRB.'
        endif
 
        if(ncofrb.le.0)go to 10
 
        call picrb(arb,area,grb,ierror,indx,ipivrb,iwrite,
     &    maxcofrb,maxelm,maxnfl,maxsim,nbcrb,ncofrb,nelem,
     &    nferb,node,np,npar,nparf,nx,ny,par,phirb,rb,resrb,rmax,
     &    tauflo,tolsim,xc,xrange,yc,yrange)
 
        if(iwrite.le.1)then
          write(*,*)'  Final Picard residual was MxNorm(FXRB)=',rmax
        endif
c
c  PRFPFL
c
      elseif(leqi(command,'prfpfl'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr FP FL:'
          write(*,*)'  Print full jacobian.'
          write(*,*)'  Rows ILO=',ilo,' to IHI=',ihi
          write(*,*)'  Cols JLO=',jlo,' to JHI=',jhi
          write(*,*)' '
          write(*,*)'  Parameters for matrix, PARAFL:'

          call prpar(iopt,npar,nparb,nparf,parafl)
        endif
 
        call prbmat(afl,ihi,ilo,jhi,jlo,ldafl,neqnfl,nlband)
c
c  PRFPRB
c
      elseif(leqi(command,'prfprb'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr FP RB:'
          write(*,*)'  Print reduced jacobian.'
          write(*,*)'  Rows ILO=',ilo,' to IHI=',ihi
          write(*,*)'  Cols JLO=',jlo,' to JHI=',jhi
          write(*,*)' '
          write(*,*)'  Parameters for matrix, PARARB:'
          call prpar(iopt,npar,nparb,nparf,pararb)
        endif
 
        mlo=1
        mhi=maxcofrb
        nlo=1
        nhi=maxcofrb
 
        call prdmat(arb,ihi,ilo,jhi,jlo,mhi,mlo,nhi,nlo)
c
c  PRDAT
c
      elseif(leqi(command,'prdat'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr Dat'
          write(*,*)'  Print current problem data.'
        endif

        call prdat(disfil,drey,epsdif,gridx,gridy,hx,hy,ibs,
     &    ibump,ifs,ijac,iopt,maxnew,maxopt,maxsim,
     &    nbcrb,ncofrb,nelem,nferb,neqnfl,np,npar,nparb,nparf,ntay,
     &    nx,ny,region,reytay,tecfil,tolnew,tolopt,tolsim,
     &    wateb,
     &    watep,wateu,watev,xbl,xbr,xprof,xrange,
     &    ybl,ybr,yrange)
c
c  PRELEM
c
      elseif(leqi(command,'prelem'))then

        if(iprint.gt.0)then   
          write(*,*)'ARBY3 - Pr Elem'
          write(*,*)'  Print element data.'
        endif
 
        call prelem(ihi,ilo,nelem,node,np,xc,yc)
c
c  PRFXFL
c
      elseif(leqi(command,'prfxfl'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr FX FL'
          write(*,*)'  Print full residual FXFL.'
        endif

        call prvecfl(eqn,ihi,ilo,indx,neqnfl,np,resfl)
c
c  PRFXFLNRM
c
      elseif(leqi(command,'prfxflnrm'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr FX FL Nrm'
          write(*,*)'  Print norm of full residual FXFL.'
        endif

        call prfxfln(neqnfl,resfl)
c
c  PRFXRB
c
      elseif(leqi(command,'prfxrb'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr FX RB'
          write(*,*)'  Print reduced residual FXRB.'
        endif

        nlo=1
        nhi=ncofrb
        call prvecrb(ihi,ilo,nhi,nlo,resrb)
c
c  PRGFL
c
      elseif(leqi(command,'prgfl'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr G FL:'
          write(*,*)'  Print full solution GFL.'
          write(*,*)' '
          write(*,*)'  Flow parameters, PAR:'
          call prpar(iopt,npar,nparb,nparf,par)
        endif

        call prvecfl(eqn,ihi,ilo,indx,neqnfl,np,gfl)
c
c  PRGFLNRM
c
      elseif(leqi(command,'prgflnrm'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr GFL Nrm:'
          write(*,*)'  Print norms of full solution GFL.'
        endif

        call fxfl(area,eqn,gfl,ifs,indx,nelem,neqnfl,node,np,npar,
     &    nparb,nparf,par,phifl,region,resfl,splflo,tauflo,
     &    xrange,yrange)
 
        call nrmflo(gfl,indx,neqnfl,np,resfl)
c
c  PRGRB
c
      elseif(leqi(command,'prgrb'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr G RB:'
          write(*,*)'  Print reduced solution GRB.'
        endif

        call prgrb(grb,ncofrb)
c
c  PRGSEN
c
      elseif(leqi(command,'prgsen'))then
 
        call prgrb(gsen,nbcrb+nsenfl)
 
c
c  PRINDX
c
      elseif(leqi(command,'prindx'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr INDX'
          write(*,*)'  Print node/equation table,'
          write(*,*)'  for nodes ILO=',ilo,' to IHI=',ihi
        endif
 
        call prindx(ihi,ilo,indx,np,xc,yc)
c
c  PRPAR
c
      elseif(leqi(command,'prpar'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr PAR: Print current parameters PAR.'
        endif

        call prpar(iopt,npar,nparb,nparf,par)
c
c  PRRBASE
c
      elseif(leqi(command,'prrbase'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr RBase'
          write(*,*)'  Print the "R" factor of the reduced basis.'
        endif
 
        ilo=1
        ihi=ncofrb
        jlo=1
        jhi=ncofrb
        mlo=1
        mhi=maxcofrb
        nlo=1
        nhi=maxcofrb
 
        call prdmat(rbase,ihi,ilo,jhi,jlo,mhi,mlo,nhi,nlo)
c
c  PRRB
c
      elseif(leqi(command,'prrb'))then
 
        if(ncofrb.le.0)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  PRRB command cancelled, NCOFRB=',ncofrb
          write(*,*)'  Use the GETRB command first!'
          go to 10
        endif
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr RB'
          write(*,*)'  Print the reduced basis,'
          write(*,*)'  nodes ILO=',ilo,' to IHI=',ihi
          write(*,*)'  columns JLO=',jlo,' to JHI=',jhi
          write(*,*)' '
          write(*,*)'  Parameters at reduced basis, PARRB:'
          call prpar(iopt,npar,nparb,nparf,parrb)
        endif
 
        call prmatfl(rb,eqn,ihi,ilo,indx,jhi,jlo,maxnfl,ncofrb,
     &    neqnfl,np)
c
c  PRSENFL: Print full sensitivities.
c
      elseif(leqi(command,'prsenfl'))then
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr Sen FL'
          write(*,*)'  Print full sensitivities.'
          write(*,*)' '
          write(*,*)'  Parameters at sensitivity, PARSEN:'

          call prpar(iopt,npar,nparb,nparf,parsen)
        endif
 
        call prmatfl(senfl,eqn,ihi,ilo,indx,jhi,jlo,maxnfl,nsenfl,
     &    neqnfl,np)
c
c  PRSENNRM: Print full sensitivity norms.
c
      elseif(leqi(command,'prsennrm'))then
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr Sen Nrm'
          write(*,*)'  Print sensitivity norms.'
        endif
 
        call prsenn(maxcofrb,maxnfl,ncofrb,neqnfl,senfl)
c
c  PRSENRB: Print reduced sensitivities.
c
      elseif(leqi(command,'prsenrb'))then

        if(iprint.gt.0)then   
          write(*,*)'ARBY3 - Pr Sen RB'
          write(*,*)'  Print matrix of reduced sensitivities,'
          write(*,*)'  rows ILO=',ilo,' to IHI=',ihi
          write(*,*)'  columns JLO=',jlo,' to JHI=',jhi
          write(*,*)' '
          write(*,*)'  Parameters at sensitivity, PARSEN:'

          call prpar(iopt,npar,nparb,nparf,parsen)
        endif
 
        mlo=1
        mhi=maxcofrb
        nlo=1
        nhi=ncofrb
 
        call prdmat(senrb,ihi,ilo,jhi,jlo,mhi,mlo,nhi,nlo)
c 
c  PRUVPGFL
c
      elseif(leqi(command,'pruvpgfl'))then
 
        call pruvpfl(gfl,indx,neqnfl,np,xc,xmax,xmin,yc,ymax,ymin)
c
c  PRUVPRB
c
      elseif(leqi(command,'pruvprb'))then
 
        do j=1,ncofrb
 
          write(*,*)' '
          write(*,*)'Reduced basis vector ',j
          write(*,*)' '
 
          call pruvpfl(rb(1,j),indx,neqnfl,np,xc,xmax,xmin,yc,ymax,ymin)
 
        enddo
c
c  PRUVPSENFL
c
      elseif(leqi(command,'pruvpsenfl'))then
 
        do j=1,nsenfl
 
          write(*,*)' '
          write(*,*)'Sensitivity vector ',j
          write(*,*)' '
 
          call pruvpfl(senfl(1,j),indx,neqnfl,np,xc,xmax,xmin,yc,ymax,
     &      ymin)
 
        enddo
c 
c  PRUVPGRB
c
      elseif(leqi(command,'pruvpgrb'))then
 
        call pruvprb(grb,indx,maxnfl,ncofrb,nelem,node,nodelm,np,
     &    rb,xc,xmax,xmin,yc,ymax,ymin)
 
c
c  PRXY
c
      elseif(leqi(command,'prxy'))then
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Pr XY'
          write(*,*)'  Print out X and Y nodal coordinates.'
        endif
 
        call prxy(ihi,ilo,np,ny,xc,yc) 
c
c  REDUCE GFL
c
      elseif(leqi(command,'reduce gfl'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Reduce GFL:'
          write(*,*)'  Given a reduced basis RB computed at the'
          write(*,*)'  full solution GFLRB, and an arbitrary full'
          write(*,*)'  solution GFL, compute the reduced basis '
          write(*,*)'  coefficients of GFL:'
          write(*,*)'    GRB = RB^T * GFL.'
        endif

        call gfl2rb(gfl,grb,maxnfl,ncofrb,neqnfl,rb)
c
c  REGION=
c
      elseif(leqi(command(1:6),'region'))then

        if(leqi(command(1:7),'region='))then
          region=command(8:)
        else
          write(*,*)'Enter the region, CAVITY, CHANNEL or STEP:'
          read(*,'(a)')region
          write(17,'(a)')region
        endif

        if(leqi(region,'cavity'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - Cavity:'
            write(*,*)'  Set user input values to cavity defaults.'
          endif

          call cavity(ibs,ibump,ifs,iopt,maxopt,maxpar,nbcrb,
     &      npar,nparb,nparf,npe,nx,ny,par,region,reynld,tolnew,tolopt,
     &      tolsim,wateb,wateu,watev,watep,xbl,xbr,xprof,
     &      xrange,ybl,ybr,yrange)

        elseif(leqi(region,'channel'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - Channel:'
            write(*,*)'  Set user input values to channel defaults.'
          endif

          call channl(ibs,ibump,ifs,iopt,maxopt,maxpar,nbcrb,
     &      npar,nparb,nparf,npe,
     &      nx,ny,par,region,reynld,tolnew,tolopt,
     &      tolsim,
     &      wateb,wateu,watev,watep,xbl,xbr,xprof,
     &      xrange,ybl,ybr,yrange)

        elseif(leqi(region,'step'))then

          if(iprint.gt.0)then
            write(*,*)'ARBY3 - Step`:'
            write(*,*)'  Set user input values to step defaults.'
          endif

          call step(ibs,ibump,ifs,iopt,maxopt,maxpar,nbcrb,
     &      npar,nparb,nparf,npe,
     &      nx,ny,par,region,reynld,tolnew,tolopt,
     &      tolsim,
     &      wateb,wateu,watev,watep,xbl,xbr,xprof,
     &      xrange,ybl,ybr,yrange)

        endif
c
c  REYNLD=
c
      elseif(leqi(command(1:6),'reynld'))then

        if(leqi(command(1:7),'reynld='))then
          read(command(8:),*)reynld
        else
          write(*,*)'Enter value for REYNLD:'
          read(*,*)reynld
          write(17,*)reynld
        endif

        par(nparf+nparb+1)=reynld

        if(iprint.gt.0)then
          write(*,*)'REYNLD parameter set to ',reynld
        endif
c
c  REYTAY=
c
      elseif(leqi(command(1:6),'reytay'))then

        if(leqi(command(1:7),'reytay='))then
          if(leqi(command,'reytay=reynld'))then
            reytay=reynld
          else
            read(command(8:),*)reytay
          endif
        else
          write(*,*)'Enter value for REYTAY:'
          read(*,*)reytay
          write(17,*)reytay
        endif

        if(iprint.gt.0)then
          write(*,*)'REYTAY parameter set to ',reytay
        endif
c
c  SETGEO
c
      elseif(leqi(command,'setgeo'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - SetGeo: Set problem geometry.'
        endif

        call setgeo(area,etaq,gridx,gridy,ibs,isotri,nelem,node,
     &    nodelm,np,npar,nparb,nparf,nx,ny,par,phifl,region,
     &    splbmp,taubmp,wquad,xbl,xbr,xc,xquad,xrange,
     &    xsiq,ybl,ybr,yc,yquad,yrange)
 
c
c  SETLOG
c
      elseif(leqi(command,'setlog'))then

        if(iprint.gt.0)then   
          write(*,*)'ARBY3 - SetLog: Set problem logical data.'
        endif
 
        call setlog(eqn,hx,hy,ibump,indx,isotri,ldafl,maxelm,
     &    maxnfl,maxnp,nelem,neqnfl,nlband,node,np,nprof,nx,ny,
     &    region,xbl,xbr,xprof,xrange,ybr,yrange)
c
c  STOP
c
      elseif(leqi(command,'stop').or.
     &       leqi(command,'quit'))then

        write(*,*)'ARBY3 - Stop:'
        write(*,*)'  Halt the program!'
        write(*,*)' '

        close(unit=17)
        write(*,*)'  Closing the user input file ARBY.IN.'

        call cpu_time ( estop )
        write(*,*)' '
        write(*,*)'  CPU in seconds = ',estop-estart
        write(*,*)'  CPU in minutes = ',(estop-estart)/60.0
        write(*,*)' '
        write(*,*)'  End of execution.'

        stop
c
c  TARGET
c
      elseif(leqi(command,'target'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Target:'
          write(*,*)'  Save current GFL as GTAR.'
        endif

        call target(cost0,gfl,gfltar,indx,maxnfl,maxny,maxparb,
     &    neqnfl,np,npar,nparb,nprof,ny,par,partar,splbmp,
     &    taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)
c
c  TEST2
c
      elseif(leqi(command,'test2'))then
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Test2:'
          write(*,*)'  Compare full and reduced state variables'
          write(*,*)'  in elements ILO through IHI.'
        endif
 
        call test2(gfl,grb,ihi,ilo,indx,maxcofrb,maxelm,ncofrb,
     &    nelem,neqnfl,node,np,phifl,phirb)
c
c  TEST3
c
      elseif(leqi(command,'test3'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Test3:'
          write(*,*)'  Compare RB*Rfact and SenFL.'
        endif
 
        call test3(maxcofrb,maxnfl,ncofrb,neqnfl,rb,senfl,senrb)
c
c  TEST4
c
      elseif(leqi(command,'test4'))then
 
        if(ipar.le.0.or.ipar.gt.npar)then
          write(*,*)' '
          write(*,*)'ARBY3 - Warning!'
          write(*,*)'  Cancelling the TEST4 command.'
          write(*,*)'  IPAR = ',ipar
          write(*,*)'  but IPAR must be at least 0'
          write(*,*)'  and no more than NPAR=',npar
          go to 10
        endif
 
        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Test4'
          write(*,*)'  Compare full sensitivities computed directly'
          write(*,*)'  and via finite differences.'
        endif
 
        call test4(afl,area,difcof,dpar,drey,eqn,gfl,gflafl,
     &    ifs,ijac,indx,ipar,ipivfl,iwrite,ldafl,maxcofrb,maxelm,
     &    maxnew,maxnfl,ncofrb,nelem,neqnfl,nlband,node,np,npar,
     &    nparb,nparf,nsenfl,par,parafl,phifl,region,resfl,
     &    senfl,splflo,tauflo,tolnew,xrange,yrange)
c
c  TEST5
c
      elseif(leqi(command,'test5'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Test5:'
          write(*,*)'  Compare RB*Rfact and basis vectors.'
        endif
 
        call test5(maxcofrb,maxnfl,ncofrb,neqnfl,rb,rbase)
c
c  TIME
c
      elseif(leqi(command,'time'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - Time:'
          write(*,*)'  Report current time,'
          write(*,*)'  time elapsed since last TIME call,'
          write(*,*)'  time elapsed since the program began.'
        endif
c
c  TOLNEW=
c
      elseif(leqi(command(1:6),'tolnew'))then

        if(leqi(command(1:7),'tolnew='))then
          read(command(8:),*)tolnew
        else
          write(*,*)'Enter value for TOLNEW:'
          read(*,*)tolnew
          write(17,*)tolnew
        endif

        if(iprint.gt.0)then
          write(*,*)'TOLNEW set to ',tolnew
        endif
c
c  TOLOPT=
c
      elseif(leqi(command(1:6),'tolopt'))then

        if(leqi(command(1:7),'tolopt='))then
          read(command(8:),*)tolopt
        else
          write(*,*)'Enter value for TOLOPT:'
          read(*,*)tolopt
          write(17,*)tolopt
        endif

        if(iprint.gt.0)then
          write(*,*)'TOLOPT set to ',tolopt
        endif
c
c  TOLSIM=
c
      elseif(leqi(command(1:6),'tolsim'))then

        if(leqi(command(1:7),'tolsim='))then
          read(command(8:),*)tolsim
        else
          write(*,*)'Enter value for TOLSIM:'
          read(*,*)tolsim
          write(17,*)tolsim
        endif

        if(iprint.gt.0)then
          write(*,*)'TOLSIM set to ',tolsim
        endif
c
c  TecPlot
c
      elseif(leqi(command,'tecplot'))then

        if(iprint.gt.0)then
          write(*,*)'ARBY3 - TecPlot:'
          write(*,*)'  Write data to TECPLOT plot file.'
        endif

        call intprs(gfl,indx,nelem,neqnfl,node,np,p)

        do i=1,np
          u(i)=gfl(indx(1,i))
          v(i)=gfl(indx(2,i))
        enddo

        call wrtec(nelem,node,np,p,tecfil,u,v,xc,yc)
c
c  WATEB=
c
      elseif(leqi(command(1:5),'wateb'))then

        if(leqi(command(1:6),'wateb='))then
          read(command(7:),*)wateb
        else
          write(*,*)'Enter value for WATEB:'
          read(*,*)wateb
          write(17,*)wateb
        endif

        if(iprint.gt.0)then
          write(*,*)'WATEB set to ',wateb
        endif
c
c  WATEP=
c
      elseif(leqi(command(1:5),'watep'))then

        if(leqi(command(1:6),'watep='))then
          read(command(7:),*)watep
        else
          write(*,*)'Enter value for WATEP:'
          read(*,*)watep
          write(17,*)watep
        endif

        if(iprint.gt.0)then
          write(*,*)'WATEP set to ',watep
        endif
c
c  WATEU=
c
      elseif(leqi(command(1:5),'wateu'))then

        if(leqi(command(1:6),'wateu='))then
          read(command(7:),*)wateu
        else
          write(*,*)'Enter value for WATEU:'
          read(*,*)wateu
          write(17,*)wateu
        endif

        if(iprint.gt.0)then
          write(*,*)'WATEU set to ',wateu
        endif
c
c  WATEV=
c
      elseif(leqi(command(1:5),'watev'))then

        if(leqi(command(1:6),'watev='))then
          read(command(7:),*)watev
        else
          write(*,*)'Enter value for WATEV:'
          read(*,*)watev
          write(17,*)watev
        endif

        if(iprint.gt.0)then
          write(*,*)'WATEV set to ',watev
        endif
c
c  XBL=
c
      elseif(leqi(command(1:3),'xbl'))then

        if(leqi(command(1:4),'xbl='))then
          read(command(5:),*)xbl
        else
          write(*,*)'Enter value for XBL:'
          read(*,*)xbl
          write(17,*)xbl
        endif

        if(iprint.gt.0)then
          write(*,*)'XBL set to ',xbl
        endif
c
c  XBR=
c
      elseif(leqi(command(1:3),'xbr'))then

        if(leqi(command(1:4),'xbr='))then
          read(command(5:),*)xbr
        else
          write(*,*)'Enter value for XBR:'
          read(*,*)xbr
          write(17,*)xbr
        endif

        if(iprint.gt.0)then
          write(*,*)'XBR set to ',xbr
        endif
c
c  XMAX=
c
      elseif(leqi(command(1:4),'xmax'))then

        if(leqi(command(1:5),'xmax='))then
          read(command(6:),*)xmax
        else
          write(*,*)'Enter value for XMAX:'
          read(*,*)xmax
          write(17,*)xmax
        endif

        if(iprint.gt.0)then
          write(*,*)'XMAX set to ',xmax
        endif
c
c  XMIN=
c
      elseif(leqi(command(1:4),'xmin'))then

        if(leqi(command(1:5),'xmin='))then
          read(command(6:),*)xmin
        else
          write(*,*)'Enter value for XMIN:'
          read(*,*)xmin
          write(17,*)xmin
        endif

        if(iprint.gt.0)then
          write(*,*)'XMIN set to ',xmin
        endif
c
c  XPROF=
c
      elseif(leqi(command(1:5),'xprof'))then

        if(leqi(command(1:6),'xprof='))then
          read(command(7:),*)xprof
        else
          write(*,*)'Enter value for XPROF:'
          read(*,*)xprof
          write(17,*)xprof
        endif

        if(iprint.gt.0)then
          write(*,*)'XPROF set to ',xprof
        endif
c
c  XRANGE=
c
      elseif(leqi(command(1:6),'xrange'))then

        if(leqi(command(1:7),'xrange='))then
          read(command(8:),*)xrange
        else
          write(*,*)'Enter value for XRANGE:'
          read(*,*)xrange
          write(17,*)xrange
        endif

        if(iprint.gt.0)then
          write(*,*)'XRANGE set to ',xrange
        endif
c
c  YBL=
c
      elseif(leqi(command(1:3),'ybl'))then

        if(leqi(command(1:4),'ybl='))then
          read(command(5:),*)ybl
        else
          write(*,*)'Enter value for YBL:'
          read(*,*)ybl
          write(17,*)ybl
        endif

        if(iprint.gt.0)then
          write(*,*)'YBL set to ',ybl
        endif
c
c  YBR=
c
      elseif(leqi(command(1:3),'ybr'))then

        if(leqi(command(1:4),'ybr='))then
          read(command(5:),*)ybr
        else
          write(*,*)'Enter value for YBR:'
          read(*,*)ybr
          write(17,*)ybr
        endif

        if(iprint.gt.0)then
          write(*,*)'YBR set to ',ybr
        endif
c
c  YMAX=
c
      elseif(leqi(command(1:4),'ymax'))then

        if(leqi(command(1:5),'ymax='))then
          read(command(6:),*)ymax
        else
          write(*,*)'Enter value for YMAX:'
          read(*,*)ymax
          write(17,*)ymax
        endif

        if(iprint.gt.0)then
          write(*,*)'YMAX set to ',ymax
        endif
c
c  YMIN=
c
      elseif(leqi(command(1:4),'ymin'))then

        if(leqi(command(1:5),'ymin='))then
          read(command(6:),*)ymin
        else
          write(*,*)'Enter value for YMIN:'
          read(*,*)ymin
          write(17,*)ymin
        endif

        if(iprint.gt.0)then
          write(*,*)'YMIN set to ',ymin
        endif
c
c  YRANGE=
c
      elseif(leqi(command(1:6),'yrange'))then

        if(leqi(command(1:7),'yrange='))then
          read(command(8:),*)yrange
        else
          write(*,*)'Enter value for YRANGE:'
          read(*,*)yrange
          write(17,*)yrange
        endif

        if(iprint.gt.0)then
          write(*,*)'YRANGE set to ',yrange
        endif
c
c  Unrecognized command
c
      else

        write(*,*)' '
        write(*,*)'ARBY3 - Warning!'
        write(*,*)'  Unrecognized command: '//command

      endif

      go to 10

c
c  End of file.
c
110   continue
      write(*,*)' '
      write(*,*)'ARBY3 - Fatal error!'
      write(*,*)'  End of file in input.'
      stop
c
c  Error in input file.
c
120   continue
      write(*,*)' '
      write(*,*)'ARBY3 - Fatal error!'
      write(*,*)'  Error in input file.'
      stop
      end
      subroutine ch_cap ( ch )

c*********************************************************************72
c
cc CH_CAP capitalizes a single character.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character CH, the character to capitalize.
c
      implicit none

      character ch
      integer itemp

      itemp = ichar ( ch )
 
      if ( 97 .le. itemp .and. itemp .le. 122 ) then
        ch = char ( itemp - 32 )
      end if
 
      return
      end
      subroutine diffprb(arb,area,epsdif,grb,indx,maxcofrb,maxelm,
     &  maxnfl,nbcrb,ncofrb,nelem,nferb,node,np,npar,nparf,nx,
     &  ny,par,phirb,rb,resrb,reynld,tauflo,xc,xrange,yc,yrange)
c
c***********************************************************************
c
cc DIFFPRB estimates the jacobian of the reduced function, using
c  finite differences.
c
c
c  DIFFPRB was modified on 31 July 1996.
c
c
c  ARB    Output, double precision ARB(MAXNRB,MAXNRB).
c         ARB contains the Jacobian or Picard matrix for the reduced
c         Navier Stokes system, stored as a dense NCOFRB by NCOFRB 
c         array.
c
c  AREA   Input, double precision AREA(3,MAXELM).
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  EPSDIF Input, double precision EPSDIF.
c         EPSDIF is a small quantity, which is used to compute the 
c         perturbations for the finite difference approximations.
c
c  GRB    Input, double precision GRB(NCOFRB).
c         GRB contains the reduced basis coefficients of the current
c         estimate of the state solution.
c
c  GRBTMP Workspace, double precision GRBTMP(NCOFRB).
c
c  MAXNRB Input, integer MAXNRB.
c         MAXNRB is the maximum number of equations allowed for the 
c         reduced basis system.
c
c  NELEM  integer NELEM.
c         NELEM is the number of elements.
c         NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NCOFRB Input, integer NCOFRB.
c         NCOFRB is the number of basis functions used for the
c         reduced basis method.  (The first basis vector is labeled
c         "0").  In this program, that amounts to the number of columns
c         in the matrix RB.  NCOFRB is also the number of reduced basis 
c         state equations, and reduced basis coefficients GRB.
c
c  PHIRB  Input, double precision PHIRB(3,NCOFRB,15,NELEM).
c         PHIRB contains the values of a finite element basis function
c         or its X or Y derivative, in a given element, at a given
c         quadrature point, for a particular reduced basis function.
c
c         For PHIRB(I,J,K,L), index J refers to the reduced basis
c         basis functions, for J=0 to NCOFRB.
c
c         The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c           For the quadrature point I, and reduced basis function J,
c           in element L, PHIRB(I,J,K,L) represents the value of:
c
c             K=1, WUrb, the finite element U velocity basis function;
c             K=2, dWUrbdX, the X derivative of WUrb;
c             K=3, dWUrbdY, the Y derivative of WUrb;
c             K=4, WVrb, the finite element V velocity basis function;
c             K=5, dWVrbdX, the X derivative of WVrb;
c             K=6, dWVrbdY, the Y derivative of WVrb;
c             K=7, Q, the finite element pressure basis function.
c             K=8, dQrbdX, the X derivative of Qrb;
c             K=9, dQrbdY, the Y derivative of Qrb.
c             K=10, WU0rb, same as WUrb, with zero BC.
c             K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c             K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c             K=13, WV0rb, same as WVrb, with zero BC.
c             K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c             K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  RESRB  Workspace, double precision RESRB(NCOFRB).
c         RESRB contains the residual in the reduced basis equations,
c         for the parameter values PAR and reduced basis coefficients GRB.
c
c  REYNLD Input, double precision REYNLD.
c         REYNLD is the current value of the Reynolds number.
c         Normally, REYNLD is stored as PARA(NPARF+NPARB+1).
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxelm
      integer maxnfl
      integer nelem
      integer ncofrb
      integer np
      integer npar
      integer nparf
c
      double precision arb(maxcofrb,ncofrb)
      double precision area(3,nelem)
      double precision delta
      double precision epsdif
      double precision grb(ncofrb)
c
c  FORTRAN 90 local array:
c
      double precision grbtmp(ncofrb)
      integer i
      integer indx(3,np)
      integer j
      integer nbcrb
      integer nferb
      integer node(6,maxelm)
      integer nx
      integer ny
      double precision par(npar)
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision rb(maxnfl,maxcofrb)
      double precision resrb(ncofrb)
      double precision reynld
      double precision tauflo(nparf)
      double precision xc(np)
      double precision xrange
      double precision yc(np)
      double precision yrange
c
c  Get the function value at the base value.
c
      call fxrb(area,grb,indx,maxcofrb,maxelm,maxnfl,nbcrb,ncofrb,
     &  nelem,nferb,node,np,npar,nparf,nx,ny,par,phirb,rb,
     &  resrb,reynld,tauflo,xc,xrange,yc,yrange)
c
c  Start each column of the jacobian equal to F(GRB).
c
      do i=1,ncofrb
        do j=1,ncofrb
          arb(i,j)=resrb(i)
        enddo
      enddo
c
      do j=1,ncofrb
c
c  Perturb G(J).
c
        do i=1,ncofrb
          grbtmp(i)=grb(i)
        enddo
        delta=epsdif*(1.0+abs(grb(j)))
        grbtmp(j)=grb(j)+delta
c
c  Evaluate F(I) at the perturbed value G(J).
c
        call fxrb(area,grbtmp,indx,maxcofrb,maxelm,maxnfl,nbcrb,ncofrb,
     &    nelem,nferb,node,np,npar,nparf,nx,ny,par,phirb,rb,
     &    resrb,reynld,tauflo,xc,xrange,yc,yrange)
c
c  Estimate the dependence ARB(I,J)=dF(I)/dG(J)
c
        do i=1,ncofrb
          arb(i,j)=(resrb(i)-arb(i,j))/delta
        enddo
 
      enddo
 
      return
      end
      subroutine difsenfl(afl,area,difcof,dpar,eqn,gfl,gflafl,ifs,
     &  ijac,indx,ipar,ipivfl,iwrite,ldafl,maxcofrb,maxelm,maxnew,
     &  maxnfl,
     &  ncofrb,nelem,neqnfl,nlband,node,np,npar,nparb,nparf,par,
     &  parafl,phifl,region,resfl,senfl,splflo,tauflo,
     &  tolnew,xrange,yrange)
c
c***********************************************************************
c  
cc DIFSENFL computes a central difference estimate for the first NCOFRB 
c  derivatives of the full solution GFL with respect to the IPAR-th 
c  parameter.
c
c
c  NOTE: DIFSENFL IS RATHER INEFFICIENT.  ALTHOUGH SOME SOLUTIONS
c  ARE USED SEVERAL TIMES, DIFSENFL RECOMPUTES THEM EACH TIME.
c  A CORRECTION OF THIS PROBLEM WOULD BE TO COMPUTE THE ENTIRE
c  TRIANGLE OF COEFFICIENTS FIRST, AND THEN COMPUTE JUST THE
c  SOLUTIONS NEEDED ONCE.
c
c
c  DIFSENFL was modified on 01 July 1996.
c
c
c  AFL    Workspace, double precision AFL(LDAFL,MAXNFL).
c
c         AFL contains the Jacobian matrix for the full system,
c         stored in LINPACK general band storage mode.
c         The two dimensional array is of logical dimensions LDAFL by 
c         NEQNFL.
c
c  AREA   Input, double precision AREA(3,NELEM).
c
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  DCOF   Workspace, double precision DCOF(0:NDIF).
c         DCOF contains the coefficients needed to approximate
c         the NDIF-th derivative of a function F.
c
c  DPAR   Input, double precision DPAR.
c         DPAR is the suggested increment in the parameter value,
c         to be used during the finite difference estimations.
c
c  EQN    Input, character*2 EQN(NEQNFL).
c         EQN records the "type" of each equation that will be generated, and
c         which is associated with an unknown.  Note that most boundary 
c         conditions do not result in an equation.  The current values are:
c
c         'U'  The horizontal momentum equation.
c         'UB' The condition U=0 applied at a node on the bump.
c         'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c         'UW' The condition U=0 applied at a node on a fixed wall.
c
c         'V'  The vertical momentum equation.
c         'VB' The condition V=0 applied at a node on the bump.
c         'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c         'VW' The condition V=0 applied at a node on a fixed wall.
c
c         'P'  The continuity equation.
c         'PB' The condition P=0 applied at (XMAX,YMAX).
c
c  GFL    Input, double precision GFL(NEQNFL), the current solution 
c         estimate for the full problem.
c
c  GFLDIF Output, double precision GFLDIF(NEQNFL).
c         GFLDIF stores the value of GFL at which the sensitivities 
c         were approximated by finite differences.
c
c  GFLMAT Input/output, double precision GFLMAT(NEQNFL).
c         GFLMAT stores the value of GFL at which the Jacobian 
c         was generated.
c
c  GFLTMP Workspace, double precision GFLTMP(NEQNFL).
c
c  IFS    Input, integer IFS.
c         1, the inflow is modeled by C0 linear splines.
c         2, the inflow is modeled by C0 quadratic splines.
c
c  IJAC   Input, integer IJAC.
c         IJAC determines the frequency for evaluating and factoring
c         the Jacobian matrix during any particular Newton process.
c
c         1, evaluate the Jacobian on every step of the Newton 
c            iteration.
c
c         n, evaluate the Jacobian only at steps 0, n, 2*n, and so on.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  IPAR   Input, integer IPAR.
c         The index of the parameter to be varied.
c
c  IPIVFL Workspace, integer IPIVFL(NEQNFL), pivot vector for the solution
c         of the full linear system.
c 
c  IWRITE Input, integer IWRITE.
c        IWRITE controls the amount of output printed.
c        0, print out the least amount.
c        1, print out some.
c        2, print out a lot.
c
c  LDAFL  Input, integer LDAFL, the first dimension of the matrix AFL.
c
c  MAXNEW Input, integer MAXNEW, the maximum number of Newton steps
c         that may be taken.  10 should usually be enough.
c
c  MAXNFL Input, integer MAXNFL.
c
c         The maximum number of equations allowed for the full system.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NEQNFL Input, integer NEQNFL.
c         NEQNFL is the number of equations in the full system.
c
c  NCOFRB Input, integer NCOFRB, the number of equations in the reduced 
c         system.
c
c  NLBAND Input, integer NLBAND.
c
c         The lower bandwidth of the matrix AFL.  The zero structure of AFL
c         is assumed to be symmetric, and so NLBAND is also the upper
c         bandwidth of AFL.  
c 
c  NODE   Input, integer NODE(6,NELEM).
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
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c 
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c 
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c 
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the 
c         inflow.  NPARF must be at least 1.  
c
c  PAR    Input, double precision PAR(NPAR).
c 
c         PAR is the current estimate for the parameters.
c
c  PARDIF Output, double precision PARDIF(NPAR).
c         PARDIF contains the parameter values at which the sensitivities
c         were approximated by finite differences.
c
c  PARMAT Input/output, double precision PARMAT(NPAR).
c         PARMAT contains the parameter values at which the Jacobian
c         was evaluated.
c
c  PARTMP Workspace, double precision PARTMP(NPAR).
c
c  PHIFL  Input, double precision PHIFL(3,6,10,NELEM).  
c
c         PHIFL contains the value of a finite element basis function, its
c         derivative, or other information, evaluated at the quadrature 
c         points.
c 
c         The meaning of the entry PHIFL(I,J,K,L) is as follows.  
c         For the quadrature point I, and basis function J, in element L,
c         PHIFL(I,J,K,L) represents the value of:
c
c           K= 1, W, the finite element basis function for velocities;
c           K= 2, dWdX, the X derivative of W;
c           K= 3, dWdY, the Y derivative of W;
c           K= 4, Q, the finite element basis function for pressures;
c           K= 5, dQdX, the X derivative of Q;
c           K= 6, dQdY, the Y derivative of Q;
c           K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c           K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c           K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c           K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c         In particular, PHIFL(I,J,K,L) is the value of the quadratic 
c         basis function W associated with local node J in element L, 
c         evaluated at quadrature point I.
c 
c         Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6, 
c         since there are only three linear basis functions.
c
c  REGION Input, character*20 REGION.
c         REGION specifies the flow region.
c
c         'cavity', a driven cavity, 1 unit on each side, open on 
c         the top with a tangential velocity specification there.
c
c         'channel', a channel, 10 units long by 3 high, inflow on 
c         the left, outflow on the right, with a bump on the bottom.
c
c         'step', a channel, 12 units long by 3 high, inflow on the
c         left, outflow on the right, with a step on the bottom.
c
c  RESFL  Workspace, double precision RESFL(NEQNFL), the residual in the
c         full basis equations.
c
c  SENFL  Output, double precision SENFL(MAXNFL,NCOFRB).
c         SENFL contains the finite difference approximation to the
c         first several order sensitivities of the
c         solution with respect to the REYNLD parameter.
c
c  SPLFLO Input, double precision SPLFLO(NPARF+2).
c
c         SPLFLO contains the spline coefficients for the inflow.
c
c  TAUFLO Input, double precision TAUFLO(NPARF+2).
c
c         TAUFLO contains the location of the spline abscissas for
c         the inflow.  There are NPARF+2 of them, because the end 
c         values of the spline are constrained to have particular 
c         values.
c
c  TOLNEW Input, double precision TOLNEW, the Newton tolerance.
c         NEWTFL is asked to find an approximate solution so that
c         the maximum absolute value of all the residuals is no more
c         than TOLNEW.  A value such as 10E-7 is often reasonable,
c         though this depends on the actual equations being solved.
c
c  XC     Input, double precision XC(NP).
c
c         The X coordinates of the nodes.
c 
c  YC     Input, double precision YC(NP).
c
c         The Y coordinates of the nodes.
c 
      implicit double precision (a-h,o-z)
c
      integer ldafl
      integer maxcofrb
      integer maxelm
      integer maxnfl
      integer ncofrb
      integer nelem
      integer neqnfl
      integer np
      integer npar
      integer nparf
c
      double precision afl(ldafl,neqnfl)
      double precision area(3,nelem)
      double precision difcof(ncofrb)
      double precision dpar
      character*2 eqn(neqnfl)
      double precision gfl(neqnfl)
      double precision gflafl(neqnfl)
c
c  FORTRAN 90 local array:
c
      double precision gfltmp(neqnfl)
      integer i
      integer ierror
      integer ifs
      integer ijac
      integer indx(3,np)
      integer ipar
      integer ipivfl(neqnfl)
      integer iwrite
      integer j
      integer maxnew
      integer ndif
      integer nlband
      integer node(6,nelem)
      integer nparb
      integer numnew
      double precision par(npar)
      double precision parafl(npar)
c
c  FORTRAN 90 local array.
c
      double precision partmp(npar)
      double precision phifl(3,6,10,nelem)
      character*20 region
      double precision resfl(neqnfl)
      double precision rmax
      double precision senfl(maxnfl,maxcofrb)
      double precision splflo(nparf+2)
      double precision tauflo(nparf+2)
      double precision tolnew
      double precision xrange
      double precision yrange
c
c  Zero out the SENFL array.
c
      do i=1,neqnfl
        do j=1,ncofrb
          senfl(i,j)=0.0
        enddo
      enddo
c
      write(*,*)' '
      write(*,*)'  DIFSENFL: DPAR=',dpar
c
c  Compute difference NDIF, for NDIF=0 to NCOFRB.
c
      do ndif=1,ncofrb
 
        if(ndif.eq.0)then
 
          do i=1,neqnfl
            senfl(i,ndif)=gfl(i)
          enddo
 
        else
 
          if(iwrite.ge.2)then
            write(*,*)' '
            write(*,*)'DIFSENFL - Computing difference NDIF=',ndif
          endif
c
c  Get the NDIF-1 order difference coefficients.
c
          call difset(difcof,dpar,iwrite,ndif)
c
c  Evaluate the solution at several values of the parameter.
c
          do i=0,ndif
c
c  Copy the parameters, but reset the IPAR-th parameter value.
c
            do j=1,npar
              partmp(j)=par(j)
            enddo
            partmp(ipar)=par(ipar)+(2*i-ndif)*dpar
            write(*,*)'J=',j,' PAR(IPAR)=',partmp(ipar)
c
c  Estimate the solution GTMP at parameters PARTMP.
c
            do j=1,neqnfl
              gfltmp(j)=gfl(j)
            enddo
c
c  Call NEWTFL to get the solution more closely.
c
            call newtfl(afl,area,eqn,gfltmp,gflafl,ierror,ifs,ijac,
     &        indx,ipivfl,iwrite,ldafl,maxelm,maxnew,nelem,neqnfl,
     &        nlband,node,np,npar,nparb,nparf,numnew,partmp,parafl,
     &        phifl,
     &        region,resfl,rmax,splflo,tauflo,tolnew,xrange,yrange)
 
            if(ierror.ne.0)then
              write(*,*)' '
              write(*,*)'DIFSENFL - Fatal error!'
              write(*,*)'  NEWTFL failed, with IERROR=',ierror
              stop
            endif
c
c  Add the term associated with this solution to the estimate
c  of the NDIF-th derivative.
c
            do j=1,neqnfl
              senfl(j,ndif)=senfl(j,ndif)+difcof(i+1)*gfltmp(j)
            enddo
 
          enddo
        endif
      enddo
 
      return
      end
      subroutine difsenrb(arb,area,difcof,dpar,grb,grbarb,
     &  indx,ipar,ipivrb,iwrite,maxcofrb,maxelm,maxnew,maxnfl,nbcrb,
     &  ncofrb,nelem,nferb,node,np,npar,nparf,nx,ny,par,
     &  pararb,phirb,rb,resrb,senrb,tauflo,tolnew,xc,xrange,yc,yrange)
c
c***********************************************************************
c  
cc DIFSENRB estimates the reduced sensitivities using finite differences.
c
c
c  WARNING: The denominators used in the difference calculations
c  get very small if the original increment is smaller than 1.
c  It is strongly suggested that the parameter increment not be
c  made too small!
c
c
c  DIFSENRB was modified on 01 August 1996.
c
c
c  ARB    Workspace, double precision ARB(MAXNRB,MAXNRB).
c         ARB contains the Jacobian or Picard matrix for the reduced
c         Navier Stokes system, stored as a dense NCOFRB by NCOFRB 
c         array.
c
c  AREA   Input, double precision AREA(3,MAXELM).
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  DCOF   Workspace, double precision DCOF(0:NDIF).
c         DCOF contains the coefficients needed to approximate
c         the NDIF-th derivative of a function F.
c
c  DPAR   Input, double precision DPAR.
c         DPAR is the suggested increment in the parameter value,
c         to be used during the finite difference estimations.
c
c  GRB    Input, double precision GRB(NCOFRB).
c         GRB contains the reduced basis coefficients of the current
c         estimate of the state solution.
c
c  GRBARB Output, double precision GRBARB(NCOFRB).
c         GRBARB contains the reduced basis coefficients at which
c         the matrix ARB was last evaluated.
c
c  GRBTMP Workspace, double precision GRBTMP(NCOFRB).
c
c  IPAR   Input, integer IPAR.
c         The index of the parameter to be varied.
c
c  IPIVRB Workspace, integer IPIVRB(NCOFRB).
c         IPIVRB is a pivot vector for the solution of the reduced
c         linear system.
c
c  IWRITE Input, integer IWRITE.
c         IWRITE controls the amount of output printed.
c         0, print out the least amount.
c         1, print out some.
c         2, print out a lot.
c
c  MAXNEW Input, integer MAXNEW.
c         MAXNEW is the maximum number of steps to take in one Newton
c         iteration.  A typical value is 20.
c
c  MAXNRB Input, integer MAXNRB.
c         MAXNRB is the maximum number of equations allowed for the 
c         reduced basis system.
c
c  NELEM  Input, integer NELEM.
c         NELEM is the number of elements.
c         NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NCOFRB Input, integer NCOFRB.
c         NCOFRB is the number of basis functions used for the
c         reduced basis method.  (The first basis vector is labeled
c         "0").  In this program, that amounts to the number of columns
c         in the matrix RB.  NCOFRB is also the number of reduced basis 
c         state equations, and reduced basis coefficients GRB.
c
c  NPAR   Input, integer NPAR.
c         NPAR is the number of parameters.
c
c         NPAR = NPARF + NPARB + 1.
c
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c
c  PAR    Input, double precision PAR(NPAR).
c         PAR is the current estimate for the parameters.
c
c         PAR(1:NPARF)             = inflow controls.
c
c         PAR(NPARF+1:NPARF+NPARB) = bump controls.
c
c         PAR(NPARF+NPARB+1)       = the REYNLD parameter.
c
c  PARARB Output, double precision PARARB(NPAR).
c         PARARB contains the parameters where the Picard matrix or
c         Jacobian of the reduced system was generated.
c
c  PARTMP Workspace, double precision PARTMP(NPAR).
c
c  PHIRB  double precision PHIRB(3,NCOFRB,15,NELEM).
c         PHIRB contains the values of a finite element basis function
c         or its X or Y derivative, in a given element, at a given
c         quadrature point, for a particular reduced basis function.
c
c         For PHIRB(I,J,K,L), index J refers to the reduced basis
c         basis functions, for J=0 to NCOFRB.
c
c         The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c           For the quadrature point I, and reduced basis function J,
c           in element L, PHIRB(I,J,K,L) represents the value of:
c
c             K=1, WUrb, the finite element U velocity basis function;
c             K=2, dWUrbdX, the X derivative of WUrb;
c             K=3, dWUrbdY, the Y derivative of WUrb;
c             K=4, WVrb, the finite element V velocity basis function;
c             K=5, dWVrbdX, the X derivative of WVrb;
c             K=6, dWVrbdY, the Y derivative of WVrb;
c             K=7, Q, the finite element pressure basis function.
c             K=8, dQrbdX, the X derivative of Qrb;
c             K=9, dQrbdY, the Y derivative of Qrb.
c             K=10, WU0rb, same as WUrb, with zero BC.
c             K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c             K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c             K=13, WV0rb, same as WVrb, with zero BC.
c             K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c             K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  RESRB  Workspace, double precision RESRB(NCOFRB).
c         RESRB contains the residual in the reduced basis equations,
c         for the parameter values PAR and reduced basis coefficients GRB.
c
c  SENRB  Output, double precision SENRB(MAXNRB,NCOFRB).
c         SENRB contains the first several order sensitivities of the
c         reduced solution with respect to the REYNLD parameter.
c
c         SENRB(I,J) contains the J-th sensitivity of the I-th reduced unknown
c         with respect to REYNLD.
c
c  TOLNEW Input, double precision TOLNEW.
c         TOLNEW is the convergence tolerance for the Newton iteration.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxelm
      integer maxnfl
      integer ncofrb
      integer nelem
      integer np
      integer npar
      integer nparf
c
      double precision arb(maxcofrb,maxcofrb)
      double precision area(3,nelem)
      double precision difcof(maxcofrb)
      double precision dpar
      double precision grb(maxcofrb)
      double precision grbarb(maxcofrb)
c
c  FORTRAN 90 local array:
c
      double precision grbtmp(maxcofrb)
      integer i
      integer icof
      integer ierror
      integer indx(3,np)
      integer ipar
      integer ipivrb(maxcofrb)
      integer iwrite
      integer j
      integer jcof
      integer jdif
      integer maxnew
      integer nbcrb
      integer nferb
      integer node(6,nelem)
      integer nx
      integer ny
      double precision par(npar)
      double precision pararb(npar)
c
c  FORTRAN 90 local array:
c
      double precision partmp(npar)
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision rb(maxnfl,maxcofrb)
      double precision rmax
      double precision resrb(maxcofrb)
      double precision senrb(maxcofrb,maxcofrb)
      double precision tauflo(nparf)
      double precision tolnew
      double precision xc(np)
      double precision xrange
      double precision yc(np)
      double precision yrange
c
c  Zero out the SENRB array.
c
      do i=1,maxcofrb
        do j=1,maxcofrb
          senrb(i,j)=0.0
        enddo
      enddo
c
c  JCOF counts the number of coefficients we will compute on
c  each pass.  We're done on the last pass.
c
      do jcof=1,ncofrb
 
        jdif=jcof-1
 
        if(jdif.eq.0)then
 
          do i=1,ncofrb
            senrb(i,jcof)=grb(i)
          enddo
 
        else
          write(*,*)' '
          write(*,*)'Computing difference order JDIF=',jdif
c
c  Get the JCOF difference coefficients DIFCOF.
c
          call difset(difcof,dpar,iwrite,jcof)
c
c  Evaluate the solution at JCOF values of the parameter.
c
          do icof=1,jcof
c
c  Copy the parameters, but reset the IPAR-th parameter value.
c
            do j=1,npar
              partmp(j)=par(j)
            enddo
            partmp(ipar)=par(ipar)+(2*icof-jcof-1)*dpar
            write(*,*)'ICOF=',ICOF,' PAR(IPAR)=',partmp(ipar)
c
c  Estimate the solution GRBTMP at parameters PARTMP.
c
            do j=1,ncofrb
              grbtmp(j)=grb(j)
            enddo
c
c  Call NEWTRB to get the solution more closely.
c
            write(*,*)'About to call NEWTRB'
            call newtrb(arb,area,grbtmp,grbarb,ierror,indx,ipivrb,
     &        iwrite,maxcofrb,maxelm,maxnew,maxnfl,nbcrb,ncofrb,nelem,
     &        nferb,node,np,npar,nparf,nx,ny,partmp,pararb,phirb,
     &        rb,resrb,rmax,tauflo,tolnew,xc,xrange,yc,yrange)
 
            if(ierror.ne.0)then
              write(*,*)' '
              write(*,*)'DIFSENRB - Fatal error!'
              write(*,*)'  NEWTRB failed, with IERROR=',ierror
              stop
            endif
c
c  Add the term associated with this solution to the estimate
c  of the JDIF-th derivative.
c
            write(*,*)'ABOUT TO ADD BLEEDING TERM'
            do j=1,ncofrb
              senrb(j,jcof)=senrb(j,jcof)+difcof(icof)*grbtmp(j)
            enddo
 
          enddo
 
        endif
 
      enddo
 
      return
      end
      subroutine flowbc(ifs,npar,nparf,partar,region,splflo,tauflo,
     &  ubc,vbc,xrange,xval,yrange,yval)
c
c*******************************************************************
c
cc FLOWBC computes the specified boundary values of velocity for a
c  given position as determined by the value of the parameters.
c
c
c  FLOWBC was modified on 08 August 1996.
c
c
c  IFS    
c    Input, integer IFS.
c    IFS is the inflow shape option.
c    0, piecewise constant function.
c    1, piecewise linear function.
c    2, piecewise quadratic function.
c
c  NPAR   
c    Input, integer NPAR.
c    NPAR is the number of parameters.
c      NPAR = NPARF + NPARB + 1.
c    The parameters control the shape and strength of the inflow, the shape of
c    the bump, and the value of the Reynolds number.
c
c  NPARF  
c    Input, integer NPARF.
c    NPARF is the number of parameters associated with the
c    inflow.  NPARF must be at least 1.
c
c  PAR    
c    Input, double precision PAR(NPAR).
c    PAR contains the values of the problem parameters.
c
c      PAR(1:NPARF)             = inflow controls.
c      PAR(NPARF+1:NPARF+NPARB) = bump controls.
c      PAR(NPARF+NPARB+1)       = the REYNLD parameter.
c
c  REGION 
c    Input, character*20 REGION.
c    REGION specifies the flow region.
c
c    'cavity', a driven cavity, 1 unit on each side, open on
c    the top with a tangential velocity specification there.
c
c    'channel', a channel, 10 units long by 3 high, inflow on
c    the left, outflow on the right, with a bump on the bottom.
c
c    'step', a channel, 12 units long by 3 high, inflow on the
c    left, outflow on the right, with a step on the bottom.
c
c  SPLFLO 
c    Workspace, double precision SPLFLO(NPARF+2).
c    SPLFLO contains the spline coefficients for the inflow.
c
c  TAUFLO 
c    Workspace, double precision TAUFLO(NPARF+2).
c    TAUFLO contains the location of the spline abscissas for
c    the inflow.  There are NPARF+2 of them, because the end
c    values of the spline are constrained to have particular
c    values.
c
c  UBC    
c    Output, double precision UBC.
c    UBC is the value of the horizontal velocity specified 
c    at (XVAL,YVAL).
c
c  VBC   
c    Output, double precision VBC.
c    VBC is the value of the vertical velocity specified at (XVAL,YVAL).
c
c  XRANGE 
c    Input, double precision XRANGE.
c    XRANGE is the total width of the region.
c
c  XVAL   
c    Input, double precision XVAL.
c    XVAL is the X coordinate of the point on the inflow boundary at 
c    which the specified velocity is desired.
c
c  YRANGE 
c    Input, double precision YRANGE.
c    YRANGE is the total width of the region.
c
c  YVAL  
c    Input, double precision YVAL.
c    YVAL is the Y coordinate of the point on the inflow boundary at 
c    which the specified velocity is desired.
c
      implicit double precision (a-h,o-z)
c
      integer npar
      integer nparf
c
      integer i
      integer ifs
      logical leqi
      double precision partar(npar)
      character*20 region
      double precision splflo(nparf+2)
      double precision tauflo(nparf+2)
      double precision ubc
      double precision vbc
      double precision xrange
      double precision xval
      double precision yrange
      double precision yval
c
c  Inflow points for the cavity have the form (X,YRANGE).
c
      if(leqi(region,'cavity'))then

        if(nparf.eq.1)then
          tauflo(1)=xrange/2.0
        else
          do i=1,nparf
            tauflo(i)=xrange*dble((i-1))/dble(nparf-1)
          enddo
        endif

        do i=1,nparf
          splflo(i)=partar(i)
        enddo
 
        if(ifs.eq.0)then
          call pcval(nparf,xval,tauflo,ubc,splflo)
        elseif(ifs.eq.1)then
          call plval(nparf,xval,tauflo,ubc,splflo)
        elseif(ifs.eq.2)then
          call pqval(nparf,xval,tauflo,ubc,splflo)
        else
          write(*,*)' '
          write(*,*)'FlowBC - Fatal error!'
          write(*,*)'  Illegal value of IFS=',ifs
          stop
        endif

        vbc=0.0
c
c  Inflow points for the channel have the form (0,Y).
c
      elseif(leqi(region,'channel'))then

        do i=1,nparf+2
          tauflo(i)=yrange*dble((i-1))/dble(nparf+1)
        enddo

        do i=1,nparf+2
 
          if(i.eq.1)then
            splflo(i)=0.0
          elseif(2.le.i.and.i.le.nparf+1)then
            splflo(i)=partar(i-1)
          elseif(i.eq.nparf+2)then
            splflo(i)=0.0
          endif
 
        enddo

        if(ifs.eq.0)then
          call pcval(nparf+1,yval,tauflo,ubc,splflo)
        elseif(ifs.eq.1)then
          call plval(nparf+2,yval,tauflo,ubc,splflo)
        elseif(ifs.eq.2)then
          call pqval(nparf+2,yval,tauflo,ubc,splflo)
        else
          write(*,*)' '
          write(*,*)'FlowBC - Fatal error!'
          write(*,*)'  Illegal value of IFS=',ifs
          stop
        endif

        vbc=0.0
c
c  Inflow points for the step have the coordinates (0,Y).
c
      elseif(leqi(region,'step'))then

        do i=1,nparf+2
          tauflo(i)=yrange*dble((i-1))/dble(nparf+1)
        enddo

        do i=1,nparf+2
 
          if(i.eq.1)then
            splflo(i)=0.0
          elseif(2.le.i.and.i.le.nparf+1)then
            splflo(i)=partar(i-1)
          elseif(i.eq.nparf+2)then
            splflo(i)=0.0
          endif
 
        enddo

        if(ifs.eq.0)then
          call pcval(nparf+1,yval,tauflo,ubc,splflo)
        elseif(ifs.eq.1)then
          call plval(nparf+2,yval,tauflo,ubc,splflo)
        elseif(ifs.eq.2)then
          call pqval(nparf+2,yval,tauflo,ubc,splflo)
        else
          write(*,*)' '
          write(*,*)'FlowBC - Fatal error!'
          write(*,*)'  Illegal value of IFS=',ifs
          stop
        endif

        vbc=0.0

      endif
 
      return
      end
      subroutine fpbcrb(arb,indx,maxcofrb,maxnfl,nbcrb,ncofrb,
     &  nelem,node,np,nx,ny,rb,xc,xrange,yc,yrange)
c
c***********************************************************************
c
cc FPBCRB evaluates the jacobian of the boundary conditions that form 
c  part of the reduced function.
c
c
c  FPBCRB was written on 01 August 1996.
c
c
c  ARB    double precision ARB(MAXCOFRB,MAXCOFRB).
c         ARB contains the Jacobian or Picard matrix for the reduced
c         Navier Stokes system, stored as an NCOFRB by NCOFRB array. 
c
c  INDX   integer INDX(3,NP).
c         INDX(I,J) contains, for each node J, the global index of U,
c         V and P at that node, or 0 or a negative value.  The global
c         index of U, V, or P is the index of the coefficient vector
c         that contains the value of the finite element coefficient
c         associated with the corresponding basis function at the
c         given node.
c
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  MAXCOFRB
c         Integer MAXCOFRB.
c         MAXCOFRB is the maximum legal value for NCOFRB, the number
c         of coefficients used to specify a particular reduced basis
c         solution.
c
c  MAXNFL integer MAXNFL.
c         MAXNFL is the maximum number of equations or coefficients allowed
c         for the full system.  MAXNFL must be used instead of NEQNFL as
c         the leading dimension of certain multi-dimensional arrays.
c
c  NBCRB  integer NBCRB.
c         NBCRB is the number of independent boundary condition
c         vectors used for the reduced basis.  NBCRB is normally
c         at least 1, and must be no more than MAXBCRB.
c
c  NCOFRB integer NCOFRB.
c         NCOFRB is the number of coefficients needed to determine
c         a particular reduced basis function.
c         NCOFRB is the sum of NBCRB and NFERB.
c
c  NELEM  integer NELEM.
c         NELEM is the number of elements.
c         NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NODE   integer NODE(6,MAXELM) or NODE(6,NELEM).
c         NODE(I,J) contains, for an element J, the global index of
c         the node whose local number in J is I.
c
c         The local ordering of the nodes is suggested by this diagram:
c
c           Global nodes   Elements      NODE
c                                                          1  2  3  4  5  6
c           74  84  94     3-6-1   2     Left element =  (94,72,74,83,73,84)
c                          |  /   /|
c           73  83  93     5 4   4 5     Right element = (72,94,92,83,93,82)
c                          |/   /  |
c           72  82  92     2   1-6-3
c
c  NP     integer NP.
c         NP is the number of nodes used to define the finite element mesh.
c         Typically, the mesh is generated as a rectangular array, with
c         an odd number of nodes in the horizontal and vertical directions.
c         The formula for NP is NP=(2*NX-1)*(2*NY-1).
c
c  NX     integer NX.
c         NX controls the spacing of nodes and elements in
c         the X direction.  There are 2*NX-1 nodes along various
c         lines in the X direction.
c
c         The number of elements along a line in the X direction is
c         NX-1 (or 2*(NX-1) to make a full rectangular strip).
c
c  NY     integer NY.
c         NY controls the spacing of nodes and elements in
c         the Y direction.  There are 2*NY-1 nodes along various
c         lines in the Y direction.
c
c         The number of elements along a line in the Y direction is
c         NY-1 (or 2*(NY-1) to make a full vertical strip).
c
c  RB     double precision RB(MAXNFL,MAXCOFRB).
c
c         RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
c         RB is generated by computing a finite element solution GFL,
c         which is saved for later reference as "GFLRB".
c         GFLRB is copied into the first column of RB.
c         Then, we compute the first NCOFRB derivatives of GFLRB with
c         respect to a parameter.  The first derivative
c         is stored in column 1 of RB, and so on.  
c
c         Now we compute the QR factorization of this matrix.
c
c         We intend that NEQNFL >> NCOFRB, and RB is a matrix with orthogonal
c         columns, so that:
c
c           Transpose(RB) * RB = Identity(1+NCOFRB)
c
c
c         If GFL is any set of finite element coefficients, the corresponding
c         set of reduced basis coefficients can be computed as:
c
c           GRB = Transpose(RB) * GFL
c
c         If GRB is a set of reduced basis coefficients, a corresponding
c         set of finite element coefficients can be computed as:
c
c           GFL = RB * GRB.
c
c         While it is the case that you can expand and then reduce,
c         and always get the same result, it is not the case that
c         when you reduce and then expand you get the same result!
c
c         It is true, for ANY GRB, that
c
c           GRB = Transpose(RB) * RB * GRB
c
c         which follows from Transpose(RB) * RB = Identity(1+NCOFRB).
c
c         However, for a general GFL, it is the case that
c
c           GFL =/= RB * Transpose(RB) * GFL.
c
c         Only if GFL was generated from a reduced basis coefficient
c         vector will equality apply.  In other words, if GFL was generated
c         from a reduced basis coefficient:
c
c           GFL = RB * GRB
c
c         then
c
c           RB * Transpose(RB) * GFL = RB * Transpose(RB) * (RB * GRB)
c           = RB * GRB = GFL
c
c         so in this strictly limited case,
c
c           RB * Transpose(RB) = Identity(NEQNFL).
c
c  XRANGE double precision XRANGE.
c         XRANGE is the total width of the region.
c
c  YRANGE double precision YRANGE.
c         YRANGE is the total height of the region.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxnfl
      integer ncofrb
      integer nelem
      integer np
c
      double precision arb(maxcofrb,maxcofrb)
      double precision dwdx
      double precision dwdy
      integer ibcrb
      integer icoffl
      integer icofrb
      integer icol
      integer ielem
      integer indx(3,np)
      integer inode
      integer iq
      integer nbcrb
      integer node(6,nelem)
      integer nx
      integer ny
      double precision rb(maxnfl,maxcofrb)
      double precision w
      double precision wurb
      double precision xbc
      double precision xc(np)
      double precision xrange
      double precision ybc
      double precision yc(np)
      double precision yrange
c
c  Zero out the BC rows of the matrix.
c
      do ibcrb=1,nbcrb
        do icofrb=1,ncofrb
          arb(ibcrb,icofrb)=0.0
        enddo
      enddo
 
      do ibcrb=1,nbcrb
c
c  For the driven cavity, the boundary collocation points are evenly 
c  spaced between the ends of the  upper boundary.
c
        xbc=xrange*dble(ibcrb)/dble(nbcrb+1)
        ybc=yrange

        icol=1+xbc*dble(nx-1)/xrange
        if(icol.gt.nx-1)icol=nx-1

        ielem=icol*(2*ny-2)-1
c
c  Evaluate the reduced solution UBCRB at (XBC,YBC).
c
        do icofrb=1,ncofrb
          wurb=0.0
          do iq=1,6
            call qbf(ielem,iq,w,dwdx,dwdy,nelem,node,np,xc,xbc,yc,ybc)
            inode=node(iq,ielem)
            icoffl=indx(1,inode)
            wurb=wurb+rb(icoffl,icofrb)*w
          enddo
          arb(ibcrb,icofrb)=wurb
        enddo
 
      enddo
 
      return
      end
      subroutine fpferb(arb,area,grb,maxcofrb,maxelm,nbcrb,ncofrb,
     &  nelem,nferb,phirb,reynld)
c
c***********************************************************************
c
cc FPFERB evaluates the reduced basis jacobian directly (that is,
c  without computing the full basis jacobian first).
c
c  FPFERB is given
c
c    GRB, the reduced basis coefficients of an approximate solution,
c    PHIRB, the reduced basis functions, evaluated at the quadrature 
c      points,
c    REYNLD, the current Reynolds number,
c
c  and computes 
c
c    ARB, the reduced basis jacobian of the Navier Stokes  
c      equations.
c
c
c  FPFERB was modified on 01 August 1996.
c
c
c  ARB    
c    double precision ARB(MAXCOFRB,MAXCOFRB).
c    ARB contains the Jacobian or Picard matrix for the reduced
c    Navier Stokes system, stored as an NCOFRB by NCOFRB array. 
c
c  AREA   
c    double precision AREA(3,MAXELM).
c    AREA contains a common factor multiplying the term associated
c    with a quadrature point in a given element, namely,
c
c      AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c    or, if the element is isoperimetric,
c
c      AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c    Here Ar(IELEM) represents the area of element IELEM.
c
c  GRB    
c    double precision GRB(NCOFRB).
c    GRB contains the reduced basis coefficients of the current
c    estimate of the state solution.
c
c  MAXCOFRB
c    integer MAXCOFRB.
c    MAXCOFRB is the maximum legal value for NCOFRB, the number
c    of coefficients used to specify a particular reduced basis
c    solution.
c
c  MAXELM 
c    integer MAXELM.
c    MAXELM is the maximum number of elements.
c
c  NBCRB  
c    integer NBCRB.
c    NBCRB is the number of independent boundary condition
c    vectors used for the reduced basis.  NBCRB is normally
c    at least 1, and must be no more than MAXBCRB.
c
c  NCOFRB 
c    integer NCOFRB.
c    NCOFRB is the number of coefficients needed to determine
c    a particular reduced basis function.
c    NCOFRB is the sum of NBCRB and NFERB.
c
c  NELEM  
c    integer NELEM.
c    NELEM is the number of elements.
c    NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NFERB  
c    integer NFERB.
c    NFERB is the number of reduced basis coefficients that will
c    be determined via the finite element method.
c
c  PHIRB  
c    double precision PHIRB(3,MAXCOFRB,15,MAXELM).
c    PHIRB contains the values of a finite element basis function
c    or its X or Y derivative, in a given element, at a given
c    quadrature point, for a particular reduced basis function.
c
c    For PHIRB(I,J,K,L), index J refers to the reduced basis
c    basis functions, for J=0 to NCOFRB.
c
c    The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c      For the quadrature point I, and reduced basis function J,
c      in element L, PHIRB(I,J,K,L) represents the value of:
c
c        K=1, WUrb, the finite element U velocity basis function;
c        K=2, dWUrbdX, the X derivative of WUrb;
c        K=3, dWUrbdY, the Y derivative of WUrb;
c        K=4, WVrb, the finite element V velocity basis function;
c        K=5, dWVrbdX, the X derivative of WVrb;
c        K=6, dWVrbdY, the Y derivative of WVrb;
c        K=7, Q, the finite element pressure basis function.
c        K=8, dQrbdX, the X derivative of Qrb;
c        K=9, dQrbdY, the Y derivative of Qrb.
c        K=10, WU0rb, same as WUrb, with zero BC.
c        K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c        K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c        K=13, WV0rb, same as WVrb, with zero BC.
c        K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c        K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  REYNLD 
c    double precision REYNLD.
c    REYNLD is the current value of the Reynolds number.
c    Normally, REYNLD is stored as PARA(NPARF+NPARB+1).
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxelm
      integer ncofrb
      integer nelem
c
      double precision ar
      double precision arb(maxcofrb,maxcofrb)
      double precision area(3,maxelm)
      double precision dqjdx
      double precision dqjdy
      double precision dprbdx
      double precision dprbdy
      double precision durbdx
      double precision durbdy
      double precision dvrbdx
      double precision dvrbdy
      double precision dwu0dx
      double precision dwujdx
      double precision dwu0dy
      double precision dwujdy
      double precision dwv0dx
      double precision dwvjdx
      double precision dwv0dy
      double precision dwvjdy
      double precision grb(ncofrb)
      integer icofrb
      integer ielem
      integer iquad
      integer jcofrb
      logical leqi
      integer nbcrb
      integer nferb
      double precision prb
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision reynld
      double precision urb
      double precision vrb
      double precision wu0
      double precision wuj
      double precision wv0
      double precision wvj
c
      external leqi
c
c  Zero out the FE rows of the matrix.
c
      do icofrb=nbcrb+1,nbcrb+nferb
        do jcofrb=1,ncofrb
          arb(icofrb,jcofrb)=0.0
        enddo
      enddo
c
c  Consider an element IELEM...
c
      do ielem=1,nelem
c
c  ...and a quadrature point IQUAD...
c
        do iquad=1,3
 
          ar=area(iquad,ielem)
c
c  For the given reduced coefficients GRB, and basis functions
c  PHIRB, evaluate U, V, and P, and their spatial derivatives.
c
          call uvpqrb(dprbdx,dprbdy,durbdx,durbdy,dvrbdx,dvrbdy,grb,
     &      ielem,iquad,maxcofrb,maxelm,ncofrb,phirb,prb,urb,vrb)
c
c  Consider FE reduced basis function ICOFRB.
c
          do icofrb=nbcrb+1,nbcrb+nferb

            wu0   =phirb(iquad,icofrb,10,ielem)
            dwu0dx=phirb(iquad,icofrb,11,ielem)
            dwu0dy=phirb(iquad,icofrb,12,ielem)
            wv0   =phirb(iquad,icofrb,13,ielem)
            dwv0dx=phirb(iquad,icofrb,14,ielem)
            dwv0dy=phirb(iquad,icofrb,15,ielem)
c
c  Take the derivative with respect to basis function JCOFRB.
c
            do jcofrb=1,ncofrb

              wuj   =phirb(iquad,jcofrb,1,ielem)
              dwujdx=phirb(iquad,jcofrb,2,ielem)
              dwujdy=phirb(iquad,jcofrb,3,ielem)

              wvj   =phirb(iquad,jcofrb,4,ielem)
              dwvjdx=phirb(iquad,jcofrb,5,ielem)
              dwvjdy=phirb(iquad,jcofrb,6,ielem)

              dqjdx =phirb(iquad,jcofrb,8,ielem)
              dqjdy =phirb(iquad,jcofrb,9,ielem)
c
c  The horizontal momentum equations.
c
              arb(icofrb,jcofrb)=arb(icofrb,jcofrb)+ar*
     &          (dwujdx*dwu0dx + dwujdy*dwu0dy+reynld
     &          *(wuj*durbdx+urb*dwujdx+wvj*durbdy+vrb*dwujdy+dqjdx)
     &          *wu0)
c
c  The vertical momentum equations.
c
              arb(icofrb,jcofrb)=arb(icofrb,jcofrb)+ar*
     &          (dwvjdx*dwv0dx + dwvjdy*dwv0dy +reynld
     &          *(wuj*dvrbdx+urb*dwvjdx+wvj*dvrbdy+vrb*dwvjdy+dqjdy)
     &          *wv0)
 
            enddo
          enddo
        enddo
      enddo
 
      return
      end
      subroutine fpfl(afl,area,eqn,gfl,indx,ldafl,maxelm,nelem,neqnfl,
     &  nlband,node,np,npar,nparb,nparf,par,phifl)
c
c***********************************************************************
c
cc FPFL computes the jacobian of the Navier Stokes residual function
c  evaluated at the full solution G.
c
c
c  The differentiated Navier Stokes functions have the form:
c
c
c  d U-Eqn/d U-Coef:
c
c    Integral
c
c      dWj/dx * dWi/dx + dWj/dy * dWi/dy
c    + reynld * (Wj*dUold/dx + Uold*dWj/dx+ Vold*dWj/dy) * Wi dx dy
c
c  d U-Eqn/d V-Coef:
c
c    Integral
c
c    reynld * Wj*dUold/dy * Wi dx dy
c
c  d U-Eqn/d P-Coef:
c
c    Integral
c
c    reynld * dQj/dx * Wi dx dy
c
c  d V-Eqn/d U-Coef:
c
c    Integral
c
c    reynld * Wj*dVold/dx * Wi dx dy
c
c  d V-Eqn/d V-Coef:
c
c    Integral
c
c      dWj/dx * dWi/dx + dWj/dy * dWi/dy
c    + reynld * (Uold*dWj/dx + Wj*dVold/dy + Vold*dWj/dy) * Wi dx dy
c
c  d V-Eqn/d P-Coef:
c
c    Integral
c
c    reynld * dQj/dy * Wi dx dy
c
c  d P-Eqn/d U-Coef:
c
c    Integral
c
c      dWj/dx * Qi dx dy
c
c  d P-Eqn/d V-Coef:
c
c    Integral
c
c      dWj/dy * Qi dx dy
c
c
c  AFL    Output, double precision A(LDAFL,NEQNFL), contains the
c         value of D F(I)/D X(J) for each of the NEQNFL residual
c         functions F(I) with respect to each of the unknown
c         coefficients X(J).
c
c  AREA   Input, double precision AREA(3,NELEM).
c
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  EQN    Input, character*2 EQN(NEQNFL).
c         EQN records the "type" of each equation that will be generated, and
c         which is associated with an unknown.  Note that most boundary 
c         conditions do not result in an equation.  The current values are:
c
c         'U'  The horizontal momentum equation.
c         'UB' The condition U=0 applied at a node on the bump.
c         'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c         'UW' The condition U=0 applied at a node on a fixed wall.
c
c         'V'  The vertical momentum equation.
c         'VB' The condition V=0 applied at a node on the bump.
c         'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c         'VW' The condition V=0 applied at a node on a fixed wall.
c
c         'P'  The continuity equation.
c         'PB' The condition P=0 applied at (XMAX,YMAX).
c
c  GFL    Input, double precision GFL(NEQNFL).
c
c         G is the current solution vector, in which are stored 
c         the finite element coefficients that define the velocity
c         and pressure functions, U, V and P.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  LDAFL  Input, integer LDAFL, the first dimension of the matrix AFL.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NEQNFL Input, integer NEQNFL, the number of finite element equations used
c         to define the horizontal and vertical velocities and the
c         pressure.
c 
c  NLBAND Input, integer NLBAND.
c
c         The lower bandwidth of the matrix A.  The zero structure of A
c         is assumed to be symmetric, and so NLBAND is also the upper
c         bandwidth of A.  
c 
c  NODE   Input, integer NODE(6,NELEM).
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
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c 
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c 
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c 
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the 
c         inflow.  NPARF must be at least 1.  
c
c  PAR    Input, double precision PAR(NPAR).
c
c         PAR is the current set of parameter values, including the
c         Reynolds parameter, the flow parameters, and the bump parameters.
c
c  PHIFL  Input, double precision PHIFL(3,6,10,NELEM).  
c
c         PHIFL contains the value of a finite element basis function, its
c         derivative, or other information, evaluated at the quadrature 
c         points.
c 
c         The meaning of the entry PHIFL(I,J,K,L) is as follows.  
c         For the quadrature point I, and basis function J, in element L,
c         PHIFL(I,J,K,L) represents the value of:
c
c           K= 1, W, the finite element basis function for velocities;
c           K= 2, dWdX, the X derivative of W;
c           K= 3, dWdY, the Y derivative of W;
c           K= 4, Q, the finite element basis function for pressures;
c           K= 5, dQdX, the X derivative of Q;
c           K= 6, dQdY, the Y derivative of Q;
c           K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c           K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c           K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c           K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c         In particular, PHIFL(I,J,K,L) is the value of the quadratic 
c         basis function W associated with local node J in element L, 
c         evaluated at quadrature point I.
c 
c         Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6, 
c         since there are only three linear basis functions.
c
      implicit double precision (a-h,o-z)
c
      integer ldafl
      integer maxelm
      integer nelem
      integer neqnfl
      integer np
      integer npar
      integer nparf
c
      double precision afl(ldafl,neqnfl)
      double precision ar
      double precision area(3,maxelm)
      double precision dpdx
      double precision dpdy
      double precision dqjdx
      double precision dqjdy
      double precision dudx
      double precision dudy
      double precision dvdx
      double precision dvdy
      double precision dwidx
      double precision dwidy
      double precision dwjdx
      double precision dwjdy
      character*2 eqn(neqnfl)
      double precision gfl(neqnfl)
      integer i
      integer ielem
      integer ihor
      integer indx(3,np)
      integer ip
      integer iprs
      integer iq
      integer iquad
      integer iuse
      integer iver
      integer j
      integer jhor
      integer jp
      integer jprs
      integer jq
      integer jver
      logical leqi
      integer nlband
      integer node(6,nelem)
      integer nparb
      double precision p
      double precision par(npar)
      double precision phifl(3,6,10,nelem)
      double precision qi
      double precision reynld
      double precision term
      double precision u
      double precision v
      double precision wi
      double precision wj
c
      external leqi
c
      reynld=par(nparf+nparb+1)

      do i=1,3*nlband+1
        do j=1,neqnfl
          afl(i,j)=0.0
        enddo
      enddo
c
c  Approximate the integral by summing over all elements.
c
      do ielem=1,nelem
c
c  Evaluate the integrand at the quadrature points.
c
        do iquad=1,3
 
          ar=area(iquad,ielem)
c
c  Evaluate U, V and P at the IQUAD-th quadrature point.
c
          call uvpqfl(dpdx,dpdy,dudx,dudy,dvdx,dvdy,gfl,ielem,indx,
     &      iquad,nelem,neqnfl,node,np,p,phifl,u,v)
c
c  Consider each node in the element.
c
          do iq=1,6
 
            ip=node(iq,ielem)
 
            wi=phifl(iquad,iq,1,ielem)
            dwidx=phifl(iquad,iq,2,ielem)
            dwidy=phifl(iquad,iq,3,ielem)
            qi=phifl(iquad,iq,4,ielem)
 
            ihor=indx(1,ip)
            iver=indx(2,ip)
            iprs=indx(3,ip)
c
c  Now compute the derivatives of the functions associated
c  with U, V and P, with respect to the coefficients associated
c  with basis vectors at each node of the element.
c
            do jq=1,6
 
              jp=node(jq,ielem)
 
              wj=phifl(iquad,jq,1,ielem)
              dwjdx=phifl(iquad,jq,2,ielem)
              dwjdy=phifl(iquad,jq,3,ielem)
 
              dqjdx=phifl(iquad,jq,5,ielem)
              dqjdy=phifl(iquad,jq,6,ielem)
 
              jhor=indx(1,jp)
              jver=indx(2,jp)
              jprs=indx(3,jp)
c
c  Contributions of the JHOR horizontal velocity to the U, V, and 
c  P equations.
c
              if(leqi(eqn(ihor),'U'))then

                term=ar*(dwjdx*dwidx+dwjdy*dwidy+
     &            reynld*(wj*dudx+u*dwjdx+v*dwjdy)*wi)

                iuse=ihor-jhor+2*nlband+1
                afl(iuse,jhor)=afl(iuse,jhor)+term

              endif
 
              if(leqi(eqn(iver),'V'))then
                term=ar*(reynld*wj*dvdx*wi)
                iuse=iver-jhor+2*nlband+1
                afl(iuse,jhor)=afl(iuse,jhor)+term
              endif
 
              if(iprs.gt.0)then
                if(leqi(eqn(iprs),'P'))then
                  term=ar*dwjdx*qi
                  iuse=iprs-jhor+2*nlband+1
                  afl(iuse,jhor)=afl(iuse,jhor)+term
                endif
              endif
c
c  Contributions of the JVER vertical velocity variable to the
c  U, V and P equations.
c
              if(leqi(eqn(ihor),'U'))then
                term=ar*reynld*wj*dudy*wi
                iuse=ihor-jver+2*nlband+1
                afl(iuse,jver)=afl(iuse,jver)+term
              endif

              if(leqi(eqn(iver),'V'))then

                term=ar*(dwjdx*dwidx+dwjdy*dwidy
     &            +reynld*(u*dwjdx+wj*dvdy+v*dwjdy)*wi)

                iuse=iver-jver+2*nlband+1
                afl(iuse,jver)=afl(iuse,jver)+term
              endif
 
              if(iprs.gt.0)then
                if(leqi(eqn(iprs),'P'))then
                  term=ar*dwjdy*qi
                  iuse=iprs-jver+2*nlband+1
                  afl(iuse,jver)=afl(iuse,jver)+term
                endif
              endif
c
c  Contributions of the JPRS pressure to the U and V equations.
c
              if(jprs.gt.0)then
 
                if(leqi(eqn(ihor),'U'))then
                  term=ar*reynld*dqjdx*wi
                  iuse=ihor-jprs+2*nlband+1
                  afl(iuse,jprs)=afl(iuse,jprs)+term
                endif
 
                if(leqi(eqn(iver),'V'))then
                  term=ar*reynld*dqjdy*wi
                  iuse=iver-jprs+2*nlband+1
                  afl(iuse,jprs)=afl(iuse,jprs)+term
                endif
 
              endif
 
            enddo
          enddo
        enddo
      enddo
c
c  Set up the equations that enforce boundary conditions.
c
      do ip=1,np

        ihor=indx(1,ip)
        iver=indx(2,ip)
        iprs=indx(3,ip)

        if(leqi(eqn(ihor),'UB').or.
     &     leqi(eqn(ihor),'UI').or.
     &     leqi(eqn(ihor),'UW').or.
     &     leqi(eqn(ihor),'U0'))then
          afl(2*nlband+1,ihor)=1.0
        endif

        if(leqi(eqn(iver),'VB').or.
     &     leqi(eqn(iver),'VI').or.
     &     leqi(eqn(iver),'VW').or.
     &     leqi(eqn(iver),'V0'))then
          afl(2*nlband+1,iver)=1.0
        endif

        if(iprs.gt.0)then
          if(leqi(eqn(iprs),'PB'))then
            afl(2*nlband+1,iprs)=1.0
          elseif(leqi(eqn(iprs),'P0'))then
            afl(2*nlband+1,iprs)=1.0
          endif
        endif

      enddo

      return
      end
      subroutine fpirb(afl,arb,area,eqn,grb,indx,ldafl,maxcofrb,
     &  maxelm,maxnfl,nbcrb,ncofrb,nelem,neqnfl,nferb,nlband,node,
     &  np,npar,nparb,nparf,nx,ny,par,phifl,rb,xc,xrange,yc,yrange)
c
c***********************************************************************
c
cc FPIRB computes the jacobian of the reduced basis solution GRB
c  using the indirect method.
c
c
c  FPIRB was modified on 05 August 1996.
c
      implicit double precision (a-h,o-z)
c
      integer ldafl
      integer maxcofrb
      integer maxelm
      integer maxnfl
      integer ncofrb
      integer nelem
      integer neqnfl
      integer np
      integer npar
      integer nparf
c
c
c  For some reason, I couldn't set AFL as a local array.
c
      double precision afl(ldafl,maxnfl)
      double precision arb(maxcofrb,maxcofrb)
      double precision area(3,nelem)
      character*2 eqn(neqnfl)
c
c  FORTRAN 90 temporary array.
c
      double precision gfltmp(neqnfl)
      double precision grb(ncofrb)
      integer i
      integer ieqn
      integer indx(3,np)
      integer j
      integer jhi
      integer jlo
      integer k
      integer l
      logical leqi
      integer nbcrb
      integer nferb
      integer nlband
      integer node(6,nelem)
      integer nparb
      integer nx
      integer ny
      double precision par(npar)
      double precision phifl(3,6,10,nelem)
      double precision rb(maxnfl,ncofrb)
      double precision xc(np)
      double precision xrange
      double precision yc(np)
      double precision yrange
c
c  Zero out ARB.
c
      do i=1,ncofrb
        do j=1,ncofrb
          arb(i,j)=0.0
        enddo
      enddo
c
c  Get the partial derivative of the boundary conditions.
c
      call fpbcrb(arb,indx,maxcofrb,maxnfl,nbcrb,ncofrb,
     &  nelem,node,np,nx,ny,rb,xc,xrange,yc,yrange)
c
c  Recover the equivalent full basis coefficients GFLTMP from 
c  the reduced basis coefficients GRB.
c
      call grb2fl(gfltmp,grb,maxnfl,ncofrb,neqnfl,rb)
c
c  Get the jacobian FPFL for the full coefficient set.
c
      call fpfl(afl,area,eqn,gfltmp,indx,ldafl,maxelm,nelem,neqnfl,
     &  nlband,node,np,npar,nparb,nparf,par,phifl)
c
c  Zero out all rows except for U and V momentum equations.
c
      do i=1,np
        ieqn=indx(1,i)
        if(.not.leqi(eqn(ieqn),'U'))then
          jlo=max(1,ieqn-nlband)
          jhi=min(neqnfl,ieqn+nlband)
          do j=jlo,jhi
            afl(ieqn-j+2*nlband+1,j)=0.0
          enddo
        endif
        ieqn=indx(2,i)
        if(.not.leqi(eqn(ieqn),'V'))then
          jlo=max(1,ieqn-nlband)
          jhi=min(neqnfl,ieqn+nlband)
          do j=jlo,jhi
            afl(ieqn-j+2*nlband+1,j)=0.0
          enddo
        endif
        ieqn=indx(3,i)
        if(ieqn.gt.0)then
          jlo=max(1,ieqn-nlband)
          jhi=min(neqnfl,ieqn+nlband)
          do j=jlo,jhi
            afl(ieqn-j+2*nlband+1,j)=0.0
          enddo
        endif
      enddo
c
c  Compute the FE portion of the jacobian,
c    FPRB = QT * FPFL * Q.
c
      do i=nbcrb+1,nbcrb+nferb
        do j=1,neqnfl
          do k=1,neqnfl
           do l=1,ncofrb
             if(-nlband.le.j-k.and.j-k.le.nlband)then
               arb(i,l)=arb(i,l)+rb(j,i)*afl(j-k+2*nlband+1,k)
     &           *rb(k,l)
             endif
           enddo
         enddo
        enddo
      enddo

      return
      end
      subroutine fprb(arb,area,grb,indx,maxcofrb,maxelm,maxnfl,nbcrb,
     &  ncofrb,nelem,nferb,node,np,nx,ny,phirb,rb,reynld,xc,xrange,yc,
     &  yrange)
c
c***********************************************************************
c
cc FPRB evaluates the reduced basis jacobian directly (that is,
c  without computing the full basis jacobian first).
c
c  FPRB is given
c
c    PAR, the current parameter values,
c    GRB, the reduced basis coefficients of an approximate solution,
c    PHIRB, the reduced basis functions, evaluated at the quadrature 
c      points,
c
c  and computes 
c
c    ARB, the reduced basis jacobian of the Navier Stokes  
c      equations.
c
c
c  FPRB was modified on 11 July 1996.
c
c
c  ARB    Output, double precision ARB(MAXNRB,MAXNRB).
c         ARB contains the Jacobian matrix for the reduced basis system,
c         stored as an NCOFRB by NCOFRB array.
c
c  AREA   Input, double precision AREA(3,NELEM).
c
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  GRB    Input, double precision GRB(NCOFRB).
c         GRB contains the reduced basis coefficients of the current 
c         estimate of the state solution.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NCOFRB Input, integer NCOFRB.
c         NCOFRB is the number of basis functions, reduced state equations and 
c         coefficients in the reduced basis system.
c
c  PHIRB  Input, double precision PHIRB(3,NCOFRB,15,NELEM).
c         PHIRB contains the values of a finite element basis function
c         or its X or Y derivative, in a given element, at a given
c         quadrature point, for a particular reduced basis function.
c
c         For PHIRB(I,J,K,L), index J refers to the reduced basis
c         basis functions, for J=0 to NCOFRB.
c
c         The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c           For the quadrature point I, and reduced basis function J,
c           in element L, PHIRB(I,J,K,L) represents the value of:
c
c             K=1, WUrb, the finite element U velocity basis function;
c             K=2, dWUrbdX, the X derivative of WUrb;
c             K=3, dWUrbdY, the Y derivative of WUrb;
c             K=4, WVrb, the finite element V velocity basis function;
c             K=5, dWVrbdX, the X derivative of WVrb;
c             K=6, dWVrbdY, the Y derivative of WVrb;
c             K=7, Q, the finite element pressure basis function.
c             K=8, dQrbdX, the X derivative of Qrb;
c             K=9, dQrbdY, the Y derivative of Qrb.
c             K=10, WU0rb, same as WUrb, with zero BC.
c             K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c             K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c             K=13, WV0rb, same as WVrb, with zero BC.
c             K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c             K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  REYNLD Input, double precision REYNLD.
c         The current value of the Reynolds number parameter.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxelm
      integer maxnfl
      integer nelem
      integer ncofrb
      integer np
c
      double precision arb(maxcofrb,maxcofrb)
      double precision area(3,nelem)
      double precision grb(ncofrb)
      integer indx(3,np)
      integer nbcrb
      integer nferb
      integer node(6,nelem)
      integer nx
      integer ny
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision rb(maxnfl,maxcofrb)
      double precision reynld
      double precision xc(np)
      double precision xrange
      double precision yc(np)
      double precision yrange
c
      call fpbcrb(arb,indx,maxcofrb,maxnfl,nbcrb,ncofrb,
     &  nelem,node,np,nx,ny,rb,xc,xrange,yc,yrange)
 
      call fpferb(arb,area,grb,maxcofrb,maxelm,nbcrb,ncofrb,
     &  nelem,nferb,phirb,reynld)
 
      return
      end
      subroutine fxbcrb(grb,indx,maxcofrb,maxnfl,nbcrb,ncofrb,nelem,
     &  node,np,npar,nparf,nx,ny,par,rb,resrb,tauflo,xc,xrange,yc,
     &  yrange)
c
c***********************************************************************
c
cc FXBCRB evaluates the boundary conditions that form part of the
c  reduced function.
c
c
c  FXBCRB was modified on 12 August 1996.
c
c
c  GRB    
c    double precision GRB(NCOFRB).
c    GRB contains the reduced basis coefficients of the current
c    estimate of the state solution.
c
c  INDX   
c    integer INDX(3,NP).
c    INDX(I,J) contains, for each node J, the global index of U,
c    V and P at that node, or 0 or a negative value.  The global
c    index of U, V, or P is the index of the coefficient vector
c    that contains the value of the finite element coefficient
c    associated with the corresponding basis function at the
c    given node.
c
c  MAXCOFRB
c    integer MAXCOFRB.
c    MAXCOFRB is the maximum legal value for NCOFRB, the number
c    of coefficients used to specify a particular reduced basis
c    solution.
c
c  MAXNFL 
c    integer MAXNFL.
c    MAXNFL is the maximum number of equations or coefficients allowed
c    for the full system.  MAXNFL must be used instead of NEQNFL as
c    the leading dimension of certain multi-dimensional arrays.
c
c  NBCRB  
c    integer NBCRB.
c    NBCRB is the number of independent boundary condition
c    vectors used for the reduced basis.  NBCRB is normally
c    at least 1, and must be no more than MAXBCRB.
c
c  NCOFRB 
c    integer NCOFRB.
c    NCOFRB is the number of coefficients needed to determine
c    a particular reduced basis function.
c    NCOFRB is the sum of NBCRB and NFERB.
c
c  NELEM  
c    integer NELEM.
c    NELEM is the number of elements.
c    NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NODE   
c    integer NODE(6,MAXELM) or NODE(6,NELEM).
c    NODE(I,J) contains, for an element J, the global index of
c    the node whose local number in J is I.
c
c  NP     
c    integer NP.
c    NP is the number of nodes used to define the finite element mesh.
c    Typically, the mesh is generated as a rectangular array, with
c    an odd number of nodes in the horizontal and vertical directions.
c    The formula for NP is NP=(2*NX-1)*(2*NY-1).
c
c  NPAR   
c    integer NPAR.
c    NPAR is the number of parameters.
c      NPAR = NPARF + NPARB + 1.
c    The parameters control the shape and strength of the inflow, 
c    the shape of the bump, and the value of the Reynolds number.
c
c  NPARF  
c    integer NPARF.
c    NPARF is the number of parameters associated with the
c    inflow.  NPARF must be at least 1.
c
c  NX     
c    integer NX.
c    NX controls the spacing of nodes and elements in
c    the X direction.  There are 2*NX-1 nodes along various
c    lines in the X direction.
c
c    The number of elements along a line in the X direction is
c    NX-1 (or 2*(NX-1) to make a full rectangular strip).
c
c  NY     
c    integer NY.
c    NY controls the spacing of nodes and elements in
c    the Y direction.  There are 2*NY-1 nodes along various
c    lines in the Y direction.
c
c    The number of elements along a line in the Y direction is
c    NY-1 (or 2*(NY-1) to make a full vertical strip).
c
c  PAR    
c    double precision PAR(NPAR).
c    PAR contains the values of the problem parameters.
c
c      PAR(1:NPARF)             = inflow controls.
c      PAR(NPARF+1:NPARF+NPARB) = bump controls.
c      PAR(NPARF+NPARB+1)       = the REYNLD parameter.
c
c  RB     
c    double precision RB(MAXNFL,MAXCOFRB).
c
c    RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
c    RB is generated by computing a finite element solution GFL,
c    which is saved for later reference as "GFLRB".
c    GFLRB is copied into the first column of RB.
c    Then, we compute the first NCOFRB derivatives of GFLRB with
c    respect to a parameter.  The first derivative
c    is stored in column 1 of RB, and so on.  
c
c  RESRB  
c    double precision RESRB(NCOFRB).
c    RESRB contains the residual in the reduced basis equations,
c    for the parameter values PAR and reduced basis coefficients GRB.
c
c  TAUFLO 
c    double precision TAUFLO(NPARF+2).
c    TAUFLO contains the location of the spline abscissas for
c    the inflow.  There are NPARF+2 of them, because the end
c    values of the spline are constrained to have particular
c    values.
c
c  XC     
c    double precision XC(NP).
c    XC contains the X coordinates of the nodes.
c
c  XRANGE 
c    double precision XRANGE.
c    XRANGE is the total width of the region.
c
c  YC     
c    double precision YC(NP).
c    YC contains the Y coordinates of the nodes.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxnfl
      integer ncofrb
      integer nelem
      integer np
      integer npar
      integer nparf
c
      double precision dwdx
      double precision dwdy
      double precision grb(ncofrb)
      integer ibcrb
      integer icoffl
      integer icofrb
      integer icol
      integer ielem
      integer indx(3,np)
      integer inode
      integer iq
      integer nbcrb
      integer node(6,nelem)
      integer nx
      integer ny
      double precision par(npar)
      double precision rb(maxnfl,maxcofrb)
      double precision resrb(ncofrb)
      double precision tauflo(nparf)
      double precision ubcrb
      double precision w
      double precision wurb
      double precision xbc
      double precision xc(np)
      double precision xrange
      double precision ybc
      double precision yc(np)
      double precision yrange
c
      do ibcrb=1,nbcrb
 
c
c  These (X,Y) values are only valid for the driven cavity.
c  You should pass REGION in here to sort it out.
c
        xbc=tauflo(ibcrb)
        ybc=yrange

        icol=1+xbc*dble(nx-1)/xrange
        if(icol.gt.nx-1)icol=nx-1

        ielem=icol*(2*ny-2)-1
c
c  Evaluate the reduced solution UBCRB at (XBC,YBC).
c
        ubcrb=0.0
        do icofrb=1,ncofrb
          wurb=0.0
          do iq=1,6
            call qbf(ielem,iq,w,dwdx,dwdy,nelem,node,np,xc,xbc,yc,ybc)
            inode=node(iq,ielem)
            icoffl=indx(1,inode)
            wurb=wurb+rb(icoffl,icofrb)*w
          enddo
          ubcrb=ubcrb+grb(icofrb)*wurb
        enddo
c
c  Set the function value.
c
        resrb(ibcrb)=ubcrb-par(ibcrb)
 
      enddo
 
      return
      end
      subroutine fxferb(area,grb,maxcofrb,maxelm,nbcrb,ncofrb,nelem,
     &  nferb,phirb,resrb,reynld)
c
c***********************************************************************
c
cc FXFERB evaluates the finite element portion of the reduced function.
c
c  FXFERB is given
c    GRB, the reduced basis coefficients of an approximate solution,
c    PHIRB, the reduced basis functions, evaluated at the quadrature 
c      points,
c  and computes 
c    RESRB, the reduced basis residual of the Navier Stokes  
c      equations.
c
c  The reduced discretized Navier Stokes equations have the form:
c
c    Integral
c
c      dUrb/dx * dWu0/dx + dUrb/dy * dWu0dy
c    + reynld * (Urb*dUrb/dx + Vrb*dUrb/dy + dPrb/dx) * Wu0 dx dy = 0
c
c    Integral
c
c      dVrb/dx * dWv0/dx + dVrb/dy * dWv0/dy
c    + reynld * (Urb*dVrb/dx + Vrb*dVrb/dy + dPrb/dy) * Wv0 dx dy = 0
c
c  Here, WU0 and WV0 are the reduced basis functions for U and V
c  assuming homogeneous boundary conditions.
c
c
c  FXFERB was modified on 31 July 1996.
c
c
c  AREA   Input, double precision AREA(3,NELEM).
c
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  GRB    Input, double precision GRB(NCOFRB).
c         GRB contains the reduced basis coefficients of the current 
c         estimate of the state solution.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NCOFRB Input, integer NCOFRB.
c         NCOFRB is the number of basis functions, reduced state equations and 
c         coefficients in the reduced basis system.
c
c  PHIRB  double precision PHIRB(3,NCOFRB,15,NELEM).
c         PHIRB contains the values of a finite element basis function
c         or its X or Y derivative, in a given element, at a given
c         quadrature point, for a particular reduced basis function.
c
c         For PHIRB(I,J,K,L), index J refers to the reduced basis
c         basis functions, for J=0 to NCOFRB.
c
c         The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c           For the quadrature point I, and reduced basis function J,
c           in element L, PHIRB(I,J,K,L) represents the value of:
c
c             K=1, WUrb, the finite element U velocity basis function;
c             K=2, dWUrbdX, the X derivative of WUrb;
c             K=3, dWUrbdY, the Y derivative of WUrb;
c             K=4, WVrb, the finite element V velocity basis function;
c             K=5, dWVrbdX, the X derivative of WVrb;
c             K=6, dWVrbdY, the Y derivative of WVrb;
c             K=7, Q, the finite element pressure basis function.
c             K=8, dQrbdX, the X derivative of Qrb;
c             K=9, dQrbdY, the Y derivative of Qrb.
c             K=10, WU0rb, same as WUrb, with zero BC.
c             K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c             K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c             K=13, WV0rb, same as WVrb, with zero BC.
c             K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c             K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  RESRB  Output, double precision RESRB(NCOFRB).
c         RESRB contains the residual in the reduced basis equations, 
c         for the given parameter values and reduced basis coefficients GRB.
c
c  REYNLD Input, double precision REYNLD.
c         The current value of the Reynolds number parameter.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxelm
      integer nelem
      integer ncofrb
c
      double precision ar
      double precision area(3,nelem)
      double precision dpdx
      double precision dpdy
      double precision dudx
      double precision dudy
      double precision dvdx
      double precision dvdy
      double precision dwu0dx
      double precision dwu0dy
      double precision dwv0dx
      double precision dwv0dy
      double precision grb(ncofrb)
      integer icofrb
      integer ielem
      integer iquad
      logical leqi
      integer nbcrb
      integer nferb
      double precision p
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision resrb(ncofrb)
      double precision reynld
      double precision u
      double precision v
      double precision wu0
      double precision wv0
c
      external leqi
c
      do icofrb=nbcrb+1,nbcrb+nferb
        resrb(icofrb)=0.0
      enddo
c
c  Consider an element IELEM...
c
      do ielem=1,nelem
c
c  ...and a quadrature point IQUAD...
c
        do iquad=1,3
 
          ar=area(iquad,ielem)
c
c  For the given reduced coefficients GRB, and basis functions
c  PHIRB, evaluate U, V, and P, and their spatial derivatives.
c
          call uvpqrb(dpdx,dpdy,dudx,dudy,dvdx,dvdy,grb,
     &      ielem,iquad,maxcofrb,maxelm,ncofrb,phirb,p,u,v)
c
c  Now consider the residual associated with each finite element
c  reduced basis function.
c
          do icofrb=nbcrb+1,nbcrb+nferb
 
            wu0=phirb(iquad,icofrb,10,ielem)
            dwu0dx=phirb(iquad,icofrb,11,ielem)
            dwu0dy=phirb(iquad,icofrb,12,ielem)
            wv0=phirb(iquad,icofrb,13,ielem)
            dwv0dx=phirb(iquad,icofrb,14,ielem)
            dwv0dy=phirb(iquad,icofrb,15,ielem)
 
            resrb(icofrb)=resrb(icofrb)+ar*
     &        (dudx*dwu0dx+dudy*dwu0dy
     &        +reynld*(u*dudx+v*dudy+dpdx)*wu0
     &        +dvdx*dwv0dx+dvdy*dwv0dy
     &        +reynld*(u*dvdx+v*dvdy+dpdy)*wv0)
 
          enddo
        enddo
      enddo
 
      return
      end
      subroutine fxfl(area,eqn,gfl,ifs,indx,nelem,neqnfl,node,np,npar,
     &  nparb,nparf,par,phifl,region,resfl,splflo,tauflo,
     &  xrange,yrange)
c
c***********************************************************************
c
cc FXFL computes the residual of the Navier Stokes equations,
c  evaluated at the solution GFL.
c
c
c  The discretized Navier Stokes equations have the form:
c
c    Integral
c
c      dU/dx * dW/dx + dU/dy * dW/dy
c    + reynld * (U*dU/dx + V*dU/dy + dP/dx) * W dx dy = 0
c
c    Integral
c
c      dV/dx * dW/dx + dV/dy * dW/dy
c    + reynld * (U*dV/dx + V*dV/dy + dP/dy) * W dx dy = 0
c
c    Integral
c
c      (dU/dx + dV/dy) * Q dx dy = 0
c
c  Here W is a basis function for U and V, and Q is a basis
c  function for P.
c
c
c  FXFL was modified on 08 August 1996.
c
c
c  AREA   Input, double precision AREA(3,NELEM).
c
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  EQN    Input, character*2 EQN(NEQNFL).
c         EQN records the "type" of each equation that will be generated, and
c         which is associated with an unknown.  Note that most boundary 
c         conditions do not result in an equation.  The current values are:
c
c         'U'  The horizontal momentum equation.
c         'UB' The condition U=0 applied at a node on the bump.
c         'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c         'UW' The condition U=0 applied at a node on a fixed wall.
c
c         'V'  The vertical momentum equation.
c         'VB' The condition V=0 applied at a node on the bump.
c         'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c         'VW' The condition V=0 applied at a node on a fixed wall.
c
c         'P'  The continuity equation.
c         'PB' The condition P=0 applied at (XMAX,YMAX).
c
c  GFL    Input, double precision GFL(NEQNFL).
c
c         GFL is the current solution vector, in which are stored 
c         the finite element coefficients that define the velocity
c         and pressure functions, U, V and P.
c
c  IFS    Input, integer IFS.
c         1, the inflow is modeled by C0 linear splines.
c         2, the inflow is modeled by C0 quadratic splines.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NEQNFL Input, integer NEQNFL, the number of finite element equations used
c         to define the horizontal and vertical velocities and the
c         pressure.
c 
c  NODE   Input, integer NODE(6,NELEM).
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
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c 
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c 
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c 
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the 
c         inflow.  NPARF must be at least 1.  
c
c  PAR    Input, double precision PAR(NPAR).
c
c         PAR is the current set of parameter values, including the
c         Reynolds parameter, the flow parameters, and the bump parameters.
c
c  PHIFL  Input, double precision PHIFL(3,6,10,NELEM).  
c
c         PHIFL contains the value of a finite element basis function, its
c         derivative, or other information, evaluated at the quadrature 
c         points.
c 
c         The meaning of the entry PHIFL(I,J,K,L) is as follows.  
c         For the quadrature point I, and basis function J, in element L,
c         PHIFL(I,J,K,L) represents the value of:
c
c           K= 1, W, the finite element basis function for velocities;
c           K= 2, dWdX, the X derivative of W;
c           K= 3, dWdY, the Y derivative of W;
c           K= 4, Q, the finite element basis function for pressures;
c           K= 5, dQdX, the X derivative of Q;
c           K= 6, dQdY, the Y derivative of Q;
c           K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c           K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c           K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c           K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c         In particular, PHIFL(I,J,K,L) is the value of the quadratic 
c         basis function W associated with local node J in element L, 
c         evaluated at quadrature point I.
c 
c         Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6, 
c         since there are only three linear basis functions.
c
c  REGION Input, character*20 REGION.
c         REGION specifies the flow region.
c
c         'cavity', a driven cavity, 1 unit on each side, open on 
c         the top with a tangential velocity specification there.
c
c         'channel', a channel, 10 units long by 3 high, inflow on 
c         the left, outflow on the right, with a bump on the bottom.
c
c         'step', a channel, 12 units long by 3 high, inflow on the
c         left, outflow on the right, with a step on the bottom.
c
c  RESFL  Output, double precision RESFL(NEQNFL), contains the value
c         of the residual.
c
c  SPLFLO Input, double precision SPLFLO(NPARF+2).
c
c         SPLFLO contains the spline coefficients for the inflow.
c
c  TAUFLO Input, double precision TAUFLO(NPARF+2).
c
c         TAUFLO contains the location of the spline abscissas for
c         the inflow.  There are NPARF+2 of them, because the end 
c         values of the spline are constrained to have particular 
c         values.
c
c  XC     Input, double precision XC(NP).
c
c         The X coordinates of the nodes.
c 
c  YC     Input, double precision YC(NP).
c
c         The Y coordinates of the nodes.
c 
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer neqnfl
      integer np
      integer npar
      integer nparf
c
      double precision ar
      double precision area(3,nelem)
      double precision dpdx
      double precision dpdy
      double precision dudx
      double precision dudy
      double precision dvdx
      double precision dvdy
      double precision dwidx
      double precision dwidy
      character*2 eqn(neqnfl)
      double precision gfl(neqnfl)
      integer i
      integer ifs
      integer ielem
      integer ihor
      integer indx(3,np)
      integer ip
      integer iprs
      integer iq
      integer iquad
      integer iver
      logical leqi
      integer node(6,nelem)
      integer nparb
      double precision p
      double precision par(npar)
      double precision phifl(3,6,10,nelem)
      double precision qi
      character*20 region
      double precision resfl(neqnfl)
      double precision reynld
      double precision splflo(nparf+2)
      double precision tauflo(nparf+2)
      double precision u
      double precision ubc
      double precision v
      double precision vbc
      double precision wi
      double precision xrange
      double precision xval
      double precision yrange
      double precision yval
c
      external leqi
c
      reynld=par(nparf+nparb+1)

      do i=1,neqnfl
        resfl(i)=0.0
      enddo
c
c  Consider an element.
c
      do ielem=1,nelem
c
c  Evaluate the integrand at the quadrature points.
c
        do iquad=1,3
 
          ar=area(iquad,ielem)
c
c  Evaluate U, V and P at the IQUAD-th quadrature point.
c
          call uvpqfl(dpdx,dpdy,dudx,dudy,dvdx,dvdy,gfl,ielem,indx,
     &      iquad,nelem,neqnfl,node,np,p,phifl,u,v)
c
c  Look at nearby basis functions.
c
          do iq=1,6
 
            ip=node(iq,ielem)
 
            wi=phifl(iquad,iq,1,ielem)
            dwidx=phifl(iquad,iq,2,ielem)
            dwidy=phifl(iquad,iq,3,ielem)
            qi=phifl(iquad,iq,4,ielem)
c
c  The horizontal velocity equations.
c
            ihor=indx(1,ip)

            if(leqi(eqn(ihor),'U'))then

              resfl(ihor)=resfl(ihor)+ar*(dudx*dwidx + dudy*dwidy
     &          +reynld*(u*dudx+v*dudy+dpdx)*wi )
 
            elseif(leqi(eqn(ihor),'UB'))then

              resfl(ihor)=gfl(ihor)

            elseif(leqi(eqn(ihor),'UI'))then

              call flowbc(ifs,npar,nparf,par,region,splflo,tauflo,
     &          ubc,vbc,xrange,xval,yrange,yval)
 
              resfl(ihor)=gfl(ihor)-ubc

            elseif(leqi(eqn(ihor),'UW'))then

              resfl(ihor)=gfl(ihor)

            elseif(leqi(eqn(ihor),'U0'))then

              resfl(ihor)=gfl(ihor)

            endif
c
c  The vertical velocity equations.
c
            iver=indx(2,ip)

            if(leqi(eqn(iver),'V'))then

              resfl(iver)=resfl(iver)+ar*(dvdx*dwidx + dvdy*dwidy
     &          +reynld*(u*dvdx+v*dvdy+dpdy)*wi )

            elseif(leqi(eqn(iver),'VB'))then

              resfl(iver)=gfl(iver)

            elseif(leqi(eqn(iver),'VI'))then

              call flowbc(ifs,npar,nparf,par,region,splflo,tauflo,
     &          ubc,vbc,xrange,xval,yrange,yval)
 
              resfl(iver)=gfl(iver)-vbc

            elseif(leqi(eqn(iver),'VW'))then

              resfl(iver)=gfl(iver)

            elseif(leqi(eqn(iver),'V0'))then

              resfl(iver)=gfl(iver)

            endif
c
c  The pressure equations.
c
            iprs=indx(3,ip)
            if(iprs.gt.0)then
              if(leqi(eqn(iprs),'P'))then
                resfl(iprs)=resfl(iprs)+ar*(dudx+dvdy)*qi
              elseif(leqi(eqn(iprs),'PB'))then
                resfl(iprs)=gfl(iprs)
              elseif(leqi(eqn(iprs),'P0'))then
                resfl(iprs)=gfl(iprs)
              endif
            endif
 
          enddo
        enddo
      enddo
 
      return
      end
      subroutine fxfl2rb(grb,indx,maxcofrb,maxnfl,nbcrb,ncofrb,nelem,
     &  neqnfl,nferb,node,np,npar,nparf,nx,ny,par,rb,resfl,resrb,
     &  tauflo,xc,xrange,yc,yrange)
c
c***********************************************************************
c
cc FXFL2RB projects a function vector RESFL for the full system into 
c  RESRB, the corresponding function vector for the reduced system.
c
c  The relationship used is
c
c    RESRB = Q^T * RESFL.
c
c
c  FXFL2RB was modified on 12 August 1996.
c
c
c  RESFL  Input, double precision RESFL(NEQNFL), the function value
c         in the full system.
c
c  RESRB  Output, double precision RESRB(NCOFRB), the function value
c         in the reduced system.
c
c  MAXNFL Input, integer MAXNFL, the maximum value of NEQNFL, used as
c         the leading dimension of RB.
c
c  NCOFRB Input, integer NCOFRB, the number of coefficients for the
c         reduced system.
c
c  NEQNFL Input, integer NEQNFL, the number of coefficients for the
c         full system.
c
c  RB     Input, double precision rb(maxnfl,ncofrb).
c         RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxnfl
      integer ncofrb
      integer nelem
      integer neqnfl
      integer np
      integer npar
      integer nparf
c
      double precision grb(ncofrb)
      integer i
      integer indx(3,np)
      integer j
      integer nbcrb
      integer nferb
      integer node(6,nelem)
      integer nx
      integer ny
      double precision par(npar)
      double precision rb(maxnfl,ncofrb)
      double precision resfl(neqnfl)
      double precision resrb(ncofrb)
      double precision tauflo(nparf)
      double precision xc(np)
      double precision xrange
      double precision yc(np)
      double precision yrange
c
c  Compute the boundary conditions directly from GFLBC.
c
      call fxbcrb(grb,indx,maxcofrb,maxnfl,nbcrb,ncofrb,nelem,
     &  node,np,npar,nparf,nx,ny,par,rb,resrb,tauflo,xc,xrange,yc,
     &  yrange)
c
c  Multiply the second half of the vector RESFL by the second half of 
c  RB transpose.
c
      do i=nbcrb+1,nbcrb+nferb
        resrb(i)=0.0
        do j=1,neqnfl
          resrb(i)=resrb(i)+rb(j,i)*resfl(j)
        enddo
      enddo

      return
      end
      subroutine fxirb(area,eqn,grb,ifs,indx,maxcofrb,maxnfl,nbcrb,
     &  ncofrb,nelem,neqnfl,nferb,node,np,npar,nparb,nparf,
     &  nx,ny,par,phifl,rb,region,resrb,splflo,tauflo,xc,xrange,
     &  yc,yrange)
c
c***********************************************************************
c
cc FXIRB computes the residual of the reduced basis solution GRB
c  using the indirect method.
c
c
c  FXIRB was modified on 04 August 1996.
c
c
c  AREA   Input, double precision AREA(3,NELEM).
c
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  EQN    Input, character*2 EQN(NEQNFL).
c         EQN records the "type" of each equation that will be generated, and
c         which is associated with an unknown.  Note that most boundary 
c         conditions do not result in an equation.  The current values are:
c
c         'U'  The horizontal momentum equation.
c         'UB' The condition U=0 applied at a node on the bump.
c         'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c         'UW' The condition U=0 applied at a node on a fixed wall.
c
c         'V'  The vertical momentum equation.
c         'VB' The condition V=0 applied at a node on the bump.
c         'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c         'VW' The condition V=0 applied at a node on a fixed wall.
c
c         'P'  The continuity equation.
c         'PB' The condition P=0 applied at (XMAX,YMAX).
c
c  GFL    Input/output, double precision GFL(NEQNFL).
c
c         GFL must contain on input the coefficients 
c         for the full basis system that are equivalent to GRB.
c
c  GFLRB  Input, double precision GFLRB(NEQNFL), the full basis coefficients
c         of the solution at which the reduced basis was generated.
c
c  GRB    Input, double precision GRB(NCOFRB).
c         The coefficients for the reduced basis system.
c
c  IFS    Input, integer IFS.
c         1, the inflow is modeled by C0 linear splines.
c         2, the inflow is modeled by C0 quadratic splines.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  MAXNFL Input, integer MAXNFL, the maximum number of equations in the
c         full system.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NEQNFL Input, integer NEQNFL, the number of equations in the full system.
c
c  NCOFRB Input, integer NCOFRB, the number of equations in the reduced 
c         system.
c
c  NODE   Input, integer NODE(6,MAXELM).
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
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c   
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c 
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c 
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the 
c         inflow.  NPARF must be at least 1.  
c
c  PAR    Input, double precision PAR(NPAR).
c
c         PAR is the current estimate for the parameters.
c
c  PHIFL  Input, double precision PHIFL(3,6,10,NELEM).  
c
c         PHIFL contains the value of a finite element basis function, its
c         derivative, or other information, evaluated at the quadrature 
c         points.
c 
c         The meaning of the entry PHIFL(I,J,K,L) is as follows.  
c         For the quadrature point I, and basis function J, in element L,
c         PHIFL(I,J,K,L) represents the value of:
c
c           K= 1, W, the finite element basis function for velocities;
c           K= 2, dWdX, the X derivative of W;
c           K= 3, dWdY, the Y derivative of W;
c           K= 4, Q, the finite element basis function for pressures;
c           K= 5, dQdX, the X derivative of Q;
c           K= 6, dQdY, the Y derivative of Q;
c           K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c           K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c           K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c           K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c         In particular, PHIFL(I,J,K,L) is the value of the quadratic 
c         basis function W associated with local node J in element L, 
c         evaluated at quadrature point I.
c 
c         Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6, 
c         since there are only three linear basis functions.
c
c  RB     Input, double precision RB(MAXNFL,NCOFRB), the columns of RB
c         contain the orthonormal reduced basis vectors.
c
c  REGION Input, character*20 REGION.
c         REGION specifies the flow region.
c
c         'cavity', a driven cavity, 1 unit on each side, open on 
c         the top with a tangential velocity specification there.
c
c         'channel', a channel, 10 units long by 3 high, inflow on 
c         the left, outflow on the right, with a bump on the bottom.
c
c         'step', a channel, 12 units long by 3 high, inflow on the
c         left, outflow on the right, with a step on the bottom.
c
c  RESFL  Output, double precision RESFL(NEQNFL), the residual in the
c         full basis equations.
c
c  RESRB  Output, double precision RESRB(NCOFRB), the residual in the
c         reduced basis equations, evaluated at the coefficient
c         vector GRB.
c
c  SPLFLO Input, double precision SPLFLO(NPARF+2).
c
c         SPLFLO contains the spline coefficients for the inflow.
c
c  TAUFLO Input, double precision TAUFLO(NPARF+2).
c
c         TAUFLO contains the location of the spline abscissas for
c         the inflow.  There are NPARF+2 of them, because the end 
c         values of the spline are constrained to have particular 
c         values.
c
c  XC     Input, double precision XC(NP).
c
c         The X coordinates of the nodes.
c
c  YC     Input, double precision YC(NP).
c
c         The Y coordinates of the nodes.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxnfl
      integer ncofrb
      integer nelem
      integer neqnfl
      integer np
      integer npar
      integer nparf
c
      double precision area(3,nelem)
      character*2 eqn(neqnfl)
c
c  GFLTMP is a FORTRAN 90 temporary array.
c
      double precision gfltmp(neqnfl)
      double precision grb(ncofrb)
      integer i
      integer ieqn
      integer ifs
      integer indx(3,np)
      logical leqi
      integer nbcrb
      integer nferb
      integer node(6,nelem)
      integer nparb
      integer nx
      integer ny
      double precision par(npar)
      double precision phifl(3,6,10,nelem)
      double precision rb(maxnfl,ncofrb)
      character*20 region
c
c  RESFLTMP is a FORTRAN 90 temporary array.
c
      double precision resfltmp(neqnfl)
      double precision resrb(ncofrb)
      double precision splflo(nparf+2)
      double precision tauflo(nparf+2)
      double precision xc(np)
      double precision xrange
      double precision yc(np)
      double precision yrange
c
c  Recover the equivalent full basis coefficients GFLTMP from 
c  the reduced basis coefficients GRB.
c
      call grb2fl(gfltmp,grb,maxnfl,ncofrb,neqnfl,rb)
c
c  Evaluate the residual RESFLTMP at GFLTMP.
c
      call fxfl(area,eqn,gfltmp,ifs,indx,nelem,neqnfl,node,np,npar,
     &  nparb,nparf,par,phifl,region,resfltmp,splflo,tauflo,
     &  xrange,yrange)
c
c  Zero out all residuals except for U and V momentum equations.
c
      do i=1,np
        ieqn=indx(1,i)
        if(.not.leqi(eqn(ieqn),'U'))then
          resfltmp(ieqn)=0.0
        endif
        ieqn=indx(2,i)
        if(.not.leqi(eqn(ieqn),'V'))then
          resfltmp(ieqn)=0.0
        endif
        ieqn=indx(3,i)
        if(ieqn.gt.0)then
          resfltmp(ieqn)=0.0
        endif
      enddo
c
c  Project the residual RESFLTMP back into the reduced space,
c  arriving at RESRB.
c
      call fxfl2rb(grb,indx,maxcofrb,maxnfl,nbcrb,ncofrb,nelem,
     &  neqnfl,nferb,node,np,npar,nparf,nx,ny,par,rb,resfltmp,resrb,
     &  tauflo,xc,xrange,yc,yrange)

      return
      end
      subroutine fxrb(area,grb,indx,maxcofrb,maxelm,maxnfl,nbcrb,ncofrb,
     &  nelem,nferb,node,np,npar,nparf,nx,ny,par,phirb,rb,resrb,reynld,
     &  tauflo,xc,xrange,yc,yrange)
c
c***********************************************************************
c
cc FXRB evaluates the boundary conditions and finite element equations
c  that form the reduced function.
c
c
c  FXRB was written on 31 July 1996.
c
c
c  AREA   Input, double precision AREA(3,NELEM).
c
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  GFLBC  double precision GFLBC(NEQNFL).
c         GFLBC contains the current full solution, or, in fact,
c         ANY full solution, which satisfies the boundary conditions.
c
c  GRB    double precision GRB(NCOFRB).
c         GRB contains the reduced basis coefficients of the current
c         estimate of the state solution.
c
c  INDX   integer INDX(3,NP).
c         INDX(I,J) contains, for each node J, the global index of U,
c         V and P at that node, or 0 or a negative value.  The global
c         index of U, V, or P is the index of the coefficient vector
c         that contains the value of the finite element coefficient
c         associated with the corresponding basis function at the
c         given node.
c
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  MAXCOFRB
c         Integer MAXCOFRB.
c         MAXCOFRB is the maximum legal value for NCOFRB, the number
c         of coefficients used to specify a particular reduced basis
c         solution.
c
c  MAXNFL integer MAXNFL.
c         MAXNFL is the maximum number of equations or coefficients allowed
c         for the full system.  MAXNFL must be used instead of NEQNFL as
c         the leading dimension of certain multi-dimensional arrays.
c
c  NBCRB  integer NBCRB.
c         NBCRB is the number of independent boundary condition
c         vectors used for the reduced basis.  NBCRB is normally
c         at least 1, and must be no more than MAXBCRB.
c
c  NCOFRB integer NCOFRB.
c         NCOFRB is the number of coefficients needed to determine
c         a particular reduced basis function.
c         NCOFRB is the sum of NBCRB and NFERB.
c
c  NELEM  integer NELEM.
c         NELEM is the number of elements.
c         NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NEQNFL integer NEQNFL.
c         NEQNFL is the number of equations (and coefficients) in the full
c         finite element system.
c
c  NODE   integer NODE(6,MAXELM) or NODE(6,NELEM).
c         NODE(I,J) contains, for an element J, the global index of
c         the node whose local number in J is I.
c
c         The local ordering of the nodes is suggested by this diagram:
c
c           Global nodes   Elements      NODE
c                                                          1  2  3  4  5  6
c           74  84  94     3-6-1   2     Left element =  (94,72,74,83,73,84)
c                          |  /   /|
c           73  83  93     5 4   4 5     Right element = (72,94,92,83,93,82)
c                          |/   /  |
c           72  82  92     2   1-6-3
c
c  NP     integer NP.
c         NP is the number of nodes used to define the finite element mesh.
c         Typically, the mesh is generated as a rectangular array, with
c         an odd number of nodes in the horizontal and vertical directions.
c         The formula for NP is NP=(2*NX-1)*(2*NY-1).
c
c  NX     integer NX.
c         NX controls the spacing of nodes and elements in
c         the X direction.  There are 2*NX-1 nodes along various
c         lines in the X direction.
c
c         The number of elements along a line in the X direction is
c         NX-1 (or 2*(NX-1) to make a full rectangular strip).
c
c  NY     integer NY.
c         NY controls the spacing of nodes and elements in
c         the Y direction.  There are 2*NY-1 nodes along various
c         lines in the Y direction.
c
c         The number of elements along a line in the Y direction is
c         NY-1 (or 2*(NY-1) to make a full vertical strip).
c
c  PHIRB  double precision PHIRB(3,NCOFRB,15,NELEM).
c         PHIRB contains the values of a finite element basis function
c         or its X or Y derivative, in a given element, at a given
c         quadrature point, for a particular reduced basis function.
c
c         For PHIRB(I,J,K,L), index J refers to the reduced basis
c         basis functions, for J=0 to NCOFRB.
c
c         The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c           For the quadrature point I, and reduced basis function J,
c           in element L, PHIRB(I,J,K,L) represents the value of:
c
c             K=1, WUrb, the finite element U velocity basis function;
c             K=2, dWUrbdX, the X derivative of WUrb;
c             K=3, dWUrbdY, the Y derivative of WUrb;
c             K=4, WVrb, the finite element V velocity basis function;
c             K=5, dWVrbdX, the X derivative of WVrb;
c             K=6, dWVrbdY, the Y derivative of WVrb;
c             K=7, Q, the finite element pressure basis function.
c             K=8, dQrbdX, the X derivative of Qrb;
c             K=9, dQrbdY, the Y derivative of Qrb.
c             K=10, WU0rb, same as WUrb, with zero BC.
c             K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c             K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c             K=13, WV0rb, same as WVrb, with zero BC.
c             K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c             K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  RB     double precision RB(MAXNFL,MAXCOFRB).
c
c         RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
c         RB is generated by computing a finite element solution GFL,
c         which is saved for later reference as "GFLRB".
c         GFLRB is copied into the first column of RB.
c         Then, we compute the first NCOFRB derivatives of GFLRB with
c         respect to a parameter.  The first derivative
c         is stored in column 1 of RB, and so on.  
c
c         Now we compute the QR factorization of this matrix.
c
c         We intend that NEQNFL >> NCOFRB, and RB is a matrix with orthogonal
c         columns, so that:
c
c           Transpose(RB) * RB = Identity(1+NCOFRB)
c
c
c         If GFL is any set of finite element coefficients, the corresponding
c         set of reduced basis coefficients can be computed as:
c
c           GRB = Transpose(RB) * GFL
c
c         If GRB is a set of reduced basis coefficients, a corresponding
c         set of finite element coefficients can be computed as:
c
c           GFL = RB * GRB.
c
c         While it is the case that you can expand and then reduce,
c         and always get the same result, it is not the case that
c         when you reduce and then expand you get the same result!
c
c         It is true, for ANY GRB, that
c
c           GRB = Transpose(RB) * RB * GRB
c
c         which follows from Transpose(RB) * RB = Identity(1+NCOFRB).
c
c         However, for a general GFL, it is the case that
c
c           GFL =/= RB * Transpose(RB) * GFL.
c
c         Only if GFL was generated from a reduced basis coefficient
c         vector will equality apply.  In other words, if GFL was generated
c         from a reduced basis coefficient:
c
c           GFL = RB * GRB
c
c         then
c
c           RB * Transpose(RB) * GFL = RB * Transpose(RB) * (RB * GRB)
c           = RB * GRB = GFL
c
c         so in this strictly limited case,
c
c           RB * Transpose(RB) = Identity(NEQNFL).
c
c  RESRB  double precision RESRB(NCOFRB).
c         RESRB contains the residual in the reduced basis equations,
c         for the parameter values PAR and reduced basis coefficients GRB.
c
c  REYNLD Input, double precision REYNLD.
c         The current value of the Reynolds number parameter.
c
c  XRANGE double precision XRANGE.
c         XRANGE is the total width of the region.
c
c  YRANGE double precision YRANGE.
c         YRANGE is the total height of the region.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxelm
      integer maxnfl
      integer ncofrb
      integer nelem
      integer np
      integer npar
      integer nparf
c 
      double precision area(3,nelem)
      double precision grb(ncofrb)
      integer indx(3,np)
      integer nbcrb
      integer nferb
      integer node(6,nelem)
      integer nx
      integer ny
      double precision par(npar)
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision rb(maxnfl,maxcofrb)
      double precision resrb(ncofrb)
      double precision reynld
      double precision tauflo(nparf)
      double precision xc(np)
      double precision xrange
      double precision yc(np)
      double precision yrange
c
      call fxbcrb(grb,indx,maxcofrb,maxnfl,nbcrb,ncofrb,nelem,
     &  node,np,npar,nparf,nx,ny,par,rb,resrb,tauflo,xc,xrange,yc,
     &  yrange)

      call fxferb(area,grb,maxcofrb,maxelm,nbcrb,ncofrb,nelem,
     &  nferb,phirb,resrb,reynld)

      return
      end
      subroutine getgsen(grb,gsen,icolrb,maxcofrb,nbcrb,ncofrb,nsenfl,
     &  rbase)
c 
c***********************************************************************
c
cc GETGSEN computes the coefficients of the sensitivity matrix S,
c  using the fact that
c    S = Q*R
c  and given GRB, the coefficients of Q.  The calculation is simply
c    GSEN = R^(-1) * GRB
c  where R is a square upper triangular matrix.
c
c  The calculation is slightly more complicated than this, since  
c  we may have dropped some columns of Q (and hence rows and 
c  columns of R.
c
c
c  GETGSEN was modified on 14 August 1996.
c
c  
c  GRB    
c    double precision GRB(NCOFRB).
c    GRB contains the reduced basis coefficients of the current
c    estimate of the state solution.
c
c  GSEN
c    double precision GSEN(NBCRB+NCOFRB).
c    GSEN contains the "sensitivity coefficients".  These are simply
c    the reduced basis coefficients GRB after multiplication by
c    the inverse of RBASE, and accounting for the fact that only
c    some columns of the original set of candidate basis vectors
c    were used.
c
c  ICOLRB
c    integer ICOLRB(MAXCOFRB).
c    ICOLRB records which columns of the initial collection of
c    candidate basis vectors were actually chosen to form the
c    reduced basis.
c
c  MAXCOFRB
c    integer MAXCOFRB.
c    MAXCOFRB is the maximum legal value for NCOFRB, the number
c    of coefficients used to specify a particular reduced basis
c    solution.
c
c  NBCRB  
c    integer NBCRB.
c    NBCRB is the number of independent boundary condition
c    vectors used for the reduced basis.  NBCRB is normally
c    at least 1, and must be no more than MAXBCRB.
c
c  NCOFRB 
c    integer NCOFRB.
c    NCOFRB is the number of coefficients needed to determine
c    a particular reduced basis function.
c    NCOFRB is the sum of NBCRB and NFERB.
c
c  NSENFL 
c    integer NSENFL.
c    NSENFL is the number of full solution sensitivities to compute,
c    counting the 0-th order sensitivity as the first one.
c
c  RBASE  
c    double precision RBASE(MAXCOFRB,MAXCOFRB).
c    RBASE is the R factor in the QR factorization of the
c    reduced basis matrix.
c
c    In the special case where the reduced basis matrix is
c    exactly equal to SENFL, then RBASE equals SENRB.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer ncofrb
      integer nsenfl
c
      double precision grb(ncofrb)
      double precision gsen(nbcrb+nsenfl)
      integer i
      integer icol
      integer icolrb(ncofrb)
      integer j
      integer nbcrb
      double precision rbase(maxcofrb,maxcofrb)
c
      do i=1,nbcrb+nsenfl
        gsen(i)=0.0
      enddo

      do i=ncofrb,1,-1
        icol=icolrb(i)
        gsen(icol)=grb(i)
        do j=i+1,ncofrb
          gsen(icol)=gsen(icol)-rbase(i,j)*gsen(j)
        enddo
        gsen(icol)=gsen(icol)/rbase(i,i)
      enddo
 
      return
      end
      subroutine getrb(gfl,gflrb,icolrb,maxcofrb,maxnfl,nbcrb,ncofrb,
     &  neqnfl,nferb,npar,nsenfl,par,parrb,rb,rbase,senfl,senrb)
c
c***********************************************************************
c
cc GETRB computes the orthogonal reduced basis matrix RB.
c
c  GETRB is given:
c    NCOFRB, the number of reduced basis vectors;
c    GFL, the full solution at which the reduced basis vectors were
c      computed,
c    SENFL, the full solution sensitivities of orders 1 through
c      NCOFRB,
c    PAR, the parameter values at which the full solution was computed,
c  and computes
c    PARRB, a copy of PAR,
c    GFLRB, a copy of GFL,
c    RB, an NEQNFL by NCOFRB orthogonal matrix, whose columns
c     were initially SENFL(0), SENFL(1), ..., SENFL(NCOFRB), and which
c     is essentially the "Q" factor of this matrix,
c    RBASE, the "R" factor in the QR factorization of the matrix.
c
c
c  GETRB was revised on 14 August 1996.
c
c
c  GFL    Input, double precision GFL(NEQNFL), the current solution
c         estimate for the full problem.
c
c  GFLRB  Output, double precision GFLRB(NEQNFL), a copy of GFL, the full
c         solution at which the reduced basis was generated.
c
c  MAXNFL Input, integer MAXNFL.
c
c         The maximum number of equations allowed for the full system.
c
c  MAXNRB Input, integer MAXNRB.
c         MAXNRB is the maximum number of equations allowed for the 
c         reduced basis system.
c
c  NEQNFL Input, integer NEQNFL, the number of equations in the full system.
c
c  NCOFRB Input, integer NCOFRB, the number of equations in the reduced
c         system.
c
c  NPAR   Input, integer NPAR, the number of parameters.
c
c         NPAR = NPARF + NPARB + 1.
c
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c
c  PAR    Input, double precision PAR(NPAR).
c
c         PAR is the current estimate for the parameters.
c
c  PARRB  Output, double precision PARRB(NPAR).
c         PARRB contains the parameter values at which the reduced
c         basis matrix RB was computed.
c
c  RB     Output, double precision rb(maxnfl,maxcofrb).
c         RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
c         RB is generated by computing a finite element solution GFL,
c         which is saved for later reference as "GFLRB".
c         GFLRB is copied into the first column of RB.
c         Then, we compute the first NCOFRB derivatives of GFLRB with
c         respect to a parameter.  The first derivative
c         is stored in column 1 of RB, and so on.  Then we orthogonalize
c         the columns of RB.
c
c  RFACT  Output, double precision RFACT(MAXNRB,MAXNRB).
c         RFACT contains the NCOFRB by NCOFRB R factor in the QR
c         factorization of the NEQNFL by NCOFRB matrix SENFL.
c         RFACT may be used, in conjunction with the Q factor, to
c         make sensible Taylor predictions in the reduced basis.
c
c  SENFL  Input, double precision SENFL(MAXNFL,NCOFRB).
c         SENFL contains the first several order sensitivities of the
c         full solution with respect to the REYNLD parameter.
c
c         SENFL(I,J) contains the J-th sensitivity of the I-th full unknown
c         with respect to REYNLD.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxnfl
      integer neqnfl
      integer ncofrb
      integer npar
c
      double precision ddot
      double precision dnrm2
      double precision dtemp
      double precision dtemp1
      double precision gfl(neqnfl)
      double precision gflrb(neqnfl)
      integer i
      integer icolrb(maxcofrb)
      integer isen
      integer j
      integer jhi
      integer k
      integer mbcrb
      integer nbcrb
      integer nferb
      integer nsenfl
      double precision par(npar)
      double precision parrb(npar)
      double precision rb(maxnfl,maxcofrb)
      double precision rbase(maxcofrb,maxcofrb)
      double precision senfl(maxnfl,maxcofrb)
      double precision senrb(maxcofrb,maxcofrb)
c
c  Save the current parameters and solution.
c
      do i=1,npar
        parrb(i)=par(i)
      enddo
 
      do i=1,neqnfl
        gflrb(i)=gfl(i)
      enddo
c
c  Copy the sensitivities SENFL(:,:) into RB(:,NBCRB+1:).
c
      do i=1,neqnfl
        do isen=1,nsenfl
          rb(i,nbcrb+isen)=senfl(i,isen)
        enddo
      enddo
c
c  Initialize the R factor.
c
      do i=1,nbcrb+nsenfl
        do j=1,nbcrb+nsenfl
          if(i.eq.j)then
            rbase(i,j)=1.0
          else
            rbase(i,j)=0.0
          endif
        enddo
      enddo
c
c  Do a cheap sort of Gram Schmidt process to eliminate sensitivity columns that
c  are dependent on the boundary conditions or earlier sensitivities.
c
      mbcrb=nbcrb
      jhi=nbcrb+nsenfl

      ncofrb=0
      nbcrb=0
      nferb=0
 
      do j=1,jhi
c
c  For each column of the initial RB matrix,
c  
c  ...get the Euclidean norm of the column...
c
        dtemp1=dnrm2(neqnfl,rb(1,j),1)
c
c  ...and then subtract off the projections onto the 
c  already accepted columns...
c 
        do i=1,ncofrb
          dtemp=ddot(neqnfl,rb(1,i),1,rb(1,j),1)
          rbase(i,ncofrb+1)=dtemp
          call daxpy(neqnfl,-dtemp,rb(1,i),1,rb(1,j),1)
        enddo
c
c  ...then get the Euclidean norm of what is left,
c  save it in RBASE, and normalize the column.
c
        dtemp=dnrm2(neqnfl,rb(1,j),1)
        rbase(ncofrb+1,ncofrb+1)=dtemp

        do i=1,neqnfl
          rb(i,ncofrb+1)=rb(i,j)/dtemp
        enddo
c
c  Now decide whether to accept this column.   
c
        if(dtemp.ne.0.0.and.dtemp.gt.0.00001*dtemp1)then

          ncofrb=ncofrb+1

          if(j.le.mbcrb)then
            nbcrb=nbcrb+1
          else
            nferb=nferb+1
          endif
                  
          icolrb(ncofrb)=j

        endif

      enddo

      write(*,*)' '
      write(*,*)'GETRB - Information:'
      write(*,*)'  # of BC vectors, NBCRB  = ',nbcrb
      write(*,*)'  # of FE vectors, NFERB  = ',nferb
      write(*,*)'  # of RB coeffs,  NCOFRB = ',ncofrb

c
c  Compute SENRB as Transpose(Q) * SENFL.
c  NSENFL may, or may not, equal NCOFRB.
c
      write(*,*)' '
      write(*,*)'GETRB - Note:'
      write(*,*)'  Automatically computing SENRB.'
 
      do i=1,ncofrb
        do j=1,nsenfl
          senrb(i,j)=0.0
          do k=1,ncofrb
            senrb(i,j)=senrb(i,j)+rb(k,i)*senfl(k,j)
          enddo
        enddo
      enddo
 
      return
      end
      subroutine getsenfl(afl,area,eqn,gfl,indx,ipivfl,ldafl,
     &  maxcofrb,maxnfl,nelem,neqnfl,nlband,node,np,npar,nsenfl,
     &  par,phifl,resfl,senfl)
c
c***********************************************************************
c
cc GETSENFL computes the matrix SENFL of sensitivity vectors.
c
c  To do so, it first saves a copy of GFL, the current solution.
c
c  Then it constructs a matrix SENFL, whose first column is d GFL/d REYNLD, 
c  which must be computed by solving a linear system.  
c
c  Similarly, higher derivatives of GFL with respect to REYNLD are computed
c  and stored in successive columns of SENFL.  
c
c
c  GETSENFL was revised on 22 July 1996.
c
c
c  AFL    
c    double precision AFL(LDAFL,MAXNFL).
c    If Newton iteration is being carried out, AFL contains the
c    Jacobian matrix for the full system.
c    If Picard iteration is being carried out, AFL contains the
c    Picard matrix for the full system.
c
c    AFL is stored in LINPACK general band storage mode, with
c    logical dimensions (3*NLBAND+1, NEQNFL).
c
c    Where is the (I,J) entry of AFL actually stored?
c    AFL has actual storage for such an entry only if
c      -NLBAND <= I-J <= NLBAND.
c    In such a case, the (I,J) entry is actually stored in
c      AFL(I-J+2*NLBAND+1,J)
c
c  AREA   
c    double precision AREA(3,MAXELM).
c    AREA contains a common factor multiplying the term associated
c    with a quadrature point in a given element, namely,
c
c      AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c    or, if the element is isoperimetric,
c
c      AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c    Here Ar(IELEM) represents the area of element IELEM.
c
c  EQN    
c    character*2 EQN(MAXNFL).
c    EQN records the "type" of each equation that will be generated, and
c    which is associated with an unknown.  
c
c    'U'  A horizontal momentum equation.
c    'UB' The condition U=0 applied at a node on the bump.
c    'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c    'UW' The condition U=0 applied at a node on a fixed wall.
c    'U0' A dummy value of U=0 should be set.
c
c    'V'  A vertical momentum equation.
c    'VB' The condition V=0 applied at a node on the bump.
c    'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c    'VW' The condition V=0 applied at a node on a fixed wall.
c    'V0' A dummy value of V=0 should be set.
c
c    'P'  A continuity equation.
c    'PB' The condition P=0 applied at (XMAX,YMAX).
c    'P0' A dummy value of P=0 should be set.
c
c  GFL    
c    double precision GFL(NEQNFL).
c    GFL contains the current solution estimate for the full problem,
c    containing the pressure and velocity coefficients.
c    The vector INDX must be used to index this data.
c
c  INDX   
c    integer INDX(3,NP).
c    INDX(I,J) contains, for each node J, the global index of U,
c    V and P at that node, or 0 or a negative value.  The global
c    index of U, V, or P is the index of the coefficient vector
c    that contains the value of the finite element coefficient
c    associated with the corresponding basis function at the
c    given node.
c
c  IPIVFL Workspace, 
c    integer IPIVFL(NEQNFL).
c    IPIVFL is a pivot vector for the solution of the full
c    linear system.
c
c  LDAFL  
c    integer LDAFL.
c    LDAFL is the first dimension of the matrix AFL as declared in
c    the main program.  LDAFL must be at least 3*NLBAND+1.
c
c  MAXCOFRB
c    integer MAXCOFRB.
c    MAXCOFRB is the maximum legal value for NCOFRB, the number
c    of coefficients used to specify a particular reduced basis
c    solution.
c
c  MAXNFL 
c    integer MAXNFL.
c    MAXNFL is the maximum number of equations or coefficients allowed
c    for the full system.  MAXNFL must be used instead of NEQNFL as
c    the leading dimension of certain multi-dimensional arrays.
c
c  NELEM  
c    integer NELEM.
c    NELEM is the number of elements.
c    NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NEQNFL 
c    integer NEQNFL.
c    NEQNFL is the number of equations (and coefficients) in the full
c    finite element system.
c
c  NLBAND 
c    integer NLBAND.
c    NLBAND is the lower bandwidth of the matrix AFL.
c    The zero structure of AFL is assumed to be symmetric, and so
c    NLBAND is also the upper bandwidth of AFL.
c
c  NODE   
c    integer NODE(6,MAXELM) or NODE(6,NELEM).
c    NODE(I,J) contains, for an element J, the global index of
c    the node whose local number in J is I.
c
c  NP     
c    integer NP.
c    NP is the number of nodes used to define the finite element mesh.
c    Typically, the mesh is generated as a rectangular array, with
c    an odd number of nodes in the horizontal and vertical directions.
c    The formula for NP is NP=(2*NX-1)*(2*NY-1).
c
c  NPAR   
c    integer NPAR.
c    NPAR is the number of parameters.
c      NPAR = NPARF + NPARB + 1.
c    The parameters control the shape and strength of the inflow, 
c    the shape of the bump, and the value of the Reynolds number.
c
c  NSENFL 
c    integer NSENFL.
c    NSENFL is the number of full solution sensitivities to compute,
c    counting the 0-th order sensitivity as the first one.
c
c  PAR    
c    double precision PAR(NPAR).
c    PAR contains the values of the problem parameters.
c
c      PAR(1:NPARF)             = inflow controls.
c      PAR(NPARF+1:NPARF+NPARB) = bump controls.
c      PAR(NPARF+NPARB+1)       = the REYNLD parameter.
c
c  PHIFL  
c    double precision PHIFL(3,6,10,NELEM).
c    PHIFL contains the value of a finite element basis function, its
c    derivative, or other information, evaluated at the quadrature
c    points (which are the element midside nodes).
c
c    The meaning of the entry PHIFL(I,J,K,L) is as follows.
c    For the quadrature point I, and basis function J, in element L,
c    PHIFL(I,J,K,L) represents the value of:
c
c      K= 1, W, the finite element basis function for velocities;
c      K= 2, dWdX, the X derivative of W;
c      K= 3, dWdY, the Y derivative of W;
c      K= 4, Q, the finite element basis function for pressures;
c      K= 5, dQdX, the X derivative of Q;
c      K= 6, dQdY, the Y derivative of Q;
c      K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c      K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c      K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c      K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c    In particular, PHIFL(I,J,K,L) is the value of the quadratic
c    basis function W associated with local node J in element L,
c    evaluated at quadrature point I.
c
c    Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6,
c    since there are only three linear basis functions.
c
c  RESFL  
c    double precision RESFL(NEQNFL).
c    RESFL contains the residual in the full basis equations.
c
c  SENFL  
c    double precision SENFL(MAXNFL,MAXCOFRB).
c    Columns 1 through NSENFL of SENFL contain the sensitivities 
c    of the full solution with respect to the REYNLD parameter, for
c    orders 0 through NSENFL-1.
c
c    SENFL(I,J) contains the (J-1)-th sensitivity of the I-th full unknown
c    with respect to REYNLD.
c
      implicit double precision (a-h,o-z)
c
      integer ldafl
      integer maxcofrb
      integer maxnfl
      integer nelem
      integer neqnfl
      integer np
      integer npar
c
      double precision afl(ldafl,maxnfl)
      double precision area(3,nelem)
      character*2 eqn(neqnfl)
      double precision gfl(neqnfl)
      integer i
      integer indx(3,np)
      integer ipivfl(maxnfl)
      integer isen
      integer nlband
      integer node(6,nelem)
      integer nsenfl
      double precision par(npar)
      double precision phifl(3,6,10,nelem)
      double precision resfl(neqnfl)
      double precision reynld
      double precision rpnrm
      double precision ruvnrm
      double precision senfl(maxnfl,maxcofrb)
      double precision spnrm
      double precision suvnrm
c
      reynld=par(npar)
c
c  Compute, one at a time, the columns of the RB matrix.
c
      write(*,*)' '
      write(*,*)'GETSENFL - Information:'
      write(*,*)'  Number of sensitivities requested, NSENFL=',nsenfl
      write(*,*)' '
      write(*,*)'Order     MxNorm(UVRHS) MxNorm(PRHS)'
     &  //'  MxNorm(UVSen) MxNorm(PSen)'
 
      do i=1,neqnfl
        senfl(i,1)=gfl(i)
      enddo
 
      isen=0
      call uvpnrm(senfl(1,1),indx,neqnfl,np,spnrm,suvnrm)
      write(*,'(1x,i6,28x,2g14.6)')isen,suvnrm,spnrm
 
      do isen=1,nsenfl-1
c
c  Given the current solution and lower order sensitivities
c  in SENFL, compute in RESFL the right hand side for sensitivity
c  of order ISEN.
c
        call reysen(area,eqn,indx,isen,maxcofrb,maxnfl,nelem,
     &    neqnfl,node,np,phifl,resfl,reynld,senfl)
c
c  Compute the norm of this right hand side.
c
        call uvpnrm(resfl,indx,neqnfl,np,rpnrm,ruvnrm)
c
c  Solve the linear system AFL * SENFL(ISEN) = RESFL
c
        call dsolfl(afl,ldafl,neqnfl,nlband,nlband,ipivfl,resfl)
c
c  Get the norm of this new sensitivity.
c
        call uvpnrm(resfl,indx,neqnfl,np,spnrm,suvnrm)

        write(*,'(1x,i6,4g14.6)')isen,ruvnrm,rpnrm,suvnrm,spnrm
c
c  Copy the new sensitivity into the SENFL array.
c
        do i=1,neqnfl
          senfl(i,isen+1)=resfl(i)
        enddo

      enddo

      return
      end
      subroutine getsenrb(maxcofrb,maxnfl,ncofrb,neqnfl,nsenfl,rb,
     &  senfl,senrb)
c
c***********************************************************************
c  
cc GETSENRB computes the value of the reduced sensitivities by
c  multiplying the full sensitivities by Transpose(Q).
c
c
c  GETSENRB was written on 01 August 1996.
c
c
c  MAXCOFRB
c         Integer MAXCOFRB.
c         MAXCOFRB is the maximum legal value for NCOFRB, the number
c         of coefficients used to specify a particular reduced basis
c         solution.
c
c  MAXNFL integer MAXNFL.
c         MAXNFL is the maximum number of equations or coefficients allowed
c         for the full system.  MAXNFL must be used instead of NEQNFL as
c         the leading dimension of certain multi-dimensional arrays.
c
c  NCOFRB integer NCOFRB.
c         NCOFRB is the number of coefficients needed to determine
c         a particular reduced basis function.
c         NCOFRB is the sum of NBCRB and NFERB.
c
c  NEQNFL integer NEQNFL.
c         NEQNFL is the number of equations (and coefficients) in the full
c         finite element system.
c
c  NSENFL integer NSENFL.
c         NSENFL is the number of full solution sensitivities to compute,
c         counting the 0-th order sensitivity as the first one.
c
c  RB     double precision RB(MAXNFL,MAXCOFRB).
c
c         RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
c         RB is generated by computing a finite element solution GFL,
c         which is saved for later reference as "GFLRB".
c         GFLRB is copied into the first column of RB.
c         Then, we compute the first NCOFRB derivatives of GFLRB with
c         respect to a parameter.  The first derivative
c         is stored in column 1 of RB, and so on.  
c
c         Now we compute the QR factorization of this matrix.
c
c         We intend that NEQNFL >> NCOFRB, and RB is a matrix with orthogonal
c         columns, so that:
c
c           Transpose(RB) * RB = Identity(NCOFRB)
c
c
c         If GFL is any set of finite element coefficients, the corresponding
c         set of reduced basis coefficients can be computed as:
c
c           GRB = Transpose(RB) * GFL
c
c         If GRB is a set of reduced basis coefficients, a corresponding
c         set of finite element coefficients can be computed as:
c
c           GFL = RB * GRB.
c
c         While it is the case that you can expand and then reduce,
c         and always get the same result, it is not the case that
c         when you reduce and then expand you get the same result!
c
c         It is true, for ANY GRB, that
c
c           GRB = Transpose(RB) * RB * GRB
c
c         which follows from Transpose(RB) * RB = Identity(NCOFRB).
c
c         However, for a general GFL, it is the case that
c
c           GFL =/= RB * Transpose(RB) * GFL.
c
c         Only if GFL was generated from a reduced basis coefficient
c         vector will equality apply.  In other words, if GFL was generated
c         from a reduced basis coefficient:
c
c           GFL = RB * GRB
c
c         then
c
c           RB * Transpose(RB) * GFL = RB * Transpose(RB) * (RB * GRB)
c           = RB * GRB = GFL
c
c         so in this strictly limited case,
c
c           RB * Transpose(RB) = Identity(NEQNFL).
c
c  SENFL  double precision SENFL(MAXNFL,MAXCOFRB).
c         Columns 1 through NSENFL of SENFL contain the sensitivities 
c         of the full solution with respect to the REYNLD parameter, for
c         orders 0 through NSENFL-1.
c
c         SENFL(I,J) contains the (J-1)-th sensitivity of the I-th full unknown
c         with respect to REYNLD.
c
c  SENRB  double precision SENRB(MAXCOFRB,NSENFL).
c         SENRB contains the first NSENFL order sensitivities of the
c         reduced solution with respect to the REYNLD parameter.
c
c         SENRB(I,J) contains the (J-1)-th sensitivity of the I-th reduced 
c         unknown with respect to REYNLD.
c
c         SENRB is computed by premultiplying SENFL by Transpose(RB).
c           SENRB = Transpose(RB) * SENFL.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxnfl
c
      integer i
      integer j
      integer k
      integer ncofrb
      integer neqnfl
      integer nsenfl
      double precision rb(maxnfl,maxcofrb)
      double precision senfl(maxnfl,maxcofrb)
      double precision senrb(maxcofrb,maxcofrb)
c
c  Multiply SENRB = QT * SENFL
c
      do i=1,ncofrb
        do j=1,nsenfl
          senrb(i,j)=0.0
          do k=1,neqnfl
            senrb(i,j)=senrb(i,j)+rb(k,i)*senfl(k,j)
          enddo
        enddo
      enddo
 
      return
      end
      subroutine gfl2rb(gfl,grb,maxnfl,ncofrb,neqnfl,rb)
c
c***********************************************************************
c
cc GFL2RB projects a full solution vector GFL into 
c  GRB, the corresponding solution vector for the reduced system.
c
c  The relationship used is
c
c    GRB = Q^T * GFL.
c
c
c  GFL2RB was written on 02 August 1996.
c
c
c  GFL    Input, double precision GFL(NEQNFL).
c         GFL contains the current solution estimate for the full problem,
c         containing the pressure and velocity coefficients.
c         The vector INDX must be used to index this data.
c
c  GRB    Output, double precision GRB(NCOFRB).
c         GRB contains the reduced basis coefficients of the current
c         estimate of the state solution.
c
c  MAXNFL Input, integer MAXNFL, the maximum value of NEQNFL, used as
c         the leading dimension of RB.
c
c  NCOFRB Input, integer NCOFRB, the number of coefficients for the
c         reduced system.
c
c  NEQNFL Input, integer NEQNFL, the number of coefficients for the
c         full system.
c
c  NCOFRB Input, integer NCOFRB, the number of coefficients for the
c         reduced system.
c
c  RB     Input, double precision RB(MAXNFL,MAXCOFRB).
c
c         RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
c  RB     Input, double precision rb(maxnfl,ncofrb).
c         RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
      implicit double precision (a-h,o-z)
c
      integer maxnfl
      integer neqnfl
      integer ncofrb
c
      double precision gfl(neqnfl)
      double precision grb(ncofrb)
      integer i
      integer j
      double precision rb(maxnfl,ncofrb)
c
c  Multiply the full vector GFL by RB transpose.
c
      do i=1,ncofrb
        grb(i)=0.0
        do j=1,neqnfl
          grb(i)=grb(i)+rb(j,i)*gfl(j)
        enddo
      enddo

      return
      end
      subroutine grb2fl(gfl,grb,maxnfl,ncofrb,neqnfl,rb)
c
c***********************************************************************
c
cc GRB2FL computes the full solution GFL represented by a set of
c  reduced basis coefficients GRB.
c
c  GRB2FL is given:
c    NCOFRB, the number of reduced basis vectors and coefficients;
c    NEQNFL, the number of full basis vectors and coefficients;
c    GRB, the reduced basis coefficients;
c    RB, the matrix of reduced basis vectors.
c  and computes
c    GFL, the corresponding full solution.
c
c
c  GRB2FL was revised on 03 August 1996.
c
c
c  GFL    Output, double precision GFL(NEQNFL), the coefficients for the
c         full system equivalent to the reduced set GRB.
c
c  GRB    Input, double precision GRB(NCOFRB), coefficients for the
c         reduced system.
c
c  MAXNFL Input, integer MAXNFL, the maximum value of NEQNFL, used as
c         the leading dimension of RB.
c
c  NEQNFL Input, integer NEQNFL, the number of coefficients for the
c         full system.
c
c  NCOFRB Input, integer NCOFRB.
c         NCOFRB is the number of basis functions used for the
c         reduced basis method.  (The first basis vector is labeled
c         "0").  In this program, that amounts to the number of columns
c         in the matrix RB.  NCOFRB is also the number of reduced basis 
c         state equations, and reduced basis coefficients GRB.
c
c  RB     Input, double precision rb(maxnfl,ncofrb).
c         RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
c         RB is generated by computing a finite element solution GFL,
c         which is saved for later reference as "GFLRB".
c         GFLRB is copied into the first column of RB.
c         Then, we compute the first NCOFRB derivatives of GFLRB with
c         respect to a parameter.  The first derivative
c         is stored in column 1 of RB, and so on.  Then we orthogonalize
c         the columns of RB.
c
c         We intend that NEQNFL >> NCOFRB, and RB is a matrix with orthogonal
c         columns, so that:
c
c           Transpose(RB) * RB = Identity(NCOFRB)
c
c
c         If GFL is any set of finite element coefficients, the corresponding
c         set of reduced basis coefficients can be computed as:
c
c           GRB = Transpose(RB) * GFL
c
c         If GRB is a set of reduced basis coefficients, a corresponding
c         set of finite element coefficients can be computed as:
c
c           GFL = RB * GRB.
c
c         While it is the case that you can expand and then reduce,
c         and always get the same result, it is not the case that
c         when you reduce and then expand you get the same result!
c
c         It is true, for ANY GRB, that
c
c           GRB = Transpose(RB) * RB * GRB
c
c         which follows from Transpose(RB) * RB = Identity(NCOFRB).
c
c         However, for a general GFL, it is the case that
c
c           GFL =/= RB * Transpose(RB) * GFL.
c
c         Only if GFL was generated from a reduced basis coefficient
c         vector will equality apply.  In other words, if GFL was generated
c         from a reduced basis coefficient:
c
c           GFL = RB * GRB
c
c         then
c
c           RB * Transpose(RB) * GFL = RB * Transpose(RB) * (RB * GRB)
c           = RB * GRB = GFL
c
c         so in this strictly limited case,
c
c           RB * Transpose(RB) = Identity(NEQNFL).
c
      implicit double precision (a-h,o-z)
c
      integer maxnfl
      integer ncofrb
      integer neqnfl
c
      double precision gfl(neqnfl)
      double precision grb(ncofrb)
      integer i
      integer j
      double precision rb(maxnfl,ncofrb)
c
      do i=1,neqnfl
        gfl(i)=0.0
        do j=1,ncofrb
          gfl(i)=gfl(i)+rb(i,j)*grb(j)
        enddo
      enddo

      return
      end
      subroutine hello(maxnx,maxny)
c
c***********************************************************************
c
cc HELLO prints the program name, date of revision, time and date,
c  and maximum problem size.
c
c
c  HELLO was written on 01 July 1996.
c
c
c  MAXNX  Input, integer MAXNX.
c         MAXNX is the maximum size of NX that the program can handle.
c
c  MAXNY  Input, integer MAXNY.
c         MAXNY is the maximum size of NY that the program can handle.
c
      implicit double precision (a-h,o-z)
c
      character*9 chdate
      character*8 chtime
      integer maxnx
      integer maxny
      character*10 sysnam

      call system(sysnam)
c
c  Say hello.
c
      write(*,*)'John Burkardt'
      write(*,*)'438 Carver Hall'
      write(*,*)'ISU Math Department'
      write(*,*)' '
      write(*,*)'HELLO!'
      write(*,*)' '
      write(*,*)'  This is ARBY3, the version of 14 August 1996.'
      write(*,*)'  This run is being made on the '//sysnam
      write(*,*)'  The maximum problem size is MAXNX=',maxnx,
     &  ' by MAXNY=',maxny

      return
      end 
      subroutine help
c
c***********************************************************************
c
cc HELP prints out a list of the interactive commands which the
c  user can give.
c
c
c  HELP was written on 01 July 1996.
c
c
      write(*,*)' '
      write(*,*)'HELP'
      write(*,*)' '
      write(*,*)'Compare    Compare GFL to GFLSAV;'
      write(*,*)'CostFL     Evaluate cost of GFL;'
      write(*,*)'CostRB     Evaluate cost of GRB;'
      write(*,*)'DetFpFL    Determinant of full jacobian;'
      write(*,*)'DetFpRB    Determinant of reduced jacobian;'
      write(*,*)'DifFPRB    FD estimate of reduced jacobian;'
      write(*,*)'DifSenFL   FD estimate of full sensitivities;'
      write(*,*)'DifSenRB   FD estimate of reduced sensitivities;'
      write(*,*)'DisFil=    Name the DISPLAY output file;'
      write(*,*)'DisPlot    Make DISPLAY plot file of current data;'
      write(*,*)'DREY=      Set REYNLD Taylor increment;'
      write(*,*)'Echo       Echo user commands;'
      write(*,*)'EPSDIF=    Set finite difference increment;'
      write(*,*)'Expand GRB Compute GFL = RB*GRB;'
      write(*,*)'FPFL       Evaluate full jacobian;'
      write(*,*)'FPIRB      Evaluate reduced jacobian indirectly;'
      write(*,*)'FPRB       Evaluate reduced jacobian;'
      write(*,*)'FPRB=0     Zero out reduced jacobian;'
      write(*,*)'FxFl       Evaluate full residual, FXFL(GFL);'
      write(*,*)'FxIRB      Evaluate FXRB=RB^T*FX(RB*GRB), indirectly;'
      write(*,*)'FxRB       Evaluate FXRB=FXrb(GRB) directly;'
      write(*,*)'FxRB=0     Set vector FXRB=0;'
      write(*,*)'GetGSEN    Compute sensitivity coefficients;'
      write(*,*)'GetRB      Compute reduced basis;'
      write(*,*)'GetSenFL   Compute full sensitivities;'
      write(*,*)'GetSenRB   Compute reduced sensitivities;'
      write(*,*)'GFL=       Set current full solution,'
      write(*,*)'           Legal values: 0, GFLSAV, GFLTAY, TAYLOR;'
      write(*,*)'GFLSAV=GFL Save current GFL value;'
      write(*,*)'GFLTAY=    Set Taylor base solution,'
      write(*,*)'           Legal values: 0, GFL, GFLSAV;'
      write(*,*)'GFLTMP=    Set temporary base solution,'
      write(*,*)'           Legal values: 0, GFL, GFL-GFLSAV,'
      write(*,*)'           GFL-GFLTAR, GFLSAV, GFLSAV-GFLTAY;'
      write(*,*)'GRB(*)=*   Set an entry of GRB to a value;'
      write(*,*)'GRB=       Set current reduced solution GRB,'
      write(*,*)'           Legal values: 0, GRBSAV, GRBTAY, TAYLOR;'
      write(*,*)'GRB=(*,*,...,*)  Set individual entries of GRB;'
      write(*,*)'GRBSAV=    Save a GRB value,'
      write(*,*)'           Legal values: 0, GRB;'
      write(*,*)'GRBTAY=    Set Taylor base solution,'
      write(*,*)'           Legal values: 0, GRB, GRBSAV;'
      write(*,*)'GridX=     Uniform, Cos, or SqrtSin;'
      write(*,*)'GridY=     Uniform, Cos, or SqrtSin;'
      write(*,*)'Hello      Print program version and other info;'
      write(*,*)'Help       Print list of commands;'
      write(*,*)'IBS=       Set bump shape option;'
      write(*,*)'IBUMP=     Set bump option;'
      write(*,*)'IFS=       Set inflow shape option;'
      write(*,*)'IHI=       Maximum row for printout,'
      write(*,*)'           NCOFRB, NEQNFL, NP are legal;'
      write(*,*)'IJAC=      Set Jacobian option;'
      write(*,*)'ILO=       Minimum row for printout;'
      write(*,*)'Init       Initialize variables;'
      write(*,*)'IOPT(*)=   Specify free or fixed variables;'
      write(*,*)'IWRITE=    Set level of output;'
      write(*,*)'JHI=       Maximum column for printout,'
      write(*,*)'           NCOFRB, NEQNFL, NSENFL are legal;'
      write(*,*)'JLO=       Minimum column for printout,'
      write(*,*)'L2NORM *   Compute big L2 norm of *,'
      write(*,*)'           * = GFL, GFLSAV, GFLTAR, GFLTAY, GFLTMP;'
      write(*,*)'MAXNEW=    Set number of Newton steps;'
      write(*,*)'MAXOPT=    Set number of optimization steps;'
      write(*,*)'MAXSIM=    Set number of simple steps;'
      write(*,*)'NBCRB=     Set number of boundary conditions;'
      write(*,*)'           (0 or 1, right now);'
      write(*,*)'NewtFL     Newton''s method applied to GFL;'
      write(*,*)'NewtRB     Newton''s method applied to GRB;'
      write(*,*)'NPARB=     Set number of bump parameters;'
      write(*,*)'NPARF=     Set number of inflow parameters;'
      write(*,*)'NSENFL=    Set number of full sensitivities;'
      write(*,*)'NTAY=      Set number of Taylor vectors to use,'
      write(*,*)'           NTAY=NCOFRB is legal, too;'
      write(*,*)'NX=        Set number of X nodes;'
      write(*,*)'NY=        Set number of Y nodes;'
      write(*,*)'OptDifFl   Optimize the full system, using'
      write(*,*)'           FD estimates for gradients;'
      write(*,*)'PAR(*)=    Set a parameter;'
      write(*,*)'PARTAR(*)= Set a target parameter;'
      write(*,*)'PicFL      Picard''s method applied to GFL;'
      write(*,*)'PicRB      Picard''s method applied to GRB;'
      write(*,*)'PrDat      Print the variable values;'
      write(*,*)'PrElem     Print element data, for elements'
      write(*,*)'           ILO to IHI;'
      write(*,*)'PrFPFL     Print full jacobian,'
      write(*,*)'           Equations ILO to IHI,'
      write(*,*)'           Variables JLO to JHI;'
      write(*,*)'PrFPRB     Print reduced jacobian,'
      write(*,*)'           Equations ILO to IHI,'
      write(*,*)'           Variables JLO to JHI;'
      write(*,*)'PrFXFL     Print FXFL(GFL),'
      write(*,*)'           nodes ILO to IHI;'
      write(*,*)'PrFXFLNrm  Print norm of FXFL(GFL);'
      write(*,*)'PrFXRB     Print FXRB(GRB),'
      write(*,*)'           equations ILO to IHI;'
      write(*,*)'PrGFL      Print full solution GFL,'
      write(*,*)'           nodes ILO to IHI;'
      write(*,*)'PrGFLNrm   Print GFL and FX(GFL) norms;'
      write(*,*)'PrGRB      Print reduced solution GRB;'
      write(*,*)'PrGSEN     Print sensitivity coefficients;'
      write(*,*)'PrINDX     Print node/equation table INDX,'
      write(*,*)'           nodes ILO to IHI.'
      write(*,*)'PrPar      Print current parameters;'
      write(*,*)'PrRBase    Print the reduced basis R factor;'
      write(*,*)'PrRB       Print reduced basis matrix RB,'
      write(*,*)'           nodes ILO to IHI,'
      write(*,*)'           columns JLO to JHI;'
      write(*,*)'PrSenFL    Print full sensitivity matrix SENFL,'
      write(*,*)'           nodes ILO to IHI,'
      write(*,*)'           sensitivities JLO to JHI;'
      write(*,*)'PrSenNrm   Print full sensitivity norms;'
      write(*,*)'PrSenRB    Print reduced sensitivity matrix SENRB,'
      write(*,*)'           rows ILO to IHI,'
      write(*,*)'           columns JLO to JHI;'
      write(*,*)'PrUVPGFL   Print full solution at nodes in'
      write(*,*)'           XMIN, YMIN, XMAX, YMAX;'
      write(*,*)'PrUVPRB    Print reduced basis vectors at nodes in'
      write(*,*)'           XMIN, YMIN, XMAX, YMAX;'
      write(*,*)'PrUVPSENFL Print sensitivity vectors at nodes in'
      write(*,*)'           XMIN, YMIN, XMAX, YMAX;'
      write(*,*)'PrUVPGRB   Print reduced solution at nodes in'
      write(*,*)'           XMIN, YMIN, XMAX, YMAX;'
      write(*,*)'PrXY       Print X, Y nodal coordinates,'
      write(*,*)'           nodes ILO to IHI;'
      write(*,*)'Reduce GFL Compute GRB = RB^T * GFL;'
      write(*,*)'REGION=    Cavity, Channel, or Step;'
      write(*,*)'REYNLD=    Set REYNLD parameter;'
      write(*,*)'REYTAY=    Set REYNLD parameter for Taylor;'
      write(*,*)'           ("REYTAY=REYNLD" is legal.)'
      write(*,*)'SetGeo     Set problem geometric data;'
      write(*,*)'SetLog     Set problem logical data;'
      write(*,*)'Stop       Stop the program;'
      write(*,*)'Target     Save current GFL as GFLTAR;'
      write(*,*)'Test2      Compare full and reduced U,V,P'
      write(*,*)'           in elements ILO through IHI;'
      write(*,*)'Test3      Compare RB*RFACT and SENFL;'
      write(*,*)'Test4      Compare regular and FD full sens;'
      write(*,*)'Test5      Compare RB*RFACT and old RB;'
      write(*,*)'Time       Print elapsed time;'
      write(*,*)'TOLNEW=    Set Newton tolerance;'
      write(*,*)'TOLOPT=    Set optimization tolerance;'
      write(*,*)'TOLSIM=    Set Picard tolerance;'
      write(*,*)'TecFil=    Name the TECPLOT output file;'
      write(*,*)'TecPlot    Make TECPLOT plot file of current data;'
      write(*,*)'WATEB=     Set bump weight in cost;'
      write(*,*)'WATEP=     Set pressure weight in cost;'
      write(*,*)'WATEU=     Set H-velocity weight in cost;'
      write(*,*)'WATEV=     Set V-velocity weight in cost;'
      write(*,*)'XBL=       Set left bump X coordinate;'
      write(*,*)'XBR=       Set right bump X coordinate;'
      write(*,*)'XMAX=      Specify XMAX.'
      write(*,*)'XMIN=      Specify XMIN.'
      write(*,*)'XPROF=     Set profile X coordinate;'
      write(*,*)'XRANGE=    Set width of region;'
      write(*,*)'YBL=       Set left bump Y coordinate;'
      write(*,*)'YBR=       Set right bump Y coordinate;'
      write(*,*)'YMAX=      Specify YMAX.'
      write(*,*)'YMIN=      Specify YMIN.'
      write(*,*)'YRANGE=    Set height of region;'

      return
      end
      subroutine init(afl,arb,area,command,cost,costb,costp,costu,
     &  costv,difcof,disfil,drey,epsdif,eqn,etaq,gfl,gflafl,
     &  gflrb,gflsav,gflsen,gfltar,gfltay,grb,grbarb,grbsav,
     &  grbsen,grbtay,gridx,gridy,hx,hy,ibs,ibump,icolrb,ierror,
     &  ifs,ihi,ijac,ilo,indx,iopt,ipar,
     &  ipivfl,ipivrb,isotri,iwrite,jhi,jlo,
     &  ldafl,maxcofrb,maxelm,maxnew,maxnfl,maxnp,
     &  maxny,maxopt,maxpar,maxparb,maxparf,maxsim,
     &  nbcrb,ncofrb,nelem,neqnfl,nferb,
     &  nlband,node,nodelm,np,npar,nparb,nparf,npe,nprof,nsenfl,ntay,
     &  numnew,numopt,numsim,nx,
     &  ny,par,parafl,pararb,pardif,parrb,parsav,parsen,partar,
     &  phifl,phirb,rb,rbase,region,resfl,resflsav,resrb,reynld,reytay,
     &  senfl,
     &  senrb,splbmp,splflo,taubmp,tauflo,tecfil,tolnew,tolopt,
     &  tolsim,value,wateb,watep,wateu,watev,wquad,xbl,xbr,
     &  xc,xmax,xmin,xprof,xquad,xrange,xsiq,
     &  ybl,ybr,yc,ymax,ymin,yquad,yrange)
c 
c***********************************************************************
c
cc INIT sets problem data to default values.
c
c
c  INIT was modified on 01 July 1996.
c
c
      implicit double precision (a-h,o-z)
c
      integer ldafl
      integer maxcofrb
      integer maxelm
      integer maxnfl
      integer maxnp
      integer maxny
      integer maxpar
      integer maxparb
      integer maxparf
c
      double precision afl(ldafl,maxnfl)
      double precision arb(maxcofrb,maxcofrb)
      double precision area(3,maxelm)
      character*80 command
      double precision cost
      double precision costb
      double precision costp
      double precision costu
      double precision costv
      double precision difcof(maxcofrb)
      character*30 disfil
      double precision drey
      double precision epsdif
      character*2 eqn(maxnfl)
      double precision etaq(3)
      double precision gfl(maxnfl)
      double precision gflafl(maxnfl)
      double precision gflrb(maxnfl)
      double precision gflsav(maxnfl)
      double precision gflsen(maxnfl)
      double precision gfltar(maxnfl)
      double precision gfltay(maxnfl)
      double precision grb(maxcofrb)
      double precision grbarb(maxcofrb)
      double precision grbsav(maxcofrb)
      double precision grbsen(maxcofrb)
      double precision grbtay(maxcofrb)
      character*20 gridx
      character*20 gridy
      double precision hx
      double precision hy
      integer i
      integer ibs
      integer ibump
      integer icolrb(maxcofrb)
      integer ierror
      integer ifs
      integer ihi
      integer ijac
      integer ilo
      integer indx(3,maxnp)
      integer iopt(maxpar)
      integer ipar
      integer ipivfl(maxnfl)
      integer ipivrb(maxcofrb)
      integer isotri(maxelm)
      integer iwrite
      integer j
      integer jhi
      integer jlo
      integer k
      integer l
      integer maxnew
      integer maxopt
      integer maxsim
      integer nbcrb
      integer ncofrb
      integer nelem
      integer neqnfl
      integer nferb
      integer nlband
      integer node(6,maxelm)
      integer nodelm(maxnp)
      integer np
      integer npar
      integer nparb
      integer nparf
      integer npe
      integer nprof(2*maxny-1)
      integer nsenfl
      integer ntay
      integer numnew
      integer numopt
      integer numsim
      integer nx
      integer ny
      double precision par(maxpar)
      double precision parafl(maxpar)
      double precision pararb(maxpar)
      double precision pardif(maxpar)
      double precision parrb(maxpar)
      double precision parsav(maxpar)
      double precision parsen(maxpar)
      double precision partar(maxpar)
      double precision phifl(3,6,10,maxelm)
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision rb(maxnfl,maxcofrb)
      double precision rbase(maxcofrb,maxcofrb)
      character*20 region
      double precision resfl(maxnfl)
      double precision resflsav(maxnfl)
      double precision resrb(maxcofrb)
      double precision reynld
      double precision reytay
      double precision senfl(maxnfl,maxcofrb)
      double precision senrb(maxcofrb,maxcofrb)
      double precision splbmp(maxparb+2)
      double precision splflo(maxparf+2)
      double precision taubmp(maxparb+2)
      double precision tauflo(maxparf+2)
      character*30 tecfil
      double precision tolnew
      double precision tolopt
      double precision tolsim
      double precision value
      double precision wateb
      double precision watep
      double precision wateu
      double precision watev
      double precision wquad(3)
      double precision xbl
      double precision xbr
      double precision xc(maxnp)
      double precision xmax
      double precision xmin
      double precision xquad(3,maxelm)
      double precision xprof
      double precision xrange
      double precision xsiq(3)
      double precision ybl
      double precision ybr
      double precision yc(maxnp)
      double precision ymax
      double precision ymin
      double precision yquad(3,maxelm)
      double precision yrange
c
c  Zero out the variables.
c

      do i=1,ldafl
        do j=1,maxnfl
          afl(i,j)=0.0
        enddo 
      enddo

      do i=1,maxcofrb
        do j=1,maxcofrb
          arb(i,j)=0.0
        enddo 
      enddo

      do i=1,3
        do j=1,maxelm
          area(i,j)=0.0
        enddo
      enddo

      command=' '
      cost=0.0
      costb=0.0
      costp=0.0
      costu=0.0
      costv=0.0
 
      do i=1,maxcofrb
        difcof(i)=0.0
      enddo
 
      disfil='display.dat'
      drey=0.01
      epsdif=0.000001
 
      do i=1,maxnfl
        eqn(i)=' '
      enddo
 
      do i=1,3
        etaq(i)=0.0
      enddo

 
      do i=1,maxnfl
        gfl(i)=0.0
      enddo
 
      do i=1,maxnfl
        gflafl(i)=0.0
      enddo


      do i=1,maxnfl
        gflrb(i)=0.0
      enddo
 
      do i=1,maxnfl
        gflsav(i)=0.0
      enddo

      do i=1,maxnfl
        gflsen(i)=0.0
      enddo
 
      do i=1,maxnfl
        gfltar(i)=0.0
      enddo
 
      do i=1,maxnfl
        gfltay(i)=0.0
      enddo
 
      do i=1,maxcofrb
        grb(i)=0.0
      enddo

      do i=1,maxcofrb
        grbarb(i)=0.0
      enddo
 
      do i=1,maxcofrb
        grbsav(i)=0.0
      enddo

      do i=1,maxcofrb
        grbsen(i)=0.0
      enddo
 
      do i=1,maxcofrb
        grbtay(i)=0.0
      enddo

      gridx='uniform'
      gridy='uniform'
      hx=0.0
      hy=0.0
      ibs=0
      ibump=0
 
      do i=1,maxcofrb
        icolrb(i)=0
      enddo
 
      ierror=0
      ifs=0
      ihi=0
      ijac=1
      ilo=0
 
      do i=1,3
        do j=1,maxnp
          indx(i,j)=0
        enddo
      enddo
 
      do i=1,maxpar
        iopt(i)=0
      enddo

      ipar=0
 
      do i=1,maxnfl
        ipivfl(i)=0
      enddo

      do i=1,maxcofrb
        ipivrb(i)=0
      enddo


      do i=1,maxelm
        isotri(i)=0
      enddo

      iwrite=0
      jhi=0
      jlo=0
      maxnew=10
      maxopt=0
      maxsim=10
      nbcrb=0
      ncofrb=0
      nelem=0
      neqnfl=0
      nferb=0
      nlband=0

      do i=1,6
        do j=1,maxelm
          node(i,j)=0
        enddo
      enddo
 
      do i=1,maxnp
        nodelm(i)=0
      enddo
 
      np=0
      npar=1
      nparb=0
      nparf=0
      npe=0

      do i=1,2*maxny-1
        nprof(i)=0
      enddo

      nsenfl=5
      ntay=0
      numnew=0
      numopt=0
      numsim=0
      nx=0
      ny=0
 
      do i=1,maxpar
        par(i)=0.0
      enddo

      do i=1,maxpar
        parafl(i)=0.0
      enddo
 
      do i=1,maxpar
        pararb(i)=0.0
      enddo
 
      do i=1,maxpar
        pardif(i)=0.0
      enddo
 
      do i=1,maxpar
        parrb(i)=0.0
      enddo

      do i=1,maxpar
        parsav(i)=0.0
      enddo

      do i=1,maxpar
        parsen(i)=0.0
      enddo

      do i=1,maxpar
        partar(i)=0.0
      enddo

      do i=1,3
        do j=1,6
          do k=1,10
            do l=1,maxelm
              phifl(i,j,k,l)=0.0
            enddo
          enddo
        enddo
      enddo
 
      do i=1,3
        do j=1,maxcofrb
          do k=1,15
            do l=1,maxelm
              phirb(i,j,k,l)=0.0
            enddo
          enddo
        enddo
      enddo

      do i=1,maxnfl
        do j=1,maxcofrb
          if(i.eq.j)then
            rb(i,j)=1.0
          else
            rb(i,j)=0.0
          endif
        enddo
      enddo

      do i=1,maxcofrb
        do j=1,maxcofrb
          if(i.eq.j)then
            rbase(i,j)=1.0
          else
            rbase(i,j)=0.0
          endif
        enddo
      enddo
 
      region=' '

      do i=1,maxnfl
        resfl(i)=0.0
      enddo

      do i=1,maxnfl
        resflsav(i)=0.0
      enddo

      do i=1,maxcofrb
        resrb(i)=0.0
      enddo

      reynld=1.0
      reytay=1.0

      do i=1,maxnfl
        do j=1,maxcofrb
          if(i.eq.j)then
            senfl(i,j)=1.0
          else
            senfl(i,j)=0.0
          endif
        enddo
      enddo
 
      do i=1,maxcofrb
        do j=1,maxcofrb
          if(i.eq.j)then
            senrb(i,j)=1.0
          else
            senrb(i,j)=0.0
          endif
        enddo
      enddo

      do i=1,maxparb+2
        splbmp(i)=0.0
      enddo

      do i=1,maxparf+2
        splflo(i)=0.0
      enddo


      do i=1,maxparb+2
        taubmp(i)=0.0
      enddo

      do i=1,maxparf+2
        tauflo(i)=0.0
      enddo

      tecfil='tecplot.dat'
      tolnew=0.0
      tolopt=0.0
      tolsim=0.0
      value=0.0

      wateb=0.0
      watep=0.0
      wateu=0.0
      watev=0.0

      do i=1,3
        wquad(i)=0.0
      enddo

      xbl=0.0
      xbr=0.0

      do i=1,maxnp
        xc(i)=0.0
      enddo

      xmax=0.0
      xmin=0.0
      xprof=0.0

      do i=1,3
        do j=1,maxelm
          xquad(i,j)=0.0
        enddo
      enddo

      xrange=0.0

      do i=1,3
        xsiq(i)=0.0
      enddo

      ybl=0.0
      ybr=0.0

      do i=1,maxnp
        yc(i)=0.0
      enddo

      ymax=0.0
      ymin=0.0
 
      do i=1,3
        do j=1,maxelm
          yquad(i,j)=0.0
        enddo
      enddo

      yrange=0.0

      return
      end
      subroutine newtfl(afl,area,eqn,gfl,gflafl,ierror,ifs,ijac,
     &  indx,ipivfl,iwrite,ldafl,maxelm,maxnew,nelem,neqnfl,nlband,
     &  node,np,npar,nparb,nparf,numnew,par,parafl,phifl,
     &  region,resfl,rmax,splflo,tauflo,tolnew,xrange,yrange)
c
c***********************************************************************
c
cc NEWTFL is given an initial estimate of the solution of the full
c  nonlinear state equations, and seeks a better solution.
c
c  The exact solution would have a zero residual, as computed by
c  the routine FXFL.  NEWTFL uses Newton's method to seek a solution
c  whose maximum residual is no more than TOLNEW.  The routine FPFL
c  is used to compute the Jacobian of the residual functions.
c
c
c  NEWTFL was modified on 01 July 1996.
c
c
c  AFL    Workspace, double precision AFL(LDAFL,NEQNFL).
c
c         AFL contains the Jacobian matrix for the full system,
c         stored in LINPACK general band storage mode.
c         The two dimensional array is of logical dimensions LDAFL by 
c         NEQNFL.
c
c  AREA   Input, double precision AREA(3,NELEM).
c
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  EQN    Input, character*2 EQN(NEQNFL).
c         EQN records the "type" of each equation that will be generated, and
c         which is associated with an unknown.  Note that most boundary 
c         conditions do not result in an equation.  The current values are:
c
c         'U'  The horizontal momentum equation.
c         'UB' The condition U=0 applied at a node on the bump.
c         'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c         'UW' The condition U=0 applied at a node on a fixed wall.
c
c         'V'  The vertical momentum equation.
c         'VB' The condition V=0 applied at a node on the bump.
c         'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c         'VW' The condition V=0 applied at a node on a fixed wall.
c
c         'P'  The continuity equation.
c         'PB' The condition P=0 applied at (XMAX,YMAX).
c
c  GFL    Input/output, double precision GFL(NEQNFL), the current solution 
c         estimate for the full problem.
c
c  IERROR Output, integer IERROR, error flag.
c         0, no error occurred.
c         1, an error occurred, and the improved solution could not
c         be computed.
c
c  IFS    Input, integer IFS.
c         1, the inflow is modeled by C0 linear splines.
c         2, the inflow is modeled by C0 quadratic splines.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  IPIVFL Workspace, integer IPIVFL(NEQNFL), pivot vector for the solution
c         of the full linear system.
c 
c  LDAFL  Input, integer LDAFL, the first dimension of the matrix AFL.
c
c  MAXNEW Input, integer MAXNEW, the maximum number of Newton steps
c         that may be taken.  10 should usually be enough.
c
c  MAXNFL Input, integer MAXNFL.
c
c         The maximum number of equations allowed for the full system.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NEQNFL Input, integer NEQNFL, the number of equations in the full system.
c
c  NLBAND Input, integer NLBAND.
c
c         The lower bandwidth of the matrix AFL.  The zero structure of AFL
c         is assumed to be symmetric, and so NLBAND is also the upper
c         bandwidth of AFL.  
c 
c  NODE   Input, integer NODE(6,NELEM).
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
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c 
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c 
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c 
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the 
c         inflow.  NPARF must be at least 1.  
c
c  PAR    Input, double precision PAR(NPAR).
c 
c         PAR is the current estimate for the parameters.
c
c  PHIFL  Input, double precision PHIFL(3,6,10,NELEM).  
c
c         PHIFL contains the value of a finite element basis function, its
c         derivative, or other information, evaluated at the quadrature 
c         points.
c 
c         The meaning of the entry PHIFL(I,J,K,L) is as follows.  
c         For the quadrature point I, and basis function J, in element L,
c         PHIFL(I,J,K,L) represents the value of:
c
c           K= 1, W, the finite element basis function for velocities;
c           K= 2, dWdX, the X derivative of W;
c           K= 3, dWdY, the Y derivative of W;
c           K= 4, Q, the finite element basis function for pressures;
c           K= 5, dQdX, the X derivative of Q;
c           K= 6, dQdY, the Y derivative of Q;
c           K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c           K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c           K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c           K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c         In particular, PHIFL(I,J,K,L) is the value of the quadratic 
c         basis function W associated with local node J in element L, 
c         evaluated at quadrature point I.
c 
c         Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6, 
c         since there are only three linear basis functions.
c
c  REGION Input, character*20 REGION.
c         REGION specifies the flow region.
c
c         'cavity', a driven cavity, 1 unit on each side, open on 
c         the top with a tangential velocity specification there.
c
c         'channel', a channel, 10 units long by 3 high, inflow on 
c         the left, outflow on the right, with a bump on the bottom.
c
c         'step', a channel, 12 units long by 3 high, inflow on the
c         left, outflow on the right, with a step on the bottom.
c
c  RESFL  Workspace, double precision RESFL(NEQNFL), the residual in the
c         full basis equations.
c
c  SPLFLO Input, double precision SPLFLO(NPARF+2).
c
c         SPLFLO contains the spline coefficients for the inflow.
c
c  TAUFLO Input, double precision TAUFLO(NPARF+2).
c
c         TAUFLO contains the location of the spline abscissas for
c         the inflow.  There are NPARF+2 of them, because the end 
c         values of the spline are constrained to have particular 
c         values.
c
c  TOLNEW Input, double precision TOLNEW, the Newton tolerance.
c         NEWTFL is asked to find an approximate solution so that
c         the maximum absolute value of all the residuals is no more
c         than TOLNEW.  A value such as 10E-7 is often reasonable,
c         though this depends on the actual equations being solved.
c
c  XC     Input, double precision XC(NP).
c
c         The X coordinates of the nodes.
c 
c  YC     Input, double precision YC(NP).
c
c         The Y coordinates of the nodes.
c 
      implicit double precision (a-h,o-z)
c
      integer ldafl
      integer maxelm
      integer nelem
      integer neqnfl
      integer np
      integer npar
      integer nparf
c
      double precision afl(ldafl,neqnfl)
      double precision area(3,nelem)
      double precision dmax
      character*2 eqn(neqnfl)
      double precision gfl(neqnfl)
      double precision gflafl(neqnfl)
      integer i
      integer idamax
      integer idmax
      integer ierror
      integer ifs
      integer ijac
      integer indx(3,np)
      integer info
      integer ipivfl(neqnfl)
      integer irmax
      integer iwrite
      integer ixmax
      logical lmat
      integer maxnew
      integer nlband
      integer node(6,nelem)
      integer nparb
      integer numnew
      double precision par(npar)
      double precision parafl(npar)
      double precision phifl(3,6,10,nelem)
      character*20 region
      double precision resfl(neqnfl)
      double precision rmax
      double precision rmax0
      double precision splflo(nparf+2)
      double precision tauflo(nparf+2)
      double precision tolnew
      double precision xmax
      double precision xmax0
      double precision xrange
      double precision yrange
c
c  Force the jacobian matrix to be evaluated on the first iteration.
c
      lmat=.false.
c
c  If the first Newton iteration failed, you may want to try again
c  by coming back here.
c
10    continue
 
      ierror=0
      numnew=0
c
c  Compute the norm of the initial solution estimate.
c
      ixmax=idamax(neqnfl,gfl,1)
      xmax=abs(gfl(ixmax))
      xmax0=xmax
c
c  Evaluate the residual of the initial solution.
c
      call fxfl(area,eqn,gfl,ifs,indx,nelem,neqnfl,node,np,npar,
     &  nparb,nparf,par,phifl,region,resfl,splflo,tauflo,
     &  xrange,yrange)

      irmax=idamax(neqnfl,resfl,1)
      rmax=abs(resfl(irmax))
      rmax0=rmax

      if(iwrite.ge.2)then
        write(*,*)' '
        write(*,*)' Step   MxNorm(X)   IXmax  MxNorm(FX)   IRmax'
        write(*,'(i6,g14.6,i6,g14.6,i6)')numnew,xmax,ixmax,rmax,irmax
      endif
c
c  Begin the Newton iteration.
c
      do numnew=1,maxnew
c
c  If we have a valid, factored jacobian already, then we may
c  reuse it, if it's not too old, and if we're allowed.
c
        if(ijac.gt.1)then
          if(mod(numnew-1,ijac).eq.0)then
            lmat=.false.
          else
            lmat=.true.
          endif
        else
          lmat=.false.
        endif
c
c  If it's time, evaluate and factor the jacobian.
c
        if(.not.lmat)then

          do i=1,npar
            parafl(i)=par(i)
          enddo
 
          do i=1,neqnfl
            gflafl(i)=gfl(i)
          enddo
 
 
          call fpfl(afl,area,eqn,gfl,indx,ldafl,maxelm,nelem,neqnfl,
     &      nlband,node,np,npar,nparb,nparf,par,phifl)

          call dfacfl(afl,ldafl,neqnfl,nlband,nlband,ipivfl,info)

          if(info.ne.0)then
            write(*,*)' '
            write(*,*)'NEWTFL - Fatal error!'
            write(*,*)'  The jacobian is singular.'
            write(*,*)'  DFACFL returns INFO=',info
            ierror=1
            return
          else
            lmat=.true.
          endif
 
        endif
c
c  Solve the linear system A*DX=RES
c
        call dsolfl(afl,ldafl,neqnfl,nlband,nlband,ipivfl,resfl)
 
        idmax=idamax(neqnfl,resfl,1)
        dmax=abs(resfl(idmax))
c
c  Update the estimated solution G.
c
        do i=1,neqnfl
          gfl(i)=gfl(i)-resfl(i)
        enddo
c
c  Compute the norm of the current solution.
c
        ixmax=idamax(neqnfl,gfl,1)
        xmax=abs(gfl(ixmax))
c
c  Evaluate the residual of the current estimated solution.
c
        call fxfl(area,eqn,gfl,ifs,indx,nelem,neqnfl,node,np,npar,
     &    nparb,nparf,par,phifl,region,resfl,splflo,tauflo,
     &    xrange,yrange)
 
        irmax=idamax(neqnfl,resfl,1)
        rmax=abs(resfl(irmax))

        if(iwrite.ge.2)then
          write(*,'(i6,g14.6,i6,g14.6,i6)')numnew,xmax,ixmax,rmax,irmax
        endif
c
c  Accept the iterate if the residual is small enough.
c
        if(rmax.le.tolnew)then
          return
        endif
c
c  Reject the iterate if the residual has grown too large.
c
        if(rmax.gt.10.0*(rmax0+tolnew).and.numnew.gt.1)then
          write(*,*)' '
          write(*,*)'NEWTFL - Warning!'
          write(*,*)'  Residual too big on step ',numnew
          write(*,*)'  MxNorm of first FX=',rmax0
          write(*,*)'  MxNorm of this FX= ',rmax
          go to 20
        endif
 
      enddo
c
c  The iteration has failed to converge, or may actually
c  have been terminated early.
c
20    continue
 
      ierror=1
 
      write(*,*)' '
      write(*,*)'NEWTFL - Warning!'
      write(*,*)'  No Newton convergence after   ',maxnew,' steps.'
      write(*,*)'  MxNorm of last step=',dmax
      write(*,*)'  MxNorm of first X = ',xmax0
      write(*,*)'  MxNorm of last X =  ',xmax
      write(*,*)'  MxNorm of first FX =',rmax0
      write(*,*)'  MxNorm of last FX = ',rmax
      write(*,*)'  Tolerance for FX=   ',tolnew

      return
      end
      subroutine newtrb(arb,area,grb,grbarb,ierror,indx,ipivrb,
     &  iwrite,maxcofrb,maxelm,maxnew,maxnfl,nbcrb,ncofrb,nelem,
     &  nferb,node,np,npar,nparf,nx,ny,par,pararb,phirb,
     &  rb,resrb,rmax,tauflo,tolnew,xc,xrange,yc,yrange)
c
c***********************************************************************
c
cc NEWTRB is given an initial estimate of the solution of the reduced
c  nonlinear state equations, and seeks a better solution.
c
c  The exact solution would have a zero residual, as computed by
c  the routine FXRB.  NEWTRB uses Newton's method to seek a solution
c  whose maximum residual is no more than TOLNEW.  The routine FPRB
c  is used to compute the Jacobian of the residual functions.
c
c
c  NEWTRB was modified on 01 August 1996.
c
c
c  ARB    Workspace, double precision ARB(MAXNRB,MAXNRB).
c
c         ARB contains the Jacobian matrix for the reduced basis system.
c
c  GRB    Input/output, double precision GRB(NCOFRB), the current solution
c         estimate for the reduced basis problem.
c
c  IERROR Output, integer IERROR, error flag.
c         0, no error occurred.
c         1, an error occurred, and the improved solution could not
c         be computed.
c
c  IPIVRB Workspace, integer IPIVRB(NCOFRB), pivot vector for the solution
c         of the reduced linear system.
c 
c  IWRITE Input, integer IWRITE.
c         IWRITE controls the amount of output printed.
c         0 = little, 1=some, 2=a lot.
c
c  MAXNEW Input, integer MAXNEW, the maximum number of Newton steps
c         that may be taken.  10 should usually be enough.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NCOFRB Input, integer NCOFRB, the number of equations in the reduced 
c         system.
c
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c 
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c 
c  PAR    Input, double precision PAR(NPAR).
c 
c         PAR is the current estimate for the parameters.
c
c  PARMAT Output, double precision PARMAT(NPAR).
c         PARMAT contains the parameters where the Jacobian was generated.
c
c  PHIRB  Input, double precision PHIRB(3,NCOFRB,15,NELEM).
c         PHIRB contains the values of a finite element basis function
c         or its X or Y derivative, in a given element, at a given
c         quadrature point, for a particular reduced basis function.
c
c         For PHIRB(I,J,K,L), index J refers to the reduced basis
c         basis functions, for J=0 to NCOFRB.
c
c         The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c           For the quadrature point I, and reduced basis function J,
c           in element L, PHIRB(I,J,K,L) represents the value of:
c
c             K=1, WUrb, the finite element U velocity basis function;
c             K=2, dWUrbdX, the X derivative of WUrb;
c             K=3, dWUrbdY, the Y derivative of WUrb;
c             K=4, WVrb, the finite element V velocity basis function;
c             K=5, dWVrbdX, the X derivative of WVrb;
c             K=6, dWVrbdY, the Y derivative of WVrb;
c             K=7, Q, the finite element pressure basis function.
c             K=8, dQrbdX, the X derivative of Qrb;
c             K=9, dQrbdY, the Y derivative of Qrb.
c             K=10, WU0rb, same as WUrb, with zero BC.
c             K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c             K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c             K=13, WV0rb, same as WVrb, with zero BC.
c             K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c             K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  RESRB  Workspace, double precision RESRB(NCOFRB), the residual in the
c         reduced basis equations, evaluated at the coefficient
c         vector GRB.
c
c  TOLNEW Input, double precision TOLNEW, the Newton tolerance.
c         NEWTRB is asked to find an approximate solution so that
c         the maximum absolute value of all the residuals is no more
c         than TOLNEW.  A value such as 10E-7 is often reasonable,
c         though this depends on the actual equations being solved.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxelm
      integer maxnfl
      integer ncofrb
      integer nelem
      integer np
      integer npar
      integer nparf
c
      double precision arb(maxcofrb,maxcofrb)
      double precision area(3,nelem)
      double precision dmax
      double precision grb(ncofrb)
      double precision grbarb(ncofrb)
      integer i
      integer idamax
      integer idmax
      integer ierror
      integer indx(3,np)
      integer info
      integer ipivrb(ncofrb)
      integer irmax
      integer iwrite
      integer ixmax
      integer maxnew
      integer nbcrb
      integer nferb
      integer node(6,nelem)
      integer numnew
      integer nx
      integer ny
      double precision par(npar)
      double precision pararb(npar)
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision rb(maxnfl,maxcofrb)
      double precision resrb(ncofrb)
      double precision reynld
      double precision rmax
      double precision rmax0
      double precision tauflo(nparf)
      double precision tolnew
      double precision xc(np)
      double precision xmax
      double precision xmax0
      double precision xrange
      double precision yc(np)
      double precision yrange
c
c  If the first Newton iteration failed, you may want to try again
c  by coming back here.
c
10    continue
 
      ierror=0
      numnew=0
c
c  Compute the norm of the initial solution estimate.
c
      ixmax=idamax(ncofrb,grb,1)
      xmax=abs(grb(ixmax))
      xmax0=xmax
c
c  Evaluate the residual of the initial solution.
c
      reynld=par(npar)
 
      call fxrb(area,grb,indx,maxcofrb,maxelm,maxnfl,nbcrb,ncofrb,
     &  nelem,nferb,node,np,npar,nparf,nx,ny,par,phirb,rb,
     &  resrb,reynld,tauflo,xc,xrange,yc,yrange)

      irmax=idamax(ncofrb,resrb,1)
      rmax=abs(resrb(irmax))
      rmax0=rmax

      if(iwrite.ge.2)then
        write(*,*)' '
        write(*,*)' Step   MxNorm(X)   IXmax  MxNorm(FX)   IRmax'
        write(*,'(i6,g14.6,i6,g14.6,i6)')numnew,xmax,ixmax,rmax,irmax
      endif
c
c  Begin the Newton iteration.
c
      do numnew=1,maxnew
c
c  Evaluate the Jacobian.
c
        do i=1,npar
          pararb(i)=par(i)
        enddo
 
        do i=1,ncofrb
          grbarb(i)=grb(i)
        enddo
 
        call fprb(arb,area,grb,indx,maxcofrb,maxelm,maxnfl,nbcrb,
     &    ncofrb,nelem,nferb,node,np,nx,ny,phirb,rb,reynld,xc,xrange,yc,
     &    yrange)
c
c  Factor the Jacobian.
c
        call dfacrb(arb,maxcofrb,ncofrb,ipivrb,info)

        if(info.ne.0)then
          write(*,*)' '
          write(*,*)'NEWTRB - Fatal error!'
          write(*,*)'  The reduced Jacobian is singular.'
          write(*,*)'  DFACRB returns INFO=',info
          ierror=1
          return
        endif
c
c  Solve the linear system A*DX=RES
c
        call dsolrb(arb,maxcofrb,ncofrb,ipivrb,resrb)
 
        idmax=idamax(ncofrb,resrb,1)
        dmax=abs(resrb(idmax))
c
c  Update the estimated solution G.
c
        do i=1,ncofrb
          grb(i)=grb(i)-resrb(i)
        enddo
c
c  Compute the norm of the current solution.
c
        ixmax=idamax(ncofrb,grb,1)
        xmax=abs(grb(ixmax))
c
c  Evaluate the residual of the current estimated solution.
c
        call fxrb(area,grb,indx,maxcofrb,maxelm,maxnfl,nbcrb,ncofrb,
     &    nelem,nferb,node,np,npar,nparf,nx,ny,par,phirb,rb,
     &    resrb,reynld,tauflo,xc,xrange,yc,yrange)
 
        irmax=idamax(ncofrb,resrb,1)
        rmax=abs(resrb(irmax))

        if(iwrite.ge.2)then
          write(*,'(i6,g14.6,i6,g14.6,i6)')numnew,xmax,ixmax,rmax,irmax
        endif
c
c  Accept the iterate if the residual is small enough.
c
        if(rmax.le.tolnew)then
          return
        endif
c
c  Reject the iterate if the residual has grown too large.
c
        if(rmax.gt.10.0*(rmax0+tolnew).and.numnew.gt.1)then
          write(*,*)' '
          write(*,*)'NEWTRB - Warning!'
          write(*,*)'  Residual too big on step ',numnew
          write(*,*)'  MxNorm of first FX=',rmax0
          write(*,*)'  MxNorm of this FX= ',rmax
          go to 20
        endif
 
      enddo
c
c  The iteration has failed to converge, or may actually
c  have been terminated early.
c
20    continue
 
      ierror=1
 
      write(*,*)' '
      write(*,*)'NEWTRB - Warning!'
      write(*,*)'  No Newton convergence after   ',numnew,' steps.'
      write(*,*)' '
      write(*,*)'  MxNorm of first X = ',xmax0
      write(*,*)'  MxNorm of first FX =',rmax0
      write(*,*)' '
      write(*,*)'  MxNorm of last step=',dmax
      write(*,*)'  MxNorm of last X =  ',xmax
      write(*,*)'  MxNorm of last FX = ',rmax

      return
      end
      subroutine optdiffl(afl,area,cost,dopt,eqn,etaq,gfl,gflafl,
     &  gflopt,gfltar,gridx,gridy,ibs,ierror,ifs,ijac,indx,iopt,
     &  ipivfl,isotri,ivopt,iwrite,ldafl,liv,lv,maxelm,maxnew,maxnfl,
     &  maxnp,maxny,maxopt,maxpar,maxparb,maxparf,maxsim,nelem,
     &  neqnfl,nlband,node,nodelm,np,npar,nparb,nparf,nprof,numdif,
     &  numopt,nx,ny,par,parafl,paropt,phifl,region,resfl,splbmp,
     &  splflo,taubmp,tauflo,tolnew,tolopt,tolsim,vopt,wateb,watep,
     &  wateu,watev,wquad,xbl,xbr,xc,xopt,xquad,xrange,xsiq,ybl,
     &  ybr,yc,yquad,yrange)
c 
c***********************************************************************
c
cc OPTDIFFL optimizes the full problem, without gradient information.
c
c  That is, OPTDIFFL searches for a set of parameters PAROPT,
c  and the corresponding flow solution GFLOPT, which minimize
c  the cost function COST.
c
c  The ACM TOMS 611 routine SNOIT is used, which does not require
c  direct information about the gradient of COST with respect to
c  the parameters PAROPT.  Instead, it estimates this information
c  indirectly, via finite differences.
c
c
c  OPTDIFFL was modified on 01 July 1996.
c
c
c  AFL    Output, double precision AFL(LDAFL,MAXNFL).
c         If Newton iteration is being carried out, AFL contains the
c         Jacobian matrix for the full system.
c         If Picard iteration is being carried out, AFL contains the
c         Picard matrix for the full system.
c
c         AFL is stored in LINPACK general band storage mode, with
c         logical dimensions (3*NBANDL+1, NEQNFL).
c
c  AREA   Input, double precision AREA(3,MAXELM).
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  COST   Output, double precision COST.
c         COST contains the current value of the cost function.  This
c         is the function which the optimizer is to minimize.
c
c         COST = WATEP*COSTP + WATEB*COSTB + WATEU*COSTU + WATEV*COSTV
c
c  DOPT   Workspace, double precision DOPT(MAXPAR).
c         DOPT contains scaling factors used during an optimization.
c         These scaling factors are intended to adjust problems
c         in which some variables are typically very much smaller
c         or larger than others.
c
c  EQN    Input, character*2 EQN(MAXNFL).
c         EQN records the "type" of each equation that will be generated, and
c         which is associated with an unknown.  Note that most boundary
c         conditions do not result in an equation.  The current values are:
c
c         'U'  The horizontal momentum equation.
c         'UB' The condition U=0 applied at a node on the bump.
c         'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c         'UW' The condition U=0 applied at a node on a fixed wall.
c         'U0' A dummy value of U=0 should be set.
c
c         'V'  The vertical momentum equation.
c         'VB' The condition V=0 applied at a node on the bump.
c         'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c         'VW' The condition V=0 applied at a node on a fixed wall.
c         'V0' A dummy value of V=0 should be set.
c
c         'P'  The continuity equation.
c         'PB' The condition P=0 applied at (XMAX,YMAX).
c         'P0' A dummy value of P=0 should be set.
c
c  ETAQ   Input, double precision ETAQ(3).
c         ETAQ contains the "Eta" coordinates of the quadrature points.
c
c  GFL    Input, double precision GFL(NEQNFL).
c         GFL contains the current solution estimate for the full problem,
c         containing the pressure and velocity coefficients.
c         The vector INDX must be used to index this data.
c
c  GFLAFL Output, double precision GFLAFL(NEQNFL).
c         GFLAFL stores the value of GFL at which the Jacobian
c         was generated.
c
c  GFLOPT Output, double precision GFLOPT(NEQNFL).
c         GFLOPT stores the value of a full solution which is being
c         optimized.
c
c  GFLTAR Input, double precision GFLTAR(NEQNFL).
c         GFLTAR is a target solution, used to generate data that defines
c         the cost functional.  The corresponding parameters are PARTAR.
c
c  IBS    Input, integer IBS.
c         IBS is the bump shape option.
c         0, piecewise constant function.
c         1, piecewise linear function.
c         2, piecewise quadratic function.
c
c  IERROR Output, integer IERROR.
c         0, the optimization was successful.
c         1, the optimization failed.
c
c  INDX   Input, integer INDX(3,NP).
c         INDX(I,J) contains, for each node J, the global index of U,
c         V and P at that node, or 0 or a negative value.  The global
c         index of U, V, or P is the index of the coefficient vector
c         that contains the value of the finite element coefficient
c         associated with the corresponding basis function at the
c         given node.
c
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  IOPT   Workspace, integer IOPT(MAXPAR).
c         IOPT is used during an optimization.  For each parameter I,
c         the meaning of IOPT(I) is:
c         0, the parameter value must remain fixed;
c         1, the parameter value may be varied.
c
c  IPIVFL Workspace, integer IPIVFL(NEQNFL).
c         IPIVFL is a pivot vector for the solution of the full
c         linear system.
c
c  ISOTRI Input, integer ISOTRI(NELEM).
c         0, the element is NOT isoparametric, and the nodes never move.
c         That means that the quadrature points are only computed once.
c
c         1, the element is NOT isoparametric, but the nodes may move.
c         Quadrature point locations must be updated on each step.
c         This could occur for elements above, but not touching, the bump.
c
c         2, the element is isoparametric.
c
c  IVOPT  Workspace, integer IVOPT(LIV).
c         IVOPT provides integer workspace for several of the
c         optimization routines.
c
c  IWRITE Input, integer IWRITE.
c         IWRITE controls the amount of output printed.
c         0, print out the least amount.
c         1, print out some.
c         2, print out a lot.
c
c  LDAFL  Input, integer LDAFL.
c         LDAFL is the first dimension of the matrix AFL as declared in
c         the main program.  LDAFL must be at least 3*NLBAND+1.
c
c  LIV    Input, integer LIV.
c         LIV is the dimension of the work vector IVOPT, used by
c         the ACM TOMS 611 optimization package.  LIV is always 60.
c
c  LV     Input, integer LV.
c         LV is the dimension of the work vector VOPT, used by
c         the ACM TOMS 611 optimization package.  
c
c  MAXELM Input, integer MAXELM.
c         MAXELM is the maximum number of elements.
c
c  MAXNEW Input, integer MAXNEW.
c         MAXNEW is the maximum number of steps to take in one Newton
c         iteration.  A typical value is 20.
c
c  MAXNFL Input, integer MAXNFL.
c         MAXNFL is the maximum number of equations or coefficients allowed
c         for the full system.  MAXNFL must be used instead of NEQNFL as
c         the leading dimension of certain multi-dimensional arrays.
c
c  MAXNP  Input, integer MAXNP.
c         MAXNP is the maximum number of nodes allowed in the program.
c
c  MAXNY  Input, integer MAXNY.
c         MAXNY is the maximum size of NY that the program can handle.
c
c  MAXOPT Input, integer MAXOPT.
c         MAXOPT is the maximum number of optimization steps.
c
c  MAXPAR Input, integer MAXPAR.
c         MAXPAR is the maximum number of parameters allowed.
c         MAXPAR = MAXPARF + MAXPARB + 1.
c
c  MAXPARB
c         Input, integer MAXPARB.
c         MAXPARB is the maximum number of bump parameters allowed.
c
c  MAXPARF
c         Input, integer MAXPARF.
c         MAXPARF is the maximum number of inflow parameters allowed.
c
c  MAXSIM Input, integer MAXSIM.
c         MAXSIM is the maximum number of steps to take in one Picard
c         iteration.  A typical value is 20.
c
c  NELEM  Input, integer NELEM.
c         NELEM is the number of elements.
c         NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NEQNFL Input, integer NEQNFL.
c         NEQNFL is the number of equations (and coefficients) in the full
c         finite element system.
c
c  NLBAND Input, integer NLBAND.
c         NLBAND is the lower bandwidth of the matrix AFL.
c         The zero structure of AFL is assumed to be symmetric, and so
c         NLBAND is also the upper bandwidth of AFL.
c
c  NODE   Input, integer NODE(6,MAXELM) or NODE(6,NELEM).
c         NODE(I,J) contains, for an element J, the global index of
c         the node whose local number in J is I.
c
c         The local ordering of the nodes is suggested by this diagram:
c
c           Global nodes   Elements      NODE
c                                                          1  2  3  4  5  6
c           74  84  94     3-6-1   2     Left element =  (94,72,74,83,73,84)
c                          |  /   /|
c           73  83  93     5 4   4 5     Right element = (72,94,92,83,93,82)
c                          |/   /  |
c           72  82  92     2   1-6-3
c
c  NP     Input, integer NP.
c         NP is the number of nodes used to define the finite element mesh.
c         Typically, the mesh is generated as a rectangular array, with
c         an odd number of nodes in the horizontal and vertical directions.
c         The formula for NP is NP=(2*NX-1)*(2*NY-1).
c
c  NPAR   Input, integer NPAR.
c         NPAR is the number of parameters.
c
c         NPAR = NPARF + NPARB + 1.
c
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c
c  NPARB  Input, integer NPARB.
c         NPARB is the number of parameters associated with the position and
c         shape of the bump.
c
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c
c  NPARF  Input, integer NPARF.
c         NPARF is the number of parameters associated with the
c         inflow.  NPARF must be at least 1.
c
c  NPROF  Input, integer NPROF(2*MAXNY-1).
c         NPROF contains the numbers of the nodes along the profile
c         line.
c
c  NX     Input, integer NX.
c         NX controls the spacing of nodes and elements in
c         the X direction.  There are 2*NX-1 nodes along various
c         lines in the X direction.
c
c         Roughly speaking, NX (or 2*NX) is the number of elements along
c         a line in the X direction.
c
c  NY     Input, integer NY.
c         NY controls the spacing of nodes and elements in
c         the Y direction.  There are 2*NY-1 nodes along various
c         lines in the Y direction.
c
c         Roughly speaking, NY (or 2*NY) is the number of elements along
c         a line in the Y direction.
c
c  PAR    Input, double precision PAR(NPAR).
c         PAR is the current estimate for the parameters.
c
c         PAR(1:NPARF)             = inflow controls.
c
c         PAR(NPARF+1:NPARF+NPARB) = bump controls.
c
c         PAR(NPARF+NPARB+1)       = the REYNLD parameter.
c
c  PARAFL Output, double precision PARAFL(NPAR).
c         PARAFL contains the parameters where the Picard matrix or
c         Jacobian of the full system was generated.
c
c  PAROPT Output, double precision PAROPT(NPAR).
c         PAROPT contains the estimate for the optimizing parameter
c         values which minimize the cost.
c
c  PHIFL  Input, double precision PHIFL(3,6,10,NELEM).
c         PHIFL contains the value of a finite element basis function, its
c         derivative, or other information, evaluated at the quadrature
c         points.
c
c         The meaning of the entry PHIFL(I,J,K,L) is as follows.
c         For the quadrature point I, and basis function J, in element L,
c         PHIFL(I,J,K,L) represents the value of:
c
c           K= 1, W, the finite element basis function for velocities;
c           K= 2, dWdX, the X derivative of W;
c           K= 3, dWdY, the Y derivative of W;
c           K= 4, Q, the finite element basis function for pressures;
c           K= 5, dQdX, the X derivative of Q;
c           K= 6, dQdY, the Y derivative of Q;
c           K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c           K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c           K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c           K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c         In particular, PHIFL(I,J,K,L) is the value of the quadratic
c         basis function W associated with local node J in element L,
c         evaluated at quadrature point I.
c
c         Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6,
c         since there are only three linear basis functions.
c
c  REGION Input, character*20 REGION.
c         REGION specifies the flow region.
c
c         'cavity', a driven cavity, 1 unit on each side, open on
c         the top with a tangential velocity specification there.
c
c         'channel', a channel, 10 units long by 3 high, inflow on
c         the left, outflow on the right, with a bump on the bottom.
c
c         'step', a channel, 12 units long by 3 high, inflow on the
c         left, outflow on the right, with a step on the bottom.
c
c  RESFL  Workspace, double precision RESFL(NEQNFL).
c         RESFL contains the residual in the full basis equations.
c
c  SPLBMP Input, double precision SPLBMP(NPARB+2).
c         SPLBMP contains the spline coefficients for the bump.
c
c  SPLFLO Input, double precision SPLFLO(NPARF+2).
c         SPLFLO contains the spline coefficients for the inflow.
c
c  TAUBMP Input, double precision TAUBMP(NPARB+2).
c         TAUBMP contains the location of the spline abscissas for
c         the bump.  There are NPARB+2 of them, because the end values
c         of the spline are constrained to have particular values.
c
c  TAUFLO Input, double precision TAUFLO(NPARF+2).
c         TAUFLO contains the location of the spline abscissas for
c         the inflow.  There are NPARF+2 of them, because the end
c         values of the spline are constrained to have particular
c         values.
c
c  TOLNEW Input, double precision TOLNEW.
c         TOLNEW is the convergence tolerance for the Newton
c         iteration.
c
c  TOLOPT Input, double precision TOLOPT.
c         TOLOPT is the convergence tolerance for the optimization.
c
c         If TOLOPT is zero, then default values are used.
c
c  TOLSIM Input, double precision TOLSIM.
c         TOLSIM is the convergence tolerance for the Picard
c         iteration.
c
c  VOPT   Workspace, double precision VOPT(LV).
c         VOPT provides real workspace for the optimization routines.
c
c  WATEB  Input, double precision WATEB.
c         WATEB is the multiplier of the bump control cost used
c         when computing the total cost.
c
c  WATEP,
c  WATEU,
c  WATEV  Input, double precision WATEP, WATEU, WATEV.
c
c         WATEP, WATEU and WATEV are weights used in computing the
c         cost function based on the costs of the flow discrepancy.
c
c  WQUAD  Input, double precision WQUAD(3).
c         WQUAD contains the weights for Gaussian quadrature.
c
c  XBL    Input, double precision XBL.
c         XBL is the X coordinate of the left corner of the bump.
c
c  XBR    Input, double precision XBR.
c         XBR is the X coordinate of the right corner of the bump.
c
c  XC     Input, double precision XC(NP).
c         XC contains the X coordinates of the nodes.
c
c  XOPT   Workspace, double precision XOPT(MAXPAR).
c         XOPT is used by the optimization routines to hold only
c         the values of parameters which are allowed to vary.
c
c  XQUAD  Input, double precision XQUAD(3,NELEM).
c         The X coordinates of the quadrature points for
c         each element.
c
c  XRANGE Input, double precision XRANGE.
c         The total width of the region.
c
c  XSIQ   Input, double precision XSIQ(3).
c         The "Xsi" coordinates of the quadrature points.
c
c  YBL    Input, double precision YBL.
c         The Y coordinate of the left corner of the bump.
c
c  YBR    Input, double precision YBR.
c         YBR is the Y coordinate of the right corner of the bump.
c
c  YC     Input, double precision YC(NP).
c         YC contains the Y coordinates of the nodes.
c
c  YQUAD  Input, double precision YQUAD(3,NELEM).
c         The Y coordinates of the quadrature points for
c         each element.
c
c  YRANGE Input, double precision YRANGE.
c         YRANGE is the total height of the region.
c
      implicit double precision (a-h,o-z)
c
c  Set parameters that are independent.
c
      integer ldafl
      integer liv
      integer lv
      integer maxelm    
      integer maxnfl   
      integer maxnp               
      integer maxny
      integer maxpar      
      integer maxparb
      integer maxparf
c
      double precision afl(ldafl,maxnfl)
      double precision area(3,maxelm)
      double precision cost
      double precision costb
      double precision costp
      double precision costu
      double precision costv
      double precision dopt(maxpar)
      character*2 eqn(maxnfl)
      double precision etaq(3)
      double precision gfl(maxnfl)
      double precision gflafl(maxnfl)
      double precision gflopt(maxnfl)
      double precision gfltar(maxnfl)
      character*20 gridx
      character*20 gridy
      integer i
      integer ibs
      integer ierror
      integer ifs
      integer ijac
      integer indx(3,maxnp)
      integer iopt(maxpar)
      integer ipivfl(maxnfl)
      integer isotri(maxelm)
      integer ival
      integer ivopt(liv)
      integer iwrite
      integer maxnew
      integer maxopt
      integer maxsim
      integer nelem
      integer neqnfl
      integer nlband
      integer node(6,maxelm)
      integer nodelm(np)
      integer nopt
      integer np
      integer npar
      integer nparb
      integer nparf
      integer nprof(2*maxny-1)
      integer numdif
      integer numnew
      integer numopt
      integer numsim
      integer nx
      integer ny
      double precision par(maxpar)
      double precision parafl(maxpar)
      double precision paropt(maxpar)
      double precision phifl(3,6,10,maxelm)
      character*20 region
      double precision resfl(maxnfl)
      double precision rmax
      double precision splbmp(maxparb+2)
      double precision splflo(maxparf+2)
      double precision taubmp(maxparb+2)
      double precision tauflo(maxparf+2)
      double precision tolnew
      double precision tolopt
      double precision tolsim
      double precision vopt(lv)
      double precision wateb
      double precision watep
      double precision wateu
      double precision watev
      double precision wquad(3)
      double precision xbl
      double precision xbr
      double precision xc(maxnp)
      double precision xopt(maxpar)
      double precision xquad(3,maxelm)
      double precision xrange
      double precision xsiq(3)
      double precision ybl
      double precision ybr
      double precision yc(maxnp)
      double precision yquad(3,maxelm)
      double precision yrange
c
      ierror=0
c
c  Copy the initial solution estimate.
c
      do i=1,npar
        paropt(i)=par(i)
      enddo
 
      do i=1,neqnfl
        gflopt(i)=gfl(i)
      enddo
c
c  Initialize the local optimization data.
c
      cost=0.0

      do i=1,npar
        dopt(i)=1.0
      enddo
      
      do i=1,liv
        ivopt(i)=0
      enddo
      
      nopt=0
      
      do i=1,lv
        vopt(i)=0.0
      enddo
      
      do i=1,maxpar
        xopt(i)=0.0
      enddo
c
c  Set the 611 data to default values,
c  and then modify some values.
c
      ival=2
      call deflt(ival,ivopt,liv,lv,vopt)

      vopt(31)=tolopt
      vopt(32)=tolopt
      vopt(33)=tolopt
      vopt(34)=tolopt
      vopt(37)=tolopt

      ivopt(1)=12
      ivopt(19)=0
c
c  Set the step counters.
c
      numdif=0
      numopt=0
c
c  Take the next optimization step.
c
10    continue

      if(numopt.gt.maxopt)then
        write(*,*)' '
        write(*,*)'OPTDIFFL - Warning!'
        write(*,*)'  The number of optimization steps was exceeded.'
        return
      endif
c
c  Make a "copy" of PAR that only contains the free variables.
c
      nopt=0
      do i=1,npar
        if(iopt(i).eq.1)then
          nopt=nopt+1
          xopt(nopt)=paropt(i)
        endif
      enddo
c
c  Call the optimizer to get a new parameter estimate.
c
      call snoit(dopt,cost,ivopt,liv,lv,nopt,vopt,xopt)
c
c  Copy the new free variable values back into PAR.
c
      nopt=0
      do i=1,npar
        if(iopt(i).eq.1)then
          nopt=nopt+1
          paropt(i)=xopt(nopt)
        endif
      enddo
c
c  For the given values of PAROPT, set up the flow problem.
c  We are only varying the REYNLD parameter, and no geometric
c  quantities vary with REYNLD, so we only have to make this
c  call once.
c
      if(numopt.eq.0)then
        call setgeo(area,etaq,gridx,gridy,ibs,isotri,nelem,node,
     &    nodelm,np,npar,nparb,nparf,nx,ny,paropt,phifl,region,
     &    splbmp,taubmp,wquad,xbl,xbr,xc,xquad,xrange,
     &    xsiq,ybl,ybr,yc,yquad,yrange)
      endif
c
c  Apply Picard's method to the approximate solution GFLOPT.
c
        call picfl(afl,area,eqn,gflopt,ierror,ifs,indx,ipivfl,iwrite,
     &    ldafl,maxsim,nelem,neqnfl,nlband,node,np,npar,nparb,nparf,
     &    numsim,paropt,phifl,region,resfl,rmax,splflo,tauflo,
     &    tolsim,xc,xrange,yc,yrange)
c
c  Apply Newton's method to the approximate solution GFLOPT.
c
        if(rmax.le.tolnew)then
          write(*,*)'OPTDIFFL - Picard iterate skips Newton.'
        else
          call newtfl(afl,area,eqn,gflopt,gflafl,ierror,ifs,ijac,
     &      indx,ipivfl,iwrite,ldafl,maxelm,maxnew,nelem,neqnfl,nlband,
     &      node,np,npar,nparb,nparf,numnew,paropt,parafl,phifl,
     &      region,resfl,rmax,splflo,tauflo,tolnew,xrange,yrange)

          if(ierror.ne.0)then
            write(*,*)' '
            write(*,*)'OPTDIFFL - Fatal error!'
            write(*,*)'  NEWTFL failed!'
            write(*,*)'  The parameters at which failure occurred:'
            write(*,*)' '
            call prpar(iopt,npar,nparb,nparf,paropt)
            ierror=1
            return
          endif
        endif
c
c  Compute the cost function COST.
c
      call getcst(cost,costb,costp,costu,costv,gflopt,gfltar,
     &  indx,neqnfl,np,nparb,nprof,ny,splbmp,
     &  taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)
 
      if(ivopt(1).eq.1.and.iwrite.ge.0)then
        call prpar(iopt,npar,nparb,nparf,paropt)
        write(*,*)'  Cost=',cost
      endif
c
c  If IVOPT(1) is 1, then this was a call for a legitimate
c  solution candidate.
c
c  If IVOPT(1) is 2, then this was a call for a temporary
c  solution used only for estimating the gradient.
c
c  Other values of IVOPT call for acceptance or rejection.
c
      if(ivopt(1).eq.1)then
        numopt=numopt+1
      elseif(ivopt(1).eq.2)then
        numdif=numdif+1
      elseif(ivopt(1).ge.3.and.ivopt(1).le.8)then
        write(*,*)' '
        write(*,*)'  Convergence to a minimizer was achieved!'
        return
      elseif(ivopt(1).gt.8)then
        write(*,*)' '
        write(*,*)'  Bad value of IVOPT(1)=',ivopt(1)
        return
      endif
 
      go to 10

      end
      subroutine optdifrb(arb,area,cost,dopt,gfltar,gfltmp,
     &  grb,grbarb,grbopt,ierror,indx,iopt,ipivrb,
     &  ivopt,iwrite,liv,lv,maxcofrb,maxelm,maxnew,maxnfl,maxnp,
     &  maxny,maxopt,maxpar,maxparb,maxsim,nbcrb,ncofrb,nelem,neqnfl,
     &  nferb,node,np,npar,nparb,nparf,nprof,numdif,numopt,nx,ny,par,
     &  pararb,paropt,phirb,rb,resrb,splbmp,tauflo,taubmp,tolnew,
     &  tolopt,tolsim,vopt,wateb,watep,wateu,watev,xbl,xbr,xc,xopt,
     &  xrange,ybl,ybr,yc,yrange)
c 
c***********************************************************************
c
cc OPTDIFRB optimizes the reduced problem, without gradient information.
c
c  That is, OPTDIFRB searches for a set of parameters PAROPT,
c  and the corresponding flow solution GRBOPT, which minimize
c  the cost function COST.
c
c  The ACM TOMS 611 routine SNOIT is used, which does not require
c  direct information about the gradient of COST with respect to
c  the parameters PAROPT.  Instead, it estimates this information
c  indirectly, via finite differences.
c
c
c  OPTDIFRB was modified on 01 July 1996.
c
c
c  ARB    Workspace, double precision ARB(MAXNRB,MAXNRB).
c         ARB contains the Jacobian or Picard matrix for the reduced
c         Navier Stokes system, stored as a dense NCOFRB by NCOFRB array.
c
c  AREA   double precision AREA(3,MAXELM).
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  COST   double precision COST.
c         COST contains the current value of the cost function.  This
c         is the function which the optimizer is to minimize.
c
c         COST = WATEP*COSTP + WATEB*COSTB + WATEU*COSTU + WATEV*COSTV
c
c  DOPT   double precision DOPT(MAXPAR).
c         DOPT contains scaling factors used during an optimization.
c         These scaling factors are intended to adjust problems
c         in which some variables are typically very much smaller
c         or larger than others.
c
c  GFLRB  double precision GFLRB(NEQNFL).
c         GFLRB is the solution value at which the reduced basis was computed.
c         The corresponding parameters are PARRB.
c
c  GFLTAR double precision GFLTAR(NEQNFL).
c         GFLTAR is a target solution, used to generate data that defines
c         the cost functional.  The corresponding parameters are PARTAR.
c
c  GFLTMP Workspace, double precision GFLTMP(NEQNFL).
c
c  GRB    double precision GRB(NCOFRB).
c         GRB contains the reduced basis coefficients of the current
c         estimate of the state solution.
c
c  GRBARB double precision GRBARB(NCOFRB).
c         GRBARB contains the reduced basis coefficients at which
c         the matrix ARB was last evaluated.
c
c  GRBOPT double precision GRBOPT(NCOFRB).
c         GRBOPT stores the value of a reduced solution which is being
c         optimized.
c
c  GRBTMP Workspace, double precision GRBTMP(NCOFRB).
c
c  IERROR integer IERROR.
c         IERROR is an error flag.
c         0, no error occurred in this routine.
c         nonzero, an error occurred.
c
c  INDX   integer INDX(3,NP).
c         INDX(I,J) contains, for each node J, the global index of U,
c         V and P at that node, or 0 or a negative value.  The global
c         index of U, V, or P is the index of the coefficient vector
c         that contains the value of the finite element coefficient
c         associated with the corresponding basis function at the
c         given node.
c
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  IOPT   integer IOPT(MAXPAR).
c         IOPT is used during an optimization.  For each parameter I,
c         the meaning of IOPT(I) is:
c         0, the parameter value must remain fixed;
c         1, the parameter value may be varied.
c
c  IPIVRB Workspace, integer IPIVRB(NCOFRB).
c         IPIVRB is a pivot vector for the solution of the reduced
c         linear system.
c
c  IVOPT  integer IVOPT(LIV).
c         IVOPT provides integer workspace for several of the
c         optimization routines.
c
c  IWRITE integer IWRITE.
c         IWRITE controls the amount of output printed.
c         0, print out the least amount.
c         1, print out some.
c         2, print out a lot.
c
c  LIV    integer LIV.
c         LIV is the dimension of the work vector IVOPT, used by
c         the ACM TOMS 611 optimization package.  LIV is always 60.
c
c  LV     integer LV.
c         LV is the dimension of the work vector VOPT, used by
c         the ACM TOMS 611 optimization package.  
c
c  MAXELM integer MAXELM.
c         MAXELM is the maximum number of elements.
c
c  MAXNEW integer MAXNEW.
c         MAXNEW is the maximum number of steps to take in one Newton
c         iteration.  A typical value is 20.
c
c  MAXNFL integer MAXNFL.
c         MAXNFL is the maximum number of equations or coefficients allowed
c         for the full system.  MAXNFL must be used instead of NEQNFL as
c         the leading dimension of certain multi-dimensional arrays.
c
c  MAXNP  integer MAXNP.
c         MAXNP is the maximum number of nodes allowed in the program.
c
c  MAXNRB integer MAXNRB.
c         The maximum number of equations allowed for the reduced basis system.
c
c  MAXNY  integer MAXNY.
c         MAXNY is the maximum size of NY that the program can handle.
c
c  MAXOPT integer MAXOPT.
c         MAXOPT is the maximum number of optimization steps.
c
c  MAXPAR integer MAXPAR.
c         MAXPAR is the maximum number of parameters allowed.
c         MAXPAR = MAXPARF + MAXPARB + 1.
c
c  MAXPARB
c         integer MAXPARB.
c         MAXPARB is the maximum number of bump parameters allowed.
c
c  MAXSIM integer MAXSIM.
c         MAXSIM is the maximum number of steps to take in one Picard
c         iteration.  A typical value is 20.
c
c  NELEM  integer NELEM.
c         NELEM is the number of elements.
c         NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NEQNFL integer NEQNFL.
c         NEQNFL is the number of equations (and coefficients) in the full
c         finite element system.
c
c  NCOFRB integer NCOFRB.
c         NCOFRB is the number of basis functions, reduced state equations and
c         coefficients in the reduced basis system.
c
c  NP     integer NP.
c         NP is the number of nodes used to define the finite element mesh.
c         Typically, the mesh is generated as a rectangular array, with
c         an odd number of nodes in the horizontal and vertical directions.
c         The formula for NP is NP=(2*NX-1)*(2*NY-1).
c
c  NPAR   integer NPAR.
c         NPAR is the number of parameters.
c
c         NPAR = NPARF + NPARB + 1.
c
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c
c  NPARB  integer NPARB.
c         NPARB is the number of parameters associated with the position and
c         shape of the bump.
c
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c
c  NPARF  integer NPARF.
c         NPARF is the number of parameters associated with the
c         inflow.  NPARF must be at least 1.
c
c  NPROF  integer NPROF(2*MAXNY-1).
c         NPROF contains the numbers of the nodes along the profile
c         line.
c
c  NUMDIF integer NUMDIF.
c         NUMDIF is the number of flow solutions generated strictly for 
c         finite difference calculations.
c
c  NUMOPT integer NUMOPT.
c         NUMOPT is the number of flow solutions calculated during
c         an optimization which were actual candidate minimizers.
c
c  NY     integer NY.
c         NY controls the spacing of nodes and elements in
c         the Y direction.  There are 2*NY-1 nodes along various
c         lines in the Y direction.
c
c         Roughly speaking, NY (or 2*NY) is the number of elements along
c         a line in the Y direction.
c
c  PAR    double precision PAR(NPAR).
c         PAR is the current estimate for the parameters.
c
c         PAR(1:NPARF)             = inflow controls.
c
c         PAR(NPARF+1:NPARF+NPARB) = bump controls.
c
c         PAR(NPARF+NPARB+1)       = the REYNLD parameter.
c
c  PARARB double precision PARARB(NPAR).
c         PARARB contains the parameters where the Picard matrix or
c         Jacobian of the reduced system was generated.
c
c  PAROPT double precision PAROPT(NPAR).
c         PAROPT contains the estimate for the optimizing parameter
c         values which minimize the cost.
c
c  PHIRB  Input, double precision PHIRB(3,NCOFRB,15,NELEM).
c         PHIRB contains the values of a finite element basis function
c         or its X or Y derivative, in a given element, at a given
c         quadrature point, for a particular reduced basis function.
c
c         For PHIRB(I,J,K,L), index J refers to the reduced basis
c         basis functions, for J=0 to NCOFRB.
c
c         The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c           For the quadrature point I, and reduced basis function J,
c           in element L, PHIRB(I,J,K,L) represents the value of:
c
c             K=1, WUrb, the finite element U velocity basis function;
c             K=2, dWUrbdX, the X derivative of WUrb;
c             K=3, dWUrbdY, the Y derivative of WUrb;
c             K=4, WVrb, the finite element V velocity basis function;
c             K=5, dWVrbdX, the X derivative of WVrb;
c             K=6, dWVrbdY, the Y derivative of WVrb;
c             K=7, Q, the finite element pressure basis function.
c             K=8, dQrbdX, the X derivative of Qrb;
c             K=9, dQrbdY, the Y derivative of Qrb.
c             K=10, WU0rb, same as WUrb, with zero BC.
c             K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c             K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c             K=13, WV0rb, same as WVrb, with zero BC.
c             K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c             K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  RB     double precision RB(MAXNFL,NCOFRB).
c         RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
c         RB is generated by computing a finite element solution GFL.
c         A copy of this solution will be saved and called "GFLRB".
c         Then, we compute the first NCOFRB derivatives of GFLRB with
c         respect to a parameter (for us, REYNLD).  The first derivative
c         is stored in column 1 of RB, and so on.  Then we orthogonalize
c         the columns of RB.
c
c         We intend that NEQNFL >> NCOFRB, and RB is a matrix with orthogonal
c         columns, so that:
c
c           Transpose(RB) * RB = Identity(NCOFRB)
c
c
c         If GFL is any set of finite element coefficients, the corresponding
c         set of reduced basis coefficients can be computed as:
c
c           GRB = Transpose(RB) * (GFL-GFLRB)
c
c         If GRB is a set of reduced basis coefficients, a corresponding
c         set of finite element coefficients can be computed as:
c
c           GFL = GFLRB + RB * GRB.
c
c         While it is the case that you can expand and then reduce,
c         and always get the same result, it is not the case that
c         when you reduce and then expand you get the same result!
c
c         It is true, for ANY GRB, that
c
c           GRB = Transpose(RB) * RB * GRB
c
c         which follows from Transpose(RB) * RB = Identity(NCOFRB).
c
c         However, for a general GFL, it is the case that
c
c           GFL =/= GFLRB + RB * Transpose(RB) * (GFL-GFLRB).
c
c         Only if GFL was generated from a reduced basis coefficient
c         vector will equality apply.  In other words, if GFL was generated
c         from a reduced basis coefficient:
c
c           GFL = GFLRB + RB * GRB
c
c         then
c
c           GFLRB + RB * Transpose(RB) * (GFL-GFLRB)
c           = GFLRB + RB * Transpose(RB) * (RB * GRB)
c           = GFLRB + RB *                       GRB
c           = GFL
c
c         so in this strictly limited case,
c
c           RB * Transpose(RB) = Identity(NEQNFL).
c
c  RESRB  double precision RESRB(NCOFRB).
c         RESRB contains the residual in the reduced basis equations,
c         for the parameter values PAR and reduced basis coefficients GRB.
c
c  SPLBMP double precision SPLBMP(NPARB+2).
c         SPLBMP contains the spline coefficients for the bump.
c
c  TAUBMP double precision TAUBMP(NPARB+2).
c         TAUBMP contains the location of the spline abscissas for
c         the bump.  There are NPARB+2 of them, because the end values
c         of the spline are constrained to have particular values.
c
c  TOLNEW double precision TOLNEW.
c         TOLNEW is the convergence tolerance for the Newton
c         iteration.
c
c  TOLOPT double precision TOLOPT.
c         TOLOPT is the convergence tolerance for the optimization.
c
c         If TOLOPT is zero, then default values are used.
c
c  TOLSIM double precision TOLSIM.
c         TOLSIM is the convergence tolerance for the Picard
c         iteration.
c
c  VOPT   double precision VOPT(LV).
c         VOPT provides real workspace for the optimization routines.
c
c  WATEB  double precision WATEB.
c         WATEB is the multiplier of the bump control cost used
c         when computing the total cost.
c
c  WATEP,
c  WATEU,
c  WATEV  double precision WATEP, WATEU, WATEV.
c
c         WATEP, WATEU and WATEV are weights used in computing the
c         cost function based on the costs of the flow discrepancy.
c
c  XBL    double precision XBL.
c         XBL is the X coordinate of the left corner of the bump.
c
c  XBR    double precision XBR.
c         XBR is the X coordinate of the right corner of the bump.
c
c  XOPT   double precision XOPT(MAXPAR).
c         XOPT is used by the optimization routines to hold only
c         the values of parameters which are allowed to vary.
c
c  YBL    double precision YBL.
c         YBL is the Y coordinate of the left corner of the bump.
c
c  YBR    double precision YBR.
c         YBR is the Y coordinate of the right corner of the bump.
c
c  YC     double precision YC(NP).
c         YC contains the Y coordinates of the nodes.
c
      implicit double precision (a-h,o-z)
c
c  Set parameters that are independent.
c
      integer liv
      integer lv
      integer maxcofrb
      integer maxelm    
      integer maxnfl   
      integer maxnp               
      integer maxny
      integer maxpar      
      integer maxparb
c
      double precision arb(maxcofrb,maxcofrb)
      double precision area(3,maxelm)
      double precision cost
      double precision costb
      double precision costp
      double precision costu
      double precision costv
      double precision dopt(maxpar)
      double precision gfltar(maxnfl)
      double precision gfltmp(maxnfl)
      double precision grb(ncofrb)
      double precision grbarb(ncofrb)
      double precision grbopt(ncofrb)
c
c  FORTRAN 90 local array:
c
      integer i
      integer ierror
      integer indx(3,maxnp)
      integer iopt(maxpar)
      integer ipivrb(maxcofrb)
      integer ival
      integer ivopt(liv)
      integer iwrite
      integer maxnew
      integer maxopt
      integer maxsim
      integer nbcrb
      integer ncofrb
      integer nelem
      integer neqnfl
      integer nferb
      integer node(6,nelem)
      integer nopt
      integer np
      integer npar
      integer nparb
      integer nparf
      integer nprof(2*maxny-1)
      integer numdif
      integer numopt
      integer nx
      integer ny
      double precision par(maxpar)
      double precision pararb(maxpar)
      double precision paropt(maxpar)
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision rb(maxnfl,maxcofrb)
      double precision resrb(maxcofrb)
      double precision rmax
      double precision splbmp(maxparb+2)
      double precision taubmp(maxparb+2)
      double precision tauflo(nparf)
      double precision tolnew
      double precision tolopt
      double precision tolsim
      double precision vopt(lv)
      double precision wateb
      double precision watep
      double precision wateu
      double precision watev
      double precision xbl
      double precision xbr
      double precision xc(np)
      double precision xopt(maxpar)
      double precision xrange
      double precision ybl
      double precision ybr
      double precision yc(maxnp)
      double precision yrange
c
      ierror=0
c
c  Copy the initial solution estimate.
c
      do i=1,npar
        paropt(i)=par(i)
      enddo
 
      do i=1,ncofrb
        grbopt(i)=grb(i)
      enddo
c
c  Initialize the local optimization data.
c
      cost=0.0

      do i=1,npar
        dopt(i)=1.0
      enddo
      
      do i=1,liv
        ivopt(i)=0
      enddo
      
      nopt=0
      
      do i=1,lv
        vopt(i)=0.0
      enddo
      
      do i=1,maxpar
        xopt(i)=0.0
      enddo
c
c  Set the 611 data to default values,
c  and then modify some values.
c
      ival=2
      call deflt(ival,ivopt,liv,lv,vopt)

      vopt(31)=tolopt
      vopt(32)=tolopt
      vopt(33)=tolopt
      vopt(34)=tolopt
      vopt(37)=tolopt

      ivopt(1)=12
      ivopt(19)=0
c
c  Set the step counters.
c
      numdif=0
      numopt=0
c
c  Take the next optimization step.
c
10    continue

      if(numopt.gt.maxopt)then
        write(*,*)' '
        write(*,*)'OPTDIFRB - Warning!'
        write(*,*)'  The number of optimization steps was exceeded.'
        return
      endif
c
c  Make a "copy" of PAR that only contains the free variables.
c
      nopt=0
      do i=1,npar
        if(iopt(i).eq.1)then
          nopt=nopt+1
          xopt(nopt)=paropt(i)
        endif
      enddo
c
c  Call the optimizer to get a new parameter estimate.
c
      call snoit(dopt,cost,ivopt,liv,lv,nopt,vopt,xopt)
c
c  Copy the new free variable values back into PAR.
c
      nopt=0
      do i=1,npar
        if(iopt(i).eq.1)then
          nopt=nopt+1
          paropt(i)=xopt(nopt)
        endif
      enddo
c
c  Apply Picard's method to the approximate solution GRBOPT.
c
        call picrb(arb,area,grbopt,ierror,indx,ipivrb,iwrite,
     &    maxcofrb,maxelm,maxnfl,maxsim,nbcrb,ncofrb,nelem,
     &    nferb,node,np,npar,nparf,nx,ny,paropt,phirb,rb,resrb,rmax,
     &    tauflo,tolsim,xc,xrange,yc,yrange)
c
c  Apply Newton's method to the approximate solution GRBOPT.
c
        if(rmax.gt.tolnew)then
          call newtrb(arb,area,grbopt,grbarb,ierror,indx,ipivrb,
     &      iwrite,maxcofrb,maxelm,maxnew,maxnfl,nbcrb,ncofrb,nelem,
     &      nferb,node,np,npar,nparf,nx,ny,paropt,pararb,phirb,
     &      rb,resrb,rmax,tauflo,tolnew,xc,xrange,yc,yrange)

          if(ierror.ne.0)then
            write(*,*)' '
            write(*,*)'OPTDIFRB - Fatal error!'
            write(*,*)'  NEWTRB failed!'
            write(*,*)'  The parameters at which failure occurred:'
            write(*,*)' '
            call prpar(iopt,npar,nparb,nparf,paropt)
            ierror=1
            return
          endif
        endif
c
c  Compute the equivalent full basis solution GFLTMP=RB*GRB.
c
      call grb2fl(gfltmp,grbopt,maxnfl,ncofrb,neqnfl,rb)
c
c  Compute the cost function COST.
c
      call getcst(cost,costb,costp,costu,costv,gfltmp,gfltar,
     &  indx,neqnfl,np,nparb,nprof,ny,splbmp,
     &  taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)
 
      if(ivopt(1).eq.1.and.iwrite.ge.0)then
        call prpar(iopt,npar,nparb,nparf,paropt)
        write(*,*)'  Cost=',cost
      endif
c
c  If IVOPT(1) is 1, then this was a call for a legitimate
c  solution candidate.
c
c  If IVOPT(1) is 2, then this was a call for a temporary
c  solution used only for estimating the gradient.
c
c  Other values of IVOPT indicate acceptance or rejection
c  of the iteration.
c
      if(ivopt(1).eq.1)then
        numopt=numopt+1
      elseif(ivopt(1).eq.2)then
        numdif=numdif+1
      elseif(ivopt(1).ge.3.and.ivopt(1).le.8)then
        write(*,*)' '
        write(*,*)'  Convergence to a minimizer was achieved!'
        return
      elseif(ivopt(1).gt.8)then
        write(*,*)' '
        write(*,*)'  Bad value of IVOPT(1)=',ivopt(1)
        return
      endif
 
      go to 10

      end
      subroutine picfl(afl,area,eqn,gfl,ierror,ifs,indx,ipivfl,iwrite,
     &  ldafl,maxsim,nelem,neqnfl,nlband,node,np,npar,nparb,nparf,
     &  numsim,par,phifl,region,resfl,rmax,splflo,tauflo,
     &  tolsim,xc,xrange,yc,yrange)
c
c***********************************************************************
c
cc PICFL carries out simple iteration on the full Navier Stokes 
c  equations.
c
c  The simple iteration equations have the form:
c
c    Integral
c
c      dU/dx * dW/dx
c    + dU/dy * dW/dy
c    + reynld * (UOLD*dU/dx + VOLD*dU/dy + dP/dx) * W dx dy = 0
c
c    Integral
c
c      dV/dx * dW/dx
c    + dV/dy * dW/dy
c    + reynld * (UOLD*dV/dx + VOLD*dV/dy + dP/dy) * W dx dy = 0
c
c    Integral
c
c      (dU/dx + dV/dy) * Q dx dy = 0
c
c  Here W is a basis function for U and V, and Q is a basis
c  function for P.  UOLD and VOLD are the values of U and V
c  on the previous step of the iteration.
c
c
c  PICFL was modified on 06 August 1996.
c
c
c  AFL    Output, double precision A(LDAFL,NEQNFL), contains the
c         coefficients of the Picard iteration matrix.
c
c  AREA   Input, double precision AREA(3,NELEM).
c
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  EQN    Input, character*2 EQN(NEQNFL).
c         EQN records the "type" of each equation that will be generated, and
c         which is associated with an unknown.  Note that most boundary 
c         conditions do not result in an equation.  The current values are:
c
c         'U'  The horizontal momentum equation.
c         'UB' The condition U=0 applied at a node on the bump.
c         'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c         'UW' The condition U=0 applied at a node on a fixed wall.
c
c         'V'  The vertical momentum equation.
c         'VB' The condition V=0 applied at a node on the bump.
c         'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c         'VW' The condition V=0 applied at a node on a fixed wall.
c
c         'P'  The continuity equation.
c         'PB' The condition P=0 applied at (XMAX,YMAX).
c
c  GFL    Input/output, double precision GFL(NEQNFL).
c
c         G is the current solution vector, in which are stored 
c         the finite element coefficients that define the velocity
c         and pressure functions, U, V and P.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  LDAFL  Input, integer LDAFL, the first dimension of the matrix AFL.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NEQNFL Input, integer NEQNFL, the number of finite element equations used
c         to define the horizontal and vertical velocities and the
c         pressure.
c 
c  NLBAND Input, integer NLBAND.
c
c         The lower bandwidth of the matrix A.  The zero structure of A
c         is assumed to be symmetric, and so NLBAND is also the upper
c         bandwidth of A.  
c 
c  NODE   Input, integer NODE(6,NELEM).
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
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c 
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c 
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c 
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the 
c         inflow.  NPARF must be at least 1.  
c
c  PAR    Input, double precision PAR(NPAR).
c
c         PAR is the current set of parameter values, including the
c         Reynolds parameter, the flow parameters, and the bump parameters.
c
c  PHIFL  Input, double precision PHIFL(3,6,10,NELEM).  
c
c         PHIFL contains the value of a finite element basis function, its
c         derivative, or other information, evaluated at the quadrature 
c         points.
c 
c         The meaning of the entry PHIFL(I,J,K,L) is as follows.  
c         For the quadrature point I, and basis function J, in element L,
c         PHIFL(I,J,K,L) represents the value of:
c
c           K= 1, W, the finite element basis function for velocities;
c           K= 2, dWdX, the X derivative of W;
c           K= 3, dWdY, the Y derivative of W;
c           K= 4, Q, the finite element basis function for pressures;
c           K= 5, dQdX, the X derivative of Q;
c           K= 6, dQdY, the Y derivative of Q;
c           K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c           K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c           K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c           K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c         In particular, PHIFL(I,J,K,L) is the value of the quadratic 
c         basis function W associated with local node J in element L, 
c         evaluated at quadrature point I.
c 
c         Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6, 
c         since there are only three linear basis functions.
c
c
c  RMAX   Output, double precision RMAX.
c         RMAX contains the maximum absolute value of the the
c         entries of the residual vector evaluated at the returned
c         solution estimate.
c
      implicit double precision (a-h,o-z)
c
      integer ldafl
      integer nelem
      integer neqnfl
      integer np
      integer npar
      integer nparf
c
      double precision afl(ldafl,neqnfl)
      double precision area(3,nelem)
      double precision dxmax
      character*2 eqn(neqnfl)
      double precision gfl(neqnfl)
      integer i
      integer idamax
      integer ierror
      integer ifs
      integer indx(3,np)
      integer info
      integer ipivfl(neqnfl)
      integer irmax
      integer iwrite
      integer ixmax
      integer maxsim
      integer nlband
      integer node(6,nelem)
      integer nparb
      integer numsim
      double precision par(npar)
      double precision phifl(3,6,10,nelem)
      character*20 region
      double precision resfl(neqnfl)
      double precision rmax
      double precision rmax0
      double precision splflo(nparf+2)
      double precision tauflo(nparf+2)
      double precision tolsim
      double precision xc(np)
      double precision xmax
      double precision xmax0
      double precision xrange
      double precision yc(np)
      double precision yrange
c
      ierror=0
c
c  Get XMAX0, the norm of the initial guess GFL.
c
      ixmax=idamax(neqnfl,gfl,1)
      xmax=abs(gfl(ixmax))
      xmax0=xmax
c
c  Get RMAX0, the norm of the error RESFL of the initial guess, GFL.
c
      call fxfl(area,eqn,gfl,ifs,indx,nelem,neqnfl,node,np,npar,
     &  nparb,nparf,par,phifl,region,resfl,splflo,tauflo,
     &  xrange,yrange)
 
      irmax=idamax(neqnfl,resfl,1)
      rmax=abs(resfl(irmax))
      rmax0=rmax

      numsim=0

      if(iwrite.ge.2)then
        write(*,*)' '
        write(*,*)' Step   MxNorm(X)   IXmax  MxNorm(FX)   IRmax'
        write(*,'(i6,g14.6,i6,g14.6,i6)')numsim,xmax,ixmax,rmax,irmax
      endif
 
      if(rmax0.lt.tolsim)then
        return
      endif
c
c  Do up to MAXSIM steps of simple iteration.
c
      do numsim=1,maxsim
c
c  Get the simple iteration system matrix AFL evaluated at GFL.
c
        call picmfl(afl,area,eqn,gfl,indx,ldafl,nelem,neqnfl,nlband,
     &    node,np,npar,par,phifl)
c
c  Factor the matrix.
c
        call dfacfl(afl,ldafl,neqnfl,nlband,nlband,ipivfl,info)

        if(info.ne.0)then
          write(*,*)' '
          write(*,*)'PicFL - Fatal error!'
          write(*,*)'  The Picard matrix AFL is singular!'
          write(*,*)'  DFACFL returns nonzero INFO=',info
          ierror=1
          return
        endif
c
c  Get the right hand side, RESFL.
c
        call picvfl(eqn,ifs,indx,neqnfl,np,npar,nparf,par,
     &    region,resfl,splflo,tauflo,xc,xrange,yc,yrange)
c
c  Solve the linear system AFL*GFL=RESFL.
c
        call dsolfl(afl,ldafl,neqnfl,nlband,nlband,ipivfl,resfl)
c
c  Compare RESFL and the previous estimate GFL.
c
        dxmax=0.0
        ixmax=0
        do i=1,neqnfl
          if(abs(resfl(i)-gfl(i)).ge.dxmax)then
            ixmax=i
            dxmax=abs(resfl(i)-gfl(i))
          endif
        enddo
c
c  Update GFL with the new estimate, and save its norm.
c
        do i=1,neqnfl
          gfl(i)=resfl(i)
        enddo

        ixmax=idamax(neqnfl,gfl,1)
        xmax=abs(gfl(ixmax))
c
c  Compute FX(GFL).
c
        call fxfl(area,eqn,gfl,ifs,indx,nelem,neqnfl,node,np,npar,
     &    nparb,nparf,par,phifl,region,resfl,splflo,tauflo,
     &    xrange,yrange)

        irmax=idamax(neqnfl,resfl,1)
        rmax=abs(resfl(irmax))
c
c  Print out
c
        if(iwrite.ge.2)then
          write(*,'(i6,g14.6,i6,g14.6,i6)')numsim,xmax,ixmax,rmax,irmax
        endif
c
c  Converged, Failed, or Continue?
c
        if(rmax.lt.tolsim*(rmax0+1.0))then
          if(iwrite.ge.2)then
            write(*,*)'PicFL - Residual acceptance.'
          endif
          return
        endif
 
        if(rmax.gt.1000.0*rmax0)then
          ierror=1
          write(*,*)' '
          write(*,*)'PicFL - Fatal error!'
          write(*,*)'  Simple iteration diverging on step ',numsim
          write(*,*)'  MxNorm of first X = ',xmax0
          write(*,*)'  MxNorm of last X =  ',xmax
          write(*,*)'  MxNorm of first FX =',rmax0
          write(*,*)'  MxNorm of last FX = ',rmax
          return
        endif
 
      enddo

      if(iwrite.ge.3)then
        write(*,*)' '
        write(*,*)'PicFL - Warning:'
        write(*,*)'  Simple iteration did not converge.'
        write(*,*)'  MxNorm of first X = ',xmax0
        write(*,*)'  MxNorm of last X =  ',xmax
        write(*,*)'  MxNorm of first FX =',rmax0
        write(*,*)'  MxNorm of last FX = ',rmax
      endif
 
      return
      end
      subroutine picmferb(arb,area,grb,maxcofrb,maxelm,nbcrb,ncofrb,
     &  nelem,nferb,phirb,reynld)
c
c***********************************************************************
c
cc PICMFERB evaluates the simple iteration matrix.
c
c  PICMFERB is given
c
c    GRB, the reduced basis coefficients of an approximate solution,
c    PHIRB, the reduced basis functions, evaluated at the quadrature 
c      points,
c    REYNLD, the current Reynolds number,
c
c  and computes 
c
c    ARB, the simple iteration matrix.
c
c
c  PICMFERB was modified on 11 August 1996.
c
c
c  ARB    
c    double precision ARB(MAXCOFRB,MAXCOFRB).
c    ARB contains the Jacobian or Picard matrix for the reduced
c    Navier Stokes system, stored as an NCOFRB by NCOFRB array. 
c
c  AREA   
c    double precision AREA(3,MAXELM).
c    AREA contains a common factor multiplying the term associated
c    with a quadrature point in a given element, namely,
c
c      AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c    or, if the element is isoperimetric,
c
c      AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c    Here Ar(IELEM) represents the area of element IELEM.
c
c  GRB    
c    double precision GRB(NCOFRB).
c    GRB contains the reduced basis coefficients of the current
c    estimate of the state solution.
c
c  MAXCOFRB
c    integer MAXCOFRB.
c    MAXCOFRB is the maximum legal value for NCOFRB, the number
c    of coefficients used to specify a particular reduced basis
c    solution.
c
c  MAXELM 
c    integer MAXELM.
c    MAXELM is the maximum number of elements.
c
c  NBCRB  
c    integer NBCRB.
c    NBCRB is the number of independent boundary condition
c    vectors used for the reduced basis.  NBCRB is normally
c    at least 1, and must be no more than MAXBCRB.
c
c  NCOFRB 
c    integer NCOFRB.
c    NCOFRB is the number of coefficients needed to determine
c    a particular reduced basis function.
c    NCOFRB is the sum of NBCRB and NFERB.
c
c  NELEM  
c    integer NELEM.
c    NELEM is the number of elements.
c    NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NFERB  
c    integer NFERB.
c    NFERB is the number of reduced basis coefficients that will
c    be determined via the finite element method.
c
c  PHIRB  
c    double precision PHIRB(3,MAXCOFRB,15,MAXELM).
c    PHIRB contains the values of a finite element basis function
c    or its X or Y derivative, in a given element, at a given
c    quadrature point, for a particular reduced basis function.
c
c    For PHIRB(I,J,K,L), index J refers to the reduced basis
c    basis functions, for J=0 to NCOFRB.
c
c    The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c      For the quadrature point I, and reduced basis function J,
c      in element L, PHIRB(I,J,K,L) represents the value of:
c
c        K=1, WUrb, the finite element U velocity basis function;
c        K=2, dWUrbdX, the X derivative of WUrb;
c        K=3, dWUrbdY, the Y derivative of WUrb;
c        K=4, WVrb, the finite element V velocity basis function;
c        K=5, dWVrbdX, the X derivative of WVrb;
c        K=6, dWVrbdY, the Y derivative of WVrb;
c        K=7, Q, the finite element pressure basis function.
c        K=8, dQrbdX, the X derivative of Qrb;
c        K=9, dQrbdY, the Y derivative of Qrb.
c        K=10, WU0rb, same as WUrb, with zero BC.
c        K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c        K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c        K=13, WV0rb, same as WVrb, with zero BC.
c        K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c        K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  REYNLD 
c    double precision REYNLD.
c    REYNLD is the current value of the Reynolds number.
c    Normally, REYNLD is stored as PARA(NPARF+NPARB+1).
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxelm
      integer ncofrb
      integer nelem
c
      double precision ar
      double precision arb(maxcofrb,maxcofrb)
      double precision area(3,maxelm)
      double precision dqjdx
      double precision dqjdy
      double precision dprbdx
      double precision dprbdy
      double precision durbdx
      double precision durbdy
      double precision dvrbdx
      double precision dvrbdy
      double precision dwu0dx
      double precision dwujdx
      double precision dwu0dy
      double precision dwujdy
      double precision dwv0dx
      double precision dwvjdx
      double precision dwv0dy
      double precision dwvjdy
      double precision grb(ncofrb)
      integer icofrb
      integer ielem
      integer iquad
      integer jcofrb
      logical leqi
      integer nbcrb
      integer nferb
      double precision prb
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision reynld
      double precision urb
      double precision vrb
      double precision wu0
      double precision wv0
c
      external leqi
c
c  Zero out the FE rows of the matrix.
c
      do icofrb=nbcrb+1,nbcrb+nferb
        do jcofrb=1,ncofrb
          arb(icofrb,jcofrb)=0.0
        enddo
      enddo
c
c  Consider an element IELEM...
c
      do ielem=1,nelem
c
c  ...and a quadrature point IQUAD...
c
        do iquad=1,3
 
          ar=area(iquad,ielem)
c
c  For the given reduced coefficients GRB, and basis functions
c  PHIRB, evaluate U, V, and P, and their spatial derivatives.
c
          call uvpqrb(dprbdx,dprbdy,durbdx,durbdy,dvrbdx,dvrbdy,grb,
     &      ielem,iquad,maxcofrb,maxelm,ncofrb,phirb,prb,urb,vrb)
c
c  Consider FE reduced basis function ICOFRB.
c
          do icofrb=nbcrb+1,nbcrb+nferb

            wu0   =phirb(iquad,icofrb,10,ielem)
            dwu0dx=phirb(iquad,icofrb,11,ielem)
            dwu0dy=phirb(iquad,icofrb,12,ielem)
            wv0   =phirb(iquad,icofrb,13,ielem)
            dwv0dx=phirb(iquad,icofrb,14,ielem)
            dwv0dy=phirb(iquad,icofrb,15,ielem)
c
c  Take the derivative with respect to basis function JCOFRB.
c
            do jcofrb=1,ncofrb

              dwujdx=phirb(iquad,jcofrb,2,ielem)
              dwujdy=phirb(iquad,jcofrb,3,ielem)

              dwvjdx=phirb(iquad,jcofrb,5,ielem)
              dwvjdy=phirb(iquad,jcofrb,6,ielem)

              dqjdx =phirb(iquad,jcofrb,8,ielem)
              dqjdy =phirb(iquad,jcofrb,9,ielem)
c
c  The horizontal momentum equations.
c
              arb(icofrb,jcofrb)=arb(icofrb,jcofrb)
     &          +ar*(dwujdx*dwu0dx + dwujdy*dwu0dy
     &          +reynld*(urb*dwujdx+vrb*dwujdy+dqjdx)*wu0)
c
c  The vertical momentum equations.
c
              arb(icofrb,jcofrb)=arb(icofrb,jcofrb)
     &          +ar*(dwvjdx*dwv0dx + dwvjdy*dwv0dy
     &          +reynld*(urb*dwvjdx+vrb*dwvjdy+dqjdy)*wv0)
 
            enddo
          enddo
        enddo
      enddo
 
      return
      end
      subroutine picmfl(afl,area,eqn,gfl,indx,ldafl,nelem,neqnfl,nlband,
     &  node,np,npar,par,phifl)
c
c***********************************************************************
c
cc PICMFL computes the Picard iteration matrix for the full
c  Navier Stokes equations.
c
c
c  The coefficients are:
c
c
c  d U-Eqn/d U-Coef:
c
c    Integral
c
c      dWj/dx * dWi/dx + dWj/dy * dWi/dy
c    + reynld * (Uold*dWj/dx+ Vold*dWj/dy) * Wi dx dy
c
c  d U-Eqn/d P-Coef:
c
c    Integral
c
c    reynld * dQj/dx * Wi dx dy
c
c  d V-Eqn/d V-Coef:
c
c    Integral
c
c      dWj/dx * dWi/dx + dWj/dy * dWi/dy
c    + reynld * (Uold*dWj/dx + Vold*dWj/dy) * Wi dx dy
c
c  d V-Eqn/d P-Coef:
c
c    Integral
c
c    reynld * dQj/dy * Wi dx dy
c
c  d P-Eqn/d U-Coef:
c
c    Integral
c
c      dWj/dx * Qi dx dy
c
c  d P-Eqn/d V-Coef:
c
c    Integral
c
c      dWj/dy * Qi dx dy
c
c
c  PICMFL was modified on 01 July 1996.
c
c
c  AFL    Output, double precision A(LDAFL,NEQNFL), contains the
c         coefficients of the Picard iteration matrix.
c
c  AREA   Input, double precision AREA(3,NELEM).
c
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  EQN    Input, character*2 EQN(NEQNFL).
c         EQN records the "type" of each equation that will be generated, and
c         which is associated with an unknown.  Note that most boundary 
c         conditions do not result in an equation.  The current values are:
c
c         'U'  The horizontal momentum equation.
c         'UB' The condition U=0 applied at a node on the bump.
c         'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c         'UW' The condition U=0 applied at a node on a fixed wall.
c
c         'V'  The vertical momentum equation.
c         'VB' The condition V=0 applied at a node on the bump.
c         'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c         'VW' The condition V=0 applied at a node on a fixed wall.
c
c         'P'  The continuity equation.
c         'PB' The condition P=0 applied at (XMAX,YMAX).
c
c  GFL    Input, double precision GFL(NEQNFL).
c
c         G is the current solution vector, in which are stored 
c         the finite element coefficients that define the velocity
c         and pressure functions, U, V and P.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  LDAFL  Input, integer LDAFL, the first dimension of the matrix AFL.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NEQNFL Input, integer NEQNFL, the number of finite element equations used
c         to define the horizontal and vertical velocities and the
c         pressure.
c 
c  NLBAND Input, integer NLBAND.
c
c         The lower bandwidth of the matrix A.  The zero structure of A
c         is assumed to be symmetric, and so NLBAND is also the upper
c         bandwidth of A.  
c 
c  NODE   Input, integer NODE(6,NELEM).
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
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c 
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c 
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c 
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the 
c         inflow.  NPARF must be at least 1.  
c
c  PAR    Input, double precision PAR(NPAR).
c
c         PAR is the current set of parameter values, including the
c         Reynolds parameter, the flow parameters, and the bump parameters.
c
c  PHIFL  Input, double precision PHIFL(3,6,10,NELEM).  
c
c         PHIFL contains the value of a finite element basis function, its
c         derivative, or other information, evaluated at the quadrature 
c         points.
c 
c         The meaning of the entry PHIFL(I,J,K,L) is as follows.  
c         For the quadrature point I, and basis function J, in element L,
c         PHIFL(I,J,K,L) represents the value of:
c
c           K= 1, W, the finite element basis function for velocities;
c           K= 2, dWdX, the X derivative of W;
c           K= 3, dWdY, the Y derivative of W;
c           K= 4, Q, the finite element basis function for pressures;
c           K= 5, dQdX, the X derivative of Q;
c           K= 6, dQdY, the Y derivative of Q;
c           K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c           K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c           K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c           K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c         In particular, PHIFL(I,J,K,L) is the value of the quadratic 
c         basis function W associated with local node J in element L, 
c         evaluated at quadrature point I.
c 
c         Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6, 
c         since there are only three linear basis functions.
c
      implicit double precision (a-h,o-z)
c
      integer ldafl
      integer nelem
      integer neqnfl
      integer np
      integer npar
c
      double precision afl(ldafl,neqnfl)
      double precision ar
      double precision area(3,nelem)
      double precision dpdx
      double precision dpdy
      double precision dqjdx
      double precision dqjdy
      double precision dudx
      double precision dudy
      double precision dvdx
      double precision dvdy
      double precision dwidx
      double precision dwidy
      double precision dwjdx
      double precision dwjdy
      character*2 eqn(neqnfl)
      double precision gfl(neqnfl)
      integer i
      integer ielem
      integer ihor
      integer indx(3,np)
      integer ip
      integer iprs
      integer iq
      integer iquad
      integer iuse
      integer iver
      integer j
      integer jhor
      integer jp
      integer jprs
      integer jq
      integer jver
      logical leqi
      integer nlband
      integer node(6,nelem)
      double precision p
      double precision par(npar)
      double precision phifl(3,6,10,nelem)
      double precision qi
      double precision reynld
      double precision term
      double precision u
      double precision v
      double precision wi
c
      external leqi
c
      reynld=par(npar)

      do i=1,3*nlband+1
        do j=1,neqnfl
          afl(i,j)=0.0
        enddo
      enddo
c
c  Approximate the integral by summing over all elements.
c
      do ielem=1,nelem
c
c  Evaluate the integrand at the quadrature points.
c
        do iquad=1,3
 
          ar=area(iquad,ielem)
c
c  Evaluate U, V and P at the IQUAD-th quadrature point.
c
          call uvpqfl(dpdx,dpdy,dudx,dudy,dvdx,dvdy,gfl,ielem,indx,
     &      iquad,nelem,neqnfl,node,np,p,phifl,u,v)
c
c  Consider each node in the element.
c
          do iq=1,6
 
            ip=node(iq,ielem)
 
            wi=phifl(iquad,iq,1,ielem)
            dwidx=phifl(iquad,iq,2,ielem)
            dwidy=phifl(iquad,iq,3,ielem)
            qi=phifl(iquad,iq,4,ielem)
 
            ihor=indx(1,ip)
            iver=indx(2,ip)
            iprs=indx(3,ip)
c
c  Now compute the derivatives of the functions associated
c  with U, V and P, with respect to the coefficients associated
c  with basis vectors at each node of the element.
c
            do jq=1,6
 
              jp=node(jq,ielem)
 
              dwjdx=phifl(iquad,jq,2,ielem)
              dwjdy=phifl(iquad,jq,3,ielem)
 
              dqjdx=phifl(iquad,jq,5,ielem)
              dqjdy=phifl(iquad,jq,6,ielem)
 
              jhor=indx(1,jp)
              jver=indx(2,jp)
              jprs=indx(3,jp)
c
c  Contributions of the JHOR horizontal velocity to the U, V, and 
c  P equations.
c
              if(eqn(ihor).eq.'U')then

                  term=ar*(dwjdx*dwidx+dwjdy*dwidy+
     &              reynld*(u*dwjdx+v*dwjdy)*wi)

                iuse=ihor-jhor+2*nlband+1
                afl(iuse,jhor)=afl(iuse,jhor)+term

              endif
 
              if(iprs.gt.0)then
                if(eqn(iprs).eq.'P')then
                  term=ar*dwjdx*qi
                  iuse=iprs-jhor+2*nlband+1
                  afl(iuse,jhor)=afl(iuse,jhor)+term
                endif
              endif
c
c  Contributions of the JVER vertical velocity variable to the
c  U, V and P equations.
c
              if(eqn(iver).eq.'V')then

                  term=ar*(dwjdx*dwidx+dwjdy*dwidy
     &              +reynld*(u*dwjdx+v*dwjdy)*wi)

                iuse=iver-jver+2*nlband+1
                afl(iuse,jver)=afl(iuse,jver)+term
              endif
 
              if(iprs.gt.0)then
                if(eqn(iprs).eq.'P')then
                  term=ar*dwjdy*qi
                  iuse=iprs-jver+2*nlband+1
                  afl(iuse,jver)=afl(iuse,jver)+term
                endif
              endif
c
c  Contributions of the JPRS pressure to the U and V equations.
c
              if(jprs.gt.0)then
 
                if(eqn(ihor).eq.'U')then
                  term=ar*reynld*dqjdx*wi
                  iuse=ihor-jprs+2*nlband+1
                  afl(iuse,jprs)=afl(iuse,jprs)+term
                endif
 
                if(eqn(iver).eq.'V')then
                  term=ar*reynld*dqjdy*wi
                  iuse=iver-jprs+2*nlband+1
                  afl(iuse,jprs)=afl(iuse,jprs)+term
                endif
 
              endif
 
            enddo
          enddo
        enddo
      enddo
c
c  Set up the equations that enforce boundary conditions.
c
      do ip=1,np

        ihor=indx(1,ip)
        iver=indx(2,ip)
        iprs=indx(3,ip)

        if(eqn(ihor).eq.'UB'.or.
     &     eqn(ihor).eq.'UI'.or.
     &     eqn(ihor).eq.'UW'.or.
     &     eqn(ihor).eq.'U0')then
          afl(2*nlband+1,ihor)=1.0
        endif

        if(eqn(iver).eq.'VB'.or.
     &     eqn(iver).eq.'VI'.or.
     &     eqn(iver).eq.'VW'.or.
     &     eqn(iver).eq.'V0')then
          afl(2*nlband+1,iver)=1.0
        endif

        if(iprs.gt.0)then
          if(eqn(iprs).eq.'PB')then
            afl(2*nlband+1,iprs)=1.0
          elseif(eqn(iprs).eq.'P0')then
            afl(2*nlband+1,iprs)=1.0
          endif
        endif

      enddo

      return
      end
      subroutine picrb(arb,area,grb,ierror,indx,ipivrb,iwrite,
     &  maxcofrb,maxelm,maxnfl,maxsim,nbcrb,ncofrb,nelem,
     &  nferb,node,np,npar,nparf,nx,ny,par,phirb,rb,resrb,rmax,
     &  tauflo,tolsim,xc,xrange,yc,yrange)
c
c***********************************************************************
c
cc PICRB carries out simple iteration on the reduced Navier Stokes 
c  equations. 
c
c  The simple iteration equations have the form:
c
c    Integral
c
c      dU/dx * dW/dx + dU/dy * dW/dy
c    + reynld * (URB*dU/dx + VRB*dU/dy + dP/dx) * W dx dy = 0
c
c    Integral
c
c      dV/dx * dW/dx + dV/dy * dW/dy
c    + reynld * (URB*dV/dx + VRB*dV/dy + dP/dy) * W dx dy = 0
c
c  Here W is a basis function for U and V.
c  UOLD and VOLD are the values of U and V
c  on the previous step of the iteration.
c
c
c  PICRB was modified on 08 August 1996.
c
c
c  ARB    Workspace, double precision ARB(MAXNRB,MAXNRB).
c         ARB contains the Jacobian or Picard matrix for the reduced 
c         Navier Stokes system, stored as a dense NCOFRB by NCOFRB array.
c
c  AREA   Input, double precision AREA(3,NELEM).
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  GRB    Input, double precision GRB(NCOFRB).
c         GRB contains the reduced basis coefficients of the current 
c         estimate of the state solution.
c
c  GRBTMP Workspace, double precision GRBTMP(NCOFRB).
c
c  IERROR Output, integer IERROR.
c         0, no error occurred.
c         1, an error occurred.  The matrix was singular.
c
c  IPIVRB Workspace, integer IPIVRB(NCOFRB).
c
c  IWRITE Input, integer IWRITE.
c         IWRITE controls the amount of output printed.
c
c  MAXSIM Input, integer MAXSIM.
c         MAXSIM is the maximum number of simple iteration steps
c         that may be taken.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NCOFRB Input, integer NCOFRB.
c         NCOFRB is the number of basis functions, reduced state equations and 
c         coefficients in the reduced basis system.
c
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c 
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c 
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the 
c         inflow.  NPARF must be at least 1.  
c
c  PAR    Input, double precision PAR(NPAR).
c
c         PAR is the current set of parameter values, including the
c         Reynolds parameter, the flow parameters, and the bump parameters.
c
c  PHIRB  Input, double precision PHIRB(3,NCOFRB,15,NELEM).
c         PHIRB contains the values of a finite element basis function
c         or its X or Y derivative, in a given element, at a given
c         quadrature point, for a particular reduced basis function.
c
c         For PHIRB(I,J,K,L), index J refers to the reduced basis
c         basis functions, for J=0 to NCOFRB.
c
c         The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c           For the quadrature point I, and reduced basis function J,
c           in element L, PHIRB(I,J,K,L) represents the value of:
c
c             K=1, WUrb, the finite element U velocity basis function;
c             K=2, dWUrbdX, the X derivative of WUrb;
c             K=3, dWUrbdY, the Y derivative of WUrb;
c             K=4, WVrb, the finite element V velocity basis function;
c             K=5, dWVrbdX, the X derivative of WVrb;
c             K=6, dWVrbdY, the Y derivative of WVrb;
c             K=7, Q, the finite element pressure basis function.
c             K=8, dQrbdX, the X derivative of Qrb;
c             K=9, dQrbdY, the Y derivative of Qrb.
c             K=10, WU0rb, same as WUrb, with zero BC.
c             K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c             K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c             K=13, WV0rb, same as WVrb, with zero BC.
c             K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c             K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  RESRB  Workspace, double precision RESRB(NCOFRB).
c
c  TOLSIM Input, double precision TOLSIM.
c         TOLSIM is the convergence tolerance for the iteration.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxelm
      integer maxnfl
      integer ncofrb
      integer nelem
      integer np
      integer npar
      integer nparf
c
      double precision arb(maxcofrb,ncofrb)
      double precision area(3,nelem)
      double precision dxmax
      double precision grb(ncofrb)
c
c  FORTRAN 90 local array:
c
      double precision grbtmp(ncofrb)
      integer i
      integer idamax
      integer ierror
      integer indx(3,np)
      integer info
      integer ipivrb(ncofrb)
      integer irmax
      integer iwrite
      integer ixmax
      integer maxsim
      integer nbcrb
      integer nferb
      integer node(6,nelem)
      integer numsim
      integer nx
      integer ny
      double precision par(npar)
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision rb(maxnfl,maxcofrb)
      double precision resrb(ncofrb)
      double precision reynld
      double precision rmax
      double precision rmax0
      double precision tauflo(nparf)
      double precision tolsim
      double precision xc(np)
      double precision xmax
      double precision xmax0
      double precision xrange
      double precision yc(np)
      double precision yrange
c
      if(ncofrb.le.0)then
        write(*,*)' '
        write(*,*)'PICRB - Fatal error!'
        write(*,*)'  NCOFRB <= 0, NCOFRB=',ncofrb
        stop
      endif
c
      reynld=par(npar)
c
      ierror=0
c
c  Get XMAX0, the norm of the initial guess GRB.
c
      ixmax=idamax(ncofrb,grb,1)
      xmax=abs(grb(ixmax))
      xmax0=xmax
c
c  Get RMAX0, the norm of the error RESRB of the initial guess, GRB.
c
      call fxrb(area,grb,indx,maxcofrb,maxelm,maxnfl,nbcrb,ncofrb,
     &  nelem,nferb,node,np,npar,nparf,nx,ny,par,phirb,rb,
     &  resrb,reynld,tauflo,xc,xrange,yc,yrange)
 
      irmax=idamax(ncofrb,resrb,1)
      rmax=abs(resrb(irmax))
      rmax0=rmax

      numsim=0

      if(iwrite.ge.2)then
        write(*,*)' '
        write(*,*)' Step   MxNorm(X)   IXmax  MxNorm(FX)   IRmax'
        write(*,'(i6,g14.6,i6,g14.6,i6)')numsim,xmax,ixmax,rmax,irmax
      endif
 
      if(rmax0.lt.tolsim)then
        return
      endif
c
c  Do up to MAXSIM steps of simple iteration.
c
      do numsim=1,maxsim
c
c  Get the simple iteration system matrix ARB evaluated at GRB.
c
        call fpbcrb(arb,indx,maxcofrb,maxnfl,nbcrb,ncofrb,
     &    nelem,node,np,nx,ny,rb,xc,xrange,yc,yrange)
 
        call picmferb(arb,area,grb,maxcofrb,maxelm,nbcrb,ncofrb,
     &    nelem,nferb,phirb,reynld)
c
c  Factor the matrix.
c
        call dfacrb(arb,maxcofrb,ncofrb,ipivrb,info)

        if(info.ne.0)then
          write(*,*)' '
          write(*,*)'PicRB - Fatal error!'
          write(*,*)'  The Picard matrix ARB is singular!'
          write(*,*)'  DFACRB returns nonzero INFO=',info
          ierror=1
          return
        endif
c
c  Get the right hand side, RESRB.
c
        do i=1,ncofrb
          grbtmp(i)=0.0
        enddo
 
        call fxbcrb(grbtmp,indx,maxcofrb,maxnfl,nbcrb,ncofrb,nelem,
     &    node,np,npar,nparf,nx,ny,par,rb,resrb,tauflo,xc,xrange,yc,
     &    yrange)
 
        do i=1,nbcrb
          resrb(i)=-resrb(i)
        enddo
 
        call picvferb(area,grb,maxcofrb,maxelm,nbcrb,
     &    ncofrb,nelem,nferb,npar,par,phirb,resrb)
 
c
c  Solve the linear system ARB*GRB=RESRB.
c
        call dsolrb(arb,maxcofrb,ncofrb,ipivrb,resrb)
c
c  Compare RESRB and the previous estimate GRB.
c
        dxmax=0.0
        ixmax=0
        do i=1,ncofrb
          if(abs(resrb(i)-grb(i)).ge.dxmax)then
            ixmax=i
            dxmax=abs(resrb(i)-grb(i))
          endif
        enddo
c
c  Update GRB with the new estimate, and save its norm.
c
        do i=1,ncofrb
          grb(i)=resrb(i)
        enddo

        ixmax=idamax(ncofrb,grb,1)
        xmax=abs(grb(ixmax))
c
c  Compute FX(GRB).
c
        call fxrb(area,grb,indx,maxcofrb,maxelm,maxnfl,nbcrb,ncofrb,
     &    nelem,nferb,node,np,npar,nparf,nx,ny,par,phirb,rb,
     &    resrb,reynld,tauflo,xc,xrange,yc,yrange)

        irmax=idamax(ncofrb,resrb,1)
        rmax=abs(resrb(irmax))
c
c  Print out
c
        if(iwrite.ge.2)then
          write(*,'(i6,g14.6,i6,g14.6,i6)')numsim,xmax,ixmax,rmax,irmax
        endif
c
c  Converged, Failed, or Continue?
c
        if(rmax.lt.tolsim*(rmax0+1.0))then
          if(iwrite.ge.2)then
            write(*,*)'PicRB - Residual acceptance.'
          endif
          return
        endif
 
        if(rmax.gt.1000.0*rmax0)then
          ierror=1
          write(*,*)' '
          write(*,*)'PICRB - Fatal error!'
          write(*,*)'  Simple iteration diverging on step ',numsim
          write(*,*)'  MxNorm of first X = ',xmax0
          write(*,*)'  MxNorm of last X =  ',xmax
          write(*,*)'  MxNorm of first FX =',rmax0
          write(*,*)'  MxNorm of last FX = ',rmax
          return
        endif
 
      enddo

      if(iwrite.ge.3)then
        write(*,*)' '
        write(*,*)'PICRB - Warning:'
        write(*,*)'  Simple iteration did not converge.'
        write(*,*)'  MxNorm of first X = ',xmax0
        write(*,*)'  MxNorm of last X =  ',xmax
        write(*,*)'  MxNorm of first FX =',rmax0
        write(*,*)'  MxNorm of last FX = ',rmax
      endif
 
      return
      end
      subroutine picvferb(area,grb,maxcofrb,maxelm,nbcrb,
     &  ncofrb,nelem,nferb,npar,par,phirb,resrb)
c
c***********************************************************************
c
cc PICVFERB computes the finite element portion of the right hand
c  side of the Picard iteration for the reduced Navier Stokes 
c  equations.
c
c  The right hand side is simply the basis solution GFLRB 
c  multiplied by the iteration matrix and negated.  
c  The easiest way to access the solution in GFLRB is to set
c  a temporary copy of GRB to zero, and call UVPQRB.
c
c
c  PICVFERB was modified on 08 August 1996.
c
c
c  AREA   Input, double precision AREA(3,NELEM).
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  GRB    Input, double precision GRB(NCOFRB).
c         GRB contains the reduced basis coefficients of the current 
c         estimate of the state solution.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NCOFRB Input, integer NCOFRB.
c         NCOFRB is the number of basis functions, reduced state equations and 
c         coefficients in the reduced basis system.
c
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c 
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c 
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the 
c         inflow.  NPARF must be at least 1.  
c
c  PAR    Input, double precision PAR(NPAR).
c
c         PAR is the current set of parameter values, including the
c         Reynolds parameter, the flow parameters, and the bump parameters.
c
c  PHIRB  Input, double precision PHIRB(3,NCOFRB,15,NELEM).
c         PHIRB contains the values of a finite element basis function
c         or its X or Y derivative, in a given element, at a given
c         quadrature point, for a particular reduced basis function.
c
c         For PHIRB(I,J,K,L), index J refers to the reduced basis
c         basis functions, for J=0 to NCOFRB.
c
c         The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c           For the quadrature point I, and reduced basis function J,
c           in element L, PHIRB(I,J,K,L) represents the value of:
c
c             K=1, WUrb, the finite element U velocity basis function;
c             K=2, dWUrbdX, the X derivative of WUrb;
c             K=3, dWUrbdY, the Y derivative of WUrb;
c             K=4, WVrb, the finite element V velocity basis function;
c             K=5, dWVrbdX, the X derivative of WVrb;
c             K=6, dWVrbdY, the Y derivative of WVrb;
c             K=7, Q, the finite element pressure basis function.
c             K=8, dQrbdX, the X derivative of Qrb;
c             K=9, dQrbdY, the Y derivative of Qrb.
c             K=10, WU0rb, same as WUrb, with zero BC.
c             K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c             K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c             K=13, WV0rb, same as WVrb, with zero BC.
c             K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c             K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  RESRB  Output, double precision RESRB(NCOFRB).
c         For this routine, RESRB returns the right hand side of
c         the Picard iteration system.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxelm
      integer nelem
      integer ncofrb
      integer npar
c
      double precision ar
      double precision area(3,nelem)
      double precision dpdx
      double precision dpdy
      double precision dprbdx
      double precision dprbdy
      double precision dudx
      double precision dudy
      double precision durbdx
      double precision durbdy
      double precision dvdx
      double precision dvdy
      double precision dvrbdx
      double precision dvrbdy
      double precision dwuidx
      double precision dwuidy
      double precision dwvidx
      double precision dwvidy
      double precision grb(ncofrb)
c
c  FORTRAN 90 local array:
c
      double precision grbtmp(ncofrb)
      integer ielem
      integer icofrb
      integer iquad
      logical leqi
      integer nbcrb
      integer nferb
      double precision p
      double precision prb
      double precision par(npar)
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision resrb(ncofrb)
      double precision reynld
      double precision u
      double precision urb
      double precision v
      double precision vrb
      double precision wui
      double precision wvi
c
      external leqi
c
      reynld=par(npar)

      do icofrb=1,ncofrb
        grbtmp(icofrb)=0.0
      enddo
 
      do icofrb=nbcrb+1,nbcrb+nferb
        resrb(icofrb)=0.0
      enddo
c
c  Approximate the integral by summing over all elements.
c
      do ielem=1,nelem
c
c  Evaluate the integrand at the quadrature points.
c
        do iquad=1,3
 
          ar=area(iquad,ielem)
c
c  Evaluate the full solution GFLRB at which the reduced basis
c  was generated.  This is the implicit "1" coefficient in the
c  set of reduced basis coefficients, which must be multiplied
c  by the Picard coefficients and carried to the right hand side.
c
c  We do this by using GRBTMP, set to 0.
c
          call uvpqrb(dpdx,dpdy,dudx,dudy,dvdx,dvdy,grbtmp,
     &      ielem,iquad,maxcofrb,maxelm,ncofrb,phirb,p,u,v)
c
c  Evaluate the reduced basis solution GRB from the previous iterate.
c
          call uvpqrb(dprbdx,dprbdy,durbdx,durbdy,dvrbdx,dvrbdy,grb,
     &      ielem,iquad,maxcofrb,maxelm,ncofrb,phirb,prb,urb,vrb)
c
c  Now consider each reduced basis function, and retrieve the
c  corresponding values of the U and V basis functions.
c
          do icofrb=nbcrb+1,nbcrb+nferb

            wui   =phirb(iquad,icofrb,1,ielem)
            dwuidx=phirb(iquad,icofrb,2,ielem)
            dwuidy=phirb(iquad,icofrb,3,ielem)

            wvi   =phirb(iquad,icofrb,4,ielem)
            dwvidx=phirb(iquad,icofrb,5,ielem)
            dwvidy=phirb(iquad,icofrb,6,ielem)
c
c  The horizontal velocity equations.
c
            resrb(icofrb)=resrb(icofrb)
     &        -ar*(dudx*dwuidx + dudy*dwuidy
     &        +reynld*(urb*dudx+vrb*dudy+dpdx)*wui)
c
c  The vertical velocity equations.
c
            resrb(icofrb)=resrb(icofrb)
     &        -ar*(dvdx*dwvidx + dvdy*dwvidy
     &        +reynld*(urb*dvdx+vrb*dvdy+dpdy)*wvi )
 
          enddo
        enddo
      enddo
 
      return
      end
      subroutine picvfl(eqn,ifs,indx,neqnfl,np,npar,nparf,par,
     &  region,resfl,splflo,tauflo,xc,xrange,yc,yrange)
c
c***********************************************************************
c
cc PICVFL computes the right hand side for Picard iteration on the
c  full Navier Stokes equations.
c
c
c  The Picard iteration equations have the form:
c
c    Integral
c
c      dU/dx * dW/dx
c    + dU/dy * dW/dy
c    + reynld * (UOLD*dU/dx + VOLD*dU/dy + dP/dx) * W dx dy = 0
c
c    Integral
c
c      dV/dx * dW/dx
c    + dV/dy * dW/dy
c    + reynld * (UOLD*dV/dx + VOLD*dV/dy + dP/dy) * W dx dy = 0
c
c    Integral
c
c      (dU/dx + dV/dy) * Q dx dy = 0
c
c  Here W is a basis function for U and V, and Q is a basis
c  function for P.  UOLD and VOLD are the values of U and V
c  on a previous step of the iteration.
c
c
c  PICVFL was modified on 10 August 1996.
c
c
      implicit double precision (a-h,o-z)
c
      integer neqnfl
      integer np
      integer npar
      integer nparf
c
      character*2 eqn(neqnfl)
      integer i
      integer ifs
      integer ihor
      integer indx(3,np)
      integer ip
      integer iver
      double precision par(npar)
      character*20 region
      double precision resfl(neqnfl)
      double precision splflo(nparf+2)
      double precision tauflo(nparf+2)
      double precision ubc
      double precision vbc
      double precision xc(np)
      double precision xrange
      double precision xval
      double precision yc(np)
      double precision yrange
      double precision yval
c
c  Initialize the right hand side to zero.
c
      do i=1,neqnfl
        resfl(i)=0.0
      enddo
c
      do ip=1,np

        xval=xc(ip)
        yval=yc(ip)

        ihor=indx(1,ip)
        iver=indx(2,ip)

        if(eqn(ihor).eq.'UI'.or.eqn(iver).eq.'VI')then

          call flowbc(ifs,npar,nparf,par,region,splflo,tauflo,
     &      ubc,vbc,xrange,xval,yrange,yval)

          if(eqn(ihor).eq.'UI')then
            resfl(ihor)=ubc
          endif

          if(eqn(iver).eq.'VI')then
            resfl(iver)=vbc
          endif

        endif

      enddo

      return
      end
      subroutine reysen(area,eqn,indx,isen,maxcofrb,maxnfl,nelem,
     &  neqnfl,node,np,phifl,resfl,reynld,senfl)
c
c***********************************************************************
c
cc REYSEN sets up the right hand side RHS associated with the ISEN-th 
c  order sensitivities with respect to the REYNLD parameter of a 
c  given state function (U,V,P).
c
c  In order to compute the right hand side for the ISEN-th order,
c  a state solution and the sensitivities for all orders less than
c  ISEN must already have been computed.
c
c
c  REYSEN was modified on 22 July 1996.
c
c
c  AREA   
c    double precision AREA(3,MAXELM).
c    AREA contains a common factor multiplying the term associated
c    with a quadrature point in a given element, namely,
c
c      AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c    or, if the element is isoperimetric,
c
c      AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c    Here Ar(IELEM) represents the area of element IELEM.
c
c  EQN    
c    character*2 EQN(MAXNFL).
c    EQN records the "type" of each equation that will be generated, and
c    which is associated with an unknown.  
c
c    'U'  A horizontal momentum equation.
c    'UB' The condition U=0 applied at a node on the bump.
c    'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c    'UW' The condition U=0 applied at a node on a fixed wall.
c    'U0' A dummy value of U=0 should be set.
c
c    'V'  A vertical momentum equation.
c    'VB' The condition V=0 applied at a node on the bump.
c    'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c    'VW' The condition V=0 applied at a node on a fixed wall.
c    'V0' A dummy value of V=0 should be set.
c
c    'P'  A continuity equation.
c    'PB' The condition P=0 applied at (XMAX,YMAX).
c    'P0' A dummy value of P=0 should be set.
c
c  INDX   
c    integer INDX(3,NP).
c    INDX(I,J) contains, for each node J, the global index of U,
c    V and P at that node, or 0 or a negative value.  The global
c    index of U, V, or P is the index of the coefficient vector
c    that contains the value of the finite element coefficient
c    associated with the corresponding basis function at the
c    given node.
c
c  ISEN
c    Input, integer ISEN.
c    ISEN is the order of the sensitivity to be calculated.
c
c
c  MAXCOFRB
c    integer MAXCOFRB.
c    MAXCOFRB is the maximum legal value for NCOFRB, the number
c    of coefficients used to specify a particular reduced basis
c    solution.
c
c  MAXNFL 
c    integer MAXNFL.
c    MAXNFL is the maximum number of equations or coefficients allowed
c    for the full system.  MAXNFL must be used instead of NEQNFL as
c    the leading dimension of certain multi-dimensional arrays.
c
c  NELEM  
c    integer NELEM.
c    NELEM is the number of elements.
c    NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NEQNFL 
c    integer NEQNFL.
c    NEQNFL is the number of equations (and coefficients) in the full
c    finite element system.
c
c  NODE   
c    integer NODE(6,MAXELM) or NODE(6,NELEM).
c    NODE(I,J) contains, for an element J, the global index of
c    the node whose local number in J is I.
c
c  NP     
c    integer NP.
c    NP is the number of nodes used to define the finite element mesh.
c    Typically, the mesh is generated as a rectangular array, with
c    an odd number of nodes in the horizontal and vertical directions.
c    The formula for NP is NP=(2*NX-1)*(2*NY-1).
c
c  PHIFL  
c    double precision PHIFL(3,6,10,NELEM).
c    PHIFL contains the value of a finite element basis function, its
c    derivative, or other information, evaluated at the quadrature
c    points (which are the element midside nodes).
c
c    The meaning of the entry PHIFL(I,J,K,L) is as follows.
c    For the quadrature point I, and basis function J, in element L,
c    PHIFL(I,J,K,L) represents the value of:
c
c      K= 1, W, the finite element basis function for velocities;
c      K= 2, dWdX, the X derivative of W;
c      K= 3, dWdY, the Y derivative of W;
c      K= 4, Q, the finite element basis function for pressures;
c      K= 5, dQdX, the X derivative of Q;
c      K= 6, dQdY, the Y derivative of Q;
c      K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c      K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c      K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c      K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c    In particular, PHIFL(I,J,K,L) is the value of the quadratic
c    basis function W associated with local node J in element L,
c    evaluated at quadrature point I.
c
c    Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6,
c    since there are only three linear basis functions.
c
c  RESFL  
c    double precision RESFL(NEQNFL).
c    RESFL contains the residual in the full basis equations.
c
c  REYNLD
c    double precision REYNLD.
c    REYNLD is the value of the Reynolds number.
c
c  SENFL  
c    double precision SENFL(MAXNFL,MAXCOFRB).
c    Columns 1 through NSENFL of SENFL contain the sensitivities 
c    of the full solution with respect to the REYNLD parameter, for
c    orders 0 through NSENFL-1.
c
c    SENFL(I,J) contains the (J-1)-th sensitivity of the I-th full unknown
c    with respect to REYNLD.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxnfl
      integer nelem
      integer neqnfl
      integer np
c
      double precision ar
      double precision area(3,nelem)
c
c  Note: These are FORTRAN 90 local arrays!
c
      double precision dpdx(maxcofrb)
      double precision dpdy(maxcofrb)
      double precision dudx(maxcofrb)
      double precision dudy(maxcofrb)
      double precision dvdx(maxcofrb)
      double precision dvdy(maxcofrb)
 
      character*2 eqn(neqnfl)
      integer i
      integer ielem
      integer ihor
      integer indx(3,np)
      integer ip
      integer iq
      integer iquad
      integer isen
      integer iver
      integer jsen
      integer jsendx
      integer nbinom
      integer node(6,nelem)
      double precision p(maxcofrb)
      double precision phifl(3,6,10,nelem)
      double precision resfl(neqnfl)
      double precision reynld
      double precision senfl(maxnfl,maxcofrb)
      double precision term
      double precision u(maxcofrb)
      double precision v(maxcofrb)
      double precision wi
c
c  Check the value of REYNLD.
c
      if(reynld.le.0.0)then
        write(*,*)' '
        write(*,*)'REYSEN - Fatal error!'
        write(*,*)'  Nonpositive value of REYNLD=',reynld
        stop
      endif
c
c  Check the value of ISEN.
c
      if(isen.le.0)then
        write(*,*)' '
        write(*,*)'REYSEN - Fatal error!'
        write(*,*)'  The input value of ISEN is ',isen
        write(*,*)'  but ISEN must be strictly positive.'
        stop
      endif
c
      if(isen.gt.maxcofrb-1)then
        write(*,*)' '
        write(*,*)'REYSEN - Fatal error!'
        write(*,*)'  The input value of ISEN is ',isen
        write(*,*)'  but the limit is MAXCOFRB-1=',maxcofrb-1
        stop
      endif
c
c  Zero out the right hand side vector RESFL.
c
      do i=1,neqnfl
        resfl(i)=0.0
      enddo
c
c  Approximate the integrated ISEN-th order sensitivity equations by 
c  adding the contribution from element IELEM.
c
      do ielem=1,nelem
c
c  In element IELEM, approximate the integrals by moving to
c  quadrature point IQUAD.  
c
        do iquad=1,3
 
          ar=area(iquad,ielem)
c
c  We are about to compute the sensitivity of order ISEN, which
c  is stored in vector entry ISEN+1, or matrix column ISEN+1.
c  Evaluate the fundamental solution (U,V,P), and the first ISEN-1
c  sensitivities, (U',V',P'), (U'',V'',P''), and so on, stored in SENFL.
c
          do jsen=0,isen-1
            jsendx=jsen+1
            call uvpqfl(dpdx(jsendx),dpdy(jsendx),dudx(jsendx),
     &        dudy(jsendx),dvdx(jsendx),dvdy(jsendx),
     &        senfl(1,jsendx),ielem,indx,iquad,nelem,neqnfl,node,
     &        np,p(jsendx),phifl,u(jsendx),v(jsendx))
          enddo
c
c  Now consider a node with local index IQ, and global index IP,
c  whose quadratic basis function evaluated at the quadrature point
c  IQUAD has value WI.  Evaluate the right hand sides of equations
c  IHOR and IVER and add the contributions to the total.
c
          do iq=1,6
 
            ip=node(iq,ielem)
            wi=phifl(iquad,iq,1,ielem)
            ihor=indx(1,ip)
            iver=indx(2,ip)

            if(eqn(ihor).eq.'U')then
 
              term=0.0
 
              do jsen=1,isen-1
                jsendx=jsen+1
                term=term+reynld*nbinom(isen,jsen)*
     &            (u(isen-jsen+1)*dudx(jsendx)
     &            +v(isen-jsen+1)*dudy(jsendx))
              enddo
 
              do jsen=0,isen-1
                jsendx=jsen+1
                term=term+isen*nbinom(isen-1,jsen)*
     &            (u(isen-jsen)*dudx(jsendx)
     &            +v(isen-jsen)*dudy(jsendx))
              enddo
 
              term=term+isen*dpdx(isen)

              resfl(ihor)=resfl(ihor)-ar*term*wi

            endif
 
c
c  Note that the vertical right hand side should be obtainable
c  from the horizontal right hand side by interchanging U and V,
c  and X and Y.
c
            if(eqn(iver).eq.'V')then
 
              term=0.0
 
              do jsen=1,isen-1
                jsendx=jsen+1
                term=term+reynld*nbinom(isen,jsen)*
     &            (v(isen-jsen+1)*dvdy(jsendx)
     &            +u(isen-jsen+1)*dvdx(jsendx))
              enddo
 
              do jsen=0,isen-1
                jsendx=jsen+1
                term=term+isen*nbinom(isen-1,jsen)*
     &            (v(isen-jsen)*dvdy(jsendx)
     &            +u(isen-jsen)*dvdx(jsendx))
              enddo
 
              term=term+isen*dpdy(isen)

              resfl(iver)=resfl(iver)-ar*term*wi

            endif
 
          enddo
        enddo
      enddo
 
      return
      end
      subroutine target(cost0,gfl,gfltar,indx,maxnfl,maxny,maxparb, 
     &  neqnfl,np,npar,nparb,nprof,ny,par,partar,splbmp,
     &  taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)
c
c*********************************************************************72
c
cc TARGET is called to save the current parameters and solution
c  as the "target solution".
c
c
c  TARGET was modified on 08 August 1996.
c
c
c  COST0  Output, double precision COST0.
c         COST0 is the cost of the solution with PAR = GFL=0.
c
c  GFL    Input, double precision GFL(NEQNFL).
c         GFL contains the current solution estimate for the full problem,
c         containing the pressure and velocity coefficients.
c         The vector INDX must be used to index this data.
c
c  GFLTAR Output, double precision GFLTAR(NEQNFL).
c         GFLTAR is a target solution, used to generate data that defines
c         the cost functional.  The corresponding parameters are PARTAR.
c
c  INDX   Input, integer INDX(3,NP). 
c         INDX(I,J) contains, for each node J, the global index of U,
c         V and P at that node, or 0 or a negative value.  The global
c         index of U, V, or P is the index of the coefficient vector
c         that contains the value of the finite element coefficient
c         associated with the corresponding basis function at the
c         given node.
c
c  MAXNFL Input, integer MAXNFL, the maximum number of equations in the
c         full system.
c
c  MAXNY  Input, integer MAXNY.
c         MAXNY is the maximum size of NY that the program can handle.
c
c  MAXPARB
c         Input, integer MAXPARB.
c         The maximum number of bump parameters allowed.
c
c  NEQNFL Input, integer NEQNFL, the number of finite element equations used
c         to define the horizontal and vertical velocities and the
c         pressure.
c
c  NP     Input, integer NP, the number of nodes used to define the finite
c         element mesh.  NP = (2*NX-1)*(2*NY-1).
c
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and
c         shape of the bump.
c
c         Note that if NPARB = 0, the bump is replaced by a flat wall.
c
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the
c         inflow.  NPARF must be at least 1. 
c
c  NY     Input, integer NY, the number of nodes in each column.
c
c  PAR    Input, double precision PAR(NPAR).
c
c         PAR is the current set of parameter values, including the
c         Reynolds parameter, the flow parameters, and the bump parameters.
c
c  PARTAR Output, double precision PARTAR(NPAR).
c         PARTAR is the value of the parameters that generated the
c         target solution contained in GFLTAR.
c
c  SPLBMP Input, double precision SPLBMP(NPARB+2).
c         SPLBMP contains the spline coefficients for the bump.
c
c  TAUBMP Input, double precision TAUBMP(NPARB+2).
c         TAUBMP contains the location of the spline abscissas for
c         the bump.  There are NPARB+2 of them, because the end values
c         of the spline are constrained to have particular values.
c
c  WATEB  Input, double precision WATEB.
c         WATEB is the multiplier of the bump control cost used
c         when computing the total cost.
c
c  WATEP,
c  WATEU,
c  WATEV  Input, double precision WATEP, WATEU, WATEV.
c         WATEP, WATEU and WATEV are weights used in computing the 
c         cost function based on the costs of the flow discrepancy.
c
c  XBL    Input, double precision XBL.
c         XBL is the X coordinate of the left corner of the bump.
c
c  XBR    Input, double precision XBR.
c         XBR is the X coordinate of the right corner of the bump.
c
c  YBL    Input, double precision YBL.
c         The Y coordinate of the left corner of the bump.
c
c  YBR    Input, double precision YBR.
c         YBR is the Y coordinate of the right corner of the bump.
c
c  YC     Input, double precision YC(NP).
c         YC contains the Y coordinates of the nodes.
c
      implicit none

      integer maxnfl
      integer maxny
      integer maxparb
      integer np
      integer npar
c
c  Set parameters that are dependent on parameters that are dependent
c  on parameters.
c
      double precision cost
      double precision cost0
      double precision costb
      double precision costp
      double precision costu
      double precision costv
      double precision gfl(maxnfl)
      double precision gfltar(maxnfl)
      integer i
      integer indx(3,np)
      integer neqnfl
      integer nparb
      integer nprof(2*maxny-1)
      integer ny
      double precision par(npar)
      double precision partar(npar)
      double precision splbmp(maxparb+2)
      double precision taubmp(maxparb+2)
      double precision wateb
      double precision watep
      double precision wateu
      double precision watev
      double precision xbl
      double precision xbr
      double precision ybl
      double precision ybr
      double precision yc(np)

      do i = 1,npar
        partar(i) = 0.0D+00
      end do

      do i = 1,neqnfl
        gfltar(i) = 0.0D+00
      end do

      call getcst(cost,costb,costp,costu,costv,gfl,gfltar, 
     &  indx,neqnfl,np,nparb,nprof,ny,splbmp, 
     &  taubmp,wateb,watep,wateu,watev,xbl,xbr,ybl,ybr,yc)

      cost0 = cost
      write ( *, * ) ' '
      write ( *, * ) '"Cost" of target GFLTAR versus zero GFL:',cost0

      do i = 1,npar
        partar(i) = par(i)
      end do

      do i = 1,neqnfl
        gfltar(i) = gfl(i)
      end do

      return
      end
      subroutine test2(gfl,grb,ihi,ilo,indx,maxcofrb,maxelm,ncofrb,
     &  nelem,neqnfl,node,np,phifl,phirb)
c
c***********************************************************************
c
cc TEST2 compares U, V, and P for the full solution and the reduced
c  solution at the quadrature points.
c
c
c  TEST2 was dashed off on 09 July 1996.
c
      implicit double precision (a-h,o-z)

      integer maxcofrb
      integer maxelm
      integer nelem
      integer neqnfl
      integer ncofrb
      integer np

      double precision dpdx
      double precision dpdy
      double precision dprbdx
      double precision dprbdy
      double precision dudx
      double precision dudy
      double precision durbdx
      double precision durbdy
      double precision dvdx
      double precision dvdy
      double precision dvrbdx
      double precision dvrbdy
      double precision gfl(neqnfl)
      double precision grb(ncofrb)
      integer ielem
      integer ihi
      integer ihi2
      integer ilo
      integer ilo2
      integer indx(3,np)
      integer iquad
      integer node(6,nelem)
      double precision p
      double precision prb
      double precision phifl(3,6,10,nelem)
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision u
      double precision urb
      double precision v
      double precision vrb
c
      if(ilo.lt.1)then
        ilo2=1
      else
        ilo2=ilo
      endif
 
      if(ihi.gt.nelem)then
        ihi2=nelem
      else
        ihi2=ihi
      endif
c
c  Consider an element IELEM...
c
      write(*,*)' '
      write(*,*)'Elements from ILO2=',ilo2,' to IHI2=',ihi2
 
      do ielem=ilo2,ihi2
c
c  ...and a quadrature point IQUAD...
c
        do iquad=1,3
c
c  Evaluate U, V, and P for GFL and for GRB.
c
          call uvpqfl(dpdx,dpdy,dudx,dudy,dvdx,dvdy,gfl,ielem,indx,
     &      iquad,nelem,neqnfl,node,np,p,phifl,u,v)
 
          call uvpqrb(dprbdx,dprbdy,durbdx,durbdy,dvrbdx,dvrbdy,grb,
     &      ielem,iquad,maxcofrb,maxelm,ncofrb,phirb,prb,urb,vrb)
 
          write(*,*)' '
          write(*,*)'Element ',ielem,' Quad point ',iquad
          write(*,'(a,3g14.6)')'Full U,V,P:   ',u,v,p
          write(*,'(a,3g14.6)')'Reduced U,V,P:',urb,vrb,prb
 
        enddo
      enddo
 
      return
      end
      subroutine test3(maxcofrb,maxnfl,ncofrb,neqnfl,rb,senfl,
     &  senrb)
c 
c***********************************************************************
c
cc TEST3 verifies that RB*RFACT=SENFL
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxnfl
      integer ncofrb
      integer neqnfl
c
      double precision dmax
      integer i
      integer imax
      integer j
      integer jmax
      integer k
      double precision rb(maxnfl,ncofrb)
      double precision senfl(maxnfl,maxcofrb)
      double precision senrb(maxcofrb,maxcofrb)
      double precision temp
c
      dmax=0.0
      imax=0
      jmax=0
 
      do i=1,neqnfl
        do j=1,ncofrb
          temp=0.0
          do k=1,ncofrb
            temp=temp+rb(i,k)*senrb(k,j)
          enddo
          temp=abs(temp-senfl(i,j))
          if(temp.ge.dmax)then
            dmax=temp
            imax=i
            jmax=j
          endif
        enddo
      enddo
 
      write(*,*)' '
      write(*,*)'TEST3 - Results:'
      write(*,*)'  The maximum difference between RB*SENRB and SENFL'
      write(*,*)'  is ',dmax,' for (I,J)=',imax,jmax
 
      return
      end
      subroutine test4(afl,area,difcof,dpar,drey,eqn,gfl,gflafl,
     &  ifs,ijac,indx,ipar,ipivfl,iwrite,ldafl,maxcofrb,maxelm,
     &  maxnew,maxnfl,ncofrb,nelem,neqnfl,nlband,node,np,npar,
     &  nparb,nparf,nsenfl,par,parafl,phifl,region,resfl,
     &  senfl,splflo,tauflo,tolnew,xrange,yrange)
c
c***********************************************************************
c
cc TEST4
c
      implicit double precision (a-h,o-z)
c
      integer ldafl
      integer maxcofrb
      integer maxelm
      integer maxnfl
      integer nelem
      integer neqnfl
      integer np
      integer npar
      integer nparf
c
      double precision afl(ldafl,maxnfl)
      double precision area(3,nelem)
      double precision difcof(maxcofrb)
      double precision difmax
      double precision dpar
      double precision drey
      character*2 eqn(neqnfl)
      double precision gfl(neqnfl)
      double precision gflafl(neqnfl)
      integer i
      integer ifs
      integer ijac
      integer imax
      integer indx(3,np)
      integer ipar
      integer ipivfl(maxnfl)
      integer iwrite
      integer j
      integer jmax
      integer maxnew
      integer ncofrb
      integer nlband
      integer node(6,nelem)
      integer nparb
      integer nsenfl
      double precision par(npar)
      double precision parafl(npar)
      double precision phifl(3,6,10,nelem)
      character*20 region
      double precision resfl(neqnfl)
      double precision senfl(maxnfl,maxcofrb)
c
c  FORTRAN 90 local array.
c
      double precision senfltmp(maxnfl,maxcofrb)
      double precision splflo(nparf+2)
      double precision tauflo(nparf+2)
      double precision tolnew
      double precision xrange
      double precision yrange
c
      call getsenfl(afl,area,eqn,gfl,indx,ipivfl,ldafl,
     &  maxcofrb,maxnfl,nelem,neqnfl,nlband,node,np,npar,
     &  nsenfl,par,phifl,resfl,senfl)
 
      do i=1,neqnfl
        do j=1,ncofrb
          senfltmp(i,j)=senfl(i,j)
        enddo
      enddo
 
      dpar=drey
 
      call difsenfl(afl,area,difcof,dpar,eqn,gfl,gflafl,ifs,
     &  ijac,indx,ipar,ipivfl,iwrite,ldafl,maxcofrb,maxelm,
     &  maxnew,maxnfl,
     &  ncofrb,nelem,neqnfl,nlband,node,np,npar,nparb,nparf,
     &  par,parafl,phifl,region,resfl,senfl,splflo,tauflo,
     &  tolnew,xrange,yrange)
 
      imax=0
      jmax=0
      difmax=0.0
 
      do i=1,neqnfl
        do j=1,ncofrb
          if(abs(senfltmp(i,j)-senfl(i,j)).ge.difmax)then
            imax=i
            jmax=j
            difmax=abs(senfltmp(i,j)-senfl(i,j))
          endif
        enddo
      enddo
 
      write(*,*)' '
      write(*,*)'TEST4 - Results:'
      write(*,*)'  MAXIMUM DIFFERENCE is ',difmax
      write(*,*)'  I=',imax,' j=',jmax
 
      return
      end
      subroutine test5(maxcofrb,maxnfl,ncofrb,neqnfl,rb,rbase)
c
c***********************************************************************
c
cc TEST5 is given the QR factors of the reduced basis matrix, and 
c  multiplies them out.
c
c
c  TEST5 was written on 30 July 1996.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxnfl
      integer ncofrb
c
      integer i
      integer ihi
      integer ilo
      integer j
      integer jhi
      integer jlo
      integer k
      integer mhi
      integer mlo
      integer neqnfl
      integer nhi
      integer nlo
      double precision rb(maxnfl,ncofrb)
      double precision rbase(maxcofrb,maxcofrb)
      double precision test(10,5)
c
      ilo=1
      ihi=min(10,neqnfl)
      jlo=1
      jhi=min(5,ncofrb)

      do i=1,ihi
        do j=1,jhi
          test(i,j)=0.0
          do k=1,ncofrb
            test(i,j)=test(i,j)+rb(i,k)*rbase(k,j)
          enddo
        enddo
      enddo
 
      mhi=ihi
      mlo=ilo
      nhi=jhi
      nlo=jlo

      call prdmat(test,ihi,ilo,jhi,jlo,mhi,mlo,nhi,nlo)

      return
      end
      subroutine pruvpfl(gfl,indx,neqnfl,np,xc,xmax,xmin,yc,ymax,ymin)
c
c***********************************************************************
c
cc PRUVPFL prints the values of velocity and pressure for all nodes
c  within the user defined box (XMIN,YMIN) to (XMAX,YMAX).
c
c
c  PRUVPFL was modified on 11 August 1996.
c
c
c  GFL    
c    double precision GFL(NEQNFL).
c    GFL contains the current solution estimate for the full problem,
c    containing the pressure and velocity coefficients.
c    The vector INDX must be used to index this data.
c
c  INDX   
c    integer INDX(3,NP).
c    INDX(I,J) contains, for each node J, the global index of U,
c    V and P at that node, or 0 or a negative value.  The global
c    index of U, V, or P is the index of the coefficient vector
c    that contains the value of the finite element coefficient
c    associated with the corresponding basis function at the
c    given node.
c
c  NEQNFL 
c    integer NEQNFL.
c    NEQNFL is the number of equations (and coefficients) in the full
c    finite element system.
c
c  NP     
c    integer NP.
c    NP is the number of nodes used to define the finite element mesh.
c    Typically, the mesh is generated as a rectangular array, with
c    an odd number of nodes in the horizontal and vertical directions.
c    The formula for NP is NP=(2*NX-1)*(2*NY-1).
c
c  XC     
c    double precision XC(NP).
c    XC contains the X coordinates of the nodes.
c
c  XMAX 
c    double precision XMAX.
c    The maximum X for which a node should be displayed.
c
c  XMIN
c    double precision XMIN.
c    The mininum X for which a node should be displayed.
c
c  YC     
c    double precision YC(NP).
c    YC contains the Y coordinates of the nodes.
c
c  YMAX
c    double precision YMAX.
c    The maximum Y for which a node should be displayed.
c
c  YMIN
c    double precision YMIN.
c    The minimum Y for which a node should be displayed.
c
      implicit double precision (a-h,o-z)
c
      integer neqnfl
      integer np
c
      double precision gfl(neqnfl)
      integer i
      integer i1
      integer i2
      integer i3
      integer indx(3,np)
      double precision xc(np)
      double precision xmax
      double precision xmin
      double precision yc(np)
      double precision ymax
      double precision ymin
c
      write(*,*)' '
      write(*,*)'PRUVPFL - Print selected flow data'
      write(*,*)xmin,' = XMIN <= X <= XMAX = ',xmax
      write(*,*)ymin,' = YMIN <= Y <= YMAX = ',ymax
      write(*,*)' '
 
      do i=1,np
        if(xmin.le.xc(i).and.xc(i).le.xmax.and.
     &     ymin.le.yc(i).and.yc(i).le.ymax)then
 
          i1=indx(1,i)
          i2=indx(2,i)
          i3=indx(3,i)
 
          if(i3.gt.0)then
            write(*,'(i5,2g12.4,3g14.6)')i,xc(i),yc(i),gfl(i1),gfl(i2),
     &        gfl(i3)
          else
            write(*,'(i5,2g12.4,2g14.6)')i,xc(i),yc(i),gfl(i1),gfl(i2)
          endif

        endif
      enddo
 
      return
      end
      subroutine pruvprb(grb,indx,maxnfl,ncofrb,nelem,node,nodelm,
     &  np,rb,xc,xmax,xmin,yc,ymax,ymin)
c
c***********************************************************************
c
cc PRUVPRB prints the values of the reduced velocity and pressure 
c  for all nodes within the user defined box (XMIN,YMIN) to (XMAX,YMAX).
c
c
c  PRUVPRB was modified on 11 August 1996.
c
c
c  GRB    
c    double precision GRB(NCOFRB).
c    GRB contains the reduced basis coefficients of the current
c    estimate of the state solution.
c
c  INDX   
c    integer INDX(3,NP).
c    INDX(I,J) contains, for each node J, the global index of U,
c    V and P at that node, or 0 or a negative value.  The global
c    index of U, V, or P is the index of the coefficient vector
c    that contains the value of the finite element coefficient
c    associated with the corresponding basis function at the
c    given node.
c
c  MAXNFL 
c    integer MAXNFL.
c    MAXNFL is the maximum number of equations or coefficients allowed
c    for the full system.  MAXNFL must be used instead of NEQNFL as
c    the leading dimension of certain multi-dimensional arrays.
c
c  NCOFRB 
c    integer NCOFRB.
c    NCOFRB is the number of coefficients needed to determine
c    a particular reduced basis function.
c    NCOFRB is the sum of NBCRB and NFERB.
c
c  NELEM  
c    integer NELEM.
c    NELEM is the number of elements.
c    NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NODE   
c    integer NODE(6,MAXELM) or NODE(6,NELEM).
c    NODE(I,J) contains, for an element J, the global index of
c    the node whose local number in J is I.
c
c  NP     
c    integer NP.
c    NP is the number of nodes used to define the finite element mesh.
c    Typically, the mesh is generated as a rectangular array, with
c    an odd number of nodes in the horizontal and vertical directions.
c    The formula for NP is NP=(2*NX-1)*(2*NY-1).
c
c  RB     
c    double precision RB(MAXNFL,MAXCOFRB).
c
c    RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
c    RB is generated by computing a finite element solution GFL,
c    which is saved for later reference as "GFLRB".
c    GFLRB is copied into the first column of RB.
c    Then, we compute the first NCOFRB derivatives of GFLRB with
c    respect to a parameter.  The first derivative
c    is stored in column 1 of RB, and so on.  
c
c  XC     
c    double precision XC(NP).
c    XC contains the X coordinates of the nodes.
c
c  XMAX 
c    double precision XMAX.
c    The maximum X for which a node should be displayed.
c
c  XMIN
c    double precision XMIN.
c    The mininum X for which a node should be displayed.
c
c  YC     
c    double precision YC(NP).
c    YC contains the Y coordinates of the nodes.
c
c  YMAX
c    double precision YMAX.
c    The maximum Y for which a node should be displayed.
c
c  YMIN
c    double precision YMIN.
c    The minimum Y for which a node should be displayed.
c
      implicit double precision (a-h,o-z)
c
      integer maxnfl
      integer ncofrb
      integer nelem
      integer np
c
      double precision grb(ncofrb)
      integer ip
      integer ielem
      integer indx(3,np)
      integer node(6,nelem)
      integer nodelm(np)
      double precision prb
      double precision rb(maxnfl,ncofrb)
      double precision urb
      double precision vrb
      double precision xc(np)
      double precision xmax
      double precision xmin
      double precision xval
      double precision yc(np)
      double precision ymax
      double precision ymin
      double precision yval
c
      write(*,*)' '
      write(*,*)'PRUVPRB - Print selected flow data'
      write(*,*)xmin,' = XMIN <= X <= XMAX = ',xmax
      write(*,*)ymin,' = YMIN <= Y <= YMAX = ',ymax
      write(*,*)' '
      write(*,*)'  IP   XC(IP)      YC(IP)      U(IP), V(IP), P(IP)'
      write(*,*)' '

      do ip=1,np

        if(xmin.le.xc(ip).and.xc(ip).le.xmax.and.
     &     ymin.le.yc(ip).and.yc(ip).le.ymax)then
 
          ielem=nodelm(ip)
          xval=xc(ip)
          yval=yc(ip)
 
          call uvprb(grb,ielem,indx,maxnfl,ncofrb,nelem,node,np,prb,
     &      rb,urb,vrb,xc,xval,yc,yval)

          write(*,'(i5,2g12.4,3g14.6)')ip,xc(ip),yc(ip),urb,vrb,prb

        endif
 
      enddo
 
      return
      end
      subroutine uvprb(grb,ielem,indx,maxnfl,ncofrb,nelem,node,np,prb,
     &  rb,urb,vrb,xc,xval,yc,yval)
c
c***********************************************************************
c
cc UVPRB is given a set of reduced coefficients, an element IELEM,
c  and a point XVAL, YVAL, and returns the values of the velocity and 
c  pressure at that point.
c
c
c  UVPRB was modified on 11 August 1996.
c
c
c  GRB    
c    double precision GRB(NCOFRB).
c    GRB contains the reduced basis coefficients of the current
c    estimate of the state solution.
c
c  IELEM
c    integer IELEM.
c    IELEM is the element in which the point (XVAL,YVAL) lies.
c
c  INDX   
c    integer INDX(3,NP).
c    INDX(I,J) contains, for each node J, the global index of U,
c    V and P at that node, or 0 or a negative value.  The global
c    index of U, V, or P is the index of the coefficient vector
c    that contains the value of the finite element coefficient
c    associated with the corresponding basis function at the
c    given node.
c
c  MAXNFL 
c    integer MAXNFL.
c    MAXNFL is the maximum number of equations or coefficients allowed
c    for the full system.  MAXNFL must be used instead of NEQNFL as
c    the leading dimension of certain multi-dimensional arrays.
c
c  NCOFRB 
c    integer NCOFRB.
c    NCOFRB is the number of coefficients needed to determine
c    a particular reduced basis function.
c    NCOFRB is the sum of NBCRB and NFERB.
c
c  NELEM  
c    integer NELEM.
c    NELEM is the number of elements.
c    NELEM can be determined as 2*(NX-1)*(NY-1).
c
c  NODE   
c    integer NODE(6,MAXELM) or NODE(6,NELEM).
c    NODE(I,J) contains, for an element J, the global index of
c    the node whose local number in J is I.
c
c  NP     
c    integer NP.
c    NP is the number of nodes used to define the finite element mesh.
c    Typically, the mesh is generated as a rectangular array, with
c    an odd number of nodes in the horizontal and vertical directions.
c    The formula for NP is NP=(2*NX-1)*(2*NY-1).
c
c  PRB
c    Output, double precision PRB.
c    PRB is the value of the reduced pressure at (XVAL,YVAL).
c
c  RB     
c    double precision RB(MAXNFL,MAXCOFRB).
c
c    RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
c    RB is generated by computing a finite element solution GFL,
c    which is saved for later reference as "GFLRB".
c    GFLRB is copied into the first column of RB.
c    Then, we compute the first NCOFRB derivatives of GFLRB with
c    respect to a parameter.  The first derivative
c    is stored in column 1 of RB, and so on.  
c
c  URB
c    Output, double precision URB.
c    URB is the value of the reduced horizontal velocity at (XVAL,YVAL).
c
c  VRB
c    Output, double precision VRB.
c    VRB is the value of the reduced vertical velocity at (XVAL,YVAL).
c
c  XC     
c    double precision XC(NP).
c    XC contains the X coordinates of the nodes.
c
c  XVAL
c    Input, double precision XVAL.
c    XVAL is the X coordinate of the point at which the reduced
c    solution values are desired.
c
c  YC     
c    double precision YC(NP).
c    YC contains the Y coordinates of the nodes.
c
c  YVAL
c    Input, double precision YVAL.
c    YVAL is the Y coordinate of the point at which the reduced
c    solution values are desired.
c
      implicit double precision (a-h,o-z)
c
      integer maxnfl
      integer ncofrb
      integer nelem
      integer np
c
      double precision dqdx
      double precision dqdy
      double precision dwdx
      double precision dwdy
      double precision grb(ncofrb)
      integer i
      integer i1
      integer i2
      integer i3
      integer ielem
      integer indx(3,np)
      integer ip
      integer j
      integer node(6,nelem)
      double precision pfl
      double precision prb
      double precision q
      double precision rb(maxnfl,ncofrb)
      double precision ufl
      double precision urb
      double precision vfl
      double precision vrb
      double precision w
      double precision xc(np)
      double precision xval
      double precision yc(np)
      double precision yval
c
 
      urb=0.0
      vrb=0.0
      prb=0.0
 
      do i=1,ncofrb
 
        ufl=0.0
        vfl=0.0
        pfl=0.0
 
        do j=1,6
 
          ip=node(j,ielem)
 
          call qbf(ielem,j,w,dwdx,dwdy,nelem,node,np,xc,xval,yc,yval)
 
          i1=indx(1,ip)
          ufl=ufl+rb(i1,i)*w
 
          i2=indx(2,ip)
          vfl=vfl+rb(i2,i)*w
 
          i3=indx(3,ip)
 
          if(i3.gt.0)then 
            call bsp(q,dqdx,dqdy,ielem,j,nelem,node,np,xc,xval,yc,yval)
            pfl=pfl+rb(i3,i)*q
          endif
 
        enddo
 
        urb=urb+grb(i)*ufl
        vrb=vrb+grb(i)*vfl
        prb=prb+grb(i)*pfl
 
      enddo
 
      return
      end
      subroutine bmpcst(costb,nparb,splbmp,taubmp,xbl,xbr,ybl,ybr)
c
c***********************************************************************
c
cc BMPCST evaluates the cost of the bump control.
c
c
c  The bump connects the points (XBL,YBL) and (XBR,YBR).
c
c  Compute its "cost" by comparing its slope to the slope of the
c  straight line that connects those two points.
c
c    COSTB = Integral (XBL <= X <= XBR) (Bump'(X) - Line'(X))**2 dX
c
c  Here, Bump(X) represents the function describing the shape
c  of the bump, and Line(X) represents the straight line which
c  simply joins the two endpoints, (XBL,YBL) and (XBR,YBR).
c
c  This integral is approximated by numerical integration.
c
c  The interval between XBL and XBR is divided into NPARB+1
c  intervals, over each of which the bump's height is described
c  by a spline.
c
c  For each such interval, pick NQUAD1 quadrature points,
c  evaluate the derivative of the bump function there, and
c  subtract the slope of the straight line.
c
c
c  COSTB  Output, double precision COSTB.
c
c         COSTB is the integral of the difference of the
c         derivatives of the straight line joining the two straight line
c         line segments of the bottom, and the bump that is
c         actually drawn there.
c
c         This measures the cost of bump control.
c
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  SPLBMP Input, double precision SPLBMP(NPARB+2).
c
c         SPLBMP contains the spline coefficients for the bump.
c
c  TAUBMP Input, double precision TAUBMP(NPARB+2).
c
c         TAUBMP contains the location of the spline abscissas for
c         the bump.  There are NPARB+2 of them, because the end values
c         of the spline are constrained to have particular values.
c
c  XBL    Input, double precision XBL, the X coordinate of the left corner 
c         of the bump.
c
c  XBR    Input, double precision XBR, the X coordinate of the right corner 
c         of the bump.
c
c  YBL    Input, double precision YBL, the Y coordinate of the left corner 
c         of the bump.
c
c  YBR    Input, double precision YBR, the Y coordinate of the right corner 
c         of the bump.
c
      implicit double precision (a-h,o-z)
c
      integer nparb
c
      integer nquad1
      parameter (nquad1=5)
c
      double precision costb
      double precision cprime
      integer i
      integer j
      double precision slope
      double precision splbmp(nparb+2)
      double precision taubmp(nparb+2)
      double precision wquad1(nquad1)
      double precision xbl
      double precision xbr
      double precision xleft
      double precision xsiquad(nquad1)
      double precision xrite
      double precision xx
      double precision ybl
      double precision ybr
c
      costb=0.0
 
      if(nparb.eq.0)return
 
      if(xbl.ge.xbr)return
c
c  Get the Gauss weights and abscissas for one dimensional quadrature.
c
      call gquad1(nquad1,wquad1,xsiquad)
c
c  Get the slope of the line joining the endpoints of the bump.
c
      slope=(ybr-ybl)/(xbr-xbl)
c
c  Estimate the integral of the square of the difference between
c  the slope of the line and the slope of the bump over the
c  bump interval.
c
      do i=1,nparb+1
 
        xleft=(dble(nparb+2-i)*xbl+dble(i-1)*xbr)/dble(nparb+1)
        xrite=(dble(nparb+1-i)*xbl+dble(i)*xbr)/dble(nparb+1)
 
        do j=1,nquad1
 
          xx=0.5*((1.0+xsiquad(j))*xrite+(1.0-xsiquad(j))*xleft)
 
          call pqdx(nparb+2,xx,taubmp,cprime,splbmp)
 
          costb=costb
     &      +0.5*wquad1(j)*(xrite-xleft)*(cprime-slope)**2
 
        enddo
 
      enddo
 
      return
      end
      subroutine bmpspl(npar,nparb,nparf,par,splbmp,taubmp,xbl,xbr,
     &  ybl,ybr)
c
c***********************************************************************
c
cc BMPSPL sets up or updates the spline data that describes the bump.
c
c  It does this for the target parameters and the feasible parameters.
c
c
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c 
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c 
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the 
c         inflow.  NPARF must be at least 1.  
c
c  PAR    Input, double precision PAR(NPAR).
c 
c         PAR is the current estimate for the parameters.
c
c  SPLBMP Output, double precision SPLBMP(NPARB+2).
c
c         SPLBMP contains the spline coefficients for the bump.
c
c  TAUBMP Output, double precision TAUBMP(NPARB+2).
c
c         TAUBMP contains the location of the spline abscissas for
c         the bump.  There are NPARB+2 of them, because the end values
c         of the spline are constrained to have particular values.
c
c  XBL    Input, double precision XBL, the X coordinate of the left corner 
c         of the bump.
c
c  XBR    Input, double precision XBR, the X coordinate of the right corner 
c         of the bump.
c
c  YBL    Input, double precision YBL, the Y coordinate of the left corner 
c         of the bump.
c
c  YBR    Input, double precision YBR, the Y coordinate of the right corner 
c         of the bump.
c
      implicit double precision (a-h,o-z)
c
      integer npar
      integer nparb
      integer nparf
c
      integer i
      double precision par(npar)
      double precision splbmp(nparb+2)
      double precision taubmp(nparb+2)
      double precision xbl
      double precision xbr
      double precision ybl
      double precision ybr
c
      if(nparb.le.0)return
c
c  Set up the bump arrays, including:
c
c    TAUBMP, containing the abscissas, which never change,
c    SPLBMP(I), the location of the bump at abscissa I.
c
      do i=1,nparb+2
        taubmp(i)=((nparb+2-i)*xbl+(i-1)*xbr)/dble(nparb+1)
      enddo
c
c  Watch out!  The indexing of SPLBMP here is technically illegal.
c
        splbmp(1)=ybl
        do i=2,nparb+1
          splbmp(i)=par(nparf+i-1)
        enddo
        splbmp(nparb+2)=ybr
 
      return
      end
      subroutine bsp(q,dqdx,dqdy,ielem,iq,nelem,node,np,xc,xq,yc,yq)
c
c***********************************************************************
c
cc BSP computes the value and spatial derivatives of the linear basis
c  functions associated with pressure.
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
c
c  Q      Output, double precision Q, the value of the IQ-th basis
c         function at the point with global coordinates (XQ,YQ).
c
c  DQDX,
c  DQDY   Output, double precision DQDX, DQDY, the X and Y
c         derivatives of the IQ-th basis function at the point
c         with global coordinates (XQ,YQ).
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
c  NODE   Input, integer NODE(6,NELEM).  NODE(J,I) is
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
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer np
c
      double precision q
      double precision dqdx
      double precision dqdy
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
        write(*,*)'BSP - Fatal error!'
        write(*,*)'  The requested basis function is IQ=',iq
        write(*,*)'  but only values from 1 to 6 are legal.'
        stop
      elseif(iq.ge.4.and.iq.le.6)then
        q=0.0
        dqdx=0.0
        dqdy=0.0
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
 
      dqdx=(yc(i2)-yc(i3))/d
      dqdy=(xc(i3)-xc(i2))/d
 
      q=1.0 + dqdx*(xq-xc(i1)) + dqdy*(yq-yc(i1))
 
      return
      end
      subroutine capchr(string)
c
c***********************************************************************
c
cc CAPCHR accepts a STRING of characters and replaces any lowercase
c  letters by uppercase ones.
c
c
c  STRING Input/output, character*(*) STRING, the string of
c         characters to be transformed.
c
      implicit double precision (a-h,o-z)
c
      integer i
      integer itemp
      integer nchar
      character*(*) string
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
      subroutine cavity(ibs,ibump,ifs,iopt,maxopt,maxpar,nbcrb,
     &  npar,nparb,nparf,npe,nx,ny,par,region,reynld,tolnew,tolopt,
     &  tolsim,wateb,wateu,watev,watep,xbl,xbr,xprof,
     &  xrange,ybl,ybr,yrange)
c 
c***********************************************************************
c
cc CAVITY sets up the standard driven cavity problem.
c
c  Reference:
c
c  Janet Peterson,
c  The Reduced Basis Method for Incompressible Viscous Flow Calculations,
c  SIAM Journal of Scientific and Statistical Computing,
c  Volume 10, Number 4, pages 777-786, July 1989.
c
      implicit double precision (a-h,o-z)
c
      integer maxpar
c
      integer i
      integer ibs
      integer ibump
      integer ifs
      integer iopt(maxpar)
      integer maxopt
      integer nbcrb
      integer npar
      integer nparb
      integer nparf
      integer npe
      integer nx
      integer ny
      double precision par(maxpar)
      character*20 region
      double precision reynld
      double precision tolnew
      double precision tolopt
      double precision tolsim
      double precision wateb
      double precision watep
      double precision wateu
      double precision watev
      double precision xbl
      double precision xbr
      double precision xprof
      double precision xrange
      double precision ybl
      double precision ybr
      double precision yrange
c
      ibs=0
      ibump=0
c
c  The "inflow" is modeled by a piecewise constant function.
c
      ifs=0
      maxopt=15
      nbcrb=1
      nparb=0
c
c  For our piecewise constant function, we specify one value.
c
      nparf=1
      npe=6
c
c  Peterson used a nonuniform mesh with NX=NY=25.
c
      nx=11
      ny=11
      region='cavity'
      tolnew=0.0000000001
      tolopt=0.000000001
      tolsim=0.0000000001
      wateb=0.0
      wateu=1.0
      watev=1.0
      watep=0.0
      xbl=0.0
      xbr=0.0
      xprof=0.50
      xrange=1.0
      ybl=0.0
      ybr=0.0
      yrange=1.0
c
c  Set things that depend on other things.
c
      npar=nparf+nparb+1

      do i=1,nparf
        iopt(i)=0
      enddo

      do i=nparf+1,nparf+nparb
        iopt(i)=0
      enddo

      iopt(nparf+nparb+1)=1

c
c  Set the parameter that determines the tangential flow.
c
      par(1)=-1.0
c
c  Set the REYNLD value.  Here, it is arbitrarily set
c  to 5.  Peterson worked with values as high as 5000.
c
      reynld=5.0
      par(2)=reynld

      return
      end
      subroutine channl(ibs,ibump,ifs,iopt,maxopt,maxpar,nbcrb,
     &  npar,nparb,nparf,npe,nx,ny,par,region,reynld,
     &  tolnew,tolopt,tolsim,wateb,wateu,watev,watep,xbl,
     &  xbr,xprof,xrange,ybl,ybr,yrange)
c 
c***********************************************************************
c
cc CHANNL sets up the standard channel problem.
c
      implicit double precision (a-h,o-z)
c
      integer maxpar
c
      integer i
      integer ibs
      integer ibump
      integer ifs
      integer iopt(maxpar)
      integer maxopt
      integer nbcrb
      integer npar
      integer nparb
      integer nparf
      integer npe
      integer nx
      integer ny
      double precision par(maxpar)
      character*20 region
      double precision reynld
      double precision tolnew
      double precision tolopt
      double precision tolsim
      double precision wateb
      double precision watep
      double precision wateu
      double precision watev
      double precision xbl
      double precision xbr
      double precision xprof
      double precision xrange
      double precision ybl
      double precision ybr
      double precision yrange
c
      ibs=2
      ibump=2
      ifs=2
      maxopt=10
      nbcrb=1
      nparb=3
      nparf=1
      npe=6
      nx=11
      ny=4
      region='channel'
      tolnew=0.0000000001
      tolopt=0.000000001
      tolsim=0.0000000001
      wateb=0.0
      wateu=1.0
      watev=1.0
      watep=0.0
      xbl=1.0
      xbr=3.0
      xprof=3.0
      xrange=10.0
      ybl=0.0
      ybr=0.0
      yrange=3.0
c
c  Set things that depend on other things.
c
      npar=nparf+nparb+1

      do i=1,nparf
        iopt(i)=1
      enddo

      do i=nparf+1,nparf+nparb
        iopt(i)=1
      enddo

      iopt(nparf+nparb+1)=1

      par(1)=0.5
      par(2)=0.375
      par(3)=0.5
      par(4)=0.375
      reynld=1.0
      par(5)=reynld

      return
      end
      subroutine chrctd(string,dval,ierror,lchar)
c
c***********************************************************************
c
cc CHRCTD accepts a string of characters, and tries to extract a
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
c  STRING Input, character*(*) STRING, the string containing the
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
      nchar=len(string)
 
      ierror=0
      dval=0.0
      lchar=-1
      isgn=1
      rtop=0.0
      rbot=1.0
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
        write(*,*)'CHRCTD - Fatal error!'
        write(*,*)'  Illegal or nonnumeric input!'
        return
      endif
c
c  Number seems OK.  Form it.
c
      if(jtop.eq.0)then
        rexp=1.0
      else
        if(jbot.eq.1)then
          rexp=10.0**(jsgn*jtop)
        else
          rexp=dble(jsgn*jtop)
          rexp=rexp/dble(jbot)
          rexp=10.0**rexp
        endif
      endif
 
      dval=dble(isgn)*rexp*rtop/rbot
 
      return
      end
      subroutine chrcti(string,intval,ierror,lchar)
c
c***********************************************************************
c
cc CHRCTI accepts a STRING of characters and reads an integer
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
c  STRING Input, character*(*) STRING, the string containing the
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
      implicit double precision (a-h,o-z)
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
        write(*,*)'CHRCTI - Fatal error!'
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
c***********************************************************************
c
cc CHRDB1 accepts a string of characters and removes all
c  blanks and nulls, left justifying the remainder and padding with
c  blanks.
c
c
c  STRING Input/output, character*(*) STRING, the string to be
c         transformed.
c
      implicit double precision (a-h,o-z)
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
c***********************************************************************
c
cc CHRUP2 copies STRING into STRNG2, up to, but not including, the
c  first occurrence of the string STRNG3.  Setting STRING='ABCDEFGH'
c  and STRNG3='EF' results in STRNG2='ABCD'.
c
c
c  STRING Input, character*(*) STRING, the string to be copied.
c
c  STRNG2 Output, character*(*) STRNG2, the copied portion of
c         STRING.
c
c  STRNG3 Input, character*(*) STRNG3, the 'flag' string at which
c         the copy stops.
c
      implicit double precision (a-h,o-z)
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
      subroutine ddetfl(afl,detlog,detman,ipivfl,lda,neqnfl,ml,mu)
c
c***********************************************************************
c
cc DDETFL computes the determinant of a double precision band matrix
c  that has been factored by DFACFL.
c
c
c  This routine was written 13 July 1996
c
c
c  AFL    Input, double precision AFL(LDA,N).
c         AFL contains the matrix as factored by DFACFL.
c
c  DETLOG Output, double precision DETLOG.
c         DETLOG is the integer part of the log base 10 of the determinant 
c         of the matrix.
c
c  DETMAN Output, double precision DETMAN.
c         DETMAN is the mantissa of the determinant of the matrix.
c         det(AFL) = DETMAN * 10 ** DETLOG.
c
c  IPIVFL Output, integer IPIVFL(NEQNFL).
c         The pivot vector.
c
c  LDA    Input, integer LDA.
c         LDA is the leading dimension of AFL.
c         LDA must be at least 2*ML+MU+1.
c
c  NEQNFL Input, integer NEQNFL.
c         The order of the original matrix.
c
c  ML     Input, integer ML.
c         The number of diagonals below the main diagonal.
c         ML must be at least 0, and no greater than NEQNFL.
c
c  MU     Input, integer MU.
c         The number of diagonals above the main diagonal.
c         MU must be at least 0, and no greater than NEQNFL.
c
      implicit double precision (a-h,o-z)
c
      integer lda
      integer neqnfl
c
      double precision afl(lda,neqnfl)
      double precision detlog
      double precision detman
      integer i
      integer ipivfl(neqnfl)
      integer ml
      integer mu
c
      detlog=0.0
      detman=1.0
c
      do i=1,neqnfl
 
        detman=detman*afl(ml+mu+1,i)
 
10      continue
 
        if(abs(detman).gt.10.0)then
          detman=detman/10.0
          detlog=detlog+1.0
          go to 10
        endif
 
20      continue
 
        if(detman.ne.0.0.and.abs(detman).lt.1.0)then
          detman=detman*10.0
          detlog=detlog-1.0
          go to 20
        endif
 
      enddo
 
      do i=1,neqnfl
        if(ipivfl(i).ne.i)then
          detman=-detman
        endif
      enddo
 
      return
      end
      subroutine ddetrb(arb,detlog,detman,ipivrb,maxcofrb,ncofrb)
c
c***********************************************************************
c
cc DDETRB computes the determinant of a double precision matrix
c  that has been factored by DFACRB.
c
c
c  This routine was written 13 July 1996
c
c
c  ARB    Input, double precision ARB(MAXNRB,NCOFRB).
c         ARB contains the matrix as factored by DFACFL.
c
c  DETLOG Output, double precision DETLOG.
c         DETLOG is the integer part of the log base 10 of the determinant 
c         of the matrix.
c
c  DETMAN Output, double precision DETMAN
c         DETMAN is the mantissa of the determinant of the matrix.
c         det(ARB) = DETMAN * 10 ** DETLOG.
c
c  IPIVRB Input, integer IPIVRB(NCOFRB).
c         The pivot vector.
c
c  MAXNRB Input, integer MAXNRB.
c         The leading dimension of the array ARB.
c
c  NCOFRB Input, integer NCOFRB.
c         The order of the original matrix.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer ncofrb
c
      double precision arb(maxcofrb,ncofrb)
      double precision detlog
      double precision detman
      integer i
      integer ipivrb(ncofrb)
c
      detlog=0.0
      detman=1.0
c
      do i=1,ncofrb
 
        detman=detman*arb(i,i)
 
10      continue
 
        if(abs(detman).gt.10.0)then
          detman=detman/10.0
          detlog=detlog+1.0
          go to 10
        endif
 
20      continue
 
        if(detman.ne.0.0.and.abs(detman).lt.1.0)then
          detman=detman*10.0
          detlog=detlog-1.0
          go to 20
        endif
 
      enddo
 
      do i=1,ncofrb
        if(ipivrb(i).ne.i)then
          detman=-detman
        endif
      enddo
 
      return
      end
      subroutine delhms(nsec,time1,time2)
c
c***********************************************************************
c
cc DELHMS returns the number of seconds that elapsed between
c  TIME1 and TIME2.  DELHMS tries to account for the possibility
c  that the clock time "wrapped around".  DELHMS assumes a 12 hour
c  clock is used, not a 24 hour clock!
c
c
c  NSEC   Output, INTEGER NSEC.
c         NSEC is the number of elapsed seconds.
c
c  TIME1,
c  TIME2  Input, CHARACTER*8 TIME1, TIME2, representations of
c         the beginning and end times, which must have the
c         form 'HH:MM:SS'.
c
      integer ihr1
      integer ihr2
      integer imn1
      integer imn2
      integer isc1
      integer isc2
      integer nsec
      character*8 time1
      character*8 time2
c
      read(time1,'(i2,1x,i2,1x,i2)')ihr1,imn1,isc1
      read(time2,'(i2,1x,i2,1x,i2)')ihr2,imn2,isc2
c
c  Add 12 hours to the second time, if you think it wrapped around
c  the clock.
c
      if(ihr1.gt.ihr2)then
        ihr2=ihr2+12
      elseif(ihr1.eq.ihr2)then
        if(imn1.gt.imn2)then
          ihr2=ihr2+12
        elseif(imn1.eq.imn2)then
          if(isc1.gt.isc2)then
            ihr2=ihr2+12
          endif
        endif
      endif
c
c  Add up the seconds.
c
      nsec=(ihr2-ihr1)*3600+(imn2-imn1)*60+(isc2-isc1)
 
      return
      end
      subroutine dfacfl(afl,lda,n,ml,mu,ipivfl,info)
c
c***********************************************************************
c
cc DFACFL factors a double precision band matrix by elimination.
c
c  DFACFL is a simplified version of the LINPACK routine DGBFA.
c
c  In order to use DFACFL, it is necessary to store the matrix AFL
c  in "LINPACK General Band Storage" format.
c
c  If AFL is a band matrix, the following program segment
c  will set up the compressed matrix properly:
c
c        m=ml+mu+1
c        do j=1,n
c          i1=max(1,j-mu)
c          i2=min(n,j+ml)
c          do i=i1,i2
c            k=i-j+m
c            afl(k,j)=Entry I, J
c          enddo
c        enddo
c
c  This uses rows ML+1 through 2*ML+MU+1 of the array AFL.
c  In addition, the first ML rows in ABD are used for
c  elements generated during the triangularization because of pivoting.
c  The total number of rows needed in AFL is 2*ML+MU+1.
c  The ML+MU by ML+MU upper left triangle and the
c  ML by ML lower right triangle are not referenced.
c
c
c  AFL    Input/output, double precision AFL(LDA,N).
c         On input, AFL contains the matrix in band storage.  The 
c         columns of the matrix are stored in the columns of AFL and
c         the diagonals of the matrix are stored in rows
c         ML+1 through 2*ML+MU+1 of AFL.
c
c         On output, an upper triangular matrix in band storage and
c         the multipliers which were used to obtain it.
c         The factorization can be written AFL = L*U where
c         L is a product of permutation and unit lower
c         triangular matrices and U is upper triangular.
c
c  LDA    Input, integer LDA.
c         The leading dimension of the array AFL.
c         LDA must be at least 2*ML+MU+1.
c
c  N      Input, integer N.
c         The order of the original matrix.
c
c  ML     Input, integer ML.
c         The number of diagonals below the main diagonal.
c         ML must be at least 0, and no greater than N.
c
c  MU     Input, integer MU.
c         The number of diagonals above the main diagonal.
c         MU must be at least 0, and no greater than N.
c
c  IPIVFL Output, integer IPIVFL(N).
c         An integer vector of pivot indices needed by DSOLFL.
c
c  INFO   Output, integer INFO.
c         = 0  normal value.
c         = K  if U(K,K) .eq. 0.0.  In this case, the matrix is exactly
c         numerically singular, and DSOLFL should not be called to attempt
c         a linear solution.
c 
      implicit double precision (a-h,o-z)
c
      integer lda
      integer n
c
      double precision afl(lda,n)
      integer i
      integer i0
      integer info
      integer ipivfl(n)
      integer j
      integer j1
      integer ju
      integer jz
      integer k
      integer l
      integer lm
      integer m
      integer ml
      integer mm
      integer mu
      double precision t
c
      m=ml+mu+1
      info=0
c
c  Zero out the initial fill-in columns of the matrix.
c
      j1=min(n,m)-1
      do jz=mu+2,j1
        i0=m+1-jz
        do i=i0,ml
          afl(i,jz)=0.0
        enddo
      enddo

      jz=j1
      ju=0
c
c  Carry out Gaussian elimination with partial pivoting
c
      do k=1,n-1
c
c  Zero out the next fill-in column.
c
        jz=jz+1
        if(jz.le.n)then
          do i=1,ml
             afl(i,jz)=0.0
          enddo
        endif
c
c  Find L = pivot index
c
        lm=min(ml,n-k)

        l=m
        do i=m+1,m+lm
          if(abs(afl(i,k)).gt.abs(afl(l,k)))then
            l=i
          endif
        enddo

        ipivfl(k)=l+k-m
c
c  A zero pivot means the matrix is singular.
c
        if(afl(l,k).eq.0.0)then
          info=k
        else
c
c  Interchange rows unless the pivot row is already on the diagonal.
c
          if(l.ne.m)then
            t=afl(l,k)
            afl(l,k)=afl(m,k)
            afl(m,k)=t
          endif
c
c  Compute the multipliers that form the lower diagonal entries of 
c  the L factor.
c
          do i=m+1,m+lm
            afl(i,k)=-afl(i,k)/afl(m,k)
          enddo
c
c  Row elimination with column indexing.
c
          ju=max(ju,mu+ipivfl(k))
          ju=min(ju,n)
          mm=m

          do j=k+1,ju
            l=l-1
            mm=mm-1
            
            t=afl(l,j)
            if(l.ne.mm)then
              afl(l,j)=afl(mm,j)
              afl(mm,j)=t
            endif
            
            do i=1,lm
              afl(mm+i,j)=afl(mm+i,j)+afl(m+i,k)*t
            enddo
 
          enddo

        endif
        
      enddo
      
      ipivfl(n)=n
      
      if(afl(m,n).eq.0.0)then
        info=n
      endif
      
      return
      end
      subroutine dfacrb(a,lda,n,ipivot,info)
c 
c***********************************************************************
c
cc DFACRB factors a double precision dense matrix.
c
c  DFACRB is similar to the LINPACK routine DGEFA, but does not call
c  any subroutines or functions.
c
c
c  DFACRB was modified on 14 August 1996.
c
c
c  A    
c    Input/output, double precision A(LDA,N).
c
c    On input, A contains the N by N matrix to be factored.
c
c    On output, A contains the L and U factors of the matrix, in 
c    compressed storage.
c
c  LDA    
c    Input, integer LDA.
c    LDA is the leading dimension of A, which is often equal to N.
c    LDA may be larger than N, but must not be smaller than N.
c
c  N      
c    Input, integer N.
c    N is the order of A, that is, the number of rows and columns
c    of the matrix.
c
c  IPIVOT
c    Output, integer IPIVOT(N).
c    IPIVOT is the pivot array.
c
c  INFO   
c    Output, integer INFO.
c    INFO is an error flag.
c
c    INFO=0, no error, the matrix was factored.
c    INFO=K, the K-th pivot U(K,K) was zero.
c
      implicit double precision (a-h,o-z)
c
      integer lda
      integer n
c
      double precision a(lda,n)
      integer i
      integer info
      integer ipivot(n)
      integer j
      integer k
      integer l
      double precision t
c
      info=0

      do k=1,n-1
c
c  Find the pivot row L.
c
        l=k
        do i=k+1,n
          if(abs(a(i,k)).gt.abs(a(i,l)))then
            l=i
          endif
        enddo

        ipivot(k)=l
c
c  Check for a zero pivot.
c
        if(a(l,k).eq.0.0)then

          info=k
          return

        endif
c
c  Check to see whether we must swap rows L and K.
c
        if(l.ne.k)then
          t=a(l,k)
          a(l,k)=a(k,k)
          a(k,k)=t
        endif
c
c  Rescale the pivot row so that A(K,K)=1.
c
        do i=k+1,n
          a(i,k)=-a(i,k)/a(k,k)
        enddo
c
c  Wipe out the entries below A(K,K).
c
        do j=k+1,n

          t=a(l,j)

          if(l.ne.k)then
            a(l,j)=a(k,j)
            a(k,j)=t
          endif

          do i=k+1,n
            a(i,j)=a(i,j)+t*a(i,k)
          enddo

        enddo

      enddo

      ipivot(n)=n

      if(a(n,n).eq.0.0)then
        info=n
      endif

      return
      end
      subroutine difset(difcof,h,iwrite,ncof)
c
c***********************************************************************
c  
cc DIFSET computes the NCOF coefficients for a centered finite difference
c  estimate of the (NCOF-1)-th derivative of a function.
c
c  The estimate has the form
c
c    FDIF(NCOF-1,X) = Sum (I=1 to NCOF) COF(I) * F(X(I))
c
c  To understand the computation of the coefficients, it is enough
c  to realize that the first difference approximation is
c
c    FDIF(1,X) = F(X+DX) - F(X-DX) ) / (2*DX)
c
c  and that the second difference approximation can be regarded as
c  the first difference approximation repeated:
c
c    FDIF(2,X) = FDIF(1,X+DX) - FDIF(1,X-DX) / (2*DX)
c           = F(X+2*DX) - 2 F(X) + F(X-2*DX) / (4*DX)
c
c  and so on for higher order differences.
c
c  Thus, the next thing to consider is the integer coefficients of
c  the sampled values of F, which are clearly the Pascal coefficients,
c  but with an alternating negative sign.  In particular, if we
c  consider row I of Pascal's triangle to have entries J=0 through I,
c  then P(I,J) = P(I-1,J-1) - P(I-1,J), where P(*,-1) is taken to be 0,
c  and P(0,0)=1.
c
c     1
c    -1  1
c     1 -2   1
c    -1  3  -3   1
c     1 -4   6  -4   1
c    -1  5 -10  10  -5  1
c     1 -6  15 -20  15 -6 1
c
c  Next, we note that the denominator of the approximation for the
c  I-th derivative will be (2*DX)**I.
c
c  And finally, we must consider the location of the NDIF sampling
c  points for F:
c
c    X-NDIF*DX, X-(NDIF-2)*DX, X-(NDIF-4)*DX, ...,
c    X+(NDIF-4)*DX, X+(NDIF-2*DX), X+(NDIF-1)*DX.
c
c
c  Thus, a formula for evaluating FDIF(NDIF,X) is
c
c    fdif=0.0
c    ncof=ndif+1
c    do i=1,ncof
c      xi=x+(2*(i-1)-ncof)*h
c      fdif=fdif+difcof(i)*f(xi)
c    enddo
c
c
c  DIFSET was modified on 01 July 1996.
c
c
c  DIFCOF Output, REAL DIFCOF(NCOF), the coefficients needed to approximate
c         the (NCOF-1)-th derivative of a function F.
c
c  H      Input, REAL H, the half spacing between points.  H must 
c         be positive.
c
c  NCOF   Input, INTEGER NCOF.
c         NCOF is the number of coefficients desired, which also
c         determines NDIF=NCOF-1, the derivative being estimated.
c
      implicit double precision (a-h,o-z)
c
      integer ncof
c
      double precision difcof(ncof)
      double precision h
      integer i
      integer iwrite
      integer j
      integer ndif
c
      ndif=ncof-1
 
      if(ndif.lt.0)then
        write(*,*)' '
        write(*,*)'DIFSET - Fatal error!'
        write(*,*)'  Derivative order NDIF=',ndif
        write(*,*)'  but NDIF must be at least 0.'
        stop
      endif
 
      if(h.le.0.0)then
        write(*,*)' '
        write(*,*)'DIFSET - Fatal error!'
        write(*,*)'  The half sampling spacing is H=',H
        write(*,*)'  but H must be positive.' 
        stop
      endif
 
      do i=1,ncof
 
        difcof(i)=1.0
 
        do j=i-1,2,-1
          difcof(j)=-difcof(j)+difcof(j-1)
        enddo
 
        if(i.gt.1)then
          difcof(1)=-difcof(1)
        endif
 
      enddo
c
      if(iwrite.ge.2)then
        write(*,*)' '
        write(*,*)'DIFSET - Unnormalized coefficients:'
        do i=1,ncof
          write(*,'(i6,g14.6)')i,difcof(i)
        enddo
      endif
 
      do i=1,ncof
        difcof(i)=difcof(i)/(2.0*h)**ndif
      enddo
 
      if(iwrite.ge.2)then
        write(*,*)' '
        write(*,*)'DIFSET - Normalized coefficients:'
        do i=1,ncof
          write(*,'(i6,g14.6)')i,difcof(i)
        enddo
      endif
 
      return
      end
      subroutine discst(costp,costu,costv,gfl,gfltar,indx,neqnfl,np,
     &  nprof,ny,yc)
c
c***********************************************************************
c
cc DISCST computes the discrepancy integrals for the pressure,
c  horizontal and vertical velocities, along the profile line.
c
c  This integration scheme assumes that the profile line, and
c  the element sides that define it, are straight.  Otherwise,
c  the integration scheme used is not correct.
c
c
c  DISCST was modified on 01 July 1996.
c
c
c  COSTP  Output, double precision COSTP.
c
c         The integral of the difference between
c         the computed and target pressure functions along the
c         profile line.
c
c  COSTU  Output, double precision COSTU.
c
c         The integral of the difference between
c         the computed and target horizontal velocity functions along 
c         the profile line.
c
c  COSTV  Output, double precision COSTV.
c
c         The integral of the difference between
c         the computed and target vertical velocity functions along 
c         the profile line.
c 
c  GFL    Input, double precision GFL(NEQNFL), the current solution 
c         estimate for the full problem.
c
c  GTARFL Input, double precision GTARFL(NEQNFL), the target solution vector.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  NEQNFL Input, integer NEQNFL, the number of equations in the full system.
c
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c
c  NPROF  Input, integer NPROF(2*MAXNY-1).
c
c         NPROF contains the numbers of the nodes along the profile
c         line.
c 
c  NY     Input, integer NY.
c
c         NY controls the spacing of nodes and elements in
c         the Y direction.  There are 2*NY-1 nodes along various
c         lines in the Y direction.
c
c  YC     Input, double precision YC(NP), the Y coordinates of the nodes.
c 
      implicit double precision (a-h,o-z)
c
      integer nquad1
c
      parameter (nquad1=5)
c
      integer neqnfl
      integer np
      integer ny
c
      double precision bval
      double precision costp
      double precision costu
      double precision costv
      double precision gfl(neqnfl)
      double precision gfltar(neqnfl)
      integer i
      integer ii
      integer indx(3,np)
      integer j
      integer k
      integer npol
      integer nprof(2*ny-1)
      double precision pcof(2)
      double precision pval
      double precision ucof(3)
      double precision uval
      double precision vcof(3)
      double precision vval
      double precision wquad1(nquad1)
      double precision xsiquad(nquad1)
      double precision yc(np)
      double precision yhi
      double precision ylo
      double precision ypol(3)
      double precision yval
c
c  Get the weights and abscissas to approximate a line integral.
c
      call gquad1(nquad1,wquad1,xsiquad)
c
c  Compute the integral of the difference squared between the
c  current velocity and the target values.
c
      costu=0.0
      costv=0.0
c
c  The line along which we integrate is broken into NY-1
c  subintervals, over each of which, U and V are represented
c  by quadratic functions.
c
      do i=1,ny-1
c
c  Get the values of U and V at the beginning, middle, and
c  end of the subinterval.  Use these to compute the quadratic
c  representation of U and V for any point on the subinterval.
c
        ylo=yc(nprof(2*i-1))
        yhi=yc(nprof(2*i+1))
 
        npol=3
 
        do k=1,npol
 
          ii=2*i-2+k
          ypol(k)=yc(nprof(ii))
 
          j=indx(1,nprof(ii))
          ucof(k)=gfl(j)-gfltar(j)
 
          j=indx(2,nprof(ii))
          vcof(k)=gfl(j)-gfltar(j)
 
        enddo
c
c  Evaluate the discrepancy at each quadrature point.
c
        do j=1,nquad1
 
          yval=0.5*((1.0+xsiquad(j))*ylo+(1.0-xsiquad(j))*yhi)
 
          uval=0.0
          vval=0.0
 
          do k=1,npol
            call lbase(k,npol,bval,ypol,yval)
            uval=uval+bval*ucof(k)
            vval=vval+bval*vcof(k)
          enddo
 
          costu=costu+0.5*wquad1(j)*(yhi-ylo)*uval**2
          costv=costv+0.5*wquad1(j)*(yhi-ylo)*vval**2
 
        enddo
      enddo
c
c  Compute the square root of the integral of the difference
c  squared between the current pressure and the target values.
c
      costp=0.0
 
      do i=1,ny-1
 
        ylo=yc(nprof(2*i-1))
        yhi=yc(nprof(2*i+1))
 
        npol=2
 
        do k=1,npol
 
          ii=2*i-3+2*k
 
          ypol(k)=yc(nprof(ii))
 
          j=indx(3,nprof(ii))
          if(j.le.0)then
            pcof(k)=0.0
          else
            pcof(k)=gfl(j)-gfltar(j)
          endif
 
        enddo
 
        do j=1,nquad1
 
          yval=0.5*((1.0+xsiquad(j))*ylo
     &              + (1.0-xsiquad(j))*yhi)
 
          pval=0.0
 
          do k=1,npol
            call lbase(k,npol,bval,ypol,yval)
            pval=pval+bval*pcof(k)
          enddo
 
          costp=costp+0.5*wquad1(j)*(yhi-ylo)*pval**2
 
        enddo
      enddo
 
      return
      end
      subroutine dsolfl(afl,lda,n,ml,mu,ipivfl,b)
c
c***********************************************************************
c
cc DSOLFL solves the linear system AFL*X=B
c
c  where AFL, X, and B are double precision, and AFL is a banded matrix
c  which has already been decomposed into LU factors by DFACFL.
c
c  DSOLFL is a simplied version of the LINPACK routine DGBSL.
c
c
c  AFL    Input, double precision AFL(LDA,N).
c         The factored matrix produced by DFACFL.
c
c  LDA    Input, integer LDA.
c         The leading dimension of the array AFL.
c
c  N      Input, integer N.
c         The order of the original matrix.
c
c  ML     Input, integer ML.
c         The number of diagonals below the main diagonal.
c
c  MU     Input, integer MU.
c         The number of diagonals above the main diagonal.
c
c  IPIVFL Input, integer IPIVFL(N).
c         The pivot vector from DFACFL.
c
c  B      Input/output, double precision B(N).
c         On input, the right hand side vector.
c         On output, the solution vector X.
c
      implicit double precision (a-h,o-z)
c
      integer lda
      integer n
c
      double precision afl(lda,n)
      double precision b(n)
      integer i
      integer ipivfl(n)
      integer k
      integer l
      integer la
      integer lb
      integer lm
      integer m
      integer ml
      integer mu
      double precision t
c
      m=mu+ml+1
c
c  First solve L*Y=B.
c
      if(ml.ne.0)then
      
        do k=1,n-1
        
          lm=min(ml,n-k)
          l=ipivfl(k)

          if(l.ne.k)then
            t=b(l)
            b(l)=b(k)
            b(k)=t
          endif
          
          do i=1,lm
            b(k+i)=b(k+i)+afl(m+i,k)*b(k)
          enddo
          
        enddo
      endif
c
c  Now solve U*X=Y.
c
      do k=n,1,-1
      
        if(afl(m,k).eq.0.0)then
          write(*,*)' '
          write(*,*)'DSOLFL - Fatal error!'
          write(*,*)'  Pivot K=',k,' is zero.'
          stop
        else
          b(k)=b(k)/afl(m,k)
        endif
        
        lm=min(k,m)-1
        la=m-lm
        lb=k-lm
        do i=1,lm
          b(lb-1+i)=b(lb-1+i)-afl(la-1+i,k)*b(k)
        enddo

      enddo
      
      return
      end
      subroutine dsolrb(a,lda,n,ipivot,b)
c 
c***********************************************************************
c
cd DSOLRB solves the linear system A*X=B
c
c  where A is a full storage double precision array which has been 
c  LU-factored by DFACRB.
c
c  DSOLRB is similar to the LINPACK routine DGESL, but does not call
c  any subroutines or functions.
c
c
c  DSOLRB was modified on 14 August 1996.
c
c
c  A   
c    Input, double precision A(LDA,N).
c    A contains the LU factors of a matrix, as computed by DFACRB.
c
c  LDA    
c    Input, integer LDA.
c    LDA is the leading dimension of the matrix A.
c
c  N      
c    Input, integer N.
c    N is the order of the matrix A.
c
c  IPIVOT 
c    Input, integer IPIVOT(N).
c    IPIVOT is the pivot vector computed by DFACRB.
c
c  B      
c    Input/output, double precision B(N).
c
c    On input, B is the right hand side of the linear system.
c
c    On output, B is the solution of the linear system.
c
      implicit double precision (a-h,o-z)
c
      integer lda
      integer n
c
      double precision a(lda,n)
      double precision b(n)
      integer i
      integer ipivot(n)
      integer k
      integer l
      double precision t
c
c  First solve L*Y=B.
c
      do k=1,n-1

        l=ipivot(k)

        t=b(l)
        if(l.ne.k)then
          b(l)=b(k)
          b(k)=t
        endif

        do i=k+1,n
          b(i)=b(i)+t*a(i,k)
        enddo

      enddo
c
c  Now solve U*X=Y.
c
      do k=n,1,-1

        b(k)=b(k)/a(k,k)
        t=-b(k)

        do i=1,k-1
          b(i)=b(i)+t*a(i,k)
        enddo

      enddo

      return
      end
      function dveq(n,dvec1,dvec2)
c
c***********************************************************************
c
cc DVEQ returns .TRUE. if the N elements of the double precision
c  vectors DVEC1 and DVEC2 are equal.
c
c
c  N      Input, INTEGER N, the number of entries in the vectors.
c
c  DVEC1,
c  DVEC2  Input, DOUBLE PRECISION DVEC1(N), DVEC2(N), the two vectors
c         to be compared.
c
c  DVEQ   Output, LOGICAL DVEQ.
c         DVEQ is .TRUE. if all N elements of DVEC1 and DVEC2 are equal,
c         and .FALSE. otherwise.
c
      implicit double precision (a-h,o-z)
c
      integer n
c
      double precision dvec1(n)
      double precision dvec2(n)
      logical dveq
      integer i
c
      dveq=.false.
      
      do i=1,n
        if(dvec1(i).ne.dvec2(i))return
      enddo
      
      dveq=.true.
      
      return
      end
      function dvneq(n,dvec1,dvec2)
c
c***********************************************************************
c
cc DVNEQ returns .TRUE. if any of the N elements of the double precision
c  vectors DVEC1 and DVEC2 are not equal.
c
c
c  N      
c    Input, INTEGER N.
c    N is the number of entries in the vectors.
c
c  DVEC1,
c  DVEC2  
c    Input, DOUBLE PRECISION DVEC1(N), DVEC2(N).
c    DVEC1 and DVEC2 are the two vectors to be compared.
c
c  DVNEQ  
c    Output, LOGICAL DVNEQ.
c    DVNEQ is .TRUE. if any elements of DVEC1 and DVEC2 differ,
c    and .FALSE. otherwise.
c
      integer n
c
      double precision dvec1(n)
      double precision dvec2(n)
      logical dvneq
      integer i
c
      dvneq=.true.
      
      do i=1,n
        if(dvec1(i).ne.dvec2(i))return
      enddo
      
      dvneq=.false.
      
      return
      end
      subroutine fact(n,factn)
c
c***********************************************************************
c
cc FACT computes the (real) factorial of a nonnegative integer.
c
c
c  N      Input, INTEGER N, the nonnegative value for which N! 
c         is desired.
c
c  FACTN  Output, DOUBLE PRECISION FACTN, the factorial of N.
c
      implicit double precision (a-h,o-z)
c
      double precision factn
      integer i
      integer n
c
      if(n.lt.0)then
        write(*,*)' '
        write(*,*)'FACT - Fatal error!'
        write(*,*)'  Negative input argument is N=',n
        stop
      endif

      factn=1.0
      do i=1,n
        factn=factn*dble(i)
      enddo

      return
      end
      subroutine getcst(cost,costb,costp,costu,costv,gfl,gfltar,indx,
     &  neqnfl,np,nparb,nprof,ny,splbmp,taubmp,wateb,watep,wateu,
     &  watev,xbl,xbr,ybl,ybr,yc)
c
c***********************************************************************
c
cc GETCST is given the value of the solution, GFL, the target
c  solution GTARFL, and information about the shape of the bump,
c  and returns the value of the overall and individual cost
c  functions.
c
c
c  GETCST was modified on 08 August 1996.
c
c
c  COST   Output, double precision COST, the weighted cost.
c
c  COSTB  Output, double precision COSTB.
c
c         COSTB is the integral of the difference of the
c         derivatives of the straight line joining the two straight line
c         line segments of the bottom, and the bump that is
c         actually drawn there.
c
c         This measures the cost of bump control.
c
c  COSTP  Output, double precision COSTP.
c
c         The integral of the difference between
c         the computed and target pressure functions along the
c         profile line.
c
c  COSTU  Output, double precision COSTU.
c
c         The integral of the difference between
c         the computed and target horizontal velocity functions along 
c         the profile line.
c
c  COSTV  Output, double precision COSTV.
c
c         The integral of the difference between
c         the computed and target vertical velocity functions along 
c         the profile line.
c 
c  GFL    Input, double precision GFL(NEQNFL), the current solution 
c         estimate for the full problem.
c
c  GTARFL Input, double precision GTARFL(NEQNFL), the target solution vector.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  NEQNFL Input, integer NEQNFL, the number of equations in the full system.
c
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPROF  Input, integer NPROF(2*MAXNY-1).
c
c         NPROF contains the numbers of the nodes along the profile
c         line.
c 
c  NY     Input, integer NY.
c
c         NY controls the spacing of nodes and elements in
c         the Y direction.  There are 2*NY-1 nodes along various
c         lines in the Y direction.
c
c  SPLBMP Input, double precision SPLBMP(NPARB+2).
c
c         SPLBMP contains the spline coefficients for the bump.
c
c  TAUBMP Input, double precision TAUBMP(NPARB+2).
c
c         TAUBMP contains the location of the spline abscissas for
c         the bump.  There are NPARB+2 of them, because the end values
c         of the spline are constrained to have particular values.
c
c  WATEB  Input, double precision WATEB.
c
c         WATEB is the multiplier of the bump control cost used
c         when computing the total cost.
c
c  WATEP,
c  WATEU,
c  WATEV  Input, double precision WATEP, WATEU, WATEV.
c
c         These are weights used in computing the overall cost 
c         function based on the costs of the flow discrepancy.
c
c  XBL    Input, double precision XBL, the X coordinate of the left corner 
c         of the bump.
c
c  XBR    Input, double precision XBR, the X coordinate of the right corner 
c         of the bump.
c
c  YBL    Input, double precision YBL, the Y coordinate of the left corner 
c         of the bump.
c
c  YBR    Input, double precision YBR, the Y coordinate of the right corner 
c         of the bump.
c
c  YC     Input, double precision YC(NP), the Y coordinates of the nodes.
c 
      implicit double precision (a-h,o-z)
c
      integer neqnfl
      integer np
      integer nparb
      integer ny
c
      double precision cost
      double precision costb
      double precision costp
      double precision costu
      double precision costv
      double precision gfl(neqnfl)
      double precision gfltar(neqnfl)
      integer indx(3,np)
      integer nprof(2*ny-1)
      double precision splbmp(nparb+2)
      double precision taubmp(nparb+2)
      double precision wateb
      double precision watep
      double precision wateu
      double precision watev
      double precision xbl
      double precision xbr
      double precision ybl
      double precision ybr
      double precision yc(np)
c
      call bmpcst(costb,nparb,splbmp,taubmp,xbl,xbr,ybl,ybr)
 
      call discst(costp,costu,costv,gfl,gfltar,indx,neqnfl,np,
     &  nprof,ny,yc)
 
      cost=wateb*costb+watep*costp+wateu*costu+watev*costv
 
      return
      end
      subroutine gquad1(nquad1,wquad1,xsiquad)
c
c***********************************************************************
c
cc GQUAD1 returns the weights and abscissas for a 1 dimensional,
c  3 or 5 point Gauss quadrature rule defined on the interval [-1,1].
c
c
c  The integral of a function F(X) over the interval [-1,1]
c
c    Integral (-1 to 1) F(X) DX
c
c  may then be approximated by
c
c    Sum (I = 1 to NQUAD1) WQUAD1(I) * F(XSIQUAD(I))
c
c
c  NQUAD1 Input, integer NQUAD1.
c         The user specifies the rule desired by setting NQUAD1
c         to 3 or 5.  Any other value is illegal, and will cause
c         GQUAD1 to stop.
c
c  WQUAD1 Output, double precision WQUAD1(NQUAD1).
c         WQUAD1(I) is the weight factor corresponding to the
c         I-th quadrature point.
c
c  XSIQUAD
c         Output, double precision XSIQUAD(NQUAD1).
c         XSIQUAD(I) is the I-th quadrature point.
c
      implicit double precision (a-h,o-z)
c
      integer nquad1
c
      double precision wquad1(nquad1)
      double precision xsiquad(nquad1)
c
      if(nquad1.eq.3)then
 
        xsiquad(1)=-0.7745966692
        xsiquad(2)= 0.0
        xsiquad(3)= 0.7745966692
 
        wquad1(1)=5.0/9.0
        wquad1(2)=8.0/9.0
        wquad1(3)=5.0/9.0
 
      elseif(nquad1.eq.5)then
 
        xsiquad(1)=-0.906179845938664
        xsiquad(2)=-0.538469310105683
        xsiquad(3)= 0.0
        xsiquad(4)= 0.538469310105683
        xsiquad(5)= 0.906179845938664
 
        wquad1(1)=0.236926885056189
        wquad1(2)=0.478628670499366
        wquad1(3)=0.568888888888889
        wquad1(4)=0.478628670499366
        wquad1(5)=0.236926885056189
 
      else
 
        write(*,*)' '
        write(*,*)'GQuad1 - Fatal error!'
        write(*,*)'  An illegal value of NQUAD1 was input.'
        write(*,*)'  Only NQUAD1=3 or 5 are legal.'
        write(*,*)'  The input value was ',nquad1
        write(*,*)'  The code is stopping now.'
        stop
 
      endif
 
      return
      end
      subroutine grid(gridx,i,ihi,ilo,x,xhi,xlo)
c
c***********************************************************************
c
cc GRID computes the X or Y coordinate of the I-th gridpoint.
c
c  GRIDX  Input, CHARACTER*20 GRIDX.
c         GRIDX tells how the finite element nodes should be layed out
c         in the X direction.
c         'uniform' makes them equally spaced.
c         'cos' uses the COS function to cluster them near edges.
c         'sqrtsin' uses the SQRT(SIN()) function to cluster near edges.
c
c  I      Input, INTEGER I.
c         I is the index of the grid point whose X coordinate is to
c         be computed.  Normally, ILO <= I <= IHI.
c
c  IHI,
c  ILO    Input, INTEGER IHI, ILO.
c         ILO is the index of the grid point whose X coordinate is XLO,
c         IHI is the same for XHI.
c
c  X      Output, double precision X.
c         X is the X coordinate of the I-th grid point, according to
c         the specified scheme.
c
c  XHI,
c  XLO    Input, double precision XHI, XLO.
c         XLO is the X coordinate of grid point ILO, and XHI
c         is the X coordinate of grid point IHI.
c
      implicit double precision (a-h,o-z)
c
      double precision pi
      parameter (pi=3.14159265)
c
      character*20 gridx
      integer i
      integer ihi
      integer ilo
      logical leqi
      double precision s
      double precision theta
      double precision thi
      double precision tlo
      double precision x
      double precision xhi
      double precision xlo
c
      if(leqi(gridx,'uniform'))then
        x=(dble(ihi-i)*xlo+dble(i-ilo)*xhi)/dble(ihi-ilo)
      elseif(leqi(gridx,'sin'))then
        tlo=-pi/2.0
        thi=pi/2.0
        theta=(dble(ihi-i)*tlo + dble(i-ilo)*thi)/dble(ihi-ilo)
        s=sin(theta)
        x=((1.0-s)*xlo+(s+1.0)*xhi)/2.0
c
c  Equivalent to 'SIN'.
c
      elseif(leqi(gridx,'cos'))then
        tlo=-pi
        thi=0.0
        theta=(dble(ihi-i)*tlo + dble(i-ilo)*thi)/dble(ihi-ilo)
        x=((1.0-cos(theta))*xlo+(1.0+cos(theta))*xhi)/2.0
      elseif(leqi(gridx,'sqrtsin'))then
        tlo=-pi/2.0
        thi=pi/2.0
        theta=(dble(ihi-i)*tlo + dble(i-ilo)*thi)/dble(ihi-ilo)
        if(sin(theta).ge.0.0)then
          s=sqrt(sin(theta))
        else
          s=-sqrt(-sin(theta))
        endif
        x=((1.0-s)*xlo+(s+1.0)*xhi)/2.0
      endif
 
      return
      end
      subroutine intprs(gfl,indx,nelem,neqnfl,node,np,p)
c
c***********************************************************************
c
cc INTPRS interpolates the pressure at the midside nodes.
c
c
c  GFL    Input, double precision GFL(NEQNFL).
c         GFL is the current solution estimate for the full problem, 
c         containing pressure and velocity coefficients.  The vector 
c         INDX must be used to index this data.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the global index of U, 
c         V and P at that node, or 0 or a negative value.  The global
c         index of U, V, or P is the index of the coefficient vector
c         that contains the value of the finite element coefficient
c         associated with the corresponding basis function at the
c         given node.
c   
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NEQNFL Input, integer NEQNFL.
c         NEQNFL is the number of equations (and coefficients) in the full 
c         finite element system.
c
c  NODE   Input, integer NODE(6,MAXELM) or NODE(6,NELEM).
c
c         NODE(I,J) contains, for an element J, the global index of 
c         the node whose local number in J is I.
c
c         The local ordering of the nodes is suggested by this diagram:
c
c           Global nodes   Elements      NODE
c                                                          1  2  3  4  5  6
c           74  84  94     3-6-1   2     Left element =  (94,72,74,83,73,84)
c                          |  /   /|
c           73  83  93     5 4   4 5     Right element = (72,94,92,83,93,82)
c                          |/   /  |
c           72  82  92     2   1-6-3
c 
c  NP     Input, INTEGER NP, the number of nodes.
c
c  P      Input, REAL P(NP), the pressure.
c
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer neqnfl
      integer np
c
      double precision gfl(neqnfl)
      integer i
      integer in1
      integer in2
      integer in3
      integer in4
      integer in5
      integer in6
      integer indx(3,np)
      integer node(6,nelem)
      double precision p(np)
c
c  For each element,...
c
      do i=1,nelem
c
c  Get the six global node numbers.
c
        in1=node(1,i)
        in2=node(2,i)
        in3=node(3,i)
        in4=node(4,i)
        in5=node(5,i)
        in6=node(6,i)
c
c  Read off the three computed values, and average the other three.
c
        p(in1)=gfl(indx(3,in1))
        p(in2)=gfl(indx(3,in2))
        p(in3)=gfl(indx(3,in3))
        p(in4)=0.5*(p(in1)+p(in2))
        p(in5)=0.5*(p(in2)+p(in3))
        p(in6)=0.5*(p(in3)+p(in1))

      enddo

      return
      end
      subroutine l2norm(gfl,gflnrm,indx,nelem,neqnfl,node,np,xc,yc)
c
c***********************************************************************
c
cc L2NORM computes the "big" L2 norm of the velocity over the flow region,
c  using a 13 point Gauss rule.
c
c  Note that this is the "BIG L2" norm, that is, the square root
c  of the integral of the square of the velocity over the flow region,
c  and NOT the "little l2" norm, which is simply the square root of the
c  sum of the squares of the coefficients.
c
c
c  GFL    Input, double precision GFL(NEQNFL).
c
c         GFL is the current solution vector, in which are stored 
c         the finite element coefficients that define the velocity
c         and pressure functions, U, V and P.
c
c  GFLNRM Output, double recision GFLNRM.
c         GFLNRM is the approximate value of the square root of
c         the integral of the square of the velocity over the
c         flow domain.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NEQNFL Input, integer NEQNFL, the number of finite element equations used
c         to define the horizontal and vertical velocities and the
c         pressure.
c 
c  NODE   Input, integer NODE(6,NELEM).
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
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c 
c  XC     Input, double precision XC(NP).
c
c         The X coordinates of the nodes.
c 
c  YC     Input, double precision YC(NP).
c
c         The Y coordinates of the nodes.
c 
      implicit double precision (a-h,o-z)
c
      integer nquad
c
      parameter (nquad=13)
c
      integer nelem
      integer neqnfl
      integer np
c
      double precision area
      double precision area2
      double precision dwdx
      double precision dwdy
      double precision eta
      double precision etaquad(nquad)
      double precision gfl(neqnfl)
      double precision gflnrm
      integer i
      integer ielem
      integer in
      integer indx(3,np)
      integer ip
      integer ip1
      integer ip2
      integer ip3
      integer iquad
      integer jp
      integer node(6,nelem)
      double precision u
      double precision v
      double precision vmax
      double precision w
      double precision wquad(nquad)
      double precision xc(np)
      double precision xq
      double precision xsi
      double precision xsiquad(nquad)
      double precision yc(np)
      double precision yq
c
      wquad(1) =0.175615257433204
      wquad(2) =0.175615257433204
      wquad(3) =0.175615257433204

      wquad(4) =0.053347235608839
      wquad(5) =0.053347235608839
      wquad(6) =0.053347235608839

      wquad(7) =0.077113760890257
      wquad(8) =0.077113760890257
      wquad(9) =0.077113760890257
      wquad(10)=0.077113760890257
      wquad(11)=0.077113760890257
      wquad(12)=0.077113760890257

      wquad(13)=-0.149570044467670

      do i=1,nquad
        wquad(i)=wquad(i)/2.0
      enddo

      xsiquad(1)= 0.260345966079038
      etaquad(1)= 0.479308067841923

      xsiquad(2)= 0.260345966079038
      etaquad(2)= 0.260345966079038

      xsiquad(3)= 0.479308067841923
      etaquad(3)= 0.260345966079038

      xsiquad(4)= 0.065130102902216
      etaquad(4)= 0.869739794195568

      xsiquad(5)= 0.065130102902216
      etaquad(5)= 0.065130102902216

      xsiquad(6)= 0.869739794195568
      etaquad(6)= 0.065130102902216

      xsiquad(7)= 0.048690315425316
      etaquad(7)= 0.638444188569809

      xsiquad(8)= 0.312865496004875
      etaquad(8)= 0.638444188569809

      xsiquad(9)= 0.048690315425316
      etaquad(9)= 0.312865496004875

      xsiquad(10)=0.638444188569809
      etaquad(10)=0.312865496004875

      xsiquad(11)=0.312865496004875
      etaquad(11)=0.048690315425316

      xsiquad(12)=0.638444188569809
      etaquad(12)=0.048690315425316

      xsiquad(13)=1.0/3.0
      etaquad(13)=1.0/3.0
c
      do i=1,nquad
        xsiquad(i)=1.0-xsiquad(i)
      enddo
c
      gflnrm=0.0
      area2=0.0
      vmax=0.0
c
c  Consider an element.
c
      do ielem=1,nelem
 
c
c  Compute the area of the element.  For now, we assume that all
c  elements are triangles, and NOT isoparametric!
c
        ip1=node(1,ielem)
        ip2=node(2,ielem)
        ip3=node(3,ielem)
 
        area=abs(
     &    (yc(ip1)+yc(ip2))*(xc(ip2)-xc(ip1))
     &   +(yc(ip2)+yc(ip3))*(xc(ip3)-xc(ip2))
     &   +(yc(ip3)+yc(ip1))*(xc(ip1)-xc(ip3)) )
c
c  Evaluate the integrand at the quadrature points.
c
        do iquad=1,nquad
 
          xsi=xsiquad(iquad)
          eta=etaquad(iquad)
          call xofxsi(eta,ielem,nelem,node,np,xq,xc,xsi,yq,yc)
c
c  Evaluate U, V and P at the IQUAD-th quadrature point by
c  finding the value of each of the 6 basis functions there,
c  and multiplying by their respective coefficients.
c
          u=0.0
          v=0.0
          do in=1,6
            call qbf(ielem,in,w,dwdx,dwdy,nelem,node,np,xc,xq,yc,yq)
            ip=node(in,ielem)
            jp=indx(1,ip)
            u=u+gfl(jp)*w
            jp=indx(2,ip)
            v=v+gfl(jp)*w
          enddo
 
          gflnrm=gflnrm+area*wquad(iquad)*(u**2+v**2)
          if(u**2+v**2.gt.vmax)then
            vmax=u**2+v**2
          endif
 
          area2=area2+area*wquad(iquad)
        enddo
      enddo
 
      gflnrm=sqrt(gflnrm)
 
      return
      end
      subroutine lbase(ival,npol,pval,xpol,xval)
c
c***********************************************************************
c
cc LBASE evalualates the IVAL-th Lagrange polynomial based
c  on the NPOL points XPOL.
c
c
c  IVAL   Input, integer IVAL, the polynomial to evaluate.
c         IVAL should be between 1 and NPOL.
c
c  NPOL   Input, integer NPOL, the number of points that define
c         the Lagrange polynomials.
c
c  PVAL   Output, double precision PVAL, the value of the IVAL-th
c         Lagrange polynomial at the point XVAL.
c
c  XPOL   Input, double precision XPOL(NPOL), the abscissas of the
c         Lagrange polynomials.  The entries in XPOL should be
c         distinct.
c
c  XVAL   Input, double precision XVAL, the point at which the
c         IVAL-th Lagrange polynomial is to be evaluated.
c
      implicit double precision (a-h,o-z)
c
      integer npol
c
      integer i
      integer ival
      double precision pval
      double precision xpol(npol)
      double precision xval
c
      pval=1.0
      do i=1,npol
        if(i.ne.ival)then
          pval=pval*(xval-xpol(i))/(xpol(ival)-xpol(i))
        endif
      enddo
 
      return
      end
      function lenchr(string)
c
c***********************************************************************
c
cc LENCHR returns the length of STRING up to the last nonblank
c  character.
c
c
c  STRING Input, character*(*) STRING, the string to be measured.
c
c  LENCHR Output, integer LENCHR, the location of the last nonblank
c         character in STRING.
c
      implicit double precision (a-h,o-z)
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
c***********************************************************************
c
cc LEQI is a case insensitive comparison of two strings for
c  equality.  Thus, LEQI('Anjana','ANJANA') is .TRUE.
c
c
c  STRNG1,
c  STRNG2 Input, character*(*) STRNG1, STRNG2, the strings to
c         compare.
c
c  LEQI   Output, logical LEQI, the result of the comparison.
c
      implicit double precision (a-h,o-z)
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
      function leqidb(strng1,strng2)
c
c***********************************************************************
c
cc LEQIDB is a case insensitive comparison of two strings for
c  equality, ignoring blanks.  
c
c  Thus, LEQIDB('Nor Way','NORway') is .TRUE.
c
c  STRNG1,
c  STRNG2 Input, CHARACTER*(*) STRNG1, STRNG2, the strings to
c         compare.
c
c  LEQIDB Output, LOGICAL LEQIDB, the result of the comparison.
c
      integer i1
      integer i2
      integer len1
      integer len2
      logical leqidb
      character*1 s1
      character*1 s2
      character*(*) strng1
      character*(*) strng2
c
      intrinsic char
      intrinsic len
      intrinsic min
c
      len1=len(strng1)
      len2=len(strng2)

      leqidb=.false.
      
      i1=0
      i2=0
 
10    continue
c
c  If we've matched all the nonblank characters in both strings,
c  then return with LEQIDB=.TRUE.
c
      if(i1.eq.len1.and.i2.eq.len2)then
        leqidb=.true.
        return
      endif
c
c  Get S1, the next nonblank character in the first string.
c
20    continue

      i1=i1+1
      if(i1.gt.len1)return
      
      if(strng1(i1:i1).eq.' ')go to 20
      s1=strng1(i1:i1)
c
c  Get S2, the next nonblank character in the second string.
c
30    continue

      i2=i2+1
      if(i2.gt.len2)return
      
      if(strng2(i2:i2).eq.' ')go to 30
      s2=strng2(i2:i2)
      
      if(s1.ne.s2)return

      go to 10

      end      
      function nbinom(m,n)
c
c***********************************************************************
c
cc NBINOM calculates the number of combinations of M things taken N
c  at a time.  NBINOM is ACM algorithm 160 translated to FORTRAN.  
c
c  The formula used is
c
c    NBINOM = M! / ( N! * (M-N)! ) 
c
c  This value is calculated in a way that tries to avoid overflow.
c
c
c  M      Input, INTEGER M.
c         M is the number of objects to choose from in the set,
c         or the row of Pascal's triangle in which the coefficient lies.
c         M should be zero or greater.
c
c  N      Input, INTEGER N.
c         N is the number of objects selected from the set,
c         or the column of Pascal's triangle in which the coefficient 
c         lies.  N should be 0 or greater, and no greater than M.
c
c  NBINOM Output, INTEGER NBINOM.
c         NBINOM is the number of combinations of M things taken N 
c         at a time.
c
      integer i
      integer m
      integer n
      integer n1
      integer nbinom
c
      if(m.lt.0)then
        nbinom=0
        return
      endif
      
      if(n.lt.0)then
        nbinom=0
        return
      endif
      
      if(n.gt.m)then
        nbinom=0
        return
      endif
c
      if(n.lt.m-n)then
        n1=m-n
      else
        n1=n
      endif
      
      nbinom=1

      do i=1,m-n1
        nbinom=(nbinom*(n1+i)) / i
      enddo
        
      return
      end
      subroutine nrmflo(gfl,indx,neqnfl,np,resfl)
c 
c***********************************************************************
c
cc NRMFLO returns norms of a flow solution or flow residual.
c  
c  GFL    Input, double precision GFL(NEQNFL).
c
c         GFL is the current solution vector, in which are stored 
c         the finite element coefficients that define the velocity
c         and pressure functions, U, V and P.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  NEQNFL Input, integer NEQNFL, the number of finite element equations used
c         to define the horizontal and vertical velocities and the
c         pressure.
c 
c  NP     Input, integer NP, the number of nodes.
c
      implicit double precision (a-h,o-z)
c
      integer neqnfl
      integer np
c
      double precision anrmf
      double precision anrmfp
      double precision anrmfu
      double precision anrmfv
      double precision anrmg
      double precision anrmp
      double precision anrmu
      double precision anrmv
      double precision enrmf
      double precision enrmfp
      double precision enrmfu
      double precision enrmfv
      double precision enrmg
      double precision enrmp
      double precision enrmu
      double precision enrmv
      double precision fp
      double precision fu
      double precision fv
      double precision gfl(neqnfl)
      integer i
      integer indx(3,np)
      integer inrmf
      integer inrmfp
      integer inrmfu
      integer inrmfv
      integer inrmg
      integer inrmp
      integer inrmu
      integer inrmv
      double precision p
      double precision resfl(neqnfl)
      double precision u
      double precision v
c
      anrmf=0.0
      anrmfp=0.0
      anrmfu=0.0
      anrmfv=0.0
      anrmg=0.0
      anrmp=0.0
      anrmu=0.0
      anrmv=0.0
      enrmfp=0.0
      enrmf=0.0
      enrmfu=0.0
      enrmfv=0.0
      enrmg=0.0
      enrmp=0.0
      enrmu=0.0
      enrmv=0.0
      inrmf=1
      inrmfp=1
      inrmfu=1
      inrmfv=1
      inrmg=1
      inrmp=1
      inrmu=1
      inrmv=1

      do i=1,np

        u=gfl(indx(1,i))
        enrmu=enrmu+u**2
        enrmg=enrmg+u**2
        if(abs(u).gt.anrmu)then
          anrmu=abs(u)
          inrmu=i
        endif
        if(abs(u).gt.anrmg)then
          anrmg=abs(u)
          inrmg=i
        endif
 
        fu=resfl(indx(1,i))
        enrmf=enrmf+fu**2
        enrmfu=enrmfu+fu**2
        if(abs(fu).gt.anrmf)then
          anrmf=abs(fu)
          inrmf=i
        endif
        if(abs(fu).gt.anrmfu)then
          anrmfu=abs(fu)
          inrmfu=i
        endif
 
        v=gfl(indx(2,i))
        enrmv=enrmv+v**2
        enrmg=enrmg+v**2
        if(abs(v).ge.anrmv)then
          anrmv=abs(v)
          inrmv=i
        endif
        if(abs(v).ge.anrmg)then
          anrmg=abs(v)
          inrmg=i
        endif
 
        fv=resfl(indx(2,i))
        enrmf=enrmf+fv**2
        enrmfv=enrmfv+fv**2
        if(abs(fv).gt.anrmf)then
          anrmf=abs(fv)
          inrmf=i
        endif
        if(abs(fv).gt.anrmfv)then
          anrmfv=abs(fv)
          inrmfv=i
        endif
 
        if(indx(3,i).gt.0)then
 
          p=gfl(indx(3,i))
          enrmp=enrmp+p**2
          enrmg=enrmg+p**2
          if(abs(p).ge.anrmp)then
            inrmp=i
            anrmp=abs(p)
          endif
          if(abs(p).ge.anrmg)then
            inrmg=i
            anrmg=abs(p)
          endif
 
          fp=resfl(indx(3,i))
          enrmf=enrmf+fp**2
          enrmfp=enrmfp+fp**2
          if(abs(fp).gt.anrmf)then
            inrmf=i
            anrmf=abs(fp)
          endif
          if(abs(fp).gt.anrmfp)then
            anrmfp=abs(fp)
            inrmfp=i
          endif
 
        endif

      enddo

      enrmf=sqrt(enrmf)
      enrmfp=sqrt(enrmfp)
      enrmfu=sqrt(enrmfu)
      enrmfv=sqrt(enrmfv)
      enrmg=sqrt(enrmg)
      enrmp=sqrt(enrmp)
      enrmu=sqrt(enrmu)
      enrmv=sqrt(enrmv)
c
c  Print out results.
c
      write(*,*)' '
      write(*,*)'     MxNorm       Node    l2 Norm'
      write(*,*)' '
      write(*,'(''U  '',g14.6,i6,g14.6)')anrmu,inrmu,enrmu
      write(*,'(''V  '',g14.6,i6,g14.6)')anrmv,inrmv,enrmv
      write(*,'(''P  '',g14.6,i6,g14.6)')anrmp,inrmp,enrmp
      write(*,'(''GFL'',g14.6,i6,g14.6)')anrmg,inrmg,enrmg
      write(*,'(''FU '',g14.6,i6,g14.6)')anrmfu,inrmfu,enrmfu
      write(*,'(''FV '',g14.6,i6,g14.6)')anrmfv,inrmfv,enrmfv
      write(*,'(''FP '',g14.6,i6,g14.6)')anrmfp,inrmfp,enrmfp
      write(*,'(''F  '',g14.6,i6,g14.6)')anrmf,inrmf,enrmf
 
      return
      end
      subroutine pcval(nvec,xval,xvec,yval,yvec)
c
c***********************************************************************
c
cc PCVAL evaluates a piecewise constant function at a given point.
c
c  The piecewise constant function is specified as suggested by the
c  following graph:
c
c
c  Y(2)->              *---------*
c                      |         |
c  Y(1)->   *----------*         |
c                                |
c                                |
c  Y(3)->                        *---------......
c                              
c           ^          ^         ^
c           |          |         |
c         X(1)        X(2)      X(3)
c
c  Note that if XVAL falls to the left of XVEC(1), then YVAL=YVEC(1).
c
c
c  NVEC   Input, integer NVEC, the number of abscissas and coefficients
c         that define the piecewise constant function.  NVEC must be at 
c         least 1.
c
c  XVAL   Input, double precision XVAL, the point at which the function
c         is to be evaluated.
c
c  XVEC   Input, double precision XVEC(NVEC), the abscissas of the
c         function.  These should be distinct and in ascending order.
c
c  YVAL   Output, double precision YVAL, the value of the piecewise
c         constant function at the point XVAL.
c
c  YVEC   Input, double precision YVEC(NVEC), the value of the piecewise
c         constant function at each of the abscissas.
c
      implicit double precision (a-h,o-z)
c
      integer nvec
c
      integer i
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
          yval=xvec(i)
          return
        endif
 
      enddo
 
      write(*,*)' '
      write(*,*)'PCVal - Fatal error!'
      write(*,*)'  Could not bracket XVAL=',xval
      stop
      end
      subroutine pldx(nvec,xval,xvec,yder,yvec)
c
c***********************************************************************
c
cc PLDX evaluates the derivative of a piecewise linear function with
c  respect to its argument at a given point.
c
c  Note that if XVAL falls to the left of XVEC(1), then YDER=0,
c  and similarly, if XDER is greater than XVEC(NVEC), YVAL=0.
c
c
c  NVEC   Input, integer NVEC, the number of abscissas and coefficients
c         that define the piecewise linear.  NVEC must be odd, and
c         at least 3.
c
c  XVAL   Input, double precision XVAL, the point at which the
c         derivative with respect to X is to be evaluated.
c
c  XVEC   Input, double precision XVEC(NVEC), the abscissas of the 
c         function.  These should be distinct and in ascending order.
c
c  YDER   Output, double precision YDER, the value of the derivative of
c         the piecewise linear function with respect to X, at the point
c         XVAL.
c
c  YVEC   Input, double precision YVEC(NVEC), the value of the
c         piecewise linear function at each of the abscissas.
c
      implicit double precision (a-h,o-z)
c
      integer nvec
c
      integer i
      integer ival
      double precision xval
      double precision xvec(nvec)
      double precision yder
      double precision yvec(nvec)
c
c  Step 1: Check if XVAL lies outside the intervals.
c
      if(xval.le.xvec(1))then
        yder=0
        return
      elseif(xval.ge.xvec(nvec))then
        yder=0
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
c  Step 3: Evaluate the slope of the linear function at XVAL.
c
      i=ival
 
      yder=(yvec(i+1)-yvec(i))/(xvec(i+1)-xvec(i))
 
      return
      end
      subroutine pldx1(ivec,nvec,xval,xvec,yder)
c
c***********************************************************************
c
cc PLDX1 evaluates the X derivative of the piecewise linear
c  polynomial which is 1 at the IVEC-th node and 0 at the others.
c
c  Note that if XVAL falls to the left of XVEC(1), then YDER=0,
c  and similarly, if XVAL is greater than XVEC(NVEC), YDER=0.
c
c
c  IVEC   Input, integer IVEC, the coefficient with respect to which
c         the partial derivative is desired.
c
c  NVEC   Input, integer NVEC, the number of abscissas and coefficients
c         that define the piecewise linear.  NVEC must be odd, and
c         at least 3.
c
c  XVAL   Input, double precision XVAL, the point at which the function
c         is to be evaluated.
c
c  XVEC   Input, double precision XVEC(NVEC), the abscissas of the 
c         function.  These should be distinct and in ascending order.
c
c  YDER   Output, double precision YDER, the value of the derivative of
c         the piecewise linear function at the point XVAL.
c
      implicit double precision (a-h,o-z)
c
      integer nvec
c
      integer i
      integer ival
      integer ivec
      double precision xval
      double precision xvec(nvec)
      double precision yder
c
c  Step 1: Check if XVAL lies outside the intervals.
c
      if(xval.le.xvec(1))then
        yder=0.0
        return
      elseif(xval.ge.xvec(nvec))then
        yder=0.0
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
      write(*,*)'PLDX1 - Fatal error!'
      write(*,*)'  Could not bracket XVAL=',xval
      stop
 
10    continue
c
c  Step 3: Evaluate the slope of the IVEC-th linear function at XVAL.
c
      i=ival
      if(ival.eq.ivec)then
        yder=(0.0-1.0)/(xvec(ival+1)-xvec(ival))
      elseif(ival+1.eq.ivec)then
        yder=(1.0-0.0)/(xvec(ival+1)-xvec(ival))
      else
        yder=0.0
      endif
 
      return
      end
      subroutine pltopn(disfil,igunit)
c
c***********************************************************************
c
cc PLTOPN opens the plotting file.
c
c
c  DISFIL Input, character*30 DISFIL.
c
c         DISFIL contains the name of the file into which the DISPLAY
c         graphics information will be stored.
c
c  IGUNIT Input/output, integer IGUNIT.
c
c         On input, if IGUNIT is zero, then the routine believes
c         that the graphics unit has not yet been opened.
c
c         If the FORTRAN unit has already been opened, then IGUNIT
c         should be nonzero, and the routine will know not to try
c         to open the file, since it is already open.
c
c         On output, IGUNIT is the FORTRAN unit used for writing data
c         to the plotfile FILEG.
c
      implicit double precision (a-h,o-z)
c
      character*30 disfil
      integer igunit
c
c  If IGUNIT is not zero, then the graphics unit has already
c  been opened.
c
      if(igunit.eq.0)then
 
        write(*,*)' '
        write(*,*)'PltOpn - Note:'
        write(*,*)'  Opening the DISPLAY plot file '//disfil
        write(*,*)' '
c
c  Delete any old copy of the file.
c
        igunit=11
        open(unit=igunit,file=disfil,status='unknown',
     &    form='formatted',access='sequential',err=10)
 
        return
c
c  Write a warning if the plot file could not be opened.
c
10      continue
 
        write(*,*)' '
        write(*,*)'PltOpn - Warning!'
        write(*,*)'  The plot file could not be opened.'
        igunit=0
c
c  Else plotfile is already open.
c
      else
        write(*,*)' '
        write(*,*)'PltOpn - Note'
        write(*,*)'  The plot file is already open.'
        write(*,*)'  New information will be appended to it.'
      endif
 
      return
      end
      subroutine plval(nvec,xval,xvec,yval,yvec)
c
c***********************************************************************
c
cc PLVAL evaluates a piecewise linear function at a given point.
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
      subroutine plval1(ivec,nvec,xval,xvec,yval)
c
c***********************************************************************
c
cc PLVAL1 evaluates the piecewise linear polynomial which is 1
c  at node IVEC and 0 at the other nodes.
c
c  Note that if XVAL falls to the left of XVEC(1), then YVAL=0,
c  and similarly, if XVAL is greater than XVEC(NVEC), YVAL=0.
c
c
c  IVEC   Input, integer IVEC, the coefficient with respect to which
c         the partial derivative is desired.
c
c  NVEC   Input, integer NVEC, the number of abscissas and coefficients
c         that define the piecewise linear.  NVEC must be odd, and
c         at least 3.
c
c  XVAL   Input, double precision XVAL, the point at which the function
c         is to be evaluated.
c
c  XVEC   Input, double precision XVEC(NVEC), the abscissas of the
c         function.  These should be distinct and in ascending order.
c
c  YDER   Output, double precision YDER, the value of the derivative of
c         the piecewise linear function at the point XVAL.
c
      implicit double precision (a-h,o-z)
c
      integer nvec
c
      integer i
      integer ival
      integer ivec
      double precision xval
      double precision xvec(nvec)
      double precision yval
c
c  Step 1: Check if XVAL lies outside the intervals.
c
      if(xval.le.xvec(1))then
        yval=0.0
        return
      elseif(xval.ge.xvec(nvec))then
        yval=0.0
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
      write(*,*)'PLVAL1 - Fatal error!'
      write(*,*)'  Could not bracket XVAL=',xval
      stop
 
10    continue
c
c  Step 3: Determine the index of the left endpoint of the least and
c  greatest intervals that IVEC can affect.
c
      i=ival
      if(ival.eq.ivec)then
        if(xval.eq.xvec(ival))then
          yval=1.0
        else
          yval=(xvec(ival+1)-xval)/(xvec(ival+1)-xvec(ival))
        endif
      elseif(ival+1.eq.ivec)then
        if(xval.eq.xvec(ival+1))then
          yval=1.0
        else
          yval=(xval-xvec(ival))/(xvec(ival+1)-xvec(ival))
        endif
      else
        yval=0.0
      endif
 
      return
      end
      subroutine pqdx(nvec,xval,xvec,yder,yvec)
c
c***********************************************************************
c
cc PQDX evaluates the derivative of a piecewise quadratic function with
c  respect to its argument at a given point.
c
c  Note that if XDER falls to the left of XVEC(1), then YVAL=0,
c  and similarly, if XVAL is greater than XVEC(NVEC), YDER=0.
c
c
c  NVEC   Input, integer NVEC, the number of abscissas and coefficients
c         that define the piecewise quadratic.  NVEC must be odd, and
c         at least 3.
c
c  XVAL   Input, double precision XVAL, the point at which the 
c         derivative with respect to X is to be evaluated.
c
c  XVEC   Input, double precision XVEC(NVEC), the abscissas of the 
c         function.  These should be distinct and in ascending order.
c
c  YDER   Output, double precision YDER, the value of the derivative
c         of the piecewise  quadratic function with respect to X,
c         at the point XVAL.
c
c  YVEC   Input, double precision YVEC(NVEC), the value of the piecewise
c         quadratic function at each of the abscissas.
c
      implicit double precision (a-h,o-z)
c
      integer nvec
c
      integer i
      integer ival
      double precision xval
      double precision xvec(nvec)
      double precision yder
      double precision yvec(nvec)
c
c  Step 0: Check data.
c
      if(nvec.lt.3)then
        write(*,*)' '
        write(*,*)'PQDX - Fatal error.'
        write(*,*)'  NVEC is ',nvec
        write(*,*)'  but NVEC must be at least 3.'
        stop
      endif
 
      if(mod(nvec,2).ne.1)then
        write(*,*)' '
        write(*,*)'PQDX - Fatal error!'
        write(*,*)'  Even value of NVEC=',nvec
        stop
      endif
c
c  Step 1: Find odd index I so that XVEC(I) <= XVAL < XVEC(I+2)
c
      if(xval.le.xvec(1))then
        yder=yvec(1)
        return
      elseif(xval.ge.xvec(nvec))then
        yder=yvec(nvec)
        return
      endif
 
      do i=1,nvec-2,2
 
        if(xvec(i).le.xval.and.xval.le.xvec(i+2))then
          ival=i
          go to 10
        endif
 
      enddo
 
      write(*,*)' '
      write(*,*)'PQDX - Fatal error!'
      write(*,*)'  Could not bracket XVAL=',xval
      stop
 
10    continue
c
c  Step 2: Evaluate the derivative of the quadratic function at XVAL.
c
      i=ival
 
      yder=yvec(i)*(2*xval-xvec(i+1)-xvec(i+2))
     &    /((xvec(i)-xvec(i+1))*(xvec(i)-xvec(i+2)))
     &    +yvec(i+1)*(2*xval-xvec(i)-xvec(i+2))
     &    /((xvec(i+1)-xvec(i))*(xvec(i+1)-xvec(i+2)))
     &    +yvec(i+1)*(2*xval-xvec(i)-xvec(i+2))
     &    /((xvec(i+1)-xvec(i))*(xvec(i+1)-xvec(i+2)))
 
      return
      end
      subroutine pqdx1(ivec,nvec,xval,xvec,yder)
c
c***********************************************************************
c
cc PQDX1 evaluates the X derivative of the piecewise quadratic
c  polynomial which is 1 at the IVEC-th node and 0 at the others.
c
c  Note that if XVAL falls to the left of XVEC(1), then YDER=0,
c  and similarly, if XVAL is greater than XVEC(NVEC), YDER=0.
c
c
c  IVEC   Input, integer IVEC, the coefficient with respect to which
c         the partial derivative is desired.
c
c  NVEC   Input, integer NVEC, the number of abscissas and coefficients
c         that define the piecewise quadratic.  NVEC must be odd, and
c         at least 3.
c
c  XVAL   Input, double precision XVAL, the point at which the function
c         be evaluated.
c
c  XVEC   Input, double precision XVEC(NVEC), the abscissas of the 
c         function.  These should be distinct and in ascending order.
c
c  YDER   Output, double precision YDER, the value of the derivative of
c         the piecewise quadratic function at the point XVAL.
c
      implicit double precision (a-h,o-z)
c
      integer nvec
c
      integer i
      integer ihi
      integer ilo
      integer ival
      integer ivec
      double precision xval
      double precision xvec(nvec)
      double precision yder
c
c  Step 0: Check data.
c
      if(nvec.lt.3)then
        write(*,*)' '
        write(*,*)'PQDX1 - Fatal error!'
        write(*,*)'  NVEC=',nvec
        write(*,*)'  but NVEC must be at least 3.'
        stop
      endif
 
      if(mod(nvec,2).ne.1)then
        write(*,*)' '
        write(*,*)'PQDX1 - Fatal error!'
        write(*,*)'  Even value of NVEC=',nvec
        stop
      endif
c
c  Step 1: Find odd index I so that XVEC(I) <= XVAL < XVEC(I+2)
c
      if(xval.le.xvec(1))then
        yder=0
        return
      elseif(xval.ge.xvec(nvec))then
        yder=0
        return
      endif
 
      do i=1,nvec-2,2
 
        if(xvec(i).le.xval.and.xval.le.xvec(i+2))then
          ival=i
          go to 10
        endif
 
      enddo
 
      write(*,*)' '
      write(*,*)'PQDX1 - Fatal error!'
      write(*,*)'  Could not bracket XVAL=',xval
      stop
 
10    continue
c
c  Step 2: Determine the index of the left endpoint of the least and
c  greatest intervals that IVEC can affect.
c
      if(mod(ivec,2).eq.0)then
        ilo=ivec-1
        ihi=ivec-1
      else
        ilo=max(ivec-2,1)
        ihi=ivec
      endif
c
c  Step 3: If XVAL is outside of the intervals that IVEC can affect,
c  the derivative is zero.
c
      if(ival.lt.ilo.or.ival.gt.ihi)then
        yder=0
        return
      endif
c
c  Step 3: Evaluate the derivative of the quadratic function at XVAL.
c
      i=ival
 
      if(ivec.eq.ival)then
        yder=(2*xval-xvec(i+1)-xvec(i+2))
     &      /((xvec(i)-xvec(i+1))*(xvec(i)-xvec(i+2)))
      elseif(ivec.eq.ival+1)then
         yder=(2*xval-xvec(i)-xvec(i+2))
     &    /((xvec(i+1)-xvec(i))*(xvec(i+1)-xvec(i+2)))
      elseif(ivec.eq.ival+2)then
          yder=(2*xval-xvec(i)-xvec(i+1))
     &    /((xvec(i+2)-xvec(i))*(xvec(i+2)-xvec(i+1)))
      else
        write(*,*)' '
        write(*,*)'PQDX1 - Fatal error!'
        write(*,*)'  IVEC=',ivec
        write(*,*)'  IVAL=',ival
      endif
 
      return
      end
      subroutine pqval(nvec,xval,xvec,yval,yvec)
c
c***********************************************************************
c
cc PQVAL evaluates a piecewise quadratic function at a given point.
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
      subroutine pqval1(ivec,nvec,xval,xvec,yval)
c
c***********************************************************************
c
cc PQVAL1 evaluates the piecewise quadratic polynomial which is 1
c  at node IVEC and 0 at the other nodes.
c
c  Note that if XVAL falls to the left of XVEC(1), then YVAL=0,
c  and similarly, if XVAL is greater than XVEC(NVEC), YVAL=0.
c
c
c  IVEC   Input, integer IVEC, the coefficient with respect to which
c         the partial derivative is desired.
c
c  NVEC   Input, integer NVEC, the number of abscissas and coefficients
c         that define the piecewise quadratic.  NVEC must be odd, and
c         at least 3.
c
c  XVAL   Input, double precision XVAL, the point at which the function
c         is to be evaluated.
c
c  XVEC   Input, double precision XVEC(NVEC), the abscissas of the 
c         function.  These should be distinct and in ascending order.
c
c  YDER   Output, double precision YDER, the value of the derivative of
c         the piecewise quadratic function at the point XVAL.
c
      implicit double precision (a-h,o-z)
c
      integer nvec
c
      integer i
      integer ihi
      integer ilo
      integer ival
      integer ivec
      double precision xval
      double precision xvec(nvec)
      double precision yval
c
c  Step 0: Check data.
c
      if(nvec.lt.3)then
        write(*,*)' '
        write(*,*)'PQVal1 - Fatal error!'
        write(*,*)'  Value of NVEC is ',nvec
        write(*,*)'  but NVEC must be at least 3.'
        stop
      endif
 
      if(mod(nvec,2).ne.1)then
        write(*,*)' '
        write(*,*)'PQVal1 - Fatal error!'
        write(*,*)'  Even value of NVEC=',nvec
        stop
      endif
c
c  Step 1: Find odd index I so that XVEC(I) <= XVAL < XVEC(I+2)
c
      if(xval.le.xvec(1))then
        yval=0.0
        return
      elseif(xval.ge.xvec(nvec))then
        yval=0.0
        return
      endif
 
      do i=1,nvec-2,2
 
        if(xvec(i).le.xval.and.xval.le.xvec(i+2))then
          ival=i
          go to 10
        endif
 
      enddo
 
      write(*,*)' '
      write(*,*)'PQVal1 - Fatal error!'
      write(*,*)'  Could not bracket XVAL=',xval
      stop
 
10    continue
c
c  Step 2: Determine the index of the left endpoint of the least and
c  greatest intervals that IVEC can affect.
c
      if(mod(ivec,2).eq.0)then
        ilo=ivec-1
        ihi=ivec-1
      else
        ilo=max(ivec-2,1)
        ihi=ivec
      endif
c
c  Step 3: If XVAL is outside of the intervals that IVEC can affect,
c  the value is zero.
c
      if(ival.lt.ilo.or.ival.gt.ihi)then
        yval=0
        return
      endif
c
c  Step 3: Evaluate the quadratic function at XVAL.
c
      i=ival
 
      if(ivec.eq.ival)then
        yval=(xval-xvec(i+1)) * (xval-xvec(i+2))
     &      /((xvec(i)-xvec(i+1))*(xvec(i)-xvec(i+2)))
      elseif(ivec.eq.ival+1)then
         yval=(xval-xvec(i)) * (xval-xvec(i+2))
     &    /((xvec(i+1)-xvec(i))*(xvec(i+1)-xvec(i+2)))
      elseif(ivec.eq.ival+2)then
          yval=(xval-xvec(i)) * (xval-xvec(i+1))
     &    /((xvec(i+2)-xvec(i))*(xvec(i+2)-xvec(i+1)))
      else
        write(*,*)' '
        write(*,*)'PQVal1 - Fatal error!'
        write(*,*)'  IVEC=',ivec
        write(*,*)'  IVAL=',ival
      endif
 
      return
      end
      subroutine prbmat(afl,ihi,ilo,jhi,jlo,ldafl,neqnfl,nlband)
c
c***********************************************************************
c
cc PRBMAT prints all nonzero entries of rows ILO to IHI, columns JLO to 
c  JHI of a square band matrix.
c
c
c  AFL    Input, double precision AFL(LDAFL,MAXNFL).
c         If Newton iteration is being carried out, then AFL contains the 
c         Jacobian matrix for the full system.  If Picard iteration is
c         being carried out, AFL contains the Picard matrix.
c         AFL is stored in LINPACK general band storage mode, with
c         dimension 3*NBANDL+1 by NEQNFL.
c
c  IHI,
c  ILO,
c  JHI,
c  JLO    Input, integer IHI, ILO, JHI, JLO.
c         PRMAT is to print all nonzero entries in rows ILO through IHI,
c         and columns JLO through JHI, of the matrix AFL.
c
c  LDAFL  Input, integer LDAFL.
c         LDAFL is the first dimension of the matrix AFL as declared in
c         the main program.  LDAFL must be at least 3*NLBAND+1.
c
c  NEQNFL Input, integer NEQNFL.
c         NEQNFL is the number of equations (and coefficients) in the full 
c         finite element system.
c
c  NLBAND Input, integer NLBAND, the lower bandwidth of the matrix AFL.  
c         The zero structure of AFL is assumed to be symmetric, and so 
c         NLBAND is also the upper bandwidth of AFL.  
c 
      implicit double precision (a-h,o-z)
      integer incx
      parameter (incx=5)
c
      integer ldafl
      integer neqnfl
c
      double precision afl(ldafl,neqnfl)
      character*14 ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      integer nlband
c
      do j2lo=jlo,jhi,incx
 
        j2hi=j2lo+incx-1
        j2hi=min(j2hi,neqnfl)
        j2hi=min(j2hi,jhi)
 
        inc=j2hi+1-j2lo
 
        write(*,*)' '
        do j=j2lo,j2hi
          j2=j+1-j2lo
          write(ctemp(j2),'(i7,7x)')j
        enddo
        write(*,'(''Columns '',5a14)')(ctemp(j2),j2=1,inc)
c       write(*,*)'Columns ',j2lo,' to ',j2hi
        write(*,*)'  Row'
        write(*,*)' '

        i2lo=ilo
        i2lo=max(ilo,1)
        i2lo=max(j2lo-nlband,i2lo)

        i2hi=ihi
        i2hi=min(ihi,neqnfl)
        i2hi=min(j2hi+nlband,i2hi)
 
        do i=i2lo,i2hi
 
          do j2=1,inc
 
            j=j2lo-1+j2
 
            if(i-j.le.nlband.and.j-i.le.nlband)then
              write(ctemp(j2),'(g14.6)')afl(i-j+2*nlband+1,j)
              if(afl(i-j+2*nlband+1,j).eq.0.0)ctemp(j2)='    0.0'
            else
              ctemp(j2)='              '
            endif
 
          enddo
 
          write(*,'(i5,1x,5a14)')i,(ctemp(j2),j2=1,inc)
 
        enddo
 
      enddo
 
      write(*,*)' '
 
      return
      end
      subroutine prdat(disfil,drey,epsdif,gridx,gridy,hx,hy,ibs,
     &  ibump,ifs,ijac,iopt,maxnew,maxopt,maxsim,
     &  nbcrb,ncofrb,nelem,nferb,neqnfl,np,npar,nparb,nparf,ntay,
     &  nx,ny,region,reytay,
     &  tecfil,tolnew,tolopt,tolsim,wateb,watep,wateu,watev,xbl,
     &  xbr,xprof,
     &  xrange,ybl,ybr,yrange)
c
c***********************************************************************
c
cc PRDAT prints the problem information.
c
      implicit double precision (a-h,o-z)
c
      integer npar
c
      character*30 disfil
      double precision drey
      double precision epsdif
      character*20 gridx
      character*20 gridy
      double precision hx
      double precision hy
      integer i
      integer ibs
      integer ibump
      integer ifs
      integer ijac
      integer iopt(npar)
      integer maxnew
      integer maxopt
      integer maxsim
      integer nbcrb
      integer ncofrb
      integer nelem
      integer neqnfl
      integer nferb
      integer np
      integer nparb
      integer nparf
      integer ntay
      integer nx
      integer ny
      character*20 region
      double precision reytay
      character*30 tecfil
      double precision tolnew
      double precision tolopt
      double precision tolsim
      character*6 type
      double precision wateb
      double precision watep
      double precision wateu
      double precision watev
      double precision xbl
      double precision xbr
      double precision xprof
      double precision xrange
      double precision ybl
      double precision ybr
      character*3 yesno
      double precision yrange
c
      write(*,*)' '
      write(*,*)'  DISPLAY graphics file is DISFIL=             '
     &  //disfil
      write(*,*)'  REYNLD increment for finite differences DREY=',drey
      write(*,*)'  Finite difference perturbation EPSDIF=       ',epsdif
      write(*,*)'  X grid generation option GRIDX='//gridx
      write(*,*)'  Y grid generation option GRIDY='//gridy
      write(*,*)'  X spacing, HX=  ',hx
      write(*,*)'  Y spacing, HY=  ',hy
      write(*,*)'  Bump piecewise polynomial order IBS=',ibs
      write(*,*)'  Bump option IBUMP=                  ',ibump
      write(*,*)'  Flow piecewise polynomial order IFS=',ifs
      write(*,*)'  Jacobian option IJAC=               ',ijac
      write(*,*)' '
      write(*,*)'  Variable  Type  Free to Vary?'
      write(*,*)' '
      do i=1,npar
        if(i.le.nparf)then
          type='Inflow'
        elseif(i.le.nparf+nparb)then
          type='Shape'
        else
          type='Reynld'
        endif
        if(iopt(i).eq.0)then
          yesno='No'
        else
          yesno='Yes'
        endif
        write(*,'(6x,i5,2x,a6,2x,a3)')i,type,yesno
      enddo
      write(*,*)' '
      write(*,*)'  Maximum Newton iterations MAXNEW=   ',maxnew
      write(*,*)'  Maximum optimization steps MAXOPT=  ',maxopt
      write(*,*)'  Maximum Newton iterations MAXSIM=   ',maxsim
      write(*,*)'  # of RB boundary conditions NBCRB=  ',nbcrb
      write(*,*)'  Number of reduced equations, NCOFRB=',ncofrb
      write(*,*)'  Number of elements, NELEM=          ',nelem
      write(*,*)'  Number of full equations, NEQNFL=   ',neqnfl
      write(*,*)'  # of FE reduced basis cofs, NFERB=  ',nferb
      write(*,*)'  Number of nodes, NP=                ',np
      write(*,*)'  Number of parameters NPAR=          ',npar
      write(*,*)'  Number of inflow parameters NPARF=  ',nparf
      write(*,*)'  Number of Taylor vectors NTAY=      ',ntay
      write(*,*)'  Number of bump parameters NPARB=    ',nparb
      write(*,*)'  Number of X elements, NX=           ',nx
      write(*,*)'  Number of Y elements, NY=           ',ny
      write(*,*)'  The flow region is REGION='//region
      write(*,*)'  REYNLD value for Taylor, REYTAY=    ',reytay
      write(*,*)'  TECPLOT graphics file is TECFIL=             '
     &  //tecfil
      write(*,*)'  Newton convergence tolerance TOLNEW=',tolnew
      write(*,*)'  Optimization tolerance TOLOPT=      ',tolopt
      write(*,*)'  Picard convergence tolerance TOLSIM=',tolsim
      write(*,*)'  Bump control cost,   WATEB=         ',wateb
      write(*,*)'  Pressure discrepancy, WATEP=        ',watep
      write(*,*)'  U discrepancy, WATEU=               ',wateu
      write(*,*)'  V discrepancy, WATEV=               ',watev
      write(*,*)'  Left X of bump, XBL=                ',xbl
      write(*,*)'  Right X of bump, XBR=               ',xbr
      write(*,*)'  Flow profile measured at XPROF=     ',xprof
      write(*,*)'  X range, XRANGE=                    ',xrange
      write(*,*)'  Left Y of bump, YBL=                ',ybl
      write(*,*)'  Right Y of bump, YBR=               ',ybr
      write(*,*)'  Y range, YRANGE=                    ',yrange

      return
      end
      subroutine prdmat(a,ihi,ilo,jhi,jlo,mhi,mlo,nhi,nlo)
c
c***********************************************************************
c
cc PRDMAT prints out a portion of a dense matrix.
c
c
c  This routine was modified on 11 July 1996.
c
c
c  A      Input, REAL A(MLO:MHI,NLO:NHI).
c         A contains the matrix to be printed.
c
c  IHI,
c  ILO    Input, INTEGER IHI, ILO.
c         ILO is the first and IHI the last row to print.
c
c  JHI,
c  JLO    Input, INTEGER JHI, JLO.
c         JLO is the first, and JHI the last column to print.
c
c  MHI,
c  MLO    Input, INTEGER MHI, MLO.
c         The rows of A go from MLO to MHI.
c
c  NHI,
c  NLO    Input, INTEGER NHI, NLO.
c         The columns of A go from NLO to NHI.
c
      implicit double precision (a-h,o-z)
c
      integer incx
      parameter (incx=5)
c
      integer mhi
      integer mlo
      integer nhi
      integer nlo
c
      double precision a(mlo:mhi,nlo:nhi)
      character ctemp(incx)*14
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
c
      write(*,*)' '

      do j2lo=jlo,jhi,incx
 
        j2hi=j2lo+incx-1
        if(j2hi.gt.nhi)then
          j2hi=nhi
        endif
 
        if(j2hi.gt.jhi)then
          j2hi=jhi
        endif
 
        inc=j2hi+1-j2lo
 
        write(*,*)' '
        do j=j2lo,j2hi
          j2=j+1-j2lo
          write(ctemp(j2),'(i7,7x)')j
        enddo
        write(*,'(''Columns '',5a14)')(ctemp(j2),j2=1,inc)
        write(*,*)'  Row'
        write(*,*)' '
 
        i2lo=max(ilo,mlo)
        i2hi=min(ihi,mhi)
 
        do i=i2lo,i2hi
 
          do j2=1,inc
 
            j=j2lo-1+j2
            
            write(ctemp(j2),'(g14.6)')a(i,j)
            if(a(i,j).eq.0.0)ctemp(j2)='    0.0'
 
          enddo
 
          write(*,'(i5,1x,5a14)')i,(ctemp(j),j=1,inc)
 
        enddo
 
      enddo
 
      write(*,*)' '
 
      return
      end
      subroutine prelem(ihi,ilo,nelem,node,np,xc,yc)
c
c***********************************************************************
c
cc PRELEM prints out data about one or more elements.
c
c
c  IHI,
c  ILO    Input, integer IHI, ILO.
c         ILO is the first element of interest, and IHI the last.
c
c  NELEM  Input, integer NELEM.
c         NELEM is the numberof elements.
c
c  NODE   Input, integer NODE(6,NELEM), contains the numbers
c         of the nodes that make up each element.  Element number
c         I is associated with nodes NODE(1,I) through NODE(6,I).
c
c  NP     Input, integer NP.
c
c         NP is the number of nodes.  NP=(2*NX-1)*(2*NY-1).
c
c  XC,
c  YC     Input, double precision XC(NP), YC(NP).
c         XC and YC are the X and Y coordinates of the nodes.
c
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer np
c
      integer i
      integer ielem
      integer ihi
      integer ihi2
      integer ilo
      integer ilo2
      integer ip
      integer node(6,nelem)
      double precision xc(np)
      double precision yc(np)
c
      if(ilo.lt.1)then
        ilo2=1
      else
        ilo2=ilo
      endif

      if(ihi.gt.nelem)then
        ihi2=nelem
      else
        ihi2=ihi
      endif

      do ielem=ilo2,ihi2
        write(*,*)' '
        write(*,*)'Element IELEM=',ielem
        write(*,*)' '
        write(*,*)'  Nodes:'
        write(*,*)' '
        do i=1,6
          ip=node(i,ielem)
          write(*,*)i,ip,xc(ip),yc(ip)
        enddo
      enddo
 
      return
      end
      subroutine prfxfln(neqnfl,resfl)
c
c***********************************************************************
c
cc PRFXFLN prints out the norm of a full residual.
c
c
c  NEQNFL Input, integer NEQNFL.
c         NEQNFL is the number of equations (and coefficients) in the full 
c         finite element system.
c
c  RESFL  Input, double precision RESFL(NEQNFL).
c         RESFL contains the residual in the full basis equations.
c
      implicit double precision (a-h,o-z)
c
      integer neqnfl
c
      double precision anrmr
      double precision dnrm2
      double precision enrmr
      integer idamax
      integer itemp
      double precision resfl(neqnfl)
c
      external dnrm2
      external idamax
c
      itemp=idamax(neqnfl,resfl,1)
      anrmr=abs(resfl(itemp))
      enrmr=dnrm2(neqnfl,resfl,1)
 
      write(*,*)' '
      write(*,*)'          MxNorm      l2 Norm'
      write(*,*)' '
      write(*,'(''Fx(GFL) '',2g14.6)')anrmr,enrmr

      return
      end
      subroutine prgrb(grb,ncofrb)
c
c***********************************************************************
c
cc PRGRB prints out the reduced basis solution.
c
c
c  GRB    Input, double precision GRB(NCOFRB), coefficients for the
c         reduced system.
c
c  NCOFRB Input, integer NCOFRB, the number of coefficients for the
c         reduced system.
c
      implicit double precision (a-h,o-z)
c
      integer ncofrb
c
      double precision anrmg
      double precision dnrm2
      double precision enrmg
      double precision grb(ncofrb)
      integer i
      integer idamax
      integer itemp
c
      external dnrm2
      external idamax
c
      if(ncofrb.le.0)then
        write(*,*)' '
        write(*,*)'PrGRB - Fatal error.'
        write(*,*)'  Input value of NCOFRB=',ncofrb
        stop
      endif
 
      write(*,*)' '
      write(*,*)'PrGRB - The reduced basis coefficients:'
      write(*,*)' '
      do i=1,ncofrb
        write(*,'(i6,g14.6)')i,grb(i)
      enddo

      itemp=idamax(ncofrb,grb,1)
      anrmg=abs(grb(itemp))
      enrmg=dnrm2(ncofrb,grb,1)
 
      write(*,*)' '
      write(*,*)'          MxNorm      l2 Norm'
      write(*,*)' '
      write(*,'(''GRB      '',2g14.6)')anrmg,enrmg
 
      return
      end
      subroutine prindx(ihi,ilo,indx,np,xc,yc)
c
c***********************************************************************
c
cc PRINDX prints out the integer variables that define the
c  relationships between the nodes and elements.
c
c
c  IHI,
c  ILO    Input, integer IHI, ILO.
c         ILO is the first, and IHI the last node at which the
c         information is desired.
c
c  INDX   Input, integer INDX(3,NP).
c
c         INDX contains, for each node I, the index of U, V and P at
c         that node, or 0 or a negative value.
c
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry G(K).
c
c         If INDX(I,J) is positive, then that means that a degree of
c         freedom for variable J (U, V or P) is associated with node
c         I, and an equation will be generated to determine its value.
c
c         If INDX(I,J) is zero, then that means the the value of variabl
c         J (U, V or P) has been specified at node I.  No equation is
c         generated to determine its value.
c
c  NP     Input, integer NP.
c
c         NP is the number of nodes.  NP=(2*NX-1)*(2*NY-1).
c
c  XC,
c  YC     Input, double precision XC(NP), YC(NP).
c         XC and YC are the X and Y coordinates of the nodes.
c
      implicit double precision (a-h,o-z)
c
      integer np
c
      integer i
      integer ihi
      integer ilo
      integer indx(3,np)
      double precision xc(np)
      double precision yc(np)
c
      write(*,*)' '
      write(*,*)'PrIndx:'
      write(*,*)' '
      write(*,*)' Node    X             Y              U'
     &  //'     V     P'
      write(*,*)' '
      do i=max(ilo,1),min(ihi,np)
        if(indx(3,i).ne.0)then
          write(*,'(i6,2g14.6,3i6)')i,xc(i),yc(i),
     &      indx(1,i),indx(2,i),indx(3,i)
        else
          write(*,'(i6,2g14.6,2i6)')i,xc(i),yc(i),
     &      indx(1,i),indx(2,i)
        endif
      enddo
 
      return
      end
      subroutine prmatfl(a,eqn,ihi,ilo,indx,jhi,jlo,maxnfl,ncol,
     &  neqnfl,np)
c
c***********************************************************************
c
cc PRMATFL prints entries from a matrix A associated with a full
c  flow problem.
c
c  PRMATFL prints out a range of rows and columns of a dense matrix,
c  whose rows are indirectly indexed by node number, and whose
c  columns are indexed in the usual way.
c
c  This routine was revised on 08 July 1996.
c
c
c  A      Input, double precision A(MAXNFL,NCOFRB).
c         A is the matrix whose entries are to be printed.
c
c  EQN    Input, character*2 EQN(MAXNFL).
c         EQN records the "type" of each equation that will be generated, and
c         which is associated with an unknown.  Note that most boundary
c         conditions do not result in an equation.  The current values are:
c
c         'U'  The horizontal momentum equation.
c         'UB' The condition U=0 applied at a node on the bump.
c         'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c         'UW' The condition U=0 applied at a node on a fixed wall.
c         'U0' A dummy value of U=0 should be set.
c
c         'V'  The vertical momentum equation.
c         'VB' The condition V=0 applied at a node on the bump.
c         'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c         'VW' The condition V=0 applied at a node on a fixed wall.
c         'V0' A dummy value of V=0 should be set.
c
c         'P'  The continuity equation.
c         'PB' The condition P=0 applied at (XMAX,YMAX).
c         'P0' A dummy value of P=0 should be set.
c
c  IHI,
c  ILO    Input, INTEGER IHI, ILO.
c         ILO is the first node, and IHI the last node, for which the
c         data should be printed.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K).
c  
c         If INDX(I,J) is positive, then that means that a degree of
c         freedom for variable I (U, V or P) is associated with node
c         J, and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J.
c
c  JHI,
c  JLO    Input, INTEGER JHI, JLO.
c         JLO is the first, and JHI the last column of A to print.
c
c  MAXNFL Input, INTEGER MAXNFL.
c         MAXNFL is the maximum number of equations in the full system.
c
c  NEQNFL Input, integer NEQNFL.
c         NEQNFL is the number of equations (and coefficients) in the full
c         finite element system.
c
c  NCOFRB Input, INTEGER NCOFRB.
c         NCOFRB is the number of sensitivities.
c
c  NP     Input, INTEGER NP.
c         NP is the number of nodes.
c
      implicit double precision (a-h,o-z)
c
      integer maxnfl
      integer ncol
      integer neqnfl
      integer np
c
      double precision a(maxnfl,ncol)
      character*2 eqn(neqnfl)
      integer i
      integer ihi
      integer ihi2
      integer ilo
      integer ilo2
      integer indx(3,np)
      integer j
      integer jhi
      integer jhi2
      integer jlo
      integer jlo2
      integer k
      integer l
      integer lhi
      integer llo
      integer ncols
c
      if(neqnfl.le.0)then
        write(*,*)' '
        write(*,*)'PrMatFL - Fatal error!'
        write(*,*)'  NEQNFL = ',neqnfl
        stop
      endif
 
      if(ncol.le.0)then
        write(*,*)' '
        write(*,*)'PrMatFL - Fatal error!'
        write(*,*)'  NCOL = ',ncol
        stop
      endif
 
      if(ilo.lt.1)then
        ilo2=1
        write(*,*)' '
        write(*,*)'PrMatFL - Warning!'
        write(*,*)'  Input ILO = ',ilo
        write(*,*)'  Reset to ILO2 = ',ilo2
      else
        ilo2=ilo
      endif
 
      if(ihi.gt.np)then
        ihi2=np
        write(*,*)' '
        write(*,*)'PrMatFL - Warning!'
        write(*,*)'  Input IHI = ',ihi
        write(*,*)'  Reset to IHI2 = ',ihi2
      else
        ihi2=ihi
      endif

      if(ilo2.gt.ihi2)then
        write(*,*)' '
        write(*,*)'PrMatFL - Warning:'
        write(*,*)'  Input ILO=     ',ilo, ' IHI= ',ihi
        write(*,*)'  Effective ILO2=',ilo2,' IHI2=',ihi2
        return
      endif
 
      if(jlo.lt.1)then
        jlo2=1
        write(*,*)' '
        write(*,*)'PrMatFL - Warning!'
        write(*,*)'  Input value of JLO was ',jlo
        write(*,*)'  Reset to JLO2 = ',jlo2
      else
        jlo2=jlo
      endif
 
      if(jhi.gt.ncol)then
        jhi2=ncol
        write(*,*)' '
        write(*,*)'PrMatFL - Warning!'
        write(*,*)'  Input value of JHI was ',jhi
        write(*,*)'  Reset to JHI2 = ',jhi2
      else
        jhi2=jhi
      endif

      if(jlo2.gt.jhi2)then
        write(*,*)' '
        write(*,*)'PrMatFL - Warning:'
        write(*,*)'  Input JLO=     ',jlo, ' JHI= ',jhi
        write(*,*)'  Effective JLO2=',jlo2,' JHI2=',jhi2
        return
      endif
c
c  Print the individual entries.
c
      write(*,*)' '
      write(*,*)'PrMatFL: Matrix entries'
      write(*,*)'  for nodes ILO2=',ilo2,' to IHI2=',ihi2
      write(*,*)'  and columns JLO2=',jlo2,' to JHI2=',jhi2
      write(*,*)' '
c
c  Print columns in groups, going from LLO to LHI.
c
      ncols=5
 
      do llo=jlo2,jhi2,ncols
 
        lhi=min(llo+ncols-1,jhi2)
        write(*,*)' '
        write(*,'(''    Eqn Node'',6(i6,6x))')(l,l=llo,lhi)
        write(*,*)' '
c
c  Compute the index of the equation just before the first
c  equation to be printed.
c
        i=indx(1,ilo2)-1
 
        do j=ilo2,ihi2
 
          do k=1,3
 
            if(indx(k,j).gt.0)then
 
              i=i+1
 
              write(*,'(a2,2i4,6g12.4)')eqn(i),i,j,
     &          (a(i,l),l=llo,lhi)
 
            endif
 
          enddo
 
        enddo
 
      enddo
 
      return
      end
      subroutine prpar(iopt,npar,nparb,nparf,par)
c
c***********************************************************************
c
cc PRPAR prints out the current parameters.
c
c
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the 
c         inflow.  NPARF must be at least 1.  
c
c  PAR    Input, double precision PAR(NPAR).
c 
c         PAR is the current estimate for the parameters.
c
      implicit double precision (a-h,o-z)
c
      integer npar
c
      integer i
      integer iopt(npar)
      character*6 label1
      character*5 label2
      integer nparb
      integer nparf
      double precision par(npar)
c
      write(*,*)' '
 
      do i=1,npar
 
        if(iopt(i).eq.0)then
          label2='Fixed'
        else
          label2='Free '
        endif
 
        if(i.le.nparf)then
          label1='Inflow  '
        elseif(nparf+1.le.i.and.i.le.nparf+nparb)then
          label1='Bump  '
        elseif(i.eq.nparf+nparb+1)then
          label1='Reynld'
        endif
 
        write(*,'(i2,2x,a6,2x,a5,2x,g14.6)')i,label1,label2,par(i)
 
      enddo
 
      return
      end
      subroutine prsenn(maxcofrb,maxnfl,ncofrb,neqnfl,senfl)
c
c***********************************************************************
c
cc PRSENN prints out the norms of the sensitivities.
c
c  This routine was revisedon 07 July 1996.
c
c
c  MAXNFL Input, integer MAXNFL.
c         MAXNFL is the maximum number of equations or coefficients allowed
c         for the full system.  MAXNFL must be used instead of NEQNFL as
c         the leading dimension of certain multi-dimensional arrays.
c
c  NEQNFL Input, integer NEQNFL.
c         NEQNFL is the number of equations (and coefficients) in the full
c         finite element system.
c
c  NCOFRB Input, integer NCOFRB.
c         NCOFRB is the number of basis functions used for the
c         reduced basis method.  (The first basis vector is labeled
c         "0").  In this program, that amounts to the number of columns
c         in the matrix RB.  NCOFRB is also the number of reduced basis 
c         state equations, and reduced basis coefficients GRB.
c
c  SENFL  Input, double precision SENFL(MAXNFL,NCOFRB).
c         SENFL contains the first several order sensitivities of the
c         full solution with respect to the REYNLD parameter.
c
c         SENFL(I,J) contains the J-th sensitivity of the I-th full unknown
c         with respect to REYNLD.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxnfl
      integer ncofrb
      integer neqnfl
c
      double precision anrmg
      double precision dnrm2
      double precision enrmg
      integer i
      integer idamax
      integer itemp
      double precision senfl(maxnfl,maxcofrb)
c
      intrinsic abs
      external dnrm2
      external idamax
c
c  Print the norms of the columns of SENFL.
c
      write(*,*)' '
      write(*,*)'Order    MxNorm     Index    l2 Norm'
      write(*,*)' '

      do i=1,ncofrb
        itemp=idamax(neqnfl,senfl(1,i),1)
        anrmg=abs(senfl(itemp,i))
        enrmg=dnrm2(neqnfl,senfl(itemp,i),1)
        write(*,'(i6,g14.6,i6,g14.6)')i,anrmg,itemp,enrmg
      enddo
 
      return
      end
      subroutine prvecfl(eqn,ihi,ilo,indx,neqnfl,np,vec)
c
c***********************************************************************
c
cc PRVECFL prints out some entries of a vector indexed by node number.
c
c
c  IHI,
c  ILO    Input, INTEGER IHI, ILO.
c         ILO is the first node, and IHI the last node, for which the
c         data should be printed.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K).
c  
c         If INDX(I,J) is positive, then that means that a degree of
c         freedom for variable I (U, V or P) is associated with node
c         J, and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J.
c
c  NEQNFL Input, INTEGER NEQNFL.
c         NEQNFL is the number of equations in the full system.
c
c  NP     Input, INTEGER NP.
c         NP is the number of nodes.
c
c  VEC    Input, double precision VEC(NEQNFL).
c         VEC contains the vector information to be printed.
c
      implicit double precision (a-h,o-z)
c
      integer neqnfl
      integer np
c
      character*2 eqn(neqnfl)
      integer i
      integer ihi
      integer ihi2
      integer ilo
      integer ilo2
      integer indx(3,np)
      integer j
      integer k
      double precision vec(neqnfl)
c
      ilo2=max(1,ilo)
      ihi2=min(np,ihi)

      if(ilo2.gt.ihi2)then
        write(*,*)' '
        write(*,*)'PrVecFL - Warning:'
        write(*,*)'  Input ILO=     ',ilo, ' IHI= ',ihi
        write(*,*)'  Effective ILO2=',ilo2,' IHI2=',ihi2
        return
      endif
 
c
c  Print the individual entries.
c
      write(*,*)' '
      write(*,*)'PrVecFL: Vector entries'
      write(*,*)'  for nodes ILO2=',ilo2,' to IHI2=',ihi2
      write(*,*)' '
      write(*,*)'   Eqn Node      Value'
      write(*,*)' '
c
c  Compute the index of the equation just before the first
c  equation to be printed.
c
      i=indx(1,ilo2)-1
 
      do j=ilo2,ihi2
 
        do k=1,3
 
          if(indx(k,j).gt.0)then
 
            i=i+1
 
 
            write(*,'(a2,2i5,5g12.4)')eqn(i),i,j,vec(i)
 
          endif
 
        enddo
 
      enddo
 
      return
      end
      subroutine prvecrb(ihi,ilo,nhi,nlo,vec)
c
c***********************************************************************
c
cc PRVECRB prints out entries ILO through IHI of a vector.
c
c
c  This routine was modified on 08 July 1996.
c
c
c  IHI,
c  ILO    Input, integer IHI, ILO.
c         ILO is the lowest, and IHI the highest index to print.
c
c  NHI,
c  NHI    Input, integer NHI, NLO.
c         NLO is the index of the lowest, and NHI the index of
c         the highest entry of VEC.
c
c  VEC    Input, double precision VEC(NLO:NHI).
c         VEC is the vector whose entries are to be printed.
c
      implicit double precision (a-h,o-z)
c
      integer nhi
      integer nlo
c
      integer i
      integer ihi
      integer ihi2
      integer ilo
      integer ilo2
      double precision vec(nlo:nhi)
c
      if(nlo.gt.nhi)then
        write(*,*)' '
        write(*,*)'PrVecRB - Fatal error!'
        write(*,*)'  NLO > NHI!'
        write(*,*)'  NLO=',nlo
        write(*,*)'  NHI=',nhi
        stop
      endif
 
      if(ilo.lt.nlo)then
        ilo2=nlo
      else
        ilo2=ilo
      endif
 
      if(ihi.gt.nhi)then
        ihi2=nhi
      else
        ihi2=ihi
      endif
 
      write(*,*)'Vector entries ILO2=',ilo2,' to IHI2=',ihi2
      write(*,*)' '
      do i=ilo2,ihi2
        write(*,'(i6,g14.6)')i,vec(i)
      enddo

      return
      end
      subroutine prxy(ihi,ilo,np,ny,xc,yc)
c
c***********************************************************************
c
cc PRXY prints the X and Y coordinates of each node.
c
c
c  IHI,
c  ILO    Input, INTEGER IHI, ILO, the indices of the last and
c         first nodes to print.
c
c  NP     Input, INTEGER NP, the number of nodes.
c
c  NY     Input, INTEGER NY, the number of nodes in each column.
c
c  XC     Input, real XC(NP), the X coordinates of the nodes.
c
c  YC     Input, real YC(NP), the Y coordinates of the nodes.
c
      implicit double precision (a-h,o-z)
c
      integer np
c
      integer i
      integer icol
      integer ihi
      integer ilo
      integer irow
      integer ny
      double precision xc(np)
      double precision yc(np)
c
      write(*,*)' '
      write(*,*)'PrXY:'
      write(*,*)'  Print X and Y coordinates of nodes'
      write(*,*)ilo,' through ',ihi
      write(*,*)' '
      write(*,*)'  Node  Row  Column     X      Y'
      write(*,*)' '
 
      do i=max(ilo,1),min(ihi,np)
        icol=(i-1)/(2*ny-1) + 1
        irow=i-(icol-1)*(2*ny-1)
        if(irow.eq.1)then
          write(*,*)' '
        endif
        write(*,'(3i5,2f12.5)')i,irow,icol,xc(i),yc(i)
      enddo
 
      return
      end
      subroutine qbf(ielem,in,w,dwdx,dwdy,nelem,node,np,xc,xq,yc,yq)
c
c***********************************************************************
c
cc QBF evaluates a particular quadratic basis function at a point
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
c  W,
c  DWDX,
c  DWDY   Output, double precision W, DWDX, DWDY, the value of the
c         IN-th basis  function and its X and Y derivatives, at the
c         given point.
c
c  NELEM  Input, integer NELEM, the number of elements.
c
c  NODE   Input, integer NODE(6,NELEM), contains the numbers
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
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer np
c
      double precision c
      double precision d
      double precision dwdx
      double precision dwdy
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

        if(d.eq.0.0)then
          write(*,*)' '
          write(*,*)'QBF - Fatal error!'
          write(*,*)'  D=0'
          write(*,*)'  Element IELEM=',ielem
          write(*,*)'  I1, XC(I1), YC(I1)=',i1,xc(i1),yc(i1)
          write(*,*)'  I2, XC(I2), YC(I2)=',i2,xc(i2),yc(i2)
          write(*,*)'  I3, XC(I3), YC(I3)=',i3,xc(i3),yc(i3)
          stop
        endif

        t=1.0+( (xq    -xc(i1))*(yc(i2)-yc(i3))
     &       +(xc(i3)-xc(i2))*(yq    -yc(i1)) )/d
 
        w=t*(2.0*t-1.0)
 
        dwdx=(yc(i2)-yc(i3))*(4.0*t-1.0)/d
        dwdy=(xc(i3)-xc(i2))*(4.0*t-1.0)/d
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

        if(d.eq.0.0)then
          write(*,*)' '
          write(*,*)'QBF - Fatal error!'
          write(*,*)'  D=0'
          write(*,*)'  Element IELEM=',ielem
          write(*,*)'  I1, XC(I1), YC(I1)=',i1,xc(i1),yc(i1)
          write(*,*)'  I2, XC(I2), YC(I2)=',i2,xc(i2),yc(i2)
          write(*,*)'  I3, XC(I3), YC(I3)=',i3,xc(i3),yc(i3)
          stop
        endif

        c=    (xc(i3)-xc(i2))*(yc(i1)-yc(i2))
     &       -(xc(i1)-xc(i2))*(yc(i3)-yc(i2))

        if(c.eq.0.0)then
          write(*,*)' '
          write(*,*)'QBF - Fatal error!'
          write(*,*)'  C=0'
          write(*,*)'  Element IELEM=',ielem
          write(*,*)'  I1, XC(I1), YC(I1)=',i1,xc(i1),yc(i1)
          write(*,*)'  I2, XC(I2), YC(I2)=',i2,xc(i2),yc(i2)
          write(*,*)'  I3, XC(I3), YC(I3)=',i3,xc(i3),yc(i3)
          stop
        endif

        t=1.0+( (xq    -xc(i1))*(yc(i2)-yc(i3))
     &    +(xc(i3)-xc(i2))*(yq    -yc(i1)) )/d
 
        s=1.0+( (xq    -xc(i2))*(yc(i3)-yc(i1))
     &    +(xc(i1)-xc(i3))*(yq    -yc(i2)) )/c
 
        w=4.0 * s*t
        dwdx=4.0 * ((yc(i3)-yc(i1))*t/c + (yc(i2)-yc(i3))*s/d)
        dwdy=4.0 * ((xc(i1)-xc(i3))*t/c + (xc(i3)-xc(i2))*s/d)
 
      else
 
        write(*,*)' '
        write(*,*)'QBF - Fatal error!'
        write(*,*)'  Request for basis function IN=',in
        write(*,*)'  but IN must be between 1 and 6.'
        stop
 
      endif
 
      return
      end
      subroutine refbsp(q,dqdx,dqdy,detadx,detady,dxsidx,dxsidy,
     &  eta,iq,xsi)
c
c***********************************************************************
c
cc REFBSP evaluates one of the three linear basis functions,
c  and its X and Y derivatives, at a particular point (X,Y)
c  in a particular element, by referring to the corresponding
c  points (XSI,ETA) in the reference triangle.
c
c  It is assumed that we already know the value of the jacobian
c  of the isoparametric transformation between the (XSI, ETA) and
c  (X, Y) spaces.  The four entries of the jacobian are
c  symbolically named DETADX, DETADY, DXSIDX and DXSIDY, and
c  we know that the jacobian gives us the following relation
c  between derivatives with respect to XSI and ETA, and derivatives
c  with respect to X and Y:
c
c    dF/dX = dF/dXsi dXsi/dX + dF/dEta dEta/dX
c    dF/dY = dF/dXsi dXsi/dY + dF/dEta dEta/dY
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
c  Q,
c  DQDX,
c  DQDY   Output, double precision Q, DQDX, DQDY, the value of the basis
c         function, and its derivatives with respect to X and Y, at
c         the point (ETA,XSI).
c
c  DETADX,
c  DETADY Input, double precision DETADX, DETADY, the partial derivative
c         d ETA/d X and d ETA/d Y at (ETA,XSI).
c
c  IQ     Input, integer IQ, the local node number, between 1 and
c         3, whose basis function is being evaluated.
c
c  DXSIDX,
c  DXSIDY Input, double precision DXSIDX, DXSIDY, the partial derivative
c         d XSI/d X and d XSI/d Y at (ETA,XSI).
c
c  ETA,
c  XSI    Input, double precision ETA, XSI, the local coordinates of the
c         at which the basis information is desired.
c
      implicit double precision (a-h,o-z)
c
      double precision detadx
      double precision detady
      double precision dqdeta
      double precision dqdx
      double precision dqdxsi
      double precision dqdy
      double precision dxsidx
      double precision dxsidy
      double precision eta
      integer iq
      double precision q
      double precision xsi
c
c  Refuse to evaluate the basis functions for arguments (XSI,ETA)
c  that lie outside the reference triangle.
c
      if(xsi.gt.1.0)then
        write(*,*)' '
        write(*,*)'REFBSP - Fatal error!'
        write(*,*)'  XSI must be less than or equal to 1.'
        write(*,*)'  Input XSI is ',xsi
        stop
      endif
 
      if(eta.gt.xsi)then
        write(*,*)' '
        write(*,*)'REFBSP - Fatal error!'
        write(*,*)'  ETA must be less or equal to XSI.'
        write(*,*)'  Input XSI, ETA = ',xsi,eta
        stop
      endif
 
      if(eta.lt.0.0)then
        write(*,*)' '
        write(*,*)'REFBSP - Fatal error!'
        write(*,*)'  ETA must be 0 or greater.'
        write(*,*)'  Input ETA=',eta
        stop
      endif
c
      if(iq.eq.1)then
        q=1.0-xsi
        dqdxsi=-1.0
        dqdeta= 0.0
      elseif(iq.eq.2)then
        q=eta
        dqdxsi=0.0
        dqdeta=1.0
      elseif(iq.eq.3)then
        q=xsi-eta
        dqdxsi=1.0
        dqdeta=-1.0
      elseif(iq.ge.4.and.iq.le.6)then
        q=0.0
        dqdxsi=0.0
        dqdeta=0.0
      else
        write(*,*)'RefBSP - Fatal error!'
        write(*,*)'  Request for basis function IQ=',iq
        write(*,*)'  but IQ must be between 1 and 6.'
        stop
      endif
 
      dqdx=dqdxsi*dxsidx+dqdeta*detadx
      dqdy=dqdxsi*dxsidy+dqdeta*detady
 
      return
      end
      subroutine refqbf(w,dwdx,dwdy,detadx,detady,dxsidx,dxsidy,
     &  eta,iq,xsi)
c
c***********************************************************************
c
cc REFQBF evaluates one of the six quadratic basis functions,
c  and its X and Y derivatives, at a particular point in a
c  particular element, by referring to the reference triangle.
c
c  The point we are interested in is referred to by its coordinates
c  in the reference triangle.  That is, we are given coordinates
c  (XSI, ETA), even though, physically, we are interested
c  in points in (X, Y) space.
c
c  It is assumed that we already know the value of the jacobian
c  of the isoparametric transformation between the (XSI, ETA) and
c  (X, Y) spaces.  The four entries of the jacobian are
c  symbolically named DETADX, DETADY, DXSIDX and DXSIDY, and
c  we know that the jacobian gives us the following relation
c  between derivatives with respect to XSI and ETA, and derivatives
c  with respect to X and Y:
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
c
c  W,
c  DWDX,
c  DWDY   Output, double precision W, DWDX, DWDY, the value of the basis
c         function, and its derivatives with respect to X and Y, at
c         the point (XSI,ETA).
c
c  DETADX,
c  DETADY Input, double precision DETADX, DETADY, the partial derivative
c         d ETA/d X and d ETA/d Y at (XSI,ETA).
c
c  DXSIDX,
c  DXSIDY Input, double precision DXSIDX, DXSIDY, the partial derivative
c         d XSI/d X and d XSI/d Y at (XSI,ETA).
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
      double precision detadx
      double precision detady
      double precision dwdeta
      double precision dwdx
      double precision dwdxsi
      double precision dwdy
      double precision dxsidx
      double precision dxsidy
      double precision eta
      integer iq
      double precision w
      double precision xsi
c
c  Refuse to evaluate the basis functions for arguments (XSI,ETA)
c  that lie outside the reference triangle.
c
      if(xsi.gt.1.0)then
        write(*,*)' '
        write(*,*)'REFQBF - Fatal error!'
        write(*,*)'  XSI must be less than or equal to 1.'
        write(*,*)'  Input XSI is ',xsi
        stop
      endif
 
      if(eta.gt.xsi)then
        write(*,*)' '
        write(*,*)'REFQBF - Fatal error!'
        write(*,*)'  ETA must be less or equal to XSI.'
        write(*,*)'  Input XSI, ETA = ',xsi,eta
        stop
      endif
 
      if(eta.lt.0.0)then
        write(*,*)' '
        write(*,*)'REFQBF - Fatal error!'
        write(*,*)'  ETA must be 0 or greater.'
        write(*,*)'  Input ETA=',eta
        stop
      endif
c
c  Evaluate W, the quadratic basis function.
c  Evaluate DWDXSI and DWDETA, the partial derivatives d W/d XSI
c  and d W/d ETA.
c
c  Basis 1 is zero if XSI=0.5 or XSI=1.
c
      if(iq.eq.1)then
        w= (2.0*xsi-1.0) * (xsi-1.0)
        dwdxsi=-3.0+4.0*xsi
        dwdeta=0.0
c
c  Basis 2 is zero if ETA=0 or ETA=0.5.
c
      elseif(iq.eq.2)then
        w= eta * (2.0*eta-1.0)
        dwdxsi=0.0
        dwdeta=-1.0+4.0*eta
c
c  Basis 3 is zero if XSI=ETA, or XSI=ETA+0.5
c
      elseif(iq.eq.3)then
        w= (xsi-eta) * (2.0*xsi-2.0*eta-1.0)
        dwdxsi=-1.0+4.0*xsi-4.0*eta
        dwdeta=1.0-4.0*xsi+4.0*eta
c
c  Basis 4 is zero if ETA=0 or XSI=1.
c
      elseif(iq.eq.4)then
        w= 4.0 * eta * (1.0-xsi)
        dwdxsi=-4.0*eta
        dwdeta=4.0-4.0*xsi
c
c  Basis 5 is zero if ETA=0 or XSI=ETA.
c
      elseif(iq.eq.5)then
        w=4.0 * eta * (xsi-eta)
        dwdxsi=4.0*eta
        dwdeta=4.0*xsi-8.0*eta
c
c  Basis 6 is zero if XSI=ETA or XSI=1.
c
      elseif(iq.eq.6)then
        w=4.0 * (xsi-eta) * (1.0-xsi)
        dwdxsi=4.0-8.0*xsi+4.0*eta
        dwdeta=-4.0+4.0*xsi
c
c  Stop if we were given an unexpected value of IQ.
c
      else
        write(*,*)' '
        write(*,*)'RefQBF - Fatal error!'
        write(*,*)'  A basis function index must be between 1 and 6,'
        write(*,*)'  but you input the value IQ=',iq
        stop
      endif
c
c  Convert the d W/d XSI and d W/d ETA derivatives to d W/d X
c  and d W/d Y.
c
      dwdx=dwdxsi*dxsidx + dwdeta*detadx
      dwdy=dwdxsi*dxsidy + dwdeta*detady
 
      return
      end
      function s_eqi ( s1, s2 )

c*********************************************************************72
c
cc S_EQI is a case insensitive comparison of two strings for equality.
c
c  Example:
c
c    S_EQI ( 'Anjana', 'ANJANA' ) is TRUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S1, S2, the strings to compare.
c
c    Output, logical S_EQI, the result of the comparison.
c
      implicit none

      character c1
      character c2
      integer i
      integer lenc
      logical s_eqi
      character*(*) s1
      integer s1_length
      character*(*) s2
      integer s2_length

      s1_length = len ( s1 )
      s2_length = len ( s2 )
      lenc = min ( s1_length, s2_length )

      s_eqi = .false.

      do i = 1, lenc

        c1 = s1(i:i)
        c2 = s2(i:i)
        call ch_cap ( c1 )
        call ch_cap ( c2 )

        if ( c1 .ne. c2 ) then
          return
        end if

      end do

      do i = lenc + 1, s1_length
        if ( s1(i:i) .ne. ' ' ) then
          return
        end if
      end do

      do i = lenc + 1, s2_length
        if ( s2(i:i) .ne. ' ' ) then
          return
        end if
      end do

      s_eqi = .true.

      return
      end
      subroutine setban(indx,ldafl,nelem,nlband,node,np)
c
c***********************************************************************
c
cc SETBAN computes NLBAND, the lower band width of the Jacobian matrix
c  stored in LINPACK general band storage format.
c
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  LDAFL  Input, integer LDAFL, the first dimension of the matrix AFL.
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NLBAND Output, integer NLBAND.
c
c         The lower bandwidth of the matrix A.  The zero structure of A
c         is assumed to be symmetric, and so NLBAND is also the upper
c         bandwidth of A.  
c 
c  NODE   Input, integer NODE(6,NELEM).
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
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c 
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer np
c
      integer i
      integer ielem
      integer indx(3,np)
      integer ip
      integer ipp
      integer iq
      integer iqq
      integer iuk
      integer iukk
      integer j
      integer ldafl
      integer nlband
      integer node(6,nelem)
c
      nlband=0
 
      do ielem=1,nelem
        do iq=1,6
          ip=node(iq,ielem)
          do iuk=1,3
            i=indx(iuk,ip)
            if(i.gt.0)then
              do iqq=1,6
                ipp=node(iqq,ielem)
                do iukk=1,3
                  j=indx(iukk,ipp)
                  if(j.gt.0)then
                    if(j-i.gt.nlband)then
                      nlband=j-i
                    endif
                  endif
                enddo
              enddo
            endif
          enddo
        enddo
      enddo
 
      if(3*nlband+1.gt.ldafl)then
        write(*,*)' '
        write(*,*)'SetBan - Fatal error!'
        write(*,*)'  Not enough room for matrix!'
        write(*,*)'  Number of rows needed is ',3*nlband+1
        write(*,*)'  The maximum allowed is LDAFL= ',ldafl
        stop
      endif
 
      return
      end
      subroutine setgeo(area,etaq,gridx,gridy,ibs,isotri,nelem,node,
     &  nodelm,np,npar,nparb,nparf,nx,ny,par,phifl,region,splbmp,
     &  taubmp,wquad,xbl,xbr,xc,xquad,xrange,xsiq,ybl,ybr,yc,
     &  yquad,yrange)
c
c***********************************************************************
c
cc SETGEO is given a set of flow parameters in PAR, and an
c  approximate solution vector G, and proceeds to set up the
c  constraints associated with PAR, and use Newton iteration
c  to correct G to a solution that satisfies the constraints
c  to within some tolerance.
c
c
c  AREA   Input, double precision AREA(3,NELEM).
c
c         AREA contains a common factor multiplying the term associated
c         with a quadrature point in a given element, namely,
c
c           AREA(IQUAD,IELEM) = Ar(IELEM) * WQUAD(IQUAD)
c
c         or, if the element is isoperimetric,
c
c           AREA(IQUAD,IELEM) = DET * Ar(IELEM) * WQUAD(IQUAD)
c
c         Here Ar(IELEM) represents the area of element IELEM.
c
c  ETAQ   Input, double precision ETAQ(3).
c         The "Eta" coordinates of the quadrature points.
c
c  IBS    Input, integer IBS.
c         1, the bump is modeled by C0 linear splines.
c         2, the bump is modeled by C0 quadratic splines.
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
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NODE   Input, integer NODE(6,NELEM).
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
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c 
c  NPAR   Input, integer NPAR.
c
c         The number of parameters.  NPAR = NPARF + NPARB + 1.
c 
c         The parameters control the shape of the inflow,
c         the shape of the bump obstacle, and the strength of the
c         flow.
c 
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
c 
c  NPARF  Input, integer NPARF.
c
c         NPARF is the number of parameters associated with the 
c         inflow.  NPARF must be at least 1.  
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
c  PAR    Input, double precision PAR(NPAR).
c 
c         PAR is the current estimate for the parameters.
c
c  PHIFL  Output, double precision PHIFL(3,6,10,NELEM).  
c
c         PHIFL contains the value of a finite element basis function, its
c         derivative, or other information, evaluated at the quadrature 
c         points.
c 
c         The meaning of the entry PHIFL(I,J,K,L) is as follows.  
c         For the quadrature point I, and basis function J, in element L,
c         PHIFL(I,J,K,L) represents the value of:
c
c           K= 1, W, the finite element basis function for velocities;
c           K= 2, dWdX, the X derivative of W;
c           K= 3, dWdY, the Y derivative of W;
c           K= 4, Q, the finite element basis function for pressures;
c           K= 5, dQdX, the X derivative of Q;
c           K= 6, dQdY, the Y derivative of Q;
c           K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c           K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c           K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c           K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c         In particular, PHIFL(I,J,K,L) is the value of the quadratic 
c         basis function W associated with local node J in element L, 
c         evaluated at quadrature point I.
c 
c         Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6, 
c         since there are only three linear basis functions.
c
c  REGION Input, character*20 REGION.
c         REGION specifies the flow region.
c
c         'cavity', a driven cavity, 1 unit on each side, open on 
c         the top with a tangential velocity specification there.
c
c         'channel', a channel, 10 units long by 3 high, inflow on 
c         the left, outflow on the right, with a bump on the bottom.
c
c         'step', a channel, 12 units long by 3 high, inflow on the
c         left, outflow on the right, with a step on the bottom.
c
c  SPLBMP Output, double precision SPLBMP(NPARB+2).
c
c         SPLBMP contains the spline coefficients for the bump.
c
c  SPLFLO Output, double precision SPLFLO(NPARF+2).
c
c         SPLFLO contains the spline coefficients for the inflow.
c
c  TAUBMP Output, double precision TAUBMP(NPARB+2).
c
c         TAUBMP contains the location of the spline abscissas for
c         the bump.  There are NPARB+2 of them, because the end values
c         of the spline are constrained to have particular values.
c
c  TAUFLO Output, double precision TAUFLO(NPARF+2).
c
c         TAUFLO contains the location of the spline abscissas for
c         the inflow.  There are NPARF+2 of them, because the end 
c         values of the spline are constrained to have particular 
c         values.
c
c  WQUAD  Input, double precision WQUAD(3), the weights for Gaussian
c         quadrature.
c
c  XBL    Input, double precision XBL, the X coordinate of the left corner 
c         of the bump.
c
c  XBR    Input, double precision XBR, the X coordinate of the right corner 
c         of the bump.
c
c  XC     Input, double precision XC(NP).
c
c         The X coordinates of the nodes.
c 
c  XQUAD  Input, double precision XQUAD(3,NELEM).
c
c         The X coordinates of the quadrature points for each element.
c 
c  XSIQ   Input, double precision XSIQ(3).
c         The "Xsi" coordinates of the quadrature points.
c
c  YBL    Input, double precision YBL, the Y coordinate of the left corner 
c         of the bump.
c
c  YBR    Input, double precision YBR, the Y coordinate of the right corner 
c         of the bump.
c
c  YC     Input, double precision YC(NP).
c
c         The Y coordinates of the nodes.
c 
c  YQUAD  Input, double precision YQUAD(3,NELEM).
c
c         The Y coordinates of the quadrature points for each element.
c 
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer np
      integer npar
      integer nparb
      integer nparf
      integer nx
      integer ny
c
      double precision area(3,nelem)
      double precision etaq(3)
      character*20 gridx
      character*20 gridy
      integer ibs
      integer isotri(nelem)
      integer node(6,nelem)
      integer nodelm(np)
      double precision par(npar)
      double precision phifl(3,6,10,nelem)
      character*20 region
      double precision splbmp(nparb+2)
      double precision taubmp(nparb+2)
      double precision wquad(3)
      double precision xbl
      double precision xbr
      double precision xc(np)
      double precision xquad(3,nelem)
      double precision xrange
      double precision xsiq(3)
      double precision ybl
      double precision ybr
      double precision yc(np)
      double precision yquad(3,nelem)
      double precision yrange
c
c  Set the spline coefficients for the bump.
c
      call bmpspl(npar,nparb,nparf,par,splbmp,taubmp,xbl,
     &  xbr,ybl,ybr)
c
c  Set the X and Y coordinates of the nodes that form the grid.
c
      call setxy(gridx,gridy,ibs,np,nparb,nx,ny,region,splbmp,taubmp,
     &  xbl,xbr,xc,xrange,ybl,ybr,yc,yrange)
c
c  Set the quadrature points, which move every step if there
c  are bump parameters.
c
      call setq3(area,etaq,isotri,nelem,node,np,wquad,xc,xquad,
     &  xsiq,yc,yquad)
c
c  Set the value of the basis functions at all quadrature points.
c
      call setpfl(area,etaq,isotri,nelem,node,np,phifl,
     &  xc,xquad,xsiq,yc,yquad)
c
c  Set the NODELM array.
c
      do ip=1,nelem
        nodelm(ip)=0
      enddo
 
      do ielem=1,nelem
        do j=1,6
          ip=node(j,ielem)
          nodelm(ip)=ielem
        enddo
      enddo

      return
      end
      subroutine setlog(eqn,hx,hy,ibump,indx,isotri,ldafl,maxelm,
     &  maxnfl,maxnp,nelem,neqnfl,nlband,node,np,nprof,nx,ny,
     &  region,xbl,xbr,xprof,xrange,ybr,yrange)
c
c***********************************************************************
c
cc SETLOG determines some data that depends on the user input.
c
c
c  EQN    Output, character*2 EQN(NEQNFL).
c         EQN records the "type" of each equation that will be generated, and
c         which is associated with an unknown.  Note that most boundary 
c         conditions do not result in an equation.  The current values are:
c
c         'U'  The horizontal momentum equation.
c         'UB' The condition U=0 applied at a node on the bump.
c         'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c         'UW' The condition U=0 applied at a node on a fixed wall.
c
c         'V'  The vertical momentum equation.
c         'VB' The condition V=0 applied at a node on the bump.
c         'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c         'VW' The condition V=0 applied at a node on a fixed wall.
c
c         'P'  The continuity equation.
c         'PB' The condition P=0 applied at (XMAX,YMAX).
c
c  HX     Output, double precision HX.
c         HX is the nominal spacing between nodes in the X direction.
c
c  HY     Output, double precision HY.
c         HY is the nominal spacing between nodes in the Y direction.
c
c  NELEM  Output, integer NELEM.
c         NELEM is the number of elements.
c         NELEM can be determined as 2*(NX-1)*(NY-1).
c 
c  NP     Output, integer NP.
c         NP is the number of nodes used to define the finite element mesh.
c         Typically, the mesh is generated as a rectangular array, with
c         an odd number of nodes in the horizontal and vertical directions.
c         The formula for NP is NP=(2*NX-1)*(2*NY-1).
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
c  XPROF  Output, double precision XPROF.
c
c         The X coordinate at which the profile is measured.  This 
c         value should be a grid value!
c 
c  XRANGE Input, double precision XRANGE.
c         The total width of the region.
c
c  YRANGE Input, double precision YRANGE.
c         YRANGE is the total height of the region.
c
      implicit double precision (a-h,o-z)
c
      integer maxelm
      integer maxnfl
      integer maxnp
c
      character*2 eqn(maxnfl)
      double precision hx
      double precision hy
      integer i
      integer ibump
      integer indx(3,maxnp)
      integer isotri(maxelm)
      integer itemp
      integer ldafl
      integer nelem
      integer neqnfl
      integer nlband
      integer node(6,maxelm)
      integer np
      integer nprof(2*ny-1)
      integer nx
      integer ny
      character*20 region
      double precision xbl
      double precision xbr
      double precision xprof
      double precision xrange
      double precision ybr
      double precision yrange
c
      nelem=2*(nx-1)*(ny-1)

      write(*,*)' '
      write(*,*)'SetLog - Note:'
      write(*,*)'  Number of elements, NELEM=',nelem

      np=(2*nx-1)*(2*ny-1)

      write(*,*)'  Number of nodes, NP=',np

      if(xprof.lt.0.0.or.xprof.gt.xrange)then
        write(*,*)' '
        write(*,*)'SetLog - Fatal error!'
        write(*,*)'  XPROF lies outside of XRANGE.'
        write(*,*)'  XPROF=',xprof
        write(*,*)'  XRANGE=',xrange
        stop
      endif

      if(nx.gt.1)then
        hx=xrange/(2.0*dble(nx-1))
      else
        write(*,*)' '
        write(*,*)'SetLog - Fatal error!'
        write(*,*)'  NX=',nx
        stop
      endif

      if(ny.gt.1)then
        hy=yrange/(2.0*dble(ny-1))
      else
        write(*,*)' '
        write(*,*)'SetLog - Fatal error!'
        write(*,*)'  NY=',ny
        stop
      endif

      write(*,*)'  X nodal spacing is HX=',hx
      write(*,*)'  Y nodal spacing is HY=',hy

c
c  Set the logical NODE array.
c
      call setnod(eqn,ibump,indx,isotri,maxnfl,nelem,neqnfl,node,
     &  np,nx,ny,region,xbl,xbr,xrange,ybr,yrange)

      write(*,*)'  The number of unknowns is NEQNFL=',neqnfl

c
c  Set the location of the profile nodes.
c
      itemp=nint((2.0*dble(nx-1)*xprof)/xrange)
      do i=1,2*ny-1
        nprof(i)=itemp*(2*ny-1)+i
      enddo

      write(*,*)' '
      write(*,*)'  Profile nodes extend from ',nprof(1)
      write(*,*)'  to ',nprof(2*ny-1)
c
c  Get the matrix bandwidth.
c
      write(*,*)' '
      write(*,*)'  Maximum full matrix rows LDAFL=',ldafl

      call setban(indx,ldafl,nelem,nlband,node,np)

      write(*,*)'  Lower bandwidth NLBAND=    ',nlband
      write(*,*)'  Required matrix rows 3*NLBAND+1= ',3*nlband+1
 
      return
      end
      subroutine setnod(eqn,ibump,indx,isotri,maxnfl,nelem,neqnfl,node,
     &  np,nx,ny,region,xbl,xbr,xrange,ybr,yrange)
c
c***********************************************************************
c
cc SETNOD assigns numbers to the nodes and elements, decides which 
c  elements shall be isoparametric, (ISOTRI) and assigns six nodes 
c  to each (NODE).  
c
c  It associates global unknown indices with each node (INDX), and
c  computes the total number of unknowns and equations (NEQNFL), and
c  compares that to the maximum allowed value, MAXNFL.
c
c
c  EQN    Output, character*2 EQN(NEQNFL).
c         EQN records the "type" of each equation that will be generated, and
c         which is associated with an unknown.  Note that most boundary 
c         conditions do not result in an equation.  The current values are:
c
c         'U'  The horizontal momentum equation.
c         'UB' The condition U=0 applied at a node on the bump.
c         'UI' The condition U=UInflow(Y,Lambda) at the inflow.
c         'UW' The condition U=0 applied at a node on a fixed wall.
c
c         'V'  The vertical momentum equation.
c         'VB' The condition V=0 applied at a node on the bump.
c         'VI' The condition V=VInflow(Y,Lambda) at the inflow.
c         'VW' The condition V=0 applied at a node on a fixed wall.
c
c         'P'  The continuity equation.
c         'PB' The condition P=0 applied at (XMAX,YMAX).
c
c  IBUMP  Input, integer IBUMP.  
c
c         IBUMP determines where isoparametric elements will be used.
c 
c         0, no isoparametric elements will be used.  
c            Midside nodes of nonisoparametric elements above the
c            bump will be recomputed so that the sides are straight.
c
c         1, isoparametric elements will be used only for the
c            elements which directly impinge on the bump.
c            Midside nodes of nonisoparametric elements above the
c            bump will be recomputed so that the sides are straight.
c 
c         2, isoparametric elements will be used for all elements 
c            which are above the bump.  All nodes above the bump
c            will be equally spaced in the Y direction.
c 
c         3, isoparametric elements will be used for all elements.
c            All nodes above the bump will be equally spaced in 
c            the Y direction.
c
c  INDX   Output, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  ISOTRI Output, integer ISOTRI(NELEM).
c
c         0, the element is NOT isoparametric.
c 
c         1, the element is isoparametric.
c 
c  MAXNFL Input, integer MAXNFL, the maximum number of equations that
c         the program can handle.
c
c  NELEM  Input, integer NELEM, the number of elements.
c
c  NEQNFL Output, integer NEQNFL, the number of finite element equations used
c         to define the horizontal and vertical velocities and the
c         pressure.
c 
c  NODE   Output, integer NODE(6,NELEM), contains the numbers
c         of the nodes that make up each element.  Element number
c         I is associated with nodes NODE(1,I) through NODE(6,I).
c
c  NP     Input, integer NP, the number of nodes.
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
c  REGION Input, character*20 REGION.
c         REGION specifies the flow region.
c
c         'cavity', a driven cavity, 1 unit on each side, open on 
c         the top with a tangential velocity specification there.
c
c         'channel', a channel, 10 units long by 3 high, inflow on 
c         the left, outflow on the right, with a bump on the bottom.
c
c         'step', a channel, 12 units long by 3 high, inflow on the
c         left, outflow on the right, with a step on the bottom.
c
c  XBL    Input, double precision XBL.
c
c         The X coordinate of the left corner of the bump.
c 
c  XBR    Input, double precision XBR.
c
c         The X coordinate of the right corner of the bump.
c
c  XRANGE Input, double precision XRANGE.
c         The total width of the region.
c
      implicit double precision (a-h,o-z)
c
      integer maxnfl
      integer nelem
      integer np
c
      character*2 eqn(maxnfl)
      integer ibump
      integer icol
      integer icol2
      integer ielem
      integer indx(3,np)
      integer ip
      integer irow
      integer irow2
      integer isotri(nelem)
      integer jcol
      integer jrow
      logical leqi
      integer nbleft
      integer nbrite
      integer neqnfl
      integer node(6,nelem)
      integer nx
      integer ny
      character*20 region
      double precision xbl
      double precision xbr
      double precision xrange
      double precision ybr
      double precision yrange
c
c  Compute the global node numbers that will be assigned to the 
c  beginning and ending of the bump.  These numbers are only used to 
c  determine which elements are isoparametric.
c
      if(leqi(region,'channel'))then
        nbleft=nint(xbl*(2*nx-2)/xrange)+1
        nbrite=nint(xbr*(2*nx-2)/xrange)+1
c
c  Or else determine the horizontal and vertical mesh lines that will 
c  pass through the point (XBR,YBR).  Here, we are assuming the
c  step is vertical, and nonnegative!
c
      elseif(leqi(region,'step'))then
        jcol=2*nint(xbr*(nx-1)/xrange)+1
        jrow=2*nint(ybr*(ny-1)/yrange)+1
      endif
c
c  Consider each of the NP nodes, which logically lie in an MX by MY
c  rectangular array.  A pair of new elements must be generated every
c  time we reach a node that lies in an odd row and column, (except for
c  the top row, and last column, of course).  At every node, we
c  will have to decide how many equations to generate.
c
      ielem=0
      neqnfl=0

      do ip=1,np
c
c  Determine the row and column of this node, and also whether each
c  of these quantities is odd or even.
c
        icol=((ip-1)/(2*ny-1))+1
        irow=mod((ip-1),2*ny-1)+1
 
        icol2=mod(icol,2)
        irow2=mod(irow,2)
c
c  If both the row and the column are odd, and we're not in the last
c  column or top row, then we can define two new triangular elements 
c  based at the node.
c
c  Given the following arrangement of nodes, for instance:
c
c    05 10 15 20 25
c    04 09 14 19 24
c    03 08 13 18 23
c    02 07 12 17 22
c    01 06 11 16 21
c
c  when we arrive at node 13, we will define
c
c    element 7: (25, 13, 15, 19, 14, 20)
c    element 8: (13, 25, 23, 19, 24, 18)
c
        if((irow2.eq.1.and.icol2.eq.1).and.
     &     (icol.ne.2*nx-1).and.(irow.ne.2*ny-1))then
 
          ielem=ielem+1
 
          node(1,ielem)=ip+2*(2*ny-1)+2
          node(2,ielem)=ip
          node(3,ielem)=ip+2
          node(4,ielem)=ip+(2*ny-1)+1
          node(5,ielem)=ip+1
          node(6,ielem)=ip+(2*ny-1)+2
c
c  Determine if the elements are isoparametric.
c
          if(leqi(region,'cavity'))then

            isotri(ielem)=0

          elseif(leqi(region,'channel'))then

            if(ibump.eq.0)then
 
              if(icol.ge.nbleft.and.icol.lt.nbrite)then
                isotri(ielem)=1
              else
                isotri(ielem)=0
              endif
 
            elseif(ibump.eq.1)then
 
              if(icol.ge.nbleft.and.icol.lt.nbrite)then
                isotri(ielem)=1
              else
                isotri(ielem)=0
              endif
 
            elseif(ibump.eq.2)then
 
              if(icol.ge.nbleft.and.icol.lt.nbrite)then
                isotri(ielem)=2
              else
                isotri(ielem)=0
              endif
 
            else
 
              isotri(ielem)=2
 
            endif

          elseif(leqi(region,'step'))then

            isotri(ielem)=0

          endif
 
          ielem=ielem+1
 
          node(1,ielem)=ip
          node(2,ielem)=ip+2*(2*ny-1)+2
          node(3,ielem)=ip+2*(2*ny-1)
          node(4,ielem)=ip+(2*ny-1)+1
          node(5,ielem)=ip+2*(2*ny-1)+1
          node(6,ielem)=ip+(2*ny-1)
 
          if(leqi(region,'cavity'))then

            isotri(ielem)=0

          elseif(leqi(region,'channel'))then

            if(ibump.eq.0)then
 
              if(icol.ge.nbleft.and.icol.lt.nbrite)then
                isotri(ielem)=1
              else
                isotri(ielem)=0
              endif
 
            elseif(ibump.eq.1)then
 
              if(irow.eq.1.and.icol.ge.nbleft.and.icol.lt.nbrite)then
                isotri(ielem)=2
              elseif(icol.ge.nbleft.and.icol.lt.nbrite)then
                isotri(ielem)=1
              else
                isotri(ielem)=0
              endif
 
            elseif(ibump.eq.2)then
 
              if(icol.ge.nbleft.and.icol.lt.nbrite)then
                isotri(ielem)=2
              else
                isotri(ielem)=0
              endif
 
            else
 
              isotri(ielem)=2
 
            endif

          elseif(leqi(region,'step'))then

            isotri(ielem)=0

          endif
 
        endif

        if(neqnfl+2.gt.maxnfl)then
          write(*,*)' '
          write(*,*)'SetNod - Fatal error!'
          write(*,*)'  Too many unknowns!'
          write(*,*)'  Processing node IP=',ip
          write(*,*)'  The maximum allowed is MAXNFL=',maxnfl
          write(*,*)'  This problem requires NEQNFL=',neqnfl+2
          stop
        endif
c
c  Now determine what equations to associate with this node.
c
c  CAVITY:
c
        if(leqi(region,'cavity'))then

          if(irow.eq.2*ny-1)then
 
            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='UI'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='VI'

          elseif(icol.eq.1.or.icol.eq.2*nx-1.or.irow.eq.1)then

            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='UW'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='VW'

          else

            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='U'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='V'

          endif
c
c  CHANNEL:
c
        elseif(leqi(region,'channel'))then
c
c  The node lies on the left hand inflow boundary.
c  The horizontal and vertical velocities are specified.
c
          if(icol.eq.1.and.1.lt.irow.and.irow.lt.2*ny-1)then
 
            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='UI'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='VI'
c
c  The node lies on the right hand boundary.
c  The horizontal velocity is an unknown, the vertical velocity is zero.
c
          elseif(icol.eq.2*nx-1.and.1.lt.irow.and.irow.lt.2*ny-1)then
 
            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='U'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='VW'
c
c  The node lies on the moving bump surface.
c  The horizontal and vertical velocities are zero.
c
          elseif(irow.eq.1.and.icol.gt.nbleft.and.icol.lt.nbrite)then
 
            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='UB'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='VB'
c
c  The node lies on a fixed wall.
c  The horizontal and vertical velocities are zero.
c
          elseif(icol.eq.1.or.icol.eq.2*nx-1.or.
     &      (irow.eq.1.and.icol.le.nbleft).or.
     &      (irow.eq.1.and.icol.ge.nbrite).or.irow.eq.2*ny-1)then
 
            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='UW'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='VW'
c
c  The node is a normal interior node.
c  The horizontal and vertical velocities are unknown.
c
          else
 
            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='U'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='V'
 
          endif
c
c  STEP:
c
c  The node lies on the left hand inflow boundary.
c  The horizontal and vertical velocities are specified.
c
        elseif(leqi(region,'step'))then

          if(icol.eq.1.and.1.lt.irow.and.irow.lt.2*ny-1)then
 
            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='UI'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='VI'
c
c  The node lies on the right hand boundary, above the JROW row.
c  The horizontal velocity is an unknown, the vertical velocity is zero.
c
          elseif(icol.eq.2*nx-1.and.jrow.lt.irow.and.irow.lt.2*ny-1)then
 
            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='U'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='VW'
c
c  The node lies on a fixed wall or step.
c  The horizontal and vertical velocities are zero.
c
          elseif(
     &      (irow.eq.1.and.icol.le.jcol).or.
     &      (irow.le.jrow.and.icol.eq.jcol).or.
     &      (irow.eq.jrow.and.icol.ge.jcol).or.
     &      (irow.eq.2*ny-1) )then

            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='UW'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='VW'
c
c  The node lies in the "dead" zone.
c
          elseif(irow.lt.jrow.and.icol.gt.jcol)then

            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='U0'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='V0'
c
c  The node is a normal interior node.
c  The horizontal and vertical velocities are unknown.
c
          else
 
            neqnfl=neqnfl+1
            indx(1,ip)=neqnfl
            eqn(neqnfl)='U'
 
            neqnfl=neqnfl+1
            indx(2,ip)=neqnfl
            eqn(neqnfl)='V'
 
          endif
        endif
c
c  On nodes in an odd row and column, add a pressure equation.
c
        if(irow2.eq.1.and.icol2.eq.1)then

          neqnfl=neqnfl+1

          if(neqnfl.gt.maxnfl)then
            write(*,*)' '
            write(*,*)'SetNod - Fatal error!'
            write(*,*)'  Too many unknowns!'
            write(*,*)'  Processing node IP=',ip
            write(*,*)'  The maximum allowed is MAXNFL=',maxnfl
            write(*,*)'  This problem requires NEQNFL=',neqnfl
            stop
          endif

          indx(3,ip)=neqnfl

          eqn(neqnfl)='P'

          if(leqi(region,'step'))then
            if(irow.lt.jrow.and.icol.gt.jcol)then
              eqn(neqnfl)='P0'
            endif
          endif

        else
          indx(3,ip)=0
        endif
 


      enddo
c
c  The last equation, which is guaranteed to be a pressure equation,
c  is replaced by a pressure boundary condition, associated with
c  an unknown.  (Even though we know this pressure will be zero).
c
      eqn(neqnfl)='PB'

      return
      end
      subroutine setpfl(area,etaq,isotri,nelem,node,np,phifl,
     &  xc,xquad,xsiq,yc,yquad)
c
c***********************************************************************
c
cc SETPFL computes the value of the finite element basis functions at 
c  each quadrature point.  The basis functions are computed and saved
c  in this way for efficiency.
c
c
c  AREA   Input, double precision AREA(3,NELEM).
c
c         AREA contains the area of each element.  These values are
c         needed when computed the integrals associated with the
c         finite element method.
c
c         For runs in which the region is allowed to change from
c         step to step, AREA must be recalculated at each step.
c
c  ETAQ   Input, double precision ETAQ(3).
c         The "Eta" coordinates of the quadrature points.
c
c  ISOTRI Input, integer ISOTRI(NELEM).
c
c         0, the element is NOT isoparametric.  The six node
c         triangle has straight sides.
c
c         1, the element is isoparametric.  The six node triangle
c         has curved sides.  Many computations involving such an
c         element must be computed by using a reference triangle,
c         and evaluating the jacobian of a transformation between
c         that triangle and the element.
c
c  NELEM  Input, integer NELEM, the number of elements.
c
c  NODE   Input, integer NODE(6,NELEM), contains the numbers
c         of the nodes that make up each element.  Element number
c         I is associated with nodes NODE(1,I) through NODE(6,I).
c
c  NP     Input, integer NP, the number of nodes.
c
c  PHIFL  Output, double precision PHIFL(3,6,10,NELEM).  
c
c         PHIFL contains the value of a finite element basis function, its
c         derivative, or other information, evaluated at the quadrature 
c         points.
c 
c         The meaning of the entry PHIFL(I,J,K,L) is as follows.  
c         For the quadrature point I, and basis function J, in element L,
c         PHIFL(I,J,K,L) represents the value of:
c
c           K= 1, W, the finite element basis function for velocities;
c           K= 2, dWdX, the X derivative of W;
c           K= 3, dWdY, the Y derivative of W;
c           K= 4, Q, the finite element basis function for pressures;
c           K= 5, dQdX, the X derivative of Q;
c           K= 6, dQdY, the Y derivative of Q;
c           K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c           K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c           K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c           K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c         In particular, PHIFL(I,J,K,L) is the value of the quadratic 
c         basis function W associated with local node J in element L, 
c         evaluated at quadrature point I.
c 
c         Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6, 
c         since there are only three linear basis functions.
c
c  XC     Input, double precision XC(NP), contains the X coordinates
c         of the nodes.
c
c  XQUAD  Input, double precision XQUAD(3,NELEM), contains the
c         X coordinates  of the quadrature points in a given element.
c
c  XSIQ   Input, double precision XSIQ(3).
c         The "Xsi" coordinates of the quadrature points.
c
c  YC     Input, double precision YC(NP), contains the Y coordinates
c         of the nodes.
c
c  YQUAD  Input, double precision YQUAD(3,NELEM), contains the
c         Y coordinates of the quadrature points in a given element.
c
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer np
c
      double precision area(3,nelem)
      double precision det
      double precision detadx
      double precision detady
      double precision dqdx
      double precision dqdy
      double precision dwdx
      double precision dwdy
      double precision dxsidx
      double precision dxsidy
      double precision eta
      double precision etaq(3)
      integer i
      integer ielem
      integer iq
      integer isotri(nelem)
      integer node(6,nelem)
      double precision phifl(3,6,10,nelem)
      double precision q
      double precision w
      double precision xc(np)
      double precision xquad(3,nelem)
      double precision xq
      double precision xsi
      double precision xsiq(3)
      double precision yc(np)
      double precision yquad(3,nelem)
      double precision yq
c
c  Consider a particular element,
c  and a particular quadrature point (XQ,YQ) in that element.
c
c  Compute, at (XQ,YQ), the local values of the jacobian matrix
c  and its determinant.
c
c  Adjust the AREA array   
c
      do ielem=1,nelem
 
        do i=1,3
 
          xq=xquad(i,ielem)
          yq=yquad(i,ielem)
 
          if(isotri(ielem).eq.2)then
            eta=etaq(i)
            xsi=xsiq(i)
            call trans(det,detadx,detady,dxsidx,dxsidy,eta,ielem,
     &        nelem,node,np,xc,xsi,yc)
            area(i,ielem)=det*area(i,ielem)
          else
            eta=0.0
            xsi=0.0
          endif
c
c  Now consider each of the basis functions associated with a
c  node in the given element.
c
          do iq=1,6
c
c  If the element is NOT isoparametric, compute the basis values
c  directly.
c
c  For isoparametric elements, use the reference triangle method.
c
            if(isotri(ielem).eq.0.or.isotri(ielem).eq.1)then
 
              call bsp(q,dqdx,dqdy,ielem,iq,nelem,node,np,
     &          xc,xq,yc,yq)
 
              call qbf(ielem,iq,w,dwdx,dwdy,nelem,node,np,xc,
     &          xq,yc,yq)
 
              dxsidx=1.0
              dxsidy=0.0
              detadx=0.0
              detady=1.0
 
            else
 
              call refqbf(w,dwdx,dwdy,detadx,detady,dxsidx,dxsidy,
     &          eta,iq,xsi)
 
              call refbsp(q,dqdx,dqdy,detadx,detady,dxsidx,dxsidy,
     &          eta,iq,xsi)
 
            endif
c
c  Store the values into PHIFL.
c
            phifl(i,iq,1,ielem)=w
            phifl(i,iq,2,ielem)=dwdx
            phifl(i,iq,3,ielem)=dwdy
            phifl(i,iq,4,ielem)=q
            phifl(i,iq,5,ielem)=dqdx
            phifl(i,iq,6,ielem)=dqdy
            phifl(i,iq,7,ielem)=dxsidx
            phifl(i,iq,8,ielem)=dxsidy
            phifl(i,iq,9,ielem)=detadx
            phifl(i,iq,10,ielem)=detady     

          enddo
        enddo
      enddo

      return
      end
      subroutine setprb(eqn,indx,maxcofrb,maxelm,maxnfl,nelem,neqnfl,
     &  ncofrb,node,np,phifl,phirb,rb)
c
c***********************************************************************
c
cc SETPRB is given:
c
c    GFLRB, the full solution at which the reduced basis was generated;
c    PHIFL, the value of the finite element basis functions
c      at each quadrature point,
c    RB, the reduced basis vectors;
c
c  and computes: 
c
c    PHIRB, the value of the reduced basis functions at each quadrature 
c      point, for each reduced basis vector.
c
c  Note that the PHIFL contains the values of the finite element basis
c  functions at each quadrature point, and so we can compute ANY possible
c  finite element solution by "multiplying" PHIFL by a choice of coefficients
c  GFL.
c
c  What we are essentially doing is picking particular choices of coefficients,
c  namely the columns of the RB array, and computing the resulting values
c  of U, V, and P at each quadrature point in each element.  Then, later
c  on, a linear combination of reduced basis vectors can be evaluated
c  easily at any quadrature point simply by "multiplying" the reduced
c  basis coefficients by the entries of PHIRB that tell us the values
c  of the basis functions associated with each reduced basis vector.
c
c  Note that although the finite element basis functions are the same
c  for the U and V velocities, this is NOT true in the reduced basis.
c
c
c  This routine was modified on 11 July 1996.
c
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the global index of U, 
c         V and P at that node, or 0 or a negative value.  The global
c         index of U, V, or P is the index of the coefficient vector
c         that contains the value of the finite element coefficient
c         associated with the corresponding basis function at the
c         given node.
c  
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  MAXNFL Input, integer MAXNFL.
c         MAXNFL is the maximum number of equations or coefficients allowed 
c         for the full system.  MAXNFL must be used instead of NEQNFL as
c         the leading dimension of certain multi-dimensional arrays.
c 
c  NELEM  Input, integer NELEM, the number of elements.
c
c  NCOFRB Input, integer NCOFRB.
c         NCOFRB is the number of equations in the reduced system,
c         and also the number of reduced basis coefficients
c         which need to be determined by those equations.
c
c  NODE   Input, integer NODE(6,MAXELM) or NODE(6,NELEM).
c
c         NODE(I,J) contains, for an element J, the global index of 
c         the node whose local number in J is I.
c
c         The local ordering of the nodes is suggested by this diagram:
c
c           Global nodes   Elements      NODE
c                                                        1  2  3  4  5  6
c           74  84  94     3-6-1   2     Left element =  (94,72,74,83,73,84)
c                          |  /   /|
c           73  83  93     5 4   4 5     Right element = (72,94,92,83,93,82)
c                          |/   /  |
c           72  82  92     2   1-6-3
c
c  NP     Input, integer NP.
c         NP is the number of nodes used to define the finite element mesh.
c         Typically, the mesh is generated as a rectangular array, with
c         an odd number of nodes in the horizontal and vertical directions.
c         The formula for NP is NP=(2*NX-1)*(2*NY-1).
c  
c  PHIFL  Input, double precision PHIFL(3,6,10,NELEM).  
c
c         PHIFL contains the value of a finite element basis function, its
c         derivative, or other information, evaluated at the quadrature 
c         points.
c 
c         The meaning of the entry PHIFL(I,J,K,L) is as follows.  
c         For the quadrature point I, and basis function J, in element L,
c         PHIFL(I,J,K,L) represents the value of:
c
c           K= 1, W, the finite element basis function for velocities;
c           K= 2, dWdX, the X derivative of W;
c           K= 3, dWdY, the Y derivative of W;
c           K= 4, Q, the finite element basis function for pressures;
c           K= 5, dQdX, the X derivative of Q;
c           K= 6, dQdY, the Y derivative of Q;
c           K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c           K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c           K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c           K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c         In particular, PHIFL(I,J,K,L) is the value of the quadratic 
c         basis function W associated with local node J in element L, 
c         evaluated at quadrature point I.
c 
c         Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6, 
c         since there are only three linear basis functions.
c
c  PHIRB  Output, double precision PHIRB(3,NCOFRB,15,NELEM).
c         PHIRB contains the values of a finite element basis function
c         or its X or Y derivative, in a given element, at a given
c         quadrature point, for a particular reduced basis function.
c
c         For PHIRB(I,J,K,L), index J refers to the reduced basis
c         basis functions, for J=0 to NCOFRB.
c
c         The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c           For the quadrature point I, and reduced basis function J,
c           in element L, PHIRB(I,J,K,L) represents the value of:
c
c             K=1, WUrb, the finite element U velocity basis function;
c             K=2, dWUrbdX, the X derivative of WUrb;
c             K=3, dWUrbdY, the Y derivative of WUrb;
c             K=4, WVrb, the finite element V velocity basis function;
c             K=5, dWVrbdX, the X derivative of WVrb;
c             K=6, dWVrbdY, the Y derivative of WVrb;
c             K=7, Q, the finite element pressure basis function.
c             K=8, dQrbdX, the X derivative of Qrb;
c             K=9, dQrbdY, the Y derivative of Qrb.
c             K=10, WU0rb, same as WUrb, with zero BC.
c             K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c             K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c             K=13, WV0rb, same as WVrb, with zero BC.
c             K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c             K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  RB     Input, double precision rb(maxnfl,ncofrb).
c         RB is the NEQNFL by NCOFRB array of reduced basis vectors.
c
c         RB is generated by computing a finite element solution GFL,
c         which is saved for later reference as "GFLRB".
c         GFLRB is copied into the first column of RB.
c         Then, we compute the first NCOFRB derivatives of GFLRB with
c         respect to a parameter.  The first derivative
c         is stored in column 1 of RB, and so on.  Then we orthogonalize
c         the columns of RB.
c
      implicit double precision (a-h,o-z)
c
      integer maxcofrb
      integer maxelm
      integer maxnfl
      integer nelem
      integer neqnfl
      integer ncofrb
      integer np
c
      double precision dqdx
      double precision dqdy
      double precision dqrbdx
      double precision dqrbdy
      double precision dwdx
      double precision dwdy
      double precision dwu0rbdx
      double precision dwu0rbdy
      double precision dwurbdx
      double precision dwurbdy
      double precision dwv0rbdx
      double precision dwv0rbdy
      double precision dwvrbdx
      double precision dwvrbdy
      character*2 eqn(neqnfl)
      integer ielem
      integer ieqnrb
      integer iglob
      integer ilocal
      integer indx(3,np)
      integer iquad
      logical leqi
      integer nglob
      integer node(6,nelem)
      double precision phifl(3,6,10,nelem)
      double precision phirb(3,maxcofrb,15,maxelm)
      double precision q
      double precision qrb
      double precision rb(maxnfl,ncofrb)
      double precision w
      double precision wu0rb
      double precision wurb
      double precision wv0rb
      double precision wvrb
c
c  Consider element IELEM...
c
      do ielem=1,nelem
c
c  ...and quadrature point IQUAD in element IELEM...
c
        do iquad=1,3
c
c  ...and reduced basis function IEQNRB, evaluated at
c  quadrature point IQUAD in element IELEM.
c
          do ieqnrb=1,ncofrb

            wurb=0.0
            dwurbdx=0.0
            dwurbdy=0.0

            wvrb=0.0
            dwvrbdx=0.0
            dwvrbdy=0.0

            qrb=0.0
            dqrbdx=0.0
            dqrbdy=0.0

            wu0rb=0.0
            dwu0rbdx=0.0
            dwu0rbdy=0.0

            wv0rb=0.0
            dwv0rbdx=0.0
            dwv0rbdy=0.0
c
c  Now add up the U, V, or P finite element coefficients, weighted by the 
c  values of the finite element basis functions or derivatives, at the
c  quadrature point.
c
            do ilocal=1,6

              w   =phifl(iquad,ilocal,1,ielem)
              dwdx=phifl(iquad,ilocal,2,ielem)
              dwdy=phifl(iquad,ilocal,3,ielem)
 
              q   =phifl(iquad,ilocal,4,ielem)
              dqdx=phifl(iquad,ilocal,5,ielem)
              dqdy=phifl(iquad,ilocal,6,ielem)

              nglob=node(ilocal,ielem)
              
              iglob=indx(1,nglob)

              wurb   =wurb   +rb(iglob,ieqnrb)*w
              dwurbdx=dwurbdx+rb(iglob,ieqnrb)*dwdx
              dwurbdy=dwurbdy+rb(iglob,ieqnrb)*dwdy
 
              if(leqi(eqn(iglob),'u'))then
                wu0rb   =wu0rb   +rb(iglob,ieqnrb)*w
                dwu0rbdx=dwu0rbdx+rb(iglob,ieqnrb)*dwdx
                dwu0rbdy=dwu0rbdy+rb(iglob,ieqnrb)*dwdy
              endif

              iglob=indx(2,nglob)

              wvrb   =wvrb   +rb(iglob,ieqnrb)*w
              dwvrbdx=dwvrbdx+rb(iglob,ieqnrb)*dwdx
              dwvrbdy=dwvrbdy+rb(iglob,ieqnrb)*dwdy
 
              if(leqi(eqn(iglob),'v'))then
                wv0rb   =wv0rb   +rb(iglob,ieqnrb)*w
                dwv0rbdx=dwv0rbdx+rb(iglob,ieqnrb)*dwdx
                dwv0rbdy=dwv0rbdy+rb(iglob,ieqnrb)*dwdy
              endif

              iglob=indx(3,nglob)

              if(iglob.gt.0)then

                qrb   =qrb   +rb(iglob,ieqnrb)*q
                dqrbdx=dqrbdx+rb(iglob,ieqnrb)*dqdx
                dqrbdy=dqrbdy+rb(iglob,ieqnrb)*dqdy

              endif

            enddo
c
c  Save the values of the finite element basis functions associated
c  with the given reduced basis vector.
c
            phirb(iquad,ieqnrb,1,ielem)=wurb
            phirb(iquad,ieqnrb,2,ielem)=dwurbdx
            phirb(iquad,ieqnrb,3,ielem)=dwurbdy  

            phirb(iquad,ieqnrb,4,ielem)=wvrb
            phirb(iquad,ieqnrb,5,ielem)=dwvrbdx
            phirb(iquad,ieqnrb,6,ielem)=dwvrbdy 

            phirb(iquad,ieqnrb,7,ielem)=qrb
            phirb(iquad,ieqnrb,8,ielem)=dqrbdx
            phirb(iquad,ieqnrb,9,ielem)=dqrbdy 

            phirb(iquad,ieqnrb,10,ielem)=wu0rb
            phirb(iquad,ieqnrb,11,ielem)=dwu0rbdx
            phirb(iquad,ieqnrb,12,ielem)=dwu0rbdy  

            phirb(iquad,ieqnrb,13,ielem)=wv0rb
            phirb(iquad,ieqnrb,14,ielem)=dwv0rbdx
            phirb(iquad,ieqnrb,15,ielem)=dwv0rbdy 
          enddo
        enddo
 
      enddo

      return
      end
      subroutine setq3(area,etaq,isotri,nelem,node,np,wquad,xc,xquad,
     &  xsiq,yc,yquad)
c
c***********************************************************************
c
cc SETQ3 sets the abscissas and weights for a three point quadrature 
c  rule on a triangle.
c
c
c  AREA   Output, double precision AREA(3,NELEM).
c
c  ETAQ   Input, double precision ETAQ(3).
c         The "Eta" coordinates of the quadrature points.
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
c  NELEM  Input, integer NELEM, the number of elements.
c
c  NODE   Input, integer NODE(6,NELEM).
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
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c 
c  WQUAD  Input, double precision WQUAD(3), the weights for Gaussian
c         quadrature.
c
c  XC     Input, double precision XC(NP).
c
c         The X coordinates of the nodes.
c 
c  XQUAD  Output, double precision XQUAD(3,NELEM).
c
c         The X coordinates of the quadrature points for
c         each element.
c
c  XSIQ   Input, double precision XSIQ(3).
c         The "Xsi" coordinates of the quadrature points.
c
c  YC     Input, double precision YC(NP).
c
c         The Y coordinates of the nodes.
c 
c  YQUAD  Output, double precision YQUAD(3,NELEM).
c
c         The Y coordinates of the quadrature points for
c         each element.
c
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer np
c
      double precision area(3,nelem)
      double precision eta
      double precision etaq(3)
      integer i
      integer ielem
      integer ip1
      integer ip2
      integer ip3
      integer iquad
      integer isotri(nelem)
      integer node(6,nelem)
      double precision wquad(3)
      double precision x
      double precision xc(np)
      double precision xquad(3,nelem)
      double precision xsi
      double precision xsiq(3)
      double precision y
      double precision yc(np)
      double precision yquad(3,nelem)
c
c  Set the weights.
c
      do i=1,3
        wquad(i)=1.0/6.0
      enddo
c
c  Set the quadrature points in the reference element.
c
      xsiq(1)=0.5
      etaq(1)=0.5

      xsiq(2)=1.0
      etaq(2)=0.5

      xsiq(3)=0.5
      etaq(3)=0.0
c
c  Set the X, Y coordinates of quadrature points for each element.
c
      do ielem=1,nelem
 
        do i=1,3
          xsi=xsiq(i)
          eta=etaq(i)
          call xofxsi(eta,ielem,nelem,node,np,x,xc,xsi,y,yc)
          xquad(i,ielem)=x
          yquad(i,ielem)=y
        enddo
c
c  We only calculate true areas for nonisoparametric elements.
c
        ip1=node(1,ielem)
        ip2=node(2,ielem)
        ip3=node(3,ielem)

        do iquad=1,3

          if(isotri(ielem).eq.0.or.isotri(ielem).eq.1)then
 
            area(iquad,ielem)=wquad(iquad)*abs(
     &         (yc(ip1)+yc(ip2))*(xc(ip2)-xc(ip1))
     &        +(yc(ip2)+yc(ip3))*(xc(ip3)-xc(ip2))
     &        +(yc(ip3)+yc(ip1))*(xc(ip1)-xc(ip3)) )
 
          else
 
            area(iquad,ielem)=wquad(iquad)
 
          endif
  
        enddo

      enddo
 
      return
      end
      subroutine setxy(gridx,gridy,ibs,np,nparb,nx,ny,region,splbmp,
     &  taubmp,xbl,xbr,xc,xrange,ybl,ybr,yc,yrange)
c
c***********************************************************************
c
cc SETXY sets the X and Y coordinates of the nodes.
c
c
c  SETXY assumes that the nodes are numbered
c  in "stacks", starting with the least X and Y coordinates,
c  then fixing X and running through all values of Y, then
c  increasing X to the next value and running through all
c  values of Y, and so on.  For example:
c
c    5  10  15
c    4   9  14
c    3   8  13
c    2   7  12
c    1   6  11
c
c  SETXY allows a certain number of schemes for computing the
c  grid in the X and Y directions, aside from uniform spacing.
c  However, SETXY forces the nodes in "even" rows to have the
c  Y coordinates that are the average of the nodes above and
c  below, and nodes in "even" columns to have the X coordinates
c  that are the average of the nodes left and right of them.
c
c
c  GRIDX  Input, character*20 GRIDX.
c         GRIDX tells how the finite element nodes should be layed out
c         in the X direction.
c         'uniform' makes them equally spaced.
c         'cos' uses the COS function to cluster them near edges.
c         'sqrtsin' uses the SQRT(SIN()) function.
c
c  GRIDY  Input, character*20 GRIDY.
c         GRIDY tells how the finite element nodes should be layed out
c         in the Y direction.
c         'uniform' makes them equally spaced.
c         'cos' uses the COS function to cluster them near edges.
c         'sqrtsin' uses the SQRT(SIN()) function.
c
c  IBS    Input, integer IBS.
c         IBS is the bump shape option.
c         0, piecewise constant function.
c         1, piecewise linear function.
c         2, piecewise quadratic function.
c
c  NP     Input, integer NP, the number of nodes.
c
c  NPARB  Input, integer NPARB.
c
c         The number of parameters associated with the position and 
c         shape of the bump.
c 
c         Note that if NPARB=0, the bump is replaced by a flat wall.
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
c    lines in the Y direction.
c
c    Roughly speaking, NY (or 2*NY) is the number of elements along
c    a line in the Y direction.
c
c  REGION
c    Input, character ( len = 20 ) REGION.
c    REGION specifies the flow region.
c
c    'cavity', a driven cavity, 1 unit on each side, open on
c    the top with a tangential velocity specification there.
c
c    'cavity2', a driven cavity, 1 unit on each side, open on
c    the top and bottom with tangential velocity specifications there.
c
c    'channel', a channel, 10 units long by 3 high, inflow on
c    the left, outflow on the right, with a bump on the bottom.
c
c    'step', a channel, 12 units long by 3 high, inflow on the
c    left, outflow on the right, with a step on the bottom.
c
c  SPLBMP
c    Input, double precision SPLBMP(NPARB+2).
c
c    SPLBMP contains the spline coefficients for the bump.
c
c  TAUBMP
c    Input, double precision TAUBMP(NPARB+2).
c
c    TAUBMP contains the location of the spline abscissas for
c    the bump.  There are NPARB+2 of them, because the end values
c    of the spline are constrained to have particular values.
c
c  XBL  
c    Input, double precision XBL.
c    XBL is the X coordinate of the left corner
c    of the bump.
c
c  XBR  
c    Input, double precision XBR.
c    XBR is the X coordinate of the right corner
c    of the bump.
c
c  XC   
c    Output, double precision XC(NP).
c    XC is the X coordinates of the nodes.
c
c  YBL   
c    Input, double precision YBL.
c    YBL is the Y coordinate of the left corner of the bump.
c
c  YBR   
c    Input, double precision YBR.
c    YBR is the Y coordinate of the right corner of the bump.
c
c  YC    
c    Output, double precision YC(NP).
c    YC is the Y coordinates of the nodes.
c
      implicit none

      integer np
      integer nparb
      integer nx
      integer ny

      character * 20 gridx
      character * 20 gridy
      integer ibs
      integer icol
      integer ihi
      integer ilo
      integer ip
      integer ipe
      integer ipn
      integer ips
      integer ipw
      integer irow
      integer jcol
      integer jrow
      logical s_eqi
      character * 20 region
      double precision splbmp(nparb+2)
      double precision taubmp(nparb+2)
      double precision x
      double precision xbl
      double precision xbr
      double precision xc(np)
      double precision xhi
      double precision xlo
      double precision xrange
      double precision y
      double precision ybl
      double precision ybot
      double precision ybr
      double precision yc(np)
      double precision yhi
      double precision ylo
      double precision yrange
!
      ip = 0
!
!  For the step problem only,
!  set the odd row and column of the corner of the step.
!
      if ( s_eqi ( region,'step')) then
        jcol = 2*nint(xbr*(nx-1)/xrange)+1
        jrow = 2*nint(ybr*(ny-1)/yrange)+1
      end if
!
!  Consider each column of the region.
!
      do icol = 1,2*nx-1

        if ( s_eqi ( region,'cavity') .or.
     &       s_eqi ( region,'cavity2')) then

          ilo = 1
          ihi = 2*nx-1
          xlo = 0.0D+00
          xhi = xrange

          call grid(gridx,icol,ihi,ilo,x,xhi,xlo)

        else if ( s_eqi ( region,'channel')) then

          ilo = 1
          ihi = 2*nx-1
          xlo = 0.0D+00
          xhi = xrange
 
          call grid(gridx,icol,ihi,ilo,x,xhi,xlo)

          if ( abs(x-xbl)*(2*nx-2) <= 0.5) then
            x = xbl
          else if ( abs(x-xbr)*(2*nx-2) <= 0.5) then
            x = xbr
          end if

        else if ( s_eqi ( region,'step')) then

          if ( icol < jcol) then

            ilo = 1
            ihi = jcol
            xlo = 0.0D+00
            xhi = xbr

            call grid(gridx,icol,ihi,ilo,x,xhi,xlo)

          else if ( icol == jcol) then
            x = xbr
          else if ( jcol < icol ) then

            ilo = jcol
            ihi = 2*nx-1
            xlo = xbr
            xhi = xrange

            call grid(gridx,icol,ihi,ilo,x,xhi,xlo)

          end if

        end if
c
c  Consider each row of the region.
c
        do irow = 1,2*ny-1

          ip = ip+1

          if ( s_eqi ( region,'cavity') .or.
     &         s_eqi ( region,'cavity2')) then

            ilo = 1
            ihi = 2*ny-1
            ylo = 0.0D+00
            yhi = yrange

            call grid(gridy,irow,ihi,ilo,y,yhi,ylo)

          else if ( s_eqi ( region,'channel')) then

            if ( x <= xbl) then
              ybot = ybl
            else if ( xbl <= x .and. x <= xbr ) then
              if ( ibs == 0) then
                call pcval(nparb+1,x,taubmp,ybot,splbmp)
              else if ( ibs == 1) then
                call plval(nparb+2,x,taubmp,ybot,splbmp)
              else if ( ibs == 2) then
                call pqval(nparb+2,x,taubmp,ybot,splbmp)
              end if
            else
              ybot = ybr
            end if

            ilo = 1
            ihi = 2*ny-1
            ylo = ybot
            yhi = yrange

            call grid(gridy,irow,ihi,ilo,y,yhi,ylo)

          else if ( s_eqi ( region,'step')) then

            if ( irow < jrow) then

              ilo = 1
              ihi = jrow
              ylo = 0.0D+00
              yhi = ybr

              call grid(gridy,irow,ihi,ilo,y,yhi,ylo)

            else if ( irow == jrow) then
              y = ybr
            else if ( jrow < irow ) then

              ilo = jrow
              ihi = 2*ny-1
              ylo = ybr
              yhi = yrange

              call grid(gridy,irow,ihi,ilo,y,yhi,ylo)

            end if

          end if

          xc(ip) = x
          yc(ip) = y

        end do
      end do
c
c  Average the X coordinates of all nodes that lie in even columns.
c
      do irow = 1,2*ny-1
        do icol = 2,2*nx-2,2
          ip = (icol-1)*(2*ny-1)+irow
          ipw = ip-(2*ny-1)
          ipe = ip+(2*ny-1)
          xc(ip) = 0.5D+00 *(xc(ipe)+xc(ipw))
        end do
      end do
c
c  Average the Y coordinates of all nodes that lie in even rows.
c
      do irow = 2,2*ny-2,2
        do icol = 1,2*nx-1
          ip = (icol-1)*(2*ny-1)+irow
          ipn = ip+1
          ips = ip-1
          yc(ip) = 0.5D+00 *(yc(ipn)+yc(ips))
        end do
      end do

      return
      end
      subroutine step(ibs,ibump,ifs,iopt,maxopt,maxpar,
     &  npar,nparb,nparf,npe,nx,ny,par,region,reynld,
     &  tolnew,tolopt,tolsim,wateb,wateu,watev,watep,xbl,
     &  xbr,xprof,xrange,ybl,ybr,yrange)
c 
c***********************************************************************
c
cc STEP sets up a forward facing step problem.
c 
c  Reference:
c
c  Janet Peterson,
c  The Reduced Basis Method for Incompressible Viscous Flow Calculations,
c  SIAM Journal of Scientific and Statistical Computing,
c  Volume 10, Number 4, pages 777-786, July 1989.
c
c
      implicit double precision (a-h,o-z)
c
      integer maxpar
c
      integer i
      integer ibs
      integer ibump
      integer ifs
      integer iopt(maxpar)
      integer maxopt
      integer npar
      integer nparb
      integer nparf
      integer npe
      integer nx
      integer ny
      double precision par(maxpar)
      character*20 region
      double precision reynld
      double precision tolnew
      double precision tolopt
      double precision tolsim
      double precision wateb
      double precision watep
      double precision wateu
      double precision watev
      double precision xbl
      double precision xbr
      double precision xprof
      double precision xrange
      double precision ybl
      double precision ybr
      double precision yrange
c
      ibs=0
      ibump=0
c
c  The inflow is to be a (piecewise) quadratic function.
c
      ifs=2
      maxopt=10
      nparb=0
c
c  There is only one unknown coefficient for the inflow function,
c  so there is only one "piece".
c
      nparf=1
      npe=6
c
c  Peterson used a nonuniform mesh with NX=70 and NY=35!
c
      nx=11
      ny=4
      region='step'
      tolnew=0.0000000001
      tolopt=0.000000001
      tolsim=0.0000000001
      wateb=0.0
      wateu=1.0
      watev=1.0
      watep=0.0
      xbl=4.0
      xbr=4.0
      xprof=3.0
      xrange=12.0
      ybl=1.0
      ybr=1.0
      yrange=3.0
c
c  Set things that depend on other things.
c
      npar=nparf+nparb+1

      do i=1,nparf
        iopt(i)=0
      enddo

      do i=nparf+1,nparf+nparb
        iopt(i)=0
      enddo

      iopt(nparf+nparb+1)=1
c
c  The inflow parameter should be 1 to match Peterson's paper.
c
      par(1)=1.0
c
c  The REYNLD parameter may be varied.  Here, it is arbitrarily
c  set to 1.  Peterson worked with values up to 1500.
c
      reynld=1.0
      par(2)=reynld
 
      return
      end
      subroutine trans(det,detadx,detady,dxsidx,dxsidy,eta,ielem,
     &  nelem,node,np,xc,xsi,yc)

c***********************************************************************
c
cc TRANS calculates the biquadratic transformation which maps the 
c  reference element in (XSI,ETA) space into a particular 
c  isoparametric element in (X,Y) space.
c
c  We know everything about the isoparametric element once we
c  specify the location of its six nodes.
c
c  TRANS computes the entries of the jacobian of the transformation
c  and the determinant of the jacobian.  Essentially, the jacobian
c  records the relationship between derivatives with respect to XSI
c  and ETA and a point in the reference element, and derivatives
c  with respect to X and Y of the same function as defined in the
c  isoparametric element.
c
c  The four entries of the jacobian are symbolically named DETADX,
c  DETADY, DXSIDX and DXSIDY, and we know that the jacobian gives
c  us the following relation between derivatives with respect to
c  XSI and ETA, and derivatives with respect to X and Y:
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
c            XSI
c
c
c  DET    Output, double precision DET, the determinant of the jacobian
c         of the transformation between the reference and isoparametric
c         elements.
c
c  DETADX,
c  DETADY Output, double precision DETADX, DETADY, the partial 
c         derivative d ETA/d X and d ETA/d Y at (XSI,ETA).
c
c  DXSIDX,
c  DXSIDY Output, double precision DXSIDX, DXSIDY, the partial 
c         derivative d XSI/d X and d XSI/d Y at (XSI,ETA).
c
c  ETA    Input, double precision ETA, the ETA coordinate of the point.
c
c  IELEM  Input, integer IELEM, the number of the isoparametric
c         element we are examining.
c
c  NELEM  Input, integer NELEM, the number of elements.
c
c  NODE   Input, integer NODE(6,NELEM), contains the numbers
c         of the nodes that make up each element.  Element number
c         I is associated with nodes NODE(1,I) through NODE(6,I).
c
c  NP     Input, integer NP, the number of nodes.
c
c  XC     Input, double precision XC(NP), the X coordinates of the 
c         nodes.
c
c  XSI    Input, double precision XSI, the XSI coordinate of the point.
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
      double precision det
      double precision detadx
      double precision detady
      double precision dxdeta
      double precision dxdxsi
      double precision dxsidx
      double precision dxsidy
      double precision dydeta
      double precision dydxsi
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
c    X(XSI,ETA) = A1 * XSI**2 + B1 * XSI*ETA + C1 * ETA**2
c               + D1 * XSI    + E1 * ETA     + F1
c
c    Y(XSI,ETA) = A2 * XSI**2 + B2 * XSI*ETA + C2 * ETA**2
c               + D2 * XSI    + E2 * ETA     + F2
c
      a1= 2.0*xn(1)+2.0*xn(3)-4.0*xn(6)
      b1=-4.0*xn(3)-4.0*xn(4)+4.0*xn(5)+4.0*xn(6)
      c1= 2.0*xn(2)+2.0*xn(3)-4.0*xn(5)
      d1=-3.0*xn(1)      -xn(3)+4.0*xn(6)
      e1=      -xn(2)      +xn(3)+4.0*xn(4)-4.0*xn(6)
      f1=       xn(1)
 
      a2= 2.0*yn(1)+2.0*yn(3)-4.0*yn(6)
      b2=-4.0*yn(3)-4.0*yn(4)+4.0*yn(5)+4.0*yn(6)
      c2= 2.0*yn(2)+2.0*yn(3)-4.0*yn(5)
      d2=-3.0*yn(1)      -yn(3)+4.0*yn(6)
      e2=      -yn(2)      +yn(3)+4.0*yn(4)-4.0*yn(6)
      f2=       yn(1)
c
c  Compute the partial derivatives at the point (XSI,ETA).
c  This is the jacobian matrix
c
c    J: (XSI,ETA) --> (X,Y).
c
      dxdxsi= 2.0*a1*xsi +       b1*eta + d1
      dxdeta=       b1*xsi + 2.0*c1*eta + e1
 
      dydxsi= 2.0*a2*xsi +       b2*eta + d2
      dydeta=       b2*xsi + 2.0*c2*eta + e2
c
c  Compute the determinant of the jacobian matrix:
c
c    J: (XSI,ETA) --> (X,Y)
c
      det=dxdxsi*dydeta-dxdeta*dydxsi
c
c  Watch out for a zero determinant.
c
      if(det.eq.0.0)then
        write(*,*)' '
        write(*,*)'Trans - Fatal error!'
        write(*,*)'  The jacobian J: (XSI,ETA) --> (X,Y) is singular!'
        write(*,*)'  This occurred for element number ',ielem
        write(*,*)'  Local coordinates XSI,ETA=',xsi,eta
        x=a1*xsi**2+b1*xsi*eta+c1*eta**2+d1*xsi+e1*eta+f1
        y=a2*xsi**2+b2*xsi*eta+c2*eta**2+d2*xsi+e2*eta+f2
        write(*,*)'  Global coordinates X,Y=',x,y
        write(*,*)' '
        write(*,*)'  The X, Y nodes were:'
        write(*,*)' '
        do i=1,6
          write(*,*)xn(i),yn(i)
        enddo
 
        stop
      endif
c
c  Compute
c
c    d ETA/d X, d ETA/d Y, d XSI/d X, d XSI/d Y
c
c  by inverting the jacobian matrix
c
c    J: (XSI,ETA) --> (X,Y)
c
c  to get the jacobian matrix
c
c    J: (X,Y) --> (XSI,ETA).
c
c  This uses the simple fact that the inverse of
c
c    (a b)
c    (c d)
c
c  is
c
c    1/(ad-bc) * ( d -b)
c                (-c  a)
c
      dxsidx= dydeta/det
      dxsidy=-dxdeta/det
 
      detadx=-dydxsi/det
      detady= dxdxsi/det
 
      return
      end
      subroutine uvpnrm(gfl,indx,neqnfl,np,pnorm,uvnorm)

c*********************************************************************72
c
cc UVPNRM returns the "norm" of the solution.  Here, the norm of
c  a solution GFL = (U,V,P) is defined as two numbers, the maximum
c  velocity magnitude at a node, and the maximum pressure at a node.
c
c 
c  Modified:
c
c    07 October 1996
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  GFL    Input, double precision GFL(NEQNFL).
c
c         GFL is the current solution vector, in which are stored
c         the finite element coefficients that define the velocity
c         and pressure functions, U, V and P.
c
c  INDX   Input, integer INDX(3,NP). 
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at
c         that node, or 0 or a negative value.
c
c         If K = INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  NEQNFL Input, integer NEQNFL, the number of finite element equations used
c         to define the horizontal and vertical velocities and the
c         pressure.
c
c  NP     Input, integer NP, the number of nodes.
c
c  PNORM  Output, double precision PNORM, the maximum absolute value
c         pressure coefficient.
c
c  UVNORM Output, double precision UVNORM, the maximum velocity magnitude.
c
      implicit none

      integer neqnfl
      integer np

      double precision gfl(neqnfl)
      integer i
      integer indx(3,np)
      double precision p
      double precision pnorm
      double precision u
      double precision uvnorm
      double precision v

      uvnorm = 0.0D+00
      pnorm = 0.0D+00

      do i = 1,np

        u = gfl(indx(1,i))
        v = gfl(indx(2,i))
        uvnorm = max(uvnorm,sqrt(u**2+v**2))

        if ( 0 < indx(3,i) ) then
          p = gfl(indx(3,i))
          pnorm = max(pnorm,abs(p))
        end if

      end do

      return
      end
      subroutine uvpqfl(dpdx,dpdy,dudx,dudy,dvdx,dvdy,gfl,ielem,indx,
     &  iquad,nelem,neqnfl,node,np,p,phifl,u,v)
c
c***********************************************************************
c
cc UVPQFL evaluates the velocities and pressure, and their X and Y
c  derivatives, at a quadrature point in a given element, given
c  the finite element coefficients, and the value of the basis functions
c  and their X and Y derivatives at each quadrature point.
c
c
c  DPDX,
c  DPDY   Output, double precision DPDX, DPDY, the derivatives of the
c         pressure function with respect to X and Y.
c
c  DUDX,
c  DUDY   Output, double precision DUDX, DUDY, the derivatives of the
c         horizontal velocity function with respect to X and Y.
c
c  DVDX,
c  DVDY   Output, double precision DVDX, DVDY, the derivatives of the
c         vertical velocity function with respect to X and Y.
c
c  GFL    Input, double precision GFL(NEQNFL), the current solution 
c         estimate for the full problem.
c
c  IELEM  Input, integer IELEM, the element in which the quadrature
c         point lies.
c
c  INDX   Input, integer INDX(3,NP).  
c
c         INDX(I,J) contains, for each node J, the index of U, V and P at 
c         that node, or 0 or a negative value.
c 
c         If K=INDX(I,J) is positive, then the value of the degree
c         of freedom is stored in the solution vector entry GFL(K),
c         and an equation will be generated to determine its value.
c
c         If INDX(I,J) is not positive, then no equation is
c         generated to determine for variable I at node J, either because
c         the variable is specified in some other way, or because
c         (in the case of pressure), there is no coefficient associated
c         with that node.
c
c  IQUAD  Input, integer IQUAD, the local index of the quadrature point.
c
c  NELEM  Input, integer NELEM, the number of elements.
c
c  NEQNFL Input, integer NEQNFL, the number of equations in the full system.
c
c  NODE   Input, integer NODE(6,MAXELM).
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
c  NP     Input, integer NP, the number of nodes used to define the finite 
c         element mesh.  NP=(2*NX-1)*(2*NY-1).
c 
c  P      Output, double precision P, the value of the pressure.
c
c  PHIFL  Input, double precision PHIFL(3,6,10,NELEM).  
c
c         PHIFL contains the value of a finite element basis function, its
c         derivative, or other information, evaluated at the quadrature 
c         points.
c 
c         The meaning of the entry PHIFL(I,J,K,L) is as follows.  
c         For the quadrature point I, and basis function J, in element L,
c         PHIFL(I,J,K,L) represents the value of:
c
c           K= 1, W, the finite element basis function for velocities;
c           K= 2, dWdX, the X derivative of W;
c           K= 3, dWdY, the Y derivative of W;
c           K= 4, Q, the finite element basis function for pressures;
c           K= 5, dQdX, the X derivative of Q;
c           K= 6, dQdY, the Y derivative of Q;
c           K= 7, dXsidX, the X derivative of the mapping (X,Y)->XSI;
c           K= 8, dXsidY, the Y derivative of the mapping (X,Y)->XSI;
c           K= 9, dEtadX, the X derivative of the mapping (X,Y)->ETA;
c           K=10, dEtadY, the Y derivative of the mapping (X,Y)->ETA;
c
c         In particular, PHIFL(I,J,K,L) is the value of the quadratic 
c         basis function W associated with local node J in element L, 
c         evaluated at quadrature point I.
c 
c         Note that PHIFL(I,J,K,L)=0 whenever J=4, 5, or 6 and K=4, 5, or 6, 
c         since there are only three linear basis functions.
c
c  U      Output, double precision U, the value of the horizontal 
c         velocity.
c
c  V      Output, double precision V, the value of the vertical 
c         velocity.
c
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer neqnfl
      integer np
c
      double precision coef
      double precision dpdx
      double precision dpdy
      double precision dqdx
      double precision dqdy
      double precision dudx
      double precision dudy
      double precision dvdx
      double precision dvdy
      double precision dwdx
      double precision dwdy
      double precision gfl(neqnfl)
      integer ielem
      integer indx(3,np)
      integer ip
      integer iq
      integer iquad
      integer iun
      integer node(6,nelem)
      double precision p
      double precision phifl(3,6,10,nelem)
      double precision q
      double precision u
      double precision v
      double precision w
c
c  Start all the functions at zero.
c
      p=0.0
      u=0.0
      v=0.0
      dpdx=0.0
      dpdy=0.0
      dudx=0.0
      dudy=0.0
      dvdx=0.0
      dvdy=0.0
c
c  Now each of these functions is represented as the sum of
c  coefficients times basis functions.  In this particular
c  element, at this particular quadrature point, we know that
c  exactly 6 basis functions are nonzero.  So if
c  we simply look up the values of the basis functions (and
c  their X and Y derivatives), and multiply by the appropriate
c  coefficients, we can evaluate the functions.
c
c  W, DWDX and DWDY represent the value of a quadratic basis
c  function and its X and Y derivative.
c
c  Q, DQDX and DQDY represent the value of a linear basis
c  function and its X and Y derivatives.
c
      do iq=1,6
 
        w=phifl(iquad,iq,1,ielem)
        dwdx=phifl(iquad,iq,2,ielem)
        dwdy=phifl(iquad,iq,3,ielem)
 
        q=phifl(iquad,iq,4,ielem)
        dqdx=phifl(iquad,iq,5,ielem)
        dqdy=phifl(iquad,iq,6,ielem)
c
c  Now that we have the basis function values, we need to look
c  up the coefficient COEF that multiplies the basis function.
c
        ip=node(iq,ielem)

        iun=indx(1,ip)
        coef=gfl(iun)
        u=u+coef*w
        dudx=dudx+coef*dwdx
        dudy=dudy+coef*dwdy

        iun=indx(2,ip)
        coef=gfl(iun)
        v=v+coef*w
        dvdx=dvdx+coef*dwdx
        dvdy=dvdy+coef*dwdy

        iun=indx(3,ip)
        if(iun.gt.0)then
          coef=gfl(iun)
          p=p+coef*q
          dpdx=dpdx+coef*dqdx
          dpdy=dpdy+coef*dqdy
        endif
 
      enddo
 
      return
      end
      subroutine uvpqrb(dprbdx,dprbdy,durbdx,durbdy,dvrbdx,dvrbdy,grb,
     &  ielem,iquad,nelem,neqnrb,prb,phirb,urb,vrb)
c
c***********************************************************************
c
cc UVPQRB is given:
c
c    GRB, the reduced basis coefficients,
c    PHIRB, the reduced basis functions and derivatives evaluated 
c      at the quadrature points,
c    IELEM, a particular element, and
c    IQUAD, a particular quadrature point in that element.
c
c  UVPQRB computes:
c
c    URB, VRB, PRB, the state variables evaluated at that quadrature point,
c    the X and Y derivatives of these quantities.
c
c  This routine was modified on 09 July 1996.
c
c
c  DPRBDX,
c  DPRBDY Output, double precision DPRBDX, DPRBDY, the derivatives of the
c         pressure function with respect to X and Y.
c
c  DURBDX,
c  DURBDY Output, double precision DURBDX, DURBDY, the derivatives of the
c         horizontal velocity function with respect to X and Y.
c
c  DVRBDX,
c  DVRBDY Output, double precision DVRBDX, DVRBDY, the derivatives of the
c         vertical velocity function with respect to X and Y.
c
c  GRB    Input, double precision GRB(0:NEQNRB), the current solution 
c         estimate for the reduced problem.
c
c  IELEM  Input, integer IELEM, the element in which the quadrature
c         point lies.
c
c  IQUAD  Input, integer IQUAD, the local index of the quadrature point.
c
c  NELEM  Input, integer NELEM, the number of elements.
c
c  NEQNRB Input, integer NEQNRB, the number of equations in the reduced
c         system.
c
c  PRB    Output, double precision PRB, the value of the pressure.
c
c  PHIRB  Input, double precision PHIRB(3,0:NEQNRB,15,NELEM).
c         PHIRB contains the values of a finite element basis function
c         or its X or Y derivative, in a given element, at a given
c         quadrature point, for a particular reduced basis function.
c
c         For PHIRB(I,J,K,L), index J refers to the reduced basis
c         basis functions, for J=0 to NEQNRB.
c
c         The meaning of the K index of PHIRB(I,J,K,L) is as follows:
c
c           For the quadrature point I, and reduced basis function J,
c           in element L, PHIRB(I,J,K,L) represents the value of:
c
c             K=1, WUrb, the finite element U velocity basis function;
c             K=2, dWUrbdX, the X derivative of WUrb;
c             K=3, dWUrbdY, the Y derivative of WUrb;
c             K=4, WVrb, the finite element V velocity basis function;
c             K=5, dWVrbdX, the X derivative of WVrb;
c             K=6, dWVrbdY, the Y derivative of WVrb;
c             K=7, Q, the finite element pressure basis function.
c             K=8, dQrbdX, the X derivative of Qrb;
c             K=9, dQrbdY, the Y derivative of Qrb.
c             K=10, WU0rb, same as WUrb, with zero BC.
c             K=11, dWU0rbdX, same as dWUrbdX, with zero BC.
c             K=12, dWU0rbdY, same as dWUrbdY, with zero BC.
c             K=13, WV0rb, same as WVrb, with zero BC.
c             K=14, dWV0rbdX, same as dWVrbdX, with zero BC.
c             K=15, dWV0rbdY, same as dWVrbdY, with zero BC.
c
c  URB    Output, double precision URB, the value of the horizontal 
c         velocity.
c
c  VRB    Output, double precision VRB, the value of the vertical 
c         velocity.
c
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer neqnrb
c
      double precision coef
      double precision dprbdx
      double precision dprbdy
      double precision dqrbdx
      double precision dqrbdy
      double precision durbdx
      double precision durbdy
      double precision dvrbdx
      double precision dvrbdy
      double precision dwurbdx
      double precision dwurbdy
      double precision dwvrbdx
      double precision dwvrbdy
      double precision grb(0:neqnrb)
      integer ielem
      integer ieqnrb
      integer iquad
      double precision prb
      double precision phirb(3,0:neqnrb,15,nelem)
      double precision qrb
      double precision urb
      double precision vrb
      double precision wurb
      double precision wvrb
c
c  Start all the function values at 0.
c
      urb=0.0
      durbdx=0.0
      durbdy=0.0
 
      vrb=0.0
      dvrbdx=0.0
      dvrbdy=0.0
 
      prb=0.0
      dprbdx=0.0
      dprbdy=0.0
c
c  Now each of these functions is represented as the sum of
c  coefficients times the reduced basis vectors.  So if
c  we simply look up the values of the reduced basis functions (and
c  their X and Y derivatives), and multiply by the appropriate
c  coefficients, we can evaluate the functions.
c
c  WURB, DWURBDX and DWURBDY represent the value of a quadratic basis
c  function and its X and Y derivative.
c
c  WVRB, DWVRBDX and DWVRBDY represent the value of a quadratic basis
c  function and its X and Y derivative.
c
c  QRB, DQRBDX and DQRBDY represent the value of a linear basis
c  function and its X and Y derivatives.
c
c  See routine SETPRB, where these values are loaded into PHIRB.
c
      do ieqnrb=0,neqnrb
 
        wurb   =phirb(iquad,ieqnrb,1,ielem)
        dwurbdx=phirb(iquad,ieqnrb,2,ielem)
        dwurbdy=phirb(iquad,ieqnrb,3,ielem)

        wvrb   =phirb(iquad,ieqnrb,4,ielem)
        dwvrbdx=phirb(iquad,ieqnrb,5,ielem)
        dwvrbdy=phirb(iquad,ieqnrb,6,ielem)

        qrb   =phirb(iquad,ieqnrb,7,ielem)
        dqrbdx=phirb(iquad,ieqnrb,8,ielem)
        dqrbdy=phirb(iquad,ieqnrb,9,ielem)

        coef=grb(ieqnrb)

        urb   =urb+coef*wurb
        durbdx=durbdx+coef*dwurbdx
        durbdy=durbdy+coef*dwurbdy

        vrb   =vrb+coef*wvrb
        dvrbdx=dvrbdx+coef*dwvrbdx
        dvrbdy=dvrbdy+coef*dwvrbdy

        prb   =prb+coef*qrb
        dprbdx=dprbdx+coef*dqrbdx
        dprbdy=dprbdy+coef*dqrbdy
 
      enddo
 
      return
      end
      subroutine wrdis(disfil,eqn,igunit,indx,isotri,maxnfl,nelem,
     &  neqnfl,neqnrb,node,np,npar,npe,nprof,nx,ny,p,par,senfl,
     &  u,v,xc,xprof,yc)
c
c***********************************************************************
c
cc WRDIS writes information to a file which can be used to create
c  graphics images.  
c
      implicit double precision (a-h,o-z)
c
      integer maxnfl
      integer nelem
      integer neqnfl
      integer neqnrb
      integer np
      integer npar
      integer ny
c
      character*2 ctemp
      character*30 disfil
      character*2 eqn(neqnfl)
      integer i
      integer icheck
      integer igunit
      integer ihor
      integer indx(3,np)
      integer iprs
      integer isen
      integer iset
      integer isotri(nelem)
      integer iver
      integer j
      integer node(6,nelem)
      integer npe
      integer nprof(2*ny-1)
      integer nsen
      integer nx
      double precision p(np)
      double precision par(npar)
      real rtemp
      double precision senfl(maxnfl,0:neqnrb)
      double precision u(np)
      double precision v(np)
      double precision xc(np)
      double precision xprof
      double precision yc(np)
c
      save iset
c
      data iset /0/
c
      if(igunit.eq.0)then
 
        write(*,*)' '
        write(*,*)'PltWrt - Note:'
        write(*,*)'  Opening the DISPLAY plot file '//disfil
        write(*,*)' '
c
c  Delete any old copy of the file.
c
        igunit=11
        open(unit=igunit,file=disfil,status='unknown',
     &    form='formatted',access='sequential')
 
      endif

      iset=iset+1
 
      nsen=neqnrb
c
c  Number of elements, nodes, parameters, 
c  elements in the X direction, elements in the Y direction.
c
      write(igunit,*)nelem
      write(igunit,*)np
      write(igunit,*)npar
      write(igunit,*)npe
      write(igunit,*)nsen
      write(igunit,*)nx
      write(igunit,*)ny
c
c  Pressures, P.
c
      do i=1,np
        write(igunit,*)p(i)
      enddo
c
c  Horizontal velocities, U.
c
      do i=1,np
        write(igunit,*)u(i)
      enddo
c
c  Vertical velocities, V
c
      do i=1,np
        write(igunit,*)v(i)
      enddo
c
c  Indicator of element type (isoparametric or not).
c
      do i=1,nelem
        write(igunit,*)isotri(i)
      enddo
c
c  Nodes that make up each element.
c
      do i=1,npe
        do j=1,nelem
          write(igunit,*)node(i,j)
        enddo
      enddo
c
c  Indices of the nodes along the profile line.
c
      do i=1,2*ny-1
        write(igunit,*)nprof(i)
      enddo
c
c  Parameters.
c
      do i=1,npar
        write(igunit,*)par(i)
      enddo
c
c  Pressure sensitivities, dP/dpar
c
      do isen=1,nsen

        do i=1,np
          iprs=indx(3,i)
          rtemp=0.0
          if(iprs.gt.0)then
            rtemp=sngl(senfl(iprs,isen))
          endif
          write(igunit,*)rtemp
        enddo
c
c  Horizontal velocity sensitivities, dU/dpar
c
        do i=1,np
          ihor=indx(1,i)
          write(igunit,*)senfl(ihor,isen)
        enddo
c
c  Vertical velocity sensitivities, dV/dpar
c
        do i=1,np
          iver=indx(2,i)
          write(igunit,*)senfl(iver,isen)
        enddo

      enddo
c
c  X coordinates of nodes.
c
      do i=1,np
        write(igunit,*)xc(i)
      enddo
c
c  X coordinate of profile line.
c
      write(igunit,*)xprof
c
c  Y coordinates of nodes.
c
      do i=1,np
        write(igunit,*)yc(i)
      enddo
c
c  Nodal equation types.
c
      do i=1,np
        ihor=indx(1,i)
        iver=indx(2,i)
        iprs=indx(3,i)
        if(iprs.le.0)then
          ctemp='  '
        else
          ctemp=eqn(iprs)
        endif
        write(igunit,'(3a2)')eqn(ihor),eqn(iver),ctemp
      enddo
c
c  Write a check at the the end.
c
      icheck=1953
      write(igunit,*)icheck
 
      write(*,*)' '
      write(*,*)'PLTWRT wrote data set ',iset,' to file.'
 
      return
      end
      subroutine wrtec(nelem,node,np,p,tecfil,u,v,xc,yc)

c*********************************************************************72
c
cc WRTEC writes out information which can be used for with the
c  TECPLOT graphics program.
c
c
c  NELEM  Input, integer NELEM, the number of elements.
c 
c  NODE   Input, integer NODE(6,MAXELM) or NODE(6,NELEM).
c
c         NODE(I,J) contains, for an element J, the global index of 
c         the node whose local number in J is I.
c
c         The local ordering of the nodes is suggested by this diagram:
c
c           Global nodes   Elements      NODE
c                                                          1  2  3  4  5  6
c           74  84  94     3-6-1   2     Left element =  (94,72,74,83,73,84)
c                          |  /   /|
c           73  83  93     5 4   4 5     Right element = (72,94,92,83,93,82)
c                          |/   /  |
c           72  82  92     2   1-6-3
c 
c  NP     Input, INTEGER NP, the number of nodes.
c
c  P      Input, REAL P(NP), the pressure.
c
c  U      Input, REAL U(NP), the horizontal velocity.
c
c  V      Input, REAL V(NP), the vertical velocity.
c
c  XC     Input, REAL XC(NP), the X coordinates of the nodes.
c
c  YC     Input, REAL YC(NP), the Y coordinates of the nodes.
c
      implicit double precision (a-h,o-z)
c
      integer nelem
      integer np
c
      integer i
      integer node(6,nelem)
      double precision p(np)
      character*30 tecfil
      double precision u(np)
      double precision v(np)
      double precision xc(np)
      double precision yc(np)
c
c  Delete any old copy of the TECPLOT data file.
c
      open(unit=10,file=tecfil,status='old',err=10)
      write(*,*)' '
      write(*,*)'WRTEC - Note:'
      write(*,*)'  Deleting an old copy of the TECPLOT data file.'
      close(unit=10,status='delete')

10    continue

      open(unit=10,file=tecfil,status='unknown')

      write(10,*)'Title=','"ARBY data"'
      write(10,*)'Variables="X","Y","P","U","V"'
      write(10,*)'Zone N=',np,', E=',4*nelem,', F=FEPOINT, ET=TRIANGLE'
c
c  Write out the data at each node.
c
      do i=1,np
        write(10,'(5g15.6)')xc(i),yc(i),p(i),u(i),v(i)
      enddo
c
c  Write out the data that defines the elements.
c  Each 6 node quadratic element must be described as 4 linear elements.
c
      do i=1,nelem
        write(10,'(3i6)')node(1,i),node(4,i),node(6,i)
        write(10,'(3i6)')node(2,i),node(5,i),node(4,i)
        write(10,'(3i6)')node(3,i),node(6,i),node(5,i)
        write(10,'(3i6)')node(4,i),node(5,i),node(6,i)
      enddo

      close(unit=10)

      return
      end
      subroutine xofxsi(eta,ielem,nelem,node,np,x,xc,xsi,y,yc)
c
c***********************************************************************
c
cc XOFXSI is given the XSI, ETA coordinates of a point in an
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
c  NELEM  Input, integer NELEM, the number of elements.
c
c  NODE   Input, integer NODE(6,nelem), contains the numbers
c         of the nodes that make up each element.  Element number
c         I is associated with nodes NODE(1,I) through NODE(6,I).
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
      implicit none

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
!
!  Pick off the X, Y coordinates of the nodes and store them
!  in two short lists.
!
      do i = 1,6
        xn(i) = xc(node(i,ielem))
        yn(i) = yc(node(i,ielem))
      end do
!
!  Set the coefficients in the transformation
!
!    (XSI,ETA) --> (X,Y).
!
!  The mapping has the form:
!
!    X(ETA,XSI) = A1 * XSI**2 + B1 * XSI*ETA + C1 * ETA**2
!               + D1 * XSI    + E1 * ETA     + F1
!
!    Y(ETA,XSI) = A2 * XSI**2 + B2 * XSI*ETA + C2 * ETA**2
!               + D2 * XSI    + E2 * ETA     + F2
!
      a1 =  2.0D+00 *xn(1)+2.0D+00 *xn(3)-4.0D+00 *xn(6)
      b1 = -4.0D+00 *xn(3)-4.0D+00 *xn(4)+4.0D+00 *xn(5)+4.0D+00 *xn(6)
      c1 =  2.0D+00 *xn(2)+2.0D+00 *xn(3)-4.0D+00 *xn(5)
      d1 = -3.0D+00 *xn(1)      -xn(3)+4.0D+00 *xn(6)
      e1 =       -xn(2)      +xn(3)+4.0D+00 *xn(4)-4.0D+00 *xn(6)
      f1 =        xn(1)

      a2 =  2.0D+00 *yn(1)+2.0D+00 *yn(3)-4.0D+00 *yn(6)
      b2 = -4.0D+00 *yn(3)-4.0D+00 *yn(4)+4.0D+00 *yn(5)+4.0D+00 *yn(6)
      c2 =  2.0D+00 *yn(2)+2.0D+00 *yn(3)-4.0D+00 *yn(5)
      d2 = -3.0D+00 *yn(1)      -yn(3)+4.0D+00 *yn(6)
      e2 =       -yn(2)      +yn(3)+4.0D+00 *yn(4)-4.0D+00 *yn(6)
      f2 =        yn(1)

      x = a1*xsi**2 + b1*xsi*eta + c1*eta**2 + d1*xsi + e1*eta + f1

      y = a2*xsi**2 + b2*xsi*eta + c2*eta**2 + d2*xsi + e2*eta + f2

      return
      end
      function dasum(n,dx,incx)
c
c***********************************************************************
c
cc DASUM takes the sum of the absolute values of the entries of 
c  a vector.
c
      double precision dasum
      double precision dtemp
      double precision dx(*)
      integer i
      integer incx
      integer m
      integer n
c
      if(n.le.0)then
        dasum=0.0d0
        return
      endif

      if(incx.le.0)then

        dasum=0.0d0
        return

      elseif(incx.ne.1)then

        dtemp=0.0d0
        do i=1,n*incx,incx
          dtemp=dtemp+abs(dx(i))
        enddo

      else

        m=mod(n,6)

        dtemp=0.0d0
        do i=1,m
          dtemp=dtemp+abs(dx(i))
        enddo

        do i=m+1,n,6
          dtemp=dtemp+abs(dx(i))+abs(dx(i+1))+abs(dx(i+2))
     &    +abs(dx(i+3))+abs(dx(i+4))+abs(dx(i+5))
        enddo

      endif

      dasum = dtemp

      return
      end
      subroutine daxpy(n,da,dx,incx,dy,incy)
c
c***********************************************************************
c
cc DAXPY adds a multiple of one vector to another.
c
      integer i
      integer incx
      integer incy
      integer ix
      integer iy
      integer m
      integer n
      double precision da
      double precision dx(*)
      double precision dy(*)
c
      if(n.le.0)return

      if (da.eq.0.0)return

      if(incx.eq.1.and.incy.eq.1)go to 20

      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1

      do i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
      enddo

      return

   20 m = mod(n,4)

      do i = 1,m
        dy(i) = dy(i) + da*dx(i)
      enddo

      do i = m+1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
      enddo

      return
      end
      subroutine dcopy(n,dx,incx,dy,incy)
c 
c***********************************************************************
c
cc DCOPY copies a vector X to a vector Y.
c
c
c  N      Input, integer N, the number of entries to copy.
c
c  DX     Input, double precision DX(*), the vector to be copied.
c
c  INCX   Input, integer INCX, the increment between successive
c         entries of DX.
c
c  DY     Output, double precision DY(*), the vector to be copied.
c
c  INCY   Input, integer INCY, the increment between successive
c         entries of DY.
c
      implicit double precision (a-h,o-z)
c
      double precision dx(*)
      double precision dy(*)
      integer i
      integer incx
      integer incy
      integer ix
      integer iy
      integer m
      integer n
c
      if(n.le.0)return

      if(incx.eq.1.and.incy.eq.1)then

        m = mod(n,7)

        do i = 1,m
          dy(i) = dx(i)
        enddo

        do i = m+1,n,7
          dy(i) = dx(i)
          dy(i + 1) = dx(i + 1)
          dy(i + 2) = dx(i + 2)
          dy(i + 3) = dx(i + 3)
          dy(i + 4) = dx(i + 4)
          dy(i + 5) = dx(i + 5)
          dy(i + 6) = dx(i + 6)
        enddo

      else

        if(incx.lt.0)then
          ix = (-n+1)*incx + 1
        else
          ix=1
        endif

        if(incy.lt.0)then
          iy = (-n+1)*incy + 1
        else
          iy=1
        endif

        do i = 1,n
          dy(iy) = dx(ix)
          ix = ix + incx
          iy = iy + incy
        enddo

      endif

      return
      end
      function ddot(n,dx,incx,dy,incy)
c
c***********************************************************************
c
cc DDOT forms the dot product of two vectors.
c
      double precision ddot
      double precision dtemp
      double precision dx(*)
      double precision dy(*)
      integer i
      integer incx
      integer incy
      integer ix
      integer iy
      integer m
      integer n
c
      ddot = 0.0d0
      dtemp = 0.0d0

      if(n.le.0)return

      if(incx.eq.1.and.incy.eq.1)go to 20

      if(incx.lt.0)then
        ix = (-n+1)*incx + 1
      else
        ix=1
      endif

      if(incy.lt.0)then
        iy = (-n+1)*incy + 1
      else
        iy=1
      endif

      do i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
      enddo

      ddot = dtemp
      return
c
   20 m = mod(n,5)

      do i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
      enddo

      do i = m+1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
      enddo

      ddot = dtemp

      return
      end
      subroutine dgbtf2(m,n,kl,ku,ab,ldab,ipiv,info)
c
c***********************************************************************
c
c  -- LAPACK routine (version 1.0) --
c     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
c     Courant Institute, Argonne National Lab, and Rice University
c     February 29, 1992
c
c     .. Scalar Arguments ..
      integer            info, kl, ku, ldab, m, n
c     ..
c     .. Array Arguments ..
      integer            ipiv( * )
      double precision   ab( ldab, * )
c
cc DGBTF2 computes an LU factorization of a real m-by-n band matrix A
c  using partial pivoting with row interchanges.
c
c  This is the unblocked version of the algorithm, calling Level 2 BLAS.
c
c  Arguments
c  =========
c
c  M       (input) integer
c          The number of rows of the matrix A.  M >= 0.
c
c  N       (input) integer
c          The number of columns of the matrix A.  N >= 0.
c
c  KL      (input) integer
c          The number of subdiagonals within the band of A.  KL >= 0.
c
c  KU      (input) integer
c          The number of superdiagonals within the band of A.  KU >= 0.
c
c  AB      (input/output) double precision array, dimension (LDAB,N)
c          On entry, the matrix A in band storage, in rows KL+1 to
c          2*KL+KU+1; rows 1 to KL of the array need not be set.
c          The j-th column of A is stored in the j-th column of the
c          array AB as follows:
c          AB(kl+ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(m,j+kl)
c
c          On exit, details of the factorization: U is stored as an
c          upper triangular band matrix with KL+KU superdiagonals in
c          rows 1 to KL+KU+1, and the multipliers used during the
c          factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
c          See below for further details.
c
c  LDAB    (input) integer
c          The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
c
c  IPIV    (output) integer array, dimension (min(M,N))
c          The pivot indices; for 1 <= i <= min(M,N), row i of the
c          matrix was interchanged with row IPIV(i).
c
c  INFO    (output) integer
c          = 0: successful exit
c          < 0: if INFO = -i, the i-th argument had an illegal value
c          > 0: if INFO = +i, U(i,i) is exactly zero. The factorization
c               has been completed, but the factor U is exactly
c               singular, and division by zero will occur if it is used
c               to solve a system of equations.
c
c  Further Details
c  ===============
c
c  The band storage scheme is illustrated by the following example, when
c  M = N = 6, KL = 2, KU = 1:
c
c  On entry:                       On exit:
c
c      *    *    *    +    +    +       *    *    *   u14  u25  u36
c      *    *    +    +    +    +       *    *   u13  u24  u35  u46
c      *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
c     a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
c     a21  a32  a43  a54  a65   *      m21  m32  m43  m54  m65   *
c     a31  a42  a53  a64   *    *      m31  m42  m53  m64   *    *
c
c  Array elements marked * are not used by the routine; elements marked
c  + need not be set on entry, but are required by the routine to store
c  elements of U, because of fill-in resulting from the row
c  interchanges.
c
c  =====================================================================
c
c     .. Parameters ..
      double precision   one, zero
      parameter          ( one = 1.0d+0, zero = 0.0d+0 )
c     ..
c     .. Local Scalars ..
      integer            i, j, jp, ju, km, kv
c     ..
c     .. External Functions ..
      integer            idamax
      external           idamax
c     ..
c     .. External Subroutines ..
      external           dger, dscal, dswap, xerbla
c     ..
c     .. Intrinsic Functions ..
      intrinsic          max, min
c     ..
c     .. Executable Statements ..
c
c     KV is the number of superdiagonals in the factor U, allowing for
c     fill-in.
c
      kv = ku + kl
c
c     Test the input parameters.
c
      info = 0
      if( m.lt.0 ) then
         info = -1
      else if( n.lt.0 ) then
         info = -2
      else if( kl.lt.0 ) then
         info = -3
      else if( ku.lt.0 ) then
         info = -4
      else if( ldab.lt.kl+kv+1 ) then
         info = -6
      end if
      if( info.ne.0 ) then
         call xerbla( 'dgbtf2', -info )
         return
      end if
c
c     Quick return if possible
c
      if( m.eq.0 .or. n.eq.0 )
     $   return
c
c     Gaussian elimination with partial pivoting
c
c     Set fill-in elements in columns KU+2 to KV to zero.
c
      do j = ku + 2, min( kv, n )
        do i = kv - j + 2, kl
          ab( i, j ) = zero
        enddo
      enddo
c
c     JU is the index of the last column affected by the current stage
c     of the factorization.
c
      ju = 1
c
      do j = 1, min( m, n )
c
c        Set fill-in elements in column J+KV to zero.
c
         if( j+kv.le.n ) then
            do i = 1, kl
               ab( i, j+kv ) = zero
            enddo
         end if
c
c        Find pivot and test for singularity. KM is the number of
c        subdiagonal elements in the current column.
c
         km = min( kl, m-j )
         jp = idamax( km+1, ab( kv+1, j ), 1 )
         ipiv( j ) = jp + j - 1
         if( ab( kv+jp, j ).ne.zero ) then
            ju = max( ju, min( j+ku+jp-1, n ) )
c
c  Apply interchange to columns J to JU.
c
            if( jp.ne.1 )
     $         call dswap( ju-j+1, ab( kv+jp, j ), ldab-1,
     $                     ab( kv+1, j ), ldab-1 )

            if( km.gt.0 ) then
c
c  Compute multipliers.
c
               call dscal( km, one / ab( kv+1, j ), ab( kv+2, j ), 1 )
c
c  Update trailing submatrix within the band.
c
               if( ju.gt.j )then

                  call dger( km, ju-j, -one, ab( kv+2, j ), 1,
     $                       ab( kv, j+1 ), ldab-1, ab( kv+1, j+1 ),
     $                       ldab-1 )
               endif

            end if
         else
c
c  If pivot is zero, set INFO to the index of the pivot
c  unless a zero pivot has already been found.
c
            if( info.eq.0 )then
               info = j
            endif

         end if

      enddo

      return
      end
      subroutine dgbtrf(m,n,kl,ku,ab,ldab,ipiv,info)
c
c***********************************************************************
c
c  -- LAPACK routine (version 1.0) --
c     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
c     Courant Institute, Argonne National Lab, and Rice University
c     February 29, 1992
c
c     .. Scalar Arguments ..
      integer            info, kl, ku, ldab, m, n
c     ..
c     .. Array Arguments ..
      integer            ipiv( * )
      double precision   ab( ldab, * )
c
cc DGBTRF computes an LU factorization of a real m-by-n band matrix A
c  using partial pivoting with row interchanges.
c
c  This is the blocked version of the algorithm, calling Level 3 BLAS.
c
c  Arguments
c  =========
c
c  M       (input) integer
c          The number of rows of the matrix A.  M >= 0.
c
c  N       (input) integer
c          The number of columns of the matrix A.  N >= 0.
c
c  KL      (input) integer
c          The number of subdiagonals within the band of A.  KL >= 0.
c
c  KU      (input) integer
c          The number of superdiagonals within the band of A.  KU >= 0.
c
c  AB      (input/output) double precision array, dimension (LDAB,N)
c          On entry, the matrix A in band storage, in rows KL+1 to
c          2*KL+KU+1; rows 1 to KL of the array need not be set.
c          The j-th column of A is stored in the j-th column of the
c          array AB as follows:
c          AB(kl+ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(m,j+kl)
c
c          On exit, details of the factorization: U is stored as an
c          upper triangular band matrix with KL+KU superdiagonals in
c          rows 1 to KL+KU+1, and the multipliers used during the
c          factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
c          See below for further details.
c
c  LDAB    (input) integer
c          The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
c
c  IPIV    (output) integer array, dimension (min(M,N))
c          The pivot indices; for 1 <= i <= min(M,N), row i of the
c          matrix was interchanged with row IPIV(i).
c
c  INFO    (output) integer
c          = 0: successful exit
c          < 0: if INFO = -i, the i-th argument had an illegal value
c          > 0: if INFO = +i, U(i,i) is exactly zero. The factorization
c               has been completed, but the factor U is exactly
c               singular, and division by zero will occur if it is used
c               to solve a system of equations.
c
c  Further Details
c  ===============
c
c  The band storage scheme is illustrated by the following example, when
c  M = N = 6, KL = 2, KU = 1:
c
c  On entry:                       On exit:
c
c      *    *    *    +    +    +       *    *    *   u14  u25  u36
c      *    *    +    +    +    +       *    *   u13  u24  u35  u46
c      *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
c     a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
c     a21  a32  a43  a54  a65   *      m21  m32  m43  m54  m65   *
c     a31  a42  a53  a64   *    *      m31  m42  m53  m64   *    *
c
c  Array elements marked * are not used by the routine; elements marked
c  + need not be set on entry, but are required by the routine to store
c  elements of U because of fill-in resulting from the row interchanges.
c
c  =====================================================================
c
c     .. Parameters ..
      double precision   one, zero
      parameter          ( one = 1.0d+0, zero = 0.0d+0 )
      integer            nbmax, ldwork
      parameter          ( nbmax = 64, ldwork = nbmax+1 )
c     ..
c     .. Local Scalars ..
      integer            i, i2, i3, ii, ip, j, j2, j3, jb, jj, jm, jp,
     $                   ju, k2, km, kv, nb, nw
      double precision   temp
c     ..
c     .. Local Arrays ..
      double precision   work13( ldwork, nbmax ),
     $                   work31( ldwork, nbmax )
c     ..
c     .. External Functions ..
      integer            idamax, ilaenv
      external           idamax, ilaenv
c     ..
c     .. External Subroutines ..
      external           dcopy, dgbtf2, dgemm, dger, dlaswp, dscal,
     $                   dswap, dtrsm, xerbla
c     ..
c     .. Intrinsic Functions ..
      intrinsic          max, min
c     ..
c     .. Executable Statements ..
c
c     KV is the number of superdiagonals in the factor U, allowing for
c     fill-in
c
      kv = ku + kl
c
c     Test the input parameters.
c
      info = 0
      if( m.lt.0 ) then
         info = -1
      else if( n.lt.0 ) then
         info = -2
      else if( kl.lt.0 ) then
         info = -3
      else if( ku.lt.0 ) then
         info = -4
      else if( ldab.lt.kl+kv+1 ) then
         info = -6
      end if
      if( info.ne.0 ) then
         call xerbla( 'dgbtrf', -info )
         return
      end if
c
c  Quick return if possible
c
      if( m.eq.0 .or. n.eq.0 )then
        return
      endif
c
c  Determine the block size for this environment
c
      nb = ilaenv( 1, 'dgbtrf', ' ', m, n, kl, ku )
c
c  The block size must not exceed the limit set by the size of the
c  local arrays WORK13 and WORK31.
c
      nb = min( nb, nbmax )

      if( nb.le.1 .or. nb.gt.kl ) then
c
c  Use unblocked code
c
         call dgbtf2( m, n, kl, ku, ab, ldab, ipiv, info )
      else
c
c        Use blocked code
c
c        Zero the superdiagonal elements of the work array WORK13
c
         do j = 1, nb
            do i = 1, j - 1
               work13( i, j ) = zero
            enddo
         enddo
c
c        Zero the subdiagonal elements of the work array WORK31
c
         do j = 1, nb
            do i = j + 1, nb
               work31( i, j ) = zero
            enddo
         enddo
c
c        Gaussian elimination with partial pivoting
c
c        Set fill-in elements in columns KU+2 to KV to zero
c
         do j = ku + 2, min( kv, n )
            do i = kv - j + 2, kl
               ab( i, j ) = zero
            enddo
         enddo
c
c        JU is the index of the last column affected by the current
c        stage of the factorization
c
         ju = 1
c
         do 180 j = 1, min( m, n ), nb
            jb = min( nb, min( m, n )-j+1 )
c
c           The active part of the matrix is partitioned
c
c              A11   A12   A13
c              A21   A22   A23
c              A31   A32   A33
c
c           Here A11, A21 and A31 denote the current block of JB columns
c           which is about to be factorized. The number of rows in the
c           partitioning are JB, I2, I3 respectively, and the numbers
c           of columns are JB, J2, J3. The superdiagonal elements of A13
c           and the subdiagonal elements of A31 lie outside the band.
c
            i2 = min( kl-jb, m-j-jb+1 )
            i3 = min( jb, m-j-kl+1 )
c
c           J2 and J3 are computed after JU has been updated.
c
c           Factorize the current block of JB columns
c
            do 80 jj = j, j + jb - 1
c
c              Set fill-in elements in column JJ+KV to zero
c
               if( jj+kv.le.n ) then
                  do i = 1, kl
                     ab( i, jj+kv ) = zero
                  enddo
               end if
c
c              Find pivot and test for singularity. KM is the number of
c              subdiagonal elements in the current column.
c
               km = min( kl, m-jj )
               jp = idamax( km+1, ab( kv+1, jj ), 1 )
               ipiv( jj ) = jp + jj - j
               if( ab( kv+jp, jj ).ne.zero ) then
                  ju = max( ju, min( jj+ku+jp-1, n ) )
                  if( jp.ne.1 ) then
c
c                    Apply interchange to columns J to J+JB-1
c
                     if( jp+jj-1.lt.j+kl ) then
c
                        call dswap( jb, ab( kv+1+jj-j, j ), ldab-1,
     $                              ab( kv+jp+jj-j, j ), ldab-1 )
                     else
c
c                       The interchange affects columns J to JJ-1 of A31
c                       which are stored in the work array WORK31
c
                        call dswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,
     $                              work31( jp+jj-j-kl, 1 ), ldwork )
                        call dswap( j+jb-jj, ab( kv+1, jj ), ldab-1,
     $                              ab( kv+jp, jj ), ldab-1 )
                     end if
                  end if
c
c                 Compute multipliers
c
                  call dscal( km, one / ab( kv+1, jj ), ab( kv+2, jj ),
     $                        1 )
c
c  Update trailing submatrix within the band and within
c                 the current block. JM is the index of the last column
c                 which needs to be updated.
c
                  jm = min( ju, j+jb-1 )

                  if( jm.gt.jj )then
                     call dger( km, jm-jj, -one, ab( kv+2, jj ), 1,
     $                          ab( kv, jj+1 ), ldab-1,
     $                          ab( kv+1, jj+1 ), ldab-1 )
                  endif

               else
c
c                 If pivot is zero, set INFO to the index of the pivot
c                 unless a zero pivot has already been found.
c
                  if( info.eq.0 )then
                     info = jj
                  endif

               end if
c
c              Copy current column of A31 into the work array WORK31
c
               nw = min( jj-j+1, i3 )
               if( nw.gt.0 )
     $            call dcopy( nw, ab( kv+kl+1-jj+j, jj ), 1,
     $                        work31( 1, jj-j+1 ), 1 )
   80       continue

            if( j+jb.le.n ) then
c
c              Apply the row interchanges to the other blocks.
c
               j2 = min( ju-j+1, kv ) - jb
               j3 = max( 0, ju-j-kv+1 )
c
c              Use DLASWP to apply the row interchanges to A12, A22, and
c              A32.
c
               call dlaswp( j2, ab( kv+1-jb, j+jb ), ldab-1, 1, jb,
     $                      ipiv( j ), 1 )
c
c              Adjust the pivot indices.
c
               do 90 i = j, j + jb - 1
                  ipiv( i ) = ipiv( i ) + j - 1
   90          continue
c
c              Apply the row interchanges to A13, A23, and A33
c              columnwise.
c
               k2 = j - 1 + jb + j2
               do 110 i = 1, j3
                  jj = k2 + i
                  do 100 ii = j + i - 1, j + jb - 1
                     ip = ipiv( ii )
                     if( ip.ne.ii ) then
                        temp = ab( kv+1+ii-jj, jj )
                        ab( kv+1+ii-jj, jj ) = ab( kv+1+ip-jj, jj )
                        ab( kv+1+ip-jj, jj ) = temp
                     end if
  100             continue
  110          continue
c
c              Update the relevant part of the trailing submatrix
c
               if( j2.gt.0 ) then
c
c                 Update A12
c
                  call dtrsm( 'left', 'lower', 'no transpose', 'unit',
     $                        jb, j2, one, ab( kv+1, j ), ldab-1,
     $                        ab( kv+1-jb, j+jb ), ldab-1 )
c
                  if( i2.gt.0 ) then
c
c                    Update A22
c
                     call dgemm( 'no transpose', 'no transpose', i2, j2,
     $                           jb, -one, ab( kv+1+jb, j ), ldab-1,
     $                           ab( kv+1-jb, j+jb ), ldab-1, one,
     $                           ab( kv+1, j+jb ), ldab-1 )
                  end if
c
                  if( i3.gt.0 ) then
c
c                    Update A32
c
                     call dgemm( 'no transpose', 'no transpose', i3, j2,
     $                           jb, -one, work31, ldwork,
     $                           ab( kv+1-jb, j+jb ), ldab-1, one,
     $                           ab( kv+kl+1-jb, j+jb ), ldab-1 )
                  end if
               end if
c
               if( j3.gt.0 ) then
c
c                 Copy the lower triangle of A13 into the work array
c                 WORK13
c
                  do 130 jj = 1, j3
                     do 120 ii = jj, jb
                        work13( ii, jj ) = ab( ii-jj+1, jj+j+kv-1 )
  120                continue
  130             continue
c
c                 Update A13 in the work array
c
                  call dtrsm( 'left', 'lower', 'no transpose', 'unit',
     $                        jb, j3, one, ab( kv+1, j ), ldab-1,
     $                        work13, ldwork )
c
                  if( i2.gt.0 ) then
c
c                    Update A23
c
                     call dgemm( 'no transpose', 'no transpose', i2, j3,
     $                           jb, -one, ab( kv+1+jb, j ), ldab-1,
     $                           work13, ldwork, one, ab( 1+jb, j+kv ),
     $                           ldab-1 )
                  end if
c
                  if( i3.gt.0 ) then
c
c                    Update A33
c
                     call dgemm( 'no transpose', 'no transpose', i3, j3,
     $                           jb, -one, work31, ldwork, work13,
     $                           ldwork, one, ab( 1+kl, j+kv ), ldab-1 )
                  end if
c
c                 Copy the lower triangle of A13 back into place
c
                  do 150 jj = 1, j3
                     do 140 ii = jj, jb
                        ab( ii-jj+1, jj+j+kv-1 ) = work13( ii, jj )
  140                continue
  150             continue
               end if
            else
c
c              Adjust the pivot indices.
c
               do 160 i = j, j + jb - 1
                  ipiv( i ) = ipiv( i ) + j - 1
  160          continue
            end if
c
c           Partially undo the interchanges in the current block to
c           restore the upper triangular form of A31 and copy the upper
c           triangle of A31 back into place
c
            do 170 jj = j + jb - 1, j, -1
               jp = ipiv( jj ) - jj + 1
               if( jp.ne.1 ) then
c
c                 Apply interchange to columns J to JJ-1
c
                  if( jp+jj-1.lt.j+kl ) then
c
c                    The interchange does not affect A31
c
                     call dswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,
     $                           ab( kv+jp+jj-j, j ), ldab-1 )
                  else
c
c  The interchange does affect A31
c
                     call dswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,
     $                           work31( jp+jj-j-kl, 1 ), ldwork )
                  end if
               end if
c
c  Copy the current column of A31 back into place
c
               nw = min( i3, jj-j+1 )

               if( nw.gt.0)then
                  call dcopy( nw, work31( 1, jj-j+1 ), 1,
     $                        ab( kv+kl+1-jj+j, jj ), 1 )
               endif

  170       continue
  180    continue
      end if

      return
      end
      subroutine dgbtrs(trans,n,kl,ku,nrhs,ab,ldab,ipiv,b,ldb,info)
c
c***********************************************************************
c
c  -- LAPACK routine (version 1.0) --
c     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
c     Courant Institute, Argonne National Lab, and Rice University
c     February 29, 1992
c
c     .. Scalar Arguments ..
      character          trans
      integer            info, kl, ku, ldab, ldb, n, nrhs
c     ..
c     .. Array Arguments ..
      integer            ipiv( * )
      double precision   ab( ldab, * ), b( ldb, * )
c
cc DGBTRS solves a system of linear equations
c     A * X = B  or  A' * X = B
c  with a general band matrix A using the LU factorization computed
c  by DGBTRF.
c
c  Arguments
c  =========
c
c  TRANS   (input) CHARACTER*1
c          Specifies the form of the system of equations.
c          = 'N':  A * X = B  (No transpose)
c          = 'T':  A'* X = B  (Transpose)
c          = 'C':  A'* X = B  (Conjugate transpose = Transpose)
c
c  N       (input) integer
c          The order of the matrix A.  N >= 0.
c
c  KL      (input) integer
c          The number of subdiagonals within the band of A.  KL >= 0.
c
c  KU      (input) integer
c          The number of superdiagonals within the band of A.  KU >= 0.
c
c  NRHS    (input) integer
c          The number of right hand sides, i.e., the number of columns
c          of the matrix B.  NRHS >= 0.
c
c  AB      (input) double precision array, dimension (LDAB,N)
c          Details of the LU factorization of the band matrix A, as
c          computed by DGBTRF.  U is stored as an upper triangular band
c          matrix with KL+KU superdiagonals in rows 1 to KL+KU+1, and
c          the multipliers used during the factorization are stored in
c          rows KL+KU+2 to 2*KL+KU+1.
c
c  LDAB    (input) integer
c          The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
c
c  IPIV    (input) integer array, dimension (N)
c          The pivot indices; for 1 <= i <= N, row i of the matrix was
c          interchanged with row IPIV(i).
c
c  B       (input/output) double precision array, dimension (LDB,NRHS)
c          On entry, the right hand side vectors B for the system of
c          linear equations.
c          On exit, the solution vectors, X.
c
c  LDB     (input) integer
c          The leading dimension of the array B.  LDB >= max(1,N).
c
c  INFO    (output) integer
c          = 0:  successful exit
c          < 0: if INFO = -k, the k-th argument had an illegal value
c
c  =====================================================================
c
c     .. Parameters ..
      double precision   one
      parameter          ( one = 1.0d+0 )
c     ..
c     .. Local Scalars ..
      logical            lnoti, notran
      integer            i, j, kd, l, lm
c     ..
c     .. External Functions ..
      logical            lsame
      external           lsame
c     ..
c     .. External Subroutines ..
      external           dgemv, dger, dswap, dtbsv, xerbla
c     ..
c     .. Intrinsic Functions ..
      intrinsic          max, min
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      info = 0
      notran = lsame( trans, 'n' )
      if( .not.notran .and. .not.lsame( trans, 't' ) .and. .not.
     $    lsame( trans, 'c' ) ) then
         info = -1
      else if( n.lt.0 ) then
         info = -2
      else if( kl.lt.0 ) then
         info = -3
      else if( ku.lt.0 ) then
         info = -4
      else if( nrhs.lt.0 ) then
         info = -5
      else if( ldab.lt.( 2*kl+ku+1 ) ) then
         info = -7
      else if( ldb.lt.max( 1, n ) ) then
         info = -10
      end if
      if( info.ne.0 ) then
         call xerbla( 'dgbtrs', -info )
         return
      end if
c
c  Quick return if possible
c
      if( n.eq.0 .or. nrhs.eq.0 )
     $   return

      kd = ku + kl + 1
      lnoti = kl.gt.0

      if( notran ) then
c
c  Solve  A*X = B.
c
c  Solve L*X = B, overwriting B with X.
c
c  L is represented as a product of permutations and unit lower
c  triangular matrices L = P(1) * L(1) * ... * P(n-1) * L(n-1),
c  where each transformation L(i) is a rank-one modification of
c  the identity matrix.
c
         if( lnoti ) then
            do j = 1, n - 1
               lm = min( kl, n-j )
               l = ipiv( j )

               if( l.ne.j )then
                  call dswap( nrhs, b( l, 1 ), ldb, b( j, 1 ), ldb )
               endif

               call dger( lm, nrhs, -one, ab( kd+1, j ), 1, b( j, 1 ),
     $                    ldb, b( j+1, 1 ), ldb )
            enddo
         end if

         do i = 1, nrhs
c
c  Solve U*X = B, overwriting B with X.
c
            call dtbsv( 'upper', 'no transpose', 'non-unit', n, kl+ku,
     $                  ab, ldab, b( 1, i ), 1 )
         enddo

      else
c
c  Solve A'*X = B.
c
         do i = 1, nrhs
c
c  Solve U'*X = B, overwriting B with X.
c
            call dtbsv( 'upper', 'transpose', 'non-unit', n, kl+ku, ab,
     $                  ldab, b( 1, i ), 1 )
         enddo
c
c   Solve L'*X = B, overwriting B with X.
c
         if( lnoti ) then
            do j = n - 1, 1, -1
               lm = min( kl, n-j )
               call dgemv( 'transpose', lm, nrhs, -one, b( j+1, 1 ),
     $                     ldb, ab( kd+1, j ), 1, one, b( j, 1 ), ldb )
               l = ipiv( j )
               if( l.ne.j )
     $            call dswap( nrhs, b( l, 1 ), ldb, b( j, 1 ), ldb )
            enddo
         end if
      end if

      return
      end
      subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb,
     $                   beta, c, ldc )
c 
c***********************************************************************
c
*     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      integer            M, N, K, LDA, LDB, LDC
      double precision   ALPHA, BETA
*     .. Array Arguments ..
      double precision   A( LDA, * ), B( LDB, * ), C( LDC, * )
c
cc DGEMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X',
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = A'.
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = B'.
*
*           Unchanged on exit.
*
*  M      - integer.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - integer.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - integer.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - double precision.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - double precision array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - integer.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - double precision array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - integer.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - double precision.
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - double precision array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
*
*  LDC    - integer.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      integer            I, INFO, J, L, NROWA, NROWB
      double precision   TEMP
*     .. Parameters ..
      double precision   ONE         , zero
      PARAMETER        ( ONE = 1.0D+0, zero = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     transposed and set  NROWA and  NROWB  as the number of rows
*     and  columns of  A  and the  number of  rows  of  B  respectively.
*
      NOTA  = LSAME( TRANSA, 'N' )
      NOTB  = LSAME( TRANSB, 'N' )
      if( NOTA )then
         NROWA = M
      ELSE
         NROWA = K
      endif
      if( NOTB )then
         NROWB = K
      ELSE
         NROWB = N
      endif
*
*     Test the input parameters.
*
      INFO = 0
      if(      ( .NOT.NOTA                 ).and.
     $         ( .NOT.LSAME( TRANSA, 'C' ) ).and.
     $         ( .NOT.LSAME( TRANSA, 'T' ) )      )then
         INFO = 1
      elseif( ( .NOT.NOTB                 ).and.
     $         ( .NOT.LSAME( TRANSB, 'C' ) ).and.
     $         ( .NOT.LSAME( TRANSB, 'T' ) )      )then
         INFO = 2
      elseif( M  .LT.0               )then
         INFO = 3
      elseif( N  .LT.0               )then
         INFO = 4
      elseif( K  .LT.0               )then
         INFO = 5
      elseif( LDA.LT.MAX( 1, NROWA ) )then
         INFO = 8
      elseif( LDB.LT.MAX( 1, NROWB ) )then
         INFO = 10
      elseif( LDC.LT.MAX( 1, M     ) )then
         INFO = 13
      endif
      if( INFO.NE.0 )then
         CALL XERBLA( 'DGEMM ', INFO )
         return
      endif
*
*     Quick return if possible.
*
      if( ( M.eq.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.eq.zero ).OR.( K.EQ.0 ) ).and.( BETA.EQ.ONE ) ) )
     $   return
*
*     And if  alpha.eq.zero.
*
      if( ALPHA.eq.zero )then
         if( BETA.eq.zero )then
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = zero
   10          continue
   20       continue
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          continue
   40       continue
         endif
         return
      endif
*
*     Start the operations.
*
      if( NOTB )then
         if( NOTA )then
*
*           Form  C := alpha*A*B + beta*C.
*
            DO 90, J = 1, N
               if( BETA.eq.zero )then
                  DO 50, I = 1, M
                     C( I, J ) = zero
   50             continue
               elseif( BETA.NE.ONE )then
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             continue
               endif
               DO 80, L = 1, K
                  if( B( L, J ).NE.zero )then
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                continue
                  endif
   80          continue
   90       continue
         ELSE
*
*           Form  C := alpha*A'*B + beta*C
*
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = zero
                  DO 100, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  100             continue
                  if( BETA.eq.zero )then
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  endif
  110          continue
  120       continue
         endif
      ELSE
         if( NOTA )then
*
*           Form  C := alpha*A*B' + beta*C
*
            DO 170, J = 1, N
               if( BETA.eq.zero )then
                  DO 130, I = 1, M
                     C( I, J ) = zero
  130             continue
               elseif( BETA.NE.ONE )then
                  DO 140, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  140             continue
               endif
               DO 160, L = 1, K
                  if( B( J, L ).NE.zero )then
                     TEMP = ALPHA*B( J, L )
                     DO 150, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  150                continue
                  endif
  160          continue
  170       continue
         ELSE
*
*           Form  C := alpha*A'*B' + beta*C
*
            DO 200, J = 1, N
               DO 190, I = 1, M
                  TEMP = zero
                  DO 180, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  180             continue
                  if( BETA.eq.zero )then
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  endif
  190          continue
  200       continue
         endif
      endif
*
      return
*
*     End of DGEMM .
*
      END
      subroutine dgemv( trans, m, n, alpha, a, lda, x, incx,
     $                   beta, y, incy )
c 
c***********************************************************************
c
*     .. Scalar Arguments ..
      double precision   ALPHA, BETA
      integer            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
*     .. Array Arguments ..
      double precision   A( LDA, * ), X( * ), Y( * )
c
cc DGEMV  performs one of the matrix-vector operations
*
*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and A is an
*  m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*
*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
*
*              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
*
*           Unchanged on exit.
*
*  M      - integer.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - integer.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - double precision.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - double precision array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients.
*           Unchanged on exit.
*
*  LDA    - integer.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*  X      - double precision array of DIMENSION at least
*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*           Before entry, the incremented array X must contain the
*           vector x.
*           Unchanged on exit.
*
*  INCX   - integer.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - double precision.
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - double precision array of DIMENSION at least
*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*           Before entry with BETA non-zero, the incremented array Y
*           must contain the vector y. On exit, Y is overwritten by the
*           updated vector y.
*
*  INCY   - integer.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      double precision   ONE         , zero
      PARAMETER        ( ONE = 1.0D+0, zero = 0.0D+0 )
*     .. Local Scalars ..
      double precision   TEMP
      integer            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).and.
     $         .NOT.LSAME( TRANS, 'T' ).and.
     $         .NOT.LSAME( TRANS, 'C' )      )then
         INFO = 1
      elseif( M.LT.0 )then
         INFO = 2
      elseif( N.LT.0 )then
         INFO = 3
      elseif( LDA.LT.MAX( 1, M ) )then
         INFO = 6
      elseif( INCX.eq.0 )then
         INFO = 8
      elseif( INCY.eq.0 )then
         INFO = 11
      endif
      if( INFO.NE.0 )then
         CALL XERBLA( 'DGEMV ', INFO )
         return
      endif
*
*  Quick return if possible.
*
      if( ( M.eq.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.eq.zero ).and.( BETA.EQ.ONE ) ) )
     $   return
*
*  Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*  up the start points in  X  and  Y.
*
      if( LSAME( TRANS, 'N' ) )then
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      endif
      if( INCX.GT.0 )then
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      endif
      if( INCY.GT.0 )then
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      endif
*
*  Start the operations. In this version the elements of A are
*  accessed sequentially with one pass through A.
*
*  First form  y := beta*y.
*
      if( BETA.NE.ONE )then
         if( INCY.eq.1 )then
            if( BETA.eq.zero )then
               DO 10, I = 1, LENY
                  Y( I ) = zero
   10          continue
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          continue
            endif
         ELSE
            IY = KY
            if( BETA.eq.zero )then
               DO 30, I = 1, LENY
                  Y( IY ) = zero
                  IY      = IY   + INCY
   30          continue
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          continue
            endif
         endif
      endif
      if( ALPHA.eq.zero )
     $   return
      if( LSAME( TRANS, 'N' ) )then
*
*  Form  y := alpha*A*x + y.
*
         JX = KX
         if( INCY.eq.1 )then
            DO 60, J = 1, N
               if( X( JX ).NE.zero )then
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             continue
               endif
               JX = JX + INCX
   60       continue
         ELSE
            DO 80, J = 1, N
               if( X( JX ).NE.zero )then
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             continue
               endif
               JX = JX + INCX
   80       continue
         endif
      ELSE
*
*  Form  y := alpha*A'*x + y.
*
         JY = KY
         if( INCX.eq.1 )then
            DO 100, J = 1, N
               TEMP = zero
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          continue
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       continue
         ELSE
            DO 120, J = 1, N
               TEMP = zero
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          continue
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       continue
         endif
      endif

      return
      END
      subroutine dgeqr2(m,n,a,lda,tau,work,info)
c 
c***********************************************************************
c
cc DGEQR2 computes a QR factorization of a real m by n matrix A:
*  A = Q * R.
*
*  Arguments
*  =========
*
*  M       (input) integer
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) integer
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) double precision array, dimension (LDA,N)
*          On entry, the m by n matrix A.
*          On exit, the elements on and above the diagonal of the array
*          contain the min(m,n) by n upper trapezoidal matrix R (R is
*          upper triangular if m >= n); the elements below the diagonal,
*          with the array TAU, represent the orthogonal matrix Q as a
*          product of elementary reflectors (see Further Details).
*
*  LDA     (input) integer
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) double precision array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace) double precision array, dimension (N)
*
*  INFO    (output) integer
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
*  and tau in TAU(i).
*
*  =====================================================================
*
      double precision one
      parameter        ( one = 1.0D+0 )
c
      integer lda
      integer n
c
      double precision a(lda,n)
      double precision aii
      integer i
      integer info
      integer k
      integer m
      double precision tau(*)
      double precision work(n)
c
      EXTERNAL           DLARF, DLARFG, XERBLA
      INTRINSIC          MAX, MIN
*
*  Test the input arguments
*
      info = 0
 
      if(m.lt.0)then
         info = -1
      elseif(n.lt.0)then
         info = -2
      elseif( LDA.LT.MAX( 1, M ) )then
         INFO = -4
      endif

      if(info.ne.0 )then
         CALL XERBLA( 'DGEQR2', -INFO )
         return
      endif

      k = MIN( M, N )
*
*  Generate elementary reflector H(i) to annihilate A(i+1:m,i)
*
      DO I = 1, K

         call dlarfg(m-i+1,a(i,i),a(min(i+1, m),i),1,tau(i))
*
*  Apply H(i) to A(i:m,i+1:n) from the left
*
         if(i.lt.n)then
            aii = a( i,i )
            a( i,i ) = one
            call dlarf( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
            A( I, I ) = AII
         endif

      enddo

      return
      end
      subroutine dgeqrf( m, n, a, lda, tau, work, lwork, info )
c 
c***********************************************************************
c
cc DGEQRF computes a QR factorization of a real M-by-N matrix A:
*  A = Q * R.
*
*  Arguments
*  =========
*
*  M       (input) integer
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) integer
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) double precision array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the elements on and above the diagonal of the array
*          contain the min(M,N)-by-N upper trapezoidal matrix R (R is
*          upper triangular if m >= n); the elements below the diagonal,
*          with the array TAU, represent the orthogonal matrix Q as a
*          product of min(m,n) elementary reflectors (see Further
*          Details).
*
*  LDA     (input) integer
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) double precision array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace/output) double precision array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) integer
*          The dimension of the array WORK.  LWORK >= max(1,N).
*          For optimum performance LWORK >= N*NB, where NB is
*          the optimal blocksize.
*
*  INFO    (output) integer
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
*  and tau in TAU(i).
*
*  =====================================================================
*
      integer lda
      integer lwork
      integer m
      integer n
c
      double precision a(lda,n)
      integer i
      integer ib
      integer iinfo
      integer ilaenv
      integer info
      integer iws
      integer k
      integer ldwork
      integer nb
      integer nbmin
      integer nx
      double precision tau(min(m,n))
      double precision work(lwork)
c
      EXTERNAL           DGEQR2, DLARFB, DLARFT, XERBLA
      INTRINSIC          MAX, MIN
      EXTERNAL           ILAENV
*
*  Test the input arguments
*
      info = 0
 
      if( M.LT.0 ) then
         INFO = -1
      elseif( N.LT.0 ) then
         INFO = -2
      elseif( LDA.LT.MAX( 1, M ) ) then
         INFO = -4
      elseif( LWORK.LT.MAX( 1, N ) ) then
         INFO = -7
      endif
 
      if( INFO.NE.0 ) then
         CALL XERBLA( 'DGEQRF', -INFO )
         return
      endif
*
*  Quick return if possible
*
      K = MIN( M, N )
 
      if( K.eq.0 ) then
         WORK( 1 ) = 1
         return
      endif
*
*  Determine the block size.
*
      NB = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
      nbmin = 2
      NX = 0
      IWS = N
*
*  Determine when to cross over from blocked to unblocked code.
*
      if( NB.GT.1 .and. NB.LT.K )then
         NX = MAX( 0, ILAENV( 3, 'DGEQRF', ' ', M, N, -1, -1 ) )
*
*  Determine if workspace is large enough for blocked code.
*
         if(nx.lt.k)then
            LDWORK = N
            IWS = LDWORK*NB
*
*  Not enough workspace to use optimal NB:  reduce NB and
*  determine the minimum value of NB.
*
            if( LWORK.LT.IWS )then
               NB = LWORK / LDWORK
               NBMIN=MAX(2,ILAENV( 2,'DGEQRF',' ', M, N, -1,-1))
            endif
 
         endif
 
      endif
 
*
*  Use blocked code initially
*
      if(nb.ge.nbmin.and.nb.lt.k.and.nx.lt.k)then
         do i = 1, k - nx, nb
            ib = min( k-i+1, nb )
*
*  Compute the QR factorization of the current block A(i:m,i:i+ib-1)
*
            call dgeqr2(m-i+1,ib,a(i,i),lda,tau(i),work,iinfo)
*
*  Form the triangular factor of the block reflector
*  H = H(i) H(i+1) . . . H(i+ib-1)
*
            if(i+ib.le.n)then
               call dlarft( 'Forward', 'Columnwise', M-I+1, IB,
     $                      a( i,i ), lda, tau( i ), work, ldwork )
*
*  Apply H' to A(i:m,i+ib:n) from the left
*
               call dlarfb( 'Left', 'Transpose', 'Forward',
     $                      'Columnwise', m-i+1, n-i-ib+1, ib,
     $                      a( i,i ), lda, work, ldwork, a( i,i+ib ),
     $                      lda, work( ib+1 ), ldwork )
            endif
        enddo
 
      else
 
         i = 1
 
      endif
 
*
*  Use unblocked code to factor the last or only block.
*
      if(i.le.k)then
        call dgeqr2(m-i+1,n-i+1,a(i,i),lda,tau(i),work,iinfo)
      endif
 
      work(1) = iws
 
      return
      end
      subroutine dger( m, n, alpha, x, incx, y, incy, a, lda )
c 
c***********************************************************************
c
*     .. Scalar Arguments ..
      double precision   ALPHA
      integer            INCX, INCY, LDA, M, N
*     .. Array Arguments ..
      double precision   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
cc DGER   performs the rank 1 operation A := alpha*x*y' + A,
*
*  where alpha is a scalar, x is an m element vector, y is an n element
*  vector and A is an m by n matrix.
*
*  Parameters
*  ==========
*
*  M      - integer.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - integer.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - double precision.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  X      - double precision array of dimension at least
*           ( 1 + ( m - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the m
*           element vector x.
*           Unchanged on exit.
*
*  INCX   - integer.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  Y      - double precision array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCY ) ).
*           Before entry, the incremented array Y must contain the n
*           element vector y.
*           Unchanged on exit.
*
*  INCY   - integer.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*  A      - double precision array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients. On exit, A is
*           overwritten by the updated matrix.
*
*  LDA    - integer.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      double precision   zero
      PARAMETER        ( zero = 0.0D+0 )
*     .. Local Scalars ..
      double precision   TEMP
      integer            I, INFO, IX, J, JY, KX
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( M.LT.0 )then
         INFO = 1
      elseif( N.LT.0 )then
         INFO = 2
         write(*,*)' '
         write(*,*)'DGER - Fatal error!'
         write(*,*)'  Input value of N was less than 0.'
         write(*,*)'  N = ',n
      elseif( incx.eq.0 )then
         INFO = 5
      elseif( INCY.eq.0 )then
         INFO = 7
      elseif( LDA.LT.MAX( 1, M ) )then
         INFO = 9
      endif
      if( INFO.NE.0 )then
         CALL XERBLA( 'DGER  ', INFO )
         return
      endif
*
*     Quick return if possible.
*
      if( ( M.eq.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.zero ) )
     $   return
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
      if( INCY.GT.0 )then
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      endif
      if( INCX.eq.1 )then
         DO 20, J = 1, N
            if( Y( JY ).NE.zero )then
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          continue
            endif
            JY = JY + INCY
   20    continue
      ELSE
         if( INCX.GT.0 )then
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         endif
         DO 40, J = 1, N
            if( Y( JY ).NE.zero )then
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          continue
            endif
            JY = JY + INCY
   40    continue
      endif
*
      return
*
*     End of DGER  .
*
      end
      subroutine dgetf2(m,n,a,lda,ipiv,info)
c
c***********************************************************************
c
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1992
*
*     .. Scalar Arguments ..
      integer            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      integer            IPIV( * )
      double precision   A( LDA, * )
c
cc DGETF2 computes an LU factorization of a general m-by-n matrix A
*  using partial pivoting with row interchanges.
*
*  The factorization has the form
*     A = P * L * U
*  where P is a permutation matrix, L is lower triangular with unit
*  diagonal elements (lower trapezoidal if m > n), and U is upper
*  triangular (upper trapezoidal if m < n).
*
*  This is the right-looking Level 2 BLAS version of the algorithm.
*
*  Arguments
*  =========
*
*  M       (input) integer
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) integer
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) double precision array, dimension (LDA,N)
*          On entry, the m by n matrix to be factored.
*          On exit, the factors L and U from the factorization
*          A = P*L*U; the unit diagonal elements of L are not stored.
*
*  LDA     (input) integer
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  IPIV    (output) integer array, dimension (min(M,N))
*          The pivot indices; for 1 <= i <= min(M,N), row i of the
*          matrix was interchanged with row IPIV(i).
*
*  INFO    (output) integer
*          = 0: successful exit
*          < 0: if INFO = -k, the k-th argument had an illegal value
*          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
*               has been completed, but the factor U is exactly
*               singular, and division by zero will occur if it is used
*               to solve a system of equations.
*
*  =====================================================================
*
*     .. Parameters ..
      double precision   ONE, zero
      PARAMETER          ( ONE = 1.0D+0, zero = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      integer            J, JP
*     ..
*     .. External Functions ..
      integer            IDAMAX
      EXTERNAL           IDAMAX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGER, DSCAL, DSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      if( M.LT.0 )then
         INFO = -1
      elseif( N.LT.0 )then
         INFO = -2
      elseif( LDA.LT.MAX( 1, M ) )then
         INFO = -4
      endif
      if( INFO.NE.0 )then
         CALL XERBLA( 'DGETF2', -INFO )
         return
      endif
*
*     Quick return if possible
*
      if( M.eq.0 .OR. N.EQ.0 )
     $   return
*
      DO 10 J = 1, MIN( M, N )
*
*        Find pivot and test for singularity.
*
         JP = J - 1 + IDAMAX( M-J+1, A( J, J ), 1 )
         IPIV( J ) = JP
         if( A( JP, J ).NE.zero )then
*
*  Apply the interchange to columns 1:N.
*
            if( JP.NE.J )then
               CALL DSWAP( N, A( J, 1 ), LDA, A( JP, 1 ), LDA )
            endif
*
*   Compute elements J+1:M of J-th column.
*
            if( J.LT.M )then
               CALL DSCAL( M-J, ONE / A( J, J ), A( J+1, J ), 1 )
            endif
         elseif( INFO.eq.0 )then
            INFO = J
         endif

         if( J.LT.MIN( M, N ) )then
*
*  Update trailing submatrix.
*
            call dger( M-J, N-J, -ONE, A( J+1, J ), 1, A( J, J+1 ), LDA,
     $                 A( J+1, J+1 ), LDA )
         endif

   10 continue

      return
      end
      subroutine dgetrf( M, N, A, LDA, IPIV, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      integer            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      integer            IPIV( * )
      double precision   A( LDA, * )
c
cc DGETRF computes an LU factorization of a general M-by-N matrix A
*  using partial pivoting with row interchanges.
*
*  The factorization has the form
*     A = P * L * U
*  where P is a permutation matrix, L is lower triangular with unit
*  diagonal elements (lower trapezoidal if m > n), and U is upper
*  triangular (upper trapezoidal if m < n).
*
*  This is the right-looking Level 3 BLAS version of the algorithm.
*
*  Arguments
*  =========
*
*  M       (input) integer
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) integer
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) double precision array, dimension (LDA,N)
*          On entry, the M-by-N matrix to be factored.
*          On exit, the factors L and U from the factorization
*          A = P*L*U; the unit diagonal elements of L are not stored.
*
*  LDA     (input) integer
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  IPIV    (output) integer array, dimension (min(M,N))
*          The pivot indices; for 1 <= i <= min(M,N), row i of the
*          matrix was interchanged with row IPIV(i).
*
*  INFO    (output) integer
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
*                has been completed, but the factor U is exactly
*                singular, and division by zero will occur if it is used
*                to solve a system of equations.
*
*  =====================================================================
*
*     .. Parameters ..
      double precision   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      integer            I, IINFO, J, JB, NB
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DGETF2, DLASWP, DTRSM, XERBLA
*     ..
*     .. External Functions ..
      integer            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      if( M.LT.0 )then
         INFO = -1
      elseif( N.LT.0 )then
         INFO = -2
      elseif( LDA.LT.MAX( 1, M ) )then
         INFO = -4
      endif
      if( INFO.NE.0 )then
         CALL XERBLA( 'DGETRF', -INFO )
         return
      endif
*
*     Quick return if possible
*
      if( M.eq.0 .OR. N.EQ.0 )
     $   return
*
*     Determine the block size for this environment.
*
      NB = ILAENV( 1, 'DGETRF', ' ', M, N, -1, -1 )
      if( NB.LE.1 .OR. NB.ge.MIN( M, N ) )then
*
*        Use unblocked code.
*
         CALL DGETF2( M, N, A, LDA, IPIV, INFO )
      ELSE
*
*        Use blocked code.
*
         DO 20 J = 1, MIN( M, N ), NB
            JB = MIN( MIN( M, N )-J+1, NB )
*
*           Factor diagonal and subdiagonal blocks and test for exact
*           singularity.
*
            CALL DGETF2( M-J+1, JB, A( J, J ), LDA, IPIV( J ), IINFO )
*
*           Adjust INFO and the pivot indices.
*
            if( INFO.eq.0 .and. IINFO.GT.0 )
     $         INFO = IINFO + J - 1
            DO 10 I = J, MIN( M, J+JB-1 )
               IPIV( I ) = J - 1 + IPIV( I )
   10       continue
*
*           Apply interchanges to columns 1:J-1.
*
            CALL DLASWP( J-1, A, LDA, J, J+JB-1, IPIV, 1 )
*
            if( J+JB.LE.N )then
*
*              Apply interchanges to columns J+JB:N.
*
               CALL DLASWP( N-J-JB+1, A( 1, J+JB ), LDA, J, J+JB-1,
     $                      IPIV, 1 )
*
*              Compute block row of U.
*
               CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Unit', JB,
     $                     N-J-JB+1, ONE, A( J, J ), LDA, A( J, J+JB ),
     $                     LDA )
               if( J+JB.LE.M )then
*
*                 Update trailing submatrix.
*
                  CALL DGEMM( 'No transpose', 'No transpose', M-J-JB+1,
     $                        N-J-JB+1, JB, -ONE, A( J+JB, J ), LDA,
     $                        A( J, J+JB ), LDA, ONE, A( J+JB, J+JB ),
     $                        LDA )
               endif
            endif
   20    continue
      endif
      return
*
*     End of DGETRF
*
      END
      subroutine dgetrs( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS
      integer            INFO, LDA, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      integer            IPIV( * )
      double precision   A( LDA, * ), B( LDB, * )
c
cc DGETRS solves a system of linear equations
*     A * X = B  or  A' * X = B
*  with a general N-by-N matrix A using the LU factorization computed
*  by DGETRF.
*
*  Arguments
*  =========
*
*  TRANS   (input) CHARACTER*1
*          Specifies the form of the system of equations:
*          = 'N':  A * X = B  (No transpose)
*          = 'T':  A'* X = B  (Transpose)
*          = 'C':  A'* X = B  (Conjugate transpose = Transpose)
*
*  N       (input) integer
*          The order of the matrix A.  N >= 0.
*
*  NRHS    (input) integer
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  A       (input) double precision array, dimension (LDA,N)
*          The factors L and U from the factorization A = P*L*U
*          as computed by DGETRF.
*
*  LDA     (input) integer
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  IPIV    (input) integer array, dimension (N)
*          The pivot indices from DGETRF; for 1<=i<=N, row i of the
*          matrix was interchanged with row IPIV(i).
*
*  B       (input/output) double precision array, dimension (LDB,NRHS)
*          On entry, the right hand side matrix B.
*          On exit, the solution matrix X.
*
*  LDB     (input) integer
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) integer
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      double precision   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOTRAN
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASWP, DTRSM, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      NOTRAN = LSAME( TRANS, 'N' )
      if( .NOT.NOTRAN .and. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $    LSAME( TRANS, 'C' ) )then
         INFO = -1
      elseif( N.LT.0 )then
         INFO = -2
      elseif( NRHS.LT.0 )then
         INFO = -3
      elseif( LDA.LT.MAX( 1, N ) )then
         INFO = -5
      elseif( LDB.LT.MAX( 1, N ) )then
         INFO = -8
      endif
      if( INFO.NE.0 )then
         CALL XERBLA( 'DGETRS', -INFO )
         return
      endif
*
*     Quick return if possible
*
      if( N.eq.0 .OR. NRHS.EQ.0 )
     $   return
*
      if( NOTRAN )then
*
*        Solve A * X = B.
*
*        Apply row interchanges to the right hand sides.
*
         CALL DLASWP( NRHS, B, LDB, 1, N, IPIV, 1 )
*
*        Solve L*X = B, overwriting B with X.
*
         CALL DTRSM( 'Left', 'Lower', 'No transpose', 'Unit', N, NRHS,
     $               ONE, A, LDA, B, LDB )
*
*        Solve U*X = B, overwriting B with X.
*
         CALL DTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', N,
     $               NRHS, ONE, A, LDA, B, LDB )
      ELSE
*
*        Solve A' * X = B.
*
*        Solve U'*X = B, overwriting B with X.
*
         CALL DTRSM( 'Left', 'Upper', 'Transpose', 'Non-unit', N, NRHS,
     $               ONE, A, LDA, B, LDB )
*
*        Solve L'*X = B, overwriting B with X.
*
         CALL DTRSM( 'Left', 'Lower', 'Transpose', 'Unit', N, NRHS, ONE,
     $               A, LDA, B, LDB )
*
*        Apply row interchanges to the solution vectors.
*
         CALL DLASWP( NRHS, B, LDB, 1, N, IPIV, -1 )
      endif
*
      return
*
*     End of DGETRS
*
      END
      double precision function dlapy2( x, y )
c 
c***********************************************************************
c
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      double precision   X, Y
c
cc DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
*  overflow.
*
*  Arguments
*  =========
*
*  X       (input) double precision
*  Y       (input) double precision
*          X and Y specify the values x and y.
*
*  =====================================================================
*
*     .. Parameters ..
      double precision   zero
      PARAMETER          ( zero = 0.0D0 )
      double precision   ONE
      PARAMETER          ( ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      double precision   W, XABS, YABS, Z
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      XABS = ABS( X )
      YABS = ABS( Y )
      W = MAX( XABS, YABS )
      Z = MIN( XABS, YABS )
      if( Z.eq.zero )then
         DLAPY2 = W
      ELSE
         DLAPY2 = W*SQRT( ONE+( Z / W )**2 )
      endif
      return
*
*     End of DLAPY2
*
      END
      subroutine dlarf( side, m, n, v, incv, tau, c, ldc, work )
c 
c***********************************************************************
c
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE
      integer            INCV, LDC, M, N
      double precision   TAU
*     ..
*     .. Array Arguments ..
      double precision   C( LDC, * ), V( * ), WORK( * )
c
cc DLARF applies a real elementary reflector H to a real m by n matrix
*  C, from either the left or the right. H is represented in the form
*
*        H = I - tau * v * v'
*
*  where tau is a real scalar and v is a real vector.
*
*  If tau = 0, then H is taken to be the unit matrix.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': form  H * C
*          = 'R': form  C * H
*
*  M       (input) integer
*          The number of rows of the matrix C.
*
*  N       (input) integer
*          The number of columns of the matrix C.
*
*  V       (input) double precision array, dimension
*                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
*                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
*          The vector v in the representation of H. V is not used if
*          TAU = 0.
*
*  INCV    (input) integer
*          The increment between elements of v. INCV <> 0.
*
*  TAU     (input) double precision
*          The value tau in the representation of H.
*
*  C       (input/output) double precision array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
*          or C * H if SIDE = 'R'.
*
*  LDC     (input) integer
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) double precision array, dimension
*                         (N) if SIDE = 'L'
*                      or (M) if SIDE = 'R'
*
*  =====================================================================
*
*     .. Parameters ..
      double precision   ONE, zero
      PARAMETER          ( ONE = 1.0D+0, zero = 0.0D+0 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DGER
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*
      if( LSAME( SIDE, 'L' ) )then
*
*  Form  H * C
*
         if( TAU.NE.zero )then
*
*  w := C' * v
*
            CALL DGEMV( 'Transpose', M, N, ONE, C, LDC, V, INCV, zero,
     $                  WORK, 1 )
*
*  C := C - v * w'
*
            CALL DGER( M, N, -TAU, V, INCV, WORK, 1, C, LDC )
         endif
      ELSE
*
*  Form  C * H
*
         if( TAU.NE.zero )then
*
*           w := C * v
*
            CALL DGEMV( 'No transpose', M, N, ONE, C, LDC, V, INCV,
     $                  zero, WORK, 1 )
*
*  C := C - w * v'
*
            CALL DGER( M, N, -TAU, WORK, 1, V, INCV, C, LDC )
         endif
      endif
      return
      end
      subroutine dlarfb( side, trans, direct, storev, m, n, k, v, ldv,
     $                   t, ldt, c, ldc, work, ldwork )
c 
c***********************************************************************
c
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, SIDE, STOREV, TRANS
      integer            K, LDC, LDT, LDV, LDWORK, M, N
*     ..
*     .. Array Arguments ..
      double precision   C( LDC, * ), T( LDT, * ), V( LDV, * ),
     $                   WORK( LDWORK, * )
c
cc DLARFB applies a real block reflector H or its transpose H' to a
*  real m by n matrix C, from either the left or the right.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply H or H' from the Left
*          = 'R': apply H or H' from the Right
*
*  TRANS   (input) CHARACTER*1
*          = 'N': apply H (No transpose)
*          = 'T': apply H' (Transpose)
*
*  DIRECT  (input) CHARACTER*1
*          Indicates how H is formed from a product of elementary
*          reflectors
*          = 'F': H = H(1) H(2) . . . H(k) (Forward)
*          = 'B': H = H(k) . . . H(2) H(1) (Backward)
*
*  STOREV  (input) CHARACTER*1
*          Indicates how the vectors which define the elementary
*          reflectors are stored:
*          = 'C': Columnwise
*          = 'R': Rowwise
*
*  M       (input) integer
*          The number of rows of the matrix C.
*
*  N       (input) integer
*          The number of columns of the matrix C.
*
*  K       (input) integer
*          The order of the matrix T (= the number of elementary
*          reflectors whose product defines the block reflector).
*
*  V       (input) double precision array, dimension
*                                (LDV,K) if STOREV = 'C'
*                                (LDV,M) if STOREV = 'R' and SIDE = 'L'
*                                (LDV,N) if STOREV = 'R' and SIDE = 'R'
*          The matrix V. See further details.
*
*  LDV     (input) integer
*          The leading dimension of the array V.
*          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
*          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
*          if STOREV = 'R', LDV >= K.
*
*  T       (input) double precision array, dimension (LDT,K)
*          The triangular k by k matrix T in the representation of the
*          block reflector.
*
*  LDT     (input) integer
*          The leading dimension of the array T. LDT >= K.
*
*  C       (input/output) double precision array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
*
*  LDC     (input) integer
*          The leading dimension of the array C. LDA >= max(1,M).
*
*  WORK    (workspace) double precision array, dimension (LDWORK,K)
*
*  LDWORK  (input) integer
*          The leading dimension of the array WORK.
*          If SIDE = 'L', LDWORK >= max(1,N);
*          if SIDE = 'R', LDWORK >= max(1,M).
*
*  =====================================================================
*
*     .. Parameters ..
      double precision   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          TRANST
      integer            I, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DTRMM
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      if( M.LE.0 .OR. N.LE.0 )
     $   return
*
      if( LSAME( TRANS, 'N' ) )then
         TRANST = 'T'
      ELSE
         TRANST = 'N'
      endif
*
      if( LSAME( STOREV, 'C' ) )then
*
         if( LSAME( DIRECT, 'F' ) )then
*
*           Let  V =  ( V1 )    (first K rows)
*                     ( V2 )
*           where  V1  is unit lower triangular.
*
            if( LSAME( SIDE, 'L' ) )then
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
*
*              W := C1'
*
               DO 10 J = 1, K
                  CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
   10          continue
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               if( M.GT.K )then
*
*                 W := W + C2'*V2
*
                  CALL DGEMM( 'Transpose', 'No transpose', N, K, M-K,
     $                        ONE, C( K+1, 1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               endif
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V * W'
*
               if( M.GT.K )then
*
*                 C2 := C2 - V2 * W'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M-K, N, K,
     $                        -ONE, V( K+1, 1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               endif
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K,
     $                     ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W'
*
               DO 30 J = 1, K
                  DO 20 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
   20             continue
   30          continue
*
            elseif( LSAME( SIDE, 'R' ) )then
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
*
*              W := C1
*
               DO 40 J = 1, K
                  CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
   40          continue
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               if( N.GT.K )then
*
*                 W := W + C2 * V2
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               endif
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V'
*
               if( N.GT.K )then
*
*                 C2 := C2 - W * V2'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( K+1, 1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               endif
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K,
     $                     ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W
*
               DO 60 J = 1, K
                  DO 50 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
   50             continue
   60          continue
            endif
*
         ELSE
*
*           Let  V =  ( V1 )
*                     ( V2 )    (last K rows)
*           where  V2  is unit upper triangular.
*
            if( LSAME( SIDE, 'L' ) )then
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
*
*              W := C2'
*
               DO 70 J = 1, K
                  CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
   70          continue
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
               if( M.GT.K )then
*
*                 W := W + C1'*V1
*
                  CALL DGEMM( 'Transpose', 'No transpose', N, K, M-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               endif
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V * W'
*
               if( M.GT.K )then
*
*                 C1 := C1 - V1 * W'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M-K, N, K,
     $                        -ONE, V, LDV, WORK, LDWORK, ONE, C, LDC )
               endif
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K,
     $                     ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
*
*              C2 := C2 - W'
*
               DO 90 J = 1, K
                  DO 80 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
   80             continue
   90          continue
*
            elseif( LSAME( SIDE, 'R' ) )then
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
*
*              W := C2
*
               DO 100 J = 1, K
                  CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  100          continue
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
               if( N.GT.K )then
*
*                 W := W + C1 * V1
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               endif
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V'
*
               if( N.GT.K )then
*
*                 C1 := C1 - W * V1'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               endif
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K,
     $                     ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
*
*              C2 := C2 - W
*
               DO 120 J = 1, K
                  DO 110 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  110             continue
  120          continue
            endif
         endif
*
      elseif( LSAME( STOREV, 'R' ) )then
*
         if( LSAME( DIRECT, 'F' ) )then
*
*           Let  V =  ( V1  V2 )    (V1: first K columns)
*           where  V1  is unit upper triangular.
*
            if( LSAME( SIDE, 'L' ) )then
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
*
*              W := C1'
*
               DO 130 J = 1, K
                  CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
  130          continue
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               if( M.GT.K )then
*
*                 W := W + C2'*V2'
*
                  CALL DGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE,
     $                        C( K+1, 1 ), LDC, V( 1, K+1 ), LDV, ONE,
     $                        WORK, LDWORK )
               endif
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V' * W'
*
               if( M.GT.K )then
*
*                 C2 := C2 - V2' * W'
*
                  CALL DGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE,
     $                        V( 1, K+1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               endif
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W'
*
               DO 150 J = 1, K
                  DO 140 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
  140             continue
  150          continue
*
            elseif( LSAME( SIDE, 'R' ) )then
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
*
*              W := C1
*
               DO 160 J = 1, K
                  CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
  160          continue
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               if( N.GT.K )then
*
*                 W := W + C2 * V2'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( 1, K+1 ), LDV,
     $                        ONE, WORK, LDWORK )
               endif
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V
*
               if( N.GT.K )then
*
*                 C2 := C2 - W * V2
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( 1, K+1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               endif
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W
*
               DO 180 J = 1, K
                  DO 170 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
  170             continue
  180          continue
*
            endif
*
         ELSE
*
*           Let  V =  ( V1  V2 )    (V2: last K columns)
*           where  V2  is unit lower triangular.
*
            if( LSAME( SIDE, 'L' ) )then
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
*
*              W := C2'
*
               DO 190 J = 1, K
                  CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
  190          continue
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K,
     $                     ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
               if( M.GT.K )then
*
*                 W := W + C1'*V1'
*
                  CALL DGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE,
     $                        C, LDC, V, LDV, ONE, WORK, LDWORK )
               endif
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V' * W'
*
               if( M.GT.K )then
*
*                 C1 := C1 - V1' * W'
*
                  CALL DGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE,
     $                        V, LDV, WORK, LDWORK, ONE, C, LDC )
               endif
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
*
*              C2 := C2 - W'
*
               DO 210 J = 1, K
                  DO 200 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
  200             continue
  210          continue
*
            elseif( LSAME( SIDE, 'R' ) )then
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
*
*              W := C2
*
               DO 220 J = 1, K
                  CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  220          continue
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K,
     $                     ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
               if( N.GT.K )then
*
*                 W := W + C1 * V1'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               endif
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V
*
               if( N.GT.K )then
*
*                 C1 := C1 - W * V1
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               endif
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
*
*              C1 := C1 - W
*
               DO 240 J = 1, K
                  DO 230 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  230             continue
  240          continue
*
            endif
*
         endif
      endif
*
      return
*
*     End of DLARFB
*
      END
      subroutine dlarft( direct, storev, n, k, v, ldv, tau, t, ldt )
c 
c***********************************************************************
c
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, STOREV
      integer            K, LDT, LDV, N
*     ..
*     .. Array Arguments ..
      double precision   T( LDT, * ), TAU( * ), V( LDV, * )
c
cc DLARFT forms the triangular factor T of a real block reflector H
*  of order n, which is defined as a product of k elementary reflectors.
*
*  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
*
*  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
*
*  If STOREV = 'C', the vector which defines the elementary reflector
*  H(i) is stored in the i-th column of the array V, and
*
*     H  =  I - V * T * V'
*
*  If STOREV = 'R', the vector which defines the elementary reflector
*  H(i) is stored in the i-th row of the array V, and
*
*     H  =  I - V' * T * V
*
*  Arguments
*  =========
*
*  DIRECT  (input) CHARACTER*1
*          Specifies the order in which the elementary reflectors are
*          multiplied to form the block reflector:
*          = 'F': H = H(1) H(2) . . . H(k) (Forward)
*          = 'B': H = H(k) . . . H(2) H(1) (Backward)
*
*  STOREV  (input) CHARACTER*1
*          Specifies how the vectors which define the elementary
*          reflectors are stored (see also Further Details):
*          = 'C': columnwise
*          = 'R': rowwise
*
*  N       (input) integer
*          The order of the block reflector H. N >= 0.
*
*  K       (input) integer
*          The order of the triangular factor T (= the number of
*          elementary reflectors). K >= 1.
*
*  V       (input/output) double precision array, dimension
*                               (LDV,K) if STOREV = 'C'
*                               (LDV,N) if STOREV = 'R'
*          The matrix V. See further details.
*
*  LDV     (input) integer
*          The leading dimension of the array V.
*          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
*
*  TAU     (input) double precision array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i).
*
*  T       (output) double precision array, dimension (LDT,K)
*          The k by k triangular factor T of the block reflector.
*          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
*          lower triangular. The rest of the array is not used.
*
*  LDT     (input) integer
*          The leading dimension of the array T. LDT >= K.
*
*  Further Details
*  ===============
*
*  The shape of the matrix V and the storage of the vectors which define
*  the H(i) is best illustrated by the following example with n = 5 and
*  k = 3. The elements equal to 1 are not stored; the corresponding
*  array elements are modified but restored on exit. The rest of the
*  array is not used.
*
*  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
*
*               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
*                   ( v1  1    )                     (     1 v2 v2 v2 )
*                   ( v1 v2  1 )                     (        1 v3 v3 )
*                   ( v1 v2 v3 )
*                   ( v1 v2 v3 )
*
*  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
*
*               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
*                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
*                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
*                   (     1 v3 )
*                   (        1 )
*
*  =====================================================================
*
*     .. Parameters ..
      double precision   ONE, zero
      PARAMETER          ( ONE = 1.0D+0, zero = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      integer            I, J
      double precision   VII
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DTRMV
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      if( N.eq.0 )
     $   return
*
      if( LSAME( DIRECT, 'F' ) )then
         DO 20 I = 1, K
            if( TAU( I ).eq.zero )then
*
*              H(i)  =  I
*
               DO 10 J = 1, I
                  T( J, I ) = zero
   10          continue
            ELSE
*
*              general case
*
               VII = V( I, I )
               V( I, I ) = ONE
               if( LSAME( STOREV, 'C' ) )then
*
*                 T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' * V(i:n,i)
*
                  CALL DGEMV( 'Transpose', N-I+1, I-1, -TAU( I ),
     $                        V( I, 1 ), LDV, V( I, I ), 1, zero,
     $                        T( 1, I ), 1 )
               ELSE
*
*                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:n) * V(i,i:n)'
*
                  CALL DGEMV( 'No transpose', I-1, N-I+1, -TAU( I ),
     $                        V( 1, I ), LDV, V( I, I ), LDV, zero,
     $                        T( 1, I ), 1 )
               endif
               V( I, I ) = VII
*
*              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i)
*
               CALL DTRMV( 'Upper', 'No transpose', 'Non-unit', I-1, T,
     $                     LDT, T( 1, I ), 1 )
               T( I, I ) = TAU( I )
            endif
   20    continue
      ELSE
         DO 40 I = K, 1, -1
            if( TAU( I ).eq.zero )then
*
*              H(i)  =  I
*
               DO 30 J = I, K
                  T( J, I ) = zero
   30          continue
            ELSE
*
*              general case
*
               if( I.LT.K )then
                  if( LSAME( STOREV, 'C' ) )then
                     VII = V( N-K+I, I )
                     V( N-K+I, I ) = ONE
*
*                    T(i+1:k,i) :=
*                            - tau(i) * V(1:n-k+i,i+1:k)' * V(1:n-k+i,i)
*
                     CALL DGEMV( 'Transpose', N-K+I, K-I, -TAU( I ),
     $                           V( 1, I+1 ), LDV, V( 1, I ), 1, zero,
     $                           T( I+1, I ), 1 )
                     V( N-K+I, I ) = VII
                  ELSE
                     VII = V( I, N-K+I )
                     V( I, N-K+I ) = ONE
*
*                    T(i+1:k,i) :=
*                            - tau(i) * V(i+1:k,1:n-k+i) * V(i,1:n-k+i)'
*
                     CALL DGEMV( 'No transpose', K-I, N-K+I, -TAU( I ),
     $                           V( I+1, 1 ), LDV, V( I, 1 ), LDV, zero,
     $                           T( I+1, I ), 1 )
                     V( I, N-K+I ) = VII
                  endif
*
*                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i)
*
                  CALL DTRMV( 'Lower', 'No transpose', 'Non-unit', K-I,
     $                        T( I+1, I+1 ), LDT, T( I+1, I ), 1 )
               endif
               T( I, I ) = TAU( I )
            endif
   40    continue
      endif
      return
*
*     End of DLARFT
*
      end
      subroutine dlaswp(n,a,lda,k1,k2,ipiv,incx)
c
c***********************************************************************
c
c  -- LAPACK auxiliary routine (version 1.0) --
c     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
c     Courant Institute, Argonne National Lab, and Rice University
c     February 29, 1992
c
c     .. Scalar Arguments ..
      integer            incx, k1, k2, lda, n
c     ..
c     .. Array Arguments ..
      integer            ipiv( * )
      double precision   a( lda, * )
c
cc DLASWP performs a series of row interchanges on the matrix A.
c  One row interchange is initiated for each of rows K1 through K2 of A.
c
c  Arguments
c  =========
c
c  N       (input) integer
c          The number of columns of the matrix A.
c
c  A       (input/output) double precision array, dimension (LDA,N)
c          On entry, the matrix of column dimension N to which the row
c          interchanges will be applied.
c          On exit, the permuted matrix.
c
c  LDA     (input) integer
c          The leading dimension of the array A.
c
c  K1      (input) integer
c          The first element of IPIV for which a row interchange will
c          be done.
c
c  K2      (input) integer
c          The last element of IPIV for which a row interchange will
c          be done.
c
c  IPIV    (input) integer array, dimension (M*abs(INCX))
c          The vector of pivot indices.  Only the elements in positions
c          K1 through K2 of IPIV are accessed.
c          IPIV(K) = L implies rows K and L are to be interchanged.
c
c  INCX    (input) integer
c          The increment between successive values of IPIV.  If IPIV
c          is negative, the pivots are applied in reverse order.
c
c
c     .. Local Scalars ..
      integer            i, ip, ix
c     ..
c     .. External Subroutines ..
      external           dswap
c     ..
c     .. Executable Statements ..
c
c     Interchange row I with row IPIV(I) for each of rows K1 through K2.
c
      if( incx.eq.0 )
     $   return
      if( incx.gt.0 ) then
         ix = k1
      else
         ix = 1 + ( 1-k2 )*incx
      end if
      if( incx.eq.1 ) then
         do 10 i = k1, k2
            ip = ipiv( i )
            if( ip.ne.i )
     $         call dswap( n, a( i, 1 ), lda, a( ip, 1 ), lda )
   10    continue
      else if( incx.gt.1 ) then
         do 20 i = k1, k2
            ip = ipiv( ix )
            if( ip.ne.i )
     $         call dswap( n, a( i, 1 ), lda, a( ip, 1 ), lda )
            ix = ix + incx
   20    continue
      else if( incx.lt.0 ) then
         do 30 i = k2, k1, -1
            ip = ipiv( ix )
            if( ip.ne.i )
     $         call dswap( n, a( i, 1 ), lda, a( ip, 1 ), lda )
            ix = ix + incx
   30    continue
      end if
c
      return
      end
      double precision function dnrm2( n, x, incx )
c 
c***********************************************************************
c
*     .. Scalar Arguments ..
      integer                           INCX, N
*     .. Array Arguments ..
      double precision                  X( * )
c
cc DNRM2 returns the euclidean norm of a vector via the function
*  name, so that
*
*     DNRM2 := sqrt( x'*x )
*
*
*
*  -- This version written on 25-October-1982.
*     Modified on 14-October-1993 to inline the call to DLASSQ.
*     Sven Hammarling, Nag Ltd.
*
*
*     .. Parameters ..
      double precision      ONE         , zero
      PARAMETER           ( ONE = 1.0D+0, zero = 0.0D+0 )
*     .. Local Scalars ..
      integer               IX
      double precision      ABSXI, NORM, SCALE, SSQ
*     .. Intrinsic Functions ..
      INTRINSIC             ABS, SQRT
*     ..
*     .. Executable Statements ..
      if( N.LT.1 .OR. INCX.LT.1 )then
         NORM  = zero
      elseif( N.eq.1 )then
         NORM  = ABS( X( 1 ) )
      ELSE
         SCALE = zero
         SSQ   = ONE
*        The following loop is equivalent to this call to the LAPACK
*        auxiliary routine:
*        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
*
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            if( X( IX ).NE.zero )then
               ABSXI = ABS( X( IX ) )
               if( SCALE.LT.ABSXI )then
                  SSQ   = ONE   + SSQ*( SCALE/ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SSQ   = SSQ   +     ( ABSXI/SCALE )**2
               endif
            endif
   10    continue
         NORM  = SCALE * SQRT( SSQ )
      endif
*
      DNRM2 = NORM
      return
*
*     End of DNRM2.
*
      END
      subroutine dorg2r( m, n, k, a, lda, tau, work, info )
c 
c***********************************************************************
c
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      integer            INFO, K, LDA, M, N
*     ..
*     .. Array Arguments ..
      double precision   A( LDA, * ), TAU( * ), WORK( * )
c
cc DORG2R generates an m by n real matrix Q with orthonormal columns,
*  which is defined as the first n columns of a product of k elementary
*  reflectors of order m
*
*        Q  =  H(1) H(2) . . . H(k)
*
*  as returned by DGEQRF.
*
*  Arguments
*  =========
*
*  M       (input) integer
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) integer
*          The number of columns of the matrix Q. M >= N >= 0.
*
*  K       (input) integer
*          The number of elementary reflectors whose product defines the
*          matrix Q. N >= K >= 0.
*
*  A       (input/output) double precision array, dimension (LDA,N)
*          On entry, the i-th column must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by DGEQRF in the first k columns of its array
*          argument A.
*          On exit, the m-by-n matrix Q.
*
*  LDA     (input) integer
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) double precision array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGEQRF.
*
*  WORK    (workspace) double precision array, dimension (N)
*
*  INFO    (output) integer
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      double precision   ONE, zero
      PARAMETER          ( ONE = 1.0D+0, zero = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      integer            I, J, L
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      if( M.LT.0 )then
         INFO = -1
      elseif( N.LT.0 .OR. N.GT.M )then
         INFO = -2
      elseif( K.LT.0 .OR. K.GT.N )then
         INFO = -3
      elseif( LDA.LT.MAX( 1, M ) )then
         INFO = -5
      endif
      if( INFO.NE.0 )then
         CALL XERBLA( 'DORG2R', -INFO )
         return
      endif
*
*     Quick return if possible
*
      if( N.LE.0 )
     $   return
*
*     Initialise columns k+1:n to columns of the unit matrix
*
      DO 20 J = K + 1, N
         DO 10 L = 1, M
            A( L, J ) = zero
   10    continue
         A( J, J ) = ONE
   20 continue
*
      DO 40 I = K, 1, -1
*
*        Apply H(i) to A(i:m,i:n) from the left
*
         if( I.LT.N )then
            A( I, I ) = ONE
            CALL DLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
         endif
         if( I.LT.M )
     $      CALL DSCAL( M-I, -TAU( I ), A( I+1, I ), 1 )
         A( I, I ) = ONE - TAU( I )
*
*        Set A(1:i-1,i) to zero
*
         DO 30 L = 1, I - 1
            A( L, I ) = zero
   30    continue
   40 continue
      return
*
*     End of DORG2R
*
      END
      subroutine dorgqr( m, n, k, a, lda, tau, work, lwork, info )
c 
c***********************************************************************
c
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      integer            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      double precision   A( LDA, * ), TAU( * ), WORK( LWORK )
c
cc DORGQR generates an M-by-N real matrix Q with orthonormal columns,
*  which is defined as the first N columns of a product of K elementary
*  reflectors of order M
*
*        Q  =  H(1) H(2) . . . H(k)
*
*  as returned by DGEQRF.
*
*  Arguments
*  =========
*
*  M       (input) integer
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) integer
*          The number of columns of the matrix Q. M >= N >= 0.
*
*  K       (input) integer
*          The number of elementary reflectors whose product defines the
*          matrix Q. N >= K >= 0.
*
*  A       (input/output) double precision array, dimension (LDA,N)
*          On entry, the i-th column must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by DGEQRF in the first k columns of its array
*          argument A.
*          On exit, the M-by-N matrix Q.
*
*  LDA     (input) integer
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) double precision array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGEQRF.
*
*  WORK    (workspace/output) double precision array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) integer
*          The dimension of the array WORK. LWORK >= max(1,N).
*          For optimum performance LWORK >= N*NB, where NB is the
*          optimal blocksize.
*
*  INFO    (output) integer
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      double precision   zero
      PARAMETER          ( zero = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      integer            I, IB, IINFO, IWS, J, KI, KK, L, LDWORK, NB,
     $                   NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARFB, DLARFT, DORG2R, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      integer            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      if( M.LT.0 )then
         INFO = -1
      elseif( N.LT.0 .OR. N.GT.M )then
         INFO = -2
      elseif( K.LT.0 .OR. K.GT.N )then
         INFO = -3
      elseif( LDA.LT.MAX( 1, M ) )then
         INFO = -5
      elseif( LWORK.LT.MAX( 1, N ) )then
         INFO = -8
      endif
      if( INFO.NE.0 )then
         CALL XERBLA( 'DORGQR', -INFO )
         return
      endif
*
*     Quick return if possible
*
      if( N.LE.0 )then
         WORK( 1 ) = 1
         return
      endif
*
*     Determine the block size.
*
      NB = ILAENV( 1, 'DORGQR', ' ', M, N, K, -1 )
      NBMIN = 2
      NX = 0
      IWS = N
      if( NB.GT.1 .and. NB.LT.K )then
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'DORGQR', ' ', M, N, K, -1 ) )
         if( NX.LT.K )then
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = N
            IWS = LDWORK*NB
            if( LWORK.LT.IWS )then
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'DORGQR', ' ', M, N, K, -1 ) )
            endif
         endif
      endif
*
      if( NB.ge.NBMIN .and. NB.LT.K .AND. NX.LT.K )then
*
*        Use blocked code after the last block.
*        The first kk columns are handled by the block method.
*
         KI = ( ( K-NX-1 ) / NB )*NB
         KK = MIN( K, KI+NB )
*
*        Set A(1:kk,kk+1:n) to zero.
*
         DO 20 J = KK + 1, N
            DO 10 I = 1, KK
               A( I, J ) = zero
   10       continue
   20    continue
      ELSE
         KK = 0
      endif
*
*     Use unblocked code for the last or only block.
*
      if( KK.LT.N )
     $   CALL DORG2R( M-KK, N-KK, K-KK, A( KK+1, KK+1 ), LDA,
     $                TAU( KK+1 ), WORK, IINFO )
*
      if( KK.GT.0 )then
*
*        Use blocked code
*
         DO 50 I = KI + 1, 1, -NB
            IB = MIN( NB, K-I+1 )
            if( I+IB.LE.N )then
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL DLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H to A(i:m,i+ib:n) from the left
*
               CALL DLARFB( 'Left', 'No transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            endif
*
*           Apply H to rows i:m of current block
*
            CALL DORG2R( M-I+1, IB, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
*
*           Set rows 1:i-1 of current block to zero
*
            DO 40 J = I, I + IB - 1
               DO 30 L = 1, I - 1
                  A( L, J ) = zero
   30          continue
   40       continue
   50    continue
      endif
*
      WORK( 1 ) = IWS
      return
*
*     End of DORGQR
*
      END
      subroutine dscal(n,da,dx,incx)
c 
c***********************************************************************
c
cc DSCAL scales a vector by a constant.
c
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision da,dx(*)
      integer i,incx,m,mp1,n,nincx
c
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end
      subroutine dswap(n,dx,incx,dy,incy)
c
c***********************************************************************
c
cc DSWAP interchanges two vectors.
c
      double precision dtemp
      double precision dx(*)
      double precision dy(*)
      integer i
      integer incx
      integer incy
      integer ix
      integer iy
      integer m
      integer n
c
      if(n.le.0)return

      if(incx.eq.1.and.incy.eq.1)go to 20
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1

      do i = 1,n
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
        ix = ix + incx
        iy = iy + incy
      enddo

      return
c
   20 m = mod(n,3)

      do i = 1,m
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
      enddo

      do i = m+1,n,3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
      enddo

      return
      end
      subroutine dtbsv(uplo,trans,diag,n,k,a,lda,x,incx)
c
c***********************************************************************
c
      integer incx, k, lda, n
      character*1        diag, trans, uplo
      double precision   a( lda, * ), x( * )
c
c  Purpose
c  =======
c
cc DTBSV  solves one of the systems of equations
c
c     A*x = b,   or   A'*x = b,
c
c  where b and x are n element vectors and A is an n by n unit, or
c  non-unit, upper or lower triangular band matrix, with ( k + 1 )
c  diagonals.
c
c  No test for singularity or near-singularity is included in this
c  routine. Such tests must be performed before calling this routine.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the matrix is an upper or
c           lower triangular matrix as follows:
c
c              UPLO = 'U' or 'u'   A is an upper triangular matrix.
c
c              UPLO = 'L' or 'l'   A is a lower triangular matrix.
c
c           Unchanged on exit.
c
c  TRANS  - CHARACTER*1.
c           On entry, TRANS specifies the equations to be solved as
c           follows:
c
c              TRANS = 'N' or 'n'   A*x = b.
c
c              TRANS = 'T' or 't'   A'*x = b.
c
c              TRANS = 'C' or 'c'   A'*x = b.
c
c           Unchanged on exit.
c
c  DIAG   - CHARACTER*1.
c           On entry, DIAG specifies whether or not A is unit
c           triangular as follows:
c
c              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
c
c              DIAG = 'N' or 'n'   A is not assumed to be unit
c                                  triangular.
c
c           Unchanged on exit.
c
c  N      - integer.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  K      - integer.
c           On entry with UPLO = 'U' or 'u', K specifies the number of
c           super-diagonals of the matrix A.
c           On entry with UPLO = 'L' or 'l', K specifies the number of
c           sub-diagonals of the matrix A.
c           K must satisfy  0 .le. K.
c           Unchanged on exit.
c
c  A      - double precision array of DIMENSION ( LDA, n ).
c           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
c           by n part of the array A must contain the upper triangular
c           band part of the matrix of coefficients, supplied column by
c           column, with the leading diagonal of the matrix in row
c           ( k + 1 ) of the array, the first super-diagonal starting at
c           position 2 in row k, and so on. The top left k by k triangle
c           of the array A is not referenced.
c           The following program segment will transfer an upper
c           triangular band matrix from conventional full matrix storage
c           to band storage:
c
c                 DO 20, J = 1, N
c                    M = K + 1 - J
c                    DO 10, I = MAX( 1, J - K ), J
c                       A( M + I, J ) = matrix( I, J )
c              10    continue
c              20 continue
c
c           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
c           by n part of the array A must contain the lower triangular
c           band part of the matrix of coefficients, supplied column by
c           column, with the leading diagonal of the matrix in row 1 of
c           the array, the first sub-diagonal starting at position 1 in
c           row 2, and so on. The bottom right k by k triangle of the
c           array A is not referenced.
c           The following program segment will transfer a lower
c           triangular band matrix from conventional full matrix storage
c           to band storage:
c
c                 DO 20, J = 1, N
c                    M = 1 - J
c                    DO 10, I = J, MIN( N, J + K )
c                       A( M + I, J ) = matrix( I, J )
c              10    continue
c              20 continue
c
c           Note that when DIAG = 'U' or 'u' the elements of the array A
c           corresponding to the diagonal elements of the matrix are not
c           referenced, but are assumed to be unity.
c           Unchanged on exit.
c
c  LDA    - integer.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           ( k + 1 ).
c           Unchanged on exit.
c
c  X      - double precision array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element right-hand side vector b. On exit, X is overwritten
c           with the solution vector x.
c
c  INCX   - integer.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      double precision   zero
      parameter        ( zero = 0.0d+0 )
c     .. Local Scalars ..
      double precision   temp
      integer            i, info, ix, j, jx, kplus1, kx, l
      logical            nounit
c     .. External Functions ..
      logical            lsame
      external           lsame
c     .. External Subroutines ..
      external           xerbla
c     .. Intrinsic Functions ..
      intrinsic          max, min
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      info = 0
      if     ( .not.lsame( uplo , 'u' ).and.
     $         .not.lsame( uplo , 'l' )      )then
         info = 1
      else if( .not.lsame( trans, 'n' ).and.
     $         .not.lsame( trans, 't' ).and.
     $         .not.lsame( trans, 'c' )      )then
         info = 2
      else if( .not.lsame( diag , 'u' ).and.
     $         .not.lsame( diag , 'n' )      )then
         info = 3
      else if( n.lt.0 )then
         info = 4
      else if( k.lt.0 )then
         info = 5
      else if( lda.lt.( k + 1 ) )then
         info = 7
      else if( incx.eq.0 )then
         info = 9
      end if
      if( info.ne.0 )then
         call xerbla( 'dtbsv ', info )
         return
      end if
c
c     Quick return if possible.
c
      if( n.eq.0 )
     $   return
c
      nounit = lsame( diag, 'n' )
c
c     Set up the start point in X if the increment is not unity. This
c     will be  ( N - 1 )*INCX  too small for descending loops.
c
      if( incx.le.0 )then
         kx = 1 - ( n - 1 )*incx
      else if( incx.ne.1 )then
         kx = 1
      end if
c
c     Start the operations. In this version the elements of A are
c     accessed by sequentially with one pass through A.
c
      if( lsame( trans, 'n' ) )then
c
c        Form  x := inv( A )*x.
c
         if( lsame( uplo, 'u' ) )then
            kplus1 = k + 1
            if( incx.eq.1 )then
               do 20, j = n, 1, -1
                  if( x( j ).ne.zero )then
                     l = kplus1 - j
                     if( nounit )
     $                  x( j ) = x( j )/a( kplus1, j )
                     temp = x( j )
                     do 10, i = j - 1, max( 1, j - k ), -1
                        x( i ) = x( i ) - temp*a( l + i, j )
   10                continue
                  end if
   20          continue
            else
               kx = kx + ( n - 1 )*incx
               jx = kx
               do 40, j = n, 1, -1
                  kx = kx - incx
                  if( x( jx ).ne.zero )then
                     ix = kx
                     l  = kplus1 - j
                     if( nounit )
     $                  x( jx ) = x( jx )/a( kplus1, j )
                     temp = x( jx )
                     do 30, i = j - 1, max( 1, j - k ), -1
                        x( ix ) = x( ix ) - temp*a( l + i, j )
                        ix      = ix      - incx
   30                continue
                  end if
                  jx = jx - incx
   40          continue
            end if
         else
            if( incx.eq.1 )then
               do 60, j = 1, n
                  if( x( j ).ne.zero )then
                     l = 1 - j
                     if( nounit )
     $                  x( j ) = x( j )/a( 1, j )
                     temp = x( j )
                     do 50, i = j + 1, min( n, j + k )
                        x( i ) = x( i ) - temp*a( l + i, j )
   50                continue
                  end if
   60          continue
            else
               jx = kx
               do 80, j = 1, n
                  kx = kx + incx
                  if( x( jx ).ne.zero )then
                     ix = kx
                     l  = 1  - j
                     if( nounit )
     $                  x( jx ) = x( jx )/a( 1, j )
                     temp = x( jx )
                     do 70, i = j + 1, min( n, j + k )
                        x( ix ) = x( ix ) - temp*a( l + i, j )
                        ix      = ix      + incx
   70                continue
                  end if
                  jx = jx + incx
   80          continue
            end if
         end if
      else
c
c        Form  x := inv( A')*x.
c
         if( lsame( uplo, 'u' ) )then
            kplus1 = k + 1
            if( incx.eq.1 )then
               do 100, j = 1, n
                  temp = x( j )
                  l    = kplus1 - j
                  do 90, i = max( 1, j - k ), j - 1
                     temp = temp - a( l + i, j )*x( i )
   90             continue
                  if( nounit )
     $               temp = temp/a( kplus1, j )
                  x( j ) = temp
  100          continue
            else
               jx = kx
               do 120, j = 1, n
                  temp = x( jx )
                  ix   = kx
                  l    = kplus1  - j
                  do 110, i = max( 1, j - k ), j - 1
                     temp = temp - a( l + i, j )*x( ix )
                     ix   = ix   + incx
  110             continue
                  if( nounit )
     $               temp = temp/a( kplus1, j )
                  x( jx ) = temp
                  jx      = jx   + incx
                  if( j.gt.k )
     $               kx = kx + incx
  120          continue
            end if
         else
            if( incx.eq.1 )then
               do 140, j = n, 1, -1
                  temp = x( j )
                  l    = 1      - j
                  do 130, i = min( n, j + k ), j + 1, -1
                     temp = temp - a( l + i, j )*x( i )
  130             continue
                  if( nounit )
     $               temp = temp/a( 1, j )
                  x( j ) = temp
  140          continue
            else
               kx = kx + ( n - 1 )*incx
               jx = kx
               do 160, j = n, 1, -1
                  temp = x( jx )
                  ix   = kx
                  l    = 1       - j
                  do 150, i = min( n, j + k ), j + 1, -1
                     temp = temp - a( l + i, j )*x( ix )
                     ix   = ix   - incx
  150             continue
                  if( nounit )
     $               temp = temp/a( 1, j )
                  x( jx ) = temp
                  jx      = jx   - incx
                  if( ( n - j ).ge.k )
     $               kx = kx - incx
  160          continue
            end if
         end if
      end if
c
      return
c
c     End of DTBSV .
c
      end
      subroutine dtrmm(side, uplo, transa, diag, m, n, alpha, a, lda,
     $                   b, ldb )
c 
c***********************************************************************
c
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      integer            M, N, LDA, LDB
      double precision   ALPHA
*     .. Array Arguments ..
      double precision   A( LDA, * ), B( LDB, * )
c
cc DTRMM  performs one of the matrix-matrix operations
*
*     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
*
*  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*
*     op( A ) = A   or   op( A ) = A'.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry,  SIDE specifies whether  op( A ) multiplies B from
*           the left or right as follows:
*
*              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
*
*              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
*
*           Unchanged on exit.
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix A is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n'   op( A ) = A.
*
*              TRANSA = 'T' or 't'   op( A ) = A'.
*
*              TRANSA = 'C' or 'c'   op( A ) = A'.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit triangular
*           as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  M      - integer.
*           On entry, M specifies the number of rows of B. M must be at
*           least zero.
*           Unchanged on exit.
*
*  N      - integer.
*           On entry, N specifies the number of columns of B.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  ALPHA  - double precision.
*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*           zero then  A is not referenced and  B need not be set before
*           entry.
*           Unchanged on exit.
*
*  A      - double precision array of DIMENSION ( LDA, k ), where k is m
*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
*           upper triangular part of the array  A must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           A is not referenced.
*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
*           lower triangular part of the array  A must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
*           A  are not referenced either,  but are assumed to be  unity.
*           Unchanged on exit.
*
*  LDA    - integer.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
*           then LDA must be at least max( 1, n ).
*           Unchanged on exit.
*
*  B      - double precision array of DIMENSION ( LDB, n ).
*           Before entry,  the leading  m by n part of the array  B must
*           contain the matrix  B,  and  on exit  is overwritten  by the
*           transformed matrix.
*
*  LDB    - integer.
*           On entry, LDB specifies the first dimension of B as declared
*           in  the  calling  (sub)  program.   LDB  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      integer            I, INFO, J, K, NROWA
      double precision   TEMP
*     .. Parameters ..
      double precision   ONE         , zero
      PARAMETER        ( ONE = 1.0D+0, zero = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE  = LSAME( SIDE  , 'L' )
      if( LSIDE )then
         NROWA = M
      ELSE
         NROWA = N
      endif
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
*
      INFO   = 0
      if(      ( .NOT.LSIDE                ).and.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )then
         INFO = 1
      elseif( ( .NOT.UPPER                ).and.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )then
         INFO = 2
      elseif( ( .NOT.LSAME( TRANSA, 'N' ) ).and.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).and.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )then
         INFO = 3
      elseif( ( .NOT.LSAME( DIAG  , 'U' ) ).and.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )then
         INFO = 4
      elseif( M  .LT.0               )then
         INFO = 5
      elseif( N  .LT.0               )then
         INFO = 6
      elseif( LDA.LT.MAX( 1, NROWA ) )then
         INFO = 9
      elseif( LDB.LT.MAX( 1, M     ) )then
         INFO = 11
      endif
      if( INFO.NE.0 )then
         CALL XERBLA( 'DTRMM ', INFO )
         return
      endif
*
*     Quick return if possible.
*
      if( N.eq.0 )
     $   return
*
*     And when  alpha.eq.zero.
*
      if( ALPHA.eq.zero )then
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = zero
   10       continue
   20    continue
         return
      endif
*
*     Start the operations.
*
      if( LSIDE )then
         if( LSAME( TRANSA, 'N' ) )then
*
*           Form  B := alpha*A*B.
*
            if( UPPER )then
               DO 50, J = 1, N
                  DO 40, K = 1, M
                     if( B( K, J ).NE.zero )then
                        TEMP = ALPHA*B( K, J )
                        DO 30, I = 1, K - 1
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   30                   continue
                        if( NOUNIT )
     $                     TEMP = TEMP*A( K, K )
                        B( K, J ) = TEMP
                     endif
   40             continue
   50          continue
            ELSE
               DO 80, J = 1, N
                  DO 70 K = M, 1, -1
                     if( B( K, J ).NE.zero )then
                        TEMP      = ALPHA*B( K, J )
                        B( K, J ) = TEMP
                        if( NOUNIT )
     $                     B( K, J ) = B( K, J )*A( K, K )
                        DO 60, I = K + 1, M
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   60                   continue
                     endif
   70             continue
   80          continue
            endif
         ELSE
*
*           Form  B := alpha*B*A'.
*
            if( UPPER )then
               DO 110, J = 1, N
                  DO 100, I = M, 1, -1
                     TEMP = B( I, J )
                     if( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
                     DO 90, K = 1, I - 1
                        TEMP = TEMP + A( K, I )*B( K, J )
   90                continue
                     B( I, J ) = ALPHA*TEMP
  100             continue
  110          continue
            ELSE
               DO 140, J = 1, N
                  DO 130, I = 1, M
                     TEMP = B( I, J )
                     if( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
                     DO 120, K = I + 1, M
                        TEMP = TEMP + A( K, I )*B( K, J )
  120                continue
                     B( I, J ) = ALPHA*TEMP
  130             continue
  140          continue
            endif
         endif
      ELSE
         if( LSAME( TRANSA, 'N' ) )then
*
*           Form  B := alpha*B*A.
*
            if( UPPER )then
               DO 180, J = N, 1, -1
                  TEMP = ALPHA
                  if( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 150, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  150             continue
                  DO 170, K = 1, J - 1
                     if( A( K, J ).NE.zero )then
                        TEMP = ALPHA*A( K, J )
                        DO 160, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  160                   continue
                     endif
  170             continue
  180          continue
            ELSE
               DO 220, J = 1, N
                  TEMP = ALPHA
                  if( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 190, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  190             continue
                  DO 210, K = J + 1, N
                     if( A( K, J ).NE.zero )then
                        TEMP = ALPHA*A( K, J )
                        DO 200, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  200                   continue
                     endif
  210             continue
  220          continue
            endif
         ELSE
*
*           Form  B := alpha*B*A'.
*
            if( UPPER )then
               DO 260, K = 1, N
                  DO 240, J = 1, K - 1
                     if( A( J, K ).NE.zero )then
                        TEMP = ALPHA*A( J, K )
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  230                   continue
                     endif
  240             continue
                  TEMP = ALPHA
                  if( NOUNIT )
     $               TEMP = TEMP*A( K, K )
                  if( TEMP.NE.ONE )then
                     DO 250, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  250                continue
                  endif
  260          continue
            ELSE
               DO 300, K = N, 1, -1
                  DO 280, J = K + 1, N
                     if( A( J, K ).NE.zero )then
                        TEMP = ALPHA*A( J, K )
                        DO 270, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  270                   continue
                     endif
  280             continue
                  TEMP = ALPHA
                  if( NOUNIT )
     $               TEMP = TEMP*A( K, K )
                  if( TEMP.NE.ONE )then
                     DO 290, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  290                continue
                  endif
  300          continue
            endif
         endif
      endif
*
      return
*
*     End of DTRMM .
*
      END
      subroutine dtrmv(uplo, trans, diag, n, a, lda, x, incx )
c 
c***********************************************************************
c
*     .. Scalar Arguments ..
      integer            INCX, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
*     .. Array Arguments ..
      double precision   A( LDA, * ), X( * )
c
cc DTRMV  performs one of the matrix-vector operations
*
*     x := A*x,   or   x := A'*x,
*
*  where x is an n element vector and  A is an n by n unit, or non-unit,
*  upper or lower triangular matrix.
*
*  Parameters
*  ==========
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   x := A*x.
*
*              TRANS = 'T' or 't'   x := A'*x.
*
*              TRANS = 'C' or 'c'   x := A'*x.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit
*           triangular as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  N      - integer.
*           On entry, N specifies the order of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  A      - double precision array of DIMENSION ( LDA, n ).
*           Before entry with  UPLO = 'U' or 'u', the leading n by n
*           upper triangular part of the array A must contain the upper
*           triangular matrix and the strictly lower triangular part of
*           A is not referenced.
*           Before entry with UPLO = 'L' or 'l', the leading n by n
*           lower triangular part of the array A must contain the lower
*           triangular matrix and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u', the diagonal elements of
*           A are not referenced either, but are assumed to be unity.
*           Unchanged on exit.
*
*  LDA    - integer.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, n ).
*           Unchanged on exit.
*
*  X      - double precision array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the n
*           element vector x. On exit, X is overwritten with the
*           tranformed vector x.
*
*  INCX   - integer.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      double precision   zero
      PARAMETER        ( zero = 0.0D+0 )
*     .. Local Scalars ..
      double precision   TEMP
      integer            I, INFO, IX, J, JX, KX
      LOGICAL            NOUNIT
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).and.
     $         .NOT.LSAME( UPLO , 'L' )      )then
         INFO = 1
      elseif( .NOT.LSAME( TRANS, 'N' ).and.
     $         .NOT.LSAME( TRANS, 'T' ).and.
     $         .NOT.LSAME( TRANS, 'C' )      )then
         INFO = 2
      elseif( .NOT.LSAME( DIAG , 'U' ).and.
     $         .NOT.LSAME( DIAG , 'N' )      )then
         INFO = 3
      elseif( N.LT.0 )then
         INFO = 4
      elseif( LDA.LT.MAX( 1, N ) )then
         INFO = 6
      elseif( INCX.eq.0 )then
         INFO = 8
      endif
      if( INFO.NE.0 )then
         CALL XERBLA( 'DTRMV ', INFO )
         return
      endif
*
*     Quick return if possible.
*
      if( N.eq.0 )
     $   return
*
      NOUNIT = LSAME( DIAG, 'N' )
*
*     Set up the start point in X if the increment is not unity. This
*     will be  ( N - 1 )*INCX  too small for descending loops.
*
      if( INCX.LE.0 )then
         KX = 1 - ( N - 1 )*INCX
      elseif( INCX.NE.1 )then
         KX = 1
      endif
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
      if( LSAME( TRANS, 'N' ) )then
*
*        Form  x := A*x.
*
         if( LSAME( UPLO, 'U' ) )then
            if( INCX.eq.1 )then
               DO 20, J = 1, N
                  if( X( J ).NE.zero )then
                     TEMP = X( J )
                     DO 10, I = 1, J - 1
                        X( I ) = X( I ) + TEMP*A( I, J )
   10                continue
                     if( NOUNIT )
     $                  X( J ) = X( J )*A( J, J )
                  endif
   20          continue
            ELSE
               JX = KX
               DO 40, J = 1, N
                  if( X( JX ).NE.zero )then
                     TEMP = X( JX )
                     IX   = KX
                     DO 30, I = 1, J - 1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      + INCX
   30                continue
                     if( NOUNIT )
     $                  X( JX ) = X( JX )*A( J, J )
                  endif
                  JX = JX + INCX
   40          continue
            endif
         ELSE
            if( INCX.eq.1 )then
               DO 60, J = N, 1, -1
                  if( X( J ).NE.zero )then
                     TEMP = X( J )
                     DO 50, I = N, J + 1, -1
                        X( I ) = X( I ) + TEMP*A( I, J )
   50                continue
                     if( NOUNIT )
     $                  X( J ) = X( J )*A( J, J )
                  endif
   60          continue
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 80, J = N, 1, -1
                  if( X( JX ).NE.zero )then
                     TEMP = X( JX )
                     IX   = KX
                     DO 70, I = N, J + 1, -1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      - INCX
   70                continue
                     if( NOUNIT )
     $                  X( JX ) = X( JX )*A( J, J )
                  endif
                  JX = JX - INCX
   80          continue
            endif
         endif
      ELSE
*
*        Form  x := A'*x.
*
         if( LSAME( UPLO, 'U' ) )then
            if( INCX.eq.1 )then
               DO 100, J = N, 1, -1
                  TEMP = X( J )
                  if( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 90, I = J - 1, 1, -1
                     TEMP = TEMP + A( I, J )*X( I )
   90             continue
                  X( J ) = TEMP
  100          continue
            ELSE
               JX = KX + ( N - 1 )*INCX
               DO 120, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = JX
                  if( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 110, I = J - 1, 1, -1
                     IX   = IX   - INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  110             continue
                  X( JX ) = TEMP
                  JX      = JX   - INCX
  120          continue
            endif
         ELSE
            if( INCX.eq.1 )then
               DO 140, J = 1, N
                  TEMP = X( J )
                  if( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 130, I = J + 1, N
                     TEMP = TEMP + A( I, J )*X( I )
  130             continue
                  X( J ) = TEMP
  140          continue
            ELSE
               JX = KX
               DO 160, J = 1, N
                  TEMP = X( JX )
                  IX   = JX
                  if( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 150, I = J + 1, N
                     IX   = IX   + INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  150             continue
                  X( JX ) = TEMP
                  JX      = JX   + INCX
  160          continue
            endif
         endif
      endif
*
      return
*
*     End of DTRMV .
*
      END
      subroutine dtrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
c
c***********************************************************************
c
      character*1        side, uplo, transa, diag
      integer            m, n, lda, ldb
      double precision   alpha
      double precision   a( lda, * ), b( ldb, * )
c
cc DTRSM  solves one of the matrix equations
c
c     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
c
c  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
c  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
c
c     op( A ) = A   or   op( A ) = A'.
c
c  The matrix X is overwritten on B.
c
c  Parameters
c  ==========
c
c  SIDE   - CHARACTER*1.
c           On entry, SIDE specifies whether op( A ) appears on the left
c           or right of X as follows:
c
c              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
c
c              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
c
c           Unchanged on exit.
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the matrix A is an upper or
c           lower triangular matrix as follows:
c
c              UPLO = 'U' or 'u'   A is an upper triangular matrix.
c
c              UPLO = 'L' or 'l'   A is a lower triangular matrix.
c
c           Unchanged on exit.
c
c  TRANSA - CHARACTER*1.
c           On entry, TRANSA specifies the form of op( A ) to be used in
c           the matrix multiplication as follows:
c
c              TRANSA = 'N' or 'n'   op( A ) = A.
c
c              TRANSA = 'T' or 't'   op( A ) = A'.
c
c              TRANSA = 'C' or 'c'   op( A ) = A'.
c
c           Unchanged on exit.
c
c  DIAG   - CHARACTER*1.
c           On entry, DIAG specifies whether or not A is unit triangular
c           as follows:
c
c              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
c
c              DIAG = 'N' or 'n'   A is not assumed to be unit
c                                  triangular.
c
c           Unchanged on exit.
c
c  M      - integer.
c           On entry, M specifies the number of rows of B. M must be at
c           least zero.
c           Unchanged on exit.
c
c  N      - integer.
c           On entry, N specifies the number of columns of B.  N must be
c           at least zero.
c           Unchanged on exit.
c
c  ALPHA  - double precision.
c           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
c           zero then  A is not referenced and  B need not be set before
c           entry.
c           Unchanged on exit.
c
c  A      - double precision array of DIMENSION ( LDA, k ), where k is m
c           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
c           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
c           upper triangular part of the array  A must contain the upper
c           triangular matrix  and the strictly lower triangular part of
c           A is not referenced.
c           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
c           lower triangular part of the array  A must contain the lower
c           triangular matrix  and the strictly upper triangular part of
c           A is not referenced.
c           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
c           A  are not referenced either,  but are assumed to be  unity.
c           Unchanged on exit.
c
c  LDA    - integer.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
c           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
c           then LDA must be at least max( 1, n ).
c           Unchanged on exit.
c
c  B      - double precision array of DIMENSION ( LDB, n ).
c           Before entry,  the leading  m by n part of the array  B must
c           contain  the  right-hand  side  matrix  B,  and  on exit  is
c           overwritten by the solution matrix  X.
c
c  LDB    - integer.
c           On entry, LDB specifies the first dimension of B as declared
c           in  the  calling  (sub)  program.   LDB  must  be  at  least
c           max( 1, m ).
c           Unchanged on exit.
c
c
c  Level 3 Blas routine.
c
c
c  -- Written on 8-February-1989.
c     Jack Dongarra, Argonne National Laboratory.
c     Iain Duff, AERE Harwell.
c     Jeremy Du Croz, Numerical Algorithms Group Ltd.
c     Sven Hammarling, Numerical Algorithms Group Ltd.
c
c
c     .. External Functions ..
      logical            lsame
      external           lsame
c     .. External Subroutines ..
      external           xerbla
c     .. Intrinsic Functions ..
      intrinsic          max
c     .. Local Scalars ..
      logical            lside, nounit, upper
      integer            i, info, j, k, nrowa
      double precision   temp
c     .. Parameters ..
      double precision   one         , zero
      parameter        ( one = 1.0d+0, zero = 0.0d+0 )
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      lside  = lsame( side  , 'l' )
      if( lside )then
         nrowa = m
      else
         nrowa = n
      end if
      nounit = lsame( diag  , 'n' )
      upper  = lsame( uplo  , 'u' )
c
      info   = 0
      if(      ( .not.lside                ).and.
     $         ( .not.lsame( side  , 'r' ) )      )then
         info = 1
      else if( ( .not.upper                ).and.
     $         ( .not.lsame( uplo  , 'l' ) )      )then
         info = 2
      else if( ( .not.lsame( transa, 'n' ) ).and.
     $         ( .not.lsame( transa, 't' ) ).and.
     $         ( .not.lsame( transa, 'c' ) )      )then
         info = 3
      else if( ( .not.lsame( diag  , 'u' ) ).and.
     $         ( .not.lsame( diag  , 'n' ) )      )then
         info = 4
      else if( m  .lt.0               )then
         info = 5
      else if( n  .lt.0               )then
         info = 6
      else if( lda.lt.max( 1, nrowa ) )then
         info = 9
      else if( ldb.lt.max( 1, m     ) )then
         info = 11
      end if
      if( info.ne.0 )then
         call xerbla( 'dtrsm ', info )
         return
      end if
c
c     Quick return if possible.
c
      if( n.eq.0 )
     $   return
c
c     And when  alpha.eq.zero.
c
      if( alpha.eq.zero )then
         do 20, j = 1, n
            do 10, i = 1, m
               b( i, j ) = zero
   10       continue
   20    continue
         return
      end if
c
c     Start the operations.
c
      if( lside )then
         if( lsame( transa, 'n' ) )then
c
c           Form  B := alpha*inv( A )*B.
c
            if( upper )then
               do 60, j = 1, n
                  if( alpha.ne.one )then
                     do 30, i = 1, m
                        b( i, j ) = alpha*b( i, j )
   30                continue
                  end if
                  do 50, k = m, 1, -1
                     if( b( k, j ).ne.zero )then
                        if( nounit )
     $                     b( k, j ) = b( k, j )/a( k, k )
                        do 40, i = 1, k - 1
                           b( i, j ) = b( i, j ) - b( k, j )*a( i, k )
   40                   continue
                     end if
   50             continue
   60          continue
            else
               do 100, j = 1, n
                  if( alpha.ne.one )then
                     do 70, i = 1, m
                        b( i, j ) = alpha*b( i, j )
   70                continue
                  end if
                  do 90 k = 1, m
                     if( b( k, j ).ne.zero )then
                        if( nounit )
     $                     b( k, j ) = b( k, j )/a( k, k )
                        do 80, i = k + 1, m
                           b( i, j ) = b( i, j ) - b( k, j )*a( i, k )
   80                   continue
                     end if
   90             continue
  100          continue
            end if
         else
c
c           Form  B := alpha*inv( A' )*B.
c
            if( upper )then
               do 130, j = 1, n
                  do 120, i = 1, m
                     temp = alpha*b( i, j )
                     do 110, k = 1, i - 1
                        temp = temp - a( k, i )*b( k, j )
  110                continue
                     if( nounit )
     $                  temp = temp/a( i, i )
                     b( i, j ) = temp
  120             continue
  130          continue
            else
               do 160, j = 1, n
                  do 150, i = m, 1, -1
                     temp = alpha*b( i, j )
                     do 140, k = i + 1, m
                        temp = temp - a( k, i )*b( k, j )
  140                continue
                     if( nounit )
     $                  temp = temp/a( i, i )
                     b( i, j ) = temp
  150             continue
  160          continue
            end if
         end if
      else
         if( lsame( transa, 'n' ) )then
c
c           Form  B := alpha*B*inv( A ).
c
            if( upper )then
               do 210, j = 1, n
                  if( alpha.ne.one )then
                     do 170, i = 1, m
                        b( i, j ) = alpha*b( i, j )
  170                continue
                  end if
                  do 190, k = 1, j - 1
                     if( a( k, j ).ne.zero )then
                        do 180, i = 1, m
                           b( i, j ) = b( i, j ) - a( k, j )*b( i, k )
  180                   continue
                     end if
  190             continue
                  if( nounit )then
                     temp = one/a( j, j )
                     do 200, i = 1, m
                        b( i, j ) = temp*b( i, j )
  200                continue
                  end if
  210          continue
            else
               do 260, j = n, 1, -1
                  if( alpha.ne.one )then
                     do 220, i = 1, m
                        b( i, j ) = alpha*b( i, j )
  220                continue
                  end if
                  do 240, k = j + 1, n
                     if( a( k, j ).ne.zero )then
                        do 230, i = 1, m
                           b( i, j ) = b( i, j ) - a( k, j )*b( i, k )
  230                   continue
                     end if
  240             continue
                  if( nounit )then
                     temp = one/a( j, j )
                     do 250, i = 1, m
                       b( i, j ) = temp*b( i, j )
  250                continue
                  end if
  260          continue
            end if
         else
c
c           Form  B := alpha*B*inv( A' ).
c
            if( upper )then
               do 310, k = n, 1, -1
                  if( nounit )then
                     temp = one/a( k, k )
                     do 270, i = 1, m
                        b( i, k ) = temp*b( i, k )
  270                continue
                  end if
                  do 290, j = 1, k - 1
                     if( a( j, k ).ne.zero )then
                        temp = a( j, k )
                        do 280, i = 1, m
                           b( i, j ) = b( i, j ) - temp*b( i, k )
  280                   continue
                     end if
  290             continue
                  if( alpha.ne.one )then
                     do 300, i = 1, m
                        b( i, k ) = alpha*b( i, k )
  300                continue
                  end if
  310          continue
            else
               do 360, k = 1, n
                  if( nounit )then
                     temp = one/a( k, k )
                     do 320, i = 1, m
                        b( i, k ) = temp*b( i, k )
  320                continue
                  end if
                  do 340, j = k + 1, n
                     if( a( j, k ).ne.zero )then
                        temp = a( j, k )
                        do 330, i = 1, m
                           b( i, j ) = b( i, j ) - temp*b( i, k )
  330                   continue
                     end if
  340             continue
                  if( alpha.ne.one )then
                     do 350, i = 1, m
                        b( i, k ) = alpha*b( i, k )
  350                continue
                  end if
  360          continue
            end if
         end if
      end if
c
      return
      end
      function idamax(n,dx,incx)
c
c***********************************************************************
c
cc IDAMAX FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
c
      double precision dmax
      double precision dx(*)
      integer i
      integer idamax
      integer incx
      integer ix
      integer n
c
      idamax = 0

      if( n .lt. 1 ) return

      idamax = 1
      if(n.eq.1)return

      if(incx.eq.1)go to 20
c
      ix = 1
      dmax=abs(dx(1))
      ix = ix + incx

      do i=2,n

        if(abs(dx(ix)).gt.dmax)then
          idamax = i
          dmax = abs(dx(ix))
          ix = ix + incx
        endif

      enddo

      return
c
   20 dmax=abs(dx(1))

      do i=2,n
        if(abs(dx(i)).gt.dmax)then
          idamax=i
          dmax=abs(dx(i))
        endif
      enddo

      return
      end
      integer function ilaenv( ispec, name, opts, n1, n2, n3, n4 )
c 
c***********************************************************************
c
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    NAME, OPTS
      integer            ISPEC, N1, N2, N3, N4
*     ..
cc ILAENV is called from the LAPACK routines to choose problem-dependent
*  parameters for the local environment.  See ISPEC for a description of
*  the parameters.
*
*  This version provides a set of parameters which should give good,
*  but not optimal, performance on many of the currently available
*  computers.  Users are encouraged to modify this subroutine to set
*  the tuning parameters for their particular machine using the option
*  and problem size information in the arguments.
*
*  This routine will not function correctly if it is converted to all
*  lower case.  Converting it to all upper case is allowed.
*
*  Arguments
*  =========
*
*  ISPEC   (input) integer
*          Specifies the parameter to be returned as the value of
*          ILAENV.
*          = 1: the optimal blocksize; if this value is 1, an unblocked
*               algorithm will give the best performance.
*          = 2: the minimum block size for which the block routine
*               should be used; if the usable block size is less than
*               this value, an unblocked routine should be used.
*          = 3: the crossover point (in a block routine, for N less
*               than this value, an unblocked routine should be used)
*          = 4: the number of shifts, used in the nonsymmetric
*               eigenvalue routines
*          = 5: the minimum column dimension for blocking to be used;
*               rectangular blocks must have dimension at least k by m,
*               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
*          = 6: the crossover point for the SVD (when reducing an m by n
*               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
*               this value, a QR factorization is used first to reduce
*               the matrix to a triangular form.)
*          = 7: the number of processors
*          = 8: the crossover point for the multishift QR and QZ methods
*               for nonsymmetric eigenvalue problems.
*
*  NAME    (input) CHARACTER*(*)
*          The name of the calling subroutine, in either upper case or
*          lower case.
*
*  OPTS    (input) CHARACTER*(*)
*          The character options to the subroutine NAME, concatenated
*          into a single character string.  For example, UPLO = 'U',
*          TRANS = 'T', and DIAG = 'N' for a triangular routine would
*          be specified as OPTS = 'UTN'.
*
*  N1      (input) integer
*  N2      (input) integer
*  N3      (input) integer
*  N4      (input) integer
*          Problem dimensions for the subroutine NAME; these may not all
*          be required.
*
* (ILAENV) (output) integer
*          >= 0: the value of the parameter specified by ISPEC
*          < 0:  if ILAENV = -k, the k-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The following conventions have been used when calling ILAENV from the
*  LAPACK routines:
*  1)  OPTS is a concatenation of all of the character options to
*      subroutine NAME, in the same order that they appear in the
*      argument list for NAME, even if they are not used in determining
*      the value of the parameter specified by ISPEC.
*  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
*      that they appear in the argument list for NAME.  N1 is used
*      first, N2 second, and so on, and unused problem dimensions are
*      passed a value of -1.
*  3)  The parameter value returned by ILAENV is checked for validity in
*      the calling subroutine.  For example, ILAENV is used to retrieve
*      the optimal blocksize for STRTRI as follows:
*
*      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
*      if( NB.LE.1 ) NB = MAX( 1, N )
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            CNAME, SNAME
      CHARACTER*1        C1
      CHARACTER*2        C2, C4
      CHARACTER*3        C3
      CHARACTER*6        SUBNAM
      integer            I, IC, IZ, NB, NBMIN, NX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CHAR, ICHAR, INT, MIN, REAL
*     ..
*     .. Executable Statements ..
*
      go to ( 100, 100, 100, 400, 500, 600, 700, 800 ) ISPEC
*
*     Invalid value for ISPEC
*
      ILAENV = -1
      return
*
  100 continue
*
*     Convert NAME to upper case if the first character is lower case.
*
      ILAENV = 1
      SUBNAM = NAME
      IC = ICHAR( SUBNAM( 1:1 ) )
      IZ = ICHAR( 'Z' )
      if( IZ.eq.90 .OR. IZ.EQ.122 )then
*
*        ASCII character set
*
         if( IC.ge.97 .and. IC.LE.122 )then
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 10 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               if( IC.ge.97 .and. IC.LE.122 )
     $            SUBNAM( I:I ) = CHAR( IC-32 )
   10       continue
         endif
*
      elseif( IZ.eq.233 .OR. IZ.EQ.169 )then
*
*        EBCDIC character set
*
         if( ( IC.ge.129 .and. IC.LE.137 ) .OR.
     $       ( IC.ge.145 .and. IC.LE.153 ) .OR.
     $       ( IC.ge.162 .and. IC.LE.169 ) )then
            SUBNAM( 1:1 ) = CHAR( IC+64 )
            DO 20 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               if( ( IC.ge.129 .and. IC.LE.137 ) .OR.
     $             ( IC.ge.145 .and. IC.LE.153 ) .OR.
     $             ( IC.ge.162 .and. IC.LE.169 ) )
     $            SUBNAM( I:I ) = CHAR( IC+64 )
   20       continue
         endif
*
      elseif( IZ.eq.218 .OR. IZ.EQ.250 )then
*
*        Prime machines:  ASCII+128
*
         if( IC.ge.225 .and. IC.LE.250 )then
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 30 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               if( IC.ge.225 .and. IC.LE.250 )
     $            SUBNAM( I:I ) = CHAR( IC-32 )
   30       continue
         endif
      endif
*
      C1 = SUBNAM( 1:1 )
      SNAME = C1.eq.'S' .OR. C1.EQ.'D'
      CNAME = C1.eq.'C' .OR. C1.EQ.'Z'
      if( .NOT.( CNAME .OR. SNAME ) )
     $   return
      C2 = SUBNAM( 2:3 )
      C3 = SUBNAM( 4:6 )
      C4 = C3( 2:3 )
*
      go to ( 110, 200, 300 ) ISPEC
*
  110 continue
*
*     ISPEC = 1:  block size
*
*     In these examples, separate code is provided for setting NB for
*     real and complex.  We assume that NB will take the same value in
*     single or double precision.
*
      NB = 1
*
      if( C2.eq.'GE' )then
         if( C3.eq.'TRF' )then
            if( SNAME )then
               NB = 64
            ELSE
               NB = 64
            endif
         elseif( C3.eq.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $            C3.eq.'QLF' )then
            if( SNAME )then
               NB = 32
            ELSE
               NB = 32
            endif
         elseif( C3.eq.'HRD' )then
            if( SNAME )then
               NB = 32
            ELSE
               NB = 32
            endif
         elseif( C3.eq.'BRD' )then
            if( SNAME )then
               NB = 32
            ELSE
               NB = 32
            endif
         elseif( C3.eq.'TRI' )then
            if( SNAME )then
               NB = 64
            ELSE
               NB = 64
            endif
         endif
      elseif( C2.eq.'PO' )then
         if( C3.eq.'TRF' )then
            if( SNAME )then
               NB = 64
            ELSE
               NB = 64
            endif
         endif
      elseif( C2.eq.'SY' )then
         if( C3.eq.'TRF' )then
            if( SNAME )then
               NB = 64
            ELSE
               NB = 64
            endif
         elseif( SNAME .and. C3.eq.'TRD' )then
            NB = 1
         elseif( SNAME .and. C3.eq.'GST' )then
            NB = 64
         endif
      elseif( CNAME .and. C2.eq.'HE' )then
         if( C3.eq.'TRF' )then
            NB = 64
         elseif( C3.eq.'TRD' )then
            NB = 1
         elseif( C3.eq.'GST' )then
            NB = 64
         endif
      elseif( SNAME .and. C2.eq.'OR' )then
         if( C3( 1:1 ).eq.'G' )then
            if( C4.eq.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.eq.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.eq.'BR' )then
               NB = 32
            endif
         elseif( C3( 1:1 ).eq.'M' )then
            if( C4.eq.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.eq.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.eq.'BR' )then
               NB = 32
            endif
         endif
      elseif( CNAME .and. C2.eq.'UN' )then
         if( C3( 1:1 ).eq.'G' )then
            if( C4.eq.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.eq.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.eq.'BR' )then
               NB = 32
            endif
         elseif( C3( 1:1 ).eq.'M' )then
            if( C4.eq.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.eq.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.eq.'BR' )then
               NB = 32
            endif
         endif
      elseif( C2.eq.'GB' )then
         if( C3.eq.'TRF' )then
            if( SNAME )then
               if( N4.LE.64 )then
                  NB = 1
               ELSE
                  NB = 32
               endif
            ELSE
               if( N4.LE.64 )then
                  NB = 1
               ELSE
                  NB = 32
               endif
            endif
         endif
      elseif( C2.eq.'PB' )then
         if( C3.eq.'TRF' )then
            if( SNAME )then
               if( N2.LE.64 )then
                  NB = 1
               ELSE
                  NB = 32
               endif
            ELSE
               if( N2.LE.64 )then
                  NB = 1
               ELSE
                  NB = 32
               endif
            endif
         endif
      elseif( C2.eq.'TR' )then
         if( C3.eq.'TRI' )then
            if( SNAME )then
               NB = 64
            ELSE
               NB = 64
            endif
         endif
      elseif( C2.eq.'LA' )then
         if( C3.eq.'UUM' )then
            if( SNAME )then
               NB = 64
            ELSE
               NB = 64
            endif
         endif
      elseif( SNAME .and. C2.eq.'ST' )then
         if( C3.eq.'EBZ' )then
            NB = 1
         endif
      endif
      ILAENV = NB
      return
*
  200 continue
*
*     ISPEC = 2:  minimum block size
*
      NBMIN = 2
      if( C2.eq.'GE' )then
         if( C3.eq.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $       C3.eq.'QLF' )then
            if( SNAME )then
               NBMIN = 2
            ELSE
               NBMIN = 2
            endif
         elseif( C3.eq.'HRD' )then
            if( SNAME )then
               NBMIN = 2
            ELSE
               NBMIN = 2
            endif
         elseif( C3.eq.'BRD' )then
            if( SNAME )then
               NBMIN = 2
            ELSE
               NBMIN = 2
            endif
         elseif( C3.eq.'TRI' )then
            if( SNAME )then
               NBMIN = 2
            ELSE
               NBMIN = 2
            endif
         endif
      elseif( C2.eq.'SY' )then
         if( C3.eq.'TRF' )then
            if( SNAME )then
               NBMIN = 8
            ELSE
               NBMIN = 8
            endif
         elseif( SNAME .and. C3.eq.'TRD' )then
            NBMIN = 2
         endif
      elseif( CNAME .and. C2.eq.'HE' )then
         if( C3.eq.'TRD' )then
            NBMIN = 2
         endif
      elseif( SNAME .and. C2.eq.'OR' )then
         if( C3( 1:1 ).eq.'G' )then
            if( C4.eq.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.eq.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.eq.'BR' )then
               NBMIN = 2
            endif
         elseif( C3( 1:1 ).eq.'M' )then
            if( C4.eq.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.eq.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.eq.'BR' )then
               NBMIN = 2
            endif
         endif
      elseif( CNAME .and. C2.eq.'UN' )then
         if( C3( 1:1 ).eq.'G' )then
            if( C4.eq.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.eq.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.eq.'BR' )then
               NBMIN = 2
            endif
         elseif( C3( 1:1 ).eq.'M' )then
            if( C4.eq.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.eq.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.eq.'BR' )then
               NBMIN = 2
            endif
         endif
      endif
      ILAENV = NBMIN
      return
*
  300 continue
*
*     ISPEC = 3:  crossover point
*
      NX = 0
      if( C2.eq.'GE' )then
         if( C3.eq.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $       C3.eq.'QLF' )then
            if( SNAME )then
               NX = 128
            ELSE
               NX = 128
            endif
         elseif( C3.eq.'HRD' )then
            if( SNAME )then
               NX = 128
            ELSE
               NX = 128
            endif
         elseif( C3.eq.'BRD' )then
            if( SNAME )then
               NX = 128
            ELSE
               NX = 128
            endif
         endif
      elseif( C2.eq.'SY' )then
         if( SNAME .and. C3.eq.'TRD' )then
            NX = 1
         endif
      elseif( CNAME .and. C2.eq.'HE' )then
         if( C3.eq.'TRD' )then
            NX = 1
         endif
      elseif( SNAME .and. C2.eq.'OR' )then
         if( C3( 1:1 ).eq.'G' )then
            if( C4.eq.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.eq.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.eq.'BR' )then
               NX = 128
            endif
         endif
      elseif( CNAME .and. C2.eq.'UN' )then
         if( C3( 1:1 ).eq.'G' )then
            if( C4.eq.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.eq.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.eq.'BR' )then
               NX = 128
            endif
         endif
      endif
      ILAENV = NX
      return
*
  400 continue
*
*     ISPEC = 4:  number of shifts (used by xHSEQR)
*
      ILAENV = 6
      return
*
  500 continue
*
*     ISPEC = 5:  minimum column dimension (not used)
*
      ILAENV = 2
      return
*
  600 continue 
*
*     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
*
      ILAENV = INT( REAL( MIN( N1, N2 ) )*1.6E0 )
      return
*
  700 continue
*
*     ISPEC = 7:  number of processors (not used)
*
      ILAENV = 1
      return
*
  800 continue
*
*     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
*
      ILAENV = 50
      return
*
*     End of ILAENV
*
      end
      logical function lsame( ca, cb )
c 
c***********************************************************************
c
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          CA, CB
*     ..
*
*  Purpose
*  =======
*
cc LSAME returns .TRUE. if CA is the same letter as CB regardless of
*  case.
*
*  Arguments
*  =========
*
*  CA      (input) CHARACTER*1
*  CB      (input) CHARACTER*1
*          CA and CB specify the single characters to be compared.
*
* =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
*     ..
*     .. Local Scalars ..
      integer            INTA, INTB, ZCODE
*     ..
*     .. Executable Statements ..
*
*     Test if the characters are equal
*
      LSAME = CA.eq.CB
      if( LSAME )
     $   return
*
*     Now test for equivalence if both characters are alphabetic.
*
      ZCODE = ICHAR( 'Z' )
*
*     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
*     machines, on which ICHAR returns a value with bit 8 set.
*     ICHAR('A') on Prime machines returns 193 which is the same as
*     ICHAR('A') on an EBCDIC machine.
*
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
*
      if( ZCODE.eq.90 .OR. ZCODE.EQ.122 )then
*
*        ASCII is assumed - ZCODE is the ASCII code of either lower or
*        upper case 'Z'.
*
         if( INTA.ge.97 .and. INTA.LE.122 ) INTA = INTA - 32
         if( INTB.ge.97 .and. INTB.LE.122 ) INTB = INTB - 32
*
      elseif( ZCODE.eq.233 .OR. ZCODE.EQ.169 )then
*
*        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
*        upper case 'Z'.
*
         if( INTA.ge.129 .and. INTA.LE.137 .OR.
     $       INTA.ge.145 .and. INTA.LE.153 .OR.
     $       INTA.ge.162 .and. INTA.LE.169 ) INTA = INTA + 64
         if( INTB.ge.129 .and. INTB.LE.137 .OR.
     $       INTB.ge.145 .and. INTB.LE.153 .OR.
     $       INTB.ge.162 .and. INTB.LE.169 ) INTB = INTB + 64
*
      elseif( ZCODE.eq.218 .OR. ZCODE.EQ.250 )then
*
*        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
*        plus 128 of either lower or upper case 'Z'.
*
         if( INTA.ge.225 .and. INTA.LE.250 ) INTA = INTA - 32
         if( INTB.ge.225 .and. INTB.LE.250 ) INTB = INTB - 32
      endif
      LSAME = INTA.eq.INTB
 
      return
      end
      subroutine xerbla(srname,info)
c 
c***********************************************************************
c
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      integer            INFO
*     ..
*
*  Purpose
*  =======
*
cc XERBLA  is an error handler for the LAPACK routines.
*  It is called by an LAPACK routine if an input parameter has an
*  invalid value.  A message is printed and execution stops.
*
*  Installers may consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Arguments
*  =========
*
*  SRNAME  (input) CHARACTER*6
*          The name of the routine which called XERBLA.
*
*  INFO    (input) integer
*          The position of the invalid parameter in the parameter list
*          of the calling routine.
*
* =====================================================================
*
*     .. Executable Statements ..
*
      WRITE( *, FMT = 9999 )SRNAME, INFO
*
      STOP
*
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
*
*     End of XERBLA
*
      END
c  arby3sub3.f  16 July 1996
c
      double precision function dlamch( CMACH )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          CMACH
*     ..
*
*  Purpose
*  =======
*
cc DLAMCH determines double precision machine parameters.
*
*  Arguments
*  =========
*
*  CMACH   (input) CHARACTER*1
*          Specifies the value to be returned by dlamch:
*          = 'E' or 'e',   dlamch := eps
*          = 'S' or 's ,   dlamch := sfmin
*          = 'B' or 'b',   dlamch := base
*          = 'P' or 'p',   dlamch := eps*base
*          = 'N' or 'n',   dlamch := t
*          = 'R' or 'r',   dlamch := rnd
*          = 'M' or 'm',   dlamch := emin
*          = 'U' or 'u',   dlamch := rmin
*          = 'L' or 'l',   dlamch := emax
*          = 'O' or 'o',   dlamch := rmax
*
*          where
*
*          eps   = relative machine precision
*          sfmin = safe minimum, such that 1/sfmin does not overflow
*          base  = base of the machine
*          prec  = eps*base
*          t     = number of (base) digits in the mantissa
*          rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
*          emin  = minimum exponent before (gradual) underflow
*          rmin  = underflow threshold - base**(emin-1)
*          emax  = largest exponent before overflow
*          rmax  = overflow threshold  - (base**emax)*(1-eps)
*
* =====================================================================
*
*     .. Parameters ..
      double precision   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FIRST, LRND
      INTEGER            BETA, IMAX, IMIN, IT
      double precision   BASE, EMAX, EMIN, EPS, PREC, RMACH, RMAX, RMIN,
     $                   RND, SFMIN, SMALL, T
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           dlamc2
*     ..
*     .. Save statement ..
      SAVE               FIRST, EPS, SFMIN, BASE, T, RND, EMIN, RMIN,
     $                   EMAX, RMAX, PREC
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. /
*     ..
*     .. Executable Statements ..
*
      IF( FIRST ) THEN
         FIRST = .FALSE.
         CALL dlamc2( BETA, IT, LRND, EPS, IMIN, RMIN, IMAX, RMAX )
         BASE = BETA
         T = IT
         IF( LRND ) THEN
            RND = ONE
            EPS = ( BASE**( 1-IT ) ) / 2
         ELSE
            RND = ZERO
            EPS = BASE**( 1-IT )
         END IF
         PREC = EPS*BASE
         EMIN = IMIN
         EMAX = IMAX
         SFMIN = RMIN
         SMALL = ONE / RMAX
         IF( SMALL.GE.SFMIN ) THEN
*
*           Use SMALL plus a bit, to avoid the possibility of rounding
*           causing overflow when computing  1/sfmin.
*
            SFMIN = SMALL*( ONE+EPS )
         END IF
      END IF
*
      IF( LSAME( CMACH, 'E' ) ) THEN
         RMACH = EPS
      ELSE IF( LSAME( CMACH, 'S' ) ) THEN
         RMACH = SFMIN
      ELSE IF( LSAME( CMACH, 'B' ) ) THEN
         RMACH = BASE
      ELSE IF( LSAME( CMACH, 'P' ) ) THEN
         RMACH = PREC
      ELSE IF( LSAME( CMACH, 'N' ) ) THEN
         RMACH = T
      ELSE IF( LSAME( CMACH, 'R' ) ) THEN
         RMACH = RND
      ELSE IF( LSAME( CMACH, 'M' ) ) THEN
         RMACH = EMIN
      ELSE IF( LSAME( CMACH, 'U' ) ) THEN
         RMACH = RMIN
      ELSE IF( LSAME( CMACH, 'L' ) ) THEN
         RMACH = EMAX
      ELSE IF( LSAME( CMACH, 'O' ) ) THEN
         RMACH = RMAX
      END IF
*
      dlamch = RMACH
      RETURN
*
*     End of dlamch
*
      END
*
************************************************************************
*
      subroutine dlamc1( BETA, T, RND, IEEE1 )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            IEEE1, RND
      INTEGER            BETA, T
*     ..
*
*  Purpose
*  =======
*
cc DLAMC1 determines the machine parameters given by BETA, T, RND, and
*  IEEE1.
*
*  Arguments
*  =========
*
*  BETA    (output) INTEGER
*          The base of the machine.
*
*  T       (output) INTEGER
*          The number of ( BETA ) digits in the mantissa.
*
*  RND     (output) LOGICAL
*          Specifies whether proper rounding  ( RND = .TRUE. )  or
*          chopping  ( RND = .FALSE. )  occurs in addition. This may not
*          be a reliable guide to the way in which the machine performs
*          its arithmetic.
*
*  IEEE1   (output) LOGICAL
*          Specifies whether rounding appears to be done in the IEEE
*          'round to nearest' style.
*
*  Further Details
*  ===============
*
*  The routine is based on the routine  ENVRON  by Malcolm and
*  incorporates suggestions by Gentleman and Marovich. See
*
*     Malcolm M. A. (1972) Algorithms to reveal properties of
*        floating-point arithmetic. Comms. of the ACM, 15, 949-951.
*
*     Gentleman W. M. and Marovich S. B. (1974) More on algorithms
*        that reveal properties of floating point arithmetic units.
*        Comms. of the ACM, 17, 276-277.
*
* =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            FIRST, LIEEE1, LRND
      INTEGER            LBETA, LT
      double precision   A, B, C, F, ONE, QTR, SAVEC, T1, T2
*     ..
*     .. External Functions ..
      double precision   dlamc3
      EXTERNAL           dlamc3
*     ..
*     .. Save statement ..
      SAVE               FIRST, LIEEE1, LBETA, LRND, LT
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. /
*     ..
*     .. Executable Statements ..
*
      IF( FIRST ) THEN
         FIRST = .FALSE.
         ONE = 1
*
*        LBETA,  LIEEE1,  LT and  LRND  are the  local values  of  BETA,
*        IEEE1, T and RND.
*
*        Throughout this routine  we use the function  dlamc3  to ensure
*        that relevant values are  stored and not held in registers,  or
*        are not affected by optimizers.
*
*        Compute  a = 2.0**m  with the  smallest positive integer m such
*        that
*
*           fl( a + 1.0 ) = a.
*
         A = 1
         C = 1
*
*+       WHILE( C.EQ.ONE )LOOP
   10    CONTINUE
         IF( C.EQ.ONE ) THEN
            A = 2*A
            C = dlamc3( A, ONE )
            C = dlamc3( C, -A )
            GO TO 10
         END IF
*+       END WHILE
*
*        Now compute  b = 2.0**m  with the smallest positive integer m
*        such that
*
*           fl( a + b ) .gt. a.
*
         B = 1
         C = dlamc3( A, B )
*
*+       WHILE( C.EQ.A )LOOP
   20    CONTINUE
         IF( C.EQ.A ) THEN
            B = 2*B
            C = dlamc3( A, B )
            GO TO 20
         END IF
*+       END WHILE
*
*        Now compute the base.  a and c  are neighbouring floating point
*        numbers  in the  interval  ( beta**t, beta**( t + 1 ) )  and so
*        their difference is beta. Adding 0.25 to c is to ensure that it
*        is truncated to beta and not ( beta - 1 ).
*
         QTR = ONE / 4
         SAVEC = C
         C = dlamc3( C, -A )
         LBETA = C + QTR
*
*        Now determine whether rounding or chopping occurs,  by adding a
*        bit  less  than  beta/2  and a  bit  more  than  beta/2  to  a.
*
         B = LBETA
         F = dlamc3( B / 2, -B / 100 )
         C = dlamc3( F, A )
         IF( C.EQ.A ) THEN
            LRND = .TRUE.
         ELSE
            LRND = .FALSE.
         END IF
         F = dlamc3( B / 2, B / 100 )
         C = dlamc3( F, A )
         IF( ( LRND ) .AND. ( C.EQ.A ) )
     $      LRND = .FALSE.
*
*        Try and decide whether rounding is done in the  IEEE  'round to
*        nearest' style. B/2 is half a unit in the last place of the two
*        numbers A and SAVEC. Furthermore, A is even, i.e. has last  bit
*        zero, and SAVEC is odd. Thus adding B/2 to A should not  change
*        A, but adding B/2 to SAVEC should change SAVEC.
*
         T1 = dlamc3( B / 2, A )
         T2 = dlamc3( B / 2, SAVEC )
         LIEEE1 = ( T1.EQ.A ) .AND. ( T2.GT.SAVEC ) .AND. LRND
*
*        Now find  the  mantissa, t.  It should  be the  integer part of
*        log to the base beta of a,  however it is safer to determine  t
*        by powering.  So we find t as the smallest positive integer for
*        which
*
*           fl( beta**t + 1.0 ) = 1.0.
*
         LT = 0
         A = 1
         C = 1
*
*+       WHILE( C.EQ.ONE )LOOP
   30    CONTINUE
         IF( C.EQ.ONE ) THEN
            LT = LT + 1
            A = A*LBETA
            C = dlamc3( A, ONE )
            C = dlamc3( C, -A )
            GO TO 30
         END IF
*+       END WHILE
*
      END IF
*
      BETA = LBETA
      T = LT
      RND = LRND
      IEEE1 = LIEEE1
      RETURN
*
*     End of dlamc1
*
      END
*
************************************************************************
*
      subroutine dlamc2( BETA, T, RND, EPS, EMIN, RMIN, EMAX, RMAX )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            RND
      INTEGER            BETA, EMAX, EMIN, T
      double precision   EPS, RMAX, RMIN
*     ..
*
*  Purpose
*  =======
*
cc DLAMC2 determines the machine parameters specified in its argument
*  list.
*
*  Arguments
*  =========
*
*  BETA    (output) INTEGER
*          The base of the machine.
*
*  T       (output) INTEGER
*          The number of ( BETA ) digits in the mantissa.
*
*  RND     (output) LOGICAL
*          Specifies whether proper rounding  ( RND = .TRUE. )  or
*          chopping  ( RND = .FALSE. )  occurs in addition. This may not
*          be a reliable guide to the way in which the machine performs
*          its arithmetic.
*
*  EPS     (output) double precision
*          The smallest positive number such that
*
*             fl( 1.0 - EPS ) .LT. 1.0,
*
*          where fl denotes the computed value.
*
*  EMIN    (output) INTEGER
*          The minimum exponent before (gradual) underflow occurs.
*
*  RMIN    (output) double precision
*          The smallest normalized number for the machine, given by
*          BASE**( EMIN - 1 ), where  BASE  is the floating point value
*          of BETA.
*
*  EMAX    (output) INTEGER
*          The maximum exponent before overflow occurs.
*
*  RMAX    (output) double precision
*          The largest positive number for the machine, given by
*          BASE**EMAX * ( 1 - EPS ), where  BASE  is the floating point
*          value of BETA.
*
*  Further Details
*  ===============
*
*  The computation of  EPS  is based on a routine PARANOIA by
*  W. Kahan of the University of California at Berkeley.
*
* =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            FIRST, IEEE, IWARN, LIEEE1, LRND
      INTEGER            GNMIN, GPMIN, I, LBETA, LEMAX, LEMIN, LT,
     $                   NGNMIN, NGPMIN
      double precision   A, B, C, HALF, LEPS, LRMAX, LRMIN, ONE, RBASE,
     $                   SIXTH, SMALL, THIRD, TWO, ZERO
*     ..
*     .. External Functions ..
      double precision   dlamc3
      EXTERNAL           dlamc3
*     ..
*     .. External Subroutines ..
      EXTERNAL           dlamc1, DLAMC4, DLAMC5
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Save statement ..
      SAVE               FIRST, IWARN, LBETA, LEMAX, LEMIN, LEPS, LRMAX,
     $                   LRMIN, LT
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. / , IWARN / .FALSE. /
*     ..
*     .. Executable Statements ..
*
      IF( FIRST ) THEN
         FIRST = .FALSE.
         ZERO = 0
         ONE = 1
         TWO = 2
*
*        LBETA, LT, LRND, LEPS, LEMIN and LRMIN  are the local values of
*        BETA, T, RND, EPS, EMIN and RMIN.
*
*        Throughout this routine  we use the function  dlamc3  to ensure
*        that relevant values are stored  and not held in registers,  or
*        are not affected by optimizers.
*
*        dlamc1 returns the parameters  LBETA, LT, LRND and LIEEE1.
*
         CALL dlamc1( LBETA, LT, LRND, LIEEE1 )
*
*        Start to find EPS.
*
         B = LBETA
         A = B**( -LT )
         LEPS = A
*
*        Try some tricks to see whether or not this is the correct  EPS.
*
         B = TWO / 3
         HALF = ONE / 2
         SIXTH = dlamc3( B, -HALF )
         THIRD = dlamc3( SIXTH, SIXTH )
         B = dlamc3( THIRD, -HALF )
         B = dlamc3( B, SIXTH )
         B = ABS( B )
         IF( B.LT.LEPS )
     $      B = LEPS
*
         LEPS = 1
*
*+       WHILE( ( LEPS.GT.B ).AND.( B.GT.ZERO ) )LOOP
   10    CONTINUE
         IF( ( LEPS.GT.B ) .AND. ( B.GT.ZERO ) ) THEN
            LEPS = B
            C = dlamc3( HALF*LEPS, ( TWO**5 )*( LEPS**2 ) )
            C = dlamc3( HALF, -C )
            B = dlamc3( HALF, C )
            C = dlamc3( HALF, -B )
            B = dlamc3( HALF, C )
            GO TO 10
         END IF
*+       END WHILE
*
         IF( A.LT.LEPS )
     $      LEPS = A
*
*        Computation of EPS complete.
*
*        Now find  EMIN.  Let A = + or - 1, and + or - (1 + BASE**(-3)).
*        Keep dividing  A by BETA until (gradual) underflow occurs. This
*        is detected when we cannot recover the previous A.
*
         RBASE = ONE / LBETA
         SMALL = ONE
         DO 20 I = 1, 3
            SMALL = dlamc3( SMALL*RBASE, ZERO )
   20    CONTINUE
         A = dlamc3( ONE, SMALL )
         CALL dlamc4( NGPMIN, ONE, LBETA )
         CALL dlamc4( NGNMIN, -ONE, LBETA )
         CALL dlamc4( GPMIN, A, LBETA )
         CALL dlamc4( GNMIN, -A, LBETA )
         IEEE = .FALSE.
*
         IF( ( NGPMIN.EQ.NGNMIN ) .AND. ( GPMIN.EQ.GNMIN ) ) THEN
            IF( NGPMIN.EQ.GPMIN ) THEN
               LEMIN = NGPMIN
*            ( Non twos-complement machines, no gradual underflow;
*              e.g.,  VAX )
            ELSE IF( ( GPMIN-NGPMIN ).EQ.3 ) THEN
               LEMIN = NGPMIN - 1 + LT
               IEEE = .TRUE.
*            ( Non twos-complement machines, with gradual underflow;
*              e.g., IEEE standard followers )
            ELSE
               LEMIN = MIN( NGPMIN, GPMIN )
*            ( A guess; no known machine )
               IWARN = .TRUE.
            END IF
*
         ELSE IF( ( NGPMIN.EQ.GPMIN ) .AND. ( NGNMIN.EQ.GNMIN ) ) THEN
            IF( ABS( NGPMIN-NGNMIN ).EQ.1 ) THEN
               LEMIN = MAX( NGPMIN, NGNMIN )
*            ( Twos-complement machines, no gradual underflow;
*              e.g., CYBER 205 )
            ELSE
               LEMIN = MIN( NGPMIN, NGNMIN )
*            ( A guess; no known machine )
               IWARN = .TRUE.
            END IF
*
         ELSE IF( ( ABS( NGPMIN-NGNMIN ).EQ.1 ) .AND.
     $            ( GPMIN.EQ.GNMIN ) ) THEN
            IF( ( GPMIN-MIN( NGPMIN, NGNMIN ) ).EQ.3 ) THEN
               LEMIN = MAX( NGPMIN, NGNMIN ) - 1 + LT
*            ( Twos-complement machines with gradual underflow;
*              no known machine )
            ELSE
               LEMIN = MIN( NGPMIN, NGNMIN )
*            ( A guess; no known machine )
               IWARN = .TRUE.
            END IF
*
         ELSE
            LEMIN = MIN( NGPMIN, NGNMIN, GPMIN, GNMIN )
*         ( A guess; no known machine )
            IWARN = .TRUE.
         END IF
***
* Comment out this if block if EMIN is ok
         IF( IWARN ) THEN
            FIRST = .TRUE.
            WRITE( 6, FMT = 9999 )LEMIN
         END IF
***
*
*        Assume IEEE arithmetic if we found denormalised  numbers above,
*        or if arithmetic seems to round in the  IEEE style,  determined
*        in routine dlamc1. A true IEEE machine should have both  things
*        true; however, faulty machines may have one or the other.
*
         IEEE = IEEE .OR. LIEEE1
*
*        Compute  RMIN by successive division by  BETA. We could compute
*        RMIN as BASE**( EMIN - 1 ),  but some machines underflow during
*        this computation.
*
         LRMIN = 1
         DO 30 I = 1, 1 - LEMIN
            LRMIN = dlamc3( LRMIN*RBASE, ZERO )
   30    CONTINUE
*
*        Finally, call dlamc5 to compute EMAX and RMAX.
*
         CALL dlamc5( LBETA, LT, LEMIN, IEEE, LEMAX, LRMAX )
      END IF
*
      BETA = LBETA
      T = LT
      RND = LRND
      EPS = LEPS
      EMIN = LEMIN
      RMIN = LRMIN
      EMAX = LEMAX
      RMAX = LRMAX
*
      RETURN
*
 9999 FORMAT( / / ' WARNING. The value EMIN may be incorrect:-',
     $      '  EMIN = ', I8, /
     $      ' If, after inspection, the value EMIN looks',
     $      ' acceptable please comment out ',
     $      / ' the IF block as marked within the code of routine',
     $      ' dlamc2,', / ' otherwise supply EMIN explicitly.', / )
*
*     End of dlamc2
*
      END
*
************************************************************************
*
      double precision function dlamc3( A, B )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      double precision   A, B
*     ..
*
*  Purpose
*  =======
*
cc DLAMC3 is intended to force  A  and  B  to be stored prior to doing
*  the addition of  A  and  B ,  for use in situations where optimizers
*  might hold one of these in a register.
*
*  Arguments
*  =========
*
*  A, B    (input) double precision
*          The values A and B.
*
* =====================================================================
*
*     .. Executable Statements ..
*
      dlamc3 = A + B
*
      RETURN
*
*     End of dlamc3
*
      END
*
************************************************************************
*
      subroutine dlamc4( EMIN, START, BASE )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      INTEGER            BASE, EMIN
      double precision   START
*     ..
*
*  Purpose
*  =======
*
cc DLAMC4 is a service routine for DLAMC2.
*
*  Arguments
*  =========
*
*  EMIN    (output) EMIN
*          The minimum exponent before (gradual) underflow, computed by
*          setting A = START and dividing by BASE until the previous A
*          can not be recovered.
*
*  START   (input) double precision
*          The starting point for determining EMIN.
*
*  BASE    (input) INTEGER
*          The base of the machine.
*
* =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I
      double precision   A, B1, B2, C1, C2, D1, D2, ONE, RBASE, ZERO
*     ..
*     .. External Functions ..
      double precision   dlamc3
      EXTERNAL           dlamc3
*     ..
*     .. Executable Statements ..
*
      A = START
      ONE = 1
      RBASE = ONE / BASE
      ZERO = 0
      EMIN = 1
      B1 = dlamc3( A*RBASE, ZERO )
      C1 = A
      C2 = A
      D1 = A
      D2 = A
*+    WHILE( ( C1.EQ.A ).AND.( C2.EQ.A ).AND.
*    $       ( D1.EQ.A ).AND.( D2.EQ.A )      )LOOP
   10 CONTINUE
      IF( ( C1.EQ.A ) .AND. ( C2.EQ.A ) .AND. ( D1.EQ.A ) .AND.
     $    ( D2.EQ.A ) ) THEN
         EMIN = EMIN - 1
         A = B1
         B1 = dlamc3( A / BASE, ZERO )
         C1 = dlamc3( B1*BASE, ZERO )
         D1 = ZERO
         DO 20 I = 1, BASE
            D1 = D1 + B1
   20    CONTINUE
         B2 = dlamc3( A*RBASE, ZERO )
         C2 = dlamc3( B2 / RBASE, ZERO )
         D2 = ZERO
         DO 30 I = 1, BASE
            D2 = D2 + B2
   30    CONTINUE
         GO TO 10
      END IF
*+    END WHILE
*
      RETURN
*
*     End of dlamc4
*
      END
*
************************************************************************
*
      subroutine dlamc5( BETA, P, EMIN, IEEE, EMAX, RMAX )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            IEEE
      INTEGER            BETA, EMAX, EMIN, P
      double precision   RMAX
*     ..
*
cc DlAMC5 attempts to compute RMAX, the largest machine floating-point
*  number, without overflow.  It assumes that EMAX + abs(EMIN) sum
*  approximately to a power of 2.  It will fail on machines where this
*  assumption does not hold, for example, the Cyber 205 (EMIN = -28625,
*  EMAX = 28718).  It will also fail if the value supplied for EMIN is
*  too large (i.e. too close to zero), probably with overflow.
*
*  Arguments
*  =========
*
*  BETA    (input) INTEGER
*          The base of floating-point arithmetic.
*
*  P       (input) INTEGER
*          The number of base BETA digits in the mantissa of a
*          floating-point value.
*
*  EMIN    (input) INTEGER
*          The minimum exponent before (gradual) underflow.
*
*  IEEE    (input) LOGICAL
*          A logical flag specifying whether or not the arithmetic
*          system is thought to comply with the IEEE standard.
*
*  EMAX    (output) INTEGER
*          The largest exponent before overflow
*
*  RMAX    (output) double precision
*          The largest machine floating-point number.
*
* =====================================================================
*
*     .. Parameters ..
      double precision   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            EXBITS, EXPSUM, I, LEXP, NBITS, TRY, UEXP
      double precision   OLDY, RECBAS, Y, Z
*     ..
*     .. External Functions ..
      double precision   dlamc3
      EXTERNAL           dlamc3
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MOD
*     ..
*     .. Executable Statements ..
*
*     First compute LEXP and UEXP, two powers of 2 that bound
*     abs(EMIN). We then assume that EMAX + abs(EMIN) will sum
*     approximately to the bound that is closest to abs(EMIN).
*     (EMAX is the exponent of the required number RMAX).
*
      LEXP = 1
      EXBITS = 1
   10 CONTINUE
      TRY = LEXP*2
      IF( TRY.LE.( -EMIN ) ) THEN
         LEXP = TRY
         EXBITS = EXBITS + 1
         GO TO 10
      END IF
      IF( LEXP.EQ.-EMIN ) THEN
         UEXP = LEXP
      ELSE
         UEXP = TRY
         EXBITS = EXBITS + 1
      END IF
*
*     Now -LEXP is less than or equal to EMIN, and -UEXP is greater
*     than or equal to EMIN. EXBITS is the number of bits needed to
*     store the exponent.
*
      IF( ( UEXP+EMIN ).GT.( -LEXP-EMIN ) ) THEN
         EXPSUM = 2*LEXP
      ELSE
         EXPSUM = 2*UEXP
      END IF
*
*     EXPSUM is the exponent range, approximately equal to
*     EMAX - EMIN + 1 .
*
      EMAX = EXPSUM + EMIN - 1
      NBITS = 1 + EXBITS + P
*
*     NBITS is the total number of bits needed to store a
*     floating-point number.
*
      IF( ( MOD( NBITS, 2 ).EQ.1 ) .AND. ( BETA.EQ.2 ) ) THEN
*
*        Either there are an odd number of bits used to store a
*        floating-point number, which is unlikely, or some bits are
*        not used in the representation of numbers, which is possible,
*        (e.g. Cray machines) or the mantissa has an implicit bit,
*        (e.g. IEEE machines, Dec Vax machines), which is perhaps the
*        most likely. We have to assume the last alternative.
*        If this is true, then we need to reduce EMAX by one because
*        there must be some way of representing zero in an implicit-bit
*        system. On machines like Cray, we are reducing EMAX by one
*        unnecessarily.
*
         EMAX = EMAX - 1
      END IF
*
      IF( IEEE ) THEN
*
*        Assume we are on an IEEE machine which reserves one exponent
*        for infinity and NaN.
*
         EMAX = EMAX - 1
      END IF
*
*     Now create RMAX, the largest machine number, which should
*     be equal to (1.0 - BETA**(-P)) * BETA**EMAX .
*
*     First compute 1.0 - BETA**(-P), being careful that the
*     result is less than 1.0 .
*
      RECBAS = ONE / BETA
      Z = BETA - ONE
      Y = ZERO
      DO 20 I = 1, P
         Z = Z*RECBAS
         IF( Y.LT.ONE )
     $      OLDY = Y
         Y = dlamc3( Y, Z )
   20 CONTINUE
      IF( Y.GE.ONE )
     $   Y = OLDY
*
*     Now multiply by BETA**EMAX to get RMAX.
*
      DO 30 I = 1, EMAX
         Y = dlamc3( Y*BETA, ZERO )
   30 CONTINUE
*
      RMAX = Y
      RETURN
*
*     End of dlamc5
*
      END
      subroutine dlarfg(n,alpha,x,incx,tau)
c 
c***********************************************************************
c
*
*  Purpose
*  =======
*
cc DLARFG generates a real elementary reflector H of order n, such
*  that
*
*        H * ( alpha ) = ( beta ),   H' * H = I.
*            (   x   )   (   0  )
*
*  where alpha and beta are scalars, and x is an (n-1)-element real
*  vector. H is represented in the form
*
*        H = I - tau * ( 1 ) * ( 1 v' ) ,
*                      ( v )
*
*  where tau is a real scalar and v is a real (n-1)-element
*  vector.
*
*  If the elements of x are all zero, then tau = 0 and H is taken to be
*  the unit matrix.
*
*  Otherwise  1 <= tau <= 2.
*
*  Arguments
*  =========
*
*  N       (input) integer
*          The order of the elementary reflector.
*
*  ALPHA   (input/output) double precision
*          On entry, the value alpha.
*          On exit, it is overwritten with the value beta.
*
*  X       (input/output) double precision array, dimension
*                         (1+(N-2)*abs(INCX))
*          On entry, the vector x.
*          On exit, it is overwritten with the vector v.
*
*  INCX    (input) integer
*          The increment between elements of X. INCX > 0.
*
*  TAU     (output) double precision
*          The value tau.
*
      double precision one
      double precision zero
 
      parameter (one=1.0d0)
      parameter (zero=0.0d0)
c
      double precision alpha
      double precision beta
      double precision dlamch
      double precision dlapy2
      double precision dnrm2
      integer incx
      integer j
      integer knt
      integer n
      double precision rsafmn
      double precision safmin
      double precision tau
      double precision x(*)
      double precision xnorm
c
      EXTERNAL           DLAMCH, DLAPY2, DNRM2
      INTRINSIC          ABS, SIGN
      EXTERNAL           DSCAL
*
      if(n.le.1)then
         TAU = zero
         return
      endif
*
      XNORM = DNRM2( N-1, X, INCX )
*
      if( XNORM.eq.zero )then
*
*        H  =  I
*
         TAU = zero
      ELSE
*
*        general case
*
         BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA )
         SAFMIN = DLAMCH( 'S' ) / DLAMCH( 'E' )
 
*
*  XNORM, BETA may be inaccurate; scale X and recompute them
*
         if( ABS( BETA ).LT.SAFMIN )then
            RSAFMN = ONE / SAFMIN
            KNT = 0
   10       continue
            knt = knt + 1
            call DSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHA = ALPHA*RSAFMN
            if( ABS( BETA ).LT.SAFMIN ) go to 10
*
*  New BETA is at most 1, at least SAFMIN
*
            XNORM = DNRM2( N-1, X, INCX )
            BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA )
            TAU = ( BETA-ALPHA ) / BETA
            CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
*
*  If ALPHA is subnormal, it may lose relative accuracy
*
            ALPHA = BETA
            DO J = 1, KNT
               ALPHA = ALPHA*SAFMIN
            enddo
         ELSE
            TAU = ( BETA-ALPHA ) / BETA
            CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
            ALPHA = BETA
         endif
      endif
 
      return
      end
