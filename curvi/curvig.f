c ********************************************************************
c     This is the file curvig.for using only function and gradient
c     values
c  ********************************************************************
c ---------------------------------------------------------------------
      subroutine curvig(fu,gradie,n,x0,fopt,eps,ibound,
     *jbound,bl,bu,wa,nfu,ngr,nit,ier)
c
c  ********************************************************************
c                       Arguments
c                       ---------
c     Input:
c
c        fu:  user supplied subroutine which computes the function
c             to be minimized.
c
c     Usage:  call fu(n,x,f)
c         n:  dimension of x.
c         x:  n-dimensional vector.
c         f:  value of the function at the point x.
c
c    gradie:  user supplied subroutine which computes the gradient of
c             the function to be minimized.
c
c     Usage:  call gradie(n,x,g)
c         g:  the gradient at the point x.
c
c         n:  dimension of the problem.
c
c        x0:  initial guess point.
c
c       eps:  tolerance for the stopping criterion.
c
c    ibound:  parameter such that if equal to
c             0 is an unconstrained problem 
c             1 is a constrained problem.
c
c    jbound:  working vector of dimension n defining the sort of
c             constraint for each variable (only used when ibound.ne.0).
c             jbound(i) = 0 if the ith variable has no constraints.
c                         1 if the ith variable has only upper bounds.
c                         2 if the ith variable has only lower bounds.
c                         3 if the ith variable has both upper and
c                           lower bounds.
c
c        bl:  vector of lower bounds (not used if ibound=0).
c
c        bu:  vector of upper bounds (not used if ibound=0).
c 
c        For keeping constant the ith variable set:
c
c             jbound(i) = 3
c             bl(i) = bu(i) = constant
c
c        wa:  working vector of dimension (see output)
c             9*n+n*(n+1)/2+n*n+max(9*n-n*(n+1)/2,0) 
c
c       nfu:  maximum number of function evaluations. If equal to
c             zero, the default value of 1000*n is used.
c----------------------------------------------------------------------
c    Output:
c
c        x0:  best obtained point.
c
c      fopt:  value of the function at the point x0.
c
c       nfu:  number of function evaluations.
c
c       ngr:  number of gradient evaluations.
c
c       nit:  number of iterations.
c
c        wa:  this vector contains gradient(x0) in the unconstrained
c             case, and the projected gradient(x0) for constrained
c             problems.
c
c       ier:  0 convergence has been achieved.
c             1 maximum number of function evaluations exceeded.
c             2 failure to converge.
c             3 wrong input in a constrained problem.
c----------------------------------------------------------------------
c      The algorithm for unconstrained optimization is described in:
c       "A curvilinear search using tridiagonal secant updates for
c                    unconstrained optimization"
c      J.E.Dennis,N.Echebest,M.T.Guardarucci,J.M.Martinez,H.D.Scolnik
c      and C.Vacchino.
c      SIAM J. on Optimization, Vol.1, Number 3, August 1991, page 351
c----------------------------------------------------------------------

c      Necessary routines: RUT and RUTGH

      implicit real*8 (a-h,o-z)
      dimension x0(n),wa(1),bl(n),bu(n),jbound(n)
      common /busca/eig,dnor,v1
      common/npar/maxfun
      common/param/stmin,stmax
      external fu,gradie
c----------------------------------------------------------------------
c
c     The parameter kmax is such that the hessian is recomputed every
c     kmax iterations. For every other iteration only the tridiagonal
c     matrix T is updated in the decomposition H = Q(t)*T*Q using
c     the least change secant update theory (see reference). 
c     Hence, larger values of kmax lead to fewer hessian evaluations. 
c     The chosen value of 3 has been found experimentally to be a good
c     compromise between highly and mildly nonlinear problems.
c
c----------------------------------------------------------------------
      data sal/1.d-5/,kmax/3/,zero/0.d0/
      ier=0
      n1=n*(n+1)/2
c----------------------------------------------------------------------
c     Set up pointers of the working vector.
c----------------------------------------------------------------------
c     Pointers for the working vector wa( ) are as follows:
c     nx1  :  the new point x1.
c     ngr0 :  the gradient at the point x0.
c     ngry :  ngr0+n
c     nxy  :  ngry+n
c     ngr1 :  the gradient at the new point.
c     nqt  :  Q transposed, where Q is an orthogonal matrix.
c     nhe  :  the Hessian
c     nd   :  diagonal  of the  tridiagonal matrix
c     ne   :  subdiagonal of the tridiagonal matrix
c     nw   :  - Q*grad0
c     nsq  :  Q*s
c----------------------------------------------------------------------
      nqt=1
      nx1=nqt+n*n
      ngr0=nx1+n
      ngry=ngr0+n
      nxy=ngry+n
      ngr1=nxy+n
      nw=ngr1+n
      nsq=nw+n
      nd=nsq+n
      ne=nd+n
      nhe=ne+n
      naux1=nhe
      naux2=nx1
c----------------------------------------------------------------------
c      auxiliar indexes.
c----------------------------------------------------------------------
      nmx1=nx1-1
      nmgr0=ngr0-1
      nmgr1=ngr1-1
      nmgry=ngry-1
      nmxy=nxy-1
      nmw=nw-1
      nmqt=nqt-1
      nmd=nd-1
      nme=ne-1
      ndc=naux1-1
      nec=naux2-1
c----------------------------------------------------------------------
c      Set default value for maxfun
c----------------------------------------------------------------------
      maxfun=1000*n
      if (nfu.gt.0)maxfun=nfu
      nit=0
      nfu=0
      ngr=0
c----------------------------------------------------------------------
c      Compute initial values of function and gradient.
c----------------------------------------------------------------------
      if(ibound.eq.0)then
            call fu(n,x0,fopt)
            call gradie(n,x0,wa(ngr0))
      else
c ---------------------------------------------------------------------
c      If it is a constrained problem is transformed into an
c      unconstrained one according to ibound.
c      The initial point is stored in wa(nxy).
c----------------------------------------------------------------------
      do i=1,n
            wa(nmxy+i)=x0(i)
      end do
c----------------------------------------------------------------------
c     x0, although transformed, is kept as the starting point of the
c     new unconstrained problem.
c----------------------------------------------------------------------
            call transf(n,nfu,wa(nxy),bl,bu,1,x0,jbound,ier)
            if(ier.eq.3)return
            call fu(n,wa(nxy),fopt)
            call gradie(n,wa(nxy),wa(ngry))
            call gradx(n,x0,bl,bu,wa(ngry),wa(ngr0),jbound)
      end if
      nfu=1
      ngr=1
c----------------------------------------------------------------------
c      Optimality test.
c----------------------------------------------------------------------
      ier=ierst(n,bl,bu,x0,fopt,wa(ngr0),wa(ngry),wa(nxy),eps,nfu,
     *          ngr,wa(naux1),fu,gradie,ibound,jbound)
      if (ier.gt.-1) go to 50
10    k=1
c----------------------------------------------------------------------
c      Compute and factorize the Hessian
c      as h=Qt*T*Q , where Q is an
c      orthogonal matrix and T is a
c      tridiagonal matrix.
c----------------------------------------------------------------------
      f=fopt
      if(ibound.eq.0)then
      call hgsime(f,gradie,n,x0,wa(nhe),wa(ngr0),wa(nw),wa(nsq),wa(nqt))
      else
c---------------------------------------------------------------------- 
c      The Hessian of the constrained problem is stored in wa(nxy)
c      and the Hessian of the unconstrained one in x0
c----------------------------------------------------------------------
      call hgsime(f,gradie,n,wa(nxy),wa(nhe),wa(ngry),wa(nw),wa(nsq),
     *wa(nqt))
            call hessix(n,x0,bl,bu,wa(ngry),wa(nhe),jbound)
      end if
      ngr=ngr+n
      call factor(n,n1,wa(nhe),wa(nqt),wa(nd),wa(ne),0)
20    nit=nit+1
c----------------------------------------------------------------------
c      Compute the smallest eigenvalue of
c      the tridiagonal matrix.
c      Set parameters for gsrch.
c----------------------------------------------------------------------
      do i=1,n
            wa(ndc+i)=wa(nmd+i)
            wa(nec+i)=wa(nme+i)**2
      end do
      call autova(wa(naux1),wa(naux2),n)
      eig=wa(naux1)
      dnor=dnrm2(n,wa(ngr0),1)
      v1=1.d0
      v3=sal

      if (eig.ge.sal) then
            cotmu=v1/eig
            step= cotmu
      else
            if (eig.gt.0.d0) then
                    v1=5.d0*eig
                    cotmu= v1/(eig)
                    step= v1/(eig+(sal-eig)+1.d-2*dsqrt(dnor))
            else
                    v2=1.d0
                    v1=1.d0
                    if(dnor.gt.1.d2) v2=1.d-1
                    cotmu=v1/sal
                    v3=dmax1(sal, 1.d-1*dabs(eig))
                    step=v1/(v3+v2*dsqrt(dnor))
            end if
      end if

      iqt=nmqt
      do j=1,n
            aw=0.d0
            do i=1,n
                    iqt=iqt+1
                    aw=aw-wa(iqt)*wa(nmgr0+i)
            end do
                    wa(nmw+j)=aw
      end do
      alfa=1.d0
      kalf=0
30    step0=step
      xmu=0.d0
      stmax=cotmu
      dgg=dnor*dnor
      der=-dgg/v1
      stmin=dmax1(alfa* v1/(dabs(v3 - eig)+eig+1.d16),1.d-30)
      f=fopt
      call gsrch (fu,gradie,n,x0,bl,bu,wa(nqt),wa(nd),wa(ne),wa(nw),
     *            wa(ngr0),xmu,f,der,step,iflag,nnf,wa(nx1),wa(nsq),
     *            wa(ngr1),wa(nxy),wa(ngry),wa(naux1),ibound,jbound)
      nfu=nfu+nnf
      ngr=ngr+nnf
c----------------------------------------------------------------------
c      Analyze alternatives if gsrch did not converge
c----------------------------------------------------------------------
      irot=3
      ierp=0
      if (dabs(fopt-f).gt. zero ) go to 40
            if (k.ne.1) then
                    k=kmax
            else
c----------------------------------------------------------------------
c      Update the initial step if gsrch did not converge
c----------------------------------------------------------------------
                    if(iflag.lt.5.and.kalf.lt.3)then
                        kalf=kalf+1
                        alfa=1.d-1*alfa
                        step=alfa*step0
                        go to 30
                    end if
c----------------------------------------------------------------------
c      Return if gsrch did not converge
c----------------------------------------------------------------------
            ierp=2
            end if
c----------------------------------------------------------------------
c      Optimality test.
c----------------------------------------------------------------------
40    ier=ierst(n,bl,bu,wa(nx1),f,wa(ngr1),wa(ngry),wa(nxy),eps,
     *          nfu,ngr,wa(naux1),fu,gradie,ibound,jbound)
      if(ier.eq.-1.and.ierp.eq.2)ier=2
      if(ier.eq.-2)k=kmax
      if (ier.le.-1)then
            irot=1
c----------------------------------------------------------------------
c      Decide if either update the Hessian or
c      recompute it.
c----------------------------------------------------------------------
            if(k.lt.kmax) then
                    k=k+1
                    call updat(n,bl,bu,wa(nd),wa(ne),wa(nqt),wa(nsq),
     *              x0,gradie,ig,wa(ngr0),wa(ngr1),wa(naux1),ibound,
     *              jbound)
                    ngr=ngr+ig
                    irot=2
            end if
      end if
c----------------------------------------------------------------------
c      Restore the current iterate, its function value and
c      its gradient.
c      If irot.le.2 continue
c      else --> return
c----------------------------------------------------------------------
      fopt=f
      do i=1,n
            x0(i)=wa(nmx1+i)
            wa(nmgr0+i)=wa(nmgr1+i)
      end do
      go to (10,20)irot
c----------------------------------------------------------------------
c      Restore the optimal point and the projected gradient
c      in the original variables before return.
c----------------------------------------------------------------------
50    if(ibound.ne.0)then
            do i= 1,n
                    x0(i)= wa(nmxy+i)
                    wa(nmgr0+i)=wa(naux1+i-1)
            end do
      end if
      return
      end
	subroutine hgsime(fx,gradie,n,x,h,g,gg,del,hh)
	implicit real*8(a-h,o-z)
	dimension x(1),h(1),g(1),gg(1),del(1),hh(1)
            common/infu/infu
	external gradie
c
c  ********************************************************************
c                       Arguments
c                       ---------
c     Input:
c
c        fx:  f(x)
c
c    gradie:  user supplied subroutine which computes the gradient of
c             the function to be minimized.
c
c         n:  dimension of the problem.
c
c         x:  current iterate.
c
c      g,gg:  working vectors
c
c       del:  increments used for hessian estimation.
c
c        hh:  auxiliary vector for hessian estimation.
c----------------------------------------------------------------------
c    Output:
c
c         h:  vector containing the lower triangular portion of the
c             numerically estimated hessian
c
c----------------------------------------------------------------------
            data beta1/53.1036811792759202d0/,
     *	     beta2/2.36701536161196047d0/,
     *	     beta3/0.66580545056039918d-12/,
     *	     beta4/1.d-7/
	c=dabs(fx)
	id=0
	if(c.gt.0.d0)then
		id=dlog10(c)
		if(id.gt.0)id=id+1
	end if
30	do i=1,n
		a=dabs(x(i))
		ib=0
		if(a.gt.0.d0)then
			ib=dlog10(a)
			if(ib.eq.0)ib=ib+1
		end if
50		ae=beta1**ib*beta2**id*beta3
		del(i)=ae+beta4*a
		xx=x(i)
		x(i)=xx+del(i)
		call gradie(n,x,gg)
		do j=1,n
			hh((i-1)*n+j)=(gg(j)-g(j))/del(i)
		end do
		x(i)=xx
	end do
	do i=1,n
		do j=1,n
			xx=hh((j-1)*n+i)
			xy=hh((i-1)*n+j)
			xx=0.5d0*(xx+xy)
			hh((j-1)*n+i)=xx
			hh((i-1)*n+j)=xx
		end do
	end do
	do i=1,n
		do j=i,n
			k=j*(j-1)/2+i
			h(k)=hh((j-1)*n+i)
		end do
	end do
	return
	end
