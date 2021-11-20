c  ********************************************************************
c
c      Adaptation of the one dimensional search routine originally
c      developed by M.J.D.Powell for the MINPACK project for finding
c      the minimum along the curve xi+s(xmu)
c
c  ********************************************************************
      subroutine gsrch(fu,gradie,n,xi,bl,bu,qt,d,e,wp,gri,xmu,f,g,step,
     *iflag,nf,xf,qs,grf,y,gy,wa,ibound,jbound)
c----------------------------------------------------------------------
c                       Arguments
c                       ---------
c     Input:
c
c        fu:  external function (see description in the main routine).
c    gradie:  external function (see description in the main routine).
c         n:  number of variables.
c        xi:  starting point.
c        bl:  vector of lower bounds for constrained problems.
c        bu:  vector of upper bounds for constrained problems.
c        qt:  orthogonal factor of the Householder transformation.
c             corresponding to the Hessian matrix of the quadratic
c             model at xi.
c         d:  diagonal of the tridiagonal factor.
c         e:  subdiagonal of the tridiagonal factor.
c       gri:  gradient at xi.
c        wp:  - Q * gri.
c       xmu:  variable of the subproblem g(xmu)= f(xi+ s(xmu)).
c         f:  f(xi).
c         g:  the derivative g'(xmu) at xmu=0.
c      step:  starting step for the search along the curve s(xmu).
c     stmin:  minimum allowable step.
c     stmax:  maximum allowable step.
c      nmax:  maximum allowable number of function evaluations.
c    grhtol:  tolerance for the curvilinear search.
c         y:  coordinates of the starting point in the original problem.
c        gy:  gradient at y.
c    ibound:  see definition in the main routine.
c    jbound:  see definition in the main routine.
c        wa:  working vector.
c----------------------------------------------------------------------
c    Output:
c
c     iflag: 0 convergence has been achieved.
c            1 error return because the computed step lies outside
c              the interval [stmin,stmax].
c            2 error return because step is insignificantly small.
c            3 error return because rounding limits the value of step.
c            4 error return if fu has been called nmax times.
c            5 error return if the search direction is not downhill
c              or if step is not positive
c        nf: number of function evaluations equal to the number
c            of gradient evaluations.
c        xf: new point.
c        qs: q*(xf - xi).
c       grf: gradient at xf.
c         y: coordinates of xf in the original problem.
c        gy: gradient at y.
c ---------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension xi(n),wp(n),gri(n),qt(n,n),d(n),e(n),xf(n),qs(n),
     *          grf(n),y(n),gy(n),wa(1),bl(n),bu(n),jbound(n)
      common /busca/anda,dnor,v1
      common/param/stmin,stmax
      external fu,gradie
      data grhtol/0.1d0/,nmax/25/
      ialg=0
      na1=1
      na2=na1+n
      na3=na2+n
      nsder=na3+n
      na4=nsder+n
      na5=na4+n
      na6=na5+n
      nma1=na1-1
      nma2=na2-1
      nma3=na3-1
      nma4=na4-1
      nma5=na5-1
c----------------------------------------------------------------------
c      stmult limits the extrapolation steps
c      stl and stu are the calculated bounds on step
c----------------------------------------------------------------------
      stmult=3.d0
      stl=0.d0
      stu=0.d0
c----------------------------------------------------------------------
c      nf and ng counts the number of calls to fu and gradie
c----------------------------------------------------------------------
      nf=0
      do  i=1,n
            xf(i)=xi(i)
            grf(i)=gri(i)
      end do
      go to 160
c----------------------------------------------------------------------
c      Select stepc or stepl for the new step lenght
c----------------------------------------------------------------------
15    stepc=stepbd
20    stepl=0d0
30    step=stepc
      if (stepl*stepla.le.0d0) go to 40
      if (dabs(stepl-stepla).le.dabs(stepl-stepca)) step=stepl
40    stepca=stepc
      stepla=stepl
c----------------------------------------------------------------------
c      Ensure that the extrapolation step is not too long
c----------------------------------------------------------------------
      if (stepbd.le.0d0) go to 50
      if (stp.lt.stx) step=dmax1(stepbd,step)
      if (stp.gt.stx) step=dmin1(stepbd,step)
      go to 60
c----------------------------------------------------------------------
c      Store the most recent f, xmu, dg, xf, grf,qs, y and gy
c      if they are the best so far
c----------------------------------------------------------------------
50    if (f.lt.fx) go to 60
      if (f.gt.fx) go to 90
      if (dabs(dg).ge.dabs(dgx)) go to 90
60    stx=stp
70    fx=f
      dgx=dg
      w1=xmu
      do i=1,n
            wa(nma1+i)=grf(i)
            wa(nma2+i)=xf(i)
            wa(nma3+i)=qs(i)
            wa(nma4+i)=y(i)
            wa(nma5+i)=gy(i)
      end do
c----------------------------------------------------------------------
c      Test for error return because step is
c      insignificantly small
c----------------------------------------------------------------------
90    if (stl.gt.0d0) go to 100
      iflag=2
      c=step+stinit
      if (c.ge.stinbd) go to 230
      stinbd=c
      go to 130
c----------------------------------------------------------------------
c      Test for error return because rounding limits
c      the value of step.
c----------------------------------------------------------------------
100   iflag=3
110   if(step.le.stl) go to 230
      if (stu.le.0.d0) go to 120
      if (step.ge.stu) go to 230
120   if (iflag.le.1) go to 140
c----------------------------------------------------------------------
c      Test for return because of the bounds stmin and stmax
c----------------------------------------------------------------------
130   iflag=1
      step=dmax1(step,stmin)
      if (stmax.gt.stmin) step=dmin1(step,stmax)
      go to 110
c----------------------------------------------------------------------
c      Error return if fu has been called nmax times
c----------------------------------------------------------------------
140   iflag=4
      if(nf.ge.nmax) go to 230
      nf=nf+1
c----------------------------------------------------------------------
c      Calculate another function and gradient
c----------------------------------------------------------------------
      h=step-stx
      xmu=w1+h
      stp=step
c----------------------------------------------------------------------
c     A change of variables is performed in order to compute
c     the function value and its gradient on the curve s(xmu)
c----------------------------------------------------------------------
      ax=v1/xmu-anda
      call funct(fu,n,xi,bl,bu,d,e,qt,wp,ax,f,xf,y,qs,wa(na6),
     *           ibound,jbound,nfu)
      call deriv(gradie,n,xf,y,bl,bu,qt,d,e,qs,ax,grf,gy,
     *           wa(nsder),g,wa(na6),ibound,jbound)
      dgg=g
      dnds=dnrm2(n,wa(nsder),1)
c----------------------------------------------------------------------
c     Calculate the new derivative of g(xmu)
c----------------------------------------------------------------------
      g=-g*v1/(xmu*xmu)
160   dg=g
      if(ialg.eq.1) go to 170
      ialg=1
      iflag=5
      stx=0d0
c----------------------------------------------------------------------
c      Error return if the search direction is not downhill
c      or if step is not positive
c----------------------------------------------------------------------
      if (dmax1(dg,-step).ge.0d0) go to 250
c----------------------------------------------------------------------
c      Set some parameters to begin the calculation
c----------------------------------------------------------------------
      dgin2=dnor
      dginit=-dg
      stinit=step
      stepca=step
      stepla=0d0
      stinbd=3d0*step
      go to 70
c----------------------------------------------------------------------
c      Test for convergence
c----------------------------------------------------------------------
170   ww=dg-dgx
      if (f.gt.fx) go to 200
      iflag=0
      if (dabs(dg).lt.1.d-15) go to 260
      dgpa=dabs(dgg/dnds)
      if(dgpa.le.grhtol* dgin2) go to 260
      if (dabs(dg).le.0.25*dginit) go to 260
      if (dabs(dg).le.0.7*dginit.and. nf.gt. min(15,nmax/2))go to 260
c----------------------------------------------------------------------
c      Revise the bound on step when the new f is small
c----------------------------------------------------------------------
      if(dg.gt.0d0) stu=step
      if(dg.lt.0d0) stl=step
      if(dgx*dg.lt.0d0) go to 210
c----------------------------------------------------------------------
c      Set the bound on step for extrapolation
c----------------------------------------------------------------------
      stepbd=5d-1*(stl+stu)
      if(stu.le.0d0) stepbd=stmult*stp
      if(dabs(dg).ge.dabs(dgx)) go to 15
c----------------------------------------------------------------------
c      Calculate the step lenght from the linear fit
c      to the gradients.
c----------------------------------------------------------------------
190   c=-dgx/ww
      stepl=stx+c*h
      if (stepbd.le.0d0) go to 30
c----------------------------------------------------------------------
c      Extrapolate by the perturbation method
c----------------------------------------------------------------------
      cc=2d0*(f-fx)-h*(dg+dgx)
      if (cc.lt.0d0) go to 220
      stepc=stepl+3d0*c*(c-1d0)*cc/ww
      go to 30
c----------------------------------------------------------------------
c      Revise the bounds on step when the new f is large
c----------------------------------------------------------------------
200   if (step.gt.stx) stu=step
      if(step.lt.stx) stl=step
c----------------------------------------------------------------------
c      Calculate the lenght from the cubic polynomial fit
c----------------------------------------------------------------------
210   stepbd=0d0
220   c=3d0*(f-fx)/h-dgx-dg
      cc=dsign(dsqrt(dabs(c*c-dg*dgx)),h)
      stepc=5d-1*(stx+step)-h*(5d-1*(dg+dgx)+c)/(ww+cc+cc)
      if(stepbd.gt.0d0) go to 30
      if (dgx*dg.ge.0d0) go to 20
      go to 190
c----------------------------------------------------------------------
c      Restore the best xmu, f, xf, grf, y and gy at an error return
c----------------------------------------------------------------------
230   if (dabs(stp-stx).gt. 0.d0) then
            f=fx
            xmu=w1
            do  i=1,n
                grf(i)=wa(nma1+i)
                xf(i)=wa(nma2+i)
                qs(i)=wa(nma3+i)
                y(i)=wa(nma4+i)
                gy(i)=wa(nma5+i)
            end do
      else
            go to 260
      end if
250   step=stx
260   continue
      return
      end
c  ********************************************************************
c----------------------------------------------------------------------
c      The subroutine deriv computes the derivative of the
c      objective function with regard to the parameter of
c      the Levenberg-Marquardt curve at the point x1=x0+s(al)
c----------------------------------------------------------------------
      subroutine deriv(gradie,n,x1,y,bl,bu,qt,d,e,qs,al,gra1,gy,
     *                  sder,der,wa,ibound,jbound)
c----------------------------------------------------------------------
c                       Arguments
c                       ---------
c     Input:
c
c    gradie:  see description in the main routine.
c         n:  dimension of the problem.
c        x1:  point on the curve x0+s(al).
c         y:  coordinates of x1 in the original problem.
c        bl:  vector of lower bounds.
c        bu:  vector of upper bounds.
c        Qt:  orthogonal matrix of the decomposition Qt*T*Q.
c         d:  diagonal of the tridiagonal matrix T.
c         e:  subdiagonal of the tridiagonal matrix T.
c        qs:  Q*s(al).
c        al:  parameter corresponding to the point x1.
c        wa:  working vector.
c    ibound:  see definition in the main routine.
c    jbound:  see definition in the main routine.
c
c    Output:
c
c      gra1:  gradient at x1.
c        gy:  gradient at y.
c      sder:  vector of dimension n, tangent to the curve at x1: s'(al).
c       der:  derivative of f with respect to the parameter al, at x1.
c             der=gra1*s'(al).
c----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension qt(n,n),d(n),e(n),qs(n),x1(n),gra1(n),y(n),gy(n),bl(n),
     *          bu(n),sder(n),wa(1),jbound(n)
      nt1=0
      nt2=nt1+n
      nqsn=nt2+n
c----------------------------------------------------------------------
c       Set parameters for tri1
c----------------------------------------------------------------------
      do i=1,n-1
            wa(nt1+i)=e(i+1)
            wa(nt2+i)=d(i)+al
            wa(nqsn+i)=-qs(i)
      end do
      wa(nt1+n)=0.d0
      wa(nt2+n)=d(n)+al
      wa(nqsn+n)=-qs(n)
c----------------------------------------------------------------------
c      Solve the system for finding the vector Q*s'(al)
c      and compute the gradient at x1.
c----------------------------------------------------------------------
      call tri1(n,wa(nt2+1),wa(nt1+1),wa(nqsn+1))
      do i=1,n
            sdx=0.d0
            do j=1,n
                sdx=sdx+qt(i,j)*wa(nqsn+j)
            end do
            sder(i)=sdx
      end do

      if(ibound.eq.0)then
            call gradie(n,x1,gra1)
      else
            call gradie(n,y,gy)
            call gradx(n,x1,bl,bu,gy,gra1,jbound)
      end if

c----------------------------------------------------------------------
c      Compute the derivative of f with regard to the
c      parameter al.
c----------------------------------------------------------------------
      der=ddotsa(n,gra1,1,sder,1)
      return
      end
c  ********************************************************************
      integer function ierst(n,bl,bu,x,f,gx,gz,z,eps,nfu,ngr,
     * gp,fun,gradie,ibound,jbound)
c----------------------------------------------------------------------
c                       Arguments
c                       ---------
c    Input :
c
c        n :  number of variables.
c       bl :  vector of lower bounds.
c       bu :  vector of upper bounds.
c        x :  current iterate of the unconstrained problem.
c        f :  f(x).
c       gx :  gradient at x.
c        z :  coordinates of the point x in the constrained problem.
c       gz :  gradient at z.
c      eps :  tolerance for the optimality test.   
c      nit :  number of iterations.
c      nfu :  number of function evaluations.
c      ngr :  number of gradient evaluations.
c      fun :  external function (see main routine).
c   gradie :  external function (see main routine).
c   ibound :  see definition in the main routine.
c   jbound :  see definition in the main routine.
c
c   Output :
c
c        x :  current iterate or a modification of it.
c        f :  f(x).
c       gx :  gradient at x.
c        z :  coordinates of x in the constrained problem. 
c       gz :  gradient at z .
c       gp :  projection of gz onto the domain of the original problem.
c      nfu :  number of function evaluations.
c      ngr :  number of gradient evaluations.
c        
c----------------------------------------------------------------------
      implicit real*8(a-h,o-z)
      dimension x(n),z(n),gx(n),bl(n),bu(n),gz(n),gp(n),jbound(n)
      common/npar/maxfun
      external fun,gradie 
      ierst=-1
c----------------------------------------------------------------------      
c      Optimality test 
c----------------------------------------------------------------------
10    smax=0.d0                                                         
      s1=dmax1(dabs(f),1.d0)                                            
      do i=1,n
            s2=dmax1(dabs(x(i)),1.d0)                                  
            s3= dabs(gx(i))*s2/s1                                      
            if(s3.gt.smax)smax= s3
      end do
c----------------------------------------------------------------------    
c      Optimality test for ibound=0 
c----------------------------------------------------------------------  
      if(ibound.eq.0)then
	if(smax.le.eps)then                                             
        	        smax=dnrm2(n,gx,1)
                    ierst=0
        	        return
      	else
                    go to 20
            end if
      else
c----------------------------------------------------------------------
c      The projection of gz onto the domain of the original problem
c      is calculated, and the optimality test is performed. If
c      satisfied, return.
c----------------------------------------------------------------------
            do i=1,n
                    gp(i)=gz(i)
                    if(jbound(i).eq.1.or.jbound(i).eq.3)then
                        bcot=dmax1(1.d-5,dabs(bu(i))* 1.d-8)
                        if(dabs(z(i)-bu(i)).le.bcot.and.gz(i).lt.0.d0)
     *                  gp(i)=0.d0
                    end if
                    if(jbound(i).gt.1)then
                        bcoi=dmax1(1.d-5,dabs(bl(i))*1.d-8)  
                        if(dabs(z(i)-bl(i)).le.bcoi.and.gz(i).gt.0.d0) 
     *                  gp(i)=0.d0  
                    end if
            end do
            smaz=0.d0
            smgp=dnrm2(n,gp,1)
            smgx=dnrm2(n,gx,1)
            do i=1,n
                    sz=dmax1(dabs(z(i)),1.d0)                                  
                    si= dabs(gp(i))*sz/s1                                      
                    if(si.gt.smaz)smaz= si
            end do
                    if(smaz.le.eps.or.smgp.le.eps)then                 
                        ierst=0
                        return
                    else
                        if(dmin1(smgx,smax).le.eps)then
                            if(dmin1(smgx,smax).gt.1.d-2*eps.and.
     *                      dmin1(smaz,smgp).lt.1.d2*eps) go to 20 
c----------------------------------------------------------------------
c      If the norm of the gradient at x is too small when
c      compared with the one of the projected gradient gp at z,
c      the current iterate is perturbed.
c      z, x, f, gz, gx at the perturbed point are computed.   
c----------------------------------------------------------------------
            if(dmin1(smgx,smax).lt.1.d-4*dmin1(smaz,smgp))then 
                    ierst=-2
                    call transf2(n,bl,bu,gp,z,x,f,fun,jbound,nfun)
                    call gradie(n,z,gz)
                    call gradx(n,x,bl,bu,gz,gx,jbound)
                    nfu=nfu+1
                    ngr=ngr+1
c----------------------------------------------------------------------
c      Analize if the optimality criterion is satisfied.
c----------------------------------------------------------------------
                   go to 10
                   end if
            end if
      end if
      end if
c----------------------------------------------------------------------
c      return if the maximum number of 
c      function evaluations is exceeded.
c----------------------------------------------------------------------
20    if(nfu.ge.maxfun)then                                        
            ierst=1                                                      
      end if 
      return
      end
c----------------------------------------------------------------------
c    This subroutine computes the sparse secant update  Tk+1,
c    using the least-change theory.
c----------------------------------------------------------------------
      subroutine updat(n,bl,bu,d,e,qt,sq,xk,gradie,ng,gk,gkp1,
     *                  wa,ibound,jbound)
c----------------------------------------------------------------------
c                       Arguments
c                       ---------
c     Input:
c
c        n :  dimension of the problem
c       bl :  vector of lower bounds
c       bu :  vector of upper bounds
c        d :  diagonal of the tridiagonal matrix Tk
c        e :  subdiagonal of the tridiagonal matrix Tk
c       qt :  orthogonal matrix of the factorization Qt*Tk*Q at xk
c       sq :  vector Q*s where, s= xc-xk, xc is the current iterate
c       xk :  previous iterate
c    gradie:  user supplied function which computes the gradient
c       gk :  gradient at xk
c     gkp1 :  gradient at xk+s
c       wa :  working vector
c    ibound:  see definition in the main routine.
c    jbound:  see definition in the main routine.
c
c    Output:
c
c         d:   diagonal of the updated matrix Tk+1
c         e:   subdiagonal of the matrix Tk+1
c        ng:   number of gradient evaluations needed for ensuring
c              the 'bounded norm condition' of Tk
c              (see the reference).
c----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      external gradie
      dimension d(n),e(n),qt(n,n),sq(n),xk(n),gk(n),gkp1(n),wa(1),
     *          bl(n),bu(n),jbound(n)
      data tol/0.10842022d-18/

      np1=0
      np2=np1+n
      nyq=np2+n
      nwt=nyq+n
      nwt2=nyq
      nwt1=nwt+1
      nwt21=nwt2+1
      nwt3=nwt+n
      nwt31=nwt3+1
      nwty=nwt3+n
      nwty1=nwty+1
      nwgy=nwty+n
      nwgy1=nwgy+1
      ng=0
      nm1=n-1
      xr=dnrm2(n,sq,1)
      sts=xr*xr
      axr=tol*xr
      do j=1,n
            wa(nwt3+j)=gkp1(j)
      end do
      ma=1
c----------------------------------------------------------------------
c      Analize if the condition sq(i+1)**2+sq(i)**2 > 0 holds.
c----------------------------------------------------------------------
      do while (ma.eq.1)
            ma=0
            i=2
            s1=sq(1)*sq(1)
            si=s1
      do while (i.le.n.and.ma.eq.0)
            sii=si
            si=sq(i)*sq(i)
      if (dsqrt(sii+si).ge.axr) then
            i=i+1
      else
            ma=1
      end if
      end do
      if (dsqrt(s1+si).lt.axr) ma=1
c----------------------------------------------------------------------
c      If necessary, modify the step sq and in such a case
c      calculate the gradient at xk+Qt*sq
c----------------------------------------------------------------------
      if (ma.eq.1) then
            drn=dsqrt(dfloat(n))
            do  ik=1,n
                    sq(ik)=xr/drn
            end do
            call matmul(n,qt,sq,1,wa(nwt21))
            do kl=1,n
                    wa(nwt+kl)=xk(kl)+wa(nwt2+kl)
            end do
            if (ibound.eq.0)then
            call gradie(n,wa(nwt1),wa(nwt31))
            else
            call transf(n,1,wa(nwty1),bl,bu,2,wa(nwt1),jbound,ir)
            call gradie(n,wa(nwty1),wa(nwgy1))
            call gradx(n,wa(nwt1),bl,bu,wa(nwgy1),wa(nwt31),jbound)
            end if
            ng=ng+1
      end if
      end do
c----------------------------------------------------------------------
c      Restore qy=Q*(grad(xk+Qt*sq)-gk)
c----------------------------------------------------------------------
      do  j=1,n
               wa(nwt+j)=wa(nwt3+j)-gk(j)
      end do
      call matmul(n,qt,wa(nwt1),2,wa(nyq+1))
c----------------------------------------------------------------------
c      Compute the elements of the matrix P and set the
c      parameters for Tri1
c----------------------------------------------------------------------
      wa(np1+1)=0.d0
      a2=s1/sts
      wa(np2+1)=2.d0*a2
      wa(nyq+1)=wa(nyq+1)-d(1)*sq(1)
      if(n.ge. 2)then
            a3=sq(2)*sq(2)/sts
            wa(np2+1)=wa(np2+1)+a3
            wa(nyq+1)=wa(nyq+1)-e(2)*sq(2)
            if(n.gt.2)then
                do i=2,nm1
                        a1=a2
                        a2=a3
                        a3=sq(i+1)**2 /sts
                        wa(np1+i)=sq(i)*sq(i-1)/sts
                        wa(np2+i)=a1+2.d0*a2+a3
                        wa(nyq+i)=wa(nyq+i)-e(i)*sq(i-1)
     *                  -e(i+1)*sq(i+1)-d(i)*sq(i)
                end do
            end if
            wa(np1+n)=sq(n)*sq(n-1)/sts
            wa(np2+n)=a2+2.d0*a3
            wa(nyq+n)=wa(nyq+n)-e(n)*sq(n-1)-d(n)*sq(n)
            do i=2,n
                wa(np1+i-1)=wa(np1+i)
            end do
            wa(np1+n)=0.d0
       end if
c----------------------------------------------------------------------
c      Solve the tridiagonal system P*v= qy - Tk*sq
c----------------------------------------------------------------------
       call tri1(n,wa(np2+1),wa(np1+1),wa(nyq+1))
c----------------------------------------------------------------------
c      Find the diagonal and subdiagonal of Tk+1
c      using Tk+1 = Tk+ Proj( sq* vt + v* sqt), where
c      Proj is the orthogonal projector onto the subspace
c      of the symmetric tridiagonal matrices.
c----------------------------------------------------------------------
      d(1)=d(1)+2.0d0*wa(nyq+1)*sq(1)/sts
      if(n.ge.2)then
            do  i=2,n
                d(i)=d(i)+2.d0*wa(nyq+i)*sq(i)/sts
                e(i)=e(i)+ (wa(nyq+i)*sq(i-1)+wa(nyq+i-1)*sq(i))/sts
            end do
      end if
      return
      end
