      subroutine b4step1 ( maxmx, mbc, mx, meqn, q,
     &  xlower, dx, t, dt, maux, aux )

c*********************************************************************72
c
cc B4STEP1 carries out initialization before each step in 1D.
c
c  Discussion:
c
c    called from claw1 before each call to step1.
c    use to set time-dependent aux arrays or perform other tasks
c    which must be done every time step.
c    dummy routine 
c
c  Modified:
c
c    05 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
c  Parameters:
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)

      return
      end
      subroutine bc1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt,mthbc)

c*********************************************************************72
c
cc BC1 implements standard boundary condition choices for 1D.
c
c  Discussion:
c
c    At each boundary  k = 1 (left),  2 (right):
c      mthbc(k) =  0  for user-supplied BC's (must be inserted!)
c               =  1  for zero-order extrapolation
c               =  2  for periodic boundary coniditions
c               =  3  for solid walls, assuming this can be implemented
c                     by reflecting the data about the boundary and then
c                     negating the 2'nd component of q.
c
c    Extend the data from the computational region
c         i = 1, 2, ..., mx2
c    to the virtual cells outside the region, with
c         i = 1-ibc  and   i = mx+ibc   for ibc=1,...,mbc
c
c  Modified:
c
c    05 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)

      dimension mthbc(2)
c
c  left boundary:
c
      go to (100,110,120,130) mthbc(1)+1

  100 continue
c
c  user-specified boundary conditions go here in place of error output
c
      write(6,*) '*** ERROR *** mthbc(1)=0 and no BCs specified in bc1'
      stop
      go to 199

  110 continue
c
c  zero-order extrapolation:
c
      do 115 m=1,meqn
         do 115 ibc=1,mbc
               q(1-ibc,m) = q(1,m)
  115       continue
      go to 199

  120 continue
c
c  periodic:  
c
      do m=1,meqn
        do ibc=1,mbc
          q(1-ibc,m) = q(mx+1-ibc,m)
        end do
      end do

      go to 199

  130 continue
c
c  solid wall (assumes 2'nd component is velocity or momentum in x):
c
      do m=1,meqn
         do ibc=1,mbc
               q(1-ibc,m) = q(ibc,m)
        end do
      end do
c
c  negate the normal velocity:
c
      do ibc=1,mbc
            q(1-ibc,2) = -q(ibc,2)
      end do
      go to 199

  199 continue
c
c  right boundary:
c
      go to (200,210,220,230) mthbc(2)+1

  200 continue
c
c  user-specified boundary conditions go here in place of error output
c
      write(6,*) '*** ERROR *** mthbc(2)=0 and no BCs specified in bc2'
      stop
      go to 299

  210 continue
c
c  zero-order extrapolation:
c
      do 215 m=1,meqn
         do 215 ibc=1,mbc
               q(mx+ibc,m) = q(mx,m)
  215       continue
      go to 299

  220 continue
c
c  periodic:  
c
      do 225 m=1,meqn
         do 225 ibc=1,mbc
               q(mx+ibc,m) = q(ibc,m)
  225       continue
      go to 299

  230 continue
c
c  solid wall (assumes 2'nd component is velocity or momentum in x):
c
      do m=1,meqn
         do ibc=1,mbc
               q(mx+ibc,m) = q(mx+1-ibc,m)
         end do
      end do

      do ibc=1,mbc
            q(mx+ibc,2) = -q(mx+1-ibc,2)
      end do

      go to 299

  299 continue

      return
      end
      subroutine claw1 ( maxmx, meqn, mwaves, mbc, mx, q, aux, xlower,
     &  dx, tstart, tend, dtv, cflv, nv, method, mthlim, mthbc, work,
     &  mwork, info, bc1, rp1, src1, b4step1 )

c*********************************************************************72
c
cc CLAW1 solves a hyperbolic system of conservation laws in one space dimension.
c
c  Discussion:
c
c    The equations have the general form 
c
c      capa * q_t + A q_x = psi
c
c    The "capacity function" capa(x) and source term psi are optional
c
c    For a more complete description see the documentation at
c      http://www.amath.washington.edu/~claw
c
c    Sample driver programs and user-supplied subroutines are available.
c    See the the directories claw/clawpack/1d/example* for some examples, and
c    codes in claw/applications for more extensive examples.
c
c    The user must supply the following subroutines:
c
c      bc1       subroutine specifying the boundary conditions.
c
c      rp1       subroutine specifying the Riemann solver.
c
c      b4step1   The routine b4step1 is called each time step and
c                can be supplied by the user in order to perform
c                other operations that are necessary every time
c                step.  For example, if the variables stored in
c                the aux arrays are time-dependent then these
c                values can be set.
c
c    In addition, if the equation contains source terms psi, then the user
c    must provide:
c
c      src1      subroutine that solves capa * q_t = psi over a single time step.
c
c    These routines must be declared EXTERNAL in the main program.
c    For description of the calling sequences, see below.
c
c    Dummy routines b4step1 and src1 are provided in this library.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
c  Parameters:
c
c    Input, integer MAXMX, the maximum number of interior grid points in X. 
c
c    Input, integer MEQN, the number of equations in the system of conservation laws.
c
c    Input, integer MWAVES, the number of waves that result from the solution 
c    of each Riemann problem.  Often, MWAVES = MEQN but for some problems 
c    these may be different.
c
c    Input, integer MBC, the number of "ghost cells" that must be added on to each
c    side of the domain to handle boundary conditions.  The cells actually in the 
c    physical domain are labelled from 1 to MX in X.  The arrays are dimensioned 
c    actually indexed from 1-MBC to MX+MBC.  For the methods currently implemented, 
c    MBC = 2 should be used.  If the user implements another method that has a 
c    larger stencil and hence requires more ghost cells, a larger value of MBC 
c    could be used.  Q is extended from the physical domain to the ghost cells by the
c    user-supplied routine BC1.
c
c    Input, integer MX, the number of grid cells in the X-direction, in the
c    physical domain.  In addition there are MBC grid cells along each edge of the 
c    grid that are used for boundary conditions.  MX <= MAXMX is required.
c 
c    q(1-mbc:maxmx+mbc, meqn) 
c        On input:  initial data at time tstart.
c        On output: final solution at time tend.
c        q(i,m) = value of mth component in the i'th cell.
c        Values within the physical domain are in q(i,m) 
c                for i = 1,2,...,mx
c        mbc extra cells on each end are needed for boundary conditions
c        as specified in the routine bc1.
c
c    aux(1-mbc:maxmx+mbc, maux)
c        Array of auxiliary variables that are used in specifying the problem.
c        If method(7) = 0 then there are no auxiliary variables and aux
c                         can be a dummy variable.
c        If method(7) = maux > 0 then there are maux auxiliary variables
c                         and aux must be dimensioned as above.
c
c        Capacity functions are one particular form of auxiliary variable.
c        These arise in some applications, e.g. variable coefficients in
c        advection or acoustics problems.
c        See Clawpack Note # 5 for examples.
c
c        If method(6) = 0 then there is no capacity function.
c        If method(6) = mcapa > 0  then there is a capacity function and
c            capa(i), the "capacity" of the i'th cell, is assumed to be
c            stored in aux(i,mcapa).
c            In this case we require method(7).ge.mcapa.
c
c    dx = grid spacing in x.  
c         (for a computation in ax <= x <= bx,  set dx = (bx-ax)/mx.)
c
c    tstart = initial time.
c
c    tend = Desired final time (on input).
c              If tend<tstart, then claw1 returns after a single successful
c                 time step has been taken (single-step mode).
c              Otherwise, as many steps are taken as needed to reach tend, 
c                 up to a maximum of nv(1).
c         = Actual time reached (on output).
c
c    dtv(1:5) = array of values related to the time step:
c               (Note: method(1)=1 indicates variable size time steps)
c         dtv(1) = value of dt to be used in all steps if method(1) = 0
c                = value of dt to use in first step if method(1) = 1
c         dtv(2) = unused if method(1) = 0.
c                = maximum dt allowed if method(1) = 1.
c         dtv(3) = smallest dt used (on output)
c         dtv(4) = largest dt used (on output)
c         dtv(5) = dt used in last step (on output)
c
c    cflv(1:4) = array of values related to Courant number:
c         cflv(1) = maximum Courant number to be allowed.  With variable
c                   time steps the step is repeated if the Courant
c                   number is larger than this value.  With fixed time
c                   steps the routine aborts.  Usually cflv(1)=1.0
c                   should work.
c         cflv(2) = unused if method(1) = 0.
c                 = desired Courant number if method(1) = 1.
c                   Should be somewhat less than cflv(1), e.g. 0.9
c         cflv(3) = largest Courant number observed (on output).
c         cflv(4) = Courant number in last step (on output).
c
c    nv(1:2) = array of values related to the number of time steps:
c         nv(1) = unused if method(1) = 0
c               = maximum number of time steps allowed if method(1) = 1
c         nv(2) = number of time steps taken (on output).
c
c    Input, integer METHOD(7), specifies the numerical method to use.
c    The recommended choice of methods for most problems is
c    METHOD(1) = 1,  METHOD(2) = 2.
c    METHOD(1)
c    = 0, if fixed size time steps are to be taken.  In this case, DT = dTV(1) 
c      in all steps.
c    = 1, if variable time steps are to be used.  In this case, DT = DTV(1) 
c      in the first step and thereafter the value cflv(2) is used to choose the
c      next time step based on the maximum wave speed seen in the previous step.  
c      Note that since this value comes from the previous step, the Courant 
c      number will not in general be exactly equal to the desired value
c      If the actual Courant number in the next step is greater than 1, then 
c      this step is redone with a smaller DT.
c    METHOD(2)
c    = 1 if Godunov's method is to be used, with no 2nd order corrections.
c    = 2 if second order correction terms are to be added, with
c      a flux limiter as specified by mthlim.  
c    METHOD(3)  is not used in one-dimension.
c    METHOD(4) 
c    = 0 to suppress printing
c    = 1 to print DT and Courant number every time step
c    METHOD(5) 
c    = 0 if there is no source term PSI.  In this case the subroutine SRC1 
c      is never called so a dummy parameter can be given.
c    = 1 if there is a source term.  In this case the subroutine SRC1 must 
c      be provided and a fractional step method is used.
c      In each time step the following sequence is followed:
c        call BC to extend data to ghost cells
c        call STEP1 to advance hyperbolic eqn by DT
c        call SRC1 to advance source terms by DT
c    = 2 if there is a source term and Strang splitting is to
c      be used instead of the Godunov splitting above.
c      In each time step the following sequence is followed:
c        call BC to extend data to ghost cells
c        call SRC1 to advance source terms by DT/2
c        call STEP1 to advance hyperbolic equation by DT
c        call SRC1 to advance source terms by DT/2
c      For most problems 1 is recommended rather than 2
c      since it is less expensive and works essentially as
c      well on most problems.
c    METHOD(6)
c    = 0 if there is no capacity function CAPA.
c    = MCAPA > 0 if there is a capacity function.  In this case
c      AUX(I,MCAPA) is the capacity of the I'th cell and you
c      must also specify METHOD(7) .ge. MCAPA and set AUX.
c    METHOD(7)
c      = 0 if there is no aux array used.
c      = MAUX > 0 if there are MAUX auxiliary variables.
c
c    mthlim(1:mwaves) = array of values specifying the flux limiter to be used
c                     in each wave family mw.  Often the same value will be used
c                     for each value of mw, but in some cases it may be
c                     desirable to use different limiters.  For example,
c                     for the Euler equations the superbee limiter might be
c                     used for the contact discontinuity (mw=2) while another
c                     limiter is used for the nonlinear waves.  Several limiters
c                     are built in and others can be added by modifying the
c                     subroutine philim.
c
c        mthlim(mw) = 0 for no limiter
c                   = 1 for minmod
c                   = 2 for superbee
c                   = 3 for van Leer
c                   = 4 for monotonized centered
c
c    work(mwork) = double precision work array of length at least mwork
c
c    mwork = length of work array.  Must be at least
c               (maxmx + 2*mbc) * (2 + 4*meqn + mwaves + meqn*mwaves)
c            If mwork is too small then the program returns with info = 4
c            and prints the necessary value of mwork to unit 6.
c
c            
c    info = output value yielding error information:
c         = 0 if normal return.
c         = 1 if mx.gt.maxmx   or  mbc.lt.2
c         = 2 if method(1)=0 and dt doesn't divide (tend - tstart).
c         = 3 if method(1)=1 and cflv(2) > cflv(1).
c         = 4 if mwork is too small.
c         = 11 if the code attempted to take too many time steps, n > nv(1).
c              This could only happen if method(1) = 1 (variable time steps).
c         = 12 if the method(1)=0 and the Courant number is greater than 1
c              in some time step.
c
c           Note: if info.ne.0, then tend is reset to the value of t actually
c           reached and q contains the value of the solution at this time.
c
c    User-supplied subroutines
c
c
c    bc1 = subroutine that specifies the boundary conditions.  
c         This subroutine should extend the values of q from cells
c         1:mx to the mbc ghost cells along each edge of the domain.
c
c          The form of this subroutine is
c
c     subroutine bc1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,mthbc)
c     implicit double precision (a-h,o-z)
c     dimension   q(1-mbc:maxmx+mbc, meqn)
c     dimension aux(1-mbc:maxmx+mbc, *)
c     dimension mthbc(2)
c
c
c    The routine claw/clawpack/1d/lib/bc1.f can be used to specify
c    various standard boundary conditions.
c
c
c    rp1 = user-supplied subroutine that implements the Riemann solver
c
c          The form of this subroutine is
c
c     subroutine rp1(maxmx,meqn,mwaves,mbc,mx,ql,qr,auxl,auxr,wave,s,amdq,apdq)
c     implicit double precision (a-h,o-z)
c     dimension   ql(1-mbc:maxmx+mbc, meqn)
c     dimension   qr(1-mbc:maxmx+mbc, meqn)
c     dimension auxl(1-mbc:maxmx+mbc, *)
c     dimension auxr(1-mbc:maxmx+mbc, *)
c     dimension wave(1-mbc:maxmx+mbc, meqn, mwaves)
c     dimension    s(1-mbc:maxmx+mbc, mwaves)
c     dimension amdq(1-mbc:maxmx+mbc, meqn)
c     dimension apdq(1-mbc:maxmx+mbc, meqn)
c
c
c         On input, ql contains the state vector at the left edge of each cell
c                   qr contains the state vector at the right edge of each cell
c                 auxl contains auxiliary values at the left edge of each cell
c                 auxr contains auxiliary values at the right edge of each cell
c
c         Note that the i'th Riemann problem has left state qr(i-1,:)
c                                            and right state ql(i,:)
c         In the standard clawpack routines, this Riemann solver is
c         called with ql=qr=q along this slice.  More flexibility is allowed
c         in case the user wishes to implement another solution method
c         that requires left and rate states at each interface.

c         If method(7)=maux > 0 then the auxiliary variables along this slice
c         are passed in using auxl and auxr.  Again, in the standard routines
c         auxl=auxr=aux in the call to rp1.
c
c          On output, 
c              wave(i,m,mw) is the m'th component of the jump across
c                              wave number mw in the ith Riemann problem.
c              s(i,mw) is the wave speed of wave number mw in the
c                              ith Riemann problem.
c              amdq(i,m) = m'th component of A^- Delta q,
c              apdq(i,m) = m'th component of A^+ Delta q,
c                     the decomposition of the flux difference
c                         f(qr(i-1)) - f(ql(i))
c                     into leftgoing and rightgoing parts respectively.
c
c           It is assumed that each wave consists of a jump discontinuity
c           propagating at a single speed, as results, for example, from a
c           Roe approximate Riemann solver.  An entropy fix can be included
c           into the specification of amdq and apdq.
c
c    src1 = subroutine for the source terms that solves the equation
c               capa * q_t = psi 
c           over time dt.
c
c           If method(5)=0 then the equation does not contain a source
c           term and this routine is never called.  A dummy argument can
c           be used with many compilers, or provide a dummy subroutine that
c           does nothing (such a subroutine can be found in
c           claw/clawpack/1d/lib/src1.f)
c
c          The form of this subroutine is
c
c     subroutine src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)
c     implicit double precision (a-h,o-z)
c     dimension   q(1-mbc:maxmx+mbc, meqn)
c     dimension aux(1-mbc:maxmx+mbc, *)
c
c      If method(7)=0  or the auxiliary variables are not needed in this solver,
c      then the latter dimension statement can be omitted, but aux should
c      still appear in the argument list.
c
c      On input, q(i,m) contains the data for solving the
c                source term equation.
c      On output, q(i,m) should have been replaced by the solution to
c                 the source term equation after a step of length dt.
c
c
c      b4step1 = subroutine that is called from claw1 before each call to
c                step1.  Use to set time-dependent aux arrays or perform
c                other tasks which must be done every time step.
c
c          The form of this subroutine is
c      
c
c      subroutine b4step1(maxmx,mbc,mx,meqn,q,xlower,dx,time,dt,maux,aux)
c      implicit double precision (a-h,o-z)
c      dimension   q(1-mbc:maxmx+mbc, meqn)
c      dimension aux(1-mbc:maxmx+mbc, *)
c
c
c  Copyright 1994 -- 2002 R. J. LeVeque
c
c  This software is made available for research and instructional use only. 
c  You may copy and use this software without charge for these non-commercial
c  purposes, provided that the copyright notice and associated text is
c  reproduced on all copies.  For all other uses (including distribution of
c  modified versions), please contact the author at the address given below. 
c  
c  This software is made available "as is" without any assurance that it
c  will work for your purposes.  The software may in fact have defects, so
c  use the software at your own risk.
c
c
c    CLAWPACK Version 4.1,  August, 2002
c    Webpage: http://www.amath.washington.edu/~claw
c
c    Author:  Randall J. LeVeque
c             Applied Mathematics
c             Box 352420
c             University of Washington, 
c             Seattle, WA 98195-2420
c             rjl@amath.washington.edu
c
c    Beginning of claw1 code
c
c 
      implicit double precision (a-h,o-z)

      external bc1,rp1,src1,b4step1
      dimension q(1-mbc:maxmx+mbc, meqn)
      dimension aux(1-mbc:maxmx+mbc, *)
      dimension work(mwork)
      dimension mthlim(mwaves),method(7),dtv(5),cflv(4),nv(2)
      dimension mthbc(2)
      common /comxt/ dtcom,dxcom,tcom

      info = 0
      t = tstart
      maxn = nv(1)
      dt = dtv(1)   !# initial dt
      cflmax = 0.d0
      dtmin = dt
      dtmax = dt
      nv(2) = 0
      maux = method(7)
c
c  check for errors in data:
c
      if (mx .gt. maxmx) then
         info = 1
         go to 900
         end if

      if (method(1) .eq. 0) then
c
c        # fixed size time steps.  Compute the number of steps:
c
         if (tend .lt. tstart) then
c
c             # single step mode
c
	      maxn = 1
           else
              maxn = (tend - tstart + 1d-10) / dt
              if (dabs(maxn*dt - (tend-tstart)) .gt.
     &                          1d-5*(tend-tstart)) then
c
c                # dt doesn't divide time interval integer number of times
c
                 info = 2
                 go to 900
                 end if
	   end if
         end if

      if (method(1).eq.1 .and. cflv(2).gt.cflv(1)) then
         info = 3
         go to 900
         end if
c
c  partition work array into pieces for passing into step1:
c
      i0f = 1
      i0wave = i0f + (maxmx + 2*mbc) * meqn
      i0s = i0wave + (maxmx + 2*mbc) * meqn * mwaves
      i0dtdx = i0s + (maxmx + 2*mbc) * mwaves
      i0qwork = i0dtdx + (maxmx + 2*mbc) 
      i0amdq = i0qwork + (maxmx + 2*mbc) * meqn
      i0apdq = i0amdq + (maxmx + 2*mbc) * meqn
      i0dtdx = i0apdq + (maxmx + 2*mbc) * meqn
      i0end = i0dtdx + (maxmx + 2*mbc) - 1

      if (mwork .lt. i0end) then
         write(6,*) 'mwork must be increased to ',i0end
         info = 4
         go to 900
         end if
c
c  main loop
c
      if (maxn.eq.0) go to 900
      do 100 n=1,maxn
         told = t   !# time at beginning of time step.

c        # adjust dt to hit tend exactly if we're near end of computation
c        #  (unless tend < tstart, which is a flag to take only a single step)
         if (told+dt.gt.tend .and. tstart.lt.tend) dt = tend - told

         if (method(1).eq.1) then
c
c           # save old q in case we need to retake step with smaller dt:
c
            call copyq1(maxmx,meqn,mbc,mx,q,work(i0qwork))
            end if
         
   40    continue
         dt2 = dt / 2.d0
         thalf = t + dt2  !# midpoint in time for Strang splitting
         t = told + dt    !# time at end of step
c
c        # store dt and t in the common block comxt in case they are needed
c        # in the Riemann solvers (for variable coefficients)
c
         tcom = told
         dtcom = dt
         dxcom = dx
c
c  main steps in algorithm:
c
c  extend data from grid to bordering boundary cells:
c
         call bc1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,told,dt,mthbc)
c
c  call user-supplied routine which might set aux arrays
c  for this time step, for example.
c

         call b4step1(maxmx,mbc,mx,meqn,q,
     &                xlower,dx,told,dt,maux,aux)

         if (method(5).eq.2) then
c            # with Strang splitting for source term:
             call src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,told,dt2)
             end if
c
c        # take a step on the homogeneous conservation law:
         call step1(maxmx,meqn,mwaves,mbc,mx,q,aux,dx,dt,
     &             method,mthlim,cfl,work(i0f),work(i0wave),
     &             work(i0s),work(i0amdq),work(i0apdq),work(i0dtdx),
     &             rp1)

         if (method(5).eq.2) then
c
c            # source terms over a second half time step for Strang splitting:
c            # Note it is not so clear what time t should be used here if
c            # the source terms are time-dependent!
c
             call src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,thalf,dt2)
             end if

         if (method(5).eq.1) then
c
c            # source terms over a full time step:
c
             call src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)
             end if

         if (method(4) .eq. 1) write(6,601) n,cfl,dt,t
  601    format('CLAW1... Step',i4,
     &                   '   Courant number =',f6.3,'  dt =',d12.4,
     &                   '  t =',d12.4)

         if (method(1) .eq. 1) then
c
c           # choose new time step if variable time step
c
            if (cfl .gt. 0.d0) then
                dt = dmin1(dtv(2), dt * cflv(2)/cfl)
                dtmin = dmin1(dt,dtmin)
                dtmax = dmax1(dt,dtmax)
              else
                dt = dtv(2)
              end if
            end if
c
c        # check to see if the Courant number was too large:
c
         if (cfl .le. cflv(1)) then
c
c               # accept this step
c
                cflmax = dmax1(cfl,cflmax)
              else
c
c               # reject this step
c
                t = told
                call copyq1(maxmx,meqn,mbc,mx,work(i0qwork),q)

                if (method(4) .eq. 1) then
                   write(6,602) 
  602              format('CLAW1 rejecting step... ',
     &                         'Courant number too large')
                   end if
                if (method(1).eq.1) then
c
c                   # if variable dt, go back and take a smaller step
c
                    go to 40
                  else
c
c                   # if fixed dt, give up and return
c
                    cflmax = dmax1(cfl,cflmax)
                    go to 900
                  end if
               end if
c
c        # see if we are done:
         nv(2) = nv(2) + 1
         if (t .ge. tend) go to 900

  100    continue

  900  continue
c 
c  return information
c
c  too many timesteps
c
       if (method(1).eq.1 .and. t.lt.tend .and. nv(2) .eq. maxn) then
          info = 11
       end if
c
c  Courant number too large with fixed dt
c
       if (method(1).eq.0 .and. cflmax .gt. cflv(1)) then
          info = 12
       end if

       tend = t
       cflv(3) = cflmax
       cflv(4) = cfl
       dtv(3) = dtmin
       dtv(4) = dtmax
       dtv(5) = dt
       return 
       end
      subroutine claw1ez ( maxmx, meqn, mwaves, mbc, maux, mwork, 
     &  mthlim, q, work, aux )

c*********************************************************************72
c
cc CLAW1EZ is an easy-to-use clawpack driver routine for simple applications.
c
c  Discussion:
c
c    This routine makes it easier to use the CLAW1 routine, by making many
c    choices for the user, using default values and supplying simple versions
c    of certain routines that would otherwise be user-supplied.
c
c    In the argument list for CLAW1EZ, the user supplies 6 numbers that
c    specify the sizes of certain arrays, and 4 arrays, which do not need to
c    be initialized.
c
c    The user also supplies a data file, named "claw1ez.data",
c    containing the value of about 16 variables (some of which are short arrays).
c
c    The user also supplies the routines QINIT, RP1 and SETPROB.
c
c    The entire computation is controlled by the CLAW1 routine.
c
c    Documentation is available at
c      http://www.amath.washington.edu/~claw/doc.html
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
c  Parameters:
c
c    Input, integer MAXMX, the maximum number of interior grid points in X. 
c
c    Input, integer MEQN, the number of equations in the system of conservation laws.
c
c    Input, integer MWAVES, the number of waves that result from the solution 
c    of each Riemann problem.  Often, MWAVES = MEQN but for some problems 
c    these may be different.
c
c    Input, integer MBC, the number of "ghost cells" that must be added on to each
c    side of the domain to handle boundary conditions.  The cells actually in the 
c    physical domain are labelled from 1 to MX in X.  The arrays are dimensioned 
c    actually indexed from 1-MBC to MX+MBC.  For the methods currently implemented, 
c    MBC = 2 should be used.  If the user implements another method that has a 
c    larger stencil and hence requires more ghost cells, a larger value of MBC 
c    could be used.  Q is extended from the physical domain to the ghost cells by the
c    library-supplied routine BC1.
c
c    Input, integer MAUX, the number of auxilliary variables.
c
c    Input, integer MWORK, the size of the work array WORK.
c    MWORK must be at least
c    ( MAXMX + 2 * MBC ) * (2 + 4 * MEQN + MWAVES + MEQN * MWAVES )
c
c    Work array, integer MTHLIM(MWAVES), space in which a limiter for each wave.
c    can be stored.  The actual values are read from the user's input data file.
c
c    Work array, double precision Q(1-MBC:MAXMX+MBC,MEQN), space in which
c    the value of Q may be stored.  The actual values are determined by calling
c    QINIT, and then by computation.
c
c    Work array, double precision WORK(MWORK).
c
c    Work array, double precision AUX(1-MBC:MAXMX+MBC,MAUX), space in which
c    the value of the auxilliary variables can be store.
c
      implicit double precision (a-h,o-z)

      integer maux
      integer maxmx
      integer mbc
      integer meqn
      integer mwaves
      integer mwork

      double precision aux(1-mbc:maxmx+mbc, maux)
      external b4step1
      external bc1
      double precision cflv(4)
      double precision dtv(5)
      integer method(7)
      integer mthbc(2)
      integer mthlim(mwaves)
      integer nv(2)
      logical outt0
      double precision q(1-mbc:maxmx+mbc, meqn)
      external rp1
      external src1
      double precision tout(100)
      double precision work(mwork)

      open(55,file='claw1ez.data',status='old',form='formatted')
      open(10,file='fort.info',status='unknown',form='formatted')
      open(11,file='fort.nplot',status='unknown',form='formatted')
c
c  Read the input in standard form from claw1ez.data:
c  For a description of input parameters see the documentation at
c  http://www.amath.washington.edu/~claw
c
c  MX = number of grid cells:
c
      read(55,*) mx
c
c  I/O variables.
c
      read(55,*) nout
      read(55,*) outstyle

      if (outstyle.eq.1) then
        read(55,*) tfinal
        nstepout = 1
      elseif (outstyle.eq.2) then
        read(55,*) (tout(i), i=1,nout)
        nstepout = 1
      elseif (outstyle.eq.3) then
        read(55,*) nstepout, nstop
	nout = nstop
      end if
c
c     timestepping variables.
c
      read(55,*) dtv(1)
      read(55,*) dtv(2)
      read(55,*) cflv(1)
      read(55,*) cflv(2)
      read(55,*) nv(1)
c
c  Input parameters for clawpack routines.
c
      read(55,*) method(1)
      read(55,*) method(2)
      read(55,*) method(3)
      read(55,*) method(4)
      read(55,*) method(5)
      read(55,*) method(6)  
      read(55,*) method(7) 

      read(55,*) meqn1
      read(55,*) mwaves1
      read(55,*) (mthlim(mw), mw=1,mwaves)
c
c  Physical domain:
c
      read(55,*) t0
      read(55,*) xlower
      read(55,*) xupper
c
c  Boundary conditions.
c
      read(55,*) mbc1
      read(55,*) mthbc(1)
      read(55,*) mthbc(2)

      close ( unit = 55 )
c
c  Now check the input.
c
      if (method(7) .ne. maux) then
         write(6,*) '*** ERROR ***  maux set wrong in input or driver'
         stop
         end if

      if (meqn1 .ne. meqn) then
         write(6,*) '*** ERROR ***  meqn set wrong in input or driver'
         stop
         end if
      if (mwaves1 .ne. mwaves) then
         write(6,*) '*** ERROR ***  mwaves set wrong in input or driver'
         stop
         end if
      if (mbc1 .ne. mbc) then
         write(6,*) '*** ERROR ***  mbc set wrong in input or driver'
         stop
         end if

      if ((mthbc(1).eq.2 .and. mthbc(2).ne.2) .or.
     &    (mthbc(2).eq.2 .and. mthbc(1).ne.2)) then
         write(6,*) '*** ERROR ***  periodic boundary conditions'
         write(6,*) ' require mthbc(1) and mthbc(2) BOTH be set to 2'
         stop 
         end if
c
c  Check that enough storage has been allocated:
c
      mwork1 = (maxmx + 2*mbc) * (2 + 4*meqn + mwaves + meqn*mwaves)

      if (mx.gt.maxmx .or. mwork.lt.mwork1) then
         maxmx1 = max0(mx,maxmx)

         mwork1 = (maxmx1 + 2*mbc) * (2 + 4*meqn + mwaves + meqn*mwaves)

         write(6,*) ' '
         write(6,*) '*** ERROR *** Insufficient storage allocated'
         write(6,*) 'Recompile after increasing values in driver.f:'
         write(6,611) maxmx1
         write(6,613) mwork1
 611     format(/,'parameter (maxmx = ',i5,')')
 613     format('parameter (mwork = ',i7,')',/)
         stop
         end if

      write(6,*) 'running...'
      write(6,*) ' '
c
c  Grid spacing.
c
      dx = (xupper - xlower) / float(mx)
c
c  Time increments between outputing solution.
c
      if (outstyle .eq. 1) then
         dtout = (tfinal - t0)/float(nout)
         end if

      write(11,1101) nout
 1101 format(i5)
c        
c  Call user's routine setprob to set any specific parameters
c  or other initialization required.
c
      call setprob
c        
c  Set auxilliary arrays.
c
      if ( maux .gt. 0 )  then
         call setaux1 ( maxmx, mbc, mx, xlower, dx, maux, aux )
      end if
c
c  Set initial conditions:
c
      call qinit ( maxmx, meqn, mbc, mx, xlower, dx, q, maux, aux )

      outt0 = .true.
c
c  Output the initial data.
c
      if (outt0) then
         call out1 ( maxmx, meqn, mbc, mx, xlower, dx, q, t0, 0 )
         write(6,601) 0, t0
      end if
c
c     Main loop:
c
      tend = t0

      do 100 n=1,nout
         tstart = tend
         if (outstyle .eq. 1)  tend = tstart + dtout
         if (outstyle .eq. 2)  tend = tout(n)
         if (outstyle .eq. 3)  tend = tstart - 1.d0  !# single-step mode

         call claw1(maxmx,meqn,mwaves,mbc,mx,
     &           q,aux,xlower,dx,tstart,tend,dtv,cflv,nv,method,mthlim,
     &           mthbc,work,mwork,info,bc1,rp1,src1,b4step1)
c
c  check to see if an error occured:
c
         if (info .ne. 0) then
            write(6,*) '*** ERROR in claw1 ***  info =',info
            if (info.eq.1) then
               write(6,*) '***   either mx > maxmx or mbc < 2'
               end if
            if (info.eq.2) then
               write(6,*) '***   dt does not divide (tend - tstart)'
               write(6,*) '***   and dt is fixed since method(1)=0'
               end if
            if (info.eq.3) then
               write(6,*) '***   method(1)=1 and cflv(2) > cflv(1)'
               end if
            if (info.eq.4) then
               write(6,*) '***   mwork is too small'
               end if
            if (info.eq.11) then
               write(6,*) '***   Too many times steps, n > nv(1)'
               end if
            if (info.eq.12) then
               write(6,*) 
     &          '***   The Courant number is greater than cflv(1)'
               write(6,*) '***   and dt is fixed since method(1)=0'
               end if

            go to 999
            end if

         dtv(1) = dtv(5)  !# use final dt as starting value on next call
c
c  output solution at this time
c
c  if outstyle=1 or 2, then nstepout=1 and we output every time
c  we reach this point, since claw1 was called for the entire time
c  increment between outputs.
c
c  if outstyle=3 then we only output if we have taken nstepout
c  time steps since the last output.
c
c  iframe is the frame number used to form file names in out1
c
         iframe = n/nstepout
	 if (iframe*nstepout .eq. n) then
            call out1(maxmx,meqn,mbc,mx,xlower,dx,q,tend,iframe)
            write(6,601) iframe,tend
            write(10,1010) tend,info,dtv(3),dtv(4),dtv(5),
     &           cflv(3),cflv(4),nv(2)
	    end if
c
c  formats for writing out information about this call to claw:
c
  601    format('CLAW1EZ: Frame ',i4,
     &           ' matlab plot files done at time t =',
     &           d12.4,/)

 1010    format('tend =',d15.4,/,
     &       'info =',i5,/,'smallest dt =',d15.4,/,'largest dt =',
     &       d15.4,/,'last dt =',d15.4,/,'largest cfl =',
     &         d15.4,/,'last cfl =',d15.4,/,'steps taken =',i4,/)

  100    continue

  999 continue

      return 
      end
      subroutine copyq1(maxmx,meqn,mbc,mx,q1,q2)

c*********************************************************************72
c
cc COPYQ1 copies the contents of q1 into q2
c
c  Discussion:
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q1(1-mbc:maxmx+mbc, meqn)
      dimension q2(1-mbc:maxmx+mbc, meqn)

      do i = 1-mbc, mx+mbc
        do m=1,meqn
          q2(i,m) = q1(i,m)
        end do
      end do

      return
      end
      subroutine limiter(maxm,meqn,mwaves,mbc,mx,wave,s,mthlim)

c*********************************************************************72
c
cc LIMITER applies a limiter to the waves.  
c
c  Discussion:
c
c     # Version of December, 2002.
c     # Modified from the original CLAWPACK routine to eliminate calls 
c     # to philim.  Since philim was called for every wave at each cell
c     # interface, this was adding substantial overhead in some cases.
c
c     # The limiter is computed by comparing the 2-norm of each wave with
c     # the projection of the wave from the interface to the left or
c     # right onto the current wave.  For a linear system this would
c     # correspond to comparing the norms of the two waves.  For a 
c     # nonlinear problem the eigenvectors are not colinear and so the 
c     # projection is needed to provide more limiting in the case where the
c     # neighboring wave has large norm but points in a different direction
c     # in phase space.
c
c     # The specific limiter used in each family is determined by the
c     # value of the corresponding element of the array mthlim.
c     # Note that a different limiter may be used in each wave family.
c
c     # dotl and dotr denote the inner product of wave with the wave to
c     # the left or right.  The norm of the projections onto the wave are then
c     # given by dotl/wnorm2 and dotr/wnorm2, where wnorm2 is the 2-norm
c     # of wave.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension mthlim(mwaves)
      dimension wave(1-mbc:maxm+mbc, meqn, mwaves)
      dimension    s(1-mbc:maxm+mbc, mwaves)
c
c
      do 200 mw=1,mwaves
         if (mthlim(mw) .eq. 0) go to 200
         dotr = 0.d0
         do 190 i = 0, mx+1
            wnorm2 = 0.d0
            dotl = dotr
            dotr = 0.d0
            do 5 m=1,meqn
               wnorm2 = wnorm2 + wave(i,m,mw)**2
               dotr = dotr + wave(i,m,mw)*wave(i+1,m,mw)
    5          continue
            if (i.eq.0) go to 190
            if (wnorm2.eq.0.d0) go to 190
c
            if (s(i,mw) .gt. 0.d0) then
                r = dotl / wnorm2
              else
                r = dotr / wnorm2
              end if
c
            go to (10,20,30,40,50) mthlim(mw)
c
   10       continue
c
c           # minmod
c
            wlimitr = dmax1(0.d0, dmin1(1.d0, r))
            go to 170
c
   20       continue
c
c           # superbee
c
            wlimitr = dmax1(0.d0, dmin1(1.d0, 2.d0*r), dmin1(2.d0, r))
            go to 170
c
   30       continue
c
c           # van Leer
c
            wlimitr = (r + dabs(r)) / (1.d0 + dabs(r))
            go to 170
c
   40       continue
c
c           # monotinized centered
c
            c = (1.d0 + r)/2.d0
            wlimitr = dmax1(0.d0, dmin1(c, 2.d0, 2.d0*r))
            go to 170
c
   50       continue
c
c           # Beam-Warming
c
            wlimitr = r
            go to 170
c
  170       continue
c
c           # apply limiter to waves:
c
            do 180 m=1,meqn
               wave(i,m,mw) = wlimitr * wave(i,m,mw)
  180          continue

  190       continue
  200    continue
c
      return
      end
      subroutine out1(maxmx,meqn,mbc,mx,xlower,dx,q,t,iframe)

c*********************************************************************72
c
cc OUT1 is an output routine for 1D.
c
c  Discussion:
c
c     # Write the results to the file fort.q<iframe>
c     # Use format required by matlab script  plotclaw1.m
c     # The same format is used by the amrclaw package.  
c     # Here it's adapted to output just the single grid.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, meqn)
      character*10 fname1, fname2
c
c     # first create the file name and open file
c
         fname1 = 'fort.qxxxx'
         fname2 = 'fort.txxxx'
         nstp = iframe
         do 55 ipos = 10, 7, -1
            idigit = mod(nstp,10)
            fname1(ipos:ipos) = char(ichar('0') + idigit)
            fname2(ipos:ipos) = char(ichar('0') + idigit)
            nstp = nstp / 10
 55      continue

         open(unit=50,file=fname1,status='unknown',form='formatted')
         open(unit=60,file=fname2,status='unknown',form='formatted')

c
c     # the following parameters are used in amrclaw where there are
c     # multiple grids.  Here they are all set to 1:
      ngrids = 1
      mptr = 1
      level = 1

      write(50,1001) mptr,level,mx
 1001 format(i5,'                 grid_number',/,
     &       i5,'                 AMR_level',/,
     &       i5,'                 mx')

      write(50,1002) xlower,dx
 1002 format(e18.8,'    xlow', /,
     &       e18.8,'    dx', /)
c
        do 10 i=1,mx
          do m=1,meqn
c            # exponents with more than 2 digits cause problems reading
c            # into matlab... reset tiny values to zero:
             if (dabs(q(i,m)) .lt. 1d-99) q(i,m) = 0.d0
             enddo
c
          write(50,1005) (q(i,m), m=1,meqn)
 1005     format(4e16.8)
c
 10       continue
        write(50,*) ' '
 20     continue
      write(50,*) ' '

      write(60,1000) t,meqn,ngrids
 1000 format(e18.8,'    time', /, 
     &       i5,'                 meqn'/,
     &       i5,'                 ngrids'/,/)
c

      close(unit=50)
      close(unit=60)

      return
      end
      subroutine setaux1(maxmx,mbc,mx,xlower,dx,maux,aux)

c*********************************************************************72
c
cc SETAUX1 sets auxiliary arrays for 1D.
c
c  Discussion:
c     
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension aux(1-mbc:maxmx+mbc, *)

      return
      end
      subroutine setprob

c*********************************************************************72
c
cc SETPROB can be used to set up problem parameters or read other data.
c
c  Discussion:
c
c    The user may supply a SETPROB routine, which will then be used instead of
c    this one.  In that case, the user's SETPROB routine should be used to
c    initialize various problem parameters, perhaps by reading data from a file,
c    and transmitting that data to other routines, perhaps through a common block.
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      return
      end
      subroutine src1(maxmx,meqn,mbc,mx,xlower,dx,q,maux,aux,t,dt)

c*********************************************************************72
c
cc SRC1 sets the source term for 1D.
c
c  Discussion:
c
c     do nothing... no source term
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension q(1-mbc:maxmx+mbc, meqn)

      return
      end
      subroutine step1(maxmx,meqn,mwaves,mbc,mx,q,aux,dx,dt,
     &              method,mthlim,cfl,f,wave,s,amdq,apdq,dtdx,rp1)

c*********************************************************************72
c
cc STEP1 takes one time step, updating Q, in 1D.
c
c  Discussion:
c
c     method(1) = 1   ==>  Godunov method
c     method(1) = 2   ==>  Slope limiter method
c     mthlim(p)  controls what limiter is used in the pth family
c
c
c     amdq, apdq, wave, s, and f are used locally:
c
c     amdq(1-mbc:maxmx+mbc, meqn) = left-going flux-differences
c     apdq(1-mbc:maxmx+mbc, meqn) = right-going flux-differences
c        e.g. amdq(i,m) = m'th component of A^- \Delta q from i'th Riemann
c                         problem (between cells i-1 and i).
c
c     wave(1-mbc:maxmx+mbc, meqn, mwaves) = waves from solution of
c                                           Riemann problems,
c            wave(i,m,mw) = mth component of jump in q across
c                           wave in family mw in Riemann problem between
c                           states i-1 and i.
c
c     s(1-mbc:maxmx+mbc, mwaves) = wave speeds,
c            s(i,mw) = speed of wave in family mw in Riemann problem between
c                      states i-1 and i.
c
c     f(1-mbc:maxmx+mbc, meqn) = correction fluxes for second order method
c            f(i,m) = mth component of flux at left edge of ith cell 
c
c  Modified:
c
c    07 April 2006
c
c  Author:
c
c    Randy LeVeque
c
c  Reference:
c
c    Randy LeVeque,
c    Finite Volume Methods for Hyperbolic Problems,
c    Cambridge University Press, 2002.
c
      implicit double precision (a-h,o-z)

      dimension    q(1-mbc:maxmx+mbc, meqn)
      dimension  aux(1-mbc:maxmx+mbc, *)
      dimension    f(1-mbc:maxmx+mbc, meqn)
      dimension    s(1-mbc:maxmx+mbc, mwaves)
      dimension wave(1-mbc:maxmx+mbc, meqn, mwaves)
      dimension amdq(1-mbc:maxmx+mbc, meqn)
      dimension apdq(1-mbc:maxmx+mbc, meqn)
      dimension dtdx(1-mbc:maxmx+mbc)
      dimension method(7),mthlim(mwaves)
      logical limit
c
c     # check if any limiters are used:
      limit = .false.
      do 5 mw=1,mwaves
         if (mthlim(mw) .gt. 0) limit = .true.
   5     continue
c
      mcapa = method(6)
      do 10 i=1-mbc,mx+mbc
         if (mcapa.gt.0) then
             if (aux(i,mcapa) .le. 0.d0) then
                write(6,*) 'Error -- capa must be positive'
                stop
                end if
             dtdx(i) = dt / (dx*aux(i,mcapa))
            else
             dtdx(i) = dt/dx
            end if
   10    continue
c
c
c
c     # solve Riemann problem at each interface 
c
c
      call rp1(maxmx,meqn,mwaves,mbc,mx,q,q,aux,aux,wave,s,amdq,apdq)
c
c     # Modify q for Godunov update:
c     # Note this may not correspond to a conservative flux-differencing
c     # for equations not in conservation form.  It is conservative if
c     # amdq + apdq = f(q(i)) - f(q(i-1)).
c
      do 40 i=1,mx+1
         do 40 m=1,meqn
            q(i,m) = q(i,m) - dtdx(i)*apdq(i,m)
            q(i-1,m) = q(i-1,m) - dtdx(i-1)*amdq(i,m)
   40       continue

c
c     # compute maximum wave speed:
      cfl = 0.d0
      do 50 mw=1,mwaves
         do 45 i=1,mx+1
c          # if s>0 use dtdx(i) to compute CFL,
c          # if s<0 use dtdx(i-1) to compute CFL:
           cfl = dmax1(cfl, dtdx(i)*s(i,mw), -dtdx(i-1)*s(i,mw))
   45      continue
   50    continue
c
      if (method(2) .eq. 1) go to 900
c
c     # compute correction fluxes for second order q_{xx} terms:
c
c
      do 100 m = 1, meqn
            do 100 i = 1-mbc, mx+mbc
               f(i,m) = 0.d0
  100          continue
c
c      # apply limiter to waves:
      if (limit) call limiter(maxmx,meqn,mwaves,mbc,mx,wave,s,mthlim)
c
      do 120 i=1,mx+1
         do 120 m=1,meqn
            do 110 mw=1,mwaves
               dtdxave = 0.5d0 * (dtdx(i-1) + dtdx(i))
               f(i,m) = f(i,m) + 0.5d0 * dabs(s(i,mw))
     &             * (1.d0 - dabs(s(i,mw))*dtdxave) * wave(i,m,mw)
  110          continue
  120       continue
c
c
  140 continue
c
c     # update q by differencing correction fluxes 
c
c
c     # (Note:  Godunov update has already been performed above)
c
      do 150 m=1,meqn
         do 150 i=1,mx
            q(i,m) = q(i,m) - dtdx(i) * (f(i+1,m) - f(i,m))
  150       continue
c
  900 continue
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
c  Modified:
c
c    16 September 2005
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

      character ( len = 8 ) date
      character ( len = 10 ) time

      call date_and_time ( date, time )

      write ( *, '(a8,2x,a10)' ) date, time

      return
      end
