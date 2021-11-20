      program driver

c*******************************************************************************
c
cc EXAMPLE04 is an example 2D problem for CLAWPACK.
c
c  Discussion:
c
c    A data file named "claw2ez.data" must be available when this program
c    is run, since the CLAW2EZ routine will read it.
c
c    The array AUX is set up for user convenience, allowing auxilliary
c    arrays to be defined.  In this case, we don't need any, so we simply
c    give AUX a dummy dimension of 1.  If we needed auxilliary arrays,
c    AUX might have a dimension like 
c
c      dimension  aux(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, maux)
c
c  Modified:
c
c    12 April 2006
c
      implicit double precision (a-h,o-z)

      integer maux
      integer maxmx
      integer maxmy
      integer mbc
      integer meqn
      integer mwaves
      integer mwork

      parameter ( maux = 0 )   
      parameter ( maxmx = 200 )
      parameter ( maxmy = 200 )
      parameter ( mbc = 2 )
      parameter ( meqn = 1 )
      parameter ( mwaves = 1 )
      parameter ( mwork = 44472 )

      double precision aux(1)
      integer mthlim(mwaves)
      double precision q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      double precision work(mwork)

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EXAMPLE04'
      write ( *, '(a)' ) '  A 2D example of the use of CLAWPACK.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  u_t + (u^2)_x + (u^4)_y = 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  We use piecewise constant initial data.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  We call CLAW2EZ to do the work.'

      call claw2ez ( maxmx, maxmy, meqn, mwaves, mbc, maux, mwork,
     &  mthlim, q, work, aux )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EXAMPLE04'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop 
      end
      subroutine qinit ( maxmx, maxmy, meqn, mbc, mx, my, 
     &  xlower, ylower, dx, dy, q, maux, aux )

c*******************************************************************************
c
cc QINIT sets initial conditions for Q.
c
c  Discussion:
c
c    Sample scalar equation with data that is piecewise constant with
c    q = 1.0  if  0.1 < x < 0.6   and   0.1 < y < 0.6
c        0.1  otherwise
c
c  Modified:
c
c    12 April 2006
c
      implicit double precision (a-h,o-z)

      integer maux
      integer maxmx
      integer maxmy
      integer mbc
      integer meqn

      double precision aux(1)
      double precision dx
      double precision dy
      integer i
      integer j
      integer mx
      integer my
      double precision q(1-mbc:maxmx+mbc, 1-mbc:maxmy+mbc, meqn)
      double precision xi
      double precision xlower
      double precision yj
      double precision ylower

      do i=1,mx
        xi = xlower + (i-0.5d0)*dx
        do j=1,my
          yj = ylower + (j-0.5d0)*dy

          if ( xi.gt.0.1d0 .and. xi.lt.0.6d0 .and.
     &          yj.gt.0.1d0 .and. yj.lt.0.6d0 ) then
            q(i,j,1) = 1.d0
          else
            q(i,j,1) = 0.1d0
          end if

        end do
      end do

      return
      end
      subroutine rpn2 ( ixy, maxm, meqn, mwaves, mbc, mx, ql, qr,
     &  auxl, auxr, wave, s, amdq, apdq )

c*******************************************************************************
c
cc RPN2 is a Riemann solver for the sample scalar equation.
c
c    Riemann solver for the sample scalar equation
c     u_t + (u^2)_x + (u^4)_y = 0
c     
c    On input, ql contains the state vector at the left edge of each cell
c              qr contains the state vector at the right edge of each cell
c
c    This data is along a slice in the x-direction if ixy=1 
c                               or the y-direction if ixy=2.
c    On output, wave contains the waves, 
c               s the speeds, 
c   
c               amdq = A^- Delta q, 
c               apdq = A^+ Delta q, 
c                      the decomposition of the flux difference
c                          f(qr(i-1)) - f(ql(i))
c                      into leftgoing and rightgoing parts respectively.
c                  
c
c    Note that the i'th Riemann problem has left state qr(i-1,:)
c                                       and right state ql(i,:)
c    From the basic clawpack routines, this routine is called with ql = qr
c    maux=0 and aux arrays are unused in this example.
c
      implicit double precision (a-h,o-z)

      integer maxm
      integer maxm2
      parameter (maxm2 = 502)
      integer mbc
      integer meqn
      integer mwaves
      integer mx

      double precision amdq(1-mbc:maxm+mbc, meqn)
      double precision apdq(1-mbc:maxm+mbc, meqn)
      double precision auxl(1)
      double precision auxr(1)
      double precision df
      logical efix
      integer i
      integer ixy
      double precision ql(1-mbc:maxm+mbc, meqn)
      double precision qr(1-mbc:maxm+mbc, meqn)
      double precision s(1-mbc:maxm+mbc, mwaves)
      double precision stran(-1:maxm2)
      double precision wave(1-mbc:maxm+mbc, meqn, mwaves)

      common /comrp/ stran
c
c  Use entropy fix for transonic rarefaction.
c
      data efix /.true./
c
c  Solve Riemann problem in x-direction for u_t + (u^2)_x = 0:
c
      if (ixy.eq.1) then

          do i = 2-mbc, mx+mbc
c
c  wave is jump in q, speed comes from R-H condition:
c
             wave(i,1,1) = ql(i,1) - qr(i-1,1)
             s(i,1) = qr(i-1,1) + ql(i,1) 
c
c  compute left-going and right-going flux differences:
c
             df = ql(i,1)**2 - qr(i-1,1)**2

             if (s(i,1) .lt. 0.d0) then
               amdq(i,1) = df
               apdq(i,1) = 0.d0
             else
               amdq(i,1) = 0.d0
               apdq(i,1) = df
             endif
c
c  check for sonic point, in which case f(q^0) = 0:
c
             if (efix .and. qr(i-1,1).lt.0.d0 
     &                .and. ql(i,1).gt.0.d0) then
               amdq(i,1) = -qr(i-1,1)**2
               apdq(i,1) = ql(i,1)**2
             endif
c
c  set velocity in transverse direction, passed to rpt2:
c
             stran(i) = qr(i-1,1)**3 + ql(i,1)*qr(i-1,1)**2
     &               + ql(i,1)**2*qr(i-1,1) + ql(i,1)**3
        end do
c
c  Solve Riemann problem in y-direction for u_t + (u^4)_y = 0:
c
      else

          do i = 2-mbc, mx+mbc
c
c  wave is jump in q, speed comes from R-H condition:
c
             wave(i,1,1) = ql(i,1) - qr(i-1,1)
             s(i,1) = qr(i-1,1)**3 + ql(i,1)*qr(i-1,1)**2
     &               + ql(i,1)**2*qr(i-1,1) + ql(i,1)**3
c
c  compute left-going and right-going flux differences:
c
             df = ql(i,1)**4 - qr(i-1,1)**4

             if (s(i,1) .lt. 0.d0) then
               amdq(i,1) = df
               apdq(i,1) = 0.d0
             else
               amdq(i,1) = 0.d0
               apdq(i,1) = df
             end if
c
c  check for sonic point, in which case f(q^0) = 0:
c
             if (efix .and. qr(i-1,1).lt.0.d0 
     &                .and. ql(i,1).gt.0.d0) then
               amdq(i,1) = -qr(i-1,1)**4
               apdq(i,1) = ql(i,1)**4
             end if
c
c  set velocity in transverse direction, passed to rpt2:
c
             stran(i) = qr(i-1,1) + ql(i,1) 
        end do

      end if

      return
      end
      subroutine rpt2 ( ixy, maxm, meqn, mwaves, mbc, mx, ql, 
     &  qr, aux1, aux2, aux3, ilr, asdq, bmasdq, bpasdq ) 

c*******************************************************************************
c
cc RPT2 is a Riemann solver in the transverse direction for the scalar equation.
c
c    Riemann solver in the transverse direction for the scalar equation
c
c    Split asdq (= A^* \Delta q, where * = + or -)
c    into down-going flux difference bmasdq (= B^- A^* \Delta q)
c       and up-going flux difference bpasdq (= B^+ A^* \Delta q)
c
c    For the scalar equation, this simply amounts to computing the
c    transverse wave speed sb from the transverse Riemann problem and
c    setting bmasdq or bpasdq to sb*asdq.
c
      implicit double precision (a-h,o-z)

      integer maxm
      integer maxm2
      parameter (maxm2 = 502)
      integer mbc
      integer meqn
      integer mwaves
      integer mx

      double precision asdq(1-mbc:maxm+mbc, meqn)
      double precision aux1(1)
      double precision aux2(1)
      double precision aux3(1)
      double precision bmasdq(1-mbc:maxm+mbc, meqn)
      double precision bpasdq(1-mbc:maxm+mbc, meqn)
      integer i
      integer ilr
      double precision ql(1-mbc:maxm+mbc, meqn)
      double precision qr(1-mbc:maxm+mbc, meqn)
      double precision stran(-1:maxm2)

      common /comrp/ stran
c
c  transverse wave speeds have been computed in rpn2
c  maux=0 and aux arrays are unused in this example.
c
      do i = 2-mbc, mx+mbc
        bmasdq(i,1) = dmin1 ( stran(i), 0.d0 ) * asdq(i,1)
        bpasdq(i,1) = dmax1 ( stran(i), 0.d0 ) * asdq(i,1)
      end do

      return
      end
