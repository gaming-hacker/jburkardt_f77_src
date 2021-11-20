      program main

c*******************************************************************************
c
cc EXAMPLE01 is an example 1D problem for CLAWPACK.
c
c  Discussion:
c
c    The 1D constant velocity advection equation
c
c      Qt + U * Qx = 0
c
c    A data file named "claw1ez.data" must be available when this program
c    is run, since the CLAW1EZ routine will read it.
c
c    A second data file named "setprob.data" must be available to
c    be read by SETPROB.
c
c    The array AUX is set up for user convenience, allowing auxilliary
c    arrays to be defined.  In this case, we don't need any, so we simply
c    give AUX a dummy dimension of 1.  If we needed auxilliary arrays,
c    AUX might have a dimension like 
c
c      double precision aux(1-mbc:maxmx+mbc, maux)
c
c  Modified:
c
c    13 April 2006
c
c  Parameters:
c
c    You probably only need to set the following variables: 
c      MAXMX (number of spatial points),
c      MEQN (number of equations)!
c
c    * MAUX is the number of auxilliary variables.  You probably aren't using
c    any, so set this to 0!
c
c    * MAXMX is the maximum number of cells.  In other words, this limits the
c    number of spatial points you are allowed to use to represent your solution.
c    You actually choose a specific number MX of spatial points MX in your input
c    file.  That number MX must be no greater than the value MAXMX you
c    specify here.
c
c    * MBC should be set to 2.  This controls the number of extra cells used to
c    handle the boundary conditions.
c
c    * MEQN is the number of equations, or state variables, that you are modeling.
c    The simplest scalar equation will have MEQN = 1.
c
c    * MWAVES is the number of waves produced in each Riemann solution.  For most
c    applications, this will be the same as MEQN.
c
c    * MWORK is the size of the work array.  It must be at least
c    ( MAXMX + 2 * MBC ) * ( 2 + 4 * MEQN + MWAVES + MEQN * MWAVES ).
c    The assignment statement below automatically sets up this value for you.
c
      implicit double precision (a-h,o-z)

      integer maux
      integer maxmx
      integer mbc
      integer meqn
      integer mwaves
      integer mwork

      parameter ( maux = 0 )
      parameter ( maxmx = 500 )
      parameter ( mbc = 2 )
      parameter ( meqn = 1 )
      parameter ( mwaves = 1 )
 
      parameter ( mwork = ( maxmx + 2 * mbc ) 
     &                 * ( 2 + 4 * meqn + mwaves + meqn * mwaves ) )

      double precision aux(1)
      integer mthlim(mwaves)
      double precision q(1-mbc:maxmx+mbc, meqn)
      double precision work(mwork)

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EXAMPLE01'
      write ( *, '(a)' ) '  A 1D example of the use of CLAWPACK.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  The 1D constant velocity advection equation: '
      write ( *, '(a)' ) '  Q_t + U * Q_x = 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  We call CLAW1EZ to do the work.'

      call claw1ez ( maxmx, meqn, mwaves, mbc, maux, mwork, mthlim,
     &  q, work, aux )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EXAMPLE01'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop 
      end
      subroutine qinit ( maxmx, meqn, mbc, mx, xlower, dx, q, 
     &  maux, aux )

c*******************************************************************************
c
cc QINIT sets the initial conditions for Q.
c
c  Modified:
c
c    04 April 2006
c
      implicit double precision (a-h,o-z)

      integer maux
      integer maxmx
      integer mbc
      integer meqn

      double precision aux(1-mbc:maxmx+mbc, *)
      double precision beta
      double precision dx
      integer i
      integer mx
      double precision pi2
      double precision q(1-mbc:maxmx+mbc, meqn)
      double precision xcell
      double precision xlower

      common /comic/ beta

      pi2 = 8.0D+00 * datan ( 1.0D+00 )

      do i = 1, mx
        xcell = xlower + ( i - 0.5D+00 ) * dx
        q(i,1) = dexp ( -beta * ( xcell - 0.3D+00 )**2 )
      end do

      return
      end
      subroutine rp1 ( maxmx, meqn, mwaves, mbc, mx, ql, qr, 
     &  auxl, auxr, wave, s, amdq, apdq )

c*******************************************************************************
c
cc RP1 solves Riemann problems for the 1D advection equation q_t + u*q_x = 0.
c
c  Discussion:
c
c    This routine computes values of WAVE(I,J), S(I,J), AMDQ(I,J), 
c    and APDQ(I,J) for each wave J, and at each cell interface I.
c
c    Although the user asked for MX cells, the boundary conditions require
c    that we add MBC (which is probably 2) cells at both ends, so there
c    are MBC + MX + MBC cells, numbered:
c    -1, 0, 1, 2, 3, ..., MX-1, MX, MX+1, MX+2.
c     
c    If you think about it, this means there are only MBC + MX + MBC - 1
c    interfaces!  Therefore, even though the four arrays have enough
c    space for 1 more entry, you only want to assign values for entries
c    I = 2-MBC through MX+MBC.  Entry I of the arrays is associated with 
c    interface I, that is, the place between cells I-1 and I.
c
c    For constant advection velocity u, passed in common block.
c
c    The advection speed u is passed in the common block comrp
c    On input, ql contains the state vector at the left edge of each cell
c              qr contains the state vector at the right edge of each cell
c    On output, wave contains the waves,
c               s the speeds,
c               amdq the  left-going flux difference  A^- \Delta q
c               apdq the right-going flux difference  A^+ \Delta q
c
c    Note that the i'th Riemann problem has left state qr(i-1,:)
c                                       and right state ql(i,:)
c    From the basic clawpack routine step1, rp is called with ql = qr = q.
c
c  Modified:
c
c    13 April 2006
c
      implicit double precision (a-h,o-z)

      integer maxmx
      integer mbc
      integer meqn
      integer mwaves

      double precision apdq(1-mbc:maxmx+mbc, meqn)
      double precision amdq(1-mbc:maxmx+mbc, meqn)
      integer i
      double precision ql(1-mbc:maxmx+mbc, meqn)
      double precision qr(1-mbc:maxmx+mbc, meqn)
      double precision u
      double precision s(1-mbc:maxmx+mbc, mwaves)
      double precision wave(1-mbc:maxmx+mbc, meqn, mwaves)

      common /comrp/ u
c
c  Compute the wave and speed.
c
      do i = 2-mbc, mx+mbc

        wave(i,1,1) = ql(i,1) - qr(i-1,1)
        s(i,1) = u
        amdq(i,1) = dmin1 ( u, 0.0D+00 ) * wave(i,1,1)
        apdq(i,1) = dmax1 ( u, 0.0D+00 ) * wave(i,1,1)

      end do

      return
      end
      subroutine setprob

c*******************************************************************************
c
cc SETPROB sets the velocity for scalar advection.
c
c  Discussion:
c
c    The value U is passed to the Riemann solver RP1 in a common block
c
c    The value BETA, the width of the initial Gaussian pulse,
c    is passed to QINIT in a common block.
c
c  Modified:
c
c    04 April 2006
c
      implicit none

      double precision beta
      double precision u

      common /comrp/ u
      common /comic/ beta

      open ( unit = 1, file = 'setprob.data', status = 'old',
     &  form = 'formatted' )

      read ( 1, * ) u
      read ( 1, * ) beta

      close ( unit = 1 )

      return
      end

