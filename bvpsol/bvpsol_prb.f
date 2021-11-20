      program main

c*********************************************************************72
c
cc MAIN is the main program for the reentry problem.
c
c  Modified:
c
c    09 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      implicit none

      integer liwk
      parameter ( liwk = 6000 )
      integer lrwk
      parameter ( lrwk = 4000 )
      integer m
      parameter ( m = 6 )
      integer n
      parameter ( n = 7 )

      double precision betar
      external bldfx1
      double precision eps
      external f
      double precision fix
      integer i
      integer ifail
      integer ifcn
      integer info
      integer iopt(5)
      integer iw(liwk)
      integer j
      integer kprint
      integer m1
      integer naccpt
      integer ndec
      integer nrejct
      integer nsol
      integer nstep
      external r
      double precision rk
      double precision t1
      double precision t2
      double precision work(lrwk)
      double precision x(m)
      double precision y(n,m)

      common /konst/ rk,betar,fix
      common /dxstat/ ifcn,nstep,naccpt,nrejct,ndec,nsol

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVPSOL_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Reentry Forward Direction With Hamiltonian At T = 0'

      nstep = 0
      ifcn = 0
      kprint = 0

      rk = 1.0D+00 / 2.09D+02
      betar = 890.34D+00
      fix = degrees_to_radians ( 8.1D+00 )

      open ( 15, file = 'bvpsol_prb_data.txt', status = 'old' )
      read ( 15,*) (x(j),j=1,m)
      do j=1,m
        read(15,*)(y(i,j),i=1,4)
        read(15,*)(y(i,j),i=5,n)
      end do
      close(15)

      m1 = m - 1

      y(2,1) = -fix
      y(3,1) = 4.0d0 / 209.0d0
      y(3,m) = 2.5d0 / 209.0d0
      eps = 1.0d-6

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Calling BVPSOL with DIFEX1.'
c
c  MAXIT: Maximum number of iterations
c
      iopt(1) = 30
c
c  NONLIN: Highly nonlinear problem
c
      iopt(2) = 3
c
c  IBVPSL: 0 = use original BVPSOL code, 1 = use "BVPSOG" code
c
      iopt(3) = 1
c
c  IPRINT: Maximum level printout
c
      iopt(4) = 1
c
c  LUPRI: Print output unit
c
      iopt(5) = 6

      call zibsec ( t1, ifail )
      call bvpsol ( f, r, bldfx1, n, m, x, y, eps, iopt, info, lrwk, 
     &  work, liwk, iw )
      call zibsec ( t2, ifail )

      write ( *, '(a,i7)' ) '  IFCN = ', ifcn
      write ( *, '(a,g14.6)' ) '  Execution time = ', t2 - t1
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVPSOL_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      function degrees_to_radians ( degrees )

c*********************************************************************72
c
cc DEGREES_TO_RADIANS converts an angle measure from degrees to radians.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    01 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision DEGREES, the angle measure in degrees.
c
c    Output, double precision DEGREES_TO_RADIANS, the angle measure in radians.
c
      implicit none

      double precision degrees
      double precision degrees_to_radians
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )

      degrees_to_radians = ( degrees / 180.0D+00 ) * pi

      return
      subroutine f ( n, x, y, d )

c*********************************************************************72
c
cc F evaluates the ODE's for the reentry example.
c
c  Modified:
c
c    09 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
c  Parameters:
c
c    Input, integer N.
c
c    Input, double precision X.
c
c    Input, double precision Y(7).
c
c    Output, double precision D(7).
c
      implicit none

      integer n

      double precision betar
      double precision brxi
      double precision cdv
      double precision cdvv
      double precision cl
      double precision clv
      double precision cosxi
      double precision cosy2
      double precision d(n)
      double precision fix
      double precision q1
      double precision q2
      double precision q3
      double precision q4
      double precision rk
      double precision ro
      double precision s2mro
      double precision s2mrov
      double precision sinxi
      double precision siny2
      double precision u1
      double precision u2
      double precision u3
      double precision wurz
      double precision x
      double precision xi
      double precision y(n)
      double precision y1
      double precision y12
      double precision y1k
      double precision y2
      double precision y3
      double precision y4
      double precision y5
      double precision y6
      double precision y7

      common /konst/ rk,betar,fix

      y1 = y(1)
      y2 = y(2)
      y3 = y(3)
      y4 = y(4)
      y5 = y(5)
      y6 = y(6)
      y7 = y(7)

      y1k = 1.0d0 / y1
      y12 = y1*y1
      u1 = 0.6d0*y5
      u2 = 0.9d0*y1*y4
      u3 = -1.0d0 / dsqrt(u1*u1+u2*u2)
      brxi =-betar*y3
      brxi = min ( brxi, +150.0D+00 )
      brxi = max ( brxi, -150.0D+00 )
      ro = 2.704d-3*dexp(brxi)
      wurz = dsqrt(ro)*y12
      s2mro = 2.66d4*ro
      s2mrov = s2mro*y1
      cdv = s2mrov*(1.174d0-0.9d0*u2*u3)
      cl = s2mro*0.6d0*u1*u3
      clv = cl*y1
      cdvv = cdv*y1
      xi = 1.0d0 / (1.0d0+y3)
      q1 = y1*rk
      q2 = 3.2172d-4*xi
      q3 = q2*y1k
      q4 = q1-q3
      siny2 = dsin(y2)
      cosy2 = dcos(y2)
      sinxi = siny2*xi
      cosxi = cosy2*xi

      d(1)=-y7*(cdvv+q2*sinxi)
      d(2)=y7*(clv+cosxi*q4)
      d(3)=y7*q1*siny2
      d(4)=y7*(-30.0d0*wurz+2.0d0*y4*cdv-y5*(cl+cosxi*(rk+q3*y1k))
     &  -y6*rk*siny2)
      d(5)=y7*(y4*q2*cosxi+y5*sinxi*q4-y6*cosy2*q1)
      d(6)=y7*(5.0d0*betar*y1*wurz-y4*(betar*cdvv+2.0d0*q2*sinxi*
     &  xi)+y5*(betar*clv+cosxi*xi*(q4-q3)))
      d(7)=0.0d0

      return
      end
      subroutine r ( ya, yb, w )

c*********************************************************************72
c
cc R evaluates boundary conditions for the reentry example.
c
c  Modified:
c
c    09 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
c  Parameters:
c
c    Input, double precision YA(7), YB(7).
c
c    Output, double precision W(7).
c
      implicit none

      double precision betar
      double precision brxia
      double precision brxib
      double precision cdvva
      double precision cdvvb
      double precision clva
      double precision clvb
      double precision cosxia
      double precision cosxib
      double precision fix
      double precision q1a
      double precision q1b
      double precision q2a
      double precision q2b
      double precision rk
      double precision roa
      double precision rob
      double precision sina
      double precision sinb
      double precision w(7)
      double precision ya(7)
      double precision ya1
      double precision ya12
      double precision ya2
      double precision ya3
      double precision yb(7)
      double precision yb1
      double precision yb12
      double precision yb2
      double precision yb3

      double precision sinxia,sinxib,
     *s2mroa,s2mrob,u1a,u2a,u3a,u1b,u2b,u3b,xia,xib

      common /konst/ rk,betar,fix

      ya1 = ya(1)
      ya2 = ya(2)
      ya3 = ya(3)
      yb1 = yb(1)
      yb2 = yb(2)
      yb3 = yb(3)
      u1a = 0.6d0*ya(5)
      u2a = 0.9d0*ya(4)*ya1
      u3a = -1.0d0 / dsqrt(u1a*u1a+u2a*u2a)
      u1b = 0.6d0*yb(5)
      u2b = 0.9d0*yb(4)*yb1
      u3b = -1.0d0 / dsqrt(u1b*u1b+u2b*u2b)

      brxia = -890.34d0*ya3
      brxia = min ( brxia, +150.0D+00 )
      brxia = max ( brxia, -150.0D+00 )

      brxib = -890.34d0*yb3
      brxib = min ( brxib, +150.0D+00 )
      brxib = max ( brxib, -150.0D+00 )

      roa = 2.704d-3*dexp(brxia)
      rob = 2.704d-3*dexp(brxib)
      xia = 1.0d0 / (1.0d0+ya3)
      xib = 1.0d0 / (1.0d0+yb3)
      sina = dsin(ya2)
      cosxia = dcos(ya2)*xia
      sinxia = sina*xia
      ya12 = ya1*ya1
      s2mroa = roa*2.66d4
      cdvva = s2mroa*ya12*(1.174d0-0.9d0*u2a*u3a)
      clva = s2mroa*ya1*0.6d0*u1a*u3a
      q1a = 3.2172d-4*xia
      q2a = ya1 / 2.09d2
      sinb = dsin(yb2)
      cosxib = dcos(yb2)*xib
      sinxib = sinb*xib
      yb12 = yb1*yb1
      s2mrob = rob*2.66d4
      cdvvb = s2mrob*yb12*(1.174d0-0.9d0*u2b*u3b)
      clvb = s2mrob*yb1*0.6d0*u1b*u3b
      q1b = 3.2172d-4*xib
      q2b = yb1 / 2.09d2
      w(1)=yb(1)-0.27d0
      w(2)=yb(2)
      w(3)=yb(3)-2.5d0/209.0d0
      w(4)=ya(1)-0.36d0
      w(5)=ya(2)+fix
      w(6)=ya(3)-4.0d0/2.09d2
c
c  Hamilton function
c
      w(7)=10.0d0*ya12*ya1*dsqrt(roa)-ya(4)*(cdvva+q1a*sinxia)+ya
     &  (5)*(clva+cosxia*(q2a-q1a/ya1))+ya(6)*q2a*sina

      return
      end
