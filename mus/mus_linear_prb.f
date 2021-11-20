      program main

c*********************************************************************72
c
cc MAIN is the main program for MUS_LINEAR_PRB.
c
c  Discussion:
c
c    MUS_LINEAR_PRB tests the MUS library.
c
c    Consider the ordinary differential equation of the general form:
c
c      dx(t) / dt = L(t)*x(t) + r(t),   0 <= t <= 6
c
c    and a boundary condition of the general form:
c
c      MA x(0) + MB x(6) = BCV  
c
c    where, for this problem, we have:
c
c      0 <= t <= 6
c
c             | 1-2cos(2t)    0   1+2sin(2t) |
c      L(t) = |   0           2     0        |
c             | -1+2sin(2t)   0   1+2cos(2t) |
c
c             | (-1+2cos(2t)-2sin(2t)) exp(t) |
c      r(t) = |       -exp(t)                 |
c             | ( 1-2cos(2t)-2sin(2t)) exp(t) |
c
c      MA = MB = I
c
c            | 1+exp(6) |
c      BCV = | 1+exp(6) |
c            | 1+exp(6) |
c 
c    The solution of this problem is:
c 
c             | exp(t) |
c      x(t) = | exp(t) |
c             | exp(t) |
c
c  Modified:
c
c    09 January 2013
c
c  Author:
c
c    Robert Mattheij, G.W.M. Staarink.
c
c  Reference:
c
c    Uri Ascher, Robert Mattheij, Robert Russell,
c    Numerical Solution of Boundary Value Problems for 
c    Ordinary Differential Equations,
c    Prentice Hall, 1988,
c    ISBN: 0-13-627266-5,
c    LC: QA379.A83.
c
      implicit none

      integer n
      parameter ( n = 3 )
      integer nti
      parameter ( nti = 15 )
      integer nu
      parameter ( nu = 6 )

      integer liw
      parameter ( liw = 3 * n )
      integer lw
      parameter ( lw = 8 * n + 2 * n * n )

      double precision a
      double precision ae
      double precision amp
      double precision b
      double precision bcv(n)
      double precision d(n,nti)
      double precision er(5)
      double precision exsol
      external fdif
      external flin
      integer i
      integer ierror
      integer ihom
      integer iw(liw)
      integer j
      integer k
      integer kpart
      double precision ma(n,n)
      double precision mb(n,n)
      integer nrti
      double precision phirec(nu,nti)
      double precision q(n,n,nti)
      double precision ti(nti)
      double precision u(nu,nti)
      double precision w(lw)
      double precision x(n,nti)

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MUS_LINEAR_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the MUS library on a linear problem.'
c
c  Set input parameters.
c
      ihom = 1
      ER(1) = 1.0D-11
      ER(2) = 1.0D-06
      ER(3) = 1.1D-16
      a = 0.D+00
      b = 6.D+00
      amp = 0.0D+00
      nrti = 10
c
c  Set the boundary condition matrices MA and MB.
c
      DO I = 1 , N
        DO J = 1 , N
          MA(I,J) = 0.0D+00
          MB(I,J) = 0.0D+00
        end do
        MA(I,I) = 1.D+00
        MB(I,I) = 1.D+00
      end do
c
c  Set the boundary condition vector BCV.
c
      BCV(1) = 1.0D+00 + DEXP(6.0D+00)
      BCV(2) = BCV(1)
      BCV(3) = BCV(1)
c
c  Call MUSL
c
      CALL MUSL ( FLIN, FDIF, N, IHOM, A, B, MA, MB, BCV, AMP, ER, 
     &  NRTI, TI, NTI, X, U, NU, Q, D, KPART, PHIREC, W, LW, IW, LIW, 
     &  IERROR )

      IF ( ( IERROR .NE. 0 ) .AND.
     &     ( IERROR .NE. 200 ) .AND. 
     &     ( IERROR .NE. 213 ) .AND.
     &     ( IERROR .NE. 240 ) ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MUS_LINEAR_PRB - Fatal error!'
        write ( *, '(a,i6)' ) '  Error code IERROR = ', ierror
        write ( *, '(a)' ) '  Abnormal end of execution.'
        write ( *, '(a)' ) ''
        call timestamp ( )
        stop
      end if
c
c  Compare the computed and exact solutions.
c
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Condition number =     ', er(4)
      write ( *, '(a,g14.6)' ) '  Amplification factor = ', er(5)
      write ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) '  I       T        APPROX. SOL.         '
     &  // 'EXACT SOL.        ABS. ERROR'
      write ( *, '(a)' ) ''
      DO K = 1 , NRTI
        EXSOL = DEXP(TI(K))
        AE = EXSOL - X(1,K)
        WRITE(*,'(i3,3x,f7.4,3(3x,d16.9))') K,TI(K),X(1,K),EXSOL,AE
        DO I = 2 , N
          AE = EXSOL - X(I,K)
          WRITE(*,'(13x,3(3x,d16.9))') X(I,K),EXSOL,AE
        end do
      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MUS_LINEAR_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine flin ( t, y, f )

c*********************************************************************72
c
cc FLIN evaluates the linear part of the ODE, the value of L(t)*y(t).
c
c  Modified:
c
c    09 January 2013
c
c  Author:
c
c    Robert Mattheij, G.W.M. Staarink.
c
c  Reference:
c
c    Uri Ascher, Robert Mattheij, Robert Russell,
c    Numerical Solutioni of Boundary Value Problems for 
c    Ordinary Differential Equations,
c    Prentice Hall, 1988,
c    ISBN: 0-13-627266-5,
c    LC: QA379.A83.
c
c  Parameters:
c
c    Input, double precision T, the location in [A,B].
c
c    Input, double precision Y(N), the solution value at T.
c
c    Output, double precision F(N), the value of L(T) * Y(T).
c
      implicit none

      double precision co
      double precision f(3)
      double precision si
      double precision t
      double precision ti
      double precision y(3)

      TI = 2.0D+00 * T
      SI = 2.0D+00 * DSIN(TI)
      CO = 2.0D+00 * DCOS(TI)

      F(1) = (1.0D+00 - CO) * Y(1) + (1.0D+00 + SI) * Y(3)
      F(2) = 2.0D+00 * Y(2)
      F(3) = (-1.0D+00 + SI) * Y(1) + (1.0D+00 + CO) * Y(3)

      return
      end
      subroutine fdif ( t, y, f )

c*********************************************************************72
c
cc FDIF evaluates the right hand side of the ODE, L(t)*y(t)+r(t).
c
c  Modified:
c
c    09 January 2013
c
c  Author:
c
c    Robert Mattheij, G.W.M. Staarink.
c
c  Reference:
c
c    Uri Ascher, Robert Mattheij, Robert Russell,
c    Numerical Solutioni of Boundary Value Problems for 
c    Ordinary Differential Equations,
c    Prentice Hall, 1988,
c    ISBN: 0-13-627266-5,
c    LC: QA379.A83.
c
c  Parameters:
c
c    Input, double precision T, the location in [A,B].
c
c    Input, double precision Y(N), the solution value at T.
c
c    Output, double precision F(N), the value of L(T) * Y(T) + R(T).
c
      implicit none

      double precision co
      double precision f(3)
      double precision si
      double precision t
      double precision ti
      double precision y(3)
c
c  Save time and have FLIN compute the linear part.
c
      CALL FLIN ( T, Y, F )
c
c  Add on the nonlinear part.
c
      TI = 2.0D+00 * T
      SI = 2.0D+00 * DSIN ( TI )
      CO = 2.0D+00 * DCOS ( TI )
      TI = DEXP ( T )

      F(1) = F(1) + ( -1.0D+00 + CO - SI ) * TI
      F(2) = F(2) - TI
      F(3) = F(3) + ( 1.0D+00 - CO - SI ) * TI

      RETURN
      END
