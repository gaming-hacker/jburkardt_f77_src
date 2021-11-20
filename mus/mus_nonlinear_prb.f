      program main

c*********************************************************************72
c
cc MAIN is the main program for MUS_NONLINEAR_PRB.
c
c  Discussion:
c
c    MUS_NONLINEAR_PRB tests the MUS library.
c
c    The problem to be solved is as follows:
c
C          X1' = A * X1 / X2 * (X3 - X1 )
C          X2' = - A * ( X3 - X1 )
C          X3' = 1 / X4 * ( B - C * ( X3 - X5 ) - A * X3 * ( X3 - X1 ) )
C          X4' = A * ( X3 - X1 )
C          X5' = - C / D * ( X5 - X3 )
C
C          X1(0) = 1
C          X2(0) = 1
C          X3(0) = 1
C          X4(0) = -10
C          X3(1) = X5(1)
C
C          A = 0.5
C          B = 0.9
C          C = 1000.0
C          D = 10.0
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
c    Michael Hanke, Rene Lamour, Renate Winkler,
c    The program system RWA for the solution of two-point 
c    boundary-value problems: foundations, algorithms, comparisons,
c    Seminarberichte 67,
c    Sektion Mathematik, Humboldt University.
c
      implicit none

      integer lwg
      parameter ( lwg = 20 )
      integer n
      parameter ( n = 5 )
      integer nti
      parameter ( nti = 12 )
      integer nu
      parameter ( nu = 15 )

      integer liw
      parameter ( liw = 3 * n + nti )
      integer lw
      parameter ( lw = 7 * n + 3 * n * nti + 4 * n * n )

      double precision a
      double precision amp
      double precision b
      double precision d(n,nti)
      double precision er(5)
      external fdif
      external g
      integer ierror
      integer itlim
      integer iw(liw)
      integer j
      integer k
      integer kpart
      integer nrti
      double precision phirec(nu,nti)
      double precision q(n,n,nti)
      double precision ti(nti)
      double precision u(nu,nti)
      double precision w(lw)
      double precision wgr(lwg)
      double precision x(n,nti)
      external x0t

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MUS_NONLINEAR_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Test the MUS library on a nonlinear problem.'
c
c  Set input parameters.
c
      ER(3) = 1.1D-15
      ER(1) = 1.0d-06
      ER(2) = 1.0D-02
      A = 0.0D+00
      B = 1.0D+00
      NRTI=10
      AMP=100.0D+00
      ITLIM=20
      IERROR=1
c
c  Call MUSN for the solution.
c
      CALL MUSN(FDIF,X0T,G,N,A,B,ER,TI,NTI,NRTI,AMP,ITLIM,X,Q,U,NU,D,
     1          PHIREC,KPART,W,LW,IW,LIW,WGR,LWG,IERROR)

      WRITE(*,*) ' MUSN: IERROR =',IERROR

      WRITE(*,200) A,B,ER(1),ER(2),ER(4),ER(5),KPART
  200 FORMAT(' A = ',F8.4,3X,'B = ',F8.4,/,' REQUIRED TOLERANCE = ',1P,
     1 D12.5,3X,'START TOLERANCE  = ',D12.5,/,
     2 ' CONDITION NUMBER = ',D12.5,3X,
     3 'AMPLIFICATION FACTOR = ',D12.5,/,' K-PARTITIONING =',I2,/)

      IF (IERROR.NE.0) GOTO 3000

      WRITE(*,215)
  215 FORMAT(' I ',4X,'T',9X,'Y1',12X,'Y2',12X,'Y3',12X,'Y4',12X,'Y5',
     1 /)
      DO K = 1 , NRTI
        WRITE(*,220) K, TI(K), (X(J,K),J=1,N)
      end do
  220 FORMAT(' ',I2,1X,F6.4,1P,5(2X,D12.5))
 3000 CONTINUE
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MUS_NONLINEAR_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine fdif ( t, y, f )

c*********************************************************************72
c
cc FDIF evaluates the right hand side of the ODE.
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
c  Parameters:
c
c    Input, double precision T, the value of the independent variable.
c
c    Input, double precision Y(N), the dependent variable at T.
c
c    Output, double precision F(N), the right hand side at T, Y.
c
      implicit none

      double precision c1
      double precision c2
      double precision c3
      double precision c4
      double precision f(5)
      double precision t
      double precision y(5)

      c1 = 0.5D+00
      c2 = 0.9D+00
      c3 = 1000.0D+00
      c4 = 10.0D+00

      F(1) = c1 * Y(1) * ( Y(3) - Y(1) ) / Y(2)
      F(2) = - c1 * ( Y(3) - Y(1) )
      F(3) = ( c2 - c3 * ( Y(3) - Y(5) ) 
     &  - c1 * Y(3) * ( Y(3) - Y(1) ) ) / Y(4)
      F(4) = c1 * ( Y(3) - Y(1) )
      F(5) = c3 / c4 * ( Y(3) - Y(5) )

      return
      end
      subroutine x0t ( t, x )

c*********************************************************************72
c
cc X0T supplies the initial approximate solution to the nonlinear BVP.
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
c    Input, double precision T, the value of the independent variable.
c
c    Output, double precision X(N), the approximate solution at T.
c
      implicit none

      double precision t
      double precision X(5)

      X(1) = 1.0D+00
      X(2) = 1.0D+00
      X(4) = -10.0D+00
      X(3) = - 4.5D0*T*T + 8.91D+00 * T + 1.0D+00
      X(5) = - 4.5D0*T*T + 9.0D+00 * T + 0.91D+00

      return
      end
      subroutine G ( N, XA, XB, FG, DGA, DGB )

c*********************************************************************72
c
cc G evaluates the nonlinear boundary condition.
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
c    Input, integer N, the number of equations.
c
c    Input, double precision XA(N), XB(N), the value of the dependent
c    variables at A and B.
c
c    Output, double precision FG(N), the value of the boundary conditions.
c
c    Output, double precision DGA(N,N), DGB(N,N), the Jacobians of FG
c    with respect to XA(N) and XB(N) respectively.
c
      implicit none

      integer n

      double precision dga(n,n)
      double precision dgb(n,n)
      double precision fg(n)
      integer i
      integer j
      double precision xa(n)
      double precision xb(n)

      FG(1) = XA(1) - 1.0D0
      FG(2) = XA(2) - 1.0D0
      FG(3) = XA(3) - 1.0D0
      FG(4) = XA(4) + 10.0D0
      FG(5) = XB(3) - XB(5)

      DO I = 1, N
        DO J = 1, N
          DGA(I,J) = 0.0D0
          DGB(I,J) = 0.0D0
        end do
      end do

      DGA(1,1) = 1.0D0
      DGA(2,2) = 1.0D0
      DGA(3,3) = 1.0D0
      DGA(4,4) = 1.0D0
      DGB(5,3) = 1.0D0
      DGB(5,5) = -1.0D0

      RETURN
      END
