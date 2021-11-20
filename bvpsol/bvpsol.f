      SUBROUTINE BVPSOL(FCN,BC,IVPSOL,N,M,T,X,EPS,IOPT,INFO,IRW,RW,
     &                  IIW,IW)

c*********************************************************************72
c
cc BVPSOL is a solver for highly nonlinear two point boundary value problems.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)

      EXTERNAL FCN,BC,IVPSOL
      INTEGER N,M
      DOUBLE PRECISION T(M)
      DOUBLE PRECISION X(N,M)
      DOUBLE PRECISION EPS
      INTEGER INFO,IOPT(5)
      INTEGER IRW
      DOUBLE PRECISION RW(IRW)
      INTEGER IIW
      INTEGER IW(IIW)
C
C     ------------------------------------------------------------
C
C*  Title
C
C     (B)oundary (V)alue (P)roblem (Sol)lver for highly nonlinear
C     two point boundary value problems using a local linear
C     solver (condensing algorithm) or a global sparse linear solver
C     for the solution of the arising 
C     linear subproblems.
C
C*  Written by        P. Deuflhard, G.Bader, L. Weimann
C*  Purpose           Solution of nonlinear two-point boundary value
C                     problems.
C*  Method            Local and Global Nonlinear two-point Boundary Value
C                     Problems solver (Multiple shooting approach)
C*  Category          I1b2a - Differential and integral equations
C                             Two point boundary value problems
C*  Keywords          Nonlinear boundary value problems, Multiple
C                     shooting, Newton methods
C*  Version           1.2
C*  Revision          February 2002
C*  Latest Change     January 2004
C*  Library           CodeLib
C*  Code              Fortran 77, Double Precision
C*  Environment       Standard Fortran 77 environment on PC's,
C                     workstations and hosts.
C*  Copyright     (c) Konrad-Zuse-Zentrum fuer
C                     Informationstechnik Berlin (ZIB)
C                     Takustrasse 7, D-14195 Berlin-Dahlem
C                     phone : + 49/30/84185-0
C                     fax   : + 49/30/84185-125
C*  Contact           Lutz Weimann
C                     ZIB, Division Scientific Computing, 
C                          Department Numerical Analysis and Modelling
C                     phone : + 49/30/84185-185
C                     fax   : + 49/30/84185-107
C                     e-mail: weimann@zib.de
C
C*    References:
C
C     /1/ R.Bulirsch:
C         Die Mehrzielmethode zur numerischen Loesung von
C         nichtlinearen Randwertproblemen und Aufgaben der
C         optimalen Steuerung.
C         Carl-Cranz-Gesellschaft: Tech.Rep. (Oct.1971)
C
C     /2/ J.Stoer, R.Bulirsch:
C         Einfuehrung in die Numerische Mathematik II.
C         Berlin, Heidelberg, New York: Springer (1st Ed. 1973)
C
C     /3/ P.Deuflhard:
C         A Modified Newton Method for the Solution of
C         Ill-Conditioned Systems of Nonlinear Equations with
C         Application to Multiple Shooting.
C         Numer. Math. 22, 289-315 (1974)
C
C     /4/ P.Deuflhard:
C         Recent Advances in Multiple Shooting Techniques.
C         (Survey Article including further References)
C         In: I.Gladwell, D.K.Sayers (Ed.): Computational
C         Techniques for Ordinary Differential Equations.
C         Section 10, P.217-272.
C         London, New York: Academic Press (1980)
C
C     /5/ P.Deuflhard, G.Bader:
C         Multiple Shooting Techniques Revisited.
C         Univ. Heidelberg, SFB 123, Tech. Rep. 163 (1982)
C
C     /6/ P. Deuflhard:
C         Newton Methods for Nonlinear Problems. -
C         Affine Invariance and Adaptive Algorithms.
C         Series Computational Mathematics 35, Springer (2004)
C
C  ---------------------------------------------------------------
C
C* Licence
C    You may use or modify this code for your own non commercial
C    purposes for an unlimited time.
C    In any case you should not deliver this code without a special
C    permission of ZIB.
C    In case you intend to use the code commercially, we oblige you
C    to sign an according licence agreement with ZIB.
C
C* Warranty
C    This code has been tested up to a certain level. Defects and
C    weaknesses, which may be included in the code, do not establish
C    any warranties by ZIB. ZIB does not take over any liabilities
C    which may follow from aquisition or application of this code.
C
C* Software status
C    This code is under partial care of ZIB and belongs to ZIB software
C    class 2.
C
C     ------------------------------------------------------------
C
C     External subroutines (to be supplied by the user)
C     =================================================
C
C       FCN(N,T,Y,DY)         Right-hand side of system of
C                             first-order differential equations
C         N                   Input: Number of first order ODE's
C         T                   Input: Actual position in the
C                             interval ³ A ,  B ü
C         Y(N)                Input: Values at T
C         DY(N)               Output: Derivatives at T
C
C       BC(YA,YB,R)           Two-point boundary conditions at ( A
C                             = T(1),  B = T(M))
C         YA(N)               Input: Values at A = T(1)
C         YB(N)               Input: Values at B = T(M)
C         R(N)                Output: Values of
C                             boundary conditions function
C
C       IVPSOL(N,FCN,T,Y,TEND,TOL,HMAX,H,KFLAG)
C                             Initial value problem (IVP)
C                             integrator
C         N                   Number of first-order ODE's
C         FCN                 Right-hand side of the ODE's system
C                             ( see above )
C         T                   Input: Starting point of integration
C                             T.LT.TEND
C                             Output: Achieved final point of
C                             integration
C         Y(N)                Input and Output: Values at T
C         TEND                Input: Prescribed final point of
C                             integration
C         TOL                 Input: Prescribed relative precision
C                             (>0)
C         HMAX                Input: Maximum permitted stepsize
C         H                   Input: Initial stepsize guess
C                             Output: Stepsize proposal for next
C                             integration step ( H.EQ.0 ,  if
C                             IVPSOL fails to proceed )
C         KFLAG               Input: Print parameter
C                             Output: Error flag ( KFLAG.LT.0
C                             indicates an error ) .
C                             For further details, see IVPSOL .
C
C     Input parameters (* marks inout parameters)
C     ===========================================
C
C       N                     Number of first-order ordinary
C                             differential equations.
C       M                     Number of Shooting nodes.
C                             =2    Single Shooting
C                             >2    Multiple Shooting
C       T(M)                  Single/Multiple Shooting Nodes
C                             ( T(1)= A ,  T(M)= B )
C     * X(N,M)                Start data for Newton iteration.
C       EPS                   Required relative precision of
C                             solution.
C       IOPT(5)               Options array. The options have the
C                               meanings as described below:
C         IOPT(1) - MAXIT     Maximum permitted number of
C                             iteration steps.
C         IOPT(2) - NONLIN    Boundary value problem
C                             classification by user:
C                             0     Linear boundary value problem.
C                             1     Nonlinear boundary value
C                                   problem. Good initial data
C                                   available.
C                             2     Highly nonlinear boundary
C                                   value problem. Only bad
C                                   initial data available. Small
C                                   initial damping factor in
C                                   Gauss Newton method.
C                             3     Highly nonlinear boundary
C                                   value problem. Only bad
C                                   initial data available. Small
C                                   initial damping factor in
C                                   Gauss Newton method.
C                                   Additionally initial rank
C                                   reduction to separable linear
C                                   boundary conditions.
C         IOPT(3) - IBVPSL    Solution method switch:
C                             0     use local linear solver with
C                                   condensing algorithm
C                             1     use global sparse linear solver
C                                   (formerly realized as "BVPSOG")
C         IOPT(4) - IPRINT    Print parameter:
C                             -1    No print
C                              0    Print initial data, iterative
C                                   values of level functions,
C                                   solution data (or final data,
C                                   respectively)
C                             +1    Additionally print iterates
C                                   T(J),X(I,J),  I = 1,...,N ,  J
C                                   = 1,...,M
C         IOPT(5) - LUPRI     Print output unit:
C                             The unit where the output, controlled
C                             by IOPT(4), is written to.
C                             If set to 0, output unit will be 6.
C       NRW                   Dimension of real workspace RW
C                             If IOPT(3) = 0: 
C                               NRW.GE.N*N*(M+5)+10*M*N+10*N+M
C                             If IOPT(3) = 1:
C                               NRW.GE.N*N*(M+1)+12*N*M+4*N+M-1+LICN
C                             with:
C                               NZ = N*N*(M+1)+N*(M-1)
C                               LICN = 2*NZ
C                               LIRN = DMIN1(DMAX1(1.5*NZ,NZ+4*M*N),
C                                            LICN)
C       RW(NRW)               Real workspace
C
C       NIW                   Dimension of integer workspace IW
C                             If IOPT(3) = 0: 
C                               NIW.GE.2*N*N+4*N
C                             If IOPT(3) = 1:
C                               NIW.GE.2*N*N+3*N+8*M*N+8*M*N
C                                      +2*NZ+LICN+LIRN
C       IW(NIW)               Integer workspace
C
C
C
C     Output parameters:
C     ==================
C
C       X(N,M)                Solution data ( or final data,
C                             respectively )
C       INFO                  Information output parameter
C                              >0   Number of iterations performed
C                                   to obtain the solution
C                              <0   BVPSOL termination
C                              -1   If IOPT(3) = 0:
C                                     Iteration stops at stationary
C                                     point
C                                   If IOPT(3) = 1:
C                                     Gaussian elimination failed due 
C                                     to singular Jacobian
C                              -2   Iteration stops after ITMAX
C                                   iteration steps ( as indicated
C                                   by option ITMAX=IOPT(1) )
C                              -3   Integrator failed to complete
C                                   the trajectory computation
C                              -4   Gauss Newton method failed to
C                                   converge
C                              -5   Given initial values
C                                   inconsistent with separable
C                                   linear boundary conditions
C                              -6   If IOPT(3) = 0:
C                                     Iterative refinement failed to
C                                     converge
C                                   If IOPT(3) = 1:
C                                     Termination since Multiple
C                                     Shooting condition or
C                                     condition of Jacobian is too bad
C                              -7   Reliable relative accuracy
C                                   greater than 1.0D-2
C                              -8   Condensing algorithm for
C                                   linear block system fails, use
C                                   global linear solver in
C                                   boundary value problem routine,
C                                   i.e. set IOPT(3)=1
C                              -9   If IOPT(3) = 1:
C                                     Sparse linear solver failed,
C                                     possibly workspace is
C                                     too small
C                             -10   Real or integer work-space
C                                   exhausted
C                             -11   If IOPT(3) = 0:
C                                     Rank reduction failed -
C                                     resulting rank is zero
C
C     ------------------------------------------------------------
C
C*    End Prologue
C:    SMALL = squareroot of "smallest positive machine number
C     divided by relative machine precision"
      DOUBLE PRECISION SMALL
      PARAMETER (SMALL=4.94D-32)
      INTEGER M1,NM,NM1,NN,NRW,NIW,ITMAX,NONLIN,IBVPSL
      DOUBLE PRECISION RELDIF,TOL,XTHR,V
C:    Begin
C     ------------------------------------------------------------
C     1 Internal parameters
C     Standard values fixed below
C     Monitor output unit
      LUMON = IOPT(5)
      IF (LUMON.LE.0.OR.LUMON.GE.100) LUMON=6
C     Scaling threshold
      XTHR = SMALL
C     Prescribed relative precision for numerical integration
      TOL = EPS*1.0D-2
C     Prescribed relative deviation for numerical differentiation
      RELDIF = DSQRT(TOL)
      ITMAX  = IOPT(1)
      NONLIN = IOPT(2)
      IBVPSL = IOPT(3)
      INFO   = IOPT(4)
      IF(INFO.GE.0)THEN
C       Print BVPSOL heading lines
1       FORMAT('B V P S O L',2X,5('*'),2X,'V e r s i o n',2
     *  X,'1 . 2',1X,3('*'),//,1X,'Newton',1('-'),'Method ','for ',
     *  'the ','solution ','of ','boundary ','value ','problems',/
     *  /)
        write ( lumon, '(a)' ) ' '
        WRITE(LUMON,1)
      ENDIF
      IF (IBVPSL.EQ.0) THEN
C     Starting value for pseudo - rank of sensitivity matrix E
      IRANK = N
C     Initial preparations
      M1 = M-1
      NN = N*N
      NM = N*M
      NM1 = N*M1
C:    WorkSpace: IW
        L4=1
        L5=L4+N
        L6=L5+N
        L7=L6+N
        L8=L7+N
        L9=L8+N*N
        L10=L9+N*N
        NIW=L10-1
C.    End WorkSpace at NIW
C:    WorkSpace: RW
        L11=1
        L12=L11+N*N*M1
        L13=L12+N*N
        L14=L13+N*N
        L15=L14+N*N
        L16=L15+N*N
        L17=L16+N*N
        L18=L17+NM
        L19=L18+NM
        L20=L19+NM
        L21=L20+NM
        L22=L21+NM
        L23=L22+NM
        L24=L23+NM1
        L25=L24+NM1
        L26=L25+NM1
        L27=L26+NM1
        L28=L27+N
        L29=L28+N
        L30=L29+N
        L31=L30+N
        L32=L31+N
        L33=L32+N
        L34=L33+N
        L35=L34+N
        L36=L35+N
        L37=L36+N
        L38=L37+N
        L39=L38+N
        L40=L39+N
        L41=L40+M
        L42=L41+N
        L43=L42+N*N
        NRW=L43-1
C.    End WorkSpace at NRW
C     ------------------------------------------------------------
C     2 Check for sufficient real/integer workspace

      IF (INFO.GE.0) THEN
2       FORMAT('Minimal ','required ','work-space ',':',/,
     *  'Real    ','array ','RW(',I4,')',/,'Integer ','array ',
     *  'IW(',I4,')')
        write ( lumon, '(a)' ) ' '
        WRITE(LUMON,2)NRW,NIW
      ENDIF

      IF(NRW.LE.IRW.AND.NIW.LE.IIW)THEN
        CALL BVPL(FCN,BC,IVPSOL,N,M,M1,NM,NM1,T,X,EPS,TOL,RELDIF,
     *  NONLIN,IRANK,ITMAX,INFO,XTHR,IW(L4),IW(L5),IW(L6),IW(L7),
     *  IW(L8),IW(L9),RW(L11),RW(L12),RW(L13),RW(L14),RW(L15),RW(
     *  L16),RW(L17),RW(L18),RW(L19),RW(L20),RW(L21),RW(L22),RW(
     *  L23),RW(L24),RW(L25),RW(L26),RW(L27),RW(L28),RW(L29),RW(
     *  L30),RW(L31),RW(L32),RW(L33),RW(L34),RW(L35),RW(L36),RW(
     *  L37),RW(L38),RW(L39),RW(L40),RW(L41),RW(L42),LUMON)
      ELSE
C       Fail exit work-space exhausted
        IF(INFO.GE.0.AND.NRW.GT.IRW)THEN
3         FORMAT('Error: ','real    ','work ','- ','space ',
     *    'exhausted',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,3)
        ENDIF
        IF(INFO.GE.0.AND.NIW.GT.IIW)THEN
4         FORMAT('Error: ','integer ','work ','- ','space ',
     *    'exhausted',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,4)
        ENDIF
        INFO = -10
      ENDIF
      ELSE IF (IBVPSL.EQ.1) THEN
C     Initial preparations
      M1 = M-1
      NN = N*N
      NM = N*M
      NM1 = N*M1
      NMX8 = 8*NM
      NZ = N*N*(M+1)+N*(M-1)
      LICNQ = 2*NZ
      LIRNQ = MAX0(3*NZ/2,NZ+4*M*N)
      LIRNQ = MIN0(LIRNQ,LICNQ)
      V = DBLE(LIRNQ)/DBLE(LICNQ)
      NRW = N*N*(M+1)+12*M*N+4*N+M-1
      NI2W = 2*NZ+8*M*N
      NIW = NMX8+2*N*N+3*N
      L1 = IRW-NRW
      L2 = IDINT(DBLE(IIW-NIW-NI2W)/(V+1.0D0))
      LICN = MIN0(L1,L2)
      LIRN = IDINT(V*DBLE(LICN))
      LISNQ = LICNQ+LIRNQ
      NKEEP = NMX8
      MINI2W = NI2W + LISNQ
C:    WorkSpace: I2W
        L4=1
        L5=L4+LIRN
        L6=L5+LICN
        L7=L6+NZ
        L8=L7+NZ
        L9=L8+NKEEP
        NI2W=L9-1
C.    End WorkSpace at NI2W
C:    WorkSpace: IW
        L10=L9
        L11=L10+N
        L12=L11+N
        L13=L12+N
        L14=L13+N*N
        L15=L14+N*N
        L16=L15+NMX8
        NIW=L16-1
C.    End WorkSpace at NIW
C:    WorkSpace: RW
        L17=1
        L18=L17+N*N*M1
        L19=L18+N*N
        L20=L19+N*N
        L21=L20+LICN
        L22=L21+NM
        L23=L22+NM
        L24=L23+NM
        L25=L24+NM
        L26=L25+NM
        L27=L26+NM
        L28=L27+NM1
        L29=L28+NM1
        L30=L29+NM1
        L31=L30+NM1
        L32=L31+N
        L33=L32+N
        L34=L33+N
        L35=L34+N
        L36=L35+NM
        L37=L36+NM
        L38=L37+N
        L39=L38+N
        L40=L39+N
        L41=L40+N
        L42=L41+M1
        NRW=L42-1
C.    End WorkSpace at NRW
C     ------------------------------------------------------------
C     2 Check for sufficient real/integer workspace
      IF (INFO.GE.0) THEN
5       FORMAT('Minimal ','required ','work-space ',':',/,
     *  'Real          ','array ','RW( ',I5,')',/,
     *  'Integer       ','array ','IW( ',I5,')')
        write ( lumon, '(a)' ) ' '
        WRITE(LUMON,5)NRW-LICN+LICNQ,NIW-LIRN-LICN+LISNQ
      ENDIF
      IF(NRW.LE.IRW.AND.NIW.LE.IIW
     *   .AND.LICNQ.LE.LICN+1.AND.LIRNQ.LE.LIRN+1)THEN
        CALL BVPG(FCN,BC,IVPSOL,N,M,M1,NM,NM1,NMX8,NZ,LICN,LIRN,
     *  LISNQ,NKEEP,T,X,EPS,TOL,RELDIF,NONLIN,ITMAX,INFO,XTHR,IW(
     *  L10),IW(L11),IW(L12),IW(L13),IW(L14),IW(L15),RW(L17),RW(
     *  L18),RW(L19),RW(L20),RW(L21),RW(L22),RW(L23),RW(L24),RW(
     *  L25),RW(L26),RW(L27),RW(L28),RW(L29),RW(L30),RW(L31),RW(
     *  L32),RW(L33),RW(L34),RW(L35),RW(L36),RW(L37),RW(L38),RW(
     *  L39),RW(L40),RW(L41),IW(L6),IW(L7),IW(L5),IW(L4),IW(
     *  L8),LUMON)
      ELSE
C       Fail exit work-space exhausted
        IF(INFO.GE.0.AND.NRW.GT.IRW)THEN
6         FORMAT('Error: ',A,'work ','- ',
     *    'space ','exhausted',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,6) 'real          '
        ENDIF
        IF(INFO.GE.0.AND.NIW.GT.IIW)THEN
          WRITE(LUMON,6) 'integer       '
        ENDIF
        INFO = -10
      ENDIF
      ELSE
        WRITE(LUMON,*) ' Invalid option IOPT(3) set - must be 0 or 1'
      ENDIF

      RETURN
      END
      SUBROUTINE BVPL(FCN,BC,IVPSOL,N,M,M1,NM,NM1,T,X,EPS,TOL,
     *RELDIF,NONLIN,IRANK,ITMAX,INFO,XTHR,IROW,ICOL,ICOLB,PIVOT,IA,
     *IB,G,A,B,BG,E,QE,DX,DDX,DXQ,DXQA,XA,XW,XU,HH,DHH,HHA,D,DE,R,
     *DR,RA,U,DU,QU,X1,XM,T1,T2,DX1,RF,US,EH,LUMON)

c*********************************************************************72
c
cc BVPL carries out the local linear solver approach.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)

      EXTERNAL FCN,BC,IVPSOL
      INTEGER N,M,M1,NM,NM1,LUMON
      DOUBLE PRECISION T(M),X(NM)
      DOUBLE PRECISION EPS
      DOUBLE PRECISION TOL,RELDIF
      INTEGER NONLIN,ITMAX,IRANK
      INTEGER INFO
      DOUBLE PRECISION XTHR
      INTEGER IROW(N),ICOL(N),ICOLB(N),PIVOT(N)
      INTEGER IA(N,N),IB(N,N)
      DOUBLE PRECISION G(N,N,M1)
      DOUBLE PRECISION A(N,N),B(N,N),BG(N,N),E(N,N),QE(N,N)
      DOUBLE PRECISION DX(NM),DDX(NM),DXQ(NM),DXQA(NM),XA(NM),XW(
     *NM),XU(NM1),HH(NM1),DHH(NM1),HHA(NM1),D(N),DE(N),R(N),DR(N),
     *RA(N),U(N),DU(N),QU(N),X1(N),XM(N),T1(N),T2(N),DX1(N),RF(M),
     *US(N)
      DOUBLE PRECISION EH(N,N)
C
C     Additional dimensional integer variables
C     ========================================
C
C       M1                M-1
C       NM                N*M
C       NM1               N*(M-1)
C
C     Internal real arrays (workspace) :
C     ==================================
C
C       G(N,N,M1)        (N,N) -Wronskian Matrices G(1),...,G(M-1)
C                         .
C       A(N,N)            Wronskian Matrix on left boundary
C                         dBC/dX(X(1,...,N),T(1)).
C       B(N,N)            Wronskian Matrix on right boundary
C                         dBC/dX(X((N-1)*M+1,...,N*M),T(M)).
C       BG(N,N)           Workspace for subroutine RHS1. Holds
C                         subsequently the matrices B,B*G(M-1),...,
C                         B*G(M-1)*...*G(2)during computation of
C                         the right hand side of the condensed
C                         linear system.
C       E(N,N)            Sensitivity Matrix of the boundary value
C                         problem: E = A+B*G(M-1)*...*G(1).
C       EH(N,N)           Holds a copy of the row- and column
C                         scaled, but not decomposed sensitivity
C                         matrix E needed for iterative refinement
C                         computations.
C       QE(N,N)           Workspace for DECCON and SOLCON to hold
C                         the updating part of the pseudoinverse
C                         of E in case of it's rank defeciency.
C       D(N)              Diagonal elements of upper triangular
C                         matrix of QR-decomposition computed by
C                         DECCON .
C       DDX(NM)           Workspace for subroutine BLSOLI - gets
C                         the iterative refinement vector of DXQ .
C       DE(N)             Holds row scaling factors for the
C                         sensitivity matrix.
C       DHH(NM1)          Holds the recursive refinement of HH
C                         computed in BLSOLI .
C       DR(N)             Workspace for subroutine BLSOLI to hold
C                         the boundary residual
C                         BC(DXQ(1,...,N),DXQ((M-1)*N+1,...,M*N))+
C                         (A*DXQ(1,...,N))+B*DXQ((M-1)*N+1,...,M*N)
C                         .
C       DU(N)             Workspace for subroutine BLSOLI to hold
C                         the right hand side of the linear system
C                         E*T2 = U-E*DX1 solved to compute the
C                         iterative refinement for DX1 .
C       DX(NM)            Actual newton correction.
C       DXQ(NM)           Simplified Newton correction J(k-1)*X(k)
C                         with the Jacobian J(k) and the iterate
C                         vector X(k) at the k-th iterate.
C       DXQA(NM)          Previous simplified Newton correction
C                         J(k-2)*X(k-1).
C       DX1(N)            Workspace to receive the solution output
C                         of SOLCON within computation of DXQ and
C                         it's iterative refinement.
C       HH(NM1)           Elements (J-1)*N+1 to J*N are holding
C                         the values
C                         Y(T(J+1),X((J-1)*N+1,...,J*N))-X(J*N+1,
C                         ...,(J+1)*N)
C                         ( with the trajectory Y in
C                         ³ T(J),T(J+1)ü , J = 1,...,M-1 ).
C       HHA(NM1)          Holds the previous values of HH .
C       QU(N)             Savespace to hold U(N)for restoring it
C                         in the case of rank reduction.
C       R(N)              Value of the boundary condition function
C                         BC for the current iterate.
C       RA(N)             Previous values of R .
C       RF(M)             Workspace for subroutine BLSOLI - R(J)
C                          gets the maximum norm of the
C                         components (J-1)*N,...,J*N of the
C                         iterative refinement vector to DXQ for
C                         determination of the sweep index for
C                         subsequent iterations.
C       T1(N)             Workspace used for miscellaneous
C                         purposes temporarely.
C       T2(N)             Workspace used for miscellaneous
C                         purposes temporarely.
C       U(N)              Holds the right hand side of the
C                         condensed linear system computed by
C                         subroutine BLRHS1 .
C       US(N)             Workspace for subroutine BLSOLI to save
C                         U(N).
C       XA(NM)            Previous Newton iterate.
C       XU(NM1)           Elements (J-1)*N+1 to J*N are holding
C                         the values Y(T(J+1),X((J-1)*N+1,...,J*N))
C                         of the trajectory in the interval ³ T(J),
C                         T(J+1)ü , (for J = 1,...,M-1 ).
C       XW(NM)            Scaling factors for iteration vector.
C       X1(N)             Components of the iteration vector
C                         corresponding to the left boundary
C                         A = T(1).
C       XM(N)             Components of the iteration vector
C                         corresponding to the right boundary
C                         B = T(M).
C
C     Internal integer arrays (workspace)
C     ===================================
C
C       IROW(N)           Row permutations of sensitivity matrix.
C       ICOL(N)           Column permutations of matrix A
C                         (left boundary).
C       ICOLB(N)          Column permutations of matrix B
C                         (right boundary).
C       PIVOT(N)          Workspace for DECCON and SOLCON to hold
C                         permutations performed during
C                         QR-decomposition.
C       IA(N,N)           Reflects the sparse structure of matrix
C                         A by values 0, 1.
C       IB(N,N)           Reflects the sparse structure of matrix
C                         B by values 0, 1.
C
C     Internal real variables
C     =======================
C
C       COND              Gets the condition of the sensitivity
C                         matrix determined by DECCON . Not
C                         further used.
C       CONDE             Maximum permitted subcondition of
C                         sensitivity matrix E .
C       COND1             Condition of boundary conditions part of
C                         the decomposed sensitivity matrix E .
C       COND2             Condition of the remaining part of the
C                         sensitivity matrix E .
C       CONV              Scaled maximum norm of DXQ computed by
C                         subroutine BLLVLS . Used for convergence
C                         test.
C       CONVA             Holds the previous value of CONV .
C       DEL               Becomes the Euklid's normsquare of the
C                         defect belonging to the condensed linear
C                         system in case of rank defeciency. Used
C                         for computation of damping factor
C                         predictor.
C       EPSMIN            Smallest reasonable permitted accuracy
C                         EPS that can be prescribed by the user.
C       FC                Actual Gauss Newton iteration damping
C                         factor.
C       FCA               Previous Gauss Newton iteration damping
C                         factor.
C       FCDNM             Used to compute the denominator of the
C                         damping factor FC during computation of
C                         it's predictor, corrector and
C                         aposteriori estimate (in the case of
C                         performing a Rank1 update) .
C       FCH               Temporarely used for storing the new FC
C                         when computing aposteriori estimate.
C       FCMIN             Minimum permitted relaxation factor. If
C                         FC becomes smaller than this value, one
C                         of the following may occur:
C                         a.    Recomputation of the sensitivity
C                               matrix by means of difference
C                               approximation (instead of Rank1
C                               update), if Rank1 - update
C                               previously was used
C                         b.    Rank reduction of sensitivity
C                               matrix E ,  if difference
C                               approximation was used previously
C                               and Rank(E).NE.0
C                         c.    Fail exit otherwise
C       FCMINH            DSQRT(FCMIN). Used for rank
C                         determination computations.
C       FCMIN2            FCMIN**2 . Used for FC-predictor
C                         computation.
C       FCNUM             Gets the numerator of the aposteriori
C                         estimate of FC .
C       FCNUMK            Gets the numerator of the corrector
C                         computation of FC .
C       FCNUMP            Gets the numerator of the predictor
C                         computation of FC .
C       H                 Actual integrator stepsize.
C       HMAX              Maximum permitted integrator stepsize.
C                         Set to the length of the integration
C                         interval, e.g. the distance of the
C                         effected Shooting points.
C       HSAVE             Stepsize saved across the call of the
C                         integrator.
C       HSTART            Start stepsize for integration used by
C                         subroutines BLFCNI and BLDERG .
C       MUE               Temporary value used during computation
C                         of damping factors predictor.
C       REDH              Multi purpose reduction factor. (???)
C       RELDIF            Relative deviation for numerical
C                         differentation.
C       SENS1             Sensitivity of boundary conditions part
C                         of the decomposed sensitivity matrix E .
C       SENS2             Sensitivity of the remaining part of the
C                         matrix E .
C       SIGMA             Decision parameter for Jacobian Rank1
C                         updates (SIGMA.GT.1) . Rank1 updates are
C                         inhibited, if SIGMA.GT.1/FCMIN is set.
C       SKAP              Used to compute and print out the
C                         incompatibility factor of the nonlinear
C                         boundary value (e.g. least squares)
C                         problem.
C       SUMF              Standard level of the current iterate,
C                         e.g. Norm2(F(X))**2
C                         with the nonlinear model function F on
C                         which Newton iteration is performed,
C                         arising from the Multiple Shooting
C                         approach.
C       SUMX              Natural level of the current iterate,
C                         e.g. Norm2(DX)
C                         with the Newton correcture DX
C                         (see above).
C       SUMXA             Natural level of the previous iterate.
C       TFAIL             Used to get and print out in case of an
C                         integrator failure the last reached T
C                         value as a proposal for insertion of a
C                         new Shooting point.
C       TOL               Prescribed relative precision for
C                         numerical integration.
C       TOLH              Temporary used for computation of TOL
C                         (may be obmitted|).
C       TOLMIN            Lower bound value for TOL .
C       XTHR              Threshold for scaling.
C       TJ                Used by BLFCNI to hold T(J).
C       TJ1               Used by BLFCNI to hold T(J+1).
C       CORR              Used by BLSOLI to compute RF(J),  J = 1,
C                         ...,M .
C       EPX1              Used by BLSOLI to get the maximum norm
C                         of DX1 corrected by iterative
C                         refinement.
C       EPDX1             Used by BLSOLI to get the maximum norm
C                         of the iterative refinement correcture
C                         for DX1 .
C       EPX1H             Relative accuracy of DX1 after iterative
C                         refinement (= EPDX1/EPX1 ). If EPX1H.GT.
C                         1/2 ,  refinement is considered to be
C                         failed.
C       EPH               Used by BLSOLI as tolerance for the
C                         iterative refinement vectors maximum
C                         norm. If it exceeds at index J the
C                         tolerance, refinement will be performed
C                         next time for the components associated
C                         to Shooting points T(J),...,T(M).
C       SIGDEL            Used by BLSOLI to compute the required
C                         integrator accuracy from the multiple
C                         shooting condition.
C       SIGDLH            Used by BLSOLI to compute the multiple
C                         shooting condition
C                         Max( RF(J+1)/RF(J) ) ,  ( J = 1,...,M-1
C                         ). See above for RF .
C
C     Internal integer variables
C     ==========================
C
C       IC                Permutated index. Used by BLSOLI .
C       ICA               Temporarely used during search for
C                         separable linear boundary conditions at
C                         T(1).
C       ICB               Temporarely used during search for
C                         separable linear boundary conditions at
C                         T(M).
C       IH                Temporarely used during search for
C                         separable linear boundary conditions at
C                         T(1) and T(M).
C       IR                Temporary storage for a row permutation
C                         index.
C       IRANK             Rank of sensitivity matrix E of current
C                         iteration step.
C       IRANKA            Rank of sensitivity matrix E of previous
C                         iteration step.
C       IRANKB            Rank of the decomposed sensitivity
C                         matrix part belonging to the boundary
C                         conditions.
C       IREPET            Parameter of subroutines DECCON and
C                         SOLCON indicating the mode of operation
C                         of these routines:
C                         >=0   Full QR-decomposition required
C                          <0   Rank reduction required
C       IRKMAX            Holds the maximum applied rank of all
C                         previous iterates. Must be necessary
C                         .EQ. NE for convergence.
C       IS                Additional DO loop index.
C       ISUM              Used for determination of sparse
C                         structure of matrices A and B as
C                         nonzeros location counter.
C       ITER              Iteration count.
C       JA                Previous sweep index. Used in subroutine
C                         BLSOLI .
C       JIN               Current sweep index. Used in subroutine
C                         BLSOLI .
C       JJ                Used as "reverse DO loop" index:
C                         JJ = IUPB-J in a loop like DO J = 1,IUPB
C                         ...
C       JN                New sweep index determined in subroutine
C                         BLSOLI .
C       JRED              Damping factor reduction count during an
C                         iterate.
C       KC                Temporary storage for a column
C                         permutation index.
C       KFLAG             Gets the subintervall number of the
C                         failure from subroutine BLDERG
C                         if the integrator failed.
C       KOUNT             Trajectory evaluations count.
C       KPRINT            Print parameter - copy of input
C                         parameter INFO .
C       LEVEL             Flow control parameter needed by
C                         subroutine BLSOLI :
C                         0     indicates computation of Gauss
C                               Newton correcture,
C                         1     indicates computation of
C                               simplified Gauss Newton correcture
C                               (after computation of the
C                               preliminary new iterate)
C       NB                Number of separable boundary conditions
C                         at T(M)
C       NE                Number of not separable boundary
C                         conditions at T(1) (and number of rows
C                         and columns of sensitivity matrix)
C       NEW               Count of subsequent performed Rank1
C                         (Broyden) updates.
C       NY                Iterative refinement sweep count for an
C                         iterate ( used in subroutine BLSOLI ) .
C       NYMAX             Highest allowed iterative refinement
C                         sweep index.
C:    End Parameter
C:    EPMACH = relative machine precision
      DOUBLE PRECISION EPMACH,SMALL
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=0.5D0)
      DOUBLE PRECISION REDH
      PARAMETER (REDH=1.0D-2)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.0D0)
      DOUBLE PRECISION TWO
      PARAMETER (TWO=2.0D0)
      DOUBLE PRECISION EIGHT
      PARAMETER (EIGHT=8.0D0)
      DOUBLE PRECISION TEN
      PARAMETER (TEN=1.0D1)
      DOUBLE PRECISION FCMIN
      PARAMETER (FCMIN=1.0D-2)
      DOUBLE PRECISION FCMIN2
      PARAMETER (FCMIN2=1.0D-4)
      DOUBLE PRECISION FCNLIN
      PARAMETER (FCNLIN=1.0D-2)
      DOUBLE PRECISION SIGMA
      PARAMETER (SIGMA=2.0D0)
      INTEGER I,ICA,ICB,IH,IR,IRANKA,IRANKB,IREPET,IRKMAX,IS,
     *ISUM,ITER,I0,J,JJ,JN,JRED,J1,K,KC,KFLAG,
     *KOUNT,KPRINT,L,LEVEL,NB,NE,NEW,NY,NYMAX
      DOUBLE PRECISION COND,COND1,COND2,CONV,CONVA,DEL,EPH,
     *EPSMIN,FC,FCA,FCDNM,FCH,FCMINH,FCNMP2,FCNUM,FCNUMK,FCNUMP,
     *HSTART,MUE,S,SENS1,SENS2,SIGDEL,SIGDLH,SKAP,
     *SMALIN,ST,SUMF,SUMX,SUMXA,TFAIL,TH,TOLH,TOLMIN,
     *EPX1H,CONDE
      LOGICAL DIFAPP,FCOMPT,JACRFR,NEXT,REDUCT
      INTEGER L1,L2
      DOUBLE PRECISION S1
C:    Begin
C:    Begin of Segment Bvpsol.Body
C       ----------------------------------------------------------
C       1 Initialization
C       ----------------------------------------------------------
C       1.1 Internal parameters
C       Standard values fixed below
C       Minimum relative precision of integrator ( to be adapted )
        CALL ZIBCONST(EPMACH,SMALL)
        TOLMIN = EPMACH*TEN*TEN
C       Maximum permitted number of iterative refinements sweeps
C       Maximum permitted sub - condition number of senitivity
C         matrix E
        CONDE = ONE/EPMACH
        NYMAX = M-1
C
        FCMINH = DSQRT(FCMIN)
C       ----------------------------------------------------------
C       1.1.1 Common parameters
C       Starting value of relaxation factor (FCMIN.LE.FC.LE.1.0)
        IF(NONLIN.LE.1)THEN
C         for linear or mildly nonlinear problems
          FC = ONE
        ELSE
C         for highly nonlinear problems
          FC = FCNLIN
        ENDIF
C       Starting value for pseudo - rank of matrix A
        IRANK = N
C       Minimum reasonable value for EPS
        EPSMIN = DSQRT(TEN*EPMACH)
        IF(EPS.LT.EPSMIN) EPS = EPSMIN
C       ----------------------------------------------------------
C       1.2 Initial preparations
        IF(FC.LT.FCMIN) FC = FCMIN
        IF(FC.GT.ONE) FC = ONE
        KPRINT = INFO
        ITER = 0
        KOUNT = 0
        IREPET = 0
        INFO = -1000
        FCA = FC
        CONV = ONE
        JACRFR = .FALSE.
C:      Begin SetVec.Vec
        DO 5 L1=1,NM
          XA(L1)=X(L1)
5       CONTINUE
C.      End SetVec.Vec
        IF(TOL.LE.ZERO) TOL = EPS/TEN
        IF(TOL.LT.TOLMIN) TOL = TOLMIN
        DIFAPP = .TRUE.
        HSTART =(T(2)-T(1))*REDH
        SENS1 = ZERO
        COND1 = ONE
        SENS2 = ZERO
        COND2 = ONE
        IRKMAX = 0
        IRANKB = 0
        SIGDLH = ZERO
        SUMF = ONE
C:      Mat IA = Scalar (Rows 1,N ; Cols 1,N)
        L1 = 0
        DO 6 L2=1,N
        DO 6 L44=1,N
          IA(L2,L44)=L1
6       CONTINUE
C.      End SetIntMat.S
C:      Mat IB = Scalar (Rows 1,N ; Cols 1,N)
        L1 = 0
        DO 7 L2=1,N
        DO 7 L44=1,N
          IB(L2,L44)=L1
7       CONTINUE
C.      End SetIntMat.S
C:      CubeMat G (layer 1)= Scalar (Rows 1,N ; Cols 1,N)
        S1 = ZERO
        DO 8 L1=1,N
        DO 8 L2=1,N
          G(L1,L2,1)=S1
8       CONTINUE
C.      End SetCubeMat.S
        IF(KPRINT.GE.0)THEN
C         Print Start vector data, predescribed precision and max
C         iteration steps
9         FORMAT('Initial ','data',//)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,9)
          DO 10 J=1,M
11          FORMAT(D13.5,2X)
            WRITE(LUMON,11)T(J)
12          FORMAT((14X,3(D20.10,1X)))
            WRITE(LUMON,12)(X(L1),L1=(J-1)*N+1,J*N)
10        CONTINUE
13        FORMAT('N ','=',I2,2X,'M ','=',I2,/,
     *    'Prescribed ','relative ','precision',D10.2,2X,/,
     *    'Maximum ','permitted ','number ','of ','iteration ',
     *    'steps',1X,I3,//,'1')
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,13)N,M,EPS,ITMAX
          IF(KPRINT.EQ.0)THEN
14          FORMAT(1X,66('*'))
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,14)
15          FORMAT(4X,'It',4X,'Ny',7X,'Levelf',10X,'Levelx',8X,
     *      'Rel.Fc.')
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,15)
          ENDIF
        ENDIF
C       ----------------------------------------------------------
C       1.3 Startup step
C       ----------------------------------------------------------
C       1.3.1 Computation of the residual vector
c
        CALL BLFCNI(IVPSOL,FCN,BC,N,M,NM,NM1,ITER,KPRINT,HSTART,
     *  FCMIN,T,X,X1,XM,T1,XU,HH,R,TOL,FC,FCOMPT,REDUCT,KFLAG,
     *  KOUNT,INFO,LUMON)
C
C       Main iteration loop
C       ===================
C
C:      While (expression)
16      IF(INFO.EQ.-1000)THEN
C:          Begin of Segment Bvpsol.Core
C             ----------------------------------------------------
C             2 Startup of iteration step
              IF(.NOT.JACRFR)THEN
                LEVEL = 0
C               --------------------------------------------------
C               2.1 Scaling of variables X(NM)
                CALL BLSCLE(N,M,NM,NM1,X,XU,XW,XTHR)
                IF(ITER.NE.0)THEN
                  IRANKA = IRANK
C:                Begin SetVec.Vec
                  DO 17 L1=1,NM
                    DXQA(L1)=DXQ(L1)
17                CONTINUE
C.                End SetVec.Vec
C:                FCNUM = Sum of Formula Elements (for 1,NM)
                  FCNUM = 0.0D0
                  DO 18 L1=1,NM
                    FCNUM=FCNUM+((DX(L1)/XW(L1))**2)
18                CONTINUE
C.                End MakeSum.Comp
C:                FCNMP2 = Sum of Formula Elements (for 1,NM)
                  FCNMP2 = 0.0D0
                  DO 19 L1=1,NM
                    FCNMP2=FCNMP2+((DXQ(L1)/XW(L1))**2)
19                CONTINUE
C.                End MakeSum.Comp
                  FCNUMP = FCNUM*FCNMP2
                ENDIF
                IF(ITER.NE.0)THEN
                  IF(IRANK.GE.NB.AND.FC.GT.FCMINH) IRANK = NE
                  IF(IRANK.LT.NB) IRANK = NB
                  TH = FC-ONE
C:                FCDNM = Sum of Formula Elements (for 1,NM)
                  FCDNM = 0.0D0
                  DO 20 L1=1,NM
                    FCDNM=FCDNM+(((DXQ(L1)+TH*DX(L1))/XW(L1))**2)
20                CONTINUE
C.                End MakeSum.Comp
                  FCH = DSQRT(FCNUM/FCDNM)*FC*FC*HALF
C                 ------------------------------------------------
C                 2.1.1 Decision criterion for Jacobian updating
C                       technique:
C                       DIFAPP.EQ..TRUE. numerical
C                       differentiation,
C                       DIFAPP.EQ..FALSE. rank1 updating
                  DIFAPP = FC.LT.FCA.AND.NEW.GT.0.OR.FCH.LT.FC*
     *            SIGMA.OR.IRANKA.LT.IRANK.OR.EPH*REDH.GT.EPS
                  FCA = FC
                  IF(NONLIN.GT.0) FC = DMIN1(FCH,ONE)
                ENDIF
C               --------------------------------------------------
C               2.2 Difference approximation of jacobian matrix A
C                   ( If Difapp.EQ..TRUE. ) or
C                   Rank-1 update of jacobian matrix A ( If Difapp
C                   .EQ..FALSE. )
                CALL BLDERA(BC,N,M,NM,XW,X1,XM,R,T2,A,B,RELDIF)
C               --------------------------------------------------
C               2.3 Determination of sparse structure of matrices
C                   A and B and determination of internal row
C                   scaling of sensitivity matrix E
                ISUM = 0
                DO 21 I=1,N
                  S = ZERO
                  DO 22 K=1,N
                    TH = DABS(A(I,K))*XW(K)
                    IF(S.LT.TH) S = TH
                    TH = DABS(B(I,K))*XW(K+NM1)
                    IF(S.LT.TH) S = TH
22                CONTINUE
                  IF(S.LT.XTHR) S = XTHR
                  DE(I)=SMALL/S
                  DO 23 K=1,N
                    IF(IA(I,K).LE.0)THEN
                      IF(A(I,K).NE.ZERO)THEN
                        IA(I,K)=1
                        ISUM = 1
                      ENDIF
                    ENDIF
                    IF(IB(I,K).LE.0)THEN
                      IF(B(I,K).NE.ZERO)THEN
                        IB(I,K)=1
                        ISUM = 1
                      ENDIF
                    ENDIF
23                CONTINUE
21              CONTINUE
                IF(ISUM.NE.0)THEN
C                 ------------------------------------------------
C                 2.3.1 Determination of row and column
C                       permutation vectors
                  DO 24 I=1,N
                    ICOL(I)=I
                    ICOLB(I)=I
                    IROW(I)=I
24                CONTINUE
C                 ------------------------------------------------
C                 2.3.2 Search for separable linear boundary
C                       conditions at T(1)
                  NE = N
                  DO 25 I=1,N
                      DO 26 K=1,N
                        IF(IB(I,K).NE.0) GOTO 9996
26                    CONTINUE
                      ISUM = 0
                      DO 27 K=1,N
                        IF(IA(I,K).NE.0)THEN
                          ISUM = ISUM+1
                          ICA = K
                        ENDIF
27                    CONTINUE
                      IF(ISUM.LE.1)THEN
                        DO 28 IS=1,N
                          IH = ICOL(IS)
                          IF(IH.EQ.ICA) ICOL(IS)=ICOL(NE)
                          IH = IROW(IS)
                          IF(IH.EQ.I) IROW(IS)=IROW(NE)
28                      CONTINUE
                        ICOL(NE)=ICA
                        IROW(NE)=I
                        NE = NE-1
                        IF(DABS(R(I)).GT.TEN*EPMACH*DABS(X(ICA)))
     *                  THEN
                          INFO = -5
                          GOTO 9998
                        ENDIF
                      ENDIF
9996                CONTINUE
25                CONTINUE
                  IF(KPRINT.GE.0.AND.NE.EQ.0)THEN
29                  FORMAT('Warning: ','attempt ','to ',
     *              'solve ','initial ','value ','problem')
                    write ( lumon, '(a)' ) ' '
                    WRITE(LUMON,29)
                  ENDIF
                  IF(IRANK.GT.NE) IRANK = NE
                  IRANKA = IRANK
C                 ------------------------------------------------
C                 2.3.3 Search for separable linear boundary
C                       conditions at T(M)
                  NB = 0
                ENDIF
                IF(ISUM.NE.0.AND.NE.NE.0)THEN
                  DO 30 I=1,NE
                      IR = IROW(I)
                      DO 31 K=1,N
                        IF(IA(IR,K).NE.0) GOTO 9995
31                    CONTINUE
                      ISUM = 0
                      DO 32 K=1,N
                        IF(IB(IR,K).NE.0)THEN
                          ISUM = ISUM+1
                          ICB = K
                        ENDIF
32                    CONTINUE
                      IF(ISUM.LE.1)THEN
                        NB = NB+1
                        DO 33 IS=1,N
                          IH = ICOLB(IS)
                          IF(IH.EQ.ICB) ICOLB(IS)=ICOLB(NB)
33                      CONTINUE
                        ICOLB(NB)=ICB
                        IROW(I)=IROW(NB)
                        IROW(NB)=IR
                        IF(DABS(R(IR)).GT.TEN*EPMACH*DABS(X(ICB+
     *                  NM1)))THEN
                          INFO = -5
                          GOTO 9998
                        ENDIF
                      ENDIF
9995                CONTINUE
30                CONTINUE
                  IF(KPRINT.GE.0.AND.NB.EQ.N)THEN
34                  FORMAT('Warning: ','attempt ','to ',
     *              'solve ','initial ','value ','problem')
                    write ( lumon, '(a)' ) ' '
                    WRITE(LUMON,34)
                  ENDIF
C                 Initial rank strategy for highly nonlinear
C                   problems
                  IF(NB.LT.NE.AND.ITER.EQ.0.AND.NONLIN.GT.2) IRANK
     *            = NB
                ENDIF
              ENDIF
              JACRFR = .FALSE.
              IF(DIFAPP)THEN
                NEW = 0
                KFLAG = 0
                CALL BLDERG(FCN,N,NE,M,M1,NM,NM1,T,X,XU,XW,T2,
     *          TFAIL,G,ICOL,IVPSOL,HSTART,TOL,RELDIF,KFLAG)
                IF(KFLAG.LT.0)THEN
                  INFO = -3
                  GOTO 9998
                ENDIF
                IF(M.GT.2) KOUNT = KOUNT+N
                IF(M.EQ.2) KOUNT = KOUNT+NE
              ELSE
                NEW = NEW+1
                CALL BLRK1G(N,M,M1,NM,NM1,XW,DX,HH,HHA,T1,G,FCA)
              ENDIF
C             ----------------------------------------------------
C             2.3.4 Computation of sensitivity matrix E =-A+B*G(M1
C                   *..*G(1))
C                   (projections included)
              IF(IRANK.NE.0)THEN
                DO 35 I=1,NE
                  IR = IROW(I)
C:                Mat E(Row I)= Mat B(Row IR)* Scalar  (for 1,N)
                  S1 = DE(IR)
                  DO 36 L1=1,N
                    E(I,L1)=B(IR,L1)*S1
36                CONTINUE
C.                End SetRow.RowxS
35              CONTINUE
                DO 37 JJ=1,M1
                  J = M-JJ
                  DO 38 I=1,NE
                    DO 39 K=1,N
                      S = ZERO
                      DO 40 L=1,N
                        S = S+E(I,L)*G(L,K,J)
40                    CONTINUE
                      T1(K)=S
39                  CONTINUE
                    DO 41 K=1,N
                      E(I,K)=T1(K)
41                  CONTINUE
38                CONTINUE
37              CONTINUE
C               --------------------------------------------------
C               2.4 Prepare solution of the linear system
C               --------------------------------------------------
C               2.4.1 internal row and column scaling of matrix A
C               INTERNAL ROW AND COLUMN SCALING AND PERMUTATION OF
C                 MATRIX E
                DO 42 K=1,NE
                  KC = ICOL(K)
                  S = XW(KC)
                  DO 43 I=1,NE
                    IR = IROW(I)
                    E(I,K)=-(A(IR,KC)*DE(IR)+E(I,KC))*S
43                CONTINUE
42              CONTINUE
C               --------------------------------------------------
C               2.4.2 Save matrix E on EH
C:              Mat EH = Mat E (Rows 1,NE ; Cols 1,NE)
                DO 44 L1=1,NE
                DO 44 L2=1,NE
                  EH(L1,L2)=E(L1,L2)
44              CONTINUE
C.              End SetMat.Mat
              ENDIF
              IRANKB = NB
C             ----------------------------------------------------
C             2.4.3 Monitor for actually applied maximum rank
              IF(IRKMAX.LT.IRANK) IRKMAX = IRANK
C             ----------------------------------------------------
C             2.5 Save values of R(N)and HH((M-1)*N)
              IF(IREPET.EQ.0)THEN
C:              Begin SetVec.Vec
                DO 45 L1=1,N
                  RA(L1)=R(L1)
45              CONTINUE
C.              End SetVec.Vec
C:              Begin SetVec.Vec
                DO 46 L1=1,NM1
                  HHA(L1)=HH(L1)
46              CONTINUE
C.              End SetVec.Vec
              ENDIF
              NEXT = .FALSE.
C
C             Pseudo-rank reduction loop
C             ==========================
C
C:            DO (Until)
47            CONTINUE
C               --------------------------------------------------
C               3 Main-part of iteration step
C               --------------------------------------------------
C               3.1 Solution of the linear system
C               --------------------------------------------------
C               3.1.1 Constrained QR-decomposition of ( ( COMMA NE
C                     NE ) ) - matrix E
                COND = CONDE
                IF(IRANK.GT.0) CALL BLDECC(E,N,N,IRANKB,NE,NE,
     *          IRANK,COND,D,PIVOT,IREPET,QE,T1)
                IF(NONLIN.EQ.0.AND.IRANK.LT.NE)THEN
                  INFO = -8
                  GOTO 9998
                ENDIF
C               --------------------------------------------------
C               3.1.2 evaluation of subcondition and sensitivity
C                     numbers
                COND1 = ONE
                COND2 = ONE
                SENS1 = ZERO
                SENS2 = ZERO
                IF(IRANKB.NE.0)THEN
                  SENS1 = DABS(D(1))
                  COND1 = SENS1/DABS(D(IRANKB))
                ENDIF
                IF(IRANKB.NE.IRANK)THEN
                  SENS2 = DABS(D(IRANKB+1))
                  COND2 = SENS2/DABS(D(IRANK))
                ENDIF
                IF(FCA.GE.1.0D0.AND.FC.GE.1.0D0.AND.ITER.NE.0)THEN
                  IF(IRANKB.NE.IRANK.AND.SENS2.LT.(EPS/REDH)*SMALL)
     *            IRANK = IRANKB
                  IF(IRANKB.NE.0.AND.SENS1.LT.(EPS/REDH)*SMALL)
     *            IRANK = 0
                ENDIF
C               --------------------------------------------------
C               3.1.3 (best) (least squares) solution of linear (N,
C                     N)-system
                CALL BLSOLI(N,M,M1,NM,NM1,LEVEL,NE,NB,IRANK,IRANKB,
     *          IREPET,NYMAX,KPRINT,EPS,REDH,TOLMIN,TOL,RELDIF,EPH,
     *          EPX1H,SIGDEL,SIGDLH,E,EH,HH,DHH,R,A,B,BG,G,QE,U,QU,
     *          DE,DU,T1,T2,US,DX1,D,DDX,DXQ,XW,DR,RF,IROW,ICOL,
     *          ICOLB,PIVOT,NY,INFO,LUMON)
                IF(INFO.NE.-1000) GOTO 9998
C               --------------------------------------------------
C               3.2 Evaluation of scaled natural level function
C                   SUMX
C                   scaled maximum error norm CONV
C                   evaluation of (scaled) standard level function
C                   SUMF ( SUMF only, if KPRINT.GE.0 )
C                   and computation of ordinary newton corrections
C                   DX(N)
                CALL BLLVLS(N,M,NM,NM1,XW,DXQ,HH,R,DE,CONV,SUMX,
     *          SUMF,KPRINT)
C:              Begin SetVec.Vec
                DO 48 L1=1,NM
                  DX(L1)=DXQ(L1)
48              CONTINUE
C.              End SetVec.Vec
                IF(IREPET.EQ.0.AND.IRANK.NE.0)THEN
C:                Begin SetVec.Vec
                  DO 49 L1=1,IRANK
                    QU(L1)=U(L1)
49                CONTINUE
C.                End SetVec.Vec
                ENDIF
C:              Begin SetVec.Vec
                DO 50 L1=1,NM
                  XA(L1)=X(L1)
50              CONTINUE
C.              End SetVec.Vec
                SUMXA = SUMX
                CONVA = CONV
C               --------------------------------------------------
C               3.3 a - priori estimate of relaxation factor FC
                JRED = 0
                REDUCT = .FALSE.
                IF(ITER.NE.0.AND.NONLIN.NE.0)THEN
                  IF(NEW.LE.0.AND.(IRANK.GE.NE.OR.IRANKA.GE.NE)
     *            .OR.IREPET.NE.0)THEN
C                   ----------------------------------------------
C                   3.3.1 Full rank case (independent of preceding
C                         rank) computation of the denominator of
C                         a-priori estimate
C:                  FCDNM = Sum of Formula Elements (for 1,NM)
                    FCDNM = 0.0D0
                    DO 51 L1=1,NM
                      FCDNM=FCDNM+(((DX(L1)-DXQA(L1))/XW(L1))**2)
51                  CONTINUE
C.                  End MakeSum.Comp
                    IF(IRANK.NE.N)THEN
C                     --------------------------------------------
C                     3.4 Rank - deficient case ( if previous rank
C                         was full )
C                     --------------------------------------------
C                     3.4.1 Computation of the projected
C                           denominator of a-priori estimate
C:                    Vec T1 = Scalar (for 1,N)
                      S1 = ZERO
                      DO 52 L1=1,N
                        T1(L1)=S1
52                    CONTINUE
C.                    End SetVec.S
                      IF(IRANK.NE.0)THEN
                        DO 53 L=1,NE
                          K = ICOL(L)
                          DX1(L)=DXQA(K)/XW(K)
53                      CONTINUE
C                       ------------------------------------------
C                       3.4.2 Projection for reduced component DX1
C                             (NE)
                        CALL BLPRJC(N,NE,IRANK,DEL,DX1,D,T2,QE,
     *                  PIVOT)
                        DO 54 L=1,NE
                          K = ICOL(L)
                          T1(K)=DX1(L)*XW(K)
54                      CONTINUE
                      ENDIF
                      DO 55 J=1,M1
                        DO 56 I=1,N
                          S = ZERO
                          DO 57 K=1,N
                            S = S+T1(K)*G(I,K,J)
57                        CONTINUE
                          T2(I)=S
56                      CONTINUE
C:                      Begin SetVec.Vec
                        DO 58 L1=1,N
                          T1(L1)=T2(L1)
58                      CONTINUE
C.                      End SetVec.Vec
                        I0 = J*N
                        DO 59 I=1,N
                          ST = ONE/XW(I+I0)
                          S = T1(I)
                          DEL = DEL+S*ST*ST*(S+(DX(I+I0)-DXQA(I+I0))
     *                    *TWO)
59                      CONTINUE
55                    CONTINUE
                      FCDNM = FCDNM+DEL
                    ENDIF
                    FCDNM = FCDNM*SUMX
C                   ----------------------------------------------
C                   3.4.3 New relaxation factor
                    IF(FCDNM.GE.FCNUMP*FCMIN2)THEN
                      MUE = FCA*DSQRT(FCNUMP/FCDNM)
                      FC = DMIN1(MUE,ONE)
                    ELSE
                      FC = ONE
                    ENDIF
                  ENDIF
                  IREPET = 0
                  REDUCT = FC.LT.FCMIN
                ENDIF
                LEVEL = 1
                  IF(.NOT.REDUCT)THEN
C                   ----------------------------------------------
C                   3.5 Save natural level for later computations
C                       of corrector and print iterate
                    FCNUMK = SUMX
                    IF(KPRINT.GE.0)THEN
C                     Print Standard - and natural level
                      IF(KPRINT.GT.0)THEN
60                      FORMAT(1X,66('*'))
                        write ( lumon, '(a)' ) ' '
                        WRITE(LUMON,60)
61                      FORMAT(4X,'It',4X,'Ny',7X,'Levelf',10X,
     *                  'Levelx',18X,'New',4X,'Rank')
                        write ( lumon, '(a)' ) ' '
                        WRITE(LUMON,61)
                      ENDIF
62                    FORMAT(4X,I2,4X,I2,5X,D10.3,2X,4X,D10.3
     *                ,2X,13X,I2,6X,I2)
                      write ( lumon, '(a)' ) ' '
                      WRITE(LUMON,62)ITER,NY,SUMF,SUMXA,NEW,IRANK
                      IF(KPRINT.GT.0)THEN
63                      FORMAT(1X,66('*'))
                        write ( lumon, '(a)' ) ' '
                        WRITE(LUMON,63)
                      ENDIF
                    ENDIF
C
C                   Relaxation-factor reduction loop
C                   ================================
C
C:                  DO (Until)
64                  CONTINUE
C                     --------------------------------------------
C                     3.6 Preliminary new iterate
C:                    DO (Until)
65                    CONTINUE
                        FCOMPT = .FALSE.
C:                      Vec X = Vec XA + Vec DX * Scalar (for 1,NM)
                        S1 = FC
                        DO 66 L1=1,NM
                          X(L1)=XA(L1)+DX(L1)*S1
66                      CONTINUE
C.                      End SetVec.Vec&VecxS
                        IF(ITER.GT.ITMAX)THEN
                          INFO = -2
                          GOTO 9997
                        ENDIF
C                       ------------------------------------------
C                       3.6.1 Computation of the residual vector
                        CALL BLFCNI(IVPSOL,FCN,BC,N,M,NM,NM1,ITER,
     *                  KPRINT,HSTART,FCMIN,T,X,X1,XM,T1,XU,HH,R,
     *                  TOL,FC,FCOMPT,REDUCT,KFLAG,KOUNT,INFO,LUMON)
                        IF(INFO.NE.-1000) GOTO 9997
                        IF(REDUCT) GOTO 9994
                      IF(.NOT.(FCOMPT)) GOTO  65
C.                    UNTIL ( expression - negated above)
C                     --------------------------------------------
C                     3.6.2 (best) (least squares) solution of
C                           linear (N,N) -system
                      CALL BLSOLI(N,M,M1,NM,NM1,LEVEL,NE,NB,IRANK,
     *                IRANKB,IREPET,NYMAX,KPRINT,EPS,REDH,TOLMIN,
     *                TOL,RELDIF,EPH,EPX1H,SIGDEL,SIGDLH,E,EH,HH,
     *                DHH,R,A,B,BG,G,QE,U,QU,DE,DU,T1,T2,US,DX1,D,
     *                DDX,DXQ,XW,DR,RF,IROW,ICOL,ICOLB,PIVOT,NY,
     *                INFO,LUMON)
                      IF(INFO.NE.-1000) GOTO 9998
C                     --------------------------------------------
C                     3.6.3 Evaluation of scaled natural level
C                           function SUMX
C                           scaled maximum error norm CONV and
C                           evaluation of (scaled) standard level
C                           function SUMF
                      CALL BLLVLS(N,M,NM,NM1,XW,DXQ,HH,R,DE,CONV,
     *                SUMX,SUMF,KPRINT)
C                     --------------------------------------------
C                     3.7 Rank independent convergence test
                      IF(CONV.LE.EPS.AND.IRKMAX.EQ.NE)THEN
                        INFO = 0
                        GOTO 9997
                      ENDIF
C                     --------------------------------------------
C                     3.8 Natural monotonicity test
                      IF(SUMX.GT.SUMXA)THEN
C                       ------------------------------------------
C                       3.9 Output of iterate
                        IF(KPRINT.GE.0)THEN
C                         Print Standard - and natural level, and
C                         damping factor
                          IF(KPRINT.GT.0)THEN
67                          FORMAT(1X,66('*'))
                            write ( lumon, '(a)' ) ' '
                            WRITE(LUMON,67)
68                          FORMAT(4X,'It',4X,'Ny',7X,'Levelf',
     *                      10X,'Levelx',8X,'Rel.Fc.')
                            write ( lumon, '(a)' ) ' '
                            WRITE(LUMON,68)
                          ENDIF
69                        FORMAT(4X,I2,4X,I2,5X,D10.3,2X,4X,D10.3
     *                    ,2X,4X,F5.3)
                          write ( lumon, '(a)' ) ' '
                          WRITE(LUMON,69)ITER,NY,SUMF,SUMX,FC
                          IF(KPRINT.GT.0)THEN
70                          FORMAT(1X,66('*'))
                            write ( lumon, '(a)' ) ' '
                            WRITE(LUMON,70)
                          ENDIF
                        ENDIF
                        JRED = JRED+1
                        IF(NONLIN.EQ.0)THEN
                          INFO = -4
                          GOTO 9997
                        ENDIF
C                       ------------------------------------------
C                       3.10 Compute reduced relaxation factor FC
                        TH = FC-ONE
C:                      FCDNM = Sum of Formula Elements (for 1,NM)
                        FCDNM = 0.0D0
                        DO 71 L1=1,NM
                          FCDNM=FCDNM+(((DXQ(L1)+TH*DX(L1))/XW(L1))
     *                    **2)
71                      CONTINUE
C.                      End MakeSum.Comp
                        FC = DSQRT(FCNUMK/FCDNM)*FC*FC*HALF
C                       Rank reduction, if relaxation factor to
C                         small
                        REDUCT = FC.LT.FCMIN.OR.NEW.GT.0.AND.JRED
     *                  .GT.1
                      ELSE
                        NEXT = .TRUE.
                      ENDIF
                    IF(.NOT.(NEXT.OR.REDUCT)) GOTO  64
C.                  UNTIL ( expression - negated above)
C
C                   End of relaxation-factor reduction loop
C                   =======================================
C
                  ENDIF
9994            CONTINUE
                IF(.NOT.NEXT)THEN
C                 ------------------------------------------------
C                 3.11 Restore former values for repeting
C                      iteration step
                  IREPET = 1
C                 Restore former values
                  LEVEL = 0
C:                Begin SetVec.Vec
                  DO 72 L1=1,N
                    R(L1)=RA(L1)
72                CONTINUE
C.                End SetVec.Vec
C:                Begin SetVec.Vec
                  DO 73 L1=1,N
                    X1(L1)=XA(L1)
73                CONTINUE
C.                End SetVec.Vec
C:                Begin SetVec.Vec
                  DO 74 L1=1,N
                    XM(L1)=XA(L1+NM1)
74                CONTINUE
C.                End SetVec.Vec
C:                Begin SetVec.Vec
                  DO 75 L1=1,NM
                    X(L1)=XA(L1)
75                CONTINUE
C.                End SetVec.Vec
C:                Begin SetVec.Vec&Vec
                  DO 76 L1=1,NM1
                    XU(L1)=X(L1+N)+HHA(L1)
76                CONTINUE
C.                End SetVec.Vec&Vec
C:                Begin SetVec.Vec
                  DO 77 L1=1,NM1
                    HH(L1)=HHA(L1)
77                CONTINUE
C.                End SetVec.Vec
                  IF(KPRINT.GE.0)THEN
78                  FORMAT(5X,I2,1X,'Not ','accepted ',
     *              'relaxation ','factor',5X,F5.3,12X,I2)
                    write ( lumon, '(a)' ) ' '
                    WRITE(LUMON,78)ITER,FC,IRANK
                  ENDIF
                  IF(ITER.EQ.0)THEN
                    FC = FCMIN
                  ENDIF
                  IF(NEW.GT.0)THEN
                    DIFAPP = .TRUE.
                    JACRFR = .TRUE.
                    IRANK = NE
                    IF(IRANK.LT.NB) IRANK = NB
                    GOTO 9998
                  ENDIF
C                 ------------------------------------------------
C                 3.12 Pseudo-rank reduction
                  IREPET = -1
                  IF(IRANK.EQ.0)THEN
                    INFO = -11
                    GOTO 9997
                  ENDIF
C:                Begin SetVec.Vec
                  DO 79 L1=1,IRANK
                    U(L1)=QU(L1)
79                CONTINUE
C.                End SetVec.Vec
                  IRANK = IRANK-1
                  IF(IRANKB.GT.IRANK) IRANKB = IRANK
                ENDIF
              IF(.NOT.(NEXT)) GOTO  47
C.            UNTIL ( expression - negated above)
C
C             End of pseudo-rank reduction loop
C             =================================
C
C             ----------------------------------------------------
C             4 Preparations to start the following iteration step
              ITER = ITER+1
              IRANKA = IRANK
C             Preliminary pseudo-rank
              IF(IRANK.GE.NB.AND.FC.GT.FCMINH) IRANK = NE
              IF(IRANK.LT.NB) IRANK = NB
C             ----------------------------------------------------
C             4.1 Print values
              IF(KPRINT.GE.0)THEN
C               Print Standard - and natural level, and damping
C               factor
                IF(KPRINT.GT.0)THEN
80                FORMAT(1X,66('*'))
                  write ( lumon, '(a)' ) ' '
                  WRITE(LUMON,80)
81                FORMAT(4X,'It',4X,'Ny',7X,'Levelf',10X,
     *            'Levelx',8X,'Rel.Fc.')
                  write ( lumon, '(a)' ) ' '
                  WRITE(LUMON,81)
                ENDIF
82              FORMAT(4X,I2,4X,I2,5X,D10.3,2X,4X,D10.3,2X,4X,F5.3)
                write ( lumon, '(a)' ) ' '
                WRITE(LUMON,82)ITER,NY,SUMF,SUMX,FC
                IF(KPRINT.GT.0)THEN
83                FORMAT(1X,66('*'))
                  write ( lumon, '(a)' ) ' '
                  WRITE(LUMON,83)
                  DO 84 J=1,M
85                  FORMAT(D13.5,2X)
                    WRITE(LUMON,85)T(J)
86                  FORMAT((14X,3(D20.10,1X)))
                    WRITE(LUMON,86)(X(L1),L1=(J-1)*N+1,J*N)
84                CONTINUE
                ENDIF
              ENDIF
9997        CONTINUE
C.          End of Segment Bvpsol.Core
9998      CONTINUE
        GOTO 16
        ENDIF
C.      EndWhile
C
C       End of main iteration loop
C       ==========================
C
C       ----------------------------------------------------------
C       5 Exits
C       ----------------------------------------------------------
C       5.1 Solution exit
        IF(INFO.EQ.0)THEN
          ITER = ITER+1
C:        Vec X = Vec X + Vec DXQ (for 1,NM)
          DO 87 L1=1,NM
            X(L1)=X(L1)+DXQ(L1)
87        CONTINUE
C.        End SetVec.&Vec
          INFO = ITER
          IF(KPRINT.LT.0)THEN
            GOTO 9999
          ENDIF
          IF(KPRINT.GT.0)THEN
C           Print levels, damping factor of last iteration step
88          FORMAT(1X,66('*'))
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,88)
89          FORMAT(4X,'It',4X,'Ny',7X,'Levelf',10X,'Levelx',8X,
     *      'Rel.Fc.')
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,89)
          ENDIF
90        FORMAT(4X,I2,4X,I2,5X,D10.3,2X,4X,D10.3,2X,4X,F5.3)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,90)ITER,NY,SUMF,SUMX,FC
91        FORMAT(1X,66('*'))
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,91)
92        FORMAT('1')
          WRITE(LUMON,92)
          IF(IRANK.LT.NE)THEN
            INFO = -1
          ELSE
C           Print solution info
93          FORMAT('Solution ','of',1X,'boundary ','value ',
     *      'problem',' obtained',/,'BVPSOL',' required',I3,1X,
     *      'Iteration ','steps ','with',I4,1X,'trajectory',
     *      ' evaluations',//)
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,93)ITER,KOUNT
            CALL BLPRCV(LUMON,CONV,EPH)
          ENDIF
        ENDIF
C       ----------------------------------------------------------
C       5.2 Fail exit messages
C       ----------------------------------------------------------
C       5.2.1 Rank-deficiency : best least squares solution of bvp
C             obtained
        IF(INFO.EQ.-1.AND.KPRINT.GE.0)THEN
94        FORMAT('Iteration ','terminates ','at ',
     *    'stationary ','point',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,94)
          CALL BLPRCV(LUMON,CONVA,EPH)
          IF(ITER.NE.0)THEN
            SKAP = ZERO
            IF(FCA.EQ.ONE.AND.FC.EQ.ONE.AND.IRANKA.EQ.IRANK) SKAP
     *      = DSQRT(SUMXA/FCNUMK)
            IF(SKAP.GT.ZERO)THEN
95            FORMAT('Incompatibility ','factor ','kappa',D10.3
     *        ,2X,/)
              write ( lumon, '(a)' ) ' '
              WRITE(LUMON,95)SKAP
            ENDIF
          ENDIF
        ENDIF
C       ----------------------------------------------------------
C       5.2.2 Termination after more than itmax iterations
        IF(INFO.EQ.-2.AND.KPRINT.GE.0)THEN
96        FORMAT('Iteration ','terminates ','after ','itmax ',
     *    '=',I3,2X,'iteration ','steps')
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,96)ITMAX
        ENDIF
C       ----------------------------------------------------------
C       5.2.3 Integrator failed in trajectory computation
        IF(INFO.EQ.-3.AND.KPRINT.GE.0)THEN
97        FORMAT('Integrator ','failed ','in ','trajectory ',
     *    'computation ',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,97)
          J1 =-KFLAG
98        FORMAT('BVPSOL ','terminates',/,'Subinterval',I3,1X,
     *    'possibly ','insert ','new ','node',D20.11,2X,/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,98)J1,TFAIL
        ENDIF
C       ----------------------------------------------------------
C       5.2.4 Convergence fail of Gauss - Newton method
        IF(INFO.EQ.-4.AND.KPRINT.GE.0)THEN
99        FORMAT('Gauss ','Newton ','method ','fails ','to ',
     *    'converge',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,99)
        ENDIF
C       ----------------------------------------------------------
C       5.2.5 Inconsistent initial data
        IF(INFO.EQ.-5.AND.KPRINT.GE.0)THEN
100       FORMAT('Error: ','initial ','data ','and ',
     *    'boundary ','conditions ','are ','inconsistent',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,100)
        ENDIF
C       ----------------------------------------------------------
C       5.2.6 Convergence fail of iterative refinement sweeps
        IF(INFO.EQ.-6)THEN
          IF(KPRINT.GE.0)THEN
101         FORMAT('Termination ','since ','iterative ',
     *      'refinement ','fails ','to ','converge',/,2X,'Insert ',
     *      'new ','nodes',/)
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,101)
          ENDIF
          JN = JN-1
          IF(JN.GT.0)THEN
102         FORMAT(8X,'in ','subinterval',2X,I3,/)
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,102)JN
          ENDIF
        ENDIF
C       ----------------------------------------------------------
C       5.2.7 Insufficient error tolerance for integrator
        IF(INFO.EQ.-7.AND.KPRINT.GE.0)THEN
          TOLH = EPS/SIGDEL
          RELDIF = DSQRT(TOLH/SIGDEL)
103       FORMAT('Suggested ','integrator ','accuracy',D10.1
     *    ,2X,/,'Suggested ','relative ','deviation ',
     *    'parameter',D10.1,2X,/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,103)TOLH,RELDIF
104       FORMAT('Reduce ','relative ','error ','tolerance ',
     *    'for ','integrator ','to',D10.1,2X,/,2X,'or ','insert ',
     *    'new ','nodes',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,104)TOLH
          S = REDH/TOL
          DO 105 J=1,M1
            IF(RF(J).GT.S)THEN
106           FORMAT(2X,'in ','subinterval',I3,/)
              WRITE(LUMON,106)J
            ENDIF
105       CONTINUE
107       FORMAT('Reliable ','relative ','accuracy ',
     *    'greater ','than',1X,D6.1,2X,/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,107)1.0D-2
        ENDIF
C       ----------------------------------------------------------
C       5.2.8 ill - conditioned condensed linear system
        IF(INFO.EQ.-8.AND.KPRINT.GE.0)THEN
108       FORMAT('Gaussian ','block ','elimination ','fails',/,
     *    2X,'by ','ill ','- ','conditioned ','condensed ',
     *    'linear ','system',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,108)
          IF(IRANK.EQ.NE)THEN
109         FORMAT('Relative ','accuracy ','of ','DX1',D10.3
     *      ,2X)
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,109)EPX1H
          ENDIF
110       FORMAT('Possibly set IOPT(3)=1',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,110)
        ENDIF
C       ----------------------------------------------------------
C       5.2.9 rank reduced to zero
        IF(INFO.EQ.-11.AND.KPRINT.GE.0)THEN
11001     FORMAT('rank ','reduction ','failed - ',
     *    2X,'resulting ','rank ','is ','zero ',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,11001)
        ENDIF
C       ----------------------------------------------------------
C       5.3 Common exit
        IF(KPRINT.GE.0)THEN
C
          J1 = 1
          SMALIN = ONE/SMALL
          IF(IRANKB.NE.0) 
     $      CALL BLPRCD(LUMON,COND1,SENS1,SMALIN,J1,IRANKB)
          IF(IRANKB.NE.IRANK)THEN
            J1 = IRANKB+1
            CALL BLPRCD(LUMON,COND2,SENS2,SMALIN,J1,IRANK)
          ENDIF
111       FORMAT('Multiple ','shooting ','condition',D10.3,2X,
     *    /,'1')
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,111)SIGDLH
          IF(INFO.GT.0)THEN
112         FORMAT('Solution ','data',/)
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,112)
          ENDIF
          IF(INFO.LT.0)THEN
113         FORMAT('Final ','data',/)
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,113)
          ENDIF
          DO 114 J=1,M
115         FORMAT(D13.5,2X)
            WRITE(LUMON,115)T(J)
116         FORMAT((14X,3(D20.10,1X)))
            WRITE(LUMON,116)(X(L1),L1=(J-1)*N+1,J*N)
114       CONTINUE
        ENDIF
C       End of exits
9999  CONTINUE

      RETURN
      END
      SUBROUTINE BVPG(FCN,BC,IVPSOL,N,M,M1,NM,NM1,NMX8,NZ,LICN,
     *LIRN,LISNQ,NKEEP,T,X,EPS,TOL,RELDIF,NONLIN,ITMAX,INFO,XTHR,
     *IROW,ICOLA,ICOLB,IA,IB,IW,G,A,B,E,WO,DX,DXQ,DXQA,XA,XW,XU,HH,
     *DHH,HHA,DE,R,DR,RA,U,DU,X1,XM,T1,T2,RF,IVECT,JVECT,ICN,IRN,
     *IKEEP,LUMON)

c*********************************************************************72
c
cc BVPG carries out the global linear solver approach.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)

      EXTERNAL FCN,BC,IVPSOL
      INTEGER N,M,M1,NM,NM1,NMX8,LISNQ,NKEEP,LUMON
      DOUBLE PRECISION T(M),X(NM)
      DOUBLE PRECISION EPS
      DOUBLE PRECISION TOL,RELDIF
      INTEGER NONLIN,ITMAX
      INTEGER INFO
      DOUBLE PRECISION XTHR
      INTEGER IRN(LIRN),ICN(LICN),IVECT(NZ),JVECT(NZ),IKEEP(
     *NKEEP)
      INTEGER IROW(N),ICOLA(N),ICOLB(N)
      INTEGER IA(N,N),IB(N,N)
      INTEGER IW(NMX8)
      DOUBLE PRECISION G(N,N,M1)
      DOUBLE PRECISION A(N,N),B(N,N)
      DOUBLE PRECISION E(LICN),WO(NM)
      DOUBLE PRECISION DX(NM),DXQ(NM),DXQA(NM),XA(NM),XW(NM),XU(
     *NM1),HH(NM1),DHH(NM1),HHA(NM1),DE(N),R(N),DR(N),RA(N),U(NM),
     *DU(NM),X1(N),XM(N),T1(N),T2(N),RF(M1)
C
C     Addtional dimensional integer variables:
C     ========================================
C
C       M1                M-1
C       NM                N*M
C       NM1               N*(M-1)
C       NMX8              8*N*M
C       LIRN,LICN,NKEEP,NZ
C                         See driver routine BVPSOL
C
C     Internal real arrays (workspace) :
C     ==================================
C
C       G(N,N,M1)        (N,N) -Wronskian Matrices G(1),...,G(M-1)
C                         .
C       A(N,N)            Wronskian Matrix on left boundary
C                         dBC/dX(X(1,...,N),T(1)).
C       B(N,N)            Wronskian Matrix on right boundary
C                         dBC/dX(X((N-1)*M+1,...,N*M),T(M)).
C       E(LICN)           Holds the values of the Jacobian stored
C                         in sparse mode.
C       DE(N)             Holds row scaling factors for the
C                         boundary conditions part of the Jacobian
C                         matrix.
C       DHH(NM1)          Holds the continuity residuals computed
C                         in BGSOLI .
C       DR(N)             Workspace for subroutine BGSOLI to hold
C                         the boundary residual
C                         BC(DXQ(1,...,N),DXQ((M-1)*N+1,...,M*N))+
C                         (A*DXQ(1,...,N))+B*DXQ((M-1)*N+1,...,M*N)
C                         .
C       DU(NM)            Used by BGSOLI . Gets the total residual
C                         for the current iterate.
C       DX(NM)            Actual newton correction.
C       DXQ(NM)           Simplified Newton correction J(k-1)*X(k)
C                         with the Jacobian J(k) and the iterate
C                         vector X(k) at the k-th iterate.
C       DXQA(NM)          Previous simplified Newton correction
C                         J(k-2)*X(k-1).
C       HH(NM1)           Elements (J-1)*N+1 to J*N are holding
C                         the values
C                         Y(T(J+1),X((J-1)*N+1,...,J*N))-X(J*N+1,
C                         ...,(J+1)*N)
C                         ( with the trajectory Y in
C                         [T(J),T(J+1)] , J = 1,...,M-1 ).
C       HHA(NM1)          Holds the previous value of HH .
C       R(N)              Value of the boundary condition function
C                         BC for the current iterate.
C       RA(N)             Previous values of R .
C       RF(M1)            Used by BGSOLI . Gets the norms of the
C                         Wronskian matrices.
C       T1(N)             Workspace used for miscellaneous
C                         purposes temporarely.
C       T2(N)             Workspace used for miscellaneous
C                         purposes temporarely.
C       U(NM)             Gets the right hand side of the linear
C                         system to be solved in each iteration
C                         step. Used in BGSOLI .
C       WO(NM)            Workspace needed for sparse solver. Must
C                         not be altered outside the sparse packet
C                         routines.
C       XA(NM)            Previous Newton iterate.
C       XU(NM1)           Elements (J-1)*N+1 to J*N are holding
C                         the values Y(T(J+1),X((J-1)*N+1,...,J*N))
C                         of the trajectory in the interval
C                         [T(J),T(J+1)] , (for J = 1,...,M-1 ).
C       XW(NM)            Scaling factors for iteration vector.
C       X1(N)             Components of the iteration vector
C                         corresponding to the left boundary
C                         A = T(1).
C       XM(N)             Components of the iteration vector
C                         corresponding to the right boundary
C                         B = T(M).
C
C     Internal integer arrays (workspace)
C     ===================================
C
C       IROW(N)           Row permutations of boundary derivative
C                         matrices A and B .
C       ICOLA(N)          Column permutations of matrix A
C                         (left boundary).
C       ICOLB(N)          Column permutations of matrix B
C                         (right boundary).
C       IA(N,N)           Reflects the sparse structure of matrix
C                         A by values 0, 1.
C       IB(N,N)           Reflects the sparse structure of matrix
C                         B by values 0, 1.
C       IW(NMX8)          Workspace needed for sparse solver
C                         package.
C
C     Internal short integer arrays (workspace)
C     =========================================
C
C       IRN(LIRN)         Workspace for MA28/MA30 sparse package.
C                         On Input to routine MA28A, it must hold
C                         the row indices of the sparse matrix.
C       ICN(LICN)         Workspace for MA28/MA30 sparse package.
C                         On Input to routine MA28A, it must hold
C                         the column indices of the sparse matrix.
C       IVECT(NZ)         Input to routine MA28B: must hold the
C                         row indices of the sparse matrix.
C       JVECT(NZ)         Input to routine MA28B: must hold the
C                         column indices of the sparse matrix.
C       IKEEP(NKEEP)      Workspace array for MA28 sparse package.
C                         To be preserved across the calls of the
C                         routines MA28A,MA28B,MA28C .
C
C     Internal real variables:
C     ========================
C
C       COND              Gets the condition of the Jacobian
C                         matrix computed by BGSOLI .
C       CORR              Gets the 1-norm of the residual DU .
C                         Computed by BGSOLI .
C       CONV              Scaled maximum norm of DXQ computed by
C                         subroutine BGLVLS . Used for convergence
C                         test.
C       CONVA             Holds the previous value of CONV .
C       EPSMIN            Smallest reasonable permitted accuracy
C                         EPS that can be prescribed by the user.
C       FC                Actual Gauss Newton iteration damping
C                         factor.
C       FCA               Previous Gauss Newton iteration damping
C                         factor.
C       FCDNM             Used to compute the denominator of the
C                         damping factor FC during computation of
C                         it's predictor, corrector and
C                         aposteriori estimate (in the case of
C                         performing a Rank1 update) .
C       FCH               Temporarely used for storing the new FC
C                         when computing aposteriori estimate.
C       FCMIN             Minimum permitted relaxation factor. If
C                         FC becomes smaller than this value, one
C                         of the following may occur:
C                         a.    Recomputation of the sensitivity
C                               matrix by means of difference
C                               approximation (instead of Rank1
C                               update), if Rank1 - update
C                               previously was used
C                         b.    Rank reduction of sensitivity
C                               matrix E ,  if difference
C                               approximation was used previously
C                               and Rank(E).NE.0
C                         c.    Fail exit otherwise
C       FCMIN2            FCMIN**2 . Used for FC-predictor
C                         computation.
C       FCNUM             Gets the numerator of the aposteriori
C                         estimate of FC .
C       FCNUMP            Gets the numerator of the predictor
C                         computation of FC .
C       FCNUMK            Gets the numerator of the corrector
C                         computation of FC .
C       H                 Actual integrator stepsize.
C       HMAX              Maximum permitted integrator stepsize.
C                         Set to the length of the integration
C                         interval, e.g. the distance of the
C                         effected Shooting points.
C       HSAVE             Stepsize saved across the call of the
C                         integrator.
C       HSTART            Start stepsize for integration used by
C                         subroutines BLFCNI and BLDERG .
C       MUE               Temporary value used during computation
C                         of damping factors predictor.
C       REDH              Multi purpose reduction factor. (???)
C       RELDIF            Relative deviation for numerical
C                         differentation.
C       SIGMA             Decision parameter for Jacobian Rank1
C                         updates (SIGMA.GT.1) . Rank1 updates are
C                         inhibited, if SIGMA.GT.1/FCMIN is set.
C       SKAP              Used to compute and print out the
C                         incompatibility factor of the nonlinear
C                         boundary value (e.g. least squares)
C                         problem.
C       SUMF              Standard level of the current iterate,
C                         e.g. Norm2(F(X))**2
C                         with the nonlinear model function F on
C                         which Newton iteration is performed,
C                         arising from the Multiple Shooting
C                         approach.
C       SUMX              Natural level of the current iterate,
C                         e.g. Norm2(DX)
C                         with the Newton correcture DX
C                         (see above).
C       SUMXA             Natural level of the previous iterate.
C       TFAIL             Used to get and print out in case of an
C                         integrator failure the last reached T
C                         value as a proposal for insertion of a
C                         new Shooting point.
C       TOL               Prescribed relative precision for
C                         numerical integration.
C       TOLH              Temporary used for computation of TOL
C                         (may be obmitted|).
C       TOLMIN            Lower bound value for TOL .
C       XTHR              Threshold for scaling.
C       TJ                Used by BLFCNI to hold T(J).
C       TJ1               Used by BLFCNI to hold T(J+1).
C       EPH               Gets TOL*SIGDEL by BGSOLI . If EPH.GT.
C                         REDH ,  termination occurs, since
C                         Multiple Shooting condition is too bad.
C       SIGDEL            Used by BGSOLI to compute the required
C                         integrator accuracy from the multiple
C                         shooting condition.
C       SIGDLH            Used by BGSOLI temporary during
C                         determination of SIGDEL .
C
C     Internal integer variables
C     ==========================
C
C       IAF               Indicates, if sparse structure of
C                         Jacobian matrix must be reordered by
C                         sparse solver or not:
C                         0     Not necessary, can call MA28B to
C                               decompose Jacobian
C                         1     Must be done, so use MA28A .
C       IC                Permutated index. Used by BGSOLI .
C       ICA               Temporarely used during search for
C                         separable linear boundary conditions at
C                         T(1).
C       ICB               Temporarely used during search for
C                         separable linear boundary conditions at
C                         T(M).
C       IH                Temporarely used during search for
C                         separable linear boundary conditions at
C                         T(1)and T(M).
C       INZ               Count of nonzero Jacobian matrix
C                         elements.
C       IR                Temporary storage for a row permutation
C                         index.
C       IRANK             Rank of Jacobian matrix E of current
C                         iteration step estimated by sparse
C                         solver package (Common block variable).
C       IS                Additional DO loop index.
C       ISUM              Used for determination of sparse
C                         structure of matrices A and B as
C                         nonzeros location counter.
C       ITER              Iteration count.
C       JJ                Used as "reverse DO loop" index:
C                         JJ = IUPB-J in a loop like DO J = 1,IUPB
C                         ...
C       JRED              Damping factor reduction count during an
C                         iterate.
C       KC                Temporary storage for a column
C                         permutation index.
C       KFLAG             Gets the subintervall number of the
C                         failure from subroutine BLDERG ,  if the
C                         integrator failed.
C       KOUNT             Trajectory evaluations count.
C       KPRINT            Print parameter - copy of input
C                         parameter INFO .
C       LEVEL             Flow control parameter needed by
C                         subroutine BGSOLI :
C                         0     indicates computation of Newton
C                               correcture,
C                         1     indicates computation of
C                               simplified Newton correcture
C                               (after computation of the
C                               preliminary new iterate)
C       NA                Number of separable boundary conditions
C                         at T(1): N-NAQ
C       NAQ               Number of not separable boundary
C                         conditions at T(1)
C       NB                Number of separable boundary conditions
C                         at T(M)
C       NBQ               Number of not separable boundary
C                         conditions at T(M): N-NB
C       NEW               Count of subsequent performed Rank1
C                         (Broyden) updates.
C       NM                Number of rows and columns of the
C                         Jacobian matrix part to be decomposed by
C                         the sparse solver.
C       NRS               N-(NA+NB)
C:    End Parameter
C:    EPMACH = relative machine precision
      DOUBLE PRECISION EPMACH,SMALL
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=0.5D0)
      DOUBLE PRECISION REDH
      PARAMETER (REDH=1.0D-2)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.0D0)
      DOUBLE PRECISION TWO
      PARAMETER (TWO=2.0D0)
      DOUBLE PRECISION EIGHT
      PARAMETER (EIGHT=8.0D0)
      DOUBLE PRECISION TEN
      PARAMETER (TEN=1.0D1)
      DOUBLE PRECISION FCMIN
      PARAMETER (FCMIN=1.0D-2)
      DOUBLE PRECISION FCMIN2
      PARAMETER (FCMIN2=1.0D-4)
      DOUBLE PRECISION FCNLIN
      PARAMETER (FCNLIN=1.0D-2)
      DOUBLE PRECISION SIGMA
      PARAMETER (SIGMA=2.0D0)
      INTEGER I,ICA,ICB,ICNCP,IH,IR,IRNCP,IRANK,IS,ISUM,ITER,
     *J,JRED,J0,J1,K,KFLAG,KOUNT,KPRINT,
     *L,LEVEL,MINIRN,MINICN,NB,NDIM,NAQ,NEW
      DOUBLE PRECISION COND,CORR,CONV,CONVA,EPH,EPSMIN,
     *EPX1H,EPSQ,FC,FCA,FCDNM,FCH,FCNMP2,FCNUM,FCNUMK,FCNUMP,
     *HSTART,MUE,RESID,RMIN,S,SIGDEL,SIGDLH,SUMF,
     *SUMX,SUMXA,TFAIL,TH,TOLH,TOLMIN,UQ
      LOGICAL ABORT1,ABORT2,DIFAPP,FCOMPT,GROW,IBLOCK,IVFAIL,
     *JACRFR,JACRST,NEXT
      COMMON /MA28ED/ LP,MP,IBLOCK,GROW
      COMMON /MA28FD/ EPSQ,RMIN,RESID,IRNCP,ICNCP,MINIRN,MINICN,
     *IRANK,ABORT1,ABORT2
      INTEGER L1,L2
      DOUBLE PRECISION S1
C:    Begin
C:    Begin of Segment BVPSOG.Body
C       ----------------------------------------------------------
C       1 Initialization
C       ----------------------------------------------------------
C       1.1 Internal parameters
C       Standard values fixed below
C       Minimum relative precision of integrator ( to be adapted )
        CALL ZIBCONST(EPMACH,SMALL)
        TOLMIN = EPMACH*TEN*TEN
C       Maximum permitted number of iterative refinements sweeps
C       ----------------------------------------------------------
C       1.1.1 Common parameters
C       Starting value of relaxation factor (FCMIN.LE.FC.LE.1.0)
        IF(NONLIN.LE.1)THEN
C         for linear or mildly nonlinear problems
          FC = ONE
        ELSE
C         for highly nonlinear problems
          FC = FCNLIN
        ENDIF
C       Minimum reasonable value for EPS
        EPSMIN = DSQRT(TEN*EPMACH)
        IF(EPS.LT.EPSMIN) EPS = EPSMIN
C       ----------------------------------------------------------
C       1.2 Initial preparations
        IF(FC.LT.FCMIN) FC = FCMIN
        IF(FC.GT.ONE) FC = ONE
        KPRINT = INFO
        ITER = 0
        KOUNT = 0
        INFO = -1000
        FCA = FC
        CONV = ZERO
        JACRFR = .FALSE.
        JACRST = .FALSE.
C:      Begin SetVec.Vec
        DO 6 L1=1,NM
          XA(L1)=X(L1)
6       CONTINUE
C.      End SetVec.Vec
        IF(TOL.LE.ZERO) TOL = EPS/TEN
        IF(TOL.LT.TOLMIN) TOL = TOLMIN
        DIFAPP = .TRUE.
        HSTART =(T(2)-T(1))*REDH
        LP = 0
        MP = 0
        IBLOCK = .FALSE.
        EPSQ = 0.1D0
        UQ = 0.1D0
        SIGDLH = ZERO
        SUMF = ONE
C:      Mat IA = Scalar (Rows 1,N ; Cols 1,N)
        L1 = 0
        DO 7 L2=1,N
        DO 7 L43=1,N
          IA(L2,L43)=L1
7       CONTINUE
C.      End SetIntMat.S
C:      Mat IB = Scalar (Rows 1,N ; Cols 1,N)
        L1 = 0
        DO 8 L2=1,N
        DO 8 L43=1,N
          IB(L2,L43)=L1
8       CONTINUE
C.      End SetIntMat.S
C:      CubeMat G (layer 1)= Scalar (Rows 1,N ; Cols 1,N)
        S1 = ZERO
        DO 9 L1=1,N
        DO 9 L2=1,N
          G(L1,L2,1)=S1
9       CONTINUE
C.      End SetCubeMat.S
        IF(KPRINT.GE.0)THEN
C         Print Start vector data, predescribed precision and max
C         iteration steps
10        FORMAT('Initial ','data',//)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,10)
          DO 11 J=1,M
12          FORMAT(D13.5,2X)
            WRITE(LUMON,12)T(J)
13          FORMAT((14X,3(D20.10,1X)))
            WRITE(LUMON,13)(X(L1),L1=(J-1)*N+1,J*N)
11        CONTINUE
14        FORMAT('N ','=',I2,2X,'M ','=',I2,/,
     *    'Prescribed ','relative ','precision',D10.2,2X,/,
     *    'Maximum ','permitted ','number ','of ','iteration ',
     *    'steps',1X,I3,//,'1')
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,14)N,M,EPS,ITMAX
          IF(KPRINT.EQ.0)THEN
15          FORMAT(1X,66('*'))
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,15)
16          FORMAT(4X,'It',7X,'Levelf',10X,'Levelx',8X,
     *      'Rel.Fc.')
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,16)
          ENDIF
        ENDIF
C       ----------------------------------------------------------
C       1.3 Startup step
C       ----------------------------------------------------------
C       1.3.1 Computation of the residual vector
        CALL BLFCNI(IVPSOL,FCN,BC,N,M,NM,NM1,ITER,KPRINT,HSTART,
     *  FCMIN,T,X,X1,XM,T1,XU,HH,R,TOL,FC,FCOMPT,IVFAIL,KFLAG,
     *  KOUNT,INFO,LUMON)
C
C       Main iteration loop
C       ===================
C
C:      While (expression)
17      IF(INFO.EQ.-1000)THEN
C:          Begin of Segment BVPSOG.Core
C             ----------------------------------------------------
C             2 Startup of iteration step
              IF(.NOT.(JACRFR.OR.JACRST))THEN
                LEVEL = 0
C               --------------------------------------------------
C               2.1 Scaling of variables X(NM)
                CALL BLSCLE(N,M,NM,NM1,X,XU,XW,XTHR)
                IF(ITER.NE.0)THEN
C:                Begin SetVec.Vec
                  DO 18 L1=1,NM
                    DXQA(L1)=DXQ(L1)
18                CONTINUE
C.                End SetVec.Vec
C:                FCNUM = Sum of Formula Elements (for 1,NM)
                  FCNUM = 0.0D0
                  DO 19 L1=1,NM
                    FCNUM=FCNUM+((DX(L1)/XW(L1))**2)
19                CONTINUE
C.                End MakeSum.Comp
C:                FCNMP2 = Sum of Formula Elements (for 1,NM)
                  FCNMP2 = 0.0D0
                  DO 20 L1=1,NM
                    FCNMP2=FCNMP2+((DXQ(L1)/XW(L1))**2)
20                CONTINUE
C.                End MakeSum.Comp
                  FCNUMP = FCNUM*FCNMP2
                ENDIF
                IF(ITER.NE.0)THEN
                  TH = FC-ONE
C:                FCDNM = Sum of Formula Elements (for 1,NM)
                  FCDNM = 0.0D0
                  DO 21 L1=1,NM
                    FCDNM=FCDNM+(((DXQ(L1)+TH*DX(L1))/XW(L1))**2)
21                CONTINUE
C.                End MakeSum.Comp
                  FCH = DSQRT(FCNUM/FCDNM)*FC*FC*HALF
C                 ------------------------------------------------
C                 2.1.1 Decision criterion for Jacobian updating
C                       technique:
C                       DIFAPP.EQ..TRUE. numerical
C                       differentiation,
C                       DIFAPP.EQ..FALSE. rank1 updating
                  DIFAPP = FC.LT.FCA.AND.NEW.GT.0.OR.FCH.LT.FC*
     *            SIGMA
                  FCA = FC
                  IF(NONLIN.GT.0) FC = DMIN1(FCH,ONE)
                ENDIF
C               --------------------------------------------------
C               2.2 Difference approximation of jacobian matrix A
C                   ( If Difapp.EQ..TRUE. ) or
C                   Rank-1 update of jacobian matrix A ( If Difapp
C                   .EQ..FALSE. )
                CALL BLDERA(BC,N,M,NM,XW,X1,XM,R,T2,A,B,RELDIF)
C               --------------------------------------------------
C               2.3 Determination of sparse structure of matrices
C                   A and B
                IAF = 0
                DO 22 I=1,N
                  S = ZERO
                  DO 23 K=1,N
                    TH = DABS(A(I,K))*XW(K)
                    S = S+TH
                    TH = DABS(B(I,K))*XW(K+NM1)
                    S = S+TH
23                CONTINUE
                  IF(S.LT.XTHR) S = XTHR
                  DE(I)=ONE/S
                  DO 24 K=1,N
                    IF(IA(I,K).LE.0)THEN
                      IF(A(I,K).NE.ZERO)THEN
                        IA(I,K)=1
                        IAF = 1
                      ENDIF
                    ENDIF
                    IF(IB(I,K).LE.0)THEN
                      IF(B(I,K).NE.ZERO)THEN
                        IB(I,K)=1
                        IAF = 1
                      ENDIF
                    ENDIF
24                CONTINUE
22              CONTINUE
                IF(IAF.NE.0)THEN
C                 ------------------------------------------------
C                 2.3.1 Determination of row and column
C                       permutation vectors
                  DO 25 I=1,N
                    ICOLA(I)=I
                    ICOLB(I)=I
                    IROW(I)=I
25                CONTINUE
C                 ------------------------------------------------
C                 2.3.2 Search for separable linear boundary
C                       conditions at T(1)
                  NAQ = N
                  DO 26 I=1,N
                      DO 27 K=1,N
                        IF(IB(I,K).NE.0) GOTO 9996
27                    CONTINUE
                      ISUM = 0
                      DO 28 K=1,N
                        IF(IA(I,K).NE.0)THEN
                          ISUM = ISUM+1
                          ICA = K
                        ENDIF
28                    CONTINUE
                      IF(ISUM.LE.1)THEN
                        DO 29 IS=1,N
                          IH = ICOLA(IS)
                          IF(IH.EQ.ICA) ICOLA(IS)=ICOLA(NAQ)
                          IH = IROW(IS)
                          IF(IH.EQ.I) IROW(IS)=IROW(NAQ)
29                      CONTINUE
                        ICOLA(NAQ)=ICA
                        IROW(NAQ)=I
                        NAQ = NAQ-1
                        IF(DABS(R(I)).GT.TEN*EPMACH*DABS(X(ICA)))
     *                  THEN
                          INFO = -5
                          GOTO 9998
                        ENDIF
                      ENDIF
9996                CONTINUE
26                CONTINUE
                  IF(KPRINT.GE.0.AND.NAQ.EQ.0)THEN
30                  FORMAT('Warning: ','attempt ','to ',
     *              'solve ','initial ','value ','problem')
                    write ( lumon, '(a)' ) ' '
                    WRITE(LUMON,30)
                  ENDIF
                  NA = N-NAQ
C                 ------------------------------------------------
C                 2.3.3 Search for separable linear boundary
C                       conditions at T(M)
                  NB = 0
                ENDIF
                IF(IAF.NE.0.AND.NAQ.NE.0)THEN
                  DO 31 I=1,NAQ
                      IR = IROW(I)
                      DO 32 K=1,N
                        IF(IA(IR,K).NE.0) GOTO 9995
32                    CONTINUE
                      ISUM = 0
                      DO 33 K=1,N
                        IF(IB(IR,K).NE.0)THEN
                          ISUM = ISUM+1
                          ICB = K
                        ENDIF
33                    CONTINUE
                      IF(ISUM.LE.1)THEN
                        NB = NB+1
                        DO 34 IS=1,N
                          IH = ICOLB(IS)
                          IF(IH.EQ.ICB) ICOLB(IS)=ICOLB(NB)
34                      CONTINUE
                        ICOLB(NB)=ICB
                        IROW(I)=IROW(NB)
                        IROW(NB)=IR
                        IF(DABS(R(IR)).GT.TEN*EPMACH*DABS(X(ICB+
     *                  NM1)))THEN
                          INFO = -5
                          GOTO 9998
                        ENDIF
                      ENDIF
9995                CONTINUE
31                CONTINUE
                  IF(KPRINT.GE.0.AND.NB.EQ.N)THEN
35                  FORMAT('Warning: ','attempt ','to ',
     *              'solve ','initial ','value ','problem')
                    write ( lumon, '(a)' ) ' '
                    WRITE(LUMON,35)
                  ENDIF
                  NBQ = N-NB
C                 ------------------------------------------------
C                 2.3.4 Count non-zeroes in jacobian and store
C                       their locations
                  NDIM = NM-NA-NB
                  NRS = N-NA-NB
                  INZ = 0
                  DO 36 I=1,N
                      IF(NAQ.NE.0)THEN
                        DO 37 K=1,NAQ
                          INZ = INZ+1
                          IVECT(INZ)=I
                          JVECT(INZ)=K
37                      CONTINUE
                      ENDIF
                      II = I
                      IF(M.EQ.2)THEN
                        II = 0
                        IF(NBQ.EQ.0) GOTO 9994
                        DO 38 L=1,NBQ
                          IF(ICOLB(NB+L).EQ.I) II = L
38                      CONTINUE
                        IF(II.EQ.0) GOTO 9994
                      ENDIF
                      INZ = INZ+1
                      IVECT(INZ)=I
                      JVECT(INZ)=NAQ+II
9994                CONTINUE
36                CONTINUE
                  IF(M.NE.2)THEN
                    DO 39 J=2,M1
                      DO 40 I=1,N
                          DO 41 K=1,N
                            INZ = INZ+1
                            IVECT(INZ)=(J-1)*N+I
                            JVECT(INZ)=(J-1)*N+K-NA
41                        CONTINUE
                          II = I
                          IF(J.EQ.M1)THEN
                            II = 0
                            IF(NBQ.EQ.0) GOTO 9993
                            DO 42 L=1,NBQ
                              IF(ICOLB(NB+L).EQ.I) II = L
42                          CONTINUE
                            IF(II.EQ.0) GOTO 9993
                          ENDIF
                          INZ = INZ+1
                          IVECT(INZ)=(J-1)*N+I
                          JVECT(INZ)=J*N+II-NA
9993                    CONTINUE
40                    CONTINUE
39                  CONTINUE
                  ENDIF
                  IF(NRS.NE.0)THEN
                    DO 43 I=1,NRS
                      II = I+NB
                      IF(NAQ.NE.0)THEN
                        DO 44 K=1,NAQ
                          IF(IA(IROW(II),ICOLA(K)).NE.0)THEN
                            INZ = INZ+1
                            IVECT(INZ)=NM1+I
                            JVECT(INZ)=K
                          ENDIF
44                      CONTINUE
                      ENDIF
                      IF(NBQ.NE.0)THEN
                        DO 45 KK=1,NBQ
                          K = KK+NB
                          IF(IB(IROW(II),ICOLB(K)).NE.0)THEN
                            INZ = INZ+1
                            IVECT(INZ)=NM1+I
                            JVECT(INZ)=NM1+KK-NA
                          ENDIF
45                      CONTINUE
                      ENDIF
43                  CONTINUE
                  ENDIF
                ENDIF
              ENDIF
              JACRFR = .FALSE.
              IF(.NOT.JACRST)THEN
                IF(DIFAPP)THEN
                  NEW = 0
                  KFLAG = 0
                  CALL BLDERG(FCN,N,NAQ,M,M1,NM,NM1,T,X,XU,XW,T2,
     *            TFAIL,G,ICOLA,IVPSOL,HSTART,TOL,RELDIF,KFLAG)
                  IF(KFLAG.LT.0)THEN
                    INFO = -3
                    GOTO 9998
                  ENDIF
                  IF(M.GT.2) KOUNT = KOUNT+N
                  IF(M.EQ.2) KOUNT = KOUNT+NAQ
                ELSE
                  NEW = NEW+1
                  CALL BLRK1G(N,M,M1,NM,NM1,XW,DX,HH,HHA,T1,G,FCA)
                ENDIF
              ENDIF
              JACRST = .FALSE.
C             ----------------------------------------------------
C             2.3.5 Storing of total sparse jacobian
C                   ( including row and column scaling )
              INZ = 0
              DO 46 I=1,N
                IF(NAQ.NE.0)THEN
                  DO 47 K=1,NAQ
                    INZ = INZ+1
                    E(INZ)=-G(I,ICOLA(K),1)*XW(ICOLA(K))/XW(I+N)
47                CONTINUE
                ENDIF
                II = I
                  IF(M.EQ.2)THEN
                    II = 0
                    IF(NBQ.EQ.0) GOTO 9992
                    DO 48 L=1,NBQ
                      IF(ICOLB(NB+L).EQ.I) II = L
48                  CONTINUE
                    IF(II.EQ.0) GOTO 9992
                  ENDIF
                  INZ = INZ+1
                  E(INZ)=ONE
9992            CONTINUE
46            CONTINUE
              IF(M.NE.2)THEN
                DO 49 J=2,M1
                  J0 =(J-1)*N
                  J1 = J0+N
                  DO 50 I=1,N
                    DO 51 K=1,N
                      INZ = INZ+1
                      E(INZ)=-G(I,K,J)*XW(K+J0)/XW(I+J1)
51                  CONTINUE
                    II = I
                      IF(J.EQ.M1)THEN
                        II = 0
                        IF(NBQ.EQ.0) GOTO 9991
                        DO 52 L=1,NBQ
                          IF(ICOLB(NB+L).EQ.I) II = L
52                      CONTINUE
                        IF(II.EQ.0) GOTO 9991
                      ENDIF
                      INZ = INZ+1
                      E(INZ)=ONE
9991                CONTINUE
50                CONTINUE
49              CONTINUE
              ENDIF
              IF(NRS.NE.0)THEN
                DO 53 I=1,NRS
                  II = I+NB
                  IF(NAQ.NE.0)THEN
                    DO 54 K=1,NAQ
                      IF(IA(IROW(II),ICOLA(K)).NE.0)THEN
                        INZ = INZ+1
                        E(INZ)=-A(IROW(II),ICOLA(K))*DE(IROW(II))*
     *                  XW(ICOLA(K))
                      ENDIF
54                  CONTINUE
                  ENDIF
                  IF(NBQ.NE.0)THEN
                    DO 55 KK=1,NBQ
                      K = KK+NB
                      IF(IB(IROW(II),ICOLB(K)).NE.0)THEN
                        INZ = INZ+1
                        E(INZ)=-B(IROW(II),ICOLB(K))*DE(IROW(II))*
     *                  XW(ICOLB(K)+NM1)
                      ENDIF
55                  CONTINUE
                  ENDIF
53              CONTINUE
              ENDIF
C             ----------------------------------------------------
C             2.4 Save values of R(N)and HH((M-1)*N)
C:            Begin SetVec.Vec
              DO 56 L1=1,N
                RA(L1)=R(L1)
56            CONTINUE
C.            End SetVec.Vec
C:            Begin SetVec.Vec
              DO 57 L1=1,NM1
                HHA(L1)=HH(L1)
57            CONTINUE
C.            End SetVec.Vec
              NEXT = .FALSE.
C             ----------------------------------------------------
C             3 Main-part of iteration step
C             ----------------------------------------------------
C             3.1 Solution of the linear system
C             ----------------------------------------------------
C             3.1.1 Decomposition of (N,N)-matrix A
C             ----------------------------------------------------
C             3.1.2 LU-decomposition of(NDIM,NDIM)-MATRIX E
              IF(IAF.EQ.1)THEN
                DO 58 I=1,INZ
                  IRN(I)=IVECT(I)
                  ICN(I)=JVECT(I)
58              CONTINUE
                CALL MA28AD(NDIM,INZ,E,LICN,IRN,LIRN,ICN,UQ,IKEEP,
     *          IW,WO,IFLAG)
                IAF = 0
              ELSE
                CALL MA28BD(NDIM,INZ,E,LICN,IVECT,JVECT,ICN,IKEEP,
     *          IW,WO,IFLAG)
                IF(RMIN.LT.1.0D-4) IAF = 1
                IF(IAF.EQ.1)THEN
                  JACRST = .TRUE.
                  GOTO 9998
                ENDIF
              ENDIF
              IF(IFLAG.EQ.-1.OR.IFLAG.EQ.-2)THEN
                INFO = -1
                GOTO 9998
              ENDIF
              IF(IFLAG.LE.-3)THEN
                INFO = -9
                GOTO 9998
              ENDIF
C             ----------------------------------------------------
C             3.1.3 Solution of linear (N,N)-system
              CALL BGSOLI(N,M,M1,NM,NM1,NDIM,LICN,NKEEP,NA,NAQ,NB,
     *        NBQ,NRS,ITER,LEVEL,KPRINT,EPS,REDH,TOLMIN,FC,FCA,TOL,
     *        RELDIF,EPH,EPX1H,SIGDEL,SIGDLH,COND,CORR,HH,DHH,R,A,
     *        B,G,U,DE,DU,T1,DXQ,XW,DR,RF,WO,E,IROW,ICOLA,ICOLB,
     *        ICN,IKEEP,INFO,LUMON)
              IF(INFO.NE.-1000) GOTO 9998
C             ----------------------------------------------------
C             3.2 Evaluation of scaled natural level function SUMX
C                 scaled maximum error norm CONV
C                 evaluation of (scaled) standard level function
C                 SUMF ( SUMF only, if KPRINT.GE.0 )
C                 and computation of ordinary newton corrections
C                 DX(N)
              CALL BGLVLS(N,M,NM,NM1,XW,DXQ,HH,R,DE,CONV,SUMX,SUMF,
     *        KPRINT)
C:            Begin SetVec.Vec
              DO 59 L1=1,NM
                DX(L1)=DXQ(L1)
59            CONTINUE
C.            End SetVec.Vec
C:            Begin SetVec.Vec
              DO 60 L1=1,NM
                XA(L1)=X(L1)
60            CONTINUE
C.            End SetVec.Vec
              SUMXA = SUMX
              CONVA = CONV
C             ----------------------------------------------------
C             3.3 a - priori estimate of relaxation factor FC
              JRED = 0
              IF(ITER.NE.0.AND.NONLIN.NE.0)THEN
                IF(NEW.EQ.0)THEN
C                 ------------------------------------------------
C                 3.3.1 Computation of the denominator of a-priori
C                       estimate
C:                FCDNM = Sum of Formula Elements (for 1,NM)
                  FCDNM = 0.0D0
                  DO 61 L1=1,NM
                    FCDNM=FCDNM+(((DX(L1)-DXQA(L1))/XW(L1))**2)
61                CONTINUE
C.                End MakeSum.Comp
C                 ------------------------------------------------
C                 3.3.2 New relaxation factor
                  FCDNM = FCDNM*SUMX
                  IF(FCDNM.GE.FCNUMP*FCMIN2)THEN
                    MUE = FCA*DSQRT(FCNUMP/FCDNM)
                    FC = DMIN1(MUE,ONE)
                  ELSE
                    FC = ONE
                  ENDIF
                ENDIF
                IF(FC.LT.FCMIN)THEN
                  INFO = -4
                  GOTO 9997
                ENDIF
              ENDIF
              LEVEL = 1
C             ----------------------------------------------------
C             3.4 Save natural level for later computations of
C                 corrector and print iterate
              FCNUMK = SUMX
              IF(KPRINT.GE.0)THEN
C               Print Standard - and natural level
                IF(KPRINT.GT.0)THEN
62                FORMAT(1X,66('*'))
                  write ( lumon, '(a)' ) ' '
                  WRITE(LUMON,62)
63                FORMAT(4X,'It',7X,'Levelf',10X,'Levelx',18X,
     *            'New')
                  write ( lumon, '(a)' ) ' '
                  WRITE(LUMON,63)
                ENDIF
64              FORMAT(4X,I2,5X,D10.3,2X,4X,D10.3,2X,13X,I2)
                write ( lumon, '(a)' ) ' '
                WRITE(LUMON,64)ITER,SUMF,SUMXA,NEW
                IF(KPRINT.GT.0)THEN
65                FORMAT(1X,66('*'))
                  write ( lumon, '(a)' ) ' '
                  WRITE(LUMON,65)
                ENDIF
              ENDIF
C
C             Relaxation-factor reduction loop
C             ================================
C
C:            DO (Until)
66            CONTINUE
C               --------------------------------------------------
C               3.5 Preliminary new iterate
C:              DO (Until)
67              CONTINUE
                  FCOMPT = .FALSE.
C:                Vec X = Vec XA + Vec DX * Scalar (for 1,NM)
                  S1 = FC
                  DO 68 L1=1,NM
                    X(L1)=XA(L1)+DX(L1)*S1
68                CONTINUE
C.                End SetVec.Vec&VecxS
                  IF(ITER.GT.ITMAX)THEN
                    INFO = -2
                    GOTO 9997
                  ENDIF
C                 ------------------------------------------------
C                 3.5.1 Computation of the residual vector
                  CALL BLFCNI(IVPSOL,FCN,BC,N,M,NM,NM1,ITER,KPRINT,
     *            HSTART,FCMIN,T,X,X1,XM,T1,XU,HH,R,TOL,FC,FCOMPT,
     *            IVFAIL,KFLAG,KOUNT,INFO,LUMON)
                  IF(IVFAIL)THEN
                    INFO = -4
                    GOTO 9997
                  ENDIF
                  IF(INFO.NE.-1000) GOTO 9997
                IF(.NOT.(FCOMPT)) GOTO  67
C.              UNTIL ( expression - negated above)
C               --------------------------------------------------
C               3.5.2 Solution of linear (N,N)-system
                CALL BGSOLI(N,M,M1,NM,NM1,NDIM,LICN,NKEEP,NA,NAQ,
     *          NB,NBQ,NRS,ITER,LEVEL,KPRINT,EPS,REDH,TOLMIN,FC,
     *          FCA,TOL,RELDIF,EPH,EPX1H,SIGDEL,SIGDLH,COND,CORR,
     *          HH,DHH,R,A,B,G,U,DE,DU,T1,DXQ,XW,DR,RF,WO,E,IROW,
     *          ICOLA,ICOLB,ICN,IKEEP,INFO,LUMON)
                IF(INFO.NE.-1000) GOTO 9998
C               --------------------------------------------------
C               3.5.3 Evaluation of scaled natural level function
C                     SUMX
C                     scaled maximum error norm CONV and
C                     evaluation of (scaled) standard level
C                     function SUMF
                CALL BGLVLS(N,M,NM,NM1,XW,DXQ,HH,R,DE,CONV,SUMX,
     *          SUMF,KPRINT)
C               --------------------------------------------------
C               3.6 Convergence test
                IF(CONV.LE.EPS)THEN
                  INFO = 0
                  GOTO 9997
                ENDIF
C               --------------------------------------------------
C               3.7 Natural monotonicity test
                IF(SUMX.GT.SUMXA)THEN
C                 ------------------------------------------------
C                 3.8 Output of iterate
                  IF(KPRINT.GE.0)THEN
C                   Print Standard - and natural level, and
C                   damping factor
                    IF(KPRINT.GT.0)THEN
69                    FORMAT(1X,66('*'))
                      write ( lumon, '(a)' ) ' '
                      WRITE(LUMON,69)
70                    FORMAT(4X,'It',7X,'Levelf',10X,'Levelx',
     *                8X,'Rel.Fc.')
                      write ( lumon, '(a)' ) ' '
                      WRITE(LUMON,70)
                    ENDIF
71                  FORMAT(4X,I2,5X,D10.3,2X,4X,D10.3,2X,4X,F5.3)
                    write ( lumon, '(a)' ) ' '
                    WRITE(LUMON,71)ITER,SUMF,SUMX,FC
                    IF(KPRINT.GT.0)THEN
72                    FORMAT(1X,66('*'))
                      write ( lumon, '(a)' ) ' '
                      WRITE(LUMON,72)
                    ENDIF
                  ENDIF
                  JRED = JRED+1
                  IF(NONLIN.EQ.0)THEN
                    INFO = -4
                    GOTO 9997
                  ENDIF
C                 ------------------------------------------------
C                 3.9 Compute reduced relaxation factor
                  TH = FC-ONE
C:                FCDNM = Sum of Formula Elements (for 1,NM)
                  FCDNM = 0.0D0
                  DO 73 L1=1,NM
                    FCDNM=FCDNM+(((DXQ(L1)+TH*DX(L1))/XW(L1))**2)
73                CONTINUE
C.                End MakeSum.Comp
                  FC = DSQRT(FCNUMK/FCDNM)*FC*FC*HALF
C                 ------------------------------------------------
C                 3.10 Fail exit, if relaxation factor to small
                  JACRFR = FC.LT.FCMIN.OR.NEW.GT.0.AND.JRED.GT.1
                  IF(JACRFR.AND.NEW.EQ.0)THEN
                    INFO = -4
                    GOTO 9998
                  ENDIF
                ENDIF
              IF(.NOT.(SUMX.LE.SUMXA.OR.JACRFR)) GOTO  66
C.            UNTIL ( expression - negated above)
C
C             End of relaxation-factor reduction loop
C             =======================================
C
              IF(JACRFR)THEN
C               --------------------------------------------------
C               3.11 Restore former values for repeting iteration
C                    step
C               Restore former values
                LEVEL = 0
C:              Begin SetVec.Vec
                DO 74 L1=1,N
                  R(L1)=RA(L1)
74              CONTINUE
C.              End SetVec.Vec
C:              Begin SetVec.Vec
                DO 75 L1=1,N
                  X1(L1)=XA(L1)
75              CONTINUE
C.              End SetVec.Vec
C:              Begin SetVec.Vec
                DO 76 L1=1,N
                  XM(L1)=XA(L1+NM1)
76              CONTINUE
C.              End SetVec.Vec
C:              Begin SetVec.Vec
                DO 77 L1=1,NM
                  X(L1)=XA(L1)
77              CONTINUE
C.              End SetVec.Vec
C:              Begin SetVec.Vec&Vec
                DO 78 L1=1,NM1
                  XU(L1)=X(L1+N)+HHA(L1)
78              CONTINUE
C.              End SetVec.Vec&Vec
C:              Begin SetVec.Vec
                DO 79 L1=1,NM1
                  HH(L1)=HHA(L1)
79              CONTINUE
C.              End SetVec.Vec
                IF(KPRINT.GE.0)THEN
80                FORMAT(5X,I2,1X,'Not ','accepted ',
     *            'relaxation ','factor',5X,F5.3)
                  write ( lumon, '(a)' ) ' '
                  WRITE(LUMON,80)ITER,FC
                ENDIF
                IF(ITER.EQ.0)THEN
                  FC = FCMIN
                ENDIF
                DIFAPP = .TRUE.
                JACRFR = .TRUE.
                GOTO 9998
              ENDIF
C             ----------------------------------------------------
C             4 Preparations to start the following iteration step
              ITER = ITER+1
              FCA = FC
C             ----------------------------------------------------
C             4.1 Print values
              IF(KPRINT.GE.0)THEN
C               Print Standard - and natural level, and damping
C               factor
                IF(KPRINT.GT.0)THEN
81                FORMAT(1X,66('*'))
                  write ( lumon, '(a)' ) ' '
                  WRITE(LUMON,81)
82                FORMAT(4X,'It',7X,'Levelf',10X,'Levelx',8X,
     *            'Rel.Fc.')
                  write ( lumon, '(a)' ) ' '
                  WRITE(LUMON,82)
                ENDIF
83              FORMAT(4X,I2,5X,D10.3,2X,4X,D10.3,2X,4X,F5.3)
                write ( lumon, '(a)' ) ' '
                WRITE(LUMON,83)ITER,SUMF,SUMX,FC
                IF(KPRINT.GT.0)THEN
84                FORMAT(1X,66('*'))
                  write ( lumon, '(a)' ) ' '
                  WRITE(LUMON,84)
                  DO 85 J=1,M
86                  FORMAT(D13.5,2X)
                    WRITE(LUMON,86)T(J)
87                  FORMAT((14X,3(D20.10,1X)))
                    WRITE(LUMON,87)(X(L1),L1=(J-1)*N+1,J*N)
85                CONTINUE
                ENDIF
              ENDIF
9997        CONTINUE
C.          End of Segment BVPSOG.Core
9998      CONTINUE
        GOTO 17
        ENDIF
C.      EndWhile
C
C       End of main iteration loop
C       ==========================
C
C       ----------------------------------------------------------
C       5 Exits
C       ----------------------------------------------------------
C       5.1 Solution exit
        IF(INFO.EQ.0)THEN
          ITER = ITER+1
C:        Vec X = Vec X + Vec DXQ (for 1,NM)
          DO 88 L1=1,NM
            X(L1)=X(L1)+DXQ(L1)
88        CONTINUE
C.        End SetVec.&Vec
          INFO = ITER
          IF(KPRINT.LT.0)THEN
            GOTO 9999
          ENDIF
          IF(KPRINT.GT.0)THEN
C           Print levels, damping factor of last iteration step
C           and solution info
89          FORMAT(1X,66('*'))
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,89)
90          FORMAT(4X,'It',7X,'Levelf',10X,'Levelx',8X,
     *      'Rel.Fc.')
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,90)
          ENDIF
91        FORMAT(4X,I2,5X,D10.3,2X,4X,D10.3,2X,4X,F5.3)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,91)ITER,SUMF,SUMX,FC
92        FORMAT(1X,66('*'))
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,92)
          write ( lumon, '(a)' ) ' '
          write ( lumon, '(a)' ) ' '
          write ( lumon, '(a)' ) ' '
C         Print solution info
94        FORMAT('Solution ','of',1X,'boundary ','value ',
     *    'problem',' obtained',/,'BVPSOL',' required',I3,1X,
     *    'Iteration ','steps ','with',I4,1X,'trajectory',
     *    ' evaluations',//)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,94)ITER,KOUNT
95        FORMAT('Achieved ','relative ','accuracy',D10.3,2X)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,95)CONV
          IF(EPH.GT.CONV) CONV = EPH
96        FORMAT('Reliable ','relative ','accuracy',D10.3,2X,/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,96)CONV
          S = DLOG(DBLE((M-1)*(2*N+M-1))*EPMACH)
          DO 97 J=1,M1
            S = S+DLOG(RF(J))
97        CONTINUE
          IF(S.LT.DLOG(EPS))THEN
98          FORMAT('This ','boundary ','value ','problem ',
     *      'can ','also ','be ','solved ','with IOPT(3)=0 set',/)
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,98)
          ENDIF
        ENDIF
C       ----------------------------------------------------------
C       5.2 Fail exit messages
C       ----------------------------------------------------------
C       5.2.1 Gaussian decomposition failed by singular jacobian
        IF(INFO.EQ.-1.AND.KPRINT.GE.0)THEN
99        FORMAT('Gaussian ','elimination ','failed ','by ',
     *    'singular ','Jacobian',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,99)
        ENDIF
C       ----------------------------------------------------------
C       5.2.2 Termination after more than itmax iterations
        IF(INFO.EQ.-2.AND.KPRINT.GE.0)THEN
100       FORMAT('Iteration ','terminates ','after ','itmax ',
     *    '=',I3,2X,'iteration ','steps')
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,100)ITMAX
        ENDIF
C       ----------------------------------------------------------
C       5.2.3 Integrator failed in trajectory computation
        IF(INFO.EQ.-3.AND.KPRINT.GE.0)THEN
101       FORMAT('Integrator ','failed ','in ','trajectory ',
     *    'computation ',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,101)
          J1 =-KFLAG
102       FORMAT('BVPSOL ','terminates',/,'Subinterval',I3,1X,
     *    'possibly ','insert ','new ','node',D20.11,2X,/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,102)J1,TFAIL
        ENDIF
C       ----------------------------------------------------------
C       5.2.4 Convergence fail of Gauss - Newton method
        IF(INFO.EQ.-4.AND.KPRINT.GE.0)THEN
103       FORMAT('Gauss ','Newton ','method ','fails ','to ',
     *    'converge',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,103)
        ENDIF
C       ----------------------------------------------------------
C       5.2.5 Inconsistent initial data
        IF(INFO.EQ.-5.AND.KPRINT.GE.0)THEN
104       FORMAT('Error: ','initial ','data ','and ',
     *    'boundary ','conditions ','are ','inconsistent',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,104)
        ENDIF
C       ----------------------------------------------------------
C       5.2.6 Multiple shooting condition too bad -insert new
C             nodes
        IF(INFO.EQ.-6.AND.KPRINT.GE.0)THEN
105       FORMAT('Termination ','since ','Multiple ',
     *    'Shooting ','condition',/,' ','or ','condition ','of ',
     *    'Jacobian ','is ','too ','bad',/,'insert ','new ',
     *    'nodes',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,105)
          S = REDH/TOL
          DO 106 J=1,M1
            IF(RF(J).GT.S)THEN
107           FORMAT(8X,'in ','subinterval',2X,I3,/)
              write ( lumon, '(a)' ) ' '
              WRITE(LUMON,107)J
            ENDIF
106       CONTINUE
        ENDIF
C       ----------------------------------------------------------
C       5.2.7 Insufficient error tolerance for integrator
        IF(INFO.EQ.-7.AND.KPRINT.GE.0)THEN
          TOLH = DMIN1(REDH/COND,EPS)/SIGDEL
          RELDIF = DSQRT(TOLH/SIGDEL)
108       FORMAT('Suggested ','integrator ','accuracy',D10.1
     *    ,2X,//,'Suggested ','relative ','deviation ',
     *    'parameter',D10.1,2X,/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,108)TOLH,RELDIF
109       FORMAT('Reduce ','relative ','error ','tolerance ',
     *    'for ','integrator ','to',D10.1,2X,/,2X,'or ','insert ',
     *    'new ','nodes',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,109)TOLH
          S = REDH/TOL
          DO 110 J=1,M1
            IF(RF(J).GT.S)THEN
111           FORMAT(8X,'in ','subinterval',2X,I3,/)
              write ( lumon, '(a)' ) ' '
              WRITE(LUMON,111)J
            ENDIF
110       CONTINUE
112       FORMAT('Reliable ','relative ','accuracy ',
     *    'greater ','than',1X,D6.1,2X,/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,112)1.0D-2
        ENDIF
C       ----------------------------------------------------------
C       5.2.8 Too small storage for sparse linear system solver
        IF(INFO.EQ.-9.AND.KPRINT.GE.0)THEN
113       FORMAT('Too ','small ','storage ','for ','linear ',
     *    'system ','solver',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,113)
        ENDIF
C       ----------------------------------------------------------
C       5.3 Common exit
        IF(KPRINT.GE.0)THEN
C
114       FORMAT('Condition ','of ','Jacobian',D10.3,2X,/,
     *    'Multiple ','shooting ','condition',D10.3,2X,/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,114)COND,SIGDLH
          write ( lumon, '(a)' ) ' '
          IF(INFO.GT.0)THEN
115         FORMAT('Solution ','data',/)
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,115)
          ENDIF
          IF(INFO.LT.0)THEN
116         FORMAT('Final ','data',/)
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,116)
          ENDIF
          DO 117 J=1,M
118         FORMAT(D13.5,2X)
            WRITE(LUMON,118)T(J)
119         FORMAT((14X,3(D20.10,1X)))
            WRITE(LUMON,119)(X(L1),L1=(J-1)*N+1,J*N)
117       CONTINUE
        ENDIF
C       End of exits
9999  CONTINUE

      RETURN
      END
      SUBROUTINE BLFCNI(IVPSOL,FCN,BC,N,M,NM,NM1,ITER,KPRINT,
     *HSTART,FCMIN,T,X,X1,XM,T1,XU,HH,R,TOL,FC,FCOMPT,REDUCT,KFLAG,
     *KOUNT,INFO,LUMON)

c*********************************************************************72
c
cc BLFCNI computes the residual vector.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)

      EXTERNAL FCN,IVPSOL,BC
      INTEGER N,M,NM,NM1,ITER,KPRINT
      DOUBLE PRECISION HSTART,FCMIN
      INTEGER KOUNT,KFLAG,INFO,LUMON
      LOGICAL REDUCT
      DOUBLE PRECISION TOL,FC
      LOGICAL FCOMPT
      DOUBLE PRECISION T(M),X(NM)
      DOUBLE PRECISION XU(NM1),HH(NM1),R(N),X1(N),XM(N),T1(N)
C:    End Parameter
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=0.5D0)
      INTEGER J,J1,KB,KB2
      DOUBLE PRECISION HMAX,HSAVE,TJ,TJ1,H
      INTEGER L1

C:    Begin of Segment FcnInt.Body
C       Computation of the trajectories (solution of M1 initial
C         value problems)
        REDUCT = .FALSE.
        KOUNT = KOUNT+1
        HSAVE = HSTART
        DO 117 J=1,M-1
          J1 = J+1
          TJ = T(J)
          TJ1 = T(J1)
          H = HSAVE
          HMAX = DABS(TJ1-TJ)
          KFLAG = 0
          KB =(J-1)*N
C:        Begin SetVec.Vec
          DO 118 L1=1,N
            T1(L1)=X(L1+KB)
118       CONTINUE
C.        End SetVec.Vec
          CALL IVPSOL(N,FCN,TJ,T1,TJ1,TOL,HMAX,H,KFLAG)
          HSAVE = H
          IF(H.EQ.ZERO)THEN
C           trajectory computation failed
            IF(ITER.EQ.0)THEN
              INFO = -3
              GOTO 9993
            ENDIF
            IF(KPRINT.GE.0)THEN
119           FORMAT('trajectory ','computation ','failed, ',
     *        'relaxation ','factor ','or ','pseudo-rank ','reduced',/)
              write ( lumon, '(a)' ) ' '
              WRITE(LUMON,119)
            ENDIF
            FC = FC*HALF
            IF(FC.LT.FCMIN)THEN
              REDUCT = .TRUE.
              GOTO 9993
            ENDIF
            FCOMPT = .FALSE.
            GOTO 9993
          ENDIF
          FCOMPT = .TRUE.
C         continuity conditions
C:        Begin SetVec.Vec
          DO 120 L1=1,N
            XU(L1+KB)=T1(L1)
120       CONTINUE
C.        End SetVec.Vec
          KB2 = KB+N
C:        Begin SetVec.Vec-Vec
          DO 121 L1=1,N
            HH(L1+KB)=T1(L1)-X(L1+KB2)
121       CONTINUE
C.        End SetVec.Vec-Vec
117     CONTINUE
C       two-point boundary conditions
C:      Begin SetVec.Vec
        DO 122 L1=1,N
          XM(L1)=X(L1+NM1)
122     CONTINUE
C.      End SetVec.Vec
C:      Begin SetVec.Vec
        DO 123 L1=1,N
          X1(L1)=X(L1)
123     CONTINUE
C.      End SetVec.Vec
        CALL BC(X1,XM,R)
9993  CONTINUE
C.    End of Segment FcnInt.Body
      RETURN
      END
      SUBROUTINE BLSOLI(N,M,M1,NM,NM1,LEVEL,NE,NB,IRANK,IRANKB,
     *IREPET,NYMAX,KPRINT,EPS,REDH,TOLMIN,TOL,RELDIF,EPH,EPX1H,
     *SIGDEL,SIGDLH,E,EH,HH,DHH,R,A,B,BG,G,QE,U,QU,DE,DU,T1,T2,US,
     *DX1,D,DDX,DXQ,XW,DR,RF,IROW,ICOL,ICOLB,PIVOT,NY,INFO,LUMON)

c*********************************************************************72
c
cc BLSOLI seeks the least squares solution for the local approach.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)

      INTEGER N,M,M1,NM,NM1,LEVEL,NE,NB,IRANK,IRANKB,IREPET,NYMAX,
     *KPRINT
      DOUBLE PRECISION EPS,REDH,TOLMIN
      DOUBLE PRECISION TOL,RELDIF,EPH,EPX1H,SIGDEL,SIGDLH
      INTEGER INFO,NY,LUMON
      INTEGER IROW(N),ICOL(N),ICOLB(N),PIVOT(N)
      DOUBLE PRECISION G(N,N,M1)
      DOUBLE PRECISION A(N,N),B(N,N),BG(N,N),E(N,N),QE(N,N)
      DOUBLE PRECISION DDX(NM),DXQ(NM),XW(NM),HH(NM1),DHH(NM1),D(N),
     *DE(N),R(N),DR(N),U(N),DU(N),QU(N),T1(N),T2(N),DX1(N),RF(M),
     *US(N)
      DOUBLE PRECISION EH(N,N)
C:    End Parameter
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=0.5D0)
      DOUBLE PRECISION TEN
      PARAMETER (TEN=1.0D1)
      INTEGER I,IC,I0,J,JA,JIN,JN,J1,K,K0
      DOUBLE PRECISION CORR,EPDX1,EPX1,S,TH,TOLH
      LOGICAL NOTFND
      INTEGER L1,L2
      DOUBLE PRECISION S1
C:    Begin
C:    Begin of Segment SolvIn.Body
        IF(IREPET.GE.0)THEN
C         --------------------------------------------------------
C         1 computation of condensed right - hand side U(NE)
          IF(IRANK.GT.0) CALL BLRHS1(N,NE,M1,NM1,1,HH,R,B,G,U,DE,
     *    T1,BG,IROW)
C         --------------------------------------------------------
C         2 saving of right - hand side U
          IF(IRANK.GE.NE)THEN
C:          Begin SetVec.Vec
            DO 124 L1=1,NE
              US(L1)=U(L1)
124         CONTINUE
C.          End SetVec.Vec
          ENDIF
        ENDIF
C       ----------------------------------------------------------
C       3 ( best ) constrained least squares solution of linear(NE,
C         NE) -system
C:      Vec DX1 = Scalar (for 1,N)
        S1 = ZERO
        DO 125 L1=1,N
          DX1(L1)=S1
125     CONTINUE
C.      End SetVec.S
        IF(IRANK.GT.0)THEN
          CALL BLSOLC(E,N,N,IRANKB,NE,NE,DX1,U,IRANK,D,PIVOT,
     *    IREPET,QE,T1)
        ENDIF
C       ----------------------------------------------------------
C       4 iterative refinement of DX1
        EPH = EPS
        IF(IRANK.GE.NE.AND.NE.NE.0)THEN
C:        Begin SetVec.MatxVec
          DO 126 L1=1,NE
            S1=0.0
            DO 127 L2=1,NE
              S1=S1+EH(L1,L2)*DX1(L2)
127         CONTINUE
            DU(L1)=S1
126       CONTINUE
C.        End SetVec.MatxVec
C:        Begin SetVec.Vec-Vec
          DO 128 L1=1,NE
            DU(L1)=US(L1)-DU(L1)
128       CONTINUE
C.        End SetVec.Vec-Vec
C         Solution of residual equation
          CALL BLSOLC(E,N,N,IRANKB,NE,NE,T2,DU,IRANK,D,PIVOT,
     *    IREPET,QE,T1)
C:        EPDX1 = Max.Norm of Vec T2 (for 1,NE)
          EPDX1 = 0.0
          DO 129 L1=1,NE
            S1 = DABS(T2(L1))
            IF(S1.GT.EPDX1) EPDX1=S1
129       CONTINUE
C.        End MakeMaxNorm.Vec
C:        Vec DX1 = Vec DX1 + Vec T2 (for 1,NE)
          DO 130 L1=1,NE
            DX1(L1)=DX1(L1)+T2(L1)
130       CONTINUE
C.        End SetVec.&Vec
C:        EPX1 = Max.Norm of Vec DX1 (for 1,NE)
          EPX1 = 0.0
          DO 131 L1=1,NE
            S1 = DABS(DX1(L1))
            IF(S1.GT.EPX1) EPX1=S1
131       CONTINUE
C.        End MakeMaxNorm.Vec
          EPX1H = EPDX1/EPX1
          EPH = TEN*EPDX1
          IF(EPX1H.GT.HALF)THEN
            INFO = -8
            GOTO 9992
          ENDIF
        ENDIF
C       ----------------------------------------------------------
C       5 Descaling and back - permutation of solution DX1
C:      Permutation ICOL of Vec DXQ = Vec DX1 (for 1,N)
        DO 132 L1=1,N
          L2 = ICOL(L1)
          DXQ(L2) = DX1(L1)
132     CONTINUE
C.      End SetVecByPermVec.Vec
C:      Vec DXQ = Vec DXQ * Vec XW (for 1,N)
        DO 133 L1=1,N
          DXQ(L1)=DXQ(L1)*XW(L1)
133     CONTINUE
C.      End SetVec.VecxVec
C       ----------------------------------------------------------
C       6 Recursive computation of DXQ(N,2),  ... ,  DXQ(N,M)
        CALL BLRCRS(N,M,M1,NM,NM1,1,HH,G,DXQ,T1,T2)
C       ----------------------------------------------------------
C       1 Iterative refinement sweeps NY = 1 ,  ... ,  NYMAX
        NY = 0
        SIGDEL = TEN*TEN
        SIGDLH = ZERO
        IF(EPH.LT.EPS) EPH = EPS
        IF(NYMAX.NE.0.AND.IRANK.GE.NE.AND.NE.NE.0)THEN
          IF(KPRINT.GT.0)THEN
134         FORMAT('Iterative ','refinement',/)
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,134)
          ENDIF
C         --------------------------------------------------------
C         1.1 Computation of required continuity residuals DHH(N,
C             M1)
          JN = 1
          JIN = M
C         --------------------------------------------------------
C         1.2 Computation of boundary residual DR(N)
C:        DO (Until)
135       CONTINUE
C:          Begin SetVec.MatxVec
            DO 136 L1=1,N
              S1=0.0
              DO 137 L2=1,N
                S1=S1+A(L1,L2)*DXQ(L2)
137           CONTINUE
              DR(L1)=S1
136         CONTINUE
C.          End SetVec.MatxVec
C:          Begin SetVec.MatxVec
            DO 138 L1=1,N
              S1=0.0
              DO 139 L2=1,N
                S1=S1+B(L1,L2)*DXQ(L2+NM1)
139           CONTINUE
              T1(L1)=S1
138         CONTINUE
C.          End SetVec.MatxVec
C:          Vec DR = Formula (for 1,N)
            DO 140 L1=1,N
              DR(L1)=R(L1)+DR(L1)+T1(L1)
140         CONTINUE
C.          End SetVec.Comp
C           ------------------------------------------------------
C           1.3 Computation of condensed residual DU(NE)
            IF(IRANK.GT.0) CALL BLRHS1(N,NE,M1,NM1,JIN,DHH,DR,B,G,
     *      DU,DE,T1,BG,IROW)
C           ------------------------------------------------------
C           1.4 Computation of correction DDX(N)
C:          Vec DX1 = Scalar (for 1,N)
            S1 = ZERO
            DO 141 L1=1,N
              DX1(L1)=S1
141         CONTINUE
C.          End SetVec.S
            IF(IRANK.GT.0) CALL BLSOLC(E,N,N,IRANKB,NE,NE,DX1,DU,
     *      IRANK,D,PIVOT,IREPET,QE,T1)
C           ------------------------------------------------------
C           2 Descaling of DDX(N),  refinement of DXQ(N)
C:          CORR = Max.Norm of Vec DX1 (for 1,N)
            CORR = 0.0
            DO 142 L1=1,N
              S1 = DABS(DX1(L1))
              IF(S1.GT.CORR) CORR=S1
142         CONTINUE
C.          End MakeMaxNorm.Vec
C:          Permutation ICOL of Vec T1 = Vec DX1 (for 1,N)
            DO 143 L1=1,N
              L2 = ICOL(L1)
              T1(L2) = DX1(L1)
143         CONTINUE
C.          End SetVecByPermVec.Vec
C:          Vec DDX = Vec T1 * Vec XW (for 1,N)
            DO 144 L1=1,N
              DDX(L1)=T1(L1)*XW(L1)
144         CONTINUE
C.          End SetVec.VecxVec
C:          Vec DXQ = Vec DXQ + Vec DDX (for 1,N)
            DO 145 L1=1,N
              DXQ(L1)=DXQ(L1)+DDX(L1)
145         CONTINUE
C.          End SetVec.&Vec
            IF(CORR.GE.EPH)THEN
              EPH = CORR
              INFO = -8
              GOTO 9992
            ENDIF
            RF(1)=CORR
C           ------------------------------------------------------
C           3 Recursive computation of DDX(N+1),  ... ,  DDX(NM)
            CALL BLRCRS(N,M,M1,NM,NM1,JIN,DHH,G,DDX,T1,T2)
C           ------------------------------------------------------
C           3.1 Refinement of DXQ(N+1),  ... ,  DXQ(NM)
            DO 146 J=2,M
              I0 =(J-1)*N
C:            Vec DXQ = Vec DXQ + Vec DDX (for I0+1,I0+N)
              DO 147 L1=I0+1,I0+N
                DXQ(L1)=DXQ(L1)+DDX(L1)
147           CONTINUE
C.            End SetVec.&Vec
C:            CORR = Max of Formula Elements (for I0+1,I0+N)
              CORR =-7.23D75
              DO 148 L1=I0+1,I0+N
                S1=DABS(DDX(L1)/XW(L1))
                IF(S1.GT.CORR)CORR=S1
148           CONTINUE
C.            End MakeMax.Comp
              RF(J)=CORR
146         CONTINUE
C           ------------------------------------------------------
C           3.2 Determination of sweep index JN
            JA = JN
            J = 1
            NOTFND = .TRUE.
C:          While (expression)
149         IF(J.LE.M.AND.NOTFND)THEN
              IF(RF(J).GT.EPH)THEN
                NOTFND = .FALSE.
              ELSE
                JN = J
                J = J+1
              ENDIF
            GOTO 149
            ENDIF
C.          EndWhile
            NY = NY+1
            IF(KPRINT.GT.0)THEN
150           FORMAT('Sweep',1X,I3,1X,'starts ','at',1X,I3)
              write ( lumon, '(a)' ) ' '
              WRITE(LUMON,150)NY,JA
151           FORMAT((1X,5(D12.3,1X)))
              WRITE(LUMON,151)(RF(L1),L1=1,M)
            ENDIF
            IF(JN.LE.JA)THEN
              INFO = -6
              GOTO 9992
            ENDIF
            IF(JN.NE.M) JIN = JN
C           ------------------------------------------------------
C           3.3 Determination and adaptation of parameters TOL AND
C               RELDIF
            IF(LEVEL.NE.0.AND.NY.LE.1)THEN
              DO 152 J=1,M1
                S = RF(J+1)/RF(J)
                IF(SIGDLH.LT.S) SIGDLH = S
                RF(J)=S
152           CONTINUE
              IF(KPRINT.GT.0)THEN
153             FORMAT('Norms ','of ','wronskians')
                write ( lumon, '(a)' ) ' '
                WRITE(LUMON,153)
154             FORMAT((1X,5(D12.3,1X)))
                WRITE(LUMON,154)(RF(L1),L1=1,M1)
              ENDIF
              SIGDEL = DMAX1(SIGDLH,SIGDEL)
              TH = TOL*SIGDEL
              IF(TH.GT.REDH)THEN
                INFO = -7
                GOTO 9992
              ENDIF
              IF(TH.GT.EPH) EPH = TH
              TOLH = EPS/SIGDEL
              IF(TOLH.LT.TOLMIN) TOLH = TOLMIN
CWei;         TOL = TOLH
CWei;         RELDIF = DSQRT(TOL/SIGDEL)
              IF(KPRINT.GE.0)THEN
155             FORMAT('Suggested ','integrator ','accuracy',D10.1
     *          ,2X,/,'Suggested ','relative ','deviation ',
     *          'parameter',D10.1,2X,//,'Adapted ','in ',
     *          'the ','next ','iteration ','step',/)
                write ( lumon, '(a)' ) ' '
                WRITE(LUMON,155)TOLH,RELDIF
              ENDIF
            ENDIF
            IF(JN.NE.M)THEN
              DO 156 J=JN,M1
                J1 = J+1
                DO 157 I=1,N
                  K0 =(J-1)*N
                  S = HH(I+K0)
                  DO 158 K=1,N
                    S = S+G(I,K,J)*DXQ(K+K0)
158               CONTINUE
                  DHH(I+K0)=S-DXQ(I+K0+N)
157             CONTINUE
156           CONTINUE
            ENDIF
          IF(.NOT.(JN.EQ.M)) GOTO  135
C.        UNTIL ( expression - negated above)
        ENDIF
C       End of iterative refinement sweeps
C       ----------------------------------------------------------
C       4 Projection of separated linear boundary conditions at T(
C         M)
        IF(NB.NE.0)THEN
          DO 159 K=1,NB
            IC = ICOLB(K)
            DXQ(IC+NM1)=ZERO
159       CONTINUE
        ENDIF
9992  CONTINUE
C.    End of Segment SolvIn.Body
      RETURN
      END
      SUBROUTINE BGSOLI(N,M,M1,NM,NM1,NDIM,LICN,NKEEP,NA,NAQ,NB,
     *NBQ,NRS,ITER,LEVEL,KPRINT,EPS,REDH,TOLMIN,FC,FCA,TOL,RELDIF,
     *EPH,EPX1H,SIGDEL,SIGDLH,COND,CORR,HH,DHH,R,A,B,G,U,DE,DU,T1,
     *DXQ,XW,DR,RF,WO,E,IROW,ICOLA,ICOLB,ICN,IKEEP,INFO,LUMON)

c*********************************************************************72
c
cc BGSOLI seeks the least squares solution for the global approach.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)

      INTEGER N,M,M1,NM,NM1,NDIM,LICN,NKEEP,NA,NAQ,NB,NBQ,NRS,ITER,
     *LEVEL,KPRINT,LUMON
      DOUBLE PRECISION EPS,REDH,TOLMIN,FC,FCA
      DOUBLE PRECISION TOL,RELDIF,EPH,EPX1H,SIGDEL,SIGDLH,COND,
     *CORR
      INTEGER INFO
      INTEGER IROW(N),ICOLA(N),ICOLB(N)
      INTEGER ICN(LICN),IKEEP(NKEEP)
      DOUBLE PRECISION G(N,N,M1)
      DOUBLE PRECISION A(N,N),B(N,N)
      DOUBLE PRECISION DXQ(NM),XW(NM),HH(NM1),DHH(NM1),DE(N),R(N),
     *DR(N),U(NM),DU(NM),T1(N),RF(M),E(LICN),WO(NM)
C:    End Parameter
C:    EPMACH = relative machine precision
      DOUBLE PRECISION EPMACH,SMALL
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=0.5D0)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.0D0)
      DOUBLE PRECISION TEN
      PARAMETER (TEN=1.0D1)
      INTEGER I,IC,I0,J,J1,K,MTYPE
      DOUBLE PRECISION S,TOLH
      REAL ST
      INTEGER L1,L2
      DOUBLE PRECISION S1
C:    Begin
C:    Begin of Segment SolvIn.Body
C       ----------------------------------------------------------
C       1 Computation of right - hand side U(NDIM)
        CALL ZIBCONST(EPMACH,SMALL)
        DO 127 J=1,M1
          J0 =(J-1)*N
          J1 = J0+N
          DO 128 I=1,N
            U(I+J0)=HH(I+J0)/XW(I+J1)
128       CONTINUE
127     CONTINUE
        IF(NRS.NE.0)THEN
          DO 129 I=1,NRS
            I0 = IROW(NB+I)
            U(NM1+I)=DE(I0)*R(I0)
129       CONTINUE
        ENDIF
C       ----------------------------------------------------------
C       2 Solution of linear(NDIM,NDIM) -system
        MTYPE = 1
        CALL MA28CD(NDIM,E,LICN,ICN,IKEEP,U,WO,MTYPE)
        COND = ZERO
        IF(NAQ.NE.0)THEN
          DO 130 I=1,NAQ
            IC = ICOLA(I)
            S = U(I)
            DXQ(IC)=S*XW(IC)
            COND = COND+DABS(S)
130       CONTINUE
        ENDIF
        IF(M.NE.2)THEN
          DO 131 J=2,M1
            J0 =(J-1)*N
            DO 132 I=1,N
              S = U(J0-NA+I)
              DXQ(I+J0)=S*XW(I+J0)
              COND = COND+DABS(S)
132         CONTINUE
131       CONTINUE
        ENDIF
        IF(NBQ.NE.0)THEN
          DO 133 I=1,NBQ
            IC = ICOLB(NB+I)
            S = U(NM1-NA+I)
            DXQ(IC+NM1)=S*XW(IC+NM1)
            COND = COND+DABS(S)
133       CONTINUE
        ENDIF
        IF(NB.NE.0)THEN
          DO 134 K=1,NB
            IC = ICOLB(K)
            DXQ(IC+NM1)=ZERO
134       CONTINUE
        ENDIF
        IF(NA.NE.0)THEN
          DO 135 K=1,NA
            IC = ICOLA(NAQ+K)
            DXQ(IC)=ZERO
135       CONTINUE
        ENDIF
C       ----------------------------------------------------------
C       3 Iterative refinement
        IF(KPRINT.GT.0)THEN
136       FORMAT('Iterative ','refinement',/)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,136)
        ENDIF
C       ----------------------------------------------------------
C       4 Computation of required continuity residuals DHH(NM1)
        DO 137 J=1,M1
          J0 =(J-1)*N
          J1 = J0+N
          DO 138 I=1,N
            S = HH(I+J0)
            DO 139 K=1,N
              S = S+G(I,K,J)*DXQ(K+J0)
139         CONTINUE
            DHH(I+J0)=S-DXQ(I+J1)
138       CONTINUE
137     CONTINUE
C       ----------------------------------------------------------
C       5 Computation of boundary residual DR(N)
C:      Begin SetVec.MatxVec
        DO 140 L1=1,N
          S1=0.0
          DO 141 L2=1,N
            S1=S1+A(L1,L2)*DXQ(L2)
141       CONTINUE
          DR(L1)=S1
140     CONTINUE
C.      End SetVec.MatxVec
C:      Begin SetVec.MatxVec
        DO 142 L1=1,N
          S1=0.0
          DO 143 L2=1,N
            S1=S1+B(L1,L2)*DXQ(L2+NM1)
143       CONTINUE
          T1(L1)=S1
142     CONTINUE
C.      End SetVec.MatxVec
C:      Vec DR = Formula (for 1,N)
        DO 144 L1=1,N
          DR(L1)=R(L1)+DR(L1)+T1(L1)
144     CONTINUE
C.      End SetVec.Comp
C       ----------------------------------------------------------
C       6 Computation of residual DU(NDIM)
        DO 145 J=1,M1
          J0 =(J-1)*N
          J1 = J0+N
          DO 146 I=1,N
            DU(J0+I)=DHH(I+J0)/XW(I+J1)
146       CONTINUE
145     CONTINUE
        IF(NRS.NE.0)THEN
          DO 147 I=1,NRS
            I0 = IROW(NB+I)
            DU(NM1+I)=DE(I0)*DR(I0)
147       CONTINUE
        ENDIF
C       ----------------------------------------------------------
C       7 Computation of correction
        MTYPE = 1
        CALL MA28CD(NDIM,E,LICN,ICN,IKEEP,DU,WO,MTYPE)
C       ----------------------------------------------------------
C       8 Refinement of DXQ
        CORR = ZERO
        IF(NAQ.NE.0)THEN
          DO 148 I=1,NAQ
            IC = ICOLA(I)
            S = DU(I)
            DXQ(IC)=DXQ(IC)+S*XW(IC)
            CORR = CORR+DABS(S)
148       CONTINUE
        ENDIF
        IF(M.NE.2)THEN
          DO 149 J=2,M1
            J0 =(J-1)*N
            DO 150 I=1,N
              S = DU(J0-NA+I)
              DXQ(I+J0)=DXQ(I+J0)+S*XW(I+J0)
              CORR = CORR+DABS(S)
150         CONTINUE
149       CONTINUE
        ENDIF
        IF(NBQ.NE.0)THEN
          DO 151 I=1,NBQ
            IC = ICOLB(NB+I)
            S = DU(NM1-NA+I)
            DXQ(IC+NM1)=DXQ(IC+NM1)+S*XW(IC+NM1)
            CORR = CORR+DABS(S)
151       CONTINUE
        ENDIF
        COND = CORR/(COND*EPMACH)
        IF(KPRINT.GT.0)THEN
152       FORMAT('Norm ','of ','residual',D12.3,2X)
          write ( lumon, '(a)' ) ' '
          WRITE(LUMON,152)CORR
        ENDIF
C       End of iterative refinement
C       ----------------------------------------------------------
C       9 Determination and adaptation of parameters TOL and
C         RELDIF
        IF(LEVEL.NE.0)THEN
          DO 153 J=1,M1
            J0 =(J-1)*N
            J1 = J0+N
            SIGDEL = ZERO
            ST = ZERO
            DO 154 I=1,N
              II = N
              IF(J.EQ.1) II = NAQ
              S = DABS(DXQ(I+J0))/XW(I+J0)
              IF(ST.LT.S) ST = S
              S = ZERO
              DO 155 K=1,II
                KK = K
                IF(J.EQ.1) KK = ICOLA(KK)
                S = S+G(I,KK,J)*DXQ(KK+J0)/XW(I+J1)
155           CONTINUE
              S = DABS(S)
              IF(SIGDEL.LT.S) SIGDEL = S
154         CONTINUE
            RF(J)=SIGDEL/ST+ONE
153       CONTINUE
          IF(KPRINT.GT.0)THEN
156         FORMAT('Norms ','of ','wronskians')
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,156)
157         FORMAT((1X,5(D12.3,1X)))
            WRITE(LUMON,157)(RF(L1),L1=1,M1)
          ENDIF
          SIGDLH = ZERO
          DO 158 J=1,M1
            IF(SIGDLH.LT.RF(J)) SIGDLH = RF(J)
158       CONTINUE
          SIGDEL = SIGDLH
          IF(FC.EQ.ONE.AND.FCA.EQ.ONE.AND.ITER.GT.0) SIGDEL =
     *    SIGDLH*COND
          SIGDEL = DMAX1(SIGDEL,TEN)
          EPH = TOL*SIGDEL
          IF(EPH.GT.REDH)THEN
            INFO = -7
            GOTO 9989
          ENDIF
          TOLH = EPS/SIGDEL
          IF(TOLH.LT.TOLMIN) TOLH = TOLMIN
CWEI;     TOL = TOLH
CWEI;     RELDIF = DSQRT(TOL/SIGDEL)
          IF(KPRINT.GE.0)THEN
159         FORMAT('Suggested ','integrator ','accuracy',D10.1
     *      ,2X,/,'Suggested ','relative ','deviation ',
     *      'parameter',D10.1,2X,/)
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,159)TOLH,RELDIF
160         FORMAT('Adapted ','in ','the ','next ',
     *      'iteration ','step',/)
            write ( lumon, '(a)' ) ' '
            WRITE(LUMON,160)
          ENDIF
        ENDIF
9989  CONTINUE
C.    End of Segment SolvIn.Body
      RETURN
      END
      SUBROUTINE BLPRCD(LUMON,COND,SENS,SMALIN,J,IRANK)

c*********************************************************************72
c
cc BLPRCD prints the subcondition and sensitivity information.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)
      DOUBLE PRECISION COND,SENS,SMALIN
      INTEGER J,IRANK,LUMON
C:    End Parameter
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.0D0)
      DOUBLE PRECISION SENSP

      IF(SENS.LT.ONE)THEN
        SENSP = SENS*SMALIN
160     FORMAT('Subcondition (',I2,',',I2,') ',D10.3,2X,/,
     *  'Sensitivity  (',I2,',',I2,') ',D10.3,2X,/)
        write ( lumon, '(a)' ) ' '
        WRITE(LUMON,160)J,IRANK,COND,J,IRANK,SENSP
      ELSE
161     FORMAT('Subcondition ','(',I2,',',I2,') ',D10.3,2X,/,
     *  'Sensitivity ','(',I2,',',I2,') ',D10.3,2X,' *',D7.0
     *  ,2X,/)
        write ( lumon, '(a)' ) ' '
        WRITE(LUMON,161)J,IRANK,COND,J,IRANK,SENS,SMALIN
      ENDIF

      return
      END
      SUBROUTINE BLPRCV(LUMON,CONV,EPH)

c*********************************************************************72
c
cc BLPRCV prints the relative accuracy information.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)
      DOUBLE PRECISION CONV,EPH
      INTEGER LUMON

162   FORMAT('Achieved ','relative ','accuracy',D10.3,2X)
      write ( lumon, '(a)' ) ' '
      WRITE(LUMON,162)CONV
      IF(EPH.GT.CONV) CONV = EPH
163   FORMAT('Reliable ','relative ','accuracy',D10.3,2X,/)
      write ( lumon, '(a)' ) ' '
      WRITE(LUMON,163)CONV

      return
      END
      SUBROUTINE BLSCLE(N,M,NM,NM1,X,XU,XW,XTHR)

c*********************************************************************72
c
cc BLSCLE scales the variables.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)
      INTEGER N
      INTEGER M
      INTEGER NM
      INTEGER NM1
      DOUBLE PRECISION X(NM)
      DOUBLE PRECISION XW(NM)
      DOUBLE PRECISION XU(NM1)
      DOUBLE PRECISION XTHR
C:    End Parameter
C     PROVIDES SCALING XW(NM)OF VARIABLES X(NM)
C:    EPMACH = relative machine precision
      DOUBLE PRECISION EPMACH,SMALL
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      DOUBLE PRECISION HALF
      PARAMETER (HALF=0.5D0)
      DOUBLE PRECISION RED
      PARAMETER (RED=1.0D-2)
      INTEGER I,J,J0,J1
      DOUBLE PRECISION XMAX
      INTEGER L1
C:    Begin
C:    Vec XW = Formula (for 1,N)
      CALL ZIBCONST(EPMACH,SMALL)
      DO 164 L1=1,N
        XW(L1)=DABS(X(L1))
164   CONTINUE
C.    End SetVec.Comp
C     ------------------------------------------------------------
C     1 Arithmetic mean for XW(2*N)... XW(M*N)
      DO 165 J=1,M-1
        J0 =(J-1)*N
        J1 = J0+N
        DO 166 I=1,N
          XW(I+J1)=(DABS(X(I+J1))+DABS(XU(I+J0)))*HALF
166     CONTINUE
165   CONTINUE
C     ------------------------------------------------------------
C     2 Threshold
      DO 167 I=1,N
        XMAX = ZERO
        DO 168 J=0,NM1,N
          IF(XMAX.LT.XW(I+J)) XMAX = XW(I+J)
168     CONTINUE
        XMAX = XMAX*RED
        IF(XMAX.LT.XTHR) XMAX = XTHR
        DO 169 J=0,NM1,N
          IF(XW(I+J).LT.XMAX) XW(I+J)=XMAX
169     CONTINUE
167   CONTINUE
      RETURN
      END
      SUBROUTINE BLLVLS(N,M,NM,NM1,XW,DXQ,HH,R,DE,CONV,SUMX,SUMF,
     *KPRINT)

c*********************************************************************72
c
cc BLLVLS evaluates level functions for the local approach.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)
C
      INTEGER N,M,NM,NM1,KPRINT
      DOUBLE PRECISION XW(NM),DXQ(NM),HH(NM1),R(N),DE(N)
      DOUBLE PRECISION CONV,SUMX,SUMF
C:    End Parameter
C:    SMALL = squareroot of "smallest positive machine number
C     divided by relative machine precision"
      DOUBLE PRECISION SMALL
      PARAMETER (SMALL=4.94D-32)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      INTEGER I,J,J0,J1
      DOUBLE PRECISION S
      INTEGER L1
C:    Begin
C     ------------------------------------------------------------
C     1 Evaluation of scaled natural level function SUMX and
C       scaled maximum error norm CONV
      CONV = ZERO
      SUMX = ZERO
      DO 170 J=1,NM
        S = DABS(DXQ(J)/XW(J))
        IF(CONV.LT.S) CONV = S
        SUMX = SUMX+S*S
170   CONTINUE
C     ------------------------------------------------------------
C     2 Evaluation of (scaled) standard level function sumfs (only
C       if needed for print)
C:    SUMF = Sum of Formula Elements (for 1,N)
      SUMF = 0.0D0
      DO 171 L1=1,N
        SUMF=SUMF+((R(L1)*DE(L1)/SMALL)**2)
171   CONTINUE
C.    End MakeSum.Comp
      DO 172 J=1,M-1
        J0 =(J-1)*N
        J1 = J0+N
        DO 173 I=1,N
          SUMF = SUMF+(HH(I+J0)/XW(I+J1))**2
173     CONTINUE
172   CONTINUE
      RETURN
      END
      SUBROUTINE BGLVLS(N,M,NM,NM1,XW,DXQ,HH,R,DE,CONV,SUMX,SUMF,
     *KPRINT)

c*********************************************************************72
c
cc BGLVLS evaluates level functions for the global approach.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)
C
      INTEGER N,M,NM,NM1,KPRINT
      DOUBLE PRECISION XW(NM),DXQ(NM),HH(NM1),R(N),DE(N)
      DOUBLE PRECISION CONV,SUMX,SUMF
C:    End Parameter
C:    SMALL = squareroot of "smallest positive machine number
C     divided by relative machine precision"
      DOUBLE PRECISION SMALL
      PARAMETER (SMALL=4.94D-32)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      INTEGER I,J,J0,J1
      DOUBLE PRECISION S
      INTEGER L1
C:    Begin
C     ------------------------------------------------------------
C     1 Evaluation of scaled natural level function SUMX and
C       scaled maximum error norm CONV
      CONV = ZERO
      SUMX = ZERO
      DO 167 J=1,NM
        S = DABS(DXQ(J)/XW(J))
        IF(CONV.LT.S) CONV = S
        SUMX = SUMX+S*S
167   CONTINUE
C     ------------------------------------------------------------
C     2 Evaluation of (scaled) standard level function sumfs (only
C       if needed for print)
C:    SUMF = Sum of Formula Elements (for 1,N)
      SUMF = 0.0D0
      DO 168 L1=1,N
        SUMF=SUMF+((R(L1)*DE(L1))**2)
168   CONTINUE
C.    End MakeSum.Comp
      DO 169 J=1,M-1
        J0 =(J-1)*N
        J1 = J0+N
        DO 170 I=1,N
          SUMF = SUMF+(HH(I+J0)/XW(I+J1))**2
170     CONTINUE
169   CONTINUE
      RETURN
      END
      SUBROUTINE BLRHS1(N,NE,M1,NM1,JIN,HH,R,B,G,U,DE,V,BG,IROW)

c*********************************************************************72
c
cc BLRHS1 computes the condensed right hand side.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)
      INTEGER N,NE,M1,NM1,JIN
      DOUBLE PRECISION HH(NM1),R(N)
      DOUBLE PRECISION B(N,N)
      DOUBLE PRECISION G(N,N,M1)
      DOUBLE PRECISION U(N),DE(N),V(N)
      DOUBLE PRECISION BG(N,N)
      INTEGER IROW(N)
C:    End Parameter
C     Computation of condensed right-hand side U(NE)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      INTEGER I,IR,J,JJ,J1,K,K0,L,M2
      DOUBLE PRECISION S,TH
C:    Begin
      DO 174 I=1,NE
        IR = IROW(I)
        U(I)=DE(IR)*R(IR)
174   CONTINUE
      IF(JIN.GT.M1)THEN
        RETURN
      ENDIF
      DO 175 I=1,NE
        IR = IROW(I)
        S = U(I)
        K0 = NM1-N
        DO 176 K=1,N
          TH = DE(IR)*B(IR,K)
          BG(I,K)=TH
          S = S+TH*HH(K+K0)
176     CONTINUE
        U(I)=S
175   CONTINUE
      IF(M1.EQ.1.OR.JIN.EQ.M1) RETURN
      M2 = M1-1
      DO 177 JJ=JIN,M2
        J = M2+JIN-JJ
        J1 = J+1
        DO 178 I=1,NE
          DO 179 K=1,N
            S = ZERO
            DO 180 L=1,N
              S = S+BG(I,L)*G(L,K,J1)
180         CONTINUE
            V(K)=S
179       CONTINUE
          S = U(I)
          K0 =(J-1)*N
          DO 181 K=1,N
            S = S+V(K)*HH(K+K0)
            BG(I,K)=V(K)
181       CONTINUE
          U(I)=S
178     CONTINUE
177   CONTINUE
      RETURN
      END
      SUBROUTINE BLRCRS(N,M,M1,NM,NM1,JIN,HH,G,DX,U,V)

c*********************************************************************72
c
cc BLRCRS carries out the recursive solution of a system of equations.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)
      INTEGER N,M,M1,NM,NM1,JIN
      DOUBLE PRECISION HH(NM1)
      DOUBLE PRECISION G(N,N,M1)
      DOUBLE PRECISION DX(NM),U(N),V(N)
C:    End Parameter
C     Recursive solution of m1 linear(N,N)-systems
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      INTEGER I,J,J0,J1,K
      INTEGER L1
      DOUBLE PRECISION S
C:    Begin
C:    Begin SetVec.Vec
      DO 182 L1=1,N
        U(L1)=DX(L1)
182   CONTINUE
C.    End SetVec.Vec
      DO 183 J=1,M1
        J0 =(J-1)*N
        J1 = J0+N
        DO 184 I=1,N
          IF(J.GE.JIN)THEN
            S = HH(I+J0)
          ELSE
            S = ZERO
          ENDIF
          DO 185 K=1,N
            S = S+G(I,K,J)*U(K)
185       CONTINUE
          V(I)=S
          DX(I+J1)=S
184     CONTINUE
C:      Begin SetVec.Vec
        DO 186 L1=1,N
          U(L1)=V(L1)
186     CONTINUE
C.      End SetVec.Vec
183   CONTINUE
      RETURN
      END
      SUBROUTINE BLPRJC(N,NE,IRANK,DEL,U,D,V,QE,PIVOT)

c*********************************************************************72
c
cc BLPRJC projects the reduced component.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)
C
      INTEGER IRANK,N,NE
      INTEGER PIVOT(N)
      DOUBLE PRECISION DEL
      DOUBLE PRECISION QE(N,N)
      DOUBLE PRECISION U(N),D(N),V(N)
C:    End Parameter
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      INTEGER L1,L2
      INTEGER I,IRK1
      DOUBLE PRECISION S,SH
C:    Begin
      DO 187 I=1,NE
        V(I)=U(PIVOT(I))
187   CONTINUE
      IRK1 = IRANK+1
      DEL = ZERO
      DO 188 I=IRK1,NE
C:      SH = Col I of QE * Vec V (for 1,I-1)
        SH = 0.0
        DO 189 L1=1,I-1
          SH = SH+QE(L1,I)*V(L1)
189     CONTINUE
C.      End MakeSProd.ColxVec
        S =(V(I)-SH)/D(I)
        DEL = DEL-S*S
        V(I)=S
188   CONTINUE
      DO 190 I=IRK1,NE
        K = NE+IRK1-I
        S = V(K)
        IF(K.NE.NE)THEN
C:        SH = Row K of QE * Vec V (for K+1,NE)
          SH = 0.0
          DO 191 L1=K+1,NE
            SH=SH+QE(K,L1)*V(L1)
191       CONTINUE
C.        End MakeSProd.RowxVec
          S = S-SH
        ENDIF
        S = S/D(K)
        V(K)=S
190   CONTINUE
      DO 192 I=1,IRANK
C:      S = Row I of QE * Vec V (for IRK1,NE)
        S = 0.0
        DO 193 L1=IRK1,NE
          S=S+QE(I,L1)*V(L1)
193     CONTINUE
C.      End MakeSProd.RowxVec
        V(I)=-S
192   CONTINUE
C:    Permutation PIVOT of Vec U = Vec V (for 1,NE)
      DO 194 L1=1,NE
        L2 = PIVOT(L1)
        U(L2) = V(L1)
194   CONTINUE
C.    End SetVecByPermVec.Vec
      RETURN
      END
      SUBROUTINE BLDERA(BC,N,M,NM,XW,X1,XM,R,RH,A,B,RELDIF)

c*********************************************************************72
c
cc BLDERA approximates boundary derivative matrices using differences.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)
      EXTERNAL BC
      INTEGER N,M,NM
      DOUBLE PRECISION XW(NM),X1(N),XM(N),R(N),RH(N)
      DOUBLE PRECISION A(N,N),B(N,N)
      DOUBLE PRECISION RELDIF
c
C     Difference approx. of boundary derivative matrices A(N,N)and
C       B(N,N)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.0D0)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      INTEGER I,K,NM1
      DOUBLE PRECISION S,XH
C:    Begin
      NM1 = N*(M-1)
      DO 195 K=1,N
        XH = X1(K)
        S = RELDIF*XW(K)
        IF(XH.LT.ZERO) S =-S
        X1(K)=XH+S
        CALL BC(X1,XM,RH)
        X1(K)=XH
        S = ONE/S
        DO 196 I=1,N
          A(I,K)=(RH(I)-R(I))*S
196     CONTINUE
        XH = XM(K)
        S = RELDIF*XW(K+NM1)
        IF(XH.LT.ZERO) S =-S
        XM(K)=XH+S
        CALL BC(X1,XM,RH)
        XM(K)=XH
        S = ONE/S
        DO 197 I=1,N
          B(I,K)=(RH(I)-R(I))*S
197     CONTINUE
195   CONTINUE
      RETURN
      END
      SUBROUTINE BLDERG(FCN,N,NE,M,M1,NM,NM1,T,X,XU,XW,XJ,TJ,G,
     *ICOL,IVPSOL,HSTART,TOL,RELDIF,KFLAG)

c*********************************************************************72
c
cc BLDERG estimates Wronskian matrices using differences.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)
      INTEGER N
      INTEGER NE
      INTEGER M
      INTEGER M1
      INTEGER NM
      INTEGER NM1
      DOUBLE PRECISION T(M)
      DOUBLE PRECISION X(NM)
      DOUBLE PRECISION XU(NM1)
      DOUBLE PRECISION XW(NM)
      DOUBLE PRECISION XJ(N)
      DOUBLE PRECISION TJ
      DOUBLE PRECISION G(N,N,M1)
      INTEGER ICOL(N)
      EXTERNAL IVPSOL
      DOUBLE PRECISION HSTART
      DOUBLE PRECISION TOL
      DOUBLE PRECISION RELDIF
      INTEGER KFLAG
C:    End Parameter
C     Difference approximation of Wronskian Matrices G(1),.., G(M1)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.0D0)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      INTEGER I,IK,J,J0,J1,K
      DOUBLE PRECISION HMAX,H,HSAVE,S,TJ1,TJA,TH
      EXTERNAL FCN
C:    Begin
      HSAVE = HSTART
      DO 198 J=1,M-1
        J0 =(J-1)*N
        J1 = J+1
        TJA = T(J)
        TJ1 = T(J1)
        HMAX = DABS(TJ1-TJA)
        DO 199 IK=1,N
          I = ICOL(IK)
          H = HSAVE
          IF(J.NE.1.OR.IK.LE.NE)THEN
            TJ = TJA
            KFLAG = 0
            DO 200 K=1,N
              XJ(K)=X(K+J0)
200         CONTINUE
            TH = XJ(I)
            S = RELDIF*XW(I+J0)
            IF(TH.LT.ZERO) S =-S
            XJ(I)=TH+S
            S = ONE/S
            CALL IVPSOL(N,FCN,TJ,XJ,TJ1,TOL,HMAX,H,KFLAG)
            IF(H.EQ.ZERO)THEN
              KFLAG =-J
              RETURN
            ENDIF
            DO K=1,N
              G(K,I,J)=S*(XJ(K)-XU(K+J0))
            end do
          ENDIF
199     CONTINUE
        HSAVE = H
198   CONTINUE
      KFLAG = 0
      RETURN
      END
      SUBROUTINE BLRK1G(N,M,M1,NM,NM1,XW,DX,HH,HHA,DXJ,G,FCA)

c*********************************************************************72
c
cc BLRK1G performs a rank-1 update of the Wronskian matrices.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      IMPLICIT double precision(S)
      INTEGER N,M,M1,NM,NM1
      DOUBLE PRECISION XW(NM),DX(NM),HH(NM1),HHA(NM1),DXJ(N)
      DOUBLE PRECISION G(N,N,M1)
      DOUBLE PRECISION FCA
C:    End Parameter
C     RANK-1 UPDATES OF WRONSKIAN MATRICES G(1),..., G(M1)
      DOUBLE PRECISION ONE
      PARAMETER (ONE=1.0D0)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D0)
      INTEGER I,J,J0,K
      DOUBLE PRECISION DNM,FCH,S,T

      FCH = FCA-ONE
      DO J=1,M1
        J0 =(J-1)*N
        DNM = ZERO
        DO I=1,N
          T = DX(I+J0)/XW(I+J0)
          DXJ(I)=T/XW(I+J0)
          DNM = DNM+T*T
        end do
        DNM = DNM*FCA
        IF(DNM.NE.ZERO)THEN
          DO K=1,N
            T = DXJ(K)/DNM
            DO I=1,N
              S = G(I,K,J)
              IF(S.NE.ZERO) G(I,K,J)=S+T*(HH(I+J0)+FCH*HHA(I+J0))
             end do
          end do
        ENDIF
      end do

      RETURN
      END
      SUBROUTINE BLDFX1 (N,FCN,T,Y,TEND,TOL,HMAX,H,KFLAG)

c*********************************************************************72
c
cc BLDFX1 - an explicit extrapolation integrator for nonstiff ODE's.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
C* Begin prologue DIFEX1
C
C  ---------------------------------------------------------------------
C
C* Title
C
C    Explicit extrapolation integrator for non-stiff systems of
C    ordinary first-order differential equations.
C
C* Written by        P. Deuflhard, U. Nowak, U. Poehle
C                    Adapted by L. Weimann for use with BVPSOL
C* Purpose           Solution of systems of initial value problems
C* Method            Explicit mid-point rule discretization with
C                    h**2-extrapolation
C* Category          i1a1c1. - System of nonstiff first order
C                              differential equations
C* Keywords          extrapolation, ODE, explicit mid-point rule,
C                    nonstiff
C* Version           1.2 , August 1991
C* Latest Change     January 2004
C* Library           CodeLib
C* Code              Fortran 77
C                    Double Precision
C* Environment       Standard version for FORTRAN77 environments on
C                    PCs, workstations, and hosts
C* Copyright     (c) Konrad-Zuse-Zentrum fuer Informationstechnik
C                    Berlin (ZIB)
C                    Takustrasse 7, D-14195 Berlin-Dahlem
C                    phone : + 49/30/84185-0
C                    fax   : + 49/30/84185-125
C* Contact           Uwe Poehle
C                    ZIB, Scientific Software Group
C                    phone : + 49/30/84185-241
C                    fax   : + 49/30/84185-107
C                    e-mail: poehle@zib.de
C
C  ---------------------------------------------------------------------
C
C* Licence
C  -------
C
C  You may use or modify this code for your own non-commercial
C  purposes for an unlimited time. 
C  In any case you should not deliver this code without a special 
C  permission of ZIB.
C  In case you intend to use the code commercially, we oblige you
C  to sign an according licence agreement with ZIB.
C
C
C* Warranty
C  --------
C 
C  This code has been tested up to a certain level. Defects and
C  weaknesses, which may be included in the code, do not establish
C  any warranties by ZIB. ZIB does not take over any liabilities
C  which may follow from acquisition or application of this code.
C
C
C* Software status 
C  ---------------
C
C  This code is under care of ZIB and belongs to ZIB software
C  class II.
C
C
C  ---------------------------------------------------------------------
C
C* References:
C
C /1/ W. B. Gragg:
C     On Extrapolation Algorithms for Ordinary
C     Initial Value Problems
C     SIAM J. Numer. Anal. 2, 384-404 (1965)
C
C /2/ R. Bulirsch, J. Stoer:
C     Numerical Treatment of Ordinary Differential Equations
C     by Extrapolation Methods
C     Num. Math. 8, 1-13 (1966)
C
C /3/ P. Deuflhard:
C     Order and Stepsize Control in Extrapolation Methods
C     Numer. Math. 41, 399-422 (1983)
C
C
C* External subroutine: (to be supplied by the user)
C
C    FCN           EXT  Subroutine FCN (N,T,Y,DY)
C                       Right-hand side of first-order
C                       differential equations
C                       N      Number of first-order ODEs
C                       T      Actual position
C                       Y(N)   Values at T
C                       DY(N)  Derivatives at T
C
C
C* Parameters: (* marks input/output parameters)
C
C    N         I   IN   Number of first-order ODEs
C  * T         D   IN   Starting point of integration
C                  OUT  Achieved final point of integration
C  * Y         D   IN   Array of initial values Y(1),...,Y(N)
C                  OUT  Array of final values
C    TEND      D   IN   Prescribed final point of integration
C    TOL       D   IN   Prescribed relative precision (.GT.0)
C    HMAX      D   IN   Maximum permitted stepsize
C  * H         D   IN   Initial stepsize guess
C                  OUT  Stepsize proposal for next integration step
C                       (H .EQ. 0. ,if DIFEX1 fails to proceed)
C  * KFLAG     I   IN   Print parameter
C                         0   no output
C                         1   Integration monitor
C                         2   Intermediate solution points  T,Y(I),I=1,N
C                         3   Integration monitor and solution points
C                       +10   if KFLAG is augmented by 10, a time
C                             monitor is printed additionally (this may
C                             be very expensive in terms of cpu-time)
C                  OUT  Error flag
C                       .GE. 0  Successful integration
C                               (KFLAG not altered internally)
C                       -2   More than NSTMAX basic integration steps
C                            per interval have been performed
C                       -3   More than JRMAX stepsize reductions
C                            occurred per basic integration step
C                       -4   Stepsize proposal for next basic
C                            integration step too small
C
C
C* End prologue
C  ------------
C
C
C    COMMON /STAT/ NFCN, NSTEP, NACCPT, NREJCT, NDEC, NSOL
C                       Internally initialized, for statistical
C                       purposes
C    NFCN               Number of FCN-evaluations
C    NSTEP              Number of integration steps
C    NACCPT             Number of steps accepted
C    NREJCT             Number of steps rejected
C    NDEC               Number of decompositions
C    NSOL               Number of substitutions
C
C* Type declaration
C
      INTEGER I, J, JK, JL, JM, JMACT, JOPT, JRED, JRMAX, J1, K, KFIN,
     $     KFLAG, KM, KMACT, KOPT, K1, L, LOUT, M, MAXODE, MDT, M1, N,
     $     NACCPT, NDEC, NFCN, NJ, NREJCT, NSOL, NSTEP, NSTMAX
C
      DOUBLE PRECISION ALPHA, AWK, D, DMAX1, DMIN1, DT, DUMMY(1), DY, 
     &  DYM,
     $     DZ, BLDFER, EPMACH, EPSAFE, ERR, FC, FCK, FCM, FCO, FMIN,
     $     FNJ, FN1, FN, H, HALF, HJ,  HJ2, HMAX, HMAXU, HN, HREST,
     $     HRTRN, OMJ, OMJO, ONE, PCT101, PCT90, QUART, RED, RMAX, RO,
     $     SAFE, SMALL, T, TEND, TN, TOL, TOLH, TOLMIN, U, Y, YK, YM,
     $     YMAX, YWGT, ZERO
C
      LOGICAL QFIRST, QKONV, QLAST, QPRMON, QPRSOL, QRED
C
      CHARACTER CHGDAT*20, PRODCT*8
C
      EXTERNAL FCN
C
C
C* Constants problem oriented: (to be supplied by the user)
C
C    MAXODE    I   K    Maximal number of first-order ODEs
C
      PARAMETER ( MAXODE = 1024          )
C
C* Constants machine oriented: (to be verified by the user)
C
C    EPMACH    D   K    Relative machine precision
C    LOUT      I   K    Output is written on logical unit LOUT
C    SMALL     D   K    Square-root of smallest positive machine number
C
CRAY  (adapted to Cray X-MP)
CRAY  PARAMETER ( EPMACH = 7.106D-15     ,
CRAY $            EPSAFE = EPMACH*10.0D0 ,
CRAY $            LOUT   = 6             ,
CRAY $            SMALL  = 6.771D-1234   )
C
CIBM  (adapted to Siemens 7.865, IBM 370-compatible)
CIBM  PARAMETER ( EPMACH = 2.22D-16      ,
CIBM $            EPSAFE = EPMACH*10.0D0 ,
CIBM $            LOUT   = 6             ,
CIBM $            SMALL  = 7.35D-40      )
C
CSUN  (adapted to sun)
C      PARAMETER ( EPMACH = 0.1085D-18    ,
C     $            EPSAFE = EPMACH*10.0D0 ,
C     $            LOUT   = 6             ,
C     $            SMALL  = 0.2223D-161   )
C
C* Other Constants:
C
C    HALF      D   K    1/2
C    ONE       D   K    1
C    PCT101    D   K    101 Percent
C    PCT90     D   K    90 Percent
C    QUART     D   K    1/4
C    ZERO      D   K    0
C
      PARAMETER ( HALF   = 0.5  D0       ,
     $            ONE    = 1.0  D0       ,
     $            PCT101 = 1.01 D0       ,
     $            PCT90  = 0.9  D0       ,
     $            QUART  = 0.25 D0       ,
     $            ZERO   = 0.0  D0       )
C
C* Control parameters: (to be supplied by the user)
C  standard values fixed below
C
C    NSTMAX    I   K    Maximum permitted number of integration steps
C                       per interval  =  10000
C    JRMAX     I   K    Maximum permitted number of stepsize reductions
C    KM        I   K    Prescribed maximum column number
C    JM        I   K    Associated maximum row number
C                       (JM = KM + 1)
C    MDT       I   K    Associated dimension of DT
C    BLDFSQ         EXT  Subroutine BLDFSQ(JM,NJ)
C                       Generate stepsize sequence with respect to /1/
C                       JM     Maximum row number
C                       NJ     Array(JM) of stepsize sequence
C    BLDFSC        EXT  Subroutine BLDFSC (MODE, Y, N, YOLD, YWGT,
C                                          YMAX, THREL, THABS)
C                       Scaling for DIFEX1
C                       MODE   ='INITIAL '    Initial scaling
C                              ='INTERNAL'    Scaling during 
C                                             discretization
C                              ='ACCEPTED'    Rescaling if step accepted
C                              Else           Error
C                       Y      Array of values Y(1),...,Y(N)
C                       N      Length of vectors Y, YOLD, YWGT, and YMAX
C                       YOLD   Array of old values
C                       YWGT   Array of scaled values
C                       YMAX   Array of maximum values
C                       THREL  Relative threshold value
C                       THABS  Absolute threshold value
C    BLDFER        EXT  Double Precision function BLDFER(Y, N, YWGT)
C                       Scaled root mean square error
C                       Y      Array of values Y(1),...,Y(N)
C                       N      Length of vectors Y and YWGT
C                       YWGT   Array of scaled values
C
      PARAMETER ( NSTMAX = 10000         ,
     $            JRMAX  = 10            ,
     $            KM     = 8             ,
     $            JM     = KM + 1        ,
     $            MDT    = MAXODE*JM     )
C
C* Internal parameters: (modification not recommended)
C
C
      PARAMETER ( FMIN   = 1.0  D-3      ,
     $            RMAX   = 0.75 D0       ,
     $            RO     = QUART         ,
     $            SAFE   = 0.7  D0       )
C
C
C* Local variables: (workspace)
C
C
C
C    QFIRST    L   V    First integration step
C    QKONV     L   V    Convergence detected
C    QLAST     L   V    Last integration step
C    QPRMON    L   V    Print integration monitor
C    QPRSOL    L   V    Print intermediate solution points
C
C* Dimensions:
C
      DIMENSION ALPHA(JM,JM), AWK(JM), D(JM,JM), DT(MAXODE,JM),
     $     DY(MAXODE), DYM(MAXODE), DZ(MAXODE), FCK(KM), NJ(JM),
     $     Y(MAXODE), YK(MAXODE), YM(MAXODE), YMAX(MAXODE),
     $     YWGT(MAXODE)
C
      COMMON /DXSTAT/ NFCN, NSTEP, NACCPT, NREJCT, NDEC, NSOL
C
C*******  Revision 1 *******  Latest change:
      DATA      CHGDAT      /'August 27, 1991'/
      DATA      PRODCT      /'BLDFX1'/
C***************************
C
C
C* Modification history
C  --------------------
C
C
C  1.0       Feb  9, 1988    First release at ZIB
C  1.1       Mar 27, 1991    Vectorize extrapolation loop,
C                            Time monitor
C  1.2       Aug 27, 1991    Allow reverse integration direction
C
C
      DATA  DT/MDT*0.D0/
C
C---1. Initial preparations
      CALL ZIBCONST(EPMACH,SMALL)
      EPSAFE = EPMACH*10.0D0
      LOUT   = 6
      QPRMON = (KFLAG .EQ. 1 .OR. KFLAG .EQ. 3)
      QPRSOL = (KFLAG .GE. 2)
      HRTRN = H
C
      DO 1001 I = 1, N
         YMAX(I) = ZERO
 1001 CONTINUE
C
      HREST = TEND - T
      H = SIGN (DMIN1 (DABS(H), DABS(HREST)), HREST)
      HMAX = DABS(HMAX)
      HMAXU = HMAX
      FCM = DMAX1 (DABS(H)/HMAX, FMIN)
      KMACT = KM
      JMACT = JM
      CALL BLDFSQ (JM, NJ)
      FN = DBLE (N)
      FN1 = DBLE (NJ(1))
      TOLH = RO*TOL
      TOLMIN = EPSAFE*FN
      IF (TOL .LT. TOLMIN) THEN
         WRITE (LOUT, 10002) PRODCT, TOL, TOLMIN
         TOL = TOLMIN
      ENDIF
C
C---  Compute amount of work per row of extrapolation tableau
      AWK(1) = FN1 + ONE
      DO 101 J=2,JM
         J1 = J - 1
         FNJ = DBLE (NJ(J))
         AWK(J) = AWK(J1) + FNJ
         DO 1011 K=1,J1
            D(J,K) = (FNJ / DBLE (NJ(K)))*(FNJ / DBLE (NJ(K)))
 1011    CONTINUE
C
         IF (J .NE. 2) THEN
            DO 1012 K1=2,J1
               K = K1 - 1
               ALPHA(J1,K) = TOLH**((AWK(K1) - AWK(J)) /
     $              ((AWK(J) - AWK(1) + ONE)*DBLE(K + K1)))
 1012       CONTINUE
C
         ENDIF
 101  CONTINUE
C
C---1.2 Determination of maximum column number in extrapolation
C---    tableau (information theoretic concept, ref./3/)
      KOPT = 1
      JOPT = 2
 121  CONTINUE
C     DO WHILE (JOPT .LT. KMACT .AND.
C               AWK(JOPT+1)*PCT101 .LE. AWK(JOPT)*ALPHA(JOPT,KOPT))
         IF (JOPT .GE. KMACT .OR.
     $     AWK(JOPT+1)*PCT101 .GT. AWK(JOPT)*ALPHA(JOPT,KOPT)) GOTO 122
C                                                              Exit 121
         KOPT = JOPT
         JOPT = JOPT + 1
         GOTO  121
C     ENDDO
 122  KMACT = KOPT + 1
      JMACT = JOPT
      IF (QPRMON) WRITE (LOUT, 11221)
     $     PRODCT, CHGDAT, TOL, KMACT, NJ
C
      IF (QPRSOL) WRITE (LOUT, 11222)
      NSTEP = 0
      QFIRST = .TRUE.
      QLAST = .FALSE.
C      NFCN = 0
      KFIN = 0
      OMJO = ZERO
      CALL BLDFSC ('INITIAL ', Y, N, DUMMY, YWGT, YMAX, TOL, ONE)
C
C---2. Basic integration step
 2    CONTINUE
C     DO WHILE (T .NE. TEND)
         IF (QPRMON) WRITE (LOUT, 12001) NSTEP,NFCN,T,H,KFIN,KOPT
         IF (QPRSOL) WRITE (LOUT, 12002) NSTEP,NFCN,T,H,(Y(I),I=1,N)
         JRED = 0
C
C---     Explicit euler starting step
         CALL FCN (N, T, Y, DZ)
         NFCN = NFCN + 1
C
C---3.   Basic discretization step
 3       CONTINUE
C        DO WHILE (JRED .LE. JRMAX .AND. .NOT. QKONV)
            IF (QLAST) THEN
               TN = TEND
            ELSE
               TN = T + H
            ENDIF
            IF (TN .EQ. T) THEN
C              Error 4
               IF (QPRMON) WRITE (LOUT, 13001) PRODCT
               KFLAG = -4
               GOTO  9
C              Exit to Return
            ENDIF
C
C---3.1     Internal discretization
            DO 31 J=1,JMACT
               M = NJ(J)
               M1 = M - 1
               KFIN = J - 1
               FNJ = DBLE (M)
               HJ = H / FNJ
               HJ2 = HJ + HJ
               DO I=1,N
                  YK(I) = Y(I)
                  YM(I) = Y(I) + HJ*DZ(I)
               end do
C
C---3.1.3      Explicit mid-point rule
               DO 313 K=1,M1
                  CALL FCN (N, T + HJ*DBLE (K), YM, DY)
                  NFCN = NFCN + 1
                  DO 3135 I=1,N
                     U = YK(I) + HJ2*DY(I)
                     YK(I) = YM(I)
                     YM(I) = U
 3135             CONTINUE
 313           CONTINUE
C
C---3.1.4      Smoothing final step
               CALL FCN (N, TN, YM, DY)
               NFCN = NFCN + 1
               DO 3141 I = 1,N
                  YM(I) = (YM(I) + YK(I) + HJ*DY(I))*HALF
 3141          CONTINUE
C
C
C---3.1.5      Extrapolation
               DO 3153 I=1,N
                  DY(I) = YM(I)
                  YK(I) = DT(I,1)
                  DT(I,1) = DY(I)
 3153          CONTINUE
C
               DO 3158 K=2,J
                  JK = J - K + 1
C
                  DO 3155 I=1,N
                     DYM(I) = (DY(I) - YK(I)) / (D(J,JK) - ONE)
                     DY(I) = D(J,JK)*DYM(I)
                     YK(I) = DT(I,K)
                     DT(I,K) = DYM(I)
                     YM(I) = DYM(I) + YM(I)
 3155             CONTINUE
C
 3158          CONTINUE
C
               IF (J .NE. 1) THEN
C
C---3.1.6         Convergence monitor
                  CALL BLDFSC ('INTERNAL',YM,N,Y,YWGT,YMAX,TOL,ONE)
                  ERR = BLDFER (DYM, N, YWGT)
                  QKONV = ERR .LE. TOL
                  ERR = ERR / TOLH
C
C---              Order control
                  K = J - 1
                  FC = ERR**(ONE / DBLE(K + J))
                  FCK(K) = FC
C
C---              Order window
                  IF (J .GE. KOPT .OR. QFIRST .OR. QLAST) THEN
                     IF (QKONV) GOTO 25
C                                Exit 3 for next basic integration step
C
C---                 Check for possible stepsize reduction
                     RED = ONE / FC
                     QRED = .FALSE.
                     IF (K .EQ. KMACT .OR. K .EQ. JOPT) THEN
                        RED = RED*SAFE
                        QRED = .TRUE.
                     ELSE
                        IF (K .EQ. KOPT) THEN
                           RED = RED*ALPHA(JOPT,KOPT)
                           IF (RED .LT. ONE) THEN
                              RED = ONE / FC
                              QRED = .TRUE.
                           ENDIF
                        ELSE
                           IF (KOPT .EQ. KMACT) THEN
                              RED = RED*ALPHA(KMACT,K)
                              IF (RED .LT. ONE) THEN
                                 RED = RED * SAFE
                                 QRED = .TRUE.
                              ENDIF
                           ELSE
                              RED = RED*ALPHA(JOPT,K)
                              IF (RED .LT. ONE) THEN
                                 RED = ALPHA(KOPT,K) / FC
                                 QRED = .TRUE.
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                     IF (QRED) GOTO 32
C                              Exit 3.1 to stepsize reduction
                  ENDIF
               ENDIF
 31         CONTINUE
C
C---3.2     Prepare stepsize reduction
 32         CONTINUE
C
C---3.5     Stepsize reduction
            RED = DMIN1 (RED, RMAX)
            H = H*RED
            IF (NSTEP .GT. 0) QLAST = .FALSE.
            JRED = JRED + 1
            IF (QPRMON) WRITE (LOUT, 13501) JRED,RED,
     $           KFIN,KOPT,KMACT
            IF (JRED .GT. JRMAX) THEN
C              Error 3
               IF (QPRMON) WRITE (LOUT, 13502) JRMAX
               KFLAG = -3
               GOTO  9
C              Exit to Return
            ENDIF
            GOTO  3
C        ENDDO
C
C        ************************************************
C---2.5  Preparations for next basic integration step
 25      NSTEP = NSTEP + 1
         QFIRST = .FALSE.
         IF (NSTEP .GT. NSTMAX) THEN
C           Error 2
C           Emergency exit, if too many steps taken
            IF (QPRMON) WRITE (LOUT, 12501) PRODCT, NSTMAX
            KFLAG = -2
            GOTO  9
C           Exit to return
         ENDIF
C
C---     Restoring
         DO 251 I=1, N
            Y(I) = YM(I)
 251     CONTINUE
C
         T = TN
         IF (T .EQ. TEND) GOTO 9
C                         Exit to return
         CALL BLDFSC ('ACCEPTED', Y, N, DUMMY, YWGT, YMAX, TOL, ONE)
C
C---2.7  Order and stepsize selection
C
C---2.7.1 Stepsize restrictions
         HMAX = DMIN1(HMAXU, DABS(H)/FMIN)
         FCM = DABS(H) / HMAX
C
C---2.7.2 Optimal order determination
         KOPT = 1
         JOPT = 2
         FCO = DMAX1 (FCK(1), FCM)
         OMJO = FCO*AWK(2)
         IF (KFIN .GE. 2) THEN
            DO 272 L=2,KFIN
               JL = L + 1
               FC = DMAX1 (FCK(L), FCM)
               OMJ = FC*AWK(JL)
               IF (OMJ*PCT101 .LE. OMJO .AND. L .LT. KMACT) THEN
                  KOPT = L
                  JOPT = JL
                  OMJO = OMJ
                  FCO = FC
               ENDIF
 272        CONTINUE
         ENDIF
         HREST = TEND - T
         HN = H / FCO
C
C---2.7.3 Possible increase of order
         IF (DABS(HN) .LT. DABS(HREST)) THEN
            IF ((JRED .EQ. 0 .OR. NSTEP .EQ. 0) .AND.
     $           KOPT .GE. KFIN .AND. KOPT .NE. KMACT) THEN
               FC = DMAX1 (FCO/ALPHA(JOPT,KOPT), FCM)
               JL = JOPT + 1
               IF (AWK(JL)*FC*PCT101 .LE. OMJO .AND.
     $              JOPT .LT. KMACT) THEN
                  FCO = FC
                  HN = H / FCO
                  KOPT = JOPT
                  JOPT = JOPT + 1
               ENDIF
            ENDIF
         ENDIF
C
C---2.7.4 Stepsize selection
         H = HN
         HRTRN = H
         IF (DABS(H) .GT. DABS(HREST)*PCT90) THEN
            H = HREST
            QLAST = .TRUE.
         ENDIF
         GOTO  2
C     ENDDO
C
C---9. Exit
 9    HMAX = HMAXU
      IF (KFLAG .LT. 0) THEN
C        Fail exit
         H = ZERO
      ELSE
C        Solution exit
         H = HRTRN
         IF (QPRMON) WRITE (LOUT, 12001) NSTEP,NFCN,T,H,KFIN,KOPT
         IF (QPRSOL) WRITE (LOUT, 12002) NSTEP,NFCN,T,H,(Y(I),I=1,N)
      ENDIF
      RETURN
C
C
10001 FORMAT(//,' ',A8,'  - Error -  '
     $      ,   ' Direction if integration is reverse to convention.')
10002 FORMAT(//,' ',A8,'  - Warning -'
     $      ,   ' Desired tolerance ', D10.3, ' too small.', /,
     $      22X,' tolerance set to  ', D10.3, '.')
C
11221 FORMAT(1H0,A8,' - ',A20,/,
     $       1H0,' Rel.prec. TOL ',D10.3,' max.col. ',I3, /,
     $       ' sequence ',(1H ,13I4))
11222 FORMAT(//,5X,'Step',3X,'F-calls',8X,'T',25X,'H',5X,'Y1(T)..',//)
12001 FORMAT(2I9,D20.11,D12.4,I9,I6)
12002 FORMAT(2I9,D20.11,D12.4,4D20.11,/,(50X,4D20.11))
12501 FORMAT(//,' ',A8,'  - Error -  '
     $      ,' more than NSTMAX=',I3,' integration steps',//)
13001 FORMAT(//,' ',A8,'  - Error -  '
     $      ,' stepsize reduction failed to succeed  ',//)
13501 FORMAT(I3,' Stepsize reduction factor ',D10.3,
     $      ' KFIN',I3,' KOPT',I3,' KMAX',I3)
13502 FORMAT(//,' ',A8,'  - Error -  '
     $      ,' more than JRMAX=',I3,' stepsize reductions per step',/)
      END
      SUBROUTINE BLDFSQ(M,NJ)

c*********************************************************************72
c
cc BLDFSQ sets the stepsize sequence for DIFEX1.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
      INTEGER I, M, NJ
      DIMENSION NJ(M)

      NJ(1) = 2
      DO I=2,M
         NJ(I) = NJ(I-1) + 2
      end do

      RETURN
      END
      SUBROUTINE BLDFSC (MODE, Y, N, YOLD, YWGT, YMAX, THREL, THABS)

c*********************************************************************72
c
cc BLDFSC carries out scaling for DIFEX1.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
C       (May be altered for real life applications
C        by the skillful user)
C
C
C* Parameters:
C
C    MODE      C*8 IN   ='INITIAL '    Initial scaling
C                       ='INTERNAL'    Scaling during discretization
C                       ='ACCEPTED'    Rescaling if step accepted
C                       Else           Error
C    Y         D   IN   Array of values Y(1),...,Y(N)
C    N         I   IN   Length of vectors Y, YOLD, YWGT, and YMAX
C    YOLD      D   IN   Array of old values
C    YWGT      D   OUT  Array of scaled values new
C    YMAX      D   IN   Array of maximum values old
C                  OUT  Array of maximum values new
C    THREL     D   IN   Relative threshold value
C    THABS     D   IN   Absolute threshold value
C
C* Local variables:
C
C    YUSER     D   V    User defined array of maximum values
C
C* Type declaration
C
      INTEGER I, LOUT, MAXODE, N
C
      DOUBLE PRECISION DABS, DMAX1, EPMACH, ONE, THABS, THREL, U, Y,
     $     YMAX, YOLD, YUSER, YWGT, ZERO, SMALL
C
      CHARACTER MODE*8
C
C* Constants:
C
C    EPMACH    D   K    Relative machine precision
C    LOUT      I   K    Output is written on logical unit LOUT
C    MAXODE    I   K    Maximal number of first-order ODEs
C    ONE       D   K    1.0
C    ZERO      D   K    0.0
C
CRAY  (adapted to Cray X-MP)
CRAY  PARAMETER ( EPMACH = 7.106D-15     ,
C
CIBM  (adapted to Siemens 7.865, IBM 370-compatible)
CIBM  PARAMETER ( EPMACH = 2.22D-16      ,
C
CSUN  (adapted to sun)
      PARAMETER (
     $            LOUT   = 6             ,
     $            MAXODE = 1024          ,
     $            ONE    = 1.0  D0       ,
     $            ZERO   = 0.0  D0       )
C
      DIMENSION Y(N), YOLD(N), YWGT(N), YMAX(N), YUSER(MAXODE)
      SAVE YUSER
      CALL ZIBCONST(EPMACH,SMALL)
      IF (MODE .EQ.          'INITIAL '         ) THEN
C                             --------
         DO 100 I=1,N
            YUSER(I) = DABS (YMAX(I))
            U = DABS (Y(I))
            IF (U .LT. EPMACH) U = ONE
            YMAX(I) = DMAX1 (U, YUSER(I), THABS)
            YWGT(I) = YMAX(I)
 100     CONTINUE
C
      ELSE IF (MODE .EQ.     'INTERNAL'         ) THEN
C                             --------
         DO 200 I=1,N
            YWGT(I) = DMAX1 (YMAX(I)*THREL, DABS(Y(I)),
     $                       DABS(YOLD(I)), YUSER(I), THABS)
 200     CONTINUE
C
      ELSE IF (MODE .EQ.     'ACCEPTED'         ) THEN
C                             --------
         DO I=1,N
            YMAX(I) = DMAX1 (YMAX(I), DABS(Y(I)))
         end do

      ELSE
         WRITE (LOUT, '(//,A,/)')
     $      ' D1SCAL    - ERROR -   Illegal mode'
      ENDIF
      RETURN
      END
      FUNCTION BLDFER ( Y, N, YWGT )

c*********************************************************************72
c
cc BLDFER evaluates the scaled root mean square error.
c
c  Modified:
c
c    08 January 2013
c
c  Reference:
c
c    Josef Stoer, Roland Bulirsch,
c    Introduction to Numerical Mathematics,
c    Springer, 2002,
c    ISBN: 038795452X,
c    LC: QA297.S8213
c
C* Parameters:
C
C    Y         D   IN   Array of values Y(1),...,Y(N)
C    N         I   IN   Length of Vectors Y and YWGT
C    YWGT      D   IN   Array of scaled values
C
C* Type declaration
C
      double precision bldfer
      INTEGER I, N
C
      DOUBLE PRECISION DBLE, DSQRT, SUM, Y, YWGT, ZERO
C
C* Constants:
C
C    ZERO      D   K    0
C
      PARAMETER ( ZERO   = 0.0  D0       )
C
      DIMENSION Y(N), YWGT(N)
C
      SUM = ZERO
      DO I=1,N
         SUM = SUM + (Y(I) / YWGT(I)) * (Y(I) / YWGT(I))
 100  end do

      BLDFER = DSQRT(SUM / DBLE(N))

      RETURN
      END
      SUBROUTINE BLDECC (A,NROW,NCOL,MCON,M,N,IRANK,COND,D,
     1                                            PIVOT,KRED,AH,V)

c*********************************************************************72
c
cc BLDECC carries out a constrained least squares QR decomposition.
c
c  Modified:
c
c    08 January 2013
C
C*  Written by        P. Deuflhard
C*  Purpose           Solution of least squares problems, optionally
C                     with equality constraints.
C*  Method            Constrained Least Squares QR-Decomposition
C                     (see references below)
C*  Category          D9b1. -  Singular, overdetermined or
C                              underdetermined systems of linear 
C                              equations, generalized inverses. 
C                              Constrained Least Squares solution
C*  Keywords          Linear Least Square Problems, constrained, 
C                     QR-decomposition, pseudo inverse.
C*  Version           0.9
C*  Revision          April 1984
C*  Latest Change     January 1991
C*  Library           CodeLib
C*  Code              Fortran 77, Double Precision
C*  Environment       Standard Fortran 77 environment on PC's,
C                     workstations and hosts.
C*  Copyright     (c) Konrad-Zuse-Zentrum fuer
C                     Informationstechnik Berlin (ZIB)
C                     Takustrasse 7, D-14195 Berlin-Dahlem
C                     phone : + 49/30/84185-0
C                     fax   : + 49/30/84185-125
C*  Contact           Lutz Weimann
C                     ZIB, Division Scientific Computing, 
C                          Department Numerical Analysis and Modelling
C                     phone : + 49/30/84185-185
C                     fax   : + 49/30/84185-107
C                     e-mail: weimann@zib.de
C
C*    References:
C     ===========
C
C       /1/ P.Deuflhard, V.Apostolescu:
C           An underrelaxed Gauss-Newton method for equality
C           constrained nonlinear least squares problems.
C           Lecture Notes Control Inform. Sci. vol. 7, p.
C           22-32 (1978)
C       /2/ P.Deuflhard, W.Sautter:
C           On rank-deficient pseudoinverses.
C           J. Lin. Alg. Appl. vol. 29, p. 91-111 (1980)
C    
C*    Related Programs:     SOLCON (BLSOLC)
C
C  ---------------------------------------------------------------
C
C* Licence
C    You may use or modify this code for your own non commercial
C    purposes for an unlimited time. 
C    In any case you should not deliver this code without a special 
C    permission of ZIB.
C    In case you intend to use the code commercially, we oblige you
C    to sign an according licence agreement with ZIB.
C
C* Warranty 
C    This code has been tested up to a certain level. Defects and
C    weaknesses, which may be included in the code, do not establish
C    any warranties by ZIB. ZIB does not take over any liabilities
C    which may follow from aquisition or application of this code.
C
C* Software status 
C    This code is under partial care of ZIB and belongs to ZIB software
C    class 2.
C
C     ------------------------------------------------------------
C
C*    Summary:
C     ========
C     Constrained QR-decomposition of (M,N)-system  with
C     computation of pseudoinverse in case of rank-defeciency .
C     First MCON rows belong to equality constraints.
C
C     ------------------------------------------------------------
C
C     INPUT PARAMETERS (* MARKS INOUT PARAMETERS)
C     -----------------------------------------------
C
C
C      * A(NROW,NCOL)  INPUT MATRIX
C                      A(M,N) CONTAINS ACTUAL INPUT
C        NROW          DECLARED NUMBER OF ROWS OF A AND AH
C        NCOL          DECLARED NUMBER OF COLUMNS OF A AND AH
C     (*)MCON          NUMBER OF EQUALITY CONSTRAINTS (MCON<=N)
C                      INTERNALLY REDUCED IF EQUALITY CONSTRAINTS
C                      ARE LINEARLY DEPENDENT
C        M             TREATED NUMBER OF ROWS OF MATRIX A
C        N             TREATED NUMBER OF COLUMNS OF MATRIX A
C     (*)IRANK         PSEUDO-RANK OF MATRIX A
C      * COND          PERMITTED UPPER BOUND OF DABS(D(1)/D(IRANKC))
C                      AND OF DABS(D(IRANKC+1))/D(IRANK))
C                      (SUB-CONDITION NUMBERS OF A)
C        KRED          >=0    HOUSEHOLDER TRIANGULARIZATION
C                             (BUILD UP OF PSEUDO-INVERSE,IF IRANK<N )
C                      < 0    REDUCTION OF PSEUDO-RANK OF MATRIX A
C                             SKIPPING HOUSEHOLDER TRIANGULARIZATION
C                             BUILD-UP OF NEW PSEUDO-INVERSE
C        V(N)          REAL WORK ARRAY
C
C     OUTPUT PARAMETERS
C     -----------------
C
C        A(M,N)        OUTPUT MATRIX UPDATING PRODUCT OF HOUSEHOLDER
C                      TRANSFORMATIONS AND UPPER TRIANGULAR MATRIX
C        MCON          PSEUDO-RANK OF CONSTRAINED PART OF MATRIX A
C        IRANK         PSEUDO-RANK OF TOTAL MATRIX A
C        D(IRANK)      DIAGONAL ELEMENTS OF UPPER TRIANGULAR MATRIX
C        PIVOT(N)      INDEX VECTOR STORING PERMUTATION OF COLUMNS
C                      DUE TO PIVOTING
C        COND          SUB-CONDITION NUMBER OF A
C                      (IN CASE OF RANK REDUCTION: SUB-CONDITION NUMBER
C                      WHICH LED TO RANK REDUCTION)
C        AH(N,N)       UPDATING MATRIX FOR PART OF PSEUDO INVERSE
C
C----------------------------------------------------------------------
C
      INTEGER  IRANK, KRED, MCON, M, N, NROW, NCOL, PIVOT(N)
      INTEGER  I, II, IRK1, I1, J, JD, JJ, K, K1, MH, ISUB
      DOUBLE PRECISION    A(NROW,NCOL), AH(NCOL,NCOL), D(N), V(N)
      DOUBLE PRECISION    COND, ONE , DD, DABS, DSQRT
      DOUBLE PRECISION    H, HMAX, S, T, SMALL, ZERO, EPMACH
C     COMMON /MACHIN/ EPMACH, SMALL
C
      PARAMETER( ZERO=0.D0, ONE=1.D0 )
C
      CALL ZIBCONST(EPMACH,SMALL)
      SMALL = DSQRT(EPMACH*1.D1)
C
      IF(IRANK.GT.N) IRANK=N
      IF(IRANK.GT.M) IRANK=M
C
C---1.0 SPECIAL CASE M=1 AND N=1
C
      IF(M.EQ.1 .AND. N.EQ.1) THEN
         PIVOT(1)=1
         D(1)=A(1,1)
         COND=1.D0
         RETURN
      END IF
C
C---1.1 INITIALIZE PIVOT-ARRAY
c
      IF  (KRED.GE.0)  THEN
         DO J=1,N
            PIVOT(J) = J
         ENDDO
C
C---2. CONSTRAINED HOUSEHOLDER TRIANGULARIZATION
C
         JD = 1
         ISUB = 1
         MH = MCON
         IF (MH.EQ.0) MH=M
         K1 = 1

2000     continue

         K = K1

         IF (K.NE.N)  THEN

            K1 = K+1

2100        continue

            IF (JD.NE.0)  THEN
               DO J=K,N
                  S = ZERO
                  DO I=K,MH
                     S = S+A(I,J)*A(I,J)
                  end do
                  D(J) = S
               ENDDO
            ENDIF
C
C---2.1     COLUMN PIVOTING
c
            H = D(K)
            JJ = K
            DO J=K1,N
               IF (D(J).GT.H)  THEN
                  H = D(J)
                  JJ = J
               ENDIF
            ENDDO
            IF (JD.EQ.1)  HMAX = H * SMALL
            JD = 0
            IF (H.LT.HMAX)  THEN
               JD = 1
               GOTO 2100
            ENDIF
            IF (JJ.NE.K)  THEN
C
C---2.2        COLUMN INTERCHANGE
c
               I = PIVOT(K)
               PIVOT(K) = PIVOT(JJ)
               PIVOT(JJ) = I
               D(JJ) = D(K)
               DO I=1,M
                  T = A(I,K)
                  A(I,K) = A(I,JJ)
                  A(I,JJ) = T
               ENDDO
            ENDIF
         ENDIF

         H = ZERO
         DO I=K,MH
            H = H+A(I,K)*A(I,K)
         ENDDO
         T = DSQRT(H)
C
C---2.3.0  A PRIORI TEST ON PSEUDO-RANK
C
         IF (ISUB.GT.0) DD = T/COND
         ISUB = 0
         IF (T.LE.DD) THEN
C
C---2.3.1 RANK REDUCTION
C
c  Constraints are linearly dependent:
c
            IF (K.LE.MCON) THEN
               MCON = K-1
               K1 = K
               MH = M
               JD = 1
               ISUB = 1
               GOTO 2000
            ENDIF

            IRANK = K - 1
            IF (IRANK.EQ.0)  THEN
               GOTO 4000
            ELSE
               GOTO 3000
            ENDIF
         ENDIF

         S = A(K,K)
         IF (S.GT.ZERO) T = -T
         D(K) = T
         A(K,K) = S-T
         IF (K.EQ.N)  GOTO 4000

         T = ONE/(H-S*T)
         DO J=K1,N
            S = ZERO
            DO I=K,MH
               S = S+A(I,K)*A(I,J)
            ENDDO
            S = S*T
            DO I=K,M
               A(I,J) = A(I,J)-A(I,K)*S
            ENDDO
            D(J) = D(J)-A(K,J)*A(K,J)
         ENDDO

         IF (K.EQ.IRANK) GOTO 3000
         IF (K.EQ.MCON) THEN
            MH = M
            JD = 1
            ISUB = 1
         ENDIF
         GOTO 2000
      ENDIF
C
C---3. RANK-DEFICIENT PSEUDO-INVERSE
C
3000  continue

      IRK1 = IRANK+1

      DO J=IRK1,N

         DO II=1,IRANK
            I = IRK1-II
            S = A(I,J)
            IF (II.NE.1)  THEN
               DO JJ=I1,IRANK
                  S = S-A(I,JJ)*V(JJ)
               ENDDO
            ENDIF
            I1 = I
            V(I) = S/D(I)
            AH(I,J) = V(I)
         ENDDO

         DO I=IRK1,J
            S = ZERO
            I1 = I-1
            DO JJ=1,I1
               S = S+AH(JJ,I)*V(JJ)
            ENDDO
            IF (I.NE.J)  THEN
               V(I) = -S/D(I)
               AH(I,J) = -V(I)
            ENDIF
         ENDDO

         D(J) = DSQRT(S+ONE)
      ENDDO
C
C---4.  EXIT
C
4000  continue

      IF (K.EQ.IRANK) then
        T=D(IRANK)
      end if

      IF (T.NE.0.D0) then
        COND=DABS(D(1)/T)
      end if

      RETURN
      END
      SUBROUTINE BLSOLC (A,NROW,NCOL,MCON,M,N,X,B,IRANK,D,
     &                   PIVOT,KRED,AH,V)

c*********************************************************************72
c
cc BLSOLC performs a best constrained lienar least squares solution.
c
c  Modified:
c
c    08 January 2013
C
C     BEST CONSTRAINED LINEAR LEAST SQUARES SOLUTION OF (M,N)-SYSTEM
C     FIRST MCON ROWS COMPRISE MCON EQUALITY CONSTRAINTS
C
C *********************************************************************
C
C     TO BE USED IN CONNECTION WITH SUBROUTINE DECCON
C
C     RESEARCH CODE FOR GENERAL (M,N)-MATRICES     V 19.01.1984
C
C     INPUT PARAMETERS (* MARKS INOUT PARAMETERS)
C     -----------------------------------------------
C
C        A(M,N)      SEE OUTPUT OF DECCON
C        NROW        SEE OUTPUT OF DECCON
C        NCOL        SEE OUTPUT OF DECCON
C        M           SEE OUTPUT OF DECCON
C        N           SEE OUTPUT OF DECCON
C        MCON        SEE OUTPUT OF DECCON
C        IRANK       SEE OUTPUT OF DECCON
C        D(N)        SEE OUTPUT OF DECCON
C        PIVOT(N)    SEE OUTPUT OF DECCON
C        AH(N,N)     SEE OUTPUT OF DECCON
C        KRED        SEE OUTPUT OF DECCON
C      * B(M)        RIGHT-HAND SIDE OF LINEAR SYSTEM, IF (KRED.GE.0)
C                    RIGHT-HAND SIDE OF UPPER LINEAR SYSTEM,
C                                                      IF (KRED.LT.0)
C        V(N)        REAL WORK ARRAY
C
C     OUTPUT PARAMETERS
C     -----------------
C
C        X(N)        BEST LSQ-SOLUTION OF LINEAR SYSTEM
C        B(M)        RIGHT-HAND OF UPPER TRIGULAR SYSTEM
C                    (TRANSFORMED RIGHT-HAND SIDE OF LINEAR SYSTEM)
C
C
      INTEGER  I, II, I1, IH, IRK1, J, JJ, J1, MH
      INTEGER  IRANK, KRED, M, MCON, N, NROW, NCOL, PIVOT(N)
      DOUBLE PRECISION A(NROW,NCOL), AH(NCOL,NCOL)
      DOUBLE PRECISION B(M), D(N), V(N), X(N), S, ZERO
C
C     COMMON /MACHIN/ EPMACH, SMALL
C
C
      PARAMETER( ZERO=0.D0 )
C
C---1. SOLUTION FOR PSEUDO-RANK ZERO
C
      IF (IRANK.EQ.0)  THEN
         DO I=1,N
            X(I) = ZERO
         ENDDO
         RETURN
      ENDIF
C
      IF (KRED.GE.0 .AND. (M.NE.1 .OR. N.NE.1) ) THEN
C
C---2. CONSTRAINED HOUSEHOLDER TRANSFORMATIONS OF RIGHT-HAND SIDE
C
         MH = MCON
         IF (MH.EQ.0)  MH = M
         DO  2100 J=1,IRANK
            S = ZERO
            DO I=J,MH
               S = S+A(I,J)*B(I)
            ENDDO
            S = S/(D(J)*A(J,J))
            DO I=J,M
               B(I) = B(I)+A(I,J)*S
            end do
            IF (J.EQ.MCON)  MH = M
2100     CONTINUE
C        ENDDO
      ENDIF
C
C---3.1  SOLUTION OF UPPER TRIANGULAR SYSTEM
C
      IRK1 = IRANK+1
      DO  3100 II=1,IRANK
         I = IRK1-II
         I1 = I + 1
         S = B(I)
         IF (I1.LE.IRANK)  THEN
            DO JJ=I1,IRANK
               S = S-A(I,JJ)*V(JJ)
            ENDDO
         ENDIF
3100     V(I) = S/D(I)
C     ENDDO
      IF (IRK1.LE.N) THEN
C
C---3.2  COMPUTATION OF THE BEST CONSTRAINED LSQ-SOLUTION
C
         DO J=IRK1,N
            S = ZERO
            J1 = J-1
            DO I=1,J1
               S = S+AH(I,J)*V(I)
            ENDDO
            V(J) = -S/D(J)
         ENDDO

         DO JJ=1,N
            J = N-JJ+1
            S = ZERO
            IF (JJ.NE.1) THEN
               DO I=J1,N
                  S = S+AH(J,I)*V(I)
               ENDDO
               IF (J.LE.IRANK) THEN
                  V(J) = V(J)-S
                  GOTO 3220
               ENDIF
            ENDIF
            J1=J
            V(J)=-(V(J)+S)/D(J)
3220        CONTINUE
         ENDDO
      ENDIF
C
C---4. BACK-PERMUTATION OF SOLUTION COMPONENTS
C
      DO J=1,N
         IH=PIVOT(J)
         X(IH) = V(J)
      ENDDO
      RETURN
      END
      SUBROUTINE MA28AD(N, NZ, A, LICN, IRN, LIRN, ICN, U, IKEEP, IW, W,
     * IFLAG)

c*********************************************************************72
c
cc MA28AD performs the LU factorization of A.
c
c  Modified:
c
c    08 January 2013
c
c the parameters are as follows.....
c n     order of matrix  not altered by subroutine.
c nz    number of non-zeros in input matrix  not altered by subroutine.
c a is a  real array  length licn.  holds non-zeros of matrix on entry
c     and non-zeros of factors on exit.  reordered by mc20a/ad and
c     mc23a/ad and altered by ma30a/ad.
c licn  integer  length of arrays a and icn.  not altered by subroutine.
c irn   integer array of length lirn.  holds row indices on input.
c     used as workspace by ma30a/ad to hold column orientation of
c     matrix.
c lirn  integer  length of array irn. not altered by the subroutine.
c icn   integer array of length licn.  holds column indices on entry
c     and column indices of decomposed matrix on exit. reordered by
c     mc20a/ad and mc23a/ad and altered by ma30a/ad.
c u     real variable  set by user to control bias towards numeric or
c     sparsity pivoting.  u=1.0 gives partial pivoting while u=0. does
c     not check multipliers at all.  values of u greater than one are
c     treated as one while negative values are treated as zero.  not
c     altered by subroutine.
c ikeep  integer array of length 5*n  used as workspace by ma28a/ad
c     (see later comments).  it is not required to be set on entry
c     and, on exit, it contains information about the decomposition.
c     it should be preserved between this call and subsequent calls
c     to ma28b/bd or ma28c/cd.
c     ikeep(i,1),i=1,n  holds the total length of the part of row i
c     in the diagonal block.
c     row ikeep(i,2),i=1,n  of the input matrix is the ith row in
c     pivot order.
c     column ikeep(i,3),i=1,n  of the input matrix is the ith column
c     in pivot order.
c     ikeep(i,4),i=1,n  holds the length of the part of row i in
c     the l part of the l/u decomposition.
c     ikeep(i,5),i=1,n  holds the length of the part of row i in the
c     off-diagonal blocks.  if there is only one diagonal block,
c     ikeep(1,5) will be set to -1.
c iw    integer array of length 8*n.  if the option nsrch.le.n is
c     used, then the length of array iw can be reduced to 7*n.
c w real array  length n.  used by mc24a/ad both as workspace and to
c     return growth estimate in w(1).  the use of this array by ma28a/ad
c     is thus optional depending on common block logical variable grow.
c iflag  integer variable  used as error flag by routine.  a positive
c     or zero value on exit indicates success.  possible negative
c     values are -1 through -14.
c
      INTEGER N, NZ, LICN, LIRN, IFLAG
      INTEGER IRN(LIRN), ICN(LICN), IKEEP(N,5), IW(N,8)
      DOUBLE PRECISION A(LICN), U, W(N)
c
c common and private variables.
c     common block ma28f/fd is used merely
c     to communicate with common block ma30f/fd  so that the user
c     need not declare this common block in his main program.
c the common block variables are as follows ...
c lp,mp  integer  default value 6 (line printer).  unit number
c     for error messages and duplicate element warning resp.
c nlp,mlp  integer  unit number for messages from ma30a/ad and
c     mc23a/ad resp.  set by ma28a/ad to value of lp.
c lblock  logical  default value true.  if true mc23a/ad is used
c     to first permute the matrix to block lower triangular form.
c grow    logical  default value true.  if true then an estimate
c     of the increase in size of matrix elements during l/u
c     decomposition is given by mc24a/ad.
c eps,rmin,resid  real/double precision variables not referenced
c     by ma28a/ad.
c irncp,icncp  integer  set to number of compresses on arrays irn and
c     icn/a respectively.
c minirn,minicn  integer  minimum length of arrays irn and icn/a
c     respectively, for success on future runs.
c irank  integer   estimated rank of matrix.
c mirncp,micncp,mirank,mirn,micn integer variables.  used to
c     communicate between ma30f/fd and ma28f/fd values of abovenamed
c     variables with somewhat similar names.
c abort1,abort2  logical variables with default value true.  if false
c     then decomposition will be performed even if the matrix is
c     structurally or numerically singular respectively.
c aborta,abortb  logical variables used to communicate values of
c     abort1 and abort2 to ma30a/ad.
c abort  logical  used to communicate value of abort1 to mc23a/ad.
c abort3  logical variable not referenced by ma28a/ad.
c idisp   integer array  length 2.  used to communicate information
c     on decomposition between this call to ma28a/ad and subsequent
c     calls to ma28b/bd and ma28c/cd.  on exit, idisp(1) and
c     idisp(2) indicate position in arrays a and icn of the
c     first and last elements in the l/u decomposition of the
c     diagonal blocks, respectively.
c numnz  integer  structural rank of matrix.
c num    integer  number of diagonal blocks.
c large  integer  size of largest diagonal block.
c
c see block data for further comments on common block variables.
c see code for comments on private variables.
c
      DOUBLE PRECISION TOL, THEMAX, BIG, DXMAX, ERRMAX, DRES, CGCE,
     * TOL1, BIG1, UPRIV, RMIN, EPS, RESID, ZERO
      INTEGER IDISP(2)
      LOGICAL GROW, LBLOCK, ABORT, ABORT1, ABORT2, ABORT3, ABORTA,
     * ABORTB, LBIG, LBIG1
      COMMON /MA28ED/ LP, MP, LBLOCK, GROW
      COMMON /MA28FD/ EPS, RMIN, RESID, IRNCP, ICNCP, MINIRN, MINICN,
     * IRANK, ABORT1, ABORT2
      COMMON /MA28GD/ IDISP
      COMMON /MA28HD/ TOL, THEMAX, BIG, DXMAX, ERRMAX, DRES, CGCE,
     * NDROP, MAXIT, NOITER, NSRCH, ISTART, LBIG
      COMMON /MA30ID/ TOL1, BIG1, NDROP1, NSRCH1, LBIG1
      COMMON /MA30ED/ NLP, ABORTA, ABORTB, ABORT3
      COMMON /MA30FD/ MIRNCP, MICNCP, MIRANK, MIRN, MICN
      COMMON /MC23BD/ MLP, NUMNZ, NUM, LARGE, ABORT
      COMMON /LPIVOT/ LPIV(10),LNPIV(10),MAPIV,MANPIV,IAVPIV,
     *                IANPIV,KOUNTL
c
c some  initialization and transfer of information between
c     common blocks (see earlier comments).
      DATA ZERO /0.0D0/
      IFLAG = 0
      ABORTA = ABORT1
      ABORTB = ABORT2
      ABORT = ABORT1
      MLP = LP
      NLP = LP
      TOL1 = TOL
      LBIG1 = LBIG
      NSRCH1 = NSRCH
c upriv private copy of u is used in case it is outside
c     range  zero to one  and  is thus altered by ma30a/ad.
      UPRIV = U
c simple data check on input variables and array dimensions.
      IF (N.GT.0) GO TO 10
      IFLAG = -8
      IF (LP.NE.0) WRITE (LP,99999) N
      GO TO 210
   10 IF (NZ.GT.0) GO TO 20
      IFLAG = -9
      IF (LP.NE.0) WRITE (LP,99998) NZ
      GO TO 210
   20 IF (LICN.GE.NZ) GO TO 30
      IFLAG = -10
      IF (LP.NE.0) WRITE (LP,99997) LICN
      GO TO 210
   30 IF (LIRN.GE.NZ) GO TO 40
      IFLAG = -11
      IF (LP.NE.0) WRITE (LP,99996) LIRN
      GO TO 210
c
c data check to see if all indices lie between 1 and n.
   40 DO 50 I=1,NZ
        IF (IRN(I).GT.0 .AND. IRN(I).LE.N .AND. ICN(I).GT.0 .AND.
     *   ICN(I).LE.N) GO TO 50
        IF (IFLAG.EQ.0 .AND. LP.NE.0) WRITE (LP,99995)
        IFLAG = -12
        IF (LP.NE.0) WRITE (LP,99994) I, A(I), IRN(I), ICN(I)
   50 CONTINUE
      IF (IFLAG.LT.0) GO TO 220
c
c sort matrix into row order.
      CALL MC20AD(N, NZ, A, ICN, IW, IRN, 0)
c part of ikeep is used here as a work-array.  ikeep(i,2) is
c     the last row to have a non-zero in column i.  ikeep(i,3)
c     is the off-set of column i from the start of the row.
      DO 60 I=1,N
        IKEEP(I,2) = 0
        IKEEP(I,1) = 0
   60 CONTINUE
c
c check for duplicate elements .. summing any such entries and
c     printing a warning message on unit mp.
c move is equal to the number of duplicate elements found.
      MOVE = 0
c the loop also calculates the largest element in the matrix, themax.
      THEMAX = ZERO
c j1 is position in arrays of first non-zero in row.
      J1 = IW(1,1)
      DO 130 I=1,N
        IEND = NZ + 1
        IF (I.NE.N) IEND = IW(I+1,1)
        LENGTH = IEND - J1
        IF (LENGTH.EQ.0) GO TO 130
        J2 = IEND - 1
        NEWJ1 = J1 - MOVE
        DO 120 JJ=J1,J2
          J = ICN(JJ)
          THEMAX = DMAX1(THEMAX,DABS(A(JJ)))
          IF (IKEEP(J,2).EQ.I) GO TO 110
c first time column has ocurred in current row.
          IKEEP(J,2) = I
          IKEEP(J,3) = JJ - MOVE - NEWJ1
          IF (MOVE.EQ.0) GO TO 120
c shift necessary because of  previous duplicate element.
          NEWPOS = JJ - MOVE
          A(NEWPOS) = A(JJ)
          ICN(NEWPOS) = ICN(JJ)
          GO TO 120
c duplicate element.
  110     MOVE = MOVE + 1
          LENGTH = LENGTH - 1
          JAY = IKEEP(J,3) + NEWJ1
          IF (MP.NE.0) WRITE (MP,99993) I, J, A(JJ)
          A(JAY) = A(JAY) + A(JJ)
          THEMAX = DMAX1(THEMAX,DABS(A(JAY)))
  120   CONTINUE
        IKEEP(I,1) = LENGTH
        J1 = IEND
  130 CONTINUE
c
c knum is actual number of non-zeros in matrix with any multiple
c     entries counted only once.
      KNUM = NZ - MOVE
      IF (.NOT.LBLOCK) GO TO 140
c
c perform block triangularisation.
      CALL MC23AD(N, ICN, A, LICN, IKEEP, IDISP, IKEEP(1,2),
     *IKEEP(1,3), IKEEP(1,5), IW(1,3), IW)
      IF (IDISP(1).GT.0) GO TO 170
      IFLAG = -7
      IF (IDISP(1).EQ.-1) IFLAG = -1
      IF (LP.NE.0) WRITE (LP,99992)
      GO TO 210
c
c block triangularization not requested.
c move structure to end of data arrays in preparation for
c     ma30a/ad.
c also set lenoff(1) to -1 and set permutation arrays.
  140 DO 150 I=1,KNUM
        II = KNUM - I + 1
        NEWPOS = LICN - I + 1
        ICN(NEWPOS) = ICN(II)
        A(NEWPOS) = A(II)
  150 CONTINUE
      IDISP(1) = 1
      IDISP(2) = LICN - KNUM + 1
      DO 160 I=1,N
        IKEEP(I,2) = I
        IKEEP(I,3) = I
  160 CONTINUE
      IKEEP(1,5) = -1
  170 IF (LBIG) BIG1 = THEMAX
      IF (NSRCH.LE.N) GO TO 180
c
c perform l/u decomosition on diagonal blocks.
      CALL MA30AD(N, ICN, A, LICN, IKEEP, IKEEP(1,4), IDISP,
     *IKEEP(1,2), IKEEP(1,3), IRN, LIRN, IW(1,2), IW(1,3), IW(1,4),
     *IW(1,5), IW(1,6), IW(1,7), IW(1,8), IW, UPRIV, IFLAG)
      GO TO 190
c this call if used if nsrch has been set less than or equal n.
c     in this case, two integer work arrays of length can be saved.
  180 CALL MA30AD(N, ICN, A, LICN, IKEEP, IKEEP(1,4), IDISP,
     * IKEEP(1,2), IKEEP(1,3), IRN, LIRN, IW(1,2), IW(1,3), IW(1,4),
     * IW(1,5), IW, IW, IW(1,6), IW, UPRIV, IFLAG)
c
c transfer common block information.
  190 MINIRN = MAX0(MIRN,NZ)
      MINICN = MAX0(MICN,NZ)
      IRNCP = MIRNCP
      ICNCP = MICNCP
      IRANK = MIRANK
      NDROP = NDROP1
      IF (LBIG) BIG = BIG1
      IF (IFLAG.GE.0) GO TO 200
      IF (LP.NE.0) WRITE (LP,99991)
      GO TO 210
c
c reorder off-diagonal blocks according to pivot permutation.
  200 I1 = IDISP(1) - 1
      IF (I1.NE.0) CALL MC22AD(N, ICN, A, I1, IKEEP(1,5), IKEEP(1,2),
     * IKEEP(1,3), IW, IRN)
      I1 = IDISP(1)
      IEND = LICN - I1 + 1
c
c optionally calculate element growth estimate.
      IF (GROW) CALL MC24AD(N, ICN, A(I1), IEND, IKEEP, IKEEP(1,4), W)
c increment growth estimate by original maximum element.
      IF (GROW) W(1) = W(1) + THEMAX
      IF (GROW .AND. N.GT.1) W(2) = THEMAX
c set flag if the only error is due to duplicate elements.
      IF (IFLAG.GE.0 .AND. MOVE.NE.0) IFLAG = -14
      GO TO 220
  210 IF (LP.NE.0) WRITE (LP,99990)
  220 RETURN
99999 FORMAT (36X, 'N OUT OF RANGE = ', I10)
99998 FORMAT (36X, 'NZ NON POSITIVE = ', I10)
99997 FORMAT (36X, 'LICN TOO SMALL = ', I10)
99996 FORMAT (36X, 'LIRN TOO SMALL = ', I10)
99995 FORMAT (' ERROR RETURN FROM MA28A/AD BECAUSE INDICES FOUND OUT ',
     * 'OF RANGE')
99994 FORMAT (1X, I6, 'TH ELEMENT WITH VALUE ', 1PD22.14, ' IS OUT O',
     * 'F RANGE WITH INDICES ', I8, ' ,', I8)
99993 FORMAT (' DUPLICATE ELEMENT IN POSITION ', I8, ' ,', I8,
     * ' WITH VALUE ', 1PD22.14)
99992 FORMAT (36X, 'ERROR RETURN FROM MC23A/AD')
99991 FORMAT (36X, 'ERROR RETURN FROM MA30A/AD')
99990 FORMAT (' ERROR RETURN FROM MA28A/AD BECAUSE ')
      END
      SUBROUTINE MA28BD(N, NZ, A, LICN, IVECT, JVECT, ICN, IKEEP, IW, W,
     * IFLAG)

c*********************************************************************72
c
cc MA28BD factorizes a matrix of known sparsity pattern.
c
c  Discussion:
c
c    The new matrix is presumed to have a similar sparsity
c    pattern to that previously factorized by ma28a/ad.
c
c  Modified:
c
c    08 January 2013
c
c the parameters are as follows ...
c n      integer  order of matrix  not altered by subroutine.
c nz     integer  number of non-zeros in input matrix  not altered
c     by subroutine.
c a      real/double precision array  length licn.  holds non-zeros of
c     matrix on entry and non-zeros of factors on exit.  reordered by
c     ma28d/dd and altered by subroutine ma30b/bd.
c licn   integer  length of arrays a and icn.  not altered by
c     subroutine.
c ivect,jvect  integer arrays of length nz.  hold row and column
c     indices of non-zeros respectively.  not altered by subroutine.
c icn    integer array of length licn.  same array as output from
c     ma28a/ad.  unchanged by ma28b/bd.
c ikeep  integer array of length 5*n.  same array as output from
c     ma28a/ad.  unchanged by ma28b/bd.
c iw     integer array  length 5*n.  used as workspace by ma28d/dd and
c     ma30b/bd.
c w      real/double precision array  length n.  used as workspace
c     by ma28d/dd,ma30b/bd and (optionally) mc24a/ad.
c iflag  integer  used as error flag with positive or zero value
c     indicating success.
c
      INTEGER N, NZ, LICN, IW(N,5), IFLAG
      INTEGER IKEEP(N,5), IVECT(NZ), JVECT(NZ), ICN(LICN)
      DOUBLE PRECISION A(LICN), W(N)
c
c private and common variables.
c unless otherwise stated common block variables are as in ma28a/ad.
c     those variables referenced by ma28b/bd are mentioned below.
c lp,mp  integers  used as in ma28a/ad as unit number for error and
c     warning messages, respectively.
c nlp    integer variable used to give value of lp to ma30e/ed.
c eps    real/double precision  ma30b/bd will output a positive value
c     for iflag if any modulus of the ratio of pivot element to the
c     largest element in its row (u part only) is less than eps (unless
c     eps is greater than 1.0 when no action takes place).
c rmin   real/double precision  variable equal to the value of this
c     minimum ratio in cases where eps is less than or equal to 1.0.
c meps,mrmin  real/double precision variables used by the subroutine
c     to communicate between common blocks ma28f/fd and ma30g/gd.
c idisp  integer array  length 2  the same as that used by ma28a/ad.
c     it is unchanged by ma28b/bd.
c
c see block data or ma28a/ad for further comments on variables
c     in common.
c see code for comments on private variables.
c
      LOGICAL GROW, LBLOCK, ABORTA, ABORTB, ABORT1, ABORT2, ABORT3,
     * LBIG, LBIG1
      INTEGER IDISP(2)
      DOUBLE PRECISION EPS, MEPS, RMIN, MRMIN, RESID, TOL,
     * THEMAX, BIG, DXMAX, ERRMAX, DRES, CGCE, TOL1, BIG1
c
      COMMON /MA28ED/ MP, LP, LBLOCK, GROW
      COMMON /MA28FD/ EPS, RMIN, RESID, IRNCP, ICNCP, MINIRN, MINICN,
     * IRANK, ABORT1, ABORT2
      COMMON /MA28GD/ IDISP
      COMMON /MA28HD/ TOL, THEMAX, BIG, DXMAX, ERRMAX, DRES, CGCE,
     * NDROP, MAXIT, NOITER, NSRCH, ISTART, LBIG
      COMMON /MA30ED/ NLP, ABORTA, ABORTB, ABORT3
      COMMON /MA30GD/ MEPS, MRMIN
      COMMON /MA30ID/ TOL1, BIG1, NDROP1, NSRCH1, LBIG1
c
c check to see if elements were dropped in previous ma28a/ad call.
      IF (NDROP.EQ.0) GO TO 10
      IFLAG = -15
      WRITE (6,99999) IFLAG, NDROP
      GO TO 70
   10 IFLAG = 0
      MEPS = EPS
      NLP = LP
c simple data check on variables.
      IF (N.GT.0) GO TO 20
      IFLAG = -11
      IF (LP.NE.0) WRITE (LP,99998) N
      GO TO 60
   20 IF (NZ.GT.0) GO TO 30
      IFLAG = -10
      IF (LP.NE.0) WRITE (LP,99997) NZ
      GO TO 60
   30 IF (LICN.GE.NZ) GO TO 40
      IFLAG = -9
      IF (LP.NE.0) WRITE (LP,99996) LICN
      GO TO 60
c
   40 CALL MA28DD(N, A, LICN, IVECT, JVECT, NZ, ICN, IKEEP, IKEEP(1,4),
     * IKEEP(1,5), IKEEP(1,2), IKEEP(1,3), IW(1,3), IW, W(1), IFLAG)
c themax is largest element in matrix.
      THEMAX = W(1)
      IF (LBIG) BIG1 = THEMAX
c idup equals one if there were duplicate elements, zero otherwise.
      IDUP = 0
      IF (IFLAG.EQ.(N+1)) IDUP = 1
      IF (IFLAG.LT.0) GO TO 60
c
c perform row-gauss elimination on the structure received from ma28d/dd
      CALL MA30BD(N, ICN, A, LICN, IKEEP, IKEEP(1,4), IDISP,
     * IKEEP(1,2), IKEEP(1,3), W, IW, IFLAG)
c
c transfer common block information.
      IF (LBIG) BIG1 = BIG
      RMIN = MRMIN
      IF (IFLAG.GE.0) GO TO 50
      IFLAG = -2
      IF (LP.NE.0) WRITE (LP,99995)
      GO TO 60
c
c optionally calculate the growth parameter.
   50 I1 = IDISP(1)
      IEND = LICN - I1 + 1
      IF (GROW) CALL MC24AD(N, ICN, A(I1), IEND, IKEEP, IKEEP(1,4), W)
c increment estimate by largest element in input matrix.
      IF (GROW) W(1) = W(1) + THEMAX
      IF (GROW .AND. N.GT.1) W(2) = THEMAX
c set flag if the only error is due to duplicate elements.
      IF (IDUP.EQ.1 .AND. IFLAG.GE.0) IFLAG = -14
      GO TO 70
   60 IF (LP.NE.0) WRITE (LP,99994)
   70 RETURN
99999 FORMAT (39H ERROR RETURN FROM MA28B/BD WITH IFLAG=, I4/I7, 4H ENT,
     * 39HRIES DROPPED FROM STRUCTURE BY MA28A/AD)
99998 FORMAT (36X, 17HN OUT OF RANGE = , I10)
99997 FORMAT (36X, 18HNZ NON POSITIVE = , I10)
99996 FORMAT (36X, 17HLICN TOO SMALL = , I10)
99995 FORMAT (36X, 26HERROR RETURN FROM MA30B/BD)
99994 FORMAT (36H+ERROR RETURN FROM MA28B/BD BECAUSE )
      END
      SUBROUTINE MA28CD(N, A, LICN, ICN, IKEEP, RHS, W, MTYPE)

c*********************************************************************72
c
cc MA28CD solves a system factored by MA28AD or MA28BD.
c
c  Discussion:
c
c    The routine uses the factors from ma28a/ad or ma28b/bd to
c    solve a system of equations without iterative refinement.
c
c  Modified:
c
c    08 January 2013
c
c the parameters are ...
c n   integer  order of matrix  not altered by subroutine.
c a   real/double precision array  length licn.  the same array as
c     was used in the most recent call to ma28a/ad or ma28b/bd.
c licn  integer  length of arrays a and icn.  not altered by
c     subroutine.
c icn    integer array of length licn.  same array as output from
c     ma28a/ad.  unchanged by ma28c/cd.
c ikeep  integer array of length 5*n.  same array as output from
c     ma28a/ad.  unchanged by ma28c/cd.
c rhs    real/double precision array  length n.  on entry, it holds the
c     right hand side.  on exit, the solution vector.
c w      real/double precision array  length n. used as workspace by
c     ma30c/cd.
c mtype  integer  used to tell ma30c/cd to solve the direct equation
c     (mtype=1) or its transpose (mtype.ne.1).
c
      DOUBLE PRECISION A(LICN), RHS(N), W(N), RESID, MRESID, EPS, RMIN
      INTEGER IDISP(2)
      INTEGER ICN(LICN), IKEEP(N,5)
      LOGICAL ABORT1, ABORT2
c common block variables.
c unless otherwise stated common block variables are as in ma28a/ad.
c     those variables referenced by ma28c/cd are mentioned below.
c resid  real/double precision  variable returns maximum residual of
c     equations where pivot was zero.
c mresid  real/double precision variable used by ma28c/cd to
c     communicate between ma28f/fd and ma30h/hd.
c idisp  integer array  length 2  the same as that used by ma28a/ad.
c     it is unchanged by ma28b/bd.
c
c further information on common block variables can be found in block
c     data or ma28a/ad.
      COMMON /MA28FD/ EPS, RMIN, RESID, IRNCP, ICNCP, MINIRN, MINICN,
     * IRANK, ABORT1, ABORT2
      COMMON /MA28GD/ IDISP
      COMMON /MA30HD/ MRESID
c
c this call performs the solution of the set of equations.
      CALL MA30CD(N, ICN, A, LICN, IKEEP, IKEEP(1,4), IKEEP(1,5),
     * IDISP, IKEEP(1,2), IKEEP(1,3), RHS, W, MTYPE)
c transfer common block information.
      RESID = MRESID
      RETURN
      END
      SUBROUTINE MA28DD(N, A, LICN, IVECT, JVECT, NZ, ICN, LENR, LENRL,
     * LENOFF, IP, IQ, IW1, IW, W1, IFLAG)

c*********************************************************************72
c
cc MA28DD sorts the user's matrix into the structure for the decomposed form.
c
c  Discussion:
c
c    This routine need never be called by the user directly.
c
c    it sorts the user's matrix into the structure of the decomposed
c    form and checks for the presence of duplicate entries or
c    non-zeros lying outside the sparsity pattern of the decomposition
c    it also calculates the largest element in the input matrix.
c
c  Modified:
c
c    08 January 2013
c
      DOUBLE PRECISION A(LICN), ZERO, W1, AA
      INTEGER IW(N,2), IDISP(2)
      INTEGER ICN(LICN), IVECT(NZ), JVECT(NZ), IP(N), IQ(N),
     * LENR(N), IW1(N,3), LENRL(N), LENOFF(N)
      LOGICAL LBLOCK, GROW, BLOCKL
      COMMON /MA28ED/ LP, MP, LBLOCK, GROW
      COMMON /MA28GD/ IDISP
      DATA ZERO /0.0D0/
      BLOCKL = LENOFF(1).GE.0
c iw1(i,3)  is set to the block in which row i lies and the
c     inverse permutations to ip and iq are set in iw1(.,1) and
c     iw1(.,2) resp.
c pointers to beginning of the part of row i in diagonal and
c   off-diagonal blocks are set in iw(i,2) and iw(i,1) resp.
      IBLOCK = 1
      IW(1,1) = 1
      IW(1,2) = IDISP(1)
      DO 10 I=1,N
        IW1(I,3) = IBLOCK
        IF (IP(I).LT.0) IBLOCK = IBLOCK + 1
        II = IABS(IP(I)+0)
        IW1(II,1) = I
        JJ = IQ(I)
        JJ = IABS(JJ)
        IW1(JJ,2) = I
        IF (I.EQ.1) GO TO 10
        IF (BLOCKL) IW(I,1) = IW(I-1,1) + LENOFF(I-1)
        IW(I,2) = IW(I-1,2) + LENR(I-1)
   10 CONTINUE
c place each non-zero in turn into its correct location
c    in the a/icn array.
      IDISP2 = IDISP(2)
      DO 170 I=1,NZ
c necessary to avoid reference to unassigned element of icn.
        IF (I.GT.IDISP2) GO TO 20
        IF (ICN(I).LT.0) GO TO 170
   20   IOLD = IVECT(I)
        JOLD = JVECT(I)
        AA = A(I)
c this is a dummy loop for following a chain of interchanges.
c   it will be executed nz times in total.
        DO 140 IDUMMY=1,NZ
c perform some validity checks on iold and jold.
          IF (IOLD.LE.N .AND. IOLD.GT.0 .AND. JOLD.LE.N .AND.
     *     JOLD.GT.0) GO TO 30
          IF (LP.NE.0) WRITE (LP,99999) I, A(I), IOLD, JOLD
          IFLAG = -12
          GO TO 180
   30     INEW = IW1(IOLD,1)
          JNEW = IW1(JOLD,2)
c are we in a valid block and is it diagonal or off-diagonal?
          IF (IW1(INEW,3)-IW1(JNEW,3)) 40, 60, 50
   40     IFLAG = -13
          IF (LP.NE.0) WRITE (LP,99998) IOLD, JOLD
          GO TO 180
   50     J1 = IW(INEW,1)
          J2 = J1 + LENOFF(INEW) - 1
          GO TO 110
c element is in diagonal block.
   60     J1 = IW(INEW,2)
          IF (INEW.GT.JNEW) GO TO 70
          J2 = J1 + LENR(INEW) - 1
          J1 = J1 + LENRL(INEW)
          GO TO 110
   70     J2 = J1 + LENRL(INEW)
c binary search of ordered list  .. element in l part of row.
          DO 100 JDUMMY=1,N
            MIDPT = (J1+J2)/2
            JCOMP = IABS(ICN(MIDPT)+0)
            IF (JNEW-JCOMP) 80, 130, 90
   80       J2 = MIDPT
            GO TO 100
   90       J1 = MIDPT
  100     CONTINUE
          IFLAG = -13
          IF (LP.NE.0) WRITE (LP,99997) IOLD, JOLD
          GO TO 180
c linear search ... element in l part of row or off-diagonal blocks.
  110     DO 120 MIDPT=J1,J2
            IF (IABS(ICN(MIDPT)+0).EQ.JNEW) GO TO 130
  120     CONTINUE
          IFLAG = -13
          IF (LP.NE.0) WRITE (LP,99997) IOLD, JOLD
          GO TO 180
c equivalent element of icn is in position midpt.
  130     IF (ICN(MIDPT).LT.0) GO TO 160
          IF (MIDPT.GT.NZ .OR. MIDPT.LE.I) GO TO 150
          W1 = A(MIDPT)
          A(MIDPT) = AA
          AA = W1
          IOLD = IVECT(MIDPT)
          JOLD = JVECT(MIDPT)
          ICN(MIDPT) = -ICN(MIDPT)
  140   CONTINUE
  150   A(MIDPT) = AA
        ICN(MIDPT) = -ICN(MIDPT)
        GO TO 170
  160   A(MIDPT) = A(MIDPT) + AA
c set flag for duplicate elements.
        IFLAG = N + 1
  170 CONTINUE
c reset icn array  and zero elements in l/u but not in a.
c also calculate maximum element of a.
  180 W1 = ZERO
      DO 200 I=1,IDISP2
        IF (ICN(I).LT.0) GO TO 190
        A(I) = ZERO
        GO TO 200
  190   ICN(I) = -ICN(I)
        W1 = DMAX1(W1,DABS(A(I)))
  200 CONTINUE
      RETURN
99999 FORMAT (' ELEMENT ', I6, ' WITH VALUE ', 1PD22.14, ' HAS INDIC',
     * 'ES ', I8, ' ,', I8/'INDICES OUT OF RANGE.')
99998 FORMAT (36X, 8HNON-ZERO, I7, 2H ,, I6, 23H IN ZERO OFF-DIAGONAL B,
     * 4HLOCK)
99997 FORMAT (36X, 8H ELEMENT, I6, 2H ,, I6, 23H WAS NOT IN L/U PATTERN)
      END
      SUBROUTINE MA30AD(NN, ICN, A, LICN, LENR, LENRL, IDISP, IP, IQ,
     * IRN, LIRN, LENC, IFIRST, LASTR, NEXTR, LASTC, NEXTC, IPTR, IPC,
     * U, IFLAG)

c*********************************************************************72
c
cc MA30AD solves a general sparse linear system.
c
c  Discussion:
c
c    if the user requires a more convenient data interface then the ma28
c    package should be used.  the ma28 subroutines call the ma30
c    subroutines after checking the user's input data and optionally
c    using mc23a/ad to permute the matrix to block triangular form.
c
c    this package of subroutines (ma30a/ad, ma30b/bd, ma30c/cd and
c    ma30d/dd) performs operations pertinent to the solution of a
c    general sparse n by n system of linear equations (i.e. solve
c    ax=b). structually singular matrices are permitted including
c    those with row or columns consisting entirely of zeros (i.e.
c    including rectangular matrices).  it is assumed that the
c    non-zeros of the matrix a do not differ widely in size.  if
c    necessary a prior call of the scaling subroutine mc19a/ad may be
c    made.
c
c    a discussion of the design of these subroutines is given by duff and
c    reid (acm trans math software 5 pp 18-35,1979 (css 48)) while
c    fuller details of the implementation are given in duff (harwell
c    report aere-r 8730,1977).  the additional pivoting option in
c    ma30a/ad and the use of drop tolerances (see common block
c    ma30i/id) were added to the package after joint work with reid,
c    schaumburg, wasniewski and zlatev (duff, reid, schaumburg,
c    wasniewski and zlatev, harwell report css 135, 1983).
c
c    ma30a/ad performs the lu decomposition of the diagonal blocks of the
c    permutation paq of a sparse matrix a, where input permutations
c    p1 and q1 are used to define the diagonal blocks.  there may be
c    non-zeros in the off-diagonal blocks but they are unaffected by
c    ma30a/ad. p and p1 differ only within blocks as do q and q1. the
c    permutations p1 and q1 may be found by calling mc23a/ad or the
c    matrix may be treated as a single block by using p1=q1=i. the
c    matrix non-zeros should be held compactly by rows, although it
c    should be noted that the user can supply the matrix by columns
c    to get the lu decomposition of a transpose.
c
c  Modified:
c
c    08 January 2013
c
c the parameters are...
c this description should also be consulted for further information on
c     most of the parameters of ma30b/bd and ma30c/cd.
c
c n  is an integer variable which must be set by the user to the order
c     of the matrix.  it is not altered by ma30a/ad.
c icn is an integer array of length licn. positions idisp(2) to
c     licn must be set by the user to contain the column indices of
c     the non-zeros in the diagonal blocks of p1*a*q1. those belonging
c     to a single row must be contiguous but the ordering of column
c     indices with each row is unimportant. the non-zeros of row i
c     precede those of row i+1,i=1,...,n-1 and no wasted space is
c     allowed between the rows.  on output the column indices of the
c     lu decomposition of paq are held in positions idisp(1) to
c     idisp(2), the rows are in pivotal order, and the column indices
c     of the l part of each row are in pivotal order and precede those
c     of u. again there is no wasted space either within a row or
c     between the rows. icn(1) to icn(idisp(1)-1), are neither
c     required nor altered. if mc23a/ad been called, these will hold
c     information about the off-diagonal blocks.
c a is a real/double precision array of length licn whose entries
c     idisp(2) to licn must be set by the user to the  values of the
c     non-zero entries of the matrix in the order indicated by  icn.
c     on output a will hold the lu factors of the matrix where again
c     the position in the matrix is determined by the corresponding
c     values in icn. a(1) to a(idisp(1)-1) are neither required nor
c     altered.
c licn  is an integer variable which must be set by the user to the
c     length of arrays icn and a. it must be big enough for a and icn
c     to hold all the non-zeros of l and u and leave some "elbow
c     room".  it is possible to calculate a minimum value for licn by
c     a preliminary run of ma30a/ad. the adequacy of the elbow room
c     can be judged by the size of the common block variable icncp. it
c     is not altered by ma30a/ad.
c lenr  is an integer array of length n.  on input, lenr(i) should
c     equal the number of non-zeros in row i, i=1,...,n of the
c     diagonal blocks of p1*a*q1. on output, lenr(i) will equal the
c     total number of non-zeros in row i of l and row i of u.
c lenrl  is an integer array of length n. on output from ma30a/ad,
c     lenrl(i) will hold the number of non-zeros in row i of l.
c idisp  is an integer array of length 2. the user should set idisp(1)
c     to be the first available position in a/icn for the lu
c     decomposition while idisp(2) is set to the position in a/icn of
c     the first non-zero in the diagonal blocks of p1*a*q1. on output,
c     idisp(1) will be unaltered while idisp(2) will be set to the
c     position in a/icn of the last non-zero of the lu decomposition.
c ip  is an integer array of length n which holds a permutation of
c     the integers 1 to n.  on input to ma30a/ad, the absolute value of
c     ip(i) must be set to the row of a which is row i of p1*a*q1. a
c     negative value for ip(i) indicates that row i is at the end of a
c     diagonal block.  on output from ma30a/ad, ip(i) indicates the row
c     of a which is the i th row in paq. ip(i) will still be negative
c     for the last row of each block (except the last).
c iq is an integer array of length n which again holds a
c     permutation of the integers 1 to n.  on input to ma30a/ad, iq(j)
c     must be set to the column of a which is column j of p1*a*q1. on
c     output from ma30a/ad, the absolute value of iq(j) indicates the
c     column of a which is the j th in paq.  for rows, i say, in which
c     structural or numerical singularity is detected iq(i) is
c     negated.
c irn  is an integer array of length lirn used as workspace by
c     ma30a/ad.
c lirn  is an integer variable. it should be greater than the
c     largest number of non-zeros in a diagonal block of p1*a*q1 but
c     need not be as large as licn. it is the length of array irn and
c     should be large enough to hold the active part of any block,
c     plus some "elbow room", the  a posteriori  adequacy of which can
c     be estimated by examining the size of common block variable
c     irncp.
c lenc,ifirst,lastr,nextr,lastc,nextc are all integer arrays of
c     length n which are used as workspace by ma30a/ad.  if nsrch is
c     set to a value less than or equal to n, then arrays lastc and
c     nextc are not referenced by ma30a/ad and so can be dummied in
c     the call to ma30a/ad.
c iptr,ipc are integer arrays of length n which are used as workspace
c     by ma30a/ad.
c u  is a real/double precision variable which should be set by the
c     user to a value between 0. and 1.0. if less than zero it is
c     reset to zero and if its value is 1.0 or greater it is reset to
c     0.9999 (0.999999999 in d version).  it determines the balance
c     between pivoting for sparsity and for stability, values near
c     zero emphasizing sparsity and values near one emphasizing
c     stability. we recommend u=0.1 as a posible first trial value.
c     the stability can be judged by a later call to mc24a/ad or by
c     setting lbig to .true.
c iflag  is an integer variable. it will have a non-negative value if
c     ma30a/ad is successful. negative values indicate error
c     conditions while positive values indicate that the matrix has
c     been successfully decomposed but is singular. for each non-zero
c     value, an appropriate message is output on unit lp.  possible
c     non-zero values for iflag are ...
c
c -1  the matrix is structually singular with rank given by irank in
c     common block ma30f/fd.
c +1  if, however, the user wants the lu decomposition of a
c     structurally singular matrix and sets the common block variable
c     abort1 to .false., then, in the event of singularity and a
c     successful decomposition, iflag is returned with the value +1
c     and no message is output.
c -2  the matrix is numerically singular (it may also be structually
c     singular) with estimated rank given by irank in common block
c     ma30f/fd.
c +2  the  user can choose to continue the decomposition even when a
c     zero pivot is encountered by setting common block variable
c     abort2 to .false.  if a singularity is encountered, iflag will
c     then return with a value of +2, and no message is output if the
c     decomposition has been completed successfully.
c -3  lirn has not been large enough to continue with the
c     decomposition.  if the stage was zero then common block variable
c     minirn gives the length sufficient to start the decomposition on
c     this block.  for a successful decomposition on this block the
c     user should make lirn slightly (say about n/2) greater than this
c     value.
c -4  licn not large enough to continue with the decomposition.
c -5  the decomposition has been completed but some of the lu factors
c     have been discarded to create enough room in a/icn to continue
c     the decomposition. the variable minicn in common block ma30f/fd
c     then gives the size that licn should be to enable the
c     factorization to be successful.  if the user sets common block
c     variable abort3 to .true., then the subroutine will exit
c     immediately instead of destroying any factors and continuing.
c -6  both licn and lirn are too small. termination has been caused by
c     lack of space in irn (see error iflag= -3), but already some of
c     the lu factors in a/icn have been lost (see error iflag= -5).
c     minicn gives the minimum amount of space required in a/icn for
c     decomposition up to this point.
c
      DOUBLE PRECISION A(LICN), U, AU, UMAX, AMAX, ZERO, PIVRAT, PIVR,
     * TOL, BIG, ANEW, AANEW, SCALE
      INTEGER IPTR(NN), PIVOT, PIVEND, DISPC, OLDPIV, OLDEND, PIVROW,
     * ROWI, IPC(NN), IDISP(2), COLUPD
      INTEGER ICN(LICN), LENR(NN), LENRL(NN), IP(NN), IQ(NN),
     * LENC(NN), IRN(LIRN), IFIRST(NN), LASTR(NN), NEXTR(NN),
     * LASTC(NN), NEXTC(NN)
      LOGICAL ABORT1, ABORT2, ABORT3, LBIG
c for comments of common block variables see block data subprogram.
      COMMON /MA30ED/ LP, ABORT1, ABORT2, ABORT3
      COMMON /MA30FD/ IRNCP, ICNCP, IRANK, MINIRN, MINICN
      COMMON /MA30ID/ TOL, BIG, NDROP, NSRCH, LBIG
      COMMON /LPIVOT/ LPIV(10),LNPIV(10),MAPIV,MANPIV,IAVPIV,
     *                IANPIV,KOUNTL
c
      DATA UMAX/.999999999D0/
      DATA ZERO /0.0D0/
      MSRCH = NSRCH
      NDROP = 0
      DO 1272 KK=1,10
        LNPIV(KK)=0
        LPIV(KK)=0
 1272 CONTINUE
      MAPIV = 0
      MANPIV = 0
      IAVPIV = 0
      IANPIV = 0
      KOUNTL = 0
      MINIRN = 0
      MINICN = IDISP(1) - 1
      MOREI = 0
      IRANK = NN
      IRNCP = 0
      ICNCP = 0
      IFLAG = 0
c reset u if necessary.
      U = DMIN1(U,UMAX)
c ibeg is the position of the next pivot row after elimination step
c     using it.
      U = DMAX1(U,ZERO)
      IBEG = IDISP(1)
c iactiv is the position of the first entry in the active part of a/icn.
      IACTIV = IDISP(2)
c nzrow is current number of non-zeros in active and unprocessed part
c     of row file icn.
      NZROW = LICN - IACTIV + 1
      MINICN = NZROW + MINICN
c
c count the number of diagonal blocks and set up pointers to the
c     beginnings of the rows.
c num is the number of diagonal blocks.
      NUM = 1
      IPTR(1) = IACTIV
      IF (NN.EQ.1) GO TO 20
      NNM1 = NN - 1
      DO 10 I=1,NNM1
        IF (IP(I).LT.0) NUM = NUM + 1
        IPTR(I+1) = IPTR(I) + LENR(I)
   10 CONTINUE
c ilast is the last row in the previous block.
   20 ILAST = 0
c
c ***********************************************
c ****    lu decomposition of block nblock   ****
c ***********************************************
c
c each pass through this loop performs lu decomposition on one
c     of the diagonal blocks.
      DO 1000 NBLOCK=1,NUM
        ISTART = ILAST + 1
        DO 30 IROWS=ISTART,NN
          IF (IP(IROWS).LT.0) GO TO 40
   30   CONTINUE
        IROWS = NN
   40   ILAST = IROWS
c n is the number of rows in the current block.
c istart is the index of the first row in the current block.
c ilast is the index of the last row in the current block.
c iactiv is the position of the first entry in the block.
c itop is the position of the last entry in the block.
        N = ILAST - ISTART + 1
        IF (N.NE.1) GO TO 90
c
c code for dealing with 1x1 block.
        LENRL(ILAST) = 0
        ISING = ISTART
        IF (LENR(ILAST).NE.0) GO TO 50
c block is structurally singular.
        IRANK = IRANK - 1
        ISING = -ISING
        IF (IFLAG.NE.2 .AND. IFLAG.NE.-5) IFLAG = 1
        IF (.NOT.ABORT1) GO TO 80
        IDISP(2) = IACTIV
        IFLAG = -1
        IF (LP.NE.0) WRITE (LP,99999)
c     return
        GO TO 1120
   50   SCALE = DABS(A(IACTIV))
        IF (SCALE.EQ.ZERO) GO TO 60
        IF (LBIG) BIG = DMAX1(BIG,SCALE)
        GO TO 70
   60   ISING = -ISING
        IRANK = IRANK - 1
        IPTR(ILAST) = 0
        IF (IFLAG.NE.-5) IFLAG = 2
        IF (.NOT.ABORT2) GO TO 70
        IDISP(2) = IACTIV
        IFLAG = -2
        IF (LP.NE.0) WRITE (LP,99998)
        GO TO 1120
   70   A(IBEG) = A(IACTIV)
        ICN(IBEG) = ICN(IACTIV)
        IACTIV = IACTIV + 1
        IPTR(ISTART) = 0
        IBEG = IBEG + 1
        NZROW = NZROW - 1
   80   LASTR(ISTART) = ISTART
        IPC(ISTART) = -ISING
        GO TO 1000
c
c non-trivial block.
   90   ITOP = LICN
        IF (ILAST.NE.NN) ITOP = IPTR(ILAST+1) - 1
c
c set up column oriented storage.
        DO 100 I=ISTART,ILAST
          LENRL(I) = 0
          LENC(I) = 0
  100   CONTINUE
        IF (ITOP-IACTIV.LT.LIRN) GO TO 110
        MINIRN = ITOP - IACTIV + 1
        PIVOT = ISTART - 1
        GO TO 1100
c
c calculate column counts.
  110   DO 120 II=IACTIV,ITOP
          I = ICN(II)
          LENC(I) = LENC(I) + 1
  120   CONTINUE
c set up column pointers so that ipc(j) points to position after end
c     of column j in column file.
        IPC(ILAST) = LIRN + 1
        J1 = ISTART + 1
        DO 130 JJ=J1,ILAST
          J = ILAST - JJ + J1 - 1
          IPC(J) = IPC(J+1) - LENC(J+1)
  130   CONTINUE
        DO 150 INDROW=ISTART,ILAST
          J1 = IPTR(INDROW)
          J2 = J1 + LENR(INDROW) - 1
          IF (J1.GT.J2) GO TO 150
          DO 140 JJ=J1,J2
            J = ICN(JJ)
            IPOS = IPC(J) - 1
            IRN(IPOS) = INDROW
            IPC(J) = IPOS
  140     CONTINUE
  150   CONTINUE
c dispc is the lowest indexed active location in the column file.
        DISPC = IPC(ISTART)
        NZCOL = LIRN - DISPC + 1
        MINIRN = MAX0(NZCOL,MINIRN)
        NZMIN = 1
c
c initialize array ifirst.  ifirst(i) = +/- k indicates that row/col
c     k has i non-zeros.  if ifirst(i) = 0, there is no row or column
c     with i non zeros.
        DO 160 I=1,N
          IFIRST(I) = 0
  160   CONTINUE
c
c compute ordering of row and column counts.
c first run through columns (from column n to column 1).
        DO 180 JJ=ISTART,ILAST
          J = ILAST - JJ + ISTART
          NZ = LENC(J)
          IF (NZ.NE.0) GO TO 170
          IPC(J) = 0
          GO TO 180
  170     IF (NSRCH.LE.NN) GO TO 180
          ISW = IFIRST(NZ)
          IFIRST(NZ) = -J
          LASTC(J) = 0
          NEXTC(J) = -ISW
          ISW1 = IABS(ISW)
          IF (ISW.NE.0) LASTC(ISW1) = J
  180   CONTINUE
c now run through rows (again from n to 1).
        DO 210 II=ISTART,ILAST
          I = ILAST - II + ISTART
          NZ = LENR(I)
          IF (NZ.NE.0) GO TO 190
          IPTR(I) = 0
          LASTR(I) = 0
          GO TO 210
  190     ISW = IFIRST(NZ)
          IFIRST(NZ) = I
          IF (ISW.GT.0) GO TO 200
          NEXTR(I) = 0
          LASTR(I) = ISW
          GO TO 210
  200     NEXTR(I) = ISW
          LASTR(I) = LASTR(ISW)
          LASTR(ISW) = I
  210   CONTINUE
c
c
c **********************************************
c ****    start of main elimination loop    ****
c **********************************************
        DO 980 PIVOT=ISTART,ILAST
c
c first find the pivot using markowitz criterion with stability
c     control.
c jcost is the markowitz cost of the best pivot so far,.. this
c     pivot is in row ipiv and column jpiv.
          NZ2 = NZMIN
          JCOST = N*N
c
c examine rows/columns in order of ascending count.
          DO 340 L=1,2
            PIVRAT = ZERO
            ISRCH = 1
            LL = L
c a pass with l equal to 2 is only performed in the case of singularity.
            DO 330 NZ=NZ2,N
              IF (JCOST.LE.(NZ-1)**2) GO TO 420
              IJFIR = IFIRST(NZ)
              IF (IJFIR) 230, 220, 240
  220         IF (LL.EQ.1) NZMIN = NZ + 1
              GO TO 330
  230         LL = 2
              IJFIR = -IJFIR
              GO TO 290
  240         LL = 2
c scan rows with nz non-zeros.
              DO 270 IDUMMY=1,N
                IF (JCOST.LE.(NZ-1)**2) GO TO 420
                IF (ISRCH.GT.MSRCH) GO TO 420
                IF (IJFIR.EQ.0) GO TO 280
c row ijfir is now examined.
                I = IJFIR
                IJFIR = NEXTR(I)
c first calculate multiplier threshold level.
                AMAX = ZERO
                J1 = IPTR(I) + LENRL(I)
                J2 = IPTR(I) + LENR(I) - 1
                DO 250 JJ=J1,J2
                  AMAX = DMAX1(AMAX,DABS(A(JJ)))
  250           CONTINUE
                AU = AMAX*U
                ISRCH = ISRCH + 1
c scan row for possible pivots
                DO 260 JJ=J1,J2
                  IF (DABS(A(JJ)).LE.AU .AND. L.EQ.1) GO TO 260
                  J = ICN(JJ)
                  KCOST = (NZ-1)*(LENC(J)-1)
                  IF (KCOST.GT.JCOST) GO TO 260
                  PIVR = ZERO
                  IF (AMAX.NE.ZERO) PIVR = DABS(A(JJ))/AMAX
                  IF (KCOST.EQ.JCOST .AND. (PIVR.LE.PIVRAT .OR.
     *             NSRCH.GT.NN+1)) GO TO 260
c best pivot so far is found.
                  JCOST = KCOST
                  IJPOS = JJ
                  IPIV = I
                  JPIV = J
                  IF (MSRCH.GT.NN+1 .AND. JCOST.LE.(NZ-1)**2) GO TO 420
                  PIVRAT = PIVR
  260           CONTINUE
  270         CONTINUE
c
c columns with nz non-zeros now examined.
  280         IJFIR = IFIRST(NZ)
              IJFIR = -LASTR(IJFIR)
  290         IF (JCOST.LE.NZ*(NZ-1)) GO TO 420
              IF (MSRCH.LE.NN) GO TO 330
              DO 320 IDUMMY=1,N
                IF (IJFIR.EQ.0) GO TO 330
                J = IJFIR
                IJFIR = NEXTC(IJFIR)
                I1 = IPC(J)
                I2 = I1 + NZ - 1
c scan column j.
                DO 310 II=I1,I2
                  I = IRN(II)
                  KCOST = (NZ-1)*(LENR(I)-LENRL(I)-1)
                  IF (KCOST.GE.JCOST) GO TO 310
c pivot has best markowitz count so far ... now check its
c     suitability on numeric grounds by examining the other non-zeros
c     in its row.
                  J1 = IPTR(I) + LENRL(I)
                  J2 = IPTR(I) + LENR(I) - 1
c we need a stability check on singleton columns because of possible
c     problems with underdetermined systems.
                  AMAX = ZERO
                  DO 300 JJ=J1,J2
                    AMAX = DMAX1(AMAX,DABS(A(JJ)))
                    IF (ICN(JJ).EQ.J) JPOS = JJ
  300             CONTINUE
                  IF (DABS(A(JPOS)).LE.AMAX*U .AND. L.EQ.1) GO TO 310
                  JCOST = KCOST
                  IPIV = I
                  JPIV = J
                  IJPOS = JPOS
                  IF (AMAX.NE.ZERO) PIVRAT = DABS(A(JPOS))/AMAX
                  IF (JCOST.LE.NZ*(NZ-1)) GO TO 420
  310           CONTINUE
c
  320         CONTINUE
c
  330       CONTINUE
c in the event of singularity, we must make sure all rows and columns
c are tested.
            MSRCH = N
c
c matrix is numerically or structurally singular  ... which it is will
c     be diagnosed later.
            IRANK = IRANK - 1
  340     CONTINUE
c assign rest of rows and columns to ordering array.
c matrix is structurally singular.
          IF (IFLAG.NE.2 .AND. IFLAG.NE.-5) IFLAG = 1
          IRANK = IRANK - ILAST + PIVOT + 1
          IF (.NOT.ABORT1) GO TO 350
          IDISP(2) = IACTIV
          IFLAG = -1
          IF (LP.NE.0) WRITE (LP,99999)
          GO TO 1120
  350     K = PIVOT - 1
          DO 390 I=ISTART,ILAST
            IF (LASTR(I).NE.0) GO TO 390
            K = K + 1
            LASTR(I) = K
            IF (LENRL(I).EQ.0) GO TO 380
            MINICN = MAX0(MINICN,NZROW+IBEG-1+MOREI+LENRL(I))
            IF (IACTIV-IBEG.GE.LENRL(I)) GO TO 360
            CALL MA30DD(A, ICN, IPTR(ISTART), N, IACTIV, ITOP, .TRUE.)
c check now to see if ma30d/dd has created enough available space.
            IF (IACTIV-IBEG.GE.LENRL(I)) GO TO 360
c create more space by destroying previously created lu factors.
            MOREI = MOREI + IBEG - IDISP(1)
            IBEG = IDISP(1)
            IF (LP.NE.0) WRITE (LP,99997)
            IFLAG = -5
            IF (ABORT3) GO TO 1090
  360       J1 = IPTR(I)
            J2 = J1 + LENRL(I) - 1
            IPTR(I) = 0
            DO 370 JJ=J1,J2
              A(IBEG) = A(JJ)
              ICN(IBEG) = ICN(JJ)
              ICN(JJ) = 0
              IBEG = IBEG + 1
  370       CONTINUE
            NZROW = NZROW - LENRL(I)
  380       IF (K.EQ.ILAST) GO TO 400
  390     CONTINUE
  400     K = PIVOT - 1
          DO 410 I=ISTART,ILAST
            IF (IPC(I).NE.0) GO TO 410
            K = K + 1
            IPC(I) = K
            IF (K.EQ.ILAST) GO TO 990
  410     CONTINUE
c
c the pivot has now been found in position (ipiv,jpiv) in location
c     ijpos in row file.
c update column and row ordering arrays to correspond with removal
c     of the active part of the matrix.
  420     ISING = PIVOT
          IF (A(IJPOS).NE.ZERO) GO TO 430
c numerical singularity is recorded here.
          ISING = -ISING
          IF (IFLAG.NE.-5) IFLAG = 2
          IF (.NOT.ABORT2) GO TO 430
          IDISP(2) = IACTIV
          IFLAG = -2
          IF (LP.NE.0) WRITE (LP,99998)
          GO TO 1120
  430     OLDPIV = IPTR(IPIV) + LENRL(IPIV)
          OLDEND = IPTR(IPIV) + LENR(IPIV) - 1
c changes to column ordering.
          IF (NSRCH.LE.NN) GO TO 460
          COLUPD = NN + 1
            LENPP = OLDEND-OLDPIV+1
            IF (LENPP.LT.4) LPIV(1) = LPIV(1) + 1
            IF (LENPP.GE.4 .AND. LENPP.LE.6) LPIV(2) = LPIV(2) + 1
            IF (LENPP.GE.7 .AND. LENPP.LE.10) LPIV(3) = LPIV(3) + 1
            IF (LENPP.GE.11 .AND. LENPP.LE.15) LPIV(4) = LPIV(4) + 1
            IF (LENPP.GE.16 .AND. LENPP.LE.20) LPIV(5) = LPIV(5) + 1
            IF (LENPP.GE.21 .AND. LENPP.LE.30) LPIV(6) = LPIV(6) + 1
            IF (LENPP.GE.31 .AND. LENPP.LE.50) LPIV(7) = LPIV(7) + 1
            IF (LENPP.GE.51 .AND. LENPP.LE.70) LPIV(8) = LPIV(8) + 1
            IF (LENPP.GE.71 .AND. LENPP.LE.100) LPIV(9) = LPIV(9) + 1
            IF (LENPP.GE.101) LPIV(10) = LPIV(10) + 1
            MAPIV = MAX0(MAPIV,LENPP)
            IAVPIV = IAVPIV + LENPP
          DO 450 JJ=OLDPIV,OLDEND
            J = ICN(JJ)
            LC = LASTC(J)
            NC = NEXTC(J)
            NEXTC(J) = -COLUPD
            IF (JJ.NE.IJPOS) COLUPD = J
            IF (NC.NE.0) LASTC(NC) = LC
            IF (LC.EQ.0) GO TO 440
            NEXTC(LC) = NC
            GO TO 450
  440       NZ = LENC(J)
            ISW = IFIRST(NZ)
            IF (ISW.GT.0) LASTR(ISW) = -NC
            IF (ISW.LT.0) IFIRST(NZ) = -NC
  450     CONTINUE
c changes to row ordering.
  460     I1 = IPC(JPIV)
          I2 = I1 + LENC(JPIV) - 1
          DO 480 II=I1,I2
            I = IRN(II)
            LR = LASTR(I)
            NR = NEXTR(I)
            IF (NR.NE.0) LASTR(NR) = LR
            IF (LR.LE.0) GO TO 470
            NEXTR(LR) = NR
            GO TO 480
  470       NZ = LENR(I) - LENRL(I)
            IF (NR.NE.0) IFIRST(NZ) = NR
            IF (NR.EQ.0) IFIRST(NZ) = LR
  480     CONTINUE
c
c move pivot to position lenrl+1 in pivot row and move pivot row
c     to the beginning of the available storage.
c the l part and the pivot in the old copy of the pivot row is
c     nullified while, in the strictly upper triangular part, the
c     column indices, j say, are overwritten by the corresponding
c     entry of iq (iq(j)) and iq(j) is set to the negative of the
c     displacement of the column index from the pivot entry.
          IF (OLDPIV.EQ.IJPOS) GO TO 490
          AU = A(OLDPIV)
          A(OLDPIV) = A(IJPOS)
          A(IJPOS) = AU
          ICN(IJPOS) = ICN(OLDPIV)
          ICN(OLDPIV) = JPIV
c check to see if there is space immediately available in a/icn to
c     hold new copy of pivot row.
  490     MINICN = MAX0(MINICN,NZROW+IBEG-1+MOREI+LENR(IPIV))
          IF (IACTIV-IBEG.GE.LENR(IPIV)) GO TO 500
          CALL MA30DD(A, ICN, IPTR(ISTART), N, IACTIV, ITOP, .TRUE.)
          OLDPIV = IPTR(IPIV) + LENRL(IPIV)
          OLDEND = IPTR(IPIV) + LENR(IPIV) - 1
c check now to see if ma30d/dd has created enough available space.
          IF (IACTIV-IBEG.GE.LENR(IPIV)) GO TO 500
c create more space by destroying previously created lu factors.
          MOREI = MOREI + IBEG - IDISP(1)
          IBEG = IDISP(1)
          IF (LP.NE.0) WRITE (LP,99997)
          IFLAG = -5
          IF (ABORT3) GO TO 1090
          IF (IACTIV-IBEG.GE.LENR(IPIV)) GO TO 500
c there is still not enough room in a/icn.
          IFLAG = -4
          GO TO 1090
c copy pivot row and set up iq array.
  500     IJPOS = 0
          J1 = IPTR(IPIV)
c
          DO 530 JJ=J1,OLDEND
            A(IBEG) = A(JJ)
            ICN(IBEG) = ICN(JJ)
            IF (IJPOS.NE.0) GO TO 510
            IF (ICN(JJ).EQ.JPIV) IJPOS = IBEG
            ICN(JJ) = 0
            GO TO 520
  510       K = IBEG - IJPOS
            J = ICN(JJ)
            ICN(JJ) = IQ(J)
            IQ(J) = -K
  520       IBEG = IBEG + 1
  530     CONTINUE
c
          IJP1 = IJPOS + 1
          PIVEND = IBEG - 1
          LENPIV = PIVEND - IJPOS
          NZROW = NZROW - LENRL(IPIV) - 1
          IPTR(IPIV) = OLDPIV + 1
          IF (LENPIV.EQ.0) IPTR(IPIV) = 0
c
c remove pivot row (including pivot) from column oriented file.
          DO 560 JJ=IJPOS,PIVEND
            J = ICN(JJ)
            I1 = IPC(J)
            LENC(J) = LENC(J) - 1
c i2 is last position in new column.
            I2 = IPC(J) + LENC(J) - 1
            IF (I2.LT.I1) GO TO 550
            DO 540 II=I1,I2
              IF (IRN(II).NE.IPIV) GO TO 540
              IRN(II) = IRN(I2+1)
              GO TO 550
  540       CONTINUE
  550       IRN(I2+1) = 0
  560     CONTINUE
          NZCOL = NZCOL - LENPIV - 1
c
c go down the pivot column and for each row with a non-zero add
c     the appropriate multiple of the pivot row to it.
c we loop on the number of non-zeros in the pivot column since
c     ma30d/dd may change its actual position.
c
          NZPC = LENC(JPIV)
          IF (NZPC.EQ.0) GO TO 900
          DO 840 III=1,NZPC
            II = IPC(JPIV) + III - 1
            I = IRN(II)
c search row i for non-zero to be eliminated, calculate multiplier,
c     and place it in position lenrl+1 in its row.
c  idrop is the number of non-zero entries dropped from row    i
c        because these fall beneath tolerance level.
c
            IDROP = 0
            J1 = IPTR(I) + LENRL(I)
            IEND = IPTR(I) + LENR(I) - 1
            DO 570 JJ=J1,IEND
              IF (ICN(JJ).NE.JPIV) GO TO 570
c if pivot is zero, rest of column is and so multiplier is zero.
              AU = ZERO
              IF (A(IJPOS).NE.ZERO) AU = -A(JJ)/A(IJPOS)
              IF (LBIG) BIG = DMAX1(BIG,DABS(AU))
              A(JJ) = A(J1)
              A(J1) = AU
              ICN(JJ) = ICN(J1)
              ICN(J1) = JPIV
              LENRL(I) = LENRL(I) + 1
              GO TO 580
  570       CONTINUE
c jump if pivot row is a singleton.
  580       IF (LENPIV.EQ.0) GO TO 840
c now perform necessary operations on rest of non-pivot row i.
            ROWI = J1 + 1
            IOP = 0
c jump if all the pivot row causes fill-in.
            IF (ROWI.GT.IEND) GO TO 650
c perform operations on current non-zeros in row i.
c innermost loop.
            LENPP = IEND-ROWI+1
            IF (LENPP.LT.4) LNPIV(1) = LNPIV(1) + 1
            IF (LENPP.GE.4 .AND. LENPP.LE.6) LNPIV(2) = LNPIV(2) + 1
            IF (LENPP.GE.7 .AND. LENPP.LE.10) LNPIV(3) = LNPIV(3) + 1
            IF (LENPP.GE.11 .AND. LENPP.LE.15) LNPIV(4) = LNPIV(4) + 1
            IF (LENPP.GE.16 .AND. LENPP.LE.20) LNPIV(5) = LNPIV(5) + 1
            IF (LENPP.GE.21 .AND. LENPP.LE.30) LNPIV(6) = LNPIV(6) + 1
            IF (LENPP.GE.31 .AND. LENPP.LE.50) LNPIV(7) = LNPIV(7) + 1
            IF (LENPP.GE.51 .AND. LENPP.LE.70) LNPIV(8) = LNPIV(8) + 1
            IF (LENPP.GE.71 .AND. LENPP.LE.100) LNPIV(9) = LNPIV(9) + 1
            IF (LENPP.GE.101) LNPIV(10) = LNPIV(10) + 1
            MANPIV = MAX0(MANPIV,LENPP)
            IANPIV = IANPIV + LENPP
            KOUNTL = KOUNTL + 1
            DO 590 JJ=ROWI,IEND
              J = ICN(JJ)
              IF (IQ(J).GT.0) GO TO 590
              IOP = IOP + 1
              PIVROW = IJPOS - IQ(J)
              A(JJ) = A(JJ) + AU*A(PIVROW)
              IF (LBIG) BIG = DMAX1(DABS(A(JJ)),BIG)
              ICN(PIVROW) = -ICN(PIVROW)
              IF (DABS(A(JJ)).LT.TOL) IDROP = IDROP + 1
  590       CONTINUE
c
c  jump if no non-zeros in non-pivot row have been removed
c       because these are beneath the drop-tolerance  tol.
c
            IF (IDROP.EQ.0) GO TO 650
c
c  run through non-pivot row compressing row so that only
c      non-zeros greater than   tol   are stored.  all non-zeros
c      less than   tol   are also removed from the column structure.
c
            JNEW = ROWI
            DO 630 JJ=ROWI,IEND
              IF (DABS(A(JJ)).LT.TOL) GO TO 600
              A(JNEW) = A(JJ)
              ICN(JNEW) = ICN(JJ)
              JNEW = JNEW + 1
              GO TO 630
c
c  remove non-zero entry from column structure.
c
  600         J = ICN(JJ)
              I1 = IPC(J)
              I2 = I1 + LENC(J) - 1
              DO 610 II=I1,I2
                IF (IRN(II).EQ.I) GO TO 620
  610         CONTINUE
  620         IRN(II) = IRN(I2)
              IRN(I2) = 0
              LENC(J) = LENC(J) - 1
              IF (NSRCH.LE.NN) GO TO 630
c remove column from column chain and place in update chain.
              IF (NEXTC(J).LT.0) GO TO 630
c jump if column already in update chain.
              LC = LASTC(J)
              NC = NEXTC(J)
              NEXTC(J) = -COLUPD
              COLUPD = J
              IF (NC.NE.0) LASTC(NC) = LC
              IF (LC.EQ.0) GO TO 622
              NEXTC(LC) = NC
              GO TO 630
  622         NZ = LENC(J) + 1
              ISW = IFIRST(NZ)
              IF (ISW.GT.0) LASTR(ISW) = -NC
              IF (ISW.LT.0) IFIRST(NZ) = -NC
  630       CONTINUE
            DO 640 JJ=JNEW,IEND
              ICN(JJ) = 0
  640       CONTINUE
c the value of idrop might be different from that calculated earlier
c     because, we may now have dropped some non-zeros which were not
c     modified by the pivot row.
            IDROP = IEND + 1 - JNEW
            IEND = JNEW - 1
            LENR(I) = LENR(I) - IDROP
            NZROW = NZROW - IDROP
            NZCOL = NZCOL - IDROP
            NDROP = NDROP + IDROP
  650       IFILL = LENPIV - IOP
c jump is if there is no fill-in.
            IF (IFILL.EQ.0) GO TO 750
c now for the fill-in.
            MINICN = MAX0(MINICN,MOREI+IBEG-1+NZROW+IFILL+LENR(I))
c see if there is room for fill-in.
c get maximum space for row i in situ.
            DO 660 JDIFF=1,IFILL
              JNPOS = IEND + JDIFF
              IF (JNPOS.GT.LICN) GO TO 670
              IF (ICN(JNPOS).NE.0) GO TO 670
  660       CONTINUE
c there is room for all the fill-in after the end of the row so it
c     can be left in situ.
c next available space for fill-in.
            IEND = IEND + 1
            GO TO 750
c jmore spaces for fill-in are required in front of row.
  670       JMORE = IFILL - JDIFF + 1
            I1 = IPTR(I)
c we now look in front of the row to see if there is space for
c     the rest of the fill-in.
            DO 680 JDIFF=1,JMORE
              JNPOS = I1 - JDIFF
              IF (JNPOS.LT.IACTIV) GO TO 690
              IF (ICN(JNPOS).NE.0) GO TO 700
  680       CONTINUE
  690       JNPOS = I1 - JMORE
            GO TO 710
c whole row must be moved to the beginning of available storage.
  700       JNPOS = IACTIV - LENR(I) - IFILL
c jump if there is space immediately available for the shifted row.
  710       IF (JNPOS.GE.IBEG) GO TO 730
            CALL MA30DD(A, ICN, IPTR(ISTART), N, IACTIV, ITOP, .TRUE.)
            I1 = IPTR(I)
            IEND = I1 + LENR(I) - 1
            JNPOS = IACTIV - LENR(I) - IFILL
            IF (JNPOS.GE.IBEG) GO TO 730
c no space available so try to create some by throwing away previous
c     lu decomposition.
            MOREI = MOREI + IBEG - IDISP(1) - LENPIV - 1
            IF (LP.NE.0) WRITE (LP,99997)
            IFLAG = -5
            IF (ABORT3) GO TO 1090
c keep record of current pivot row.
            IBEG = IDISP(1)
            ICN(IBEG) = JPIV
            A(IBEG) = A(IJPOS)
            IJPOS = IBEG
            DO 720 JJ=IJP1,PIVEND
              IBEG = IBEG + 1
              A(IBEG) = A(JJ)
              ICN(IBEG) = ICN(JJ)
  720       CONTINUE
            IJP1 = IJPOS + 1
            PIVEND = IBEG
            IBEG = IBEG + 1
            IF (JNPOS.GE.IBEG) GO TO 730
c this still does not give enough room.
            IFLAG = -4
            GO TO 1090
  730       IACTIV = MIN0(IACTIV,JNPOS)
c move non-pivot row i.
            IPTR(I) = JNPOS
            DO 740 JJ=I1,IEND
              A(JNPOS) = A(JJ)
              ICN(JNPOS) = ICN(JJ)
              JNPOS = JNPOS + 1
              ICN(JJ) = 0
  740       CONTINUE
c first new available space.
            IEND = JNPOS
  750       NZROW = NZROW + IFILL
c innermost fill-in loop which also resets icn.
            IDROP = 0
            DO 830 JJ=IJP1,PIVEND
              J = ICN(JJ)
              IF (J.LT.0) GO TO 820
              ANEW = AU*A(JJ)
              AANEW = DABS(ANEW)
              IF (AANEW.GE.TOL) GO TO 760
              IDROP = IDROP + 1
              NDROP = NDROP + 1
              NZROW = NZROW - 1
              MINICN = MINICN - 1
              IFILL = IFILL - 1
              GO TO 830
  760         IF (LBIG) BIG = DMAX1(AANEW,BIG)
              A(IEND) = ANEW
              ICN(IEND) = J
              IEND = IEND + 1
c
c put new entry in column file.
              MINIRN = MAX0(MINIRN,NZCOL+LENC(J)+1)
              JEND = IPC(J) + LENC(J)
              JROOM = NZPC - III + 1 + LENC(J)
              IF (JEND.GT.LIRN) GO TO 770
              IF (IRN(JEND).EQ.0) GO TO 810
  770         IF (JROOM.LT.DISPC) GO TO 780
c compress column file to obtain space for new copy of column.
              CALL MA30DD(A, IRN, IPC(ISTART), N, DISPC, LIRN, .FALSE.)
              IF (JROOM.LT.DISPC) GO TO 780
              JROOM = DISPC - 1
              IF (JROOM.GE.LENC(J)+1) GO TO 780
c column file is not large enough.
              GO TO 1100
c copy column to beginning of file.
  780         JBEG = IPC(J)
              JEND = IPC(J) + LENC(J) - 1
              JZERO = DISPC - 1
              DISPC = DISPC - JROOM
              IDISPC = DISPC
              DO 790 II=JBEG,JEND
                IRN(IDISPC) = IRN(II)
                IRN(II) = 0
                IDISPC = IDISPC + 1
  790         CONTINUE
              IPC(J) = DISPC
              JEND = IDISPC
              DO 800 II=JEND,JZERO
                IRN(II) = 0
  800         CONTINUE
  810         IRN(JEND) = I
              NZCOL = NZCOL + 1
              LENC(J) = LENC(J) + 1
c end of adjustment to column file.
              GO TO 830
c
  820         ICN(JJ) = -J
  830       CONTINUE
            IF (IDROP.EQ.0) GO TO 834
            DO 832 KDROP=1,IDROP
            ICN(IEND) = 0
            IEND = IEND + 1
  832       CONTINUE
  834       LENR(I) = LENR(I) + IFILL
c end of scan of pivot column.
  840     CONTINUE
c
c
c remove pivot column from column oriented storage and update row
c     ordering arrays.
          I1 = IPC(JPIV)
          I2 = IPC(JPIV) + LENC(JPIV) - 1
          NZCOL = NZCOL - LENC(JPIV)
          DO 890 II=I1,I2
            I = IRN(II)
            IRN(II) = 0
            NZ = LENR(I) - LENRL(I)
            IF (NZ.NE.0) GO TO 850
            LASTR(I) = 0
            GO TO 890
  850       IFIR = IFIRST(NZ)
            IFIRST(NZ) = I
            IF (IFIR) 860, 880, 870
  860       LASTR(I) = IFIR
            NEXTR(I) = 0
            GO TO 890
  870       LASTR(I) = LASTR(IFIR)
            NEXTR(I) = IFIR
            LASTR(IFIR) = I
            GO TO 890
  880       LASTR(I) = 0
            NEXTR(I) = 0
            NZMIN = MIN0(NZMIN,NZ)
  890     CONTINUE
c restore iq and nullify u part of old pivot row.
c    record the column permutation in lastc(jpiv) and the row
c    permutation in lastr(ipiv).
  900     IPC(JPIV) = -ISING
          LASTR(IPIV) = PIVOT
          IF (LENPIV.EQ.0) GO TO 980
          NZROW = NZROW - LENPIV
          JVAL = IJP1
          JZER = IPTR(IPIV)
          IPTR(IPIV) = 0
          DO 910 JCOUNT=1,LENPIV
            J = ICN(JVAL)
            IQ(J) = ICN(JZER)
            ICN(JZER) = 0
            JVAL = JVAL + 1
            JZER = JZER + 1
  910     CONTINUE
c adjust column ordering arrays.
          IF (NSRCH.GT.NN) GO TO 920
          DO 916 JJ=IJP1,PIVEND
            J = ICN(JJ)
            NZ = LENC(J)
            IF (NZ.NE.0) GO TO 914
            IPC(J) = 0
            GO TO 916
  914       NZMIN = MIN0(NZMIN,NZ)
  916     CONTINUE
          GO TO 980
  920     JJ = COLUPD
          DO 970 JDUMMY=1,NN
            J = JJ
            IF (J.EQ.NN+1) GO TO 980
            JJ = -NEXTC(J)
            NZ = LENC(J)
            IF (NZ.NE.0) GO TO 924
            IPC(J) = 0
            GO TO 970
  924       IFIR = IFIRST(NZ)
            LASTC(J) = 0
            IF (IFIR) 930, 940, 950
  930       IFIRST(NZ) = -J
            IFIR = -IFIR
            LASTC(IFIR) = J
            NEXTC(J) = IFIR
            GO TO 970
  940       IFIRST(NZ) = -J
            NEXTC(J) = 0
            GO TO 960
  950       LC = -LASTR(IFIR)
            LASTR(IFIR) = -J
            NEXTC(J) = LC
            IF (LC.NE.0) LASTC(LC) = J
  960       NZMIN = MIN0(NZMIN,NZ)
  970     CONTINUE
  980   CONTINUE
c ********************************************
c ****    end of main elimination loop    ****
c ********************************************
c
c reset iactiv to point to the beginning of the next block.
  990   IF (ILAST.NE.NN) IACTIV = IPTR(ILAST+1)
 1000 CONTINUE
c
c ********************************************
c ****    end of deomposition of block    ****
c ********************************************
c
c record singularity (if any) in iq array.
      IF (IRANK.EQ.NN) GO TO 1020
      DO 1010 I=1,NN
        IF (IPC(I).LT.0) GO TO 1010
        ISING = IPC(I)
        IQ(ISING) = -IQ(ISING)
        IPC(I) = -ISING
 1010 CONTINUE
c
c run through lu decomposition changing column indices to that of new
c     order and permuting lenr and lenrl arrays according to pivot
c     permutations.
 1020 ISTART = IDISP(1)
      IEND = IBEG - 1
      IF (IEND.LT.ISTART) GO TO 1040
      DO 1030 JJ=ISTART,IEND
        JOLD = ICN(JJ)
        ICN(JJ) = -IPC(JOLD)
 1030 CONTINUE
 1040 DO 1050 II=1,NN
        I = LASTR(II)
        NEXTR(I) = LENR(II)
        IPTR(I) = LENRL(II)
 1050 CONTINUE
      DO 1060 I=1,NN
        LENRL(I) = IPTR(I)
        LENR(I) = NEXTR(I)
 1060 CONTINUE
c
c update permutation arrays ip and iq.
      DO 1070 II=1,NN
        I = LASTR(II)
        J = -IPC(II)
        NEXTR(I) = IABS(IP(II)+0)
        IPTR(J) = IABS(IQ(II)+0)
 1070 CONTINUE
      DO 1080 I=1,NN
        IF (IP(I).LT.0) NEXTR(I) = -NEXTR(I)
        IP(I) = NEXTR(I)
        IF (IQ(I).LT.0) IPTR(I) = -IPTR(I)
        IQ(I) = IPTR(I)
 1080 CONTINUE
      IP(NN) = IABS(IP(NN)+0)
      IDISP(2) = IEND
      GO TO 1120
c
c   ***    error returns    ***
 1090 IDISP(2) = IACTIV
      IF (LP.EQ.0) GO TO 1120
      WRITE (LP,99996)
      GO TO 1110
 1100 IF (IFLAG.EQ.-5) IFLAG = -6
      IF (IFLAG.NE.-6) IFLAG = -3
      IDISP(2) = IACTIV
      IF (LP.EQ.0) GO TO 1120
      IF (IFLAG.EQ.-3) WRITE (LP,99995)
      IF (IFLAG.EQ.-6) WRITE (LP,99994)
 1110 PIVOT = PIVOT - ISTART + 1
      WRITE (LP,99993) PIVOT, NBLOCK, ISTART, ILAST
      IF (PIVOT.EQ.0) WRITE (LP,99992) MINIRN
c
c
 1120 RETURN
99999 FORMAT (54H ERROR RETURN FROM MA30A/AD BECAUSE MATRIX IS STRUCTUR,
     * 13HALLY SINGULAR)
99998 FORMAT (54H ERROR RETURN FROM MA30A/AD BECAUSE MATRIX IS NUMERICA,
     * 12HLLY SINGULAR)
99997 FORMAT (48H LU DECOMPOSITION DESTROYED TO CREATE MORE SPACE)
99996 FORMAT (54H ERROR RETURN FROM MA30A/AD BECAUSE LICN NOT BIG ENOUG,
     * 1HH)
99995 FORMAT (54H ERROR RETURN FROM MA30A/AD BECAUSE LIRN NOT BIG ENOUG,
     * 1HH)
99994 FORMAT (51H ERROR RETURN FROM MA30A/AD LIRN AND LICN TOO SMALL)
99993 FORMAT (10H AT STAGE , I5, 10H IN BLOCK , I5, 16H WITH FIRST ROW ,
     * I5, 14H AND LAST ROW , I5)
99992 FORMAT (34H TO CONTINUE SET LIRN TO AT LEAST , I8)
      END
      SUBROUTINE MA30BD(N, ICN, A, LICN, LENR, LENRL, IDISP, IP, IQ, W,
     * IW, IFLAG)

c*********************************************************************72
c
cc MA30BD LU factors the diagonal blocks of a matrix of known sparsity pattern.
c
c  Discussion:
c
c    The routine performs the lu decomposition of the diagonal blocks of a
c    new matrix paq of the same sparsity pattern, using information
c    from a previous call to ma30a/ad. the entries of the input
c    matrix  must already be in their final positions in the lu
c    decomposition structure.  this routine executes about five times
c    faster than ma30a/ad.
c
c  Modified:
c
c    08 January 2013
c
c we now describe the argument list for ma30b/bd. consult ma30a/ad for
c     further information on these parameters.
c n  is an integer variable set to the order of the matrix.
c icn is an integer array of length licn. it should be unchanged
c     since the last call to ma30a/ad. it is not altered by ma30b/bd.
c a  is a real/double precision array of length licn the user must set
c     entries idisp(1) to idisp(2) to contain the entries in the
c     diagonal blocks of the matrix paq whose column numbers are held
c     in icn, using corresponding positions. note that some zeros may
c     need to be held explicitly. on output entries idisp(1) to
c     idisp(2) of array a contain the lu decomposition of the diagonal
c     blocks of paq. entries a(1) to a(idisp(1)-1) are neither
c     required nor altered by ma30b/bd.
c licn  is an integer variable which must be set by the user to the
c     length of arrays a and icn. it is not altered by ma30b/bd.
c lenr,lenrl are integer arrays of length n. they should be
c     unchanged since the last call to ma30a/ad. they are not altered
c     by ma30b/bd.
c idisp  is an integer array of length 2. it should be unchanged since
c     the last call to ma30a/ad. it is not altered by ma30b/bd.
c ip,iq  are integer arrays of length n. they should be unchanged
c     since the last call to ma30a/ad. they are not altered by
c     ma30b/bd.
c w  is a real/double precision array of length n which is used as
c     workspace by ma30b/bd.
c iw  is an integer array of length n which is used as workspace by
c     ma30b/bd.
c iflag  is an integer variable. on output from ma30b/bd, iflag has
c     the value zero if the factorization was successful, has the
c     value i if pivot i was very small and has the value -i if an
c     unexpected singularity was detected at stage i of the
c     decomposition.
c
      DOUBLE PRECISION A(LICN), W(N), AU, EPS, ROWMAX, ZERO, ONE, RMIN,
     * TOL, BIG
      LOGICAL ABORT1, ABORT2, ABORT3, STAB, LBIG
      INTEGER IW(N), IDISP(2), PIVPOS
      INTEGER ICN(LICN), LENR(N), LENRL(N), IP(N), IQ(N)
c see block data for comments on variables in common.
      COMMON /MA30ED/ LP, ABORT1, ABORT2, ABORT3
      COMMON /MA30ID/ TOL, BIG, NDROP, NSRCH, LBIG
      COMMON /MA30GD/ EPS, RMIN
      DATA ZERO /0.0D0/, ONE /1.0D0/
      STAB = EPS.LE.ONE
      RMIN = EPS
      ISING = 0
      IFLAG = 0
      DO 10 I=1,N
        W(I) = ZERO
   10 CONTINUE
c set up pointers to the beginning of the rows.
      IW(1) = IDISP(1)
      IF (N.EQ.1) GO TO 25
      DO 20 I=2,N
        IW(I) = IW(I-1) + LENR(I-1)
   20 CONTINUE
c
c   ****   start  of main loop    ****
c at step i, row i of a is transformed to row i of l/u by adding
c     appropriate multiples of rows 1 to i-1.
c     .... using row-gauss elimination.
   25 DO 160 I=1,N
c istart is beginning of row i of a and row i of l.
        ISTART = IW(I)
c ifin is end of row i of a and row i of u.
        IFIN = ISTART + LENR(I) - 1
c ilend is end of row i of l.
        ILEND = ISTART + LENRL(I) - 1
        IF (ISTART.GT.ILEND) GO TO 90
c load row i of a into vector w.
        DO 30 JJ=ISTART,IFIN
          J = ICN(JJ)
          W(J) = A(JJ)
   30   CONTINUE
c
c add multiples of appropriate rows of  i to i-1  to row i.
        DO 70 JJ=ISTART,ILEND
          J = ICN(JJ)
c ipivj is position of pivot in row j.
          IPIVJ = IW(J) + LENRL(J)
c form multiplier au.
          AU = -W(J)/A(IPIVJ)
          IF (LBIG) BIG = DMAX1(DABS(AU),BIG)
          W(J) = AU
c au * row j (u part) is added to row i.
          IPIVJ = IPIVJ + 1
          JFIN = IW(J) + LENR(J) - 1
          IF (IPIVJ.GT.JFIN) GO TO 70
c innermost loop.
          IF (LBIG) GO TO 50
          DO JAYJAY=IPIVJ,JFIN
            JAY = ICN(JAYJAY)
            W(JAY) = W(JAY) + AU*A(JAYJAY)
          end do
          GO TO 70
   50     DO 60 JAYJAY=IPIVJ,JFIN
            JAY = ICN(JAYJAY)
            W(JAY) = W(JAY) + AU*A(JAYJAY)
            BIG = DMAX1(DABS(W(JAY)),BIG)
   60     CONTINUE
   70   CONTINUE
c
c reload w back into a (now l/u)
        DO 80 JJ=ISTART,IFIN
          J = ICN(JJ)
          A(JJ) = W(J)
          W(J) = ZERO
   80   CONTINUE
c we now perform the stability checks.
   90   PIVPOS = ILEND + 1
        IF (IQ(I).GT.0) GO TO 140
c matrix had singularity at this point in ma30a/ad.
c is it the first such pivot in current block ?
        IF (ISING.EQ.0) ISING = I
c does current matrix have a singularity in the same place ?
        IF (PIVPOS.GT.IFIN) GO TO 100
        IF (A(PIVPOS).NE.ZERO) GO TO 170
c it does .. so set ising if it is not the end of the current block
c check to see that appropriate part of l/u is zero or null.
  100   IF (ISTART.GT.IFIN) GO TO 120
        DO 110 JJ=ISTART,IFIN
          IF (ICN(JJ).LT.ISING) GO TO 110
          IF (A(JJ).NE.ZERO) GO TO 170
  110   CONTINUE
  120   IF (PIVPOS.LE.IFIN) A(PIVPOS) = ONE
        IF (IP(I).GT.0 .AND. I.NE.N) GO TO 160
c end of current block ... reset zero pivots and ising.
        DO 130 J=ISING,I
          IF ((LENR(J)-LENRL(J)).EQ.0) GO TO 130
          JJ = IW(J) + LENRL(J)
          A(JJ) = ZERO
  130   CONTINUE
        ISING = 0
        GO TO 160
c matrix had non-zero pivot in ma30a/ad at this stage.
  140   IF (PIVPOS.GT.IFIN) GO TO 170
        IF (A(PIVPOS).EQ.ZERO) GO TO 170
        IF (.NOT.STAB) GO TO 160
        ROWMAX = ZERO
        DO 150 JJ=PIVPOS,IFIN
          ROWMAX = DMAX1(ROWMAX,DABS(A(JJ)))
  150   CONTINUE
        IF (DABS(A(PIVPOS))/ROWMAX.GE.RMIN) GO TO 160
        IFLAG = I
        RMIN = DABS(A(PIVPOS))/ROWMAX
c   ****    end of main loop    ****
  160 CONTINUE
c
      GO TO 180
c   ***   error return   ***
  170 IF (LP.NE.0) WRITE (LP,99999) I
      IFLAG = -I
c
  180 RETURN
99999 FORMAT ('ERROR RETURN FROM MA30B/BD SINGULARITY DETECTED IN RO',
     * 'W', I8)
      END
      SUBROUTINE MA30CD(N, ICN, A, LICN, LENR, LENRL, LENOFF, IDISP, IP,
     * IQ, X, W, MTYPE)

c*********************************************************************72
c
cc MA30CD solves A*x=b or A'x=b after factorization by MA30AD or MA30BD.
c
c  Discussion:
c
c    The routine uses the factors produced by ma30a/ad or ma30b/bd to solve
c    ax=b or a transpose x=b when the matrix p1*a*q1 (paq) is block
c    lower triangular (including the case of only one diagonal
c    block).
c
c  Modified:
c
c    08 January 2013
c
c we now describe the argument list for ma30c/cd.
c n  is an integer variable set to the order of the matrix. it is not
c     altered by the subroutine.
c icn is an integer array of length licn. entries idisp(1) to
c     idisp(2) should be unchanged since the last call to ma30a/ad. if
c     the matrix has more than one diagonal block, then column indices
c     corresponding to non-zeros in sub-diagonal blocks of paq must
c     appear in positions 1 to idisp(1)-1. for the same row those
c     entries must be contiguous, with those in row i preceding those
c     in row i+1 (i=1,...,n-1) and no wasted space between rows.
c     entries may be in any order within each row. it is not altered
c     by ma30c/cd.
c a  is a real/double precision array of length licn.  entries
c     idisp(1) to idisp(2) should be unchanged since the last call to
c     ma30a/ad or ma30b/bd.  if the matrix has more than one diagonal
c     block, then the values of the non-zeros in sub-diagonal blocks
c     must be in positions 1 to idisp(1)-1 in the order given by icn.
c     it is not altered by ma30c/cd.
c licn  is an integer variable set to the size of arrays icn and a.
c     it is not altered by ma30c/cd.
c lenr,lenrl are integer arrays of length n which should be
c     unchanged since the last call to ma30a/ad. they are not altered
c     by ma30c/cd.
c lenoff  is an integer array of length n. if the matrix paq (or
c     p1*a*q1) has more than one diagonal block, then lenoff(i),
c     i=1,...,n should be set to the number of non-zeros in row i of
c     the matrix paq which are in sub-diagonal blocks.  if there is
c     only one diagonal block then lenoff(1) may be set to -1, in
c     which case the other entries of lenoff are never accessed. it is
c     not altered by ma30c/cd.
c idisp  is an integer array of length 2 which should be unchanged
c     since the last call to ma30a/ad. it is not altered by ma30c/cd.
c ip,iq are integer arrays of length n which should be unchanged
c     since the last call to ma30a/ad. they are not altered by
c     ma30c/cd.
c x is a real/double precision array of length n. it must be set by
c     the user to the values of the right hand side vector b for the
c     equations being solved.  on exit from ma30c/cd it will be equal
c     to the solution x required.
c w  is a real/double precision array of length n which is used as
c     workspace by ma30c/cd.
c mtype is an integer variable which must be set by the user. if
c     mtype=1, then the solution to the system ax=b is returned; any
c     other value for mtype will return the solution to the system a
c     transpose x=b. it is not altered by ma30c/cd.
c
      DOUBLE PRECISION A(LICN), X(N), W(N), WII, WI, RESID, ZERO
      LOGICAL NEG, NOBLOC
      INTEGER IDISP(2)
      INTEGER ICN(LICN), LENR(N), LENRL(N), LENOFF(N), IP(N), IQ(N)
c see block data for comments on variables in common.
      COMMON /MA30HD/ RESID
      DATA ZERO /0.0D0/
c
c the final value of resid is the maximum residual for an inconsistent
c     set of equations.
      RESID = ZERO
c nobloc is .true. if subroutine block has been used previously and
c     is .false. otherwise.  the value .false. means that lenoff
c     will not be subsequently accessed.
      NOBLOC = LENOFF(1).LT.0
      IF (MTYPE.NE.1) GO TO 140
c
c we now solve   a * x = b.
c neg is used to indicate when the last row in a block has been
c     reached.  it is then set to true whereafter backsubstitution is
c     performed on the block.
      NEG = .FALSE.
c ip(n) is negated so that the last row of the last block can be
c     recognised.  it is reset to its positive value on exit.
      IP(N) = -IP(N)
c preorder vector ... w(i) = x(ip(i))
      DO 10 II=1,N
        I = IP(II)
        I = IABS(I)
        W(II) = X(I)
   10 CONTINUE
c lt holds the position of the first non-zero in the current row of the
c     off-diagonal blocks.
      LT = 1
c ifirst holds the index of the first row in the current block.
      IFIRST = 1
c iblock holds the position of the first non-zero in the current row
c     of the lu decomposition of the diagonal blocks.
      IBLOCK = IDISP(1)
c if i is not the last row of a block, then a pass through this loop
c     adds the inner product of row i of the off-diagonal blocks and w
c     to w and performs forward elimination using row i of the lu
c     decomposition.   if i is the last row of a block then, after
c     performing these aforementioned operations, backsubstitution is
c     performed using the rows of the block.
      DO 120 I=1,N
        WI = W(I)
        IF (NOBLOC) GO TO 30
        IF (LENOFF(I).EQ.0) GO TO 30
c operations using lower triangular blocks.
c ltend is the end of row i in the off-diagonal blocks.
        LTEND = LT + LENOFF(I) - 1
        DO 20 JJ=LT,LTEND
          J = ICN(JJ)
          WI = WI - A(JJ)*W(J)
   20   CONTINUE
c lt is set the beginning of the next off-diagonal row.
        LT = LTEND + 1
c set neg to .true. if we are on the last row of the block.
   30   IF (IP(I).LT.0) NEG = .TRUE.
        IF (LENRL(I).EQ.0) GO TO 50
c forward elimination phase.
c iend is the end of the l part of row i in the lu decomposition.
        IEND = IBLOCK + LENRL(I) - 1
        DO 40 JJ=IBLOCK,IEND
          J = ICN(JJ)
          WI = WI + A(JJ)*W(J)
   40   CONTINUE
c iblock is adjusted to point to the start of the next row.
   50   IBLOCK = IBLOCK + LENR(I)
        W(I) = WI
        IF (.NOT.NEG) GO TO 120
c back substitution phase.
c j1 is position in a/icn after end of block beginning in row ifirst
c     and ending in row i.
        J1 = IBLOCK
c are there any singularities in this block?  if not, continue with
c     the backsubstitution.
        IB = I
        IF (IQ(I).GT.0) GO TO 70
        DO 60 III=IFIRST,I
          IB = I - III + IFIRST
          IF (IQ(IB).GT.0) GO TO 70
          J1 = J1 - LENR(IB)
          RESID = DMAX1(RESID,DABS(W(IB)))
          W(IB) = ZERO
   60   CONTINUE
c entire block is singular.
        GO TO 110
c each pass through this loop performs the back-substitution
c     operations for a single row, starting at the end of the block and
c     working through it in reverse order.
   70   DO 100 III=IFIRST,IB
          II = IB - III + IFIRST
c j2 is end of row ii.
          J2 = J1 - 1
c j1 is beginning of row ii.
          J1 = J1 - LENR(II)
c jpiv is the position of the pivot in row ii.
          JPIV = J1 + LENRL(II)
          JPIVP1 = JPIV + 1
c jump if row  ii of u has no non-zeros.
          IF (J2.LT.JPIVP1) GO TO 90
          WII = W(II)
          DO 80 JJ=JPIVP1,J2
            J = ICN(JJ)
            WII = WII - A(JJ)*W(J)
   80     CONTINUE
          W(II) = WII
   90     W(II) = W(II)/A(JPIV)
  100   CONTINUE
  110   IFIRST = I + 1
        NEG = .FALSE.
  120 CONTINUE
c
c reorder solution vector ... x(i) = w(iqinverse(i))
      DO 130 II=1,N
        I = IQ(II)
        I = IABS(I)
        X(I) = W(II)
  130 CONTINUE
      IP(N) = -IP(N)
      GO TO 320
c
c
c we now solve   atranspose * x = b.
c preorder vector ... w(i)=x(iq(i))
  140 DO 150 II=1,N
        I = IQ(II)
        I = IABS(I)
        W(II) = X(I)
  150 CONTINUE
c lj1 points to the beginning the current row in the off-diagonal
c     blocks.
      LJ1 = IDISP(1)
c iblock is initialized to point to the beginning of the block after
c     the last one ]
      IBLOCK = IDISP(2) + 1
c ilast is the last row in the current block.
      ILAST = N
c iblend points to the position after the last non-zero in the
c     current block.
      IBLEND = IBLOCK
c each pass through this loop operates with one diagonal block and
c     the off-diagonal part of the matrix corresponding to the rows
c     of this block.  the blocks are taken in reverse order and the
c     number of times the loop is entered is min(n,no. blocks+1).
      DO 290 NUMBLK=1,N
        IF (ILAST.EQ.0) GO TO 300
        IBLOCK = IBLOCK - LENR(ILAST)
c this loop finds the index of the first row in the current block..
c     it is first and iblock is set to the position of the beginning
c     of this first row.
        DO 160 K=1,N
          II = ILAST - K
          IF (II.EQ.0) GO TO 170
          IF (IP(II).LT.0) GO TO 170
          IBLOCK = IBLOCK - LENR(II)
  160   CONTINUE
  170   IFIRST = II + 1
c j1 points to the position of the beginning of row i (lt part) or pivot
        J1 = IBLOCK
c forward elimination.
c each pass through this loop performs the operations for one row of the
c     block.  if the corresponding entry of w is zero then the
c     operations can be avoided.
        DO 210 I=IFIRST,ILAST
          IF (W(I).EQ.ZERO) GO TO 200
c jump if row i singular.
          IF (IQ(I).LT.0) GO TO 220
c j2 first points to the pivot in row i and then is made to point to the
c     first non-zero in the u transpose part of the row.
          J2 = J1 + LENRL(I)
          WI = W(I)/A(J2)
          IF (LENR(I)-LENRL(I).EQ.1) GO TO 190
          J2 = J2 + 1
c j3 points to the end of row i.
          J3 = J1 + LENR(I) - 1
          DO 180 JJ=J2,J3
            J = ICN(JJ)
            W(J) = W(J) - A(JJ)*WI
  180     CONTINUE
  190     W(I) = WI
  200     J1 = J1 + LENR(I)
  210   CONTINUE
        GO TO 240
c deals with rest of block which is singular.
  220   DO 230 II=I,ILAST
          RESID = DMAX1(RESID,DABS(W(II)))
          W(II) = ZERO
  230   CONTINUE
c back substitution.
c this loop does the back substitution on the rows of the block in
c     the reverse order doing it simultaneously on the l transpose part
c     of the diagonal blocks and the off-diagonal blocks.
  240   J1 = IBLEND
        DO 280 IBACK=IFIRST,ILAST
          I = ILAST - IBACK + IFIRST
c j1 points to the beginning of row i.
          J1 = J1 - LENR(I)
          IF (LENRL(I).EQ.0) GO TO 260
c j2 points to the end of the l transpose part of row i.
          J2 = J1 + LENRL(I) - 1
          DO 250 JJ=J1,J2
            J = ICN(JJ)
            W(J) = W(J) + A(JJ)*W(I)
  250     CONTINUE
  260     IF (NOBLOC) GO TO 280
c operations using lower triangular blocks.
          IF (LENOFF(I).EQ.0) GO TO 280
c lj2 points to the end of row i of the off-diagonal blocks.
          LJ2 = LJ1 - 1
c lj1 points to the beginning of row i of the off-diagonal blocks.
          LJ1 = LJ1 - LENOFF(I)
          DO 270 JJ=LJ1,LJ2
            J = ICN(JJ)
            W(J) = W(J) - A(JJ)*W(I)
  270     CONTINUE
  280   CONTINUE
        IBLEND = J1
        ILAST = IFIRST - 1
  290 CONTINUE
c reorder solution vector ... x(i)=w(ipinverse(i))
  300 DO 310 II=1,N
        I = IP(II)
        I = IABS(I)
        X(I) = W(II)
  310 CONTINUE
c
  320 RETURN
      END
      SUBROUTINE MA30DD(A, ICN, IPTR, N, IACTIV, ITOP, REALS)

c*********************************************************************72
c
cc MA30DD performs garbage collection.
c
c  Discussion:
c
c    The routine performs garbarge collection operations on the
c    arrays a, icn and irn.
c
c  Modified:
c
c    08 January 2013
c
c iactiv is the first position in arrays a/icn from which the compress
c     starts.  on exit, iactiv equals the position of the first entry
c     in the compressed part of a/icn
c
      DOUBLE PRECISION A(ITOP)
      LOGICAL REALS
      INTEGER IPTR(N)
      INTEGER ICN(ITOP)
c see block data for comments on variables in common.
      COMMON /MA30FD/ IRNCP, ICNCP, IRANK, MINIRN, MINICN
c
      IF (REALS) ICNCP = ICNCP + 1
      IF (.NOT.REALS) IRNCP = IRNCP + 1
c set the first non-zero entry in each row to the negative of the
c     row/col number and hold this row/col index in the row/col
c     pointer.  this is so that the beginning of each row/col can
c     be recognized in the subsequent scan.
      DO 10 J=1,N
        K = IPTR(J)
        IF (K.LT.IACTIV) GO TO 10
        IPTR(J) = ICN(K)
        ICN(K) = -J
   10 CONTINUE
      KN = ITOP + 1
      KL = ITOP - IACTIV + 1
c go through arrays in reverse order compressing to the back so
c     that there are no zeros held in positions iactiv to itop in icn.
c     reset first entry of each row/col and pointer array iptr.
      DO 30 K=1,KL
        JPOS = ITOP - K + 1
        IF (ICN(JPOS).EQ.0) GO TO 30
        KN = KN - 1
        IF (REALS) A(KN) = A(JPOS)
        IF (ICN(JPOS).GE.0) GO TO 20
c first non-zero of row/col has been located
        J = -ICN(JPOS)
        ICN(JPOS) = IPTR(J)
        IPTR(J) = KN
   20   ICN(KN) = ICN(JPOS)
   30 CONTINUE
      IACTIV = KN
      RETURN
      END
      SUBROUTINE MC13D(N,ICN,LICN,IP,LENR,IOR,IB,NUM,IW)

c*********************************************************************72
c
cc MC13D calls MC13E after dividing up the workspace.
c
c  Modified:
c
c    08 January 2013
c
      INTEGER IP(N)
      INTEGER ICN(LICN),LENR(N),IOR(N),IB(N),IW(N,3)

      CALL MC13E(N,ICN,LICN,IP,LENR,IOR,IB,NUM,IW(1,1),IW(1,2),IW(1,3))

      RETURN
      END
      SUBROUTINE MC13E(N,ICN,LICN,IP,LENR,ARP,IB,NUM,LOWL,NUMB,PREV)

c*********************************************************************72
c
cc MC13E finds a symmetric permutation to block lower triangular form.
c
c  Modified:
c
c    08 January 2013
c
      INTEGER STP,DUMMY
      INTEGER IP(N)
c
c arp(i) is one less than the number of unsearched edges leaving
c     node i.  at the end of the algorithm it is set to a
c     permutation which puts the matrix in block lower
c     triangular form.
c ib(i) is the position in the ordering of the start of the ith
c     block.  ib(n+1-i) holds the node number of the ith node
c     on the stack.
c lowl(i) is the smallest stack position of any node to which a path
c     from node i has been found.  it is set to n+1 when node i
c     is removed from the stack.
c numb(i) is the position of node i in the stack if it is on
c     it, is the permuted order of node i for those nodes
c     whose final position has been found and is otherwise zero.
c prev(i) is the node at the end of the path when node i was
c     placed on the stack.
      INTEGER ICN(LICN),LENR(N),ARP(N),IB(N),LOWL(N),NUMB(N),
     1PREV(N)
c
c
c   icnt is the number of nodes whose positions in final ordering have
c     been found.
      ICNT=0
c num is the number of blocks that have been found.
      NUM=0
      NNM1=N+N-1
c
c initialization of arrays.
c
      DO J=1,N
        NUMB(J)=0
        ARP(J)=LENR(J)-1
      end do
c
c
      DO 120 ISN=1,N
c look for a starting node
      IF (NUMB(ISN).NE.0) GO TO 120
      IV=ISN
c ist is the number of nodes on the stack ... it is the stack pointer.
      IST=1
c put node iv at beginning of stack.
      LOWL(IV)=1
      NUMB(IV)=1
      IB(N)=IV
c
c the body of this loop puts a new node on the stack or backtracks.
      DO 110 DUMMY=1,NNM1
      I1=ARP(IV)
c have all edges leaving node iv been searched.
      IF (I1.LT.0) GO TO 60
      I2=IP(IV)+LENR(IV)-1
      I1=I2-I1
c
c look at edges leaving node iv until one enters a new node or
c     all edges are exhausted.
      DO 50 II=I1,I2
      IW=ICN(II)
c has node iw been on stack already.
      IF (NUMB(IW).EQ.0) GO TO 100
c update value of lowl(iv) if necessary.
  50  LOWL(IV)=MIN0(LOWL(IV),LOWL(IW))
c
c there are no more edges leaving node iv.
      ARP(IV)=-1
c is node iv the root of a block.
   60 IF (LOWL(IV).LT.NUMB(IV)) GO TO 90
c
c order nodes in a block.
      NUM=NUM+1
      IST1=N+1-IST
      LCNT=ICNT+1
c peel block off the top of the stack starting at the top and
c     working down to the root of the block.
      DO 70 STP=IST1,N
      IW=IB(STP)
      LOWL(IW)=N+1
      ICNT=ICNT+1
      NUMB(IW)=ICNT
      IF (IW.EQ.IV) GO TO 80
   70 CONTINUE
   80 IST=N-STP
      IB(NUM)=LCNT
c are there any nodes left on the stack.
      IF (IST.NE.0) GO TO 90
c have all the nodes been ordered.
      IF (ICNT.LT.N) GO TO 120
      GO TO 130
c
c backtrack to previous node on path.
   90 IW=IV
      IV=PREV(IV)
c update value of lowl(iv) if necessary.
      LOWL(IV)=MIN0(LOWL(IV),LOWL(IW))
      GO TO 110
c
c put new node on the stack.
 100  ARP(IV)=I2-II-1
      PREV(IW)=IV
      IV=IW
      IST=IST+1
      LOWL(IV)=IST
      NUMB(IV)=IST
      K=N+1-IST
      IB(K)=IV
  110 CONTINUE
c
  120 CONTINUE
c
c
c put permutation in the required form.
c
  130 DO I=1,N
        II=NUMB(I)
        ARP(II)=I
      end do

      RETURN
      END
      SUBROUTINE MC20AD(NC,MAXA,A,INUM,JPTR,JNUM,JDISP)

c*********************************************************************72
c
cc MC20AD sorts the matrix into row order.
c
c  Modified:
c
c    08 January 2013
c
      INTEGER   INUM(MAXA),JNUM(MAXA)
      DOUBLE PRECISION A(MAXA),ACE,ACEP
      DIMENSION JPTR(NC)

      NULL=-JDISP
c**      clear jptr
      DO 60 J=1,NC
   60 JPTR(J)=0
c**      count the number of elements in each column.
      DO 120 K=1,MAXA
      J=JNUM(K)+JDISP
      JPTR(J)=JPTR(J)+1
  120 CONTINUE
c**      set the jptr array
      K=1
      DO 150 J=1,NC
      KR=K+JPTR(J)
      JPTR(J)=K
  150 K=KR
c
c**      reorder the elements into column order.  the algorithm is an
c        in-place sort and is of order maxa.
      DO 230 I=1,MAXA
c        establish the current entry.
      JCE=JNUM(I)+JDISP
      IF(JCE.EQ.0) GO TO 230
      ACE=A(I)
      ICE=INUM(I)
c        clear the location vacated.
      JNUM(I)=NULL
c        chain from current entry to store items.
      DO 200 J=1,MAXA
c        current entry not in correct position.  determine correct
c        position to store entry.
      LOC=JPTR(JCE)
      JPTR(JCE)=JPTR(JCE)+1
c        save contents of that location.
      ACEP=A(LOC)
      ICEP=INUM(LOC)
      JCEP=JNUM(LOC)
c        store current entry.
      A(LOC)=ACE
      INUM(LOC)=ICE
      JNUM(LOC)=NULL
c        check if next current entry needs to be processed.
      IF(JCEP.EQ.NULL) GO TO 230
c        it does.  copy into current entry.
      ACE=ACEP
      ICE=ICEP
  200 JCE=JCEP+JDISP
c
  230 CONTINUE
c
c**      reset jptr vector.
      JA=1
      DO 250 J=1,NC
      JB=JPTR(J)
      JPTR(J)=JA
  250 JA=JB
      RETURN
      END
      SUBROUTINE MC20BD(NC,MAXA,A,INUM,JPTR)

c*********************************************************************72
c
cc MC20BD sorts the nonzeros of a sparse matrix by columns.
c
c  Modified:
c
c    08 January 2013
c
      DOUBLE PRECISION A(MAXA),ACE
      INTEGER   INUM(MAXA)
      DIMENSION JPTR(NC)

      KMAX=MAXA
      DO 30 JJ=1,NC
      J=NC+1-JJ
      KLO=JPTR(J)+1
      IF(KLO.GT.KMAX)GO TO 30
      KOR=KMAX
      DO 25 KDUMMY=KLO,KMAX
C ITEMS KOR, KOR+1, .... ,KMAX ARE IN ORDER
      ACE=A(KOR-1)
      ICE=INUM(KOR-1)
      DO 10 K=KOR,KMAX
      IK=INUM(K)
      IF(IABS(ICE).LE.IABS(IK))GO TO 20
      INUM(K-1)=IK
10    A(K-1)=A(K)
      K=KMAX+1
20    INUM(K-1)=ICE
      A(K-1)=ACE
25    KOR=KOR-1
C        NEXT COLUMN
30    KMAX=KLO-2
      RETURN
      END
      SUBROUTINE MC21A(N,ICN,LICN,IP,LENR,IPERM,NUMNZ,IW)

c*********************************************************************72
c
cc MC21A calls MC21B after dividing up workspace.
c
c  Modified:
c
c    08 January 2013
c
      INTEGER IP(N)
      INTEGER ICN(LICN),LENR(N),IPERM(N),IW(N,4)

      CALL MC21B(N,ICN,LICN,IP,LENR,IPERM,NUMNZ,IW(1,1),IW(1,2),IW(1,3),
     1IW(1,4))

      RETURN
      END
      SUBROUTINE MC21B(N,ICN,LICN,IP,LENR,IPERM,NUMNZ,PR,ARP,CV,OUT)

c*********************************************************************72
c
cc MC21B finds a row permutation to make the diagonal zero free.
c
c  Modified:
c
c    08 January 2013
c
c   pr(i) is the previous row to i in the depth first search.
c it is used as a work array in the sorting algorithm.
c   elements (iperm(i),i) i=1, ... n  are non-zero at the end of the
c algorithm unless n assignments have not been made.  in which case
c (iperm(i),i) will be zero for n-numnz entries.
c   cv(i) is the most recent row extension at which column i
c was visited.
c   arp(i) is one less than the number of non-zeros in row i
c which have not been scanned when looking for a cheap assignment.
c   out(i) is one less than the number of non-zeros in row i
c which have not been scanned during one pass through the main loop.
c
      INTEGER IP(N)
      INTEGER ICN(LICN),LENR(N),IPERM(N),PR(N),CV(N),
     1ARP(N),OUT(N)
c
c   initialization of arrays.
      DO 10 I=1,N
      ARP(I)=LENR(I)-1
      CV(I)=0
   10 IPERM(I)=0
      NUMNZ=0
c
c
c   main loop.
c   each pass round this loop either results in a new assignment
c or gives a row with no assignment.
      DO 130 JORD=1,N
      J=JORD
      PR(J)=-1
      DO 100 K=1,JORD
c look for a cheap assignment
      IN1=ARP(J)
      IF (IN1.LT.0) GO TO 60
      IN2=IP(J)+LENR(J)-1
      IN1=IN2-IN1
      DO 50 II=IN1,IN2
      I=ICN(II)
      IF (IPERM(I).EQ.0) GO TO 110
   50 CONTINUE
c   no cheap assignment in row.
      ARP(J)=-1
c   begin looking for assignment chain starting with row j.
   60 OUT(J)=LENR(J)-1
c inner loop.  extends chain by one or backtracks.
      DO 90 KK=1,JORD
      IN1=OUT(J)
      IF (IN1.LT.0) GO TO 80
      IN2=IP(J)+LENR(J)-1
      IN1=IN2-IN1
c forward scan.
      DO 70 II=IN1,IN2
      I=ICN(II)
      IF (CV(I).EQ.JORD) GO TO 70
c   column i has not yet been accessed during this pass.
      J1=J
      J=IPERM(I)
      CV(I)=JORD
      PR(J)=J1
      OUT(J1)=IN2-II-1
      GO TO 100
   70 CONTINUE
c
c   backtracking step.
   80 J=PR(J)
      IF (J.EQ.-1) GO TO 130
   90 CONTINUE
c
  100 CONTINUE
c
c   new assignment is made.
  110 IPERM(I)=J
      ARP(J)=IN2-II-1
      NUMNZ=NUMNZ+1
      DO 120 K=1,JORD
      J=PR(J)
      IF (J.EQ.-1) GO TO 130
      II=IP(J)+LENR(J)-OUT(J)-2
      I=ICN(II)
      IPERM(I)=J
  120 CONTINUE
c
  130 CONTINUE
c
c   if matrix is structurally singular, we now complete the
c permutation iperm.
      IF (NUMNZ.EQ.N) RETURN
      DO 140 I=1,N
  140 ARP(I)=0
      K=0
      DO 160 I=1,N
      IF (IPERM(I).NE.0) GO TO 150
      K=K+1
      OUT(K)=I
      GO TO 160
  150 J=IPERM(I)
      ARP(J)=I
  160 CONTINUE
      K=0
      DO 170 I=1,N
      IF (ARP(I).NE.0) GO TO 170
      K=K+1
      IOUTK=OUT(K)
      IPERM(IOUTK)=I
  170 CONTINUE
      RETURN
      END
      SUBROUTINE MC22AD(N,ICN,A,NZ,LENROW,IP,IQ,IW,IW1)

c*********************************************************************72
c
cc MC22AD reorders off-diagonal blocks according to the pivot.
c
c  Modified:
c
c    08 January 2013
c
      DOUBLE PRECISION A(NZ),AVAL
      INTEGER IW(N,2)
      INTEGER   ICN(NZ),LENROW(N),IP(N),IQ(N),IW1(NZ)
      IF (NZ.LE.0) GO TO 1000
      IF (N.LE.0) GO TO 1000
c set start of row i in iw(i,1) and lenrow(i) in iw(i,2)
      IW(1,1)=1
      IW(1,2)=LENROW(1)
      DO 10 I=2,N
      IW(I,1)=IW(I-1,1)+LENROW(I-1)
 10   IW(I,2)=LENROW(I)
c permute lenrow according to ip.  set off-sets for new position
c     of row iold in iw(iold,1) and put old row indices in iw1 in
c     positions corresponding to the new position of this row in a/icn.
      JJ=1
      DO 20 I=1,N
      IOLD=IP(I)
      IOLD=IABS(IOLD)
      LENGTH=IW(IOLD,2)
      LENROW(I)=LENGTH
      IF (LENGTH.EQ.0) GO TO 20
      IW(IOLD,1)=IW(IOLD,1)-JJ
      J2=JJ+LENGTH-1
      DO 15 J=JJ,J2
 15   IW1(J)=IOLD
      JJ=J2+1
 20   CONTINUE
c set inverse permutation to iq in iw(.,2).
      DO 30 I=1,N
      IOLD=IQ(I)
      IOLD=IABS(IOLD)
 30   IW(IOLD,2)=I
c permute a and icn in place, changing to new column numbers.
c
c ***   main loop   ***
c each pass through this loop places a closed chain of column indices
c     in their new (and final) positions ... this is recorded by
c     setting the iw1 entry to zero so that any which are subsequently
c     encountered during this major scan can be bypassed.
      DO 200 I=1,NZ
      IOLD=IW1(I)
      IF (IOLD.EQ.0) GO TO 200
      IPOS=I
      JVAL=ICN(I)
c if row iold is in same positions after permutation go to 150.
      IF (IW(IOLD,1).EQ.0) GO TO 150
      AVAL=A(I)
c **  chain loop  **
c each pass through this loop places one (permuted) column index
c     in its final position  .. viz. ipos.
      DO 100 ICHAIN=1,NZ
c newpos is the original position in a/icn of the element to be placed
c in position ipos.  it is also the position of the next element in
c     the chain.
      NEWPOS=IPOS+IW(IOLD,1)
c is chain complete ?
      IF (NEWPOS.EQ.I) GO TO 130
      A(IPOS)=A(NEWPOS)
      JNUM=ICN(NEWPOS)
      ICN(IPOS)=IW(JNUM,2)
      IPOS=NEWPOS
      IOLD=IW1(IPOS)
      IW1(IPOS)=0
c **  end of chain loop  **
 100  CONTINUE
 130  A(IPOS)=AVAL
 150  ICN(IPOS)=IW(JVAL,2)
c ***   end of main loop   ***
 200  CONTINUE
c
 1000 RETURN
      END
      SUBROUTINE MC23AD(N,ICN,A,LICN,LENR,IDISP,IP,IQ,LENOFF,IW,IW1)

c*********************************************************************72
c
cc MC23AD performs block triangularization.
c
c  Modified:
c
c    08 January 2013
c
      DOUBLE PRECISION A(LICN)
      INTEGER IDISP(2),IW1(N,2)
      LOGICAL ABORT
      INTEGER   ICN(LICN),LENR(N),IP(N),IQ(N),LENOFF(N),IW(N,5)
      COMMON /MC23BD/ LP,NUMNZ,NUM,LARGE,ABORT
c input ... n,icn .. a,icn,lenr ....
c
c set up pointers iw(.,1) to the beginning of the rows and set lenoff
c     equal to lenr.
      IW1(1,1)=1
      LENOFF(1)=LENR(1)
      IF (N.EQ.1) GO TO 20
      DO 10 I=2,N
      LENOFF(I)=LENR(I)
   10 IW1(I,1)=IW1(I-1,1)+LENR(I-1)
c idisp(1) points to the first position in a/icn after the
c     off-diagonal blocks and untreated rows.
   20 IDISP(1)=IW1(N,1)+LENR(N)
c
c find row permutation ip to make diagonal zero-free.
      CALL MC21A(N,ICN,LICN,IW1,LENR,IP,NUMNZ,IW)
c
c possible error return for structurally singular matrices.
      IF (NUMNZ.NE.N.AND.ABORT) GO TO 170
c
c iw1(.,2) and lenr are permutations of iw1(.,1) and lenr/lenoff
c     suitable for entry
c     to mc13d since matrix with these row pointer and length arrays
c     has maximum number of non-zeros on the diagonal.
      DO 30 II=1,N
      I=IP(II)
      IW1(II,2)=IW1(I,1)
   30 LENR(II)=LENOFF(I)
c
c find symmetric permutation iq to block lower triangular form.
      CALL MC13D(N,ICN,LICN,IW1(1,2),LENR,IQ,IW(1,4),NUM,IW)
c
      IF (NUM.NE.1) GO TO 60
c
c action taken if matrix is irreducible.
c whole matrix is just moved to the end of the storage.
      DO 40 I=1,N
      LENR(I)=LENOFF(I)
      IP(I)=I
   40 IQ(I)=I
      LENOFF(1)=-1
c idisp(1) is the first position after the last element in the
c     off-diagonal blocks and untreated rows.
      NZ=IDISP(1)-1
      IDISP(1)=1
c idisp(2) is the position in a/icn of the first element in the
c     diagonal blocks.
      IDISP(2)=LICN-NZ+1
      LARGE=N
      IF (NZ.EQ.LICN) GO TO 230
      DO 50 K=1,NZ
      J=NZ-K+1
      JJ=LICN-K+1
      A(JJ)=A(J)
   50 ICN(JJ)=ICN(J)
c 230 = return
      GO TO 230
c
c data structure reordered.
c
c form composite row permutation ... ip(i) = ip(iq(i)).
   60 DO 70 II=1,N
      I=IQ(II)
   70 IW(II,1)=IP(I)
      DO 80 I=1,N
   80 IP(I)=IW(I,1)
c
c run through blocks in reverse order separating diagonal blocks
c     which are moved to the end of the storage.  elements in
c     off-diagonal blocks are left in place unless a compress is
c     necessary.
c
c ibeg indicates the lowest value of j for which icn(j) has been
c     set to zero when element in position j was moved to the
c     diagonal block part of storage.
      IBEG=LICN+1
c iend is the position of the first element of those treated rows
c     which are in diagonal blocks.
      IEND=LICN+1
c large is the dimension of the largest block encountered so far.
      LARGE=0
c
c num is the number of diagonal blocks.
      DO 150 K=1,NUM
      IBLOCK=NUM-K+1
c i1 is first row (in permuted form) of block iblock.
c i2 is last row (in permuted form) of block iblock.
      I1=IW(IBLOCK,4)
      I2=N
      IF (K.NE.1) I2=IW(IBLOCK+1,4)-1
      LARGE=MAX0(LARGE,I2-I1+1)
c go through the rows of block iblock in the reverse order.
      DO 140 II=I1,I2
      INEW=I2-II+I1
c we now deal with row inew in permuted form (row iold in original
c     matrix).
      IOLD=IP(INEW)
c if there is space to move up diagonal block portion of row go to 110
      IF (IEND-IDISP(1).GE.LENOFF(IOLD)) GO TO 110
c
c in-line compress.
c moves separated off-diagonal elements and untreated rows to
c     front of storage.
      JNPOS=IBEG
      ILEND=IDISP(1)-1
      IF (ILEND.LT.IBEG) GO TO 190
      DO 90 J=IBEG,ILEND
      IF (ICN(J).EQ.0) GO TO 90
      ICN(JNPOS)=ICN(J)
      A(JNPOS)=A(J)
      JNPOS=JNPOS+1
   90 CONTINUE
      IDISP(1)=JNPOS
      IF (IEND-JNPOS.LT.LENOFF(IOLD)) GO TO 190
      IBEG=LICN+1
c reset pointers to the beginning of the rows.
      DO 100 I=2,N
  100 IW1(I,1)=IW1(I-1,1)+LENOFF(I-1)
c
c row iold is now split into diag. and off-diag. parts.
  110 IROWB=IW1(IOLD,1)
      LENI=0
      IROWE=IROWB+LENOFF(IOLD)-1
c backward scan of whole of row iold (in original matrix).
      IF (IROWE.LT.IROWB) GO TO 130
      DO 120 JJ=IROWB,IROWE
      J=IROWE-JJ+IROWB
      JOLD=ICN(J)
c iw(.,2) holds the inverse permutation to iq.
c     ..... it was set to this in mc13d.
      JNEW=IW(JOLD,2)
c if (jnew.lt.i1) then ....
c element is in off-diagonal block and so is left in situ.
      IF (JNEW.LT.I1) GO TO 120
c element is in diagonal block and is moved to the end of the storage.
      IEND=IEND-1
      A(IEND)=A(J)
      ICN(IEND)=JNEW
      IBEG=MIN0(IBEG,J)
      ICN(J)=0
      LENI=LENI+1
  120 CONTINUE
c
      LENOFF(IOLD)=LENOFF(IOLD)-LENI
  130 LENR(INEW)=LENI
  140 CONTINUE
c
      IP(I2)=-IP(I2)
  150 CONTINUE
c resets ip(n) to positive value.
      IP(N)=-IP(N)
c idisp(2) is position of first element in diagonal blocks.
      IDISP(2)=IEND
c
c this compress is used to move all off-diagonal elements to the
c     front of the storage.
      IF (IBEG.GT.LICN) GO TO 230
      JNPOS=IBEG
      ILEND=IDISP(1)-1
      DO 160 J=IBEG,ILEND
      IF (ICN(J).EQ.0) GO TO 160
      ICN(JNPOS)=ICN(J)
      A(JNPOS)=A(J)
      JNPOS=JNPOS+1
  160 CONTINUE
c idisp(1) is first position after last element of off-diagonal blocks.
      IDISP(1)=JNPOS
      GO TO 230
c
c error return
c
  170 IF (LP.NE.0) WRITE(LP,180) NUMNZ
  180 FORMAT(33X,41H MATRIX IS STRUCTURALLY SINGULAR, RANK = ,I6)
      IDISP(1)=-1
      GO TO 210
  190 IF (LP.NE.0) WRITE(LP,200) N
  200 FORMAT(33X,33H LICN NOT BIG ENOUGH INCREASE BY ,I6)
      IDISP(1)=-2
  210 IF (LP.NE.0) WRITE(LP,220)
  220 FORMAT(' ERROR RETURN FROM MC23AD BECAUSE')
c
  230 RETURN
      END
      SUBROUTINE MC24AD(N,ICN,A,LICN,LENR,LENRL,W)

c*********************************************************************72
c
cc MC24AD calculates the element growth estimate.
c
c  Modified:
c
c    08 January 2013
c
      DOUBLE PRECISION A(LICN),W(N),AMAXL,WROWL,AMAXU,ZERO
      INTEGER   ICN(LICN),LENR(N),LENRL(N)
      DATA ZERO/0.0D0/
      AMAXL=ZERO
      DO 10 I=1,N
 10   W(I)=ZERO
      J0=1
      DO 100 I=1,N
      IF (LENR(I).EQ.0) GO TO 100
      J2=J0+LENR(I)-1
      IF (LENRL(I).EQ.0) GO TO 50
c calculation of 1-norm of l.
      J1=J0+LENRL(I)-1
      WROWL=ZERO
      DO 30 JJ=J0,J1
 30   WROWL=WROWL+DABS(A(JJ))
c amaxl is the maximum norm of columns of l so far found.
      AMAXL=DMAX1(AMAXL,WROWL)
      J0=J1+1
c calculation of norms of columns of u (max-norms).
 50   J0=J0+1
      IF (J0.GT.J2) GO TO 90
      DO 80 JJ=J0,J2
      J=ICN(JJ)
 80   W(J)=DMAX1(DABS(A(JJ)),W(J))
 90   J0=J2+1
 100  CONTINUE
c
c amaxu is set to maximum max-norm of columns of u.
c
      AMAXU=ZERO
      DO I=1,N
        AMAXU=DMAX1(AMAXU,W(I))
      end do
c
c grofac is max u max-norm times max l 1-norm.
c
      W(1)=AMAXL*AMAXU
      RETURN
      END
      BLOCK DATA MABLD1

c*********************************************************************72
c
cc MABLD1 contains block data.
c
c  Modified:
c
c    08 January 2013
c
c comments on all the common block variables are given here even
c     though some are not initialized by block data.
c lp,mp are used by the subroutine as the unit numbers for its warning
c     and diagnostic messages. default value for both is 6 (for line
c     printer output). the user can either reset them to a different
c     stream number or suppress the output by setting them to zero.
c     while lp directs the output of error diagnostics from the
c     principal subroutines and internally called subroutines, mp
c     controls only the output of a message which warns the user that he
c     has input two or more non-zeros a(i), . . ,a(k) with the same row
c     and column indices.  the action taken in this case is to proceed
c     using a numerical value of a(i)+...+a(k). in the absence of other
c     errors, iflag will equal -14 on exit.
c lblock is a logical variable which controls an option of first
c     preordering the matrix to block lower triangular form (using
c     harwell subroutine mc23a). the preordering is performed if lblock
c     is equal to its default value of .true. if lblock is set to
c     .false. , the option is not invoked and the space allocated to
c     ikeep can be reduced to 4*n+1.
c grow is a logical variable. if it is left at its default value of
c     .true. , then on return from ma28a/ad or ma28b/bd, w(1) will give
c     an estimate (an upper bound) of the increase in size of elements
c     encountered during the decomposition. if the matrix is well
c     scaled, then a high value for w(1), relative to the largest entry
c     in the input matrix, indicates that the lu decomposition may be
c     inaccurate and the user should be wary of his results and perhaps
c     increase u for subsequent runs.  we would like to emphasise that
c     this value only relates to the accuracy of our lu decomposition
c     and gives no indication as to the singularity of the matrix or the
c     accuracy of the solution.  this upper bound can be a significant
c     overestimate particularly if the matrix is badly scaled. if an
c     accurate value for the growth is required, lbig (q.v.) should be
c     set to .true.
c eps,rmin are real variables. if, on entry to ma28b/bd, eps is less
c     than one, then rmin will give the smallest ratio of the pivot to
c     the largest element in the corresponding row of the upper
c     triangular factor thus monitoring the stability of successive
c     factorizations. if rmin becomes very large and w(1) from
c     ma28b/bd is also very large, it may be advisable to perform a
c     new decomposition using ma28a/ad.
c resid is a real variable which on exit from ma28c/cd gives the value
c     of the maximum residual over all the equations unsatisfied because
c     of dependency (zero pivots).
c irncp,icncp are integer variables which monitor the adequacy of "elbow
c     room" in irn and a/icn respectively. if either is quite large (say
c     greater than n/10), it will probably pay to increase the size of
c     the corresponding array for subsequent runs. if either is very low
c     or zero then one can perhaps save storage by reducing the size of
c     the corresponding array.
c minirn,minicn are integer variables which, in the event of a
c     successful return (iflag ge 0 or iflag=-14) give the minimum size
c     of irn and a/icn respectively which would enable a successful run
c     on an identical matrix. on an exit with iflag equal to -5, minicn
c     gives the minimum value of icn for success on subsequent runs on
c     an identical matrix. in the event of failure with iflag= -6, -4,
c     -3, -2, or -1, then minicn and minirn give the minimum value of
c     licn and lirn respectively which would be required for a
c     successful decomposition up to the point at which the failure
c     occurred.
c irank is an integer variable which gives an upper bound on the rank of
c     the matrix.
c abort1 is a logical variable with default value .true.  if abort1 is
c     set to .false.  then ma28a/ad will decompose structurally singular
c     matrices (including rectangular ones).
c abort2 is a logical variable with default value .true.  if abort2 is
c     set to .false. then ma28a/ad will decompose numerically singular
c     matrices.
c idisp is an integer array of length 2. on output from ma28a/ad, the
c     indices of the diagonal blocks of the factors lie in positions
c     idisp(1) to idisp(2) of a/icn. this array must be preserved
c     between a call to ma28a/ad and subsequent calls to ma28b/bd,
c     ma28c/cd or ma28i/id.
c tol is a real variable.  if it is set to a positive value, then any
c     non-zero whose modulus is less than tol will be dropped from the
c     factorization.  the factorization will then require less storage
c     but will be inaccurate.  after a run of ma28a/ad with tol positive
c     it is not possible to use ma28b/bd and the user is recommended to
c     use ma28i/id to obtain the solution.  the default value for tol is
c     0.0.
c themax is a real variable.  on exit from ma28a/ad, it will hold the
c     largest entry of the original matrix.
c big is a real variable. if lbig has been set to .true., big will hold
c     the largest entry encountered during the factorization by ma28a/ad
c     or ma28b/bd.
c dxmax is a real variable. on exit from ma28i/id, dxmax will be set to
c     the largest component of the solution.
c errmax is a real variable.  on exit from ma28i/id, if maxit is
c     positive, errmax will be set to the largest component in the
c     estimate of the error.
c dres is a real variable.  on exit from ma28i/id, if maxit is positive,
c     dres will be set to the largest component of the residual.
c cgce is a real variable. it is used by ma28i/id to check the
c     convergence rate.  if the ratio of successive corrections is
c     not less than cgce then we terminate since the convergence
c     rate is adjudged too slow.
c ndrop is an integer variable. if tol has been set positive, on exit
c     from ma28a/ad, ndrop will hold the number of entries dropped from
c     the data structure.
c maxit is an integer variable. it is the maximum number of iterations
c     performed by ma28i/id. it has a default value of 16.
c noiter is an integer variable. it is set by ma28i/id to the number of
c     iterative refinement iterations actually used.
c nsrch is an integer variable. if nsrch is set to a value less than n,
c     then a different pivot option will be employed by ma28a/ad.  this
c     may result in different fill-in and execution time for ma28a/ad.
c     if nsrch is less than or equal to n, the workspace array iw can be
c     reduced in length.  the default value for nsrch is 32768.
c istart is an integer variable. if istart is set to a value other than
c     zero, then the user must supply an estimate of the solution to
c     ma28i/id.  the default value for istart is zero.
c lbig is a logical variable. if lbig is set to .true., the value of the
c     largest element encountered in the factorization by ma28a/ad or
c     ma28b/bd is returned in big.  setting lbig to .true.  will
c     increase the time for ma28a/ad marginally and that for ma28b/bd
c     by about 20%.  the default value for lbig is .false.
c
      DOUBLE PRECISION EPS, RMIN, RESID, TOL, THEMAX, BIG, DXMAX,
     * ERRMAX, DRES, CGCE
      LOGICAL LBLOCK, GROW, ABORT1, ABORT2, LBIG
      COMMON /MA28ED/ LP, MP, LBLOCK, GROW
      COMMON /MA28FD/ EPS, RMIN, RESID, IRNCP, ICNCP, MINIRN, MINICN,
     * IRANK, ABORT1, ABORT2
c     common /ma28gd/ idisp(2)
      COMMON /MA28HD/ TOL, THEMAX, BIG, DXMAX, ERRMAX, DRES, CGCE,
     * NDROP, MAXIT, NOITER, NSRCH, ISTART, LBIG
      DATA EPS /1.0D-4/, TOL /0.0D0/, CGCE /0.5D0/
      DATA MAXIT /16/
      DATA LP /6/, MP /6/, NSRCH /32768/, ISTART /0/
      DATA LBLOCK /.TRUE./, GROW /.TRUE./, LBIG /.FALSE./
      DATA ABORT1 /.TRUE./, ABORT2 /.TRUE./
      END
      BLOCK DATA MABLD2

c*********************************************************************72
c
cc MABLD2 contains block data.
c
c  Modified:
c
c    08 January 2013
c
c although all common block variables do not have default values,
c     we comment on all the common block variables here.
c
c common block ma30e/ed holds control parameters ....
c     common /ma30ed/ lp, abort1, abort2, abort3
c the integer lp is the unit number to which the error messages are
c     sent. lp has a default value of 6.  this default value can be
c     reset by the user, if desired.  a value of 0 suppresses all
c     messages.
c the logical variables abort1,abort2,abort3 are used to control the
c     conditions under which the subroutine will terminate.
c if abort1 is .true. then the subroutine will exit  immediately on
c     detecting structural singularity.
c if abort2 is .true. then the subroutine will exit immediately on
c     detecting numerical singularity.
c if abort3 is .true. then the subroutine will exit immediately when
c     the available space in a/icn is filled up by the previously
c     decomposed, active, and undecomposed parts of the matrix.
c the default values for abort1,abort2,abort3 are set to .true.,.true.
c     and .false. respectively.
c
c the variables in the common block ma30f/fd are used to provide the
c     user with information on the decomposition.
c     common /ma30fd/ irncp, icncp, irank, minirn, minicn
c irncp and icncp are integer variables used to monitor the adequacy
c     of the allocated space in arrays irn and a/icn respectively, by
c     taking account of the number of data management compresses
c     required on these arrays. if irncp or icncp is fairly large (say
c     greater than n/10), it may be advantageous to increase the size
c     of the corresponding array(s).  irncp and icncp are initialized
c     to zero on entry to ma30a/ad and are incremented each time the
c     compressing routine ma30d/dd is entered.
c icncp is the number of compresses on a/icn.
c irncp is the number of compresses on irn.
c irank is an integer variable which gives an estimate (actually an
c     upper bound) of the rank of the matrix. on an exit with iflag
c     equal to 0, this will be equal to n.
c minirn is an integer variable which, after a successful call to
c     ma30a/ad, indicates the minimum length to which irn can be
c     reduced while still permitting a successful decomposition of the
c     same matrix. if, however, the user were to decrease the length
c     of irn to that size, the number of compresses (irncp) may be
c     very high and quite costly. if lirn is not large enough to begin
c     the decomposition on a diagonal block, minirn will be equal to
c     the value required to continue the decomposition and iflag will
c     be set to -3 or -6. a value of lirn slightly greater than this
c     (say about n/2) will usually provide enough space to complete
c     the decomposition on that block. in the event of any other
c     failure minirn gives the minimum size of irn required for a
c     successful decomposition up to that point.
c minicn is an integer variable which after a successful call to
c     ma30a/ad, indicates the minimum size of licn required to enable
c     a successful decomposition. in the event of failure with iflag=
c     -5, minicn will, if abort3 is left set to .false., indicate the
c     minimum length that would be sufficient to prevent this error in
c     a subsequent run on an identical matrix. again the user may
c     prefer to use a value of icn slightly greater than minicn for
c     subsequent runs to avoid too many conpresses (icncp). in the
c     event of failure with iflag equal to any negative value except
c     -4, minicn will give the minimum length to which licn could be
c     reduced to enable a successful decomposition to the point at
c     which failure occurred.  notice that, on a successful entry
c     idisp(2) gives the amount of space in a/icn required for the
c     decomposition while minicn will usually be slightly greater
c     because of the need for "elbow room".  if the user is very
c     unsure how large to make licn, the variable minicn can be used
c     to provide that information. a preliminary run should be
c     performed with abort3 left set to .false. and licn about 3/2
c     times as big as the number of non-zeros in the original matrix.
c     unless the initial problem is very sparse (when the run will be
c     successful) or fills in extremely badly (giving an error return
c     with iflag equal to -4), an error return with iflag equal to -5
c     should result and minicn will give the amount of space required
c     for a successful decomposition.
c
c common block ma30g/gd is used by the ma30b/bd entry only.
c     common /ma30gd/ eps, rmin
c eps is a real/double precision variable. it is used to test for
c     small pivots. its default value is 1.0e-4 (1.0d-4 in d version).
c     if the user sets eps to any value greater than 1.0, then no
c     check is made on the size of the pivots. although the absence of
c     such a check would fail to warn the user of bad instability, its
c     absence will enable ma30b/bd to run slightly faster. an  a
c     posteriori  check on the stability of the factorization can be
c     obtained from mc24a/ad.
c rmin is a real/double precision variable which gives the user some
c     information about the stability of the decomposition.  at each
c     stage of the lu decomposition the magnitude of the pivot apiv
c     is compared with the largest off-diagonal entry currently in its
c     row (row of u), rowmax say. if the ratio
c                       min (apiv/rowmax)
c     where the minimum is taken over all the rows, is less than eps
c     then rmin is set to this minimum value and iflag is returned
c     with the value +i where i is the row in which this minimum
c     occurs.  if the user sets eps greater than one, then this test
c     is not performed. in this case, and when there are no small
c     pivots rmin will be set equal to eps.
c
c common block ma30h/hd is used by ma30c/cd only.
c     common /ma30hd/ resid
c resid is a real/double precision variable. in the case of singular
c     or rectangular matrices its final value will be equal to the
c     maximum residual for the unsatisfied equations; otherwise its
c     value will be set to zero.
c
c common  block ma30i/id controls the use of drop tolerances, the
c     modified pivot option and the the calculation of the largest
c     entry in the factorization process. this common block was added
c     to the ma30 package in february, 1983.
c     common /ma30id/ tol, big, ndrop, nsrch, lbig
c tol is a real/double precision variable.  if it is set to a positive
c     value, then ma30a/ad will drop from the factors any non-zero
c     whose modulus is less than tol.  the factorization will then
c     require less storage but will be inaccurate.  after a run of
c     ma30a/ad where entries have been dropped, ma30b/bd  should not
c     be called.  the default value for tol is 0.0.
c big is a real/double precision variable.  if lbig has been set to
c     .true., big will be set to the largest entry encountered during
c     the factorization.
c ndrop is an integer variable. if tol has been set positive, on exit
c     from ma30a/ad, ndrop will hold the number of entries dropped
c     from the data structure.
c nsrch is an integer variable. if nsrch is set to a value less than
c     or equal to n, then a different pivot option will be employed by
c     ma30a/ad.  this may result in different fill-in and execution
c     time for ma30a/ad. if nsrch is less than or equal to n, the
c     workspace arrays lastc and nextc are not referenced by ma30a/ad.
c     the default value for nsrch is 32768.
c lbig is a logical variable. if lbig is set to .true., the value of
c     the largest entry encountered in the factorization by ma30a/ad
c     is returned in big.  setting lbig to .true.  will marginally
c     increase the factorization time for ma30a/ad and will increase
c     that for ma30b/bd by about 20%.  the default value for lbig is
c     .false.
c
      DOUBLE PRECISION EPS, RMIN, TOL, BIG
      LOGICAL ABORT1, ABORT2, ABORT3, LBIG
      COMMON /MA30ED/ LP, ABORT1, ABORT2, ABORT3
      COMMON /MA30GD/ EPS, RMIN
      COMMON /MA30ID/ TOL, BIG, NDROP, NSRCH, LBIG
      DATA EPS /1.0D-4/, TOL /0.0D0/, BIG /0.0D0/
      DATA LP /6/, NSRCH /32768/
      DATA LBIG /.FALSE./
      DATA ABORT1 /.TRUE./, ABORT2 /.TRUE./, ABORT3 /.FALSE./
      END
      BLOCK DATA MABLD3

c*********************************************************************72
c
cc MABLD3 contains block data.
c
c  Modified:
c
c    08 January 2013
c
      LOGICAL ABORT
      COMMON /MC23BD/ LP,NUMNZ,NUM,LARGE,ABORT
      DATA LP/6/,ABORT/.FALSE./
      END
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
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

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ', 
     &  'May      ', 'June     ', 'July     ', 'August   ', 
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *, 
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) 
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
      subroutine zibconst ( epmach, small )

c*********************************************************************72
c
cc ZIBCONST sets machine constants.
c
c  Modified:
c
c    09 January 2013
c
c  Parameters:
c
c    Output, double precision EPMACH, the relative machine precision.
c
c    Output, double precision SMALL, the square root of the smallest
c    positive machine number.
c
      implicit none

      double precision epmach
      double precision small

      epmach = 2.220446049250313D-016
      small = sqrt ( 4.450147717014403D-308 )

      return
      end
      subroutine zibsec ( cptim, ifail )

c*********************************************************************72
c
cc ZIBSEC returns the CPU time in seconds.
c
c  Modified:
c
c    08 January 2013
c
c  Parameters:
c
c    Output, real CPTIM, the elapsed CPU time.
c
c    Output, integer IFAIL, an error code.
c    0 means no error was observed.
c
      implicit none

      real cptim
      integer ifail

      ifail = 0
      call cpu_time ( cptim )

      return
      end
