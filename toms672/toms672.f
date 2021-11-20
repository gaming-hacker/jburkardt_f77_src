
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C        Main algorithm begins here with structure:
C                     ->   ASSIGN
C
C                     ->   GENER  ----> EPROD
C
C       EXTEND -------->   SOLVE  ----> NEWTON/BAIR
C         v
C         v           ->   RSORT
C         v
C       CHECK         ->   WEIGHT
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      SUBROUTINE EXTEND(N,M,M0,T,RECUR,SYMMET,START,PNODES,H0,NEXP,
     *                  IDIGIT,WT,NODES,QRNODE,QINODE,ERR,EXT,
     *                  IWORK,WORKA,LDA,WORKB,LDB,IFLAG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION T(0:*),EXT(0:*)
      DOUBLE PRECISION PNODES(*),WT(*),H0
      DOUBLE PRECISION QRNODE(*),QINODE(*),ERR(*)
      DOUBLE PRECISION WORKA(0:LDA-1,0:*),WORKB(0:LDB-1,*)
      INTEGER M0,N,M,IWORK(*),LDA,LDB,NODES,IFLAG,NEXP,IDIGIT
      LOGICAL SYMMET,START
      EXTERNAL RECUR
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C FORTRAN-77 Version 2.2: March 1987
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Please address queries or comments to:
C
C        T.N.L. Patterson
C        Department of Applied Mathematics & Theoretical Physics
C        The Queen's University of Belfast
C        Belfast, BT9 1NN
C        N. Ireland
C
C        Tel: International +44 232 245133 Ext. 3792.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Calculates the N+M node quadrature rule composed of N pre-assigned nod
C together with M nodes chosen optimally to achieve algebraic degree of
C precision of at least N+2*M-1.
C
C The orthogonal system of polynomials associated with the quadrature
C weight is defined generally by the recurrence relation specified in th
C user supplied subroutine RECUR.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C
C Input parameters:
C N     = Number of pre-assigned nodes (and upper limit to the expansion
C         Note that if successful this is reset to N+M on completion
C         (the appropriate value for iterative use).
C M     = Number of nodes to be optimally added.
C M0    = Lower limit to the expansion of T. This is ignored if START
C         is .TRUE. Note that if successful this is reset to M
C         on completion (the appropriate value for iterative use).
C T     = Array holding the coefficients TI of the polynomial whose
C         roots define the N pre-assigned nodes of the quadrature
C         rule and expressed as:
C                  SUM (I=M0 to N) (TI/HI)*P(I,X)
C         where HI is the integral of W(X)*P(I,X)**2 over the
C         interval for which orthogonality with respect the weight
C         W(X) is defined (moment integrals) and P(I,X) is the
C         orthogonal polynomial of degree I. Element T(I-M0) holds the
C         value of TI.
C
C         Note that T is either,
C            (1) provided explicitly,
C            (2) generated automatically from the N pre-assigned nodes
C                given in PNODES(*) (if START is .TRUE.)
C         or,
C            (3) generated from a previous call to the subroutine.
C         This array should be declared to have at least
C         max(N-M0+1,M+1) elements in the calling program.
C
C         The service subroutine TRANSF can be used to transform
C         the expansion to the required input form if desired
C         with the parameter IFLAG set to 1.
C RECUR = Name of user supplied subroutine which defines the orthogonal
C         polynomials. Given K, CALL RECUR(K,C,D,E) gives
C         the coefficients C,D and E such that,
C                     P(K+1,X)=(C*X+D)*P(K,X)+E*P(K-1,X)
C           The parameters are defined as follows:
C             K = Index
C             C,D,E = Parameters in the recurrence relation
C                                                (functions of K)
C SYMMET = .FALSE. if no advantage is to be taken of symmetry, if any,
C          about x=0 in the interval of integration and the
C          orthogonality  weight function. Note that if symmetry in
C          fact does exist setting this parameter to zero will still
C          produce correct results - only efficiency is effected.
C        = .TRUE. if the extended rule computations should
C          exploit symmetry about x=0 in the interval of
C          integration and the orthogonality  weight function.
C          This reduces the size of the system of linear equations
C          determining EXT by a factor of about 2 (see WORKA). If
C          symmetry does not in fact exist erroneous results will be
C          produced.
C START  = .TRUE. then the polynomial T is generated to have
C          the pre-assigned nodes (PNODES) as its roots.
C        = .FALSE. then the supplied values of the coefficients
C          of T are used directly (see above).
C PNODES = Array holding the pre-assigned nodes. This array should
C          be declared to have at least N+M elements in the calling prog
C H0     = Integral of the orthogonality weight function over the
C          interval of integration. Zero moment integral.
C NEXP   = Largest negative decimal exponent supported on the
C          computer. (Positive number - typical value 38 for VAX/VMS).
C          Weights less than approximately 10**(-NEXP) are set to zero
C          when the Christoffel-Darboux identity is used (N=M).
C          This may be set to INT(LOG10(X1MACH(2))) where X is set to
C          correspond to the appropriate precision in the PORT library.
C IDIGIT = Node convergence parameter (integer greater than 0).
C          An attempt is made to calculate the nodes to the maximum
C          accuracy possible by the machine precision available.
C          IDIGIT controls the assessment procedure to take account of
C          round-off errors and specifies the number of least significan
C          decimal digits that can be ignored (i.e. attributed
C          to round-off) in the computed relative error. Typical
C          value is 5.
C IWORK  = Integer working array which should be declared in the
C          calling program to have at least max(M,N) elements.
C          On return IWORK provides information on the convergence
C          of the nodes. See output parameters.
C WORKA  = Real working matrix which should be declared in the calling
C          program to have dimension at least max(M+1,N)
C          by max(M+1,N+1). If SYMMET=.TRUE. (see above) the
C          dimension can be reduced to max(M/2+1,N)
C          by max(M/2+1,N+1).
C LDA    = Number of elements in the leading dimension of WORKA
C          declared in the calling program.
C WORKB  = Real working matrix which should be declared in the calling
C          program to have dimension at least 2*M+1 by 3.
C LDB    = Number of elements in the leading dimension of WORKB
C          declared in the calling program
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Output parameters:
C PNODES = Ordered array holding the N+M nodes of the extended
C          quadrature rule made up from the original pre-assigned
C          nodes and the new optimally extended nodes. These values can
C          be used in subsequent iterative use of the subroutine.
C WT     = Array holding the values of the quadrature weights for
C          the extended rule associated with the nodes held in PNODES.
C          This array should be declared to have at least N+M elements
C          in the calling program.
C T      = Array holding the coefficients TI of the new orthogonal
C          expansion whose roots are the nodes of the extended quadratur
C          (that is, the pre-assigned nodes plus the extended nodes) and
C          is expressed as:
C                      SUM (I=M to N+M) (TI/HI)*P(I,X)
C          T(I-M) holds the value of TI.
C          (For definitions see description of input argument T).
C          This polynomial can be used as input for further extensions.
C
C          The service subroutine TRANSF can be used to remove the
C          moment factors from the expansion if desired with the
C          parameter IFLAG set to 0.
C M0     = Lower limit defining the new orthogonal expansion T.
C          (Set to M).
C N      = Upper limit defining the new orthogonal expansion T.
C          (Set to the original value of N+M).
C NODES  = Number of extended nodes found. Normally equals M but see IFL
C QRNODE = Array holding the real parts of the extended nodes (1,..,NODE
C          This array should be declared to have at least M elements
C          in the calling program.
C QINODE = Array holding the imaginary parts of the extended
C          nodes (1,..,NODES). (Hopefully these values are zero!).
C          This array should be declared to have at least M elements
C          in the calling program.
C ERR    = Array holding a measure of the relative error in the
C          nodes. This may be inspected if the convergence
C          error flag has been raised (IFLAG=3) to decide if the nodes
C          in question are acceptable. (ERR(*) actually gives the mean
C          last correction to the quadratic factor in the generalised
C          Bairstow root finder (see BAIR). This should declared in
C          the calling program to have at least M elements.
C EXT    = Array holding the coefficients of the polynomial whose
C          roots are the  extended nodes (QRNODES(*),QINODES(*)) and
C          expressed as:
C                EXT =   SUM (I=0 to M) EXT(I)*P(I,X)
C          This array should be declared to have at least M+1 elements
C          in the calling program.
C IWORK  = Node convergence flags. Elements 1 to NODES give information
C          on the convergence of the roots of the polynomial EXT
C          corresponding to each extended node.
C          Element I = 0 Convergence of I th root satisfactory
C          Element I = 1 Convergence of I th root unsatisfactory
C IFLAG  = 0, No error detected
C        = 1, The linear system of equations defining the polynomial
C             whose roots are the extended nodes became singular or
C             very  ill-conditioned.   (FATAL).
C        = 2, The linear system of equations used to generate the
C             polynomial T when START is .TRUE. became singular
C             or very ill-conditioned. (FATAL).
C        = 3, Poor convergence has been detected in the calculation
C             of the roots of EXT (see above) corresponding to the new
C             nodes or all nodes have not been found (M not equal
C             to NODES). See also ERR(*) below.
C        = 4, Possible imaginary nodes detected.
C        = 5, Value of N and M incompatible for SYMMET=.TRUE.
C             Both cannot be odd. (FATAL)
C        = 6, Test of new quadrature rule has failed.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Library routines called: LINPACK - DGEFA, DGESL
C FORTRAN-77 versions of these are included and renamed GEFA77 and GESL7
C These call the BLAS routines DSCAL, IDAMAX, DAXPY and DDOT
C which are renamed DSCAL7, IDAMX7, DAXPY7 and DDOT7.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Changing the precision.
C
C This is accomplished as follows:
C (1) Amend the TYPE statements,
C (2) Select an appropriate value for the NEXP argument to EXTEND.
C
C NOTE:
C (a) All constants used are specified in PARAMETER statements at the st
C     of each subprogram and,
C (b) Generic names are used for all function calls.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C
      IFLAG=0
      NODES=0
      IDEG=N+2*M-1
C Look for incompatible values of N and M
      IF(SYMMET) THEN
C         Both N and M cannot be odd
          IF(MOD(N,2).EQ.1.AND.MOD(M,2).EQ.1) THEN
            IFLAG=5
            RETURN
          END IF
      END IF
C Generate if required the initial T polynomial corresponding to
C prescribed pre-assigned nodes
      IF(START .AND. N.NE.0) THEN
        CALL ASSIGN(N,PNODES,IWORK,WORKA,LDA,RECUR,T,IERR)
        M0=0
        IF(IERR.NE.0) THEN
          IFLAG=2
          RETURN
        END IF
      END IF
      NLAST=N
C Generate extended expansion coefficients and overwrite T
      CALL GENER(T,M0,N,M,RECUR,SYMMET,EXT,
     *                          IWORK,WORKA,LDA,WORKB,LDB,IERR)
      IF(IERR.NE.0) THEN
        IFLAG=1
        RETURN
      END IF
C Find extended nodes as roots of EXT(*)
      CALL SOLVE(EXT,M,SYMMET,RECUR,IDIGIT,QRNODE,QINODE,
     *                        NODES,ERR,IWORK,WORKB,LDB,IERR)
      IF(IERR.NE.0) IFLAG=IERR+2
      IF(IFLAG.NE.0) RETURN
C Accumulate nodes for extended rule
      DO 10 I=1,M
        PNODES(NLAST+I)=QRNODE(I)
10      CONTINUE
C Re-order
      CALL RSORT(PNODES,N,1)
C Compute weights (only for positive nodes if symmetric)
      IF(SYMMET) THEN
        NUM=(N+1)/2
      ELSE
        NUM=N
      END IF
      DO 20 I=1,NUM
        CALL WEIGHT(T,M0,N,PNODES(I),RECUR,H0,NEXP,WT(I))
        IF(SYMMET) THEN
          WT(N-I+1)=WT(I)
        END IF
20      CONTINUE
C Test the new rule
      DO 30 K=0,MIN(4,IDEG/2)
        CALL CHECK(N,PNODES,WT,K,H0,RECUR,TEST,IERR)
        IF(IERR.EQ.1) THEN
          IFLAG=6
          RETURN
        END IF
30      CONTINUE
      RETURN
      END
      SUBROUTINE CHECK(N,QNODE,WT,K,H0,RECUR,TEST,IERR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION QNODE(*),WT(*),H0,TEST
      INTEGER N,K,IERR
      EXTERNAL RECUR
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C FORTRAN-77 Version 2.2: March 1987
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Carry out test of the given quadrature rule by computing the
C appropriate integral of,
C                     W(X)*P(K,X)*P(K,X)
C over the region associated with the weight function W(X) and the
C orthogonal polynomials P(K,X).
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C
C Input parameters:
C
C N      = Number of nodes in the quadrature rule.
C QNODE  = Array holding the N nodes.
C WT     = Array holding the N weights.
C K      = Index of the orthogonal polynomial whose weighted square
C          is to be integrated.
C H0     = Integral of the orthogonality weight function over the
C          interval of integration. Zero moment integral. Note that
C          P(0,X) is arbitrarily taken to be 1.0.
C RECUR  = Name of the subroutine which defines the orthogonal
C          polynomials. See EXTEND for a full description.
C
C Output parameters:
C TEST   = Approximate value of the test integral normalised to
C          unity. Thus, ABS(TEST-1) gives a measure of the
C          quality of the calculated rule.
C IERR   = 0, OK.
C        = 1, Rule quality unsatisfactory
C        = 2, Invalid values for input arguments
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TOL=0.0000001D0)
      IERR=0
      IF(K.LT.0 .OR. N.LT.1 .OR. H0.LE.ZERO) THEN
        IERR=2
        RETURN
      END IF
      TEST=ZERO
      DO 30 I=1,N
        P1=ONE
        IF(K.EQ.0) GOTO 20
        P0=ZERO
        X=QNODE(I)
C Calculate integrand
        DO 10 J=0,K-1
          CALL RECUR(J,CJ,DJ,EJ)
          P=(CJ*X+DJ)*P1+EJ*P0
          P0=P1
          P1=P
10        CONTINUE
20      TEST=TEST+P1*P1*WT(I)
30      CONTINUE
      TEST=TEST/H0
      IF(K.EQ.0) RETURN
C Calculate exact value
      CALL RECUR(0,P,P0,P1)
      DO 70 J=1,K
        CALL RECUR(J,CJ,DJ,EJ)
        P=-P*EJ
70      CONTINUE
C Normalise result to unity
      TEST=TEST*CJ/P
C Test for rule quality
      IF(ABS(TEST-ONE).GT.TOL) IERR=1
      RETURN
      END
      SUBROUTINE ASSIGN(N,PNODES,IWORK,WORK,LDW,RECUR,T,IERR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PNODES(*),WORK(0:LDW-1,0:*),T(0:*)
      INTEGER N,LDW,IERR,IWORK(*)
      EXTERNAL RECUR
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Generate the initial polynomial T whose roots are the required
C pre-assigned nodes
C
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Input parameters:
C N      = Number of pre-assigned nodes to be used to generate T.
C PNODES = Array holding N pre-assigned nodes to be be used to
C          generate T.
C IWORK  = Integer working array which should be declared in the
C          calling program to have at least N elements.
C WORK   = Real working matrix which should be declared in
C          the calling program to have dimension at least N by
C          N+1.
C LDW    = Number of elements in the leading dimension of WORK
C          declared in the calling program
C RECUR  = Name of user supplied subroutine which defines the orthogonal
C          polynomials. Given K, CALL RECUR(K,C,D,E) gives
C          the coefficients C,D and E such that,
C                      P(K+1,X)=(C*X+D)*P(K,X)+E*P(K-1,X)
C          The parameters are defined as follows:
C             K = Index
C             C,D,E = Parameters in the recurrence relation
C                                              (functions of K)
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Output parameters:
C T      = Array holding the coefficients of the polynomial whose
C          roots define the pre-assigned nodes of the quadrature
C          rule and expressed as:
C                  H0* SUM (I=0 to N) T(I)/HI*P(I,X)
C          T(I) holds the value of TI.
C          This array should be declared to have at least N+1 elements
C          in the calling program.
C IERR   = 0, No error detected
C        = 1, The linear system of equations used to generate the
C             polynomial T became singular or very ill-conditioned. (FAT
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C External library routines called: LINPACK - DGEFA, DGESL
C FORTRAN-77 versions of these are used and renamed GEFA77 and GESL77.
C Note -- These call the BLAS routines DSCAL, IDAMAX, DAXPY and DDOT
C which are not renamed.
C (Quadruple precision versions used for this subprogram)
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
      IERR=0
C Set up the linear system of equations
      DO 20 J=1,N
        X=PNODES(J)
        P0=ZERO
        P1=ONE
        P=P1
        DO 10 K=0,N
          WORK(J-1,K)=P
          CALL RECUR(K,C0,D0,E0)
          P=(C0*X+D0)*P1+E0*P0
          P0=P1
          P1=P
10        CONTINUE
20      CONTINUE
C Solve linear system
      CALL GEFA77(WORK,LDW,N,IWORK,INFO)
      IF(INFO.NE.0) THEN
        IERR=1
        RETURN
      END IF
      CALL GESL77(WORK,LDW,N,IWORK,WORK(0,N),0)
      DO 30 J=0,N-1
        T(J)=-WORK(J,N)
30      CONTINUE
      T(N)=ONE
C Weight with moments
      CALL TRANSF(T,0,N,RECUR,1)
      RETURN
      END
      SUBROUTINE GENER(T,M0,N,M,RECUR,SYMMET,EXT,
     *                          IWORK,WORKA,LDA,WORKB,LDB,IFLAG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION WORKA(0:LDA-1,0:*),WORKB(0:LDB-1,*)
      DOUBLE PRECISION T(0:*),EXT(0:*)
      INTEGER M0,N,M,IWORK(*),LDA,LDB,IFLAG
      LOGICAL SYMMET
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Given N pre-assigned quadrature nodes defined as the roots of the
C polynomial expansion,
C                   SUM (I=M0 to N) (TI/HI)*P(I,X)
C calculate the polynomial expansion,
C                   SUM (I=0 to M) SI*P(I,X)
C whose roots are the M optimal nodes and new expansion
C                   SUM (I=M to N+M) (RI/HI)*P(I,X)
C whose roots are to the N+M nodes of the full extended quadrature rule.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C
C Input parameters:
C T     = Array holding the coefficients TI of the polynomial whose
C         roots define the N pre-assigned nodes of the quadrature
C         rule and expressed as:
C                  SUM (I=M0 to N) (TI/HI)*P(I,X)
C         where HI is the integral of W(X)*P(I,X)**2 over the
C         interval for which orthogonality with respect the weight
C         W(X) is defined (moment integrals) and P(I,X) is the
C         orthogonal polynomial of degree I. T(I-M0) holds the
C         value of TI. This array should be declared to have at least
C         max(N-M0+1,M+1) elements in the calling program.
C M0    = Lower limit to the expansion of T.
C N     = Upper limit to expansion of T.
C M     = Number of nodes to be optimally added.
C RECUR = Name of user supplied subroutine which defines the orthogonal
C         polynomials. Given K, CALL RECUR(K,C,D,E) gives
C         the coefficients C,D and E such that,
C                     P(K+1,X)=(C*X+D)*P(K,X)+E*P(K-1,X)
C           The parameters are defined as follows:
C             K = Index
C             C,D,E = Parameters in the recurrence relation
C                                                (functions of K)
C SYMMET=  .FALSE. if no advantage is to be taken of symmetry, if any,
C          about x=0 in the interval of integration and the
C          orthogonality  weight function. Note that if symmetry in
C          fact does exist setting this parameter to zero will still
C          produce correct results - only efficiency is effected.
C       =  .TRUE. if the extended rule computations should
C          exploit symmetry about x=0 in the interval of
C          integration and the orthogonality  weight function.
C          This reduces the size of the system of linear equations
C          determining EXT by a factor of about 2 (see WORKA). If
C          symmetry does not in fact exist erroneous results will be
C          produced.
C IWORK  = Integer working array which should be declared in the
C          calling program to have at least M elements.
C WORKA  = Real working matrix which should be declared in the calling
C          program to have dimension at least M+1 by max(M+1,N+1).
C          If SYMMET=.TRUE. (see above) the dimension can be reduced to
C          M/2+1 by max(M/2+1,N/2+1).
C LDA    = Number of elements in the leading dimension of WORKA
C          declared in the calling program
C WORKB  = Real working matrix which should be declared in the calling
C          program to have at dimension at least 2*M+1 by 3.
C LDB    = Number of elements in the leading dimension of WORKB
C          declared in the calling program.
C
C Output parameters:
C T      = Array holding the coefficients of the new orthogonal
C          expansion whose roots are the nodes of the extended quadratur
C          (that is the pre-assigned nodes plus the extended nodes).
C          It is expressed as:
C                  SUM (I=M to N+M) (TI/HI)*P(I,X)
C          where N and M have their original values. T(I-M) holds
C          the value of TI. See input argument of T for definitions.
C M0,N   = Lower and upper limits defining the new orthogonal expansion
C EXT    = Array holding the coefficients of the polynomial whose
C          roots are the  new extended nodes and expressed as:
C                EXT =   SUM (I=0 to M) EXT(I)*P(I,X)
C IFLAG  = 0, No error detected
C        = 1, The linear system of equations defining the polynomial
C             whose roots are the extended nodes became singular or
C             very  ill-conditioned.   (FATAL).
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C External library routines called: LINPACK - DGEFA, DGESL
C FORTRAN-77 versions of these are used and renamed GEFA77 and GESL77.
C Note -- These call the BLAS routines DSCAL, IDAMAX, DAXPY and DDOT
C which are not renamed.
C (Quadruple precision versions used for this subprogram)
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      LOGICAL NEVEN,MSODD,MISS
      EXTERNAL RECUR
      INTEGER S
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
      IFLAG=0
C Look for trivial case
      IF(N.EQ.0) THEN
        DO 10 I=0,M-1
          EXT(I)=ZERO
10        CONTINUE
        EXT(M)=ONE
        T(0)=ONE
        N=M
        M0=M
        RETURN
      END IF
C General case
      NEVEN=MOD(N,2).EQ.0
      NM=N+M
C Form matrix
      DO 60 S=0,M
        MSODD=MOD(M+S,2).EQ.1
        IF(NEVEN.AND.MSODD.AND.SYMMET) GOTO 60
        DO 50 J=0,S
          CALL EPROD(S,J,WORKB(0,1),WORKB(0,2),LDB,RECUR,IFAIL)
          IF(MOD(N+S+J,2).EQ.1.AND.SYMMET) GOTO 50
          IREF=S-J
          ITOP=MIN(N,J+S)
          IBOT=MAX(M0,IREF)
          SUM=ZERO
          IF(IBOT.GT.ITOP) GOTO 40
          DO 30 I=IBOT,ITOP
            SUM=SUM+T(I-M0)*WORKB(I-IREF,1)
30          CONTINUE
40        IF(.NOT.SYMMET) THEN
            WORKA(S,J)=SUM
            WORKA(J,S)=SUM
            GOTO 50
          END IF
          IF(NEVEN) THEN
            WORKA(S/2,J/2)=SUM
            WORKA(J/2,S/2)=SUM
          ELSE
            IF(MSODD) THEN
              WORKA(S/2,J/2)=SUM
            ELSE
              WORKA(J/2,S/2)=SUM
            END IF
          END IF
50        CONTINUE
60      CONTINUE
      NEQ=M
      IF(SYMMET) NEQ=M/2
C Solve for expansion coefficients
      CALL GEFA77(WORKA,LDA,NEQ,IWORK,INFO)
      IF(INFO.NE.0) THEN
        IFLAG=1
        RETURN
      END IF
      CALL GESL77(WORKA,LDA,NEQ,IWORK,WORKA(0,NEQ),0)
C Store expansion coefficients
      DO 70 J=0,NEQ-1
        EXT(J)=-WORKA(J,NEQ)
70      CONTINUE
      EXT(NEQ)=ONE
C Calculate new T polynomial
      IF(SYMMET) GOTO 160
C
C Non-symmetric case
      DO 140 S=M,NM
        IF(S.EQ.M) GOTO 120
        DO 110 J=0,M
          CALL EPROD(S,J,WORKB(0,1),WORKB(0,2),LDB,RECUR,IFAIL)
          IREF=S-J
          ITOP=MIN(N,J+S)
          IBOT=MAX(M0,IREF)
          SUM=ZERO
          IF(IBOT.GT.ITOP) GOTO 100
          DO 90 I=IBOT,ITOP
            IR=I-IREF
            SUM=SUM+T(I-M0)*WORKB(I-IREF,1)
90          CONTINUE
100       WORKA(M,J)=SUM
110       CONTINUE
120     SUM=ZERO
        DO 130 I=0,M
          SUM=SUM+EXT(I)*WORKA(M,I)
130       CONTINUE
        WORKA(M-1,S-M)=SUM
140     CONTINUE
C Overwrite old values of T
      DO 150 I=0,N
        T(I)=WORKA(M-1,I)
150     CONTINUE
      GOTO 250
C
C Symmetric case
160   DO 210 S=M,NM
        IF(MOD(N+M+S,2).EQ.1) GOTO 210
        DO 190 J=0,M
          CALL EPROD(S,J,WORKB(0,1),WORKB(0,2),LDB,RECUR,IFAIL)
          IF(MOD(N+S+J,2).EQ.1) GOTO 190
          IREF=S-J
          ITOP=MIN(N,J+S)
          IBOT=MAX(M0,IREF)
          SUM=ZERO
          IF(IBOT.GT.ITOP) GOTO 180
          DO 170 I=IBOT,ITOP
            IR=I-IREF
            SUM=SUM+T(I-M0)*WORKB(I-IREF,1)
170         CONTINUE
180       WORKA(NEQ,J/2)=SUM
190     CONTINUE
      SUM=ZERO
      DO 200 I=0,NEQ
        SUM=SUM+EXT(I)*WORKA(NEQ,I)
200     CONTINUE
      WORKA(NEQ-1,(S-M)/2)=SUM
210   CONTINUE
C Overwrite old values of T in full unsymmetric form
      IC=N/2
      MISS=.TRUE.
      DO 220 J=N,0,-1
        MISS=.NOT.MISS
        IF(MISS) THEN
          T(J)=ZERO
        ELSE
          T(J)=WORKA(NEQ-1,IC)
          IC=IC-1
        END IF
220     CONTINUE
C Convert EXT to full unsymmetric form
      WORKB(M,1)=ONE
      IC=NEQ-1
      MISS=.FALSE.
      DO 230 J=M-1,0,-1
        MISS=.NOT.MISS
        IF(MISS) THEN
          WORKB(J,1)=ZERO
        ELSE
          WORKB(J,1)=EXT(IC)
          IC=IC-1
        END IF
230     CONTINUE
      DO 240 J=0,M
        EXT(J)=WORKB(J,1)
240     CONTINUE
C Scale new T polynomial
250   PMAX=ZERO
      DO 260 I=0,N
        PMAX=MAX(PMAX,ABS(T(I)))
260     CONTINUE
      DO 270 I=0,N
        T(I)=T(I)/PMAX
270     CONTINUE
      N=NM
      M0=M
      RETURN
      END
      SUBROUTINE SOLVE(EXT,M,SYMMET,RECUR,IDIGIT,QRNODE,QINODE,
     *                 NODES,ERR,ICHECK,WORK,LDW,IERR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION EXT(0:*),WORK(0:LDW-1,*),ERR(*)
      DOUBLE PRECISION QRNODE(*),QINODE(*)
      INTEGER M,NODES,LDW,IERR,ICHECK(*),IDIGIT
      LOGICAL SYMMET,RESET
      EXTERNAL RECUR
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Calculate the roots of the orthogonal polynomial expansion
C expressed as,
C                 SUM (I=0 to M) EXT(I)*P(I,X)
C where the array EXT holds the appropriate coefficients.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C
C Input parameters:
C EXT    = Array holding the coefficients of the polynomial whose
C          roots are required (nodes of the quadrature rule)
C          and expressed as:
C                    SUM (I=0 to M) EXT(I)*P(I,X)
C          The recurrence relation for the orthogonal polynomials
C          P(I,X) is defined by the subroutine RECUR.
C          This array should be declared to have at least M+1 elements
C          in the calling program.
C M      = Upper limit to expansion EXT (polynomial degree).
C SYMMET = .FALSE. if no advantage can be taken of symmetry
C          about x=0 in the interval of integration and the
C          orthogonality  weight function.
C        = .TRUE. if symmetry exists about x=0 in the interval of
C          integration and the orthogonality weight function.
C RECUR  = Name of user supplied subroutine which defines the orthogonal
C          polynomials. Given K, CALL RECUR(K,C,D,E) gives
C          the coefficients C,D and E such that,
C                      P(K+1,X)=(C*X+D)*P(K,X)+E*P(K-1,X)
C          The parameters are defined as follows:
C             K = Index
C             C,D,E = Parameters in the recurrence relation
C                                              (functions of K)
C IDIGIT = Node convergence parameter (integer greater than 0).
C          An attempt is made to calculate the nodes to the maximum
C          accuracy possible by the machine precision available.
C          IDIGIT controls the assessment procedure to take account of
C          round-off errors and specifies the number of least significan
C          decimal digits that can be ignored (i.e. attributed
C          to round-off) in the computed relative error. Typical
C          value is 5.
C WORK   = Real working matrix which should be declared in the calling
C          program to have dimension at least M+1 by 2.
C LDW    = Number of elements in the leading dimension of WORK
C          declared in the calling program
C
C Output parameters:
C
C QRNODE = Array holding the real parts of the roots fo EXT (1,..,NODES)
C QINODE = Array holding the imaginary parts of the roots of EXT (1,..,N
C          (hopefully these values are zero!).
C NODES  = Number of extended nodes found. Normally equals M but see IER
C ICHECK = Root convergence flags. Elements 1 to NODES give information
C          on the convergence of the roots of the polynomial EXT.
C            Element I = 0 Convergence of I th root satisfactory
C            Element I = 1 Convergence of I th root unsatisfactory
C          This array should be declared to have at least M elements
C          in the calling program.
C IERR   = 0, No error detected
C        = 1, Possible imaginary nodes detected.
C        = 2, Poor convergence has been detected in the calculation
C             of the roots of EXT (see above) or all roots have not
C             been found (M not equal to NODES). See also ERR(*) below.
C ERR    = Array holding a measure of the relative error in the
C          roots. This may be inspected if the convergence
C          error flag has been raised (IERR=2) to decide if the roots
C          in question are acceptable. (ERR(*) actually gives the mean
C          last correction to the quadratic factor in the generalised
C          Bairstow root finder (see BAIR). This should declared in
C          the calling program to have at least M elements.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      PARAMETER (ZERO=0.0D0,HALF=0.5D0,ONE=1.0D0,
     *           TWO=2.0D0,VRT1=0.0000001D0)
      NODES=0
      IERR=0
C If M is odd find and remove initial real root using NEWTON iteration
C Set WORK(*,1) to polynomial to be processed
      IF(MOD(M,2).EQ.1) THEN
        ZR1=VRT1
        CALL NEWTON(EXT,M,ZR1,RECUR,IDIGIT,WORK(0,1),ERRVAL,IFAIL)
        NODES=NODES+1
        ICHECK(NODES)=IFAIL
        ERR(NODES)=ERRVAL
        QRNODE(NODES)=ZR1
        QINODE(NODES)=ZERO
        NROOT=M-1
      ELSE
        DO 10 I=0,M
          WORK(I,1)=EXT(I)
10        CONTINUE
        NROOT=M
      END IF
      IF(NROOT.EQ.0) GOTO 50
C Find remaining root pairs
C Calculate seed approximation for quadratic factor
      CALL RECUR(0,C0,D0,E0)
      CALL RECUR(1,C1,D1,E1)
      RT1=VRT1
      RT2=ZERO
      IF(SYMMET) RT2=-RT1
      P1A=C0*RT1+D0
      P1B=C0*RT2+D0
      P2A=(C1*RT1+D1)*P1A+E1
      P2B=(C1*RT2+D1)*P1B+E1
      DET=C0*(RT1-RT2)
      SA1=(P2A-P2B)/DET
      SA0=(P1A*P2B-P1B*P2A)/DET
      RESET=.TRUE.
C Alternate approximation which introduces small complex component
      RT1=VRT1
      RT2=VRT1
      SFA1=(C0*D1+D0*C1)/C0+TWO*C1*RT1
      SFA0=D0*D1+E1-D0*SFA1-C0*C1*(RT1*RT1+RT2*RT2)
C IP1 points to current deflated polynomial
      IP1=1
      IP2=2
20    IF(RESET) THEN
        A2=ONE
        A1=SA1
        A0=SA0
        RESET=.FALSE.
      END IF
      CALL BAIR(NROOT,WORK(0,IP1),WORK(0,IP2),A0,A1,A2,
     *                                     RECUR,IDIGIT,ERRVAL,IFAIL)
      IF(IFAIL.NE.0) THEN
C Try again with complex components introduced
        A2=ONE
        A1=SFA1
        A0=SFA0
        RESET=.TRUE.
        CALL BAIR(NROOT,WORK(0,IP1),WORK(0,IP2),A0,A1,A2,
     *                                     RECUR,IDIGIT,ERRVAL,IFAIL)
      END IF
C Apply Bairstow to full expansion to avoid error accumulation
      CALL BAIR(M,EXT,WORK(0,IP2),A0,A1,A2,
     *                                     RECUR,IDIGIT,ERRVAL,IFAIL)
C Tidy up the quotient polynomial
      CALL QFACT(NROOT,WORK(0,IP1),WORK(0,IP2),RECUR,A1,A0,
     *                                       ZR1,ZR1,ZR1,ZR1,ZR1,ZR1)
      CALL ROOTS(A0,A1,A2,ZR1,ZI1,ZR2,ZI2,RECUR,INFO)
C Record node information
      NODES=NODES+1
      ICHECK(NODES)=IFAIL
      ERR(NODES)=ERRVAL
      QRNODE(NODES)=ZR1
      QINODE(NODES)=ZI1
      NODES=NODES+1
      ICHECK(NODES)=IFAIL
      ERR(NODES)=ERRVAL
      QRNODE(NODES)=ZR2
      QINODE(NODES)=ZI2
      NROOT=NROOT-2
C Make the deflated polynomial current
      I=IP1
      IP1=IP2
      IP2=I
      IF(NROOT.GT.0) THEN
C Scale the deflated polynomial
        PMAX=ZERO
        DO 30 I=0,NROOT
          PMAX=MAX(PMAX,ABS(WORK(I,IP1)))
30        CONTINUE
        DO 40 I=0,NROOT
          WORK(I,IP1)=WORK(I,IP1)/PMAX
40        CONTINUE
        GOTO 20
      END IF
C Calculation complete - Check for difficulties
C Look for poor convergence
50    I=0
      DO 60 J=1,NODES
        I=I+ICHECK(J)
60      CONTINUE
      IF(NODES.NE.M.OR.I.NE.0) THEN
        IERR=1
        RETURN
      END IF
C Look for possible imaginary nodes
      DO 70 J=1,NODES
        IF(QINODE(J).NE.ZERO) THEN
          IERR=2
          RETURN
        END IF
70      CONTINUE
      RETURN
      END
      SUBROUTINE RSORT(A,N,IFLAG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION A(*)
      INTEGER N,IFLAG
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C FORTRAN-77 Version 2.2: March 1987
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C Carries out a simple ripple sort of A(*).
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C
C Input parameters:
C A     = Array holding the numbers to be sorted
C N     = Number of elements to be sorted
C IFLAG = 0 for ascending sort
C       = 1 for descending sort
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      LOGICAL DONE,ASCEND
      ASCEND=IFLAG.EQ.0
C Begin scans
      DO 30 J=N-1,1,-1
        DONE=.TRUE.
        DO 20 K=1,J
          IF(ASCEND) THEN
            K1=K
            K2=K+1
          ELSE
            K1=K+1
            K2=K
          END IF
          IF(A(K1).GT.A(K2)) THEN
C Exchange elements
            VAL=A(K1)
            A(K1)=A(K2)
            A(K2)=VAL
            DONE=.FALSE.
          END IF
20        CONTINUE
        IF(DONE) RETURN
30    CONTINUE
      RETURN
      END
      SUBROUTINE WEIGHT(T,M,N,XNODE,RECUR,H0,NEXP,WT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION T(0:*),XNODE,H0,WT
      INTEGER M,N,NEXP
      EXTERNAL RECUR
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C FORTRAN-77 Version 2.2: March 1987
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Calculates the quadrature weight associated with the node XNODE in the
C rule whose nodes are defined by the roots of polynomial T.
C
C The weight is calculated by dividing T by (X-XNODE) to give,
C
C S(X) = T(X)/(X-XNODE) = SUM (0 to N-1) G(I)*P(I,X).
C
C S(X) is then divided by (X-XNODE) to give remainder R.
C
C The weight is finally given by H0*G(0)/R. If N=M the
C Christoffel-Darboux identity result is used to reduce extreme
C cancellation effects at high degree.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C
C Input parameters:
C
C T      = Array holding the coefficients TI of the polynomial whose
C          roots define the N pre-assigned nodes of the quadrature
C          rule and expressed as:
C                   SUM (I=M to N) (TI/HI)*P(I,X)
C          where HI is the integral of W(X)*P(I,X)**2 over the
C          interval for which orthogonality with respect the weight
C          W(X) is defined (moment integrals) and P(I,X) is the
C          orthogonal polynomial of degree I. T(I-M) holds the
C          value of TI. This array should be declared to have at least
C          N-M+1 elements in the calling program.
C M      = Lower limit to the expansion of T
C N      = Upper limit to expansion of T
C XNODE  = Node whose weight is required
C RECUR  = Name of the subroutine which defines the orthogonal
C          polynomials. See EXTEND for a full description.
C H0     = Integral of the orthogonality weight function over the
C          interval of integration. Zero moment integral. Note that
C          P(0,X) is arbitrarily taken to be 1.0
C NEXP   = Largest negative decimal exponent supported on the
C          computer. (Positive number - typical value 38).
C          Weights less than approximately 10**(-NEXP) are set to zero
C          when the Christoffel-Darboux identity is used (N=M).
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Output parameters:
C WT    = Weight associated with XNODE.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TEN=10.0D0)
C Check for special case
      IF(M.EQ.N) THEN
C Use Christoffel-Darboux result
        BK1=ZERO
        BK2=ONE
        DK1=ZERO
        DK2=ZERO
        ISCALE=0
        CALL RECUR(0,H,D0,E0)
        DO 20 K=0,N-1
          CALL RECUR(K,CK,DK,EK)
          IF(K.NE.0) H=-EK*H
          BB=(CK*XNODE+DK)*BK2+EK*BK1
          DD=(CK*XNODE+DK)*DK2+EK*DK1+CK*BK2
          BK1=BK2
          BK2=BB
          DK1=DK2
          DK2=DD
          IF(BK2.NE.ZERO) THEN
            J = int ( LOG10(ABS(BK2)) )
            IF(ABS(J).GT.2) THEN
C Scale to control overflow/underflow
              ISCALE=ISCALE-2*J
              SCALE=TEN**J
              BK2=BK2/SCALE
              BK1=BK1/SCALE
              DK1=DK1/SCALE
              DK2=DK2/SCALE
            END IF
          END IF
          IF(H.NE.ZERO) THEN
            J = int ( LOG10(ABS(H)) )
            IF(ABS(J).GE.2) THEN
              ISCALE=ISCALE+J
              H=H/TEN**J
            END IF
          END IF
20        CONTINUE
        WT=H0*H/DK2/BK1
        IF(WT.NE.ZERO) THEN
          ITEST = int ( LOG10(ABS(WT)) ) + ISCALE
          IF(ITEST.GE.-NEXP) THEN
            WT=WT*TEN**ISCALE
          ELSE
            WT=ZERO
          END IF
        END IF
        RETURN
      END IF
C General case
      BK2=ZERO
      BK1=ZERO
      RK2=ZERO
      RK1=ZERO
      CALL RECUR(N,CK,DK,EK)
      CALL RECUR(N+1,CK1,DK1,EK1)
      H=ONE
      ISCALE=0
      DO 10 K=N,1,-1
        IF(K.GE.M) THEN
          RS=T(K-M)/H
C Scale and adjust for possible overflow/underflow
          IF(ISCALE.GT.NEXP) THEN
            RS=ZERO
          ELSE
            RS=RS/TEN**ISCALE
          END IF
        ELSE
          RS=ZERO
        END IF
        BB=RS+(DK+XNODE*CK)*BK1+EK1*BK2
        BK2=BK1
        BK1=BB
        CALL RECUR(K-1,CKM1,DKM1,EKM1)
        IF(N.NE.M) H=-H*CK/EK/CKM1
        BB=BB*CKM1
        WT=BB+(DKM1+XNODE*CKM1)*RK1+EK*RK2
        RK2=RK1
        RK1=WT
        CK1=CK
        DK1=DK
        EK1=EK
        CK=CKM1
        DK=DKM1
        EK=EKM1
        IF(BK1.NE.ZERO) THEN
          J = int ( LOG10(ABS(BK1)) )
          IF(ABS(J).GT.2) THEN
C Scale to control overflow/underflow
            ISCALE=ISCALE+J
            SCALE=TEN**J
            BK1=BK1/SCALE
            BK2=BK2/SCALE
            RK1=RK1/SCALE
            RK2=RK2/SCALE
          END IF
        END IF
10      CONTINUE
      WT=H0*BB/WT
      RETURN
      END
      SUBROUTINE TRANSF(T,M,N,RECUR,IFLAG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION T(0:*)
      INTEGER M,N
      EXTERNAL RECUR
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C FORTRAN-77 Version 2.2: March 1987
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Scales the polynomial expansion:
C          SUM (M to N) TI*P(I,X).
C with respect to the moments HI of the orthogonality weight function
C giving the expansion:
C          H0* SUM (M to N) (TI/HI)*P(I,X).
C or
C      (1/H0)* SUM (M to N) (TI*HI)*P(I,X).
C depending on the value of IFLAG.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Input parameters:
C
C T      = Array holding the coefficients TI of the polynomial expansion
C          to be scaled and expressed as:
C                   SUM (I=M to N) TI*P(I,X)
C          T(I-M) holds the value of TI. T(*) should be declared to
C          have at least N-M+1 elements in the calling program.
C M      = Lower limit to the expansion of T
C N      = Upper limit to expansion of T
C RECUR  = Name of the subroutine which defines the orthogonal
C          polynomials. See EXTEND for a full description.
C IFLAG  = 0, if coefficient TI is to be replaced by TI*(H0/HI).
C IFLAG  = 1, if coefficient TI is to be replaced by TI*(HI/H0).
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Output parameters:
C T      = Array holding the coefficients of the scaled polynomial
C          expansion.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      PARAMETER (ONE=1.0D0)
      H=ONE
      DO 10 K=0,N
        CALL RECUR(K,CK,DK,EK)
        IF(K.NE.0) H=-CKM1/CK*EK*H
        IF(K.GE.M) THEN
          IF(IFLAG.EQ.0) THEN
            T(K-M)=T(K-M)/H
          ELSE
            T(K-M)=T(K-M)*H
          END IF
        END IF
        CKM1=CK
10      CONTINUE
      RETURN
      END
      SUBROUTINE BAIR(N,POLIN,POLOUT,A0,A1,A2,RECUR,IDIGIT,ERRVAL,IFAIL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION POLIN(0:*),POLOUT(0:*),A0,A1,A2,ERRVAL
      INTEGER N,IDIGIT,IFAIL
      EXTERNAL RECUR
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C FORTRAN-77 Version 2.2: March 1987
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Generalised Bairstow root extraction for polynomial
C        SUM(I=0 to N)  POLIN(I)*P(I,X)
C Calculates root as quadratic factor,
C        A2*P(2,X)-A1*P(1,X)-A0*P(0,X)
C where P(I,X) is a general orthogonal polynomial of degree I
C
C (Reference: Golub & Robertson, Comm.ACM.,10,1967,371-373).
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C
C Input parameters:
C
C N        = Degree of input polynomial POLIN
C POLIN    = Coefficients of polynomial of degree N whose quadratic
C            factor is to be found, i.e.
C                POLIN = SUM(I=0 to N) POLIN(I)*P(I,X)
C            This array should be declared to have at least N+1 elements
C            in the calling program.
C A0,A1,A2 = Estimated quadratic factors
C RECUR    = Name of the subroutine which defines the orthogonal
C            polynomials. See EXTEND for full description.
C IDIGIT   = Node convergence parameter (integer greater than 0).
C            An attempt is made to calculate the nodes to the maximum
C            accuracy possible by the machine precision available.
C            IDIGIT controls the assessment procedure to take account of
C            round-off errors and specifies the number of least signific
C            decimal digits that can be ignored (i.e. attributed
C            to round-off) in the computed relative error. Typical
C            value is 5.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Output parameters:
C
C POLOUT   = Coefficients of the deflated polynomial of degree N-2 with
C            quadratic factor removed, i.e.
C                POLOUT = SUM(I=0 to N-2) POLOUT(I)*P(I,X)
C            This array should be declared to have at least N-1 elements
C            in the calling program.
C A0,A1,A2 = Calculated coefficients of the quadratic factor
C IFAIL    = 0 Quadratic factor found.
C          = 1 Convergence not achieved after 50 iterations.
C ERRVAL   = Mean value of the correction to the coefficients of
C            the quadratic factor. May be used as a measure of the
C            root accuracy when convergence is not achieved.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      PARAMETER (ZERO=0.0D0,HALF=0.5D0,ONE=1.0D0,TEN=10.0D0)
      IFAIL=0
      ITER=50
      ERRVAL=ZERO
C Special cases
      IF(N.EQ.1) THEN
        A0=-POLIN(0)
        A1=-POLIN(1)
        A2=ZERO
        RETURN
      END IF
      IF(N.EQ.2) THEN
        A0=-POLIN(0)
        A1=-POLIN(1)
        A2= POLIN(2)
        RETURN
      END IF
C Estimated ALPHA & BETA
      TOL=TEN**(-MAX(1,IDIGIT))
      ALPHA=A1/A2
      BETA=A0/A2
10    ITER=ITER-1
      IF(ITER.LT.0) THEN
        IFAIL=1
        GOTO 20
      END IF
      CALL QFACT(N,POLIN,POLOUT,RECUR,ALPHA,BETA,A,B,AA,AB,BA,BB)
      SCALE=MAX(ABS(AB),ABS(BB))
      F1=AB/SCALE
      F2=BB/SCALE
      DELTA=(B*F1-A*F2)/(AA*F2-BA*F1)
      SCALE=MAX(ABS(BA),ABS(AA))
      F1=BA/SCALE
      F2=AA/SCALE
      EPS=(A*F1-B*F2)/(BB*F2-AB*F1)
      ALPHA=ALPHA+DELTA
      BETA=BETA+EPS
C Test for convergence
C Stop if correction is less than 1/TOL times the smallest machine
C relative error.
      IF(ABS(ALPHA)+TOL*ABS(DELTA).NE.ABS(ALPHA)
     *               .OR. ABS(BETA)+TOL*ABS(EPS).NE.ABS(BETA)) GOTO 10
C Final iteration to tidy up result
      CALL QFACT(N,POLIN,POLOUT,RECUR,ALPHA,BETA,A,B,AA,AB,BA,BB)
      SCALE=MAX(ABS(AB),ABS(BB))
      F1=AB/SCALE
      F2=BB/SCALE
      DELTA=(B*F1-A*F2)/(AA*F2-BA*F1)
      SCALE=MAX(ABS(BA),ABS(AA))
      F1=BA/SCALE
      F2=AA/SCALE
      EPS=(A*F1-B*F2)/(BB*F2-AB*F1)
      ALPHA=ALPHA+DELTA
      BETA=BETA+EPS
20    A0=BETA
      A1=ALPHA
      A2=ONE
      ERRVAL=HALF*(ABS(EPS)+ABS(DELTA))
      RETURN
      END
      SUBROUTINE QFACT(N,GAMMA,DELTA,RECUR,ALPHA,BETA,A,B,AA,AB,BA,BB)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION GAMMA(0:*),DELTA(0:*),ALPHA,BETA,A,B,AA,AB,BA,BB
      INTEGER N
      EXTERNAL RECUR
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C FORTRAN-77 Version 2.2: March 1987
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Divide the polynomial SUM(I=0 to N) GAMMA(I)*P(I,X)
C by the quadratic factor, P(2,X)-ALPHA*P(1,X)-BETA*P(0,X)
C giving the quotient SUM(I=0 to N-2) DELTA(I)*P(I,X)
C and remainder A*P(1,X)+B*P(0,X) where P(I,X) is the orthogonal
C polynomial of degree I defined by RECUR.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C
C Input parameters:
C
C N          = Degree of GAMMA
C GAMMA      = Polynomial to be divided by quadratic factor
C ALPHA,BETA = Coefficients of quadratic factor
C RECUR      = Name of the subroutine which defines the orthogonal
C              polynomials. See EXTEND for a full description.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Output parameters:
C
C DELTA = Quotient polynomial of degreee N-2
C A,B   = Remainder coefficients
C AA    = Partial of A with respect to ALPHA
C AB    = Partial of A with respect to BETA
C BA    = Partial of B with respect to ALPHA
C BB    = Partial of B with respect to BETA
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
C Initialise coefficients
      DNP2=ZERO
      DNP1=ZERO
      DN  =ZERO
      DNM1=ZERO
C Partial coefficients wrt ALPHA
      ADNP2=ZERO
      ADNP1=ZERO
      ADN  =ZERO
      ADNM1=ZERO
C Partial coefficients wrt BETA
      BDNP2=ZERO
      BDNP1=ZERO
      BDN  =ZERO
      BDNM1=ZERO
C
C Scaling parameters
      SN1=ONE
      SN2=ONE
      SN3=ONE
      SN4=ONE
      CALL RECUR(0,C0,D0,E0)
      CALL RECUR(1,C1,D1,E1)
      CALL RECUR(2,C2,D2,E2)
      CALL RECUR(3,C3,D3,E3)
      R0=-C0*E1/C1
      R1=-C0*E2/C2
      R2=-C0*E3/C3
      VM1=D0-C0*D1/C1
      VM2=D0-C0*D2/C2
      W0=-R1*E1
      W1=-C1*R2*E2/C2
      V1=D1*R1-C1*VM2*E2/C2-C1*R1*D1/C1
      K=N-2
      CALL RECUR(K+4,CK4,DK4,EK4)
      CALL RECUR(K+3,CK3,DK3,EK3)
      CALL RECUR(K+2,CK2,DK2,EK2)
      CALL RECUR(K+1,CK1,DK1,EK1)
      VLK4=C0/CK3
      VLK3=C0/CK2
      VLK2=C0/CK1
      RK3=-C0*EK4/CK4
      RK2=-C0*EK3/CK3
      VMK3=D0-DK3*VLK4
      VMK2=D0-DK2*VLK3
C Extract quadratic factor and find partial derivatives
      DO 100 K=N-2,0,-1
        CALL RECUR(K,CK,DK,EK)
        VLK1=C0/CK
        RK1=-C0*EK2/CK2
        VMK1=D0-DK1*VLK2
        SK2=C1*VLK1*VLK2/C0
        TK2=VLK2*(D1-C1*DK2/CK2)+C1*VMK1/CK1
        UK2=D1*VMK2+E1-C1*VLK3*EK3/CK3-C1*VMK2*DK2/CK2+C1*RK1/CK1
        VK2=D1*RK2-C1*VMK3*EK3/CK3-C1*RK2*DK2/CK2
        WK2=-C1*RK3*EK3/CK3
        CF1=(ALPHA*VLK2-TK2)/SN1
        CF2=(BETA+ALPHA*VMK2-UK2)/SN2
        CF3=(ALPHA*RK2-VK2)/SN3
        CF4=-WK2/SN4
        RS=GAMMA(K+2)
        D=RS+CF1*DNM1+CF2*DN+CF3*DNP1+CF4*DNP2
        DELTA(K)=D/SK2
        DA=VLK2*DNM1/SN1+VMK2*DN/SN2+RK2*DNP1/SN3
     *      + CF1*ADNM1+CF2*ADN+CF3*ADNP1+CF4*ADNP2
        DB=DN/SN2+CF1*BDNM1+CF2*BDN+CF3*BDNP1+CF4*BDNP2
C Recycle old values
        SN4=SN3
        SN3=SN2
        SN2=SN1
        SN1=SK2
        DNP2=DNP1
        DNP1=DN
        DN=DNM1
        DNM1=D
        ADNP2=ADNP1
        ADNP1=ADN
        ADN=ADNM1
        ADNM1=DA
        BDNP2=BDNP1
        BDNP1=BDN
        BDN=BDNM1
        BDNM1=DB
        CK4=CK3
        CK3=CK2
        CK2=CK1
        CK1=CK
        DK4=DK3
        DK3=DK2
        DK2=DK1
        DK1=DK
        EK4=EK3
        EK3=EK2
        EK2=EK1
        EK1=EK
        VLK4=VLK3
        VLK3=VLK2
        VLK2=VLK1
        RK3=RK2
        RK2=RK1
        VMK3=VMK2
        VMK2=VMK1
100     CONTINUE
      CF1=ALPHA
      CF2=BETA+ALPHA*VM1-R1
      CF3=ALPHA*R1-V1
      CF4=-W1
      CF5=ALPHA*R0
      RS0=GAMMA(0)
      RS1=GAMMA(1)
      DNM1=DNM1/SN1
      DN=DN/SN2
      DNP1=DNP1/SN3
      DNP2=DNP2/SN4
      ADNM1=ADNM1/SN1
      ADN=ADN/SN2
      ADNP1=ADNP1/SN3
      ADNP2=ADNP2/SN4
      BDNM1=BDNM1/SN1
      BDN=BDN/SN2
      BDNP1=BDNP1/SN3
      BDNP2=BDNP2/SN4
C Remainder
      A=RS1+CF1*DNM1+CF2*DN+CF3*DNP1+CF4*DNP2
      B=RS0+BETA*DNM1+CF5*DN-W0*DNP1
C Partials
      AA=DNM1+VM1*DN+R1*DNP1+CF1*ADNM1+CF2*ADN+CF3*ADNP1+CF4*ADNP2
      AB=DN+CF1*BDNM1+CF2*BDN+CF3*BDNP1+CF4*BDNP2
      BA=R0*DN+BETA*ADNM1+CF5*ADN-W0*ADNP1
      BB=DNM1+BETA*BDNM1+CF5*BDN-W0*BDNP1
      RETURN
      END
      SUBROUTINE ROOTS(A0,A1,A2,ZREAL1,ZIMAG1,ZREAL2,ZIMAG2,RECUR,INFO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION A0,A1,A2,ZREAL1,ZIMAG1,ZREAL2,ZIMAG2
      INTEGER INFO
      EXTERNAL RECUR
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C FORTRAN-77 Version 2.2: March 1987
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Calculates the roots corresponding to the quadratic factor
C        A2*P(2,X)-A1*P(1,X)-A0*P(0,X)
C where P(I,X) is a general orthogonal polynomial of degree I
C defined by the recurrence calculated by subroutine RECUR.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C
C Input parameters:
C   A0,A1,A2 = Coefficients of quadratic factor
C   RECUR    = Name of the subroutine which defines the orthogonal
C              polynomials. See EXTEND for full description.
C
C Output parameters:
C   ZREAL1   = Real part of root 1
C   ZIMAG1   = Imaginary part of root 1
C   ZREAL2   = Real part of root 2
C   ZIMAG2   = Imaginary part of root 2
C   INFO     = 0 Two roots found
C            = 1 One root only (A2=0)
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      PARAMETER (ZERO=0.0D0,HALF=0.5D0,FOUR=4.0D0)
      INFO=0
C
      CALL RECUR(0,C0,D0,E0)
      IF(A2.EQ.ZERO) THEN
        ZREAL1=-(A0+A1*D0)/A1/C0
        ZREAL2=ZERO
        ZIMAG1=ZERO
        ZIMAG2=ZERO
        INFO=1
        RETURN
      END IF
      CALL RECUR(1,C1,D1,E1)
      AA=-C0*C1*A2
      BB=-A2*(C0*D1+D0*C1)+C0*A1
      CC=-D0*D1*A2-E1*A2+A0+A1*D0
      Z=BB*BB-FOUR*AA*CC
      ZR=SQRT(ABS(Z))
      IF(Z.GE.ZERO) THEN
        ZIMAG1=ZERO
        ZIMAG2=ZERO
        ZREAL1=HALF*(-BB-SIGN(ZR,BB))/AA
        ZREAL2=CC/AA/ZREAL1
      ELSE
        ZREAL1=-HALF*BB/AA
        ZREAL2=ZREAL1
        ZIMAG1=HALF*ZR/AA
        ZIMAG2=-ZIMAG1
      END IF
      END
      SUBROUTINE NEWTON(T,N,XNODE,RECUR,IDIGIT,DELTA,ERRVAL,IFAIL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION T(0:*),DELTA(0:*)
      DOUBLE PRECISION XNODE,ERRVAL
      INTEGER N,IDIGIT
      EXTERNAL RECUR
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C FORTRAN-77 Version 2.2: March 1987
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Applies Newton's method to find a single root of the
C polynomial T expressed as:
C                  T =   SUM (I=0 to N) T(I)*P(I,X)
C where P(I,X) are the orthogonal polymonials whose recurrence
C relation is defined by RECUR.
C
C The value of T is found from the remainder when T is divided
C by (X-XNODE). The derivative (of the remainder) is
C calculated simultaneously. The deflated polynomial
C              DELTA = SUM (I=0 to N-1) DELTA(I)*P(I,X)
C is also computed.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C
C Input parameters:
C
C T      = Polynomial whose roots define the nodes of the quadrature rul
C          and expressed as:
C                  T =   SUM (I=0 to N) T(I)*P(I,X)
C          This array should be declared to have at least N+1 elements
C          in the calling program.
C N      = Degree of the expansion of T.
C XNODE  = Approximation to root
C RECUR  = Name of the subroutine which defines the orthogonal
C          polynomials. See EXTEND for a full description.
C IDIGIT = Node convergence paramter (integer greater than 0).
C          An attempt is made to calculate the nodes to the maximum
C          accuracy possible by the machine precision available.
C          IDIGIT controls the assessment procedure to take account of
C          round-off errors and specifies the number of least significan
C          decimal digits that can be ignored (i.e. attributed
C          to round-off) in the computed relative error. Typical
C          value is 5.
C
C Output parameters:
C
C XNODE  = Required root.
C DELTA  = Array holding the coefficients of the deflated polynomial
C          of degree N-1. This array should be declared to have at
C          least N elements in the calling program.
C ERRVAL = Value of the correction. May be used as a measure of the
C          root accuracy when convergence is not achieved.
C IFAIL  = 0, Convergence OK.
C        = 1, Unsatisfactory convergence after 50 iterations.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      PARAMETER (TEN=10.0D0)
C
      ITER=50
      TOL=TEN**(-MAX(1,IDIGIT))
10    ITER=ITER-1
      IF(ITER.LT.0) THEN
        IFAIL=1
        ERRVAL=ABS(EPS)
        RETURN
      END IF
      CALL LFACT(T,DELTA,N,XNODE,RECUR,R,DR)
      EPS=-R/DR
      XNODE=XNODE+EPS
      IF(ABS(XNODE)+TOL*ABS(EPS).NE.ABS(XNODE)) GOTO 10
C Final iteration
      CALL LFACT(T,DELTA,N,XNODE,RECUR,R,DR)
      EPS=-R/DR
      XNODE=XNODE+EPS
      IFAIL=0
      ERRVAL=ABS(EPS)
      RETURN
      END
      SUBROUTINE LFACT(GAMMA,DELTA,N,XNODE,RECUR,R,DR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION GAMMA(0:*),DELTA(0:*),XNODE,R,DR
      INTEGER N
      EXTERNAL RECUR
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C FORTRAN-77 Version 2.2: March 1987
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Remove the linear factor (X-XNODE) from the polynomial expansion
C             SUM(I=0 to N) GAMMA(I) P(I,X)
C to give the quotient,
C             SUM (I=0 to N-1) DELTA(I)*P(I,X).
C and the remainder and its derivative at XNODE.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Input parameters:
C GAMMA  = Polynomial from which factor is to be removed
C          and expressed as:
C             GAMMA =   SUM (I=0 to N) GAMMA(I)*P(I,X)
C          This array should be declared to have at least N+1 elements
C          in the calling program.
C N      = Degree of GAMMA.
C XNODE  = Node to be removed.
C RECUR  = Name of the subroutine which defines the orthogonal
C          polynomials. See EXTEND for a full description.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Output parameters:
C DELTA  = Quotient polynomial expressed as:
C                  DELTA =   SUM (I=0 to N-1) DELTA(I)*P(I,X)
C          This array should be declared to have at least N elements
C          in the calling program.
C R      = Remainder from division.
C DR     = Derivative of R with respect to XNODE.
C          (-R/DR is the Newton correction).
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      PARAMETER (ZERO=0.0D0)
C
      BK1=ZERO
      BK2=ZERO
      DBK1=ZERO
      DBK2=ZERO
      CALL RECUR(N,CK,DK,EK)
      CALL RECUR(N+1,CK1,DK1,EK1)
      DO 10 K=N,0,-1
        R=GAMMA(K)+(DK+XNODE*CK)*BK1+EK1*BK2
        DR=(DK+XNODE*CK)*DBK1+EK1*DBK2+CK*BK1
        BK2=BK1
        BK1=R
        DBK2=DBK1
        DBK1=DR
        IF(K.NE.0) THEN
          CALL RECUR(K-1,CKM1,DKM1,EKM1)
          DELTA(K-1)=R*CKM1
        END IF
        EK1=EK
        CK=CKM1
        DK=DKM1
        EK=EKM1
10      CONTINUE
      RETURN
      END
      SUBROUTINE EPROD(N,J,COEFF,WORK,LW,RECUR,IFAIL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION COEFF(*),WORK(LW,2)
      INTEGER N,J,LW,IFAIL
      EXTERNAL RECUR
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C FORTRAN-77 Version 2.2: March 1987
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Purpose:
C
C Calculates the expansion of a product of two orthogonal polynomials
C
C     P(N,X)*P(J,X) = SUM (I=N-J to N+J ) COEFF(I)*P(I,X)
C
C where J must not exceed N. The orthogonal polynomials are defined
C by the recurrence relation calculated by the external
C subroutine RECUR.
C
C For proper initialisation the subroutine must first be called
C with J=0 and the required value of N. Subsequent calls must be in
C the order J=1,2,,,,,N with the appropriate expansion being
C generated from previous values and returned in COEFF(*). The
C coefficients of P(N-J,X),...., P(N+J,X) are stored in the array
C COEFF(1),...,COEFF(2*J+1).
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Unless indicated otherwise the type of each variable is implied
C by the default FORTRAN-77 naming convention.
C
C Input parameters:
C
C N     = Highest polynomial degree. Note that after the initial
C         call with J=0 the value of N in this argument is ignored.
C J     = Current product of P(J,X) with P(N,X) to be calculated.
C         Note that the subroutine must be first called with J=0 and
C         the required largest N. Subsequent calls must be
C         in the order J=1,2,..,N.
C WORK  = Matrix work area which must be declared in the calling
C         program to have dimensions at least (2*J+1) by 2.
C         The contents of this work area must not be altered between
C         calls by the calling program.
C LW    = Leading dimension of WORK in the calling program
C RECUR = Name of the subroutine which defines the orthogonal
C         polynomials. See EXTEND for a full description.
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Output parameters:
C
C    COEFF = Array holding the calculated coefficients.
C            This array should be declared to have at least 2*J+1 elemen
C            in the calling program.
C    IFAIL = 0 Result OK
C          = 1 J exceeds N
C          = 2 J has not been called sequentially
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
      INTEGER S,SS,IX(2)
      SAVE IX,SS,LAST
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
C
      IFAIL=0
C Initialise
      IF(J.EQ.0) THEN
        IX(1)=1
        IX(2)=2
        COEFF(1)=ONE
        WORK(1,2)=ONE
        LAST=0
        SS=N
        RETURN
      END IF
      S=SS
C Check that J does not exceed S value
      IF(S.LT.J) THEN
        IFAIL=1
        RETURN
      END IF
C Check that J is used sequentially
      IF(LAST.NE.J-1) THEN
        IFAIL=2
        RETURN
      END IF
      LAST=J
      J2=J+J
      CALL RECUR(J-1,CJ1,DJ1,EJ1)
      IF(J.EQ.1) THEN
        DO 20 I=1,J2+1
          COEFF(I)=ZERO
20        CONTINUE
      ELSE
        DO 25 I=1,J2-3
          COEFF(I+2)=WORK(I,IX(1))*EJ1
25        CONTINUE
        COEFF(1)   =ZERO
        COEFF(2)   =ZERO
        COEFF(J2)  =ZERO
        COEFF(J2+1)=ZERO
      END IF
      IBOT=S-J+1
      ITOP=S+J-1
      DO 30 II=IBOT,ITOP
        I=II-IBOT+1
        CALL RECUR(II,CI,DI,EI)
        COEFF(I+2)=COEFF(I+2)+(WORK(I,IX(2))/CI)*CJ1
        COEFF(I+1)=COEFF(I+1)+WORK(I,IX(2))*(DJ1-(CJ1/CI)*DI)
        COEFF(I)=COEFF(I)-(WORK(I,IX(2))/CI)*CJ1*EI
30      CONTINUE
      II=IX(1)
      IX(1)=IX(2)
      IX(2)=II
      DO 35 I=1,J2+1
        WORK(I,IX(2))=COEFF(I)
35      CONTINUE
      RETURN
      END
      SUBROUTINE GEFA77(A,LDA,N,IPVT,INFO)
      INTEGER LDA,N,IPVT(*),INFO
      DOUBLE PRECISION A(LDA,*)
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C This is an FORTRAN-77 adaption of LINPACK routine DGEFA
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C     GEFA77 FACTORS A MATRIX BY GAUSSIAN ELIMINATION.
C
C     ON ENTRY
C
C        A       THE MATRIX TO BE FACTORED.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C     ON RETURN
C
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
C                WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        INFO    INTEGER
C                = 0  NORMAL VALUE.
C                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
C                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES
C                     INDICATE THAT DGESL OR DGEDI WILL DIVIDE BY ZERO
C                     IF CALLED.  USE  RCOND  IN DGECO FOR A RELIABLE
C                     INDICATION OF SINGULARITY.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS subroutines: DAXPY,DSCAL,IDAMAX
C     These have been renamed DAXPY7,DSCAL7,IDAMX7
C
C     INTERNAL VARIABLES
C
      DOUBLE PRECISION T
      DOUBLE PRECISION ZERO,ONE
      INTEGER IDAMX7,J,K,KP1,L,NM1
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
C
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
C
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
C
C        FIND L = PIVOT INDEX
C
         L = IDAMX7(N-K+1,A(K,K),1) + K - 1
         IPVT(K) = L
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
         IF (A(L,K) .EQ. ZERO) GO TO 40
C
C           INTERCHANGE IF NECESSARY
C
            IF (L .EQ. K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
C
C           COMPUTE MULTIPLIERS
C
            T = -ONE/A(K,K)
            CALL DSCAL7(N-K,T,A(K+1,K),1)
C
C           ROW ELIMINATION WITH COLUMN INDEXING
C
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .EQ. K) GO TO 20
                  A(L,J) = A(K,J)
                  A(K,J) = T
   20          CONTINUE
               CALL DAXPY7(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30       CONTINUE
         GO TO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. ZERO) INFO = N
      RETURN
      END
      SUBROUTINE GESL77(A,LDA,N,IPVT,B,JOB)
      INTEGER LDA,N,IPVT(*),JOB
      DOUBLE PRECISION A(LDA,*),B(*)
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C This is an FORTRAN-77 adaption of LINPACK routine DGESL
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C     GESL77 SOLVES THE SYSTEM
C     A * X = B  OR  TRANS(A) * X = B
C     USING THE FACTORS COMPUTED BY GEFA77.
C
C     ON ENTRY
C
C        A       THE OUTPUT FROM GEFA77.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        IPVT    INTEGER(N)
C                THE PIVOT VECTOR FROM GEFA77.
C
C        B       DIMENSION (N)
C                THE RIGHT HAND SIDE VECTOR.
C
C        JOB     INTEGER
C                = 0         TO SOLVE  A*X = B ,
C                = NONZERO   TO SOLVE  TRANS(A)*X = B  WHERE
C                            TRANS(A)  IS THE TRANSPOSE.
C
C     ON RETURN
C
C        B       THE SOLUTION VECTOR  X .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
C        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
C        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
C        CALLED CORRECTLY AND IF DGECO HAS SET RCOND .GT. 0.0
C        OR DGEFA HAS SET INFO .EQ. 0 .
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS subroutines: DAXPY,DDOT
C     These have been renamed: DAXPY7,DDOT7
C     INTERNAL VARIABLES
C
      INTEGER K,KB,L,NM1
      DOUBLE PRECISION DDOT7,T
      DOUBLE PRECISION ZERO,ONE
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
C
      NM1 = N - 1
      IF (JOB .NE. 0) GO TO 50
C
C        JOB = 0 , SOLVE  A * X = B
C        FIRST SOLVE  L*Y = B
C
         IF (NM1 .LT. 1) GO TO 30
         DO 20 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L .EQ. K) GO TO 10
               B(L) = B(K)
               B(K) = T
   10       CONTINUE
            CALL DAXPY7(N-K,T,A(K+1,K),1,B(K+1),1)
   20    CONTINUE
   30    CONTINUE
C
C        NOW SOLVE  U*X = Y
C
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/A(K,K)
            T = -B(K)
            CALL DAXPY7(K-1,T,A(1,K),1,B(1),1)
   40    CONTINUE
      GO TO 100
   50 CONTINUE
C
C        JOB = NONZERO, SOLVE  TRANS(A) * X = B
C        FIRST SOLVE  TRANS(U)*Y = B
C
         DO 60 K = 1, N
            T = DDOT7(K-1,A(1,K),1,B(1),1)
            B(K) = (B(K) - T)/A(K,K)
   60    CONTINUE
C
C        NOW SOLVE TRANS(L)*X = Y
C
         IF (NM1 .LT. 1) GO TO 90
         DO 80 KB = 1, NM1
            K = N - KB
            B(K) = B(K) + DDOT7(N-K,A(K+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF (L .EQ. K) GO TO 70
               T = B(L)
               B(L) = B(K)
               B(K) = T
   70       CONTINUE
   80    CONTINUE
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
      SUBROUTINE  DSCAL7(N,DA,DX,INCX)
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C This is an FORTRAN-77 adaption of BLAS routine DSCAL
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C     SCALES A VECTOR BY A CONSTANT.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
C
      DOUBLE PRECISION DA,DX(*)
      DOUBLE PRECISION ZERO,ONE
      INTEGER I,INCX,M,MP1,N,NINCX
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I + 1) = DA*DX(I + 1)
        DX(I + 2) = DA*DX(I + 2)
        DX(I + 3) = DA*DX(I + 3)
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE
      RETURN
      END
      INTEGER FUNCTION IDAMX7(N,DX,INCX)
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C This is an FORTRAN-77 adaption of BLAS routine IDAMAX
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
C
      DOUBLE PRECISION DX(*),DMAX
      INTEGER I,INCX,IX,N
C
      IDAMX7 = 0
      IF( N .LT. 1 ) RETURN
      IDAMX7 = 1
      IF(N.EQ.1)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      DMAX = ABS(DX(1))
      IX = IX + INCX
      DO 10 I = 2,N
         IF(ABS(DX(IX)).LE.DMAX) GO TO 5
         IDAMX7 = I
         DMAX = ABS(DX(IX))
    5    IX = IX + INCX
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
   20 DMAX = ABS(DX(1))
      DO 30 I = 2,N
         IF(ABS(DX(I)).LE.DMAX) GO TO 30
         IDAMX7 = I
         DMAX = ABS(DX(I))
   30 CONTINUE
      RETURN
      END
      SUBROUTINE DAXPY7(N,DA,DX,INCX,DY,INCY)
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C This is an FORTRAN-77 adaption of BLAS routine DAXPY
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C
      DOUBLE PRECISION DX(*),DY(*),DA
      INTEGER I,INCX,INCY,M,MP1,N
      DOUBLE PRECISION ZERO,ONE
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
C
      IF(N.LE.0)RETURN
      IF (DA .EQ. ZERO) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DY(IY) + DA*DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
   50 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DDOT7(N,DX,INCX,DY,INCY)
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C This is an FORTRAN-77 adaption of BLAS routine DDOT
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     FORMS THE DOT PRODUCT OF TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C
      DOUBLE PRECISION DX(*),DY(*),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
      DOUBLE PRECISION ZERO,ONE
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
C
      DDOT7 = ZERO
      DTEMP = ZERO
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = DTEMP + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      DDOT7 = DTEMP
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DTEMP = DTEMP + DX(I)*DY(I) + DX(I + 1)*DY(I + 1) +
     *   DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
   60 DDOT7 = DTEMP
      RETURN
      END

