      PROGRAM main

c*********************************************************************72
c
cc MAIN is the main program for CODE1.
c
c  Discussion:
c
c    CODE1 uses the finite element method to solve a 1D boundary value problem.
c
c  Modified:
c
c    13 May 2014
c
c  Author:
c
c    Original FORTRAN77 version by Eric Becker, Graham Carey, John Oden.
c
c  Reference:
c
c    Eric Becker, Graham Carey, John Oden,
c    Finite Elements, An Introduction, Volume I,
c    Prentice-Hall, 1981,
c    ISBN: 0133170578,
c    LC: TA347.F5.B4.
c
      include 'control.inc'
      include 'elements.inc'
      include 'materials.inc'
      include 'bc_data.inc'
      include 'integration.inc'
      include 'nodes.inc'
c
c  INITIALIZE GLOBAL ARRAYS.
c
      do 2 i=1,99
      kind(i)=0
      mat(i)=0
      nint(i)=0
      do 2 j=1,4
  2    nodes(j,i)=0
      CALL PREP
      CALL PROC
      CALL POST
      stop
      END
      SUBROUTINE PREP
cc PREP does preprocessing.
      include 'control.inc'
      READ(5,100)TITLE
      WRITE(6,101) TITLE
      CALL RCON
      CALL RNODE
      CALL RELEM
      CALL RMAT
      CALL RBC
      CALL SETINT
      RETURN
      STOP
100   FORMAT(10A8)
101   FORMAT(1H1,10A8)
      END
      SUBROUTINE RCON
cc RCON reads control parameters.
      include 'control.inc'
      READ(5,*) NNODE,NELEM,NMAT,NPOINT
      WRITE(6,101) NNODE,NELEM,NMAT,NPOINT
 101  FORMAT(//' NNODE =',I3/' NELEM =',I3/' NMAT  =',I3/ ' NPOINT=',I3
     .    ///)
      RETURN
      END
      SUBROUTINE RNODE
cc RNODE reads node information.
      include 'control.inc'
      include 'nodes.inc'
      REAL LARGE
      LARGE=1.0E+20
      DO 10 I=1,NNODE
10    X(I)=LARGE
      READ(5,*) NREC
      DO 30 NR=1,NREC
      READ(5,*)N1,N2,X1,X2
      IF(N1.LT.1.OR.N2.GT.NNODE) GO TO 99
      IF(N2.NE.0) GO TO 11
      X(N1)=X1
      GO TO 30
 11   DN=N2-N1
      DX=(X2-X1)/DN
      XX=X1-DX
      DO 20 I=N1,N2
      XX=XX+DX
20    X(I)=XX
30    CONTINUE
      WRITE (6,101) (I,X(I),I=1,NNODE)
      DO 40 I=1,NNODE
      IF(X(I).EQ.LARGE) GO TO 99
40    CONTINUE
      RETURN
99    WRITE(6,102)
      STOP
 101  FORMAT('  NODE NO.',I6,5X,12HX-COORDINATE,F11.3)
102   FORMAT(42H0-----ERROR IN NODAL POINT COORDINATE DATA)
      END
      SUBROUTINE RELEM
cc RELEM reads element information.
      include 'control.inc'
      include 'elements.inc'
      READ(5,*)NREC
      NOD=1
      DO 10 NR=1,NREC
      READ(5,*)NEL1,KIND1,MAT1,NINT1,NODE1,NUMBER
      NN=KIND1+1
      NEL2=NEL1+NUMBER-1
      NOD=NODE1
      DO 10 NEL=NEL1,NEL2
      NOD=NOD-1
      KIND(NEL)=KIND1
      MAT(NEL)=MAT1
      NINT(NEL)=NINT1
      DO 10 N=1,NN
      NOD=NOD+1
  10  NODES(N,NEL)=NOD
      WRITE(6,101)
      DO 20 NEL=1,NELEM
  20  WRITE(6,102)NEL,KIND(NEL),MAT(NEL),NINT(NEL),(NODES(I,NEL)
     .   ,I=1,4)
 101  FORMAT(///'EL NO   KIND   MAT   NINT       NODES')
 102  FORMAT(8I6)
      RETURN
      END
      SUBROUTINE RMAT
cc RMAT reads material information.
      include 'control.inc'
      include 'materials.inc'
      READ(5,*) ((PROP(I,J),I=1,4),J=1,NMAT)
      WRITE(6,101)
      WRITE(6,102) (J,(PROP(I,J),I=1,4),J=1,NMAT)
 101  FORMAT(///'MAT NO',10X,'K',10X,'C',10X,'B',10X,'F')
 102  FORMAT(I7,4E11.3)
      RETURN
      END
      SUBROUTINE RBC
cc RBC reads boundary condition information.
      include 'control.inc'
      include 'bc_data.inc'
      READ(5,*)(KBC(I),VBC(1,I),VBC(2,I),I=1,2)
      WRITE(6,101)(KBC(I),VBC(1,I),VBC(2,I),I=1,2)
      IF(NPOINT.EQ.0)RETURN
      READ(5,*)  (KPOINT(I),POINT(I),I=1,NPOINT)
      WRITE(6,103) (KPOINT(I),POINT(I),I=1,NPOINT)
 101  FORMAT(///' BOUNDARY CONDITION DATA'
     .    /10X,'TYPE',7X,'V1',10X,'V2',
     .    /' AT X=0:'I5,2E12.3
     .    /' AT X=L:'I5,2E12.3)
 102  FORMAT(I5,F10.0)
 103  FORMAT(//' POINT LOAD DATA:'/,(I5,E12.3))
      RETURN
      END
      SUBROUTINE PROC
cc PROC carries out the solution procedure.
      include 'control.inc'
      CALL SETINT
      CALL FORMKF
      CALL APLYBC
      CALL SOLVE
      RETURN
      END
      SUBROUTINE SETINT
cc SETINT sets quadrature data.
      include 'control.inc'
      include 'integration.inc'
C.....GAUSS QUADRATURE ORDER 1
      XI(1,1)=0.
      W(1,1)=2.
C.....GAUSS QUADRATURE ORDER 2
      XI(1,2)=-1./SQRT(3.)
      XI(2,2)=-XI(1,2)
      W(1,2)=1.0
      W(2,2)=W(1,2)
C....GAUSS QUADRATURE ORDER 3
      XI(1,3)=-SQRT(3./5.)
      XI(2,3)=0.
      XI(3,3)=-XI(1,3)
      W(1,3)=5./9.
      W(2,3)=8./9.
      W(3,3)=W(1,3)
C.....GAUSS QUADRATURE ORDER 4
      XI(1,4)=-0.8611363115
      XI(2,4)=-0.3399810435
      XI(3,4)=-XI(2,4)
      XI(4,4)=-XI(1,4)
      W(1,4)=0.3478548451
      W(2,4)=0.6521451549
      W(3,4)=W(2,4)
      W(4,4)=W(1,4)
      RETURN
      END
      SUBROUTINE FORMKF
cc FORMKF sets the stiffness matrix and right hand side.
      include 'control.inc'
      include 'nodes.inc'
      include 'elements.inc'
      include 'integration.inc'
      include 'matrix.inc'
      DIMENSION EK(4,4),EF(4)
      DO 10 I=1,NNODE
      GF(I)=0.0
      DO 10 J=1,NNODE
   10 GK(I,J)=0.0
      NG=100
      DO 20 NEL=1,NELEM
      N=KIND(NEL)+1
      I1=NODES(1,NEL)
      I2=NODES(N,NEL)
      I3=NINT(NEL)
      CALL ELEM(X(I1),X(I2),N,EK,EF,I3,XI(1,I3),W(1,I3),MAT(NEL))
      CALL ASSMB(EK,EF,N,NODES(1,NEL),GK,GF,NG)
   20 CONTINUE
      RETURN
      END
      SUBROUTINE ELEM(X1,X2,N,EK,EF,NL,XI,W,MAT)
cc ELEM integrates the equations over one element.
      include 'control.inc'
      DIMENSION EK(N,1),EF(1),XI(1),W(1)
      DIMENSION PSI(4),DPSI(4)
      DX=(X2-X1)/2.
C.....INITIALIZE ELEMENT ARRAYS
      DO 10 I=1,N
      EF(I)=0.0
      DO 10 J=1,N
10    EK(I,J)=0.
C.....BEGIN INTEGRATION POINT LOOP
      DO 20 L=1,NL
      X=X1+(1.+XI(L))*DX
      CALL GETMAT(MAT,X,XK,XC,XB,XF)
      CALL SHAPE(XI(L),N,PSI,DPSI)
      DO 20 I=1,N
11    EF(I)=EF(I)+PSI(I)*XF*W(L)*DX
      DO 20 J=1,N
12    EK(I,J)=EK(I,J)+(XK*DPSI(I)*DPSI(J)/DX/DX
     1        +XC*PSI(I)*DPSI(J)/DX+XB*PSI(I)*PSI(J))*W(L)*DX
20    CONTINUE
      RETURN
      END
      SUBROUTINE SHAPE(XI,N,PSI,DPSI)
cc SHAPE evaluates the shape functions.
      include 'control.inc'
      DIMENSION PSI(*),DPSI(*)
      IF(N.LT.2.OR.N.GT.4) GO TO 99
      GO TO (99,10,20,30) N
C.....LINEAR SHAPE FUNCTIONS
10    PSI(1)=.5*(1.-XI)
      PSI(2)=.5*(1.+XI)
      DPSI(1)=-.5
      DPSI(2)= .5
      RETURN
C.....QUADRATIC ELEMENTS
   20 PSI(1)=XI*(XI-1.)*.5
      PSI(2)=1.-XI**2
      PSI(3)=XI*(XI+1.)*.5
      DPSI(1)=XI-.5
      DPSI(2)=-2.*XI
      DPSI(3)=XI+.5
      RETURN
C.....CUBIC ELEMENTS
 30   PSI(1)=9./2.*(1./9.-XI**2)*(1.-XI)
      PSI(2)=16./27.*(1.-XI**2)*(1./3.-XI)
      PSI(3)=16./27.*(1.-XI**2)*(1./3.+XI)
      PSI(4)=9./2.*(1./9.-XI**2)*(1.+XI)
      DPSI(1)=9./2.*(3.*XI**2-2.*XI-1./9.)
      DPSI(2)=16./27.*(3.*XI**2-2./3.*XI-1.)
      DPSI(3)=16./27.*(-3.*XI**2-2./3.*XI+1.)
      DPSI(4)=9./2.*(-3.*XI**2-2.*XI+1./9.)
      RETURN
99    WRITE(6,100)N
100   FORMAT(' ERROR IN CALL TO SHAPE,N=',I3)
      STOP
      END
      SUBROUTINE GETMAT(MAT,X,XK,XC,XB,XF)
cc GETMAT retrieves the properties of a given material.
      include 'control.inc'
      include 'materials.inc'
      XK=PROP(1,MAT)
      XC=PROP(2,MAT)
      XB=PROP(3,MAT)
      XF=X
      RETURN
      END
      SUBROUTINE ASSMB(EK,EF,N,NODE,GK,GF,NNODE)
cc ASSMB adds an elemental contribution to the global matrix.
      DIMENSION EK(N,1),EF(1),NODE(1),GK(NNODE,1),GF(1)
      DO 10 I=1,N
      IG=NODE(I)
      GF(IG)=GF(IG)+EF(I)
      DO 10 J=1,N
      JG=NODE(J)
10    GK(IG,JG)=GK(IG,JG)+EK(I,J)
      RETURN
      END
      SUBROUTINE APLYBC
cc APLYBC applies boundary conditions.
      include 'control.inc'
      include 'bc_data.inc'
      include 'matrix.inc'
C.....BOUNDARY CONDITION AT NODE 1
      IF(KBC(1).LT.1.OR.KBC(1).GT.3) GO TO 99
      GO TO (10,11,12) KBC(1)
   10 CALL DRCHLT(VBC(1,1),1,NNODE)
      GO TO 15
11    GF(1)=GF(1)+VBC(1,1)
      GO TO 15
12    GK(1,1)=GF(1)+VBC(1,1)
      GF(1)=GF(1)+VBC(1,1)*VBC(2,1)
C.....BOUNDARY CONDITION AT NODE NNODE
15    IF (KBC(2).LT.1.OR.KBC(2).GT.3) GO TO 99
      GO TO (20,21,22)KBC(2)
   20 CALL DRCHLT(VBC(1,2),NNODE,NNODE)
      RETURN
21    GF(NNODE)=GF(NNODE)+VBC(1,2)
      RETURN
22    GK(NNODE,NNODE)=GK(NNODE,NNODE)+VBC(1,2)
      GF(NNODE)=GF(NNODE)+VBC(1,2)*VBC(2,2)
      RETURN
99    WRITE(6,100)KBC
100   FORMAT('ERROR IN CALL TO APLYBC KBC= ',I3)
      STOP
      END
      SUBROUTINE DRCHLT(VAL,N,NNODE)
cc DRCHLT applies a Dirichlet boundary condition.
      include 'matrix.inc'
      DO 10 I=1,NNODE
      GF(I)=GF(I)-GK(I,N)*VAL
      GK(I,N)=0.
10    GK(N,I)=0.
      GK(N,N)=1.
      GF(N)=VAL
      RETURN
      END
      SUBROUTINE SOLVE
cc SOLVE solves the linear system.
      include 'control.inc'
      include 'matrix.inc'
      include 'nodes.inc'
      CALL TRI(GK,100,NNODE)
      CALL BACK(GK,U,GF,100,NNODE)
      RETURN
      END
      SUBROUTINE TRI(A,N,M)
cc TRI factors a linear system.
      include 'control.inc'
      DIMENSION A(N,N)
      M1=M-1
      TINY=1.E-30
      DO 30 I=1,M1
      IF(ABS(A(I,I)).LT.TINY) GO TO 99
      J1=I+1
      DO 20 J=J1,M
      IF(A(J,I).EQ.0.) GO TO 20
      FAC=A(J,I)/A(I,I)
      DO 10 K=J1,M
   10 A(J,K)=A(J,K)-A(I,K)*FAC
20    CONTINUE
30    CONTINUE
      RETURN
99    WRITE (6,100)I,A(I,I)
100   FORMAT('ELIMINATION FAILED DUE TO SMALL PIVOT'/
     .' EQUATION NO.',I5,'PIVOT',E10.3)
      STOP
      END
      SUBROUTINE BACK(A,X,B,N,M)
cc BACK solves a linear system with factored matrix.
      include 'control.inc'
      DIMENSION A(N,N),X(1),B(1)
      M1=M-1
      DO 20 I=1,M1
      J1=I+1
      DO 10 J=J1,M
10    B(J)=B(J)-B(I)*A(J,I)/A(I,I)
20    CONTINUE
      X(M)=B(M)/A(M,M)
      DO 40 I=1,M1
      IB=M-I
      J1=IB+1
      DO 30 J=J1,M
30    B(IB)=B(IB)-A(IB,J)*X(J)
      X(IB)=B(IB)/A(IB,IB)
40    CONTINUE
      RETURN
      END
      SUBROUTINE POST
cc POST carries out post-processing.
      include 'control.inc'
      character * ( 5 ) cmmnd
      DIMENSION OUTPTS(10),EU(4)
   1  READ(5,100) CMMND
      write(6,*) cmmnd
      IF(CMMND.EQ.' SOLN') THEN
C....  OUTPUT SOLUTION AT SELECTED POINTS
      READ(5,*) NEL1,NEL2
        IF(NEL1+NEL2.EQ.0) THEN
         NEL1=1
         NEL2=NELEM
        END IF
       READ(5,*)NPTS,(OUTPTS(I),I=1,NPTS)
       WRITE(6,102) NPTS,(OUTPTS(I),I=1,NPTS)
   11  WRITE(6,103)TITLE
       DO 13 NEL=NEL1,NEL2
       WRITE(6,104) NEL
       DO 13 NPT=1,NPTS
       CALL EVAL(NEL,OUTPTS(NPT),XX,UH,UX,SIGH,SIGX)
       DU=UX-UH
       DSIG=SIGX-SIGH
       WRITE(6,105) XX,UH,UX,DU,SIGH,SIGX,DSIG
13     CONTINUE
       GO TO 1
      ELSE IF (CMMND.EQ.' NORM') THEN
C..... CALL ERROR NORM ROUTINE (IF EXACT SOLUTION IS AVAILABLE)
 20    CALL ENORMS
       GO TO 1

      ELSE IF ( CMMND .EQ. ' PLOT' ) THEN

        open ( unit = 1, file = 'plot.txt', status = 'replace' )

        nel = 1
        s = -1.0
        call eval ( nel, s, xx, uh, ux, sigh, sigx )
        write ( 1, * ) xx, uh, ux, sigh, sigx

        do nel = 1, nelem
          do npt = 1, npts
            s = ( real ( npts - npt ) * ( - 1.0 )
     &          + real (        npt ) * ( + 1.0 ) )
     &          / real ( npts )
            call eval ( nel, s, xx, uh, ux, sigh, sigx )
            write ( 1, * ) xx, uh, ux, sigh, sigx
          end do
        end do

        close ( unit = 1 )
        write ( *, '(a)' ) '  Created plot file "plot.txt"'

        go to 1

      END IF

      TITLE(1)=CMMND
      RETURN
100   FORMAT(A5,2I5)
102   FORMAT(///,'EVALUATE SOLUTION AT',I3,
     .  ' OUTPUT POINTS PER ELEMENT'/'  XI= ',(10(F6.3,',')))
103   FORMAT(///,10A8,///,
     .4X,1HX,8X,1HU,10X,1HU,10X,1HU,7X,7HK DU/DX,4X,
     .7HK DU/DX,4X,7HK DU/DX/
     .12X,3HF E,7X,5HEXACT,6X,5HERROR,7X,3HF E,7X,5HEXACT,
     .6X,5HERROR)
104   FORMAT('ELEMENT NO.',I3)
105   FORMAT(F8.4,6E11.3)
      END
      SUBROUTINE EVAL(NEL,XI,XX,UH,UX,SIGH,SIGX)
cc EVAL evaluates the computed solution at a point.
      include 'control.inc'
      include 'nodes.inc'
      include 'elements.inc'
      DIMENSION PSI(4),DPSI(4)
      N=KIND(NEL)+1
      CALL SHAPE(XI,N,PSI,DPSI)
C....  CALCULATE UH AND DUH/DX FROM SHAPE FUNCTIONS
      UH=0.
      SIGH=0.
      DO 10 I=1,N
      I1=NODES(I,NEL)
      IF(I.EQ.1) X1=X(I1)
      IF(I.EQ.N) X2=X(I1)
      UH=UH+PSI(I)*U(I1)
10    SIGH=SIGH+DPSI(I)*U(I1)
      DX=(X2-X1)*.5
      XX=X1+(1.+XI)*DX
      CALL GETMAT(MAT(NEL),XX,XK,XC,XB,XF)
      SIGH=SIGH*XK/DX
      CALL EXACT(XX,UX,SIGX)
      RETURN
      END
      SUBROUTINE EXACT(X,U,SIG)
cc EXACT evaluates the exact solution.
      include 'control.inc'
C.... MODEL PROBLEM
C.... -D2U/DX2+U=X
C.... U(X)=X-SINH(X)/SINH(1)
C.... SIG(X)=1-COSH(X)/SINH(1)
C....
      E=EXP(1.)
      EX=EXP(X)
      S1=E-1./E
      SX=EX-1./EX
      CX=EX+1./EX
      U=X-SX/S1
      SIG=1.-CX/S1
      RETURN
      END
      SUBROUTINE ENORMS
cc ENORMS evaluates error norms.
      include 'control.inc'
      include 'nodes.inc'
      include 'elements.inc'
      SQNORM=0.
      ENORM=0.
      DXI=1./25.
      DO 40 I=1,NELEM
      N1=NODES(1,I)
      N=KIND(I)+1
      N2=NODES(N,I)
      WT=(X(N2)-X(N1))/50.
      XI=-1.
      DO 40 K=1,51
      XX=X(N1)+(1.+XI)*(X(N2)-X(N1))/2.
      CALL EVAL(I,XI,XX,UH,UX,SIGH,SIGX)
      CALL GETMAT(MAT(I),XX,XK,XC,XB,XF)
      IF(K.EQ.1.OR.K.EQ.51) THEN
      W=WT/2.
      ELSE
      W=WT
      END IF
      XI=XI+.02
30    SQNORM=SQNORM+(UX-UH)**2*W
40    ENORM=ENORM+(SIGX-SIGH)**2*W/XK+(UX-UH)**2*W*XB
      SQNORM=SQRT(SQNORM)
      ENORM=SQRT(ENORM)
      WRITE(6,100) TITLE,SQNORM,ENORM
100   FORMAT(///,10A8,/,'THE H0 NORM OF THE ERROR IS   ',E13.3/
     .    'THE ENERGY NORM OF THE ERROR IS',E12.3)
      RETURN
      END

