      PROGRAM CODE2
C**************************************************************
C**************************************************************
C                                                             *
C     MAIN PROGRAM                                            *
C                                                             *
C**************************************************************
C**************************************************************
c
c  Reference:
c
c    Eric Becker, Graham Carey, John Oden,
c    Finite Elements, An Introduction, Volume I,
c    Prentice-Hall, 1981,
c    ISBN: 0133170578,
c    LC: TA347.F5.B4.
c
      implicit double precision (A-H,O-Z)
      CHARACTER*8 TITLE
      COMMON/CCON/TITLE(10),NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      COMMON/CELEM/MAT(100),NE(100),NODES(9,100)
      COMMON/CNODE/X(2,450),U(450)
      COMMON/CMATL/PROP(3,5)
      COMMON/CBC/NODBC1(70),VBC1(70),NELBC(40),NSIDE(40),VBC2(2,40),
     .  NPT(10),VPT(10)
      COMMON/CMATRX/GK(450,50),GF(450),IB
      COMMON/CINT/XIQ(2,9),XIT(2,4),WQ(9),WT(4)
c     OPEN(5,FILE='CODE2.DAT')
c     OPEN(7,FILE='Code2.out',STATUS='NEW')
      CALL SETINT
      CALL PREP
      CALL PROC
      CALL POST
      END
      SUBROUTINE PREP
C**************************************************************
C**************************************************************
C
C     PREPROCESSOR CALLING; ROUTINE- PREP
C
C**************************************************************
C**************************************************************
      implicit double precision (A-H,O-Z)
      CHARACTER*8 TITLE
      COMMON/CCON/TITLE(10),NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      READ(*,100)TITLE
      WRITE(*,101) TITLE
      CALL RCON
      CALL RNODE
      CALL RELEM
      CALL RMAT
      CALL RBC
      RETURN
99    STOP
100   FORMAT(10A8)
101   FORMAT(////10A8/80('*')/)
      END
C
C
C
      SUBROUTINE RCON
C**************************************************************
C
C     CONTROL DATA; ROUTINE-RCON
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      CHARACTER*8 TITLE
      COMMON/CCON/TITLE(10),NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      READ(*,*)NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      IF((NNODE.GT.450).OR.(NNODE.LT.3)) GO TO 95
      IF((NELEM.GT.100).OR.(NELEM.LT.1)) GO TO 95
      IF(NPOINT.GT.10) GO TO 95
      IF((NMAT.GT.5).OR.(NMAT.LT.1)) GO TO 95
      write(*,110)NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      RETURN
95    write(*,120)
      STOP
100   FORMAT(6I5)
110   FORMAT(/'NUMBER OF NODAL POINTS =',I4/
     &       /'NUMBER OF ELEMENTS     =',I4/
     &       /'NUMBER OF MATERIALS    =',I4/
     &       /'NUMBER OF POINT LOADS  =',I4/
     &/'NUMBER OF NODES WITH ESSENTIAL BOUNDARY COND.       =',I4/
     &/'NUMBER OF ELEMENT SIDES WITH NATURAL BOUNDARY COND. =',I4//)
120   FORMAT (5X,'ERROR MESSAGE.',5X/,' THE DATA ARE OUTSIDE THE ALLOWAB
     &LE RANGE OF CONTROL PARAMETERS ')
      END
C
C
C
      SUBROUTINE RNODE
C**************************************************************
C
C     NODAL POINT COORDINATE DEFINITION; ROUTINE-RNODE
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      CHARACTER*8 TITLE
      COMMON/CCON/TITLE(10),NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      COMMON/CNODE/X(2,450),U(450)

      READ(*,*)NREC

      DO 20 IREC=1,NREC
        READ(*,*)N1,NN,INC,X1,Y1,XN,YN
	NUM=NN-N1+1
        NUM1=(NUM-1)/INC
        XNUM=NUM1
        X(1,N1)=X1
        X(2,N1)=Y1
        IF (1 .lt. NUM ) THEN
          DX=(XN-X1)/XNUM
          DY=(YN-Y1)/XNUM
          DO 10 N=1,NUM1
            IN=N1+N*INC
            X(1,IN)=X1+N*DX
            X(2,IN)=Y1+N*DY
10        CONTINUE
        END IF

20    CONTINUE
c
C.....PRINT NODAL POINT COORDINATES
c
      write(*,101)
      DO 30 N=1,NNODE
        write(*,102) N,X(1,N),X(2,N)
30    CONTINUE
      RETURN
100   FORMAT(3I5,4F10.0)
101   FORMAT(39HNODE NO.   X-COORDINATE    Y-COORDINATE/39('-')/)
102   FORMAT(I5,2X,2E16.4)
110   FORMAT(20A4)
      END
C
C
C
      SUBROUTINE RELEM
C**************************************************************
C
C     ELEMENT DATA DEFINITION; ROUTINE-RELEM
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      CHARACTER*8 TITLE
      COMMON/CCON/TITLE(10),NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      COMMON/CELEM/MAT(100),NE(100),NODES(9,100)
      DIMENSION NODE(9)
C.....READ ELEMENT DATA
      READ(*,*)NREC
      NEL=0
      DO 20 IREC=1,NREC
        READ(*,*)NUM,INC,NEE,MATE,(NODE(I),I=1,NEE)
        N1=NEL+1
        NEL=NEL+NUM
        IF(NEL.GT.100) GO TO 99
        DO 20 N=N1,NEL
          NINC=(N-N1)*INC
          DO 10 M=1,NEE
            NODES(M,N)=NODE(M)+NINC
10        CONTINUE
          NE(N)=NEE
          MAT(N)=MATE
20    CONTINUE
C.....PRINT ELEMENT DEFINITIONS
      write(*,101)
      DO 30 N=1,NELEM
        NEN=NE(N)
        write(*,102)N,MAT(N),NE(N),(NODES(I,N),I=1,NEN)
30    CONTINUE
      RETURN
99    write(*,103)
100   FORMAT(13I5)
101   FORMAT(//48HELEMENT   MATERIAL   NO. OF NODES   GLOBAL NODE
     &       7HNUMBERS/78('-')/)
102   FORMAT(I4,6X,I8,I9,6X,9I5)
103   FORMAT(37H0ELEMENT NUMBER EXCEEDS MAXIMUM VALUE  )
      STOP
      END
C
C
C
      SUBROUTINE RBC
C**************************************************************
C
C     BOUNDARY CONDITION DEFINITION; ROUTINE-RBC
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      CHARACTER*8 TITLE
      COMMON/CCON/TITLE(10),NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      COMMON/CBC/NODBC1(70),VBC1(70),NELBC(40),NSIDE(40),VBC2(2,40),
     &NPT(10),VPT(10)
C.....READ POINT LOADS
      IF(NPOINT.EQ.0) GO TO 20
      write(*,115)
      DO 10 I=1,NPOINT
        READ(*,*)N,V
        write(*,125)I,N,V
        NPT(I)=N
        VPT(I)=V
10    CONTINUE
C.....READ ESSENTIAL NODAL POINT BOUNDARY CONDITION DATA
20    IF(NBC1.EQ.0) GO TO 40
      NBC1=0
      READ(*,*)NREC
      write(*,135)NREC
      DO 30 J=1,NREC
        READ(*,*)N1,NUM,INC,V
        DO 30 I=1,NUM
          NBC1=NBC1+1
          N=N1+(I-1)*INC
          NODBC1(NBC1)=N
          write(*,145)N,V
          VBC1(NBC1)=V
30    CONTINUE
C.....READ ELEMENT SIDE BOUNDARY CONDITION DATA
40    IF(NBC2.EQ.0) GO TO 60
      NBC2=0
      READ(*,*)NREC
      write(*,155)NREC
      DO 50 J=1,NREC
        READ(*,*) BCTYPE,N1,NUM,INC,NS,P,V
	IF (BCTYPE.EQ.1) THEN
	   V=1.E10*P
	   P=1.E10
	ELSE IF (BCTYPE.EQ.2) THEN
	   V=P
	   P=0.
	END IF
        DO 50 I=1,NUM
          NBC2=NBC2+1
          N=N1+(I-1)*INC
          NELBC(NBC2)=N
          NSIDE(NBC2)=NS
          VBC2(1,NBC2)=P
          VBC2(2,NBC2)=V
          write(*,165)N,NS,P,V
50    CONTINUE
60    RETURN
C.....PRINT BOUNDARY CONDITION DATA
100   FORMAT(I5)
101   FORMAT(I5,F10.0)
102   FORMAT(3I5,F10.0)
103   FORMAT(4I5,2F10.0)
115   FORMAT(//42HPOINT LOAD NO.     NODE NO.     LOAD VALUE/42('-')/)
125   FORMAT(I7,I15,F17.2)
135   FORMAT(// 
     &  'NO. OF RECORDS WITH ESSENTIAL BOUNDARY COND. DATA=',
     &       I3//'NODE NO.    VALUE OF U'/22('-')/)
145   FORMAT(I5,F15.2)
155   FORMAT(//'NO. OF RECORDS WITH ELEMENT SIDE BOUNDARY COND. DATA=',
     &       I3//'ELEMENT NO.   SIDE NO.   VALUE OF A   VALUE OF B'
     &       /52('-')/)
165   FORMAT(I6,I13,F15.2,F17.2)
      END
      SUBROUTINE RMAT
C**************************************************************
C
C     MATERIAL PROPERTY DEFINITION; ROUTINE-RMAT
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      CHARACTER*8 TITLE
      COMMON/CCON/TITLE(10),NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      COMMON/CMATL/PROP(3,5)

      DO 1 I=1,NMAT
        READ(*,*) (PROP(J,I),J=1,3)
        write(*,30) i,(PROP(J,I),J=1,3)
 1    CONTINUE
      RETURN
10    FORMAT(20A4)
20    FORMAT(//'MATERIAL INDEX     K(X)   B(X)   F(X)'/36('-')/)
30    FORMAT(5X,i5,5x,3F7.1)
40    FORMAT(' NO.',I3,5X,
     .   'MATERIAL PROPERTIES DEFINED BY USER CODING')
      END
C
C
C
      SUBROUTINE SETINT
C**************************************************************
C
C     SET INTEGRATING CONSTANTS; ROUTINE-SETINT
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      COMMON/CINT/XIQ(2,9),XIT(2,4),WQ(9),WT(4)
C.....QUADRILATERAL ELEMENTS
C.....GAUSS INTEGRATION ORDER 3*3
      XIQ(1,1)=-SQRT(3./5.)
      XIQ(2,1)=XIQ(1,1)
      XIQ(1,2)=0.
      XIQ(2,2)=XIQ(1,1)
      XIQ(1,3)=-XIQ(1,1)
      XIQ(2,3)=XIQ(1,1)
      XIQ(1,4)=XIQ(1,1)
      XIQ(2,4)=0.
      XIQ(1,5)=0.
      XIQ(2,5)=0.
      XIQ(1,6)=-XIQ(1,1)
      XIQ(2,6)=0.
      XIQ(1,7)=XIQ(1,1)
      XIQ(2,7)=-XIQ(1,1)
      XIQ(1,8)=0.
      XIQ(2,8)=-XIQ(1,1)
      XIQ(1,9)=-XIQ(1,1)
      XIQ(2,9)=-XIQ(1,1)
      WQ(1)=25./81.
      WQ(2)=40./81.
      WQ(3)=WQ(1)
      WQ(4)=WQ(2)
      WQ(5)=64./81.
      WQ(6)=WQ(2)
      WQ(7)=WQ(1)
      WQ(8)=WQ(2)
      WQ(9)=WQ(1)
C.....TRIANGULAR ELEMENTS
C.....GAUSS INTEGRATION ORDER 3
      XIT(1,1)=1./3.
      XIT(2,1)=XIT(1,1)
      XIT(1,2)=2./15.
      XIT(2,2)=11./15.
      XIT(1,3)=XIT(1,2)
      XIT(2,3)=XIT(1,2)
      XIT(1,4)=XIT(2,2)
      XIT(2,4)=XIT(1,2)
      WT(1)=-27./96.
      WT(2)=25./96.
      WT(3)=WT(2)
      WT(4)=WT(2)
      RETURN
      END
C
C
C
      SUBROUTINE PROC
C**************************************************************
C**************************************************************
C
C     PROCESSOR CALLING; ROUTINE-PROC
C
C**************************************************************
C**************************************************************
      implicit double precision (A-H,O-Z)

      CALL FORMKF
      CALL APLYBC
      CALL SOLVE

      RETURN
      END
C
C
C
      SUBROUTINE FORMKF
C**************************************************************
C
C     FORMATION OF K & F; ROUTINE-FORMKF
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      CHARACTER*8 TITLE
      COMMON/CCON/TITLE(10),NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      COMMON/CELEM/MAT(100),NE(100),NODES(9,100)
      COMMON/CNODE/X(2,450),U(450)
      COMMON/CMATRX/GK(450,50),GF(450),IB
      COMMON/CINT/XIQ(2,9),XIT(2,4),WQ(9),WT(4)
      DIMENSION EK(9,9),EF(9)
      DIMENSION XA(2,9)

      IB=50
      DO 10 I=1,NNODE
        GF(I)=0.0
       DO 10 J=1,IB
          GK(I,J)=0.0
10    CONTINUE
      NG=450
      DO 50 NEL=1,NELEM
C??????????????????????????????????????????????????????????
        DO 20 J=1,NE(NEL)
          IN=NODES(J,NEL)
          XA(1,J)=X(1,IN)
          XA(2,J)=X(2,IN)
20      CONTINUE
        IF(NE(NEL).EQ.6)GO TO 30
        CALL ELEM(XA,NE(NEL),EK,EF,9,XIQ,WQ,MAT(NEL))
        GO TO 40
30      CONTINUE
        CALL ELEM(XA,NE(NEL),EK,EF,4,XIT,WT,MAT(NEL))
40      CONTINUE
        CALL ASSMB(EK,EF,NE(NEL),NODES(1,NEL),GK,GF,NG)
50    CONTINUE
C??????????????????????????????????????????????????????????

      RETURN
      END
C
C
C
      SUBROUTINE ELEM(X,N,EK,EF,NL,XI,W,MAT)
C**************************************************************
C
C     CONSTRUCTING ELEMENT MATRICES; ROUTINE-ELEM
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      COMMON/CMATL/PROP(3,5)
      DIMENSION X(2,N),EK(N,N),EF(N),XI(2,NL),W(NL)
      DIMENSION DPSIX(9),DPSIY(9),DXDS(2,2),DSDX(2,2)
      DIMENSION PSI(9),DPSI(2,9)

C??????????????????????????????????????????????????????????
C.....INITIALIZE ELEMENT ARRAYS
      DO I=1,N
        EF(I)=0.0
        DO J=1,N
          EK(I,J)=0.0
        end do
      end do
c
C.....BEGIN INTEGRATION POINT LOOP
      DO 50 L=1,NL
        CALL SHAPE(XI(1,L),N,PSI,DPSI)
        CALL GETMAT(XK,XB,XF,MAT,X,N,PSI,XXX,YYY)
C.....  CALCULATE DXDS...EQUATION (5.3.6)
        DO 20 I=1,2
          DO 20 J=1,2
            DXDS(I,J)=0.0
            DO 20 K=1,N
            DXDS(I,J)=DXDS(I,J)+DPSI(J,K)*X(I,K)
20      CONTINUE

C.....  CALCULATE DSDX...EQUATION (5.2.5)

        DETJ=DXDS(1,1)*DXDS(2,2)-DXDS(1,2)*DXDS(2,1)
        IF(DETJ.LE.0.0) GO TO 99
        DSDX(1,1)=DXDS(2,2)/DETJ
        DSDX(2,2)=DXDS(1,1)/DETJ
        DSDX(1,2)=-DXDS(1,2)/DETJ
        DSDX(2,1)=-DXDS(2,1)/DETJ

C.....  CALCULATE D(PSI)/DX...EQUATION (5.3.5)
        DO 30 I=1,N
          DPSIX(I)=DPSI(1,I)*DSDX(1,1)+DPSI(2,I)*DSDX(2,1)
          DPSIY(I)=DPSI(1,I)*DSDX(1,2)+DPSI(2,I)*DSDX(2,2)
30      CONTINUE
C.....  ACCUMULATE INTEGRATION POINT VALUE OF INTEGRALS
        FAC=DETJ*W(L)
        DO 40 I=1,N
          EF(I)=EF(I)+XF*PSI(I)*FAC
          DO 40 J=I,N
            EK(I,J)=EK(I,J)+FAC*(XK*(DPSIX(I)*DPSIX(J)+DPSIY(I)
     &              *DPSIY(J))+XB*PSI(I)*PSI(J))
40      CONTINUE
50    CONTINUE
C.....CALCULATE LOWER SYMMETRIC PART OF EK
      DO 60 I=1,N
        DO 60 J=1,I
          EK(I,J)=EK(J,I)
60    CONTINUE
C??????????????????????????????????????????????????????????
      RETURN
99    continue
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ELEM - Fatal error!'
      write ( *, '(a)' ) '  Determinant DETJ <= 0'
      write ( *, '(a,g14.6)' ) '  DETJ = ', detj
      do j = 1, n
        write ( *, * ) x(1:2,j)
      end do

110   FORMAT(2F12.3,2X,3F9.3)
      STOP
      END
C
C
C
      SUBROUTINE SHAPE(XI,N,PSI,DPSI)
C**************************************************************
C
C     SHAPE FUNCTIONS; ROUTINE-SHAPE
C
C*************************************************************
      implicit double precision (A-H,O-Z)
      DIMENSION PSI(9),DPSI(2,9)
      DIMENSION XI(2)
      IF(N.LT.6.OR.N.GT.9)GO TO 99
      IF(N.EQ.6)GO TO 10
C.....SHAPE FUNCTIONS FOR
C.....QUADRILATERAL 9-NODE ELEMENTS
      PSI(1)=(1./4.)*(XI(1)**2-XI(1))*(XI(2)**2-XI(2))
      PSI(2)=(1./4.)*(XI(1)**2+XI(1))*(XI(2)**2-XI(2))
      PSI(3)=(1./4.)*(XI(1)**2+XI(1))*(XI(2)**2+XI(2))
      PSI(4)=(1./4.)*(XI(1)**2-XI(1))*(XI(2)**2+XI(2))
      PSI(5)=(1./2.)*(1.-XI(1)**2)*(XI(2)**2-XI(2))
      PSI(6)=(1./2.)*(XI(1)**2+XI(1))*(1.-XI(2)**2)
      PSI(7)=(1./2.)*(1.-XI(1)**2)*(XI(2)**2+XI(2))
      PSI(8)=(1./2.)*(XI(1)**2-XI(1))*(1.-XI(2)**2)
      PSI(9)=(1.-XI(1)**2)*(1.-XI(2)**2)
      DPSI(1,1)=(0.5*XI(1)-0.25)*(XI(2)**2-XI(2))
      DPSI(2,1)=(0.5*XI(2)-0.25)*(XI(1)**2-XI(1))
      DPSI(1,2)=(0.5*XI(1)+0.25)*(XI(2)**2-XI(2))
      DPSI(2,2)=(0.5*XI(2)-0.25)*(XI(1)**2+XI(1))
      DPSI(1,3)=(0.5*XI(1)+0.25)*(XI(2)**2+XI(2))
      DPSI(2,3)=(0.5*XI(2)+0.25)*(XI(1)**2+XI(1))
      DPSI(1,4)=(0.5*XI(1)-0.25)*(XI(2)**2+XI(2))
      DPSI(2,4)=(0.5*XI(2)+0.25)*(XI(1)**2-XI(1))
      DPSI(1,5)=-XI(1)*(XI(2)**2-XI(2))
      DPSI(2,5)=(XI(2)-0.5)*(1.-XI(1)**2)
      DPSI(1,6)=(XI(1)+0.5)*(1.-XI(2)**2)
      DPSI(2,6)=-XI(2)*(XI(1)**2+XI(1))
      DPSI(1,7)=-XI(1)*(XI(2)**2+XI(2))
      DPSI(2,7)=(XI(2)+0.5)*(1.-XI(1)**2)
      DPSI(1,8)=(XI(1)-0.5)*(1.-XI(2)**2)
      DPSI(2,8)=-XI(2)*(XI(1)**2-XI(1))
      DPSI(1,9)=-2.*XI(1)*(1.-XI(2)**2)
      DPSI(2,9)=-2.*XI(2)*(1.-XI(1)**2)
C.....SHAPE FUNCTIONS FOR
C.....TRIANGULAR 6-NODES ELEMENTS
      GO TO 20
10    CONTINUE
      PSI(1)=2.*(1.-XI(1)-XI(2))*(0.5-XI(1)-XI(2))
      PSI(2)=2.*XI(1)*(XI(1)-0.5)
      PSI(3)=2.*XI(2)*(XI(2)-0.5)
      PSI(4)=4.*(1.-XI(1)-XI(2))*XI(1)
      PSI(5)=4.*XI(1)*XI(2)
      PSI(6)=4.*XI(2)*(1.-XI(1)-XI(2))
      DPSI(1,1)=-3.+4*(XI(1)+XI(2))
      DPSI(2,1)=DPSI(1,1)
      DPSI(1,2)=4.*XI(1)-1.
      DPSI(2,2)=0.
      DPSI(1,3)=0.
      DPSI(2,3)=4.*XI(2)-1.
      DPSI(1,4)=4.-8.*XI(1)-4.*XI(2)
      DPSI(2,4)=-4.*XI(1)
      DPSI(1,5)=4.*XI(2)
      DPSI(2,5)=4.*XI(1)
      DPSI(1,6)=-4.*XI(2)
      DPSI(2,6)=4.-4.*XI(1)-8.*XI(2)
20    CONTINUE
      RETURN
99    write(*,100)N
100   FORMAT(' ERROR IN CALL TO SHAPE,N='I3)
      STOP
      END
C
C
C
      SUBROUTINE GETMAT(XK,XB,XF,MAT,X,N,PSI,XXX,YYY)
C**************************************************************
C
C     MATERIAL PROPERTY; ROUTINE-GETMAT
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      COMMON/CMATL/PROP(3,5)
      DIMENSION XY(2),PSI(9),X(2,9)
 
      XK=PROP(1,MAT)
      XB=PROP(2,MAT)
      XF=PROP(3,MAT)

      RETURN
      END
C
C
C
      SUBROUTINE ASSMB(EK,EF,N,NODE,GK,GF,NNODE)
C**************************************************************
C
C     ASSEMBLY; ROUTINE-ASSMB
C
C**************************************************************
      implicit double precision (A-H,O-Z)
C??????????????????????????????????????????????????????????
      DIMENSION EK(N,1),EF(1),NODE(1),GK(NNODE,1),GF(1)
      DO 10 I=1,N
        IG=NODE(I)
C.....  ASSEMBLE GLOBAL VECTOR GF
        GF(IG)=GF(IG)+EF(I)
        DO 10 J=1,N
          JG=NODE(J)-IG+1
          IF(JG.LE.0) GO TO 10
C.....    ASSEMBLE GLOBAL STIFFNESS MATRIX GK
          GK(IG,JG)=GK(IG,JG)+EK(I,J)
10    CONTINUE
C??????????????????????????????????????????????????????????
      RETURN
      END
C
C
C
      SUBROUTINE APLYBC
C**************************************************************
C
C     BOUNDARY CONDITION; ROUTINE-APLYBC
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      CHARACTER*8 TITLE
      COMMON/CCON/TITLE(10),NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      COMMON/CBC/NODBC1(70),VBC1(70),NELBC(40),NSIDE(40),VBC2(2,40)
     .  ,NPT(10),VPT(10)
      COMMON/CELEM/MAT(100),NE(100),NODES(9,100)
      COMMON/CMATRX/GK(450,50),GF(450),IB
      COMMON/CNODE/X(2,450),U(450)
      DIMENSION NOD(3), XBC(2,3),PE(3,3),GAME(3)
C.....APPLY POINT LOADS
      IF (NPOINT.EQ.0) GO TO 20
      DO 10 I=1,NPOINT
        N=NPT(I)
        GF(N)=GF(N)+VPT(I)
10    CONTINUE
C.....APPLY ESSENTIAL BOUNDARY CONDITIONS
20    IF (NBC1.EQ.0) GO TO 40
      BIG=1.E30
      DO 30 I=1,NBC1
        N=NODBC1(I)
        GK(N,1)=BIG
        GF(N)=BIG*VBC1(I)
30    CONTINUE
C.....APPLY NATURAL BOUNDARY CONDITIONS
40    IF (NBC2.EQ.0) GO TO 70
      DO 60 I=1,NBC2
C.....  PICK OUT NODES ON SIDE OF ELEMENT
        NEL=NELBC(I)
        NS=NSIDE(I)
        NC=4
        IF(NE(NEL).EQ.6) NC=3
        NOD(1)=NS
        NOD(2)=NS+NC
        NOD(3)=NS+1
        IF(NS.EQ.NC) NOD(3)=1
C.....  PICK OUT NODAL COORDINATES
        DO 50 J=1,3
          NJ=NOD(J)
          NOD(J)=NODES(NJ,NEL)
          NJ=NOD(J)
          XBC(1,J)=X(1,NJ)
          XBC(2,J)=X(2,NJ)
50      CONTINUE
C.....  CALL BCINT TO CALCULATE BOUNDARY INTEGRALS PE AND GAME
        CALL BCINT(VBC2(1,I),VBC2(2,I),XBC,PE,GAME)
C.....  CALL ASSEMB TO ADD PE TO GK AND GAME TO GF
        CALL ASSMB(PE,GAME,3,NOD,GK,GF,450)
60    CONTINUE
70    RETURN
      END
C
C
C
      SUBROUTINE BCINT(P,V,XBC,PE,GAME)
C**************************************************************
C
C     CALCULATE BAUNDARY INTEGRALS; ROUTINE-BCINT
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      DIMENSION GAME(3),PE(3,3),XBC(2,3),XI(2,2),W(2),PSI(9),
     .          DPSI(2,9)
      COMMON/CINT/XIQ(2,9),XIT(2,4),WQ(9),WT(4)
      DO 10 II=1,3
        GAME(II)=0.
        DO 10 JJ=1,3
          PE(II,JJ)=0.
10    CONTINUE
C.....USING GAUSS ORDER 2
C??????????????????????????????????????????????????????????
      XI(1,1)=-1./SQRT(3.)
      XI(2,1)=-1.
      XI(1,2)=-XI(1,1)
      XI(2,2)=-1.
      W(1)=1.
      W(2)=1.
      DO 40 I=1,2
        CALL SHAPE(XI(1,I),9,PSI,DPSI)
        PSI(3)=PSI(2)
        DPSI(I,3)=DPSI(I,2)
        PSI(2)=PSI(5)
        DPSI(I,2)=DPSI(I,5)
        DETJ1=0.
        DETJ2=0.
        DO 20 N=1,3
          DETJ1=DETJ1+XBC(1,N)*DPSI(I,N)
          DETJ2=DETJ2+XBC(2,N)*DPSI(I,N)
20      CONTINUE
        DETJ=SQRT(DETJ1**2+DETJ2**2)
        FAC=DETJ*W(I)
        DO 30 IN=1,3
          GAME(IN)=GAME(IN)+V*PSI(IN)*FAC
          DO 30 JN=1,3
            PE(IN,JN)=PE(IN,JN)+P*PSI(IN)*PSI(JN)*FAC
30      CONTINUE
40    CONTINUE
C??????????????????????????????????????????????????????????
      RETURN
      END

C
C
C
      SUBROUTINE SOLVE
C**************************************************************
C
C     EQUATION SOLVING; ROUTINE-SOLVE
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      CHARACTER*8 TITLE
      COMMON/CCON/TITLE(10),NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      COMMON/CMATRX/GK(450,50),GF(450),IB
      COMMON/CNODE/X(2,450),U(450)
      CALL TRIB(GK,NNODE,IB,450,50)
      CALL RHSUB(GK,U,GF,NNODE,IB,450,50)
      RETURN
      END
C
C
C
      SUBROUTINE TRIB(A,N,IB,L1,L2)
C**************************************************************
C
C     TRIANGULARIZATION; ROUTINE-TRIB
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      DIMENSION A(L1,L2)
C.....THIS SUBROUTINE TRIANGULARIZES A BANDED AND SYMMETRIC
C.....MATRIX A
C.....ONLY THE UPPER HALF-BAND OF THE MATRIX IS STORED
C.....STORAGE IS IN THE FORM OF A RECTANGULAR ARRAY L1*L2
C.....THE HALF-BAND WIDTH IS IB
C.....THE NUMBER OF EQUATIONS IS N
      DO 120 I=2,N
        M1=MIN0(IB-1,N-I+1)
        DO 120 J=1,M1
          SUM=0.0
          K1=MIN0(I-1,IB-J)
          DO 100 K=1,K1
            SUM=SUM+A(I-K,K+1)*A(I-K,J+K)/A(I-K,1)
100       CONTINUE
          A(I,J)=A(I,J)-SUM
120   CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE RHSUB(A,X,B,N,IB,L1,L2)
C**************************************************************
C
C     FORWARD & BACKWARD SUBSTITUTION; ROUTINE RHSUB
C
C**************************************************************
C.....FOR THE LINEAR SYSTEM A*X=B WITH THE MATRIX A
C.....TRIANGULARIZED BY ROUTINE TRIB
C.....THIS ROUTINE PERFORMS THE FORWARD SUBSTITUTION
C.....INTO B AND BACK SUBSTITUTION INTO X
C.....THE HALF-BAND WIDTH OF A IS IB
C.....THE NUMBER OF EQUATIONS IS N
      implicit double precision (A-H,O-Z)
      DIMENSION A(L1,L2),B(1),X(1)
      NP1=N+1
      DO 110 I=2,N
        SUM=0.
        K1=MIN0(IB-1,I-1)
        DO 100 K=1,K1
          SUM=SUM+A(I-K,K+1)/A(I-K,1)*B(I-K)
100     CONTINUE
        B(I)=B(I)-SUM
110   CONTINUE
C.....BEGIN  BACK SUBSTITUTION
      X(N)=B(N)/A(N,1)
      DO 130 K=2,N
        I=NP1-K
        J1=I+1
        J2=MIN0(N,I+IB-1)
        SUM=0.0
        DO 120 J=J1,J2
          MM=J-J1+2
          SUM=SUM+X(J)*A(I,MM)
120     CONTINUE
        X(I)=(B(I)-SUM)/A(I,1)
130   CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE POST
C**************************************************************
C**************************************************************
C
C     POSTPROCESSING CONTROL; ROUTINE-POST
C
C**************************************************************
C**************************************************************
      implicit double precision (A-H,O-Z)
      CHARACTER*8 TITLE
      COMMON/CCON/TITLE(10),NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      COMMON/CELEM/MAT(100),NE(100),NODES(9,100)
      COMMON/CNODE/X(2,450),U(450)
      DIMENSION XA(2,9),UA(9)
      DIMENSION Q(2,4),T(2,3)
      CALL PRINTU
      RETURN
      A=-1./SQRT(3.)
      Q(1,1)=A
      Q(2,1)=A
      Q(1,2)=-A
      Q(2,2)=A
      Q(1,3)=A
      Q(2,3)=-A
      Q(1,4)=-A
      Q(2,4)=-A
      T(1,1)=0.0
      T(2,1)=0.5
      T(1,2)=0.5
      T(2,2)=0.0
      T(1,3)=0.5
      T(2,3)=0.5
      write(*,100)
      DO 30 NEL=1,NELEM
        write(*,105)NEL
        DO 10 J=1,NE(NEL)
          IN=NODES(J,NEL)
          UA(J)=U(IN)
          XA(1,J)=X(1,IN)
          XA(2,J)=X(2,IN)
10      CONTINUE
        IF(NE(NEL).EQ.9) THEN
        CALL FLUX(XA,NE(NEL),4,Q,MAT(NEL),UA)
         ELSE IF(NE(NEL).EQ.6) THEN
        CALL FLUX(XA,NE(NEL),3,T,MAT(NEL),UA)
	END IF
30    CONTINUE
103   FORMAT(20A4)
100   FORMAT(/T18,'X-COORDINATE    Y-COORDINATE    STRESS SIGXH'
     &       ,'    STRESS SIGYH'/T18,60('-')/)
105   FORMAT(/'ELEMENT NO. =',I3/
     &,8X,'STRESS SIGXH',8X,'STRESS SIGYH')
      RETURN
      END
C
C
C
      SUBROUTINE PRINTU
C**************************************************************
C
C     OUTPUT; ROUTINE-PRINTU
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      CHARACTER*8 TITLE
      COMMON/CCON/TITLE(10),NNODE,NELEM,NMAT,NPOINT,NBC1,NBC2
      COMMON/CELEM/MAT(100),NE(100),NODES(9,100)
      COMMON/CNODE/X(2,450),U(450)
      write(*,90)
      DO 20 I=1,NNODE
        write(*,100)I,X(1,I),X(2,I),U(I)
20    CONTINUE
      RETURN
80    FORMAT(1H1,20A4/)
90    FORMAT(/'NODE NUMBER',5X,'X COORDINATE',5X,'Y COORDINATE',5X,
     &       'DISPLACEMENT UH'/65('-')/)
100   FORMAT(I8,10X,F10.3,7X,F10.3,5X,F10.5)
      END
C
C
C
      SUBROUTINE FLUX(X,N,NL,XI,MAT,UU)
C**************************************************************
C
C     CALCULATES FLUX; ROUTINE-FLUX
C
C**************************************************************
      implicit double precision (A-H,O-Z)
      DIMENSION X(2,N),XI(2,NL),UU(N)
      DIMENSION DXDS(2,2),DSDX(2,2)
      DIMENSION PSI(9),DPSI(2,9)
      DO 50 L=1,NL
        CALL SHAPE(XI(1,L),N,PSI,DPSI)
	DO 20 I=1,2
         DO 20 J=1,2
            DXDS(I,J)=0.
            DO 20 K=1,N
              DXDS(I,J)=DXDS(I,J)+DPSI(J,K)*X(I,K)
20      CONTINUE
        XX=0.
	YY=0.
	DO 25 K=1,N
	  XX=XX+PSI(K)*X(1,K)
25	  YY=YY+PSI(K)*X(2,K)
        CALL GETMAT(XK,XB,XF,MAT,X,N,PSI,XX,YY)
        DETJ=DXDS(1,1)*DXDS(2,2)-DXDS(1,2)*DXDS(2,1)
        IF(DETJ.LE.0.)GO TO 99
        DSDX(1,1)=DXDS(2,2)/DETJ
        DSDX(2,2)=DXDS(1,1)/DETJ
        DSDX(1,2)=-DXDS(1,2)/DETJ
        DSDX(2,1)=-DXDS(2,1)/DETJ
C.....  CALCULATE DUH/DX AND DUH/DY
        DUDX=0.
        DUDY=0.
        DO 30 I=1,N
          DUDX=DUDX+(DPSI(1,I)*DSDX(1,1)+DPSI(2,I)*DSDX(2,1))*UU(I)
          DUDY=DUDY+(DPSI(1,I)*DSDX(1,2)+DPSI(2,I)*DSDX(2,2))*UU(I)
30      CONTINUE
C.....  CALCULATE THE STRESS SIGMA-H
        SIGXH=-XK*DUDX
        SIGYH=-XK*DUDY
        write(*,100)XX,YY,SIGXH,SIGYH
50    CONTINUE
      RETURN
99    write(*,110)DETJ,X
100   FORMAT(T14,4F16.3)
110   FORMAT(13H BAD JACOBIAN,E10.3/9E10.3/9E10.3)
      STOP
      END
C
C
C
      FUNCTION FXK(XXX,YYY)
      implicit double precision (A-H,O-Z)
      FXK=50.+5.*XXX
      RETURN
      END
C
      FUNCTION FXB(XXX,YYY)
      implicit double precision (A-H,O-Z)
      FXB=0.
      RETURN
      END
C
      FUNCTION FXF(XXX,YYY)
      implicit double precision (A-H,O-Z)
      FXF=0.
      RETURN
      END
