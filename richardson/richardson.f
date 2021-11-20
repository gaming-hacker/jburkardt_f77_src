      PROGRAM RICHIE

c*********************************************************************72
C
C     ******************************************************************
C     *                                                                *
C     *        FINE-MESH LIMITED AREA PRIMITIVE EQUATION MODEL.        *
C     *                                                                *
C     *                                                                *
C     *        THIS MODEL IS A REALIZATION OF THE NUMERICAL            *
C     *            SCHEME DESCRIBED IN RICHARDSON'S BOOK               *
C     *                                                                *
C     *           Weather Prediction by Numerical Process              *
C     *                                                                *
C     ******************************************************************
C
C     **************************************************************
C                                                                 
C         Coding started on 29th Jan, 1992, IMS, Dublin. 
C
C     **************************************************************
C                                                                
C        Units here are SI. LFR's Momenta are ten times bigger. 
C        LFR's pressures are in microbars or deci-Pascals.     
C                                                             
C        Trick: prognostic variables on E-grid are interpolated 
C               to a finer A-grid. The forecast step is done
C               on the A-grid (with due care in choosing
C               finite differences). Then, the interpolation
C               is repeated at the beginning of each step.
C               The result is to double the computation, but
C               the program is MUCH easier to code on the A-grid.
C                                                             
C     **************************************************************
C
c          Parameters to determine size of grid
           PARAMETER
     *                ( IM= 13, JM=07, LM=5,
     *                  IMM1=IM-1, IMM2=IM-2, 
     *                  JMM1=JM-1, JMM2=JM-2,
     *                  LMM1=LM-1, LMM2=LM-2, 
     *                  KB=IM+2*JM-3          )  
C
c=================================================================
c
c      2-D Work fields common to all modules.
       COMMON /WORK/ WORK1(IM,JM), WORK2(IM,JM), WORK3(IM,JM)
c
c=================================================================
C
          DIMENSION
     *    COSPHI(JM,2), FCOR(JM,2), H1(JM,2), RH1(JM,2)

C         Divergence and Vorticity
          DIMENSION DIV (IM,JM,LM)
CCC       DIMENSION VORT(IM,JM,LM)
C
c=================================================================
c
c       space for the basic INPUT variables 
        REAL PS(IM,JM), Z(IM,JM,LMM1)
        REAL U(IM,JM,LM),V(IM,JM,LM), DDDFF(IM,JM,LM)
        REAL OROG(IM,2*JM)
        COMMON / WINPUT / PS, Z, U, V, DDDFF, OROG
c
c       space for the basic PROGNOSTIC variables
        real P(IM,JM,LM),MU(IM,JM,LM),MV(IM,JM,LM),TTOP(IM,JM)
        COMMON / WPROG / P, MU, MV, TTOP
c
c    -     -    -    -    -    -    -    -    -    -    -    -
c
c       size of fine A-grid
        PARAMETER (JM2=14,JM2M1=JM2-1,JM2M2=JM2-2,JM2M3=JM2-3)
C
c       space for the basic PROGNOSTIC variables on FINE grid
        real P2(IM,JM2,LM),MU2(IM,JM2,LM),
     X       MV2(IM,JM2,LM),TTOP2(IM,JM2)
        COMMON / WPROG2 / P2, MU2, MV2, TTOP2
C
c       space for various DIAGNOSTIC variables on FINE grid
        real PP2(IM,JM2,LM),U2(IM,JM2,LM),
     X       V2(IM,JM2,LM),W2(IM,JM2,LM),
     X       RR2(IM,JM2,LM),PMID2(IM,JM2,LM),
     X       RHO2(IM,JM2,LM),T2(IM,JM2,LM),
     X       RHO2I(IM,JM2,LM),T2I(IM,JM2,LM)
        COMMON / WDIAG2 / PP2, U2, V2, W2, 
     X       RR2, PMID2, RHO2, T2, RHO2I,T2I
c
c       space for the basic HISTORICAL variables on FINE grid
        real P2OLD(IM,JM2,LM),MU2OLD(IM,JM2,LM),
     X       MV2OLD(IM,JM2,LM),TT2OLD(IM,JM2)
        real P2NEW(IM,JM2,LM),MU2NEW(IM,JM2,LM),
     X       MV2NEW(IM,JM2,LM),TT2NEW(IM,JM2)
        COMMON / WOLDNEW / P2OLD, MU2OLD, MV2OLD, TT2OLD,
     X                     P2NEW, MU2NEW, MV2NEW, TT2NEW
C
c       Space for various TENDENCY variables on FINE grid
        real DP2DT (IM,JM2,LM),DRR2DT(IM,JM2,LM),DTT2DT(IM,JM2),
     X       DMU2DT(IM,JM2,LM),DMV2DT(IM,JM2,LM)
        COMMON /TENDY / DP2DT, DRR2DT, DTT2DT, DMU2DT, DMV2DT 
c
        COMMON / WMISC / BRAKET(IM,JM2,LM), DWDZ(IM,JM2,LM)
c
CCC     COMMON / PGRAD / DPDX(IM,JM2,LM), DPDY(IM,JM2,LM)
c
        REAL COSPH2(JM2), FCOR2(JM2), TAN2(JM2)
c
c       Work fields for plotting (on fine A-grid)
CCC     real ZPLOT(IM,2*JM)
        real PSEA2(IM,JM2),Z2(IM,JM2,LM)
c
c      2-D Work fields on A-grid.
       COMMON /WORKK/ WORKK1(IM,JM2), WORKK2(IM,JM2), 
     +                WORKK3(IM,JM2), WORKK4(IM,JM2)
C
C      Divergence of momentum and velocity.
       REAL  DIV2M(IM,JM2,LM), DIV2V(IM,JM2,LM)
c
c      1-D Work fields for vertical integrals, etc. 
       COMMON /WVERT / VERT1(LM), VERT2(LM), 
     +                 VERT3(LM), VERT4(LM),
     +                 VERT5(LM), VERT6(LM) 
C
c       space for the basic INITIALIZED variables on FINE grid
        real P2INI(IM,JM2,LM),MU2INI(IM,JM2,LM),
     X       MV2INI(IM,JM2,LM),TT2INI(IM,JM2)
        COMMON / WINIT2 / P2INI, MU2INI, MV2INI, TT2INI
C
c=================================================================
C
c       Conventional interface levels (km).
        real ZBAR(LM), PBAR(LM),TBAR(LM)
c
c      Work fields for other vertical quantities (SI units).
       COMMON /VERTIC/ ZLEV(LM), DELZ(LM)
C
        REAL KAPPA , GAMMA
C
        REAL NOISE1, NOISE2, BRUS
        REAL N1MID , N2MID , BRMID
c
        CHARACTER*10 STRING
        INTEGER YEAR, MONTH, DAY, HOUR
c
        LOGICAL OLDDAT, LFRDAT 
        LOGICAL TIMEFILT, MASSFILT, WINDFILT, DIVDAMP
c
        REAL WHOUR(10)
        CHARACTER*6 WNAME(10), WFILE
        CHARACTER*10 WIFILE, OLDFIL
c
        PARAMETER ( NSMAX=1000)
        REAL 
     X       XVAR(0:NSMAX),YVAR(0:NSMAX),
     X       UVAR(0:NSMAX),VVAR(0:NSMAX),DVAR(0:NSMAX),
     X       KVAR(0:NSMAX),AVAR(0:NSMAX),PVAR(0:NSMAX),
     X       MVAR(0:NSMAX),
     X       N1VAR(0:NSMAX),N2VAR(0:NSMAX),BRVAR(0:NSMAX),
     X       N1PVAR(0:NSMAX),N2PVAR(0:NSMAX),BRPVAR(0:NSMAX)
C
C-----------------------------------------------------------------------
c
c       Parameters relating to initialization.
        LOGICAL INIT
        REAL HH(0:500), HH2(0:1000)
        DATA TAUC /6./, IWINDOW  /0/, WPARAM /0./
c
C       Control for geostrophic momentum initial conditions.
        LOGICAL ICGEOS
        DATA ICGEOS /.FALSE./
C
C-----------------------------------------------------------------------
C
        DATA ZBAR / 11.8,   7.2,   4.2 ,   2.0 ,   0.0 /
        DATA PBAR / 200.,  400.,   600.,   800.,  1013./
        DATA TBAR / -55.,  -32.,   -12.,    +2.,   +15./
c
                             DATA
     X   A/6376000./,  OMEGA2/.00014584/, GRAV/9.80/,
     X   PI/3.141592654/,  RGAS/287./, CP/1004./
c
         DATA NDISC /88/, TSTART /0./, TEND /24./
c
         DATA IMID,JMID / 7 , 4 /
         DATA JMID2Z, JMID2V  / 7 , 8 /
c
         DATA YEAR /1910/, MONTH /05/, DAY /20/, HOUR / 07/
c
         DATA DT / 600./
         DATA EpsTime / 0./, EpSpace / 1./
c
CCC      DATA WHOUR / 00 , 0.25, 06, 12, 24, 48, 4*0 /
         DATA WHOUR / 00 ,   01, 06, 12, 24, 48, 4*0 /
         DATA WNAME /'fcst00','fcst01','fcst06',
     X               'fcst12','fcst24','fcst48',
     X              4*' ' /
c
C***********************************************************************
c
c       OPEN CONTROL CARD FILE and READ PARAMETERS.
        OPEN(UNIT=5,FILE='richardson_input.txt',FORM='FORMATTED')
        READ(5,*) string,  YEAR, MONTH, DAY, HOUR
        READ(5,*) string,  OLDDAT , OLDFIL
        READ(5,*) string,  LFRDAT
        READ(5,*) string,  DT 
        READ(5,*) string,  TSTART 
        READ(5,*) string,  TEND 
        READ(5,*) string,  TFORWARD
        READ(5,*) string,  TIMEFILT,EpsTime
        READ(5,*) string,  MASSFILT, WINDFILT, EpSpace,EPSSDT
        READ(5,*) string,  DIVDAMP, CDDAMP 
        READ(5,*) string,  SURFAC
        READ(5,*) string,  INIT
        READ(5,*) string,  TAUC 
        READ(5,*) string,  IWINDOW , WPARAM
        READ(5,*) string,  ICGEOS
C
        print*, ' Date:   ',  YEAR, MONTH, DAY, HOUR
        print*, ' OLDdat  ',  OLDDAT, OLDFIL
        print*, ' LFRdat  ',  LFRDAT
        print*, ' Delta t ',  DT 
        print*, ' tStart  ',  TSTART
        print*, ' tEnd    ',  TEND   
        print*, ' tForward',  TFORWARD   
        print*, ' EpsTime ',  TIMEFILT, EpsTime
        print*, ' EpSpace ',  MASSFILT, WINDFILT, EpSpace,EPSSDT
        print*, ' DivDamp ',  DIVDAMP,  CDDAMP
        print*, ' INIT    ',  INIT
        print*, ' TauC    ',  TAUC 
        print*, ' Window  ',  IWINDOW , WPARAM
        print*, ' ICGEOS  ',  ICGEOS
c
C***********************************************************************
c
C-----------------------------------------------------------------------
C
C       SET THE AREA AND OTHER PARAMETERS.
c
C       Specify the grid for plotting.
        WBLMD = -7.0
        PHISB = +37.8
        DLAMD = 3.00
        DPHID = 1.80
C
        GLM0D = 0.00
        ETH0D = 0.00
        IMCOPY = 13
        JMCOPY = 14
        LMCOPY = 1
C
C***********************************************************************
c
      IP=IM
      JP=JM
      LP=LM
c
      print  2200, IP,JP,LP,DLAMD,DPHID,WBLMD,PHISB
 2200 FORMAT(' IM=',I3,' JM=',I2,' LM=',I2/
     +       ' DLAMD=',F4.2,' DPHID=',F4.2,
     +       ' WBLMD=',F3.0,' PHISB=',F3.0)

      print  2240, NDISC,TSTART,TEND
 2240 FORMAT(' NDISC=',I2,' TSTART=',F4.0,' TEND=',F4.0)
c
C  RUN CONTROL CONSTANTS.
C
      NTSPH=3600./ABS(DT)

      NSTART=ABS(TSTART)*NTSPH   +.5
      NSTEPS=ABS(TEND  )*NTSPH   +.5

      IF (ABS(DT).gt.3600.) THEN
         XNTSPH=3600./ABS(DT)
         NSTEPS=NINT(ABS(TEND)*XNTSPH)
      ENDIF

      print  2250, DT, NSTART, NSTEPS 
 2250 FORMAT(' DELTAT=',F8.0,'   NSTART=',I6,'   NSTEPS=',I6)

      IF ( NSTEPS.GT.NSMAX ) then
        STOP ' Increase NSMAX '
      end if
C
C  DERIVED SPACE-TIME GRID AND OTHER CONSTANTS.
C
      DRAD=PI/180.
      DLAM=DLAMD*DRAD
      DPHI=DPHID*DRAD

      DO 102 J=1,JM
      DO 102 K=1,2
        PHI=(PHISB+(2*J+K-3)*DPHID)*DRAD
        COSPHI(J,K)=COS(PHI)
        H1 (J,K)=A*COSPHI(J,K)
        RH1(J,K)=1./H1(J,K)
        FCOR(J,K)=OMEGA2*SIN(PHI)
 102  CONTINUE

      DO 602 J=1,JM2
        PHI=(PHISB+(J-1)*DPHID)*DRAD
        COSPH2(J)=COS(PHI)
        FCOR2(J)=OMEGA2*SIN(PHI)
        TAN2(J)=SIN(PHI)/COS(PHI)
 602  CONTINUE

        DO L=1,LM
          ZLEV(L) = ZBAR(L) * 1000.
        end do
        DO L=2,LM
          DELZ(L) = ZLEV(L-1) - ZLEV(L)
        end do

        KAPPA = RGAS/CP
        GAMMA = 1/(1-KAPPA)
c       print*, ' KAPPA, GAMMA ', KAPPA, GAMMA
c
C=======================================================================
c
C     Calculate the Filter Weights for DFI.
      IF ( INIT ) THEN 
C
        IF      (IWINDOW.eq.1) THEN
          CALL HFILT1 (NSTEPS,DT,TAUC,IWINDOW,WPARAM,HH)
          print*,' HFILT1: ', (HH(NNN),NNN=0,NSTEPS)
        ELSE IF (IWINDOW.eq.-1) THEN
          NCHOICE = 1
          TAUS = TAUC
          NSH = NSTEPS/2
          CALL  DOLPH(ABS(DT), NCHOICE, TauS, ripple, NSTEPS, HH2)
          DO NNN=0,NSTEPS
            HH(NNN) = HH2(NNN+NSTEPS)
          end do
          print*,' DOLPH : ', (HH(NNN),NNN=0,NSTEPS)
        ELSE 
          STOP ' IWINDOW not recognized '
        ENDIF
C
      ENDIF
c
C=======================================================================
C
C       READ IN THE INITIAL DATA.
CCC     OPEN(UNIT=NDISC,FILE='RICHIE.DAT',FORM='FORMATTED')
CCC     print*, ' READ in the INITIAL fields '
CCC     READ(NDISC,777) P,MU,MV,TTOP,OROG
c 777   format( 5E20.8 )
C
C***********************************************************************
c
c       get the initial fields
C
        IF (OLDDAT ) THEN
c
C  READ IN THE INITIAL DATA FOR RE_RUNS.
c
            OPEN(UNIT=60,FILE=OLDFIL,FORM='UNFORMATTED')
            print*, ' Read in the INITIAL fields (B1)'
            READ (60) PSEA2,Z2,U2,V2,TTOP2,OROG
            print*, ' Read in the INITIAL fields (B2)'
            READ (60) P2,MU2,MV2
            CLOSE(UNIT=60)
            GO TO 1000

        ELSE

            print*,' @ @    Entry to GETDAT  @ @  '
            CALL GETDAT
            print*,' @@@@  Back from GETDAT  @@@@ '
c
c  make alterations in data to agree with
c  Richardson's values (Table, LFR, p 185).
c
            CALL TABLOT(P,MU,MV,TTOP,OROG,IM,JM,LM,'   BEFOR   ')
            IF ( LFRDAT ) THEN
               CALL RICDAT
               CALL TABLOT(P,MU,MV,TTOP,OROG,IM,JM,LM,'   AFTER   ')
            ENDIF

        ENDIF
c
C***********************************************************************
c
C-----------   COMPUTATION OF DIVERGENCE on E-GRID   -------------------
C-----------   (Old code, for checking purposes only)-------------------
c
      DO 106 J=1,JM
      DO 106 I=1,IM
         WORK1(I,J)=0.
 106  CONTINUE
 
      DO 402 L=1,LM

      ncount = 0
      sumdiv = 0.
      absdiv = 0.
      DO 401 I=3,IMM2
      MI2=MOD(I,2)
      KV=1+MI2
      KH=2-MI2
      JF=JMM2+MI2
      DO 401 J=2,JF
      JM1=J-MI2
      JP1=JM1+1
      DIV(I,J,L)= (MU(I+1,J,L)-MU(I-1,J,L))/(H1(J,KH)*2*DLAM)
     +           +( MV(I,JP1,L)*COSPHI(JP1,KV)
     +             -MV(I,JM1,L)*COSPHI(JM1,KV) )/(H1(J,KH)*2*DPHI)
            ncount = ncount + 1
            sumdiv = sumdiv + DIV(I,J,L)
            absdiv = absdiv + ABS(DIV(I,J,L))
  401 CONTINUE
      sumdiv = sumdiv / ncount
      absdiv = absdiv / ncount
c
        DMID = DIV(IMID,JMID,L)
        print9441, L, DMID, sumdiv, absdiv, ncount
 9441   FORMAT(' Div(7,4), lev:',I2,1PE12.3,
     +         ' sumdiv,absdiv :', 1P2E12.3,    i3)
c
  402 CONTINUE
      DSAVE = DIV(IMID,JMID,4)
      print*, ' DSAVE: ', DSAVE
C
C---------VERTICAL INTEGRATION TO GET SURF. PRESS. TENDENCY---------
c
      DO 202 L=1,LM
      DO 202 I=3,IMM2
      JF=JMM2+MOD(I,2)
      DO 202 J=2,JF
      WORK1(I,J)=WORK1(I,J)-DIV(I,J,L)*GRAV
 202  CONTINUE
c
      DPSDT = WORK1(IMID,JMID)
      print9442, DPSDT
 9442 FORMAT(' Pressure Tendency (7,4)', 1PE12.3,' Pa/s')
      DPSDT2 = DPSDT*6*3600/100
      print9443, DPSDT2
 9443 FORMAT(' Pressure Tendency (7,4)', F12.1,' mb/6h')
c
c  Interpolate PROGNOSTIC Variables from E-grid to A-grid.
c
        DO L=1,LM
          CALL FILLZ(P (1,1,L),P2 (1,1,L),IM,JM)
          CALL FILLV(MU(1,1,L),MU2(1,1,L),IM,JM)
          CALL FILLV(MV(1,1,L),MV2(1,1,L),IM,JM)
        end do
        CALL FILLZ(TTOP,TTOP2,IM,JM)
c
c  Define geostrophic initial momenta if desired.
c
      IF (ICGEOS) THEN

      DO I=1,IM
      DO J=1,JM2 
         PMID2(I,J,1) = P2(I,J,1) * EXP(-1.) 
         DO L=2,LMM1
           PMID2(I,J,L)  = ( P2(I,J,L-1) + P2(I,J,L) ) / 2.
         end do
         PMID2(I,J,LM) = ( P2(I,J,L-1) + 100000. ) / 2.
         IF(I.EQ.IMID .and. J.EQ.JMID2Z )
     +   print*, ' PMID0 (7,7) ', (PMID2(I,J,L),l=1,LM)
      end do
      end do

         FCOR0 = FCOR2(JMID2Z)
         DO L=1,LM
         DO I=2,IMM1
         DO J=1,JM2
           DDXP=(PMID2(I+1,J,L)-PMID2(I-1,J,L))/(A*COSPH2(J)*2*DLAM)
           MV2(I,J,L) = (20000/grav) * DDXP/FCOR0
           IF(I.EQ.IMID .and. J.EQ.JMID2V) THEN
              print*, ' MV2GEOS ', MV2(I,J,L)
           ENDIF
         end do
         end do
         end do

         DO L=1,LM
         DO I=1,IM
         DO J=2,JM2M1
           DDYP = ( PMID2(I,J+1,L)-PMID2(I,J-1,L) ) / (A*2*DPHI)
           MU2(I,J,L) = - (20000/grav) * DDYP/FCOR0
           IF(I.EQ.IMID .and. J.EQ.JMID2V) THEN
              print*, ' MU2GEOS ', MU2(I,J,L)
           ENDIF
         end do
         end do
         end do

C        CALL MMTOUV(P2,MU2,MV2, U2,V2,IM,JM2,LM)
      ELSE
         DO L=1,LM
            print*, ' MV2ORIG ', MV2(IMID,JMID2V,L)
         end do
         DO L=1,LM
            print*, ' MU2ORIG ', MU2(IMID,JMIDV,L)
         end do

       ENDIF

 1000   CONTINUE
c
c  Copy the initial values into the HISTORICAL fields. 
c
        DO J=1,JM2
        DO I=1,IM
          DO L=1,LM
            P2OLD (I,J,L) = P2 (I,J,L)
            MU2OLD(I,J,L) = MU2(I,J,L)
            MV2OLD(I,J,L) = MV2(I,J,L)
            P2NEW (I,J,L) = P2 (I,J,L)
            MU2NEW(I,J,L) = MU2(I,J,L)
            MV2NEW(I,J,L) = MV2(I,J,L)
          end do
        TT2OLD(I,J) = TTOP2(I,J)
        TT2NEW(I,J) = TTOP2(I,J)
        end do
        end do

         NSTEP = 0
         print9091, 
     X    ( NSTEP, L, P2NEW (IMID,JMID2Z,L), MU2NEW(IMID,JMID2V,L),
     X                MV2NEW(IMID,JMID2V,L) ,  L=1,LM )
 9091    FORMAT(' NsteP=',I6,'   LeveL=',I1,'   P2/MU2/MV2/ ',
     X                 F12.1,2x,2f9.1) 
         print9092, NSTEP, TT2NEW(IMID,JMID2Z)
 9092    FORMAT(' NsteP=',I6,'   TTOP2/',F12.1) 
C
c     Put the initial values into the INITIALIZED arrays. 
C     (First pass is with DT < 0; second is with DT > 0).
      IF ( INIT ) THEN
         IF ( DT . GT. 0 ) THEN
c
C           READ IN THE semi-INITIALIZED DATA
c
            OPEN(UNIT=55,FILE='fini1',FORM='UNFORMATTED')
            print*, ' Read in the FINI1 fields '
            READ (55) P2INI,MU2INI,MV2INI, TT2INI
            CLOSE(UNIT=55)
            DO J=1,JM2
            DO I=1,IM
             DO L=1,LM
               P2INI (I,J,L) = P2INI (I,J,L)+0.5*hh(nstep)*P2 (I,J,L)
               MU2INI(I,J,L) = MU2INI(I,J,L)+0.5*hh(nstep)*MU2(I,J,L)
               MV2INI(I,J,L) = MV2INI(I,J,L)+0.5*hh(nstep)*MV2(I,J,L)
             end do
             TT2INI(I,J) = TT2INI(I,J)+0.5*hh(nstep)*TTOP2(I,J)
            end do
            end do
         ELSE
            DO J=1,JM2
            DO I=1,IM
               DO L=1,LM
                  P2INI (I,J,L) = 0.5*hh(nstep)*P2 (I,J,L)
                  MU2INI(I,J,L) = 0.5*hh(nstep)*MU2(I,J,L)
                  MV2INI(I,J,L) = 0.5*hh(nstep)*MV2(I,J,L)
               end do
               TT2INI(I,J) = 0.5*hh(nstep)*TTOP2(I,J)
            end do
            end do
         ENDIF
      ENDIF
c
C  Calculate the total Mass or areal pressure integral.
c
        ZAREA = 0.
        ZMASS = 0.
        DO J=1,JM2
        DO I=1,IM
          IF(((I+J)/2*2).EQ.(I+J)) THEN 
            DAREA = 2*(A*COSPH2(J)*DLAM)*(A*DPHI)
            ZAREA = ZAREA + DAREA
            DMASS = P2 (I,J,LM)*DAREA
            ZMASS = ZMASS + DMASS
          ENDIF
        end do
        end do
        print9095, NSTEP, ZAREA, ZMASS, ZMASS/ZAREA 
 9095   FORMAT(' NsteP=',I6,' Area, MASS, pMEAN:',1P2E12.3,-2PF12.1) 
c
C-----------------------------------------------------------------------
c
C      Convert fields to PSEA and Z and U and V for OUTPUT.
       CALL PTOZ(P2,TTOP2,OROG, Z2,PSEA2,IM,JM2,LM)
       CALL MMTOUV(P2,MU2,MV2, U2,V2,IM,JM2,LM)
C
C
C       Write out the Initial Data.
        OPEN(UNIT=50,FILE=WNAME(1),FORM='UNFORMATTED')
        print*, ' Write out the INITIAL fields '
        WRITE(50) PSEA2,Z2,U2,V2,TTOP2,OROG
        WRITE(50) P2,MU2,MV2
        CLOSE(UNIT=50)
C
C***********************************************************************
C
C       Calculate Energy and other diagnostic quantities.
        NTSTEP = 0
C       CALL ENERGY(NTSTEP, FI,U,V,FI0,IM,JM, DX,DY,
C    X                    EK,EA,EP,AK,PK,EM, .TRUE.)
C
  		XVAR(NTSTEP)=NTSTEP
  		YVAR(NTSTEP)= P2NEW (IMID,JMID2Z,LM)
		UVAR(NTSTEP)=USAVE
		VVAR(NTSTEP)=VSAVE
 		DVAR(NTSTEP)=0.
                KVAR(NTSTEP)=EK
                AVAR(NTSTEP)=EA
		PVAR(NTSTEP)=EP
                MVAR(NTSTEP) =  ZMASS/ZAREA
		N1VAR(NTSTEP)=  0.
		N2VAR(NTSTEP)=  0.
                BRVAR(NTSTEP) = 0
		N1PVAR(NTSTEP)=  N1MID
		N2PVAR(NTSTEP)=  N2MID
                BRPVAR(NTSTEP) = BRMID
C
C***********************************************************************
C
C                            MAIN LOOP
c
C-----------------------------------------------------------------------
c
      DO 5000 NSTEP=1,NSTEPS
c
        print*,' >>>>> Start of Main Loop, NSTEP = ', NSTEP
c
c       First step is forward, others are centered.
        IF ( NSTEP.eq.1 ) THEN
          DELTAT = DT
        ELSE
          DELTAT = 2.*DT
        ENDIF
c
        TIME = NSTEP*DT
        HR= TIME/(3600.)
        print*,' Time Step ending at', HR, '   Hours '
c
c       Take a forward step now and then.
        ITF = INT(TFORWARD*3600.)
        ITIME = INT(TIME)
        If ( ITIME/ITF*ITF .EQ. ITIME ) THEN
           print*,' Forward Step at ', HR,'   Hours '
           DELTAT = DT
c          Copy the MIDDLE values into the HISTORICAL fields. 
           DO L=1,LM
             CALL SMEARZ(P2 (1,1,L),P2OLD (1,1,L),IM,JM)
             CALL SMEARV(MU2(1,1,L),MU2OLD(1,1,L),IM,JM)
             CALL SMEARV(MV2(1,1,L),MV2OLD(1,1,L),IM,JM)
           end do
           CALL SMEARZ(TTOP2,TT2OLD,IM,JM)
        ENDIF
C
C************************************************************
C*********************    ( i )  ****************************
C************************************************************
c
C------  CENTRAL PRESSURE (LAYER INTEGRAL OF P) -------------
C------  LAYER INTEGRAL OF DENSITY ( RR ) -------------------
C
      DO 1011 I=1,IM
      DO 1011 J=1,JM2 
         PP2(I,J,1) = ( RGAS*TTOP2(I,J)/GRAV ) * P2(I,J,1)
         DO 1010 L=2,LM
         IF (L.eq.LM) THEN
            DELTAZ = ZLEV(L-1)-OROG(I,J)
         ELSE
            DELTAZ = DELZ(L)
         ENDIF
         PP2(I,J,L) = DELTAZ*(P2(I,J,L)-P2(I,J,L-1))
     +              / ( LOG(P2(I,J,L)) - LOG(P2(I,J,L-1)) )
 1010    CONTINUE
         IF(I.EQ.IMID .and. J.EQ.JMID2Z .and. NSTEP.EQ.1 )
     +   print*, ' PP2(7,7) ', (PP2(I,J,L),l=1,LM)
 1011 CONTINUE
c
C
      DO 1015 L=1,LM
      DO 1014 I=1,IM
      DO 1014 J=1,JM2
        IF ( L.eq.1 ) THEN
           RR2(I,J,L) = ( P2(I,J,1)            ) / GRAV
        ELSE
           RR2(I,J,L) = ( P2(I,J,L)-P2(I,J,L-1) ) / GRAV
        ENDIF
           IF(I.eq.IMID .and. J.eq.JMID2Z .and. NSTEP.EQ.1) THEN
              RMID=RR2(I,J,L)
              print*,' CHECK: RR2: ', L,RMID
           ENDIF
 1014 CONTINUE
 1015 CONTINUE
c
C************************************************************
C*********************    ( ii )  ***************************
C************************************************************
c
C------  CENTRAL VALUE OF PRESSURE ( PMID ) -----------------
C------  CENTRAL VALUE OF DENSITY ( RHO2 ) ------------------
c
      DO 1017 I=3,IMM2
      DO 1017 J=3,JM2M3 
CCC      PMID2(I,J,1) = P2(I,J,1) + ( RGAS*TTOP2(I,J)/GRAV )
         PMID2(I,J,1) = P2(I,J,1) * EXP (-1.) 
         DO 1016 L=2,LM
            IF (L.eq.LM) THEN
               DELTAZ = ZLEV(L-1)-OROG(I,J)
            ELSE
               DELTAZ = DELZ(L)
            ENDIF
            PMID2(I,J,L) = PP2(I,J,L)/DELTAZ
 1016    CONTINUE
         IF(I.EQ.IMID .and. J.EQ.JMID2Z .and. NSTEP.EQ.1 )
     +   print*, ' PMID2(7,7) ', (PMID2(I,J,L),l=1,LM)
 1017 CONTINUE
c
      DO 1019 I=3,IMM2
      DO 1019 J=3,JM2M3 
         DO 1018 L=2,LM
            IF (L.eq.LM) THEN
               DELTAZ = ZLEV(L-1)-OROG(I,J)
            ELSE
               DELTAZ = DELZ(L)
            ENDIF
            RHO = RR2(I,J,L) / DELTAZ
            TEMP = PMID2(I,J,L) / ( RGAS*RHO )
            RHO2(I,J,L) = RHO 
            T2(I,J,L) = TEMP 
 1018    CONTINUE
      IF ( I.EQ.IMID .and. J.EQ.JMID2Z .and. NSTEP.EQ.1 ) THEN
         print*, ' RHO   ', (RHO2(I,J,L),L=2,LM)
         print*, ' TEMP  ', (T2 (I,J,L),L=2,LM)
      ENDIF
 1019 CONTINUE
c
C************************************************************
C*********************    (iii)  ****************************
C************************************************************
c
C--------------COMPUTATION OF DIVERGENCE (of Momentum) -----------------
c
      DO 1004 L=1,LM
c
      ncount = 0
      sumdiv = 0.
      absdiv = 0.
CCC   DO 1002 I=3,IMM2
CCC   DO 1002 J=3,JM2M3
                            DO 1002 I=2,IMM1
                            DO 1002 J=2,JM2M2
      DIV2M(I,J,L)= (MU2(I+1,J,L)-MU2(I-1,J,L))/(A*COSPH2(J)*2*DLAM)
     +            +(MV2(I,J+1,L)*COSPH2(J+1)
     +             -MV2(I,J-1,L)*COSPH2(J-1) )/(A*COSPH2(J)*2*DPHI)
      if(((I+J)/2*2).EQ.(I+J)) ncount = ncount + 1
      if(((I+J)/2*2).EQ.(I+J)) sumdiv = sumdiv + DIV2M(I,J,L)
      if(((I+J)/2*2).EQ.(I+J)) absdiv = absdiv + ABS(DIV2M(I,J,L))
 1002 CONTINUE
      sumdiv = sumdiv / ncount
      absdiv = absdiv / ncount
c

        DMID = DIV2M(IMID,JMID2Z,L)
        print9444, L, DMID, sumdiv, absdiv, ncount
 9444   FORMAT(' DivM(7,7),lev:',I2,1PE12.3,
     +         ' sumdiv,absdiv :', 1P2E12.3,  i3)
c
 1004 CONTINUE
      DSAVE = DIV2M(IMID,JMID2Z,4)
      print*, ' DSAVE: ', DSAVE
C
C----   Divergence Analysis Check.
      DIVXAS = 0
      DIVYAS = 0
      DIVAS = 0
      IF ( NSTEP.EQ.1 ) THEN
        DO 10049 L=1,LM
        I=IMID
        J=JMID2Z
        DIVX2M =
     +   (MU2(I+1,J,L)-MU2(I-1,J,L))/(A*COSPH2(J)*2*DLAM)
        DIVY2M =
     +   (MV2(I,J+1,L)*COSPH2(J+1)-MV2(I,J-1,L)*COSPH2(J-1))
     +    /  (A*COSPH2(J)*2*DPHI)
        DMIDXY = DIVX2M + DIVY2M 
        print 94449, L, DIVX2M, DIVY2M, DMIDXY
94449   FORMAT(' Div. Anal., Lev:',I2,' DIVX2M DIVY2M',
     +           2F10.4,' DMIDXY ', F10.4)
        DIVXAS = DIVXAS + ABS(DIVX2M)
        DIVYAS = DIVYAS + ABS(DIVY2M)
        DIVAS = DIVAS + ABS(DMIDXY)
10049   CONTINUE
        print 94459, DIVXAS/LM, DIVYAS/LM, DIVAS/LM
94459   FORMAT(' Div. Mean Abs Vals DIVXAS DIVYAS DIVAS',
     +           3F10.4)
      ENDIF
C
C************************************************************
C*********************    (iv)  *****************************
C************************************************************
c
C---------VERTICAL INTEGRATION TO GET SURF. PRESS. TENDENCY---------
c
      DO 1001 J=1,JM2
      DO 1001 I=1,IM
         WORKK2(I,J)=0.
 1001 CONTINUE
c
      DO 1006 L=1,LM
        DO 1006 I=3,IMM2
        DO 1006 J=3,JM2M3
          WORKK2(I,J)=WORKK2(I,J)-DIV2M(I,J,L)*GRAV
 1006  CONTINUE
c
      DPSDT = WORKK2(IMID,JMID2Z)
      print9445, DPSDT
 9445 FORMAT(' Pressure Tendency (7,7)', 1PE12.3,' Pa/s')
      DPSDT2 = DPSDT*6*3600/100
      print9446, DPSDT2
 9446 FORMAT(' Pressure Tendency (7,7)', F12.1,' mb/6h')
C
C--------
C
C     GET the NOISE Parameter based on mean modulus of dps/dt.
      ncount = 0
      PNOIS1 = 0.
      PNOIS2 = 0.
      DO I=3,IMM2
      DO J=3,JM2M3
        IF ( ((I+J)/2*2).EQ.(I+J) ) THEN 
          ncount = ncount + 1
          PNOIS1 = PNOIS1 + ABS(WORKK2(I,J))
        ENDIF
        IF(I.eq.IMID .and. J.eq.JMID2Z) THEN
          PNOIS2 = PNOIS2 + WORKK2(I,J)
        ENDIF
      end do
      end do
      PNOIS1 = PNOIS1 / ncount
      PNOIS2 = ABS(PNOIS2)
c
      print94581, PNOIS1, NSTEP
94581 FORMAT(' PNOIS1 Param, Mean dps/dt',1PE12.3,' NSTEP:',I4)
      PNOISE = PNOIS1*6*3600/100
      print94582, PNOISE
94582 FORMAT(' PNOISE:  Mean abs(dps/dt)',1PE12.3,' hPa/6h ')
      print94583, PNOIS2*6*3600/100
94583 FORMAT(' PNOIS2: dps/dt at mid-pt ',1PE12.3,' hPa/6h ')
C
C************************************************************
C*********************    ( v )  ****************************
C************************************************************
c
C--------------COMPUTATION OF DIVERGENCE (of Velocity) -----------------
c
      DO 1020 J=1,JM2
      DO 1020 I=1,IM
         WORKK2(I,J)=0.
 1020 CONTINUE
C
C     First, get the Velocities.
      DO 1022 L=1,LM
c
      DO 1021 I=1,IM
      DO 1021 J=1,JM2
        RR = RR2(I,J,L)
        U2(I,J,L) = MU2(I,J,L)/RR
        V2(I,J,L) = MV2(I,J,L)/RR
 1021 CONTINUE
CCC     print9455, L, RMID
 9455   FORMAT(' RR(7,7), lev:',I2,1PE12.3)
c
 1022 CONTINUE
c
C     Now, get the Divergences
      DO 1024 L=1,LM
c
      ncount = 0
      sumdiv = 0.
      absdiv = 0.
      DO 1023 I=3,IMM2
      DO 1023 J=3,JM2M3
      DIV2V(I,J,L)= (U2(I+1,J,L)- U2(I-1,J,L))/(A*COSPH2(J)*2*DLAM)
     +             +(V2(I,J+1,L)*COSPH2(J+1)
     +              -V2(I,J-1,L)*COSPH2(J-1) )/(A*COSPH2(J)*2*DPHI)
C = = = Approximation to obtain closer agreement with LFR = = = =
          IF(NSTEP.EQ.1) DIV2V(I,J,L)= DIV2M(I,J,L)/RR2(I,J,L)
C = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      if(((I+J)/2*2).EQ.(I+J)) ncount = ncount + 1
      if(((I+J)/2*2).EQ.(I+J)) sumdiv = sumdiv + DIV2V(I,J,L)
      if(((I+J)/2*2).EQ.(I+J)) absdiv = absdiv + ABS(DIV2V(I,J,L))
c
         IF ( I.EQ.IMID .and. J.EQ.JMID2Z .and. NSTEP.EQ.1 ) THEN
            VERT1(L) = U2(I-1,J,L)
            VERT2(L) = U2(I+1,J,L)
            VERT3(L) = V2(I,J-1,L)
            VERT4(L) = V2(I,J+1,L)
         ENDIF
c
 1023 CONTINUE
      sumdiv = sumdiv / ncount
      absdiv = absdiv / ncount
            DMID = DIV2V(IMID,JMID2Z,L)
            IF(NSTEP.EQ.1)
     X      print9454, L, DMID, sumdiv, absdiv, ncount
 9454       FORMAT(' DivV(7,7),lev:',I2,1PE12.3,
     +             ' sumdiv,absdiv :', 1P2E12.3,  i3)
c
 1024 CONTINUE
c
C     print9453, (L,VERT1(L),VERT2(L),VERT3(L),VERT4(L),M)
 9453 FORMAT(' UUVV(7,7),lev:',I2,2F10.2,3x,2F10.2)
C
C     GET the NOISE Parameters of the Divergences
c
      ncount = 0
      NOISE1 = 0.
      NOISE2 = 0.
         N1MID = 0.
         N2MID = 0.
      DO 1028 I=3,IMM2
      DO 1028 J=3,JM2M3
        sum1 = 0
        sum2 = 0
         DO 1027 L=1,LM
            IF ( ((I+J)/2*2).EQ.(I+J) ) THEN 
               ncount = ncount+1
               sum1 = sum1 + DIV2M(I,J,L) 
               sum2 = sum2 + ABS( DIV2M(I,J,L) )
            ENDIF
            IF(I.eq.IMID .and. J.eq.JMID2Z) THEN
               N1MID = N1MID + DIV2M(I,J,L) 
               N2MID = N2MID + ABS( DIV2M(I,J,L) )
            ENDIF
 1027    CONTINUE
         NOISE1 = NOISE1 + ABS(sum1)
         NOISE2 = NOISE2 + sum2
 1028 CONTINUE
      NOISE1 = NOISE1 / ncount
      NOISE2 = NOISE2 / ncount
C     Convert to hPa per hour.
      NOISE1 = (3600/100) * GRAV * NOISE1 
      NOISE2 = (3600/100) * GRAV * NOISE2
      BRUS = ( NOISE1 / NOISE2 ) * 100.
      N1MID = (3600/100) * GRAV * ABS(N1MID)
      N2MID = (3600/100) * GRAV * N2MID
      BRMID = ( N1MID / N2MID ) * 100.
c
        print9458, NOISE1, NOISE2, BRUS, NSTEP
 9458   FORMAT(' Noise Params (hPa/h), N1 N2 Br : ',1P2E12.3,0PF8.1,
     X         '%   NSTEP:',I4)
        print9459, N1MID, N2MID, BRMID
 9459   FORMAT(' N1 N2 Br at mid-point  : ',1P2E12.3,0PF8.1,'%')
C
      USAVE = U2(IMID,JMID2V,4)
      VSAVE = V2(IMID,JMID2V,4)
c     
        PZERO = 10**5
        DPZERO = 10**5 / LM
        print*,'  PNOIS1/(p0*N1)  ', PNOIS1/(NOISE1*PZERO)
        print*,' DPSDT/(DP*N1MID) ', DPSDT /(N1MID*DPZERO)
        print*,'PNOIS2/(DP*N1MID) ', PNOIS2/(N1MID*DPZERO)
C
C************************************************************
C*********************    ( vi )  ***************************
C************************************************************
c
C--------------THE VERTICAL VELOCITY GRADIENT -------------------------
c
      DO 1032 I=3,IMM2
      DO 1032 J=3,JM2M3 
         PMID = PMID2(I,J,1)
         VERT1(1) = DIV2V(I,J,1) * PMID
         VHALF1   = DIV2V(I,J,1) * ( P2(I,J,1)-PMID )
         DWDZ(I,J,1) = - DIV2V(I,J,1) + VERT1(1)/(GAMMA*PMID)
         DO 1031 L=2,LM
           PMID = PMID2(I,J,L)
           VHALF2 = DIV2V(I,J,L  )*( PMID-P2(I,J,L-1) )
           VERT1(L) = VERT1(L-1) + VHALF1 + VHALF2
           VHALF1 = DIV2V(I,J,L  )*( P2(I,J,L)-PMID )
           DWDZ(I,J,L) = - DIV2V(I,J,L) + VERT1(L)/(GAMMA*PMID)
 1031    CONTINUE
         IF(I.EQ.IMID .and. J.EQ.JMID2Z .and. NSTEP.EQ.1 )
     +   print91031, (DWDZ(I,J,L),l=1,LM)
91031    FORMAT(' DWDZ:', 1P5E15.6)
 1032 CONTINUE
c
C     Correction for small integral term.
      DO 1034 I=3,IMM2
      DO 1034 J=3,JM2M3 
         DUDP = (U2(I,J,2)-U2(I,J,1))/(PMID2(I,J,2)-PMID2(I,J,1))
         DVDP = (V2(I,J,2)-V2(I,J,1))/(PMID2(I,J,2)-PMID2(I,J,1))
         DPDX = (P2(I+1,J,1)-P2(I-1,J,1))/(A*COSPH2(J)*2*DLAM)
         DPDY = (P2(I,J+1,1)-P2(I,J-1,1))/(A*          2*DPHI)
         DVGP = DUDP*DPDX + DVDP*DPDY
         PMID = PMID2(I,J,1)
         VSTEP = DVGP * PMID
         VERT1(1) =  VSTEP
         DWDZ(I,J,1) = DWDZ(I,J,1) - VERT1(1)/(GAMMA*PMID)
        DO 1033 L=2,LM
         DUDP = (U2(I,J,L)-U2(I,J,L-1))/(PMID2(I,J,L)-PMID2(I,J,L-1))
         DVDP = (V2(I,J,L)-V2(I,J,L-1))/(PMID2(I,J,L)-PMID2(I,J,L-1))
         DPDX = (P2(I+1,J,L-1)-P2(I-1,J,L-1))/(A*COSPH2(J)*2*DLAM)
         DPDY = (P2(I,J+1,L-1)-P2(I,J-1,L-1))/(A*          2*DPHI)
         DVGP = DUDP*DPDX + DVDP*DPDY
         DELP = PMID2(I,J,L)-PMID2(I,J,L-1)
           VSTEP = DVGP * DELP
           PMID = PMID2(I,J,L)
           VERT1(L) = VERT1(L-1) + VSTEP
           DWDZ(I,J,L) = DWDZ(I,J,L) - VERT1(L)/(GAMMA*PMID)
 1033    CONTINUE
         IF(I.EQ.IMID .and. J.EQ.JMID2Z .and. NSTEP.EQ.1 )
     +   print91033, (DWDZ(I,J,L),l=1,LM)
91033    FORMAT(' DwDz:', 1P5E15.6)
 1034 CONTINUE
c
C--------------THE VERTICAL VELOCITY ----------------------------------
c
C     Scale Factor for surface wind 
c     (as multiple of bottom layer wind)
CCC   SURFAC = 0.2  !!!   Now read in on a card.
c
      DO 1038 I=3,IMM2
      DO 1038 J=3,JM2M3 
c        First, get the value of w at the ground.
         DHDX = ( OROG(I+1,J)-OROG(I-1,J) ) / (A*COSPH2(J)*2*DLAM)
         DHDY = ( OROG(I,J+1)-OROG(I,J-1) ) / (A*2*DPHI)
         WLAYER = U2(I,J,LM)*DHDX + V2(I,J,LM)*DHDY
         W2(I,J,LM) = SURFAC * WLAYER
C        Now, work up from the bottom, using the gradient. 
         DO 1035 L=LM,2,-1
            IF (L.eq.LM) THEN
               DELTAZ = ZLEV(L-1)-OROG(I,J)
            ELSE
               DELTAZ = DELZ(L)
            ENDIF
            W2(I,J,L-1) = W2(I,J,L) + DELTAZ*DWDZ(I,J,L)
 1035    CONTINUE
         IF(I.EQ.IMID .and. J.EQ.JMID2Z .and. NSTEP.EQ.1 )
     +   print91035, (W2(I,J,L),l=1,LM)
91035    FORMAT('  W2 :', 1P5E15.6)
 1038 CONTINUE
C
C************************************************************
C*********************    ( vii )  **************************
C************************************************************
c
C--------------THE STRATOSPHERIC TEMPERATURE TENDENCY -----------------
c
      DO 1048 I=3,IMM2
      DO 1048 J=3,JM2M3 
         DTT2DT(I,J) = TTOP2(I,J) * DWDZ(I,J,1)
 1048 CONTINUE
c
      DTDT = DTT2DT(IMID,JMID2Z)
C     print9434, TTOP2(IMID,JMID2Z)
 9434 FORMAT(' Temperature (Strato) (7,7)', F12.1,' K')
      print9435, DTDT
 9435 FORMAT(' Temperature Tendency (7,7)', 1PE12.3,' K/s')
      DTDT2 = DTDT*6*3600
      print9436, DTDT2
 9436 FORMAT(' Temperature Tendency (7,7)', F12.1,' K/6h')
C
C************************************************************
C*********************    ( viii )  *************************
C************************************************************
C
C     Get temperature and, thence, density at the interfaces.
      DO 1052 I=3,IMM2
      DO 1052 J=3,JM2M3 
        DO 1051 l=1,LM
          IF ( L.eq.1 ) then
            T2I(I,J,L)  = ( TTOP2(I,J) + T2(I,J,2) ) / 2.
          ELSE IF ( L.eq.LM ) THEN
            T2I(I,J,L)  = ( 3.*T2(I,J,LM)-T2(I,J,LMM1) ) / 2.
          ELSE
            T2I(I,J,L)  = ( T2(I,J,L) + T2(I,J,L+1) ) / 2.
          ENDIF
          RHO2I(I,J,L) = P2(I,J,L) / ( RGAS*T2I(I,J,L) )
 1051   CONTINUE
              IF(I.EQ.IMID .and. J.EQ.JMID2Z .and. NSTEP.EQ.1 ) THEN
                print*, ' T2I  (7,7) ', (T2I  (I,J,L),l=1,LM)
                print*, ' RHO2I(7,7) ', (RHO2I(I,J,L),l=1,LM)
              ENDIF
 1052 CONTINUE
c
c     Get the vertical momentum at the interfaces
c     and the layer boundary term [mh].
      DO 1055 I=3,IMM2
      DO 1055 J=3,JM2M3 
        DO 1053 L=1,LM
          VERT1(L) = RHO2I(I,J,L)*W2(I,J,L)  
 1053   CONTINUE
              IF(I.EQ.IMID .and. J.EQ.JMID2Z .and. NSTEP.EQ.1 )
     +        print*, ' MH(7,7) ', (VERT1(L),l=1,LM)
        DO 1054 L=1,LM
          IF ( L.eq.1 ) then
            VERT2(L) =            - VERT1(L) 
          ELSE IF ( L.eq.LM ) THEN
            VERT2(L) = VERT1(L-1)
          ELSE
            VERT2(L) = VERT1(L-1) - VERT1(L) 
          ENDIF
          BRAKET(I,J,L) = VERT2(L)
 1054   CONTINUE
          IF(I.EQ.IMID .and. J.EQ.JMID2Z .and. NSTEP.EQ.1 ) THEN
            print 90155, (BRAKET(I,J,L),l=1,LM)
90155       FORMAT( ' BRAKET  ', 5F10.4) 
            BRAKAS = 0
            DO L=1,LM
              BRAKAS = BRAKAS + ABS(BRAKET(I,J,L))
            end do
          ENDIF
 1055 CONTINUE
          print 90255, BRAKAS
90255     FORMAT( ' BRAKAS  ', F10.4) 
c
C************************************************************
C*********************    ( ix )  ***************************
C************************************************************
C
C--------------THE CONTINUITY EQUATION ---------------------------------
C
c     Get the tendency of layer 'density' RR: DRR2DT.
c
      SUMM1 = 0
      SUMM2 = 0
      SUMM3 = 0
      DO 1057 I=3,IMM2
      DO 1057 J=3,JM2M3 
        DO 1056 L=1,LM
          DRR2DT(I,J,L) = - ( DIV2M(I,J,L) + BRAKET(I,J,L) )
          DRR2DTX = DRR2DT(I,J,L)*(6*3600)
           IF(I.EQ.IMID .and. J.EQ.JMID2Z .and. NSTEP.EQ.1 ) THEN
              print*, ' DRR2DT ', L, DRR2DT(I,J,L), DRR2DTX
              VERT1(L) =   DRR2DT(I,J,L)*(6*3600)*(grav/100.)
              VERT2(L) = - DIV2M(I,J,L) *(6*3600)*(grav/100.)
              VERT3(L) = - BRAKET(I,J,L)*(6*3600)*(grav/100.)
              SUMM1 = SUMM1 + VERT1(L)
              SUMM2 = SUMM2 + VERT2(L)
              SUMM3 = SUMM3 + VERT3(L)
           ENDIF
 1056   CONTINUE
 1057 CONTINUE
        IF(NSTEP.EQ.1 ) THEN
           print90901, ( VERT1(L), VERT2(L),VERT3(L),L=1,LM )
90901      FORMAT( 'PPPPP: DDP, HORIZ, VERT ', 3F8.1)
           print90902,   SUMM1, SUMM2, SUMM3           
90902      FORMAT( ':::::::  SUMS        ', 3F8.1)
        ENDIF
C
C************************************************************
C*********************    ( x )  ****************************
C************************************************************
C
c     Get the tendency of interface pressures DP2DT.
c
      DO 1059 I=3,IMM2
      DO 1059 J=3,JM2M3 
        DP2DT(I,J,1) = GRAV*DRR2DT(I,J,1)
        DO 1058 L=2,LM
          DP2DT(I,J,L) = DP2DT(I,J,L-1) + GRAV*DRR2DT(I,J,L)
 1058   CONTINUE
 1059 CONTINUE
      IF(NSTEP.EQ.1)
     X    print9641, (L, (6*3600/100)*DP2DT(IMID,JMID2Z,L),L=1,LM)
 9641 FORMAT(' DP2DT, Lev',I2,F12.1,' mb/6h')
      DPSDT = DP2DT(IMID,JMID2Z,LM)
      print9642, DPSDT
 9642 FORMAT(' Pressure Tendency (7,7)', 1PE12.3,' Pa/s')
      DPSDT = DPSDT*6*3600/100
      print9643, DPSDT,NSTEP
 9643 FORMAT(' Pressure Tendency (7,7)', F12.1,' mb/6h ### NSTEP:',I4)
C
C-----------------------------------------------------------------------
c
c       Interpolate Fields from scalar to vector points
        DO L=1,LM
          CALL SMEARZ(RR2(1,1,L),RR2(1,1,L),IM,JM)
          CALL SMEARZ(RHO2I(1,1,L),RHO2I(1,1,L),IM,JM)
          CALL SMEARZ(W2(1,1,L),W2(1,1,L),IM,JM)
        end do
C
C***********************************************************************
C***********************************************************************
C
C***********************************************************************
C***********************************************************************
c
C       MOMENTUM EQUATIONS.
C
C-----------------------------------------------------------------------
      print*, ' MMMMM:  Solution of Momentum Equations '
c
      DO 2000 L=1,LM
c
          IF(NSTEP.EQ.1) print9921,L
 9921     FORMAT('MMM: Momentum Tendency, Level:',I2)
c
C************************************************************
C*********************    ( xi )  ***************************
C************************************************************
C
C--------------THE PRESSURE GRADIENT ----------------------------------
c
      IF ( L.LT.LM ) THEN
c
        DO 1102 I=3,IMM2
        DO 1102 J=3,JM2M3 
          DDXP = ( PP2(I+1,J,L)-PP2(I-1,J,L) ) / (A*COSPH2(J)*2*DLAM)
          DDYP = ( PP2(I,J+1,L)-PP2(I,J-1,L) ) / (A*2*DPHI)
              IF(I.EQ.IMID .and. J.EQ.JMID2V .and. NSTEP.EQ.1) THEN
                 print*, ' DDXP, DDYP ', DDXP, DDYP
                 VERT1(L) = DDXP*6*3600
                 VERT4(L) = DDYP*6*3600
              ENDIF
         DMU2DT(I,J,L) = DDXP
         DMV2DT(I,J,L) = DDYP
 1102   CONTINUE
c
      ELSE
C
        DO 1104 I=3,IMM2
        DO 1104 J=3,JM2M3 
          TERM1X = (PP2(I+1,J,L)-PP2(I-1,J,L))
          TERM2X = (OROG(I+1,J)-OROG(I-1,J))*
     X             (P2(I+1,J,L)-P2(I-1,J,L))/
     X             (LOG(P2(I+1,J,L))-LOG(P2(I-1,J,L)))
          DDXP = ( TERM1X + TERM2X ) / (A*COSPH2(J)*2*DLAM)
          TERM1Y = (PP2(I,J+1,L)-PP2(I,J-1,L))
          TERM2Y = (OROG(I,J+1)-OROG(I,J-1))*
     X             (P2(I,J+1,L)-P2(I,J-1,L))/
     X             (LOG(P2(I,J+1,L))-LOG(P2(I,J-1,L)))
          DDYP = ( TERM1Y + TERM2Y ) / (A*2*DPHI)
              IF (I.EQ.IMID .and. J.EQ.JMID2V .and.  NSTEP.EQ.1 ) THEN
                 print*, ' DDXP, DDYP ', DDXP, DDYP
                 print*, ' TM1X, TM1Y ',
     X           TERM1X/(A*COSPH2(J)*2*DLAM),TERM1Y/(A*2*DPHI)
                 print*, ' TERM2X, TERM2Y ',
     X           TERM2X/(A*COSPH2(J)*2*DLAM),TERM2Y/(A*2*DPHI)
                 VERT1(L) = DDXP*6*3600
                 VERT4(L) = DDYP*6*3600
              ENDIF
         DMU2DT(I,J,L) = DDXP
         DMV2DT(I,J,L) = DDYP
 1104   CONTINUE
c
      ENDIF
C
C************************************************************
C*********************    ( xii )  **************************
C************************************************************
C
C--------------THE CORIOLIS FORCE (and Spherical Terms) ---------------
c
        DO 1105 I=3,IMM2
        DO 1105 J=3,JM2M3 
          CORX = - MV2(I,J,L) * FCOR2(J)
          CORY = + MU2(I,J,L) * FCOR2(J)
              IF(I.EQ.IMID .and. J.EQ.JMID2V .and. NSTEP.EQ.1) THEN
                 print*, ' CORX, CORY ', CORX, CORY
                 VERT2(L) = CORX*6*3600
                 VERT5(L) = CORY*6*3600
              ENDIF
          DMU2DT(I,J,L) =  DMU2DT(I,J,L) + CORX
          DMV2DT(I,J,L) =  DMV2DT(I,J,L) + CORY
c
          SPHERU = -2.*MU2(I,J,L)*MV2(I,J,L)*TAN2(J)/(A*RR2(I,J,L)) 
          SPHERV = (MU2(I,J,L)**2-MV2(I,J,L)**2)*TAN2(J)/(A*RR2(I,J,L)) 
              IF(I.EQ.IMID .and. J.EQ.JMID2V .and. NSTEP.EQ.1 )
     +        print*, ' SPHERIC    ', SPHERU, SPHERV
          DMU2DT(I,J,L) =  DMU2DT(I,J,L) + SPHERU
          DMV2DT(I,J,L) =  DMV2DT(I,J,L) + SPHERV
 1105   CONTINUE
c
C************************************************************
C*********************    ( xiii )  *************************
C************************************************************
C
C--------- Horizontal Advection or flux terms --------------------------
C
        DO 1106 I=3,IMM2
        DO 1106 J=3,JM2M3 
          XPLUS = ( MU2(I+2,J,L)**2 )/RR2(I+2,J,L)
          XMINS = ( MU2(I-2,J,L)**2 )/RR2(I-2,J,L)
          FLUXUX = ( XPLUS-XMINS ) / (A*COSPH2(J)*4*DLAM)
          YPLUS = ( MU2(I,J+2,L)*MV2(I,J+2,L) )/RR2(I,J+2,L)
          YMINS = ( MU2(I,J-2,L)*MV2(I,J-2,L) )/RR2(I,J-2,L)
          FLUXUY = ( YPLUS-YMINS ) / (A*4*DPHI)
c
          XPLUS = ( MU2(I+2,J,L)*MV2(I+2,J,L) )/RR2(I+2,J,L)
          XMINS = ( MU2(I-2,J,L)*MV2(I-2,J,L) )/RR2(I-2,J,L)
          FLUXVX = ( XPLUS-XMINS ) / (A*COSPH2(J)*4*DLAM)
          YPLUS = ( MV2(I,J+2,L)**2 )/RR2(I,J+2,L)
          YMINS = ( MV2(I,J-2,L)**2 )/RR2(I,J-2,L)
          FLUXVY = ( YPLUS-YMINS ) / (A*4*DPHI)
              IF(I.EQ.IMID .and. J.EQ.JMID2V .and. NSTEP.EQ.1 ) THEN
                 print*, ' FLUX(east)', FLUXUX, FLUXVX
                 print*, ' FLUX(nrth)', FLUXUY, FLUXVY
              ENDIF
         DMU2DT(I,J,L) = DMU2DT(I,J,L) + (FLUXUX+FLUXUY)
         DMV2DT(I,J,L) = DMV2DT(I,J,L) + (FLUXVX+FLUXVY)
 1106   CONTINUE
c
C************************************************************
C*********************    ( xiv )  **************************
C************************************************************
C
C--------- Vertical Advection or flux terms ----------------------------
c
C     (Note: Perhaps w Should be Smoothed from M to P points.)
      DO 1109 I=3,IMM2
      DO 1109 J=3,JM2M3 
          IF ( L.eq.1 ) then
            UHI = 0.
            VHI = 0.
            WHI = 0.
            ULO = ( U2(I,J,L)+U2(I,J,L+1) ) / 2.
            VLO = ( V2(I,J,L)+V2(I,J,L+1) ) / 2.
            WLO = W2(I,J,L)*RHO2I(I,J,L)  
ccccc             WLO = W2(I,J-1,L)*RHO2I(I,J-1,L)  
          ELSE IF ( L.eq.LM ) THEN
            UHI = ( U2(I,J,L)+U2(I,J,L-1) ) / 2.
            VHI = ( V2(I,J,L)+V2(I,J,L-1) ) / 2.
            WHI = W2(I,J,L-1)*RHO2I(I,J,L-1)  
ccccc             WHI = W2(I,J-1,L-1)*RHO2I(I,J-1,L)  
            ULO = 0.
            VLO = 0.
            WLO = 0.
          ELSE
            UHI = ( U2(I,J,L)+U2(I,J,L-1) ) / 2.
            VHI = ( V2(I,J,L)+V2(I,J,L-1) ) / 2.
            WHI = W2(I,J,L-1)*RHO2I(I,J,L-1)  
ccccc             WHI = W2(I,J-1,L-1)*RHO2I(I,J,L-1)  
            ULO = ( U2(I,J,L)+U2(I,J,L+1) ) / 2.
            VLO = ( V2(I,J,L)+V2(I,J,L+1) ) / 2.
            WLO = W2(I,J,L)*RHO2I(I,J,L)  
ccccc             WLO = W2(I,J-1,L)*RHO2I(I,J-1,L)  
          ENDIF
          VFLXU1 =   UHI*WHI 
          VFLXU2 = - ULO*WLO
          VFLXV1 =   VHI*WHI 
          VFLXV2 = - VLO*WLO
              IF(I.EQ.IMID .and. J.EQ.JMID2V .and. NSTEP.EQ.1 ) THEN
                 print*, ' WFLUX UPR ', VFLXU1, VFLXV1
                 print*, ' WFLUX LWR ', VFLXU2, VFLXV2
              ENDIF
         DMU2DT(I,J,L) = DMU2DT(I,J,L) + ( VFLXU1 + VFLXU2 )
         DMV2DT(I,J,L) = DMV2DT(I,J,L) + ( VFLXV1 + VFLXV2 ) 
 1109 CONTINUE
c
C-----------------------------------------------------------------------
c
c       Divergence Damping
        IF ( DIVDAMP ) THEN
           DO 1112 I=3,IMM2
           DO 1112 J=3,JM2M3 
             DDXDM=(DIV2M(I+1,J,L)-DIV2M(I-1,J,L))/(A*COSPH2(J)*2*DLAM)
             DDYDM=(DIV2M(I,J+1,L)-DIV2M(I,J-1,L))/(A*2*DPHI)
                 IF(I.EQ.IMID .and. J.EQ.JMID2V .and. NSTEP.EQ.1 )
     +           print*, ' DDXDM, DDYDM ', DDXDM, DDYDM
             DMU2DT(I,J,L) = DMU2DT(I,J,L) - CDDAMP*DDXDM
             DMV2DT(I,J,L) = DMV2DT(I,J,L) - CDDAMP*DDYDM 
 1112      CONTINUE
        ENDIF
c
C-----------------------------------------------------------------------
c
 2000 CONTINUE
C
C************************************************************
C*********************    ( xv )  ***************************
C************************************************************
C
C----------------- Tendency of Momenta ---------------------------------
c
        DO 1400 L=1,LM
c
        DO 1200 I=3,IMM2
        DO 1200 J=3,JM2M3 
         DMU2DT(I,J,L) = - DMU2DT(I,J,L)
         DMV2DT(I,J,L) = - DMV2DT(I,J,L)
 1200   CONTINUE
c
      DDTMU = DMU2DT(IMID,JMID2V,L)
      DDTMV = DMV2DT(IMID,JMID2V,L)
      DDTMU2 = DDTMU*6*3600
      DDTMV2 = DDTMV*6*3600
      print9742, DDTMU, DDTMV, DDTMU2, DDTMV2
 9742 FORMAT('Momentum Tendencies:(', 2F7.2,')kg/m/s; (',
     X                             -3P2F6.1,')T/m/6h')
      VERT3(L) = DDTMU2 + ( VERT1(L)+VERT2(L) )
      VERT6(L) = DDTMV2 + ( VERT4(L)+VERT5(L) )
c
        IF(NSTEP.EQ.1 ) THEN
           print90911, DDTMU2,-VERT1(L),-VERT2(L), VERT3(L)
           print90912, DDTMV2,-VERT4(L),-VERT5(L), VERT6(L)
90911      FORMAT( 'UUUUU:  DU, PGF, COR, RES ', -3P4F8.1)
90912      FORMAT( 'VVVVV:  DV, PGF, COR, RES ', -3P4F8.1)
        ENDIF
c
 1400   CONTINUE
C
C***********************************************************************
C***********************************************************************
C
C***********************************************************************
C***********************************************************************
c
C       CALCULATE THE FIELDS AT THE NEW TIME.
        DO 1600 I=3,IMM2
        DO 1600 J=3,JM2M3 
           DO L=1,LM
             P2NEW (I,J,L) = P2OLD (I,J,L) + DELTAT*DP2DT (I,J,L)
             MU2NEW(I,J,L) = MU2OLD(I,J,L) + DELTAT*DMU2DT(I,J,L)
             MV2NEW(I,J,L) = MV2OLD(I,J,L) + DELTAT*DMV2DT(I,J,L)
           end do
           TT2NEW(I,J) = TT2OLD(I,J) + DELTAT*DTT2DT(I,J)
 1600   CONTINUE
c
C------- Filtering and Smoothing ---------------------------------------
c
c     Apply the Robert-Asselin time filter.
      IF ( TIMEFILT ) THEN
          ALPHA = EpsTime/2.
          DO 1700 I=3,IMM2
          DO 1700 J=3,JM2M3 
             DO L=1,LM
               P2 (I,J,L) = P2 (I,J,L) + 
     X           ALPHA*(P2OLD (I,J,L)-2*P2 (I,J,L)+P2NEW (I,J,L))
               MU2(I,J,L) = MU2(I,J,L) + 
     X           ALPHA*(MU2OLD(I,J,L)-2*MU2(I,J,L)+MU2NEW(I,J,L))
               MV2(I,J,L) = MV2(I,J,L) + 
     X           ALPHA*(MV2OLD(I,J,L)-2*MV2(I,J,L)+MV2NEW(I,J,L))
             end do
             TTOP2(I,J) = TTOP2(I,J) + 
     X         ALPHA*(TT2OLD(I,J)-2*TTOP2(I,J)+TT2NEW(I,J))
 1700     CONTINUE
      ENDIF
c
C     Spatial Smoothing with Raymond Filter.
C     ISWEST = 2
C     ISEAST = IMM1
C     JSSOUTH = 2
C     JSNORTH = JM2M2
                ISWEST = 1
                ISEAST = IM
                JSSOUTH = 1
                JSNORTH = JM2M1
c
C     Allow for DAMP COEFF to depend on the time.
      IF ( EPSSDT.eq.0. ) THEN
         EPSILON = EpSpace 
      ELSE
         EPSILON = EpSpace * ((NSTEP-1)*DT)/(EPSSDT*3600.-DT)
      ENDIF
      print*, ' EPSILON  ', EPSILON
C
      IF ( MASSFILT ) THEN
c          First, average the fields. 
           DO L=1,LM
             CALL SMEARZ(P2NEW (1,1,L),P2NEW (1,1,L),IM,JM)
           end do
           CALL SMEARZ(TT2NEW,TT2NEW,IM,JM)
C          Then, Apply the Implicit Filter (Surface pressure
C          requires special treatment).
           DO L=1,LMM1
             CALL IMPFIL(P2NEW (1,1,L),P2NEW (1,1,L),IM,JM2,IM,JM2,
     X            ISWEST,ISEAST,JSSOUTH,JSNORTH, EPSILON)
           end do
           CALL IMPFIL(TT2NEW,TT2NEW,IM,JM2,IM,JM2,
     X            ISWEST,ISEAST,JSSOUTH,JSNORTH, EPSILON)
           CALL PSFILT(P2NEW(1,1,LM),OROG,PSEA2,IM,JM2,
     X            ISWEST,ISEAST,JSSOUTH,JSNORTH, EPSILON)
      ENDIF
c
      IF ( WINDFILT ) THEN
c          First, average the fields. 
           DO L=1,LM
             CALL SMEARV(MU2NEW(1,1,L),MU2NEW(1,1,L),IM,JM)
             CALL SMEARV(MV2NEW(1,1,L),MV2NEW(1,1,L),IM,JM)
           end do
C          Then, Apply the Implicit Filter.
           DO L=1,LM
             CALL IMPFIL(MU2NEW(1,1,L),MU2NEW(1,1,L),IM,JM2,IM,JM2,
     X            ISWEST,ISEAST,JSSOUTH,JSNORTH, EPSILON)
             CALL IMPFIL(MV2NEW(1,1,L),MV2NEW(1,1,L),IM,JM2,IM,JM2,
     X            ISWEST,ISEAST,JSSOUTH,JSNORTH, EPSILON)
           end do
      ENDIF
c
C-----------------------------------------------------------------------
c
c       print out a few of the new values.
         print9191, 
     X    ( NSTEP, L, P2NEW (IMID,JMID2Z,L), MU2NEW(IMID,JMID2V,L),
     X                MV2NEW(IMID,JMID2V,L) ,  L=1,LM )
 9191    FORMAT(' NsteP=',I6,'   LeveL=',I1,'   P2/MU2/MV2/ ',
     X                 F12.1,2x,2f9.1) 
         print9192, NSTEP, TT2NEW(IMID,JMID2Z)
 9192    FORMAT(' NsteP=',I6,'   TTOP2/',F12.1) 
C
C       Calculate the total Mass or areal pressure integral.
        ZAREA = 0.
        ZMASS = 0.
        DO J=1,JM2
        DO I=1,IM
            DAREA = (A*COSPH2(J)*DLAM)*(A*DPHI)
            ZAREA = ZAREA + DAREA
            DMASS = P2 (I,J,LM)*DAREA
            ZMASS = ZMASS + DMASS
        end do
        end do
        print9195, NSTEP, ZAREA, ZMASS, ZMASS/ZAREA 
 9195   FORMAT(' NsteP=',I6,' Area, MASS, pMEAN:',1P2E12.3,-2PF12.1) 
c
C-----------------------------------------------------------------------
C------ Prepare for the Next Time Step ---------------------------------
c
c       Copy the MIDDLE values into the HISTORICAL fields. 
        DO L=1,LM
          CALL SMEARZ(P2 (1,1,L),P2OLD (1,1,L),IM,JM)
          CALL SMEARV(MU2(1,1,L),MU2OLD(1,1,L),IM,JM)
          CALL SMEARV(MV2(1,1,L),MV2OLD(1,1,L),IM,JM)
        end do
        CALL SMEARZ(TTOP2,TT2OLD,IM,JM)
C
c       Copy the NEW values into the MIDDLE fields. 
        DO L=1,LM
          CALL SMEARZ(P2NEW (1,1,L),P2 (1,1,L),IM,JM)
          CALL SMEARV(MU2NEW(1,1,L),MU2(1,1,L),IM,JM)
          CALL SMEARV(MV2NEW(1,1,L),MV2(1,1,L),IM,JM)
        end do
        CALL SMEARZ(TT2NEW,TTOP2,IM,JM)
C
C-----------------------------------------------------------------------
c
C       Calculate Energy and other diagnostic quantities.
        NTSTEP = NSTEP
C       CALL ENERGY(NTSTEP, FI,U,V,FI0,IM,JM, DX,DY,
C    X                    EK,EA,EP,AK,PK,EM, .TRUE.)
C
  		XVAR(NTSTEP)=NTSTEP
  		YVAR(NTSTEP)= P2NEW (IMID,JMID2Z,LM)
		UVAR(NTSTEP)=USAVE
		VVAR(NTSTEP)=VSAVE
 		DVAR(NTSTEP)=0.
                KVAR(NTSTEP)=EK
                AVAR(NTSTEP)=EA
		PVAR(NTSTEP)=EP
                MVAR(NTSTEP) =  ZMASS/ZAREA
		N1VAR(NTSTEP-1)= NOISE1
		N2VAR(NTSTEP-1)= NOISE2
                BRVAR(NTSTEP-1)= BRUS
		N1PVAR(NTSTEP-1)=  N1MID
		N2PVAR(NTSTEP-1)=  N2MID
                BRPVAR(NTSTEP-1) = BRMID
C
c---------------------------------------------------------------
c
C      Write out Results at intermittent times.
c
      DO 3000 NWRITE=1,10
         TIME = NSTEP*DT
         HR= TIME/(3600.)
         IF ( ABS(HR).EQ.WHOUR(NWRITE) ) THEN
         print*,' Write-out at ', HR, '   Hours '
c
C           Convert fields to PSEA and Z and U and V for OUTPUT.
            CALL PTOZ(P2,TTOP2,OROG, Z2,PSEA2,IM,JM2,LM)
            CALL MMTOUV(P2,MU2,MV2, U2,V2,IM,JM2,LM)
C
            WFILE = WNAME(NWRITE)
C           Write out the Data.
            OPEN(UNIT=50,FILE=WFILE,FORM='UNFORMATTED')
            WRITE(50) PSEA2,Z2,U2,V2,TTOP2,OROG
            WRITE(50) P2,MU2,MV2
            CLOSE(UNIT=50)
         ENDIF
 3000 CONTINUE
C
C***********************************************************************
c
c     Update the values of the INITIALIZED arrays. 
      IF ( INIT ) THEN
        DO J=1,JM2
        DO I=1,IM
          DO L=1,LM
            P2INI (I,J,L) = P2INI (I,J,L) + hh(nstep)*P2 (I,J,L)
            MU2INI(I,J,L) = MU2INI(I,J,L) + hh(nstep)*MU2(I,J,L)
            MV2INI(I,J,L) = MV2INI(I,J,L) + hh(nstep)*MV2(I,J,L)
          end do
        TT2INI(I,J) = TT2INI(I,J) + hh(nstep)*TTOP2(I,J)
        end do
        end do
      ENDIF
c
c---------------------------------------------------------------
c
      DPTRUE = (P2(IMID,JMID2Z,LM)-P2OLD(IMID,JMID2Z,LM))/DT
      print9842, DPTRUE
 9842 FORMAT(' True Pressure Tendency (7,7)', 1PE12.3,' Pa/s')
      DPTRUE = DPTRUE*6*3600/100
      print9843, DPTRUE,NSTEP
 9843 FORMAT('True Pressure Tendency (7,7)', 
     X         F12.1,' mb/6h #%# NSTEP:',I4)
c
c---------------------------------------------------------------
c
 5000 CONTINUE
c
c-----------------------------------------------------------------------
C--------------OUTPUT SECTION-------------------------------------------
C
        print*,' Opening graph file "graf.dat".'

        OPEN(UNIT=33, status = 'replace', FILE='graf.dat',
     &  FORM='UNFORMATTED')
          WRITE (33)  NTSTEP,XVAR,YVAR
          WRITE (33)  NTSTEP,XVAR,UVAR
          WRITE (33)  NTSTEP,XVAR,VVAR
          WRITE (33)  NTSTEP,XVAR,DVAR
          WRITE (33)  NTSTEP,XVAR,KVAR
          WRITE (33)  NTSTEP,XVAR,AVAR
          WRITE (33)  NTSTEP,XVAR,PVAR
          WRITE (33)  NTSTEP,XVAR,MVAR
          WRITE (33)  NTSTEP,XVAR,N1VAR
          WRITE (33)  NTSTEP,XVAR,N2VAR
          WRITE (33)  NTSTEP,XVAR,BRVAR
          WRITE (33)  NTSTEP,XVAR,N1PVAR
          WRITE (33)  NTSTEP,XVAR,N2PVAR
          WRITE (33)  NTSTEP,XVAR,BRPVAR
        CLOSE(UNIT=33)
C
C-----------------------------------------------------------------------
c
C     Write out Initialized fields.
c
      IF ( INIT ) THEN
        IF ( DT .LT. 0 ) THEN
           WIFILE = 'fini1.dat'
           OPEN(UNIT=60, status = 'replace', FILE=WIFILE,
     &       FORM='UNFORMATTED')
           WRITE(60) P2INI,MU2INI,MV2INI, TT2INI
           CLOSE(UNIT=60)
        ELSE IF ( DT .GT. 0 ) THEN
           CALL PTOZ(P2INI,TT2INI,OROG, Z2,PSEA2,IM,JM2,LM)
           CALL MMTOUV(P2INI,MU2INI,MV2INI, U2,V2,IM,JM2,LM)
           WIFILE = 'finit.dat'
           OPEN(UNIT=65, status = 'replace', FILE=WIFILE,
     &       FORM='UNFORMATTED')
           WRITE(65) PSEA2,Z2,U2,V2,TT2INI,OROG
           WRITE(65) P2INI,MU2INI,MV2INI
           CLOSE(UNIT=65)
        ENDIF
      ENDIF
!
!  Terminate.
!
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RICHARDSON:'
      write ( *, '(a)' ) '  Normal end of execution.'
      STOP

  998 STOP 998  
  999 STOP 999  

      END
      SUBROUTINE  DOLPH(DeltaT,NCHOICE,TauS,r,M, WINDOW)

c*********************************************************************72
C
cc DOLPH calculates the Dolph-Chebyshev window.
c
C       Calculation of Dolph-Chebyshev window or, for short,
C       Dolph Window, using the expression in the reference:
C
C       Antoniou, Andreas, 1993: Digital Filters: Analysis,
C       Design and Applications. McGraw-Hill, Inc., 689pp.
C
C       The Dolph window is optimal in the following sense:
C       For a given main-lobe width, the stop-band attenuation
C       is minimal; for a given stop-band level, the main-lobe
C       width is minimal.
C
C       It is possible to specify either the ripple-ratio r
C       or the stop-band edge THETA1.
C
        REAL WINDOW(0:2*M)
C
        PARAMETER (NMAX = 1000)
        REAL T(0:NMAX)
        REAL w(0:NMAX),TIME(0:NMAX)
        REAL w2(0:2*NMAX),TIME2(0:2*NMAX)
C
        COMPLEX HH(0:1000)
        CHARACTER*10 STRING
        integer system
        LOGICAL CHECK
        DATA CHECK /.false./
C
C       OPEN(UNIT= 5,FILE='dolph.cds')
C       ierr=system('cat dolph.cds ')
C
C       READ(5,*)  STRING, DeltaT
C       READ(5,*)  STRING, NCHOICE
C       READ(5,*)  STRING, TauS
C       READ(5,*)  STRING, r
C       READ(5,*)  STRING, M
C
C       Set up the parameters to specify the window.
C          There are two options: 
C            (1) specify the cutoff period, TauS
C            (2) specify the Ripple ratio, r.
C
        PI = 4*ATAN(1.D0)
C
C       print*, '-------------------------------------------'
        N = 2*M+1
        NM1 = N-1 
C         print*, ' M, N '
C         print*,   M, N
C
        IF      ( NCHOICE.eq.1 ) THEN
          TauS = TauS * 3600
          ThetaS = 2*PI*DeltaT/TauS
C         print*,' NCHOICE = ',NCHOICE,' Specify cutoff period'
C         print*, ' ThetaS (Stop-band edge): ', ThetaS
          X0 = 1/COS(ThetaS/2)  
C         print*, ' X0   ', X0
          TERM1 = (X0 + SQRT(X0**2-1))**(FLOAT(N-1))  
          TERM2 = (X0 - SQRT(X0**2-1))**(FLOAT(N-1))  
          RR = 0.5*(TERM1+TERM2)
          r = 1/RR
          dB = 20*LOG10(r)
C         print*, ' r dB   ', r, dB
C
        ELSE IF ( NCHOICE.eq.2 ) THEN
C         print*,' NCHOICE = ',NCHOICE,' Specify ripple ratio'
          R = 0.1
            dB = 20*LOG10(R)
C           print*, '######################################'
C           print*, ' Ripple ratio  r = ', R
C           print*, ' Ripple ratio dB = ', dB
C           print*, '######################################'
          RR = 1/R
          COSHINV = LOG(RR+SQRT(RR**2-1))
          X0 = COSH(COSHINV/(N-1))
C         print*, ' COSHINV, X0 '
C         print*,   COSHINV, X0
C         print*, ' M, N, R, X0         '
C         print*,   M, N, R, X0  
          THETA1 = (FLOAT(N)/FLOAT(M))*ACOS(1/X0)
          THETA1 =                   2*ACOS(1/X0)
C         print*, ' THETA1 (Stop-band edge): ', THETA1
C           TERM1 = (RR + SQRT(RR**2-1))**(1/FLOAT(N-1))  
C           TERM2 = (RR - SQRT(RR**2-1))**(1/FLOAT(N-1))  
C           Z0 = 0.5*(TERM1+TERM2)
C           print*, ' Z0   ', Z0
C           TERM1 = (Z0 + SQRT(Z0**2-1))**(FLOAT(N-1))  
C           TERM2 = (Z0 - SQRT(Z0**2-1))**(FLOAT(N-1))  
C           r2 = 1/(0.5*(TERM1+TERM2))
C           dB2 = 20*LOG10(r2)
C           print*, ' r2 dB2   ', r2, dB2
C
        ELSE 
          print*, ' Invalid value of NCHOICE ', NCHOICE
          STOP
        ENDIF
C
C       print*, '-------------------------------------------'
C
        DO nT=0,M
          SUM = RR
          DO i=1,M
            arg = X0*cos(i*PI/N)
            CALL CHEBY(T,NM1,ARG)
            TERM1 = T(NM1)
            TERM2 = cos(2*nT*PI*i/N)
            SUM = SUM + 2*TERM1*TERM2
          end do
          w(nT) = SUM/N
          TIME(nT) = nT 
C           print*, ' TIME, w ',  TIME(nT), w(nT)
        end do
C
C       Fill in the negative-time values by symmetry.
        DO nT=0,M
          w2(M+nT) = w(nT)
          w2(M-nT) = w(nT)
          TIME2(M+nT) = TIME(nT)
          TIME2(M-nT) = -TIME(nT)
        end do
C
C  Fill up the array for return. NORMILIZE WINDOW.
c
        WINSOM = 0
        DO nT=0,2*M
          WINDOW(nT) = w2(nT)
          WINSOM = WINSOM + WINDOW(nT)
        end do
        DO nT=0,2*M
          WINDOW(nT) = WINDOW(nT) / WINSOM
        end do
C
      IF ( .not. CHECK ) RETURN
C
C----------------------------------------------------------
CC      YMIN =  0.
CC      YMAX =  0.
CC      CALL PLOTLN (TIME,w,0,M,YMIN,YMAX,'   w      ')
CC      print*, (TIME2(nT), nT=0,2*M)
CC      print*, (w2(nT),    nT=0,2*M)
CC      YMIN =  0.
CC      YMAX =  0.
CC      CALL PLOTLN (TIME2(0),w2(0),0,2*M,YMIN,YMAX,'   w2     ')
CC      CALL RESPONSE(w2,0, N+1,2*NMAX,HH,PI)
C
        RETURN
        END
        SUBROUTINE CHEBY(T,N,X)

C*********************************************************************72
c
CC CHEBY calculates Chebyshev polynomials up to order N.
C
C       Reference: Numerical Recipes, Page 184, recurrence
C           T_n(x) = 2xT_{n-1}(x) - T_{n-2}(x) ,  n>=2.
C
        REAL T(0:N)

        T(0) = 1
        T(1) = X
        IF(N.lt.2) RETURN
        DO NN=2,N
          T(NN) = 2*X*T(NN-1) - T(NN-2)
        end do

        RETURN
        END
        SUBROUTINE GETDAT

C*********************************************************************72
C
CC GETDAT prepares initial data for the program.
C 
C   (1) Read the basic data files:
c       PS.DAT --- sea level pressure on the E-grid
c       Z.DAT  --- heights of the pressure surfaces
c                  200, 400, 600 and 800 mb,
c                  on a coarse A_Grid
c       UV.DAT --- Wind Direction/Speed at levels
c                  100, 300, 500, 700 and 900mb,
c                  on a coarse A_Grid
c       T.DAT  --- 200-100mb thickness for calculation of
c                  temperature in the stratosphere
c       OROG.DAT - height of orography on fine A-grid.
c
c   (2) Fill up the E-grid from the coarse A-grid
c       where necessary (for Z, u, v and TTOP ),
c       in S/Rs ATOEZ and ATOEV.
c
c   (3) If required, fill up a fime A-grid to make
c       plotting easier, with the same grid for all.
c
c   (4) Convert sea-level pressure and heights to pressures
c       at ground level and conventional interface levels.
c
c   (5) Convert wind data to momenta in layers.
c
c
c       size of grid
        PARAMETER (IM=13, JM=7, LM=5)
        PARAMETER (LMM1=LM-1)
c
c
c      2-D Work fields common to all modules.
       COMMON /WORK/ WORK1(IM,JM), WORK2(IM,JM), WORK3(IM,JM)
c
c       space for the basic INPUT variables 
        REAL PS(IM,JM), Z(IM,JM,LMM1)
        REAL U(IM,JM,LM),V(IM,JM,LM), DDDFF(IM,JM,LM)
        REAL OROG(IM,2*JM)
        COMMON / WINPUT / PS, Z, U, V, DDDFF, OROG
c
c       space for the basic PROGNOSTIC variables
        real P(IM,JM,LM),MU(IM,JM,LM),MV(IM,JM,LM),TTOP(IM,JM)
        COMMON / WPROG / P, MU, MV, TTOP
c
c       size of fine A-grid
        PARAMETER (JM2=14,JM2M1=JM2-1,JM2M2=JM2-2,JM2M3=JM2-3)
C
c       space for various DIAGNOSTIC variables on FINE grid
        real P2 (IM,JM2,LM),U2(IM,JM2,LM),V2(IM,JM2,LM)
        COMMON / WDIAG2 / P2, U2, V2
c
c       space for the basic PROGNOSTIC variables on FINE grid
        real PP2(IM,JM2,LM),MU2(IM,JM2,LM),
     +       MV2(IM,JM2,LM),TTOP2(IM,JM2)
        COMMON / WPROG2 / PP2, MU2, MV2, TTOP2
C
c
        LUNPS= 21
        LUNZ = 22
        LUNV = 23
        LUNT = 24
        LUNO = 25
        open(unit=LUNPS, status = 'old', file='ps.txt' )
        open(unit=LUNZ, status = 'old', file='z.txt' )
        open(unit=LUNV, status = 'old', file='uv.txt' )
        open(unit=LUNT, status = 'old', file='t.txt' )
        open(unit=LUNO, status = 'old', file='orog.txt')
c
c
c       read the sea-level pressure.
        CALL READPS(LUNPS,PS,IM,JM)
c
C       read the geopotential heights.
        CALL READZ(LUNZ,Z,IM,JM,LMM1)
c
c       read the wind data.
        CALL READV(LUNV,U,V,DDDFF,IM,JM,LM)
c
c       read the stratospheric temperature.
        CALL READT(LUNT,TTOP,IM,JM)
c
c       read the height of orography
        CALL READO(LUNO,OROG,IM,JM)
c
c       Get the initial pressures.
        CALL ZTOP(PS,Z,TTOP,OROG,P,IM,JM,LM)
c
c       Get the initial momenta.
        CALL UVTOMM(U,V,P,MU,MV,IM,JM,LM)
c
c
C       WRITE OUT THE INITIAL FIELDS.
c       NDISC=88
c       OPEN(UNIT=NDISC,FILE='RICHIE.DAT',FORM='FORMATTED')
c       WRITE(NDISC,777) P,MU,MV,TTOP,OROG
c777    format( 5E20.8 )
C
C
c       fill the fine A-grid for plotting, and write out.
c       CALL FILLZ(PS,ZPLOT,IM,JM)
c                LUNPLT = 30
c                open(unit=LUNPLT,file='PSPLOT.OUT')
c                WRITE(LUNPLT,9) ((ZPLOT(I,J),I=1,IM),J=2*JM,1,-1)
c   9            FORMAT(13f6.0)
c                close(unit=LUNPLT)
c
        RETURN
        end
        SUBROUTINE READPS(LUN,ARRAY,IM,JM)

c*********************************************************************72
c
cc READPS reads a scalar field on a staggered E-Grid, 
c       as it appears on the printed page.
C       THIS VERSION CONVERTS SURFACE PRESSURE FROM 
C                10 X mm HG to Pascals.
c
        REAL ARRAY(IM,JM)
        CHARACTER*80 HEADER
c
c       FLUSH OUT the HEADER RECORD
        READ(LUN,9901) HEADER 
        print9901, HEADER
 9901   FORMAT(A80)
c
        IMM1 = IM-1
c
        DO J=JM,1,-1
          READ(LUN,*) ( ARRAY(I,J) , I=2,IMM1,2 )
          READ(LUN,*) ( ARRAY(I,J) , I=1,IM  ,2 )
ccc              print98, ( ARRAY(I,J) , I=2,IMM1,2 )
ccc              print99, ( ARRAY(I,J) , I=1,IM  ,2 )
   98            FORMAT(13(6x,f6.0))
   99            FORMAT(13(f6.0,6x))
        end do
c
        DO J=1,JM
        DO I=1,IM
           ARRAY(I,J) = (1.33322 * ARRAY(I,J)/10.) * 100.
        end do
        end do
c
        DO J=JM,1,-1
          print998, ( ARRAY(I,J) , I=2,IMM1,2 )
          print999, ( ARRAY(I,J) , I=1,IM  ,2 )
  998            FORMAT(13(6x,-2pf6.0))
  999            FORMAT(13(-2pf6.0,6x))
        end do
C
        RETURN
        END
        SUBROUTINE READO(LUN,OROG,IM,JM)

c*********************************************************************72
c
CC READO reads a scalar field on a fine A-Grid, 
c
        REAL OROG(IM,2*JM)
        CHARACTER*80 HEADER

        IMM1 = IM-1
        JM2 = 2*JM
c
c       FLUSH OUT the HEADER RECORD
        READ(LUN,9901) HEADER 
        print9901, HEADER
 9901   FORMAT(A80)
        DO J=JM2,1,-1
          READ(LUN,*) ( OROG(I,J) , I=1,IM )
                 print99, ( OROG(I,J) , I=1,IM )
   99            FORMAT(13(f6.0))
        end do
c
C       Smooth the Orography from the Fine A-Grid to the E-Grid.
        print*,' OROG(7,6), OROG(7,7) Before Smoothing ', 
     X            OROG(7,6),OROG(7,7)
        call SMEARZ(OROG,OROG,IM,JM)
        print*,' OROG(7,6), OROG(7,7) After  Smoothing ', 
     X            OROG(7,6),OROG(7,7)
C
C       printout the Smoothed Orography.
        print9902
 9902   FORMAT(' Orography After Smoothing')
        DO J=JM2,1,-1
           print99, ( OROG(I,J) , I=1,IM )
        end do

        RETURN
        END
        SUBROUTINE READZ(LUN,ARRAY,IM,JM,LM)

c*********************************************************************72
c
cc READZ reads a scalar field on a Sparse A-Grid,
c       as it appears on the printed page,
c       and fill it out to an E-grid.
c
        REAL ARRAY(IM,JM,LM)
        CHARACTER*80 HEADER
c
        IMM1 = IM-1
c
c       Loop for each level
        DO 1000 L=1,LM
c
c       FLUSH OUT the HEADER RECORD
        READ(LUN,9901) HEADER 
        print9901, HEADER
 9901   FORMAT(A80)
        DO J=JM,1,-1
          READ(LUN,*) ( ARRAY(I,J,L) , I=1,IM  ,2 )
                 print99, ( ARRAY(I,J,L) , I=1,IM  ,2 )
   99            FORMAT(13(f6.0,6x))
        end do
c
c       Fill the E-grid by interpolation.
c       print9991
c       DO J=JM,1,-1
c       print9993, (array(I,J,L),I=2,IMM1,2) 
c       print9994, (array(I,J,L),I=1,IM,2)
c       end do
c
        CALL ATOEZ(ARRAY(1,1,L),IM,JM)
c
c       print9992
c       DO J=JM,1,-1
c       print9993, (array(I,J,L),I=2,IMM1,2)
c       print9994, (array(I,J,L),I=1,IM,2)
c       end do
c
c9991   FORMAT(' Z BEFOR ATOEZ ')
c9992   FORMAT(' Z AFTER ATOEZ ')
c9993   FORMAT(3X,6(1X,F8.0))
c9994   FORMAT(7(F8.0,1X))
c
 1000   CONTINUE
c
        RETURN
        END
        SUBROUTINE READT(LUN,TTOP,IM,JM)

c*********************************************************************72
c
cc READT reads the thickness 200-100mb height field on a 
c       Sparse A-Grid,and convert thickness to 
c       stratospheric temperature.
c
        REAL TTOP(IM,JM)
        CHARACTER*80 HEADER
c
        GRAV = 9.80
        RGAS = 287.
        DELTAP = (200.-100.) * 100.
        PBAR = 150. * 100.
        FACTOR = ( GRAV*PBAR ) / ( RGAS*DELTAP )
C
        IMM1 = IM-1
c
c       FLUSH OUT the HEADER RECORD
        READ(LUN,9901) HEADER 
        print9901, HEADER
 9901   FORMAT(A80)
        DO J=JM,1,-1
          READ(LUN,*) ( TTOP(I,J) , I=1,IM  ,2 )
                 print99, ( TTOP(I,J) , I=1,IM  ,2 )
   99            FORMAT(13(f6.0,6x))
        end do
c
c       Fill the E-grid by interpolation.
        CALL ATOEZ(TTOP,IM,JM)
c
C       Convert thickness to temperature
        DO J=1,JM
        DO I=1,IM
          TTOP(I,J) = FACTOR*TTOP(I,J)
        end do
        end do

               DO J=JM,1,-1
                 print9993, (TTOP(I,J),I=2,IMM1,2)
                 print9994, (TTOP(I,J),I=1,IM,2)
 9993            FORMAT(3X,6(1X,F8.0))
 9994            FORMAT(7(F8.0,1X))
               end do

        RETURN
        END
        SUBROUTINE READV(LUN,U,V,DDDFF,IM,JM,LM)

c*********************************************************************72
c
cc READV reads a vector field on a Sparse A-Grid,
c       as it appears on the printed page,
c       and fill it out to an E-grid.
C       Values input as ddd.ff and split into u,v.
c
        REAL U(IM,JM,LM),V(IM,JM,LM), DDDFF(IM,JM)
        CHARACTER*80 HEADER
c
        IMM1 = IM-1
c
c       Loop over each level
        DO 1000 L=1,LM
c
c       FLUSH OUT the HEADER RECORD
        READ(LUN,9901) HEADER 
        print9901, HEADER
 9901   FORMAT(A80)
        DO J=JM,1,-1
          READ(LUN,*) ( DDDFF(I,J) , I=1,IM,2 )
                 print98, ( DDDFF(I,J), I=1,IM,2 )
   98            FORMAT(7f7.2)
        end do
c
        drad = 3.14159265/180.
        DO J=JM,1,-1
        DO I=1,IM,2
          df = DDDFF(I,J)
          dir= INT(df)
          speed = 100*(df-dir)
          angle = 270 - dir
          if (angle.lt.0) angle = angle+360
          rangle = angle*drad
          uu = speed*cos(rangle)
          vv = speed*sin(rangle)
          U(I,J,L) = uu
          V(I,J,L) = vv
c         print9, i,j, dir,angle,speed,uu,vv
    9     format('ij dir angle speed uv',2I3,3F6.0,4X,2F4.0)
        end do
        end do
c
c       Fill the E-grid by interpolation.
c       print9991
c       DO J=JM,1,-1
c       print9995, (U(I,J,L),I=1,IM,2) , (U(I,J,L),I=2,IMM1,2)
c       end do
c       print9992
c       DO J=JM,1,-1
c       print9995, (V(I,J,L),I=1,IM,2) , (V(I,J,L),I=2,IMM1,2)
c       end do
c
        CALL ATOEV(U(1,1,L),V(1,1,L),IM,JM)
c
c       print9993
c       DO J=JM,1,-1
c       print9995, (U(I,J,L),I=1,IM,2) , (U(I,J,L),I=2,IMM1,2)
c       end do
c       print9994
c       DO J=JM,1,-1
c       print9995, (V(I,J,L),I=1,IM,2) , (V(I,J,L),I=2,IMM1,2)
c       end do
c
c9991   FORMAT(' U BEFOR ATOEV ')
c9992   FORMAT(' V BEFOR ATOEV ')
c9993   FORMAT(' U AFTER ATOEV ')
c9994   FORMAT(' V AFTER ATOEV ')
c9995   FORMAT( (6(F5.1,5X),F5.1/6(5X,F5.1)) )
c
 1000   CONTINUE
c
        RETURN
        END
        SUBROUTINE ATOEZ(Z,IM,JM)

c*********************************************************************72
c
cc ATOEZ uses bilinear interpolation to transfer data to a denser A grid.
c
c       From a scalar field on a sparse A-Grid, 
c       construct it on a denser A-grid,
c       using bilinear interpolation.
c
        REAL Z(IM,JM)
c
        IMM1 = IM-1
        JMM1 = JM-1
c
c       Internal Points.
        DO J=1,JMM1
        DO I=2,IMM1,2
          Z(I,J) = ( Z(I-1,J  )+Z(I+1,J  ) 
     X             + Z(I-1,J+1)+Z(I+1,J+1) ) / 4.
        end do
        end do
c
c       Top Row
        DO I=2,IMM1,2
          Z(I,JM) = ( Z(I-1,JM)+Z(I+1,JM)-Z(I,JMM1) )
        end do
c
        RETURN
        END
        SUBROUTINE ATOEV(U,V,IM,JM)

c*********************************************************************72
c
c       From a vector field on a sparse A-Grid, 
c       construct it on a denser A-grid,
c       using bilinear interpolation.
c
        REAL U(IM,JM), V(IM,JM)
c
        IMM1 = IM-1
        JMM1 = JM-1
c
c       Internal Points.
        DO J=2,JM
        DO I=2,IMM1,2
          U(I,J) = ( U(I-1,J  )+U(I+1,J  ) 
     X             + U(I-1,J-1)+U(I+1,J-1) ) / 4.
          V(I,J) = ( V(I-1,J  )+V(I+1,J  ) 
     X             + V(I-1,J-1)+V(I+1,J-1) ) / 4.
        end do
        end do
c
c       Bottom Row
        DO I=2,IMM1,2
          U(I,1) = ( U(I-1,1)+U(I+1,1)-U(I,2) )
          V(I,1) = ( V(I-1,1)+V(I+1,1)-V(I,2) )
        end do
c
        RETURN
        END
        SUBROUTINE FILLZ(Z,Z2,IM,JM)

c*********************************************************************72
c
c       From a scalar field on a staggered E-Grid, 
c       construct another field on a denser A-grid.
c       Interpolation is linear at edges
c       and bilinear for internal points.
c
        REAL Z(IM,JM), Z2(IM,2*JM)
c
        IMM1 = IM-1
        IMM2 = IM-2
        JMM1 = JM-1
        JMX2 = 2*JM
c
c       First, fill in the known points.
        DO J=1,JM
          DO I=1,IM,2
            Z2(I,J*2-1) =  Z(I  ,J)
          end do
          DO I=2,IMM1,2
            Z2(I,J*2  ) =  Z(I  ,J)
          end do
        end do
c
C       Next, interpolate the unknown points.
c
c       Bottom Row.
        DO I=2,IMM1,2
          Z2(I,1) = ( Z(I-1,1)+Z(I+1,1) ) /2.
        end do
c
c       Top Row.
        Z2(1 ,JMX2) = ( Z(1 ,JM)+Z(2   ,JM) ) /2.
        DO I=3,IMM2,2
          Z2(I,JMX2) = ( Z(I-1,JM)+Z(I+1,JM) ) /2.
        end do
        Z2(IM,JMX2) = ( Z(IMM1,JM)+Z(IM,JM) ) /2.
c
c       Left and Right Sides
        DO J=1,JMM1
          Z2(1 ,J*2) = ( Z(1 ,J)+Z(1 ,J+1) ) /2.
          Z2(IM,J*2) = ( Z(IM,J)+Z(IM,J+1) ) /2.
        end do
c
c       Internal Points.
        DO J=2,JM
        DO I=2,IMM1,2
          Z2(I,J*2-1) = ( Z(I  ,J)+Z(I,J-1) 
     X                     + Z(I+1,J)+Z(I-1,J) ) / 4.
        end do
        end do
        DO J=1,JMM1
        DO I=3,IMM2,2
          Z2(I,J*2  ) = ( Z(I  ,J)+Z(I,J+1) 
     X                     + Z(I+1,J)+Z(I-1,J) ) / 4.
        end do
        end do
c
        RETURN
        END
        SUBROUTINE FILLV(V,V2,IM,JM)

c*********************************************************************72
c
c       From a vector field on a staggered E-Grid, 
c       construct another field on a denser A-grid.
c       Interpolation is linear at edges
c       and bilinear for internal points.
c
        REAL V(IM,JM), V2(IM,2*JM)
c
        IMM1 = IM-1
        IMM2 = IM-2
        JMM1 = JM-1
        JMX2 = 2*JM
c
c       First, fill in the known points.
        DO J=1,JM
          DO I=1,IM,2
            V2(I,J*2  ) =  V(I  ,J)
          end do
          DO I=2,IMM1,2
            V2(I,J*2-1) =  V(I  ,J)
          end do
        end do
c
C       Next, interpolate the unknown points.
c
c       Top Row.
        DO I=2,IMM1,2
          V2(I,JMX2) = ( V(I-1,JM)+V(I+1,JM) ) /2.
        end do
c
c       Bottom Row.
        V2(1 ,1) = ( V(1 ,1)+V(2   ,1) ) /2.
        DO I=3,IMM2,2
         V2(I,1) = ( V(I-1,1)+V(I+1,1) ) /2.
        end do
        V2(IM,1) = ( V(IMM1,1)+V(IM,1) ) /2.
c
c       Left and Right Sides
        DO J=2,JM
          V2(1 ,J*2-1) = ( V(1 ,J)+V(1 ,J-1) ) /2.
          V2(IM,J*2-1) = ( V(IM,J)+V(IM,J-1) ) /2.
        end do
c
c       Internal Points.
        DO J=1,JMM1
        DO I=2,IMM1,2
          V2(I,J*2  ) = ( V(I  ,J)+V(I,J+1) 
     X                  + V(I+1,J)+V(I-1,J) ) / 4.
        end do
        end do
        DO J=2,JM
        DO I=3,IMM2,2
          V2(I,J*2-1) = ( V(I  ,J)+V(I,J-1) 
     X                  + V(I+1,J)+V(I-1,J) ) / 4.
        end do
        end do
c
        RETURN
        END
        SUBROUTINE ZTOP(PS,Z,TTOP,OROG,P,IM,JM,LM)

c*********************************************************************72
C
cc ZTOP gets the initial pressures. 
C
        REAL PS(IM,JM),TTOP(IM,JM)
        REAL OROG(IM,2*JM)
        REAL Z(IM,JM,LM-1)
        REAL P(IM,JM,LM)
c
        integer VMETHOD
        DATA VMETHOD /3/
C
c       Conventional interface levels (km).
        real PBAR(5),TBAR(5), ZBAR(5), ZDYNAM(5)
        DATA PBAR /   200.,   400.,   600.,   800.,  1013./
        DATA TBAR /   -55.,   -32.,   -12.,    +2.,   +15./
        DATA ZBAR /   11.8,    7.2,   4.2 ,   2.0 ,   0.0 /
c       change to 'dynamic kilometers" (LFR, p 181).
        DATA ZDYNAM / 11.543,  7.048,  4.113,  1.959,   0.0 /
c
        IMM1 = IM-1
        LMM1 = LM-1
c
        GRAV=9.80
        RGAS=287.
c
c       convert upper heights to pressures.
        DO 100 L=1,LMM1
        ZL = ZDYNAM(L)*1000.
        TL = TBAR(L)+273.
        PL = PBAR(L)*100.
        HL = RGAS*TL/GRAV         
          print*,' ZDYNAM, PBAR, TBAR : ', ZL,PL,TL
        DO 100 J=1,JM
        DO 100 I=1,IM 
          P(I,J,L) = PL * ( 1. + (Z(I,J,L)-ZL)/HL )
  100   CONTINUE
c
c       interpolate the surface pressure.
           ZL = ZDYNAM(LM)*1000.
           TL = TBAR(LM)+273.
           PL = PBAR(LM)*100.
           print*,' ZDYNAM, PBAR, TBAR : ', ZL,PL,TL
      IF ( VMETHOD.EQ.1 ) THEN
           DO 150 J=1,JM
           DO 150 I=1,IM 
             H9 = Z(I,J,LMM1) / LOG(PS(I,J)/P(I,J,LMM1))
             if(I/2*2.EQ.I) JH=2*J
             if(I/2*2.NE.I) JH=2*J-1
             ZG = OROG(I,JH)
             P(I,J,LM) = PS(I,J) * EXP(-ZG/H9) 
  150      CONTINUE
      ELSE IF ( VMETHOD.EQ.2 ) THEN
           DO 151 J=1,JM
           DO 151 I=1,IM 
             PMID = (PS(I,J)+P(I,J,LMM1))/2.
             PDIF = (PS(I,J)-P(I,J,LMM1))
             ZDIF = (Z(I,J,LMM1)-0.)
             H9 = (PMID/PDIF)*ZDIF
             if(I/2*2.EQ.I) JH=2*J
             if(I/2*2.NE.I) JH=2*J-1
             ZG = OROG(I,JH)
             DELZ = Z(I,J,LMM1)-ZG
             QQ=DELZ/(2*H9)
             P(I,J,LM) = P(I,J,LMM1) * (1+QQ)/(1-QQ)
  151      CONTINUE
      ELSE IF ( VMETHOD.EQ.3 ) THEN
           TZERO = 273. + 15.
           PZERO = 1013.25 * 100.
           GG = 9.80665
           RR = 287.
           GAM = 0.0065
           PWR = GG/(GAM*RR)
           DO 1511 J=1,JM
           DO 1511 I=1,IM 
             if(I/2*2.EQ.I) JH=2*J
             if(I/2*2.NE.I) JH=2*J-1
             ZG = OROG(I,JH)
             DELZ = ZG
             P(I,J,LM) = PS(I,J) * ( 1-(GAM/TZERO)*DELZ )**(+PWR)
CCC          print*,'P CHECK ', P(I,J,LM) , PS(I,J) 
 1511      CONTINUE
      ENDIF
c
        DO 200 L=1,LM
        print97,L
   97   FORMAT(' INTERFACE PRESSURES, Level: ',I4)
        DO J=JM,1,-1
           print98, ( P(I,J,L) , I=2,IMM1,2 )
           print99, ( P(I,J,L) , I=1,IM  ,2 )
   98      FORMAT(13(6x,-2pf6.0))
   99      FORMAT(13(-2pf6.0,6x))
        end do
  200   CONTINUE
c
        RETURN
        END
        SUBROUTINE PTOZ(P2,TTOP2,OROG, Z2,PSEA2,IM,JM2,LM)

c*********************************************************************72
C
cc PTOZ gets the sea-level pressure and upper heights. 
C
        REAL PSEA2(IM,JM2),TTOP2(IM,JM2)
        REAL OROG(IM,JM2)
        REAL Z2(IM,JM2,LM-1)
        REAL P2(IM,JM2,LM)
c
        integer VMETHOD
        DATA VMETHOD /3/
C
c       Conventional interface levels (km).
        real PBAR(5),TBAR(5), ZBAR(5), ZDYNAM(5)
        DATA PBAR /   200.,   400.,   600.,   800.,  1013./
        DATA TBAR /   -55.,   -32.,   -12.,    +2.,   +15./
        DATA ZBAR /   11.8,    7.2,   4.2 ,   2.0 ,   0.0 /
c       change to 'dynamic kilometers" (LFR, p 181).
        DATA ZDYNAM / 11.543,  7.048,  4.113,  1.959,   0.0 /
c
        IMM1 = IM-1
        LMM1 = LM-1
        JM = JM2/2
c
        GRAV=9.80
        RGAS=287.
c
c       convert upper pressures to heights 
        DO 100 L=1,LMM1
        ZL = ZDYNAM(L)*1000.
        TL = TBAR(L)+273.
        PL = PBAR(L)*100.
        HL = RGAS*TL/GRAV         
CCC       print*,' PTOZ:: ZDYNAM, PBAR, TBAR : ', ZL,PL,TL
        DO 100 J=1,JM2
        DO 100 I=1,IM 
          Z2(I,J,L) = ZL + (P2(I,J,L)/PL - 1.)*HL
  100   CONTINUE
c
c       interpolate the surface pressure.
        ZL = ZDYNAM(LM)*1000.
        TL = TBAR(LM)+273.
        PL = PBAR(LM)*100.
CCC     print*,' PTOZ:: ZDYNAM, PBAR, TBAR : ', ZL,PL,TL
      IF ( VMETHOD.EQ.1 ) THEN
          DO 150 J=1,JM2
          DO 150 I=1,IM 
            ZG = OROG(I,J)
            H9 = (Z2(I,J,LMM1)-ZG) / LOG(P2(I,J,LM)/P2(I,J,LMM1))
            PSEA2(I,J) = P2(I,J,LM) * EXP(+ZG/H9) 
  150     CONTINUE
      ELSE IF ( VMETHOD.EQ.2 ) THEN
          DO 151 J=1,JM2
          DO 151 I=1,IM 
             PMID = (P2(I,J,LM)+P2(I,J,LMM1))/2.
             PDIF = (P2(I,J,LM)-P2(I,J,LMM1))
             ZG = OROG(I,J)
             ZDIF = (Z2(I,J,LMM1)-ZG)
             H9 = (PMID/PDIF)*ZDIF
             DELZ = Z2(I,J,LMM1)-0.
             QQ=DELZ/(2*H9)
             PSEA2(I,J) = P2(I,J,LMM1) * (1+QQ)/(1-QQ)
  151     CONTINUE
      ELSE IF ( VMETHOD.EQ.3 ) THEN
          TZERO = 273. + 15.
          PZERO = 1013.25 * 100.
          GG = 9.80665
          RR = 287.
          GAM = 0.0065
          PWR = GG/(GAM*RR)
          DO 1511 J=1,JM2
          DO 1511 I=1,IM 
             ZG = OROG(I,J)
             DELZ = ZG
             PSEA2(I,J) = P2(I,J,LM) * (1-(GAM/TZERO)*DELZ)**(-PWR)
CCC          print*,'P CHECK ', P2(I,J,LM) , PSEA2(I,J) 
 1511     CONTINUE
      ENDIF
c
C       INTERPOLATE FROM E- TO A-GRID.
        call SMEARZ(PSEA2,PSEA2,IM,JM)
C
c       Calculate the 1000 mb geopotential. 
        PL = 1000. * 100.
        TL = TBAR(LM)+273.
        HS = RGAS*TL/GRAV
        DZ = HS/PL 
CCC     print*,' Z1000:: PL,TL,HS,DZ:', PL,TL,HS,DZ
          DO 152 J=1,JM2
          DO 152 I=1,IM 
            Z2(I,J,LM) = ( PSEA2(I,J)-PL ) * DZ 
  152     CONTINUE
c
        print98
   98   FORMAT(' SEA LEVEL PRESSURE ')
        DO J=JM2,1,-1
           print91, ( PSEA2(I,J) , I=1,IM)
   91      FORMAT(13(-2pf6.0))
        end do
c
        RETURN
c
        DO 200 L=1,LM
        print97,L
   97   FORMAT(' INTERFACE HEIGHTS, Level: ',I4)
        DO J=JM2,1,-1
           print99, ( Z2(I,J,L) , I=1,IM)
   99      FORMAT(13(f6.0))
        end do
  200   CONTINUE
c
        RETURN
        END
        SUBROUTINE UVTOMM(U,V,P,MU,MV,IM,JM,LM)

c*********************************************************************72
C
cc UVTOMM gets the initial momenta. 
c
      PARAMETER (IX=13, JX=7, LX=5)

      REAL MU(IM,JM,LM)
      REAL MV(IM,JM,LM)
      REAL P(IM,JM,LM)
      REAL U(IM,JM,LM)
      REAL V(IM,JM,LM)

      COMMON /WORK/ WORK1(IX,JX), WORK2(IX,JX), WORK3(IX,JX)

      GRAV = 9.80
c
c  convert velocities to momenta.
c
      DO L = 1, LM
c
c  Get dp/g and interpolate to wind points.
c
        DO J = 1, JM
          DO I = 1, IM 
            IF ( L .EQ. 1 ) THEN
              RR = P(I,J,1) / GRAV
            ELSE
              RR = ( P(I,J,L) - P(I,J,L-1) ) / GRAV
            END IF
            WORK1(I,J) = RR
          end do
        end do

        CALL ZTOV(WORK1,WORK2,IM,JM)

        DO J=1,JM
          DO I=1,IM 
            RR = WORK2(I,J)
            MU(I,J,L) = RR * U(I,J,L) 
            MV(I,J,L) = RR * V(I,J,L) 
          end do
        end do

      end do

      do L = 1, LM

        print96,L
   96   FORMAT(' LAYER MOMENTA (MU) , Level: ',I4)
c       write ( *, * ) 'Eat my monkey!'
        DO J=JM,1,-1
c         write ( *, * ) 'Feed my monkey!'
          print99, (MU(I,J,L) , I=1,IM  ,2 )
          write ( * , * ) j
          print98, (MU(I,J,L) , I=2,IMM1,2 )
   99     FORMAT(13(-2pf6.0,6x))
   98     FORMAT(13(6x,-2pf6.0))
CCCCCCCC   these scaling factors make printed values
CCCCCCCC   "look the same size" as in LFR's table.
        END DO

        print97,L
   97   FORMAT(' LAYER MOMENTA (MV) , Level: ',I4)

        DO J=JM,1,-1
          print99, (MV(I,J,L) , I=1,IM  ,2 )
          print98, (MV(I,J,L) , I=2,IMM1,2 )
        end do

      end do

      RETURN
      END
        SUBROUTINE MMTOUV(P2,MU2,MV2, U2,V2,IM,JM2,LM)

c*********************************************************************72
C
cc MMTOUV gets the velocities from momenta. 
c
c
c      2-D Work fields common to all modules.
       PARAMETER (IX=13, JX=7, LX=5)
       COMMON /WORK/ WORK1(IX,JX), WORK2(IX,JX), WORK3(IX,JX)
c
C
        REAL P2(IM,JM2,LM), MU2(IM,JM2,LM), MV2(IM,JM2,LM)
        REAL U2(IM,JM2,LM), V2(IM,JM2,LM)
c
        GRAV=9.80
c
c       convert momenta to velocities.
c
        DO 300 L=1,LM
c
        DO 100 J=1,JM2
        DO 100 I=1,IM 
          IF ( L.EQ.1 ) THEN
             RR = P2(I,J,1)/GRAV
          ELSE
             RR = ( P2(I,J,L)-P2(I,J,L-1) )/GRAV
          ENDIF
          U2(I,J,L) = MU2(I,J,L) / RR
          V2(I,J,L) = MV2(I,J,L) / RR
  100   CONTINUE
c
  300   CONTINUE
c
        RETURN
c
        DO 400 L=1,LM
        print96,L
   96   FORMAT(' LAYER VELOCITY (U) , Level: ',I4)
        DO J=JM2,1,-1
           print99, (U2(I,J,L) , I=1,IM)
   99      FORMAT(13(f6.0))
        end do
        print97,L
   97   FORMAT(' LAYER VELOCITY (V) , Level: ',I4)
        DO J=JM2,1,-1
           print99, (V2(I,J,L) , I=1,IM)
        end do
  400   CONTINUE
c
        RETURN
        END
        SUBROUTINE ZTOV(Z,ZV,IM,JM)

c*********************************************************************72
c
c       From a scalar field on a staggered E-Grid, 
c       construct values at the vector points.
c       Interpolation is linear at edges
c       and bilinear for internal points.
c
        REAL Z(IM,JM), ZV(IM,JM)
c
        IMM1 = IM-1
        IMM2 = IM-2
        JMM1 = JM-1
c
c       Bottom Row.
        DO I=2,IMM1,2
          ZV(I,1) = ( Z(I-1,1)+Z(I+1,1) ) /2.
        end do
c
c       Top Row.
        ZV(1 ,JM) = ( Z(1 ,JM)+Z(2   ,JM) ) /2.
        DO I=3,IMM2,2
          ZV(I,JM) = ( Z(I-1,JM)+Z(I+1,JM) ) /2.
        end do
        ZV(IM,JM) = ( Z(IMM1,JM)+Z(IM,JM) ) /2.
c
c       Left and Right Sides
        DO J=1,JMM1
          ZV(1 ,J) = ( Z(1 ,J)+Z(1 ,J+1) ) /2.
          ZV(IM,J) = ( Z(IM,J)+Z(IM,J+1) ) /2.
        end do
c
c       Internal Points.
        DO J=2,JM
        DO I=2,IMM1,2
          ZV(I,J) = ( Z(I  ,J)+Z(I,J-1) 
     X                + Z(I+1,J)+Z(I-1,J) ) / 4.
        end do
        end do
        DO J=1,JMM1
        DO I=3,IMM2,2
          ZV(I,J) = ( Z(I  ,J)+Z(I,J+1) 
     X                + Z(I+1,J)+Z(I-1,J) ) / 4.
        end do
        end do
c
        RETURN
        END
        SUBROUTINE SMEAR

c*********************************************************************72
C
C       CHECK routine to test SMEARZ and SMEARV.
c
c      2-D Work fields on A-grid.
       PARAMETER ( IM=13, JM=7, JM2=14 )
       COMMON /WORKK/ WORKK1(IM,JM2), WORKK2(IM,JM2), 
     +                WORKK3(IM,JM2), WORKK4(IM,JM2)
C
C       CHECK ACTION OF SMEARZ.
        DO I=1,IM
        DO J=1,JM*2
        WORKK1(I,J) =0.0 
        WORKK2(I,J) =0.0 
        end do
        end do
c
        DO I=1,IM,2
        DO J=1,JM*2-1,2
        WORKK1(I,J) = FLOAT(I)+FLOAT(J)/100.
        end do
        end do
        DO I=2,IM-1,2
        DO J=2,JM*2,2
        WORKK1(I,J) = FLOAT(I)+FLOAT(J)/100.
        end do
        end do
c
        print* , ' WORKK1 '
        print9929, (( WORKK1(I,J), I=1,IM ), J=JM2,1,-1 )
        CALL SMEARZ(WORKK1,WORKK1,IM,JM)
        print* , ' SMEARZ '
        print9929, (( WORKK1(I,J), I=1,IM ), J=JM2,1,-1 )
 9929   FORMAT( 13f6.2 )
C
c
C       CHECK ACTION OF SMEARV.
        DO I=1,IM
        DO J=1,JM*2
        WORKK3(I,J) =0.0 
        WORKK4(I,J) =0.0 
        end do
        end do
c
        DO J=1,JM*2-1,2
        DO I=2,IM-1,2
        WORKK3(I,J) = FLOAT(I)+FLOAT(J)/100.
        end do
        end do
        DO J=2,JM*2,2
        DO I=1,IM,2
        WORKK3(I,J) = FLOAT(I)+FLOAT(J)/100.
        end do
        end do
c
        print* , ' WORKK3 '
        print9929, (( WORKK3(I,J), I=1,IM ), J=JM2,1,-1 )
        CALL SMEARV(WORKK3,WORKK3,IM,JM)
        print* , ' SMEARV '
        print9929, (( WORKK3(I,J), I=1,IM ), J=JM2,1,-1 )
C
        RETURN
        END
        SUBROUTINE SMEARZ(Z1,Z2,IM,JM)

c*********************************************************************72
c
c       From a scalar field on a dense A-grid,
c       construct another field on the same grid 
c       where values at vector points are averaged
c       Interpolation is linear at edges and bilinear 
c       for internal points.
c
        REAL Z1(IM,2*JM), Z2(IM,2*JM)
c
        IMM1 = IM-1
        IMM2 = IM-2
        JMM1 = JM-1
        JM2 = 2*JM
        JM2M1 = 2*JM-1
        JM2M2 = 2*JM-2
c
c       First, fill in the E-grid scalar points.
        DO J=1,JM2M1,2
          DO I=1,IM,2
            Z2(I,J) =  Z1(I,J)
          end do
        end do
        DO J=2,JM2,2
          DO I=2,IMM1,2
            Z2(I,J) =  Z1(I,J)
          end do
        end do
c
C       Next, interpolate the intermediate points.
c
c       Bottom Row.
        DO I=2,IMM1,2
          Z2(I,1) = ( Z1(I-1,1)+Z1(I+1,1) ) /2.
        end do
c
c       Top Row.
        Z2(1 ,JM2) = ( Z1(1 ,JM2M1)+Z1(2   ,JM2) ) /2.
        DO I=3,IMM2,2
          Z2(I,JM2) = ( Z1(I-1,JM2)+Z1(I+1,JM2) ) /2.
        end do
        Z2(IM,JM2) = ( Z1(IMM1,JM2)+Z1(IM,JM2M1) ) /2.
c
c       Left and Right Sides
        DO J=2,JM2M2,2
          Z2(1 ,J) = ( Z1(1 ,J-1)+Z1(1 ,J+1) ) /2.
          Z2(IM,J) = ( Z1(IM,J-1)+Z1(IM,J+1) ) /2.
        end do
c
c       Internal Points.
        DO J=2,JM2M2,2
        DO I=3,IMM2,2
          Z2(I,J) = ( Z1(I-1,J)+Z1(I+1,J) 
     X              + Z1(I,J-1)+Z1(I,J+1) ) / 4.
        end do
        end do
        DO J=3,JM2M1,2
        DO I=2,IMM1,2
          Z2(I,J) = ( Z1(I-1,J)+Z1(I+1,J) 
     X              + Z1(I,J-1)+Z1(I,J+1) ) / 4.
        end do
        end do
c
        RETURN
        END
        SUBROUTINE SMEARV(Z1,Z2,IM,JM)

c*********************************************************************72
c
c       From a vector field on a dense A-grid,
c       construct another field on the same grid 
c       where values at scaler points are averaged
c       Interpolation is linear at edges and bilinear 
c       for internal points.
c
        REAL Z1(IM,2*JM), Z2(IM,2*JM)
c
        IMM1 = IM-1
        IMM2 = IM-2
        JMM1 = JM-1
        JM2 = 2*JM
        JM2M1 = 2*JM-1
        JM2M2 = 2*JM-2
c
c       First, fill in the E-grid vector points.
        DO J=1,JM2M1,2
          DO I=2,IMM1,2
            Z2(I,J) =  Z1(I,J)
          end do
        end do
        DO J=2,JM2,2
          DO I=1,IM,2
            Z2(I,J) =  Z1(I,J)
          end do
        end do
c
C       Next, interpolate the intermediate points.
c
c       Top Row.
        DO I=2,IMM1,2
          Z2(I,JM2) = ( Z1(I-1,JM2)+Z1(I+1,JM2) ) /2.
        end do
c
c       Bottom Row.
        Z2(1 ,1) = ( Z1(1 ,2)+Z1(2   ,1) ) /2.
        DO I=3,IMM2,2
          Z2(I,1) = ( Z1(I-1,1)+Z1(I+1,1) ) /2.
        end do
        Z2(IM,1) = ( Z1(IMM1,1)+Z1(IM,2) ) /2.
c
c       Left and Right Sides
        DO J=3,JM2M1,2
          Z2(1 ,J) = ( Z1(1 ,J-1)+Z1(1 ,J+1) ) /2.
          Z2(IM,J) = ( Z1(IM,J-1)+Z1(IM,J+1) ) /2.
        end do
c
c       Internal Points.
        DO J=2,JM2M2,2
        DO I=2,IMM1,2
          Z2(I,J) = ( Z1(I-1,J)+Z1(I+1,J) 
     X              + Z1(I,J-1)+Z1(I,J+1) ) / 4.
        end do
        end do
        DO J=3,JM2M1,2
        DO I=3,IMM2,2
          Z2(I,J) = ( Z1(I-1,J)+Z1(I+1,J) 
     X              + Z1(I,J-1)+Z1(I,J+1) ) / 4.
        end do
        end do

        RETURN
        END
        SUBROUTINE PSFILT(PS2,OROG, PSEA2,IM,JM2,
     X            ISWEST,ISEAST,JSSOUTH,JSNORTH, EpSpace)

c*********************************************************************72
C
c       Filter the pressure at the lowest level: 
C          (1) Convert Surface pressure to MSL Pressure
C          (2) Filter that field
C          (3) Convert MSL pressure to Surface Pressure
C      (Note: PS2 is P2 at the level LM: P2(1,1,LM) in CALL.
C
        REAL PS2(IM,JM2)
        REAL PSEA2(IM,JM2)
        REAL OROG(IM,JM2)
C
c       print95
   95   FORMAT(' UP-N-DOWN SURF PRESSURES, BEFOR  ')
        DO J=JM2,1,-1
c          print98, ( PS2(I,J) , I=1,IM )
   98      FORMAT(13(-2pf6.0))
        end do
c
c       interpolate the surface pressure.
          TZERO = 273. + 15.
          PZERO = 1013.25 * 100.
          GG = 9.80665
          RR = 287.
          GAM = 0.0065
          PWR = GG/(GAM*RR)
c
          DO 150 J=1,JM2
          DO 150 I=1,IM 
             ZG = OROG(I,J)
             DELZ = ZG
             PSEA2(I,J) = PS2(I,J) * (1-(GAM/TZERO)*DELZ)**(-PWR)
c            print*,'P CHECK ', PS2(I,J) , PSEA2(I,J) 
 150      CONTINUE
c
c       print995
  995   FORMAT(' UP-N-DOWN MSL. PRESSURES, BEFOR  ')
        DO J=JM2,1,-1
c          print998, ( PSEA2(I,J) , I=1,IM )
  998      FORMAT(13(-2pf6.0))
        end do
C
C - - -  Call the Filtering Routine.
             IMM1 = IM-1
             JM2M2 = JM2-2 
             CALL IMPFIL(PSEA2,PSEA2,IM,JM2,IM,JM2,
     X            ISWEST,ISEAST,JSSOUTH,JSNORTH,EpSpace)
c
c       interpolate the surface pressure.
           DO 151 J=1,JM2
           DO 151 I=1,IM 
             ZG = OROG(I,J)
             DELZ = ZG
             PS2(I,J) = PSEA2(I,J) * ( 1-(GAM/TZERO)*DELZ )**(+PWR)
CCC          print*,'P CHECK ', PS2(I,J) , PSEA2(I,J) 
 151       CONTINUE
c
c       print97
   97   FORMAT(' UP-N-DOWN SURF PRESSURES, AFTER  ')
        DO J=JM2,1,-1
c          print99, ( PS2(I,J) , I=1,IM )
   99      FORMAT(13(-2pf6.0))
        end do
c       print997
  997   FORMAT(' UP-N-DOWN MSL. PRESSURES, AFTER  ')
        DO J=JM2,1,-1
c          print999, ( PSEA2(I,J) , I=1,IM )
  999      FORMAT(13(-2pf6.0))
        end do
c
        RETURN
        END
      SUBROUTINE HFILT2 (NMAX,NSTEPS,DT,TAUC,IWINDOW,WPARAM,HH)

c*********************************************************************72
c
c     Routine to call HFILT1 and fill up the array HH.
c     HFILT1 gets the values HH(0) to HH(NSTEPS), and it
c     is completed under the assumption that it is symmetric.

      REAL HH(-NMAX:NMAX)
c
      CALL HFILT1 (NSTEPS,DT,TAUC,IWINDOW,WPARAM,HH(0))
c
      DO 100 N=1,NSTEPS
        HH(-N) = HH(N)
  100 CONTINUE
      return
      end
      SUBROUTINE HFILT1 (NSTEPS,DT,TAUC,IWINDOW,WPARAM,HH)

c*********************************************************************72
c    
cc HFILT1 calculates filter weights with selected window.
c
c       Ref: see Hamming, R.W., 1989: Digital Filters,
c                Prentice-Hall International. 3rd Edition.
c
c       INPUT:      NSTEPS  -  Number of timesteps
c                              forward or backward.
c
c                   DT      -  Time step in seconds.
c
c                   TAUC    -  Cut-off period in hours.
c
c                   IWINDOW -  Indicator for selected window.
c
c                   WPARAM  -  Parameter (possibly) needed
c                              to specify window.
c
c
c       OUTPUT:     HH      -  Array(0:NSTEPS) with the
c                              required filter weights
c
        real HH(0:NSTEPS)
c
        data pi / 3.14159265358979 /

c------------------------------------------------------------
c
c       Windows are defined by a call and held in HH. 
        IF ( IWINDOW .eq. 0 ) CALL UNIFORM  (NSTEPS,dummy ,HH)
        IF ( IWINDOW .eq. 1 ) CALL LANCZOS  (NSTEPS,WPARAM,HH)
        IF ( IWINDOW .eq. 2 ) CALL HAMMING  (NSTEPS,WPARAM,HH)
        IF ( IWINDOW .eq. 3 ) CALL BLACKMAN (NSTEPS,dummy ,HH)
        IF ( IWINDOW .eq. 4 ) CALL KAISER   (NSTEPS,WPARAM,HH)
        IF ( IWINDOW .eq. 5 ) CALL POTTER2  (NSTEPS,WPARAM,HH)
C
        IF ( IWINDOW .eq. 50 ) CALL CRAZY1   (NSTEPS,WPARAM,HH)
        IF ( IWINDOW .eq. 60 ) CALL CRAZY2   (NSTEPS,WPARAM,HH)
        IF ( IWINDOW .eq. 70 ) CALL CRAZY3   (NSTEPS,WPARAM,HH)
        IF ( IWINDOW .eq. 80 ) CALL CRAZY4   (NSTEPS,WPARAM,HH)
c
c------------------------------------------------------------
c
c       calculate the cutoff frequency
        omegac = 2.*pi/(TAUC*3600.)
c
        NN = IABS(NSTEPS)
        DELTAT = ABS(DT)
c
        do 100 n=0,NN
          window = HH(n)
          if ( n .EQ. 0 ) then
            h = (omegac*DELTAT/pi)
          else
            h = sin(n*omegac*DELTAT)/(n*pi)
          endif
          HH(n) = h*window
  100   continue
c
c       normalize the sums to be unity
        call NORMLZ(HH,NN)
c
        return
        end
        SUBROUTINE NORMLZ(HH,NMAX)

c*********************************************************************72
c
cc NORMLZ normalizes the sum of HH to be unity
c
        REAL HH(0:NMAX)
c
        sumhh = HH(0) 
        do 100 n=1,NMAX
          sumhh = sumhh + 2*HH(n)
  100   continue
        do 200 n=0,NMAX
          HH(n)  = HH(n)/sumhh
  200   continue
c
        return
        end
      SUBROUTINE UNIFORM(NSTEPS,dummy,WW)

c*********************************************************************72
c
c     define UNIFORM or RECTANGULAR window function.
c
      real WW(0:NSTEPS)
c
      DO 100 n=0,nsteps
        WW(n) = 1. 
  100 continue
      return
      end
      SUBROUTINE LANCZOS(NSTEPS,power,WW)

c*********************************************************************72
c
c     define (Genaralised) LANCZOS window function.
c     (For the usual Lanczos window, power = 1 )
c
      real WW(0:NSTEPS)
c
        pi=4*atan(1.)
        do 100 n=0,nsteps
          if ( n .EQ. 0 ) then
            w = 1.0
          else
            w = sin(n*pi/(NSTEPS+1)) / ( n*pi/(NSTEPS+1))
          endif
          WW(n) = w**power
  100   continue
      return
      end
      SUBROUTINE HAMMING(NSTEPS,alpha,WW)

c*********************************************************************72
c
cc HAMMING defines the generalized Hamming window function.
c
c     (For the usual Hamming window, alpha=0.54,
c          for the Hann window, alpha=0.50). 
c
      real WW(0:NSTEPS)
c
        pi=4*atan(1.)
        do 100 n=0,nsteps
          if ( n .EQ. 0 ) then
            w = 1.0
          else
            w = alpha + (1-alpha)*cos(n*pi/(NSTEPS))
          endif
          WW(n) = w
  100   continue
      return
      end
      SUBROUTINE BLACKMAN(NSTEPS,dummy,WW)

c*********************************************************************72
c
cc BLACKMAN defines the Blackman window function.
c
      real WW(0:NSTEPS)
c
        pi=4*atan(1.)
        do 100 n=0,nsteps
          if ( n .EQ. 0 ) then
            w = 1.0
          else
            w = 0.42 + 0.50*cos(  n*pi/(NSTEPS))
     X                + 0.08*cos(2*n*pi/(NSTEPS))
          endif
          WW(n) = w
  100   continue
      return
      end
      SUBROUTINE KAISER(NSTEPS,alpha,WW)

c*********************************************************************72
c
cc KAISER defines the Kaiser window function.
c
      real WW(0:NSTEPS)
c
        XI0A =  bessi0(alpha)
        do 100 n=0,NSTEPS
          xn = n
          as = alpha*SQRT(1.-(xn/NSTEPS)**2)
          WW(n) = bessi0(as) / XI0A 
  100   continue
      return
      end
      FUNCTION BESSI0(X)

c*********************************************************************72
c
cc   From NUMERICAL RECIPES (Press, et al.)
c
      REAL*8 Y,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9
      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,3.5156229D0,
     *    3.0899424D0,1.2067492D0,
     *    0.2659732D0,0.360768D-1,0.45813D-2/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,0.1328592D-1,
     *    0.225319D-2,-0.157565D-2,0.916281D-2,-0.2057706D-1,
     *    0.2635537D-1,-0.1647633D-1,0.392377D-2/
      IF (ABS(X).LT.3.75) THEN
        Y=(X/3.75)**2
        BESSI0=P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7)))))
      ELSE
        AX=ABS(X)
        Y=3.75/AX
        BESSI0=(EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4
     *      +Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
      ENDIF
      RETURN
      END
      SUBROUTINE POTTER2(NSTEPS,dummy,WW)

c*********************************************************************72
c
c     define POTTER window function.
c     Modified (by me) to fall off over twice the range.
c
      real WW(0:NSTEPS), D(0:3)
      DATA D / 0.35577019, 0.2436983, 0.07211497, 0.00630165 /
      DATA PI / 3.14159265 /
c
        CK = 1.0
        do 100 n=0,NSTEPS
          IF(n.EQ.NSTEPS) CK = 0.5
          arg = PI*FLOAT(N)/FLOAT(NSTEPS)
C              MODIFICATION IN NEXT STATEMENT 
               arg = arg/2.
C              END OF MODIFICATION
          sum = D(0)
          DO 50 IP=1,3
             sum = sum + 2.*D(IP)*COS(ARG*FLOAT(IP))
   50     CONTINUE
          WW(n) = CK*sum 
  100   continue
      return
      end
      SUBROUTINE CRAZY1 (NSTEPS,alpha,WW)

c*********************************************************************72
c
c     define (Genaralised) HAMMING window function.
c     (For the usual Hamming window, alpha=0.54,
c          for the Hann window, alpha=0.50). 
c     CRAZY1 window is inverse of this.
c
      real WW(0:NSTEPS)
c
        pi=4*atan(1.)
        do 100 n=0,nsteps
          if ( n .EQ. 0 ) then
            w = 1.0
          else
            w = alpha + (1-alpha)*cos(n*pi/(NSTEPS))
          endif
          WW(n) = 1./w
  100   continue
      return
      end
      SUBROUTINE CRAZY2 (NSTEPS,gamma,WW)

c*********************************************************************72
c
c     define (Genaralised) HAMMING window function.
c     (For the usual Hamming window, alpha=0.54,
c          for the Hann window, alpha=0.50). 
c     CRAZY2 window is derived from this. 
c
      real WW(0:NSTEPS)
c
        pi=4*atan(1.)
        do 100 n=0,nsteps
          if ( n .EQ. 0 ) then
            w = 1.0
          else
            w =(1+gamma)-gamma*cos(2*n*pi/(NSTEPS))
          endif
          WW(n) = w
  100   continue
      return
      end
      SUBROUTINE CRAZY3 (NSTEPS,gamma,WW)

c*********************************************************************72
c
c     define (Genaralised) HAMMING window function.
c     (For the usual Hamming window, alpha=0.54,
c          for the Hann window, alpha=0.50). 
c     CRAZY3 window is derived from this. 
c
      real WW(0:NSTEPS)
c
        pi=4*atan(1.)
        do 100 n=0,nsteps
          if ( n .EQ. 0 ) then
            w = 1.0
          else
            w =(1+gamma)-gamma*cos(2*n*pi/(NSTEPS))
          endif
          WW(n) = 1./w
  100   continue
      return
      end
      SUBROUTINE CRAZY4 (NSTEPS,gamma,WW)

c*********************************************************************72
c
c     define (Genaralised) HAMMING window function.
c     (For the usual Hamming window, alpha=0.54,
c          for the Hann window, alpha=0.50). 
c     CRAZY4 window is derived from this. 
c
      real WW(0:NSTEPS)
c
        pi=4*atan(1.)
        do 100 n=0,nsteps
          if ( n .EQ. 0 ) then
            w = 1.0
          else
            w =(1+gamma)-gamma*cos(2*n*pi/(NSTEPS))
          endif
          WW(n) = w + 1./w
  100   continue
      return
      end
        SUBROUTINE IMPFIL(FIN,FOUT,IM,JM,IMAX,JMAX,
     +                    IWEST,IEAST,JSOUTH,JNORTH,EPSILON)

c*********************************************************************72
C
C       IMPLICIT FILTERING OF A TWO DIMENSIONAL FIELD USING
C       THE SIXTH-ORDER LOW-PASS IMPLICIT TANGENT FILTER
C       DESCRIBED IN RAYMOND, M.W.R., 116, 2132-2141.
C
C     FIN (IMAX,JMAX): ----  INPUT  FIELD
C     FOUT(IMAX,JMAX): ----  OUTPUT FIELD
C
C     IM, JM:                ACTIVE DIMENSIONS OF FIN, FOUT
C     IMAX, JMAX:      ----  MAXIMUM DIMENSIONS OF FIN, FOUT
C
C     IWEST, IEAST,    ----  LIMITS OF AREA OVER WHICH THE
C     JSOUTH,JNORTH:   ----  FILTER IS TO BE APPLIED (THESE
C                            LINES ARE UNCHANGED BY THE FILTER)
C
C     EPSILON:         ----  FILTER PARAMETER TO DETERMINE CUTOFF
C                            WHERE FILTER RESPONSE IS ONE-HALF.
C
        REAL FIN(IMAX,JMAX),FOUT(IMAX,JMAX)
        DOUBLE PRECISION FDUBL(250), EDUBL
C
C       CHECK FOR ADEQUATE WORK-SPACE DIMENSIONS
        IF (IMAX.GT.250 .OR. JMAX.GT.250) THEN
           WRITE(6,9999)
           STOP
 9999      FORMAT(' DIMENSIONS TOO LARGE IN IMPFIL ')
        END IF
C
CCC     DETERMINE CUTOFF FREQUENCY/WAVENUMBER.
CCC     PI = 2*ASIN(1.)
CCC     THETAC = 2*ATAN(EPSILON**(-1./6.))
CCC     XLENC = 2*PI/THETAC 
CCC     TYPE *,' XLENC, THETAC, EPSILON ',XLENC,THETAC,EPSILON
C
C       PARAMETER IS DOUBLE PRECISION IN LOWPAS.
        EDUBL = EPSILON
C
C       FIRST, COPY THE INPUT TO THE OUTPUT. 
        DO J=1,JM
        DO I=1,IM
            FOUT(I,J) = FIN(I,J)
        end do
        end do
C
C       NEXT, FILTER THE ROWS IN THE X-DIRECTION.
        NLEN = IEAST-IWEST+1
        DO J=JSOUTH+1,JNORTH-1
          DO I=IWEST,IEAST
            FDUBL(I-IWEST+1) = FOUT(I,J)
          end do
          CALL LOWPAS(FDUBL,NLEN,EDUBL)
          DO I=IWEST,IEAST
            FOUT(I,J) = FDUBL(I-IWEST+1)
          end do
        end do
C
C       FINALLY, FILTER THE ROWS IN THE Y-DIRECTION.
        NLEN = JNORTH-JSOUTH+1
        DO I=IWEST+1,IEAST-1
          DO J=JSOUTH,JNORTH
            FDUBL(J-JSOUTH+1) = FOUT(I,J)
          end do
          CALL LOWPAS(FDUBL,NLEN,EDUBL)
          DO J=JSOUTH,JNORTH
            FOUT(I,J) = FDUBL(J-JSOUTH+1)
          end do
        end do

        RETURN
        END
        SUBROUTINE LOWPAS(XY,N,EPS)

c*********************************************************************72
C
C       SIXTH-ORDER LOW-PASS IMPLICIT TANGENT FILTER
C       (RAYMOND, MWR, 116, 2132-2141)
C
C*************************************************************
C***     THIS CODE IS COPIED FROM A LISTING PROVIDED       ***
C***     BY WILLIAM H RAYMOND. SOME NOTATIONAL CHANGES     ***
C***     HAVE BEEN MADE IN THE ROUTINE LOWPAS. THE         ***
C***     ROUTINE INVLOW HAS BEEN COPIED ALMOST VERBATIM.   ***
C*************************************************************
C
C       XY     UNFILTERED VALUES ON INPUT
C              FILTERED VALUES ON OUTPUT.
C       N      NUMBER OF VALUES.
C       EPS    FILTER PARAMETER 
C              (DETERMINES CUTOFF)
C
C       LOCAL VARIABLE NSAVE KEPT AND COMPARED TO
C       INCOMING VALUE N. (INITIALLY SET TO ZERO).
C       IF VALUE IS UNCHANGED, ISKIP IS SET TO .TRUE.
C
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION XY(N),RHS(481),XDASH(481)
        LOGICAL ISKIP
        DATA NSAVE /0/
        DATA ISKIP /.FALSE./
C
        SAVE

        IF ( N .GT. 481 ) STOP ' Increase N in LOWPAS '
C
C
C       SKIP FIRST PART OF GAUSSIAN ELIMINATION ON REPEAT
C       CALLS IF FILTER LENGTH REMAINS UNCHANGED.
        IF (N.EQ.NSAVE) ISKIP=.TRUE.
        IF (N.NE.NSAVE) ISKIP=.FALSE.
        NSAVE = N
C
        NM1 = N-1
        NM2 = N-2
        NM3 = N-3
        NM4 = N-4
C        
        RHS(1) = 0
        RHS(N) = 0
        RHS(  2) = EPS*(XY(  1)-2.D0*XY(  2)+XY(  3))
        RHS(NM1) = EPS*(XY(NM2)-2.D0*XY(NM1)+XY(  N)) 
        RHS(  3) = EPS*(-1.D0*(XY(  1)+XY(  5))
     +                  +4.D0*(XY(  2)+XY(  4))
     +                  -6.D0* XY(  3)         )
        RHS(NM2) = EPS*(-1.D0*(XY(  N)+XY(NM4))
     +                  +4.D0*(XY(NM1)+XY(NM3))
     +                  -6.D0* XY(NM2)         )
        DO 1000 J=4,NM3
        RHS(J) = EPS*(       (XY(J-3)+XY(J+3))
     +                - 6.D0*(XY(J-2)+XY(J+2))
     +                +15.D0*(XY(J-1)+XY(J+1))
     +                -20.D0* XY(  J)         )
 1000   CONTINUE
C
C       SOLVE THE LINEAR SYSTEM FOR XDASH
        CALL INVLOW(RHS,N,XDASH,EPS,ISKIP)
C
C       ADD CORRECTION TO GET FILTERED VALUES.
        DO 2000 J=1,N
        XY(J) = XY(J) + XDASH(J)
 2000   CONTINUE
C
        RETURN
        END
        SUBROUTINE INVLOW(BB,N,XANS,EP,ISKIP)

c*********************************************************************72
C
C       GAUSSIAN ELIMINATION FOR LOW-PASS FILTER.
C
C       SIXTH-ORDER LOW-PASS IMPLICIT TANGENT FILTER.
C       (REF: WILLIAM H RAYMOND, MWR, 116, 2132-2124)
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        LOGICAL ISKIP
C
        PARAMETER (NNMAX=481)
C
        DIMENSION A(NNMAX),B(NNMAX),C(NNMAX),D(NNMAX),E(NNMAX),
     +            DELTA(NNMAX),BETA(NNMAX),W(NNMAX),GAM(NNMAX),
     +            H(NNMAX),XANS(NNMAX),BB(NNMAX),PI(NNMAX),
     +            AP(NNMAX),F(NNMAX),Z(NNMAX)
C
        SAVE
C
C---------------------------------------------------------------
C
C       SKIP INITIALIZATION OF MATRIX ON REPEAT CALLS.
        IF ( ISKIP ) GO TO 100
C
C       INITIALIZE THE MATRIX
C
        DO 10 I=4,N-3
        Z(I) = 1.D0-EP
        A(I) = 6.D0*(1.D0+EP)
        B(I) = 15.D0*(1.D0-EP)
        C(I) = 20.D0*(1.D0+EP)
        D(I) = B(I)
        E(I) = A(I)
        F(I) = Z(I)
   10   CONTINUE
C
        Z(1) = 0
        Z(2) = 0
        Z(3) = 0
C
        A(1) = 0
        A(2) = 0
        A(3) = 1.D0+EP
C
        B(1) = 0
        B(2) = 1.D0-EP
        B(3) = 4.D0*(1.D0-EP)
C
        C(1) = 1.D0
        C(2) = 2.D0*(1.D0+EP)
        C(3) = 6.D0*(1.D0+EP)
C
        D(1) = 0
        D(2) = 1.D0-EP
        D(3) = 4.D0*(1.D0-EP)
C
        E(1) = 0
        E(2) = 0
        E(3) = 1.D0+EP
C
        F(1) = 0
        F(2) = 0
        F(3) = 0
C
C
        Z(N-2) = 0 
        Z(N-1) = 0
        Z(N) = 0
C
        A(N-2) = 1.D0+EP
        A(N-1) = 0
        A(N) = 0
C
        B(N-2) = 4.D0*(1.D0-EP)
        B(N-1) = 1.D0-EP
        B(N) = 0
C
        C(N-2) = 6.D0*(1.D0+EP)
        C(N-1) = 2.D0*(1.D0+EP)
        C(N) = 1.D0
C
        D(N-2) = 4.D0*(1.D0-EP)
        D(N-1) = 1.D0-EP
        D(N) = 0
C
        E(N-2) = 1.D0+EP
        E(N-1) = 0
        E(N) = 0
C
        F(N-2) = 0 
        F(N-1) = 0
        F(N) = 0
C
C       Step One.
        BETA(1) = D(1)/C(1)
        DELTA(2) = B(2)
        W(1) = C(1)
        PI(1) = F(1)/W(1)
        AP(1) = 0
        AP(2) = 0
        AP(3) = A(3)
        W(2) = C(2)-DELTA(2)*BETA(1)
        GAM(1) = E(1)/C(1)
        BETA(2) = (D(2)-DELTA(2)*GAM(1))/W(2)
        GAM(2) = (E(2)-PI(1)*DELTA(2))/W(2)
        PI(2) = F(2)/W(2)
        DELTA(3) = (B(3)-AP(3)*BETA(1))
        W(3) = C(3)-DELTA(3)*BETA(2)-AP(3)*GAM(1)
        BETA(3) = (D(3)-AP(3)*PI(1)-DELTA(3)*GAM(2))/W(3)
        GAM(3) = (E(3)-DELTA(3)*PI(2))/W(3)
        PI(3) = F(3)/W(3)
C
C       Step Two
        DO 20 I=4,N
        AP(I) = A(I)-Z(I)*BETA(I-3)
        DELTA(I) = B(I)-AP(I)*BETA(I-2)-Z(I)*GAM(I-3)
        W(I) = C(I)-AP(I)*GAM(I-2)-DELTA(I)*BETA(I-1)
     +             -Z(I)*PI(I-3)
        BETA(I) = (D(I)-AP(I)*PI(I-2)-DELTA(I)*GAM(I-1))/W(I)
        GAM(I) = (E(I)-DELTA(I)*PI(I-1))/W(I)
        PI(I) = F(I)/W(I)
   20   CONTINUE
C
  100   CONTINUE
C
C       Step Three
        H(1) = BB(1)/W(1)
        H(2) = (BB(2)-DELTA(2)*H(1))/W(2)
        H(3) = (BB(3)-DELTA(3)*H(2)-AP(3)*H(1))/W(3)
        DO 30 I=4,N
        H(I) = (BB(I)-DELTA(I)*H(I-1)-AP(I)*H(I-2)
     +               -Z(I)*H(I-3))/W(I)
   30   CONTINUE
C
C       Step Four
        XANS(N) = H(N)
        XANS(N-1) = H(N-1)-BETA(N-1)*XANS(N)
        XANS(N-2) = H(N-2)-BETA(N-2)*XANS(N-1)-GAM(N-2)*XANS(N)
        DO 40 I=N-3,1,-1
        XANS(I) = H(I)-BETA(I)*XANS(I+1)-GAM(I)*XANS(I+2)
     +                                  -PI(I)*XANS(I+3)
   40   CONTINUE
        RETURN
        END
        SUBROUTINE RICDAT

c*********************************************************************72
C
c       Modify Initial Data to agree with RICHARDSON'S values.
C 
c
c       size of grid
        PARAMETER (IM=13, JM=7, LM=5)
        PARAMETER (LMM1=LM-1)
c
c
c       space for the basic INPUT variables 
        REAL PS(IM,JM), Z(IM,JM,LMM1)
        REAL U(IM,JM,LM),V(IM,JM,LM), DDDFF(IM,JM,LM)
        REAL OROG(IM,2*JM)
        COMMON / WINPUT / PS, Z, U, V, DDDFF, OROG
c
c       space for the basic PROGNOSTIC variables
        real P(IM,JM,LM),MU(IM,JM,LM),MV(IM,JM,LM),TTOP(IM,JM)
        COMMON / WPROG / P, MU, MV, TTOP
c
C
c=================================================================
c
c       Level 2 pressures.
        P(7,5,1) = 20470
        P(6,4,1) = 20470
        P(8,4,1) = 20490
        P(5,4,1) = 20300
        P(7,4,1) = 20500
        P(9,4,1) = 20440
        P(6,3,1) = 20390
        P(8,3,1) = 20430
        P(7,3,1) = 20340
c
c       Level 4 pressures.
        P(7,5,2) = 40900
        P(6,4,2) = 40830
        P(8,4,2) = 40910
        P(5,4,2) = 40490
        P(7,4,2) = 40900
        P(9,4,2) = 40820
        P(6,3,2) = 40620
        P(8,3,2) = 40750
        P(7,3,2) = 40340
c
c       Level 6 pressures.
        P(7,5,3) = 60860
        P(6,4,3) = 60670
        P(8,4,3) = 60870
        P(5,4,3) = 60440
        P(7,4,3) = 60790
        P(9,4,3) = 60680
        P(6,3,3) = 60500
        P(8,3,3) = 60650
        P(7,3,3) = 60310
C
c       Level 8 pressures.
        P(7,5,4) = 79830
        P(6,4,4) = 79500
        P(8,4,4) = 79790
        P(5,4,4) = 79280
        P(7,4,4) = 79600
        P(9,4,4) = 79780
        P(6,3,4) = 79490
        P(8,3,4) = 79670
        P(7,3,4) = 79570
c
c       Level G pressures.
        P(7,5,5) = 98830
        P(6,4,5) = 98340
        P(8,4,5) = 97630
        P(5,4,5) = 97440
        P(7,4,5) = 96260
        P(9,4,5) = 98820
        P(6,3,5) = 87460
        P(8,3,5) = 84580
        P(7,3,5) = 99720
c
c=================================================================
c
c       Level 1 U-momenta.
        MU(7,5,1) = -6500
        MU(6,5,1) = -7000
        MU(8,5,1) = -16000
        MU(5,4,1) = -3000
        MU(7,4,1) = -5600
        MU(9,4,1) = -10000
        MU(6,4,1) =  2700
        MU(8,4,1) =  0
        MU(7,3,1) = -5000
c
c       Level 3 U-momenta.
        MU(7,5,2) =  12700
        MU(6,5,2) = -6200
        MU(8,5,2) =  4000
        MU(5,4,2) = -24500
        MU(7,4,2) = -14600
        MU(9,4,2) =  0
        MU(6,4,2) = -32800
        MU(8,4,2) = -16600
        MU(7,3,2) = -28000
c
c       Level 5 U-momenta.
        MU(7,5,3) =  8100
        MU(6,5,3) = -11400
        MU(8,5,3) = -6000
        MU(5,4,3) = -22300
        MU(7,4,3) = -9500
        MU(9,4,3) = -5500
        MU(6,4,3) = -13600
        MU(8,4,3) = -9500
        MU(7,3,3) = -17500
c
c       Level 7 U-momenta.
        MU(7,5,4) = -8100
        MU(6,5,4) = -9100
        MU(8,5,4) = -6000
        MU(5,4,4) = -9100
        MU(7,4,4) = -5200
        MU(9,4,4) = -2500
        MU(6,4,4) = -3300
        MU(8,4,4) = -1900
        MU(7,3,4) = -10500
c
c       Level 9 U-momenta.
        MU(7,5,5) = -19800
        MU(6,5,5) = -16000
        MU(8,5,5) = -21900
        MU(5,4,5) = -1800
        MU(7,4,5) = -11000
        MU(9,4,5) = -19000
        MU(6,4,5) =  4800
        MU(8,4,5) = -6500
        MU(7,3,5) = -15500
c
c       Level 1 V-momenta.
        MV(7,5,1) =  800
        MV(5,4,1) = -11000
        MV(7,4,1) = -1800
        MV(9,4,1) = -3200
        MV(7,3,1) =  8000
c
c       Level 3 V-momenta.
        MV(7,5,2) = -10400
        MV(5,4,2) =  30000
        MV(7,4,2) = -6200
        MV(9,4,2) = -26000
        MV(7,3,2) =  4100
c
c       Level 5 V-momenta.
        MV(7,5,3) = -2500
        MV(5,4,3) =  15800
        MV(7,4,3) =  2900
        MV(9,4,3) = -13500
        MV(7,3,3) =  15000
c
c       Level 7 V-momenta.
        MV(7,5,4) =  0
        MV(5,4,4) =  8700
        MV(7,4,4) =  5800
        MV(9,4,4) =  4800
        MV(7,3,4) =  8000
c
c       Level 9 V-momenta.
        MV(7,5,5) =  8400
        MV(5,4,5) =  1500
        MV(7,4,5) =  5500
        MV(9,4,5) =  16000
        MV(7,3,5) =  4000
c
c=================================================================
c
c       Level 1 temperatures.
        TTOP(6,5) = 214
        TTOP(8,5) = 216
        TTOP(5,5) = 215
        TTOP(7,5) = 214
        TTOP(9,5) = 216
        TTOP(6,4) = 212
        TTOP(8,4) = 214
        TTOP(5,4) = 214
        TTOP(7,4) = 212
        TTOP(9,4) = 214
        TTOP(6,3) = 214
        TTOP(8,3) = 214
        TTOP(7,3) = 213

        RETURN
        end
        SUBROUTINE TABLOT(P,MU,MV,TTOP,OROG,IM,JM,LM,MESAGE)

c*********************************************************************72
c
cc TABLOT prints a table of central values.
c
        CHARACTER*10 MESAGE
        REAL P(IM,JM,LM), MU(IM,JM,LM), MV(IM,JM,LM)
        REAL TTOP(IM,JM)
        REAL OROG(IM,2*JM)
c
c       Print a HEADER RECORD
        print9800, MESAGE
        print9901, ' Table of CENTRAL Values '
c       print9900
 9800   FORMAT(A10)
 9900   FORMAT(1X)
 9901   FORMAT(A50)
 9802   FORMAT('_______________________________',
     +          '_____________________________________________')
 9902   FORMAT('|______________|______________|',
     +          '______________|______________|______________|')
c
        print9802
        DO J=5,3,-1
c
          print901, ( TTOP(I,J) , I=6,8,2 )
          DO L=1,5
             print902, (MU(I,J,L),MV(I,J,L) , I=5,9,2 )
             print903, ( P(I,J,L) , I=6,8,2 )
          end do
          JU = 2*J
          print916, (OROG(I,JU),I=5,9,1)
  901 FORMAT('| ',12x,' | ',6x,f6.0,' | ',12x,' | ',
     +            6x,f6.0,' | ',12x,' |')
  902 FORMAT('| ',   2f6.0,' | ',12x,' | ',2f6.0,' | ',
     +            12x,' | ',2f6.0,' |')
  903 FORMAT('| ',12x,' | ',   f6.1,6x,' | ',12x,' | ',
     +            f6.1,6x,' | ',12x,' |')
  916 FORMAT('| ',f12.0,' | ',f12.0,' | ',f12.0,' | ',
     +            f12.0,' | ',f12.0,' |')
c
        print9902
          print904, ( TTOP(I,J) , I=5,9,2 )
          DO L=1,5
             print905, (MU(I,J,L),MV(I,J,L) , I=6,8,2 )
             print906, ( P(I,J,L) , I=5,9,2 )
          end do
          JL = 2*J-1
          print916, (OROG(I,JL),I=5,9,1)
  904 FORMAT('| ',6x,f6.0,' | ',12x,' | ',6x,
     +            f6.0,' | ',12x,' | ',6x,f6.0,' |')
  905 FORMAT('| ',12x,' | ',   2f6.0,' | ',12x,' | ',
     +            2f6.0,' | ',12x,' |')
  906 FORMAT('| ',   f6.1,6x,' | ',12x,' | ',
     +             f6.1,6x,' | ',12x,' | ',f6.1,6x,' |')
c
          print9902
        end do
C
        CALL TEXTAB(P,MU,MV,TTOP,OROG,IM,JM,LM)
c
        RETURN
        END
        SUBROUTINE TEXTAB(P,MU,MV,TTOP,OROG,IM,JM,LM)

c*********************************************************************72
c
c  TABLE Output for TeX.
c
        REAL P(IM,JM,LM), MU(IM,JM,LM), MV(IM,JM,LM)
        REAL TTOP(IM,JM)
        REAL OROG(IM,2*JM)

        PARAMETER( IMM= 13, JMM=07, LMM=5 )
        INTEGER IP(IMM,JMM,LMM),IMU(IMM,JMM,LMM),IMV(IMM,JMM,LMM)
        INTEGER ITTOP(IMM,JMM)
        INTEGER IOROG(IMM,2*JMM)
c
        DO I=1,IM
        DO J=1,JM
          DO L=1,LM
            IP (I,J,L) = NINT( P (I,J,L)/100. )
            IMU(I,J,L) = NINT( MU(I,J,L)/100. )
            IMV(I,J,L) = NINT( MV(I,J,L)/100. )
          end do
          ITTOP(I,J) = NINT( TTOP(I,J) )
        end do
        end do

        DO I=1,IM
        DO J=1,2*JM
          IOROG(I,J) = NINT( OROG(I,J) )
        end do
        end do
c
C       "Blank" Line.
c       print99802
99802   FORMAT('|&|&|&|&|& \\nr ')
99842   FORMAT(' ')
c
c  Header Block
c
        print99802
        print90052
        print99803
90052   FORMAT('|$ 5\\Deg E$&|$ 8\\Deg E$&|$11\\Deg E$',
     X        '&|$14\\Deg E$&|$17\\Deg E$     & \\nr ')
99803   FORMAT('|&|&|&|&|& \\cr ')
c
c  First Two Blocks
        DO J=5,5,-1
c
          JU = 2*J
          JL = 2*J-1
c
c         print8401, ( TTOP(I,J) , I=6,8,2 )
          print99802
          DO L=1,5
             IF(L.EQ.3) THEN
             print8683,  100*IMU(7,J,L),100*IMV(7,J,L) 
             ELSE
             print8613,  100*IMU(7,J,L),100*IMV(7,J,L) 
             ENDIF
          end do
C         JU = 2*J
C         print8416, IOROG(7,JU)
          print99803
 8401 FORMAT('|&|& ',I5,'|&|& ',I5,'|& \\nr ')
 8416 FORMAT('|&|&|&',I5,'|&|& \\cr ')
 8613 FORMAT('|&|&|\\bf ',   I5,'&\\bf ',I5,'|&|& \\nr ')
 8683 FORMAT('$54.0\\Deg N$ |&|&|\\bf ',   I5,
     X                         '&\\bf ',   I5,'|&|& \\nr ')
c
        print99842
c
          print8504, ( ITTOP(I,J) , I=7,7,1 )
          DO L=1,5
             IF(L.EQ.3) THEN
             print8685, 
     +          100*IMU(6,J,L), 100*IP(7,J,L), 100*IMU(8,J,L)
             ELSE
             print8615, 
     +          100*IMU(6,J,L), 100*IP(7,J,L), 100*IMU(8,J,L)
             ENDIF
          end do
C         JL = 2*J-1
          print8516, (IOROG(I,JL),I=7,7,1)
 8504 FORMAT('|&|&|&',I5,'|&|&\\nr ')
 8516 FORMAT('|&|&|& ',I5,'|&|& \\cr ')
 8615 FORMAT('|&|\\bf ',   I5,'&|',
     X          I5,'&|\\bf ',I5,'&|& \\nr ')
 8685 FORMAT('$52.2\\Deg N$ |&|\\bf ',   I5,'&|',
     X          I5,'&|\\bf ',I5,'&|& \\nr ')
c
        print99842
c
        end do
C
c------------------------- Next Two Blocks
        DO J=4,4,-1
c
          JU = 2*J
          JL = 2*J-1
c
          print9401, ( ITTOP(I,J) , I=6,8,2 )
          DO L=1,5
c            print9402, (MU(I,J,L),MV(I,J,L) , I=5,9,2 )
c            print9403, ( P(I,J,L) , I=6,8,2 )
           IF(L.EQ.10) THEN
             print9603, 
     X        MU(5,J,L),MV(5,J,L) , P(6,J,L) , 100*TTOP(6,J),
     X        MU(7,J,L),MV(7,J,L) , P(8,J,L) , 100*TTOP(8,J),
     X        MU(9,J,L),MV(9,J,L)  
           ELSE IF(L.EQ.50) THEN
             print9693, 
     X        MU(5,J,L),MV(5,J,L) , P(6,J,L) , 100*OROG(6,JU),
     X        MU(7,J,L),MV(7,J,L) , P(8,J,L) , 100*OROG(8,JU),
     X        MU(9,J,L),MV(9,J,L)  
           ELSE IF(L.EQ.3) THEN
             print9683, 
     X        IMU(5,J,L),IMV(5,J,L) , IP(6,J,L) ,
     X        IMU(7,J,L),IMV(7,J,L) , IP(8,J,L) ,
     X        IMU(9,J,L),IMV(9,J,L)  
           ELSE 
             print9613, 
     X        IMU(5,J,L),IMV(5,J,L) , IP(6,J,L) ,
     X        IMU(7,J,L),IMV(7,J,L) , IP(8,J,L) ,
     X        IMU(9,J,L),IMV(9,J,L)  
           ENDIF
          end do
C         JU = 2*J
          print9416, (IOROG(I,JU),I=6,8,2)
 9401 FORMAT('|&|& ',I5,'\\hfill|&|& ',I5,'\\hfill|& \\nr ')
C9402 FORMAT('|',   I5,'&',I5,'|&|',I5,'&',I5,
C    X                        '|&|',2x,I5,'&',I5,' \\nr ')
C9403 FORMAT('|&|',   f5.1,'&|&|',f6.1,'&|&\\nr ')
 9416 FORMAT('|&|&',I5,'|&|&',I5,'|& \\cr ')
 9603 FORMAT('|',   I5,'&',I5,'|',I5,'& \\it ',I5,'|',
     X                   I5,'&',I5,
     X                        '|',I5,'& \\it ',I5,'|',
     X                   I5,'&',I5, ' \\nr ')
 9693 FORMAT('|',   I5,'&',I5,'|',I5,'& \\bf ',I5,'|',
     X                   I5,'&',I5,
     X                        '|',I5,'& \\bf ',I5,'|',
     X                   I5,'&',I5, ' \\cr ')
 9613 FORMAT('|\\bf ',   I5,'&\\bf ',I5,'|',I5,
     X            '&|\\bf ',I5,'&\\bf ',I5,
     X   '|',I5,'&|\\bf ',I5,'&\\bf ',I5,' \\nr ')
 9683 FORMAT('$50.4\\Deg N$ |\\bf ',   I5,'&\\bf ',I5,'|',
     X             I5,'&|\\bf ',I5,'&\\bf ',I5,
     X            '|',I5,'&|\\bf ',I5,'&\\bf ',I5,' \\nr ')
c
C       print99802
        print99842
c
          print9504, ( ITTOP(I,J) , I=5,9,2 )
          DO L=1,5
C            print9505, (MU(I,J,L),MV(I,J,L) , I=6,8,2 )
C            print9506, ( P(I,J,L) , I=5,9,2 )
           IF(L.EQ.10) THEN
             print9605, 
     X        P(5,J,L), 100*TTOP(5,J), MU(6,J,L),MV(6,J,L) ,
     X        P(7,J,L), 100*TTOP(7,J), MU(8,J,L),MV(8,J,L) ,
     X        P(9,J,L), 100*TTOP(9,J)
           ELSE IF(L.EQ.50) THEN
             print9695, 
     X        P(5,J,L), 100*OROG(5,JL), MU(6,J,L),MV(6,J,L) ,
     X        P(7,J,L), 100*OROG(7,JL), MU(8,J,L),MV(8,J,L) ,
     X        P(9,J,L), 100*OROG(9,JL)
           ELSE IF(L.EQ.3) THEN
             print9685, 
     X        IP(5,J,L),  IMU(6,J,L),
     X        IP(7,J,L),  IMU(8,J,L),
     X        IP(9,J,L)
           ELSE
             print9615, 
     X        IP(5,J,L),  IMU(6,J,L),
     X        IP(7,J,L),  IMU(8,J,L),
     X        IP(9,J,L)
           ENDIF
          end do
C         JL = 2*J-1
          print9516, (IOROG(I,JL),I=5,9,2)
 9504 FORMAT('|&',I5,'|&|&',I5,
     X                '|&|&',I5,'\\nr ')
C9505 FORMAT('|&|',   I5,'&',I5,'|&|',I5,'&',I5,'|& \\nr ')
C9506 FORMAT('|&',   f5.1,'&|&|',f6.1,' \\nr ',
C    X               '&|&|',f5.1,' \\nr ')
C9516 FORMAT('|&\\bf ',I5,'|&\\bf ',I5,'|&\\bf ',I5,
C    X                '|&\\bf ',I5,'|&\\bf ',I5,' \\cr ')
 9516 FORMAT('|& ',I5,'|&|& ',I5,'|&|& ',I5,' \\cr ')
 9605 FORMAT('|',   I5,'& \\it ',I5,'|',
     X          I5,'&',I5,'|',
     X          I5,'& \\it ',I5,'|',
     X          I5,'&',I5,'|',
     X          I5,'& \\it ',I5,'\\nr ')
 9695 FORMAT('|',   I5,'& \\bf ',I5,'|',
     X          I5,'&',I5,'|',
     X          I5,'& \\bf ',I5,'|',
     X          I5,'&',I5,'|',
     X          I5,'& \\bf ',I5,'\\cr ')
 9615 FORMAT('|',   I5,'&|\\bf ',I5,'&|',
     X          I5,'&|\\bf ',I5,'&|',I5,'& \\nr ')
 9685 FORMAT('$48.6\\Deg N$|',   I5,'&|\\bf ',I5,'&|',
     X          I5,'&|\\bf ',I5,'&|',I5,'& \\nr ')
c
C       print99802
        print99842
c
        end do
C
c  LAST Two Blocks
        DO J=3,3,-1
c
          JU = 2*J
          JL = 2*J-1
c
          print7401, ( ITTOP(I,J) , I=6,8,2 )
          DO L=1,5
             IF(L.EQ.3) THEN
             print7683, 
     X         IP(6,J,L), IMU(7,J,L),IMV(7,J,L) , IP(8,J,L)
             ELSE
             print7613, 
     X         IP(6,J,L), IMU(7,J,L),IMV(7,J,L) , IP(8,J,L)
             ENDIF
          end do
C         JU = 2*J
          print7416, (IOROG(I,JU),I=6,8,2)
 7401 FORMAT('|&|& ',I5,'\\hfill|&|& ',I5,'\\hfill|& \\nr ')
 7416 FORMAT('|&|&',I5,'|&|&',I5,'|& \\cr ')
 7613 FORMAT('|&|',   I5,'&|\\bf ',I5,'&\\bf ',
     X                I5,'|',I5,'&|& \\nr ')
 7683 FORMAT('$46.8\\Deg N$|&|',   I5,'&|\\bf ',I5,'&\\bf ',
     X                           I5,'|',I5,'&|& \\nr ')
c
        print99842
c
          print7504, ( ITTOP(I,J) , I=7,7,1 )
          DO L=1,5
             IF(L.EQ.3) THEN
             print7685, IP(7,J,L)
             ELSE
             print7615, IP(7,J,L)
             ENDIF
          end do
C         JL = 2*J-1
          print7516, (IOROG(I,JL),I=7,7,1)
 7504 FORMAT('|&|&|&',I5,'|&|& \\nr ')
 7516 FORMAT('|&|&|&',I5,'|&|& %%% \\cr  ')
 7615 FORMAT('|&|&|',    I5,'&|&|& \\nr ')
 7685 FORMAT('$45.0\\Deg N$|&|&|',    I5,'&|&|& \\nr ')
c
        print99842
c
        end do
C
        RETURN
        END
