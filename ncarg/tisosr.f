      program main

c***********************************************************************
c
cc This is a sample test for the ISOSRF isosurface routine.
c
c Define error file, Fortran unit number, and workstation type,
c and workstation ID.
c
      integer ierrf
      integer iwkid
      integer iwtype
      integer lunit

      ERRF = 6
      LUNIT = 2
      IWKID = 1
c
c  The workstation type will determine what kind of output is created
c  1: NCGM, an NCAR CGM file, called "gmeta" by default.
c  7: Display graphics in the current X window;
c  8: Display graphics in a new X window.
c  20: Color PS File, Portrait
c  21: Color EPS File, Portrait
c  27: Color EPS File, Landscape
c
      iwtype = 20
c
c  OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
c
      call GOPKS ( IERRF, ISZDM )
      call GOPWK ( IWKID, LUNIT, IWTYPE )
      call GACWK ( IWKID)
c
c  Call the test code.
c
      call TISOSR ( IERR )
c
c  DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
c
      call GDAWK ( IWKID )
      call GCLWK ( IWKID )
      call GCLKS

      STOP
      END
      SUBROUTINE TISOSR ( IERROR )

c***********************************************************************
c
cc  TISOSR provides a simple demonstration of ISOSRF.
c
c  Discussion:
c
c    Values of a function on a 3-D rectangular grid are stored in 
c    array T.  Subroutines EZISOS and ISOSRF are called to draw 
c    iso-valued surface plots of the function.
c
c ARGUMENTS
c
c ON OUTPUT              IERROR
c                          An integer variable
c                          = 0, if the test was successful,
c                          = 1, the test was not successful.
c
      SAVE
      DIMENSION       T(21,31,19),SLAB(33,33),EYE(3)
c
c  Specify coordinates for plot titles.
c
      REAL IX,IY
      DATA IX/.44/, IY/.95/

      DATA NU,NV,NW/21,31,19/
      DATA RBIG1,RBIG2,RSML1,RSML2/6.,6.,2.,2./
      DATA TISO/0./
      DATA MUVWP2/33/
      DATA IFLAG/-7/
c
c  Initialize the error parameter.
c
      IERROR = 1
c
c  Fill the 3-D array to be plotted.
c
      JCENT1 = FLOAT(NV)*.5-RBIG1*.5
      JCENT2 = FLOAT(NV)*.5+RBIG2*.5
      DO I=1,NU
         FIMID = I-NU/2
         DO J=1,NV
            FJMID1 = J-JCENT1
            FJMID2 = J-JCENT2
            DO K=1,NW
               FKMID = K-NW/2
               F1 = SQRT(RBIG1*RBIG1/(FJMID1*FJMID1+FKMID*FKMID+.1))
               F2 = SQRT(RBIG2*RBIG2/(FIMID*FIMID+FJMID2*FJMID2+.1))
               FIP1 = (1.-F1)*FIMID
               FIP2 = (1.-F2)*FIMID
               FJP1 = (1.-F1)*FJMID1
               FJP2 = (1.-F2)*FJMID2
               FKP1 = (1.-F1)*FKMID
               FKP2 = (1.-F2)*FKMID
               T(I,J,K) = AMIN1(FIMID*FIMID+FJP1*FJP1+FKP1*FKP1-
     &                    RSML1*RSML1,
     &                      FKMID*FKMID+FIP2*FIP2+FJP2*FJP2-RSML2*RSML2)
            end do
         end do
      end do
c
c  Define the eye position.
c
      EYE(1) = 100.
      EYE(2) = 150.
      EYE(3) = 125.
c
c     Frame 1 -- The EZISOS entry.
c
c  Select normalization transformation 0.
c
      call GSELNT ( 0 )
c
c  Call PLCHLQ to write the plot title.
c
      call PLCHLQ(IX,IY,'DEMONSTRATION PLOT FOR ENTRY EZISOS OF ISOSRF',
     & 16.,0.,0.)

      call EZISOS ( T, NU, NV, NW, EYE, SLAB, TISO )
c
c  Frame 2 -- The ISOSRF entry.
c
c  Select normalization transformation 0.
c
      call GSELNT(0)
c
c  Call PLCHLQ to write the plot title.
c
      call PLCHLQ ( IX, IY, 
     &  'DEMONSTRATION PLOT FOR ENTRY ISOSRF OF ISOSRF',16.,0.,0. )
c
c  Test ISOSRF with a subarray of T.
c
      MU=NU/2
      MV=NV/2
      MW=NW/2
      MUVWP2=MAX0(MU,MV,MW)+2
      call ISOSRF(T(MU,MV,MW),NU,MU,NV,MV,MW,EYE,MUVWP2,SLAB,TISO,IFLAG)
      call FRAME

      IERROR = 0
      WRITE (6,1001)
      RETURN

 1001 FORMAT (' ISOSRF TEST EXECUTED--SEE PLOTS TO CERTIFY')

      END
