      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS563_PRB.
c
c  Discussion:
c
c    TOMS563_PRB tests the TOMS563 library.
c
      INTEGER I,IFL,INDX(50),MXS,NACT,NEQC,NEQNS,NIQC
      INTEGER NVARS,NXINPT,NXOUTP,NXPRMP
      LOGICAL PSW
      REAL E(500),EL1N,F(50),RES(50),X(10),W(570)
C
C     ***************
C     A SIMPLE DRIVER PROGRAM FOR   CL1.
C
C     NXINPT,NXOUTP,NXPRMP   ARE UNIT NUMBERS FOR
C     INPUT, OUTPUT, AND PROMPTING RESPECTIVELY.
C
C     PROBLEM DIMENSIONS ARE READ BY  DATFL1.
C     DATA ARRAYS ARE READ BY  DATFL2.
C     SEVERAL SUCCESSIVE PROBLEMS CAN BE SOLVED.
C
C     A TYPICAL INPUT SEQUENCE IS AS FOLLOWS...
C
C       NEW                           (NEW=1)
C       NVARS NEQNS NEQC NIQC
C       MXS
C       PSW                           (1 FOR PRINTING)
C       E(1,1)   THROUGH    E(N,1)
C       F(1)
C       .
C       .
C       .
C       E(1,K)   THROUGH    E(N,K)
C       F(K)
C       E(1,K+1) THROUGH    E(N,K+1)
C       F(K+1)
C       .
C       .
C       .
C       E(1,L)   THROUGH    E(N,L)
C       F(L)
C       E(1,L+1) THROUGH    E(N,L+1)
C       F(L+1)
C       .
C       .
C       .
C       E(1,M)   THROUGH    E(N,M)
C       F(M)
C       X(1)     THROUGH    X(N)
C       NEW                           (1 FOR MORE, 0 TO STOP)
C
C     WHERE
C       K STANDS FOR NEQNS,
C       L STANDS FOR NEQNS+NEQC,
C       M STANDS FOR NEQNS+NEQC+NIQC.
C
C     FOR FURTHER INFORMATION, CONSULT THE
C     COMMENTS IN  DATFL1,DATFL2, OR RUN THE
C     PROGRAM IN INTERACTIVE MODE AND RESPOND
C     TO THE PROMPTS PUT OUT ON UNIT  NXPRMP.
C     ***************
C
      DATA NXINPT /5/ , NXOUTP /6/ , NXPRMP /6/
C
   10 CONTINUE
C
C     ***************
C     READ DATA
C     ***************
C
      WRITE (NXOUTP,1000)
      CALL DATFL1(NEQNS,NEQC,NIQC,NVARS,MXS,PSW,NXINPT,NXOUTP,NXPRMP)
      CALL DATFL2(NEQNS,NEQC,NIQC,NVARS,E,X,F,NXINPT,NXOUTP,NXPRMP)
C
C     ***************
C     ENTER  CL1
C     ***************
C
      CALL CL1(NEQNS,NEQC,NIQC,NVARS,NACT,IFL,MXS,PSW,
     *         E,NVARS,X,F,EL1N,RES,INDX,W)
C
C     ***************
C     PRINT RESULTS, AND
C     GO BACK FOR NEW PROBLEM
C     ***************
C
      WRITE (NXOUTP,1010) IFL,EL1N
      WRITE (NXOUTP,1020)  (X(I),I=1,NVARS)
      IF (NACT .LE. 0)  GO TO 20
        WRITE (NXOUTP,1030)
        WRITE (NXOUTP,1040)  (INDX(I),I=1,NACT)
   20 CONTINUE
      GO TO 10
C
C     ***************
C     FORMATS
C     ***************
C
 1000 FORMAT(1H1)
 1010 FORMAT(21H0TERMINATION CODE =   ,I2/
     *       16H0L1 VALUE.     = ,1PE15.7/
     *       9H0X-VECTOR )
 1020 FORMAT(5(1PE15.7))
 1030 FORMAT(29H0ACTIVE EQUATIONS/CONSTRAINTS)
 1040 FORMAT(10I5)
      END
      SUBROUTINE DATFL1(NEQNS,NEQC,NIQC,NVARS,MXS,PSW,
     *                  NXINPT,NXOUTP,NXPRMP)
C
      INTEGER K,MXS,NEQNS,NEQC,NIQC,NVARS
      INTEGER NXINPT,NXOUTP,NXPRMP
      LOGICAL PSW
C
C     ***************
C     READ  1  TO EXECUTE A PROBLEM.
C     READ  0  TO STOP.
C     FORMAT  I1
C     ***************
C
      WRITE (NXPRMP,1000)
      READ (NXINPT,1010)  K
      IF (K .EQ. 0)  STOP
C
C     ***************
C     READ  NVARS,NEQNS,NEQC,NIQC.
C     FORMAT  4I5
C     ***************
C
      WRITE (NXPRMP,1020)
      READ (NXINPT,1030) NVARS,NEQNS,NEQC,NIQC
      WRITE (NXOUTP,1040) NVARS,NEQNS,NEQC,NIQC
C
C     ***************
C     READ  MXS.
C     FORMAT  I5
C     ***************
C
      WRITE (NXPRMP,1050)
      READ (NXINPT,1030) MXS
      WRITE (NXOUTP,1060) MXS
C
C     ***************
C     READ  1  TO GET INTERMEDIATE PRINTING.
C     READ  0  TO GET NONE.
C     FORMAT  I1
C     ***************
C
      WRITE (NXPRMP,1070)
      READ (NXINPT,1010) K
      PSW = .FALSE.
      IF (K .EQ. 1)  PSW = .TRUE.
      RETURN
C
C     ***************
C     FORMATS
C     ***************
C
 1000 FORMAT(27H0NEW PROBLEM (1=YES,0=STOP))
 1010 FORMAT(I1)
 1020 FORMAT(20H0NUMBER OF VARIABLES/
     *        11X,9HEQUATIONS/11X,20HEQUALITY CONSTRAINTS/
     *        11X,28HINEQUALITY CONSTRAINTS (4I5))
 1030 FORMAT(10I5)
 1040 FORMAT(23H0NUMBER OF VARIABLES = ,I3/
     *       23H0NUMBER OF EQUATIONS = ,I3/
     *       34H0NUMBER OF EQUALITY CONSTRAINTS = ,I3/
     *       36H0NUMBER OF INEQUALITY CONSTRAINTS = ,I3)
 1050 FORMAT(26H0NUMBER OF ITERATIONS (I5))
 1060 FORMAT(24H0NUMBER OF ITERATIONS =  ,I3)
 1070 FORMAT(35H0INTERMEDIATE PRINTING (1=YES,0=NO))
      END
      SUBROUTINE DATFL2(NEQNS,NEQC,NIQC,NVARS,E,X,F,
     *                  NXINPT,NXOUTP,NXPRMP)
C
      INTEGER I,IX,J,NEQNS,NEQC,NIQC
      INTEGER NVARS,NXINPT,NXOUTP,NXPRMP
      REAL E(NVARS,1),F(1),X(1)
C
      IF (NEQNS .LE. 0)  GO TO 20
C
C     ***************
C     IF  NEQNS .GT. 0, READ IN
C     EQUATION COEFFICIENTS
C     FOLLOWED BY RIGHT HAND SIDES.
C     FORMAT  8(F8.3,2X)
C     ***************
C
        WRITE (NXPRMP,1000)
        DO 10 I=1,NEQNS
          WRITE (NXPRMP,1010)  NVARS,I
          READ (NXINPT,1020)  (E(J,I),J=1,NVARS)
          WRITE (NXPRMP,1030)
          READ (NXINPT,1020) F(I)
   10   CONTINUE
   20 CONTINUE
      IF (NEQC .LE. 0)  GO TO 40
C
C     ***************
C     IF  NEQC .GT. 0, READ IN
C     EQUALITY-CONSTRAINT COEFFICIENTS
C     FOLLOWED BY RIGHT HAND SIDES.
C     FORMAT  8(F8.3,2X)
C     ***************
C
        WRITE (NXPRMP,1040)
        DO 30 I=1,NEQC
          IX = I + NEQNS
          WRITE (NXPRMP,1050)  NVARS,I
          READ (NXINPT,1020)  (E(J,IX),J=1,NVARS)
          WRITE (NXPRMP,1030)
          READ (NXINPT,1020)  F(IX)
   30   CONTINUE
   40 CONTINUE
      IF (NIQC .LE. 0)  GO TO 60
C
C     ***************
C     IF  NIQC .GT. 0, READ IN
C     INEQUALITY-CONSTRAINT COEFFICIENTS
C     FOLLOWED BY RIGHT HAND SIDES.
C     FORMAT  8(F8.3,2X)
C     ***************
C
        WRITE (NXPRMP,1060)
        DO 50 I=1,NIQC
          IX = I + NEQNS + NEQC
          WRITE (NXPRMP,1070)  NVARS,I
          READ (NXINPT,1020)  (E(J,IX),J=1,NVARS)
          WRITE (NXPRMP,1030)
          READ (NXINPT,1020)  F(IX)
   50   CONTINUE
   60 CONTINUE
C
C     ***************
C     READ IN STARTING VALUES FOR  X.
C     FORMAT  8(F8.3,2X)
C     ***************
C
      WRITE (NXPRMP,1080)
      READ (NXINPT,1020)  (X(I),I=1,NVARS)
      WRITE (NXOUTP,1090)
C
C     ***************
C     ECHO ALL ARRAYS
C     ***************
C
      WRITE (NXOUTP,1100)  (X(I),I=1,NVARS)
      WRITE (NXOUTP,1110)
      IX = NEQNS + NEQC + NIQC
      DO 70 I=1,IX
        WRITE (NXOUTP,1100)  (E(J,I),J=1,NVARS),F(I)
   70 CONTINUE
      RETURN
C
C     ***************
C     FORMATS
C     ***************
C
 1000 FORMAT(18H0SPECIFY EQUATIONS)
 1010 FORMAT(1H ,I3,28H  COEFFICIENTS FOR EQUATION ,
     *       I3,13H (8(F8.3,2X)))
 1020 FORMAT(8(F8.3,2X))
 1030 FORMAT(24H RIGHT-HAND SIDE  (F8.3))
 1040 FORMAT(29H0SPECIFY EQUALITY CONSTRAINTS )
 1050 FORMAT(1H ,I3,
     *       39H  COEFFICIENTS FOR EQUALITY CONSTRAINT  ,
     *       I3,10H (F8.3,2X))
 1060 FORMAT(31H0SPECIFY INEQUALITY CONSTRAINTS )
 1070 FORMAT(1H ,I3,
     *       41H  COEFFICIENTS FOR INEQUALITY CONSTRAINT ,
     *       I3,10H (F8.3,2X))
 1080 FORMAT(37H0SPECIFY STARTING POINT  (8(F8.3,2X)))
 1090 FORMAT(16H0STARTING POINT /1H )
 1100 FORMAT(8(2X,F8.3))
 1110 FORMAT(18H0ARRAYS  E  AND  F/1H )
      END

