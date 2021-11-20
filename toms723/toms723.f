      subroutine AMACH(MODE, I, I1, R1, D1)

c*********************************************************************72
c
cc AMACH provides machine constants.
c
c>> 1995-01-05 AMACH  Snyder  Add VAX D-3 for Alpha running VMS
c>> 1994-10-26 AMACH  Krogh   Changes to use M77CON
c>> 1994-09-23 AMACH  Snyder  Add VAX G parameters
c>> 1994-06-21 AMACH  Snyder  Compute only round-off and u-flow at first
c>> 1994-05-25 AMACH  Snyder  Added an option to compute at run time.
c>> 1992-04-07 AMACH  Oken    Removed ^Z at EOF (error found by VAX comp
c>> 1992-02-20 AMACH  Snyder  Added Cray-YMP stuff, q.v.
c>> 1990-06-11 AMACH  Snyder  Added Apollo DN-10000 stuff, q.v.
c>> 1990-12-14 AMACH  Lawson  Changed to eliminate ENTRY statements.
c>> 1990-08-21 AMACH  Krogh   No test was getting done for bad machine.
c>> 1990-02-28 AMACH  Krogh   Correct missing DOUBLE PRECISION AMSUB1
c>> 1989-08-14 AMACH  Krogh   Parameterized everything -- Massive change
c>> 1989-03-30 AMACH  Snyder  Correct missing "/" line 921
c>> 1989-01-30 AMACH  Snyder  Incorporate more constants from NETLIB.
C>> 1988-05-19 AMACH  Lawson  Initial code.
c File AMACH.FOR contains user-callable functions I1MACH, D1MACH, and
c R1MACH, plus second-level subroutines AMACH, AMTEST, and AMSUB1.
c Appropriate lines must be switched between comment and non-comment
c status when this code is moved to a different computer system.
c     These changes can be done with any text editor, however the "c++"
c lines permit automation of the change using the M77CON processor.
c Note that when the M77CON processor activates a line it shifts
c Columns 2-72 to 1-71 and puts a blank in Column 72.  When it inactiv-
c ates a line it shifts Columns 1-71 to 2-72 and puts a C in Column 1.
c     The possible choices using M77CON (don't include parenthetical
c     comments) are:
c      c++ CURRENT HAS SYS = IEEE
c      c++ CURRENT HAS SYS = ALPHA_D3
c      c++ CURRENT HAS SYS = AMDAHL
c      c++ CURRENT HAS SYS = APOLLO_10000
c      c++ CURRENT HAS SYS = BUR1700
c      c++ CURRENT HAS SYS = BUR5700
c      c++ CURRENT HAS SYS = BUR67_7700
c      c++ CURRENT HAS SYS = CDC60_7000
c      c++ CURRENT HAS SYS = CONVEXC_1
c      c++ CURRENT HAS SYS = CRAY1
c      c++ CURRENT HAS SYS = CRAY1_SD (Sngl prec.arith. used for dble.)
c      c++ CURRENT HAS SYS = CRAY1_64 (64 bit integers)
c      c++ CURRENT HAS SYS = CRAY1_SD_64 (64 bit int, SP used for DP)
c      c++ CURRENT HAS SYS = CRAY_T3D
c      c++ CURRENT HAS SYS = CRAY_YMP
c      c++ CURRENT HAS SYS = CRAY_YMP_SD (Sngl prec. used for dble.)
c      c++ CURRENT HAS SYS = DG_S2000
c      c++ CURRENT HAS SYS = HARRIS220
c      c++ CURRENT HAS SYS = HON600_6000
c      c++ CURRENT HAS SYS = HON_DPS_8_70
c      c++ CURRENT HAS SYS = HP700Q
c      c++ CURRENT HAS SYS = IBM360_370
c      c++ CURRENT HAS SYS = INTERDATA_8_32
c      c++ CURRENT HAS SYS = PDP10_KA
c      c++ CURRENT HAS SYS = PDP10_KB
c      c++ CURRENT HAS SYS = PDP11
c      c++ CURRENT HAS SYS = PRIME50
c      c++ CURRENT HAS SYS = SEQ_BAL_8000
c      c++ CURRENT HAS SYS = UNIVAC
c      c++ CURRENT HAS SYS = VAX
c      c++ CURRENT HAS SYS = VAX_G
c     The current choice is:
c++ CURRENT HAS SYS = IEEE
c
c     One can also select whether floating point constants are created
c     by the compiler or created at run time.  The choices using M77CON
c     are:
c      c++ CURRENT HAS HOW = COMPILER
c      c++ CURRENT HAS HOW = RUN
c     The current choice is:
c++ CURRENT HAS HOW = COMPILER
c
c     If the constants are created at run time, and they fail the run-
c     time check for reasonableness, they are re-created assuming IEEE.
c     If they still fail, the program stops.
c
C  I/O UNIT NUMBERS:
C
C    IM1 = I1MACH( 1) = THE STANDARD INPUT UNIT.
C    IM2 = I1MACH( 2) = THE STANDARD OUTPUT UNIT.
C    IM3 = I1MACH( 3) = THE STANDARD PUNCH UNIT.
C    IM4 = I1MACH( 4) = THE STANDARD ERROR MESSAGE UNIT.
C
C  WORDS:
C
C    IM5 = I1MACH( 5) = THE NUMBER OF BITS PER INTEGER STORAGE UNIT.
C    IM6 = I1MACH( 6) = THE NUMBER OF CHARACTERS/INTEGER STORAGE UNIT.
C
C  INTEGERS:
C
C    ASSUME INTEGERS ARE REPRESENTED IN THE S-DIGIT, BASE-A FORM
C
C               SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C
C               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,S-1.
C
C    IM7 = I1MACH( 7) = A, THE BASE.
C    IM8 = I1MACH( 8) = S, THE NUMBER OF BASE-A DIGITS.
C    IM9 = I1MACH( 9) = A**S - 1, THE LARGEST MAGNITUDE.
C
C  FLOATING-POINT NUMBERS:
C
C    ASSUME FLOATING-POINT NUMBERS ARE REPRESENTED IN THE T-DIGIT,
C    BASE-B FORM
C
C               SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C               WHERE 0 .LE. X(I) .LT. B FOR I=1,...,T,
C               0 .LT. X(1), AND EMIN .LE. E .LE. EMAX.
C
C    IM10 = I1MACH(10) = B, THE BASE.
C
C  SINGLE-PRECISION:
C
C    IM11 = I1MACH(11) = T, THE NUMBER OF BASE-B DIGITS.
C    IM12 = I1MACH(12) = EMIN, THE SMALLEST EXPONENT E.
C    IM13 = I1MACH(13) = EMAX, THE LARGEST EXPONENT E.
C
C  DOUBLE-PRECISION:
C
C    IM14 = I1MACH(14) = T, THE NUMBER OF BASE-B DIGITS.
C    IM15 = I1MACH(15) = EMIN, THE SMALLEST EXPONENT E.
C    IM16 = I1MACH(16) = EMAX, THE LARGEST EXPONENT E.
C
C  CONVERSION FROM FUNCTIONAL TO STRUCTURAL FLOATING POINT CONSTANTS
C
C    IM17 = CONSTANT SUCH THAT IM14 + IM17 = ACTUAL NUMBER OF BASE-B
C           DIGITS IN DOUBLE PRECISION, USED FOR CHECKING THAT CORRECT
C           VERSION OF THIS PROGRAM IS INSTALLED.  (SEE DEFINITION OF
C           DM6, AND THE USE OF DM6 IN CALLING AMTEST.)
C
C  TO ALTER THIS FUNCTION FOR A PARTICULAR ENVIRONMENT,
C  THE DESIRED SET OF PARAMETER STATEMENTS SHOULD BE ACTIVATED BY
C  REMOVING THE C FROM COLUMN 1.  ALSO, THE VALUES OF
C  IM1 - IM4 SHOULD BE CHECKED FOR CONSISTENCY
C  WITH THE LOCAL OPERATING SYSTEM.
c     -----------------------------------------------------------------
c     Original design and code due to P. A. Fox, A. D. Hall, and
c     N. L. Schryer, Bell Laboratories.  See ACM TOMS, 4,(1978),177-188.
c     Adapted to Univac 1100 by Kris Stewart, JPL, 7/30/81.
c     Adapted for the JPL MATH77 library by C. L. Lawson and F. T. Krogh
c     Sept, 1987.
c     1989-08-14 AMACH  Krogh   Parameterized everything. Major changes.
C     1990 Dec. CLL reorganized code to avoid using ENTRY statements
c     for functions of different types.  Also added save statements.
c     -----------------------------------------------------------------
c     On the first call to this function, tests are done to verify that
c     IM10 and IM14 are not grossly wrong for the host environment.
c     This gives some protection against using the wrong version of this
c     subprogram.
c     -----------------------------------------------------------------
      integer MODE, I, I1
      real R1
      double precision D1, TEST
c
      integer IMACH(17)
      integer IM1, IM2, IM3, IM4, IM5, IM6, IM7, IM8, IM9, IM10, IM11,
     1            IM12, IM13, IM14, IM15, IM16, IM17
c++ Code for HOW=RUN is INACTIVE
C      integer IEEE
C      integer ID1, ID2, ID3, ID4, ID5, ID6, ID7, ID8, ID10, ID11,
C     1   ID12, ID13, ID14, ID15, ID16, ID17
c++ Code for (HOW=RUN) | SYS=IEEE is ACTIVE
      integer IE1, IE2, IE3, IE4, IE5, IE6, IE7, IE8, IE10, IE11,
     1   IE12, IE13, IE14, IE15, IE16, IE17
c++ end
      real             RMACH(5), RM1, RM2, RM3, RM4, RM5,
     1                 RMA, RMB, RBASE
      double precision DMACH(5), DM1, DM2, DM3, DM4, DM5, DM6,
     1                 DMA, DMB, DBASE
      save TEST, IMACH, RMACH, DMACH
C     -----------------------------------------------------------------
C     Machine constants for IEEE standard binary floating-point
c     processors.  This includes PC's and work-stations using the
c     Intel 8087, 80287, 80387, ... processors or the
c     Motorola 68881, 68882, ... processors.
c     Note:  We are setting the "most negative exponent" (IMACH(12) and
c     IMACH(15)) to be the exponent of the smallest normalized number.
c     An IEEE processor actually handles smaller numbers before
c     underflowing, however these "unnormalized" numbers have
c     diminished precision.
c
c++ Code for (HOW=RUN) | SYS=IEEE is ACTIVE
c     Parameters for IEEE when generating at run time:
      PARAMETER (IE1 =5, IE2 =6, IE3 =7, IE4 =6)
      PARAMETER (IE5 =32, IE6 =4, IE7 =2, IE8 =31)
      PARAMETER (IE10 =2, IE11 =24, IE12 =-125, IE13 =128)
      PARAMETER (IE14 =53, IE15 =-1021, IE16 =1024, IE17=0)
c++ Code for SYS = IEEE is ACTIVE
      PARAMETER (IM1 = IE1, IM2 = IE2, IM3 = IE3, IM4 = IE4)
      PARAMETER (IM5 = IE5, IM6 = IE6, IM7 = IE7, IM8 = IE8)
      PARAMETER (IM10 = IE10, IM11 = IE11, IM12 = IE12, IM13 = IE13)
      PARAMETER (IM14 = IE14, IM15 = IE15, IM16 = IE16, IM17 = IE17)
C     -----------------------------------------------------------------
c++ Code for SYS = ALPHA_D3 is INACTIVE
Cc     MACHINE CONSTANTS for the VAX/VMS F and D-3 format for Alpha

C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =53, IM15 =-127, IM16 =127, IM17=0)
c++ end
C     -----------------------------------------------------------------
c++ Code for HOW = RUN is INACTIVE
Cc     MACHINE CONSTANTS for the VAX/VMS F and D-3 format for Alpha

C      PARAMETER (ID1 =5, ID2 =6, ID3 =7, ID4 =6)
C      PARAMETER (ID5 =32, ID6 =4, ID7 =2, ID8 =31)
C      PARAMETER (ID10 =2, ID11 =24, ID12 =-127, ID13 =127)
C      PARAMETER (ID14 =53, ID15 =-127, ID16 =127, ID17=0)
c++ end
C     -----------------------------------------------------------------
c++ Code for SYS = AMDAHL is INACTIVE
CC     MACHINE CONSTANTS FOR AMDAHL MACHINES.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =16, IM11 =6, IM12 =-64, IM13 =63)
C      PARAMETER (IM14 =14, IM15 =-64, IM16 =63, IM17=0)
C      -----------------------------------------------------------------
c++ Code for SYS = APOLLO_10000 is INACTIVE
cc     MACHINE CONSTANTS FOR APOLLO DN_10000 MACHINES.
cc     The only difference from IEEE is IM13.  This difference has
cc     nothing to do with the arithmetic or representation used by the
cc     machine.  It is caused by a bug in the compiler:  The right-hand
cc     side of RM2 (below) is apparently evaluated in double precision.
cc     When the compiler is ready to store the resulting value into its
cc     internal data structures, it compares it to an incorrect value
cc     of the overflow limit.  It appears the incorrect value has the
cc     correct exponent, but the fraction is 1.5 instead of 2-2**(-p),
cc     where p is the precision in bits.  You can get the correct result
cc     by changing IM13 to 128, changing RM2 from a parameter to a
cc     variable, and changing the parameter statement that assigns a
cc     value to RM2 into an ordinary assignment statement.
CC
c      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
c      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
c      PARAMETER (IM10 =2, IM11 =24, IM12 =-125, IM13 =127)
c      PARAMETER (IM14 =53, IM15 =-1021, IM16 =1024, IM17 =0)
CC     -----------------------------------------------------------------
c++ Code for SYS = BUR1700 is INACTIVE
CC     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
CC
C      PARAMETER (IM1 =7, IM2 =2, IM3 =2, IM4 =2)
C      PARAMETER (IM5 =36, IM6 =4, IM7 =2, IM8 =33)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-256, IM13 =255)
C      PARAMETER (IM14 =60, IM15 =-256, IM16 =255, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = BUR5700 is INACTIVE
CC     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =48, IM6 =6, IM7 =2, IM8 =39)
C      PARAMETER (IM10 =8, IM11 =13, IM12 =-50, IM13 =76)
C      PARAMETER (IM14 =26, IM15 =-50, IM16 =76, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = BUR67_7700 is INACTIVE
CC     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =48, IM6 =6, IM7 =2, IM8 =39)
C      PARAMETER (IM10 =8, IM11 =13, IM12 =-50, IM13 =76)
C      PARAMETER (IM14 =26, IM15 =-32754, IM16 =32780, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = CDC60_7000 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =60, IM6 =10, IM7 =2, IM8 =48)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-929, IM13 =1070)
C      PARAMETER (IM14 =94, IM15 =-929, IM16 =1069, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = CONVEXC_1 is INACTIVE
CC     MACHINE CONSTANTS FOR CONVEX C-1.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-128, IM13 =127)
C      PARAMETER (IM14 =53, IM15 =-1024, IM16 =1023, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY1 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =46)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8189, IM13 =8190)
C      PARAMETER (IM14 =94, IM15 =-8099, IM16 =8190, IM17=2)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY_T3D is INACTIVE
cc     Machine constants for Cray T3D.  IEEE double for both precisions.
c      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
c      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
c      PARAMETER (IM10 =2, IM11 =53, IM12 =-1021, IM13 =1024)
c      PARAMETER (IM14 =53, IM15 =-1021, IM16 =1024, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY_YMP is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY YMP
CC     Cray claims the overflow exponent (IM13 and IM16) is 8189, and
CC     the underflow exponent (IM12 and IM15) is -8189, but these values
CC     don't seem to work in cf77:  the underflow limit underflows, and
CC     the overflow limit overflows when using Cray's values.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =46)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8188, IM13 =8189)
C      PARAMETER (IM14 =94, IM15 =-8188, IM16 =8189, IM17=2)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY_YMP_SD is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY YMP
CC     Cray claims the overflow exponent (IM13 and IM16) is 8189, and
CC     the underflow exponent (IM12 and IM15) is -8189, but these
CC     values don't seem to work in cf77:  the underflow limit under-
CC     flows, and the overflow limit overflows when using Cray's values.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =46)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8188, IM13 =8189)
C      PARAMETER (IM14 =47, IM15 =-8188, IM16 =8189, IM17=1)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY1_SD is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3, WHEN DOUBLE
CC     PRECISION IS TO USE SINGLE PRECISION ARITHMETIC.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =46)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8189, IM13 =8190)
C      PARAMETER (IM14 =47, IM15 =-8189, IM16 =8190, IM17=1)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY1_64 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =63)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8189, IM13 =8190)
C      PARAMETER (IM14 =94, IM15 =-8099, IM16 =8190, IM17=2)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY1_SD_64 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3, WHEN DOUBLE
CC     PRECISION IS TO USE SINGLE PRECISION ARITHMETIC.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =63)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8189, IM13 =8190)
C      PARAMETER (IM14 =47, IM15 =-8189, IM16 =8190, IM17=1)
CC     -----------------------------------------------------------------
c++ Code for SYS = DG_S2000 is INACTIVE
CC     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
CC
C      PARAMETER (IM1 =11, IM2 =12, IM3 =8, IM4 =10)
C      PARAMETER (IM5 =16, IM6 =2, IM7 =2, IM8 =15)
C      PARAMETER (IM10 =16, IM11 =6, IM12 =-64, IM13 =63)
C      PARAMETER (IM14 =14, IM15 =-64, IM16 =63, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = HARRIS220 is INACTIVE
CC     MACHINE CONSTANTS FOR THE HARRIS 220, SLASH 6, SLASH 7.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =0, IM4 =6)
C      PARAMETER (IM5 =24, IM6 =3, IM7 =2, IM8 =23)
C      PARAMETER (IM10 =2, IM11 =23, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =38, IM15 =-127, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = HON600_6000 is INACTIVE
CC     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =43, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =6, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =63, IM15 =-127, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = HON_DPS_8_70 is INACTIVE
CC     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =43, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =4, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =63, IM15 =-127, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = HP700Q is INACTIVE
cc     Machine constants for HP-700 using the +autodblpad option,
cc     which automatically increases DOUBLE PRECISION to REAL*16, and
cc     REAL to DOUBLE PRECISION.
c      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
c      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
c      PARAMETER (IM10 =2, IM11 =53, IM12 =-1021, IM13 =1024)
c      PARAMETER (IM14 = 113, IM15 = -16381, IM16 = 16384, IM17 = 0)
CC     -----------------------------------------------------------------
c++ Code for SYS = IBM360_370 is INACTIVE
CC     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
CC     THE XEROX SIGMA 5/7/9 AND THE SEL SYSTEMS 85/86.

C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =16, IM11 =6, IM12 =-64, IM13 =63)
C      PARAMETER (IM14 =14, IM15 =-64, IM16 =63, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = INTERDATA_8_32 is INACTIVE
CC     MACHINE CONSTANTS FOR THE INTERDATA 8/32
CC     WITH THE UNIX SYSTEM FORTRAN 77 COMPILER.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =6, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =16, IM11 =6, IM12 =-64, IM13 =62)
C      PARAMETER (IM14 =14, IM15 =-64, IM16 =62, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = PDP10_KA is INACTIVE
CC     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =5, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-128, IM13 =127)
C      PARAMETER (IM14 =54, IM15 =-101, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = PDP10_KB is INACTIVE
CC     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =5, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-128, IM13 =127)
C      PARAMETER (IM14 =62, IM15 =-128, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = PDP11 is INACTIVE
CC     MACHINE CONSTANTS FOR PDP-11 FORTRAN'S SUPPORTING
CC     16-BIT INTEGER ARITHMETIC.

C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =16, IM6 =2, IM7 =2, IM8 =15)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =56, IM15 =-127, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = PRIME50 is INACTIVE
CC     MACHINE CONSTANTS FOR THE PRIME 50 SERIES SYSTEMS
CC     WITH 32-BIT INTEGERS AND 64V MODE INSTRUCTIONS,
CC     SUPPLIED BY IGOR BRAY.

C      PARAMETER (IM1 =1, IM2 =1, IM3 =2, IM4 =1)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =23, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =47, IM15 =-32895, IM16 =32637, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = SEQ_BAL_8000 is INACTIVE
CC     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
CC
C      PARAMETER (IM1 =0, IM2 =0, IM3 =7, IM4 =0)
C      PARAMETER (IM5 =32, IM6 =1, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-125, IM13 =128)
C      PARAMETER (IM14 =53, IM15 =-1021, IM16 =1024, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = UNIVAC is INACTIVE
CC     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
CC
CC     NOTE THAT THE PUNCH UNIT, I1MACH(3), HAS BEEN SET TO 1
CC     WHICH IS APPROPRIATE FOR THE UNIVAC-FTN SYSTEM.
CC     IF YOU HAVE THE UNIVAC-FOR SYSTEM, SET IT TO 7.
CC     IM6 = 4 for FTN (4 chars per word), 6 for FOR (6 chars per word).
Cc
C      PARAMETER (IM1 =5, IM2 =6, IM3 =1, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =4, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-128, IM13 =127)
C      PARAMETER (IM14 =60, IM15 =-1024, IM16 =1023, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = VAX is INACTIVE
Cc     MACHINE CONSTANTS for the VAX/VMS F and D formats
Cc     and for PDP-11 FORTRAN SUPPORTING 32-BIT INTEGER ARITHMETIC.

C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =56, IM15 =-127, IM16 =127, IM17=0)
c++ end
C     -----------------------------------------------------------------
c++ Code for SYS = VAX_G is INACTIVE
Cc     MACHINE CONSTANTS for the VAX/VMS F and G formats
Cc     and for PDP-11 FORTRAN SUPPORTING 32-BIT INTEGER ARITHMETIC.

C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =53, IM15 =-1023, IM16 =1023, IM17=0)
c++ end
C     -----------------------------------------------------------------
C
C
C Real parameters
C
C  RM1 = R1MACH(1) = B**(EMIN-1), The smallest positive number, i.e.,
c                    the underflow limit.
C  RM2 = R1MACH(2) = B**EMAX*(1 - B**(-T)), The largest number, i.e.,
c                    the overflow limit.
C  RM3 = R1MACH(3) = B**(-T), The smallest relative spacing, i.e., the
c                    difference between 1.0 and the next smaller number.
C  RM4 = R1MACH(4) = B**(1-T), The largest relative spacing, i.e., the
c                     difference between 1.0 and the next larger number.
C  RM5 = R1MACH(5) = LOG10(B).  When B = 2 this value is
c              Log10(2) = 0.30102_99956_63981_19521_37388_94724
C
C Parameter RMA and RMB are selected so that for values of the base =
C 2, 8, 16, 10, RMA has the values 1, 3, 4, 0, and RMB has the values 0,
C 0, 0, 1.  These values are used in computing RM5.
C $$$$ Note that if other bases are to be supported, the calculation of
C $$$$ RMA and RMB will have to be generalized.
C
c++   Code for HOW = COMPILER is ACTIVE
      PARAMETER (IM9 = 2 * (2**(IM8-1) - 1) + 1)
      PARAMETER (RMA = ((IM10 - 10) * (-3 + ((IM10 - 2) * (-77 +
     1    12 * (IM10 - 8))) / 14)) / 24)
      PARAMETER (RMB = ((IM10 - 2) * (IM10 - 8) * (16 - IM10)) / 96)
      PARAMETER (RBASE = IM10)
C
C     Weird subterfuges below are NECESSARY to compute DM1 and DM2 on
C     some systems.  DON'T SIMPLIFY THEM.  We compute RM1 and RM2 using
C     these subterfuges so it will be clear we're computing the REAL
C     and DOUBLE PRECISION characteristics in the same way.
      PARAMETER (RM1 = (RBASE**(IM12/2)) * (RBASE**(IM12-IM12/2-1)))
      PARAMETER (RM2 = RBASE**(IM13-IM11) * ((RBASE**IM11 - RBASE)
     1               + (RBASE - 1.0E0)))
      PARAMETER (RM3 = RBASE**(-IM11))
      PARAMETER (RM4 = RBASE**(1-IM11))
c     PARAMETER (RM5 = RMA*0.30102 99956 63981 19521 37388 94724E0+RMB)
      PARAMETER (RM5 = RMA*0.301029995663981195213738894724E0+RMB)
C
C Double precision parameters -- (Defined like the real ones.)
C
      PARAMETER (DMA = ((IM10 - 10) * (-3 + ((IM10 - 2) * (-77 +
     1    12 * (IM10 - 8))) / 14)) / 24)
      PARAMETER (DMB = ((IM10 - 2) * (IM10 - 8) * (16 - IM10)) / 96)
      PARAMETER (DBASE = IM10)
C
C     Weird subterfuges below are NECESSARY to compute DM1 and DM2 on
C     some systems.  DON'T SIMPLIFY THEM.
      PARAMETER (DM1 = (DBASE**(IM15/2)) * (DBASE**(IM15-IM15/2-1)))
      PARAMETER (DM2 = DBASE**(IM16-IM14) * ((DBASE**IM14 - DBASE)
     1               + (DBASE - 1.0D0)))
      PARAMETER (DM3 = DBASE**(-IM14))
      PARAMETER (DM4 = DBASE**(1-IM14))
c     PARAMETER (DM5 = DMA *
c    1 0.30102 99956 63981 19521 37388 94724 49302 67681 89881 46211 D0
c    2 + DMB)
c
      PARAMETER (DM5 = DMA*
     1 0.30102999566398119521373889472449302676818988146211D0 + DMB)
C DM6 and TEST are used in checking that the correct constants have
C been selected.
      PARAMETER (DM6 = DBASE**(-IM14-IM17))
c++   END
      data TEST / 0.D0 /
C
c     DATA IMACH / IM1, IM2, IM3, IM4, IM5, IM6, IM7, IM8, IM9, IM10,
c    1   IM11, IM12, IM13, IM14, IM15, IM16 /
c     DATA RMACH / RM1, RM2, RM3, RM4, RM5 /
c     DATA DMACH / DM1, DM2, DM3, DM4, DM5 /
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      if (TEST .eq. 0.0D0) then
C         IM9 = 2 * (2**(IM8-1) - 1) + 1
         IMACH(1) = IM1
         IMACH(2) = IM2
         IMACH(3) = IM3
         IMACH(4) = IM4
         IMACH(5) = IM5
         IMACH(6) = IM6
         IMACH(7) = IM7
         IMACH(8) = IM8
         IMACH(10) = IM10
         IMACH(11) = IM11
         IMACH(12) = IM12
         IMACH(13) = IM13
         IMACH(14) = IM14
         IMACH(15) = IM15
         IMACH(16) = IM16
         IMACH(17) = IM17
c++   Code for HOW = RUN is INACTIVE
C         IEEE = 0
C100      continue
C      DBASE = IMACH(10)
CC
CC     Weird subterfuge below is NECESSARY to compute DM1 on
CC     some systems.  DON'T SIMPLIFY IT.
C      DM1=(DBASE**(IMACH(15)/2)) * (DBASE**(IMACH(15)-IMACH(15)/2-1))
CC DM6 and TEST are used in checking that the correct constants have
CC been selected.
C      DM6 = DBASE**(-IMACH(14)-IMACH(17))
c++   end
         CALL AMTEST (TEST, DM6)
         if (dm1 .eq. 0.0d0 .or. test .eq. 0.0d0) then
c++   Code for HOW = RUN is INACTIVE
C           if (IEEE .eq. 0) then
C              IEEE = 1
C              IMACH(1) = IE1
C              IMACH(2) = IE2
C              IMACH(3) = IE3
C              IMACH(4) = IE4
C              IMACH(5) = IE5
C              IMACH(6) = IE6
C              IMACH(7) = IE7
C              IMACH(8) = IE8
C              IMACH(10) = IE10
C              IMACH(11) = IE11
C              IMACH(12) = IE12
C              IMACH(13) = IE13
C              IMACH(14) = IE14
C              IMACH(15) = IE15
C              IMACH(16) = IE16
C              IMACH(17) = IE17
C              go to 100
C           end if
C           if (IEEE .eq. 1) then
C              IEEE = 2
C              IMACH(1) = ID1
C              IMACH(2) = ID2
C              IMACH(3) = ID3
C              IMACH(4) = ID4
C              IMACH(5) = ID5
C              IMACH(6) = ID6
C              IMACH(7) = ID7
C              IMACH(8) = ID8
C              IMACH(10) = ID10
C              IMACH(11) = ID11
C              IMACH(12) = ID12
C              IMACH(13) = ID13
C              IMACH(14) = ID14
C              IMACH(15) = ID15
C              IMACH(16) = ID16
C              IMACH(17) = ID17
C              go to 100
C           end if
c++   END
            print*,'AMACH has bad parameters for current environment.'
            stop
         end if
c++   Code for HOW = RUN is INACTIVE
C         IM9 = 2 * (2**(IMACH(8)-1) - 1) + 1
C         RMA = ((IMACH(10) - 10) * (-3 + ((IMACH(10) - 2) * (-77 +
C     1       12 * (IMACH(10) - 8))) / 14)) / 24
C         RMB = ((IMACH(10)-2) * (IMACH(10)-8) * (16-IMACH(10)))/96
C         RBASE = IMACH(10)
CC
CC        Weird subterfuges below are NECESSARY to compute DM1 and DM2
CC        on some systems.  DON'T SIMPLIFY THEM.  We compute RM1 and
CC        RM2 using these subterfuges so it will be clear we're
CC        computing the REAL and DOUBLE PRECISION characteristics in
Cc        the same way.
C         RM1=(RBASE**(IMACH(12)/2))*(RBASE**(IMACH(12)-IMACH(12)/2-1))
C         RM2 = RBASE**(IMACH(13)-IMACH(11))*((RBASE**IMACH(11) - RBASE)
C     1                  + (RBASE - 1.0E0))
C         RM3 = RBASE**(-IMACH(11))
C         RM4 = RBASE**(1-IMACH(11))
Cc        RM5 = RMA*0.30102 99956 63981 19521 37388 94724E0+RMB
C         RM5 = RMA*0.301029995663981195213738894724E0+RMB
CC
CC Double precision parameters -- (Defined like the real ones.)
CC
C         DMA = ((IMACH(10) - 10) * (-3 + ((IMACH(10) - 2) * (-77 +
C     1       12 * (IMACH(10) - 8))) / 14)) / 24
C         DMB = ((IMACH(10)-2) * (IMACH(10)-8) * (16-IMACH(10)))/96
CC
CC        Weird subterfuge below is NECESSARY to compute DM2 on
CC        some systems.  DON'T SIMPLIFY IT.
C         DM2 = DBASE**(IMACH(16)-IMACH(14))*((DBASE**IMACH(14) - DBASE)
C     1                  + (DBASE - 1.0D0))
C         DM3 = DBASE**(-IMACH(14))
C         DM4 = DBASE**(1-IMACH(14))
Cc        DM5 = DMA*0.30102 99956 63981 19521 37388 94724D0+DMB
C         DM5 = DMA*0.301029995663981195213738894724D0+DMB
c++   END
         IMACH(9) = IM9
         RMACH(1) = RM1
         RMACH(2) = RM2
         RMACH(3) = RM3
         RMACH(4) = RM4
         RMACH(5) = RM5
         DMACH(1) = DM1
         DMACH(2) = DM2
         DMACH(3) = DM3
         DMACH(4) = DM4
         DMACH(5) = DM5
      ENDIF

      if (MODE .eq. 0) then
         I1=IMACH(I)
      else if (MODE .eq. 1) then
         R1=RMACH(I)
c                                  Here we assume MODE = 2.
      else
         D1=DMACH(I)
      endif
      return
      end
      integer function I1MACH(I)

c*********************************************************************72
c
cc I1MACH provides integer machine constants.
c
      integer I, I1
      real R1
      double precision D1
      IF (I .LT. 1  .OR.  I .GT. 16) THEN
         PRINT*,'I1MACH.. Bad argument: I =',I
         STOP 'I1MACH error'
      END IF
      call AMACH (0, I, I1, R1, D1)
      I1MACH = I1
      return
      end
      real function R1MACH(I)

c*********************************************************************72
c
cc R1MACH provides single precision machine constants.
c
      integer I, I1
      real R1
      double precision D1
      IF (I .lt. 1  .or.  I .gt. 5) THEN
         print*,'R1MACH.. Bad argument: I = ',I
         stop 'R1MACH error'
      END IF
      call AMACH (1, I, I1, R1, D1)
      R1MACH = R1
      RETURN
      end
      double precision function D1MACH(I)

c*********************************************************************72
c
cc D1MACH provides double precision machine constants.
c
      integer I, I1
      real R1
      double precision D1
      IF (I .lt. 1  .or.  I .gt. 5) THEN
         print*,'D1MACH.. Bad argument: I = ',I
         stop 'D1MACH error'
      END IF
      call AMACH (2, I, I1, R1, D1)
      D1MACH = D1
      RETURN
      END
      SUBROUTINE AMTEST (TEST, D6)

c*********************************************************************72
c
cc AMTEST ???
c
c Verifies that D6 is an appropriate value for DM6.
c Returns TEST = D6 + D6 - 1, .ne. 0 if D6 is an appropriate value for
c DM6, else returns TEST = 0.  The caller uses TEST = 0 as a signal to
c try again with IEEE settings (unless that's already been done).
      DOUBLE PRECISION AMSUB1, D6, TEST
      TEST = AMSUB1(1.D0 + D6)
C
C The comparison with 1.875E0*D6 in the line below is to guard
C against the possibility that TEST is > 0 as a result of rounding
C up in the addition of D6 to 1.
C
      IF ((TEST .eq. 0.D0) .or. (TEST .gt. 1.875D0*D6)) THEN
         TEST = (D6 + D6) + 1.D0
         IF (AMSUB1(TEST) .ne. 0.D0) RETURN
      END IF
      test = 0.0d0
      END
      DOUBLE PRECISION FUNCTION AMSUB1 (TEST1)

c*********************************************************************72
c
cc AMSUB1 returns the value of TEST1 - 1.
c
      DOUBLE PRECISION TEST1
C     Returns the value of TEST1 - 1.
      AMSUB1 = TEST1 - 1.0D0
      RETURN
      END
      DOUBLE PRECISION FUNCTION DCOSPX (X)

c*********************************************************************72
c
cc DCOSPX evaluates cos(pi*x).
c
C>> 1996-01-29 DCOSPX WVS JPL Add better acknowledgement for origins.
C>> 1994-10-20 DCOSPX Krogh  Changes to use M77CON
C>> 1994-04-22 DCOSPX CLL Made SP and DP codes similar.
C>> 1993-05-06 DCOSPX WVS JPL Convert from NSWC to Math 77
C ----------------------------------------------------------------------
c This procedure was originally procedure COS1 from the Naval Surface
c Warfare Center library.
C ----------------------------------------------------------------------
C
C                         EVALUATION OF COS(PI*X)
C
C                             --------------
C
C     THE EXPANSION FOR SIN(PI*A) (ABS(A) .LE. 1/4) USING A1,...,A13
C     IS ACCURATE TO WITHIN 2 UNITS OF THE 40-TH SIGNIFICANT DIGIT, AND
C     THE EXPANSION FOR COS(PI*A) (ABS(A) .LE. 1/4) USING B1,...,B13
C     IS ACCURATE TO WITHIN 4 UNITS OF THE 40-TH SIGNIFICANT DIGIT.
c
c     The polynomials using coefficients SA0, ... SA6 and SB1, ..., SB6
c     give approximations whose largest observed relative error in the
c     relevant intervals is 0.129d-14.
c     We will use this latter approximation when the machine epsilon
c     is larger than 0.2d-14.
C ----------------------------------------------------------------------
c--D replaces "?": ?COSPX, ?ERM1
C ----------------------------------------------------------------------
      integer N
      double precision D1MACH
      DOUBLE PRECISION A, BIG, CUTOFF, EPS, PI, T, W, X
      DOUBLE PRECISION A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
     *                 A11, A12, A13
      DOUBLE PRECISION B1, B2, B3, B4, B5, B6, B7, B8, B9, B10,
     *                 B11, B12, B13
      DOUBLE PRECISION SA0, SA1, SA2, SA3, SA4, SA5, SA6
      DOUBLE PRECISION      SB1, SB2, SB3, SB4, SB5, SB6
      SAVE BIG, EPS
C -----------------------
      PARAMETER ( PI =3.141592653589793238462643383279502884197D+00 )
      parameter ( CUTOFF = 0.2d-14 )
C -----------------------
      data EPS / -1.0d0 /
      DATA SA0 /.314159265358979D+01/, SA1 /-.516771278004995D+01/,
     *     SA2 /.255016403987327D+01/, SA3 /-.599264528932149D+00/,
     *     SA4 /.821458689493251D-01/, SA5 /-.737001831310553D-02/,
     *     SA6 /.461514425296398D-03/
      DATA SB1 /-.493480220054460D+01/, SB2 /.405871212639605D+01/,
     *     SB3 /-.133526276691575D+01/, SB4 /.235330543508553D+00/,
     *     SB5 /-.258048861575714D-01/, SB6 /.190653140279462D-02/
      DATA A1  /-.1028083791780141522795259479153765743002D+00/,
     *     A2  / .3170868848763100170457042079710451905600D-02/,
     *     A3  /-.4657026956105571623449026167864697920000D-04/,
     *     A4  / .3989844942879455643410226655783424000000D-06/,
     *     A5  /-.2237397227721999776371894030796800000000D-08/,
     *     A6  / .8847045483056962709715066675200000000000D-11/,
     *     A7  /-.2598715447506450292885585920000000000000D-13/,
     *     A8  / .5893449774331011070033920000000000000000D-16/,
     *     A9  /-.1062975472045522550784000000000000000000D-18/,
     *     A10 / .1561182648301780992000000000000000000000D-21/,
     *     A11 /-.1903193516670976000000000000000000000000D-24/,
     *     A12 / .1956617650176000000000000000000000000000D-27/,
     *     A13 /-.1711276032000000000000000000000000000000D-30/
C -----------------------
      DATA B1  /-.3084251375340424568385778437461297229882D+00/,
     *     B2  / .1585434424381550085228521039855226435920D-01/,
     *     B3  /-.3259918869273900136414318317506279360000D-03/,
     *     B4  / .3590860448591510079069203991239232000000D-05/,
     *     B5  /-.2461136950494199754009084061808640000000D-07/,
     *     B6  / .1150115912797405152263195572224000000000D-09/,
     *     B7  /-.3898073171259675439899172864000000000000D-12/,
     *     B8  / .1001886461636271969091584000000000000000D-14/,
     *     B9  /-.2019653396886572027084800000000000000000D-17/,
     *     B10 / .3278483561466560512000000000000000000000D-20/,
     *     B11 /-.4377345082051788800000000000000000000000D-23/,
     *     B12 / .4891532381388800000000000000000000000000D-26/,
     *     B13 /-.4617089843200000000000000000000000000000D-29/
C -----------------------
C
      IF (eps .LT. 0.0d0) then
         eps = D1MACH(3)
         BIG = 1.0d0/eps
      endif
C
C -----------------------
      A = ABS(X)
      IF (A .GE. BIG) THEN
         CALL DERM1 ('DCOSPX',1,2,
     1   'No precision because ABS(X) is too large','X',X,'.')
         DCOSPX = 1.0D0
         RETURN
      END IF
      N = A
      T = N
      A = A - T
      IF (A .GT. 0.75D0) GO TO 20
      IF (A .LT. 0.25D0) GO TO 21
C
C                    0.25 .LE. A .LE. 0.75
C
      A = 0.25D0 + (0.25D0 - A)
      if(eps .lt. cutoff) then
         T = 16.D0*A*A
         W = (((((((((((((A13*T + A12)*T + A11)*T + A10)*T + A9)*T +
     *            A8)*T + A7)*T + A6)*T + A5)*T + A4)*T + A3)*T +
     *            A2)*T + A1)*T + 0.5D0) + 0.5D0
         DCOSPX = PI*A*W
      else
         T = A*A
         DCOSPX = ((((((SA6*T + SA5)*T + SA4)*T + SA3)*T + SA2)*T
     *                  + SA1)*T + SA0)*A
      endif
      GO TO 30
C
C                 A .LT. 0.25  OR  A .GT. 0.75
C
   20 A = 0.25D0 + (0.75D0 - A)
      N = N - 1
   21 continue
      if(eps .lt. cutoff) then
         T = 16.D0*A*A
         DCOSPX = (((((((((((((B13*T + B12)*T + B11)*T + B10)*T + B9)*T+
     *                B8)*T + B7)*T + B6)*T + B5)*T + B4)*T + B3)*T +
     *                B2)*T + B1)*T + 0.5D0) + 0.5D0
      else
         T = A*A
         DCOSPX = ((((((SB6*T + SB5)*T + SB4)*T + SB3)*T + SB2)*T
     *                  + SB1)*T + 0.5D0) + 0.5D0
      endif
C
C                        TERMINATION
C
   30 continue
      IF (MOD(N,2) .NE. 0) DCOSPX = - DCOSPX
      RETURN
      END
c--D replaces "?": ?CSPXX, ?COSPX, ?SINPX
c>> 1996-01-08 DCSPXX WV Snyder Original code

      double precision function DCSPXX (X)

c*********************************************************************72
c
cc DCSPXX carefully evaluates cos(pi*x*x/2).
c
c COS(PI * X * X / 2) carefully to avoid loss of precision for large X

      double precision X

c DCOSPX is used to compute COS(PI * X)

      double precision D1MACH, DCOSPX, DSINPX
      external D1MACH, DCOSPX, DSINPX

c BIGX = 1 / round-off = biggest integer exactly representable by F.P.
c    If X > BIGX then to the working precision x**2 is an integer (which
c    we assume to be a multiple of four), so cos(pi/2 * x**2) = 1.
c N = [X], and later [K F]
c F = X - N = fractional part of X
c K = [ N / 2 ]
c J = N mod 2
c M = [K F]
c G = K F - M = fractional part of K F

      integer J, K, N
      double precision BIGX, F, G
      save BIGX
      data BIGX /-1.0/

      if (bigx .lt. 0.0d0) bigx = 1.0d0 / d1mach(4)
      f = abs(x)
      if (f .gt. bigx) then
c       Assume x is an even integer.
        dcspxx = 1.0d0
        return
      endif
      n = f
      f = f - n
      k = n / 2
      j = mod(n,2)
      g = k * f
      n = g
      g = g - n
      if (j .eq. 0) then
        dcspxx = dcospx(0.5d0*f*f + g + g)
      else
        dcspxx = -dsinpx(0.5d0*f*f + f + g + g)
      endif
      return
      end
      SUBROUTINE DERM1(SUBNAM,INDIC,LEVEL,MSG,LABEL,VALUE,FLAG)

c*********************************************************************72
c
cc DERM1 ???
c
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-10-20 DERM1  Krogh  Changes to use M77CON
C>> 1994-04-20 DERM1  CLL Edited to make DP & SP files similar.
C>> 1985-08-02 DERM1  Lawson  Initial code.
c--D replaces "?": ?ERM1, ?ERV1
C
      CHARACTER*(*) SUBNAM,MSG,LABEL
      CHARACTER*1 FLAG
      integer INDIC, LEVEL
      DOUBLE PRECISION VALUE
C
      CALL ERMSG(SUBNAM,INDIC,LEVEL,MSG,',')
      CALL DERV1(LABEL,VALUE,FLAG)
C
      RETURN
      END
      SUBROUTINE DERV1 ( LABEL, VALUE, FLAG )

c*********************************************************************72
c
cc DERV1 ???
c
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-10-20 DERV1  Krogh  Changes to use M77CON
C>> 1994-04-20 DERV1  CLL Edited to make DP & SP files similar.
C>> 1985-09-20 DERV1  Lawson  Initial code.
c--D replaces "?": ?ERV1
C
C     ------------------------------------------------------------
C     SUBROUTINE ARGUMENTS
C     --------------------
C     LABEL     An identifing name to be printed with VALUE.
C
C     VALUE     A floating point number to be printed.
C
C     FLAG      See write up for FLAG in ERMSG.
C
C     ------------------------------------------------------------
C
      COMMON/M77ERR/IDELTA,IALPHA
      INTEGER IDELTA,IALPHA
      DOUBLE PRECISION VALUE
      CHARACTER*(*) LABEL
      CHARACTER*1 FLAG
      SAVE /M77ERR/
C
      IF (IALPHA.GE.-1) THEN
        WRITE (*,*) '  ',LABEL,' = ',VALUE
        IF (FLAG.EQ.'.') CALL ERFIN
      ENDIF
      RETURN
C
      END
c  File: dfrenl.[for|f|c]
c  Contains procedures: DFRENC(), DFRENF(), DFRENG(), DFRENS()
c  and private low-level procedure: DFREN1().
c     .  Copyright (C) 1992, California Institute of Technology.
c     .  U. S. Government sponsorship under
c     .  NASA contract NAS7-918 is acknowledged.
c>> 1996-01-08 DFRENL WV Snyder Use DCSPXX for cos(Pi/2 x**2), etc.
C>> 1995-11-03 DFRENL Krogh  Removed blanks in numbers for C conversion.
c>> 1994-11-02 DFRENL Krogh  Changes to use M77CON
c>> 1994-10-18 DFRENL WV Snyder More specializing instructions
c>> 1993-02-25 DFRENL CLL. Edited to eliminate ENTRY and EQUIVALENCE.
c>> 1992-09-15 DFRENL WV Snyder Specializing instructions
c>> 1992-04-13 DFRENL WV Snyder Declare DFRENF, DFRENG, DFRENS
c>> 1992-03-18 DFRENL WV Snyder Move declarations for coefficient arrays
c>> 1992-01-24 DFRENL WV Snyder Original code
c--D replaces "?": ?FRENC, ?FREN1, ?FRENF, ?FRENG, ?FRENS, ?FRENL
c--&               ?CSPXX, ?SNPXX
c Subprograms in this file compute the Fresnel Cosine and Sine
c integrals C(x) and S(x), and the auxiliary functions f(x) and g(x),
c for any X:
c     DFRENC(X) for Fresnel integral C(X)
c     DFRENS(X) for Fresnel integral S(x)
c     DFRENF(X) for Fresnel integral auxiliary function f(x)
c     DFRENG(X) for Fresnel integral auxiliary function g(x).
c
c Developed by W. V. Snyder, Jet Propulsion Laboratory, 24 January 1992.
c
c Ref: W. J. Cody, "Chebyshev Approximations for the Fresnel Integrals",
c Mathematics of Computation, 1968, pp 450-453 plus Microfiche Suppl.
c W. V. Snyder, "Algorithm 723: Fresnel Integrals," ACM Trans. Math.
c Softw. 19, 4 (December 1993) 452-456.
c Accuracies of highest order formulae, where E is relative error:
c
c Range           Function   -log10(E)   Function   -log10(E)
c |X|<=1.2          C(x)       16.24       S(x)       17.26
c 1.2<|X|<=1.6      C(x)       17.47       S(x)       18.66
c 1.6<|X|<=1.9      f(x)       17.13       g(x)       16.25
c 1.9<|X|<=2.4      f(x)       16.64       g(x)       15.65
c 2.4<|X|           f(x)       16.89       g(x)       15.58
c
c Refer to Cody for accuracy of other approximations.
c
      double precision function DFRENC(X)

c*********************************************************************72
c
cc DFRENC evaluates the Fresnel cosine integral.
c
      external DFREN1
      double precision X, DFREN1
      DFRENC = DFREN1(1,X)
      return
      end
      double precision function DFRENF(X)

c*********************************************************************72
c
cc DFRENF evaluates the auxilliary f function.
c
      external DFREN1
      double precision X, DFREN1
      DFRENF = DFREN1(3,X)
      return
      end
      double precision function DFRENG(X)

c*********************************************************************72
c
cc DFRENG evaluates the auxilliary g function.
c
      external DFREN1
      double precision X, DFREN1
      DFRENG = DFREN1(4,X)
      return
      end
      double precision function DFRENS(X)

c*********************************************************************72
c
cc DFRENS evaluates the Fresnel sine integral.
c
      external DFREN1
      double precision X, DFREN1
      DFRENS = DFREN1(2,X)
      return
      end
      double precision function DFREN1(MODE, X)

c*********************************************************************72
c
cc DFREN1 computes a Fresnel integral or auxilliary function.
c
c     MODE = 1 means compute C.
c     MODE = 2 means compute S.
c     MODE = 3 means compute F.
c     MODE = 4 means compute G.
c     ------------------------------------------------------------------
c                        Internal variables.
c
c PID2 is pi / 2.
c RPI is the reciprocal of PI.
c RPISQ is the reciprocal of PI squared.
c AX is abs(x).
c BIGX is 1/sqrt(round-off).  If X > BIGX then to the working
c         precision x**2 is an integer (which we assume to be a multiple
c         of four), so cos(pi/2 * x**2) = 1, and sin(pi/2 * x**2) = 0.
c C and S are values of C(x) and S(x), respectively.
c CX and SX are cos(pi/2 * ax**2) and sin(pi/2 * ax**2), respectively.
c F and G are used to compute f(x) and g(x) when X > 1.6.
c HAVEC, HAVEF, HAVEG, HAVES are logical variables that indicate
c         whether the values stored in C, F, G and S correspond to the
c         value stored in X.  HAVEF indicates we have both F and G when
c         XSAVE .le. 1.6, and HAVEC indicates we have both C and S when
c         XSAVE .gt. 1.6.
c LARGEF is 1/(pi * underflow).  If X > LARGEF then f ~ 0.
c LARGEG is cbrt(1/(pi**2 * underflow)).  If X > LARGEG then g ~ 0.
c LARGEX is 1/sqrt(sqrt(underflow)).  If X > LARGEX then f ~ 1/(pi * x)
c         and g ~ 1/(pi**2 * x**3).
c MODE indicates the function to be computed: 1 = C(x), 2 = S(x),
c         3 = f(x), 4 = g(x).
c NEEDC, NEEDF, NEEDG, NEEDS are arrays indexed by MODE (MODE+4 when
c         X .gt. 1.6) that indicate what functions are needed.
c WANTC indicates whether C and S must be computed from F and G.
c WANTF and WANTG indicate we computed F and G on the present call.
c XSAVE is the most recently provided value of X.
c X4 is either X ** 4 or (1.0/X) ** 4.
c     If you change the order of approximation, you must change the
c     declarations and DATA statements for the coefficient arrays,
c     and the executable statements that evaluate the approximations.
c     ------------------------------------------------------------------
      external D1MACH, DCSPXX, DSNPXX
      double precision D1MACH, DCSPXX, DSNPXX
      double precision PID2, RPI, RPISQ
      parameter (PID2 = 1.570796326794896619231321691639751442099d0)
      parameter (RPI = 0.3183098861837906715377675267450287240689d0)
      parameter (RPISQ = RPI * RPI)
      double precision AX, BIGX, C, CX, F, G, LARGEF, LARGEG, LARGEX
      double precision S, SX, X, XSAVE, X4
      integer MODE
      logical HAVEC, HAVEF, HAVEG, HAVES, WANTC, WANTF, WANTG
      logical NEEDC(8), NEEDF(8), NEEDG(8), NEEDS(8)
      save BIGX, C, F, G, LARGEF, LARGEG, LARGEX, S, XSAVE
      save HAVEC, HAVEF, HAVEG, HAVES
c++   default digits=16
c++   code for digits <= 3 is inactive
c      double precision PC1(0:1), QC1(1:1)
c++   code for digits > 3 & digits <= 6 is inactive
c      double precision PC1(0:2), QC1(1:2)
c++   code for digits > 6 & digits <= 11 is inactive
c      double precision PC1(0:3), QC1(1:3)
c++   code for digits > 11 is active
      double precision PC1(0:4), QC1(1:4)
c++   end
c++   code for digits <= 2 is inactive
c      double precision PC2(0:1), QC2(1:1)
c++   code for digits > 2 & digits <= 5 is inactive
c      double precision PC2(0:2), QC2(1:2)
c++   code for digits > 5 & digits <= 8 is inactive
c      double precision PC2(0:3), QC2(1:3)
c++   code for digits > 8 & digits <= 12 is inactive
c      double precision PC2(0:4), QC2(1:4)
c++   code for digits > 12 is active
      double precision PC2(0:5), QC2(1:5)
c++   end
c++   code for digits <= 3 is inactive
c      double precision PS1(0:1), QS1(1:1)
c++   code for digits > 3 & digits <= 7 is inactive
c      double precision PS1(0:2), QS1(1:2)
c++   code for digits > 7 & digits <= 12 is inactive
c      double precision PS1(0:3), QS1(1:3)
c++   code for digits > 12 is active
      double precision PS1(0:4), QS1(1:4)
c++   end
c++   code for digits <= 2 is inactive
c      double precision PS2(0:1), QS2(1:1)
c++   code for digits > 2 & digits <= 5 is inactive
c      double precision PS2(0:2), QS2(1:2)
c++   code for digits > 5 & digits <= 9 is inactive
c      double precision PS2(0:3), QS2(1:3)
c++   code for digits > 9 & digits <= 14 is inactive
c      double precision PS2(0:4), QS2(1:4)
c++   code for digits > 14 is active
      double precision PS2(0:5), QS2(1:5)
c++   end
c++   code for digits <= 5 is inactive
c      double precision PF1(0:1), QF1(1:1)
c++   code for digits > 5 & digits <= 8 is inactive
c      double precision PF1(0:2), QF1(1:2)
c++   code for digits > 8 & digits <= 11 is inactive
c      double precision PF1(0:3), QF1(1:3)
c++   code for digits > 11 & digits <= 14 is inactive
c      double precision PF1(0:4), QF1(1:4)
c++   code for digits > 14 is active
      double precision PF1(0:5), QF1(1:5)
c++   end
c++   code for digits <= 5 is inactive
c      double precision PF2(0:1), QF2(1:1)
c++   code for digits > 5 & digits <= 8 is inactive
c      double precision PF2(0:2), QF2(1:2)
c++   code for digits > 8 & digits <= 11 is inactive
c      double precision PF2(0:3), QF2(1:3)
c++   code for digits > 11 & digits <= 14 is inactive
c      double precision PF2(0:4), QF2(1:4)
c++   code for digits > 14 is active
      double precision PF2(0:5), QF2(1:5)
c++   end
c++   code for digits <= 3 is inactive
c      double precision PF3(0:0)
c++   code for digits > 3 & digits <= 6 is inactive
c      double precision PF3(0:1), QF3(1:1)
c++   code for digits > 6 & digits <= 9 is inactive
c      double precision PF3(0:2), QF3(1:2)
c++   code for digits > 9 & digits <= 11 is inactive
c      double precision PF3(0:3), QF3(1:3)
c++   code for digits > 11 & digits <= 13 is inactive
c      double precision PF3(0:4), QF3(1:4)
c++   code for digits > 13 & digits <= 15 is inactive
c      double precision PF3(0:5), QF3(1:5)
c++   code for digits > 15 is active
      double precision PF3(0:6), QF3(1:6)
c++   end
c++   code for digits <= 4 is inactive
c      double precision PG1(0:1), QG1(1:1)
c++   code for digits > 4 & digits <= 7 is inactive
c      double precision PG1(0:2), QG1(1:2)
c++   code for digits > 7 & digits <= 10 is inactive
c      double precision PG1(0:3), QG1(1:3)
c++   code for digits > 10 & digits <= 13 is inactive
c     double precision PG1(0:4), QG1(1:4)
c++   code for digits > 13 is active
      double precision PG1(0:5), QG1(1:5)
c++   end
c++   code for digits <= 4 is inactive
c      double precision PG2(0:1), QG2(1:1)
c++   code for digits > 4 & digits <= 7 is inactive
c      double precision PG2(0:2), QG2(1:2)
c++   code for digits > 7 & digits <= 10 is inactive
c      double precision PG2(0:3), QG2(1:3)
c++   code for digits > 10 & digits <= 13 is inactive
c      double precision PG2(0:4), QG2(1:4)
c++   code for digits > 13 is active
      double precision PG2(0:5), QG2(1:5)
c++   end
c++   code for digits <= 3 is inactive
c      double precision PG3(0:0)
c++   code for digits > 3 & digits <= 5 is inactive
c      double precision PG3(0:1), QG3(r:1)
c++   code for digits > 5 & digits <= 8 is inactive
c      double precision PG3(0:2), QG3(1:2)
c++   code for digits > 8 & digits <= 10 is inactive
c      double precision PG3(0:3), QG3(1:3)
c++   code for digits > 10 & digits <= 12 is inactive
c      double precision PG3(0:4), QG3(1:4)
c++   code for digits = 13 is inactive
c      double precision PG3(0:5), QG3(1:5)
c++   code for digits > 13 is active
      double precision PG3(0:6), QG3(1:6)
c++   end
c
      data BIGX /-1.0d0/
      data C /0.0d0/, F /0.5d0/, G /0.5d0/, S /0.0d0/, XSAVE /0.0d0/
      data HAVEC/.TRUE./, HAVEF/.TRUE./, HAVEG/.TRUE./, HAVES/.TRUE./
c        C(x)    S(x)    f(x)    g(x)    C(x)    S(x)    f(x)    g(x)
      data NEEDC
     1 /.TRUE., .FALSE.,.TRUE., .TRUE., .TRUE., .FALSE.,.FALSE.,.FALSE./
      data NEEDS
     1 /.FALSE.,.TRUE., .TRUE., .TRUE., .FALSE.,.TRUE., .FALSE.,.FALSE./
      data NEEDF
     1 /.FALSE.,.FALSE.,.TRUE., .FALSE.,.TRUE., .TRUE., .TRUE., .FALSE./
      data NEEDG
     1 /.FALSE.,.FALSE.,.FALSE.,.TRUE. ,.TRUE., .TRUE., .FALSE.,.TRUE. /
c
c     Coefficients for C(x), |X| <= 1.2
c++   code for digits <= 3 is inactive
c      data pc1 / 1.00053d0, -1.12353d-1/, qc1 /1.38937d-1/
c++   code for digits > 3 & digits <= 6 is inactive
c      data pc1 / 9.99999896d-1, -1.63090954d-1, 1.06388604d-2/
c      data qc1 / 8.36467414d-2,  3.10155884d-3/
c++   code for digits > 6 & digits <= 11 is inactive
c      data pc1 / 1.0000000000042d0,  -1.8651127631106d-1,
c     *           1.5065663274457d-2, -3.1058074693185d-4/
c      data qc1 / 6.0228833908128d-2,  1.7410300558198d-3,
c     *            .6308617899210d-5/
c++   code for digits > 11 is active
      data pc1 / 9.999999999999999421d-1 ,
     *          -1.994608988261842706d-1 ,
     *           1.761939525434914045d-2 ,
     *          -5.280796513726226960d-4 ,
     *           5.477113856826871660d-6/
      data qc1 / 4.727921120104532689d-2 ,
     *           1.099572150256418851d-3 ,
     *           1.552378852769941331d-5 ,
     *           1.189389014228757184d-7/
c++   end
c
c     Coefficients for C(x), 1.2 < |X| <= 1.6
c++   code for digits <= 2 is inactive
c      data pc2 / 1.3139d0, -5.8827d-2/, qc2 / 4.7324d-1/
c++   code for digits > 2 & digits <= 5 is inactive
c      data pc2 / 9.9790890d-1, -1.5391895d-1, 9.9817933d-3/
c      data qc2 / 8.9878647d-2,  5.6003071d-3/
c++   code for digits > 5 & digits <= 8 is inactive
c      data pc2 / 1.00000440109d0, -1.83413851908d-1,
c     *           1.45776170828d-2,-2.78980705270d-4/
c      data qc2 / 6.33347445637d-2, 2.01245738369d-3,
c     *           4.03949004646d-5/
c++   code for digits > 8 & digits <= 12 is inactive
c      data pc2 / 9.999999966496876d-1, -1.980300987022688d-1,
c     *           1.735518748450023d-2, -5.069442297935788d-4,
c     *           5.059962254678234d-6/
c      data qc2 / 4.871000308997918d-2,  1.188406974004084d-3,
c     *           1.824527635843850d-5,  1.678314257874103d-7/
c++   code for digits > 12 is active
      data pc2 / 1.00000000000111043640d0  ,
     *          -2.07073360335323894245d-1 ,
     *           1.91870279431746926505d-2 ,
     *          -6.71376034694922109230d-4 ,
     *           1.02365435056105864908d-5 ,
     *          -5.68293310121870728343d-8/
      data qc2 / 3.96667496952323433510d-2 ,
     *           7.88905245052359907842d-4 ,
     *           1.01344630866749406081d-5 ,
     *           8.77945377892369265356d-8 ,
     *           4.41701374065009620393d-10/
c++   end
c
c     Coefficients for S(x), |X| <= 1.2
c++   code for digits <= 3 is inactive
c      data ps1 / 5.23677d-1, -4.67900d-2/, qs1 / 8.81622d-2/
c++   code for digits > 3 & digits <= 7 is inactive
c      data ps1 / 5.235987665d-1,-5.837763961d-2, 2.165920196d-3/
c      data qs1 / 6.474944765d-2, 1.713287588d-3/
c++   code for digits > 7 & digits <= 12 is inactive
c      data ps1 / 5.23598775598566d-1, -6.59149581139046d-2,
c     *           3.21501649828293d-3, -4.88704436240178d-5/
c      data qs1 / 5.03546388670085d-2,  1.17835980356588d-3,
c     *           1.37089875826980d-5/
c++   code for digits > 12 is active
      data ps1 / 5.2359877559829887021d-1 ,
     *          -7.0748991514452302596d-2 ,
     *           3.8778212346368287939d-3 ,
     *          -8.4555728435277680591d-5 ,
     *           6.7174846662514086196d-7/
      data qs1 / 4.1122315114238422205d-2 ,
     *           8.1709194215213447204d-4 ,
     *           9.6269087593903403370d-6 ,
     *           5.9528122767840998345d-8/
c++   end
c
c     coefficients for S(x), 1.2 < |X| <= 1.6
c++   code for digits <= 2 is inactive
c      data ps2 / 5.4766d-1, -3.7151d-2/, qs2 / 1.4559d-1/
c++   code for digits > 2 & digits <= 5 is inactive
c      data ps2 / 5.2343994d-1, -5.4828347d-2, 1.9020881d-3/
c      data qs2 / 7.1104704d-2,  2.5596274d-3/
c++   code for digits > 5 & digits <= 9 is inactive
c      data ps2 / 5.23599040498d-1, -6.46593392426d-2,
c     *           3.08030794361d-3, -4.40800854418d-5/
c      data qs2 / 5.27536681685d-2,  1.34311026821d-3,
c     *           1.90476612849d-5/
c++   code for digits > 9 & digits <= 14 is inactive
c      data ps2 / 5.2359877543509178d-1,-7.0149076634833662d-2,
c     *           3.8031581605987038d-3,-8.0964948714408156d-5,
c     *           6.1908080210052772d-7/
c      data qs2 / 4.2268067370395487d-2, 8.7642753831073237d-4,
c     *           1.1088542889789282d-5, 7.8725829545478464d-8/
c++   code for digits > 14 is active
      data ps2 / 5.23598775598344165913d-1 ,
     *          -7.37766914010191323867d-2 ,
     *           4.30730526504366510217d-3 ,
     *          -1.09540023911434994566d-4 ,
     *           1.28531043742724820610d-6 ,
     *          -5.76765815593088804567d-9/
      data qs2 / 3.53398342767472162540d-2 ,
     *           6.18224620195473216538d-4 ,
     *           6.87086265718620117905d-6 ,
     *           5.03090581246612375866d-8 ,
     *           2.05539124458579596075d-10/
c++   end
c
c     coefficients for f(x), 1.6 < |X| <= 1.9
c++   code for digits <= 5 is inactive
c      data pf1 / 3.1803519d-1, 4.2352332d-1/, qf1 / 1.6015403d0/
c++   code for digits > 5 & digits <= 8 is inactive
c      data pf1 / 3.18285252094d-1,  2.02860275713d0,
c     *           1.08108136048d0 /
c      data qf1 / 6.67171548135d0,   4.52997553972d0 /
c++   code for digits > 8 & digits <= 11 is inactive
c      data pf1 / 3.1830646311448d-1,  4.6077249266684d0,
c     *           1.1914578438074d1,   3.5555073665830d0 /
c      data qf1 / 1.4778464938936d1,   4.0902971163705d1,
c     *           1.6130432784819d1 /
c++   code for digits > 11 & digits <= 14 is inactive
c      data pf1 / 3.1830926850490599d-1,  8.0358812280394156d0,
c     *           4.8034065557792487d1,   6.9853426160102065d1,
c     *           1.3530423554038784d1 /
c      data qf1 / 2.5549161843579529d1,   1.5761100558012250d2,
c     *           2.4956199380517229d2,   6.5563064008391554d1 /
c++   code for digits > 14 is active
      data pf1 / 3.1830975293580985290d-1 ,
     *           1.2226000551672961219d1  ,
     *           1.2924886131901657025d2  ,
     *           4.3886367156695547655d2  ,
     *           4.1466722177958961672d2  ,
     *           5.6771463664185116454d1 /
      data qf1 / 3.8713003365583442831d1  ,
     *           4.1674359830705629745d2  ,
     *           1.4740030733966610568d3  ,
     *           1.5371675584895759916d3  ,
     *           2.9113088788847831515d2 /
c++   end
c
c     coefficients for f(x), 1.9 < |X| <= 2.4
c++   code for digits <= 5 is inactive
c      data pf2 / 3.1825112d-1, 5.8395951d-1/, qf2 / 2.1243944d0 /
c++   code for digits > 5 & digits <= 8 is inactive
c      data pf2 / 3.1830699932d-1, 2.9993457087d0, 2.3956340010d0/
c      data qf2 / 9.7254517756d0,  9.4814077696d0 /
c++   code for digits > 8 & digits <= 11 is inactive
c      data pf2 / 3.1830963944521d-1,  7.0770431878327d0,
c     *           2.8756083262903d1,   1.3583685742326d1 /
c      data qf2 / 2.2536994405207d1,   9.6127568469278d1,
c     *           5.7407028004031d1 /
c++   code for digits > 11 & digits <= 14 is inactive
c      data pf2 / 3.183098568640159d-1,  1.265129469683175d1,
c     *           1.219467616498339d2,   2.910033655512762d2,
c     *           9.278397828631516d1/
c      data qf2 / 4.004915302781009d1,   3.942059697951583d2,
c     *           1.001368403495691d3,   4.142676224222433d2/
c++   code for digits > 14 is active
      data pf2 / 3.183098818220169217d-1 ,
     *           1.958839410219691002d1  ,
     *           3.398371349269842400d2  ,
     *           1.930076407867157531d3  ,
     *           3.091451615744296552d3  ,
     *           7.177032493651399590d2 /
      data qf2 / 6.184271381728873709d1  ,
     *           1.085350675006501251d3  ,
     *           6.337471558511437898d3  ,
     *           1.093342489888087888d4  ,
     *           3.361216991805511494d3 /
c++   end
c
c     coefficients for f(x), 2.4 < |X|
c++   code for digits <= 3 is inactive
c      data pf3 /-8.97969d-2/
c++   code for digits > 3 & digits <= 6 is inactive
c      data pf3 / -9.67122165d-2, -3.73565920d-1/
c      data qf3 / 7.29466572d0 /
c++   code for digits > 6 & digits <= 9 is inactive
c      data pf3 /-9.67541443648d-2, -2.26566293818d0,
c      *         -3.53429821084d0 /
c      data qf3 / 2.69596939977d1,   9.72883957469d1 /
c++   code for digits > 9 & digits <= 11 is inactive
c      data pf3 /-9.6754596389025d-2, -5.5254943840897d0,
c     *          -5.8606282086171d1,  -5.2235560918394d1 /
c      data qf3 / 6.0654483020979d1,   7.8528841711294d2,
c     *           1.8592498578831d3 /
c++   code for digits > 11 & digits <= 13 is inactive
c      data pf3 /-9.675460316952504d-2, -1.023876428129288d1,
c     *          -2.712579634037998d2,  -1.766119345282127d3,
c     *          -1.043464426656267d3 /
c      data qf3 / 1.093682244053459d2,   3.155843461920557d3,
c     *           2.625634316204420d4,   4.578252057246393d4 /
c++   code for digits > 13 & digits <= 15 is inactive
c      data pf3 /-9.67546032967090380d-2,-1.64797712841245767d1,
c     *          -8.16343401784374598d2, -1.34922028171857248d4,
c     *          -6.13547113614699772d4, -2.61294753225141779d4/
c      data qf3 / 1.73871690673649114d2,  9.01827596231524147d3,
c     *           1.65946462621853184d5,  1.00105478900791339d6,
c     *           1.37012364817225972d6 /
c++   code for digits > 15 is active
      data pf3 /-9.675460329952532343d-2 ,
     *          -2.431275407194161683d1  ,
     *          -1.947621998306889176d3  ,
     *          -6.059852197160773639d4  ,
     *          -7.076806952837779823d5  ,
     *          -2.417656749061154155d6  ,
     *          -7.834914590078317336d5 /
      data qf3 / 2.548289012949732752d2  ,
     *           2.099761536857815105d4  ,
     *           6.924122509827708985d5  ,
     *           9.178823229918143780d6  ,
     *           4.292733255630186679d7  ,
     *           4.803294784260528342d7 /
c++   end
c
c     coefficients for g(x), 1.6 < |X| <= 1.9
c++   code for digits <= 4 is inactive
c      data pg1 / 1.007011d-1, 1.457780d-1/, qg1 / 2.700253d0 /
c++   code for digits > 4 & digits <= 7 is inactive
c      data pg1 / 1.012500483d-1, 7.735207446d-1, 3.878282770d-1/
c      data qg1 / 9.101956178d0,  1.002234185d1 /
c++   code for digits > 7 & digits <= 10 is inactive
c      data pg1 / 1.013095436817d-1,  1.731937984173d0,
c     *           5.036588245265d0,   1.290707246507d0 /
c      data qg1 / 1.860077430076d1,   6.901951935725d1,
c     *           4.308918659989d1 /
c++   code for digits > 10 & digits <= 13 is inactive
c      data pg1 / 1.013188096509180d-1,  2.966220554725899d0,
c     *           2.015435299505393d1,   3.155605679387908d1,
c     *           4.916012830646366d0 /
c      data qg1 / 3.079178736724045d1,   2.362874318980473d2,
c     *           4.953861258248338d2,   2.026144493403599d2 /
c++   code for digits > 13 is active
      data pg1 / 1.013206188102747985d-1 ,
     *           4.445338275505123778d0  ,
     *           5.311228134809894481d1  ,
     *           1.991828186789025318d2  ,
     *           1.962320379716626191d2  ,
     *           2.054214324985006303d1 /
      data qg1 / 4.539250196736893605d1  ,
     *           5.835905757164290666d2  ,
     *           2.544731331818221034d3  ,
     *           3.481121478565452837d3  ,
     *           1.013794833960028555d3 /
c++   end
c
c     coefficients for g(x), 1.9 < |X| <= 2.4
c++   code for digits <= 4 is inactive
c      data pg2 / 1.011711d-1,  2.239630d-1/, qg2 / 3.609237d0 /
c++   code for digits > 4 & digits <= 7 is inactive
c      data pg2 / 1.013115463d-1,  1.182021191d0,  9.886315969d-1/
c      data qg2 / 1.317219285d1,   2.101104994d1 /
c++   code for digits > 7 & digits <= 10 is inactive
c      data pg2 / 1.013202025653d-1,  2.690767013770d0,
c     *           1.283624749271d1,   5.791337587723d0 /
c      data qg2 / 2.807457005500d1,   1.598704313522d2,
c     *           1.525630385045d2 /
c++   code for digits > 10 & digits <= 13 is inactive
c      data pg2 / 1.013210509409046d-1,  4.682769769757399d0,
c     *           5.241351613346472d1,   1.411735944550041d2,
c     *           4.019756012788710d1 /
c      data qg2 / 4.773652966704634d1,   5.802036967947208d2,
c     *           1.947179327244406d3,   1.266041236960445d3 /
c++   code for digits > 13 is active
      data pg2 / 1.01321161761804586d-1 ,
     *           7.11205001789782823d0  ,
     *           1.40959617911315524d2  ,
     *           9.08311749529593938d2  ,
     *           1.59268006085353864d3  ,
     *           3.13330163068755950d2 /
      data qg2 / 7.17128596939302198d1  ,
     *           1.49051922797329229d3  ,
     *           1.06729678030580897d4  ,
     *           2.41315567213369742d4  ,
     *           1.15149832376260604d4 /
c++   end
c
c     coefficients for g(x), 2.4 < |X|
c++   code for digits <= 3 is inactive
c      data pg3 /-1.3549d-1/
c++   code for digits > 3 & digits <= 5 is inactive
c      data pg3 /-1.5382824d-1, -6.4547464d-1/, qg3 / 1.0290328d1/
c++   code for digits > 5 & digits <= 8 is inactive
c      data pg3 /-1.5398760302d-1, -4.2772857997d0,
c     *          -6.5940181141d0 /
c      data qg3 / 3.4149986477d1,   1.7071708816d2 /
c++   code for digits > 8 & digits <= 10 is inactive
c      data pg3 /-1.539896971616d-1, -1.024638314446d1,
c     *          -1.252507402120d2,  -1.029189676144d2 /
c      data qg3 / 7.292229303754d1,   1.186528673248d3,
c     *           3.844430908476d3 /
c++   code for digits > 10 & digits <= 12 is inactive
c      data pg3 /-1.53989733057971d-1, -1.86483223831639d1,
c     *          -5.66882778026550d2,  -4.16714647017489d3,
c     *          -2.14678074364341d3 /
c      data qg3 / 1.27484298075273d2,   4.40258858159927d3,
c     *           4.57583830694684d4,   1.08207883281275d5 /
c++   code for digits = 13 is inactive
c      data pg3 /-1.539897338019247d-1, -2.959078231855258d1,
c     *          -1.661867087183632d3,  -3.112796454920657d4,
c     *          -1.573536286819766d5,  -5.574884113746041d4 /
c      data qg3 / 1.985439813544440d2,   1.196693134508007d4,
c     *           2.625573864512749d5,   1.969055829311119d6,
c     *           3.629081333313312d6 /
c++   code for digits > 13 is active
      data pg3 /-1.53989733819769316d-1 ,
     *          -4.31710157823357568d1  ,
     *          -3.87754141746378493d3  ,
     *          -1.35678867813756347d5  ,
     *          -1.77758950838029676d6  ,
     *          -6.66907061668636416d6  ,
     *          -1.72590224654836845d6 /
      data qg3 / 2.86733194975899483d2  ,
     *           2.69183180396242536d4  ,
     *           1.02878693056687506d6  ,
     *           1.62095600500231646d7  ,
     *           9.38695862531635179d7  ,
     *           1.40622441123580005d8 /
c++   end
c     ------------------------------------------------------------------
      if (bigx .lt. 0.0d0) then
         bigx = 1.0d0 / D1MACH(4)
         largef = rpi / D1MACH(1)
         largeg = (rpi * largef) ** (1.0d0 / 3.0d0)
         largex = 1.0d0/sqrt(sqrt(D1MACH(1)))
      end if
      if (x .ne. xsave) then
         havec = .false.
         havef = .false.
         haveg = .false.
         haves = .false.
      end if
      ax = abs(x)
      if (ax .le. 1.6d0) then
         x4 = ax**4
         if (needc(mode) .and. .not. havec) then
            if (ax .le. 1.2d0) then
c++             code for digits <= 3 is inactive
c               c = x * (pc1(1)*x4+pc1(0)) / (qc1(1)*x4+1.0d0)
c++             code for digits > 3 & digits <= 6 is inactive
c               c = x * ((pc1(2)*x4+pc1(1))*x4+pc1(0))
c     1               / ((qc1(2)*x4+qc1(1))*x4+1.0d0)
c++             code for digits > 6 & digits <= 11 is inactive
c               c = x * (((pc1(3)*x4+pc1(2))*x4+pc1(1))*x4+pc1(0))
c     1               / (((qc1(3)*x4+qc1(2))*x4+qc1(1))*x4+1.0d0)
c++             code for digits > 11 is active
               c = x * ((((pc1(4)*x4+pc1(3))*x4+pc1(2))*x4+pc1(1))*x4+
     1                     pc1(0))
     2           / ((((qc1(4)*x4+qc1(3))*x4+qc1(2))*x4+qc1(1))*x4+1.0d0)
c++             end
            else
c++             code for digits <= 2 is inactive
c               c = x * (pc2(1)*x4+pc2(0)) / (qc2(1)*x4+1.0d0)
c++             code for digits > 2 & digits <= 5 is inactive
c               c = x * ((pc2(2)*x4+pc2(1))*x4+pc2(0))
c     1               / ((qc2(2)*x4+qc2(1))*x4+1.0d0)
c++             code for digits > 5 & digits <= 8 is inactive
c               c = x * (((pc2(3)*x4+pc2(2))*x4+pc2(1))*x4+pc2(0))
c     1               / (((qc2(3)*x4+qc2(2))*x4+qc2(1))*x4+1.0d0)
c++             code for digits > 8 & digits <= 12 is inactive
c               c = x * ((((pc2(4)*x4+pc2(3))*x4+pc2(2))*x4+pc2(1))*x4+
c     1                   pc2(0))
c     2               / ((((qc2(4)*x4+qc2(3))*x4+qc2(2))*x4+qc2(1))*x4+
c     3                   1.0d0)
c++             code for digits > 12 is active
               c = x * (((((pc2(5)*x4+pc2(4))*x4+pc2(3))*x4+pc2(2))*x4+
     1                     pc2(1))*x4+pc2(0))
     2               / (((((qc2(5)*x4+qc2(4))*x4+qc2(3))*x4+qc2(2))*x4+
     3                   qc2(1))*x4+1.0d0)
c++             end
            end if
            havec = .true.
         end if
         if (needs(mode) .and. .not. haves) then
            if (ax .le. 1.2d0) then
c++             code for digits <= 3 is inactive
c               s = x**3*(ps1(1)*x4+ps1(0)) / (qs1(1)*x4+1.0d0)
c++             code for digits > 3 & digits <= 7 is inactive
c               s = x**3*((ps1(2)*x4+ps1(1))*x4+ps1(0))
c     1                / ((qs1(2)*x4+qs1(1))*x4+1.0d0)
c++             code for digits > 7 & digits <= 12 is inactive
c               s = x**3*(((ps1(3)*x4+ps1(2))*x4+ps1(1))*x4+ps1(0))
c     1                / (((qs1(3)*x4+qs1(2))*x4+qs1(1))*x4+1.0d0)
c++             code for digits > 12 is active
               s = x**3*((((ps1(4)*x4+ps1(3))*x4+ps1(2))*x4+ps1(1))*x4+
     1                      ps1(0))
     2           / ((((qs1(4)*x4+qs1(3))*x4+qs1(2))*x4+qs1(1))*x4+1.0d0)
c++             end
            else
c++             code for digits <= 2 is inactive
c               s = x**3*(ps2(1)*x4+ps2(0)) / (qs2(1)*x4+1.0d0)
c++             code for digits > 2 & digits <= 5 is inactive
c               s = x**3*((ps2(2)*x4+ps2(1))*x4+ps2(0))
c     1              /   ((qs2(2)*x4+qs2(1))*x4+1.0d0)
c++             code for digits > 5 & digits <= 9 is inactive
c               s = x**3*(((ps2(3)*x4+ps2(2))*x4+ps2(1))*x4+ps2(0))
c     1              /   (((qs2(3)*x4+qs2(2))*x4+qs2(1))*x4+1.0d0)
c++             code for digits > 9 & digits <= 14 is inactive
c               s = x**3*((((ps2(4)*x4+ps2(3))*x4+ps2(2))*x4+ps2(1))*x4+
c     1                      ps2(0))
c     2              /   ((((qs2(4)*x4+qs2(3))*x4+qs2(2))*x4+qs2(1))*x4+
c     3                      1.0d0)
c++             code for digits > 14 is active
               s = x**3*(((((ps2(5)*x4+ps2(4))*x4+ps2(3))*x4+ps2(2))*x4+
     1                      ps2(1))*x4+ps2(0))
     2              /   (((((qs2(5)*x4+qs2(4))*x4+qs2(3))*x4+qs2(2))*x4+
     3                      qs2(1))*x4+1.0d0)
c++             end
            end if
            haves = .true.
         end if
         if ((needf(mode) .or. needg(mode)) .and. .not. havef) then
            cx = dcspxx (ax)
            sx = dsnpxx (ax)
            f = (0.5d0 - s) * cx - (0.5d0 - c) * sx
            g = (0.5d0 - c) * cx + (0.5d0 - s) * sx
            havef = .true.
         end if
      else
         if (ax .le. largex) then
            x4 = (1.0d0 / ax) ** 4
            wantf = needf(mode+4) .and. .not. havef
            if (wantf) then
               if (ax .le. 1.9d0) then
c++                code for digits <= 5 is inactive
c                  f = (pf1(1)*x4+pf1(0)) / ((qf1(1)*x4+1.0d0) * ax)
c++                code for digits > 5 & digits <= 8 is inactive
c                  f = ((pf1(2)*x4+pf1(1))*x4+pf1(0))
c     1             / (((qf1(2)*x4+qf1(1))*x4+1.0d0) * ax)
c++                code for digits > 8 & digits <= 11 is inactive
c                  f = (((pf1(3)*x4+pf1(2))*x4+pf1(1))*x4+pf1(0))
c     1             / ((((qf1(3)*x4+qf1(2))*x4+qf1(1))*x4+1.0d0) * ax)
c++                code for digits > 11 & digits <= 14 is inactive
c                  f = ((((pf1(4)*x4+pf1(3))*x4+pf1(2))*x4+pf1(1))*x4+
c     1                    pf1(0))
c     2             / (((((qf1(4)*x4+qf1(3))*x4+qf1(2))*x4+qf1(1))*x4+
c     3                    1.0d0) * ax)
c++                code for digits > 14 is active
                  f = (((((pf1(5)*x4+pf1(4))*x4+pf1(3))*x4+pf1(2))*x4+
     1                    pf1(1))*x4+pf1(0))
     2             / ((((((qf1(5)*x4+qf1(4))*x4+qf1(3))*x4+qf1(2))*x4+
     3                    qf1(1))*x4+1.0d0) * ax)
c++                end
               else if (ax .le. 2.4) then
c++                code for digits <= 5 is inactive
c                  f = (pf2(1)*x4+pf2(0)) / ((qf2(1))*x4+1.0d0) * ax)
c++                code for digits > 5 & digits <= 8 is inactive
c                  f = ((pf2(2)*x4+pf2(1))*x4+pf2(0))
c     1             / (((qf2(2)*x4+qf2(1))*x4+1.0d0) * ax)
c++                code for digits > 8 & digits <= 11 is inactive
c                  f = (((pf2(3)*x4+pf2(2))*x4+pf2(1))*x4+pf2(0))
c     1             / ((((qf2(3)*x4+qf2(2))*x4+qf2(1))*x4+1.0d0) * ax)
c++                code for digits > 11 & digits <= 14 is inactive
c                  f = ((((pf2(4)*x4+pf2(3))*x4+pf2(2))*x4+pf2(1))*x4+
c     1                    pf2(0))
c     2             / (((((qf2(4)*x4+qf2(3))*x4+qf2(2))*x4+qf2(1))*x4+
c     3                    1.0d0) * ax)
c++                code for digits > 14 is active
                  f = (((((pf2(5)*x4+pf2(4))*x4+pf2(3))*x4+pf2(2))*x4+
     1                    pf2(1))*x4+pf2(0))
     2             / ((((((qf2(5)*x4+qf2(4))*x4+qf2(3))*x4+qf2(2))*x4+
     3                    qf2(1))*x4+1.0d0) * ax)
c++                end
               else
c++                code for digits <= 3 is inactive
c                  f = (rpi + x4*pf3(0)) / ax
c++                code for digits > 3 & digits <= 6 is inactive
c                  f = (rpi + x4*(pf3(1)*x4+pf3(0)) / (qf3(1)*x4+1.0d0))
c                      / ax
c++                code for digits > 6 & digits <= 9 is inactive
c                  f = (rpi +
c     1              x4*((pf3(2)*x4+pf3(1))*x4+pf3(0))
c     2            /    ((qf3(2)*x4+qf3(1))*x4+1.0d0)) / ax
c++                code for digits > 9 & digits <= 11 is inactive
c                  f = (rpi +
c     1              x4*(((pf3(3)*x4+pf3(2))*x4+pf3(1))*x4+pf3(0))
c     2            /    (((qf3(3)*x4+qf3(2))*x4+qf3(1))*x4+1.0d0)) / ax
c++                code for digits > 11 & digits <= 13 is inactive
c                  f = (rpi +
c     1              x4*((((pf3(4)*x4+pf3(3))*x4+pf3(2))*x4+pf3(1))*x4+
c     2                   pf3(0))
c     3            /    ((((qf3(4)*x4+qf3(3))*x4+qf3(2))*x4+qf3(1))*x4+
c     4                   1.0d0)) / ax
c++                code for digits > 13 & digits <= 15 is inactive
c                  f = (rpi +
c     1              x4*(((((pf3(5)*x4+pf3(4))*x4+pf3(3))*x4+pf3(2))*x4+
c     2                   pf3(1))*x4+pf3(0))
c     3            /    (((((qf3(5)*x4+qf3(4))*x4+qf3(3))*x4+qf3(2))*x4+
c     4                   qf3(1))*x4+1.0d0)) / ax
c++                code for digits > 15 is active
                  f = (rpi +
     1              x4*((((((pf3(6)*x4+pf3(5))*x4+pf3(4))*x4+pf3(3))*x4+
     2                   pf3(2))*x4+pf3(1))*x4+pf3(0))
     3            /    ((((((qf3(6)*x4+qf3(5))*x4+qf3(4))*x4+qf3(3))*x4+
     4                   qf3(2))*x4+qf3(1))*x4+1.0d0)) / ax
c++                end
               end if
               havef = .true.
            end if
            wantg = needg(mode+4) .and. .not. haveg
            if (wantg) then
               if (ax .le. 1.9d0) then
c++                code for digits <=4 is inactive
c                  g = (pg1(1)*x4+pg1(0)) / ((qg1(1)*x4+1.0d0) * ax**3)
c++                code for digits > 4 & digits <= 7 is inactive
c                  g = ((pg1(2)*x4+pg1(1))*x4+pg1(0))
c     1             / (((qg1(2)*x4+qg1(1))*x4+1.0d0) * ax**3)
c++                code for digits > 7 & digits <= 10 is inactive
c                  g = (((pg1(3)*x4+pg1(2))*x4+pg1(1))*x4+pg1(0))
c     1             / ((((qg1(3)*x4+qg1(2))*x4+qg1(1))*x4+1.0d0) *ax**3)
c++                code for digits > 10 & digits <= 13 is inactive
c                  g = ((((pg1(4)*x4+pg1(3))*x4+pg1(2))*x4+pg1(1))*x4+
c     1                    pg1(0))
c     2             / (((((qg1(4)*x4+qg1(3))*x4+qg1(2))*x4+qg1(1))*x4+
c     3                    1.0d0) * ax**3)
c++                code for digits > 13 is active
                  g = (((((pg1(5)*x4+pg1(4))*x4+pg1(3))*x4+pg1(2))*x4+
     1                    pg1(1))*x4+pg1(0))
     2             / ((((((qg1(5)*x4+qg1(4))*x4+qg1(3))*x4+qg1(2))*x4+
     3                    qg1(1))*x4+1.0d0) * ax**3)
c++                end
               else if (ax .le. 2.4d0) then
c++                code for digits <=4 is inactive
c                  g = (pg2(1)*x4+pg2(0)) / ((qg2(1)*x4+1.0d0) * ax**3)
c++                code for digits > 4 & digits <= 7 is inactive
c                  g = ((pg2(2)*x4+pg2(1))*x4+pg2(0))
c     1             / (((qg2(2)*x4+qg2(1))*x4+1.0d0) * ax**3)
c++                code for digits > 7 & digits <= 10 is inactive
c                  g = (((pg2(3)*x4+pg2(2))*x4+pg2(1))*x4+pg2(0))
c     1             / ((((qg2(3)*x4+qg2(2))*x4+qg2(1))*x4+1.0d0) *ax**3)
c++                code for digits > 10 & digits <= 13 is inactive
c                  g = ((((pg2(4)*x4+pg2(3))*x4+pg2(2))*x4+pg2(1))*x4+
c     1                    pg2(0))
c     2             / (((((qg2(4)*x4+qg2(3))*x4+qg2(2))*x4+qg2(1))*x4+
c     3                    1.0d0) * ax**3)
c++                code for digits > 13 is active
                  g = (((((pg2(5)*x4+pg2(4))*x4+pg2(3))*x4+pg2(2))*x4+
     1                     pg2(1))*x4+pg2(0))
     2             / ((((((qg2(5)*x4+qg2(4))*x4+qg2(3))*x4+qg2(2))*x4+
     3                    qg2(1))*x4+1.0d0) * ax**3)
c++                end
               else
c++                code for digits <=3 is inactive
c                  g = (rpisq + x4*pg3(0)) / ax**3
c++                code for digits > 3 & digits <= 5 is inactive
c                  g = (rpisq + x4*(pg3(1)*x4+pg3(0))
c     1            /    (qg3(1)*x4+1.0d0)) / ax**3
c++                code for digits > 5 & digits <= 8 is inactive
c                  g = (rpisq + x4*((pg3(2)*x4+pg3(1))*x4+pg3(0))
c     1            /    ((qg3(2)*x4+qg3(1))*x4+1.0d0)) / ax**3
c++                code for digits > 8 & digits <= 10 is inactive
c                  g = (rpisq +
c     1              x4*(((pg3(3)*x4+pg3(2))*x4+pg3(1))*x4+pg3(0))
c     2            /    (((qg3(3)*x4+qg3(2))*x4+qg3(1))*x4+1.0d0))
c     3            / ax**3
c++                code for digits > 10 & digits <= 12 is inactive
c                  g = (rpisq +
c     1              x4*((((pg3(4)*x4+pg3(3))*x4+pg3(2))*x4+pg3(1))*x4+
c     2                     pg3(0))
c     3            /    ((((qg3(4)*x4+qg3(3))*x4+qg3(2))*x4+qg3(1))*x4+
c     4                     1.0d0)) / ax**3
c++                code for digits = 13 is inactive
c                  g = (rpisq +
c     1              x4*(((((pg3(5)*x4+pg3(4))*x4+pg3(3))*x4+pg3(2))*x4+
c     2                      pg3(1))*x4+pg3(0))
c     3            /    (((((qg3(5)*x4+qg3(4))*x4+qg3(3))*x4+qg3(2))*x4+
c     4                      qg3(1))*x4+1.0d0)) / ax**3
c++                code for digits > 13 is active
                  g = (rpisq +
     1              x4*((((((pg3(6)*x4+pg3(5))*x4+pg3(4))*x4+pg3(3))*x4+
     2                   pg3(2))*x4+pg3(1))*x4+pg3(0))
     3            /    ((((((qg3(6)*x4+qg3(5))*x4+qg3(4))*x4+qg3(3))*x4+
     4                   qg3(2))*x4+qg3(1))*x4+1.0d0)) / ax**3
c++                end
               end if
               haveg = .true.
            end if
         else
            wantf = needf(mode)
            if (wantf) then
               if (x .le. largef) then
                  f = rpi / ax
               else
                  f = 0.0d0
               end if
            end if
            wantg = needg(mode)
            if (wantg) then
               if (x .le. largeg) then
                  g = rpisq / ax**3
               else
                  g = 0.0d0
               end if
            end if
         end if
         wantc = (needc(mode+4) .or. needs(mode+4)) .and. .not. havec
         if (wantc .or. x.lt.0.0d0) then
            cx = dcspxx (ax)
            sx = dsnpxx (ax)
            if (wantc) then
               c = 0.5d0 + f*sx - g*cx
               s = 0.5d0 - f*cx - g*sx
               if (x .lt. 0.0) then
                  c = -c
                  s = -s
               end if
               havec = .true.
            end if
            if (x .lt. 0.0) then
c              We COULD do the following before the preceeding, and then
c              not put in a test in the preceeding for x .lt. 0, but
c              even though the results are mathematically identical, we
c              would have some cancellation above if we did so.
               if (wantg) g = cx + sx - g
               if (wantf) f = cx - sx - f
            end if
          end if
      end if
      xsave = x
      go to (1, 2, 3, 4), MODE
    1    DFREN1 = c
         return
    2    DFREN1 = s
         return
    3    DFREN1 = f
         return
    4    DFREN1 = g
         return
      end
      DOUBLE PRECISION FUNCTION DSINPX (X)

c*********************************************************************72
c
cc DSINPX evaluates sin(pi*x).
c
C>> 1996-01-29 SCOSPX WVS JPL Add better acknowledgement for origins.
C>> 1994-10-20 DSINPX Krogh  Changes to use M77CON
C>> 1994-04-22 DSINPX CLL Made SP and DP codes similar.
C>> 1993-05-06 DSINPX WVS JPL Convert from NSWC to Math 77
c--D replaces "?": ?SINPX, ?ERM1
C ----------------------------------------------------------------------
c This procedure was originally procedure SIN1 from the Naval Surface
c Warfare Center library.
C ----------------------------------------------------------------------
C
C                        EVALUATION OF SIN(PI*X)
C
C                             --------------
C
C     THE EXPANSION FOR SIN(PI*A) (ABS(A) .LE. 1/4) USING A1,...,A13
C     IS ACCURATE TO WITHIN 2 UNITS OF THE 40-TH SIGNIFICANT DIGIT, AND
C     THE EXPANSION FOR COS(PI*A) (ABS(A) .LE. 1/4) USING B1,...,B13
C     IS ACCURATE TO WITHIN 4 UNITS OF THE 40-TH SIGNIFICANT DIGIT.
c
c     The polynomials using coefficients SA0, ... SA6 and SB1, ..., SB6
c     give approximations whose largest observed relative error in the
c     relevant intervals is 0.129d-14.
c     We will use this latter approximation when the machine epsilon
c     is larger than 0.2d-14.
C-----------------------------------------------------------------------
      integer N
      double precision D1MACH
      DOUBLE PRECISION A, BIG, CUTOFF, EPS, PI, T, W, X
      DOUBLE PRECISION A1, A2, A3, A4, A5, A6, A7, A8, A9, A10,
     *                 A11, A12, A13
      DOUBLE PRECISION B1, B2, B3, B4, B5, B6, B7, B8, B9, B10,
     *                 B11, B12, B13
      DOUBLE PRECISION SA0, SA1, SA2, SA3, SA4, SA5, SA6
      DOUBLE PRECISION      SB1, SB2, SB3, SB4, SB5, SB6
      SAVE BIG, EPS
C------------------------
      PARAMETER ( PI = 3.141592653589793238462643383279502884197D+00 )
      parameter ( CUTOFF = 0.2d-14 )
C------------------------
      data EPS / -1.0d0 /
      DATA SA0 /.314159265358979D+01/, SA1 /-.516771278004995D+01/,
     *     SA2 /.255016403987327D+01/, SA3 /-.599264528932149D+00/,
     *     SA4 /.821458689493251D-01/, SA5 /-.737001831310553D-02/,
     *     SA6 /.461514425296398D-03/
      DATA SB1 /-.493480220054460D+01/, SB2 /.405871212639605D+01/,
     *     SB3 /-.133526276691575D+01/, SB4 /.235330543508553D+00/,
     *     SB5 /-.258048861575714D-01/, SB6 /.190653140279462D-02/
      DATA A1  /-.1028083791780141522795259479153765743002D+00/,
     *     A2  / .3170868848763100170457042079710451905600D-02/,
     *     A3  /-.4657026956105571623449026167864697920000D-04/,
     *     A4  / .3989844942879455643410226655783424000000D-06/,
     *     A5  /-.2237397227721999776371894030796800000000D-08/,
     *     A6  / .8847045483056962709715066675200000000000D-11/,
     *     A7  /-.2598715447506450292885585920000000000000D-13/,
     *     A8  / .5893449774331011070033920000000000000000D-16/,
     *     A9  /-.1062975472045522550784000000000000000000D-18/,
     *     A10 / .1561182648301780992000000000000000000000D-21/,
     *     A11 /-.1903193516670976000000000000000000000000D-24/,
     *     A12 / .1956617650176000000000000000000000000000D-27/,
     *     A13 /-.1711276032000000000000000000000000000000D-30/
C------------------------
      DATA B1  /-.3084251375340424568385778437461297229882D+00/,
     *     B2  / .1585434424381550085228521039855226435920D-01/,
     *     B3  /-.3259918869273900136414318317506279360000D-03/,
     *     B4  / .3590860448591510079069203991239232000000D-05/,
     *     B5  /-.2461136950494199754009084061808640000000D-07/,
     *     B6  / .1150115912797405152263195572224000000000D-09/,
     *     B7  /-.3898073171259675439899172864000000000000D-12/,
     *     B8  / .1001886461636271969091584000000000000000D-14/,
     *     B9  /-.2019653396886572027084800000000000000000D-17/,
     *     B10 / .3278483561466560512000000000000000000000D-20/,
     *     B11 /-.4377345082051788800000000000000000000000D-23/,
     *     B12 / .4891532381388800000000000000000000000000D-26/,
     *     B13 /-.4617089843200000000000000000000000000000D-29/
C------------------------
      IF (eps .LT. 0.0d0) then
         eps = D1MACH(3)
         BIG = 1.0d0/eps
      endif
C------------------------
      A = ABS(X)
      IF (A .GE. BIG) THEN
         CALL DERM1 ('DSINPX',1,2,
     1   'No precision because ABS(X) is too large','X',X,'.')
         DSINPX = 0.D0
         RETURN
      END IF
      N = A
      T = N
      A = A - T
      IF (A .GT. 0.75D0) GO TO 20
      IF (A .LT. 0.25D0) GO TO 21
C
C                    0.25 .LE. A .LE. 0.75
C
      A = 0.25D0 + (0.25D0 - A)
      if(eps .lt. cutoff) then
         T = 16.D0*A*A
         DSINPX = (((((((((((((B13*T + B12)*T + B11)*T + B10)*T + B9)*T+
     *                B8)*T + B7)*T + B6)*T + B5)*T + B4)*T + B3)*T +
     *                B2)*T + B1)*T + 0.5D0) + 0.5D0
      else
         T = A*A
         DSINPX = ((((((SB6*T + SB5)*T + SB4)*T + SB3)*T + SB2)*T
     *                  + SB1)*T + 0.5D0) + 0.5D0
      endif
      GO TO 30
C
C                 A .LT. 0.25  OR  A .GT. 0.75
C
   20 A = 0.25D0 + (0.75D0 - A)
   21 continue
      if(eps .lt. cutoff) then
         T = 16.D0*A*A
         W = (((((((((((((A13*T + A12)*T + A11)*T + A10)*T + A9)*T +
     *            A8)*T + A7)*T + A6)*T + A5)*T + A4)*T + A3)*T +
     *            A2)*T + A1)*T + 0.5D0) + 0.5D0
         DSINPX = PI*A*W
      else
         T = A*A
         DSINPX = ((((((SA6*T + SA5)*T + SA4)*T + SA3)*T + SA2)*T
     *                  + SA1)*T + SA0)*A
      endif
C
C                        TERMINATION
C
   30 IF (X .LT. 0.0D0) DSINPX = - DSINPX
      IF (MOD(N,2) .NE. 0) DSINPX = - DSINPX
      RETURN
      END
c--D replaces "?": ?SNPXX, ?COSPX, ?SINPX
c>> 1996-01-08 DSNPXX WV Snyder Original code

      double precision function DSNPXX (X)

c*********************************************************************72
c
cc DSNPXX carefully evaluates sin(pi*x*x/2).
c
c SIN(PI * X * X / 2) carefully to avoid loss of precision for large X

      double precision X

c DSINPX is used to compute SIN(PI * X)

      double precision D1MACH, DCOSPX, DSINPX
      external D1MACH, DCOSPX, DSINPX

c BIGX = 1 / round-off = biggest integer exactly representable by F.P.
c    If X > BIGX then to the working precision x**2 is an integer (which
c    we assume to be a multiple of four), so sin(pi/2 * x**2) = 0.
c N = [X], and later [K F]
c F = X - N = fractional part of X
c K = [ N / 2 ]
c J = N mod 2
c G = K F - M = fractional part of K F

      integer J, K, N
      double precision BIGX, F, G
      save BIGX
      data BIGX /-1.0/

      if (bigx .lt. 0.0d0) bigx = 1.0d0 / d1mach(4)
      f = abs(x)
      if (f .gt. bigx) then
c       Assume x is an even integer.
        dsnpxx = 0.0d0
        return
      endif
      n = f
      f = f - n
      k = n / 2
      j = mod(n,2)
      g = k * f
      n = g
      g = g - n
      if (j .eq. 0) then
        dsnpxx = dsinpx(0.5d0*f*f + g + g)
      else
        dsnpxx = dcospx(0.5d0*f*f + f + g + g)
      endif
      return
      end
      SUBROUTINE ERFIN ( )

c*********************************************************************72
c
cc ERFIN is the final function called when an error is encountered.
c
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-11-11 CLL Typing all variables.
C>> 1985-09-23 ERFIN  Lawson  Initial code.
C
      integer idelta, ialpha
      COMMON/M77ERR/IDELTA,IALPHA
      SAVE /M77ERR/
C
 1003 FORMAT(1X,72('$')/' ')
      PRINT 1003
      IF (IALPHA.GE.2) STOP
      RETURN
      END
      SUBROUTINE ERMSG(SUBNAM,INDIC,LEVEL,MSG,FLAG)

c*********************************************************************72
c
cc ERMSG prints an error message.
c
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
c>> 1995-09-15 ERMSG  Krogh Remove '0' in format.
C>> 1994-11-11 ERMSG  Krogh   Declared all vars.
C>> 1992-10-20 ERMSG  WV Snyder  added ERLSET, ERLGET
C>> 1985-09-25 ERMSG  Lawson  Initial code.
C
C     --------------------------------------------------------------
C
C     Four entries: ERMSG, ERMSET, ERLGET, ERLSET
C     ERMSG initiates an error message. This subr also manages the
C     saved value IDELOC and the saved COMMON block M77ERR to
C     control the level of action. This is intended to be the
C     only subr that assigns a value to IALPHA in COMMON.
C     ERMSET resets IDELOC & IDELTA.  ERLGET returns the last value
C     of LEVEL passed to ERMSG.  ERLSET sets the last value of LEVEL.
C     ERLSET and ERLGET may be used together to determine the level
C     of error that occurs during execution of a routine that uses
C     ERMSG.
C
C     --------------------------------------------------------------
C     SUBROUTINE ARGUMENTS
C     --------------------
C     SUBNAM   A name that identifies the subprogram in which
C              the error occurs.
C
C     INDIC    An integer printed as part of the mininal error
C              message. It together with SUBNAM can be used to
C              uniquely identify an error.
C
C     LEVEL    The user sets LEVEL=2,0,or -2 to specify the
C              nominal action to be taken by ERMSG. The
C              subroutine ERMSG contains an internal variable
C              IDELTA, whose nominal value is zero. The
C              subroutine will compute IALPHA = LEVEL + IDELTA
C              and proceed as follows:
C              If (IALPHA.GE.2)        Print message and STOP.
C              If (IALPHA=-1,0,1)      Print message and return.
C              If (IALPHA.LE.-2)       Just RETURN.
C
C     MSG      Message to be printed as part of the diagnostic.
C
C     FLAG     A single character,which when set to '.' will
C              call the subroutine ERFIN and will just RETURN
C              when set to any other character.
C
C     --------------------------------------------------------------
C
C     C.Lawson & S.Chan, JPL, 1983 Nov
C
C     ------------------------------------------------------------------
      INTEGER OLDLEV
      INTEGER IDELOC, LEVEL, IDELTA, IALPHA, INDIC, IDEL
      COMMON/M77ERR/IDELTA,IALPHA
      CHARACTER*(*) SUBNAM,MSG
      CHARACTER*1 FLAG
      SAVE/M77ERR/,IDELOC,OLDLEV
      DATA IDELOC/0/, OLDLEV /0/
      OLDLEV = LEVEL
      IDELTA = IDELOC
      IALPHA = LEVEL + IDELTA
      IF (IALPHA.GE.-1) THEN
c
c            Setting FILE = 'CON' works for MS/DOS systems.
c
c
        WRITE (*,1001) SUBNAM,INDIC
        WRITE (*,*) MSG
        IF (FLAG.EQ.'.') CALL ERFIN
      ENDIF
      RETURN
C
 1001 FORMAT(1X/' ',72('$')/' SUBPROGRAM ',A,' REPORTS ERROR NO. ',I4)
C
C
      ENTRY ERMSET(IDEL)

c*********************************************************************72
c
cc ERMSET ???
c
      IDELTA=IDEL
      IDELOC=IDEL
      RETURN
C
C
      ENTRY ERLSET (LEVEL)

c*********************************************************************72
c
cc ERLSET ???
c
      OLDLEV = LEVEL
      RETURN
C
C     
      ENTRY ERLGET (LEVEL)

c*********************************************************************72
c
cc ERLGET ???
c
      LEVEL = OLDLEV
      RETURN
      END
