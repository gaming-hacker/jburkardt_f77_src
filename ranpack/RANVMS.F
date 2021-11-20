C  RANPACKVMS.F  28 February 1992
C
      function coss(x)
c
c***********************************************************************
c
c  COSS returns the cosine and sine of X as the real and imaginary
c  parts of a complex number.  On the Cray, COSS is a built in routine,
c  and is faster than calling COS and SIN separately.
c
c  X      Input, REAL X, the number whose cosine and sine are desired.
c
c  COSS   Output, COMPLEX COSS, a complex number whose real part is
c         COS(X) and whose imaginary part is SIN(X).
c
c           REAL(COSS)=COS(X)
c           AIMAG(COSS)=SIN(X)
c
      complex coss
      real x
c
      coss=cmplx(cos(x),sin(x))

      return
      end
      subroutine ran20(array,n)
c
c***********************************************************************
c
c  RAN20 is a vectorized random number generator for the Cray.  It is 
c  written in CAL by Oscar Buneman of Stanford.  
c
c  On other machines, RAN20 simply calls RANDOM(ISEED).
c
c  RAN20 was obtained over NETLIB.
c
c  ARRAY  Output, REAL ARRAY(N), an array of random values.
c
c  N      Input, INTEGER N, the number of random values desired.
c
      integer n
c
      real array(n)
      integer i
      integer iseed
      real random
c
      save iseed
c
      data iseed /1234567/
c
      do i=1,n
        array(i)=random(iseed)
      enddo

      return
      end
      function random(iseed)
c
c***********************************************************************
c
c  RANDOM generates uniformly distributed random numbers in (0,1).
c
c  RANDOM is an interface betwen RANPACK and the local system random 
c  number generator.  Therefore, the exact use and performance of RANDOM 
c  will vary from system to system.  
c
c  On UNICOS, RANDOM calls RANF().  
c
c  On VMS, RANDOM calls RAN(ISEED).
c
c  On the SGI, RANDOM calls the double precision routine RAND().
c
c  On the ALPHA, RANDOM calls the single precision routine RAND().
c 
c  ISEED  Input/output, INTEGER ISEED, possible input to the system 
c         random number generator.  On output, ISEED may have been 
c         changed, depending on the routine called.
c
c  RANDOM Output, REAL RANDOM, a random number in the range (0.0, 1.0).
 
      integer iseed
      double precision rand
      real random
c
      random=ran(iseed)

      if(random.le.0.0.or.random.ge.1.0)then
        random=0.5
      endif

      return
      end
      function ranf()
c
c***********************************************************************
c
c  Emulation of the Cray RANF/RANGET/RANSET routines.
c
c  Notes:
c
c  No attempt is made to reproduce the actual output of RANF.
c  This RANF returns uniformly distributed pseudorandom numbers
c  between 0 and 1, but not the same sequence that would be
c  produced by the true RANF, which also uses higher precision
c  arithmetic and is vectorizable.
c
c  The Cray RANGET and RANSET are callable either as subroutines
c  or functions.  These emulations can only be called as subroutines.
c
      integer iseed
      integer iset
      integer jseed
      real ranf
c
      iset=0
      iseed=0
      jseed=0
      call seeder(iset,iseed,jseed)

      jseed = jseed * 125
      jseed = jseed - (jseed/2796203) * 2796203
      ranf = float(jseed) / 2796203.0e0

      iset=1
      call seeder(iset,iseed,jseed)

      return
      end
      subroutine ranget(jseed)
c
c***********************************************************************
c
      integer iseed
      integer iset
      integer jseed
c
      iset=0
      iseed=0
      jseed=0

      call seeder(iset,iseed,jseed)

      return
      end
      subroutine ranset(iseed)
c
c***********************************************************************
c
      integer iseed
      integer iset
      integer jseed
c
      iset=1
      jseed=iseed

      call seeder(iset,iseed,jseed)

      return
      end
      subroutine seeder(iset,iseed,jseed)
c
c***********************************************************************
c
c  If ISET=0, SEEDER is being called with the request to return
c  the current values of ISEED and JSEED.  If SEEDER has never been
c  called before, it must set both to a default value.
c
c  If ISET=1, SEEDER is being asked to store the current values
c  of ISEED and JSEED.
 
      integer icall
      integer iseed
      integer iset
      integer jseed
      integer kseed
      integer lseed
c
      save icall
      save kseed
      save lseed
c
      data icall /0/
c
      if(iset.eq.1)then
        kseed=iseed
        lseed=jseed
      elseif(iset.eq.0.and.icall.eq.0)then
        kseed=100001
        lseed=kseed
        iseed=kseed
        jseed=kseed
      else
        iseed=kseed
        jseed=lseed
      endif

      icall=1

      return
      end
      DOUBLE PRECISION FUNCTION D1MACH(I)
c
c***********************************************************************
c
C  D1MACH returns double precision machine constants.
C
C  Assuming that the internal representation of a double precision number is 
C  in base B, with T the number of base-B digits in the mantissa, and EMIN the 
C  smallest possible exponent and EMAX the largest possible exponent, then
C
C    D1MACH(1) = B**(EMIN-1), the smallest positive magnitude.
C    D1MACH(2) = B**EMAX*(1-B**(-T)), the largest magnitude.
C    D1MACH(3) = B**(-T), the smallest relative spacing.
C    D1MACH(4) = B**(1-T), the largest relative spacing.
C    D1MACH(5) = LOG10(B).
C
C  To alter this function for a particular environment, the desired set of DATA 
C  statements should be activated by removing the C from column 1.  On rare 
C  machines, a STATIC statement may need to be added, but probably more systems 
C  prohibit than require it.
C
C  For IEEE-arithmetic machines (binary standard), one of the first two sets of 
C  constants below should be appropriate.
C
C  Where possible, octal or hexadecimal constants have been used to specify the 
C  constants exactly, which has in some cases required the use of EQUIVALENCED
C  integer arrays.
C
      INTEGER SMALL(4)
      INTEGER LARGE(4)
      INTEGER RIGHT(4)
      INTEGER DIVER(4)
      INTEGER LOG10(4)
C
      DOUBLE PRECISION DMACH(5)
C
      EQUIVALENCE (DMACH(1),SMALL(1))
      EQUIVALENCE (DMACH(2),LARGE(1))
      EQUIVALENCE (DMACH(3),RIGHT(1))
      EQUIVALENCE (DMACH(4),DIVER(1))
      EQUIVALENCE (DMACH(5),LOG10(1))
C
C  IEEE arithmetic machines, such as the AT&T 3B series and Motorola 68000 
C  based machines such as the SUN 3 and AT&T PC 7300, in which the most 
C  significant byte is stored first.
C
       DATA SMALL(1),SMALL(2) /    1048576,          0 /
       DATA LARGE(1),LARGE(2) / 2146435071,         -1 /
       DATA RIGHT(1),RIGHT(2) / 1017118720,          0 /
       DATA DIVER(1),DIVER(2) / 1018167296,          0 /
       DATA LOG10(1),LOG10(2) / 1070810131, 1352628735 /
C
C  IEEE arithmetic machines and 8087-based micros, such as the IBM PC,
C  AT&T 6300, DEC PMAX, in which the most significant byte is stored last.
C
C      DATA SMALL(1),SMALL(2) /          0,    1048576 /
C      DATA LARGE(1),LARGE(2) /         -1, 2146435071 /
C      DATA RIGHT(1),RIGHT(2) /          0, 1017118720 /
C      DATA DIVER(1),DIVER(2) /          0, 1018167296 /
C      DATA LOG10(1),LOG10(2) / 1352628735, 1070810131 /
C
C  ALLIANT FX/8 UNIX FORTRAN compiler.
C
C      DATA DMACH(1) / 2.22507385850721D-308 /
C      DATA DMACH(2) / 1.79769313486231D+308 /
C      DATA DMACH(3) / 1.1101827117665D-16 /
C      DATA DMACH(4) / 2.2203654423533D-16 /
C      DATA DMACH(5) / 3.01029995663981E-1 /
C
C  AMDAHL machines.
C
C      DATA SMALL(1),SMALL(2) /    1048576,          0 /
C      DATA LARGE(1),LARGE(2) / 2147483647,         -1 /
C      DATA RIGHT(1),RIGHT(2) /  856686592,          0 /
C      DATA DIVER(1),DIVER(2) /  873463808,          0 /
C      DATA LOG10(1),LOG10(2) / 1091781651, 1352628735 /
C
C  BURROUGHS 1700 system.
C
C      DATA SMALL(1) / ZC00800000 /
C      DATA SMALL(2) / Z000000000 /
C
C      DATA LARGE(1) / ZDFFFFFFFF /
C      DATA LARGE(2) / ZFFFFFFFFF /
C
C      DATA RIGHT(1) / ZCC5800000 /
C      DATA RIGHT(2) / Z000000000 /
C
C      DATA DIVER(1) / ZCC6800000 /
C      DATA DIVER(2) / Z000000000 /
C
C      DATA LOG10(1) / ZD00E730E7 /
C      DATA LOG10(2) / ZC77800DC0 /
C
C  BURROUGHS 5700 system.
C
C      DATA SMALL(1) / O1771000000000000 /
C      DATA SMALL(2) / O0000000000000000 /
C
C      DATA LARGE(1) / O0777777777777777 /
C      DATA LARGE(2) / O0007777777777777 /
C
C      DATA RIGHT(1) / O1461000000000000 /
C      DATA RIGHT(2) / O0000000000000000 /
C
C      DATA DIVER(1) / O1451000000000000 /
C      DATA DIVER(2) / O0000000000000000 /
C
C      DATA LOG10(1) / O1157163034761674 /
C      DATA LOG10(2) / O0006677466732724 /
C
C  BURROUGHS 6700/7700 systems.
C
C      DATA SMALL(1) / O1771000000000000 /
C      DATA SMALL(2) / O7770000000000000 /
C
C      DATA LARGE(1) / O0777777777777777 /
C      DATA LARGE(2) / O7777777777777777 /
C
C      DATA RIGHT(1) / O1461000000000000 /
C      DATA RIGHT(2) / O0000000000000000 /
C
C      DATA DIVER(1) / O1451000000000000 /
C      DATA DIVER(2) / O0000000000000000 /
C
C      DATA LOG10(1) / O1157163034761674 /
C      DATA LOG10(2) / O0006677466732724 /
C
C  CDC CYBER 170/180 series using NOS
C
C      DATA SMALL(1) / O"00604000000000000000" /
C      DATA SMALL(2) / O"00000000000000000000" /
C
C      DATA LARGE(1) / O"37767777777777777777" /
C      DATA LARGE(2) / O"37167777777777777777" /
C
C      DATA RIGHT(1) / O"15604000000000000000" /
C      DATA RIGHT(2) / O"15000000000000000000" /
C
C      DATA DIVER(1) / O"15614000000000000000" /
C      DATA DIVER(2) / O"15010000000000000000" /
C
C      DATA LOG10(1) / O"17164642023241175717" /
C      DATA LOG10(2) / O"16367571421742254654" /
C
C  CDC CYBER 170/180 series using NOS/VE
C
C      DATA SMALL(1) / Z"3001800000000000" /
C      DATA SMALL(2) / Z"3001000000000000" /
C
C      DATA LARGE(1) / Z"4FFEFFFFFFFFFFFE" /
C      DATA LARGE(2) / Z"4FFE000000000000" /
C
C      DATA RIGHT(1) / Z"3FD2800000000000" /
C      DATA RIGHT(2) / Z"3FD2000000000000" /
C
C      DATA DIVER(1) / Z"3FD3800000000000" /
C      DATA DIVER(2) / Z"3FD3000000000000" /
C
C      DATA LOG10(1) / Z"3FFF9A209A84FBCF" /
C      DATA LOG10(2) / Z"3FFFF7988F8959AC" /
C
C  CDC CYBER 200 series
C
C      DATA SMALL(1) / X'9000400000000000' /
C      DATA SMALL(2) / X'8FD1000000000000' /
C
C      DATA LARGE(1) / X'6FFF7FFFFFFFFFFF' /
C      DATA LARGE(2) / X'6FD07FFFFFFFFFFF' /
C
C      DATA RIGHT(1) / X'FF74400000000000' /
C      DATA RIGHT(2) / X'FF45000000000000' /
C
C      DATA DIVER(1) / X'FF75400000000000' /
C      DATA DIVER(2) / X'FF46000000000000' /
C
C      DATA LOG10(1) / X'FFD04D104D427DE7' /
C      DATA LOG10(2) / X'FFA17DE623E2566A' /
C
C  CDC 6000/7000 series using FTN4.
C
C      DATA SMALL(1) / 00564000000000000000B /
C      DATA SMALL(2) / 00000000000000000000B /
C
C      DATA LARGE(1) / 37757777777777777777B /
C      DATA LARGE(2) / 37157777777777777774B /
C
C      DATA RIGHT(1) / 15624000000000000000B /
C      DATA RIGHT(2) / 00000000000000000000B /
C
C      DATA DIVER(1) / 15634000000000000000B /
C      DATA DIVER(2) / 00000000000000000000B /
C
C      DATA LOG10(1) / 17164642023241175717B /
C      DATA LOG10(2) / 16367571421742254654B /
C
C  CDC 6000/7000 series using FTN5.
C
C      DATA SMALL(1) / O"00564000000000000000" /
C      DATA SMALL(2) / O"00000000000000000000" /
C
C      DATA LARGE(1) / O"37757777777777777777" /
C      DATA LARGE(2) / O"37157777777777777774" /
C
C      DATA RIGHT(1) / O"15624000000000000000" /
C      DATA RIGHT(2) / O"00000000000000000000" /
C
C      DATA DIVER(1) / O"15634000000000000000" /
C      DATA DIVER(2) / O"00000000000000000000" /
C
C      DATA LOG10(1) / O"17164642023241175717" /
C      DATA LOG10(2) / O"16367571421742254654" /
C
C  CONVEX C-1
C
C      DATA SMALL(1),SMALL(2) / '00100000'X, '00000000'X /
C      DATA LARGE(1),LARGE(2) / '7FFFFFFF'X, 'FFFFFFFF'X /
C      DATA RIGHT(1),RIGHT(2) / '3CC00000'X, '00000000'X /
C      DATA DIVER(1),DIVER(2) / '3CD00000'X, '00000000'X /
C      DATA LOG10(1),LOG10(2) / '3FF34413'X, '509F79FF'X /
C
C  CONVEX C-120 (native mode) with or without -R8 option
C
C      DATA DMACH(1) / 5.562684646268007D-309 /
C      DATA DMACH(2) / 8.988465674311577D+307 /
C      DATA DMACH(3) / 1.110223024625157D-016 /
C      DATA DMACH(4) / 2.220446049250313D-016 /
C      DATA DMACH(5) / 3.010299956639812D-001 /
C
C  CONVEX C-120 (IEEE mode) with or without -R8 option
C
C      DATA DMACH(1) / 2.225073858507202D-308 /
C      DATA DMACH(2) / 1.797693134862315D+308 /
C      DATA DMACH(3) / 1.110223024625157D-016 /
C      DATA DMACH(4) / 2.220446049250313D-016 /
C      DATA DMACH(5) / 3.010299956639812D-001 /
C
C  CRAY 1, 2, XMP and YMP.
C
C      DATA SMALL(1) / 201354000000000000000B /
C      DATA SMALL(2) / 000000000000000000000B /
C
C      DATA LARGE(1) / 577767777777777777777B /
C      DATA LARGE(2) / 000007777777777777776B /
C
C      DATA RIGHT(1) / 376434000000000000000B /
C      DATA RIGHT(2) / 000000000000000000000B /
C
C      DATA DIVER(1) / 376444000000000000000B /
C      DATA DIVER(2) / 000000000000000000000B /
C
C      DATA LOG10(1) / 377774642023241175717B /
C      DATA LOG10(2) / 000007571421742254654B /
C
C  DATA GENERAL ECLIPSE S/200
C  Note - It may be appropriate to include the line: STATIC DMACH(5)
C
C      DATA SMALL /20K,3*0/
C      DATA LARGE /77777K,3*177777K/
C      DATA RIGHT /31420K,3*0/
C      DATA DIVER /32020K,3*0/
C      DATA LOG10 /40423K,42023K,50237K,74776K/
C
C  ELXSI 6400, assuming REAL*8 is the default DOUBLE PRECISION type.
C
C      DATA SMALL(1), SMALL(2) / '00100000'X,'00000000'X /
C      DATA LARGE(1), LARGE(2) / '7FEFFFFF'X,'FFFFFFFF'X /
C      DATA RIGHT(1), RIGHT(2) / '3CB00000'X,'00000000'X /
C      DATA DIVER(1), DIVER(2) / '3CC00000'X,'00000000'X /
C      DATA LOG10(1), DIVER(2) / '3FD34413'X,'509F79FF'X /
C
C  HARRIS 220
C
C      DATA SMALL(1),SMALL(2) / '20000000, '00000201 /
C      DATA LARGE(1),LARGE(2) / '37777777, '37777577 /
C      DATA RIGHT(1),RIGHT(2) / '20000000, '00000333 /
C      DATA DIVER(1),DIVER(2) / '20000000, '00000334 /
C      DATA LOG10(1),LOG10(2) / '23210115, '10237777 /
C
C  HARRIS SLASH 6 and SLASH 7
C
C      DATA SMALL(1),SMALL(2) / '20000000, '00000201 /
C      DATA LARGE(1),LARGE(2) / '37777777, '37777577 /
C      DATA RIGHT(1),RIGHT(2) / '20000000, '00000333 /
C      DATA DIVER(1),DIVER(2) / '20000000, '00000334 /
C      DATA LOG10(1),LOG10(2) / '23210115, '10237777 /
C
C  HONEYWELL DPS 8/70 and 600/6000 series.
C
C      DATA SMALL(1),SMALL(2) / O402400000000, O000000000000 /
C      DATA LARGE(1),LARGE(2) / O376777777777, O777777777777 /
C      DATA RIGHT(1),RIGHT(2) / O604400000000, O000000000000 /
C      DATA DIVER(1),DIVER(2) / O606400000000, O000000000000 /
C      DATA LOG10(1),LOG10(2) / O776464202324, O117571775714 /
C
C  HP 2100, three word double precision option with FTN4.
C
C      DATA SMALL(1), SMALL(2), SMALL(3) / 40000B,       0,       1 /
C      DATA LARGE(1), LARGE(2), LARGE(3) / 77777B, 177777B, 177776B /
C      DATA RIGHT(1), RIGHT(2), RIGHT(3) / 40000B,       0,    265B /
C      DATA DIVER(1), DIVER(2), DIVER(3) / 40000B,       0,    276B /
C      DATA LOG10(1), LOG10(2), LOG10(3) / 46420B,  46502B,  77777B /
C
C  HP 2100, four word double precision option with FTN4.
C
C      DATA SMALL(1), SMALL(2) /  40000B,       0 /
C      DATA SMALL(3), SMALL(4) /       0,       1 /
C      DATA LARGE(1), LARGE(2) /  77777B, 177777B /
C      DATA LARGE(3), LARGE(4) / 177777B, 177776B /
C      DATA RIGHT(1), RIGHT(2) /  40000B,       0 /
C      DATA RIGHT(3), RIGHT(4) /       0,    225B /
C      DATA DIVER(1), DIVER(2) /  40000B,       0 /
C      DATA DIVER(3), DIVER(4) /       0,    227B /
C      DATA LOG10(1), LOG10(2) /  46420B,  46502B /
C      DATA LOG10(3), LOG10(4) /  76747B, 176377B /
C
C  HP 9000
C
C      D1MACH(1) = 2.8480954D-306
C      D1MACH(2) = 1.40444776D+306
C      D1MACH(3) = 2.22044605D-16
C      D1MACH(4) = 4.44089210D-16
C      D1MACH(5) = 3.01029996D-1
C
C      DATA SMALL(1), SMALL(2) / 00040000000B, 00000000000B /
C      DATA LARGE(1), LARGE(2) / 17737777777B, 37777777777B /
C      DATA RIGHT(1), RIGHT(2) / 07454000000B, 00000000000B /
C      DATA DIVER(1), DIVER(2) / 07460000000B, 00000000000B /
C      DATA LOG10(1), LOG10(2) / 07764642023B, 12047674777B /
C
C  IBM 360/370 series, XEROX SIGMA 5/7/9, SEL SYSTEMS 85/86, PERKIN ELMER 3230, 
C  and PERKIN ELMER (INTERDATA) 3230.
C
C      DATA SMALL(1),SMALL(2) / Z00100000, Z00000000 /
C      DATA LARGE(1),LARGE(2) / Z7FFFFFFF, ZFFFFFFFF /
C      DATA RIGHT(1),RIGHT(2) / Z33100000, Z00000000 /
C      DATA DIVER(1),DIVER(2) / Z34100000, Z00000000 /
C      DATA LOG10(1),LOG10(2) / Z41134413, Z509F79FF /
C
C  IBM PC - Microsoft FORTRAN
C
C      DATA SMALL(1), SMALL(2) / #00000000, #00100000 /
C      DATA LARGE(1), LARGE(2) / #FFFFFFFF, #7FEFFFFF /
C      DATA RIGHT(1), RIGHT(2) / #00000000, #3CA00000 /
C      DATA DIVER(1), DIVER(2) / #00000000, #3CB00000 /
C      DATA LOG10(1), LOG10(2) / #509F79FF, #3FD34413 /
C
C  IBM PC - Professional FORTRAN and Lahey FORTRAN
C
C      DATA SMALL(1), SMALL(2) / Z'00000000', Z'00100000' /
C      DATA LARGE(1), LARGE(2) / Z'FFFFFFFF', Z'7FEFFFFF' /
C      DATA RIGHT(1), RIGHT(2) / Z'00000000', Z'3CA00000' /
C      DATA DIVER(1), DIVER(2) / Z'00000000', Z'3CB00000' /
C      DATA LOG10(1), LOG10(2) / Z'509F79FF', Z'3FD34413' /
C
C  INTERDATA 8/32 with the UNIX system FORTRAN 77 compiler.
C  For the INTERDATA FORTRAN VII compiler, replace the Z's specifying hex 
C  constants with Y's.
C
C      DATA SMALL(1),SMALL(2) / Z'00100000', Z'00000000' /
C      DATA LARGE(1),LARGE(2) / Z'7EFFFFFF', Z'FFFFFFFF' /
C      DATA RIGHT(1),RIGHT(2) / Z'33100000', Z'00000000' /
C      DATA DIVER(1),DIVER(2) / Z'34100000', Z'00000000' /
C      DATA LOG10(1),LOG10(2) / Z'41134413', Z'509F79FF' /
C
C  PDP-10 (KA processor).
C
C      DATA SMALL(1),SMALL(2) / "033400000000, "000000000000 /
C      DATA LARGE(1),LARGE(2) / "377777777777, "344777777777 /
C      DATA RIGHT(1),RIGHT(2) / "113400000000, "000000000000 /
C      DATA DIVER(1),DIVER(2) / "114400000000, "000000000000 /
C      DATA LOG10(1),LOG10(2) / "177464202324, "144117571776 /
C
C  PDP-10 (KI processor).
C
C      DATA SMALL(1),SMALL(2) / "000400000000, "000000000000 /
C      DATA LARGE(1),LARGE(2) / "377777777777, "377777777777 /
C      DATA RIGHT(1),RIGHT(2) / "103400000000, "000000000000 /
C      DATA DIVER(1),DIVER(2) / "104400000000, "000000000000 /
C      DATA LOG10(1),LOG10(2) / "177464202324, "047674776746 /
C
C  PDP-11 FORTRANS supporting 32-bit integers (integer version).
C
C      DATA SMALL(1),SMALL(2) /    8388608,           0 /
C      DATA LARGE(1),LARGE(2) / 2147483647,          -1 /
C      DATA RIGHT(1),RIGHT(2) /  612368384,           0 /
C      DATA DIVER(1),DIVER(2) /  620756992,           0 /
C      DATA LOG10(1),LOG10(2) / 1067065498, -2063872008 /
C
C  PDP-11 FORTRANS supporting 32-bit integers (octal version)
C
C      DATA SMALL(1),SMALL(2) / O00040000000, O00000000000 /
C      DATA LARGE(1),LARGE(2) / O17777777777, O37777777777 /
C      DATA RIGHT(1),RIGHT(2) / O04440000000, O00000000000 /
C      DATA DIVER(1),DIVER(2) / O04500000000, O00000000000 /
C      DATA LOG10(1),LOG10(2) / O07746420232, O20476747770 /
C
C  PDP-11 FORTRANS supporting 16-bit integers (integer version).
C
C      DATA SMALL(1),SMALL(2) /    128,      0 /
C      DATA SMALL(3),SMALL(4) /      0,      0 /
C
C      DATA LARGE(1),LARGE(2) /  32767,     -1 /
C      DATA LARGE(3),LARGE(4) /     -1,     -1 /
C
C      DATA RIGHT(1),RIGHT(2) /   9344,      0 /
C      DATA RIGHT(3),RIGHT(4) /      0,      0 /
C
C      DATA DIVER(1),DIVER(2) /   9472,      0 /
C      DATA DIVER(3),DIVER(4) /      0,      0 /
C
C      DATA LOG10(1),LOG10(2) /  16282,   8346 /
C      DATA LOG10(3),LOG10(4) / -31493, -12296 /
C
C  PDP-11 FORTRANS supporting 16-bit integers (octal version).
C
C      DATA SMALL(1),SMALL(2) / O000200, O000000 /
C      DATA SMALL(3),SMALL(4) / O000000, O000000 /
C
C      DATA LARGE(1),LARGE(2) / O077777, O177777 /
C      DATA LARGE(3),LARGE(4) / O177777, O177777 /
C
C      DATA RIGHT(1),RIGHT(2) / O022200, O000000 /
C      DATA RIGHT(3),RIGHT(4) / O000000, O000000 /
C
C      DATA DIVER(1),DIVER(2) / O022400, O000000 /
C      DATA DIVER(3),DIVER(4) / O000000, O000000 /
C
C      DATA LOG10(1),LOG10(2) / O037632, O020232 /
C      DATA LOG10(3),LOG10(4) / O102373, O147770 /
C
C  PRIME 50 series systems with 32-bit integers and 64V MODE instructions,
C  supplied by Igor Bray.
C
C      DATA SMALL(1),SMALL(2) / :10000000000, :00000100001 /
C      DATA LARGE(1),LARGE(2) / :17777777777, :37777677775 /
C      DATA RIGHT(1),RIGHT(2) / :10000000000, :00000000122 /
C      DATA DIVER(1),DIVER(2) / :10000000000, :00000000123 /
C      DATA LOG10(1),LOG10(2) / :11504046501, :07674600177 /
C
C  SEQUENT BALANCE 8000
C
C      DATA SMALL(1),SMALL(2) / $00000000,  $00100000 /
C      DATA LARGE(1),LARGE(2) / $FFFFFFFF,  $7FEFFFFF /
C      DATA RIGHT(1),RIGHT(2) / $00000000,  $3CA00000 /
C      DATA DIVER(1),DIVER(2) / $00000000,  $3CB00000 /
C      DATA LOG10(1),LOG10(2) / $509F79FF,  $3FD34413 /
C
C  SUN Microsystems UNIX F77 compiler.
C
C      DATA DMACH(1) / 2.22507385850720D-308 /
C      DATA DMACH(2) / 1.79769313486231D+308 /
C      DATA DMACH(3) / 1.1101827117665D-16 /
C      DATA DMACH(4) / 2.2203654423533D-16 /
C      DATA DMACH(5) / 3.01029995663981D-1 /
C
C  SUN 3 (68881 or FPA)
C
C      DATA SMALL(1),SMALL(2) / X'00100000', X'00000000' /
C      DATA LARGE(1),LARGE(2) / X'7FEFFFFF', X'FFFFFFFF' /
C      DATA RIGHT(1),RIGHT(2) / X'3CA00000', X'00000000' /
C      DATA DIVER(1),DIVER(2) / X'3CB00000', X'00000000' /
C      DATA LOG10(1),LOG10(2) / X'3FD34413', X'509F79FF' /
C
C  UNIVAC 1100 series.
C
C      DATA SMALL(1),SMALL(2) / O000040000000, O000000000000 /
C      DATA LARGE(1),LARGE(2) / O377777777777, O777777777777 /
C      DATA RIGHT(1),RIGHT(2) / O170540000000, O000000000000 /
C      DATA DIVER(1),DIVER(2) / O170640000000, O000000000000 /
C      DATA LOG10(1),LOG10(2) / O177746420232, O411757177572 /
C
C  VAX/ULTRIX F77 compiler  
C
C      DATA SMALL(1),SMALL(2) /        128,           0 /
C      DATA LARGE(1),LARGE(2) /     -32769,          -1 /
C      DATA RIGHT(1),RIGHT(2) /       9344,           0 /
C      DATA DIVER(1),DIVER(2) /       9472,           0 /
C      DATA LOG10(1),LOG10(2) /  546979738,  -805796613 /
C
C  VAX/ULTRIX F77 compiler, G floating
C
C      DATA SMALL(1), SMALL(2) /         16,           0 /
C      DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C      DATA RIGHT(1), RIGHT(2) /      15552,           0 /
C      DATA DIVER(1), DIVER(2) /      15568,           0 /
C      DATA LOG10(1), LOG10(2) /  1142112243, 2046775455 /
C
C  VAX-11 with FORTRAN IV-PLUS compiler
C
C      DATA SMALL(1),SMALL(2) / Z00000080, Z00000000 /
C      DATA LARGE(1),LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C      DATA RIGHT(1),RIGHT(2) / Z00002480, Z00000000 /
C      DATA DIVER(1),DIVER(2) / Z00002500, Z00000000 /
C      DATA LOG10(1),LOG10(2) / Z209A3F9A, ZCFF884FB /
C
C  VAX/VMS version 2.2
C
C      DATA SMALL(1),SMALL(2) /       '80'X,        '0'X /
C      DATA LARGE(1),LARGE(2) / 'FFFF7FFF'X, 'FFFFFFFF'X /
C      DATA RIGHT(1),RIGHT(2) /     '2480'X,        '0'X /
C      DATA DIVER(1),DIVER(2) /     '2500'X,        '0'X /
C      DATA LOG10(1),LOG10(2) / '209A3F9A'X, 'CFF884FB'X /
C
C  VAX/VMS 11/780  
C
C      DATA SMALL(1), SMALL(2) / Z00000080, Z00000000 /
C      DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C      DATA RIGHT(1), RIGHT(2) / Z00002480, Z00000000 /
C      DATA DIVER(1), DIVER(2) / Z00002500, Z00000000 /
C      DATA LOG10(1), LOG10(2) / Z209A3F9A, ZCFF884FB /
C
C  VAX/VMS 11/780 (G-FLOATING)
C
C      DATA SMALL(1), SMALL(2) / Z00000010, Z00000000 /
C      DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C      DATA RIGHT(1), RIGHT(2) / Z00003CC0, Z00000000 /
C      DATA DIVER(1), DIVER(2) / Z00003CD0, Z00000000 /
C      DATA LOG10(1), LOG10(2) / Z44133FF3, Z79FF509F /
C
      IF(I.LT.1.OR.I.GT.5)THEN
        WRITE(*,*)'D1MACH - ERROR, I out of bounds:',I
        D1MACH=0.0D0
      ELSE
        D1MACH = DMACH(I)
        ENDIF
      RETURN
      END
      INTEGER FUNCTION I1MACH(I)
c
c***********************************************************************
c
C  I1MACH returns integer machine constants.
C
C  I/O unit numbers.
C
C    I1MACH(1) = the standard input unit.
C    I1MACH(2) = the standard output unit.
C    I1MACH(3) = the standard punch unit.
C    I1MACH(4) = the standard error message unit.
C
C  Words.
C
C    I1MACH(5) = the number of bits per integer storage unit.
C    I1MACH(6) = the number of characters per integer storage unit.
C
C  Integers.
C
C  Assume integers are represented in the S digit base A form:
C
C  Sign * (X(S-1)*A**(S-1) + ... + X(1)*A + X(0))
C  where 0<=X(I)<A for I=0 to S-1.
C
C    I1MACH(7) = A, the base.
C    I1MACH(8) = S, the number of base A digits.
C    I1MACH(9) = A**S-1, the largest integer.
C
C  Floating point numbers
C
C  Assume floating point numbers are represented in the T digit base B form:
C
C    Sign * (B**E) * ((X(1)/B) + ... + (X(T)/B**T) )
C
C  where 0<=X(I)<B for I=1 to T, 0<X(1) and EMIN<=E<=EMAX
C
C    I1MACH(10) = B, the base.
C
C  Single precision
C
C    I1MACH(11) = T, the number of base B digits.
C    I1MACH(12) = EMIN, the smallest exponent E.
C    I1MACH(13) = EMAX, the largest exponent E.
C
C  Double precision
C
C    I1MACH(14) = T, the number of base B digits.
C    I1MACH(15) = EMIN, the smallest exponent E.
C    I1MACH(16) = EMAX, the largest exponent E.
C
C  To alter this function for a particular environment, the desired set of DATA 
C  statements should be activated by removing the C from column 1.  On rare 
C  machines, a STATIC statement may need to be added, but probably more systems 
C  prohibit than require it.
C
C  Also, the values of I1MACH(1) through I1MACH(4) should be checked for 
C  consistency with the local operating system.  For FORTRAN 77, you may wish 
C  to adjust the data statement so IMACH(6) is set to 1, and then to comment 
C  out the executable test on I.EQ.6 below.
C
C  For IEEE-arithmetic machines (binary standard), the first set of constants 
C  below should be appropriate, except perhaps for IMACH(1) - IMACH(4).
C
      INTEGER IMACH(16),OUTPUT
C
      EQUIVALENCE (IMACH(4),OUTPUT)
C
C  IEEE arithmetic machines, such as the AT&T 3B series, Motorola 68000 based 
C  machines such as the SUN 3 and AT&T PC 7300, and 8087 based micros such as 
C  the IBM PC and AT&T 6300.
C
       DATA IMACH( 1) /    5 /
       DATA IMACH( 2) /    6 /
       DATA IMACH( 3) /    7 /
       DATA IMACH( 4) /    6 /
       DATA IMACH( 5) /   32 /
       DATA IMACH( 6) /    4 /
       DATA IMACH( 7) /    2 / 
       DATA IMACH( 8) /   31 /
       DATA IMACH( 9) / 2147483647 /
       DATA IMACH(10) /    2 /
       DATA IMACH(11) /   24 /
       DATA IMACH(12) / -125 /
       DATA IMACH(13) /  128 /
       DATA IMACH(14) /   53 /
       DATA IMACH(15) / -1021 /
       DATA IMACH(16) /  1024 /
C
C  ALLIANT FX/8 UNIX FORTRAN compiler.
C
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     6 /
C      DATA IMACH( 4) /     0 /
C      DATA IMACH( 5) /    32 /
c      DATA IMACH( 6) /     4 /
c      DATA IMACH( 7) /     2 /
c      DATA IMACH( 8) /    32 /
c      DATA IMACH( 9) /2147483647/
c      DATA IMACH(10) /     2 /
c      DATA IMACH(11) /    24 /
c      DATA IMACH(12) /  -126 /
c      DATA IMACH(13) /   128 /
c      DATA IMACH(14) /    53 /
c      DATA IMACH(15) / -1022 /
c      DATA IMACH(16) /  1024 /
C
C  AMDAHL machines.
C
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   7 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  32 /
C      DATA IMACH( 6) /   4 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /  16 /
C      DATA IMACH(11) /   6 /
C      DATA IMACH(12) / -64 /
C      DATA IMACH(13) /  63 /
C      DATA IMACH(14) /  14 /
C      DATA IMACH(15) / -64 /
C      DATA IMACH(16) /  63 /
C
C  BURROUGHS 1700 system.
C
C      DATA IMACH( 1) /    7 /
C      DATA IMACH( 2) /    2 /
C      DATA IMACH( 3) /    2 /
C      DATA IMACH( 4) /    2 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   33 /
C      DATA IMACH( 9) / Z1FFFFFFFF /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -256 /
C      DATA IMACH(13) /  255 /
C      DATA IMACH(14) /   60 /
C      DATA IMACH(15) / -256 /
C      DATA IMACH(16) /  255 /
C
C  BURROUGHS 5700 system.
C
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   7 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  48 /
C      DATA IMACH( 6) /   6 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  39 /
C      DATA IMACH( 9) / O0007777777777777 /
C      DATA IMACH(10) /   8 /
C      DATA IMACH(11) /  13 /
C      DATA IMACH(12) / -50 /
C      DATA IMACH(13) /  76 /
C      DATA IMACH(14) /  26 /
C      DATA IMACH(15) / -50 /
C      DATA IMACH(16) /  76 /
C
C  BURROUGHS 6700/7700 systems.
C
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   7 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  48 /
C      DATA IMACH( 6) /   6 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  39 /
C      DATA IMACH( 9) / O0007777777777777 /
C      DATA IMACH(10) /   8 /
C      DATA IMACH(11) /  13 /
C      DATA IMACH(12) / -50 /
C      DATA IMACH(13) /  76 /
C      DATA IMACH(14) /  26 /
C      DATA IMACH(15) / -32754 /
C      DATA IMACH(16) /  32780 /
C
C  CDC CYBER 170/180 series using NOS
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   60 /
C      DATA IMACH( 6) /   10 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   48 /
C      DATA IMACH( 9) / O"00007777777777777777" /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   48 /
C      DATA IMACH(12) / -974 /
C      DATA IMACH(13) / 1070 /
C      DATA IMACH(14) /   96 /
C      DATA IMACH(15) / -927 /
C      DATA IMACH(16) / 1070 /
C
C  CDC CYBER 170/180 series using NOS/VE
C
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     7 /
C      DATA IMACH( 4) /     6 /
C      DATA IMACH( 5) /    64 /
C      DATA IMACH( 6) /     8 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    63 /
C      DATA IMACH( 9) / 9223372036854775807 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    47 /
C      DATA IMACH(12) / -4095 /
C      DATA IMACH(13) /  4094 /
C      DATA IMACH(14) /    94 /
C      DATA IMACH(15) / -4095 /
C      DATA IMACH(16) /  4094 /
C
C  CDC CYBER 200 series
C
C      DATA IMACH( 1) /      5 /
C      DATA IMACH( 2) /      6 /
C      DATA IMACH( 3) /      7 /
C      DATA IMACH( 4) /      6 /
C      DATA IMACH( 5) /     64 /
C      DATA IMACH( 6) /      8 /
C      DATA IMACH( 7) /      2 /
C      DATA IMACH( 8) /     47 /
C      DATA IMACH( 9) / X'00007FFFFFFFFFFF' /
C      DATA IMACH(10) /      2 /
C      DATA IMACH(11) /     47 /
C      DATA IMACH(12) / -28625 /
C      DATA IMACH(13) /  28718 /
C      DATA IMACH(14) /     94 /
C      DATA IMACH(15) / -28625 /
C      DATA IMACH(16) /  28718 /
C
C  CDC 6000/7000 series using FTN4.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   60 /
C      DATA IMACH( 6) /   10 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   48 /
C      DATA IMACH( 9) / 00007777777777777777B /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   47 /
C      DATA IMACH(12) / -929 /
C      DATA IMACH(13) / 1070 /
C      DATA IMACH(14) /   94 /
C      DATA IMACH(15) / -929 /
C      DATA IMACH(16) / 1069 /
C
C  CDC 6000/7000 series using FTN5.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   60 /
C      DATA IMACH( 6) /   10 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   48 /
C      DATA IMACH( 9) / O"00007777777777777777" /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   47 /
C      DATA IMACH(12) / -929 /
C      DATA IMACH(13) / 1070 /
C      DATA IMACH(14) /   94 /
C      DATA IMACH(15) / -929 /
C      DATA IMACH(16) / 1069 /
C
C  CONVEX C-1.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   53 /
C      DATA IMACH(15) /-1024 /
C      DATA IMACH(16) / 1023 /
C
C  CONVEX C-120 (native mode) without -R8 option
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    0 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   53 /
C      DATA IMACH(15) / -1023 /
C      DATA IMACH(16) /  1023 /
C
C  CONVEX C-120 (native mode) with -R8 option
C
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     0 /
C      DATA IMACH( 4) /     6 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     4 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    53 /
C      DATA IMACH(12) / -1023 /
C      DATA IMACH(13) /  1023 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1023 /
C      DATA IMACH(16) /  1023 /
C
C  CONVEX C-120 (IEEE mode) without -R8 option
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    0 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -125 /
C      DATA IMACH(13) /  128 /
C      DATA IMACH(14) /   53 /
C      DATA IMACH(15) / -1021 /
C      DATA IMACH(16) /  1024 /
C
C  CONVEX C-120 (IEEE mode) with -R8 option
C
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     0 /
C      DATA IMACH( 4) /     6 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     4 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    53 /
C      DATA IMACH(12) / -1021 /
C      DATA IMACH(13) /  1024 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1021 /
C      DATA IMACH(16) /  1024 /
C
C  CRAY 1, 2, XMP and YMP.
C
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /   102 /
C      DATA IMACH( 4) /     6 /
C      DATA IMACH( 5) /    64 /
C      DATA IMACH( 6) /     8 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    63 /
C      DATA IMACH( 9) /  777777777777777777777B /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    47 /
C      DATA IMACH(12) / -8189 /
C      DATA IMACH(13) /  8190 /
C      DATA IMACH(14) /    94 /
C      DATA IMACH(15) / -8099 /
C      DATA IMACH(16) /  8190 /
C
C  DATA GENERAL ECLIPSE S/200.
C
C      DATA IMACH( 1) /   11 /
C      DATA IMACH( 2) /   12 /
C      DATA IMACH( 3) /    8 /
C      DATA IMACH( 4) /   10 /
C      DATA IMACH( 5) /   16 /
C      DATA IMACH( 6) /    2 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   15 /
C      DATA IMACH( 9) /32767 /
C      DATA IMACH(10) /   16 /
C      DATA IMACH(11) /    6 /
C      DATA IMACH(12) /  -64 /
C      DATA IMACH(13) /   63 /
C      DATA IMACH(14) /   14 /
C      DATA IMACH(15) /  -64 /
C      DATA IMACH(16) /   63 /
C
C  ELXSI  6400
C
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     6 /
C      DATA IMACH( 4) /     6 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     4 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    32 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    24 /
C      DATA IMACH(12) /  -126 /
C      DATA IMACH(13) /   127 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1022 /
C      DATA IMACH(16) /  1023 /
C
C  HARRIS 220
C
C      DATA IMACH( 1) /       5 /
C      DATA IMACH( 2) /       6 /
C      DATA IMACH( 3) /       0 /
C      DATA IMACH( 4) /       6 /
C      DATA IMACH( 5) /      24 /
C      DATA IMACH( 6) /       3 /
C      DATA IMACH( 7) /       2 /
C      DATA IMACH( 8) /      23 /
C      DATA IMACH( 9) / 8388607 /
C      DATA IMACH(10) /       2 /
C      DATA IMACH(11) /      23 /
C      DATA IMACH(12) /    -127 /
C      DATA IMACH(13) /     127 /
C      DATA IMACH(14) /      38 /
C      DATA IMACH(15) /    -127 /
C      DATA IMACH(16) /     127 /
C
C  HARRIS SLASH 6 and SLASH 7.
C
C      DATA IMACH( 1) /       5 /
C      DATA IMACH( 2) /       6 /
C      DATA IMACH( 3) /       0 /
C      DATA IMACH( 4) /       6 /
C      DATA IMACH( 5) /      24 /
C      DATA IMACH( 6) /       3 /
C      DATA IMACH( 7) /       2 /
C      DATA IMACH( 8) /      23 /
C      DATA IMACH( 9) / 8388607 /
C      DATA IMACH(10) /       2 /
C      DATA IMACH(11) /      23 /
C      DATA IMACH(12) /    -127 /
C      DATA IMACH(13) /     127 /
C      DATA IMACH(14) /      38 /
C      DATA IMACH(15) /    -127 /
C      DATA IMACH(16) /     127 /
C
C  HONEYWELL DPS 8/70 and 600/6000 series.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /   43 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / O377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   63 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /
C
C  HP 2100, 3 word double precision option with FTN4
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    4 /
C      DATA IMACH( 4) /    1 /
C      DATA IMACH( 5) /   16 /
C      DATA IMACH( 6) /    2 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   15 /
C      DATA IMACH( 9) / 32767 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   23 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   39 /
C      DATA IMACH(15) / -128 /
C      DATA IMACH(16) /  127 /
C
C  HP 2100, 4 word double precision option with FTN4
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    4 /
C      DATA IMACH( 4) /    1 /
C      DATA IMACH( 5) /   16 /
C      DATA IMACH( 6) /    2 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   15 /
C      DATA IMACH( 9) / 32767 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   23 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   55 /
C      DATA IMACH(15) / -128 /
C      DATA IMACH(16) /  127 /
C
C  HP 9000
C
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     6 /
C      DATA IMACH( 4) /     7 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     4 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    32 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    24 /
C      DATA IMACH(12) /  -126 /
C      DATA IMACH(13) /   127 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1015 /
C      DATA IMACH(16) /  1017 /
C
C  IBM 360/370 series, XEROX SIGMA 5/7/9, SEL systems 85/86, PERKIN ELMER 3230, 
C  and PERKIN ELMER (INTERDATA) 3230.
C
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   7 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  32 /
C      DATA IMACH( 6) /   4 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  31 /
C      DATA IMACH( 9) / Z7FFFFFFF /
C      DATA IMACH(10) /  16 /
C      DATA IMACH(11) /   6 /
C      DATA IMACH(12) / -64 /
C      DATA IMACH(13) /  63 /
C      DATA IMACH(14) /  14 /
C      DATA IMACH(15) / -64 /
C      DATA IMACH(16) /  63 /
C
C  IBM PC - Microsoft FORTRAN
C
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     6 /
C      DATA IMACH( 4) /     0 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     4 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    24 /
C      DATA IMACH(12) /  -126 /
C      DATA IMACH(13) /   127 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1022 /
C      DATA IMACH(16) /  1023 /
C
C  IBM PC - Professional FORTRAN and Lahey FORTRAN
C
C      DATA IMACH( 1) /     4 /
C      DATA IMACH( 2) /     7 /
C      DATA IMACH( 3) /     7 /
C      DATA IMACH( 4) /     0 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     4 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    24 /
C      DATA IMACH(12) /  -126 /
C      DATA IMACH(13) /   127 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1022 /
C      DATA IMACH(16) /  1023 /
C
C  INTERDATA 8/32 with the UNIX system FORTRAN 77 compiler.
C  For the INTERDATA FORTRAN VII compiler, replace the Z's specifying hex 
C  constants with Y's.
C
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   6 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  32 /
C      DATA IMACH( 6) /   4 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  31 /
C      DATA IMACH( 9) / Z'7FFFFFFF' /
C      DATA IMACH(10) /  16 /
C      DATA IMACH(11) /   6 /
C      DATA IMACH(12) / -64 /
C      DATA IMACH(13) /  62 /
C      DATA IMACH(14) /  14 /
C      DATA IMACH(15) / -64 /
C      DATA IMACH(16) /  62 /
C
C  PDP-10 (KA processor).
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    5 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / "377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   54 /
C      DATA IMACH(15) / -101 /
C      DATA IMACH(16) /  127 /
C
C  PDP-10 (KI processor).
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    5 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / "377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   62 /
C      DATA IMACH(15) / -128 /
C      DATA IMACH(16) /  127 /
C
C  PDP-11 FORTRANS supporting 32-bit integer arithmetic.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   56 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /
C
C  PDP-11 FORTRANS supporting 16-bit integer arithmetic.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   16 /
C      DATA IMACH( 6) /    2 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   15 /
C      DATA IMACH( 9) / 32767 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   56 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /
C
C  PRIME 50 series systems with 32-bit integers and 64V MODE instructions,
C  supplied by Igor Bray.
C
C      DATA IMACH( 1) /            1 /
C      DATA IMACH( 2) /            1 /
C      DATA IMACH( 3) /            2 /
C      DATA IMACH( 4) /            1 /
C      DATA IMACH( 5) /           32 /
C      DATA IMACH( 6) /            4 /
C      DATA IMACH( 7) /            2 /
C      DATA IMACH( 8) /           31 /
C      DATA IMACH( 9) / :17777777777 /
C      DATA IMACH(10) /            2 /
C      DATA IMACH(11) /           23 /
C      DATA IMACH(12) /         -127 /
C      DATA IMACH(13) /         +127 /
C      DATA IMACH(14) /           47 /
C      DATA IMACH(15) /       -32895 /
C      DATA IMACH(16) /       +32637 /
C
C  SEQUENT BALANCE 8000.
C
C      DATA IMACH( 1) /     0 /
C      DATA IMACH( 2) /     0 /
C      DATA IMACH( 3) /     7 /
C      DATA IMACH( 4) /     0 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     1 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    31 /
C      DATA IMACH( 9) /  2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    24 /
C      DATA IMACH(12) /  -125 /
C      DATA IMACH(13) /   128 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1021 /
C      DATA IMACH(16) /  1024 /
C
C  SUN Microsystems UNIX F77 compiler.
C
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     6 /
C      DATA IMACH( 4) /     0 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     4 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    32 /
C      DATA IMACH( 9) /2147483647/
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    24 /
C      DATA IMACH(12) /  -126 /
C      DATA IMACH(13) /   128 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1022 /
C      DATA IMACH(16) /  1024 /
C
C  SUN 3 (68881 or FPA)
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    6 /
C      DATA IMACH( 4) /    0 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -125 /
C      DATA IMACH(13) /  128 /
C      DATA IMACH(14) /   53 /
C      DATA IMACH(15) / -1021 /
C      DATA IMACH(16) /  1024 /
C
C  UNIVAC 1100 series.
C  Note that the punch unit, I1MACH(3), has been set to 7, which is appropriate 
C  for the UNIVAC-FOR system.  If you have the UNIVAC-FTN system, set it to 1 
C  instead.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    6 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / O377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   60 /
C      DATA IMACH(15) /-1024 /
C      DATA IMACH(16) / 1023 /
C
C  VAX.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   56 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /
C
C  Z80 microprocessor.
C
C      DATA IMACH( 1) /    1 /
C      DATA IMACH( 2) /    1 /
C      DATA IMACH( 3) /    0 /
C      DATA IMACH( 4) /    1 /
C      DATA IMACH( 5) /   16 /
C      DATA IMACH( 6) /    2 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   15 /
C      DATA IMACH( 9) / 32767 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   56 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /
C
      IF(I.LT.1.OR.I.GT.16)THEN
        WRITE(*,*)'I1MACH - ERROR, I out of bounds:',I
        I1MACH=0
      ELSE
        I1MACH=IMACH(I)
        ENDIF
      RETURN
      END
      REAL FUNCTION R1MACH(I)
c
c***********************************************************************
c
C  R1MACH returns single precision machine constants.
C
C  Assume that single precision numbers are stored with a mantissa of T digits 
C  in base B, with an exponent whose value must lie between EMIN and EMAX.  Then
C  for values of I between 1 and 5, R1MACH will return the following values:
C
C    R1MACH(1) = B**(EMIN-1), the smallest positive magnitude.
C    R1MACH(2) = B**EMAX*(1-B**(-T)), the largest magnitude.
C    R1MACH(3) = B**(-T), the smallest relative spacing.
C    R1MACH(4) = B**(1-T), the largest relative spacing.
C    R1MACH(5) = LOG10(B)
C
C  To alter this function for a particular environment, the desired set of data 
C  statements should be activated by removing the C from column 1.
C
C  On rare machines a STATIC statement may need to be added.  But probably more
C  systems prohibit it that require it.
C
C  For IEEE-arithmetic machines (binary standard), the first set of constants 
C  below should be appropriate.
C
C  Where possible, octal or hexadecimal constants have been used to specify the
C  constants exactly which has in some cases required the use of EQUIVALENCED 
C  integer arrays.  If your compiler uses half-word integers by default 
C  (sometimes called INTEGER*2), you may need to change INTEGER to INTEGER*4 or
C  otherwise instruct your compiler to use full-word integers in the next 5 
C  declarations.
C
      INTEGER SMALL(2)
      INTEGER LARGE(2)
      INTEGER RIGHT(2)
      INTEGER DIVER(2)
      INTEGER LOG10(2)
C
      REAL RMACH(5)
C
      EQUIVALENCE (RMACH(1),SMALL(1))
      EQUIVALENCE (RMACH(2),LARGE(1))
      EQUIVALENCE (RMACH(3),RIGHT(1))
      EQUIVALENCE (RMACH(4),DIVER(1))
      EQUIVALENCE (RMACH(5),LOG10(1))
C
C  IEEE arithmetic machines, such as the AT&T 3B series, Motorola 68000 based 
C  machines such as the SUN 3 and AT&T PC 7300, and 8087 based micros such as 
C  the IBM PC and AT&T 6300.
C
       DATA SMALL(1) /     8388608 /
       DATA LARGE(1) /  2139095039 /
       DATA RIGHT(1) /   864026624 /
       DATA DIVER(1) /   872415232 /
       DATA LOG10(1) /  1050288283 /
C
C  ALLiant FX/8 UNIX Fortran compiler with the -r8 command line option.  This 
C  option causes all variables declared with 'REAL' to be of type 'REAL*8' or
C  DOUBLE PRECISION.  This option does not override the 'REAL*4' declarations.  
C  These R1MACH numbers below and the coresponding I1MACH are simply the DOUBLE
C  PRECISION or 'REAL*8' numbers.  If you use the -r8 your whole code (and the 
C  user libraries you link with, the system libraries are taken care of 
C  automagicly) must be compiled with this option.
C
c      DATA RMACH(1) / 2.22507385850721D-308 /
c      DATA RMACH(2) / 1.79769313486231D+308 /
c      DATA RMACH(3) / 1.1101827117665D-16 /
c      DATA RMACH(4) / 2.2203654423533D-16 /
c      DATA RMACH(5) / 3.01029995663981E-1 /
C
C  AMDAHL machines.
C
C      DATA SMALL(1) /    1048576 /
C      DATA LARGE(1) / 2147483647 /
C      DATA RIGHT(1) /  990904320 /
C      DATA DIVER(1) / 1007681536 /
C      DATA LOG10(1) / 1091781651 /
C
C  BURROUGHS 1700 system.
C
C      DATA RMACH(1) / Z400800000 /
C      DATA RMACH(2) / Z5FFFFFFFF /
C      DATA RMACH(3) / Z4E9800000 /
C      DATA RMACH(4) / Z4EA800000 /
C      DATA RMACH(5) / Z500E730E8 /
C
C  BURROUGHS 5700/6700/7700 systems.
C
C      DATA RMACH(1) / O1771000000000000 /
C      DATA RMACH(2) / O0777777777777777 /
C      DATA RMACH(3) / O1311000000000000 /
C      DATA RMACH(4) / O1301000000000000 /
C      DATA RMACH(5) / O1157163034761675 /
C
C  CDC CYBER 170/180 series using NOS
C
C      DATA RMACH(1) / O"00014000000000000000" /
C      DATA RMACH(2) / O"37767777777777777777" /
C      DATA RMACH(3) / O"16404000000000000000" /
C      DATA RMACH(4) / O"16414000000000000000" /
C      DATA RMACH(5) / O"17164642023241175720" /
C
C  CDC CYBER 170/180 series using NOS/VE
C
C      DATA RMACH(1) / Z"3001800000000000" /
C      DATA RMACH(2) / Z"4FFEFFFFFFFFFFFE" /
C      DATA RMACH(3) / Z"3FD2800000000000" /
C      DATA RMACH(4) / Z"3FD3800000000000" /
C      DATA RMACH(5) / Z"3FFF9A209A84FBCF" /
C
C  CDC CYBER 200 series
C
C      DATA RMACH(1) / X'9000400000000000' /
C      DATA RMACH(2) / X'6FFF7FFFFFFFFFFF' /
C      DATA RMACH(3) / X'FFA3400000000000' /
C      DATA RMACH(4) / X'FFA4400000000000' /
C      DATA RMACH(5) / X'FFD04D104D427DE8' /
C
C  CDC 6000/7000 series using FTN4.
C
C      DATA RMACH(1) / 00564000000000000000B /
C      DATA RMACH(2) / 37767777777777777776B /
C      DATA RMACH(3) / 16414000000000000000B /
C      DATA RMACH(4) / 16424000000000000000B /
C      DATA RMACH(5) / 17164642023241175720B /
C
C  CDC 6000/7000 series using FTN5.
C
C      DATA RMACH(1) / O"00564000000000000000" /
C      DATA RMACH(2) / O"37767777777777777776" /
C      DATA RMACH(3) / O"16414000000000000000" /
C      DATA RMACH(4) / O"16424000000000000000" /
C      DATA RMACH(5) / O"17164642023241175720" /
C
C  CONVEX C-1.
C
C      DATA RMACH(1) / '00800000'X /
C      DATA RMACH(2) / '7FFFFFFF'X /
C      DATA RMACH(3) / '34800000'X /
C      DATA RMACH(4) / '35000000'X /
C      DATA RMACH(5) / '3F9A209B'X /
C
C  CONVEX C-120 (native mode) without -R8 option
C
C      DATA RMACH(1) / 2.9387360E-39 /
C      DATA RMACH(2) / 1.7014117E+38 /
C      DATA RMACH(3) / 5.9604645E-08 /
C      DATA RMACH(4) / 1.1920929E-07 /
C      DATA RMACH(5) / 3.0102999E-01 /
C
C  CONVEX C-120 (native mode) with -R8 option
C
C      DATA RMACH(1) / 5.562684646268007D-309 /
C      DATA RMACH(2) / 8.988465674311577D+307 /
C      DATA RMACH(3) / 1.110223024625157D-016 /
C      DATA RMACH(4) / 2.220446049250313D-016 /
C      DATA RMACH(5) / 3.010299956639812D-001 /
C
C  CONVEX C-120 (IEEE mode) without -R8 option
C
C      DATA RMACH(1) / 1.1754945E-38 /
C      DATA RMACH(2) / 3.4028234E+38 /
C      DATA RMACH(3) / 5.9604645E-08 /
C      DATA RMACH(4) / 1.1920929E-07 /
C      DATA RMACH(5) / 3.0102999E-01 /
C
C  CONVEX C-120 (IEEE mode) with -R8 option
C
C      DATA RMACH(1) / 2.225073858507202D-308 /
C      DATA RMACH(2) / 1.797693134862315D+308 /
C      DATA RMACH(3) / 1.110223024625157D-016 /
C      DATA RMACH(4) / 2.220446049250313D-016 /
C      DATA RMACH(5) / 3.010299956639812D-001 /
C
C  CRAY 1, 2, XMP and YMP.
C
C      DATA RMACH(1) / 200034000000000000000B /
C      DATA RMACH(2) / 577767777777777777776B /
C      DATA RMACH(3) / 377224000000000000000B /
C      DATA RMACH(4) / 377234000000000000000B /
C      DATA RMACH(5) / 377774642023241175720B /
C
C  Data General ECLIPSE S/200.
C  Note - It may be appropriate to include the line: STATIC RMACH(5)
C
C      DATA SMALL /20K,0/
C      DATA LARGE /77777K,177777K/
C      DATA RIGHT /35420K,0/
C      DATA DIVER /36020K,0/
C      DATA LOG10 /40423K,42023K/
C
C  ELXSI 6400, assuming REAL*4 is the default real type.
C
C      DATA SMALL(1) / '00800000'X /
C      DATA LARGE(1) / '7F7FFFFF'X /
C      DATA RIGHT(1) / '33800000'X /
C      DATA DIVER(1) / '34000000'X /
C      DATA LOG10(1) / '3E9A209B'X /
C
C  HARRIS 220
C
C      DATA SMALL(1),SMALL(2) / '20000000, '00000201 /
C      DATA LARGE(1),LARGE(2) / '37777777, '00000177 /
C      DATA RIGHT(1),RIGHT(2) / '20000000, '00000352 /
C      DATA DIVER(1),DIVER(2) / '20000000, '00000353 /
C      DATA LOG10(1),LOG10(2) / '23210115, '00000377 /
C
C  HARRIS SLASH 6 and SLASH 7.
C
C      DATA SMALL(1),SMALL(2) / '20000000, '00000201 /
C      DATA LARGE(1),LARGE(2) / '37777777, '00000177 /
C      DATA RIGHT(1),RIGHT(2) / '20000000, '00000352 /
C      DATA DIVER(1),DIVER(2) / '20000000, '00000353 /
C      DATA LOG10(1),LOG10(2) / '23210115, '00000377 /
C
C  HONEYWELL DPS 8/70 and 600/6000 series.
C
C      DATA RMACH(1) / O402400000000 /
C      DATA RMACH(2) / O376777777777 /
C      DATA RMACH(3) / O714400000000 /
C      DATA RMACH(4) / O716400000000 /
C      DATA RMACH(5) / O776464202324 /
C
C  HP 2100, 3 word double precision with FTN4
C
C      DATA SMALL(1), SMALL(2) / 40000B,       1 /
C      DATA LARGE(1), LARGE(2) / 77777B, 177776B /
C      DATA RIGHT(1), RIGHT(2) / 40000B,    325B /
C      DATA DIVER(1), DIVER(2) / 40000B,    327B /
C      DATA LOG10(1), LOG10(2) / 46420B,  46777B /
C
C  HP 2100, 4 word double precision with FTN4
C
C      DATA SMALL(1), SMALL(2) / 40000B,       1 /
C      DATA LARGE91), LARGE(2) / 77777B, 177776B /
C      DATA RIGHT(1), RIGHT(2) / 40000B,    325B /
C      DATA DIVER(1), DIVER(2) / 40000B,    327B /
C      DATA LOG10(1), LOG10(2) / 46420B,  46777B /
C
C  HP 9000
C
C      R1MACH(1) = 1.17549435E-38
C      R1MACH(2) = 1.70141163E+38
C      R1MACH(3) = 5.960464478E-8
C      R1MACH(4) = 1.119209290E-7
C      R1MACH(5) = 3.01030010E-1
C
C      DATA SMALL(1) / 00040000000B /
C      DATA LARGE(1) / 17677777777B /
C      DATA RIGHT(1) / 06340000000B /
C      DATA DIVER(1) / 06400000000B /
C      DATA LOG10(1) / 07646420233B /
C
C  IBM 360/370 series, XEROX SIGMA 5/7/9, SEL systems 85/86, PERKIN ELMER 3230, 
C  and PERKIN ELMER (INTERDATA) 3230.
C
C      DATA RMACH(1) / Z00100000 /
C      DATA RMACH(2) / Z7FFFFFFF /
C      DATA RMACH(3) / Z3B100000 /
C      DATA RMACH(4) / Z3C100000 /
C      DATA RMACH(5) / Z41134413 /
C
C  IBM PC - Microsoft FORTRAN
C
C      DATA SMALL(1) / #00800000 /
C      DATA LARGE(1) / #7F7FFFFF /
C      DATA RIGHT(1) / #33800000 /
C      DATA DIVER(1) / #34000000 /
C      DATA LOG10(1) / #3E9A209A /
C
C  IBM PC - Professional FORTRAN and Lahey FORTRAN
C
C      DATA SMALL(1)/ Z'00800000' /
C      DATA LARGE(1)/ Z'7F7FFFFF' /
C      DATA RIGHT(1)/ Z'33800000' /
C      DATA DIVER(1)/ Z'34000000' /
C      DATA LOG10(1)/ Z'3E9A209A' /
C
C  INTERDATA 8/32 with the UNIX system FORTRAN 77 compiler.
C  For the INTERDATA FORTRAN VII compiler replace the Z'S specifying HEX 
C  constants with Y'S.
C
C      DATA RMACH(1) / Z'00100000' /
C      DATA RMACH(2) / Z'7EFFFFFF' /
C      DATA RMACH(3) / Z'3B100000' /
C      DATA RMACH(4) / Z'3C100000' /
C      DATA RMACH(5) / Z'41134413' /
C
C  PDP-10 (KA or KI processor).
C
C      DATA RMACH(1) / "000400000000 /
C      DATA RMACH(2) / "377777777777 /
C      DATA RMACH(3) / "146400000000 /
C      DATA RMACH(4) / "147400000000 /
C      DATA RMACH(5) / "177464202324 /
C
C  PDP-11 FORTRANS supporting 32-bit integers (integer version).
C
C      DATA SMALL(1) /    8388608 /
C      DATA LARGE(1) / 2147483647 /
C      DATA RIGHT(1) /  880803840 /
C      DATA DIVER(1) /  889192448 /
C      DATA LOG10(1) / 1067065499 /
C
C  PDP-11 FORTRANS supporting 32-bit integers (octal version).
C
C      DATA RMACH(1) / O00040000000 /
C      DATA RMACH(2) / O17777777777 /
C      DATA RMACH(3) / O06440000000 /
C      DATA RMACH(4) / O06500000000 /
C      DATA RMACH(5) / O07746420233 /
C
C  PDP-11 FORTRANS supporting 16-bit integers (integer version).
C
C      DATA SMALL(1),SMALL(2) /   128,     0 /
C      DATA LARGE(1),LARGE(2) / 32767,    -1 /
C      DATA RIGHT(1),RIGHT(2) / 13440,     0 /
C      DATA DIVER(1),DIVER(2) / 13568,     0 /
C      DATA LOG10(1),LOG10(2) / 16282,  8347 /
C
C  PDP-11 FORTRANS supporting 16-bit integers (octal version).
C
C      DATA SMALL(1),SMALL(2) / O000200, O000000 /
C      DATA LARGE(1),LARGE(2) / O077777, O177777 /
C      DATA RIGHT(1),RIGHT(2) / O032200, O000000 /
C      DATA DIVER(1),DIVER(2) / O032400, O000000 /
C      DATA LOG10(1),LOG10(2) / O037632, O020233 /
C
C  SEQUENT BALANCE 8000.
C
C      DATA SMALL(1) / $00800000 /
C      DATA LARGE(1) / $7F7FFFFF /
C      DATA RIGHT(1) / $33800000 /
C      DATA DIVER(1) / $34000000 /
C      DATA LOG10(1) / $3E9A209B /
C
C  SUN Microsystems UNIX F77 compiler.
C
C      DATA RMACH(1) / 1.17549435E-38 /
C      DATA RMACH(2) / 3.40282347E+38 /
C      DATA RMACH(3) / 5.96016605E-08 /
C      DATA RMACH(4) / 1.19203321E-07 /
C      DATA RMACH(5) / 3.01030010E-01 /
C
C  SUN 3 (68881 or FPA)
C
C      DATA SMALL(1) / X'00800000' /
C      DATA LARGE(1) / X'7F7FFFFF' /
C      DATA RIGHT(1) / X'33800000' /
C      DATA DIVER(1) / X'34000000' /
C      DATA LOG10(1) / X'3E9A209B' /
C
C  UNIVAC 1100 series.
C
C      DATA RMACH(1) / O000400000000 /
C      DATA RMACH(2) / O377777777777 /
C      DATA RMACH(3) / O146400000000 /
C      DATA RMACH(4) / O147400000000 /
C      DATA RMACH(5) / O177464202324 /
C
C  VAX/ULTRIX F77 compiler.
C
C      DATA SMALL(1) /       128 /
C      DATA LARGE(1) /    -32769 /
C      DATA RIGHT(1) /     13440 /
C      DATA DIVER(1) /     13568 /
C      DATA LOG10(1) / 547045274 /
C
C  VAX-11 with FORTRAN IV-PLUS compiler.
C
C      DATA RMACH(1) / Z00000080 /
C      DATA RMACH(2) / ZFFFF7FFF /
C      DATA RMACH(3) / Z00003480 /
C      DATA RMACH(4) / Z00003500 /
C      DATA RMACH(5) / Z209B3F9A /
C
C  VAX/VMS version 2.2.
C
C      DATA RMACH(1) /       '80'X /
C      DATA RMACH(2) / 'FFFF7FFF'X /
C      DATA RMACH(3) /     '3480'X /
C      DATA RMACH(4) /     '3500'X /
C      DATA RMACH(5) / '209B3F9A'X /
C
C  VAX/VMS 11/780 
C
C      DATA SMALL(1) / Z00000080 /
C      DATA LARGE(1) / ZFFFF7FFF /
C      DATA RIGHT(1) / Z00003480 /
C      DATA DIVER(1) / Z00003500 /
C      DATA LOG10(1) / Z209B3F9A /
C
C  Z80 microprocessor.
C
C      DATA SMALL(1), SMALL(2) /     0,    256 /
C      DATA LARGE(1), LARGE(2) /    -1,   -129 /
C      DATA RIGHT(1), RIGHT(2) /     0,  26880 /
C      DATA DIVER(1), DIVER(2) /     0,  27136 /
C      DATA LOG10(1), LOG10(2) /  8347,  32538 /
C
      IF(I.LT.1.OR.I.GT.5)THEN
        WRITE(*,*)'R1MACH - ERROR, I out of bounds=',I
        R1MACH=0.0
      ELSE
        R1MACH = RMACH(I)
        ENDIF
      RETURN
      END
