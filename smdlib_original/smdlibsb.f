c  MACHINESGI.F  18 June 1993
c
      function d1mach(i)
c
c***********************************************************************
c
c  D1MACH returns double precision machine constants.
c
c  Assuming that the internal representation of a double precision number is
c  in base B, with T the number of base-B digits in the mantissa, and EMIN the
c  smallest possible exponent and EMAX the largest possible exponent, then
c
c    D1MACH(1) = B**(EMIN-1), the smallest positive magnitude.
c    D1MACH(2) = B**EMAX*(1-B**(-T)), the largest magnitude.
c    D1MACH(3) = B**(-T), the smallest relative spacing.
c    D1MACH(4) = B**(1-T), the largest relative spacing.
c    D1MACH(5) = log10(B).
c
c  To alter this function for a particular environment, the desired set of DATA
c  statements should be activated by removing the C from column 1.  On rare
c  machines, a STATIC statement may need to be added, but probably more systems
c  prohibit than require it.
c
c  For IEEE-arithmetic machines (binary standard), one of the first two sets of
c  constants below should be appropriate.
c
c  Where possible, octal or hexadecimal constants have been used to specify the
c  constants exactly, which has in some cases required the use of EQUIVALENCED
c  integer arrays.
c
      double precision d1mach
      integer diver(4)
      double precision dmach(5)
      integer i
      integer large(4)
      integer log10(4)
      integer right(4)
      integer small(4)
c
      equivalence (dmach(1),small(1))
      equivalence (dmach(2),large(1))
      equivalence (dmach(3),right(1))
      equivalence (dmach(4),diver(1))
      equivalence (dmach(5),log10(1))
c
c  IEEE arithmetic machines, such as the AT&T 3B series and 
c  Motorola 68000 based machines such as the SUN 3 and AT&T PC 
c  7300, and the SGI Iris, in which the most significant byte is 
c  stored first.
c
       data small(1),small(2) /    1048576,          0 /
       data large(1),large(2) / 2146435071,         -1 /
       data right(1),right(2) / 1017118720,          0 /
       data diver(1),diver(2) / 1018167296,          0 /
       data log10(1),log10(2) / 1070810131, 1352628735 /
c
c  IEEE arithmetic machines and 8087-based micros, such as the IBM PC,
c  AT&T 6300, DEC PMAX, DEC ALPHA, NEXT, in which the most 
c  significant byte is stored last.
c
c      data small(1),small(2) /          0,    1048576 /
c      data large(1),large(2) /         -1, 2146435071 /
c      data right(1),right(2) /          0, 1017118720 /
c      data diver(1),diver(2) /          0, 1018167296 /
c      data log10(1),log10(2) / 1352628735, 1070810131 /
c
c  ALLIANT FX/8 UNIX FORTRAN compiler.
c
c      data dmach(1) / 2.22507385850721D-308 /
c      data dmach(2) / 1.79769313486231D+308 /
c      data dmach(3) / 1.1101827117665D-16 /
c      data dmach(4) / 2.2203654423533D-16 /
c      data dmach(5) / 3.01029995663981E-1 /
c
c  AMDAHL machines.
c
c      data small(1),small(2) /    1048576,          0 /
c      data large(1),large(2) / 2147483647,         -1 /
c      data right(1),right(2) /  856686592,          0 /
c      data diver(1),diver(2) /  873463808,          0 /
c      data log10(1),log10(2) / 1091781651, 1352628735 /
c
c  BURROUGHS 1700 system.
c
c      data small(1) / ZC00800000 /
c      data small(2) / Z000000000 /
c
c      data large(1) / ZDFFFFFFFF /
c      data large(2) / ZFFFFFFFFF /
c
c      data right(1) / ZCC5800000 /
c      data right(2) / Z000000000 /
c
c      data diver(1) / ZCC6800000 /
c      data diver(2) / Z000000000 /
c
c      data log10(1) / ZD00E730E7 /
c      data log10(2) / ZC77800DC0 /
c
c  BURROUGHS 5700 system.
c
c      data small(1) / O1771000000000000 /
c      data small(2) / O0000000000000000 /
c
c      data large(1) / O0777777777777777 /
c      data large(2) / O0007777777777777 /
c
c      data right(1) / O1461000000000000 /
c      data right(2) / O0000000000000000 /
c
c      data diver(1) / O1451000000000000 /
c      data diver(2) / O0000000000000000 /
c
c      data log10(1) / O1157163034761674 /
c      data log10(2) / O0006677466732724 /
c
c  BURROUGHS 6700/7700 systems.
c
c      data small(1) / O1771000000000000 /
c      data small(2) / O7770000000000000 /
c
c      data large(1) / O0777777777777777 /
c      data large(2) / O7777777777777777 /
c
c      data right(1) / O1461000000000000 /
c      data right(2) / O0000000000000000 /
c
c      data diver(1) / O1451000000000000 /
c      data diver(2) / O0000000000000000 /
c
c      data log10(1) / O1157163034761674 /
c      data log10(2) / O0006677466732724 /
c
c  CDC CYBER 170/180 series using NOS
c
c      data small(1) / O"00604000000000000000" /
c      data small(2) / O"00000000000000000000" /
c
c      data large(1) / O"37767777777777777777" /
c      data large(2) / O"37167777777777777777" /
c
c      data right(1) / O"15604000000000000000" /
c      data right(2) / O"15000000000000000000" /
c
c      data diver(1) / O"15614000000000000000" /
c      data diver(2) / O"15010000000000000000" /
c
c      data log10(1) / O"17164642023241175717" /
c      data log10(2) / O"16367571421742254654" /
c
c  CDC CYBER 170/180 series using NOS/VE
c
c      data small(1) / Z"3001800000000000" /
c      data small(2) / Z"3001000000000000" /
c
c      data large(1) / Z"4FFEFFFFFFFFFFFE" /
c      data large(2) / Z"4FFE000000000000" /
c
c      data right(1) / Z"3FD2800000000000" /
c      data right(2) / Z"3FD2000000000000" /
c
c      data diver(1) / Z"3FD3800000000000" /
c      data diver(2) / Z"3FD3000000000000" /
c
c      data log10(1) / Z"3FFF9A209A84FBCF" /
c      data log10(2) / Z"3FFFF7988F8959AC" /
c
c  CDC CYBER 200 series
c
c      data small(1) / X'9000400000000000' /
c      data small(2) / X'8FD1000000000000' /
c
c      data large(1) / X'6FFF7FFFFFFFFFFF' /
c      data large(2) / X'6FD07FFFFFFFFFFF' /
c
c      data right(1) / X'FF74400000000000' /
c      data right(2) / X'FF45000000000000' /
c
c      data diver(1) / X'FF75400000000000' /
c      data diver(2) / X'FF46000000000000' /
c
c      data log10(1) / X'FFD04D104D427DE7' /
c      data log10(2) / X'FFA17DE623E2566A' /
c
c  CDC 6000/7000 series using FTN4.
c
c      data small(1) / 00564000000000000000B /
c      data small(2) / 00000000000000000000B /
c
c      data large(1) / 37757777777777777777B /
c      data large(2) / 37157777777777777774B /
c
c      data right(1) / 15624000000000000000B /
c      data right(2) / 00000000000000000000B /
c
c      data diver(1) / 15634000000000000000B /
c      data diver(2) / 00000000000000000000B /
c
c      data log10(1) / 17164642023241175717B /
c      data log10(2) / 16367571421742254654B /
c
c  CDC 6000/7000 series using FTN5.
c
c      data small(1) / O"00564000000000000000" /
c      data small(2) / O"00000000000000000000" /
c
c      data large(1) / O"37757777777777777777" /
c      data large(2) / O"37157777777777777774" /
c
c      data right(1) / O"15624000000000000000" /
c      data right(2) / O"00000000000000000000" /
c
c      data diver(1) / O"15634000000000000000" /
c      data diver(2) / O"00000000000000000000" /
c
c      data log10(1) / O"17164642023241175717" /
c      data log10(2) / O"16367571421742254654" /
c
c  CONVEX C-1
c
c      data small(1),small(2) / '00100000'X, '00000000'X /
c      data large(1),large(2) / '7FFFFFFF'X, 'FFFFFFFF'X /
c      data right(1),right(2) / '3CC00000'X, '00000000'X /
c      data diver(1),diver(2) / '3CD00000'X, '00000000'X /
c      data log10(1),log10(2) / '3FF34413'X, '509F79FF'X /
c
c  CONVEX C-120 (native mode) with or without -R8 option
c
c      data dmach(1) / 5.562684646268007D-309 /
c      data dmach(2) / 8.988465674311577D+307 /
c      data dmach(3) / 1.110223024625157D-016 /
c      data dmach(4) / 2.220446049250313D-016 /
c      data dmach(5) / 3.010299956639812D-001 /
c
c  CONVEX C-120 (IEEE mode) with or without -R8 option
c
c      data dmach(1) / 2.225073858507202D-308 /
c      data dmach(2) / 1.797693134862315D+308 /
c      data dmach(3) / 1.110223024625157D-016 /
c      data dmach(4) / 2.220446049250313D-016 /
c      data dmach(5) / 3.010299956639812D-001 /
c
c  CRAY 1, 2, XMP and YMP.
c
c      data small(1) / 201354000000000000000B /
c      data small(2) / 000000000000000000000B /
c
c      data large(1) / 577767777777777777777B /
c      data large(2) / 000007777777777777776B /
c
c      data right(1) / 376434000000000000000B /
c      data right(2) / 000000000000000000000B /
c
c      data diver(1) / 376444000000000000000B /
c      data diver(2) / 000000000000000000000B /
c
c      data log10(1) / 377774642023241175717B /
c      data log10(2) / 000007571421742254654B /
c
c  DATA GENERAL ECLIPSE S/200
c  Note - It may be appropriate to include the line: STATIC dmach(5)
c
c      data small /20K,3*0/
c      data large /77777K,3*177777K/
c      data right /31420K,3*0/
c      data diver /32020K,3*0/
c      data log10 /40423K,42023K,50237K,74776K/
c
c  ELXSI 6400, assuming REAL*8 is the default DOUBLE PRECISION type.
c
c      data small(1), small(2) / '00100000'X,'00000000'X /
c      data large(1), large(2) / '7FEFFFFF'X,'FFFFFFFF'X /
c      data right(1), right(2) / '3CB00000'X,'00000000'X /
c      data diver(1), diver(2) / '3CC00000'X,'00000000'X /
c      data log10(1), diver(2) / '3FD34413'X,'509F79FF'X /
c
c  HARRIS 220
c
c      data small(1),small(2) / '20000000, '00000201 /
c      data large(1),large(2) / '37777777, '37777577 /
c      data right(1),right(2) / '20000000, '00000333 /
c      data diver(1),diver(2) / '20000000, '00000334 /
c      data log10(1),log10(2) / '23210115, '10237777 /
c
c  HARRIS SLASH 6 and SLASH 7
c
c      data small(1),small(2) / '20000000, '00000201 /
c      data large(1),large(2) / '37777777, '37777577 /
c      data right(1),right(2) / '20000000, '00000333 /
c      data diver(1),diver(2) / '20000000, '00000334 /
c      data log10(1),log10(2) / '23210115, '10237777 /
c
c  HONEYWELL DPS 8/70 and 600/6000 series.
c
c      data small(1),small(2) / O402400000000, O000000000000 /
c      data large(1),large(2) / O376777777777, O777777777777 /
c      data right(1),right(2) / O604400000000, O000000000000 /
c      data diver(1),diver(2) / O606400000000, O000000000000 /
c      data log10(1),log10(2) / O776464202324, O117571775714 /
c
c  HP 2100, three word double precision option with FTN4.
c
c      data small(1), small(2), small(3) / 40000B,       0,       1 /
c      data large(1), large(2), large(3) / 77777B, 177777B, 177776B /
c      data right(1), right(2), right(3) / 40000B,       0,    265B /
c      data diver(1), diver(2), diver(3) / 40000B,       0,    276B /
c      data log10(1), log10(2), log10(3) / 46420B,  46502B,  77777B /
c
c  HP 2100, four word double precision option with FTN4.
c
c      data small(1), small(2) /  40000B,       0 /
c      data small(3), small(4) /       0,       1 /
c      data large(1), large(2) /  77777B, 177777B /
c      data large(3), large(4) / 177777B, 177776B /
c      data right(1), right(2) /  40000B,       0 /
c      data right(3), right(4) /       0,    225B /
c      data diver(1), diver(2) /  40000B,       0 /
c      data diver(3), diver(4) /       0,    227B /
c      data log10(1), log10(2) /  46420B,  46502B /
c      data log10(3), log10(4) /  76747B, 176377B /
c
c  HP 9000
c
c      d1mach(1) = 2.8480954D-306
c      d1mach(2) = 1.40444776D+306
c      d1mach(3) = 2.22044605D-16
c      d1mach(4) = 4.44089210D-16
c      d1mach(5) = 3.01029996D-1
c
c      data small(1), small(2) / 00040000000B, 00000000000B /
c      data large(1), large(2) / 17737777777B, 37777777777B /
c      data right(1), right(2) / 07454000000B, 00000000000B /
c      data diver(1), diver(2) / 07460000000B, 00000000000B /
c      data log10(1), log10(2) / 07764642023B, 12047674777B /
c
c  IBM 360/370 series, XEROX SIGMA 5/7/9, SEL SYSTEMS 85/86, PERKIN ELMER 3230,
c  and PERKIN ELMER (INTERDATA) 3230.
c
c      data small(1),small(2) / Z00100000, Z00000000 /
c      data large(1),large(2) / Z7FFFFFFF, ZFFFFFFFF /
c      data right(1),right(2) / Z33100000, Z00000000 /
c      data diver(1),diver(2) / Z34100000, Z00000000 /
c      data log10(1),log10(2) / Z41134413, Z509F79FF /
c
c  IBM PC - Microsoft FORTRAN
c
c      data small(1), small(2) / #00000000, #00100000 /
c      data large(1), large(2) / #FFFFFFFF, #7FEFFFFF /
c      data right(1), right(2) / #00000000, #3CA00000 /
c      data diver(1), diver(2) / #00000000, #3CB00000 /
c      data log10(1), log10(2) / #509F79FF, #3FD34413 /
c
c  IBM PC - Professional FORTRAN and Lahey FORTRAN
c
c      data small(1), small(2) / Z'00000000', Z'00100000' /
c      data large(1), large(2) / Z'FFFFFFFF', Z'7FEFFFFF' /
c      data right(1), right(2) / Z'00000000', Z'3CA00000' /
c      data diver(1), diver(2) / Z'00000000', Z'3CB00000' /
c      data log10(1), log10(2) / Z'509F79FF', Z'3FD34413' /
c
c  INTERDATA 8/32 with the UNIX system FORTRAN 77 compiler.
c  For the INTERDATA FORTRAN VII compiler, replace the Z's specifying hex
c  constants with Y's.
c
c      data small(1),small(2) / Z'00100000', Z'00000000' /
c      data large(1),large(2) / Z'7EFFFFFF', Z'FFFFFFFF' /
c      data right(1),right(2) / Z'33100000', Z'00000000' /
c      data diver(1),diver(2) / Z'34100000', Z'00000000' /
c      data log10(1),log10(2) / Z'41134413', Z'509F79FF' /
c
c  PDP-10 (KA processor).
c
c      data small(1),small(2) / "033400000000, "000000000000 /
c      data large(1),large(2) / "377777777777, "344777777777 /
c      data right(1),right(2) / "113400000000, "000000000000 /
c      data diver(1),diver(2) / "114400000000, "000000000000 /
c      data log10(1),log10(2) / "177464202324, "144117571776 /
c
c  PDP-10 (KI processor).
c
c      data small(1),small(2) / "000400000000, "000000000000 /
c      data large(1),large(2) / "377777777777, "377777777777 /
c      data right(1),right(2) / "103400000000, "000000000000 /
c      data diver(1),diver(2) / "104400000000, "000000000000 /
c      data log10(1),log10(2) / "177464202324, "047674776746 /
c
c  PDP-11 FORTRANS supporting 32-bit integers (integer version).
c
c      data small(1),small(2) /    8388608,           0 /
c      data large(1),large(2) / 2147483647,          -1 /
c      data right(1),right(2) /  612368384,           0 /
c      data diver(1),diver(2) /  620756992,           0 /
c      data log10(1),log10(2) / 1067065498, -2063872008 /
c
c  PDP-11 FORTRANS supporting 32-bit integers (octal version)
c
c      data small(1),small(2) / O00040000000, O00000000000 /
c      data large(1),large(2) / O17777777777, O37777777777 /
c      data right(1),right(2) / O04440000000, O00000000000 /
c      data diver(1),diver(2) / O04500000000, O00000000000 /
c      data log10(1),log10(2) / O07746420232, O20476747770 /
c
c  PDP-11 FORTRANS supporting 16-bit integers (integer version).
c
c      data small(1),small(2) /    128,      0 /
c      data small(3),small(4) /      0,      0 /
c
c      data large(1),large(2) /  32767,     -1 /
c      data large(3),large(4) /     -1,     -1 /
c
c      data right(1),right(2) /   9344,      0 /
c      data right(3),right(4) /      0,      0 /
c
c      data diver(1),diver(2) /   9472,      0 /
c      data diver(3),diver(4) /      0,      0 /
c
c      data log10(1),log10(2) /  16282,   8346 /
c      data log10(3),log10(4) / -31493, -12296 /
c
c  PDP-11 FORTRANS supporting 16-bit integers (octal version).
c
c      data small(1),small(2) / O000200, O000000 /
c      data small(3),small(4) / O000000, O000000 /
c
c      data large(1),large(2) / O077777, O177777 /
c      data large(3),large(4) / O177777, O177777 /
c
c      data right(1),right(2) / O022200, O000000 /
c      data right(3),right(4) / O000000, O000000 /
c
c      data diver(1),diver(2) / O022400, O000000 /
c      data diver(3),diver(4) / O000000, O000000 /
c
c      data log10(1),log10(2) / O037632, O020232 /
c      data log10(3),log10(4) / O102373, O147770 /
c
c  PRIME 50 series systems with 32-bit integers and 64V MODE instructions,
c  supplied by Igor Bray.
c
c      data small(1),small(2) / :10000000000, :00000100001 /
c      data large(1),large(2) / :17777777777, :37777677775 /
c      data right(1),right(2) / :10000000000, :00000000122 /
c      data diver(1),diver(2) / :10000000000, :00000000123 /
c      data log10(1),log10(2) / :11504046501, :07674600177 /
c
c  SEQUENT BALANCE 8000
c
c      data small(1),small(2) / $00000000,  $00100000 /
c      data large(1),large(2) / $FFFFFFFF,  $7FEFFFFF /
c      data right(1),right(2) / $00000000,  $3CA00000 /
c      data diver(1),diver(2) / $00000000,  $3CB00000 /
c      data log10(1),log10(2) / $509F79FF,  $3FD34413 /
c
c  SUN Microsystems UNIX F77 compiler.
c
c      data dmach(1) / 2.22507385850720D-308 /
c      data dmach(2) / 1.79769313486231D+308 /
c      data dmach(3) / 1.1101827117665D-16 /
c      data dmach(4) / 2.2203654423533D-16 /
c      data dmach(5) / 3.01029995663981D-1 /
c
c  SUN 3 (68881 or FPA)
c
c      data small(1),small(2) / X'00100000', X'00000000' /
c      data large(1),large(2) / X'7FEFFFFF', X'FFFFFFFF' /
c      data right(1),right(2) / X'3CA00000', X'00000000' /
c      data diver(1),diver(2) / X'3CB00000', X'00000000' /
c      data log10(1),log10(2) / X'3FD34413', X'509F79FF' /
c
c  UNIVAC 1100 series.
c
c      data small(1),small(2) / O000040000000, O000000000000 /
c      data large(1),large(2) / O377777777777, O777777777777 /
c      data right(1),right(2) / O170540000000, O000000000000 /
c      data diver(1),diver(2) / O170640000000, O000000000000 /
c      data log10(1),log10(2) / O177746420232, O411757177572 /
c
c  VAX/ULTRIX F77 compiler
c
c      data small(1),small(2) /        128,           0 /
c      data large(1),large(2) /     -32769,          -1 /
c      data right(1),right(2) /       9344,           0 /
c      data diver(1),diver(2) /       9472,           0 /
c      data log10(1),log10(2) /  546979738,  -805796613 /
c
c  VAX/ULTRIX F77 compiler, G floating
c
c      data small(1), small(2) /         16,           0 /
c      data large(1), large(2) /     -32769,          -1 /
c      data right(1), right(2) /      15552,           0 /
c      data diver(1), diver(2) /      15568,           0 /
c      data log10(1), log10(2) /  1142112243, 2046775455 /
c
c  VAX-11 with FORTRAN IV-PLUS compiler
c
c      data small(1),small(2) / Z00000080, Z00000000 /
c      data large(1),large(2) / ZFFFF7FFF, ZFFFFFFFF /
c      data right(1),right(2) / Z00002480, Z00000000 /
c      data diver(1),diver(2) / Z00002500, Z00000000 /
c      data log10(1),log10(2) / Z209A3F9A, ZCFF884FB /
c
c  VAX/VMS version 2.2
c
c      data small(1),small(2) /       '80'X,        '0'X /
c      data large(1),large(2) / 'FFFF7FFF'X, 'FFFFFFFF'X /
c      data right(1),right(2) /     '2480'X,        '0'X /
c      data diver(1),diver(2) /     '2500'X,        '0'X /
c      data log10(1),log10(2) / '209A3F9A'X, 'CFF884FB'X /
c
c  VAX/VMS 11/780
c
c      data small(1), small(2) / Z00000080, Z00000000 /
c      data large(1), large(2) / ZFFFF7FFF, ZFFFFFFFF /
c      data right(1), right(2) / Z00002480, Z00000000 /
c      data diver(1), diver(2) / Z00002500, Z00000000 /
c      data log10(1), log10(2) / Z209A3F9A, ZCFF884FB /
c
c  VAX/VMS 11/780 (G-FLOATING)
c
c      data small(1), small(2) / Z00000010, Z00000000 /
c      data large(1), large(2) / ZFFFF7FFF, ZFFFFFFFF /
c      data right(1), right(2) / Z00003CC0, Z00000000 /
c      data diver(1), diver(2) / Z00003CD0, Z00000000 /
c      data log10(1), log10(2) / Z44133FF3, Z79FF509F /
c
      if(i.lt.1.or.i.gt.5)then
        write(*,*)'D1MACH - Error, I out of bounds:',i
        d1mach=0.0d0
        stop
      else
        d1mach = dmach(i)
      endif

      return
      end
      function i1mach(i)
c
c***********************************************************************
c
c  I1MACH returns integer machine constants.
c
c  I/O unit numbers.
c
c    I1MACH(1) = the standard input unit.
c    I1MACH(2) = the standard output unit.
c    I1MACH(3) = the standard punch unit.
c    I1MACH(4) = the standard error message unit.
c
c  Words.
c
c    I1MACH(5) = the number of bits per integer storage unit.
c    I1MACH(6) = the number of characters per integer storage unit.
c
c  Integers.
c
c  Assume integers are represented in the S digit base A form:
c
c  Sign * (X(S-1)*A**(S-1) + ... + X(1)*A + X(0))
c  where 0<=X(I)<A for I=0 to S-1.
c
c    I1MACH(7) = A, the base.
c    I1MACH(8) = S, the number of base A digits.
c    I1MACH(9) = A**S-1, the largest integer.
c
c  Floating point numbers
c
c  Assume floating point numbers are represented in the T digit base B form:
c
c    Sign * (B**E) * ((X(1)/B) + ... + (X(T)/B**T) )
c
c  where 0<=X(I)<B for I=1 to T, 0<X(1) and EMIN<=E<=EMAX
c
c    I1MACH(10) = B, the base.
c
c  Single precision
c
c    I1MACH(11) = T, the number of base B digits.
c    I1MACH(12) = EMIN, the smallest exponent E.
c    I1MACH(13) = EMAX, the largest exponent E.
c
c  Double precision
c
c    I1MACH(14) = T, the number of base B digits.
c    I1MACH(15) = EMIN, the smallest exponent E.
c    I1MACH(16) = EMAX, the largest exponent E.
c
c  To alter this function for a particular environment, the desired set of DATA
c  statements should be activated by removing the C from column 1.  On rare
c  machines, a STATIC statement may need to be added, but probably more systems
c  prohibit than require it.
c
c  Also, the values of I1MACH(1) through I1MACH(4) should be checked for
c  consistency with the local operating system.  For FORTRAN 77, you may wish
c  to adjust the data statement so imach(6) is set to 1, and then to comment
c  out the executable test on I.EQ.6 below.
c
c  For IEEE-arithmetic machines (binary standard), the first set of constants
c  below should be appropriate, except perhaps for IMACH(1) - IMACH(4).
c
      integer i
      integer i1mach
      integer imach(16)
      integer output
c
      equivalence (imach(4),output)
c
c  IEEE arithmetic machines, such as the AT&T 3B series, Motorola 
c  68000 based machines such as the SUN 3 and AT&T PC 7300, and 
c  8087 based micros such asthe IBM PC and AT&T 6300.
c
       data imach( 1) /    5 /
       data imach( 2) /    6 /
       data imach( 3) /    7 /
       data imach( 4) /    6 /
       data imach( 5) /   32 /
       data imach( 6) /    4 /
       data imach( 7) /    2 /
       data imach( 8) /   31 /
       data imach( 9) / 2147483647 /
       data imach(10) /    2 /
       data imach(11) /   24 /
       data imach(12) / -125 /
       data imach(13) /  128 /
       data imach(14) /   53 /
       data imach(15) / -1021 /
       data imach(16) /  1024 /
c
c  ALLIANT FX/8 UNIX FORTRAN compiler.
c
c      data imach( 1) /     5 /
c      data imach( 2) /     6 /
c      data imach( 3) /     6 /
c      data imach( 4) /     0 /
c      data imach( 5) /    32 /
c      data imach( 6) /     4 /
c      data imach( 7) /     2 /
c      data imach( 8) /    32 /
c      data imach( 9) /2147483647/
c      data imach(10) /     2 /
c      data imach(11) /    24 /
c      data imach(12) /  -126 /
c      data imach(13) /   128 /
c      data imach(14) /    53 /
c      data imach(15) / -1022 /
c      data imach(16) /  1024 /
c
c  AMDAHL machines.
c
c      data imach( 1) /   5 /
c      data imach( 2) /   6 /
c      data imach( 3) /   7 /
c      data imach( 4) /   6 /
c      data imach( 5) /  32 /
c      data imach( 6) /   4 /
c      data imach( 7) /   2 /
c      data imach( 8) /  31 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /  16 /
c      data imach(11) /   6 /
c      data imach(12) / -64 /
c      data imach(13) /  63 /
c      data imach(14) /  14 /
c      data imach(15) / -64 /
c      data imach(16) /  63 /
c
c  BURROUGHS 1700 system.
c
c      data imach( 1) /    7 /
c      data imach( 2) /    2 /
c      data imach( 3) /    2 /
c      data imach( 4) /    2 /
c      data imach( 5) /   36 /
c      data imach( 6) /    4 /
c      data imach( 7) /    2 /
c      data imach( 8) /   33 /
c      data imach( 9) / Z1FFFFFFFF /
c      data imach(10) /    2 /
c      data imach(11) /   24 /
c      data imach(12) / -256 /
c      data imach(13) /  255 /
c      data imach(14) /   60 /
c      data imach(15) / -256 /
c      data imach(16) /  255 /
c
c  BURROUGHS 5700 system.
c
c      data imach( 1) /   5 /
c      data imach( 2) /   6 /
c      data imach( 3) /   7 /
c      data imach( 4) /   6 /
c      data imach( 5) /  48 /
c      data imach( 6) /   6 /
c      data imach( 7) /   2 /
c      data imach( 8) /  39 /
c      data imach( 9) / O0007777777777777 /
c      data imach(10) /   8 /
c      data imach(11) /  13 /
c      data imach(12) / -50 /
c      data imach(13) /  76 /
c      data imach(14) /  26 /
c      data imach(15) / -50 /
c      data imach(16) /  76 /
c
c  BURROUGHS 6700/7700 systems.
c
c      data imach( 1) /   5 /
c      data imach( 2) /   6 /
c      data imach( 3) /   7 /
c      data imach( 4) /   6 /
c      data imach( 5) /  48 /
c      data imach( 6) /   6 /
c      data imach( 7) /   2 /
c      data imach( 8) /  39 /
c      data imach( 9) / O0007777777777777 /
c      data imach(10) /   8 /
c      data imach(11) /  13 /
c      data imach(12) / -50 /
c      data imach(13) /  76 /
c      data imach(14) /  26 /
c      data imach(15) / -32754 /
c      data imach(16) /  32780 /
c
c  CDC CYBER 170/180 series using NOS
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    7 /
c      data imach( 4) /    6 /
c      data imach( 5) /   60 /
c      data imach( 6) /   10 /
c      data imach( 7) /    2 /
c      data imach( 8) /   48 /
c      data imach( 9) / O"00007777777777777777" /
c      data imach(10) /    2 /
c      data imach(11) /   48 /
c      data imach(12) / -974 /
c      data imach(13) / 1070 /
c      data imach(14) /   96 /
c      data imach(15) / -927 /
c      data imach(16) / 1070 /
c
c  CDC CYBER 170/180 series using NOS/VE
c
c      data imach( 1) /     5 /
c      data imach( 2) /     6 /
c      data imach( 3) /     7 /
c      data imach( 4) /     6 /
c      data imach( 5) /    64 /
c      data imach( 6) /     8 /
c      data imach( 7) /     2 /
c      data imach( 8) /    63 /
c      data imach( 9) / 9223372036854775807 /
c      data imach(10) /     2 /
c      data imach(11) /    47 /
c      data imach(12) / -4095 /
c      data imach(13) /  4094 /
c      data imach(14) /    94 /
c      data imach(15) / -4095 /
c      data imach(16) /  4094 /
c
c  CDC CYBER 200 series
c
c      data imach( 1) /      5 /
c      data imach( 2) /      6 /
c      data imach( 3) /      7 /
c      data imach( 4) /      6 /
c      data imach( 5) /     64 /
c      data imach( 6) /      8 /
c      data imach( 7) /      2 /
c      data imach( 8) /     47 /
c      data imach( 9) / X'00007FFFFFFFFFFF' /
c      data imach(10) /      2 /
c      data imach(11) /     47 /
c      data imach(12) / -28625 /
c      data imach(13) /  28718 /
c      data imach(14) /     94 /
c      data imach(15) / -28625 /
c      data imach(16) /  28718 /
c
c  CDC 6000/7000 series using FTN4.
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    7 /
c      data imach( 4) /    6 /
c      data imach( 5) /   60 /
c      data imach( 6) /   10 /
c      data imach( 7) /    2 /
c      data imach( 8) /   48 /
c      data imach( 9) / 00007777777777777777B /
c      data imach(10) /    2 /
c      data imach(11) /   47 /
c      data imach(12) / -929 /
c      data imach(13) / 1070 /
c      data imach(14) /   94 /
c      data imach(15) / -929 /
c      data imach(16) / 1069 /
c
c  CDC 6000/7000 series using FTN5.
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    7 /
c      data imach( 4) /    6 /
c      data imach( 5) /   60 /
c      data imach( 6) /   10 /
c      data imach( 7) /    2 /
c      data imach( 8) /   48 /
c      data imach( 9) / O"00007777777777777777" /
c      data imach(10) /    2 /
c      data imach(11) /   47 /
c      data imach(12) / -929 /
c      data imach(13) / 1070 /
c      data imach(14) /   94 /
c      data imach(15) / -929 /
c      data imach(16) / 1069 /
c
c  CONVEX C-1.
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    7 /
c      data imach( 4) /    6 /
c      data imach( 5) /   32 /
c      data imach( 6) /    4 /
c      data imach( 7) /    2 /
c      data imach( 8) /   31 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /    2 /
c      data imach(11) /   24 /
c      data imach(12) / -128 /
c      data imach(13) /  127 /
c      data imach(14) /   53 /
c      data imach(15) /-1024 /
c      data imach(16) / 1023 /
c
c  CONVEX C-120 (native mode) without -R8 option
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    0 /
c      data imach( 4) /    6 /
c      data imach( 5) /   32 /
c      data imach( 6) /    4 /
c      data imach( 7) /    2 /
c      data imach( 8) /   31 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /    2 /
c      data imach(11) /   24 /
c      data imach(12) / -127 /
c      data imach(13) /  127 /
c      data imach(14) /   53 /
c      data imach(15) / -1023 /
c      data imach(16) /  1023 /
c
c  CONVEX C-120 (native mode) with -R8 option
c
c      data imach( 1) /     5 /
c      data imach( 2) /     6 /
c      data imach( 3) /     0 /
c      data imach( 4) /     6 /
c      data imach( 5) /    32 /
c      data imach( 6) /     4 /
c      data imach( 7) /     2 /
c      data imach( 8) /    31 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /     2 /
c      data imach(11) /    53 /
c      data imach(12) / -1023 /
c      data imach(13) /  1023 /
c      data imach(14) /    53 /
c      data imach(15) / -1023 /
c      data imach(16) /  1023 /
c
c  CONVEX C-120 (IEEE mode) without -R8 option
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    0 /
c      data imach( 4) /    6 /
c      data imach( 5) /   32 /
c      data imach( 6) /    4 /
c      data imach( 7) /    2 /
c      data imach( 8) /   31 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /    2 /
c      data imach(11) /   24 /
c      data imach(12) / -125 /
c      data imach(13) /  128 /
c      data imach(14) /   53 /
c      data imach(15) / -1021 /
c      data imach(16) /  1024 /
c
c  CONVEX C-120 (IEEE mode) with -R8 option
c
c      data imach( 1) /     5 /
c      data imach( 2) /     6 /
c      data imach( 3) /     0 /
c      data imach( 4) /     6 /
c      data imach( 5) /    32 /
c      data imach( 6) /     4 /
c      data imach( 7) /     2 /
c      data imach( 8) /    31 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /     2 /
c      data imach(11) /    53 /
c      data imach(12) / -1021 /
c      data imach(13) /  1024 /
c      data imach(14) /    53 /
c      data imach(15) / -1021 /
c      data imach(16) /  1024 /
c
c  CRAY 1, 2, XMP and YMP.
c
c      data imach( 1) /     5 /
c      data imach( 2) /     6 /
c      data imach( 3) /   102 /
c      data imach( 4) /     6 /
c      data imach( 5) /    64 /
c      data imach( 6) /     8 /
c      data imach( 7) /     2 /
c      data imach( 8) /    63 /
c      data imach( 9) /  777777777777777777777B /
c      data imach(10) /     2 /
c      data imach(11) /    47 /
c      data imach(12) / -8189 /
c      data imach(13) /  8190 /
c      data imach(14) /    94 /
c      data imach(15) / -8099 /
c      data imach(16) /  8190 /
c
c  DATA GENERAL ECLIPSE S/200.
c
c      data imach( 1) /   11 /
c      data imach( 2) /   12 /
c      data imach( 3) /    8 /
c      data imach( 4) /   10 /
c      data imach( 5) /   16 /
c      data imach( 6) /    2 /
c      data imach( 7) /    2 /
c      data imach( 8) /   15 /
c      data imach( 9) /32767 /
c      data imach(10) /   16 /
c      data imach(11) /    6 /
c      data imach(12) /  -64 /
c      data imach(13) /   63 /
c      data imach(14) /   14 /
c      data imach(15) /  -64 /
c      data imach(16) /   63 /
c
c  ELXSI 6400
c
c      data imach( 1) /     5 /
c      data imach( 2) /     6 /
c      data imach( 3) /     6 /
c      data imach( 4) /     6 /
c      data imach( 5) /    32 /
c      data imach( 6) /     4 /
c      data imach( 7) /     2 /
c      data imach( 8) /    32 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /     2 /
c      data imach(11) /    24 /
c      data imach(12) /  -126 /
c      data imach(13) /   127 /
c      data imach(14) /    53 /
c      data imach(15) / -1022 /
c      data imach(16) /  1023 /
c
c  HARRIS 220
c
c      data imach( 1) /       5 /
c      data imach( 2) /       6 /
c      data imach( 3) /       0 /
c      data imach( 4) /       6 /
c      data imach( 5) /      24 /
c      data imach( 6) /       3 /
c      data imach( 7) /       2 /
c      data imach( 8) /      23 /
c      data imach( 9) / 8388607 /
c      data imach(10) /       2 /
c      data imach(11) /      23 /
c      data imach(12) /    -127 /
c      data imach(13) /     127 /
c      data imach(14) /      38 /
c      data imach(15) /    -127 /
c      data imach(16) /     127 /
c
c  HARRIS SLASH 6 and SLASH 7.
c
c      data imach( 1) /       5 /
c      data imach( 2) /       6 /
c      data imach( 3) /       0 /
c      data imach( 4) /       6 /
c      data imach( 5) /      24 /
c      data imach( 6) /       3 /
c      data imach( 7) /       2 /
c      data imach( 8) /      23 /
c      data imach( 9) / 8388607 /
c      data imach(10) /       2 /
c      data imach(11) /      23 /
c      data imach(12) /    -127 /
c      data imach(13) /     127 /
c      data imach(14) /      38 /
c      data imach(15) /    -127 /
c      data imach(16) /     127 /
c
c  HONEYWELL DPS 8/70 and 600/6000 series.
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /   43 /
c      data imach( 4) /    6 /
c      data imach( 5) /   36 /
c      data imach( 6) /    4 /
c      data imach( 7) /    2 /
c      data imach( 8) /   35 /
c      data imach( 9) / O377777777777 /
c      data imach(10) /    2 /
c      data imach(11) /   27 /
c      data imach(12) / -127 /
c      data imach(13) /  127 /
c      data imach(14) /   63 /
c      data imach(15) / -127 /
c      data imach(16) /  127 /
c
c  HP 2100, 3 word double precision option with FTN4
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    4 /
c      data imach( 4) /    1 /
c      data imach( 5) /   16 /
c      data imach( 6) /    2 /
c      data imach( 7) /    2 /
c      data imach( 8) /   15 /
c      data imach( 9) / 32767 /
c      data imach(10) /    2 /
c      data imach(11) /   23 /
c      data imach(12) / -128 /
c      data imach(13) /  127 /
c      data imach(14) /   39 /
c      data imach(15) / -128 /
c      data imach(16) /  127 /
c
c  HP 2100, 4 word double precision option with FTN4
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    4 /
c      data imach( 4) /    1 /
c      data imach( 5) /   16 /
c      data imach( 6) /    2 /
c      data imach( 7) /    2 /
c      data imach( 8) /   15 /
c      data imach( 9) / 32767 /
c      data imach(10) /    2 /
c      data imach(11) /   23 /
c      data imach(12) / -128 /
c      data imach(13) /  127 /
c      data imach(14) /   55 /
c      data imach(15) / -128 /
c      data imach(16) /  127 /
c
c  HP 9000
c
c      data imach( 1) /     5 /
c      data imach( 2) /     6 /
c      data imach( 3) /     6 /
c      data imach( 4) /     7 /
c      data imach( 5) /    32 /
c      data imach( 6) /     4 /
c      data imach( 7) /     2 /
c      data imach( 8) /    32 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /     2 /
c      data imach(11) /    24 /
c      data imach(12) /  -126 /
c      data imach(13) /   127 /
c      data imach(14) /    53 /
c      data imach(15) / -1015 /
c      data imach(16) /  1017 /
c
c  IBM 360/370 series, XEROX SIGMA 5/7/9, SEL systems 85/86, PERKIN ELMER 3230,
c  and PERKIN ELMER (INTERDATA) 3230.
c
c      data imach( 1) /   5 /
c      data imach( 2) /   6 /
c      data imach( 3) /   7 /
c      data imach( 4) /   6 /
c      data imach( 5) /  32 /
c      data imach( 6) /   4 /
c      data imach( 7) /   2 /
c      data imach( 8) /  31 /
c      data imach( 9) / Z7FFFFFFF /
c      data imach(10) /  16 /
c      data imach(11) /   6 /
c      data imach(12) / -64 /
c      data imach(13) /  63 /
c      data imach(14) /  14 /
c      data imach(15) / -64 /
c      data imach(16) /  63 /
c
c  IBM PC - Microsoft FORTRAN
c
c      data imach( 1) /     5 /
c      data imach( 2) /     6 /
c      data imach( 3) /     6 /
c      data imach( 4) /     0 /
c      data imach( 5) /    32 /
c      data imach( 6) /     4 /
c      data imach( 7) /     2 /
c      data imach( 8) /    31 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /     2 /
c      data imach(11) /    24 /
c      data imach(12) /  -126 /
c      data imach(13) /   127 /
c      data imach(14) /    53 /
c      data imach(15) / -1022 /
c      data imach(16) /  1023 /
c
c  IBM PC - Professional FORTRAN and Lahey FORTRAN
c
c      data imach( 1) /     4 /
c      data imach( 2) /     7 /
c      data imach( 3) /     7 /
c      data imach( 4) /     0 /
c      data imach( 5) /    32 /
c      data imach( 6) /     4 /
c      data imach( 7) /     2 /
c      data imach( 8) /    31 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /     2 /
c      data imach(11) /    24 /
c      data imach(12) /  -126 /
c      data imach(13) /   127 /
c      data imach(14) /    53 /
c      data imach(15) / -1022 /
c      data imach(16) /  1023 /
c
c  INTERDATA 8/32 with the UNIX system FORTRAN 77 compiler.
c  For the INTERDATA FORTRAN VII compiler, replace the Z's specifying hex
c  constants with Y's.
c
c      data imach( 1) /   5 /
c      data imach( 2) /   6 /
c      data imach( 3) /   6 /
c      data imach( 4) /   6 /
c      data imach( 5) /  32 /
c      data imach( 6) /   4 /
c      data imach( 7) /   2 /
c      data imach( 8) /  31 /
c      data imach( 9) / Z'7FFFFFFF' /
c      data imach(10) /  16 /
c      data imach(11) /   6 /
c      data imach(12) / -64 /
c      data imach(13) /  62 /
c      data imach(14) /  14 /
c      data imach(15) / -64 /
c      data imach(16) /  62 /
c
c  PDP-10 (KA processor).
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    7 /
c      data imach( 4) /    6 /
c      data imach( 5) /   36 /
c      data imach( 6) /    5 /
c      data imach( 7) /    2 /
c      data imach( 8) /   35 /
c      data imach( 9) / "377777777777 /
c      data imach(10) /    2 /
c      data imach(11) /   27 /
c      data imach(12) / -128 /
c      data imach(13) /  127 /
c      data imach(14) /   54 /
c      data imach(15) / -101 /
c      data imach(16) /  127 /
c
c  PDP-10 (KI processor).
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    7 /
c      data imach( 4) /    6 /
c      data imach( 5) /   36 /
c      data imach( 6) /    5 /
c      data imach( 7) /    2 /
c      data imach( 8) /   35 /
c      data imach( 9) / "377777777777 /
c      data imach(10) /    2 /
c      data imach(11) /   27 /
c      data imach(12) / -128 /
c      data imach(13) /  127 /
c      data imach(14) /   62 /
c      data imach(15) / -128 /
c      data imach(16) /  127 /
c
c  PDP-11 FORTRANS supporting 32-bit integer arithmetic.
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    7 /
c      data imach( 4) /    6 /
c      data imach( 5) /   32 /
c      data imach( 6) /    4 /
c      data imach( 7) /    2 /
c      data imach( 8) /   31 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /    2 /
c      data imach(11) /   24 /
c      data imach(12) / -127 /
c      data imach(13) /  127 /
c      data imach(14) /   56 /
c      data imach(15) / -127 /
c      data imach(16) /  127 /
c
c  PDP-11 FORTRANS supporting 16-bit integer arithmetic.
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    7 /
c      data imach( 4) /    6 /
c      data imach( 5) /   16 /
c      data imach( 6) /    2 /
c      data imach( 7) /    2 /
c      data imach( 8) /   15 /
c      data imach( 9) / 32767 /
c      data imach(10) /    2 /
c      data imach(11) /   24 /
c      data imach(12) / -127 /
c      data imach(13) /  127 /
c      data imach(14) /   56 /
c      data imach(15) / -127 /
c      data imach(16) /  127 /
c
c  PRIME 50 series systems with 32-bit integers and 64V MODE instructions,
c  supplied by Igor Bray.
c
c      data imach( 1) /            1 /
c      data imach( 2) /            1 /
c      data imach( 3) /            2 /
c      data imach( 4) /            1 /
c      data imach( 5) /           32 /
c      data imach( 6) /            4 /
c      data imach( 7) /            2 /
c      data imach( 8) /           31 /
c      data imach( 9) / :17777777777 /
c      data imach(10) /            2 /
c      data imach(11) /           23 /
c      data imach(12) /         -127 /
c      data imach(13) /         +127 /
c      data imach(14) /           47 /
c      data imach(15) /       -32895 /
c      data imach(16) /       +32637 /
c
c  SEQUENT BALANCE 8000.
c
c      data imach( 1) /     0 /
c      data imach( 2) /     0 /
c      data imach( 3) /     7 /
c      data imach( 4) /     0 /
c      data imach( 5) /    32 /
c      data imach( 6) /     1 /
c      data imach( 7) /     2 /
c      data imach( 8) /    31 /
c      data imach( 9) /  2147483647 /
c      data imach(10) /     2 /
c      data imach(11) /    24 /
c      data imach(12) /  -125 /
c      data imach(13) /   128 /
c      data imach(14) /    53 /
c      data imach(15) / -1021 /
c      data imach(16) /  1024 /
c
c  SUN Microsystems UNIX F77 compiler.
c
c      data imach( 1) /     5 /
c      data imach( 2) /     6 /
c      data imach( 3) /     6 /
c      data imach( 4) /     0 /
c      data imach( 5) /    32 /
c      data imach( 6) /     4 /
c      data imach( 7) /     2 /
c      data imach( 8) /    32 /
c      data imach( 9) /2147483647/
c      data imach(10) /     2 /
c      data imach(11) /    24 /
c      data imach(12) /  -126 /
c      data imach(13) /   128 /
c      data imach(14) /    53 /
c      data imach(15) / -1022 /
c      data imach(16) /  1024 /
c
c  SUN 3 (68881 or FPA)
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    6 /
c      data imach( 4) /    0 /
c      data imach( 5) /   32 /
c      data imach( 6) /    4 /
c      data imach( 7) /    2 /
c      data imach( 8) /   31 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /    2 /
c      data imach(11) /   24 /
c      data imach(12) / -125 /
c      data imach(13) /  128 /
c      data imach(14) /   53 /
c      data imach(15) / -1021 /
c      data imach(16) /  1024 /
c
c  UNIVAC 1100 series.
c  Note that the punch unit, I1MACH(3), has been set to 7, which is appropriate
c  for the UNIVAC-FOR system.  If you have the UNIVAC-FTN system, set it to 1
c  instead.
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    7 /
c      data imach( 4) /    6 /
c      data imach( 5) /   36 /
c      data imach( 6) /    6 /
c      data imach( 7) /    2 /
c      data imach( 8) /   35 /
c      data imach( 9) / O377777777777 /
c      data imach(10) /    2 /
c      data imach(11) /   27 /
c      data imach(12) / -128 /
c      data imach(13) /  127 /
c      data imach(14) /   60 /
c      data imach(15) /-1024 /
c      data imach(16) / 1023 /
c
c  VAX.
c
c      data imach( 1) /    5 /
c      data imach( 2) /    6 /
c      data imach( 3) /    7 /
c      data imach( 4) /    6 /
c      data imach( 5) /   32 /
c      data imach( 6) /    4 /
c      data imach( 7) /    2 /
c      data imach( 8) /   31 /
c      data imach( 9) / 2147483647 /
c      data imach(10) /    2 /
c      data imach(11) /   24 /
c      data imach(12) / -127 /
c      data imach(13) /  127 /
c      data imach(14) /   56 /
c      data imach(15) / -127 /
c      data imach(16) /  127 /
c
c  Z80 microprocessor.
c
c      data imach( 1) /    1 /
c      data imach( 2) /    1 /
c      data imach( 3) /    0 /
c      data imach( 4) /    1 /
c      data imach( 5) /   16 /
c      data imach( 6) /    2 /
c      data imach( 7) /    2 /
c      data imach( 8) /   15 /
c      data imach( 9) / 32767 /
c      data imach(10) /    2 /
c      data imach(11) /   24 /
c      data imach(12) / -127 /
c      data imach(13) /  127 /
c      data imach(14) /   56 /
c      data imach(15) / -127 /
c      data imach(16) /  127 /
c
      if(i.lt.1.or.i.gt.16)then
        write(*,*)'I1MACH - Error, I out of bounds:',i
        i1mach=0
        stop
      else
        i1mach=imach(i)
      endif

      return
      end
      function r1mach(i)
c
c***********************************************************************
c
c  R1MACH returns single precision machine constants.
c
c  Assume that single precision numbers are stored with a mantissa of T digits
c  in base B, with an exponent whose value must lie between EMIN and EMAX.  Then
c  for values of I between 1 and 5, R1MACH will return the following values:
c
c    R1MACH(1) = B**(EMIN-1), the smallest positive magnitude.
c    R1MACH(2) = B**EMAX*(1-B**(-T)), the largest magnitude.
c    R1MACH(3) = B**(-T), the smallest relative spacing.
c    R1MACH(4) = B**(1-T), the largest relative spacing.
c    R1MACH(5) = log10(B)
c
c  To alter this function for a particular environment, the desired set of data
c  statements should be activated by removing the C from column 1.
c
c  On rare machines a STATIC statement may need to be added.  But probably more
c  systems prohibit it that require it.
c
c  For IEEE-arithmetic machines (binary standard), the first set of constants
c  below should be appropriate.
c
c  Where possible, octal or hexadecimal constants have been used to specify the
c  constants exactly which has in some cases required the use of EQUIVALENCED
c  integer arrays.  If your compiler uses half-word integers by default
c  (sometimes called INTEGER*2), you may need to change INTEGER to INTEGER*4 or
c  otherwise instruct your compiler to use full-word integers in the next 5
c  declarations.
c
      integer diver(2)
      integer i
      integer large(2)
      integer log10(2)
      real r1mach
      integer right(2)
      real rmach(5)
      integer small(2)
c
      equivalence (rmach(1),small(1))
      equivalence (rmach(2),large(1))
      equivalence (rmach(3),right(1))
      equivalence (rmach(4),diver(1))
      equivalence (rmach(5),log10(1))
c
c  IEEE arithmetic machines, such as the AT&T 3B series, Motorola 68000 based
c  machines such as the SUN 3 and AT&T PC 7300, and 8087 based micros such as
c  the IBM PC and AT&T 6300.
c
       data small(1) /     8388608 /
       data large(1) /  2139095039 /
       data right(1) /   864026624 /
       data diver(1) /   872415232 /
       data log10(1) /  1050288283 /
c
c  ALLIANT FX/8 UNIX Fortran compiler with the -r8 command line option.  This
c  option causes all variables declared with 'REAL' to be of type 'REAL*8' or
c  DOUBLE PRECISION.  This option does not override the 'REAL*4' declarations.
c  These R1MACH numbers below and the coresponding I1MACH are simply the DOUBLE
c  PRECISION or 'REAL*8' numbers.  If you use the -r8 your whole code (and the
c  user libraries you link with, the system libraries are taken care of
c  automagicly) must be compiled with this option.
c
c      data rmach(1) / 2.22507385850721D-308 /
c      data rmach(2) / 1.79769313486231D+308 /
c      data rmach(3) / 1.1101827117665D-16 /
c      data rmach(4) / 2.2203654423533D-16 /
c      data rmach(5) / 3.01029995663981E-1 /
c
c  AMDAHL machines.
c
c      data small(1) /    1048576 /
c      data large(1) / 2147483647 /
c      data right(1) /  990904320 /
c      data diver(1) / 1007681536 /
c      data log10(1) / 1091781651 /
c
c  BURROUGHS 1700 system.
c
c      data rmach(1) / Z400800000 /
c      data rmach(2) / Z5FFFFFFFF /
c      data rmach(3) / Z4E9800000 /
c      data rmach(4) / Z4EA800000 /
c      data rmach(5) / Z500E730E8 /
c
c  BURROUGHS 5700/6700/7700 systems.
c
c      data rmach(1) / O1771000000000000 /
c      data rmach(2) / O0777777777777777 /
c      data rmach(3) / O1311000000000000 /
c      data rmach(4) / O1301000000000000 /
c      data rmach(5) / O1157163034761675 /
c
c  CDC CYBER 170/180 series using NOS
c
c      data rmach(1) / O"00014000000000000000" /
c      data rmach(2) / O"37767777777777777777" /
c      data rmach(3) / O"16404000000000000000" /
c      data rmach(4) / O"16414000000000000000" /
c      data rmach(5) / O"17164642023241175720" /
c
c  CDC CYBER 170/180 series using NOS/VE
c
c      data rmach(1) / Z"3001800000000000" /
c      data rmach(2) / Z"4FFEFFFFFFFFFFFE" /
c      data rmach(3) / Z"3FD2800000000000" /
c      data rmach(4) / Z"3FD3800000000000" /
c      data rmach(5) / Z"3FFF9A209A84FBCF" /
c
c  CDC CYBER 200 series
c
c      data rmach(1) / X'9000400000000000' /
c      data rmach(2) / X'6FFF7FFFFFFFFFFF' /
c      data rmach(3) / X'FFA3400000000000' /
c      data rmach(4) / X'FFA4400000000000' /
c      data rmach(5) / X'FFD04D104D427DE8' /
c
c  CDC 6000/7000 series using FTN4.
c
c      data rmach(1) / 00564000000000000000B /
c      data rmach(2) / 37767777777777777776B /
c      data rmach(3) / 16414000000000000000B /
c      data rmach(4) / 16424000000000000000B /
c      data rmach(5) / 17164642023241175720B /
c
c  CDC 6000/7000 series using FTN5.
c
c      data rmach(1) / O"00564000000000000000" /
c      data rmach(2) / O"37767777777777777776" /
c      data rmach(3) / O"16414000000000000000" /
c      data rmach(4) / O"16424000000000000000" /
c      data rmach(5) / O"17164642023241175720" /
c
c  CONVEX C-1.
c
c      data rmach(1) / '00800000'X /
c      data rmach(2) / '7FFFFFFF'X /
c      data rmach(3) / '34800000'X /
c      data rmach(4) / '35000000'X /
c      data rmach(5) / '3F9A209B'X /
c
c  CONVEX C-120 (native mode) without -R8 option
c
c      data rmach(1) / 2.9387360E-39 /
c      data rmach(2) / 1.7014117E+38 /
c      data rmach(3) / 5.9604645E-08 /
c      data rmach(4) / 1.1920929E-07 /
c      data rmach(5) / 3.0102999E-01 /
c
c  CONVEX C-120 (native mode) with -R8 option
c
c      data rmach(1) / 5.562684646268007D-309 /
c      data rmach(2) / 8.988465674311577D+307 /
c      data rmach(3) / 1.110223024625157D-016 /
c      data rmach(4) / 2.220446049250313D-016 /
c      data rmach(5) / 3.010299956639812D-001 /
c
c  CONVEX C-120 (IEEE mode) without -R8 option
c
c      data rmach(1) / 1.1754945E-38 /
c      data rmach(2) / 3.4028234E+38 /
c      data rmach(3) / 5.9604645E-08 /
c      data rmach(4) / 1.1920929E-07 /
c      data rmach(5) / 3.0102999E-01 /
c
c  CONVEX C-120 (IEEE mode) with -R8 option
c
c      data rmach(1) / 2.225073858507202D-308 /
c      data rmach(2) / 1.797693134862315D+308 /
c      data rmach(3) / 1.110223024625157D-016 /
c      data rmach(4) / 2.220446049250313D-016 /
c      data rmach(5) / 3.010299956639812D-001 /
c
c  CRAY 1, 2, XMP and YMP.
c
c      data rmach(1) / 200034000000000000000B /
c      data rmach(2) / 577767777777777777776B /
c      data rmach(3) / 377224000000000000000B /
c      data rmach(4) / 377234000000000000000B /
c      data rmach(5) / 377774642023241175720B /
c
c  DATA GENERAL ECLIPSE S/200.
c  Note - It may be appropriate to include the line: STATIC RMACH(5)
c
c      data small /20K,0/
c      data large /77777K,177777K/
c      data right /35420K,0/
c      data diver /36020K,0/
c      data log10 /40423K,42023K/
c
c  ELXSI 6400, assuming REAL*4 is the default real type.
c
c      data small(1) / '00800000'X /
c      data large(1) / '7F7FFFFF'X /
c      data right(1) / '33800000'X /
c      data diver(1) / '34000000'X /
c      data log10(1) / '3E9A209B'X /
c
c  HARRIS 220
c
c      data small(1),small(2) / '20000000, '00000201 /
c      data large(1),large(2) / '37777777, '00000177 /
c      data right(1),right(2) / '20000000, '00000352 /
c      data diver(1),diver(2) / '20000000, '00000353 /
c      data log10(1),log10(2) / '23210115, '00000377 /
c
c  HARRIS SLASH 6 and SLASH 7.
c
c      data small(1),small(2) / '20000000, '00000201 /
c      data large(1),large(2) / '37777777, '00000177 /
c      data right(1),right(2) / '20000000, '00000352 /
c      data diver(1),diver(2) / '20000000, '00000353 /
c      data log10(1),log10(2) / '23210115, '00000377 /
c
c  HONEYWELL DPS 8/70 and 600/6000 series.
c
c      data rmach(1) / O402400000000 /
c      data rmach(2) / O376777777777 /
c      data rmach(3) / O714400000000 /
c      data rmach(4) / O716400000000 /
c      data rmach(5) / O776464202324 /
c
c  HP 2100, 3 word double precision with FTN4
c
c      data small(1), small(2) / 40000B,       1 /
c      data large(1), large(2) / 77777B, 177776B /
c      data right(1), right(2) / 40000B,    325B /
c      data diver(1), diver(2) / 40000B,    327B /
c      data log10(1), log10(2) / 46420B,  46777B /
c
c  HP 2100, 4 word double precision with FTN4
c
c      data small(1), small(2) / 40000B,       1 /
c      data large91), large(2) / 77777B, 177776B /
c      data right(1), right(2) / 40000B,    325B /
c      data diver(1), diver(2) / 40000B,    327B /
c      data log10(1), log10(2) / 46420B,  46777B /
c
c  HP 9000
c
c      r1mach(1) = 1.17549435E-38
c      r1mach(2) = 1.70141163E+38
c      r1mach(3) = 5.960464478E-8
c      r1mach(4) = 1.119209290E-7
c      r1mach(5) = 3.01030010E-1
c
c      data small(1) / 00040000000B /
c      data large(1) / 17677777777B /
c      data right(1) / 06340000000B /
c      data diver(1) / 06400000000B /
c      data log10(1) / 07646420233B /
c
c  IBM 360/370 series, XEROX SIGMA 5/7/9, SEL systems 85/86, PERKIN ELMER 3230,
c  and PERKIN ELMER (INTERDATA) 3230.
c
c      data rmach(1) / Z00100000 /
c      data rmach(2) / Z7FFFFFFF /
c      data rmach(3) / Z3B100000 /
c      data rmach(4) / Z3C100000 /
c      data rmach(5) / Z41134413 /
c
c  IBM PC - Microsoft FORTRAN
c
c      data small(1) / #00800000 /
c      data large(1) / #7F7FFFFF /
c      data right(1) / #33800000 /
c      data diver(1) / #34000000 /
c      data log10(1) / #3E9A209A /
c
c  IBM PC - Professional FORTRAN and Lahey FORTRAN
c
c      data small(1)/ Z'00800000' /
c      data large(1)/ Z'7F7FFFFF' /
c      data right(1)/ Z'33800000' /
c      data diver(1)/ Z'34000000' /
c      data log10(1)/ Z'3E9A209A' /
c
c  INTERDATA 8/32 with the UNIX system FORTRAN 77 compiler.
c  For the INTERDATA FORTRAN VII compiler replace the Z'S specifying HEX
c  constants with Y'S.
c
c      data rmach(1) / Z'00100000' /
c      data rmach(2) / Z'7EFFFFFF' /
c      data rmach(3) / Z'3B100000' /
c      data rmach(4) / Z'3C100000' /
c      data rmach(5) / Z'41134413' /
c
c  PDP-10 (KA or KI processor).
c
c      data rmach(1) / "000400000000 /
c      data rmach(2) / "377777777777 /
c      data rmach(3) / "146400000000 /
c      data rmach(4) / "147400000000 /
c      data rmach(5) / "177464202324 /
c
c  PDP-11 FORTRANS supporting 32-bit integers (integer version).
c
c      data small(1) /    8388608 /
c      data large(1) / 2147483647 /
c      data right(1) /  880803840 /
c      data diver(1) /  889192448 /
c      data log10(1) / 1067065499 /
c
c  PDP-11 FORTRANS supporting 32-bit integers (octal version).
c
c      data rmach(1) / O00040000000 /
c      data rmach(2) / O17777777777 /
c      data rmach(3) / O06440000000 /
c      data rmach(4) / O06500000000 /
c      data rmach(5) / O07746420233 /
c
c  PDP-11 FORTRANS supporting 16-bit integers (integer version).
c
c      data small(1),small(2) /   128,     0 /
c      data large(1),large(2) / 32767,    -1 /
c      data right(1),right(2) / 13440,     0 /
c      data diver(1),diver(2) / 13568,     0 /
c      data log10(1),log10(2) / 16282,  8347 /
c
c  PDP-11 FORTRANS supporting 16-bit integers (octal version).
c
c      data small(1),small(2) / O000200, O000000 /
c      data large(1),large(2) / O077777, O177777 /
c      data right(1),right(2) / O032200, O000000 /
c      data diver(1),diver(2) / O032400, O000000 /
c      data log10(1),log10(2) / O037632, O020233 /
c
c  SEQUENT BALANCE 8000.
c
c      data small(1) / $00800000 /
c      data large(1) / $7F7FFFFF /
c      data right(1) / $33800000 /
c      data diver(1) / $34000000 /
c      data log10(1) / $3E9A209B /
c
c  SUN Microsystems UNIX F77 compiler.
c
c      data rmach(1) / 1.17549435E-38 /
c      data rmach(2) / 3.40282347E+38 /
c      data rmach(3) / 5.96016605E-08 /
c      data rmach(4) / 1.19203321E-07 /
c      data rmach(5) / 3.01030010E-01 /
c
c  SUN 3 (68881 or FPA)
c
c      data small(1) / X'00800000' /
c      data large(1) / X'7F7FFFFF' /
c      data right(1) / X'33800000' /
c      data diver(1) / X'34000000' /
c      data log10(1) / X'3E9A209B' /
c
c  UNIVAC 1100 series.
c
c      data rmach(1) / O000400000000 /
c      data rmach(2) / O377777777777 /
c      data rmach(3) / O146400000000 /
c      data rmach(4) / O147400000000 /
c      data rmach(5) / O177464202324 /
c
c  VAX/ULTRIX F77 compiler.
c
c      data small(1) /       128 /
c      data large(1) /    -32769 /
c      data right(1) /     13440 /
c      data diver(1) /     13568 /
c      data log10(1) / 547045274 /
c
c  VAX-11 with FORTRAN IV-PLUS compiler.
c
c      data rmach(1) / Z00000080 /
c      data rmach(2) / ZFFFF7FFF /
c      data rmach(3) / Z00003480 /
c      data rmach(4) / Z00003500 /
c      data rmach(5) / Z209B3F9A /
c
c  VAX/VMS version 2.2.
c
c      data rmach(1) /       '80'X /
c      data rmach(2) / 'FFFF7FFF'X /
c      data rmach(3) /     '3480'X /
c      data rmach(4) /     '3500'X /
c      data rmach(5) / '209B3F9A'X /
c
c  VAX/VMS 11/780
c
c      data small(1) / Z00000080 /
c      data large(1) / ZFFFF7FFF /
c      data right(1) / Z00003480 /
c      data diver(1) / Z00003500 /
c      data log10(1) / Z209B3F9A /
c
c  Z80 microprocessor.
c
c      data small(1), small(2) /     0,    256 /
c      data large(1), large(2) /    -1,   -129 /
c      data right(1), right(2) /     0,  26880 /
c      data diver(1), diver(2) /     0,  27136 /
c      data log10(1), log10(2) /  8347,  32538 /
c
      if(i.lt.1.or.i.gt.5)then
        write(*,*)'R1MACH - Error, I out of bounds=',i
        r1mach=0.0
        stop
      else
        r1mach = rmach(i)
      endif
 
      return
      end
