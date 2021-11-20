      PROGRAM main

c*********************************************************************72
c
cc MAIN is the main program for TOMS599_PRB.
c
c  Discussion:
c
c    TOMS599_PRB tests the TOMS599 library.
c
      REAL MU                                                           MAN  180
      DIMENSION VPAR4(22),VPAR5(24),SAMPLE(10000),II(5)                 MAN  190
C                                                                       MAN  200
C     VPAR4 - VECTOR OF PARAMETER VALUES FOR CASE 4 : SGAMMA            MAN  210
C                                                                       MAN  220
      DATA VPAR4 /.0001,.25,.5,.75,.9999,1.,1.5,2.,                     MAN  230
     ,3.,4.,5.,7.,10.,15.,20.,30.,50.,100.,1000.,                       MAN  240
     ,10000.,100000.,1000000./                                          MAN  250
C                                                                       MAN  260
C     VPAR5 - VECTOR OF PARAMETER VALUES FOR CASE 5 : KPOISS            MAN  270
C                                                                       MAN  280
      DATA VPAR5 /.0001,1.,2.,5.,9.99,10.,                              MAN  290
     ,12.,15.,20.,25.,30.,40.,50.,75.,100.,150.,                        MAN  300
     ,200.,500.,1000.,2000.,5000.,1.E4,1.E5,1.E6/                       MAN  310
C                                                                       MAN  320
C     II - NUMBER OF RUNS FOR EACH DISTRIBUTION                         MAN  330
C                                                                       MAN  340
      DATA II /1,1,1,22,24/                                             MAN  350
C                                                                       MAN  360
C     FORMAT STATEMENTS                                                 MAN  370
C                                                                       MAN  380
   1  FORMAT(' ',/,' ',/,' LISTING OF TRIAL RUNS',                      MAN  390
     ,' FOR RANDOM NUMBER PACKAGE AHRENS/DIETER/KOHRT',                 MAN  400
C    ,/,' ===============================',                             MAN  410
     ,'====================================')                           MAN  420
   2  FORMAT('   FIRST 100 SAMPLES:',/,                                 MAN  430
     ,'   ..................',/,' ',/,(5E15.6))                         MAN  440
   3  FORMAT(' ',/,' ',/,'   TEST DATA:',                               MAN  450
     ,'     (]BASED ON 10000 SAMPLES])',                                MAN  460
     ,/,'   ..........',/,20X,'MEAN',11X,                               MAN  470
     ,'STD.DEV.',7X,'SKEWNESS',                                         MAN  480
     ,7X,'EXCESS',/,' ',/,                                              MAN  490
     ,'   TRUE VALUES:',4E15.6,/,                                       MAN  500
     ,'   SAMPLE DATA:',4E15.6,/,' ')                                   MAN  510
   4  FORMAT(' ',/,' 1.)   0,1 -UNIFORM DISTRIBUTION:',                 MAN  520
     ,/,' ********************************')                            MAN  530
   5  FORMAT(' ',/,' 2.)  (STANDARD-) EXPONENTIAL DISTRIBUTION:',       MAN  540
     ,/,' ******************************************')                  MAN  550
   6  FORMAT(' ',/,' 3.)  (STANDARD-) NORMAL DISTRIBUTION:',            MAN  560
     ,/,' ******************************************')                  MAN  570
   7  FORMAT(' ',/,' 4.)  (STANDARD-) GAMMA-(A) DISTRIBUTION:',         MAN  580
     ,/,' ****************************************')                    MAN  590
   8  FORMAT(' ',/,' 5.)  POISSON-(MU) DISTRIBUTION:',                  MAN  600
     ,/,' *******************************',/,' ',                       MAN  610
     ,/,'   (INTEGER SAMPLES ARE DISPLAYED AS REALS])',                 MAN  620
     ,/,' ',/,' ')                                                      MAN  630
   9  FORMAT(43X,'    GAMMA-(A):  A =',E13.6,                           MAN  640
     ,/,43X,'    ----------------------------')                         MAN  650
  10  FORMAT(43X,'POISSON-(MU):  MU =',E13.6,                           MAN  660
     ,/,43X,'--------------------------------')                         MAN  670
  11  FORMAT(' ',/,' ')                                                 MAN  680
C                                                                       MAN  730
C     OUTPUT: MAIN HEADING                                              MAN  740
C                                                                       MAN  750
      WRITE ( *,1)                                                      MAN  760
C                                                                       MAN  770
C     TRIAL RUNS FOR 5 DIFFERENT CASES:                                 MAN  780
C                                                                       MAN  790
C       NDIS=1 :   0,1 -UNIFORM DISTRIBUTION                            MAN  800
C       NDIS=2 :  (STANDARD-) EXPONENTIAL DISTRIBUTION                  MAN  810
C       NDIS=3 :  (STANDARD-) NORMAL DISTRIBUTION                       MAN  820
C       NDIS=4 :  (STANDARD-) GAMMA-(A) DISTRIBUTION                    MAN  830
C       NDIS=5 :  POISSON-(MU) DISTRIBUTION                             MAN  840
C                                                                       MAN  850
      DO 27 NDIS=1,5                                                    MAN  860
      NRUN=II(NDIS)                                                     MAN  870
C                                                                       MAN  880
C     OUTPUT: CASE HEADING                                              MAN  890
C                                                                       MAN  900
      WRITE ( *,11)                                                     MAN  910
      IF (NDIS .EQ. 1) WRITE (*,4)                                      MAN  920
      IF (NDIS .EQ. 2) WRITE (*,5)                                      MAN  930
      IF (NDIS .EQ. 3) WRITE (*,6)                                      MAN  940
      IF (NDIS .EQ. 4) WRITE (*,7)                                      MAN  950
      IF (NDIS .EQ. 5) WRITE (*,8)                                      MAN  960
C                                                                       MAN  970
C     EACH CASE: ONE RUN FOR EVERY PARAMETER VALUE                      MAN  980
C                                                                       MAN  990
      DO 26 NPAR=1,NRUN                                                 MAN 1000
C                                                                       MAN 1010
C     CASE 4 AND 5: SET PARAMETER VALUES ACCORDING TO DATA VECTOR       MAN 1020
C                                                                       MAN 1030
      IF (NDIS .EQ. 4) A=VPAR4(NPAR)                                    MAN 1040
      IF (NDIS .EQ. 5) MU=VPAR5(NPAR)                                   MAN 1050
C                                                                       MAN 1060
C     SEED FOR UNIFORM RANDOM NUMBER GENERATOR IS INITIALIZED TO 4*0+1  MAN 1070
C                                                                       MAN 1080
      IR=1                                                              MAN 1090
C                                                                       MAN 1100
C     EACH CASE SEPARATELY: SAMPLING AND TEST DATA                      MAN 1110
C        T2 - STANDARD DEVIATION (=SQRT(VARIANCE)),                     MAN 1120
C        T1 - MEAN,   T3 - SKEWNESS,   T4 - EXCESS.                     MAN 1130
C                                                                       MAN 1140
      GO TO (12,14,16,18,20) , NDIS                                     MAN 1150
C                                                                       MAN 1160
C     CASE 1 :   0,1 -UNIFORM DISTRIBUTION                              MAN 1170
C                                                                       MAN 1180
  12  DO 13 I=1,10000                                                   MAN 1190
  13  SAMPLE(I)=SUNIF(IR)                                               MAN 1200
      T1=0.5                                                            MAN 1210
      T2=1.0/SQRT(12.0)                                                 MAN 1220
      T3=0.0                                                            MAN 1230
      T4=-1.2                                                           MAN 1240
      GO TO 23                                                          MAN 1250
C                                                                       MAN 1260
C     (STANDARD-) EXPONENTIAL DISTRIBUTION                              MAN 1270
C                                                                       MAN 1280
  14  DO 15 I=1,10000                                                   MAN 1290
  15  SAMPLE(I)=SEXPO(IR)                                               MAN 1300
      T1=1.0                                                            MAN 1310
      T2=1.0                                                            MAN 1320
      T3=2.0                                                            MAN 1330
      T4=6.0                                                            MAN 1340
      GO TO 23                                                          MAN 1350
C                                                                       MAN 1360
C     (STANDARD-) NORMAL DISTRIBUTION                                   MAN 1370
C                                                                       MAN 1380
  16  DO 17 I=1,10000                                                   MAN 1390
  17  SAMPLE(I)=SNORM(IR)                                               MAN 1400
      T1=0.0                                                            MAN 1410
      T2=1.0                                                            MAN 1420
      T3=0.0                                                            MAN 1430
      T4=0.0                                                            MAN 1440
      GO TO 23                                                          MAN 1450
C                                                                       MAN 1460
C     (STANDARD-) GAMMA-(A) DISTRIBUTION                                MAN 1470
C                                                                       MAN 1480
  18  DO 19 I=1,10000                                                   MAN 1490
  19  SAMPLE(I)=SGAMMA(IR,A)                                            MAN 1500
      T1=A                                                              MAN 1510
      T2=SQRT(A)                                                        MAN 1520
      T3=2.0/T2                                                         MAN 1530
      T4=6.0/A                                                          MAN 1540
      GO TO 22                                                          MAN 1550
C                                                                       MAN 1560
C     POISSON-(MU) DISTRIBUTION                                         MAN 1570
C                                                                       MAN 1580
  20  DO 21 I=1,10000                                                   MAN 1590
  21  SAMPLE(I)=FLOAT(KPOISS(IR,MU))                                    MAN 1600
      T1=MU                                                             MAN 1610
      T2=SQRT(MU)                                                       MAN 1620
      T3=1.0/T2                                                         MAN 1630
      T4=1.0/MU                                                         MAN 1640
C                                                                       MAN 1650
C     CASE 4 AND 5:  OUTPUT: PARAMETER VALUE                            MAN 1660
C                                                                       MAN 1670
  22  IF (NPAR .NE. 1) WRITE (*,11)
      IF (NDIS .EQ. 4) WRITE (*, 9) A
      IF (NDIS .EQ. 5) WRITE (*,10) MU
C                                                                       MAN 1710
  23  CONTINUE                                                          MAN 1720
C                                                                       MAN 1730
C     OUTPUT : FIRST 100 RANDOM DEVIATES FOR EACH RUN                   MAN 1740
C              (INTEGER SAMPLES ARE DISPLAYED AS REALS])                MAN 1750
C                                                                       MAN 1760
      IF (NDIS .LE. 3) WRITE (*,11)
      WRITE (*,2) (SAMPLE(I),I=1,100)
C                                                                       MAN 1790
C     EVALUATION OF SAMPLE MEAN:    E1 - (1/N)*SUM(SAMPLE(I))           MAN 1800
C                                                                       MAN 1810
      S1=0.0                                                            MAN 1820
      DO 24 I=1,10000                                                   MAN 1830
  24  S1=S1+SAMPLE(I)                                                   MAN 1840
      E1=S1/10000.0                                                     MAN 1850
C                                                                       MAN 1860
C     EVALUATION OF FURTHER SAMPLE ESTIMATES :                          MAN 1870
C                                                                       MAN 1880
C     SK       - (1/N)*SUM((SAMPLE(I)-E1)**K)                           MAN 1890
C                SAMPLE CENTRAL MOMENTS  (K=2,3,4)                      MAN 1900
C                WITH RESPECT TO SAMPLE MEAN                            MAN 1910
C                                                                       MAN 1920
C     E2       - SQRT(S2)                                               MAN 1930
C                SAMPLE STANDARD DEVIATION                              MAN 1940
C                (=SQRT(SAMPLE VARIANCE))                               MAN 1950
C                                                                       MAN 1960
C     E3       - S3/S2**(3/2)                                           MAN 1970
C                SAMPLE SKEWNESS                                        MAN 1980
C                                                                       MAN 1990
C     E4       - S4/S2**2-3                                             MAN 2000
C                SAMPLE EXCESS                                          MAN 2010
C                                                                       MAN 2020
      S2=0.0                                                            MAN 2030
      S3=0.0                                                            MAN 2040
      S4=0.0                                                            MAN 2050
C                                                                       MAN 2060
      DO 25 I=1,10000                                                   MAN 2070
C                                                                       MAN 2080
      X1=SAMPLE(I)-E1                                                   MAN 2090
      X2=X1*X1                                                          MAN 2100
      X3=X2*X1                                                          MAN 2110
      X4=X2*X2                                                          MAN 2120
C                                                                       MAN 2130
      S2=S2+X2                                                          MAN 2140
      S3=S3+X3                                                          MAN 2150
      S4=S4+X4                                                          MAN 2160
C                                                                       MAN 2170
  25  CONTINUE                                                          MAN 2180
C                                                                       MAN 2190
      S2=S2/10000.0                                                     MAN 2200
      S3=S3/10000.0                                                     MAN 2210
      S4=S4/10000.0                                                     MAN 2220
C                                                                       MAN 2230
      E2=SQRT(S2)                                                       MAN 2240
      E3=S3/SQRT(S2*S2*S2)                                              MAN 2250
      E4=S4/(S2*S2)-3.0                                                 MAN 2260
C                                                                       MAN 2270
C     END OF EVALUATION                                                 MAN 2280
C     OUTPUT: CHARACTERISTIC DATA                                       MAN 2290
C                                                                       MAN 2300
      WRITE (*,3) T1,T2,T3,T4,E1,E2,E3,E4
C                                                                       MAN 2320
C     END OF PARAMETER LOOP                                             MAN 2330
C                                                                       MAN 2340
  26  CONTINUE                                                          MAN 2350
C                                                                       MAN 2360
C     END OF DISTRIBUTION LOOP                                          MAN 2370
C                                                                       MAN 2380
  27  CONTINUE                                                          MAN 2390
C                                                                       MAN 2400
C     END OF PROGRAM                                                    MAN 2410
C                                                                       MAN 2420
      STOP                                                              MAN 2430
      END                                                               MAN 2440
