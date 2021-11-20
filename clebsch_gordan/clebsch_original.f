
      write ( *, * ) winej(2,16,14,13,15,15,12,20,12)
      write ( *, * ) winej(2,16,14,12,14,14,12,20,12)
      write ( *, * ) winej(4,32,28,26,30,30,24,40,24)
      stop
      end
C  Clebsch-Gordan coefficients.
C
      FUNCTION COF3J ( XJ1, XJ2, XJ3, XM1, XM2, XM3 )
C
      real cof3j
      real h
      integer ih
      integer j12
      integer j22
      integer j32
      integer m12
      integer m22
      real phase
      real xj1
      real xj2
      real xj3
      real xm1
      real xm2
      real xm3
!
      H = XJ1 - XJ2 - XM3
      IH = nint ( ABS(H) )
      PHASE = (-1)**(IH)

      J12 = 2 * nint ( XJ1 )
      J22 = 2 * nint ( XJ2 )
      J32 = 2 * nint ( XJ3 )

      M12 = 2 * nint ( XM1 )
      M22 = 2 * nint ( XM2 )

      COF3J = PHASE * VCC ( J12, J22, J32, M12, M22 ) 
     &  / sqrt ( 2.0 * XJ3 + 1.0 )

      RETURN
      END
      FUNCTION COF6J ( XJ1, XJ2, XJ3, XJ4, XJ5, XJ6 )
C
      real cof6j
!
      IH = nint ( XJ1 + XJ2 + XJ4 + XJ5 )
      PHASE=(-1)**IH

      J12 = 2 * nint ( XJ1 )
      J22 = 2 * nint ( XJ2 )
      J32 = 2 * nint ( XJ3 )
      J42 = 2 * nint ( XJ4 )
      J52 = 2 * nint ( XJ5 )
      J62 = 2 * nint ( XJ6 )

      COF6J = PHASE * RACAH ( J12, J22, J52, J42, J32, J62 )

      RETURN
      END
      FUNCTION COF9J(XJ1,XJ2,XJ3,XJ4,XJ5,XJ6,XJ7,XJ8,XJ9)
C
      real cof9j
!
      J1 = 2 * nint ( XJ1 )
      J2 = 2 * nint ( XJ2 )
      J3 = 2 * nint ( XJ3 )
      J4 = 2 * nint ( XJ4 )
      J5 = 2 * nint ( XJ5 )
      J6 = 2 * nint ( XJ6 )
      J7 = 2 * nint ( XJ7 )
      J8 = 2 * nint ( XJ8 )
      J9 = 2 * nint ( XJ9 )

      COF9J = WINEJ ( J1, J2, J3, J4, J5, J6, J7, J8, J9 )

      RETURN
      END
      FUNCTION VCC ( JD1, JD2, JD3, MD1, MD2 )
C
      LOGICAL SETUP
      real vcc
!
      COMMON /FACTRL/ FL(322)
      DIMENSION MTRI(9)
      DATA SETUP /.TRUE./
C
      VCC = 0.0

      J1=JD1
      J2=JD2
      J3=JD3

      M1=MD1
      M2=MD2
      M3 = - M1 - M2

      IF ( SETUP ) then
        CALL FACALC(SETUP)
      end if

      I=J1+J2-J3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRI(1)=I1
      I=J1-J2+J3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRI(2)=I1
      I=-J1+J2+J3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRI(3)=I1
      I=J1+M1
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRI(4)=I1
      MTRI(5)=(J1-M1)/2
      I=J2+M2
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRI(6)=I1
      MTRI(7)=(J2-M2)/2
      I=J3+M3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRI(8)=I1
      MTRI(9)=(J3-M3)/2

      DO N = 1, 9
        IF(MTRI(N).LT.0) RETURN
      end do

      MIN4=J3-J2+M1
      IF(MIN4.LT.0)THEN
        KMIN=-MIN4
      ELSE
        KMIN=0
      END IF

      MIN5=J3-J1-M2
      IF(-MIN5.GT.KMIN) then
        KMIN=-MIN5
      end if
      KMIN=KMIN/2

      IF(J2-J3+M1.LT.0)THEN
        KMAX=J1+J2-J3
      ELSE
        KMAX=J1-M1
      END IF

      IF(J2+M2.LT.KMAX)THEN
        KMAX=J2+M2
      ELSE
        KMAX=KMAX/2
      END IF

      MIN1=MTRI(1)-KMIN+1
      MIN2=MTRI(5)-KMIN+1
      MIN3=MTRI(6)-KMIN+1
      MIN4=MIN4/2+KMIN
      MIN5=MIN5/2+KMIN
C
C  SUM SERIES
C
      UK=1.E-10
      S=1.E-10
      NCUT=0
      KMAX=KMAX-KMIN

      DO K = 1, KMAX

        UK=-UK * real ( (MIN1-K) * (MIN2-K)* (MIN3-K) )
     1  / real ( (KMIN+K) * (MIN4+K) * (MIN5+K) )

        IF ( ABS(UK) .GE. 1.E30 ) THEN
          UK=1.E-10*UK
          S=1.E-10*S
          NCUT=NCUT+1
        END IF

        IF ( ABS(UK) .LT. 1.E-20 ) then
          GO TO 165
        end if

        S=S + UK

      end do
C
C  CALCULATE DELTA FUNCTIONS
C
  165 continue

      DELOG = 0.0
      DO N = 1, 9
        DELOG = DELOG + FL ( MTRI(N)+1 )
      end do

      NUM=(J1+J2+J3)/2+2
      DELOG=0.5*(DELOG-FL(NUM))
      ULOG=-FL(KMIN+1)-FL(MIN1)-FL(MIN2)-FL(MIN3)-FL(MIN4+1)-FL(MIN5+1)
      PLOG=DELOG+ULOG

      IF ( PLOG .GE. -80.0 .AND. NCUT .LE. 0 ) THEN
        S=S*1.E+10
        P=EXP(PLOG)
        F3J=P*S
      ELSE
        SIG=SIGN ( 1.0, S )
        S=ABS(S)
        SLOG=ALOG(S)+real(NCUT+1) * ALOG(1.E+10)
        F3J=SIG * EXP(SLOG+PLOG)
      END IF

      VCC = SQRT ( real(J3+1) ) * F3J * PHASEF(KMIN)

      RETURN
      END
      FUNCTION RACAH ( JD1, JD2, LD2, LD1, JD3, LD3 )
C
      DIMENSION MED(12)
      real racah
C
      RACAH=0.0
      J1=JD1
      J2=JD2
      J3=JD3
      L1=LD1
      L2=LD2
      L3=LD3
C
C  ANGULAR MOMENTUM COUPLING TESTS
C
      I=-J1+J2+J3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MED(1)=I1
      I=+J1-J2+J3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MED(2)=I1
      I=+J1+J2-J3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MED(3)=I1
      I=-J1+L2+L3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MED(4)=I1
      I=+J1-L2+L3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MED(5)=I1
      I=+J1+L2-L3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MED(6)=I1
      I=-L1+J2+L3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MED(7)=I1
      I=+L1-J2+L3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MED(8)=I1
      I=+L1+J2-L3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MED(9)=I1
      I=-L1+L2+J3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MED(10)=I1
      I=+L1-L2+J3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MED(11)=I1
      I=+L1+L2-J3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MED(12)=I1

      DO N = 1, 12
        IF ( MED(N) .LT. 0 ) then
          RETURN
        end if
      end do

      F6J=S6J(J1,J2,J3,L1,L2,L3)

      RACAH=PHASEF((J1+J2+L1+L2)/2)*F6J

      RETURN
      END
      FUNCTION S6J(JD1,JD2,JD3,LD1,LD2,LD3)
C
      real s6j
      LOGICAL SETUP
!
      COMMON/FACTRL/ FL(322)
      DIMENSION MA(4),MB(3),MED(12)
      DATA SETUP/.TRUE./
C
      J1=JD1
      J2=JD2
      J3=JD3

      L1=LD1
      L2=LD2
      L3=LD3

      IF ( SETUP ) then
        CALL FACALC(SETUP)
      end if

      MED(1)=(-J1+J2+J3)/2
      MED(2)=(+J1-J2+J3)/2
      MED(3)=(+J1+J2-J3)/2
      MED(4)=(-J1+L2+L3)/2
      MED(5)=(+J1-L2+L3)/2
      MED(6)=(+J1+L2-L3)/2
      MED(7)=(-L1+J2+L3)/2
      MED(8)=(+L1-J2+L3)/2
      MED(9)=(+L1+J2-L3)/2
      MED(10)=(-L1+L2+J3)/2
      MED(11)=(+L1-L2+J3)/2
      MED(12)=(+L1+L2-J3)/2

      MA(1)=MED(1)+MED(2)+MED(3)
      MA(2)=MED(4)+MED(5)+MED(6)
      MA(3)=MED(7)+MED(8)+MED(9)
      MA(4)=MED(10)+MED(11)+MED(12)

      MB(1)=MA(1)+MED(12)
      MB(2)=MA(1)+MED(4)
      MB(3)=MA(1)+MED(8)
C
C  DETERMINE MAXIMUM OF (J1+J2+J3),(J1+L2+L3),(L1+J2+L3),(L1+L2+J3)
C
      MAX=MA(1)
      DO N = 2, 4
        IF ( MAX .LT. MA(N) ) then
          MAX=MA(N)
        end if
      end do
C
C  DETERMINE MINIMUM OF (J1+J2+L1+L2),(J2+J3+L2+L3),(J3+J1+L3+L1)
C
      MIN=MB(1)
      DO N = 2, 3
        IF(MIN.GT.MB(N)) then 
          MIN=MB(N)
        end if
      end do

      KMAX = MIN - MAX
      MINP1 = MIN + 1
      MIN1 = MIN + 1 - MA(1)
      MIN2 = MIN + 1 - MA(2)
      MIN3 = MIN + 1 - MA(3)
      MIN4 = MIN + 1-MA(4)
      MIN5 = MIN + 2
      MIN6 = MB(1)-MIN
      MIN7 = MB(2)-MIN
      MIN8 = MB(3)-MIN
C
C  SUM SERIES
C  CUT OFF SERIES AT 1.E-25
C
      UK = 1.0E-15
      S = 1.0E-15
      DO K=1,KMAX

        UK=-UK*FLOAT((MIN1-K)*(MIN2-K)*(MIN3-K)*(MIN4-K))
     1  /FLOAT((MIN5-K)*(MIN6+K)*(MIN7+K)*(MIN8+K))

        IF(ABS(UK).LE.1.E-25) then
          GO TO 65
        end if

        S=S+UK

      end do

   65 continue

      S = S * 1.E+15
C
C     CALCULATE DELTA FUNCTIONS
C
      DELOG=0.0
      DO N = 1, 12
        DELOG = DELOG + FL ( MED(N)+1 )
      end do

      NUM1=MA(1)+2
      NUM2=MA(2)+2
      NUM3=MA(3)+2
      NUM4=MA(4)+2

      DELOG=DELOG-FL(NUM1)-FL(NUM2)-FL(NUM3)-FL(NUM4)
      DELOG=0.5*DELOG
      ULOG=FL(MIN5)-FL(MIN1)-FL(MIN2)-FL(MIN3)-FL(MIN4)
     1  -FL(MIN6+1)-FL(MIN7+1)-FL(MIN8+1)
      PLOG=DELOG+ULOG

      IF ( PLOG .LT. -64.0 )THEN
        Q=PLOG+64.0
        Q=EXP(Q)
        S6J=Q*S
        IF(ABS(S6J).LE.1.0)THEN
          S6J=0.0
          RETURN
        END IF
        S6J=S6J*EXP(-64.0)
      ELSE
        P=EXP(PLOG)
        S6J=P*S
      END IF

      IF ( mod ( MIN, 2 ) .eq. 1 ) then
        S6J=-S6J
      end if

      RETURN
      END
      FUNCTION WINEJ(JD1,JD2,JD3,JD4,JD5,JD6,JD7,JD8,JD9)
C
      integer kn(6)
      integer kx(6)
      integer MTRIA(18)
      integer nn(6)
      real winej
C
      WINEJ=0.0
      J1=JD1
      J2=JD2
      J3=JD3
      J4=JD4
      J5=JD5
      J6=JD6
      J7=JD7
      J8=JD8
      J9=JD9
C
C  ANGULAR MOMENTUM COUPLING TESTS
C
      I=-J1+J2+J3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(1)=I1
      I=+J1-J2+J3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(2)=I1
      I=+J1+J2-J3
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(3)=I1
      I=-J4+J5+J6
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(4)=I1
      I=+J4-J5+J6
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(5)=I1
      I=+J4+J5-J6
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(6)=I1
      I=-J7+J8+J9
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(7)=I1
      I=+J7-J8+J9
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(8)=I1
      I=+J7+J8-J9
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(9)=I1
      I=-J1+J4+J7
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(10)=I1
      I=+J1-J4+J7
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(11)=I1
      I=+J1+J4-J7
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(12)=I1
      I=-J2+J5+J8
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(13)=I1
      I=+J2-J5+J8
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(14)=I1
      I=+J2+J5-J8
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(15)=I1
      I=-J3+J6+J9
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(16)=I1
      I=+J3-J6+J9
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(17)=I1
      I=+J3+J6-J9
      I1=I/2
      IF(I.NE.2*I1) RETURN

      MTRIA(18)=I1

      DO N=1,18
        IF(MTRIA(N).LT.0) then
          RETURN
        end if
      end do

      KN(1)=MAX(IABS(J2-J6),IABS(J1-J9),IABS(J4-J8))
      KN(2)=MAX(IABS(J2-J7),IABS(J5-J9),IABS(J4-J3))
      KN(3)=MAX(IABS(J6-J7),IABS(J5-J1),IABS(J8-J3))
      KN(4)=MAX(IABS(J6-J1),IABS(J2-J9),IABS(J5-J7))
      KN(5)=MAX(IABS(J2-J4),IABS(J3-J7),IABS(J6-J8))
      KN(6)=MAX(IABS(J3-J5),IABS(J1-J8),IABS(J4-J9))

      KX(1)=MIN(J2+J6,J1+J9,J4+J8)
      KX(2)=MIN(J2+J7,J5+J9,J4+J3)
      KX(3)=MIN(J6+J7,J5+J1,J8+J3)
      KX(4)=MIN(J1+J6,J2+J9,J5+J7)
      KX(5)=MIN(J2+J4,J3+J7,J6+J8)
      KX(6)=MIN(J3+J5,J1+J8,J4+J9)

      DO K = 1, 6
        NN(K) = KX(K)-KN(K)
      end do

      KSIGN=1

      I=MIN(NN(1),NN(2),NN(3),NN(4),NN(5),NN(6))

      DO K = 1, 6
        IF(I.EQ.NN(K)) then
          GO TO 50
        end if
      end do

      K=6

   50 continue

      KMIN=KN(K)+1
      KMAX=KX(K)+1

      if ( k .eq. 1 ) then
        go to 130
      else if ( k .eq. 2 ) then
        J=J1
        J1=J5
        J5=J
        J=J3
        J3=J8
        J8=J
        J=J6
        J6=J7
        J7=J
        GO TO 130
      else if ( k .eq. 3 ) then
        J=J2
        J2=J7
        J7=J
        J=J3
        J3=J4
        J4=J
        J=J5
        J5=J9
        J9=J
        GO TO 130
      else if ( k .eq. 4 ) then
        J=J1
        J1=J2
        J2=J
        J=J4
        J4=J5
        J5=J
        J=J7
        J7=J8
        J8=J
        GO TO 120
      else if ( k .eq. 5 ) then
        J=J1
        J1=J3
        J3=J
        J=J4
        J4=J6
        J6=J
        J=J7
        J7=J9
        J9=J
        GO TO 120
      else if ( k .eq. 6 ) then
        J=J2
        J2=J3
        J3=J
        J=J5
        J5=J6
        J6=J
        J=J8
        J8=J9
        J9=J
      end if

  120 continue

      KSIGN = 1 - MOD ( J1+J2+J3+J4+J5+J6+J7+J8+J9, 4 )
C
C  SUMMATION OF SERIES
C
  130 continue

      SUM=0.0
      SIG=PHASEF(KMIN-1)*KSIGN
      FLK=FLOAT(KMIN)

      DO K = KMIN, KMAX, 2
        TERM = FLK * S6J(J1,J4,J7,J8,J9,K-1) * S6J(J2,J5,J8,J4,K-1,J6)
     1  * S6J(J3,J6,J9,K-1,J1,J2)
        FLK=FLK+2.0
        SUM=SUM+TERM
      end do

      WINEJ = SUM * SIG

      RETURN
      END
      SUBROUTINE FACALC ( SETUP )
C
      LOGICAL SETUP
!
      COMMON/FACTRL/ FL(322)
C
      FL(1) = 0.0
      FL(2) = 0.0
      DO N = 3, 322
        FL(N) = FL(N-1) + LOG ( real (N-1) )
      end do

      SETUP=.FALSE.

      RETURN
      END
      FUNCTION PHASEF(N)
C
      real phasef
!
      PHASEF = real ( 1 - 2*ABS(N-2*(N/2)) )

      RETURN
      END
