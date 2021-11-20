C     A FORTRAN TRANSLATION OF THE ALGOL PROCEDURE GLOMIN.
C     SEE PROCEDURE GLOMIN, SECTION 6.10, FOR COMMENTS ETC.
C     REAL FUNCTION GLOMIN ( A, B, C, M, MACHEP, E, T, F, X )
C     REAL A,B,C,M,MACHEP,E,T,F,X,SC
C     REAL A0,A2,A3,D0,D1,D2,H,M2,P,Q,QS,R,S,Y,Y0,Y1,Y2,Y3,YB,Z0,Z1,Z2
      DOUBLE PRECISION FUNCTION GLOMIN ( A, B, C, M, MACHEP, E, T, 
     &  F, X, CALLS )
      DOUBLE PRECISION A,B,C,M,MACHEP,E,T,F,X,SC
      DOUBLE PRECISION A0,A2,A3,D0,D1,D2,H,M2,P,Q,QS,R,S,Y,Y0,Y1,Y2,
     &  Y3,YB,Z0,Z1,Z2
      INTEGER K
      INTEGER CALLS
      CALLS = 0
      A0 = B
      X = A0
      A2 = A
      Y0 = F(B)
      CALLS = CALLS + 1
      YB = Y0
      Y2 = F(A)
      CALLS = CALLS + 1
      Y = Y2
      IF (Y0.GE.Y) GO TO 10
      Y = Y0
      GO TO 20
   10 X = A
   20 IF ((M.LE.0.0).OR.(A.GE.B)) GO TO 140
      M2 = 0.5*(1.0 + 16.0*MACHEP)*M
      SC = C
      IF ((SC.LE.A).OR.(SC.GE.B)) SC = 0.5*(A + B)
      Y1 = F(SC)
      CALLS = CALLS + 1
      K = 3
      D0 = A2 - SC
      H = 0.8181818
      IF (Y1.GE.Y) GO TO 30
      X = SC
      Y = Y1
   30 D1 = A2 - A0
      D2 = SC - A0
      Z2 = B - A2
      Z0 = Y2 - Y1
      Z1 = Y2 - Y0
      R = D1*D1*Z0 - D0*D0*Z1
      P = R
      QS = 2.0*(D0*Z1 - D1*Z0)
      Q = QS
      IF ((K.GT.100000).AND.(Y.LT.Y2)) GO TO 50
   40 IF (Q*(R*(YB-Y2)+Z2*Q*((Y2-Y)+T)).GE.Z2*M2*R*(Z2*Q-R)) GO TO 50
      A3 = A2 + R/Q
      Y3 = F(A3)
      CALLS = CALLS + 1
      IF (Y3.GE.Y) GO TO 50
      X = A3
      Y = Y3
C     ASSUME THAT 1611*K DOES NOT OVERFLOW.
   50 K = MOD(1611*K,1048576)
      Q = 1.0
      R = (B-A)*0.00001*FLOAT(K)
      IF (R.LT.Z2) GO TO 40
      R = M2*D0*D1*D2
C
C  Correction
C
C     S = SQRT (((Y2-Y1) + T)/M2)
      S = SQRT (((Y2-Y) + T)/M2)
      H = 0.5*(1.0 + H)
      P = H*(P + 2.0*R*S)
      Q = R + 0.5*QS
      R = -0.5*(D0 + (Z0 + 2.01*E)/(D0*M2))
      IF ((R.GE.S).AND.(D0.GE.0.0)) GO TO 60
      R = A2 + S
      GO TO 70
   60 R = A2 + R
   70 IF (P*Q.LE.0.0) GO TO 80
      A3 = A2 + P/Q
      GO TO 90
   80 A3 = R
   90 IF (A3.LT.R) A3 = R
      IF (A3.LT.B) GO TO 100
      A3 = B
      Y3 = YB
      GO TO 110
  100 Y3 = F(A3)
      CALLS = CALLS + 1
  110 IF (Y3.GE.Y) GO TO 120
      X = A3
      Y = Y3
  120 D0 = A3 - A2
      IF ( A3.LT.R) GO TO 130
      P = 2.0*(Y2 - Y3)/(M*D0)
      IF (ABS(P).GE.(1.0+9.0*MACHEP)*D0) GO TO 130
      IF (0.5*M2*(D0*D0+P*P).LE.(Y2-Y)+(Y3-Y)+2.0*T) GO TO 130
      A3 = 0.5*(A2 + A3)
      H = 0.9*H
      GO TO 90
  130 IF (A3.GE.B) GO TO 140
      A0 = SC
      SC = A2
      A2 = A3
      Y0 = Y1
      Y1 = Y2
      Y2 = Y3
      GO TO 30
  140 GLOMIN = Y
      RETURN
      END

