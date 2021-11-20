C TEST PROGRAM FOR MAXL01
C LAST MODIFIED BY  R.M.SORLI  ON  26 NOVEMBER 1996
C
C     .. Local Scalars ..
      INTEGER P1M,P1N,P2M,P2N
C     ..
C     .. Local Arrays ..
      INTEGER P1(42),P2(55)
C     ..
C     .. External Subroutines ..
      EXTERNAL SOLVE
C     ..
C     .. Data statements ..
C
      DATA P1M,P1N/6,6/
      DATA P1/2,5,4,1,-3,-1,3,2,-1,3,0,5,-2,3,0,4,-7,3,1,-1,-9,1,8,0,4,
     +     2,3,7,5,-2,4,3,0,-5,-5,1,-1,0,1,0,1,0/
      DATA P2M,P2N/5,10/
      DATA P2/2,-1,4,7,-5,12,9,-4,-1,2,5,3,-1,2,0,0,4,-3,8,1,0,5,0,4,7,
     +     1,2,-5,0,0,3,9,1,1,-1,0,3,7,8,5,-1,-7,4,12,2,0,4,-1,0,0,0,4,
     +     5,3,2/

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TOMS449_TEST:'
      write ( *, '(a)' ) '  MAXL01 solves linear programming problems'
      write ( *, '(a)' ) '  that involve variables restricted to the'
      write ( *, '(a)' ) '  values 0 and 1.'
C
C SOLVE PROBLEM 1
C
      write ( *, '(a)' ) ''
      PRINT *,'Solving problem 1 .....'
      CALL SOLVE(P1M,P1N,P1)
C
C SOLVE PROBLEM 2
C
      write ( *, '(a)' ) ''
      PRINT *,'Solving problem 2 .....'
      CALL SOLVE(P2M,P2N,P2)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TOMS449_TEST:'
      write ( *, '(a)' ) '  Normal end of execution.'

      STOP 0
      END
      SUBROUTINE SOLVE(P1M,P1N,P1)
C     .. Scalar Arguments ..
      INTEGER P1M,P1N
C     ..
C     .. Array Arguments ..
      INTEGER P1(*)
C     ..
C     .. Local Scalars ..
      INTEGER I,INC,J,K,M,MO,N,NAT,NEST,NESTEX,NI,NO,NOPT,V
C     ..
C     .. Local Arrays ..
      INTEGER A(30,80),AO(30,80),B(30),B1(30),BC(80),BO(30),C(80),
     +        IND(30),OPTS(20,80),S(80),S1(30),SO(80),T(80),X(80)
C     ..
C     .. External Subroutines ..
      EXTERNAL MAXL01
C     ..
      MO = 30
      NO = 80
      NEST = 20
      M = P1M
      N = P1N
      K = 1
      DO 20 I = 1,M
          DO 10 J = 1,N
              AO(I,J) = P1(K)
              K = K + 1
   10     CONTINUE
          BO(I) = P1(K)
          K = K + 1
   20 CONTINUE
C
      PRINT *,'M=',M,'  N=',N
      PRINT *,'RHS = ', (BO(I),I=1,M)
c
      CALL MAXL01(MO,NO,NEST,M,N,AO,BO,A,B,B1,S1,C,X,S,SO,BC,T,IND,INC,
     +            NESTEX,V,NOPT,OPTS,NI,NAT)
C
      IF (INC.NE.0) GO TO 40
      IF (NESTEX.NE.0) GO TO 50
      WRITE (6,FMT=9000) V
      WRITE (6,FMT=9010) NOPT
      DO 30 I = 1,NOPT
          WRITE (6,FMT=9020) I, (OPTS(I,J),J=1,N)
   30 CONTINUE
      WRITE (6,FMT=9050) NI,NAT
      GO TO 60

   40 CONTINUE
      WRITE (6,FMT=9030)
      GO TO 60

   50 CONTINUE
      WRITE (6,FMT=9040)
C
   60 CONTINUE
C
      RETURN
C
 9000 FORMAT (1X,'MAXIMAL VALUE OF THE OBJECTIVE FUNCTION =',I6)
 9010 FORMAT (1X,'NUMBER OF MAXIMIZING POINTS =',I4)
 9020 FORMAT (1X,I4,' :',20I3)
 9030 FORMAT (1X,'PROBLEM IS INCONSISTENT')
 9040 FORMAT (1X,'ESTIMATED NUMBER OF FEASIBLE SOLUTIONS WAS EXCEEDED')
 9050 FORMAT (1X,'ITERATIONS =',I9,3X,'ACCELERATION TEST =',I6)
      END

