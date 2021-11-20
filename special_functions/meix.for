        PROGRAM MEIX
C
C       =========================================================
C       Purpose: This program computes the exponential integral 
C                Ei(x) using subroutine EIX
C       Example:
C                  x        Ei(x)
C                -----------------------
C                  0    -.10000000+301
C                  1     .18951178E+01
C                  2     .49542344E+01
C                  3     .99338326E+01
C                  4     .19630874E+02
C                  5     .40185275E+02
C       =========================================================
C
        DOUBLE PRECISION EI,X
        WRITE(*,*)'Please enter x '
        READ(*,*)X
        WRITE(*,*)
        WRITE(*,*)'   x         Ei(x)'
        WRITE(*,*)'------------------------'
        CALL EIX(X,EI)
        WRITE(*,10)X,EI
10      FORMAT(1X,F5.1,E18.8)
        END


        SUBROUTINE EIX(X,EI)
C
C       ============================================
C       Purpose: Compute exponential integral Ei(x)
C       Input :  x  --- Argument of Ei(x)
C       Output:  EI --- Ei(x) ( x > 0 )
C       ============================================
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        IF (X.EQ.0.0) THEN
           EI=-1.0D+300
        ELSE IF (X.LE.40.0) THEN
           EI=1.0D0
           R=1.0D0
           DO 15 K=1,100
              R=R*K*X/(K+1.0D0)**2
              EI=EI+R
              IF (DABS(R/EI).LE.1.0D-15) GO TO 20
15         CONTINUE
20         GA=0.5772156649015328D0
           EI=GA+DLOG(X)+X*EI
        ELSE
           EI=1.0D0
           R=1.0D0
           DO 25 K=1,20
              R=R*K/X
25            EI=EI+R
           EI=DEXP(X)/X*EI
        ENDIF
        RETURN
        END
