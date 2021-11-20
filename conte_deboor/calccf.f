      SUBROUTINE CALCCF ( XI, C, N )

C****** I N P U T ******
C  XI(1), . . . . XI(N+1) STRICTLY INCREASING SEQUENCE OF BREAKPOINTS.
C  C(1,I), C(2,I), VALUE AND FIRST DERIVATIVE AT XI(I), I=1 ,... ,N+1,
C  OF THE PIECEWISE CUBIC FUNCTION.
C****** O U T P U T ******
C  C(1,I), C(2,1), C(3,I), C(4,I) POLYNOMIAL COEFFICIENTS OF THE FUNC-
C  TION ON THE INTERVAL (XI(I), XI(I+1)) , I=1,...,N .
C
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c
      INTEGER N

      REAL C(4,N+1)
      real DIVDF1
      real DIVDF3
      real DX
      integer i
      real XI(N+1)

      DO I = 1, N
        DX = XI(I+1) - XI(I)
        DIVDF1 = (C(1,I+1) - C(1,I))/DX
        DIVDF3 = C(2,I) + C(2,I+1) - 2.*DIVDF1
        C(3,I) = (DIVDF1 - C(2,I) - DIVDF3)/DX
        C(4,I) = DIVDF3 / (DX*DX)
      end do

      RETURN
      END
