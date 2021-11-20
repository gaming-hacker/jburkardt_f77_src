      FUNCTION SF(X)
C
C     THIS EVALUATES AN APPROXIMATION TO
C
C                 1         X  1
C         SF(X) = - INTEGRAL   - SIN(T)*DT
C                 X         0  T
C
C     FOR -1 .LE. X .LE. 1.  THE APPROXIMATION IS THE
C     DEGREE 8 TAYLOR POLYNOMIAL FOR SF(X).  ITS
C     MAXIMUM ERROR IS 5.0E-9, PROVIDED THE COMPUTER
C     ARITHMETIC ALLOWS A RELATIVE ERROR OF THAT SIZE.
c
c  Reference:
c
c    Kendall Atkinson, Weimin Han,
c    Elementary Numerical Analysis,
c    Wiley, 2004,
c    ISBN: 0471433373,
c    LC: QA297.A83.2004.
c
      DIMENSION A(0:4)
      PARAMETER(C1=-1.0/18.0,C2=1.0/600.0,
     *          C3=-1.0/35280.0,C4=1.0/3265920.0)
      DATA A/1.0,C1,C2,C3,C4/,
     *     NDEG/4/
C
C     INITIALIZE.
C
      U = X*X
      SF = A(NDEG)
C
C     DO NESTED MULTIPLICATION.
C
      DO 10 I=NDEG-1,0,-1
        SF = A(I) + U*SF
10      CONTINUE
      RETURN
      END
