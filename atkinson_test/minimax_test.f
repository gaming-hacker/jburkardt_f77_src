      program minimax_test

C     TITLE: FIND A NEAR-MINIMAX APPROXIMATION
C
C     THIS PROGRAM CALCULATES A NEAR-MINIMAX APPROXIMATION
C     TO A GIVEN FUNCTION 'FCN' ON THE INTERVAL [-1,1].
C     THE APPROXIMATION IS BASED ON INTERPOLATION AT THE
C     CHEBYSHEV ZEROS ON [-1,1].  THE PROGRAM THEN
C     ESTIMATES THE MAXIMUM ERROR BY CALCULATING THE ERROR
C     AT A LARGE SAMPLE OF EVENLY SPACED POINTS ON [-1,1].
C     THE PROGRAM CAN BE EASILY MODIFIED TO PRINT THE
C     DIVIDED DIFFERENCES AND THE ERRORS.
C
      PARAMETER (MAXN=20)
      PARAMETER (ZERO=0.0, ONE=1.0, P02=.02)
      DIMENSION FX(0:MAXN)
      real minimax_fcn
      dimension X(0:MAXN)
C
C     THE FOLLOWING STATEMENT DEFINES THE FUNCTION TO BE
C     APPROXIMATED. IT CAN ALSO BE SPECIFIED USING A
C     FUNCTION SUBPROGRAM.
C
C     INITIALIZE VARIABLES.
      PI = 4*ATAN(ONE)
      
      do n = 1, 10

        H = PI/(2*(N+1))
C
C     SET UP INTERPOLATION NODES AND CORRESPONDING
C     FUNCTION VALUES.
        DO I=0,N
          X(I) = COS((2*I+1)*H)
          FX(I) = minimax_fcn ( X(I) )
        end do
C
C     CALCULATE THE DIVIDED DIFFERENCES FOR THE
C     INTERPOLATION POLYNOMIAL.
        CALL DIVDIF(N,X,FX)
C
C     CALCULATE THE MAXIMUM INTERPOLATION ERROR AT 101
C     EVENLY SPACED POINTS ON [-1,1].
        ERRMAX = ZERO
        DO I=0,100
          Z = -ONE + P02*I
C       USE NESTED MULTIPLICATION TO EVALUATE THE
C       INTERPOLATION POLYNOMIAL AT Z.
          P = FX(N)
          DO J=N-1,0,-1
            P = FX(J) + (Z - X(J))*P
          end do
          ERROR = minimax_fcn ( Z ) - P
          ERRMAX = MAX(ABS(ERROR),ERRMAX)
        end do
C
C     PRINT MAXIMUM ERROR.
        write ( *, '(a,i2,a,g14.6)' ) 
     &    '  For N = ', n, ' the maximum error is ', errmax

      end do

      STOP 0
      END
      function minimax_fcn ( x )
      real minimax_fcn
      real x

      minimax_fcn = exp ( x )

      return
      end


