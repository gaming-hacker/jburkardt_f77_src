      program noise_test

C     TITLE: EXAMPLE OF NOISE IN EVALUATING A FUNCTION.
C
C     THIS IS THE PROGRAM IN SECTION 3.1.
C
C     IN THE PROGRAM, THE FUNCTION
C        F(X) = -1 + 3*X - 3*X**2 + X**3
C     IS EVALUATED IN TWO FORMS: NESTED AND DIRECT.
C     THE INTERVAL OF EVALUATION IS
C        [A,B]  = [1-DELTA,1+DELTA],
C     WITH DELTA SPECIFIED IN THE DATA STATEMENT.
C     FOR OTHER COMPUTERS, THIS VALUE MAY HAVE TO BE
C     CHOSEN SMALLER, IN ORDER TO OBTAIN THE DESIRED
C     DEMONSTRATION OF NOISE IN EVALUATING F(X).
C
      DATA N/100/, DELTA/.001/
C
C     INITIALIZATION.
      A = 1.0 - DELTA
      B = 1.0 + DELTA
      H = 2.0*DELTA/N
      PRINT *, 'FOLLOWING ARE THE VALUES OF X, F(X):NESTED'
      PRINT *, 'EVALUATION, AND F(X):DIRECT EVALUATION.'
C
C     BEGIN LOOP FOR EVALUATING FUNCTION ON [A,B].
      DO J=0,N
        X = A + J*H
        FNESTD = -1.0 + X*(3.0 + X*(-3.0 + X))
        FDIRCT = -1.0 + 3.0*X - 3.0*X*X + X*X*X
        PRINT *, X, FNESTD, FDIRCT
      end do

      STOP 0
      END
