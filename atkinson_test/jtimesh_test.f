      program jtimesh_test

C     TITLE: DEMONSTRATION OF COMPUTING X=J*H
C
C     TWO METHODS ARE USED TO COMPUTE X=J*H,
C     USING (3.40) AND (3.41) IN THE TEXT.
C     SEE SECTION 3.3 FOR MORE INFORMATION.
C
      DOUBLE PRECISION DX, DH
      DATA N/100/, H/0.1/
C
C     INITIALIZE.
      XSUM = 0.0
      DH = H
      PRINT *, 'FOLLOWING ARE THE VALUES OF J, X=J*H, AND'
      PRINT *, 'THE ERRORS IN THE COMPUTATION OF X USING'
      PRINT *, '(3.40) AND (3.41), RESPECTIVELY.'
      PRINT *, ' '
C
      DO J=1,N
        XSUM = XSUM + H
        XPROD = J*H
        DX = J*DH
        ERRSUM = real ( DX - XSUM )
        ERRPRD = real ( DX - XPROD )
        PRINT *, J, DX, ERRPRD, ERRSUM
      end do

      STOP 0
      END
