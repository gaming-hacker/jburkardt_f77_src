C  THIS IS MTH:NEWPR2.FOR AS OF 21 FEBRUARY 1985,
C  A SAMPLE PROBLEM FOR MTH:NEWTON,
C  CONTAINING THE FUNCTION AND JACOBIAN ROUTINES.
C
C  THE ROOT OF THE SET OF EQUATIONS IS (1,1)
C
C***********************************************************************
C
      SUBROUTINE FUNK(FX,NEQN,X)
C
C***********************************************************************
C
      DIMENSION FX(NEQN)
      DIMENSION X(NEQN)
      FX(1)=X(1)**2 + X(2)**2 -9.0
      RETURN
C
C***********************************************************************
C
      END
      SUBROUTINE JACOB(FPRIME,NROW,NEQN,X)
C
C***********************************************************************
C
      DIMENSION FPRIME(NROW,NEQN)
      DIMENSION X(NEQN)
      FPRIME(1,1)=2.0*X(1)
      FPRIME(1,2)=2.0*X(2)
      RETURN
C
C***********************************************************************
C
      END
