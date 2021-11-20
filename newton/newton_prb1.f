C  THIS IS MTH:NEWPR1.FOR AS OF 18 MARCH 1985,
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
      FX(1)=-2.0*(1.0-X(1))+2.0*X(3)*COS(X(2))
      FX(2)=0.5*X(2)-2.0*X(3)*X(1)*SIN(X(2))
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
      FPRIME(1,1)=2.0
      FPRIME(1,2)=-2.0*X(3)*SIN(X(2))
      FPRIME(1,3)=2.0*COS(X(2))
      FPRIME(2,1)=-2.0*X(3)*SIN(X(2))
      FPRIME(2,2)=0.5-2.0*X(3)*X(1)*COS(X(2))
      FPRIME(2,3)=-2.0*X(1)*SIN(X(2))
      RETURN
C
C***********************************************************************
C
      END
