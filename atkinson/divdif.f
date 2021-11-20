      SUBROUTINE DIVDIF(N,X,F) 
C
C     INPUT: (1) 'N' DENOTES A POSITIVE INTEGER.
C            (2) 'X' DENOTES A VECTOR OF POINTS X(0),...,X(N).
C            (3) 'F' DENOTES A VECTOR OF VALUES OF SOME FUNCTION
C                EVALUATED AT THE NODES IN X.
C     OUTPUT: THE VECTOR F IS CONVERTED TO A VECTOR OF DIVIDED
C                DIFFERENCES: F(I)=F[X(0),...,X(I)], I=0,...,N
C
C     WARNING: NOTE THE FORM OF THE DIMENSION STATEMENT.
c
c  Reference:
c
c    Kendall Atkinson, Weimin Han,
c    Elementary Numerical Analysis,
c    Wiley, 2004,
c    ISBN: 0471433373,
c    LC: QA297.A83.2004.
c
      DIMENSION X(0:*), F(0:*)
C
      DO 1 I=1,N
      DO 1 J=N,I,-1
1       F(J) = (F(J) - F(J-1))/(X(J) - X(J-I))
      RETURN
      END
