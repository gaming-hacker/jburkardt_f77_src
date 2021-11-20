      FUNCTION SUMPRD(A,B,N)
C     ---------------
C
C     THIS CALCULATES THE INNER PRODUCT
C
C                       I=N
C            SUMPRD=  SUM   A(I)*B(I)
C                       I=1
C                    
C     THE PRODUCTS AND SUMS ARE DONE IN DOUBLE
C     PRECISION, AND THE FINAL RESULT IS CONVERTED
C     BACK TO SINGLE PRECISION.
C
      DOUBLE PRECISION DSUM
      DIMENSION A(*), B(*)
C
      DSUM = 0.0D0
      DO I=1,N
        DSUM = DSUM + DBLE(A(I))*DBLE(B(I))
      end do
      SUMPRD = real ( DSUM )
      RETURN
      END
