      subroutine dgemm ( transa, transb, m, n, k, alpha, a, lda, b, 
     &  ldb, beta, c, ldc )

c*********************************************************************72
c
cc DGEMM computes C = alpha * A * B and related operations.
c
c  Discussion:
c
c    DGEMM performs one of the matrix-matrix operations
c
c     C := alpha * op ( A ) * op ( B ) + beta * C,
c
c    where op ( X ) is one of
c
c      op ( X ) = X   or   op ( X ) = X',
c
c    ALPHA and BETA are scalars, and A, B and C are matrices, with op ( A )
c    an M by K matrix, op ( B ) a K by N matrix and C an N by N matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c    
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    Original FORTRAN77 version by Jack Dongarra.
c    This version by John Burkardt.
c
c  Parameters:
c
c    Input, character * 1 TRANSA, specifies the form of op( A ) to be used in
c    the matrix multiplication as follows:
c    'N' or 'n', op ( A ) = A.
c    'T' or 't', op ( A ) = A'.
c    'C' or 'c', op ( A ) = A'.
c
c    Input, character * 1 TRANSB, specifies the form of op ( B ) to be used in
c    the matrix multiplication as follows:
c    'N' or 'n', op ( B ) = B.
c    'T' or 't', op ( B ) = B'.
c    'C' or 'c', op ( B ) = B'.
c
c    Input, integer M, the number of rows of the  matrix op ( A ) and of the  
c    matrix C.  0 <= M.
c
c    Input, integer N, the number  of columns of the matrix op ( B ) and the 
c    number of columns of the matrix C.  0 <= N.
c
c    Input, integer K, the number of columns of the matrix op ( A ) and the 
c    number of rows of the matrix op ( B ).  0 <= K.
c
c    Input, double precision ALPHA, the scalar multiplier 
c    for op ( A ) * op ( B ).
c
c    Input, double precision A(LDA,KA), where:
c    if TRANSA is 'N' or 'n', KA is equal to K, and the leading M by K
c    part of the array contains A;
c    if TRANSA is not 'N' or 'n', then KA is equal to M, and the leading
c    K by M part of the array must contain the matrix A.
c
c    Input, integer LDA, the first dimension of A as declared in the calling 
c    routine.  When TRANSA = 'N' or 'n' then LDA must be at least max ( 1, M ), 
c    otherwise LDA must be at least max ( 1, K ).
c
c    Input, double precision B(LDB,KB), where:
c    if TRANSB is 'N' or 'n', kB is N, and the leading K by N 
c    part of the array contains B;
c    if TRANSB is not 'N' or 'n', then KB is equal to K, and the leading
c    n by k  part of the array must contain the matrix B.
c
c    Input, integer LDB, the first dimension of B as declared in the calling 
c    routine.  When TRANSB = 'N' or 'n' then LDB must be at least max ( 1, K ), 
c    otherwise LDB must be at least max ( 1, N ).
c
c    Input, double precision BETA, the scalar multiplier for C.
c
c    Input/output, double precision C(LDC,N).
c    Before entry, the leading M by N part of this array must contain the 
c    matrix C, except when BETA is zero, in which case C need not be set.
c    On exit, the array C is overwritten by the M by N matrix
c    alpha * op ( A ) * op ( B ) + beta * C.
c
c    Input, integer LDC, the first dimension of C as declared in the calling 
c    routine.  max ( 1, M ) <= LDC.
c
      implicit none

      integer lda
      integer ldb
      integer ldc

      double precision a(lda,*)
      double precision alpha
      double precision b(ldb,*)
      double precision beta
      double precision c(ldc,*)
      integer i
      integer info
      integer j
      integer k
      integer l
      integer m
      integer n
      integer ncola
      integer nrowa
      integer nrowb
      logical nota
      logical notb
      double precision temp
      character * 1 transa
      character * 1 transb
c
c  Set NOTA and NOTB as true if A and B respectively are not
c  transposed and set NROWA, NCOLA and NROWB as the number of rows
c  and columns of A and the number of rows of B respectively.
c
      nota = ( transa == 'N' .or. transa == 'n' )

      if ( nota ) then
        nrowa = m
        ncola = k
      else
        nrowa = k
        ncola = m
      end if

      notb = ( transb == 'N' .or. transb == 'n' )

      if ( notb ) then
        nrowb = k
      else
        nrowb = n
      end if
c
c  Test the input parameters.
c
      info = 0

      if ( transa .ne. 'N' .and. transa .ne. 'n' .and.
     &     transa .ne. 'C' .and. transa .ne. 'c' .and.
     &     transa .ne. 'T' .and. transa .ne. 't' ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DGEMM - Fatal error!'
        write ( *, '(a)' ) '  Input TRANSA had illegal value.'
        stop 1
      end if

      if ( transb .ne. 'N' .and. transb .ne. 'n' .and.
     &     transb .ne. 'C' .and. transb .ne. 'c' .and.
     &     transb .ne. 'T' .and. transb .ne. 't' ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DGEMM - Fatal error!'
        write ( *, '(a)' ) '  Input TRANSB had illegal value.'
        stop 1
      end if

      if ( m .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DGEMM - Fatal error!'
        write ( *, '(a)' ) '  Input M had illegal value.'
        stop 1
      end if

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DGEMM - Fatal error!'
        write ( *, '(a)' ) '  Input N had illegal value.'
        stop 1
      end if

      if ( k .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DGEMM - Fatal error!'
        write ( *, '(a)' ) '  Input K had illegal value.'
        stop 1
      end if

      if ( lda .lt. max ( 1, nrowa ) ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DGEMM - Fatal error!'
        write ( *, '(a)' ) '  Input LDA had illegal value.'
        stop 1
      end if

      if ( ldb .lt. max ( 1, nrowb ) ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DGEMM - Fatal error!'
        write ( *, '(a)' ) '  Input LDB had illegal value.'
        stop 1
      end if

      if ( ldc .lt. max ( 1, m ) ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DGEMM - Fatal error!'
        write ( *, '(a)' ) '  Input LDC had illegal value.'
        stop 1
      end if
c
c  Quick return if possible.
c
      if ( m .eq. 0 ) then
        return
      end if

      if ( n .eq. 0 ) then
        return
      end if

      if ( ( alpha .eq. 0.0D+00 .or. k .eq. 0 ) .and. 
     &   ( beta .eq. 1.0D+00 ) ) then
        return
      end if
c
c  And if alpha is zero.
c
      if ( alpha .eq. 0.0D+00 ) then
        if ( beta .eq. 0.0D+00 ) then
          do j = 1, n
            do i = 1, m
              c(i,j) = 0.0D+00
            end do
          end do
        else
          do j = 1, n
            do i = 1, m
              c(i,j) = beta * c(i,j)
            end do
          end do
        end if
        return
      end if
c
c  Start the operations.
c
      if ( notb ) then
c
c  Form  C := alpha*A*B + beta*C.
c
        if ( nota ) then

          do j = 1, n

            if ( beta .eq. 0.0D+00 ) then
              do i = 1, m
                c(i,j) = 0.0D+00
              end do
            else if ( beta .ne. 1.0D+00 ) then
              do i = 1, m
                c(i,j) = beta * c(i,j)
              end do
            end if

            do l = 1, k
              if ( b(l,j) .ne. 0.0D+00 ) then
                temp = alpha * b(l,j)
                do i = 1, m
                  c(i,j) = c(i,j) + temp * a(i,l)
                end do
              end if
            end do

          end do
c
c  Form  C := alpha*A'*B + beta*C
c
        else

          do j = 1, n
            do i = 1, m

              temp = 0.0D+00
              do l = 1, k
                temp = temp + a(l,i) * b(l,j)
              end do

              if ( beta .eq. 0.0D+00 ) then
                c(i,j) = alpha * temp
              else
                c(i,j) = alpha * temp + beta * c(i,j)
              end if

            end do
          end do

        end if
c
c  Form  C := alpha*A*B' + beta*C
c
      else

        if ( nota ) then

          do j = 1, n

            if ( beta .eq. 0.0D+00 ) then
              do i = 1, m
                c(i,j) = 0.0D+00
              end do
            else if ( beta .ne. 1.0D+00 ) then
              do i = 1, m
                c(i,j) = beta * c(i,j)
              end do
            end if

            do l = 1, k
              if ( b(j,l) .ne. 0.0D+00 ) then
                temp = alpha * b(j,l)
                do i = 1, m
                  c(i,j) = c(i,j) + temp * a(i,l)
                end do
              end if
            end do

          end do
c
c  Form  C := alpha*A'*B' + beta*C
c
        else

          do j = 1, n
            do i = 1, m

              temp = 0.0D+00
              do l = 1, k
                temp = temp + a(l,i) * b(j,l)
              end do
              if ( beta .eq. 0.0D+00 ) then
                c(i,j) = alpha * temp
              else
                c(i,j) = alpha * temp + beta * c(i,j)
              end if
            end do
          end do

        end if

      end if

      return
      end
      SUBROUTINE DSYMM ( SIDE, UPLO, M, N, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
c     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO
      INTEGER            M, N, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
c     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
c     ..
c
c  Purpose
c  =======
c
c  DSYMM  performs one of the matrix-matrix operations
c
c     C := alpha*A*B + beta*C,
c
c  or
c
c     C := alpha*B*A + beta*C,
c
c  where alpha and beta are scalars,  A is a symmetric matrix and  B and
c  C are  m by n matrices.
c
c  Parameters
c  ==========
c
c  SIDE   - CHARACTER*1.
c           On entry,  SIDE  specifies whether  the  symmetric matrix  A
c           appears on the  left or right  in the  operation as follows:
c
c              SIDE = 'L' or 'l'   C := alpha*A*B + beta*C,
c
c              SIDE = 'R' or 'r'   C := alpha*B*A + beta*C,
c
c           Unchanged on exit.
c
c  UPLO   - CHARACTER*1.
c           On  entry,   UPLO  specifies  whether  the  upper  or  lower
c           triangular  part  of  the  symmetric  matrix   A  is  to  be
c           referenced as follows:
c
c              UPLO = 'U' or 'u'   Only the upper triangular part of the
c                                  symmetric matrix is to be referenced.
c
c              UPLO = 'L' or 'l'   Only the lower triangular part of the
c                                  symmetric matrix is to be referenced.
c
c           Unchanged on exit.
c
c  M      - INTEGER.
c           On entry,  M  specifies the number of rows of the matrix  C.
c           M  must be at least zero.
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the number of columns of the matrix C.
c           N  must be at least zero.
c           Unchanged on exit.
c
c  ALPHA  - DOUBLE PRECISION.
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
c           m  when  SIDE = 'L' or 'l'  and is  n otherwise.
c           Before entry  with  SIDE = 'L' or 'l',  the  m by m  part of
c           the array  A  must contain the  symmetric matrix,  such that
c           when  UPLO = 'U' or 'u', the leading m by m upper triangular
c           part of the array  A  must contain the upper triangular part
c           of the  symmetric matrix and the  strictly  lower triangular
c           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',
c           the leading  m by m  lower triangular part  of the  array  A
c           must  contain  the  lower triangular part  of the  symmetric
c           matrix and the  strictly upper triangular part of  A  is not
c           referenced.
c           Before entry  with  SIDE = 'R' or 'r',  the  n by n  part of
c           the array  A  must contain the  symmetric matrix,  such that
c           when  UPLO = 'U' or 'u', the leading n by n upper triangular
c           part of the array  A  must contain the upper triangular part
c           of the  symmetric matrix and the  strictly  lower triangular
c           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',
c           the leading  n by n  lower triangular part  of the  array  A
c           must  contain  the  lower triangular part  of the  symmetric
c           matrix and the  strictly upper triangular part of  A  is not
c           referenced.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
c           LDA must be at least  max( 1, m ), otherwise  LDA must be at
c           least  max( 1, n ).
c           Unchanged on exit.
c
c  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
c           Before entry, the leading  m by n part of the array  B  must
c           contain the matrix B.
c           Unchanged on exit.
c
c  LDB    - INTEGER.
c           On entry, LDB specifies the first dimension of B as declared
c           in  the  calling  (sub)  program.   LDB  must  be  at  least
c           max( 1, m ).
c           Unchanged on exit.
c
c  BETA   - DOUBLE PRECISION.
c           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
c           supplied as zero then C need not be set on input.
c           Unchanged on exit.
c
c  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
c           Before entry, the leading  m by n  part of the array  C must
c           contain the matrix  C,  except when  beta  is zero, in which
c           case C need not be set on entry.
c           On exit, the array  C  is overwritten by the  m by n updated
c           matrix.
c
c  LDC    - INTEGER.
c           On entry, LDC specifies the first dimension of C as declared
c           in  the  calling  (sub)  program.   LDC  must  be  at  least
c           max( 1, m ).
c           Unchanged on exit.
c
c
c  Level 3 Blas routine.
c
c  -- Written on 8-February-1989.
c     Jack Dongarra, Argonne National Laboratory.
c     Iain Duff, AERE Harwell.
c     Jeremy Du Croz, Numerical Algorithms Group Ltd.
c     Sven Hammarling, Numerical Algorithms Group Ltd.
c
c
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          MAX
c     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP1, TEMP2
c     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
c     ..
c     .. Executable Statements ..
c
c     Set NROWA as the number of rows of A.
c
      IF( LSAME( SIDE, 'L' ) )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      UPPER = LSAME( UPLO, 'U' )
c
c     Test the input parameters.
c
      INFO = 0
      IF(      ( .NOT.LSAME( SIDE, 'L' ) ).AND.
     $         ( .NOT.LSAME( SIDE, 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER              ).AND.
     $         ( .NOT.LSAME( UPLO, 'L' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 7
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 9
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 12
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DSYMM ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
c
c     And when  alpha.eq.zero.
c
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
c
c     Start the operations.
c
      IF( LSAME( SIDE, 'L' ) )THEN
c
c        Form  C := alpha*A*B + beta*C.
c
         IF( UPPER )THEN
            DO 70, J = 1, N
               DO 60, I = 1, M
                  TEMP1 = ALPHA*B( I, J )
                  TEMP2 = ZERO
                  DO 50, K = 1, I - 1
                     C( K, J ) = C( K, J ) + TEMP1    *A( K, I )
                     TEMP2     = TEMP2     + B( K, J )*A( K, I )
   50             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = TEMP1*A( I, I ) + ALPHA*TEMP2
                  ELSE
                     C( I, J ) = BETA *C( I, J ) +
     $                           TEMP1*A( I, I ) + ALPHA*TEMP2
                  END IF
   60          CONTINUE
   70       CONTINUE
         ELSE
            DO 100, J = 1, N
               DO 90, I = M, 1, -1
                  TEMP1 = ALPHA*B( I, J )
                  TEMP2 = ZERO
                  DO 80, K = I + 1, M
                     C( K, J ) = C( K, J ) + TEMP1    *A( K, I )
                     TEMP2     = TEMP2     + B( K, J )*A( K, I )
   80             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = TEMP1*A( I, I ) + ALPHA*TEMP2
                  ELSE
                     C( I, J ) = BETA *C( I, J ) +
     $                           TEMP1*A( I, I ) + ALPHA*TEMP2
                  END IF
   90          CONTINUE
  100       CONTINUE
         END IF
      ELSE
c
c        Form  C := alpha*B*A + beta*C.
c
         DO 170, J = 1, N
            TEMP1 = ALPHA*A( J, J )
            IF( BETA.EQ.ZERO )THEN
               DO 110, I = 1, M
                  C( I, J ) = TEMP1*B( I, J )
  110          CONTINUE
            ELSE
               DO 120, I = 1, M
                  C( I, J ) = BETA*C( I, J ) + TEMP1*B( I, J )
  120          CONTINUE
            END IF
            DO 140, K = 1, J - 1
               IF( UPPER )THEN
                  TEMP1 = ALPHA*A( K, J )
               ELSE
                  TEMP1 = ALPHA*A( J, K )
               END IF
               DO 130, I = 1, M
                  C( I, J ) = C( I, J ) + TEMP1*B( I, K )
  130          CONTINUE
  140       CONTINUE
            DO 160, K = J + 1, N
               IF( UPPER )THEN
                  TEMP1 = ALPHA*A( J, K )
               ELSE
                  TEMP1 = ALPHA*A( K, J )
               END IF
               DO 150, I = 1, M
                  C( I, J ) = C( I, J ) + TEMP1*B( I, K )
  150          CONTINUE
  160       CONTINUE
  170    CONTINUE
      END IF
c
      RETURN
c
c     End of DSYMM .
c
      END
      SUBROUTINE DSYR2K( UPLO, TRANS, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
c     .. Scalar Arguments ..
      CHARACTER*1        UPLO, TRANS
      INTEGER            N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
c     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
c     ..
c
c  Purpose
c  =======
c
c  DSYR2K  performs one of the symmetric rank 2k operations
c
c     C := alpha*A*B' + alpha*B*A' + beta*C,
c
c  or
c
c     C := alpha*A'*B + alpha*B'*A + beta*C,
c
c  where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
c  and  A and B  are  n by k  matrices  in the  first  case  and  k by n
c  matrices in the second case.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On  entry,   UPLO  specifies  whether  the  upper  or  lower
c           triangular  part  of the  array  C  is to be  referenced  as
c           follows:
c
c              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
c                                  is to be referenced.
c
c              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
c                                  is to be referenced.
c
c           Unchanged on exit.
c
c  TRANS  - CHARACTER*1.
c           On entry,  TRANS  specifies the operation to be performed as
c           follows:
c
c              TRANS = 'N' or 'n'   C := alpha*A*B' + alpha*B*A' +
c                                        beta*C.
c
c              TRANS = 'T' or 't'   C := alpha*A'*B + alpha*B'*A +
c                                        beta*C.
c
c              TRANS = 'C' or 'c'   C := alpha*A'*B + alpha*B'*A +
c                                        beta*C.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry,  N specifies the order of the matrix C.  N must be
c           at least zero.
c           Unchanged on exit.
c
c  K      - INTEGER.
c           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
c           of  columns  of the  matrices  A and B,  and on  entry  with
c           TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
c           of rows of the matrices  A and B.  K must be at least  zero.
c           Unchanged on exit.
c
c  ALPHA  - DOUBLE PRECISION.
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
c           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
c           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
c           part of the array  A  must contain the matrix  A,  otherwise
c           the leading  k by n  part of the array  A  must contain  the
c           matrix A.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
c           then  LDA must be at least  max( 1, n ), otherwise  LDA must
c           be at least  max( 1, k ).
c           Unchanged on exit.
c
c  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
c           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
c           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
c           part of the array  B  must contain the matrix  B,  otherwise
c           the leading  k by n  part of the array  B  must contain  the
c           matrix B.
c           Unchanged on exit.
c
c  LDB    - INTEGER.
c           On entry, LDB specifies the first dimension of B as declared
c           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
c           then  LDB must be at least  max( 1, n ), otherwise  LDB must
c           be at least  max( 1, k ).
c           Unchanged on exit.
c
c  BETA   - DOUBLE PRECISION.
c           On entry, BETA specifies the scalar beta.
c           Unchanged on exit.
c
c  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
c           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
c           upper triangular part of the array C must contain the upper
c           triangular part  of the  symmetric matrix  and the strictly
c           lower triangular part of C is not referenced.  On exit, the
c           upper triangular part of the array  C is overwritten by the
c           upper triangular part of the updated matrix.
c           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
c           lower triangular part of the array C must contain the lower
c           triangular part  of the  symmetric matrix  and the strictly
c           upper triangular part of C is not referenced.  On exit, the
c           lower triangular part of the array  C is overwritten by the
c           lower triangular part of the updated matrix.
c
c  LDC    - INTEGER.
c           On entry, LDC specifies the first dimension of C as declared
c           in  the  calling  (sub)  program.   LDC  must  be  at  least
c           max( 1, n ).
c           Unchanged on exit.
c
c
c  Level 3 Blas routine.
c
c
c  -- Written on 8-February-1989.
c     Jack Dongarra, Argonne National Laboratory.
c     Iain Duff, AERE Harwell.
c     Jeremy Du Croz, Numerical Algorithms Group Ltd.
c     Sven Hammarling, Numerical Algorithms Group Ltd.
c
c
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          MAX
c     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I, INFO, J, L, NROWA
      DOUBLE PRECISION   TEMP1, TEMP2
c     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      IF( LSAME( TRANS, 'N' ) )THEN
         NROWA = N
      ELSE
         NROWA = K
      END IF
      UPPER = LSAME( UPLO, 'U' )
c
      INFO = 0
      IF(      ( .NOT.UPPER               ).AND.
     $         ( .NOT.LSAME( UPLO , 'L' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.LSAME( TRANS, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANS, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANS, 'C' ) )      )THEN
         INFO = 2
      ELSE IF( N  .LT.0               )THEN
         INFO = 3
      ELSE IF( K  .LT.0               )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 7
      ELSE IF( LDB.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDC.LT.MAX( 1, N     ) )THEN
         INFO = 12
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DSYR2K', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
c
c     And when  alpha.eq.zero.
c
      IF( ALPHA.EQ.ZERO )THEN
         IF( UPPER )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 20, J = 1, N
                  DO 10, I = 1, J
                     C( I, J ) = ZERO
   10             CONTINUE
   20          CONTINUE
            ELSE
               DO 40, J = 1, N
                  DO 30, I = 1, J
                     C( I, J ) = BETA*C( I, J )
   30             CONTINUE
   40          CONTINUE
            END IF
         ELSE
            IF( BETA.EQ.ZERO )THEN
               DO 60, J = 1, N
                  DO 50, I = J, N
                     C( I, J ) = ZERO
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 80, J = 1, N
                  DO 70, I = J, N
                     C( I, J ) = BETA*C( I, J )
   70             CONTINUE
   80          CONTINUE
            END IF
         END IF
         RETURN
      END IF
c
c     Start the operations.
c
      IF( LSAME( TRANS, 'N' ) )THEN
c
c        Form  C := alpha*A*B' + alpha*B*A' + C.
c
         IF( UPPER )THEN
            DO 130, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 90, I = 1, J
                     C( I, J ) = ZERO
   90             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 100, I = 1, J
                     C( I, J ) = BETA*C( I, J )
  100             CONTINUE
               END IF
               DO 120, L = 1, K
                  IF( ( A( J, L ).NE.ZERO ).OR.
     $                ( B( J, L ).NE.ZERO )     )THEN
                     TEMP1 = ALPHA*B( J, L )
                     TEMP2 = ALPHA*A( J, L )
                     DO 110, I = 1, J
                        C( I, J ) = C( I, J ) +
     $                              A( I, L )*TEMP1 + B( I, L )*TEMP2
  110                CONTINUE
                  END IF
  120          CONTINUE
  130       CONTINUE
         ELSE
            DO 180, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 140, I = J, N
                     C( I, J ) = ZERO
  140             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 150, I = J, N
                     C( I, J ) = BETA*C( I, J )
  150             CONTINUE
               END IF
               DO 170, L = 1, K
                  IF( ( A( J, L ).NE.ZERO ).OR.
     $                ( B( J, L ).NE.ZERO )     )THEN
                     TEMP1 = ALPHA*B( J, L )
                     TEMP2 = ALPHA*A( J, L )
                     DO 160, I = J, N
                        C( I, J ) = C( I, J ) +
     $                              A( I, L )*TEMP1 + B( I, L )*TEMP2
  160                CONTINUE
                  END IF
  170          CONTINUE
  180       CONTINUE
         END IF
      ELSE
c
c        Form  C := alpha*A'*B + alpha*B'*A + C.
c
         IF( UPPER )THEN
            DO 210, J = 1, N
               DO 200, I = 1, J
                  TEMP1 = ZERO
                  TEMP2 = ZERO
                  DO 190, L = 1, K
                     TEMP1 = TEMP1 + A( L, I )*B( L, J )
                     TEMP2 = TEMP2 + B( L, I )*A( L, J )
  190             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP1 + ALPHA*TEMP2
                  ELSE
                     C( I, J ) = BETA *C( I, J ) +
     $                           ALPHA*TEMP1 + ALPHA*TEMP2
                  END IF
  200          CONTINUE
  210       CONTINUE
         ELSE
            DO 240, J = 1, N
               DO 230, I = J, N
                  TEMP1 = ZERO
                  TEMP2 = ZERO
                  DO 220, L = 1, K
                     TEMP1 = TEMP1 + A( L, I )*B( L, J )
                     TEMP2 = TEMP2 + B( L, I )*A( L, J )
  220             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP1 + ALPHA*TEMP2
                  ELSE
                     C( I, J ) = BETA *C( I, J ) +
     $                           ALPHA*TEMP1 + ALPHA*TEMP2
                  END IF
  230          CONTINUE
  240       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of DSYR2K.
c
      END
      SUBROUTINE DSYRK ( UPLO, TRANS, N, K, ALPHA, A, LDA,
     $                   BETA, C, LDC )
c     .. Scalar Arguments ..
      CHARACTER*1        UPLO, TRANS
      INTEGER            N, K, LDA, LDC
      DOUBLE PRECISION   ALPHA, BETA
c     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * )
c     ..
c
c  Purpose
c  =======
c
c  DSYRK  performs one of the symmetric rank k operations
c
c     C := alpha*A*A' + beta*C,
c
c  or
c
c     C := alpha*A'*A + beta*C,
c
c  where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
c  and  A  is an  n by k  matrix in the first case and a  k by n  matrix
c  in the second case.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On  entry,   UPLO  specifies  whether  the  upper  or  lower
c           triangular  part  of the  array  C  is to be  referenced  as
c           follows:
c
c              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
c                                  is to be referenced.
c
c              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
c                                  is to be referenced.
c
c           Unchanged on exit.
c
c  TRANS  - CHARACTER*1.
c           On entry,  TRANS  specifies the operation to be performed as
c           follows:
c
c              TRANS = 'N' or 'n'   C := alpha*A*A' + beta*C.
c
c              TRANS = 'T' or 't'   C := alpha*A'*A + beta*C.
c
c              TRANS = 'C' or 'c'   C := alpha*A'*A + beta*C.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry,  N specifies the order of the matrix C.  N must be
c           at least zero.
c           Unchanged on exit.
c
c  K      - INTEGER.
c           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
c           of  columns   of  the   matrix   A,   and  on   entry   with
c           TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
c           of rows of the matrix  A.  K must be at least zero.
c           Unchanged on exit.
c
c  ALPHA  - DOUBLE PRECISION.
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
c           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
c           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
c           part of the array  A  must contain the matrix  A,  otherwise
c           the leading  k by n  part of the array  A  must contain  the
c           matrix A.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
c           then  LDA must be at least  max( 1, n ), otherwise  LDA must
c           be at least  max( 1, k ).
c           Unchanged on exit.
c
c  BETA   - DOUBLE PRECISION.
c           On entry, BETA specifies the scalar beta.
c           Unchanged on exit.
c
c  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
c           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
c           upper triangular part of the array C must contain the upper
c           triangular part  of the  symmetric matrix  and the strictly
c           lower triangular part of C is not referenced.  On exit, the
c           upper triangular part of the array  C is overwritten by the
c           upper triangular part of the updated matrix.
c           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
c           lower triangular part of the array C must contain the lower
c           triangular part  of the  symmetric matrix  and the strictly
c           upper triangular part of C is not referenced.  On exit, the
c           lower triangular part of the array  C is overwritten by the
c           lower triangular part of the updated matrix.
c
c  LDC    - INTEGER.
c           On entry, LDC specifies the first dimension of C as declared
c           in  the  calling  (sub)  program.   LDC  must  be  at  least
c           max( 1, n ).
c           Unchanged on exit.
c
c
c  Level 3 Blas routine.
c
c  -- Written on 8-February-1989.
c     Jack Dongarra, Argonne National Laboratory.
c     Iain Duff, AERE Harwell.
c     Jeremy Du Croz, Numerical Algorithms Group Ltd.
c     Sven Hammarling, Numerical Algorithms Group Ltd.
c
c
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          MAX
c     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I, INFO, J, L, NROWA
      DOUBLE PRECISION   TEMP
c     .. Parameters ..
      DOUBLE PRECISION   ONE ,         ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      IF( LSAME( TRANS, 'N' ) )THEN
         NROWA = N
      ELSE
         NROWA = K
      END IF
      UPPER = LSAME( UPLO, 'U' )
c
      INFO = 0
      IF(      ( .NOT.UPPER               ).AND.
     $         ( .NOT.LSAME( UPLO , 'L' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.LSAME( TRANS, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANS, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANS, 'C' ) )      )THEN
         INFO = 2
      ELSE IF( N  .LT.0               )THEN
         INFO = 3
      ELSE IF( K  .LT.0               )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 7
      ELSE IF( LDC.LT.MAX( 1, N     ) )THEN
         INFO = 10
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DSYRK ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
c
c     And when  alpha.eq.zero.
c
      IF( ALPHA.EQ.ZERO )THEN
         IF( UPPER )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 20, J = 1, N
                  DO 10, I = 1, J
                     C( I, J ) = ZERO
   10             CONTINUE
   20          CONTINUE
            ELSE
               DO 40, J = 1, N
                  DO 30, I = 1, J
                     C( I, J ) = BETA*C( I, J )
   30             CONTINUE
   40          CONTINUE
            END IF
         ELSE
            IF( BETA.EQ.ZERO )THEN
               DO 60, J = 1, N
                  DO 50, I = J, N
                     C( I, J ) = ZERO
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 80, J = 1, N
                  DO 70, I = J, N
                     C( I, J ) = BETA*C( I, J )
   70             CONTINUE
   80          CONTINUE
            END IF
         END IF
         RETURN
      END IF
c
c     Start the operations.
c
      IF( LSAME( TRANS, 'N' ) )THEN
c
c        Form  C := alpha*A*A' + beta*C.
c
         IF( UPPER )THEN
            DO 130, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 90, I = 1, J
                     C( I, J ) = ZERO
   90             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 100, I = 1, J
                     C( I, J ) = BETA*C( I, J )
  100             CONTINUE
               END IF
               DO 120, L = 1, K
                  IF( A( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*A( J, L )
                     DO 110, I = 1, J
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  110                CONTINUE
                  END IF
  120          CONTINUE
  130       CONTINUE
         ELSE
            DO 180, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 140, I = J, N
                     C( I, J ) = ZERO
  140             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 150, I = J, N
                     C( I, J ) = BETA*C( I, J )
  150             CONTINUE
               END IF
               DO 170, L = 1, K
                  IF( A( J, L ).NE.ZERO )THEN
                     TEMP      = ALPHA*A( J, L )
                     DO 160, I = J, N
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  160                CONTINUE
                  END IF
  170          CONTINUE
  180       CONTINUE
         END IF
      ELSE
c
c        Form  C := alpha*A'*A + beta*C.
c
         IF( UPPER )THEN
            DO 210, J = 1, N
               DO 200, I = 1, J
                  TEMP = ZERO
                  DO 190, L = 1, K
                     TEMP = TEMP + A( L, I )*A( L, J )
  190             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  200          CONTINUE
  210       CONTINUE
         ELSE
            DO 240, J = 1, N
               DO 230, I = J, N
                  TEMP = ZERO
                  DO 220, L = 1, K
                     TEMP = TEMP + A( L, I )*A( L, J )
  220             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  230          CONTINUE
  240       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of DSYRK .
c
      END
      SUBROUTINE DTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
c     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
c     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
c     ..
c
c  Purpose
c  =======
c
c  DTRMM  performs one of the matrix-matrix operations
c
c     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
c
c  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
c  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
c
c     op( A ) = A   or   op( A ) = A'.
c
c  Parameters
c  ==========
c
c  SIDE   - CHARACTER*1.
c           On entry,  SIDE specifies whether  op( A ) multiplies B from
c           the left or right as follows:
c
c              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
c
c              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
c
c           Unchanged on exit.
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the matrix A is an upper or
c           lower triangular matrix as follows:
c
c              UPLO = 'U' or 'u'   A is an upper triangular matrix.
c
c              UPLO = 'L' or 'l'   A is a lower triangular matrix.
c
c           Unchanged on exit.
c
c  TRANSA - CHARACTER*1.
c           On entry, TRANSA specifies the form of op( A ) to be used in
c           the matrix multiplication as follows:
c
c              TRANSA = 'N' or 'n'   op( A ) = A.
c
c              TRANSA = 'T' or 't'   op( A ) = A'.
c
c              TRANSA = 'C' or 'c'   op( A ) = A'.
c
c           Unchanged on exit.
c
c  DIAG   - CHARACTER*1.
c           On entry, DIAG specifies whether or not A is unit triangular
c           as follows:
c
c              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
c
c              DIAG = 'N' or 'n'   A is not assumed to be unit
c                                  triangular.
c
c           Unchanged on exit.
c
c  M      - INTEGER.
c           On entry, M specifies the number of rows of B. M must be at
c           least zero.
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the number of columns of B.  N must be
c           at least zero.
c           Unchanged on exit.
c
c  ALPHA  - DOUBLE PRECISION.
c           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
c           zero then  A is not referenced and  B need not be set before
c           entry.
c           Unchanged on exit.
c
c  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
c           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
c           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
c           upper triangular part of the array  A must contain the upper
c           triangular matrix  and the strictly lower triangular part of
c           A is not referenced.
c           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
c           lower triangular part of the array  A must contain the lower
c           triangular matrix  and the strictly upper triangular part of
c           A is not referenced.
c           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
c           A  are not referenced either,  but are assumed to be  unity.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
c           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
c           then LDA must be at least max( 1, n ).
c           Unchanged on exit.
c
c  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
c           Before entry,  the leading  m by n part of the array  B must
c           contain the matrix  B,  and  on exit  is overwritten  by the
c           transformed matrix.
c
c  LDB    - INTEGER.
c           On entry, LDB specifies the first dimension of B as declared
c           in  the  calling  (sub)  program.   LDB  must  be  at  least
c           max( 1, m ).
c           Unchanged on exit.
c
c
c  Level 3 Blas routine.
c
c  -- Written on 8-February-1989.
c     Jack Dongarra, Argonne National Laboratory.
c     Iain Duff, AERE Harwell.
c     Jeremy Du Croz, Numerical Algorithms Group Ltd.
c     Sven Hammarling, Numerical Algorithms Group Ltd.
c
c
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          MAX
c     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
c     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
c
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRMM ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
c     And when  alpha.eq.zero.
c
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
c
c     Start the operations.
c
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
c
c           Form  B := alpha*A*B.
c
            IF( UPPER )THEN
               DO 50, J = 1, N
                  DO 40, K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*B( K, J )
                        DO 30, I = 1, K - 1
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   30                   CONTINUE
                        IF( NOUNIT )
     $                     TEMP = TEMP*A( K, K )
                        B( K, J ) = TEMP
                     END IF
   40             CONTINUE
   50          CONTINUE
            ELSE
               DO 80, J = 1, N
                  DO 70 K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP      = ALPHA*B( K, J )
                        B( K, J ) = TEMP
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )*A( K, K )
                        DO 60, I = K + 1, M
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   60                   CONTINUE
                     END IF
   70             CONTINUE
   80          CONTINUE
            END IF
         ELSE
c
c           Form  B := alpha*A'*B.
c
            IF( UPPER )THEN
               DO 110, J = 1, N
                  DO 100, I = M, 1, -1
                     TEMP = B( I, J )
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
                     DO 90, K = 1, I - 1
                        TEMP = TEMP + A( K, I )*B( K, J )
   90                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  100             CONTINUE
  110          CONTINUE
            ELSE
               DO 140, J = 1, N
                  DO 130, I = 1, M
                     TEMP = B( I, J )
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
                     DO 120, K = I + 1, M
                        TEMP = TEMP + A( K, I )*B( K, J )
  120                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  130             CONTINUE
  140          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
c
c           Form  B := alpha*B*A.
c
            IF( UPPER )THEN
               DO 180, J = N, 1, -1
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 150, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  150             CONTINUE
                  DO 170, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 160, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  160                   CONTINUE
                     END IF
  170             CONTINUE
  180          CONTINUE
            ELSE
               DO 220, J = 1, N
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 190, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  190             CONTINUE
                  DO 210, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 200, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  200                   CONTINUE
                     END IF
  210             CONTINUE
  220          CONTINUE
            END IF
         ELSE
c
c           Form  B := alpha*B*A'.
c
            IF( UPPER )THEN
               DO 260, K = 1, N
                  DO 240, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 250, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  250                CONTINUE
                  END IF
  260          CONTINUE
            ELSE
               DO 300, K = N, 1, -1
                  DO 280, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 270, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  270                   CONTINUE
                     END IF
  280             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 290, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  290                CONTINUE
                  END IF
  300          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of DTRMM .
c
      END
      SUBROUTINE DTRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
c     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
c     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
c     ..
c
c  Purpose
c  =======
c
c  DTRSM  solves one of the matrix equations
c
c     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
c
c  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
c  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
c
c     op( A ) = A   or   op( A ) = A'.
c
c  The matrix X is overwritten on B.
c
c  Parameters
c  ==========
c
c  SIDE   - CHARACTER*1.
c           On entry, SIDE specifies whether op( A ) appears on the left
c           or right of X as follows:
c
c              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
c
c              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
c
c           Unchanged on exit.
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the matrix A is an upper or
c           lower triangular matrix as follows:
c
c              UPLO = 'U' or 'u'   A is an upper triangular matrix.
c
c              UPLO = 'L' or 'l'   A is a lower triangular matrix.
c
c           Unchanged on exit.
c
c  TRANSA - CHARACTER*1.
c           On entry, TRANSA specifies the form of op( A ) to be used in
c           the matrix multiplication as follows:
c
c              TRANSA = 'N' or 'n'   op( A ) = A.
c
c              TRANSA = 'T' or 't'   op( A ) = A'.
c
c              TRANSA = 'C' or 'c'   op( A ) = A'.
c
c           Unchanged on exit.
c
c  DIAG   - CHARACTER*1.
c           On entry, DIAG specifies whether or not A is unit triangular
c           as follows:
c
c              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
c
c              DIAG = 'N' or 'n'   A is not assumed to be unit
c                                  triangular.
c
c           Unchanged on exit.
c
c  M      - INTEGER.
c           On entry, M specifies the number of rows of B. M must be at
c           least zero.
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the number of columns of B.  N must be
c           at least zero.
c           Unchanged on exit.
c
c  ALPHA  - DOUBLE PRECISION.
c           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
c           zero then  A is not referenced and  B need not be set before
c           entry.
c           Unchanged on exit.
c
c  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
c           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
c           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
c           upper triangular part of the array  A must contain the upper
c           triangular matrix  and the strictly lower triangular part of
c           A is not referenced.
c           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
c           lower triangular part of the array  A must contain the lower
c           triangular matrix  and the strictly upper triangular part of
c           A is not referenced.
c           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
c           A  are not referenced either,  but are assumed to be  unity.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
c           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
c           then LDA must be at least max( 1, n ).
c           Unchanged on exit.
c
c  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
c           Before entry,  the leading  m by n part of the array  B must
c           contain  the  right-hand  side  matrix  B,  and  on exit  is
c           overwritten by the solution matrix  X.
c
c  LDB    - INTEGER.
c           On entry, LDB specifies the first dimension of B as declared
c           in  the  calling  (sub)  program.   LDB  must  be  at  least
c           max( 1, m ).
c           Unchanged on exit.
c
c
c  Level 3 Blas routine.
c
c
c  -- Written on 8-February-1989.
c     Jack Dongarra, Argonne National Laboratory.
c     Iain Duff, AERE Harwell.
c     Jeremy Du Croz, Numerical Algorithms Group Ltd.
c     Sven Hammarling, Numerical Algorithms Group Ltd.
c
c
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          MAX
c     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
c     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
c
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRSM ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
c     And when  alpha.eq.zero.
c
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
c
c     Start the operations.
c
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
c
c           Form  B := alpha*inv( A )*B.
c
            IF( UPPER )THEN
               DO 60, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 30, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   30                CONTINUE
                  END IF
                  DO 50, K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 40, I = 1, K - 1
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   40                   CONTINUE
                     END IF
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 100, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 70, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   70                CONTINUE
                  END IF
                  DO 90 K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 80, I = K + 1, M
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   80                   CONTINUE
                     END IF
   90             CONTINUE
  100          CONTINUE
            END IF
         ELSE
c
c           Form  B := alpha*inv( A' )*B.
c
            IF( UPPER )THEN
               DO 130, J = 1, N
                  DO 120, I = 1, M
                     TEMP = ALPHA*B( I, J )
                     DO 110, K = 1, I - 1
                        TEMP = TEMP - A( K, I )*B( K, J )
  110                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  120             CONTINUE
  130          CONTINUE
            ELSE
               DO 160, J = 1, N
                  DO 150, I = M, 1, -1
                     TEMP = ALPHA*B( I, J )
                     DO 140, K = I + 1, M
                        TEMP = TEMP - A( K, I )*B( K, J )
  140                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  150             CONTINUE
  160          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
c
c           Form  B := alpha*B*inv( A ).
c
            IF( UPPER )THEN
               DO 210, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 170, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  170                CONTINUE
                  END IF
                  DO 190, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 180, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  180                   CONTINUE
                     END IF
  190             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 200, I = 1, M
                        B( I, J ) = TEMP*B( I, J )
  200                CONTINUE
                  END IF
  210          CONTINUE
            ELSE
               DO 260, J = N, 1, -1
                  IF( ALPHA.NE.ONE )THEN
                     DO 220, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  220                CONTINUE
                  END IF
                  DO 240, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 250, I = 1, M
                       B( I, J ) = TEMP*B( I, J )
  250                CONTINUE
                  END IF
  260          CONTINUE
            END IF
         ELSE
c
c           Form  B := alpha*B*inv( A' ).
c
            IF( UPPER )THEN
               DO 310, K = N, 1, -1
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 270, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  270                CONTINUE
                  END IF
                  DO 290, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 280, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  280                   CONTINUE
                     END IF
  290             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 300, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  300                CONTINUE
                  END IF
  310          CONTINUE
            ELSE
               DO 360, K = 1, N
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 320, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  320                CONTINUE
                  END IF
                  DO 340, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 330, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  330                   CONTINUE
                     END IF
  340             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 350, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  350                CONTINUE
                  END IF
  360          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of DTRSM .
c
      END
