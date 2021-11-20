      SUBROUTINE ZGBMV ( TRANS, M, N, KL, KU, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
c     .. Scalar Arguments ..
      COMPLEX*16         ALPHA, BETA
      INTEGER            INCX, INCY, KL, KU, LDA, M, N
      CHARACTER*1        TRANS
c     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  ZGBMV  performs one of the matrix-vector operations
c
c     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,   or
c
c     y := alpha*conjg( A' )*x + beta*y,
c
c  where alpha and beta are scalars, x and y are vectors and A is an
c  m by n band matrix, with kl sub-diagonals and ku super-diagonals.
c
c  Parameters
c  ==========
c
c  TRANS  - CHARACTER*1.
c           On entry, TRANS specifies the operation to be performed as
c           follows:
c
c              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
c
c              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
c
c              TRANS = 'C' or 'c'   y := alpha*conjg( A' )*x + beta*y.
c
c           Unchanged on exit.
c
c  M      - INTEGER.
c           On entry, M specifies the number of rows of the matrix A.
c           M must be at least zero.
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the number of columns of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  KL     - INTEGER.
c           On entry, KL specifies the number of sub-diagonals of the
c           matrix A. KL must satisfy  0 .le. KL.
c           Unchanged on exit.
c
c  KU     - INTEGER.
c           On entry, KU specifies the number of super-diagonals of the
c           matrix A. KU must satisfy  0 .le. KU.
c           Unchanged on exit.
c
c  ALPHA  - COMPLEX*16      .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
c           Before entry, the leading ( kl + ku + 1 ) by n part of the
c           array A must contain the matrix of coefficients, supplied
c           column by column, with the leading diagonal of the matrix in
c           row ( ku + 1 ) of the array, the first super-diagonal
c           starting at position 2 in row ku, the first sub-diagonal
c           starting at position 1 in row ( ku + 2 ), and so on.
c           Elements in the array A that do not correspond to elements
c           in the band matrix (such as the top left ku by ku triangle)
c           are not referenced.
c           The following program segment will transfer a band matrix
c           from conventional full matrix storage to band storage:
c
c                 DO 20, J = 1, N
c                    K = KU + 1 - J
c                    DO 10, I = MAX( 1, J - KU ), MIN( M, J + KL )
c                       A( K + I, J ) = matrix( I, J )
c              10    CONTINUE
c              20 CONTINUE
c
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           ( kl + ku + 1 ).
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of DIMENSION at least
c           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
c           and at least
c           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
c           Before entry, the incremented array X must contain the
c           vector x.
c           Unchanged on exit.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c  BETA   - COMPLEX*16      .
c           On entry, BETA specifies the scalar beta. When BETA is
c           supplied as zero then Y need not be set on input.
c           Unchanged on exit.
c
c  Y      - COMPLEX*16       array of DIMENSION at least
c           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
c           and at least
c           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
c           Before entry, the incremented array Y must contain the
c           vector y. On exit, Y is overwritten by the updated vector y.
c
c
c  INCY   - INTEGER.
c           On entry, INCY specifies the increment for the elements of
c           Y. INCY must not be zero.
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER        ( ONE  = ( 1.0D+0, 0.0D+0 ) )
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, K, KUP1, KX, KY,
     $                   LENX, LENY
      LOGICAL            NOCONJ
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX, MIN
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( KL.LT.0 )THEN
         INFO = 4
      ELSE IF( KU.LT.0 )THEN
         INFO = 5
      ELSE IF( LDA.LT.( KL + KU + 1 ) )THEN
         INFO = 8
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 10
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZGBMV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
c
      NOCONJ = LSAME( TRANS, 'T' )
c
c     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
c     up the start points in  X  and  Y.
c
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
c
c     Start the operations. In this version the elements of A are
c     accessed sequentially with one pass through the band part of A.
c
c     First form  y := beta*y.
c
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      KUP1 = KU + 1
      IF( LSAME( TRANS, 'N' ) )THEN
c
c        Form  y := alpha*A*x + y.
c
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  K    = KUP1 - J
                  DO 50, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     Y( I ) = Y( I ) + TEMP*A( K + I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  K    = KUP1 - J
                  DO 70, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     Y( IY ) = Y( IY ) + TEMP*A( K + I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
               IF( J.GT.KU )
     $            KY = KY + INCY
   80       CONTINUE
         END IF
      ELSE
c
c        Form  y := alpha*A'*x + y  or  y := alpha*conjg( A' )*x + y.
c
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 110, J = 1, N
               TEMP = ZERO
               K    = KUP1 - J
               IF( NOCONJ )THEN
                  DO 90, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     TEMP = TEMP + A( K + I, J )*X( I )
   90             CONTINUE
               ELSE
                  DO 100, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     TEMP = TEMP + DCONJG( A( K + I, J ) )*X( I )
  100             CONTINUE
               END IF
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  110       CONTINUE
         ELSE
            DO 140, J = 1, N
               TEMP = ZERO
               IX   = KX
               K    = KUP1 - J
               IF( NOCONJ )THEN
                  DO 120, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     TEMP = TEMP + A( K + I, J )*X( IX )
                     IX   = IX   + INCX
  120             CONTINUE
               ELSE
                  DO 130, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     TEMP = TEMP + DCONJG( A( K + I, J ) )*X( IX )
                     IX   = IX   + INCX
  130             CONTINUE
               END IF
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
               IF( J.GT.KU )
     $            KX = KX + INCX
  140       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of ZGBMV .
c
      END
      SUBROUTINE ZGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
c     .. Scalar Arguments ..
      COMPLEX*16         ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
c     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  ZGEMV  performs one of the matrix-vector operations
c
c     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,   or
c
c     y := alpha*conjg( A' )*x + beta*y,
c
c  where alpha and beta are scalars, x and y are vectors and A is an
c  m by n matrix.
c
c  Parameters
c  ==========
c
c  TRANS  - CHARACTER*1.
c           On entry, TRANS specifies the operation to be performed as
c           follows:
c
c              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
c
c              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
c
c              TRANS = 'C' or 'c'   y := alpha*conjg( A' )*x + beta*y.
c
c           Unchanged on exit.
c
c  M      - INTEGER.
c           On entry, M specifies the number of rows of the matrix A.
c           M must be at least zero.
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the number of columns of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  ALPHA  - COMPLEX*16      .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
c           Before entry, the leading m by n part of the array A must
c           contain the matrix of coefficients.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           max( 1, m ).
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of DIMENSION at least
c           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
c           and at least
c           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
c           Before entry, the incremented array X must contain the
c           vector x.
c           Unchanged on exit.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c  BETA   - COMPLEX*16      .
c           On entry, BETA specifies the scalar beta. When BETA is
c           supplied as zero then Y need not be set on input.
c           Unchanged on exit.
c
c  Y      - COMPLEX*16       array of DIMENSION at least
c           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
c           and at least
c           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
c           Before entry with BETA non-zero, the incremented array Y
c           must contain the vector y. On exit, Y is overwritten by the
c           updated vector y.
c
c  INCY   - INTEGER.
c           On entry, INCY specifies the increment for the elements of
c           Y. INCY must not be zero.
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER        ( ONE  = ( 1.0D+0, 0.0D+0 ) )
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
      LOGICAL            NOCONJ
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZGEMV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
c
      NOCONJ = LSAME( TRANS, 'T' )
c
c     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
c     up the start points in  X  and  Y.
c
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
c
c     Start the operations. In this version the elements of A are
c     accessed sequentially with one pass through A.
c
c     First form  y := beta*y.
c
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
c
c        Form  y := alpha*A*x + y.
c
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
c
c        Form  y := alpha*A'*x + y  or  y := alpha*conjg( A' )*x + y.
c
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 110, J = 1, N
               TEMP = ZERO
               IF( NOCONJ )THEN
                  DO 90, I = 1, M
                     TEMP = TEMP + A( I, J )*X( I )
   90             CONTINUE
               ELSE
                  DO 100, I = 1, M
                     TEMP = TEMP + DCONJG( A( I, J ) )*X( I )
  100             CONTINUE
               END IF
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  110       CONTINUE
         ELSE
            DO 140, J = 1, N
               TEMP = ZERO
               IX   = KX
               IF( NOCONJ )THEN
                  DO 120, I = 1, M
                     TEMP = TEMP + A( I, J )*X( IX )
                     IX   = IX   + INCX
  120             CONTINUE
               ELSE
                  DO 130, I = 1, M
                     TEMP = TEMP + DCONJG( A( I, J ) )*X( IX )
                     IX   = IX   + INCX
  130             CONTINUE
               END IF
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  140       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of ZGEMV .
c
      END
      SUBROUTINE ZGERC ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
c     .. Scalar Arguments ..
      COMPLEX*16         ALPHA
      INTEGER            INCX, INCY, LDA, M, N
c     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  ZGERC  performs the rank 1 operation
c
c     A := alpha*x*conjg( y' ) + A,
c
c  where alpha is a scalar, x is an m element vector, y is an n element
c  vector and A is an m by n matrix.
c
c  Parameters
c  ==========
c
c  M      - INTEGER.
c           On entry, M specifies the number of rows of the matrix A.
c           M must be at least zero.
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the number of columns of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  ALPHA  - COMPLEX*16      .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( m - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the m
c           element vector x.
c           Unchanged on exit.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c  Y      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCY ) ).
c           Before entry, the incremented array Y must contain the n
c           element vector y.
c           Unchanged on exit.
c
c  INCY   - INTEGER.
c           On entry, INCY specifies the increment for the elements of
c           Y. INCY must not be zero.
c           Unchanged on exit.
c
c  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
c           Before entry, the leading m by n part of the array A must
c           contain the matrix of coefficients. On exit, A is
c           overwritten by the updated matrix.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           max( 1, m ).
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, J, JY, KX
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZGERC ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
c
c     Start the operations. In this version the elements of A are
c     accessed sequentially with one pass through A.
c
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*DCONJG( Y( JY ) )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*DCONJG( Y( JY ) )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
c
      RETURN
c
c     End of ZGERC .
c
      END
      SUBROUTINE ZGERU ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
c     .. Scalar Arguments ..
      COMPLEX*16         ALPHA
      INTEGER            INCX, INCY, LDA, M, N
c     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  ZGERU  performs the rank 1 operation
c
c     A := alpha*x*y' + A,
c
c  where alpha is a scalar, x is an m element vector, y is an n element
c  vector and A is an m by n matrix.
c
c  Parameters
c  ==========
c
c  M      - INTEGER.
c           On entry, M specifies the number of rows of the matrix A.
c           M must be at least zero.
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the number of columns of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  ALPHA  - COMPLEX*16      .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( m - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the m
c           element vector x.
c           Unchanged on exit.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c  Y      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCY ) ).
c           Before entry, the incremented array Y must contain the n
c           element vector y.
c           Unchanged on exit.
c
c  INCY   - INTEGER.
c           On entry, INCY specifies the increment for the elements of
c           Y. INCY must not be zero.
c           Unchanged on exit.
c
c  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
c           Before entry, the leading m by n part of the array A must
c           contain the matrix of coefficients. On exit, A is
c           overwritten by the updated matrix.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           max( 1, m ).
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, J, JY, KX
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          MAX
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZGERU ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
c
c     Start the operations. In this version the elements of A are
c     accessed sequentially with one pass through A.
c
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
c
      RETURN
c
c     End of ZGERU .
c
      END
      SUBROUTINE ZHBMV ( UPLO, N, K, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
c     .. Scalar Arguments ..
      COMPLEX*16         ALPHA, BETA
      INTEGER            INCX, INCY, K, LDA, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  ZHBMV  performs the matrix-vector  operation
c
c     y := alpha*A*x + beta*y,
c
c  where alpha and beta are scalars, x and y are n element vectors and
c  A is an n by n hermitian band matrix, with k super-diagonals.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the upper or lower
c           triangular part of the band matrix A is being supplied as
c           follows:
c
c              UPLO = 'U' or 'u'   The upper triangular part of A is
c                                  being supplied.
c
c              UPLO = 'L' or 'l'   The lower triangular part of A is
c                                  being supplied.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  K      - INTEGER.
c           On entry, K specifies the number of super-diagonals of the
c           matrix A. K must satisfy  0 .le. K.
c           Unchanged on exit.
c
c  ALPHA  - COMPLEX*16      .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
c           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
c           by n part of the array A must contain the upper triangular
c           band part of the hermitian matrix, supplied column by
c           column, with the leading diagonal of the matrix in row
c           ( k + 1 ) of the array, the first super-diagonal starting at
c           position 2 in row k, and so on. The top left k by k triangle
c           of the array A is not referenced.
c           The following program segment will transfer the upper
c           triangular part of a hermitian band matrix from conventional
c           full matrix storage to band storage:
c
c                 DO 20, J = 1, N
c                    M = K + 1 - J
c                    DO 10, I = MAX( 1, J - K ), J
c                       A( M + I, J ) = matrix( I, J )
c              10    CONTINUE
c              20 CONTINUE
c
c           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
c           by n part of the array A must contain the lower triangular
c           band part of the hermitian matrix, supplied column by
c           column, with the leading diagonal of the matrix in row 1 of
c           the array, the first sub-diagonal starting at position 1 in
c           row 2, and so on. The bottom right k by k triangle of the
c           array A is not referenced.
c           The following program segment will transfer the lower
c           triangular part of a hermitian band matrix from conventional
c           full matrix storage to band storage:
c
c                 DO 20, J = 1, N
c                    M = 1 - J
c                    DO 10, I = J, MIN( N, J + K )
c                       A( M + I, J ) = matrix( I, J )
c              10    CONTINUE
c              20 CONTINUE
c
c           Note that the imaginary parts of the diagonal elements need
c           not be set and are assumed to be zero.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           ( k + 1 ).
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of DIMENSION at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the
c           vector x.
c           Unchanged on exit.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c  BETA   - COMPLEX*16      .
c           On entry, BETA specifies the scalar beta.
c           Unchanged on exit.
c
c  Y      - COMPLEX*16       array of DIMENSION at least
c           ( 1 + ( n - 1 )*abs( INCY ) ).
c           Before entry, the incremented array Y must contain the
c           vector y. On exit, Y is overwritten by the updated vector y.
c
c  INCY   - INTEGER.
c           On entry, INCY specifies the increment for the elements of
c           Y. INCY must not be zero.
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER        ( ONE  = ( 1.0D+0, 0.0D+0 ) )
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, KPLUS1, KX, KY, L
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX, MIN, DBLE
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO, 'U' ).AND.
     $         .NOT.LSAME( UPLO, 'L' )      )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( K.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.( K + 1 ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZHBMV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( N.EQ.0 ).OR.( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
c
c     Set up the start points in  X  and  Y.
c
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( N - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( N - 1 )*INCY
      END IF
c
c     Start the operations. In this version the elements of the array A
c     are accessed sequentially with one pass through A.
c
c     First form  y := beta*y.
c
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, N
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, N
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, N
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, N
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( UPLO, 'U' ) )THEN
c
c        Form  y  when upper triangle of A is stored.
c
         KPLUS1 = K + 1
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 60, J = 1, N
               TEMP1 = ALPHA*X( J )
               TEMP2 = ZERO
               L     = KPLUS1 - J
               DO 50, I = MAX( 1, J - K ), J - 1
                  Y( I ) = Y( I ) + TEMP1*A( L + I, J )
                  TEMP2  = TEMP2  + DCONJG( A( L + I, J ) )*X( I )
   50          CONTINUE
               Y( J ) = Y( J ) + TEMP1*DBLE( A( KPLUS1, J ) )
     $                         + ALPHA*TEMP2
   60       CONTINUE
         ELSE
            JX = KX
            JY = KY
            DO 80, J = 1, N
               TEMP1 = ALPHA*X( JX )
               TEMP2 = ZERO
               IX    = KX
               IY    = KY
               L     = KPLUS1 - J
               DO 70, I = MAX( 1, J - K ), J - 1
                  Y( IY ) = Y( IY ) + TEMP1*A( L + I, J )
                  TEMP2   = TEMP2   + DCONJG( A( L + I, J ) )*X( IX )
                  IX      = IX      + INCX
                  IY      = IY      + INCY
   70          CONTINUE
               Y( JY ) = Y( JY ) + TEMP1*DBLE( A( KPLUS1, J ) )
     $                           + ALPHA*TEMP2
               JX      = JX      + INCX
               JY      = JY      + INCY
               IF( J.GT.K )THEN
                  KX = KX + INCX
                  KY = KY + INCY
               END IF
   80       CONTINUE
         END IF
      ELSE
c
c        Form  y  when lower triangle of A is stored.
c
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 100, J = 1, N
               TEMP1  = ALPHA*X( J )
               TEMP2  = ZERO
               Y( J ) = Y( J ) + TEMP1*DBLE( A( 1, J ) )
               L      = 1      - J
               DO 90, I = J + 1, MIN( N, J + K )
                  Y( I ) = Y( I ) + TEMP1*A( L + I, J )
                  TEMP2  = TEMP2  + DCONJG( A( L + I, J ) )*X( I )
   90          CONTINUE
               Y( J ) = Y( J ) + ALPHA*TEMP2
  100       CONTINUE
         ELSE
            JX = KX
            JY = KY
            DO 120, J = 1, N
               TEMP1   = ALPHA*X( JX )
               TEMP2   = ZERO
               Y( JY ) = Y( JY ) + TEMP1*DBLE( A( 1, J ) )
               L       = 1       - J
               IX      = JX
               IY      = JY
               DO 110, I = J + 1, MIN( N, J + K )
                  IX      = IX      + INCX
                  IY      = IY      + INCY
                  Y( IY ) = Y( IY ) + TEMP1*A( L + I, J )
                  TEMP2   = TEMP2   + DCONJG( A( L + I, J ) )*X( IX )
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP2
               JX      = JX      + INCX
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of ZHBMV .
c
      END
      SUBROUTINE ZHEMV ( UPLO, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
c     .. Scalar Arguments ..
      COMPLEX*16         ALPHA, BETA
      INTEGER            INCX, INCY, LDA, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  ZHEMV  performs the matrix-vector  operation
c
c     y := alpha*A*x + beta*y,
c
c  where alpha and beta are scalars, x and y are n element vectors and
c  A is an n by n hermitian matrix.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the upper or lower
c           triangular part of the array A is to be referenced as
c           follows:
c
c              UPLO = 'U' or 'u'   Only the upper triangular part of A
c                                  is to be referenced.
c
c              UPLO = 'L' or 'l'   Only the lower triangular part of A
c                                  is to be referenced.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  ALPHA  - COMPLEX*16      .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
c           Before entry with  UPLO = 'U' or 'u', the leading n by n
c           upper triangular part of the array A must contain the upper
c           triangular part of the hermitian matrix and the strictly
c           lower triangular part of A is not referenced.
c           Before entry with UPLO = 'L' or 'l', the leading n by n
c           lower triangular part of the array A must contain the lower
c           triangular part of the hermitian matrix and the strictly
c           upper triangular part of A is not referenced.
c           Note that the imaginary parts of the diagonal elements need
c           not be set and are assumed to be zero.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           max( 1, n ).
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element vector x.
c           Unchanged on exit.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c  BETA   - COMPLEX*16      .
c           On entry, BETA specifies the scalar beta. When BETA is
c           supplied as zero then Y need not be set on input.
c           Unchanged on exit.
c
c  Y      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCY ) ).
c           Before entry, the incremented array Y must contain the n
c           element vector y. On exit, Y is overwritten by the updated
c           vector y.
c
c  INCY   - INTEGER.
c           On entry, INCY specifies the increment for the elements of
c           Y. INCY must not be zero.
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER        ( ONE  = ( 1.0D+0, 0.0D+0 ) )
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX, DBLE
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO, 'U' ).AND.
     $         .NOT.LSAME( UPLO, 'L' )      )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 5
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 7
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 10
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZHEMV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( N.EQ.0 ).OR.( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
c
c     Set up the start points in  X  and  Y.
c
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( N - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( N - 1 )*INCY
      END IF
c
c     Start the operations. In this version the elements of A are
c     accessed sequentially with one pass through the triangular part
c     of A.
c
c     First form  y := beta*y.
c
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, N
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, N
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, N
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, N
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( UPLO, 'U' ) )THEN
c
c        Form  y  when A is stored in upper triangle.
c
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 60, J = 1, N
               TEMP1 = ALPHA*X( J )
               TEMP2 = ZERO
               DO 50, I = 1, J - 1
                  Y( I ) = Y( I ) + TEMP1*A( I, J )
                  TEMP2  = TEMP2  + DCONJG( A( I, J ) )*X( I )
   50          CONTINUE
               Y( J ) = Y( J ) + TEMP1*DBLE( A( J, J ) ) + ALPHA*TEMP2
   60       CONTINUE
         ELSE
            JX = KX
            JY = KY
            DO 80, J = 1, N
               TEMP1 = ALPHA*X( JX )
               TEMP2 = ZERO
               IX    = KX
               IY    = KY
               DO 70, I = 1, J - 1
                  Y( IY ) = Y( IY ) + TEMP1*A( I, J )
                  TEMP2   = TEMP2   + DCONJG( A( I, J ) )*X( IX )
                  IX      = IX      + INCX
                  IY      = IY      + INCY
   70          CONTINUE
               Y( JY ) = Y( JY ) + TEMP1*DBLE( A( J, J ) ) + ALPHA*TEMP2
               JX      = JX      + INCX
               JY      = JY      + INCY
   80       CONTINUE
         END IF
      ELSE
c
c        Form  y  when A is stored in lower triangle.
c
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 100, J = 1, N
               TEMP1  = ALPHA*X( J )
               TEMP2  = ZERO
               Y( J ) = Y( J ) + TEMP1*DBLE( A( J, J ) )
               DO 90, I = J + 1, N
                  Y( I ) = Y( I ) + TEMP1*A( I, J )
                  TEMP2  = TEMP2  + DCONJG( A( I, J ) )*X( I )
   90          CONTINUE
               Y( J ) = Y( J ) + ALPHA*TEMP2
  100       CONTINUE
         ELSE
            JX = KX
            JY = KY
            DO 120, J = 1, N
               TEMP1   = ALPHA*X( JX )
               TEMP2   = ZERO
               Y( JY ) = Y( JY ) + TEMP1*DBLE( A( J, J ) )
               IX      = JX
               IY      = JY
               DO 110, I = J + 1, N
                  IX      = IX      + INCX
                  IY      = IY      + INCY
                  Y( IY ) = Y( IY ) + TEMP1*A( I, J )
                  TEMP2   = TEMP2   + DCONJG( A( I, J ) )*X( IX )
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP2
               JX      = JX      + INCX
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of ZHEMV .
c
      END
      SUBROUTINE ZHER2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, A, LDA )
c     .. Scalar Arguments ..
      COMPLEX*16         ALPHA
      INTEGER            INCX, INCY, LDA, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  ZHER2  performs the hermitian rank 2 operation
c
c     A := alpha*x*conjg( y' ) + conjg( alpha )*y*conjg( x' ) + A,
c
c  where alpha is a scalar, x and y are n element vectors and A is an n
c  by n hermitian matrix.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the upper or lower
c           triangular part of the array A is to be referenced as
c           follows:
c
c              UPLO = 'U' or 'u'   Only the upper triangular part of A
c                                  is to be referenced.
c
c              UPLO = 'L' or 'l'   Only the lower triangular part of A
c                                  is to be referenced.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  ALPHA  - COMPLEX*16      .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element vector x.
c           Unchanged on exit.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c  Y      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCY ) ).
c           Before entry, the incremented array Y must contain the n
c           element vector y.
c           Unchanged on exit.
c
c  INCY   - INTEGER.
c           On entry, INCY specifies the increment for the elements of
c           Y. INCY must not be zero.
c           Unchanged on exit.
c
c  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
c           Before entry with  UPLO = 'U' or 'u', the leading n by n
c           upper triangular part of the array A must contain the upper
c           triangular part of the hermitian matrix and the strictly
c           lower triangular part of A is not referenced. On exit, the
c           upper triangular part of the array A is overwritten by the
c           upper triangular part of the updated matrix.
c           Before entry with UPLO = 'L' or 'l', the leading n by n
c           lower triangular part of the array A must contain the lower
c           triangular part of the hermitian matrix and the strictly
c           upper triangular part of A is not referenced. On exit, the
c           lower triangular part of the array A is overwritten by the
c           lower triangular part of the updated matrix.
c           Note that the imaginary parts of the diagonal elements need
c           not be set, they are assumed to be zero, and on exit they
c           are set to zero.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           max( 1, n ).
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX, DBLE
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO, 'U' ).AND.
     $         .NOT.LSAME( UPLO, 'L' )      )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZHER2 ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
c
c     Set up the start points in X and Y if the increments are not both
c     unity.
c
      IF( ( INCX.NE.1 ).OR.( INCY.NE.1 ) )THEN
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( N - 1 )*INCX
         END IF
         IF( INCY.GT.0 )THEN
            KY = 1
         ELSE
            KY = 1 - ( N - 1 )*INCY
         END IF
         JX = KX
         JY = KY
      END IF
c
c     Start the operations. In this version the elements of A are
c     accessed sequentially with one pass through the triangular part
c     of A.
c
      IF( LSAME( UPLO, 'U' ) )THEN
c
c        Form  A  when A is stored in the upper triangle.
c
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 20, J = 1, N
               IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*DCONJG( Y( J ) )
                  TEMP2 = DCONJG( ALPHA*X( J ) )
                  DO 10, I = 1, J - 1
                     A( I, J ) = A( I, J ) + X( I )*TEMP1 + Y( I )*TEMP2
   10             CONTINUE
                  A( J, J ) = DBLE( A( J, J ) ) +
     $                        DBLE( X( J )*TEMP1 + Y( J )*TEMP2 )
               ELSE
                  A( J, J ) = DBLE( A( J, J ) )
               END IF
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*DCONJG( Y( JY ) )
                  TEMP2 = DCONJG( ALPHA*X( JX ) )
                  IX    = KX
                  IY    = KY
                  DO 30, I = 1, J - 1
                     A( I, J ) = A( I, J ) + X( IX )*TEMP1
     $                                     + Y( IY )*TEMP2
                     IX        = IX        + INCX
                     IY        = IY        + INCY
   30             CONTINUE
                  A( J, J ) = DBLE( A( J, J ) ) +
     $                        DBLE( X( JX )*TEMP1 + Y( JY )*TEMP2 )
               ELSE
                  A( J, J ) = DBLE( A( J, J ) )
               END IF
               JX = JX + INCX
               JY = JY + INCY
   40       CONTINUE
         END IF
      ELSE
c
c        Form  A  when A is stored in the lower triangle.
c
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 60, J = 1, N
               IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
                  TEMP1     = ALPHA*DCONJG( Y( J ) )
                  TEMP2     = DCONJG( ALPHA*X( J ) )
                  A( J, J ) = DBLE( A( J, J ) ) +
     $                        DBLE( X( J )*TEMP1 + Y( J )*TEMP2 )
                  DO 50, I = J + 1, N
                     A( I, J ) = A( I, J ) + X( I )*TEMP1 + Y( I )*TEMP2
   50             CONTINUE
               ELSE
                  A( J, J ) = DBLE( A( J, J ) )
               END IF
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1     = ALPHA*DCONJG( Y( JY ) )
                  TEMP2     = DCONJG( ALPHA*X( JX ) )
                  A( J, J ) = DBLE( A( J, J ) ) +
     $                        DBLE( X( JX )*TEMP1 + Y( JY )*TEMP2 )
                  IX        = JX
                  IY        = JY
                  DO 70, I = J + 1, N
                     IX        = IX        + INCX
                     IY        = IY        + INCY
                     A( I, J ) = A( I, J ) + X( IX )*TEMP1
     $                                     + Y( IY )*TEMP2
   70             CONTINUE
               ELSE
                  A( J, J ) = DBLE( A( J, J ) )
               END IF
               JX = JX + INCX
               JY = JY + INCY
   80       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of ZHER2 .
c
      END
      SUBROUTINE ZHER  ( UPLO, N, ALPHA, X, INCX, A, LDA )
c     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, LDA, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  ZHER   performs the hermitian rank 1 operation
c
c     A := alpha*x*conjg( x' ) + A,
c
c  where alpha is a real scalar, x is an n element vector and A is an
c  n by n hermitian matrix.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the upper or lower
c           triangular part of the array A is to be referenced as
c           follows:
c
c              UPLO = 'U' or 'u'   Only the upper triangular part of A
c                                  is to be referenced.
c
c              UPLO = 'L' or 'l'   Only the lower triangular part of A
c                                  is to be referenced.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  ALPHA  - DOUBLE PRECISION.
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element vector x.
c           Unchanged on exit.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
c           Before entry with  UPLO = 'U' or 'u', the leading n by n
c           upper triangular part of the array A must contain the upper
c           triangular part of the hermitian matrix and the strictly
c           lower triangular part of A is not referenced. On exit, the
c           upper triangular part of the array A is overwritten by the
c           upper triangular part of the updated matrix.
c           Before entry with UPLO = 'L' or 'l', the leading n by n
c           lower triangular part of the array A must contain the lower
c           triangular part of the hermitian matrix and the strictly
c           upper triangular part of A is not referenced. On exit, the
c           lower triangular part of the array A is overwritten by the
c           lower triangular part of the updated matrix.
c           Note that the imaginary parts of the diagonal elements need
c           not be set, they are assumed to be zero, and on exit they
c           are set to zero.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           max( 1, n ).
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, J, JX, KX
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX, DBLE
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO, 'U' ).AND.
     $         .NOT.LSAME( UPLO, 'L' )      )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 7
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZHER  ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( N.EQ.0 ).OR.( ALPHA.EQ.DBLE( ZERO ) ) )
     $   RETURN
c
c     Set the start point in X if the increment is not unity.
c
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
c
c     Start the operations. In this version the elements of A are
c     accessed sequentially with one pass through the triangular part
c     of A.
c
      IF( LSAME( UPLO, 'U' ) )THEN
c
c        Form  A  when A is stored in upper triangle.
c
         IF( INCX.EQ.1 )THEN
            DO 20, J = 1, N
               IF( X( J ).NE.ZERO )THEN
                  TEMP = ALPHA*DCONJG( X( J ) )
                  DO 10, I = 1, J - 1
                     A( I, J ) = A( I, J ) + X( I )*TEMP
   10             CONTINUE
                  A( J, J ) = DBLE( A( J, J ) ) + DBLE( X( J )*TEMP )
               ELSE
                  A( J, J ) = DBLE( A( J, J ) )
               END IF
   20       CONTINUE
         ELSE
            JX = KX
            DO 40, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*DCONJG( X( JX ) )
                  IX   = KX
                  DO 30, I = 1, J - 1
                     A( I, J ) = A( I, J ) + X( IX )*TEMP
                     IX        = IX        + INCX
   30             CONTINUE
                  A( J, J ) = DBLE( A( J, J ) ) + DBLE( X( JX )*TEMP )
               ELSE
                  A( J, J ) = DBLE( A( J, J ) )
               END IF
               JX = JX + INCX
   40       CONTINUE
         END IF
      ELSE
c
c        Form  A  when A is stored in lower triangle.
c
         IF( INCX.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( J ).NE.ZERO )THEN
                  TEMP      = ALPHA*DCONJG( X( J ) )
                  A( J, J ) = DBLE( A( J, J ) ) + DBLE( TEMP*X( J ) )
                  DO 50, I = J + 1, N
                     A( I, J ) = A( I, J ) + X( I )*TEMP
   50             CONTINUE
               ELSE
                  A( J, J ) = DBLE( A( J, J ) )
               END IF
   60       CONTINUE
         ELSE
            JX = KX
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP      = ALPHA*DCONJG( X( JX ) )
                  A( J, J ) = DBLE( A( J, J ) ) + DBLE( TEMP*X( JX ) )
                  IX        = JX
                  DO 70, I = J + 1, N
                     IX        = IX        + INCX
                     A( I, J ) = A( I, J ) + X( IX )*TEMP
   70             CONTINUE
               ELSE
                  A( J, J ) = DBLE( A( J, J ) )
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of ZHER  .
c
      END
      SUBROUTINE ZHPMV ( UPLO, N, ALPHA, AP, X, INCX, BETA, Y, INCY )
c     .. Scalar Arguments ..
      COMPLEX*16         ALPHA, BETA
      INTEGER            INCX, INCY, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      COMPLEX*16         AP( * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  ZHPMV  performs the matrix-vector operation
c
c     y := alpha*A*x + beta*y,
c
c  where alpha and beta are scalars, x and y are n element vectors and
c  A is an n by n hermitian matrix, supplied in packed form.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the upper or lower
c           triangular part of the matrix A is supplied in the packed
c           array AP as follows:
c
c              UPLO = 'U' or 'u'   The upper triangular part of A is
c                                  supplied in AP.
c
c              UPLO = 'L' or 'l'   The lower triangular part of A is
c                                  supplied in AP.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  ALPHA  - COMPLEX*16      .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  AP     - COMPLEX*16       array of DIMENSION at least
c           ( ( n*( n + 1 ) )/2 ).
c           Before entry with UPLO = 'U' or 'u', the array AP must
c           contain the upper triangular part of the hermitian matrix
c           packed sequentially, column by column, so that AP( 1 )
c           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
c           and a( 2, 2 ) respectively, and so on.
c           Before entry with UPLO = 'L' or 'l', the array AP must
c           contain the lower triangular part of the hermitian matrix
c           packed sequentially, column by column, so that AP( 1 )
c           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
c           and a( 3, 1 ) respectively, and so on.
c           Note that the imaginary parts of the diagonal elements need
c           not be set and are assumed to be zero.
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element vector x.
c           Unchanged on exit.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c  BETA   - COMPLEX*16      .
c           On entry, BETA specifies the scalar beta. When BETA is
c           supplied as zero then Y need not be set on input.
c           Unchanged on exit.
c
c  Y      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCY ) ).
c           Before entry, the incremented array Y must contain the n
c           element vector y. On exit, Y is overwritten by the updated
c           vector y.
c
c  INCY   - INTEGER.
c           On entry, INCY specifies the increment for the elements of
c           Y. INCY must not be zero.
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER        ( ONE  = ( 1.0D+0, 0.0D+0 ) )
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, K, KK, KX, KY
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, DBLE
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO, 'U' ).AND.
     $         .NOT.LSAME( UPLO, 'L' )      )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 6
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZHPMV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( N.EQ.0 ).OR.( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
c
c     Set up the start points in  X  and  Y.
c
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( N - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( N - 1 )*INCY
      END IF
c
c     Start the operations. In this version the elements of the array AP
c     are accessed sequentially with one pass through AP.
c
c     First form  y := beta*y.
c
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, N
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, N
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, N
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, N
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      KK = 1
      IF( LSAME( UPLO, 'U' ) )THEN
c
c        Form  y  when AP contains the upper triangle.
c
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 60, J = 1, N
               TEMP1 = ALPHA*X( J )
               TEMP2 = ZERO
               K     = KK
               DO 50, I = 1, J - 1
                  Y( I ) = Y( I ) + TEMP1*AP( K )
                  TEMP2  = TEMP2  + DCONJG( AP( K ) )*X( I )
                  K      = K      + 1
   50          CONTINUE
               Y( J ) = Y( J ) + TEMP1*DBLE( AP( KK + J - 1 ) )
     $                         + ALPHA*TEMP2
               KK     = KK     + J
   60       CONTINUE
         ELSE
            JX = KX
            JY = KY
            DO 80, J = 1, N
               TEMP1 = ALPHA*X( JX )
               TEMP2 = ZERO
               IX    = KX
               IY    = KY
               DO 70, K = KK, KK + J - 2
                  Y( IY ) = Y( IY ) + TEMP1*AP( K )
                  TEMP2   = TEMP2   + DCONJG( AP( K ) )*X( IX )
                  IX      = IX      + INCX
                  IY      = IY      + INCY
   70          CONTINUE
               Y( JY ) = Y( JY ) + TEMP1*DBLE( AP( KK + J - 1 ) )
     $                           + ALPHA*TEMP2
               JX      = JX      + INCX
               JY      = JY      + INCY
               KK      = KK      + J
   80       CONTINUE
         END IF
      ELSE
c
c        Form  y  when AP contains the lower triangle.
c
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 100, J = 1, N
               TEMP1  = ALPHA*X( J )
               TEMP2  = ZERO
               Y( J ) = Y( J ) + TEMP1*DBLE( AP( KK ) )
               K      = KK     + 1
               DO 90, I = J + 1, N
                  Y( I ) = Y( I ) + TEMP1*AP( K )
                  TEMP2  = TEMP2  + DCONJG( AP( K ) )*X( I )
                  K      = K      + 1
   90          CONTINUE
               Y( J ) = Y( J ) + ALPHA*TEMP2
               KK     = KK     + ( N - J + 1 )
  100       CONTINUE
         ELSE
            JX = KX
            JY = KY
            DO 120, J = 1, N
               TEMP1   = ALPHA*X( JX )
               TEMP2   = ZERO
               Y( JY ) = Y( JY ) + TEMP1*DBLE( AP( KK ) )
               IX      = JX
               IY      = JY
               DO 110, K = KK + 1, KK + N - J
                  IX      = IX      + INCX
                  IY      = IY      + INCY
                  Y( IY ) = Y( IY ) + TEMP1*AP( K )
                  TEMP2   = TEMP2   + DCONJG( AP( K ) )*X( IX )
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP2
               JX      = JX      + INCX
               JY      = JY      + INCY
               KK      = KK      + ( N - J + 1 )
  120       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of ZHPMV .
c
      END
      SUBROUTINE ZHPR2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, AP )
c     .. Scalar Arguments ..
      COMPLEX*16         ALPHA
      INTEGER            INCX, INCY, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      COMPLEX*16         AP( * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  ZHPR2  performs the hermitian rank 2 operation
c
c     A := alpha*x*conjg( y' ) + conjg( alpha )*y*conjg( x' ) + A,
c
c  where alpha is a scalar, x and y are n element vectors and A is an
c  n by n hermitian matrix, supplied in packed form.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the upper or lower
c           triangular part of the matrix A is supplied in the packed
c           array AP as follows:
c
c              UPLO = 'U' or 'u'   The upper triangular part of A is
c                                  supplied in AP.
c
c              UPLO = 'L' or 'l'   The lower triangular part of A is
c                                  supplied in AP.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  ALPHA  - COMPLEX*16      .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element vector x.
c           Unchanged on exit.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c  Y      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCY ) ).
c           Before entry, the incremented array Y must contain the n
c           element vector y.
c           Unchanged on exit.
c
c  INCY   - INTEGER.
c           On entry, INCY specifies the increment for the elements of
c           Y. INCY must not be zero.
c           Unchanged on exit.
c
c  AP     - COMPLEX*16       array of DIMENSION at least
c           ( ( n*( n + 1 ) )/2 ).
c           Before entry with  UPLO = 'U' or 'u', the array AP must
c           contain the upper triangular part of the hermitian matrix
c           packed sequentially, column by column, so that AP( 1 )
c           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
c           and a( 2, 2 ) respectively, and so on. On exit, the array
c           AP is overwritten by the upper triangular part of the
c           updated matrix.
c           Before entry with UPLO = 'L' or 'l', the array AP must
c           contain the lower triangular part of the hermitian matrix
c           packed sequentially, column by column, so that AP( 1 )
c           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
c           and a( 3, 1 ) respectively, and so on. On exit, the array
c           AP is overwritten by the lower triangular part of the
c           updated matrix.
c           Note that the imaginary parts of the diagonal elements need
c           not be set, they are assumed to be zero, and on exit they
c           are set to zero.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, K, KK, KX, KY
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, DBLE
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO, 'U' ).AND.
     $         .NOT.LSAME( UPLO, 'L' )      )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZHPR2 ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
c
c     Set up the start points in X and Y if the increments are not both
c     unity.
c
      IF( ( INCX.NE.1 ).OR.( INCY.NE.1 ) )THEN
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( N - 1 )*INCX
         END IF
         IF( INCY.GT.0 )THEN
            KY = 1
         ELSE
            KY = 1 - ( N - 1 )*INCY
         END IF
         JX = KX
         JY = KY
      END IF
c
c     Start the operations. In this version the elements of the array AP
c     are accessed sequentially with one pass through AP.
c
      KK = 1
      IF( LSAME( UPLO, 'U' ) )THEN
c
c        Form  A  when upper triangle is stored in AP.
c
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 20, J = 1, N
               IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*DCONJG( Y( J ) )
                  TEMP2 = DCONJG( ALPHA*X( J ) )
                  K     = KK
                  DO 10, I = 1, J - 1
                     AP( K ) = AP( K ) + X( I )*TEMP1 + Y( I )*TEMP2
                     K       = K       + 1
   10             CONTINUE
                  AP( KK + J - 1 ) = DBLE( AP( KK + J - 1 ) ) +
     $                               DBLE( X( J )*TEMP1 + Y( J )*TEMP2 )
               ELSE
                  AP( KK + J - 1 ) = DBLE( AP( KK + J - 1 ) )
               END IF
               KK = KK + J
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*DCONJG( Y( JY ) )
                  TEMP2 = DCONJG( ALPHA*X( JX ) )
                  IX    = KX
                  IY    = KY
                  DO 30, K = KK, KK + J - 2
                     AP( K ) = AP( K ) + X( IX )*TEMP1 + Y( IY )*TEMP2
                     IX      = IX      + INCX
                     IY      = IY      + INCY
   30             CONTINUE
                  AP( KK + J - 1 ) = DBLE( AP( KK + J - 1 ) ) +
     $                               DBLE( X( JX )*TEMP1 +
     $                                     Y( JY )*TEMP2 )
               ELSE
                  AP( KK + J - 1 ) = DBLE( AP( KK + J - 1 ) )
               END IF
               JX = JX + INCX
               JY = JY + INCY
               KK = KK + J
   40       CONTINUE
         END IF
      ELSE
c
c        Form  A  when lower triangle is stored in AP.
c
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 60, J = 1, N
               IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
                  TEMP1   = ALPHA*DCONJG( Y( J ) )
                  TEMP2   = DCONJG( ALPHA*X( J ) )
                  AP( KK ) = DBLE( AP( KK ) ) +
     $                       DBLE( X( J )*TEMP1 + Y( J )*TEMP2 )
                  K        = KK               + 1
                  DO 50, I = J + 1, N
                     AP( K ) = AP( K ) + X( I )*TEMP1 + Y( I )*TEMP2
                     K       = K       + 1
   50             CONTINUE
               ELSE
                  AP( KK ) = DBLE( AP( KK ) )
               END IF
               KK = KK + N - J + 1
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1    = ALPHA*DCONJG( Y( JY ) )
                  TEMP2    = DCONJG( ALPHA*X( JX ) )
                  AP( KK ) = DBLE( AP( KK ) ) +
     $                       DBLE( X( JX )*TEMP1 + Y( JY )*TEMP2 )
                  IX       = JX
                  IY       = JY
                  DO 70, K = KK + 1, KK + N - J
                     IX      = IX      + INCX
                     IY      = IY      + INCY
                     AP( K ) = AP( K ) + X( IX )*TEMP1 + Y( IY )*TEMP2
   70             CONTINUE
               ELSE
                  AP( KK ) = DBLE( AP( KK ) )
               END IF
               JX = JX + INCX
               JY = JY + INCY
               KK = KK + N - J + 1
   80       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of ZHPR2 .
c
      END
      SUBROUTINE ZHPR  ( UPLO, N, ALPHA, X, INCX, AP )
c     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      COMPLEX*16         AP( * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  ZHPR    performs the hermitian rank 1 operation
c
c     A := alpha*x*conjg( x' ) + A,
c
c  where alpha is a real scalar, x is an n element vector and A is an
c  n by n hermitian matrix, supplied in packed form.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the upper or lower
c           triangular part of the matrix A is supplied in the packed
c           array AP as follows:
c
c              UPLO = 'U' or 'u'   The upper triangular part of A is
c                                  supplied in AP.
c
c              UPLO = 'L' or 'l'   The lower triangular part of A is
c                                  supplied in AP.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  ALPHA  - DOUBLE PRECISION.
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element vector x.
c           Unchanged on exit.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c  AP     - COMPLEX*16       array of DIMENSION at least
c           ( ( n*( n + 1 ) )/2 ).
c           Before entry with  UPLO = 'U' or 'u', the array AP must
c           contain the upper triangular part of the hermitian matrix
c           packed sequentially, column by column, so that AP( 1 )
c           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
c           and a( 2, 2 ) respectively, and so on. On exit, the array
c           AP is overwritten by the upper triangular part of the
c           updated matrix.
c           Before entry with UPLO = 'L' or 'l', the array AP must
c           contain the lower triangular part of the hermitian matrix
c           packed sequentially, column by column, so that AP( 1 )
c           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
c           and a( 3, 1 ) respectively, and so on. On exit, the array
c           AP is overwritten by the lower triangular part of the
c           updated matrix.
c           Note that the imaginary parts of the diagonal elements need
c           not be set, they are assumed to be zero, and on exit they
c           are set to zero.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, J, JX, K, KK, KX
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, DBLE
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO, 'U' ).AND.
     $         .NOT.LSAME( UPLO, 'L' )      )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZHPR  ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( N.EQ.0 ).OR.( ALPHA.EQ.DBLE( ZERO ) ) )
     $   RETURN
c
c     Set the start point in X if the increment is not unity.
c
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
c
c     Start the operations. In this version the elements of the array AP
c     are accessed sequentially with one pass through AP.
c
      KK = 1
      IF( LSAME( UPLO, 'U' ) )THEN
c
c        Form  A  when upper triangle is stored in AP.
c
         IF( INCX.EQ.1 )THEN
            DO 20, J = 1, N
               IF( X( J ).NE.ZERO )THEN
                  TEMP = ALPHA*DCONJG( X( J ) )
                  K    = KK
                  DO 10, I = 1, J - 1
                     AP( K ) = AP( K ) + X( I )*TEMP
                     K       = K       + 1
   10             CONTINUE
                  AP( KK + J - 1 ) = DBLE( AP( KK + J - 1 ) )
     $                               + DBLE( X( J )*TEMP )
               ELSE
                  AP( KK + J - 1 ) = DBLE( AP( KK + J - 1 ) )
               END IF
               KK = KK + J
   20       CONTINUE
         ELSE
            JX = KX
            DO 40, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*DCONJG( X( JX ) )
                  IX   = KX
                  DO 30, K = KK, KK + J - 2
                     AP( K ) = AP( K ) + X( IX )*TEMP
                     IX      = IX      + INCX
   30             CONTINUE
                  AP( KK + J - 1 ) = DBLE( AP( KK + J - 1 ) )
     $                               + DBLE( X( JX )*TEMP )
               ELSE
                  AP( KK + J - 1 ) = DBLE( AP( KK + J - 1 ) )
               END IF
               JX = JX + INCX
               KK = KK + J
   40       CONTINUE
         END IF
      ELSE
c
c        Form  A  when lower triangle is stored in AP.
c
         IF( INCX.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( J ).NE.ZERO )THEN
                  TEMP     = ALPHA*DCONJG( X( J ) )
                  AP( KK ) = DBLE( AP( KK ) ) + DBLE( TEMP*X( J ) )
                  K        = KK               + 1
                  DO 50, I = J + 1, N
                     AP( K ) = AP( K ) + X( I )*TEMP
                     K       = K       + 1
   50             CONTINUE
               ELSE
                  AP( KK ) = DBLE( AP( KK ) )
               END IF
               KK = KK + N - J + 1
   60       CONTINUE
         ELSE
            JX = KX
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP    = ALPHA*DCONJG( X( JX ) )
                  AP( KK ) = DBLE( AP( KK ) ) + DBLE( TEMP*X( JX ) )
                  IX      = JX
                  DO 70, K = KK + 1, KK + N - J
                     IX      = IX      + INCX
                     AP( K ) = AP( K ) + X( IX )*TEMP
   70             CONTINUE
               ELSE
                  AP( KK ) = DBLE( AP( KK ) )
               END IF
               JX = JX + INCX
               KK = KK + N - J + 1
   80       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of ZHPR  .
c
      END
      SUBROUTINE ZTBMV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
c     .. Scalar Arguments ..
      INTEGER            INCX, K, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
c     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  ZTBMV  performs one of the matrix-vector operations
c
c     x := A*x,   or   x := A'*x,   or   x := conjg( A' )*x,
c
c  where x is an n element vector and  A is an n by n unit, or non-unit,
c  upper or lower triangular band matrix, with ( k + 1 ) diagonals.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the matrix is an upper or
c           lower triangular matrix as follows:
c
c              UPLO = 'U' or 'u'   A is an upper triangular matrix.
c
c              UPLO = 'L' or 'l'   A is a lower triangular matrix.
c
c           Unchanged on exit.
c
c  TRANS  - CHARACTER*1.
c           On entry, TRANS specifies the operation to be performed as
c           follows:
c
c              TRANS = 'N' or 'n'   x := A*x.
c
c              TRANS = 'T' or 't'   x := A'*x.
c
c              TRANS = 'C' or 'c'   x := conjg( A' )*x.
c
c           Unchanged on exit.
c
c  DIAG   - CHARACTER*1.
c           On entry, DIAG specifies whether or not A is unit
c           triangular as follows:
c
c              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
c
c              DIAG = 'N' or 'n'   A is not assumed to be unit
c                                  triangular.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  K      - INTEGER.
c           On entry with UPLO = 'U' or 'u', K specifies the number of
c           super-diagonals of the matrix A.
c           On entry with UPLO = 'L' or 'l', K specifies the number of
c           sub-diagonals of the matrix A.
c           K must satisfy  0 .le. K.
c           Unchanged on exit.
c
c  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
c           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
c           by n part of the array A must contain the upper triangular
c           band part of the matrix of coefficients, supplied column by
c           column, with the leading diagonal of the matrix in row
c           ( k + 1 ) of the array, the first super-diagonal starting at
c           position 2 in row k, and so on. The top left k by k triangle
c           of the array A is not referenced.
c           The following program segment will transfer an upper
c           triangular band matrix from conventional full matrix storage
c           to band storage:
c
c                 DO 20, J = 1, N
c                    M = K + 1 - J
c                    DO 10, I = MAX( 1, J - K ), J
c                       A( M + I, J ) = matrix( I, J )
c              10    CONTINUE
c              20 CONTINUE
c
c           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
c           by n part of the array A must contain the lower triangular
c           band part of the matrix of coefficients, supplied column by
c           column, with the leading diagonal of the matrix in row 1 of
c           the array, the first sub-diagonal starting at position 1 in
c           row 2, and so on. The bottom right k by k triangle of the
c           array A is not referenced.
c           The following program segment will transfer a lower
c           triangular band matrix from conventional full matrix storage
c           to band storage:
c
c                 DO 20, J = 1, N
c                    M = 1 - J
c                    DO 10, I = J, MIN( N, J + K )
c                       A( M + I, J ) = matrix( I, J )
c              10    CONTINUE
c              20 CONTINUE
c
c           Note that when DIAG = 'U' or 'u' the elements of the array A
c           corresponding to the diagonal elements of the matrix are not
c           referenced, but are assumed to be unity.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           ( k + 1 ).
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element vector x. On exit, X is overwritten with the
c           tranformed vector x.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, J, JX, KPLUS1, KX, L
      LOGICAL            NOCONJ, NOUNIT
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX, MIN
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     $         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     $         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( K.LT.0 )THEN
         INFO = 5
      ELSE IF( LDA.LT.( K + 1 ) )THEN
         INFO = 7
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZTBMV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
      NOCONJ = LSAME( TRANS, 'T' )
      NOUNIT = LSAME( DIAG , 'N' )
c
c     Set up the start point in X if the increment is not unity. This
c     will be  ( N - 1 )*INCX   too small for descending loops.
c
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
c
c     Start the operations. In this version the elements of A are
c     accessed sequentially with one pass through A.
c
      IF( LSAME( TRANS, 'N' ) )THEN
c
c         Form  x := A*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            KPLUS1 = K + 1
            IF( INCX.EQ.1 )THEN
               DO 20, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     L    = KPLUS1 - J
                     DO 10, I = MAX( 1, J - K ), J - 1
                        X( I ) = X( I ) + TEMP*A( L + I, J )
   10                CONTINUE
                     IF( NOUNIT )
     $                  X( J ) = X( J )*A( KPLUS1, J )
                  END IF
   20          CONTINUE
            ELSE
               JX = KX
               DO 40, J = 1, N
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     L    = KPLUS1  - J
                     DO 30, I = MAX( 1, J - K ), J - 1
                        X( IX ) = X( IX ) + TEMP*A( L + I, J )
                        IX      = IX      + INCX
   30                CONTINUE
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )*A( KPLUS1, J )
                  END IF
                  JX = JX + INCX
                  IF( J.GT.K )
     $               KX = KX + INCX
   40          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 60, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     L    = 1      - J
                     DO 50, I = MIN( N, J + K ), J + 1, -1
                        X( I ) = X( I ) + TEMP*A( L + I, J )
   50                CONTINUE
                     IF( NOUNIT )
     $                  X( J ) = X( J )*A( 1, J )
                  END IF
   60          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 80, J = N, 1, -1
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     L    = 1       - J
                     DO 70, I = MIN( N, J + K ), J + 1, -1
                        X( IX ) = X( IX ) + TEMP*A( L + I, J )
                        IX      = IX      - INCX
   70                CONTINUE
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )*A( 1, J )
                  END IF
                  JX = JX - INCX
                  IF( ( N - J ).GE.K )
     $               KX = KX - INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
c
c        Form  x := A'*x  or  x := conjg( A' )*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            KPLUS1 = K + 1
            IF( INCX.EQ.1 )THEN
               DO 110, J = N, 1, -1
                  TEMP = X( J )
                  L    = KPLUS1 - J
                  IF( NOCONJ )THEN
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( KPLUS1, J )
                     DO 90, I = J - 1, MAX( 1, J - K ), -1
                        TEMP = TEMP + A( L + I, J )*X( I )
   90                CONTINUE
                  ELSE
                     IF( NOUNIT )
     $                  TEMP = TEMP*DCONJG( A( KPLUS1, J ) )
                     DO 100, I = J - 1, MAX( 1, J - K ), -1
                        TEMP = TEMP + DCONJG( A( L + I, J ) )*X( I )
  100                CONTINUE
                  END IF
                  X( J ) = TEMP
  110          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 140, J = N, 1, -1
                  TEMP = X( JX )
                  KX   = KX      - INCX
                  IX   = KX
                  L    = KPLUS1  - J
                  IF( NOCONJ )THEN
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( KPLUS1, J )
                     DO 120, I = J - 1, MAX( 1, J - K ), -1
                        TEMP = TEMP + A( L + I, J )*X( IX )
                        IX   = IX   - INCX
  120                CONTINUE
                  ELSE
                     IF( NOUNIT )
     $                  TEMP = TEMP*DCONJG( A( KPLUS1, J ) )
                     DO 130, I = J - 1, MAX( 1, J - K ), -1
                        TEMP = TEMP + DCONJG( A( L + I, J ) )*X( IX )
                        IX   = IX   - INCX
  130                CONTINUE
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   - INCX
  140          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 170, J = 1, N
                  TEMP = X( J )
                  L    = 1      - J
                  IF( NOCONJ )THEN
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( 1, J )
                     DO 150, I = J + 1, MIN( N, J + K )
                        TEMP = TEMP + A( L + I, J )*X( I )
  150                CONTINUE
                  ELSE
                     IF( NOUNIT )
     $                  TEMP = TEMP*DCONJG( A( 1, J ) )
                     DO 160, I = J + 1, MIN( N, J + K )
                        TEMP = TEMP + DCONJG( A( L + I, J ) )*X( I )
  160                CONTINUE
                  END IF
                  X( J ) = TEMP
  170          CONTINUE
            ELSE
               JX = KX
               DO 200, J = 1, N
                  TEMP = X( JX )
                  KX   = KX      + INCX
                  IX   = KX
                  L    = 1       - J
                  IF( NOCONJ )THEN
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( 1, J )
                     DO 180, I = J + 1, MIN( N, J + K )
                        TEMP = TEMP + A( L + I, J )*X( IX )
                        IX   = IX   + INCX
  180                CONTINUE
                  ELSE
                     IF( NOUNIT )
     $                  TEMP = TEMP*DCONJG( A( 1, J ) )
                     DO 190, I = J + 1, MIN( N, J + K )
                        TEMP = TEMP + DCONJG( A( L + I, J ) )*X( IX )
                        IX   = IX   + INCX
  190                CONTINUE
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   + INCX
  200          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of ZTBMV .
c
      END
      SUBROUTINE ZTBSV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
c     .. Scalar Arguments ..
      INTEGER            INCX, K, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
c     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  ZTBSV  solves one of the systems of equations
c
c     A*x = b,   or   A'*x = b,   or   conjg( A' )*x = b,
c
c  where b and x are n element vectors and A is an n by n unit, or
c  non-unit, upper or lower triangular band matrix, with ( k + 1 )
c  diagonals.
c
c  No test for singularity or near-singularity is included in this
c  routine. Such tests must be performed before calling this routine.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the matrix is an upper or
c           lower triangular matrix as follows:
c
c              UPLO = 'U' or 'u'   A is an upper triangular matrix.
c
c              UPLO = 'L' or 'l'   A is a lower triangular matrix.
c
c           Unchanged on exit.
c
c  TRANS  - CHARACTER*1.
c           On entry, TRANS specifies the equations to be solved as
c           follows:
c
c              TRANS = 'N' or 'n'   A*x = b.
c
c              TRANS = 'T' or 't'   A'*x = b.
c
c              TRANS = 'C' or 'c'   conjg( A' )*x = b.
c
c           Unchanged on exit.
c
c  DIAG   - CHARACTER*1.
c           On entry, DIAG specifies whether or not A is unit
c           triangular as follows:
c
c              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
c
c              DIAG = 'N' or 'n'   A is not assumed to be unit
c                                  triangular.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  K      - INTEGER.
c           On entry with UPLO = 'U' or 'u', K specifies the number of
c           super-diagonals of the matrix A.
c           On entry with UPLO = 'L' or 'l', K specifies the number of
c           sub-diagonals of the matrix A.
c           K must satisfy  0 .le. K.
c           Unchanged on exit.
c
c  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
c           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
c           by n part of the array A must contain the upper triangular
c           band part of the matrix of coefficients, supplied column by
c           column, with the leading diagonal of the matrix in row
c           ( k + 1 ) of the array, the first super-diagonal starting at
c           position 2 in row k, and so on. The top left k by k triangle
c           of the array A is not referenced.
c           The following program segment will transfer an upper
c           triangular band matrix from conventional full matrix storage
c           to band storage:
c
c                 DO 20, J = 1, N
c                    M = K + 1 - J
c                    DO 10, I = MAX( 1, J - K ), J
c                       A( M + I, J ) = matrix( I, J )
c              10    CONTINUE
c              20 CONTINUE
c
c           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
c           by n part of the array A must contain the lower triangular
c           band part of the matrix of coefficients, supplied column by
c           column, with the leading diagonal of the matrix in row 1 of
c           the array, the first sub-diagonal starting at position 1 in
c           row 2, and so on. The bottom right k by k triangle of the
c           array A is not referenced.
c           The following program segment will transfer a lower
c           triangular band matrix from conventional full matrix storage
c           to band storage:
c
c                 DO 20, J = 1, N
c                    M = 1 - J
c                    DO 10, I = J, MIN( N, J + K )
c                       A( M + I, J ) = matrix( I, J )
c              10    CONTINUE
c              20 CONTINUE
c
c           Note that when DIAG = 'U' or 'u' the elements of the array A
c           corresponding to the diagonal elements of the matrix are not
c           referenced, but are assumed to be unity.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           ( k + 1 ).
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element right-hand side vector b. On exit, X is overwritten
c           with the solution vector x.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, J, JX, KPLUS1, KX, L
      LOGICAL            NOCONJ, NOUNIT
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX, MIN
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     $         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     $         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( K.LT.0 )THEN
         INFO = 5
      ELSE IF( LDA.LT.( K + 1 ) )THEN
         INFO = 7
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZTBSV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
      NOCONJ = LSAME( TRANS, 'T' )
      NOUNIT = LSAME( DIAG , 'N' )
c
c     Set up the start point in X if the increment is not unity. This
c     will be  ( N - 1 )*INCX  too small for descending loops.
c
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
c
c     Start the operations. In this version the elements of A are
c     accessed by sequentially with one pass through A.
c
      IF( LSAME( TRANS, 'N' ) )THEN
c
c        Form  x := inv( A )*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            KPLUS1 = K + 1
            IF( INCX.EQ.1 )THEN
               DO 20, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     L = KPLUS1 - J
                     IF( NOUNIT )
     $                  X( J ) = X( J )/A( KPLUS1, J )
                     TEMP = X( J )
                     DO 10, I = J - 1, MAX( 1, J - K ), -1
                        X( I ) = X( I ) - TEMP*A( L + I, J )
   10                CONTINUE
                  END IF
   20          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 40, J = N, 1, -1
                  KX = KX - INCX
                  IF( X( JX ).NE.ZERO )THEN
                     IX = KX
                     L  = KPLUS1 - J
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )/A( KPLUS1, J )
                     TEMP = X( JX )
                     DO 30, I = J - 1, MAX( 1, J - K ), -1
                        X( IX ) = X( IX ) - TEMP*A( L + I, J )
                        IX      = IX      - INCX
   30                CONTINUE
                  END IF
                  JX = JX - INCX
   40          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 60, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     L = 1 - J
                     IF( NOUNIT )
     $                  X( J ) = X( J )/A( 1, J )
                     TEMP = X( J )
                     DO 50, I = J + 1, MIN( N, J + K )
                        X( I ) = X( I ) - TEMP*A( L + I, J )
   50                CONTINUE
                  END IF
   60          CONTINUE
            ELSE
               JX = KX
               DO 80, J = 1, N
                  KX = KX + INCX
                  IF( X( JX ).NE.ZERO )THEN
                     IX = KX
                     L  = 1  - J
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )/A( 1, J )
                     TEMP = X( JX )
                     DO 70, I = J + 1, MIN( N, J + K )
                        X( IX ) = X( IX ) - TEMP*A( L + I, J )
                        IX      = IX      + INCX
   70                CONTINUE
                  END IF
                  JX = JX + INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
c
c        Form  x := inv( A' )*x  or  x := inv( conjg( A') )*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            KPLUS1 = K + 1
            IF( INCX.EQ.1 )THEN
               DO 110, J = 1, N
                  TEMP = X( J )
                  L    = KPLUS1 - J
                  IF( NOCONJ )THEN
                     DO 90, I = MAX( 1, J - K ), J - 1
                        TEMP = TEMP - A( L + I, J )*X( I )
   90                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( KPLUS1, J )
                  ELSE
                     DO 100, I = MAX( 1, J - K ), J - 1
                        TEMP = TEMP - DCONJG( A( L + I, J ) )*X( I )
  100                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( A( KPLUS1, J ) )
                  END IF
                  X( J ) = TEMP
  110          CONTINUE
            ELSE
               JX = KX
               DO 140, J = 1, N
                  TEMP = X( JX )
                  IX   = KX
                  L    = KPLUS1  - J
                  IF( NOCONJ )THEN
                     DO 120, I = MAX( 1, J - K ), J - 1
                        TEMP = TEMP - A( L + I, J )*X( IX )
                        IX   = IX   + INCX
  120                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( KPLUS1, J )
                  ELSE
                     DO 130, I = MAX( 1, J - K ), J - 1
                        TEMP = TEMP - DCONJG( A( L + I, J ) )*X( IX )
                        IX   = IX   + INCX
  130                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( A( KPLUS1, J ) )
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   + INCX
                  IF( J.GT.K )
     $               KX = KX + INCX
  140          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 170, J = N, 1, -1
                  TEMP = X( J )
                  L    = 1      - J
                  IF( NOCONJ )THEN
                     DO 150, I = MIN( N, J + K ), J + 1, -1
                        TEMP = TEMP - A( L + I, J )*X( I )
  150                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( 1, J )
                  ELSE
                     DO 160, I = MIN( N, J + K ), J + 1, -1
                        TEMP = TEMP - DCONJG( A( L + I, J ) )*X( I )
  160                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( A( 1, J ) )
                  END IF
                  X( J ) = TEMP
  170          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 200, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = KX
                  L    = 1       - J
                  IF( NOCONJ )THEN
                     DO 180, I = MIN( N, J + K ), J + 1, -1
                        TEMP = TEMP - A( L + I, J )*X( IX )
                        IX   = IX   - INCX
  180                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( 1, J )
                  ELSE
                     DO 190, I = MIN( N, J + K ), J + 1, -1
                        TEMP = TEMP - DCONJG( A( L + I, J ) )*X( IX )
                        IX   = IX   - INCX
  190                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( A( 1, J ) )
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   - INCX
                  IF( ( N - J ).GE.K )
     $               KX = KX - INCX
  200          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of ZTBSV .
c
      END
      SUBROUTINE ZTPMV ( UPLO, TRANS, DIAG, N, AP, X, INCX )
c     .. Scalar Arguments ..
      INTEGER            INCX, N
      CHARACTER*1        DIAG, TRANS, UPLO
c     .. Array Arguments ..
      COMPLEX*16         AP( * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  ZTPMV  performs one of the matrix-vector operations
c
c     x := A*x,   or   x := A'*x,   or   x := conjg( A' )*x,
c
c  where x is an n element vector and  A is an n by n unit, or non-unit,
c  upper or lower triangular matrix, supplied in packed form.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the matrix is an upper or
c           lower triangular matrix as follows:
c
c              UPLO = 'U' or 'u'   A is an upper triangular matrix.
c
c              UPLO = 'L' or 'l'   A is a lower triangular matrix.
c
c           Unchanged on exit.
c
c  TRANS  - CHARACTER*1.
c           On entry, TRANS specifies the operation to be performed as
c           follows:
c
c              TRANS = 'N' or 'n'   x := A*x.
c
c              TRANS = 'T' or 't'   x := A'*x.
c
c              TRANS = 'C' or 'c'   x := conjg( A' )*x.
c
c           Unchanged on exit.
c
c  DIAG   - CHARACTER*1.
c           On entry, DIAG specifies whether or not A is unit
c           triangular as follows:
c
c              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
c
c              DIAG = 'N' or 'n'   A is not assumed to be unit
c                                  triangular.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  AP     - COMPLEX*16       array of DIMENSION at least
c           ( ( n*( n + 1 ) )/2 ).
c           Before entry with  UPLO = 'U' or 'u', the array AP must
c           contain the upper triangular matrix packed sequentially,
c           column by column, so that AP( 1 ) contains a( 1, 1 ),
c           AP( 2 ) and AP( 3 ) contain a( 1, 2 ) and a( 2, 2 )
c           respectively, and so on.
c           Before entry with UPLO = 'L' or 'l', the array AP must
c           contain the lower triangular matrix packed sequentially,
c           column by column, so that AP( 1 ) contains a( 1, 1 ),
c           AP( 2 ) and AP( 3 ) contain a( 2, 1 ) and a( 3, 1 )
c           respectively, and so on.
c           Note that when  DIAG = 'U' or 'u', the diagonal elements of
c           A are not referenced, but are assumed to be unity.
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element vector x. On exit, X is overwritten with the
c           tranformed vector x.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, J, JX, K, KK, KX
      LOGICAL            NOCONJ, NOUNIT
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     $         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     $         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 7
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZTPMV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
      NOCONJ = LSAME( TRANS, 'T' )
      NOUNIT = LSAME( DIAG , 'N' )
c
c     Set up the start point in X if the increment is not unity. This
c     will be  ( N - 1 )*INCX  too small for descending loops.
c
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
c
c     Start the operations. In this version the elements of AP are
c     accessed sequentially with one pass through AP.
c
      IF( LSAME( TRANS, 'N' ) )THEN
c
c        Form  x:= A*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            KK = 1
            IF( INCX.EQ.1 )THEN
               DO 20, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     K    = KK
                     DO 10, I = 1, J - 1
                        X( I ) = X( I ) + TEMP*AP( K )
                        K      = K      + 1
   10                CONTINUE
                     IF( NOUNIT )
     $                  X( J ) = X( J )*AP( KK + J - 1 )
                  END IF
                  KK = KK + J
   20          CONTINUE
            ELSE
               JX = KX
               DO 40, J = 1, N
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 30, K = KK, KK + J - 2
                        X( IX ) = X( IX ) + TEMP*AP( K )
                        IX      = IX      + INCX
   30                CONTINUE
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )*AP( KK + J - 1 )
                  END IF
                  JX = JX + INCX
                  KK = KK + J
   40          CONTINUE
            END IF
         ELSE
            KK = ( N*( N + 1 ) )/2
            IF( INCX.EQ.1 )THEN
               DO 60, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     K    = KK
                     DO 50, I = N, J + 1, -1
                        X( I ) = X( I ) + TEMP*AP( K )
                        K      = K      - 1
   50                CONTINUE
                     IF( NOUNIT )
     $                  X( J ) = X( J )*AP( KK - N + J )
                  END IF
                  KK = KK - ( N - J + 1 )
   60          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 80, J = N, 1, -1
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 70, K = KK, KK - ( N - ( J + 1 ) ), -1
                        X( IX ) = X( IX ) + TEMP*AP( K )
                        IX      = IX      - INCX
   70                CONTINUE
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )*AP( KK - N + J )
                  END IF
                  JX = JX - INCX
                  KK = KK - ( N - J + 1 )
   80          CONTINUE
            END IF
         END IF
      ELSE
c
c        Form  x := A'*x  or  x := conjg( A' )*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            KK = ( N*( N + 1 ) )/2
            IF( INCX.EQ.1 )THEN
               DO 110, J = N, 1, -1
                  TEMP = X( J )
                  K    = KK     - 1
                  IF( NOCONJ )THEN
                     IF( NOUNIT )
     $                  TEMP = TEMP*AP( KK )
                     DO 90, I = J - 1, 1, -1
                        TEMP = TEMP + AP( K )*X( I )
                        K    = K    - 1
   90                CONTINUE
                  ELSE
                     IF( NOUNIT )
     $                  TEMP = TEMP*DCONJG( AP( KK ) )
                     DO 100, I = J - 1, 1, -1
                        TEMP = TEMP + DCONJG( AP( K ) )*X( I )
                        K    = K    - 1
  100                CONTINUE
                  END IF
                  X( J ) = TEMP
                  KK     = KK   - J
  110          CONTINUE
            ELSE
               JX = KX + ( N - 1 )*INCX
               DO 140, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOCONJ )THEN
                     IF( NOUNIT )
     $                  TEMP = TEMP*AP( KK )
                     DO 120, K = KK - 1, KK - J + 1, -1
                        IX   = IX   - INCX
                        TEMP = TEMP + AP( K )*X( IX )
  120                CONTINUE
                  ELSE
                     IF( NOUNIT )
     $                  TEMP = TEMP*DCONJG( AP( KK ) )
                     DO 130, K = KK - 1, KK - J + 1, -1
                        IX   = IX   - INCX
                        TEMP = TEMP + DCONJG( AP( K ) )*X( IX )
  130                CONTINUE
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   - INCX
                  KK      = KK   - J
  140          CONTINUE
            END IF
         ELSE
            KK = 1
            IF( INCX.EQ.1 )THEN
               DO 170, J = 1, N
                  TEMP = X( J )
                  K    = KK     + 1
                  IF( NOCONJ )THEN
                     IF( NOUNIT )
     $                  TEMP = TEMP*AP( KK )
                     DO 150, I = J + 1, N
                        TEMP = TEMP + AP( K )*X( I )
                        K    = K    + 1
  150                CONTINUE
                  ELSE
                     IF( NOUNIT )
     $                  TEMP = TEMP*DCONJG( AP( KK ) )
                     DO 160, I = J + 1, N
                        TEMP = TEMP + DCONJG( AP( K ) )*X( I )
                        K    = K    + 1
  160                CONTINUE
                  END IF
                  X( J ) = TEMP
                  KK     = KK   + ( N - J + 1 )
  170          CONTINUE
            ELSE
               JX = KX
               DO 200, J = 1, N
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOCONJ )THEN
                     IF( NOUNIT )
     $                  TEMP = TEMP*AP( KK )
                     DO 180, K = KK + 1, KK + N - J
                        IX   = IX   + INCX
                        TEMP = TEMP + AP( K )*X( IX )
  180                CONTINUE
                  ELSE
                     IF( NOUNIT )
     $                  TEMP = TEMP*DCONJG( AP( KK ) )
                     DO 190, K = KK + 1, KK + N - J
                        IX   = IX   + INCX
                        TEMP = TEMP + DCONJG( AP( K ) )*X( IX )
  190                CONTINUE
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   + INCX
                  KK      = KK   + ( N - J + 1 )
  200          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of ZTPMV .
c
      END
      SUBROUTINE ZTPSV ( UPLO, TRANS, DIAG, N, AP, X, INCX )
c     .. Scalar Arguments ..
      INTEGER            INCX, N
      CHARACTER*1        DIAG, TRANS, UPLO
c     .. Array Arguments ..
      COMPLEX*16         AP( * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  ZTPSV  solves one of the systems of equations
c
c     A*x = b,   or   A'*x = b,   or   conjg( A' )*x = b,
c
c  where b and x are n element vectors and A is an n by n unit, or
c  non-unit, upper or lower triangular matrix, supplied in packed form.
c
c  No test for singularity or near-singularity is included in this
c  routine. Such tests must be performed before calling this routine.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the matrix is an upper or
c           lower triangular matrix as follows:
c
c              UPLO = 'U' or 'u'   A is an upper triangular matrix.
c
c              UPLO = 'L' or 'l'   A is a lower triangular matrix.
c
c           Unchanged on exit.
c
c  TRANS  - CHARACTER*1.
c           On entry, TRANS specifies the equations to be solved as
c           follows:
c
c              TRANS = 'N' or 'n'   A*x = b.
c
c              TRANS = 'T' or 't'   A'*x = b.
c
c              TRANS = 'C' or 'c'   conjg( A' )*x = b.
c
c           Unchanged on exit.
c
c  DIAG   - CHARACTER*1.
c           On entry, DIAG specifies whether or not A is unit
c           triangular as follows:
c
c              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
c
c              DIAG = 'N' or 'n'   A is not assumed to be unit
c                                  triangular.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  AP     - COMPLEX*16       array of DIMENSION at least
c           ( ( n*( n + 1 ) )/2 ).
c           Before entry with  UPLO = 'U' or 'u', the array AP must
c           contain the upper triangular matrix packed sequentially,
c           column by column, so that AP( 1 ) contains a( 1, 1 ),
c           AP( 2 ) and AP( 3 ) contain a( 1, 2 ) and a( 2, 2 )
c           respectively, and so on.
c           Before entry with UPLO = 'L' or 'l', the array AP must
c           contain the lower triangular matrix packed sequentially,
c           column by column, so that AP( 1 ) contains a( 1, 1 ),
c           AP( 2 ) and AP( 3 ) contain a( 2, 1 ) and a( 3, 1 )
c           respectively, and so on.
c           Note that when  DIAG = 'U' or 'u', the diagonal elements of
c           A are not referenced, but are assumed to be unity.
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element right-hand side vector b. On exit, X is overwritten
c           with the solution vector x.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, J, JX, K, KK, KX
      LOGICAL            NOCONJ, NOUNIT
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     $         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     $         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 7
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZTPSV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
      NOCONJ = LSAME( TRANS, 'T' )
      NOUNIT = LSAME( DIAG , 'N' )
c
c     Set up the start point in X if the increment is not unity. This
c     will be  ( N - 1 )*INCX  too small for descending loops.
c
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
c
c     Start the operations. In this version the elements of AP are
c     accessed sequentially with one pass through AP.
c
      IF( LSAME( TRANS, 'N' ) )THEN
c
c        Form  x := inv( A )*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            KK = ( N*( N + 1 ) )/2
            IF( INCX.EQ.1 )THEN
               DO 20, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     IF( NOUNIT )
     $                  X( J ) = X( J )/AP( KK )
                     TEMP = X( J )
                     K    = KK     - 1
                     DO 10, I = J - 1, 1, -1
                        X( I ) = X( I ) - TEMP*AP( K )
                        K      = K      - 1
   10                CONTINUE
                  END IF
                  KK = KK - J
   20          CONTINUE
            ELSE
               JX = KX + ( N - 1 )*INCX
               DO 40, J = N, 1, -1
                  IF( X( JX ).NE.ZERO )THEN
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )/AP( KK )
                     TEMP = X( JX )
                     IX   = JX
                     DO 30, K = KK - 1, KK - J + 1, -1
                        IX      = IX      - INCX
                        X( IX ) = X( IX ) - TEMP*AP( K )
   30                CONTINUE
                  END IF
                  JX = JX - INCX
                  KK = KK - J
   40          CONTINUE
            END IF
         ELSE
            KK = 1
            IF( INCX.EQ.1 )THEN
               DO 60, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     IF( NOUNIT )
     $                  X( J ) = X( J )/AP( KK )
                     TEMP = X( J )
                     K    = KK     + 1
                     DO 50, I = J + 1, N
                        X( I ) = X( I ) - TEMP*AP( K )
                        K      = K      + 1
   50                CONTINUE
                  END IF
                  KK = KK + ( N - J + 1 )
   60          CONTINUE
            ELSE
               JX = KX
               DO 80, J = 1, N
                  IF( X( JX ).NE.ZERO )THEN
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )/AP( KK )
                     TEMP = X( JX )
                     IX   = JX
                     DO 70, K = KK + 1, KK + N - J
                        IX      = IX      + INCX
                        X( IX ) = X( IX ) - TEMP*AP( K )
   70                CONTINUE
                  END IF
                  JX = JX + INCX
                  KK = KK + ( N - J + 1 )
   80          CONTINUE
            END IF
         END IF
      ELSE
c
c        Form  x := inv( A' )*x  or  x := inv( conjg( A' ) )*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            KK = 1
            IF( INCX.EQ.1 )THEN
               DO 110, J = 1, N
                  TEMP = X( J )
                  K    = KK
                  IF( NOCONJ )THEN
                     DO 90, I = 1, J - 1
                        TEMP = TEMP - AP( K )*X( I )
                        K    = K    + 1
   90                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/AP( KK + J - 1 )
                  ELSE
                     DO 100, I = 1, J - 1
                        TEMP = TEMP - DCONJG( AP( K ) )*X( I )
                        K    = K    + 1
  100                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( AP( KK + J - 1 ) )
                  END IF
                  X( J ) = TEMP
                  KK     = KK   + J
  110          CONTINUE
            ELSE
               JX = KX
               DO 140, J = 1, N
                  TEMP = X( JX )
                  IX   = KX
                  IF( NOCONJ )THEN
                     DO 120, K = KK, KK + J - 2
                        TEMP = TEMP - AP( K )*X( IX )
                        IX   = IX   + INCX
  120                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/AP( KK + J - 1 )
                  ELSE
                     DO 130, K = KK, KK + J - 2
                        TEMP = TEMP - DCONJG( AP( K ) )*X( IX )
                        IX   = IX   + INCX
  130                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( AP( KK + J - 1 ) )
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   + INCX
                  KK      = KK   + J
  140          CONTINUE
            END IF
         ELSE
            KK = ( N*( N + 1 ) )/2
            IF( INCX.EQ.1 )THEN
               DO 170, J = N, 1, -1
                  TEMP = X( J )
                  K    = KK
                  IF( NOCONJ )THEN
                     DO 150, I = N, J + 1, -1
                        TEMP = TEMP - AP( K )*X( I )
                        K    = K    - 1
  150                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/AP( KK - N + J )
                  ELSE
                     DO 160, I = N, J + 1, -1
                        TEMP = TEMP - DCONJG( AP( K ) )*X( I )
                        K    = K    - 1
  160                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( AP( KK - N + J ) )
                  END IF
                  X( J ) = TEMP
                  KK     = KK   - ( N - J + 1 )
  170          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 200, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = KX
                  IF( NOCONJ )THEN
                     DO 180, K = KK, KK - ( N - ( J + 1 ) ), -1
                        TEMP = TEMP - AP( K )*X( IX )
                        IX   = IX   - INCX
  180                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/AP( KK - N + J )
                  ELSE
                     DO 190, K = KK, KK - ( N - ( J + 1 ) ), -1
                        TEMP = TEMP - DCONJG( AP( K ) )*X( IX )
                        IX   = IX   - INCX
  190                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( AP( KK - N + J ) )
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   - INCX
                  KK      = KK   - ( N - J + 1 )
  200          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of ZTPSV .
c
      END
      SUBROUTINE ZTRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
c     .. Scalar Arguments ..
      INTEGER            INCX, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
c     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  ZTRMV  performs one of the matrix-vector operations
c
c     x := A*x,   or   x := A'*x,   or   x := conjg( A' )*x,
c
c  where x is an n element vector and  A is an n by n unit, or non-unit,
c  upper or lower triangular matrix.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the matrix is an upper or
c           lower triangular matrix as follows:
c
c              UPLO = 'U' or 'u'   A is an upper triangular matrix.
c
c              UPLO = 'L' or 'l'   A is a lower triangular matrix.
c
c           Unchanged on exit.
c
c  TRANS  - CHARACTER*1.
c           On entry, TRANS specifies the operation to be performed as
c           follows:
c
c              TRANS = 'N' or 'n'   x := A*x.
c
c              TRANS = 'T' or 't'   x := A'*x.
c
c              TRANS = 'C' or 'c'   x := conjg( A' )*x.
c
c           Unchanged on exit.
c
c  DIAG   - CHARACTER*1.
c           On entry, DIAG specifies whether or not A is unit
c           triangular as follows:
c
c              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
c
c              DIAG = 'N' or 'n'   A is not assumed to be unit
c                                  triangular.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
c           Before entry with  UPLO = 'U' or 'u', the leading n by n
c           upper triangular part of the array A must contain the upper
c           triangular matrix and the strictly lower triangular part of
c           A is not referenced.
c           Before entry with UPLO = 'L' or 'l', the leading n by n
c           lower triangular part of the array A must contain the lower
c           triangular matrix and the strictly upper triangular part of
c           A is not referenced.
c           Note that when  DIAG = 'U' or 'u', the diagonal elements of
c           A are not referenced either, but are assumed to be unity.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           max( 1, n ).
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element vector x. On exit, X is overwritten with the
c           tranformed vector x.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, J, JX, KX
      LOGICAL            NOCONJ, NOUNIT
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     $         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     $         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZTRMV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
      NOCONJ = LSAME( TRANS, 'T' )
      NOUNIT = LSAME( DIAG , 'N' )
c
c     Set up the start point in X if the increment is not unity. This
c     will be  ( N - 1 )*INCX  too small for descending loops.
c
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
c
c     Start the operations. In this version the elements of A are
c     accessed sequentially with one pass through A.
c
      IF( LSAME( TRANS, 'N' ) )THEN
c
c        Form  x := A*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 20, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 10, I = 1, J - 1
                        X( I ) = X( I ) + TEMP*A( I, J )
   10                CONTINUE
                     IF( NOUNIT )
     $                  X( J ) = X( J )*A( J, J )
                  END IF
   20          CONTINUE
            ELSE
               JX = KX
               DO 40, J = 1, N
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 30, I = 1, J - 1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      + INCX
   30                CONTINUE
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX + INCX
   40          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 60, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 50, I = N, J + 1, -1
                        X( I ) = X( I ) + TEMP*A( I, J )
   50                CONTINUE
                     IF( NOUNIT )
     $                  X( J ) = X( J )*A( J, J )
                  END IF
   60          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 80, J = N, 1, -1
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 70, I = N, J + 1, -1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      - INCX
   70                CONTINUE
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX - INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
c
c        Form  x := A'*x  or  x := conjg( A' )*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 110, J = N, 1, -1
                  TEMP = X( J )
                  IF( NOCONJ )THEN
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( J, J )
                     DO 90, I = J - 1, 1, -1
                        TEMP = TEMP + A( I, J )*X( I )
   90                CONTINUE
                  ELSE
                     IF( NOUNIT )
     $                  TEMP = TEMP*DCONJG( A( J, J ) )
                     DO 100, I = J - 1, 1, -1
                        TEMP = TEMP + DCONJG( A( I, J ) )*X( I )
  100                CONTINUE
                  END IF
                  X( J ) = TEMP
  110          CONTINUE
            ELSE
               JX = KX + ( N - 1 )*INCX
               DO 140, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOCONJ )THEN
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( J, J )
                     DO 120, I = J - 1, 1, -1
                        IX   = IX   - INCX
                        TEMP = TEMP + A( I, J )*X( IX )
  120                CONTINUE
                  ELSE
                     IF( NOUNIT )
     $                  TEMP = TEMP*DCONJG( A( J, J ) )
                     DO 130, I = J - 1, 1, -1
                        IX   = IX   - INCX
                        TEMP = TEMP + DCONJG( A( I, J ) )*X( IX )
  130                CONTINUE
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   - INCX
  140          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 170, J = 1, N
                  TEMP = X( J )
                  IF( NOCONJ )THEN
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( J, J )
                     DO 150, I = J + 1, N
                        TEMP = TEMP + A( I, J )*X( I )
  150                CONTINUE
                  ELSE
                     IF( NOUNIT )
     $                  TEMP = TEMP*DCONJG( A( J, J ) )
                     DO 160, I = J + 1, N
                        TEMP = TEMP + DCONJG( A( I, J ) )*X( I )
  160                CONTINUE
                  END IF
                  X( J ) = TEMP
  170          CONTINUE
            ELSE
               JX = KX
               DO 200, J = 1, N
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOCONJ )THEN
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( J, J )
                     DO 180, I = J + 1, N
                        IX   = IX   + INCX
                        TEMP = TEMP + A( I, J )*X( IX )
  180                CONTINUE
                  ELSE
                     IF( NOUNIT )
     $                  TEMP = TEMP*DCONJG( A( J, J ) )
                     DO 190, I = J + 1, N
                        IX   = IX   + INCX
                        TEMP = TEMP + DCONJG( A( I, J ) )*X( IX )
  190                CONTINUE
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   + INCX
  200          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of ZTRMV .
c
      END
      SUBROUTINE ZTRSV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
c     .. Scalar Arguments ..
      INTEGER            INCX, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
c     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  ZTRSV  solves one of the systems of equations
c
c     A*x = b,   or   A'*x = b,   or   conjg( A' )*x = b,
c
c  where b and x are n element vectors and A is an n by n unit, or
c  non-unit, upper or lower triangular matrix.
c
c  No test for singularity or near-singularity is included in this
c  routine. Such tests must be performed before calling this routine.
c
c  Parameters
c  ==========
c
c  UPLO   - CHARACTER*1.
c           On entry, UPLO specifies whether the matrix is an upper or
c           lower triangular matrix as follows:
c
c              UPLO = 'U' or 'u'   A is an upper triangular matrix.
c
c              UPLO = 'L' or 'l'   A is a lower triangular matrix.
c
c           Unchanged on exit.
c
c  TRANS  - CHARACTER*1.
c           On entry, TRANS specifies the equations to be solved as
c           follows:
c
c              TRANS = 'N' or 'n'   A*x = b.
c
c              TRANS = 'T' or 't'   A'*x = b.
c
c              TRANS = 'C' or 'c'   conjg( A' )*x = b.
c
c           Unchanged on exit.
c
c  DIAG   - CHARACTER*1.
c           On entry, DIAG specifies whether or not A is unit
c           triangular as follows:
c
c              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
c
c              DIAG = 'N' or 'n'   A is not assumed to be unit
c                                  triangular.
c
c           Unchanged on exit.
c
c  N      - INTEGER.
c           On entry, N specifies the order of the matrix A.
c           N must be at least zero.
c           Unchanged on exit.
c
c  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
c           Before entry with  UPLO = 'U' or 'u', the leading n by n
c           upper triangular part of the array A must contain the upper
c           triangular matrix and the strictly lower triangular part of
c           A is not referenced.
c           Before entry with UPLO = 'L' or 'l', the leading n by n
c           lower triangular part of the array A must contain the lower
c           triangular matrix and the strictly upper triangular part of
c           A is not referenced.
c           Note that when  DIAG = 'U' or 'u', the diagonal elements of
c           A are not referenced either, but are assumed to be unity.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           max( 1, n ).
c           Unchanged on exit.
c
c  X      - COMPLEX*16       array of dimension at least
c           ( 1 + ( n - 1 )*abs( INCX ) ).
c           Before entry, the incremented array X must contain the n
c           element right-hand side vector b. On exit, X is overwritten
c           with the solution vector x.
c
c  INCX   - INTEGER.
c           On entry, INCX specifies the increment for the elements of
c           X. INCX must not be zero.
c           Unchanged on exit.
c
c
c  Level 2 Blas routine.
c
c  -- Written on 22-October-1986.
c     Jack Dongarra, Argonne National Lab.
c     Jeremy Du Croz, Nag Central Office.
c     Sven Hammarling, Nag Central Office.
c     Richard Hanson, Sandia National Labs.
c
c
c     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
c     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, J, JX, KX
      LOGICAL            NOCONJ, NOUNIT
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX
c     ..
c     .. Executable Statements ..
c
c     Test the input parameters.
c
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     $         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     $         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZTRSV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
      NOCONJ = LSAME( TRANS, 'T' )
      NOUNIT = LSAME( DIAG , 'N' )
c
c     Set up the start point in X if the increment is not unity. This
c     will be  ( N - 1 )*INCX  too small for descending loops.
c
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
c
c     Start the operations. In this version the elements of A are
c     accessed sequentially with one pass through A.
c
      IF( LSAME( TRANS, 'N' ) )THEN
c
c        Form  x := inv( A )*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 20, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     IF( NOUNIT )
     $                  X( J ) = X( J )/A( J, J )
                     TEMP = X( J )
                     DO 10, I = J - 1, 1, -1
                        X( I ) = X( I ) - TEMP*A( I, J )
   10                CONTINUE
                  END IF
   20          CONTINUE
            ELSE
               JX = KX + ( N - 1 )*INCX
               DO 40, J = N, 1, -1
                  IF( X( JX ).NE.ZERO )THEN
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )/A( J, J )
                     TEMP = X( JX )
                     IX   = JX
                     DO 30, I = J - 1, 1, -1
                        IX      = IX      - INCX
                        X( IX ) = X( IX ) - TEMP*A( I, J )
   30                CONTINUE
                  END IF
                  JX = JX - INCX
   40          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 60, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     IF( NOUNIT )
     $                  X( J ) = X( J )/A( J, J )
                     TEMP = X( J )
                     DO 50, I = J + 1, N
                        X( I ) = X( I ) - TEMP*A( I, J )
   50                CONTINUE
                  END IF
   60          CONTINUE
            ELSE
               JX = KX
               DO 80, J = 1, N
                  IF( X( JX ).NE.ZERO )THEN
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )/A( J, J )
                     TEMP = X( JX )
                     IX   = JX
                     DO 70, I = J + 1, N
                        IX      = IX      + INCX
                        X( IX ) = X( IX ) - TEMP*A( I, J )
   70                CONTINUE
                  END IF
                  JX = JX + INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
c
c        Form  x := inv( A' )*x  or  x := inv( conjg( A' ) )*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 110, J = 1, N
                  TEMP = X( J )
                  IF( NOCONJ )THEN
                     DO 90, I = 1, J - 1
                        TEMP = TEMP - A( I, J )*X( I )
   90                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( J, J )
                  ELSE
                     DO 100, I = 1, J - 1
                        TEMP = TEMP - DCONJG( A( I, J ) )*X( I )
  100                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( A( J, J ) )
                  END IF
                  X( J ) = TEMP
  110          CONTINUE
            ELSE
               JX = KX
               DO 140, J = 1, N
                  IX   = KX
                  TEMP = X( JX )
                  IF( NOCONJ )THEN
                     DO 120, I = 1, J - 1
                        TEMP = TEMP - A( I, J )*X( IX )
                        IX   = IX   + INCX
  120                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( J, J )
                  ELSE
                     DO 130, I = 1, J - 1
                        TEMP = TEMP - DCONJG( A( I, J ) )*X( IX )
                        IX   = IX   + INCX
  130                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( A( J, J ) )
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   + INCX
  140          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 170, J = N, 1, -1
                  TEMP = X( J )
                  IF( NOCONJ )THEN
                     DO 150, I = N, J + 1, -1
                        TEMP = TEMP - A( I, J )*X( I )
  150                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( J, J )
                  ELSE
                     DO 160, I = N, J + 1, -1
                        TEMP = TEMP - DCONJG( A( I, J ) )*X( I )
  160                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( A( J, J ) )
                  END IF
                  X( J ) = TEMP
  170          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 200, J = N, 1, -1
                  IX   = KX
                  TEMP = X( JX )
                  IF( NOCONJ )THEN
                     DO 180, I = N, J + 1, -1
                        TEMP = TEMP - A( I, J )*X( IX )
                        IX   = IX   - INCX
  180                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( J, J )
                  ELSE
                     DO 190, I = N, J + 1, -1
                        TEMP = TEMP - DCONJG( A( I, J ) )*X( IX )
                        IX   = IX   - INCX
  190                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( A( J, J ) )
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   - INCX
  200          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of ZTRSV .
c
      END
