      SUBROUTINE SGBMV ( TRANS, M, N, KL, KU, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
c     .. Scalar Arguments ..
      REAL               ALPHA, BETA
      INTEGER            INCX, INCY, KL, KU, LDA, M, N
      CHARACTER*1        TRANS
c     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  SGBMV  performs one of the matrix-vector operations
c
c     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
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
c              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
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
c  ALPHA  - REAL            .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  A      - REAL             array of DIMENSION ( LDA, n ).
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
c  X      - REAL             array of DIMENSION at least
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
c  BETA   - REAL            .
c           On entry, BETA specifies the scalar beta. When BETA is
c           supplied as zero then Y need not be set on input.
c           Unchanged on exit.
c
c  Y      - REAL             array of DIMENSION at least
c           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
c           and at least
c           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
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
c     .. Parameters ..
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, K, KUP1, KX, KY,
     $                   LENX, LENY
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
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
         CALL XERBLA( 'SGBMV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
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
c        Form  y := alpha*A'*x + y.
c
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               K    = KUP1 - J
               DO 90, I = MAX( 1, J - KU ), MIN( M, J + KL )
                  TEMP = TEMP + A( K + I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               K    = KUP1 - J
               DO 110, I = MAX( 1, J - KU ), MIN( M, J + KL )
                  TEMP = TEMP + A( K + I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
               IF( J.GT.KU )
     $            KX = KX + INCX
  120       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of SGBMV .
c
      END
      subroutine sgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, 
     &  incy )

c*********************************************************************72
c
cc SGEMV computes y := alpha * A * x + beta * y for general matrix A.
c
c  Discussion:
c
c    SGEMV performs one of the matrix-vector operations
c      y := alpha*A *x + beta*y
c    or
c      y := alpha*A'*x + beta*y,
c    where alpha and beta are scalars, x and y are vectors and A is an
c    m by n matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 2014
c
c  Author:
c
c    Jack Dongarra, Jeremy Du Croz, Sven Hammarling,  Richard Hanson.
c
c  Parameters:
c
c    Input, character TRANS, specifies the operation to be performed:
c    'N' or 'N'   y := alpha*A *x + beta*y.
c    'T' or 'T'   y := alpha*A'*x + beta*y.
c    'C' or 'C'   y := alpha*A'*x + beta*y.
c
c    Input, integer M, the number of rows of the matrix A.
c    0 <= M.
c
c    Input, integer N, the number of columns of the matrix A.
c    0 <= N.
c
c    Input, real ALPHA, the scalar multiplier for A * x.
c
c    Input, real A(LDA,N).  The M x N subarray contains
c    the matrix A.
c
c    Input, integer  LDA, the the first dimension of A as declared
c    in the calling routine.  max ( 1, M ) <= LDA.
c
c    Input, real X(*), an array containing the vector to be 
c    multiplied by the matrix A.  
c    If TRANS = 'N' or 'n', then X must contain N entries, stored in INCX 
c    increments in a space of at least ( 1 + ( N - 1 ) * abs ( INCX ) ) 
c    locations.
c    Otherwise, X must contain M entries, store in INCX increments
c    in a space of at least ( 1 + ( M - 1 ) * abs ( INCX ) ) locations.
c
c    Input, integer INCX, the increment for the elements of
c    X.  INCX must not be zero.
c
c    Input, real BETA, the scalar multiplier for Y.
c
c    Input/output, real Y(*), an array containing the vector to
c    be scaled and incremented by A*X.
c    If TRANS = 'N' or 'n', then Y must contain M entries, stored in INCY
c    increments in a space of at least ( 1 + ( M - 1 ) * abs ( INCY ) ) 
c    locations.
c    Otherwise, Y must contain N entries, store in INCY increments
c    in a space of at least ( 1 + ( N - 1 ) * abs ( INCY ) ) locations.
c
c    Input, integer INCY, the increment for the elements of
c    Y.  INCY must not be zero.
c
      implicit none

      integer lda

      real a(lda,*)
      real alpha
      real beta
      integer i
      integer incx
      integer incy
      integer info
      integer ix
      integer iy
      integer j
      integer jx
      integer jy
      integer kx
      integer ky
      integer lenx
      integer leny
      logical lsame
      external lsame
      integer m
      intrinsic max
      integer n
      real temp
      character trans
      real x(*)
      external xerbla
      real y(*)
c
c  Test the input parameters.
c
      info = 0
      if ( .not. lsame ( trans, 'N' ) .and.  
     &     .not. lsame ( trans, 'T' ) .and.  
     &     .not. lsame ( trans, 'C' ) ) then
        info = 1
      else if ( m .lt. 0 ) then
        info = 2
      else if ( n .lt. 0 ) then
        info = 3
      else if ( lda .lt. max ( 1, m ) ) then
        info = 6
      else if ( incx .eq. 0 ) then
        info = 8
      else if ( incy .eq. 0 ) then
        info = 11
      end if

      if ( info .ne. 0 ) then
        call xerbla ( 'sgemv', info )
        return
      end if
c
c  Quick return if possible.
c
      if ( ( m .eq. 0 ) .or. 
     &     ( n .eq. 0 ) .or. 
     &     ( ( alpha .eq. 0.0E+00 ) .and. ( beta .eq. 1.0E+00 ) ) ) then
        return
      end if
c
c  Set LENX and LENY, the lengths of the vectors x and y, and set
c  up the start points in X and Y.
c
      if ( lsame ( trans, 'N' ) ) then
        lenx = n
        leny = m
      else
        lenx = m
        leny = n
      end if

      if ( 0 .lt. incx ) then
        kx = 1
      else
        kx = 1 - ( lenx - 1 ) * incx
      end if

      if ( 0 .lt. incy ) then
        ky = 1
      else
        ky = 1 - ( leny - 1 ) * incy
      end if
c
c  Start the operations. In this version the elements of A are
c  accessed sequentially with one pass through A.
c
c  First form  y := beta*y.
c
      if ( beta .ne. 1.0E+00 ) then
        if ( incy .eq. 1 ) then
          if ( beta .eq. 0.0E+00 ) then
            do i = 1, leny
              y(i) = 0.0E+00
            end do
          else
            do i = 1, leny
              y(i) = beta * y(i)
            end do
          end if
        else
          iy = ky
          if ( beta .eq. 0.0E+00 ) then
            do i = 1, leny
              y(iy) = 0.0E+00
              iy = iy + incy
            end do
          else
            do i = 1, leny
              y(iy) = beta * y(iy)
              iy = iy + incy
            end do
          end if
        end if
      end if

      if ( alpha .eq. 0.0E+00 ) then
        return
      end if
c
c  Form y := alpha*A*x + y.
c
      if ( lsame ( trans, 'N' ) ) then
        jx = kx
        if ( incy .eq. 1 ) then
          do j = 1, n
            if ( x(jx) .ne. 0.0E+00 ) then
              temp = alpha * x(jx)
              do i = 1, m
                y(i) = y(i) + temp * a(i,j)
              end do
            end if
            jx = jx + incx
          end do
        else
          do j = 1, n
            if ( x(jx) .ne. 0.0E+00 ) then
              temp = alpha * x(jx)
              iy = ky
              do i = 1, m
                y(iy) = y(iy) + temp * a(i,j)
                iy = iy + incy
              end do
            end if
            jx = jx + incx
          end do
        end if
c
c  Form y := alpha*A'*x + y.
c
      else
        jy = ky
        if ( incx .eq. 1 ) then
          do j = 1, n
            temp = 0.0E+00
            do i = 1, m
              temp = temp + a(i,j) * x(i)
            end do
            y(jy) = y(jy) + alpha * temp
            jy = jy + incy
          end do
        else
          do j = 1, n
            temp = 0.0E+00
            ix = kx
            do i = 1, m
              temp = temp + a(i,j) * x(ix)
              ix = ix + incx
            end do
            y(jy) = y(jy) + alpha * temp
            jy = jy + incy
          end do
        end if
      end if

      return
      end
      SUBROUTINE SGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
c     .. Scalar Arguments ..
      REAL               ALPHA
      INTEGER            INCX, INCY, LDA, M, N
c     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  SGER   performs the rank 1 operation
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
c  ALPHA  - REAL            .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  X      - REAL             array of dimension at least
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
c  Y      - REAL             array of dimension at least
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
c  A      - REAL             array of DIMENSION ( LDA, n ).
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
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP
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
         CALL XERBLA( 'SGER  ', INFO )
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
c     End of SGER  .
c
      END
      SUBROUTINE SSBMV ( UPLO, N, K, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
c     .. Scalar Arguments ..
      REAL               ALPHA, BETA
      INTEGER            INCX, INCY, K, LDA, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  SSBMV  performs the matrix-vector  operation
c
c     y := alpha*A*x + beta*y,
c
c  where alpha and beta are scalars, x and y are n element vectors and
c  A is an n by n symmetric band matrix, with k super-diagonals.
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
c  ALPHA  - REAL            .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  A      - REAL             array of DIMENSION ( LDA, n ).
c           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
c           by n part of the array A must contain the upper triangular
c           band part of the symmetric matrix, supplied column by
c           column, with the leading diagonal of the matrix in row
c           ( k + 1 ) of the array, the first super-diagonal starting at
c           position 2 in row k, and so on. The top left k by k triangle
c           of the array A is not referenced.
c           The following program segment will transfer the upper
c           triangular part of a symmetric band matrix from conventional
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
c           band part of the symmetric matrix, supplied column by
c           column, with the leading diagonal of the matrix in row 1 of
c           the array, the first sub-diagonal starting at position 1 in
c           row 2, and so on. The bottom right k by k triangle of the
c           array A is not referenced.
c           The following program segment will transfer the lower
c           triangular part of a symmetric band matrix from conventional
c           full matrix storage to band storage:
c
c                 DO 20, J = 1, N
c                    M = 1 - J
c                    DO 10, I = J, MIN( N, J + K )
c                       A( M + I, J ) = matrix( I, J )
c              10    CONTINUE
c              20 CONTINUE
c
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           ( k + 1 ).
c           Unchanged on exit.
c
c  X      - REAL             array of DIMENSION at least
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
c  BETA   - REAL            .
c           On entry, BETA specifies the scalar beta.
c           Unchanged on exit.
c
c  Y      - REAL             array of DIMENSION at least
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
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, KPLUS1, KX, KY, L
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
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
         CALL XERBLA( 'SSBMV ', INFO )
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
                  TEMP2  = TEMP2  + A( L + I, J )*X( I )
   50          CONTINUE
               Y( J ) = Y( J ) + TEMP1*A( KPLUS1, J ) + ALPHA*TEMP2
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
                  TEMP2   = TEMP2   + A( L + I, J )*X( IX )
                  IX      = IX      + INCX
                  IY      = IY      + INCY
   70          CONTINUE
               Y( JY ) = Y( JY ) + TEMP1*A( KPLUS1, J ) + ALPHA*TEMP2
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
               Y( J ) = Y( J )       + TEMP1*A( 1, J )
               L      = 1            - J
               DO 90, I = J + 1, MIN( N, J + K )
                  Y( I ) = Y( I ) + TEMP1*A( L + I, J )
                  TEMP2  = TEMP2  + A( L + I, J )*X( I )
   90          CONTINUE
               Y( J ) = Y( J ) + ALPHA*TEMP2
  100       CONTINUE
         ELSE
            JX = KX
            JY = KY
            DO 120, J = 1, N
               TEMP1   = ALPHA*X( JX )
               TEMP2   = ZERO
               Y( JY ) = Y( JY )       + TEMP1*A( 1, J )
               L       = 1             - J
               IX      = JX
               IY      = JY
               DO 110, I = J + 1, MIN( N, J + K )
                  IX      = IX      + INCX
                  IY      = IY      + INCY
                  Y( IY ) = Y( IY ) + TEMP1*A( L + I, J )
                  TEMP2   = TEMP2   + A( L + I, J )*X( IX )
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
c     End of SSBMV .
c
      END
      SUBROUTINE SSPMV ( UPLO, N, ALPHA, AP, X, INCX, BETA, Y, INCY )
c     .. Scalar Arguments ..
      REAL               ALPHA, BETA
      INTEGER            INCX, INCY, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      REAL               AP( * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  SSPMV  performs the matrix-vector operation
c
c     y := alpha*A*x + beta*y,
c
c  where alpha and beta are scalars, x and y are n element vectors and
c  A is an n by n symmetric matrix, supplied in packed form.
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
c  ALPHA  - REAL            .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  AP     - REAL             array of DIMENSION at least
c           ( ( n*( n + 1 ) )/2 ).
c           Before entry with UPLO = 'U' or 'u', the array AP must
c           contain the upper triangular part of the symmetric matrix
c           packed sequentially, column by column, so that AP( 1 )
c           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
c           and a( 2, 2 ) respectively, and so on.
c           Before entry with UPLO = 'L' or 'l', the array AP must
c           contain the lower triangular part of the symmetric matrix
c           packed sequentially, column by column, so that AP( 1 )
c           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
c           and a( 3, 1 ) respectively, and so on.
c           Unchanged on exit.
c
c  X      - REAL             array of dimension at least
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
c  BETA   - REAL            .
c           On entry, BETA specifies the scalar beta. When BETA is
c           supplied as zero then Y need not be set on input.
c           Unchanged on exit.
c
c  Y      - REAL             array of dimension at least
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
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, K, KK, KX, KY
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
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
         CALL XERBLA( 'SSPMV ', INFO )
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
                  TEMP2  = TEMP2  + AP( K )*X( I )
                  K      = K      + 1
   50          CONTINUE
               Y( J ) = Y( J ) + TEMP1*AP( KK + J - 1 ) + ALPHA*TEMP2
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
                  TEMP2   = TEMP2   + AP( K )*X( IX )
                  IX      = IX      + INCX
                  IY      = IY      + INCY
   70          CONTINUE
               Y( JY ) = Y( JY ) + TEMP1*AP( KK + J - 1 ) + ALPHA*TEMP2
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
               Y( J ) = Y( J )       + TEMP1*AP( KK )
               K      = KK           + 1
               DO 90, I = J + 1, N
                  Y( I ) = Y( I ) + TEMP1*AP( K )
                  TEMP2  = TEMP2  + AP( K )*X( I )
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
               Y( JY ) = Y( JY )       + TEMP1*AP( KK )
               IX      = JX
               IY      = JY
               DO 110, K = KK + 1, KK + N - J
                  IX      = IX      + INCX
                  IY      = IY      + INCY
                  Y( IY ) = Y( IY ) + TEMP1*AP( K )
                  TEMP2   = TEMP2   + AP( K )*X( IX )
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
c     End of SSPMV .
c
      END
      SUBROUTINE SSPR2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, AP )
c     .. Scalar Arguments ..
      REAL               ALPHA
      INTEGER            INCX, INCY, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      REAL               AP( * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  SSPR2  performs the symmetric rank 2 operation
c
c     A := alpha*x*y' + alpha*y*x' + A,
c
c  where alpha is a scalar, x and y are n element vectors and A is an
c  n by n symmetric matrix, supplied in packed form.
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
c  ALPHA  - REAL            .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  X      - REAL             array of dimension at least
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
c  Y      - REAL             array of dimension at least
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
c  AP     - REAL             array of DIMENSION at least
c           ( ( n*( n + 1 ) )/2 ).
c           Before entry with  UPLO = 'U' or 'u', the array AP must
c           contain the upper triangular part of the symmetric matrix
c           packed sequentially, column by column, so that AP( 1 )
c           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
c           and a( 2, 2 ) respectively, and so on. On exit, the array
c           AP is overwritten by the upper triangular part of the
c           updated matrix.
c           Before entry with UPLO = 'L' or 'l', the array AP must
c           contain the lower triangular part of the symmetric matrix
c           packed sequentially, column by column, so that AP( 1 )
c           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
c           and a( 3, 1 ) respectively, and so on. On exit, the array
c           AP is overwritten by the lower triangular part of the
c           updated matrix.
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
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, K, KK, KX, KY
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
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
         CALL XERBLA( 'SSPR2 ', INFO )
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
                  TEMP1 = ALPHA*Y( J )
                  TEMP2 = ALPHA*X( J )
                  K     = KK
                  DO 10, I = 1, J
                     AP( K ) = AP( K ) + X( I )*TEMP1 + Y( I )*TEMP2
                     K       = K       + 1
   10             CONTINUE
               END IF
               KK = KK + J
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*Y( JY )
                  TEMP2 = ALPHA*X( JX )
                  IX    = KX
                  IY    = KY
                  DO 30, K = KK, KK + J - 1
                     AP( K ) = AP( K ) + X( IX )*TEMP1 + Y( IY )*TEMP2
                     IX      = IX      + INCX
                     IY      = IY      + INCY
   30             CONTINUE
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
                  TEMP1 = ALPHA*Y( J )
                  TEMP2 = ALPHA*X( J )
                  K     = KK
                  DO 50, I = J, N
                     AP( K ) = AP( K ) + X( I )*TEMP1 + Y( I )*TEMP2
                     K       = K       + 1
   50             CONTINUE
               END IF
               KK = KK + N - J + 1
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*Y( JY )
                  TEMP2 = ALPHA*X( JX )
                  IX    = JX
                  IY    = JY
                  DO 70, K = KK, KK + N - J
                     AP( K ) = AP( K ) + X( IX )*TEMP1 + Y( IY )*TEMP2
                     IX      = IX      + INCX
                     IY      = IY      + INCY
   70             CONTINUE
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
c     End of SSPR2 .
c
      END
      SUBROUTINE SSPR  ( UPLO, N, ALPHA, X, INCX, AP )
c     .. Scalar Arguments ..
      REAL               ALPHA
      INTEGER            INCX, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      REAL               AP( * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  SSPR    performs the symmetric rank 1 operation
c
c     A := alpha*x*x' + A,
c
c  where alpha is a real scalar, x is an n element vector and A is an
c  n by n symmetric matrix, supplied in packed form.
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
c  ALPHA  - REAL            .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  X      - REAL             array of dimension at least
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
c  AP     - REAL             array of DIMENSION at least
c           ( ( n*( n + 1 ) )/2 ).
c           Before entry with  UPLO = 'U' or 'u', the array AP must
c           contain the upper triangular part of the symmetric matrix
c           packed sequentially, column by column, so that AP( 1 )
c           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
c           and a( 2, 2 ) respectively, and so on. On exit, the array
c           AP is overwritten by the upper triangular part of the
c           updated matrix.
c           Before entry with UPLO = 'L' or 'l', the array AP must
c           contain the lower triangular part of the symmetric matrix
c           packed sequentially, column by column, so that AP( 1 )
c           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
c           and a( 3, 1 ) respectively, and so on. On exit, the array
c           AP is overwritten by the lower triangular part of the
c           updated matrix.
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
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JX, K, KK, KX
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
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
         CALL XERBLA( 'SSPR  ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
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
                  TEMP = ALPHA*X( J )
                  K    = KK
                  DO 10, I = 1, J
                     AP( K ) = AP( K ) + X( I )*TEMP
                     K       = K       + 1
   10             CONTINUE
               END IF
               KK = KK + J
   20       CONTINUE
         ELSE
            JX = KX
            DO 40, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IX   = KX
                  DO 30, K = KK, KK + J - 1
                     AP( K ) = AP( K ) + X( IX )*TEMP
                     IX      = IX      + INCX
   30             CONTINUE
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
                  TEMP = ALPHA*X( J )
                  K    = KK
                  DO 50, I = J, N
                     AP( K ) = AP( K ) + X( I )*TEMP
                     K       = K       + 1
   50             CONTINUE
               END IF
               KK = KK + N - J + 1
   60       CONTINUE
         ELSE
            JX = KX
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IX   = JX
                  DO 70, K = KK, KK + N - J
                     AP( K ) = AP( K ) + X( IX )*TEMP
                     IX      = IX      + INCX
   70             CONTINUE
               END IF
               JX = JX + INCX
               KK = KK + N - J + 1
   80       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of SSPR  .
c
      END
      SUBROUTINE SSYMV ( UPLO, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
c     .. Scalar Arguments ..
      REAL               ALPHA, BETA
      INTEGER            INCX, INCY, LDA, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  SSYMV  performs the matrix-vector  operation
c
c     y := alpha*A*x + beta*y,
c
c  where alpha and beta are scalars, x and y are n element vectors and
c  A is an n by n symmetric matrix.
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
c  ALPHA  - REAL            .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  A      - REAL             array of DIMENSION ( LDA, n ).
c           Before entry with  UPLO = 'U' or 'u', the leading n by n
c           upper triangular part of the array A must contain the upper
c           triangular part of the symmetric matrix and the strictly
c           lower triangular part of A is not referenced.
c           Before entry with UPLO = 'L' or 'l', the leading n by n
c           lower triangular part of the array A must contain the lower
c           triangular part of the symmetric matrix and the strictly
c           upper triangular part of A is not referenced.
c           Unchanged on exit.
c
c  LDA    - INTEGER.
c           On entry, LDA specifies the first dimension of A as declared
c           in the calling (sub) program. LDA must be at least
c           max( 1, n ).
c           Unchanged on exit.
c
c  X      - REAL             array of dimension at least
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
c  BETA   - REAL            .
c           On entry, BETA specifies the scalar beta. When BETA is
c           supplied as zero then Y need not be set on input.
c           Unchanged on exit.
c
c  Y      - REAL             array of dimension at least
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
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
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
         CALL XERBLA( 'SSYMV ', INFO )
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
                  TEMP2  = TEMP2  + A( I, J )*X( I )
   50          CONTINUE
               Y( J ) = Y( J ) + TEMP1*A( J, J ) + ALPHA*TEMP2
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
                  TEMP2   = TEMP2   + A( I, J )*X( IX )
                  IX      = IX      + INCX
                  IY      = IY      + INCY
   70          CONTINUE
               Y( JY ) = Y( JY ) + TEMP1*A( J, J ) + ALPHA*TEMP2
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
               Y( J ) = Y( J )       + TEMP1*A( J, J )
               DO 90, I = J + 1, N
                  Y( I ) = Y( I ) + TEMP1*A( I, J )
                  TEMP2  = TEMP2  + A( I, J )*X( I )
   90          CONTINUE
               Y( J ) = Y( J ) + ALPHA*TEMP2
  100       CONTINUE
         ELSE
            JX = KX
            JY = KY
            DO 120, J = 1, N
               TEMP1   = ALPHA*X( JX )
               TEMP2   = ZERO
               Y( JY ) = Y( JY )       + TEMP1*A( J, J )
               IX      = JX
               IY      = JY
               DO 110, I = J + 1, N
                  IX      = IX      + INCX
                  IY      = IY      + INCY
                  Y( IY ) = Y( IY ) + TEMP1*A( I, J )
                  TEMP2   = TEMP2   + A( I, J )*X( IX )
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
c     End of SSYMV .
c
      END
      SUBROUTINE SSYR2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, A, LDA )
c     .. Scalar Arguments ..
      REAL               ALPHA
      INTEGER            INCX, INCY, LDA, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
c     ..
c
c  Purpose
c  =======
c
c  SSYR2  performs the symmetric rank 2 operation
c
c     A := alpha*x*y' + alpha*y*x' + A,
c
c  where alpha is a scalar, x and y are n element vectors and A is an n
c  by n symmetric matrix.
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
c  ALPHA  - REAL            .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  X      - REAL             array of dimension at least
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
c  Y      - REAL             array of dimension at least
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
c  A      - REAL             array of DIMENSION ( LDA, n ).
c           Before entry with  UPLO = 'U' or 'u', the leading n by n
c           upper triangular part of the array A must contain the upper
c           triangular part of the symmetric matrix and the strictly
c           lower triangular part of A is not referenced. On exit, the
c           upper triangular part of the array A is overwritten by the
c           upper triangular part of the updated matrix.
c           Before entry with UPLO = 'L' or 'l', the leading n by n
c           lower triangular part of the array A must contain the lower
c           triangular part of the symmetric matrix and the strictly
c           upper triangular part of A is not referenced. On exit, the
c           lower triangular part of the array A is overwritten by the
c           lower triangular part of the updated matrix.
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
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
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
         CALL XERBLA( 'SSYR2 ', INFO )
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
                  TEMP1 = ALPHA*Y( J )
                  TEMP2 = ALPHA*X( J )
                  DO 10, I = 1, J
                     A( I, J ) = A( I, J ) + X( I )*TEMP1 + Y( I )*TEMP2
   10             CONTINUE
               END IF
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*Y( JY )
                  TEMP2 = ALPHA*X( JX )
                  IX    = KX
                  IY    = KY
                  DO 30, I = 1, J
                     A( I, J ) = A( I, J ) + X( IX )*TEMP1
     $                                     + Y( IY )*TEMP2
                     IX        = IX        + INCX
                     IY        = IY        + INCY
   30             CONTINUE
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
                  TEMP1 = ALPHA*Y( J )
                  TEMP2 = ALPHA*X( J )
                  DO 50, I = J, N
                     A( I, J ) = A( I, J ) + X( I )*TEMP1 + Y( I )*TEMP2
   50             CONTINUE
               END IF
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*Y( JY )
                  TEMP2 = ALPHA*X( JX )
                  IX    = JX
                  IY    = JY
                  DO 70, I = J, N
                     A( I, J ) = A( I, J ) + X( IX )*TEMP1
     $                                     + Y( IY )*TEMP2
                     IX        = IX        + INCX
                     IY        = IY        + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
               JY = JY + INCY
   80       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of SSYR2 .
c
      END
      SUBROUTINE SSYR  ( UPLO, N, ALPHA, X, INCX, A, LDA )
c     .. Scalar Arguments ..
      REAL               ALPHA
      INTEGER            INCX, LDA, N
      CHARACTER*1        UPLO
c     .. Array Arguments ..
      REAL               A( LDA, * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  SSYR   performs the symmetric rank 1 operation
c
c     A := alpha*x*x' + A,
c
c  where alpha is a real scalar, x is an n element vector and A is an
c  n by n symmetric matrix.
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
c  ALPHA  - REAL            .
c           On entry, ALPHA specifies the scalar alpha.
c           Unchanged on exit.
c
c  X      - REAL             array of dimension at least
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
c  A      - REAL             array of DIMENSION ( LDA, n ).
c           Before entry with  UPLO = 'U' or 'u', the leading n by n
c           upper triangular part of the array A must contain the upper
c           triangular part of the symmetric matrix and the strictly
c           lower triangular part of A is not referenced. On exit, the
c           upper triangular part of the array A is overwritten by the
c           upper triangular part of the updated matrix.
c           Before entry with UPLO = 'L' or 'l', the leading n by n
c           lower triangular part of the array A must contain the lower
c           triangular part of the symmetric matrix and the strictly
c           upper triangular part of A is not referenced. On exit, the
c           lower triangular part of the array A is overwritten by the
c           lower triangular part of the updated matrix.
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
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JX, KX
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
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
         CALL XERBLA( 'SSYR  ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( ( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
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
                  TEMP = ALPHA*X( J )
                  DO 10, I = 1, J
                     A( I, J ) = A( I, J ) + X( I )*TEMP
   10             CONTINUE
               END IF
   20       CONTINUE
         ELSE
            JX = KX
            DO 40, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IX   = KX
                  DO 30, I = 1, J
                     A( I, J ) = A( I, J ) + X( IX )*TEMP
                     IX        = IX        + INCX
   30             CONTINUE
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
                  TEMP = ALPHA*X( J )
                  DO 50, I = J, N
                     A( I, J ) = A( I, J ) + X( I )*TEMP
   50             CONTINUE
               END IF
   60       CONTINUE
         ELSE
            JX = KX
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IX   = JX
                  DO 70, I = J, N
                     A( I, J ) = A( I, J ) + X( IX )*TEMP
                     IX        = IX        + INCX
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      END IF
c
      RETURN
c
c     End of SSYR  .
c
      END
      SUBROUTINE STBMV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
c     .. Scalar Arguments ..
      INTEGER            INCX, K, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
c     .. Array Arguments ..
      REAL               A( LDA, * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  STBMV  performs one of the matrix-vector operations
c
c     x := A*x,   or   x := A'*x,
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
c              TRANS = 'C' or 'c'   x := A'*x.
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
c  A      - REAL             array of DIMENSION ( LDA, n ).
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
c  X      - REAL             array of dimension at least
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
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JX, KPLUS1, KX, L
      LOGICAL            NOUNIT
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
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
         CALL XERBLA( 'STBMV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
      NOUNIT = LSAME( DIAG, 'N' )
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
c        Form  x := A'*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            KPLUS1 = K + 1
            IF( INCX.EQ.1 )THEN
               DO 100, J = N, 1, -1
                  TEMP = X( J )
                  L    = KPLUS1 - J
                  IF( NOUNIT )
     $               TEMP = TEMP*A( KPLUS1, J )
                  DO 90, I = J - 1, MAX( 1, J - K ), -1
                     TEMP = TEMP + A( L + I, J )*X( I )
   90             CONTINUE
                  X( J ) = TEMP
  100          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 120, J = N, 1, -1
                  TEMP = X( JX )
                  KX   = KX      - INCX
                  IX   = KX
                  L    = KPLUS1  - J
                  IF( NOUNIT )
     $               TEMP = TEMP*A( KPLUS1, J )
                  DO 110, I = J - 1, MAX( 1, J - K ), -1
                     TEMP = TEMP + A( L + I, J )*X( IX )
                     IX   = IX   - INCX
  110             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   - INCX
  120          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 140, J = 1, N
                  TEMP = X( J )
                  L    = 1      - J
                  IF( NOUNIT )
     $               TEMP = TEMP*A( 1, J )
                  DO 130, I = J + 1, MIN( N, J + K )
                     TEMP = TEMP + A( L + I, J )*X( I )
  130             CONTINUE
                  X( J ) = TEMP
  140          CONTINUE
            ELSE
               JX = KX
               DO 160, J = 1, N
                  TEMP = X( JX )
                  KX   = KX      + INCX
                  IX   = KX
                  L    = 1       - J
                  IF( NOUNIT )
     $               TEMP = TEMP*A( 1, J )
                  DO 150, I = J + 1, MIN( N, J + K )
                     TEMP = TEMP + A( L + I, J )*X( IX )
                     IX   = IX   + INCX
  150             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   + INCX
  160          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of STBMV .
c
      END
      SUBROUTINE STBSV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
c     .. Scalar Arguments ..
      INTEGER            INCX, K, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
c     .. Array Arguments ..
      REAL               A( LDA, * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  STBSV  solves one of the systems of equations
c
c     A*x = b,   or   A'*x = b,
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
c              TRANS = 'C' or 'c'   A'*x = b.
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
c  A      - REAL             array of DIMENSION ( LDA, n ).
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
c  X      - REAL             array of dimension at least
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
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JX, KPLUS1, KX, L
      LOGICAL            NOUNIT
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
c     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
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
         CALL XERBLA( 'STBSV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
      NOUNIT = LSAME( DIAG, 'N' )
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
c        Form  x := inv( A')*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            KPLUS1 = K + 1
            IF( INCX.EQ.1 )THEN
               DO 100, J = 1, N
                  TEMP = X( J )
                  L    = KPLUS1 - J
                  DO 90, I = MAX( 1, J - K ), J - 1
                     TEMP = TEMP - A( L + I, J )*X( I )
   90             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( KPLUS1, J )
                  X( J ) = TEMP
  100          CONTINUE
            ELSE
               JX = KX
               DO 120, J = 1, N
                  TEMP = X( JX )
                  IX   = KX
                  L    = KPLUS1  - J
                  DO 110, I = MAX( 1, J - K ), J - 1
                     TEMP = TEMP - A( L + I, J )*X( IX )
                     IX   = IX   + INCX
  110             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( KPLUS1, J )
                  X( JX ) = TEMP
                  JX      = JX   + INCX
                  IF( J.GT.K )
     $               KX = KX + INCX
  120          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 140, J = N, 1, -1
                  TEMP = X( J )
                  L    = 1      - J
                  DO 130, I = MIN( N, J + K ), J + 1, -1
                     TEMP = TEMP - A( L + I, J )*X( I )
  130             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( 1, J )
                  X( J ) = TEMP
  140          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 160, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = KX
                  L    = 1       - J
                  DO 150, I = MIN( N, J + K ), J + 1, -1
                     TEMP = TEMP - A( L + I, J )*X( IX )
                     IX   = IX   - INCX
  150             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( 1, J )
                  X( JX ) = TEMP
                  JX      = JX   - INCX
                  IF( ( N - J ).GE.K )
     $               KX = KX - INCX
  160          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of STBSV .
c
      END
      SUBROUTINE STPMV ( UPLO, TRANS, DIAG, N, AP, X, INCX )
c     .. Scalar Arguments ..
      INTEGER            INCX, N
      CHARACTER*1        DIAG, TRANS, UPLO
c     .. Array Arguments ..
      REAL               AP( * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  STPMV  performs one of the matrix-vector operations
c
c     x := A*x,   or   x := A'*x,
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
c              TRANS = 'C' or 'c'   x := A'*x.
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
c  AP     - REAL             array of DIMENSION at least
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
c  X      - REAL             array of dimension at least
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
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JX, K, KK, KX
      LOGICAL            NOUNIT
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
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
         CALL XERBLA( 'STPMV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
      NOUNIT = LSAME( DIAG, 'N' )
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
            KK =1
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
c        Form  x := A'*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            KK = ( N*( N + 1 ) )/2
            IF( INCX.EQ.1 )THEN
               DO 100, J = N, 1, -1
                  TEMP = X( J )
                  IF( NOUNIT )
     $               TEMP = TEMP*AP( KK )
                  K = KK - 1
                  DO 90, I = J - 1, 1, -1
                     TEMP = TEMP + AP( K )*X( I )
                     K    = K    - 1
   90             CONTINUE
                  X( J ) = TEMP
                  KK     = KK   - J
  100          CONTINUE
            ELSE
               JX = KX + ( N - 1 )*INCX
               DO 120, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )
     $               TEMP = TEMP*AP( KK )
                  DO 110, K = KK - 1, KK - J + 1, -1
                     IX   = IX   - INCX
                     TEMP = TEMP + AP( K )*X( IX )
  110             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   - INCX
                  KK      = KK   - J
  120          CONTINUE
            END IF
         ELSE
            KK = 1
            IF( INCX.EQ.1 )THEN
               DO 140, J = 1, N
                  TEMP = X( J )
                  IF( NOUNIT )
     $               TEMP = TEMP*AP( KK )
                  K = KK + 1
                  DO 130, I = J + 1, N
                     TEMP = TEMP + AP( K )*X( I )
                     K    = K    + 1
  130             CONTINUE
                  X( J ) = TEMP
                  KK     = KK   + ( N - J + 1 )
  140          CONTINUE
            ELSE
               JX = KX
               DO 160, J = 1, N
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )
     $               TEMP = TEMP*AP( KK )
                  DO 150, K = KK + 1, KK + N - J
                     IX   = IX   + INCX
                     TEMP = TEMP + AP( K )*X( IX )
  150             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   + INCX
                  KK      = KK   + ( N - J + 1 )
  160          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of STPMV .
c
      END
      SUBROUTINE STPSV ( UPLO, TRANS, DIAG, N, AP, X, INCX )
c     .. Scalar Arguments ..
      INTEGER            INCX, N
      CHARACTER*1        DIAG, TRANS, UPLO
c     .. Array Arguments ..
      REAL               AP( * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  STPSV  solves one of the systems of equations
c
c     A*x = b,   or   A'*x = b,
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
c              TRANS = 'C' or 'c'   A'*x = b.
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
c  AP     - REAL             array of DIMENSION at least
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
c  X      - REAL             array of dimension at least
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
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JX, K, KK, KX
      LOGICAL            NOUNIT
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
c     .. External Subroutines ..
      EXTERNAL           XERBLA
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
         CALL XERBLA( 'STPSV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
      NOUNIT = LSAME( DIAG, 'N' )
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
c        Form  x := inv( A' )*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            KK = 1
            IF( INCX.EQ.1 )THEN
               DO 100, J = 1, N
                  TEMP = X( J )
                  K    = KK
                  DO 90, I = 1, J - 1
                     TEMP = TEMP - AP( K )*X( I )
                     K    = K    + 1
   90             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/AP( KK + J - 1 )
                  X( J ) = TEMP
                  KK     = KK   + J
  100          CONTINUE
            ELSE
               JX = KX
               DO 120, J = 1, N
                  TEMP = X( JX )
                  IX   = KX
                  DO 110, K = KK, KK + J - 2
                     TEMP = TEMP - AP( K )*X( IX )
                     IX   = IX   + INCX
  110             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/AP( KK + J - 1 )
                  X( JX ) = TEMP
                  JX      = JX   + INCX
                  KK      = KK   + J
  120          CONTINUE
            END IF
         ELSE
            KK = ( N*( N + 1 ) )/2
            IF( INCX.EQ.1 )THEN
               DO 140, J = N, 1, -1
                  TEMP = X( J )
                  K = KK
                  DO 130, I = N, J + 1, -1
                     TEMP = TEMP - AP( K )*X( I )
                     K    = K    - 1
  130             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/AP( KK - N + J )
                  X( J ) = TEMP
                  KK     = KK   - ( N - J + 1 )
  140          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 160, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = KX
                  DO 150, K = KK, KK - ( N - ( J + 1 ) ), -1
                     TEMP = TEMP - AP( K )*X( IX )
                     IX   = IX   - INCX
  150             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/AP( KK - N + J )
                  X( JX ) = TEMP
                  JX      = JX   - INCX
                  KK      = KK   - (N - J + 1 )
  160          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of STPSV .
c
      END
      SUBROUTINE STRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
c     .. Scalar Arguments ..
      INTEGER            INCX, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
c     .. Array Arguments ..
      REAL               A( LDA, * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  STRMV  performs one of the matrix-vector operations
c
c     x := A*x,   or   x := A'*x,
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
c              TRANS = 'C' or 'c'   x := A'*x.
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
c  A      - REAL             array of DIMENSION ( LDA, n ).
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
c  X      - REAL             array of dimension at least
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
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JX, KX
      LOGICAL            NOUNIT
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
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
         CALL XERBLA( 'STRMV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
      NOUNIT = LSAME( DIAG, 'N' )
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
c        Form  x := A'*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 100, J = N, 1, -1
                  TEMP = X( J )
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 90, I = J - 1, 1, -1
                     TEMP = TEMP + A( I, J )*X( I )
   90             CONTINUE
                  X( J ) = TEMP
  100          CONTINUE
            ELSE
               JX = KX + ( N - 1 )*INCX
               DO 120, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 110, I = J - 1, 1, -1
                     IX   = IX   - INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  110             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   - INCX
  120          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 140, J = 1, N
                  TEMP = X( J )
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 130, I = J + 1, N
                     TEMP = TEMP + A( I, J )*X( I )
  130             CONTINUE
                  X( J ) = TEMP
  140          CONTINUE
            ELSE
               JX = KX
               DO 160, J = 1, N
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 150, I = J + 1, N
                     IX   = IX   + INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  150             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   + INCX
  160          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of STRMV .
c
      END
      SUBROUTINE STRSV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
c     .. Scalar Arguments ..
      INTEGER            INCX, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
c     .. Array Arguments ..
      REAL               A( LDA, * ), X( * )
c     ..
c
c  Purpose
c  =======
c
c  STRSV  solves one of the systems of equations
c
c     A*x = b,   or   A'*x = b,
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
c              TRANS = 'C' or 'c'   A'*x = b.
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
c  A      - REAL             array of DIMENSION ( LDA, n ).
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
c  X      - REAL             array of dimension at least
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
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
c     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JX, KX
      LOGICAL            NOUNIT
c     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
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
         CALL XERBLA( 'STRSV ', INFO )
         RETURN
      END IF
c
c     Quick return if possible.
c
      IF( N.EQ.0 )
     $   RETURN
c
      NOUNIT = LSAME( DIAG, 'N' )
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
c        Form  x := inv( A' )*x.
c
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 100, J = 1, N
                  TEMP = X( J )
                  DO 90, I = 1, J - 1
                     TEMP = TEMP - A( I, J )*X( I )
   90             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( J, J )
                  X( J ) = TEMP
  100          CONTINUE
            ELSE
               JX = KX
               DO 120, J = 1, N
                  TEMP = X( JX )
                  IX   = KX
                  DO 110, I = 1, J - 1
                     TEMP = TEMP - A( I, J )*X( IX )
                     IX   = IX   + INCX
  110             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( J, J )
                  X( JX ) = TEMP
                  JX      = JX   + INCX
  120          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 140, J = N, 1, -1
                  TEMP = X( J )
                  DO 130, I = N, J + 1, -1
                     TEMP = TEMP - A( I, J )*X( I )
  130             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( J, J )
                  X( J ) = TEMP
  140          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 160, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = KX
                  DO 150, I = N, J + 1, -1
                     TEMP = TEMP - A( I, J )*X( IX )
                     IX   = IX   - INCX
  150             CONTINUE
                  IF( NOUNIT )
     $               TEMP = TEMP/A( J, J )
                  X( JX ) = TEMP
                  JX      = JX   - INCX
  160          CONTINUE
            END IF
         END IF
      END IF
c
      RETURN
c
c     End of STRSV .
c
      END
